package model.mmr

import scala.math
import util.solveItp
import zio.*
import java.util.concurrent.ConcurrentHashMap
import scala.jdk.CollectionConverters.*

inline val TANH_MULTIPLIER = math.Pi / 1.7320508075688772

case class Rating(
    mu: Double,
    sig: Double
):
  def withNoise(sigNoise: Double) = Rating(mu, math.hypot(sig, sigNoise))
  def toTanhTerm = {
    val w = TANH_MULTIPLIER / sig
    TanhTerm(mu, w * 0.5, w)
  }

case class TanhTerm(
    mu: Double,
    wArg: Double,
    wOut: Double
):
  def weight = wArg * wOut * 2.0 / math.pow(TANH_MULTIPLIER, 2.0)
  def apply(x: Double) = -math.tanh((x - mu) * wArg) * wOut
  def evalLess(x: Double) = this(x) - wOut
  def evalGreater(x: Double) = this(x) + wOut
  def evalEqual(x: Double) = this(x)

case class Player(
    id: Long,
    normalFactor: Rating,
    logisticFactors: Vector[TanhTerm],
    approxPosterior: Rating
):
  def truncateHistory(maxHistory: Int) = {
    val (left, right) =
      logisticFactors.splitAt(logisticFactors.length - maxHistory)
    val normalFactor1 =
      left.foldLeft(normalFactor)((factor, term) => {
        val wN = math.pow(factor.sig, -2.0)
        val wL = term.weight
        Rating(
          (wN * factor.mu + wL * term.mu) / (wN + wL),
          math.sqrt(1.0 / (wN + wL))
        )
      })
    Player(id, normalFactor1, right, approxPosterior)
  }

  def updateRating(performance: Rating) = {
    val logisticFactors1 = logisticFactors :+ performance.toTanhTerm
    val wN = math.pow(normalFactor.sig, -2.0)
    val mu1 = robustAverage(logisticFactors1, -normalFactor.mu * wN, wN)
    val sig1 = math.sqrt(
      1.0 / (math.pow(approxPosterior.sig, -2.0) + math
        .pow(performance.sig, -2.0))
    )
    Player(id, normalFactor, logisticFactors1, Rating(mu1, sig1))
  }

  def addNoise(sigNoise: Double, transferSpeed: Double) = {
    val approxPosterior1 = approxPosterior.withNoise(sigNoise)
    val decay = math.pow(approxPosterior.sig / approxPosterior1.sig, 2.0)
    val transfer = math.pow(decay, transferSpeed)
    val wTNormOld = math.pow(normalFactor.sig, -2.0)
    val wTFromNormOld = transfer * wTNormOld
    val wTFromTransfer =
      (1.0 - transfer) * (wTNormOld + logisticFactors.map(_.weight).sum)
    val wTTotal = wTFromNormOld + wTFromTransfer
    val normalFactor1 = Rating(
      (wTFromNormOld * normalFactor.mu + wTFromTransfer * approxPosterior.mu) / wTTotal,
      math.sqrt(1.0 / (decay * wTTotal))
    )
    val logisticFactors1 = logisticFactors.map(term =>
      TanhTerm(term.mu, term.wArg, term.wOut * transfer * decay)
    )

    Player(id, normalFactor1, logisticFactors1, approxPosterior1)
  }

object Player:
  def withRating(id: Long, rating: Rating) =
    Player(id, rating, Vector.empty, rating)

def robustAverage(
    terms: Iterable[TanhTerm],
    offset: Double,
    slope: Double
) = {
  inline def bounds = (-6000.0, 9000.0)
  solveItp(x => terms.map(-_(x)).sum + offset + slope * x, bounds)
}

case class EloMmrParameters(
    sigLimit: Double,
    transferSpeed: Double,
    muInit: Double,
    sigInit: Double,
    maxHistory: Int,
    weightLimit: Double
):
  def sigPerfAndDrift(contestWeight: Double) = {
    val scaledContestWeight = contestWeight * weightLimit
    val sigPerf = math.sqrt(1.0 + 1.0 / scaledContestWeight) * sigLimit
    val sigDrift = math.sqrt(scaledContestWeight * math.pow(sigLimit, 2.0))
    (sigPerf, sigDrift)
  }

object EloMmrParameters:
  def default =
    EloMmrParameters(80.0, 1.0, 1500.0, 350.0, Int.MaxValue - 1, 0.2)

trait EloMmrStorage:
  def loadPlayer(id: Long): ZIO[Any, Throwable, Option[Player]]
  def savePlayer(player: Player): ZIO[Any, Throwable, Unit]
  def loadAllPlayers: ZIO[Any, Throwable, Vector[Player]]

object EloMmrStorage:
  def loadPlayer(id: Long): ZIO[EloMmrStorage, Throwable, Option[Player]] =
    ZIO.serviceWithZIO[EloMmrStorage](_.loadPlayer(id))

  def savePlayer(player: Player): ZIO[EloMmrStorage, Throwable, Unit] =
    ZIO.serviceWithZIO[EloMmrStorage](_.savePlayer(player))

  def loadAllPlayers: ZIO[EloMmrStorage, Throwable, Vector[Player]] =
    ZIO.serviceWithZIO[EloMmrStorage](_.loadAllPlayers)

class ConcurrentMapStorage(
    inner: scala.collection.concurrent.Map[Long, Player]
) extends EloMmrStorage:
  override def loadPlayer(id: Long): ZIO[Any, Throwable, Option[Player]] =
    ZIO.attempt(inner.get(id))

  override def savePlayer(player: Player): ZIO[Any, Throwable, Unit] =
    ZIO.attempt(
      inner
        .putIfAbsent(player.id, player)
        .fold(())(_ => inner.replace(player.id, player))
    )

  override def loadAllPlayers: ZIO[Any, Throwable, Vector[Player]] =
    ZIO.attempt(
      inner.toVector.map(e => e(1))
    )

object ConcurrentMapStorage:
  def layer: ZLayer[Any, Throwable, ConcurrentMapStorage] = ZLayer {
    ZIO.attempt(ConcurrentMapStorage(ConcurrentHashMap[Long, Player]().asScala))
  }

object EloMmr:
  def updateRound(using
      params: EloMmrParameters
  )(scores: Map[Long, Long], contestWeight: Double) = {
    val (sigPerf, sigDrift) = params.sigPerfAndDrift(contestWeight)
    for {
      players1 <- ZIO
        .collectAllPar(
          scores.map((id, score) =>
            EloMmrStorage
              .loadPlayer(id)
              .map(
                _.fold(
                  Player.withRating(id, Rating(params.muInit, params.sigInit))
                )(identity)
              )
              .map(_.addNoise(sigDrift, params.transferSpeed))
              .map(p => (p.id, p))
          )
        )
        .map(_.toMap)
      players2 <- ZIO.collectAllPar(
        players1.map((selfId, self) =>
          ZIO.attempt {
            val selfScore = scores(selfId)
            val wins = scores
              .filter((_, otherScore) => selfScore > otherScore)
              .keys
              .toVector
              .map(players1(_).approxPosterior.withNoise(sigPerf).toTanhTerm)
            val ties = scores
              .filter((_, otherScore) => selfScore == otherScore)
              .keys
              .toVector
              .map(players1(_).approxPosterior.withNoise(sigPerf).toTanhTerm)
            val loses = scores
              .filter((_, otherScore) => selfScore < otherScore)
              .keys
              .toVector
              .map(players1(_).approxPosterior.withNoise(sigPerf).toTanhTerm)

            def f(x: Double): Double = {
              wins
                .map(_.evalGreater(x))
                .sum
                + ties
                  .map(_.evalEqual(x))
                  .sum
                + loses
                  .map(_.evalLess(x))
                  .sum
            }

            if (selfId == 1) {
              pprint.pprintln(f(1500.0))
            }

            val muPerf = solveItp(f, (-6000.0, 9000.0))

            (
              self
                .truncateHistory(params.maxHistory + 1)
                .updateRating(Rating(muPerf, sigPerf)),
              muPerf
            )
          }
        )
      )
      results <- ZIO.collectAllPar(players2.map { (p, muPerf) =>
        val before = players1(p.id).approxPosterior
        val after = p.approxPosterior

        for {
          _ <- EloMmrStorage.savePlayer(p)
          result <- ZIO.succeed((p.id, (muPerf, after, after.mu - before.mu)))
        } yield result
      })
    } yield results.toMap
  }
