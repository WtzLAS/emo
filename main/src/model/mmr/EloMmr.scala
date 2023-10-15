package model.mmr

import io.github.chronoscala.Imports.*
import util.solveItp
import zio.ZIO

import scala.math
import java.time.Duration

inline val TANH_MULTIPLIER = math.Pi / 1.7320508075688772

case class Rating(
    mu: Double,
    sig: Double
):
  def withNoise(sigNoise: Double) = this.copy(sig = math.hypot(sig, sigNoise))
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
    approxPosterior: Rating,
    lastUpdate: OffsetDateTime
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
    this.copy(normalFactor = normalFactor1)
  }

  def updateRating(performance: Rating, updateTime: OffsetDateTime) = {
    val logisticFactors1 = logisticFactors :+ performance.toTanhTerm
    val wN = math.pow(normalFactor.sig, -2.0)
    val mu1 = robustAverage(logisticFactors1, -normalFactor.mu * wN, wN)
    val sig1 = math.sqrt(
      1.0 / (math.pow(approxPosterior.sig, -2.0) + math
        .pow(performance.sig, -2.0))
    )
    this.copy(
      logisticFactors = logisticFactors1,
      approxPosterior = Rating(mu1, sig1),
      lastUpdate = updateTime
    )
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
    this.copy(
      normalFactor = normalFactor1,
      logisticFactors = logisticFactors1,
      approxPosterior = approxPosterior1
    )
  }

object Player:
  def withRating(id: Long, rating: Rating) =
    Player(id, rating, Vector.empty, rating, OffsetDateTime.now())

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
    weightLimit: Double,
    driftPerSec: Double
):
  def sigPerfAndDrift(contestWeight: Double) = {
    val scaledContestWeight = contestWeight * weightLimit
    val sigPerf = math.sqrt(1.0 + 1.0 / scaledContestWeight) * sigLimit
    val sigDriftSq = scaledContestWeight * math.pow(sigLimit, 2.0)
    (sigPerf, sigDriftSq)
  }

object EloMmrParameters:
  def default =
    EloMmrParameters(80.0, 1.0, 1500.0, 350.0, Int.MaxValue - 1, 0.2, 0)

object EloMmr:
  def updateRound(using
      params: EloMmrParameters
  )(
      scores: Map[Long, Long],
      contestWeight: Double,
      updateTime: OffsetDateTime
  ) =
    val (sigPerf, discreteDrift) = params.sigPerfAndDrift(contestWeight)
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
              .map(p =>
                p.addNoise(
                  math.sqrt(
                    discreteDrift + params.driftPerSec * (Duration
                      .between(p.lastUpdate, updateTime)
                      .toMillis()
                      .max(0) / 1000.0)
                  ),
                  params.transferSpeed
                )
              )
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

            def f(x: Double): Double =
              wins
                .map(_.evalGreater(x))
                .sum
                + ties
                  .map(_.evalEqual(x))
                  .sum
                + loses
                  .map(_.evalLess(x))
                  .sum

            if selfId == 1 then pprint.pprintln(f(1500.0))

            val muPerf = solveItp(f, (-6000.0, 9000.0))

            (
              self
                .truncateHistory(params.maxHistory + 1)
                .updateRating(Rating(muPerf, sigPerf), updateTime),
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
