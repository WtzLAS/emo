package model.emo

import model.*
import zio.ZIO
import java.time.OffsetDateTime

case class RatedMatch(
    id: Long,
    name: String,
    games: Seq[RatedGame],
    users: Map[Long, OsuUser]
)

object RatedMatch:
  def rate(using mmr.EloMmrParameters)(
      `match`: OsuMatch,
      excludedUserIds: Option[Set[Long]] = None,
      contestWeightFunc: Option[OsuGame => Option[Double]] = None
  ): ZIO[mmr.EloMmrStorage, Throwable, RatedMatch] = {
    for {
      filtered <- ZIO.succeed(
        `match`.filterUser(u =>
          !excludedUserIds.getOrElse(Set.empty).contains(u.id)
        )
      )
      ratedGames <- ZIO.foreach(filtered.games)(g =>
        g.rate(contestWeightFunc.getOrElse(_ => None)(g).getOrElse(1.0))
      )
    } yield RatedMatch(
      `match`.id,
      `match`.name,
      ratedGames,
      `match`.users.map(u => (u.id, u)).toMap
    )
  }

extension (m: OsuMatch)
  def rate(using mmr.EloMmrParameters)(
      excludedUserIds: Option[Set[Long]] = None,
      contestWeightFunc: Option[OsuGame => Option[Double]] = None
  ) =
    RatedMatch.rate(m, excludedUserIds, contestWeightFunc)

case class RatedGame(
    beatmap: OsuBeatmap,
    scores: Seq[RatedScore],
    contestWeight: Double
)

object RatedGame:
  def rate(using mmr.EloMmrParameters)(
      game: OsuGame,
      contestWeight: Double
  ): ZIO[mmr.EloMmrStorage, Throwable, RatedGame] = {
    for {
      rawScores <- ZIO.succeed(game.scores.map(s => (s.userId, s.score)).toMap)
      perfs <- mmr.EloMmr.updateRound(rawScores, contestWeight, game.endTime)
      ratedScores <- ZIO.succeed {
        game.scores.map { s =>
          val perf = perfs(s.userId)
          RatedScore(s, perf(0), perf(1), perf(2))
        }
      }
    } yield RatedGame(game.beatmap, ratedScores, contestWeight)
  }

extension (g: OsuGame)
  def rate(using mmr.EloMmrParameters)(contestWeight: Double) =
    RatedGame.rate(g, contestWeight)

case class RatedScore(
    raw: OsuScore,
    muPerf: Double,
    rating: mmr.Rating,
    delta: Double
)
