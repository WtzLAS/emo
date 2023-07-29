package model.emo

import model.*
import zio.*

case class RatedMatch(
    id: Long,
    name: String,
    games: Vector[RatedGame],
    users: Map[Long, User]
)

object RatedMatch:
  def rate(using mmr.EloMmrParameters)(
      `match`: Match
  ): ZIO[mmr.EloMmrStorage, Throwable, RatedMatch] = {
    for {
      ratedGames <- ZIO.foreach(`match`.games)(_.rate(1.0))
    } yield RatedMatch(
      `match`.id,
      `match`.name,
      ratedGames,
      `match`.users.map(u => (u.id, u)).toMap
    )
  }

extension (m: Match)
  def rate(using mmr.EloMmrParameters) =
    RatedMatch.rate(m)

case class RatedGame(
    beatmap: Beatmap,
    scores: Vector[RatedScore]
)

object RatedGame:
  def rate(using mmr.EloMmrParameters)(
      game: Game,
      contestWeight: Double
  ): ZIO[mmr.EloMmrStorage, Throwable, RatedGame] = {
    for {
      rawScores <- ZIO.succeed(game.scores.map(s => (s.userId, s.score)).toMap)
      perfs <- mmr.EloMmr.updateRound(rawScores, contestWeight)
      ratedScores <- ZIO.succeed {
        game.scores.map { s =>
          val perf = perfs(s.userId)
          RatedScore(s, perf(0), perf(1), perf(2))
        }
      }
    } yield RatedGame(game.beatmap, ratedScores)
  }

extension (g: Game)
  def rate(using mmr.EloMmrParameters)(contestWeight: Double) =
    RatedGame.rate(g, contestWeight)

case class RatedScore(
    raw: Score,
    muPerf: Double,
    rating: mmr.Rating,
    delta: Double
)
