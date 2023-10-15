import com.ocadotechnology.sttp.oauth2.Secret
import eu.timepit.refined.collection.NonEmpty
import lucuma.refined.*
import model.*
import model.emo.*
import model.mmr.EloMmr
import model.mmr.EloMmrStorage
import service.*
import sttp.client3.*
import sttp.client3.httpclient.zio.HttpClientZioBackend
import java.time.OffsetDateTime
import io.github.chronoscala.Imports._
import zio.ZIOAppDefault
import zio.ZIO
import zio.Duration
import zio.Random

object Main extends ZIOAppDefault {
  val tokenUrl = uri"https://osu.ppy.sh/oauth/token"
  val clientId = "114514".refined[NonEmpty]
  val clientSecret = Secret("old ones in the git history have been revoked")

  def run = program3.provide(
    HttpClientZioBackend.layer(),
    CachingAccessTokenProviderImpl.layer(tokenUrl, clientId, clientSecret),
    OsuApiImpl.layer(),
    model.mmr.ConcurrentMapStorage.layer
  )

  // val ids = Vector(91801182, 91865778, 91456788, 91460744, 91517880, 91532008,
  //   91098751, 91105128, 91110899, 91118282, 91118278, 91141152, 91145501,
  //   91167258, 91168915, 90664912, 90713398, 90730667, 90735710, 90750093,
  //   90754244, 90776350, 90795660, 90798956, 90799071, 90802153, 90802301,
  //   90805086, 90822624, 90954061, 90349827, 90370589, 90370929, 90370226,
  //   90373397, 90381476, 90385407, 90385405, 90385403, 90385591, 90389224,
  //   90396245, 90407086, 90409718, 90411955, 90417905, 90426376, 90426159,
  //   90428611, 90431069, 90431565, 90433161, 90433506, 90433508, 90442198,
  //   90442195, 90451042, 90491610, 90000968, 90006529, 90006521, 90010297,
  //   90010059, 90012374, 90012550, 90015402, 90022369, 90025562, 90034080,
  //   90045914, 90048283, 90058274, 90061876, 90070513, 90073561, 90076404,
  //   90079797, 90065138, 90080188, 90128921, 90134682, 90139842, 89555170,
  //   89595366, 89611626, 89612952, 89614029, 89617201, 89620054, 89622660,
  //   89622673, 89635728, 89646086, 89650373, 89669002, 89671085, 89671687,
  //   89673897, 89676440, 89677184, 89679656, 89682748, 89683718, 89685117,
  //   89731229, 89733422, 89733807, 89750968).sorted
  val ids = Vector(110694420, 110695711, 110697447, 110699305, 110709294,
    110710108, 110711279, 110712641, 110722240, 110723364, 110733145, 110733930,
    110735100, 110742023, 110759389, 110760868, 110762615, 110774513, 110776003,
    110794189, 110797184, 110811944, 110846345, 110875660, 110877994, 110879510,
    110885987, 110890591, 110891734, 110893521).sorted

  val beatmapIds = Vector(3682179, 4241682, 2946166, 2064271, 2067426, 2946442,
    756508, 1863256, 2911393, 3326709).map(_.toLong)

  val banUserIds = Set(15184992L, 12166015L)

  val startMatchId = 110891734
  val targetTime = OffsetDateTime.parse("2023-10-15T14:00:00+00:00")

  given model.mmr.EloMmrParameters = model.mmr.EloMmrParameters.default

  val program1 = for {
    _ <- ZIO.foreach(ids) { matchId =>
      for {
        `match` <- OsuApi.getMatch(matchId)
        result <- `match`.rate()
        _ <- ZIO.attempt {
          val wd = os.pwd / "cache" / "results" / "matches"
          os.makeDir.all(wd)
          os.write.over(wd / s"$matchId.html", result.toPage)
        }
      } yield ()
    }
  } yield ()

  val program2 = for {
    _ <- ZIO.iterate((startMatchId + 1, 1))((id, delta) =>
      id <= startMatchId + 100000
    )((id, delta) =>
      (for {
        m <- OsuApi.getMatch(id)
        _ <-
          if (m.name.startsWith("KIT")) {
            ZIO.logError(s"Found $id = ${m.name}") *> ZIO.fail(1)
          } else { ZIO.unit }
        _ <- ZIO.sleep(Duration.fromMillis(100))
        newS <-
          if (m.startTime < targetTime - 30.minutes) {
            ZIO.succeed((id + (delta * 2).min(200), (delta * 2).min(200)))
          } else if (m.startTime < targetTime) {
            ZIO.succeed((id + 1, 1))
          } else {
            ZIO.succeed((id - 1, 1))
          }
      } yield newS).catchAll {
        case e: Int => ZIO.fail(e)
        case _ =>
          Random.nextBoolean.map(b =>
            (
              id + (if (b) { 1 }
                    else { -1 }),
              1
            )
          )
      }
    )
  } yield ()

  val program3 = for {
    tmatch <- ZIO
      .collectAll(ids.map(OsuApi.getMatch(_)))
      .map(mas =>
        (
          mas
            .flatMap(_.games)
            .groupMapReduce(_.beatmap.id)(identity)((g1, g2) =>
              OsuGame(
                g1.id,
                g1.startTime,
                g1.endTime,
                g1.beatmap,
                g1.scores.filter(_.score != 0) ++ g2.scores.filter(_.score != 0)
              )
            ),
          mas.flatMap(_.users)
        )
      )
      .map((games, users) =>
        OsuMatch(
          1,
          OffsetDateTime.now(),
          OffsetDateTime.now(),
          "Summary",
          beatmapIds.map(games(_)),
          users
        )
      )
    rmatch <- tmatch.rate(excludedUserIds = Some(banUserIds))
    _ <- ZIO.succeed {
      val wd = os.pwd / "cache" / "results" / "matches"
      os.makeDir.all(wd)
      os.write.over(wd / s"1.html", rmatch.toPage)
    }
    ps <- EloMmrStorage.loadAllPlayers
    page <- ps.toPage
    _ <- ZIO.attempt {
      val wd = os.pwd / "cache"
      os.makeDir.all(wd)
      os.write.over(wd / "leaderboard.html", page)
    }
  } yield ()
}