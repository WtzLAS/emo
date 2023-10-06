package service

import com.ocadotechnology.sttp.oauth2.common.Scope
import io.circe.*
import io.circe.parser.*
import io.circe.syntax.*
import model.{*, given}
import sttp.client3.*
import sttp.client3.circe.*
import zio.*

trait OsuApi:
  def getMatch(id: Long): ZIO[Any, Throwable, OsuMatch]
  def getUser(id: Long): ZIO[Any, Throwable, OsuUserExtended]

object OsuApi:
  def getMatch(id: Long): ZIO[OsuApi, Throwable, OsuMatch] =
    ZIO.serviceWithZIO[OsuApi](_.getMatch(id))

  def getUser(id: Long): ZIO[OsuApi, Throwable, OsuUserExtended] =
    ZIO.serviceWithZIO[OsuApi](_.getUser(id))

case class OsuApiImpl(
    accessTokenProvider: CachingAccessTokenProvider,
    backend: SttpBackend[Task, Any]
) extends OsuApi {
  override def getUser(id: Long): ZIO[Any, Throwable, OsuUserExtended] = {
    def apiUrl = uri"https://osu.ppy.sh/api/v2/users/$id/osu?key=id"
    inline def scope = Some(Scope.unsafeFrom("public"))

    for {
      cache <- ZIO.attemptBlocking {
        val wd = os.pwd / "cache" / "users"
        os.makeDir.all(wd)
        if (os.exists(wd / s"$id.json")) {
          Some(os.read(wd / s"$id.json"))
        } else {
          None
        }
      }
      result <- cache match
        case Some(value) =>
          ZIO.fromEither(decode[OsuUserExtended](value))
        case None =>
          for {
            _ <- ZIO.logInfo(s"Fetching user $id from ppy")
            token <- accessTokenProvider.requestToken(scope)
            request = basicRequest.auth
              .bearer(token.accessToken.value)
              .response(asJson[Json])
              .get(apiUrl)
            response <- request.send(backend)
            _ <- ZIO.attemptBlocking {
              val wd = os.pwd / "cache" / "users"
              response.body match
                case Left(value) => ()
                case Right(value) =>
                  os.write.over(wd / s"$id.json", value.toString)
            }
            result <- ZIO.fromEither(response.body.flatMap(_.as[OsuUserExtended]))
          } yield result
    } yield result
  }

  override def getMatch(
      id: Long
  ): ZIO[Any, Throwable, OsuMatch] = {
    def apiUrl = uri"https://osu.ppy.sh/api/v2/matches/$id"
    inline def scope = Some(Scope.unsafeFrom("public"))

    for {
      cache <- ZIO.attemptBlocking {
        val wd = os.pwd / "cache" / "matches"
        os.makeDir.all(wd)
        if (os.exists(wd / s"$id.json")) {
          Some(os.read(wd / s"$id.json"))
        } else {
          None
        }
      }
      result <- cache match
        case Some(value) =>
          ZIO.fromEither(decode[OsuMatch](value))
        case None =>
          for {
            _ <- ZIO.logInfo(s"Fetching match $id from ppy")
            token <- accessTokenProvider.requestToken(scope)
            request = basicRequest.auth
              .bearer(token.accessToken.value)
              .response(asJson[Json])
              .get(apiUrl)
            response <- request.send(backend)
            _ <- ZIO.attemptBlocking {
              val wd = os.pwd / "cache" / "matches"
              os.makeDir.all(wd)
              response.body match
                case Left(value) => ()
                case Right(value) =>
                  os.write.over(wd / s"$id.json", value.toString)
            }
            result <- ZIO.fromEither(response.body.flatMap(_.as[OsuMatch]))
          } yield result
    } yield result
  }
}

object OsuApiImpl:
  def layer(): ZLayer[
    CachingAccessTokenProvider & SttpBackend[Task, Any],
    Throwable,
    OsuApiImpl
  ] = {
    ZLayer {
      for {
        accessTokenProvider <- ZIO.service[CachingAccessTokenProvider]
        backend <- ZIO.service[SttpBackend[Task, Any]]
      } yield OsuApiImpl(accessTokenProvider, backend)
    }
  }
