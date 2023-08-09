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
  def getMatch(id: Long): ZIO[Any, Throwable, Match]

  def getUser(id: Long): ZIO[Any, Throwable, User]

object OsuApi:
  def getMatch(id: Long): ZIO[OsuApi, Throwable, Match] =
    ZIO.serviceWithZIO[OsuApi](_.getMatch(id))

  def getUser(id: Long): ZIO[OsuApi, Throwable, User] =
    ZIO.serviceWithZIO[OsuApi](_.getUser(id))

case class OsuApiImpl(
    accessTokenProvider: CachingAccessTokenProvider,
    backend: SttpBackend[Task, Any]
) extends OsuApi {
  override def getUser(id: Long): ZIO[Any, Throwable, User] = {
    def apiUrl = uri"https://osu.ppy.sh/api/v2/users/$id?key=id"
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
          ZIO.fromEither(decode[User](value))
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
            result <- ZIO.fromEither(response.body.flatMap(_.as[User]))
          } yield result
    } yield result
  }

  override def getMatch(
      id: Long
  ): ZIO[Any, Throwable, Match] = {
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
          ZIO.fromEither(decode[Match](value))
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
            result <- ZIO.fromEither(response.body.flatMap(_.as[Match]))
            _ <- ZIO.attemptBlocking {
              val wd = os.pwd / "cache" / "users"
              os.makeDir.all(wd)
              for u <- result.users do {
                val p = wd / s"${u.id}.json"
                if (!os.exists(p)) {
                  os.write.over(p, u.asJson.toString)
                }
              }
            }
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
