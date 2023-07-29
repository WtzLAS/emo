package service

import com.ocadotechnology.sttp.oauth2.*
import com.ocadotechnology.sttp.oauth2.common.Scope
import com.ocadotechnology.sttp.oauth2.json.circe.instances.given
import eu.timepit.refined.types.string.NonEmptyString
import io.github.chronoscala.Imports.*
import sttp.client3.SttpBackend
import sttp.model.Uri
import zio.*

trait CachingAccessTokenProvider {
  def requestToken(
      scope: Option[Scope]
  ): ZIO[Any, Throwable, ClientCredentialsToken.AccessTokenResponse]
}

object CachingAccessTokenProvider {
  def requestToken(
      scope: Option[Scope]
  ): ZIO[
    CachingAccessTokenProvider,
    Throwable,
    ClientCredentialsToken.AccessTokenResponse
  ] = {
    ZIO.serviceWithZIO[CachingAccessTokenProvider](_.requestToken(scope))
  }
}

case class CachingAccessTokenProviderImpl(
    lookup: AccessTokenProvider[Task],
    semaphore: Semaphore,
    ref: Ref[
      Option[(ClientCredentialsToken.AccessTokenResponse, OffsetDateTime)]
    ]
) extends CachingAccessTokenProvider {

  override def requestToken(
      scope: Option[Scope]
  ): ZIO[Any, Throwable, ClientCredentialsToken.AccessTokenResponse] = {
    ref.get.flatMap(_ match
      case None => waitAndAcquire(scope)
      case Some((response, expireTime)) =>
        if (OffsetDateTime.now() < expireTime) {
          ZIO.succeed(response)
        } else {
          waitAndAcquire(scope)
        }
    )
  }

  def waitAndAcquire(
      scope: Option[Scope]
  ): ZIO[Any, Throwable, ClientCredentialsToken.AccessTokenResponse] = {
    semaphore.withPermit(
      ref.get.flatMap(_ match
        case None => acquireToken(scope)
        case Some((response, expireTime)) =>
          if (OffsetDateTime.now() < expireTime) {
            ZIO.succeed(response)
          } else {
            acquireToken(scope)
          }
      )
    )
  }

  def acquireToken(
      scope: Option[Scope]
  ): ZIO[Any, Throwable, ClientCredentialsToken.AccessTokenResponse] = {
    for {
      response <- lookup.requestToken(scope)
      _ <- ref.set(
        Some(
          (
            response,
            OffsetDateTime.now() + response.expiresIn.toNanos.nanos
          )
        )
      )
    } yield response
  }
}

object CachingAccessTokenProviderImpl {
  def layer(
      tokenUrl: Uri,
      clientId: NonEmptyString,
      clientSecret: Secret[String]
  ): ZLayer[
    SttpBackend[Task, Any],
    Throwable,
    CachingAccessTokenProviderImpl
  ] = {
    ZLayer {
      for {
        backend <- ZIO.service[SttpBackend[Task, Any]]
        lookup = AccessTokenProvider[Task](tokenUrl, clientId, clientSecret)(
          backend
        )
        semaphore <- Semaphore.make(1)
        ref <- Ref.make(Option.empty)
      } yield CachingAccessTokenProviderImpl(lookup, semaphore, ref)
    }
  }
}
