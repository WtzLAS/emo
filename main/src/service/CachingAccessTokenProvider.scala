package service

import com.ocadotechnology.sttp.oauth2.*
import com.ocadotechnology.sttp.oauth2.common.Scope
import com.ocadotechnology.sttp.oauth2.json.circe.instances.given
import eu.timepit.refined.types.string.NonEmptyString
import io.github.chronoscala.Imports.*
import sttp.client3.SttpBackend
import sttp.model.Uri
import zio.*
import zio.cache.*

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
    cache: Cache[Option[
      Scope
    ], Throwable, (ClientCredentialsToken.AccessTokenResponse, OffsetDateTime)]
) extends CachingAccessTokenProvider {
  override def requestToken(
      scope: Option[Scope]
  ): ZIO[Any, Throwable, ClientCredentialsToken.AccessTokenResponse] = for {
    cached <- cache.get(scope)
    updated <-
      if (cached(1) < OffsetDateTime.now()) {
        cache.invalidateAll *> requestToken(scope)
      } else { ZIO.succeed(cached(0)) }
  } yield updated
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
        provider = AccessTokenProvider[Task](tokenUrl, clientId, clientSecret)(
          backend
        )
        lookup = Lookup((key: Option[Scope]) =>
          provider
            .requestToken(key)
            .map(res => (res, OffsetDateTime.now() + res.expiresIn.toMillis.millis))
        )
        cache <- Cache.make(1, 1.days, lookup)
      } yield CachingAccessTokenProviderImpl(cache)
    }
  }
}
