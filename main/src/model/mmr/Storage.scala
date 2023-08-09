package model.mmr

import zio.ZIO
import zio.ZLayer

import java.util.concurrent.ConcurrentHashMap
import scala.jdk.CollectionConverters.given

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
