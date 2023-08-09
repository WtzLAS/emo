package model

import io.circe.*
import io.circe.generic.semiauto.*

case class Match(
    id: Long,
    name: String,
    games: Vector[Game],
    users: Vector[User]
)

given Decoder[Match] = new Decoder[Match] {
  override def apply(cursor: HCursor): Decoder.Result[Match] = {
    for {
      id <- cursor.downField("match").downField("id").as[Long]
      name <- cursor.downField("match").downField("name").as[String]
      events <- cursor.downField("events").as[Vector[Json]]
      games = events
        .flatMap(
          _.hcursor
            .downField("game")
            .as[Game]
            .toOption
        )
      users <- cursor
        .downField("users")
        .as[Vector[User]]
    } yield Match(id, name, games, users)
  }
}

case class Game(
    beatmap: Beatmap,
    scores: Vector[Score]
)

given Decoder[Game] = deriveDecoder

case class Beatmap(
    id: Long,
    setId: Long,
    version: String,
    artist: String,
    title: String,
    creator: String,
    cover: String
):
  override def toString(): String = {
    f"$artist%s - $title%s [$version%s] ($creator%s)"
  }

given Decoder[Beatmap] = new Decoder[Beatmap] {
  override def apply(cursor: HCursor): Decoder.Result[Beatmap] = {
    for {
      id <- cursor.downField("id").as[Long]
      setId <- cursor.downField("beatmapset_id").as[Long]
      version <- cursor.downField("version").as[String]
      beatmapSetCursor = cursor.downField("beatmapset")
      artist <- beatmapSetCursor.downField("artist_unicode").as[String]
      title <- beatmapSetCursor.downField("title_unicode").as[String]
      creator <- beatmapSetCursor.downField("creator").as[String]
      cover <- beatmapSetCursor
        .downField("covers")
        .downField("cover")
        .as[String]
    } yield Beatmap(
      id,
      setId,
      version,
      artist,
      title,
      creator,
      cover
    )
  }
}

case class Score(accuracy: Double, mods: Vector[Mod], score: Long, userId: Long)

given Decoder[Score] =
  Decoder.forProduct4("accuracy", "mods", "score", "user_id")(Score.apply)

enum Mod:
  case DT, EZ, HD, HR, NF, FL, RX

  override def toString(): String =
    this match
      case DT => "DT"
      case EZ => "EZ"
      case HD => "HD"
      case HR => "HR"
      case NF => "NF"
      case FL => "FL"
      case RX => "RX"

given Decoder[Mod] = new Decoder[Mod] {
  def apply(c: HCursor): Decoder.Result[Mod] = {
    c.as[String].flatMap {
      case "DT" => Right(Mod.DT)
      case "EZ" => Right(Mod.EZ)
      case "HD" => Right(Mod.HD)
      case "HR" => Right(Mod.HR)
      case "NF" => Right(Mod.NF)
      case "FL" => Right(Mod.FL)
      case "RX" => Right(Mod.RX)
      case e: String =>
        Left(
          DecodingFailure(
            DecodingFailure.Reason.CustomReason(s"Unsupported Mod: $e"),
            c
          )
        )
    }
  }
}

case class User(
    id: Long,
    username: String,
    avatarUrl: String
)

given Codec[User] =
  Codec.forProduct3("id", "username", "avatar_url")(User.apply)(u =>
    (u.id, u.username, u.avatarUrl)
  )
