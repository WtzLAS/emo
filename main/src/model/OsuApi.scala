package model

import io.circe.*
import io.circe.derivation.*
import io.github.chronoscala.Imports.*

given Configuration = Configuration.default.withSnakeCaseMemberNames

enum OsuMod derives ConfiguredEnumDecoder:
  case EZ, NF, HT, HR, SD, PF, DT, NC, HD, FL, RL, AP, SO, AT, SV2

case class OsuScore(
    id: Option[Long],
    accuracy: Double,
    mods: Set[OsuMod],
    score: Long,
    userId: Long
) derives ConfiguredDecoder

case class OsuBeatmapSetCovers(
    cover: String,
    `cover@2x`: String,
    card: String,
    `card@2x`: String,
    list: String,
    `list@2x`: String,
    slimcover: String,
    `slimcover@2x`: String
) derives Decoder

case class OsuBeatmapSet(
    id: Long,
    artist: String,
    artistUnicode: String,
    title: String,
    titleUnicode: String,
    creator: String,
    covers: OsuBeatmapSetCovers
) derives ConfiguredDecoder

case class OsuBeatmap(
    id: Long,
    version: String,
    beatmapset: OsuBeatmapSet
) derives ConfiguredDecoder:
  override def toString(): String = s"${beatmapset.artistUnicode} - ${beatmapset.titleUnicode} [$version] (${beatmapset.creator})"

case class OsuGame(
    id: Long,
    startTime: OffsetDateTime,
    endTime: OffsetDateTime,
    beatmap: OsuBeatmap,
    scores: Seq[OsuScore]
) derives ConfiguredDecoder

case class OsuUser(
    id: Long,
    username: String,
    isActive: Boolean,
    isBot: Boolean,
    isDeleted: Boolean,
    isOnline: Boolean,
    isSupporter: Boolean,
    avatarUrl: String,
    countryCode: String
) derives ConfiguredDecoder

case class OsuMatch(
    id: Long,
    startTime: OffsetDateTime,
    endTime: OffsetDateTime,
    name: String,
    games: Seq[OsuGame],
    users: Seq[OsuUser]
):
  def filterUser(p: OsuUser => Boolean) = this.copy(
    games = games.map(g =>
      g.copy(scores = g.scores.filter(s => p(users.find(_.id == s.userId).get)))
    ),
    users = users.filter(p)
  )

given Decoder[OsuMatch] = Decoder.instance(hcursor =>
  for {
    id <- hcursor.downField("match").downField("id").as[Long]
    startTime <- hcursor
      .downField("match")
      .downField("start_time")
      .as[OffsetDateTime]
    endTime <- hcursor
      .downField("match")
      .downField("end_time")
      .as[OffsetDateTime]
    name <- hcursor.downField("match").downField("name").as[String]
    games <- hcursor
      .downField("events")
      .as[Seq[Json]]
      .map(
        _.flatMap(
          _.hcursor
            .downField("game")
            .as[OsuGame]
            .toOption
        )
      )
    users <- hcursor.downField("users").as[Seq[OsuUser]]
  } yield OsuMatch(id, startTime, endTime, name, games, users)
)

case class OsuUserRankHighest(
    rank: Long,
    updatedAt: OffsetDateTime
) derives ConfiguredDecoder

case class OsuUserStatistics(
    pp: Double,
    globalRank: Long,
    countryRank: Long
) derives ConfiguredDecoder

case class OsuUserExtended(
    id: Long,
    username: String,
    isActive: Boolean,
    isBot: Boolean,
    isDeleted: Boolean,
    isOnline: Boolean,
    isSupporter: Boolean,
    avatarUrl: String,
    countryCode: String,
    joinDate: OffsetDateTime,
    previousUsernames: Seq[String],
    rankHighest: OsuUserRankHighest,
    statistics: OsuUserStatistics
) derives ConfiguredDecoder
