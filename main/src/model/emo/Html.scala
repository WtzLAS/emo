package model.emo

import model.mmr.Player
import service.OsuApi
import zio.ZIO

extension (ratedMatch: RatedMatch)
  def toHtml = {
    import scalatags.Text.all.*

    div(
      cls := "container",
      a(
        href := s"https://osu.ppy.sh/community/matches/${ratedMatch.id}",
        p(cls := "title has-text-centered mb-6", ratedMatch.name)
      ),
      for (ratedGame <- ratedMatch.games)
        yield div(
          cls := "card block",
          div(
            cls := "card-image",
            figure(
              cls := "image",
              img(
                src := ratedGame.beatmap.beatmapset.covers.cover
              )
            )
          ),
          div(
            cls := "card-content",
            a(
              href := s"https://osu.ppy.sh/b/${ratedGame.beatmap.id}",
              p(
                cls := "subtitle has-text-centered",
                ratedGame.beatmap.toString()
              )
            ),
            table(
              cls := "table container",
              thead(
                tr(
                  th("#"),
                  th("Player"),
                  th("Mods"),
                  th("Score"),
                  th(
                    tag("abbr")(
                      title := "The estimated rating in this game",
                      "p"
                    )
                  ),
                  th(
                    tag("abbr")(
                      title := "Rating and deviation after this game",
                      "μ±σ"
                    )
                  ),
                  th(
                    tag("abbr")(
                      title := "Rating change in this game",
                      "Δμ"
                    )
                  )
                )
              ),
              tbody(
                for (
                  (ratedScore, r) <- ratedGame.scores
                    .sortBy(_.muPerf)
                    .reverse
                    .zipWithIndex
                )
                  yield tr(
                    td(s"${r + 1}"),
                    th(
                      a(
                        href := s"https://osu.ppy.sh/u/${ratedScore.raw.userId}",
                        ratedMatch.users(ratedScore.raw.userId).username
                      )
                    ),
                    td("+" + ratedScore.raw.mods.mkString("")),
                    td(ratedScore.raw.score),
                    td(f"${ratedScore.muPerf}%.2f"),
                    td(
                      f"${ratedScore.rating.mu}%.2f±${ratedScore.rating.sig}%.2f"
                    ),
                    td(
                      if (ratedScore.delta > 10.0) {
                        cls := "has-text-success"
                      } else if (ratedScore.delta < -10.0) {
                        cls := "has-text-danger"
                      } else {
                        cls := "has-text-grey"
                      },
                      f"${ratedScore.delta}%+.2f"
                    )
                  )
              )
            )
          )
        )
    )
  }

  def toPage = {
    import scalatags.Text.all.*

    "<!DOCTYPE html>" +
      html(
        head(
          meta(charset := "utf-8"),
          meta(
            name := "viewport",
            content := "width=device-width, initial-scale=1"
          ),
          link(
            rel := "stylesheet",
            href := "https://cdn.jsdelivr.net/npm/bulma@0.9.4/css/bulma.min.css"
          ),
          tag("title")("Emo Rated Match Detail")
        ),
        body(
          tag("section")(
            cls := "section",
            ratedMatch.toHtml
          )
        )
      )
  }

extension (players: Vector[Player])
  def toPage: ZIO[OsuApi, Throwable, String] = {
    import scalatags.Text.all.*

    for {
      fetched <- ZIO
        .collectAllPar(players.map { p =>
          for {
            user <- OsuApi.getUser(p.id)
          } yield (p, user)
        })
        .map(_.sortBy(-_(0).approxPosterior.mu).zipWithIndex)
    } yield "<!DOCTYPE html>" +
      html(
        head(
          meta(charset := "utf-8"),
          meta(
            name := "viewport",
            content := "width=device-width, initial-scale=1"
          ),
          link(
            rel := "stylesheet",
            href := "https://cdn.jsdelivr.net/npm/bulma@0.9.4/css/bulma.min.css"
          ),
          tag("title")("Emo Leaderboard")
        ),
        body(
          tag("section")(
            cls := "section",
            div(
              cls := "container",
              table(
                cls := "table container",
                thead(
                  tr(
                    th("#"),
                    th("Player"),
                    th("Global Rank"),
                    th("pp"),
                    th("μ±σ")
                  )
                ),
                tbody(
                  for ((p, u), r) <- fetched
                  yield tr(
                    td(
                      s"${r + 1}"
                    ),
                    td(
                      a(
                        href := s"https://osu.ppy.sh/u/${p.id}",
                        u.username
                      )
                    ),
                    td(
                      s"${u.statistics.globalRank}"
                    ),
                    td(
                      f"${u.statistics.pp}%.2f"
                    ),
                    td(
                      f"${p.approxPosterior.mu}%.2f±${p.approxPosterior.sig}%.2f"
                    )
                  )
                )
              )
            )
          )
        )
      )
  }
