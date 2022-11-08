//> using lib "co.fs2::fs2-io:3.3.0"
//> using lib "io.circe::circe-parser:0.14.3"

import cats._
import cats.effect._
import cats.effect.std.Console
import cats.effect.unsafe.implicits.global
import cats.implicits._
import fs2.{Pipe, Stream}
import fs2.io.file.{Files, Path}
import fs2.text
import io.circe._
import io.circe.Decoder.Result
import io.circe.parser._

val input = Path("./outbox.json")
val output = Path("./src/hugo/content/notes/toots")

val me = "https://mastodon.social/users/rossabaker"

def parseToots[F[_]: ApplicativeThrow: Files]: Stream[F, Json] =
  Files[F]
    .readAll(input)
    .through(text.utf8.decode)
    .foldMonoid
    .map(parse)
    .rethrow
    .map(_.hcursor.get[Vector[Json]]("orderedItems"))
    .rethrow
    .flatMap(Stream.emits)

def created[F[_]: ApplicativeThrow](json: Json): F[Boolean] =
  json.hcursor
    .get[String]("type")
    .map(_ === "Create")
    .liftTo[F]

def topLevel[F[_]: ApplicativeThrow](json: Json): F[Boolean] =
  json.hcursor
    .downField("object")
    .get[Json]("inReplyTo")
    .map(_.isNull)
    .liftTo[F]

def onlyMe[F[_]: ApplicativeThrow](json: Json): F[Boolean] =
  json.hcursor
    .downField("object")
    .get[Vector[String]]("cc")
    .map(_ === Vector(s"${me}/followers"))
    .liftTo[F]

def getId[F[_]: ApplicativeThrow](json: Json): F[Long] =
  json.hcursor
    .downField("object")
    .get[String]("id")
    .map(_.split("/").last.toLong)
    .liftTo[F]

def getHashtags(json: Json): Decoder.Result[Vector[String]] =
  json.hcursor
    .downField("object")
    .get[Vector[Map[String, String]]]("tag")
    .map(_.collect {
      case m if m.get("type") === Some("Hashtag") =>
        "\"" + (m("name") match {
          case "#GoIU" => "hoosiers"
          case "#italianbeef" => "italian-beef"
          case "#opentelemetry" => "open-telemetry"
          case "#cw" => "content-warning"
          case "#contentwarning" => "content-warning"
          case "#trickortreat" => "trick-or-treat"
          case "#GoingViral" => "going-viral"
          case other =>
            other.split("[\\W_]+")
              .filter(_.nonEmpty)
              .map(_.toLowerCase(java.util.Locale.ROOT))
            .mkString("-")
        }) + "\""
    })

def renderToot[F[_]: ApplicativeThrow](json: Json): F[String] =
  (json.hcursor.downField("object").get[String]("published"),
   json.hcursor.downField("object").get[String]("id"),
   json.hcursor.downField("object").get[String]("content"),
   getHashtags(json)
  ).mapN(
    (published, id, content, hashtags) =>
       List(
         "+++",
         s"date = ${published}",
         s"""canonical = "${id}"""",
         s"""tags = [ ${hashtags.mkString(", ")} ]""",
         "+++",
         "",
         content,
         ""
       ).mkString("\n")
  )
  .liftTo[F]

def handleToot[F[_]: MonadThrow: Files](json: Json): Stream[F, Unit] =
  for {
    id <- Stream.eval(getId(json))
    _ <- Stream.eval(renderToot(json))
      .through(text.utf8.encode)
      .through(Files[F].writeAll(output / s"${id}.md"))
  } yield ()

def program[F[_]: Sync: Files]: F[Unit] =
  Files[F].createDirectories(output) >>
  parseToots
    .evalFilter(created[F])
    .evalFilter(topLevel[F])
    .evalFilter(onlyMe[F])
    .flatMap(handleToot[F])
    .compile
    .drain

object Main extends IOApp.Simple {
  def run: IO[Unit] = program[IO]
}
