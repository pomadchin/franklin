package geotrellis.stac

import com.azavea.stac4s.syntax._
import cats.effect._
import com.azavea.stac4s.extensions.layer.LayerItemExtension
import org.http4s.client.blaze.BlazeClientBuilder
import org.http4s.client.middleware.Logger

import scala.concurrent.ExecutionContext.Implicits.global

object Main extends IOApp {
  def run(args: List[String]): IO[ExitCode] =
    BlazeClientBuilder[IO](global).resource.use { httpClient =>
      val http = Logger(logBody = false, logHeaders = false)(httpClient)
      val api = new Http4sApi[IO](http)
      val res = api.search()
      res.map { list =>
        list.map { i =>
          val k = i.getExtensionFields[LayerItemExtension]
          println("---")
          println(k)
          println("---")
        }
      }
    }.as(ExitCode.Success)
}
