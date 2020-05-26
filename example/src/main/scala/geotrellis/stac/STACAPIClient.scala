package geotrellis.stac

// import cats.Id
import cats.Id
import cats.syntax.functor._
import cats.syntax.either._
import cats.effect.Sync
import com.azavea.stac4s.{Bbox, StacItem, TemporalExtent, TwoDimBbox}
import geotrellis.raster.RasterSource
import geotrellis.store.query.{Query, QueryF}
import geotrellis.store.query.QueryF.{All, And, At, Between, Contains, Covers, Intersects, Nothing, Or, WithName, WithNames}
import org.http4s.{Request, Uri, Query => HQuery}
import org.http4s.client.Client
import org.http4s.Method.GET
import org.http4s.Uri._
import org.http4s._
import io.circe.{Encoder, Json}
import io.circe.syntax._
import org.http4s.circe._
import io.circe.JsonObject
import geotrellis.vector._
import higherkindness.droste.{Algebra, scheme}

// import cats.Id

trait RepositoryM[M[_], G[_]] {
  def find(query: Query): M[G[RasterSource]]
}

trait Repository[G[_]] extends RepositoryM[Id, G]

case class STACRepository[F[_]: Sync](store: Http4sApi[F]) extends RepositoryM[F, List] {
  def find(query: Query): F[List[RasterSource]] =
    store.searchRS(scheme.cata(FilterSearch.algebra).apply(query))
}

case class FilterSearch(
  bbox: Option[Bbox] = None,
  datetime: Option[TemporalExtent] = None,
  intersects: Option[Geometry] = None,
  collections: List[String] = Nil,
  items: List[String] = Nil,
  limit: Option[Int] = None,
  next: Option[String] = None,
  layers: Set[String] = Set.empty
) {

  import FilterSearch._

  def toQuery: HQuery = Query.fromVector(
    Vector(
      "bbox" -> toJsonString(bbox),
      "datetime" -> toJsonString(datetime),
      "intersects" -> toJsonString(intersects),
      "collections" -> toJsonStringList(collections),
      "items" -> toJsonStringList(items),
      "limit" -> toJsonString(limit),
      "next" -> toJsonString(next),
      "query" -> Option(JsonObject(
        "query" -> JsonObject(
          "layer:ids" -> JsonObject(
            "eq" -> layers.asJson
          ).asJson
        ).asJson
      ).asJson.noSpaces)
    ).filter(_._2.nonEmpty)
  )

  def and(other: FilterSearch): FilterSearch = {
    FilterSearch(
      bbox = bbox orElse other.bbox,
      datetime = (datetime, other.datetime) match {
        case (Some(left), Some(right)) =>
          val (lmin, lmax) = left.value.min -> left.value.max
          val (rmin, rmax) = right.value.min -> right.value.max
          Option(TemporalExtent.unsafeFrom(List(
            List(lmin, rmin).max,
            List(lmax, rmax).min
          )))
        case (l @ Some(_), _) => l
        case (_, r @ Some(_)) => r
        case _ => None
      },
      intersects = (intersects, other.intersects) match {
        case (Some(l), Some(r)) => Some(l.intersection(r))
        case (l @ Some(_), _) => l
        case (_,  r @ Some(_)) => r
        case _ => None
      },
      collections = (collections ++ other.collections).distinct,
      items = (collections ++ other.collections).distinct,
      limit = List(limit, other.limit).min,
      next = List(next, other.next).max,
      layers = layers ++ other.layers
    )
  }
}

object FilterSearch {
  def toJsonString[T: Encoder](t: Option[T]): Option[String] =
    t.map(_.asJson.noSpaces)

  def toJsonStringList[T: Encoder](t: List[T]): Option[String] =
    if(t.isEmpty) None else Option(t.asJson.noSpaces)

  // dont use the word demo it confuses Eugene
  // and me
  //
  // Question for James and Chris
  // can it sort htwe return items?
  // mb it a franklin?>
  // limit + sort can fix
  // sort by time, to get the most recent?
  //
  // how this should look like?
  def algebra: Algebra[QueryF, FilterSearch] = Algebra {
    case Nothing() => FilterSearch() // unsupported node
    case All() => FilterSearch()
    case WithName(name) => FilterSearch(layers = Set(name))
    case WithNames(names) => FilterSearch(layers = names)
    case At(t, _) => FilterSearch(datetime = Some(TemporalExtent(t.toInstant, None)))
    case Between(t1, t2, _) => FilterSearch(datetime = Some(TemporalExtent(t1.toInstant, t2.toInstant)))
    case Intersects(e) => FilterSearch(intersects = Some(e.extent.toPolygon))
    case Covers(e) => FilterSearch(
      bbox = Some(TwoDimBbox(e.extent.xmin, e.extent.xmax, e.extent.ymin, e.extent.ymax))
    ) // unsupported node ?
    case Contains(_) => FilterSearch() // unsupported node
    case And(e1, e2) => e1 and e2
    case Or(_, _) => FilterSearch() // unsupported node
  }
}

trait Api[F[_]] {
  def search(filter: FilterSearch): F[List[StacItem]]
  def searchRS(filter: FilterSearch): F[List[RasterSource]]
}

case class Http4sApi[F[_]: Sync](
  client: Client[F],
  host: String = "localhost",
  port: Int = 9090
) extends Api[F] {
  private def getRequest: Request[F] = Request[F]().withMethod(GET)

  def search(filter: FilterSearch = FilterSearch()): F[List[StacItem]] = {
    val uri = Uri(
      Some(Scheme.http),
      Some(Authority(host = RegName(host), port = Some(port))),
      "/search",
      filter.toQuery
    )

    client
      .expect(getRequest.withUri(uri))(jsonOf[F, Json])
      .map(_.hcursor.downField("features").as[List[StacItem]].bimap(_ => Nil, identity).merge)
  }

  def searchRS(filter: FilterSearch = FilterSearch()): F[List[RasterSource]] =
    search(filter).map { _ flatMap { item =>
      item.assets.values.map(a => RasterSource(a.href))
    } }
}

object Http4sApi {

}
