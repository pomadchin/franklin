package com.azavea.franklin.api

import io.circe.{Decoder, Encoder, Json}
import io.circe.generic.semiauto._
import cats.implicits._

case class Query(query: Json) {

  def traverseThroughProperties[T](op: String)(layersTransform: List[String] => T): List[T] = {
    val hc = query.hcursor
    hc.keys.toList.flatMap { keys =>
      keys.toList
        .flatMap { key => hc.downField(key).downField(op).as[List[String]].toList }
        .map(layersTransform)
    }
  }
}

object Query {
  implicit val queryEncoder: Encoder[Query] = deriveEncoder[Query]
  implicit val queryDecoder: Decoder[Query] = deriveDecoder[Query]
}
