package network

import helpers.requestException
import io.circe.Json
import io.circe.parser.parse
import scalaj.http.{BaseHttp, HttpConstants}

import scala.util.{Failure, Success, Try}

object Request{
  object RaffleHttp extends BaseHttp (None, HttpConstants.defaultOptions, HttpConstants.utf8, 4096, "Mozilla/5.0 (X11; OpenBSD amd64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/43.0.2357.81 Safari/537.36", true)
  private val defaultHeader: Seq[(String, String)] = Seq[(String, String)](("Accept", "application/json"))
  private val defaultPostHeader: Seq[(String, String)] = Seq[(String, String)](("Accept", "application/json"), ("Content-Type", "application/json"))
  def httpGetWithError(url: String, headers: Seq[(String, String)] = defaultHeader): Either[Throwable, Json] = {
    Try {
      val responseReq = RaffleHttp(url).headers(defaultHeader).asString
      (responseReq.code, responseReq)
    }
    match{
      case Success((200, responseReq)) => parse(responseReq.body)
      case Success((responseHttpCode, responseReq)) => Left(requestException(s"returned a error with http code $responseHttpCode and error ${responseReq.throwError}"))
      case Failure(exception) => Left(exception)
    }
  }

  def httpGet(url: String): Json = {
    httpGetWithError(url) match {
      case Right(json) => json
      case Left(ex) => throw ex
    }
  }

  def httpPostWithError(url: String, data: String): Either[Throwable, Json] = {
    Try {
      val responseReq = RaffleHttp(url).postData(data).headers(defaultPostHeader).asString
      (responseReq.is2xx, responseReq.code, responseReq)
    }
    match{
      case Success((true, responseHttpCode, responseReq)) => if (responseHttpCode == 204) parse(Json.Null.toString()) else parse(responseReq.body)
      case Success((false, responseHttpCode, responseReq)) => Left(new Exception(s"returned a error with http code $responseHttpCode and error ${responseReq.throwError}"))
      case Failure(exception) => Left(exception)
    }
  }

  def httpPost(url: String, data: String): Json = {
    httpPostWithError(url, data) match {
      case Right(json) =>
        json
      case Left(ex) =>
        throw ex
    }
  }
}
