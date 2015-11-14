package test

import bodyparser.{ MultipartMixedRequest, MultipartMixed }
import bodyparser.MultipartMixed.Boundary

import org.scalatestplus.play._

import play.api.GlobalSettings
import play.api.libs.iteratee.Iteratee
import play.api.libs.json.Json
import play.api.Logger
import play.api.test.Helpers._
import play.api.test._
import play.api.mvc._
import play.api.mvc.BodyParsers._
import play.api.mvc.Results._

import scala.concurrent.ExecutionContext.Implicits._
import scala.concurrent.duration._
import scala.concurrent.{ Await, Future }

class MultipartMixedControllerSpec extends PlaySpec with TestSpec with OneAppPerSuite {

  implicit override lazy val app: FakeApplication =
    FakeApplication(
      withGlobal = Some(new GlobalSettings() {
        override def onRouteRequest(request: RequestHeader): Option[Handler] = {
          request.uri match {
            case uri if uri.startsWith("/wildcard") ⇒ Some(wildcardAction)
            case uri if uri.startsWith("/json")     ⇒ Some(jsonAction)
            case uri if uri.startsWith("/batch")    ⇒ Some(batch)
            case uri                                ⇒ super.onRouteRequest(request)
          }
        }
      }))

  private def requestHeader(
    _id: Long,
    _uri: String = s"/wildcard/",
    _method: String = "POST",
    _headers: Map[String, String] = Map.empty[String, String]) = new RequestHeader {
    override def secure: Boolean = false
    override def uri: String = _uri
    override def remoteAddress: String = "1.2.3.4"
    override def queryString: Map[String, Seq[String]] = Map.empty
    override def method: String = _method
    override def headers: Headers = Headers(_headers.toArray: _*)
    override def path: String = _uri
    override def version: String = HTTP_1_1
    override def tags: Map[String, String] = Map.empty
    override def id: Long = _id
  }

  def _body(userId: Int = 123, email: String = "foo@bar.com"): Array[Byte] = s"""{"userId": $userId, "email":"$email"}""".getBytes("utf-8")

  private val batch = Action.async(MultipartMixed.multipartMixed) { request ⇒ MultipartMixed.response(request) }

  private val wildcardAction = Action.async { request ⇒
    Future.successful({
      Ok(s"requestBody='${request.body}'")
    })
  }

  private val jsonAction = Action.async(parse.json) { request ⇒ Future { Ok(request.body) } }

  "The multipart/mixed controller" must {
    "receive and handle multipart/mixed requests" in {

      implicit val boundary = Boundary("735323031399963166993862150")
      val innerHeaders = Map("content-type" -> "application/json", "accept" -> "application/json")
      val rh = requestHeader(_id = 0, _method = "POST", _uri = "/batch")
      val requests = ((1 until 5) map { i ⇒ Request(requestHeader(i, _uri = s"/json/$i", _headers = innerHeaders), _body(i)) }) :+
        Request(requestHeader(6, _uri = "/404", _method = "GET", _headers = innerHeaders), Array.emptyByteArray)

      val multipartMixedRequest = MultipartMixedRequest(rh, requests: _*)

      Logger.debug("request body='" + new String(multipartMixedRequest.body, "utf-8") + "'")

      val request = FakeRequest(multipartMixedRequest.method, multipartMixedRequest.uri)
        .withHeaders(multipartMixedRequest.requestHeader.headers.toSimpleMap.toArray: _*)
        .withRawBody(multipartMixedRequest.body)

      val result = call(batch, request)
      val bodyStr = contentAsString(result)

      Logger.debug(s"response body='$bodyStr'")

      status(result) mustBe OK

      val futureResults = result.map { res ⇒
        res.body.run(MultipartMixed.parseResult(res)).map {
          case Right((results, badParts)) ⇒
            Some(results)
          case Left(_) ⇒
            None
        }
      }

      val futureBooleans = futureResults.flatMap(identity).map {
        case Some(results) ⇒
          results map { result ⇒
            val eBytes = result.header.headers.get(TRANSFER_ENCODING) match {
              case Some("chunked") ⇒ result.body &> Results.dechunk
              case _               ⇒ result.body
            }
            val bodyBytes = Await.result(eBytes |>>> Iteratee.consume[Array[Byte]](), 5.seconds)
            val body = new String(bodyBytes)
            val bodyJson = Json.parse(body)

            Logger.debug(s"result: headers=${result.header.headers} body=$bodyJson")

            (bodyJson \ "userId").toOption.isDefined &&
              (bodyJson \ "email").toOption.isDefined
          } forall (_.equals(true))

        case None ⇒ false
      }

      assert[Boolean](f = futureBooleans, assertion = _.equals(true))
    }
  }
}

