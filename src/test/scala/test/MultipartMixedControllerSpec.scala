package test

import bodyparser.MultipartMixed
import bodyparser.MultipartMixed.Boundary
import org.scalatestplus.play._
import play.api.GlobalSettings
import play.api.{ Logger }
import play.api.test.Helpers._
import play.api.test._
import play.api.mvc._
import play.api.mvc.BodyParsers._
import play.api.mvc.Results._
import scala.concurrent.ExecutionContext.Implicits._

import scala.concurrent.Future

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

  def body(userId: Int = 123, email: String = "foo@bar.com"): Array[Byte] = s"""{"userId": $userId, "email":"$email"}""".getBytes("utf-8")

  private val batch = Action.async(MultipartMixed.multipartMixed) { request ⇒ MultipartMixed.response(request) }

  private val wildcardAction = Action.async { request ⇒
    Future.successful({
      //Logger.warn(s"done: anything='$anything' requestBody='${request.body}'")
      Ok(s"requestBody='${request.body}'")
    })
  }

  private val jsonAction = Action.async(parse.json) { request ⇒ Future { Ok(request.body) } }

  "The multipart/mixed controller" must {
    "receive and handle multipart/mixed requests" in {

      implicit val boundary = Boundary("735323031399963166993862150")
      val innerHeaders = Map("content-type" -> "application/json", "accept" -> "application/json")
      val rh = requestHeader(_id = 0, _method = "POST", _uri = "/batch")
      val requests = ((1 until 5) map { i ⇒ Request(requestHeader(i, _uri = s"/json/$i", _headers = innerHeaders), body(i)) }) :+
        Request(requestHeader(6, _uri = "/404", _method = "GET", _headers = innerHeaders), Array.emptyByteArray)

      val multipartMixedRequest = MultipartMixedRequest(rh, requests: _*)

      Logger.warn("\n\n\n'" + new String(multipartMixedRequest.body, "utf-8") + "'\n\n\n")

      val request = FakeRequest(multipartMixedRequest.method, multipartMixedRequest.uri)
        .withHeaders(multipartMixedRequest.requestHeader.headers.toSimpleMap.toArray: _*)
        .withRawBody(multipartMixedRequest.body)

      val result = call(batch, request)
      val bodyStr = contentAsString(result)

      Logger.warn(s"response body='$bodyStr'")

      status(result) mustBe OK
    }

  }
}

case class MultipartMixedRequest(requestHeader: RequestHeader, _body: Array[Byte]) extends Request[Array[Byte]] {
  override def body: Array[Byte] = _body
  override def secure: Boolean = requestHeader.secure
  override def uri: String = requestHeader.uri
  override def queryString: Map[String, Seq[String]] = requestHeader.queryString
  override def remoteAddress: String = requestHeader.remoteAddress
  override def method: String = requestHeader.method
  override def headers: Headers = requestHeader.headers
  override def path: String = requestHeader.path
  override def version: String = requestHeader.version
  override def tags: Map[String, String] = requestHeader.tags
  override def id: Long = requestHeader.id
}

object MultipartMixedRequest {

  private val DASHDASHSTR = "--"
  private val DASHDASH = "--".getBytes("utf-8")
  private val CRLFSTR = "\r\n"
  private val CRLF = CRLFSTR.getBytes("utf-8")
  private val CRLFCRLF = CRLF ++ CRLF

  def apply(requestHeader: RequestHeader, request: Request[Array[Byte]]*)(implicit boundary: Boundary): MultipartMixedRequest = {
    val requestBytes = mergeRequests(request: _*)(boundary.toBytes)
    val requestSize = requestBytes.length
    val requestHeaders = Array(
      "content-type" -> s"multipart/mixed; boundary=${boundary.value}",
      "content-length" -> requestSize.toString)
    val rh = requestHeader.copy(headers = requestHeader.headers.add(requestHeaders: _*))

    MultipartMixedRequest(rh, requestBytes)
  }

  private def chunkHeaders(contentId: Int = 1): Array[Byte] =
    ("Content-Type: application/http" + CRLFSTR +
      "Content-Transfer-Encoding: binary" + CRLFSTR +
      s"Content-ID: <b29c5de2-0db4-490b-b421-6a51b598bd22+$contentId>").getBytes("utf-8")

  private def requestAsBytes(request: Request[Array[Byte]]): Array[Byte] = {
    val contentLength = "content-length" -> request.body.length
    val headers = request.headers.toSimpleMap
    val headersAsStr = headers + contentLength map { case (k, v) ⇒ s"$k: $v" }
    val method = request.method
    val path = request.uri
    val version = request.version

    s"$method $path $version".getBytes("utf-8") ++ CRLF ++
      headersAsStr.mkString(CRLFSTR).getBytes("utf-8") ++
      CRLFCRLF ++
      request.body
  }

  private def mergeRequests(requests: Request[Array[Byte]]*)(boundary: Array[Byte]): Array[Byte] = {
    requests
      // transform to bytes
      .map(requestAsBytes)
      .zip(1 until 5)
      // append chunk specific headers
      .map { x ⇒ chunkHeaders(x._2) ++ CRLFCRLF ++ x._1 }
      // join bytes, start with boundary, then CRLF, then request (missing final boundary)
      .fold(Array.empty[Byte]) { case (acc, req) ⇒ acc ++ DASHDASH ++ boundary ++ CRLF ++ req ++ CRLF } ++
      DASHDASH ++ boundary ++ DASHDASH
  }
}
