package bodyparser

import bodyparser.MultipartMixed.Boundary

import play.api.mvc._

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
