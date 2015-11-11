package bodyparser

import play.api.Play
import play.api.http.{ LazyHttpErrorHandler, Status, HttpConfiguration, ParserConfiguration }
import play.api.http.Status._
import play.api.libs.iteratee.Parsing.MatchInfo
import play.api.libs.iteratee._
import play.api.mvc._
import play.api.http.HttpProtocol.HTTP_1_1

import play.api.{ Logger, Application }
import play.core.parsers.FormUrlEncodedParser
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits._
import scala.util.control.NonFatal

object MultipartMixed {

  private val hcCache = Application.instanceCache[HttpConfiguration]
  private def config = play.api.Play.maybeApplication.map(app ⇒ hcCache(app).parser)
    .getOrElse(ParserConfiguration())

  private def DefaultMaxDiskLength: Long = config.maxDiskBuffer
  private def DefaultMaxTextLength: Int = config.maxMemoryBuffer
  private val MAX_HEADER_BUF_SIZE = 4 * 1024

  private val DASHDASHSTR = "--"
  private val DASHDASH = "--".getBytes("utf-8")
  private val CRLFSTR = "\r\n"
  private val CRLF = CRLFSTR.getBytes("utf-8")
  private val CRLFCRLF = CRLF ++ CRLF

  implicit val BOUNDARY = Boundary("735323031399963166993862150")

  private def takeUpToBoundary = Enumeratee.takeWhile[ParserInput](!_.isMatch)
  private def maxHeaderBuffer = Traversable.takeUpTo[Array[Byte]](MAX_HEADER_BUF_SIZE) transform Iteratee.consume[Array[Byte]]()

  type PartHandler[A] = PartialFunction[Map[String, String], Iteratee[Array[Byte], A]]
  private type ParserInput = MatchInfo[Array[Byte]]
  private type Parser[T] = Iteratee[ParserInput, T]

  case class Boundary(value: String) {
    private val _bytes = value.getBytes("utf-8")
    val toBytes = _bytes
    val withLeadingDashes = DASHDASH ++ _bytes
    val withLeadingTrailingDashes = withLeadingDashes ++ DASHDASH
  }

  sealed trait Part
  case class MultipartMixed(innerRequests: Seq[InnerRequestPart], badParts: Seq[BadPart])

  // TODO: add content-id header support
  // TODO: add content-type: application/http
  case class InnerRequestPart(rh: RequestHeader, body: Array[Byte], contentId: Option[String] = None) extends Part

  case class BadPart(headers: Map[String, String]) extends Part
  case class InnerRequestInfo(headers: Map[String, String], contentType: Option[String])

  object InnerRequestMatcher {

    def unapply(headers: Map[String, String]): Option[(Option[String], Map[String, String])] = {
      val contentType = headers.get("content-type")
      contentType.map { c ⇒ (contentType, headers) }
    }
  }

  case object BadRequestHeaders extends RequestHeader {
    override def id: Long = 0
    override def secure: Boolean = false
    override def uri: String = ""
    override def queryString: Map[String, Seq[String]] = Map.empty[String, Seq[String]]
    override def remoteAddress: String = ""
    override def method: String = ""
    override def headers: Headers = Headers.apply(("", ""))
    override def path: String = ""
    override def version: String = ""
    override def tags: Map[String, String] = Map.empty[String, String]
  }

  class BadInnerRequest extends InnerRequestPart(BadRequestHeaders, Array.empty[Byte])
  object BadInnerRequest {
    def apply() = new BadInnerRequest
  }

  def multipartMixed: BodyParser[MultipartMixed] =
    multipartMixed(handleInnerRequestPart)

  def multipartMixed(
    innerRequestPartHandler: RequestHeader ⇒ PartHandler[InnerRequestPart],
    maxLength: Long = DefaultMaxDiskLength): BodyParser[MultipartMixed] = {

    BodyParser("multipartMixed") { request ⇒

      val parser = Traversable.takeUpTo[Array[Byte]](maxLength).transform(
        multipartParser(DefaultMaxTextLength, innerRequestPartHandler)(request)).flatMap {
          case d @ Left(r) ⇒ Iteratee.eofOrElse(r)(d)
          case d           ⇒ checkForEof(request)(d)
        }

      parser.map {
        case Left(tooLarge)         ⇒ Left(tooLarge)
        case Right(Left(badResult)) ⇒ Left(badResult)
        case Right(Right(body))     ⇒ Right(body)
      }
    }
  }

  def multipartParser(maxDataLength: Int, innerRequestHandler: RequestHeader ⇒ PartHandler[InnerRequestPart]): BodyParser[MultipartMixed] = BodyParser("multipartMixed") { request ⇒

    val maybeBoundary = for {
      mt ← request.mediaType
      (_, value) ← mt.parameters.find(_._1.equalsIgnoreCase("boundary"))
      boundary ← value
    } yield ("\r\n--" + boundary).getBytes("utf-8")

    maybeBoundary.map { boundary ⇒
      for {
        // First, we ignore the first boundary.  Note that if the body contains a preamble, this won't work.  But the
        // body never contains a preamble.
        _ ← Traversable.take[Array[Byte]](boundary.size - 2) transform Iteratee.ignore
        // We use the search Enumeratee to turn the stream into a stream of data chunks that are either the boundary,
        // or not the boundary, and we parse that
        result ← Parsing.search(boundary) transform parseParts(request, innerRequestHandler)
      } yield {
        result.right.map { reversed ⇒
          // We built the parts by prepending a list, so we need to reverse them
          val parts = reversed.reverse
          val innerRequests = parts.collect { case innerRequest: InnerRequestPart ⇒ innerRequest }
          val bad = parts.collect { case bad: BadPart ⇒ bad }
          MultipartMixed(innerRequests, bad)
        }
      }
    }.getOrElse {
      Iteratee.flatten(createBadResult("Missing boundary header")(request).
        map(r ⇒ Done(Left(r))))
    }
  }

  private def parseParts(
    originalRequest: RequestHeader,
    innerRequestHandler: RequestHeader ⇒ PartHandler[Part],
    parts: List[Part] = Nil): Parser[Either[Result, List[Part]]] = {

    parsePart(originalRequest, innerRequestHandler).flatMap {
      case None ⇒ Done(Right(parts)) // None, we've reached the end of the body
      case Some(other: Part) ⇒ // All other parts
        for {
          _ ← Iteratee.head // Drop the boundary
          result ← parseParts(originalRequest, innerRequestHandler, other :: parts)
        } yield result
    }
  }

  private def parsePart(
    originalRequest: RequestHeader,
    innerRequestHandler: RequestHeader ⇒ PartHandler[Part]): Parser[Option[Part]] = {

    val collectHeaders: Iteratee[Array[Byte], Option[(Map[String, String], Array[Byte])]] = maxHeaderBuffer.map { buffer ⇒

      val (headerString, rest) = splitHeaders(Option(buffer))

      if (headerString.startsWith("--") || headerString.isEmpty) {
        None // It's the last part
      } else {

        val headers = parseHeaders(headerString)
        val left = rest.drop(CRLFCRLF.length)
        Some((headers, left))
      }
    }

    val readPart: PartHandler[Part] = innerRequestHandler(originalRequest)
      .orElse({ case headers ⇒ Done(BadPart(headers), Input.Empty) })

    // Take up to the boundary, remove the MatchInfo wrapping, collect headers, and then read the part
    takeUpToBoundary compose Enumeratee.map[MatchInfo[Array[Byte]]](_.content) transform collectHeaders.flatMap {
      case Some((headers, left)) ⇒ Iteratee.flatten(readPart(headers).feed(Input.El(left))).map(Some.apply)
      case _                     ⇒ Done(None)
    }

  }

  def handleInnerRequestPart(originalRequest: RequestHeader): PartHandler[InnerRequestPart] = {

    case InnerRequestMatcher(contentType, headers) ⇒

      val collectHeadersAndBody: Iteratee[Array[Byte], Option[(RequestHeader, Array[Byte])]] = maxHeaderBuffer.map { buffer ⇒

        val (methodPathVersionBytes, restForHeaders) = Option(buffer).map(b ⇒ b.splitAt(b.indexOfSlice(CRLF))).get
        val methodPathVersion = new String(methodPathVersionBytes, "utf-8").trim.split(" ")
        val (headerString, rest) = splitHeaders(Some(restForHeaders))

        if (methodPathVersion.length == 3) {
          val Array(_method, _path, _version) = methodPathVersion

          val r = if (headerString.startsWith("--") || headerString.isEmpty) {
            None // It's the last part
          } else {

            val _headers = parseHeaders(headerString)
            val left = rest.drop(CRLFCRLF.length)
            val qs = parseQueryString(_path)
            // TODO: create request id properly
            val rh = createRequestHeader(
              originalRequest.secure, _path,
              originalRequest.remoteAddress, qs,
              _method, Headers(_headers.toArray: _*),
              _path, _version, Map.empty[String, String], 123L)
            Some((rh, left))
          }

          r
        } else {
          None
        }
      }

      collectHeadersAndBody.flatMap[InnerRequestPart] {
        case Some((rh, left)) ⇒
          Logger.info(s"$rh -> body=${new String(left)}")
          Done(InnerRequestPart(rh, left, contentId = headers.get("content-id")))

        case _ ⇒ Done(BadInnerRequest())
      }
  }

  private def createBadResult(msg: String, statusCode: Int = BAD_REQUEST): RequestHeader ⇒ Future[Result] = { request ⇒
    LazyHttpErrorHandler.onClientError(request, statusCode, msg)
  }

  private def checkForEof[A](request: RequestHeader): A ⇒ Iteratee[Array[Byte], Either[Result, A]] = { eofValue: A ⇒
      def cont: Iteratee[Array[Byte], Either[Result, A]] = Cont {
        case in @ Input.El(e) ⇒
          val badResult: Future[Result] = createBadResult("Request Entity Too Large", Status.REQUEST_ENTITY_TOO_LARGE)(request)
          Iteratee.flatten(badResult.map(r ⇒ Done(Left(r), in)))
        case in @ Input.EOF ⇒
          Done(Right(eofValue), in)
        case Input.Empty ⇒
          cont
      }
    cont
  }

  private def parseQueryString(_path: String): Map[String, Seq[String]] = {
    val questionMarkPos = _path.indexOf('?')

    if (questionMarkPos > 0) {
      FormUrlEncodedParser.parse(Option(_path.splitAt(questionMarkPos)._2).getOrElse(""))
    } else {
      Map.empty[String, Seq[String]]
    }
  }

  private def parseHeaders(headerString: String): Map[String, String] = headerString.lines.map { header ⇒
    val key :: value = header.trim.split(":").toList
    (key.trim.toLowerCase, value.mkString(":").trim)
  }.toMap

  private def splitHeaders(buffer: Option[Array[Byte]]): (String, Array[Byte]) = {
    val (headerBytes, rest) = buffer.map(b ⇒ b.splitAt(b.indexOfSlice(CRLFCRLF))).get
    (new String(headerBytes, "utf-8").trim, rest)
  }

  private def createRequestHeader(
    _secure: Boolean,
    _uri: String,
    _remoteAddress: String,
    _queryString: Map[String, Seq[String]],
    _method: String,
    _headers: Headers,
    _path: String,
    _version: String,
    _tags: Map[String, String],
    _id: Long): RequestHeader = new RequestHeader {
    override def secure: Boolean = _secure
    override def uri: String = _path
    override def remoteAddress: String = _remoteAddress
    override def queryString: Map[String, Seq[String]] = _queryString
    override def method: String = _method
    override def headers: Headers = _headers
    override def path: String = _path
    override def version: String = _version
    override def tags: Map[String, String] = Map.empty[String, String]
    // TODO: create RequestHeader id properly
    override def id: Long = 123L
  }

  def response(request: Request[MultipartMixed]) = {
    Future {
      val resultFutures = request.body.innerRequests
        .filterNot(_.isInstanceOf[BadInnerRequest])
        .map { req ⇒ (Request.apply(req.rh, req.body), req.contentId) }
        .map {
          case (r, contentId) ⇒

            val handler = Play.current.global.onRouteRequest(r)
            handler.map {
              case action: EssentialAction ⇒ proxyAction(action)(r, contentId)
              case x                       ⇒ Future.failed[(Result, Option[String])](new IllegalArgumentException(s"Unexpected handler type"))
            } getOrElse {
              Future.failed[(Result, Option[String])](new IllegalArgumentException(s"No handler for request '$r'"))
            }
        }

      val resultsFuture = Future.sequence(resultFutures)
      val empty = Enumerator(Array.empty[Byte])

      val bodyEnumerator = resultsFuture.map { resultsList ⇒
        // wrap results with boundary, append "--" at the end of the last one
        Enumerator(BOUNDARY.withLeadingDashes) andThen
          resultsList.map {
            case (result, contentId) ⇒
              Enumerator(CRLF) andThen
                Enumerator(initialHeaders(contentId)) andThen
                Enumerator(CRLFCRLF) andThen
                Enumerator(responseLineAndHeaders(result)) andThen
                Enumerator(CRLFCRLF) andThen
                result.body andThen
                Enumerator(CRLF) andThen
                Enumerator(BOUNDARY.withLeadingDashes)
          }.fold(empty)(_ andThen _) andThen
          Enumerator(DASHDASH)
      }

      Result(ResponseHeader(play.api.http.Status.OK), Enumerator.flatten(bodyEnumerator))
    }
  }

  private def proxyAction(action: ⇒ EssentialAction)(implicit request: Request[Array[Byte]], contentId: Option[String]): Future[(Result, Option[String])] = {
    // Surround action invocation with try and recover with a failed future in case there is an exception in the action
    try {
      val actionIteratee = action(request)
      val data = request.body
      //Logger.warn(s"making request: method: ${request.method} uri:${request.uri} headers:${request.headers} body:${new String(data)}")
      Iteratee.flatten(actionIteratee.feed(Input.El(data))).run.map { r ⇒ /*Logger.info("got result: " + r);*/ (r, contentId) }
    } catch {
      case NonFatal(e) ⇒ Future.failed(e)
    }
  }

  private def responseLineAndHeaders(result: Result): Array[Byte] = {
    val responseLine = HTTP_1_1 + " " + result.header.status + " " + result.header.reasonPhrase.getOrElse("")
    val headersAsStr = result.header.headers map { case (k, v) ⇒ s"$k: $v" }
    responseLine ++ CRLFSTR ++ headersAsStr.mkString(CRLFSTR) getBytes "utf-8"
  }

  private def initialHeaders(contentId: Option[String]): Array[Byte] =
    contentId.map("Content-ID" -> _).map { case (k, v) ⇒ s"$k: $v".getBytes("utf-8") }.getOrElse(Array.emptyByteArray)
}

