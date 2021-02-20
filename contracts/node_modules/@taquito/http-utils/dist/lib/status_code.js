"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.STATUS_CODE = void 0;
/**
 * Hypertext Transfer Protocol (HTTP) response status codes.
 * @see {@link https://en.wikipedia.org/wiki/List_of_HTTP_status_codes}
 */
var STATUS_CODE;
(function (STATUS_CODE) {
    /**
     * The server has received the request headers and the client should proceed to send the request body
     * (in the case of a request for which a body needs to be sent; for example, a POST request).
     * Sending a large request body to a server after a request has been rejected for inappropriate headers would be inefficient.
     * To have a server check the request's headers, a client must send Expect: 100-continue as a header in its initial request
     * and receive a 100 Continue status code in response before sending the body. The response 417 Expectation Failed indicates the request should not be continued.
     */
    STATUS_CODE[STATUS_CODE["CONTINUE"] = 100] = "CONTINUE";
    /**
     * The requester has asked the server to switch protocols and the server has agreed to do so.
     */
    STATUS_CODE[STATUS_CODE["SWITCHING_PROTOCOLS"] = 101] = "SWITCHING_PROTOCOLS";
    /**
     * A WebDAV request may contain many sub-requests involving file operations, requiring a long time to complete the request.
     * This code indicates that the server has received and is processing the request, but no response is available yet.
     * This prevents the client from timing out and assuming the request was lost.
     */
    STATUS_CODE[STATUS_CODE["PROCESSING"] = 102] = "PROCESSING";
    /**
     * Standard response for successful HTTP requests.
     * The actual response will depend on the request method used.
     * In a GET request, the response will contain an entity corresponding to the requested resource.
     * In a POST request, the response will contain an entity describing or containing the result of the action.
     */
    STATUS_CODE[STATUS_CODE["OK"] = 200] = "OK";
    /**
     * The request has been fulfilled, resulting in the creation of a new resource.
     */
    STATUS_CODE[STATUS_CODE["CREATED"] = 201] = "CREATED";
    /**
     * The request has been accepted for processing, but the processing has not been completed.
     * The request might or might not be eventually acted upon, and may be disallowed when processing occurs.
     */
    STATUS_CODE[STATUS_CODE["ACCEPTED"] = 202] = "ACCEPTED";
    /**
     * SINCE HTTP/1.1
     * The server is a transforming proxy that received a 200 OK from its origin,
     * but is returning a modified version of the origin's response.
     */
    STATUS_CODE[STATUS_CODE["NON_AUTHORITATIVE_INFORMATION"] = 203] = "NON_AUTHORITATIVE_INFORMATION";
    /**
     * The server successfully processed the request and is not returning any content.
     */
    STATUS_CODE[STATUS_CODE["NO_CONTENT"] = 204] = "NO_CONTENT";
    /**
     * The server successfully processed the request, but is not returning any content.
     * Unlike a 204 response, this response requires that the requester reset the document view.
     */
    STATUS_CODE[STATUS_CODE["RESET_CONTENT"] = 205] = "RESET_CONTENT";
    /**
     * The server is delivering only part of the resource (byte serving) due to a range header sent by the client.
     * The range header is used by HTTP clients to enable resuming of interrupted downloads,
     * or split a download into multiple simultaneous streams.
     */
    STATUS_CODE[STATUS_CODE["PARTIAL_CONTENT"] = 206] = "PARTIAL_CONTENT";
    /**
     * The message body that follows is an XML message and can contain a number of separate response codes,
     * depending on how many sub-requests were made.
     */
    STATUS_CODE[STATUS_CODE["MULTI_STATUS"] = 207] = "MULTI_STATUS";
    /**
     * The members of a DAV binding have already been enumerated in a preceding part of the (multistatus) response,
     * and are not being included again.
     */
    STATUS_CODE[STATUS_CODE["ALREADY_REPORTED"] = 208] = "ALREADY_REPORTED";
    /**
     * The server has fulfilled a request for the resource,
     * and the response is a representation of the result of one or more instance-manipulations applied to the current instance.
     */
    STATUS_CODE[STATUS_CODE["IM_USED"] = 226] = "IM_USED";
    /**
     * Indicates multiple options for the resource from which the client may choose (via agent-driven content negotiation).
     * For example, this code could be used to present multiple video format options,
     * to list files with different filename extensions, or to suggest word-sense disambiguation.
     */
    STATUS_CODE[STATUS_CODE["MULTIPLE_CHOICES"] = 300] = "MULTIPLE_CHOICES";
    /**
     * This and all future requests should be directed to the given URI.
     */
    STATUS_CODE[STATUS_CODE["MOVED_PERMANENTLY"] = 301] = "MOVED_PERMANENTLY";
    /**
     * This is an example of industry practice contradicting the standard.
     * The HTTP/1.0 specification (RFC 1945) required the client to perform a temporary redirect
     * (the original describing phrase was "Moved Temporarily"), but popular browsers implemented 302
     * with the functionality of a 303 See Other. Therefore, HTTP/1.1 added status codes 303 and 307
     * to distinguish between the two behaviours. However, some Web applications and frameworks
     * use the 302 status code as if it were the 303.
     */
    STATUS_CODE[STATUS_CODE["FOUND"] = 302] = "FOUND";
    /**
     * SINCE HTTP/1.1
     * The response to the request can be found under another URI using a GET method.
     * When received in response to a POST (or PUT/DELETE), the client should presume that
     * the server has received the data and should issue a redirect with a separate GET message.
     */
    STATUS_CODE[STATUS_CODE["SEE_OTHER"] = 303] = "SEE_OTHER";
    /**
     * Indicates that the resource has not been modified since the version specified by the request headers If-Modified-Since or If-None-Match.
     * In such case, there is no need to retransmit the resource since the client still has a previously-downloaded copy.
     */
    STATUS_CODE[STATUS_CODE["NOT_MODIFIED"] = 304] = "NOT_MODIFIED";
    /**
     * SINCE HTTP/1.1
     * The requested resource is available only through a proxy, the address for which is provided in the response.
     * Many HTTP clients (such as Mozilla and Internet Explorer) do not correctly handle responses with this status code, primarily for security reasons.
     */
    STATUS_CODE[STATUS_CODE["USE_PROXY"] = 305] = "USE_PROXY";
    /**
     * No longer used. Originally meant "Subsequent requests should use the specified proxy."
     */
    STATUS_CODE[STATUS_CODE["SWITCH_PROXY"] = 306] = "SWITCH_PROXY";
    /**
     * SINCE HTTP/1.1
     * In this case, the request should be repeated with another URI; however, future requests should still use the original URI.
     * In contrast to how 302 was historically implemented, the request method is not allowed to be changed when reissuing the original request.
     * For example, a POST request should be repeated using another POST request.
     */
    STATUS_CODE[STATUS_CODE["TEMPORARY_REDIRECT"] = 307] = "TEMPORARY_REDIRECT";
    /**
     * The request and all future requests should be repeated using another URI.
     * 307 and 308 parallel the behaviors of 302 and 301, but do not allow the HTTP method to change.
     * So, for example, submitting a form to a permanently redirected resource may continue smoothly.
     */
    STATUS_CODE[STATUS_CODE["PERMANENT_REDIRECT"] = 308] = "PERMANENT_REDIRECT";
    /**
     * The server cannot or will not process the request due to an apparent client error
     * (e.g., malformed request syntax, too large size, invalid request message framing, or deceptive request routing).
     */
    STATUS_CODE[STATUS_CODE["BAD_REQUEST"] = 400] = "BAD_REQUEST";
    /**
     * Similar to 403 Forbidden, but specifically for use when authentication is required and has failed or has not yet
     * been provided. The response must include a WWW-Authenticate header field containing a challenge applicable to the
     * requested resource. See Basic access authentication and Digest access authentication. 401 semantically means
     * "unauthenticated",i.e. the user does not have the necessary credentials.
     */
    STATUS_CODE[STATUS_CODE["UNAUTHORIZED"] = 401] = "UNAUTHORIZED";
    /**
     * Reserved for future use. The original intention was that this code might be used as part of some form of digital
     * cash or micro payment scheme, but that has not happened, and this code is not usually used.
     * Google Developers API uses this status if a particular developer has exceeded the daily limit on requests.
     */
    STATUS_CODE[STATUS_CODE["PAYMENT_REQUIRED"] = 402] = "PAYMENT_REQUIRED";
    /**
     * The request was valid, but the server is refusing action.
     * The user might not have the necessary permissions for a resource.
     */
    STATUS_CODE[STATUS_CODE["FORBIDDEN"] = 403] = "FORBIDDEN";
    /**
     * The requested resource could not be found but may be available in the future.
     * Subsequent requests by the client are permissible.
     */
    STATUS_CODE[STATUS_CODE["NOT_FOUND"] = 404] = "NOT_FOUND";
    /**
     * A request method is not supported for the requested resource;
     * for example, a GET request on a form that requires data to be presented via POST, or a PUT request on a read-only resource.
     */
    STATUS_CODE[STATUS_CODE["METHOD_NOT_ALLOWED"] = 405] = "METHOD_NOT_ALLOWED";
    /**
     * The requested resource is capable of generating only content not acceptable according to the Accept headers sent in the request.
     */
    STATUS_CODE[STATUS_CODE["NOT_ACCEPTABLE"] = 406] = "NOT_ACCEPTABLE";
    /**
     * The client must first authenticate itself with the proxy.
     */
    STATUS_CODE[STATUS_CODE["PROXY_AUTHENTICATION_REQUIRED"] = 407] = "PROXY_AUTHENTICATION_REQUIRED";
    /**
     * The server timed out waiting for the request.
     * According to HTTP specifications:
     * "The client did not produce a request within the time that the server was prepared to wait. The client MAY repeat the request without modifications at any later time."
     */
    STATUS_CODE[STATUS_CODE["REQUEST_TIMEOUT"] = 408] = "REQUEST_TIMEOUT";
    /**
     * Indicates that the request could not be processed because of conflict in the request,
     * such as an edit conflict between multiple simultaneous updates.
     */
    STATUS_CODE[STATUS_CODE["CONFLICT"] = 409] = "CONFLICT";
    /**
     * Indicates that the resource requested is no longer available and will not be available again.
     * This should be used when a resource has been intentionally removed and the resource should be purged.
     * Upon receiving a 410 status code, the client should not request the resource in the future.
     * Clients such as search engines should remove the resource from their indices.
     * Most use cases do not require clients and search engines to purge the resource, and a "404 Not Found" may be used instead.
     */
    STATUS_CODE[STATUS_CODE["GONE"] = 410] = "GONE";
    /**
     * The request did not specify the length of its content, which is required by the requested resource.
     */
    STATUS_CODE[STATUS_CODE["LENGTH_REQUIRED"] = 411] = "LENGTH_REQUIRED";
    /**
     * The server does not meet one of the preconditions that the requester put on the request.
     */
    STATUS_CODE[STATUS_CODE["PRECONDITION_FAILED"] = 412] = "PRECONDITION_FAILED";
    /**
     * The request is larger than the server is willing or able to process. Previously called "Request Entity Too Large".
     */
    STATUS_CODE[STATUS_CODE["PAYLOAD_TOO_LARGE"] = 413] = "PAYLOAD_TOO_LARGE";
    /**
     * The URI provided was too long for the server to process. Often the result of too much data being encoded as a query-string of a GET request,
     * in which case it should be converted to a POST request.
     * Called "Request-URI Too Long" previously.
     */
    STATUS_CODE[STATUS_CODE["URI_TOO_LONG"] = 414] = "URI_TOO_LONG";
    /**
     * The request entity has a media type which the server or resource does not support.
     * For example, the client uploads an image as image/svg+xml, but the server requires that images use a different format.
     */
    STATUS_CODE[STATUS_CODE["UNSUPPORTED_MEDIA_TYPE"] = 415] = "UNSUPPORTED_MEDIA_TYPE";
    /**
     * The client has asked for a portion of the file (byte serving), but the server cannot supply that portion.
     * For example, if the client asked for a part of the file that lies beyond the end of the file.
     * Called "Requested Range Not Satisfiable" previously.
     */
    STATUS_CODE[STATUS_CODE["RANGE_NOT_SATISFIABLE"] = 416] = "RANGE_NOT_SATISFIABLE";
    /**
     * The server cannot meet the requirements of the Expect request-header field.
     */
    STATUS_CODE[STATUS_CODE["EXPECTATION_FAILED"] = 417] = "EXPECTATION_FAILED";
    /**
     * This code was defined in 1998 as one of the traditional IETF April Fools' jokes, in RFC 2324, Hyper Text Coffee Pot Control Protocol,
     * and is not expected to be implemented by actual HTTP servers. The RFC specifies this code should be returned by
     * teapots requested to brew coffee. This HTTP status is used as an Easter egg in some websites, including Google.com.
     */
    STATUS_CODE[STATUS_CODE["I_AM_A_TEAPOT"] = 418] = "I_AM_A_TEAPOT";
    /**
     * The request was directed at a server that is not able to produce a response (for example because a connection reuse).
     */
    STATUS_CODE[STATUS_CODE["MISDIRECTED_REQUEST"] = 421] = "MISDIRECTED_REQUEST";
    /**
     * The request was well-formed but was unable to be followed due to semantic errors.
     */
    STATUS_CODE[STATUS_CODE["UNPROCESSABLE_ENTITY"] = 422] = "UNPROCESSABLE_ENTITY";
    /**
     * The resource that is being accessed is locked.
     */
    STATUS_CODE[STATUS_CODE["LOCKED"] = 423] = "LOCKED";
    /**
     * The request failed due to failure of a previous request (e.g., a PROPPATCH).
     */
    STATUS_CODE[STATUS_CODE["FAILED_DEPENDENCY"] = 424] = "FAILED_DEPENDENCY";
    /**
     * The client should switch to a different protocol such as TLS/1.0, given in the Upgrade header field.
     */
    STATUS_CODE[STATUS_CODE["UPGRADE_REQUIRED"] = 426] = "UPGRADE_REQUIRED";
    /**
     * The origin server requires the request to be conditional.
     * Intended to prevent "the 'lost update' problem, where a client
     * GETs a resource's state, modifies it, and PUTs it back to the server,
     * when meanwhile a third party has modified the state on the server, leading to a conflict."
     */
    STATUS_CODE[STATUS_CODE["PRECONDITION_REQUIRED"] = 428] = "PRECONDITION_REQUIRED";
    /**
     * The user has sent too many requests in a given amount of time. Intended for use with rate-limiting schemes.
     */
    STATUS_CODE[STATUS_CODE["TOO_MANY_REQUESTS"] = 429] = "TOO_MANY_REQUESTS";
    /**
     * The server is unwilling to process the request because either an individual header field,
     * or all the header fields collectively, are too large.
     */
    STATUS_CODE[STATUS_CODE["REQUEST_HEADER_FIELDS_TOO_LARGE"] = 431] = "REQUEST_HEADER_FIELDS_TOO_LARGE";
    /**
     * A server operator has received a legal demand to deny access to a resource or to a set of resources
     * that includes the requested resource. The code 451 was chosen as a reference to the novel Fahrenheit 451.
     */
    STATUS_CODE[STATUS_CODE["UNAVAILABLE_FOR_LEGAL_REASONS"] = 451] = "UNAVAILABLE_FOR_LEGAL_REASONS";
    /**
     * A generic error message, given when an unexpected condition was encountered and no more specific message is suitable.
     */
    STATUS_CODE[STATUS_CODE["INTERNAL_SERVER_ERROR"] = 500] = "INTERNAL_SERVER_ERROR";
    /**
     * The server either does not recognize the request method, or it lacks the ability to fulfill the request.
     * Usually this implies future availability (e.g., a new feature of a web-service API).
     */
    STATUS_CODE[STATUS_CODE["NOT_IMPLEMENTED"] = 501] = "NOT_IMPLEMENTED";
    /**
     * The server was acting as a gateway or proxy and received an invalid response from the upstream server.
     */
    STATUS_CODE[STATUS_CODE["BAD_GATEWAY"] = 502] = "BAD_GATEWAY";
    /**
     * The server is currently unavailable (because it is overloaded or down for maintenance).
     * Generally, this is a temporary state.
     */
    STATUS_CODE[STATUS_CODE["SERVICE_UNAVAILABLE"] = 503] = "SERVICE_UNAVAILABLE";
    /**
     * The server was acting as a gateway or proxy and did not receive a timely response from the upstream server.
     */
    STATUS_CODE[STATUS_CODE["GATEWAY_TIMEOUT"] = 504] = "GATEWAY_TIMEOUT";
    /**
     * The server does not support the HTTP protocol version used in the request
     */
    STATUS_CODE[STATUS_CODE["HTTP_VERSION_NOT_SUPPORTED"] = 505] = "HTTP_VERSION_NOT_SUPPORTED";
    /**
     * Transparent content negotiation for the request results in a circular reference.
     */
    STATUS_CODE[STATUS_CODE["VARIANT_ALSO_NEGOTIATES"] = 506] = "VARIANT_ALSO_NEGOTIATES";
    /**
     * The server is unable to store the representation needed to complete the request.
     */
    STATUS_CODE[STATUS_CODE["INSUFFICIENT_STORAGE"] = 507] = "INSUFFICIENT_STORAGE";
    /**
     * The server detected an infinite loop while processing the request.
     */
    STATUS_CODE[STATUS_CODE["LOOP_DETECTED"] = 508] = "LOOP_DETECTED";
    /**
     * Further extensions to the request are required for the server to fulfill it.
     */
    STATUS_CODE[STATUS_CODE["NOT_EXTENDED"] = 510] = "NOT_EXTENDED";
    /**
     * The client needs to authenticate to gain network access.
     * Intended for use by intercepting proxies used to control access to the network (e.g., "captive portals" used
     * to require agreement to Terms of Service before granting full Internet access via a Wi-Fi hotspot).
     */
    STATUS_CODE[STATUS_CODE["NETWORK_AUTHENTICATION_REQUIRED"] = 511] = "NETWORK_AUTHENTICATION_REQUIRED";
})(STATUS_CODE = exports.STATUS_CODE || (exports.STATUS_CODE = {}));
//# sourceMappingURL=status_code.js.map