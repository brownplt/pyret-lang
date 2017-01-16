provide *
provide-types *

import global as _
import httplib as HTTP

type Request = {
  req-protocol :: String,
  req-hostname :: String,
  req-path :: String,
  req-method :: String
}

type Response = {
  rsp-code :: Number,
  rsp-headers :: {},
  rsp-body :: String
}

fun make-get-request(hostname :: String, path :: String) -> Request:
  {
    req-protocol: 'http:',
    req-hostname: hostname,
    req-path: path,
    req-method: 'GET'
  }
end

fun execute(req :: Request) -> Response:
  HTTP.execute(req)
end

fun get(hostname :: String, path :: String) -> Response:
  req = make-get-request(hostname, path)
  execute(req)
end
