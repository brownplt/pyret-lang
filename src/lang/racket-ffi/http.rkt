#lang pyret

import url as U

provide *

# Data definitions for HTTP requests and responses
# Modelled after Racket's web-server/http, but we use "string" instead of
# "bytes" in many places, and lack promises for bindings


data Request:
  | request(
        http-method :: String,
        uri :: U.URL,
        headers :: List<Header>,
        bindings :: List<Binding>,
        post-data :: Option<String>,
        host-ip :: String,
        host-port :: Number,
        client-ip :: String
      )
where:
  r = request(
      "POST",
      U.url(
          some("https"),
          none,
          some("cs.brown.edu"),
          some(4000),
          false,
          [U.path-param(U.path-part-string("~joe"), [])],
          { },
          some("#pubs")
        ),
      [
        header("X-Awesome-Header", "Totally True")
      ],
      [
        form-binding("mail-subject", "Here's a file"),
        file-binding("attachment", "virus.sh", [], "/usr/bin/copy-to-internet")
      ],
      none,
      "127.0.0.1",
      3000,
      "168.10.21.32"
  )
  r satisfies is-request
end


data Binding:
  | form-binding(id :: String, value :: String)
  | file-binding(
        id :: String,
        filename :: String,
        headers :: List<Header>,
        content :: String
      )
where:
  b1 = form-binding("name", "Mr. Potato Head")
  b1 satisfies is-form-binding
  b2 = form-binding("name", "<script>alert('hacked')</script>")
  b2 satisfies is-form-binding

  b3 = file-binding("file", "ahoy.arr", [], "#lang pyret\n'Ahoy world!'")
  b3 satisfies is-file-binding
end

data Header:
  | header(field :: String, value :: String)
where:
  h1 = header("Cors-request-origin", "*")
  h1 satisfies is-header
end

