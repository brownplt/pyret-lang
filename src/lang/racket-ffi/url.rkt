#lang pyret

provide *

data URL:
  | url(
        scheme :: Option<String>,
        user :: Option<String>,
        host :: Option<String>,
        port :: Option<Number>,
        is-path-absolute :: Bool,
        path :: List<PathParam>,
        query :: Object, # TODO: make a dictionary
        fragment :: Option<String>
      )
where:
  u1 = url(
      some("https"),
      none,
      some("cs.brown.edu"),
      some(4000),
      false,
      [path-param(path-part-string("~joe"), [])],
      { },
      some("#pubs")
    )
  u1 satisfies is-url
end

data PathParam:
  | path-param(path-part :: PathPart, param :: List<String>)
where:
  p1 = path-param(path-part-up, ["index.html"])
  p1 satisfies is-path-param
end

data PathPart:
  | path-part-string(part :: String)
  | path-part-up
  | path-part-same
where:
  path-part-string("~joe") satisfies is-path-part-string
end

