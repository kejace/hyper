let upstream =
https://github.com/purescript/package-sets/releases/download/psc-0.14.4-20210905/packages.dhall sha256:140f3630801f2b02d5f3a405d4872e0af317e4ef187016a6b00f97d59d6275c6

let overrides =
  { node-http =
      upstream.node-http // { repo = "https://github.com/srghma/purescript-node-http.git", version = "master" }
, js-uri =
        { dependencies = [ "functions", "maybe" ]
        , repo = "https://github.com/srghma/purescript-js-uri.git"
        , version = "main"
        }
  }

let additions =
      { smolder =
        { dependencies = [ "prelude", "js-uri" ]
        , repo = "https://github.com/nsaunders/purescript-smolder.git"
        , version = "ps-0.14"
        }
      }
in  upstream // overrides // additions
