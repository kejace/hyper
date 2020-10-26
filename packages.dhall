let upstream = /home/srghma/projects/upstream.dhall

let overrides =
  { spec-should-equal-or-satisfy =
    { dependencies =
      [ "console"
      , "effect"
      , "psci-support"
      , "spec"
      , "contravariant"
      , "foldable-traversable"
      ]
    , repo = "https://github.com/srghma/purescript-spec-should-equal-or-satisfy.git"
    , version = "master"
    }
  }

in  ( upstream // overrides
    )
    with spec.repo = "https://github.com/instateam/purescript-spec.git"
    with spec.version = "master"
    with strings.repo = "https://github.com/instateam/purescript-strings.git"
    with strings.version = "unix-parenthesis"
    with tuples.repo = "https://github.com/srghma/purescript-tuples.git"
    with tuples.version = "master"
    with prelude.version = "master"
