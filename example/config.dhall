\(id : Text) ->
\(role : Text) ->
  { network =
      { addresses = toMap
          { alpha = { host = "localhost", port = 8090 }
          , beta = { host = "localhost", port = 8091 }
          , gamma = { host = "localhost", port = 8092 }
          }
      , initOwner = "alpha"
      , initState = +100
      }
  , experiment =
      { rate = 10
      , duration = 20
      }
  , id
  , role
  }
