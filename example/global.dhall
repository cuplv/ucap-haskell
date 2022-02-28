let Ex =
      < Token : { initOwner : Text }
      | Escrow : { initOwner : Text, amount : Natural }
      >

let replicas =
      { alpha = { host = "localhost", port = 8090 }
      , beta = { host = "localhost", port = 8091 }
      , gamma = { host = "localhost", port = 8092 }
      }

in  { network = toMap replicas
    , rate = 50.0
    , duration = 10
    , setup = Ex.Escrow { initOwner = "alpha", amount = 20000 }
    }
