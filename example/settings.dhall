let peers =
      { alpha = { host = "localhost", port = 8090 }
      , beta = { host = "localhost", port = 8091 }
      , gamma = { host = "localhost", port = 8092 }
      }

let network = { addresses = toMap peers, initOwner = "alpha", initState = +100 }

let experiment = { rate = 5, duration = 10 }

let debug = 0

in  \(id : Text) -> \(role : Text) -> { network, experiment, id, role, debug }
