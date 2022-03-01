let ex = ./experiment.dhall

let replicas =
      { alpha = { host = "localhost", port = 8090 }
      , beta = { host = "localhost", port = 8091 }
      , gamma = { host = "localhost", port = 8092 }
      }

in  { network = toMap replicas, rate = 200.0, duration = 4, setup = ex.escrow }
