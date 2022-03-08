let Prelude = ./Prelude.dhall

let mkDRange =
      \(start : Natural) ->
      \(step : Natural) ->
      \(number : Natural) ->
        Prelude.List.map
          Natural
          Double
          (\(n : Natural) -> Prelude.Natural.toDouble (start + n * step))
          (Prelude.Natural.enumerate number)

let Ex = < Token | Escrow : { amount : Natural, bufferFactor : Natural } >

let defaultDebug = { transport = 0, mainLoop = 0, script = 0, setup = 0 }

let Experiment = { rate : Double, duration : Natural, setup : Ex }

in  { defaultDebug, token = Ex.Token, escrow = Ex.Escrow, mkDRange, Experiment }
