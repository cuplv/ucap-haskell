let Ex = < Token | Escrow : { amount : Natural, bufferFactor : Natural } >

let defaultDebug = { transport = 0, mainLoop = 0, script = 0, setup = 0 }

in  { defaultDebug, token = Ex.Token, escrow = Ex.Escrow }
