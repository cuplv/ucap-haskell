let Ex =
      < Token : { initOwner : Text }
      | Escrow : { initOwner : Text, amount : Natural, bufferFactor : Natural }
      >

let initOwner = "alpha"

in  { escrow = Ex.Escrow { initOwner, amount = 20000, bufferFactor = 1 }
    , token = Ex.Token { initOwner }
    , Ex
    }
