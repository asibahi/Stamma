module StammaTests.Helpers

open Stamma

let toColor = 
    function 
    | "White" -> White
    | "Black" -> Black
    | _ -> failwith "Invalid"

let toPiece = 
    function 
    | "King" -> King
    | "Queen" -> Queen
    | "Marhsal" -> Marshal
    | "Cardinal" -> Cardinal
    | "Knight" -> Knight
    | "Bishop" -> Bishop
    | "Rook" -> Rook
    | "Pawn" -> Pawn
    | _ -> failwith "Invalid"
