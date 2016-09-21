[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Stamma.Piece

open System

let private charMap = 
    [ ('k', King)
      ('q', Queen)
      ('m', Marshal)
      ('c', Cardinal)
      ('p', Pawn)
      ('n', Knight)
      ('b', Bishop)
      ('r', Rook) ]
    |> Map.ofList

let ofChar c = 
    let color = 
        if Char.IsLower c then Black
        else White
    Piece(Map.find (Char.ToLower c) charMap, color)

let toChar color piece = 
    let ch = Map.findKey (fun c p -> p = piece) charMap
    if color = White then Char.ToUpper ch
    else ch

let internal vectors = 
    function 
    | Rook -> 
        [ (1, 0)
          (0, 1)
          (-1, 0)
          (0, -1) ]
    | Bishop -> 
        [ (1, 1)
          (-1, -1)
          (-1, 1)
          (1, -1) ]
    | King | Queen -> 
        [ (1, 0)
          (0, 1)
          (-1, 0)
          (0, -1)
          (1, 1)
          (-1, -1)
          (-1, 1)
          (1, -1) ]
    | Knight -> 
        [ (-2, -1)
          (-2, 1)
          (-1, -2)
          (-1, 2)
          (1, -2)
          (1, 2)
          (2, -1)
          (2, 1) ]
    | _ -> [] (* unused *)
