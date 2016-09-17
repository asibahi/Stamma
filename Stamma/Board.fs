[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Stamma.Board

let private ofFenRank fenR = 
    let mutable baseArray = Array.create 10 Empty
    if fenR = "10" then baseArray
    else 
        let rec loop (rnk : string) count = 
            if count = 10 then ()
            elif System.Char.IsLetter rnk.[0] then 
                baseArray.[count] <- Piece.ofChar rnk.[0]
                loop (rnk.Substring 1) (count + 1)
            else 
                let dgt = int rnk.[0] - 48
                loop (rnk.Substring 1) (count + dgt)
        loop fenR 0
        baseArray

let ofFen (fen : string) = 
    fen.Split '/'
    |> Array.rev
    |> Array.map ofFenRank
    |> Array.collect id
    |> Array.mapi (fun idx F -> 
           ({ Rank = idx / 10 + 1
              File = idx % 10 + 1 }, F))
    |> Map.ofArray

(* logic translated from my C# experiment, which I probably stole from somewhere *)
let toFen board = 
    let mutable esc = 0
    let mutable sb = System.Text.StringBuilder()
    for r in 10..-1..1 do
        for f in 1..10 do
            match Map.find { Rank = r
                             File = f } board with
            | Empty -> esc <- esc + 1
            | Piece(p, c) -> 
                if esc > 0 then 
                    sb.Append esc |> ignore
                    esc <- 0
                else sb.Append(Piece.toChar c p) |> ignore
        if esc > 0 then 
            sb.Append esc |> ignore
            esc <- 0
        if r > 1 then sb.Append '/' |> ignore
    sb.ToString()

let empty = ofFen "10/10/10/10/10/10/10/10/10/10"
let start = 
    ofFen "r8r/1nbqkmcbn1/pppppppppp/10/10/10/10/PPPPPPPPPP/1NBQKMCBN1/R8R"

let toAscii board = 
    seq { 
        for r in 10..-1..1 do
            for f in 1..11 do
                if f = 11 then yield sprintf "| %d\n" r
                else 
                    match Map.find { Rank = r
                                     File = f } board with
                    | Empty -> yield "| "
                    | Piece(p, c) -> yield sprintf "|%c" (Piece.toChar c p)
        yield " a b c d e f g h i j\n"
    }
    |> String.concat ""
