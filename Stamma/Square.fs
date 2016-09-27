[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Stamma.Square

let ofAlg (str : string) = 
    let str = str.ToLowerInvariant()
    if "abcdefghij".IndexOf str.[0] = -1 then failwith "Invalid file"
    let rnk = str.Substring 1 |> int
    if rnk < 1 || rnk > 10 then failwith "Invalid rank"
    { Rank = rnk
      File = int str.[0] - 96 }

let toAlg sq = 
    if sq.Rank < 1 || sq.Rank > 10 || sq.File < 1 || sq.File > 10 then failwith "Invalid coordinates"
    sprintf "%c%d" (sq.File + 96 |> char) sq.Rank
