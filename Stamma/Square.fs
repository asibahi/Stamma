[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Stamma.Square

let ofAlg (str : string) = 
    { Rank = str.Substring 1 |> int
      File = int str.[0] - 96 }

let toAlg sq = sprintf "%c%d" (sq.File + 96 |> char) sq.Rank
