[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Stamma.Square

let ofAlg (str : string) = 
    { Rank = int str.[0] - 96
      File = str.Substring 1 |> int }

let toAlg sq = sprintf "%c%d" (sq.Rank + 96 |> char) sq.File
