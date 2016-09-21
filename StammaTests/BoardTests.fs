module StammaTests.BoardTests

open Stamma
open NUnit.Framework
open FsUnitTyped

[<Test>]
let ``ofFen for empty board - should succeed``() = 
    let fen = "10/10/10/10/10/10/10/10/10/10"
    let board = Board.ofFen fen
    for r in 1..10 do
        for f in 1..10 do
            let field = 
                board |> Map.find { Rank = r
                                    File = f }
            field |> shouldEqual Empty
            board.Count |> shouldEqual 100
