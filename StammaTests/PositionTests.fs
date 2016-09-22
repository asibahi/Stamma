module StammaTests.PositionTests

open Stamma
open NUnit.Framework
open FsUnitTyped

[<Test>]
let ``ofFen position - should be true`` () =
    let pos =
        "r8r/1nbqkmcbn1/pppppppppp/10/10/10/10/PPPPPPPPPP/1NBQKMCBN1/R8R w - 0"
        |> Position.ofFen 

    pos.KingWhite |> Square.toAlg |> shouldEqual "e2"
    pos.KingBlack |> Square.toAlg |> shouldEqual "e9"
    pos.CappedWhite |> shouldBeEmpty
    pos.CappedBlack |> shouldBeEmpty
    pos.EnPassant |> shouldEqual {Rank = 0; File = 0}
    pos.ReversibleCount |> shouldEqual 0
    pos.Turn |> shouldEqual White 
    pos.Board |> shouldEqual Board.start



[<Test>]
let ``isCheckmate position - should be true``() = 
    { Board = Board.ofFen "r4q3r/1nb1kmcbn1/ppCppppppp/10/10/10/10/PPPPPPPPPP/1NBQKM1BN1/R8R"
      Turn = Black
      EnPassant = 
          { Rank = 0
            File = 0 }
      KingWhite = 
          { Rank = 5
            File = 2 }
      KingBlack = 
          { Rank = 5
            File = 9 }
      CappedWhite = []
      CappedBlack = []
      ReversibleCount = 0 }
    |> Position.isCheckmate
    |> shouldEqual true
