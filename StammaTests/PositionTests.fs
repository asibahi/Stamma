module StammaTests.PositionTests

open Stamma
open NUnit.Framework
open FsUnitTyped

[<Test>]
let ``isCheckmate position - should pass``() = 
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
