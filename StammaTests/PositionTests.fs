module StammaTests.PositionTests

open Stamma
open NUnit.Framework
open FsUnitTyped

[<Test>]
let ``ofFen starting position``() = 
    let pos = Position.ofFen "r8r/1nbqkmcbn1/pppppppppp/10/10/10/10/PPPPPPPPPP/1NBQKMCBN1/R8R w - 0"
    Square.toAlg pos.KingWhite |> shouldEqual "e2"
    Square.toAlg pos.KingBlack |> shouldEqual "e9"
    pos.CappedWhite |> shouldBeEmpty
    pos.CappedBlack |> shouldBeEmpty
    pos.EnPassant |> shouldEqual { Rank = 0
                                   File = 0 }
    pos.ReversibleCount |> shouldEqual 0
    pos.Turn |> shouldEqual White
    pos.Board |> shouldEqual Board.start

[<Test>]
let ``ofFen KK``() = 
    let pos = Position.ofFen "10/10/10/10/10/3k1K4/10/10/10/10 w - 0"
    Square.toAlg pos.KingWhite |> shouldEqual "f5"
    Square.toAlg pos.KingBlack |> shouldEqual "d5"
    pos.CappedWhite |> shouldHaveLength 9
    pos.CappedBlack |> shouldHaveLength 9

[<Test>]
let ``ofFen KQK``() = 
    let pos = Position.ofFen "10/10/10/10/10/3k1K2Q1/10/10/10/10 w - 0"
    Square.toAlg pos.KingWhite |> shouldEqual "f5"
    Square.toAlg pos.KingBlack |> shouldEqual "d5"
    pos.CappedWhite |> shouldHaveLength 8
    pos.CappedWhite |> shouldNotContain Queen
    pos.CappedBlack |> shouldHaveLength 9

[<Test>]
let ``ofFen Fool's mate``() = 
    let pos = 
        Position.ofFen "r4q3r/1nb1kmcbn1/ppCppppppp/10/10/10/10/PPPPPPPPPP/1NBQKM1BN1/R8R b - 0"
    Square.toAlg pos.KingWhite |> shouldEqual "e2"
    Square.toAlg pos.KingBlack |> shouldEqual "e9"
    pos.CappedWhite |> shouldBeEmpty
    pos.CappedBlack |> shouldBeEmpty
    pos.Turn |> shouldEqual Black

[<Test>]
let ``ofFen KQPK : Pal Benko composition Mate in 2``() = 
    let pos = Position.ofFen "10/9k/10/7P2/8K1/10/10/10/10/Q9 w - 0"
    Square.toAlg pos.KingWhite |> shouldEqual "i6"
    Square.toAlg pos.KingBlack |> shouldEqual "j9"
    pos.CappedWhite |> shouldHaveLength 8
    pos.CappedWhite |> shouldNotContain Queen
    pos.CappedBlack |> shouldHaveLength 9

[<Test>]
let ``isCheckmate Fool's mate - should be true (obviously)``() = 
    Position.ofFen "r4q3r/1nb1kmcbn1/ppCppppppp/10/10/10/10/PPPPPPPPPP/1NBQKM1BN1/R8R b - 0"
    |> Position.isCheckmate
    |> shouldEqual true
