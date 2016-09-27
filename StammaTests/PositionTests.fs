module StammaTests.PositionTests

open Stamma
open NUnit.Framework
open FsUnitTyped

(* ofFen Tests *)
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

(* isCheck tests *)

[<TestCase("r4q3r/1nb1kmcbn1/ppCppppppp/10/10/10/10/PPPPPPPPPP/1NBQKM1BN1/R8R b - 0")>]
[<TestCase("9k/10/7C2/10/10/9K/10/10/10/10 b - 0")>]
[<TestCase("5k4/R9/5M4/10/5K4/10/10/10/10/10 b - 0")>]
[<TestCase("5K4/r9/5m4/10/5k4/10/10/10/10/10 w - 0")>]
[<TestCase("9k/9R/7N2/10/K9/10/10/10/10/10 b - 0")>]
// fool's mate
// Cardinal mate
// Rook and marshal
// Rook and marshal opposite colors
// Arabian mate
let ``isCheck tests - checkmate input - should be true`` (str) = 
    Position.ofFen str
    |> Position.isCheck
    |> shouldEqual true

(* isLegal tests *)

[<TestCase("10/10/5K4/5k4/10/5p4/5P4/10/10/10 w - 0")>]
[<TestCase("10/10/10/4Kk4/10/5p4/5P4/10/10/10 b - 0")>]
[<TestCase("R3k5/10/5K4/10/10/10/10/10/10/10 w - 0")>]
[<TestCase("r3K5/10/5k4/10/10/10/10/10/10/10 b - 0")>]
// Kings touching
// Kings touching
// Black in check white to move
// White in check black to move
let ``isLegal tests - illegal positions - should be false`` (str) = 
    Position.ofFen str
    |> Position.isLegal
    |> shouldEqual false

[<TestCase("r4q3r/1nb1kmcbn1/ppCppppppp/10/10/10/10/PPPPPPPPPP/1NBQKM1BN1/R8R b - 0")>]
[<TestCase("9k/10/7C2/10/10/9K/10/10/10/10 b - 0")>]
[<TestCase("5k4/R9/5M4/10/5K4/10/10/10/10/10 b - 0")>]
[<TestCase("5K4/r9/5m4/10/5k4/10/10/10/10/10 w - 0")>]
[<TestCase("9k/9R/7N2/10/K9/10/10/10/10/10 b - 0")>]
let ``isLegal tests - checkmate positions - should be true`` (str) = 
    Position.ofFen str
    |> Position.isLegal
    |> shouldEqual true

(* isCheckmate tests *)

[<TestCase("r4q3r/1nb1kmcbn1/ppCppppppp/10/10/10/10/PPPPPPPPPP/1NBQKM1BN1/R8R b - 0")>]
[<TestCase("9k/10/7C2/10/10/9K/10/10/10/10 b - 0")>]
[<TestCase("5k4/R9/5M4/10/5K4/10/10/10/10/10 b - 0")>]
[<TestCase("5K4/r9/5m4/10/5k4/10/10/10/10/10 w - 0")>]
[<TestCase("9k/9R/7N2/10/K9/10/10/10/10/10 b - 0")>]
let ``isCheckmate tests - actual checkmates input - should be true`` (str) = 
    Position.ofFen str
    |> Position.isCheckmate
    |> shouldEqual true

[<TestCase("5k4/5P4/5K4/10/10/10/10/10/10/10 b - 0")>]
[<TestCase("5k4/10/5P4/5K4/10/10/10/10/10/10 b - 0")>]
[<TestCase("5k4/5P4/5K4/10/10/10/10/10/10/10 w - 0")>]
[<TestCase("5k4/10/5P4/5K4/10/10/10/10/10/10 w - 0")>]
let ``isCheckmate tests - no check input - should be false`` (str) = 
    Position.ofFen str
    |> Position.isCheckmate
    |> shouldEqual false

(* isStalemate test *)
[<Test>]
let ``isStalemate tests - classic stalemate - should be true``() = 
    Position.ofFen "5k4/5P4/5K4/10/10/10/10/10/10/10 b - 0"
    |> Position.isStalemate
    |> shouldEqual true

[<Test>]
let ``isStalemate tests - not stalemate - should be false``() = 
    Position.ofFen "5k4/10/5P4/5K4/10/10/10/10/10/10 b - 0"
    |> Position.isStalemate
    |> shouldEqual false
