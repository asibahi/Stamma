module StammaTests.PieceTests

open Stamma
open NUnit.Framework
open FsUnitTyped

(* Testing Piece.ofChar *)
[<TestCase('K', "White", "King")>]
[<TestCase('Q', "White", "Queen")>]
[<TestCase('M', "White", "Marhsal")>]
[<TestCase('C', "White", "Cardinal")>]
[<TestCase('N', "White", "Knight")>]
[<TestCase('B', "White", "Bishop")>]
[<TestCase('R', "White", "Rook")>]
[<TestCase('P', "White", "Pawn")>]
[<TestCase('k', "Black", "King")>]
[<TestCase('q', "Black", "Queen")>]
[<TestCase('m', "Black", "Marhsal")>]
[<TestCase('c', "Black", "Cardinal")>]
[<TestCase('n', "Black", "Knight")>]
[<TestCase('b', "Black", "Bishop")>]
[<TestCase('r', "Black", "Rook")>]
[<TestCase('p', "Black", "Pawn")>]
let ``ofChar Test`` (ch, colorStr, pieceStr) = 
    let expectedColor = Helpers.toColor colorStr
    let expectedPiece = Helpers.toPiece pieceStr
    Piece.ofChar ch |> shouldEqual (Piece(expectedPiece, expectedColor))

(* Testing Piece.toChar *)

[<TestCase("White", "King", 'K')>]
[<TestCase("White", "Queen", 'Q')>]
[<TestCase("White", "Marhsal", 'M')>]
[<TestCase("White", "Cardinal", 'C')>]
[<TestCase("White", "Knight", 'N')>]
[<TestCase("White", "Bishop", 'B')>]
[<TestCase("White", "Rook", 'R')>]
[<TestCase("White", "Pawn", 'P')>]
[<TestCase("Black", "King", 'k')>]
[<TestCase("Black", "Queen", 'q')>]
[<TestCase("Black", "Marhsal", 'm')>]
[<TestCase("Black", "Cardinal", 'c')>]
[<TestCase("Black", "Knight", 'n')>]
[<TestCase("Black", "Bishop", 'b')>]
[<TestCase("Black", "Rook", 'r')>]
[<TestCase("Black", "Pawn", 'p')>]
let ``toChar Test`` (colorStr, pieceStr, ch) = 
    let givenColor = Helpers.toColor colorStr
    let givenPiece = Helpers.toPiece pieceStr
    Piece.toChar givenColor givenPiece |> shouldEqual ch
