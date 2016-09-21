module StammaTests.PieceTests

open Stamma
open NUnit.Framework
open FsUnitTyped

(* Testing Piece.ofChar *)

[<Test>]
let ``Piece.ofChar 'K' should return a White King``() = 
    Piece.ofChar 'K' |> shouldEqual (Piece(King, White))

[<Test>]
let ``Piece.ofChar 'Q' should return a White Queen``() = 
    Piece.ofChar 'Q' |> shouldEqual (Piece(Queen, White))

[<Test>]
let ``Piece.ofChar 'M' should return a White Marshal``() = 
    Piece.ofChar 'M' |> shouldEqual (Piece(Marshal, White))

[<Test>]
let ``Piece.ofChar 'C' should return a White Cardinal``() = 
    Piece.ofChar 'C' |> shouldEqual (Piece(Cardinal, White))

[<Test>]
let ``Piece.ofChar 'N' should return a White Knight``() = 
    Piece.ofChar 'N' |> shouldEqual (Piece(Knight, White))

[<Test>]
let ``Piece.ofChar 'B' should return a White Bishop``() = 
    Piece.ofChar 'B' |> shouldEqual (Piece(Bishop, White))

[<Test>]
let ``Piece.ofChar 'R' should return a White Rook``() = 
    Piece.ofChar 'R' |> shouldEqual (Piece(Rook, White))

[<Test>]
let ``Piece.ofChar 'P' should return a White Pawn``() = 
    Piece.ofChar 'P' |> shouldEqual (Piece(Pawn, White))

[<Test>]
let ``Piece.ofChar 'k' should return a Black King``() = 
    Piece.ofChar 'k' |> shouldEqual (Piece(King, Black))

[<Test>]
let ``Piece.ofChar 'q' should return a Black Queen``() = 
    Piece.ofChar 'q' |> shouldEqual (Piece(Queen, Black))

[<Test>]
let ``Piece.ofChar 'm' should return a Black Marshal``() = 
    Piece.ofChar 'm' |> shouldEqual (Piece(Marshal, Black))

[<Test>]
let ``Piece.ofChar 'c' should return a Black Cardinal``() = 
    Piece.ofChar 'c' |> shouldEqual (Piece(Cardinal, Black))

[<Test>]
let ``Piece.ofChar 'n' should return a Black Knight``() = 
    Piece.ofChar 'n' |> shouldEqual (Piece(Knight, Black))

[<Test>]
let ``Piece.ofChar 'b' should return a Black Bishop``() = 
    Piece.ofChar 'b' |> shouldEqual (Piece(Bishop, Black))

[<Test>]
let ``Piece.ofChar 'r' should return a Black Rook``() = 
    Piece.ofChar 'r' |> shouldEqual (Piece(Rook, Black))

[<Test>]
let ``Piece.ofChar 'p' should return a Black Pawn``() = 
    Piece.ofChar 'p' |> shouldEqual (Piece(Pawn, Black))

(* Testing Piece.toChar *)

[<Test>]
let ``Piece.toChar White King should return 'K'``() = Piece.toChar White King |> shouldEqual 'K'

[<Test>]
let ``Piece.toChar White Queen should return 'Q'``() = Piece.toChar White Queen |> shouldEqual 'Q'

[<Test>]
let ``Piece.toChar White Marshal should return 'M'``() = 
    Piece.toChar White Marshal |> shouldEqual 'M'

[<Test>]
let ``Piece.toChar White Cardinal should return 'C'``() = 
    Piece.toChar White Cardinal |> shouldEqual 'C'

[<Test>]
let ``Piece.toChar White Knight should return 'N'``() = Piece.toChar White Knight |> shouldEqual 'N'

[<Test>]
let ``Piece.toChar White Bishop should return 'B'``() = Piece.toChar White Bishop |> shouldEqual 'B'

[<Test>]
let ``Piece.toChar White Rook should return 'R'``() = Piece.toChar White Rook |> shouldEqual 'R'

[<Test>]
let ``Piece.toChar White Pawn should return 'P'``() = Piece.toChar White Pawn |> shouldEqual 'P'

[<Test>]
let ``Piece.toChar Black King should return 'k'``() = Piece.toChar Black King |> shouldEqual 'k'

[<Test>]
let ``Piece.toChar Black Queen should return 'q'``() = Piece.toChar Black Queen |> shouldEqual 'q'

[<Test>]
let ``Piece.toChar Black Marshal should return 'm'``() = 
    Piece.toChar Black Marshal |> shouldEqual 'm'

[<Test>]
let ``Piece.toChar Black Cardinal should return 'c'``() = 
    Piece.toChar Black Cardinal |> shouldEqual 'c'

[<Test>]
let ``Piece.toChar Black Knight should return 'n'``() = Piece.toChar Black Knight |> shouldEqual 'n'

[<Test>]
let ``Piece.toChar Black Bishop should return 'b'``() = Piece.toChar Black Bishop |> shouldEqual 'b'

[<Test>]
let ``Piece.toChar Black Rook should return 'r'``() = Piece.toChar Black Rook |> shouldEqual 'r'

[<Test>]
let ``Piece.toChar Black Pawn should return 'p'``() = Piece.toChar Black Pawn |> shouldEqual 'p'
