module StammaTests.SquareTests

open Stamma
open NUnit.Framework
open FsUnitTyped

[<TestCase(1, 3, "a3")>]
[<TestCase(1, 6, "a6")>]
[<TestCase(1, 10, "a10")>]
[<TestCase(2, 2, "b2")>]
[<TestCase(2, 6, "b6")>]
[<TestCase(2, 10, "b10")>]
[<TestCase(3, 2, "c2")>]
[<TestCase(4, 5, "d5")>]
[<TestCase(4, 9, "d9")>]
[<TestCase(5, 2, "e2")>]
[<TestCase(5, 4, "e4")>]
[<TestCase(5, 5, "e5")>]
[<TestCase(5, 8, "e8")>]
[<TestCase(5, 9, "e9")>]
[<TestCase(6, 1, "f1")>]
[<TestCase(6, 7, "f7")>]
[<TestCase(6, 9, "f9")>]
[<TestCase(7, 5, "g5")>]
[<TestCase(7, 4, "g4")>]
[<TestCase(7, 8, "g8")>]
[<TestCase(7, 10, "g10")>]
[<TestCase(9, 7, "i7")>]
[<TestCase(8, 8, "h8")>]
[<TestCase(10, 8, "j8")>]
[<TestCase(10, 10, "j10")>]
let ``toAlg Test - valid arguments - passes`` (file, rank, expected) = 
    Square.toAlg { Rank = rank
                   File = file }
    |> shouldEqual expected

[<TestCase(1, 1, "a1")>]
[<TestCase(1, 3, "A3")>]
[<TestCase(1, 4, "a4")>]
[<TestCase(1, 7, "A7")>]
[<TestCase(1, 9, "a9")>]
[<TestCase(3, 4, "C4")>]
[<TestCase(3, 8, "c8")>]
[<TestCase(3, 9, "C9")>]
[<TestCase(4, 3, "d3")>]
[<TestCase(4, 5, "D5")>]
[<TestCase(4, 6, "d6")>]
[<TestCase(5, 2, "E2")>]
[<TestCase(5, 3, "e3")>]
[<TestCase(5, 5, "E5")>]
[<TestCase(5, 7, "e7")>]
[<TestCase(6, 1, "F1")>]
[<TestCase(6, 7, "f7")>]
[<TestCase(6, 8, "F8")>]
[<TestCase(6, 10, "f10")>]
[<TestCase(7, 2, "G2")>]
[<TestCase(8, 3, "h3")>]
[<TestCase(8, 7, "H7")>]
[<TestCase(9, 7, "i7")>]
[<TestCase(10, 2, "J2")>]
[<TestCase(10, 5, "j5")>]
let ``ofAlg Test - valid argument - passes`` (file, rank, str) = 
    Square.ofAlg str |> shouldEqual { Rank = rank
                                      File = file }

[<TestCase("a")>]
[<TestCase("")>]
[<TestCase("5r")>]
[<TestCase("s100")>]
[<TestCase("435")>]
[<TestCase("fd")>]
[<TestCase("ب4")>]
[<TestCase("d11")>]
[<TestCase("d0")>]
[<TestCase("k5")>]
let ``ofAlg Test - inavlid arguments - throws`` (str) = 
    (fun _ -> Square.ofAlg str |> ignore) |> shouldFail

[<TestCase(-5, 6)>]
[<TestCase(-5, 17)>]
[<TestCase(0, 6)>]
[<TestCase(1, 11)>]
[<TestCase(1, 16)>]
[<TestCase(3, 11)>]
[<TestCase(10, -1)>]
[<TestCase(14, 9)>]
[<TestCase(16, -5)>]
[<TestCase(19, 10)>]
let ``toAlg test - invalid argument - throws`` (file, rank) = 
    (fun _ -> 
    Square.toAlg { Rank = rank
                   File = file }
    |> ignore)
    |> shouldFail
