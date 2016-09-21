namespace Stamma

type Color = 
    | White
    | Black
    member x.Opp = 
        match x with
        | White -> Black
        | Black -> White

(*
This type is replaced by simple int * int tuples throughout

type Vector = 
    { Forward : int (* changes per player *)
      East : int } (* to the Right of the white player. *)
*)

type Piece = 
    | King
    | Queen
    | Marshal
    | Cardinal
    | Bishop
    | Knight
    | Rook
    | Pawn

type MoveType = 
    | Move
    | Capture
    | Promotion of Piece
    | CapAndPromotion of Piece

type Coordinates = 
    { Rank : int
      File : int }

[<CompilationRepresentation(CompilationRepresentationFlags.UseNullAsTrueValue)>]
type Field = 
    | Empty
    | Piece of Piece * Color

type Position = 
    { Board : Map<Coordinates, Field>
      EnPassant : Coordinates
      Turn : Color
      KingWhite : Coordinates
      KingBlack : Coordinates
      CappedWhite : Piece list
      CappedBlack : Piece list 
      ReversibleCount : int}
