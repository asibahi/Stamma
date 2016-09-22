[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Stamma.Position

(* Pawn Promotion logic *)

let private applyVector turn start (x, y) = 
    { Rank = 
          start.Rank + ((if turn = White then 1
                         else -1)
                        * x)
      File = start.File + y }

let private pawnMove start pos = 
    let reach = 
        match start.Rank, pos.Turn with
        | 3, White | 8, Black -> 2
        | _ -> 1
    
    let rec loop start acc r = 
        if r = 0 then acc
        else 
            let dist = applyVector pos.Turn start (1, 0)
            match Map.tryFind dist pos.Board with
            | Some Empty -> loop dist (dist :: acc) (r - 1)
            | _ -> acc
    
    loop start [] reach

let private pawnCap start pos = 
    [ -1; 1 ]
    |> List.map (fun x -> applyVector pos.Turn start (1, x))
    |> List.filter (fun sq -> 
           sq = pos.EnPassant || (match Map.tryFind sq pos.Board with
                                  | Some(Piece(_, c)) when c <> pos.Turn -> true
                                  | _ -> false))

let private promotionTarget start pos = 
    let proTarsWhite = 
        pos.CappedWhite
        |> List.distinct
        |> List.map Promotion
    
    let proTarsBlack = 
        pos.CappedBlack
        |> List.distinct
        |> List.map Promotion
    
    match pos.Turn, start.Rank with
    | White, n when n < 7 -> [ Move ]
    | Black, n when n > 4 -> [ Move ]
    | White, n when n < 9 -> Move :: proTarsWhite
    | Black, n when n > 2 -> Move :: proTarsBlack
    | White, _ -> proTarsWhite
    | _ -> proTarsBlack

(* Way finding Logic :
 * Result is ( Coordinates * Coordinates * Piece )
 * Stands for ( Start Square * End Square * Piece after move )
 * Piece-after-move only changes for Pawns, to indicates different promotion targets.
 *)

let private wayfindCore start pos piece reach = 
    let rec loop start acc reach vector = 
        if reach = 0 then acc
        else 
            let dist = applyVector pos.Turn start vector
            match Map.tryFind dist pos.Board with
            | Some(Piece(p, c)) when c <> pos.Turn -> (dist, Capture) :: acc
            | Some(Empty) -> loop dist ((dist, Move) :: acc) (reach - 1) vector
            | _ -> acc
    Piece.vectors piece
    |> List.collect (loop start [] reach)
    |> List.map (fun (d, m) -> start, d, m)

let rec private wayfindMoveAux start piece pos = 
    match piece with
    | Rook | Bishop | Queen -> wayfindCore start pos piece 10
    | Knight | King -> wayfindCore start pos piece 1
    | Marshal -> wayfindMoveAux start Rook pos @ wayfindMoveAux start Knight pos
    | Cardinal -> wayfindMoveAux start Bishop pos @ wayfindMoveAux start Knight pos
    | Pawn -> 
        pawnMove start pos 
        |> List.collect (fun x -> promotionTarget start pos |> List.map (fun y -> start, x, y))

let private wayfindCapsAux start piece pos = 
    match piece with
    | Pawn -> 
        pawnCap start pos
        |> List.collect (fun x -> promotionTarget start pos |> List.map (fun y -> start, x, y))
        |> List.map (fun (s, d, m) -> 
               (s, d, 
                match m with
                | Move | Capture -> Capture
                | Promotion(p) | CapAndPromotion(p) -> CapAndPromotion(p)))
    | _ -> 
        wayfindMoveAux start piece pos |> List.filter (function 
                                              | _, _, Capture | _, _, CapAndPromotion(_) -> true
                                              | _ -> false)

(* END OF AUX FUNCTIONS *)

(* START OF USEFUL FUNCTIONS *)

let wayfindMovesFrom start pos = 
    match Map.find start pos.Board with
    | Empty -> []
    | Piece(_, c) when c <> pos.Turn -> []
    | Piece(p, _) -> wayfindMoveAux start p pos

let wayfindCapsFrom start pos = 
    match Map.find start pos.Board with
    | Empty -> []
    | Piece(_, c) when c <> pos.Turn -> []
    | Piece(p, _) -> wayfindCapsAux start p pos

let wayfind pos = 
    pos.Board
    |> Map.toList
    |> List.collect (fun (s, f) -> 
           wayfindMovesFrom s pos @ (if f = Piece(Pawn, pos.Turn) then wayfindCapsFrom s pos
                                     else []))

(* Legality checking  *)
let isAttacked loc pos = 
    List.exists (fun (_, dist, _) -> 
        match Map.find dist pos.Board with
        | Piece(Rook, c) | Piece(Queen, c) | Piece(Marshal, c) when c = pos.Turn.Opp -> true
        | _ -> false) (wayfindCapsAux loc Rook pos) 
    && List.exists (fun (_, dist, _) -> 
           match Map.find dist pos.Board with
           | Piece(Knight, c) | Piece(Cardinal, c) | Piece(Marshal, c) when c = pos.Turn.Opp -> true
           | _ -> false) (wayfindCapsAux loc Knight pos) 
    && List.exists (fun (_, dist, _) -> 
           match Map.find dist pos.Board with
           | Piece(Bishop, c) | Piece(Queen, c) | Piece(Cardinal, c) when c = pos.Turn.Opp -> true
           | _ -> false) (wayfindCapsAux loc Bishop pos) 
    && List.exists (fun (_, dist, _) -> 
           match Map.find dist pos.Board with
           | Piece(Pawn, c) when c <> pos.Turn -> true
           | _ -> false) (wayfindCapsAux loc Pawn pos)
    && List.exists (fun (_, dist, _) -> 
           match Map.find dist pos.Board with
           | Piece(King, c) when c <> pos.Turn -> true
           | _ -> false) (wayfindCapsAux loc King pos)

let isDefended loc pos = isAttacked loc { pos with Turn = pos.Turn.Opp }

let isCheck pos = 
    if pos.Turn = White then isAttacked pos.KingWhite pos
    else isAttacked pos.KingBlack pos

let isLegal pos = 
    (* checks if the wrong king is in check *)
    isCheck { pos with Turn = pos.Turn.Opp } |> not

let applyMove pos (start, dist, move) = 
    let movingPiece = 
        match Map.find start pos.Board with
        | Piece(p, c) when c = pos.Turn -> p
        | _ -> failwith "What are you trying to move?"
    
    let endPiece, promo = 
        match move with
        | Promotion(p) | CapAndPromotion(p) -> p, true
        | _ -> movingPiece, false
    
    let capturedPiece = 
        match move with
        | Move | Promotion(_) -> None
        | _ -> 
            match Map.find dist pos.Board with
            | Piece(p, _) when p <> Pawn -> Some p
            | _ -> None
    
    let update capped turn = 
        match pos.Turn, capturedPiece with
        | t, Some p when t = turn -> p :: capped
        | _ -> capped
    
    let rec remove pred lst = 
        match lst with
        | h :: t when pred h -> t
        | h :: t -> h :: remove pred t
        | _ -> []
    
    let newBoard = 
        pos.Board
        |> Map.add dist (Piece(endPiece, pos.Turn))
        |> Map.add start Empty
    
    { pos with Board = 
                   if pos.EnPassant = dist && movingPiece = Pawn then 
                       newBoard |> Map.add { Rank = start.Rank
                                             File = dist.File } Empty
                   else newBoard
               KingWhite = 
                   if movingPiece = King && pos.Turn = White then dist
                   else pos.KingWhite
               KingBlack = 
                   if movingPiece = King && pos.Turn = Black then dist
                   else pos.KingBlack
               CappedWhite = update pos.CappedWhite Black |> remove (fun x -> promo && x = endPiece)
               CappedBlack = update pos.CappedBlack White |> remove (fun x -> promo && x = endPiece)
               Turn = pos.Turn.Opp
               EnPassant = 
                   if movingPiece = Pawn && abs (start.Rank - dist.Rank) = 2 then 
                       { Rank = (start.Rank + dist.Rank) / 2
                         File = start.File }
                   else 
                       { Rank = 0
                         File = 0 } }

let wayfindLegalPositions pos = 
    pos
    |> wayfind
    |> List.map (applyMove pos)
    |> List.filter isLegal

let isCheckmate pos = isCheck pos && (wayfindLegalPositions pos |> List.length) = 0
let isStalemate pos = not (isCheck pos) && (wayfindLegalPositions pos |> List.length) = 0

let toFen pos = 
    let boardFen = Board.toFen pos.Board
    
    let turn = 
        match pos.Turn with
        | White -> "w"
        | Black -> "b"
    
    let epSq = 
        match pos.EnPassant with
        | { Rank = 0; File = 0 } -> "-"
        | _ -> Square.toAlg pos.EnPassant
    
    sprintf "%s %s %s %d" boardFen turn epSq pos.ReversibleCount

let ofFen (fen : string) = 
    let e = fen.Split ' '
    let board = Board.ofFen e.[0]
    
    let capped color = 
        let pList = 
            board
            |> Map.toList
            |> List.choose (fun (_, f) -> 
                   match f with
                   | Piece(p, c) when p <> Pawn && c = color -> Some p
                   | _ -> None)
            |> List.countBy id
        
        let mutable pieces = []
        for p in [ Queen; Marshal; Cardinal ] do
            if List.contains (p, 1) pList |> not then pieces <- p :: pieces
        for p in [ Knight; Bishop; Rook ] do
            if List.contains (p, 2) pList |> not then 
                if List.contains (p, 1) pList then pieces <- p :: pieces
                else pieces <- p :: p :: pieces
        pieces
    { Board = board
      EnPassant = 
          if e.[2] = "-" then 
              { Rank = 0
                File = 0 }
          else Square.ofAlg e.[2]
      Turn = 
          if e.[1] = "w" then White
          else Black
      KingWhite = Map.findKey (fun sq p -> p = Piece(King, White)) board
      KingBlack = Map.findKey (fun sq p -> p = Piece(King, Black)) board
      CappedWhite = capped White
      CappedBlack = capped Black
      ReversibleCount = int e.[3] }
