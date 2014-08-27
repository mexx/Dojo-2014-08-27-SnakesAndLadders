module GameTests

open Xunit
open FsCheck
open FsCheck.Xunit
open Game

[<Fact>]
let peekXUnit () = true

[<Property>]
let ``Augenzahl ist größer gleich 1`` seed =
    let result = Dice(seed).throw()
    (result >= Throw 1) |@ sprintf "%A" result

[<Property>]
let ``Augenzahl ist kleiner gleich 6`` seed =
    let result = Dice(seed).throw()
    result <= Throw 6


let play (board: Board) (GameScore score) (currentPlayer: Player) (throw: Throw) =
    let next position =
        match position + (value throw) with
        | newPosition when newPosition > board.Size -> position
        | newPosition ->
            board.Objects
            |> List.tryFind (start >> (=) newPosition)
            |> Option.map ende
            |> defaultArg <| newPosition

    score
    |> List.map (fun (player, position) -> if currentPlayer = player then (player, next position) else (player, position))
    |> GameScore

[<Property>]
let ``Wenn Aktuelle Position + Augenzahl ist größer als Feldgröße, dann Neue Position = Aktuelle Position nur für den aktuellen Spieler``
     actualPosition throw objects players =
    let fieldSize = actualPosition + (value throw) - 1
    let board = { Size = fieldSize; Objects = objects }
    let player = Player "Hugo"
    let score = GameScore ((player, actualPosition) :: players)
    let result = play board score player throw
    result = score

[<Property>]
let ``Wenn Aktuelle Position + Augenzahl ist kein Leiterstart oder Schlangenkopf, dann Neue Position = Aktuelle Position + Augenzahl nur für den aktuellen Spieler``
     actualPosition throw (Interval (ladderSize, length)) players =
    (ladderSize <> 0) ==>
    let targetPosition = actualPosition + (value throw)
    let objects = [{From = targetPosition + ladderSize; To = targetPosition + length}]
    let board = { Size = targetPosition + length; Objects = objects }
    let player = Player "Hugo"
    let score = GameScore ((player, actualPosition) :: players)
    let result = play board score player throw
    result = GameScore ((player, targetPosition) :: players)

[<Property>]
let ``Wenn Aktuelle Position + Augenzahl ist ein Leiterstart, dann Neue Position = Leiterende nur für den aktuellen Spieler``
     actualPosition throw (Interval (ladderSize, length)) players =
    let targetPosition = actualPosition + (value throw)
    let ladderEndPosition = targetPosition + ladderSize
    let objects = [{From = targetPosition; To = ladderEndPosition }]
    let board = { Size = targetPosition + length; Objects = objects }
    let player = Player "Hugo"
    let score = GameScore ((player, actualPosition) :: players)
    let result = play board score player throw
    result = GameScore ((player, ladderEndPosition) :: players)
