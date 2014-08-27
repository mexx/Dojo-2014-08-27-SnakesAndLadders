module Game

type GoTo = {
    From: int
    To: int
}

let start {From = start} = start
let ende {To = ende} = ende

type Object = GoTo

let (|Ladder|Snake|) {From = start; To = ende} =
    if (start < ende) then Ladder else Snake

type Player = Player of string

type Board = {
    Size: int
    Objects: Object list
}

type GameScore = GameScore of (Player * int) list

type Throw = Throw of int
let value (Throw value) = value
let Throw n = Throw n

type Dice(seed) =
    let random = System.Random(seed)

    member __.throw() : Throw = random.Next(1, 6) |> Throw
