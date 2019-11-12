// Learn more about F# at http://fsharp.org

open System
open System.Collections.Immutable
open System.Threading

let FirstFloorSpawnProb = 0.1
let AnyFloorSpawnProb = 0.05
let FirstFloorNeed = 0.5
let MAX = 6
let mutable GlobalTimer = 0

type Direction = Up | Down | Both

type LiftAction = 
    MoveUp
    | MoveDown
    | ReleaseAndTake of Direction
    | Release

type Passenger = { spawned: int; from: int; target: int; id: string}

type Lift<'s> = { capacity: int; inside: Passenger list; floor: int; state: 's}
    with
        member lift.HasSomeSpace() = lift.inside.Length < lift.capacity

type Floor = { number: int; queueUp: ImmutableQueue<Passenger>; queueDown: ImmutableQueue<Passenger>; passengerSpawnProbability: float }

type Decider<'s> = Floor [] -> Lift<'s> -> LiftAction * 's

let floorByNumber number (floors: Floor []) =
    floors.[number - 1]

let mutable arrived = List.empty

let arrive passenger =
    printfn "Passenger %A arrived!" passenger
    arrived <- (passenger, GlobalTimer) :: arrived

let release lift floors =
    let (exit, stay) = List.partition (fun pass -> pass.target = lift.floor) lift.inside
    // printfn "RELEASE! Exit: %A, STAY: %A" exit stay
    List.iter arrive exit
    ({lift with inside = stay}, floors)


let rec takeFloor dir (lift: Lift<'s>, floor) =
    let (queue, selectedDirection) =
        match dir with
        | Up -> floor.queueUp, Up
        | Down -> floor.queueDown, Down
        | Both -> if floor.queueDown.IsEmpty then floor.queueUp, Up else floor.queueDown, Down
    if (not queue.IsEmpty) && lift.HasSomeSpace() then
        takeFloor
            dir
            ({lift with inside = queue.Peek() :: lift.inside },
            match selectedDirection with
            | Up -> {floor with queueUp = floor.queueUp.Dequeue()}
            | Down -> {floor with queueDown = floor.queueDown.Dequeue()}
            | Both -> floor // impossible
            )
    else  
        (lift, floor)


let take dir (lift, floors) =
    let floor = floorByNumber lift.floor floors
    // printfn "take, lift at %d, floor: %A" (lift.floor) floor
    let (updatedLift, updatedFloor) = takeFloor dir (lift, floor)
    floors.[lift.floor - 1] <- updatedFloor
    // printfn "taken! after take: %A | %A" updatedLift updatedFloor
    (updatedLift, floors)


let tick<'s> (decider: Decider<'s>) lift floors =
    let (decision, newState) = decider floors lift
    match decision with
    | MoveUp -> {lift with floor = lift.floor + 1; state = newState}, floors
    | MoveDown -> {lift with floor = lift.floor - 1; state = newState}, floors
    | ReleaseAndTake(dir) -> release {lift with state = newState} floors |> take dir
    | Release -> release {lift with state = newState} floors


let createFloor i = 
    let q = ImmutableQueue.Create()
    {number = i + 1; queueUp = q; queueDown = q;
        passengerSpawnProbability =
            if i = 0 then FirstFloorSpawnProb else AnyFloorSpawnProb}


let rand = Random(1488)

let gensym n : string = 
    String(Array.init n (fun _ -> char (rand.Next(97,123))))

let genUID() =
    gensym 6

let tower =
    Array.init MAX createFloor

let randomFloat() =
    rand.NextDouble()

let takeRandom (lst: 'a list) =
    lst.[rand.Next(List.length lst)]

let spawnPassenger currentFloor =
    let target =
        if randomFloat() < FirstFloorNeed && currentFloor <> 1 then
            1
        else
            List.init MAX (fun x -> x + 1) // enumarate floors from 1
            |> List.except [currentFloor]
            |> takeRandom
    {spawned = GlobalTimer; from = currentFloor; target = target; id = genUID()}

let trySpawnPassenger floor =
    if randomFloat() < floor.passengerSpawnProbability then
        let passenger = spawnPassenger floor.number
        printfn "Passenger spawned: %A" passenger
        if passenger.target > floor.number then
            {floor with queueUp = floor.queueUp.Enqueue(passenger)}
        else
            {floor with queueDown = floor.queueDown.Enqueue(passenger)}
    else
        floor


// vizualize

let vizualize (lift, floors) =
    printfn ""
    // Thread.Sleep(500)
    // Console.Clear()
    let printPassengerSeq (inp: seq<string>) =
        if Seq.isEmpty inp then
            ""
        else
            Seq.reduce (fun acc el -> acc + " " + el) inp

    let printPassenger (passenger: Passenger) =
        sprintf "%s.%d>%d" passenger.id passenger.from passenger.target

    let printQueues (floor: Floor) =
        Seq.append floor.queueUp floor.queueDown
        |> Seq.map printPassenger

    let printLiftOrEmpty lift i = 
        if i = lift.floor then
            (sprintf "[%s] " (List.map printPassenger lift.inside |> printPassengerSeq)).PadLeft(46)
        else
            "".PadLeft(46)

    let printFloor (floor: Floor) =
        sprintf "%d) %s | %s" 
            floor.number 
            (printLiftOrEmpty lift floor.number)
            ((printQueues >> printPassengerSeq) floor)

    Seq.iter (printFloor >> printfn "%s") (Seq.rev floors)
    // Thread.Sleep(800)
    (lift, floors)


let getStats() =
    let count = arrived.Length
    if count <> 0 then
        let timings = List.map (fun (pass, time) -> time - pass.spawned) arrived
        let max = List.max timings
        let min = List.min timings
        let avg = List.averageBy float timings
        sprintf "Delivered: %d\nMAX: %d\nMIN: %d\nAVG: %f" count max min avg
    else 
        "Delivered: nobody!"


// engine

let populateFloors = 
    Array.map trySpawnPassenger


let processLoop iterations decider floors lift =
    let mutable mFloors = floors
    let mutable mLift = lift
    let loopBody i =
        printfn "-------------------------"
        printfn "------ iteration %d ----" i
        let populatedFloors = populateFloors mFloors
        vizualize (mLift, populatedFloors) |> ignore
        let (tickLift, tickFloors) = tick decider mLift populatedFloors |> vizualize
        mFloors <- tickFloors
        mLift <- tickLift
    for i = 1 to iterations do
        GlobalTimer <- i
        loopBody i

// lift logic

let toTargetDirection i from =
    if i > from then
        Some(MoveUp)
    else
        if i < from then
            Some(MoveDown)
        else
            None

let firstPassengerDecision floors lift =
    let target =
        if lift.inside.IsEmpty then
            1 // return to 1st floor
        else
            let pass = List.last lift.inside
            pass.target
    printfn "TARGET: %d\nFROM: %d" target lift.floor
    let target = toTargetDirection target lift.floor
                |> Option.defaultValue (ReleaseAndTake(Both))
    (target, []) // no state, we are stateless


let hasSomebodyWaiting floor =
        (floor.queueDown.IsEmpty && floor.queueUp.IsEmpty) |> not

let waitingFloors floors =    
    printfn "Waiting floors: %A" 
        <| (Seq.filter hasSomebodyWaiting floors 
        |> Seq.map (fun x -> x.number))
    Seq.filter hasSomebodyWaiting floors


let callTarget state current floors chooser def =
    printfn "Callmem: %A" state
    match state with
    | Some(target) -> 
        if target = current then
            (target, None) // reset state
        else
            (target, state) // keep state
    | None -> 
        let targetOp = waitingFloors floors |> chooser |> Option.map (fun x -> x.number)
        (Option.defaultValue def targetOp, targetOp)


let firstPassengerDecisionCallable floors lift =
    let (target, nstate) =
        if lift.inside.IsEmpty then
            callTarget lift.state lift.floor floors Seq.tryHead 1 // default 1
        else
            let pass = List.last lift.inside
            (pass.target, None)
    printfn "TARGET: %d\nFROM: %d" target lift.floor
    let decision = toTargetDirection target lift.floor
                   |> Option.defaultValue (ReleaseAndTake(Both))
    (decision, nstate)


let firstPassengerDecisionCallableOpenWhenNeed floors lift =
    let (target, nstate) =
        if lift.inside.IsEmpty then
            callTarget lift.state lift.floor floors Seq.tryHead 1 // default 1
        else
            let pass = List.last lift.inside
            (pass.target, None)
    printfn "TARGET: %d\nFROM: %d" target lift.floor
    if lift.HasSomeSpace() && (hasSomebodyWaiting <| floorByNumber lift.floor floors) then
        (ReleaseAndTake(Both), nstate)
    else
        let decision = 
            toTargetDirection target lift.floor
            |> Option.defaultValue (ReleaseAndTake(Both))
        (decision, nstate)


let testfirstPassengerDecision() =
    let statelesslift = {capacity = 4; inside = []; floor = 1; state = []}
    let floor = trySpawnPassenger tower.[0]
                |> trySpawnPassenger
                |> trySpawnPassenger
                |> trySpawnPassenger
    tower.[0] <- floor
    vizualize (statelesslift, tower) |> ignore
    let decider: Decider<'a list> = firstPassengerDecision
    processLoop 4000 decider tower statelesslift
    printfn "%s" <| getStats()


let testfirstPassengerDecisionPlus() =
    let statefulLift = {capacity = 4; inside = []; floor = 1; state = None}
    let floor = trySpawnPassenger tower.[0]
                |> trySpawnPassenger
                |> trySpawnPassenger
                |> trySpawnPassenger
    tower.[0] <- floor
    vizualize (statefulLift, tower) |> ignore
    let decider: Decider<int option> = firstPassengerDecisionCallableOpenWhenNeed
    processLoop 4000 decider tower statefulLift
    printfn "%s" <| getStats()


[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    // testfirstPassengerDecision()
    testfirstPassengerDecisionPlus()
    0 // return an integer exit code

