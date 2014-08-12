open System

[<Measure>] type min

type SuccessionLink =
    | ImmediatelyAfter of Task
    | After of Task

and Task = {
    Name: string
    Duration: int<min>
    Predecessors: SuccessionLink list
    IsBackground: bool
}
with
    static member ForegroundTask(name, duration, predecessors): Task =
        { Name = name
          Duration = duration
          Predecessors = predecessors
          IsBackground = false }

    static member BackgroundTask(name, duration, predecessors): Task =
        { Name = name
          Duration = duration
          Predecessors = predecessors
          IsBackground = true }

    static member IndependentForegroundTask(name, duration): Task =
        Task.ForegroundTask(name, duration, predecessors=[])

    static member IndependentBackgroundTask(name, duration): Task =
        Task.BackgroundTask(name, duration, predecessors=[])

let Curry: Task =
    let poulet =
        ("Préparer cuisses de poulet", 4<min>)
        |> Task.IndependentForegroundTask

    let aubergines =
        ("Couper aubergines", 3<min>)
        |> Task.IndependentForegroundTask

    let haricots =
        ("Equeuter haricots", 4<min>)
        |> Task.IndependentForegroundTask

    let preparerPatesCurry =
        ("Préparer pâtes de curry", 1<min>)
        |> Task.IndependentForegroundTask

    let decongelerKroeung =
        ("Décongeler kroeung", 3<min>, [After preparerPatesCurry])
        |> Task.BackgroundTask

    let revenirCurryCoco =
        ("Faire revenir pâte de curry dans coco", 3<min>, [After decongelerKroeung])
        |> Task.ForegroundTask

    let colorerPoulet =
        ("Faire revenir cuisses de poulet", 5<min>, [After poulet; ImmediatelyAfter revenirCurryCoco])
        |> Task.ForegroundTask

    let ajouterSauce =
        ("Ajouter coco, sauce, aromates et eau", 3<min>, [ImmediatelyAfter colorerPoulet])
        |> Task.ForegroundTask

    let mijoterPoulet =
        ("Laisser mijoter le poulet à couvert", 40<min>, [ImmediatelyAfter ajouterSauce])
        |> Task.BackgroundTask

    let mijoterLégumes =
        ("Laisser mijoter légumes", 15<min>, [ImmediatelyAfter mijoterPoulet; After aubergines; After haricots])
        |> Task.BackgroundTask

    let assaisonner =
        ("Baisser le feu et assaisonner", 5<min>, [ImmediatelyAfter mijoterLégumes])
        |> Task.ForegroundTask

    assaisonner

let reorder task =
    let rec visit task = [
        yield task
        for (After pred | ImmediatelyAfter pred) in task.Predecessors do
            yield! (visit pred)
    ]
    
    visit task
    |> List.rev


let projectOnTimeline (taskList: seq<Task>) = [
    let currentStartTime = ref 0<min>
    for task in taskList do
        yield task, !currentStartTime
        currentStartTime := !currentStartTime + task.Duration
]

let projection =
    Curry
    |> reorder
    |> projectOnTimeline

let duration (task: Task) =
    let projection = task |> reorder |> projectOnTimeline

    projection
    |> List.map (fun (task, startTime) -> task.Duration + startTime)
    |> List.max

Curry |> duration

//reorder Curry
//|> Seq.iter (fun t -> printfn "%s (%d min)" t.Name (int t.Duration))
//|> Seq.sumBy (fun t -> t.Duration)