open System

[<Measure>] type min

type Task = {
    Name: string
    Duration: int<min>
    IsBackground: bool
}

type LinkType = After | Before | JustAfter | JustBefore
with
    member this.Opposite =
        match this with
        | After -> Before
        | Before -> After
        | JustAfter -> JustBefore
        | JustBefore -> JustAfter

type TaskDependency = {
    LinkType: LinkType
    Task: Task
}

type TaskDependencyGraph = {
    AdjacencyInfo: Map<Task, Set<TaskDependency>>
}
with
    member this.Tasks =
        this.AdjacencyInfo
        |> Map.toSeq
        |> Seq.map (fun (key, _) -> key)

    static member CreateFromTaskRelations(taskRelations: (Task * LinkType * Task) list) =
        let adjacencyInfo =
            [ for (taskA, linkType, taskB) in taskRelations do
                yield taskA, { LinkType = linkType; Task = taskB }
                yield taskB, { LinkType = linkType.Opposite; Task = taskA }
            ]
            |> Seq.groupBy (fun (key, _) -> key)
            |> Seq.map (fun (taskKey, taskSeqValue) ->
                // Read carefully! Sequence to Set: 'Seq' to 'Set'
                let taskSetValue =
                    taskSeqValue
                    |> Seq.map (fun (_key, value) -> value)
                    |> Set.ofSeq
                taskKey, taskSetValue)
            |> Map.ofSeq

        { AdjacencyInfo = adjacencyInfo }

// Soit une tâche

//    static member ForegroundTask(name, duration, predecessors): Task =
//        { Name = name
//          Duration = duration
//          Predecessors = predecessors
//          IsBackground = false }
//
//    static member BackgroundTask(name, duration, predecessors): Task =
//        { Name = name
//          Duration = duration
//          Predecessors = predecessors
//          IsBackground = true }
//
//    static member IndependentForegroundTask(name, duration): Task =
//        Task.ForegroundTask(name, duration, predecessors=[])
//
//    static member IndependentBackgroundTask(name, duration): Task =
//        Task.BackgroundTask(name, duration, predecessors=[])

//reorder Curry
//|> Seq.iter (fun t -> printfn "%s (%d min)" t.Name (int t.Duration))
//|> Seq.sumBy (fun t -> t.Duration)