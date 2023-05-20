namespace ApplicativeProperty

open System
open System.Threading
open System.Windows.Input


type internal Command(onExecute: obj -> unit, canExecute: IGetProp<bool>, context: SynchronizationContext) =
    do
        if isNull context then
            nullArg (nameof context)
    let eventDisposers = ResizeArray<struct (EventHandler * IDisposable)>()
    member _.Execute(arg: obj) = onExecute (arg)
    member _.CanExecute() = canExecute.Value
    new(onExecute, canExecute) = Command(onExecute, canExecute, SynchronizationContext.Current)

    interface ICommand with
        member _.Execute(arg: obj) = onExecute (arg)
        member _.CanExecute(_: obj) = canExecute.Value
        member this.add_CanExecuteChanged(handler) =
            let disposer =
                canExecute.Subscribe(fun _ -> context.Post(SendOrPostCallback(fun _ -> handler.Invoke(this, EventArgs.Empty)), null))
            eventDisposers.Add(handler, disposer)
        member _.remove_CanExecuteChanged(handler) =
            let index = eventDisposers.FindIndex(0, (fun struct (eh, _) -> eh = handler))
            match eventDisposers.[index] with
            | (_, d) -> d.Dispose()
            eventDisposers.RemoveAt(index)
