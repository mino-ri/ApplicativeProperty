namespace ApplicativeProperty
open System
open System.ComponentModel
open System.Windows.Input

type internal PropertyChangedEventHolder<'T>(eventDisposers: ResizeArray<struct(PropertyChangedEventHandler * IDisposable)>) =
    struct
        member _.Add(handler: PropertyChangedEventHandler, getProp : IGetProp<'T>) =
            let disposer = getProp.Subscribe(fun _ -> handler.Invoke(getProp, PropertyChangedEventArgs("Value")))
            eventDisposers.Add(handler, disposer)

        member _.Remove(handler: PropertyChangedEventHandler) =
            let index = eventDisposers.FindIndex(0, fun struct(eh, _) -> eh = handler)
            match eventDisposers.[index] with (_, d) -> d.Dispose()
            eventDisposers.RemoveAt(index)           
    end


[<CompiledName("ReadOnlyNotifyProperty")>]
type NotifyGetProp<'T>(getter: IGetProp<'T>) =
    let eventHolder = PropertyChangedEventHolder<'T>(ResizeArray())

    member _.Value = getter.Value

    member _.Subscribe(onNext) = getter.Subscribe(onNext)

    interface INotifyGetProp<'T> with
        member _.Value = getter.Value
        member _.Subscribe(onNext) = getter.Subscribe(onNext)
        member this.add_PropertyChanged(handler) = eventHolder.Add(handler, this)
        member _.remove_PropertyChanged(handler) = eventHolder.Remove(handler)


[<CompiledName("NotifyProperty")>]
type NotifyProp<'T>(getter: IGetProp<'T>, setter: ISetProp<'T>) =
    inherit NotifyGetProp<'T>(getter)

    member _.Value with get() = getter.Value and set(v) = setter.OnNext(v)

    member _.OnNext(value) = setter.OnNext(value)

    member _.AsGet() = NotifyGetProp(getter)

    new(prop: IProp<'T>) = NotifyProp(prop, prop)
    
    interface INotifyProp<'T> with
        member _.OnNext(value) = setter.OnNext(value)


type internal Command(onExecute: obj -> unit, canExecute: IGetProp<bool>) =
    let eventDisposers = ResizeArray<struct(EventHandler * IDisposable)>()    
    member _.Execute(arg: obj) = onExecute(arg)
    member _.CanExecute() = canExecute.Value

    interface ICommand with
        member _.Execute(arg: obj) = onExecute(arg)
        member _.CanExecute(_: obj) = canExecute.Value
        member this.add_CanExecuteChanged(handler) =
            let disposer = canExecute.Subscribe(fun _ -> handler.Invoke(this, EventArgs.Empty))
            eventDisposers.Add(handler, disposer)            
        member _.remove_CanExecuteChanged(handler) =
            let index = eventDisposers.FindIndex(0, fun struct(eh, _) -> eh = handler)
            match eventDisposers.[index] with (_, d) -> d.Dispose()
            eventDisposers.RemoveAt(index)
