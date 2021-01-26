namespace ApplicativeProperty
open System
open System.Collections.Generic

type internal ObserverHolder<'T>(collection: ResizeArray<ObserverHolder<'T>>, onNext: 'T -> unit) =
    let mutable disposed = false
    member __.OnNext(value) = onNext(value)

    interface IDisposable with
        member this.Dispose() =
            if not disposed then
                disposed <- true
                ignore (collection.Remove(this))


type ValueProp<'T>(init: 'T, compare: 'T -> 'T -> bool) =
    let mutable backField = init
    let holders = ResizeArray<ObserverHolder<'T>>()

    member this.Value with get() = backField and set(v) = this.OnNext(v)
    
    member _.Subscribe(onNext) =
        let holder = new ObserverHolder<'T>(holders, onNext)
        holders.Add(holder)
        holder :> IDisposable

    member _.OnNext(value) =
        if not (compare backField value) then
            backField <- value
            for holder in holders do holder.OnNext(value)

    new(init: 'T) = ValueProp(init, fun x y -> EqualityComparer<'T>.Default.Equals(x, y))

    interface IProp<'T> with
        member this.Value = this.Value
        member this.OnNext(value) = this.OnNext(value)
        member this.Subscribe(onNext) = this.Subscribe(onNext)


type ConstProp<'T>(init: 'T) =
    member _.Value = init
    member _.Subscribe(onNext: 'T -> unit) = Disposable.dummy

    interface IGetProp<'T> with
        member _.Value = init
        member _.Subscribe(_) = Disposable.dummy
