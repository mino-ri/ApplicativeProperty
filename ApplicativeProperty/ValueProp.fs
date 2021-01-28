namespace ApplicativeProperty
open System
open System.Collections.Generic
open System.ComponentModel


type internal PropertyChangedEventHolder<'T>(eventDisposers: ResizeArray<struct(PropertyChangedEventHandler * IDisposable)>) =
    struct
        member _.Add(handler: PropertyChangedEventHandler, target : IObservable<'T>) =
            let disposer = target.Subscribe(fun _ -> handler.Invoke(target, PropertyChangedEventArgs("Value")))
            eventDisposers.Add(handler, disposer)

        member _.Remove(handler: PropertyChangedEventHandler) =
            let index = eventDisposers.FindIndex(0, fun struct(eh, _) -> eh = handler)
            match eventDisposers.[index] with (_, d) -> d.Dispose()
            eventDisposers.RemoveAt(index)           
    end


type internal ObserverHolder<'T>(collection: ResizeArray<ObserverHolder<'T>>, observer: IObserver<'T>) =
    let mutable disposed = false
    member _.Observer = observer

    interface IDisposable with
        member this.Dispose() =
            if not disposed then
                disposed <- true
                ignore (collection.Remove(this))


type Subject<'T>() =
    let observers = ResizeArray<ObserverHolder<'T>>()

    member _.OnNext(value) = for holder in observers do holder.Observer.OnNext(value)

    member _.OnCompleted() = for holder in observers do holder.Observer.OnCompleted()

    member _.OnError(error) = for holder in observers do holder.Observer.OnError(error)

    member _.Subscribe(observer) =
        let holder = new ObserverHolder<'T>(observers, observer)
        observers.Add(holder)
        holder :> IDisposable

    interface ISubject<'T> with
        member this.OnNext(value) = this.OnNext(value)
        member this.OnCompleted() = this.OnCompleted()
        member this.OnError(error) = this.OnError(error)
        member this.Subscribe(observer) = this.Subscribe(observer)


type ValueProp<'T>(init: 'T) =
    let mutable backField = init
    let observers = ResizeArray<ObserverHolder<'T>>()
    let events = PropertyChangedEventHolder<'T>(ResizeArray())

    member this.Value with get() = backField and set(v) = this.OnNext(v)
    
    member _.Subscribe(onNext) =
        let holder = new ObserverHolder<'T>(observers, onNext)
        observers.Add(holder)
        holder :> IDisposable

    member _.OnNext(value) =
        if not (EqualityComparer<'T>.Default.Equals(backField, value)) then
            backField <- value
            for holder in observers do holder.Observer.OnNext(value)

    member _.OnCompleted() = for holder in observers do holder.Observer.OnCompleted()

    member _.OnError(error) = for holder in observers do holder.Observer.OnError(error)

    interface IProp<'T> with
        member this.Value = this.Value
        member this.OnNext(value) = this.OnNext(value)
        member this.OnCompleted() = this.OnCompleted()
        member this.OnError(error) = this.OnError(error)
        member this.Subscribe(onNext) = this.Subscribe(onNext)
        member this.add_PropertyChanged(handler) = events.Add(handler, this)
        member _.remove_PropertyChanged(handler) = events.Remove(handler)


type ConstProp<'T>(init: 'T) =
    member _.Value = init

    member _.Subscribe(observer: IObserver<'T>) = Disposable.dummy

    interface IGetProp<'T> with
        member _.Value = init
        member _.Subscribe(_) = Disposable.dummy
        member _.add_PropertyChanged(_) = ()
        member _.remove_PropertyChanged(_) = ()


type ObserverProp<'T>(init: 'T) =
    let mutable backField = init
    let observers = ResizeArray<ObserverHolder<'T>>()
    let events = PropertyChangedEventHolder<'T>(ResizeArray())
    let disposables = new CompositeDisposable()
    let selfObserver =
        { new IObserver<'T> with
            member _.OnNext(value) =
                if not (EqualityComparer<'T>.Default.Equals(backField, value)) then
                    backField <- value
                    for holder in observers do holder.Observer.OnNext(value)
            member _.OnCompleted() = for holder in observers do holder.Observer.OnCompleted()
            member _.OnError(error) = for holder in observers do holder.Observer.OnError(error)
        }

    member _.Value = backField

    member _.Observe(observable: IObservable<'T>) = disposables.Add(observable.Subscribe(selfObserver))

    member _.Subscribe(onNext) =
        let holder = new ObserverHolder<'T>(observers, onNext)
        observers.Add(holder)
        holder :> IDisposable

    member _.Dispose() = disposables.Dispose();

    interface IGetProp<'T> with
        member this.Value = this.Value
        member this.Subscribe(onNext) = this.Subscribe(onNext)
        member this.add_PropertyChanged(handler) = events.Add(handler, this)
        member _.remove_PropertyChanged(handler) = events.Remove(handler)

    interface IDisposable with
        member this.Dispose() = this.Dispose()


type internal DelegationProp<'T>(observer: IObserver<'T>, prop: IGetProp<'T>) =
    let events = PropertyChangedEventHolder<'T>(ResizeArray())
    interface IProp<'T> with
        member _.Value = prop.Value
        member _.OnNext(value) = observer.OnNext(value)
        member _.OnCompleted() = observer.OnCompleted()
        member _.OnError(error) = observer.OnError(error)
        member _.Subscribe(onNext) = prop.Subscribe(onNext)
        member this.add_PropertyChanged(handler) = events.Add(handler, this)
        member _.remove_PropertyChanged(handler) = events.Remove(handler)
