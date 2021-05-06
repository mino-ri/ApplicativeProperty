namespace ApplicativeProperty
open System
open System.Collections.Generic
open System.ComponentModel
open System.Threading


type internal PropertyChangedEventHolder<'T>(eventDisposers: ResizeArray<struct(PropertyChangedEventHandler * IDisposable)>) =
    struct
        member _.Add(handler: PropertyChangedEventHandler, target : IObservable<'T>, context: SynchronizationContext) =
            if isNull context then nullArg (nameof context)
            let disposer = target.Subscribe(fun _ ->
                context.Post(SendOrPostCallback(fun _ ->
                    handler.Invoke(target, PropertyChangedEventArgs("Value"))), null))
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


type ValueProp<'T>(init: 'T, comparer: IEqualityComparer<'T>, context: SynchronizationContext) =
    let mutable backField = init
    let observers = ResizeArray<ObserverHolder<'T>>()
    let events = PropertyChangedEventHolder<'T>(ResizeArray())

    member this.Value with get() = backField and set(v) = this.OnNext(v)
    
    member _.Subscribe(onNext) =
        let holder = new ObserverHolder<'T>(observers, onNext)
        observers.Add(holder)
        holder :> IDisposable

    member _.OnNext(value) =
        if not (comparer.Equals(backField, value)) then
            backField <- value
            for holder in observers do holder.Observer.OnNext(value)

    member _.OnCompleted() = for holder in observers do holder.Observer.OnCompleted()

    member _.OnError(error) = for holder in observers do holder.Observer.OnError(error)

    new(init: 'T) = ValueProp(init, EqualityComparer.Default, SynchronizationContext.Current)
    new(init: 'T, context: SynchronizationContext) = ValueProp(init, EqualityComparer.Default, context)
    new(init: 'T, compare: 'T -> 'T -> bool) = ValueProp(init, { new IEqualityComparer<'T> with
            member _.Equals(x, y) = compare x y
            member _.GetHashCode(_) = 0 // not used
        }, SynchronizationContext.Current)

    interface IProp<'T> with
        member this.Value = this.Value
        member this.OnNext(value) = this.OnNext(value)
        member this.OnCompleted() = this.OnCompleted()
        member this.OnError(error) = this.OnError(error)
        member this.Subscribe(onNext) = this.Subscribe(onNext)
        member this.add_PropertyChanged(handler) = events.Add(handler, this, context)
        member _.remove_PropertyChanged(handler) = events.Remove(handler)


type ConstProp<'T>(init: 'T) =
    member _.Value = init

    member _.Subscribe(observer: IObserver<'T>) = Disposable.dummy

    interface IGetProp<'T> with
        member _.Value = init
        member _.Subscribe(_) = Disposable.dummy
        member _.add_PropertyChanged(_) = ()
        member _.remove_PropertyChanged(_) = ()


type ObserverProp<'T>(init: 'T, comparer: IEqualityComparer<'T>, context: SynchronizationContext) =
    let mutable backField = init
    let observers = ResizeArray<ObserverHolder<'T>>()
    let events = PropertyChangedEventHolder<'T>(ResizeArray())
    let disposables = new CompositeDisposable()
    let selfObserver =
        { new IObserver<'T> with
            member _.OnNext(value) =
                if not (comparer.Equals(backField, value)) then
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

    member _.Dispose() = disposables.Dispose()

    new(init: 'T) = new ObserverProp<'T>(init, EqualityComparer.Default, SynchronizationContext.Current)
    new(init: 'T, context: SynchronizationContext) = new ObserverProp<'T>(init, EqualityComparer.Default, context)
    new(init: 'T, compare: 'T -> 'T -> bool) = new ObserverProp<'T>(init, { new IEqualityComparer<'T> with
            member _.Equals(x, y) = compare x y
            member _.GetHashCode(_) = 0 // not used
        }, SynchronizationContext.Current)

    interface IGetProp<'T> with
        member this.Value = this.Value
        member this.Subscribe(onNext) = this.Subscribe(onNext)
        member this.add_PropertyChanged(handler) = events.Add(handler, this, context)
        member _.remove_PropertyChanged(handler) = events.Remove(handler)

    interface IDisposable with
        member this.Dispose() = this.Dispose()


type DelegationProp<'T>(observer: IObserver<'T>, prop: IGetProp<'T>, context: SynchronizationContext) =
    let events = PropertyChangedEventHolder<'T>(ResizeArray())
    member _.Value with get() = prop.Value and set v = observer.OnNext(v)
    interface IProp<'T> with
        member _.Value = prop.Value
        member _.OnNext(value) = observer.OnNext(value)
        member _.OnCompleted() = observer.OnCompleted()
        member _.OnError(error) = observer.OnError(error)
        member _.Subscribe(onNext) = prop.Subscribe(onNext)
        member this.add_PropertyChanged(handler) = events.Add(handler, this, context)
        member _.remove_PropertyChanged(handler) = events.Remove(handler)
    new(observer, prop) = DelegationProp<'T>(observer, prop, SynchronizationContext.Current)
