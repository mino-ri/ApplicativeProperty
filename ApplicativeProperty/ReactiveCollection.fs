namespace ApplicativeProperty

open System
open System.Collections
open System.Collections.Generic
open System.Collections.Specialized
open System.Threading


type internal CollectionChangedEventHolder<'T>(eventDisposers: ResizeArray<struct (NotifyCollectionChangedEventHandler * IDisposable)>) =
    struct
        member _.Add
            (
                handler: NotifyCollectionChangedEventHandler,
                target: IObservable<CollectionChange<'T>>,
                context: SynchronizationContext
            ) =
            let disposer =
                target.Subscribe(
                    { new BasicObserver<_>() with
                        member _.OnNext(value) =
                            context.Post(
                                SendOrPostCallback(fun _ ->
                                    for e in CollectionChange.toEventArgs value do
                                        handler.Invoke(target, e)
                                ),
                                null
                            )
                        member _.OnCompleted() = ()
                        member _.OnError(error) = raise error
                    }
                )
            eventDisposers.Add(handler, disposer)

        member _.Remove(handler: NotifyCollectionChangedEventHandler) =
            let index = eventDisposers.FindIndex(0, (fun struct (eh, _) -> eh = handler))
            match eventDisposers.[index] with
            | (_, d) -> d.Dispose()
            eventDisposers.RemoveAt(index)
    end


type ReactiveCollection<'T> private (inner: ResizeArray<'T>, context: SynchronizationContext) =
    let observers = ResizeArray<ObserverHolder<CollectionChange<'T>>>()
    let events = CollectionChangedEventHolder<'T>(ResizeArray())
    let countProp = ValueProp(inner.Count)

    member val CountProp = Prop.asGet countProp

    member this.Count = this.CountProp.Value

    member this.Item
        with get index = inner.[index]
        and set index value = this.Replace(index, value)

    member private _.OnNext(collectionChange) =
        for holder in observers do
            holder.Observer.OnNext(collectionChange)
        countProp.OnNext(inner.Count)

    member _.Subscribe(onNext) =
        let holder = new ObserverHolder<_>(observers, onNext)
        observers.Add(holder)
        holder :> IDisposable

    member this.Add(item) =
        inner.Add(item)
        this.OnNext(CollectionChange.add (inner.Count - 1) [ item ])

    member this.AddRange(items: 'T list) =
        if items.Length > 0 then
            let index = inner.Count
            inner.AddRange(items)
            this.OnNext(CollectionChange.add index items)

    member this.RemoveAt(index: int) =
        let oldItem = inner.[index]
        inner.RemoveAt(index)
        this.OnNext(CollectionChange.remove index [ oldItem ])

    member this.RemoveRange(index: int, count: int) =
        let target = inner |> Seq.skip index |> Seq.take count |> Seq.toList
        inner.RemoveRange(index, count)
        this.OnNext(CollectionChange.remove index target)

    member this.RemoveTail(count: int) =
        let count = min count inner.Count
        if count > 0 then
            this.RemoveRange(inner.Count - count, count)

    member this.Replace(index: int, item: 'T) =
        let oldItem = inner.[index]
        inner.[index] <- item
        this.OnNext(CollectionChange.replace index oldItem item)

    member this.Reset(items: 'T list) =
        let oldItems = inner |> Seq.toList
        inner.Clear()
        inner.AddRange(items)
        this.OnNext(CollectionChange.reset oldItems items)

    member _.GetEnumerator() = inner.GetEnumerator()

    new() = ReactiveCollection(ResizeArray<'T>(), SynchronizationContext.Current)
    new(context: SynchronizationContext) = ReactiveCollection(ResizeArray<'T>(), context)
    new(capacity: int) = ReactiveCollection(ResizeArray(capacity), SynchronizationContext.Current)
    new(source: seq<'T>) = ReactiveCollection(ResizeArray(source), SynchronizationContext.Current)
    new(source: seq<'T>, context: SynchronizationContext) = ReactiveCollection(ResizeArray(source), context)

    interface IReactiveCollection<'T> with
        member _.GetEnumerator() : IEnumerator = (inner :> IEnumerable).GetEnumerator()
        member _.GetEnumerator() : IEnumerator<'T> = (inner :> IEnumerable<'T>).GetEnumerator()
        member _.Count = inner.Count
        member this.CountProp = this.CountProp
        member _.Item
            with get (index) = inner.[index]
        member this.Subscribe(observer) = this.Subscribe(observer)
        member this.add_CollectionChanged(handler) = events.Add(handler, this, context)
        member _.remove_CollectionChanged(handler) = events.Remove(handler)
