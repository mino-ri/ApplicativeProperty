namespace ApplicativeProperty
open System
open System.Collections
open System.Collections.Generic

type ActionDisposable(onDispose: unit -> unit) =
    let mutable disposed = false
    interface IDisposable with
        member __.Dispose() =
            if not disposed then
                disposed <- true
                onDispose()


type CompositeDisposable(source: seq<IDisposable>) =
    let storage = ResizeArray(source)
    let mutable disposed = false

    member __.Add(item: IDisposable) =
        if disposed then item.Dispose()
        else storage.Add(item)
    member __.AddRange(items: seq<IDisposable>) =
        if disposed then for item in items do item.Dispose()
        else storage.AddRange(items)
    member __.Clear() = storage.Clear()
    member __.Contains(item) = storage.Contains(item)
    member __.CopyTo(array, arrayIndex) = storage.CopyTo(array, arrayIndex)
    member __.Count = storage.Count
    member __.Remove(item) = storage.Remove(item)
    member __.GetEnumerator() = storage.GetEnumerator()
    member __.Dispose() =
        if not disposed then
            disposed <- true
            for item in storage do item.Dispose()
            storage.Clear()
    new() = new CompositeDisposable(Array.Empty())

    interface IEnumerable with
        member __.GetEnumerator() = (storage :> IEnumerable).GetEnumerator()

    interface IEnumerable<IDisposable> with
        member __.GetEnumerator() = (storage :> IEnumerable<IDisposable>).GetEnumerator()

    interface IReadOnlyCollection<IDisposable> with
        member __.Count = storage.Count

    interface ICollection with
        member __.Count = storage.Count
        member __.IsSynchronized = (storage :> ICollection).IsSynchronized
        member __.SyncRoot = (storage :> ICollection).SyncRoot
        member __.CopyTo(array, index) = (storage :> ICollection).CopyTo(array, index)

    interface ICollection<IDisposable> with
        member __.Count = storage.Count
        member __.IsReadOnly = (storage :> ICollection<IDisposable>).IsReadOnly
        member this.Add(item) = this.Add(item)
        member this.Clear() = this.Clear()
        member this.Contains(item) = this.Contains(item)
        member this.CopyTo(array, arrayIndex) = this.CopyTo(array, arrayIndex)
        member this.Remove(item) = this.Remove(item)

    interface IDisposable with
        member this.Dispose() = this.Dispose()


module Disposable =
    let dummy = { new IDisposable with member __.Dispose() = () }

    let create onDispose = new ActionDisposable(onDispose) :> IDisposable

    let collect disposables = new CompositeDisposable(disposables) :> IDisposable
