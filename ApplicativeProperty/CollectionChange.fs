namespace ApplicativeProperty

open System.Collections.Generic
open System.Collections.Specialized


[<RequireQualifiedAccess>]
type CollectionChange<'Item> =
    | Add of index: int * items: IReadOnlyList<'Item>
    | Remove of index: int * items: IReadOnlyList<'Item>
    | Replace of index: int * oldItem: 'Item * newItem: 'Item
    | Reset of oldItems: IReadOnlyList<'Item> * newItems: IReadOnlyList<'Item>
    member this.Action =
        match this with
        | Add _ -> NotifyCollectionChangedAction.Add
        | Remove _ -> NotifyCollectionChangedAction.Remove
        | Replace _ -> NotifyCollectionChangedAction.Replace
        | Reset _ -> NotifyCollectionChangedAction.Reset

    member this.Index =
        match this with
        | Add(index, _)
        | Remove(index, _)
        | Replace(index, _, _) -> index
        | Reset(_) -> 0

    member this.OldItems =
        match this with
        | Reset(items, _) -> items
        | Add(_, _) -> upcast []
        | Remove(_, items) -> items
        | Replace(_, oldItem, _) -> upcast [ oldItem ]

    member this.NewItems =
        match this with
        | Reset(_, items) -> items
        | Add(_, items) -> items
        | Remove(_, _) -> upcast []
        | Replace(_, _, item) -> upcast [ item ]


module CollectionChange =
    [<CompiledName("Add")>]
    let add index (items: IReadOnlyList<'Item>) = CollectionChange.Add(index, items)

    [<CompiledName("Remove")>]
    let remove index (items: IReadOnlyList<'Item>) = CollectionChange.Remove(index, items)

    [<CompiledName("Replace")>]
    let replace index (oldItem: 'Item) newItem = CollectionChange.Replace(index, oldItem, newItem)

    [<CompiledName("Reset")>]
    let reset (oldItems: IReadOnlyList<'Item>) newItems = CollectionChange.Reset(oldItems, newItems)

    [<CompiledName("ToEventArgs")>]
    let toEventArgs<'Item> collectionChange =
        match collectionChange with
        | CollectionChange.Replace(index, oldItem: 'Item, newItem) ->
            Seq.singleton (NotifyCollectionChangedEventArgs(NotifyCollectionChangedAction.Replace, oldItem, newItem, index))
        | CollectionChange.Reset _ -> Seq.singleton (NotifyCollectionChangedEventArgs(NotifyCollectionChangedAction.Reset))
        | CollectionChange.Add(index, items) ->
            // WPF は複数要素の一括Addに非対応なので、1つずつ送る
            items
            |> Seq.indexed
            |> Seq.map (fun (ix, item) -> NotifyCollectionChangedEventArgs(NotifyCollectionChangedAction.Add, item, index + ix))
        | CollectionChange.Remove(index, items) ->
            // WPF は複数要素の一括Removeに非対応なので、1つずつ送る
            items
            |> Seq.rev
            |> Seq.indexed
            |> Seq.map (fun (ix, item) ->
                NotifyCollectionChangedEventArgs(NotifyCollectionChangedAction.Remove, item, index + items.Count - 1 - ix)
            )

    let map mapping collectionChange =
        match collectionChange with
        | CollectionChange.Add(index, items) -> add index (MappingList(mapping, items))
        | CollectionChange.Remove(index, items) -> remove index (MappingList(mapping, items))
        | CollectionChange.Replace(index, oldItem, newItem) -> replace index (mapping oldItem) (mapping newItem)
        | CollectionChange.Reset(oldItems, newItems) -> reset (MappingList(mapping, oldItems)) (MappingList(mapping, newItems))
