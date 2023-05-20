[<RequireQualifiedAccess>]
module ApplicativeProperty.ReactiveCollection

open System

let mapItem (mapping: 'T -> 'U) (source: IObservable<CollectionChange<'T>>) = source |> Observable.map (CollectionChange.map mapping)
