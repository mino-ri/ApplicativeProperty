namespace ApplicativeProperty
open System
open System.Collections.Generic
open System.Collections.Specialized
open System.ComponentModel

type ISubject<'T> =
    inherit IObservable<'T>
    inherit IObserver<'T>

type IGetProp<'T> =
    inherit IObservable<'T>
    inherit INotifyPropertyChanged
    abstract member Value : 'T

type IProp<'T> =
    inherit IGetProp<'T>
    inherit ISubject<'T>

type IDisposableGetProp<'T> =
    inherit IGetProp<'T>
    inherit IDisposable

type IReactiveCollection<'T> =
    inherit IObservable<CollectionChange<'T>>
    inherit INotifyCollectionChanged
    inherit IReadOnlyList<'T>
