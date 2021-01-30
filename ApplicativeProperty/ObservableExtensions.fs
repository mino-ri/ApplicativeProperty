namespace ApplicativeProperty
open System
open System.Runtime.CompilerServices

[<Sealed; AbstractClass; Extension>]
type ObservableExtensions() =

    [<Extension>]
    static member Subscribe(source, callBack: Action<'T>) = Observable.subscribe2 (action1 callBack) source

    [<Extension>]
    static member Select(source, selector: Func<'T, 'U>) = Observable.map selector.Invoke source

    [<Extension>]
    static member Where(source, predicate: Func<'T, bool>) = Observable.filter predicate.Invoke source

    [<Extension>]
    static member SelectMany(source, selector: Func<'T, IObservable<'U>>) = Observable.bind selector.Invoke source
