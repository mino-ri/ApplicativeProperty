[<RequireQualifiedAccess; System.Runtime.CompilerServices.Extension>]
module ApplicativeProperty.Observable
open System
open System.Runtime.CompilerServices
open System.Threading

[<CompiledName("Subscribe2")>]
let subscribe2 callback (source: IObservable<'T>) =
    source.Subscribe({ new BasicObserver<'T>() with
        member _.OnNext(value) = callback value
        member _.OnCompleted() = ()
        member _.OnError(error) = raise error })

[<CompiledName("ToProp")>]
let toProp init source =
    let prop = new ObserverProp<'T>(init)
    prop.Observe(source)
    prop
    
[<CompiledName("AsObservable"); Extension>]
let asObservable (source: IObservable<'T>) =
    { new IObservable<'T> with
        member _.Subscribe(observer) = source.Subscribe(observer) }

[<CompiledName("Bind")>]
let bind (mapping: 'T -> IObservable<'U>) (source: IObservable<'T>) =
    { new IObservable<'U> with
        member _.Subscribe(observer) =
            let disposer = new CompositeDisposable()
            let mutable count = 1
            disposer.Add(source.Subscribe({ new BasicObserver<'T>() with
                member _.OnCompleted() =
                    if Interlocked.Decrement(&count) <= 0 then
                        observer.OnCompleted()
                member _.OnError(error) = observer.OnError(error)
                member _.OnNext(value) =
                    ignore (Interlocked.Increment(&count))
                    disposer.Add((mapping value).Subscribe({ new BasicObserver<'U>() with
                        member _.OnNext(value) = observer.OnNext(value)
                        member _.OnError(error) = observer.OnError(error)
                        member _.OnCompleted() =
                            if Interlocked.Decrement(&count) <= 0 then
                                observer.OnCompleted()
                    }))
            }))
            disposer :> IDisposable }
