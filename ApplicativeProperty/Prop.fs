[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ApplicativeProperty.Prop
open System
open System.Windows.Input

[<Sealed>]
type ObservableProp<'T>(init: 'T, observable: IObservable<'T>, subscriver: ('T -> unit) -> IObserver<'T>) =
    let mutable backField = init
    let disconnector = observable.Subscribe(fun value -> backField <- value)
    interface IDisposableGetProp<'T> with
        member _.Value = backField
        member _.Subscribe(onNext) = observable.Subscribe(subscriver onNext)
        member _.Dispose() = disconnector.Dispose()


[<CompiledName("Constant")>]
let constant value = ConstProp(value)

[<CompiledName("OfObservable")>]
let ofObservable init (source: IObservable<'T>) =
    new ObservableProp<'T>(init, source, fun onNext -> 
        { new IObserver<'T> with
            member _.OnNext(value) = onNext(value)
            member _.OnCompleted() = ()
            member _.OnError(_) = ()
        })
    :> IDisposableGetProp<'T>

[<CompiledName("OfObservableWithError")>]
let ofObservableWithExn init (source: IObservable<'T>) =
    new ObservableProp<'T>(init, source, fun onNext -> 
        { new IObserver<'T> with
            member _.OnNext(value) = onNext(value)
            member _.OnCompleted() = ()
            member _.OnError(error) = raise error
        })
    :> IDisposableGetProp<'T>

[<CompiledName("TofObservable")>]
let toObservable (prop: IGetProp<'T>) =
    { new IObservable<'T> with
        member _.Subscribe(observer) = prop.Subscribe(observer.OnNext) }

[<CompiledName("Subscribe")>]
let add onNext (prop: IGetProp<'T>) = ignore (prop.Subscribe(onNext))

[<CompiledName("Subscribe")>]
let subscribe onNext (prop: IGetProp<'T>) = prop.Subscribe(onNext)

[<CompiledName("AsGet")>]
let asGet (prop: IGetProp<'T>) =
    { new IGetProp<'T> with
        member _.Value = prop.Value
        member _.Subscribe(onNext) = prop.Subscribe(onNext) }

[<CompiledName("Map")>]
let map mapping (prop: IGetProp<'T>) =
    { new IGetProp<'U> with
        member _.Value = mapping prop.Value
        member _.Subscribe(onNext) = prop.Subscribe(mapping >> onNext) }

[<CompiledName("Map")>]
let map2 mapping (prop1: IGetProp<'T1>) (prop2: IGetProp<'T2>) =
    { new IGetProp<'U> with
        member _.Value = mapping prop1.Value prop2.Value
        member _.Subscribe(onNext) =
            Disposable.collect [|
                prop1.Subscribe(fun value -> onNext (mapping value prop2.Value))
                prop2.Subscribe(fun value -> onNext (mapping prop1.Value value))
            |]
    }

[<CompiledName("Map")>]
let map3 mapping (prop1: IGetProp<'T1>) (prop2: IGetProp<'T2>) (prop3: IGetProp<'T3>) =
    { new IGetProp<'U> with
        member _.Value = mapping prop1.Value prop2.Value prop3.Value
        member _.Subscribe(onNext) =
            Disposable.collect [|
                prop1.Subscribe(fun value -> onNext (mapping value prop2.Value prop3.Value))
                prop2.Subscribe(fun value -> onNext (mapping prop1.Value value prop3.Value))
                prop3.Subscribe(fun value -> onNext (mapping prop1.Value prop2.Value value))
            |]
    }

[<CompiledName("Bind")>]
let bind (mapping: 'T -> IGetProp<'U>) (prop: IGetProp<'T>) =
    { new IGetProp<'U> with
        member _.Value = (mapping prop.Value).Value
        member _.Subscribe(onNext) =
            let disposer = new CompositeDisposable()
            disposer.Add(prop.Subscribe(fun value -> disposer.Add((mapping value).Subscribe(onNext))))
            disposer :> IDisposable
    }

[<CompiledName("Cache")>]
let cache (prop: IGetProp<'T>) =
    let p = ValueProp(prop.Value)
    ignore (prop.Subscribe(p.OnNext))
    p :> IGetProp<'T>

[<CompiledName("Filter")>]
let filter pred (prop: IGetProp<'T>) =
    { new IGetProp<'T> with
        member _.Value = prop.Value
        member _.Subscribe(onNext) =
            prop.Subscribe(fun value -> if pred value then onNext value)
    }

[<CompiledName("FilterCached")>]
let filterc pred init (prop: IGetProp<'T>) =
    let p = ValueProp(if pred prop.Value then prop.Value else init)
    ignore (prop.Subscribe(fun v -> if pred v then p.OnNext(v)))
    p :> IGetProp<'T>

[<CompiledName("Zip")>]
let zip (prop1: IGetProp<'T1>) (prop2: IGetProp<'T2>) = map2 tpl2 prop1 prop2

[<CompiledName("Zip")>]
let zip3 (prop1: IGetProp<'T1>) (prop2: IGetProp<'T2>) (prop3: IGetProp<'T3>) =
    map3 tpl3 prop1 prop2 prop3

[<CompiledName("Unzip")>]
let unzip (prop: IGetProp<'T1 * 'T2>) =
    let cached = cache prop
    map fst cached, map snd cached

[<CompiledName("Unzip")>]
let unzip3 (prop: IGetProp<'T1 * 'T2 * 'T3>) =
    let cached = cache prop
    map fst3 cached, map snd3 cached, map thd3 cached

[<CompiledName("Apply")>]
let apply (funcProp: IGetProp<'T -> 'U>) (argProp: IGetProp<'T>) =
    map2 (<|) funcProp argProp

type GetPropBuilder() =
    member __.MergeSources(t1, t2) : IGetProp<'T1 * 'T2> = zip t1 t2
    member __.BindReturn(m, f: 'T -> 'U) = map f m
    member __.Bind(m, f: 'T -> IGetProp<'U>) = bind f m
    member __.Return(x: 'T) = constant x
    member __.ReturnFrom(m: IGetProp<'T>) = m

[<CompiledName("CreateGetPropBuilder")>]
let prop = GetPropBuilder()

// == set way ======================================================================

[<CompiledName("OnSet")>]
let onSet f = { new ISetProp<'T> with member _.OnNext(value) = f value }

[<CompiledName("OnNext")>]
let onNext value (prop: ISetProp<'T>) = prop.OnNext(value)

[<CompiledName("AsSet")>]
let asSet (prop: ISetProp<'T>) =
    { new ISetProp<'T> with member _.OnNext(value) = prop.OnNext(value) }

[<CompiledName("MapSet")>]
let mapSet mapping (prop: ISetProp<'T>) =
    { new ISetProp<'U> with
        member _.OnNext(value) = prop.OnNext(mapping value) }

[<CompiledName("MapSet")>]
let map2Set mapping (prop1: ISetProp<'T1>) (prop2: ISetProp<'T2>) =
    { new ISetProp<'U> with
        member _.OnNext(value) =
            let v1, v2 = mapping value
            prop1.OnNext(v1)
            prop2.OnNext(v2)
    }

[<CompiledName("MapSet")>]
let map3Set mapping (prop1: ISetProp<'T1>) (prop2: ISetProp<'T2>) (prop3: ISetProp<'T3>) =
    { new ISetProp<'U> with
        member _.OnNext(value) =
            let v1, v2, v3 = mapping value
            prop1.OnNext(v1)
            prop2.OnNext(v2)
            prop3.OnNext(v3)
    }

[<CompiledName("FilterSet")>]
let filterSet pred (prop: ISetProp<'T>) =
    { new ISetProp<'T> with
        member _.OnNext(value) = if pred value then prop.OnNext(value) }

// == both way ======================================================================

[<CompiledName("Value")>]
let value init = ValueProp<'T>(init)

[<CompiledName("WithSet")>]
let withSet (setter: ISetProp<'T>) (getter: IGetProp<'T>) =
    { new IProp<'T> with
        member _.Value = getter.Value
        member _.Subscribe(onNext) = getter.Subscribe(onNext)
        member _.OnNext(value) = setter.OnNext(value) }

[<CompiledName("WithGet")>]
let withGet (getter: IGetProp<'T>) (setter: ISetProp<'T>) =
    { new IProp<'T> with
        member _.Value = getter.Value
        member _.Subscribe(onNext) = getter.Subscribe(onNext)
        member _.OnNext(value) = setter.OnNext(value) }

[<CompiledName("Modify")>]
let modify f (prop: IProp<'T>) = prop.OnNext(f prop.Value)

[<CompiledName("MapBoth")>]
let mapBoth getMapping setMapping (prop: IProp<'T>) =
    { new IProp<'U> with
        member _.Value = getMapping prop.Value
        member _.Subscribe(onNext) = prop.Subscribe(getMapping >> onNext)
        member _.OnNext(value) = prop.OnNext(setMapping value) }

[<CompiledName("MapBoth")>]
let map2Both getMapping setMapping (prop1: IProp<'T1>) (prop2: IProp<'T2>) =
    { new IProp<'U> with
        member _.Value = getMapping prop1.Value prop2.Value
        member _.Subscribe(onNext) =
            Disposable.collect [|
                prop1.Subscribe(fun value -> onNext (getMapping value prop2.Value))
                prop2.Subscribe(fun value -> onNext (getMapping prop1.Value value))
            |]
        member _.OnNext(value) =
            let v1, v2 = setMapping value
            prop1.OnNext(v1)
            prop2.OnNext(v2)
    }

[<CompiledName("MapBoth")>]
let map3Both getMapping setMapping (prop1: IProp<'T1>) (prop2: IProp<'T2>) (prop3: IProp<'T3>) =
    { new IProp<'U> with
        member _.Value = getMapping prop1.Value prop2.Value prop3.Value
        member _.Subscribe(onNext) =
            Disposable.collect [|
                prop1.Subscribe(fun value -> onNext (getMapping value prop2.Value prop3.Value))
                prop2.Subscribe(fun value -> onNext (getMapping prop1.Value value prop3.Value))
                prop3.Subscribe(fun value -> onNext (getMapping prop1.Value prop2.Value value))
            |]
        member _.OnNext(value) =
            let v1, v2, v3 = setMapping value
            prop1.OnNext(v1)
            prop2.OnNext(v2)
            prop3.OnNext(v3)
    }

[<CompiledName("CacheBoth")>]
let cacheBoth (prop: IProp<'T>) =
    let p = ValueProp(prop.Value)
    Operators.ignore (prop.Subscribe(p.OnNext))
    Operators.ignore (p.Subscribe(prop.OnNext))
    p :> IProp<'T>

[<CompiledName("ZipBoth")>]
let zipBoth (prop1: IProp<'T1>) (prop2: IProp<'T2>) =
    map2Both tpl2 id prop1 prop2

[<CompiledName("ZipBoth")>]
let zip3Both (prop1: IProp<'T1>) (prop2: IProp<'T2>) (prop3: IProp<'T3>) =
    map3Both tpl3 id prop1 prop2 prop3

[<CompiledName("UnzipBoth")>]
let unzipBoth (prop: IProp<'T1 * 'T2>) =
    let mutable r2 = Unchecked.defaultof<IProp<'T2>>
    let r1 =
       { new IProp<'T1> with
           member _.Value = fst prop.Value
           member _.Subscribe(onNext) = prop.Subscribe(fst >> onNext)
           member _.OnNext(value) = prop.OnNext(value, r2.Value) }
    r2 <-
        { new IProp<'T2> with
            member _.Value = snd prop.Value
            member _.Subscribe(onNext) = prop.Subscribe(snd >> onNext)
            member _.OnNext(value) = prop.OnNext(r1.Value, value) }
    r1, r2

[<CompiledName("UnzipBoth")>]
let unzip3Both (prop: IProp<'T1 * 'T2 * 'T3>) =
    let mutable r2 = Unchecked.defaultof<IProp<'T2>>
    let mutable r3 = Unchecked.defaultof<IProp<'T3>>
    let r1 =
       { new IProp<'T1> with
           member _.Value = fst3 prop.Value
           member _.Subscribe(onNext) = prop.Subscribe(fst3 >> onNext)
           member _.OnNext(value) = prop.OnNext(value, r2.Value, r3.Value) }
    r2 <-
        { new IProp<'T2> with
            member _.Value = snd3 prop.Value
            member _.Subscribe(onNext) = prop.Subscribe(snd3 >> onNext)
            member _.OnNext(value) = prop.OnNext(r1.Value, value, r3.Value) }
    r3 <-
        { new IProp<'T3> with
            member _.Value = thd3 prop.Value
            member _.Subscribe(onNext) = prop.Subscribe(thd3 >> onNext)
            member _.OnNext(value) = prop.OnNext(r1.Value, r2.Value, value) }
    r1, r2, r3

// == notifications ======================================================================

[<CompiledName("NotifyValue")>]
let valuen init = NotifyProp(ValueProp<'T>(init))

[<CompiledName("ToReadOnlyNotify")>]
let notifyGet (prop: IGetProp<'T>) = NotifyGetProp(prop)

[<CompiledName("ToNotify")>]
let notify (prop: IProp<'T>) = NotifyProp(prop)

[<CompiledName("NotifyWithGet")>]
let notifyWithGet (getter: IGetProp<'T>) (setter: ISetProp<'T>) = NotifyProp(getter, setter)

[<CompiledName("NotifyWithSet")>]
let notifyWithSet (setter: ISetProp<'T>) (getter: IGetProp<'T>) = NotifyProp(getter, setter)

[<CompiledName("ToCommand")>]
let command onExecute canExecute = Command(onExecute, canExecute) :> ICommand

[<CompiledName("ToCommand")>]
let commandp canExecute =
    let valueProp = ValueProp(null)
    Command(valueProp.OnNext, canExecute) :> ICommand, valueProp :> IGetProp<obj>
    
// == pure operator mapping ======================================================================
    
[<CompiledName("Increment")>]
let incr (prop: IProp<int>) = prop.OnNext(prop.Value + 1)

[<CompiledName("Decrement")>]
let decr (prop: IProp<int>) = prop.OnNext(prop.Value - 1)

[<CompiledName("Not")>]
let not prop = map not prop

[<CompiledName("NotSet")>]
let notSet prop = mapSet Operators.not prop

[<CompiledName("NotBoth")>]
let notBoth prop = mapBoth Operators.not Operators.not prop

[<CompiledName("Ignore")>]
let ignore (prop1: IGetProp<'T>) = map ignore prop1

[<CompiledName("True")>]
let ctrue = constant true

[<CompiledName("False")>]
let cfalse = constant false

[<CompiledName("Unit")>]
let cunit = constant ()
