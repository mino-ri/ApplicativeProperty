[<RequireQualifiedAccess; System.Runtime.CompilerServices.Extension>]
module ApplicativeProperty.Prop
open System
open System.Runtime.CompilerServices
open System.Threading
open System.Windows.Input


[<AbstractClass>]
type BasicGetProp<'T>(context: SynchronizationContext) =
    let events = PropertyChangedEventHolder<'T>(ResizeArray())
    abstract member Value : 'T
    abstract member Subscribe : observer: IObserver<'T> -> IDisposable
    interface IGetProp<'T> with
        member this.Value = this.Value
        member this.Subscribe(onNext) = this.Subscribe(onNext)
        member this.add_PropertyChanged(handler) = events.Add(handler, this, context)
        member _.remove_PropertyChanged(handler) = events.Remove(handler)
    new() = BasicGetProp<'T>(SynchronizationContext.Current)


[<AbstractClass>]
type BasicProp<'T>(context: SynchronizationContext) =
    let events = PropertyChangedEventHolder<'T>(ResizeArray())
    abstract member GetValue : unit -> 'T
    abstract member Subscribe : observer: IObserver<'T> -> IDisposable
    abstract member OnNext : 'T -> unit
    abstract member OnCompleted : unit -> unit
    abstract member OnError : exn -> unit
    member this.Value with get() = this.GetValue() and set v = this.OnNext(v)
    interface IProp<'T> with
        member this.Value = this.Value
        member this.Subscribe(onNext) = this.Subscribe(onNext)
        member this.OnNext(value) = this.OnNext(value)
        member this.OnError(error) = this.OnError(error)
        member this.OnCompleted() = this.OnCompleted()
        member this.add_PropertyChanged(handler) = events.Add(handler, this, context)
        member _.remove_PropertyChanged(handler) = events.Remove(handler)
    new() = BasicProp<'T>(SynchronizationContext.Current)


[<CompiledName("Constant")>]
let constant (value: 'T) = ConstProp(value)

[<CompiledName("ToProp"); Extension>]
let ofObservable init source =
    let prop = new ObserverProp<'T>(init)
    prop.Observe(source)
    prop

[<CompiledName("AsGet"); Extension>]
let asGet (prop: IGetProp<'T>) =
    { new BasicGetProp<'T>() with
        member _.Value = prop.Value
        member _.Subscribe(observer) = prop.Subscribe(observer)
    } :> IGetProp<_>

[<CompiledName("Fetch")>]
let fetch context (prop: IGetProp<'T>) =
    { new BasicGetProp<'T>(context) with
        member _.Value = prop.Value
        member _.Subscribe(observer) = prop.Subscribe(observer)
    } :> IGetProp<_>

[<CompiledName("Fetch"); Extension>]
let fetchCurrent (prop: IGetProp<'T>) = fetch SynchronizationContext.Current prop

[<CompiledName("Map")>]
let map mapping (prop: IGetProp<'T>) =
    { new BasicGetProp<'U>() with
        member _.Value = mapping prop.Value
        member _.Subscribe(observer) = (observer, prop) ||> mapSubsc mapping
    } :> IGetProp<_>

[<CompiledName("Map")>]
let map2 mapping (prop1: IGetProp<'T1>) (prop2: IGetProp<'T2>) =
    { new BasicGetProp<'U>() with
        member _.Value = mapping prop1.Value prop2.Value
        member _.Subscribe(observer) =
            Disposable.collect [|
                (observer, prop1) ||> mapSubsc(fun value -> mapping value prop2.Value)
                (observer, prop2) ||> mapSubsc(fun value -> mapping prop1.Value value)
            |]
    } :> IGetProp<_>

[<CompiledName("Map")>]
let map3 mapping (prop1: IGetProp<'T1>) (prop2: IGetProp<'T2>) (prop3: IGetProp<'T3>) =
    { new BasicGetProp<'U>() with
        member _.Value = mapping prop1.Value prop2.Value prop3.Value
        member _.Subscribe(observer) =
            Disposable.collect [|
                (observer, prop1) ||> mapSubsc(fun value -> mapping value prop2.Value prop3.Value)
                (observer, prop2) ||> mapSubsc(fun value -> mapping prop1.Value value prop3.Value)
                (observer, prop3) ||> mapSubsc(fun value -> mapping prop1.Value prop2.Value value)
            |]
    } :> IGetProp<_>

[<CompiledName("Cache"); Extension>]
let cache (prop: IGetProp<'T>) = ofObservable prop.Value prop

[<CompiledName("ScanCached")>]
let scanc collector state (prop: IGetProp<'T>) = Observable.scan collector state prop |> ofObservable state

[<CompiledName("ChooseCached")>]
let choosec chooser (init: 'U) (prop: IGetProp<'T>) = Observable.choose chooser prop |> ofObservable init

[<CompiledName("FilterCached")>]
let filterc pred init (prop: IGetProp<'T>) = Observable.filter pred prop |> ofObservable init

[<CompiledName("MergeCached")>]
let mergec (other: IObservable<'T>) (prop: IGetProp<'T>) = Observable.merge prop other |> ofObservable prop.Value

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
let apply (funcProp: IGetProp<'T -> 'U>) argProp =
    map2 (<|) funcProp argProp

[<CompiledName("Apply")>]
let apply2 (funcProp: IGetProp<'T1 -> 'T2 -> 'U>) argProp1 argProp2 =
    map3 (fun f a b -> f a b) funcProp argProp1 argProp2

type GetPropBuilder() =
    member __.MergeSources(t1, t2) : IGetProp<'T1 * 'T2> = zip t1 t2
    member __.BindReturn(m, f: 'T -> 'U) = map f m
    member __.Return(x: 'T) = constant x
    member __.ReturnFrom(m: IGetProp<'T>) = m

[<CompiledName("CreateGetPropBuilder")>]
let prop = GetPropBuilder()

// == both way ======================================================================

[<CompiledName("Value")>]
let value init = ValueProp<'T>(init)

[<CompiledName("WithSet")>]
let withSet (setter: IObserver<'T>) (getter: IGetProp<'T>) = DelegationProp(setter, getter) :> IProp<_>

[<CompiledName("WithGet")>]
let withGet (getter: IGetProp<'T>) (setter: IObserver<'T>) = withSet setter getter

[<CompiledName("Modify")>]
let modify f (prop: IProp<'T>) = prop.OnNext(f prop.Value)

[<CompiledName("MapBoth")>]
let mapBoth getMapping setMapping (prop: IProp<'T>) =
    { new BasicProp<'U>() with
        member _.GetValue() = getMapping prop.Value
        member _.Subscribe(observer) = (observer, prop) ||> mapSubsc getMapping
        member _.OnNext(value) = prop.OnNext(setMapping value)
        member _.OnCompleted() = prop.OnCompleted()
        member _.OnError(error) = prop.OnError(error)
    } :> IProp<_>

[<CompiledName("MapBoth")>]
let map2Both getMapping setMapping (prop1: IProp<'T1>) (prop2: IProp<'T2>) =
    { new BasicProp<'U>() with
        member _.GetValue() = getMapping prop1.Value prop2.Value
        member _.Subscribe(observer) =
            Disposable.collect [|
                (observer, prop1) ||> mapSubsc(fun value -> getMapping value prop2.Value)
                (observer, prop2) ||> mapSubsc(fun value -> getMapping prop1.Value value)
            |]
        member _.OnNext(value) =
            let v1, v2 = setMapping value
            prop1.OnNext(v1)
            prop2.OnNext(v2)
        member _.OnCompleted() =
            prop1.OnCompleted()
            prop2.OnCompleted()
        member _.OnError(error) =
            prop1.OnError(error)
            prop2.OnError(error)
    } :> IProp<_>

[<CompiledName("MapBoth")>]
let map3Both getMapping setMapping (prop1: IProp<'T1>) (prop2: IProp<'T2>) (prop3: IProp<'T3>) =
    { new BasicProp<'U>() with
        member _.GetValue() = getMapping prop1.Value prop2.Value prop3.Value
        member _.Subscribe(observer) =
            Disposable.collect [|
                (observer, prop1) ||> mapSubsc(fun value -> getMapping value prop2.Value prop3.Value)
                (observer, prop2) ||> mapSubsc(fun value -> getMapping prop1.Value value prop3.Value)
                (observer, prop3) ||> mapSubsc(fun value -> getMapping prop1.Value prop2.Value value)
            |]
        member _.OnNext(value) =
            let v1, v2, v3 = setMapping value
            prop1.OnNext(v1)
            prop2.OnNext(v2)
            prop3.OnNext(v3)
        member _.OnCompleted() =
            prop1.OnCompleted()
            prop2.OnCompleted()
            prop3.OnCompleted()
        member _.OnError(error) =
            prop1.OnError(error)
            prop2.OnError(error)
            prop3.OnError(error)
    } :> IProp<_>

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
       { new BasicProp<'T1>() with
           member _.GetValue() = fst prop.Value
           member _.Subscribe(observer) = (observer, prop) ||> mapSubsc fst
           member _.OnNext(value) = prop.OnNext(value, r2.Value)
           member _.OnCompleted() = prop.OnCompleted()
           member _.OnError(error) = prop.OnError(error)
       } :> IProp<_>
    r2 <-
        { new BasicProp<'T2>() with
            member _.GetValue() = snd prop.Value
            member _.Subscribe(observer) = (observer, prop) ||> mapSubsc snd
            member _.OnNext(value) = prop.OnNext(r1.Value, value)
            member _.OnCompleted() = prop.OnCompleted()
            member _.OnError(error) = prop.OnError(error)
        } :> IProp<_>
    r1, r2

[<CompiledName("UnzipBoth")>]
let unzip3Both (prop: IProp<'T1 * 'T2 * 'T3>) =
    let mutable r2 = Unchecked.defaultof<IProp<'T2>>
    let mutable r3 = Unchecked.defaultof<IProp<'T3>>
    let r1 =
       { new BasicProp<'T1>() with
           member _.GetValue() = fst3 prop.Value
           member _.Subscribe(observer) = (observer, prop) ||> mapSubsc fst3
           member _.OnNext(value) = prop.OnNext(value, r2.Value, r3.Value)
           member _.OnCompleted() = prop.OnCompleted()
           member _.OnError(error) = prop.OnError(error)
       } :> IProp<_>
    r2 <-
        { new BasicProp<'T2>() with
            member _.GetValue() = snd3 prop.Value
            member _.Subscribe(observer) = (observer, prop) ||> mapSubsc snd3
            member _.OnNext(value) = prop.OnNext(r1.Value, value, r3.Value)
            member _.OnCompleted() = prop.OnCompleted()
            member _.OnError(error) = prop.OnError(error)
        } :> IProp<_>
    r3 <-
        { new BasicProp<'T3>() with
            member _.GetValue() = thd3 prop.Value
            member _.Subscribe(observer) = (observer, prop) ||> mapSubsc thd3
            member _.OnNext(value) = prop.OnNext(r1.Value, r2.Value, value)
            member _.OnCompleted() = prop.OnCompleted()
            member _.OnError(error) = prop.OnError(error)
        } :> IProp<_>
    r1, r2, r3
    
[<CompiledName("FetchBoth")>]
let fetchBoth context (prop: IProp<'T>) = DelegationProp(prop, prop, context) :> IProp<_>
    
[<CompiledName("FetchBoth"); Extension>]
let fetchCurrentBoth (prop: IProp<'T>) = fetch SynchronizationContext.Current prop
    
// == command ======================================================================

[<CompiledName("ToCommand")>]
let command onExecute canExecute = Command(onExecute, canExecute) :> ICommand

[<CompiledName("ToCommand")>]
let commandp canExecute =
    let subject = Subject()
    Command(subject.OnNext, canExecute) :> ICommand, Observable.asObservable subject
    
[<CompiledName("ToCommand")>]
let commands onExecute canExecute context = Command(onExecute, canExecute, context) :> ICommand

[<CompiledName("ToCommand")>]
let commandps canExecute context =
    let subject = Subject()
    Command(subject.OnNext, canExecute, context) :> ICommand, Observable.asObservable subject

// == pure operator mapping ======================================================================
    
[<CompiledName("Increment"); Extension>]
let incr (prop: IProp<int>) = prop.OnNext(prop.Value + 1)

[<CompiledName("Decrement"); Extension>]
let decr (prop: IProp<int>) = prop.OnNext(prop.Value - 1)

[<CompiledName("Not"); Extension>]
let not prop = map not prop

[<CompiledName("NotBoth"); Extension>]
let notBoth prop = mapBoth Operators.not Operators.not prop

[<CompiledName("Ignore")>]
let ignore (prop1: IGetProp<'T>) = map ignore prop1

[<CompiledName("True")>]
let ctrue = constant true

[<CompiledName("False")>]
let cfalse = constant false

[<CompiledName("Unit")>]
let cunit = constant ()
