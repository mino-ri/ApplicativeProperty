namespace ApplicativeProperty
open System
open System.Runtime.CompilerServices
open System.Windows.Input

[<Sealed; AbstractClass; Extension; CompiledName("Prop")>]
type PropExtensions =
    [<Extension>]
    static member Subscribe(getProp: IGetProp<'T>, setProp: ISetProp<'T>) =
        getProp.Subscribe(setProp.OnNext)

    [<Extension>]
    static member Subscribe(getProp: IGetProp<'T>, onNext) =
        getProp.Subscribe(action1 onNext)

    static member Constant(value: 'T) = Prop.constant value

    [<Extension>]
    static member ToGetProp(source: IObservable<'T>, init) = Prop.ofObservable init source

    [<Extension>]
    static member ToGetPropWithError(source: IObservable<'T>, init) = Prop.ofObservableWithExn init source

    [<Extension>]
    static member ToObservable(prop: IGetProp<'T>) = Prop.toObservable prop

    [<Extension>]
    static member AsGet(prop: IGetProp<'T>) = Prop.asGet prop

    [<Extension>]
    static member Select(prop, selector: Func<'T, 'U>) = Prop.map selector.Invoke prop

    [<Extension>]
    static member Zip(prop1, prop2, selector: Func<'T1, 'T2, 'U>) = Prop.map2 (fun x y -> selector.Invoke(x, y)) prop1 prop2

    [<Extension>]
    static member Zip(prop1, prop2, prop3, selector: Func<'T1, 'T2, 'T3, 'U>) = Prop.map3 (fun x y z -> selector.Invoke(x, y, z)) prop1 prop2 prop3

    [<Extension>]
    static member SelectMany(prop, selector: Func<'T, IGetProp<'U>>) = Prop.bind selector.Invoke prop

    [<Extension>]
    static member Cache(prop: IGetProp<'T>) = Prop.cache prop

    [<Extension>]
    static member Where(prop, predicate: Func<'T, bool>) = Prop.filter predicate.Invoke prop

    [<Extension>]
    static member WhereCached(prop, predicate: Func<'T, bool>) = Prop.filterc predicate.Invoke prop

    [<Extension>]
    static member Unzip(prop: IGetProp<'T>, selector: Func<'T, struct('U1 * 'U2)>) =
        let cached = prop |> Prop.map selector.Invoke |> Prop.cache
        Prop.map vfst cached, Prop.map vsnd cached

    [<Extension>]
    static member Apply(funcProp: IGetProp<'T -> 'U>, argProp) = Prop.apply funcProp argProp

    static member OnSet(onNext: Action<'T>) = { new ISetProp<'T> with member _.OnNext(value) = onNext.Invoke(value) }

    [<Extension>]
    static member AsSet(prop: ISetProp<'T>) = Prop.asSet prop

    [<Extension>]
    static member SelectSet(prop, selector: Func<'U, 'T>) = Prop.mapSet selector.Invoke prop

    [<Extension>]
    static member ZipSet(prop1, prop2, selector: Func<'U, struct('T1 * 'T2)>) =
        Prop.map2Set (tuple2 selector) prop1 prop2

    [<Extension>]
    static member WhereSet(prop, predicate: Func<'T, bool>) = Prop.filterSet predicate.Invoke prop

    static member Value(value) = ValueProp<'T>(value)

    [<Extension>]
    static member WithSet(getter: IGetProp<'T>, setter) = Prop.withSet setter getter
    
    [<Extension>]
    static member WithGet(setter: ISetProp<'T>, getter) = Prop.withGet getter setter

    [<Extension>]
    static member Modify(prop, modification: Func<'T, 'T>) = Prop.modify modification.Invoke prop

    [<Extension>]
    static member SelectBoth(prop, getSelector: Func<'T, 'U>, setSelector: Func<'U, 'T>) =
        Prop.mapBoth getSelector.Invoke setSelector.Invoke prop

    [<Extension>]
    static member ZipBoth(prop1, prop2, getSelector:Func<'T1, 'T2, 'U>, setSelector) =
        Prop.map2Both (fun x y -> getSelector.Invoke(x, y)) (tuple2 setSelector) prop1 prop2

    [<Extension>]
    static member ToReadOnlyNotify(prop: IGetProp<'T>) = NotifyGetProp(prop)

    [<Extension>]
    static member ToNotify(prop: IProp<'T>) = NotifyProp(prop)

    static member NotifyValue(init: 'T) = NotifyProp(ValueProp(init))

    [<Extension>]
    static member ToNotifyWithGet(setter: ISetProp<'T>, getter: IGetProp<'T>) = NotifyProp(getter, setter)

    [<Extension>]
    static member ToNotifyWithSet(getter: IGetProp<'T>, setter: ISetProp<'T>) = NotifyProp(getter, setter)

    [<Extension>]
    static member ToCommand(canExecute, onExecute: Action<obj>) = Command(onExecute.Invoke, canExecute) :> ICommand

    [<Extension>]
    static member ToCommand(canExecute) =
        let valueProp = ValueProp(null)
        struct(Command(valueProp.OnNext, canExecute) :> ICommand, valueProp :> IGetProp<obj>)

    [<Extension>]
    static member Increment(prop) = Prop.incr prop

    [<Extension>]
    static member Decrement(prop) = Prop.decr prop

    [<Extension>]
    static member Not(prop) = Prop.not prop

    [<Extension>]
    static member NotSet(prop) = Prop.notSet prop

    [<Extension>]
    static member NotBoth(prop) = Prop.notBoth prop

    static member True = Prop.ctrue

    static member False = Prop.cfalse
