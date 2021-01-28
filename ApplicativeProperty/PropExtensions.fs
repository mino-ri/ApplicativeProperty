namespace ApplicativeProperty
open System
open System.Runtime.CompilerServices
open System.Windows.Input

[<Sealed; AbstractClass; Extension>]
type PropExtensions =

    [<Extension>]
    static member Subscribe(getProp: IGetProp<'T>, onNext) =
        getProp.Subscribe(action1 onNext)

    [<Extension>]
    static member Select(prop, selector: Func<'T, 'U>) = Prop.map selector.Invoke prop

    [<Extension>]
    static member Zip(prop1, prop2, selector: Func<'T1, 'T2, 'U>) = Prop.map2 (fun x y -> selector.Invoke(x, y)) prop1 prop2

    [<Extension>]
    static member Zip(prop1, prop2, prop3, selector: Func<'T1, 'T2, 'T3, 'U>) = Prop.map3 (fun x y z -> selector.Invoke(x, y, z)) prop1 prop2 prop3

    [<Extension>]
    static member WhereCached(prop, predicate: Func<'T, bool>) = Prop.filterc predicate.Invoke prop

    [<Extension>]
    static member Unzip(prop: IGetProp<'T>, selector: Func<'T, struct('U1 * 'U2)>) =
        let cached = prop |> Prop.map selector.Invoke |> Prop.cache
        Prop.map vfst cached, Prop.map vsnd cached

    [<Extension>]
    static member Apply(funcProp: IGetProp<Func<'T, 'U>>, argProp) =
        (funcProp, argProp) ||> Prop.map2 (fun f x -> f.Invoke(x))

    [<Extension>]
    static member Apply(funcProp: IGetProp<Func<'T1, 'T2, 'U>>, argProp1, argProp2) =
        (funcProp, argProp1, argProp2) |||> Prop.map3 (fun f x y -> f.Invoke(x, y))

    [<Extension>]
    static member Apply2(funcProp: IGetProp<'T -> 'U>, argProp) = Prop.apply funcProp argProp

    [<Extension>]
    static member WithSet(getter, setter) : IProp<'T> = Prop.withSet setter getter
    
    [<Extension>]
    static member WithGet(setter, getter) : IProp<'T> = Prop.withGet getter setter

    [<Extension>]
    static member Modify(prop, modification: Func<'T, 'T>) = Prop.modify modification.Invoke prop

    [<Extension>]
    static member SelectBoth(prop, getSelector: Func<'T, 'U>, setSelector: Func<'U, 'T>) =
        Prop.mapBoth getSelector.Invoke setSelector.Invoke prop

    [<Extension>]
    static member ZipBoth(prop1, prop2, getSelector:Func<'T1, 'T2, 'U>, setSelector) =
        Prop.map2Both (func2 getSelector) (tuple2 setSelector) prop1 prop2

    [<Extension>]
    static member ZipBoth(prop1, prop2, prop3, getSelector:Func<'T1, 'T2, 'T3, 'U>, setSelector) =
        Prop.map3Both (func3 getSelector) (tuple3 setSelector) prop1 prop2 prop3

    [<Extension>]
    static member ToCommand(canExecute, onExecute: Action<obj>) = Command(onExecute.Invoke, canExecute) :> ICommand

    [<Extension>]
    static member ToCommand(canExecute) =
        let subject = Subject()
        struct(Command(subject.OnNext, canExecute) :> ICommand, Observable.asObservable subject)
