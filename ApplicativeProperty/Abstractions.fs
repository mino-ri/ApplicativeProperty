namespace ApplicativeProperty
open System
open System.ComponentModel
open System.Runtime.CompilerServices

type IGetProp<'T> =
    abstract member Value : 'T with get
    abstract member Subscribe : onNext: ('T -> unit) -> IDisposable

type ISetProp<'T> =
    abstract member OnNext : vaue: 'T -> unit

type IProp<'T> =
    inherit ISetProp<'T>
    inherit IGetProp<'T>

type INotifyGetProp<'T> =
    inherit IGetProp<'T>
    inherit INotifyPropertyChanged

type INotifyProp<'T> =
    inherit IProp<'T>
    inherit INotifyGetProp<'T>

type IDisposableGetProp<'T> =
    inherit IGetProp<'T>
    inherit IDisposable

[<Sealed; AbstractClass; Extension>]
type PropExtensions =
    [<Extension>]
    static member Subscribe<'T>(getProp: IGetProp<'T>, setProp: ISetProp<'T>) =
        getProp.Subscribe(setProp.OnNext)
