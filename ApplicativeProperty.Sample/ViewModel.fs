namespace ApplicativeProperty.Sample

open System.ComponentModel
open ApplicativeProperty
open ApplicativeProperty.PropOperators

type ViewModel() =
    let value1 = Prop.value 0
    let value2 = Prop.value 1

    member _.Value1 = value1

    member _.Value2 = value2

    member val Sum = value1 .+. value2

    member val IncrementCommand = Prop.command (fun _ -> Prop.incr value1) (value1 .< 10)

    member val DecrementCommand = Prop.command (fun _ -> Prop.decr value1) (value1 .> 0)

    interface INotifyPropertyChanged with
        member _.add_PropertyChanged(_) = ()
        member _.remove_PropertyChanged(_) = ()
