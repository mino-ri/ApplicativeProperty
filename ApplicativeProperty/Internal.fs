[<AutoOpen>]
module internal ApplicativeProperty.Internal
open System

let fst3 (x, _, _) = x

let snd3 (_, x, _) = x

let thd3 (_, _, x) = x

let tpl2 x y = x, y

let tpl3 x y z = x, y, z

let vfst struct(x, _) = x

let vsnd struct(_, x) = x

let inline action0 (f: Action) = f.Invoke

let inline action1 (f: Action<_>) = f.Invoke

let inline action2 (f: Action<_, _>) x y = f.Invoke(x, y)

let inline action3 (f: Action<_, _, _>) x y z = f.Invoke(x, y, z)

let inline func0 (f: Func<_>) = f.Invoke

let inline func1 (f: Func<_, _>) x = f.Invoke(x)

let inline func2 (f: Func<_, _, _>) x y = f.Invoke(x, y)

let inline func3 (f: Func<_, _, _, _>) x y z = f.Invoke(x, y, z)

let inline tuple2 (f: Func<_, struct(_ * _)>) x = let struct(y, z) = f.Invoke(x) in y, z

let inline tuple3 (f: Func<_, struct(_ * _ * _)>) x = let struct(y, z, w) = f.Invoke(x) in y, z, w
