[<AutoOpen>]
module internal ApplicativeProperty.Internal

let fst3 (x, _, _) = x

let snd3 (_, x, _) = x

let thd3 (_, _, x) = x

let tpl2 x y = x, y

let tpl3 x y z = x, y, z
