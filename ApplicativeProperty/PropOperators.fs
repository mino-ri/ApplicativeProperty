module ApplicativeProperty.PropOperators
open System

let inline ( .+. ) prop1 prop2 = Prop.map2 (+) prop1 prop2

let inline ( .+ ) prop1 value2 = Prop.map (fun x -> x + value2) prop1

let inline ( +. ) value1 prop2 = Prop.map (fun x -> value1 + x) prop2

let inline ( .-. ) prop1 prop2 = Prop.map2 (-) prop1 prop2

let inline ( .- ) prop1 value2 = Prop.map (fun x -> x - value2) prop1

let inline ( -. ) value1 prop2 = Prop.map (fun x -> value1 - x) prop2

let inline ( .*. ) prop1 prop2 = Prop.map2 ( * ) prop1 prop2

let inline ( .* ) prop1 value2 = Prop.map (fun x -> x * value2) prop1

let inline ( *. ) value1 prop2 = Prop.map (fun x -> value1 * x) prop2

let inline ( ./. ) prop1 prop2 = Prop.map2 (/) prop1 prop2

let inline ( ./ ) prop1 value2 = Prop.map (fun x -> x / value2) prop1

let inline ( /. ) value1 prop2 = Prop.map (fun x -> value1 / x) prop2

let inline ( .%. ) prop1 prop2 = Prop.map2 (%) prop1 prop2

let inline ( .% ) prop1 value2 = Prop.map (fun x -> x % value2) prop1

let inline ( %. ) value1 prop2 = Prop.map (fun x -> value1 % x) prop2

let inline ( .**. ) prop1 prop2 = Prop.map2 ( ** ) prop1 prop2

let inline ( .** ) prop1 value2 = Prop.map (fun x -> x ** value2) prop1

let inline ( **. ) value1 prop2 = Prop.map (fun x -> value1 ** x) prop2

let inline ( .<. ) prop1 prop2 = Prop.map2 (<) prop1 prop2

let inline ( .< ) prop1 value2 = Prop.map (fun x -> x < value2) prop1

let inline ( <. ) value1 prop2 = Prop.map (fun x -> value1 < x) prop2

let inline ( .>. ) prop1 prop2 = Prop.map2 (>) prop1 prop2

let inline ( .> ) prop1 value2 = Prop.map (fun x -> x > value2) prop1

let inline ( >. ) value1 prop2 = Prop.map (fun x -> value1 > x) prop2

let inline ( .>=. ) prop1 prop2 = Prop.map2 (>=) prop1 prop2

let inline ( .>= ) prop1 value2 = Prop.map (fun x -> x >= value2) prop1

let inline ( >=. ) value1 prop2 = Prop.map (fun x -> value1 >= x) prop2

let inline ( .<=. ) prop1 prop2 = Prop.map2 (<=) prop1 prop2

let inline ( .<= ) prop1 value2 = Prop.map (fun x -> x <= value2) prop1

let inline ( <=. ) value1 prop2 = Prop.map (fun x -> value1 <= x) prop2

let inline ( .=. ) prop1 prop2 = Prop.map2 (=) prop1 prop2

let inline ( .= ) prop1 value2 = Prop.map (fun x -> x = value2) prop1

let inline ( =. ) value1 prop2 = Prop.map (fun x -> value1 = x) prop2

let inline ( .<>. ) prop1 prop2 = Prop.map2 (<>) prop1 prop2

let inline ( .<> ) prop1 value2 = Prop.map (fun x -> x <> value2) prop1

let inline ( <>. ) value1 prop2 = Prop.map (fun x -> value1 <> x) prop2

let inline ( .&&. ) prop1 prop2 = Prop.map2 (&&) prop1 prop2

let inline ( .&& ) prop1 value2 = Prop.map (fun x -> x && value2) prop1

let inline ( &&. ) value1 prop2 = Prop.map (fun x -> value1 && x) prop2

let inline ( .||. ) prop1 prop2 = Prop.map2 (||) prop1 prop2

let inline ( .|| ) prop1 value2 = Prop.map (fun x -> x || value2) prop1

let inline ( ||. ) value1 prop2 = Prop.map (fun x -> value1 || x) prop2

let inline ( .&&&. ) prop1 prop2 = Prop.map2 (&&&) prop1 prop2

let inline ( .&&& ) prop1 value2 = Prop.map (fun x -> x &&& value2) prop1

let inline ( &&&. ) value1 prop2 = Prop.map (fun x -> value1 &&& x) prop2

let inline ( .|||. ) prop1 prop2 = Prop.map2 (|||) prop1 prop2

let inline ( .||| ) prop1 value2 = Prop.map (fun x -> x ||| value2) prop1

let inline ( |||. ) value1 prop2 = Prop.map (fun x -> value1 ||| x) prop2

let inline ( .^^^. ) prop1 prop2 = Prop.map2 (^^^) prop1 prop2

let inline ( .^^^ ) prop1 value2 = Prop.map (fun x -> x ^^^ value2) prop1

let inline ( ^^^. ) value1 prop2 = Prop.map (fun x -> value1 ^^^ x) prop2

let inline ( .<<<. ) prop1 prop2 = Prop.map2 (<<<) prop1 prop2

let inline ( .<<< ) prop1 value2 = Prop.map (fun x -> x <<< value2) prop1

let inline ( <<<. ) value1 prop2 = Prop.map (fun x -> value1 <<< x) prop2

let inline ( .>>>. ) prop1 prop2 = Prop.map2 (>>>) prop1 prop2

let inline ( .>>> ) prop1 value2 = Prop.map (fun x -> x >>> value2) prop1

let inline ( >>>. ) value1 prop2 = Prop.map (fun x -> value1 >>> x) prop2

let inline ( .<|. ) fProp argProp = Prop.map2 (<|) fProp argProp

let inline ( .<| ) fProp arg = Prop.map (fun x -> x <| arg) fProp

let inline ( <|. ) f argProp = Prop.map (fun x -> f <| x) argProp

let inline ( .|>. ) argProp fProp = Prop.map2 (|>) argProp fProp

let inline ( .|> ) argProp f = Prop.map (fun x -> x |> f) argProp

let inline ( |>. ) arg fProp = Prop.map (fun x -> arg |> x) fProp

let inline ( .>>. ) prop1 prop2 = Prop.map2 (>>) prop1 prop2

let inline ( .>> ) prop1 value2 = Prop.map (fun x -> x >> value2) prop1

let inline ( >>. ) value1 prop2 = Prop.map (fun x -> value1 >> x) prop2

let inline ( .<<. ) prop1 prop2 = Prop.map2 (<<) prop1 prop2

let inline ( .<< ) prop1 value2 = Prop.map (fun x -> x << value2) prop1

let inline ( <<. ) value1 prop2 = Prop.map (fun x -> value1 << x) prop2

let inline ( .@. ) prop1 prop2 = Prop.map2 (@) prop1 prop2

let inline ( .@ ) prop1 value2 = Prop.map (fun x -> x @ value2) prop1

let inline ( @. ) value1 prop2 = Prop.map (fun x -> value1 @ x) prop2

let inline ( ~+. ) prop = Prop.map (~+) prop

let inline ( ~-. ) prop = Prop.map (~-) prop

let inline ( .<-. ) observer (getProp: IGetProp<_>) = getProp.Subscribe(observer)

let inline ( .->. ) (getProp: IGetProp<_>) observer = getProp.Subscribe(observer)

let inline ( .<- ) (observer: IObserver<_>) value = observer.OnNext(value)

let inline ( ->. ) value (observer: IObserver<_>) = observer.OnNext(value)
