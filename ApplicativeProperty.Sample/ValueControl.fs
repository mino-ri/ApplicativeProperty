namespace ApplicativeProperty.Sample
open System
open System.Windows
open System.Windows.Controls

type ValueControl() =
    inherit Control()
    static member val AmountProperty =
        DependencyProperty.Register("Amount", typeof<int>, typeof<ValueControl>,
            FrameworkPropertyMetadata(5))

    member this.Amount
        with get() = this.GetValue(ValueControl.AmountProperty) :?> int
        and set(v: int) = this.SetValue(ValueControl.AmountProperty, box v)

    static member val AttachProperty =
        DependencyProperty.RegisterAttached("Attach", typeof<string>, typeof<FrameworkElement>,
            FrameworkPropertyMetadata("Default"))

    static member GetAttach(o: FrameworkElement) =
        o.GetValue(ValueControl.AttachProperty) :?> string
    static member SetAttach(o: FrameworkElement, value: string) =
        o.SetValue(ValueControl.AttachProperty, box value)
