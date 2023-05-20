namespace ApplicativeProperty.Sample

open System
open System.Windows
open System.Windows.Markup

type LoadExtension(uri: string) =
    inherit MarkupExtension()

    member val Uri = uri with get, set

    new() = LoadExtension("")

    override this.ProvideValue(_) = Application.LoadComponent(Uri(this.Uri, UriKind.Relative))
