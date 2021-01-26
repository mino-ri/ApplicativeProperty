open System
open System.Windows

[<STAThread; EntryPoint>]
let main argv = 
    let uri = new Uri("/ApplicativeProperty.Sample;component/App.xaml", UriKind.Relative)
    let application = Application.LoadComponent(uri) :?> Application
    application.Run()
