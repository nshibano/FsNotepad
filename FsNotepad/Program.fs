module Program
open System
open System.IO
open System.Diagnostics
open System.Windows.Forms
open System.Runtime.InteropServices

[<DllImport("user32.dll")>]
extern bool SetProcessDPIAware()

[<EntryPoint; STAThread>]
let main args = 
    if Environment.OSVersion.Version.Major >= 6 then SetProcessDPIAware() |> ignore
    Application.EnableVisualStyles()
    Application.SetCompatibleTextRenderingDefault(false)
    Application.SetUnhandledExceptionMode(UnhandledExceptionMode.ThrowException)

    let editor = new Editor(None)

    AppDomain.CurrentDomain.UnhandledException.Add(fun ev ->
        let logDir = Environment.GetFolderPath(Environment.SpecialFolder.Desktop)
        let id = sprintf "FsNotepad-%s-%d-" (DateTime.Now.ToString("yyyyMMddHHmmss")) (Process.GetCurrentProcess().Id)
        let exn = ev.ExceptionObject
        File.WriteAllText(Path.Combine(logDir, id + "Crash-Exn.txt"), exn.ToString())
        let name =
            match editor.TextFileHandle with
            | None -> sprintf "Untitled.txt"
            | Some h -> Path.GetFileName (h.OriginalPath)
        let text = Doc.getAllString editor.Doc
        File.WriteAllText(Path.Combine(logDir, id + name), text))
    
    if args.Length > 0 then
        editor.OpenPath args.[0]

    Application.Run(editor)

    0