open System
open System.Diagnostics
open System.IO

let cmd name arg =
    Console.WriteLine(name + " " + arg)
    let info = ProcessStartInfo(name, arg)
    info.RedirectStandardOutput <- true
    info.RedirectStandardError <- true
    info.UseShellExecute <- false
    info.CreateNoWindow <- true
    let proc = Process.Start(info)
    proc.OutputDataReceived.Add(fun x -> Console.WriteLine(x.Data))
    proc.ErrorDataReceived.Add(fun x -> Console.Error.WriteLine(x.Data))
    proc.BeginOutputReadLine()
    proc.BeginErrorReadLine()
    proc.WaitForExit()
    if proc.ExitCode <> 0 then failwithf "command %s %s failed with exit code %d." name arg proc.ExitCode

let ensureDirectory (pathToDir : string) =
    try Directory.CreateDirectory(pathToDir) |> ignore
    with _ -> ()
    
let copyFile pathFrom pathTo =
    ensureDirectory (Path.GetDirectoryName(pathTo))
    File.Copy(pathFrom, pathTo, true)

let copyDir origDir newDir =
    Directory.CreateDirectory(newDir) |> ignore
    for file in Directory.GetFiles(origDir) do
        File.Copy(file, Path.Combine(newDir, Path.GetFileName(file)))

let copyAllFiles fromDir toDir =
    for file in Directory.GetFiles(fromDir) do
        let toPath = Path.Combine(toDir, Path.GetFileName(file))
        ensureDirectory (Path.GetDirectoryName(toPath))
        File.Copy(file, Path.Combine(toDir, Path.GetFileName(file)), true)
    