#load "Common.fsx"
#r @"System.IO.Compression.dll"
#r @"System.IO.Compression.FileSystem.dll"

open System
open System.IO
open System.IO.Compression

open Common

let inAppveyor, buildVersion =
    let s = Environment.GetEnvironmentVariable("APPVEYOR_BUILD_VERSION")
    if isNull s then
        false, "0.0.0"
    else
        true, s

let pushArtifact path =
    if inAppveyor then
        cmd "appveyor" ("PushArtifact " + path)

cmd "nuget" "restore"
cmd "msbuild" @"/p:Configuration=Release FsNotepad.sln"

if Directory.Exists("Publish") then
    Directory.Delete("Publish", true)
Directory.CreateDirectory(@"Publish") |> ignore

copyAllFiles @"FsNotepad\bin\Release" "Publish"
let net45ZipFileName = sprintf "FsNotepad-%s.zip" buildVersion
if File.Exists(net45ZipFileName) then
    File.Delete(net45ZipFileName)
ZipFile.CreateFromDirectory("Publish", net45ZipFileName)
pushArtifact net45ZipFileName
