module FileHelper

open System
open System.IO
open System.Text
open System.Windows.Forms
open Microsoft.WindowsAPICodePack.Dialogs
open Microsoft.WindowsAPICodePack.Dialogs.Controls

type TextEncoding =
    | UTF8
    | UTF8BOM
    | SJIS
    | EUCJP
    member x.ToEncoding() =
        match x with
        | UTF8 -> UTF8Encoding(false) :> System.Text.Encoding
        | UTF8BOM -> UTF8Encoding(true) :> System.Text.Encoding
        | SJIS -> Encoding.GetEncoding("shift_jis")
        | EUCJP -> Encoding.GetEncoding("EUC-JP")

type TextDecodingMode =
    | TDMauto
    | TDMutf8
    | TDMsjis
    | TDMeucjp

type LineEnding =
    | CRLF 
    | LF
    | CR

let utf8bom = Encoding.UTF8.GetPreamble()

let startsWithUtf8Bom (bytes : byte array) = bytes.Length >= 3 && Array.sub bytes 0 3 = utf8bom

let decodeUtf8 (bytes : byte array) =
    if startsWithUtf8Bom bytes then
        Encoding.UTF8.GetString(Array.sub bytes 3 (bytes.Length - 3))
    else Encoding.UTF8.GetString(bytes)

let encode (enc : TextEncoding) (s : string) : byte array =
    match enc with
    | UTF8 -> Encoding.UTF8.GetBytes(s)
    | UTF8BOM -> Array.append utf8bom (Encoding.UTF8.GetBytes(s))
    | SJIS -> Encoding.GetEncoding("shift_jis").GetBytes(s)
    | EUCJP -> Encoding.GetEncoding("EUC-JP").GetBytes(s)

let byte_is_ascii (byte : byte) =
    match byte with
    | 0x04uy
    | 0x09uy
    | 0x0Auy
    | 0x0Duy -> true
    | byte -> 0x20uy <= byte && byte <= 0x7Euy

let guess (bytes : byte array) : TextEncoding * string =
    if startsWithUtf8Bom bytes then
        (UTF8BOM, decodeUtf8 bytes)
    elif Array.forall byte_is_ascii bytes then
        (UTF8, decodeUtf8 bytes)
    else
        let cdet = Ude.CharsetDetector()
        cdet.Feed(bytes, 0, bytes.Length)
        cdet.DataEnd()
        //System.Diagnostics.Debug.WriteLine("cdet result: " + if cdet.Charset = null then "null" else cdet.Charset)

        let e =
            match cdet.Charset with
            | "UTF-8" -> UTF8
            | "EUC-JP" -> EUCJP
            | "Shift-JIS" -> SJIS
            | null -> SJIS
            | _ -> SJIS

        (e, e.ToEncoding().GetString(bytes))

let decode (mode : TextDecodingMode) (bytes : byte array) : (TextEncoding * string) =
    match mode with
    | TDMauto -> guess bytes
    | TDMutf8 ->
        if startsWithUtf8Bom bytes then
            (UTF8BOM, Encoding.UTF8.GetString(Array.sub bytes 3 (bytes.Length - 3)))
        else 
            (UTF8, Encoding.UTF8.GetString(bytes))
    | TDMsjis -> (SJIS, Encoding.GetEncoding("shift_jis").GetString(bytes))
    | TDMeucjp -> (EUCJP, Encoding.GetEncoding("EUC-JP").GetString(bytes))

let detectLineEnding (s : string) =
    let mutable crlf_count = 0
    let mutable lf_count = 0
    let mutable cr_count = 0
    let mutable i = 0
    while i < s.Length do
        let c0 = s.[i]
        if c0 = '\r' then
            if i + 1 < s.Length then
                let c1 = s.[i + 1]
                if c1 = '\n' then
                    crlf_count <- crlf_count + 1
                    i <- i + 2
                else
                    cr_count <- cr_count + 1
                    i <- i + 1
            else 
                cr_count <- cr_count + 1
                i <- i + 1
        elif c0 = '\n' then
            lf_count <- lf_count + 1
            i <- i + 1
        else i <- i + 1
    if crlf_count >= lf_count && crlf_count >= cr_count then CRLF
    elif lf_count >= cr_count then LF
    else CR

let maximumFileSize = 100_000_000

type TextFileHandle =
    { OriginalPath : string
      FileStream : FileStream
      mutable LatestText : string
      TextEncoding : TextEncoding
      LineEnding : LineEnding }

type TryOpenPathResult =
    | TOPRsuccess of TextFileHandle
    | TOPRfailed of exn

let tryOpenPath (path : string) =
    let mutable fd = null : FileStream
    try
        fd <- new FileStream(path, FileMode.Open, FileAccess.ReadWrite, FileShare.Read)
        if fd.Length > int64 maximumFileSize then raise (Exception("File is too large."))
        let len = int fd.Length
        let buf = Array.zeroCreate<byte> len
        if fd.Read(buf, 0, len) <> len then raise (Exception("Failed to read file."))
        let enc, s = decode TDMauto buf
        let le = detectLineEnding s
        TOPRsuccess ({ OriginalPath = path; FileStream = fd; LatestText = s; TextEncoding = enc; LineEnding = le })
    with exn ->
        if not (isNull fd) then fd.Dispose()
        TOPRfailed exn

type TryOpenResult =
    | TORsuccess of TextFileHandle
    | TORcancelled
    | TORfailed of exn

let defaultInitialPath() = Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments)

let tryOpen (oldHandle : TextFileHandle option)  : TryOpenResult =
    use dialog = new CommonOpenFileDialog()
    dialog.Filters.Add(CommonFileDialogFilter("All files", "*"))
    dialog.Filters.Add(CommonFileDialogFilter("Text files", "txt,cs,fs,csv,md,c,h,ini,json,log,htm,html"))
    dialog.InitialDirectory <-
        match oldHandle with
        | Some h ->
            let dir = Path.GetDirectoryName(h.OriginalPath)
            if Directory.Exists(dir) then
                dir
            else defaultInitialPath()
        | None -> defaultInitialPath()
    dialog.DefaultFileName <- match oldHandle with None -> "" | Some h -> Path.GetFileName(h.OriginalPath)
    let decodingModes = [|("Auto", TDMauto); ("UTF8", TDMutf8); ("SJIS", TDMsjis); ("EUCJP", TDMeucjp)|]
    let comboBox = new CommonFileDialogComboBox()
    for (name, _) in decodingModes do
        comboBox.Items.Add(CommonFileDialogComboBoxItem(name))
    comboBox.SelectedIndex <- 0
    dialog.Controls.Add(comboBox)
    match applyWithInNestedMessageLoopFlagSet (fun () -> dialog.ShowDialog(Form.ActiveForm.Handle)) () with
    | CommonFileDialogResult.Ok ->
        Option.iter (fun (f : TextFileHandle) -> f.FileStream.Close()) oldHandle
        let mutable fd = null : FileStream
        try
            let path = dialog.FileName
            fd <- new FileStream(path, FileMode.Open, FileAccess.ReadWrite, FileShare.Read)
            if fd.Length > int64 maximumFileSize then raise (Exception("File is too large."))
            let len = int fd.Length
            let buf = Array.zeroCreate<byte> len
            if fd.Read(buf, 0, len) <> len then raise (Exception("Failed to read file."))
            let mode = snd (decodingModes.[comboBox.SelectedIndex])
            let enc, s = decode mode buf
            let le = detectLineEnding s
            TORsuccess ({ OriginalPath = path; FileStream = fd; LatestText = s; TextEncoding = enc; LineEnding = le })
        with exn ->
            if not (isNull fd) then fd.Dispose()
            TORfailed exn
    | _ -> TORcancelled

type TrySaveResult =
    | TSRsuccess
    | TSRfialed of exn

let trySave (handle : TextFileHandle) (newText : string) =
    let bytes = encode handle.TextEncoding newText
    try
        handle.FileStream.Seek(0L, SeekOrigin.Begin) |> ignore
        handle.FileStream.Write(bytes, 0, bytes.Length)
        handle.FileStream.SetLength(int64 bytes.Length)
        handle.FileStream.Flush()
        handle.LatestText <- newText
        TrySaveResult.TSRsuccess
    with exn -> TrySaveResult.TSRfialed exn

type TrySaveAsResult =
    | TSARsuccess of TextFileHandle
    | TSARcancelled
    | TSARfailed of exn

let trySaveAs (oldHandle : TextFileHandle option) (newText : string) =
    use dialog = new CommonSaveFileDialog()
    dialog.Filters.Add(CommonFileDialogFilter("Text files", "txt,cs,fs,csv,md,c,h,ini,json,log,htm,html"))
    dialog.Filters.Add(CommonFileDialogFilter("All files", "*"))
    dialog.InitialDirectory <-
        match oldHandle with
        | Some h ->
            let dir = Path.GetDirectoryName(h.OriginalPath)
            if Directory.Exists(dir) then
                dir
            else defaultInitialPath()
        | None -> defaultInitialPath()
    dialog.DefaultFileName <- match oldHandle with None -> "" | Some h -> Path.GetFileName(h.OriginalPath)
    let encodings = [|("UTF8", UTF8); ("UTF8BOM", UTF8BOM); ("SJIS", SJIS); ("EUCJP", EUCJP)|]
    let comboBox = new CommonFileDialogComboBox()
    for (name, _) in encodings do
        comboBox.Items.Add(CommonFileDialogComboBoxItem(name))
    comboBox.SelectedIndex <-
        match oldHandle with
        | Some h -> Array.findIndex (fun (_, enc) -> enc = h.TextEncoding) encodings
        | None -> 0
    dialog.Controls.Add(comboBox)
    match applyWithInNestedMessageLoopFlagSet (fun () -> dialog.ShowDialog(Form.ActiveForm.Handle)) () with
    | CommonFileDialogResult.Ok ->
        match oldHandle with
        | Some h -> h.FileStream.Dispose()
        | None -> ()
        let encoding = snd (encodings.[comboBox.SelectedIndex])
        let bytes = encode encoding newText
        let lineEnding = detectLineEnding newText
        let path = dialog.FileName
        let path =
            if Path.GetExtension(path) = "" && dialog.SelectedFileTypeIndex = 1 (* 1-based index *) then
                path + ".txt"
            else path
        let mutable fd = null : FileStream
        try
           fd <- new FileStream(dialog.FileName, FileMode.OpenOrCreate, FileAccess.ReadWrite, FileShare.Read)
           fd.Seek(0L, SeekOrigin.Begin) |> ignore
           fd.Write(bytes, 0, bytes.Length)
           fd.SetLength(int64 bytes.Length)
           fd.Flush()
           TSARsuccess { OriginalPath = dialog.FileName; FileStream = fd; LatestText = newText; TextEncoding = encoding; LineEnding = lineEnding }
        with exn ->
            if not (isNull fd) then fd.Dispose()
            TSARfailed exn
    | _ -> TSARcancelled