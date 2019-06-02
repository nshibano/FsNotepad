[<AutoOpen>]
module Common

#nowarn "9"

open System
open System.Diagnostics
open System.Windows.Forms
open System.Drawing
open Microsoft.FSharp.NativeInterop
open System.Text
open System.IO
open System.Reflection

let readAllTextFromLockedFile path =
    use fs = new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.ReadWrite)
    use r = new StreamReader(fs)
    r.ReadToEnd()

let exeDir = Path.GetDirectoryName(Assembly.GetEntryAssembly().Location)

exception DontCareException
let dontcare() = raise DontCareException

let fontSizeSeries =
    [| 8; 10; 12; 14; 16; 18; 20; 22; 24; 26; 28; 30; 32; 34; 36; 38;
       40; 44; 48; 52; 56; 60; 64; 68; 72; 76; 80; 84; 88; 92; 96;
       100; 110; 120; 130; 140; 150; 160; 170; 180; 190;
       200; 220; 240; 260; 280; 300; 320; 340; 360; 380; 400 |]

let symbol_rn =
    let x0, x1, x2 = 0.15f, 0.25f, 0.35f
    let y0, y1, y2, y3 = 0.7f, 0.6f, 0.8f, 0.25f
    let p0 = PointF(x0, y0)
    let p1 = PointF(x1, y1)
    let p2 = PointF(x1, y2)
    let p3 = PointF(x2, y0)
    let p4 = PointF(x2, y3)
    [| struct (p0, p1)
       struct (p0, p2)
       struct (p0, p3)
       struct (p3, p4) |]
    
let symbol_n =
    let x0, x1, x2 = 0.15f, 0.25f, 0.35f
    let y0, y1, y2 = 0.7f, 0.6f, 0.25f
    let p0 = PointF(x0, y1)
    let p1 = PointF(x2, y1)
    let p2 = PointF(x1, y0)
    let p3 = PointF(x1, y2)
    [| struct (p0, p2)
       struct (p1, p2)
       struct (p2, p3) |]

let symbol_t =
    let x0, x1, x2 = -0.4f, -0.2f, -0.1f
    let y0, y1, y2 = 0.4f, 0.5f, 0.6f
    let p0 = PointF(x2, y1)
    let p1 = PointF(x0, y1)
    let p2 = PointF(x1, y0)
    let p3 = PointF(x1, y2)
    [| struct (p0, p1)
       struct (p0, p2)
       struct (p0, p3) |]

let mutable inNestedMessageLoop = false

let applyWithInNestedMessageLoopFlagSet f x =
    inNestedMessageLoop <- true
    let y = f x
    inNestedMessageLoop <- false
    y

let dialogFilter = "Supported files (*.txt;*.cs;*.fs;*.mal)|*.txt;*.cs;*.fs;*.mal|Text files (*.txt)|*.txt|All files (*.*)|*.*"

type Rectangle with
    member r.Center = Point(r.X + r.Width / 2, r.Y + r.Height / 2)
    
type Stopwatch with
    member this.ElapsedMicroseconds = int64 ((1000000I * bigint this.ElapsedTicks) / bigint Stopwatch.Frequency)

type ScrollBar with
    // The maximum value that can be reached through user interaction is equal to
    // 1 plus the Maximum property value minus the LargeChange property value.
    member this.MaximumValueThatCanBeReachedThroughUserInteraction =
        Math.Min(Math.Max(1 + this.Maximum - this.LargeChange, this.Minimum), this.Maximum)

type System.Text.StringBuilder with
    member sb.Add(c : char) = sb.Append(c) |> ignore
    member sb.Add(c : char, n : int) = sb.Append(c, n) |> ignore
    member sb.Add(s : string) = sb.Append(s) |> ignore

type System.Random with
    member random.NextLightColor() =
        let f() = 192 + random.Next(64)
        Color.FromArgb(f(), f(), f())

type ImeResultStrEventArgs(resultStr : string) =
    inherit EventArgs()
    member this.ResultStr = resultStr

type OpaqueIMEControl() as this =
    inherit Control()
    do this.SetStyle(ControlStyles.Opaque, true)
    
    let startComposition = Event<EventArgs>()
    let endComposition = Event<EventArgs>()
    let resultStr = Event<ImeResultStrEventArgs>()

    [<CLIEvent>]
    member this.ImeStartComposition = startComposition.Publish
    [<CLIEvent>]
    member this.ImeEndComposition = endComposition.Publish    
    [<CLIEvent>]
    member this.ImeResultStr = resultStr.Publish

    override this.IsInputKey(keyData) =
        if keyData.HasFlag(Keys.Left) ||
           keyData.HasFlag(Keys.Right) ||
           keyData.HasFlag(Keys.Up) ||
           keyData.HasFlag(Keys.Down) ||
           keyData.HasFlag(Keys.Tab)
        then true
        else base.IsInputKey(keyData)

    override this.WndProc(m : Message byref) =
        match m.Msg with
        | Win32.WM_IME_STARTCOMPOSITION ->
            //Debug.WriteLine("WM_IME_STARTCOMPOSITION")
            startComposition.Trigger(EventArgs())
        | Win32.WM_IME_ENDCOMPOSITION ->
            //Debug.WriteLine("WM_IME_ENDCOMPOSITION")
            endComposition.Trigger(EventArgs())
        | Win32.WM_IME_COMPOSITION ->
            //Debug.WriteLine(sprintf "WM_IME_COMPOSITION (%s)" (Win32.string_of_imm (int m.LParam)))
            if Win32.GCS_RESULTSTR &&& int m.LParam <> 0 then
                let ime = Win32.ImmGetContext(this.Handle)
                let byteCount = Win32.ImmGetCompositionString(ime, uint32 Win32.GCS_RESULTSTR, IntPtr.Zero, 0u)
                let resultstr =
                    if byteCount > 0 then
                        let buf = Array.zeroCreate<byte> byteCount
                        use p = fixed buf
                        Win32.ImmGetCompositionString(ime, uint32 Win32.GCS_RESULTSTR, NativePtr.toNativeInt p, uint32 buf.Length) |> ignore
                        System.Text.Encoding.Unicode.GetString(buf)
                    else ""
                Win32.ImmReleaseContext(this.Handle, ime) |> ignore
                //Debug.WriteLine(sprintf "RESULTSTR = \"%s\"" resultstr)
                resultStr.Trigger(ImeResultStrEventArgs(resultstr))
        | Win32.WM_IME_NOTIFY ->
            //Debug.WriteLine(sprintf "WM_IME_NOTIFY (%s)" (Win32.string_of_imn (int m.WParam)))
            ()
        | Win32.WM_IME_CHAR ->
            //Debug.WriteLine("WM_IME_CHAR")
            ()
        | Win32.WM_IME_REQUEST ->
            //Debug.WriteLine(sprintf "WM_IME_REQUEST (%s)" (Win32.string_of_imr (int m.WParam)))
            ()
        | _ -> ()

        base.WndProc(&m)