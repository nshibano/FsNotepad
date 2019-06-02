module Win32

#nowarn "9"

open System
open System.Runtime.InteropServices

// C:\Program Files (x86)\Windows Kits\10\Include\10.0.16299.0\um\Windows.h
// C:\Program Files (x86)\Windows Kits\10\Include\10.0.16299.0\um\imm.h
// C:\Program Files (x86)\Windows Kits\10\Include\10.0.16299.0\um\wingdi.h

[<StructLayout(LayoutKind.Sequential)>]
type Message =
    struct
        val mutable hWnd : IntPtr
        val mutable Msg : uint32
        val mutable wParam : IntPtr
        val mutable lParam : IntPtr
        val mutable Time : uint32
        val mutable Point : System.Drawing.Point
    end

[<DllImport("user32.dll")>] extern Int32 SetCaretPos(Int32 x, Int32 y)
[<DllImport("user32.dll")>] extern Int32 CreateCaret(IntPtr window, IntPtr hBitmap, Int32 width, Int32 height)
[<DllImport("user32.dll")>] extern Int32 DestroyCaret()
[<DllImport("user32.dll")>] extern Int32 ShowCaret(IntPtr window)
[<DllImport("user32.dll")>] extern Int32 HideCaret(IntPtr window)
[<DllImport("user32.dll")>] extern [<MarshalAs(UnmanagedType.Bool)>] bool PeekMessage(Message& message, IntPtr hWnd, uint32 filterMin, uint32 filterMax, uint32 flags)

[<StructLayout(LayoutKind.Sequential)>]
type POINT =
    struct
        val mutable x : Int32
        val mutable y : Int32
    end

[<StructLayout(LayoutKind.Sequential)>]
type RECT =
    struct
        val mutable left : Int32
        val mutable top : Int32
        val mutable right : Int32
        val mutable bottom : Int32
    end

let [<Literal>] CFS_POINT = 0x0002u

[<StructLayout(LayoutKind.Sequential)>]
type COMPOSITIONFORM =
    struct
        val mutable dwStyle : UInt32
        val mutable ptCurrentPos : POINT
        val mutable rcArea : RECT
    end

[<StructLayout(LayoutKind.Sequential, CharSet = CharSet.Unicode)>]
type LOGFONT =
    val mutable lfHeight : int
    val mutable lfWidth : int
    val mutable lfEscapement : int
    val mutable lfOrientation : int
    val mutable lfWeight : int
    val mutable lfItalic : byte
    val mutable lfUnderline : byte
    val mutable lfStrikeOut : byte
    val mutable lfCharSet : byte
    val mutable lfOutPrecision : byte
    val mutable lfClipPrecision : byte
    val mutable lfQuality : byte
    val mutable lfPitchAndFamily : byte
    [<MarshalAs(UnmanagedType.ByValTStr, SizeConst = 32)>]
    val mutable lfFaceName : string
    new() = { lfHeight = 0; lfWidth = 0; lfEscapement = 0; lfOrientation = 0; lfWeight = 0; lfItalic = 0uy; lfUnderline = 0uy; lfStrikeOut = 0uy; lfCharSet = 0uy; lfOutPrecision = 0uy; lfClipPrecision = 0uy; lfQuality = 0uy; lfPitchAndFamily = 0uy; lfFaceName = "" }

[<DllImport("imm32.dll", CharSet = CharSet.Auto)>] extern IntPtr ImmGetContext(IntPtr hWnd)
[<DllImport("imm32.dll", CharSet = CharSet.Auto)>] extern Int32  ImmSetCompositionWindow(IntPtr imContext, COMPOSITIONFORM& compForm)
[<DllImport("imm32.dll", CharSet = CharSet.Auto)>] extern Int32  ImmReleaseContext(IntPtr hWnd, IntPtr context)
[<DllImport("imm32.dll", CharSet = CharSet.Auto)>] extern Int32  ImmSetCompositionFont(IntPtr imContext, LOGFONT logFont)
[<DllImport("imm32.dll", CharSet = CharSet.Auto)>] extern Int32  ImmGetCompositionString(IntPtr hIMC, uint32 dwIndex, IntPtr lpBuf, uint32 dwBufLen)

let [<Literal>] WM_IME_STARTCOMPOSITION = 0x010D
let [<Literal>] WM_IME_ENDCOMPOSITION = 0x010E
let [<Literal>] WM_IME_COMPOSITION = 0x010F
let [<Literal>] WM_IME_NOTIFY = 0x0282
let [<Literal>] WM_IME_CHAR = 0x0286
let [<Literal>] WM_IME_REQUEST = 0x0288
let [<Literal>] WM_MOUSEACTIVATE = 0x21

// wParam of report message WM_IME_NOTIFY
let [<Literal>] IMN_CLOSESTATUSWINDOW           = 0x0001
let [<Literal>] IMN_OPENSTATUSWINDOW            = 0x0002
let [<Literal>] IMN_CHANGECANDIDATE             = 0x0003
let [<Literal>] IMN_CLOSECANDIDATE              = 0x0004
let [<Literal>] IMN_OPENCANDIDATE               = 0x0005
let [<Literal>] IMN_SETCONVERSIONMODE           = 0x0006
let [<Literal>] IMN_SETSENTENCEMODE             = 0x0007
let [<Literal>] IMN_SETOPENSTATUS               = 0x0008
let [<Literal>] IMN_SETCANDIDATEPOS             = 0x0009
let [<Literal>] IMN_SETCOMPOSITIONFONT          = 0x000A
let [<Literal>] IMN_SETCOMPOSITIONWINDOW        = 0x000B
let [<Literal>] IMN_SETSTATUSWINDOWPOS          = 0x000C
let [<Literal>] IMN_GUIDELINE                   = 0x000D
let [<Literal>] IMN_PRIVATE                     = 0x000E

// wParam of report message WM_IME_REQUEST
let [<Literal>] IMR_COMPOSITIONWINDOW           = 0x0001
let [<Literal>] IMR_CANDIDATEWINDOW             = 0x0002
let [<Literal>] IMR_COMPOSITIONFONT             = 0x0003
let [<Literal>] IMR_RECONVERTSTRING             = 0x0004
let [<Literal>] IMR_CONFIRMRECONVERTSTRING      = 0x0005
let [<Literal>] IMR_QUERYCHARPOSITION           = 0x0006
let [<Literal>] IMR_DOCUMENTFEED                = 0x0007

// parameter of WM_IME_COMPOSITION and ImmGetCompositionString
// From Windows.h
let [<Literal>] GCS_COMPREADSTR                 = 0x0001
let [<Literal>] GCS_COMPREADATTR                = 0x0002
let [<Literal>] GCS_COMPREADCLAUSE              = 0x0004
let [<Literal>] GCS_COMPSTR                     = 0x0008
let [<Literal>] GCS_COMPATTR                    = 0x0010
let [<Literal>] GCS_COMPCLAUSE                  = 0x0020
let [<Literal>] GCS_CURSORPOS                   = 0x0080
let [<Literal>] GCS_DELTASTART                  = 0x0100
let [<Literal>] GCS_RESULTREADSTR               = 0x0200
let [<Literal>] GCS_RESULTREADCLAUSE            = 0x0400
let [<Literal>] GCS_RESULTSTR                   = 0x0800
let [<Literal>] GCS_RESULTCLAUSE                = 0x1000

// From imm.h
let [<Literal>] CS_INSERTCHAR                   = 0x2000
let [<Literal>] CS_NOMOVECARET                  = 0x4000

[<DllImport("gdi32.dll", CharSet = CharSet.Unicode)>] extern bool TextOut(IntPtr hdc, int nXStart, int nYStart, string lpString, int cbString)
[<DllImport("gdi32.dll")>] extern IntPtr SelectObject(IntPtr hdc, IntPtr hgdiobj)
[<DllImport("gdi32.dll")>] extern bool DeleteObject(IntPtr hgdiobj)
[<DllImport("gdi32.dll")>] extern int SetBkMode(IntPtr hdc, int iBkMode)
[<DllImport("gdi32.dll")>] extern uint32 SetTextColor(IntPtr hdc, uint32 crColor)
[<DllImport("gdi32.dll")>] extern IntPtr CreateSolidBrush(uint32 crColor)
[<DllImport("user32.dll")>] extern int FillRect(IntPtr hDC, RECT& lprc, IntPtr hbr)
[<DllImport("gdi32.dll")>] extern IntPtr CreatePen(int fnPenStyle, int nWidth, uint32 crColor)
[<DllImport("gdi32.dll")>] extern bool MoveToEx(IntPtr hdc, int X, int Y, IntPtr lpPoint)
[<DllImport("gdi32.dll")>] extern bool LineTo(IntPtr hdc, int nXEnd, int nYEnd)

let colorref_of_color (c : System.Drawing.Color) =
    (uint32 c.B <<< 16) |||
    (uint32 c.G <<< 8) |||
    (uint32 c.R)

// From wingdi.h
let [<Literal>] TRANSPARENT = 1
let [<Literal>] OPAQUE      = 2

let string_of_imm (imm : int) =
    let mutable imm = imm
    let accu = ResizeArray<string>()
    let inc (mask : int) (name : string) =
        if imm &&& mask <> 0 then
            accu.Add(name)
            imm <- (~~~mask) &&& imm
    inc GCS_COMPREADSTR "GCS_COMPREADSTR"
    inc GCS_COMPREADATTR "GCS_COMPREADATTR"
    inc GCS_COMPREADCLAUSE "GCS_COMPREADCLAUSE"
    inc GCS_COMPSTR "GCS_COMPSTR"
    inc GCS_COMPATTR "GCS_COMPATTR"
    inc GCS_COMPCLAUSE "GCS_COMPCLAUSE"
    inc GCS_CURSORPOS "GCS_CURSORPOS"
    inc GCS_DELTASTART "GCS_DELTASTART"
    inc GCS_RESULTREADSTR "GCS_RESULTREADSTR"
    inc GCS_RESULTREADCLAUSE "GCS_RESULTREADCLAUSE"
    inc GCS_RESULTSTR "GCS_RESULTSTR"
    inc GCS_RESULTCLAUSE "GCS_RESULTCLAUSE"
    inc CS_INSERTCHAR "CS_INSERTCHAR"
    inc CS_NOMOVECARET "CS_NOMOVECARET"
    if imm <> 0 then accu.Add(sprintf "0x%X" imm)
    String.Join("|", accu)

let string_of_imn (imn : int) =
    let n = (~~~0x100) &&& imn
    (match n with
     | IMN_CHANGECANDIDATE -> "IMN_CHANGECANDIDATE"
     | IMN_CLOSECANDIDATE -> "IMN_CLOSECANDIDATE"
     | IMN_CLOSESTATUSWINDOW -> "IMN_CLOSESTATUSWINDOW"
     | IMN_GUIDELINE -> "IMN_GUIDELINE"
     | IMN_OPENCANDIDATE -> "IMN_OPENCANDIDATE"
     | IMN_OPENSTATUSWINDOW -> "IMN_OPENSTATUSWINDOW"
     | IMN_SETCANDIDATEPOS -> "IMN_SETCANDIDATEPOS"
     | IMN_SETCOMPOSITIONFONT -> "IMN_SETCOMPOSITIONFONT"
     | IMN_SETCOMPOSITIONWINDOW -> "IMN_SETCOMPOSITIONWINDOW"
     | IMN_SETCONVERSIONMODE -> "IMN_SETCONVERSIONMODE"
     | IMN_SETOPENSTATUS -> "IMN_SETOPENSTATUS"
     | IMN_SETSENTENCEMODE -> "IMN_SETSENTENCEMODE"
     | IMN_SETSTATUSWINDOWPOS -> "IMN_SETSTATUSWINDOWPOS"
     | IMN_PRIVATE -> "IMN_PRIVATE"
     | _ -> sprintf "0x%X" n) +
    (if 0x100 &&& imn <> 0 then "|0x100" else "")

let string_of_imr (imr : int) =
    match imr with
    | IMR_CANDIDATEWINDOW -> "IMR_CANDIDATEWINDOW"
    | IMR_COMPOSITIONFONT -> "IMR_COMPOSITIONFONT"
    | IMR_COMPOSITIONWINDOW -> "IMR_COMPOSITIONWINDOW"
    | IMR_CONFIRMRECONVERTSTRING -> "IMR_CONFIRMRECONVERTSTRING"
    | IMR_DOCUMENTFEED -> "IMR_DOCUMENTFEED"
    | IMR_QUERYCHARPOSITION -> "IMR_QUERYCHARPOSITION"
    | IMR_RECONVERTSTRING -> "IMR_RECONVERTSTRING"
    | _ -> sprintf "0x%X" imr
