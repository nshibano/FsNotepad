[<AutoOpen>]
module ClickThrough

open System.Windows.Forms

type ToolStripWithClickThrough() =
    inherit ToolStrip()

    override this.WndProc(m : Message byref) =
        
        if m.Msg = Win32.WM_MOUSEACTIVATE && this.CanFocus && not this.Focused then
            this.Focus() |> ignore
        
        base.WndProc(&m)

type MenuStripWithClickThrough() =
    inherit MenuStrip()

    override this.WndProc(m : Message byref) =
        
        if m.Msg = Win32.WM_MOUSEACTIVATE && this.CanFocus && not this.Focused then
            this.Focus() |> ignore
        
        base.WndProc(&m)