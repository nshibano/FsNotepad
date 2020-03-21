[<AutoOpen>]
module Forms
open System
open System.Windows.Forms
open System.Drawing
open System.IO
open System.Text
open System.Collections.Generic
open System.Text.RegularExpressions
open System.Diagnostics

open FileHelper
    
type EditorState =
    | Idle
    | LeftDown // MouseMove will be accepted in this state only. (If it accept MouseMove in Idle state, selection range will be lost when miximizing the window by double click the bar.)
    | Compositioning
    
and Editor(textFileHandle : FileHelper.TextFileHandle option) as this =
    inherit Form()

    let statusHeight = 20
    let mutable linenoWidth = 40
    let leftMargin = 5
    let rightMargin = 5
    let mutable drawNewlineSymbol = false
    let mutable drawTabSymbol = false
    let mutable drawEofSymbol = false
    let mutable drawPageBoundary = true

    let textArea = new OpaqueIMEControl()
    let statusArea = new OpaqueIMEControl()
    let menu = new MenuStripWithClickThrough()
    let vScroll = new VScrollBar()
    let hScroll = new HScrollBar()
    let contextMenu = new ContextMenuStrip()
    let mutable state = EditorState.Idle
    let mutable lastlyHighlightingInitiatedContentId = -1
    let mutable hasCaret = false

    let mutable cols = 1000

    let createLayoutInfo fontSize =
        let info = DocLayoutInfo.Default 1000 fontSize
        { info with PageWidth = cols * (Doc.measure info "a") }

    let undoTree : UndoTree<Doc> =
        let info = createLayoutInfo 20
        let doc = Doc.createFromString info (match textFileHandle with Some handle -> handle.LatestText | None -> "")
        UndoTree.Create(doc)
    
    let mutable lastlySavedRevision = 0
    let mutable topRowIndex = 0
    let mutable xOffset = 0
    let mutable wheelAccu = 0
    let mutable caretXPos = 0
    let mutable textFileHandle = textFileHandle
    let mutable latestCommitDate = DateTime.MinValue

    let resetCaretXPos() =
        caretXPos <- (Doc.getCaretPoint undoTree.Get).X

    let setPos (charPos : int) =
        undoTree.Amend(Doc.setPos undoTree.Get charPos)
        resetCaretXPos()

    let setSelection (sel : Selection) =
        undoTree.Amend({ undoTree.Get with Selection = sel })
        resetCaretXPos()
            
    let maximumAmendPeriod = TimeSpan.FromSeconds(1.0)

    let commit (newDoc : Doc) (atomic : bool) =
        if atomic || undoTree.Current.Sealed || undoTree.Current.Next.Count > 0 || DateTime.Now - latestCommitDate > maximumAmendPeriod then
            latestCommitDate <- if atomic then DateTime.MinValue else DateTime.Now
            undoTree.Commit(newDoc)
        else
            undoTree.Amend(newDoc)
        resetCaretXPos()

    let beep() = System.Media.SystemSounds.Beep.Play()
    
    let upd (scrollToCaret : bool) =
        let clientRectangle = this.ClientRectangle
        
        let doc = undoTree.Get
        let layoutInfo = doc.LayoutInfo
        let lineHeight = layoutInfo.LineHeight

        linenoWidth <- 4 * (Doc.measure layoutInfo "0")
        
        textArea.Bounds <- Rectangle(0, menu.Height, clientRectangle.Width - vScroll.Width, clientRectangle.Height - menu.Height - hScroll.Height - statusHeight)
        statusArea.Bounds <- Rectangle(0, clientRectangle.Bottom - statusHeight, clientRectangle.Width, statusHeight)
        menu.Bounds <- Rectangle(0, 0, clientRectangle.Width, menu.Height)

        vScroll.Bounds <- Rectangle(clientRectangle.Right - vScroll.Width, menu.Height, vScroll.Width, clientRectangle.Height - menu.Height - hScroll.Height - statusHeight)
        vScroll.Minimum <- 0
        vScroll.Maximum <- max 0 (doc.RowCount + textArea.Height / lineHeight - 2)
        vScroll.SmallChange <- 1
        vScroll.LargeChange <- max (textArea.Height / lineHeight) 1
        if scrollToCaret then
            let caretRowIndex = Doc.getRowIndexFromCharPos doc doc.Selection.sCaretPos
            topRowIndex <- max (caretRowIndex - (max 1 textArea.Height / lineHeight) + 1) (min topRowIndex caretRowIndex)
        topRowIndex <- max 0 (min topRowIndex vScroll.MaximumValueThatCanBeReachedThroughUserInteraction)
        vScroll.Value <- topRowIndex

        hScroll.Bounds <- Rectangle(0, clientRectangle.Bottom - statusHeight - hScroll.Height, clientRectangle.Width - vScroll.Width, hScroll.Height)
        hScroll.Minimum <- 0
        hScroll.Maximum <- max 0 (doc.RowTree.RootMeasure.MaximumWidth + rightMargin)
        hScroll.SmallChange <- 1
        hScroll.LargeChange <- max 0 (textArea.Width - linenoWidth - leftMargin)
        if scrollToCaret then
            let caretXOffset = (Doc.getCaretPoint doc).X
            xOffset <- max (caretXOffset - (textArea.Width - linenoWidth - leftMargin - rightMargin)) (min xOffset caretXOffset)
        xOffset <- max 0 (min xOffset hScroll.MaximumValueThatCanBeReachedThroughUserInteraction)
        hScroll.Value <- xOffset

        let p0 = Doc.getCaretPoint doc
        let p1 = Point(linenoWidth + leftMargin + p0.X, p0.Y - lineHeight * topRowIndex)
        // caret
        if state <> EditorState.Compositioning && textArea.Focused then
            let w = 2
            Win32.CreateCaret(textArea.Handle, IntPtr.Zero, w, layoutInfo.FontSize) |> ignore
            Win32.SetCaretPos(p1.X - w/2 - xOffset, p1.Y + layoutInfo.Padding) |> ignore
            Win32.ShowCaret(textArea.Handle) |> ignore
            hasCaret <- true
        elif hasCaret then
            Win32.DestroyCaret() |> ignore
            hasCaret <- false
        
        match state with
        | EditorState.Compositioning ->
            let ime = Win32.ImmGetContext(textArea.Handle)
            let mutable compForm = Win32.COMPOSITIONFORM(dwStyle = Win32.CFS_POINT, ptCurrentPos = Win32.POINT(x = p1.X - xOffset, y = p1.Y + layoutInfo.Padding + layoutInfo.YOffset2), rcArea = Win32.RECT())
            Win32.ImmSetCompositionWindow(ime, &compForm) |> ignore
            use font = new Font("MS Gothic", float32 layoutInfo.FontSize, GraphicsUnit.Pixel)
            let logfont = Win32.LOGFONT()
            font.ToLogFont(logfont)
            Win32.ImmSetCompositionFont(ime, logfont) |> ignore
            Win32.ImmReleaseContext(textArea.Handle, ime) |> ignore
        | _ -> ()

        // redraw all
        textArea.Invalidate()
        statusArea.Invalidate()
        
        let menu_file_save = (menu.Items.[0] :?> ToolStripMenuItem).DropDownItems.[2] :?> ToolStripMenuItem
        menu_file_save.Enabled <- textFileHandle.IsSome
        
        this.Text <-
            match textFileHandle with
            | Some handle -> sprintf "%s (%s) - FsNotepad" (Path.GetFileName(handle.OriginalPath)) (Path.GetDirectoryName(handle.OriginalPath))
            | None -> "Untitled - FsNotepad"
    
    let input_upd (atomic : bool) (s : string) =
        let newDoc = Doc.replace undoTree.Get s
        commit newDoc atomic
        upd true

    let cut_upd() =
        let s = Doc.getSelectedString undoTree.Get
        if s.Length > 0 then
            Clipboard.SetText(s)
            input_upd true ""

    let copy() =
        let s = Doc.getSelectedString undoTree.Get
        if s.Length > 0 then
            Clipboard.SetText(s)

    let paste_upd() =
        input_upd true (Clipboard.GetText())

    let selectAll() =
        setSelection { sAnchorPos = 0; sCaretPos = undoTree.Get.CharCount }
    
    let selectAll_upd() =
        selectAll()
        upd false
    
    let undoUpd() =
        if undoTree.CanUndo then
            latestCommitDate <- DateTime.MinValue
            let li = undoTree.Get.LayoutInfo
            undoTree.Undo()
            undoTree.Amend(Doc.changeLayout li undoTree.Get)
            resetCaretXPos()
            upd true
        else beep()
    
    let redoUpd() =
        if undoTree.CanRedo then
            latestCommitDate <- DateTime.MinValue
            let li = undoTree.Get.LayoutInfo
            undoTree.Redo()
            undoTree.Amend(Doc.changeLayout li undoTree.Get)
            resetCaretXPos()
            upd true
        else beep()

    let getDp (p : Point) = Point(p.X - linenoWidth - leftMargin + xOffset, undoTree.Get.LayoutInfo.LineHeight * topRowIndex + p.Y)
    let getLineEnding() = match textFileHandle with None -> CRLF | Some handle -> handle.LineEnding
    let getNewlineString() = match getLineEnding() with CRLF -> "\r\n" | LF -> "\n" | CR -> "\r"
    let getEncoding() = match textFileHandle with None -> UTF8 | Some handle -> handle.TextEncoding

    let defaultKeyDown (kd : KeyDown) =
        match kd with
        | { kdKey = Kenter } ->
            input_upd true (getNewlineString())
        | { kdKey = Kspace } -> input_upd false " "
        | { kdKey = Ktab } -> input_upd false "\t"
        | { kdKey = Kback }
        | { kdKey = Kdelete } ->
            let atomic = undoTree.Get.Selection.Length <> 0
            match Doc.backDelete (kd.kdKey = Kdelete) undoTree.Get with
            | Some newDoc ->
                commit newDoc atomic
            | None -> beep()
            upd true
        | { kdKey = Kleft; kdShift = false }
        | { kdKey = Kright; kdShift = false } ->
            match Doc.leftRight (kd.kdKey = Kright) undoTree.Get with
            | Some newDoc -> undoTree.Amend newDoc
            | None -> beep()
            resetCaretXPos()
            upd true
        | { kdKey = Kleft; kdShift = true }
        | { kdKey = Kright; kdShift = true } ->
            match Doc.shiftLeftRight (kd.kdKey = Kright) undoTree.Get with
            | Some newDoc -> undoTree.Amend(newDoc)
            | None -> beep()
            resetCaretXPos()
            upd true
        | { kdKey = Kup }
        | { kdKey = Kdown } ->
            let down = (kd.kdKey = Kdown)
            let shift = kd.kdShift
            let doc = undoTree.Get
            let dp = Doc.getCaretPoint doc
            let y =
                if down then
                    let y = dp.Y + doc.LayoutInfo.LineHeight
                    if y >= doc.LayoutInfo.LineHeight * doc.RowCount then
                        None
                    else
                        Some y
                else
                    if dp.Y = 0 then None
                    else Some (dp.Y - 1)
            match y with
            | None -> beep()
            | Some y ->
                let dp = Point(caretXPos, y)
                let pos = Doc.getCharPosFromPoint doc dp
                if shift then
                    undoTree.Amend({ doc with Selection = { doc.Selection with sCaretPos = pos }})
                else
                    undoTree.Amend({ doc with Selection = { sAnchorPos = pos; sCaretPos = pos }})
            upd true
        | { kdKey = Kx; kdControl = true } ->
            cut_upd()
        | { kdKey = Kc; kdControl = true } ->
            copy()
        | { kdKey = Kv; kdControl = true } ->
            paste_upd()
        | { kdKey = Ka; kdControl = true } ->
            selectAll_upd()
        | { kdKey = Kz; kdControl = true } ->
            undoUpd()
        | { kdKey = Ky; kdControl = true }
        | { kdKey = Kz; kdShift = true; kdControl = true } ->
            redoUpd()
            upd true
        | { kdKey = Kf5 } ->
            GC.Collect()
            upd false
        | _ -> ()

    let keyPress (ev : KeyPressEventArgs) =
        if (not ev.Handled) && '\x20' < ev.KeyChar then // enter, space, tab はここで除外される
            match state with
            | Compositioning -> ()
            | _ -> input_upd false (String(ev.KeyChar, 1))
            ev.Handled <- true

    let textAreaPaint (ev : PaintEventArgs) =
        let clientRectangle = textArea.ClientRectangle
        if clientRectangle.Width > 0 && clientRectangle.Height > 0 then
            let g0 = ev.Graphics
            use buffer = BufferedGraphicsManager.Current.Allocate(g0, clientRectangle)
            let g = buffer.Graphics
            let doc = undoTree.Get
            Doc.draw
                g
                Color.White
                doc
                clientRectangle
                linenoWidth
                leftMargin
                xOffset
                drawNewlineSymbol
                drawTabSymbol
                (Doc.getRowIndexFromCharPos doc (Doc.getCharPosFromPoint doc (Point(0, doc.LayoutInfo.LineHeight * topRowIndex))))
            
            // EOF mark
            if drawEofSymbol then
                let p = Doc.getPointFromCharPos doc doc.CharCount
                let p = Point(p.X + linenoWidth + leftMargin, - doc.LayoutInfo.LineHeight * topRowIndex + p.Y + doc.LayoutInfo.Padding + doc.LayoutInfo.YOffset1)
                use eofFont = new Font("MS Gothic", float32 doc.LayoutInfo.FontSize, GraphicsUnit.Pixel)
                g.DrawString("[EOF]", eofFont, Brushes.LightGray, float32 p.X, float32 p.Y, StringFormat.GenericTypographic)
            
            // right boundary of page
            if drawPageBoundary then
                let x = linenoWidth + leftMargin + doc.LayoutInfo.PageWidth - xOffset
                g.DrawLine(Pens.LightGray, x, clientRectangle.Top, x, clientRectangle.Bottom)

            buffer.Render()

    let statusAreaPaint (ev : PaintEventArgs) =
        let g0 = ev.Graphics
        use buf = BufferedGraphicsManager.Current.Allocate(g0, statusArea.ClientRectangle)
        let g = buf.Graphics
        let doc = undoTree.Get
        use statusFont = new Font("MS Gothic", 20.0f, GraphicsUnit.Pixel)
        
        let bgColor =
            if this.TextArea.Focused
            then Color.FromArgb(0xFF505060)
            else Color.FromArgb(0xFFC0C0C0)
        
        g.Clear(bgColor)

        let posDesc =
            if doc.Selection.Length = 0 then
                sprintf "%d" doc.Selection.sCaretPos
            else
                sprintf "%d-%d" doc.Selection.sAnchorPos doc.Selection.sCaretPos

        let symbolDesc =
            if doc.Selection.sCaretPos = doc.CharCount then "EOF"
            else
                let symbol = Doc.getSymbolFromCharPos doc doc.Selection.sCaretPos
                let name =
                    match symbol with
                    | " " -> "SP"
                    | "\r" -> "CR"
                    | "\n" -> "LF"
                    | "\r\n" -> "CRLF"
                    | "\t" -> "TAB"
                    | "\uFEFF" -> "BOM"
                    | _ -> symbol
                let code =
                    match symbol with
                    | "\r\n" -> "(U+0D U+0A)"
                    | _ ->
                        let codepoint = Char.ConvertToUtf32(symbol, 0)
                        if codepoint <= 0xFF then
                            sprintf "(U+%02x)" codepoint
                        elif codepoint <= 0xFFFF then
                            sprintf "(U+%04x)" codepoint
                        else
                            sprintf "(U+%06x)" codepoint
                name + " " + code
       
        
        let leftLine =
            sprintf "r%d%s a%d b%d"
                undoTree.Current.Revision
                (if lastlySavedRevision <> undoTree.Current.Revision then "*" else "")
                undoTree.Current.RevisionsAhead
                undoTree.Current.Next.Count
        g.DrawString(leftLine, statusFont, Brushes.White, 0.f, 1.f, StringFormat.GenericTypographic)

        let centerLine = sprintf "%s %s" posDesc symbolDesc
        use sfCenter = new StringFormat(StringFormat.GenericTypographic)
        sfCenter.Alignment <- StringAlignment.Center
        g.DrawString(centerLine, statusFont, Brushes.White, float32 ((statusArea.Left + statusArea.Right) / 2), 1.f, sfCenter)

        let encoding = getEncoding()
        let rightLine =
            match encoding with
            | UTF8 | SJIS when doc.RowTree.RootMeasure.IsAsciiOnly ->
                "ASCII"
            | _ ->
                match encoding with 
                | UTF8 | UTF8BOM ->
                    sprintf "UTF8 %s %s" (match encoding with UTF8 -> "NOBOM" | UTF8BOM -> "BOM" | _ -> dontcare()) (getLineEnding().ToString())
                | _ ->
                    sprintf "%s %s" (encoding.ToString()) (getLineEnding().ToString())
        use sfRight = new StringFormat(StringFormat.GenericTypographic)
        sfRight.Alignment <- StringAlignment.Far
        g.DrawString(rightLine, statusFont, Brushes.White, float32 statusArea.Right, 1.f, sfRight)

        buf.Render()

    let mouseDown (ev : MouseEventArgs) =
        textArea.Focus() |> ignore
        if state = EditorState.Idle then
            if ev.Button.HasFlag(MouseButtons.Left) then
                let dp = getDp ev.Location
                let pos = Doc.getCharPosFromPoint undoTree.Get dp
                state <- LeftDown
                setSelection
                    { sCaretPos = pos
                      sAnchorPos =
                      if Control.ModifierKeys.HasFlag(Keys.Shift) then
                          undoTree.Get.Selection.sAnchorPos
                      else pos }
                caretXPos <- dp.X
                upd false
            else ()
        elif state = Compositioning then
            let dp = getDp ev.Location
            let doc = undoTree.Get
            let pos = Doc.getCharPosFromPoint doc dp
            setPos pos
            caretXPos <- dp.X
            upd false

    let mouseUp (ev : MouseEventArgs) =
        if ev.Button.HasFlag(MouseButtons.Left) && state = LeftDown then
            state <- EditorState.Idle
            upd false
        elif ev.Button.HasFlag(MouseButtons.Right) then
            contextMenu.Show(Cursor.Position)
    
    let mouseDoubleClick (ev : MouseEventArgs) =
        if ev.Button.HasFlag(MouseButtons.Left) then
            let dp = getDp ev.Location
            let sel = Doc.getWordSelection undoTree.Get dp
            setSelection sel
            upd false
 
    let mouseMove (ev : MouseEventArgs) =
        if state = LeftDown && ev.Button.HasFlag(MouseButtons.Left) then
            let dp = getDp ev.Location
            let doc = undoTree.Get
            let pos = Doc.getCharPosFromPoint doc dp
            setSelection { doc.Selection with sCaretPos = pos }
            caretXPos <- dp.X
            upd false

    let mouseWheel (ev : MouseEventArgs) =
        if Control.ModifierKeys.HasFlag(Keys.Control) then
            let i = Array.BinarySearch(fontSizeSeries, undoTree.Get.LayoutInfo.FontSize)
            let newSize =
                if ev.Delta > 0 then
                    fontSizeSeries.[min (i + 1) (fontSizeSeries.Length - 1)]
                else
                    fontSizeSeries.[max (i - 1) 0]

            let newLayoutInfo = createLayoutInfo newSize
            undoTree.Amend(Doc.changeLayout newLayoutInfo undoTree.Get)
            resetCaretXPos()
            upd false
        else
            wheelAccu <- wheelAccu + ev.Delta
            let scroll =
                if wheelAccu > 0 then
                    wheelAccu / 120
                else
                    -((-wheelAccu) / 120)
            wheelAccu <- wheelAccu - 120 * scroll
            topRowIndex <- topRowIndex - 3 * scroll
            upd false
        
    let ensure() =
        if (match textFileHandle with Some h -> h.LatestText | None -> "") <> Doc.getAllString undoTree.Get then
            let result = applyWithInNestedMessageLoopFlagSet (fun () -> MessageBox.Show("Save?", "FsNotepad",  MessageBoxButtons.YesNoCancel, MessageBoxIcon.None)) ()
            match result with
            | DialogResult.Cancel -> false
            | DialogResult.No -> true
            | DialogResult.Yes ->
                let text = Doc.getAllString undoTree.Get
                match textFileHandle with
                | Some handle ->
                    match FileHelper.trySave handle text with
                    | TrySaveResult.TSRsuccess -> true
                    | TrySaveResult.TSRfialed exn ->
                        applyWithInNestedMessageLoopFlagSet (fun () -> MessageBox.Show(exn.Message) |> ignore) ()
                        false
                | None ->
                    let text = Doc.getAllString undoTree.Get
                    match FileHelper.trySaveAs None text with
                    | TrySaveAsResult.TSARsuccess handle ->
                        textFileHandle <- Some handle
                        true
                    | TrySaveAsResult.TSARcancelled -> false
                    | TrySaveAsResult.TSARfailed exn ->
                        applyWithInNestedMessageLoopFlagSet (fun () -> MessageBox.Show(exn.Message) |> ignore) ()
                        false
            | _ -> dontcare()
        else true

    let new_upd() =
        if ensure() then
            undoTree.Clear(Doc.create undoTree.Get.LayoutInfo)
            caretXPos <- 0
            textFileHandle <- None
        upd false
    
    let openPath (path : string) =
        if ensure() then

            Option.iter (fun (h : TextFileHandle) -> h.FileStream.Close()) textFileHandle
            textFileHandle <- None

            match FileHelper.tryOpenPath path with
            | TOPRsuccess f ->
                undoTree.Clear(Doc.createFromString undoTree.Get.LayoutInfo f.LatestText)
                textFileHandle <- Some f
                topRowIndex <- 0
                lastlySavedRevision <- 0
                resetCaretXPos()
                upd false
            | TOPRfailed exn ->
                applyWithInNestedMessageLoopFlagSet (fun () -> MessageBox.Show(exn.Message) |> ignore) ()
        
    let open_upd() =
        if ensure() then
            match FileHelper.tryOpen textFileHandle with
            | TryOpenResult.TORcancelled -> ()
            | TryOpenResult.TORfailed exn ->
                applyWithInNestedMessageLoopFlagSet (fun () -> MessageBox.Show(exn.Message) |> ignore) ()
                match textFileHandle with
                | Some h ->
                    h.FileStream.Close()
                    textFileHandle <- None
                | None -> ()
            | TryOpenResult.TORsuccess f ->
                undoTree.Clear(Doc.createFromString undoTree.Get.LayoutInfo f.LatestText)
                textFileHandle <- Some f
                topRowIndex <- 0
                lastlySavedRevision <- 0
                resetCaretXPos()
        upd false
    
    let saveas_upd() =
        let text = Doc.getAllString undoTree.Get
        match FileHelper.trySaveAs textFileHandle text with
        | TrySaveAsResult.TSARsuccess handle ->
            undoTree.Current.Sealed <- true
            textFileHandle <- Some handle
            lastlySavedRevision <- undoTree.Current.Revision
        | TrySaveAsResult.TSARcancelled -> ()
        | TrySaveAsResult.TSARfailed exn ->
            textFileHandle <- None
            applyWithInNestedMessageLoopFlagSet (fun () -> MessageBox.Show(exn.Message) |> ignore) ()
        upd false
    
    let save_upd() =
        let text = Doc.getAllString undoTree.Get
        match FileHelper.trySave textFileHandle.Value text with
        | FileHelper.TrySaveResult.TSRsuccess ->
            undoTree.Current.Sealed <- true
            lastlySavedRevision <- undoTree.Current.Revision
        | FileHelper.TrySaveResult.TSRfialed exn ->
            applyWithInNestedMessageLoopFlagSet (fun () -> MessageBox.Show(exn.Message) |> ignore) ()
        upd false

    do  
        this.Icon <- new Icon(System.Reflection.Assembly.GetExecutingAssembly().GetManifestResourceStream("FsNotepad.TextFileIcon.ico"))
        vScroll.Parent <- this
        vScroll.Scroll.Add(fun ev ->
            topRowIndex <- ev.NewValue
            upd false)
        
        hScroll.Parent <- this
        hScroll.Scroll.Add(fun ev ->
            xOffset <- ev.NewValue
            upd false)

        textArea.KeyDown.Add(fun ev ->
            match KeyDown.CreateFromKeyData(ev.KeyData) with
            | Some kd ->
                defaultKeyDown kd
            | None -> ()
            ev.Handled <- true)
        textArea.KeyPress.Add(keyPress)
        textArea.Paint.Add(textAreaPaint)
        textArea.GotFocus.Add(fun ev ->
            //repl.SelectedEditor <- Some this
            upd false)
        textArea.LostFocus.Add(fun ev ->
            upd false)
        this.Resize.Add(fun ev -> upd false)
        textArea.MouseDown.Add(mouseDown)
        textArea.MouseUp.Add(mouseUp)
        textArea.MouseDoubleClick.Add(mouseDoubleClick)
        textArea.MouseMove.Add(mouseMove)
        textArea.MouseWheel.Add(mouseWheel)
        textArea.ImeStartComposition.Add(fun ev ->
            state <- EditorState.Compositioning
            upd false)
        textArea.ImeEndComposition.Add(fun ev ->
            state <- EditorState.Idle
            upd false)
        textArea.ImeResultStr.Add(fun ev ->
            input_upd false ev.ResultStr)
        
        statusArea.Paint.Add(statusAreaPaint)

        menu.Items.Add(
            new ToolStripMenuItem("File", null,
                new ToolStripMenuItem("New", null, fun o ev -> new_upd()),
                new ToolStripMenuItem("Open", null, fun o ev -> open_upd()),
                new ToolStripMenuItem("Save", null, (fun o ev -> save_upd())),
                new ToolStripMenuItem("Save As", null, fun o ev -> saveas_upd()))) |> ignore

        contextMenu.Items.Add("Cut", null, fun o e -> cut_upd()) |> ignore
        contextMenu.Items.Add("Copy", null, fun o e -> copy()) |> ignore
        contextMenu.Items.Add("Paste", null, fun o e -> paste_upd()) |> ignore
        contextMenu.Items.Add(new ToolStripSeparator()) |> ignore

        this.ClientSize <- Size(800, 450)
        textArea.Parent <- this

        statusArea.Parent <- this

        menu.Parent <- this
        menu.GotFocus.Add(fun ev -> textArea.Focus() |> ignore)

        this.AllowDrop <- true
        this.DragEnter.Add(fun ev ->
            if ev.Data.GetDataPresent(DataFormats.FileDrop) then
                ev.Effect <- DragDropEffects.Copy
            else
                ev.Effect <- DragDropEffects.None)
        this.DragDrop.Add(fun ev ->
            let paths = ev.Data.GetData(DataFormats.FileDrop, false) :?> string array
            openPath paths.[0])

        upd false
        resetCaretXPos()

    member this.SetRevisionSealed() = undoTree.Current.Sealed <- true
    member this.LastlyHighlightingInitiatedContentId with get() = lastlyHighlightingInitiatedContentId and set x = lastlyHighlightingInitiatedContentId <- x
    member this.TextArea : OpaqueIMEControl = textArea
    member this.Doc = undoTree.Get
    member this.Input atomic s = input_upd atomic s
    member this.Amend newDoc = undoTree.Amend(newDoc)
    member this.DefaultKeyDown e = defaultKeyDown e
    member this.GetNewlineString() = getNewlineString()
    member this.EditorText
        with get() = Doc.getAllString undoTree.Get
        and set s =
            selectAll()
            input_upd true s
    
    member this.TextFileHandle
        with get() = textFileHandle
        and set x = textFileHandle <- x
    member this.OpenPath path = openPath path
    override this.OnFormClosing(ev) =
        let ensured = ensure()
        if not ensured then
            ev.Cancel <- true
        base.OnFormClosing(ev)
    
    override this.OnFormClosed(ev) =
        match this.TextFileHandle with
        | Some handle ->
            if not (isNull handle.FileStream) then
                handle.FileStream.Dispose()
        | None -> ()
        base.OnFormClosed(ev)