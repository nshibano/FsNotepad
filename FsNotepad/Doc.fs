[<AutoOpen>]
module Doc
#nowarn "9"

open System
open System.Drawing
open System.Collections.Generic
open System.Text
open System.Diagnostics

type ColorInfo =
    { ciForeColor : Color option
      ciBackColor : Color option
      ciUnderlineColor : Color option
      ciText : string option }

// Terminology
//
// Char : System.Char type value.
//
// Symbol : An item edited in text editor. It consists of 1 or 2 char. 2-char symbols are either of "\r\n" or surrogate pair.
//          Press Left/right key will skip one symbol, and press backspace/delete key will remove one symbol.
//
// Row : Sequence of symbols displayed horizontally on the editor, or one may call it "visual line". A line contain 1 or more rows.
//
// Line : Sequence of symbols in the text which ends with newline symbols ("\r\n", "\r" or "\n"), or ends when there is no more symbols in the file.
//        The last newline symbol of the line is part of the content of the line.
//        If a text ends with newline symbol, the final line of the text is empty line (a line that contains zero symbols). 
//        Empty text contains one empty line.
//
// Index : An integer number that specifies item in a sequence. When sequence have n items, valid range of index is [0, n).
//
// Pos : Abbreviation of 'position'. An integer number that specifies position in sequence of items. When sequence have n items, valid range of pos is [0, n].
//       Pos 0 is the position before sequence start, or position before the first item.
//       Pos n is the position after end of the sequence, of position after the last item.
//       Pos i is the position before the i-th item, or the position after (i-1)-th item.

type Row =
    { String : string
      Colors : ColorInfo array // row.String.Length = row.Colors.Length
      CharOffsets : int array
      XOffsets : int array
      IsEndOfLine : bool }
    
    member row.SymbolCount = row.CharOffsets.Length - 1
    member row.GetSymbol(symbolIndex : int) =
        let ofs = row.CharOffsets.[symbolIndex]
        let len = row.CharOffsets.[symbolIndex + 1] - ofs
        row.String.Substring(ofs, len)
    member row.Width = row.XOffsets.[row.XOffsets.Length - 1]

type RowTreeInfo =
    { CharCount : int
      EndOfLineCount : int
      MaximumWidth : int }

type FontSpec =
    | Ja_Consolas_MSGothic

type DocLayoutInfo =
    { FontSpec : FontSpec
      FontSize: int
      PageWidth : int
      TabWidthInSpaces : int
      Padding : int
      YOffset1 : int
      YOffset2 : int }
    
    member x.LineHeight = x.FontSize + 2 * x.Padding

    static member Default pageWidth fontSize =
        let padding = fontSize / 8
        {
            FontSpec = Ja_Consolas_MSGothic
            FontSize = fontSize
            PageWidth = pageWidth
            TabWidthInSpaces = 8
            Padding = padding
            YOffset1 = int (Math.Round(- float fontSize / 10.0))
            YOffset2 = 0
        }          

type RowTree = MeasuredTreeList<Row, RowTreeInfo>
type RowTreeNode = MeasuredTreeListNode<Row, RowTreeInfo>

type Range =
    { rBegin : int
      rEnd : int }
    
    member r.Length = r.rEnd - r.rBegin
    member r.Contains x = r.rBegin <= x && x < r.rEnd
    member r.Clip x =
        if r.Length = 0 then dontcare()
        if x < r.rBegin then r.rBegin
        elif r.rEnd <= x then r.rEnd - 1
        else x

type Selection =
    { sCaretPos : int
      sAnchorPos : int }

    member x.Length =
        if x.sAnchorPos <= x.sCaretPos then
            x.sCaretPos - x.sAnchorPos
        else
            x.sAnchorPos - x.sCaretPos

    member x.ToRange() =
        if x.sCaretPos < x.sAnchorPos then
            { rBegin = x.sCaretPos; rEnd = x.sAnchorPos }
        else
            { rBegin = x.sAnchorPos; rEnd = x.sCaretPos }

type Doc =
    { LayoutInfo : DocLayoutInfo
      RowTree : RowTree
      Selection : Selection
      ContentId : int }
    
    member this.CharCount = this.RowTree.RootMeasure.CharCount
    member this.HasTrailingEmptyLine = this.RowTree.Count = 0 || this.RowTree.[this.RowTree.Count - 1].IsEndOfLine
    member this.RowCount = this.RowTree.Count + (if this.HasTrailingEmptyLine then 1 else 0)
    member this.LineCount = this.RowTree.RootMeasure.EndOfLineCount + 1

type Char_ColorInfo =
    struct
        val mutable Char : char
        val mutable ColorInfo : ColorInfo
        new(char : Char, colorInfo : ColorInfo) = { Char = char; ColorInfo = colorInfo }
    end
    
type Buf() =
    let mutable start = 0
    let mutable count = 0
    let mutable storage : Char_ColorInfo array = [||]

    member this.Count = count
    member this.Item with get i = storage.[start + i]

    member this.RemoveHead(n) =
        start <- start + n
        count <- count - n

    member this.AddRange(s : string, colors : ColorInfo array) =
        if start + count + s.Length <= storage.Length then
            for i = 0 to s.Length - 1 do
                let ofs = start + count + i
                storage.[ofs].Char <- s.[i]
                storage.[ofs].ColorInfo <- colors.[i]
            count <- count + s.Length
        else
            let newCount = count + s.Length
            /// raises InvalidOperatonException when not found
            let find_next_capacity_exn maxlen needed =
                if needed = 0 then 0
                elif needed <= maxlen && needed <= (1 <<< 30) then
                    let mutable n = 1
                    while n < needed do
                        n <- 2 * n
                    min maxlen n
                else raise (InvalidOperationException())
            let newCapacity = find_next_capacity_exn Int32.MaxValue newCount
            let newStorage = Array.zeroCreate<Char_ColorInfo> newCapacity
            Array.blit storage start newStorage 0 count
            for i = 0 to s.Length - 1 do
                let ofs = count + i
                newStorage.[ofs].Char <- s.[i]
                newStorage.[ofs].ColorInfo <- colors.[i]
            start <- 0
            count <- newCount
            storage <- newStorage
    
    member this.ReplaceRange(replBegin : int, replCount : int, replacement : string, colorInfo : ColorInfo) =
        let newCount = count - replCount + replacement.Length
        let newStorage = Array.zeroCreate<Char_ColorInfo> newCount
        for i = 0 to replBegin - 1 do
            newStorage.[i] <- storage.[start + i]
        for i = 0 to replacement.Length - 1 do
            let ofs = replBegin + i
            newStorage.[ofs].Char <- replacement.[i]
            newStorage.[ofs].ColorInfo <- colorInfo
        for i = 0 to count - replBegin - replCount - 1 do
            newStorage.[replBegin + replacement.Length + i] <- storage.[start + replBegin + replCount + i]
        start <- 0
        count <- newCount
        storage <- newStorage

[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Doc =
    
    let intArrayCache = Dictionary<int array, int array>(HashIdentity.Structural)
    let intArrayIntern (x : int array) =
        match intArrayCache.TryGetValue(x) with
        | true, y -> y
        | false, _ ->
            intArrayCache.Add(x, x)
            x

    let ColorInfo_Default = { ciForeColor = None; ciBackColor = None; ciUnderlineColor = None; ciText = None }
    let clearColorInfoArrays = List<ColorInfo array>()

    let colorInfoArrayIsClear (ary : ColorInfo array) =
        Array.forall (fun x -> LanguagePrimitives.PhysicalEquality x ColorInfo_Default) ary

    let clearColorInfoArrayCreate len =
        while clearColorInfoArrays.Count <= len do
            clearColorInfoArrays.Add(Array.create clearColorInfoArrays.Count ColorInfo_Default)
        clearColorInfoArrays.[len]
    
    let colorInfoArrayIntern (ary : ColorInfo array) =
        if colorInfoArrayIsClear ary then
            clearColorInfoArrayCreate ary.Length
        else ary

    let RowTreeInfo_Zero = { CharCount = 0; EndOfLineCount = 0; MaximumWidth = 0 }
    let Selection_Zero = { sCaretPos = 0; sAnchorPos = 0 }

    /// Returns index of the table item which is equal to the key.
    /// If not found, returns index of the highest item of the items which is lower than the key.
    /// If there is no item which is lower than the key, returns -1.
    /// Therefore, -1 <= returnValue <= table.Length - 1.
    /// The table must be sorted in ascending order.
    let binchopFloor (table : int array) (key : int) =
        
        let rec loop i j =
            if j - i = 1 then
                if table.[i] <= key then
                    i
                else
                    i - 1
            else
                let k = i + (j - i) / 2
                match compare key table.[k] with
                | 0 -> k
                | -1 -> loop i k
                | 1 -> loop k j
                | _ -> dontcare()

        if table.Length = 0 then -1
        else loop 0 table.Length

    /// Returns index of the table item which is equal to the key.
    /// If not found, returns index of the lowest item of the items which is higher than the key.
    /// If there is no item which is higher than the key, returns table.Length.
    /// Therefore, 0 <= returnValue <= table.Length.
    /// The table must be sorted in ascending order.
    let binchopCeil (table : int array) (key : int) =

        if table.Length = 0 then raise (ArgumentException())
        
        let rec loop i j =
            if j - i = 1 then
                if key <= table.[i] then
                    i
                else
                    i + 1
            else
                let k = i + (j - i) / 2
                match compare key table.[k] with
                | 0 -> k
                | -1 -> loop i k
                | 1 -> loop k j
                | _ -> dontcare()

        if table.Length = 0 then table.Length
        else loop 0 table.Length
    
    let symbolIsNewline (symbol : string) =
        let c0 = symbol.[0]
        c0 = '\r' || c0 = '\n'

    let symbolIsSpaceOrTab (symbol : string) =
        symbol = " " || symbol = "\t"

    let symbolIsAscii (symbol : string) =
        symbol.Length = 1 &&
        let c = symbol.[0]
        '\x00' <= c && c <= '\xFF'
    
    //let symbolIsHalfwidth (symbol : string) =
    //    symbol.Length = 1 &&
    //    let c = symbol.[0]
    //    ('\u0020' <= c && c <= '\u007E') || // ascii
    //    ('\uFF66' <= c && c <= '\uFF9F') // 半角カナ

    let dummy_bmp = new System.Drawing.Bitmap(1, 1)
    let dummy_g = Graphics.FromImage(dummy_bmp)
    let measure_cache = Dictionary<string, int>()
    let mutable latest_info = DocLayoutInfo.Default 800 20
    let createFont (info : DocLayoutInfo) =
        match info.FontSpec with
        | Ja_Consolas_MSGothic ->
            (new Font("Consolas", float32 info.FontSize, GraphicsUnit.Pixel),
             new Font("MS Gothic", float32 info.FontSize, GraphicsUnit.Pixel))
    let mutable latest_font = createFont latest_info
    let measure (info : DocLayoutInfo) (symbol : string) =
        if not (LanguagePrimitives.PhysicalEquality info latest_info) then
            latest_info <- info
            latest_font <- createFont info
            measure_cache.Clear()
        match measure_cache.TryGetValue(symbol) with
        | true, w -> w
        | false, _ ->
            let symbol =
                match symbol with
                | "\r\n"
                | "\n"
                | "\r"
                | " " -> "a"
                | "　" -> "あ"
                | _ -> symbol
            let font1, font2 = latest_font
            let font = if symbolIsAscii symbol then font1 else font2
            let w = int (round (dummy_g.MeasureString(symbol, font, PointF(0.0f, 0.0f), StringFormat.GenericTypographic).Width))
            let w = max w (info.FontSize / 10)
            measure_cache.[symbol] <- w
            w
    
    let Row_empty =
        { String = ""
          Colors = [||]
          CharOffsets = [| 0 |]
          XOffsets = [| 0 |]
          IsEndOfLine = false }
    
    let rowTreeFunc (left : RowTreeInfo) (row : Row) (right : RowTreeInfo) =
        { CharCount = left.CharCount + row.String.Length + right.CharCount
          EndOfLineCount = left.EndOfLineCount + (if row.IsEndOfLine then 1 else 0) + right.EndOfLineCount
          MaximumWidth = max (max left.MaximumWidth row.Width) right.MaximumWidth }
    
    let mutable contentIdTop = 0
    let contentIdNew() =
        let newId = contentIdTop
        contentIdTop <- contentIdTop + 1
        newId
   
    let create info =
        { LayoutInfo = info
          RowTree = MeasuredTreeList<Row, RowTreeInfo>(rowTreeFunc, RowTreeInfo_Zero)
          Selection = { sCaretPos = 0; sAnchorPos = 0 }
          ContentId = contentIdNew() }

    let getRow (doc : Doc) rowIndex =
        if 0 <= rowIndex && rowIndex < doc.RowTree.Count then
            let bgn = doc.RowTree.MeasureRange(0, rowIndex).CharCount
            let row = doc.RowTree.[rowIndex]
            row, { rBegin = bgn; rEnd = bgn + row.String.Length }
        elif rowIndex = doc.RowTree.Count && doc.HasTrailingEmptyLine then
            let bgn = doc.RowTree.RootMeasure.CharCount
            Row_empty, { rBegin = bgn; rEnd = bgn }
        else raise (IndexOutOfRangeException())

    let getRowIndexFromCharPos (doc : Doc) charPos =
        if 0 <= charPos && charPos < doc.CharCount then
            let rec loop upleftRowCount node pos =
                match node with
                | Node (Left = left; Value = value; Right = right) ->
                    let leftCharCount = doc.RowTree.MeasureOf(left).CharCount
                    if pos < leftCharCount then
                        loop upleftRowCount left pos
                    elif pos < leftCharCount + value.String.Length then
                        upleftRowCount + left.Count
                    else
                        loop (upleftRowCount + left.Count + 1) right (pos - leftCharCount - value.String.Length)
                | Nil -> dontcare()
            loop 0 doc.RowTree.Root charPos
        elif charPos = doc.CharCount then
            if doc.HasTrailingEmptyLine then
                doc.RowTree.Count
            else
                doc.RowTree.Count - 1
        else raise (IndexOutOfRangeException())
    
    let rec getRowIndexOfIthEolRowLoop (doc : Doc) upleftRowCount (rowTree : RowTreeNode)  i =
        match rowTree with
        | Node (Left = left; Value = value; Right = right; Measure = measure) ->
            let leftEolCount = doc.RowTree.MeasureOf(left).EndOfLineCount
            if i < leftEolCount then
                getRowIndexOfIthEolRowLoop doc upleftRowCount left i
            elif i = leftEolCount && value.IsEndOfLine then
                upleftRowCount + left.Count
            else
                getRowIndexOfIthEolRowLoop doc (upleftRowCount + left.Count + 1) right (i - leftEolCount - (if value.IsEndOfLine then 1 else 0))
        | Nil -> dontcare()

    let getLineIndexFromCharPos (doc : Doc) charPos =
        if 0 <= charPos && charPos < doc.CharCount then
            let rec loop upleftEolCount node pos =
                match node with
                | Node (Left = left; Value = value; Right = right) ->
                    let leftMeasure = doc.RowTree.MeasureOf(left)
                    if pos < leftMeasure.CharCount then
                        loop upleftEolCount left pos
                    elif pos < leftMeasure.CharCount + value.String.Length then
                        upleftEolCount + leftMeasure.EndOfLineCount
                    else
                        loop (upleftEolCount + leftMeasure.EndOfLineCount + (if value.IsEndOfLine then 1 else 0)) right (pos - leftMeasure.CharCount - value.String.Length)
                | Nil -> dontcare()
            loop 0 doc.RowTree.Root charPos
        elif charPos = doc.CharCount then
            doc.RowTree.RootMeasure.EndOfLineCount
        else raise (IndexOutOfRangeException()) 
    
    let getLineRange (doc : Doc) lineIndex =
        if lineIndex < doc.LineCount then
            let bgn =
                if lineIndex = 0 then
                    0
                else
                    let rowIndex = getRowIndexOfIthEolRowLoop doc 0 doc.RowTree.Root (lineIndex - 1)
                    let _, rowRange = getRow doc rowIndex
                    rowRange.rEnd
            let ed =
                if lineIndex = doc.LineCount - 1 then
                    doc.CharCount
                else
                    let rowIndex = getRowIndexOfIthEolRowLoop doc 0 doc.RowTree.Root lineIndex
                    let _, rowRange = getRow doc rowIndex
                    rowRange.rEnd
            { rBegin = bgn; rEnd = ed }
        else raise (IndexOutOfRangeException())
    
    let validateCharPos (doc : Doc) (rightNotLeft : bool) charPos =
        if charPos < 0 then 0
        elif doc.CharCount < charPos then doc.CharCount
        else
            let rowIndex = getRowIndexFromCharPos doc charPos
            let row, rowRange = getRow doc rowIndex
            let binchop = if rightNotLeft then binchopCeil else binchopFloor
            let i = binchop row.CharOffsets (charPos - rowRange.rBegin)
            rowRange.rBegin + row.CharOffsets.[i]

    let setPos (doc : Doc) pos =
        { doc with Selection = { sAnchorPos = pos; sCaretPos = pos }}

    let getChar (doc : Doc) charIndex =
        if 0 <= charIndex && charIndex < doc.CharCount then
            let rowIndex = getRowIndexFromCharPos doc charIndex
            let row, _ = getRow doc rowIndex
            let leftCharCount = doc.RowTree.MeasureRange(0, rowIndex).CharCount
            row.String.[charIndex - leftCharCount]
        else raise (IndexOutOfRangeException())

    let getColorInfo (doc : Doc) charIndex =
        if 0 <= charIndex && charIndex < doc.CharCount then
            let rowIndex = getRowIndexFromCharPos doc charIndex
            let row, rowRange = getRow doc rowIndex
            row.Colors.[charIndex - rowRange.rBegin]
        else raise (IndexOutOfRangeException())
    
    let replace (doc : Doc) (replacement : string) =

        let charRange = doc.Selection.ToRange()

        let deconsrBeginCharPos =
            if (0 < charRange.rBegin &&
                let c = getChar doc (charRange.rBegin - 1)
                c = '\r' || Char.IsHighSurrogate(c))
            then charRange.rBegin - 1
            else charRange.rBegin
        
        let deconstrEndCharPos =
            if (charRange.rEnd < doc.CharCount &&
                let c = getChar doc charRange.rEnd
                c = '\n' || Char.IsLowSurrogate(c))
            then charRange.rEnd + 1
            else charRange.rEnd
        
        let deconstrRowIndexBegin = getRowIndexFromCharPos doc deconsrBeginCharPos
        let deconstrRowIndexEnd = getRowIndexFromCharPos doc deconstrEndCharPos + 1
        
        let mutable head = doc.RowTree.TakeFirst(deconstrRowIndexBegin)
        let mutable tail = doc.RowTree.TakeLast(max 0 (doc.RowTree.Count - deconstrRowIndexEnd))
        
        let buf = Buf()
        for rowIndex = deconstrRowIndexBegin to deconstrRowIndexEnd - 1 do
            let row = fst (getRow doc rowIndex)
            buf.AddRange(row.String, row.Colors)
        
        let ci =
            let i = charRange.rBegin - 1
            if 0 <= i && i < doc.CharCount then
                let rowIndex = getRowIndexFromCharPos doc i
                let row, rowRange = getRow doc rowIndex
                row.Colors.[i - rowRange.rBegin]
            else ColorInfo_Default

        buf.ReplaceRange((charRange.rBegin - head.RootMeasure.CharCount), (charRange.Length), replacement, ci)
        let charCount = head.RootMeasure.CharCount + buf.Count + tail.RootMeasure.CharCount

        let rec rowTree_charAt_loop (tree : RowTree) (node : RowTreeNode) i =
            match node with
            | Node (Left = left; Value = value; Right = right) ->
                let leftCharCount = tree.MeasureOf(left).CharCount
                if i < leftCharCount then
                    rowTree_charAt_loop tree left i
                elif i < leftCharCount + value.String.Length then
                    let ofs = i - leftCharCount
                    Char_ColorInfo(value.String.[ofs], value.Colors.[ofs])
                else
                    rowTree_charAt_loop tree right (i - leftCharCount - value.String.Length)
            | Nil -> dontcare()
        
        let charAt i =
            let headCount = head.RootMeasure.CharCount
            let bufferCount = buf.Count
            if i < headCount then
                rowTree_charAt_loop head head.Root i
            elif i < headCount + bufferCount then
                buf.[i - headCount]
            else
                rowTree_charAt_loop tail tail.Root (i - headCount - bufferCount)
        
        let tabWidthInPixels = doc.LayoutInfo.TabWidthInSpaces * measure doc.LayoutInfo " "
        
        while buf.Count > 0 do

            let scanStart = head.RootMeasure.CharCount

            let mutable charPos = 0            
            let mutable xOffset = 0
            let charPoss = List<int>()
            let xOffsets = List<int>()

            let add (symbol : string) (nextXOffset : int) =
                charPoss.Add(charPos)
                xOffsets.Add(xOffset)
                charPos <- charPos + symbol.Length
                xOffset <- nextXOffset
            
            let mutable cont = true
            while cont && scanStart + charPos < charCount do

                let symbol =
                    let c0 = (charAt (scanStart + charPos)).Char
                    if (c0 = '\r' || Char.IsHighSurrogate(c0)) && scanStart + charPos + 1 < charCount then
                        let c1 = (charAt (scanStart + charPos + 1)).Char
                        if (c0 = '\r' && c1 = '\n') || Char.IsSurrogatePair(c0, c1) then
                            String([| c0; c1 |])
                        else String(c0, 1)
                    else String(c0, 1)
                
                match symbol with
                | "\t" ->
                    let xOffsetAfterAdd = ((xOffset / tabWidthInPixels + 1) * tabWidthInPixels)
                    if xOffsetAfterAdd <= doc.LayoutInfo.PageWidth then
                        add symbol xOffsetAfterAdd
                    else cont <- false                     
                | _ ->                  
                    let xOffsetAfterAdd = xOffset + measure doc.LayoutInfo symbol
                    if xOffsetAfterAdd <= doc.LayoutInfo.PageWidth then
                        add symbol xOffsetAfterAdd
                        if symbol.[0] = '\r' || symbol.[0] = '\n' then
                            cont <- false
                    else cont <- false
                
            charPoss.Add(charPos)
            xOffsets.Add(xOffset)

            let ary = Array.init (charPoss.[charPoss.Count - 1] - charPoss.[0]) (fun i -> charAt (scanStart + i))
            let rowString = String(Array.map (fun (x : Char_ColorInfo) -> x.Char) ary)
            let colors = Array.map (fun (x : Char_ColorInfo) -> x.ColorInfo) ary
            let isEol =
                rowString.Length > 0 &&
                let c = rowString.[rowString.Length - 1]
                c = '\r' || c = '\n'

            let row =
                { String = rowString
                  Colors = colorInfoArrayIntern colors
                  CharOffsets = intArrayIntern(charPoss.ToArray())
                  XOffsets = intArrayIntern (xOffsets.ToArray())
                  IsEndOfLine = isEol }
            head <- head.Add(row)

            while buf.Count < rowString.Length do
                let tail0 = tail.[0]
                buf.AddRange(tail0.String, tail0.Colors)
                tail <- tail.RemoveAt(0)
                
            buf.RemoveHead(rowString.Length)

        let doc = { doc with RowTree = head.AddRange(tail); ContentId = contentIdNew() }
        let newPos = validateCharPos doc true (charRange.rBegin + replacement.Length)
        { doc with Selection = { sAnchorPos = newPos; sCaretPos = newPos }}
    
    let createFromString info (s : string) = replace (create info) s

    let getSymbolFromCharPos (doc : Doc) (charPos : int) =
        let rowIndex = getRowIndexFromCharPos doc charPos
        let row, rowRange = getRow doc rowIndex
        let symbolIndexInRow = binchopFloor row.CharOffsets (charPos - rowRange.rBegin)
        let ofs = row.CharOffsets.[symbolIndexInRow]
        let len = row.CharOffsets.[symbolIndexInRow + 1] - row.CharOffsets.[symbolIndexInRow]
        row.String.Substring(ofs, len)
    
    let getSelectedString (doc : Doc) =
        let selectedRange = doc.Selection.ToRange()
        let rowIndexBegin = getRowIndexFromCharPos doc selectedRange.rBegin
        let rowIndexEnd = getRowIndexFromCharPos doc selectedRange.rEnd
        let sb = StringBuilder(selectedRange.Length)
        for rowIndex = rowIndexBegin to rowIndexEnd do
            let row, rowRange = getRow doc rowIndex
            let bgn = max rowRange.rBegin selectedRange.rBegin
            let ed = min rowRange.rEnd selectedRange.rEnd
            sb.Append(row.String, bgn - rowRange.rBegin, ed - bgn) |> ignore
        sb.ToString()

    let getAllString (doc : Doc) = String.Concat(Array.map (fun (row : Row) -> row.String) (doc.RowTree.ToArray()))

    let getPointFromCharPos (doc : Doc) (charPos : int) =
        let rowIndex = getRowIndexFromCharPos doc charPos
        let y = doc.LayoutInfo.LineHeight * rowIndex
        let row, rowRange = getRow doc rowIndex
        let symbolIndex = binchopFloor row.CharOffsets (charPos - rowRange.rBegin)
        let x = row.XOffsets.[symbolIndex]
        Point(x, y)

    let getCaretPoint (doc : Doc) = getPointFromCharPos doc doc.Selection.sCaretPos

    let getCharIndexFromPoint (doc : Doc) (p : Point) =
        let rowIndex = { rBegin = 0; rEnd = doc.RowCount }.Clip(p.Y / doc.LayoutInfo.LineHeight)
        let row, rowRange = getRow doc rowIndex
        let i = binchopFloor row.XOffsets p.X
        if i = -1 || i = row.XOffsets.Length - 1 then
            None
        else
            let charIndex =rowRange.rBegin + row.CharOffsets.[i]
            if not (0 <= charIndex && charIndex < doc.CharCount) then dontcare()
            Some charIndex
    
    let getCharPosFromPoint (doc : Doc) (p : Point) =
        let rowIndex = { rBegin = 0; rEnd = doc.RowCount }.Clip(p.Y / doc.LayoutInfo.LineHeight)
        let row, rowRange = getRow doc rowIndex
        let symbolPosInRow =
            let i = binchopFloor row.XOffsets p.X
            if i = -1 then 0 else i

        // if the point is on right half of bounding rectangle of the symbol, it is pointing next symbol.
        let symbolPosInRow =
            if symbolPosInRow < row.SymbolCount then
                let w = row.XOffsets.[symbolPosInRow + 1] - row.XOffsets.[symbolPosInRow]
                if row.XOffsets.[symbolPosInRow] + w / 2 <= p.X then
                    symbolPosInRow + 1
                else symbolPosInRow
            else symbolPosInRow
        
        // if the row contains newline symbol and the point is pointing the next of it, move to previous of it.
        let symbolPosInRow =
            if row.IsEndOfLine && symbolPosInRow = row.SymbolCount then
                symbolPosInRow - 1
            else symbolPosInRow
        
        rowRange.rBegin + row.CharOffsets.[symbolPosInRow]
    
    let getWordSelection (doc : Doc) (p : Point) =
        if doc.CharCount > 0 then
            let rowIndex = { rBegin = 0; rEnd = doc.RowCount }.Clip(p.Y / doc.LayoutInfo.LineHeight)
            let row, rowRange = getRow doc rowIndex
            if rowRange.Length > 0 then
                let symbolPosInRow =
                    let i = binchopFloor row.XOffsets p.X
                    if i = -1 then 0 else i
                let symbolIndexInRow = if symbolPosInRow = row.SymbolCount then row.SymbolCount - 1 else symbolPosInRow
                let sym = row.GetSymbol(symbolIndexInRow)
                if not (symbolIsNewline sym) then
                    let targetIsWhitespace = symbolIsSpaceOrTab sym
                    let mutable i = symbolIndexInRow
                    while i - 1 >= 0 && (symbolIsSpaceOrTab (row.GetSymbol(i - 1)) = targetIsWhitespace) do
                        i <- i - 1
                    let mutable j = symbolIndexInRow
                    while j + 1 < row.SymbolCount && (symbolIsSpaceOrTab (row.GetSymbol(j + 1)) = targetIsWhitespace) do
                        j <- j + 1
                    { sAnchorPos = rowRange.rBegin + row.CharOffsets.[i]; sCaretPos = rowRange.rBegin + row.CharOffsets.[j + 1] }
                else
                    let pos = rowRange.rBegin + row.CharOffsets.[symbolIndexInRow]
                    { sAnchorPos = pos; sCaretPos = pos; }
            else { sCaretPos = rowRange.rBegin; sAnchorPos = rowRange.rBegin }
        else { sCaretPos = 0; sAnchorPos = 0 }
    
    let changeLayout (layoutInfo : DocLayoutInfo) (doc : Doc) =
        if layoutInfo <> doc.LayoutInfo then
            createFromString layoutInfo (getAllString doc)
        else doc

    let clearColor (doc : Doc) : Doc =
        let mutable accu = doc.RowTree
        for i = 0 to doc.RowTree.Count - 1 do
            let row = doc.RowTree.[i]
            if not (colorInfoArrayIsClear row.Colors) then
                accu <- accu.ReplaceAt(i, { row with Colors = clearColorInfoArrayCreate row.String.Length })
        { doc with RowTree = accu }

    let prevNext (nextNotPrev : bool) (doc : Doc) charPos =
        let newPos = validateCharPos doc nextNotPrev (if nextNotPrev then charPos + 1 else charPos - 1)
        if newPos <> charPos
        then Some newPos
        else None

    let leftRight (rightNotleft : bool) (doc : Doc) =
        let sel = doc.Selection
        if sel.Length = 0 then
            match prevNext rightNotleft doc sel.sCaretPos with
            | Some left -> Some (setPos doc left)
            | None -> None
        else
            Some (setPos doc ((if rightNotleft then max else min) sel.sAnchorPos sel.sCaretPos))
    
    let shiftLeftRight (rightNotLeft : bool) (doc : Doc) =
        let sel = doc.Selection
        match prevNext rightNotLeft doc sel.sCaretPos with
        | Some newPos -> Some { doc with Selection = { sel with sCaretPos = newPos }}
        | None -> None

    let backDelete (deleteNotBack : bool) (doc : Doc) =
        let sel = doc.Selection
        let sel =
            if sel.Length = 0 then
                match prevNext deleteNotBack doc sel.sCaretPos with
                | Some newPos -> { sel with sCaretPos = newPos }
                | None -> sel
            else sel
        if sel.Length > 0 then
            Some (replace { doc with Selection = sel } "")
        else None
    
    let draw
            (g : Graphics)
            (bgColor : Color)
            (doc : Doc)
            (area : Rectangle)
            (linenoWidth : int)
            (leftMargin : int)
            (xOffset : int)
            (drawNewlineSymbol : bool)
            (drawTabSymbol : bool)
            (topRowIndex : int) =
        let li = doc.LayoutInfo
        use bgBrush = new SolidBrush(bgColor)
        g.FillRectangle(bgBrush, area)
        let bottomRowIndex = min (topRowIndex + area.Height / li.LineHeight) (doc.RowCount - 1)
        let mutable y = area.Y
        let x0 = area.X + linenoWidth + leftMargin - xOffset
        let font1, font2 = createFont li
        let mutable lineno = doc.RowTree.MeasureRange(0, topRowIndex).EndOfLineCount + 1
        let mutable printLineno = topRowIndex = 0 || (fst (getRow doc (topRowIndex - 1))).IsEndOfLine
        let hdc = g.GetHdc()
        Win32.SetBkMode(hdc, Win32.TRANSPARENT) |> ignore
        let hfont1 = font1.ToHfont()
        let hfont2 = font2.ToHfont()
        let old_hfont = Win32.SelectObject(hdc, hfont1)
        let mutable font2Selected = false
        let selectFont (selectFont2 : bool) =
            if selectFont2 <> font2Selected then
                if selectFont2 then Win32.SelectObject(hdc, hfont2) |> ignore
                else Win32.SelectObject(hdc, hfont1) |> ignore
                font2Selected <- selectFont2

        let darkgreen_pen = Win32.CreatePen(0, 1, Win32.colorref_of_color(Color.DarkGreen))
        let old_pen = Win32.SelectObject(hdc, darkgreen_pen)
        let selected_brush = Win32.CreateSolidBrush(Win32.colorref_of_color(Color.SkyBlue))

        let drawLineSymbol (p : Point) (lineSymbol : struct (PointF * PointF) array) =
            let k = float32 li.FontSize
            let round (x : float32) = int (Math.Floor(float x + 0.5))
            let translate (pf : PointF) = Point(p.X + round (k * pf.X), li.Padding + p.Y + round (k * pf.Y))
            for struct (a, b) in lineSymbol do
                let ta = translate a
                let tb = translate b
                Win32.MoveToEx(hdc, ta.X, ta.Y, IntPtr.Zero) |> ignore
                Win32.LineTo(hdc, tb.X, tb.Y) |> ignore
        
        for k = topRowIndex to bottomRowIndex do
            let row, rowRange = getRow doc k
            Win32.SetTextColor(hdc, Win32.colorref_of_color Color.Black) |> ignore
            for i = 0 to row.SymbolCount - 1 do
                let colorInfo = row.Colors.[row.CharOffsets.[i]]
                let x = x0 + row.XOffsets.[i]
                let w = row.XOffsets.[i + 1] - row.XOffsets.[i]
                let range = doc.Selection.ToRange()
                if range.Contains (rowRange.rBegin + row.CharOffsets.[i]) then
                    let mutable rect = Win32.RECT(left = x, top = y, right = x + w, bottom = y + li.LineHeight)
                    Win32.FillRect(hdc, &rect, selected_brush) |> ignore
                elif Option.isSome colorInfo.ciBackColor then
                    let mutable rect = Win32.RECT(left = x, top = y, right = x + w, bottom = y + li.LineHeight)
                    let brush = Win32.CreateSolidBrush(Win32.colorref_of_color(colorInfo.ciBackColor.Value))
                    Win32.FillRect(hdc, &rect, brush) |> ignore
                    Win32.DeleteObject(brush) |> ignore
                if Option.isSome colorInfo.ciUnderlineColor then
                    let mutable rect = Win32.RECT(left = x, top = y + li.LineHeight - (max 2 (li.FontSize / 10)) - li.Padding, right = x + w, bottom = y + li.LineHeight - li.Padding)
                    let brush = Win32.CreateSolidBrush(Win32.colorref_of_color(colorInfo.ciUnderlineColor.Value))
                    Win32.FillRect(hdc, &rect, brush) |> ignore
                    Win32.DeleteObject(brush) |> ignore
                match row.String.[row.CharOffsets.[i]] with
                | '\r' | '\n' ->
                    if drawNewlineSymbol then
                        drawLineSymbol (Point(x0 + row.XOffsets.[i], y)) symbol_rn
                | '\t' ->
                    if drawTabSymbol then
                        drawLineSymbol (Point(x0 + row.XOffsets.[i + 1], y)) symbol_t
                | ' '
                | '　' -> ()
                | _ ->
                    let color =
                        if Option.isNone colorInfo.ciForeColor then
                            Color.Black
                        else
                            colorInfo.ciForeColor.Value
                    Win32.SetTextColor(hdc, Win32.colorref_of_color color) |> ignore
                    let ofs = row.CharOffsets.[i]
                    let len = row.CharOffsets.[i+1] - ofs
                    if not (0 <= ofs && ofs < row.String.Length && ofs + len <= row.String.Length) then dontcare()
                    let symbol = row.String.Substring(ofs,len)
                    let isAscii = symbolIsAscii symbol
                    selectFont (not isAscii)
                    let yOfs = if isAscii then li.YOffset1 else li.YOffset2
                    Win32.TextOut(hdc, x0 + row.XOffsets.[i], li.Padding + y + yOfs, row.String.Substring(ofs,len), len) |> ignore

            if linenoWidth > 0 then
                let mutable rect = Win32.RECT(left = area.X, top = y, right = area.X + linenoWidth, bottom = y + li.LineHeight)
                let brush = Win32.CreateSolidBrush(Win32.colorref_of_color(Color.DarkGray))
                Win32.FillRect(hdc, &rect, brush) |> ignore
                Win32.DeleteObject(brush) |> ignore
                if printLineno then
                    let s = sprintf "%4d" lineno
                    let s = if s.Length > 4 then s.Substring(s.Length - 4, 4) else s
                    Win32.SetTextColor(hdc, Win32.colorref_of_color Color.DarkGreen) |> ignore
                    selectFont false
                    Win32.TextOut(hdc, area.X, li.Padding + y + li.YOffset1, s, s.Length) |> ignore
                if row.IsEndOfLine then
                    lineno <- lineno + 1
                    printLineno <- true
                else
                    printLineno <- false
                    
            y <- y + li.LineHeight

        for k = bottomRowIndex to topRowIndex + area.Height / li.LineHeight - 1 do
            if linenoWidth > 0 then
                let mutable rect = Win32.RECT(left = area.X, top = y, right = area.X + linenoWidth, bottom = y + li.LineHeight)
                let brush = Win32.CreateSolidBrush(Win32.colorref_of_color(Color.DarkGray))
                Win32.FillRect(hdc, &rect, brush) |> ignore
                Win32.DeleteObject(brush) |> ignore

            y <- y + li.LineHeight

        Win32.DeleteObject(Win32.SelectObject(hdc, old_pen)) |> ignore
        Win32.SelectObject(hdc, old_hfont) |> ignore
        Win32.DeleteObject(hfont1) |> ignore
        Win32.DeleteObject(hfont2) |> ignore
        Win32.DeleteObject(selected_brush) |> ignore
        font1.Dispose()
        font2.Dispose()
        g.ReleaseHdc(hdc)
