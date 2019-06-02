[<AutoOpen>]
module TreeList

open System
open System.Collections.Generic

[<CustomEquality; CustomComparison>]
type TreeList<[<EqualityConditionalOn; ComparisonConditionalOn>] 'T> =
    | Nil
    | Node of Height : int * 
              Count : int * 
              Left : TreeList<'T> *
              Value : 'T *
              Right : TreeList<'T>
    
    member x.Count =
        match x with
        | Nil -> 0
        | Node (Count = count) -> count
    
    member x.Height =
        match x with
        | Nil -> 0
        | Node (Height = height) -> height
    
    member x.Item
        with get i =
            match x with
            | Node _ ->
                let rec get_loop node i =
                    match node with
                    | Node (Left = left; Value = value; Right = right)->
                        let left_count = left.Count
                        if i < left_count then
                            get_loop left i
                        elif left_count < i then
                            get_loop right (i - left_count - 1)
                        else value
                    | Nil -> dontcare()
    
                if not (0 <= i && i < x.Count) then raise (IndexOutOfRangeException())
                get_loop x i
            | Nil -> raise (IndexOutOfRangeException())

    override x.Equals(yobj) = 
        match yobj with
        | :? TreeList<'T> as y ->
            if x.Count = y.Count then
                let count = x.Count
                let mutable cont = true
                let mutable i = 0
                while cont && i < count do
                    cont <- Unchecked.equals x.[i] y.[i]
                    i <- i + 1
                cont
            else false
        | _ -> false
    
    override x.GetHashCode() =
        let mutable hash = 17
        for i = 0 to x.Count - 1 do
            hash <- (23 * hash) ^^^ Unchecked.hash x
        hash

    interface IComparable with
        member x.CompareTo yobj = 
            match yobj with
            | :? TreeList<'T> as y ->
                match compare x.Count y.Count with
                | 0 ->
                    let count = x.Count
                    let mutable accu = 0
                    let mutable i = 0
                    while accu = 0 && i < count do
                        accu <- Unchecked.compare x.[i] y.[i]
                        i <- i + 1
                    accu
                | d -> d
            | _ -> invalidArg "yobj" "can not compare"

    member x.GetEnumerator() = TreeListEnumeraotr.Create(x) :> IEnumerator<'T>

    interface IReadOnlyList<'T> with
        member x.Count = x.Count
        member x.Item with get i = x.[i]
    interface IEnumerable<'T> with
        member x.GetEnumerator() = x.GetEnumerator()
    interface System.Collections.IEnumerable with
        member x.GetEnumerator() = x.GetEnumerator() :> System.Collections.IEnumerator

and TreeListEnumeraotr<'T> =
    { mutable current : 'T
      stack_storage : TreeList<'T> array
      mutable stack_count : int }

    member x.Push(node : TreeList<'T>) =
        match node with
        | Nil -> ()
        | Node (Left = left) ->
            x.stack_storage.[x.stack_count] <- node
            x.stack_count <- x.stack_count + 1
            x.Push(left)

    static member Create(node : TreeList<'T>) : TreeListEnumeraotr<'T> =
        let h = match node with Nil -> 0 | Node (Height = height) -> height
        let e =
            { current = Unchecked.defaultof<'T>
              stack_storage = Array.zeroCreate h
              stack_count = 0 }
        e.Push(node)
        e

    interface IEnumerator<'T> with
        member x.Current with get () = x.current
    interface System.Collections.IEnumerator with
        member x.Current with get () = box x.current
        member x.MoveNext() =
            if x.stack_count = 0 then
                x.current <- Unchecked.defaultof<'T>
                false
            else
                let topidx = x.stack_count - 1
                let top = x.stack_storage.[topidx]
                x.stack_storage.[topidx] <- Unchecked.defaultof<TreeList<'T>>
                x.stack_count <- topidx
                match top with
                | Nil -> dontcare()
                | Node (Value = value; Right = right) ->
                    x.current <- value
                    x.Push(right)
                    true
                
        member x.Reset() = raise (NotImplementedException())
    interface IDisposable with
        member x.Dispose() = ()

type treelist<'T> = TreeList<'T>

[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module TreeList =
    
    let empty<'T> = TreeList<'T>.Nil

    let height_of node =
        match node with
        | Nil -> 0
        | Node (Height = height) -> height

    let count_of (node : 'a treelist) = node.Count

    let create_node left value right =
        let height = (max (height_of left) (height_of right)) + 1
        let count = left.Count + 1 + right.Count
        Node (height, count, left, value, right)
        
    let rec private toArrayLoop (ary : 'V array) (node : TreeList<'V>) i =
        match node with
        | Nil -> ()
        | Node (Left = left; Value = value; Right = right) ->
            let left_count = left.Count
            toArrayLoop ary left i
            ary.[i + left_count] <- value
            toArrayLoop ary right (i + left_count + 1)
    
    let toArray (root : 'a treelist) =
        let ary = Array.zeroCreate root.Count
        toArrayLoop ary root 0
        ary
    
    let rec join l v r =
        let b = height_of l - height_of r
        match b with
        | 0 | 1 | -1 -> create_node l v r
        | 2 ->
            match l with
            | Node (Left = ll; Value = lv; Right = lr) ->
                let lb = height_of ll - height_of lr
                if lb = -1 then
                    match lr with
                    | Node (Left = lrl; Value = lrv; Right = lrr) ->
                        create_node (create_node ll lv lrl) lrv (create_node lrr v r)
                    | Nil -> dontcare()
                else
                    create_node ll lv (create_node lr v r)
            | Nil -> dontcare()
        | -2 ->
            match r with
            | Node (Left = rl; Value = rv; Right = rr) ->
                let rb = height_of rl - height_of rr
                if rb = 1 then
                    match rl with
                    | Node (Left = rll; Value = rlv; Right = rlr) ->
                        create_node (create_node l v rll) rlv (create_node rlr rv rr)
                    | Nil -> dontcare()
                else
                    create_node (create_node l v rl) rv rr
            | Nil -> dontcare()
        | _ ->
            if b > 0 then
                match l with
                | Node (Left = ll; Value = lv; Right = lr) ->
                    join ll lv (join lr v r)
                | Nil -> dontcare()
            else
                match r with
                | Node (Left = rl; Value = rv; Right = rr) ->
                    join (join l v rl) rv rr
                | Nil -> dontcare()
    
    let add list value = join list value Nil
    let push value list = join Nil value list

    let rec private take_first_loop node n =
        if n = 0 then
            Nil
        else
            match node with
            | Node (Left = left; Value = value; Right = right; Count = count) ->
                let left_count = left.Count
                if n < left_count then
                    take_first_loop left n
                elif n = left_count then
                    left
                elif n = left_count + 1 then
                    join left value Nil
                elif n < count then
                    join left value (take_first_loop right (n - left_count - 1))
                elif n = count then
                    node
                else
                    raise (IndexOutOfRangeException())
            | Nil -> raise (IndexOutOfRangeException())

    let rec private take_last_loop node n =
        if n = 0 then
            Nil
        else
            match node with
            | Node (Left = left; Value = value; Right = right; Count = count) ->
                let right_count = right.Count
                if n < right_count then
                    take_last_loop right n
                elif n = right_count then
                    right
                elif n = right_count + 1 then
                    join Nil value right
                elif n < count then
                    join (take_last_loop left (n - right_count - 1)) value right
                elif n = count then
                    node
                else
                    raise (IndexOutOfRangeException())
            | Nil -> raise (IndexOutOfRangeException())
            
    let ofArray (ary : 'v array) =

        let rec loop start count =
            match count with
            | 0 -> Nil
            | 1 -> Node (1, 1, Nil, ary.[start], Nil)
            | 2 ->
                let left = Node (1, 1, Nil, ary.[start], Nil)
                Node (2, 2, left, ary.[start + 1], Nil)
            | 3 ->
                let left = Node (1, 1, Nil, ary.[start], Nil)
                let right = Node (1, 1, Nil, ary.[start+2], Nil)
                Node (2, 3, left, ary.[start+1], right)
            | _ ->
                let left_count = (count - 1) / 2
                let right_count = count - 1 - left_count
            
                let left = loop start left_count
                let value = ary.[start + left_count]
                let right = loop (start + left_count + 1) right_count

                create_node left value right
        
        loop 0 ary.Length
    
    let append (node1 : 'a treelist) (node2 : 'a treelist) =
        let node2_count = node2.Count
        if node2_count = 0 then
            node1
        else
            let node2_hd = node2.[0]
            let node2_tl = take_last_loop node2 (node2_count - 1)
            join node1 node2_hd node2_tl

    let addRange list ary = append list (ofArray ary) 
    let pushRange ary list = append (ofArray ary) list

    let removeRange (list : 'a treelist) start count =
        let ed = start + count
        let list_count = list.Count
        if not (0 <= start && start <= list_count && 0 <= count && 0 <= ed && ed <= list_count) then
            raise (IndexOutOfRangeException())
        append (take_first_loop list start) (take_last_loop list (list_count - start - count))

    let insertRange (list : 'a treelist) pos values =
        let list_count = list.Count
        if not (0 <= pos && pos <= list_count) then raise (IndexOutOfRangeException())
        append (append (take_first_loop list pos) (ofArray values)) (take_last_loop list (list_count - pos))

[<AutoOpen>]
module TreeListExtensions =
    type TreeList<'T> with
        member x.ToArray() = TreeList.toArray x