[<AutoOpen>]
module MeasuredTreeList

open System
open System.Collections.Generic

exception DontCareException

type MeasuredTreeListNode<'V, 'M> =
    | Nil
    | Node of Height : int * 
              Count : int * 
              Left : MeasuredTreeListNode<'V, 'M> *
              Value : 'V *
              Measure : 'M *
              Right : MeasuredTreeListNode<'V, 'M>
 
    member x.Count = match x with Nil -> 0 | Node (Count = count) -> count

type MeasuredTreeList<'V, 'M> private (func : 'M -> 'V -> 'M -> 'M,
                                       zero : 'M,
                                       root : MeasuredTreeListNode<'V, 'M>) =
    static let dontcare () = raise DontCareException
    
    static let height_of node =
        match node with
        | Nil -> 0
        | Node (Height = height) -> height

    static let count_of node =
        match node with
        | Nil -> 0
        | Node (Count = count) -> count
    
    let measure_of (node : MeasuredTreeListNode<'V, 'M>) =
        match node with
        | Nil -> zero
        | Node (Measure = measure) -> measure

    let create_node left value right =
        let height = (max (height_of left) (height_of right)) + 1
        let count = count_of left + 1 + count_of right
        let measure = func (measure_of left) value (measure_of right)
        Node (height, count, left, value, measure, right)
    
    let rec get_loop (node : MeasuredTreeListNode<'V, 'M>) i =
        match node with
        | Node (Left = left; Value = value; Right = right)->
            let left_count = count_of left
            if i < left_count then
                get_loop left i
            elif left_count < i then
                get_loop right (i - left_count - 1)
            else value
        | Nil -> dontcare()
    
    let get i =
        if not (0 <= i && i < count_of root) then raise (IndexOutOfRangeException())
        get_loop root i
    
    let rec to_array_loop (ary : 'V array) (node : MeasuredTreeListNode<'V, 'M>) i =
        match node with
        | Nil -> ()
        | Node (Left = left; Value = value; Right = right) ->
            let left_count = count_of left
            to_array_loop ary left i
            ary.[i + left_count] <- value
            to_array_loop ary right (i + left_count + 1)
    
    let to_array () =
        let ary = Array.zeroCreate (count_of root)
        to_array_loop ary root 0
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
    
    let rec take_first_loop node n =
        if n = 0 then
            Nil
        else
            match node with
            | Node (Left = left; Value = value; Right = right; Count = count) ->
                let left_count = count_of left
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

    let rec take_last_loop node n =
        if n = 0 then
            Nil
        else
            match node with
            | Node (Left = left; Value = value; Right = right; Count = count) ->
                let right_count = count_of right
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
            
    static let create_tree_fast func (zero : 'm) (ary : 'v array) =

        let measure_of node =
            match node with
            | Nil -> zero
            | Node (Measure = measure) -> measure

        let create_node left value right =
            let height = (max (height_of left) (height_of right)) + 1
            let count = count_of left + 1 + count_of right
            let measure = func (measure_of left) value (measure_of right)
            Node (height, count, left, value, measure, right)

        let rec loop start count =
            match count with
            | 0 -> Nil
            | 1 -> Node (1, 1, Nil, ary.[start], func zero ary.[start] zero, Nil)
            | 2 ->
                let left = Node (1, 1, Nil, ary.[start], func zero ary.[start] zero, Nil)
                Node (2, 2, left, ary.[start + 1], func (measure_of left) ary.[start+1] zero, Nil)
            | 3 ->
                let left = Node (1, 1, Nil, ary.[start], func zero ary.[start] zero, Nil)
                let right = Node (1, 1, Nil, ary.[start+2], func zero ary.[start+2] zero, Nil)
                Node (2, 3, left, ary.[start+1], func (measure_of left) ary.[start+1] (measure_of right), right)
            | _ ->
                let left_count = (count - 1) / 2
                let right_count = count - 1 - left_count
            
                let left = loop start left_count
                let value = ary.[start + left_count]
                let right = loop (start + left_count + 1) right_count

                create_node left value right
        
        loop 0 ary.Length
    
    let append node1 node2 =
        let node2_count = count_of node2
        if node2_count = 0 then
            node1
        else
            let node2_hd = get_loop node2 0
            let node2_tl = take_last_loop node2 (node2_count - 1)
            join node1 node2_hd node2_tl

    let wrap (node : MeasuredTreeListNode<'V, 'M>) = MeasuredTreeList<'V, 'M>(func, zero, node)

    let rec measure_range node start count =
        match node with
        | Nil -> zero
        | Node (Count = nodeCount; Left = left; Value = value; Measure = measure; Right = right) ->
            let ed = start + count
            let left_count = count_of left
            if nodeCount <= start || ed <= 0 then
                zero
            elif start <= 0 && nodeCount <= ed then
                measure
            elif ed <= left_count then
                measure_range left start count
            elif left_count + 1 <= start then
                measure_range right (start - left_count - 1) count
            else
                let leftRangeMeasure = measure_range left start count
                let rightRangeMeasure = measure_range right (start - left_count - 1) count
                func leftRangeMeasure value rightRangeMeasure

    new (func, zero) = MeasuredTreeList(func, zero, Nil)
    new (func, zero : 'M, ary : 'V array) = MeasuredTreeList(func, zero, create_tree_fast func zero ary)
    member this.MeasureFunc = func
    member this.MeasureZero = zero
    member this.MeasureOf (node : MeasuredTreeListNode<'V, 'M>) = measure_of node
    member this.Root = root
    member this.RootMeasure = match root with Node (Measure = measure) -> measure | Nil -> zero
    member this.Count = count_of root
    member this.Item with get i = get i
    member this.ToArray() = to_array ()
    member this.Add x = wrap (join root x Nil)
    member this.Push x = wrap (join Nil x root)
    member this.TakeFirst n = wrap (take_first_loop root n)
    member this.TakeLast n = wrap (take_last_loop root n)
    member this.GetRange(start, count) =
        let ed = start + count
        let count_of_root = count_of root
        if not (0 <= start && start <= count_of_root && 0 <= count && 0 <= ed && ed <= count_of_root) then
            raise (IndexOutOfRangeException())
        wrap (take_first_loop(take_last_loop root ((count_of root) - start)) count)
    member this.RemoveRange(start, count) =
        let ed = start + count
        let count_of_root = count_of root
        if not (0 <= start && start <= count_of_root && 0 <= count && 0 <= ed && ed <= count_of_root) then
            raise (IndexOutOfRangeException())
        wrap (append (take_first_loop root start) (take_last_loop root (count_of root - start - count)))
    member this.RemoveAt i =
        if not (0 <= i && i < count_of root) then raise (IndexOutOfRangeException())
        this.RemoveRange(i, 1)
    member this.AddRange(values : 'V array) = wrap (append root (create_tree_fast func zero values))
    member this.PushRange(values : 'V array) = wrap (append (create_tree_fast func zero values) root)
    member this.AddRange(other : MeasuredTreeList<'V, 'M>) = wrap (append root other.Root)
    member this.PushRange(other : MeasuredTreeList<'V, 'M>) = wrap (append other.Root root)
    member this.InsertRange(i, values) =
        if not (0 <= i && i <= count_of root) then raise (IndexOutOfRangeException())
        wrap (append (append (take_first_loop root i) (create_tree_fast func zero values)) (take_last_loop root (count_of root - i)))
    member this.InsertAt(i, v) = this.InsertRange(i, [| v |])
    member this.ReplaceAt(i, v) = this.RemoveAt(i).InsertAt(i, v)
    member this.MeasureRange(start, count) = measure_range root start count
    member this.GetEnumerator() = (seq { for i = 0 to count_of root - 1 do yield get i }).GetEnumerator()
    interface IEnumerable<'V> with
        member this.GetEnumerator() = this.GetEnumerator()
    interface System.Collections.IEnumerable with
        member this.GetEnumerator() = this.GetEnumerator() :> System.Collections.IEnumerator

module MeasuredTreeListTest =
    let Assert cond = if not cond then raise (Exception())

    let height_of node = match node with Node (Height = height) -> height | Nil -> 0

    let rec validate_node node =
        match node with
        | Node (Left = left; Right = right) ->
            let bal = height_of left - height_of right
            Assert (abs bal <= 1)
            validate_node left
            validate_node right
        | Nil -> ()
    
    let validate (list : MeasuredTreeList<_, _>) = validate_node list.Root

    let test_construction n m =
        let all = Array.init (n + m) id
        let left_half = Array.init n id
        let right_half = Array.init m (fun i -> n + i)

        let func l v r = 0
        let list1 = MeasuredTreeList<int, int>(func, 0, all)
        let list2 = MeasuredTreeList<int, int>(func, 0, left_half).AddRange(right_half)
        let list3 = MeasuredTreeList<int, int>(func, 0, right_half).PushRange(left_half)
        let list4 = MeasuredTreeList<int, int>(func, 0, left_half).AddRange(MeasuredTreeList<int, int>(func, 0, right_half))
        let list5 = MeasuredTreeList<int, int>(func, 0, right_half).PushRange(MeasuredTreeList<int, int>(func, 0, left_half))

        Assert(Array.ofSeq list1 = all)
        Assert(list1.ToArray() = all)
        Assert(list2.ToArray() = all)
        Assert(list3.ToArray() = all)
        Assert(list4.ToArray() = all)
        Assert(list5.ToArray() = all)

        validate list1
        validate list2
        validate list3
        validate list4
        validate list5

    let test1 () =
        for n = 0 to 20 do
            for m = 0 to 20 do
                test_construction n m
        for n = 0 to 1000 do
            test_construction (n / 2) (n - n / 2)

    
    let test2 () =
        let rand = new Random()

        for i = 0 to 99 do
            let ary = Array.init 100 (fun _ -> rand.Next())
            let l1 = List<int>(ary)
            let mutable l2 = MeasuredTreeList<int, int64>((fun l v r -> l + int64 v + r), 0L, ary)
            
            for j = 0 to 9 do
                let remove_count = rand.Next(101)
                let remove_pos = rand.Next(l1.Count - remove_count + 1)

                l1.RemoveRange(remove_pos, remove_count)
                l2 <- l2.RemoveRange(remove_pos, remove_count)
                
                let insert_count = remove_count
                let insert_items = Array.init insert_count (fun _ -> rand.Next())
                let insert_pos = rand.Next(l1.Count + 1)

                l1.InsertRange(insert_pos, insert_items)
                l2 <- l2.InsertRange(insert_pos, insert_items)

            validate l2

            Assert(l1.ToArray() = l2.ToArray())
            let sum = l1 |> Seq.map (fun i -> int64 i) |> Seq.sum
            Assert(sum = l2.RootMeasure)



