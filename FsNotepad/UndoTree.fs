[<AutoOpen>]
module UndoTree

open System
open System.Collections.Generic

type UndoTreeNode<'T> =
    { mutable Value : 'T
      DateTime : DateTime
      Revision : int
      Prev : UndoTreeNode<'T> option
      Next : List<UndoTreeNode<'T>>
      mutable SelectedBranchIndex : int
      mutable Sealed : bool
      mutable RevisionsAhead : int }
    
    static member Create(prev, x, rev) =
        { Value = x
          DateTime = DateTime.Now
          Revision = rev
          Prev = prev
          Next = List<UndoTreeNode<'T>>()
          SelectedBranchIndex = 0
          Sealed = false
          RevisionsAhead = 0 }

type UndoTree<'T> =
    { mutable Current : UndoTreeNode<'T>
      Revisions : List<UndoTreeNode<'T>> }

    member this.Get = this.Current.Value
    member this.Amend(x) = this.Current.Value <- x

    static member Create(init : 'T) =
        let rev0 = UndoTreeNode.Create(None, init, 0)
        { Current = rev0
          Revisions = List([| rev0 |]) }
    
    member this.Clear(newInit) =
        let newRev0 = UndoTreeNode.Create(None, newInit, 0)
        this.Current <- newRev0
        this.Revisions.Clear()
        this.Revisions.Add(newRev0)

    member this.Commit(x) =
        let newCurrent = UndoTreeNode.Create(Some this.Current, x, this.Revisions.Count)
        let i = this.Current.Next.Count
        this.Current.Next.Add(newCurrent)
        this.Current.SelectedBranchIndex <- i
        this.Revisions.Add(newCurrent)
        this.Current <- newCurrent

    member this.Undo() =
        let ahead = this.Current.RevisionsAhead
        match this.Current.Prev with
        | Some prev ->
            prev.RevisionsAhead <- ahead + 1
            this.Current <- prev
        | _ -> dontcare()
    
    member this.CanUndo = this.Current.Prev.IsSome
    member tree.Redo() = tree.Current <- tree.Current.Next.[tree.Current.SelectedBranchIndex]
    member this.CanRedo = this.Current.Next.Count > 0
    member this.BranchCount = this.Current.Next.Count

    member this.RedoBranch(i) =
        this.Current.SelectedBranchIndex <- i
        this.Current <- this.Current.Next.[i]

