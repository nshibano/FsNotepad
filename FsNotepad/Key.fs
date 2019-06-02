[<AutoOpen>]
module Key

open System.Windows.Forms

type Key =
    | Kenter
    | Kspace
    | Ktab
    | Kback
    | Kdelete
    | Kleft
    | Kright
    | Kup
    | Kdown
    | Ka
    | Kb
    | Kc
    | Kd
    | Ke
    | Kf
    | Kg
    | Kh
    | Ki
    | Kj
    | Kk
    | Kl
    | Km
    | Kn
    | Ko
    | Kp
    | Kq
    | Kr
    | Ks
    | Kt
    | Ku
    | Kv
    | Kw
    | Kx
    | Ky
    | Kz
    | Kf1
    | Kf2
    | Kf3
    | Kf4
    | Kf5
    | Kf6
    | Kf7
    | Kf8
    | Kf9
    | Kf10
    | Kf11
    | Kf12

    static member FromKeyCodeExn (keyCode : Keys) =
        match keyCode with
        | Keys.Enter -> Kenter
        | Keys.Space -> Kspace
        | Keys.Tab -> Ktab
        | Keys.Back -> Kback
        | Keys.Delete -> Kdelete
        | Keys.Left -> Kleft
        | Keys.Right -> Kright
        | Keys.Up -> Kup
        | Keys.Down -> Kdown
        | Keys.A -> Ka
        | Keys.B -> Kb
        | Keys.C -> Kc
        | Keys.D -> Kd
        | Keys.E -> Ke
        | Keys.F -> Kf
        | Keys.G -> Kg
        | Keys.H -> Kh
        | Keys.I -> Ki
        | Keys.J -> Kj
        | Keys.K -> Kk
        | Keys.L -> Kl
        | Keys.M -> Km
        | Keys.N -> Kn
        | Keys.O -> Ko
        | Keys.P -> Kp
        | Keys.Q -> Kq
        | Keys.R -> Kr
        | Keys.S -> Ks
        | Keys.T -> Kt
        | Keys.U -> Ku
        | Keys.V -> Kv
        | Keys.W -> Kw
        | Keys.X -> Kx
        | Keys.Y -> Ky
        | Keys.Z -> Kz
        | Keys.F1 -> Kf1
        | Keys.F2 -> Kf2
        | Keys.F3 -> Kf3
        | Keys.F4 -> Kf4
        | Keys.F5 -> Kf5
        | Keys.F6 -> Kf6
        | Keys.F7 -> Kf7
        | Keys.F8 -> Kf8
        | Keys.F9 -> Kf9
        | Keys.F10 -> Kf10
        | Keys.F11 -> Kf11
        | Keys.F12 -> Kf12
        | _ -> failwith "None"

type KeyDown =
    { kdKey : Key
      kdShift : bool
      kdControl : bool }

    static member CreateFromKeyDataExn(keyData : Keys) =
        { kdKey = Key.FromKeyCodeExn(Keys.KeyCode &&& keyData)
          kdShift = keyData.HasFlag(Keys.Shift)
          kdControl = keyData.HasFlag(Keys.Control) }

    static member CreateFromKeyData(keyData : Keys) =
        try Some (KeyDown.CreateFromKeyDataExn(keyData))
        with _ -> None
