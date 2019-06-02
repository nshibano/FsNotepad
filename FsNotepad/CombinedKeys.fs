module CombinedKeys

open System.Windows.Forms

let [<Literal>] Control_X = Keys.X ||| Keys.Control
let [<Literal>] Control_Y = Keys.Y ||| Keys.Control
let [<Literal>] Control_Z = Keys.Z ||| Keys.Control
let [<Literal>] Control_C = Keys.C ||| Keys.Control
let [<Literal>] Control_V = Keys.V ||| Keys.Control
let [<Literal>] Control_A = Keys.A ||| Keys.Control

let [<Literal>] Shift_Space = Keys.Space ||| Keys.Shift
let [<Literal>] Shift_Tab = Keys.Tab ||| Keys.Shift
let [<Literal>] Shift_Up = Keys.Shift ||| Keys.Up
let [<Literal>] Shift_Down = Keys.Shift ||| Keys.Down
let [<Literal>] Shift_Left = Keys.Shift ||| Keys.Left
let [<Literal>] Shift_Right = Keys.Shift ||| Keys.Right

let [<Literal>] Control_Shift_Z = Keys.Control ||| Keys.Shift ||| Keys.Z
