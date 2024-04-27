module App

open Sutil
open Sutil.Styling
open type Feliz.length
open System

type Colour = Green | Red

type Agent = {
    Coordinates: int * int
    Colour : Colour
}

type Model = { 
    ColCount: int
    RowCount: int
    Agents : Agent array
    PopulationDensity: float
    Running: bool
    Ticks: int
    }

let Init () = { 
    ColCount = 100
    RowCount = 100
    Agents = [||]
    PopulationDensity = 0.5
    Running = false
    Ticks = 0
}

let stateStore = Store.make (Init())
let getState () = Store.get stateStore

let rnd = System.Random()

let convertIdToCoordinates id rows cols =
    let rowNumber = (id / cols) 
    let colNumber = (id % cols) 
    rowNumber + 1, colNumber
let convertCoordinatesToId row col rows cols =
    (row - 1) * cols + col
    
let getEmptyCell state =
    let totalCells = (state.RowCount * state.ColCount) 

    let rec checkEmpty id =
        let row, col = convertIdToCoordinates id state.RowCount state.ColCount
        if (
            state.Agents
            |> Array.exists( fun a -> (a.Coordinates |> fst) = row  &&  (a.Coordinates |> snd) = col) 
            )   
            then checkEmpty (rnd.Next totalCells + 1)
            else row, col 

    checkEmpty (rnd.Next totalCells)

let getInitialCellsForPopulation  requiredNumber totalRange =

    let rec genUnique (cells : int Set) = 
        if cells |> Set.count = requiredNumber 
            then cells
            else
                let newNo = rnd.Next (totalRange + 1)
                if Set.contains newNo cells then
                        genUnique cells
                    else
                        Set.add newNo cells

    genUnique Set.empty

let populateGrid state = 
    let agents = 
        getInitialCellsForPopulation 
            ((float (state.RowCount * state.ColCount) * state.PopulationDensity) |> int) 
            (state.RowCount * state.ColCount)
        |> Set.map(fun id -> 
            {
                Coordinates = convertIdToCoordinates id state.RowCount state.ColCount;
                Colour = if id % 2 = 0 then Green else Red
            }
        )
        |> Set.toArray

    {state with Agents = agents}
  
let tableStyle =
    [   rule "table, th, td" [ Css.border (px 1, Feliz.borderStyle.solid, "grey"); Css.tableLayoutFixed' ]
        rule
            "table"
            [
                Css.tableLayoutFixed'
                Css.border (px 1, Feliz.borderStyle.solid, "grey")
                Css.margin 5 ]
        rule "td" [ Css.height (em 1); Css.width (em 1) ] ]

let smed =
    Html.div[
        Attr.style
            "height: 100%;
            width: 100%;
            background-color: green;
            display: inline-block;"
        ]

let smee  =
    Html.div[
        Attr.style
           "height: 100%;
            width: 100%;
            background-color: red;
            display: inline-block;"
        ]

let empty  =
    Html.div[
        Attr.style
            "height: 1px;
            width: 1px;
            display: inline-block;"
        ]

let createRow rowNumber columns =
    let cols =
        [ 1..columns ]
        |> Seq.map (fun c -> 
            empty
            |> Html.td)

    Html.tableRow cols

let generateGrid totalRows totalColumns =

    let s: seq<Core.SutilElement> = [ Attr.style "overflow: hidden;" ]

    Html.div
        [   Attr.style "float: left"
            ([ 1..totalRows ]
            |> Seq.map (fun r -> createRow r totalColumns)
            |> Html.table
            |> withStyle tableStyle) 
        ]

Html.div[
    Attr.style [Css.marginLeft 5; Css.marginRight 5]
    Bind.el(stateStore, (fun ps -> generateGrid (getState().RowCount) (getState().ColCount)))
    ]   
|> Program.mount 

