%%raw(`import './App.css'`)

module Cell = {
  type state = Alive | Dead

  type t = {
    id: int,
    state: state,
  }

  let make = (state, id) => { state, id }

  let getColor = (t) => {
    switch t.state {
      | Alive => "green"
      | _ => "grey"
    }
  }

  let reverse = (t) => {
    state: t.state == Alive ? Dead: Alive,
    id: t.id
  }

  let updateCell = (cells, cell, index) => {
    cells->Belt.Array.mapWithIndex((i, current) => {
      if (i == index) {
        cell
      } else {
        current
      }
    })
  }
}

module Grid = {
  let cols = 60
  let rows = 60
  let cell = 10
  let cellSize = string_of_int(cell) ++ "px"

  let make = (~total = cols * rows, ()) => {
    let cells = Belt.Array.make(total, Cell.make(Cell.Dead, 0))
    for x in 0 to cells->Belt.Array.length - 1 {
      let seed = Js.Math.random_int(0, 2)
      let state = {
        switch seed {
          | 1 => Cell.Alive
          | _ => Cell.Dead
        }
      }
      cells->Belt.Array.set(x, Cell.make(state, x))->ignore
    }
    cells
  }
}

let handleCycle = (cells) => {
  cells->Belt.Array.mapWithIndex((index, cell) => {
    let neighbors: array<Cell.t> = []
    if (index > Grid.cols - 1) {
      if (mod(index, Grid.cols) !== 1) {
        // topleft
        switch cells->Belt.Array.get(index - Grid.cols - 1) {
          | Some(cell) => neighbors->Belt.Array.push(cell)
          | _ => ()
        }
      }

      // top
      switch cells->Belt.Array.get(index - Grid.cols) {
        | Some(cell) => neighbors->Belt.Array.push(cell)
        | _ => ()
      }

      if (mod(index, Grid.cols) !== 0) {
        // topright
        switch cells->Belt.Array.get(index - Grid.cols + 1) {
          | Some(cell) => neighbors->Belt.Array.push(cell)
          | _ => ()
        }
      }
    }

    if (mod(index, Grid.cols) > 0) {
      // left
      switch cells->Belt.Array.get(index - 1) {
        | Some(cell) => neighbors->Belt.Array.push(cell)
        | _ => ()
      }
    }

    if (index < Grid.cols - 1 || mod(index, Grid.cols) !== 0) {
      // right
      switch cells->Belt.Array.get(index + 1) {
        | Some(cell) => neighbors->Belt.Array.push(cell)
        | _ => ()
      }
    }

    // bottomleft
    switch cells->Belt.Array.get(index + Grid.cols - 1) {
      | Some(cell) => neighbors->Belt.Array.push(cell)
      | _ => ()
    }

    // bottom
    switch cells->Belt.Array.get(index + Grid.cols) {
      | Some(cell) => neighbors->Belt.Array.push(cell)
      | _ => ()
    }

    // bottomright
    switch cells->Belt.Array.get(index + Grid.cols + 1) {
      | Some(cell) => neighbors->Belt.Array.push(cell)
      | _ => ()
    }
    switch neighbors->Belt.Array.keep((cell) => cell.state == Cell.Alive)->Belt.Array.length {
      | 2 => cell
      | 3 => {
        cell.state == Cell.Alive ? cell : Cell.make(Cell.Alive, cell.id)
      }
      | _ => Cell.make(Cell.Dead, cell.id)
    }
  })
}

type state = {
  cells: array<Cell.t>,
}

type action = HandleCycle | UpdateCell(Cell.t, int) | Reset

let reducer = (state, action) => {
  switch action {
    | HandleCycle => {
      cells: handleCycle(state.cells)
    }
    | UpdateCell(cell, index) => {
      cells: Cell.updateCell(state.cells, cell, index)
    }
    | Reset => {
      cells: Grid.make()
    }
  }
}

@react.component
let make = () => {
  let (state, dispatch) = React.useReducer(reducer, { cells: Grid.make() })

  React.useEffect0(() => {
    Js.Global.setInterval(() => dispatch(HandleCycle), 500)->ignore
    None
  })

  <div>
    <button onClick={(_) => dispatch(Reset)}>{"Reset"->React.string}</button>
    <div style={
      ReactDOM.Style.make(
        ~display = "flex",
        ~flexWrap = "wrap",
        ~width = string_of_int(Grid.cols * Grid.cell) ++ "px",
        ()
      )
    }>
    {
      state.cells->Belt.Array.mapWithIndex((index, cell) => {
        <div style={
          ReactDOM.Style.make(
            ~backgroundColor = cell->Cell.getColor,
            ~width = Grid.cellSize,
            ~height = Grid.cellSize,
            ()
          )
        }
        onClick={
          (_) => dispatch(UpdateCell(cell->Cell.reverse, index))
        }></div>
      })->React.array
    }
    </div>
  </div>
}
