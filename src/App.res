%%raw(`import './App.css'`)

module Cell = {
  type state = Alive | Dead

  type t = {
    id: int,
    state,
  }

  let make = (state, id) => { state, id }

  let getColor = (t) => {
    switch t.state {
      | Alive => "green"
      | Dead => "grey"
    }
  }

  let reverse = (t) => {
    state: t.state == Alive ? Dead: Alive,
    id: t.id
  }
}

module Grid = {
  let make = (total) => {
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

type state = {
  grid: array<Cell.t>,
  cols: int,
  rows: int,
  size: int,
}

type action = HandleCycle | UpdateCell(Cell.t, int) | UpdateCols(int) | UpdateRows(int) | UpdateSize(int) | Reset

let handleCycle = ({ grid, cols }) => {
  grid->Belt.Array.mapWithIndex((index, cell) => {
    let neighbors: array<Cell.t> = []
    if (index > cols - 1) {
      if (mod(index, cols) !== 1) {
        // topleft
        switch grid->Belt.Array.get(index - cols - 1) {
          | Some(cell) => neighbors->Belt.Array.push(cell)
          | _ => ()
        }
      }

      // top
      switch grid->Belt.Array.get(index - cols) {
        | Some(cell) => neighbors->Belt.Array.push(cell)
        | _ => ()
      }

      if (mod(index, cols) !== 0) {
        // topright
        switch grid->Belt.Array.get(index - cols + 1) {
          | Some(cell) => neighbors->Belt.Array.push(cell)
          | _ => ()
        }
      }
    }

    if (mod(index, cols) > 0) {
      // left
      switch grid->Belt.Array.get(index - 1) {
        | Some(cell) => neighbors->Belt.Array.push(cell)
        | _ => ()
      }
    }

    if (index < cols - 1 || mod(index, cols) !== 0) {
      // right
      switch grid->Belt.Array.get(index + 1) {
        | Some(cell) => neighbors->Belt.Array.push(cell)
        | _ => ()
      }
    }

    // bottomleft
    switch grid->Belt.Array.get(index + cols - 1) {
      | Some(cell) => neighbors->Belt.Array.push(cell)
      | _ => ()
    }

    // bottom
    switch grid->Belt.Array.get(index + cols) {
      | Some(cell) => neighbors->Belt.Array.push(cell)
      | _ => ()
    }

    // bottomright
    switch grid->Belt.Array.get(index + cols + 1) {
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

let reducer = (state, action) => {
  switch action {
    | HandleCycle => {
      ...state,
      grid: handleCycle(state),
    }
    | UpdateCell(cell, index) => {
      ...state,
      grid: Grid.updateCell(state.grid, cell, index)
    }
    | UpdateSize(size) => {
      ...state,
      size
    }
    | UpdateCols(cols) => {
      let grid = Grid.make(cols * state.rows)
      {
        ...state,
        cols,
        grid,
      }
    }
    | UpdateRows(rows) => {
      let grid = Grid.make(state.cols * rows)
      {
        ...state,
        rows,
        grid,
      }
    }
    | Reset => {
      let grid = Grid.make(state.cols * state.rows)
      {
        ...state,
        grid
      }
    }
  }
}

let px = (n) => string_of_int(n) ++ "px"

let defaultState = {
  let cols = 100
  let rows = 100
  { cols, rows, grid: Grid.make(cols * rows), size: 10 }
}

@react.component
let make = () => {
  let (state, dispatch) = React.useReducer(reducer, defaultState)

  React.useEffect0(() => {
    Js.Global.setInterval(() => dispatch(HandleCycle), 500)->ignore
    None
  })

  <div>
    <div style={
      ReactDOM.Style.make(
        ~display = "flex",
        ~flexDirection = "column",
        ()
      )
    }>
      <button onClick={(_) => dispatch(Reset)}>{"Reset"->React.string}</button>
      <label>{"columns"->React.string}</label>
      <input
        value={string_of_int(state.cols)}
        onInput={event => dispatch(UpdateCols(ReactEvent.Form.currentTarget(event)["value"]->int_of_string))}
        name="cols" />
      <label>{"rows"->React.string}</label>
      <input
        value={string_of_int(state.rows)}
        onInput={event => dispatch(UpdateRows(ReactEvent.Form.currentTarget(event)["value"]->int_of_string))}
        name="rows" />
      <label>{"size"->React.string}</label>
      <input
        value={string_of_int(state.size)}
        onInput={event => dispatch(UpdateSize(ReactEvent.Form.currentTarget(event)["value"]->int_of_string))}
        name="size" />
    </div>
    <div style={
      ReactDOM.Style.make(
        ~display = "flex",
        ~flexWrap = "wrap",
        ~width = (state.cols * state.size)->px,
        ()
      )
    }>
    {
      state.grid->Belt.Array.mapWithIndex((index, cell) => {
        <div style={
          ReactDOM.Style.make(
            ~backgroundColor = cell->Cell.getColor,
            ~width = state.size->px,
            ~height = state.size->px,
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
