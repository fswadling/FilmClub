module Client

open Elmish
open Elmish.React
open Elmish.Streams
open FSharp.Control
open Fable.React
open Fable.React.Props
open Fulma
open Thoth.Json

open Shared

// The model holds data that you want to keep track of while the application is running
// in this case, we are keeping track of a counter
// we mark it as optional, because initially it will not be available from the client
// the initial value will be requested from server
type Model = {
   Test: string
}

// The Msg type defines what events/actions can occur while the application is running
// the state of the application changes *only* in reaction to these events
type Msg =
    | Test


module Server =

    open Shared
    open Fable.Remoting.Client

    /// A proxy you can use to talk to server directly
    let api : IFilmClubApi =
      Remoting.createApi()
      |> Remoting.withRouteBuilder Route.builder
      |> Remoting.buildProxy<IFilmClubApi>

// defines the initial state
let init () : Model =
    { Test = "Hello world" }

// The update function computes the next state of the application based on the current state and the incoming events/messages
let update (msg : Msg) (currentModel : Model) : Model =
    currentModel

let stream model msgs =
    msgs

let view (model : Model) (dispatch : Msg -> unit) =
    div [] [ str model.Test ]

#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

Program.mkSimple init update view
|> Program.withStream stream "msgs"
#if DEBUG
|> Program.withConsoleTrace
#endif
|> Program.withReactBatched "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
