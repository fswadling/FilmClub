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
type Model = { Films: Film list option }

// The Msg type defines what events/actions can occur while the application is running
// the state of the application changes *only* in reaction to these events
type Msg =
    | InitialFilmsLoaded of Film list


module Server =

    open Shared
    open Fable.Remoting.Client

    /// A proxy you can use to talk to server directly
    let api : IFilmsApi =
      Remoting.createApi()
      |> Remoting.withRouteBuilder Route.builder
      |> Remoting.buildProxy<IFilmsApi>

let getFilms = Server.api.getFilms

// defines the initial state
let init () : Model =
    { Films = None }

// The update function computes the next state of the application based on the current state and the incoming events/messages
let update (msg : Msg) (currentModel : Model) : Model =
    match msg with
    | InitialFilmsLoaded initialFilms ->
        { currentModel with Films = Some initialFilms }


let load = AsyncRx.ofAsync (getFilms ())

let loadCount =
    load
    |> AsyncRx.map InitialFilmsLoaded
    |> AsyncRx.toStream "loading"

let stream model msgs =
    match model.Films with
    | None -> loadCount
    | _ -> msgs

let safeComponents =
    span [ ]
        [ str "Version "
          strong [ ] [ str Version.app ]
          str " powered by SAFE stack"
        ]

let button txt onClick =
    Button.button
        [ Button.IsFullWidth
          Button.Color IsPrimary
          Button.OnClick onClick ]
        [ str txt ]

let view (model : Model) (dispatch : Msg -> unit) =
    div []
        [ Navbar.navbar [ Navbar.Color IsBlack ]
            [ Navbar.Item.div [ ]
                [ Heading.h2 [ Heading.Modifiers [ Modifier.TextColor IsWhite ] ]
                    [ str "Film Club" ] ] ]

          Footer.footer [ ]
                [ Content.content [ Content.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ]
                    [ safeComponents ] ] ]


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
