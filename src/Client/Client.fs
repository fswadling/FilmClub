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
    Films: Film list option;
    User: User option;
}

// The Msg type defines what events/actions can occur while the application is running
// the state of the application changes *only* in reaction to these events
type Msg =
    | InitialFilmsLoaded of Film list
    | UserLoaded of User


module Server =

    open Shared
    open Fable.Remoting.Client

    /// A proxy you can use to talk to server directly
    let api : IFilmClubApi =
      Remoting.createApi()
      |> Remoting.withRouteBuilder Route.builder
      |> Remoting.buildProxy<IFilmClubApi>

let getFilms = Server.api.getFilms
let getUser = Server.api.getUser

// defines the initial state
let init () : Model =
    { Films = None; User = None; }

// The update function computes the next state of the application based on the current state and the incoming events/messages
let update (msg : Msg) (currentModel : Model) : Model =
    match msg with
    | InitialFilmsLoaded initialFilms ->
        { currentModel with Films = Some initialFilms }
    | UserLoaded user ->
        { currentModel with User = Some user }

let filmsObs = AsyncRx.ofAsync (getFilms ())
let userObs = AsyncRx.ofAsync (getUser ())

let filmsMsgs =
    filmsObs
    |> AsyncRx.map InitialFilmsLoaded

let userMsgs =
    userObs
    |> AsyncRx.map UserLoaded

let loadStream =
    AsyncRx.merge filmsMsgs userMsgs
    |> AsyncRx.toStream("msgs")

let stream model msgs =
    match model.Films with
    | None -> loadStream
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

let renderFilm (film: Film) =
    div [ ClassName "card" ] [
        div [ ClassName "card-title" ] [
            str film.Name
        ]
    ]

let renderUserNavbarItems userOption =
    match userOption with
        | Some user -> [
            Navbar.End.div [ ] [
                Navbar.Item.div [ Navbar.Item.HasDropdown; Navbar.Item.IsHoverable ] [
                    Navbar.Link.div [ Navbar.Link.Modifiers [ Modifier.TextColor IsWhite ] ] [
                        str user.Name
                    ]
                    Navbar.Dropdown.div  [  ] [
                        Navbar.Item.a [ ] [
                            str "Log out"
                        ]
                    ]
                ]
            ]
            ]
        | None -> []

let renderNavBar model =
    Navbar.navbar [ Navbar.Color IsBlack ] 
        ((Navbar.Item.div [ ] [
            Heading.h2 [ Heading.Modifiers [ Modifier.TextColor IsWhite ] ] [
                str "Film Club"
            ]
        ])::(renderUserNavbarItems model.User))

let renderFooter() =
    Footer.footer [ ] [
        Content.content [ Content.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ] [
            safeComponents
        ]
    ]
    

let view (model : Model) (dispatch : Msg -> unit) =
    div [] [
        renderNavBar model
        div [ ClassName "card-list" ] [
            yield! match model.Films with
                | Some films -> Seq.map renderFilm films
                | None -> Seq.empty
        ]
        renderFooter ()
    ]

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
