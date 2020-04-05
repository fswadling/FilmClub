module Client

open Elmish
open Elmish.React
open Elmish.Streams
open FSharp.Control
open Fable.React
open FilmClubRouter
open Elmish.Navigation
open Fulma

open Shared

// The model holds data that you want to keep track of while the application is running
// in this case, we are keeping track of a counter
// we mark it as optional, because initially it will not be available from the client
// the initial value will be requested from server
type Model = {
   User: User option
   Route: Route option
}

// The Msg type defines what events/actions can occur while the application is running
// the state of the application changes *only* in reaction to these events
type Msg =
    | UserLoaded of User

let urlUpdate (result: Route option) model : Model * Cmd<Msg> =
    match result with
    | Some Home ->
        { model with Route = Some Home }, Cmd.none
    | Some (Club id) ->
        { model with Route = Some (Club id) }, Cmd.none
    | None ->
        model, Navigation.modifyUrl (FilmClubRouter.toPath Home)


module Server =

    open Shared
    open Fable.Remoting.Client

    /// A proxy you can use to talk to server directly
    let api : IFilmClubApi =
      Remoting.createApi()
      |> Remoting.withRouteBuilder Route.builder
      |> Remoting.buildProxy<IFilmClubApi>

let userObs () =
    Server.api.getUser ()
        |> AsyncRx.ofAsync
        |> AsyncRx.delay 2000
        |> AsyncRx.map UserLoaded
        |> AsyncRx.toStream "user"

// defines the initial state
let init route : Model * Cmd<Msg> =
    match route with
    | None -> { User = None; Route = Some Home }, (Navigation.modifyUrl (FilmClubRouter.toPath Home))
    | Some route -> { User = None; Route = Some route }, Cmd.none

// The update function computes the next state of the application based on the current state and the incoming events/messages
let update (msg : Msg) (currentModel : Model) : Model * Cmd<Msg> =
    match msg with
    | UserLoaded user ->
        { currentModel with User = Some user }, Cmd.none

let stream model msgs =
    match model.User with
    | None -> userObs ()
    | _ -> msgs

let view (model : Model) (dispatch : Msg -> unit) =
    let navbarFn = FilmClubNavBar.Component Server.api model.User
    let filmClubHomePageFn = FilmClubHomePage.Component Server.api
    div [] [
        navbarFn ()
        match model.User with
        | Some user -> FilmClubRouter.renderRouteTarget Server.api model.Route user
        | None -> Utils.LoadingPage "Loading User" ]




#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

Program.mkProgram init update view
|> Program.withStream stream "msgs"
|> Program.toNavigable FilmClubRouter.urlParser urlUpdate
#if DEBUG
|> Program.withConsoleTrace
#endif
|> Program.withReactBatched "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
