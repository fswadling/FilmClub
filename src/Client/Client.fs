module Client

open Elmish
open Elmish.React
open Elmish.Streams
open FSharp.Control
open Fable.React
open FilmClubRouter
open Elmish.Navigation
open Fulma
open Auth0

open Shared

let auth0Config: AuthConfig = {
    allowSignUp = true
    allowedConnections = [|"Username-Password-Authentication"|]
    autoclose = true
}

let auth0Credentials = ("RYOW08F8w735kMjIIqODMkxOcY1UStBk", "dev-9doum9hw.auth0.com", auth0Config)

// The model holds data that you want to keep track of while the application is running
// in this case, we are keeping track of a counter
// we mark it as optional, because initially it will not be available from the client
// the initial value will be requested from server
type Model = {
   User: IAuth0UserProfile option
   Route: Route option
}

// The Msg type defines what events/actions can occur while the application is running
// the state of the application changes *only* in reaction to these events
type Msg =
    | Login
    | Logout
    | Authenticated of IAuthResult
    | SetUserProfile of IAuth0UserProfile

let lock = Auth0Lock.CreateWithConfig auth0Credentials

let getUserInfo (authResult: IAuthResult) push =
  lock.getUserInfo (authResult.accessToken, fun auth0Error userProfile -> SetUserProfile userProfile |> push)

let onAuthenticated push =
  lock.on_authenticated (fun authResult -> Authenticated authResult |> push)

let urlUpdate (route: Route option) (model: Model) : Model * Cmd<Msg> =
    match route with
    | None ->
        model, Navigation.modifyUrl (FilmClubRouter.toPath Home)
    | route ->
        { model with Route = route }, Cmd.none


module Server =

    open Shared
    open Fable.Remoting.Client

    /// A proxy you can use to talk to server directly
    let api : IFilmClubApi =
      Remoting.createApi()
      |> Remoting.withRouteBuilder Route.builder
      |> Remoting.buildProxy<IFilmClubApi>

// defines the initial state
let init route : Model * Cmd<Msg> =
    match route with
    | None -> { User = None; Route = Some Home }, Cmd.none
    | Some route -> { User = None; Route = Some route }, Cmd.none

// The update function computes the next state of the application based on the current state and the incoming events/messages
let update (msg : Msg) (model : Model) : Model * Cmd<Msg> =
    match msg with
    | Login ->
        lock.show()
        model, Cmd.ofSub (onAuthenticated)
    | Logout ->
        { model with User = None }, Navigation.modifyUrl (FilmClubRouter.toPath Home)
    | Authenticated authResult ->
        model, Cmd.ofSub (getUserInfo authResult)
    | SetUserProfile userProfile ->
        { model with User = Some userProfile }, Cmd.none

let stream model msgs =
    match model.User with
    | None -> msgs
    | _ -> msgs

let view (model : Model) (dispatch : Msg -> unit) =
    let navbarFn = FilmClubNavBar.Component Server.api (fun () -> dispatch Logout) model.User
    let filmClubHomePageFn = FilmClubHomePage.Component Server.api
    div [] [
        navbarFn ()
        match model.User with
        | Some user -> FilmClubRouter.renderRouteTarget Server.api model.Route user
        | None -> FilmClubLandingPage.Component Server.api (fun () -> dispatch Login) () ]

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
