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
open Fable.Core.JsInterop

open Shared

let auth0Config: AuthConfig = {
    allowSignUp = true
    allowedConnections = [|"Username-Password-Authentication"|]
    autoclose = true
    auth = {
        redirect = false
        redirectUrl = "http://localhost:8080"
        responseType = "token"
    }
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
    | UserProfileLoaded of IAuth0UserProfile
    | RegisteredInBackend of IAuth0UserProfile
    | RouteCmd of Route

let lock = Auth0Lock.CreateWithConfig auth0Credentials

let getUserInfo (authResult: IAuthResult) push =
    lock.getUserInfo (authResult.accessToken, (fun err userProfile ->
        UserProfileLoaded userProfile |> push))

let onAuthenticated push =
    lock.on_authenticated (fun authResult -> Authenticated authResult |> push)

let authenticationSub model =
    Cmd.ofSub onAuthenticated

let onCheckSessionComplete push result =
    match result with
    | Result result -> Authenticated result |> push
    | Error error -> Login |> push

let checkAuthentication (push: Msg -> unit) =
    Auth0.checkSessionHelper lock {scope = "openid profile email" } (onCheckSessionComplete push)

let urlUpdate (route: Route option) (model: Model) : Model * Cmd<Msg> =
    match route with
    | None ->
        model, Cmd.none
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
    | None -> { User = None; Route = Some Home }, Cmd.batch( seq { Navigation.modifyUrl (FilmClubRouter.toPath Home); Cmd.ofSub checkAuthentication })
    | Some route -> { User = None; Route = Some route }, Cmd.ofSub checkAuthentication

let onUserRegisterComplete userProfile newUser =
    RegisteredInBackend userProfile

let dispatchRoute dispatch route =
    dispatch (RouteCmd route)

// The update function computes the next state of the application based on the current state and the incoming events/messages
let update (msg : Msg) (model : Model) : Model * Cmd<Msg> =
    match msg with
    | Login ->
        lock.show()
        model, Cmd.none
    | Logout ->
        lock.logout ()
        { model with User = None }, Navigation.newUrl (FilmClubRouter.toPath Home)
    | Authenticated authResult ->
        model, Cmd.ofSub (getUserInfo authResult)
    | UserProfileLoaded userProfile ->
        model, Cmd.OfAsync.perform (Server.api.registerUser) userProfile.sub (onUserRegisterComplete userProfile)
    | RegisteredInBackend userProfile ->
        { model with User = Some userProfile }, Cmd.none
    | RouteCmd route ->
        { model with Route = Some route }, Navigation.newUrl (FilmClubRouter.toPath route)

let stream model msgs =
    match model.User with
    | None -> msgs
    | _ -> msgs

let view (model : Model) (dispatch : Msg -> unit) =
    let navbarFn = FilmClubNavBar.Component Server.api (fun () -> dispatch Logout) model.User
    let dispRoute = dispatchRoute dispatch
    div [] [
        navbarFn ()
        match model.User with
        | Some user -> FilmClubRouter.renderRouteTarget Server.api dispRoute model.Route user
        | None -> FilmClubLandingPage.render ]

#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

Program.mkProgram init update view
|> Program.withStream stream "msgs"
|> Program.withSubscription authenticationSub
|> Program.toNavigable FilmClubRouter.urlParser urlUpdate
#if DEBUG
|> Program.withConsoleTrace
#endif
|> Program.withReactBatched "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
