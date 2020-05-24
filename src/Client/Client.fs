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
open Routes

let auth0Config: AuthConfig = {
    allowSignUp = true
    allowedConnections = [|"Username-Password-Authentication"|]
    autoclose = true
    auth = {
        redirect = true
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
    | UserRegistered of User
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

let routeIsSame route1 route2 =
    let path1 = Routes.toPath route1
    let path2 = Routes.toPath route2
    path1 = path2

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
    | None -> { User = None; Route = Some Home }, Cmd.batch( seq { Navigation.modifyUrl (Routes.toPath Home); Cmd.ofSub checkAuthentication })
    | Some route -> { User = None; Route = Some route }, Cmd.ofSub checkAuthentication

let dispatchRoute dispatch route =
    dispatch (RouteCmd route)

let urlUpdate (optRoute: Route option) (model: Model) : Model * Cmd<Msg> =
    match optRoute with
    | None ->
        model, Cmd.none
    | Some route ->
        model.Route
            |> Option.map (routeIsSame route)
            |> Option.defaultValue false
            |> function
                | true -> model, Cmd.none
                | false ->
                    let cmd =
                        model.User
                            |> Option.bind (fun user -> Routes.getDataForRoute Server.api user.sub route)
                            |> Option.map (Utils.mapAsync RouteCmd)
                            |> Option.map Cmd.OfAsync.result
                            |> Option.defaultValue Cmd.none
                    { model with Route = Some route }, cmd

// The update function computes the next state of the application based on the current state and the incoming events/messages
let update (msg : Msg) (model : Model) : Model * Cmd<Msg> =
    match msg with
    | Login ->
        lock.show()
        model, Cmd.none
    | Logout ->
        lock.logout ()
        { model with User = None }, Navigation.newUrl (Routes.toPath Home)
    | Authenticated authResult ->
        model, Cmd.ofSub (getUserInfo authResult)
    | UserProfileLoaded userProfile ->
        let userCmd =
            Server.api.UpdateUser userProfile.sub userProfile.name
                |> Utils.mapAsync UserRegistered
                |> Cmd.OfAsync.result
        { model with User = Some userProfile }, userCmd
    | UserRegistered user ->
        let routeCmd =
            model.Route
                |> Option.bind (fun route -> Routes.getDataForRoute Server.api model.User.Value.sub route)
                |> Option.map (Utils.mapAsync RouteCmd)
                |> (function | Some asyn -> Cmd.OfAsync.result asyn | None -> Cmd.none)
        model, routeCmd
    | RouteCmd route ->
        model.User
            |> Option.bind (fun user -> Routes.getDataForRoute Server.api user.sub route)
            |> Option.map (Utils.mapAsync RouteCmd)
            |> Option.map Cmd.OfAsync.result
            |> function
                | Some cmd -> model, cmd
                | None -> { model with Route = Some route }, Navigation.newUrl (Routes.toPath route)

let stream model msgs =
    msgs

let view (model : Model) (dispatch : Msg -> unit) =
    let dispRoute = dispatchRoute dispatch
    let navbarFn = FilmClubNavBar.Component Server.api (fun () -> dispatch Logout) dispRoute model.User model.Route
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
|> Program.toNavigable Routes.urlParser urlUpdate
#if DEBUG
|> Program.withConsoleTrace
#endif
|> Program.withReactBatched "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
