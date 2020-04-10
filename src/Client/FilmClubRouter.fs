module FilmClubRouter
open Elmish.UrlParser
open Elmish
open Fable.React
open Auth0

open FilmClubHomePage
open Shared

type Route =
    | Home
    | Club of int

let router: Parser<Route -> Route, _> =
    oneOf
        [ Elmish.UrlParser.map Home (Elmish.UrlParser.s "home")
          Elmish.UrlParser.map Club (Elmish.UrlParser.s "club" </> i32) ]

let toPath route =
    match route with
    | Home -> "/#home"
    | Club id -> "/#club/" + id.ToString()

let renderRouteTarget (api: IFilmClubApi) (route: Route option) (user: IAuth0UserProfile) =
    match route with
    | Some Home -> FilmClubHomePage.Component api user ()
    | Some (Club c) -> div [] [ Fable.React.Helpers.str "Club" ]
    | _ -> div [] [ Fable.React.Helpers.str "No route" ]

let urlParser location = parseHash router location



