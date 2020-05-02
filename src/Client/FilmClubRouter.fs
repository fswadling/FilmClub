module FilmClubRouter
open Elmish.UrlParser
open Elmish
open Fable.React
open Auth0

open FilmClubHomePage
open Shared

let router: Parser<Route -> Route, _> =
    oneOf
        [ Elmish.UrlParser.map Home (Elmish.UrlParser.s "home")
          Elmish.UrlParser.map Club (Elmish.UrlParser.s "club/" </> i32)
          Elmish.UrlParser.map NewClub (Elmish.UrlParser.s "new-club") ]

let toPath route =
    match route with
    | Home -> "/home"
    | Club id -> "/club/" + id.ToString()
    | NewClub -> "/new-club"

let renderRouteTarget (api: IFilmClubApi) (dispatchRoute: Route -> unit) (route: Route option) (user: IAuth0UserProfile) =
    match route with
    | Some Home -> FilmClubHomePage.Component api dispatchRoute user ()
    | Some NewClub -> FilmClubNewClubForm.Component api dispatchRoute user ()
    | Some (Club id) -> FilmClubClubPage.Component id api dispatchRoute user ()
    | _ -> div [] [ Fable.React.Helpers.str "No route" ]

let urlParser location = parsePath router location



