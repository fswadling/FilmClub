module FilmClubRouter

open Elmish
open Fable.React
open Auth0

open FilmClubHomePage
open Shared
open Routes

let renderRouteTarget (api: IFilmClubApi) (dispatchRoute: Route -> unit) (route: Route option) (user: IAuth0UserProfile) =
    match route with
    | Some Home -> FilmClubHomePage.Component api dispatchRoute user ()
    | Some NewClub -> FilmClubNewClubForm.Component api dispatchRoute user ()
    | Some (ClubRoute clubRouteArg) ->
        let optClub = clubRouteArg |> toOption
        FilmClubClubPage.Component optClub api dispatchRoute user ()
    | _ -> div [] [ Fable.React.Helpers.str "No route" ]





