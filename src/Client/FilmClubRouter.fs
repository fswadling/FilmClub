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
    | Some NewClub -> FilmClubNewClubPage.Component api dispatchRoute user ()
    | Some JoinClub -> FilmClubJoinClubPage.Component api dispatchRoute user ()
    | Some (ClubRoute clubRouteArg) ->
        let optClub = clubRouteArg.EntityOrId |> toOption
        match clubRouteArg.SubRoute with
        | ClubMain -> FilmClubClubPage.Component optClub api dispatchRoute user ()
        | ClubAdmin -> FilmClubClubAdminPage.Component optClub api dispatchRoute user ()
        | ClubAddNewFilm ->
            optClub
                |> Option.map (fun c -> FilmClubAddNewFilmPage.Component c api dispatchRoute user ())
                |> Option.defaultValue (Utils.LoadingPage "Loading Club...")
        | ClubFilmPage filmRet ->
            let optFilm = filmRet |> toOption
            optClub
                |> Option.bind (fun c -> optFilm |> Option.map (fun f -> c, f))
                |> Option.map (fun (club, film) -> FilmClubFilmPage.Component club film api dispatchRoute user ())
                |> Option.defaultValue (Utils.LoadingPage "Loading Film...")
    | Some NotAllowed -> Utils.MessagePage "Not allowed"
    | _ -> div [] [ Fable.React.Helpers.str "No route" ]





