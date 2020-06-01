module Routes

open Elmish.UrlParser
open Shared
open Microsoft.FSharp.Control
open Utils

type EntityOrId<'TEntity> =
    | ActualObject of 'TEntity
    | OnlyId of int

let toOption<'a> (routeArg: EntityOrId<'a>) =
    match routeArg with
    | ActualObject obj -> Some obj
    | OnlyId id -> None

type ClubSubRoute =
    | ClubMain
    | ClubAdmin
    | ClubAddNewFilm
    | ClubFilmPage of EntityOrId<Film>

type ClubRouteType = {
    EntityOrId: EntityOrId<Club>
    SubRoute: ClubSubRoute
}

type Route =
    | Home
    | ClubRoute of ClubRouteType
    | NewClub
    | JoinClub
    | NotAllowed

let createClubRouteType subRoute entityOrId: ClubRouteType = {
    EntityOrId = entityOrId
    SubRoute = subRoute
}

let createClubFilmRouteType clubId filmId =
    let filmRouteVar = (ClubFilmPage << OnlyId) filmId
    let x = createClubRouteType filmRouteVar (OnlyId clubId)
    ClubRoute x

let curry f x y = f (x,y)

let router: Parser<Route -> Route, _> =
    oneOf
        [ Elmish.UrlParser.map Home (s "home")
          Elmish.UrlParser.map NotAllowed (s "not-allowed")
          Elmish.UrlParser.map (ClubRoute << (createClubRouteType ClubAdmin) << OnlyId) (s "club" </> i32 </> s "admin")
          Elmish.UrlParser.map (ClubRoute << (createClubRouteType ClubAddNewFilm) << OnlyId) (s "club"</> i32 </> s "add-film")
          Elmish.UrlParser.map createClubFilmRouteType (s "club" </> i32 </> s "film" </> i32)
          Elmish.UrlParser.map (ClubRoute << (createClubRouteType ClubMain) << OnlyId) (s "club" </> i32)
          Elmish.UrlParser.map NewClub (s "new-club")
          Elmish.UrlParser.map JoinClub (s "join-club") ]

let getIdString<'a> (getId: 'a -> int) (routeVar: EntityOrId<'a>) =
    match routeVar with
    | OnlyId id -> id.ToString()
    | ActualObject obj -> (getId obj).ToString()

let toClubPath clubSubRoute =
    match clubSubRoute with
    | ClubMain -> ""
    | ClubAdmin -> "/admin"
    | ClubAddNewFilm -> "/add-film"
    | ClubFilmPage routeVar -> "/film/" + (getIdString<Film> (fun film -> film.Id) routeVar)

let toPath route =
    match route with
    | Home -> "/home"
    | ClubRoute routeVar -> "/club/" + (getIdString<Club> (fun club -> club.Id) routeVar.EntityOrId) + toClubPath routeVar.SubRoute
    | NewClub -> "/new-club"
    | NotAllowed -> "/not-allowed"
    | JoinClub -> "/join-club"

let private getDataForRouteVariable (getDataFromId: int -> Async<'a>) (routeVar: EntityOrId<'a>) =
    match routeVar with
    | ActualObject x -> None
    | OnlyId id ->
        getDataFromId id
            |> Utils.mapAsync ActualObject
            |> Some

let private mapGetDataBackToRoute<'a> (transFormToRoute: 'a -> Route) (getData: Async<'a>) =
    async {
        let! data = getData
        return transFormToRoute data
    }

let mapClubResponseToRoute (subRoute: ClubSubRoute) (response: Response<Club>) =
    match response with
    | Valid club ->
        let entity = ActualObject club
        ClubRoute (createClubRouteType subRoute entity)
    | Invalid -> NotAllowed

let mapFilmResponseToRoute (club: Club) (response: Response<Film>) =
    match response with
    | Valid film ->
        let filmEntity = ActualObject film
        let subRoute = ClubFilmPage filmEntity
        let clubEntity = ActualObject club
        ClubRoute { EntityOrId = clubEntity; SubRoute = subRoute }
    | Invalid -> NotAllowed

let getDataForRoute (api: IFilmClubApi) (userId: string) (route: Route) =
    match route with
    | ClubRoute clubRouteType ->
        match clubRouteType.EntityOrId with
        | ActualObject club ->
            match (List.contains userId club.MemberIds) with
            | true ->
                match clubRouteType.SubRoute with
                | ClubFilmPage filmRouteType ->
                    match filmRouteType with
                    | ActualObject film -> None
                    | OnlyId filmId ->
                        let response = api.GetFilm userId filmId
                        response
                            |> Utils.mapAsync (mapFilmResponseToRoute club)
                            |> Some
                | route -> None

            | false -> Some (async { return NotAllowed })
        | OnlyId id ->
            let response = api.GetClubById userId id
            response
                |> Utils.mapAsync (mapClubResponseToRoute clubRouteType.SubRoute)
                |> Some
    | _ -> None

let urlParser location = parsePath router location