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

type ClubRouteType = {
    EntityOrId: EntityOrId<Club>
    SubRoute: ClubSubRoute
}

type Route =
    | Home
    | ClubRoute of ClubRouteType
    | NewClub
    | NotAllowed

let createClubRouteType subRoute entityOrId: ClubRouteType = {
    EntityOrId = entityOrId
    SubRoute = subRoute
}

let router: Parser<Route -> Route, _> =
    oneOf
        [ Elmish.UrlParser.map Home (s "home")
          Elmish.UrlParser.map NotAllowed (s "not-allowed")
          Elmish.UrlParser.map (ClubRoute << (createClubRouteType ClubAdmin) << OnlyId) (s "club" </> i32 </> s "admin")
          Elmish.UrlParser.map (ClubRoute << (createClubRouteType ClubMain) << OnlyId) (s "club" </> i32)
          Elmish.UrlParser.map NewClub (s "new-club") ]

let getIdString (getId: 'a -> int) (routeVar: EntityOrId<'a>) =
    match routeVar with
    | OnlyId id -> id.ToString()
    | ActualObject obj -> (getId obj).ToString()

let toClubPath clubSubRoute =
    match clubSubRoute with
    | ClubMain -> ""
    | ClubAdmin -> "/admin"

let toPath route =
    match route with
    | Home -> "/home"
    | ClubRoute routeVar -> "/club/" + (getIdString (fun club -> club.Id) routeVar.EntityOrId) + toClubPath routeVar.SubRoute
    | NewClub -> "/new-club"
    | NotAllowed -> "/not-allowed"

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

let getDataForRoute (api: IFilmClubApi) (route: Route) =
    match route with
    | ClubRoute entityOrId ->
        entityOrId.EntityOrId
            |> (getDataForRouteVariable api.GetClubById)
            |> Option.map (mapGetDataBackToRoute (ClubRoute << (createClubRouteType entityOrId.SubRoute)))
    | _ -> None

let urlParser location = parsePath router location