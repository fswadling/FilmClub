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

type Route =
    | Home
    | ClubRoute of EntityOrId<Club>
    | NewClub

let router: Parser<Route -> Route, _> =
    oneOf
        [ Elmish.UrlParser.map Home (Elmish.UrlParser.s "home")
          Elmish.UrlParser.map (ClubRoute << OnlyId) (Elmish.UrlParser.s "club" </> i32)
          Elmish.UrlParser.map NewClub (Elmish.UrlParser.s "new-club") ]

let getIdString (getId: 'a -> int) (routeVar: EntityOrId<'a>) =
    match routeVar with
    | OnlyId id -> id.ToString()
    | ActualObject obj -> (getId obj).ToString()

let toPath route =
    match route with
    | Home -> "/home"
    | ClubRoute routeVar -> "/club/" + (getIdString (fun club -> club.Id) routeVar)
    | NewClub -> "/new-club"

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
        entityOrId
            |> (getDataForRouteVariable api.GetClubById)
            |> Option.map (mapGetDataBackToRoute ClubRoute)
    | _ -> None

let urlParser location = parsePath router location