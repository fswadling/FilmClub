module FilmClubJoinClubPage

open Auth0
open Shared
open Fable.React
open Fable.Reaction
open Fulma
open FSharp.Control

open Thoth.Elmish
open Thoth.Elmish.FormBuilder
open Thoth.Elmish.FormBuilder.BasicFields
open Thoth.Json
open Routes


type private Model = {
    Test: string
}

type private JoinClubArgs = {
    UserId: string
    ClubId: int
}

type private MyMsg =
    | RequestJoinClub of JoinClubArgs
    | RequestMade of Response<ClubJoinRequest>

let private init =
    { Test = "test" }

let private update (dispatchRoute: Route -> unit) (model : Model) (msg : MyMsg) : Model =
    match msg with
    | RequestMade _ ->
        model
    | RequestJoinClub args ->
        model

let private makeCall (api: IFilmClubApi) (args: JoinClubArgs) =
    AsyncRx.ofAsync (api.RequestJoinClub args.UserId args.ClubId)

let private stream (api: IFilmClubApi) (model: Model) (msgs: IAsyncObservable<MyMsg>) =
    let joinClubRequests =
        msgs
        |> AsyncRx.choose (function | RequestJoinClub joinArgs -> Some joinArgs | _ -> None)
        |> AsyncRx.flatMapLatest (fun args -> makeCall api args)
        |> AsyncRx.map RequestMade

    joinClubRequests
        |> AsyncRx.merge msgs
        |> AsyncRx.tag "msgs"

let private view (user: IAuth0UserProfile) (api: IFilmClubApi) (model : Model) (dispatch : MyMsg -> unit)  =
    div [] [
        FilmClubJoinClubForm.Component (fun id -> dispatch (RequestJoinClub { UserId = user.sub; ClubId = id})) ()
        FilmClubPendingJoinClubRequests.Component api user () ]

let Component (api: IFilmClubApi) (dispatchRoute: Route -> unit) (user: IAuth0UserProfile) =
    let model = init
    Reaction.StreamComponent model (view user api) (update dispatchRoute) (stream api)