module FilmClubPendingJoinRequests

open Fable.React
open Fable.Reaction
open Shared
open Auth0
open FSharp.Control
open Fulma
open Routes
open Utils

type private Msg =
    | RequestsLoaded of ClubJoinRequest list
    | AllowRequest of ClubJoinRequest
    | RequestAllowed of ClubJoinRequest
    | DenyRequest of ClubJoinRequest
    | RequestDenied of ClubJoinRequest

type private Model = {
    Requests: ClubJoinRequest list option
}

let private init : Model =
    { Requests = None }

let private renderRequest (dispatch: Msg -> unit) (request: ClubJoinRequest) =
    Card.card [ ] [
        Card.header [ ] [
            Card.Header.title [ ] [
                str "Join club request" ] ]
        Card.content [ ] [
            Content.content [ ] [
                p [] [ str ("User: " + request.UserName) ]
                getRequestStatusText request.RequestStatus ] ]
        Card.footer [ ] [
            Card.Footer.a [ GenericOption.Props [ Props.OnClick (fun e -> (dispatch << AllowRequest) request) ] ] [
                str "Allow" ]
            Card.Footer.a [ GenericOption.Props [ Props.OnClick (fun e -> (dispatch << DenyRequest) request) ] ] [
                str "Deny" ] ] ]

let private view (model : Model) (dispatch : Msg -> unit) =
    match model.Requests with
    | None ->
        Icon.icon [Icon.Size IsLarge ] [ Fable.FontAwesome.Fa.i [ Fable.FontAwesome.Free.Fa.Solid.Spinner; Fable.FontAwesome.Fa.Spin ] [] ]
    | Some requests ->
        Container.container [ Container.IsFluid; Container.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Left) ] ] [
            yield! (List.map (renderRequest dispatch) requests) ]

let replaceRequest request requests =
    requests |> List.map (fun req -> match req.Id = request.Id with |true -> request |false -> req)

let private update (model : Model) (msg : Msg) : Model =
    match msg with
    | RequestsLoaded requests ->
        { model with Requests = Some requests }
    | RequestAllowed request ->
        { model with Requests = model.Requests |> Option.map (replaceRequest request) }
    | RequestDenied request ->
        { model with Requests = model.Requests |> Option.map (replaceRequest request) }
    | _ ->
        model

let private makeCall (api: IFilmClubApi) userSub club =
    AsyncRx.ofAsync (api.UpdateClub userSub club)

let approveRequest (api: IFilmClubApi) (requestId: int) =
    AsyncRx.ofAsync (api.AllowRequest requestId)

let denyRequest (api: IFilmClubApi) (requestId: int) =
    AsyncRx.ofAsync (api.DenyRequest requestId)

let private stream (club: Club) (api: IFilmClubApi) (model: Model) msgs =
    match model.Requests with
    | None ->
        api.GetJoinClubRequestsForClub club.Id
            |> AsyncRx.ofAsync
            |> AsyncRx.map RequestsLoaded
            |> AsyncRx.tag "loadRequests"
    | _ ->
        let approveRequests =
            msgs
                |> AsyncRx.choose (function | AllowRequest request -> Some request | _ -> None)
                |> AsyncRx.flatMapLatest (fun req -> approveRequest api req.Id)
                |> AsyncRx.choose (function | Valid response -> Some response | _ -> None)
                |> AsyncRx.map RequestAllowed

        let denyRequests =
            msgs
                |> AsyncRx.choose (function |DenyRequest request -> Some request | _ -> None)
                |> AsyncRx.flatMapLatest (fun req -> denyRequest api req.Id)
                |> AsyncRx.choose (function | Valid response -> Some response | _ -> None)
                |> AsyncRx.map RequestDenied
        msgs
            |> AsyncRx.merge approveRequests
            |> AsyncRx.merge denyRequests
            |> AsyncRx.tag "msgs"

let Component (club: Club) (api: IFilmClubApi) =
    let model = init
    Reaction.StreamComponent model view update (stream club api)