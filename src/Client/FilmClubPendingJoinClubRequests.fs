module FilmClubPendingJoinClubRequests

open Auth0
open Shared
open Fable.React
open Fable.Reaction
open Fulma
open FSharp.Control


type private Model = {
    Requests: ClubJoinRequest list option
}

type private MyMsg =
    | LoadPendingRequests of ClubJoinRequest list

let private init =
    { Requests = None }

let private update (model : Model) (msg : MyMsg) : Model =
    match msg with
    | LoadPendingRequests requests ->
        { model with Requests = Some requests }

let private stream (api: IFilmClubApi) (user: IAuth0UserProfile) (model: Model) (msgs: IAsyncObservable<MyMsg>) =
    match model.Requests with
    | None ->
        (api.GetJoinClubRequestsForUser user.sub)
        |> AsyncRx.ofAsync
        |> AsyncRx.map LoadPendingRequests
        |> AsyncRx.tag "loadRequests"
    | _ -> msgs |> AsyncRx.tag "msgs"

let private getRequestStatusText (requestStatus: ClubJoinRequestStatus) =
    match requestStatus with
    | Pending -> Text.div [ Modifiers [ Modifier.TextColor IsGrey ] ] [ str "Pending" ]
    | Accepted -> Text.div [ Modifiers [ Modifier.TextColor IsSuccess ] ] [ str "Accepted" ]
    | Denied -> Text.div [ Modifiers [ Modifier.TextColor IsDanger ] ] [ str "Denied" ]

let private createRequestRow (request: ClubJoinRequest) =
    Content.content [ Content.Props [ Props.ClassName "request" ] ] [
        p [] [ str ("Request to join club with id: " + request.ClubId.ToString())]
        getRequestStatusText request.RequestStatus ]

let private view (model : Model) (dispatch : MyMsg -> unit)  =
    Container.container [ Container.IsFluid; Container.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Left) ] ] [
        Content.content [ ] [
            yield! model.Requests
                |> Option.map (List.map createRequestRow)
                |> Option.defaultValue [] ] ]

let Component api user =
    let model = init
    Reaction.StreamComponent model view update (stream api user)