module FilmClubJoinClubPage

open Auth0
open Shared
open Fable.React
open Fable.Reaction
open Fulma
open FSharp.Control
open Utils

open Thoth.Elmish
open Thoth.Elmish.FormBuilder
open Thoth.Elmish.FormBuilder.BasicFields
open Thoth.Json
open Routes


type private Model = {
    Requests: ClubJoinRequest list option
}

type private JoinClubArgs = {
    UserId: string
    UserName: string
    ClubId: int
}

type private MyMsg =
    | RequestJoinClub of JoinClubArgs
    | RequestMade of Response<ClubJoinRequest>
    | LoadPendingRequests of ClubJoinRequest list

let private init =
    { Requests = None }

let private update (dispatchRoute: Route -> unit) (model : Model) (msg : MyMsg) : Model =
    match msg with
    | RequestMade requestResponse ->
        match requestResponse with
        | Valid request ->
            { model with Requests = Some (request::(model.Requests |> Option.defaultValue [])) }
        | _ ->
            model
    | RequestJoinClub args ->
        model
    | LoadPendingRequests requests ->
        { model with Requests = Some requests }

let private makeCall (api: IFilmClubApi) (args: JoinClubArgs) =
    AsyncRx.ofAsync (api.RequestJoinClub args.UserId args.UserName args.ClubId)

let private stream (api: IFilmClubApi) (user: IAuth0UserProfile) (model: Model) (msgs: IAsyncObservable<MyMsg>) =
    match model.Requests with
    | None ->
        (api.GetJoinClubRequestsForUser user.sub)
        |> AsyncRx.ofAsync
        |> AsyncRx.map LoadPendingRequests
        |> AsyncRx.tag "loadRequests"
    | _ ->
        let joinClubRequests =
            msgs
            |> AsyncRx.choose (function | RequestJoinClub joinArgs -> Some joinArgs | _ -> None)
            |> AsyncRx.flatMapLatest (fun args -> makeCall api args)
            |> AsyncRx.map RequestMade

        joinClubRequests
            |> AsyncRx.merge msgs
            |> AsyncRx.tag "msgs"

let private createRequestRow (request: ClubJoinRequest) =
    Content.content [ Content.Props [ Props.ClassName "request" ] ] [
        p [] [ str ("Request to join club with id: " + request.ClubId.ToString())]
        getRequestStatusText request.RequestStatus ]

let private view (user: IAuth0UserProfile) (api: IFilmClubApi) (model : Model) (dispatch : MyMsg -> unit)  =
    div [] [
        FilmClubJoinClubForm.Component (fun id -> dispatch (RequestJoinClub { UserId = user.sub; UserName = user.name; ClubId = id})) ()
        Container.container [ Container.IsFluid; Container.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Left) ] ] [
        yield! match model.Requests with
                | Some requests ->
                    List.map createRequestRow requests
                | None -> [ Icon.icon [Icon.Size IsLarge ] [ Fable.FontAwesome.Fa.i [ Fable.FontAwesome.Free.Fa.Solid.Spinner; Fable.FontAwesome.Fa.Spin ] []] ] ] ]

let Component (api: IFilmClubApi) (dispatchRoute: Route -> unit) (user: IAuth0UserProfile) =
    let model = init
    Reaction.StreamComponent model (view user api) (update dispatchRoute) (stream api user)