module FilmClubClubPage

open Fable.React
open Fable.Reaction
open Shared
open Auth0
open FSharp.Control
open Fulma

type private Msg =
    LoadClub of Club

type private Model = {
    Club: Club option
}

let private init : Model =
    { Club = None }

let private view (model : Model) (dispatch : Msg -> unit) =
    match model.Club with
    | None -> Utils.LoadingPage "Loading club..."
    | Some x -> Content.content [ ] [
            h1 [] [ str x.Name ] ]

let private update (currentModel : Model) (msg : Msg) : Model =
    match msg with
    | LoadClub club ->
        { Club = Some club }

let private stream (api: IFilmClubApi) (clubId: int) (model: Model) msgs =
    match model.Club with
        | Some club -> msgs |> AsyncRx.tag "msgs"
        | None ->
            (api.GetClubById clubId)
                |> AsyncRx.ofAsync
                |> AsyncRx.map LoadClub
                |> AsyncRx.tag "loadingClub"

let Component (clubId: int) (api: IFilmClubApi) (dispatchRoute: Route -> unit) (user: IAuth0UserProfile) =
    let model = init
    let streamr = stream api clubId
    Reaction.StreamComponent model view update streamr