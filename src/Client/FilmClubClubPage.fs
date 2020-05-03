module FilmClubClubPage

open Fable.React
open Fable.Reaction
open Shared
open Auth0
open FSharp.Control
open Fulma
open Routes

type private Msg =
    LoadClub of Club

type private Model = {
    Club: Club option
}

let private init optClub : Model =
    { Club = optClub }

let private view (model : Model) (dispatch : Msg -> unit) =
    match model.Club with
    | None -> Utils.LoadingPage "Loading club..."
    | Some x -> Content.content [ ] [
            h1 [] [ str x.Name ] ]

let private update (currentModel : Model) (msg : Msg) : Model =
    match msg with
    | LoadClub club ->
        { Club = Some club }

let private stream (model: Model) msgs =
    msgs |> AsyncRx.tag "msgs"


let Component (optClub: Club option) (api: IFilmClubApi) (dispatchRoute: Route -> unit) (user: IAuth0UserProfile) =
    let model = init optClub
    Reaction.StreamComponent model view update stream