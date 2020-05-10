module FilmClubClubAdminPage

open Fable.React
open Fable.Reaction
open Shared
open Auth0
open FSharp.Control
open Fulma
open Routes

type private Msg =
    | UpdateClub of Club
    | ClubUpdated of Club

type private Model = {
    Club: Club option
}

let private init optClub : Model =
    { Club = optClub }

let private updateClub dispatch (club: Club) name image =
    let newClub = { club with Name = name; Image = image}
    dispatch (UpdateClub newClub)

let private view (model : Model) (dispatch : Msg -> unit) =
    match model.Club with
    | None -> Utils.LoadingPage "Loading club..."
    | Some club -> Content.content [ ] [
            FilmClubNewClubForm.Component "Edit existing club" club.Name (Some club.Image) (updateClub dispatch club) ()]

let private update (currentModel : Model) (msg : Msg) : Model =
    match msg with
    | UpdateClub club ->
        currentModel
    | ClubUpdated club ->
        Browser.Dom.console.log club
        { currentModel with Club = Some club }

let private makeCall (api: IFilmClubApi) userSub club =
    AsyncRx.ofAsync (api.UpdateClub userSub club)

let private stream (api: IFilmClubApi) (user: IAuth0UserProfile) (model: Model) msgs =
    let updatedClubs =
        msgs
        |> AsyncRx.choose (function | UpdateClub clubArgs -> Some clubArgs | _ -> None)
        |> AsyncRx.flatMapLatest (makeCall api user.sub)
        |> AsyncRx.choose (function | Valid club -> Some club | Invalid -> None)
        |> AsyncRx.map ClubUpdated

    updatedClubs
        |> AsyncRx.merge msgs
        |> AsyncRx.tag "msgs"

let Component (optClub: Club option) (api: IFilmClubApi) (dispatchRoute: Route -> unit) (user: IAuth0UserProfile) =
    let model = init optClub
    Reaction.StreamComponent model view update (stream api user)