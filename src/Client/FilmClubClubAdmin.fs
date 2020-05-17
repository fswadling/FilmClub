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

let private view api (model : Model) (dispatch : Msg -> unit) =
    match model.Club with
    | None -> Utils.LoadingPage "Loading club..."
    | Some club -> Content.content [ ] [
            Container.container [ Container.IsFluid; Container.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Left) ] ] [
                Content.content [] [
                    p [] [ str ("The club id is " + club.Id.ToString() + ". Share this with your friends to let them join your club!")] ] ]
            br []
            FilmClubNewClubForm.Component "Edit club" "Update club" club.Name (Some club.Image) (updateClub dispatch club) ()
            br []
            FilmClubPendingJoinRequests.Component club api () ]

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
    Reaction.StreamComponent model (view api) update (stream api user)