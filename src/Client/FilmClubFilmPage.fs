module FilmClubFilmPage

open Auth0
open Shared
open Fable.React
open Fable.Reaction
open Fable.React.Props
open Fulma
open FSharp.Control

open Thoth.Elmish
open Thoth.Elmish.FormBuilder
open Thoth.Elmish.FormBuilder.BasicFields
open Thoth.Json
open Routes
open FilmClubAddNewFilmForm

type private Model = {
    Club: Club
    Film: Film
    User: IAuth0UserProfile
}

type private Msg =
    | UpdateFilm of Film
    | FilmUpdated of Film

let private init club film user =
    { Club = club; Film = film; User = user }

let private update (dispatchRoute: Route -> unit) (model : Model) (msg : Msg) : Model =
    match msg with
    | FilmUpdated film ->
        { model with Film = film }
    | _ ->
        model

let private makeCall (api: IFilmClubApi) (sub: string) (film: Film) =
    AsyncRx.ofAsync (api.UpdateFilm sub film)

let private stream (api: IFilmClubApi) (user: IAuth0UserProfile) (model: Model) (msgs: IAsyncObservable<Msg>) =
    let updateFilms =
        msgs
            |> AsyncRx.choose (function | UpdateFilm film -> Some film | _ -> None)
            |> AsyncRx.flatMapLatest (makeCall api user.sub)
            |> AsyncRx.choose (function | Valid film -> Some film | _ -> None)
            |> AsyncRx.map FilmUpdated

    msgs
        |> AsyncRx.merge updateFilms
        |> AsyncRx.tag "msgs"

let private updateFilmFn (model: Model) (dispatch: Msg -> unit) (filmArgs: FilmFormArgs)  =
    let newFilm = { model.Film with Name = filmArgs.Name; Image = filmArgs.Image; Description = filmArgs.Description }
    dispatch (UpdateFilm newFilm)

let private renderFilmReadOnly model =
    Content.content [ ] [
        br []
        Container.container [ Container.IsFluid; Container.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ] [
            Content.content [ ] [
                h1 [] [ str model.Film.Name ]
                img [ Class "film-read-only-image"; Src model.Film.Image.Image ]
                p [] [ str model.Film.Description ] ] ] ]

let private view (model : Model) (dispatch : Msg -> unit)  =
    match model.User.sub = model.Film.UserId || model.User.sub = model.Club.OwnerId with
    | true ->
        FilmClubAddNewFilmForm.Component model.Film.Name "Update film" model.Film.Name (Some model.Film.Image) model.Film.Description (updateFilmFn model dispatch) ()
    | false ->
        renderFilmReadOnly model

let Component (club: Club) (film: Film) (api: IFilmClubApi) (dispatchRoute: Route -> unit) (user: IAuth0UserProfile) =
    let model = init club film user
    Reaction.StreamComponent model view (update dispatchRoute) (stream api user)