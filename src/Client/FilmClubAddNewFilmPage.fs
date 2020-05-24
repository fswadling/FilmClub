module FilmClubAddNewFilmPage

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
open FilmClubAddNewFilmForm


type private Model = {
    Test: string
}

type private MyMsg =
    | FilmSaved of Film
    | SaveFilm of FilmFormArgs

let private init =
    { Test = "test" }

let private update (club: Club) (dispatchRoute: Route -> unit) (model : Model) (msg : MyMsg) : Model =
    match msg with
    | SaveFilm clubArgs ->
        model
    | FilmSaved film ->
        dispatchRoute ((ClubRoute << (createClubRouteType ClubSubRoute.ClubMain) << ActualObject) club) |> ignore
        model

let private makeCall (api: IFilmClubApi) (clubId: int) (sub: string) (args: FilmFormArgs) =
    AsyncRx.ofAsync (api.AddNewFilm args.Name args.Image args.Description clubId sub)

let private stream (api: IFilmClubApi) (clubId: int) (user: IAuth0UserProfile) (model: Model) (msgs: IAsyncObservable<MyMsg>) =
    let newClubs =
        msgs
        |> AsyncRx.choose (function | SaveFilm clubArgs -> Some clubArgs | _ -> None)
        |> AsyncRx.flatMapLatest (fun args -> makeCall api clubId user.sub args)
        |> AsyncRx.map FilmSaved

    newClubs
        |> AsyncRx.merge msgs
        |> AsyncRx.tag "msgs"

let private saveFilm dispatch args =
    dispatch (SaveFilm args)

let private view (model : Model) (dispatch : MyMsg -> unit)  =
    FilmClubAddNewFilmForm.Component "New Film" "Add new film" "" None "" (saveFilm dispatch) ()

let Component (club: Club) (api: IFilmClubApi) (dispatchRoute: Route -> unit) (user: IAuth0UserProfile) =
    let model = init
    Reaction.StreamComponent model view (update club dispatchRoute) (stream api club.Id user)