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


type private Model = {
    Test: string
}

type private FilmArgs = {
    Name: string
    Image: ImageType
}

type private MyMsg =
    | FilmSaved of Film
    | SaveFilm of FilmArgs

let private init =
    { Test = "test" }

let private update (club: Club) (dispatchRoute: Route -> unit) (model : Model) (msg : MyMsg) : Model =
    match msg with
    | SaveFilm clubArgs ->
        model
    | FilmSaved film ->
        dispatchRoute ((ClubRoute << (createClubRouteType ClubSubRoute.ClubMain) << ActualObject) club) |> ignore
        model

let private makeCall (api: IFilmClubApi) (name: string) (clubId: int) (image: ImageType) (sub: string) =
    AsyncRx.ofAsync (api.AddNewFilm name image clubId sub)

let private stream (api: IFilmClubApi) (clubId: int) (user: IAuth0UserProfile) (model: Model) (msgs: IAsyncObservable<MyMsg>) =
    let newClubs =
        msgs
        |> AsyncRx.choose (function | SaveFilm clubArgs -> Some clubArgs | _ -> None)
        |> AsyncRx.flatMapLatest (fun args -> makeCall api args.Name clubId args.Image user.sub)
        |> AsyncRx.map FilmSaved

    newClubs
        |> AsyncRx.merge msgs
        |> AsyncRx.tag "msgs"

let private saveFilm dispatch name image =
    dispatch (SaveFilm { Name = name; Image = image})

let private view (model : Model) (dispatch : MyMsg -> unit)  =
    FilmClubAddNewFilmForm.Component "New Film" "Add new film" "" None (saveFilm dispatch) ()

let Component (club: Club) (api: IFilmClubApi) (dispatchRoute: Route -> unit) (user: IAuth0UserProfile) =
    let model = init
    Reaction.StreamComponent model view (update club dispatchRoute) (stream api club.Id user)