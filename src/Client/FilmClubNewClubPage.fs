module FilmClubNewClubPage

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

type private ClubArgs = {
    Name: string
    Image: ImageType
}

type private MyMsg =
    | ClubSaved of Club
    | SaveClub of ClubArgs

let private init =
    { Test = "test" }

let private update (dispatchRoute: Route -> unit) (model : Model) (msg : MyMsg) : Model =
    match msg with
    | SaveClub clubArgs ->
        model
    | ClubSaved club ->
        dispatchRoute ((ClubRoute << (createClubRouteType ClubSubRoute.ClubMain) << ActualObject) club) |> ignore
        model

let private makeCall api name image sub =
    AsyncRx.ofAsync (api.SaveNewClub name image sub)

let private stream (api: IFilmClubApi) (user: IAuth0UserProfile) (model: Model) (msgs: IAsyncObservable<MyMsg>) =
    let newClubs =
        msgs
        |> AsyncRx.choose (function | SaveClub clubArgs -> Some clubArgs | _ -> None)
        |> AsyncRx.flatMapLatest (fun args -> makeCall api args.Name args.Image user.sub)
        |> AsyncRx.map ClubSaved

    newClubs
        |> AsyncRx.merge msgs
        |> AsyncRx.tag "msgs"

let private saveClub dispatch name image =
    dispatch (SaveClub { Name = name; Image = image})

let private view (model : Model) (dispatch : MyMsg -> unit)  =
    FilmClubNewClubForm.Component "Create new club" "" None (saveClub dispatch) ()

let Component (api: IFilmClubApi) (dispatchRoute: Route -> unit) (user: IAuth0UserProfile) =
    let model = init
    Reaction.StreamComponent model view (update dispatchRoute) (stream api user)