module FilmClubFilmPage

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

type private Msg =
    | Test

let private init =
    { Test = "test" }

let private update (dispatchRoute: Route -> unit) (model : Model) (msg : Msg) : Model =
    model

let private makeCall api name image sub =
    AsyncRx.ofAsync (api.SaveNewClub name image sub)

let private stream (api: IFilmClubApi) (user: IAuth0UserProfile) (model: Model) (msgs: IAsyncObservable<Msg>) =
    msgs
        |> AsyncRx.tag "msgs"

let private view (model : Model) (dispatch : Msg -> unit)  =
    h1 [] [ str "Club page" ]

let Component (film: Film) (api: IFilmClubApi) (dispatchRoute: Route -> unit) (user: IAuth0UserProfile) =
    let model = init
    Reaction.StreamComponent model view (update dispatchRoute) (stream api user)