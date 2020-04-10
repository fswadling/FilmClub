module FilmClubNewClubForm

open Auth0
open Shared
open Fable.React
open Fable.Reaction
open Fable.React.Props
open Fulma
open Elmish
open FSharp.Control

type private Model = {
   ClubName: string
}

type private Msg =
    | Login
    | LogOut

let private init  : Model =
    { ClubName = "" }

let private update (currentModel : Model) (msg : Msg) : Model =
    currentModel

let private stream model msgs =
    msgs
    |> AsyncRx.tag "msgs"

let private view (model : Model) (dispatch : Msg -> unit) =
    div [] [ str "Hello world"]

let Component (api: IFilmClubApi) (user: IAuth0UserProfile) =
    let model = init
    Reaction.StreamComponent model view update stream