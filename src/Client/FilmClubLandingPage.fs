module FilmClubLandingPage

open Shared
open Fable.React
open Fable.Reaction
open Fulma

type private Model =
    | None

type private Msg =
    | Login

let private init : Model =
    None

let private update (login: unit -> unit) (currentModel : Model) (msg : Msg) : Model =
    match msg with
    |Login ->
        login ()
        currentModel

let private stream model msgs =
    msgs
    |> AsyncRx.tag "msgs"

let private view (model : Model) (dispatch : Msg -> unit) =
    Hero.hero [ Hero.Color IsWhite; Hero.IsFullHeight ] [
        Hero.body [] [
            Container.container [ Container.IsFluid; Container.Modifiers [
                Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ] [
                    Content.content [ ] [
                        h1 [] [ str "Welcome to Fred's Film Club!"]
                        Button.button [ Button.OnClick (fun _ -> dispatch Login)] [ str "Enter" ] ] ] ] ]


let Component (api: IFilmClubApi) (login: unit -> unit) =
    let model = init
    let reducedUpdate = update login
    Reaction.StreamComponent model view reducedUpdate stream
