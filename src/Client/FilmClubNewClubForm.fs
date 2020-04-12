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
    Container.container [ Container.IsFluid; Container.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Left) ] ] [
        Content.content [ ] [
            h1 [] [str "Create new club" ]
            form [] [
                Field.div [ ] [
                    Label.label [ ] [ str "Name"]
                    Control.div [ ] [ Input.text [ Input.Placeholder "Example club name" ]]]
                Field.div [ ] [
                    File.file [ File.HasName ] [ File.label [ ] [
                        File.input [ ]
                        File.cta [ ] [
                            File.icon [ ] [
                                Icon.icon [ ] [
                                    Fable.FontAwesome.Fa.i [ Fable.FontAwesome.Free.Fa.Solid.Upload ]  [ ] ] ]
                            File.label [ ] [
                                str "Choose an image for the club..." ] ]
                        File.name [ ] [
                            str "Club image" ] ] ] ]
            ]
        ] ]

let Component (api: IFilmClubApi) (user: IAuth0UserProfile) =
    let model = init
    Reaction.StreamComponent model view update stream