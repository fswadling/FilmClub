module FilmClubNewClubForm

open Auth0
open Shared
open Fable.React
open Fable.Reaction
open Fable.React.Props
open Fulma
open Elmish
open FSharp.Control

open Thoth.Elmish
open Thoth.Elmish.FormBuilder
open Thoth.Elmish.FormBuilder.BasicFields

type private Model = {
   FormState : FormBuilder.Types.State
}

type Msg =
    | OnFormMsg of FormBuilder.Types.Msg

let (formState, formConfig) =
    Form<Msg>
        .Create(OnFormMsg)
        .AddField(
            BasicInput
                .Create("name")
                .WithLabel("Name")
                .WithPlaceholder("Example club name")
                .IsRequired()
                .WithDefaultView()
        )
        .Build()

let private init =
    let (formState, formCmds) = Form.init formConfig formState
    { FormState = formState }

let private update (model : Model) (msg : Msg) : Model =
    match msg with
    | OnFormMsg msg ->
        let (formState, formCmd) = Form.update formConfig msg model.FormState
        { model with FormState = formState }

let private stream model msgs =
    msgs
    |> AsyncRx.tag "msgs"

let private view (model : Model) (dispatch : Msg -> unit) =
    Container.container [ Container.IsFluid; Container.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Left) ] ] [
        Content.content [ ] [
            h1 [] [str "Create new club" ]
            form [] [
                Form.render {
                    Config = formConfig
                    State = model.FormState
                    Dispatch = dispatch
                    ActionsArea = (div [] [])
                    Loader = Form.DefaultLoader }
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
                Field.div [ Field.IsGrouped ] [
                    Control.div [ ] [
                        Button.button [ Button.Color IsPrimary ] [ str "Create Club" ] ] ] ] ] ]

let Component (api: IFilmClubApi) (user: IAuth0UserProfile) =
    let model = init
    Reaction.StreamComponent model view update stream