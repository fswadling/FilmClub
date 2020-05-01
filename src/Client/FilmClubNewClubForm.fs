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
open Thoth.Json

type private FormState =
    { Name: string }

    static member Decoder : Decoder<FormState> =
        Decode.object
            (fun get -> { Name = get.Required.Field "name" Decode.string })

type private Model = {
   FormState : FormBuilder.Types.State
}

type MyMsg =
    | OnFormMsg of FormBuilder.Types.Msg
    | ClubSaved of Club
    | SaveClub

let (formState, formConfig) =
    Form<MyMsg>
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

let private update (model : Model) (msg : MyMsg) : Model =
    match msg with
    | OnFormMsg msg ->
        let (formState, formCmd) = Form.update formConfig msg model.FormState
        { model with FormState = formState }
    | SaveClub ->
        model
    | ClubSaved club ->
        model

let getName (json:string): string option =
    let body = Decode.fromString FormState.Decoder json
    match body with
    | Ok frmState -> Some frmState.Name
    | _ -> None

let makeCall api name sub =
    AsyncRx.ofAsync (api.saveNewClub name sub)

let private stream (api: IFilmClubApi) (user: IAuth0UserProfile) (model: Model) (msgs: IAsyncObservable<MyMsg>) =
    let json = Form.toJson formConfig model.FormState
    let nameOpt = getName json
    let newClubs =
        msgs
        |> AsyncRx.filter (function | SaveClub -> true | _ -> false)
        |> AsyncRx.choose (fun m -> nameOpt)
        |> AsyncRx.flatMapLatest (fun name -> makeCall api name user.sub)
        |> AsyncRx.map ClubSaved

    newClubs
        |> AsyncRx.merge msgs
        |> AsyncRx.tag json

let clickButton dispatch () =
    dispatch SaveClub

let private view (model : Model) (dispatch : MyMsg -> unit) =
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
                            str "Club image" ] ] ] ] ]
            Button.button [ Button.OnClick (fun _ -> dispatch SaveClub) ] [ str "Create Club" ] ] ]

let Component (api: IFilmClubApi) (user: IAuth0UserProfile) =
    let model = init
    Reaction.StreamComponent model view update (stream api user)