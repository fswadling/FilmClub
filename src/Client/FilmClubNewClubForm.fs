module FilmClubNewClubForm

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

type private FormState =
    {
        Name: string
        Image: ImageType option
    }

    static member Decoder : Decoder<FormState> =
        Decode.object
            (fun get -> {
                Name = get.Required.Field "name" Decode.string
                Image = get.Required.Field "image" CustomFields.ImageInput.decoder
                })

type MyMsg =
    | OnFormMsg of FormBuilder.Types.Msg
    | SaveClub

type private Model = {
   FormState : FormBuilder.Types.State
   FormConfig: FormBuilder.Types.Config<MyMsg>
}

let initForm name image =
    Form<MyMsg>
        .Create(OnFormMsg)
        .AddField(
            BasicInput
                .Create("name")
                .WithLabel("Name")
                .WithPlaceholder("Example club name")
                .WithValue(name)
                .IsRequired()
                .WithDefaultView()
        )
        .AddField(
            CustomFields.BasicImageInput
                .Create("image")
                .WithLabel("Club image")
                .WithPlaceholder("Choose an image for the club")
                .WithValue(image)
                .IsRequired()
                .WithDefaultView()
        )
        .Build()

let private init name image =
    let (formState, formConfig) = initForm name image
    let (formState, formCmds) = Form.init formConfig formState
    { FormState = formState; FormConfig = formConfig }

let private getFrmState (json:string): FormState option =
    let body = Decode.fromString FormState.Decoder json
    match body with
    | Ok frmState -> Some frmState
    | _ -> None

let private update (save: string -> ImageType -> unit) (model : Model) (msg : MyMsg) : Model =
    match msg with
    | OnFormMsg msg ->
        let (formState, formCmd) = Form.update model.FormConfig msg model.FormState
        { model with FormState = formState }
    | SaveClub ->
        let json = Form.toJson model.FormConfig model.FormState
        let formState = getFrmState json
        formState
            |> Option.bind (fun state -> state.Image |> Option.map (fun image -> state.Name, image))
            |> Option.map (fun (name, image) -> save name image)
            |> ignore
        model

let private makeCall api state sub =
    AsyncRx.ofAsync (api.SaveNewClub state.Name state.Image.Value sub)

let private stream (model: Model) (msgs: IAsyncObservable<MyMsg>) =
    msgs |> AsyncRx.tag "msgs"

let clickButton dispatch () =
    dispatch SaveClub

let getImageElementToDisplay (imageOption: ImageType option): ReactElement =
    match imageOption with
    | None -> str "No image selected"
    | Some im -> str im.Name

let private getIsValid (model: Model) =
    let (state, isValid) = Form.validate model.FormConfig model.FormState
    isValid

let private view (header: string) (model : Model) (dispatch : MyMsg -> unit)  =
    Container.container [ Container.IsFluid; Container.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Left) ] ] [
        Content.content [ ] [
            h1 [] [ str header ]
            form [] [
                Form.render {
                    Config = model.FormConfig
                    State = model.FormState
                    Dispatch = dispatch
                    ActionsArea = (div [] [])
                    Loader = Form.DefaultLoader } ]
            Button.button [ Button.Disabled (not (getIsValid model)); Button.OnClick (fun _ -> dispatch SaveClub) ] [ str "Create Club" ] ] ]

let Component (title: string) name image (save: string -> ImageType -> unit) =
    let model = init name image
    Reaction.StreamComponent model (view title) (update save) stream