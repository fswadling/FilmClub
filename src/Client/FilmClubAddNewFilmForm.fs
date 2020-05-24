module FilmClubAddNewFilmForm

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

type FilmFormArgs =  {
    Name: string
    Image: ImageType
    Description: string
}

type private FormState =
    {
        Name: string
        Image: ImageType option
        Description: string
    }

    static member Decoder : Decoder<FormState> =
        Decode.object
            (fun get -> {
                Name = get.Required.Field "name" Decode.string
                Image = get.Required.Field "image" CustomFields.ImageInput.decoder
                Description = get.Required.Field "description" Decode.string
                })

type private MyMsg =
    | OnFormMsg of FormBuilder.Types.Msg
    | SaveFilm

type private Model = {
   FormState : FormBuilder.Types.State
   FormConfig: FormBuilder.Types.Config<MyMsg>
}

let private initForm name image description =
    Form<MyMsg>
        .Create(OnFormMsg)
        .AddField(
            BasicInput
                .Create("name")
                .WithLabel("Name")
                .WithPlaceholder("Example film name")
                .WithValue(name)
                .IsRequired()
                .WithDefaultView()
        )
        .AddField(
            CustomFields.BasicImageInput
                .Create("image")
                .WithLabel("Film image")
                .WithPlaceholder("Choose an image for the film")
                .WithValue(image)
                .IsRequired()
                .WithDefaultView()
        )
        .AddField(
            BasicTextarea
                .Create("description")
                .WithLabel("Description")
                .WithPlaceholder("Enter a description for the film")
                .WithValue(description)
                .WithDefaultView()
        )
        .Build()

let private init name image description =
    let (formState, formConfig) = initForm name image description
    let (formState, formCmds) = Form.init formConfig formState
    { FormState = formState; FormConfig = formConfig }

let private getFrmState (json:string): FormState option =
    let body = Decode.fromString FormState.Decoder json
    match body with
    | Ok frmState -> Some frmState
    | _ -> None

let private getFormArgs (state: FormState): FilmFormArgs option =
    state.Image
        |> Option.map (fun img -> {
            Name = state.Name
            Image = img
            Description = state.Description})

let private update (save: FilmFormArgs -> unit) (model : Model) (msg : MyMsg) : Model =
    match msg with
    | OnFormMsg msg ->
        let (formState, formCmd) = Form.update model.FormConfig msg model.FormState
        { model with FormState = formState }
    | SaveFilm ->
        let json = Form.toJson model.FormConfig model.FormState
        let formState = getFrmState json
        formState
            |> Option.bind getFormArgs
            |> Option.map save
            |> ignore
        model

let private stream (model: Model) (msgs: IAsyncObservable<MyMsg>) =
    msgs |> AsyncRx.tag "msgs"

let getImageElementToDisplay (imageOption: ImageType option): ReactElement =
    match imageOption with
    | None -> str "No image selected"
    | Some im -> str im.Name

let private getIsValid (model: Model) =
    let (state, isValid) = Form.validate model.FormConfig model.FormState
    isValid

let private view (header: string) (saveBtnText: string) (model : Model) (dispatch : MyMsg -> unit)  =
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
            Button.button [ Button.Disabled (not (getIsValid model)); Button.OnClick (fun _ -> dispatch SaveFilm) ] [ str saveBtnText ] ] ]

let Component (title: string) (saveBtnText: string) name image description (save: FilmFormArgs -> unit) =
    let model = init name image description
    Reaction.StreamComponent model (view title saveBtnText) (update save) stream