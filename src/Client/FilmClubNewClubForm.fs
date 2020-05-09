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
        .AddField(
            CustomFields.BasicImageInput
                .Create("image")
                .WithLabel("Club image")
                .WithPlaceholder("Choose an image for the club")
                .IsRequired()
                .WithDefaultView()
        )
        .Build()

let private init =
    let (formState, formCmds) = Form.init formConfig formState
    { FormState = formState }

let private update (dispatchRoute: Route -> unit) (model : Model) (msg : MyMsg) : Model =
    match msg with
    | OnFormMsg msg ->
        let (formState, formCmd) = Form.update formConfig msg model.FormState
        { model with FormState = formState }
    | SaveClub ->
        model
    | ClubSaved club ->
        dispatchRoute ((ClubRoute << (createClubRouteType ClubSubRoute.ClubMain) << ActualObject) club) |> ignore
        model

let private getFrmState (json:string): FormState option =
    let body = Decode.fromString FormState.Decoder json
    match body with
    | Ok frmState -> Some frmState
    | _ -> None

let private makeCall api state sub =
    AsyncRx.ofAsync (api.SaveNewClub state.Name state.Image sub)

let private stream (api: IFilmClubApi) (user: IAuth0UserProfile) (model: Model) (msgs: IAsyncObservable<MyMsg>) =
    let json = Form.toJson formConfig model.FormState
    let stateOpt = getFrmState json
    let newClubs =
        msgs
        |> AsyncRx.filter (function | SaveClub -> true | _ -> false)
        |> AsyncRx.choose (fun m -> stateOpt)
        |> AsyncRx.flatMapLatest (fun state -> makeCall api state user.sub)
        |> AsyncRx.map ClubSaved

    newClubs
        |> AsyncRx.merge msgs
        |> AsyncRx.tag json

let clickButton dispatch () =
    dispatch SaveClub

let getImageElementToDisplay (imageOption: ImageType option): ReactElement =
    match imageOption with
    | None -> str "No image selected"
    | Some im -> str im.Name

let private getIsValid (model: Model) =
    let (state, isValid) = Form.validate formConfig model.FormState
    isValid

let private view (model : Model) (dispatch : MyMsg -> unit)  =
    Container.container [ Container.IsFluid; Container.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Left) ] ] [
        Content.content [ ] [
            h1 [] [str "Create new club" ]
            form [] [
                Form.render {
                    Config = formConfig
                    State = model.FormState
                    Dispatch = dispatch
                    ActionsArea = (div [] [])
                    Loader = Form.DefaultLoader } ]
            Button.button [ Button.Disabled (not (getIsValid model)); Button.OnClick (fun _ -> dispatch SaveClub) ] [ str "Create Club" ] ] ]

let Component (api: IFilmClubApi) (dispatchRoute: Route -> unit) (user: IAuth0UserProfile) =
    let model = init
    Reaction.StreamComponent model view (update dispatchRoute) (stream api user)