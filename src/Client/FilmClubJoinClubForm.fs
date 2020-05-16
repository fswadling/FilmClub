module FilmClubJoinClubForm

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
        Id: int
    }

    static member Decoder : Decoder<FormState> =
        Decode.object
            (fun get -> {
                Id = get.Required.Field "id" Decode.int
                })

type MyMsg =
    | OnFormMsg of FormBuilder.Types.Msg
    | JoinClub

type private Model = {
   FormState : FormBuilder.Types.State
   FormConfig: FormBuilder.Types.Config<MyMsg>
}

let initForm () =
    Form<MyMsg>
        .Create(OnFormMsg)
        .AddField(
            BasicInput
                .Create("id")
                .WithLabel("Id of club you wish to join:")
                .WithType("number")
                .IsRequired()
                .WithDefaultView()
        )
        .Build()

let private init () =
    let (formState, formConfig) = initForm ()
    let (formState, formCmds) = Form.init formConfig formState
    { FormState = formState; FormConfig = formConfig }

let private getFrmState (json:string): FormState option =
    let body = Decode.fromString FormState.Decoder json
    match body with
    | Ok frmState -> Some frmState
    | _ -> None

let private update (joinClub: int -> unit) (model : Model) (msg : MyMsg) : Model =
    match msg with
    | OnFormMsg msg ->
        let (formState, formCmd) = Form.update model.FormConfig msg model.FormState
        { model with FormState = formState }
    | JoinClub ->
        let json = Form.toJson model.FormConfig model.FormState
        let formState = getFrmState json
        formState
            |> Option.map (fun state -> joinClub state.Id)
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

let private view (model : Model) (dispatch : MyMsg -> unit)  =
    Container.container [ Container.IsFluid; Container.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Left) ] ] [
        Content.content [ ] [
            h1 [] [ str "Join club" ]
            form [] [
                Form.render {
                    Config = model.FormConfig
                    State = model.FormState
                    Dispatch = dispatch
                    ActionsArea = (div [] [])
                    Loader = Form.DefaultLoader } ]
            Button.button [ Button.Disabled (not (getIsValid model)); Button.OnClick (fun _ -> dispatch JoinClub) ] [ str "Request to join club" ] ] ]

let Component (joinClub: int -> unit)  =
    let model = init ()
    Reaction.StreamComponent model view (update joinClub) stream