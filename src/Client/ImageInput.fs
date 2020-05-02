namespace CustomFields

open Shared
open Fable.React
open Fable.React.Props
open Thoth.Elmish.FormBuilder
open Thoth.Elmish.FormBuilder.Types
open System
open Thoth.Json
open Fable.Core.JsInterop

open Fulma.Common

[<RequireQualifiedAccess>]
module ImageInput =
    type State =
        { Label : string
          Value : ImageType option
          Placeholder : string option
          Validators : Validator list
          ValidationState : ValidationState
          Name : string }

    and Validator = State -> ValidationState

    type Msg =
        | ChangeValue of ImageType option
        interface IFieldMsg

    let private init (state : FieldState) =
        state, FormCmd.none

    let private validate (state : FieldState) =
        let state : State = state :?> State
        let rec applyValidators (validators : Validator list) (state : State) =
            match validators with
                | validator::rest ->
                    match validator state with
                    | Valid -> applyValidators rest state
                    | Invalid msg ->
                        { state with ValidationState = Invalid msg }
                | [] -> state

        applyValidators state.Validators { state with ValidationState = Valid } |> box

    let private isValid (state : FieldState) =
        let state : State = state :?> State
        state.ValidationState = Valid

    let private setError (state : FieldState) (message : string) =
        let state : State = state :?> State
        { state with ValidationState = Invalid message } |> box

    let private toJson (state : FieldState) =
        let state : State = state :?> State
        let json = state.Value |> Encode.option (fun im -> Encode.object [
            "name", Encode.string im.Name
            "image", Encode.string im.Image
        ])
        state.Name, json

    let private update (msg : FieldMsg) (state : FieldState) =
        let msg = msg :?> Msg
        let state = state :?> State

        match msg with
        | ChangeValue newValue ->
            { state with Value = newValue }
            |> validate
            |> box, FormCmd.none

    let startFileUpload dispatch (ev: Browser.Types.Event) =
        let file = ev.target?files?(0)
        let name: string = file?name

        let reader = Browser.Dom.FileReader.Create()

        reader.onload <- fun evt ->
            let img = evt.target?result
            dispatch (ChangeValue (Some { Name = name; Image = img }))

        reader.readAsDataURL(file)

    let private view (state : FieldState) (dispatch : IFieldMsg -> unit) =
        let state : State = state :?> State
        let className =
            if isValid state then
                "input"
            else
                "input is-danger"

        div [ Class "field" ] [
            label [ Class "label"; HtmlFor state.Name ] [ str state.Label ]
            div [ Class "control" ] [
                Fulma.Field.div [] [
                    Fulma.File.file [ Fulma.File.HasName ] [ Fulma.File.label [ ] [
                        Fulma.File.input [ GenericOption.Props [
                            Id state.Name
                            Accept "image/png, image/jpeg"
                            OnChange (fun ev -> startFileUpload dispatch ev |> ignore  ) ] ]
                        Fulma.File.cta [ ] [
                            Fulma.File.icon [ ] [
                                Fulma.Icon.icon [ ] [
                                    Fable.FontAwesome.Fa.i [ Fable.FontAwesome.Free.Fa.Solid.Upload ] [ ] ] ]
                            Fulma.File.label [ ] [
                                str (state.Placeholder |> Option.defaultValue "") ] ]
                        Fulma.File.name [ ] [
                            state.Value |> (function | Some im -> str im.Name |None -> str "Click here" )] ] ] ] ]
            span [ Class "help is-danger" ]
              [ str state.ValidationState.Text ]
            yield! match state.Value with
                    |Some image -> [
                        Fulma.Image.image [ ] [
                            img [ Class "card-image"; Src image.Image ] ] ]
                    | None -> [] ]

    let decoder: Decoder<ImageType option>=
        Decode.object (fun fields -> {
            Name = fields.Required.At ["name"] Decode.string
            Image = fields.Required.At ["image"] Decode.string })
            |> Decode.option

    let config : FieldConfig =
        { View = view
          Update = update
          Init = init
          Validate = validate
          IsValid = isValid
          ToJson = toJson
          SetError = setError }

type BasicImageInput private (state : ImageInput.State) =

    member __.WithDefaultView () : FieldBuilder =
        { Type = "basic-image-input"
          State = state
          Name = state.Name
          Config = ImageInput.config }

    member __.WithCustomView (view) : FieldBuilder =
        { Type = "basic-image-input"
          State = state
          Name = state.Name
          Config = { ImageInput.config with View = view } }

    static member Create(name : string) =
        BasicImageInput
            { Label = ""
              Value = None
              Placeholder = None
              Validators = [ ]
              ValidationState = Valid
              Name = name }

    member __.WithLabel (label : string) =
        BasicImageInput { state with Label = label }

    member __.WithValue (value : ImageType option) =
        BasicImageInput { state with Value = value }

    member __.WithPlaceholder (placeholder : string) =
        BasicImageInput { state with Placeholder = Some placeholder }

    member __.IsRequired (?msg : String) =
        let msg = defaultArg msg "This field is required"

        let validator (state : ImageInput.State) =
            if state.Value.IsNone then
                Invalid msg
            else
                Valid

        BasicImageInput { state with Validators = state.Validators @ [ validator ] }

    member __.AddValidator (validator) =
        BasicImageInput { state with Validators = state.Validators @ [ validator ] }