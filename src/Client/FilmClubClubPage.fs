module FilmClubClubPage

open Fable.React
open Fable.Reaction
open Shared
open Auth0
open FSharp.Control
open Fulma
open Routes

type private Msg =
    LoadFilms of Film list

type private ClubModel = {
    Club: Club
    Films: Film list option
}

type private Model = {
    ClubModel: ClubModel option
}

let private init optClub : Model =
    { ClubModel = optClub }

let private clickAddFilm (dispatchRoute: Route -> unit) (model: Model) =
    model.ClubModel
        |> Option.map (fun cm -> cm.Club)
        |> Option.map (fun club -> (dispatchRoute ((ClubRoute << (createClubRouteType ClubSubRoute.ClubAddNewFilm) << ActualObject) club)))
        |> ignore

let private view (dispatchRoute: Route -> unit) (model : Model) (dispatch : Msg -> unit) =
    match model.ClubModel with
    | None -> Utils.LoadingPage "Loading club..."
    | Some x -> Content.content [ ] [
            h1 [] [ str x.Club.Name ]
            Container.container [ Container.IsFluid; Container.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ] [
                Content.content [ ] [
                    Button.button [ Button.CustomClass "home-btn"; Button.OnClick (fun _ -> clickAddFilm dispatchRoute model) ] [ str "Add new film" ] ] ] ]

let private updateClubModel (currentModel : ClubModel) (msg : Msg) : ClubModel =
    match msg with
    | LoadFilms films ->
        { currentModel with Films = Some films }

let private update (currentModel : Model) (msg : Msg) : Model =
    match currentModel.ClubModel with
    | None -> currentModel
    | Some cm -> { currentModel with ClubModel = Some (updateClubModel cm msg)}

let private stream (api: IFilmClubApi) (model: Model) msgs =
    model.ClubModel
        |> Option.filter (fun cm -> Option.isNone cm.Films)
        |> Option.map (fun cm -> api.GetFilms cm.Club.Id)
        |> Option.map AsyncRx.ofAsync
        |> Option.map (AsyncRx.map LoadFilms)
        |> Option.map (AsyncRx.tag "loadFilms")
        |> Option.defaultValue (msgs |> AsyncRx.tag "msgs")

let Component (optClub: Club option) (api: IFilmClubApi) (dispatchRoute: Route -> unit) (user: IAuth0UserProfile) =
    let model = init (optClub |> Option.map (fun c -> { Club = c; Films = None}))
    Reaction.StreamComponent model (view dispatchRoute) update (stream api)