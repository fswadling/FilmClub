module FilmClubHomePage

open Auth0
open Shared
open Fable.React
open Fable.Reaction
open Fable.React.Props
open Fulma
open FSharp.Control
open Elmish.Navigation

type private Model = {
   User: IAuth0UserProfile
   Clubs: Club list option
}

type private Msg =
    | ClubsLoaded of Club list

let private init user : Model =
    { User = user; Clubs = None }

let private update (currentModel : Model) (msg : Msg) : Model =
    match msg with
    | ClubsLoaded clubs -> { currentModel with Clubs = Some clubs }

let private stream (api: IFilmClubApi) (user: IAuth0UserProfile) (model: Model) (msgs: IAsyncObservable<Msg>) =
    match model.Clubs with
    | None ->
        api.GetClubs user.sub
        |> AsyncRx.ofAsync
        |> AsyncRx.delay 2000
        |> AsyncRx.map ClubsLoaded
        |> AsyncRx.tag "clubsLoading"
    | _ ->
    msgs
        |> AsyncRx.tag "msgs"

let private renderClub (club: Club) =
    div [ ClassName "card" ] [
        yield! club.Image |> function |Some im -> [ img [ Class "card-image"; Src im.Image ] ] |None -> []
        div [ ClassName "card-title" ] [
            str club.Name
        ]
    ]

let private renderClubs clubs =
    let cards = Seq.map renderClub clubs
    div [ ClassName "card-list" ] cards

let private view (dispatchRoute: Route -> unit) (model : Model) (dispatch : Msg -> unit) =
    match model.Clubs with
    | None -> Utils.LoadingPage "Loading Clubs"
    | Some clubs ->
    div [] [
        Container.container [ Container.IsFluid; Container.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ] [
            Content.content [ ] [
                Button.button [ Button.CustomClass "home-btn"; Button.OnClick (fun _ -> dispatchRoute NewClub) ] [ str "Create new club" ]
                Button.button [ Button.CustomClass "home-btn" ] [ str "Join existing club" ] ] ]
        renderClubs clubs
    ]

let Component (api: IFilmClubApi) dispatchRoute (user: IAuth0UserProfile) =
    let model = init user
    let streamFn = stream api user
    let viewFn = view dispatchRoute
    Reaction.StreamComponent model viewFn update streamFn