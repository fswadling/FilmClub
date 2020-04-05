module FilmClubHomePage

open Shared
open Fable.React
open Fable.Reaction
open Fable.React.Props
open Fulma
open FSharp.Control

type private Model = {
   User: User
   Clubs: Club list option
}

type private Msg =
    | ClubsLoaded of Club list

let private init user : Model =
    { User = user; Clubs = None }

let private update (currentModel : Model) (msg : Msg) : Model =
    match msg with
    | ClubsLoaded clubs -> { currentModel with Clubs = Some clubs }

let private stream (api: IFilmClubApi) user model (msgs: IAsyncObservable<Msg>) =
    match model.Clubs with
    | None ->
        api.getClubs user
        |> AsyncRx.ofAsync
        |> AsyncRx.delay 2000
        |> AsyncRx.map ClubsLoaded
        |> AsyncRx.tag "clubsLoading"
    | _ -> msgs
        |> AsyncRx.tag "msgs"

let private renderClub (club: Club) =
    div [ ClassName "card" ] [
        div [ ClassName "card-title" ] [
            str club.Name
        ]
    ]

let private renderClubs clubs =
    let cards = Seq.map renderClub clubs
    div [ ClassName "card-list" ] cards

let private view (model : Model) (dispatch : Msg -> unit) =
    match model.Clubs with
    | None -> Utils.LoadingPage "Loading Clubs"
    | Some clubs -> renderClubs clubs

let Component (api: IFilmClubApi) (user: User) =
    let model = init user
    let streamFn = stream api user
    Reaction.StreamComponent model view update streamFn