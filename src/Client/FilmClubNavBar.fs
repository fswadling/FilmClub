module FilmClubNavBar

open Shared
open Fable.React
open Fable.React.Props
open Fable.Reaction
open Fulma
open Auth0

type private Model = {
   User: IAuth0UserProfile option
}

type private Msg =
    | Login
    | LogOut

let private init user : Model =
    { User = user }

let private update (currentModel : Model) (msg : Msg) : Model =
    currentModel

let private stream model msgs =
    msgs
    |> AsyncRx.tag "msgs"


let private renderUserNavbarItems logout (userOption: IAuth0UserProfile option) =
    match userOption with
        | Some user -> [
            Navbar.End.div [ ] [
                Navbar.Item.div [ ] [ Image.image [ Image.Is24x24 ] [ img [ Src user.picture ]  ] ]
                Navbar.Item.div [ Navbar.Item.Modifiers [ Modifier.TextColor IsWhite ] ] [ str user.name ]
                Navbar.Item.div [] [ Button.button [ Button.OnClick (fun _ -> logout ())] [ str "Log out"] ] ]
            ]
        | None -> []

let private renderNavBar logout model =
    Navbar.navbar [ Navbar.Color IsBlack ]
        ((Navbar.Item.div [ ] [
            Heading.h2 [ Heading.Modifiers [ Modifier.TextColor IsWhite ] ] [
                str "Film Club"
            ]
        ])::(renderUserNavbarItems logout model.User))

let private view (logout: unit -> unit) (model : Model) (dispatch : Msg -> unit) =
    renderNavBar logout model

let Component (api: IFilmClubApi) (logout: unit -> unit) (user: IAuth0UserProfile option) =
    let model = init user
    let reducedView = view logout
    Reaction.StreamComponent model reducedView update stream
