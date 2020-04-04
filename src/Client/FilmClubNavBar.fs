module FilmClubNavBar

open Shared
open Fable.React
open Fable.Reaction
open Fulma

type private Model = {
   User: User option
}

type private Msg =
    | LogOut

let private init user : Model =
    { User = user }

let private update (currentModel : Model) (msg : Msg) : Model =
    currentModel

let private stream model msgs =
    msgs
    |> AsyncRx.tag "msgs"

let private renderUserNavbarItems (userOption: User option) =
    match userOption with
        | Some user -> [
            Navbar.End.div [ ] [
                Navbar.Item.div [ Navbar.Item.HasDropdown; Navbar.Item.IsHoverable ] [
                    Navbar.Link.div [ Navbar.Link.Modifiers [ Modifier.TextColor IsWhite ] ] [
                        str user.Name
                    ]
                    Navbar.Dropdown.div  [  ] [
                        Navbar.Item.a [ ] [
                            str "Log out"
                        ]
                    ]
                ]
            ]
            ]
        | None -> []

let private renderNavBar model =
    Navbar.navbar [ Navbar.Color IsBlack ]
        ((Navbar.Item.div [ ] [
            Heading.h2 [ Heading.Modifiers [ Modifier.TextColor IsWhite ] ] [
                str "Film Club"
            ]
        ])::(renderUserNavbarItems model.User))

let private view (model : Model) (dispatch : Msg -> unit) =
    renderNavBar model

let Component (api: IFilmClubApi) (user: User option) =
    let model = init user
    Reaction.StreamComponent model view update stream
