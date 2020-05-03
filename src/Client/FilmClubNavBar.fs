module FilmClubNavBar

open Shared
open Fable.React
open Fable.React.Props
open Fable.Reaction
open Fulma
open Auth0
open Routes

type private Model = {
   User: IAuth0UserProfile option
   Route: Route option
}

type private Msg =
    | Login
    | LogOut

let private init user route : Model =
    { User = user; Route = route }

let private update (currentModel : Model) (msg : Msg) : Model =
    currentModel

let private stream model msgs =
    msgs
    |> AsyncRx.tag "msgs"

let getClubDropdownItems (user: IAuth0UserProfile) (club: Club) =
    [
        yield! match (user.sub = club.OwnerId) with
                | true ->
                    [
                       Navbar.Item.a [ Navbar.Item.Props [ ] ] [ str "Club admin" ]
                    ]
                | false -> []
    ]

let getRouteDropdownItems (user: IAuth0UserProfile) (route: Route) =
    match route with
    | ClubRoute clubVar ->
        clubVar
            |> Routes.toOption
            |> Option.map (getClubDropdownItems user)
            |> Option.defaultValue []
    | _ -> []

let private renderUserNavbarItems logout (userOption: IAuth0UserProfile option) (optRoute: Route option) =
    match userOption with
        | Some user -> [
            Navbar.End.div [ ] [
                //Navbar.Item.div [ ] [ Image.image [ Image.Is24x24 ] [ img [ Src user.picture ] ] ]
                Navbar.Item.div [ Navbar.Item.HasDropdown; Navbar.Item.IsHoverable ] [
                    Navbar.Link.div [ ] [
                        Icon.icon [Icon.Size IsLarge ] [ Image.image [ Image.Is24x24 ] [ img [ Src user.picture ] ] ] ]
                    Navbar.Dropdown.div [ ] [
                        Navbar.Item.a [ Navbar.Item.Props [ OnClick (fun _ -> logout ()) ] ] [ str "Log out" ]
                        yield! optRoute |> Option.map (getRouteDropdownItems user) |> Option.defaultValue [] ] ] ] ]
        | None -> []

let private renderNavBar logout model =
    Navbar.navbar [ Navbar.Color IsBlack ]
        ((Navbar.Brand.div [ ] [
            Navbar.Item.a [ Navbar.Item.Props [ Href "/home" ] ] [
                Heading.h2 [ Heading.Modifiers [ Modifier.TextColor IsWhite ] ] [
                    str "Film Club" ] ]
        ])::(renderUserNavbarItems logout model.User model.Route))

let private view (logout: unit -> unit) (model : Model) (dispatch : Msg -> unit) =
    renderNavBar logout model

let Component (api: IFilmClubApi) (logout: unit -> unit) (user: IAuth0UserProfile option) (route: Route option) =
    let model = init user route
    let reducedView = view logout
    Reaction.StreamComponent model reducedView update stream
