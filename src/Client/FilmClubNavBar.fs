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

let getClubDropdownItems (dispatchRoute: Route -> unit) (user: IAuth0UserProfile) (club: Club) =
    [
        yield! match (user.sub = club.OwnerId) with
                | true ->
                    [
                       Navbar.Item.a [ Navbar.Item.Props [ OnClick (fun e -> dispatchRoute ((ClubRoute << (createClubRouteType ClubSubRoute.ClubAdmin) << ActualObject) club) |> ignore) ] ] [ str "Club admin" ]
                    ]
                | false -> []
    ]

let getRouteDropdownItems dispatchRoute (user: IAuth0UserProfile) (route: Route) =
    match route with
    | ClubRoute clubVar ->
        clubVar.EntityOrId
            |> Routes.toOption
            |> Option.map (getClubDropdownItems dispatchRoute user)
            |> Option.defaultValue []
    | _ -> []

let private renderUserNavbarItems logout dispatchRoute (userOption: IAuth0UserProfile option) (optRoute: Route option) =
    match userOption with
        | Some user -> [
            Navbar.End.div [ ] [
                //Navbar.Item.div [ ] [ Image.image [ Image.Is24x24 ] [ img [ Src user.picture ] ] ]
                Navbar.Item.div [ Navbar.Item.HasDropdown; Navbar.Item.IsHoverable ] [
                    Navbar.Link.div [ ] [
                        Icon.icon [Icon.Size IsLarge ] [ Image.image [ Image.Is24x24 ] [ img [ Src user.picture ] ] ] ]
                    Navbar.Dropdown.div [ ] [
                        Navbar.Item.a [ Navbar.Item.Props [ OnClick (fun _ -> logout ()) ] ] [ str "Log out" ]
                        yield! optRoute |> Option.map (getRouteDropdownItems dispatchRoute user) |> Option.defaultValue [] ] ] ] ]
        | None -> []

let private renderNavBar logout dispatchRoute model =
    Navbar.navbar [ Navbar.Color IsBlack ]
        ((Navbar.Brand.div [ ] [
            Navbar.Item.a [ Navbar.Item.Props [ Href "/home" ] ] [
                Heading.h2 [ Heading.Modifiers [ Modifier.TextColor IsWhite ] ] [
                    str "Film Club" ] ]
        ])::(renderUserNavbarItems logout dispatchRoute model.User model.Route))

let private view (logout: unit -> unit) (dispatchRoute: Route -> unit) (model : Model) (dispatch : Msg -> unit) =
    renderNavBar logout dispatchRoute model

let Component (api: IFilmClubApi) (logout: unit -> unit) (dispatchRoute: Route -> unit) (user: IAuth0UserProfile option) (route: Route option) =
    let model = init user route
    let reducedView = view logout dispatchRoute
    Reaction.StreamComponent model reducedView update stream
