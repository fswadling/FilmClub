module Utils
open Fulma
open Fable.React
open Shared

let mapAsync f op = async {
        let! x    = op
        let value = f x
        return value
    }

let LoadingPage text =
    Hero.hero [ Hero.IsFullheightWithNavbar; Hero.Color IsBlack ] [
        Hero.body [] [
            Container.container [ Container.IsFluid; Container.Modifiers [Modifier.TextAlignment (Screen.All, TextAlignment.Centered)]] [
                div [ ] [
                    str text
                    Icon.icon [Icon.Size IsLarge ] [ Fable.FontAwesome.Fa.i [ Fable.FontAwesome.Free.Fa.Solid.Spinner; Fable.FontAwesome.Fa.Spin ] []]
                        ] ] ] ]

let MessagePage text =
    Hero.hero [ Hero.IsFullheightWithNavbar; Hero.Color IsBlack ] [
        Hero.body [] [
            Container.container [ Container.IsFluid; Container.Modifiers [Modifier.TextAlignment (Screen.All, TextAlignment.Centered)]] [
                div [ ] [
                    str text ] ] ] ]

let getRequestStatusText (requestStatus: ClubJoinRequestStatus) =
    match requestStatus with
    | Pending -> Text.div [ Modifiers [ Modifier.TextColor IsGrey ] ] [ str "Pending" ]
    | Accepted -> Text.div [ Modifiers [ Modifier.TextColor IsSuccess ] ] [ str "Accepted" ]
    | Denied -> Text.div [ Modifiers [ Modifier.TextColor IsDanger ] ] [ str "Denied" ]