module Utils
open Fulma
open Fable.React

let LoadingPage text =
    Hero.hero [ Hero.IsFullheightWithNavbar; Hero.Color IsBlack ] [
        Hero.body [] [
            Container.container [ Container.IsFluid; Container.Modifiers [Modifier.TextAlignment (Screen.All, TextAlignment.Centered)]] [
                div [ ] [
                    str text
                    Icon.icon [Icon.Size IsLarge ] [ Fable.FontAwesome.Fa.i [ Fable.FontAwesome.Free.Fa.Solid.Spinner; Fable.FontAwesome.Fa.Spin ] []]
                        ] ] ] ]