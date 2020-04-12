module FilmClubLandingPage

open Shared
open Fable.React
open Fable.Reaction
open Fulma

let render =
    Hero.hero [ Hero.Color IsWhite; Hero.IsFullHeight ] [
        Hero.body [] [
            Container.container [ Container.IsFluid; Container.Modifiers [
                Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ] [
                    Content.content [ ] [
                        h1 [] [ str "Welcome to Fred's Film Club!"] ] ] ] ]

