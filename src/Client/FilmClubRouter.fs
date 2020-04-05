module FilmClubRouter
open Elmish.UrlParser
open Elmish

type Route =
    | Home
    | Club of int

let router: Parser<Route -> Route, _> =
    oneOf
        [ map Home (s "home")
          map Club (s "club" </> i32) ]

let urlParser location = parseHash router location



