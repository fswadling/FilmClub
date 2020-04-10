namespace Shared

type Film = {
    Name: string
}

type Club = {
    Name: string
}

module Route =
    /// Defines how routes are generated on server and mapped from client
    let builder typeName methodName =
        sprintf "/api/%s/%s" typeName methodName

/// A type that specifies the communication protocol between client and server
/// to learn more, read the docs at https://zaid-ajaj.github.io/Fable.Remoting/src/basics.html
type IFilmClubApi = {
    getFilms: unit -> Async<Film list>
    getClubs: string -> Async<Club list>
    registerUser: string -> Async<bool>
    }

type Route =
    | Home
    | Club of int
    | NewClub

