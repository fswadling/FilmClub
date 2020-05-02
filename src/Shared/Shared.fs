namespace Shared

type Film = {
    Name: string
}

type ImageType = {
    Name: string
    Image: string
}

[<CLIMutable>]
type Club = {
    Id: int
    Image: ImageType option
    OwnerId: string
    MemberIds: string list
    Name: string
}

module Route =
    /// Defines how routes are generated on server and mapped from client
    let builder typeName methodName =
        sprintf "/api/%s/%s" typeName methodName

/// A type that specifies the communication protocol between client and server
/// to learn more, read the docs at https://zaid-ajaj.github.io/Fable.Remoting/src/basics.html
type IFilmClubApi = {
    GetFilms: unit -> Async<Film list>
    GetClubs: string -> Async<Club list>
    SaveNewClub: string -> ImageType option -> string -> Async<Club>
    }

type Route =
    | Home
    | Club of int
    | NewClub

