namespace Shared

type ImageType = {
    Name: string
    Image: string
}

[<CLIMutable>]
type Film = {
    Id: int
    ClubId: int
    UserId: string
    Image: ImageType
    Name: string
}

[<CLIMutable>]
type Club = {
    Id: int
    Image: ImageType
    OwnerId: string
    MemberIds: string list
    Name: string
}

type ClubJoinRequestStatus =
    | Pending
    | Accepted
    | Denied

[<CLIMutable>]
type ClubJoinRequest = {
    Id: int
    UserId: string
    UserName: string
    ClubId: int
    RequestStatus: ClubJoinRequestStatus
}

module Route =
    /// Defines how routes are generated on server and mapped from client
    let builder typeName methodName =
        sprintf "/api/%s/%s" typeName methodName

type Response<'a> =
    | Valid of 'a
    | Invalid

/// A type that specifies the communication protocol between client and server
/// to learn more, read the docs at https://zaid-ajaj.github.io/Fable.Remoting/src/basics.html
type IFilmClubApi = {
    GetFilms: int -> Async<Film list>
    AddNewFilm: string -> ImageType -> int -> string -> Async<Film>
    UpdateFilm: string -> Film -> Async<Response<Film>>
    GetClubsForUser: string -> Async<Club list>
    GetClubById: string -> int -> Async<Response<Club>>
    SaveNewClub: string -> ImageType -> string -> Async<Club>
    UpdateClub: string -> Club -> Async<Response<Club>>
    RequestJoinClub: string -> string -> int -> Async<Response<ClubJoinRequest>>
    GetJoinClubRequestsForUser: string -> Async<ClubJoinRequest list>
    GetJoinClubRequestsForClub: int -> Async<ClubJoinRequest list>
    AllowRequest: int -> Async<Response<ClubJoinRequest>>
    DenyRequest: int -> Async<Response<ClubJoinRequest>>
    }



