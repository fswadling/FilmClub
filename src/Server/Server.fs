open System.IO
open System.Threading.Tasks

open Microsoft.AspNetCore.Builder
open Microsoft.Extensions.DependencyInjection
open FSharp.Control.Tasks.V2
open Giraffe
open Saturn
open Shared

open Fable.Remoting.Server
open Fable.Remoting.Giraffe
open Microsoft.WindowsAzure.Storage

open LiteDB
open LiteDB.FSharp
open LiteDB.FSharp.Extensions

let tryGetEnv = System.Environment.GetEnvironmentVariable >> function null | "" -> None | x -> Some x

let publicPath = tryGetEnv "public_path" |> Option.defaultValue "../Client/public" |> Path.GetFullPath
let storageAccount = tryGetEnv "STORAGE_CONNECTIONSTRING" |> Option.defaultValue "UseDevelopmentStorage=true" |> CloudStorageAccount.Parse

let mapper = LiteDB.FSharp.FSharpBsonMapper()
let db = new LiteDatabase("simple.db", mapper)

let port =
    "SERVER_PORT"
    |> tryGetEnv |> Option.map uint16 |> Option.defaultValue 8085us

let getFilmApi (database: LiteDatabase) = {
   UpdateUser = fun (sub: string) (name: string) -> async {
       let users = database.GetCollection<User>("users")
       let userOpt = users.findMany <@ fun user -> user.Sub = sub @> |> Seq.tryExactlyOne
       return match userOpt with
            | Some user ->
                let updatedUser = { user with Name = name }
                users.Update updatedUser |> ignore
                updatedUser
            | None ->
                let user: User = {
                    Id = 0
                    Name = name
                    Sub = sub
                }
                users.Insert user |> ignore
                user
   }
   GetUsers = fun (subs: string list) -> async {
       let users = database.GetCollection<User>("users")
       return users.Find (fun user -> List.contains user.Sub subs) |> Seq.toList
   }
   GetFilms = fun (clubId: int) -> async {
       let films = database.GetCollection<Film>("films")
       let clubFilms = films.findMany <@ fun film -> film.ClubId = clubId @> |> Seq.toList
       return clubFilms
   }
   AddNewFilm = fun (filmName: string) (image: ImageType) (description: string) (clubId: int) (userId: string) -> async {
       let film: Film = {
           Id = 0
           ClubId = clubId
           UserId = userId
           Name = filmName
           Image = image
           Description = description
       }
       let films = database.GetCollection<Film>("films")
       films.Insert(film) |> ignore
       return film
   }
   UpdateFilm = fun (userId: string) (film: Film) -> async {
       let films = database.GetCollection<Film>("films")
       return films.findMany <@ fun x -> x.Id = film.Id @>
            |> Seq.tryExactlyOne
            |> Option.filter (fun f -> f.UserId = userId && film.UserId = userId)
            |> Option.filter films.Update
            |> Option.map Valid
            |> Option.defaultValue Invalid
   }
   GetClubById = fun (userId: string) (clubId: int) -> async {
       let clubs = database.GetCollection<Club>("clubs")
       return clubs.findMany <@ fun club -> club.Id = clubId @>
            |> Seq.tryExactlyOne
            |> Option.filter (fun c -> List.contains userId c.MemberIds)
            |> Option.map Valid
            |> Option.defaultValue Invalid
   }
   GetClubsForUser = fun userId -> async {
       let clubs = database.GetCollection<Club>("clubs")
       return clubs.Find (fun club -> List.contains userId club.MemberIds)
            |> Seq.toList
   }
   SaveNewClub = fun (name: string) (image: ImageType) (userId: string) -> async {
       let clubs = database.GetCollection<Club>("clubs")
       let club: Club = {
           Id = 0
           Name = name
           Image = image
           OwnerId = userId
           MemberIds = [ userId ]
       }
       clubs.Insert(club) |> ignore
       return club
   }
   UpdateClub = fun (userId: string) (club: Club) -> async {
       let clubs = database.GetCollection<Club>("clubs")
       return clubs.findMany <@ fun clubx -> clubx.Id = club.Id @>
            |> Seq.tryExactlyOne
            |> Option.filter (fun c -> c.OwnerId = userId && club.OwnerId = userId)
            |> Option.filter clubs.Update
            |> Option.map Valid
            |> Option.defaultValue Invalid
   }
   RequestJoinClub = fun (userId: string) (clubId: int) -> async {
       let requests = database.GetCollection<ClubJoinRequest>("clubJoinRequests")
       let clubs = database.GetCollection<Club>("clubs")
       return clubs.findMany <@ fun clubx -> clubx.Id = clubId @>
            |> Seq.tryExactlyOne
            |> Option.filter (fun c -> not (List.contains userId c.MemberIds))
            |> Option.filter (fun c -> requests.findMany <@ fun req -> req.ClubId = clubId && req.UserId = userId @> |> Seq.isEmpty)
            |> Option.map (fun c ->
                let request: ClubJoinRequest = {
                    Id = 0
                    UserId = userId
                    ClubId = clubId
                    RequestStatus = ClubJoinRequestStatus.Pending
                }
                requests.Insert(request) |> ignore
                request)
            |> Option.map Valid
            |> Option.defaultValue Invalid
   }
   GetJoinClubRequestsForUser = fun (userId: string) -> async {
       let requests = database.GetCollection<ClubJoinRequest>("clubJoinRequests")
       return requests.findMany <@ fun req -> req.UserId = userId @> |> Seq.toList
   }
   GetJoinClubRequestsForClub = fun (clubId: int) -> async {
       let requests = database.GetCollection<ClubJoinRequest>("clubJoinRequests")
       return requests.findMany <@ fun req -> req.ClubId = clubId @> |> Seq.toList
   }
   AllowRequest = fun (requestId: int) -> async {
       let requests = database.GetCollection<ClubJoinRequest>("clubJoinRequests")
       let clubs = database.GetCollection<Club>("clubs")
       return requests.findMany <@ fun req -> req.Id = requestId @>
            |> Seq.tryExactlyOne
            |> Option.bind (fun req -> clubs.findMany <@ fun club -> club.Id = req.ClubId @> |> Seq.tryExactlyOne |> Option.map (fun c -> req, c))
            |> Option.filter (fun (req, club) -> requests.Update { req with RequestStatus = ClubJoinRequestStatus.Accepted })
            |> Option.filter (fun (req, club) -> clubs.Update { club with MemberIds = req.UserId::club.MemberIds })
            |> Option.map (fun (req, club) -> req)
            |> Option.map Valid
            |> Option.defaultValue Invalid
   }
   DenyRequest = fun (requestId: int) -> async {
       let requests = database.GetCollection<ClubJoinRequest>("clubJoinRequests")
       let clubs = database.GetCollection<Club>("clubs")
       return requests.findMany <@ fun req -> req.Id = requestId @>
            |> Seq.tryExactlyOne
            |> Option.bind (fun req -> clubs.findMany <@ fun club -> club.Id = req.ClubId @> |> Seq.tryExactlyOne |> Option.map (fun c -> c, req))
            |> Option.filter (fun (club, req) -> requests.Update { req with RequestStatus = ClubJoinRequestStatus.Denied })
            |> Option.filter (fun (club, req) -> clubs.Update { club with MemberIds = club.MemberIds |> List.filter (fun id -> id <> req.UserId) } )
            |> Option.map (fun (club, req) -> req)
            |> Option.map Valid
            |> Option.defaultValue Invalid
   }
}

let webApp =

    Remoting.createApi()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.fromContext (fun ctx -> getFilmApi db)
    |> Remoting.buildHttpHandler

let configureAzure (services:IServiceCollection) =
    tryGetEnv "APPINSIGHTS_INSTRUMENTATIONKEY"
    |> Option.map services.AddApplicationInsightsTelemetry
    |> Option.defaultValue services

let app = application {
    url ("http://0.0.0.0:" + port.ToString() + "/")
    use_router webApp
    memory_cache
    use_static publicPath
    service_config configureAzure
    use_gzip
}

run app
