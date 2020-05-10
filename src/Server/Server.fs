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
   GetFilms = fun () -> async { return [
        { Name = "The Thing"};
        { Name = "Your Name"}
   ]}
   GetClubById = fun (userId: string) (clubId: int) -> async {
       let clubs = database.GetCollection<Club>("clubs")
       let clubs = clubs.findMany <@ fun club -> club.Id = clubId @> |> Seq.toList
       return match clubs with
              | [] -> Invalid
              | club::tail ->
                    let clubHasId = List.contains userId club.MemberIds
                    match clubHasId with
                        | true -> Valid club
                        | false -> Invalid
   }
   GetClubsForUser = fun userId -> async {
       let clubs = database.GetCollection<Club>("clubs")
       let list = clubs.Find (fun club -> List.contains userId club.MemberIds) |> Seq.toList
       return list
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
       let clubsList = clubs.findMany <@ fun clubx -> clubx.Id = club.Id @> |> Seq.toList
       return match clubsList with
              | [] -> Invalid
              | cluborig::_ ->
                    match cluborig.OwnerId = userId && club.OwnerId = userId with
                    | false -> Invalid
                    | true -> match (clubs.Update club) with
                                | false -> Invalid
                                | true -> Valid club
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
