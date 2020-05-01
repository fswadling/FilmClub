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

let tryGetEnv = System.Environment.GetEnvironmentVariable >> function null | "" -> None | x -> Some x

let publicPath = tryGetEnv "public_path" |> Option.defaultValue "../Client/public" |> Path.GetFullPath
let storageAccount = tryGetEnv "STORAGE_CONNECTIONSTRING" |> Option.defaultValue "UseDevelopmentStorage=true" |> CloudStorageAccount.Parse

//let mapper = FSharpBsonMapper()
//use db = new LiteDatabase("simple.db", mapper)

let port =
    "SERVER_PORT"
    |> tryGetEnv |> Option.map uint16 |> Option.defaultValue 8085us

let getFilmApi (database: LiteDatabase): IFilmClubApi = {
   getFilms = fun () -> async { return [
        { Name = "The Thing"};
        { Name = "Your Name"}
   ]}
   getClubs = fun userId -> async { return [
       {
           Id = 0
           Name = "Bristol friends"
           OwnerId = ""
           MemberIds = []
       };
       {
           Id = 1
           Name = "London friends"
           OwnerId = ""
           MemberIds = []
       }
   ]}
   saveNewClub = fun (name: string) (userId: string) -> async {
       let clubs = database.GetCollection<Club>("clubs")
       let club: Club = {
           Id = 0
           Name = name
           OwnerId = userId
           MemberIds = [ userId ]
       }
       clubs.Insert(club) |> ignore
       return club
   }
}

let webApp =
    let db = new LiteDatabase("simple.db")
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
