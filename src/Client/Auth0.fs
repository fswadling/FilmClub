module Auth0

open Fable.Core
open Browser;
open System

open Fable.Core.JsInterop

let isAccessToken() =
    let hash = window.location.hash
    match hash.StartsWith "#access_token" with
    | true  -> Some hash
    | false -> None

type IAuth0Error =
  abstract member error: obj with get, set
  abstract member errorDescription: string with get, set

type IAuth0UserProfile =
  abstract member email: string with get, set
  abstract member name: string with get, set
  abstract member picture: string with get, set
  abstract member sub: string with get, set

type IAuthResult =
  abstract accessToken: string with get, set

type AuthConfigAuthSection = {
    redirect: bool
    redirectUrl: string
    responseType: string
}

type AuthConfig = {
    allowSignUp: bool
    allowedConnections:string array
    autoclose: bool
    auth: AuthConfigAuthSection
}

type GetSessionParams = {
    scope: string
}

type CheckSessionResult = Result of IAuthResult | Error of IAuth0Error

type IAuth0Lock =
  [<Emit"new $0($1...)">]
  abstract Create: clientId: string * domain: string -> IAuth0Lock

  [<Emit"new $0($1...)">]
  abstract CreateWithConfig:clientId: string * domain: string * config: AuthConfig -> IAuth0Lock

  abstract show: unit -> unit

  [<Emit("$0.on('authenticated',$1...)")>]
  abstract on_authenticated: callback: Func<IAuthResult, unit> -> unit

  abstract getUserInfo: token: string * callback: Func<IAuth0Error, IAuth0UserProfile, unit> -> unit

  abstract checkSession: options: GetSessionParams * callback: Func<IAuth0Error, IAuthResult, unit> -> unit

  abstract logout: unit -> unit


let Auth0Lock:IAuth0Lock = importDefault "auth0-lock"

let private onCheckSessionComplete (push: CheckSessionResult -> unit) (authError: IAuth0Error) (auth: IAuthResult) =
    let isAuthorised = not (Fable.Core.JsInterop.isNullOrUndefined auth)
    let result = match isAuthorised with
                 | true -> Result auth
                 | false -> Error authError
    push result
    ()

let checkSessionHelper (authLock: IAuth0Lock) (config: GetSessionParams) (push: CheckSessionResult -> unit) =
    let func = onCheckSessionComplete push
    authLock.checkSession (config, System.Func<IAuth0Error, IAuthResult, unit>(func))
