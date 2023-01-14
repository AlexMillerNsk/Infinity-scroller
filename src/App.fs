module App

open Elmish
open Elmish.React
open Fable.SimpleHttp
open Fable.SimpleJson
open Feliz
open Types

type State =
  { Items: Deferred<Result<Item list, string>>
    Test: float }

type Msg =
  | Loaditems of AsyncOperationStatus<Result<Item list, string>>
  | Tested of float

let endpoint = "https://jsonplaceholder.typicode.com/albums/1/photos"

let toInt (x: float) =
  let int = int x
  match int with
  | 0 -> 50 
  | i when i > 4000 -> int / 10
  | _ -> 50

let SimpleSub dispatch = async { () } |> Async.StartImmediate
let SimpleCmd = [ SimpleSub ]

let LoadStoryItemsSub value dispatch =
  async {
    let! status, responseText = Http.get endpoint
    match status with
    | 200 -> 
      responseText
      |> Json.parseAs
      |> List.truncate value
      |> Ok
      |> Finished
      |> Loaditems
      |> dispatch
    | _ ->
      responseText
      |> Error
      |> Finished
      |> Loaditems
      |> dispatch
  } |> Async.StartImmediate


let init() =
  { Items = HasNotStartedYet
    Test = 0.0 },
  [ LoadStoryItemsSub 10 ]
    
    
let update msg state =
  match msg with
    | Loaditems Started                  -> { state with Items = InProgress }, [ LoadStoryItemsSub 10 ]
    | Loaditems (Finished (Ok itemlist)) -> { state with Items = Resolved (Ok itemlist) }, Cmd.none
    | Loaditems (Finished (Error error)) -> { state with Items = Resolved (Error error) }, Cmd.none
    | Tested x                           -> { state with Test = x }, [ LoadStoryItemsSub (toInt x) ]


let div classes (children: ReactElement list) =
  Html.div [
    prop.classes classes
    prop.children children ]

let renderError (errorMsg: string) =
  Html.h1 [
    prop.style [ style.color.red ]
    prop.text errorMsg ]

let spinner =
  Html.div [
    prop.style [ style.textAlign.center; style.marginTop 20 ]
    prop.children [ Html.i [ prop.className "fa fa-cog fa-spin fa-2x" ] ] ]

let renderItems = function
  | HasNotStartedYet -> Html.none
  | InProgress -> spinner
  | Resolved (Error errorMsg) -> renderError errorMsg
  | Resolved (Ok items) -> React.fragment [ for item in items -> Html.img [prop.src item.url] ]

let renderButton state dispatch =
  Html.div [
    prop.className "control"
    prop.children [
      Html.button [
        prop.classes [ "button"; "is-primary"; "is-medium" ]
        prop.onClick (fun _ -> dispatch (Loaditems Started))
        prop.children [ Html.i [ prop.classes [ "fa"; "fa-plus" ] ] ] ] ] ]

let render state dispatch = 
  Html.div [
    prop.style [ style.overflow.auto; style.height 1600 ]
    prop.onScroll (fun se -> dispatch (Tested (se.target :?> Browser.Types.HTMLElement).scrollHeight))
    prop.children [
      renderButton state dispatch
      renderItems state.Items 
      Html.h1 state.Test ] ]  

Program.mkProgram init update render
|> Program.withReactSynchronous "elmish-app"
|> Program.run