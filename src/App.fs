module App

open Elmish
open Elmish.React
open Feliz
open Fable.SimpleHttp
open Elmish.SweetAlert
open Npgsql.FSharp
open Types
open Fable.SimpleJson

module Cmd =
    let fromAsync (operation: Async<'msg>) : Cmd<'msg> =
        let delayedCmd (dispatch: 'msg -> unit) : unit =
            let delayedDispatch = async {
                let! msg = operation
                dispatch msg
            }

            Async.StartImmediate delayedDispatch

        Cmd.ofSub delayedCmd  


type State = { Items: Deferred<Result<Item list, string>> }

type Msg =
  | Loaditems of AsyncOperationStatus<Result<Item list, string>>

let endpoint = "https://jsonplaceholder.typicode.com/albums/1/photos"


let loadStoryItems = async {
    // simulate network delay
    let! (status, responseText) =Http.get endpoint
    match status with
    | 200 -> 
        let itemList = Json.parseAs<Item list>responseText|> List.truncate 100
        return Loaditems (Finished (Ok itemList))
    | _ ->
      // non-OK response goes finishes with an error
      return Loaditems (Finished (Error responseText))
}

let init() = 
    let initialstate = { Items = HasNotStartedYet }
    let initialCmd = Cmd.ofMsg(Loaditems Started)
    initialstate, initialCmd

let update msg state =
  match msg with
    | Loaditems Started ->
        let nextState = { state with Items = InProgress}
        nextState, Cmd.fromAsync loadStoryItems
    | Loaditems (Finished (Ok itemlist)) -> 
        let nextState = { state with Items = Resolved (Ok itemlist)}
        nextState, Cmd.none
    | Loaditems (Finished (Error error)) -> 
        let nextState = { state with Items = Resolved (Error error)}
        nextState, Cmd.none

let div (classes: string list) (children: ReactElement list) =
    Html.div [
        prop.classes classes
        prop.children children
    ]

let renderError (errorMsg: string) =
  Html.h1 [
    prop.style [ style.color.red ]
    prop.text errorMsg
  ]

let spinner =
  Html.div [
    prop.style [ style.textAlign.center; style.marginTop 20 ]
    prop.children [
      Html.i [
        prop.className "fa fa-cog fa-spin fa-2x"
      ]
    ]
  ]

let renderItems = function
  | HasNotStartedYet -> Html.none
  | InProgress -> spinner
  | Resolved (Error errorMsg) -> renderError errorMsg
  | Resolved (Ok (items: Item list)) -> React.fragment [ for item in items -> Html.img [prop.src item.url] ]

let render (state: State) (dispatch: Msg -> unit) =
  Html.div [
    prop.style [ style.padding 20 ]
    prop.children [
      Html.h1 [
        prop.className "title"
        prop.text "Elmish Hackernews"
      ]

      renderItems state.Items
    ]
  ]

Program.mkProgram init update render
|> Program.withReactSynchronous "elmish-app"
|> Program.run
