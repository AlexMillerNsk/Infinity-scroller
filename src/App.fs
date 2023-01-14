module App

open Elmish
open Elmish.React
open Feliz
open Fable.SimpleHttp
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



type State = { Items: Deferred<Result<Item list, string>>; Test:float }

type Msg =
  | Loaditems of AsyncOperationStatus<Result<Item list, string>>
  | LoadMore 
  | Tested of float


let endpoint = "https://jsonplaceholder.typicode.com/albums/1/photos"


let loadStoryItems value = async {
    let! (status, responseText) =Http.get endpoint
    match status with
    | 200 -> 
        let itemList = Json.parseAs<Item list>responseText|> List.truncate value
        return Loaditems (Finished (Ok itemList))
    | _ ->
        return Loaditems (Finished (Error responseText))}

let init() = 
    let initialstate = { Items = HasNotStartedYet; Test = 0.0 }
    let initialCmd = Cmd.ofMsg (Loaditems Started) 
    initialstate, initialCmd

let toInt (x: float) =
  let int = x|>int
  match int with
  | 0 -> 50 
  | i when i > 4000 -> int/10
  | _ -> 50

let update msg state =
  match msg with
    | Loaditems Started ->
        let nextState = { state with Items = InProgress}
        nextState, Cmd.fromAsync (loadStoryItems 35)
    | Loaditems (Finished (Ok itemlist)) -> 
        let nextState = { state with Items = Resolved (Ok itemlist)}
        nextState, Cmd.none
    | Loaditems (Finished (Error error)) -> 
        let nextState = { state with Items = Resolved (Error error)}
        nextState, Cmd.none
    | LoadMore  ->  state, Cmd.fromAsync (loadStoryItems 15)
    | Tested x -> {state with Test = x}, Cmd.fromAsync (loadStoryItems (x|>toInt))
    
//Cmd.fromAsync (loadStoryItems (x|>toInt))
                  

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


let renderButton (state: State) (dispatch: Msg -> unit) =
  Html.div [
     prop.className "control"
     prop.children [
       Html.button [
         prop.classes [ "button"; "is-primary"; "is-medium" ]
         prop.onClick (fun _ -> dispatch (Loaditems Started))
         prop.children [
           Html.i [ prop.classes [ "fa"; "fa-plus" ] ]
            ]
          ]
        ]
    ]

let render (state: State) (dispatch: Msg -> unit) = 
  Html.div [
    prop.style [ style.overflow.auto; style.height 1600 ]
    prop.onScroll ( fun se -> dispatch (Tested (se.target :?> Browser.Types.HTMLElement).scrollHeight))
    prop.children [
      renderButton state dispatch
      renderItems state.Items 
      Html.h1 state.Test
    ]
  ]  

Program.mkProgram init update render
|> Program.withReactSynchronous "elmish-app"
|> Program.run
