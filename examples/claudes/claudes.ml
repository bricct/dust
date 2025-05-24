open Core
open Notty


type custom_event = [ `Timer ]
(* First, define a generic component interface *)
module type Component = sig
  type state
  type event
  
  val init : unit -> state
  val update : state -> event -> state * bool
  val render : state -> image
  val map_event : state -> [> Dust.event | custom_event ] -> event option
end

(* Use an existential type to hide the specific state and event types *)
type any_component = 
  | AnyComponent : {
      id : string;
      state : 's;
      module_ops : (module Component with type state = 's and type event = 'e);
    } -> any_component

(* Functions to work with components *)
module ComponentList = struct
  type t = any_component list
  
  let empty = []
  
  (* Add a component to the list *)
  let add (type s e) id (module C : Component with type state = s and type event = e) components =
    let component_state = C.init () in
    AnyComponent {
      id;
      state = component_state;
      module_ops = (module C);
    } :: components
  
  (* Find a component by ID *)
  let find_by_id id components =
    List.find ~f:(function AnyComponent c -> String.equal c.id id) components
    
  (* Remove a component by ID *)
  let remove_by_id id components =
    List.filter ~f:(function AnyComponent c -> not (String.equal c.id id)) components
  
  (* Update a single component and get the dirty flag *)
  let update_component (AnyComponent {id; state; module_ops = (module C)} as comp) evt =
    match C.map_event state evt with
    | Some component_evt ->
        let new_state, is_dirty = C.update state component_evt in
        AnyComponent {id; state = new_state; module_ops = (module C)}, is_dirty
    | None -> 
        comp, false
  
  (* Update all components with an event - purely functional version *)
  let update_all evt components =
    let rec process acc dirty = function
      | [] -> List.rev acc, dirty
      | comp :: rest ->
          let updated_comp, is_dirty = update_component comp evt in
          process (updated_comp :: acc) (dirty || is_dirty) rest
    in
    process [] false components
  
  (* Render a specific component *)
  let render id components =
    match find_by_id id components with
    | Some (AnyComponent {state; module_ops = (module C); _}) -> 
        C.render state
    | None -> 
        I.empty
  
  (* Get all component IDs *)
  let get_ids components =
    List.map ~f:(function AnyComponent c -> c.id) components
  
  (* Get next ID in the list after the current one *)
  let next_id current_id components =
    let ids = get_ids components in
    let rec find_next = function
      | [] -> List.hd ids  (* Wrap around *)
      | id :: rest when String.equal id current_id ->
          (match rest with
          | [] -> List.hd ids  (* Wrap around *)
          | next :: _ -> Some next)
      | _ :: rest -> find_next rest
    in
    find_next ids
end

(* Application with dynamic components *)
module DynamicApp = struct
  type state = {
    components: ComponentList.t;
    active_component: string option;
  }
  
  (* Use GADT for type-safe component addition *)
  type add_component = 
    | AddComponent : string * (module Component with type state = 's and type event = 'e) -> add_component
  
  type app_event = [
    | `Add of add_component
    | `RemoveComponent of string
    | `SelectComponent of string
    | custom_event
    | Dust.event
  ]
  
  let init () = {
    components = ComponentList.empty;
    active_component = None;
  }
  
  (* Initialize framework state *)
  let init_framework dust_state =
    dust_state |> Dust.State.add_timer "global_timer" ~ms:20 ~event:`Timer
  
  (* App update function *)
  let update state (evt : app_event) =
    match evt with
    | `Add (AddComponent (id, component)) ->
        let new_components = ComponentList.add id component state.components in
        let new_active = 
          match state.active_component with
          | None -> Some id
          | some -> some
        in
        { components = new_components; active_component = new_active }, true
    
    | `RemoveComponent id ->
        let new_components = ComponentList.remove_by_id id state.components in
        let new_active = 
          if (match state.active_component with Some i when String.equal i id -> true | _ -> false) then
            ComponentList.get_ids new_components |> List.hd
          else 
            state.active_component
        in
        { components = new_components; active_component = new_active }, true
    
    | `SelectComponent id ->
        { state with active_component = Some id }, true
    
    | `Key (`Tab, _) ->
        let next_id = match state.active_component with
          | None -> None
          | Some id -> ComponentList.next_id id state.components
        in
        { state with active_component = next_id }, true
    
    | evt ->
        (* Update all components *)
        let new_components, dirty = ComponentList.update_all evt state.components in
        { state with components = new_components }, dirty
        
  (* Render function *)
  let render dim state =
    (* Render component tabs *)
    let tab_images = 
      ComponentList.get_ids state.components
      |> List.map ~f:(fun id ->
          let style = 
            if (match state.active_component with | Some i when String.equal i id -> true | _ -> false) then
              A.(bg white ++ fg black)
            else
              A.(bg blue ++ fg white)
          in
          I.string style (" " ^ id ^ " ")
        )
    in
    let tab_bar = match tab_images with
      | [] -> I.empty
      | _ -> I.hcat tab_images
    in
    
    (* Render active component *)
    let content = match state.active_component with
      | None -> I.string A.empty "No components"
      | Some id -> ComponentList.render id state.components
    in
    
    I.(tab_bar <-> content)
end

(* Example components *)

(* Timer component *)
module Timer : Component = struct
  type timer_state = Stopped | Started of Time_ns.t
  
  type state = { 
    remaining_ms : int;
    timer_state : timer_state;
  }
  
  type event = Start | Stop | Reset | Tick
  
  let init () = { 
    remaining_ms = 10000;
    timer_state = Stopped;
  }
  
  let update state = function
    | Start -> 
        { state with timer_state = Started (Time_ns.now ()) }, true
    | Stop -> 
        { state with timer_state = Stopped }, true
    | Reset -> 
        { remaining_ms = 10000; timer_state = Stopped }, true
    | Tick ->
        match state.timer_state with
        | Stopped -> state, false
        | Started last_ts ->
            let now = Time_ns.now () in
            let elapsed_span = Time_ns.diff now last_ts in
            let elapsed_ms = Int.of_float @@ Time_ns.Span.to_ms elapsed_span in
            if elapsed_ms > state.remaining_ms then
              { timer_state = Stopped; remaining_ms = 0 }, true
            else
              let remaining_ms = state.remaining_ms - elapsed_ms in
              { timer_state = Started now; remaining_ms }, true
  
  let render state =
    let fmt_time = 
      Printf.sprintf "%d" (state.remaining_ms / 1000) 
      ^ "." 
      ^ Printf.sprintf "%.3d" (state.remaining_ms mod 1000) 
    in
    I.string A.(bg white ++ fg (gray 8)) fmt_time
    
  let map_event state = function
    | `Key (`ASCII ' ', _) -> 
        (Some (match state.timer_state with
              | Stopped -> Start
              | Started _ -> Stop))
    | `Key (`Backspace, _) -> Some Reset
    | `Timer -> Some Tick
    | _ -> None
end

(* Counter component *)
module Counter : Component = struct
  type state = { count: int }
  type event = Increment | Decrement | Reset
  
  let init () = { count = 0 }
  
  let update state = function
    | Increment -> { count = state.count + 1 }, true
    | Decrement -> { count = state.count - 1 }, true
    | Reset -> { count = 0 }, true
  
  let render state =
    I.string A.(bg blue ++ fg white) (string_of_int state.count)
    
  let map_event _ = function
    | `Key (`ASCII '+', _) -> Some Increment
    | `Key (`ASCII '-', _) -> Some Decrement
    | `Key (`ASCII 'r', _) -> Some Reset
    | _ -> None
end

(* Text editor component *)
module TextEditor : Component = struct
  type state = {
    text: string;
    cursor: int;
  }
  
  type event = 
    | AddChar of char
    | DeleteChar
    | MoveCursor of int
    | Clear
  
  let init () = {
    text = "";
    cursor = 0;
  }
  
  let update state = function
    | AddChar c ->
        let prefix = String.sub state.text ~pos:0 ~len:state.cursor in
        let suffix = String.sub state.text ~pos:state.cursor ~len:(String.length state.text - state.cursor) in
        let new_text = prefix ^ String.make 1 c ^ suffix in
        { text = new_text; cursor = state.cursor + 1 }, true
    | DeleteChar ->
        if state.cursor > 0 then
          let prefix = String.sub state.text ~pos:0 ~len:(state.cursor - 1) in
          let suffix = String.sub state.text ~pos:state.cursor ~len:(String.length state.text - state.cursor) in
          { text = prefix ^ suffix; cursor = state.cursor - 1 }, true
        else
          state, false
    | MoveCursor delta ->
        let new_cursor = max 0 (min (String.length state.text) (state.cursor + delta)) in
        { state with cursor = new_cursor }, true
    | Clear ->
        { text = ""; cursor = 0 }, true
  
  let render state =
    let pre_cursor = String.sub state.text ~pos:0 ~len:state.cursor in
    let post_cursor = 
      if state.cursor < String.length state.text then
        String.sub state.text ~pos:state.cursor ~len:(String.length state.text - state.cursor)
      else ""
    in
    let cursor_char = 
      if state.cursor < String.length state.text then
        String.make 1 state.text.[state.cursor]
      else " "
    in
    I.(
      string A.(fg white) pre_cursor <|>
      string A.(bg white ++ fg black) cursor_char <|>
      string A.(fg white) post_cursor
    )
    
  let map_event _ = function
    | `Key (`ASCII c, []) when Char.compare c ' ' >= 0 && Char.compare c '~' <= 0 -> Some (AddChar c)
    | `Key (`Backspace, []) -> Some DeleteChar
    | `Key (`Arrow `Left, []) -> Some (MoveCursor (-1))
    | `Key (`Arrow `Right, []) -> Some (MoveCursor 1)
    | `Key (`ASCII 'x', [`Ctrl]) -> Some Clear
    | _ -> None
end

(* Main application *)
let run_multi_app () =
  (* Initial application state *)
  let init_state = DynamicApp.init () in
  
  (* Add components *)
  let app_with_timer = DynamicApp.update init_state 
    (`Add (DynamicApp.AddComponent ("timer", (module Timer))))
    |> fst
  in
  
  let app_with_components = DynamicApp.update app_with_timer
    (`Add (DynamicApp.AddComponent ("counter", (module Counter))))
    |> fst
  in
  
  let final_app = DynamicApp.update app_with_components
    (`Add (DynamicApp.AddComponent ("editor", (module TextEditor))))
    |> fst
  in

  let update dust_state evt = 
    let state = Dust.State.extract dust_state in
    let state, dirty = DynamicApp.update state evt in
    Dust.State.set dust_state state, dirty
  in
  
  (* Run the application *)
  Dust.run
    ~model:final_app
    ~init:DynamicApp.init_framework
    ~render:DynamicApp.render
    ~update
    ()

(* Run the multi-app *)
let () = run_multi_app ()

