module Entity = struct
  type t = int
end

(* Components *)
type position = { mutable x : float; mutable y : float; w : float; h : float }
type velocity = { mutable vx : float; mutable vy : float }
type animation_kind = Idle | Move | Attack
type frame_info = { src_x : float; src_y : float; duration : float }

type input = {
  mutable left : bool;
  mutable right : bool;
  mutable up : bool;
  mutable down : bool;
}

type animation = {
  mutable current_kind : animation_kind;
  mutable current_frame : int;
  mutable timer : float;
  idle_frames : frame_info array;
  idle_texture : Raylib.Texture.t;
  moving_frames : frame_info array;
  moving_texture : Raylib.Texture.t;
}

type world = {
  last_id : int;
  positions : (int, position) Hashtbl.t;
  velocities : (int, velocity) Hashtbl.t;
  animations : (int, animation) Hashtbl.t;
  inputs : (int, input) Hashtbl.t;
}

let create_world () =
  {
    last_id = 0;
    positions = Hashtbl.create 100;
    velocities = Hashtbl.create 100;
    animations = Hashtbl.create 100;
    inputs = Hashtbl.create 100;
  }

(** [create_entity world] creates a new [entity] the the [world]'s [last_id]. It
    then returns [new_world * entity] *)
let create_entity (world : world) =
  let entity = world.last_id in
  let new_world = { world with last_id = entity + 1 } in
  (new_world, entity)

(** [add_position entity pos world] adds [pos] to [world.positions] and returns
    [world] *)
let add_position (entity : Entity.t) (pos : position) (world : world) =
  Hashtbl.add world.positions entity pos;
  world

(** [add_velocity entity vel world] adds [vel] to [world.velocities] and returns
    [world] *)
let add_velocity (entity : Entity.t) (vel : velocity) (world : world) =
  Hashtbl.add world.velocities entity vel;
  world

(** [add_animation entity anim world] adds [anim] to [world.animations] and
    returns [world] *)
let add_animation (entity : Entity.t) (anim : animation) (world : world) =
  Hashtbl.add world.animations entity anim;
  world

let add_input (entity : Entity.t) (input : input) (world : world) =
  Hashtbl.add world.inputs entity input;
  world

let create_player (w : world) =
  let open Raylib in
  let w, player = create_entity w in
  let pos = { x = 200.0; y = 200.0; w = 100.0; h = 100.0 } in
  let vel = { vx = 0.0; vy = 0.0 } in

  let player_idle_texture =
    load_texture
      "assets/Asset Packs 1-5/Asset Pack-V1/Player Idle/Player Idle 48x48.png"
  in
  let player_running_texture =
    load_texture
      "assets/Asset Packs 1-5/Asset Pack-V1/Player Run/player run 48x48.png"
  in

  let frames_idle =
    [|
      { src_x = 0.0; src_y = 0.0; duration = 0.2 };
      { src_x = 48.0; src_y = 0.0; duration = 0.2 };
      { src_x = 96.0; src_y = 0.0; duration = 0.2 };
      { src_x = 144.0; src_y = 0.0; duration = 0.2 };
      { src_x = 192.0; src_y = 0.0; duration = 0.2 };
      { src_x = 240.0; src_y = 0.0; duration = 0.2 };
      { src_x = 288.0; src_y = 0.0; duration = 0.2 };
      { src_x = 336.0; src_y = 0.0; duration = 0.2 };
      { src_x = 384.0; src_y = 0.0; duration = 0.2 };
      { src_x = 432.0; src_y = 0.0; duration = 0.2 };
    |]
  in
  let frames_running =
    [|
      { src_x = 0.0; src_y = 0.0; duration = 0.2 };
      { src_x = 48.0; src_y = 0.0; duration = 0.2 };
      { src_x = 96.0; src_y = 0.0; duration = 0.2 };
      { src_x = 144.0; src_y = 0.0; duration = 0.2 };
      { src_x = 192.0; src_y = 0.0; duration = 0.2 };
      { src_x = 240.0; src_y = 0.0; duration = 0.2 };
      { src_x = 288.0; src_y = 0.0; duration = 0.2 };
      { src_x = 336.0; src_y = 0.0; duration = 0.2 };
    |]
  in

  let anim =
    {
      current_kind = Idle;
      current_frame = 0;
      timer = 0.0;
      idle_frames = frames_idle;
      idle_texture = player_idle_texture;
      moving_frames = frames_running;
      moving_texture = player_running_texture;
    }
  in

  let input = { up = false; down = false; left = false; right = false } in

  let w =
    w
    |> add_position player pos
    |> add_velocity player vel
    |> add_animation player anim
    |> add_input player input
  in
  (w, player)

(* TODO: move to module *)
let animation_system (dt : float) (w : world) =
  Hashtbl.iter
    (fun ent anim ->
      match
        (Hashtbl.find_opt w.velocities ent, Hashtbl.find_opt w.positions ent)
      with
      | Some vel, Some _pos ->
          let speed = abs_float vel.vx +. abs_float vel.vy in
          let desired_kind =
            if speed > 0.1 then
              Move
            else
              Idle
          in

          if desired_kind <> anim.current_kind then (
            anim.current_kind <- desired_kind;
            anim.current_frame <- 0;
            anim.timer <- 0.0);

          let frames =
            match anim.current_kind with
            | Move -> anim.moving_frames
            | _ -> anim.idle_frames
          in

          anim.timer <- anim.timer +. dt;
          let current_frame_duration = frames.(anim.current_frame).duration in
          if anim.timer > current_frame_duration then (
            anim.timer <- anim.timer -. current_frame_duration;
            anim.current_frame <-
              (anim.current_frame + 1) mod Array.length frames)
      | _ -> ())
    w.animations;
  w

(** [input_system w] updates the velocity of entities with an [input] component
    based on the pressed key*)
let input_system (w : world) =
  Hashtbl.iter
    (fun ent _input ->
      match Hashtbl.find_opt w.velocities ent with
      | Some vel ->
          vel.vx <- 0.0;
          vel.vy <- 0.0;
          if Raylib.is_key_down Raylib.Key.A then
            vel.vx <- vel.vx -. 100.0;
          if Raylib.is_key_down Raylib.Key.D then
            vel.vx <- vel.vx +. 100.0;
          if Raylib.is_key_down Raylib.Key.W then
            vel.vy <- vel.vy -. 100.0;
          if Raylib.is_key_down Raylib.Key.S then
            vel.vy <- vel.vy +. 100.0
      | _ -> ())
    w.inputs;
  w

let movement_system (dt : float) (w : world) =
  Hashtbl.iter
    (fun ent vel ->
      match Hashtbl.find_opt w.positions ent with
      | Some pos ->
          pos.x <- pos.x +. (vel.vx *. dt);
          pos.y <- pos.y +. (vel.vy *. dt)
      | None -> ())
    w.velocities;
  w

let draw_sprite (entity : Entity.t) (w : world) =
  match
    (Hashtbl.find_opt w.positions entity, Hashtbl.find_opt w.animations entity)
  with
  | Some pos, Some anim ->
      let frames, texture =
        match anim.current_kind with
        | Move -> (anim.moving_frames, anim.moving_texture)
        | _ -> (anim.idle_frames, anim.idle_texture)
      in
      let frame = frames.(anim.current_frame) in
      let src_rect =
        Raylib.Rectangle.create frame.src_x frame.src_y 48.0 48.0
      in
      let dst_rect = Raylib.Rectangle.create pos.x pos.y pos.w pos.h in
      Raylib.draw_texture_pro texture src_rect dst_rect
        (Raylib.Vector2.create 0.0 0.0)
        0.0 Raylib.Color.white
  | _ -> ()
