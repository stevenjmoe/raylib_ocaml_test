module Entity = struct
  type t = int
end

(* Components *)
type position = { mutable x : float; mutable y : float; w : float; h : float }
type velocity = { mutable vx : float; mutable vy : float }
type facing = { mutable horizontal : [ `Left | `Right ] }

type input = {
  mutable left : bool;
  mutable right : bool;
  mutable up : bool;
  mutable down : bool;
}

type animation_kind = Idle | Move | Attack

type frame_info = {
  src_x : float;
  src_y : float;
  duration : float;
  width : float;
  height : float;
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

(* ECS and helper functions *)
type world = {
  next_id : int;
  positions : (int, position) Hashtbl.t;
  velocities : (int, velocity) Hashtbl.t;
  animations : (int, animation) Hashtbl.t;
  inputs : (int, input) Hashtbl.t;
  facing_directions : (int, facing) Hashtbl.t;
}

(* Helpers *)

(** [create_entity world] creates a new [entity] the the [world]'s [last_id]. It
    then returns [new_world * entity] *)
let create_entity (world : world) =
  let entity = world.next_id in
  let new_world = { world with next_id = entity + 1 } in
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

let add_facing (entity : Entity.t) (facing : facing) (world : world) =
  Hashtbl.add world.facing_directions entity facing;
  world

let get_position (ent : Entity.t) (w : world) = Hashtbl.find_opt w.positions ent

let draw_sprite (ent : Entity.t) (w : world) =
  match
    ( Hashtbl.find_opt w.positions ent,
      Hashtbl.find_opt w.animations ent,
      Hashtbl.find_opt w.facing_directions ent )
  with
  | Some pos, Some anim, Some facing ->
      let frames, texture =
        match anim.current_kind with
        | Move -> (anim.moving_frames, anim.moving_texture)
        | _ -> (anim.idle_frames, anim.idle_texture)
      in
      let frame = frames.(anim.current_frame) in
      let src_width =
        if facing.horizontal = `Left then -.frame.width else frame.width
      in
      let src_rect =
        Raylib.Rectangle.create frame.src_x frame.src_y src_width frame.height
      in
      let dst_rect = Raylib.Rectangle.create pos.x pos.y pos.w pos.h in
      Raylib.draw_texture_pro texture src_rect dst_rect
        (Raylib.Vector2.create 0.0 0.0)
        0.0 Raylib.Color.white
  | _ -> ()
