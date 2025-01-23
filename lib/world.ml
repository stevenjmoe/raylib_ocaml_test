open Engine

type t = {
  next_id : int;
  positions : Position_store.store;
  velocities : Velocity_store.store;
  animations : Animation_store.store;
  inputs : Input_store.store;
  sprite_directions : Sprite_direction_store.store;
}

(* Helpers *)

(** [create_entity world] creates a new [entity] the the [world]'s [last_id]. It
    then returns [new_world * entity] *)
let create_entity (world : t) =
  let entity = world.next_id in
  let new_world = { world with next_id = entity + 1 } in
  (new_world, entity)

let add_position (entity : Entity.t) (pos : Position.t) (w : t) =
  Position_store.set w.positions entity pos;
  w

let get_position (entity : Entity.t) (w : t) =
  Position_store.get_opt w.positions entity

let add_velocity (entity : Entity.t) (vel : Velocity.t) (w : t) =
  Velocity_store.set w.velocities entity vel;
  w

let get_velocity (entity : Entity.t) (w : t) =
  Velocity_store.get_opt w.velocities entity

let add_animation (entity : Entity.t) (anim : Animation.t) (world : t) =
  Animation_store.set world.animations entity anim;
  world

let get_animation (entity : Entity.t) (w : t) =
  Animation_store.get_opt w.animations entity

let add_input (entity : Entity.t) (input : Input.t) (world : t) =
  Input_store.set world.inputs entity input;
  world

let get_input (entity : Entity.t) (w : t) = Input_store.get_opt w.inputs entity

(* TODO: This feels out of place *)
let add_facing (entity : Entity.t) (facing : Sprite_direction.t) (world : t) =
  Sprite_direction_store.set world.sprite_directions entity facing;
  world

let get_facing (entity : Entity.t) (w : t) =
  Sprite_direction_store.get_opt w.sprite_directions entity

let draw_sprite (ent : Entity.t) (w : t) =
  match
    ( Hashtbl.find_opt w.positions ent,
      Hashtbl.find_opt w.animations ent,
      Hashtbl.find_opt w.sprite_directions ent )
  with
  | Some pos, Some anim, Some facing ->
      let frames, texture =
        match anim.current_kind with
        | Move -> (anim.moving_frames, anim.moving_texture)
        | Jump -> (anim.jumping_frames, anim.jumping_texture)
        | Land -> (anim.landing_frames, anim.landing_texture)
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
