module Entity = struct
  type t = int
end

module Component = struct
  module Make_component (C : sig
    type t
  end) =
  struct
    type elt = C.t
    type store = (int, elt) Hashtbl.t

    let create () : store = Hashtbl.create 16
    let set (tbl : store) (k : int) (v : elt) = Hashtbl.replace tbl k v
    let fold f (tbl : store) init = Hashtbl.fold f tbl init
    let iter f (tbl : store) = Hashtbl.iter f tbl
    let get_opt (tbl : store) (k : int) = Hashtbl.find_opt tbl k
    let remove (tbl : store) (k : int) = Hashtbl.remove tbl k
  end
end

module Position = struct
  type t = { mutable x : float; mutable y : float; w : float; h : float }
end

module Position_store = Component.Make_component (Position)

module Velocity = struct
  type t = { mutable vx : float; mutable vy : float }
end

module Velocity_store = Component.Make_component (Velocity)

module Sprite_direction = struct
  type t = { mutable horizontal : [ `Left | `Right ] }
end

module Sprite_direction_store = Component.Make_component (Sprite_direction)

module Input = struct
  type t = {
    mutable left : bool;
    mutable right : bool;
    mutable up : bool;
    mutable down : bool;
  }
end

module Input_store = Component.Make_component (Input)

module Animation = struct
  type animation_kind = Idle | Move | Attack | Jump | Land

  type frame_info = {
    src_x : float;
    src_y : float;
    duration : float;
    width : float;
    height : float;
  }

  type t = {
    mutable current_kind : animation_kind;
    mutable current_frame : int;
    mutable timer : float;
    idle_frames : frame_info array;
    idle_texture : Raylib.Texture.t;
    moving_frames : frame_info array;
    moving_texture : Raylib.Texture.t;
    jumping_frames : frame_info array;
    jumping_texture : Raylib.Texture.t;
    landing_frames : frame_info array;
    landing_texture : Raylib.Texture.t;
  }
end

module Animation_store = Component.Make_component (Animation)
