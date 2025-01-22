open Ecs

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
      match
        ( Hashtbl.find_opt w.velocities ent,
          Hashtbl.find_opt w.facing_directions ent )
      with
      | Some vel, Some facing ->
          vel.vx <- 0.0;
          vel.vy <- 0.0;
          if Raylib.is_key_down Raylib.Key.A then (
            vel.vx <- vel.vx -. 100.0;
            facing.horizontal <- `Left);
          if Raylib.is_key_down Raylib.Key.D then (
            vel.vx <- vel.vx +. 100.0;
            facing.horizontal <- `Right);
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
