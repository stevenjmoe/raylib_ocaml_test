open Ecs

let animation_system (dt : float) (w : world) =
  Hashtbl.iter
    (fun ent anim ->
      match
        (Hashtbl.find_opt w.velocities ent, Hashtbl.find_opt w.positions ent)
      with
      | Some vel, Some _pos ->
          let speed = abs_float vel.vx in
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
          if Raylib.is_key_down Raylib.Key.A then (
            vel.vx <- vel.vx -. 100.0;
            facing.horizontal <- `Left);
          if Raylib.is_key_down Raylib.Key.D then (
            vel.vx <- vel.vx +. 100.0;
            facing.horizontal <- `Right);
          if Raylib.is_key_pressed Raylib.Key.Space && vel.vy = 0. then
            vel.vy <- vel.vy -. 1000.0
      | _ -> ())
    w.inputs;
  w

let movement_system (dt : float) (w : world) =
  Hashtbl.iter
    (fun ent vel ->
      match Hashtbl.find_opt w.positions ent with
      | Some pos ->
          pos.x <- pos.x +. (vel.vx *. dt);
          pos.y <- pos.y +. (vel.vy *. dt);

          (* TODO: temporarily stop my guy from falling off the screen with no collisions *)
          let screen_h = float_of_int (Raylib.get_screen_height ()) in
          let sprite_h = 100. in
          let bottom_y = screen_h -. sprite_h in

          if pos.y > bottom_y then (
            pos.y <- bottom_y;
            vel.vy <- 0.0)
      | None -> ())
    w.velocities;
  w

let gravity_system (_dt : float) (w : world) =
  Hashtbl.iter
    (fun _ent vel ->
      vel.vy <- (if vel.vy > 600. then 600. else vel.vy +. 100.);
      ())
    w.velocities;
  w
