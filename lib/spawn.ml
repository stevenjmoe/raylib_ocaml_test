open Ecs

let create_player (w : world) =
  let open Raylib in
  let w, player = create_entity w in
  let pos = { x = 200.0; y = 200.0; w = 100.0; h = 100.0 } in
  let vel = { vx = 0.0; vy = 0.0 } in
  let facing = { horizontal = `Right } in

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
      { src_x = 0.0; src_y = 0.0; duration = 0.2; width = 48.0; height = 48.0 };
      { src_x = 48.0; src_y = 0.0; duration = 0.2; width = 48.0; height = 48.0 };
      { src_x = 96.0; src_y = 0.0; duration = 0.2; width = 48.0; height = 48.0 };
      {
        src_x = 144.0;
        src_y = 0.0;
        duration = 0.2;
        width = 48.0;
        height = 48.0;
      };
      {
        src_x = 192.0;
        src_y = 0.0;
        duration = 0.2;
        width = 48.0;
        height = 48.0;
      };
      {
        src_x = 240.0;
        src_y = 0.0;
        duration = 0.2;
        width = 48.0;
        height = 48.0;
      };
      {
        src_x = 288.0;
        src_y = 0.0;
        duration = 0.2;
        width = 48.0;
        height = 48.0;
      };
      {
        src_x = 336.0;
        src_y = 0.0;
        duration = 0.2;
        width = 48.0;
        height = 48.0;
      };
      {
        src_x = 384.0;
        src_y = 0.0;
        duration = 0.2;
        width = 48.0;
        height = 48.0;
      };
      {
        src_x = 432.0;
        src_y = 0.0;
        duration = 0.2;
        width = 48.0;
        height = 48.0;
      };
    |]
  in
  let frames_running =
    [|
      { src_x = 0.0; src_y = 0.0; duration = 0.2; width = 48.0; height = 48.0 };
      { src_x = 48.0; src_y = 0.0; duration = 0.2; width = 48.0; height = 48.0 };
      { src_x = 96.0; src_y = 0.0; duration = 0.2; width = 48.0; height = 48.0 };
      {
        src_x = 144.0;
        src_y = 0.0;
        duration = 0.2;
        width = 48.0;
        height = 48.0;
      };
      {
        src_x = 192.0;
        src_y = 0.0;
        duration = 0.2;
        width = 48.0;
        height = 48.0;
      };
      {
        src_x = 240.0;
        src_y = 0.0;
        duration = 0.2;
        width = 48.0;
        height = 48.0;
      };
      {
        src_x = 288.0;
        src_y = 0.0;
        duration = 0.2;
        width = 48.0;
        height = 48.0;
      };
      {
        src_x = 336.0;
        src_y = 0.0;
        duration = 0.2;
        width = 48.0;
        height = 48.0;
      };
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
    |> add_facing player facing
  in
  (w, player)

let create_world () =
  {
    next_id = 0;
    positions = Hashtbl.create 100;
    velocities = Hashtbl.create 100;
    animations = Hashtbl.create 100;
    inputs = Hashtbl.create 100;
    facing_directions = Hashtbl.create 100;
  }
