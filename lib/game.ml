open Systems

let width = 800
let height = 600

let rec game_loop
    (w : World.t)
    (player : Engine.Entity.t)
    (camera : Raylib.Camera2D.t)
    (player_pos : Engine.Position.t) =
  let open Raylib in
  if Raylib.window_should_close () then (
    Raylib.close_window ();
    Raylib.close_audio_device ())
  else
    let dt = Raylib.get_frame_time () in

    (* Update systems *)
    let w =
      input_system w
      |> gravity_system dt
      |> movement_system dt
      |> animation_system dt
    in

    Camera2D.set_target camera
      (Vector2.create (player_pos.x +. 20.0) (player_pos.y +. 20.0));

    (* Rendering *)
    Raylib.begin_drawing ();
    begin_mode_2d camera;
    draw_rectangle (-6000) 320 13000 8000 Color.darkgray;
    Raylib.clear_background Color.blue;
    World.draw_sprite player w;
    end_mode_2d ();
    Raylib.draw_text "Test!" 10 10 20 Color.white;
    Raylib.end_drawing ();

    game_loop w player camera player_pos

let setup () =
  let open Raylib in
  set_config_flags [ Raylib.ConfigFlags.Window_resizable ];
  init_window width height "Test game";
  init_audio_device ();

  let world = Spawn.create_world () in
  let world, player = Spawn.create_player world in
  match World.get_position player world with
  | Some pos ->
      let camera =
        Camera2D.create
          (Vector2.create
             (Float.of_int width /. 2.0)
             (Float.of_int height /. 2.0))
          (Vector2.create (pos.x +. 20.0) (pos.y +. 20.0))
          0.0 1.0
      in
      Raylib.set_target_fps 60;
      (world, player, camera, pos)
  | None -> failwith "Could not find player position"

let run =
  let world, player, camera, player_pos = setup () in
  game_loop world player camera player_pos
