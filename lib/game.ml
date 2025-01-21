open Ecs

let rec game_loop (w : world) (player : Entity.t) =
  let open Raylib in
  if Raylib.window_should_close () then (
    Raylib.close_window ();
    Raylib.close_audio_device ())
  else
    let dt = Raylib.get_frame_time () in

    (* Update systems *)
    let w = input_system w |> movement_system dt |> animation_system dt in

    (* Rendering *)
    Raylib.begin_drawing ();
    Raylib.clear_background Color.blue;
    Raylib.draw_text "Test!" 10 10 20 Color.white;
    draw_sprite player w;
    Raylib.end_drawing ();

    game_loop w player

let setup () =
  Raylib.init_window 800 600 "Test game";
  Raylib.init_audio_device ();
  let world = Ecs.create_world in
  let world, player = create_player world in
  Raylib.set_target_fps 60;
  (world, player)

let run =
  let world, player = setup () in
  game_loop world player
