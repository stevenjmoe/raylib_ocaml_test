open Raylib

let screen_width = 800
let screen_height = 450

type sprite = { texture : Texture2D.t; dest_rect : Rectangle.t }

let setup () =
  init_window screen_width screen_height "test game";
  init_audio_device ();

  let player_idle_texture =
    load_texture
      "assets/Asset Packs 1-5/Asset Pack-V1/Player Idle/Player Idle 48x48.png"
  in

  let player =
    {
      texture = player_idle_texture;
      dest_rect = Rectangle.create 190.0 200.0 100.0 100.0;
    }
  in

  let _ =
    Camera2D.create (Vector2.create 0.0 0.0)
      (Vector2.create
         (float_of_int (screen_width + 2))
         (float_of_int (screen_height + 2)))
      0.0 1.0
  in
  set_target_fps 60;
  player

let rec loop (player : sprite) =
  if Raylib.window_should_close () then Raylib.close_window ()
  else begin_drawing ();
  clear_background Color.blue;
  draw_text "A window! Amazing!" 190 200 20 Color.lightgray;
  let _ = player.dest_rect in
  draw_texture_pro player.texture
    (Rectangle.create 0.0 48.0 16.0 16.0)
    player.dest_rect (Vector2.create 0.0 0.0) 0.0 Color.lightgray;
  end_drawing ();
  loop player;
  ()

let run = setup () |> loop
