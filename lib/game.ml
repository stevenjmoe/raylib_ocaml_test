open Raylib

let screen_width = ref 800
let screen_height = ref 450

let setup () =
  init_window !screen_width !screen_height "test game";
  init_audio_device ();
  set_target_fps 60

let rec loop () =
  if Raylib.window_should_close () then Raylib.close_window ()
  else begin_drawing ();
  clear_background Color.raywhite;
  draw_text "A window! Amazing!" 190 200 20 Color.lightgray;
  end_drawing ();
  loop ();
  ()

let run = setup () |> loop
