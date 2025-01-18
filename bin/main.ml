let setup () =
  Raylib.init_window 800 450 "test game";
  Raylib.set_target_fps 60

let rec loop () =
  if Raylib.window_should_close () then Raylib.close_window ()
  else
    let open Raylib in
    begin_drawing ();
    clear_background Color.raywhite;
    draw_text "A window! Amazing!" 190 200 20 Color.lightgray;
    end_drawing ();
    loop ()

let () = setup () |> loop
