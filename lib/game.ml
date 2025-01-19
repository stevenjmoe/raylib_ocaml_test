open Raylib

let screen_width = 800
let screen_height = 450

module Sprite = struct
  type t = { texture : Texture2D.t; dest_rect : Rectangle.t; vel : Vector2.t }

  let set_velocity sprite vel = { sprite with vel }

  let apply_velocity sprite =
    let x =
      Rectangle.x sprite.dest_rect +. (Vector2.x sprite.vel *. get_frame_time ())
    in
    let y =
      Rectangle.y sprite.dest_rect +. (Vector2.y sprite.vel *. get_frame_time ())
    in
    Rectangle.set_x sprite.dest_rect x;
    Rectangle.set_y sprite.dest_rect y;
    ()
end

let move_player sprite =
  let sprite =
    if is_key_down Key.D then
      Sprite.set_velocity sprite (Vector2.create 100.0 0.0)
    else if is_key_down Key.A then
      Sprite.set_velocity sprite (Vector2.create (-100.0) 0.0)
    else Sprite.set_velocity sprite (Vector2.create 0.0 0.0)
  in
  sprite

let apply_gravity (sprite : Sprite.t) =
  let velocity =
    match Vector2.y sprite.vel +. 10.0 with v when v > 200.0 -> 200.0 | v -> v
  in
  Sprite.set_velocity sprite (Vector2.create (Vector2.x sprite.vel) velocity)

let apply_vel sprite = Sprite.apply_velocity sprite

let setup () =
  init_window screen_width screen_height "test game";
  init_audio_device ();

  let player_idle_texture =
    load_texture
      "assets/Asset Packs 1-5/Asset Pack-V1/Player Idle/Player Idle 48x48.png"
  in

  let player =
    Sprite.
      {
        texture = player_idle_texture;
        dest_rect = Rectangle.create 200.0 100.0 100.0 100.0;
        vel = Vector2.create 0.0 0.0;
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

let rec loop (player : Sprite.t) =
  if Raylib.window_should_close () then Raylib.close_window ()
  else move_player player |> apply_gravity |> apply_vel;
  begin_drawing ();

  clear_background Color.blue;
  draw_text "A window! Amazing!" 190 200 20 Color.lightgray;
  let _ = player.dest_rect in
  draw_texture_pro player.texture
    (Rectangle.create 0.0 0.0 48.0 48.0)
    player.dest_rect (Vector2.create 0.0 0.0) 0.0 Color.lightgray;
  end_drawing ();
  loop player;
  ()

let run = setup () |> loop
