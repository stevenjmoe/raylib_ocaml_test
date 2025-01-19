open Raylib

let screen_width = 800
let screen_height = 450

module Sprite = struct
  type t = {
    texture : Texture2D.t;
    x : float;
    y : float;
    width : float;
    height : float;
    velocity : Vector2.t;
  }

  (** [set_velocity sprite vx vy] returns a new sprite with [velocity] updated
      with [vx] and [vy] *)
  let set_velocity sprite vx vy =
    { sprite with velocity = Vector2.create vx vy }

  (** [apply_velocity sprite] returns a new sprite with the [x] and [y] values
      updated with the values from [sprite.velocity] and the elapsed time frame.
  *)
  let apply_velocity sprite =
    let dt = get_frame_time () in
    let x = sprite.x +. (Vector2.x sprite.velocity *. dt) in
    let y = sprite.y +. (Vector2.y sprite.velocity *. dt) in
    { sprite with x; y }

  (* TODO: This makes assumptions about the sprite being drawn that might not always be true. I can fix this later when it is needed. *)
  let draw sprite =
    let src = Rectangle.create 0.0 0.0 48.0 48.0 in
    let dst = Rectangle.create sprite.x sprite.y sprite.width sprite.height in
    draw_texture_pro sprite.texture src dst (Vector2.create 0.0 0.0) 0.0
      Color.white
end

let move_player sprite =
  let sprite =
    if is_key_down Key.D then Sprite.set_velocity sprite 100.0 0.0
    else if is_key_down Key.A then Sprite.set_velocity sprite (-100.0) 0.0
    else Sprite.set_velocity sprite 0.0 0.0
  in
  sprite

let apply_gravity (sprite : Sprite.t) =
  let velocity =
    match Vector2.y sprite.velocity +. 10.0 with
    | v when v > 200.0 -> 200.0
    | v -> v
  in
  { sprite with velocity = Vector2.create (Vector2.x sprite.velocity) velocity }

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
        x = 200.0;
        y = 100.0;
        width = 100.0;
        height = 100.0;
        velocity = Vector2.create 0.0 0.0;
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
  if Raylib.window_should_close () then (
    close_window ();
    close_audio_device ())
  else
    let player = player |> move_player |> apply_gravity |> apply_vel in
    begin_drawing ();
    clear_background Color.blue;
    draw_text "A window! Amazing!" 190 200 20 Color.lightgray;
    Sprite.draw player;
    end_drawing ();
    loop player

let run = setup () |> loop
