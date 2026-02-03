@extern InitWindow :: (width: int, height: int, title: string) -> int;
@extern SetTargetFPS :: (fps: int) -> int;
@extern WindowShouldClose :: () -> int;
@extern BeginDrawing :: () -> int;
@extern ClearBackground :: (color: int) -> int;
@extern DrawText :: (text: string, pos_x: int, pos_y: int, font_size: int, color: int) -> int;
@extern EndDrawing :: () -> int;
@extern CloseWindow :: () -> int;

RAYWHITE :: 4294309365;   // (245,245,245,255) packed
LIGHTGRAY :: 4291348680;  // (200,200,200,255) packed

main :: () -> int {
  InitWindow(800, 450, "raylib [core] example - basic window");
  SetTargetFPS(60);

  loop (WindowShouldClose() == 0) {
    BeginDrawing();
    ClearBackground(RAYWHITE);
    DrawText("Congrats! You created your first window!", 190, 200, 20, LIGHTGRAY);
    EndDrawing();
  }

  CloseWindow();
  return 0;
}
