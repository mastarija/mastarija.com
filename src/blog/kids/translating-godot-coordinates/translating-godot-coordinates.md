---
title : Translating Godot Coordinates
caption : From screen to the world
excerpt : |
  Godot is a great piece of software, but it's missing some
  convenient methods here and there
pubDate : 2022-01-31
---

As I'm working on a game in Godot, I wanted to draw some touch indicators on the
screen so that I can visualise my gestures and debug my touch controller logic.

Godots touch events return the position of the touch "screen" coordinates,
however, to draw something using the `draw_` functions I have to provide the
"game world" coordinates.

If I were to just pass the raw screen coordinates to any of the `draw_`
functions and my camera is looking at some place faraway from the origin, I
might never see the results of my drawings.

To fix that we need to somehow translate "screen" coordinates to the "world"
coordinates that are currently right under those screen coordinates.

I was expecting a camera node to have some convenient functions like
`to_screen_coordinates` and `to_world_coordinates` but unfortunately that wasn't
the case.

After some digging through the documentation I've found we can get the canvas
transformation matrix which tells us what transformations have been applied to
the canvas (offset and such). This transformations are used to create the
illusion of camera and movement through space.

To make our screen coordinates "fit" the world coordinates we actually want to
apply the opposite transformation to them, and for that we can use the inbuilt
`affine_inverse` method of the `Transform2D` class.


Here's what the final solution looks like:

```
func to_screen ( pos : Vector2 ) -> Vector2 :
  return get_canvas_transform().affine_inverse().basis_xform( pos )
```
