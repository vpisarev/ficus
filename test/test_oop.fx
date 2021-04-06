from UTest import *
import Set, Map, Hashmap, Hashset

interface IBase
{
    fun name(): string
}

interface IClone
{
    fun clone(): IClone
}

interface IShape : IBase
{
    fun area(): float
}

type pixel_t = (uint8 * 4)
type canvas_t = pixel_t [,]
type color_t = (float * 4)
type shape_colors_t =
{
    border_color: color_t
    border_thickness: float
    fill_color: color_t
}
val color_black = (0.f, 0.f, 0.f, 1.f)
val color_white = (1.f, 1.f, 1.f, 1.f)
val color_red = (1.f, 0.f, 0.f, 1.f)
val color_green = (0.f, 1.f, 0.f, 1.f)
val color_blue = (0.f, 0.f, 1.f, 1.f)
fun make_solid(c: color_t) = shape_colors_t {border_color=c, border_thickness=0.f, fill_color=c}
type bbox_t = {x: float; y: float; w: float; h: float}

object type Rect : IShape, IClone =
{
    cx: float; cy: float
    fx: float; fy: float
    angle: float=0.f
    corner_r: float=0.f
    sc: shape_colors_t
}

object type Ellipse : IClone, IShape =
{
    cx: float; cy: float
    fx: float; fy: float
    angle: float=0.f
    sc: shape_colors_t
}

fun IShape.name(_: Rect) = "rectangle"
fun IShape.name(_: Ellipse) = "ellipse"
fun IClone.clone(r: Rect) = (Rect {cx=r.cx, cy=r.cy, fx=r.fx, fy=r.fy, angle=r.angle, corner_r=r.corner_r, sc=r.sc} :> IClone)
fun IClone.clone(e: Ellipse) = (Ellipse {cx=e.cx, cy=e.cy, fx=e.fx, fy=e.fy, angle=e.angle, sc=e.sc} :> IClone)
fun IShape.area(r: Rect) = r.fx*r.fy
fun IShape.area(c: Ellipse) = float(M_PI*c.fx*c.fy)

TEST("oop.interfaces", fun()
{
    val r = Rect {cx=100.f, cy=100.f, fx=16.f, fy=10.f, sc=make_solid(color_red)}
    val c = Ellipse {cx=150.f, cy=100.f, fx=20.f, fy=20.f, sc=make_solid(color_blue)}

    val shapes = [: (r :> IShape), (c :> IShape) :]

    for sh@i <- shapes {
        if i == 0 {
            EXPECT_EQ(sh.name(), "rectangle")
            EXPECT_EQ(sh.area(), 160.f)
        } else {
            EXPECT_EQ(sh.name(), "ellipse")
            EXPECT_NEAR(sh.area(), 1256.6371f, 0.01f)
        }
    }
})
