/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

from UTest import *
import Set, Map, Hashmap, Hashset

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
    //fun draw(img: canvas_t): void
    fun area(): float
    fun get_scale(): (float, float)
    fun set_scale(fx: float, fy: float): void
    /*fun perimeter(): float
    fun center(): (float, float)
    fun bbox(): bbox_t
    fun move_to(dx: float, dy: float): void
    fun get_scale(): (float, float)
    fun rotate(angle: float): void
    fun setcolors(sc: shape_colors_t): void
    fun getcolors(): shape_colors_t*/
}

class Rect : IShape, IClone
{
    var cx: float
    var cy: float
    var fx: float
    var fy: float
    var angle: float=0.f
    var corner_r: float=0.f
    var sc: shape_colors_t
}

class Ellipse : IClone, IShape
{
    var cx: float
    var cy: float
    var fx: float
    var fy: float
    var angle: float=0.f
    var sc: shape_colors_t
}

fun Rect.name() = "rectangle"
fun Rect.area() = self.fx*self.fy
fun Rect.get_scale() = (self.fx, self.fy)
fun Rect.set_scale(fx: float, fy: float) { self.fx = fx; self.fy = fy }
fun Rect.clone()
{
    val {cx, cy, fx, fy, angle, corner_r, sc} = self
    (Rect {cx=cx, cy=cy, fx=fx, fy=fy, angle=angle, corner_r=corner_r, sc=sc} :> IClone)
}

fun Ellipse.name() = "ellipse"
fun Ellipse.area() = float(M_PI*self.fx*self.fy)
fun Ellipse.get_scale() = (self.fx, self.fy)
fun Ellipse.set_scale(fx: float, fy: float) { self.fx = fx; self.fy = fy }
fun Ellipse.clone()
{
    val {cx, cy, fx, fy, angle, sc} = self
    (Ellipse {cx=cx, cy=cy, fx=fx, fy=fy, angle=angle, sc=sc} :> IClone)
}

TEST("oop.interfaces", fun()
{
    val r0 = Rect {cx=100.f, cy=100.f, fx=16.f, fy=10.f, sc=make_solid(color_red)}
    val e0 = Ellipse {cx=150.f, cy=100.f, fx=20.f, fy=20.f, sc=make_solid(color_blue)}

    val shapes = [:: (r0 :> IShape), (e0 :> IShape)]
    val ref_data = [:: ("rectangle", 160.f), ("ellipse", 1256.6371f)]

    for sh@i <- shapes, (n, a) <- ref_data {
        EXPECT_EQ(`sh.name()`, n)
        val sh2 = ((sh :> IClone).clone() :> IShape)
        val (fx, fy) = sh2.get_scale()
        sh2.set_scale(fx*2, fy*2)
        EXPECT_NEAR(`sh2.area()`, a*4, 0.01f)
        EXPECT_NEAR(`sh.area()`, a, 0.01f)
        fun test_ellipse()
        {
            val e = (sh2 :> Ellipse)
            EXPECT_EQ(`e.cx`, e0.cx)
            EXPECT_EQ(`e.cy`, e0.cy)
        }
        if i == 0 {
            EXPECT_THROWS(`test_ellipse`, TypeMismatchError, msg="cast (Rect:>IShape):>Ellipse should fail")
        } else {
            EXPECT_NO_THROWS(`test_ellipse`, msg="cast (Ellipse:>IShape):>Ellipse should succeed")
        }
    }
})
