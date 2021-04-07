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

type rect_data_t =
{
    cx: float; cy: float
    fx: float; fy: float
    angle: float=0.f
    corner_r: float=0.f
    sc: shape_colors_t
}

type ellipse_data_t =
{
    cx: float; cy: float
    fx: float; fy: float
    angle: float=0.f
    sc: shape_colors_t
}

object type Rect : IShape, IClone =
{
    r: rect_data_t ref
}

object type Ellipse : IClone, IShape =
{
    r: ellipse_data_t ref
}

fun make_rect(cx: float, cy: float, fx: float, fy: float,
              sc: shape_colors_t, ~angle:float=0.f, ~corner_r: float=0.f)
{
    Rect {r=ref (rect_data_t {cx=cx, cy=cy, fx=fx, fy=fy, angle=angle, sc=sc, corner_r=corner_r})}
}

fun make_ellipse(cx: float, cy: float, fx: float, fy: float,
                 sc: shape_colors_t, ~angle:float=0.f, ~corner_r: float=0.f)
{
    Ellipse {r=ref (ellipse_data_t {cx=cx, cy=cy, fx=fx, fy=fy, angle=angle, sc=sc})}
}

fun Rect.name() = "rectangle"
fun Rect.area() = self.r->fx*self.r->fy
fun Rect.get_scale() = (self.r->fx, self.r->fy)
fun Rect.set_scale(fx: float, fy: float) { self.r->fx = fx; self.r->fy = fy }
fun Rect.clone()
{
    val {cx, cy, fx, fy, angle, corner_r, sc} = *self.r
    (make_rect(cx, cy, fx, fy, sc, angle=angle, corner_r=corner_r) :> IClone)
}

fun Ellipse.name() = "ellipse"
fun Ellipse.area() = float(M_PI*self.r->fx*self.r->fy)
fun Ellipse.get_scale() = (self.r->fx, self.r->fy)
fun Ellipse.set_scale(fx: float, fy: float) { self.r->fx = fx; self.r->fy = fy }
fun Ellipse.clone()
{
    val {cx, cy, fx, fy, angle, sc} = *self.r
    (make_ellipse(cx, cy, fx, fy, sc, angle=angle) :> IClone)
}

TEST("oop.interfaces", fun()
{
    val r0 = make_rect(100.f, 100.f, 16.f, 10.f, make_solid(color_red))
    val e0 = make_ellipse(150.f, 100.f, 20.f, 20.f, make_solid(color_blue))

    val shapes = [: (r0 :> IShape), (e0 :> IShape) :]
    val ref_data = [: ("rectangle", 160.f), ("ellipse", 1256.6371f) :]

    for sh@i <- shapes, (n, a) <- ref_data {
        EXPECT_EQ(sh.name(), n)
        val sh2 = ((sh :> IClone).clone() :> IShape)
        val (fx, fy) = sh2.get_scale()
        sh2.set_scale(fx*2, fy*2)
        EXPECT_NEAR(sh2.area(), a*4, 0.01f)
        EXPECT_NEAR(sh.area(), a, 0.01f)
        fun test_ellipse()
        {
            val e = (sh2 :> Ellipse)
            EXPECT_EQ(e.r->cx, e0.r->cx)
            EXPECT_EQ(e.r->cy, e0.r->cy)
        }
        if i == 0 {
            EXPECT_THROWS(test_ellipse, TypeMismatchError, msg="cast (Rect:>IShape):>Ellipse should fail")
        } else {
            EXPECT_NO_THROWS(test_ellipse, "cast (Ellipse:>IShape):>Ellipse should succeed")
        }
    }
})
