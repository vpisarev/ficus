/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// Various color conversion and color management functions

// h: 0..360, s: 0..1, v: 0..1 -> r: 0..1, g: 0..1, b: 0..1
// 't ~ float or double (not integer)
fun hsv2rgb(h: 't, s: 't, v: 't)
{
    val _360 = (360 :> 't), _0 = (0 :> 't)
    val _1_60 = (1./60 :> 't)
    val h = h - (h >= _360)*_360 + (h < _0)*_360
    val h60 = h*_1_60
    val i = int(h60)
    val frac = h60 - i

    val p = v*(1 - s);
    val q = v*(1 - s*frac);
    val t = v*(1 - s*(1 - frac))

    if i == 0 { (v, t, p) }
    else if i == 1 { (q, v, p) }
    else if i == 2 { (p, v, t) }
    else if i == 3 { (p, q, v) }
    else if i == 4 { (t, p, v) }
    else { (v, p, q) }
}

@inline fun hsv2rgb(hsv: ('t*3)) = hsv2rgb(hsv.0, hsv.1, hsv.2)

fun rgb2hsv(r: 't, g: 't, b: 't)
{
    val _0 = (0 :> 't)
    val minv = min(min(r, g), b)
    val maxv = max(max(r, g), b)
    val v = maxv
    val d = maxv - minv

    if d < (FLT_EPSILON :> 't) {
        (_0, _0, v)
    } else {
        val s = d/maxv
        val h = if r >= maxv { val h = (g - b)/d; h + (h < _0)*6 }
                else if g >= maxv { 2 + (b - r)/d }
                else { 4 + (r - g)/d }
        (h*60, s, v)
    }
}

@inline fun rgb2hsv(rgb: ('t*3)) = rgb2hsv(rgb.0, rgb.1, rgb.2)
