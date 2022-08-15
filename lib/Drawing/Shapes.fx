/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

///////////// Draw basic shapes ///////////////

fun filled_rectangle(image: 't [,], p1: intx2, p2: intx2, color: 't)
{
    val (h, w) = image.size()
    val x0 = min(p1.0, p2.0), x1 = max(p1.0, p2.0)
    val y0 = min(p1.1, p2.1), y1 = max(p1.1, p2.1)

    if x0 < w && 0 <= x1 && y0 < h && 0 <= y1 {
        val x0 = max(x0, 0)
        val x1 = min(x1, w-1)+1
        val y0 = max(y0, 0)
        val y1 = min(y1, h-1)+1
        for y <- y0 : y1 {
            for x <- x0 : x1 {
                image[y, x] = color
            }
        }
    }
}

fun rectangle(image: 't [,], p1: intx2, p2: intx2, color: 't, ~thickness: int=1)
{
    val (h, w) = image.size()
    val x0 = min(p1.0, p2.0), x1 = max(p1.0, p2.0)
    val y0 = min(p1.1, p2.1), y1 = max(p1.1, p2.1)
    val thickness = max(thickness, 1)
    val r0 = thickness/2, r1 = thickness-1-r0

    if x0-r0 < w && 0 <= x1+r1 {
        val x0_ = max(x0-r0, 0)
        val x1_ = min(x1+r1, w-1)+1
        for x <- x0_ : x1_ {
            for dy <- 0:thickness*2 {
                val y = if dy < thickness {y0 - r0 + dy} else {y1 - r0 + dy-thickness}
                if y < 0 || y >= h {continue}
                image[y, x] = color
            }
        }
    }

    if y0+r1+1 < h && y1-r0-1 >= 0 && y0+r1+1 < y1-r0 {
        val y0_ = max(y0+r1, 0)
        val y1_ = min(y1-r0-1, h-1)+1
        for y <- y0_ : y1_ {
            for dx <- 0:thickness*2 {
                val x = if dx < thickness {x0 - r0 + dx} else {x1 - r0 + dx-thickness}
                if x < 0 || x >= w {continue}
                image[y, x] = color
            }
        }
    }
}
