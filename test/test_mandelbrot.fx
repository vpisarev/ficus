/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// mandelbrot.fx example converted into a test. Computes fractal and compares ROI with a reference result

from UTest import *

TEST("mandelbrot.regression", fun()
{
    val N = 2000
    val w = N, h = N, MAX_ITER = 50
    val inv = 2.0 / w

    val x_ = [ @parallel for x <- 0:w {double(x) * inv - 1.5}]
    val result: uint8 [,] = [
        @parallel
            for y <- 0:h
                for x8 <- 0:(w/8)
        {
            val y_ = double(y) * inv - 1.0
            val x = x8*8
            val cr = (x_[x + 0], x_[x + 1], x_[x + 2], x_[x + 3], x_[x + 4], x_[x + 5], x_[x + 6], x_[x + 7])
            val ci = (y_, y_, y_, y_, y_, y_, y_, y_)
            var zr = cr, zi = ci
            var bits = (false, false, false, false, false, false, false, false)

            for iter <- 0:MAX_ITER
            {
                val rr = zr .* zr
                val ii = zi .* zi

                val mag = rr + ii
                bits |= mag .> 4.0

                val ir = zr .* zi
                zr = (rr - ii) + cr
                zi = (ir + ir) + ci

                if all(bits) {break}
            }
            val mask = (bits.0 << 7) + (bits.1 << 6) + (bits.2 << 5) + (bits.3 << 4) +
                (bits.4 << 3) + (bits.5 << 2) + (bits.6 << 1) + (bits.7 << 0)

            uint8(mask ^ 255)
        }
    ]

    val roi = result[1000-20:1000+12,8:16]
    EXPECT_EQ(`roi`,
      [0u8, 0u8, 0u8, 0u8, 2u8, 192u8, 8u8, 96u8;
        0u8, 0u8, 0u8, 0u8, 0u8, 64u8, 60u8, 48u8;
        0u8, 0u8, 0u8, 0u8, 0u8, 66u8, 24u8, 32u8;
        0u8, 0u8, 0u8, 0u8, 49u8, 234u8, 30u8, 48u8;
        0u8, 0u8, 0u8, 0u8, 0u8, 126u8, 159u8, 228u8;
        0u8, 0u8, 0u8, 0u8, 0u8, 255u8, 255u8, 252u8;
        0u8, 0u8, 0u8, 0u8, 0u8, 31u8, 255u8, 255u8;
        0u8, 0u8, 0u8, 0u8, 0u8, 15u8, 255u8, 255u8;
        0u8, 0u8, 0u8, 0u8, 0u8, 63u8, 255u8, 255u8;
        0u8, 0u8, 0u8, 2u8, 2u8, 31u8, 255u8, 255u8;
        0u8, 0u8, 0u8, 1u8, 1u8, 63u8, 255u8, 255u8;
        0u8, 0u8, 0u8, 0u8, 144u8, 255u8, 255u8, 255u8;
        0u8, 0u8, 0u8, 0u8, 0u8, 127u8, 255u8, 255u8;
        0u8, 0u8, 0u8, 0u8, 98u8, 127u8, 255u8, 255u8;
        0u8, 0u8, 0u8, 0u8, 50u8, 255u8, 255u8, 255u8;
        0u8, 0u8, 0u8, 0u8, 31u8, 255u8, 255u8, 255u8;
        0u8, 0u8, 36u8, 0u8, 31u8, 255u8, 255u8, 255u8;
        0u8, 0u8, 32u8, 0u8, 127u8, 255u8, 255u8, 255u8;
        0u8, 0u8, 0u8, 1u8, 63u8, 255u8, 255u8, 255u8;
        0u8, 0u8, 48u8, 67u8, 255u8, 255u8, 255u8, 255u8;
        255u8, 255u8, 255u8, 255u8, 255u8, 255u8, 255u8, 255u8;
        0u8, 0u8, 48u8, 67u8, 255u8, 255u8, 255u8, 255u8;
        0u8, 0u8, 0u8, 1u8, 63u8, 255u8, 255u8, 255u8;
        0u8, 0u8, 32u8, 0u8, 127u8, 255u8, 255u8, 255u8;
        0u8, 0u8, 36u8, 0u8, 31u8, 255u8, 255u8, 255u8;
        0u8, 0u8, 0u8, 0u8, 31u8, 255u8, 255u8, 255u8;
        0u8, 0u8, 0u8, 0u8, 50u8, 255u8, 255u8, 255u8;
        0u8, 0u8, 0u8, 0u8, 98u8, 127u8, 255u8, 255u8;
        0u8, 0u8, 0u8, 0u8, 0u8, 127u8, 255u8, 255u8;
        0u8, 0u8, 0u8, 0u8, 144u8, 255u8, 255u8, 255u8;
        0u8, 0u8, 0u8, 1u8, 1u8, 63u8, 255u8, 255u8;
        0u8, 0u8, 0u8, 2u8, 2u8, 31u8, 255u8, 255u8 ])
})
