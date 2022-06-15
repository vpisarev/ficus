/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// tests for closures

from UTest import *
import Color

TEST("color.hsv", fun() {
    val rgb_arr_ref =
    [(0, 0, 0),
    (127, 127, 127),
    (255, 255, 255),
    (255, 0, 0),
    (255, 255, 0),
    (0, 255, 0),
    (0, 255, 255),
    (0, 0, 255),
    (255, 0, 255),
    (255, 255, 127)
    ]

    val hsv_arr_ref =
        [(0.f, 0.f, 0.f), (0.f, 0.f, 0.5f),
        (0.f, 0.f, 1.f), (0.f, 1.f, 1.f),
        (60.f, 1.f, 1.f), (120.f, 1.f, 1.f),
        (180.f, 1.f, 1.f), (240.f, 1.f, 1.f),
        (300.f, 1.f, 1.f), (60.f, 0.5f, 1.f)]

    val hsv_arr = [for rgb_i <- rgb_arr_ref {
        val rgb = float(rgb_i)./255.f
        Color.rgb2hsv(rgb)
    }]

    val rgb_arr = [for hsv_i <- hsv_arr {
        val rgb = Color.hsv2rgb(hsv_i)
        round(rgb.*255.f)
    }]

    EXPECT_NEAR(hsv_arr, hsv_arr_ref, 0.01f)
    EXPECT_NEAR(rgb_arr, rgb_arr_ref, 1)
})
