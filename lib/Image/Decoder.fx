/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

@ccode {
#define STB_IMAGE_IMPLEMENTATION
#define STBI_ONLY_JPEG
#define STBI_ONLY_PNG
#define STBI_ONLY_BMP
#include "stb_image.h"
}

// [TODO] modify function to avoid double memory allocation
@private fun imread_(filename: string, bits: int, desired_channels: int): uint8 [,]
@ccode {
    fx_cstr_t c_filename;
    int width=0, height=0, channels=0;
    unsigned char* data = 0;
    int fx_status = fx_str2cstr(filename, &c_filename, 0, 0);
    if (fx_status < 0)
        return fx_status;
    if (bits == 8)
        data = stbi_load(c_filename.data, &width, &height, &channels, desired_channels);
    else
        data = (unsigned char*)stbi_load_16(c_filename.data, &width, &height, &channels, desired_channels);
    fx_free_cstr(&c_filename);
    if (data) {
        if (channels == desired_channels) {
            int_ size[] = {height, width};
            size_t elemsize = channels*(bits == 8 ? 1 : 2);
            fx_status = fx_make_arr(2, size, elemsize, 0, 0, data, fx_result);
        } else {
            fx_status = FX_SET_EXN_FAST(FX_EXN_TypeMismatchError);
        }
    } else {
        fx_status = FX_SET_EXN_FAST(FX_EXN_IOError);
    }
    if (data)
        stbi_image_free(data);
    return fx_status;
}

fun imread_gray(filename: string) =
    (reinterpret(imread_(filename, 8, 1)) : uint8 [,])
fun imread_rgb(filename: string) =
    (reinterpret(imread_(filename, 8, 3)) : uint8x3 [,])
fun imread_rgba(filename: string) =
    (reinterpret(imread_(filename, 8, 4)) : uint8x4 [,])
