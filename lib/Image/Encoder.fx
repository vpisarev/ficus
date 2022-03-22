/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

@ccode {
#define STB_IMAGE_WRITE_IMPLEMENTATION
#include "stb_image_write.h"
}

@private fun imwrite_(filename: string, img: uint8 [,], bits: int, channels: int, jpeg_quality: int): void
@ccode {
    fx_cstr_t c_filename;
    int width=(int)img->dim[1].size, height=(int)img->dim[0].size;
    int stride = (int)img->dim[0].step;
    unsigned char* data = 0;
    int fx_status = fx_str2cstr(filename, &c_filename, 0, 0);
    char* path;
    int pathlen;
    if (fx_status < 0)
        return fx_status;
    path = c_filename.data;
    pathlen = (int)strlen(path);
    if (pathlen >= 5 &&
        (strcmp(path+pathlen-4, ".jpg") == 0 ||
         strcmp(path+pathlen-4, ".JPG") == 0 ||
         strcmp(path+pathlen-4, ".jpe") == 0 ||
         strcmp(path+pathlen-4, ".JPE") == 0 ||
         strcmp(path+pathlen-5, ".jpeg") == 0 ||
         strcmp(path+pathlen-5, ".JPEG") == 0))
        fx_status = stbi_write_jpg(path, width, height, channels, img->data, stride, jpeg_quality);
    else if(pathlen >= 4 &&
        (strcmp(path+pathlen-4, ".png") == 0 ||
         strcmp(path+pathlen-4, ".PNG") == 0))
        fx_status = stbi_write_png(path, width, height, channels, img->data, stride);
    else if(pathlen >= 4 &&
        (strcmp(path+pathlen-4, ".bmp") == 0 ||
         strcmp(path+pathlen-4, ".BMP") == 0))
        fx_status = stbi_write_bmp(path, width, height, channels, img->data, stride);
    else
        fx_status = -1;
    if (fx_status == 0)
        fx_status = FX_SET_EXN_FAST(FX_EXN_IOError);
    else if (fx_status < 0)
        fx_status = FX_SET_EXN_FAST(FX_EXN_BadArgError);
    return fx_status;
}

fun imwrite(filename: string, img: uint8 [,], ~jpeg_quality: int=90) =
    imwrite_(filename, img, 8, 1, jpeg_quality)
fun imwrite(filename: string, img: uint8x3 [,], ~jpeg_quality: int=90) =
    imwrite_(filename, (reinterpret(img): uint8 [,]), 8, 3, jpeg_quality)
fun imread_rgba(filename: string, img: uint8x4 [,], ~jpeg_quality: int=90) =
    imwrite_(filename, (reinterpret(img): uint8 [,]), 8, 4, jpeg_quality)
