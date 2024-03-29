/*
    This file is a part of ficus language project.
    See ficus/LICENSE.txt for the licensing terms
*/

#ifndef __FICUS_VERSION_H__
#define __FICUS_VERSION_H__

#define FX_VERSION_MAJOR 1
#define FX_VERSION_MINOR 0
#define FX_VERSION_PATCH 0
#define FX_VERSION_SUFFIX "-alpha"
#define FX_VER2STR(a) #a
#define FX_MAKE_VERSION(a, b, c, d) (FX_VER2STR(a) "." FX_VER2STR(b) "." FX_VER2STR(c) d)
#define FX_VERSION FX_MAKE_VERSION(FX_VERSION_MAJOR, FX_VERSION_MINOR, FX_VERSION_PATCH, FX_VERSION_SUFFIX)

#if __has_include("ficus/version.git_commit")
#include "ficus/version.git_commit"
#else
#define FX_GIT_COMMIT "<noinfo>"
#endif

#endif
