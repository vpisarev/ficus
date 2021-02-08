/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// operations on individual characters

pure nothrow fun isalpha(c: char): bool = ccode { return fx_isalpha(c) }
pure nothrow fun isalnum(c: char): bool = ccode { return fx_isalnum(c) }
pure nothrow fun isdigit(c: char): bool = ccode { return fx_isdigit(c) }
pure nothrow fun isdecimal(c: char): bool = ccode { return fx_isdecimal(c) }
pure nothrow fun isspace(c: char): bool = ccode { return fx_isspace(c) }
pure nothrow fun tolower(c: char): char = ccode { return fx_tolower(c) }
pure nothrow fun toupper(c: char): char = ccode { return fx_toupper(c) }
