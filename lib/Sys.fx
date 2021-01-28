/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// Various system services

pure nothrow fun getTickCount(): int64 = ccode { return fx_tickcount() }
pure nothrow fun getTickFrequency(): double = ccode { return fx_tickfreq() }
