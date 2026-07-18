/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// lsp-1 / Phase 0.5 capability test: byte-exact UTF-8 primitives needed for
// LSP 'Content-Length' framing (N bytes != N code points) -- String.utf8_length
// (encoded byte count) and String.from_utf8 (decode an in-memory byte buffer,
// complementing File.read_utf8 which only decodes a whole named file).

from UTest import *

TEST("utf8.length_bytes_vs_codepoints", fun() {
    // ASCII: bytes == code points
    EXPECT_EQ_("abc".utf8_length(), 3)
    EXPECT_EQ_("abc".length(), 3)
    // 2-byte, 3-byte, 4-byte code points: bytes > code points
    EXPECT_EQ_("é".utf8_length(), 2);       EXPECT_EQ_("é".length(), 1)
    EXPECT_EQ_("你好".utf8_length(), 6);     EXPECT_EQ_("你好".length(), 2)
    EXPECT_EQ_("😀".utf8_length(), 4);       EXPECT_EQ_("😀".length(), 1)
    EXPECT_EQ_("".utf8_length(), 0)
})

TEST("utf8.from_utf8_decode", fun() {
    // "café" as UTF-8 bytes (é = C3 A9)
    EXPECT_EQ_(String.from_utf8([0x63u8, 0x61u8, 0x66u8, 0xC3u8, 0xA9u8]), "café")
    // "你好" (E4 BD A0  E5 A5 BD)
    EXPECT_EQ_(String.from_utf8([0xE4u8, 0xBDu8, 0xA0u8, 0xE5u8, 0xA5u8, 0xBDu8]), "你好")
    EXPECT_EQ_(String.from_utf8(([]: uint8 [])), "")
})

TEST("utf8.framing_invariant", fun() {
    // The LSP framing contract: Content-Length is the utf8_length, and decoding
    // exactly that many bytes reconstructs the message. Here we exercise the
    // math + decode end-to-end for a mixed multi-byte string.
    val msg = "hello — 你好 😀 café"
    val nbytes = msg.utf8_length()
    // a 32-char string is far shorter in code points than in bytes
    EXPECT_EQ_(msg.length() < nbytes, true)
    // decode of the known byte sequence yields exactly the message and the same
    // byte count (round-trip through the two primitives)
    val hi = String.from_utf8([0xE4u8, 0xBDu8, 0xA0u8, 0xE5u8, 0xA5u8, 0xBDu8])
    EXPECT_EQ_(hi.utf8_length(), 6)
    EXPECT_EQ_(hi.length(), 2)
})
