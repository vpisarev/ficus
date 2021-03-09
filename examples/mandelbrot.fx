import Sys
import File

val N = match Sys.arguments() {
    | n_str :: [] => n_str.to_int_or(16000)
    | _ => 16000
    }

val w = N, h = N, MAX_ITER = 50
val inv = 2.0 / w

val x_ = [@parallel for x <- 0:w {double(x) * inv - 1.5}]
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

val f: File.t = File.open("result.pgm", "wb")
try {
    f.print(f"P4\n{w} {h}\n")
    f.write(result)
} finally {
    f.close()
}
