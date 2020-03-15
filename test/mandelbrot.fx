import Args
import File

val N = match (Args.arguments()) {
    | n_str :: [] => getOpt(atoi(n_str), 16000)
    | _ => 16000
    }

val w = N, h = N, MAX_ITER = 50
val inv = 2.0 / w

type vec8d = (double, double, double, double, double, double, double, double)
operator + (a: vec8d, b: vec8d) =
    (a.0+b.0, a.1+b.1, a.2+b.2, a.3+b.3, a.4+b.4, a.5+b.5, a.6+b.6, a.7+b.7)
operator - (a: vec8d, b: vec8d) =
    (a.0-b.0, a.1-b.1, a.2-b.2, a.3-b.3, a.4-b.4, a.5-b.5, a.6-b.6, a.7-b.7)
operator * (a: vec8d, b: vec8d) =
    (a.0*b.0, a.1*b.1, a.2*b.2, a.3*b.3, a.4*b.4, a.5*b.5, a.6*b.6, a.7*b.7)

val x_ = [parallel for (x in 0:w) (x :> double) * inv - 1.5]
val result: int8 [,] = [
    parallel for (y in 0:h) for (x8 in 0:(w/8))
    {
        val y_ = (y :> double) * inv - 1.0
        val x = x8*8
        val cr = (x_[x + 0],
                  x_[x + 1],
                  x_[x + 2],
                  x_[x + 3],
                  x_[x + 4],
                  x_[x + 5],
                  x_[x + 6],
                  x_[x + 7])
        val ci: vec8d = (y_, y_, y_, y_, y_, y_, y_, y_)
        var bits = 255, zr = cr, zi = ci

        for (iter in 0:MAX_ITER)
        {
            val rr = zr * zr
            val ii = zi * zi

            val mag = rr + ii
            if (mag.0 > 4.0) bits &= ~128
            if (mag.1 > 4.0) bits &= ~64
            if (mag.2 > 4.0) bits &= ~32
            if (mag.3 > 4.0) bits &= ~16
            if (mag.4 > 4.0) bits &= ~8
            if (mag.5 > 4.0) bits &= ~4
            if (mag.6 > 4.0) bits &= ~2
            if (mag.7 > 4.0) bits &= ~1

            val ir = zr * zi
            zr = (rr - ii) + cr
            zi = (ir + ir) + ci

            if (bits == 0) break
        }
        (bits :> int8)
    }
]

fun write_file()
{
    val f: File.file_t = File.open("result.pgm", "wb")
    File.print(f, "P4\n\(w) \(h)\n")
    File.write(f, result)
    File.close(f)
}

write_file()
0
