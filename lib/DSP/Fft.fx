/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// Split-radix FFT implementation,
// adopted from https://github.com/j-funk/corbanbrook-fft.
// Below is the original copyright and the license

/*
Copyright (c) 2010 Corban Brook
Copyright (c) 2017 Julien Funk

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

// lookup tables don't really gain us any speed, but they do increase
// cache footprint, so don't use them in here

// also we don't use separate arrays for real/imaginary parts

// this one a little more than twice as fast as the one in FFT
// however I only did the forward transform

// the rest of this was translated from C, see http://www.jjj.de/fxt/
// this is the real split radix FFT

@ccode {
static void fx_bitrev(float* X, int n)
{
    int n2 = n >> 1;
    for(int i = 1, r = 0; i < n2; i++) {
        r += n2;
        float t = X[r];
        X[r] = X[i];
        X[i] = t;
        i++;

        int h = n2;
        for(; ((r ^= h) & h) == 0; h >>=1)
            ;
        if (r >= i) {
            t = X[r];
            X[r] = X[i];
            X[i] = t;
            t = X[n-1-r];
            X[n-1-r] = X[n-1-i];
            X[n-1-i] = t;
        }
    }
}

// Ordering of output:
//
// trans[0]     = re[0] (==zero frequency, purely real)
// trans[1]     = re[1]
//             ...
// trans[n/2-1] = re[n/2-1]
// trans[n/2]   = re[n/2]    (==nyquist frequency, purely real)
//
// trans[n/2+1] = im[n/2-1]
// trans[n/2+2] = im[n/2-2]
//             ...
// trans[n-1]   = im[1]
//
// note before using RFFT.trans you need to scale it, however since [0-1]
// is frequently not the range you want, or you often want to work with it in
// some other way we leave it unscaled for speed (this way we make one fewer
// pass, if you're willing to remeber to scale it yourself.
static void fx_srfft_fwd(float* X, int n, float scale)
{
    const float pi2 = 3.1415926535897932384626433832795f*2;
    const float sqrt1_2 = 0.7071067811865476f;
    int i = n >> 1;
    int n2, n4, n8, nn;
    float t1, t2, t3, t4;
    int i0, i1, i2, i3, i4, i5, i6, i7, i8, ix, id;
    float st1, cc1, ss1, cc3, ss3, e, a;

    if (n == 1) {
        X[0] *= scale;
        return;
    }

    fx_bitrev(X, n);

    for (int ix = 0, id = 4; ix < n; id *= 4) {
        for (int i0 = ix; i0 < n; i0 += id) {
            //sumdiff(X[i0], X[i0+1]); // {a, b}  <--| {a+b, a-b}
            st1 = X[i0] - X[i0+1];
            X[i0] += X[i0+1];
            X[i0+1] = st1;
        }
        ix = 2*(id-1);
    }

    n2 = 2;
    nn = n >> 1;

    for( ; (nn >>= 1) != 0 ; ) {
        ix = 0;
        n2 = n2 << 1;
        id = n2 << 1;
        n4 = n2 >> 2;
        n8 = n2 >> 3;
        do {
            if(n4 != 1) {
                for(i0 = ix; i0 < n; i0 += id) {
                    i1 = i0;
                    i2 = i1 + n4;
                    i3 = i2 + n4;
                    i4 = i3 + n4;

                    //diffsum3_r(X[i3], X[i4], t1); // {a, b, s} <--| {a, b-a, a+b}
                    t1 = X[i3] + X[i4];
                    X[i4] -= X[i3];
                    //sumdiff3(X[i1], t1, X[i3]);   // {a, b, d} <--| {a+b, b, a-b}
                    X[i3] = X[i1] - t1;
                    X[i1] += t1;

                    i1 += n8;
                    i2 += n8;
                    i3 += n8;
                    i4 += n8;

                    //sumdiff(X[i3], X[i4], t1, t2); // {s, d}  <--| {a+b, a-b}
                    t1 = X[i3] + X[i4];
                    t2 = X[i3] - X[i4];

                    t1 = -t1 * sqrt1_2;
                    t2 *= sqrt1_2;

                    // sumdiff(t1, X[i2], X[i4], X[i3]); // {s, d}  <--| {a+b, a-b}
                    st1 = X[i2];
                    X[i4] = t1 + st1;
                    X[i3] = t1 - st1;

                    //sumdiff3(X[i1], t2, X[i2]); // {a, b, d} <--| {a+b, b, a-b}
                    X[i2] = X[i1] - t2;
                    X[i1] += t2;
                }
            } else {
                for(i0 = ix; i0 < n; i0 += id) {
                    i1 = i0;
                    i2 = i1 + n4;
                    i3 = i2 + n4;
                    i4 = i3 + n4;

                    //diffsum3_r(X[i3], X[i4], t1); // {a, b, s} <--| {a, b-a, a+b}
                    t1 = X[i3] + X[i4];
                    X[i4] -= X[i3];

                    //sumdiff3(X[i1], t1, X[i3]);   // {a, b, d} <--| {a+b, b, a-b}
                    X[i3] = X[i1] - t1;
                    X[i1] += t1;
                }
            }

            ix = (id << 1) - n2;
            id = id << 2;
        } while (ix < n);

        e = pi2 / n2;

        for (int j = 1; j < n8; j++) {
            a = j * e;
            ss1 = sinf(a);
            cc1 = cosf(a);

            //ss3 = sin(3*a); cc3 = cos(3*a);
            cc3 = 4*cc1*(cc1*cc1-0.75f);
            ss3 = 4*ss1*(0.75f-ss1*ss1);

            ix = 0; id = n2 << 1;
            do {
                for (i0 = ix; i0 < n; i0 += id) {
                    i1 = i0 + j;
                    i2 = i1 + n4;
                    i3 = i2 + n4;
                    i4 = i3 + n4;

                    i5 = i0 + n4 - j;
                    i6 = i5 + n4;
                    i7 = i6 + n4;
                    i8 = i7 + n4;

                    //cmult(c, s, X, y, &u, &v)
                    //cmult(cc1, ss1, X[i7], X[i3], t2, t1); // {u,v} <--| {X*c-y*s, X*s+y*c}
                    t2 = X[i7]*cc1 - X[i3]*ss1;
                    t1 = X[i7]*ss1 + X[i3]*cc1;

                    //cmult(cc3, ss3, X[i8], X[i4], t4, t3);
                    t4 = X[i8]*cc3 - X[i4]*ss3;
                    t3 = X[i8]*ss3 + X[i4]*cc3;

                    //sumdiff(t2, t4);   // {a, b} <--| {a+b, a-b}
                    st1 = t2 - t4;
                    t2 += t4;
                    t4 = st1;

                    //sumdiff(t2, X[i6], X[i8], X[i3]); // {s, d}  <--| {a+b, a-b}
                    //st1 = X[i6]; X[i8] = t2 + st1; X[i3] = t2 - st1;
                    X[i8] = t2 + X[i6];
                    X[i3] = t2 - X[i6];

                    //sumdiff_r(t1, t3); // {a, b} <--| {a+b, b-a}
                    st1 = t3 - t1;
                    t1 += t3;
                    t3 = st1;

                    //sumdiff(t3, X[i2], X[i4], X[i7]); // {s, d}  <--| {a+b, a-b}
                    //st1 = X[i2]; X[i4] = t3 + st1; X[i7] = t3 - st1;
                    X[i4] = t3 + X[i2];
                    X[i7] = t3 - X[i2];

                    //sumdiff3(X[i1], t1, X[i6]);   // {a, b, d} <--| {a+b, b, a-b}
                    X[i6] = X[i1] - t1;
                    X[i1] += t1;

                    //diffsum3_r(t4, X[i5], X[i2]); // {a, b, s} <--| {a, b-a, a+b}
                    X[i2] = t4 + X[i5];
                    X[i5] -= t4;
                }

                ix = (id << 1) - n2;
                id = id << 2;
            } while (ix < n);
        }
    }
    if(scale != 1.f) {
        for(i = 0; i < n; i++)
            X[i] *= scale;
    }
}

static void fx_srfft_inv(float* X, int n, float scale)
{
    const float pi2 = 3.1415926535897932384626433832795f*2;
    const float sqrt2 = 1.41421356237309504880f;
    int n2, n4, n8, nn;
    float t1, t2, t3, t4, t5;
    int j, i, i0, i1, i2, i3, i4, i5, i6, i7, i8, ud, ix, id;
    float st1, cc1, ss1, cc3, ss3, e, a;

    if (n == 1) {
        X[0] *= scale;
        return;
    }

    nn = n>>1;
    n2 = n<<1;

    for( ; (nn >>= 1) != 0; )
    {
        ix = 0;
        id = n2;
        n2 >>= 1;
        n4 = n2>>2;
        n8 = n4>>1;

        do  // ix
        {
            for (i0=ix; i0<n; i0+=id)
            {
                i1 = i0;
                i2 = i1 + n4;
                i3 = i2 + n4;
                i4 = i3 + n4;

                //sumdiff3(X[i1], X[i3], t1);// {a, b, d} <--| {a+b, b, a-b}
                t1 = X[i1] - X[i3]; X[i1] += X[i3];

                X[i2] += X[i2];
                X[i4] += X[i4];

                //sumdiff3_r(X[i4], t1, X[i3]);// {a,b,d} <--| {a+b, b, b-a}
                X[i3] = t1 - X[i4]; X[i4] += t1;

                if ( n4!=1 )  // note: optimise (Note this comment from original C++)
                {
                    i1 += n8;
                    i2 += n8;
                    i3 += n8;
                    i4 += n8;

                    //sumdiff3(X[i1], X[i2], t1); // {a, b, d} <--| {a+b, b, a-b}
                    t1 = X[i1] - X[i2]; X[i1] += X[i2];

                    //sumdiff(a, b, &s, &d) {s, d}  <--| {a+b, a-b}
                    //sumdiff(X[i4], X[i3], t2, X[i2]);
                    t2 = X[i4] + X[i3]; X[i2] = X[i4] - X[i3];

                    t2 = -t2 * sqrt2;
                    t1 *= sqrt2;
                    //sumdiff(a, b, &s, &d) {s, d}  <--| {a+b, a-b}
                    //sumdiff(t2, t1, X[i3], X[i4]);
                    X[i3] = t2 + t1; X[i4] = t2 - t1;
                }
            }

            ix = (id<<1) - n2;
            id <<= 2;
        } while ( ix<n );

        e = pi2/n2;
        for (j=1; j<n8; j++)
        {
            a = j*e;

            ss1 = sinf(a);
            cc1 = cosf(a);

            ss3 = sinf(3*a); cc3 = cosf(3*a);
            cc3 = 4*cc1*(cc1*cc1-0.75);
            ss3 = 4*ss1*(0.75-ss1*ss1);

            ix = 0;
            id = n2<<1;
            do  // ix-loop
            {
                for (i0=ix; i0<n; i0+=id)
                {
                    i1 = i0 + j;
                    i2 = i1 + n4;
                    i3 = i2 + n4;
                    i4 = i3 + n4;

                    i5 = i0 + n4 - j;
                    i6 = i5 + n4;
                    i7 = i6 + n4;
                    i8 = i7 + n4;

                    //sumdiff3(X[i1], X[i6], t1); // {a, b, d} <--| {a+b, b, a-b}
                    t1 = X[i1] - X[i6]; X[i1] += X[i6];
                    //sumdiff3(X[i5], X[i2], t2); // {a, b, d} <--| {a+b, b, a-b}
                    t2 = X[i5] - X[i2]; X[i5] += X[i2];
                    //t2 = X[i5] + X[i2]; X[i5] = X[i5] - X[i2];

                    //sumdiff(a, b, &s, &d) {s, d}  <--| {a+b, a-b}
                    //sumdiff(X[i8], X[i3], t3, X[i6]);
                    //sumdiff(X[i4], X[i7], t4, X[i2]);
                    t3 = X[i8] + X[i3]; X[i6] = X[i8] - X[i3];
                    t4 = X[i4] + X[i7]; X[i2] = X[i4] - X[i7];

                    //sumdiff3(t1, t4, t5); // {a, b, d} <--| {a+b, b, a-b}
                    t5 = t1 - t4; t1 += t4;
                    //sumdiff3(t2, t3, t4); // {a, b, d} <--| {a+b, b, a-b}
                    t4 = t2 - t3; t2 += t3;

                    //cmult(c, s, X, y, &u, &v) {u,v} <--| {X*c-y*s, X*s+y*c}
                    //cmult(ss1, cc1, t5, t4, X[i7], X[i3]);
                    //cmult(cc3, ss3, t1, t2, X[i4], X[i8]);
                    X[i7] = t5*ss1 - t4*cc1; X[i3] = t5*cc1 + t4*ss1;
                    X[i4] = t1*cc3 - t2*ss3; X[i8] = t1*ss3 + t2*cc3;
                }

                ix = (id<<1) - n2;
                id <<= 2;
            } while ( ix < n );
        }
    }

    for (ix=0, id=4; ix<n; id*=4)
    {
        for (i0=ix; i0<n; i0+=id) {
            // sumdiff(&a, &b) {a, b}  <--| {a+b, a-b}
            //sumdiff(X[i0], X[i0+1]);
            st1 = X[i0] - X[i0+1]; X[i0] += X[i0+1]; X[i0+1] = st1;
        }
        ix = 2*(id-1);
    }

    fx_bitrev(X, n);
    if(scale != 1.f) {
        for(i = 0; i < n; i++)
            X[i] *= scale;
    }
}
}

@private fun srfft_real(src: float [], inv: bool, scale: float): float []
@ccode {
    int_ n = src->dim[0].size;
    int fx_status;
    if (n == 0 || (n & (n-1)) != 0)
        return FX_SET_EXN_FAST(FX_EXN_SizeError);
    fx_status = fx_make_arr(1, &n, src->dim[0].step, 0, 0, src->data, fx_result);
    if (fx_status >= 0) {
        if (inv)
            fx_srfft_inv((float*)fx_result->data, (int)n, scale);
        else
            fx_srfft_fwd((float*)fx_result->data, (int)n, scale);
    }
    return fx_status;
}

fun fwd(src: float [], ~scale: float=1.f): float [] = srfft_real(src, false, scale)
fun inv(src: float [], ~scale: float=1.f): float [] = srfft_real(src, true, scale)
