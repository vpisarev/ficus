/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// various element-wise operations
import Ast

@private fun run_cast(inp: 'inpT [], out: 'outT []) = for x@idx <- inp {out[idx] = (x :> 'outT)}
@private fun run_cast(inp: float [], out: uint8 []) = for x@idx <- inp {out[idx] = sat_uint8(x)}
@private fun run_cast(inp: float [], out: int8 []) = for x@idx <- inp {out[idx] = sat_int8(x)}
@private fun run_cast(inp: float [], out: int16 []) = for x@idx <- inp {out[idx] = sat_int16(x)}
@private fun run_cast(inp: float [], out: int32 []) = for x@idx <- inp {out[idx] = int32(round(x))}

fun run_cast(model: Ast.nnmodel_t, op: Ast.nnop_t) =
match op {
| Ast.NN_Cast {t_inp, t_out} =>
    val inp = model.get_tensor(t_inp)
    val out = model.get_tensor(t_out)
    match (inp.data, out.data) {
    | (Ast.NN_Data_Empty, _) => {}
    | (_, Ast.NN_Data_Empty) => {}
    | (Ast.NN_Data_I8 inp_data, Ast.NN_Data_U8 out_data) => run_cast(inp_data, out_data)
    | (Ast.NN_Data_I16 inp_data, Ast.NN_Data_U8 out_data) => run_cast(inp_data, out_data)
    | (Ast.NN_Data_I32 inp_data, Ast.NN_Data_U8 out_data) => run_cast(inp_data, out_data)
    | (Ast.NN_Data_I64 inp_data, Ast.NN_Data_U8 out_data) => run_cast(inp_data, out_data)
    | (Ast.NN_Data_FP32 inp_data, Ast.NN_Data_U8 out_data) => run_cast(inp_data, out_data)
    | (Ast.NN_Data_Bool inp_data, Ast.NN_Data_U8 out_data) => run_cast(inp_data, out_data)

    | (Ast.NN_Data_U8 inp_data, Ast.NN_Data_I8 out_data) => run_cast(inp_data, out_data)
    | (Ast.NN_Data_I16 inp_data, Ast.NN_Data_I8 out_data) => run_cast(inp_data, out_data)
    | (Ast.NN_Data_I32 inp_data, Ast.NN_Data_I8 out_data) => run_cast(inp_data, out_data)
    | (Ast.NN_Data_I64 inp_data, Ast.NN_Data_I8 out_data) => run_cast(inp_data, out_data)
    | (Ast.NN_Data_FP32 inp_data, Ast.NN_Data_I8 out_data) => run_cast(inp_data, out_data)
    | (Ast.NN_Data_Bool inp_data, Ast.NN_Data_I8 out_data) => run_cast(inp_data, out_data)

    | (Ast.NN_Data_I8 inp_data, Ast.NN_Data_I32 out_data) => run_cast(inp_data, out_data)
    | (Ast.NN_Data_U8 inp_data, Ast.NN_Data_I32 out_data) => run_cast(inp_data, out_data)
    | (Ast.NN_Data_I16 inp_data, Ast.NN_Data_I32 out_data) => run_cast(inp_data, out_data)
    | (Ast.NN_Data_I64 inp_data, Ast.NN_Data_I32 out_data) => run_cast(inp_data, out_data)
    | (Ast.NN_Data_FP32 inp_data, Ast.NN_Data_I32 out_data) => run_cast(inp_data, out_data)
    | (Ast.NN_Data_Bool inp_data, Ast.NN_Data_I32 out_data) => run_cast(inp_data, out_data)

    | (Ast.NN_Data_I8 inp_data, Ast.NN_Data_I64 out_data) => run_cast(inp_data, out_data)
    | (Ast.NN_Data_U8 inp_data, Ast.NN_Data_I64 out_data) => run_cast(inp_data, out_data)
    | (Ast.NN_Data_I16 inp_data, Ast.NN_Data_I64 out_data) => run_cast(inp_data, out_data)
    | (Ast.NN_Data_I32 inp_data, Ast.NN_Data_I64 out_data) => run_cast(inp_data, out_data)
    | (Ast.NN_Data_FP32 inp_data, Ast.NN_Data_I64 out_data) => run_cast(inp_data, out_data)
    | (Ast.NN_Data_Bool inp_data, Ast.NN_Data_I64 out_data) => run_cast(inp_data, out_data)

    | (Ast.NN_Data_I8 inp_data, Ast.NN_Data_FP32 out_data) => run_cast(inp_data, out_data)
    | (Ast.NN_Data_U8 inp_data, Ast.NN_Data_FP32 out_data) => run_cast(inp_data, out_data)
    | (Ast.NN_Data_I16 inp_data, Ast.NN_Data_FP32 out_data) => run_cast(inp_data, out_data)
    | (Ast.NN_Data_I32 inp_data, Ast.NN_Data_FP32 out_data) => run_cast(inp_data, out_data)
    | (Ast.NN_Data_I64 inp_data, Ast.NN_Data_FP32 out_data) => run_cast(inp_data, out_data)
    | (Ast.NN_Data_Bool inp_data, Ast.NN_Data_FP32 out_data) => run_cast(inp_data, out_data)

    | (Ast.NN_Data_I8 inp_data, Ast.NN_Data_Bool out_data) => run_cast(inp_data, out_data)
    | (Ast.NN_Data_U8 inp_data, Ast.NN_Data_Bool out_data) => run_cast(inp_data, out_data)
    | (Ast.NN_Data_I16 inp_data, Ast.NN_Data_Bool out_data) => run_cast(inp_data, out_data)
    | (Ast.NN_Data_I32 inp_data, Ast.NN_Data_Bool out_data) => run_cast(inp_data, out_data)
    | (Ast.NN_Data_I64 inp_data, Ast.NN_Data_Bool out_data) => run_cast(inp_data, out_data)
    | (Ast.NN_Data_FP32 inp_data, Ast.NN_Data_Bool out_data) => run_cast(inp_data, out_data)
    | _ =>
        throw Ast.NNError(f"cast from {inp.data.elemtype()} to {out.data.elemtype()} is not implemented")
    }
| _ => throw Ast.NNError(f"unexpected op {op.name()}")
}

@private fun run_clip(inp: 't [], minval: float, maxval: float, out: 't [])
{
    val minval = (max(minval, float(__min__(0:>'t))) :> 't)
    val maxval = (min(maxval, float(__max__(0:>'t))) :> 't)
    for x@idx <- inp {out[idx] = min(max(x, minval), maxval)}
}

fun run_clip(model: Ast.nnmodel_t, op: Ast.nnop_t) =
match op
{
| Ast.NN_Clip {t_inp, t_min, t_max, t_out} =>
    val inp = model.get_tensor(t_inp)
    val out = model.get_tensor(t_out)
    val minval = model.get_tensor(t_min)
    val maxval = model.get_tensor(t_max)
    val minval = minval.data.float_scalar_or(-FLT_MAX)
    val maxval = maxval.data.float_scalar_or(FLT_MAX)
    match (inp.data, out.data) {
    | (Ast.NN_Data_U8 inp_data, Ast.NN_Data_U8 out_data) =>
        run_clip(inp_data, minval, maxval, out_data)
    | (Ast.NN_Data_I8 inp_data, Ast.NN_Data_I8 out_data) =>
        run_clip(inp_data, minval, maxval, out_data)
    | (Ast.NN_Data_I32 inp_data, Ast.NN_Data_I32 out_data) =>
        run_clip(inp_data, minval, maxval, out_data)
    | (Ast.NN_Data_FP32 inp_data, Ast.NN_Data_FP32 out_data) =>
        run_clip(inp_data, minval, maxval, out_data)
    | _ => throw NotImplementedError
    }
| _ => throw Ast.NNError(f"unexpected op {op.name()}")
}

@private fun run_constantOfShape(v: 't, out: 't []) = for _@idx <- out {out[idx] = v}

fun run_constantOfShape(model: Ast.nnmodel_t, op: Ast.nnop_t) =
match op {
| Ast.NN_ConstantOfShape {value, t_out} =>
    val out = model.get_tensor(t_out)
    match (value.data, out.data) {
    | (Ast.NN_Data_U8 v_data, Ast.NN_Data_U8 out_data) =>
        OpElemwise.run_constantOfShape(v_data[0], out_data)
    | (Ast.NN_Data_I8 v_data, Ast.NN_Data_I8 out_data) =>
        OpElemwise.run_constantOfShape(v_data[0], out_data)
    | (Ast.NN_Data_I32 v_data, Ast.NN_Data_I32 out_data) =>
        OpElemwise.run_constantOfShape(v_data[0], out_data)
    | (Ast.NN_Data_FP32 v_data, Ast.NN_Data_FP32 out_data) =>
        OpElemwise.run_constantOfShape(v_data[0], out_data)
    | _ => throw NotImplementedError
    }
| _ => throw Ast.NNError(f"unexpected op {op.name()}")
}

@private fun run_abs(inp: 't [], out: 't []) = for x@idx <- inp {out[idx] = abs(x)}
@private fun run_acos(inp: 't [], out: 't []) = for x@idx <- inp {out[idx] = acos(x)}
@private fun run_asin(inp: 't [], out: 't []) = for x@idx <- inp {out[idx] = asin(x)}
@private fun run_atan(inp: 't [], out: 't []) = for x@idx <- inp {out[idx] = atan(x)}
@private fun run_cos(inp: 't [], out: 't []) = for x@idx <- inp {out[idx] = cos(x)}
@private fun run_exp(inp: 't [], out: 't []) = for x@idx <- inp {out[idx] = exp(x)}
@private fun run_log(inp: 't [], out: 't []) = for x@idx <- inp {out[idx] = log(x)}
@private fun run_relu(inp: 't [], out: 't []) = for x@idx <- inp {out[idx] = max(x, (0:>'t))}
@private fun run_sigmoid(inp: 't [], out: 't []) = for x@idx <- inp {out[idx] = 1 / (1 + exp(-x))}
@private fun run_sign(inp: 't [], out: 't []) = for x@idx <- inp {out[idx] = (sign(x) :> 't)}
@private fun run_sin(inp: 't [], out: 't []) = for x@idx <- inp {out[idx] = sin(x)}
@private fun run_softplus(inp: 't [], out: 't []) = for x@idx <- inp {out[idx] = log(1 + exp(x))}
@private fun run_mish(inp: 't [], out: 't []) = for x@idx <- inp {
    val x = if x > -36.73f {x} else {0.f}
    val y = exp(-x)
    out[idx] = x*(1 + 2*y)/(1 + 2*y + 2*y*y)
}
@private fun run_softsign(inp: 't [], out: 't []) = for x@idx <- inp {out[idx] = x/(1 + abs(x))}
@private fun run_sqrt(inp: 't [], out: 't []) = for x@idx <- inp {out[idx] = sqrt(x)}
@private fun run_tanh(inp: 't [], out: 't []) = for x@idx <- inp {out[idx] = tanh(x)}
@private fun run_leaky_relu(inp: 't [], alpha: float, out: 't []) = for x@idx <- inp {out[idx] = if x >= 0.f {x} else {(x*alpha :> 't)}}

fun run_unary(model: Ast.nnmodel_t, op: Ast.nnop_t) =
match op
{
| Ast.NN_Elemwise {el_op, t_inp, t_out} when t_inp.size() == 1 =>
    val inp = model.get_tensor(t_inp[0])
    val out = model.get_tensor(t_out)
    match (el_op, inp.data, out.data) {
    | (Ast.NN_Abs, Ast.NN_Data_FP32 inp_data, Ast.NN_Data_FP32 out_data) =>
        run_abs(inp_data, out_data)
    | (Ast.NN_Acos, Ast.NN_Data_FP32 inp_data, Ast.NN_Data_FP32 out_data) =>
        run_acos(inp_data, out_data)
    | (Ast.NN_Asin, Ast.NN_Data_FP32 inp_data, Ast.NN_Data_FP32 out_data) =>
        run_asin(inp_data, out_data)
    | (Ast.NN_Atan, Ast.NN_Data_FP32 inp_data, Ast.NN_Data_FP32 out_data) =>
        run_atan(inp_data, out_data)
    | (Ast.NN_Cos, Ast.NN_Data_FP32 inp_data, Ast.NN_Data_FP32 out_data) =>
        run_cos(inp_data, out_data)
    | (Ast.NN_Exp, Ast.NN_Data_FP32 inp_data, Ast.NN_Data_FP32 out_data) =>
        run_exp(inp_data, out_data)
    | (Ast.NN_Log, Ast.NN_Data_FP32 inp_data, Ast.NN_Data_FP32 out_data) =>
        run_log(inp_data, out_data)
    | (Ast.NN_Mish, Ast.NN_Data_FP32 inp_data, Ast.NN_Data_FP32 out_data) =>
        run_mish(inp_data, out_data)
    | (Ast.NN_Relu, Ast.NN_Data_FP32 inp_data, Ast.NN_Data_FP32 out_data) =>
        run_relu(inp_data, out_data)
    | (Ast.NN_Sigmoid, Ast.NN_Data_FP32 inp_data, Ast.NN_Data_FP32 out_data) =>
        run_sigmoid(inp_data, out_data)
    | (Ast.NN_Sign, Ast.NN_Data_FP32 inp_data, Ast.NN_Data_FP32 out_data) =>
        run_sign(inp_data, out_data)
    | (Ast.NN_Sin, Ast.NN_Data_FP32 inp_data, Ast.NN_Data_FP32 out_data) =>
        run_sin(inp_data, out_data)
    | (Ast.NN_Softplus, Ast.NN_Data_FP32 inp_data, Ast.NN_Data_FP32 out_data) =>
        run_softplus(inp_data, out_data)
    | (Ast.NN_Softsign, Ast.NN_Data_FP32 inp_data, Ast.NN_Data_FP32 out_data) =>
        run_softsign(inp_data, out_data)
    | (Ast.NN_Sqrt, Ast.NN_Data_FP32 inp_data, Ast.NN_Data_FP32 out_data) =>
        run_sqrt(inp_data, out_data)
    | (Ast.NN_Tanh, Ast.NN_Data_FP32 inp_data, Ast.NN_Data_FP32 out_data) =>
        run_tanh(inp_data, out_data)
    | _ => throw NotImplementedError
    }
| _ => throw Ast.NNError(f"unexpected op {op.name()}")
}

fun run_leaky_relu(model: Ast.nnmodel_t, op: Ast.nnop_t) =
match op
{
| Ast.NN_LeakyRelu {alpha, t_inp, t_out} =>
    val inp = model.get_tensor(t_inp)
    val out = model.get_tensor(t_out)
    match (inp.data, out.data) {
    | (Ast.NN_Data_FP32 inp_data, Ast.NN_Data_FP32 out_data) =>
        run_leaky_relu(inp_data, alpha, out_data)
    | _ => throw NotImplementedError
    }
| _ => throw Ast.NNError(f"unexpected op {op.name()}")
}


@ccode {
#include "ficus_nn_common.h"

/*
    Prepare the strides for the efficient BROADCAST_MAX_DIMS-dimensional operation.
    The algorithm includes the following steps:

    1. Make both input array and the output array BROADCAST_MAX_DIMS-dimensional.
       If necessary, prepend the shapes with 1's and compute the corresponding strides.
       This makes the actual operation much more straight-forward,
       we just have to deal with fixed dimensionality using fixed number of nested loops.
    2. For some i one of the inputs may have i-th dimension == 1,
       whereas the other input may have the same dimension > 1.
       We need to handle it by zero'ing the corresponding i-th step.

       E.g. shape1[2] == 1, shape2[2] == 100 (and so shape[2] == 100).

       When we will iterate through this dimension within a nested loop and access elements

       for (int i0 = 0; i0 < shape[0]; i0++)
          for (int i1 = 0; i1 < shape[1]; i++) {
             for (int i2 = 0; i2 < shape[2]; i2++) {
                ...
                input1.ptr<float>[i0*step1[0] + i1*step1[1] + i2*step1[2] + ...]
             }

       we need to take into account that shape1[2] == 1 and set step1[2]=0.
    3. Often the inputs are contiguous (the output is assumed to be contiguous),
       so we can try to flatten/reshape inputs in order to increase the length of inner loops and
       correspondingly shorten the outer loops so that the loop overhead is reduced.
       We do flattening within in a loop with descending j, starting from j=BROADCAST_MAX_DIMS-2:
       3a. we check that for some stepi[j] = stepi[j+1]*shapei[j+1] for all i: i=0,1,2 (i=0 means the output tensor)
       3b. we also check that for each tensor we stay in scalar mode or stay in non-scalar mode
       3c. we also check if shapei[j] == 1 for all i.
       3d. if yes for (3a && (3b || 3c)), we do shapei[j+1] *= shapei[j] and eliminate j-th dimension.
       3e. otherwise, we leave everything as is, decrease j and proceed.
       3f. in the end of the loop we append the proper number of 1's
           to the final shape to keep it BROADCAST_MAX_DIMS-dimensional.
*/
static bool _fx_prepare_for_broadcast_op(
    int narrays, int max_ndims,
    const int* ndims, const int_** shape_,
    int_** shape, size_t** step)
{
    int i, j, k;

    // step 1.
    // * make all inputs and the output max_ndims-dimensional.
    // * compute proper step's
    for (i = max_ndims-1; i >= 0; i-- ) {
        for (k = 0; k < narrays; k++) {
            j = ndims[k] - (max_ndims - i);
            int sz_i = j >= 0 ? shape_[k][j] : 1;
            size_t st_i = i == max_ndims-1 ? 1 : step[k][i+1]*shape[k][i+1];
            shape[k][i] = sz_i;
            step[k][i] = st_i;
            if (shape[k][i] == 0)
                return false;
        }
    }

    // step 3. Let's do the flattening first,
    // since we'd need proper values of steps to check continuity.
    // this loop is probably the most tricky part
    // in the whole implementation of broadcasting.
    j = max_ndims-1;
    for (i = j - 1; i >= 0; i--) {
        bool all_contiguous = true, all_scalars = true, all_consistent = true;
        for(k = 0; k < narrays; k++) {
            size_t st = step[k][j]*shape[k][j];
            bool prev_scalar = shape[k][j] == 1;
            bool scalar = shape[k][i] == 1;
            all_contiguous = all_contiguous && (st == step[k][i]);
            all_scalars = all_scalars && scalar;
            all_consistent = all_consistent && (scalar == prev_scalar);
        }
        if (all_contiguous && (all_consistent || all_scalars)) {
            for(k = 0; k < narrays; k++)
                shape[k][j] *= shape[k][i];
        } else {
            j--;
            if (i < j) {
                for(k = 0; k < narrays; k++) {
                    shape[k][j] = shape[k][i];
                    step[k][j] = step[k][i];
                }
            }
        }
    }

    // step 2. Set some step's to 0's.
    for (i = max_ndims-1; i >= j; i--) {
        for (k = 0; k < narrays; k++)
            step[k][i] = shape[k][i] == 1 ? 0 : step[k][i];
    }
    for (; i >= 0; i--) {
        for (k = 0; k < narrays; k++) {
            step[k][i] = 0;
            shape[k][i] = 1;
        }
    }
    return true;
}

enum {
_FX_NN_Add=1,
_FX_NN_And,
_FX_NN_Div,
_FX_NN_Equal,
_FX_NN_Greater,
_FX_NN_GreaterOrEqual,
_FX_NN_Less,
_FX_NN_LessOrEqual,
_FX_NN_Mod,
_FX_NN_Mul,
_FX_NN_Pow,
_FX_NN_Or,
_FX_NN_Sub,
_FX_NN_Xor,
_FX_NN_Min,
_FX_NN_Max,
_FX_NN_Mean
};

enum {_FX_ELEMWISE_MAX_DIMS=5};

#define _FX_OP_ADD(x, y) ((x) + (y))
#define _FX_OP_SUB(x, y) ((x) - (y))
#define _FX_OP_MUL(x, y) ((x) * (y))
#define _FX_OP_DIV(x, y) ((x) / (y))
#define _FX_OP_MOD(x, y) ((x) % (y))
#define _FX_OP_MIN(x, y) ((x) <= (y) ? (x) : (y))
#define _FX_OP_MAX(x, y) ((x) >= (y) ? (x) : (y))
#define _FX_OP_MEAN(x, y) (((x) + (y))*param)
#define _FX_OP_AND(x, y) ((x) & (y))
#define _FX_OP_OR(x, y) ((x) | (y))
#define _FX_OP_XOR(x, y) ((x) ^ (y))
#define _FX_OP_CMP_GT(x, y) ((x) > (y))
#define _FX_OP_CMP_GE(x, y) ((x) >= (y))
#define _FX_OP_CMP_EQ(x, y) ((x) == (y))

#undef _FX_IMPLEMENT_BINARY_OP
#define _FX_IMPLEMENT_BINARY_OP(suffix, _Tp1, _Tp2, _Tp, do_op) \
static void _fx_elemwise_##suffix(const char* data1, size_t rowstep1, size_t dp1, \
                                const char* data2, size_t rowstep2, size_t dp2, \
                                char* data, size_t rowstep, size_t dp, \
                                int_ nrows, int_ ncols, double param) \
{ \
    param; \
    for (int_ i = 0; i < nrows; i++) { \
        const _Tp1* ptr1 = (const _Tp1*)data1 + rowstep1*i; \
        const _Tp2* ptr2 = (const _Tp2*)data2 + rowstep2*i; \
        _Tp* ptr = (_Tp*)data + rowstep*i; \
        if (dp1 == 1 && dp2 == 1 && dp == 1) { \
            for(int_ j = 0; j < ncols; j++) { \
                _Tp1 x1 = ptr1[j]; _Tp2 x2 = ptr2[j]; \
                ptr[j] = (_Tp)do_op(x1, x2); \
            } \
        } else if (dp1 == 1 && dp2 == 0 && dp == 1) { \
            _Tp2 x2 = *ptr2; \
            for(int_ j = 0; j < ncols; j++) { \
                _Tp1 x1 = ptr1[j]; \
                ptr[j] = (_Tp)do_op(x1, x2); \
            } \
        } else if (dp1 == 0 && dp2 == 1 && dp == 1) { \
            _Tp1 x1 = *ptr1; \
            for(int_ j = 0; j < ncols; j++) { \
                _Tp2 x2 = ptr2[j]; \
                ptr[j] = (_Tp)do_op(x1, x2); \
            } \
        } else { \
            for(int_ j = 0; j < ncols; j++, ptr1 += dp1, ptr2 += dp2, ptr += dp) { \
                _Tp1 x1 = *ptr1; _Tp2 x2 = *ptr2; \
                *ptr = (_Tp)do_op(x1, x2); \
            } \
        } \
    } \
}

typedef void (*_fx_elemwise_binary_func_t)(
    const char* data1, size_t rowstep1, size_t dp1,
    const char* data2, size_t rowstep2, size_t dp2,
    char* data, size_t rowstep, size_t dp,
    int_ nrows, int_ ncols, double param);

_FX_IMPLEMENT_BINARY_OP(add_f32, float, float, float, _FX_OP_ADD)
_FX_IMPLEMENT_BINARY_OP(add_i32, int, int, int, _FX_OP_ADD)
_FX_IMPLEMENT_BINARY_OP(add_i64, int64_t, int64_t, int64_t, _FX_OP_ADD)

_FX_IMPLEMENT_BINARY_OP(sub_f32, float, float, float, _FX_OP_SUB)
_FX_IMPLEMENT_BINARY_OP(sub_i32, int, int, int, _FX_OP_SUB)
_FX_IMPLEMENT_BINARY_OP(sub_i64, int64_t, int64_t, int64_t, _FX_OP_SUB)

_FX_IMPLEMENT_BINARY_OP(mul_f32, float, float, float, _FX_OP_MUL)
_FX_IMPLEMENT_BINARY_OP(mul_i32, int, int, int, _FX_OP_MUL)
_FX_IMPLEMENT_BINARY_OP(mul_i64, int64_t, int64_t, int64_t, _FX_OP_MUL)

_FX_IMPLEMENT_BINARY_OP(div_f32, float, float, float, _FX_OP_DIV)
_FX_IMPLEMENT_BINARY_OP(div_i32, int, int, int, _FX_OP_DIV)
_FX_IMPLEMENT_BINARY_OP(div_i64, int64_t, int64_t, int64_t, _FX_OP_DIV)

_FX_IMPLEMENT_BINARY_OP(mod_f32, float, float, float, fmodf)
_FX_IMPLEMENT_BINARY_OP(mod_i32, int, int, int, _FX_OP_MOD)
_FX_IMPLEMENT_BINARY_OP(mod_i64, int64_t, int64_t, int64_t, _FX_OP_MOD)

_FX_IMPLEMENT_BINARY_OP(pow_f32, float, float, float, powf)
_FX_IMPLEMENT_BINARY_OP(pow_i32, int, int, int, pow)
_FX_IMPLEMENT_BINARY_OP(pow_i64, int64_t, int64_t, int64_t, pow)

_FX_IMPLEMENT_BINARY_OP(min_f32, float, float, float, _FX_OP_MIN)
_FX_IMPLEMENT_BINARY_OP(min_i32, int, int, int, _FX_OP_MIN)
_FX_IMPLEMENT_BINARY_OP(min_i64, int64_t, int64_t, int64_t, _FX_OP_MIN)

_FX_IMPLEMENT_BINARY_OP(max_f32, float, float, float, _FX_OP_MAX)
_FX_IMPLEMENT_BINARY_OP(max_i32, int, int, int, _FX_OP_MAX)
_FX_IMPLEMENT_BINARY_OP(max_i64, int64_t, int64_t, int64_t, _FX_OP_MAX)

_FX_IMPLEMENT_BINARY_OP(mean_f32, float, float, float, _FX_OP_MEAN)
_FX_IMPLEMENT_BINARY_OP(mean_i32, int, int, int, _FX_OP_MEAN)
_FX_IMPLEMENT_BINARY_OP(mean_i64, int64_t, int64_t, int64_t, _FX_OP_MEAN)

_FX_IMPLEMENT_BINARY_OP(cmp_eq_f32, float, float, bool, _FX_OP_CMP_EQ)
_FX_IMPLEMENT_BINARY_OP(cmp_eq_i32, int, int, bool, _FX_OP_CMP_EQ)
_FX_IMPLEMENT_BINARY_OP(cmp_eq_i64, int64_t, int64_t, bool, _FX_OP_CMP_EQ)

_FX_IMPLEMENT_BINARY_OP(cmp_gt_f32, float, float, bool, _FX_OP_CMP_GT)
_FX_IMPLEMENT_BINARY_OP(cmp_gt_i32, int, int, bool, _FX_OP_CMP_GT)
_FX_IMPLEMENT_BINARY_OP(cmp_gt_i64, int64_t, int64_t, bool, _FX_OP_CMP_GT)

_FX_IMPLEMENT_BINARY_OP(cmp_ge_f32, float, float, bool, _FX_OP_CMP_GE)
_FX_IMPLEMENT_BINARY_OP(cmp_ge_i32, int, int, bool, _FX_OP_CMP_GE)
_FX_IMPLEMENT_BINARY_OP(cmp_ge_i64, int64_t, int64_t, bool, _FX_OP_CMP_GE)

_FX_IMPLEMENT_BINARY_OP(and_bool, bool, bool, bool, _FX_OP_AND)
_FX_IMPLEMENT_BINARY_OP(or_bool, bool, bool, bool, _FX_OP_OR)
_FX_IMPLEMENT_BINARY_OP(xor_bool, bool, bool, bool, _FX_OP_XOR)

static _fx_elemwise_binary_func_t
_fx_get_elemwise_binary_func(int el_op, int typ)
{
    return
        el_op == _FX_NN_Add ?
            (typ == _FX_NN_FP32 ? _fx_elemwise_add_f32 :
             typ == _FX_NN_I32 ? _fx_elemwise_add_i32 :
             typ == _FX_NN_I64 ? _fx_elemwise_add_i64 : 0) :
        el_op == _FX_NN_Sub ?
            (typ == _FX_NN_FP32 ? _fx_elemwise_sub_f32 :
             typ == _FX_NN_I32 ? _fx_elemwise_sub_i32 :
             typ == _FX_NN_I64 ? _fx_elemwise_sub_i64 : 0) :
        el_op == _FX_NN_Mul ?
            (typ == _FX_NN_FP32 ? _fx_elemwise_mul_f32 :
             typ == _FX_NN_I32 ? _fx_elemwise_mul_i32 :
             typ == _FX_NN_I64 ? _fx_elemwise_mul_i64 : 0) :
        el_op == _FX_NN_Div ?
            (typ == _FX_NN_FP32 ? _fx_elemwise_div_f32 :
             typ == _FX_NN_I32 ? _fx_elemwise_div_i32 :
             typ == _FX_NN_I64 ? _fx_elemwise_div_i64 : 0) :
        el_op == _FX_NN_Mod ?
            (typ == _FX_NN_FP32 ? _fx_elemwise_mod_f32 :
             typ == _FX_NN_I32 ? _fx_elemwise_mod_i32 :
             typ == _FX_NN_I64 ? _fx_elemwise_mod_i64 : 0) :
        el_op == _FX_NN_Pow ?
            (typ == _FX_NN_FP32 ? _fx_elemwise_pow_f32 :
             typ == _FX_NN_I32 ? _fx_elemwise_pow_i32 :
             typ == _FX_NN_I64 ? _fx_elemwise_pow_i64 : 0) :
        el_op == _FX_NN_Min ?
            (typ == _FX_NN_FP32 ? _fx_elemwise_min_f32 :
             typ == _FX_NN_I32 ? _fx_elemwise_min_i32 :
             typ == _FX_NN_I64 ? _fx_elemwise_min_i64 : 0) :
        el_op == _FX_NN_Max ?
            (typ == _FX_NN_FP32 ? _fx_elemwise_max_f32 :
             typ == _FX_NN_I32 ? _fx_elemwise_max_i32 :
             typ == _FX_NN_I64 ? _fx_elemwise_max_i64 : 0) :
        el_op == _FX_NN_Mean ?
            (typ == _FX_NN_FP32 ? _fx_elemwise_mean_f32 :
             typ == _FX_NN_I32 ? _fx_elemwise_mean_i32 :
             typ == _FX_NN_I64 ? _fx_elemwise_mean_i64 : 0) :
        el_op == _FX_NN_Equal ?
            (typ == _FX_NN_FP32 ? _fx_elemwise_cmp_eq_f32 :
             typ == _FX_NN_I32 ? _fx_elemwise_cmp_eq_i32 :
             typ == _FX_NN_I64 ? _fx_elemwise_cmp_eq_i64 : 0) :
        el_op == _FX_NN_Greater ?
            (typ == _FX_NN_FP32 ? _fx_elemwise_cmp_gt_f32 :
             typ == _FX_NN_I32 ? _fx_elemwise_cmp_gt_i32 :
             typ == _FX_NN_I64 ? _fx_elemwise_cmp_gt_i64 : 0) :
        el_op == _FX_NN_GreaterOrEqual ?
            (typ == _FX_NN_FP32 ? _fx_elemwise_cmp_ge_f32 :
             typ == _FX_NN_I32 ? _fx_elemwise_cmp_ge_i32 :
             typ == _FX_NN_I64 ? _fx_elemwise_cmp_ge_i64 : 0) :
        el_op == _FX_NN_And ?
            (typ == _FX_NN_Bool ? _fx_elemwise_and_bool : 0) :
        el_op == _FX_NN_Or ?
            (typ == _FX_NN_Bool ? _fx_elemwise_or_bool : 0) :
        el_op == _FX_NN_Xor ?
            (typ == _FX_NN_Bool ? _fx_elemwise_and_bool : 0) :
        0;
}

}

@private fun run_binary(inp1_shape_: int [], inp1_data_: Ast.nndata_t,
                        inp2_shape_: int [], inp2_data_: Ast.nndata_t,
                        out_shape_: int [], out_data_: Ast.nndata_t,
                        el_op_: Ast.nnelwise_t, ~param: double=0.): void
@ccode
{
    int_ shape1[_FX_ELEMWISE_MAX_DIMS], shape2[_FX_ELEMWISE_MAX_DIMS], shape[_FX_ELEMWISE_MAX_DIMS];
    size_t step1[_FX_ELEMWISE_MAX_DIMS], step2[_FX_ELEMWISE_MAX_DIMS], step[_FX_ELEMWISE_MAX_DIMS];
    int el_op = el_op_->tag, inp_typ = inp1_data_->tag;
    fx_arr_t* inp1_data = &inp1_data_->u.NN_Data_I8;
    fx_arr_t* inp2_data = &inp2_data_->u.NN_Data_I8;
    fx_arr_t* out_data = &out_data_->u.NN_Data_I8;

    if (el_op == _FX_NN_Less || el_op == _FX_NN_LessOrEqual) {
        el_op = el_op == _FX_NN_Less ? _FX_NN_Greater : _FX_NN_GreaterOrEqual;
        fx_arr_t* t = inp1_shape_; inp1_shape_ = inp2_shape_; inp2_shape_ = t;
        t = inp1_data; inp1_data = inp2_data; inp2_data = t;
    }

    int all_ndims[] = {
        (int)inp1_shape_->dim[0].size,
        (int)inp2_shape_->dim[0].size,
        (int)out_shape_->dim[0].size
    };
    const int_* orig_shapes[] = {
        (int_*)inp1_shape_->data,
        (int_*)inp2_shape_->data,
        (int_*)out_shape_->data
    };
    int_* shapes[] = {shape1, shape2, shape};
    size_t* steps[] = {step1, step2, step};

    for (int i = 0; i < 3; i++) {
        if (all_ndims[i] > _FX_ELEMWISE_MAX_DIMS)
            return FX_SET_EXN_FAST(FX_EXN_NotImplementedError);
    }

    // some of inputs are empty => result is empty
    if (!_fx_prepare_for_broadcast_op(3, _FX_ELEMWISE_MAX_DIMS,
                                  all_ndims, orig_shapes,
                                  shapes, steps))
        return FX_OK;

    {
    size_t dp1 = step1[_FX_ELEMWISE_MAX_DIMS-1];
    size_t dp2 = step2[_FX_ELEMWISE_MAX_DIMS-1];
    size_t dp = step[_FX_ELEMWISE_MAX_DIMS-1];
    size_t rowstep1 = step1[_FX_ELEMWISE_MAX_DIMS-2];
    size_t rowstep2 = step2[_FX_ELEMWISE_MAX_DIMS-2];
    size_t rowstep = step[_FX_ELEMWISE_MAX_DIMS-2];
    char* data1 = inp1_data->data;
    char* data2 = inp2_data->data;
    char* data = out_data->data;
    size_t esz1 = inp1_data->dim[0].step;
    size_t esz2 = inp2_data->dim[0].step;
    size_t esz = out_data->dim[0].step;
    int_ nrows = shape[_FX_ELEMWISE_MAX_DIMS-2];
    int_ ncols = shape[_FX_ELEMWISE_MAX_DIMS-1];
    size_t plane_idx, nplanes = 1;
    _fx_elemwise_binary_func_t processing_func = _fx_get_elemwise_binary_func(el_op, inp_typ);

    if (!processing_func)
        return FX_SET_EXN_FAST(FX_EXN_NotImplementedError);

    for (int k = 0; k < _FX_ELEMWISE_MAX_DIMS-2; k++) nplanes *= shape[k];

    for (plane_idx = 0; plane_idx < nplanes; plane_idx++) {
        size_t ofs1 = 0, ofs2 = 0, ofs = 0;
        size_t idx = plane_idx;
        for (int k = _FX_ELEMWISE_MAX_DIMS-3; k >= 0; k--) {
            size_t prev_idx = idx/shape[k];
            size_t i_k = idx - prev_idx*shape[k];
            ofs1 += i_k*step1[k];
            ofs2 += i_k*step2[k];
            ofs += i_k*step[k];
            idx = prev_idx;
        }

        processing_func(data1 + ofs1*esz1, rowstep1, dp1,
                        data2 + ofs2*esz2, rowstep2, dp2,
                        data + ofs*esz, rowstep, dp,
                        nrows, ncols, param);
    }
    }

    return FX_OK;
}

fun run_binary(model: Ast.nnmodel_t, op: Ast.nnop_t) =
match op
{
| Ast.NN_Elemwise {el_op, t_inp, t_out} when t_inp.size() == 2 =>
    val inp1 = model.get_tensor(t_inp[0])
    val inp2 = model.get_tensor(t_inp[1])
    val out = model.get_tensor(t_out)
    run_binary(inp1.shape.shape, inp1.data, inp2.shape.shape, inp2.data,
               out.shape.shape, out.data, el_op,
               param=0.5 // so far the only binary operation that needs parameter
                         // is Mean and there the parameter is 0.5; other operations just ignore it
               )
| _ => throw Ast.NNError(f"unexpected op {op.name()}")
}

fun run_nary(model: Ast.nnmodel_t, op: Ast.nnop_t) =
match op
{
| Ast.NN_Elemwise {el_op, t_inp, t_out} when t_inp.size() >= 2 =>
    val ninputs = t_inp.size()
    val inp = [for i <- t_inp {model.get_tensor(i)}]
    val out = model.get_tensor(t_out)
    match el_op {
    | Ast.NN_Max =>
        run_binary(inp[0].shape.shape, inp[0].data, inp[1].shape.shape, inp[1].data,
                   out.shape.shape, out.data, Ast.NN_Max)
        for j <- 2:ninputs {
            run_binary(inp[j].shape.shape, inp[j].data, out.shape.shape, out.data,
                       out.shape.shape, out.data, Ast.NN_Max)
        }
    | Ast.NN_Mean =>
        val scale = 1./ninputs
        run_binary(inp[0].shape.shape, inp[0].data, inp[1].shape.shape, inp[1].data,
                   out.shape.shape, out.data, Ast.NN_Add)
        for j <- 2:ninputs {
            val op = if j == ninputs-1 {Ast.NN_Mean} else {Ast.NN_Add}
            run_binary(inp[j].shape.shape, inp[j].data, out.shape.shape, out.data,
                       out.shape.shape, out.data, op, param=scale)
        }
    | Ast.NN_Min =>
        run_binary(inp[0].shape.shape, inp[0].data, inp[1].shape.shape, inp[1].data,
                   out.shape.shape, out.data, Ast.NN_Min)
        for j <- 2:ninputs {
            run_binary(inp[j].shape.shape, inp[j].data, out.shape.shape, out.data,
                       out.shape.shape, out.data, Ast.NN_Min)
        }
    | _ => throw NotImplementedError
    }
| _ => throw Ast.NNError(f"unexpected op {op.name()}")
}

fun run_dropout(inp: 't [], out: 't [], ratio: float)
{
    val scale = 1.f/(1 - ratio);
    for x@idx <- inp {out[idx] = (x*scale :> 't)}
}

fun run_dropout(model: Ast.nnmodel_t, op: Ast.nnop_t) =
match op
{
| Ast.NN_Dropout {t_inp, t_ratio, t_training_mode, t_out} =>
    val inp = model.get_tensor(t_inp)
    val out = model.get_tensor(t_out)
    val training_mode =
        if t_training_mode == 0 {
            false
        } else {
            val t = model.get_tensor(t_training_mode)
            match t.data {
            | Ast.NN_Data_Bool f => f[0]
            | Ast.NN_Data_FP32 f => f[0] != 0.f
            | Ast.NN_Data_I32 f => f[0] != 0i32
            | Ast.NN_Data_U8 f => f[0] != 0u8
            | Ast.NN_Data_I8 f => f[0] != 0i8
            | tdata => int(tdata)[0] != 0
            }
        }

    val ratio =
        if !training_mode {0.f}
        else if t_ratio == 0 {0.5f} else {
            match model.get_tensor(t_ratio).data {
            | Ast.NN_Data_FP32 ratio_data => ratio_data[0]
            | _ => throw NotImplementedError
            }
        }
    if ratio == 0.f {
        model.copy_tensor_data(t_inp, t_out)
    } else {
        match (inp.data, out.data) {
        | (Ast.NN_Data_FP32 inp_data, Ast.NN_Data_FP32 out_data) =>
            run_dropout(inp_data, out_data, ratio)
        | _ => throw NotImplementedError
        }
    }
| _ => throw Ast.NNError(f"unexpected op {op.name()}")
}

@private fun run_expand(inp_shape_: int [], inp_data_: Ast.nndata_t,
                        out_shape_: int [], out_data_: Ast.nndata_t): void
@ccode
{
    #undef _FX_EXPAND_MAX_DIMS
    #define _FX_EXPAND_MAX_DIMS 5
    int_ inp_shape[_FX_EXPAND_MAX_DIMS], shape[_FX_EXPAND_MAX_DIMS];
    size_t inp_step[_FX_EXPAND_MAX_DIMS], out_step[_FX_EXPAND_MAX_DIMS];
    fx_arr_t* inp_data = &inp_data_->u.NN_Data_I8;
    fx_arr_t* out_data = &out_data_->u.NN_Data_I8;
    size_t esz = inp_data->dim[0].step;

    int all_ndims[] = {
        (int)inp_shape_->dim[0].size,
        (int)out_shape_->dim[0].size
    };
    const int_* orig_shapes[] = {
        (int_*)inp_shape_->data,
        (int_*)out_shape_->data
    };
    int_* shapes[] = {inp_shape, shape};
    size_t* steps[] = {inp_step, out_step};

    if (all_ndims[0] > _FX_EXPAND_MAX_DIMS || all_ndims[1] > _FX_EXPAND_MAX_DIMS)
        return FX_SET_EXN_FAST(FX_EXN_NotImplementedError);

    if (inp_data_->tag != out_data_->tag)
        return FX_SET_EXN_FAST(FX_EXN_TypeMismatchError);

    // some of inputs are empty => result is empty
    if (!_fx_prepare_for_broadcast_op(2, _FX_EXPAND_MAX_DIMS, all_ndims, orig_shapes, shapes, steps))
        return FX_OK;

    {
    size_t inp_dp = inp_step[_FX_EXPAND_MAX_DIMS-1];
    size_t out_dp = out_step[_FX_EXPAND_MAX_DIMS-1];
    assert(out_dp == 1);
    size_t inp_rowstep = inp_step[_FX_EXPAND_MAX_DIMS-2];
    size_t out_rowstep = out_step[_FX_EXPAND_MAX_DIMS-2];
    char* inptr0 = inp_data->data;
    char* outptr0 = out_data->data;
    size_t esz = out_data->dim[0].step;
    int_ nrows = shape[_FX_EXPAND_MAX_DIMS-2];
    int_ ncols = shape[_FX_EXPAND_MAX_DIMS-1];
    size_t plane_idx, nplanes = 1;

    if (esz != 1 && esz != 2 && esz != 4 && esz != 8)
        return FX_SET_EXN_FAST(FX_EXN_NotImplementedError);

    for (int k = 0; k < _FX_EXPAND_MAX_DIMS-2; k++) nplanes *= shape[k];

    for (plane_idx = 0; plane_idx < nplanes; plane_idx++) {
        size_t inp_ofs = 0, out_ofs = 0;
        size_t idx = plane_idx;
        for (int k = _FX_EXPAND_MAX_DIMS-3; k >= 0; k--) {
            size_t prev_idx = idx/shape[k];
            size_t i_k = idx - prev_idx*shape[k];
            inp_ofs += i_k*inp_step[k];
            out_ofs += i_k*out_step[k];
            idx = prev_idx;
        }

        #undef _FX_IMPLEMENT_EXPAND
        #define _FX_IMPLEMENT_EXPAND(_Tp) \
            for (int_ i = 0; i < nrows; i++) { \
                const _Tp* inptr = (const _Tp*)inptr0 + inp_ofs + inp_rowstep*i; \
                _Tp* outptr = (_Tp*)outptr0 + out_ofs + out_rowstep*i; \
                if (inp_dp == 1) { \
                    for(int_ j = 0; j < ncols; j++) { \
                        outptr[j] = inptr[j]; \
                    } \
                } else { \
                    _Tp x = *inptr; \
                    for(int_ j = 0; j < ncols; j++) { \
                        outptr[j] = x; \
                    } \
                } \
            }

        if (esz == 1) {
            _FX_IMPLEMENT_EXPAND(int8_t)
        } else if (esz == 2) {
            _FX_IMPLEMENT_EXPAND(int16_t)
        } else if (esz == 4) {
            _FX_IMPLEMENT_EXPAND(int32_t)
        } else {
            _FX_IMPLEMENT_EXPAND(int64_t)
        }
    }
    }

    return FX_OK;
}

fun run_expand(model: Ast.nnmodel_t, op: Ast.nnop_t) =
match op
{
| Ast.NN_Expand {t_inp, t_out} =>
    val inp = model.get_tensor(t_inp)
    val out = model.get_tensor(t_out)
    run_expand(inp.shape.shape, inp.data,
               out.shape.shape, out.data)
| _ => throw Ast.NNError(f"unexpected op {op.name()}")
}
