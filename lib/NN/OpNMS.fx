/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// ONNX-compliant implementation of NonMaxSuppression

import Ast

@ccode {
#include <float.h>
#include <limits.h>

typedef struct _fx_nms_entry
{
    float y1, x1, y2, x2;
    float score;
    int cls;
    int idx;
} _fx_nms_entry;

/*
    We sort all the boxes by classes, since non-maxima suppression is done independently in each class.
    Inside each class we sort boxes by the score, highest scores go first.
    In an extremely rare case when 2 boxes belong to the same class and have absolutely the same score
    We preserve the original order. This makes the sorting results completely determinstic regarless
    of the actual implementation of qsort().
*/
static bool _fx_lt_boxes(const void* a_, const void* b_, void* userdata)
{
    const _fx_nms_entry* a = (const _fx_nms_entry*)a_;
    const _fx_nms_entry* b = (const _fx_nms_entry*)b_;
    if (a->cls != b->cls)
        return a->cls < b->cls;
    if (a->score != b->score)
        return a->score > b->score;
    return a->idx < b->idx;
}

static float _fx_iou_boxes(const _fx_nms_entry* a, const _fx_nms_entry* b)
{
    float area1 = (a->x2 - a->x1)*(a->y2 - a->y1);
    float area2 = (b->x2 - b->x1)*(b->y2 - b->y1);

    float xmin = a->x1 > b->x1 ? a->x1 : b->x1;
    float ymin = a->y1 > b->y1 ? a->y1 : b->y1;
    float xmax = a->x2 < b->x2 ? a->x2 : b->x2;
    float ymax = a->y2 < b->y2 ? a->y2 : b->y2;

    float dx = xmax - xmin, dy = ymax - ymin;
    dx = dx > 0.f ? dx : 0.f;
    dy = dy > 0.f ? dy : 0.f;

    float inter_area = dx*dy;
    float union_area = area1 + area2 - inter_area;

    return inter_area/(union_area > FLT_EPSILON ? union_area : FLT_EPSILON);
}
}

fun run_nms(boxes: Ast.nntensor_t, scores: Ast.nntensor_t,
            max_boxes: int, center_point_box: bool,
            iou_threshold: float, score_threshold: float,
            out_buf0: Ast.nnbuf_t, ntasks: int):
    (int, int64 [], Ast.nnbuf_t)
@ccode {
    int boxes_typ = boxes->data.tag, scores_typ = scores->data.tag;
    const int_* boxes_shape = (const int_*)(boxes->shape.shape.data);
    const char* boxes_data = boxes->data.u.NN_Data_I8.data;
    const int_* scores_shape = (const int_*)(scores->shape.shape.data);
    const char* scores_data = scores->data.u.NN_Data_I8.data;
    int_ N, B, Bx, C;
    volatile int status = FX_OK;
    _fx_nms_entry* best_boxes = 0;
    int* best_counters = 0;
    int_ total_best = 0;
    int64_t* outptr;
    fx_arr_t out_buf, out_data;

    if (max_boxes == 0) max_boxes = INT_MAX;

    if ((boxes_typ != FX_F32 && boxes_typ != FX_F16) ||
        (scores_typ != FX_F32 && scores_typ != FX_F16))
        return FX_SET_EXN_FAST(FX_EXN_NotImplementedError);

    if (boxes->shape.shape.dim[0].size != 3 ||
        scores->shape.shape.dim[0].size != 3)
        return FX_SET_EXN_FAST(FX_EXN_SizeError);

    if (boxes_shape[0] != scores_shape[0] ||
        boxes_shape[1] != scores_shape[2] ||
        boxes_shape[2] != 4)
        return FX_SET_EXN_FAST(FX_EXN_SizeMismatchError);

    N = boxes_shape[0];
    B = boxes_shape[1];
    Bx = B*3 + 100;
    C = scores_shape[1];
    //printf("N=%d, B=%d, C=%d\n", (int)N, (int)B, (int)C);

    best_boxes = (_fx_nms_entry*)fx_malloc(N*sizeof(int) + N*Bx*sizeof(_fx_nms_entry));
    best_counters = (int*)(best_boxes + N*Bx);

    if (!best_boxes)
        return FX_SET_EXN_FAST(FX_EXN_OutOfMemError);
    ntasks = ntasks < N ? ntasks : N;
    if (N*B*C < 10000)
        ntasks = 1;

    #pragma omp parallel for num_threads(ntasks)
    for (int_ task_id = 0; task_id < ntasks; task_id++)
    {
        int_ n0 = task_id*N/ntasks, n1 = (task_id+1)*N/ntasks;
        for (; n0 < n1 && status >= 0; n0++) {
            _fx_nms_entry* bboxes_n = best_boxes + n0*Bx, *bboxes = bboxes_n;
            _fx_nms_entry* nms_bboxes = bboxes;
            int_ i, j, k, i0, i1, nboxes;

            for (i = 0; i < B; i++) {
                const float* scores_ni = (const float*)scores_data + n0*B*C + i;
                const fx_f16* scores_ni_f16 = (const fx_f16*)scores_data + n0*B*C + i;
                float best_score = scores_ni[0];
                int_ best_label = 0;
                bool multiclass = false;
                for (int_ j = 1; j < C; j++) {
                    float score_nij = scores_typ == FX_F32 ? scores_ni[j*B] : FX_FLOAT(scores_ni_f16[j*B]);
                    //printf("%d. class=%d. score=%.2f\n", (int)i, (int)j, score_nij);
                    if (score_nij > best_score)
                    {
                        best_score = score_nij;
                        best_label = j;
                        multiclass = false;
                    }
                    else if (score_nij == best_score)
                        multiclass = true;
                }

                /*printf("%s%d. best_score=%.5g, score_threshold=%.5g\n",
                    (best_score > score_threshold ? "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! " : ""),
                    (int)i, best_score, score_threshold);*/
                if (best_score > score_threshold) {
                    float box0[4];
                    if (boxes_typ == FX_F32) {
                        const float* boxptr = (const float*)boxes_data + (n0*B + i)*4;
                        box0[0] = boxptr[0];
                        box0[1] = boxptr[1];
                        box0[2] = boxptr[2];
                        box0[3] = boxptr[3];
                    } else {
                        const fx_f16* boxptr_f16 = (const fx_f16*)boxes_data + (n0*B + i)*4;
                        box0[0] = FX_FLOAT(boxptr_f16[0]);
                        box0[1] = FX_FLOAT(boxptr_f16[1]);
                        box0[2] = FX_FLOAT(boxptr_f16[2]);
                        box0[3] = FX_FLOAT(boxptr_f16[3]);
                    }
                    i0 = 0; i1 = C;
                    if (!multiclass) {
                        i0 = best_label;
                        i1 = best_label+1;
                    }
                    for (; i0 < i1 && (bboxes - bboxes_n) < Bx; i0++) {
                        if (scores_ni[i0*B] < best_score)
                            continue;
                        if (!center_point_box) {
                            bboxes->y1 = box0[0];
                            bboxes->x1 = box0[1];
                            bboxes->y2 = box0[2];
                            bboxes->x2 = box0[3];
                        } else {
                            float cx = box0[0], cy = box0[1], w = box0[2], h = box0[3];
                            bboxes->y1 = cy - h*0.5f;
                            bboxes->x1 = cx - w*0.5f;
                            bboxes->y2 = cy + h*0.5f;
                            bboxes->x2 = cx + w*0.5f;
                        }
                        //printf("%d. multiclass=%d, cls=%d, score=%.2f\n", (int)i, (int)multiclass, (int)i0, best_score);
                        bboxes->score = best_score;
                        bboxes->cls = (int)i0;
                        bboxes->idx = (int)i;
                        bboxes++;
                    }
                }
            }
            nboxes = bboxes - bboxes_n;
            bboxes = bboxes_n;
            if (nboxes == 0) {
                best_counters[n0] = 0;
                continue;
            }

            status = fx_qsort(bboxes, nboxes, sizeof(bboxes[0]), _fx_lt_boxes, 0, 0);
            if (status < 0)
                break;
            /*printf("after qsort\n");
            for (i0 = 0; i0 < nboxes; i0++) {
                printf("idx=%d, cls=%d, score=%.2f\n", bboxes[i0].idx,
                    bboxes[i0].cls, bboxes[i0].score);
            }*/

            // do the actual non-maxima suppression inside each class
            for (i0 = 0; i0 < nboxes;) {
                int cls = bboxes[i0].cls;
                int_ ccount = 0;
                for (i1 = i0+1; i1 < nboxes; i1++) {
                    if (bboxes[i1].cls != cls)
                        break;
                }
                for (;i0 < i1 && ccount < max_boxes; nms_bboxes++, ccount++) {
                    //printf("nboxes=%d, cls=%d, i0=%d, i1=%d, ccount=%d\n", (int)nboxes, cls, (int)i0, (int)i1, (int)ccount);
                    *nms_bboxes = bboxes[i0];
                    for (j = i1 - 1, k = i1; j > i0; j--) {
                        if (_fx_iou_boxes(bboxes + j, nms_bboxes) <= iou_threshold) {
                            if (--k > j)
                                bboxes[k] = bboxes[j];
                        }
                    }
                    i0 = k;
                }
                i0 = i1;
            }
            best_counters[n0] = (int)(nms_bboxes - bboxes);
        }
    }

    for (int_ i = 0; i < N; i++) {
        //printf("best_counter[%d]=%d\n", (int)i, best_counters[i]);
        total_best += best_counters[i];
    }

    if (total_best*3*sizeof(*outptr) > out_buf0->dim[0].size*out_buf0->dim[0].step) {
        int_ total_bytes = (int_)(total_best*3*sizeof(*outptr));
        status = fx_make_arr(1, &total_bytes, 1, 0, 0, 0, &out_buf);
    } else {
        fx_copy_arr(out_buf0, &out_buf);
    }
    if (status >= 0) {
        fx_copy_arr(&out_buf, &out_data);
        out_data.dim[0].size = total_best*3;
        out_data.dim[0].step = sizeof(*outptr);
        outptr = (int64_t*)out_data.data;
        for (int_ n = 0; n < N; n++) {
            _fx_nms_entry* bboxes = best_boxes + n*B;
            int i, Bn = best_counters[n];
            for (i = 0; i < Bn; i++, bboxes++) {
                outptr[0] = (int64_t)n;
                outptr[1] = bboxes->cls;
                outptr[2] = bboxes->idx;
                outptr += 3;
            }
        }
        fx_result->t0 = total_best;
        fx_result->t1 = out_data;
        fx_result->t2 = out_buf;
    }
    fx_free(best_boxes);
    return status;
}

fun run_nms(model: Ast.nnmodel_t, op: Ast.nnop_t) =
match op {
| Ast.NN_NonMaxSuppression {center_point_box, t_boxes, t_scores,
        t_max_output_boxes_per_class,
        t_iou_threshold, t_score_threshold, t_out} =>
    val boxes = model.get_tensor(t_boxes)
    val scores = model.get_tensor(t_scores)
    val max_boxes = model.get_tensor(t_max_output_boxes_per_class)
    val out_bufidx = model.bufidxs[t_out]
    val out_buf = model.buffers[out_bufidx]
    val iou_threshold = model.get_tensor(t_iou_threshold).data.float_scalar_or(0.f)
    val score_threshold = model.get_tensor(t_score_threshold).data.float_scalar_or(0.f)
    val max_boxes = match max_boxes.data {
        | Ast.NN_Data_Empty => 0i64
        | Ast.NN_Data_I64 max_boxes_data => max_boxes_data[0]
        | _ => throw Ast.NNError(f"NonMaxSuppression: 'max_output_boxes_per_class' parameter, if any, must have I64 type")
    }
    val (ndetections, out_data, out_buf) = run_nms(boxes, scores,
            int(max_boxes), center_point_box, iou_threshold,
            score_threshold, out_buf, *model.ntasks)
    model.buffers[out_bufidx] = out_buf
    model.tensors[t_out] = Ast.nntensor_t {data = Ast.NN_Data_I64(out_data),
            shape = Ast.nnshape_t {layout=Ast.NN_Layout_ND, shape=[ndetections, 3]}}
| _ => throw Ast.NNError(f"unsupported operation '{op.name()}'")
}
