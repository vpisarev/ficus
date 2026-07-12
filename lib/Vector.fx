/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

// operations on the first-class mutable vector ('t vector, runtime fx_vec_t).
// Unlike the immutable RRB 'rrbvec', this is a contiguous, growable, in-place
// mutable buffer (STL vector / Python list). Element access v[i], v[i]=a and
// size()/empty() are compiler intrinsics; the operations below are stdlib.

// append one element in place. The vector must be already allocated (e.g. via
// `[]`); a growth reallocates the internal data buffer inside the shared header,
// so the caller's binding keeps pointing at the same (updated) header.
fun push_back(v: 't vector, elem: 't): void
{
    fun push_back_(v: 't vector, elem_: ('t, bool)): void
    @ccode {
        if (!v)
            FX_FAST_THROW_RET(FX_EXN_NullPtrError);
        return fx_vec_append(v, elem_, 1);
    }
    // the (elem, true) tuple is always passed by reference, so elem_ is a
    // pointer to the element (its first field) regardless of 't.
    push_back_(v, (elem, true))
}

// copy the vector's elements into a plain array (uses the v[i]/size intrinsics)
fun array(v: 't vector): 't [] = [for i <- 0:size(v) {v[i]}]
