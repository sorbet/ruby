/**********************************************************************

  method.h -

  $Author$
  created at: Wed Jul 15 20:02:33 2009

  Copyright (C) 2009 Koichi Sasada

**********************************************************************/
#ifndef RUBY_METHOD_H
#define RUBY_METHOD_H 1

#include "internal.h"

#ifndef END_OF_ENUMERATION
# if defined(__GNUC__) &&! defined(__STRICT_ANSI__)
#   define END_OF_ENUMERATION(key)
# else
#   define END_OF_ENUMERATION(key) END_OF_##key##_PLACEHOLDER = 0
# endif
#endif

/* cref */

typedef enum {
    METHOD_VISI_UNDEF     = 0x00,
    METHOD_VISI_PUBLIC    = 0x01,
    METHOD_VISI_PRIVATE   = 0x02,
    METHOD_VISI_PROTECTED = 0x03,

    METHOD_VISI_MASK = 0x03
} rb_method_visibility_t;

typedef struct rb_scope_visi_struct {
    BITFIELD(rb_method_visibility_t, method_visi, 3);
    unsigned int module_func : 1;
} rb_scope_visibility_t;

/*! CREF (Class REFerence) */
typedef struct rb_cref_struct {
    VALUE flags;
    VALUE refinements;
    VALUE klass;
    struct rb_cref_struct * next;
    const rb_scope_visibility_t scope_visi;
} rb_cref_t;

/* method data type */

typedef struct rb_method_entry_struct {
    VALUE flags;
    VALUE defined_class;
    struct rb_method_definition_struct * const def;
    ID called_id;
    VALUE owner;
} rb_method_entry_t;

typedef struct rb_callable_method_entry_struct { /* same fields with rb_method_entry_t */
    VALUE flags;
    const VALUE defined_class;
    struct rb_method_definition_struct * const def;
    ID called_id;
    const VALUE owner;
} rb_callable_method_entry_t;

#define METHOD_ENTRY_VISI(me)  (rb_method_visibility_t)(((me)->flags & (IMEMO_FL_USER0 | IMEMO_FL_USER1)) >> (IMEMO_FL_USHIFT+0))
#define METHOD_ENTRY_BASIC(me) (int)                   (((me)->flags & (IMEMO_FL_USER2                 )) >> (IMEMO_FL_USHIFT+2))
#define METHOD_ENTRY_COMPLEMENTED(me)     ((me)->flags & IMEMO_FL_USER3)
#define METHOD_ENTRY_COMPLEMENTED_SET(me) ((me)->flags = (me)->flags | IMEMO_FL_USER3)

static inline void
METHOD_ENTRY_VISI_SET(rb_method_entry_t *me, rb_method_visibility_t visi)
{
    VM_ASSERT((int)visi >= 0 && visi <= 3);
    me->flags = (me->flags & ~(IMEMO_FL_USER0 | IMEMO_FL_USER1)) | (visi << (IMEMO_FL_USHIFT+0));
}
static inline void
METHOD_ENTRY_BASIC_SET(rb_method_entry_t *me, unsigned int basic)
{
    VM_ASSERT(basic <= 1);
    me->flags = (me->flags & ~(IMEMO_FL_USER2                 )) | (basic << (IMEMO_FL_USHIFT+2));
}
static inline void
METHOD_ENTRY_FLAGS_SET(rb_method_entry_t *me, rb_method_visibility_t visi, unsigned int basic)
{
    VM_ASSERT((int)visi >= 0 && visi <= 3);
    VM_ASSERT(basic <= 1);
    me->flags =
      (me->flags & ~(IMEMO_FL_USER0|IMEMO_FL_USER1|IMEMO_FL_USER2)) |
        ((visi << (IMEMO_FL_USHIFT+0)) | (basic << (IMEMO_FL_USHIFT+2)));
}
static inline void
METHOD_ENTRY_FLAGS_COPY(rb_method_entry_t *dst, const rb_method_entry_t *src)
{
    dst->flags =
      (dst->flags & ~(IMEMO_FL_USER0|IMEMO_FL_USER1|IMEMO_FL_USER2)) |
        (src->flags & (IMEMO_FL_USER0|IMEMO_FL_USER1|IMEMO_FL_USER2));
}

typedef enum {
    VM_METHOD_TYPE_ISEQ,      /*!< Ruby method */
    VM_METHOD_TYPE_CFUNC,     /*!< C method */
    VM_METHOD_TYPE_SORBET,
    VM_METHOD_TYPE_ATTRSET,   /*!< attr_writer or attr_accessor */
    VM_METHOD_TYPE_IVAR,      /*!< attr_reader or attr_accessor */
    VM_METHOD_TYPE_BMETHOD,
    VM_METHOD_TYPE_ZSUPER,
    VM_METHOD_TYPE_ALIAS,
    VM_METHOD_TYPE_UNDEF,
    VM_METHOD_TYPE_NOTIMPLEMENTED,
    VM_METHOD_TYPE_OPTIMIZED, /*!< Kernel#send, Proc#call, etc */
    VM_METHOD_TYPE_MISSING,   /*!< wrapper for method_missing(id) */
    VM_METHOD_TYPE_REFINED,   /*!< refinement */

    END_OF_ENUMERATION(VM_METHOD_TYPE)
} rb_method_type_t;
#define VM_METHOD_TYPE_MINIMUM_BITS 4
STATIC_ASSERT(VM_METHOD_TYPE_MINIMUM_BITS,
              VM_METHOD_TYPE_REFINED <= (1<<VM_METHOD_TYPE_MINIMUM_BITS));

#ifndef rb_iseq_t
typedef struct rb_iseq_struct rb_iseq_t;
#define rb_iseq_t rb_iseq_t
#endif

typedef struct rb_method_iseq_struct {
    rb_iseq_t * iseqptr; /*!< iseq pointer, should be separated from iseqval */
    rb_cref_t * cref;          /*!< class reference, should be marked */
} rb_method_iseq_t; /* check rb_add_method_iseq() when modify the fields */

typedef struct rb_method_cfunc_struct {
    VALUE (*func)(ANYARGS);
    VALUE (*invoker)(VALUE recv, int argc, const VALUE *argv, VALUE (*func)(ANYARGS));
    int argc;
} rb_method_cfunc_t;

typedef struct rb_sorbet_param_struct {
    /**
     * parameter information
     *
     *  def m(a1, a2, ..., aM,                    # mandatory
     *        b1=(...), b2=(...), ..., bN=(...),  # optional
     *        *c,                                 # rest
     *        d1, d2, ..., dO,                    # post
     *        e1:(...), e2:(...), ..., eK:(...),  # keyword
     *        **f,                                # keyword_rest
     *        &g)                                 # block
     * =>
     *
     *  lead_num     = M
     *  opt_num      = N
     *  rest_start   = M+N
     *  post_start   = M+N+(*1)
     *  post_num     = O
     *  keyword_num  = K
     *  block_start  = M+N+(*1)+O+K
     *  keyword_bits = M+N+(*1)+O+K+(&1)
     *  size         = M+N+O+(*1)+K+(&1)+(**1) // parameter size.
     */

    struct {
        unsigned int has_lead   : 1;
        unsigned int has_opt    : 1;
        unsigned int has_rest   : 1;
        unsigned int has_post   : 1;
        unsigned int has_kw     : 1;
        unsigned int has_kwrest : 1;
        unsigned int has_block  : 1;

        unsigned int ambiguous_param0 : 1; /* {|a|} */
        unsigned int accepts_no_kwarg : 1;
        unsigned int ruby2_keywords: 1;
    } flags;

    unsigned int size;

    int lead_num;
    int opt_num;
    int rest_start;
    int post_start;
    int post_num;
    int block_start;

    /* M + N entries.  This is similar to rb_iseq_constant_body.local_table, but
     * Sorbet optimizes that to only include the variables that escape, so it is
     * not suited to describing parameter information for functions.
     */
    const ID *pos_table;

    /* Similar to rb_iseq_param_keyword, but inlined into the parent structure
     * so we don't need a separate allocation.  We also don't need to track
     * information about default values here.
     */
    int kw_num;
    int kw_required_num;
    int kw_bits_start;
    int kw_rest_start;
    const ID *kw_table;
} rb_sorbet_param_t;

typedef VALUE (*rb_sorbet_func_t)(int, VALUE *, VALUE, struct rb_control_frame_struct *, void *);

typedef struct rb_method_sorbet_struct {
    /* cf. rb_method_cfunc_struct, but we only support one argument style */
    rb_sorbet_func_t func;
    /* no need for invoker, since there's only the (argc, argv, recv) call style */
    /* similarly, no need for argc */

    const rb_sorbet_param_t *param; /* cf. rb_iseq_constant_body.param */
    rb_iseq_t *iseqptr;
} rb_method_sorbet_t;

typedef struct rb_method_attr_struct {
    ID id;
    VALUE location; /* should be marked */
} rb_method_attr_t;

typedef struct rb_method_alias_struct {
    struct rb_method_entry_struct * original_me; /* original_me->klass is original owner */
} rb_method_alias_t;

typedef struct rb_method_refined_struct {
    struct rb_method_entry_struct * orig_me;
    VALUE owner;
} rb_method_refined_t;

typedef struct rb_method_bmethod_struct {
    VALUE proc; /* should be marked */
    struct rb_hook_list_struct *hooks;
} rb_method_bmethod_t;

enum method_optimized_type {
    OPTIMIZED_METHOD_TYPE_SEND,
    OPTIMIZED_METHOD_TYPE_CALL,
    OPTIMIZED_METHOD_TYPE_BLOCK_CALL,
    OPTIMIZED_METHOD_TYPE__MAX
};

struct rb_method_definition_struct {
    BITFIELD(rb_method_type_t, type, VM_METHOD_TYPE_MINIMUM_BITS);
    int alias_count : 28;
    int complemented_count : 28;

    union {
        rb_method_iseq_t iseq;
        rb_method_cfunc_t cfunc;
        rb_method_sorbet_t sorbet;
        rb_method_attr_t attr;
        rb_method_alias_t alias;
        rb_method_refined_t refined;
        rb_method_bmethod_t bmethod;

        enum method_optimized_type optimize_type;
    } body;

    ID original_id;
    uintptr_t method_serial;
};

struct rb_id_table;

typedef struct rb_method_definition_struct rb_method_definition_t;
STATIC_ASSERT(sizeof_method_def, offsetof(rb_method_definition_t, body)==8);

#define UNDEFINED_METHOD_ENTRY_P(me) (!(me) || !(me)->def || (me)->def->type == VM_METHOD_TYPE_UNDEF)
#define UNDEFINED_REFINED_METHOD_P(def) \
    ((def)->type == VM_METHOD_TYPE_REFINED && \
     UNDEFINED_METHOD_ENTRY_P((def)->body.refined.orig_me))

void rb_add_method_cfunc(VALUE klass, ID mid, VALUE (*func)(ANYARGS), int argc, rb_method_visibility_t visi);
void rb_add_method_sorbet(VALUE klass, ID mid, rb_sorbet_func_t func, const rb_sorbet_param_t *param, rb_method_visibility_t visi, void *iseqptr);
/* included so we don't expose singleton_class_of outside of class.c */
/* we can't use rb_sorbet_func_t here because it's not exported */
void rb_define_singleton_sorbet_method(VALUE, const char*, rb_sorbet_func_t, const void *, void *);
void rb_add_method_iseq(VALUE klass, ID mid, const rb_iseq_t *iseq, rb_cref_t *cref, rb_method_visibility_t visi);
void rb_add_refined_method_entry(VALUE refined_class, ID mid);
void rb_add_method(VALUE klass, ID mid, rb_method_type_t type, void *option, rb_method_visibility_t visi);

rb_method_entry_t *rb_method_entry_set(VALUE klass, ID mid, const rb_method_entry_t *, rb_method_visibility_t noex);
rb_method_entry_t *rb_method_entry_create(ID called_id, VALUE klass, rb_method_visibility_t visi, const rb_method_definition_t *def);

const rb_method_entry_t *rb_method_entry_at(VALUE obj, ID id);

const rb_method_entry_t *rb_method_entry(VALUE klass, ID id);
const rb_method_entry_t *rb_method_entry_with_refinements(VALUE klass, ID id, VALUE *defined_class);
const rb_method_entry_t *rb_method_entry_without_refinements(VALUE klass, ID id, VALUE *defined_class);
const rb_method_entry_t *rb_resolve_refined_method(VALUE refinements, const rb_method_entry_t *me);
RUBY_SYMBOL_EXPORT_BEGIN
const rb_method_entry_t *rb_resolve_me_location(const rb_method_entry_t *, VALUE[5]);
RUBY_SYMBOL_EXPORT_END

const rb_callable_method_entry_t *rb_callable_method_entry(VALUE klass, ID id);
const rb_callable_method_entry_t *rb_callable_method_entry_with_refinements(VALUE klass, ID id, VALUE *defined_class);
const rb_callable_method_entry_t *rb_callable_method_entry_without_refinements(VALUE klass, ID id, VALUE *defined_class);

int rb_method_entry_arity(const rb_method_entry_t *me);
int rb_method_entry_eq(const rb_method_entry_t *m1, const rb_method_entry_t *m2);
st_index_t rb_hash_method_entry(st_index_t hash, const rb_method_entry_t *me);

VALUE rb_method_entry_location(const rb_method_entry_t *me);

void rb_free_method_entry(const rb_method_entry_t *me);

const rb_method_entry_t *rb_method_entry_clone(const rb_method_entry_t *me);
const rb_callable_method_entry_t *rb_method_entry_complement_defined_class(const rb_method_entry_t *src_me, ID called_id, VALUE defined_class);
void rb_method_entry_copy(rb_method_entry_t *dst, const rb_method_entry_t *src);

void rb_method_table_insert(VALUE klass, struct rb_id_table *table, ID method_id, const rb_method_entry_t *me);

void rb_scope_visibility_set(rb_method_visibility_t);

VALUE rb_unnamed_parameters(int arity);

#endif /* RUBY_METHOD_H */
