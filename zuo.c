#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#ifdef WIN32
# include <windows.h>
#else
# include <unistd.h>
# include <errno.h>
#endif

#if 0
# include <assert.h>
# define ASSERT(x) assert(x)
#else
# define ASSERT(x) do { } while (0)
#endif

#define ZUO_LIB_PATH "lib"

typedef long zuo_int_t;
typedef unsigned long zuo_uint_t;
#define ZUO_RECUR_LIMIT 100

#define MIN_HEAP_SIZE (8*1024*1024)

/*======================================================================*/
/* object layouts                                                       */
/*======================================================================*/

typedef enum {
  zuo_singleton_tag,
  zuo_pair_tag,
  zuo_integer_tag,
  zuo_string_tag,
  zuo_symbol_tag,
  zuo_trie_node_tag,
  zuo_variable_tag,
  zuo_primitive_tag,
  zuo_closure_tag,
  zuo_handle_tag,
  zuo_cont_tag,
  zuo_forwarded_tag
} zuo_tag_t;

typedef struct zuo_t {
  int tag;
  /* every subtype must have more to make it at least as
     large as `zuo_forwarded_t` */
} zuo_t;

typedef struct {
  zuo_t obj;
  zuo_t *forward;
} zuo_forwarded_t;

typedef struct {
  zuo_t obj;
  zuo_int_t i;
} zuo_integer_t;

#define ZUO_INT_I(p)  (((zuo_integer_t *)(p))->i)
#define ZUO_UINT_I(p) ((zuo_uint_t)(((zuo_integer_t *)(p))->i))

typedef struct {
  zuo_t obj;
  zuo_t *car;
  zuo_t *cdr;
} zuo_pair_t;

#define ZUO_CAR(p) (((zuo_pair_t *)(p))->car)
#define ZUO_CDR(p) (((zuo_pair_t *)(p))->cdr)

typedef struct {
  zuo_t obj;
  zuo_int_t len;
  unsigned char s[1];
} zuo_string_t;

#define ZUO_STRING_ALLOC_SIZE(len) (((sizeof(zuo_string_t) + (len) + 1 + 3) >> 2) << 2)

#define ZUO_STRING_LEN(obj) (((zuo_string_t *)(obj))->len)
#define ZUO_STRING_PTR(obj) ((char *)&((zuo_string_t *)(obj))->s)

typedef struct {
  zuo_t obj;
  zuo_int_t id;
  zuo_t *str;
} zuo_symbol_t;

#define ZUO_TRIE_BFACTOR_BITS 4
#define ZUO_TRIE_BFACTOR      (1 << ZUO_TRIE_BFACTOR_BITS)
#define ZUO_TRIE_BFACTOR_MASK (ZUO_TRIE_BFACTOR -1)

typedef struct zuo_trie_node_t {
  zuo_t obj;
  zuo_t *key;
  zuo_t *val;
  struct zuo_t* next[ZUO_TRIE_BFACTOR];
} zuo_trie_node_t;

typedef struct {
  zuo_t obj;
  zuo_t *name;
  zuo_t *val;
} zuo_variable_t;

typedef zuo_t *(*zuo_function_proc_t)(zuo_t *data, zuo_t *arguments);

typedef struct {
  zuo_t obj;
  zuo_function_proc_t proc;
  zuo_t *data;
  zuo_int_t arity_mask;
  zuo_t *name;
} zuo_primitive_t;

typedef struct {
  zuo_t obj;
  zuo_t *lambda;
  zuo_t *env;
} zuo_closure_t;

typedef enum {
  zuo_handle_open_fd_in_status,
  zuo_handle_open_fd_out_status,
  zuo_handle_closed_fd_status,
  zuo_handle_process_running_status,
  zuo_handle_process_done_status,
} zuo_handle_status_t;

typedef struct zuo_handle_t {
  zuo_t obj;
  zuo_int_t i;
  zuo_handle_status_t status;
} zuo_handle_t;

typedef enum {
  zuo_apply_cont,
  zuo_begin_cont,
  zuo_let_cont,
  zuo_if_cont
} zuo_cont_tag_t;

typedef struct zuo_cont_t {
  zuo_t obj;
  zuo_cont_tag_t tag;
  zuo_t *data;
  zuo_t *env;
  zuo_t *next;
} zuo_cont_t;

/* singleton values */
static zuo_t *zuo_undefined; /* internal use only */
static zuo_t *zuo_true;
static zuo_t *zuo_false;
static zuo_t *zuo_null;
static zuo_t *zuo_eof;
static zuo_t *zuo_void;

/* symbol table, root environment, and module */
static zuo_t *zuo_intern_table;
static zuo_int_t zuo_symbol_count = 0;
static zuo_t *zuo_top_env;
static zuo_t *zuo_modules;
static zuo_t *zuo_library_path;

/* CEK-style interp--continue registers */
static zuo_t *zuo_interp_e;
static zuo_t *zuo_interp_v;
static zuo_t *zuo_interp_env;
static zuo_t *zuo_interp_k;

/* symbols for kernel core forms */
static zuo_t *zuo_quote_symbol;
static zuo_t *zuo_lambda_symbol;
static zuo_t *zuo_let_symbol;
static zuo_t *zuo_letcc_symbol;
static zuo_t *zuo_begin_symbol;
static zuo_t *zuo_if_symbol;

/* process status table (for Unix) */
static zuo_t *zuo_pid_table;

/* data to save across a GC that's possibly triggered by interp */
static zuo_t *zuo_stash;

/*======================================================================*/
/* memory manager                                                       */
/*======================================================================*/

static zuo_int_t heap_size = MIN_HEAP_SIZE;
static void *to_space = NULL;
static zuo_int_t allocation_offset = 0;
static zuo_int_t total_allocation = 0;
static zuo_int_t gc_threshold = 0;

typedef struct old_space_t {
  void *space;
  struct old_space_t *next;
} old_space_t;
static old_space_t *old_spaces;

static zuo_t *zuo_new(int tag, zuo_int_t size) {
  zuo_t *obj;

  ASSERT(size >= sizeof(zuo_forwarded_t));
  ASSERT(!(size & 0x3));
  
  if (to_space == NULL) {
    to_space = malloc(heap_size);
    gc_threshold = heap_size;
  }

  if (allocation_offset + size > heap_size) {
    old_space_t *new_old_spaces;
    new_old_spaces = malloc(sizeof(old_space_t));
    new_old_spaces->space = to_space;
    new_old_spaces->next = old_spaces;
    old_spaces = new_old_spaces;

    if (heap_size < size)
      heap_size = size * 2;
    to_space = malloc(heap_size);
    allocation_offset = 0;
  }
  
  obj = (zuo_t *)((char *)to_space + allocation_offset);
  obj->tag = tag;
  
  allocation_offset += size;
  total_allocation += size;

  return obj;
}

static zuo_int_t object_size(zuo_t *obj) {
  switch(obj->tag) {
  case zuo_singleton_tag:
    return sizeof(zuo_forwarded_t);
  case zuo_integer_tag:
    return sizeof(zuo_integer_t);
  case zuo_string_tag:
    return ZUO_STRING_ALLOC_SIZE(ZUO_STRING_LEN(obj));
  case zuo_pair_tag:
    return sizeof(zuo_pair_t);
  case zuo_symbol_tag:
    return sizeof(zuo_symbol_t);
  case zuo_trie_node_tag:
    return sizeof(zuo_trie_node_t);
  case zuo_variable_tag:
    return sizeof(zuo_variable_t);
  case zuo_primitive_tag:
    return sizeof(zuo_primitive_t);
  case zuo_closure_tag:
    return sizeof(zuo_closure_t);
  case zuo_handle_tag:
    return sizeof(zuo_handle_t);
  case zuo_cont_tag:
    return sizeof(zuo_cont_t);
  default:
  case zuo_forwarded_tag:
    return sizeof(zuo_forwarded_t);
  }
}

void zuo_update(zuo_t **addr_to_update) {
  zuo_t *obj = *addr_to_update;

  if (obj->tag != zuo_forwarded_tag) {
    zuo_int_t size = object_size(obj);
    zuo_t *new_obj = (zuo_t *)((char *)to_space + allocation_offset);
    allocation_offset += size;

    memcpy(new_obj, obj, size);
    obj->tag = zuo_forwarded_tag;
    ((zuo_forwarded_t *)obj)->forward = new_obj;
  }

  *addr_to_update = ((zuo_forwarded_t *)obj)->forward;
}

static void zuo_trace(zuo_t *obj) {
  switch(obj->tag) {
  case zuo_singleton_tag:    
  case zuo_integer_tag:
  case zuo_string_tag:
  case zuo_handle_tag:
  case zuo_forwarded_tag:
    break;
  case zuo_pair_tag:
    zuo_update(&((zuo_pair_t *)obj)->car);
    zuo_update(&((zuo_pair_t *)obj)->cdr);
    break;
  case zuo_symbol_tag:
    zuo_update(&((zuo_symbol_t *)obj)->str);
    break;
  case zuo_trie_node_tag:
    {
      int i;
      zuo_update(&((zuo_trie_node_t *)obj)->key);
      zuo_update(&((zuo_trie_node_t *)obj)->val);
      for (i = 0; i < ZUO_TRIE_BFACTOR; i++)
        zuo_update(&((zuo_trie_node_t *)obj)->next[i]);
    }
    break;
  case zuo_variable_tag:
    zuo_update(&((zuo_variable_t *)obj)->name);
    zuo_update(&((zuo_variable_t *)obj)->val);
    break;
  case zuo_primitive_tag:
    zuo_update(&((zuo_primitive_t *)obj)->data);
    zuo_update(&((zuo_primitive_t *)obj)->name);
    break;
  case zuo_closure_tag:
    zuo_update(&((zuo_closure_t *)obj)->lambda);
    zuo_update(&((zuo_closure_t *)obj)->env);
    break;
  case zuo_cont_tag:
    zuo_update(&((zuo_cont_t *)obj)->data);
    zuo_update(&((zuo_cont_t *)obj)->env);
    zuo_update(&((zuo_cont_t *)obj)->next);
    break;
  }
}

static void zuo_trace_objects() {
  zuo_int_t trace_offset = 0;

  while (trace_offset < allocation_offset) {
    zuo_t *obj = (zuo_t *)((char *)to_space + trace_offset);
    zuo_trace(obj);
    trace_offset += object_size(obj);
  }
}

static void zuo_collect() {
  void *old_space = to_space;
  old_space_t *old_old_spaces = old_spaces;

  old_spaces = NULL;
  heap_size = total_allocation;
  to_space = malloc(heap_size);
  allocation_offset = 0;

  /* roots */
  zuo_update(&zuo_undefined);
  zuo_update(&zuo_true);
  zuo_update(&zuo_false);
  zuo_update(&zuo_null);
  zuo_update(&zuo_eof);
  zuo_update(&zuo_void);
  
  zuo_update(&zuo_intern_table);
  zuo_update(&zuo_top_env);
  zuo_update(&zuo_modules);
  zuo_update(&zuo_library_path);
  
  zuo_update(&zuo_interp_e);
  zuo_update(&zuo_interp_v);
  zuo_update(&zuo_interp_env);
  zuo_update(&zuo_interp_k);
  
  zuo_update(&zuo_quote_symbol);
  zuo_update(&zuo_lambda_symbol);
  zuo_update(&zuo_let_symbol);
  zuo_update(&zuo_letcc_symbol);
  zuo_update(&zuo_begin_symbol);
  zuo_update(&zuo_if_symbol);

  zuo_update(&zuo_pid_table);
  
  zuo_update(&zuo_stash);

  zuo_trace_objects();
  total_allocation = allocation_offset;
  gc_threshold = total_allocation * 2;
  if (gc_threshold < MIN_HEAP_SIZE)
    gc_threshold = MIN_HEAP_SIZE;

  free(old_space);
  while (old_old_spaces != NULL) {
    old_space_t *next_old_old_spaces = old_old_spaces->next;
    free(old_old_spaces->space);
    free(old_old_spaces);
    old_old_spaces = next_old_old_spaces;
  }
}

static void zuo_check_collect() {
  if (total_allocation >= gc_threshold)
    zuo_collect();
}

/*======================================================================*/
/* object constructors                                                  */
/*======================================================================*/

static zuo_t *zuo_integer(zuo_int_t i) {
  zuo_integer_t *obj = (zuo_integer_t *)zuo_new(zuo_integer_tag, sizeof(zuo_integer_t));
  obj->i = i;
  return (zuo_t *)obj;
}

static zuo_t *zuo_cons(zuo_t *car, zuo_t *cdr) {
  zuo_pair_t *obj = (zuo_pair_t *)zuo_new(zuo_pair_tag, sizeof(zuo_pair_t));
  obj->car = car;
  obj->cdr = cdr;
  return (zuo_t *)obj;
}

static zuo_t *zuo_sized_string(const char *str, zuo_int_t len) {
  zuo_string_t *obj = (zuo_string_t *)zuo_new(zuo_string_tag, ZUO_STRING_ALLOC_SIZE(len));
  obj->len = len;
  memcpy(&obj->s, str, len+1);
  return (zuo_t *)obj;
}

static zuo_t *zuo_string(const char *str) {
  return zuo_sized_string(str, strlen(str));
}

static zuo_t *zuo_trie_node() {
  int i;
  zuo_trie_node_t *obj = (zuo_trie_node_t *)zuo_new(zuo_trie_node_tag, sizeof(zuo_trie_node_t));

  obj->key = zuo_undefined;
  obj->val = zuo_undefined;
  for (i = 0; i < ZUO_TRIE_BFACTOR; i++)
    obj->next[i] = zuo_undefined;

  return (zuo_t *)obj;
}

static zuo_t *zuo_make_symbol_from_string(zuo_t *str) {
  zuo_symbol_t *obj = (zuo_symbol_t *)zuo_new(zuo_symbol_tag, sizeof(zuo_symbol_t));
  obj->id = zuo_symbol_count++;
  obj->str = str;
  return (zuo_t *)obj;
}

static zuo_t *zuo_make_symbol(const char *in_str) {
  return zuo_make_symbol_from_string(zuo_string(in_str));
}

static zuo_t *zuo_symbol(const char *in_str) {
  const unsigned char *str = (const unsigned char *)in_str;
  zuo_int_t i;
  zuo_trie_node_t *node = (zuo_trie_node_t *)zuo_intern_table;

  for (i = 0; str[i]; i++) {
    int c = str[i], lo = c & ZUO_TRIE_BFACTOR_MASK, hi = c >> ZUO_TRIE_BFACTOR_BITS;
    if (node->next[lo] == zuo_undefined)
      node->next[lo] = zuo_trie_node();
    node = (zuo_trie_node_t *)node->next[lo];
    if (node->next[hi] == zuo_undefined)
      node->next[hi] = zuo_trie_node();
    node = (zuo_trie_node_t *)node->next[hi];
  }

  if (node->val == zuo_undefined)
    node->val = zuo_make_symbol(in_str);
  /* the symbol table doesn't use the `key` field */
  
  if (((zuo_int_t)node->val) & 0x1) abort();
    
  return node->val;
}

static zuo_t *zuo_variable(zuo_t *name) {
  zuo_variable_t *obj = (zuo_variable_t *)zuo_new(zuo_variable_tag, sizeof(zuo_variable_t));
  obj->name = name;
  obj->val = zuo_undefined;
  return (zuo_t *)obj;
}

static zuo_t *zuo_primitive(zuo_function_proc_t proc, zuo_t *data, zuo_int_t arity_mask, zuo_t *name) {
  zuo_primitive_t *obj = (zuo_primitive_t *)zuo_new(zuo_primitive_tag, sizeof(zuo_primitive_t));
  obj->proc = proc;
  obj->data = data;
  obj->arity_mask = arity_mask;
  obj->name = name;
  return (zuo_t *)obj;
}

static zuo_t *zuo_closure(zuo_t *lambda, zuo_t *env) {
  zuo_closure_t *obj = (zuo_closure_t *)zuo_new(zuo_closure_tag, sizeof(zuo_closure_t));
  obj->lambda = lambda;
  obj->env = env;
  return (zuo_t *)obj;
}

static zuo_t *zuo_handle(zuo_int_t i, zuo_handle_status_t status){
  zuo_handle_t *obj = (zuo_handle_t *)zuo_new(zuo_handle_tag, sizeof(zuo_handle_t));
  obj->i = i;
  obj->status = status;
  return (zuo_t *)obj;
}

static zuo_t *zuo_cont(zuo_cont_tag_t tag, zuo_t *data, zuo_t *env, zuo_t *next) {
  zuo_cont_t *obj = (zuo_cont_t *)zuo_new(zuo_cont_tag, sizeof(zuo_cont_t));
  obj->tag = tag;
  obj->data = data;
  obj->env = env;
  obj->next = next;
  return (zuo_t *)obj;
}

/*======================================================================*/
/* tries                                                                */
/*======================================================================*/

/* A trie is used for the symbol table and for "hash tables" that are
   symbol-keyed persistent maps. */

static zuo_t *trie_lookup(zuo_t *trie, zuo_int_t id) {
  ASSERT(trie->tag == zuo_trie_node_tag);

  while (id > 0) {
    trie = ((zuo_trie_node_t *)trie)->next[id & ZUO_TRIE_BFACTOR_MASK];
    if (trie == zuo_undefined) return zuo_undefined;
    id = id >> ZUO_TRIE_BFACTOR_BITS;
  }

  return ((zuo_trie_node_t *)trie)->val;
}

static zuo_t *zuo_trie_lookup(zuo_t *trie, zuo_t *sym) {
  return trie_lookup(trie, ((zuo_symbol_t *)sym)->id);
}

static void trie_set(zuo_t *trie, zuo_int_t id, zuo_t *key, zuo_t *val) {
  while (id > 0) {
    zuo_t *next = ((zuo_trie_node_t *)trie)->next[id & ZUO_TRIE_BFACTOR_MASK];
    if (next == zuo_undefined) {
      next = zuo_trie_node();
      ((zuo_trie_node_t *)trie)->next[id & ZUO_TRIE_BFACTOR_MASK] = next;
      trie = next;
    } else
      trie = next;
    id = id >> ZUO_TRIE_BFACTOR_BITS;
  }
  ((zuo_trie_node_t *)trie)->key = key;
  ((zuo_trie_node_t *)trie)->val = val;
}

static void zuo_trie_set(zuo_t *trie, zuo_t *sym, zuo_t *val) {
  trie_set(trie, ((zuo_symbol_t *)sym)->id, sym, val);
}

static zuo_t *trie_extend(zuo_t *trie, zuo_int_t id, zuo_t *key, zuo_t *val) {
  zuo_trie_node_t *new_trie;

  if (trie == zuo_undefined) {
    new_trie = (zuo_trie_node_t *)zuo_trie_node();
    trie = (zuo_t *)new_trie;
  } else {
    new_trie = (zuo_trie_node_t *)zuo_new(zuo_trie_node_tag, sizeof(zuo_trie_node_t));
    memcpy(&new_trie->next, &((zuo_trie_node_t *)trie)->next, ZUO_TRIE_BFACTOR * sizeof(zuo_t *));
  }

  if (id > 0) {
    int i = id & ZUO_TRIE_BFACTOR_MASK;
    new_trie->next[i] = trie_extend(((zuo_trie_node_t *)trie)->next[i], id >> ZUO_TRIE_BFACTOR_BITS, key, val);
    new_trie->key = ((zuo_trie_node_t *)trie)->key;
    new_trie->val = ((zuo_trie_node_t *)trie)->val;
  } else {
    new_trie->key = key;
    new_trie->val = val;
  }

  return (zuo_t *)new_trie;
}

static zuo_t *zuo_trie_extend(zuo_t *trie, zuo_t *sym, zuo_t *val) {
  return trie_extend(trie, ((zuo_symbol_t *)sym)->id, sym, val);
}

static zuo_t *zuo_trie_keys(zuo_t *trie_in, zuo_t *accum) {
  int i;
  zuo_trie_node_t *trie = (zuo_trie_node_t *)trie_in;

  if (trie->key != zuo_undefined)
    accum = zuo_cons(trie->key, accum);

  for (i = 0; i < ZUO_TRIE_BFACTOR; i++) {
    if (trie->next[i] != zuo_undefined)
      accum = zuo_trie_keys(trie->next[i], accum);
  }

  return accum;
}

/*======================================================================*/
/* printing                                                             */
/*======================================================================*/

typedef enum {
  zuo_print_mode,
  zuo_write_mode,
  zuo_display_mode
} zuo_print_mode_t;

typedef struct {
  char *s;
  zuo_int_t size, len;
} zuo_out_t;

static void out_init(zuo_out_t *out) {
  out->size = 32;
  out->len = 0;
  out->s = malloc(out->size);
}

static void out_done(zuo_out_t *out) {
  free(out->s);
}

static void out_char(zuo_out_t *out, int c) {
  if (out->len == out->size) {
    zuo_int_t new_size = out->size * 2;
    char *s = malloc(new_size);
    memcpy(s, out->s, out->size);
    free(out->s);
    out->s = s;
    out->size = new_size;
  }

  out->s[out->len++] = c;
}

static void out_string(zuo_out_t *out, const char *s) {
  while (*s != 0) {
    out_char(out, *s);
    s++;
  }
}

static void zuo_out(zuo_out_t *out, zuo_t *obj, int depth, zuo_print_mode_t mode) {
  if (obj == zuo_undefined)
    out_string(out, "#<undefined>");
  else if (obj == zuo_null)
    out_string(out, "'()");
  else if (obj == zuo_false)
    out_string(out, "#f");
  else if (obj == zuo_true)
    out_string(out, "#t");
  else if (obj == zuo_eof)
    out_string(out, "#<eof>");
  else if (obj == zuo_void)
    out_string(out, "#<void>");
  else if (obj->tag == zuo_integer_tag) {
    zuo_int_t i = ZUO_INT_I(obj), n, w;
    if (i < 0) {
      out_char(out, '-');
      i = -i;
    }
    for (n = 1, w = 1; i >= n; n *= 10, w++);
    while (n > 1) {
      n /= 10;
      out_char(out, '0' + (i / n));
      i = i - ((i / n) * n);
    }
  } else if (obj->tag == zuo_string_tag) {
    zuo_string_t *str = (zuo_string_t *)obj;
    if (mode == zuo_display_mode) {
      int i;
      for (i = 0; i < str->len; i++)
        out_char(out, str->s[i]);
    } else {
      int i;
      out_string(out, "\"");
      for (i = 0; i < str->len; i++) {
        int c = str->s[i];
        if ((c == '"') || (c == '\\')) {
          out_char(out, '\\');
          out_char(out, c);
        } else if (c == '\n') {
          out_char(out, '\\');
          out_char(out, 'n');
        } else if (c == '\r') {
          out_char(out, '\\');
          out_char(out, 'r');
        } else if (isprint(c)) {
          out_char(out, c);
        } else {
          out_char(out, '\\');
          out_char(out, 'x');
          if (c > 0x90)
            out_char(out, 'a' + ((c >> 4) - 10));
          else
            out_char(out, '0' + (c >> 4));
          if ((c & 0xF) > 0x09)
            out_char(out, 'a' + ((c & 0xF) - 10));
          else
            out_char(out, '0' + (c & 0xF));
        }
      }
      out_string(out, "\"");
    }
  } else if (obj->tag == zuo_symbol_tag) {
    if (mode == zuo_print_mode)
      out_char(out, '\'');
    zuo_out(out, ((zuo_symbol_t *)obj)->str, depth, zuo_display_mode);
  } else if (depth >= ZUO_RECUR_LIMIT) {
    out_string(out, "...");
  } else if (obj->tag == zuo_pair_tag) {
    zuo_pair_t *p = (zuo_pair_t *)obj;
    out_char(out, '(');
    if (mode == zuo_print_mode) {
      zuo_t *p2 = (zuo_t *)p;
      while (p2->tag == zuo_pair_tag)
        p2 = ((zuo_pair_t *)p2)->cdr;
      if (p2 == zuo_null)
        out_string(out, "list ");
      else
        out_string(out, "list* ");
    }
    zuo_out(out, p->car, depth+1, mode);
    while (p->cdr->tag == zuo_pair_tag) {
      p = (zuo_pair_t *)p->cdr;
      out_char(out, ' ');
      zuo_out(out, p->car, depth+1, mode);
    }
    if (p->cdr != zuo_null) {
      if (mode != zuo_print_mode) {
        out_char(out, ' ');
        out_char(out, '.');
      }
      out_char(out, ' ');
      zuo_out(out, p->cdr, depth+1, mode);
    }
    out_char(out, ')');
  } else if (obj->tag == zuo_primitive_tag) {
    out_string(out, "#<function:");
    zuo_out(out, ((zuo_primitive_t *)obj)->name, depth+1, zuo_display_mode);
    out_string(out, ">");
  } else if (obj->tag == zuo_closure_tag) {
    zuo_t *dd = ZUO_CDR(ZUO_CDR(((zuo_closure_t *)obj)->lambda));
    out_string(out, "#<function");
    if (ZUO_CAR(dd)->tag == zuo_string_tag) {
      out_string(out, ":");
      zuo_out(out, ZUO_CAR(dd), depth+1, zuo_display_mode);
    }
    out_string(out, ">");
  } else if (obj->tag == zuo_trie_node_tag) {
    out_string(out, "#<hash>");
  } else if (obj->tag == zuo_handle_tag) {
    out_string(out, "#<handle>");
  } else if (obj->tag == zuo_cont_tag) {
    out_string(out, "#<continuation>");
  } else {
    out_string(out, "#<garbage>");
  }
}

static void zuo_fout(FILE *fout, zuo_t *obj, zuo_print_mode_t mode) {
  zuo_out_t out;
  out_init(&out);
  zuo_out(&out, obj, 0, mode);
  fwrite(out.s, 1, out.len, fout);
  out_done(&out);
}

static zuo_t *zuo_to_string(zuo_t *objs, zuo_print_mode_t mode) {
  zuo_out_t out;
  zuo_t *str;
  out_init(&out);

  while (objs->tag == zuo_pair_tag) {
    zuo_out(&out, ZUO_CAR(objs), 0, mode);
    objs = ZUO_CDR(objs);
    if ((mode != zuo_display_mode) && (objs != zuo_null))
      out_char(&out, ' ');
  }
  if (objs != zuo_null)
    zuo_out(&out, objs, 0, mode);

  out_char(&out, 0);
  
  str = zuo_string(out.s);
  out_done(&out);
  return str;
}

static void zuo_fprint(FILE *out, zuo_t *obj) {
  zuo_fout(out, obj, zuo_print_mode);
}

static void zuo_fwrite(FILE *out, zuo_t *obj) {
  zuo_fout(out, obj, zuo_write_mode);
}

static void zuo_fdisplay(FILE *out, zuo_t *obj) {
  zuo_fout(out, obj, zuo_display_mode);
}

static void zuo_fail(const char *str) {
  fprintf(stderr, "%s\n", str);
  exit(1);
}

#ifndef WIN32
static void zuo_fail_errno(const char *str) {
  fprintf(stderr, "%s (%d)\n", str, errno);
  exit(1);
}
#endif

static void zuo_fail1w(const char *who, const char *str, zuo_t *obj) {
  if (who != NULL)
    fprintf(stderr, "%s: ", who);
  fprintf(stderr, "%s: ", str);
  zuo_fprint(stderr, obj);
  fprintf(stderr, "\n");
  exit(1);
}

static void zuo_fail1(const char *str, zuo_t *obj) {
  zuo_fail1w(NULL, str, obj);
}

static void check_string(const char *who, zuo_t *obj) {
  if (obj->tag != zuo_string_tag)
    zuo_fail1w(who, "not a string", obj);
}

static void check_symbol(const char *who, zuo_t *obj) {
  if (obj->tag != zuo_symbol_tag)
    zuo_fail1w(who, "not a symbol", obj);
}

static void check_integer(const char *who, zuo_t *n) {
  if (n->tag != zuo_integer_tag)
    zuo_fail1w(who, "not an integer", n);
}

static void check_hash(const char *who, zuo_t *obj) {
  if (obj->tag != zuo_trie_node_tag)
    zuo_fail1w(who, "not a hash table", obj);
}

/*======================================================================*/
/* reading                                                              */
/*======================================================================*/

static const char *symbol_chars = "~!@#$%^&*-_=+:<>?/";
static zuo_t *zuo_in(const unsigned char *s, zuo_int_t *_o, int depth);

static void skip_whitespace(unsigned const char *s, zuo_int_t *_o, int depth) {
  while (1) {
    while (isspace(s[*_o]))
      (*_o)++;
    if (s[*_o] == ';') {
      while ((s[*_o] != '\n') && (s[*_o] != 0))
        (*_o)++;
    } else if (s[*_o] == '#' && s[(*_o) + 1] == ';') {
      zuo_t *discard;
      (*_o) += 2;
      discard = zuo_in(s, _o, depth+1);
      if (discard == zuo_eof)
        zuo_fail("read: end of file after comment hash-semicolon");      
    } else
      break;
  }
}

static int hex_value(int c) {
  if ((c >= '0') && (c <= '9'))
    return c - '0';
  if ((c >= 'a') && (c <= 'f'))
    return c - 'a' + 10;
  if ((c >= 'A') && (c <= 'F'))
    return c - 'A' + 10;
  zuo_fail("read: bad hex digit");
  return -1;
}

static int peek_input(const unsigned char *s, zuo_int_t *_o, const char *want) {
  int i, c;
  for (i = 0; want[i]; i++) {
    if (s[(*_o)+i] != want[i])
      return 0;
  }
  c = s[(*_o)+i];
  if (isdigit(c) || isalpha(c) || strchr(symbol_chars, c))
    return 0;
  return 1;
}

static zuo_t *zuo_in(const unsigned char *s, zuo_int_t *_o, int depth) {
  int c;

  if (depth >= ZUO_RECUR_LIMIT)
    zuo_fail("read: too nested");
  
  skip_whitespace(s, _o, depth);
  c = s[*_o];
  if (c == 0)
    return zuo_eof;
  else if ((c == '(') || (c == '[')) {
    int closer = ((c == '(') ? ')' : ']');
    zuo_t *car, *top_p, *cdr;
    zuo_pair_t *p;
    (*_o)++;
    skip_whitespace(s, _o, depth);
    if (s[*_o] == closer) {
      (*_o)++;
      return zuo_null;
    }
    car = zuo_in(s, _o, depth+1);
    top_p = zuo_cons(car, zuo_null);
    p = (zuo_pair_t *)top_p;
    while (1) {
      skip_whitespace(s, _o, depth);
      if (s[*_o] == 0) {
        zuo_fail("read: missing closer");
      } else if (s[*_o] == '.') {
        zuo_t *cdr;
        (*_o)++;
        cdr = zuo_in(s, _o, depth+1);
        p->cdr = cdr;
      } else if (s[*_o] == closer) {
        (*_o)++;
        break;
      } else {
        zuo_t *cadr, *cdr;
        cadr = zuo_in(s, _o, depth+1);
        cdr = zuo_cons(cadr, zuo_null);
        p->cdr = cdr;
        p = (zuo_pair_t *)cdr;
      }
    }
    return top_p;
  } else if ((c == ')') || (c == ']')) {
    zuo_fail("read: unbalanced closer");
    return zuo_undefined;
  } else if (c == '#') {
    (*_o)++;
    if (peek_input(s, _o, "true")) {
      (*_o) += 4;
      return zuo_true;
    } else if (peek_input(s, _o, "false")) {
      (*_o) += 5;
      return zuo_false;
    } else if (peek_input(s, _o, "t")) {
      (*_o) += 1;
      return zuo_true;
    } else if (peek_input(s, _o, "f")) {
      (*_o) += 1;
      return zuo_false;
    } else if (peek_input(s, _o, ";")) {
      zuo_t *discard;
      (*_o) += 1;
      discard = zuo_in(s, _o, depth+1);
      if (discard == zuo_eof)
        zuo_fail("read: end of file after comment hash-semicolon");
      return zuo_in(s, _o, depth+1);
    } else {
      zuo_fail("read: bad hash mark");
      return zuo_undefined;
    }
  } else if (isdigit(c)) {
    zuo_uint_t n = c - '0';
    (*_o)++;
    while (isdigit(s[*_o])) {
      zuo_uint_t new_n = (10 * n) + (s[*_o] - '0');
      if (new_n < n)
        zuo_fail("read: integer overflow");
      n = new_n;
      (*_o)++;
    }
    if ((zuo_int_t)n < 0)
      zuo_fail("read: integer overflow");
    return zuo_integer((zuo_int_t)n);
  } else if ((c == '-') && isdigit(s[(*_o)+1])) {
    zuo_t *n;
    (*_o)++;
    n = zuo_in(s, _o, depth);
    return zuo_integer(-ZUO_INT_I(n));
  } else if (c == '\'') {
    zuo_t *v;
    (*_o)++;
    v = zuo_in(s, _o, depth+1);
    if (v == zuo_eof)
      zuo_fail("read: end of file after quote");
    return zuo_cons(zuo_symbol("quote"), zuo_cons(v, zuo_null));
  } else if (c == '`') {
    zuo_t *v;
    (*_o)++;
    v = zuo_in(s, _o, depth+1);
    if (v == zuo_eof)
      zuo_fail("read: end of file after quasiquote");
    return zuo_cons(zuo_symbol("quasiquote"), zuo_cons(v, zuo_null));
  } else if (c == ',') {
    zuo_t *v;
    (*_o)++;
    v = zuo_in(s, _o, depth+1);
    if (v == zuo_eof)
      zuo_fail("read: end of file after unquote");
    return zuo_cons(zuo_symbol("unquote"), zuo_cons(v, zuo_null));
  } else if (isalpha(c) || strchr(symbol_chars, c)) {
    zuo_t *sym;
    zuo_int_t start = *_o, len;
    char *s2;
    while (1) {
      c = s[*_o];
      if (isalpha(c) || isdigit(c) || strchr(symbol_chars, c))
        (*_o)++;
      else
        break;
    }
    len = (*_o) - start;
    s2 = malloc(len+1);
    memcpy(s2, s + start, len);
    s2[len] = 0;
    sym = zuo_symbol(s2);
    free(s2);
    return sym;
  } else if (c == '"') {
    zuo_t *obj;
    zuo_int_t sz = 32;
    zuo_int_t len = 0;
    char *s2 = malloc(sz);
    (*_o)++;
    while (1) {
      if (sz == len) {
        char *s3 = malloc(sz * 2);
        memcpy(s3, s2, sz);
        free(s2);
        s2 = s3;
        sz = sz * 2;
      }
      c = s[*_o];
      if (c == 0) {
        zuo_fail("read: missing closing doublequote");
      } else if (c == '"') {
        (*_o)++;
        break;
      } else if (c == '\\') {
        int c2 = s[(*_o)+1];
        if ((c2 == '\\') || (c2 == '"')) {
          s2[len++] = c2;
          (*_o) += 2;
        } else if (c2 == 'n') {
          s2[len++] = '\n';
          (*_o) += 2;
        } else if (c2 == 'x') {
          s2[len++] = (hex_value(s[(*_o)+2]) << 8) + hex_value(s[(*_o)+3]);
          (*_o) += 4;
        } else
          zuo_fail("read: bad character after backslash");
      } else if (c == '\n') {
        zuo_fail("read: newline in string literal");
      } else if (c == '\r') {
        zuo_fail("read: carriage return in string literal");
      } else {
        s2[len++] = c;
        (*_o)++;
      }
    }
    s2[len] = 0;
    obj = zuo_string(s2);
    free(s2);
    return obj;
  } else {
    char s[2];
    s[0] = c;
    s[1] = 0;
    zuo_fail1w("read", "unrecognized character", zuo_string(s));
    return zuo_null;
  }
}

static zuo_t *zuo_read_one_str(const char *s) {
  zuo_int_t o = 0;
  zuo_t *obj = zuo_in((const unsigned char *)s, &o, 0);
  skip_whitespace((const unsigned char *)s, &o, 0);
  if (s[o] != 0)
    zuo_fail("read: extra input after S-expression");
  return obj;
}

static zuo_t *zuo_read_one(zuo_t *obj) {
  check_string("read-from-string", obj);
  return zuo_read_one_str(ZUO_STRING_PTR(obj));
}

static zuo_t *zuo_read_all_str(const char *s) {
  zuo_int_t o = 0;
  zuo_t *first = zuo_null, *last, *p;

  while (1) {
    zuo_t *obj = zuo_in((const unsigned char *)s, &o, 0);
    p = zuo_cons(obj, zuo_null);
    if (first == zuo_null)
      first = last = p;
    else {
      ((zuo_pair_t *)last)->cdr = p;
      last = p;
    }
    skip_whitespace((const unsigned char *)s, &o, 0);
    if (s[o] == 0)
      break;
  }
  
  return first;
}

static zuo_t *zuo_read_all(zuo_t *obj) {
  check_string("read-from-string-all", obj);
  return zuo_read_all_str(ZUO_STRING_PTR(obj));
}

static int zuo_is_symbol_module_char(int c) {
  return isalpha(c) || isdigit(c) || strchr("-_+/", c);
}

static char *zuo_read_language(const char *s_in, zuo_int_t *_post) {
  const unsigned char *s = (const unsigned char *)s_in;
  zuo_int_t o = 0, i, j;
  const char *expect = "#lang ";
  char *r;

  skip_whitespace(s, &o, 0);
  for (i = 0; expect[i]; i++) {
    if (s[o+i] != expect[i])
      zuo_fail("read: expected #lang followed by a space");
  }
  for (j = 0; 1; j++) {
    int c = s[o+i+j];
    if (!zuo_is_symbol_module_char(c))
      break;
  }
  if (!j || !((s[o+i+j] == 0) || isspace(s[o+i+j])))
    zuo_fail("read: expected symbolic module path after #lang");

  r = malloc(j+1);
  memcpy(r, s+o+i, j);
  r[j] = 0;

  *_post = o+i+j;

  return r;
}

/*======================================================================*/
/* primitive wrapper/dispatchers                                        */
/*======================================================================*/

static zuo_t *call_primitive0(zuo_t *data, zuo_t *args) {
  return ((zuo_t *(*)())((void *)ZUO_INT_I(data)))();
}

static zuo_t *zuo_primitive0(zuo_t *(*f)(), zuo_t *name) {
  return zuo_primitive(call_primitive0, zuo_integer((zuo_int_t)f), (1 << 0), name);
}

static zuo_t *call_primitive1(zuo_t *data, zuo_t *args) {
  return ((zuo_t *(*)(zuo_t *))((void *)ZUO_INT_I(data)))(ZUO_CAR(args));
}

static zuo_t *zuo_primitive1(zuo_t *(*f)(zuo_t *), zuo_t *name) {
  return zuo_primitive(call_primitive1, zuo_integer((zuo_int_t)f), (1 << 1), name);
}

static zuo_t *call_primitive2(zuo_t *data, zuo_t *args) {
  return ((zuo_t *(*)(zuo_t *, zuo_t *))((void *)ZUO_INT_I(data)))(ZUO_CAR(args), ZUO_CAR(ZUO_CDR(args)));
}

static zuo_t *zuo_primitive2(zuo_t *(*f)(zuo_t *, zuo_t *), zuo_t *name) {
  return zuo_primitive(call_primitive2, zuo_integer((zuo_int_t)f), (1 << 2), name);
}

static zuo_t *call_primitive3(zuo_t *data, zuo_t *args) {
  return ((zuo_t *(*)(zuo_t *, zuo_t *, zuo_t *))((void *)ZUO_INT_I(data)))(ZUO_CAR(args),
                                                                            ZUO_CAR(ZUO_CDR(args)),
                                                                            ZUO_CAR(ZUO_CDR(ZUO_CDR(args))));
}

static zuo_t *zuo_primitive3(zuo_t *(*f)(zuo_t *, zuo_t *, zuo_t *), zuo_t *name) {
  return zuo_primitive(call_primitive3, zuo_integer((zuo_int_t)f), (1 << 3), name);
}

static zuo_t *call_primitiveN(zuo_t *data, zuo_t *args) {
  return ((zuo_t *(*)(zuo_t *))((void *)ZUO_INT_I(data)))(args);
}

static zuo_t *zuo_primitiveN(zuo_t *(*f)(zuo_t *), zuo_t *name) {
  return zuo_primitive(call_primitiveN, zuo_integer((zuo_int_t)f), -1, name);
}

/*======================================================================*/
/* object primitives                                                    */
/*======================================================================*/

static zuo_t *zuo_pair_p(zuo_t *obj) {
  return (obj->tag == zuo_pair_tag) ? zuo_true : zuo_false;
}

static zuo_t *zuo_null_p(zuo_t *obj) {
  return (obj == zuo_null) ? zuo_true : zuo_false;
}

static zuo_t *zuo_integer_p(zuo_t *obj) {
  return (obj->tag == zuo_integer_tag) ? zuo_true : zuo_false;
}

static zuo_t *zuo_string_p(zuo_t *obj) {
  return (obj->tag == zuo_string_tag) ? zuo_true : zuo_false;
}

static zuo_t *zuo_symbol_p(zuo_t *obj) {
  return (obj->tag == zuo_symbol_tag) ? zuo_true : zuo_false;
}

static zuo_t *zuo_procedure_p(zuo_t *obj) {
  return ((obj->tag == zuo_primitive_tag) || (obj->tag == zuo_closure_tag)) ? zuo_true : zuo_false;
}

static zuo_t *zuo_hash_p(zuo_t *obj) {
  return (obj->tag == zuo_trie_node_tag) ? zuo_true : zuo_false;
}

static zuo_t *zuo_list_p(zuo_t *obj) {
  while (obj->tag == zuo_pair_tag)
    obj = ((zuo_pair_t *)obj)->cdr;
  return (obj == zuo_null) ? zuo_true : zuo_false;
}

static zuo_t *zuo_car(zuo_t *obj) {
  if (obj->tag != zuo_pair_tag)
    zuo_fail1w("car", "not a pair", obj);
  return ((zuo_pair_t *)obj)->car;
}

static zuo_t *zuo_cdr(zuo_t *obj) {
  if (obj->tag != zuo_pair_tag)
    zuo_fail1w("cdr", "not a pair", obj);
  return ((zuo_pair_t *)obj)->cdr;
}

static zuo_int_t zuo_length_int(zuo_t *in_l) {
  zuo_t *l = in_l;
  zuo_int_t len = 0;

  while (l->tag == zuo_pair_tag) {
    l = ((zuo_pair_t *)l)->cdr;
    len++;
  }

  if (l != zuo_null)
    zuo_fail1w("length", "not a list", in_l);

  return len;
}

static zuo_t *zuo_length(zuo_t *in_l) {
  return zuo_integer(zuo_length_int(in_l));
}

static zuo_t *zuo_reverse(zuo_t *in_l) {
  zuo_t *l = in_l, *r = zuo_null;
  while (l->tag == zuo_pair_tag) {
    r = zuo_cons(zuo_car(l), r);
    l = zuo_cdr(l);
  }

  if (l != zuo_null)
    zuo_fail1w("reverse", "not a list", in_l);

  return r;
}

static zuo_t *zuo_string_length(zuo_t *obj) {
  check_string("string-length", obj);
  return zuo_integer(ZUO_STRING_LEN(obj));
}

static zuo_t *zuo_string_ref(zuo_t *obj, zuo_t *i) {
  const char *who = "string-length";
  zuo_int_t idx;
  check_string(who, obj);
  check_integer(who, i);
    zuo_fail1w(who, "not an integer", i);
  idx = ZUO_INT_I(i);
  if ((idx < 0) || (idx >= ZUO_STRING_LEN(obj)))
    zuo_fail1w(who, "index out of bound for string", i);
  return zuo_integer(((zuo_string_t *)obj)->s[idx]);
}

static zuo_t *zuo_substring(zuo_t *obj, zuo_t *start_i, zuo_t *end_i) {
  const char *who = "string-length";
  zuo_int_t s_idx, e_idx, len;
  check_string(who, obj);
  check_integer(who, start_i);
  check_integer(who, end_i);
  s_idx = ZUO_INT_I(start_i);
  e_idx = ZUO_INT_I(end_i);
  len = ZUO_STRING_LEN(obj);
  if ((s_idx < 0) || (s_idx > len))
    zuo_fail1w(who, "starting index out of bound for string", start_i);
  if ((e_idx < 0) || (e_idx > len))
    zuo_fail1w(who, "ending index out of bound for string", end_i);
  if (e_idx < s_idx)
    zuo_fail1w(who, "ending index less than starting index", end_i);
  return zuo_sized_string((const char *)&((zuo_string_t *)obj)->s[s_idx], e_idx - s_idx);
}

static zuo_t *zuo_string_to_symbol(zuo_t *obj) {
  check_string("string->symbol", obj);
  return zuo_symbol(ZUO_STRING_PTR(obj));
}

static zuo_t *zuo_string_to_uninterned_symbol(zuo_t *obj) {
  check_string("string->uninterned-symbol", obj);
  return zuo_make_symbol_from_string(obj);
}

static zuo_t *zuo_hash(zuo_t *args) {
  zuo_t *l, *ht;

  for (l = args; l->tag == zuo_pair_tag; l = zuo_cdr(zuo_cdr(l))) {
    if ((zuo_car(l)->tag != zuo_symbol_tag)
        || (zuo_cdr(l)->tag != zuo_pair_tag))
      break;
  }
  if (l != zuo_null)
    zuo_fail1w("hash", "symbol arguments interleaved with values", args);

  ht = zuo_trie_node();
  for (l = args; l->tag == zuo_pair_tag; l = zuo_cdr(zuo_cdr(l)))
    ht = zuo_trie_extend(ht, zuo_car(l), zuo_car(zuo_cdr(l)));

  return ht;
}

static zuo_t *zuo_hash_ref(zuo_t *ht, zuo_t *sym, zuo_t *defval) {
  zuo_t *v;
  const char *who = "hash-ref";
  check_hash(who, ht);
  check_symbol(who, sym);
  v = zuo_trie_lookup(ht, sym);
  if (v == zuo_undefined) v = defval;
  return v;
}

static zuo_t *zuo_hash_set(zuo_t *ht, zuo_t *sym, zuo_t *val) {
  const char *who = "hash-set";
  check_hash(who, ht);
  check_symbol(who, sym);
  return zuo_trie_extend(ht, sym, val);
}

static zuo_t *zuo_hash_keys(zuo_t *ht) {
  check_hash("hash-keys", ht);
  return zuo_trie_keys(ht, zuo_null);
}

static void check_ints(zuo_t *n, zuo_t *m, const char *who) {
  check_integer(who, n);
  check_integer(who, m);
}

static zuo_t *zuo_add(zuo_t *n, zuo_t *m) {
  check_ints(n, m, "+");
  return zuo_integer((zuo_int_t)(ZUO_UINT_I(n) + ZUO_UINT_I(m)));
}

static zuo_t *zuo_subtract(zuo_t *n, zuo_t *m) {
  check_ints(n, m, "-");
  return zuo_integer((zuo_int_t)(ZUO_UINT_I(n) - ZUO_UINT_I(m)));
}

static zuo_t *zuo_multiply(zuo_t *n, zuo_t *m) {
  check_ints(n, m, "*");
  return zuo_integer((zuo_int_t)(ZUO_UINT_I(n) * ZUO_UINT_I(m)));
}

static zuo_t *zuo_quotient(zuo_t *n, zuo_t *m) {
  check_ints(n, m, "quotient");
  return zuo_integer((zuo_int_t)(ZUO_UINT_I(n) / ZUO_UINT_I(m)));
}

static zuo_t *zuo_modulo(zuo_t *n, zuo_t *m) {
  check_ints(n, m, "modulo");
  return zuo_integer((zuo_int_t)(ZUO_UINT_I(n) % ZUO_UINT_I(m)));
}

static zuo_t *zuo_not(zuo_t *obj) {
  return (obj == zuo_false) ? zuo_true : zuo_false;
}

static zuo_t *zuo_eql(zuo_t *n, zuo_t *m) {
  check_ints(n, m, "=");
  return (ZUO_UINT_I(n) == ZUO_UINT_I(m)) ? zuo_true : zuo_false;
}

static zuo_t *zuo_lt(zuo_t *n, zuo_t *m) {
  check_ints(n, m, "<");
  return (ZUO_UINT_I(n) < ZUO_UINT_I(m)) ? zuo_true : zuo_false;
}

static zuo_t *zuo_le(zuo_t *n, zuo_t *m) {
  check_ints(n, m, "<=");
  return (ZUO_UINT_I(n) <= ZUO_UINT_I(m)) ? zuo_true : zuo_false;
}

static zuo_t *zuo_ge(zuo_t *n, zuo_t *m) {
  check_ints(n, m, ">=");
  return (ZUO_UINT_I(n) >= ZUO_UINT_I(m)) ? zuo_true : zuo_false;
}

static zuo_t *zuo_gt(zuo_t *n, zuo_t *m) {
  check_ints(n, m, ">");
  return (ZUO_UINT_I(n) > ZUO_UINT_I(m)) ? zuo_true : zuo_false;
}

static zuo_t *zuo_eq(zuo_t *n, zuo_t *m) {
  return (n == m) ? zuo_true : zuo_false;
}

static zuo_t *zuo_string_eql(zuo_t *n, zuo_t *m) {
  const char *who = "string=?";
  check_string(who, n);
  check_string(who, m);
  return (((ZUO_STRING_LEN(n) == ZUO_STRING_LEN(m))
           && !memcmp(ZUO_STRING_PTR(n), ZUO_STRING_PTR(m), ZUO_STRING_LEN(n)))
          ? zuo_true
          : zuo_false);
}

static zuo_t *zuo_tilde_v(zuo_t *objs) {
  return zuo_to_string(objs, zuo_print_mode);
}
    
static zuo_t *zuo_tilde_s(zuo_t *objs) {
  return zuo_to_string(objs, zuo_write_mode);
}

static zuo_t *zuo_tilde_a(zuo_t *objs) {
  return zuo_to_string(objs, zuo_display_mode);
}

static zuo_t *zuo_display(zuo_t *obj) {
  zuo_fdisplay(stdout, obj);
  return zuo_void;
}

static zuo_t *zuo_error(zuo_t *objs) {
  if ((objs->tag == zuo_pair_tag)
      && (zuo_car(objs)->tag == zuo_string_tag)) {
    zuo_fdisplay(stderr, zuo_car(objs));
    objs = zuo_cdr(objs);
    if (objs != zuo_null) fprintf(stderr, ": ");
  }
  zuo_fdisplay(stderr, zuo_tilde_v(objs));
  fprintf(stderr, "\n");
  exit(1);
  return zuo_undefined;
}

static zuo_t *zuo_list(zuo_t *objs) {
  return objs;
}

static zuo_t *zuo_append(zuo_t *objs) {
  zuo_t *first = zuo_null, *last = NULL, *p;
  zuo_t *l = objs, *a;
  while ((l->tag == zuo_pair_tag)
         && (zuo_cdr(l)->tag == zuo_pair_tag)) {
    a = zuo_car(l);
    while (a->tag == zuo_pair_tag) {
      p = zuo_cons(zuo_car(a), zuo_null);
      if (last)
        ((zuo_pair_t *)last)->cdr = p;
      else
        first = p;
      last = p;
      a = zuo_cdr(a);
    }
    if (a != zuo_null)
      zuo_fail1w("append", "not a list", zuo_car(l));
    l = zuo_cdr(l);
  }

  if (l->tag == zuo_pair_tag) {
    if (last)
      ((zuo_pair_t *)last)->cdr = zuo_car(l);
    else
      first = zuo_car(l);
  }

  return first;
}

static zuo_t *zuo_variable_ref(zuo_t *var) {
  zuo_t *val;
  if (var->tag != zuo_variable_tag)
    zuo_fail1w("variable-ref", "not a variable", var);
  val = ((zuo_variable_t *)var)->val;
  if (val == zuo_undefined)
    zuo_fail1("undefined", ((zuo_variable_t *)var)->name);
  return val;
}

static zuo_t *zuo_variable_set(zuo_t *var, zuo_t *val) {
  if (var->tag != zuo_variable_tag)
    zuo_fail1w("variable-set!", "not a variable", var);
  if (((zuo_variable_t *)var)->val != zuo_undefined)
    zuo_fail1w("variable-set!", "variable already has a value", var);
  ((zuo_variable_t *)var)->val = val;
  return zuo_void;
}

static zuo_t *zuo_make_void(zuo_t *args) {
  return zuo_void;
}

static zuo_t *zuo_kernel_namespace() {
  return zuo_top_env;
}

/*======================================================================*/
/* interpreter                                                          */
/*======================================================================*/

static zuo_t *zuo_dump() {
  return zuo_cons(zuo_interp_e,
                  zuo_cons(zuo_interp_env,
                           zuo_cons(zuo_interp_k,
                                    zuo_interp_v)));
}

static void zuo_undump(zuo_t *d) {
  zuo_interp_e = zuo_car(d);
  d = zuo_cdr(d);
  zuo_interp_env = zuo_car(d);
  d = zuo_cdr(d);
  zuo_interp_k = zuo_car(d);
  zuo_interp_v = zuo_cdr(d);
}

static void bad_form(zuo_t *e) {
  zuo_fail1("bad form", e);
}

/* Not strictly necessary, but a handy sanity check on input expressions: */
static void check_syntax(zuo_t *e) {
  zuo_t *es = zuo_cons(e, zuo_null);

  while (es != zuo_null) {
    e = zuo_car(es);
    es = zuo_cdr(es);
    if (e->tag == zuo_pair_tag) {
      zuo_t *rator = zuo_car(e);

      if (rator == zuo_quote_symbol) {
        zuo_t *d = zuo_cdr(e);
        if ((d->tag != zuo_pair_tag) || (zuo_cdr(d) != zuo_null))
          bad_form(e);
      } else if (rator == zuo_if_symbol) {
        zuo_t *d = zuo_cdr(e), *dd, *ddd;
        if (d->tag != zuo_pair_tag)
          bad_form(e);
        dd = zuo_cdr(d);
        if (dd->tag != zuo_pair_tag)
          bad_form(e);
        ddd = zuo_cdr(dd);
        if ((ddd->tag != zuo_pair_tag) || (zuo_cdr(ddd) != zuo_null))
          bad_form(e);
        es = zuo_cons(zuo_car(ddd), es);
        es = zuo_cons(zuo_car(dd), es);
        es = zuo_cons(zuo_car(d), es);
      } else if (rator == zuo_lambda_symbol) {
        zuo_t *d = zuo_cdr(e), *dd, *ad;
        if (d->tag != zuo_pair_tag)
          bad_form(e);
        ad = zuo_car(d); /* formals */
        dd = zuo_cdr(d);
        if (dd->tag != zuo_pair_tag)
          bad_form(e);
        if (zuo_cdr(dd) != zuo_null) {
          /* skip over name string */
          if (zuo_car(dd)->tag != zuo_string_tag)
            bad_form(e);
          dd = zuo_cdr(dd);
        }
        if ((dd->tag != zuo_pair_tag) || (zuo_cdr(dd) != zuo_null))
          bad_form(e);
        while (ad->tag == zuo_pair_tag) {
          if (zuo_car(ad)->tag != zuo_symbol_tag)
            bad_form(e);
          ad = zuo_cdr(ad);
        }
        if ((ad != zuo_null) && (ad->tag != zuo_symbol_tag))
          bad_form(e);
        es = zuo_cons(zuo_car(dd), es);
      } else if (rator == zuo_let_symbol) {
        zuo_t *d = zuo_cdr(e), *dd, *ad, *aad, *daad, *adaad;
        if (d->tag != zuo_pair_tag)
          bad_form(e);
        ad = zuo_car(d); /* `((id rhs))` */
        dd = zuo_cdr(d);
        if ((dd->tag != zuo_pair_tag) || (zuo_cdr(dd) != zuo_null))
          bad_form(e);
        if ((ad->tag != zuo_pair_tag) || (zuo_cdr(ad) != zuo_null))
          bad_form(e);
        aad = zuo_car(ad); /* `(id rhs)` */
        if ((aad->tag != zuo_pair_tag) || (zuo_car(aad)->tag != zuo_symbol_tag))
          bad_form(e);
        daad = zuo_cdr(aad); /* `(rhs)` */
        if ((daad->tag != zuo_pair_tag) || (zuo_cdr(daad) != zuo_null))
          bad_form(e);
        adaad = zuo_car(daad); /* `rhs` */
        es = zuo_cons(adaad, es);
        es = zuo_cons(zuo_car(dd), es);
      } else if (rator == zuo_letcc_symbol) {
        zuo_t *d = zuo_cdr(e), *dd;
        if (d->tag != zuo_pair_tag)
          bad_form(e);
        if (zuo_car(d)->tag != zuo_symbol_tag)
          bad_form(e);
        dd = zuo_cdr(d);
        if ((dd->tag != zuo_pair_tag) || (zuo_cdr(dd) != zuo_null))
          bad_form(e);
        es = zuo_cons(zuo_car(dd), es);
      } else if (rator == zuo_begin_symbol) {
        zuo_t *l = zuo_cdr(e);
        if (l->tag != zuo_pair_tag)
          bad_form(e);
        while (l->tag == zuo_pair_tag) {
          es = zuo_cons(zuo_car(l), es);
          l = zuo_cdr(l);
        }
        if (l != zuo_null)
          bad_form(e);
      } else {
        zuo_t *l = e;
        while (l->tag == zuo_pair_tag) {
          es = zuo_cons(zuo_car(l), es);
          l = zuo_cdr(l);
        }
        if (l != zuo_null)
          bad_form(e);
      }
    }
  }
}

static zuo_t *env_extend(zuo_t *env, zuo_t *sym, zuo_t *val) {
  ASSERT((env->tag == zuo_trie_node_tag) || (env->tag == zuo_pair_tag));
  return zuo_cons(zuo_cons(sym, val), env);
}

static zuo_t *env_lookup(zuo_t *env, zuo_t *sym) {
  while (env->tag == zuo_pair_tag) {
    zuo_t *a = zuo_car(env);
    if (zuo_car(a) == sym)
      return zuo_cdr(a);
    env = zuo_cdr(env);
  }
  return zuo_trie_lookup(env, sym);
}

#ifdef ZUO_SAFER_INTERP
# define _zuo_car(p) zuo_car(p)
# define _zuo_cdr(p) zuo_cdr(p)
#else
# define _zuo_car(p) ZUO_CAR(p)
# define _zuo_cdr(p) ZUO_CDR(p)
#endif

static void interp_step() {
  zuo_t *e = zuo_interp_e;
  if (e->tag == zuo_symbol_tag) {
    zuo_t *val = env_lookup(zuo_interp_env, e);
    if (val == zuo_undefined)
      zuo_fail1("undefined", e);
    zuo_interp_v = val;
  } else if (e->tag == zuo_pair_tag) {
    zuo_t *rator = _zuo_car(e);

    if (rator == zuo_quote_symbol) {
      zuo_interp_v = _zuo_car(_zuo_cdr(e));
    } else if (rator == zuo_if_symbol) {
      zuo_t *d = _zuo_cdr(e);
      zuo_interp_e = _zuo_car(d);
      zuo_interp_k = zuo_cont(zuo_if_cont,
                              _zuo_cdr(d), zuo_interp_env,
                              zuo_interp_k);
    } else if (rator == zuo_lambda_symbol) {
      zuo_interp_v = zuo_closure(zuo_interp_e, zuo_interp_env);
    } else if (rator == zuo_let_symbol) {
      zuo_t *d = _zuo_cdr(e);
      zuo_interp_e = _zuo_car(_zuo_cdr(_zuo_car(_zuo_car(d))));
      zuo_interp_k = zuo_cont(zuo_let_cont,
                              d, zuo_interp_env,
                              zuo_interp_k);
    } else if (rator == zuo_letcc_symbol) {
      zuo_t *d = _zuo_cdr(e);
      zuo_interp_env = env_extend(zuo_interp_env, _zuo_car(d), zuo_interp_k);
      zuo_interp_e = _zuo_car(_zuo_cdr(d));
    } else if (rator == zuo_begin_symbol) {
      zuo_t *d = _zuo_cdr(e);
      zuo_t *dd = _zuo_cdr(d);
      zuo_interp_e = _zuo_car(d);
      if (dd != zuo_null)
        zuo_interp_k = zuo_cont(zuo_begin_cont,
                                dd, zuo_interp_env,
                                zuo_interp_k);
    } else {
      zuo_interp_e = rator;
      zuo_interp_k = zuo_cont(zuo_apply_cont,
                              zuo_cons(zuo_null, _zuo_cdr(e)), zuo_interp_env,
                              zuo_interp_k);
    }
  } else
    zuo_interp_v = e;
}

static void continue_step() {
  zuo_cont_t *k = (zuo_cont_t *)zuo_interp_k;
  zuo_interp_k = k->next;
  switch (k->tag) {
  case zuo_apply_cont:
    {
      zuo_t *vals = _zuo_car(k->data);
      zuo_t *exps = _zuo_cdr(k->data);
      vals = zuo_cons(zuo_interp_v, vals);
      if (exps == zuo_null) {
        zuo_t *rator;
        vals = zuo_reverse(vals);
        rator = _zuo_car(vals);
        if (rator->tag == zuo_closure_tag) {
          zuo_t *all_vals = vals;
          zuo_closure_t *f = (zuo_closure_t *)rator;
          zuo_t *env = f->env;
          zuo_t *args = _zuo_car(_zuo_cdr(f->lambda));
          zuo_t *body = _zuo_cdr(_zuo_cdr(f->lambda));
          zuo_t *body_d = _zuo_cdr(body);
          if (body_d != zuo_null)
            body = body_d; /* skip over function name */
          vals = _zuo_cdr(vals);
          while (args->tag == zuo_pair_tag) {
            if (vals == zuo_null)
              break;
            env = env_extend(env, _zuo_car(args), _zuo_car(vals));
            args = _zuo_cdr(args);
            vals = _zuo_cdr(vals);
          }
          if (args->tag == zuo_symbol_tag)
            env = env_extend(env, args, vals);
          else if (args != zuo_null || vals != zuo_null)
            zuo_fail1("wrong argument count", all_vals);
          
          zuo_interp_e = _zuo_car(body);
          zuo_interp_env = env;
          zuo_interp_v = zuo_undefined;
        } else if (rator->tag == zuo_primitive_tag) {
          zuo_primitive_t *f = (zuo_primitive_t *)rator;
          zuo_int_t n = ((f->arity_mask == -1) ? 0 : zuo_length_int(_zuo_cdr(vals)));
          if (f->arity_mask & (1 << ((n > 10) ? 10 : n)))
            zuo_interp_v = f->proc(f->data, _zuo_cdr(vals));
          else
            zuo_fail1("wrong argument count", vals);
        } else if (rator->tag == zuo_cont_tag) {
          zuo_t *args = _zuo_cdr(vals);
          zuo_int_t n = zuo_length_int(args);
          if (n == 1) {
            zuo_interp_k = _zuo_car(vals);
            zuo_interp_v = _zuo_car(args);
          } else
            zuo_fail1("wrong argument count", vals);
        } else
          zuo_fail1("not a function for call", rator);
      } else {
        zuo_interp_e = _zuo_car(exps);
        zuo_interp_env = k->env;
        zuo_interp_k = zuo_cont(zuo_apply_cont, zuo_cons(vals, _zuo_cdr(exps)), zuo_interp_env, zuo_interp_k);
        zuo_interp_v = zuo_undefined;
      }
    }
    break;
  case zuo_let_cont:
    zuo_interp_e = _zuo_car(_zuo_cdr(k->data));
    zuo_interp_env = env_extend(k->env, _zuo_car(_zuo_car(_zuo_car(k->data))), zuo_interp_v);
    zuo_interp_v = zuo_undefined;
    break;
  case zuo_begin_cont:
    {
      zuo_t *d = _zuo_cdr(k->data);
      zuo_interp_e = _zuo_car(k->data);
      zuo_interp_env = k->env;
      if (d != zuo_null)
        zuo_interp_k = zuo_cont(zuo_begin_cont,
                                d, zuo_interp_env,
                                zuo_interp_k);
      zuo_interp_v = zuo_undefined;
    }
    break;
  case zuo_if_cont:
    {
      if (zuo_interp_v == zuo_false)
        zuo_interp_e = _zuo_car(_zuo_cdr(k->data));
      else
        zuo_interp_e = _zuo_car(k->data);
      zuo_interp_env = k->env;
      zuo_interp_v = zuo_undefined;
    }
    break;
  }
}

zuo_t *zuo_eval(zuo_t *e) {
  check_syntax(e);

  zuo_stash = zuo_cons(zuo_dump(), zuo_stash);

  zuo_interp_e = e;
  zuo_interp_v = zuo_undefined;
  zuo_interp_env = zuo_top_env;
  zuo_interp_k = zuo_eof;

  while (1) {
    zuo_check_collect();
    if (zuo_interp_v == zuo_undefined) {
      interp_step();
    } else if (zuo_interp_k == zuo_eof) {
      zuo_t *v = zuo_interp_v;
      zuo_interp_e = zuo_interp_v = zuo_interp_env = zuo_interp_k = zuo_false;
      
      zuo_undump(zuo_car(zuo_stash));
      zuo_stash = zuo_cdr(zuo_stash);
      
      return v;
    } else {
      continue_step();
    }
  }
}

/*======================================================================*/
/* paths                                                                */
/*======================================================================*/

static void check_path_string(const char *who, zuo_t *obj) {
  zuo_int_t i;
  const char *msg = "not a path string";
  
  if ((obj->tag != zuo_string_tag)
      || ZUO_STRING_LEN(obj) == 0)
    zuo_fail1w(who, msg, obj);

  for (i = ZUO_STRING_LEN(obj); i--; ) {
    if (((zuo_string_t *)obj)->s[i] == 0)
      zuo_fail1w(who, msg, obj);
  }
}

static int path_is_absolute(const char *p) {
#ifdef WIN32
  FIXME;
#else
  return p[0] == '/';
#endif
}

static int zuo_path_is_absolute(zuo_t *obj) {
  return path_is_absolute(ZUO_STRING_PTR(obj));
}

static char *zuo_getcwd() {
  char *dir;

#ifdef WIN32
  FIXME;
#else
  char *s;
  int len = 256;
  
  s = malloc(len);
  while (1) {
    dir = getcwd(s, len);
    if (dir)
      break;
    if (errno == ERANGE) {
      free(s);
      len *= 2;
      s = malloc(len);
    } else
      break;
  }
  /* dir == s, unless failure */
#endif
  
  if (!dir)
    zuo_fail("error getting current directory");

  return dir;
}

static zuo_t *zuo_current_directory() {
  char *dir = zuo_getcwd();
  zuo_t *obj;

  obj = zuo_string(dir);
  free(dir);
  
  return obj;
}

static zuo_t *zuo_build_path(zuo_t *pre, zuo_t *post) {
  zuo_string_t *path;
  zuo_uint_t len;
  int add_sep;

  check_path_string("build-path", pre);
  check_path_string("build-path", post);
  
  if (zuo_path_is_absolute(post))
    zuo_fail1w("build-path", "second path is absolute", post);

  len = ZUO_STRING_LEN(pre);
  if (((zuo_string_t *)pre)->s[len-1] == '/')
    add_sep = 0;
  else {
    len += 1;
    add_sep = 1;
  }
  len += ZUO_STRING_LEN(post);

  path = (zuo_string_t *)zuo_new(zuo_string_tag, ZUO_STRING_ALLOC_SIZE(len));
  path->len = len;
  path->s[len] = 0;
  len = ZUO_STRING_LEN(pre);
  memcpy(&path->s, ZUO_STRING_PTR(pre), len);
  if (add_sep)
    path->s[len++] = '/';
  memcpy(&path->s[len], ZUO_STRING_PTR(post), ZUO_STRING_LEN(post));

  return (zuo_t *)path;
}

static zuo_t *zuo_split_path(zuo_t *p) {
  zuo_int_t i;
  int non_sep;
  
  check_path_string("split-path", p);

  non_sep = 0;
  for (i = ZUO_STRING_LEN(p); i--; ) {
    if (ZUO_STRING_PTR(p)[i] == '/') {
      if (non_sep) {
        return zuo_cons(zuo_sized_string(ZUO_STRING_PTR(p), i+1),
                        zuo_string(ZUO_STRING_PTR(p)+i+1));
      }
    } else
      non_sep = 1;
  }

  return zuo_cons(zuo_false, p);
}

static zuo_t *zuo_path_to_complete_path(zuo_t *path, zuo_t *rel_to) {
  zuo_string_t *ps;

  check_path_string("path->complete-path", path);

  if (zuo_path_is_absolute(path))
    return path;
  else
    return zuo_build_path((rel_to == zuo_false) ? zuo_current_directory() : rel_to, path);
}

zuo_t *zuo_library_path_to_file_path(zuo_t *path) {
  zuo_string_t *str;
  zuo_t *strobj;

  if (path->tag == zuo_symbol_tag) {
    int i;
    str = (zuo_string_t *)((zuo_symbol_t *)path)->str;
    if (str->len == 0)
      str = NULL;
    else {
      zuo_int_t i;
      for (i = 0; i < str->len; i++) {
        if (!zuo_is_symbol_module_char(str->s[i])) {
          str = NULL;
          break;
        }
      }
    }
  } else
    str = NULL;
    
  if (str == NULL)
    zuo_fail1w("module-path->path", "not a module-path symbol", path);
  
  strobj = zuo_tilde_a(zuo_cons((zuo_t *)str, zuo_string(".zuo")));

  return zuo_build_path(zuo_library_path, strobj);
}

/*======================================================================*/
/* files/streams                                                        */
/*======================================================================*/

static char *zuo_drain(FILE *f, zuo_int_t fd) {
  char *s;
  zuo_int_t sz = 256, offset = 0;
  
  s = malloc(sz);
  while (1) {
    zuo_int_t got;
    if (f) {
      got = fread(s + offset, 1, sz - offset, f);
      if ((got == 0) && ferror(f))
        got = -1;
    } else
      got = read(fd, s + offset, sz - offset);

    if (got < 0)
      zuo_fail("error reading stream");
    
    if (got == 0) {
      s[offset] = 0;
      return s;
    } else {
      offset += got;
      if (offset == sz) {
        char *new_s = malloc(sz * 2);
        memcpy(new_s, s, sz);
        free(s);
        sz = sz * 2;
        s = new_s;
      }
    }
  }
}

static void zuo_fill(const char *s, zuo_int_t len, FILE *f, zuo_int_t fd) {
  zuo_int_t done = 0;
  while (done < len) {
    zuo_int_t did;
    if (f) {
      did = fwrite(s + done, 1, len - done, f);
      if (did < len - done)
        did = -1;
    } else
      did = write(fd, s + done, len - done);

    if (did < 0)
      zuo_fail("error writing to stream");

    done += did;
  }
}

static void zuo_close(zuo_int_t p)
{
#ifdef WIN32
  CloseHandle((HANDLE)p);
#else
  close(p);
#endif
}

zuo_t *zuo_write_fd(zuo_t *fd_i, zuo_t *str) {
  const char *who = "write-fd";

  if ((fd_i->tag != zuo_handle_tag)
      || ((zuo_handle_t *)fd_i)->status != zuo_handle_open_fd_out_status)
    zuo_fail1w(who, "not an open ouput handle", fd_i);

  check_integer(who, fd_i);
  check_string(who, str);
  
  zuo_fill(ZUO_STRING_PTR(str), ZUO_STRING_LEN(str), NULL, ZUO_INT_I(fd_i));

  zuo_close(ZUO_INT_I(fd_i));
  ((zuo_handle_t *)fd_i)->status = zuo_handle_closed_fd_status;

  return zuo_void;
}

zuo_t *zuo_read_fd(zuo_t *fd_i) {
  const char *who = "read-fd";
  zuo_t *str;
  
  if ((fd_i->tag != zuo_handle_tag)
      || ((zuo_handle_t *)fd_i)->status != zuo_handle_open_fd_in_status)
    zuo_fail1w(who, "not an open input handle", fd_i);

  str = zuo_string(zuo_drain(NULL, ZUO_INT_I(fd_i)));

  zuo_close(ZUO_INT_I(fd_i));
  ((zuo_handle_t *)fd_i)->status = zuo_handle_closed_fd_status;

  return str;
}

static char *zuo_string_to_c(zuo_t *obj) {
  char *s;
  zuo_int_t len;

  check_string("string->c", obj);

  len = ZUO_STRING_LEN(obj);
  s = malloc(len + 1);
  memcpy(s, ZUO_STRING_PTR(obj), len);
  s[len] = 0;

  return s;
}

/*======================================================================*/
/* modules                                                              */
/*======================================================================*/

static zuo_t *zuo_dynamic_require(zuo_t *module_path) {
  
  if (module_path->tag == zuo_symbol_tag)
    module_path = zuo_library_path_to_file_path(module_path);
  else if (module_path->tag == zuo_string_tag)
    module_path = zuo_path_to_complete_path(module_path, zuo_false);
  else
    zuo_fail1w("dynamic-require", "not a module path", module_path);

  /* check for already-loaded module */
  {
    zuo_t *l;
    
    for (l = zuo_modules; l != zuo_null; l = zuo_cdr(l)) {
      zuo_t *a = zuo_car(l);
      if (zuo_string_eql(zuo_car(a), module_path) == zuo_true)
        return zuo_cdr(a);
    }
  }

  /* not already to loaded */

  {
    FILE *in;
    char *filename, *input;
    
    filename = ZUO_STRING_PTR(module_path);
    in = fopen(filename, "r");
    if (in == NULL)
      zuo_fail1("could not open module file", module_path);
    
    input = zuo_drain(in, 0);
    fclose(in);

    {
      char *lang;
      zuo_int_t post;
      zuo_t *v;

      lang = zuo_read_language(input, &post);
      zuo_stash = zuo_cons(module_path, zuo_stash);

      if (!strcmp(lang, "zuo/kernel")) {
        zuo_t *e = zuo_read_one_str(input + post);
        v = zuo_eval(e);
      } else {
        zuo_t *env = zuo_dynamic_require(zuo_symbol(lang));
        zuo_t *proc = zuo_trie_lookup(env, zuo_symbol("read-and-eval"));
        if (proc->tag != zuo_closure_tag)
          zuo_fail1("invalid read-and-eval from language", zuo_symbol(lang));
        module_path = zuo_car(zuo_stash);
        v = zuo_eval(zuo_cons(proc, zuo_cons(zuo_string(input),
                                             zuo_cons(zuo_integer(post),
                                                      zuo_cons(module_path,
                                                               zuo_null)))));
      }
      free(lang);

      module_path = zuo_car(zuo_stash);
      zuo_stash = zuo_cdr(zuo_stash);
      
      free(input);

      if (v->tag != zuo_trie_node_tag)
        zuo_fail1("module did not produce a hash table", module_path);

      zuo_modules = zuo_cons(zuo_cons(module_path, v), zuo_modules);

      return v;
    }
  }
}

/*======================================================================*/
/* processes                                                            */
/*======================================================================*/

static void zuo_pipe(zuo_int_t *_r, zuo_int_t *_w)
{
#ifdef WIN32
  {
    HANDLE rh, wh;
    if (!CreatePipe(&rh, &wh, NULL, 0))
      zuo_fail("pipe creation failed");
    *_r = (zuo_int_t)rh;
    *_w = (zuo_int_t)wh;
  }
#else
  {
    int fd[2];
    if (pipe(fd) != 0)
      zuo_fail("pipe creation failed");
    *_r = fd[0];
    *_w = fd[1];
  }
#endif  
}

#ifdef WIN32
static char *zuo_cmdline_protect(const char *s)
{
  char *naya;
  int ds;
  int has_space = 0, has_quote = 0, was_slash = 0;

  if (!*s) return _strdup("\"\""); /* quote an empty argument */

  for (ds = 0; s[ds]; ds++) {
    if (isspace(s[ds]) || (s[ds] == '\'')) {
      has_space = 1;
      was_slash = 0;
    } else if (s[ds] == '"') {
      has_quote += 1 + (2 * was_slash);
      was_slash = 0;
    } else if (s[ds] == '\\') {
      was_slash++;
    } else
      was_slash = 0;
  }

  if (has_space || has_quote) {
    char *p;
    int wrote_slash = 0;

    naya = malloc(strlen(s) + 3 + 3*has_quote + was_slash);
    naya[0] = '"';
    for (p = naya + 1; *s; s++) {
      if (*s == '"') {
	while (wrote_slash--) {
	  *(p++) = '\\';
	}
	*(p++) = '"'; /* endquote */
	*(p++) = '\\';
	*(p++) = '"'; /* protected */
	*(p++) = '"'; /* start quote again */
	wrote_slash = 0;
      } else if (*s == '\\') {
	*(p++) = '\\';
	wrote_slash++;
      } else {
	*(p++) = *s;
	wrote_slash = 0;
      }
    }
    while (wrote_slash--) {
      *(p++) = '\\';
    }
    *(p++) = '"';
    *p = 0;

    return naya;
  }

  return _strdup(s);
}
#endif


zuo_t *zuo_process(zuo_t *command_and_args, zuo_t *options)
{
  zuo_t *l, *p_handle;
  int redirect_in, redirect_out, redirect_err;
  zuo_int_t pid, in, in_r, out, out_w, err, err_w;
  int argc, i, ok;
  char **argv;

  /* need at least the command part */
  if (command_and_args->tag != zuo_pair_tag)
    l = zuo_false;
  else {
    for (l = command_and_args; l->tag == zuo_pair_tag; l = zuo_cdr(l))
      if (zuo_car(l)->tag != zuo_string_tag)
        break;
  }
  if (l != zuo_null)
    zuo_fail1w("process", "not a list of strings", command_and_args);

  argc = zuo_length_int(command_and_args);
  argv = malloc(sizeof(char*) * (argc + 1));

  for (i = 0; i < argc; i++) {
    argv[i] = zuo_string_to_c(zuo_car(command_and_args));
    command_and_args = zuo_cdr(command_and_args);
  }
  argv[i] = NULL;

  redirect_in = redirect_out = redirect_err = 0;
  if (options != zuo_null) {
    zuo_t *redirect_in_sym = zuo_symbol("redirect-in");
    zuo_t *redirect_out_sym = zuo_symbol("redirect-out");
    zuo_t *redirect_err_sym = zuo_symbol("redirect-err");
    
    for (l = options; l->tag == zuo_pair_tag; l = zuo_cdr(l)) {
      zuo_t *a = zuo_car(l);
      if (redirect_in_sym == a)
        redirect_in = 1;
      else if (redirect_out_sym == a)
        redirect_out = 1;
      else if (redirect_err_sym == a)
        redirect_err = 1;
      else
        break;
    }
    if (l != zuo_null)
      zuo_fail1w("process", "not a list of option symbols", options);
  }

  if (redirect_in)
    zuo_pipe(&in_r, &in);
  else
    in_r = in = 0;
  if (redirect_out)
    zuo_pipe(&out, &out_w);
  else
    out = out_w = 0;
  if (redirect_err)
    zuo_pipe(&err, &err_w);
  else
    err = err_w = 0;
  
#ifdef WIN32
  /*--------------------------------------*/
  /*              Windows                 */
  /*--------------------------------------*/
  {
    wchar_t *command_w = zuo_to_wide(argv[0]), *cmline_w;
    char *cmdline;
    int len = 9;
    STARTUPINFOW startup;
    DWORD cr_flag;
    HANDLE in_r, in_w, out_r, out_w, err_r, err_w;
    
    for (i = 0; i < argc; i++) {
      char *s = argv[i];
      argv[i] = zuo_cmdline_protect(s);
      free(s);
      len += strlen(argv[i]) + 1;
    }

    cmdline = malloc(len);

    len = 0;
    for (i = 0; i < argc; i++) {
      l = strlen(argv[i]);
      memcpy(cmdline + len, argv[i], l);
      cmdline[len + l] = ' ';
      len += l + 1;
    }
    cmdline[len-1] = 0;

    cmdline_w = zuo_to_wide(cmdline);
    free(cmdline);

    memset(&startup, 0, sizeof(startup));
    startup.cb = sizeof(*startup);
    startup.dwFlags = STARTF_USESTDHANDLES;
    startup.hStdInput = (HANDLE)in_r;
    startup.hStdOutput = (HANDLE)out_w;
    startup.hStdError = (HANDLE)err_w;
    
    cr_flag = CREATE_NO_WINDOW | CREATE_UNICODE_ENVIRONMENT;

    wd_w = WIDE_PATH_copy(wd);
    
    ok = CreateProcessW(command_w, cmdline_w, 
                        NULL, NULL, 1 /*inherit*/,
                        cr_flag, NULL, NULL,
                        &startup, &info);

    free(command_w);
    free(cmdline_w);
  }
#else
  /*--------------------------------------*/
  /*                Unix                  */
  /*--------------------------------------*/
  {
    pid = fork();
    
    if (pid > 0) {
      /* This is the original process, which needs to manage the 
         newly created child process. */
      ok = 1;
    } else if (pid == 0) {
      /* This is the new child process */
      if (redirect_in) {
        dup2(in_r, 0);
        close(in);
      }
      if (redirect_out) {
        dup2(out_w, 1);
        close(out);
      }
      if (redirect_err) {
        dup2(err_w, 2);
        close(err);
      }

      execv(argv[0], argv);
      {
        char *msg = "exec failed";
        write(2, msg, strlen(msg));
      }

      _exit(1);
    } else {
      ok = 0;
    }
  }
#endif

  if (!ok) {
    fprintf(stderr, "attempted command:");
    for (i = 0; i < argc; i++)
      fprintf(stderr, "  %s\n", argv[i]);
    zuo_fail("exec failed");
  }

  if (redirect_in)
    zuo_close(in_r);
  if (redirect_out)
    zuo_close(out_w);
  if (redirect_err)
    zuo_close(err_w);

  for (i = 0; i < argc; i++)
    free(argv[i]);
  free(argv);

  p_handle = zuo_handle(pid, zuo_handle_process_running_status);
#ifndef WIN32
  trie_set(zuo_pid_table, pid, p_handle, p_handle);
#endif

  return zuo_cons(p_handle,
                  zuo_cons(redirect_in ? zuo_handle(in, zuo_handle_open_fd_out_status) : zuo_false,
                           zuo_cons(redirect_out ? zuo_handle(out, zuo_handle_open_fd_in_status) : zuo_false,
                                    zuo_cons(redirect_err ? zuo_handle(err, zuo_handle_open_fd_in_status) : zuo_false,
                                             zuo_null))));
}

static int is_process_handle(zuo_t *p) {
  return ((p->tag == zuo_handle_tag)
          && ((((zuo_handle_t *)p)->status != zuo_handle_process_done_status)
              || (((zuo_handle_t *)p)->status != zuo_handle_process_running_status)));
}

zuo_t *zuo_process_status(zuo_t *p) {
  if (!is_process_handle(p))
    zuo_fail1w("process-status", "not a process handle", p);

  if (((zuo_handle_t *)p)->status == zuo_handle_process_running_status)
    return zuo_symbol("running");
  else
    return zuo_integer(((zuo_handle_t *)p)->i);
}

zuo_t *zuo_process_wait(zuo_t *pids_i) {
  zuo_t *l;

  if (is_process_handle(pids_i))
    pids_i = zuo_cons(pids_i, zuo_null);
    
  for (l = pids_i; l->tag == zuo_pair_tag; l = zuo_cdr(l))
    if (!is_process_handle(zuo_car(l)))
      break;
  if (l != zuo_null)
    zuo_fail1w("process-wait", "not a process handle or list of process handles", pids_i);

#ifdef WIN32
  /* loop until on of the handles is marked as done */
  while (1) {
    HANDLE *a = malloc(sizeof(HANDLE) * zuo_length_int(pids_i));
    zuo_int_i i = 0;
    
    for (l = pids_i; l != zuo_null; l = zuo_cdr(l)) {
      HANDLE sci = (HANDLE)((zuo_handle_t *)p)->i;
      DWORD w;
      if (GetExitCodeProcess(sci, &w)) {
        if (w != STILL_ACTIVE) {
          ((zuo_handle_t *)p)->status = zuo_handle_process_done_status;
          ((zuo_handle_t *)p)->i = w;
          return p;
        } else
          zuo_fail1("process-wait", "status query failed", p);
      }
      a[i++] = sci;
    }

    (void)WaitForMultipleObjects(i, a, FALSE, 0);
  }
#else
  /* loop until on of the handles is marked as done */
  while (1) {
    pid_t pid;
    int stat_loc;
    
    for (l = pids_i; l != zuo_null; l = zuo_cdr(l)) {
      zuo_t *p = zuo_car(l);
      if (((zuo_handle_t *)p)->status == zuo_handle_process_done_status)
        return p;
    }
    
    /* wait for any process to exit, and update the corresponding handle */
    pid = wait(&stat_loc);

    if ((pid >= 0) && WIFEXITED(stat_loc)) {
      zuo_t *p = trie_lookup(zuo_pid_table, pid);
      if (p->tag == zuo_handle_tag) {
        ((zuo_handle_t *)p)->status = zuo_handle_process_done_status;
        ((zuo_handle_t *)p)->i = WEXITSTATUS(stat_loc);
        trie_set(zuo_pid_table, pid, zuo_undefined, zuo_undefined);
      }
    }
  }
#endif
}

/*======================================================================*/
/* executable self path                                                 */
/*======================================================================*/

#if defined(__linux__)

# include <errno.h>
# include <unistd.h>

static char *zuo_self_path_c(char *exec_file)
{
  ssize_t len, blen = 256;
  char *s = malloc(blen);

  while (1) {
    len = readlink("/proc/self/exe", s, blen-1);
    if (len == (blen-1)) {
      free(s);
      blen *= 2;
      s = malloc(blen);
    } else if (len < 0) {
      zuo_fail_errno("failed to get self");
    } else
      break;
  }
  s[len] = 0;

  return buf;
}

#elif defined(__FreeBSD__) || defined(__NetBSD__)

# include <sys/sysctl.h>
# include <errno.h>

static char *zuo_self_path_c(char *exec_file)
{
  int mib[4];
  char *s;
  size_t len;
  int r;

  mib[0] = CTL_KERN;
#if defined(__NetBSD__)
  mib[1] = KERN_PROC_ARGS;
  mib[2] = getpid();
  mib[3] = KERN_PROC_PATHNAME;
#else
  mib[1] = KERN_PROC;
  mib[2] = KERN_PROC_PATHNAME;
  mib[3] = -1;
#endif

  r = sysctl(mib, 4, NULL, &len, NULL, 0);
  if (r < 0)
    zuo_fail_errno("failed to get self");
  s = malloc(len);
  r = sysctl(mib, 4, s, &len, NULL, 0);
  if (r < 0)
    zuo_fail_errno("failed to get self");
  
  return s;
}

#elif defined(__APPLE__) && defined(__MACH__)

# include <mach-o/getsect.h>
# include <mach-o/dyld.h>

static char *zuo_self_path_c(char *exec_file)
{
  uint32_t size = 1024;
  char *s = malloc(size);
  int r;
  
  r = _NSGetExecutablePath(s, &size);
  if (!r)
    return s;
  else {
    free(s);
    s = malloc(size);
    r = _NSGetExecutablePath(s, &size);
    if (!r)
      return s;
    zuo_fail("failed to get self");
    return NULL;
  }
}

#elif defined(WIN32)

/* used outside this file: */
static char *zuo_self_path_c(char *exec_file)
{
  wchar_t *path;
  DWORD r, sz = 1024;

  while (1) {
    path = (wchar_t *)malloc(sz * sizeof(wchar_t));
    r = GetModuleFileNameW(NULL, path, sz);
    if ((r == sz)
        && (GetLastError() == ERROR_INSUFFICIENT_BUFFER)) {
      free(path);
      sz = 2 * sz;
    } else
      break;
  }

  return zuo_from_wide_free(path);
}

#else

/* Generic Unix: get executable path via argv[0] and the `PATH` environment variable */

static int has_slash(char *s) {
  while (*s) {
    if (s[0] == '/')
      return 1;
    s++;
  }
  return 0;
}

static char *zuo_self_path_c(char *exec_file)
{
  if (path_is_absolute(exec_file)) {
    /* Absolute path */
    return strdup(exec_file);
  } else if (has_slash(exec_file)) {
    /* Relative path with a directory: */
    return zip_string_to_c(zuo_path_to_complete_path(zuo_string(exec_file), zuo_false));
  } else {
    /* We have to find the executable by searching PATH: */
    char *path = strdup(getenv("PATH")), *p;
    zuo_t *m;
    int more;

    if (!path) {
      path = "";
    }

    while (1) {
      /* Try each element of path: */
      for (p = path; *p && (*p != ':'); p++) { }
      if (*p) {
	*p = 0;
	more = 1;
      } else
	more = 0;

      if (!*path)
	break;

      m = build_path(zuo_string(path), zuo_string(exec_file));

      if (access(ZUO_STRING_PTR(m), X_OK) == 0)
        return zip_string_to_c(zuo_path_to_complete_path(m, zuo_false));

      if (more)
	path = p + 1;
      else
	break;
    }

    return strdup(exec_file);
  }
}

#endif

static zuo_t *zuo_self_path(char *exec_file) {
  char *s = zuo_self_path_c(exec_file);
  zuo_t *str = zuo_string(s);
  free(s);
  return str;
}
  
/*======================================================================*/
/* main                                                                 */
/*======================================================================*/

#define TRIE_SET_AND_SAVE_NAME(name, proc, make_prim)           \
  do {                                                          \
    zuo_t *sym = zuo_symbol(name);                              \
    zuo_trie_set(zuo_top_env, sym, make_prim(proc, sym));       \
  } while (0)

#define ZUO_TOP_ENV_SET_PRIMITIVE0(name, proc) \
  TRIE_SET_AND_SAVE_NAME(name, proc, zuo_primitive0)
#define ZUO_TOP_ENV_SET_PRIMITIVE1(name, proc) \
  TRIE_SET_AND_SAVE_NAME(name, proc, zuo_primitive1)
#define ZUO_TOP_ENV_SET_PRIMITIVE2(name, proc) \
  TRIE_SET_AND_SAVE_NAME(name, proc, zuo_primitive2)
#define ZUO_TOP_ENV_SET_PRIMITIVE3(name, proc) \
  TRIE_SET_AND_SAVE_NAME(name, proc, zuo_primitive3)
#define ZUO_TOP_ENV_SET_PRIMITIVEN(name, proc) \
  TRIE_SET_AND_SAVE_NAME(name, proc, zuo_primitiveN)

int main(int argc, char **argv) {
  char *input;

  zuo_undefined = zuo_new(zuo_singleton_tag, sizeof(zuo_forwarded_t));
  zuo_true = zuo_new(zuo_singleton_tag, sizeof(zuo_forwarded_t));
  zuo_false = zuo_new(zuo_singleton_tag, sizeof(zuo_forwarded_t));
  zuo_null = zuo_new(zuo_singleton_tag, sizeof(zuo_forwarded_t));
  zuo_eof = zuo_new(zuo_singleton_tag, sizeof(zuo_forwarded_t));
  zuo_void = zuo_new(zuo_singleton_tag, sizeof(zuo_forwarded_t));
  zuo_intern_table = zuo_trie_node();

  zuo_interp_e = zuo_interp_v = zuo_interp_env = zuo_interp_k = zuo_false;

  zuo_quote_symbol = zuo_symbol("quote");
  zuo_lambda_symbol = zuo_symbol("lambda");
  zuo_let_symbol = zuo_symbol("let");
  zuo_letcc_symbol = zuo_symbol("let/cc");
  zuo_begin_symbol = zuo_symbol("begin");
  zuo_if_symbol = zuo_symbol("if");

  zuo_pid_table = zuo_trie_node();

  zuo_stash = zuo_false;
  
  zuo_top_env = zuo_trie_node();
  zuo_modules = zuo_null;
  zuo_library_path = zuo_string(ZUO_LIB_PATH);
  if (!zuo_path_is_absolute(zuo_library_path))
    zuo_library_path = zuo_build_path(zuo_car(zuo_split_path(zuo_self_path(argv[0]))),
                                      zuo_library_path);

  ZUO_TOP_ENV_SET_PRIMITIVE1("pair?", zuo_pair_p);
  ZUO_TOP_ENV_SET_PRIMITIVE1("null?", zuo_null_p);
  ZUO_TOP_ENV_SET_PRIMITIVE1("integer?", zuo_integer_p);
  ZUO_TOP_ENV_SET_PRIMITIVE1("procedure?", zuo_procedure_p);
  ZUO_TOP_ENV_SET_PRIMITIVE1("string?", zuo_string_p);
  ZUO_TOP_ENV_SET_PRIMITIVE1("symbol?", zuo_symbol_p);
  ZUO_TOP_ENV_SET_PRIMITIVE1("hash?", zuo_hash_p);
  ZUO_TOP_ENV_SET_PRIMITIVE1("list?", zuo_list_p);
  ZUO_TOP_ENV_SET_PRIMITIVEN("void", zuo_make_void);

  ZUO_TOP_ENV_SET_PRIMITIVE2("cons", zuo_cons);
  ZUO_TOP_ENV_SET_PRIMITIVE1("car", zuo_car);
  ZUO_TOP_ENV_SET_PRIMITIVE1("cdr", zuo_cdr);
  ZUO_TOP_ENV_SET_PRIMITIVEN("list", zuo_list);
  ZUO_TOP_ENV_SET_PRIMITIVEN("append", zuo_append);
  ZUO_TOP_ENV_SET_PRIMITIVE1("reverse", zuo_reverse);
  ZUO_TOP_ENV_SET_PRIMITIVE1("length", zuo_length);

  ZUO_TOP_ENV_SET_PRIMITIVE1("not", zuo_not);
  ZUO_TOP_ENV_SET_PRIMITIVE2("eq?", zuo_eq);

  ZUO_TOP_ENV_SET_PRIMITIVE2("+", zuo_add);
  ZUO_TOP_ENV_SET_PRIMITIVE2("-", zuo_subtract);
  ZUO_TOP_ENV_SET_PRIMITIVE2("*", zuo_multiply);
  ZUO_TOP_ENV_SET_PRIMITIVE2("quotient", zuo_quotient);
  ZUO_TOP_ENV_SET_PRIMITIVE2("modulo", zuo_modulo);
  ZUO_TOP_ENV_SET_PRIMITIVE2("<", zuo_lt);
  ZUO_TOP_ENV_SET_PRIMITIVE2("<=", zuo_le);
  ZUO_TOP_ENV_SET_PRIMITIVE2("=", zuo_eql);
  ZUO_TOP_ENV_SET_PRIMITIVE2(">=", zuo_ge);
  ZUO_TOP_ENV_SET_PRIMITIVE2(">", zuo_gt);

  ZUO_TOP_ENV_SET_PRIMITIVE1("string-length", zuo_string_length);
  ZUO_TOP_ENV_SET_PRIMITIVE2("string-ref", zuo_string_ref);
  ZUO_TOP_ENV_SET_PRIMITIVE3("substring", zuo_substring);
  ZUO_TOP_ENV_SET_PRIMITIVE2("string=?", zuo_string_eql);
  ZUO_TOP_ENV_SET_PRIMITIVE1("string->symbol", zuo_string_to_symbol);
  ZUO_TOP_ENV_SET_PRIMITIVE1("string->uninterned-symbol", zuo_string_to_uninterned_symbol);

  ZUO_TOP_ENV_SET_PRIMITIVEN("hash", zuo_hash);
  ZUO_TOP_ENV_SET_PRIMITIVE3("hash-ref", zuo_hash_ref);
  ZUO_TOP_ENV_SET_PRIMITIVE3("hash-set", zuo_hash_set);
  ZUO_TOP_ENV_SET_PRIMITIVE1("hash-keys", zuo_hash_keys);

  ZUO_TOP_ENV_SET_PRIMITIVE2("build-path", zuo_build_path);
  ZUO_TOP_ENV_SET_PRIMITIVE1("split-path", zuo_split_path);
  ZUO_TOP_ENV_SET_PRIMITIVE2("path->complete-path", zuo_path_to_complete_path);

  ZUO_TOP_ENV_SET_PRIMITIVE1("variable", zuo_variable);
  ZUO_TOP_ENV_SET_PRIMITIVE1("variable-ref", zuo_variable_ref);
  ZUO_TOP_ENV_SET_PRIMITIVE2("variable-set!", zuo_variable_set);

  ZUO_TOP_ENV_SET_PRIMITIVE2("process", zuo_process);
  ZUO_TOP_ENV_SET_PRIMITIVE1("process-status", zuo_process_wait);
  ZUO_TOP_ENV_SET_PRIMITIVE1("process-wait", zuo_process_wait);
  ZUO_TOP_ENV_SET_PRIMITIVE1("read-fd", zuo_read_fd);
  ZUO_TOP_ENV_SET_PRIMITIVE2("write-fd", zuo_write_fd);

  ZUO_TOP_ENV_SET_PRIMITIVE1("display", zuo_display);
  ZUO_TOP_ENV_SET_PRIMITIVEN("error", zuo_error);
  ZUO_TOP_ENV_SET_PRIMITIVEN("~v", zuo_tilde_v);
  ZUO_TOP_ENV_SET_PRIMITIVEN("~a", zuo_tilde_a);
  ZUO_TOP_ENV_SET_PRIMITIVEN("~s", zuo_tilde_s);

  ZUO_TOP_ENV_SET_PRIMITIVE1("read-from-string", zuo_read_one);
  ZUO_TOP_ENV_SET_PRIMITIVE1("read-from-string-all", zuo_read_all);
  ZUO_TOP_ENV_SET_PRIMITIVE1("eval", zuo_eval);
  ZUO_TOP_ENV_SET_PRIMITIVE1("dynamic-require", zuo_dynamic_require);
  ZUO_TOP_ENV_SET_PRIMITIVE0("kernel-namespace", zuo_kernel_namespace);

  if (argc > 1)
    (void)zuo_dynamic_require(zuo_string(argv[1]));
  else {
    char *input = zuo_drain(stdin, 0);
    zuo_t *es = zuo_read_all_str(input);
    free(input);
    while (es != zuo_null) {
      zuo_t *v;
      zuo_stash = zuo_cdr(es);
      v = zuo_eval(zuo_car(es));
      zuo_fprint(stdout, v);
      fprintf(stdout, "\n");
      es = zuo_stash;
    }
  }

  return 0;
}
