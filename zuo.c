/* Like Zuo overall, the kernel implementation here is layered. There
   should be no need for forward function declarations, except locally
   in the rare case of mutually recursive functions. */

#if defined(_MSC_VER) || defined(__MINGW32__)
# define ZUO_WINDOWS
#else
# define ZUO_UNIX
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#ifdef ZUO_UNIX
# include <fcntl.h>
# include <unistd.h>
# include <errno.h>
# include <sys/types.h>
# include <sys/wait.h>
# include <sys/stat.h>
# include <time.h>
# include <dirent.h>
#endif
#ifdef ZUO_WINDOWS
# include <windows.h>
# include <direct.h>
# include <io.h>
# include <sys/stat.h>
#endif

#if 0
# include <assert.h>
# define ASSERT(x) assert(x)
#else
# define ASSERT(x) do { } while (0)
#endif

#include <stdint.h>

/* `zuo_int_t` should be a 64-bit integer type, so we don't have to
   worry about Y2038 or large file sizes. `zuo_int32_t` should be a
   32-bit integer type, obviously. */
typedef int64_t zuo_int_t;
typedef uint64_t zuo_uint_t;

typedef int32_t zuo_int32_t;
typedef uint32_t zuo_uint32_t;

#ifdef ZUO_WINDOWS
typedef HANDLE zuo_raw_handle_t;
#else
typedef int zuo_raw_handle_t;
#endif

/* the "image.zuo" script looks for this line: */
#define EMBEDDED_IMAGE 0

#ifndef ZUO_LIB_PATH
# define ZUO_LIB_PATH "lib"
#endif
static const char *zuo_lib_path = ZUO_LIB_PATH;

/* for read and print: */
#define ZUO_RECUR_LIMIT 100

#define MIN_HEAP_SIZE (8*1024*1024)

/*======================================================================*/
/* run-time configuration                                               */
/*======================================================================*/

static int zuo_logging = 0;
static int zuo_probe_each = 0;
static int zuo_probe_counter = 0;

static void zuo_configure() {
  const char *s;

  if ((s = getenv("ZUO_LIB_PATH"))) {
    zuo_lib_path = s;
  }

  if (getenv("ZUO_LOG"))
    zuo_logging = 1;
  
  if ((s = getenv("ZUO_PROBE_EACH"))) {
    while (isdigit(*s)) {
      zuo_probe_each = (zuo_probe_each * 10) + (s[0] - '0');
      s++;
    }
  }
}

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
  zuo_opaque_tag,
  zuo_cont_tag,
  zuo_forwarded_tag
} zuo_tag_t;

typedef struct zuo_t {
  zuo_int32_t tag;
  /* every subtype must have more to make it at least as
     large as `zuo_forwarded_t` */
} zuo_t;

typedef struct {
  zuo_t obj;
  zuo_t *forward;
} zuo_forwarded_t;

typedef struct {
  zuo_t obj;
  zuo_int_t index;
} zuo_fasl_forwarded_t;

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

#ifdef ZUO_SAFER_INTERP
# define _zuo_car(p) zuo_car(p)
# define _zuo_cdr(p) zuo_cdr(p)
#else
# define _zuo_car(p) ZUO_CAR(p)
# define _zuo_cdr(p) ZUO_CDR(p)
#endif

typedef struct {
  zuo_t obj;
  zuo_int_t len; /* must be at the same place as forwarding */
  unsigned char s[1];
} zuo_string_t;

/* Since `len` overlaps with forwarding, we can tentatively get the "length" from any object */
#define ZUO_STRING_LEN(obj) (((zuo_string_t *)(obj))->len)

#define ZUO_STRING_ALLOC_SIZE(len) (((sizeof(zuo_string_t) + (len) + 1 + 3) >> 2) << 2)
#define ZUO_STRING_PTR(obj) ((char *)&((zuo_string_t *)(obj))->s)

typedef struct {
  zuo_t obj;
  zuo_int32_t id;
  zuo_t *str;
} zuo_symbol_t;

#define ZUO_TRIE_BFACTOR_BITS 4
#define ZUO_TRIE_BFACTOR      (1 << ZUO_TRIE_BFACTOR_BITS)
#define ZUO_TRIE_BFACTOR_MASK (ZUO_TRIE_BFACTOR -1)

typedef struct zuo_trie_node_t {
  zuo_t obj;
  zuo_int_t count;
  zuo_t *key;
  zuo_t *val;
  struct zuo_t* next[ZUO_TRIE_BFACTOR];
} zuo_trie_node_t;

typedef struct {
  zuo_t obj;
  zuo_t *name;
  zuo_t *val;
} zuo_variable_t;

typedef zuo_t *(*zuo_dispatcher_proc_t)(void *proc, zuo_t *arguments);

typedef struct {
  zuo_t obj;
  zuo_dispatcher_proc_t dispatcher;
  void *proc;
  zuo_int32_t arity_mask;
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
  zuo_handle_closed_status,
  zuo_handle_process_running_status,
  zuo_handle_process_done_status,
} zuo_handle_status_t;

typedef struct zuo_handle_t {
  zuo_t obj;
  union {
    struct {
      zuo_raw_handle_t handle;
      zuo_handle_status_t status;
    } h;
    zuo_t *forward; /* make sure the object is big enough */
  } u;
} zuo_handle_t;

#define ZUO_HANDLE_RAW(obj) (((zuo_handle_t *)(obj))->u.h.handle)

typedef struct {
  zuo_t obj;
  zuo_t *tag;
  zuo_t *val;
} zuo_opaque_t;

typedef enum {
  zuo_apply_cont,
  zuo_begin_cont,
  zuo_let_cont,
  zuo_if_cont,
  zuo_done_cont
} zuo_cont_tag_t;

typedef struct zuo_cont_t {
  zuo_t obj;
  zuo_cont_tag_t tag;
  zuo_t *data;
  zuo_t *env;
  zuo_t *in_proc; /* string or #f */
  zuo_t *next;
} zuo_cont_t;

/* GC roots: */
static struct {
  /* Roots kept in an image dump: */
  struct {
    /* singleton values */
    zuo_t *o_undefined; /* internal use only */
    zuo_t *o_true;
    zuo_t *o_false;
    zuo_t *o_null;
    zuo_t *o_eof;
    zuo_t *o_void;
    zuo_t *o_empty_hash;
  
    /* sentinel for the interpreter */
    zuo_t *o_done_k;

    /* intrinsic functions that manipulate interpreter state */
    zuo_t *o_apply;
    zuo_t *o_call_cc;

    /* symbol table, root environment, and modules */
    zuo_t *o_intern_table;
    zuo_t *o_top_env;
    zuo_t *o_modules;

    /* symbols for kernel core forms */
    zuo_t *o_quote_symbol;
    zuo_t *o_lambda_symbol;
    zuo_t *o_let_symbol;
    zuo_t *o_begin_symbol;
    zuo_t *o_if_symbol;
  } image;

  /* Roots that are not included in a dump: */
  struct {
    /* CEK-style interp--continue registers */
    zuo_t *o_interp_e;
    zuo_t *o_interp_v;
    zuo_t *o_interp_env;
    zuo_t *o_interp_k;
    zuo_t *o_interp_in_proc; /* used for a stack trace on error */

    /* for cycle detection */
    zuo_t *o_pending_modules;

#ifdef ZUO_UNIX
    /* process status table and fd table */
    zuo_t *o_pid_table;
    zuo_t *o_fd_table;
#endif

    /* startup info */
    zuo_t *o_library_path;
    zuo_t *o_current_directory;
    zuo_t *o_runtime_env;
  
    /* data to save across a GC that's possibly triggered by interp */
    zuo_t *o_stash;
  } runtime;
} zuo_roots;

#define z zuo_roots.image
#define Z zuo_roots.runtime

static zuo_int32_t zuo_symbol_count = 0;

/*======================================================================*/
/* sanity checks                                                        */
/*======================================================================*/

void zuo_panic(const char *s) {
  fprintf(stderr, "%s\n", s);
  exit(1);
}

void zuo_check_sanity() {
  if (sizeof(zuo_int32_t) != 4)
    zuo_panic("wrong int32 size");
  if (sizeof(zuo_int_t) != 8)
    zuo_panic("wrong int size");
  if ((void*)&(((zuo_string_t *)NULL)->len) != (void*)&(((zuo_forwarded_t *)NULL)->forward))
    zuo_panic("string len field misplaced");
}

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

static zuo_int_t object_size(zuo_int32_t tag, zuo_int_t maybe_string_len) {
  switch(tag) {
  case zuo_singleton_tag:
    return sizeof(zuo_forwarded_t);
  case zuo_integer_tag:
    return sizeof(zuo_integer_t);
  case zuo_string_tag:
    return ZUO_STRING_ALLOC_SIZE(maybe_string_len);
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
  case zuo_opaque_tag:
    return sizeof(zuo_opaque_t);
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
    zuo_int_t size = object_size(obj->tag, ZUO_STRING_LEN(obj));
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
    zuo_update(&((zuo_primitive_t *)obj)->name);
    break;
  case zuo_closure_tag:
    zuo_update(&((zuo_closure_t *)obj)->lambda);
    zuo_update(&((zuo_closure_t *)obj)->env);
    break;
  case zuo_opaque_tag:
    zuo_update(&((zuo_opaque_t *)obj)->tag);
    zuo_update(&((zuo_opaque_t *)obj)->val);
    break;
  case zuo_cont_tag:
    zuo_update(&((zuo_cont_t *)obj)->data);
    zuo_update(&((zuo_cont_t *)obj)->env);
    zuo_update(&((zuo_cont_t *)obj)->in_proc);
    zuo_update(&((zuo_cont_t *)obj)->next);
    break;
  }
}

static void zuo_trace_objects() {
  zuo_int_t trace_offset = 0;

  while (trace_offset < allocation_offset) {
    zuo_t *obj = (zuo_t *)((char *)to_space + trace_offset);
    zuo_trace(obj);
    trace_offset += object_size(obj->tag, ZUO_STRING_LEN(obj));
  }
}

static void zuo_finish_gc(void *old_space, old_space_t *old_old_spaces) {
  free(old_space);
  while (old_old_spaces != NULL) {
    old_space_t *next_old_old_spaces = old_old_spaces->next;
    free(old_old_spaces->space);
    free(old_old_spaces);
    old_old_spaces = next_old_old_spaces;
  }

  total_allocation = allocation_offset;
  gc_threshold = total_allocation * 2;
  if (gc_threshold < MIN_HEAP_SIZE)
    gc_threshold = MIN_HEAP_SIZE;
}

static void zuo_collect() {
  void *old_space = to_space;
  old_space_t *old_old_spaces = old_spaces;

  old_spaces = NULL;
  heap_size = total_allocation;
  to_space = malloc(heap_size);
  allocation_offset = 0;

  /* roots */
  {
    zuo_t **p = (zuo_t **)&zuo_roots;
    int i, len;
    len = sizeof(zuo_roots) / sizeof(zuo_t*);
    for (i = 0; i < len; i++) {
      zuo_update(p+i);
    }
  }

  /* collect */
  zuo_trace_objects();

  /* cleanup */
  zuo_finish_gc(old_space, old_old_spaces);
}

static void zuo_check_collect() {
  if (total_allocation >= gc_threshold)
    zuo_collect();
}

static void zuo_replace_heap(void *space, zuo_int_t size, zuo_int_t offset) {
  allocation_offset = offset;
  
  zuo_finish_gc(to_space, old_spaces);

  old_spaces = NULL;
  heap_size = size;
  to_space = space;
}

/*======================================================================*/
/* table of primitives used for fasl                                    */
/*======================================================================*/

#define ZUO_MAX_PRIMITIVE_COUNT 128
static zuo_primitive_t zuo_registered_prims[ZUO_MAX_PRIMITIVE_COUNT];
static int zuo_registered_prim_count;

static void zuo_register_primitive(zuo_dispatcher_proc_t dispatcher, void *proc, zuo_int32_t arity_mask) {
  if (zuo_registered_prim_count == ZUO_MAX_PRIMITIVE_COUNT)
    zuo_panic("primitive table is too small");

  zuo_registered_prims[zuo_registered_prim_count].dispatcher = dispatcher;
  zuo_registered_prims[zuo_registered_prim_count].proc = proc;
  zuo_registered_prims[zuo_registered_prim_count].arity_mask = arity_mask;
  zuo_registered_prim_count++;
}

static zuo_int32_t zuo_primitive_to_id(zuo_primitive_t *obj) {
  int i;
  for (i = 0; i < zuo_registered_prim_count; i++)
    if (obj->proc == zuo_registered_prims[i].proc)
      return i;
  zuo_panic("could not find primitive");
  return 0;
}

static void zuo_id_to_primitive(zuo_int32_t i, zuo_primitive_t *obj) {
  obj->dispatcher = zuo_registered_prims[i].dispatcher;
  obj->proc = zuo_registered_prims[i].proc;
  obj->arity_mask = zuo_registered_prims[i].arity_mask;
}

/*======================================================================*/
/* heap fasl                                                            */
/*======================================================================*/

typedef struct {
  zuo_int32_t magic;
  zuo_int32_t map_size;      /* in int32s */
  zuo_int32_t image_size;    /* in int32s */
  zuo_int32_t symbol_count;
} zuo_fasl_header_t;

static zuo_int32_t zuo_magic() {
  /* gets magic specific to the current machine's endianness */
  return *(zuo_int32_t *)"\0zuo";
}

typedef enum {
  zuo_fasl_out,
  zuo_fasl_in
} zuo_fasl_mode_t;

typedef struct {
  zuo_fasl_mode_t mode;
} zuo_fasl_stream_t;

typedef struct {
  zuo_fasl_stream_t stream;
  void *heap, *shadow_heap;
  zuo_int32_t heap_size;
  
  zuo_t **objs;
  zuo_int32_t *map; /* offset in image */
  zuo_int32_t map_size, map_offset;

  zuo_int32_t *image;
  zuo_int32_t image_size, image_offset;
} zuo_fasl_stream_out_t;

typedef struct {
  zuo_fasl_stream_t stream;
  void *heap;
  zuo_int32_t heap_size;
  zuo_int32_t *map;
  zuo_int32_t *image;
  zuo_int32_t offset;
} zuo_fasl_stream_in_t;

static void zuo_ensure_image_room(zuo_fasl_stream_out_t *stream) {
  if (stream->image_size == stream->image_offset) {
    zuo_int32_t *new_image = malloc(stream->image_size * 2 * sizeof(zuo_int32_t));
    memcpy(new_image, stream->image, (stream->image_offset) * sizeof(zuo_int32_t));
    free(stream->image);
    stream->image = new_image;
    stream->image_size *= 2;
  }
}

static void zuo_ensure_map_room(zuo_fasl_stream_out_t *stream) {
  if (stream->map_size == stream->map_offset) {
    zuo_t **new_objs = malloc(stream->map_size * 2 * sizeof(zuo_t*));
    zuo_int32_t *new_map = malloc(stream->map_size * 2 * sizeof(zuo_int32_t));
    memcpy(new_objs, stream->objs, (stream->map_offset) * sizeof(zuo_t*));
    memcpy(new_map, stream->map, (stream->map_offset) * sizeof(zuo_int32_t));
    free(stream->objs);
    free(stream->map);
    stream->objs = new_objs;
    stream->map = new_map;
    stream->map_size *= 2;
  }
}

static void zuo_fasl_ref(zuo_t **_obj, zuo_fasl_stream_t *_stream) {
  if (_stream->mode == zuo_fasl_in) {
    zuo_fasl_stream_in_t *stream = (zuo_fasl_stream_in_t *)_stream;
    zuo_int32_t delta = stream->map[stream->image[stream->offset++]];
    *_obj = (zuo_t *)((char *)stream->heap + delta);
  } else {
    zuo_fasl_stream_out_t *stream = (zuo_fasl_stream_out_t *)_stream;
    zuo_t *obj = *_obj;
    zuo_int32_t delta = ((char *)obj) - ((char *)stream->heap);
    zuo_t *shadow_obj = (zuo_t *)(((char *)stream->shadow_heap) + delta);

    ASSERT((delta >= 0) && (delta < stream->heap_size));

    zuo_ensure_image_room(stream);

    if (shadow_obj->tag == zuo_forwarded_tag) {
      stream->image[stream->image_offset++] = ((zuo_fasl_forwarded_t *)shadow_obj)->index;
    } else {
      zuo_ensure_map_room(stream);

      shadow_obj->tag = zuo_forwarded_tag;
      ((zuo_fasl_forwarded_t *)shadow_obj)->index = stream->map_offset;

      stream->image[stream->image_offset++] = stream->map_offset;
      stream->objs[stream->map_offset++] = *_obj;
    }
  }
}

static void zuo_fasl_int32(zuo_int32_t *_i, zuo_fasl_stream_t *_stream) {
  if (_stream->mode == zuo_fasl_in) {
    zuo_fasl_stream_in_t *stream = (zuo_fasl_stream_in_t *)_stream;
    *_i = stream->image[stream->offset++];
  } else {
    zuo_fasl_stream_out_t *stream = (zuo_fasl_stream_out_t *)_stream;
    zuo_ensure_image_room(stream);
    stream->image[stream->image_offset++] = *_i;
  }
}

#define BUILD_INT(lo, hi) (((zuo_int_t)(hi) << 32) | ((zuo_int_t)(lo) & 0xFFFFFFFF))

static void zuo_fasl_int(zuo_int_t *_i, zuo_fasl_stream_t *_stream) {
  zuo_int32_t lo, hi;
  lo = *_i & (zuo_int_t)0xFFFFFFFF;
  hi = *_i >> 32;
  zuo_fasl_int32(&lo, _stream);
  zuo_fasl_int32(&hi, _stream);
  *_i = BUILD_INT(lo, hi);
}

static void zuo_fasl_char(unsigned char *_c, zuo_fasl_stream_t *stream) {
  zuo_int32_t i = *_c;
  zuo_fasl_int32(&i, stream);
  *_c = i;
}

static void zuo_fasl(zuo_t *obj, zuo_fasl_stream_t *stream) {
  {
    zuo_int32_t tag = obj->tag;
    zuo_fasl_int32(&tag, stream);
    obj->tag = tag;
  }
  
  switch(obj->tag) {
  case zuo_singleton_tag:
    break;
  case zuo_integer_tag:
    zuo_fasl_int(&((zuo_integer_t *)obj)->i, stream);
    break;
  case zuo_string_tag:
    {
      int i;
      zuo_fasl_int(&((zuo_string_t *)obj)->len, stream);
      for (i = 0; i < ((zuo_string_t *)obj)->len; i++)
        zuo_fasl_char(&((zuo_string_t *)obj)->s[i], stream);
    }
    break;
  case zuo_pair_tag:
    zuo_fasl_ref(&((zuo_pair_t *)obj)->car, stream);
    zuo_fasl_ref(&((zuo_pair_t *)obj)->cdr, stream);
    break;
  case zuo_symbol_tag:
    zuo_fasl_int32(&((zuo_symbol_t *)obj)->id, stream);
    zuo_fasl_ref(&((zuo_symbol_t *)obj)->str, stream);
    break;
  case zuo_trie_node_tag:
    {
      int i;
      /* restore assumes that a string starts with its length */
      zuo_fasl_ref(&((zuo_trie_node_t *)obj)->key, stream);
      zuo_fasl_ref(&((zuo_trie_node_t *)obj)->val, stream);
      for (i = 0; i < ZUO_TRIE_BFACTOR; i++)
        zuo_fasl_ref(&((zuo_trie_node_t *)obj)->next[i], stream);
    }
    break;
  case zuo_variable_tag:
    {
      zuo_fasl_ref(&((zuo_variable_t *)obj)->name, stream);
      zuo_fasl_ref(&((zuo_variable_t *)obj)->val, stream);
      break;
    }
  case zuo_primitive_tag:
    {
      zuo_int32_t primitive_id;
      if (stream->mode == zuo_fasl_out) {
        primitive_id = zuo_primitive_to_id((zuo_primitive_t *)obj);
        zuo_fasl_int32(&primitive_id, stream);
      } else {
        zuo_fasl_int32(&primitive_id, stream);
        zuo_id_to_primitive(primitive_id, (zuo_primitive_t *)obj);
      }

      zuo_fasl_ref(&((zuo_primitive_t *)obj)->name, stream);
    }
    break;
  case zuo_closure_tag:
    zuo_fasl_ref(&((zuo_closure_t *)obj)->lambda, stream);
    zuo_fasl_ref(&((zuo_closure_t *)obj)->env, stream);
    break;
  case zuo_opaque_tag:
    zuo_fasl_ref(&((zuo_opaque_t *)obj)->tag, stream);
    zuo_fasl_ref(&((zuo_opaque_t *)obj)->val, stream);
    break;
  case zuo_cont_tag:
    zuo_fasl_ref(&((zuo_cont_t *)obj)->data, stream);
    zuo_fasl_ref(&((zuo_cont_t *)obj)->env, stream);
    zuo_fasl_ref(&((zuo_cont_t *)obj)->in_proc, stream);
    zuo_fasl_ref(&((zuo_cont_t *)obj)->next, stream);
    break;
  case zuo_handle_tag:
    zuo_panic("cannot dump heap with handles");
    break;
  case zuo_forwarded_tag:
    ASSERT(0);
    break;
  }
}

static void zuo_fasl_roots(zuo_fasl_stream_t *stream) {
  zuo_t **p = (zuo_t **)&zuo_roots.image;
  int i, len = sizeof(zuo_roots.image) / sizeof(zuo_t*);
  for (i = 0; i < len; i++)
    zuo_fasl_ref(p+i, stream);
}

static char *zuo_fasl_dump(zuo_int_t *_len) {
  zuo_fasl_stream_out_t stream;
  zuo_int32_t total_size, header_size = sizeof(zuo_fasl_header_t) / sizeof(zuo_int32_t);
  zuo_int32_t *dump;
  zuo_int32_t map_done;

  /* make sure everything is in contiguous memory: */
  zuo_collect();

  stream.stream.mode = zuo_fasl_out;

  stream.heap = to_space;
  stream.heap_size = allocation_offset;
  stream.shadow_heap = malloc(allocation_offset);
  memset(stream.shadow_heap, 0, allocation_offset);

  stream.map_size = 1024;
  stream.map_offset = 0;
  stream.map = malloc(stream.map_size * sizeof(zuo_int32_t));
  stream.objs = malloc(stream.map_size * sizeof(zuo_t*));

  stream.image_size = 4096;
  stream.image_offset = 0;
  stream.image = malloc(stream.image_size * sizeof(zuo_int32_t));
  
  zuo_fasl_roots(&stream.stream);

  /* analogous to the collector's trace_objects loop: */
  for (map_done = 0; map_done < stream.map_offset; map_done++) {
    zuo_t *obj = stream.objs[map_done];

    /* register location of this object in the image */
    zuo_int32_t delta = ((char *)obj) - ((char *)stream.heap);
    zuo_t *shadow_obj = (zuo_t *)(((char *)stream.shadow_heap) + delta);
    ASSERT(shadow_obj->tag == zuo_forwarded_tag);
    stream.map[((zuo_fasl_forwarded_t *)shadow_obj)->index] = stream.image_offset;

    zuo_fasl(obj, &stream.stream);
  }
  
  total_size = header_size + stream.map_offset + stream.image_offset;
  dump = malloc(total_size * sizeof(zuo_int32_t));

  ((zuo_fasl_header_t *)dump)->magic = zuo_magic();
  ((zuo_fasl_header_t *)dump)->map_size = stream.map_offset;
  ((zuo_fasl_header_t *)dump)->image_size = stream.image_offset;
  ((zuo_fasl_header_t *)dump)->symbol_count = zuo_symbol_count;
  memcpy(dump + header_size, stream.map, stream.map_offset * sizeof(zuo_int32_t));
  memcpy(dump + header_size + stream.map_offset, stream.image, stream.image_offset * sizeof(zuo_int32_t));

  free(stream.image);
  free(stream.objs);
  free(stream.map);
  free(stream.shadow_heap);

  *_len = total_size * sizeof(zuo_int32_t);
  return (char *)dump;
}

#define SWAP_ENDIAN(n) \
  ((((n) & 0xFF) << 24) | (((n) & 0xFF00) << 8) | (((n) & 0xFF0000) >> 8) | (((n) >> 24) & 0xFF))

static void zuo_fasl_restore(char *dump_in, zuo_int_t len) {
  zuo_fasl_stream_in_t stream;
  zuo_int32_t *dump = (zuo_int32_t *)dump_in, i, map_len, alloc_factor = 2;
  zuo_int32_t header_size = sizeof(zuo_fasl_header_t) / sizeof(zuo_int32_t);
  zuo_int32_t magic = zuo_magic();

  if (((zuo_fasl_header_t *)dump)->magic != magic) {
    if (((zuo_fasl_header_t *)dump)->magic == SWAP_ENDIAN(magic)) {
      /* adapt little-endian to big-endian, or vice versa */
      for (i = 0; i < len / sizeof(zuo_int32_t); i++)
        dump[i] = SWAP_ENDIAN(dump[i]);
    } else
      zuo_panic("image does not start with zuo magic");
  }

  map_len = ((zuo_fasl_header_t *)dump)->map_size;

  stream.stream.mode = zuo_fasl_in;

  stream.map = dump + header_size;
  stream.image = dump + header_size + map_len;
  stream.heap_size = 0;
  stream.offset = 0;
  
  /* compure heap size and replace image offsets with heap offsets */
  for (i = 0; i < map_len; i++) {
    zuo_int32_t delta = stream.map[i];
    zuo_int32_t tag = stream.image[delta];
    zuo_int_t sz;

    ASSERT((tag >= 0) && (tag < zuo_forwarded_tag));
    
    if (tag == zuo_string_tag)
      sz = object_size(tag, BUILD_INT(stream.image[delta+1], stream.image[delta+2]));
    else
      sz = object_size(tag, 0);

    stream.map[i] = stream.heap_size;
    stream.heap_size += sz;
  }

  stream.heap = malloc(stream.heap_size * alloc_factor);
  
  zuo_fasl_roots(&stream.stream);

  for (i = 0; i < map_len; i++) {
    zuo_t *obj = (zuo_t *)((char *)stream.heap + stream.map[i]);
    zuo_fasl(obj, &stream.stream);
  }

  zuo_symbol_count = ((zuo_fasl_header_t *)dump)->symbol_count;
  
  zuo_replace_heap(stream.heap, stream.heap_size * alloc_factor, stream.heap_size);
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
  memcpy(&obj->s, str, len);
  obj->s[len] = 0;
  return (zuo_t *)obj;
}

static zuo_t *zuo_string(const char *str) {
  return zuo_sized_string(str, strlen(str));
}

static zuo_t *zuo_trie_node() {
  int i;
  zuo_trie_node_t *obj = (zuo_trie_node_t *)zuo_new(zuo_trie_node_tag, sizeof(zuo_trie_node_t));

  obj->count = 0;
  obj->key = z.o_undefined;
  obj->val = z.o_undefined;
  for (i = 0; i < ZUO_TRIE_BFACTOR; i++)
    obj->next[i] = z.o_undefined;

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

/* If `str_obj` is undefined, it's allocated as needed.
   If `str_obj` us false, the result can be false. */
static zuo_t *zuo_symbol_from_string(const char *in_str, zuo_t *str_obj) {
  const unsigned char *str = (const unsigned char *)in_str;
  zuo_int_t i;
  zuo_trie_node_t *node = (zuo_trie_node_t *)z.o_intern_table;

  for (i = 0; str[i]; i++) {
    int c = str[i], lo = c & ZUO_TRIE_BFACTOR_MASK, hi = c >> ZUO_TRIE_BFACTOR_BITS;
    if (node->next[lo] == z.o_undefined) {
      if (str_obj == z.o_false)
        return z.o_false;
      node->next[lo] = zuo_trie_node();
    }
    node = (zuo_trie_node_t *)node->next[lo];
    if (node->next[hi] == z.o_undefined)
      node->next[hi] = zuo_trie_node();
    node = (zuo_trie_node_t *)node->next[hi];
  }

  if (node->val == z.o_undefined) {
    if (str_obj == z.o_false)
      return z.o_false;
    else if (str_obj == z.o_undefined)
      node->val = zuo_make_symbol(in_str);
    else
      node->val = zuo_make_symbol_from_string(str_obj);
  }
  /* the symbol table doesn't use the `key` field */
  
  return node->val;
}

static zuo_t *zuo_symbol(const char *in_str) {
  return zuo_symbol_from_string(in_str, z.o_undefined);
}

static zuo_t *zuo_variable(zuo_t *name) {
  zuo_variable_t *obj = (zuo_variable_t *)zuo_new(zuo_variable_tag, sizeof(zuo_variable_t));
  obj->name = name;
  obj->val = z.o_undefined;
  return (zuo_t *)obj;
}

static zuo_t *zuo_primitive(zuo_dispatcher_proc_t dispatcher, void *proc, zuo_int32_t arity_mask, zuo_t *name) {
  zuo_register_primitive(dispatcher, proc, arity_mask);
  /* if `name` is undefined, we're just registering a primitive to be used for an image */
  if (name == z.o_undefined)
    return z.o_undefined;
  else {
    zuo_primitive_t *obj = (zuo_primitive_t *)zuo_new(zuo_primitive_tag, sizeof(zuo_primitive_t));
    obj->dispatcher = dispatcher;
    obj->proc = proc;
    obj->arity_mask = arity_mask;
    obj->name = name;
    return (zuo_t *)obj;
  }
}

static zuo_t *zuo_closure(zuo_t *lambda, zuo_t *env) {
  zuo_closure_t *obj = (zuo_closure_t *)zuo_new(zuo_closure_tag, sizeof(zuo_closure_t));
  obj->lambda = lambda;
  obj->env = env;
  return (zuo_t *)obj;
}

static zuo_t *zuo_handle(zuo_raw_handle_t handle, zuo_handle_status_t status) {
  zuo_handle_t *obj = (zuo_handle_t *)zuo_new(zuo_handle_tag, sizeof(zuo_handle_t));
  obj->u.h.handle = handle;
  obj->u.h.status = status;
  return (zuo_t *)obj;
}

static zuo_t *zuo_opaque(zuo_t *tag, zuo_t *val) {
  zuo_opaque_t *obj = (zuo_opaque_t *)zuo_new(zuo_opaque_tag, sizeof(zuo_opaque_t));
  obj->tag = tag;
  obj->val = val;
  return (zuo_t *)obj;
}

static zuo_t *zuo_cont(zuo_cont_tag_t tag, zuo_t *data, zuo_t *env, zuo_t *in_proc, zuo_t *next) {
  zuo_cont_t *obj = (zuo_cont_t *)zuo_new(zuo_cont_tag, sizeof(zuo_cont_t));
  obj->tag = tag;
  obj->data = data;
  obj->env = env;
  obj->in_proc = in_proc;
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
    if (trie == z.o_undefined) return z.o_undefined;
    id = id >> ZUO_TRIE_BFACTOR_BITS;
  }

  return ((zuo_trie_node_t *)trie)->val;
}

static zuo_t *zuo_trie_lookup(zuo_t *trie, zuo_t *sym) {
  return trie_lookup(trie, ((zuo_symbol_t *)sym)->id);
}

/* trie mutation, used only for the symbol table and inital env */
static void trie_set(zuo_t *trie, zuo_int_t id, zuo_t *key, zuo_t *val) {
  while (id > 0) {
    zuo_t *next = ((zuo_trie_node_t *)trie)->next[id & ZUO_TRIE_BFACTOR_MASK];
    if (next == z.o_undefined) {
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
  ASSERT(trie_lookup(trie, ((zuo_symbol_t *)sym)->id) == z.o_undefined);
  trie_set(trie, ((zuo_symbol_t *)sym)->id, sym, val);
  ((zuo_trie_node_t *)trie)->count++;
}

static zuo_trie_node_t *trie_clone(zuo_t *trie) {
  zuo_trie_node_t *new_trie = (zuo_trie_node_t *)zuo_new(zuo_trie_node_tag, sizeof(zuo_trie_node_t));
  memcpy(new_trie, trie, sizeof(zuo_trie_node_t));
  return new_trie;
}

static zuo_t *trie_extend(zuo_t *trie, zuo_int_t id, zuo_t *key, zuo_t *val, int *added) {
  zuo_trie_node_t *new_trie;

  if (trie == z.o_undefined) {
    new_trie = (zuo_trie_node_t *)zuo_trie_node();
    trie = (zuo_t *)new_trie;
    *added = 1;
  } else
    new_trie = trie_clone(trie);

  if (id > 0) {
    int i = id & ZUO_TRIE_BFACTOR_MASK;
    new_trie->next[i] = trie_extend(((zuo_trie_node_t *)trie)->next[i], id >> ZUO_TRIE_BFACTOR_BITS, key, val, added);
    new_trie->count += *added;
  } else {
    if (new_trie->val == z.o_undefined) *added = 1;
    new_trie->count += *added;
    new_trie->key = key;
    new_trie->val = val;
  }

  return (zuo_t *)new_trie;
}

static zuo_t *zuo_trie_extend(zuo_t *trie, zuo_t *sym, zuo_t *val) {
  int added = 0;
  return trie_extend(trie, ((zuo_symbol_t *)sym)->id, sym, val, &added);
}

static zuo_t *trie_remove(zuo_t *trie, zuo_int_t id, int depth) {
  zuo_trie_node_t *new_trie;

  if (trie == z.o_undefined)
    return z.o_undefined;
  else if (id > 0) {
    int i = id & ZUO_TRIE_BFACTOR_MASK;
    zuo_t *sub_trie = trie_remove(((zuo_trie_node_t *)trie)->next[i], id >> ZUO_TRIE_BFACTOR_BITS, depth+1);
    if (sub_trie == ((zuo_trie_node_t *)trie)->next[i])
      return trie;

    new_trie = trie_clone(trie);
    ((zuo_trie_node_t *)new_trie)->next[i] = sub_trie;
    new_trie->count -= 1;
  } else {
    if (((zuo_trie_node_t *)trie)->val == z.o_undefined)
      return trie;

    new_trie = trie_clone(trie);
    new_trie->count -= 1;
    new_trie->key = z.o_undefined;
    new_trie->val = z.o_undefined;
  }

  if ((depth > 0)
      && new_trie->key == z.o_undefined
      && new_trie->val == z.o_undefined) {
    int i;
    for (i = 0; i < ZUO_TRIE_BFACTOR; i++)
      if (new_trie->next[i] != z.o_undefined)
        return (zuo_t *)new_trie;
    return z.o_undefined;
  }

  return (zuo_t *)new_trie;
}

static zuo_t *zuo_trie_remove(zuo_t *trie, zuo_t *sym) {
  return trie_remove(trie, ((zuo_symbol_t *)sym)->id, 0);
}

static int zuo_trie_keys_subset_p(zuo_t *trie1_in, zuo_t *trie2_in) {
  if (trie1_in == trie2_in)
    return 1;
  else if (trie1_in == z.o_undefined)
    return 1;
  else if (trie2_in == z.o_undefined)
    return 0;
  else {
    zuo_trie_node_t *trie1 = (zuo_trie_node_t *)trie1_in;
    zuo_trie_node_t *trie2 = (zuo_trie_node_t *)trie2_in;
    int i;

    if (trie1->count > trie2->count)
      return 0;

    for (i = 0; i < ZUO_TRIE_BFACTOR; i++) {
      if (!zuo_trie_keys_subset_p(trie1->next[i], trie2->next[i]))
        return 0;
    }

    return 1;
  }
}

static zuo_t *zuo_trie_keys(zuo_t *trie_in, zuo_t *accum) {
  int i;
  zuo_trie_node_t *trie = (zuo_trie_node_t *)trie_in;

  if (trie->key != z.o_undefined)
    accum = zuo_cons(trie->key, accum);

  for (i = 0; i < ZUO_TRIE_BFACTOR; i++) {
    if (trie->next[i] != z.o_undefined)
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
  if (obj == z.o_undefined)
    out_string(out, "#<undefined>");
  else if (obj == z.o_null) {
    if (mode == zuo_print_mode)
      out_string(out, "'");
    out_string(out, "()");
  } else if (obj == z.o_false)
    out_string(out, "#f");
  else if (obj == z.o_true)
    out_string(out, "#t");
  else if (obj == z.o_eof)
    out_string(out, "#<eof>");
  else if (obj == z.o_void)
    out_string(out, "#<void>");
  else if (obj->tag == zuo_integer_tag) {
    zuo_int_t i = ZUO_INT_I(obj), di, n, w, add_back = 0;
    if (i < 0) {
      out_char(out, '-');
      i = (zuo_int_t)(0-(zuo_uint_t)i);
      if (i < 0) {
        /* min int */
        i = -(i+1);
        add_back = 1;
      }
    }
    di = i / 10;
    for (n = 1, w = 1; di >= n; n *= 10, w++);
    while (n >= 1) {
      out_char(out, '0' + (i / n));
      i = i - ((i / n) * n);
      n /= 10;
      i += add_back;
      add_back = 0;
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
    if ((mode == zuo_display_mode)
        || (obj == zuo_symbol_from_string(ZUO_STRING_PTR(((zuo_symbol_t *)obj)->str), z.o_false))) {
      if (mode == zuo_print_mode)
        out_char(out, '\'');
      zuo_out(out, ((zuo_symbol_t *)obj)->str, depth, zuo_display_mode);
    } else {
      out_string(out, "#<symbol:");
      zuo_out(out, ((zuo_symbol_t *)obj)->str, depth, zuo_display_mode);
      out_string(out, ">");
    }
  } else if (depth >= ZUO_RECUR_LIMIT) {
    out_string(out, "...");
  } else if (obj->tag == zuo_pair_tag) {
    zuo_pair_t *p = (zuo_pair_t *)obj;
    out_char(out, '(');
    if (mode == zuo_print_mode) {
      zuo_t *p2 = (zuo_t *)p;
      while (p2->tag == zuo_pair_tag)
        p2 = ((zuo_pair_t *)p2)->cdr;
      if (p2 == z.o_null)
        out_string(out, "list ");
      else if (_zuo_cdr(p)->tag != zuo_pair_tag)
        out_string(out, "cons ");
      else
        out_string(out, "list* ");
    }
    zuo_out(out, p->car, depth+1, mode);
    while (p->cdr->tag == zuo_pair_tag) {
      p = (zuo_pair_t *)p->cdr;
      out_char(out, ' ');
      zuo_out(out, p->car, depth+1, mode);
    }
    if (p->cdr != z.o_null) {
      if (mode != zuo_print_mode) {
        out_char(out, ' ');
        out_char(out, '.');
      }
      out_char(out, ' ');
      zuo_out(out, p->cdr, depth+1, mode);
    }
    out_char(out, ')');
  } else if (obj->tag == zuo_primitive_tag) {
    out_string(out, "#<procedure:");
    zuo_out(out, ((zuo_primitive_t *)obj)->name, depth+1, zuo_display_mode);
    out_string(out, ">");
  } else if (obj->tag == zuo_closure_tag) {
    zuo_t *dd = ZUO_CDR(ZUO_CDR(((zuo_closure_t *)obj)->lambda));
    out_string(out, "#<procedure");
    if (ZUO_CAR(dd)->tag == zuo_string_tag) {
      out_string(out, ":");
      zuo_out(out, ZUO_CAR(dd), depth+1, zuo_display_mode);
    }
    out_string(out, ">");
  } else if (obj == z.o_apply) {
    out_string(out, "#<procedure:apply>");
  } else if (obj == z.o_call_cc) {
    out_string(out, "#<procedure:call/cc>");
  } else if (obj->tag == zuo_opaque_tag) {
    out_string(out, "#<");
    zuo_out(out, ((zuo_opaque_t *)obj)->tag, depth+1, zuo_display_mode);
    out_string(out, ">");
  } else if (obj->tag == zuo_trie_node_tag) {
    zuo_t *keys = zuo_trie_keys(obj, z.o_null);
    if (mode == zuo_print_mode) {
      out_string(out, "(hash");
      if (keys != z.o_null)
        out_string(out, " ");
    } else
      out_string(out, "#hash(");
    while (keys != z.o_null) {
      zuo_t *key = ZUO_CAR(keys);
      if (mode != zuo_print_mode)
        out_string(out, "(");
      zuo_out(out, key, depth+1, mode);
      if (mode == zuo_print_mode)
        out_string(out, " ");
      else
        out_string(out, " . ");
      zuo_out(out, zuo_trie_lookup(obj, key), depth+1, mode);
      if (mode != zuo_print_mode)
        out_string(out, ")");
      keys = ZUO_CDR(keys);
      if (keys != z.o_null)
        out_string(out, " ");
    }
    out_string(out, ")");
  } else if (obj->tag == zuo_handle_tag) {
    out_string(out, "#<handle>");
  } else if (obj->tag == zuo_cont_tag) {
    out_string(out, "#<continuation>");
  } else if (obj->tag == zuo_variable_tag) {
    out_string(out, "#<variable:");
    zuo_out(out, ((zuo_variable_t *)obj)->name, depth+1, zuo_display_mode);
    out_string(out, ">");
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
    if ((mode != zuo_display_mode) && (objs != z.o_null))
      out_char(&out, ' ');
  }
  if (objs != z.o_null)
    zuo_out(&out, objs, 0, mode);

  out_char(&out, 0);
  
  str = zuo_string(out.s);
  out_done(&out);
  return str;
}

static void zuo_fprint(FILE *out, zuo_t *obj) {
  zuo_fout(out, obj, zuo_print_mode);
}

static void zuo_fdisplay(FILE *out, zuo_t *obj) {
  zuo_fout(out, obj, zuo_display_mode);
}

static void zuo_fwrite(FILE *out, zuo_t *obj) {
  zuo_fout(out, obj, zuo_write_mode);
}

static void done_dump_name(zuo_t *showed_name, int repeats) {
  if (showed_name != z.o_false) {
    if (repeats > 0) {
      fprintf(stderr, " {%d}", repeats+1);
      repeats = 0;
    }
    fprintf(stderr, "\n");
  }
}

static void zuo_stack_dump() {
  zuo_t *k = Z.o_interp_k;
  zuo_t *showed_name  = z.o_false;
  int repeats = 0;
  
  while (k != z.o_done_k) {
    zuo_t *name = ((zuo_cont_t *)k)->in_proc;
    if (name->tag == zuo_string_tag) {
      if (name == showed_name)
        repeats++;
      else {
        done_dump_name(showed_name, repeats);
        repeats = 0;
        showed_name = name;
        fprintf(stderr, " in %s", ZUO_STRING_PTR(name));
      }
    }
    k = ((zuo_cont_t *)k)->next;
  }
  done_dump_name(showed_name, repeats);
}

static void zuo_fail(const char *str) {
  fprintf(stderr, "%s\n", str);
  zuo_stack_dump();
  exit(1);
}

static void zuo_show_err1w(const char *who, const char *str, zuo_t *obj) {
  if (who != NULL)
    fprintf(stderr, "%s: ", who);
  fprintf(stderr, "%s: ", str);
  zuo_fprint(stderr, obj);
}

static void zuo_fail1w(const char *who, const char *str, zuo_t *obj) {
  zuo_show_err1w(who, str, obj);
  zuo_fail("");
}

static void zuo_fail1w_errno(const char *who, const char *str, zuo_t *obj) {
  const char *msg = strerror(errno);
  zuo_show_err1w(who, str, obj);
  fprintf(stderr, " (%s)", msg);
  zuo_fail("");
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

static const char *symbol_chars = "~!@#$%^&*-_=+:<>?/.";
static zuo_t *zuo_in(const unsigned char *s, zuo_int_t *_o, int depth);

static void zuo_read_fail(const unsigned char *s, zuo_int_t *_o, const char *msg) {
  fprintf(stderr, "read: %s at position %d", msg, (int)*_o);
  zuo_fail("");
}

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
      if (discard == z.o_eof)
        zuo_read_fail(s, _o, "end of file after comment hash-semicolon");      
    } else
      break;
  }
}

static int hex_value(unsigned const char *s, zuo_int_t *_o, int offset) {
  int c = s[*_o + offset];

  if ((c >= '0') && (c <= '9'))
    return c - '0';
  if ((c >= 'a') && (c <= 'f'))
    return c - 'a' + 10;
  if ((c >= 'A') && (c <= 'F'))
    return c - 'A' + 10;

  (*_o) += offset;
  zuo_read_fail(s, _o, "bad hex digit");
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
    zuo_read_fail(s, _o, "too nested");
  
  skip_whitespace(s, _o, depth);
  c = s[*_o];
  if (c == 0)
    return z.o_eof;
  else if ((c == '(') || (c == '[')) {
    int closer = ((c == '(') ? ')' : ']');
    zuo_t *car, *top_p;
    zuo_pair_t *p;
    (*_o)++;
    skip_whitespace(s, _o, depth);
    if (s[*_o] == closer) {
      (*_o)++;
      return z.o_null;
    }
    car = zuo_in(s, _o, depth+1);
    top_p = zuo_cons(car, z.o_null);
    p = (zuo_pair_t *)top_p;
    while (1) {
      skip_whitespace(s, _o, depth);
      if (s[*_o] == 0) {
        zuo_read_fail(s, _o, "missing closer");
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
        cdr = zuo_cons(cadr, z.o_null);
        p->cdr = cdr;
        p = (zuo_pair_t *)cdr;
      }
    }
    return top_p;
  } else if ((c == ')') || (c == ']')) {
    zuo_read_fail(s, _o, "unbalanced closer");
    return z.o_undefined;
  } else if (c == '#') {
    (*_o)++;
    if (peek_input(s, _o, "true")) {
      (*_o) += 4;
      return z.o_true;
    } else if (peek_input(s, _o, "false")) {
      (*_o) += 5;
      return z.o_false;
    } else if (peek_input(s, _o, "t")) {
      (*_o) += 1;
      return z.o_true;
    } else if (peek_input(s, _o, "f")) {
      (*_o) += 1;
      return z.o_false;
    } else if (peek_input(s, _o, ";")) {
      zuo_t *discard;
      (*_o) += 1;
      discard = zuo_in(s, _o, depth+1);
      if (discard == z.o_eof)
        zuo_read_fail(s, _o, "end of file after comment hash-semicolon");
      return zuo_in(s, _o, depth+1);
    } else {
      zuo_read_fail(s, _o, "bad hash mark");
      return z.o_undefined;
    }
  } else if (isdigit(c) || ((c == '-') && isdigit(s[(*_o)+1]))) {
    zuo_uint_t n;
    int neg = (c == '-');
    if (neg) (*_o)++;
    n = s[*_o] - '0';
    (*_o)++;
    while (isdigit(s[*_o])) {
      zuo_uint_t new_n = (10 * n) + (s[*_o] - '0');
      if (new_n < n)
        zuo_read_fail(s, _o, "integer overflow");
      n = new_n;
      (*_o)++;
    }
    if (neg) {
      n = 0 - n;
      if ((zuo_int_t)n > 0)
        zuo_read_fail(s, _o, "integer overflow");
      return zuo_integer((zuo_int_t)n);
    } else {
      if ((zuo_int_t)n < 0)
        zuo_read_fail(s, _o, "integer overflow");
      return zuo_integer((zuo_int_t)n);
    }
  } else if (c == '\'') {
    zuo_t *v;
    (*_o)++;
    v = zuo_in(s, _o, depth+1);
    if (v == z.o_eof)
      zuo_read_fail(s, _o, "end of file after quote");
    return zuo_cons(zuo_symbol("quote"), zuo_cons(v, z.o_null));
  } else if (c == '`') {
    zuo_t *v;
    (*_o)++;
    v = zuo_in(s, _o, depth+1);
    if (v == z.o_eof)
      zuo_read_fail(s, _o, "end of file after quasiquote");
    return zuo_cons(zuo_symbol("quasiquote"), zuo_cons(v, z.o_null));
  } else if (c == ',') {
    zuo_t *v;
    int splicing = 0;
    (*_o)++;
    if (s[*_o] == '@') {
      splicing = 1;
      (*_o)++;
    }
    v = zuo_in(s, _o, depth+1);
    if (v == z.o_eof)
      zuo_read_fail(s, _o, "end of file after unquote");
    return zuo_cons((splicing
                     ? zuo_symbol("unquote-splicing")
                     : zuo_symbol("unquote")),
                    zuo_cons(v, z.o_null));
  } else if ((c == '.') && !(isalpha(c) || strchr(symbol_chars, c))) {
    zuo_read_fail(s, _o, "misplaced `.`");
    return z.o_undefined;
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
        zuo_read_fail(s, _o, "missing closing doublequote");
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
        } else if (c2 == 'r') {
          s2[len++] = '\r';
          (*_o) += 2;
        } else if (c2 == 'x') {
          s2[len++] = (hex_value(s, _o, 2) << 4) + hex_value(s, _o, 3);
          (*_o) += 4;
        } else
          zuo_read_fail(s, _o, "bad character after backslash");
      } else if (c == '\n') {
        zuo_read_fail(s, _o, "newline in string literal");
      } else if (c == '\r') {
        zuo_read_fail(s, _o, "carriage return in string literal");
      } else {
        s2[len++] = c;
        (*_o)++;
      }
    }
    obj = zuo_sized_string(s2, len);
    free(s2);
    return obj;
  } else {
    char s[2];
    s[0] = c;
    s[1] = 0;
    zuo_fail1w("read", "unrecognized character", zuo_string(s));
    return z.o_null;
  }
}

static zuo_t *zuo_read_all_str(const char *s, zuo_int_t len, zuo_int_t start) {
  zuo_int_t o = start;
  zuo_t *first = z.o_null, *last = NULL, *p;
  zuo_int_t i;

  for (i = start; i < len; i++)
    if (s[i] == 0)
      zuo_fail("read-from-string-all: nul character in input");

  while (1) {
    zuo_t *obj = zuo_in((const unsigned char *)s, &o, 0);
    if (obj == z.o_eof)
      break;
    p = zuo_cons(obj, z.o_null);
    if (first == z.o_null)
      first = last = p;
    else {
      ((zuo_pair_t *)last)->cdr = p;
      last = p;
    }
  }
  
  return first;
}

static zuo_t *zuo_read_all(zuo_t *obj, zuo_t *start_i) {
  const char *who = "read-from-string-all";
  zuo_int_t len, start;
  check_string(who, obj);
  check_integer(who, start_i);
  len = ZUO_STRING_LEN(obj);
  start = ZUO_INT_I(start_i);
  if ((start < 0) || (start > len))
    zuo_fail1w(who, "starting index is out of bounds", start_i);
  return zuo_read_all_str(ZUO_STRING_PTR(obj), len, start);
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
      zuo_read_fail(s, &o, "expected #lang followed by a space");
  }
  for (j = 0; 1; j++) {
    int c = s[o+i+j];
    if (!zuo_is_symbol_module_char(c))
      break;
  }
  if (!j || !((s[o+i+j] == 0) || isspace(s[o+i+j])))
    zuo_read_fail(s, &o, "expected module library path after #lang");

  r = malloc(j+1);
  memcpy(r, s+o+i, j);
  r[j] = 0;

  *_post = o+i+j;

  return r;
}

/*======================================================================*/
/* primitive wrapper/dispatchers                                        */
/*======================================================================*/

static zuo_t *dispatch_primitive0(void *proc, zuo_t *args) {
  return ((zuo_t *(*)())proc)();
}

static zuo_t *zuo_primitive0(zuo_t *(*f)(), zuo_t *name) {
  return zuo_primitive(dispatch_primitive0, (void *)f, (1 << 0), name);
}

static zuo_t *dispatch_primitive1(void *proc, zuo_t *args) {
  return ((zuo_t *(*)(zuo_t *))proc)(ZUO_CAR(args));
}

static zuo_t *zuo_primitive1(zuo_t *(*f)(zuo_t *), zuo_t *name) {
  return zuo_primitive(dispatch_primitive1, (void *)f, (1 << 1), name);
}

static zuo_t *dispatch_primitive2(void *proc, zuo_t *args) {
  return ((zuo_t *(*)(zuo_t *, zuo_t *))proc)(ZUO_CAR(args), ZUO_CAR(ZUO_CDR(args)));
}

static zuo_t *zuo_primitive2(zuo_t *(*f)(zuo_t *, zuo_t *), zuo_t *name) {
  return zuo_primitive(dispatch_primitive2, (void *)f, (1 << 2), name);
}

static zuo_t *dispatch_primitive3(void *proc, zuo_t *args) {
  return ((zuo_t *(*)(zuo_t *, zuo_t *, zuo_t *))proc)(ZUO_CAR(args),
                                                       ZUO_CAR(ZUO_CDR(args)),
                                                       ZUO_CAR(ZUO_CDR(ZUO_CDR(args))));
}

static zuo_t *zuo_primitive3(zuo_t *(*f)(zuo_t *, zuo_t *, zuo_t *), zuo_t *name) {
  return zuo_primitive(dispatch_primitive3, (void *)f, (1 << 3), name);
}

static zuo_t *dispatch_primitiveN(void *proc, zuo_t *args) {
  return ((zuo_t *(*)(zuo_t *))proc)(args);
}

static zuo_t *zuo_primitiveN(zuo_t *(*f)(zuo_t *), int min_args, zuo_t *name) {
  return zuo_primitive(dispatch_primitiveN, (void *)f, -1 << min_args, name);
}

/*======================================================================*/
/* object primitives                                                    */
/*======================================================================*/

static zuo_t *zuo_pair_p(zuo_t *obj) {
  return (obj->tag == zuo_pair_tag) ? z.o_true : z.o_false;
}

static zuo_t *zuo_null_p(zuo_t *obj) {
  return (obj == z.o_null) ? z.o_true : z.o_false;
}

static zuo_t *zuo_integer_p(zuo_t *obj) {
  return (obj->tag == zuo_integer_tag) ? z.o_true : z.o_false;
}

static zuo_t *zuo_string_p(zuo_t *obj) {
  return (obj->tag == zuo_string_tag) ? z.o_true : z.o_false;
}

static zuo_t *zuo_symbol_p(zuo_t *obj) {
  return (obj->tag == zuo_symbol_tag) ? z.o_true : z.o_false;
}

static zuo_t *zuo_procedure_p(zuo_t *obj) {
  return (((obj->tag == zuo_primitive_tag)
           || (obj->tag == zuo_closure_tag)
           || (obj->tag == zuo_cont_tag)
           || (obj == z.o_apply)
           || (obj == z.o_call_cc))
          ? z.o_true
          : z.o_false);
}

static zuo_t *zuo_hash_p(zuo_t *obj) {
  return (obj->tag == zuo_trie_node_tag) ? z.o_true : z.o_false;
}

static zuo_t *zuo_list_p(zuo_t *obj) {
  while (obj->tag == zuo_pair_tag)
    obj = ((zuo_pair_t *)obj)->cdr;
  return (obj == z.o_null) ? z.o_true : z.o_false;
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

  if (l != z.o_null)
    zuo_fail1w("length", "not a list", in_l);

  return len;
}

static zuo_t *zuo_length(zuo_t *in_l) {
  return zuo_integer(zuo_length_int(in_l));
}

static zuo_t *zuo_reverse(zuo_t *in_l) {
  zuo_t *l = in_l, *r = z.o_null;
  while (l->tag == zuo_pair_tag) {
    r = zuo_cons(_zuo_car(l), r);
    l = _zuo_cdr(l);
  }

  if (l != z.o_null)
    zuo_fail1w("reverse", "not a list", in_l);

  return r;
}

static zuo_t *zuo_procedure_arity_mask(zuo_t *obj) {
  if (obj->tag == zuo_primitive_tag)
    return zuo_integer(((zuo_primitive_t *)obj)->arity_mask);
  else if (obj->tag == zuo_closure_tag) {
    zuo_t *s = ((zuo_closure_t *)obj)->lambda;
    zuo_uint_t ui = 1;
    zuo_int_t i;
    s = _zuo_car(_zuo_cdr(s));
    while (s->tag == zuo_pair_tag) {
      ui = ui << 1;
      s = _zuo_cdr(s);
    }
    i = (zuo_int_t)ui;
    if (i < 0)
      i = 0; /* overflow; claim nothing allowed */
    if (s == z.o_null)
      return zuo_integer(i);
    else
      return zuo_integer(-i);
  } else if (obj->tag == zuo_cont_tag)
    return zuo_integer(1 << 1);
  else if (obj == z.o_apply)
    return zuo_integer(1 << 2);
  else if (obj == z.o_call_cc)
    return zuo_integer(1 << 1);
  else {
    zuo_fail1w("procedure-arity", "not a procedure", obj);
    return z.o_undefined;
  }
}

static zuo_t *zuo_build_string(zuo_t *chars) {
  zuo_int_t len = 0;
  zuo_t *l, *str;
  char *s;
  for (l = chars; l != z.o_null; l = _zuo_cdr(l)) {
    zuo_t *a = _zuo_car(l);
    if ((a->tag != zuo_integer_tag)
        || (ZUO_INT_I(a) < 0)
        || (ZUO_INT_I(a) > 255))
      zuo_fail1w("string", "not an integer in [0, 255]", a);
    len++;
  }
  s = malloc(len);
  len = 0;
  for (l = chars; l != z.o_null; l = _zuo_cdr(l))
    s[len++] = ZUO_INT_I(_zuo_car(l));
  str = zuo_sized_string(s, len);
  free(s);
  return str;
}

static zuo_t *zuo_string_length(zuo_t *obj) {
  check_string("string-length", obj);
  return zuo_integer(ZUO_STRING_LEN(obj));
}

static zuo_int_t check_string_ref_index(const char *who, zuo_t *obj, zuo_t *i, int width) {
  zuo_int_t idx;
  check_string(who, obj);
  check_integer(who, i);
  idx = ZUO_INT_I(i);
  if ((idx < 0) || ((idx + width) > ZUO_STRING_LEN(obj)))
    zuo_fail1w(who, "index out of bounds for string", i);
  return idx;
}

static zuo_t *zuo_string_ref(zuo_t *obj, zuo_t *i) {
  zuo_int_t idx = check_string_ref_index("string-ref", obj, i, 1);
  return zuo_integer(((zuo_string_t *)obj)->s[idx]);
}

static zuo_t *zuo_string_u32_ref(zuo_t *obj, zuo_t *i) {
  zuo_int_t idx = check_string_ref_index("string-u32-ref", obj, i, 4);
  zuo_uint32_t v;
  memcpy(&v, (((zuo_string_t *)obj)->s + idx), sizeof(zuo_uint32_t));
  return zuo_integer(v);
}

static zuo_t *zuo_substring(zuo_t *obj, zuo_t *start_i, zuo_t *end_i) {
  const char *who = "substring";
  zuo_int_t s_idx, e_idx, len;
  check_string(who, obj);
  check_integer(who, start_i);
  check_integer(who, end_i);
  s_idx = ZUO_INT_I(start_i);
  e_idx = ZUO_INT_I(end_i);
  len = ZUO_STRING_LEN(obj);
  if ((s_idx < 0) || (s_idx > len))
    zuo_fail1w(who, "starting index out of bounds for string", start_i);
  if ((e_idx < 0) || (e_idx > len))
    zuo_fail1w(who, "ending index out of bounds for string", end_i);
  if (e_idx < s_idx)
    zuo_fail1w(who, "ending index less than starting index", end_i);
  return zuo_sized_string((const char *)&((zuo_string_t *)obj)->s[s_idx], e_idx - s_idx);
}

static zuo_t *zuo_string_to_symbol(zuo_t *obj) {
  check_string("string->symbol", obj);
  return zuo_symbol_from_string(ZUO_STRING_PTR(obj), obj);
}

static zuo_t *zuo_string_to_uninterned_symbol(zuo_t *obj) {
  check_string("string->uninterned-symbol", obj);
  return zuo_make_symbol_from_string(obj);
}

static zuo_t *zuo_symbol_to_string(zuo_t *obj) {
  check_symbol("symbol->string", obj);
  return ((zuo_symbol_t *)obj)->str;
}

static zuo_t *zuo_hash(zuo_t *args) {
  zuo_t *l, *ht;

  for (l = args; l->tag == zuo_pair_tag; l = _zuo_cdr(_zuo_cdr(l))) {
    if ((_zuo_car(l)->tag != zuo_symbol_tag)
        || (_zuo_cdr(l)->tag != zuo_pair_tag))
      break;
  }
  if (l != z.o_null)
    zuo_fail1w("hash", "arguments not symbol keys interleaved with values", args);

  ht = z.o_empty_hash;
  for (l = args; l->tag == zuo_pair_tag; l = _zuo_cdr(_zuo_cdr(l)))
    ht = zuo_trie_extend(ht, _zuo_car(l), _zuo_car(_zuo_cdr(l)));

  return ht;
}

static zuo_t *zuo_hash_count(zuo_t *ht) {
  check_hash("hash-count", ht);
  return zuo_integer(((zuo_trie_node_t *)ht)->count);
}

static zuo_t *zuo_hash_ref(zuo_t *ht, zuo_t *sym, zuo_t *defval) {
  zuo_t *v;
  const char *who = "hash-ref";
  check_hash(who, ht);
  check_symbol(who, sym);
  v = zuo_trie_lookup(ht, sym);
  if (v == z.o_undefined) v = defval;
  return v;
}

static zuo_t *zuo_hash_set(zuo_t *ht, zuo_t *sym, zuo_t *val) {
  const char *who = "hash-set";
  check_hash(who, ht);
  check_symbol(who, sym);
  return zuo_trie_extend(ht, sym, val);
}

static zuo_t *zuo_hash_remove(zuo_t *ht, zuo_t *sym) {
  const char *who = "hash-remove";
  check_hash(who, ht);
  check_symbol(who, sym);
  return zuo_trie_remove(ht, sym);
}

static zuo_t *zuo_hash_keys(zuo_t *ht) {
  check_hash("hash-keys", ht);
  return zuo_trie_keys(ht, z.o_null);
}

static zuo_t *zuo_hash_keys_subset_p(zuo_t *ht, zuo_t *ht2) {
  const char *who = "hash-keys-subset?";
  check_hash(who, ht);
  check_hash(who, ht2);
  return zuo_trie_keys_subset_p(ht, ht2) ? z.o_true : z.o_false;
}

static zuo_t *zuo_opaque_ref(zuo_t *tag, zuo_t *obj, zuo_t *defval) {
  if (obj->tag == zuo_opaque_tag) {
    if (((zuo_opaque_t *)obj)->tag == tag)
      return ((zuo_opaque_t *)obj)->val;
  }
  return defval;
}

static void check_ints(zuo_t *n, zuo_t *m, const char *who) {
  check_integer(who, n);
  check_integer(who, m);
}

static zuo_t *zuo_add(zuo_t *ns) {
  zuo_uint_t i = 0;
  while (ns != z.o_null) {
    zuo_t *n = _zuo_car(ns);
    check_integer("+", n);
    i += ZUO_UINT_I(n);
    ns = _zuo_cdr(ns);
  }
  return zuo_integer((zuo_int_t)i);
}

static zuo_t *zuo_subtract(zuo_t *ns) {
  zuo_uint_t i;
  zuo_t *n = _zuo_car(ns);
  check_integer("-", n);
  i = ZUO_UINT_I(n);
  ns = _zuo_cdr(ns);
  if (ns == z.o_null) {
    i = -i;
  } else {
    while (ns != z.o_null) {
      n = _zuo_car(ns);
      check_integer("-", n);
      i -= ZUO_UINT_I(n);
      ns = _zuo_cdr(ns);
    }
  }
  return zuo_integer((zuo_int_t)i);
}

static zuo_t *zuo_multiply(zuo_t *ns) {
  zuo_uint_t i = 1;
  while (ns != z.o_null) {
    zuo_t *n = _zuo_car(ns);
    check_integer("*", n);
    i *= ZUO_UINT_I(n);
    ns = _zuo_cdr(ns);
  }
  return zuo_integer((zuo_int_t)i);
}

static zuo_t *zuo_quotient(zuo_t *n, zuo_t *m) {
  const char *who = "quotient";
  zuo_int_t m_i;
  check_ints(n, m, who);
  m_i = ZUO_UINT_I(m);
  if (m_i == 0) zuo_fail1w(who, "divide by zero", m);
  if (m_i == -1) {
    /* avoid potential overflow a the minimum integer */
    return zuo_integer((zuo_int_t)(0 - ZUO_UINT_I(n)));
  }
  return zuo_integer(ZUO_INT_I(n) / m_i);
}

static zuo_t *zuo_modulo(zuo_t *n, zuo_t *m) {
  const char *who = "modulo";
  zuo_int_t m_i;
  check_ints(n, m, who);
  m_i = ZUO_UINT_I(m);
  if (m_i == 0) zuo_fail1w(who, "divide by zero", m);
  return zuo_integer(ZUO_INT_I(n) % m_i);
}

static zuo_t *zuo_not(zuo_t *obj) {
  return (obj == z.o_false) ? z.o_true : z.o_false;
}

static zuo_t *zuo_eql(zuo_t *n, zuo_t *m) {
  check_ints(n, m, "=");
  return (ZUO_INT_I(n) == ZUO_INT_I(m)) ? z.o_true : z.o_false;
}

static zuo_t *zuo_lt(zuo_t *n, zuo_t *m) {
  check_ints(n, m, "<");
  return (ZUO_INT_I(n) < ZUO_INT_I(m)) ? z.o_true : z.o_false;
}

static zuo_t *zuo_le(zuo_t *n, zuo_t *m) {
  check_ints(n, m, "<=");
  return (ZUO_INT_I(n) <= ZUO_INT_I(m)) ? z.o_true : z.o_false;
}

static zuo_t *zuo_ge(zuo_t *n, zuo_t *m) {
  check_ints(n, m, ">=");
  return (ZUO_INT_I(n) >= ZUO_INT_I(m)) ? z.o_true : z.o_false;
}

static zuo_t *zuo_gt(zuo_t *n, zuo_t *m) {
  check_ints(n, m, ">");
  return (ZUO_INT_I(n) > ZUO_INT_I(m)) ? z.o_true : z.o_false;
}

static zuo_t *zuo_bitwise_and(zuo_t *n, zuo_t *m) {
  check_ints(n, m, "bitwise-and");
  return zuo_integer(ZUO_UINT_I(n) & ZUO_UINT_I(m));
}

static zuo_t *zuo_bitwise_ior(zuo_t *n, zuo_t *m) {
  check_ints(n, m, "bitwise-or");
  return zuo_integer(ZUO_UINT_I(n) | ZUO_UINT_I(m));
}

static zuo_t *zuo_bitwise_xor(zuo_t *n, zuo_t *m) {
  check_ints(n, m, "bitwise-xor");
  return zuo_integer(ZUO_UINT_I(n) ^ ZUO_UINT_I(m));
}

static zuo_t *zuo_bitwise_not(zuo_t *n) {
  check_integer("bitwise-not", n);
  return zuo_integer(~ZUO_UINT_I(n));
}

static zuo_t *zuo_eq(zuo_t *n, zuo_t *m) {
  return (n == m) ? z.o_true : z.o_false;
}

static zuo_t *zuo_string_eql(zuo_t *n, zuo_t *m) {
  const char *who = "string=?";
  check_string(who, n);
  check_string(who, m);
  return (((ZUO_STRING_LEN(n) == ZUO_STRING_LEN(m))
           && !memcmp(ZUO_STRING_PTR(n), ZUO_STRING_PTR(m), ZUO_STRING_LEN(n)))
          ? z.o_true
          : z.o_false);
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

static void zuo_falert(FILE* f, zuo_t *objs) {
  if ((objs->tag == zuo_pair_tag)
      && (_zuo_car(objs)->tag == zuo_string_tag)) {
    zuo_fdisplay(f, _zuo_car(objs));
    objs = _zuo_cdr(objs);
    if (objs != z.o_null) fprintf(f, ": ");
  }
  zuo_fdisplay(f, zuo_tilde_v(objs));
}

static zuo_t *zuo_error(zuo_t *objs) {
  zuo_falert(stderr, objs);
  zuo_fail("");
  return z.o_undefined;
}

static zuo_t *zuo_alert(zuo_t *objs) {
  zuo_falert(stdout, objs);
  fprintf(stdout, "\n");
  fflush(stdout);
  return z.o_void;
}

static zuo_t *zuo_list(zuo_t *objs) {
  return objs;
}

static zuo_t *zuo_append(zuo_t *objs) {
  zuo_t *first = z.o_null, *last = NULL, *p;
  zuo_t *l = objs, *a;
  while ((l->tag == zuo_pair_tag)
         && (_zuo_cdr(l)->tag == zuo_pair_tag)) {
    a = _zuo_car(l);
    while (a->tag == zuo_pair_tag) {
      p = zuo_cons(_zuo_car(a), z.o_null);
      if (last)
        ((zuo_pair_t *)last)->cdr = p;
      else
        first = p;
      last = p;
      a = _zuo_cdr(a);
    }
    if (a != z.o_null)
      zuo_fail1w("append", "not a list", _zuo_car(l));
    l = _zuo_cdr(l);
  }

  if (l->tag == zuo_pair_tag) {
    if (last)
      ((zuo_pair_t *)last)->cdr = _zuo_car(l);
    else
      first = _zuo_car(l);
  }

  return first;
}

static zuo_t *zuo_variable_p(zuo_t *var) {
  return (var->tag == zuo_variable_tag) ? z.o_true : z.o_false;
}

static zuo_t *zuo_variable_ref(zuo_t *var) {
  zuo_t *val;
  if (var->tag != zuo_variable_tag)
    zuo_fail1w("variable-ref", "not a variable", var);
  val = ((zuo_variable_t *)var)->val;
  if (val == z.o_undefined) {
    fprintf(stderr, "undefined: ");
    zuo_fwrite(stderr, ((zuo_variable_t *)var)->name);
    zuo_fail("");
  }
  return val;
}

static zuo_t *zuo_variable_set(zuo_t *var, zuo_t *val) {
  if (var->tag != zuo_variable_tag)
    zuo_fail1w("variable-set!", "not a variable", var);
  if (((zuo_variable_t *)var)->val != z.o_undefined)
    zuo_fail1w("variable-set!", "variable already has a value", var);
  ((zuo_variable_t *)var)->val = val;
  return z.o_void;
}

static zuo_t *zuo_make_void(zuo_t *args) {
  return z.o_void;
}

static zuo_t *zuo_kernel_env() {
  return z.o_top_env;
}

/*======================================================================*/
/* interpreter                                                          */
/*======================================================================*/

static zuo_t *zuo_dump() {
  return zuo_cons(Z.o_interp_e,
                  zuo_cons(Z.o_interp_env,
                           zuo_cons(Z.o_interp_k,
                                    zuo_cons(Z.o_interp_v,
                                             Z.o_interp_in_proc))));
}

static void zuo_undump(zuo_t *d) {
  Z.o_interp_e = _zuo_car(d);
  d = _zuo_cdr(d);
  Z.o_interp_env = _zuo_car(d);
  d = _zuo_cdr(d);
  Z.o_interp_k = _zuo_car(d);
  d = _zuo_cdr(d);
  Z.o_interp_v = _zuo_car(d);
  Z.o_interp_in_proc = _zuo_cdr(d);
}

static void bad_form(zuo_t *e) {
  fprintf(stderr, "bad kernel syntax: ");
  zuo_fwrite(stderr, e);
  zuo_fail("");
}

/* Not strictly necessary, but a handy sanity check on input expressions: */
static void check_syntax(zuo_t *e) {
  zuo_t *es = zuo_cons(e, z.o_null);

  while (es != z.o_null) {
    e = _zuo_car(es);
    es = _zuo_cdr(es);
    if (e->tag == zuo_pair_tag) {
      zuo_t *rator = _zuo_car(e);

      if (rator == z.o_quote_symbol) {
        zuo_t *d = _zuo_cdr(e);
        if ((d->tag != zuo_pair_tag) || (_zuo_cdr(d) != z.o_null))
          bad_form(e);
      } else if (rator == z.o_if_symbol) {
        zuo_t *d = _zuo_cdr(e), *dd, *ddd;
        if (d->tag != zuo_pair_tag)
          bad_form(e);
        dd = _zuo_cdr(d);
        if (dd->tag != zuo_pair_tag)
          bad_form(e);
        ddd = _zuo_cdr(dd);
        if ((ddd->tag != zuo_pair_tag) || (_zuo_cdr(ddd) != z.o_null))
          bad_form(e);
        es = zuo_cons(_zuo_car(ddd), es);
        es = zuo_cons(_zuo_car(dd), es);
        es = zuo_cons(_zuo_car(d), es);
      } else if (rator == z.o_lambda_symbol) {
        zuo_t *d = _zuo_cdr(e), *dd, *ad;
        if (d->tag != zuo_pair_tag)
          bad_form(e);
        ad = _zuo_car(d); /* formals */
        dd = _zuo_cdr(d);
        if (dd->tag != zuo_pair_tag)
          bad_form(e);
        if (_zuo_cdr(dd) != z.o_null) {
          /* skip over name string */
          if (_zuo_car(dd)->tag != zuo_string_tag)
            bad_form(e);
          dd = _zuo_cdr(dd);
        }
        if ((dd->tag != zuo_pair_tag) || (_zuo_cdr(dd) != z.o_null))
          bad_form(e);
        while (ad->tag == zuo_pair_tag) {
          if (_zuo_car(ad)->tag != zuo_symbol_tag)
            bad_form(e);
          ad = _zuo_cdr(ad);
        }
        if ((ad != z.o_null) && (ad->tag != zuo_symbol_tag))
          bad_form(e);
        es = zuo_cons(_zuo_car(dd), es);
      } else if (rator == z.o_let_symbol) {
        zuo_t *d = _zuo_cdr(e), *dd, *ad, *aad, *daad, *adaad;
        if (d->tag != zuo_pair_tag)
          bad_form(e);
        ad = _zuo_car(d); /* `((id rhs))` */
        dd = _zuo_cdr(d);
        if ((dd->tag != zuo_pair_tag) || (_zuo_cdr(dd) != z.o_null))
          bad_form(e);
        if ((ad->tag != zuo_pair_tag) || (_zuo_cdr(ad) != z.o_null))
          bad_form(e);
        aad = _zuo_car(ad); /* `(id rhs)` */
        if ((aad->tag != zuo_pair_tag) || (_zuo_car(aad)->tag != zuo_symbol_tag))
          bad_form(e);
        daad = _zuo_cdr(aad); /* `(rhs)` */
        if ((daad->tag != zuo_pair_tag) || (_zuo_cdr(daad) != z.o_null))
          bad_form(e);
        adaad = _zuo_car(daad); /* `rhs` */
        es = zuo_cons(adaad, es);
        es = zuo_cons(_zuo_car(dd), es);
      } else if (rator == z.o_begin_symbol) {
        zuo_t *l = _zuo_cdr(e);
        if (l->tag != zuo_pair_tag)
          bad_form(e);
        while (l->tag == zuo_pair_tag) {
          es = zuo_cons(_zuo_car(l), es);
          l = _zuo_cdr(l);
        }
        if (l != z.o_null)
          bad_form(e);
      } else {
        zuo_t *l = e;
        while (l->tag == zuo_pair_tag) {
          es = zuo_cons(_zuo_car(l), es);
          l = _zuo_cdr(l);
        }
        if (l != z.o_null)
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
    zuo_t *a = _zuo_car(env);
    if (_zuo_car(a) == sym)
      return _zuo_cdr(a);
    env = _zuo_cdr(env);
  }
  return zuo_trie_lookup(env, sym);
}

static void interp_step() {
  zuo_t *e = Z.o_interp_e;

  if (zuo_probe_each) {
    zuo_probe_counter++;
    if ((zuo_probe_counter % 1000) == 0) {
      fprintf(stderr, "probe %d:\n", zuo_probe_counter);
      zuo_stack_dump();
    }
  }

  if (e->tag == zuo_symbol_tag) {
    zuo_t *val = env_lookup(Z.o_interp_env, e);
    if (val == z.o_undefined)
      zuo_fail1("undefined", e);
    Z.o_interp_v = val;
  } else if (e->tag == zuo_pair_tag) {
    zuo_t *rator = _zuo_car(e);

    if (rator == z.o_quote_symbol) {
      Z.o_interp_v = _zuo_car(_zuo_cdr(e));
    } else if (rator == z.o_if_symbol) {
      zuo_t *d = _zuo_cdr(e);
      Z.o_interp_e = _zuo_car(d);
      Z.o_interp_k = zuo_cont(zuo_if_cont,
                              _zuo_cdr(d), Z.o_interp_env,
                              Z.o_interp_in_proc,
                              Z.o_interp_k);
    } else if (rator == z.o_lambda_symbol) {
      Z.o_interp_v = zuo_closure(Z.o_interp_e, Z.o_interp_env);
    } else if (rator == z.o_let_symbol) {
      zuo_t *d = _zuo_cdr(e);
      Z.o_interp_e = _zuo_car(_zuo_cdr(_zuo_car(_zuo_car(d))));
      Z.o_interp_k = zuo_cont(zuo_let_cont,
                              d, Z.o_interp_env,
                              Z.o_interp_in_proc,
                              Z.o_interp_k);
    } else if (rator == z.o_begin_symbol) {
      zuo_t *d = _zuo_cdr(e);
      zuo_t *dd = _zuo_cdr(d);
      Z.o_interp_e = _zuo_car(d);
      if (dd != z.o_null)
        Z.o_interp_k = zuo_cont(zuo_begin_cont,
                                dd, Z.o_interp_env,
                                Z.o_interp_in_proc,
                                Z.o_interp_k);
    } else {
      Z.o_interp_e = rator;
      Z.o_interp_k = zuo_cont(zuo_apply_cont,
                              zuo_cons(z.o_null, _zuo_cdr(e)), Z.o_interp_env,
                              Z.o_interp_in_proc,
                              Z.o_interp_k);
    }
  } else
    Z.o_interp_v = e;
}

static void continue_step() {
  zuo_cont_t *k = (zuo_cont_t *)Z.o_interp_k;
  Z.o_interp_k = k->next;
  Z.o_interp_in_proc = k->in_proc;
  switch (k->tag) {
  case zuo_apply_cont:
    {
      zuo_t *rev_vals = _zuo_car(k->data);
      zuo_t *exps = _zuo_cdr(k->data);
      rev_vals = zuo_cons(Z.o_interp_v, rev_vals);
      if (exps == z.o_null) {
        zuo_t *rator;
        zuo_t *args = z.o_null;
        int count = 0;
        while (_zuo_cdr(rev_vals) != z.o_null) {
          args = zuo_cons(_zuo_car(rev_vals), args);
          count++;
          rev_vals = _zuo_cdr(rev_vals);
        }
        rator = _zuo_car(rev_vals);
        while (1) { /* loop in case of `apply` */
          if (rator->tag == zuo_closure_tag) {
            zuo_t *all_args = args;
            zuo_closure_t *f = (zuo_closure_t *)rator;
            zuo_t *env = f->env;
            zuo_t *formals = _zuo_car(_zuo_cdr(f->lambda));
            zuo_t *body = _zuo_cdr(_zuo_cdr(f->lambda));
            zuo_t *body_d = _zuo_cdr(body);
            if (body_d != z.o_null) {
              Z.o_interp_in_proc = _zuo_car(body);
              body = body_d; /* skip over function name */
            } else {
              Z.o_interp_in_proc = z.o_false;
            }
            while (formals->tag == zuo_pair_tag) {
              if (args == z.o_null)
                break;
              env = env_extend(env, _zuo_car(formals), _zuo_car(args));
              args = _zuo_cdr(args);
              formals = _zuo_cdr(formals);
            }
            if (formals->tag == zuo_symbol_tag)
              env = env_extend(env, formals, args);
            else if (formals != z.o_null || args != z.o_null)
              zuo_fail1("wrong argument count", zuo_cons(rator, all_args));

            Z.o_interp_e = _zuo_car(body);
            Z.o_interp_env = env;
            Z.o_interp_v = z.o_undefined;
            break;
          } else if (rator->tag == zuo_primitive_tag) {
            zuo_primitive_t *f = (zuo_primitive_t *)rator;
            if (f->arity_mask & (1 << ((count > 10) ? 10 : count))) {
              Z.o_interp_v = f->dispatcher(f->proc, args);
            } else
              zuo_fail1("wrong argument count", zuo_cons(rator, args));
            break;
          } else if (rator->tag == zuo_cont_tag) {
            if (count == 1) {
              Z.o_interp_k = rator;
              Z.o_interp_v = _zuo_car(args);
            } else
              zuo_fail1("wrong argument count", zuo_cons(rator, args));
            break;
          } else if (rator == z.o_apply) {
            if (count != 2)
              zuo_fail1("wrong argument count", zuo_cons(z.o_apply, args));
            rator = _zuo_car(args);
            args = _zuo_car(_zuo_cdr(args));
            if (!zuo_list_p(args))
              zuo_fail1("not a list", args);
            count = zuo_length_int(args);
            /* no break => loop to apply again */
          } else if (rator == z.o_call_cc) {
            if (count != 1)
              zuo_fail1("wrong argument count", zuo_cons(z.o_call_cc, args));
            rator = _zuo_car(args);
            args = zuo_cons(Z.o_interp_k, z.o_null);
            /* no break => loop to apply again */
          } else
            zuo_fail1("not a function for call", rator);
        }
      } else {
        Z.o_interp_e = _zuo_car(exps);
        Z.o_interp_env = k->env;
        Z.o_interp_k = zuo_cont(zuo_apply_cont,
                                zuo_cons(rev_vals, _zuo_cdr(exps)), Z.o_interp_env,
                                Z.o_interp_in_proc,
                                Z.o_interp_k);
        Z.o_interp_v = z.o_undefined;
      }
    }
    break;
  case zuo_let_cont:
    Z.o_interp_e = _zuo_car(_zuo_cdr(k->data));
    Z.o_interp_env = env_extend(k->env, _zuo_car(_zuo_car(_zuo_car(k->data))), Z.o_interp_v);
    Z.o_interp_v = z.o_undefined;
    break;
  case zuo_begin_cont:
    {
      zuo_t *d = _zuo_cdr(k->data);
      Z.o_interp_e = _zuo_car(k->data);
      Z.o_interp_env = k->env;
      if (d != z.o_null)
        Z.o_interp_k = zuo_cont(zuo_begin_cont,
                                d, Z.o_interp_env,
                                Z.o_interp_in_proc,
                                Z.o_interp_k);
      Z.o_interp_v = z.o_undefined;
    }
    break;
  case zuo_if_cont:
    {
      if (Z.o_interp_v == z.o_false)
        Z.o_interp_e = _zuo_car(_zuo_cdr(k->data));
      else
        Z.o_interp_e = _zuo_car(k->data);
      Z.o_interp_env = k->env;
      Z.o_interp_v = z.o_undefined;
    }
    break;
  case zuo_done_cont:
    break;
  }
}

zuo_t *zuo_kernel_eval(zuo_t *e) {
  check_syntax(e);

  Z.o_stash = zuo_cons(zuo_dump(), Z.o_stash);

  Z.o_interp_e = e;
  Z.o_interp_v = z.o_undefined;
  Z.o_interp_env = z.o_top_env;
  Z.o_interp_k = z.o_done_k;

  while (1) {
    zuo_check_collect();
    if (Z.o_interp_v == z.o_undefined) {
      interp_step();
    } else if (Z.o_interp_k == z.o_done_k) {
      zuo_t *v = Z.o_interp_v;
      Z.o_interp_e = Z.o_interp_v = Z.o_interp_env = Z.o_interp_k = z.o_false;
      
      zuo_undump(_zuo_car(Z.o_stash));
      Z.o_stash = _zuo_cdr(Z.o_stash);
      
      return v;
    } else {
      continue_step();
    }
  }
}

/*======================================================================*/
/* environment variables                                                */
/*======================================================================*/

#if defined(__APPLE__) && defined(__MACH__)
# include <crt_externs.h>
#elif defined(ZUO_UNIX)
extern char **environ;
#endif

#ifdef ZUO_WINDOWS
static wchar_t *zuo_to_wide(char *a) {
  wchar_t *wa;
  int walen, alen = strlen(a);

  walen = MultiByteToWideChar(CP_UTF8, 0, a, alen, NULL, 0);
  wa = malloc((walen+1) * sizeof(wchar_t));
  MultiByteToWideChar(CP_UTF8, 0, a, alen, wa, walen);
  wa[walen] = 0;

  return wa;
}

static char *zuo_from_wide(const wchar_t *wa) {
  char *a;
  int alen, walen = wcslen(wa);

  alen = WideCharToMultiByte(CP_UTF8, 0, wa, walen, NULL, 0, NULL, NULL);
  a = malloc(alen+1);
  alen = WideCharToMultiByte(CP_UTF8, 0, wa, walen, a, alen, NULL, NULL);
  a[alen] = 0;

  return a;
}
#endif

static zuo_t *zuo_get_envvars()
{
  zuo_t *first = z.o_null, *last = NULL, *pr;

#ifdef ZUO_UNIX
  {
    zuo_int_t i, j;
    char **ea, *p;

# if defined(__APPLE__) && defined(__MACH__)
    ea = *_NSGetEnviron();
# else
    ea = environ;
# endif

    for (i = 0; ea[i]; i++) {
      p = ea[i];
      for (j = 0; p[j] && p[j] != '='; j++) {
      }
      pr = zuo_cons(zuo_cons(zuo_sized_string(p, j), zuo_string(p+j+1)),
                    z.o_null);
      if (last == NULL) first = pr; else ZUO_CDR(last) = pr;
      last = pr;
    }
  }
#endif
#ifdef ZUO_WINDOWS
  {
    char *p;
    wchar_t *e;
    zuo_int_t i, start, j;

    e = GetEnvironmentStringsW();
    if (!e)
      zuo_fail("failed to get environment variables");

    i = 0;
    while (e[i]) {
      start = i;
      while (e[i]) { i++; }
      p = zuo_from_wide(e + start);
      for (j = 0; p[j] && p[j] != '='; j++) {
      }
      p[j] = 0;
      if (p[0] != 0) {
        pr = zuo_cons(zuo_cons(zuo_string(p), zuo_string(p+j+1)),
                      z.o_null);
        if (last == NULL) first = pr; else ZUO_CDR(last) = pr;
        last = pr;
      }
      free(p);
      i++;
    }

    FreeEnvironmentStringsW(e);
  }
#endif
  return first;
}

static void *zuo_envvars_block(const char *who, zuo_t *envvars)
{
#ifdef ZUO_UNIX
  char **r, *s;
  intptr_t len = 0, slen, c, count = 0;
  zuo_t *l;

  for (l = envvars; l != z.o_null; l = _zuo_cdr(l)) {
    zuo_t *a = _zuo_car(l);
    len += ZUO_STRING_LEN(_zuo_car(a));
    len += ZUO_STRING_LEN(_zuo_cdr(a));
    len += 2;
    count++;
  }

  r = (char **)malloc((count+1) * sizeof(char*) + len);
  s = (char *)(r + (count+1));
  c = 0;
  for (l = envvars; l != z.o_null; l = _zuo_cdr(l)) {
    zuo_t *a = _zuo_car(l);
    r[c++] = s;
    slen = ZUO_STRING_LEN(_zuo_car(a));
    memcpy(s, ZUO_STRING_PTR(_zuo_car(a)), slen);
    s[slen] = '=';
    s = s + (slen + 1);
    slen = ZUO_STRING_LEN(_zuo_cdr(a));
    memcpy(s, ZUO_STRING_PTR(_zuo_cdr(a)), slen);
    s[slen] = 0;
    s = s + (slen + 1);
  }
  r[c] = NULL;

  return r;
#endif
#ifdef ZUO_WINDOWS
  zuo_t *l;
  zuo_int_t i;
  zuo_int_t r_size = 256, r_len = 0, namelen, vallen, slen;
  wchar_t *r = malloc(r_size * sizeof(wchar_t)), *name, *val;

  for (l = envvars; l != z.o_null; l = _zuo_cdr(l)) {
    zuo_t *a = _zuo_car(l);
    name = zuo_to_wide(ZUO_STRING_PTR(_zuo_car(a)));
    val = zuo_to_wide(ZUO_STRING_PTR(_zuo_cdr(a)));
    namelen = wcslen(name);
    vallen = wcslen(val);
    slen = namelen + vallen + 2;

    if (r_len + slen >= r_size) {
      zuo_int_t new_size = 2 * (r_size + r_len);
      wchar_t *new_r = malloc(new_size * sizeof(wchar_t));
      memcpy(new_r, r, r_size * sizeof(wchar_t));
      free(r);
      r = new_r;
      r_size = new_size;
    }

    memcpy(r + r_len, name, namelen * sizeof(wchar_t));
    r_len += namelen;
    r[r_len++] = '=';
    memcpy(r + r_len, val, vallen * sizeof(wchar_t));
    r_len += vallen;
    r[r_len++] = 0;

    free(name);
    free(val);
  }
  r[r_len] = 0;

  return r;
#endif
}

  
/*======================================================================*/
/* paths                                                                */
/*======================================================================*/

#ifdef ZUO_UNIX
# define ZUO_IS_PATH_SEP(c) ((c) == '/')
# define ZUO_PATH_SEP '/'
#endif
#ifdef ZUO_WINDOWS
# define ZUO_IS_PATH_SEP(c) (((c) == '/') || ((c) == '\\'))
# define ZUO_PATH_SEP '\\'
#endif

static int zuo_is_path_string(zuo_t *obj) {
  zuo_int_t i;
  
  if ((obj->tag != zuo_string_tag)
      || ZUO_STRING_LEN(obj) == 0)
    return 0;

  for (i = ZUO_STRING_LEN(obj); i--; ) {
    if (((zuo_string_t *)obj)->s[i] == 0)
      return 0;
  }

  return 1;
}

static zuo_t *zuo_path_string_p(zuo_t *obj) {
  return zuo_is_path_string(obj) ? z.o_true : z.o_false;
}

static int zuo_is_module_path(zuo_t *obj, int *_saw_slash) {
  if (obj->tag == zuo_symbol_tag) {
    zuo_string_t *str = (zuo_string_t *)((zuo_symbol_t *)obj)->str;
    if (str->len == 0)
      str = NULL;
    else {
      zuo_int_t i, saw_slash = 0;
      for (i = 0; i < str->len; i++) {
        if (str->s[i] == '/') {
          if ((i == 0) || (i == str->len-1) || (saw_slash == i))
            return 0;
          saw_slash = i+1;
        }
        if (!zuo_is_symbol_module_char(str->s[i]))
          return 0;
      }
      *_saw_slash = (saw_slash > 0);
    }
    return 1;
  } else
    return zuo_is_path_string(obj);
}

static zuo_t *zuo_module_path_p(zuo_t *obj) {
  int saw_slash;
  return zuo_is_module_path(obj, &saw_slash) ? z.o_true : z.o_false;
}

static void check_path_string(const char *who, zuo_t *obj) {
  if (!zuo_is_path_string(obj))
    zuo_fail1w(who, "not a path string", obj);
}

static void check_module_path(const char *who, zuo_t *obj) {
  int saw_slash = 0;
  if (!zuo_is_module_path(obj, &saw_slash))
    zuo_fail1w(who, "not a module path", obj);
}

static int zuo_path_is_absolute(const char *p) {
#ifdef ZUO_UNIX
  return p[0] == '/';
#endif
#ifdef ZUO_WINDOWS
  return ((p[0] == '/')
          || (p[0] == '\\')
          || (isalpha(p[0])
              && (p[1] == ':')));
#endif
}

static zuo_t *zuo_relative_path_p(zuo_t *obj) {
  check_path_string("relative-path?", obj);
  return zuo_path_is_absolute(ZUO_STRING_PTR(obj)) ? z.o_false : z.o_true;
}

static char *zuo_getcwd() {
  char *dir;

  char *s;
  int len = 256;
  
  s = malloc(len);
  while (1) {
#ifdef ZUO_UNIX
    dir = getcwd(s, len);
#endif
#ifdef ZUO_WINDOWS
    dir = (char *)_wgetcwd((wchar_t *)s, len / sizeof(wchar_t));
#endif
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
  
  if (!dir)
    zuo_fail("error getting current directory");

#ifdef ZUO_WINDOWS
  dir = zuo_from_wide((wchar_t *)s);
  free(s);
#endif
  
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
  
  if (zuo_path_is_absolute(ZUO_STRING_PTR(post)))
    zuo_fail1w("build-path", "second path is not relative", post);

  /* add separator beteween `pre` and `post`? */
  len = ZUO_STRING_LEN(pre);
  if (ZUO_IS_PATH_SEP(((zuo_string_t *)pre)->s[len-1]))
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
    path->s[len++] = ZUO_PATH_SEP;
  memcpy(&path->s[len], ZUO_STRING_PTR(post), ZUO_STRING_LEN(post));

  return (zuo_t *)path;
}

static zuo_t *zuo_split_path(zuo_t *p) {
  zuo_int_t i;
  int non_sep, tail_seps;
  
  check_path_string("split-path", p);

  non_sep = tail_seps = 0;
  for (i = ZUO_STRING_LEN(p); i--; ) {
    if (ZUO_IS_PATH_SEP(ZUO_STRING_PTR(p)[i])) {
      if (non_sep) {
        i++;
        return zuo_cons(zuo_sized_string(ZUO_STRING_PTR(p), i),
                        zuo_sized_string(ZUO_STRING_PTR(p)+i,
                                         ZUO_STRING_LEN(p)-i-tail_seps));
      } else
        tail_seps++;
    } else
      non_sep = 1;
  }

  if (tail_seps > 0) {
    if (tail_seps == ZUO_STRING_LEN(p))
      tail_seps--;
    p = zuo_sized_string(ZUO_STRING_PTR(p), ZUO_STRING_LEN(p)-tail_seps);
  }

  return zuo_cons(z.o_false, p);
}

static zuo_t *zuo_path_to_complete_path(zuo_t *path) {
  if (zuo_path_is_absolute(ZUO_STRING_PTR(path)))
    return path;
  else
    return zuo_build_path(Z.o_current_directory, path);
}

zuo_t *zuo_library_path_to_file_path(zuo_t *path) {
  zuo_t *strobj;
  int saw_slash = 0;

  if ((path->tag != zuo_symbol_tag)
      || !zuo_is_module_path(path, &saw_slash))
    zuo_fail1w("module-path->path", "not a module library path", path);

  if (Z.o_library_path == z.o_false)
    zuo_fail1("no library path configured, cannot load module", path);

  strobj = zuo_tilde_a(zuo_cons(((zuo_symbol_t *)path)->str,
                                zuo_cons(saw_slash ? zuo_string("") : zuo_string("/main"),
                                         zuo_cons(zuo_string(".zuo"),
                                                  z.o_null))));

  return zuo_build_path(Z.o_library_path, strobj);
}

zuo_t *zuo_parse_relative_module_path(const char *who, zuo_t *rel_mod_path, int *_ups, int keep_suffix) {
  zuo_int_t i = 0, len = ZUO_STRING_LEN(rel_mod_path);
  unsigned char *s = (unsigned char *)ZUO_STRING_PTR(rel_mod_path);
  int bad = 0, ups = 1, ups_until = 0, saw_non_dot = 0, suffix = 0;

  while (i < len) {
    if (!saw_non_dot && (s[i] == '.')) {
      if (s[i+1] == '/')
        i += 2;
      else if (s[i+1] == '.') {
        if (s[i+2] == '/')
          i += 3;
        else
          bad = 1;
        ups++;
      } else
        bad = 1;
      ups_until = i;
    } else if (zuo_is_symbol_module_char(s[i])) {
      saw_non_dot = 1;
      if (s[i] == '/')
        bad = 1;
      else if (s[i+1] == '/')
        i += 2;
      else
        i++;
    } else if ((s[i] == '.')
               && (s[i+1] == 'z')
               && (s[i+2] == 'u')
               && (s[i+3] == 'o')
               && (s[i+4] == 0)) {
      suffix = 4;
      i += 4;
    } else
      bad = 1;
    if (bad)
      zuo_fail1w(who, "not a relative module library path", rel_mod_path);
  }

  if (suffix == 0)
    zuo_fail1w(who, "relative module library path lacks \".zuo\"", rel_mod_path);

  *_ups = ups;

  return zuo_sized_string((char *)s + ups_until,
                          len - ups_until - (keep_suffix ? 0 : suffix));
}

zuo_t *zuo_module_path_join(zuo_t *base_mod_path, zuo_t *rel_mod_path) {
  const char *who = "module-path-join";
  int saw_slash = 0, ups = 0, keep_suffix;
  zuo_t *rel_str;

  check_module_path(who, rel_mod_path);
  if (!zuo_is_module_path(base_mod_path, &saw_slash))
    zuo_fail1w(who, "not a module path", base_mod_path);

  if (rel_mod_path->tag == zuo_symbol_tag)
    return rel_mod_path;

  /* When an absolute path is given, normalization is the caller's problem: */
  if (zuo_path_is_absolute(ZUO_STRING_PTR(rel_mod_path)))
    return rel_mod_path;

  keep_suffix = (base_mod_path->tag == zuo_string_tag);

  rel_str = zuo_parse_relative_module_path(who, rel_mod_path, &ups, keep_suffix);

  if (base_mod_path->tag == zuo_symbol_tag) {
    zuo_t *mod_path = ((zuo_symbol_t *)base_mod_path)->str;
    if (!saw_slash)
      mod_path = zuo_tilde_a(zuo_cons(mod_path, zuo_cons(zuo_string("/main"), z.o_null)));

    while (ups) {
      zuo_t *l = zuo_split_path(mod_path);
      mod_path = _zuo_car(l);
      if (mod_path == z.o_false)
        zuo_fail1w(who, "too many up elements", rel_mod_path);
      ups--;
    }

    mod_path = zuo_tilde_a(zuo_cons(mod_path, zuo_cons(rel_str, z.o_null)));
    mod_path = zuo_string_to_symbol(mod_path);

    if (!zuo_is_module_path(mod_path, &saw_slash))
      zuo_fail1w(who, "relative path is not valid in a symbolic module path", rel_mod_path);

    return mod_path;
  } else {
    zuo_t *mod_path = base_mod_path;
    while (ups > 0) {
      zuo_t *l;
      if (mod_path == z.o_false)
        zuo_fail1w(who, "too many up elements", rel_mod_path);
      l = zuo_split_path(mod_path);
      mod_path = _zuo_car(l);
      if (strcmp(ZUO_STRING_PTR(_zuo_cdr(l)), ".") != 0)
        ups--;
    }
    if (mod_path == z.o_false)
      return rel_str;
    else
      return zuo_build_path(mod_path, rel_str);
  }
}

static zuo_t *zuo_runtime_env() {
  return Z.o_runtime_env;
}

static zuo_t *zuo_make_runtime_env(zuo_t *exe_path, const char *load_file, int argc, char **argv) {
  zuo_t *ht = z.o_empty_hash;

  ht = zuo_hash_set(ht, zuo_symbol("exe"), exe_path);

  {
    zuo_t *l = z.o_null;
    while (argc-- > 0)
      l = zuo_cons(zuo_string(argv[argc]), l);
    ht = zuo_hash_set(ht, zuo_symbol("args"), l);
  }

  ht = zuo_hash_set(ht, zuo_symbol("dir"), Z.o_current_directory);
  ht = zuo_hash_set(ht, zuo_symbol("script"), zuo_string(load_file));

  ht = zuo_hash_set(ht, zuo_symbol("env"), zuo_get_envvars());

  {
#ifdef ZUO_UNIX
    zuo_t *type = zuo_symbol("unix");
#endif
#ifdef ZUO_WINDOWS
    zuo_t *type = zuo_symbol("windows");
#endif
    ht = zuo_hash_set(ht, zuo_symbol("system-type"), type);
  }

  return ht;
}

/*======================================================================*/
/* files/streams                                                        */
/*======================================================================*/

static zuo_raw_handle_t zuo_get_std_handle(int which) {
#ifdef ZUO_UNIX
  return which;
#endif
#ifdef ZUO_WINDOWS
  HANDLE h;

  switch (which) {
  case 0:
    which = STD_INPUT_HANDLE;
    break;
  case 1:
    which = STD_OUTPUT_HANDLE;
    break;
  default:
    which = STD_ERROR_HANDLE;
    break;
  }
  
  h = GetStdHandle(which);

  if ((h == INVALID_HANDLE_VALUE) || (h == NULL))
    return h;

  if ((which == STD_OUTPUT_HANDLE) || (which == STD_ERROR_HANDLE)) {
    if (GetFileType(h) == FILE_TYPE_CHAR) {
      /* Try to enable ANSI escape codes, which should work for a recent
         enough version of Windows */
      DWORD mode = 0;
      GetConsoleMode(h, &mode);
      SetConsoleMode(h, mode | ENABLE_VIRTUAL_TERMINAL_PROCESSING);
    }
  }

  return h;
#endif
}

static zuo_t *zuo_fd_handle(zuo_raw_handle_t handle, zuo_handle_status_t status)  {
  zuo_t *h = zuo_handle(handle, status);
#ifdef ZUO_UNIX
  trie_set(Z.o_fd_table, handle, h, h);
#endif
  return h;
}

static char *zuo_drain(FILE *f, zuo_raw_handle_t fd,
                       zuo_int_t amount, zuo_int_t *_len) {
  char *s;
  zuo_int_t sz = 256, offset = 0;

  if ((amount >= 0) && (sz > amount))
    sz = amount;
  
  s = malloc(sz+1);
  while ((amount < 0) || (offset < amount)) {
    zuo_int_t got;
    if (f) {
      got = fread(s + offset, 1, sz - offset, f);
      if ((got == 0) && ferror(f))
        got = -1;
    } else {
      zuo_int_t amt = sz - offset;
      if (amt > 4096) amt = 4096;
#ifdef ZUO_UNIX
      got = read(fd, s + offset, amt);
#endif
#ifdef ZUO_WINDOWS
      {
        DWORD dgot;
        if (!ReadFile(fd, s + offset, amt, &dgot, NULL)) {
	  if (GetLastError() == ERROR_BROKEN_PIPE)
	    got = 0;
	  else
	    got = -1;
        } else
          got = dgot;
      }
#endif
    }

    if (got < 0)
      zuo_fail("error reading stream");
    
    if (got == 0) {
      break;
    } else {
      offset += got;
      if (offset == sz) {
        char *new_s;
        zuo_int_t new_sz = sz*2;
        if ((amount >= 0) && (new_sz > amount))
          new_sz = amount;
        new_s = malloc(new_sz+1);
        memcpy(new_s, s, sz);
        free(s);
        sz = new_sz;
        s = new_s;
      }
    }
  }

  s[offset] = 0;
  *_len = offset;

  if ((offset == 0) && (amount > 0))
    return NULL;

  return s;
}

static void zuo_fill(const char *s, zuo_int_t len, FILE *f, zuo_raw_handle_t fd) {
  zuo_int_t done = 0;
  while (done < len) {
    zuo_int_t did;
    if (f) {
      did = fwrite(s + done, 1, len - done, f);
      if (did < len - done)
        did = -1;
    } else {
      zuo_int_t amt = len - done;
      if (amt > 4096) amt = 4096;
#ifdef ZUO_UNIX
      did = write(fd, s + done, amt);
#endif
#ifdef ZUO_WINDOWS
      {
        DWORD ddid;
        if (!WriteFile(fd, s + done, amt, &ddid, NULL))
          did = -1;
        else
          did = ddid;
      }
#endif
    }

    if (did < 0)
      zuo_fail("error writing to stream");

    done += did;
  }
}

static void zuo_close(zuo_raw_handle_t handle)
{
#ifdef ZUO_UNIX
  close(handle);
  trie_set(Z.o_fd_table, handle, z.o_undefined, z.o_undefined);
#endif
#ifdef ZUO_WINDOWS
  CloseHandle(handle);
#endif
}

static zuo_t *zuo_fd_open_input(zuo_t *path) {
  const char *who = "fd-open-input";
  zuo_raw_handle_t fd;

  if (zuo_is_path_string(path)) {
#ifdef ZUO_UNIX
    fd = open(ZUO_STRING_PTR(path), O_RDONLY);
    if (fd == -1)
      zuo_fail1w_errno(who, "file open failed", path);
#endif
#ifdef ZUO_WINDOWS
    wchar_t *wp = zuo_to_wide(ZUO_STRING_PTR(path));
    fd = CreateFileW(wp,
                     GENERIC_READ,
                     FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
                     NULL,
                     OPEN_EXISTING,
                     0,
                     NULL);
    if (fd == INVALID_HANDLE_VALUE)
      zuo_fail1w(who, "file open failed", path);
    free(wp);
#endif
    return zuo_fd_handle(fd, zuo_handle_open_fd_in_status);
  } else {
    if (path != zuo_symbol("stdin"))
      zuo_fail1w(who, "not a path string or 'stdin", path);
    fd = zuo_get_std_handle(0);
    return zuo_handle(fd, zuo_handle_open_fd_in_status);
  }
}

static zuo_t *zuo_fd_open_output(zuo_t *path) {
  const char *who = "fd-open-output";
  zuo_raw_handle_t fd;

  if (zuo_is_path_string(path)) {
#ifdef ZUO_UNIX
    fd = open(ZUO_STRING_PTR(path), O_WRONLY | O_CREAT | O_TRUNC, 0666);
    if (fd == -1)
      zuo_fail1w_errno(who, "file open failed", path);
#endif
#ifdef ZUO_WINDOWS
    wchar_t *wp = zuo_to_wide(ZUO_STRING_PTR(path));
    fd = CreateFileW(wp,
                     GENERIC_WRITE,
                     FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
                     NULL,
                     CREATE_ALWAYS,
                     0,
                     NULL);
    if (fd == INVALID_HANDLE_VALUE)
      zuo_fail1w(who, "file open failed", path);
    free(wp);
#endif
    return zuo_fd_handle(fd, zuo_handle_open_fd_out_status);
  } else if (path == zuo_symbol("stdout")) {
    fd = zuo_get_std_handle(1);
    return zuo_handle(fd, zuo_handle_open_fd_out_status);
  } else {
    if (path != zuo_symbol("stderr"))
      zuo_fail1w(who, "not a path string, 'stdout, or 'stderr", path);
    fd = zuo_get_std_handle(2);
    return zuo_handle(fd, zuo_handle_open_fd_out_status);
  }
}

static zuo_t *zuo_fd_close(zuo_t *fd_h) {
  if (fd_h->tag == zuo_handle_tag) {
    zuo_handle_t *h = (zuo_handle_t *)fd_h;
    if ((h->u.h.status == zuo_handle_open_fd_out_status)
        || (h->u.h.status == zuo_handle_open_fd_in_status)) {
      zuo_close(h->u.h.handle);
      h->u.h.status = zuo_handle_closed_status;
      return z.o_void;
    }
  }

  zuo_fail1w("fd-close", "not an open input or output file descriptor", fd_h);
  return z.o_undefined;
}

zuo_t *zuo_fd_write(zuo_t *fd_h, zuo_t *str) {
  const char *who = "fd-write";

  if ((fd_h->tag != zuo_handle_tag)
      || ((zuo_handle_t *)fd_h)->u.h.status != zuo_handle_open_fd_out_status)
    zuo_fail1w(who, "not an open output file descriptor", fd_h);

  check_string(who, str);

  zuo_fill(ZUO_STRING_PTR(str), ZUO_STRING_LEN(str), NULL, ZUO_HANDLE_RAW(fd_h));

  return z.o_void;
}

static zuo_t *zuo_fd_read(zuo_t *fd_h, zuo_t *amount) {
  const char *who = "fd-read";
  char *data;
  zuo_int_t len, amt = -1;
  zuo_t *str;
  
  if ((fd_h->tag != zuo_handle_tag)
      || ((zuo_handle_t *)fd_h)->u.h.status != zuo_handle_open_fd_in_status)
    zuo_fail1w(who, "not an open input file descriptor", fd_h);
  if (amount != z.o_eof) {
    if ((amount->tag == zuo_integer_tag)
        && (ZUO_INT_I(amount) >= 0))
      amt = ZUO_INT_I(amount);
    else
      zuo_fail1w(who, "not a nonnegative integer or eof", amount);
  }

  data = zuo_drain(NULL, ZUO_HANDLE_RAW(fd_h), amt, &len);
  if (data == NULL)
    return z.o_eof;
  
  str = zuo_sized_string(data, len);

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

static zuo_t *zuo_dump_image_and_exit(zuo_t *fd_obj) {
  zuo_int_t len;
  char *dump;
  zuo_raw_handle_t fd;

  if ((fd_obj->tag != zuo_handle_tag)
      || ((zuo_handle_t *)fd_obj)->u.h.status != zuo_handle_open_fd_out_status)
    zuo_fail1w("dump-image-and-exit", "not an open output file descriptor", fd_obj);

  fd = ((zuo_handle_t *)fd_obj)->u.h.handle;

  /* no runtime state is preserved */
  {
    zuo_t **p = (zuo_t **)&zuo_roots.runtime;
    int i, len;
    len = sizeof(zuo_roots.runtime) / sizeof(zuo_t*);
    for (i = 0; i < len; i++)
      p[i] = z.o_undefined;
    Z.o_interp_k = z.o_done_k; /* in case of a failure that might try to show a stack trace */
  }

  dump = zuo_fasl_dump(&len);
  zuo_fill(dump, len, NULL, fd);

  exit(0);
}

static zuo_t *zuo_handle_p(zuo_t *var) {
  return (var->tag == zuo_handle_tag) ? z.o_true : z.o_false;
}

/*======================================================================*/
/* modules                                                              */
/*======================================================================*/

static zuo_t *zuo_module_to_hash(zuo_t *module_path);

static int zuo_module_path_equal(zuo_t *a, zuo_t *b) {
  if (a->tag == zuo_symbol_tag)
    return (a == b);
  else if (b->tag == zuo_symbol_tag)
    return 0;
  else
    return zuo_string_eql(a, b) == z.o_true;
}

static zuo_t *zuo_eval_module(zuo_t *module_path, char *input_to_read_and_free, zuo_int_t input_len) {
  char *input = input_to_read_and_free;
  char *lang;
  zuo_int_t post;
  zuo_t *v;

  Z.o_pending_modules = zuo_cons(module_path, Z.o_pending_modules);

  lang = zuo_read_language(input, &post);
  Z.o_stash = zuo_cons(module_path, Z.o_stash);

  if (!strcmp(lang, "zuo/kernel")) {
    zuo_t *es = zuo_read_all_str(input, input_len, post);
    if (es == z.o_null)
      zuo_fail("zuo/kernel: no S-expression in input");
    if (_zuo_cdr(es) != z.o_null)
      zuo_fail("zuo/kernel: more than one S-expression in input");
    v = zuo_kernel_eval(_zuo_car(es));
  } else {
    zuo_t *env = zuo_module_to_hash(zuo_symbol(lang));
    zuo_t *proc = zuo_trie_lookup(env, zuo_symbol("read-and-eval"));
    if (proc->tag != zuo_closure_tag)
      zuo_fail1("not a language module path", zuo_symbol(lang));
    module_path = _zuo_car(Z.o_stash);
    v = zuo_kernel_eval(zuo_cons(proc, zuo_cons(zuo_sized_string(input, input_len),
                                                zuo_cons(zuo_integer(post),
                                                         zuo_cons(zuo_cons(z.o_quote_symbol,
                                                                           zuo_cons(module_path,
                                                                                    z.o_null)),
                                                                  z.o_null)))));
  }
  free(lang);

  module_path = _zuo_car(Z.o_stash);
  Z.o_stash = _zuo_cdr(Z.o_stash);

  free(input);

  if (v->tag != zuo_trie_node_tag)
    zuo_fail1("module did not produce a hash table", module_path);

  z.o_modules = zuo_cons(zuo_cons(module_path, v), z.o_modules);

  ASSERT(zuo_module_path_equal(module_path, _zuo_car(Z.o_pending_modules)));
  Z.o_pending_modules = _zuo_cdr(Z.o_pending_modules);

  return v;
}

static zuo_t *zuo_module_to_hash(zuo_t *module_path) {
  zuo_t *file_path, *l, *mod;

  check_module_path("module->hash", module_path);

  if (module_path->tag == zuo_string_tag)
    module_path = zuo_path_to_complete_path(module_path);

  /* check for already-loaded module */
  for (l = z.o_modules; l != z.o_null; l = _zuo_cdr(l)) {
    zuo_t *a = _zuo_car(l);
    if (zuo_module_path_equal(module_path, _zuo_car(a)))
      return _zuo_cdr(a);
  }

  /* check for cycles module */
  for (l = Z.o_pending_modules; l != z.o_null; l = _zuo_cdr(l)) {
    if (zuo_module_path_equal(module_path, _zuo_car(l)))
      zuo_fail1("cycle in module loading", module_path);
  }

  /* not already loaded */

  if (module_path->tag == zuo_symbol_tag)
    file_path = zuo_library_path_to_file_path(module_path);
  else
    file_path = module_path;

  if (zuo_logging) {
    int i;
    if (zuo_logging > 1) fprintf(stderr, "\n");
    for (i = 1; i < zuo_logging; i++) fprintf(stderr, " ");
    fprintf(stderr, "["); zuo_fdisplay(stderr, module_path);
    fflush(stderr);
    zuo_logging++;
  }

  {
    FILE *in;
    char *filename, *input;
    zuo_int_t in_len;
    
    filename = ZUO_STRING_PTR(file_path);
    in = fopen(filename, "r");
    if (in == NULL)
      zuo_fail1("could not open module file", file_path);
    
    input = zuo_drain(in, 0, -1, &in_len);
    fclose(in);

    mod = zuo_eval_module(module_path, input, in_len);
  }

  if (zuo_logging) {
    zuo_logging--;
    fprintf(stderr, "]");
    if (zuo_logging == 1)
      fprintf(stderr, "]\n");
    fflush(stderr);
  }

  return mod;
}

/*======================================================================*/
/* filesystem and time                                                  */
/*======================================================================*/

#if defined(__APPLE__) && defined(__MACH__)
# define zuo_st_atim st_atimespec
# define zuo_st_mtim st_mtimespec
# define zuo_st_ctim st_ctimespec
#elif defined(ZUO_WINDOWS)
# define zuo_st_atim st_atime
# define zuo_st_mtim st_mtime
# define zuo_st_ctim st_ctime
#else
# define zuo_st_atim st_atim
# define zuo_st_mtim st_mtim
# define zuo_st_ctim st_ctim
#endif

static zuo_t *zuo_stat(zuo_t *path, zuo_t *follow_links) {
  const char *who = "stat";
  zuo_t *result = z.o_empty_hash;
#ifdef ZUO_UNIX
  struct stat stat_buf;
#endif
#ifdef ZUO_WINDOWS
  struct __stat64 stat_buf;
  wchar_t *wp;
#endif
  int stat_result;

  check_path_string(who, path);

#ifdef ZUO_UNIX
  do {
    if (follow_links == z.o_false)
      stat_result = lstat(ZUO_STRING_PTR(path), &stat_buf);
    else
      stat_result = stat(ZUO_STRING_PTR(path), &stat_buf);
  } while ((stat_result == -1) && (errno == EINTR));
#endif  
#ifdef ZUO_WINDOWS
  wp = zuo_to_wide(ZUO_STRING_PTR(path));

  do {
    /* No stat/lstat distinction under Windows */
    stat_result = _wstat64(wp, &stat_buf);
  } while ((stat_result == -1) && (errno == EINTR));
#endif

  if (stat_result != 0) {
    if (errno != ENOENT)
      zuo_fail1w_errno(who, "failed", path);
    return z.o_false;
  }

#ifdef ZUO_WINDOWS
  free(wp);
#endif

#ifdef ZUO_UNIX
  if (S_ISDIR(stat_buf.st_mode))
    result = zuo_hash_set(result, zuo_symbol("type"), zuo_symbol("dir"));
  else if (S_ISLNK(stat_buf.st_mode))
    result = zuo_hash_set(result, zuo_symbol("type"), zuo_symbol("link"));
#endif
#ifdef ZUO_WINDOWS
  if (stat_buf.st_mode & _S_IFDIR)
    result = zuo_hash_set(result, zuo_symbol("type"), zuo_symbol("dir"));
#endif
  else
    result = zuo_hash_set(result, zuo_symbol("type"), zuo_symbol("file"));
  result = zuo_hash_set(result, zuo_symbol("mode"), zuo_integer(stat_buf.st_mode));
  result = zuo_hash_set(result, zuo_symbol("device"), zuo_integer(stat_buf.st_dev));
  result = zuo_hash_set(result, zuo_symbol("inode"), zuo_integer(stat_buf.st_ino));
  result = zuo_hash_set(result, zuo_symbol("hardlink-count"), zuo_integer(stat_buf.st_nlink));
  result = zuo_hash_set(result, zuo_symbol("user-id"), zuo_integer(stat_buf.st_uid));
  result = zuo_hash_set(result, zuo_symbol("group-id"), zuo_integer(stat_buf.st_gid));
  result = zuo_hash_set(result, zuo_symbol("device-id-for-special-file"), zuo_integer(stat_buf.st_rdev));
  result = zuo_hash_set(result, zuo_symbol("size"), zuo_integer(stat_buf.st_size));
#ifdef ZUO_UNIX
  result = zuo_hash_set(result, zuo_symbol("block-size"), zuo_integer(stat_buf.st_blksize));
  result = zuo_hash_set(result, zuo_symbol("block-count"), zuo_integer(stat_buf.st_blocks));
  result = zuo_hash_set(result, zuo_symbol("access-time-seconds"), zuo_integer(stat_buf.zuo_st_atim.tv_sec));
  result = zuo_hash_set(result, zuo_symbol("access-time-nanoseconds"), zuo_integer(stat_buf.zuo_st_atim.tv_nsec));
  result = zuo_hash_set(result, zuo_symbol("modify-time-seconds"), zuo_integer(stat_buf.zuo_st_mtim.tv_sec));
  result = zuo_hash_set(result, zuo_symbol("modify-time-nanoseconds"), zuo_integer(stat_buf.zuo_st_mtim.tv_nsec));
  result = zuo_hash_set(result, zuo_symbol("creation-time-seconds"), zuo_integer(stat_buf.zuo_st_ctim.tv_sec));
  result = zuo_hash_set(result, zuo_symbol("creation-time-nanoseconds"), zuo_integer(stat_buf.zuo_st_ctim.tv_nsec));
#endif
#ifdef ZUO_WINDOWS  
  result = zuo_hash_set(result, zuo_symbol("block-size"), zuo_integer(0));
  result = zuo_hash_set(result, zuo_symbol("block-count"), zuo_integer(0));
  result = zuo_hash_set(result, zuo_symbol("access-time-seconds"), zuo_integer(stat_buf.zuo_st_atim));
  result = zuo_hash_set(result, zuo_symbol("access-time-nanoseconds"), zuo_integer(0));
  result = zuo_hash_set(result, zuo_symbol("modify-time-seconds"), zuo_integer(stat_buf.zuo_st_mtim));
  result = zuo_hash_set(result, zuo_symbol("modify-time-nanoseconds"), zuo_integer(0));
  result = zuo_hash_set(result, zuo_symbol("change-time-seconds"), zuo_integer(stat_buf.zuo_st_ctim));
  result = zuo_hash_set(result, zuo_symbol("change-time-nanoseconds"), zuo_integer(0));
#endif

  return result;
}

static zuo_t *zuo_rm(zuo_t *file_path) {
  const char *who = "rm";
  check_path_string(who, file_path);
#ifdef ZUO_UNIX
  if (unlink(ZUO_STRING_PTR(file_path)) == 0)
    return z.o_void;
#endif
#ifdef ZUO_WINDOWS
  {
    wchar_t *wp = zuo_to_wide(ZUO_STRING_PTR(file_path));
    if (_wunlink(wp) == 0) {
      free(wp);
      return z.o_void;
    }
  }
#endif
  zuo_fail1w_errno(who, "failed", file_path);
  return z.o_undefined;
}

static zuo_t *zuo_mv(zuo_t *from_path, zuo_t *to_path) {
  const char *who = "mv";
  check_path_string(who, from_path);
  check_path_string(who, to_path);
#ifdef ZUO_UNIX
  if (rename(ZUO_STRING_PTR(from_path), ZUO_STRING_PTR(to_path)) == 0)
    return z.o_void;
#endif
#ifdef ZUO_WINDOWS
  {
    wchar_t *from_wp = zuo_to_wide(ZUO_STRING_PTR(from_path));
    wchar_t *to_wp = zuo_to_wide(ZUO_STRING_PTR(to_path));
    if (_wrename(from_wp, to_wp) == 0) {
      free(from_wp);
      free(to_wp);
      return z.o_void;
    }
  }
#endif
  zuo_fail1w_errno(who, "failed", zuo_cons(from_path, zuo_cons(to_path, z.o_null)));
  return z.o_undefined;
}

static zuo_t *zuo_mkdir(zuo_t *dir_path) {
  const char *who = "mkdir";
  check_path_string(who, dir_path);
#ifdef ZUO_UNIX
  if (mkdir(ZUO_STRING_PTR(dir_path), 0777) == 0)
    return z.o_void;
#endif
#ifdef ZUO_WINDOWS
  {
    wchar_t *wp = zuo_to_wide(ZUO_STRING_PTR(dir_path));
    if (_wmkdir(wp) == 0) {
      free(wp);
      return z.o_void;
    }
  }
#endif
  zuo_fail1w_errno(who, "failed", dir_path);
  return z.o_undefined;
}

static zuo_t *zuo_rmdir(zuo_t *dir_path) {
  const char *who = "rmdir";
  check_path_string(who, dir_path);
#ifdef ZUO_UNIX
  if (rmdir(ZUO_STRING_PTR(dir_path)) == 0)
    return z.o_void;
#endif
#ifdef ZUO_WINDOWS
  {
    wchar_t *wp = zuo_to_wide(ZUO_STRING_PTR(dir_path));
    if (_wrmdir(wp) == 0) {
      free(wp);
      return z.o_void;
    }
  }
#endif
  zuo_fail1w_errno(who, "failed", dir_path);
  return z.o_undefined;
}

static zuo_t *zuo_ls(zuo_t *dir_path) {
  const char *who = "ls";
  check_path_string(who, dir_path);
#ifdef ZUO_UNIX
  DIR *dir;
  struct dirent *e;
  zuo_t *first = z.o_null, *last = NULL, *pr;
  
  dir = opendir(ZUO_STRING_PTR(dir_path));
  if (!dir)
    zuo_fail1w_errno(who, "failed", dir_path);
  
  while ((e = readdir(dir))) {
    if ((e->d_name[0] == '.')
        && ((e->d_name[1] == 0)
            || ((e->d_name[1] == '.')
                && (e->d_name[2] == 0)))) {
      /* skip */
    } else {
      pr = zuo_cons(zuo_string(e->d_name), z.o_null);
      if (last == NULL) first = pr; else ZUO_CDR(last) = pr;
      last = pr;
    }
  }

  closedir(dir);

  return first;
#endif
#ifdef ZUO_WINDOWS
  wchar_t *wwildpath;
  intptr_t handle;
  struct _wfinddata_t fileinfo;
  char *s;
  zuo_t *first = z.o_null, *last = NULL, *pr;

  wwildpath = zuo_to_wide(ZUO_STRING_PTR(zuo_build_path(dir_path, zuo_string("*"))));

  if ((handle = _wfindfirst(wwildpath, &fileinfo)) == (intptr_t)-1)
    zuo_fail1w_errno(who, "failed", dir_path);

  do {
    s = zuo_from_wide(fileinfo.name);
    pr = zuo_cons(zuo_string(s), z.o_null);
    free(s);
    if (last == NULL) first = pr; else ZUO_CDR(last) = pr;
    last = pr;
  } while (_wfindnext(handle, &fileinfo) == 0);
  _findclose(handle);

  return first;
#endif
}

static zuo_t *zuo_readlink(zuo_t *link_path) {
  const char *who = "readlink";
  check_path_string(who, link_path);
#ifdef ZUO_UNIX
  {
    int len, buf_len = 256;
    char *buffer = malloc(buf_len);
    zuo_t *str;
    
    while (1) {
      len = readlink(ZUO_STRING_PTR(link_path), buffer, buf_len);
      if (len == -1) {
        zuo_fail1w_errno(who, "failed", link_path);
      } else if (len == buf_len) {
        /* maybe too small */
        free(buffer);
        buf_len *= 2;
        buffer = malloc(buf_len);
      } else
        break;
    }
    str = zuo_sized_string(buffer, len);
    free(buffer);
    return str;
  }
#endif
#ifdef ZUO_WINDOWS
  zuo_fail("readlink: not supported on Windows");
#endif
  return z.o_undefined;
}

static zuo_t *zuo_ln(zuo_t *target_path, zuo_t *link_path) {
  const char *who = "ln";
  check_path_string(who, target_path);
  check_path_string(who, link_path);
#ifdef ZUO_UNIX
  if (symlink(ZUO_STRING_PTR(target_path), ZUO_STRING_PTR(link_path)) == 0)
    return z.o_void;
#endif
#ifdef ZUO_WINDOWS
  zuo_fail("ln: not supported on Windows");
#endif
  zuo_fail1w_errno(who, "failed", zuo_cons(target_path, zuo_cons(link_path, z.o_null)));
  return z.o_undefined;
}

zuo_t *zuo_current_time() {
#ifdef ZUO_UNIX
  struct timespec t;
  if (clock_gettime(CLOCK_REALTIME, &t) != 0)
    zuo_fail("error gettig time");
  return zuo_cons(zuo_integer(t.tv_sec),
                  zuo_integer(t.tv_nsec));
#endif
#ifdef ZUO_WINDOWS
  FILETIME ft;
  zuo_int_t t;

  GetSystemTimeAsFileTime(&ft);
  t = (((zuo_int_t)ft.dwHighDateTime) << 32) | ft.dwLowDateTime;
  /* measurement interval is 100 nanoseconds = 1/10 microseconds, and
     adjust by number of seconds between Windows (1601) and Unix (1970) epochs */
  return zuo_cons(zuo_integer(t / 10000000 - 11644473600L),
                  zuo_integer((t % 10000000) * 100));
#endif
}

/*======================================================================*/
/* optional arguments through a hash table                              */
/*======================================================================*/

static zuo_t *zuo_consume_option(zuo_t **_options, const char *name) {
  zuo_t *sym = zuo_symbol(name);
  zuo_t *opt = zuo_hash_ref(*_options, sym, z.o_undefined);
  
  if (opt != z.o_undefined)
    *_options = zuo_hash_remove(*_options, sym);
  
  return opt;
}

static void check_options_consumed(const char *who, zuo_t *options) {
  if (((zuo_trie_node_t *)options)->count > 0) {
    options = zuo_hash_keys(options);
    zuo_fail1w(who, "unrecognized option", _zuo_car(options));
  }
}

/*======================================================================*/
/* processes                                                            */
/*======================================================================*/

static void zuo_pipe(zuo_raw_handle_t *_r, zuo_raw_handle_t *_w)
{
#ifdef ZUO_UNIX
  {
    int fd[2];
    if (pipe(fd) != 0)
      zuo_fail("pipe creation failed");
    *_r = fd[0];
    *_w = fd[1];
  }
#endif
#ifdef ZUO_WINDOWS
  {
    HANDLE rh, wh;

    if (!CreatePipe(_r, _w, NULL, 0))
      zuo_fail("pipe creation failed");
  }
#endif  
}

#ifdef ZUO_WINDOWS
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


zuo_t *zuo_process(zuo_t *command_and_args)
{
  const char *who = "process";
  zuo_t *command = _zuo_car(command_and_args);
  zuo_t *args = _zuo_cdr(command_and_args);
  zuo_t *options = z.o_empty_hash, *opt;
  zuo_t *dir, *l, *p_handle, *result;
  int redirect_in, redirect_out, redirect_err;
  zuo_raw_handle_t pid, in, in_r, out, out_w, err, err_w;
  int argc = 1, i, ok;
  char **argv;
  void *env;

  check_path_string(who, command);
  for (l = args; l->tag == zuo_pair_tag; l = _zuo_cdr(l)) {
    if (_zuo_car(l)->tag != zuo_string_tag) {
      if (_zuo_cdr(l) == z.o_null) {
        options = _zuo_car(l);
        if (options->tag != zuo_trie_node_tag)
          zuo_fail1w(who, "not a string or hash table", options);
      } else
        zuo_fail1w(who, "not a string", _zuo_car(l));
    } else
      argc++;
  }

  argv = malloc(sizeof(char*) * (argc + 1));

  for (i = 0; i < argc; i++) {
    argv[i] = zuo_string_to_c(_zuo_car(command_and_args));
    command_and_args = _zuo_cdr(command_and_args);
  }
  argv[i] = NULL;

  redirect_in = redirect_out = redirect_err = 0;
  in_r = in = zuo_get_std_handle(0);
  out = out_w = zuo_get_std_handle(1);
  err = err_w = zuo_get_std_handle(2);

  opt = zuo_consume_option(&options, "stdin");
  if (opt != z.o_undefined) {
    if (opt == zuo_symbol("pipe")) {
      redirect_in = 1;
      zuo_pipe(&in_r, &in);
    } else if ((opt->tag == zuo_handle_tag)
               && (((zuo_handle_t *)opt)->u.h.status == zuo_handle_open_fd_in_status)) {
      in_r = ((zuo_handle_t *)opt)->u.h.handle;
    } else
      zuo_fail1w(who, "not 'pipe or an open input file descriptor", opt);
  }
  
  opt = zuo_consume_option(&options, "stdout");
  if (opt != z.o_undefined) {
    if (opt == zuo_symbol("pipe")) {
      redirect_out = 1;
      zuo_pipe(&out, &out_w);
    } else if ((opt->tag == zuo_handle_tag)
               && (((zuo_handle_t *)opt)->u.h.status == zuo_handle_open_fd_out_status)) {
      out_w = ((zuo_handle_t *)opt)->u.h.handle;
    } else
      zuo_fail1w(who, "not 'pipe or an open output file descriptor", opt);
  }
  
  opt = zuo_consume_option(&options, "stderr");
  if (opt != z.o_undefined) {
    if (opt == zuo_symbol("pipe")) {
      redirect_err = 1;
      zuo_pipe(&err, &err_w);
    } else if ((opt->tag == zuo_handle_tag)
               && (((zuo_handle_t *)opt)->u.h.status == zuo_handle_open_fd_out_status)) {
      err_w = ((zuo_handle_t *)opt)->u.h.handle;
    } else
      zuo_fail1w(who, "not 'pipe or an open output file descriptor", opt);
  }

  dir = zuo_consume_option(&options, "dir");
  if (dir != z.o_undefined)
    check_path_string(who, dir);

  opt = zuo_consume_option(&options, "env");
  if (opt != z.o_undefined) {
    zuo_t *l;
    for (l = opt; l->tag == zuo_pair_tag; l = _zuo_cdr(l)) {
      zuo_t *a = _zuo_car(l), *name, *val;
      zuo_int_t i;
    
      if (a->tag != zuo_pair_tag) break;
      name = _zuo_car(a);
      if (name->tag != zuo_string_tag) break;
      for (i = ZUO_STRING_LEN(name); i--; ) {
        int c = ZUO_STRING_PTR(name)[i];
        if ((c == '=') || (c == 0)) break;
      }
      if (i >= 0) break;
      
      val = _zuo_cdr(a);
      if (val->tag != zuo_string_tag) break;
    }
    if (l != z.o_null)
      zuo_fail1w(who, "not a valid environment variables list", opt);
    env = zuo_envvars_block(who, opt);
  } else
    env = NULL;

  check_options_consumed(who, options);
  
#ifdef ZUO_UNIX
  {
    zuo_t *open_fds = zuo_trie_keys(Z.o_fd_table, z.o_null);

    pid = fork();

    if (pid > 0) {
      /* This is the original process, which needs to manage the
         newly created child process. */
      ok = 1;
    } else if (pid == 0) {
      /* This is the new child process */
      char *msg;

      if (in_r != 0) {
        dup2(in_r, 0);
        if (redirect_in)
          close(in);
      }
      if (out_w != 1) {
        dup2(out_w, 1);
        if (redirect_out)
          close(out);
      }
      if (err_w != 2) {
        dup2(err_w, 2);
        if (redirect_err)
          close(err);
      }

      while (open_fds != z.o_null) {
        close(ZUO_HANDLE_RAW(_zuo_car(open_fds)));
        open_fds = _zuo_cdr(open_fds);
      }

      if ((dir == z.o_undefined)
          || (chdir(ZUO_STRING_PTR(dir)) == 0)) {
        if (env == NULL)
          execv(argv[0], argv);
        else
          execve(argv[0], argv, env);
        msg = "exec failed";
      } else
        msg = "chdir failed";

      if (write(2, msg, strlen(msg)) != 0)
        abort();

      _exit(1);
    } else {
      ok = 0;
    }
  }
#endif
#ifdef ZUO_WINDOWS
  {
    wchar_t *command_w, *cmdline_w, *wd_w;
    char *cmdline;
    int len = 9;
    STARTUPINFOW startup;
    PROCESS_INFORMATION info;
    DWORD cr_flag;

    if ((dir != z.o_undefined) && !zuo_path_is_absolute(ZUO_STRING_PTR(command)))
      command = zuo_build_path(dir, command);
    command_w = zuo_to_wide(ZUO_STRING_PTR(command));
    
    for (i = 0; i < argc; i++) {
      char *s = argv[i];
      argv[i] = zuo_cmdline_protect(s);
      free(s);
      len += strlen(argv[i]) + 1;
    }

    cmdline = malloc(len);

    len = 0;
    for (i = 0; i < argc; i++) {
      int alen = strlen(argv[i]);
      memcpy(cmdline + len, argv[i], alen);
      cmdline[len + alen] = ' ';
      len += alen + 1;
    }
    cmdline[len-1] = 0;

    cmdline_w = zuo_to_wide(cmdline);
    free(cmdline);

    memset(&startup, 0, sizeof(startup));
    startup.cb = sizeof(startup);
    startup.dwFlags = STARTF_USESTDHANDLES;
    startup.hStdInput = in_r;
    startup.hStdOutput = out_w;
    startup.hStdError = err_w;

    /* dup handles to make them inheritable */
    if (!DuplicateHandle(GetCurrentProcess(), startup.hStdInput,
			 GetCurrentProcess(), &startup.hStdInput,
			 0, 1 /* inherit */,
			 DUPLICATE_SAME_ACCESS))
      zuo_fail1w(who, "input handle dup failed", command);
    if (!DuplicateHandle(GetCurrentProcess(), startup.hStdOutput,
			 GetCurrentProcess(), &startup.hStdOutput,
			 0, 1 /* inherit */,
			 DUPLICATE_SAME_ACCESS))
      zuo_fail1w(who, "input handle dup failed", command);
    if (!DuplicateHandle(GetCurrentProcess(), startup.hStdError,
			 GetCurrentProcess(), &startup.hStdError,
			 0, 1 /* inherit */,
			 DUPLICATE_SAME_ACCESS))
      zuo_fail1w(who, "input handle dup failed", command);

    cr_flag = CREATE_NO_WINDOW | CREATE_UNICODE_ENVIRONMENT;

    if (dir != z.o_undefined)
      wd_w = zuo_to_wide(ZUO_STRING_PTR(dir));
    else
      wd_w = NULL;
    
    ok = CreateProcessW(command_w, cmdline_w, 
                        NULL, NULL, 1 /*inherit*/,
                        cr_flag, env, wd_w,
                        &startup, &info);

    free(command_w);
    free(cmdline_w);
    if (wd_w != NULL)
      free(wd_w);

    /* close inheritable dups */
    CloseHandle(startup.hStdInput);
    CloseHandle(startup.hStdOutput);
    CloseHandle(startup.hStdError);
    
    pid = info.hProcess;
  }
#endif

  if (!ok) {
    fprintf(stderr, "attempted command:");
    for (i = 0; i < argc; i++)
      fprintf(stderr, "  %s\n", argv[i]);
    zuo_fail("exec failed");
  }

  if (env != NULL)
    free(env);

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
#ifdef ZUO_UNIX
  trie_set(Z.o_pid_table, pid, p_handle, p_handle);
#endif

  result = z.o_empty_hash;
  result = zuo_hash_set(result, zuo_symbol("process"), p_handle);
  if (redirect_in)
    result = zuo_hash_set(result, zuo_symbol("stdin"), zuo_fd_handle(in, zuo_handle_open_fd_out_status));
  if (redirect_out)
    result = zuo_hash_set(result, zuo_symbol("stdout"), zuo_fd_handle(out, zuo_handle_open_fd_in_status));
  if (redirect_err)
    result = zuo_hash_set(result, zuo_symbol("stderr"), zuo_fd_handle(err, zuo_handle_open_fd_in_status));

  return result;
}

static int is_process_handle(zuo_t *p) {
  return ((p->tag == zuo_handle_tag)
          && ((((zuo_handle_t *)p)->u.h.status != zuo_handle_process_done_status)
              || (((zuo_handle_t *)p)->u.h.status != zuo_handle_process_running_status)));
}

zuo_t *zuo_process_status(zuo_t *p) {
  if (!is_process_handle(p))
    zuo_fail1w("process-status", "not a process handle", p);

  if (((zuo_handle_t *)p)->u.h.status == zuo_handle_process_running_status)
    return zuo_symbol("running");
  else
    return zuo_integer((zuo_int_t)((zuo_handle_t *)p)->u.h.handle);
}

zuo_t *zuo_process_wait(zuo_t *pids_i) {
  zuo_t *l;

  if (is_process_handle(pids_i))
    pids_i = zuo_cons(pids_i, z.o_null);
    
  for (l = pids_i; l->tag == zuo_pair_tag; l = _zuo_cdr(l))
    if (!is_process_handle(_zuo_car(l)))
      break;
  if (l != z.o_null)
    zuo_fail1w("process-wait", "not a process handle or list of process handles", pids_i);

#ifdef ZUO_UNIX
  /* loop until on of the handles is marked as done */
  while (1) {
    pid_t pid;
    int stat_loc;

    for (l = pids_i; l != z.o_null; l = _zuo_cdr(l)) {
      zuo_t *p = _zuo_car(l);
      if (((zuo_handle_t *)p)->u.h.status == zuo_handle_process_done_status)
        return p;
    }
    
    /* wait for any process to exit, and update the corresponding handle */
    pid = wait(&stat_loc);
    if (pid < 0)
      zuo_fail("process wait failed");

    if (pid >= 0) {
      zuo_t *p = trie_lookup(Z.o_pid_table, pid);
      if (p->tag == zuo_handle_tag) {
        ((zuo_handle_t *)p)->u.h.status = zuo_handle_process_done_status;
        if (WIFEXITED(stat_loc))
          ((zuo_handle_t *)p)->u.h.handle = (zuo_raw_handle_t)WEXITSTATUS(stat_loc);
        else {
          int r = WTERMSIG(stat_loc);
          if (r == 0) r = 256;
          ((zuo_handle_t *)p)->u.h.handle = (zuo_raw_handle_t)r;
        }
        trie_set(Z.o_pid_table, pid, z.o_undefined, z.o_undefined);
      }
    }
  }
#endif
#ifdef ZUO_WINDOWS
  /* loop until on of the handles is marked as done */
  while (1) {
    HANDLE *a = malloc(sizeof(HANDLE) * zuo_length_int(pids_i));
    zuo_int_t i = 0;
    
    for (l = pids_i; l != z.o_null; l = _zuo_cdr(l)) {
      zuo_t *p = _zuo_car(l);
      HANDLE sci = ZUO_HANDLE_RAW(p);
      DWORD w;
      if (GetExitCodeProcess(sci, &w)) {
        if (w != STILL_ACTIVE) {
          ((zuo_handle_t *)p)->u.h.status = zuo_handle_process_done_status;
          ((zuo_handle_t *)p)->u.h.handle = (zuo_raw_handle_t)(intptr_t)w;
          return p;
	}
      } else
	zuo_fail1w("process-wait", "status query failed", p);
      a[i++] = sci;
    }

    (void)WaitForMultipleObjects(i, a, FALSE, 0);
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
      zuo_fail("failed to get self");
    } else
      break;
  }
  s[len] = 0;

  return s;
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
    zuo_fail("failed to get self");
  s = malloc(len);
  r = sysctl(mib, 4, s, &len, NULL, 0);
  if (r < 0)
    zuo_fail("failed to get self");
  
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

#elif defined(ZUO_WINDOWS)

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

  return zuo_from_wide(path);
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
  if (zuo_path_is_absolute(exec_file)) {
    /* Absolute path */
    return strdup(exec_file);
  } else if (has_slash(exec_file)) {
    /* Relative path with a directory: */
    return zuo_string_to_c(zuo_path_to_complete_path(zuo_string(exec_file)));
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

      m = zuo_build_path(zuo_string(path), zuo_string(exec_file));

      if (access(ZUO_STRING_PTR(m), X_OK) == 0)
        return zuo_string_to_c(zuo_path_to_complete_path(m));

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

#define TRIE_SET_TOP_ENV(name, make_prim)        \
  do {                                           \
    if (boot_image == NULL) {                    \
      zuo_t *sym = zuo_symbol(name);             \
      zuo_trie_set(z.o_top_env, sym, make_prim); \
    } else {                                     \
      zuo_t *sym = z.o_undefined;                \
      (void)make_prim;                           \
    }                                            \
  } while (0)

#define ZUO_TOP_ENV_SET_PRIMITIVE0(name, proc) \
  TRIE_SET_TOP_ENV(name, zuo_primitive0(proc, sym))
#define ZUO_TOP_ENV_SET_PRIMITIVE1(name, proc) \
  TRIE_SET_TOP_ENV(name, zuo_primitive1(proc, sym))
#define ZUO_TOP_ENV_SET_PRIMITIVE2(name, proc) \
  TRIE_SET_TOP_ENV(name, zuo_primitive2(proc, sym))
#define ZUO_TOP_ENV_SET_PRIMITIVE3(name, proc) \
  TRIE_SET_TOP_ENV(name, zuo_primitive3(proc, sym))
#define ZUO_TOP_ENV_SET_PRIMITIVEN(name, proc, min_n) \
  TRIE_SET_TOP_ENV(name, zuo_primitiveN(proc, min_n, sym))
#define ZUO_TOP_ENV_SET_VALUE(name, val)  \
  zuo_trie_set(z.o_top_env, zuo_symbol(name), val)

int main(int argc, char **argv) {
  char *load_file = NULL, *library_path = NULL, *boot_image = NULL;
  char *argv0 = argv[0];
  zuo_t *exe_path, *load_path;

  zuo_check_sanity();

  zuo_configure();

  argc--;
  argv++;

  while (argc > 0) {
    if (!strcmp(argv[0], "-h") || !strcmp(argv[0], "--help")) {
      fprintf(stdout, ("\n"
                       "usage: %s [<option> ...] [<file-or-dir> <argument> ...]\n"
                       "\n"
                       "If <file-or-dir> is a file, it is used as a module path to load.\n"
                       "If <file-or-dir> is a directory, \"main.zuo\" is loded.\n"
                       "If <file-or-dir> is \"\", a module is read from stdin.\n"
                       "The <argument>s are made available via the `system-env` procedure.\n"
                       "\n"
                       "Supported <option>s:\n"
                       "\n"
                       "  -B <file>, --boot <file>\n"
                       "     Load dump from <file> as the initial image\n"
                       "  -X <dir>, --collects <dir>\n"
                       "     Use <dir> as the library-collection root, overriding `ZUO_LIB`;\n"
                       "     the default is \"%s\" relative to the executable\n"
                       "  --\n"
                       "     No argument following this switch is used as a switch\n"
                       "  -h, --help\n"
                       "     Show this information and exit, ignoring other options\n"
                       "\n"
                       "If an <option> switch is provided multiple times, the last\n"
                       "instance takes precedence.\n"
                       "\n"),
              argv0,
              ((ZUO_LIB_PATH == NULL) ? "[disabled]" : ZUO_LIB_PATH));
      exit(0);
    } else if (!strcmp(argv[0], "-B") || !strcmp(argv[0], "--boot")) {
      if (argc > 1) {
        boot_image = argv[1];
        argc -= 2;
        argv += 2;
      } else {
        fprintf(stderr, "%s: expected a path after -B", argv0);
        zuo_fail("");
      }
    } else if (!strcmp(argv[0], "-X") || !strcmp(argv[0], "--collects")) {
      if (argc > 1) {
        if (argv[1][0] == 0)
          zuo_lib_path = NULL;
        else
          library_path = argv[1];
        argc -= 2;
        argv += 2;
      } else {
        fprintf(stderr, "%s: expected a path after -X", argv0);
        zuo_fail("");
      }
    } else if (!strcmp(argv[0], "--")) {
      argc--;
      argv++;
      break;
    } else if (argv[0][0] == '-') {
      fprintf(stderr, "%s: unrecognized flag: %s", argv0, argv[0]);
      zuo_fail("");
    } else
      break;
  }

  if (argc > 0) {
    load_file = argv[0];
    argc--;
    argv++;
  }

  /* these initial constants and tables might get replaced by loading
     an image, but we need them to register primitives: */
  z.o_undefined = zuo_new(zuo_singleton_tag, sizeof(zuo_forwarded_t));
  z.o_void = zuo_new(zuo_singleton_tag, sizeof(zuo_forwarded_t));
  z.o_apply = zuo_new(zuo_singleton_tag, sizeof(zuo_forwarded_t));
  z.o_call_cc = zuo_new(zuo_singleton_tag, sizeof(zuo_forwarded_t));
  z.o_done_k = zuo_cont(zuo_done_cont, z.o_undefined, z.o_undefined, z.o_undefined, z.o_undefined);
  z.o_intern_table = zuo_trie_node();
  z.o_top_env = zuo_trie_node();

  Z.o_interp_k = z.o_done_k; /* in case of a failure that triggers a stack trace */

  ZUO_TOP_ENV_SET_PRIMITIVE1("pair?", zuo_pair_p);
  ZUO_TOP_ENV_SET_PRIMITIVE1("null?", zuo_null_p);
  ZUO_TOP_ENV_SET_PRIMITIVE1("integer?", zuo_integer_p);
  ZUO_TOP_ENV_SET_PRIMITIVE1("string?", zuo_string_p);
  ZUO_TOP_ENV_SET_PRIMITIVE1("symbol?", zuo_symbol_p);
  ZUO_TOP_ENV_SET_PRIMITIVE1("hash?", zuo_hash_p);
  ZUO_TOP_ENV_SET_PRIMITIVE1("list?", zuo_list_p);
  ZUO_TOP_ENV_SET_PRIMITIVE1("procedure?", zuo_procedure_p);
  ZUO_TOP_ENV_SET_PRIMITIVE1("path-string?", zuo_path_string_p);
  ZUO_TOP_ENV_SET_PRIMITIVE1("variable?", zuo_variable_p);
  ZUO_TOP_ENV_SET_PRIMITIVE1("handle?", zuo_handle_p);
  ZUO_TOP_ENV_SET_PRIMITIVEN("void", zuo_make_void, 0);

  ZUO_TOP_ENV_SET_PRIMITIVE1("procedure-arity-mask", zuo_procedure_arity_mask);

  ZUO_TOP_ENV_SET_PRIMITIVE2("cons", zuo_cons);
  ZUO_TOP_ENV_SET_PRIMITIVE1("car", zuo_car);
  ZUO_TOP_ENV_SET_PRIMITIVE1("cdr", zuo_cdr);
  ZUO_TOP_ENV_SET_PRIMITIVEN("list", zuo_list, 0);
  ZUO_TOP_ENV_SET_PRIMITIVEN("append", zuo_append, 0);
  ZUO_TOP_ENV_SET_PRIMITIVE1("reverse", zuo_reverse);
  ZUO_TOP_ENV_SET_PRIMITIVE1("length", zuo_length);

  ZUO_TOP_ENV_SET_PRIMITIVE1("not", zuo_not);
  ZUO_TOP_ENV_SET_PRIMITIVE2("eq?", zuo_eq);

  ZUO_TOP_ENV_SET_PRIMITIVEN("+", zuo_add, 0);
  ZUO_TOP_ENV_SET_PRIMITIVEN("-", zuo_subtract, 1);
  ZUO_TOP_ENV_SET_PRIMITIVEN("*", zuo_multiply, 0);
  ZUO_TOP_ENV_SET_PRIMITIVE2("quotient", zuo_quotient);
  ZUO_TOP_ENV_SET_PRIMITIVE2("modulo", zuo_modulo);
  ZUO_TOP_ENV_SET_PRIMITIVE2("<", zuo_lt);
  ZUO_TOP_ENV_SET_PRIMITIVE2("<=", zuo_le);
  ZUO_TOP_ENV_SET_PRIMITIVE2("=", zuo_eql);
  ZUO_TOP_ENV_SET_PRIMITIVE2(">=", zuo_ge);
  ZUO_TOP_ENV_SET_PRIMITIVE2(">", zuo_gt);
  ZUO_TOP_ENV_SET_PRIMITIVE2("bitwise-and", zuo_bitwise_and);
  ZUO_TOP_ENV_SET_PRIMITIVE2("bitwise-ior", zuo_bitwise_ior);
  ZUO_TOP_ENV_SET_PRIMITIVE2("bitwise-xor", zuo_bitwise_xor);
  ZUO_TOP_ENV_SET_PRIMITIVE1("bitwise-not", zuo_bitwise_not);

  ZUO_TOP_ENV_SET_PRIMITIVEN("string", zuo_build_string, 0);
  ZUO_TOP_ENV_SET_PRIMITIVE1("string-length", zuo_string_length);
  ZUO_TOP_ENV_SET_PRIMITIVE2("string-ref", zuo_string_ref);
  ZUO_TOP_ENV_SET_PRIMITIVE2("string-u32-ref", zuo_string_u32_ref);
  ZUO_TOP_ENV_SET_PRIMITIVE3("substring", zuo_substring);
  ZUO_TOP_ENV_SET_PRIMITIVE2("string=?", zuo_string_eql);
  ZUO_TOP_ENV_SET_PRIMITIVE1("string->symbol", zuo_string_to_symbol);
  ZUO_TOP_ENV_SET_PRIMITIVE1("string->uninterned-symbol", zuo_string_to_uninterned_symbol);
  ZUO_TOP_ENV_SET_PRIMITIVE1("symbol->string", zuo_symbol_to_string);

  ZUO_TOP_ENV_SET_PRIMITIVEN("hash", zuo_hash, 0);
  ZUO_TOP_ENV_SET_PRIMITIVE3("hash-ref", zuo_hash_ref);
  ZUO_TOP_ENV_SET_PRIMITIVE3("hash-set", zuo_hash_set);
  ZUO_TOP_ENV_SET_PRIMITIVE2("hash-remove", zuo_hash_remove);
  ZUO_TOP_ENV_SET_PRIMITIVE1("hash-keys", zuo_hash_keys);
  ZUO_TOP_ENV_SET_PRIMITIVE1("hash-count", zuo_hash_count);
  ZUO_TOP_ENV_SET_PRIMITIVE2("hash-keys-subset?", zuo_hash_keys_subset_p);

  ZUO_TOP_ENV_SET_PRIMITIVE2("opaque", zuo_opaque);
  ZUO_TOP_ENV_SET_PRIMITIVE3("opaque-ref", zuo_opaque_ref);

  ZUO_TOP_ENV_SET_PRIMITIVE2("build-path", zuo_build_path);
  ZUO_TOP_ENV_SET_PRIMITIVE1("split-path", zuo_split_path);
  ZUO_TOP_ENV_SET_PRIMITIVE1("relative-path?", zuo_relative_path_p);
  ZUO_TOP_ENV_SET_PRIMITIVE2("module-path-join", zuo_module_path_join);

  ZUO_TOP_ENV_SET_PRIMITIVE1("variable", zuo_variable);
  ZUO_TOP_ENV_SET_PRIMITIVE1("variable-ref", zuo_variable_ref);
  ZUO_TOP_ENV_SET_PRIMITIVE2("variable-set!", zuo_variable_set);

  ZUO_TOP_ENV_SET_PRIMITIVE1("fd-open-input", zuo_fd_open_input);
  ZUO_TOP_ENV_SET_PRIMITIVE1("fd-open-output", zuo_fd_open_output);
  ZUO_TOP_ENV_SET_PRIMITIVE1("fd-close", zuo_fd_close);
  ZUO_TOP_ENV_SET_PRIMITIVE2("fd-read", zuo_fd_read);
  ZUO_TOP_ENV_SET_PRIMITIVE2("fd-write", zuo_fd_write);

  ZUO_TOP_ENV_SET_PRIMITIVE2("stat", zuo_stat);
  ZUO_TOP_ENV_SET_PRIMITIVE1("rm", zuo_rm);
  ZUO_TOP_ENV_SET_PRIMITIVE2("mv", zuo_mv);
  ZUO_TOP_ENV_SET_PRIMITIVE1("mkdir", zuo_mkdir);
  ZUO_TOP_ENV_SET_PRIMITIVE1("rmdir", zuo_rmdir);
  ZUO_TOP_ENV_SET_PRIMITIVE1("ls", zuo_ls);
  ZUO_TOP_ENV_SET_PRIMITIVE2("ln", zuo_ln);
  ZUO_TOP_ENV_SET_PRIMITIVE1("readlink", zuo_readlink);
  ZUO_TOP_ENV_SET_PRIMITIVE0("current-time", zuo_current_time);
  
  ZUO_TOP_ENV_SET_PRIMITIVEN("process", zuo_process, 1);
  ZUO_TOP_ENV_SET_PRIMITIVE1("process-status", zuo_process_status);
  ZUO_TOP_ENV_SET_PRIMITIVE1("process-wait", zuo_process_wait);

  ZUO_TOP_ENV_SET_PRIMITIVEN("error", zuo_error, 0);
  ZUO_TOP_ENV_SET_PRIMITIVEN("alert", zuo_alert, 0);
  ZUO_TOP_ENV_SET_PRIMITIVEN("~v", zuo_tilde_v, 0);
  ZUO_TOP_ENV_SET_PRIMITIVEN("~a", zuo_tilde_a, 0);
  ZUO_TOP_ENV_SET_PRIMITIVEN("~s", zuo_tilde_s, 0);

  ZUO_TOP_ENV_SET_PRIMITIVE2("read-from-string-all", zuo_read_all);
  ZUO_TOP_ENV_SET_PRIMITIVE1("kernel-eval", zuo_kernel_eval);
  ZUO_TOP_ENV_SET_PRIMITIVE1("module->hash", zuo_module_to_hash);
  ZUO_TOP_ENV_SET_PRIMITIVE1("module-path?", zuo_module_path_p);
  ZUO_TOP_ENV_SET_PRIMITIVE0("kernel-env", zuo_kernel_env);

  ZUO_TOP_ENV_SET_PRIMITIVE0("runtime-env", zuo_runtime_env);

  ZUO_TOP_ENV_SET_PRIMITIVE1("dump-image-and-exit", zuo_dump_image_and_exit);

# if EMBEDDED_IMAGE
  if (!boot_image)
    zuo_fasl_restore((char *)emedded_boot_image, emedded_boot_image_len * sizeof(zuo_int32_t));
  else {
# endif

    if (boot_image) {
      FILE *f = fopen(boot_image, "r");
      zuo_int_t len;
      char *dump = zuo_drain(f, 0, -1, &len);
      fclose(f);
      zuo_fasl_restore(dump, len);
      free(dump);
    } else {
      /* Create remaining costants and tables */
      z.o_true = zuo_new(zuo_singleton_tag, sizeof(zuo_forwarded_t));
      z.o_false = zuo_new(zuo_singleton_tag, sizeof(zuo_forwarded_t));
      z.o_null = zuo_new(zuo_singleton_tag, sizeof(zuo_forwarded_t));
      z.o_eof = zuo_new(zuo_singleton_tag, sizeof(zuo_forwarded_t));
      z.o_empty_hash = zuo_trie_node();
    
      z.o_quote_symbol = zuo_symbol("quote");
      z.o_lambda_symbol = zuo_symbol("lambda");
      z.o_let_symbol = zuo_symbol("let");
      z.o_begin_symbol = zuo_symbol("begin");
      z.o_if_symbol = zuo_symbol("if");
    
      z.o_modules = z.o_null;

      ZUO_TOP_ENV_SET_VALUE("apply", z.o_apply);
      ZUO_TOP_ENV_SET_VALUE("call/cc", z.o_call_cc);
      ZUO_TOP_ENV_SET_VALUE("eof", z.o_eof);
    }

# if EMBEDDED_IMAGE
  }
# endif
        
  Z.o_interp_e = Z.o_interp_env = Z.o_interp_v = Z.o_interp_in_proc = z.o_false;
  Z.o_interp_k = z.o_done_k;
  Z.o_pending_modules = z.o_null;
  Z.o_stash = z.o_false;

#ifdef ZUO_UNIX
  Z.o_pid_table = zuo_trie_node();
  Z.o_fd_table = zuo_trie_node();
#endif

  Z.o_current_directory = zuo_current_directory();

  exe_path = zuo_self_path(argv0);

  if (library_path) {
    Z.o_library_path = zuo_path_to_complete_path(zuo_string(library_path));
  } else if (zuo_lib_path != NULL) {
    Z.o_library_path = zuo_string(zuo_lib_path);
    if (zuo_relative_path_p(Z.o_library_path) == z.o_false)
      Z.o_library_path = zuo_build_path(_zuo_car(zuo_split_path(exe_path)),
                                        Z.o_library_path);
  } else
    Z.o_library_path = z.o_false;

  if (load_file == NULL) {
    load_file = "main.zuo";
    load_path = zuo_string(load_file);
    if (zuo_stat(load_path, z.o_true) == z.o_false) {
      fprintf(stderr, "%s: no file specified, and no \"main.zuo\" found", argv0);
      zuo_fail("");
    }
  } else if (load_file[0] != 0) {
    zuo_t *st;
    load_path = zuo_string(load_file);
    st = zuo_stat(load_path, z.o_true);
    if ((st != z.o_false)
        && (zuo_hash_ref(st, zuo_symbol("type"), z.o_false) == zuo_symbol("dir")))
      load_path = zuo_build_path(load_path, zuo_string("main.zuo"));
  } else
    load_path = zuo_path_to_complete_path(zuo_string("stdin"));

  Z.o_runtime_env = zuo_make_runtime_env(exe_path, load_file, argc, argv);

  if (load_file[0] == 0) {
    zuo_int_t in_len;
    char *input = zuo_drain(stdin, 0, -1, &in_len);
    (void)zuo_eval_module(load_path, input, in_len);
  } else
    (void)zuo_module_to_hash(load_path);

  return 0;
}
