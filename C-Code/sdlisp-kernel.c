
#include <assert.h>
#include <stdlib.h>
#include <string.h>

typedef long (*obj);

typedef struct cons {
  obj          car;
  struct cons* cdr;
} cons;

typedef struct sym {
  obj   val;
  char *name;
  cons *plist;
} sym;

typedef obj ((*closure_fn)(cons*, obj*));

typedef struct {
  cons        *env;
  closure_fn  *fn;
} closure;

#define TAG_MEM     0
#define TAG_INT     1 // int has odd value
#define TAG_CONS    2
#define TAG_SYM     3
#define TAG_VEC     4
#define TAG_CLOSURE 5
#define TAG_UNKN    6
#define TAG_MASK    7

#define AddTag(p,tag) ((long)(p) | tag)
#define Tag(p)      ((long)(p) & TAG_MASK)
#define Ptr(p)      ((obj)((long)(p) & ~TAG_MASK))

#define Car(p)      (((cons*)Ptr(p))->car)
#define Cdr(p)      (((cons*)Ptr(p))->cdr)

#define SymVal(p)   (((sym*)Ptr(p))->val)
#define SymPlist(p) (((sym*)Ptr(p))->plist)

#define ClosureEnv(p) (((closure*)Ptr(p))->env)
#define ClosureFn(p)  (((closure*)Ptr(p))->fn)

#define consp(p)    (assert(Tag(p) == TAG_CONS))
#define symp(p)     (assert(Tag(p) == TAG_SYM))
#define vecp(p)     (assert(Tag(p) == TAG_VEC))
#define intp(p)     (assert(Tag(p) == TAG_INT))

extern obj   Eval(cons *env, obj *m);

obj Undefined;

// ----------------------------------------------

/*
long tst()
{
  return sizeof(long);
}
*/

cons* Cons(obj a, cons *b)
{
  cons *p = (cons*)malloc(sizeof(cons));
  p->car = a;
  p->cdr = b;
  return (cons*)AddTag(p, TAG_CONS);
}

sym* Symbol(char *name)
{
  sym *p = (sym*)malloc(sizeof(sym));
  p->name = (char*)malloc(strlen(name)+1);
  strcpy(p->name, name);
  p->val  = Undefined;
  p->plist = 0;
  return (sym*)AddTag(p, TAG_SYM);
}

closure* Closure(cons *env, closure_fn *fn)
{
  closure *p = (closure*)malloc(sizeof(closure));
  p->env = env;
  p->fn  = fn;
  return (closure*)AddTag(p, TAG_CLOSURE);
}

void init()
{
  Undefined = (obj)Cons((obj)Symbol("Undefined"),
			(cons*)Symbol("Symbol"));
}

// ---------------------------------------------

obj shallow_argument_ref (cons *env, long j)
{
  obj *frame = (obj*)Car(env);
  return frame[j];
}

cons* nthCdr(cons *p, long n)
{
  while(p && n > 0)
    {
      --n;
      p = Cdr(p);
    }
  return p;
}

obj deep_argument_ref (cons *env, long i, long j)
{
  obj *frame = (obj*)Car(nthCdr(env, i));
  return frame[j];
}

obj shallow_argument_set (cons *env, long j, obj *m)
{
  obj *frame = (obj*)Car(env);
  frame[j] = Eval(env, m);
}

obj deep_argument_set (cons *env, long i, long j, obj *m)
{
  obj *frame = (obj*)Car(nthCdr(env,i));
  frame[j] = Eval(env, m);
}
  
obj global_ref(cons *env, sym *p)
{
  return SymVal(p);
}

obj checked_global_ref(cons *env, sym *p)
{
  obj v = SymVal(p);
  assert(v != Undefined);
  return v;
}

obj global_set(cons *env, sym *p, obj *m)
{
  SymVal(p) = Eval(env, m);
}

obj sequence(cons *env, obj *m1, obj *m2)
{
  Eval(env, m1);
  Eval(env, m2);
}

obj alternative(cons *env, obj *mtest, obj *mtrue, obj *mfalse)
{
  if(Eval(env, mtest))
    Eval(env, mtrue);
  else
    Eval(env, mfalse);
}

obj tr_fix_let(cons *env, obj *mbindings, obj *mbody)
{
  obj *pbind = (obj*)Eval(env, mbindings);
  cons *new_env = (cons*)Cons((obj)pbind, env);
  obj v = Eval(new_env, mbody);
  return v;
}

#define fix_let  tr_fix_let

closure* close_over(cons *env, closure_fn *fn)
{
  return Closure(env, fn);
}

obj tr_regular_call(cons *env, closure *mclosure, obj *margs)
{
  obj *pargs = (obj*)Eval(env, margs);
  obj v = (*ClosureFn(mclosure))(ClosureEnv(mclosure), pargs);
  return v;
}

#define regular_call tr_regular_call

obj* alloc_frame(long size)
{
  obj *p = (obj*)malloc((size+1)*sizeof(long));
  p[size] = 0;
  return p;
}

obj* store_argument(cons *env, obj *m, obj *margs, long rank)
{
  obj *p = (obj*)Eval(env, margs);
  obj  v = Eval(env, m);
  p[rank] = v;
  return p;
}

obj* cons_argument(cons *env, obj *m, obj *margs, long arity)
{
  obj *p = (obj*)Eval(env, margs);
  obj  v = Eval(env, m);
  p[arity] = (obj)Cons(v, (cons*)p[arity]);
  return p;
}
