
\ ------------------------------------------

: 'cdr ( 'cons -- 'cdr )
    1 cells + ;

: cdr ( 'cons -- obj )
    'cdr @ ;

: !cdr ( obj 'cons -- )
    'cdr ! ;

: 'nth ( i cons -- obj )
    swap 0 do cdr loop ;

\ -----------------------------------

: 3rev ( a b c -- c b a )
    swap rot ;

: rot-swap ( a b c -- b a c )
    rot swap ;

: swap! ( addr obj -- )
    swap ! ;

: over! ( addr obj -- addr )
    over ! ;

: 0! ( addr -- )
    0 swap! ;

\ -----------------------------------

: >aa ( a -- a a )
    dup ;

: >ba ( a b -- b a )
    swap ;

: >aba ( a b -- a b a )
    over ;

: >bab ( a b -- b a b )
    swap over ;

: >acb ( a b c -- a c b )
    swap ;

: >bac ( a b c -- b a c )
    rot swap ;

: >bca ( a b c -- b c a )
    rot ;

: >cab ( a b c -- c a b )
    -rot ;

: >cba ( a b c -- c b a )
    swap rot ;

\ --------------------------------------

: cons ( a b -- a . b )
    new-cons-cell >bab !cdr >bab ! ;


: ->vector ( args... n -- vector )
    dup alloc-cells 0 do >bab i cells + ! loop ;

0 variable closure

: obj-type ( addr -- type )
    @ ;

: closure-env ( closure -- env )
    1 cells + @ ;

: closure-code ( closure -- code )
    2 cells + @ ;

: create-closure ( code env -- closure )
    'closure 3 ->vector ;

\ ------------------------------------------

0 variable ip

: eval ( 'list -- obj )
    ip @ <r dup ip ! @ execute r> ip ! ;

: arg ( n -- obj )
    cells ip @ + @ ;

: arg1 ( -- obj )
    1 arg ;
: arg2 ( -- obj )
    2 arg ;
: arg3 ( -- obj )
    3 arg ;

\ ------------------------------------------

0 variable %env

: 'env-cell ( j -- 'obj )
    cells %env @ @ + ;

: shallow-argument-ref ( -- obj )
    arg1 'env-cell @ ;

: shallow-argument-set! ( -- )
    arg2 eval arg1 'env-cell ! ;

\ ------------------------------------------

: 'env-nth-cell ( i j -- 'obj )
    cells swap %env @ 'nth @ + ;

: deep-argument-ref ( -- obj )
    arg1 arg2 'env-nth-cell @ ;

: deep-argument-set! ( -- )
    arg3 eval arg1 arg2 'env-nth-cell ! ;

\ ------------------------------------------

: 'sym-val ( sym -- 'obj )
    @ ;

: global-ref ( -- obj )
    arg1 'sym-val @ ;

: global-set! ( ip sym obj -- ip )
    arg2 eval arg1 'sym-val ! ;

: const ( -- obj )
    arg1 ;

\ ------------------------------------------

: alternative ( thunk-test thunk-t thunk-f -- obj )
    arg1 eval if arg2 else arg3 then eval ;

: sequence ( thunk more-thunk -- obj )
    arg1 eval drop arg2 eval ;

: tr-fix-let ( m* m+ -- obj )
    arg1 eval %env @ cons %env ! arg2 eval ;

: fix-let ( m* m+ -- obj )
    %env @ <r tr-fix-let r> %env ! ;

: close-over ( m -- closure )
    arg1 %env @ create-closure ;

: invoke ( fn frame -- obj )
    ip @ <r 1 cells - ip ! execute r> ip ! ;

: tr-regular-call ( m m* -- obj )
    arg1 eval arg2 eval invoke ;

: regular-call ( m m* -- obj )
    %env @ <r tr-regular-call r> %env ! ;

: allocate-frame ( n -- frame-vector )
    dup 1+ allocate-cells
    swap cells over + 0! ; \ null out last cell

: 'argument ( m m* index -- obj addr )
    arg2 eval
    arg1 eval
    over arg3 cells + ;

: store-argument ( m m* rank -- frame-vector )
    'argument ! ;

: cons-argument ( m m* arity -- frame-vector )
    'argument swap over @ cons over ! ;

\ --------------------------------------------

: list-> ( 'list -- items.. count )
    0 swap begin dup while dup cdr swap car rot 1+ rot repeat drop ;

: ->list ( items... count -- 'list )
    0 swap 0 do cons loop ;




    