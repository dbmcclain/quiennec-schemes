
256 1024 * 1024 * 1 1 s" gc.fs" included drop 2drop

: cells+ ( addr n -- addr' )
    cells + ;

: cells+@ ( addr ncells -- obj )
    cells+ @ ;

: cells+! ( val addr ncells -- )
    cells+ ! ;

: swap! ( addr obj -- )
    swap ! ;

: over@ ( addr x -- addr x val )
    over @ ;

: over! ( addr val -- addr )
    over ! ;

: car ( addr -- obj )
    1 cells+@ ;

: cdr ( addr -- addr' )
    @ ;

0 variable %env

: @env ( -- env )
    %env @ ;

: !env ( env -- )
    %env ! ;

: 'shallow ( j -- addr )
    cells @env car + ;

: @shallow ( j -- obj )
    'shallow @ ;

: !shallow ( obj j -- )
    'shallow over swap! ;

: cells-alloc ( n -- addr )
    cells alloc throw ;

: cons ( a b -- cons )
    2 cells-alloc
    swap over!
    swap over 1 cells+! ;

: nthcdr ( n 'cons -- addr )
    swap 0 do dup if cdr else leave then loop ;

: 'deep ( i j -- addr )
    cells swap @env nthcdr car + ;


: @deep ( i j -- obj )
    'deep @ ;

: !deep ( obj i j -- )
    'deep over swap! ;

0 variable undefined

: @chk? ( x -- x )
    @ dup undefined = if undefined throw then ;


\ : apply ( args nargs fn -- obj )
\     <r ->list r> execute ;

: closed-fn  ( closure -- fn )
    car ;

: closed-env ( closure -- env )
    cdr ;

0 variable too-few-args
0 variable incorrect-nbr-args

: chkargs ( frame closure nargs arity -- frame )
    dup 0<
    if abs < too-few-args throw
    else <>
	if incorrect-nbr-args throw
	then
    then
    closed-env cons !env ;

0 variable tmp

create template ] ahead [ 1 , ] then [

: close-over ( arity -- here )
    postpone ahead
    here swap postpone literal \ arity
    postpone chkargs
    ; immediate

: form-closure ( fn -- closure )
    @env cons ;

: end-closure ( here -- closure )
    postpone ; postpone then
    postpone literal
    postpone form-closure ; immediate

: restore-env ( saved-env ans -- ans )
    swap !env ;

: invoke ( frame closure nargs -- ans )
    over closed-fn execute ;

variable fib
: fib-fn  ( frame closure nargs -- ans )
    1 chkargs
    0 @shallow 2 <
    if
	1
    else
	@env
	2 cells-alloc
	0 @shallow 1 -
	over!
	FIB @chk?
	1 invoke
	restore-env
	@env
	2 cells-alloc
	0 @shallow 2 -
	over!
	FIB @chk?
	1 invoke
	restore-env
	+
    then ;

' fib-fn form-closure fib !

(
2 cells-alloc 35 over !
fib @ 1 invoke .
)

    