# bcc





## what's bcc?

This project translates, into C, programs in a C-style language called BCC
(pronounced `Bee-Clus-Clus`).  BCC stands for any of:
 
    a Bit Cleaner C
    Bovine Cow vernaCular
    Bland Cast of C 
    Boys' Christian C
   
We designed BCC in two steps.  First, we extracted from C99 a simpler core
that, like C, is both a pleasure to write for day-to-day programming and also
easy for the compiler optimize --- and that, unlike C, we can learn in a
day.[\*] Then we yielded to the temptation to add three ideas to this core: we
allow greater conversation between source files before each crystallizes into a
**translation unit**; we introduce **lambda**s --- that is, function literals
potentially with captured state; and we make C's **type** system stricter, more
symmetrical, and somewhat polymorphic.
  
[\*] Yes, C (say, C99) is simple.  But do you remember answers to the
following?  Must`char`s be 8 bits?  Must a comment that begins with `//` and
whose first line contains no backslash be a one-line comment?  Is `volatile
restrict` the same as `restrict`?  Must bit fields appear in memory in the same
order we write them within a `struct`?  (Answers at the end of this document
--- see [\*\*\*]!)


### bcc vs other c-like languages

By *C-like* I mean compiled, statically typed descendants of Algol but not
Simula that enjoy small specifications and that expose all mechanisms (pointers
etc).  They abstract by conglomerating types and by composing functions rather
than by anything that rhymes with `inheritance`.  They probably have curly
brace block syntax.

###### C

###### GO

###### HARE

###### RUST

###### ZIG



### brief notes on bcc's design goals

In brief: we want to maintain C's transparency[\*] --- no hidden mechanisms ---
while encouraging stricter interfaces and a more symmetrical use of types.

Herb Sutter distinguishes between "simplicity of program texts" vs "simplicity
of the language specification", two virtues that often conflict when designing
incremental changes to the C++ standard.  We hope to maintain both virtues to
the degree that C enjoys for the kinds of hobby program we write in C.

[\*] I suppose we want to increase vertical transparency and horizontal 
opacity. 

###### PERFORMANCE  

Beyond algorithms, good use of hardware can reduce wall-time dramatically.  To
exploit data and instruction **caches**, we compactify our data layout,
localize data accesses, and strive for compact object code.  To exploit
**branch prediction**, we avoid hard-to-predict branches.  To exploit
**multi-core processors**, we divide our work into mostly un-entangled threads.
And that's not to mention **gpus**. 

C (with standard libraries) helps us use hardware well in these ways, not only
by providing fine-grained control but also by limiting its abstractions to
those that a compiler can easily reason about and aggressively optimize.[\*]

Our aim for BCC is to maintain these virtues.
  
[\*] By illustrative contrast, `++` in C++ might not necessarily mean
"*increment a variable of one of these known numeric types*".  So a C++
compiler wouldn't be able to optimize something like `++c; ++c;` when `c`'s
type is unknown within the current translation unit.  On another hand (one of
many!), C++'s abstractions could in principle help even non-(game-developers)
focus more of their time on algorithms and pertinent issues --- a bias-variance
tradeoff.

###### TRANSLATION UNITS
  
###### LAMBDAS

Our slogan is that "*function types are just another type*".  We want to treat
that type (and its values) on the same footing as, say, `int`s.  For example,
BCC declarations of function-type variables are not (as in C) implicitly
`extern` --- see the passage "NO EXTERN".  And because function types are just
another type, BCC function names refer not (as in C) to function literals but
to (by-default immutable but if wished mutable) variables of type `function`:

    myfunc : sourcetype->targettype
           = (... somehow specify a function literal here ...) ; 

Supporting this is a stand-alone notation for function literals (see the
passage on "LAMBDAS".)  These expressions can capture ambient state **by
value** (and thus *shallowly* when some of those values are pointers we regard
as "owning" other data of interest).  

###### TYPES

The most important type-related change is this: we **forbid sketchy casts**
such as un-`const`ing or turning `void*` to `my_type*`.  What we lose in this
strictness we partially regain by introducing universal quantification over
types.  Our aim is to allow a programmer justified confidence when they reason
about types.  

To strengthen such reasoning, the default is for all variables (including
functions) to be `const`'ed (and assigned exactly once).  Override this with
`mut`.  More superficially but toward the same end: we scrap C-style
arrays,[\*] promote unions to actual sum types, and make nested function types
actually readable.

And yet we don't introduce anything that smells of inheritance or v-tables:
like many C users, we like **Plain Old Data**.

[\*] not built-in but easy to do is to define one's own array type (allocated
entirely on the heap).

###### SYNTAX

We prefer the uniformity of `term:type` notation to the vibrancy of K&R's
"*declarations look like use cases*" syntax.  So we declare functions like
this:

    foobar : (i32, i32) -> i32;

We use square brackets for four purposes, each of which suggests picking one
out of a discrete collection of choices:
    `accessing array elements`,
    `defining sum types`,
    `(sugar) analyzing sum types`,
    `specializing polymorphic definitions`.
  


### getting started  
  
###### INSTALLATION

###### EXAMPLE SNIPPETS

Hello world:

    var main : []->[]  
         = \() { println("hello, world!"); };

Fibonacci:

    note fib : [i32]->i32;
    
    var main:[]->[] = \() {
        println(fib(8), "should be 21");
    };
    
    /* slow recursive fibonacci */
    fib = \(n:i32)->i32 {
        if (n<=1) { n                  ; }
        else      { fib(n-1) + fib(n-2); }
    };





## organizing bcc projects

We like the .bcc (implementation) vs .bhh (interface) convention:

                                goo.bhh                        
             _____________________/|\______                   
            /                      |       \                  
        moo.bhh     who.bhh        |        \       foo.bhh
           | _________/|\_________ |         \   _____/ \_____  
           |/          |          \|          \ /             \
        moo.bcc     who.bcc     goo.bcc     f_a.bcc         f_b.bcc

However, different (.bcc)s no longer determine different translation units.
That way we avoid the C++ headache of templates and inlines having to be in
interface files rather than implementation files.

###### NO EXTERN

Nothing is considered `extern`.  Function declarations are no longer implicitly
`extern`.  So, just as writing (BCC's analogue of) a top-level statement `int
my_global_i;` not only declares but also allocates memory for `my_global_i`, so
too does writing (BCC's analogue of) `void my_function();` both declare and
allocate.  So `void my_function();  void my_function();` gives a
multiple-definition error.



### declarations and definitions

Both `.bhh`s and `.bcc`s may declare globals and declare types.  `.bcc`s may
additionally define globals and define types.  `.bhhs` may define types
declared in the same file; and they may add their items to namespaces.

All `include`s and `using`s and `namespace`s must occur before other lines of
code.  This is the file's PREAMBLE.



### namespaces

The top-level items (i.e., globals (including functions) and types) that
`who.bhh` declares are available to other `.bhh` and `.bcc`s whose PREAMBLE
contains `include who`.   

If `namespace mooblah` occurs in `who.bhh`'s preamble, then all top-level items
in the file will belong to the namespace `mooblah`.  Namespace names may
contain non-consecutive non-flanking dots e.g. `namespace mooblah.linalg`.  

What it means for a declaration --- say, of `burp` --- in a `who.bhh` to belong
to a namespace --- say, `mooblah` --- is that files that include `who.bhh` will
by default need to write `mooblah.burp` instead of `burp` to refer to the thing
declared.

If `using mooblah` occurs in a file's PREAMBLE, then all declarations and
definitions in the file may (but do not have to) write `burp` or `linalg.gulp`
instead of `mooblah.burp` or `mooblah.linalg.gulp`.





## flow of control



### stack-less control

We have `break`s and `continue`s as in C.  Loops may have `else`s as in Python: they trigger when no `break` is taken:

    if (...) {
        ...
    } else if (...) {
        ...
    } else {
        ...
    }
    
    while (...) {
        ...
        if (...) continue;
        ...
    }
    
    for (...; ...; ...) {
        if (...) break;
    } else {
        ...
    }

No more `do`-`while`s, `goto`s, or `switch`es.
And if there *were* switches, we would have no fallthrough
(so imagine `break;` before each `case`)
and nesting grammar would be as expected (Duff's device would be illegal).

We wanted to make curly braces necessary for these control flow
structures but we didn't for the sake of simple syntax.

No `return`: functions return their last expression.

###### DEFER

As in Go, we may write `defer S` within a function body
to schedule statement `S` for execution when the function exits.
This is good for cleanup code (heap allocations, file handlers, etc).
Note that the `defer`red instructions execute when we exit
the *function*, 
not the `{...}` scope block.



### functions

Last expression is return value.  No `return` keyword.  Makes lambdas cleaner.

###### LAMBDAS

Because function types are just another type, BCC function names refer not (as
in C) to function literals but to (by-default immutable but if wished mutable)
variables of type `function`:

    myfunc : sourcetype->targettype
           = (... somehow specify a function literal here ...) ; 

Supporting this is a stand-alone notation for function literals:

    \(a:i32, b:ptr i32) { *b = a }     -- returns last expression, in this case
                                          an "assignment expression", which has
                                          `unit` type.

More generally, a function goes from a product type to a type.[\*]  Like this:

    \(a:i32, b:i32) { a + b; }               :   [a:i32 , b:i32]->i32
    \(a:i32, b:i32) { c:i32=a+b; c + b; }    :   [a:i32 , b:i32]->i32

    note myfun : [moo:i32, goo:i32]->i32;

    var myfun = \(moo:i32, goo:i32) 
    {
        print(str(moo));    // side effect!
        return moo+5*goo;   // return
    };

[\*]  At first I wanted functions to by default be pure and with immutable
arguments (i.e., to neither cause nor (e.g. by mutable reading global
variables) sense changes in global state and to avoid mutating the input
portion of their new stack frame).  Then I remembered this was supposed to be
like C.  

###### CAPTURES

Nesting of functions and capturing-by-value is allowed (this is crucial for lambdas we feed into `make`/`case`!): 

    note make_adder : [incr:i32] -> [] -> i32;
    var make_adder = \(incr:i32)
    {
        var example_of_local:i32 = incr;
        var adder:[n:i32]->i32 = \(n:i32) { n+incr; } ;
        adder; 
    };

All captures are **by value**; to rule out certain mistakes, we forbid mutating or getting-addresses-of the captured context (but mutating things pointed to can be okay).

Behind the scenes, this defines something like this in C:

    typedef struct {
        int32 m;
    } adder_ctx;

    int32 adder_ptr(context_for_incr* ctx)
    {
        ctx->m = ctx->m + 1;
    }

    typedef struct {
        adder_ctx ctx;
        int32(*ptr)(incr_ctx*);
    } myfunctionoid;

    #define APPLY(fun,...) (((fun).ptr)(&((fun).ctx), __VA_ARGS__))

    ...
    {
        APPLY(fun,x);           // use instead of fun(x)
    }

###### EVALUATION

Each function takes a labeled product as an argument.  We evaluate like so:

    f               :   [aa:A , bb:B]->T  
    f(a,b)          :   T
    f(bb:b,aa:a)  :   T

Available as an alternative for unary functions is a cute dot notation:

    a.b     stands for  b(a)
    a.b.c   stands for  c(b(a))

Unlike in C, ordinary function application binds more tightly than dot:

    a.f(x)     stands for  f(x)(a)
    (a.f)(x)   stands for  f(a)(x)

This way the syntax is consistent with

    
    ().make(...)  stands for    (make(...))(())
    s.case(...)   stands for    (case(...))(s)





## types 



### types and their maps

The primitive types in BCC are:

    void=[], unit=(), bool=<true_maker:unit, false_maker:unit>
    char, i8, i32, i64, f64                                    -- unlike in C, chars MUST be (signed) 8 bits 

We have a literal `star:unit`, also (by sugar discussed below) notated `()`.
And we have literals `true,false:bool` obeying `true = true_maker(())`, `false = false_maker(())`

Note the difference in the meaning of `void`.

If S, T are types (and if P is a product type), then so are:

    P -> T       -- function type
    [s:S , t:T]  -- (labeled) product type
    <s:S , t:T>  -- (labeled) sum type
    mut T        -- mutatable version of T  
    ptr T        -- pointer

Importantly, we have no analogue of `void` pointers.

Actually, the labeled product and sum are the same as their unlabeled versions.  
It's just that when we *define* a type we've got to use those labels so that
the compiler automatically creates the appropriate structure and universal maps. 
These labels are optional other places where types show up (judgements, sizeof,
specialization of a polymorphic thing).  


###### USING PRODUCTS AND SUMS

Binary products and sums enjoy their usual structure and universal maps:

    var p : [mint:i32 , moat:f64] = ().make(
      mint : \() -8  ,
      moat : \() 16.0,
    );
    var nn:i32 = p.fst; 
    var ff:f64 = p.snd; 
    
    var s:<lft:i32 , rht:f64> = lft(640);
    s                     = rht(1.0);
    var fff:f64 = s.case(
      lft : \(n:i32) 2.0 * n,
      rht : \(f:f64) 0.5 * f,
    );

###### USING POINTERS 

TODO

    If `p:ptr T` then
    
    *p : T
    
    If `v:T` then
    
    &v : ptr T
    
    As a special case, if `v:mut T` then
    
    &v : ptr mut T



### polymorphism

TODO

### sugar for algebraic data types

###### CONSTANT FUNCTIONS AS ARGUMENTS TO UNIVERSAL MAPS

(TODO)

We have syntactic sugar for catalyzing terms of product type when the given
`Z->A`, `Z->B` are constant functions.  As constant functions are those that 
factor through the unit type, the dual sugar would probably be for analyzing 
terms of sum type when the given `A->Z`, `B->Z` factor through the void type.
Instead of that perfect dual, 
we syntactic sugar for analyzing terms of sum type when the given `A->Z`, `B->Z` are
constant functions:

    (5 , 6.0 )      stands for          ().make(\() 5, \() 6.0)
    s[5 , 6.0]      stands for          s.case(\() 7, \() 8)
  
###### MUTATORS

(TODO)

We have syntactic sugar for lenses and prisms.

    Recall the beautiful laws:
    
        put : A -> (AxB) -> (AxB)
        fst : (AxB) -> A
    
        fst(put(a)(p)) == a         i.e.        fst . put(a)    ==  const a 
        snd(put(a)(p)) == snd(p)                snd . put(a)    ==  snd 
    
        put : A -> (A+B) -> (A+B)
        lft : A -> (A+B)
    
        put(a)(lft(a_)) == a        i.e.        put(a) . lft    ==  const a 
        put(a)(rht(b_)) == rht(b_)              put(a) . rht    ==  rht 
    
    When `s:<lft:i32, rht:f64>` and `m:[fst:i32, snd:f64]` then
    
        s!lft(a)        stands for      s = s.case(\() (lft(a)), rht)
        m!fst(a)        stands for      m = m.make(\() a, snd)





## syntax



### grammar

We notate our grammar as follows:

    *A                                  for kleene star
    ?A                                  for A -or- emptystring
    A B                                 for concatenation
    A | B                               for disjunction

    "cow"                               for token literals
    [identifier]                        for label literals

    -- moo                              for comments
    
###### TOP LEVEL GRAMMAR

A `.bhh` or `.bcc` file consists of a preamble followed by top-level items.
By **item** we mean a declaration and/or allocation and/or assignment.
At the top-level, values given to variables must be statically know(n/able)
(for now, let's say they must be LITERALS).

    FILE = *HEAD *ITEM
    
    HEAD = "import"     DOTS
         | "using"      DOTS
         | "namespace"  DOTS
    
    DOTS = [identifier] *("." [identifier])
    
    ITEM = DECL
         | DEFN_TYPE
         | DEFN_VARI
    
    -- merely note that the identifier identifies
    --   a type or
    --   a variable of the specified type
    DECL      = "note" [identifier] ":" ("type" | QTYP) ";" 
    
    -- define the type
    DEFN_TYPE = "type" [identifier] "=" QTYP ";"
    
    -- also allocate memory for it (and maybe set its initial value)
    DEFN_VARI = "var" JUDG ?("=" EXPR) ";"

###### TYPE GRAMMAR

We allow universally quantified types.

    JUDG = [identifier] ":" QTYP
    
    QTYP = "forall" [identifier] TYPE
         | TYPE
    
    TYPE!= TYP1 ?("->" TYPE)            -- function
    
    TYP1!= "(" TYPE ")"
         | "mut" TYP1               
         | "ptr" TYP1               
         | "[" ?(JUDG *("," JUDG)) "]"  -- product
         | "<" ?(JUDG *("|" JUDG)) ">"  -- sum
         | [identifier]                 -- incl. void, unit, bool, c8, i32, f64
 
###### EXPRESSION GRAMMAR

Operators, `make`, and `case` de-sugar to functions (tho `make` and `case` are
variadic & polymorphic with instances specially made by the compiler).  So we
ignore them in our expression grammar: 
   
    EXPR = EXP1 *(( "&&" | "||" ) EXP1)
    
    EXP1!= EXP2 *(( "==" | "!=" | "<=" | ">=" | "<" | ">" ) EXP2)
    
    EXP2!= EXP3 *(( "+" | "-" ) EXP3)
    
    EXP3!= EXP4 *(( "*" | "/" ) EXP4)
    
    EXP4!= EXP5 ?("." EXP5)                         -- evaluation 
    
    EXP5!= EXP6 *( "(" ?(EXPR *("," EXPR)) ")"      -- evaluation
                  |"[" EXPR "]" )                   -- array index
    
    EXP6!= "(" EXPR ")"
         | [identifier]
         | LTRL        
    
    LTRL!= LAMB                                     -- function literals 
         | [string-literal]
         | [number-literal]
         | [char-literal]
         | [bool-literal]
    
    LAMB = "\" "(" ?(JUDG *("," JUDG)) ")" STMT     -- abstraction

###### STATEMENT GRAMMAR
       
    STMT = "{" *STMT "}"
         | "break" ";"
         | "continue" ";"
         | "if"    "(" EXPR ")" STMT ?("else" STMT)
         | "while" "(" EXPR ")" STMT ?("else" STMT)
         | "for"   "(" STMT "," EXPR "," STMT ")" STMT ?("else" STMT)
         | "defer" STMT  
         | ITEM
         | EXPR ?("=" EXPR) ";"  -- assignment or expression (e.g. func. call) 

   

### tokenization

The above grammar assumes a list of string-type tokens.  We have **NO** text
C-style text pre-processor.

###### COMMENTS

(TODO: implement K&R comments)

We allow comments in the styles of K&R, Bjarne, and Haskell.
Those three styles are (respectively) for:
documentation of interfaces and global structure,
commented-out code,
and local documentation of implementation tricks. 

    /*  hello!  this sort of comment
	 *  is intended for prose, especially to  
	 *  explain interfaces                    
     */
	 
	// comment out code this way
	
	-- document tricky lines of implementation this way

Only 99.98% tongue in cheek: any `.bcc` and `.bhh` file that violates these
intentions has **undefined behavior**.

### Summary of Reserved Words

###### keywords 

    --- FLOW OF CONTROL ---         
    if, while, for;
    else, break, continue;              -- loops may have `else`s a la Python     
    defer;                              -- as in Go
    
    --- DECLARATIONS ---
    note, type, var;
        
    --- POLYMORPHISM ---   
    forall;
    
    --- HANDLING MEMORY ---
    mut, arr, ptr;
    take, free;                         -- `take` is our `malloc`
    
    --- ORGANIZING PROJECTS ---
    import;                             -- no longer mere pre-processing!
    using, namespace;

###### special identifiers 

    --- ALGEBRAIC TYPES ---
    void, unit, bool, char, i32, f64;  
    star, true, false;
    make, case;                         -- auto generated for each ADT 
    
    --- COMPILER-SPECIAL FUNCTIONS ---
    str, repr;                          -- auto generated for each POD type
    main;

[\*\*\*] answers to four questions far above: **No**, **No**, **No**, and **No**!


