-------- MODULE Harmony --------
EXTENDS Integers, FiniteSets, Bags, Sequences, TLC

\* This is the Harmony TLA+ module.  All the Harmony virtual machine
\* instructions are defined below.  Mostly, if the Harmony VM instruction
\* is called X, then its definition below is under the name OpX.  There
\* are some cases where there is an extension.  For example, the Store
\* instruction has two versions: OpStore and OpStoreInd, depending on
\* whether the variable is directly specified or its address is on the
\* stack of the current thread.

\* There are three variables:
\*  active: a set of the currently active contexts
\*  ctxbag: a multiset of all contexts
\*  shared: a map of variable names to Harmony values
\*
\* A context is the state of a thread.  A context may be atomic or not.
\* There can be at most one atomic context.  If there is an atomic context,
\* it and only it is in the active set.  If there is no atomic context,
\* then the active set is the domain of the ctxbag.
VARIABLE active, ctxbag, shared
allvars == << active, ctxbag, shared >>

\* The variable "shared" is a Harmony dict type
SharedInvariant == shared.ctype = "dict"

TypeInvariant == SharedInvariant

\* Harmony values are represented by a ctype tag that contains the name of
\* their Harmony type and a cval that contains their TLA+ representation
HBool(x)    == [ ctype |-> "bool",    cval |-> x ]
HInt(x)     == [ ctype |-> "int",     cval |-> x ]
HStr(x)     == [ ctype |-> "str",     cval |-> x ]
HPc(x)      == [ ctype |-> "pc",      cval |-> x ]
HList(x)    == [ ctype |-> "list",    cval |-> x ]
HDict(x)    == [ ctype |-> "dict",    cval |-> x ]
HSet(x)     == [ ctype |-> "set",     cval |-> x ]
HAddress(x) == [ ctype |-> "address", cval |-> x ]
HContext(x) == [ ctype |-> "context", cval |-> x ]

\* Defining the Harmony constant (), which is an empty dict
EmptyFunc == [x \in {} |-> TRUE]
EmptyDict == HDict(EmptyFunc)

\* Flatten a sequence of sequences
Flatten(seq) ==
    LET F[i \in 0..Len(seq)] == IF i = 0 THEN <<>> ELSE F[i-1] \o seq[i]
    IN F[Len(seq)]

\* Harmony values are ordered first by their type
HRank(x) ==
    CASE x.ctype = "bool"    -> 0
    []   x.ctype = "int"     -> 1
    []   x.ctype = "str"     -> 2
    []   x.ctype = "pc"      -> 3
    []   x.ctype = "list"    -> 4
    []   x.ctype = "dict"    -> 5
    []   x.ctype = "set"     -> 6
    []   x.ctype = "address" -> 7
    []   x.ctype = "context" -> 8

\* TLA+ does not seem to have a direct way to compare characters in a
\* string, so...  Note that this only contains the printable ASCII
\* characters and excludes the backquote and double quote characters
\* as well as the backslash
CRank(c) ==
    CASE c=" "->32[]c="!"->33[]c="#"->35[]c="$"->36[]c="%"->37[]c="&"->38
    []c="'"->39[]c="("->40[]c=")"->41[]c="*"->42[]c="+"->43[]c=","->44
    []c="-"->45[]c="."->46[]c="/"->47[]c="0"->48[]c="1"->49[]c="2"->50
    []c="3"->51[]c="4"->52[]c="5"->53[]c="6"->54[]c="7"->55[]c="8"->56
    []c="9"->57[]c=":"->58[]c=";"->59[]c="<"->60[]c="="->61[]c=">"->62
    []c="?"->63[]c="@"->64[]c="A"->65[]c="B"->66[]c="C"->67[]c="D"->68
    []c="E"->69[]c="F"->70[]c="G"->71[]c="H"->72[]c="I"->73[]c="J"->74
    []c="K"->75[]c="L"->76[]c="M"->77[]c="N"->78[]c="O"->79[]c="P"->80
    []c="Q"->81[]c="R"->82[]c="S"->83[]c="T"->84[]c="U"->85[]c="V"->86
    []c="W"->87[]c="X"->88[]c="Y"->89[]c="Z"->90[]c="["->91[]c="]"->93
    []c="^"->94[]c="_"->95[]c="a"->97[]c="b"->98[]c="c"->99
    []c="d"->100[]c="e"->101[]c="f"->102[]c="g"->103[]c="h"->104
    []c="i"->105[]c="j"->106[]c="k"->107[]c="l"->108[]c="m"->109
    []c="n"->110[]c="o"->111[]c="p"->112[]c="q"->113[]c="r"->114
    []c="s"->115[]c="t"->116[]c="u"->117[]c="v"->118[]c="w"->119
    []c="x"->120[]c="y"->121[]c="z"->122[]c="{"->123[]c="|"->124
    []c="}"->125[]c="~"->126

\* Comparing two TLA+ strings
RECURSIVE StrCmp(_,_)
StrCmp(x, y) ==
    IF x = y
    THEN
        0
    ELSE
        CASE Len(x) = 0 ->  1
        []   Len(y) = 0 -> -1
        [] OTHER ->
            LET rx == CRank(Head(x))
                ry == CRank(Head(y))
            IN
                CASE rx < ry -> -1
                []   rx > ry ->  1
                [] OTHER -> StrCmp(Tail(x), Tail(y))

\* Setting up to compare two arbitrary Harmony values
RECURSIVE SeqCmp(_,_)
RECURSIVE HCmp(_,_)
RECURSIVE HSort(_)
RECURSIVE DictSeq(_)

\* Given a Harmony dictionary, return a sequence of its key, value
\* pairs sorted by the corresponding key.
DictSeq(dict) ==
    LET dom == HSort(DOMAIN dict)
    IN [ x \in 1..Len(dom) |-> << dom[x], dict[dom[x]] >> ]

\* Two dictionaries are ordered by their sequence of (key, value) pairs
\* Equivalently, we can flatten the sequence of (key, value) pairs first
\* into a single sequence of alternating keys and values.  Then we
\* compare the two sequences.
DictCmp(x, y) == SeqCmp(Flatten(DictSeq(x)), Flatten(DictSeq(y)))

\* Lexicographically compare two sequences of Harmony values
SeqCmp(x, y) ==
    IF x = y
    THEN
        0
    ELSE
        CASE Len(x) = 0 ->  1
        []   Len(y) = 0 -> -1
        [] OTHER ->
            LET c == HCmp(Head(x), Head(y))
            IN
                CASE c < 0 -> -1
                []   c > 0 ->  1
                [] OTHER -> SeqCmp(Tail(x), Tail(y))

\* Compare two contexts.  Essentially done lexicographically
CtxCmp(x, y) ==
    IF x = y THEN 0
    ELSE IF x.pc # y.pc THEN x.pc - y.pc
    ELSE IF x.apc # y.apc THEN x.apc - y.apc
    ELSE IF x.atomic # y.atomic THEN x.atomic - y.atomic
    ELSE IF x.vs # y.vs THEN DictCmp(x.vs, y.vs)
    ELSE IF x.stack # y.stack THEN SeqCmp(x.stack.cval, y.stack.cal)
    ELSE IF x.interruptLevel # y.interruptLevel THEN
             IF x.interruptLevel THEN -1 ELSE 1
    ELSE IF x.trap # y.trap THEN SeqCmp(x.trap, y.trap)
    ELSE IF x.readonly # y.readonly THEN x.readonly - y.readonly
    ELSE Assert(FALSE, "CtxCmp: this should not happen")

\* Compare two Harmony values as specified in the book
\* Return negative if x < y, 0 if x = y, and positive if x > y
HCmp(x, y) ==
    IF x = y
    THEN
        0
    ELSE
        IF x.ctype = y.ctype
        THEN 
            CASE x.ctype = "bool"    -> IF x.cval THEN 1 ELSE -1
            []   x.ctype = "int"     -> x.cval - y.cval
            []   x.ctype = "str"     -> StrCmp(x.cval, y.cval)
            []   x.ctype = "pc"      -> x.cval - y.cval
            []   x.ctype = "list"    -> SeqCmp(x.cval, y.cval)
            []   x.ctype = "set"     -> SeqCmp(HSort(x.cval), HSort(y.cval))
            []   x.ctype = "dict"    -> DictCmp(x.cval, y.cval)
            []   x.ctype = "address" -> SeqCmp(x.cval, y.cval)
            []   x.ctype = "context" -> CtxCmp(x.cval, y.cval)
        ELSE
            HRank(x) - HRank(y)

\* The minimum and maximum Harmony value in a set
HMin(s) == CHOOSE x \in s: \A y \in s: HCmp(x, y) <= 0
HMax(s) == CHOOSE x \in s: \A y \in s: HCmp(x, y) >= 0

\* Sort a set of Harmony values into a sequence
HSort(s) ==
    IF s = {}
    THEN
        <<>>
    ELSE
        LET min == HMin(s) IN << min >> \o HSort(s \ {min})

\* This is to represent "variable name hierarchies" used in expressions
\* such as (x, (y, z)) = (1, (2, 3))
VName(name) == [ vtype |-> "var", vname |-> name ]
VList(list) == [ vtype |-> "tup", vlist |-> list ]

\* An address has a function and a list of argument, each of which are
\* Harmony values
Address(f, a) == HAddress([ func |-> f, args |-> a ])

\* Defining the Harmony constant None
None      == Address(EmptyFunc, <<>>)

\* Representation of a context (the state of a thread).  It includes
\* the following fields:
\*  pc:     the program counter (location in the code)
\*  apc:    if atomic, the location in the code where the thread became atomic
\*  atomic: a counter: 0 means not atomic, larger than 0 is atomic
\*  vs:     a Harmony dictionary containing the variables of this thread
\*  stack:  a sequence of Harmony values
\*  interruptLevel: false if enabled, true if disabled
\*  trap:   either <<>> or a tuple containing the trap method and argument
\*  readonly: larger than 0 means not allowed to modify shared state
Context(pc, atomic, vs, stack, interruptLevel, trap, readonly) ==
    [
        pc             |-> pc,
        apc            |-> pc,
        atomic         |-> atomic,
        vs             |-> vs,
        stack          |-> stack,
        interruptLevel |-> interruptLevel,
        trap           |-> trap,
        readonly       |-> readonly
    ]

\* An initial context of a thread.  arg is the argument given when the thread
\* thread was spawned.  "process" is used by the OpReturn operator.
InitContext(pc, atomic, arg) ==
    Context(pc, atomic, EmptyDict, << arg, "process" >>, FALSE, <<>>, 0)

\* Update the given map with a new key -> value mapping
UpdateMap(map, key, value) ==
    [ x \in (DOMAIN map) \union {key} |-> IF x = key THEN value ELSE map[x] ]

\* Update a Harmony dictionary with a new key -> value mapping
UpdateDict(dict, key, value) ==
    HDict(UpdateMap(dict.cval, key, value))

\* The initial state of the Harmony module consists of a single thread starting
\* at pc = 0 and an empty set of shared variables
Init ==
    LET ctx == InitContext(0, 1, EmptyDict)
    IN /\ active = { ctx }
       /\ ctxbag = SetToBag(active)
       /\ shared = EmptyDict

\* The state of the current thread goes from 'self' to 'next'.  Update
\* both the context bag and the active set
UpdateContext(self, next) ==
    /\ active' = (active \ { self }) \union { next }
    /\ ctxbag' = (ctxbag (-) SetToBag({self})) (+) SetToBag({next})

\* Remove context from the active set and context bag.  Make all contexts
\* in the context bag active
RemoveContext(self) ==
    /\ ctxbag' = ctxbag (-) SetToBag({self})
    /\ active' = BagToSet(ctxbag')

\* A Harmony address is essentially a sequence of Harmony values
\* These compute the head (the first element) and the remaining tail
AddrHead(addr) == Head(addr.cval)
AddrTail(addr) == HAddress(Tail(addr.cval))

\* Given a non-negative integer, return a sequence of bits starting
\* with least significant one
RECURSIVE Int2BitsHelp(_)
Int2BitsHelp(x) ==
    IF x = 0
    THEN <<>>
    ELSE <<x % 2 = 1>> \o Int2BitsHelp(x \div 2)

\* Convert an integer to a bit sequence, lsb first. neg indicates if the
\* value is negative.
Int2Bits(x) ==
    IF x < 0
    THEN [ neg |-> TRUE,  bits |-> Int2BitsHelp(-x-1) ]
    ELSE [ neg |-> FALSE, bits |-> Int2BitsHelp(x)    ]

\* Convert a bit sequence (lsb first) to a non-negative integer
RECURSIVE Bits2IntHelp(_)
Bits2IntHelp(x) == 
    IF x = <<>>
    THEN 0
    ELSE (IF Head(x) THEN 1 ELSE 0) + 2 * Bits2IntHelp(Tail(x))

\* Convert a bit sequence to an integer.
Bits2Int(b) ==
    IF b.neg
    THEN -Bits2IntHelp(b.bits) - 1
    ELSE Bits2IntHelp(b.bits)

\* Compute the bitwise negation of a bit sequence
BitsNegate(b) == [ neg |-> ~b.neg, bits |-> b.bits ]

\* Compute b >> n
BitsShiftRight(b, n) ==
    IF n >= Len(b.bits)
    THEN [ neg |-> b.neg, bits |-> <<>> ]
    ELSE [ neg |-> b.neg, bits |-> SubSeq(b.bits, n + 1, Len(b.bits)) ]

\* Compute b << n
BitsShiftLeft(b, n) ==
    [ neg |-> b.neg, bits |-> [ x \in 1..n |-> b.neg ] \o b.bits ]

\* Helper functions for BitsXOR
RECURSIVE BitsXORhelp(_,_)
BitsXORhelp(x, y) ==
    CASE x = <<>> -> y
    []   y = <<>> -> x
    [] OTHER -> << Head(x) # Head (y) >> \o BitsXORhelp(Tail(x), Tail(y))

\* Compute x XOR y
BitsXOR(x, y) ==
    [ neg |-> x.neg # y.neg, bits |-> BitsXORhelp(x.bits, y.bits) ]

\* Helper function for BitsOr
RECURSIVE BitsOrHelp(_,_)
BitsOrHelp(x, y) ==
    CASE x.bits = <<>> -> IF x.neg THEN <<>> ELSE y.bits
    []   y.bits = <<>> -> IF y.neg THEN <<>> ELSE x.bits
    [] OTHER -> << (x.neg \/ y.neg) #
            ((Head(x.bits) # x.neg) \/ (Head(y.bits) # y.neg)) >> \o
            BitsOrHelp(
                [ neg |-> x.neg, bits |-> Tail(x.bits) ],
                [ neg |-> y.neg, bits |-> Tail(y.bits) ])

\* Compute x OR y
BitsOr(x, y) ==
    [ neg  |-> x.neg \/ y.neg, bits |-> BitsOrHelp(x, y) ]

\* Helper function for BitsAnd
RECURSIVE BitsAndHelp(_,_)
BitsAndHelp(x, y) ==
    CASE x.bits = <<>> -> IF x.neg THEN y.bits ELSE <<>>
    []   y.bits = <<>> -> IF y.neg THEN x.bits ELSE <<>>
    [] OTHER -> << (x.neg /\ y.neg) #
            ((Head(x.bits) # x.neg) /\ (Head(y.bits) # y.neg)) >> \o
            BitsAndHelp(
                [ neg |-> x.neg, bits |-> Tail(x.bits) ],
                [ neg |-> y.neg, bits |-> Tail(y.bits) ])

\* Compute x AND y
BitsAnd(x, y) ==
    [ neg  |-> x.neg /\ y.neg, bits |-> BitsAndHelp(x, y) ]

\* This is to implement del !addr, where addr is a Harmony address
\* (a sequence of Harmony values representing a path in dir, a directory.
\* It is a recursive operator that returns the new directory.
RECURSIVE RemoveDirAddr(_, _)
RemoveDirAddr(dir, addr) ==
    LET next == AddrHead(addr) IN
        CASE dir.ctype = "dict" ->
            HDict(
                IF Len(addr.cval) = 1
                THEN
                    [ x \in (DOMAIN dir.cval) \ {next} |-> dir.cval[x] ]
                ELSE
                    [ x \in (DOMAIN dir.cval) |->
                        IF x = next
                        THEN
                            RemoveDirAddr(dir.cval[x], AddrTail(addr))
                        ELSE
                            dir.cval[x]
                    ]
            )
        [] dir.ctype = "list" ->
            HList(
                CASE next.ctype = "int" /\ 0 <= next.cval /\ next.cval < Len(dir.cval) ->
                    IF Len(addr.cval) = 1
                    THEN
                        SubSeq(dir.cval, 1, next.cval) \o
                        SubSeq(dir.cval, next.cval + 2, Len(dir.cval))
                    ELSE
                        [ x \in (DOMAIN dir.cval) |->
                            IF x = next.cval + 1
                            THEN
                                RemoveDirAddr(dir.cval[x], AddrTail(addr))
                            ELSE
                                dir.cval[x]
                        ]
            )

\* This is to implement !addr = value, where addr is a Harmony address
\* (a sequence of Harmony values representing a path in dir, a directory
\* (tree where the internal nodes are dictionaries or lists), and value
\* is the new value.  It is a recursive operator that returns the new directory.
RECURSIVE UpdateDirAddr(_, _, _)
UpdateDirAddr(dir, addr, value) ==
    IF addr = <<>>
    THEN
        value
    ELSE
        LET next == Head(addr)
        IN
            CASE dir.ctype = "dict" ->
                HDict(
                    [ x \in (DOMAIN dir.cval) \union {next} |->
                        IF x = next
                        THEN
                            UpdateDirAddr(dir.cval[x], Tail(addr), value)
                        ELSE
                            dir.cval[x]
                    ]
                )
            [] dir.ctype = "list" ->
                HList(
                    CASE next.ctype = "int" /\ 0 <= next.cval /\ next.cval <= Len(dir.cval) ->
                        [ x \in (DOMAIN dir.cval) \union {next.cval + 1} |->
                            IF x = next.cval + 1
                            THEN
                                UpdateDirAddr(dir.cval[x], Tail(addr), value)
                            ELSE
                                dir.cval[x]
                        ]
                )

\* This is to compute the value of !addr in dir, which is a simple
\* recursive function
RECURSIVE LoadDirAddr(_, _)
LoadDirAddr(dir, addr) ==
    IF addr.cval = <<>>
    THEN
        dir
    ELSE
        LET next == AddrHead(addr)
        IN
            CASE dir.ctype = "dict" ->
                LoadDirAddr(dir.cval[next], AddrTail(addr))
            [] dir.ctype = "list" ->
                CASE next.ctype = "int" ->
                    LoadDirAddr(dir.cval[next.cval + 1], AddrTail(addr))

\* This is a helper operator for UpdateVars.
\* Harmony allows statements of the form: x,(y,z) = v.  For example,
\* if v = (1, (2, 3)), then this assigns 1 to x, 2 to y, and 3 to z.
\* For this operator, args is a tree describing the lefthand side,
\* while value is the righthand side of the equation above.  The
\* operator creates a sequence of (variable, value) records.  In the
\* example, the sequence would be << (x,1), (y,2), (z,3) >> essentially.
RECURSIVE CollectVars(_, _)
CollectVars(args, value) ==
    IF args.vtype = "var"
    THEN << [ var |-> HStr(args.vname), val |-> value ] >>
    ELSE
        Flatten([ i \in DOMAIN args.vlist |->
            CollectVars(args.vlist[i], value.cval[i])
        ])

\* Another helper operator for UpdateVars.  dict is a Harmony dictionary,
\* cv is a sequence as returned by CollectVars, and index is an index into
\* this sequence.  Fold returns an updated dictionary.
RECURSIVE Fold(_, _, _)
Fold(dict, cv, index) ==
    IF index = 0
    THEN dict
    ELSE
        LET elt == cv[index]
        IN Fold(UpdateDict(dict, elt.var, elt.val), cv, index - 1)

\* As explained in CollectVars, args is a tree of variable names that
\* appears on the lefthandside of a Harmony assignment operation.  value
\* is the value of the righthandside.  The vs are the variables of the
\* context that need to be updated.
UpdateVars(vs, args, value) ==
    LET cv == CollectVars(args, value)
    IN Fold(vs, cv, Len(cv))

\* A no-op
OpContinue(self) ==
    /\ UpdateContext(self, [self EXCEPT !.pc = @ + 1])
    /\ UNCHANGED shared

\* Pop the new interrupt level and push the old one
OpSetIntLevel(self) ==
    LET nl   == Head(self.stack)
        next == [self EXCEPT !.pc = @ + 1, !.interruptLevel = nl.cval,
            !.stack = << HBool(self.interruptLevel) >> \o Tail(@)]
    IN
        /\ nl.ctype = "bool"
        /\ UpdateContext(self, next)
        /\ UNCHANGED shared

\* Increment the readonly counter (counter because of nesting)
OpReadonlyInc(self) ==
    LET next == [self EXCEPT !.pc = @ + 1, !.readonly = @ + 1]
    IN
        /\ UpdateContext(self, next)
        /\ UNCHANGED shared

\* Decrement the readonly counter
OpReadonlyDec(self) ==
    LET next == [self EXCEPT !.pc = @ + 1, !.readonly = @ - 1]
    IN
        /\ self.readonly > 0
        /\ UpdateContext(self, next)
        /\ UNCHANGED shared

\* This is used temporarily for Harmony VM instructions that have not yet
\* been implemented
Skip(self, what) == OpContinue(self)

\* First instruction of every method.  Saves the current variables on the stack,
\* Assigns the top of the stack to args (see UpdateVars) and initializes variable
\* result to None.
OpFrame(self, name, args) ==
    LET next == [
        self EXCEPT !.pc = @ + 1,
        !.stack = << self.vs >> \o Tail(@),
        !.vs = UpdateVars(EmptyDict, args, Head(self.stack))
    ]
    IN
        /\ UpdateContext(self, next)
        /\ UNCHANGED shared

\* Remove one element from the stack
OpPop(self) ==
    LET next == [self EXCEPT !.pc = @ + 1, !.stack = Tail(@)]
    IN
        /\ UpdateContext(self, next)
        /\ UNCHANGED shared

\* Remove key from the given map
DictCut(dict, key) == [ x \in (DOMAIN dict) \ { key } |-> dict[x] ]

\* v contains the name of a local variable.  v can be a "variable tree"
\* (see UpdateVars).  The stack contains an index on top with an iterable
\* value below it.  OpCut should assign the value at that index to v.
\* If the index is valid, OpCut should also increment the index on top
\* of the stack (leaving the iterable value) and push True.  If not,
\* OpCut should pop both the index and the iterable value and push False.
OpCut(self, v) ==
    LET index    == self.stack[1]
        iterable == self.stack[2]
    IN
        /\ CASE iterable.ctype = "list" ->
                IF index.cval < Len(iterable.cval)
                THEN
                    LET next == [self EXCEPT !.pc = @ + 1, !.stack = << HBool(TRUE), HInt(index.cval + 1) >> \o Tail(@), !.vs = UpdateVars(@, v, iterable.cval[index.cval + 1])]
                    IN UpdateContext(self, next)
                ELSE
                    LET next == [self EXCEPT !.pc = @ + 1, !.stack = << HBool(FALSE) >> \o Tail(Tail(@))]
                    IN UpdateContext(self, next)
            [] iterable.ctype = "str" ->
                IF index.cval < Len(iterable.cval)
                THEN
                    LET next == [self EXCEPT !.pc = @ + 1, !.stack = << HBool(TRUE), HInt(index.cval + 1) >> \o Tail(@), !.vs = UpdateVars(@, v, HStr(SubSeq(iterable.cval, index.cval + 1, index.cval + 1)))]
                    IN UpdateContext(self, next)
                ELSE
                    LET next == [self EXCEPT !.pc = @ + 1, !.stack = << HBool(FALSE) >> \o Tail(Tail(@))]
                    IN UpdateContext(self, next)
            [] iterable.ctype = "set" ->
                IF index.cval < Cardinality(iterable.cval)
                THEN
                    LET next == [self EXCEPT !.pc = @ + 1, !.stack = << HBool(TRUE), HInt(index.cval + 1) >> \o Tail(@), !.vs = UpdateVars(@, v, HSort(iterable.cval)[index.cval + 1])]
                    IN UpdateContext(self, next)
                ELSE
                    LET next == [self EXCEPT !.pc = @ + 1, !.stack = << HBool(FALSE) >> \o Tail(Tail(@))]
                    IN UpdateContext(self, next)
            [] iterable.ctype = "dict" ->
                IF index.cval < Cardinality(DOMAIN iterable.cval)
                THEN
                    LET items == DictSeq(iterable.cval)
                        next  == [self EXCEPT !.pc = @ + 1, !.stack = << HBool(TRUE), HInt(index.cval + 1) >> \o Tail(@), !.vs = UpdateVars(@, v, items[index.cval + 1][1])]
                    IN UpdateContext(self, next)
                ELSE
                    LET next == [self EXCEPT !.pc = @ + 1, !.stack = << HBool(FALSE) >> \o Tail(Tail(@))]
                    IN UpdateContext(self, next)
        /\ UNCHANGED shared

\* Much like OpCut, but there are two variables that must be assigned: the key k
\* and the value v.
OpCut2(self, v, k) ==
    LET index    == self.stack[1]
        iterable == self.stack[2]
    IN
        /\ CASE iterable.ctype = "list" ->
                IF index.cval < Len(iterable.cval)
                THEN
                    LET intm == UpdateVars(self.vs, k, index)
                        next == [self EXCEPT !.pc = @ + 1, !.stack = << HBool(TRUE), HInt(index.cval + 1) >> \o Tail(@), !.vs = UpdateVars(intm, v, iterable.cval[index.cval + 1])]
                    IN UpdateContext(self, next)
                ELSE
                    LET next == [self EXCEPT !.pc = @ + 1, !.stack = << HBool(FALSE) >> \o Tail(Tail(@))]
                    IN UpdateContext(self, next)
            [] iterable.ctype = "str" ->
                IF index.cval < Len(iterable.cval)
                THEN
                    LET intm == UpdateVars(self.vs, k, index)
                        next == [self EXCEPT !.pc = @ + 1, !.stack = << HBool(TRUE), HInt(index.cval + 1) >> \o Tail(@), !.vs = UpdateVars(intm, v, HStr(SubSeq(iterable.cval, index.cval + 1, index.cval + 1)))]
                    IN UpdateContext(self, next)
                ELSE
                    LET next == [self EXCEPT !.pc = @ + 1, !.stack = << HBool(FALSE) >> \o Tail(Tail(@))]
                    IN UpdateContext(self, next)
            [] iterable.ctype = "set" ->
                IF index.cval < Cardinality(iterable.cval)
                THEN
                    LET intm == UpdateVars(self.vs, k, index)
                        next == [self EXCEPT !.pc = @ + 1, !.stack = << HBool(TRUE), HInt(index.cval + 1) >> \o Tail(@), !.vs = UpdateVars(intm, v, HSort(iterable.cval)[index.cval + 1])]
                    IN UpdateContext(self, next)
                ELSE
                    LET next == [self EXCEPT !.pc = @ + 1, !.stack = << HBool(FALSE) >> \o Tail(Tail(@))]
                    IN UpdateContext(self, next)
            [] iterable.ctype = "dict" ->
                IF index.cval < Cardinality(DOMAIN iterable.cval)
                THEN
                    LET items == DictSeq(iterable.cval)
                        intm  == UpdateVars(self.vs, k, items[index.cval + 1][1])
                        next  == [self EXCEPT !.pc = @ + 1, !.stack = << HBool(TRUE), HInt(index.cval + 1) >> \o Tail(@), !.vs = UpdateVars(intm, v, items[index.cval + 1][2])]
                    IN UpdateContext(self, next)
                ELSE
                    LET next == [self EXCEPT !.pc = @ + 1, !.stack = << HBool(FALSE) >> \o Tail(Tail(@))]
                    IN UpdateContext(self, next)
        /\ UNCHANGED shared

\* Delete the shared variable pointed to be v.  v is a sequence of Harmony
\* values acting as an address (path in hierarchy of dicts)
OpDel(self, v) ==
    /\ Assert(self.readonly = 0, "Del in readonly mode")
    /\ UpdateContext(self, [self EXCEPT !.pc = @ + 1])
    /\ shared' = RemoveDirAddr(shared, HAddress(v))

\* Delete the shared variable whose address is pushed on the stack
OpDelInd(self) ==
    LET addr == Head(self.stack)
        next == [self EXCEPT !.pc = @ + 1, !.stack = Tail(@)]
    IN
        /\ UpdateContext(self, next)
        /\ shared' = RemoveDirAddr(shared, addr)

\* Delete the given local variable
OpDelVar(self, v) ==
    LET next == [self EXCEPT !.pc = @ + 1,
        !.vs = HDict([ x \in (DOMAIN @.cval) \ { HStr(v.vname) } |-> @.cval[x] ]) ]
    IN
        /\ UpdateContext(self, next)
        /\ UNCHANGED shared

\* Delete the local variable whose address is pushed on the stack
OpDelVarInd(self) ==
    LET addr == Head(self.stack)
        next == [self EXCEPT !.pc = @ + 1, !.stack = Tail(@),
                                    !.vs = RemoveDirAddr(@, addr)]
    IN
        /\ UpdateContext(self, next)
        /\ UNCHANGED shared

\* Increment the given local variable
OpIncVar(self, v) ==
    LET var  == HStr(v.vname)
        next == [self EXCEPT !.pc = @ + 1,
                    !.vs = UpdateDict(@, var, HInt(@.cval[var].cval + 1)) ]
    IN
        /\ UpdateContext(self, next)
        /\ UNCHANGED shared

\* Assign the top of the stack to a local variable
OpStoreVar(self, v) ==
    LET next == [self EXCEPT !.pc = @ + 1, !.stack = Tail(@), !.vs = UpdateVars(@, v, Head(self.stack))]
    IN
        /\ UpdateContext(self, next)
        /\ UNCHANGED shared

\* Push the value of the given variable onto the stack
OpLoadVar(self, v) ==
    LET next == [self EXCEPT !.pc = @ + 1, !.stack = << self.vs.cval[HStr(v.vname)] >> \o @ ]
    IN
        /\ UpdateContext(self, next)
        /\ UNCHANGED shared

\* Increment the atomic counter for this thread.  If it was 0, boot every other
\* context out of the set of active contexts.
OpAtomicInc(self) ==
    IF self.atomic = 0
    THEN
        LET next == [self EXCEPT !.pc = @ + 1, !.apc = self.pc, !.atomic = 1]
        IN
            /\ active' = { next }
            /\ ctxbag' = (ctxbag (-) SetToBag({self})) (+) SetToBag({next})
            /\ UNCHANGED shared
    ELSE
        LET next == [self EXCEPT !.pc = @ + 1, !.atomic = @ + 1]
        IN
            /\ UpdateContext(self, next)
            /\ UNCHANGED shared

\* Decrement the atomic counter.  If it becomes 0, let all other contexts
\* back into the active set.
OpAtomicDec(self) ==
    IF self.atomic = 1
    THEN
        LET next == [self EXCEPT !.pc = @ + 1, !.apc = 0, !.atomic = 0]
        IN
            /\ ctxbag' = (ctxbag (-) SetToBag({self})) (+) SetToBag({next})
            /\ active' = DOMAIN ctxbag'
            /\ UNCHANGED shared
    ELSE
        LET next == [self EXCEPT !.pc = @ + 1, !.atomic = @ - 1]
        IN
            /\ UpdateContext(self, next)
            /\ UNCHANGED shared

\* Pop the top of stack and check if it is True.  If not, stop and print the
\* message.
OpAssert(self, msg) ==
    LET cond == Head(self.stack)
        next == [self EXCEPT !.pc = @ + 1, !.stack = Tail(@)]
    IN
        /\ cond.ctype = "bool"
        /\ Assert(cond.cval, msg)
        /\ UpdateContext(self, next)
        /\ UNCHANGED shared

\* The top of the stack contains an expression to be printed along with the
\* message if the next element on the stack is FALSE.
OpAssert2(self, msg) ==
    LET data == self.stack[1]
        cond == self.stack[2]
        next == [self EXCEPT !.pc = @ + 1, !.stack = Tail(Tail(@))]
    IN
        /\ cond.ctype = "bool"
        /\ Assert(cond.cval, << msg, data >>)
        /\ UpdateContext(self, next)
        /\ UNCHANGED shared

\* Print what is on top of the stack (and pop it)
OpPrint(self) ==
    LET msg == Head(self.stack)
        next == [self EXCEPT !.pc = @ + 1, !.stack = Tail(@)]
    IN
        /\ PrintT(msg)
        /\ UpdateContext(self, next)
        /\ UNCHANGED shared

\* Pop the top of the stack, which must be a set.  Then select one of the
\* elements and push it back onto the stack.
OpChoose(self) ==
    LET choices == Head(self.stack)
    IN
        \E v \in choices.cval:
            LET next == [self EXCEPT !.pc = @ + 1, !.stack = <<v>> \o Tail(@)]
            IN
                /\ choices.ctype = "set"
                /\ UpdateContext(self, next)
                /\ UNCHANGED shared

\* "sequential" pops the address of a variable and indicates to the model
\* checker that the variable is assumed to have sequential consistency.
\* This turns off race condition checking for the variable.  For here, it can
\* just be considered a no-op.
OpSequential(self) == OpPop(self)

\* "invariant" is essentially a no-op.  Just skip over the code for the
\* invariant.
OpInvariant(self, end) ==
    /\ UpdateContext(self, [self EXCEPT !.pc = end + 1])
    /\ UNCHANGED shared

\* This is the general form of unary operators that replace the top of the
\* stack with a function computed over that value
OpUna(self, op(_)) ==
    LET e    == Head(self.stack)
        next == [self EXCEPT !.pc = @ + 1, !.stack = << op(e) >> \o Tail(@)]
    IN
        /\ UpdateContext(self, next)
        /\ UNCHANGED shared

\* Similar to OpUna but replaces two values on the stack with a single one.
OpBin(self, op(_,_)) ==
    LET e1   == self.stack[1]
        e2   == self.stack[2]
        next == [self EXCEPT !.pc = @ + 1,
            !.stack = << op(e2, e1) >> \o Tail(Tail(@))]
    IN
        /\ UpdateContext(self, next)
        /\ UNCHANGED shared

\* Apply binary operation op to first and the top of the stack n times
RECURSIVE StackFold(_,_,_,_)
StackFold(first, stack, op(_,_), n) ==
    IF n = 0
    THEN
        <<first>> \o stack
    ELSE
        StackFold(op(Head(stack), first), Tail(stack), op, n - 1)

\* Like OpBin, but perform for top n elements of the stack
OpNary(self, op(_,_), n) ==
    LET e1   == Head(self.stack)
        ns   == StackFold(e1, Tail(self.stack), op, n - 1)
        next == [self EXCEPT !.pc = @ + 1, !.stack = ns ]
    IN
        /\ UpdateContext(self, next)
        /\ UNCHANGED shared

\* Turn a Harmony list/tuple into a (reversed) sequence
List2Seq(list) ==
    LET n == Cardinality(DOMAIN list)
    IN [ i \in 1..n |-> list[HInt(n - i)] ]

\* Pop a tuple of the stack and push each of its n components
OpSplit(self, n) ==
    LET ns   == List2Seq(Head(self.stack).cval) \o Tail(self.stack)
        next == [self EXCEPT !.pc = @ + 1, !.stack = ns ]
    IN
        /\ UpdateContext(self, next)
        /\ UNCHANGED shared

\* Move the stack element at position offset to the top
OpMove(self, offset) ==
    LET part1 == SubSeq(self.stack, 1, offset - 1)
        part2 == SubSeq(self.stack, offset, offset)
        part3 == SubSeq(self.stack, offset + 1, Len(self.stack))
        next  == [self EXCEPT !.pc = @ + 1, !.stack = part2 \o part1 \o part3 ]
    IN
        /\ UpdateContext(self, next)
        /\ UNCHANGED shared

\* Duplicate the top of the stack
OpDup(self) ==
    LET next == [self EXCEPT !.pc = @ + 1, !.stack = <<Head(@)>> \o @]
    IN
        /\ UpdateContext(self, next)
        /\ UNCHANGED shared

\* The official "pc" of a thread depends on whether it is operating in
\* atomic mode or not.  If not, the pc is simply the current location
\* in the code.  However, if in atomic mode, the pc is the location where
\* the thread became atomic.
Location(ctx) == IF ctx.atomic > 0 THEN ctx.apc ELSE ctx.pc

\* Compute how many threads are currently at the given location
FunCountLabel(label) ==
    LET fdom == { c \in DOMAIN ctxbag: Location(c) = label.cval }
        fbag == [ c \in fdom |-> ctxbag[c] ]
    IN
        HInt(BagCardinality(fbag))

\* Convert the given integer into a string
RECURSIVE Int2Str(_)
Int2Str(x) ==
    IF x < 10
    THEN
        SubSeq("0123456789", x+1, x+1)
    ELSE
        LET rem == x % 10
        IN Int2Str(x \div 10) \o SubSeq("0123456789", rem+1, rem+1)

\* Bunch of value to string conversion functions coming up
RECURSIVE Val2Str(_)
RECURSIVE Seq2Str(_)
RECURSIVE Addr2Str(_)
RECURSIVE Dict2StrHelp(_,_)

\* Convert a non-empty sequence of values to a string separated by commas
Seq2Str(x) ==
    IF Len(x) = 1
    THEN Val2Str(Head(x))
    ELSE Val2Str(Head(x)) \o ", " \o Seq2Str(Tail(x))

\* Convert a sequence of values of an address
Addr2Str(x) ==
    IF x = <<>>
    THEN ""
    ELSE "[" \o Val2Str(Head(x)) \o "]" \o Addr2Str(Tail(x))

\* Convert a non-empty dictionary to a string
Dict2StrHelp(keys, dict) ==
    LET first ==
        LET k == Head(keys) IN Val2Str(k) \o ": " \o Val2Str(dict[k])
    IN
        IF Len(keys) = 1
        THEN
            first
        ELSE
            first \o ", " \o Dict2StrHelp(Tail(keys), dict)

Dict2Str(x) == Dict2StrHelp(HSort(DOMAIN x), x)

\* Convert Harmony value x into a string
Val2Str(x) ==
    CASE x.ctype = "bool"    -> IF x.cval THEN "True" ELSE "False"
    []   x.ctype = "str"     -> "'" \o x.cval \o "'"
    []   x.ctype = "int"     -> Int2Str(x.cval)
    []   x.ctype = "pc"      -> "PC(" \o Int2Str(x.cval) \o ")"
    []   x.ctype = "list"     ->
            IF x.cval = {} THEN "[]" ELSE "[ " \o Seq2Str(HSort(x.cval)) \o " ]"
    []   x.ctype = "dict"    ->
            IF DOMAIN x.cval = {} THEN "{:}" ELSE "{ " \o Dict2Str(x.cval) \o " }"
    []   x.ctype = "set"     ->
            IF x.cval = {} THEN "{}" ELSE "{ " \o Seq2Str(HSort(x.cval)) \o " }"
    []   x.ctype = "address" -> "?" \o Head(x.cval).cval \o Addr2Str(Tail(x.cval))

\* Compute the cardinality of a set of dict, or the length of a string
FunLen(s) ==
    CASE s.ctype = "set"  -> HInt(Cardinality(s.cval))
    []   s.ctype = "list" -> HInt(Cardinality(DOMAIN s.cval))
    []   s.ctype = "dict" -> HInt(Cardinality(DOMAIN s.cval))
    []   s.ctype = "str"  -> HInt(Len(s.cval))

\* Add two integers, or concatenate two sequences or strings
FunAdd(x, y) ==
    CASE x.ctype = "int"  /\ y.ctype = "int"  -> HInt(x.cval + y.cval)
    []   x.ctype = "list" /\ y.ctype = "list" -> HList(x.cval \o y.cval)
    []   x.ctype = "str"  /\ y.ctype = "str"  -> HStr(x.cval \o y.cval)

\* Check to see if x is the empty set, dict, or string
\* OBSOLETE
FunIsEmpty(x) ==
    CASE x.ctype = "set"  -> HBool(x.cval = {})
    []   x.ctype = "list" -> HBool(x.cval = <<>>)
    []   x.ctype = "dict" -> HBool((DOMAIN x.cval) = {})
    []   x.ctype = "str"  -> HBool(Len(x.cval) = 0)

\* Get the range of a dict (i.e., the values, not the keys)
Range(dict) == { dict[x] : x \in DOMAIN dict }

\* Get the minimum of a set or list
FunMin(x) ==
    CASE x.ctype = "set"  -> HMin(x.cval)
    []   x.ctype = "list" -> HMin(Range(x.cval))

\* Get the maximum of a set or list
FunMax(x) ==
    CASE x.ctype = "set"  -> HMax(x.cval)
    []   x.ctype = "list" -> HMax(Range(x.cval))

\* See if any element in the set or list is true
FunAny(x) ==
    CASE x.ctype = "set"  -> HBool(HBool(TRUE) \in x.cval)
    []   x.ctype = "list" -> HBool(HBool(TRUE) \in Range(x.cval))

\* See if all elements in the set of list are true
FunAll(x) ==
    CASE x.ctype = "set"  -> HBool(x.cval = { HBool(TRUE) })
    []   x.ctype = "list" -> HBool(HBool(FALSE) \notin Range(x.cval))

\* Can be applied to integers or sets
FunSubtract(x, y) ==
    CASE x.ctype = "int" /\ y.ctype = "int" -> HInt(x.cval - y.cval)
    []   x.ctype = "set" /\ y.ctype = "set" -> HSet(x.cval \ y.cval)

\* The following are self-explanatory
FunStr(v)           == HStr(Val2Str(v))
FunMinus(v)         == HInt(-v.cval)
FunNegate(v)        == HInt(Bits2Int(BitsNegate(Int2Bits(v.cval))))
FunAbs(v)           == HInt(IF v.cval < 0 THEN -v.cval ELSE v.cval)
FunNot(v)           == HBool(~v.cval)
FunKeys(x)          == HSet(DOMAIN x.cval)
FunRange(x, y)      == HSet({ HInt(i) : i \in x.cval .. y.cval })
FunEquals(x, y)     == HBool(x = y)
FunNotEquals(x, y)  == HBool(x /= y)
FunLT(x, y)         == HBool(HCmp(x, y) < 0)
FunLE(x, y)         == HBool(HCmp(x, y) <= 0)
FunGT(x, y)         == HBool(HCmp(x, y) > 0)
FunGE(x, y)         == HBool(HCmp(x, y) >= 0)
FunDiv(x, y)        == HInt(x.cval \div y.cval)
FunMod(x, y)        == HInt(x.cval % y.cval)
FunPower(x, y)      == HInt(x.cval ^ y.cval)
FunSetAdd(x, y)     == HSet(x.cval \union {y})
FunShiftRight(x, y) == HInt(Bits2Int(BitsShiftRight(Int2Bits(x.cval), y.cval)))
FunShiftLeft(x, y)  == HInt(Bits2Int(BitsShiftLeft(Int2Bits(x.cval), y.cval)))

\* Functions to create and extend addresses
FunClosure(x, y)    == Address(x, <<y>>)
FunAddArg(x, y)     == Address(x.cval.func, x.cval.args \o <<y>>)

\* Compute either XOR of two ints or the union minus the intersection
\* of two sets
FunExclusion(x, y) ==
    CASE x.ctype = "set" /\ y.ctype = "set" ->
        HSet((x.cval \union y.cval) \ (x.cval \intersect y.cval))
    [] x.ctype = "int" /\ y.ctype = "int" ->
        HInt(Bits2Int(BitsXOR(Int2Bits(x.cval), Int2Bits(y.cval))))

\* Merge two dictionaries.  If two keys conflict, use the minimum value
MergeDictMin(x, y) ==
    [ k \in DOMAIN x \union DOMAIN y |->
        CASE k \notin DOMAIN x -> y[k]
        []   k \notin DOMAIN y -> x[k]
        [] OTHER -> IF HCmp(x[k], y[k]) < 0 THEN x[k] ELSE y[k]
    ]

\* Merge two dictionaries.  If two keys conflict, use the maximum value
MergeDictMax(x, y) ==
    [ k \in DOMAIN x \union DOMAIN y |->
        CASE k \notin DOMAIN x -> y[k]
        []   k \notin DOMAIN y -> x[k]
        [] OTHER -> IF HCmp(x[k], y[k]) > 0 THEN x[k] ELSE y[k]
    ]

\* Union of two sets or dictionaries
FunUnion(x, y) ==
    CASE x.ctype = "set" /\ y.ctype = "set" ->
        HSet(x.cval \union y.cval)
    [] x.ctype = "dict" /\ y.ctype = "dict" ->
        HDict(MergeDictMax(x.cval, y.cval))
    [] x.ctype = "int" /\ y.ctype = "int" ->
        HInt(Bits2Int(BitsOr(Int2Bits(x.cval), Int2Bits(y.cval))))

\* Intersection of two sets or dictionaries
FunIntersect(x, y) ==
    CASE x.ctype = "set" /\ y.ctype = "set" ->
        HSet(x.cval \intersect y.cval)
    [] x.ctype = "dict" /\ y.ctype = "dict" ->
        HDict(MergeDictMin(x.cval, y.cval))
    [] x.ctype = "int" /\ y.ctype = "int" ->
        HInt(Bits2Int(BitsAnd(Int2Bits(x.cval), Int2Bits(y.cval))))

\* See if x is in y, where y is a set, a dict, or a string. In case of
\* a string, check if x is a substring of y
FunIn(x, y) ==
    CASE y.ctype = "set"  -> HBool(x \in y.cval)
    []   y.ctype = "list" -> HBool(\E k \in DOMAIN y.cval: y.cval[k] = x)
    []   y.ctype = "dict" -> HBool(x \in DOMAIN y.cval)
    []   y.ctype = "str"  ->
            LET xn == Len(x.cval)
                yn == Len(y.cval)
            IN
                HBool(\E i \in 0..(yn - xn): 
                    x.cval = SubSeq(y.cval, i+1, i+xn))

\* Concatenate n copies of list
ListTimes(list, n) ==
    LET card == Len(list.cval)
    IN
        HList([ x \in 1..(n.cval * card) |-> list.cval[((x - 1) % card) + 1] ])

\* Multiply two integers, or concatenate copies of a list
FunMult(e1, e2) ==
    CASE e1.ctype = "int" /\ e2.ctype = "int" ->
        HInt(e2.cval * e1.cval)
    [] e1.ctype = "int" /\ e2.ctype = "list" ->
        ListTimes(e2, e1)
    [] e1.ctype = "list" /\ e2.ctype = "int" ->
        ListTimes(e1, e2)

\* By Harmony rules, if there are two conflicting key->value1 and key->value2
\* mappings, the higher values wins.
InsertMap(map, key, value) ==
    [ x \in (DOMAIN map) \union {key} |->
        IF x = key
        THEN
            IF x \in DOMAIN map
            THEN
                IF HCmp(value, map[x]) > 0
                THEN
                    value
                ELSE
                    map[x]
            ELSE
                value
        ELSE
            map[x]
    ]

\* Push the current context onto the stack.  Pop the top "()" of the stack first.
OpGetContext(self) == 
    LET next  == [self EXCEPT !.pc = @ + 1,
                        !.stack = << HContext(self) >> \o Tail(@)]
    IN
        /\ Head(self.stack) = EmptyDict
        /\ UpdateContext(self, next)
        /\ UNCHANGED shared

\* Pops a value, a key, and a dict, and pushes the dict updated to
\* reflect key->value.
OpDictAdd(self) ==
    LET value == self.stack[1]
        key   == self.stack[2]
        dict  == self.stack[3]
        newd  == HDict(InsertMap(dict.cval, key, value))
        next  == [self EXCEPT !.pc = @ + 1,
            !.stack = << newd >> \o Tail(Tail(Tail(@)))]
    IN
        /\ dict.ctype = "dict"
        /\ UpdateContext(self, next)
        /\ UNCHANGED shared

\* Pop a value and a list, and a dict, and pushes a new list with the
\* value appended.
OpListAdd(self) ==
    LET value == self.stack[1]
        list  == self.stack[2]
        newl  == HList(Append(list.cval, value))
        next  == [self EXCEPT !.pc = @ + 1,
            !.stack = << newl >> \o Tail(Tail(@))]
    IN
        /\ list.ctype = "list"
        /\ UpdateContext(self, next)
        /\ UNCHANGED shared

\* Push Harmony constant c onto the stack.
OpPush(self, c) ==
    LET next == [self EXCEPT !.pc = @ + 1, !.stack = << c >> \o @]
    IN
        /\ UpdateContext(self, next)
        /\ UNCHANGED shared

\* Pop the top of the stack and store in the shared variable pointed to
\* by the sequence v of Harmony values that acts as an address
OpStore(self, v) ==
    LET next == [self EXCEPT !.pc = @ + 1, !.stack = Tail(@)]
    IN
        /\ Assert(self.readonly = 0, "Store in readonly mode")
        /\ UpdateContext(self, next)
        /\ shared' = UpdateDirAddr(shared, v, Head(self.stack))

\* Pop a value and an address and store the value at the given address
OpStoreInd(self) ==
    LET val  == self.stack[1]
        addr == self.stack[2]
        next == [self EXCEPT !.pc = @ + 1, !.stack = Tail(Tail(@))]
    IN
        /\ Assert(self.readonly = 0, "StoreInd in readonly mode")
        /\ addr.ctype = "address"
        /\ addr.cval.func = HPc(-1)
        /\ UpdateContext(self, next)
        /\ shared' = UpdateDirAddr(shared, addr.cval.args, val)

\* Pop a value and an address and store the *local* value at the given address
OpStoreVarInd(self) ==
    LET val  == self.stack[1]
        addr == self.stack[2]
        next == [self EXCEPT !.pc = @ + 1, !.stack = Tail(Tail(@)),
                                    !.vs = UpdateDirAddr(@, addr.cval.args, val)]
    IN
        /\ addr.ctype = "address"
        /\ addr.cval.func = HPc(-2)
        /\ UpdateContext(self, next)
        /\ UNCHANGED shared

\* Call method pc.
OpApply(self, pc) ==
    LET arg  == self.stack[1]
        next == [self EXCEPT !.pc = pc, !.stack = <<
                arg,
                "apply",
                self.pc,
                <<>>
            >> \o Tail(@)]
    IN
        /\ UpdateContext(self, next)
        /\ UNCHANGED shared

\* Pop an address.  If the arguments are empty, push the function and
\* continue to the next instruction.  If not, push the arguments except
\* the first and evaluate the function with the first argument.
OpLoadInd(self) ==
    LET addr == Head(self.stack).cval
        func == CASE addr.func = HPc(-1) -> shared
               []   addr.func = HPc(-2) -> self.vs
               []   OTHER               -> addr.func
        args == addr.args
    IN
        IF args = <<>>
        THEN
            LET next == [self EXCEPT !.pc = @ + 1, !.stack = <<func>> \o Tail(@)]
            IN
                /\ UpdateContext(self, next)
                /\ UNCHANGED shared
        ELSE
            LET arg == Head(args) IN
            CASE func.ctype = "pc" ->
                LET next == [self EXCEPT !.pc = func.cval, !.stack = <<
                            arg,
                            "normal",
                            self.pc,
                            Tail(args)
                        >> \o Tail(@)]
                IN
                    /\ UpdateContext(self, next)
                    /\ UNCHANGED shared
            [] func.ctype = "list" ->
                LET next == [self EXCEPT !.stack =
                        << Address(func.cval[arg.cval+1], Tail(args)) >> \o Tail(@)]
                IN
                    /\ arg.ctype = "int"
                    /\ UpdateContext(self, next)
                    /\ UNCHANGED shared
            [] func.ctype = "dict" ->
                LET next == [self EXCEPT !.stack =
                        << Address(func.cval[arg], Tail(args)) >> \o Tail(@)]
                IN
                    /\ UpdateContext(self, next)
                    /\ UNCHANGED shared
            [] func.ctype = "str" ->
                LET char == SubSeq(func.cval, arg.cval+1, arg.cval+1)
                    next == [self EXCEPT !.stack =
                        << Address(HStr(char), Tail(args)) >> \o Tail(@)]
                IN
                    /\ arg.ctype = "int"
                    /\ UpdateContext(self, next)
                    /\ UNCHANGED shared

\* Pop an address and push the value of the addressed local variable onto the stack
\* TODO.  THIS IS OBSOLETE
OpLoadVarInd(self) ==
    LET
        addr == Head(self.stack)
        val  == LoadDirAddr(self.vs, addr)
        next == [self EXCEPT !.pc = @ + 1, !.stack = <<val>> \o Tail(@)]
    IN
        /\ UpdateContext(self, next)
        /\ UNCHANGED shared

\* Push the value of shared variable v onto the stack.
OpLoad(self, v) ==
    LET next == [ self EXCEPT !.pc = @ + 1, !.stack = << shared.cval[v] >> \o @ ]
    IN
        /\ UpdateContext(self, next)
        /\ UNCHANGED shared

\* Return the context and the given parameter
OpSave(self) ==
    LET valu == Head(self.stack)
        intm == [self EXCEPT !.pc = @ + 1, !.stack = Tail(@)]
        m1   == InsertMap(EmptyFunc, HInt(0), valu)
        m2   == InsertMap(m1, HInt(1), intm)
        next == [intm EXCEPT !.stack = << HDict(m2) >> \o @]
    IN
        /\ self.atomic > 0
        /\ UpdateContext(self, next)
        /\ UNCHANGED shared

\* Store the context at the pushed address (unless it is None or ()) and
\* remove it from the  context bag and active set.  Make all contexts in
\* the context bag
OpStopInd(self) ==
    LET addr == Head(self.stack)
        next == [self EXCEPT !.pc = @ + 1, !.stack = Tail(@)]
    IN
        /\ addr.ctype = "address"
        /\ addr.cval.func = HPc(-1)
        /\ self.atomic > 0
        /\ RemoveContext(self)
        /\ IF addr = None \/ addr = EmptyDict
            THEN
                UNCHANGED shared
            ELSE
                shared' = UpdateDirAddr(shared, addr.cval.args, HContext(next))

\* What Return should do depends on whether the methods was spawned,
\* called as an ordinary method, or as an interrupt handler.  To indicate
\* this, Spawn pushes the string "process" on the stack, OpLoadInd pushes
\* the string "normal", and an interrupt pushes the string "interrupt".
\* The Frame operation also pushed the saved variables which must be restored.
OpReturnVar(self, var) ==
    LET savedvars == self.stack[1]
        calltype  == self.stack[2]
    IN
        CASE calltype = "normal" ->
            LET raddr  == self.stack[3]
                args   == self.stack[4]
                result == self.vs.cval[HStr(var)]
                next == [ self EXCEPT
                            !.pc = raddr,
                            !.vs = savedvars,
                            !.stack = << Address(result, args) >> \o Tail(Tail(Tail(Tail(@))))
                        ]
                IN
                    /\ UpdateContext(self, next)
                    /\ UNCHANGED shared
        [] calltype = "apply" ->
            LET raddr  == self.stack[3]
                args   == self.stack[4]
                result == self.vs.cval[HStr(var)]
                next == [ self EXCEPT
                            !.pc = raddr + 1,
                            !.vs = savedvars,
                            !.stack = << result >> \o Tail(Tail(Tail(Tail(@))))
                        ]
                IN
                    /\ args = <<>>
                    /\ UpdateContext(self, next)
                    /\ UNCHANGED shared
        [] calltype = "interrupt" ->
            LET raddr == self.stack[3]
                next == [ self EXCEPT
                            !.pc = raddr,
                            !.interruptLevel = FALSE,
                            !.vs = savedvars,
                            !.stack = Tail(Tail(Tail(@)))
                        ]
            IN
                /\ UpdateContext(self, next)
                /\ UNCHANGED shared
        [] calltype = "process" ->
            /\ ctxbag' = ctxbag (-) SetToBag({self})
            /\ IF self.atomic > 0
               THEN active' = DOMAIN ctxbag'
               ELSE active' = active \ { self }
            /\ UNCHANGED shared

\* Version of OpReturnVar where result is on stack instead of in variable
OpReturn(self) ==
    LET result    == self.stack[1]
        savedvars == self.stack[2]
        calltype  == self.stack[3]
    IN
        CASE calltype = "normal" ->
            LET raddr  == self.stack[4]
                args   == self.stack[5]
                next == [ self EXCEPT
                            !.pc = raddr,
                            !.vs = savedvars,
                            !.stack = << Address(result, args) >> \o Tail(Tail(Tail(Tail(Tail(@)))))
                        ]
                IN
                    /\ UpdateContext(self, next)
                    /\ UNCHANGED shared
        [] calltype = "apply" ->
            LET raddr  == self.stack[4]
                args   == self.stack[5]
                next == [ self EXCEPT
                            !.pc = raddr + 1,
                            !.vs = savedvars,
                            !.stack = << result >> \o Tail(Tail(Tail(Tail(Tail(@)))))
                        ]
                IN
                    /\ args = <<>>
                    /\ UpdateContext(self, next)
                    /\ UNCHANGED shared
        [] calltype = "interrupt" ->
            LET raddr == self.stack[4]
                next == [ self EXCEPT
                            !.pc = raddr,
                            !.interruptLevel = FALSE,
                            !.vs = savedvars,
                            !.stack = Tail(Tail(Tail(Tail(@))))
                        ]
            IN
                /\ UpdateContext(self, next)
                /\ UNCHANGED shared
        [] calltype = "process" ->
            /\ ctxbag' = ctxbag (-) SetToBag({self})
            /\ IF self.atomic > 0
               THEN active' = DOMAIN ctxbag'
               ELSE active' = active \ { self }
            /\ UNCHANGED shared

\* Version of OpReturnVar with a default value
OpReturnVarDefault(self, var, deflt) ==
    LET savedvars == self.stack[1]
        calltype  == self.stack[2]
    IN
        CASE calltype = "normal" ->
            LET raddr  == self.stack[3]
                args   == self.stack[4]
                result == IF HStr(var) \in DOMAIN self.vs.cval THEN self.vs.cval[HStr(var)] ELSE deflt
                next == [ self EXCEPT
                            !.pc = raddr,
                            !.vs = savedvars,
                            !.stack = << Address(result, args) >> \o Tail(Tail(Tail(Tail(@))))
                        ]
                IN
                    /\ UpdateContext(self, next)
                    /\ UNCHANGED shared
        [] calltype = "apply" ->
            LET raddr  == self.stack[3]
                args   == self.stack[4]
                result == IF HStr(var) \in DOMAIN self.vs.cval THEN self.vs.cval[HStr(var)] ELSE deflt
                next == [ self EXCEPT
                            !.pc = raddr + 1,
                            !.vs = savedvars,
                            !.stack = << result >> \o Tail(Tail(Tail(Tail(@))))
                        ]
                IN
                    /\ args = <<>>
                    /\ UpdateContext(self, next)
                    /\ UNCHANGED shared
        [] calltype = "interrupt" ->
            LET raddr == self.stack[3]
                next == [ self EXCEPT
                            !.pc = raddr,
                            !.interruptLevel = FALSE,
                            !.vs = savedvars,
                            !.stack = Tail(Tail(Tail(@)))
                        ]
            IN
                /\ UpdateContext(self, next)
                /\ UNCHANGED shared
        [] calltype = "process" ->
            /\ ctxbag' = ctxbag (-) SetToBag({self})
            /\ IF self.atomic > 0
               THEN active' = DOMAIN ctxbag'
               ELSE active' = active \ { self }
            /\ UNCHANGED shared

\* Set the program counter pc to the given value
OpJump(self, pc) ==
    LET next == [ self EXCEPT !.pc = pc ]
    IN
        /\ UpdateContext(self, next)
        /\ UNCHANGED shared

\* Pop a value of the stack.  If it equals cond, set the pc to the
\* given value.
OpJumpCond(self, pc, cond) ==
    LET next == [ self EXCEPT !.pc = IF Head(self.stack) = cond
                    THEN pc ELSE (@ + 1), !.stack = Tail(@) ]
    IN
        /\ UpdateContext(self, next)
        /\ UNCHANGED shared

\* Spawn a new thread.  If the current thread is not atomic, the
\* thread goes into the active set as well.
OpSpawn(self) ==
    LET local == self.stack[1]
        addr  == self.stack[2]
        entry == addr.cval.func
        arg   == addr.cval.args[1]
        next  == [self EXCEPT !.pc = @ + 1, !.stack = Tail(Tail(@))]
        newc  == InitContext(entry.cval, 0, arg)
    IN
        /\ entry.ctype = "pc"
        /\ IF self.atomic > 0
           THEN active' = (active \ { self }) \union { next }
           ELSE active' = (active \ { self }) \union { next, newc }
        /\ ctxbag' = (ctxbag (-) SetToBag({self})) (+) SetToBag({next,newc})
        /\ UNCHANGED shared

\* Operation to set a trap.
OpTrap(self) ==
    LET entry == self.stack[1]
        arg   == self.stack[2]
        next  == [self EXCEPT !.pc = @ + 1, !.stack = Tail(Tail(@)),
                                        !.trap = << entry.cval, arg >>]
    IN
        /\ entry.ctype = "pc"
        /\ UpdateContext(self, next)
        /\ UNCHANGED shared

\* Restore a context that is pushed on the stack.  Also push the argument
\* onto the restored context's stack
\* TODO.  Currently arg and ctx are in the wrong order
OpGo(self) ==
    LET ctx  == self.stack[1]
        arg  == self.stack[2]
        next == [self EXCEPT !.pc = @ + 1, !.stack = Tail(Tail(@))]
        newc == [ctx.cval EXCEPT !.stack = << arg >> \o @]
    IN
        /\ IF self.atomic > 0
            THEN active' = (active \ { self }) \union { next }
            ELSE active' = (active \ { self }) \union { next,newc }
        /\ ctxbag' = (ctxbag (-) SetToBag({self})) (+) SetToBag({next,newc})
        /\ UNCHANGED shared

\* When there are no threads left, the Idle action kicks in
Idle ==
    /\ active = {}
    /\ UNCHANGED allvars

Step(self) == \/ self.pc = 0 /\ OpFrame(self, "__init__", VList(<<  >>))
              \/ self.pc = 1 /\ OpPush(self, HInt(0))
              \/ self.pc = 2 /\ OpStore(self, <<HStr("in_cs")>>)
              \/ self.pc = 3 /\ OpJump(self, 15)
              \/ self.pc = 4 /\ OpFrame(self, "invariant", VList(<<  >>))
              \/ self.pc = 5 /\ OpLoad(self, HStr("in_cs"))
              \/ self.pc = 6 /\ OpPush(self, HSet({}))
              \/ self.pc = 7 /\ OpPush(self, HInt(0))
              \/ self.pc = 8 /\ OpBin(self, FunSetAdd)
              \/ self.pc = 9 /\ OpPush(self, HInt(1))
              \/ self.pc = 10 /\ OpBin(self, FunSetAdd)
              \/ self.pc = 11 /\ OpBin(self, FunIn)
              \/ self.pc = 12 /\ OpUna(self, FunNot)
              \/ self.pc = 13 /\ OpAssert(self, "Harmony Assertion (file=pt.hny, line=2) failed")
              \/ self.pc = 14 /\ OpReturnVarDefault(self, "result", None)
              \/ self.pc = 15 /\ OpInvariant(self, 4)
              \/ self.pc = 16 /\ OpJump(self, 52)
              \/ self.pc = 17 /\ OpFrame(self, "invariant", VList(<<  >>))
              \/ self.pc = 18 /\ OpPush(self, HList(<<>>))
              \/ self.pc = 19 /\ OpStoreVar(self, VName("$accu18"))
              \/ self.pc = 20 /\ OpPush(self, HList(<<>>))
              \/ self.pc = 21 /\ OpPush(self, HInt(0))
              \/ self.pc = 22 /\ OpListAdd(self)
              \/ self.pc = 23 /\ OpPush(self, HInt(1))
              \/ self.pc = 24 /\ OpListAdd(self)
              \/ self.pc = 25 /\ OpPush(self, HInt(0))
              \/ self.pc = 26 /\ OpCut(self, VName("tid"))
              \/ self.pc = 27 /\ OpJumpCond(self, 36, HBool(FALSE))
              \/ self.pc = 28 /\ OpLoadVar(self, VName("$accu18"))
              \/ self.pc = 29 /\ OpDelVar(self, VName("$accu18"))
              \/ self.pc = 30 /\ OpLoadVar(self, VName("tid"))
              \/ self.pc = 31 /\ OpDelVar(self, VName("tid"))
              \/ self.pc = 32 /\ OpApply(self, 125)
              \/ self.pc = 33 /\ OpListAdd(self)
              \/ self.pc = 34 /\ OpStoreVar(self, VName("$accu18"))
              \/ self.pc = 35 /\ OpJump(self, 26)
              \/ self.pc = 36 /\ OpDelVar(self, VName("tid"))
              \/ self.pc = 37 /\ OpLoadVar(self, VName("$accu18"))
              \/ self.pc = 38 /\ OpDelVar(self, VName("$accu18"))
              \/ self.pc = 39 /\ OpUna(self, FunAny)
              \/ self.pc = 40 /\ OpJumpCond(self, 48, HBool(FALSE))
              \/ self.pc = 41 /\ OpPush(self, HList(<<>>))
              \/ self.pc = 42 /\ OpPush(self, HInt(0))
              \/ self.pc = 43 /\ OpListAdd(self)
              \/ self.pc = 44 /\ OpPush(self, HInt(1))
              \/ self.pc = 45 /\ OpListAdd(self)
              \/ self.pc = 46 /\ OpApply(self, 144)
              \/ self.pc = 47 /\ OpJump(self, 49)
              \/ self.pc = 48 /\ OpPush(self, HBool(TRUE))
              \/ self.pc = 49 /\ OpUna(self, FunNot)
              \/ self.pc = 50 /\ OpAssert(self, "Harmony Assertion (file=pt.hny, line=3) failed")
              \/ self.pc = 51 /\ OpReturnVarDefault(self, "result", None)
              \/ self.pc = 52 /\ OpInvariant(self, 17)
              \/ self.pc = 53 /\ OpPush(self, Address(HPc(-1), <<HStr("flags")>>))
              \/ self.pc = 54 /\ OpSequential(self)
              \/ self.pc = 55 /\ OpPush(self, Address(HPc(-1), <<HStr("turn")>>))
              \/ self.pc = 56 /\ OpSequential(self)
              \/ self.pc = 57 /\ OpPush(self, HList(<<>>))
              \/ self.pc = 58 /\ OpPush(self, HBool(FALSE))
              \/ self.pc = 59 /\ OpListAdd(self)
              \/ self.pc = 60 /\ OpPush(self, HBool(FALSE))
              \/ self.pc = 61 /\ OpListAdd(self)
              \/ self.pc = 62 /\ OpStore(self, <<HStr("flags")>>)
              \/ self.pc = 63 /\ OpPush(self, HSet({}))
              \/ self.pc = 64 /\ OpPush(self, HInt(0))
              \/ self.pc = 65 /\ OpBin(self, FunSetAdd)
              \/ self.pc = 66 /\ OpPush(self, HInt(1))
              \/ self.pc = 67 /\ OpBin(self, FunSetAdd)
              \/ self.pc = 68 /\ OpChoose(self)
              \/ self.pc = 69 /\ OpStore(self, <<HStr("turn")>>)
              \/ self.pc = 70 /\ OpJump(self, 195)
              \/ self.pc = 71 /\ OpFrame(self, "thread", VName("self"))
              \/ self.pc = 72 /\ OpPush(self, HSet({}))
              \/ self.pc = 73 /\ OpPush(self, HBool(FALSE))
              \/ self.pc = 74 /\ OpBin(self, FunSetAdd)
              \/ self.pc = 75 /\ OpPush(self, HBool(TRUE))
              \/ self.pc = 76 /\ OpBin(self, FunSetAdd)
              \/ self.pc = 77 /\ OpChoose(self)
              \/ self.pc = 78 /\ OpJumpCond(self, 122, HBool(FALSE))
              \/ self.pc = 79 /\ OpPush(self, Address(HPc(-1), <<HStr("flags")>>))
              \/ self.pc = 80 /\ OpLoadVar(self, VName("self"))
              \/ self.pc = 81 /\ OpBin(self, FunAddArg)
              \/ self.pc = 82 /\ OpPush(self, HBool(TRUE))
              \/ self.pc = 83 /\ OpStoreInd(self)
              \/ self.pc = 84 /\ OpPush(self, HInt(1))
              \/ self.pc = 85 /\ OpLoadVar(self, VName("self"))
              \/ self.pc = 86 /\ OpBin(self, FunSubtract)
              \/ self.pc = 87 /\ OpStore(self, <<HStr("turn")>>)
              \/ self.pc = 88 /\ OpPush(self, Address(HPc(-1), <<HStr("flags")>>))
              \/ self.pc = 89 /\ OpPush(self, HInt(1))
              \/ self.pc = 90 /\ OpLoadVar(self, VName("self"))
              \/ self.pc = 91 /\ OpBin(self, FunSubtract)
              \/ self.pc = 92 /\ OpBin(self, FunAddArg)
              \/ self.pc = 93 /\ OpLoadInd(self)
              \/ self.pc = 94 /\ OpUna(self, FunNot)
              \/ self.pc = 95 /\ OpJumpCond(self, 100, HBool(TRUE))
              \/ self.pc = 96 /\ OpLoad(self, HStr("turn"))
              \/ self.pc = 97 /\ OpLoadVar(self, VName("self"))
              \/ self.pc = 98 /\ OpBin(self, FunEquals)
              \/ self.pc = 99 /\ OpJump(self, 101)
              \/ self.pc = 100 /\ OpPush(self, HBool(TRUE))
              \/ self.pc = 101 /\ OpJumpCond(self, 88, HBool(FALSE))
              \/ self.pc = 102 /\ OpJump(self, 104)
              \/ self.pc = 103 /\ OpJump(self, 88)
              \/ self.pc = 104 /\ OpAtomicInc(self)
              \/ self.pc = 105 /\ OpLoad(self, HStr("in_cs"))
              \/ self.pc = 106 /\ OpPush(self, HInt(1))
              \/ self.pc = 107 /\ OpNary(self, FunAdd, 2)
              \/ self.pc = 108 /\ OpStore(self, <<HStr("in_cs")>>)
              \/ self.pc = 109 /\ OpAtomicDec(self)
              \/ self.pc = 110 /\ OpAtomicInc(self)
              \/ self.pc = 111 /\ OpLoad(self, HStr("in_cs"))
              \/ self.pc = 112 /\ OpPush(self, HInt(1))
              \/ self.pc = 113 /\ OpBin(self, FunSubtract)
              \/ self.pc = 114 /\ OpStore(self, <<HStr("in_cs")>>)
              \/ self.pc = 115 /\ OpAtomicDec(self)
              \/ self.pc = 116 /\ OpPush(self, Address(HPc(-1), <<HStr("flags")>>))
              \/ self.pc = 117 /\ OpLoadVar(self, VName("self"))
              \/ self.pc = 118 /\ OpBin(self, FunAddArg)
              \/ self.pc = 119 /\ OpPush(self, HBool(FALSE))
              \/ self.pc = 120 /\ OpStoreInd(self)
              \/ self.pc = 121 /\ OpJump(self, 72)
              \/ self.pc = 122 /\ OpDelVar(self, VName("self"))
              \/ self.pc = 123 /\ OpReturnVarDefault(self, "result", None)
              \/ self.pc = 124 /\ OpJump(self, 195)
              \/ self.pc = 125 /\ OpFrame(self, "entering", VName("tid"))
              \/ self.pc = 126 /\ OpPush(self, Address(HPc(-1), <<HStr("flags")>>))
              \/ self.pc = 127 /\ OpPush(self, HInt(1))
              \/ self.pc = 128 /\ OpLoadVar(self, VName("tid"))
              \/ self.pc = 129 /\ OpBin(self, FunSubtract)
              \/ self.pc = 130 /\ OpBin(self, FunAddArg)
              \/ self.pc = 131 /\ OpLoadInd(self)
              \/ self.pc = 132 /\ OpJumpCond(self, 138, HBool(FALSE))
              \/ self.pc = 133 /\ OpLoad(self, HStr("turn"))
              \/ self.pc = 134 /\ OpLoadVar(self, VName("tid"))
              \/ self.pc = 135 /\ OpDelVar(self, VName("tid"))
              \/ self.pc = 136 /\ OpBin(self, FunNotEquals)
              \/ self.pc = 137 /\ OpJump(self, 140)
              \/ self.pc = 138 /\ OpDelVar(self, VName("tid"))
              \/ self.pc = 139 /\ OpPush(self, HBool(FALSE))
              \/ self.pc = 140 /\ OpStoreVar(self, VName("tst1"))
              \/ self.pc = 141 /\ OpReturnVar(self, "tst1")
              \/ self.pc = 142 /\ OpDelVar(self, VName("tst1"))
              \/ self.pc = 143 /\ OpJump(self, 195)
              \/ self.pc = 144 /\ OpFrame(self, "one_entered", VName("tids"))
              \/ self.pc = 145 /\ OpPush(self, HInt(0))
              \/ self.pc = 146 /\ OpStoreVar(self, VName("i"))
              \/ self.pc = 147 /\ OpPush(self, HBool(FALSE))
              \/ self.pc = 148 /\ OpStoreVar(self, VName("tst2"))
              \/ self.pc = 149 /\ OpLoadVar(self, VName("i"))
              \/ self.pc = 150 /\ OpLoadVar(self, VName("tids"))
              \/ self.pc = 151 /\ OpUna(self, FunLen)
              \/ self.pc = 152 /\ OpBin(self, FunLT)
              \/ self.pc = 153 /\ OpJumpCond(self, 157, HBool(FALSE))
              \/ self.pc = 154 /\ OpLoadVar(self, VName("tst2"))
              \/ self.pc = 155 /\ OpUna(self, FunNot)
              \/ self.pc = 156 /\ OpJump(self, 158)
              \/ self.pc = 157 /\ OpPush(self, HBool(FALSE))
              \/ self.pc = 158 /\ OpJumpCond(self, 171, HBool(FALSE))
              \/ self.pc = 159 /\ OpLoadVar(self, VName("i"))
              \/ self.pc = 160 /\ OpApply(self, 176)
              \/ self.pc = 161 /\ OpJumpCond(self, 165, HBool(FALSE))
              \/ self.pc = 162 /\ OpDelVar(self, VName("tst2"))
              \/ self.pc = 163 /\ OpPush(self, HBool(TRUE))
              \/ self.pc = 164 /\ OpStoreVar(self, VName("tst2"))
              \/ self.pc = 165 /\ OpLoadVar(self, VName("i"))
              \/ self.pc = 166 /\ OpDelVar(self, VName("i"))
              \/ self.pc = 167 /\ OpPush(self, HInt(1))
              \/ self.pc = 168 /\ OpNary(self, FunAdd, 2)
              \/ self.pc = 169 /\ OpStoreVar(self, VName("i"))
              \/ self.pc = 170 /\ OpJump(self, 149)
              \/ self.pc = 171 /\ OpDelVar(self, VName("i"))
              \/ self.pc = 172 /\ OpDelVar(self, VName("tids"))
              \/ self.pc = 173 /\ OpReturnVar(self, "tst2")
              \/ self.pc = 174 /\ OpDelVar(self, VName("tst2"))
              \/ self.pc = 175 /\ OpJump(self, 195)
              \/ self.pc = 176 /\ OpFrame(self, "cs", VName("tid"))
              \/ self.pc = 177 /\ OpPush(self, Address(HPc(-1), <<HStr("flags")>>))
              \/ self.pc = 178 /\ OpPush(self, HInt(1))
              \/ self.pc = 179 /\ OpLoadVar(self, VName("tid"))
              \/ self.pc = 180 /\ OpBin(self, FunSubtract)
              \/ self.pc = 181 /\ OpBin(self, FunAddArg)
              \/ self.pc = 182 /\ OpLoadInd(self)
              \/ self.pc = 183 /\ OpUna(self, FunNot)
              \/ self.pc = 184 /\ OpJumpCond(self, 190, HBool(TRUE))
              \/ self.pc = 185 /\ OpLoad(self, HStr("turn"))
              \/ self.pc = 186 /\ OpLoadVar(self, VName("tid"))
              \/ self.pc = 187 /\ OpDelVar(self, VName("tid"))
              \/ self.pc = 188 /\ OpBin(self, FunEquals)
              \/ self.pc = 189 /\ OpJump(self, 192)
              \/ self.pc = 190 /\ OpDelVar(self, VName("tid"))
              \/ self.pc = 191 /\ OpPush(self, HBool(TRUE))
              \/ self.pc = 192 /\ OpStoreVar(self, VName("tst3"))
              \/ self.pc = 193 /\ OpReturnVar(self, "tst3")
              \/ self.pc = 194 /\ OpDelVar(self, VName("tst3"))
              \/ self.pc = 195 /\ OpPush(self, Address(HPc(71), <<>>))
              \/ self.pc = 196 /\ OpPush(self, HInt(0))
              \/ self.pc = 197 /\ OpBin(self, FunAddArg)
              \/ self.pc = 198 /\ OpPush(self, EmptyDict)
              \/ self.pc = 199 /\ OpSpawn(self)
              \/ self.pc = 200 /\ OpPush(self, Address(HPc(71), <<>>))
              \/ self.pc = 201 /\ OpPush(self, HInt(1))
              \/ self.pc = 202 /\ OpBin(self, FunAddArg)
              \/ self.pc = 203 /\ OpPush(self, EmptyDict)
              \/ self.pc = 204 /\ OpSpawn(self)
              \/ self.pc = 205 /\ OpReturnVarDefault(self, "result", None)
              \/ self.pc = 206 /\ OpDelVar(self, VName("result"))

Interrupt(self) ==
    LET next == [ self EXCEPT !.pc = self.trap[1],
                    !.stack = << self.trap[2], "interrupt", self.pc >> \o @,
                    !.interruptLevel = TRUE, !.trap = <<>> ]
    IN
        /\ self.trap # <<>>
        /\ ~self.interruptLevel
        /\ UpdateContext(self, next)
        /\ UNCHANGED shared

Next == (\E self \in active: Step(self) \/ Interrupt(self)) \/ Idle
Spec == Init /\ [][Next]_allvars

THEOREM Spec => []TypeInvariant
THEOREM Spec => [](active \subseteq (DOMAIN ctxbag))
THEOREM Spec => ((active = {}) => [](active = {}))
\* THEOREM Spec => []<>(active = {})
================
