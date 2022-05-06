signature STACK =
sig
    type 'a Stack
    exception EmptyStack
    exception Error of string
    val create: 'a -> 'a Stack
    val push : 'a * 'a Stack -> 'a Stack
    val pop : 'a Stack -> 'a Stack
    val top : 'a Stack -> 'a
    val empty: 'a Stack -> bool
    val poptop : 'a Stack -> ('a * 'a Stack) option
    val nth : 'a Stack * int -> 'a
    val drop : 'a Stack * int -> 'a Stack
    val depth : 'a Stack -> int
    val app : ('a -> unit) -> 'a Stack -> unit
    val map : ('a -> 'b) -> 'a Stack -> 'b Stack
    val mapPartial : ('a -> 'b option) -> 'a Stack -> 'b Stack
    val find : ('a -> bool) -> 'a Stack -> 'a option
    val filter : ('a -> bool) -> 'a Stack -> 'a Stack
    val foldr : ('a * 'b -> 'b) -> 'b -> 'a Stack -> 'b
    val foldl : ('a * 'b -> 'b) -> 'b -> 'a Stack -> 'b
    val exists : ('a -> bool) -> 'a Stack -> bool
    val all : ('a -> bool) -> 'a Stack -> bool
    val list2stack : 'a list -> 'a Stack (* Convert a list into a Stack *)
    val stack2list: 'a Stack -> 'a list (* Convert a Stack into a list *)
    val toString: ('a -> string) -> 'a Stack -> string
end;

structure FunStack : STACK =
struct
    type 'a Stack = 'a list
    exception EmptyStack
    exception Error of string
    
    fun create (x) = [x]

    fun push (x, stac) = (x::stac)

    fun pop ([]) = raise EmptyStack
        | pop (hd::tl) = tl

    fun top (hd::tl) = hd
        | top ([]) = raise EmptyStack

    fun empty ([]) = true
        | empty (hd::tl) = false

    fun poptop (stac)  = SOME(top(stac),pop(stac))

    fun nth ((hd::tl), n) = if (n = 0) then hd else nth (tl, n-1)
            | nth (([]), n) = raise Error("List is shorter than n")

    fun drop ((hd::tl), n) = if (n = 0) then (hd::tl) else drop(tl, n-1)
            | drop (([]), n) = []

    fun depth (hd::tl) = 1+depth(tl)
            | depth([]) = 0

    fun app f (hd::tl) = (f hd; (app f tl))
            | app f [] = ()

    fun map f (hd::tl) = (f(hd) :: (map f tl))
        | map f [] = []

    fun mapPartial f (hd::tl) = if (Option.isSome(f(hd)) = true) then (Option.valOf(f(hd))::(mapPartial f tl)) else (mapPartial f tl)
        | mapPartial f [] = []

    fun find f (hd::tl) = if(f(hd) = true) then SOME(hd) else find f tl
        | find f ([]) = NONE;

    fun filter f (hd::tl) = if (f(hd) = true) then (hd::(filter f tl)) else filter f tl
        | filter f [] = []

    fun foldr f x (hd::tl) = f(hd, (foldr f x tl))
        | foldr f x [] = x

    fun foldl f x (hd::tl) = foldl f (f(hd,x)) tl
        | foldl f x ([]) = x

    fun exists f (hd::tl) = if (f(hd) = true) then true else exists f tl
        | exists f ([]) = false

    fun all f (hd::tl) = if (f(hd) = false) then false else all f tl
        | all f [] = true

    fun list2stack (l) = l

    fun stack2list (stac) = stac

    fun toString f (hd::tl) = (f hd) ^ "." ^ toString f tl
        | toString f [] = ""

end;