(*
  Test file for HawkFun, a small functional language
  S. Knepper 2016-12-16
  C. Hoffman 2016-12-16
*)

(* 

 Windows
   cd C:Users\Cody\Desktop\Project
   bin\fslex --unicode Lexer.fsl
   bin\fsyacc --module Parser Parser.fsy


*)

   // Cody Laptop
   #r "C:\Users\Cody\Desktop\Project\\bin\FsLexYacc.Runtime.dll"
   #load "C:\Users\Cody\Desktop\Project\Absyn.fs" 
   #load "C:\Users\Cody\Desktop\Project\Parser.fs"
   #load "C:\Users\Cody\Desktop\Project\Lexer.fs" 
   #load "C:\Users\Cody\Desktop\Project\Parse.fs" 
   #load "C:\Users\Cody\Desktop\Project\Env.fs"
   #load "C:\Users\Cody\Desktop\Project\TypeCheck.fs"
   #load "C:\Users\Cody\Desktop\Project\Inter.fs" 
   // Cody Lab Machine
   #r "H:\Desktop\Project\Project\\bin\FsLexYacc.Runtime.dll"
   #load "H:\Desktop\Project\Project\Absyn.fs" 
   #load "H:\Desktop\Project\Project\Parser.fs"
   #load "H:\Desktop\Project\Project\Lexer.fs" 
   #load "H:\Desktop\Project\Project\Parse.fs" 
   #load "H:\Desktop\Project\Project\Env.fs"
   #load "H:\Desktop\Project\Project\TypeCheck.fs"
   #load "H:\Desktop\Project\Project\Inter.fs" 


open Absyn
let fromString = Parse.fromString
let check = TypeCheck.check
let eval = Inter.eval
let run = Inter.run
let crun e = run (check e)

let ex = fromString "
  local var x = false in 2 * x end
"

check ex


check (fromString "fn (x:int) => x end")

check (fromString "local fun f (x:int) = x in (f 1) end")
check (fromString "local var f = fn (x:int) => x end in (f 1) end")
check (fromString "
  local fun rec f (x:int) : bool = f (x - 1) in f 2 end
")


let ex = fromString "
  print ((1::2::3::([]:int list)) :: (4::3::([]:int list)) :: ([]:int list list))
"

let ex1 = check ex

crun ex

let ex1 = fromString "
local 
  fun add (x:int) = 
  local 
    fun addx (y:int) = x + true 
  in 
    addx
  end 
in
  add 3 4
end
"

check ex1

let ex1 = fromString "
local 
  fun add (x:int) = fn (y:int) => x + y end
in
  add 3 4
end
"
crun ex1
let ex1 = fromString "
  1 :: (1 + 2) :: ([]:int list)
"

check ex1 

run ex1

let ex1 = fromString "
local 
  fun add (x:int) = fn (y:int) => x + y end
  var x = add 3 4
in
 (print x)
end
"

crun ex1
let ex2 = check ex1
let ex = check ex1
run ex2
eval ex []

let ex1 = fromString "
local
  var add = fn (x:int) => fn (y:int) => x + y end end
in
  add 3 4
end
"
check ex1
run ex1


let ex2 = fromString "
local 
  fun add (x:int) = fn (y:int) => x + y end
in
  add
end
"

check ex2 
run ex2


let ex2 = fromString "
  (fn (y:int) => y + 1 end) 4
"

run ex2



let ex3 = fromString "
local 
  fun twice (f:int -> int) = fn (x:int) => f (f x) end
  fun inc (x:int) = x + 1 
in
  twice inc 3
end 
"

check ex3

run ex3


let ex4 = fromString "
local 
  fun compose (f:int -> int) = fn (g:int -> int) => fn (x:int) => f (g x) end end
  fun inc (x:int) = x + 1
  fun square (x:int) = x * x 
in
  compose inc square 3
end 
"

run ex4

let ex = fromString "
local
  fun rec fib (n:int) : int =
    if n = 0 then 1 else n * (fib (n - 1))
in 
  fib 4
end
"

check ex

run ex

let ex = fromString" 
local
  var x = tl (1::2::([]:int list)) 
  in
  x
end
"
crun ex

let ex = fromString "
4 ; null
"

check ex
run ex 

let ex = fromString "
5 ; 4 ; 6 + 1
"

let ex = fromString "
local
  var x = tl (4 :: 5 :: ([]:int))
in
  if x = ([]:int) then 10 else 11
end
"

let ex = fromString" 
local 
 var x = tl (4::5::55::44::100:: ([]:int list))
 in 
 x
end 
"
crun ex
let ex = fromString"
local 
  var x = tl (4::5::([]:int list))
in
x
end
"

check ex
run ex
let ex = fromString "
local
  var x = true :: false :: ([]:bool list)
in
  tl (x)
end
"

let ex = fromString "
local
  var e = ([]:bool list)
  var x = (true :: true :: e) :: (false :: e) :: ([]:bool list list)
in
  x
end
"
crun ex

let ex = fromString "
(false :: ([]:bool)) :: ([]:bool list list)
"

crun ex

let ex = fromString "
local   
  var x = true :: false :: ([]:bool list)
  in 
  tl (x)
end
"
let ex1 = check ex
run ex1

let ex = fromString "
local
  var e = ([]:bool list)
  var v0 = false :: e
  var v1 = true :: e
  var v2 = ([]:bool list list)
  var x = v0 :: v1 :: v2
in
  ise e
end
"
crun ex
check ex

let ex = fromString"
local
  var x = ([]:int list)
  12::x
  in
  
end
"
let ex = fromString" [print]:int list"
crun ex

let ex = fromString "
local
  fun rec map (f: int -> int) : (int list -> int list) = 
    fn (l: int list) => 
      if ise l then l else (f (hd l)) :: (map f (tl l)) end
  fun flip (x:bool) = not x
  var e = ([]:bool)
in
  map (fn (x:bool) => not x end) (true::false::e)
end
"

check ex

run ex

let ex = fromString"
local var x = true::false::false::true::([]:bool list) in ise x end"
crun ex

let ex = fromString"local print x = ([]:int list) in  x end"
crun ex

let ex = fromString"local var x = true in x ; 500 end"
crun ex

let ex = fromString"local print 10+10"
crun ex

let ex = fromString"print (10)"
check ex
run ex


let ex = fromString"
local
  var el = ([] : int list)
  fun reverse (l : int list) =
    local
      fun rec rev (l1 : int list) : int list =
        fn (l2 : int list) =>
          if l1 = el then l2 else rev (tl l1) ((hd l1)::l2)
        end
     in
       rev l el
     end
  in
     reverse (1::2::3::el)
  end
"
crun ex

let ex1 = check ex

let ex = fromString"
local
fun add (x : int) = fn (y : int) => x + y end
fun inc (x : int) = x + 1
var y = add 3 (inc 4)
var x = y * 3
fun rec fac (n : int) : int =
if n = 0 then 1 else n * (fac (n - 1))
in
print x; print y; fac 5; x :: y :: ([] : int list)
end"

crun ex
let ex1 = check ex 
run ex1

let ex =fromString"
local
  var el = ([] : int list)
  fun reverse (l : int list) =
  local
    fun rec rev (l1 : int list) : int list =
    fn (l2 : int list) =>
    if l1 = el then l2 else rev (tl l1) ((hd l1)::l2)
   end
in
  rev l el
end
in
  reverse (1::2::3::el)
end"

let ex1 = check ex
run ex1


let x1 = fromString" local
var twice = fn (f:int -> int) => fn (x:int) => f (f x) end end
var compose = fn (f:int -> int) => fn (g:int -> int) => fn (x:int) =>
f (g x)
end end end
fun rec map (f: int -> int) : (int list -> int list) =
fn (l: int list) =>
if ise l then l else (f (hd l)) :: (map f (tl l))
end
fun square (x:int) = x * x
fun inc (x:int) = x + 1
var inc2 = twice inc
var e = ([]:int list)
var x = compose inc square 3
var l = map (fn (x:int) => 2*x end) (1::2::3::e)
in
x::l
end"let ex1 = check x1run ex1