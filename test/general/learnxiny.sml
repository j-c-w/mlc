(* t-compile: --verify-all *)
(* t-compile: -o --verify-all *)
(* This is processed from: https://learnxinyminutes.com/docs/standard-ml/ *)

val rent = 1200
val phone_no = 5551337
val pi = 3.14159
val negative_number = ~15  

val diameter = 7926 : int
val e = 2.718 : real
val name = "Bobby" : string

fun is_large(x : int) = if x > 37 then true else false

val tau = 2.0 * pi         
val twice_rent = 2 * rent  
val yeh = 1.25 * (Real.fromInt 10) 

val real_division = 14.0 / 4.0  
val int_division  = 14 div 4    
val int_remainder = 14 mod 4    

val negative_rent = ~(rent)  

val got_milk = true
val got_bread = false
val has_breakfast = got_milk andalso got_bread  
val has_something = got_milk orelse got_bread   
val is_sad = not(has_something)                 

val pays_same_rent = (rent = 1300)  
val is_wrong_phone_no = (phone_no <> 5551337) 

fun is_large x = x > 37  
val is_sad = not has_something
val pays_same_rent = rent = 1300  
val is_wrong_phone_no = phone_no <> 5551337
val negative_rent = ~rent  

val some_answer = is_large (5 + 5)      


val foo = "Hello, World!\n"  
val one_letter = #"a"        

val combined = "Hello " ^ "there, " ^ "fellow!\n"  

val _ = print foo       
val _ = print combined  


val bar = [ #"H", #"e", #"l", #"l", #"o" ]  

val bob = "Hello"          
val bob_char_count = 5  
val _ = print (bob ^ "\n")            

val numbers = [1, 3, 3, 7, 229, 230, 248]  
val names = [ "Fred", "Jane", "Alice" ]    

val groups = [ [ "Alice", "Bob" ],
               [ "Huey", "Dewey", "Louie" ],
               [ "Bonnie", "Clyde" ] ]   

val number_count = 7     

val more_numbers = 13 :: numbers  
val more_groups  = ["Batman","Superman"] :: groups

val guest_list = [ "Mom", "Dad" ] @ [ "Aunt", "Uncle" ]

val guest_list = "Mom" :: "Dad" :: [ "Aunt", "Uncle" ]
val guest_list = "Mom" :: ("Dad" :: ("Aunt" :: ("Uncle" :: [])))

val everyone = [ "Alice", "Bob", "Huey"]

val lots = [ 5, 5, 5, 6, 4, 5, 6, 5, 4, 5, 7, 3 ]  

val person1 = ("Simon", 28, 3.14159)  

val likes = [ ("Alice", "ice cream"),
              ("Bob",   "hot dogs"),
              ("Bob",   "Alice") ]     

val mixup = [ ("Alice", 39),
              ("Bob",   37),
              ("Eve",   41) ]  

val good_bad_stuff =
  (["ice cream", "hot dogs", "chocolate"],
   ["liver", "paying the rent" ])           

fun add_them (a, b) = a + b    
val test_it = add_them (3, 4) 

fun thermometer temp =
    if temp < 37
    then "Cold"
    else if temp > 37
         then "Warm"
         else "Normal"

val test_thermo = thermometer 40  

fun fibonacci n =
    if n = 0 then 0 else                   
    if n = 1 then 1 else                  
    fibonacci (n - 1) + fibonacci (n - 2)

val x = 42
fun answer(question) =
    if question = "What is the meaning of life, the universe and everything?"
    then x
    else 1000
val x = 43
val hmm = answer "What is the meaning of life, the universe and everything?"


fun solve2 (a : real, b : real, c : real) =
    ((~b + Math.sqrt(b * b - 4.0 * a * c)) / (2.0 * a),
     (~b - Math.sqrt(b * b - 4.0 * a * c)) / (2.0 * a))

fun solve2 (a : real, b : real, c : real) =
    let val discr  = b * b - 4.0 * a * c
        val sqr = Math.sqrt discr
        val denom = 2.0 * a
    in ((~b + sqr) / denom,
        (~b - sqr) / denom)
    end


fun fibonacci 0 = 0  
  | fibonacci 1 = 1  
  | fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)  

fun first_elem (x::xs) = x
fun second_elem (x::y::xs) = y
fun evenly_positioned_elems (odd::even::xs) = even::evenly_positioned_elems xs
  | evenly_positioned_elems [odd] = []  
  | evenly_positioned_elems []    = [] 

val is_large = (fn x => x > 37)
val add_them = fn (a,b) => a + b
val thermometer =
    fn temp => if temp < 37
               then "Cold"
               else if temp > 37
                    then "Warm"
                    else "Normal"

val some_result = (fn x => thermometer (x - 5) ^ thermometer (x + 5)) 37

