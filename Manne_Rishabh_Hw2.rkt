#|
Author: Rishabh Manne
Date: 2/21/22
Description: Using the functional paradigm to implement Scheme programming
             and use the main concepts/commands that it has to offer such as defining functions,recursion, and problem-solving operations
|#



#|
PART I
|#


(define run1 (lambda () (+ 4 5 6)))   ; using lambda as a shortcut to define run1 as a function, with no parameters that adds
                                      ; 4,5 and 6 by having the operator in front of the operands





(define run2 (lambda () (/(* 5(+ 4 5))2)))   ; using nested parentheses, I added the 4 and 5 and after its closing parentheses, I
                                             ; multiplied the sum with 5, before closing the next parentheses and placing the 2 on the right as it is the divisor





(define run3 (lambda () (+(- 100(*(/ 20 5)(* 3 3)))(/ 120 10))))  ; similar concept as the run2 method, however, there were more operators and I made sure to include the 100
                                                                  ; in front of the compound expression so that it would be the value that is subtracted from





(define run4 (lambda () (* 5(+ 4(/(+(+ 10 10)(* 5 8))(+ 10 2))))))  ; like the previous two functions, I started from the inner-most expression and branched outwards to cover the whole
                                                                    ; compound expression using the lambda shortcut





(define run5 (lambda () (+(/(- (/(/ (* (+ 3 5)(+ 6 4))2)2)5)3)(+ (/(+ (* 2 10)(* 5 4))2)(* 4 5)))))   ; in this case, I treated the two big compound statements as two operands and through careful nesting
                                                                                                      ; I added both at once at the end



#|
PART II
|#




(define how-much-damage-ff6? (lambda (damage defense) (floor(+(/(*(- 255 defense)damage)256)1))))    ; calculating the damage by using the defense value and and taking the floor value of
                                                                                                     ; the entire operation to round down






(define base-attack-ff7 (lambda (strength weapon-bonus) (+ strength weapon-bonus)))       ; calculating the base attack by using the strength/weapon bonus value and simply adding them
                                                                                          






(define base-damage-ff7 (lambda (level attack) (+ attack(*(floor(/(+ attack level)32))(floor(/(* attack level)32))))))      ; calculating the base damage using the level and magnitude of attack
                                                                                                                            ; by taking the floor of the level/attack operations and adding it to the attack value





(define calculate-power-ff7 (lambda (power base-damage) (* power (/ base-damage 16))))         ; calculating the by taking into account the current base damage and power and performing a simple
                                                                                               ; compound expression in the lambda format






(define how-much-damage-ff7? (lambda (defense power) (floor(* power(/(- 512 defense) 512)))))     ; calculating the damage by keeping in mind the defense and power done to the current base and flooring the entire compound expression







(define calculate-damage-ff7 (lambda (level strength weapon-bonus power defense)              ; called the previous methods which called their previous method to eventually calculate the total damage given the parameters, which required
                               (                                                              ; careful nesting of the functions and the arguments they needed

                               how-much-damage-ff7? defense (calculate-power-ff7 power (base-damage-ff7 level (base-attack-ff7 strength weapon-bonus)))

                              )
                            )
  )
                            
  
                              
                    
  

#|
PART III
|#





(define get-second-item (lambda (list) (            ; this function obtains the second item of an user-inputted list by using the cdr command to get every item besides the first item in the list
                                                    ; and then using the car command to obtain the second item which would be the first in the new list
                           car(cdr list)
                      )
                )                            ; cdr list returns every element but the first, and then the car of that new list would be the second element of the original
        )








(define get-third-item (lambda (list)

                   (                          ; same concept as last function however using the cdr command one more time to reach the third item and beyond of the list

                    car(cdr(cdr list))
                             
                   )
                )
        )








(define helper-counter (lambda (list count) 

         (if (null? list)                                        ; a helper function that recursively cuts down on the list using the cdr command and added a count argument to keep track
                                                                 ; of how many times we "cdr", so we can obtain the size of the list, and this continues until list is empty(base case)
             count
             
             (helper-counter (cdr list) (+ 1 count)))
             
 
                                                        ; if list is null(reached end of list), then we simply return the count accumulator variable, else we make the recursive call
                                                        ; and get closer to base case by moving past first element of list each time and increasing the accumulator variable by 1
                      
       )
  )
                       






(define list-length? (lambda (list)
               (

               helper-counter list 0                  ; the interface function for user, which uses the helper-counter method to do the recursive function and obtain size of 
                                                      ; list that an user inputs
               )
           )
     )







(define position-helper (lambda (list location count)

               (if (= count (- location 1))
                                                                                    ; this helper function calculates the location of the parsing of the list by using a counter variable and 
                   list                                                             ; will "cdr" until the location is 1 less than the user inputted position so that we still grab that value at that particular index
                                                                                    
                   (position-helper (cdr list) location (+ 1 count)))
                   
             )                                                               ; since count is starting at 0, and we want to obtain the element at the index that the user enters, we see when count equals location - 1
  )                                                                          ; (base case) and return list, otherwise we make a recursive call by moving past first element of list each time and incrementing (current pos) count var
                                                                            





(define arbitrary-cdr (lambda (pos list) 

               (if ( > pos (list-length? list))
                                                                            ; this is the user interface function that calls the helper method above, however, if the user inputs a location that exceeds the bounds of 
                   #f                                                       ; the list, then it outputs a '#f' to the window signalling an error

                   (position-helper list pos 0)
               )
         )                                      ; calling the helper function with user-inputted parameters
  )

                   





(define creator-helper (lambda (size value counter lst)           ; helper function for the function below and populates a list based on user input
                         
            (define updater (list value))                       ; turning the desired value(user input) into a list so that it can be placed in an "append" function with the newly created list for user
         
                       
            (if (>= counter size)                               ; if the size of the list is satisfied, then the list is simply returned

               lst
               
               
              (creator-helper size value (+ 1 counter) (append lst updater))          ; otherwise in the recursive call, the counter to keep track of current position is updated by 1 and the desired value is appended into the new list
                                                                                      ; for the user
              

           )         

       )
   )
 

                 



(define make-list (lambda (size value)                              ; user interface function that calls the helper function above, which does the majority of the implementation

              (define lst '())                            ; creating an empty list that we will eventually populate
                   
              (if (number? size)        

                  (creator-helper size value 0 lst)            ; as long as the first parameter given is a number, the helper function above is called with all the user inputs and a current pos counter var


                  lst                       ; if the first parameter is not a number, the empty list is simply returned
               )
            )
  )

                 



#|
PART IV
|#
                  


(define traversal-helper (lambda (list counter)                        ; helper function for the function below that verifies if all elements of the list are numbers

                (if (null? list)                  ; if the end of the list is reached with no other elements besides numbers, #t symbolizing true is returned
                    
                    #t
           
                     
     
                    (if (number? (car list))                            ; car list returns the first element of list, and this if statement checks whether or not it is a numerical value
                    
                       (traversal-helper (cdr list) (+ 1 counter))       ; if it is then recursive call is made and we move past the first element each time(closer to base case) and add one more to current pos count variable

                    
                        #f                                           ; if it is not a variable, we immediately return a #f, which signifies false
                    )

                )
            )
  )
                    
                   



(define number-list? (lambda (list)
             (

              traversal-helper list 0             ; user interface function that simply calls helperr function above and passes in user-inputted list and a current pos variable

            )
        )
  )
                  




(define adder-helper (lambda (list sum)                       ; a helper function for function below that takes the sum of all elements in list, only if they are numbers

                 (if (null? list)                                  ; if the end of the list is reached, the sum is returned

                     sum

                     (adder-helper (cdr list) (+ sum (car list)))          ; otherwise recursive call is made and we move past the first element and onto the next, while adding the
                                                                                         ; current "top" element of the list into our sum variable
                 )
       )
  )




(define sum-number-list (lambda (list)          ; user interface function that makes use of helper function above

              (if (number-list? list)

                  (adder-helper list 0)                  ; if the list is all numbers(used previously made function), then the helper function is called with the user-inputted list and a default sum variable


                 #f                                ; else a #f is returned, symbolizing that the list does not have all numbers, and thus, a sum cannot be computed

              )
         )
  )


               
               
               

                          

                            