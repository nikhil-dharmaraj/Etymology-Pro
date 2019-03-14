/*
* Author: Nikhil Dharmaraj
* Date of Creation: 9/6/18
* This module serves as a toolbox for generic functions that can be used in multiple circumstances
*/

/*
* This function asks the user a prompt and returns the typed response -- it functions
* as a general ask method
*/
(deffunction ask (?prompt)      ;get user input w/ a prompt
   (printout t ?prompt " ")
   (bind ?response (read))
   (return ?response)
)

/*
* This function asks the user a prompt and appends a question mark, then returning the response
*/
(deffunction askQuestion (?prompt)      ;get user input w/ a prompt
   (return (ask (str-cat ?prompt "?")))
)

/*
* This function asks the user a prompt of the form "Do you know the value of..."
*/
(deffunction askValue (?var)      ;get var for which value is being asked
   (return (askQuestion (str-cat "Do you know the value of " ?var)))
)

/*
* This method slices a string into a character array using a for-loop
*/
(deffunction slice$ (?str)
   (bind ?len (str-length ?str))
   (bind ?list (create$))
   (for (bind ?i 1) (<= ?i ?len) (++ ?i)                      ;iterate through characters of string
      (bind ?substr (sub-string ?i ?i ?str))
      (bind ?currListLen (length$ ?list))
      (bind ?list (insert$ ?list (++ ?currListLen) ?substr))  ;append character at end of list
   )
   (return ?list)
)