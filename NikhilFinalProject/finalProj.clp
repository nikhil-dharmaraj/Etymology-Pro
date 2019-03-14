/*
* Author: Nikhil Dharmaraj
* Date of Creation: 12/5/18
* This module attempts to provide the Latin/Greek etymology of any given English word by
* analyzing common roots/prefixes/suffixes in its knowledge base. It serves as my final
* project for Expert Systems 2018-2019 at The Harker School.
* Interviewee: Mr. Clifford Hull
*/

(clear)
(reset)

/*
* SETUP
* The following functions/rules set-up UI and make way for dynamic rule generation later on
*/

(batch toolbox.clp)   ;import toolbox file

/*
* STARTUP
*/
(printout t "WELCOME TO ETYMOLOGY-PRO! Think of any English word. Type it here, and I will try to guess its etymology!")
(printout t " If you enter multiple words separated by spaces, I will only take the first. I will also reject any numerical")
(printout t " values. Also, please only enter words that come from Latin roots; otherwise, I may incorrectly break down")
(printout t " the etymology of your word. Please enjoy...(And, remember, I'm just a mere machine, so I sometimes might be wrong!)")
(printout t crlf)
(printout t crlf)


/*
* This function will validate the response to make sure it meets the criterion --  a one-word string of 
* only letters; if not, it will  prompt the user for a valid response again and again until it gets one.
* It returns out a lowercase version of the string that will be worked with.
*/
(deffunction validate (?param)
   (bind ?response ?param)
   (if (numberp ?response) then   ;makes sure input is not numerical
      (bind ?response (ask "That was an invalid response -- please enter a one-word string with no spaces."))
      (while (numberp ?response) [do]
         (bind ?response (ask "That is still an invalid response -- please enter a one-word string with no space."))
      )
   ) 
   (return (lowcase ?response))
)

(defglobal ?*WORD* = (validate (ask "What is the word you wish to etymologically analyze?")))   ;global variable for word to analyze

/*
* DYNAMIC RULE GENERATION FUNCTIONS
* The following functions build dynamically-generated rules for recognizing various prefixes, suffixes,
* and other part-of-speech roots from Latin.
*/

/*
* This function dynamically generates a rule that catches various prefixes, and if the English word 
* at-hand matches one of them, a fact is asserted. However, the dynamic generation is preconditioned
* on the prefix-length being less than the word-length (for obvious reasons). Here is an example of how 
* one of these dynamically generated rules woud look:
*
* (defrule abPrefix
*    =>
*    (if (= (sub-string 0 2 ?*WORD*) "ab") then (assert (abPrefix yes)))
* )
*/
(deffunction buildPrefixRule (?prefix)
   (bind ?prefLen (str-length ?prefix))
   (bind ?wordLen (str-length ?*WORD*))
   (if (< ?prefLen ?wordLen) then 
      (bind ?code (str-cat "(defrule " ?prefix "Prefix"))
      (bind ?code (str-cat ?code " => (if (= (sub-string 1 " ?prefLen " ?*WORD*) \"" ?prefix "\") then (assert (" ?prefix "Prefix yes))))"))
      (build ?code)
   )
   (return) 
)

/*
* This function dynamically generates a rule that catches various suffixes, and if the English word 
* at-hand matches one of them, a fact is asserted. However, the dynamic generation is preconditioned
* on the suffix-length being less than the word-length (for obvious reasons). Here is an example of how 
* one of these dynamically generated rules woud look:
*
* (defrule torSuffix
*   =>
*   (if (= (sub-string 5 6 ?*WORD*) "ix") then (assert (ixSuffix yes)))
* )
*/
(deffunction buildSuffixRule (?suffix)
   (bind ?suffLen (str-length ?suffix))
   (bind ?wordLen (str-length ?*WORD*))
   (if (< ?suffLen ?wordLen) then 
      (bind ?code (str-cat "(defrule " ?suffix "Suffix"))
      (bind ?code (str-cat ?code " => (if (= (sub-string " (+ (- ?wordLen ?suffLen) 1) " " ?wordLen " ?*WORD*) \"" ?suffix "\") then (assert (" ?suffix "Suffix yes))))"))
      (build ?code)
   )
   (return) 
)

/*
* This function dynamically generates a rule that catches various Latin PoS [part of speech] roots. Since
* a single Latin root word could show up in many forms in English derivatives, the function takes in two parameters,
* namely the Latin root word itself and a list of the possible ways that word shows up in English. To check if the
* English word  at-hand matches one of them, the function cycles through the list with a for-loop, and if one of the ways
* does match, a fact is asserted. Here is an example of how one of these dynamically generated rules woud look:
*
* (defrule ambulareRoot
*   =>
*   (bind ?rootExists FALSE)
*   (foreach ?root (list "ambula" "ambl")
*      (if (numberp (str-index ?root ?*WORD*)) then (bind ?rootExists TRUE))
*   )
*   (if ?rootExists then (assert (ambulareRoot yes)))
* )
*/
(deffunction buildRootRule (?latinWord ?list)
   (bind ?code (str-cat "(defrule " ?latinWord "Root"))
   (bind ?code (str-cat ?code " => (bind ?rootExists FALSE)"))
   (bind ?code (str-cat ?code "(foreach ?root (list " ?list ") (if (numberp (str-index ?root ?*WORD*)) then (bind ?rootExists TRUE)))"))
   (bind ?code (str-cat ?code "(if ?rootExists then (assert (" ?latinWord "Root yes))))"))
   (build ?code)
   (return) 
)


/*
* DYNAMICALLY GENERATED RULE CREATION
* The following function calls generate dynamic rules for each of the most common Latin prefixes, suffixes, and roots
*/

(buildPrefixRule "ab")
(buildPrefixRule "ad")
(buildPrefixRule "ante")
(buildPrefixRule "bi")
(buildPrefixRule "circum")
(buildPrefixRule "contra")
(buildPrefixRule "de")
(buildPrefixRule "e")
(buildPrefixRule "ex")
(buildPrefixRule "in")
(buildPrefixRule "mal")
(buildPrefixRule "per")
(buildPrefixRule "post")
(buildPrefixRule "pre")
(buildPrefixRule "re")
(buildPrefixRule "su")
(buildPrefixRule "trans")
(buildPrefixRule "tri")

(buildSuffixRule "tor")
(buildSuffixRule "ix")
(buildSuffixRule "cide")

(buildRootRule "amare" (create$ "ama" "amor"))
(buildRootRule "ambulare" (create$ "ambula" "ambl"))
(buildRootRule "audire" (create$ "audi"))
(buildRootRule "capere" (create$ "capa" "cip" "cep"))
(buildRootRule "clamare" (create$ "claim" "clama"))
(buildRootRule "credere" (create$ "cred"))
(buildRootRule "currere" (create$ "cur" "cour"))
(buildRootRule "dicere" (create$ "dic"))
(buildRootRule "dormire" (create$ "dorm"))
(buildRootRule "ducere" (create$ "duc"))
(buildRootRule "ferre" (create$ "fer" "lat"))
(buildRootRule "gerere" (create$ "ger" "gest"))
(buildRootRule "imponere" (create$ "impon"))
(buildRootRule "laborare" (create$ "labor"))
(buildRootRule "legere" (create$ "lect" "lig" "leg"))
(buildRootRule "loqui" (create$ "locu" "loq"))
(buildRootRule "mittere" (create$ "mis" "mit"))
(buildRootRule "movere" (create$ "mot"))
(buildRootRule "ponere" (create$ "pos" "pon" "poun"))
(buildRootRule "pugnare" (create$ "pugn" "pug"))
(buildRootRule "linquere" (create$ "lict" "linq" "liq"))
(buildRootRule "scribere" (create$ "scrib" "script"))
(buildRootRule "venire" (create$ "vent" "ven"))
(buildRootRule "videre" (create$ "vid" "vis"))

(buildRootRule "bonus" (create$ "bon"))
(buildRootRule "candidus" (create$ "candid"))
(buildRootRule "celer" (create$ "celer"))
(buildRootRule "centum" (create$ "cent"))
(buildRootRule "aequus" (create$ "equi" "equa"))
(buildRootRule "gravis" (create$ "grav" "grie"))
(buildRootRule "magnus" (create$ "magn"))
(buildRootRule "rectus" (create$ "rect"))
(buildRootRule "sanus" (create$ "san"))
(buildRootRule "sanus" (create$ "san"))

(buildRootRule "amicus" (create$ "ami"))
(buildRootRule "animus" (create$ "anim"))
(buildRootRule "annus" (create$ "ann" "enn"))
(buildRootRule "aqua" (create$ "aque"))
(buildRootRule "ars" (create$ "art"))
(buildRootRule "aster" (create$ "aster"))
(buildRootRule "avis" (create$ "avia"))
(buildRootRule "bellum" (create$ "bell"))
(buildRootRule "canis" (create$ "cani"))
(buildRootRule "caput" (create$ "capit"))
(buildRootRule "caro" (create$ "carn"))
(buildRootRule "civis" (create$ "civi"))
(buildRootRule "corpus" (create$ "corp"))
(buildRootRule "equus" (create$ "eque"))
(buildRootRule "flamma" (create$ "flamm" "flam"))
(buildRootRule "gladius" (create$ "gladi"))
(buildRootRule "limen" (create$ "limin"))
(buildRootRule "lumen" (create$ "lum"))
(buildRootRule "luna" (create$ "luna"))
(buildRootRule "nox" (create$ "nox" "noct"))
(buildRootRule "panis" (create$ "pan" "pani"))
(buildRootRule "plumbum" (create$ "plum"))
(buildRootRule "sal" (create$ "sal" "sau"))
(buildRootRule "senex" (create$ "sen"))

/*
* ETYMOLOGY RULE GENERATION FUNCTIONS
* The following functions dynamically generate rules for each root in the knowledge base.
* The LHS is always a pattern match for that root's presence, and the right hand side 
* always adds the breakdown of that root to a global list.
*/

/*
* This function dynamically generates rules for each of the prefixes. These rules will pattern-match
* and, if successful, add the breakdown of that prefix to a global list (at start). Here is an example
* of how one of these dynamically generated rules would look: 
* 
* (defrule ab
*    (abPrefix yes)
*    =>
*    (bind ?*ETYM* (insert$ ?*ETYM* 1 "A/AB (AWAY FROM)")) 
* )
*/
(deffunction buildPrefixEtymRule (?prefix ?breakdown)
   (bind ?code (str-cat "(defrule " ?prefix " (" ?prefix "Prefix yes)"))
   (bind ?code (str-cat ?code " => (bind ?*ETYM* (insert$ ?*ETYM* 1 \"" ?breakdown "\")))"))
   (build ?code)
   (return)
)

/*
* This function dynamically generates rules for each of the suffixes. These rules will pattern-match
* and, if successful, add the breakdown of that suffix to a global list (at end). The salience is low
* to ensure that suffix elements truly get added to the very very end. Here is an example of how one of these 
* dynamically generated rules would look: 
* 
* (defrule tor (declare (salience -25))
*    (torSuffix yes)
*    =>
*    (bind ?*ETYM* (insert$ ?*ETYM* (+ 1 (length$ ?*ETYM*)) "TOR (MALE PERSON)")) 
* )
*/
(deffunction buildSuffixEtymRule (?suffix ?breakdown)
   (bind ?code (str-cat "(defrule " ?suffix " (declare (salience -25)) (" ?suffix "Suffix yes)"))
   (bind ?code (str-cat ?code " => (bind ?*ETYM* (insert$ ?*ETYM* (+ 1 (length$ ?*ETYM*)) \"" ?breakdown "\")))"))
   (build ?code)
   (return)
)

/*
* This function dynamically generates rules for each of the roots. These rules will pattern-match
* and, if successful, add the breakdown of that root to a global list at the end. Here is an example
* of how one of these dynamically generated rules would look: 
* 
* (defrule amare
*    (amareRoot yes)
*    =>
*    (bind ?*ETYM* (insert$ ?*ETYM* (+ 1 (length$ ?*ETYM*))) "AMARE (TO LOVE)")) 
* )
*/
(deffunction buildRootEtymRule (?root ?breakdown)
   (bind ?code (str-cat "(defrule " ?root " (" ?root "Root yes)"))
   (bind ?code (str-cat ?code " => (bind ?*ETYM* (insert$ ?*ETYM* (+ 1 (length$ ?*ETYM*)) \"" ?breakdown "\")))"))
   (build ?code)
   (return)
)

/*
* ETYMOLOGY RETURN RULES
* Each of the following rules is tied to a certain Latin prefix/suffix/root. The ones that
* have a pattern match with their associated fact (indicating that the root at-hand is present
* in the word) will then fire and spit out the etymology of that root.
*/

(defglobal ?*ETYM* = (create$))   ;list of etymologies that will, together, comprise final word breakdown

(buildPrefixEtymRule "ab" "A/AB (AWAY FROM)")
(buildPrefixEtymRule "ad" "AD (TO)")
(buildPrefixEtymRule "ante" "ANTE (BEFORE)")
(buildPrefixEtymRule "circum" "CIRCUM (AROUND)")
(buildPrefixEtymRule "contra" "CONTRA (AGAINST)")
(buildPrefixEtymRule "de" "DE (FROM)")
(buildPrefixEtymRule "e" "E (FROM)")
(buildPrefixEtymRule "ex" "EX (FROM)")
(buildPrefixEtymRule "in" "IN (IN/AGAINST/NOT)")
(buildPrefixEtymRule "mal" "MALUS (BAD)")
(buildPrefixEtymRule "per" "PER (THROUGH)")
(buildPrefixEtymRule "post" "POST (AFTER)")
(buildPrefixEtymRule "pre" "PRE (BEFORE)")
(buildPrefixEtymRule "re" "RE (AGAIN)")
(buildPrefixEtymRule "su" "SUUS (ONE'S OWN)")
(buildPrefixEtymRule "trans" "TRANS (ACROSS)")
(buildPrefixEtymRule "tri" "TRI (THREE)")

(buildSuffixEtymRule "tor" "-TOR (MALE PERSON)")
(buildSuffixEtymRule "ix" "-IX (FEMALE PERSON)")
(buildSuffixEtymRule "cide" "CAEDERE (TO KILL)")

(buildRootEtymRule "amare" "AMARE (TO LOVE)")
(buildRootEtymRule "ambulare" "AMBULARE (TO WALK)")
(buildRootEtymRule "audire" "AUDIRE (TO HEAR)")
(buildRootEtymRule "capere" "CAPERE (TO SEIZE)")
(buildRootEtymRule "clamare" "CLAMARE (TO PROCLAIM)")
(buildRootEtymRule "credere" "CREDERE (TO TRUST/BELIEVE)")
(buildRootEtymRule "currere" "CURRERE (TO RUN)")
(buildRootEtymRule "dicere" "DICERE (TO SAY)")
(buildRootEtymRule "dormire" "DORMIRE (TO SLEEP)")
(buildRootEtymRule "ducere" "DUCERE (TO LEAD)")
(buildRootEtymRule "ferre" "FERRE (TO CARRY)")
(buildRootEtymRule "gerere" "GERERE (TO WEAR)")
(buildRootEtymRule "imponere" "IMPONERE (TO IMPOSE/PUT UPON)")
(buildRootEtymRule "laborare" "LABORARE (TO WORK/LABOR)")
(buildRootEtymRule "legere" "LEGERE (TO READ)")
(buildRootEtymRule "loqui" "LOQUI (TO SPEAK)")
(buildRootEtymRule "mittere" "MITTERE (TO SEND)")
(buildRootEtymRule "movere" "MOVERE (TO MOVE)")
(buildRootEtymRule "ponere" "PONERE (TO PLACE/PUT)")
(buildRootEtymRule "pugnare" "PUGNARE (TO FIGHT)")
(buildRootEtymRule "linquere" "LINQUERE (TO LEAVE/QUIT)")
(buildRootEtymRule "scribere" "SCRIBERE (TO WRITE)")
(buildRootEtymRule "venire" "VENIRE (TO COME)")
(buildRootEtymRule "videre" "VIDERE (TO WATCH/LOOK AT)")

(buildRootEtymRule "bonus" "BONUS (GOOD)")
(buildRootEtymRule "candidus" "CANDIDUS (BRIGHT/CLEAR)")
(buildRootEtymRule "celer" "CELER (QUICK)")
(buildRootEtymRule "centum" "CENTUM (ONE HUNDRED)")
(buildRootEtymRule "aequus" "AEQUUS (EQUAL)")
(buildRootEtymRule "gravis" "GRAVIS (HEAVY/GRAVE)")
(buildRootEtymRule "magnus" "MAGNUS (LARGE)")
(buildRootEtymRule "rectus" "RECTUS (RIGHT/PROPER)")
(buildRootEtymRule "sanus" "SANUS (SOUND/HEALTHY)")

(buildRootEtymRule "amicus" "AMICUS (FRIEND)")
(buildRootEtymRule "animus" "ANIMUS (SOUL/SPIRIT/MIND)")
(buildRootEtymRule "annus" "ANNUS (YEAR)")
(buildRootEtymRule "aqua" "AQUA (WATER)")
(buildRootEtymRule "ars" "ARS (ART/SKILL)")
(buildRootEtymRule "aster" "ASTER (STAR)")
(buildRootEtymRule "avis" "AVIS (BIRD)")
(buildRootEtymRule "bellum" "BELLUM (WAR)")
(buildRootEtymRule "canis" "CANIS (DOG)")
(buildRootEtymRule "caput" "CAPUT (HEAD)")
(buildRootEtymRule "caro" "CARO (MEAT/FLESH)")
(buildRootEtymRule "civis" "CIVIS (CITIZEN)")
(buildRootEtymRule "corpus" "CORPUS (BODY)")
(buildRootEtymRule "equus" "EQUUS (HORSE)")
(buildRootEtymRule "flamma" "FLAMMA (FLAME)")
(buildRootEtymRule "gladius" "GLADIUS (SWORD)")
(buildRootEtymRule "limen" "LIMEN (BOUNDARY/THRESHOLD)")
(buildRootEtymRule "lumen" "LUMEN (LIGHT)")
(buildRootEtymRule "luna" "LUNA (MOON)")
(buildRootEtymRule "nox" "NOX (NIGHT)")
(buildRootEtymRule "panis" "PANIS (BREAD)")
(buildRootEtymRule "plumbum" "PLUMBUM (LEAD)")
(buildRootEtymRule "sal" "SAL (SALT)")
(buildRootEtymRule "senex" "SENEX (OLD MAN)")

/*
* This rule wraps up the program by printing out the etymology breakdown OR returning an apology error message.
*/
(defrule finish (declare (salience -50))
   =>
   (printout t crlf)
   (bind ?etymLen (length$ ?*ETYM*))
   (bind ?lastElement (nth$ ?etymLen ?*ETYM*))
   (foreach ?i ?*ETYM* 
      (if (not (= ?i ?lastElement)) 
       then (printout t (str-cat ?i " + ")) ;prints out root at-hand followed by a + sign
       else (printout t (str-cat ?i))       ;prints out root at-hand with no + sign (last element in list)
      )
   )
   (if (= ?etymLen 0) then (printout t "Sorry! Either that isn't a Latin-derived word, or it's not yet in my knowledge base!"))
   (printout t crlf crlf)
)

(run)
