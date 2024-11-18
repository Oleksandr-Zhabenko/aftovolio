# Revision history for aftovolio

## 0.1.0.0 -- 2024-09-21

* First version. Released on an unsuspecting world.

##  0.2.0.0 -- 2024-09-22

* Second version. Added possibility to specify sequence of Word8 numbers as durations to  be compared with in the +ln ... -ln command line arguments group.

##  0.2.1.0 -- 2024-09-28

* Second version revised A. Updated dependencies. Some minor code usability improvements.
 
##  0.3.0.0 -- 2024-10-13

* Third version, Added a new possibility to compare lines using the "+di" command line argument. If present implies the "differentiation" mode of computation for the comparing options with the line in +l2 or +ln groups of command line arguments. Is useful mostly in case of the line to compare with has approximately the same number of syllables as the option lines.

##  0.3.0.1 -- 2024-10-15

* Third version revised A. Fixed some issues with documentation README.md file.

## 0.3.0.2 -- 2024-10-20

* Third version revised B. Some minor code improvements. Added 'Peculiarities of the stressed and unstressed syllables in Ukrainian and other languages' section to the README.md file.

## 0.4.0.0 -- 2024-10-22

* Fourth version. Added new command line parameter "-e" to control whether to print groups of "={digits}" on the screen while making output. Changed the number of arguments in various functions. 

## 0.5.0.0 -- 2024-11-18

* Fifth version. Removed +n command line parameter and the descending sorting of the resulting data. Changed the printing behaviour so that there is no longer too long half of the columns as empty one printed. Some code simplification in the module Aftovolio.Halfsplit. Added new constraint type G for specifying that more than two elements should be groupped together as a one whole (with possible inner permutations inside the group) during all the permutations.

## 0.5.1.0 -- 2024-11-18

* Fifth version revised A. Fixed issue with eternal loop in the halfsplit1G function.

## 0.5.1.0 -- 2024-11-19

* Fifth version revised B. Fixed documentation to reveal the latest changes.

