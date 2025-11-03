       IDENTIFICATION DIVISION.
       PROGRAM-ID. MKTEMP.
      ******************************************************
      * VERYANT - APR/02/2013 
      *
      * 2024.0126 BETH & JAY
      *   VIP TESTING WAS GETTING OPEN 93 WHICH WE FELT WERE
      *     CONFLICTS ON GETTING A UNIQUE MKTEMP FILE NAME!
      *   THIS PROGRAM USED TO ONLY START AT LOWER CASE "M" FOR FILENAMES
      *   WE REMOVED THE USE OF TIME, AND USE THE RANDOM FUNCTION WITH
      *     NO ARGUMENT SO IT IS A UNIQUE RANDOM NUMBER
      *   WE INCREASED RAND-NUM TO 9(12) FROM 9(10) TO GAIN MORE UNIQUENESS
      *   CHANGED IT TO USE THE LAST 6 BYTES INSTEAD OF THE FIRST
      *   ADDED MORE UPPER/LOWER/NUMBER CHANGES
      *   
      *
      * CONVERTED FROM A C ROUTINE
      * ROUTINE TO GENERATE A TEMPORARY RANDOM FILE NAME
      *
      * CHANGE THE FIXED-SIZE TO 0 TO USE THE ACTUAL SIZE
      * OF THE FILE NAME TAG XXXXXX FOR THE GENERATED TEMP
      * NAME
      *
      * CHANGE THE FIXED-SIZE TO A VALUE > 0 TO OVERRIDE
      * THE ACTUAL SIZE OF THE FILE NAME TAG FOR THE
      * GENERATED TEMP NAME
      *
      * MAXIMUM VALUE FOR FIXED-SIZE IS 10
      *
      * THE FNAME-TAG AND FNAME-TAG-SIZE DECIDE THE MINIMUM
      * TAG SIZE TO BE SEARCHED IN THE TEMPLATE COMING AS
      * INPUT
      *
      * THE FNAME-TAG CHARACTERS CAN ONLY BE UPPERCASE 'X'
      * CHARACTERS
      *
      *
      * MJD 130404 SET STAT TO "00" UPON EXIT.
      * MJD 130425 CHANGED FIXED SIZE TO 6 CHARACTERS (WAS 10)
      *            ADDED A CONVERSION TO LOWER CASE.
      * MJD 141024 MAKING CORRECTIONS FOR PROGRAM ENDING IN "X"
      *            ALTERED THE PROGRSAM TO SEACH MKTEMP-BUF FROM BACK TO FRONT
      *            WHEN LOOKING FOR THE "XXXXXX" RATHER THAN FROM FRONT TO BACK
      *            THIS WILL ALLOW PROGRAM NAMES ENDING IN "X" (I.E. GLMEX")
      *            TO WORK PROPERLY.
      * BAH 2024.0126 CHANGED RAND-NUM TO KEEP 12 DIGITS. USE LAST 6 DIGITS, NOT
      *               THE FIRST 6.
      ******************************************************

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)GB/MKTEMP.CBL S35 02/21/24-08:02:07 >".
       78  HUNDRED-BILLION            VALUE 100000000000.
       78  FNAME-TAG                  VALUE "XXXXXX".
       78  FNAME-TAG-SIZE             VALUE 6.
       78  FIXED-SIZE                 VALUE 6.
      *78  FIXED-SIZE                 VALUE 10.
      *78  FIXED-SIZE                 VALUE 0.

       01  SUB                        PIC 99.
       01  MESS PIC X.
       01  RAND-NUM                   PIC 9(12).
       01  RAND-DIGITS REDEFINES RAND-NUM.
           05 R-DIGIT                 PIC 9 OCCURS 12 TIMES.
       01  WS-TMPNAME                 PIC X(12).
       01  WS-TMPNAMEY REDEFINES WS-TMPNAME.
           05 WS-TMP-BYTE             PIC X OCCURS 12 TIMES.
       
       77  ORD-CHAR                   PIC 9(3).
       77  ONE-CHAR                   PIC X.
       77  IND                        PIC 9(2).
       77  IND01                      PIC 9(2).
       77  IND02                      PIC 9(2).
       77  FNAME-TAG-POS              PIC S9(2).
       77  FNAME-SUFFIX-POS           PIC S9(2).
       77  FNAME-PREFIX               PIC X(35).
       77  FNAME-SUFFIX               PIC X(35).
       77  TAG-SIZE                   PIC 9(2).

       LINKAGE SECTION.
       01 STAT                        PIC X(2).
       01 MKTEMP-BUF                  PIC X(35).

       PROCEDURE DIVISION USING STAT MKTEMP-BUF.
       MAIN-PROGRAM SECTION.

           PERFORM SPLIT-ARG.
           PERFORM DO-MKTEMP.
           PERFORM FINISH-MKTEMP.

      ***************************************
       SPLIT-ARG SECTION.
      ***************************************
           MOVE ZERO   TO FNAME-TAG-POS FNAME-SUFFIX-POS.
           MOVE SPACES TO FNAME-SUFFIX FNAME-PREFIX.

      *************************************************************************
      * NEED TO FIND THE END POSITION OF THE STRING THEN MOVE BACK 5
      *  TO FIND THE START OF THE X'S FROM MKTEMP-DEFAULT
      *
      * FIND NUMBER OF TRAILING SPACES AND SUBTRACT FROM TOTAL STRING LENGTH
      *
      * FOR EXAMPLE "/USR/TMP/LNAGEDXXXXXX               "
      *
      *  STRING TRAILING SPACED = 14 SO STRING LENGTH = 21
      *  POSITION 21 IS LAST POSITION SO MOVE BACK ANOTHER 5 POSITIONS TO
      *   FIND FIRST POSITION OF "XXXXXX" STRING
      *************************************************************************
           MOVE 0 TO IND01
           INSPECT MKTEMP-BUF TALLYING IND01 FOR TRAILING SPACES
           COMPUTE IND02 = 35 - (IND01 + 5).
           IF IND02 < 6
              PERFORM FINISH-MKTEMP-ERR
           END-IF.
            
      *************************************************************************
      * SEARCH FOR PATTERN FROM END OF THE STRING MOVING BACK TOWARDS THE FRONT
      * THIS WILL ENSURE WE GRAB ONLY TRAILING "XXXXXX" AND NOT AN
      *  "X" AT THE END OF THE PROGRAM NAME.
      *************************************************************************
           PERFORM VARYING IND FROM IND02 BY -1 UNTIL IND <= 0
              IF MKTEMP-BUF(IND:FNAME-TAG-SIZE) = FNAME-TAG
                 MOVE IND TO FNAME-TAG-POS
                 EXIT PERFORM
              END-IF
           END-PERFORM.

           IF FNAME-TAG-POS > 0
              PERFORM VARYING IND FROM FNAME-TAG-POS BY 1 UNTIL IND > 35
                 IF MKTEMP-BUF(IND:1) NOT = "X"
                    MOVE IND TO FNAME-SUFFIX-POS
                    EXIT PERFORM
                 END-IF
              END-PERFORM
           END-IF.

           ADD -1 TO FNAME-TAG-POS.

           IF FNAME-TAG-POS < 1
              PERFORM FINISH-MKTEMP.

           MOVE MKTEMP-BUF(1:FNAME-TAG-POS) TO FNAME-PREFIX.

           IF FNAME-SUFFIX-POS > 0 AND FNAME-SUFFIX-POS < 35
              MOVE MKTEMP-BUF(FNAME-SUFFIX-POS:) TO FNAME-SUFFIX.

           IF FIXED-SIZE = 0
              COMPUTE TAG-SIZE = FNAME-SUFFIX-POS - FNAME-TAG-POS - 1
           ELSE
              MOVE FIXED-SIZE TO TAG-SIZE.

      ***************************************
       DO-MKTEMP SECTION.
      ***************************************

      * RAND-NUM IS 9(12)
      * FUNCTION RANDOM WITHOUT ANY ARGUMENT (), BRINGS BACK A TOTAL
      *  RANDOM NUMBER  
      * WE REMOVED WS-TIME1 AND REMOVED THE WS-TIME1 AS AN ARGUMENT TO
      *  THE RANDOM FUNCTION
 
           COMPUTE RAND-NUM = HUNDRED-BILLION *
                              (1 / FUNCTION RANDOM() ).

           PERFORM CONVERT-TO-LETTERS.

           INITIALIZE MKTEMP-BUF.

      * TESTING FIRST 6 BYTES AND COMPARING TO THE LAST 6 BYTES TO LEAVE
      *  SOME RESULTS AS NUMBERS, SOME AS UPPER CASE LETTER AND SOME LOWER.
      * WE FELT THIS GAVE MORE UNIQUE RESULTS
      * R-DIGITS IS THE ORIGINAL RANDOM 12 DIGIT NUMBER
      * WS-TMPNAME AT THIS POINT IS ALL UPPER, ONLY DO LOWER FOR CERTAIN VALUES

      * THIS GIVES US A GREAT UNIQUE RESULT WITH NUMBERS, UPPER AND LOWER
      * NOT BASED ON SOMEONES "TIME" BUT RATHER A UNIQUE RANDOM NUMBER FROM
      * THE FUNCTION, AND THEN WE COMPARE FIRST 6 WITH LAST 6 AND TRY TO 
      * MAKE IT EVEN MORE UNIQUE.

      * IF ANY OF THE FIRST 6 DIGITS = THE SAME POSITION IN THE LAST 6;
      *   WE PUT IT BACK TO THE RANDOM DIGIT
      * IF ANY OF THE FIRST 6 DIGITS ARE ODD; WE CONVERT THAT POSITION TO LOWER
      * ANYTHING LEFT STAYS UPPER CASE

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
              IF R-DIGIT(SUB) = R-DIGIT(SUB + 6)
                 MOVE R-DIGIT(SUB + 6) TO WS-TMP-BYTE(SUB + 6)
              ELSE
                 IF R-DIGIT(SUB) = 1 OR 3 OR 5 OR 7 OR 9
                   CALL "C$TOLOWER" USING WS-TMP-BYTE(SUB + 6), VALUE 1
                 END-IF
              END-IF
           END-PERFORM.
 
      * TAKING LAST 6 POSITIONS OF WS-TMPNAME FOR MKTEMP-BUF

           STRING FNAME-PREFIX DELIMITED BY SPACE
                  WS-TMPNAME(7:TAG-SIZE) DELIMITED BY SIZE
                  FNAME-SUFFIX DELIMITED BY SPACE
             INTO MKTEMP-BUF
           END-STRING.

      ****************************************
       CONVERT-TO-LETTERS SECTION.
      ****************************************

      * TRYING TO GET THE BEST UNIQUE COMBINATION

      * TESTING EACH BYTE OF THE RANDOM NUMBER AND EITHER CONVERTING
      *  THAT OUTPUT BYTE TO BE IN THE "A" TO "J" (0-9) FOR ODD NUMBERS
      *   OR THE "M" TO "V" RANGE (0-9) FOR EVENS. THIS IS DONE BY THAT
      *   ARGUMENT IN THE *  FUNCTION ORD ( )
      * WE FELT THIS GAVE MORE UNIQUE RESULTS

           PERFORM VARYING IND FROM 1 BY 1 UNTIL IND > 12
              IF R-DIGIT(IND) = 1 OR 3 OR 5 OR 7 OR 9
                 COMPUTE ORD-CHAR = R-DIGIT(IND) + FUNCTION ORD("A")
              ELSE
                 COMPUTE ORD-CHAR = R-DIGIT(IND) + FUNCTION ORD("M")
              END-IF
              MOVE FUNCTION CHAR(ORD-CHAR) TO ONE-CHAR
              MOVE ONE-CHAR TO WS-TMPNAME(IND:1)
           END-PERFORM.

      *******************************************
       FINISH-MKTEMP-ERR SECTION.
      *******************************************
           MOVE "99" TO STAT.
           GOBACK.

      *******************************************
       FINISH-MKTEMP SECTION.
      *******************************************
           MOVE "00" TO STAT.
           GOBACK.

