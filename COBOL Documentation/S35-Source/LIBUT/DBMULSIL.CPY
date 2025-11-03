      ******************************************************************
      *
      * IDENTIFICATION: LIBUT/DBMULSIL
      *
      * DESCRIPTION   : DEBUG MULSIL VARIABLES
      *                 USED TO GET MULSIL VARIABLES AND LOAD THEM
      *                 INTO A OUTPUT FILE.
      *
      * OUTPUT FILE   : /USR/TMP/MULSIL-YYMMDD-HHMMSS
      *
      *=================================================================
      * REV:
      * JKC 2022-0614 ***NEW***
      *
      ******************************************************************
      *
      *   D B M U L S I L - R O U T I N E
      *
      ******************************************************************
       DBMULSIL-ROUTINE SECTION.

      *    STOP DBMULSIL-ECHO-TEXT.
           IF ( DBMULSIL-FIRST-TIME = "T" )      *> TESTING FUNCTION
              PERFORM DBMULSIL-INITIALIZATION
              PERFORM DBMULSIL-TEST-SETTINGS
           ELSE
           IF ( DBMULSIL-FIRST-TIME = "Y" )
              PERFORM DBMULSIL-INITIALIZATION.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    PERFORM VARYING DBMULSIL-SUB FROM 1 BY 1
      *              UNTIL DBMULSIL-SUB > 9

      *         MOVE SPACES                TO DBMULSIL-DETAIL-PROMPT
      *         STRING "DEC(", DBMULSIL-SUB,")"
      *                DELIMITED BY "  " INTO DBMULSIL-DETAIL-PROMPT
      *         MOVE DEC(DBMULSIL-SUB)     TO DBMULSIL-DETAIL-FLOAT
      *         PERFORM DBMULSIL-WRITE-RECORD

      *    END-PERFORM.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    MOVE "I-T"                      TO DBMULSIL-DETAIL-PROMPT.
      *    MOVE I-T                        TO DBMULSIL-DETAIL-DOUBLE.
      *    PERFORM DBMULSIL-WRITE-RECORD.

      *    MOVE "I-C"                      TO DBMULSIL-DETAIL-PROMPT.
      *    MOVE I-C                        TO DBMULSIL-DETAIL-DOUBLE.
      *    PERFORM DBMULSIL-WRITE-RECORD.

      *    MOVE "I-B"                      TO DBMULSIL-DETAIL-PROMPT.
      *    MOVE I-B                        TO DBMULSIL-DETAIL-DOUBLE.
      *    PERFORM DBMULSIL-WRITE-RECORD.

      *    MOVE "I-P"                      TO DBMULSIL-DETAIL-PROMPT.
      *    MOVE I-P                        TO DBMULSIL-DETAIL-DOUBLE.
      *    PERFORM DBMULSIL-WRITE-RECORD.

      *    MOVE "I-REGP"                   TO DBMULSIL-DETAIL-PROMPT.
      *    MOVE I-REGP                     TO DBMULSIL-DETAIL-DOUBLE.
      *    PERFORM DBMULSIL-WRITE-RECORD.

      *    MOVE "I-NOR"                    TO DBMULSIL-DETAIL-PROMPT.
      *    MOVE I-NOR                      TO DBMULSIL-DETAIL-DOUBLE.
      *    PERFORM DBMULSIL-WRITE-RECORD.

      *    MOVE "I-R(1)"                   TO DBMULSIL-DETAIL-PROMPT.
      *    MOVE I-R(1)                     TO DBMULSIL-DETAIL-DOUBLE.
      *    PERFORM DBMULSIL-WRITE-RECORD.

      *    MOVE "I-R(2)"                   TO DBMULSIL-DETAIL-PROMPT.
      *    MOVE I-R(2)                     TO DBMULSIL-DETAIL-DOUBLE.
      *    PERFORM DBMULSIL-WRITE-RECORD.

      *    MOVE "I-R(3)"                   TO DBMULSIL-DETAIL-PROMPT.
      *    MOVE I-R(3)                     TO DBMULSIL-DETAIL-DOUBLE.
      *    PERFORM DBMULSIL-WRITE-RECORD.

      *    MOVE "I-R(4)"                   TO DBMULSIL-DETAIL-PROMPT.
      *    MOVE I-R(4)                     TO DBMULSIL-DETAIL-DOUBLE.
      *    PERFORM DBMULSIL-WRITE-RECORD.

      *    MOVE "I-R(5)"                   TO DBMULSIL-DETAIL-PROMPT.
      *    MOVE I-R(5)                     TO DBMULSIL-DETAIL-DOUBLE.
      *    PERFORM DBMULSIL-WRITE-RECORD.

      *    MOVE "I-BRK(1)"                 TO DBMULSIL-DETAIL-PROMPT.
      *    MOVE I-BRK(1)                   TO DBMULSIL-DETAIL-DOUBLE.
      *    PERFORM DBMULSIL-WRITE-RECORD.

      *    MOVE "I-BRK(2)"                 TO DBMULSIL-DETAIL-PROMPT.
      *    MOVE I-BRK(2)                   TO DBMULSIL-DETAIL-DOUBLE.
      *    PERFORM DBMULSIL-WRITE-RECORD.

      *    MOVE "I-BRK(3)"                 TO DBMULSIL-DETAIL-PROMPT.
      *    MOVE I-BRK(3)                   TO DBMULSIL-DETAIL-DOUBLE.
      *    PERFORM DBMULSIL-WRITE-RECORD.

      *    MOVE "I-BRK(4)"                 TO DBMULSIL-DETAIL-PROMPT.
      *    MOVE I-BRK(4)                   TO DBMULSIL-DETAIL-DOUBLE.
      *    PERFORM DBMULSIL-WRITE-RECORD.

      *    MOVE "I-BRK(5)"                 TO DBMULSIL-DETAIL-PROMPT.
      *    MOVE I-BRK(5)                   TO DBMULSIL-DETAIL-DOUBLE.
      *    PERFORM DBMULSIL-WRITE-RECORD.

      *    MOVE "I-RATEIDX"                TO DBMULSIL-DETAIL-PROMPT.
      *    MOVE I-RATEIDX                  TO DBMULSIL-DETAIL-NUM2.
      *    PERFORM DBMULSIL-WRITE-RECORD.

      *    MOVE "I-RTIMESBRK(1)"           TO DBMULSIL-DETAIL-PROMPT.
      *    MOVE I-RTIMESBRK(1)             TO DBMULSIL-DETAIL-DOUBLE.
      *    PERFORM DBMULSIL-WRITE-RECORD.

      *    MOVE "I-RTIMESBRK(2)"           TO DBMULSIL-DETAIL-PROMPT.
      *    MOVE I-RTIMESBRK(2)             TO DBMULSIL-DETAIL-DOUBLE.
      *    PERFORM DBMULSIL-WRITE-RECORD.

      *    MOVE "I-RTIMESBRK(3)"           TO DBMULSIL-DETAIL-PROMPT.
      *    MOVE I-RTIMESBRK(3)             TO DBMULSIL-DETAIL-DOUBLE.
      *    PERFORM DBMULSIL-WRITE-RECORD.

      *    MOVE "I-RTIMESBRK(4)"           TO DBMULSIL-DETAIL-PROMPT.
      *    MOVE I-RTIMESBRK(4)             TO DBMULSIL-DETAIL-DOUBLE.
      *    PERFORM DBMULSIL-WRITE-RECORD.

      *    MOVE "I-RTIMESBRK(5)"           TO DBMULSIL-DETAIL-PROMPT.
      *    MOVE I-RTIMESBRK(5)             TO DBMULSIL-DETAIL-DOUBLE.
      *    PERFORM DBMULSIL-WRITE-RECORD.

      *    MOVE "P1"                       TO DBMULSIL-DETAIL-PROMPT.
      *    MOVE P1                         TO DBMULSIL-DETAIL-DOUBLE.
      *    PERFORM DBMULSIL-WRITE-RECORD.

      *    MOVE "P2"                       TO DBMULSIL-DETAIL-PROMPT.
      *    MOVE P2                         TO DBMULSIL-DETAIL-DOUBLE.
      *    PERFORM DBMULSIL-WRITE-RECORD.

      *    MOVE "B1"                       TO DBMULSIL-DETAIL-PROMPT.
      *    MOVE B1                         TO DBMULSIL-DETAIL-DOUBLE.
      *    PERFORM DBMULSIL-WRITE-RECORD.

      *    MOVE "B2"                       TO DBMULSIL-DETAIL-PROMPT.
      *    MOVE B2                         TO DBMULSIL-DETAIL-DOUBLE.
      *    PERFORM DBMULSIL-WRITE-RECORD.

      *    MOVE "AREG"                     TO DBMULSIL-DETAIL-PROMPT.
      *    MOVE AREG                       TO DBMULSIL-DETAIL-DOUBLE.
      *    PERFORM DBMULSIL-WRITE-RECORD.

      *    MOVE "BREG"                     TO DBMULSIL-DETAIL-PROMPT.
      *    MOVE BREG                       TO DBMULSIL-DETAIL-DOUBLE.
      *    PERFORM DBMULSIL-WRITE-RECORD.

      *    MOVE "TREG"                     TO DBMULSIL-DETAIL-PROMPT.
      *    MOVE TREG                       TO DBMULSIL-DETAIL-SIGN9.
      *    PERFORM DBMULSIL-WRITE-RECORD.

           MOVE "I-P"                      TO DBMULSIL-DETAIL-PROMPT.
           MOVE I-P                        TO DBMULSIL-DETAIL-DOUBLE. 
           PERFORM DBMULSIL-WRITE-RECORD. 

           MOVE "MIR BALANCE"              TO DBMULSIL-DETAIL-PROMPT.
           MOVE MIR-BALANCE                TO DBMULSIL-DETAIL-SIGN9.
           PERFORM DBMULSIL-WRITE-RECORD. 

           MOVE "MIR PAYMENT"             TO DBMULSIL-DETAIL-PROMPT.
           MOVE MIR-PAYMENT           TO DBMULSIL-DETAIL-SIGN9-WHOLE.
           PERFORM DBMULSIL-WRITE-RECORD. 

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE ALL "="                    TO DBMULSIL-ECHO-TEXT.
           MOVE DBMULSIL-ECHO-CMD          TO DBMULSIL-SYSTEM-CMD.
           CALL 'SYSTEM' USING DBMULSIL-SYSTEM.



      *=================================================================
       DBMULSIL-ROUTINE-EXIT.
           EXIT.

      ******************************************************************
      *
      *   D B M U L S I L - I N I T I A L I Z A T I O N 
      *
      ******************************************************************
       DBMULSIL-INITIALIZATION SECTION.

           MOVE "N" TO DBMULSIL-FIRST-TIME.

           ACCEPT DBMULSIL-TIME-NOW FROM TIME.
           ACCEPT DBMULSIL-DATE-NOW FROM CENTURY-DATE.

           MOVE DBMULSIL-DATE-YYMMDD TO DBMULSIL-OUTPUT-DATE.
           MOVE DBMULSIL-TIME-HHMMSS TO DBMULSIL-OUTPUT-TIME.

           CALL "C$TOLOWER" USING DBMULSIL-OUTPUT-FILE, VALUE 8.
           CALL "C$TOLOWER" USING DBMULSIL-ECHO-CMD,    VALUE 4.

           MOVE DBMULSIL-OUTPUT-FILE TO DBMULSIL-SYSTEM-OUTPUT.

      *=================================================================
       DBMULSIL-INITIAL-EXIT.
           EXIT.

      ******************************************************************
      *
      *   D B M U L S I L - T E S T - S E T T I N G S
      *
      *=================================================================
      * DESC: USED FOR FORCE SETTING THE DEC-VARS AND WS-VARS FIELDS
      *       JUST TO TEST THE OUTPUTS.
      ******************************************************************
       DBMULSIL-TEST-SETTINGS SECTION.

           MOVE 0.01                       TO DEC(1).
           MOVE 0.01                       TO DEC(2).
           MOVE 0.00                       TO DEC(3).
           MOVE 1.00                       TO DEC(4).
           MOVE 0.01                       TO DEC(5).
           MOVE 0.01                       TO DEC(6).
           MOVE 1.00                       TO DEC(7).
           MOVE 0.0001                     TO DEC(8).
           MOVE 0.01                       TO DEC(9).

           MOVE 0.1234567890123            TO I-T.
           MOVE 0.1234567890123            TO I-C.
           MOVE 0.1234567890123            TO I-B.
           MOVE 0.1234567890123            TO I-P.
           MOVE 0.1234567890123            TO I-REGP.
           MOVE 0.1234567890123            TO I-NOR.
           MOVE 0.1234567890123            TO I-R(1).
           MOVE 0.1234567890123            TO I-R(2).
           MOVE 0.1234567890123            TO I-R(3).
           MOVE 0.1234567890123            TO I-R(4).
           MOVE 0.1234567890123            TO I-R(5).
           MOVE 0.1234567890123            TO I-BRK(1).
           MOVE 0.1234567890123            TO I-BRK(2).
           MOVE 0.1234567890123            TO I-BRK(3).
           MOVE 0.1234567890123            TO I-BRK(4).
           MOVE 0.1234567890123            TO I-BRK(5).
           MOVE 12                         TO I-RATEIDX.
           MOVE 0.1234567890123            TO I-RTIMESBRK(1).
           MOVE 0.1234567890123            TO I-RTIMESBRK(2).
           MOVE 0.1234567890123            TO I-RTIMESBRK(3).
           MOVE 0.1234567890123            TO I-RTIMESBRK(4).
           MOVE 0.1234567890123            TO I-RTIMESBRK(5).
           MOVE 0.1234567890123            TO P1.
           MOVE 0.1234567890123            TO P2.
           MOVE 0.1234567890123            TO B1.
           MOVE 0.1234567890123            TO B2.
           MOVE 0.1234567890123            TO AREG.
           MOVE 0.1234567890123            TO BREG.
           MOVE -123456789                 TO TREG.

      *=================================================================
       DBMULSIL-TEST-SETT-EXIT.
           EXIT.

      ******************************************************************
      *
      *   D B M U L S I L - W R I T E - R E C O R D
      *
      ******************************************************************
       DBMULSIL-WRITE-RECORD SECTION.

      *    STOP DBMULSIL-DETAIL-LINE.
           MOVE DBMULSIL-DETAIL-LINE       TO DBMULSIL-ECHO-TEXT.
           MOVE DBMULSIL-ECHO-CMD          TO DBMULSIL-SYSTEM-CMD.
           CALL 'SYSTEM' USING DBMULSIL-SYSTEM.

           MOVE SPACES                     TO DBMULSIL-DETAIL-LINE.

      *=================================================================
       DBMULSIL-WRITE-RECORD-EXIT.
           EXIT.

