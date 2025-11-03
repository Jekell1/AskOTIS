      ******************************************************************
      *
      * IDENTIFICATION: LIBUT/DBBNKRP1
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
       DBBNKRP1-ROUTINE SECTION.

      *    STOP DBBNKRP1-ECHO-TEXT.
           IF ( DBBNKRP1-FIRST-TIME = "T" )      *> TESTING FUNCTION
              PERFORM DBBNKRP1-INITIALIZATION
              PERFORM DBBNKRP1-TEST-SETTINGS
           ELSE
           IF ( DBBNKRP1-FIRST-TIME = "Y" )
              PERFORM DBBNKRP1-INITIALIZATION.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    PERFORM VARYING DBBNKRP1-SUB FROM 1 BY 1
      *              UNTIL DBBNKRP1-SUB > 9

      *         MOVE SPACES                TO DBBNKRP1-DETAIL-PROMPT
      *         STRING "DEC(", DBBNKRP1-SUB,")"
      *                DELIMITED BY "  " INTO DBBNKRP1-DETAIL-PROMPT
      *         MOVE DEC(DBBNKRP1-SUB)     TO DBBNKRP1-DETAIL-FLOAT
      *         PERFORM DBBNKRP1-WRITE-RECORD

      *    END-PERFORM.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    MOVE "I-T"                      TO DBBNKRP1-DETAIL-PROMPT.
      *    MOVE I-T                        TO DBBNKRP1-DETAIL-DOUBLE.
      *    PERFORM DBBNKRP1-WRITE-RECORD.

      *    MOVE "I-C"                      TO DBBNKRP1-DETAIL-PROMPT.
      *    MOVE I-C                        TO DBBNKRP1-DETAIL-DOUBLE.
      *    PERFORM DBBNKRP1-WRITE-RECORD.

           MOVE "I-B"                      TO DBBNKRP1-DETAIL-PROMPT.
           MOVE I-B                        TO DBBNKRP1-DETAIL-DOUBLE.
           PERFORM DBBNKRP1-WRITE-RECORD.

      *    MOVE "I-P"                      TO DBBNKRP1-DETAIL-PROMPT.
      *    MOVE I-P                        TO DBBNKRP1-DETAIL-DOUBLE.
      *    PERFORM DBBNKRP1-WRITE-RECORD.

      *    MOVE "I-REGP"                   TO DBBNKRP1-DETAIL-PROMPT.
      *    MOVE I-REGP                     TO DBBNKRP1-DETAIL-DOUBLE.
      *    PERFORM DBBNKRP1-WRITE-RECORD.

      *    MOVE "I-NOR"                    TO DBBNKRP1-DETAIL-PROMPT.
      *    MOVE I-NOR                      TO DBBNKRP1-DETAIL-DOUBLE.
      *    PERFORM DBBNKRP1-WRITE-RECORD.

      *    MOVE "I-R(1)"                   TO DBBNKRP1-DETAIL-PROMPT.
      *    MOVE I-R(1)                     TO DBBNKRP1-DETAIL-DOUBLE.
      *    PERFORM DBBNKRP1-WRITE-RECORD.

      *    MOVE "I-R(2)"                   TO DBBNKRP1-DETAIL-PROMPT.
      *    MOVE I-R(2)                     TO DBBNKRP1-DETAIL-DOUBLE.
      *    PERFORM DBBNKRP1-WRITE-RECORD.

      *    MOVE "I-R(3)"                   TO DBBNKRP1-DETAIL-PROMPT.
      *    MOVE I-R(3)                     TO DBBNKRP1-DETAIL-DOUBLE.
      *    PERFORM DBBNKRP1-WRITE-RECORD.

      *    MOVE "I-R(4)"                   TO DBBNKRP1-DETAIL-PROMPT.
      *    MOVE I-R(4)                     TO DBBNKRP1-DETAIL-DOUBLE.
      *    PERFORM DBBNKRP1-WRITE-RECORD.

      *    MOVE "I-R(5)"                   TO DBBNKRP1-DETAIL-PROMPT.
      *    MOVE I-R(5)                     TO DBBNKRP1-DETAIL-DOUBLE.
      *    PERFORM DBBNKRP1-WRITE-RECORD.

      *    MOVE "I-BRK(1)"                 TO DBBNKRP1-DETAIL-PROMPT.
      *    MOVE I-BRK(1)                   TO DBBNKRP1-DETAIL-DOUBLE.
      *    PERFORM DBBNKRP1-WRITE-RECORD.

      *    MOVE "I-BRK(2)"                 TO DBBNKRP1-DETAIL-PROMPT.
      *    MOVE I-BRK(2)                   TO DBBNKRP1-DETAIL-DOUBLE.
      *    PERFORM DBBNKRP1-WRITE-RECORD.

      *    MOVE "I-BRK(3)"                 TO DBBNKRP1-DETAIL-PROMPT.
      *    MOVE I-BRK(3)                   TO DBBNKRP1-DETAIL-DOUBLE.
      *    PERFORM DBBNKRP1-WRITE-RECORD.

      *    MOVE "I-BRK(4)"                 TO DBBNKRP1-DETAIL-PROMPT.
      *    MOVE I-BRK(4)                   TO DBBNKRP1-DETAIL-DOUBLE.
      *    PERFORM DBBNKRP1-WRITE-RECORD.

      *    MOVE "I-BRK(5)"                 TO DBBNKRP1-DETAIL-PROMPT.
      *    MOVE I-BRK(5)                   TO DBBNKRP1-DETAIL-DOUBLE.
      *    PERFORM DBBNKRP1-WRITE-RECORD.

      *    MOVE "I-RATEIDX"                TO DBBNKRP1-DETAIL-PROMPT.
      *    MOVE I-RATEIDX                  TO DBBNKRP1-DETAIL-NUM2.
      *    PERFORM DBBNKRP1-WRITE-RECORD.

      *    MOVE "I-RTIMESBRK(1)"           TO DBBNKRP1-DETAIL-PROMPT.
      *    MOVE I-RTIMESBRK(1)             TO DBBNKRP1-DETAIL-DOUBLE.
      *    PERFORM DBBNKRP1-WRITE-RECORD.

      *    MOVE "I-RTIMESBRK(2)"           TO DBBNKRP1-DETAIL-PROMPT.
      *    MOVE I-RTIMESBRK(2)             TO DBBNKRP1-DETAIL-DOUBLE.
      *    PERFORM DBBNKRP1-WRITE-RECORD.

      *    MOVE "I-RTIMESBRK(3)"           TO DBBNKRP1-DETAIL-PROMPT.
      *    MOVE I-RTIMESBRK(3)             TO DBBNKRP1-DETAIL-DOUBLE.
      *    PERFORM DBBNKRP1-WRITE-RECORD.

      *    MOVE "I-RTIMESBRK(4)"           TO DBBNKRP1-DETAIL-PROMPT.
      *    MOVE I-RTIMESBRK(4)             TO DBBNKRP1-DETAIL-DOUBLE.
      *    PERFORM DBBNKRP1-WRITE-RECORD.

      *    MOVE "I-RTIMESBRK(5)"           TO DBBNKRP1-DETAIL-PROMPT.
      *    MOVE I-RTIMESBRK(5)             TO DBBNKRP1-DETAIL-DOUBLE.
      *    PERFORM DBBNKRP1-WRITE-RECORD.

      *    MOVE "P1"                       TO DBBNKRP1-DETAIL-PROMPT.
      *    MOVE P1                         TO DBBNKRP1-DETAIL-DOUBLE.
      *    PERFORM DBBNKRP1-WRITE-RECORD.

      *    MOVE "P2"                       TO DBBNKRP1-DETAIL-PROMPT.
      *    MOVE P2                         TO DBBNKRP1-DETAIL-DOUBLE.
      *    PERFORM DBBNKRP1-WRITE-RECORD.

      *    MOVE "B1"                       TO DBBNKRP1-DETAIL-PROMPT.
      *    MOVE B1                         TO DBBNKRP1-DETAIL-DOUBLE.
      *    PERFORM DBBNKRP1-WRITE-RECORD.

      *    MOVE "B2"                       TO DBBNKRP1-DETAIL-PROMPT.
      *    MOVE B2                         TO DBBNKRP1-DETAIL-DOUBLE.
      *    PERFORM DBBNKRP1-WRITE-RECORD.

           MOVE "AREG"                     TO DBBNKRP1-DETAIL-PROMPT.
           MOVE AREG                       TO DBBNKRP1-DETAIL-DOUBLE.
           PERFORM DBBNKRP1-WRITE-RECORD.

           MOVE "BREG"                     TO DBBNKRP1-DETAIL-PROMPT.
           MOVE BREG                       TO DBBNKRP1-DETAIL-DOUBLE.
           PERFORM DBBNKRP1-WRITE-RECORD.

           MOVE "TREG"                     TO DBBNKRP1-DETAIL-PROMPT.
           MOVE TREG                       TO DBBNKRP1-DETAIL-SIGN9.
           PERFORM DBBNKRP1-WRITE-RECORD.

      *    MOVE "I-P"                      TO DBBNKRP1-DETAIL-PROMPT.
      *    MOVE I-P                        TO DBBNKRP1-DETAIL-DOUBLE. 
      *    PERFORM DBBNKRP1-WRITE-RECORD. 

           MOVE "MIR BALANCE"              TO DBBNKRP1-DETAIL-PROMPT.
           MOVE MIR-BALANCE                TO DBBNKRP1-DETAIL-SIGN9.
           PERFORM DBBNKRP1-WRITE-RECORD. 

      *    MOVE "MIR PAYMENT"             TO DBBNKRP1-DETAIL-PROMPT.
      *    MOVE MIR-PAYMENT           TO DBBNKRP1-DETAIL-SIGN9-WHOLE.
      *    PERFORM DBBNKRP1-WRITE-RECORD. 

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE ALL "="                    TO DBBNKRP1-ECHO-TEXT.
           MOVE DBBNKRP1-ECHO-CMD          TO DBBNKRP1-SYSTEM-CMD.
           CALL 'SYSTEM' USING DBBNKRP1-SYSTEM.



      *=================================================================
       DBBNKRP1-ROUTINE-EXIT.
           EXIT.

      ******************************************************************
      *
      *   D B M U L S I L - I N I T I A L I Z A T I O N 
      *
      ******************************************************************
       DBBNKRP1-INITIALIZATION SECTION.

           MOVE "N" TO DBBNKRP1-FIRST-TIME.

           ACCEPT DBBNKRP1-TIME-NOW FROM TIME.
           ACCEPT DBBNKRP1-DATE-NOW FROM CENTURY-DATE.

           MOVE DBBNKRP1-DATE-YYMMDD TO DBBNKRP1-OUTPUT-DATE.
           MOVE DBBNKRP1-TIME-HHMMSS TO DBBNKRP1-OUTPUT-TIME.

           CALL "C$TOLOWER" USING DBBNKRP1-OUTPUT-FILE, VALUE 8.
           CALL "C$TOLOWER" USING DBBNKRP1-ECHO-CMD,    VALUE 4.

           MOVE DBBNKRP1-OUTPUT-FILE TO DBBNKRP1-SYSTEM-OUTPUT.

      *=================================================================
       DBBNKRP1-INITIAL-EXIT.
           EXIT.

      ******************************************************************
      *
      *   D B M U L S I L - T E S T - S E T T I N G S
      *
      *=================================================================
      * DESC: USED FOR FORCE SETTING THE DEC-VARS AND WS-VARS FIELDS
      *       JUST TO TEST THE OUTPUTS.
      ******************************************************************
       DBBNKRP1-TEST-SETTINGS SECTION.

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
       DBBNKRP1-TEST-SETT-EXIT.
           EXIT.

      ******************************************************************
      *
      *   D B M U L S I L - W R I T E - R E C O R D
      *
      ******************************************************************
       DBBNKRP1-WRITE-RECORD SECTION.

      *    STOP DBBNKRP1-DETAIL-LINE.
           MOVE DBBNKRP1-DETAIL-LINE       TO DBBNKRP1-ECHO-TEXT.
           MOVE DBBNKRP1-ECHO-CMD          TO DBBNKRP1-SYSTEM-CMD.
           CALL 'SYSTEM' USING DBBNKRP1-SYSTEM.

           MOVE SPACES                     TO DBBNKRP1-DETAIL-LINE.

      *=================================================================
       DBBNKRP1-WRITE-RECORD-EXIT.
           EXIT.

