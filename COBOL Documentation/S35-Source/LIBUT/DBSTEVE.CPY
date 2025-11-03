      ******************************************************************
      *
      * IDENTIFICATION: LIBUT/DBSTEVE
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
      *   D B S T E V E - R O U T I N E
      *
      ******************************************************************
       DBSTEVE-ROUTINE SECTION.

      *    STOP DBSTEVE-ECHO-TEXT.
      *    IF ( DBSTEVE-FIRST-TIME = "T" )      *> TESTING FUNCTION
      *       PERFORM DBSTEVE-INITIALIZATION
      *       PERFORM DBSTEVE-TEST-SETTINGS
      *    ELSE
           IF ( DBSTEVE-FIRST-TIME = "Y" )
              PERFORM DBSTEVE-INITIALIZATION.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *%%% MOVE "I-B"                      TO DBSTEVE-DETAIL-PROMPT.
           ACCEPT DBSTEVE-TIME-NOW FROM TIME.
           ACCEPT DBSTEVE-DATE-NOW FROM CENTURY-DATE.
           STRING DBSTEVE-DATE-YYMMDD, "-", 
                  DBSTEVE-TIME-HHMMSS
                  DELIMITED BY "  " INTO DBSTEVE-DETAIL-TEXT.
           PERFORM DBSTEVE-WRITE-RECORD.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE ALL "="                    TO DBSTEVE-ECHO-TEXT.
           MOVE DBSTEVE-ECHO-CMD          TO DBSTEVE-SYSTEM-CMD.
           CALL 'SYSTEM' USING DBSTEVE-SYSTEM.

      *=================================================================
       DBSTEVE-ROUTINE-EXIT.
           EXIT.

      ******************************************************************
      *
      *   D B S T E V E - I N I T I A L I Z A T I O N 
      *
      ******************************************************************
       DBSTEVE-INITIALIZATION SECTION.

           MOVE "N" TO DBSTEVE-FIRST-TIME.

           ACCEPT DBSTEVE-TIME-NOW FROM TIME.
           ACCEPT DBSTEVE-DATE-NOW FROM CENTURY-DATE.

           MOVE DBSTEVE-DATE-YYMMDD TO DBSTEVE-OUTPUT-DATE.
           MOVE DBSTEVE-TIME-HHMMSS TO DBSTEVE-OUTPUT-TIME.

           CALL "C$TOLOWER" USING DBSTEVE-OUTPUT-FILE, VALUE 8.
           CALL "C$TOLOWER" USING DBSTEVE-ECHO-CMD,    VALUE 4.

           MOVE DBSTEVE-OUTPUT-FILE TO DBSTEVE-SYSTEM-OUTPUT.

      *=================================================================
       DBSTEVE-INITIAL-EXIT.
           EXIT.

      ******************************************************************
      *
      *   D B S T E V E - T E S T - S E T T I N G S
      *
      *=================================================================
      * DESC: USED FOR FORCE SETTING THE DEC-VARS AND WS-VARS FIELDS
      *       JUST TO TEST THE OUTPUTS.
      ******************************************************************
       DBSTEVE-TEST-SETTINGS SECTION.

      *    MOVE 0.01                       TO DEC(1).
      *    MOVE 0.01                       TO DEC(2).
      *    MOVE 0.00                       TO DEC(3).
      *    MOVE 1.00                       TO DEC(4).
      *    MOVE 0.01                       TO DEC(5).
      *    MOVE 0.01                       TO DEC(6).
      *    MOVE 1.00                       TO DEC(7).
      *    MOVE 0.0001                     TO DEC(8).
      *    MOVE 0.01                       TO DEC(9).

      *    MOVE 0.1234567890123            TO I-T.
      *    MOVE 0.1234567890123            TO I-C.
      *    MOVE 0.1234567890123            TO I-B.
      *    MOVE 0.1234567890123            TO I-P.
      *    MOVE 0.1234567890123            TO I-REGP.
      *    MOVE 0.1234567890123            TO I-NOR.
      *    MOVE 0.1234567890123            TO I-R(1).
      *    MOVE 0.1234567890123            TO I-R(2).
      *    MOVE 0.1234567890123            TO I-R(3).
      *    MOVE 0.1234567890123            TO I-R(4).
      *    MOVE 0.1234567890123            TO I-R(5).
      *    MOVE 0.1234567890123            TO I-BRK(1).
      *    MOVE 0.1234567890123            TO I-BRK(2).
      *    MOVE 0.1234567890123            TO I-BRK(3).
      *    MOVE 0.1234567890123            TO I-BRK(4).
      *    MOVE 0.1234567890123            TO I-BRK(5).
      *    MOVE 12                         TO I-RATEIDX.
      *    MOVE 0.1234567890123            TO I-RTIMESBRK(1).
      *    MOVE 0.1234567890123            TO I-RTIMESBRK(2).
      *    MOVE 0.1234567890123            TO I-RTIMESBRK(3).
      *    MOVE 0.1234567890123            TO I-RTIMESBRK(4).
      *    MOVE 0.1234567890123            TO I-RTIMESBRK(5).
      *    MOVE 0.1234567890123            TO P1.
      *    MOVE 0.1234567890123            TO P2.
      *    MOVE 0.1234567890123            TO B1.
      *    MOVE 0.1234567890123            TO B2.
      *    MOVE 0.1234567890123            TO AREG.
      *    MOVE 0.1234567890123            TO BREG.
      *    MOVE -123456789                 TO TREG.

      *=================================================================
       DBSTEVE-TEST-SETT-EXIT.
           EXIT.

      ******************************************************************
      *
      *   D B S T E V E - W R I T E - R E C O R D
      *
      ******************************************************************
       DBSTEVE-WRITE-RECORD SECTION.

      *    STOP DBSTEVE-DETAIL-LINE.
           MOVE DBSTEVE-DETAIL-LINE       TO DBSTEVE-ECHO-TEXT.
           MOVE DBSTEVE-ECHO-CMD          TO DBSTEVE-SYSTEM-CMD.
           CALL 'SYSTEM' USING DBSTEVE-SYSTEM.

           MOVE SPACES                     TO DBSTEVE-DETAIL-LINE.

      *=================================================================
       DBSTEVE-WRITE-RECORD-EXIT.
           EXIT.

