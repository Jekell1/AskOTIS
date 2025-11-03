      ******************************************************************
      *
      * IDENTIFICATION: LIBUT/DBRON
      *
      * DESCRIPTION   : DEBUG FILE GENERATOR FOR RON
      *                 USED TO GET DATA AND LOAD THEM
      *                 INTO A OUTPUT FILE.
      *
      * OUTPUT FILE   : /USR/TMP/DBRON-YYMMDD-HHMMSS
      *
      *=================================================================
      * REV:
      * JKC 2024-0104 ***NEW***
      *
      ******************************************************************
      *
      *   D B J A Y - R O U T I N E
      *
      ******************************************************************
       DBRON-ROUTINE SECTION.

      *    STOP DBRON-ECHO-TEXT.
           IF ( DBRON-FIRST-TIME = "Y" )
              PERFORM DBRON-INITIALIZATION.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *%%% MOVE "XXX"                      TO DBRON-DETAIL-PROMPT.
           ACCEPT DBRON-TIME-NOW FROM TIME.
           ACCEPT DBRON-DATE-NOW FROM CENTURY-DATE.
           STRING DBRON-DATE-YYMMDD, "-", 
                  DBRON-TIME-HHMMSS
                  DELIMITED BY "  " INTO DBRON-DETAIL-TEXT.
           PERFORM DBRON-WRITE-RECORD.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    MOVE "TEST TEXT"                TO DBRON-DETAIL-PROMPT.
      *    MOVE "ABC XYZ"                  TO DBRON-DETAIL-TEXT.
      *    PERFORM DBRON-WRITE-RECORD.
      *
      *    MOVE "TEST DATE"                TO DBRON-DETAIL-PROMPT.
      *    MOVE 20231231                   TO DBRON-DETAIL-DATE.
      *    PERFORM DBRON-WRITE-RECORD.
      *
      *    MOVE "TEST NUM9"                TO DBRON-DETAIL-PROMPT.
      *    MOVE 123456789                  TO DBRON-DETAIL-NUM9.
      *    PERFORM DBRON-WRITE-RECORD.
      *
      *    MOVE "TEST NUM9V2"              TO DBRON-DETAIL-PROMPT.
      *    MOVE 123456789.12               TO DBRON-DETAIL-NUM9V2.
      *    PERFORM DBRON-WRITE-RECORD.
      *
      *    MOVE "TEST SIGN9"               TO DBRON-DETAIL-PROMPT.
      *    MOVE 123456789                  TO DBRON-DETAIL-SIGN9.
      *    PERFORM DBRON-WRITE-RECORD.
      *
      *    MOVE "TEST SIGN9V2"             TO DBRON-DETAIL-PROMPT.
      *    MOVE 123456789.12               TO DBRON-DETAIL-SIGN9V2.
      *    PERFORM DBRON-WRITE-RECORD.
      *
      *    MOVE "TEST SSNO"                TO DBRON-DETAIL-PROMPT.
      *    MOVE 123456789                  TO DBRON-DETAIL-SSNO.
      *    INSPECT DBRON-DETAIL-SSNO REPLACING ALL "/" BY "-".
      *    PERFORM DBRON-WRITE-RECORD.
      *
      *    MOVE ALL "="                    TO DBRON-DETAIL-LINE.
      *    PERFORM DBRON-WRITE-RECORD.
      *
      *=================================================================
       DBRON-ROUTINE-EXIT.
           EXIT.

      ******************************************************************
      *
      *   D B J A Y - I N I T I A L I Z A T I O N 
      *
      ******************************************************************
       DBRON-INITIALIZATION SECTION.

           MOVE "N" TO DBRON-FIRST-TIME.

           ACCEPT DBRON-TIME-NOW FROM TIME.
           ACCEPT DBRON-DATE-NOW FROM CENTURY-DATE.

           MOVE DBRON-DATE-YYMMDD TO DBRON-OUTPUT-DATE.
           MOVE DBRON-TIME-HHMMSS TO DBRON-OUTPUT-TIME.

           CALL "C$TOLOWER" USING DBRON-OUTPUT-FILE, VALUE 8.
           CALL "C$TOLOWER" USING DBRON-ECHO-CMD,    VALUE 4.

           MOVE DBRON-OUTPUT-FILE TO DBRON-SYSTEM-OUTPUT.

      *=================================================================
       DBRON-INITIAL-EXIT.
           EXIT.

      ******************************************************************
      *
      *   D B J A Y - W R I T E - R E C O R D
      *
      ******************************************************************
       DBRON-WRITE-RECORD SECTION.

           MOVE DBRON-DETAIL-LINE          TO DBRON-ECHO-TEXT.
           MOVE DBRON-ECHO-CMD             TO DBRON-SYSTEM-CMD.
           CALL 'SYSTEM' USING DBRON-SYSTEM.

           MOVE SPACES                     TO DBRON-DETAIL-LINE.

      *=================================================================
       DBRON-WRITE-RECORD-EXIT.
           EXIT.

