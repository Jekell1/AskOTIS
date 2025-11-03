      ******************************************************************
      *
      * IDENTIFICATION: LIBUT/DBJAY
      *
      * DESCRIPTION   : DEBUG FILE GENERATOR FOR JAY
      *                 USED TO GET DATA AND LOAD THEM
      *                 INTO A OUTPUT FILE.
      *
      * OUTPUT FILE   : /USR/TMP/DBJAY-YYMMDD-HHMMSS
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
       DBJAY-ROUTINE SECTION.

      *    STOP DBJAY-ECHO-TEXT.
           IF ( DBJAY-FIRST-TIME = "Y" )
              PERFORM DBJAY-INITIALIZATION.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *%%% MOVE "XXX"                      TO DBJAY-DETAIL-PROMPT.
           ACCEPT DBJAY-TIME-NOW FROM TIME.
           ACCEPT DBJAY-DATE-NOW FROM CENTURY-DATE.
           STRING DBJAY-DATE-YYMMDD, "-", 
                  DBJAY-TIME-HHMMSS
                  DELIMITED BY "  " INTO DBJAY-DETAIL-TEXT.
           PERFORM DBJAY-WRITE-RECORD.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    MOVE "TEST TEXT"                TO DBJAY-DETAIL-PROMPT.
      *    MOVE "ABC XYZ"                  TO DBJAY-DETAIL-TEXT.
      *    PERFORM DBJAY-WRITE-RECORD.
      *
      *    MOVE "TEST DATE"                TO DBJAY-DETAIL-PROMPT.
      *    MOVE 20231231                   TO DBJAY-DETAIL-DATE.
      *    PERFORM DBJAY-WRITE-RECORD.
      *
      *    MOVE "TEST NUM9"                TO DBJAY-DETAIL-PROMPT.
      *    MOVE 123456789                  TO DBJAY-DETAIL-NUM9.
      *    PERFORM DBJAY-WRITE-RECORD.
      *
      *    MOVE "TEST NUM9V2"              TO DBJAY-DETAIL-PROMPT.
      *    MOVE 123456789.12               TO DBJAY-DETAIL-NUM9V2.
      *    PERFORM DBJAY-WRITE-RECORD.
      *
      *    MOVE "TEST SIGN9"               TO DBJAY-DETAIL-PROMPT.
      *    MOVE 123456789                  TO DBJAY-DETAIL-SIGN9.
      *    PERFORM DBJAY-WRITE-RECORD.
      *
      *    MOVE "TEST SIGN9V2"             TO DBJAY-DETAIL-PROMPT.
      *    MOVE 123456789.12               TO DBJAY-DETAIL-SIGN9V2.
      *    PERFORM DBJAY-WRITE-RECORD.
      *
      *    MOVE "TEST SSNO"                TO DBJAY-DETAIL-PROMPT.
      *    MOVE 123456789                  TO DBJAY-DETAIL-SSNO.
      *    INSPECT DBJAY-DETAIL-SSNO REPLACING ALL "/" BY "-".
      *    PERFORM DBJAY-WRITE-RECORD.
      *
      *    MOVE ALL "="                    TO DBJAY-DETAIL-LINE.
      *    PERFORM DBJAY-WRITE-RECORD.
      *
      *=================================================================
       DBJAY-ROUTINE-EXIT.
           EXIT.

      ******************************************************************
      *
      *   D B J A Y - I N I T I A L I Z A T I O N 
      *
      ******************************************************************
       DBJAY-INITIALIZATION SECTION.

           MOVE "N" TO DBJAY-FIRST-TIME.

           ACCEPT DBJAY-TIME-NOW FROM TIME.
           ACCEPT DBJAY-DATE-NOW FROM CENTURY-DATE.

           MOVE DBJAY-DATE-YYMMDD TO DBJAY-OUTPUT-DATE.
           MOVE DBJAY-TIME-HHMMSS TO DBJAY-OUTPUT-TIME.

           CALL "C$TOLOWER" USING DBJAY-OUTPUT-FILE, VALUE 8.
           CALL "C$TOLOWER" USING DBJAY-ECHO-CMD,    VALUE 4.

           MOVE DBJAY-OUTPUT-FILE TO DBJAY-SYSTEM-OUTPUT.

      *=================================================================
       DBJAY-INITIAL-EXIT.
           EXIT.

      ******************************************************************
      *
      *   D B J A Y - W R I T E - R E C O R D
      *
      ******************************************************************
       DBJAY-WRITE-RECORD SECTION.

           MOVE DBJAY-DETAIL-LINE          TO DBJAY-ECHO-TEXT.
           MOVE DBJAY-ECHO-CMD             TO DBJAY-SYSTEM-CMD.
           CALL 'SYSTEM' USING DBJAY-SYSTEM.

           MOVE SPACES                     TO DBJAY-DETAIL-LINE.

      *=================================================================
       DBJAY-WRITE-RECORD-EXIT.
           EXIT.

