      ******************************************************************
      *
      * IDENTIFICATION: LIBUT/DBJAY1
      *
      * DESCRIPTION   : DEBUG FILE GENERATOR FOR JAY
      *                 USED TO GET DATA AND LOAD THEM
      *                 INTO A OUTPUT FILE.
      *
      *                 CREATE ONE FILE FOR LOGGING ALL PROGRAMS FOR
      *                 ONE USER; WILL APPEND TO THE FILE IF RUNNING
      *                 OTIS MULTIPLE TIMES.
      *
      * OUTPUT FILE   : /USR/TMP/DBJAY-YYMMDD-USERID
      *
      *=================================================================
      * REV:
      * JKC 2024-0122 ***NEW***
      *
      ******************************************************************
      *
      *   D B J A Y - R O U T I N E
      *
      ******************************************************************
       DBJAY-ROUTINE SECTION.

      *    STOP DBJAY-ECHO-TEXT.
           PERFORM DBJAY-INITIALIZATION.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE DBJAY-TIME-HHMMSSHD        TO DBJAY-DETAIL-TIME.
           INSPECT DBJAY-DETAIL-TIME REPLACING ALL "/" BY ":".
           MOVE FORM-PATHNAME              TO DBJAY-DETAIL-PROGRAM.
      *KC  MOVE ?????                      TO DBJAY-DETAIL-TEXT.

           PERFORM DBJAY-WRITE-RECORD.

      *=================================================================
       DBJAY-ROUTINE-EXIT.
           EXIT.

      ******************************************************************
      *
      *   D B J A Y - I N I T I A L I Z A T I O N 
      *
      ******************************************************************
       DBJAY-INITIALIZATION SECTION.

           ACCEPT DBJAY-TIME-NOW FROM TIME.
           ACCEPT DBJAY-DATE-NOW FROM CENTURY-DATE.

           MOVE DBJAY-DATE-YYMMDD  TO DBJAY-OUTPUT-DATE.
           MOVE EXT-CONAME-USER-ID TO DBJAY-OUTPUT-USERID.

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

