      ******************************************************************
      *
      * IDENTIFICATION: LIBCL/BTRACK
      *
      * DESCRIPTION   : BATCH TRACK ROUTINES
      *                 USED TO TRACK THE EXECUTION OF BATCH PROGRAMS
      *
      *                 ORIGINALLY DESIGNED FOR S35 DATA LOAD
      *                 QA COMPARISON PROGRAMS USING BATCH CODE "Q35";
      *                 USED FOR THE BASH SCRIPT "Q35.SH" TO EXECUTE AND
      *                 TRACK THE EXECUTION.
      *
      * INPUT NEEDED  : EXT-ROUTE-BUF
      *
      * OUTPUT FILE   : /TMP/####??????-XXXXXX
      *
      *                 ####   = BATCH STEP
      *                 ?????? = BATCH PROGRAM BEING EXECUTED
      *                 XXXXXX = USERID ID
      *
      *=================================================================
      * REV:
      * JKC 2025-0103 ***NEW***
      *
      ******************************************************************
      *
      *    B T R A C K - C R E A T E
      *
      *=================================================================
      * IN  : EO-ALL-SPOOL-FILE
      *       EXT-ROUTE-BUF
      * OUT : NOTHING
      * DESC: CREATE BATCH TRACK FILE FOR GIVEN STEP EXECUTION
      ******************************************************************
       BTRACK-CREATE SECTION.

           IF ( EXT-ROUTE-BUF NOT = "70" )
              GO TO BTRACK-CREATE-EXIT.

           MOVE ZEROES                     TO BTRACK-BRNO.
           MOVE SPACES                     TO BTRACK-MESS.
           MOVE "C"                        TO BTRACK-SWITCH.

           CALL "BTRACK" USING FORM-PATH BTRACK-WORKERS.
           CANCEL "TRACE".

      *=================================================================
       BTRACK-CREATE-EXIT.
           EXIT.

      ******************************************************************
      *
      *    B T R A C K - D O N E
      *
      *=================================================================
      * IN  : EO-ALL-SPOOL-FILE
      *       EXT-ROUTE-BUF
      * OUT : NOTHING
      * DESC: DONE WITH BATCH TRACK FILE FOR GIVEN STEP EXECUTION WITH
      *       SHOWING POSSIBLE VISION (E-STATUS-ERROR) &
      *       SQL (SQLERRMC) ERRORS.
      ******************************************************************
       BTRACK-DONE SECTION.

           IF ( EXT-ROUTE-BUF NOT = "70" )
              GO TO BTRACK-DONE-EXIT.

           MOVE ZEROES                     TO BTRACK-BRNO.
           MOVE SPACES                     TO BTRACK-MESS.
           MOVE "D"                        TO BTRACK-SWITCH.

           CALL "BTRACK" USING FORM-PATH BTRACK-WORKERS.
           CANCEL "TRACE".

      *=================================================================
       BTRACK-DONE-EXIT.
           EXIT.

      ******************************************************************
      *
      *    B T R A C K - G L O B A L
      *
      *=================================================================
      * IN  : BTRACK-MESS
      *       EO-ALL-SPOOL-FILE
      *       EXT-ROUTE-BUF
      * OUT : NOTHING
      * DESC: UPDATE BATCH TRACK FILE FOR GIVEN STEP EXECUTION
      *       BASED ON GLOBAL FILE
      ******************************************************************
       BTRACK-GLOBAL SECTION.

           IF ( EXT-ROUTE-BUF NOT = "70" )
              GO TO BTRACK-GLOBAL-EXIT.

           MOVE ZEROES                     TO BTRACK-BRNO.
      *KC  MOVE SPACES                     TO BTRACK-MESS.
           MOVE "G"                        TO BTRACK-SWITCH.

           CALL "BTRACK" USING FORM-PATH BTRACK-WORKERS.
           CANCEL "TRACE".

      *=================================================================
       BTRACK-GLOBAL-EXIT.
           EXIT.

      ******************************************************************
      *
      *    B T R A C K - L O C A L
      *
      *=================================================================
      * IN  : BTRACK-BRNO
      *       BTRACK-MESS
      *       EO-ALL-SPOOL-FILE
      *       EXT-ROUTE-BUF
      * OUT : NOTHING
      * DESC: UPDATE BATCH TRACK FILE FOR GIVEN STEP EXECUTION
      *       BASED ON GLOBAL FILE
      ******************************************************************
       BTRACK-LOCAL SECTION.

           IF ( EXT-ROUTE-BUF NOT = "70" )
              GO TO BTRACK-LOCAL-EXIT.

      *KC  MOVE ZEROES                     TO BTRACK-BRNO.
      *KC  MOVE SPACES                     TO BTRACK-MESS.
           MOVE "L"                        TO BTRACK-SWITCH.

           CALL "BTRACK" USING FORM-PATH BTRACK-WORKERS.
           CANCEL "TRACE".

      *=================================================================
       BTRACK-LOCAL-EXIT.
           EXIT.

