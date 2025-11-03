      ******************************************************************
      *
      * IDENTIFICATION: LIBCL/BTRACKW
      *
      * DESCRIPTION   : BATCH TRACK WORKERS
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
      * JKC 2025-0106 ***NEW***
      *
      ******************************************************************
       01  BTRACK-WORKERS.
           03  BTRACK-BRNO                 PIC 9(4)    VALUE ZEROES.
           03  BTRACK-MESS                 PIC X(60)   VALUE SPACES.
           03  BTRACK-SWITCH               PIC X(1)    VALUE SPACES.
               88  BTRACK-SWITCH-CREATE        VALUE "C".
               88  BTRACK-SWITCH-DONE          VALUE "D".
               88  BTRACK-SWITCH-GLOBAL        VALUE "G".
               88  BTRACK-SWITCH-LOCAL         VALUE "L".
               88  BTRACK-SWITCH-REMOVE        VALUE "R".
               88  BTRACK-SWITCH-VALID         VALUE "C" "D" "G"
                                                     "L" "R".

