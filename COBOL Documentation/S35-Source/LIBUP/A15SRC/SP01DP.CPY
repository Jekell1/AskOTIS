      * COPYMEMBER: LIBSP/SP01DP
      ******************************************************************
      *                                                 
      *        WORLD DISTRICT PROFICIENCY POINTS FILE                  
      *           DATA/R?/SP/DPFILE
      *                                                 
      *         NOTES: 
      *         
      *         RATING SHEET PROFILE POINTS FILES KEYED BY
      *         DATE BY GROUP FOR DISTRICT SUPERVISOR POINTS 
      *
      *         STORES 12 MONTHS OF POINTS & SOME KEY PERFORMANCE
      *         STATS WHICH ACCOUNT FOR THE POINTS GIVEN.  NOTE,
      *         TABLE IS BY FISCAL, SO OCCURENCE 1 IS PERIOD 1 (APRIL)
      *
      *         THE STANDARDS STORED HERE ARE BLENDED STANDARDS IF
      *         BRANCH DOES LARGE LOANS
      *
      *          
      * REV:
      * BLM 160629 ADD QUARTERLY BONUSES, WORLD PR# 1166                                            
      ******************************************************************
       78  DP-SIZE                           VALUE 1472.
       01  DP-REC.
           03  DP1-KEY.
               05  DP-FISCAL                 PIC 9(4).
               05  DP-GROUP                  PIC X(4).

      *        PROFICIENCY RATING POINTS:
           03  DP-POINT-TABLE                OCCURS 12 TIMES.
               05  DP-STD-PROF-PCT           PIC S9(3)V9(4) COMP-3.
               05  DP-ACT-PROF-PCT           PIC S9(3)V9(4) COMP-3.
               05  DP-PROF-POINTS            PIC 9(4)       COMP-6.

               05  DP-STD-DEL-PCT            PIC S9(3)V9(4) COMP-3.
               05  DP-ACT-DEL-PCT            PIC S9(3)V9(4) COMP-3.
               05  DP-DEL-POINTS             PIC 9(4)       COMP-6.

               05  DP-STD-VOL-PCT            PIC S9(3)V9(4) COMP-3.
               05  DP-ACT-VOL-PCT            PIC S9(3)V9(4) COMP-3.
               05  DP-VOL-POINTS             PIC 9(4)       COMP-6.

               05  DP-STD-BAD-DEBT-PCT       PIC S9(3)V9(4) COMP-3.
               05  DP-ACT-BAD-DEBT-PCT       PIC S9(3)V9(4) COMP-3.
               05  DP-BAD-DEBT-POINTS        PIC 9(4)       COMP-6.

               05  DP-ACT-SLOW-PCT           PIC S9(3)V9(4) COMP-3.
               05  DP-SLOW-POINTS            PIC 9(4)       COMP-6.

               05  DP-ACT-GAIN-PCT           PIC S9(3)V9(4) COMP-3.
               05  DP-GAIN-POINTS            PIC 9(4)       COMP-6.

               05  DP-STD-PROF-AMT           PIC S9(7)V99   COMP-3.
               05  DP-STD-DEL-AMT            PIC S9(7)V99   COMP-3.
               05  DP-STD-VOL-AMT            PIC S9(7)V99   COMP-3.
               05  DP-STD-BAD-DEBT-AMT       PIC S9(7)V99   COMP-3.

           03  DP-SLOW-STANDARDS             OCCURS 12 TIMES.
               05  DP-STD-SLOW-NO-ACCTS      PIC S9(7)      COMP-3.
               05  DP-STD-SLOW-PCT           PIC S9(3)V9(4) COMP-3.
               
           03  DP-QUARTERLY-BONUS-TABLE      OCCURS 4 TIMES.
               05  DP-PROF-BONUS             PIC S9(5)V99   COMP-3.
               05  DP-DEL-BONUS              PIC S9(5)V99   COMP-3.

           03  FILLER                        PIC X(472).
