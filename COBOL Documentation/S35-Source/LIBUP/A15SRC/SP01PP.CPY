      * COPYMEMBER: LIBSP/SP01PP
      ******************************************************************
      *                                                 
      *        WORLD PROFICIENCY POINTS FILE                  
      *           DATA/R?/SP/PPFILE
      *                                                 
      *         NOTES: 
      *         
      *         RATING SHEET PROFILE POINTS FILES
      *         KEYED BY DATA BY BR# FOR MANAGER POINTS 
      *         MAY ALSO HAVE RECORDS KEYED BY GROUP ID 
      *         FOR SUPERVISOR/VP, NOT SURE....
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
      * BLF 050216 ADD BLENDED STANDARD AMOUNTS FOR PROFIT, DELINQUENCY,
      *            VOLUME & BAD DEBT, INCREASED UNUSED, WORLD PR# 384
      * BLF 060407 ADD BLENDED STANDARD NUMBER AND PCT FOR SLOW 
      *            ACCOUNTS, WORLD PR# 463
      ******************************************************************
       78  PP-SIZE                           VALUE 1472.
       01  PP-REC.
           03  PP1-KEY.
               05  PP-FISCAL                 PIC 9(4).
               05  PP-ID.
                   07  PP-BRNO               PIC 9(4).

      *        PROFICIENCY RATING POINTS:
           03  PP-POINT-TABLE                OCCURS 12 TIMES.
               05  PP-STD-PROF-PCT           PIC S9(3)V9(4) COMP-3.
               05  PP-ACT-PROF-PCT           PIC S9(3)V9(4) COMP-3.
               05  PP-PROF-POINTS            PIC 9(4)       COMP-6.

               05  PP-STD-DEL-PCT            PIC S9(3)V9(4) COMP-3.
               05  PP-ACT-DEL-PCT            PIC S9(3)V9(4) COMP-3.
               05  PP-DEL-POINTS             PIC 9(4)       COMP-6.

               05  PP-STD-VOL-PCT            PIC S9(3)V9(4) COMP-3.
               05  PP-ACT-VOL-PCT            PIC S9(3)V9(4) COMP-3.
               05  PP-VOL-POINTS             PIC 9(4)       COMP-6.

               05  PP-STD-BAD-DEBT-PCT       PIC S9(3)V9(4) COMP-3.
               05  PP-ACT-BAD-DEBT-PCT       PIC S9(3)V9(4) COMP-3.
               05  PP-BAD-DEBT-POINTS        PIC 9(4)       COMP-6.

               05  PP-ACT-SLOW-PCT           PIC S9(3)V9(4) COMP-3.
               05  PP-SLOW-POINTS            PIC 9(4)       COMP-6.

               05  PP-ACT-GAIN-PCT           PIC S9(3)V9(4) COMP-3.
               05  PP-GAIN-POINTS            PIC 9(4)       COMP-6.

               05  PP-STD-PROF-AMT           PIC S9(7)V99   COMP-3.
               05  PP-STD-DEL-AMT            PIC S9(7)V99   COMP-3.
               05  PP-STD-VOL-AMT            PIC S9(7)V99   COMP-3.
               05  PP-STD-BAD-DEBT-AMT       PIC S9(7)V99   COMP-3.

           03  PP-SLOW-STANDARDS             OCCURS 12 TIMES.
               05  PP-STD-SLOW-NO-ACCTS      PIC S9(7)      COMP-3.
               05  PP-STD-SLOW-PCT           PIC S9(3)V9(4) COMP-3.

           03  FILLER                        PIC X(504).
