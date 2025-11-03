      * COPYMEMBER: LIBGB/GRTOTALS
      ******************************************************************
      *
      *    G R O U P - T O T A L - T A B L E - COPY MEMBER SECTIONS
      *
      ******************************************************************
      *
      *   NAME: GRTOTALS
      *
      *   DESC: A TOTALS TABLE IS COMPILED AND PRINTED TOTALS FOR GROUP
      *         LEVELS OF COMPANY, STATE, VP OPERATIONS, DIRECTOR,
      *         AND BRANCHES.
      *
      *   CONTAINS SECTIONS:  (GTT) = GROUP TOTALS TABLE.
      *      GROUP-TOTAL-TABLE SECTION.
      *      GTT-CHECK-ADD-TOTALS SECTION.
      *      GTT-TOTAL-LEVEL-CHECK SECTION.
      *
      *   NEEDS HARDCODED SECTIONS:
      *      GTT-ADD-TOTALS SECTION.
      *      GTT-COMPILE-GROUP-TOTALS SECTION.
      *      GTT-PRINT-GROUP-BR-TOTALS SECTION.
      *      GTT-PRINT-GROUP-TOTALS SECTION.
      *
      *   NEEDS HARDCODED FIELDS:
      *      (IN SCREEN-BUF) - ENT-GROUP
      *      (IN SCREEN-BUF) - GROUP-FLAG
      *      (IN SCREEN-BUF) - TOTAL-LEVEL-TABLE(MAX)  (MAX = 10)
      *
      *   COPY: LIBGB/GBFSGR;  LIBGB/GBFDGR;  LIBGB/GBWSGR;
      *         LIBGB/GBGR1RN; LIBGB/GRSERCH; LIBGB/GRWORK(W);
      *         LIBGB/GRTOTALSW;
      *
      *   ALSO NEEDED FOR WINDOW:
      *         LIBWI/GRTOTL(W); WI/GRTOTL
      *
      *   NOTE: THIS COPY MEMBER WAS CONSTRUCTED FROM LP/OPREPT.C (A90)
      *
      * REV:
      * BLV 000807 FIX GROUP PROBLEM IF INCONSISTENT LEVELS IN GROUP
      *            HIERARCHY, SEE GROUP.BUG IN /USR/A0102B/WORLD
      ******************************************************************
      *
      *    G R O U P - T O T A L - T A B L E
      *
      ******************************************************************
       GROUP-TOTAL-TABLE SECTION.

           IF ( GROUP-FLAG NOT = "Y" )
              GO TO EXIT-GROUP-TOTAL-TABLE.

           MOVE "Y" TO GTT-FIRST-TIME.
           MOVE "Y" TO GTT-HEADER-FG.
           MOVE ENT-GROUP TO GR-ID.
           PERFORM SEARCH-GR-INIT.

      *-----------------------------------------------------------------
       NEXT-GROUP-TOTALS-LOC.

           PERFORM SEARCH-GR-NEXT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * AT END OF FILE, FORCE WGR-SCURR -TABLE & TPLUS TO CHANGE FROM
      * PREVIOUS SO GRAND TOTALS WILL PRINT - MUST FORCE BECAUSE ALL
      * LEVELS START AT THE SAME POINT (THE SELECTED PARENT GROUP)
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           IF ( WGR-SRESULT = ZEROES )
              MOVE ZEROS TO WGR-SCURR-TABLE
              MOVE 15 TO WGR-TPLUS
              PERFORM GTT-TOTAL-LEVEL-CHECK
              GO TO EXIT-GROUP-TOTAL-TABLE.

           IF ( GTT-FIRST-TIME = "Y" )
              MOVE "N" TO GTT-FIRST-TIME
              MOVE WGR-SCURR-TABLE TO GTT-PREV-SCURR-TABLE
           ELSE
              PERFORM GTT-TOTAL-LEVEL-CHECK.

      *-----------------------------------------------------------------
       PROCESS-FILE-FOR-GROUP-TABLE.

           PERFORM GTT-COMPILE-GROUP-TOTALS.

           GO TO NEXT-GROUP-TOTALS-LOC.

      *=================================================================
       EXIT-GROUP-TOTAL-TABLE.
           MOVE "N" TO GTT-HEADER-FG.
      ******************************************************************
      *
      *   CHECK FOR LEVEL SUMMARY TOTALS:
      *   WGR-SCURR IS TABLE OF POINTERS FOR CURRENT GROUP WHICH POINT
      *   TO WGR-SID TABLE TO SHOW CURRENT GROUP HIERARCHY:
      *   WGR-SCURR(1) POINTS TO WGR-SID OF LOW LEVEL (SELECTED GROUP);
      *   WGR-SCURR(WGR-TPLUS) POINTS TO THE WGR-SID OF THE HIGHEST
      *   LEVEL WHICH IS THE BRANCH NUMBER CURRENTLY BEING PROCESSED.
      *   THIS ROUTINE COMPARES THE WGR-SCURR TABLE TO THE TABLE SAVED
      *   FROM THE PREVIOUS GROUP, PRINTING & ZEROING TOTALS FOR EACH
      *   LEVEL WHICH HAS CHANGED STARTING WITH THE HIGHEST (BRANCH)
      *   LEVEL & WORKING THROUGH HIERARCHY.  GRAND TOTALS ARE FORCED BY
      *   MOVING 0 TO WGR-SCURR TABLE & 15 TO WGR-TPLUS.
      *
      ******************************************************************
      *
      *    G T T - T O T A L - L E V E L - C H E C K
      *
      ******************************************************************
       GTT-TOTAL-LEVEL-CHECK SECTION.
      *-----------------------------------------------------------------
       GTT-TOT-LEVEL-CHK.

      *    MOVE WGR-TPLUS TO GTT-GROUP-SUB.
           MOVE 15 TO GTT-GROUP-SUB.

      *-----------------------------------------------------------------
       GTT-LEVEL-TOTAL-LOOP.

           IF ( GTT-GROUP-SUB = 0 )
              GO TO GTT-EXIT-TOTAL-LEVEL-CHECK.

           IF (WGR-SCURR(GTT-GROUP-SUB) = GTT-PREV-SCURR(GTT-GROUP-SUB))
              GO TO GTT-CONTINUE-LOOP.

           MOVE GTT-PREV-SCURR(GTT-GROUP-SUB) TO GTT-PREV-SUB.

           IF ( GTT-PREV-SUB = 0 )
              GO TO GTT-CONTINUE-LOOP.

           IF ( WGR-SID(GTT-PREV-SUB) NUMERIC )
              GO TO GTT-CHECK-BRANCH-SID.

           MOVE WGR-SID(GTT-PREV-SUB) TO GR-ID.
           PERFORM READ-GR1-FILE.

           IF ( TOTAL-LEVEL-TABLE(GR-LEVEL) NOT = "Y" )
              GO TO GTT-CONTINUE-LOOP.

           MOVE GR-LEVEL TO GTT-TOTAL-SUB.

      *-----------------------------------------------------------------
       GTT-DO-GROUP-TOTALS.

           PERFORM GTT-PRINT-GROUP-TOTALS.
           GO TO GTT-CONTINUE-LOOP.

      *-----------------------------------------------------------------
       GTT-CHECK-BRANCH-SID.

           IF ( TOTAL-LEVEL-TABLE(9) NOT = "Y" )
              GO TO GTT-CONTINUE-LOOP.

           PERFORM GTT-PRINT-GROUP-BR-TOTALS.

      *-----------------------------------------------------------------
       GTT-CONTINUE-LOOP.

           SUBTRACT 1 FROM GTT-GROUP-SUB.

           IF ( GTT-GROUP-SUB < WGR-SSTABLE )
              GO TO GTT-EXIT-TOTAL-LEVEL-CHECK.

           GO TO GTT-LEVEL-TOTAL-LOOP.

      *=================================================================
       GTT-EXIT-TOTAL-LEVEL-CHECK.
           MOVE WGR-SCURR-TABLE TO GTT-PREV-SCURR-TABLE.
      ******************************************************************
      *
      *    G T T - C H E C K - A D D - T O T A L S
      *
      ******************************************************************
       GTT-CHECK-ADD-TOTALS SECTION.
      *-----------------------------------------------------------------
       GTT-CHECK-ADD-TOTS.

           MOVE WGR-TPLUS TO GTT-GROUP-SUB.

      *-----------------------------------------------------------------
       GTT-CHECK-TOTAL-LOOP.

           IF ( GTT-GROUP-SUB = 0 )
              GO TO GTT-EXIT-CHECK-TOTALS.

           MOVE WGR-SCURR(GTT-GROUP-SUB) TO GTT-LEVEL-SUB.

           IF ( WGR-SID(GTT-LEVEL-SUB) IS NUMERIC )
              MOVE 9 TO GTT-TOTAL-SUB
              GO TO GTT-CHECK-TOTAL-FLAG.

           MOVE WGR-SID(GTT-LEVEL-SUB) TO GR-ID.
           PERFORM READ-GR1-FILE.
           MOVE GR-LEVEL TO GTT-TOTAL-SUB.

      *-----------------------------------------------------------------
       GTT-CHECK-TOTAL-FLAG.

           IF ( TOTAL-LEVEL-TABLE(GTT-TOTAL-SUB) NOT = "Y" )
              GO TO GTT-CONTINUE-CHECK-TOTALS.

           PERFORM GTT-ADD-TOTALS.

      *-----------------------------------------------------------------
       GTT-CONTINUE-CHECK-TOTALS.

           SUBTRACT 1 FROM GTT-GROUP-SUB.

           IF ( GTT-GROUP-SUB > 0 )
              GO TO GTT-CHECK-TOTAL-LOOP.

      *-----------------------------------------------------------------
       GTT-EXIT-CHECK-TOTALS.
           EXIT.
