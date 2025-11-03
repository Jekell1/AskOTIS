      * COPYMEMBER: LIBLP/LPANTI
      ****************************************************************
      *         ADJUST ANTICIPATED EARNINGS BUCKETS.
      *
      *   NAME: LPANTI
      *   DESC: THIS ROUTINE SUMS ALL ANTICIPATEDS WITH SP-ERNFRMLA =
      *         SPACE AT BOOKING AND PUTS THE SUM IN ANTICIPATED
      *         OCCURANCE 1.
      *          NOTE:
      *               ALL BUT LEVEL 2 FOR INSURANCE ADDONS
      *
      *   IN  : WK-ANTICERN(1 - 6)
      *   OUT : WK-ANTICERN(1 - 6)
      *
      *   USED:
      *   COPY: LPANTIW
      * REV:
      *  JTG 081090 STOPPED LEVEL 2 FROM BEING TESTED
      *  MJD 990802 ADDED TEST OF GP-INCL-OTH-IN-EARNED.
      *   CS 081105 ADDED TEST OF GP-INCL-OTH-IN-EARNED = 'R', THIS IS
      *             REGACC, DONT INCLUDE OTHER(6) OR ANTICERN(4)
      *  BAH 110307 CLEANED UP IF STATEMENT FROM CHANGE ABOVE, MADE IT
      *             EASIER TO READ.
      ****************************************************************
       ADJUST-ANTICERN SECTION.
      *    MOVE 2 TO SUB.
           MOVE 3 TO SUB.
           PERFORM ADJUST-IT 4 TIMES.
       EXIT-ADJUST-ANTICERN.
           EXIT.

       ADJUST-IT SECTION.
           IF (SUB = 4 OR 6) AND GP-INCL-OTH-IN-EARNED-R
               GO TO ADJUST-SKIP-IT.

           IF SUB NOT = 6 OR (SUB = 6 AND NOT (GP-INCL-OTH-IN-EARNED-N))
              IF SP-ERNFRMLA(SUB) = 0
                 ADD WK-ANTICERN(SUB) TO WK-ANTICERN(1)
                 MOVE 0 TO WK-ANTICERN(SUB).


       ADJUST-SKIP-IT.
           ADD 1 TO SUB.
