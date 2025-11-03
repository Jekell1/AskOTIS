      * COPYMEMBER: LIBLP/SETCTBL

      **************************************************************************
      *          SET INDEX INTO SPR RATE TABLES
      *
      *   NAME:  SETCTBL
      *   DESC:  THIS ROUTINE WILL DETERMINE THE INDEX (1 OR 2)
      *          INTO THE SP-CAL-TABLE
      *
      *   IN  :  CTBL-INT-CHARGEABLE
      *          CTBL-TERM
      *
      *   OUT :  CTBL-INDEX              INDEX
      *   COPY:  SETCTBLW
      * REV:
      *  JTG 100598 MADE NOTE THAT THIS WORKS FOR SP-CAL-BREAK-ON-FLATAMT
      *  JTG 000626 ADDED LOGIC FOR TENNESSEE RATE TYPE 'M' FLAT MONTHLY CHARGE
      *             #1225
      *  BAH 090601 ADDED IF SP-CAL-NORATES(1) = 0, DONT SET THE INDEX, GO
      *             TO EXIT, CAUSING DROP TO $ SIGN ON BAD SPR'S, REGMGM
      *  BAH 090612 CHANGED THE SET CSTP TO SP-CAL-NORATES(1) TO USE A WORKER
      *             INBETWEEN, IT DIDNT LIKE THE S9 COMP FIELD, FOUND THIS
      *             WHILE TESTING REGRDN
      **************************************************************************
       SETCTBL SECTION.
           SET CTBL-INDEX TO 1.

           IF SP-CAL-NORATES(1) = 0
              GO TO SETCTBL-EXIT.

           IF NOT SP-CAL-FLAT-MONTHLY(1)
              MOVE SP-CAL-NORATES(1) TO CTBL-WORKER
              SET CSTP TO CTBL-WORKER
      *      - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *      TERM BREAK TEST:
      *      - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
              IF SP-CAL-BREAK-ON-TERM(1)
                 IF SP-CAL-BREAK(1 CSTP) NOT = 0
                             AND CTBL-TERM > SP-CAL-BREAK(1 CSTP)
                    SET CTBL-INDEX TO 2
                 END-IF
              ELSE
      *      - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *         AMOUNT AND FLAT AMOUNT BREAK TEST:
      *      - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
                 IF SP-CAL-BREAK(1 CSTP) NOT = 0
                       AND CTBL-INT-CHARGEABLE > SP-CAL-BREAK(1 CSTP)
                    SET CTBL-INDEX TO 2.

       SETCTBL-EXIT.
           EXIT.
