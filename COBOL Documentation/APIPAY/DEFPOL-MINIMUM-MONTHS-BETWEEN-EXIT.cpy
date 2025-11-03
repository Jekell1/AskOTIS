       DEFPOL-MINIMUM-MONTHS-BETWEEN-EXIT.
      * RESTORE EXISTING LP-REC
           MOVE DEFPOL-LP-REC TO LP-REC.
      *================================================================*
      * END COPYBOOK: LIBLP\DEFPOL.CPY                                *
      *================================================================*
      *================================================================*
      * EMBEDDED COPYBOOK: LIBLP\SETCTBL.CPY                           *
      *================================================================*
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
