      ***********************************
      * HONOR PR# 4382  SP-DEFPOLCD "C"
      * THIS ROUTINE CAN BE USED BY ANYONE.
      ***********************************
       DEFPOL-MINIMUM-MONTHS-BETWEEN SECTION.
      ***********************************

      * SAVE EXISTING LP-REC
           MOVE LP-REC TO DEFPOL-LP-REC.

           IF BR-MIN-MONTHS-DEF NOT NUMERIC OR
                   BR-MIN-MONTHS-DEF = 0 OR 99
              GO TO DEFPOL-MINIMUM-MONTHS-BETWEEN-EXIT.

           PERFORM OPEN-LP1-FILE.

           MOVE LP-PATH-OWNBR TO LP-BRNO
                                 QLP1-WBEG-BRNO
                                 QLP1-WEND-BRNO.
           MOVE LN-ACCTNO     TO LP-ACCTNO
                                 QLP1-WBEG-ACCTNO
                                 QLP1-WEND-ACCTNO.
           MOVE 99999         TO LP-SEQNO
                                 QLP1-WEND-SEQNO.
           MOVE ALL "0"       TO QLP1-WBEG-SEQNO.

           PERFORM START-LP1-FILE-NOT-GREATER.
       DEFPOL-MINIMUM-MONTHS-NEXT.
           PERFORM READ-LP1-FILE-PREVIOUS.
           IF IO-FG NOT = 0
              GO TO DEFPOL-MINIMUM-MONTHS-BETWEEN-EXIT.

           IF (LP-BRNO NOT = LN-OWNBR) OR (LP-ACCTNO NOT = LN-ACCTNO)
              GO TO DEFPOL-MINIMUM-MONTHS-BETWEEN-EXIT.

           IF LP-TRCD = "RV" OR LP-REV = "Y"
              GO TO DEFPOL-MINIMUM-MONTHS-NEXT.
           IF NOT (LP-TRCD = "DF" OR "D2" OR "D3" OR "D4" OR "D5" OR
                        "D6" OR "D7" OR "D8" OR "D9")
              GO TO DEFPOL-MINIMUM-MONTHS-NEXT.

      * FOUND LAST DEFERMENT, CHECK IF WITHIN MINIMUM MONTHS
           MOVE LP-TRDATE TO NUM-DATE.
      * DEFPOL-DATE = TRANS-DATE
           MOVE DEFPOL-DATE TO SYS-DATE.
           PERFORM TIM360.

           IF ELAPSED-MONTHS < BR-MIN-MONTHS-DEF
              MOVE "D" TO DEFPOL-MAXIMUM
              GO TO DEFPOL-MINIMUM-MONTHS-BETWEEN-EXIT.

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
