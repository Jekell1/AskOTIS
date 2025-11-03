      * COPYMEMBER: LIBLP/LPTRCCL

      ***********************************************************************
      *    LPTRCCL    CHECK/CASH TRANSACTION FILE CLEAR ROUTINE
      *
      * REV:
      *  MJD 062895 ADDED CLEAR OF ATC-POSTTIME, ATC-POSTDATE.
      *  BLF 071015 ADDED CLEAR OF ATC-PAYEE-NO, HOME PR# 3078
      *  BAH 160926 ADDED ATC-TILLNO
      *  BAH 20210305 ADDED ATC-EXTRACT-FG, ATC-DEBIT-REFCD,
      *               ATC-FUTURE-ALPHA5, #1490
      *  BAH 20210426 REMOVED FUTURE FIELDS
      ***********************************************************************
       CLEAR-ATRC-FILE SECTION.
           MOVE ALL X"09" TO ATRC-REC.

           MOVE 0 TO ATC-KEY-DATE ATC-BRNO ATC-CKNO ATC-CLASS ATC-SEQ
                     ATC-LTOUCH-DATE ATC-DATE ATC-AMT ATC-REF
                     ATC-ACCTNO ATC-DLRVNO
                     ATC-SPRCLASS ATC-DLNO ATC-POSTTIME ATC-POSTDATE
                     ATC-PAYEE-NO ATC-TILLNO.

           PERFORM VARYING TRCSUB FROM 1 BY 1 UNTIL TRCSUB > 5
               MOVE 0 TO ATC-GLNO(TRCSUB)       ATC-GLAMT(TRCSUB)
           END-PERFORM.

           MOVE SPACES TO ATC-TYPE ATC-PAYEE ATC-REASON
                          ATC-VOID ATC-PAID ATC-PGM ATC-USERID
                          ATC-ADR1 ATC-ADR2 ATC-CITY ATC-STATE
                          ATC-ZIP ATC-ACH-FG ATC-REASON
                          ATC-EXTRACT-FG ATC-DEBIT-REFCD.
