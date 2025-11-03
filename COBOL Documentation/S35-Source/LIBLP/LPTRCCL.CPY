      * COPYMEMBER: LIBLP/LPTRCCL

      ***********************************************************************
      *    LPTRCCL    CHECK/CASH TRANSACTION FILE CLEAR ROUTINE
      *
      * REV:
      *  MJD 062895 ADDED CLEAR OF TC-POSTTIME, TC-POSTDATE.
      *  BLF 071015 ADDED CLEAR OF TC-PAYEE-NO, HOME PR# 3078
      *  BAH 160926 ADDED TC-TILLNO
      *  BAH 20210426 REMOVED FUTURE FIELDS
      ***********************************************************************
       CLEAR-TRC-FILE SECTION.
           MOVE SPACES TO TRC-REC.

           MOVE 0 TO TC-KEY-DATE TC-BRNO TC-CKNO TC-CLASS TC-SEQ
                     TC-LTOUCH-DATE TC-DATE TC-AMT TC-REF
                     TC-ACCTNO TC-DLRVNO
                     TC-SPRCLASS TC-DLNO TC-POSTTIME TC-POSTDATE
                     TC-PAYEE-NO TC-TILLNO.

           PERFORM VARYING TRCSUB FROM 1 BY 1 UNTIL TRCSUB > 5
               MOVE 0 TO TC-GLNO(TRCSUB)       TC-GLAMT(TRCSUB)
           END-PERFORM.

