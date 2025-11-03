      * COPYMEMBER: LIBLP/LPCXGS_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL GET & SET - CXFILE
      *
      *=================================================================
      * REV:
      * BAH 2021-0510 ***NEW***
      *
      ******************************************************************
      *-----------------------------------------------------------------
      *    LOAD CX-REC FROM QCX-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-CX-FIELDS.
           MOVE QCX-BRNO TO CX-BRNO.
           MOVE QCX-TYPE TO CX-TYPE.
           MOVE QCX-SEQNO TO CX-SEQNO.
           MOVE QCX-PAYEE TO CX-PAYEE.

           IF ( QCX-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO CX-LTOUCH-DATE
           ELSE
              MOVE QCX-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO CX-LTOUCH-DATE.

           MOVE QCX-ADR1 TO CX-ADR1.
           MOVE QCX-ADR2 TO CX-ADR2.
           MOVE QCX-CITY TO CX-CITY.
           MOVE QCX-STATE TO CX-STATE.
           MOVE QCX-ZIP TO CX-ZIP.
           MOVE QCX-AMT TO CX-AMT.
           MOVE QCX-REF TO CX-REF.
           MOVE QCX-REASON TO CX-REASON.
           MOVE QCX-GLNO-1 TO CX-GLNO(1).
           MOVE QCX-GLAMT-1 TO CX-GLAMT(1).
           MOVE QCX-GLNO-2 TO CX-GLNO(2).
           MOVE QCX-GLAMT-2 TO CX-GLAMT(2).
           MOVE QCX-GLNO-3 TO CX-GLNO(3).
           MOVE QCX-GLAMT-3 TO CX-GLAMT(3).
           MOVE QCX-GLNO-4 TO CX-GLNO(4).
           MOVE QCX-GLAMT-4 TO CX-GLAMT(4).
           MOVE QCX-GLNO-5 TO CX-GLNO(5).
           MOVE QCX-GLAMT-5 TO CX-GLAMT(5).
           MOVE QCX-FREE1 TO CX-FREE1.
           MOVE QCX-FREE2 TO CX-FREE2.
           MOVE QCX-VOID TO CX-VOID.
           MOVE QCX-CLASS TO CX-CLASS.
           MOVE QCX-CKNO TO CX-CKNO.
           MOVE QCX-DLNO TO CX-DLNO.

           IF ( QCX-DATE = "1900-01-01" )
              MOVE ZEROES TO CX-DATE
           ELSE
              MOVE QCX-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO CX-DATE.

           MOVE QCX-PAID TO CX-PAID.
           MOVE QCX-PGM TO CX-PGM.
           MOVE QCX-BATCHID TO CX-BATCHID.
           MOVE QCX-BWPROC TO CX-BWPROC.
           MOVE QCX-CKNO-NEW TO CX-CKNO-NEW.
           MOVE QCX-DLRVNO TO CX-DLRVNO.
           MOVE QCX-DT-SEQ TO CX-DT-SEQ.
           MOVE QCX-RESCISSION TO CX-RESCISSION.
           MOVE QCX-TILLNO TO CX-TILLNO.
           MOVE QCX-ACCTNO TO CX-ACCTNO.
           MOVE QCX-SERVACCT TO CX-SERVACCT.

           IF ( QCX-DT-DATE = "1900-01-01" )
              MOVE ZEROES TO CX-DT-DATE
           ELSE
              MOVE QCX-DT-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO CX-DT-DATE.

           MOVE QCX-SSNO TO CX-SSNO.
           MOVE QCX-DT-PAID TO CX-DT-PAID.
           MOVE QCX-DT-UNPAID TO CX-DT-UNPAID.
           MOVE QCX-CKPAID-NEW TO CX-CKPAID-NEW.
           MOVE QCX-EVENT-CD TO CX-EVENT-CD.
           MOVE QCX-PAYEE-NO TO CX-PAYEE-NO.
           MOVE QCX-DEPOSIT-WITHDR TO CX-DEPOSIT-WITHDR.
 
      *-----------------------------------------------------------------
      *    LOAD QCX-REC FROM CX-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-CX-FIELDS.
           MOVE CX-BRNO TO QCX-BRNO.
           MOVE CX-TYPE TO QCX-TYPE.
           MOVE CX-SEQNO TO QCX-SEQNO.
           MOVE CX-PAYEE TO QCX-PAYEE.

           IF ( CX-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QCX-LTOUCH-DATE
           ELSE
              MOVE CX-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCX-LTOUCH-DATE.

           MOVE CX-ADR1 TO QCX-ADR1.
           MOVE CX-ADR2 TO QCX-ADR2.
           MOVE CX-CITY TO QCX-CITY.
           MOVE CX-STATE TO QCX-STATE.
           MOVE CX-ZIP TO QCX-ZIP.
           MOVE CX-AMT TO QCX-AMT.
           MOVE CX-REF TO QCX-REF.
           MOVE CX-REASON TO QCX-REASON.
           MOVE CX-GLNO(1) TO QCX-GLNO-1.
           MOVE CX-GLAMT(1) TO QCX-GLAMT-1.
           MOVE CX-GLNO(2) TO QCX-GLNO-2.
           MOVE CX-GLAMT(2) TO QCX-GLAMT-2.
           MOVE CX-GLNO(3) TO QCX-GLNO-3.
           MOVE CX-GLAMT(3) TO QCX-GLAMT-3.
           MOVE CX-GLNO(4) TO QCX-GLNO-4.
           MOVE CX-GLAMT(4) TO QCX-GLAMT-4.
           MOVE CX-GLNO(5) TO QCX-GLNO-5.
           MOVE CX-GLAMT(5) TO QCX-GLAMT-5.
           MOVE CX-FREE1 TO QCX-FREE1.
           MOVE CX-FREE2 TO QCX-FREE2.
           MOVE CX-VOID TO QCX-VOID.
           MOVE CX-CLASS TO QCX-CLASS.
           MOVE CX-CKNO TO QCX-CKNO.
           MOVE CX-DLNO TO QCX-DLNO.

           IF ( CX-DATE = ZEROES )
              MOVE "1900-01-01" TO QCX-DATE
           ELSE
              MOVE CX-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCX-DATE.

           MOVE CX-PAID TO QCX-PAID.
           MOVE CX-PGM TO QCX-PGM.
           MOVE CX-BATCHID TO QCX-BATCHID.
           MOVE CX-BWPROC TO QCX-BWPROC.
           MOVE CX-CKNO-NEW TO QCX-CKNO-NEW.
           MOVE CX-DLRVNO TO QCX-DLRVNO.
           MOVE CX-DT-SEQ TO QCX-DT-SEQ.
           MOVE CX-RESCISSION TO QCX-RESCISSION.
           MOVE CX-TILLNO TO QCX-TILLNO.
           MOVE CX-ACCTNO TO QCX-ACCTNO.
           MOVE CX-SERVACCT TO QCX-SERVACCT.

           IF ( CX-DT-DATE = ZEROES )
              MOVE "1900-01-01" TO QCX-DT-DATE
           ELSE
              MOVE CX-DT-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCX-DT-DATE.

           MOVE CX-SSNO TO QCX-SSNO.
           MOVE CX-DT-PAID TO QCX-DT-PAID.
           MOVE CX-DT-UNPAID TO QCX-DT-UNPAID.
           MOVE CX-CKPAID-NEW TO QCX-CKPAID-NEW.
           MOVE CX-EVENT-CD TO QCX-EVENT-CD.
           MOVE CX-PAYEE-NO TO QCX-PAYEE-NO.
           MOVE CX-DEPOSIT-WITHDR TO QCX-DEPOSIT-WITHDR.
 
