      * COPYMEMBER: LIBLPLPCKGS_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL GET & SET - CKFILE
      *
      *=================================================================
      * REV:
      * BAH 2021-0510 ***NEW***
      *
      ******************************************************************
      *-----------------------------------------------------------------
      *    LOAD CK-REC FROM QCK-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-CK-FIELDS.
           MOVE QCK-BRNO TO CK-BRNO.
           MOVE QCK-NO TO CK-NO.

           IF ( QCK-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO CK-LTOUCH-DATE
           ELSE
              MOVE QCK-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO CK-LTOUCH-DATE.

           MOVE QCK-TYPE TO CK-TYPE.

           IF ( QCK-DATE = "1900-01-01" )
              MOVE ZEROES TO CK-DATE
           ELSE
              MOVE QCK-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO CK-DATE.

           MOVE QCK-AMT TO CK-AMT.
           MOVE QCK-PAYEE TO CK-PAYEE.
           MOVE QCK-ADR1 TO CK-ADR1.
           MOVE QCK-ADR2 TO CK-ADR2.
           MOVE QCK-CITY TO CK-CITY.
           MOVE QCK-STATE TO CK-STATE.
           MOVE QCK-ZIP TO CK-ZIP.
           MOVE QCK-REF TO CK-REF.
           MOVE QCK-REASON TO CK-REASON.
           MOVE QCK-GLNO-1 TO CK-GLNO(1).
           MOVE QCK-GLAMT-1 TO CK-GLAMT(1).
           MOVE QCK-GLNO-2 TO CK-GLNO(2).
           MOVE QCK-GLAMT-2 TO CK-GLAMT(2).
           MOVE QCK-GLNO-3 TO CK-GLNO(3).
           MOVE QCK-GLAMT-3 TO CK-GLAMT(3).
           MOVE QCK-GLNO-4 TO CK-GLNO(4).
           MOVE QCK-GLAMT-4 TO CK-GLAMT(4).
           MOVE QCK-GLNO-5 TO CK-GLNO(5).
           MOVE QCK-GLAMT-5 TO CK-GLAMT(5).
           MOVE QCK-VOID TO CK-VOID.
           MOVE QCK-PAID TO CK-PAID.
           MOVE QCK-CLASS TO CK-CLASS.
           MOVE QCK-DLNO TO CK-DLNO.
           MOVE QCK-PGM TO CK-PGM.
           MOVE QCK-NC-SEQNO TO CK-NC-SEQNO.

           IF ( QCK-VOID-DATE = "1900-01-01" )
              MOVE ZEROES TO CK-VOID-DATE
           ELSE
              MOVE QCK-VOID-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO CK-VOID-DATE.

           MOVE QCK-ACCTNO TO CK-ACCTNO.
           MOVE QCK-DLRVNO TO CK-DLRVNO.
           MOVE QCK-CLEARED-FG TO CK-CLEARED-FG.
           MOVE QCK-SSNO TO CK-SSNO.
           MOVE QCK-BATCHID TO CK-BATCHID.
           MOVE QCK-UNPAID TO CK-UNPAID.

           IF ( QCK-CLEARED-DATE = "1900-01-01" )
              MOVE ZEROES TO CK-CLEARED-DATE
           ELSE
              MOVE QCK-CLEARED-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO CK-CLEARED-DATE.

           MOVE QCK-TILLNO TO CK-TILLNO.
           MOVE QCK-EVENT-CD TO CK-EVENT-CD.
           MOVE QCK-PAYEE-NO TO CK-PAYEE-NO.
           MOVE QCK-FREE1 TO CK-FREE1.
           MOVE QCK-FREE2 TO CK-FREE2.
           MOVE QCK-BWPROC TO CK-BWPROC.
 
      *-----------------------------------------------------------------
      *    LOAD QCK-REC FROM CK-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-CK-FIELDS.
           MOVE CK-BRNO TO QCK-BRNO.
           MOVE CK-NO TO QCK-NO.

           IF ( CK-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QCK-LTOUCH-DATE
           ELSE
              MOVE CK-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCK-LTOUCH-DATE.

           MOVE CK-TYPE TO QCK-TYPE.

           IF ( CK-DATE = ZEROES )
              MOVE "1900-01-01" TO QCK-DATE
           ELSE
              MOVE CK-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCK-DATE.

           MOVE CK-AMT TO QCK-AMT.
           MOVE CK-PAYEE TO QCK-PAYEE.
           MOVE CK-ADR1 TO QCK-ADR1.
           MOVE CK-ADR2 TO QCK-ADR2.
           MOVE CK-CITY TO QCK-CITY.
           MOVE CK-STATE TO QCK-STATE.
           MOVE CK-ZIP TO QCK-ZIP.
           MOVE CK-REF TO QCK-REF.
           MOVE CK-REASON TO QCK-REASON.
           MOVE CK-GLNO(1) TO QCK-GLNO-1.
           MOVE CK-GLAMT(1) TO QCK-GLAMT-1.
           MOVE CK-GLNO(2) TO QCK-GLNO-2.
           MOVE CK-GLAMT(2) TO QCK-GLAMT-2.
           MOVE CK-GLNO(3) TO QCK-GLNO-3.
           MOVE CK-GLAMT(3) TO QCK-GLAMT-3.
           MOVE CK-GLNO(4) TO QCK-GLNO-4.
           MOVE CK-GLAMT(4) TO QCK-GLAMT-4.
           MOVE CK-GLNO(5) TO QCK-GLNO-5.
           MOVE CK-GLAMT(5) TO QCK-GLAMT-5.
           MOVE CK-VOID TO QCK-VOID.
           MOVE CK-PAID TO QCK-PAID.
           MOVE CK-CLASS TO QCK-CLASS.
           MOVE CK-DLNO TO QCK-DLNO.
           MOVE CK-PGM TO QCK-PGM.
           MOVE CK-NC-SEQNO TO QCK-NC-SEQNO.

           IF ( CK-VOID-DATE = ZEROES )
              MOVE "1900-01-01" TO QCK-VOID-DATE
           ELSE
              MOVE CK-VOID-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCK-VOID-DATE.

           MOVE CK-ACCTNO TO QCK-ACCTNO.
           MOVE CK-DLRVNO TO QCK-DLRVNO.
           MOVE CK-CLEARED-FG TO QCK-CLEARED-FG.
           MOVE CK-SSNO TO QCK-SSNO.
           MOVE CK-BATCHID TO QCK-BATCHID.
           MOVE CK-UNPAID TO QCK-UNPAID.

           IF ( CK-CLEARED-DATE = ZEROES )
              MOVE "1900-01-01" TO QCK-CLEARED-DATE
           ELSE
              MOVE CK-CLEARED-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCK-CLEARED-DATE.

           MOVE CK-TILLNO TO QCK-TILLNO.
           MOVE CK-EVENT-CD TO QCK-EVENT-CD.
           MOVE CK-PAYEE-NO TO QCK-PAYEE-NO.
           MOVE CK-FREE1 TO QCK-FREE1.
           MOVE CK-FREE2 TO QCK-FREE2.
           MOVE CK-BWPROC TO QCK-BWPROC.
 
