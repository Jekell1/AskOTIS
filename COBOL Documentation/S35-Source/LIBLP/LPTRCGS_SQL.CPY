      * COPYMEMBER: LIBLP/LPTRCGS_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL GET & SET - TRCFILE
      *
      *=================================================================
      * REV:
      * BAH 2021-0510 ***NEW***
      * BAH 2021-1027 ADDED QTC-EXTRACT-FG AND QTC-DEBIT-REFCD 
      *
      ******************************************************************
      *-----------------------------------------------------------------
      *    LOAD TRC-REC FROM QTRC-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-TRC-FIELDS.
           MOVE QTC-BRNO TO TC-BRNO.

           IF ( QTC-KEY-DATE = "1900-01-01" )
              MOVE ZEROES TO TC-KEY-DATE
           ELSE
              MOVE QTC-KEY-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO TC-KEY-DATE.

           MOVE QTC-CKNO TO TC-CKNO.
           MOVE QTC-CLASS TO TC-CLASS.
           MOVE QTC-SEQ TO TC-SEQ.

           IF ( QTC-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO TC-LTOUCH-DATE
           ELSE
              MOVE QTC-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO TC-LTOUCH-DATE.

           MOVE QTC-TYPE TO TC-TYPE.

           IF ( QTC-DATE = "1900-01-01" )
              MOVE ZEROES TO TC-DATE
           ELSE
              MOVE QTC-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO TC-DATE.

           MOVE QTC-AMT TO TC-AMT.
           MOVE QTC-PAYEE TO TC-PAYEE.
           MOVE QTC-REF TO TC-REF.
           MOVE QTC-REASON TO TC-REASON.
           MOVE QTC-GLNO-1 TO TC-GLNO(1).
           MOVE QTC-GLAMT-1 TO TC-GLAMT(1).
           MOVE QTC-GLNO-2 TO TC-GLNO(2).
           MOVE QTC-GLAMT-2 TO TC-GLAMT(2).
           MOVE QTC-GLNO-3 TO TC-GLNO(3).
           MOVE QTC-GLAMT-3 TO TC-GLAMT(3).
           MOVE QTC-GLNO-4 TO TC-GLNO(4).
           MOVE QTC-GLAMT-4 TO TC-GLAMT(4).
           MOVE QTC-GLNO-5 TO TC-GLNO(5).
           MOVE QTC-GLAMT-5 TO TC-GLAMT(5).
           MOVE QTC-VOID TO TC-VOID.
           MOVE QTC-ACCTNO TO TC-ACCTNO.
           MOVE QTC-PAID TO TC-PAID.
           MOVE QTC-PGM TO TC-PGM.
           MOVE QTC-DLRVNO TO TC-DLRVNO.
           MOVE QTC-SPRCLASS TO TC-SPRCLASS.
           MOVE QTC-DLNO TO TC-DLNO.
           MOVE QTC-USERID TO TC-USERID.

           IF ( QTC-POSTDATE = "1900-01-01" )
              MOVE ZEROES TO TC-POSTDATE
           ELSE
              MOVE QTC-POSTDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO TC-POSTDATE.

           MOVE QTC-POSTTIME TO TC-POSTTIME.
           MOVE QTC-TILLNO TO TC-TILLNO.
           MOVE QTC-ADR1 TO TC-ADR1.
           MOVE QTC-ADR2        TO TC-ADR2.
           MOVE QTC-CITY        TO TC-CITY.
           MOVE QTC-STATE       TO TC-STATE.
           MOVE QTC-ZIP         TO TC-ZIP.
           MOVE QTC-PAYEE-NO    TO TC-PAYEE-NO.
           MOVE QTC-ACH-FG      TO TC-ACH-FG.
           MOVE QTC-EXTRACT-FG  TO TC-EXTRACT-FG.
           MOVE QTC-DEBIT-REFCD TO TC-DEBIT-REFCD.
 
      *-----------------------------------------------------------------
      *    LOAD QTRC-REC FROM TRC-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-TRC-FIELDS.
           MOVE TC-BRNO TO QTC-BRNO.

           IF ( TC-KEY-DATE = ZEROES )
              MOVE "1900-01-01" TO QTC-KEY-DATE
           ELSE
              MOVE TC-KEY-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QTC-KEY-DATE.

           MOVE TC-CKNO TO QTC-CKNO.
           MOVE TC-CLASS TO QTC-CLASS.
           MOVE TC-SEQ TO QTC-SEQ.

           IF ( TC-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QTC-LTOUCH-DATE
           ELSE
              MOVE TC-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QTC-LTOUCH-DATE.

           MOVE TC-TYPE TO QTC-TYPE.

           IF ( TC-DATE = ZEROES )
              MOVE "1900-01-01" TO QTC-DATE
           ELSE
              MOVE TC-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QTC-DATE.

           MOVE TC-AMT TO QTC-AMT.
           MOVE TC-PAYEE TO QTC-PAYEE.
           MOVE TC-REF TO QTC-REF.
           MOVE TC-REASON TO QTC-REASON.
           MOVE TC-GLNO(1) TO QTC-GLNO-1.
           MOVE TC-GLAMT(1) TO QTC-GLAMT-1.
           MOVE TC-GLNO(2) TO QTC-GLNO-2.
           MOVE TC-GLAMT(2) TO QTC-GLAMT-2.
           MOVE TC-GLNO(3) TO QTC-GLNO-3.
           MOVE TC-GLAMT(3) TO QTC-GLAMT-3.
           MOVE TC-GLNO(4) TO QTC-GLNO-4.
           MOVE TC-GLAMT(4) TO QTC-GLAMT-4.
           MOVE TC-GLNO(5) TO QTC-GLNO-5.
           MOVE TC-GLAMT(5) TO QTC-GLAMT-5.
           MOVE TC-VOID TO QTC-VOID.
           MOVE TC-ACCTNO TO QTC-ACCTNO.
           MOVE TC-PAID TO QTC-PAID.
           MOVE TC-PGM TO QTC-PGM.
           MOVE TC-DLRVNO TO QTC-DLRVNO.
           MOVE TC-SPRCLASS TO QTC-SPRCLASS.
           MOVE TC-DLNO TO QTC-DLNO.
           MOVE TC-USERID TO QTC-USERID.

           IF ( TC-POSTDATE = ZEROES )
              MOVE "1900-01-01" TO QTC-POSTDATE
           ELSE
              MOVE TC-POSTDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QTC-POSTDATE.

           MOVE TC-POSTTIME TO QTC-POSTTIME.
           MOVE TC-TILLNO TO QTC-TILLNO.
           MOVE TC-ADR1 TO QTC-ADR1.
           MOVE TC-ADR2 TO QTC-ADR2.
           MOVE TC-CITY TO QTC-CITY.
           MOVE TC-STATE TO QTC-STATE.
           MOVE TC-ZIP TO QTC-ZIP.
           MOVE TC-PAYEE-NO TO QTC-PAYEE-NO.
           MOVE TC-ACH-FG TO QTC-ACH-FG.
           MOVE TC-EXTRACT-FG TO QTC-EXTRACT-FG.
           MOVE TC-DEBIT-REFCD TO QTC-DEBIT-REFCD.
