      * COPYMEMBER: LIBLP/LPLTAGS_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL GET & SET - LOAN TRAILER - AUTO INSURANCE
      *
      *=================================================================
      * REV:
      * AMM 2021-0510 ***NEW***
      *
      ******************************************************************
      *-----------------------------------------------------------------
      *    LOAD LTA-REC FROM QLTA-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-LTA-FIELDS.
           MOVE QLTA-BRNO TO LTA-BRNO.
           MOVE QLTA-ACCTNO TO LTA-ACCTNO.
           MOVE QLTA-SEQNO TO LTA-SEQNO.

           IF ( QLTA-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO LTA-LTOUCH-DATE
           ELSE
              MOVE QLTA-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LTA-LTOUCH-DATE.

           MOVE QLTA-DLR-REQ TO LTA-DLR-REQ.
           MOVE QLTA-INS-TYPE TO LTA-INS-TYPE.
           MOVE QLTA-INS-CO TO LTA-INS-CO.
           MOVE QLTA-POLICY-NO TO LTA-POLICY-NO.
           MOVE QLTA-AGENCY TO LTA-AGENCY.
           MOVE QLTA-AGENT TO LTA-AGENT.
           MOVE QLTA-ADR1 TO LTA-ADR1.
           MOVE QLTA-ADR2 TO LTA-ADR2.
           MOVE QLTA-CITY TO LTA-CITY.
           MOVE QLTA-STATE TO LTA-STATE.
           MOVE QLTA-ZIP TO LTA-ZIP.
           MOVE QLTA-PHONE TO LTA-PHONE.

           IF ( QLTA-EFF-DATE = "1900-01-01" )
              MOVE ZEROES TO LTA-EFF-DATE
           ELSE
              MOVE QLTA-EFF-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LTA-EFF-DATE.


           IF ( QLTA-EXP-DATE = "1900-01-01" )
              MOVE ZEROES TO LTA-EXP-DATE
           ELSE
              MOVE QLTA-EXP-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LTA-EXP-DATE.


           IF ( QLTA-CAN-DATE = "1900-01-01" )
              MOVE ZEROES TO LTA-CAN-DATE
           ELSE
              MOVE QLTA-CAN-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LTA-CAN-DATE.


           IF ( QLTA-CONF-DATE = "1900-01-01" )
              MOVE ZEROES TO LTA-CONF-DATE
           ELSE
              MOVE QLTA-CONF-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LTA-CONF-DATE.

           MOVE QLTA-LAST-FORM TO LTA-LAST-FORM.
           MOVE QLTA-COV-TYPE TO LTA-COV-TYPE.
           MOVE QLTA-BIL-COV TO LTA-BIL-COV.
           MOVE QLTA-PDL-COV TO LTA-PDL-COV.
           MOVE QLTA-PIP-COV TO LTA-PIP-COV.
           MOVE QLTA-UMB-COV TO LTA-UMB-COV.
           MOVE QLTA-COL-COV TO LTA-COL-COV.
           MOVE QLTA-LOSS-PAYEE TO LTA-LOSS-PAYEE.
           MOVE QLTA-COM-COV TO LTA-COM-COV.
           MOVE QLTA-TRACK-ONLY TO LTA-TRACK-ONLY.
           MOVE QLTA-INSURANCE TO LTA-INSURANCE.
           MOVE QLTA-TRACK-MONTHS TO LTA-TRACK-MONTHS.
           MOVE QLTA-INS-MONTHS TO LTA-INS-MONTHS.
           MOVE QLTA-CPP-MONTHS TO LTA-CPP-MONTHS.
 
      *-----------------------------------------------------------------
      *    LOAD QLTA-REC FROM LTA-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-LTA-FIELDS.
           MOVE LTA-BRNO TO QLTA-BRNO.
           MOVE LTA-ACCTNO TO QLTA-ACCTNO.
           MOVE LTA-SEQNO TO QLTA-SEQNO.

           IF ( LTA-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QLTA-LTOUCH-DATE
           ELSE
              MOVE LTA-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLTA-LTOUCH-DATE.

           MOVE LTA-DLR-REQ TO QLTA-DLR-REQ.
           MOVE LTA-INS-TYPE TO QLTA-INS-TYPE.
           MOVE LTA-INS-CO TO QLTA-INS-CO.
           MOVE LTA-POLICY-NO TO QLTA-POLICY-NO.
           MOVE LTA-AGENCY TO QLTA-AGENCY.
           MOVE LTA-AGENT TO QLTA-AGENT.
           MOVE LTA-ADR1 TO QLTA-ADR1.
           MOVE LTA-ADR2 TO QLTA-ADR2.
           MOVE LTA-CITY TO QLTA-CITY.
           MOVE LTA-STATE TO QLTA-STATE.
           MOVE LTA-ZIP TO QLTA-ZIP.
           MOVE LTA-PHONE TO QLTA-PHONE.

           IF ( LTA-EFF-DATE = ZEROES )
              MOVE "1900-01-01" TO QLTA-EFF-DATE
           ELSE
              MOVE LTA-EFF-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLTA-EFF-DATE.


           IF ( LTA-EXP-DATE = ZEROES )
              MOVE "1900-01-01" TO QLTA-EXP-DATE
           ELSE
              MOVE LTA-EXP-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLTA-EXP-DATE.


           IF ( LTA-CAN-DATE = ZEROES )
              MOVE "1900-01-01" TO QLTA-CAN-DATE
           ELSE
              MOVE LTA-CAN-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLTA-CAN-DATE.


           IF ( LTA-CONF-DATE = ZEROES )
              MOVE "1900-01-01" TO QLTA-CONF-DATE
           ELSE
              MOVE LTA-CONF-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLTA-CONF-DATE.

           MOVE LTA-LAST-FORM TO QLTA-LAST-FORM.
           MOVE LTA-COV-TYPE TO QLTA-COV-TYPE.
           MOVE LTA-BIL-COV TO QLTA-BIL-COV.
           MOVE LTA-PDL-COV TO QLTA-PDL-COV.
           MOVE LTA-PIP-COV TO QLTA-PIP-COV.
           MOVE LTA-UMB-COV TO QLTA-UMB-COV.
           MOVE LTA-COL-COV TO QLTA-COL-COV.
           MOVE LTA-LOSS-PAYEE TO QLTA-LOSS-PAYEE.
           MOVE LTA-COM-COV TO QLTA-COM-COV.
           MOVE LTA-TRACK-ONLY TO QLTA-TRACK-ONLY.
           MOVE LTA-INSURANCE TO QLTA-INSURANCE.
           MOVE LTA-TRACK-MONTHS TO QLTA-TRACK-MONTHS.
           MOVE LTA-INS-MONTHS TO QLTA-INS-MONTHS.
           MOVE LTA-CPP-MONTHS TO QLTA-CPP-MONTHS.
 
