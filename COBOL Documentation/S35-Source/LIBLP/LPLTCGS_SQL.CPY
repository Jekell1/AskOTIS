      * COPYMEMBER: LIBLP/LPLTCGS_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL GET & SET - LOAN TRAILER - COLLATERAL
      *
      *=================================================================
      * REV:
      * AMM 2021-0510 ***NEW***
      *
      ******************************************************************
      *-----------------------------------------------------------------
      *    LOAD LTC-REC FROM QLTC-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-LTC-FIELDS.
           MOVE QLTC-BRNO TO LTC-BRNO.
           MOVE QLTC-ACCTNO TO LTC-ACCTNO.
           MOVE QLTC-SEQNO TO LTC-SEQNO.

           IF ( QLTC-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO LTC-LTOUCH-DATE
           ELSE
              MOVE QLTC-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LTC-LTOUCH-DATE.

           MOVE QLTC-DESC-1 TO LTC-DESC(1).
           MOVE QLTC-MAKE-1 TO LTC-MAKE(1).
           MOVE QLTC-MODEL-1 TO LTC-MODEL(1).
           MOVE QLTC-YEAR-1 TO LTC-YEAR(1).
           MOVE QLTC-STYLE-1 TO LTC-STYLE(1).
           MOVE QLTC-CLASS-1 TO LTC-CLASS(1).
           MOVE QLTC-VIN-1 TO LTC-VIN(1).
           MOVE QLTC-TITLE-NO-1 TO LTC-TITLE-NO(1).
           MOVE QLTC-MILEAGE-1 TO LTC-MILEAGE(1).
           MOVE QLTC-VALUE-1 TO LTC-VALUE(1).
           MOVE QLTC-SELLPR-1 TO LTC-SELLPR(1).
           MOVE QLTC-MISC-AMT-1 TO LTC-MISC-AMT(1).
           MOVE QLTC-SENT-FLAG-1 TO LTC-SENT-FLAG(1).

           IF ( QLTC-VIN-CHG-DATE-1 = "1900-01-01" )
              MOVE ZEROES TO LTC-VIN-CHG-DATE(1)
           ELSE
              MOVE QLTC-VIN-CHG-DATE-1 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LTC-VIN-CHG-DATE(1).

           MOVE QLTC-DESC-2 TO LTC-DESC(2).
           MOVE QLTC-MAKE-2 TO LTC-MAKE(2).
           MOVE QLTC-MODEL-2 TO LTC-MODEL(2).
           MOVE QLTC-YEAR-2 TO LTC-YEAR(2).
           MOVE QLTC-STYLE-2 TO LTC-STYLE(2).
           MOVE QLTC-CLASS-2 TO LTC-CLASS(2).
           MOVE QLTC-VIN-2 TO LTC-VIN(2).
           MOVE QLTC-TITLE-NO-2 TO LTC-TITLE-NO(2).
           MOVE QLTC-MILEAGE-2 TO LTC-MILEAGE(2).
           MOVE QLTC-VALUE-2 TO LTC-VALUE(2).
           MOVE QLTC-SELLPR-2 TO LTC-SELLPR(2).
           MOVE QLTC-MISC-AMT-2 TO LTC-MISC-AMT(2).
           MOVE QLTC-SENT-FLAG-2 TO LTC-SENT-FLAG(2).

           IF ( QLTC-VIN-CHG-DATE-2 = "1900-01-01" )
              MOVE ZEROES TO LTC-VIN-CHG-DATE(2)
           ELSE
              MOVE QLTC-VIN-CHG-DATE-2 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LTC-VIN-CHG-DATE(2).

           MOVE QLTC-DESC-3 TO LTC-DESC(3).
           MOVE QLTC-MAKE-3 TO LTC-MAKE(3).
           MOVE QLTC-MODEL-3 TO LTC-MODEL(3).
           MOVE QLTC-YEAR-3 TO LTC-YEAR(3).
           MOVE QLTC-STYLE-3 TO LTC-STYLE(3).
           MOVE QLTC-CLASS-3 TO LTC-CLASS(3).
           MOVE QLTC-VIN-3 TO LTC-VIN(3).
           MOVE QLTC-TITLE-NO-3 TO LTC-TITLE-NO(3).
           MOVE QLTC-MILEAGE-3 TO LTC-MILEAGE(3).
           MOVE QLTC-VALUE-3 TO LTC-VALUE(3).
           MOVE QLTC-SELLPR-3 TO LTC-SELLPR(3).
           MOVE QLTC-MISC-AMT-3 TO LTC-MISC-AMT(3).
           MOVE QLTC-SENT-FLAG-3 TO LTC-SENT-FLAG(3).

           IF ( QLTC-VIN-CHG-DATE-3 = "1900-01-01" )
              MOVE ZEROES TO LTC-VIN-CHG-DATE(3)
           ELSE
              MOVE QLTC-VIN-CHG-DATE-3 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LTC-VIN-CHG-DATE(3).

           MOVE QLTC-HHGS-1 TO LTC-HHGS(1).
           MOVE QLTC-HHGS-2 TO LTC-HHGS(2).
           MOVE QLTC-HHGS-3 TO LTC-HHGS(3).
 
      *-----------------------------------------------------------------
      *    LOAD QLTC-REC FROM LTC-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-LTC-FIELDS.
           MOVE LTC-BRNO TO QLTC-BRNO.
           MOVE LTC-ACCTNO TO QLTC-ACCTNO.
           MOVE LTC-SEQNO TO QLTC-SEQNO.

           IF ( LTC-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QLTC-LTOUCH-DATE
           ELSE
              MOVE LTC-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLTC-LTOUCH-DATE.

           MOVE LTC-DESC(1) TO QLTC-DESC-1.
           MOVE LTC-MAKE(1) TO QLTC-MAKE-1.
           MOVE LTC-MODEL(1) TO QLTC-MODEL-1.
           MOVE LTC-YEAR(1) TO QLTC-YEAR-1.
           MOVE LTC-STYLE(1) TO QLTC-STYLE-1.
           MOVE LTC-CLASS(1) TO QLTC-CLASS-1.
           MOVE LTC-VIN(1) TO QLTC-VIN-1.
           MOVE LTC-TITLE-NO(1) TO QLTC-TITLE-NO-1.
           MOVE LTC-MILEAGE(1) TO QLTC-MILEAGE-1.
           MOVE LTC-VALUE(1) TO QLTC-VALUE-1.
           MOVE LTC-SELLPR(1) TO QLTC-SELLPR-1.
           MOVE LTC-MISC-AMT(1) TO QLTC-MISC-AMT-1.
           MOVE LTC-SENT-FLAG(1) TO QLTC-SENT-FLAG-1.

           IF ( LTC-VIN-CHG-DATE(1) = ZEROES )
              MOVE "1900-01-01" TO QLTC-VIN-CHG-DATE-1
           ELSE
              MOVE LTC-VIN-CHG-DATE(1) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLTC-VIN-CHG-DATE-1.

           MOVE LTC-DESC(2) TO QLTC-DESC-2.
           MOVE LTC-MAKE(2) TO QLTC-MAKE-2.
           MOVE LTC-MODEL(2) TO QLTC-MODEL-2.
           MOVE LTC-YEAR(2) TO QLTC-YEAR-2.
           MOVE LTC-STYLE(2) TO QLTC-STYLE-2.
           MOVE LTC-CLASS(2) TO QLTC-CLASS-2.
           MOVE LTC-VIN(2) TO QLTC-VIN-2.
           MOVE LTC-TITLE-NO(2) TO QLTC-TITLE-NO-2.
           MOVE LTC-MILEAGE(2) TO QLTC-MILEAGE-2.
           MOVE LTC-VALUE(2) TO QLTC-VALUE-2.
           MOVE LTC-SELLPR(2) TO QLTC-SELLPR-2.
           MOVE LTC-MISC-AMT(2) TO QLTC-MISC-AMT-2.
           MOVE LTC-SENT-FLAG(2) TO QLTC-SENT-FLAG-2.

           IF ( LTC-VIN-CHG-DATE(2) = ZEROES )
              MOVE "1900-01-01" TO QLTC-VIN-CHG-DATE-2
           ELSE
              MOVE LTC-VIN-CHG-DATE(2) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLTC-VIN-CHG-DATE-2.

           MOVE LTC-DESC(3) TO QLTC-DESC-3.
           MOVE LTC-MAKE(3) TO QLTC-MAKE-3.
           MOVE LTC-MODEL(3) TO QLTC-MODEL-3.
           MOVE LTC-YEAR(3) TO QLTC-YEAR-3.
           MOVE LTC-STYLE(3) TO QLTC-STYLE-3.
           MOVE LTC-CLASS(3) TO QLTC-CLASS-3.
           MOVE LTC-VIN(3) TO QLTC-VIN-3.
           MOVE LTC-TITLE-NO(3) TO QLTC-TITLE-NO-3.
           MOVE LTC-MILEAGE(3) TO QLTC-MILEAGE-3.
           MOVE LTC-VALUE(3) TO QLTC-VALUE-3.
           MOVE LTC-SELLPR(3) TO QLTC-SELLPR-3.
           MOVE LTC-MISC-AMT(3) TO QLTC-MISC-AMT-3.
           MOVE LTC-SENT-FLAG(3) TO QLTC-SENT-FLAG-3.

           IF ( LTC-VIN-CHG-DATE(3) = ZEROES )
              MOVE "1900-01-01" TO QLTC-VIN-CHG-DATE-3
           ELSE
              MOVE LTC-VIN-CHG-DATE(3) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLTC-VIN-CHG-DATE-3.

           MOVE LTC-HHGS(1) TO QLTC-HHGS-1.
           MOVE LTC-HHGS(2) TO QLTC-HHGS-2.
           MOVE LTC-HHGS(3) TO QLTC-HHGS-3.
 
