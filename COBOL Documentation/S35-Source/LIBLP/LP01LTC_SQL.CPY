      * COPYMEMBER: LIBLP/LP01LTC_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL - LOAN TRAILER FILE - COLLATERAL
      *
      *=================================================================
      * REV:
      * BLM 2021-0504 ***NEW***
      *
      ******************************************************************
      *- - -{GREATER CURSOR FOR START-GREATER & READ-NEXT}
       01  LTC1-CURSOR-STAT             PIC X(2) VALUE SPACES.
           88  LTC1-CURSOR-STAT-GOOD             VALUE "00".
           88  LTC1-CURSOR-STAT-BAD              VALUE "  " THRU "//"
                                                       "01" THRU "~~".
       01  LTC2-CURSOR-STAT             PIC X(2) VALUE SPACES.
           88  LTC2-CURSOR-STAT-GOOD             VALUE "00".
           88  LTC2-CURSOR-STAT-BAD              VALUE "  " THRU "//"
                                                       "01" THRU "~~".
       01  LTC3-CURSOR-STAT             PIC X(2) VALUE SPACES.
           88  LTC3-CURSOR-STAT-GOOD             VALUE "00".
           88  LTC3-CURSOR-STAT-BAD              VALUE "  " THRU "//"
                                                       "01" THRU "~~".
       01  LTC4-CURSOR-STAT             PIC X(2) VALUE SPACES.
           88  LTC4-CURSOR-STAT-GOOD             VALUE "00".
           88  LTC4-CURSOR-STAT-BAD              VALUE "  " THRU "//"
                                                       "01" THRU "~~".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  LTC1-RETURN-CODE          PIC S9(4)   COMP-5.
       01  LTC2-RETURN-CODE          PIC S9(4)   COMP-5.
       01  LTC3-RETURN-CODE          PIC S9(4)   COMP-5.
       01  LTC4-RETURN-CODE          PIC S9(4)   COMP-5.

       01  QLTC1-WHERE-BEG-KEY.
           03  QLTC1-WBEG-BRNO             PIC 9(04).
           03  QLTC1-WBEG-ACCTNO           PIC 9(08).
           03  QLTC1-WBEG-SEQNO            PIC 9(03).
 
       01  QLTC1-WHERE-END-KEY.
           03  QLTC1-WEND-BRNO             PIC 9(04).
           03  QLTC1-WEND-ACCTNO           PIC 9(08).
           03  QLTC1-WEND-SEQNO            PIC 9(03).

       01  QLTC2-WHERE-BEG-KEY.
           03  QLTC2-WBEG-BRNO             PIC 9(04).
           03  QLTC2-WBEG-VIN-1            PIC X(20).
       01  QLTC2-WHERE-END-KEY.
           03  QLTC2-WEND-BRNO             PIC 9(04).
           03  QLTC2-WEND-VIN-1            PIC X(20).
 
       01  QLTC3-WHERE-BEG-KEY.
           03  QLTC3-WBEG-BRNO             PIC 9(04).
           03  QLTC3-WBEG-VIN-2            PIC X(20).
       01  QLTC3-WHERE-END-KEY.
           03  QLTC3-WEND-BRNO             PIC 9(04).
           03  QLTC3-WEND-VIN-2            PIC X(20).

       01  QLTC4-WHERE-BEG-KEY.
           03  QLTC4-WBEG-BRNO             PIC 9(04).
           03  QLTC4-WBEG-VIN-3            PIC X(20).
       01  QLTC4-WHERE-END-KEY.
           03  QLTC4-WEND-BRNO             PIC 9(04).
           03  QLTC4-WEND-VIN-3            PIC X(20).
 
 
       01  QLTC-REC.
           03  QLTC-BRNO                              PIC 9(4).
           03  QLTC-ACCTNO                            PIC 9(8).
           03  QLTC-SEQNO                             PIC 999.
           03  QLTC-LTOUCH-DATE                       PIC X(10).
           03  QLTC-DESC-1                            PIC X(15).
           03  QLTC-MAKE-1                            PIC X(15).
           03  QLTC-MODEL-1                           PIC X(15).
           03  QLTC-YEAR-1                            PIC X(4).
           03  QLTC-STYLE-1                           PIC X(8).
           03  QLTC-CLASS-1                           PIC X(8).
           03  QLTC-VIN-1                             PIC X(20).
           03  QLTC-TITLE-NO-1                        PIC X(20).
           03  QLTC-MILEAGE-1                         PIC S9(7).
           03  QLTC-VALUE-1                           PIC S9(7)V99.
           03  QLTC-SELLPR-1                          PIC S9(7)V99.
           03  QLTC-MISC-AMT-1                        PIC S9(7)V99.
           03  QLTC-SENT-FLAG-1                       PIC X.
           03  QLTC-VIN-CHG-DATE-1                    PIC X(10).
           03  QLTC-DESC-2                            PIC X(15).
           03  QLTC-MAKE-2                            PIC X(15).
           03  QLTC-MODEL-2                           PIC X(15).
           03  QLTC-YEAR-2                            PIC X(4).
           03  QLTC-STYLE-2                           PIC X(8).
           03  QLTC-CLASS-2                           PIC X(8).
           03  QLTC-VIN-2                             PIC X(20).
           03  QLTC-TITLE-NO-2                        PIC X(20).
           03  QLTC-MILEAGE-2                         PIC S9(7).
           03  QLTC-VALUE-2                           PIC S9(7)V99.
           03  QLTC-SELLPR-2                          PIC S9(7)V99.
           03  QLTC-MISC-AMT-2                        PIC S9(7)V99.
           03  QLTC-SENT-FLAG-2                       PIC X.
           03  QLTC-VIN-CHG-DATE-2                    PIC X(10).
           03  QLTC-DESC-3                            PIC X(15).
           03  QLTC-MAKE-3                            PIC X(15).
           03  QLTC-MODEL-3                           PIC X(15).
           03  QLTC-YEAR-3                            PIC X(4).
           03  QLTC-STYLE-3                           PIC X(8).
           03  QLTC-CLASS-3                           PIC X(8).
           03  QLTC-VIN-3                             PIC X(20).
           03  QLTC-TITLE-NO-3                        PIC X(20).
           03  QLTC-MILEAGE-3                         PIC S9(7).
           03  QLTC-VALUE-3                           PIC S9(7)V99.
           03  QLTC-SELLPR-3                          PIC S9(7)V99.
           03  QLTC-MISC-AMT-3                        PIC S9(7)V99.
           03  QLTC-SENT-FLAG-3                       PIC X.
           03  QLTC-VIN-CHG-DATE-3                    PIC X(10).
           03  QLTC-HHGS-1                            PIC X(80).
           03  QLTC-HHGS-2                            PIC X(80).
           03  QLTC-HHGS-3                            PIC X(80).

       EXEC SQL END DECLARE SECTION END-EXEC.
