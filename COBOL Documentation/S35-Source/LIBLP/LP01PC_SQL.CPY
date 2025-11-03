      * COPYMEMBER: LIBLP/LP01PC_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL - LOAN SCRATCH PAD COLLATERAL FILE
      *
      *=================================================================
      * REV:
      * BLM 2021-0503 ***NEW***
      *
      ******************************************************************
      *- - -{GREATER CURSOR FOR START-GREATER & READ-NEXT}
       01  PC1-CURSOR-STAT              PIC X(2) VALUE SPACES.
           88  PC1-CURSOR-STAT-GOOD              VALUE "00".
           88  PC1-CURSOR-STAT-BAD               VALUE "  " THRU "//"
                                                       "01" THRU "~~".
       01  PC2-CURSOR-STAT              PIC X(2) VALUE SPACES.
           88  PC2-CURSOR-STAT-GOOD              VALUE "00".
           88  PC2-CURSOR-STAT-BAD               VALUE "  " THRU "//"
                                                       "01" THRU "~~".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  PC1-RETURN-CODE                PIC S9(4)   COMP-5.
       01  PC2-RETURN-CODE                PIC S9(4)   COMP-5.

       01  QPC1-WHERE-BEG-KEY.
           03  QPC1-WBEG-ID               PIC X.
           03  QPC1-WBEG-BRANCH           PIC 9(04).
           03  QPC1-WBEG-SSNO             PIC 9(10).
 
       01  QPC1-WHERE-END-KEY.
           03  QPC1-WEND-ID               PIC X.
           03  QPC1-WEND-BRANCH           PIC 9(04).
           03  QPC1-WEND-SSNO             PIC 9(10).

       02  QPC2-WHERE-BEG-KEY.
           03  QPC2-WBEG-ID               PIC X.
           03  QPC2-WBEG-BRANCH           PIC 9(04).
           03  QPC2-WBEG-BWNAME             PIC X(33).
 
       02  QPC2-WHERE-END-KEY.
           03  QPC2-WEND-ID               PIC X.
           03  QPC2-WEND-BRANCH           PIC 9(04).
           03  QPC2-WEND-BWNAME             PIC X(33).
 
       01  QPC-REC.
           03  QPC-ID               PIC X.
           03  QPC-BRANCH           PIC 9(4).
           03  QPC-SOC-SEC          PIC 9(10).
           03  QPC-LTC-BRNO          PIC 9(4).
           03  QPC-LTC-ACCTNO        PIC 9(8).
           03  QPC-LTC-SEQNO         PIC 999.
           03  QPC-LTC-LTOUCH-DATE   PIC X(10).
           03  QPC-LTC-DESC-1        PIC X(15).
           03  QPC-LTC-MAKE-1        PIC X(15).
           03  QPC-LTC-MODEL-1       PIC X(15).
           03  QPC-LTC-YEAR-1        PIC X(4).
           03  QPC-LTC-STYLE-1       PIC X(8).
           03  QPC-LTC-CLASS-1       PIC X(8).
           03  QPC-LTC-VIN-1         PIC X(20).
           03  QPC-LTC-TITLE-NO-1    PIC X(20).
           03  QPC-LTC-MILEAGE-1     PIC S9(7).
           03  QPC-LTC-VALUE-1       PIC S9(7)V99.
           03  QPC-LTC-SELLPR-1      PIC S9(7)V99.
           03  QPC-LTC-MISC-AMT-1    PIC S9(7)V99.
           03  QPC-LTC-SENT-FLAG-1   PIC X.
           03  QPC-LTC-VIN-CHG-DATE-1 PIC X(10).
           03  QPC-LTC-DESC-2         PIC X(15).
           03  QPC-LTC-MAKE-2         PIC X(15).
           03  QPC-LTC-MODEL-2        PIC X(15).
           03  QPC-LTC-YEAR-2         PIC X(4).
           03  QPC-LTC-STYLE-2        PIC X(8).
           03  QPC-LTC-CLASS-2        PIC X(8).
           03  QPC-LTC-VIN-2          PIC X(20).
           03  QPC-LTC-TITLE-NO-2     PIC X(20).
           03  QPC-LTC-MILEAGE-2      PIC S9(7).
           03  QPC-LTC-VALUE-2        PIC S9(7)V99.
           03  QPC-LTC-SELLPR-2       PIC S9(7)V99.
           03  QPC-LTC-MISC-AMT-2     PIC S9(7)V99.
           03  QPC-LTC-SENT-FLAG-2    PIC X.
           03  QPC-LTC-VIN-CHG-DATE-2 PIC X(10).
           03  QPC-LTC-DESC-3         PIC X(15).
           03  QPC-LTC-MAKE-3         PIC X(15).
           03  QPC-LTC-MODEL-3        PIC X(15).
           03  QPC-LTC-YEAR-3         PIC X(4).
           03  QPC-LTC-STYLE-3        PIC X(8).
           03  QPC-LTC-CLASS-3        PIC X(8).
           03  QPC-LTC-VIN-3          PIC X(20).
           03  QPC-LTC-TITLE-NO-3     PIC X(20).
           03  QPC-LTC-MILEAGE-3      PIC S9(7).
           03  QPC-LTC-VALUE-3        PIC S9(7)V99.
           03  QPC-LTC-SELLPR-3       PIC S9(7)V99.
           03  QPC-LTC-MISC-AMT-3     PIC S9(7)V99.
           03  QPC-LTC-SENT-FLAG-3    PIC X.
           03  QPC-LTC-VIN-CHG-DATE-3 PIC X(10).
           03  QPC-LTC-HHGS-1         PIC X(80).
           03  QPC-LTC-HHGS-2         PIC X(80).
           03  QPC-LTC-HHGS-3         PIC X(80).
           03  QPC-LTA-BRNO           PIC 9(4).
           03  QPC-LTA-ACCTNO         PIC 9(8).
           03  QPC-LTA-SEQNO          PIC 999.
           03  QPC-LTA-LTOUCH-DATE    PIC X(10).
           03  QPC-LTA-DLR-REQ        PIC X.
           03  QPC-LTA-INS-TYPE       PIC X.
           03  QPC-LTA-INS-CO         PIC X(25).
           03  QPC-LTA-POLICY-NO      PIC X(25).
           03  QPC-LTA-AGENCY         PIC X(25).
           03  QPC-LTA-AGENT          PIC X(25).
           03  QPC-LTA-ADR1           PIC X(25).
           03  QPC-LTA-ADR2           PIC X(25).
           03  QPC-LTA-CITY           PIC X(15).
           03  QPC-LTA-STATE          PIC XX.
           03  QPC-LTA-ZIP            PIC X(10).
           03  QPC-LTA-PHONE          PIC 9(10).
           03  QPC-LTA-EFF-DATE       PIC X(10).
           03  QPC-LTA-EXP-DATE       PIC X(10).
           03  QPC-LTA-CAN-DATE       PIC X(10).
           03  QPC-LTA-CONF-DATE      PIC X(10).
           03  QPC-LTA-LAST-FORM      PIC 9(4).
           03  QPC-LTA-COV-TYPE       PIC X.
           03  QPC-LTA-BIL-COV        PIC S9(7)V99.
           03  QPC-LTA-PDL-COV        PIC S9(7)V99.
           03  QPC-LTA-PIP-COV        PIC S9(7)V99.
           03  QPC-LTA-UMB-COV        PIC S9(7)V99.
           03  QPC-LTA-COL-COV        PIC S9(7)V99.
           03  QPC-LTA-LOSS-PAYEE     PIC X(25).
           03  QPC-LTA-COM-COV        PIC S9(7)V99.
           03  QPC-LTA-TRACK-ONLY     PIC X.
           03  QPC-LTA-INSURANCE      PIC X.
           03  QPC-LTA-TRACK-MONTHS   PIC X(12).
           03  QPC-LTA-INS-MONTHS     PIC X(12).
           03  QPC-LTA-CPP-MONTHS     PIC X(12).
           03  QPC-LOANDATE             PIC X(10).
           03  QPC-ORGTERM              PIC 9(3).
           03  QPC-REGPYAMT             PIC 9(6)V99.
           03  QPC2-ID                  PIC X.
           03  QPC2-BRANCH              PIC 9(4).
           03  QPC2-BWNAME              PIC X(33).
           03  QPC-LTOUCH-DATE          PIC X(10).

       EXEC SQL END DECLARE SECTION END-EXEC.
