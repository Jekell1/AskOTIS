      * COPYMEMBER: LIBLP/LP01EA_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL - EXTRACTION ACTION FILE
      *
      *=================================================================
      * REV:
      * AMM 2021-0430 ***NEW***
      *
      ******************************************************************
      *- - -{STANDARD CURSOR FOR START & READ-NEXT}
       01  EA1-CURSOR-STAT              PIC X(2) VALUE SPACES.
           88  EA1-CURSOR-STAT-GOOD              VALUE "00".
           88  EA1-CURSOR-STAT-BAD               VALUE "  " THRU "//"
                                                       "01" THRU "~~".

      *- - -{GREATER CURSOR FOR START-GREATER & READ-NEXT}
       01  EA1-CURSOR-GR-STAT           PIC X(2) VALUE SPACES.
           88  EA1-CURSOR-GR-STAT-GOOD           VALUE "00".
           88  EA1-CURSOR-GR-STAT-BAD            VALUE "  " THRU "//"
                                                       "01" THRU "~~".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  EA1-RETURN-CODE              PIC S9(4)   COMP-5.

       01  QEA1-WHERE-BEG-KEY.
           03  QEA1-WBEG-BRANCH         PIC 9(04).
           03  QEA1-WBEG-CLASS          PIC 9(03).
           03  QEA1-WBEG-GROUP-ID       PIC X(06).
           03  QEA1-WBEG-SEQNO          PIC 99.

       01  QEA1-WHERE-END-KEY.
           03  QEA1-WEND-BRANCH         PIC 9(04).
           03  QEA1-WEND-CLASS          PIC 9(03).
           03  QEA1-WEND-GROUP-ID       PIC X(06).
           03  QEA1-WEND-SEQNO          PIC 99.

       01  QEA-REC.
           03  QEA-BRANCH              PIC 9(4).
           03  QEA-CLASS               PIC 9(3).
           03  QEA-GROUP-ID            PIC X(6).
           03  QEA-SEQNO               PIC 99.
           03  QEA-LTOUCH-DATE         PIC X(10).
           03  QEA-DESC                PIC X(60).
           03  QEA-PASTDUE-DAY1        PIC 9(4).
           03  QEA-PASTDUE-DAY2        PIC 9(4).
           03  QEA-PASTDUE-AMT1        PIC 9(4)V99.
           03  QEA-PASTDUE-AMT2        PIC 9(4)V99.
           03  QEA-ACCT-BAL1           PIC 9(6)V99.
           03  QEA-ACCT-BAL2           PIC 9(6)V99.
           03  QEA-MATURED             PIC X.
           03  QEA-PROMISED            PIC X.
           03  QEA-ACTION              PIC X(4).
           03  QEA-LSTFM-DATE1         PIC X(10).
           03  QEA-LSTFM-DATE2         PIC X(10).
           03  QEA-LSTFM-NO1           PIC 9(4).
           03  QEA-LSTFM-NO2           PIC 9(4).
           03  QEA-PASTPROM-DAY1       PIC S9(5).
           03  QEA-PASTPROM-DAY2       PIC S9(5).
           03  QEA-RECENCY-DAY1        PIC 9(4).
           03  QEA-RECENCY-DAY2        PIC 9(4).
           03  QEA-ALPNAME1            PIC X(4).
           03  QEA-ALPNAME2            PIC X(4).
           03  QEA-RECENCYDATE         PIC X.
           03  QEA-DLNO1               PIC 9(4)V99.
           03  QEA-DLNO2               PIC 9(4)V99.
           03  QEA-CONTRACTUAL-DAY1    PIC S9(5).
           03  QEA-CONTRACTUAL-DAY2    PIC S9(5).
           03  QEA-SETFM-NO            PIC 9(4).
           03  QEA-SETACT-CD           PIC X(4).
           03  QEA-INC-CNT             PIC X.
           03  QEA-INC-PROMISED        PIC 9(2).
           03  QEA-SETFM-1             PIC 9(4).
           03  QEA-SETFM-2             PIC 9(4).
           03  QEA-SETFM-3             PIC 9(4).
           03  QEA-SET-LN-COL          PIC X.
           03  QEA-LN-COL-ID           PIC XXX.
           03  QEA-CQ-FG               PIC X.
           03  QEA-COL-POOL            PIC X(3).
           03  QEA-CPOTS               PIC X.
           03  QEA-REPO                PIC X.
           03  QEA-PL                  PIC X.
           03  QEA-BANKRUPT            PIC X.
           03  QEA-PL-IN-ACTIVE-BOTH   PIC X.
           03  QEA-CONFIDENTIAL-FG     PIC X.
           03  QEA-1ST-PMT-MISSES      PIC X.
           03  QEA-LEGAL               PIC X.
           03  QEA-DEBTAID             PIC X.
           03  QEA-JUDGEMENT           PIC X.
           03  QEA-BROKEN-PROM-FG      PIC X.
           03  QEA-PROM-BREAK1         PIC S9(5).
           03  QEA-PROM-BREAK2         PIC S9(5).
           03  QEA-ORIGIN              PIC X(4).
           03  QEA-PL-BEG-DATE         PIC X(10).
           03  QEA-PL-END-DATE         PIC X(10).
           03  QEA-RPOTS               PIC X.
           03  QEA-MISPAYS             PIC X.
           03  QEA-ALLOTMENTS          PIC X.
           03  QEA-LNDATE-DAYS1        PIC 9(4).
           03  QEA-LNDATE-DAYS2        PIC 9(4).
           03  QEA-MATURE-DAYS1        PIC 9(4).
           03  QEA-MATURE-DAYS2        PIC 9(4).
           03  QEA-DAY-OF-MO-DUE1      PIC 9(2).
           03  QEA-DAY-OF-MO-DUE2      PIC 9(2).
           03  QEA-INCL-COLLECTOR      PIC X(3).
           03  QEA-BORR-STATE          PIC X(2).


       EXEC SQL END DECLARE SECTION END-EXEC.
