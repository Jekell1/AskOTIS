      * COPYMEMBER: LIBLP/LP01TRB_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL - DAILY TRANSACTION FILE - BULK TRANSFERS
      *
      *=================================================================
      * REV:
      * BLM 2021-0503 ***NEW***
      *
      ******************************************************************
      *- - -{GREATER CURSOR FOR START-GREATER & READ-NEXT}
       01  TRB1-CURSOR-STAT             PIC X(2) VALUE SPACES.
           88  TRB1-CURSOR-STAT-GOOD             VALUE "00".
           88  TRB1-CURSOR-STAT-BAD              VALUE "  " THRU "//"
                                                       "01" THRU "~~".
       01  TRB2-CURSOR-STAT             PIC X(2) VALUE SPACES.
           88  TRB2-CURSOR-STAT-GOOD             VALUE "00".
           88  TRB2-CURSOR-STAT-BAD              VALUE "  " THRU "//"
                                                       "01" THRU "~~".
       01  TRB1-CURSOR-SC-STAT          PIC X(2) VALUE SPACES.
           88  TRB1-CURSOR-SC-STAT-GOOD          VALUE "00".
           88  TRB1-CURSOR-SC-STAT-BAD           VALUE "  " THRU "//"
                                                       "01" THRU "~~".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  TRB1-RETURN-CODE                 PIC S9(4)   COMP-5.
       01  TRB2-RETURN-CODE                 PIC S9(4)   COMP-5.

       01  QTRB1-WHERE-BEG-KEY.
           03  QTRB1-WBEG-BRNO             PIC 9(04).
           03  QTRB1-WBEG-DATE             PIC X(10).
           03  QTRB1-WBEG-CLASS            PIC 9(03).
           03  QTRB1-WBEG-ACCTNO           PIC 9(08).
           03  QTRB1-WBEG-LAWCODE          PIC 9(02).
           03  QTRB1-WBEG-LINE             PIC 9(03).
 
       01  QTRB1-WHERE-END-KEY.
           03  QTRB1-WEND-BRNO             PIC 9(04).
           03  QTRB1-WEND-DATE             PIC X(10).
           03  QTRB1-WEND-CLASS            PIC 9(03).
           03  QTRB1-WEND-ACCTNO           PIC 9(08).
           03  QTRB1-WEND-LAWCODE          PIC 9(02).
           03  QTRB1-WEND-LINE             PIC 9(03).

       01  QTRB1-WHERE-SCAN-BEG-KEY.
           03  QTRB1-WSBEG-DATE            PIC X(10).
           03  QTRB1-WSBEG-CLASS           PIC 9(03).
           03  QTRB1-WSBEG-ACCTNO          PIC 9(08).
           03  QTRB1-WSBEG-LAWCODE         PIC 9(02).

       01  QTRB2-WHERE-BEG-KEY.
           03  QTRB2-WBEG-DATE             PIC X(10).
           03  QTRB2-WBEG-BRNO             PIC 9(04).
           03  QTRB2-WBEG-CLASS            PIC 9(03).
           03  QTRB2-WBEG-ACCTNO           PIC 9(08).
           03  QTRB2-WBEG-LAWCODE          PIC 9(02).
           03  QTRB2-WBEG-LINE             PIC 9(03).
 
       01  QTRB2-WHERE-END-KEY.
           03  QTRB2-WEND-DATE             PIC X(10).
           03  QTRB2-WEND-BRNO             PIC 9(04).
           03  QTRB2-WEND-CLASS            PIC 9(03).
           03  QTRB2-WEND-ACCTNO           PIC 9(08).
           03  QTRB2-WEND-LAWCODE          PIC 9(02).
           03  QTRB2-WEND-LINE             PIC 9(03).
 
 
       01  QTRB-REC.
           03  QTB-BRNO             PIC 9(4).
           03  QTB-DATE             PIC X(10).
           03  QTB-CLASS            PIC 9(3).
           03  QTB-ACCTNO           PIC 9(8).
           03  QTB-LAWCODE          PIC 99.
           03  QTB-LINE             PIC 9(3).
           03  QTB-LTOUCH-DATE      PIC X(10).
           03  QTB-SSNO-1         PIC 9(9).
           03  QTB-MAKERCD-1      PIC X.
           03  QTB-SSNO-2         PIC 9(9).
           03  QTB-MAKERCD-2      PIC X.
           03  QTB-SSNO-3         PIC 9(9).
           03  QTB-MAKERCD-3      PIC X.
           03  QTB-SSNO-4         PIC 9(9).
           03  QTB-MAKERCD-4      PIC X.
           03  QTB-REF-FRTO       PIC X.
           03  QTB-REFBR          PIC 9999.
           03  QTB-PURCD            PIC XXX.
           03  QTB-SRCD             PIC XX.
           03  QTB-COLLCD           PIC XX.
           03  QTB-SERVACCT         PIC X.
           03  QTB-LOANTYPE         PIC X.
           03  QTB-SUBCLASS         PIC 99.
           03  QTB-LOANDATE         PIC X(10).
           03  QTB-PROCEEDS         PIC S9(7)V99.
           03  QTB-LNAMT            PIC S9(7)V99.
           03  QTB-FINCHG           PIC S9(7)V99.
           03  QTB-INTCHG           PIC 9(7)V99.
           03  QTB-SERCHG           PIC 9(5)V99.
           03  QTB-EXTCHG           PIC 9(5)V99.
           03  QTB-MAINTFEE         PIC 9(5)V99.
           03  QTB-ORGTERM          PIC 9(3).
           03  QTB-INSURED          PIC X.
           03  QTB-INSUNEARN-1      PIC S9(7)V99.
           03  QTB-INSUNEARN-2      PIC S9(7)V99.
           03  QTB-INSUNEARN-3      PIC S9(7)V99.
           03  QTB-INSUNEARN-4      PIC S9(7)V99.
           03  QTB-INSUNEARN-5      PIC S9(7)V99.
           03  QTB-INSUNEARN-6      PIC S9(7)V99.
           03  QTB-INSUNEARN-7      PIC S9(7)V99.
           03  QTB-INSUNEARN-8      PIC S9(7)V99.
           03  QTB-INSUNEARN-9      PIC S9(7)V99.
           03  QTB-INSUNEARN-10     PIC S9(7)V99.
           03  QTB-DLNO             PIC 9(4)V99.
           03  QTB-DLRV             PIC S9(7)V99.
           03  QTB-DLSPRV           PIC S9(7)V99.
           03  QTB-DLDISC           PIC S9(7)V99.
           03  QTB-DLPARV           PIC S9(7)V99.
           03  QTB-DLRECOURSE       PIC X.
           03  QTB-DLPARTRCAMT      PIC 9(7)V99.
           03  QTB-PURBAL           PIC S9(7)V99.
           03  QTB-UNEARN-1         PIC S9(7)V99.
           03  QTB-UNEARN-2         PIC S9(7)V99.
           03  QTB-UNEARN-3         PIC S9(7)V99.
           03  QTB-UNEARN-4         PIC S9(7)V99.
           03  QTB-UNEARN-5         PIC S9(7)V99.
           03  QTB-UNEARN-6         PIC S9(7)V99.
           03  QTB-UNEARN-7         PIC S9(7)V99.
           03  QTB-UNEARN-8         PIC S9(7)V99.
           03  QTB-UNEARN-9         PIC S9(7)V99.
           03  QTB-UNEARN-10        PIC S9(7)V99.
           03  QTB-REF-ACCTNO       PIC 9(8).
           03  QTB-DLRVPRIN-1       PIC S9(8)V99.
           03  QTB-DLRVPRIN-2       PIC S9(8)V99.
           03  QTB-DLRVPRIN-3       PIC S9(8)V99.
           03  QTB-DLRVCHGS-1       PIC S9(7)V99.
           03  QTB-DLRVCHGS-2       PIC S9(7)V99.
           03  QTB-DLRVCHGS-3       PIC S9(7)V99.
           03  QTB-DLPARVPAID       PIC S9(7)V99.
           03  QTB-DLPARVHELD       PIC S9(7)V99.
           03  QTB-USERID           PIC X(10).
           03  QTB-POSTDATE         PIC X(10).
           03  QTB-POSTTIME         PIC 9(6).
           03  QTB-PLCD             PIC X.
           03  QTB-PLTYPE           PIC X.
           03  QTB-REPOCD           PIC X.
           03  QTB-ACTIONCD2        PIC X(4).

       EXEC SQL END DECLARE SECTION END-EXEC.
