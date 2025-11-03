      * COPYMEMBER: LIBLP/LP01TRJ_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL - DAILY TRANSACTION FILE - CLASS TRANSFERS
      *                                             WITHIN BRANCH
      *
      *=================================================================
      * REV:
      * BLM 2021-0503 ***NEW***
      *
      ******************************************************************
      *- - -{GREATER CURSOR FOR START-GREATER & READ-NEXT}
       01  TRJ1-CURSOR-STAT             PIC X(2) VALUE SPACES.
           88  TRJ1-CURSOR-STAT-GOOD             VALUE "00".
           88  TRJ1-CURSOR-STAT-BAD              VALUE "  " THRU "//"
                                                       "01" THRU "~~".
       01  TRJ2-CURSOR-STAT             PIC X(2) VALUE SPACES.
           88  TRJ2-CURSOR-STAT-GOOD             VALUE "00".
           88  TRJ2-CURSOR-STAT-BAD              VALUE "  " THRU "//"
                                                       "01" THRU "~~".
       01  TRJ1-CURSOR-SC-STAT          PIC X(2) VALUE SPACES.
           88  TRJ1-CURSOR-SC-STAT-GOOD          VALUE "00".
           88  TRJ1-CURSOR-SC-STAT-BAD           VALUE "  " THRU "//"
                                                       "01" THRU "~~".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  TRJ1-RETURN-CODE                PIC S9(4)   COMP-5.
       01  TRJ2-RETURN-CODE                PIC S9(4)   COMP-5.

       01  QTRJ1-WHERE-BEG-KEY.
           03  QTRJ1-WBEG-BRNO             PIC 9(04).
           03  QTRJ1-WBEG-DATE             PIC X(10).
           03  QTRJ1-WBEG-CLASS            PIC 9(03).
           03  QTRJ1-WBEG-ACCTNO           PIC 9(08).
           03  QTRJ1-WBEG-LAWCODE          PIC 9(02).
           03  QTRJ1-WBEG-LINE             PIC 9(03).

       01  QTRJ1-WHERE-END-KEY.
           03  QTRJ1-WEND-BRNO             PIC 9(04).
           03  QTRJ1-WEND-DATE             PIC X(10).
           03  QTRJ1-WEND-CLASS            PIC 9(03).
           03  QTRJ1-WEND-ACCTNO           PIC 9(08).
           03  QTRJ1-WEND-LAWCODE          PIC 9(02).
           03  QTRJ1-WEND-LINE             PIC 9(03).

       01  QTRJ1-WHERE-SCAN-BEG-KEY.
           03  QTRJ1-WSBEG-DATE            PIC X(10).
           03  QTRJ1-WSBEG-CLASS           PIC 9(03).
           03  QTRJ1-WSBEG-ACCTNO          PIC 9(08).
           03  QTRJ1-WSBEG-LAWCODE         PIC 9(02).

       01  QTRJ2-WHERE-BEG-KEY.
           03  QTRJ2-WBEG-DATE             PIC X(10).
           03  QTRJ2-WBEG-BRNO             PIC 9(04).
           03  QTRJ2-WBEG-CLASS            PIC 9(03).
           03  QTRJ2-WBEG-ACCTNO           PIC 9(08).
           03  QTRJ2-WBEG-LAWCODE          PIC 9(02).
           03  QTRJ2-WBEG-LINE             PIC 9(03).

       01  QTRJ2-WHERE-END-KEY.
           03  QTRJ2-WEND-DATE             PIC X(10).
           03  QTRJ2-WEND-BRNO             PIC 9(04).
           03  QTRJ2-WEND-CLASS            PIC 9(03).
           03  QTRJ2-WEND-ACCTNO           PIC 9(08).
           03  QTRJ2-WEND-LAWCODE          PIC 9(02).
           03  QTRJ2-WEND-LINE             PIC 9(03).

       01  QTRJ-REC.
           03  QTJ-BRNO             PIC 9(4).
           03  QTJ-DATE             PIC X(10).
           03  QTJ-CLASS            PIC 9(3).
           03  QTJ-ACCTNO           PIC 9(8).
           03  QTJ-LAWCODE          PIC 99.
           03  QTJ-LINE             PIC 9(3).
           03  QTJ-LTOUCH-DATE      PIC X(10).
           03  QTJ-SSNO-1         PIC 9(9).
           03  QTJ-MAKERCD-1      PIC X.
           03  QTJ-SSNO-2         PIC 9(9).
           03  QTJ-MAKERCD-2      PIC X.
           03  QTJ-SSNO-3         PIC 9(9).
           03  QTJ-MAKERCD-3      PIC X.
           03  QTJ-SSNO-4         PIC 9(9).
           03  QTJ-MAKERCD-4      PIC X.
           03  QTJ-REF-FRLC     PIC 99.
           03  QTJ-REF-X        PIC X.
           03  QTJ-REF-TOLC     PIC 99.
           03  QTJ-FRLC         PIC 9(3).
           03  QTJ-TOLC         PIC 9(3).
           03  QTJ-PURCD            PIC XXX.
           03  QTJ-SRCD             PIC XX.
           03  QTJ-COLLCD           PIC XX.
           03  QTJ-SERVACCT         PIC X.
           03  QTJ-LOANTYPE         PIC X.
           03  QTJ-SUBCLASS         PIC 99.
           03  QTJ-LOANDATE         PIC X(10).
           03  QTJ-PROCEEDS         PIC S9(7)V99.
           03  QTJ-LNAMT            PIC S9(7)V99.
           03  QTJ-FINCHG           PIC S9(7)V99.
           03  QTJ-INTCHG           PIC 9(7)V99.
           03  QTJ-SERCHG           PIC 9(5)V99.
           03  QTJ-EXTCHG           PIC 9(5)V99.
           03  QTJ-MAINTFEE         PIC 9(5)V99.
           03  QTJ-ORGTERM          PIC 9(3).
           03  QTJ-INSURED          PIC X.
           03  QTJ-INSUNEARN-1      PIC S9(7)V99.
           03  QTJ-INSUNEARN-2      PIC S9(7)V99.
           03  QTJ-INSUNEARN-3      PIC S9(7)V99.
           03  QTJ-INSUNEARN-4      PIC S9(7)V99.
           03  QTJ-INSUNEARN-5      PIC S9(7)V99.
           03  QTJ-INSUNEARN-6      PIC S9(7)V99.
           03  QTJ-INSUNEARN-7      PIC S9(7)V99.
           03  QTJ-INSUNEARN-8      PIC S9(7)V99.
           03  QTJ-INSUNEARN-9      PIC S9(7)V99.
           03  QTJ-INSUNEARN-10     PIC S9(7)V99.
           03  QTJ-DLNO             PIC 9(4)V99.
           03  QTJ-DLRV             PIC S9(7)V99.
           03  QTJ-DLSPRV           PIC S9(7)V99.
           03  QTJ-DLDISC           PIC S9(7)V99.
           03  QTJ-DLPARV           PIC S9(7)V99.
           03  QTJ-DLRECOURSE       PIC X.
           03  QTJ-DLPARTRCAMT      PIC 9(6)V99.
           03  QTJ-PURBAL           PIC S9(7)V99.
           03  QTJ-UNEARN-1         PIC S9(7)V99.
           03  QTJ-UNEARN-2         PIC S9(7)V99.
           03  QTJ-UNEARN-3         PIC S9(7)V99.
           03  QTJ-UNEARN-4         PIC S9(7)V99.
           03  QTJ-UNEARN-5         PIC S9(7)V99.
           03  QTJ-UNEARN-6         PIC S9(7)V99.
           03  QTJ-UNEARN-7         PIC S9(7)V99.
           03  QTJ-UNEARN-8         PIC S9(7)V99.
           03  QTJ-UNEARN-9         PIC S9(7)V99.
           03  QTJ-UNEARN-10        PIC S9(7)V99.
           03  QTJ-PRLNNO           PIC 9(8).
           03  QTJ-DLRVPRIN-1       PIC S9(7)V99.
           03  QTJ-DLRVPRIN-2       PIC S9(7)V99.
           03  QTJ-DLRVPRIN-3       PIC S9(7)V99.
           03  QTJ-DLRVCHGS-1       PIC S9(7)V99.
           03  QTJ-DLRVCHGS-2       PIC S9(7)V99.
           03  QTJ-DLRVCHGS-3       PIC S9(7)V99.
           03  QTJ-DLPARVPAID       PIC S9(7)V99.
           03  QTJ-DLPARVHELD       PIC S9(7)V99.
           03  QTJ-USERID           PIC X(10).
           03  QTJ-POSTDATE         PIC X(10).
           03  QTJ-POSTTIME         PIC 9(6).
           03  QTJ-PLCD             PIC X.
           03  QTJ-PLTYPE           PIC X.
           03  QTJ-REPOCD           PIC X.

       EXEC SQL END DECLARE SECTION END-EXEC.
