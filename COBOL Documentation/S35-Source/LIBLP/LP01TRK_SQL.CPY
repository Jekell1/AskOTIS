      * COPYMEMBER: LIBLP/LP01TRK_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL - DAILY TRANSACTINO FILE - CONVERSIONS 'K'
      *
      *=================================================================
      * REV:
      * BLM 2021-0503 ***NEW***
      *
      ******************************************************************
      *- - -{GREATER CURSOR FOR START-GREATER & READ-NEXT}
       01  TRK1-CURSOR-STAT             PIC X(2) VALUE SPACES.
           88  TRK1-CURSOR-STAT-GOOD             VALUE "00".
           88  TRK1-CURSOR-STAT-BAD              VALUE "  " THRU "//"
                                                       "01" THRU "~~".
       01  TRK2-CURSOR-STAT             PIC X(2) VALUE SPACES.
           88  TRK2-CURSOR-STAT-GOOD             VALUE "00".
           88  TRK2-CURSOR-STAT-BAD              VALUE "  " THRU "//"
                                                       "01" THRU "~~".
       01  TRK1-CURSOR-SC-STAT          PIC X(2) VALUE SPACES.
           88  TRK1-CURSOR-SC-STAT-GOOD          VALUE "00".
           88  TRK1-CURSOR-SC-STAT-BAD           VALUE "  " THRU "//"
                                                       "01" THRU "~~".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  TRK1-RETURN-CODE                PIC S9(4)   COMP-5.
       01  TRK2-RETURN-CODE                PIC S9(4)   COMP-5.

       01  QTRK1-WHERE-BEG-KEY.
           03  QTRK1-WBEG-BRNO             PIC 9(04).
           03  QTRK1-WBEG-DATE             PIC X(10).
           03  QTRK1-WBEG-CLASS            PIC 9(03).
           03  QTRK1-WBEG-ACCTNO           PIC 9(08).
           03  QTRK1-WBEG-LAWCODE          PIC 9(02).
           03  QTRK1-WBEG-LINE             PIC 9(03).

       01  QTRK1-WHERE-END-KEY.
           03  QTRK1-WEND-BRNO             PIC 9(04).
           03  QTRK1-WEND-DATE             PIC X(10).
           03  QTRK1-WEND-CLASS            PIC 9(03).
           03  QTRK1-WEND-ACCTNO           PIC 9(08).
           03  QTRK1-WEND-LAWCODE          PIC 9(02).
           03  QTRK1-WEND-LINE             PIC 9(03).

       01  QTRK1-WHERE-SCAN-BEG-KEY.
           03  QTRK1-WSBEG-DATE            PIC X(10).
           03  QTRK1-WSBEG-CLASS           PIC 9(03).
           03  QTRK1-WSBEG-ACCTNO          PIC 9(08).
           03  QTRK1-WSBEG-LAWCODE         PIC 9(02).

       01  QTRK2-WHERE-BEG-KEY.
           03  QTRK2-WBEG-DATE             PIC X(10).
           03  QTRK2-WBEG-BRNO             PIC 9(04).
           03  QTRK2-WBEG-CLASS            PIC 9(03).
           03  QTRK2-WBEG-ACCTNO           PIC 9(08).
           03  QTRK2-WBEG-LAWCODE          PIC 9(02).
           03  QTRK2-WBEG-LINE             PIC 9(03).

       01  QTRK2-WHERE-END-KEY.
           03  QTRK2-WEND-DATE             PIC X(10).
           03  QTRK2-WEND-BRNO             PIC 9(04).
           03  QTRK2-WEND-CLASS            PIC 9(03).
           03  QTRK2-WEND-ACCTNO           PIC 9(08).
           03  QTRK2-WEND-LAWCODE          PIC 9(02).
           03  QTRK2-WEND-LINE             PIC 9(03).



       01  QTRK-REC.
           03  QTK-BRNO             PIC 9(4).
           03  QTK-DATE             PIC X(10).
           03  QTK-CLASS            PIC 9(3).
           03  QTK-ACCTNO           PIC 9(8).
           03  QTK-LAWCODE          PIC 99.
           03  QTK-LINE             PIC 9(3).
           03  QTK-LTOUCH-DATE        PIC X(10).
           03  QTK-SSNO-1             PIC 9(9).
           03  QTK-MAKERCD-1          PIC X.
           03  QTK-SSNO-2             PIC 9(9).
           03  QTK-MAKERCD-2          PIC X.
           03  QTK-SSNO-3             PIC 9(9).
           03  QTK-MAKERCD-3          PIC X.
           03  QTK-SSNO-4             PIC 9(9).
           03  QTK-MAKERCD-4          PIC X.
           03  QTK-REF-FRTO         PIC X.
           03  QTK-REFBR            PIC 9999.
           03  QTK-PURCD                PIC XXX.
           03  QTK-SRCD                 PIC XX.
           03  QTK-COLLCD               PIC XX.
           03  QTK-SERVACCT             PIC X.
           03  QTK-LOANTYPE             PIC X.
           03  QTK-SUBCLASS             PIC 99.
           03  QTK-REPOCD               PIC X.
           03  QTK-PLCD                 PIC X.
           03  QTK-PLTYPE               PIC X.
           03  QTK-LOANDATE             PIC X(10).
           03  QTK-PROCEEDS             PIC S9(7)V99.
           03  QTK-LNAMT                PIC S9(7)V99.
           03  QTK-FINCHG               PIC S9(7)V99.
           03  QTK-INTCHG               PIC 9(7)V99.
           03  QTK-SERCHG               PIC 9(5)V99.
           03  QTK-EXTCHG               PIC 9(5)V99.
           03  QTK-MAINTFEE             PIC 9(5)V99.
           03  QTK-ORGTERM              PIC 9(3).
           03  QTK-INSURED              PIC X.
           03  QTK-INSUNEARN-1          PIC S9(7)V99.
           03  QTK-INSUNEARN-2          PIC S9(7)V99.
           03  QTK-INSUNEARN-3          PIC S9(7)V99.
           03  QTK-INSUNEARN-4          PIC S9(7)V99.
           03  QTK-INSUNEARN-5          PIC S9(7)V99.
           03  QTK-INSUNEARN-6          PIC S9(7)V99.
           03  QTK-INSUNEARN-7          PIC S9(7)V99.
           03  QTK-INSUNEARN-8          PIC S9(7)V99.
           03  QTK-INSUNEARN-9          PIC S9(7)V99.
           03  QTK-INSUNEARN-10         PIC S9(7)V99.
           03  QTK-INSEARNED-1          PIC S9(7)V99.
           03  QTK-INSEARNED-2          PIC S9(7)V99.
           03  QTK-INSEARNED-3          PIC S9(7)V99.
           03  QTK-INSEARNED-4          PIC S9(7)V99.
           03  QTK-INSEARNED-5          PIC S9(7)V99.
           03  QTK-INSEARNED-6          PIC S9(7)V99.
           03  QTK-INSEARNED-7          PIC S9(7)V99.
           03  QTK-INSEARNED-8          PIC S9(7)V99.
           03  QTK-INSEARNED-9          PIC S9(7)V99.
           03  QTK-INSEARNED-10         PIC S9(7)V99.
           03  QTK-DLNO                 PIC 9(4)V99.
           03  QTK-DLRV                 PIC S9(7)V99.
           03  QTK-DLSPRV               PIC S9(7)V99.
           03  QTK-DLDISC               PIC S9(7)V99.
           03  QTK-DLPARV               PIC S9(7)V99.
           03  QTK-DLRECOURSE           PIC X.
           03  QTK-DLPARTRCAMT          PIC 9(6)V99.
           03  QTK-PURBAL               PIC S9(7)V99.
           03  QTK-UNEARN-1             PIC S9(7)V99.
           03  QTK-UNEARN-2             PIC S9(7)V99.
           03  QTK-UNEARN-3             PIC S9(7)V99.
           03  QTK-UNEARN-4             PIC S9(7)V99.
           03  QTK-UNEARN-5             PIC S9(7)V99.
           03  QTK-UNEARN-6             PIC S9(7)V99.
           03  QTK-UNEARN-7             PIC S9(7)V99.
           03  QTK-UNEARN-8             PIC S9(7)V99.
           03  QTK-UNEARN-9             PIC S9(7)V99.
           03  QTK-UNEARN-10            PIC S9(7)V99.
           03  QTK-PRLNNO               PIC 9(8).
           03  QTK-DLRVPRIN-1           PIC S9(7)V99.
           03  QTK-DLRVPRIN-2           PIC S9(7)V99.
           03  QTK-DLRVPRIN-3           PIC S9(7)V99.
           03  QTK-DLRVCHGS-1           PIC S9(7)V99.
           03  QTK-DLRVCHGS-2           PIC S9(7)V99.
           03  QTK-DLRVCHGS-3           PIC S9(7)V99.
           03  QTK-DLPARVPAID           PIC S9(7)V99.
           03  QTK-DLPARVHELD           PIC S9(7)V99.
           03  QTK-USERID               PIC X(10).
           03  QTK-POSTDATE             PIC X(10).
           03  QTK-POSTTIME             PIC 9(6).
           03  QTK-EARNED-1             PIC S9(7)V99.
           03  QTK-EARNED-2             PIC S9(7)V99.
           03  QTK-EARNED-3             PIC S9(7)V99.
           03  QTK-EARNED-4             PIC S9(7)V99.
           03  QTK-EARNED-5             PIC S9(7)V99.
           03  QTK-EARNED-6             PIC S9(7)V99.
           03  QTK-EARNED-7             PIC S9(7)V99.
           03  QTK-EARNED-8             PIC S9(7)V99.
           03  QTK-EARNED-9             PIC S9(7)V99.
           03  QTK-EARNED-10            PIC S9(7)V99.

       EXEC SQL END DECLARE SECTION END-EXEC.
