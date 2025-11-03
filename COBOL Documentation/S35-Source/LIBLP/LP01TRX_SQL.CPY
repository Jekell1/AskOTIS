      * COPYMEMBER: LIBLP/LP01TRX_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL - DAILY TRANSACTION FILE, CLASS TRANSFER
      *
      *=================================================================
      * REV:
      * BLM 2021-0503 ***NEW***
      *
      ******************************************************************
      *- - -{GREATER CURSOR FOR START-GREATER & READ-NEXT}
       01  TRX1-CURSOR-STAT             PIC X(2) VALUE SPACES.
           88  TRX1-CURSOR-STAT-GOOD             VALUE "00".
           88  TRX1-CURSOR-STAT-BAD              VALUE "  " THRU "//"
                                                       "01" THRU "~~".
       01  TRX2-CURSOR-STAT             PIC X(2) VALUE SPACES.
           88  TRX2-CURSOR-STAT-GOOD             VALUE "00".
           88  TRX2-CURSOR-STAT-BAD              VALUE "  " THRU "//"
                                                       "01" THRU "~~".
       01  TRX1-CURSOR-SC-STAT          PIC X(2) VALUE SPACES.
            88  TRX1-CURSOR-SC-STAT-GOOD          VALUE "00".
            88  TRX1-CURSOR-SC-STAT-BAD           VALUE "  " THRU "//"
                                                        "01" THRU "~~".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  TRX1-RETURN-CODE                PIC S9(4)   COMP-5.
       01  TRX2-RETURN-CODE                PIC S9(4)   COMP-5.

       01  QTRX1-WHERE-BEG-KEY.
           03  QTRX1-WBEG-BRNO             PIC 9(04).
           03  QTRX1-WBEG-DATE             PIC X(10).
           03  QTRX1-WBEG-CLASS            PIC 9(03).
           03  QTRX1-WBEG-ACCTNO           PIC 9(08).
           03  QTRX1-WBEG-LAWCODE          PIC 9(02).
           03  QTRX1-WBEG-LINE             PIC 9(03).

       01  QTRX1-WHERE-END-KEY.
           03  QTRX1-WEND-BRNO             PIC 9(04).
           03  QTRX1-WEND-DATE             PIC X(10).
           03  QTRX1-WEND-CLASS            PIC 9(03).
           03  QTRX1-WEND-ACCTNO           PIC 9(08).
           03  QTRX1-WEND-LAWCODE          PIC 9(02).
           03  QTRX1-WEND-LINE             PIC 9(03).

       01  QTRX1-WHERE-SCAN-BEG-KEY.
           03  QTRX1-WSBEG-DATE            PIC X(10).
           03  QTRX1-WSBEG-CLASS           PIC 9(03).
           03  QTRX1-WSBEG-ACCTNO          PIC 9(08).
           03  QTRX1-WSBEG-LAWCODE         PIC 9(02).
 
       01  QTRX2-WHERE-BEG-KEY.
           03  QTRX2-WBEG-DATE             PIC X(10).
           03  QTRX2-WBEG-BRNO             PIC 9(04).
           03  QTRX2-WBEG-CLASS            PIC 9(03).
           03  QTRX2-WBEG-ACCTNO           PIC 9(08).
           03  QTRX2-WBEG-LAWCODE          PIC 9(02).
           03  QTRX2-WBEG-LINE             PIC 9(03).

       01  QTRX2-WHERE-END-KEY.
           03  QTRX2-WEND-DATE             PIC X(10).
           03  QTRX2-WEND-BRNO             PIC 9(04).
           03  QTRX2-WEND-CLASS            PIC 9(03).
           03  QTRX2-WEND-ACCTNO           PIC 9(08).
           03  QTRX2-WEND-LAWCODE          PIC 9(02).
           03  QTRX2-WEND-LINE             PIC 9(03).
 
       01  QTRX-REC.
           03  QTX-BRNO             PIC 9(4).
           03  QTX-DATE             PIC X(10).
           03  QTX-CLASS            PIC 9(3).
           03  QTX-ACCTNO           PIC 9(8).
           03  QTX-LAWCODE          PIC 99.
           03  QTX-LINE             PIC 9(3).
           03  QTX-LTOUCH-DATE      PIC X(10).
           03  QTX-SSNO-1         PIC 9(9).
           03  QTX-MAKERCD-1      PIC X.
           03  QTX-SSNO-2         PIC 9(9).
           03  QTX-MAKERCD-2      PIC X.
           03  QTX-SSNO-3         PIC 9(9).
           03  QTX-MAKERCD-3      PIC X.
           03  QTX-SSNO-4         PIC 9(9).
           03  QTX-MAKERCD-4      PIC X.
           03  QTX-PURCD            PIC XXX.
           03  QTX-SRCD             PIC XX.
           03  QTX-COLLCD           PIC XX.
           03  QTX-SERVACCT         PIC X.
           03  QTX-LOANTYPE         PIC X.
           03  QTX-SUBCLASS         PIC 99.
           03  QTX-LOANDATE         PIC X(10).
           03  QTX-PROCEEDS         PIC S9(7)V99.
           03  QTX-LNAMT            PIC S9(7)V99.
           03  QTX-FINCHG           PIC S9(7)V99.
           03  QTX-INTCHG           PIC 9(7)V99.
           03  QTX-SERCHG           PIC 9(5)V99.
           03  QTX-EXTCHG           PIC 9(5)V99.
           03  QTX-MAINTFEE         PIC 9(5)V99.
           03  QTX-ORGTERM          PIC 9(3).
           03  QTX-INSURED          PIC X.
           03  QTX-INSANTICERN-1    PIC S9(7)V99.
           03  QTX-INSANTICERN-2    PIC S9(7)V99.
           03  QTX-INSANTICERN-3    PIC S9(7)V99.
           03  QTX-INSANTICERN-4    PIC S9(7)V99.
           03  QTX-INSANTICERN-5    PIC S9(7)V99.
           03  QTX-INSANTICERN-6    PIC S9(7)V99.
           03  QTX-INSANTICERN-7    PIC S9(7)V99.
           03  QTX-INSANTICERN-8    PIC S9(7)V99.
           03  QTX-INSANTICERN-9    PIC S9(7)V99.
           03  QTX-INSANTICERN-10   PIC S9(7)V99.
           03  QTX-INSEARNINGS-1    PIC S9(7)V99.
           03  QTX-INSEARNINGS-2    PIC S9(7)V99.
           03  QTX-INSEARNINGS-3    PIC S9(7)V99.
           03  QTX-INSEARNINGS-4    PIC S9(7)V99.
           03  QTX-INSEARNINGS-5    PIC S9(7)V99.
           03  QTX-INSEARNINGS-6    PIC S9(7)V99.
           03  QTX-INSEARNINGS-7    PIC S9(7)V99.
           03  QTX-INSEARNINGS-8    PIC S9(7)V99.
           03  QTX-INSEARNINGS-9    PIC S9(7)V99.
           03  QTX-INSEARNINGS-10   PIC S9(7)V99.
           03  QTX-DLNO             PIC 9(4)V99.
           03  QTX-DLRECOURSE       PIC X.
           03  QTX-DLPARTRCAMT      PIC 9(7)V99.
           03  QTX-PURBAL           PIC S9(7)V99.
           03  QTX-DLRVPRIN-1       PIC S9(7)V99.
           03  QTX-DLRVPRIN-2       PIC S9(7)V99.
           03  QTX-DLRVPRIN-3       PIC S9(7)V99.
           03  QTX-DLRVCHGS-1       PIC S9(7)V99.
           03  QTX-DLRVCHGS-2       PIC S9(7)V99.
           03  QTX-DLRVCHGS-3       PIC S9(7)V99.
           03  QTX-DLPARVHELD       PIC S9(7)V99.
           03  QTX-DLPARVPAID       PIC S9(7)V99.
           03  QTX-DLDISC           PIC S9(7)V99.
           03  QTX-ANTICTERM        PIC 9(3).
           03  QTX-ANTICERN-1       PIC S9(7)V99.
           03  QTX-ANTICERN-2       PIC S9(7)V99.
           03  QTX-ANTICERN-3       PIC S9(7)V99.
           03  QTX-ANTICERN-4       PIC S9(7)V99.
           03  QTX-ANTICERN-5       PIC S9(7)V99.
           03  QTX-ANTICERN-6       PIC S9(7)V99.
           03  QTX-ANTICERN-7       PIC S9(7)V99.
           03  QTX-ANTICERN-8       PIC S9(7)V99.
           03  QTX-ANTICERN-9       PIC S9(7)V99.
           03  QTX-ANTICERN-10      PIC S9(7)V99.
           03  QTX-EARNINGS-1       PIC S9(7)V99.
           03  QTX-EARNINGS-2       PIC S9(7)V99.
           03  QTX-EARNINGS-3       PIC S9(7)V99.
           03  QTX-EARNINGS-4       PIC S9(7)V99.
           03  QTX-EARNINGS-5       PIC S9(7)V99.
           03  QTX-EARNINGS-6       PIC S9(7)V99.
           03  QTX-EARNINGS-7       PIC S9(7)V99.
           03  QTX-EARNINGS-8       PIC S9(7)V99.
           03  QTX-EARNINGS-9       PIC S9(7)V99.
           03  QTX-EARNINGS-10      PIC S9(7)V99.
           03  QTX-PRLNNO           PIC 9(8).
           03  QTX-PRBAL            PIC S9(7)V99.
           03  QTX-TFRBAL           PIC S9(7)V99.
           03  QTX-SPRCLASS         PIC 999.
           03  QTX-USERID           PIC X(10).
           03  QTX-POSTDATE         PIC X(10).
           03  QTX-POSTTIME         PIC 9(6).

       EXEC SQL END DECLARE SECTION END-EXEC.
