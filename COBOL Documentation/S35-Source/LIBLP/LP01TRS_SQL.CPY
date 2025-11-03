      * COPYMEMBER: LIBLP/LP01TRS_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL - DAILY TRANSACTION FILE - SALES FINANCE
      *
      *=================================================================
      * REV:
      *
      ******************************************************************
      *- - -{GREATER CURSOR FOR START-GREATER & READ-NEXT}
       01  TRS1-CURSOR-STAT             PIC X(2) VALUE SPACES.
           88  TRS1-CURSOR-STAT-GOOD             VALUE "00".
           88  TRS1-CURSOR-STAT-BAD              VALUE "  " THRU "//"
                                                       "01" THRU "~~".
       01  TRS2-CURSOR-STAT             PIC X(2) VALUE SPACES.
           88  TRS2-CURSOR-STAT-GOOD             VALUE "00".
           88  TRS2-CURSOR-STAT-BAD              VALUE "  " THRU "//"
                                                       "01" THRU "~~".
       01  TRS1-CURSOR-SC-STAT          PIC X(2) VALUE SPACES.
           88  TRS1-CURSOR-SC-STAT-GOOD          VALUE "00".
           88  TRS1-CURSOR-SC-STAT-BAD           VALUE "  " THRU "//"
                                                       "01" THRU "~~".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  TRS1-CHECK-SW                   PIC X(1).
       01  TRS1-RETURN-CODE                PIC S9(4)   COMP-5.
       01  TRS2-RETURN-CODE                PIC S9(4)   COMP-5.

       01  QTRS1-WHERE-BEG-KEY.
           03  QTRS1-WBEG-BRNO             PIC 9(04).
           03  QTRS1-WBEG-DATE             PIC X(10).
           03  QTRS1-WBEG-CLASS            PIC 9(03).
           03  QTRS1-WBEG-ACCTNO           PIC 9(08).
           03  QTRS1-WBEG-LAWCODE          PIC 9(02).
           03  QTRS1-WBEG-LINE             PIC 9(03).

       01  QTRS1-WHERE-END-KEY.
           03  QTRS1-WEND-BRNO             PIC 9(04).
           03  QTRS1-WEND-DATE             PIC X(10).
           03  QTRS1-WEND-CLASS            PIC 9(03).
           03  QTRS1-WEND-ACCTNO           PIC 9(08).
           03  QTRS1-WEND-LAWCODE          PIC 9(02).
           03  QTRS1-WEND-LINE             PIC 9(03).

       01  QTRS1-WHERE-SCAN-BEG-KEY.
           03  QTRS1-WSBEG-DATE            PIC X(10).
           03  QTRS1-WSBEG-CLASS           PIC 9(03).
           03  QTRS1-WSBEG-ACCTNO          PIC 9(08).
           03  QTRS1-WSBEG-LAWCODE         PIC 9(02).

       01  QTRS2-WHERE-BEG-KEY.
           03  QTRS2-WBEG-DATE             PIC X(10).
           03  QTRS2-WBEG-BRNO             PIC 9(04).
           03  QTRS2-WBEG-CLASS            PIC 9(03).
           03  QTRS2-WBEG-ACCTNO           PIC 9(08).
           03  QTRS2-WBEG-LAWCODE          PIC 9(02).
           03  QTRS2-WBEG-LINE             PIC 9(03).

       01  QTRS2-WHERE-END-KEY.
           03  QTRS2-WEND-DATE             PIC X(10).
           03  QTRS2-WEND-BRNO             PIC 9(04).
           03  QTRS2-WEND-CLASS            PIC 9(03).
           03  QTRS2-WEND-ACCTNO           PIC 9(08).
           03  QTRS2-WEND-LAWCODE          PIC 9(02).
           03  QTRS2-WEND-LINE             PIC 9(03).
 
       01  QTRS-REC.
           03  QTS-BRNO             PIC 9(4).
           03  QTS-DATE             PIC X(10).
           03  QTS-CLASS            PIC 9(3).
           03  QTS-ACCTNO           PIC 9(8).
           03  QTS-LAWCODE          PIC 99.
           03  QTS-LINE             PIC 9(3).
           03  QTS-LTOUCH-DATE      PIC X(10).
           03  QTS-SSNO-1         PIC 9(9).
           03  QTS-MAKERCD-1      PIC X.
           03  QTS-SSNO-2         PIC 9(9).
           03  QTS-MAKERCD-2      PIC X.
           03  QTS-SSNO-3         PIC 9(9).
           03  QTS-MAKERCD-3      PIC X.
           03  QTS-SSNO-4         PIC 9(9).
           03  QTS-MAKERCD-4      PIC X.
           03  QTS-PURCD            PIC XXX.
           03  QTS-SRCD             PIC XX.
           03  QTS-COLLCD           PIC XX.
           03  QTS-SERVACCT         PIC X.
           03  QTS-LOANTYPE         PIC X.
           03  QTS-SUBCLASS         PIC 9(2).
           03  QTS-LOANDATE         PIC X(10).
           03  QTS-INTDATE          PIC X(10).
           03  QTS-PROCEEDS         PIC S9(7)V99.
           03  QTS-LNAMT            PIC S9(7)V99.
           03  QTS-FINCHG           PIC S9(7)V99.
           03  QTS-INTCHG           PIC S9(7)V99.
           03  QTS-SERCHG           PIC S9(5)V99.
           03  QTS-EXTCHG           PIC S9(5)V99.
           03  QTS-MAINTFEE         PIC S9(5)V99.
           03  QTS-ORGTERM          PIC 9(3).
           03  QTS-1STPYAMT         PIC S9(7)V99.
           03  QTS-NOREGPY          PIC 9(3).
           03  QTS-REGPYAMT         PIC S9(7)V99.
           03  QTS-LASTPYAMT        PIC S9(7)V99.
           03  QTS-FEECODE-1      PIC XX.
           03  QTS-FEEAMT-1       PIC S9(5)V99.
           03  QTS-FEEOURS-1      PIC X.
           03  QTS-FEECODE-2      PIC XX.
           03  QTS-FEEAMT-2       PIC S9(5)V99.
           03  QTS-FEEOURS-2      PIC X.
           03  QTS-FEECODE-3      PIC XX.
           03  QTS-FEEAMT-3       PIC S9(5)V99.
           03  QTS-FEEOURS-3      PIC X.
           03  QTS-FEECODE-4      PIC XX.
           03  QTS-FEEAMT-4       PIC S9(5)V99.
           03  QTS-FEEOURS-4      PIC X.
           03  QTS-FEECODE-5      PIC XX.
           03  QTS-FEEAMT-5       PIC S9(5)V99.
           03  QTS-FEEOURS-5      PIC X.
           03  QTS-FEECODE-6      PIC XX.
           03  QTS-FEEAMT-6       PIC S9(5)V99.
           03  QTS-FEEOURS-6      PIC X.
           03  QTS-FEECODE-7      PIC XX.
           03  QTS-FEEAMT-7       PIC S9(5)V99.
           03  QTS-FEEOURS-7      PIC X.
           03  QTS-FEECODE-8      PIC XX.
           03  QTS-FEEAMT-8       PIC S9(5)V99.
           03  QTS-FEEOURS-8      PIC X.
           03  QTS-FEECODE-9      PIC XX.
           03  QTS-FEEAMT-9       PIC S9(5)V99.
           03  QTS-FEEOURS-9      PIC X.
           03  QTS-FEECODE-10     PIC XX.
           03  QTS-FEEAMT-10      PIC S9(5)V99.
           03  QTS-FEEOURS-10     PIC X.
           03  QTS-FEECODE-11      PIC XX.
           03  QTS-FEEAMT-11       PIC S9(5)V99.
           03  QTS-FEEOURS-11      PIC X.
           03  QTS-FEECODE-12      PIC XX.
           03  QTS-FEEAMT-12       PIC S9(5)V99.
           03  QTS-FEEOURS-12      PIC X.
           03  QTS-FEECODE-13      PIC XX.
           03  QTS-FEEAMT-13       PIC S9(5)V99.
           03  QTS-FEEOURS-13      PIC X.
           03  QTS-FEECODE-14      PIC XX.
           03  QTS-FEEAMT-14       PIC S9(5)V99.
           03  QTS-FEEOURS-14      PIC X.
           03  QTS-FEECODE-15      PIC XX.
           03  QTS-FEEAMT-15       PIC S9(5)V99.
           03  QTS-FEEOURS-15      PIC X.
           03  QTS-FEECODE-16      PIC XX.
           03  QTS-FEEAMT-16       PIC S9(5)V99.
           03  QTS-FEEOURS-16      PIC X.
           03  QTS-FEECODE-17      PIC XX.
           03  QTS-FEEAMT-17       PIC S9(5)V99.
           03  QTS-FEEOURS-17      PIC X.
           03  QTS-FEECODE-18      PIC XX.
           03  QTS-FEEAMT-18       PIC S9(5)V99.
           03  QTS-FEEOURS-18      PIC X.
           03  QTS-FEECODE-19      PIC XX.
           03  QTS-FEEAMT-19       PIC S9(5)V99.
           03  QTS-FEEOURS-19      PIC X.
           03  QTS-FEECODE-20      PIC XX.
           03  QTS-FEEAMT-20       PIC S9(5)V99.
           03  QTS-FEEOURS-20      PIC X.
           03  QTS-INSURED      PIC X.
           03  QTS-INSTYPES     PIC X.
           03  QTS-INSELIGIBLE  PIC X.
           03  QTS-INSCOMP-1      PIC 99.
           03  QTS-INSOURS-1      PIC X.
           03  QTS-INSEFF-DATE-1  PIC X(10).
           03  QTS-INSEXP-DATE-1  PIC X(10).
           03  QTS-INSPREM-1      PIC S9(5)V99.
           03  QTS-INSCOMM-1      PIC S9(5)V99.
           03  QTS-INSANTICERN-1  PIC S9(7)V99.
           03  QTS-INSEARNINGS-1  PIC S9(7)V99.
           03  QTS-INSCOMP-2      PIC 99.
           03  QTS-INSOURS-2      PIC X.
           03  QTS-INSEFF-DATE-2  PIC X(10).
           03  QTS-INSEXP-DATE-2  PIC X(10).
           03  QTS-INSPREM-2      PIC S9(5)V99.
           03  QTS-INSCOMM-2      PIC S9(5)V99.
           03  QTS-INSANTICERN-2  PIC S9(7)V99.
           03  QTS-INSEARNINGS-2  PIC S9(7)V99.
           03  QTS-INSCOMP-3      PIC 99.
           03  QTS-INSOURS-3      PIC X.
           03  QTS-INSEFF-DATE-3  PIC X(10).
           03  QTS-INSEXP-DATE-3  PIC X(10).
           03  QTS-INSPREM-3      PIC S9(5)V99.
           03  QTS-INSCOMM-3      PIC S9(5)V99.
           03  QTS-INSANTICERN-3  PIC S9(7)V99.
           03  QTS-INSEARNINGS-3  PIC S9(7)V99.
           03  QTS-INSCOMP-4      PIC 99.
           03  QTS-INSOURS-4      PIC X.
           03  QTS-INSEFF-DATE-4  PIC X(10).
           03  QTS-INSEXP-DATE-4  PIC X(10).
           03  QTS-INSPREM-4      PIC S9(5)V99.
           03  QTS-INSCOMM-4      PIC S9(5)V99.
           03  QTS-INSANTICERN-4  PIC S9(7)V99.
           03  QTS-INSEARNINGS-4  PIC S9(7)V99.
           03  QTS-INSCOMP-5      PIC 99.
           03  QTS-INSOURS-5      PIC X.
           03  QTS-INSEFF-DATE-5  PIC X(10).
           03  QTS-INSEXP-DATE-5  PIC X(10).
           03  QTS-INSPREM-5      PIC S9(5)V99.
           03  QTS-INSCOMM-5      PIC S9(5)V99.
           03  QTS-INSANTICERN-5  PIC S9(7)V99.
           03  QTS-INSEARNINGS-5  PIC S9(7)V99.
           03  QTS-INSCOMP-6      PIC 99.
           03  QTS-INSOURS-6      PIC X.
           03  QTS-INSEFF-DATE-6  PIC X(10).
           03  QTS-INSEXP-DATE-6  PIC X(10).
           03  QTS-INSPREM-6      PIC S9(5)V99.
           03  QTS-INSCOMM-6      PIC S9(5)V99.
           03  QTS-INSANTICERN-6  PIC S9(7)V99.
           03  QTS-INSEARNINGS-6  PIC S9(7)V99.
           03  QTS-INSCOMP-7      PIC 99.
           03  QTS-INSOURS-7      PIC X.
           03  QTS-INSEFF-DATE-7  PIC X(10).
           03  QTS-INSEXP-DATE-7  PIC X(10).
           03  QTS-INSPREM-7      PIC S9(5)V99.
           03  QTS-INSCOMM-7      PIC S9(5)V99.
           03  QTS-INSANTICERN-7  PIC S9(7)V99.
           03  QTS-INSEARNINGS-7  PIC S9(7)V99.
           03  QTS-INSCOMP-8      PIC 99.
           03  QTS-INSOURS-8      PIC X.
           03  QTS-INSEFF-DATE-8  PIC X(10).
           03  QTS-INSEXP-DATE-8  PIC X(10).
           03  QTS-INSPREM-8      PIC S9(5)V99.
           03  QTS-INSCOMM-8      PIC S9(5)V99.
           03  QTS-INSANTICERN-8  PIC S9(7)V99.
           03  QTS-INSEARNINGS-8  PIC S9(7)V99.
           03  QTS-INSCOMP-9      PIC 99.
           03  QTS-INSOURS-9      PIC X.
           03  QTS-INSEFF-DATE-9  PIC X(10).
           03  QTS-INSEXP-DATE-9  PIC X(10).
           03  QTS-INSPREM-9      PIC S9(5)V99.
           03  QTS-INSCOMM-9      PIC S9(5)V99.
           03  QTS-INSANTICERN-9  PIC S9(7)V99.
           03  QTS-INSEARNINGS-9  PIC S9(7)V99.
           03  QTS-INSCOMP-10     PIC 99.
           03  QTS-INSOURS-10     PIC X.
           03  QTS-INSEFF-DATE-10 PIC X(10).
           03  QTS-INSEXP-DATE-10 PIC X(10).
           03  QTS-INSPREM-10     PIC S9(5)V99.
           03  QTS-INSCOMM-10     PIC S9(5)V99.
           03  QTS-INSANTICERN-10 PIC S9(7)V99.
           03  QTS-INSEARNINGS-10 PIC S9(7)V99.
           03  QTS-DLCOMM-PP-CMSI     PIC S9(5)V99.
           03  QTS-DLCOMM-O3-CMSI     PIC S9(5)V99.
           03  QTS-INS-TAX-COUNTY-CD  PIC X(3).
           03  QTS-INS-TAX-CITY-CD    PIC X(3).
           03  QTS-TILLNO             PIC 9(2).
           03  QTS-UNWIND-REASON-CODE PIC XX.
           03  QTS-DLNO             PIC 9(4)V99.
           03  QTS-DLRECOURSE       PIC X.
           03  QTS-DLPARTRCAMT      PIC S9(7)V99.
           03  QTS-PURBAL           PIC S9(7)V99.
           03  QTS-DLRVPRIN-1       PIC S9(7)V99.
           03  QTS-DLRVPRIN-2       PIC S9(7)V99.
           03  QTS-DLRVPRIN-3       PIC S9(7)V99.
           03  QTS-DLRVCHGS-1       PIC S9(7)V99.
           03  QTS-DLRVCHGS-2       PIC S9(7)V99.
           03  QTS-DLRVCHGS-3       PIC S9(7)V99.
           03  QTS-DL-RVCHGS-CODE-1 PIC X.
           03  QTS-DL-RVCHGS-CODE-2 PIC X.
           03  QTS-DL-RVCHGS-CODE-3 PIC X.
           03  QTS-DL-PARVHELD-CODE PIC X.
           03  QTS-DL-PARVPAID-CODE PIC X.
           03  QTS-DLPARVHELD       PIC S9(7)V99.
           03  QTS-DLPARVPAID       PIC S9(7)V99.
           03  QTS-DLDISC           PIC S9(7)V99.
           03  QTS-ANTICTERM        PIC 9(3).
           03  QTS-ANTICERN-1       PIC S9(7)V99.
           03  QTS-ANTICERN-2       PIC S9(7)V99.
           03  QTS-ANTICERN-3       PIC S9(7)V99.
           03  QTS-ANTICERN-4       PIC S9(7)V99.
           03  QTS-ANTICERN-5       PIC S9(7)V99.
           03  QTS-ANTICERN-6       PIC S9(7)V99.
           03  QTS-ANTICERN-7       PIC S9(7)V99.
           03  QTS-ANTICERN-8       PIC S9(7)V99.
           03  QTS-ANTICERN-9       PIC S9(7)V99.
           03  QTS-ANTICERN-10      PIC S9(7)V99.
           03  QTS-EARNINGS-1       PIC S9(7)V99.
           03  QTS-EARNINGS-2       PIC S9(7)V99.
           03  QTS-EARNINGS-3       PIC S9(7)V99.
           03  QTS-EARNINGS-4       PIC S9(7)V99.
           03  QTS-EARNINGS-5       PIC S9(7)V99.
           03  QTS-EARNINGS-6       PIC S9(7)V99.
           03  QTS-EARNINGS-7       PIC S9(7)V99.
           03  QTS-EARNINGS-8       PIC S9(7)V99.
           03  QTS-EARNINGS-9       PIC S9(7)V99.
           03  QTS-EARNINGS-10      PIC S9(7)V99.
           03  QTS-PRLNNO           PIC 9(8).
           03  QTS-INSPREM-DLADJ    PIC S9(5)V99.
           03  QTS-PRDLDISC-UNEARN  PIC S9(7)V99.
           03  QTS-SP-LNCOST-FRMLA  PIC X.
           03  QTS-DLPAID           PIC X.
           03  QTS-MIP-FG           PIC X.
           03  QTS-SPRCLASS         PIC 999.
           03  QTS-O3INS-DLPROC     PIC S9(7)V99.
           03  QTS-USERID           PIC X(10).
           03  QTS-POSTDATE         PIC X(10).
           03  QTS-POSTTIME         PIC 9(6).
           03  QTS-TAX-AMT          PIC S9(7)V99.
           03  QTS-APPROVED-BY-USER PIC X(10).
           03  QTS-APPROVED-BY-CODE PIC X(3).
           03  QTS-APPROVED-BY-DATE PIC X(10).
           03  QTS-APPROVED-BY-TIME PIC 9(6).
           03  QTS-ACTIONCD2        PIC X(4).
       
       EXEC SQL END DECLARE SECTION END-EXEC.
