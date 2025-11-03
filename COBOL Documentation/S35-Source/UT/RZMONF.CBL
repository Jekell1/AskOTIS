       PROGRAM-ID.      RZMONF.
       DATE-WRITTEN.    011890.
      ******************************************************************
      *    DIR :  LP
      *    DESC:  MONTHLY FEE-CODE INSURANCE REPORT
      *           FEE CODE INPUTTED ON SCREEN
      *
      *
      * DRIVEN OFF TRFILE
      *    SITUATIONS REQUIRING REPORTING:
      *
      *    NEW PREMIUMS:
      *        1. LOAN RECORD WITH; LN-ENTDATE         = MMYY
      *                             LN-CONVERCD        = " "
      *                             LN-POCD        NOT = "**"
      *    REBATES AND CANCELLATIONS:
      *        CANCEL = DELETED LOAN NEXT MONTH
      *
      * REV:
      *  KEC 091695 ADDED LOCAL-SPOOLING LOGIC.
      *  KEC 091695 CHANGED MILITARY TIME TO AM/PM ON HEADER.
      *  JFF 100695 ADDED CLASS SCAN
      *  KEC 111695 ADDED LN-FEEOURS TO THE DETAIL-LINE.
      *             SEPERATE TOTALS FOR LN-FEEOURS.
      *  KEC 113095 FIXED LOCAL-SPOOL FOR IF A BRANCH HAS NO DATA.
      *  KEC 113095 FIXED LOCAL-SPOOL TO NOT PRINT THE RANGE-LINES.
      *  KEC 120195 COMMENTED OUT OPENS TO BW-FILE IN THE
      *             INITIALIZATION SECTION.
      *  KEC 121195 FIXED DETAIL-LINE TO PRINT "Y" IF FEEOURS IS A
      *             SPACE AND LN-SALEFIN NOT = "Y".
      *  KEC 121295 CHANGE THE TOTALS TO PRINT ALL THE YES'S AND NO'S.
      *  JFF 012496 ADDED LOGIC TO PRINT YES'S NAD NO'S IF IN
      *             THE SAME ACCOUNT.
      *  JTG 060496 CORRECTED BUG WITH DELETED ACCOUNTS BEING SKIPPED ALTOGETHER
      *  MJD 082896 ADDED LOGIC TO HANDLE EVERY VALID COMBINATION OF
      *             POCD, ENTDATE, AND POFFDATE: (SEE DESC IN CODE)
      *  MJD 121096 CHANGED TO PERFORM LPSTAT-DISPLAY AFTER
      *             READING BWFILE.
      *  BAH 121396 SKIP RESCISSION LOANS
      *  BAH 022597 IF JUST DELETED THIS MONTH, PREMIUM WAS GETTING ADDED
      *             INTO TOTALS, SHOULDNT BE !!
      *  BLV 060597 CHANGED FOR LOCAL SPOOL WITH NEW BATCH DRIVER - BREXEC
      *  BAH 100697 FEECODE DESCRIPTION WAS NOT PRINTING IN THE HEADING
      *             WHEN RAN FROM BATCH, PL# 147..ADDED FIND-FEE-DESC
      *             IN INITIALIZATION
      *  BAH 991103 WK-QTY WAS NOT SIGNED, IF THE FIRST ACCOUNT WAS A VOID,
      *             THE WK-QTY WAS 1 INSTEAD OF -1, CAUSING BAD TOTALS
      *             KEVIN, REGACC
      *  BAH 020724 BRANCH TRANSFERS WERE GETTING REPORTED IN BOTH BRANCHES
      *             BECAUSE THIS IS DRIVEN BY THE LOAN FILE !! CHANGED TO
      *             TEST LN-BR-TFR-FG AND ORGBR NOT = OWNBR, WORLD !
      *             SHOULD ONLY REPORT NEW LOANS FOR THE ORIGINATING BRANCH
      *  BAH 030108 PRINT NEW FEE CODE PAYABLE (COMMISSION) AMOUNT , WORLD
      *  BAH 040517 PUT STATE TOTALS BACK IN, THEY WERE COMMENTED OUT, WORLD
      *  BLF 041020 CHANGE TO DRIVE OFF OF TRFILE INSTEAD OF LNFILE.
      *             ADDED SORT ROUTINE, WILL PRINT BY CLASS BY ACCOUNT.
      *  BLF 041124 FIX TO TEST FOR FEE CODE IN SORT ROUTINE
      *  BLM 090622 ADD SOURCE CODE PARAMETERS, WORLD PR# 641
      *  BLM 101001 FIXED BUG FROM 090622.  WHEN I ADDED SOURCE CODE
      *             PARAMETER, SKIPPED CHECK ON SOURCE CODE IF SRCD =
      *             SPACES.  SOME CUSTOMER'S EOFILES HAD AN OLD VALUE FROM
      *             LOCAL-PRINT-FG, SO SRCD WAS = "N " & NO RECORDS WOULD
      *             PRINT.  CHANGED TO CHECK FOR "N ".
      *  BAH 160118 SPLIT OUT LTFILE, PD0003
      *
      * KEC 2016.0315 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED CD-FEE???? TO CD-GC-????.
      *  CS 160830 CHANGED SAVE-WK1-KEY FROM X(7) TO X(8)
      *            WK2-CLASS FROM 9(2) TO 9(3)
      *            WK-CLASS FROM 9(2) TO 9(3).
      *  BAH 170705 ACTIVATE 8 DIGIT DATES, PD0006
      *  BAH 181211 GLOBAL LNFILE, L2G
      *  BAH 190124 CHANGE CDFILE TO CDBFILE, G2G
      *  BAH 20190923 REMOVED ACCESS-CALL ON TRLFILE, TRSFILE
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * TOTALS FILE
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           SELECT WK1-FILE ASSIGN WK1-PATH
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY WK1-KEY
                  FILE STATUS FILE-STAT.
           SELECT WK2-FILE ASSIGN WK2-PATH
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY WK2-KEY
                  FILE STATUS FILE-STAT.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.

       COPY "LIBGB/GBFDPR.CPY".

      ******************************************************************
      *              SUMMARY TOTALS WORK RECORD
      *-----------------------------------------------------------------
      *    WK-FEEOURS-TOTALS(1) = NO
      *    WK-FEEOURS-TOTALS(2) = YES
      *
      ******************************************************************
       FD  WK1-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK1-REC.
           03  WK1-KEY.
               05  WK-CODE         PIC 9.
               05  WK-ST.
                   07  WK-BRNO     PIC 9(4).
               05  WK-CLASS        PIC 9(3).

           03  WK-QTY              PIC S9(7)    COMP-3 OCCURS 2.
      * (1)-COVERAGE  (2)-NEW  (3)-REFUND (4)-PAYABLE(COMMISSION)
           03  WK-AMT              PIC S9(9)V99 COMP-3 OCCURS 5.
           03  WK-FEEOURS-TOTALS   OCCURS 2.
               05  WK-TOTAL-QTY    PIC S9(7)    COMP-3.
               05  WK-TOTAL-AMT    PIC S9(9)V99 COMP-3 OCCURS 5.
           03  WK-SALEFIN          PIC X(1).

      ******************************************************************
      *
      * SORTED LNFILE BY BRANCH, CLASS, ACCOUNT
      *
      ******************************************************************
       FD  WK2-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK2-REC.
           03  WK2-KEY.
               05  WK2-BRANCH           PIC 9(4).
               05  WK2-CLASS            PIC 9(3).
               05  WK2-ACCTNO           PIC 9(8).
               05  WK2-DATE             PIC 9(8).
               05  WK2-CODE             PIC X.
               05  WK2-LAWCODE          PIC 99.
               05  WK2-LINE             PIC 9(3).

      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E    S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)UT/RZMONF.CBL S35 05/26/23-09:23:19 >".

      ******************************************************************
      *
      *     F I L E   I F N
      *
      ******************************************************************
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01TRL.CPY".
      * COPY "LIBLP/LP01TRL_SQL.CPY".
      * COPYMEMBER: LIBLP/LP01TRL_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL - DAILY TRANSACTION FILE - DIRECT LOANS
      *
      *=================================================================
      * REV:
      * BLM 2021-0503 ***NEW***
      *
      ******************************************************************
      *- - -{GREATER CURSOR FOR START-GREATER & READ-NEXT}
       01  TRL1-CURSOR-STAT             PIC X(2) VALUE SPACES.
           88  TRL1-CURSOR-STAT-GOOD             VALUE "00".
           88  TRL1-CURSOR-STAT-BAD              VALUE "  " THRU "//"
                                                        "01" THRU "~~".
       01  TRL2-CURSOR-STAT             PIC X(2) VALUE SPACES.
           88  TRL2-CURSOR-STAT-GOOD             VALUE "00".
           88  TRL2-CURSOR-STAT-BAD              VALUE "  " THRU "//"
                                                       "01" THRU "~~".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  RET-CODE                        PIC S9(4) COMP-5.

       01  QTRL1-WHERE-BEG-KEY.
           03  QTRL1-WBEG-BRNO             PIC 9(04).
           03  QTRL1-WBEG-DATE             PIC X(10).
           03  QTRL1-WBEG-CLASS            PIC 9(03).
           03  QTRL1-WBEG-ACCTNO           PIC 9(08).
           03  QTRL1-WBEG-LAWCODE          PIC 9(02).
           03  QTRL1-WBEG-LINE             PIC 9(03).

       01  QTRL1-WHERE-END-KEY.
           03  QTRL1-WEND-BRNO             PIC 9(04).
           03  QTRL1-WEND-DATE             PIC X(10).
           03  QTRL1-WEND-CLASS            PIC 9(03).
           03  QTRL1-WEND-ACCTNO           PIC 9(08).
           03  QTRL1-WEND-LAWCODE          PIC 9(02).
           03  QTRL1-WEND-LINE             PIC 9(03).
 
       01  QTRL2-WHERE-BEG-KEY.
           03  QTRL2-WBEG-DATE             PIC X(10).
           03  QTRL2-WBEG-BRNO             PIC 9(04).
           03  QTRL2-WBEG-CLASS            PIC 9(03).
           03  QTRL2-WBEG-ACCTNO           PIC 9(08).
           03  QTRL2-WBEG-LAWCODE          PIC 9(02).
           03  QTRL2-WBEG-LINE             PIC 9(03).

       01  QTRL2-WHERE-END-KEY.
           03  QTRL2-WEND-DATE             PIC X(10).
           03  QTRL2-WEND-BRNO             PIC 9(04).
           03  QTRL2-WEND-CLASS            PIC 9(03).
           03  QTRL2-WEND-ACCTNO           PIC 9(08).
           03  QTRL2-WEND-LAWCODE          PIC 9(02).
           03  QTRL2-WEND-LINE             PIC 9(03).
 
       01  QTRL-REC.
           03  QTL-BRNO             PIC 9(4).
           03  QTL-DATE             PIC X(10).
           03  QTL-CLASS            PIC 9(3).
           03  QTL-ACCTNO           PIC 9(8).
           03  QTL-LAWCODE          PIC 99.
           03  QTL-LINE             PIC 9(3).
           03  QTL-LTOUCH-DATE      PIC X(10).
           03  QTL-SSNO-1           PIC 9(9).
           03  QTL-MAKERCD-1        PIC X.
           03  QTL-SSNO-2           PIC 9(9).
           03  QTL-MAKERCD-2        PIC X.
           03  QTL-SSNO-3           PIC 9(9).
           03  QTL-MAKERCD-3        PIC X.
           03  QTL-SSNO-4           PIC 9(9).
           03  QTL-MAKERCD-4        PIC X.
           03  QTL-PURCD            PIC XXX.
           03  QTL-SRCD             PIC XX.
           03  QTL-COLLCD           PIC XX.
           03  QTL-SERVACCT         PIC X.
           03  QTL-LOANTYPE         PIC X.
           03  QTL-SUBCLASS         PIC 99.
           03  QTL-LOANDATE         PIC X(10).
           03  QTL-INTDATE          PIC X(10).
           03  QTL-PROCEEDS         PIC S9(7)V99.
           03  QTL-LNAMT            PIC S9(7)V99.
           03  QTL-FINCHG           PIC S9(7)V99.
           03  QTL-INTCHG           PIC S9(7)V99.
           03  QTL-SERCHG           PIC S9(5)V99.
           03  QTL-EXTCHG           PIC S9(5)V99.
           03  QTL-MAINTFEE         PIC S9(5)V99.
           03  QTL-ORGTERM          PIC 9(3).
           03  QTL-1STPYAMT         PIC S9(7)V99.
           03  QTL-NOREGPY          PIC 9(3).
           03  QTL-REGPYAMT         PIC S9(7)V99.
           03  QTL-LASTPYAMT        PIC S9(7)V99.
           03  QTL-PROCASH          PIC S9(7)V99.
           03  QTL-TILLNO           PIC 99.
           03  QTL-FEECODE-1        PIC XX.
           03  QTL-FEEAMT-1         PIC S9(5)V99.
           03  QTL-FEECODE-2        PIC XX.
           03  QTL-FEEAMT-2         PIC S9(5)V99.
           03  QTL-FEECODE-3        PIC XX.
           03  QTL-FEEAMT-3         PIC S9(5)V99.
           03  QTL-FEECODE-4        PIC XX.
           03  QTL-FEEAMT-4         PIC S9(5)V99.
           03  QTL-FEECODE-5        PIC XX.
           03  QTL-FEEAMT-5         PIC S9(5)V99.
           03  QTL-FEECODE-6        PIC XX.
           03  QTL-FEEAMT-6         PIC S9(5)V99.
           03  QTL-FEECODE-7        PIC XX.
           03  QTL-FEEAMT-7         PIC S9(5)V99.
           03  QTL-FEECODE-8        PIC XX.
           03  QTL-FEEAMT-8         PIC S9(5)V99.
           03  QTL-FEECODE-9        PIC XX.
           03  QTL-FEEAMT-9         PIC S9(5)V99.
           03  QTL-FEECODE-10       PIC XX.
           03  QTL-FEEAMT-10        PIC S9(5)V99.
           03  QTL-FEECODE-11       PIC XX.
           03  QTL-FEEAMT-11        PIC S9(5)V99.
           03  QTL-FEECODE-12       PIC XX.
           03  QTL-FEEAMT-12        PIC S9(5)V99.
           03  QTL-FEECODE-13       PIC XX.
           03  QTL-FEEAMT-13        PIC S9(5)V99.
           03  QTL-FEECODE-14       PIC XX.
           03  QTL-FEEAMT-14        PIC S9(5)V99.
           03  QTL-FEECODE-15       PIC XX.
           03  QTL-FEEAMT-15        PIC S9(5)V99.
           03  QTL-FEECODE-16       PIC XX.
           03  QTL-FEEAMT-16        PIC S9(5)V99.
           03  QTL-FEECODE-17       PIC XX.
           03  QTL-FEEAMT-17        PIC S9(5)V99.
           03  QTL-FEECODE-18       PIC XX.
           03  QTL-FEEAMT-18        PIC S9(5)V99.
           03  QTL-FEECODE-19       PIC XX.
           03  QTL-FEEAMT-19        PIC S9(5)V99.
           03  QTL-FEECODE-20       PIC XX.
           03  QTL-FEEAMT-20        PIC S9(5)V99.
           03  QTL-INSURED          PIC X.
           03  QTL-INSTYPES         PIC X.
           03  QTL-INSELIGIBLE  PIC X.
           03  QTL-INSCOMP-1          PIC 99.
           03  QTL-INSOURS-1          PIC X.
           03  QTL-INSEFF-DATE-1      PIC X(10).
           03  QTL-INSEXP-DATE-1      PIC X(10).
           03  QTL-INSPREM-1          PIC S9(5)V99.
           03  QTL-INSCOMM-1          PIC S9(5)V99.
           03  QTL-INSEARNINGS-1      PIC S9(7)V99.
           03  QTL-INSANTICERN-1      PIC S9(7)V99.
           03  QTL-INSCOMP-2          PIC 99.
           03  QTL-INSOURS-2          PIC X.
           03  QTL-INSEFF-DATE-2      PIC X(10).
           03  QTL-INSEXP-DATE-2      PIC X(10).
           03  QTL-INSPREM-2          PIC S9(5)V99.
           03  QTL-INSCOMM-2          PIC S9(5)V99.
           03  QTL-INSEARNINGS-2      PIC S9(7)V99.
           03  QTL-INSANTICERN-2      PIC S9(7)V99.
           03  QTL-INSCOMP-3          PIC 99.
           03  QTL-INSOURS-3          PIC X.
           03  QTL-INSEFF-DATE-3      PIC X(10).
           03  QTL-INSEXP-DATE-3      PIC X(10).
           03  QTL-INSPREM-3          PIC S9(5)V99.
           03  QTL-INSCOMM-3          PIC S9(5)V99.
           03  QTL-INSEARNINGS-3      PIC S9(7)V99.
           03  QTL-INSANTICERN-3      PIC S9(7)V99.
           03  QTL-INSCOMP-4          PIC 99.
           03  QTL-INSOURS-4          PIC X.
           03  QTL-INSEFF-DATE-4      PIC X(10).
           03  QTL-INSEXP-DATE-4      PIC X(10).
           03  QTL-INSPREM-4          PIC S9(5)V99.
           03  QTL-INSCOMM-4          PIC S9(5)V99.
           03  QTL-INSEARNINGS-4      PIC S9(7)V99.
           03  QTL-INSANTICERN-4      PIC S9(7)V99.
           03  QTL-INSCOMP-5          PIC 99.
           03  QTL-INSOURS-5          PIC X.
           03  QTL-INSEFF-DATE-5      PIC X(10).
           03  QTL-INSEXP-DATE-5      PIC X(10).
           03  QTL-INSPREM-5          PIC S9(5)V99.
           03  QTL-INSCOMM-5          PIC S9(5)V99.
           03  QTL-INSEARNINGS-5      PIC S9(7)V99.
           03  QTL-INSANTICERN-5      PIC S9(7)V99.
           03  QTL-INSCOMP-6          PIC 99.
           03  QTL-INSOURS-6          PIC X.
           03  QTL-INSEFF-DATE-6      PIC X(10).
           03  QTL-INSEXP-DATE-6      PIC X(10).
           03  QTL-INSPREM-6          PIC S9(5)V99.
           03  QTL-INSCOMM-6          PIC S9(5)V99.
           03  QTL-INSEARNINGS-6      PIC S9(7)V99.
           03  QTL-INSANTICERN-6      PIC S9(7)V99.
           03  QTL-INSCOMP-7          PIC 99.
           03  QTL-INSOURS-7          PIC X.
           03  QTL-INSEFF-DATE-7      PIC X(10).
           03  QTL-INSEXP-DATE-7      PIC X(10).
           03  QTL-INSPREM-7          PIC S9(5)V99.
           03  QTL-INSCOMM-7          PIC S9(5)V99.
           03  QTL-INSEARNINGS-7      PIC S9(7)V99.
           03  QTL-INSANTICERN-7      PIC S9(7)V99.
           03  QTL-INSCOMP-8          PIC 99.
           03  QTL-INSOURS-8          PIC X.
           03  QTL-INSEFF-DATE-8      PIC X(10).
           03  QTL-INSEXP-DATE-8      PIC X(10).
           03  QTL-INSPREM-8          PIC S9(5)V99.
           03  QTL-INSCOMM-8          PIC S9(5)V99.
           03  QTL-INSEARNINGS-8      PIC S9(7)V99.
           03  QTL-INSANTICERN-8      PIC S9(7)V99.
           03  QTL-INSCOMP-9          PIC 99.
           03  QTL-INSOURS-9          PIC X.
           03  QTL-INSEFF-DATE-9      PIC X(10).
           03  QTL-INSEXP-DATE-9      PIC X(10).
           03  QTL-INSPREM-9          PIC S9(5)V99.
           03  QTL-INSCOMM-9          PIC S9(5)V99.
           03  QTL-INSEARNINGS-9      PIC S9(7)V99.
           03  QTL-INSANTICERN-9      PIC S9(7)V99.
           03  QTL-INSCOMP-10         PIC 99.
           03  QTL-INSOURS-10         PIC X.
           03  QTL-INSEFF-DATE-10     PIC X(10).
           03  QTL-INSEXP-DATE-10     PIC X(10).
           03  QTL-INSPREM-10         PIC S9(5)V99.
           03  QTL-INSCOMM-10         PIC S9(5)V99.
           03  QTL-INSEARNINGS-10     PIC S9(7)V99.
           03  QTL-INSANTICERN-10     PIC S9(7)V99.
           03  QTL-DLNO               PIC 9(4)V99.
           03  QTL-EARNINGS-1         PIC S9(7)V99.
           03  QTL-EARNINGS-2         PIC S9(7)V99.
           03  QTL-EARNINGS-3         PIC S9(7)V99.
           03  QTL-EARNINGS-4         PIC S9(7)V99.
           03  QTL-EARNINGS-5         PIC S9(7)V99.
           03  QTL-EARNINGS-6         PIC S9(7)V99.
           03  QTL-EARNINGS-7         PIC S9(7)V99.
           03  QTL-EARNINGS-8         PIC S9(7)V99.
           03  QTL-EARNINGS-9         PIC S9(7)V99.
           03  QTL-EARNINGS-10        PIC S9(7)V99.
           03  QTL-ANTICERN-1         PIC S9(7)V99.
           03  QTL-ANTICERN-2         PIC S9(7)V99.
           03  QTL-ANTICERN-3         PIC S9(7)V99.
           03  QTL-ANTICERN-4         PIC S9(7)V99.
           03  QTL-ANTICERN-5         PIC S9(7)V99.
           03  QTL-ANTICERN-6         PIC S9(7)V99.
           03  QTL-ANTICERN-7         PIC S9(7)V99.
           03  QTL-ANTICERN-8         PIC S9(7)V99.
           03  QTL-ANTICERN-9         PIC S9(7)V99.
           03  QTL-ANTICERN-10        PIC S9(7)V99.
           03  QTL-PRLNNO             PIC 9(8).
           03  QTL-PRBAL            PIC S9(7)V99.
           03  QTL-PRINTDUE         PIC S9(7)V99.
           03  QTL-PRFINCHGRB       PIC S9(7)V99.
           03  QTL-PRINSRB          PIC S9(7)V99.
           03  QTL-LPEOD5-FG        PIC X.
           03  QTL-STATE-WORKER     PIC S9(7)V99.
           03  QTL-RESTRICTD-ACCTNO PIC 9(8).
           03  QTL-SPRCLASS         PIC 999.
           03  QTL-USERID           PIC X(10).
           03  QTL-POSTDATE         PIC X(10).
           03  QTL-POSTTIME         PIC  9(6).
           03  QTL-GIFTNET          PIC X.
           03  QTL-APR              PIC 9(3)V999.
           03  QTL-SP-LNCOST-FRMLA  PIC X.
           03  QTL-MIP-FG           PIC X.
           03  QTL-INS-TAX-COUNTY-CD PIC X(3).
           03  QTL-INS-TAX-CITY-CD   PIC X(3).
           03  QTL-PRDLDISC-UNEARN   PIC S9(7)V99.
           03  QTL-POINTS            PIC 99V99.
           03  QTL-TAX-AMT           PIC S9(7)V99.
           03  QTL-PBI-PBD-FG        PIC X.
           03  QTL-APPROVED-BY-USER  PIC X(10).
           03  QTL-APPROVED-BY-CODE  PIC X(3).
           03  QTL-APPROVED-BY-DATE  PIC X(10).
           03  QTL-APPROVED-BY-TIME  PIC 9(6).
           03  QTL-DEBIT-CARD-AMT    PIC S9(7)V99.
           03  QTL-ACTIONCD2         PIC X(4).

       EXEC SQL END DECLARE SECTION END-EXEC.



       COPY "LIBLP/LPWSTRL.CPY".
       COPY "LIBLP/LP01TRS.CPY".
       COPY "LIBLP/LP01TRS_SQL.CPY".
       COPY "LIBLP/LPWSTRS.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01LTF.CPY".
       COPY "LIBLP/LP01LTF_SQL.CPY".
       COPY "LIBLP/LPWSLTF.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBLP/LP01CDB.CPY".
       COPY "LIBLP/LP01CDB_SQL.CPY".
       COPY "LIBLP/LPWSCDB.CPY".
      *----------------------------------------------------------------
       01  WK1-IFN              PIC X(3)     VALUE "WK1".
       01  WK1-PATH             PIC X(35).
       01  WK2-PATH             PIC X(35).

      ******************************************************************
      *   LINE WORKERS     *
      ******************************************************************
       01  LN-WORKERS.
           03  LN-FEED          PIC 999      VALUE 0.
           03  LN-COUNT         PIC 999      VALUE 99.
           03  LN-WK            PIC 999      VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

       01  BR-PAGE-NUMBER       PIC 9(4) COMP-3 VALUE 0.

      ******************************************************************
      *   MISC WORKERS     *
      ******************************************************************
       01  SUB                  PIC 9(6)     COMP-3.
       01  SUB-LT               PIC 9(6)     COMP-3.
       01  FEECODE-PAYABLE      PIC S9(5)V99 COMP-3 VALUE 0.
       01  FIRST-TIME           PIC X        VALUE "Y".
       01  RECORDS-FOUND        PIC X        VALUE " ".
           88  NO-RECORDS-FOUND              VALUE "N".
       01  MESS-LIST            PIC X(5)     VALUE "MSEL.".
       01  HOLD-BR              PIC 9(4)     COMP-3 VALUE 0.
       01  HOLD-FEEOURS-N       PIC X(1)     VALUE SPACES.
       01  HOLD-FEEOURS-Y       PIC X(1)     VALUE SPACES.
       01  SAVE-WK1-KEY         PIC X(8).
       01  SAVE-CODE            PIC 9.
       01  SAVE-ST.
           03  SAVE-BR          PIC 9(4).
       01  LAST-ST              PIC XX.
       01  FIRST-TOT            PIC X.
       01  VALID-FEE            PIC X        VALUE " ".
       01  DASH-LINE            PIC X(132)   VALUE ALL "-".
       01  TRL1-OPENED-SW       PIC X        VALUE "N".

       01  VOID-FLAG            PIC X        VALUE "N".
       01  INCLUDE-FLAG         PIC X        VALUE "N".
       01  INS-ACTIVITY         PIC X        VALUE "N".
       01  REC-EXISTS           PIC X        VALUE "N".
       01  TOTAL-FG             PIC X        VALUE "N".

       01  ONE-NF               PIC 9 VALUE 0.
       01  NON-FILING-N         PIC S9(7)V99 COMP-3 VALUE 0.
       01  NON-FILING-Y         PIC S9(7)V99 COMP-3 VALUE 0.
       01  NEW-COVERAGE         PIC S9(7)V99 COMP-3 VALUE 0.
       01  NEW-NF               PIC S9(7)V99 COMP-3 VALUE 0.
       01  HOLD-NF-REFUND       PIC S9(7)V99 COMP-3 VALUE 0.
       01  NF-REFUND            PIC S9(7)V99 COMP-3 VALUE 0.
       01  REFUND-DATE          PIC 9(8)     COMP-3 VALUE 0.
       01  RPT-END-DATE         PIC 9(8)     COMP-3 VALUE 0.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    TOT 1 = NO;  2 = YES
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  LINE-TOTALS.
           03  TOTALS-ALL-YES   OCCURS 2 TIMES.
               05  TOT-QTY      PIC S9(7)    COMP-3 OCCURS 2.
               05  TOT-AMT      PIC S9(9)V99 COMP-3 OCCURS 5.

       01  OLD-ACCTNO           PIC 9(8).
       01  OLD-ACCTNOX REDEFINES OLD-ACCTNO.
           03  OLD-BR           PIC 9(4).
           03  OLD-AC           PIC 9(5).
           03  OLD-CD           PIC 9.
       01  CLASS-FG             PIC X(3)     VALUE "   ".
           88  CLASS-BEG                     VALUE "BEG".
           88  CLASS-END                     VALUE "END".

      ******************************************************************
      *     S C R E E N   F I E L D S   *
      ******************************************************************
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  BEG-BRANCH      PIC 9(4).
           03  END-BRANCH      PIC 9(4).
           03  BEG-CLASS       PIC 99.
           03  END-CLASS       PIC 99.
           03  BEG-DATE        PIC 9(8).
           03  END-DATE        PIC 9(8).
           03  FEE-CODE        PIC XX.
           03  SUMMARY-ONLY    PIC X.
           03  KEY-PATH        PIC X(20).
           03  GROUP-FLAG      PIC X(1).
           03  ENT-GROUP       PIC X(4).
           03  SRCD            PIC X(2).

       01  VERSION-CONTROL     PIC X    VALUE "A".

       01  DESC-LIST           PIC X(5)  VALUE "DESC.".
       01  DESC-BUF            PIC X(20) VALUE SPACES.

       01  SV-BEG-BR           PIC 9(4)  VALUE 9999.
       01  SV-END-BR           PIC 9(4)  VALUE 9999.
       01  SV-GROUP-FLAG       PIC X     VALUE SPACES.
       01  SV-ENT-GROUP        PIC X(4)  VALUE SPACES.

      ******************************************************************
      *     P R I N T   B U F F E R S   *
      ******************************************************************
       01  HEAD1.
           03  H-DATE           PIC X(8)     VALUE " ".
           03  FILLER           PIC XX       VALUE " ".
           03  H-TIME           PIC X(8)     VALUE " ".
           03  FILLER           PIC X(28)    VALUE " ".
           03  H-NAME           PIC X(40)    VALUE " ".
           03  FILLER           PIC X(29)    VALUE " ".
           03  FILLER           PIC X(8)     VALUE "COMPANY ".
           03  FILLER           PIC X(5)     VALUE "PAGE:".
           03  H-PAGE-NO        PIC ZZZ9.

       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10) VALUE " ".
           03  FILLER           PIC X(26)    VALUE " ".
           03  FILLER           PIC X(41) VALUE
           " FEE-CODE INSURANCE PREMIUM/CANCEL REPORT".

       01  HEAD3.
           03  FILLER           PIC X(10) VALUE "FEE CODE: ".
           03  H-CODE           PIC X(2).
           03  FILLER           PIC X     VALUE " ".
           03  H-CODENAME       PIC X(20) VALUE " ".
           03  FILLER           PIC X(36) VALUE " ".

       01  HEAD4                VALUE " ".
           03  H-PAGE-DESC      PIC X(46).
           03  H-STNO.
               05  H-BRNO       PIC Z(4).
           03  FILLER           PIC X.
           03  H-BRNAME         PIC X(20).
           03  FILLER           PIC X(45).
           03  H-PAGE-PROMPT    PIC X(12).
           03  H-BR-PAGE        PIC ZZZZ.

       01  HEAD5.
           03  FILLER           PIC X(105) VALUE SPACES.
           03  FILLER           PIC X(1)   VALUE "O".
           03  FILLER           PIC X(26)  VALUE SPACES.

       01  HEAD6.
           03  FILLER           PIC X(50) VALUE
           "ACCOUNT #        BORROWER             AGE      EFF".
           03  FILLER           PIC X(50) VALUE
           "ECT  TE   MATURD      TOTAL  REG PAY  PAYABLE     ".
           03  FILLER           PIC X(32) VALUE
           " INS U  DATE OF      INS    ORIG".

       01  HEAD7.
           03  FILLER           PIC X(50) VALUE
           "                 NAME                            D".
           03  FILLER           PIC X(50) VALUE
           "ATE  RM     DATE       NOTE   AMOUNT           PRE".
           03  FILLER           PIC X(32) VALUE
           "MIUM R   CANCEL   CANCEL  BRANCH".

       01  HEAD8.
           03  FILLER           PIC X(50) VALUE
           "TOT  **NEW ACCOUNTS**            NEW LOAN        N".
           03  FILLER           PIC X(50) VALUE
           "EW INS            INS        NET INS          PAYA".
           03  FILLER           PIC X(32) VALUE
           "BLE                             ".

       01  HEAD9.
           03  FILLER           PIC X(50) VALUE
           "CLS             TOTAL            COVERAGE       PR".
           03  FILLER           PIC X(50) VALUE
           "EMIUMS        REFUNDS         AMOUNT              ".
           03  FILLER           PIC X(32) VALUE
           "                                ".

      ******************************************************************
      *    D E T A I L - L I N E
      ******************************************************************
       01  DETAIL-LINE          PIC X(132) VALUE SPACES.

       01  DETAIL1 REDEFINES DETAIL-LINE.
           03  D-LOAN-NUM       PIC X(16).
           03  D-NUM REDEFINES D-LOAN-NUM.
               05  D-CLASS          PIC 99.
               05  FILLER           PIC X.
               05  D-NUMBER         PIC Z(6).
               05  D-H2             PIC X.
               05  D-CKDIGIT        PIC 9.
               05  FILLER           PIC X(5).
           03  FILLER           PIC X.
           03  D-NAME           PIC X(20).
           03  FILLER           PIC XX.
           03  D-AGE            PIC ZZ.
           03  FILLER           PIC X(4).
           03  D-LOANDATE       PIC 99/99/99.
           03  FILLER           PIC X.
           03  D-ORGTERM        PIC ZZ9.
           03  FILLER           PIC X.
           03  D-INSEXPR        PIC 99/99/99.
           03  FILLER           PIC X.
           03  D-COVRG          PIC ZZZ,ZZ9.99-.
           03  D-REGPYAMT       PIC Z(4)9.99.
           03  FILLER           PIC X.
           03  D-FEE-PAYABLE    PIC Z(4).99-   BLANK ZERO.
           03  FILLER           PIC X.
           03  D-INSPREM        PIC Z(4)9.99   BLANK ZERO.
           03  FILLER           PIC X.
           03  D-FEEOURS        PIC X(1).
           03  FILLER           PIC X.
           03  D-REFUND-DATE    PIC 99/99/99   BLANK ZERO.
           03  FILLER           PIC X.
           03  D-REFUND         PIC Z(4)9.99   BLANK ZERO.
           03  FILLER           PIC X(4).
           03  D-ORGBR          PIC 9(4).

       01  DETAIL2 REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(3).
           03  D-QTY            PIC Z(5)9-          OCCURS 3.
           03  FILLER           PIC X(3).
           03  D-AMT            PIC ZZZ,ZZZ,ZZ9.99- OCCURS 7.

       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(20).
           03  RANGE-SEL        PIC X(112).

       01  ERROR-LINE REDEFINES DETAIL-LINE.
           03  D-STAR1          PIC X(20).
           03  FILLER           PIC X(2).
           03  D-ACCT-PROMPT    PIC X(10).
           03  D-ERR-ACCT       PIC 9(6).
           03  FILLER           PIC X(2).
           03  D-ERR-DATE       PIC 99/99/99.
           03  FILLER           PIC X(2).
           03  D-ERR-MESSG      PIC X(52).
           03  D-STAR2          PIC X(20).
      ******************************************************************
      *
      *     C O P Y    L I B R A R Y    W O R K E R S
      *
      ******************************************************************
       COPY "LIBWI/PFSWINDOWSW.CPY".
       COPY "LIBWI/TSTINTW.CPY".
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBWI/LCSCANW.CPY".
       COPY "LIBLP/LPSTATW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBLP/LPAMTSW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBLP/LPBAGEW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/NFMORP_DEF.CPY".
       COPY "LIBLP/NFMORP_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBLP/NFMORP_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE WK1-FILE WK2-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-TRS1-FILE.
           PERFORM CLOSE-TRL1-FILE.
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LTF1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-CDB1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE WK1-FILE WK2-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ******************************************************************
      *
      *    M A I N    M O D U L E
      *
      ******************************************************************
       MAIN-MODULE SECTION.

     D     STOP MESS.

           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "NFMORP" TO VDU-FORM-NAME.

           PERFORM INITIALIZATION.

           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

           IF GROUP-FLAG = "N"
              MOVE BEG-BRANCH TO BR-NO
              PERFORM START-BR-FILE
              GO TO NEXT-BRANCH-NO.

           MOVE ENT-GROUP TO GR-ID.
           PERFORM SEARCH-GR-INIT.

      *-----------------------------------------------------------------
       NEXT-GROUP-LOC.
           IF GROUP-FLAG NOT = "Y"
              GO TO NEXT-BRANCH-NO.

           PERFORM SEARCH-GR-NEXT.

           IF WGR-SRESULT = 0
              GO TO END-ROUTINE.

           MOVE WGR-SRESULT TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           GO TO MOVE-BRANCH-PATH-INFO.

      *-----------------------------------------------------------------
       NEXT-BRANCH-NO.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-FG = 9 OR BR-NO > END-BRANCH
              GO TO END-ROUTINE.

      *-----------------------------------------------------------------
       MOVE-BRANCH-PATH-INFO.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO BW-PATH-OVERRIDE
                                              LN-PATH-OVERRIDE
                                              LTF-PATH-OVERRIDE
                                              TRL-PATH-OVERRIDE
                                              TRS-PATH-OVERRIDE.


           PERFORM OPEN-LN1-FILE.
           PERFORM OPEN-BW1-FILE.
           PERFORM OPEN-LTF1-FILE.

           PERFORM SORT-TRL-FILE.
           PERFORM SORT-TRS-FILE.

           PERFORM DISABLE-INTERRUPT.

           PERFORM CLOSE-WK2-FILE.

           PERFORM OPEN-WK2-FILE.

       NEXT-TR-REPORT.
           PERFORM READ-WK2-FILE-NEXT.
           IF IO-FG = 9
              GO TO END-OF-GROUP.

           PERFORM TRAP-INTERRUPT.

           IF WK2-CODE = "L"
              MOVE WK2-DATE    TO TL-DATE
              MOVE WK2-BRANCH  TO TL-BRNO
              MOVE WK2-CLASS   TO TL-CLASS
              MOVE WK2-ACCTNO  TO TL-ACCTNO
              MOVE WK2-LAWCODE TO TL-LAWCODE
              MOVE WK2-LINE    TO TL-LINE
              PERFORM READ-TRL1-FILE
           ELSE
              MOVE WK2-DATE    TO TS-DATE
              MOVE WK2-BRANCH  TO TS-BRNO
              MOVE WK2-CLASS   TO TS-CLASS
              MOVE WK2-ACCTNO  TO TS-ACCTNO
              MOVE WK2-LAWCODE TO TS-LAWCODE
              MOVE WK2-LINE    TO TS-LINE
              PERFORM READ-TRS1-FILE.
           IF IO-FG NOT = 0
              MOVE "TRANSACTION RECORD MISSING SINCE SORT" TO MESS
              PERFORM SEND-MESS
              GO TO IO-ERROR.

           IF WK2-CODE = "L"
              PERFORM CHECK-TRL-FEE-TABLE
              MOVE TL-BRNO   TO LN-OWNBR
              MOVE TL-ACCTNO TO LN-ACCTNO
           ELSE
              PERFORM CHECK-TRS-FEE-TABLE
              MOVE TS-BRNO   TO LN-OWNBR
              MOVE TS-ACCTNO TO LN-ACCTNO.
           PERFORM READ-LN1-FILE.
           IF IO-BAD
              MOVE "********************" TO D-STAR1 D-STAR2
              MOVE "ACCOUNT # " TO D-ACCT-PROMPT
              MOVE LN-ACCTNO    TO D-ERR-ACCT
              MOVE WK2-DATE     TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY  TO D-ERR-DATE
              MOVE "ACCOUNT NOT ON FILE!!!!!!!" TO D-ERR-MESSG
              MOVE 1 TO LN-FEED
              PERFORM WRITE-DETAIL-LINE
              GO TO NEXT-TR-REPORT.

           IF LN-POCD-INRESCISSION OR LN-POCD-RESCIND
              GO TO NEXT-TR-REPORT.

      * TEST IF LOAN BOOKED & VOIDED IN SELECTED DATE RANGE, THEN SKIP:
           IF LN-POCD = "**"
              IF LN-ENTDATE NOT < BEG-DATE
                 IF LN-ENTDATE NOT > END-DATE
                    IF LN-POFFDATE NOT < BEG-DATE
                       IF LN-POFFDATE NOT > END-DATE
                          GO TO NEXT-TR-REPORT.

      *    -------------------------------------------------------------
      *    AT THIS POINT ACCOUNT IS AT 1 OF 2 STATUSES:
      *       1. ENTERED WITHIN RANGE AND NOT DELETED.
      *       2. ENTERED PRIOR TO BEG-DATE BUT DELETED WITHIN RANGE.
      *    -------------------------------------------------------------

           MOVE 0 TO HOLD-NF-REFUND NF-REFUND REFUND-DATE
                     NON-FILING-Y NON-FILING-N.
           MOVE SPACES TO HOLD-FEEOURS-N.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    BECAUSE LN-FEEOURS COULD BE A SPACE OR A "Y"; MOVE "X" FOR
      *    A CHECK TO SEE IF SOMETHING HAS BEEN MOVED INTO IT.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "X" TO HOLD-FEEOURS-Y.
           MOVE 1 TO SUB.

      *-----------------------------------------------------------------
       CHECK-NEXT-FEE.
           IF ( LN-FEECODE(SUB) = FEE-CODE )
              PERFORM SET-FEEOURS
              IF ( LN-FEEOURS(SUB) = "N" )
                 ADD LN-FEEAMT(SUB) TO NON-FILING-N HOLD-NF-REFUND
              ELSE
                 ADD LN-FEEAMT(SUB) TO NON-FILING-Y HOLD-NF-REFUND.

           IF SUB < 10
              ADD 1 TO SUB
              GO TO CHECK-NEXT-FEE.

           IF HOLD-FEEOURS-N = " " AND HOLD-FEEOURS-Y = "X"
              GO TO NEXT-TR-REPORT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * IF ACCOUNT WAS ENTERED IN PRIOR MONTH AND DELETED THIS MONTH,
      * SETUP REBATE, OTHERWISE SKIP IT:
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "N" TO INS-ACTIVITY.

           IF LN-POCD = "**"
              MOVE HOLD-NF-REFUND TO NF-REFUND
              MOVE LN-POFFDATE    TO REFUND-DATE
              MOVE "Y" TO INS-ACTIVITY.

           IF FIRST-TIME = "Y"
              MOVE "N" TO FIRST-TIME
              MOVE LN-OWNBR TO HOLD-BR
              PERFORM BR-HEADER.

           MOVE LN-ORGST    TO SP-ORGST.
           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           MOVE LN-LAWCODE  TO SP-LAWCODE.

           IF SP1-KEY NOT = LPWSSP-SP1-KEY
              MOVE SP1-KEY TO LPWSSP-SP1-KEY
              PERFORM READ-SP1-FILE
              IF IO-FG = 9
                 GO TO IO-ERROR.

           PERFORM GET-LT-FEECODE.

           PERFORM KEY-DETAIL.

           GO TO NEXT-TR-REPORT.

      *-----------------------------------------------------------------
       END-OF-GROUP.
           PERFORM CLOSE-TRL1-FILE.
           PERFORM CLOSE-TRS1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-LTF1-FILE.
           PERFORM CLOSE-WK2-FILE.
           PERFORM OPEN-WK2-FILE-OUTPUT.

           IF ( FIRST-TIME NOT = "Y" )
              PERFORM BR-TOTALS
              MOVE "Y" TO FIRST-TIME
              MOVE 0 TO PAGE-NUMBER
           ELSE
              MOVE 0 TO PAGE-NUMBER.

           GO TO NEXT-GROUP-LOC.

       END-ROUTINE.
           IF NO-RECORDS-FOUND
              GO TO END-PROGRAM.

           PERFORM GB-TOTALS.
           PERFORM RANGE-LINES.

           GO TO END-PROGRAM.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************
           MOVE NFMORP-QUEUE-POS TO Q-POS.
           MOVE NFMORP-COPIES-POS TO C-POS.
           MOVE NFMORP-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           MOVE "N"      TO RECORDS-FOUND.
           MOVE END-DATE TO RPT-END-DATE.

      * NEEDS TO BE HERE FOR RUNNING THRU BATCH
           PERFORM FIND-FEE-DESC.

           IF EXT-ROUTE-BUF = "00"
              MOVE "REPORT IN PROGRESS................" TO MESS
              PERFORM SEND-LEGEND.

           PERFORM ENABLE-INTERRUPT.

      * CREATE TOTALS WORK FILE (WK1) & SORT WORK FILE (WK2)

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           MOVE MKTEMP-BUF TO WK1-PATH.
           PERFORM OPEN-WK1-FILE-OUTPUT.
           PERFORM CLOSE-WK1-FILE.
           PERFORM OPEN-WK1-FILE.

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           MOVE MKTEMP-BUF TO WK2-PATH.
           PERFORM OPEN-WK2-FILE-OUTPUT.

           IF EXT-EOBUF-GROUP-FG = "Y"
              MOVE BEG-BRANCH TO SV-BEG-BR
              MOVE END-BRANCH TO SV-END-BR
              MOVE GROUP-FLAG TO SV-GROUP-FLAG
              MOVE ENT-GROUP TO SV-ENT-GROUP
              MOVE EXT-EOBUF-BRNO TO BEG-BRANCH END-BRANCH
              MOVE "N" TO GROUP-FLAG
              MOVE EXT-EOBUF-GROUP-PRPATH TO PRU-FILE
           ELSE
              IF ( BEG-BRANCH = 0 AND END-BRANCH = 0 )
                 MOVE BEG-BRANCH TO SV-BEG-BR
                 MOVE END-BRANCH TO SV-END-BR
                 MOVE EXT-CONAME-USER-LOC TO BEG-BRANCH END-BRANCH.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-SP1-FILE.
           PERFORM OPEN-PR-FILE.

           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.
           MOVE FEE-CODE TO H-CODE.
           MOVE DESC-BUF TO H-CODENAME.

           PERFORM CLEAR-TOTALS.

           GO TO INITIALIZE-EXIT.

      *----------------------------------------------------------------
       INITIALIZE-ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.

       INITIALIZE-EXIT.
           EXIT.

      ******************************************************************
       EO-EXEC-AUDIT SECTION.
      ******************************************************************

           MOVE EXT-EOBUF-DATE1 TO BEG-DATE.
           MOVE EXT-EOBUF-DATE2 TO END-DATE.

      ******************************************************************
       ENTER-PARAMETERS SECTION.
      ******************************************************************
       ENTER-PARAM.

      * FOR RE-ENTRY ON F4 FROM ACCEPT.
           IF VDUSTATUS = "1U" GO TO SRCD-01.

      *-----------------------------------------------------------------
       BR-01.
           MOVE "R  A000BR1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-BR.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.

           IF VDUSTATUS = "1<"
              MOVE GRSCAN-BUF TO SCAN-BUF
              PERFORM CALL-WINDOW-PROGRAM
              MOVE SCAN-BUF TO GRSCAN-BUF
              IF SCAN-STATUS NOT = "00"
                 GO TO BR-01
              ELSE
                 MOVE "S  A000BR1." TO VDU
                 MOVE GRSCAN-KEY TO VDUBUF GR1-KEY
                 PERFORM SCREENIO
                 MOVE "00" TO VDUSTATUS.

           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BR-01.

           MOVE VDUBUF TO WGR-WORD ENT-GROUP.

           IF WGR-WORD-ZERO
              GO TO BRANCH-IS-NUMERIC.

           MOVE 0 TO WGR-MIN-LEVEL.
           PERFORM CHECK-MEMBER.

           IF WGR-ERR > 0
              MOVE WGR-MSG(WGR-ERR) TO MESS
              PERFORM SEND-MESS
              GO TO BR-01.

           IF WGR-TYPE = "A"
              MOVE "S  A000BR2." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
              MOVE "Y" TO GROUP-FLAG
              MOVE 0 TO BEG-BRANCH
              MOVE 9999 TO END-BRANCH
              GO TO CHECK-BRANCH-SECURITY.

      *-----------------------------------------------------------------
       BRANCH-IS-NUMERIC.
           MOVE "N" TO GROUP-FLAG.
           MOVE WGR-SRESULT TO BEG-BRANCH.
           MOVE "SN N040BR1." TO VDU.
           MOVE BEG-BRANCH TO VDUNUM0.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       BR-02.
           MOVE "ENNN040BR2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-BR.
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BR-02.
           MOVE VDUNUM0 TO END-BRANCH.
           IF BEG-BRANCH > END-BRANCH
              PERFORM INVALID-RANGE
              GO TO BR-01.

           GO TO CHECK-BRANCH-SECURITY.

      *-----------------------------------------------------------------
       ALL-BR.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "N" TO GROUP-FLAG.

      *----------------------------------------------------------------
       CHECK-BRANCH-SECURITY.
           PERFORM BR-RANGE-SECURITY.

           IF BR-SECURITY-OKAY = "N"
              GO TO BR-01.

      *-----------------------------------------------------------------
       LC-01.
           MOVE "ENNN020LC1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-LC.
           IF VDUSTATUS = "1U" GO TO BR-02.
           IF VDUSTATUS = "1<"
              MOVE "BEG" TO CLASS-FG
              PERFORM CLASS-SCAN
              IF IO-FG NOT = 0
              GO TO LC-01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO LC-01.
           MOVE VDUNUM0 TO BEG-CLASS.

      *----------------------------------------------------------------
       LC-02.
           MOVE "ENNN020LC2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-LC.
           IF VDUSTATUS = "1U" GO TO LC-01.
           IF VDUSTATUS = "1<"
              MOVE "END" TO CLASS-FG
              PERFORM CLASS-SCAN
              IF IO-FG NOT = 0
              GO TO LC-02.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO LC-02.
           MOVE VDUNUM0 TO END-CLASS.
           IF BEG-CLASS NOT > END-CLASS
              GO TO DATE-01.
           PERFORM INVALID-RANGE.
           GO TO LC-01.

      *----------------------------------------------------------------
       ALL-LC.
           MOVE "SNNN020LC1." TO VDU.
           MOVE 1 TO BEG-CLASS VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020LC2." TO VDU.
           MOVE 99 TO END-CLASS VDUNUM0.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
       DATE-01.
           IF EXT-ROUTE-BUF NOT = "00"
              GO TO FEE-01.
           MOVE "ERNM060DATE1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-DATE.
           IF VDUSTATUS = "1U" GO TO LC-02.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO DATE-01.

           IF ( VDUNUM0 = 0 )
              GO TO DATE-01
           ELSE
              MOVE VDU-DATE-YYYYMMDD TO BEG-DATE.

      *----------------------------------------------------------------
       DATE-02.
           MOVE "ERNM060DATE2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO DATE-01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO DATE-02.

           IF ( VDUNUM0 = 0 )
              GO TO DATE-02
           ELSE
              MOVE VDU-DATE-YYYYMMDD TO END-DATE.

       CHECK-DATE-RANGE.
           MOVE BEG-DATE TO NUM-DATE.
           MOVE END-DATE TO SYS-DATE.
           IF (NUM-MO NOT = S-MM) OR (NUM-YR NOT = S-YY)
              MOVE "BEGINNING & ENDING DATES MUST BE IN SAME MONTH!"
                                                           TO MESS
                 PERFORM SEND-MESS
                 GO TO DATE-01.

           GO TO FEE-01.

      *----------------------------------------------------------------
       ALL-DATE.
           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE 1 TO DATE-YYYYMMDD-DD.
           MOVE "S  8080DATE1." TO VDU.
           MOVE DATE-YYYYMMDD TO BEG-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

           MOVE DATE-YYYYMMDD TO NDTE-DATE.
           MOVE 31            TO NDTE-DD.
           PERFORM NEW-DATE-CALCULATION.
           MOVE "S  8080DATE2." TO VDU.
           MOVE NDTE-DATE TO END-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
       FEE-01.
           MOVE SPACES TO WR-BUF.
           MOVE DESC-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
           MOVE "E  A030FEE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO DATE-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO FEE-01.
           MOVE VDUBUF TO FEE-CODE.

           PERFORM FIND-FEE-DESC.
           IF VALID-FEE NOT = "Y"
              PERFORM INVALID-FEE
              GO TO FEE-01.

           MOVE DESC-BUF TO WR-BUF.
           MOVE DESC-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
           GO TO SO-01.

       SO-01.
           MOVE "E  A010SO1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO FEE-01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO SO-01.
           MOVE VDUBUF TO SUMMARY-ONLY.
           IF NOT (SUMMARY-ONLY = "Y" OR "N") GO TO SO-01.

       SRCD-01.
           MOVE "E  A010SRCD." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16"
              MOVE "S  A020SRCD." TO VDU
              MOVE "**" TO VDUBUF SRCD
              PERFORM SCREENIO
              GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO SO-01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO SRCD-01.
           MOVE VDUBUF TO SRCD.

       EXIT-PARM.
           EXIT.

      ***************************
       FIND-FEE-DESC SECTION.
      ***************************
           MOVE "N" TO VALID-FEE.

           PERFORM OPEN-CDB1-FILE.

           MOVE "GC"    TO CDB-TYPE
                           QCDB1-WBEG-TYPE
                           QCDB1-WEND-TYPE.
           MOVE ZEROES  TO CDB-BRANCH
                           QCDB1-WBEG-BRANCH.
           MOVE ALL "9" TO QCDB1-WEND-BRANCH.
           MOVE SPACES  TO CDB-CODE
                           QCDB1-WBEG-CODE.
           MOVE ALL "Z" TO QCDB1-WEND-CODE.

           MOVE FEE-CODE TO CDB-CODE.
           MOVE "GC"     TO CDB-TYPE.

      *    PERFORM START-CDB1-FILE.

       FEE-NEXT-01.

      *    PERFORM READ-CDB1-FILE-NEXT.
           PERFORM READ-CDB1-FILE.
           IF IO-FG NOT = 0
              GO TO EXIT-FIND-FEE.

      *    IF CDB-TYPE NOT = "GC"
      *       GO TO EXIT-FIND-FEE.

      *    IF CDB-CODE NOT = FEE-CODE
      *       GO TO FEE-NEXT-01.

           MOVE CDB-DESC TO DESC-BUF.
           MOVE "Y" TO VALID-FEE.

       EXIT-FIND-FEE.
           PERFORM CLOSE-CDB1-FILE.

      ***************************
       TEST-TRL-FEE-TABLE SECTION.
      ***************************
           MOVE "N" TO INCLUDE-FLAG.
           MOVE 1 TO SUB.

       TEST-NEXT-TRL-FEE.
           IF TL-FEECODE(SUB) = FEE-CODE
              MOVE "Y" TO INCLUDE-FLAG
              GO TO EXIT-TEST-TRL-FEE-TABLE.

           IF SUB < 10
              ADD 1 TO SUB
              GO TO TEST-NEXT-TRL-FEE.

       EXIT-TEST-TRL-FEE-TABLE.
           EXIT.

      ***************************
       TEST-TRS-FEE-TABLE SECTION.
      ***************************
           MOVE "N" TO INCLUDE-FLAG.
           MOVE 1 TO SUB.

       TEST-NEXT-TRS-FEE.
           IF TS-FEECODE(SUB) = FEE-CODE
              MOVE "Y" TO INCLUDE-FLAG
              GO TO EXIT-TEST-TRS-FEE-TABLE.

           IF SUB < 10
              ADD 1 TO SUB
              GO TO TEST-NEXT-TRS-FEE.

       EXIT-TEST-TRS-FEE-TABLE.
           EXIT.

      ***************************
       CHECK-TRL-FEE-TABLE SECTION.
      ***************************
           MOVE "N" TO VOID-FLAG.
           MOVE 1 TO SUB.

       CHECK-NEXT-TRL-FEE.
           IF TL-FEECODE(SUB) = FEE-CODE
              IF TL-FEEAMT(SUB) < 0
                 MOVE "Y" TO VOID-FLAG.

           IF SUB < 10
              ADD 1 TO SUB
              GO TO CHECK-NEXT-TRL-FEE.

      ***************************
       CHECK-TRS-FEE-TABLE SECTION.
      ***************************
           MOVE "N" TO VOID-FLAG.
           MOVE 1 TO SUB.

       CHECK-NEXT-TRS-FEE.
           IF TS-FEECODE(SUB) = FEE-CODE
              IF TS-FEEAMT(SUB) < 0
                 MOVE "Y" TO VOID-FLAG.

           IF SUB < 10
              ADD 1 TO SUB
              GO TO CHECK-NEXT-TRS-FEE.

      ******************************************************************
       DISPLAY-PARAMETERS SECTION.
      ******************************************************************
           IF GROUP-FLAG = "Y"
              MOVE "S  A000BR1." TO VDU
              MOVE ENT-GROUP TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A000BR2." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "SNNN040BR1." TO VDU
              MOVE BEG-BRANCH TO VDUNUM0
              PERFORM SCREENIO
              MOVE "SNNN040BR2." TO VDU
              MOVE END-BRANCH TO VDUNUM0
              PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN020LC1." TO VDU.
           MOVE BEG-CLASS TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN020LC2." TO VDU.
           MOVE END-CLASS TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  8080DATE1." TO VDU.
           MOVE BEG-DATE TO VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  8080DATE2." TO VDU.
           MOVE END-DATE TO VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A030FEE." TO VDU.
           MOVE FEE-CODE TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010SO1." TO VDU.
           MOVE SUMMARY-ONLY TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010SRCD." TO VDU.
           MOVE SRCD TO VDUBUF.
           PERFORM SCREENIO.

      **********************************
       KEY-DETAIL SECTION.
      **********************************
           PERFORM LOAN-CALCULATIONS.

           IF SUMMARY-ONLY = "Y"
              GO TO END-KEY-DETAIL.

      * SETUP PRIMARY INSURED INFO:

           IF LN-INSURED = "S"
              MOVE LN-SSNO(2) TO BW-SSNO
           ELSE
              MOVE LN-SSNO(1) TO BW-SSNO.
           PERFORM SETUP-INSURED.

           IF LN-INSURED = "S"
              MOVE LN-SSNO(1) TO BW-SSNO
              PERFORM READ-BW1-FILE.

           PERFORM LPSTAT-DISPLAY.
           MOVE LPS-DISPLAY-LNNO TO D-LOAN-NUM.

           MOVE LN-LOANDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-LOANDATE.

      * SET INSURANCE TERM, EXPIRATION DATE AND COVERAGE:
           IF NON-FILING-Y NOT = 0 OR NON-FILING-N NOT = 0
              MOVE TOTAL-NOTE TO D-COVRG
              PERFORM MATURITY-DATE-CALCULATION
              MOVE MDTE-DATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO D-INSEXPR
              MOVE LN-ORGTERM TO D-ORGTERM.
           MOVE LN-REGPYAMT TO D-REGPYAMT.

           MOVE FEECODE-PAYABLE TO D-FEE-PAYABLE.

           IF ( HOLD-FEEOURS-N = "N" )
              MOVE HOLD-FEEOURS-N TO D-FEEOURS
              MOVE NON-FILING-N   TO D-INSPREM
           ELSE
              MOVE HOLD-FEEOURS-Y TO D-FEEOURS
              MOVE NON-FILING-Y   TO D-INSPREM.

           IF VOID-FLAG = "Y"
              MOVE REFUND-DATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO D-REFUND-DATE
              MOVE NF-REFUND TO D-REFUND.
           MOVE LN-ORGBR TO D-ORGBR.

           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF ( HOLD-FEEOURS-N = "N" )
              IF ( HOLD-FEEOURS-Y NOT = "X" )
                 MOVE HOLD-FEEOURS-Y TO D-FEEOURS
                 MOVE NON-FILING-Y   TO D-INSPREM
                 MOVE 1 TO LN-FEED
                 PERFORM WRITE-DETAIL-LINE.

           IF LN-OLDACCTNO NOT = 0
              MOVE LN-OLDACCTNO TO OLD-ACCTNO
              MOVE OLD-AC TO D-NUMBER
              MOVE "-" TO D-H2
              MOVE OLD-CD TO D-CKDIGIT
              IF LN-INSURED NOT = "J"
                 PERFORM WRITE-DETAIL-LINE
                 GO TO END-KEY-DETAIL.

      * SETUP SECONDARY INSURED IF JOINT COVERAGE:
           IF LN-INSURED = "J"
              IF LN-SSNO(2) NOT = 0
                 MOVE LN-SSNO(2) TO BW-SSNO
                 PERFORM SETUP-INSURED
                 PERFORM WRITE-DETAIL-LINE.

       END-KEY-DETAIL.
           PERFORM CREATE-TOTAL-RECS.

      ******************************************************************
       SORT-TRL-FILE SECTION.
      ******************************************************************

           IF TRL1-OPENED-SW = "N"
               PERFORM OPEN-TRL1-FILE
               MOVE "Y" TO TRL1-OPENED-SW.
      *     PERFORM OPEN-TRL1-FILE.

           MOVE BR-NO               TO TL-BRNO
		                               TRL-PATH-OWNBR
                                       QTRL1-WBEG-BRNO
                                       QTRL1-WEND-BRNO.
           MOVE BEG-DATE            TO TL-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRL1-WBEG-DATE.
           MOVE END-DATE            TO  SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRL1-WEND-DATE.
           MOVE BEG-CLASS           TO TL-CLASS
                                       QTRL1-WBEG-CLASS.
           MOVE END-CLASS           TO QTRL1-WEND-CLASS.
           MOVE 0                   TO TL-ACCTNO TL-LAWCODE TL-LINE
                                       QTRL1-WBEG-ACCTNO
                                       QTRL1-WBEG-LAWCODE
                                       QTRL1-WBEG-LINE.
           MOVE ALL "9"             TO QTRL1-WEND-ACCTNO
                                       QTRL1-WEND-LAWCODE
                                       QTRL1-WEND-LINE.

           PERFORM START-TRL1-FILE.

       SORT-TRL-NEXT.
           PERFORM READ-TRL1-FILE-NEXT.
           IF (IO-FG NOT = 0) OR (TRL-PATH-OWNBR NOT = TL-BRNO)
              GO TO SORT-TRL-EXIT.

           PERFORM NPR-TRAP-INTERRUPT.

           IF TL-DATE > END-DATE
              GO TO SORT-TRL-EXIT.

           IF TL-CLASS < BEG-CLASS OR TL-CLASS > END-CLASS
              GO TO SORT-TRL-NEXT.

      * 100110 - CHANGED TO CHECK FOR SRCD OF "N " - BEFORE PARAMETER
      *          WAS ADDED, EOFILE HAD THIS VALUE FROM OLD LOCAL PRINT
      *          FLAG & NO RECORDS WOULD BE SELECTED.

           IF SRCD NOT = "  "
              IF SRCD NOT = "**"
                 IF SRCD NOT = "N "
                    IF TL-SRCD NOT = SRCD
                       GO TO SORT-TRL-NEXT.

           PERFORM TEST-TRL-FEE-TABLE.

           IF INCLUDE-FLAG = "N"
              GO TO SORT-TRL-NEXT.

           MOVE TL-DATE    TO WK2-DATE.
           MOVE TL-BRNO    TO WK2-BRANCH.
           MOVE "L"        TO WK2-CODE.
           MOVE TL-CLASS   TO WK2-CLASS.
           MOVE TL-ACCTNO  TO WK2-ACCTNO.
           MOVE TL-LAWCODE TO WK2-LAWCODE.
           MOVE TL-LINE    TO WK2-LINE.

           PERFORM WRITE-WK2-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           MOVE "Y"        TO RECORDS-FOUND.

           GO TO SORT-TRL-NEXT.

       SORT-TRL-EXIT.
           EXIT.

      ******************************************************************
       SORT-TRS-FILE SECTION.
      ******************************************************************

           PERFORM OPEN-TRS1-FILE.

           MOVE BR-NO               TO TS-BRNO
                                       QTRS1-WBEG-BRNO
                                       QTRS1-WEND-BRNO.
           MOVE BEG-DATE            TO TS-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRS1-WBEG-DATE.
           MOVE END-DATE            TO  SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRS1-WEND-DATE.
           MOVE BEG-CLASS           TO TS-CLASS
                                       QTRS1-WBEG-CLASS.
           MOVE END-CLASS           TO QTRS1-WEND-CLASS.
           MOVE 0                   TO TS-ACCTNO TS-LAWCODE TS-LINE
                                       QTRS1-WBEG-ACCTNO
                                       QTRS1-WBEG-LAWCODE
                                       QTRS1-WBEG-LINE.
           MOVE ALL "9"             TO QTRS1-WEND-ACCTNO
                                       QTRS1-WEND-LAWCODE
                                       QTRS1-WEND-LINE.

           PERFORM START-TRS1-FILE.

       SORT-TRS-NEXT.
           PERFORM READ-TRS1-FILE-NEXT.
           IF (IO-FG NOT = 0) OR (TRS-PATH-OWNBR NOT = TS-BRNO)
              GO TO SORT-TRS-EXIT.

           PERFORM NPR-TRAP-INTERRUPT.

           IF TS-DATE > END-DATE
              GO TO SORT-TRS-EXIT.

           IF TS-CLASS < BEG-CLASS OR TS-CLASS > END-CLASS
              GO TO SORT-TRS-NEXT.

      * 100110 - CHANGED TO CHECK FOR SRCD OF "N " - BEFORE PARAMETER
      *          WAS ADDED, EOFILE HAD THIS VALUE FROM OLD LOCAL PRINT
      *          FLAG & NO RECORDS WOULD BE SELECTED.

           IF SRCD NOT = "  "
              IF SRCD NOT = "**"
                 IF SRCD NOT = "N "
                    IF TS-SRCD NOT = SRCD
                       GO TO SORT-TRS-NEXT.

           PERFORM TEST-TRS-FEE-TABLE.

           IF INCLUDE-FLAG = "N"
              GO TO SORT-TRS-NEXT.

           MOVE TS-DATE    TO WK2-DATE.
           MOVE TS-BRNO    TO WK2-BRANCH.
           MOVE "S"        TO WK2-CODE.
           MOVE TS-CLASS   TO WK2-CLASS.
           MOVE TS-ACCTNO  TO WK2-ACCTNO.
           MOVE TS-LAWCODE TO WK2-LAWCODE.
           MOVE TS-LINE    TO WK2-LINE.

           PERFORM WRITE-WK2-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           MOVE "Y"        TO RECORDS-FOUND.

           GO TO SORT-TRS-NEXT.

       SORT-TRS-EXIT.
           EXIT.

      *************************
       GET-LT-FEECODE SECTION.
      *************************
           MOVE 0 TO FEECODE-PAYABLE.

           MOVE LN-OWNBR  TO LTF-BRNO.
           MOVE LN-ACCTNO TO LTF-ACCTNO.
           MOVE 1         TO LTF-SEQNO.
           PERFORM READ-LTF1-FILE.
           IF IO-FG NOT = 0
              GO TO EXIT-GET-LT-FEECODE.

      * WILL ONLY HAVE A PAYBLE AMOUNT IF FEEOURS WAS
      * "Y" IN THE LOAN UPDATE PROGRAM
           PERFORM VARYING SUB-LT FROM 1 BY 1 UNTIL
                           SUB-LT > 10
              IF ( LTF-FEECODE(SUB-LT) = FEE-CODE )
                  ADD LTF-FEECODE-PAYABLE(SUB-LT) TO FEECODE-PAYABLE
              END-IF
           END-PERFORM.

       EXIT-GET-LT-FEECODE.
           EXIT.

      ******************************************************************
       CREATE-TOTAL-RECS SECTION.
      ******************************************************************
           MOVE 0 TO ONE-NF NEW-COVERAGE NEW-NF.

      * IF NOT ENTERED WITHIN THE DATE RANGE, DONT INCLUDE AS NEW PREMIUMS
      * OR IN THE TOTALS
           IF VOID-FLAG = "Y"
              MOVE 0 TO FEECODE-PAYABLE
              GO TO SET-WK1-KEYS.

           MOVE TOTAL-NOTE TO NEW-COVERAGE.
           IF ( HOLD-FEEOURS-N = "N" )
              MOVE NON-FILING-N TO NEW-NF
           ELSE
              IF ( HOLD-FEEOURS-Y  NOT = "X" )
                 MOVE NON-FILING-Y TO NEW-NF.
           IF NEW-NF NOT = 0
              MOVE 1 TO ONE-NF.

      *    IF ( HOLD-FEEOURS-N = "N" ) OR ( HOLD-FEEOURS-Y NOT = "X" )
      *       MOVE 1 TO ONE-NF.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    WK-CODE: 1-BR, 2-ST, 3-GB
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *----------------------------------------------------------------
       SET-WK1-KEYS.
           PERFORM INIT-WK1-KEY.
           MOVE 1 TO WK-CODE.
           MOVE LN-OWNBR TO WK-BRNO.
           PERFORM CREATE-WK.

           PERFORM INIT-WK1-KEY.
           MOVE 2 TO WK-CODE.
           MOVE LN-ORGST TO WK-ST.
           PERFORM CREATE-WK.

           PERFORM INIT-WK1-KEY.
           MOVE 3 TO WK-CODE.
           MOVE 0 TO WK-BRNO.
           PERFORM CREATE-WK.

       EXIT-CREATE-TOTAL-RECS.
           EXIT.

      ******************************************************************
       CREATE-WK SECTION.
      ******************************************************************
           MOVE WK1-KEY TO SAVE-WK1-KEY.
           MOVE "N" TO REC-EXISTS.

      *----------------------------------------------------------------
       CHECK-REC-EXISTS.
           PERFORM READ-WK1-FILE.
           IF IO-FG = 0
              GO TO UPDATE-WK1-REC.

           IF REC-EXISTS = "Y"
              GO TO IO-ERROR.

           MOVE " " TO WK1-REC.
           MOVE SAVE-WK1-KEY TO WK1-KEY.

           MOVE 0 TO WK-AMT(1) WK-AMT(2) WK-AMT(3) WK-AMT(4) WK-AMT(5)
                     WK-QTY(1) WK-QTY(2)
                     WK-TOTAL-QTY(1)   WK-TOTAL-QTY(2)
                     WK-TOTAL-AMT(1,1) WK-TOTAL-AMT(1,2)
                     WK-TOTAL-AMT(1,3) WK-TOTAL-AMT(1,4)
                     WK-TOTAL-AMT(1,5) WK-TOTAL-AMT(2,1)
                     WK-TOTAL-AMT(2,1) WK-TOTAL-AMT(2,2)
                     WK-TOTAL-AMT(2,3) WK-TOTAL-AMT(2,4)
                     WK-TOTAL-AMT(2,5).

           PERFORM WRITE-WK1-FILE.

           IF IO-FG = 9
              GO TO IO-ERROR.

           MOVE "Y" TO REC-EXISTS.
           GO TO CHECK-REC-EXISTS.

      *----------------------------------------------------------------
       UPDATE-WK1-REC.
           MOVE LN-SALEFIN   TO WK-SALEFIN.
           ADD  ONE-NF       TO WK-QTY(1).
           IF VOID-FLAG = "N"
              ADD  NEW-COVERAGE TO WK-AMT(1)
              ADD  NEW-NF       TO WK-AMT(2)
           ELSE
              ADD  NF-REFUND TO WK-AMT(3).
           ADD  FEECODE-PAYABLE TO WK-AMT(4).


           IF ( HOLD-FEEOURS-N = "N" )
              ADD ONE-NF       TO WK-TOTAL-QTY(1)
              IF VOID-FLAG = "N"
                 ADD NEW-COVERAGE TO WK-TOTAL-AMT(1,1)
                 ADD NEW-NF       TO WK-TOTAL-AMT(1,2)
              ELSE
                 ADD NF-REFUND    TO WK-TOTAL-AMT(1,3).

              IF ( HOLD-FEEOURS-Y NOT = "X" )
                 ADD ONE-NF          TO WK-TOTAL-QTY(2)
                 IF VOID-FLAG = "N"
                    ADD NEW-COVERAGE    TO WK-TOTAL-AMT(2,1)
                    ADD NEW-NF          TO WK-TOTAL-AMT(2,2)
                 ELSE
                    ADD NF-REFUND       TO WK-TOTAL-AMT(2,3)
                 END-IF
                 ADD FEECODE-PAYABLE TO WK-TOTAL-AMT(2,4).

           PERFORM REWRITE-WK1-FILE.

      ******************************************************************
       BR-TOTALS SECTION.
      ******************************************************************

           MOVE "B R A N C H   T O T A L S:" TO H-PAGE-DESC.
           MOVE 1 TO SAVE-CODE.
           MOVE HOLD-BR TO SAVE-BR.
           PERFORM PRINT-TOTALS.

           MOVE LN-OWNBR TO HOLD-BR.
           MOVE " " TO H-PAGE-DESC.
           PERFORM BR-HEADER.
           MOVE 0 TO BR-PAGE-NUMBER.

      ******************************************************************
       GB-TOTALS SECTION.
      ******************************************************************

           MOVE "S T A T E   T O T A L S:" TO H-PAGE-DESC
           MOVE 2 TO SAVE-CODE
           MOVE " " TO SAVE-ST
           PERFORM PRINT-TOTALS.

           MOVE "C O M P A N Y   T O T A L S:" TO H-PAGE-DESC.
           MOVE 0 TO H-BRNO.
           MOVE " " TO H-BRNAME.
           MOVE 3 TO SAVE-CODE.
           MOVE 0 TO SAVE-BR.
           PERFORM PRINT-TOTALS.

      ******************************************************************
       PRINT-TOTALS SECTION.
      ******************************************************************
           MOVE 99 TO LN-COUNT.
           MOVE "Y" TO TOTAL-FG.
           MOVE "Y" TO FIRST-TOT.
           MOVE " " TO WK1-KEY.
           MOVE SAVE-CODE TO WK-CODE.
           MOVE SAVE-ST TO WK-ST.
           MOVE 0 TO WK-CLASS.

           PERFORM START-WK1-FILE.

      *----------------------------------------------------------------
       NEXT-TOTAL-LINE.
           PERFORM READ-WK1-FILE-NEXT.
           IF IO-FG = 9
              GO TO EXIT-PRINT-TOTALS.

           IF WK-CODE NOT = SAVE-CODE
              GO TO EXIT-PRINT-TOTALS.

           IF SAVE-CODE NOT = 2
              GO TO SKIP-FIELD-2.

       LOOP-FIELD-2.

           IF FIRST-TOT = "Y"
              MOVE "N" TO FIRST-TOT
              MOVE WK-ST TO LAST-ST H-STNO
              MOVE "- STATE SUMMARY -" TO H-BRNAME.

           IF WK-ST NOT = LAST-ST
              PERFORM TOTAL-LINE
              MOVE "Y" TO FIRST-TOT
              GO TO LOOP-FIELD-2.

           PERFORM MOVE-ACCUM-TOTALS.
           GO TO NEXT-TOTAL-LINE.

       SKIP-FIELD-2.

      * BR/IC LOOP

           IF WK-ST NOT = SAVE-ST
              GO TO EXIT-PRINT-TOTALS.

           PERFORM MOVE-ACCUM-TOTALS.
           GO TO NEXT-TOTAL-LINE.

       EXIT-PRINT-TOTALS.
           PERFORM TOTAL-LINE.
           MOVE "N" TO TOTAL-FG.

      ******************************************************************
       SETUP-INSURED SECTION.
      ******************************************************************
           PERFORM READ-BW1-FILE.
           IF IO-FG = 9
              MOVE "*** NOT FOUND ***" TO BW-FNAME
              MOVE SPACES TO BW-LNAME
              MOVE 0      TO BW-BDATE BW-REFDATE.

      * SET NAME OF PRIMARY INSURED:

           MOVE SPACES TO D-NAME.
           STRING BW-FNAME," ",BW-LNAME DELIMITED BY "  "
               INTO D-NAME.

      * SET AGE OF PRIMARY INSURED:
           MOVE BW-BDATE     TO BAGE-DATE.
           MOVE BW-REFDATE   TO BAGE-REFDATE.
           MOVE RPT-END-DATE TO BAGE-TODAY.
           PERFORM BAGE-CALCULATION.
           MOVE BAGE-AGE TO D-AGE.

       SETUP-INSURED-EXIT.
           EXIT.

      ******************************************************************
       WRITE-DETAIL-LINE SECTION.
      ******************************************************************
           ADD LN-FEED LN-COUNT GIVING LN-WK.

           IF LN-WK >  EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.

           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      ******************************************************************
       HEAD-PAGE SECTION.
      ******************************************************************
           MOVE "N" TO FIRST-TIME.
           ADD 1 TO PAGE-NUMBER.
           MOVE PAGE-NUMBER TO H-PAGE-NO.
           PERFORM GET-TIME.
           MOVE TIME-EDIT TO H-TIME.

           MOVE HEAD1 TO PR-REC.
           MOVE 99 TO LN-FEED.
           PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE HEAD2 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE HEAD3 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF H-PAGE-DESC = " "
              ADD 1 TO BR-PAGE-NUMBER
              MOVE BR-PAGE-NUMBER TO H-BR-PAGE
              MOVE "BRANCH PAGE:" TO H-PAGE-PROMPT
           ELSE
              MOVE 0 TO H-BR-PAGE
              MOVE " " TO H-PAGE-PROMPT.
           MOVE HEAD4 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF TOTAL-FG = "N"
              MOVE HEAD5 TO PR-REC
           ELSE
              MOVE SPACES TO PR-REC.

           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF TOTAL-FG = "N"
              MOVE HEAD6 TO PR-REC
           ELSE
              MOVE HEAD8 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF TOTAL-FG = "N"
              MOVE HEAD7 TO PR-REC
           ELSE
              MOVE HEAD9 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE DASH-LINE TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

       END-HEAD-PAGE.
           MOVE 9 TO LN-COUNT.
           MOVE 2 TO LN-FEED.

      ******************************************************************
       RANGE-LINES SECTION.
      ******************************************************************
           MOVE 3 TO LN-FEED.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF GROUP-FLAG = "Y"
              MOVE "BRANCH GROUP" TO RANGE-LINE
              MOVE ENT-GROUP TO RANGE-SEL
              PERFORM WRITE-DETAIL-LINE
           ELSE
              MOVE "BEG BRANCH" TO RANGE-LINE
              MOVE BEG-BRANCH TO RANGE-SEL
              PERFORM WRITE-DETAIL-LINE
              MOVE "END BRANCH" TO RANGE-LINE
              MOVE END-BRANCH TO RANGE-SEL
              PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEG LOAN CLASS" TO RANGE-LINE.
           MOVE BEG-CLASS TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "END LOAN CLASS" TO RANGE-LINE.
           MOVE END-CLASS TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING DATE" TO RANGE-LINE.
           MOVE BEG-DATE TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "ENDING    DATE" TO RANGE-LINE.
           MOVE END-DATE TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "FEE CODE " TO RANGE-LINE.
           MOVE FEE-CODE TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    MOVE "MONTHLY DETAIL ONLY ?" TO RANGE-LINE.
      *    MOVE DETAIL-MM-ONLY TO RANGE-SEL.
      *    PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SUMMARY ONLY ?" TO RANGE-LINE.
           MOVE SUMMARY-ONLY TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SOURCE CODE " TO RANGE-LINE.
           IF SRCD = "**"
              MOVE "ALL" TO RANGE-SEL
           ELSE
              MOVE SRCD TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.

      ******************************************************************
      *
      *   C O P Y    L I B R A R Y    S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/DATER.CPY".
       COPY "LIBWI/PFSWINDOWS.CPY".
       COPY "LIBLP/LPAMTS.CPY".
       COPY "LIBLP/LPSTAT.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBLP/LPBAGE.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/BRSECURE.CPY".

      ******************************************************************
       MOVE-ACCUM-TOTALS SECTION.
      ******************************************************************

           PERFORM ADD-TO-TOTALS.

           MOVE WK-CLASS     TO D-CLASS.
           MOVE WK-QTY(1) TO D-QTY(3).

      * COVERAGE
           MOVE WK-AMT(1) TO D-AMT(1).
      * NEW PREMIUMS
           MOVE WK-AMT(2) TO D-AMT(2).
      * REFUNDS
           MOVE WK-AMT(3) TO D-AMT(3).
      * NET
           SUBTRACT WK-AMT(3) FROM WK-AMT(2) GIVING D-AMT(4).
      * PAYABLE
           MOVE WK-AMT(4) TO D-AMT(5).

           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 0 TO SUB.

      *----------------------------------------------------------------
       NEXT-TOTAL-QTY-ACCUM.
           ADD 1 TO SUB.

           IF ( SUB = 1 )
              MOVE "OUR FEE = NO" TO D-LOAN-NUM
           ELSE
              MOVE "OUR FEE = YES"  TO D-LOAN-NUM.

           IF ( WK-TOTAL-AMT(SUB,1) = ZEROES ) AND
              ( WK-TOTAL-AMT(SUB,2) = ZEROES ) AND
              ( WK-TOTAL-AMT(SUB,3) = ZEROES )
              MOVE ZEROES TO D-QTY(3)
           ELSE
              MOVE WK-TOTAL-QTY(SUB) TO D-QTY(3).

      * COVERAGE
           MOVE WK-TOTAL-AMT(SUB,1) TO D-AMT(1).
      * PREMIUMS
           MOVE WK-TOTAL-AMT(SUB,2) TO D-AMT(2).
      * REFUNDS
           MOVE WK-TOTAL-AMT(SUB,3) TO D-AMT(3).
      * NET
           SUBTRACT WK-TOTAL-AMT(SUB,3) FROM WK-TOTAL-AMT(SUB,2)
                    GIVING D-AMT(4).
      * PAYABLE
           MOVE WK-TOTAL-AMT(SUB,4) TO D-AMT(5).

           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF ( SUB < 2 )
              GO TO NEXT-TOTAL-QTY-ACCUM.

      ****************************************************************
       CLASS-SCAN SECTION.
      ****************************************************************
           MOVE 0 TO LCSCAN-KEY.
           MOVE " " TO LCSCAN-VERIFY.
           MOVE LCSCAN-BUF TO SCAN-BUF.
           PERFORM CALL-WINDOW-PROGRAM.
           MOVE SCAN-BUF TO LCSCAN-BUF.
           IF SCAN-STATUS NOT = "00"
            OR LCSCAN-KEY = 0
               MOVE 9 TO IO-FG
               GO TO EXIT-CLASS-SCAN.
           IF ( CLASS-FG = "BEG" )
              MOVE "SNNN020LC1." TO VDU
           ELSE
              MOVE "SNNN020LC2." TO VDU.
           MOVE LCSCAN-KEY TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE LCSCAN-KEY TO VDUNUM0.
           MOVE 0 TO IO-FG.

       EXIT-CLASS-SCAN.
           EXIT.

      ******************************************************************
       ADD-TO-TOTALS SECTION.
      ******************************************************************
           ADD WK-TOTAL-QTY(1)   TO TOT-QTY(1,1).
           ADD WK-TOTAL-QTY(2)   TO TOT-QTY(2,1).
           ADD WK-TOTAL-AMT(1,1) TO TOT-AMT(1,1).
           ADD WK-TOTAL-AMT(1,2) TO TOT-AMT(1,2).
           ADD WK-TOTAL-AMT(1,3) TO TOT-AMT(1,3).
           ADD WK-TOTAL-AMT(1,4) TO TOT-AMT(1,4).
           ADD WK-TOTAL-AMT(1,5) TO TOT-AMT(1,5).
           ADD WK-TOTAL-AMT(2,1) TO TOT-AMT(2,1).
           ADD WK-TOTAL-AMT(2,2) TO TOT-AMT(2,2).
           ADD WK-TOTAL-AMT(2,3) TO TOT-AMT(2,3).
           ADD WK-TOTAL-AMT(2,4) TO TOT-AMT(2,4).
           ADD WK-TOTAL-AMT(2,5) TO TOT-AMT(2,5).

      *********************************************************
       FORM-RESET SECTION.
      *********************************************************
           MOVE EXT-CONAME-CONAME TO NFMORP-CONAME.
           MOVE EXT-CONAME-SYSDATE TO NFMORP-SYSDATE.
           DISPLAY NFMORP-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE NFMORP-FORM-FIELDS TO WR-BUF.
           MOVE NFMORP-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       FORM-RESET-EXIT.
           EXIT.

      *********************************************************
       SETUP-LINK-INFO SECTION.
      *********************************************************
           MOVE NFMORP-HELP--FILE TO HELP-LINK-FILE.
           MOVE NFMORP-HELP--DIR  TO HELP-LINK-DIR.

      *********************************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/NFMORP_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISC-PARA SECTION.
      ******************************************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/RMFILE.CPY".
      *-----------------------------------------------------------------
       SET-FEEOURS.
           IF ( LN-FEEOURS(SUB) = "N" )
              MOVE LN-FEEOURS(SUB) TO HOLD-FEEOURS-N
           ELSE
              MOVE LN-FEEOURS(SUB) TO HOLD-FEEOURS-Y
              IF ( LN-SALEFIN NOT = "Y" )
                 IF ( HOLD-FEEOURS-Y = SPACES )
                    MOVE "Y" TO HOLD-FEEOURS-Y.

      *-----------------------------------------------------------------
       INVALID-FEE.
           MOVE "INVALID FEE CODE" TO MESS.
           PERFORM SEND-MESS.

      *----------------------------------------------------------------
       BR-HEADER.
           MOVE HOLD-BR TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-FG = 9
              MOVE "* INVALID *" TO BR-NAME.

           MOVE HOLD-BR TO H-BRNO.
           MOVE BR-NAME TO H-BRNAME.

      *----------------------------------------------------------------
       INIT-WK1-KEY.
           MOVE " " TO WK1-KEY.
           MOVE LN-CLASS TO WK-CLASS.

      *----------------------------------------------------------------
       TOTAL-LINE.
           MOVE "ALL CLASSES" TO DETAIL-LINE.
           COMPUTE D-QTY(3) = TOT-QTY(1,1) + TOT-QTY(2,1).
      * COVERAGE
           COMPUTE D-AMT(1) = TOT-AMT(1,1) + TOT-AMT(2,1).
      * PREMIUMS
           COMPUTE D-AMT(2) = TOT-AMT(1,2) + TOT-AMT(2,2).
      * REFUNDS
           COMPUTE D-AMT(3) = TOT-AMT(1,3) + TOT-AMT(2,3).
      * NET
           COMPUTE D-AMT(4) = ( TOT-AMT(1,2) + TOT-AMT(2,2) ) -
                              ( TOT-AMT(1,3) + TOT-AMT(2,3) ).
      * PAYABLE
           COMPUTE D-AMT(5) = TOT-AMT(1,4) + TOT-AMT(2,4).

           MOVE 3 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "    ALL = NO" TO DETAIL-LINE.
           MOVE TOT-QTY(1,1) TO D-QTY(3).
      * COVERAGE
           MOVE TOT-AMT(1,1) TO D-AMT(1).
      * PREMIUMS
           MOVE TOT-AMT(1,2) TO D-AMT(2).
      * REFUNDS
           MOVE TOT-AMT(1,3) TO D-AMT(3).
      * NET
           COMPUTE D-AMT(4) = TOT-AMT(1,2) - TOT-AMT(1,3).
      * PAYABLE
           MOVE TOT-AMT(1,4) TO D-AMT(5).

           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "    ALL = YES" TO DETAIL-LINE.
           MOVE TOT-QTY(2,1) TO D-QTY(3).
      * COVERAGE
           MOVE TOT-AMT(2,1) TO D-AMT(1).
      * PREMIUMS
           MOVE TOT-AMT(2,2) TO D-AMT(2).
      * REFUNDS
           MOVE TOT-AMT(2,3) TO D-AMT(3).
      * NET
           COMPUTE D-AMT(4) = TOT-AMT(2,2) - TOT-AMT(2,3).
      * PAYABLE
           MOVE TOT-AMT(2,4) TO D-AMT(5).

           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           PERFORM CLEAR-TOTALS.
           MOVE 99 TO LN-COUNT.

      *----------------------------------------------------------------
       CLEAR-TOTALS.
           MOVE 0 TO
                  TOT-AMT(1,1) TOT-AMT(1,2) TOT-AMT(1,3) TOT-AMT(1,4)
                  TOT-AMT(1,5) TOT-AMT(2,1) TOT-AMT(2,2) TOT-AMT(2,3)
                  TOT-AMT(2,4) TOT-AMT(2,5)
                  TOT-QTY(1,1) TOT-QTY(1,2) TOT-QTY(2,1) TOT-QTY(2,2).

      *----------------------------------------------------------------
       INVALID-RANGE.
           MOVE "INVALID RANGE" TO MESS.
           PERFORM SEND-MESS.

      *----------------------------------------------------------------
       SEND-LEGEND.
           MOVE MESS TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPTRSGS_SQL.CPY".
       COPY "LIBLP/LPTRLGS_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLTFGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPCDBGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    DO NOT ALLOW REWRITES ON LNFILE; (FIELDS MODIFIED).
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * COPY "LIBLP/LPLN1RN.CPY".
      * COPYMEMBER: LIBLP/LPLN1RN
       LOAD-LN1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( LN-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( LN-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO LN-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO LN-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO LN-ALL-DATA
              MOVE LN-ALL-PATH             TO LN-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( LN-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO LN-ALL-BASE
              MOVE FILPATH-MACHINE         TO LN-ALL-MACHINE
              MOVE FILPATH-DATA            TO LN-ALL-DATA
              MOVE LN-ALL-PATH             TO LN-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-LNFILE NOT = "S" )
              MOVE LN-PATH-BEG             TO LN-SELECT-PATH-BEG
              MOVE LN-PATH-FILE            TO LN-SELECT-PATH-FILE
           ELSE
              MOVE LN-PATH-SQL             TO LN-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-LN1-FILE.
           PERFORM LOAD-LN1-FILE.
           PERFORM OPEN-IT.
           MOVE LN-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-LN1-FILE.
           PERFORM START-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-SELECT-PATH TO E-FILE.
           MOVE LN1-KEY        TO E-KEYX.

           IF ( LN1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-LN1-FILE.

           IF ( QLN1-WEND-OWNBR = ZEROES    ) OR
              ( QLN1-WEND-OWNBR NOT NUMERIC )
              MOVE LN-OWNBR             TO QLN1-WBEG-OWNBR
              MOVE LN-OWNBR             TO QLN1-WEND-OWNBR
              MOVE LN-ACCTNO            TO QLN1-WBEG-ACCTNO
              MOVE ALL "9"              TO QLN1-WEND-ACCTNO.

           EXEC SQL
            DECLARE LN1_CURSOR CURSOR FOR
            SELECT LNFILE.LN_OWNBR,
                   LNFILE.LN_ACCTNO,
                   LNFILE.LN_CLASS,
                   LNFILE.LN_SUBCLASS,
                   LNFILE.LN_LAWCODE,
                   LNFILE.LN_SSNO1,
                   ISNULL(LNFILE.LN_MAKERCD1,''),
                   LNFILE.LN_SSNO2,
                   ISNULL(LNFILE.LN_MAKERCD2,''),
                   LNFILE.LN_SSNO3,
                   ISNULL(LNFILE.LN_MAKERCD3,''),
                   LNFILE.LN_SSNO4,
                   ISNULL(LNFILE.LN_MAKERCD4,''),
                   ISNULL(LNFILE.LN_ORGST,''),
                   LNFILE.LN_ORGBR,
                   ISNULL(LNFILE.LN_OWNST,''),
                   LNFILE.LN_SPRCLASS,
                   ISNULL(LNFILE.LN_CONVERCD,''),
                   ISNULL(LNFILE.LN_MIDSTREAM,''),
                   ISNULL(LNFILE.LN_PURCD,''),
                   ISNULL(LNFILE.LN_SRCD,''),
                   ISNULL(LNFILE.LN_COLLCD,''),
                   ISNULL(LNFILE.LN_COLLID,''),
                   ISNULL(LNFILE.LN_POCD,''),
                   ISNULL(LNFILE.LN_PLCD,''),
                   ISNULL(LNFILE.LN_PLTYPE,''),
                   ISNULL(LNFILE.LN_CONFIDENTCD,''),
                   ISNULL(LNFILE.LN_ALLOTCD,''),
                   ISNULL(LNFILE.LN_RECASTCD,''),
                   ISNULL(LNFILE.LN_SKIP_TRW,''),
                   ISNULL(LNFILE.LN_STATUSFG,''),
                   ISNULL(LNFILE.LN_SMALLOAN,''),
                   ISNULL(LNFILE.LN_SALEFIN,''),
                   ISNULL(LNFILE.LN_SERVACCT,''),
                   ISNULL(LNFILE.LN_LOANTYPE,''),
                   ISNULL(LNFILE.LN_ADDON_FG,''),
                   ISNULL(LNFILE.LN_PAY_SCHLD_FG,''),
                   LNFILE.LN_LC_CODE,
                   ISNULL(LNFILE.LN_CAN_LOAN_NO,''),
                   ISNULL(LNFILE.LN_STATFLAGS,''),
                   ISNULL(CAST(LNFILE.LN_ENTDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_LOANDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_PURDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INTDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_CASHDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_FILEDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_RESCISSION_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_PART_RECOURSE_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_ORIG_1STPYDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_1STPYDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_JDDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_PLDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_PL_TRDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_ACCELDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_REDUDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_POFFDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_PO_TRDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_LEGALDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_BNKRPTDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_BNKRPT_DISMISS_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_BNKRPT_DISCHARGE_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_BNKRPT_REAFFIRM_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_DEBTAIDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_REPODATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_REDEEMDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_SOLDDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INS_CONFIRM_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_PRIOR_RN_FOR10_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_LAST_RN_FOR10_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_TRW_DOFD_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_LAST_PYPA_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_BT_TRDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_METRO2_LAST_PMT_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_FOLLOWUP_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_LAST_ACCRUAL_DATE
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_APRATE,
                   LNFILE.LN_EFFRATE,
                   LNFILE.LN_SMPRATE,
                   LNFILE.LN_YLDRATE,
                   LNFILE.LN_OVRDRATE,
                   LNFILE.LN_ENDORATE,
                   LNFILE.LN_JDRATE,
                   LNFILE.LN_ACCRATE,
                   LNFILE.LN_REDURATE,
                   LNFILE.LN_CONRATE,
                   LNFILE.LN_SERVPER,
                   LNFILE.LN_SELLPR,
                   LNFILE.LN_DOWNPY,
                   LNFILE.LN_TRADEIN,
                   LNFILE.LN_LNAMT,
                   LNFILE.LN_FINCHG,
                   LNFILE.LN_INTCHG,
                   LNFILE.LN_SERCHG,
                   LNFILE.LN_EXTCHG,
                   LNFILE.LN_MAINTFEE,
                   LNFILE.LN_POINTS,
                   LNFILE.LN_PROCEEDS,
                   LNFILE.LN_ORGTERM,
                   LNFILE.LN_1STPYAMT,
                   LNFILE.LN_NOREGPY,
                   LNFILE.LN_REGPYAMT,
                   LNFILE.LN_LASTPYAMT,
                   LNFILE.LN_PROCASH,
                   LNFILE.LN_TILLNO,
                   ISNULL(LNFILE.LN_STATE_FLAG,''),
                   LNFILE.LN_STATE_WORKER,
                   LNFILE.LN_STATE_WORKER2,
                   ISNULL(LNFILE.LN_FEECODE_1,''),
                   LNFILE.LN_FEEAMT_1,
                   ISNULL(LNFILE.LN_FEEOURS_1,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_1,''),
                   ISNULL(LNFILE.LN_FEECODE_2,''),
                   LNFILE.LN_FEEAMT_2,
                   ISNULL(LNFILE.LN_FEEOURS_2,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_2,''),
                   ISNULL(LNFILE.LN_FEECODE_3,''),
                   LNFILE.LN_FEEAMT_3,
                   ISNULL(LNFILE.LN_FEEOURS_3,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_3,''),
                   ISNULL(LNFILE.LN_FEECODE_4,''),
                   LNFILE.LN_FEEAMT_4,
                   ISNULL(LNFILE.LN_FEEOURS_4,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_4,''),
                   ISNULL(LNFILE.LN_FEECODE_5,''),
                   LNFILE.LN_FEEAMT_5,
                   ISNULL(LNFILE.LN_FEEOURS_5,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_5,''),
                   ISNULL(LNFILE.LN_FEECODE_6,''),
                   LNFILE.LN_FEEAMT_6,
                   ISNULL(LNFILE.LN_FEEOURS_6,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_6,''),
                   ISNULL(LNFILE.LN_FEECODE_7,''),
                   LNFILE.LN_FEEAMT_7,
                   ISNULL(LNFILE.LN_FEEOURS_7,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_7,''),
                   ISNULL(LNFILE.LN_FEECODE_8,''),
                   LNFILE.LN_FEEAMT_8,
                   ISNULL(LNFILE.LN_FEEOURS_8,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_8,''),
                   ISNULL(LNFILE.LN_FEECODE_9,''),
                   LNFILE.LN_FEEAMT_9,
                   ISNULL(LNFILE.LN_FEEOURS_9,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_9,''),
                   ISNULL(LNFILE.LN_FEECODE_10,''),
                   LNFILE.LN_FEEAMT_10,
                   ISNULL(LNFILE.LN_FEEOURS_10,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_10,''),
                   ISNULL(LNFILE.LN_FEECODE_11,''),
                   LNFILE.LN_FEEAMT_11,
                   ISNULL(LNFILE.LN_FEEOURS_11,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_11,''),
                   ISNULL(LNFILE.LN_FEECODE_12,''),
                   LNFILE.LN_FEEAMT_12,
                   ISNULL(LNFILE.LN_FEEOURS_12,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_12,''),
                   ISNULL(LNFILE.LN_FEECODE_13,''),
                   LNFILE.LN_FEEAMT_13,
                   ISNULL(LNFILE.LN_FEEOURS_13,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_13,''),
                   ISNULL(LNFILE.LN_FEECODE_14,''),
                   LNFILE.LN_FEEAMT_14,
                   ISNULL(LNFILE.LN_FEEOURS_14,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_14,''),
                   ISNULL(LNFILE.LN_FEECODE_15,''),
                   LNFILE.LN_FEEAMT_15,
                   ISNULL(LNFILE.LN_FEEOURS_15,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_15,''),
                   ISNULL(LNFILE.LN_FEECODE_16,''),
                   LNFILE.LN_FEEAMT_16,
                   ISNULL(LNFILE.LN_FEEOURS_16,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_16,''),
                   ISNULL(LNFILE.LN_FEECODE_17,''),
                   LNFILE.LN_FEEAMT_17,
                   ISNULL(LNFILE.LN_FEEOURS_17,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_17,''),
                   ISNULL(LNFILE.LN_FEECODE_18,''),
                   LNFILE.LN_FEEAMT_18,
                   ISNULL(LNFILE.LN_FEEOURS_18,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_18,''),
                   ISNULL(LNFILE.LN_FEECODE_19,''),
                   LNFILE.LN_FEEAMT_19,
                   ISNULL(LNFILE.LN_FEEOURS_19,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_19,''),
                   ISNULL(LNFILE.LN_FEECODE_20,''),
                   LNFILE.LN_FEEAMT_20,
                   ISNULL(LNFILE.LN_FEEOURS_20,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_20,''),
                   ISNULL(LNFILE.LN_INSURED,''),
                   ISNULL(LNFILE.LN_INSTYPES,''),
                   ISNULL(LNFILE.LN_INSELIGIBLE,''),
                   LNFILE.LN_INSCOMP_1,
                   ISNULL(LNFILE.LN_INSOURS_1,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_1
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_1
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_1,
                   LNFILE.LN_INSCOVR_1,
                   LNFILE.LN_INSCOMM_1,
                   LNFILE.LN_INSANTICERN_1,
                   LNFILE.LN_INSANTICADJ_1,
                   LNFILE.LN_INSUFERN_1,
                   LNFILE.LN_INSACCERN_1,
                   LNFILE.LN_INSEARN_ORDER_1,
                   ISNULL(LNFILE.LN_INSFINCHG_1,''),
                   LNFILE.LN_INSCOMP_2,
                   ISNULL(LNFILE.LN_INSOURS_2,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_2
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_2
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_2,
                   LNFILE.LN_INSCOVR_2,
                   LNFILE.LN_INSCOMM_2,
                   LNFILE.LN_INSANTICERN_2,
                   LNFILE.LN_INSANTICADJ_2,
                   LNFILE.LN_INSUFERN_2,
                   LNFILE.LN_INSACCERN_2,
                   LNFILE.LN_INSEARN_ORDER_2,
                   ISNULL(LNFILE.LN_INSFINCHG_2,''),
                   LNFILE.LN_INSCOMP_3,
                   ISNULL(LNFILE.LN_INSOURS_3,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_3
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_3
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_3,
                   LNFILE.LN_INSCOVR_3,
                   LNFILE.LN_INSCOMM_3,
                   LNFILE.LN_INSANTICERN_3,
                   LNFILE.LN_INSANTICADJ_3,
                   LNFILE.LN_INSUFERN_3,
                   LNFILE.LN_INSACCERN_3,
                   LNFILE.LN_INSEARN_ORDER_3,
                   ISNULL(LNFILE.LN_INSFINCHG_3,''),
                   LNFILE.LN_INSCOMP_4,
                   ISNULL(LNFILE.LN_INSOURS_4,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_4
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_4
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_4,
                   LNFILE.LN_INSCOVR_4,
                   LNFILE.LN_INSCOMM_4,
                   LNFILE.LN_INSANTICERN_4,
                   LNFILE.LN_INSANTICADJ_4,
                   LNFILE.LN_INSUFERN_4,
                   LNFILE.LN_INSACCERN_4,
                   LNFILE.LN_INSEARN_ORDER_4,
                   ISNULL(LNFILE.LN_INSFINCHG_4,''),
                   LNFILE.LN_INSCOMP_5,
                   ISNULL(LNFILE.LN_INSOURS_5,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_5
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_5
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_5,
                   LNFILE.LN_INSCOVR_5,
                   LNFILE.LN_INSCOMM_5,
                   LNFILE.LN_INSANTICERN_5,
                   LNFILE.LN_INSANTICADJ_5,
                   LNFILE.LN_INSUFERN_5,
                   LNFILE.LN_INSACCERN_5,
                   LNFILE.LN_INSEARN_ORDER_5,
                   ISNULL(LNFILE.LN_INSFINCHG_5,''),
                   LNFILE.LN_INSCOMP_6,
                   ISNULL(LNFILE.LN_INSOURS_6,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_6
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_6
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_6,
                   LNFILE.LN_INSCOVR_6,
                   LNFILE.LN_INSCOMM_6,
                   LNFILE.LN_INSANTICERN_6,
                   LNFILE.LN_INSANTICADJ_6,
                   LNFILE.LN_INSUFERN_6,
                   LNFILE.LN_INSACCERN_6,
                   LNFILE.LN_INSEARN_ORDER_6,
                   ISNULL(LNFILE.LN_INSFINCHG_6,''),
                   LNFILE.LN_INSCOMP_7,
                   ISNULL(LNFILE.LN_INSOURS_7,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_7
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_7
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_7,
                   LNFILE.LN_INSCOVR_7,
                   LNFILE.LN_INSCOMM_7,
                   LNFILE.LN_INSANTICERN_7,
                   LNFILE.LN_INSANTICADJ_7,
                   LNFILE.LN_INSUFERN_7,
                   LNFILE.LN_INSACCERN_7,
                   LNFILE.LN_INSEARN_ORDER_7,
                   ISNULL(LNFILE.LN_INSFINCHG_7,''),
                   LNFILE.LN_INSCOMP_8,
                   ISNULL(LNFILE.LN_INSOURS_8,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_8
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_8
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_8,
                   LNFILE.LN_INSCOVR_8,
                   LNFILE.LN_INSCOMM_8,
                   LNFILE.LN_INSANTICERN_8,
                   LNFILE.LN_INSANTICADJ_8,
                   LNFILE.LN_INSUFERN_8,
                   LNFILE.LN_INSACCERN_8,
                   LNFILE.LN_INSEARN_ORDER_8,
                   ISNULL(LNFILE.LN_INSFINCHG_8,''),
                   LNFILE.LN_INSCOMP_9,
                   ISNULL(LNFILE.LN_INSOURS_9,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_9
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_9
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_9,
                   LNFILE.LN_INSCOVR_9,
                   LNFILE.LN_INSCOMM_9,
                   LNFILE.LN_INSANTICERN_9,
                   LNFILE.LN_INSANTICADJ_9,
                   LNFILE.LN_INSUFERN_9,
                   LNFILE.LN_INSACCERN_9,
                   LNFILE.LN_INSEARN_ORDER_9,
                   ISNULL(LNFILE.LN_INSFINCHG_9,''),
                   LNFILE.LN_INSCOMP_10,
                   ISNULL(LNFILE.LN_INSOURS_10,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_10
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_10
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_10,
                   LNFILE.LN_INSCOVR_10,
                   LNFILE.LN_INSCOMM_10,
                   LNFILE.LN_INSANTICERN_10,
                   LNFILE.LN_INSANTICADJ_10,
                   LNFILE.LN_INSUFERN_10,
                   LNFILE.LN_INSACCERN_10,
                   LNFILE.LN_INSEARN_ORDER_10,
                   ISNULL(LNFILE.LN_INSFINCHG_10,''),
                   LNFILE.LN_PY_YYYYMM_RECDEL,
                   LNFILE.LN_MTD_PAYS_RECDEL,
                   ISNULL(LNFILE.LN_GARNISH_EXTR,''),
                   ISNULL(LNFILE.LN_SHIP_SAME_FG,''),
                   LNFILE.LN_AD_D_INS_COVR,
                   LNFILE.LN_TOT_PARTIAL_CHGOFF,
                   ISNULL(LNFILE.LN_LPO,''),
                   ISNULL(LNFILE.LN_REP_CD,''),
                   LNFILE.LN_EMP_ID,
                   ISNULL(LNFILE.LN_PROCESS_CD,''),
                   LNFILE.LN_WCBC_CREDITLIM,
                   ISNULL(LNFILE.LN_FOLLOWUP_CD,''),
                   ISNULL(LNFILE.LN_MIP_FG,''),
                   LNFILE.LN_MIP_MTD_CLAHPAID,
                   ISNULL(CAST(LNFILE.LN_MIP_CLPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_MIP_TOT_CLPAID,
                   ISNULL(CAST(LNFILE.LN_MIP_AHPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_MIP_TOT_AHPAID,
                   LNFILE.LN_IL_GROSS_INCOME,
                   LNFILE.LN_IL_GROSS_INCOME2,
                   ISNULL(LNFILE.LN_RESCISSION,''),
                   ISNULL(LNFILE.LN_INTERIM_TRW,''),
                   LNFILE.LN_DLNO,
                   LNFILE.LN_DLPOOL,
                   ISNULL(LNFILE.LN_DLRECOURSE,''),
                   LNFILE.LN_DLPARTRCAMT,
                   LNFILE.LN_ORIGPURAMT,
                   LNFILE.LN_PURBAL,
                   LNFILE.LN_DLRVPRIN_1,
                   LNFILE.LN_DLRVPRIN_2,
                   LNFILE.LN_DLRVPRIN_3,
                   LNFILE.LN_DLRVCHGS_1,
                   LNFILE.LN_DLRVCHGS_2,
                   LNFILE.LN_DLRVCHGS_3,
                   LNFILE.LN_DLPARVHELD,
                   LNFILE.LN_DLPARVPAID,
                   LNFILE.LN_DLDISC,
                   ISNULL(CAST(LNFILE.LN_LCPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INTPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_CURBAL,
                   LNFILE.LN_INTBAL,
                   LNFILE.LN_LCBAL,
                   LNFILE.LN_OTHBAL,
                   LNFILE.LN_OT2BAL,
                   LNFILE.LN_LCPAID,
                   LNFILE.LN_LCPARTIALS,
                   LNFILE.LN_STMT_COUNTER,
                   LNFILE.LN_PRIOR_STMT_COUNTER,
                   ISNULL(LNFILE.LN_FROZEN_REASON,''),
                   ISNULL(LNFILE.LN_COUPON_STATEMENT,''),
                   LNFILE.LN_MONTHEND_BAL,
                   LNFILE.LN_ANTICTERM,
                   LNFILE.LN_ANTICERN_1,
                   LNFILE.LN_ANTICADJ_1,
                   LNFILE.LN_UFERN_1,
                   LNFILE.LN_ACCERN_1,
                   LNFILE.LN_EARN_ORDER_1,
                   LNFILE.LN_ANTICERN_2,
                   LNFILE.LN_ANTICADJ_2,
                   LNFILE.LN_UFERN_2,
                   LNFILE.LN_ACCERN_2,
                   LNFILE.LN_EARN_ORDER_2,
                   LNFILE.LN_ANTICERN_3,
                   LNFILE.LN_ANTICADJ_3,
                   LNFILE.LN_UFERN_3,
                   LNFILE.LN_ACCERN_3,
                   LNFILE.LN_EARN_ORDER_3,
                   LNFILE.LN_ANTICERN_4,
                   LNFILE.LN_ANTICADJ_4,
                   LNFILE.LN_UFERN_4,
                   LNFILE.LN_ACCERN_4,
                   LNFILE.LN_EARN_ORDER_4,
                   LNFILE.LN_ANTICERN_5,
                   LNFILE.LN_ANTICADJ_5,
                   LNFILE.LN_UFERN_5,
                   LNFILE.LN_ACCERN_5,
                   LNFILE.LN_EARN_ORDER_5,
                   LNFILE.LN_ANTICERN_6,
                   LNFILE.LN_ANTICADJ_6,
                   LNFILE.LN_UFERN_6,
                   LNFILE.LN_ACCERN_6,
                   LNFILE.LN_EARN_ORDER_6,
                   LNFILE.LN_ANTICERN_7,
                   LNFILE.LN_ANTICADJ_7,
                   LNFILE.LN_UFERN_7,
                   LNFILE.LN_ACCERN_7,
                   LNFILE.LN_EARN_ORDER_7,
                   LNFILE.LN_ANTICERN_8,
                   LNFILE.LN_ANTICADJ_8,
                   LNFILE.LN_UFERN_8,
                   LNFILE.LN_ACCERN_8,
                   LNFILE.LN_EARN_ORDER_8,
                   LNFILE.LN_ANTICERN_9,
                   LNFILE.LN_ANTICADJ_9,
                   LNFILE.LN_UFERN_9,
                   LNFILE.LN_ACCERN_9,
                   LNFILE.LN_EARN_ORDER_9,
                   LNFILE.LN_ANTICERN_10,
                   LNFILE.LN_ANTICADJ_10,
                   LNFILE.LN_UFERN_10,
                   LNFILE.LN_ACCERN_10,
                   LNFILE.LN_EARN_ORDER_10,
                   LNFILE.LN_LMO_PRIOR_INTEREST,
                   LNFILE.LN_PRIOR_UNEARNED,
                   ISNULL(LNFILE.LN_ERNDELQ_STOP,''),
                   LNFILE.LN_NONACRERN,
                   LNFILE.LN_PRIOR_NONACRERN,
                   ISNULL(CAST(LNFILE.LN_ERN_IB_PDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_ERN_IB_PRINBAL,
                   LNFILE.LN_ERN_IB_INTOWE,
                   LNFILE.LN_TOTERN,
                   LNFILE.LN_TOTOTH,
                   LNFILE.LN_TOTINT,
                   LNFILE.LN_TOTLCHG,
                   LNFILE.LN_TOTDEF,
                   LNFILE.LN_TOTSERACCT,
                   LNFILE.LN_LCWAIVERS,
                   LNFILE.LN_INTWAIVERS,
                   LNFILE.LN_TOTPAYMNTD,
                   LNFILE.LN_TOTEXCPAYMNTD,
                   LNFILE.LN_TOTNODEF,
                   LNFILE.LN_YTDNODEF,
                   LNFILE.LN_TOTNODEFACT,
                   LNFILE.LN_YTDNODEFACT,
                   LNFILE.LN_TOTNOALDEL,
                   LNFILE.LN_YTDNOALDEL,
                   LNFILE.LN_YTDINTPD,
                   LNFILE.LN_LYRINTPD,
                   LNFILE.LN_TOTINTPD,
                   LNFILE.LN_RECASTINT,
                   LNFILE.LN_RECASTPRIN,
                   ISNULL(LNFILE.LN_CL_REBATE,''),
                   ISNULL(LNFILE.LN_AH_REBATE,''),
                   ISNULL(LNFILE.LN_PP_REBATE,''),
                   ISNULL(LNFILE.LN_INT_REBATE,''),
                   ISNULL(LNFILE.LN_SC_REBATE,''),
                   ISNULL(LNFILE.LN_MF_REBATE,''),
                   ISNULL(LNFILE.LN_DF_REBATE,''),
                   ISNULL(LNFILE.LN_EX_REBATE,''),
                   ISNULL(LNFILE.LN_O1_REBATE,''),
                   ISNULL(LNFILE.LN_O2_REBATE,''),
                   ISNULL(LNFILE.LN_O3_REBATE,''),
                   ISNULL(LNFILE.LN_O4_REBATE,''),
                   ISNULL(LNFILE.LN_O5_REBATE,''),
                   ISNULL(LNFILE.LN_O6_REBATE,''),
                   ISNULL(LNFILE.LN_O7_REBATE,''),
                   ISNULL(LNFILE.LN_UN_O7_REB,''),
                   ISNULL(LNFILE.LN_UN_O8_REB,''),
                   ISNULL(LNFILE.LN_UN_O9_REB,''),
                   ISNULL(LNFILE.LN_UN_O10_REB,''),
                   ISNULL(LNFILE.LN_PLREASON,''),
                   ISNULL(LNFILE.LN_COLLECTOR,''),
                   ISNULL(LNFILE.LN_ACTIONCD,''),
                   ISNULL(LNFILE.LN_ACTIONCD2,''),
                   ISNULL(LNFILE.LN_ATCODE,''),
                   ISNULL(LNFILE.LN_RECODE,''),
                   ISNULL(LNFILE.LN_ESCROW_FG,''),
                   LNFILE.LN_PROMDAYS_PASTDUE,
                   ISNULL(LNFILE.LN_PROMID,''),
                   ISNULL(CAST(LNFILE.LN_PROMDATE AS VARCHAR(10)),'0'),
                   LNFILE.LN_PROMAMT,
                   ISNULL(CAST(LNFILE.LN_PROMDATE2
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_DATE_PAID_LAST
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_AMT_PAID_LAST,
                   LNFILE.LN_LASTCFORMNO,
                   ISNULL(CAST(LNFILE.LN_LASTCFORMDATE
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_DELCNT,
                   LNFILE.LN_DELCNT2,
                   LNFILE.LN_CREDITLIM,
                   LNFILE.LN_CRSCORE,
                   LNFILE.LN_LASTSFORMNO,
                   ISNULL(CAST(LNFILE.LN_LASTSFORMDATE
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_SOLCNT,
                   ISNULL(LNFILE.LN_MESSAGE,''),
                   LNFILE.LN_OLDACCTNO,
                   LNFILE.LN_PRLNNO,
                   LNFILE.LN_PRBAL,
                   LNFILE.LN_PRINTDUE,
                   LNFILE.LN_PRFINCHGRB,
                   LNFILE.LN_PRINSRB,
                   ISNULL(LNFILE.LN_PRPOCD,''),
                   ISNULL(CAST(LNFILE.LN_PRPODATE AS VARCHAR(10)),'0'),
                   LNFILE.LN_PRDELINQAMT,
                   LNFILE.LN_PRLNAMT,
                   LNFILE.LN_PRPDUE_DAYS,
                   LNFILE.LN_PRDLDISC_UNEARN,
                   LNFILE.LN_PRCONPROFILE,
                   LNFILE.LN_SUM_ORIGPURAMT,
                   LNFILE.LN_SUM_LNAMT,
                   LNFILE.LN_SUM_FINCHG,
                   LNFILE.LN_SUM_INTCHG,
                   LNFILE.LN_SUM_ORGTERM,
                   LNFILE.LN_SUM_REGPYAMT,
                   LNFILE.LN_SUM_EXTCHG,
                   LNFILE.LN_SUM_SERCHG,
                   LNFILE.LN_SUM_MAINTFEE,
                   ISNULL(LNFILE.LN_UNITPER_CD,''),
                   LNFILE.LN_UNITPER_FREQ,
                   LNFILE.LN_DAYS_IN_UNITPER,
                   LNFILE.LN_UNITPER_PER_YEAR,
                   ISNULL(LNFILE.LN_REPOCD,''),
                   ISNULL(LNFILE.LN_BR_TFR_FG,''),
                   LNFILE.LN_O3INS_DLPROC,
                   LNFILE.LN_INSPREM_DLADJ,
                   LNFILE.LN_PLAMT,
                   LNFILE.LN_MTD_PRIN,
                   LNFILE.LN_MTD_LC,
                   LNFILE.LN_MTD_INT,
                   LNFILE.LN_MTD_OT,
                   LNFILE.LN_MTD_EX,
                   LNFILE.LN_BNKRPT_CHAPTER,
                   ISNULL(LNFILE.LN_BMO_CON_STATUS,''),
                   ISNULL(LNFILE.LN_BMO_REC_STATUS,''),
                   LNFILE.LN_APP_NUMBER,
                   ISNULL(LNFILE.LN_GIFTNET,''),
                   LNFILE.LN_GIFTNET_OUR_PO,
                   ISNULL(LNFILE.LN_PROC_DISBURSED,''),
                   ISNULL(LNFILE.LN_NOODDPY,''),
                   LNFILE.LN_CONPROFILE_1,
                   LNFILE.LN_CONPROFILE_2,
                   LNFILE.LN_CONPROFILE_3,
                   LNFILE.LN_CONPROFILE_4,
                   LNFILE.LN_CONPROFILE_5,
                   LNFILE.LN_RECPROFILE_1,
                   LNFILE.LN_RECPROFILE_2,
                   LNFILE.LN_RECPROFILE_3,
                   LNFILE.LN_RECPROFILE_4,
                   LNFILE.LN_RECPROFILE_5,
                   ISNULL(LNFILE.LN_TRW_STATUS,''),
                   ISNULL(LNFILE.LN_ORIGIN,''),
                   ISNULL(LNFILE.LN_PO_TRCD,''),
                   LNFILE.LN_NEXIS_CRSCORE,
                   ISNULL(LNFILE.LN_TRW_B_CONSUMER_IND,''),
                   ISNULL(LNFILE.LN_PARVHELD_PAID_FG,''),
                   ISNULL(LNFILE.LN_ACCT_FROZEN,''),
                   ISNULL(LNFILE.LN_TRW_SPECIAL_CODE,''),
                   ISNULL(LNFILE.LN_CREATION_STATUS,''),
                   LNFILE.LN_TOTNOLCHG,
                   ISNULL(LNFILE.LN_LBOX,''),
                   LNFILE.LN_EXTRA_PRIN_LFP,
                   LNFILE.LN_REMAIN_INT_SINCE_LFP,
                   LNFILE.LN_REMAIN_PRIN_SINCE_LFP,
                   ISNULL(LNFILE.LN_INTERIM_COUPON,''),
                   LNFILE.LN_DLCOMM_O3_CMSI,
                   LNFILE.LN_DLCOMM_PP_CMSI,
                   ISNULL(LNFILE.LN_TRW_J_CONSUMER_IND,''),
                   ISNULL(LNFILE.LN_ACH,''),
                   ISNULL(LNFILE.LN_1099_C_FG,''),
                   ISNULL(LNFILE.LN_TRW_COMPLIANCE_CODE,''),
                   ISNULL(LNFILE.LN_UNWIND_REASON_CODE,''),
                   ISNULL(LNFILE.LN_PBI_PBD_FG,''),
                   ISNULL(LNFILE.LN_OLD_ACCT_FORMAT_FG,''),
                   ISNULL(LNFILE.LN_APPROVED_BY_USER,''),
                   ISNULL(LNFILE.LN_APPROVED_BY_CODE,''),
                   ISNULL(CAST(LNFILE.LN_APPROVED_BY_DATE
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_APPROVED_BY_TIME,
                   ISNULL(CAST(LNFILE.LN_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_LTOUCH_TIME
            FROM DBO.LNFILE
            WHERE LNFILE.LN_OWNBR >= :QLN1-WBEG-OWNBR
              AND LNFILE.LN_OWNBR <= :QLN1-WEND-OWNBR
              AND LNFILE.LN_ACCTNO >= :QLN1-WBEG-ACCTNO
              AND LNFILE.LN_ACCTNO <= :QLN1-WEND-ACCTNO 
            ORDER BY LNFILE.LN_OWNBR,
                     LNFILE.LN_ACCTNO
           END-EXEC.

           EXEC SQL
               OPEN LN1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QLN1-WBEG-OWNBR
                          QLN1-WEND-OWNBR
                          QLN1-WBEG-ACCTNO
                          QLN1-WEND-ACCTNO.

           MOVE STAT---GOOD TO LN1-CURSOR-STAT.

      *-----------------------------------------------------------------
       START-LN1-FILE-GREATER.
           PERFORM START-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-SELECT-PATH TO E-FILE.
           MOVE LN1-KEY        TO E-KEYX.

           IF ( LN1-CURSOR-GR-STAT-GOOD )
              PERFORM CLOSE-LN1-FILE.

           IF ( QLN1-WEND-OWNBR = ZEROES    ) OR
              ( QLN1-WEND-OWNBR NOT NUMERIC )
              MOVE LN-OWNBR             TO QLN1-WBEG-OWNBR
              MOVE LN-OWNBR             TO QLN1-WEND-OWNBR
              MOVE LN-ACCTNO            TO QLN1-WBEG-ACCTNO
              MOVE ALL "9"              TO QLN1-WEND-ACCTNO.

           EXEC SQL
            DECLARE LN1_CURSOR_GR CURSOR FOR
            SELECT LNFILE.LN_OWNBR,
                   LNFILE.LN_ACCTNO,
                   LNFILE.LN_CLASS,
                   LNFILE.LN_SUBCLASS,
                   LNFILE.LN_LAWCODE,
                   LNFILE.LN_SSNO1,
                   ISNULL(LNFILE.LN_MAKERCD1,''),
                   LNFILE.LN_SSNO2,
                   ISNULL(LNFILE.LN_MAKERCD2,''),
                   LNFILE.LN_SSNO3,
                   ISNULL(LNFILE.LN_MAKERCD3,''),
                   LNFILE.LN_SSNO4,
                   ISNULL(LNFILE.LN_MAKERCD4,''),
                   ISNULL(LNFILE.LN_ORGST,''),
                   LNFILE.LN_ORGBR,
                   ISNULL(LNFILE.LN_OWNST,''),
                   LNFILE.LN_SPRCLASS,
                   ISNULL(LNFILE.LN_CONVERCD,''),
                   ISNULL(LNFILE.LN_MIDSTREAM,''),
                   ISNULL(LNFILE.LN_PURCD,''),
                   ISNULL(LNFILE.LN_SRCD,''),
                   ISNULL(LNFILE.LN_COLLCD,''),
                   ISNULL(LNFILE.LN_COLLID,''),
                   ISNULL(LNFILE.LN_POCD,''),
                   ISNULL(LNFILE.LN_PLCD,''),
                   ISNULL(LNFILE.LN_PLTYPE,''),
                   ISNULL(LNFILE.LN_CONFIDENTCD,''),
                   ISNULL(LNFILE.LN_ALLOTCD,''),
                   ISNULL(LNFILE.LN_RECASTCD,''),
                   ISNULL(LNFILE.LN_SKIP_TRW,''),
                   ISNULL(LNFILE.LN_STATUSFG,''),
                   ISNULL(LNFILE.LN_SMALLOAN,''),
                   ISNULL(LNFILE.LN_SALEFIN,''),
                   ISNULL(LNFILE.LN_SERVACCT,''),
                   ISNULL(LNFILE.LN_LOANTYPE,''),
                   ISNULL(LNFILE.LN_ADDON_FG,''),
                   ISNULL(LNFILE.LN_PAY_SCHLD_FG,''),
                   LNFILE.LN_LC_CODE,
                   ISNULL(LNFILE.LN_CAN_LOAN_NO,''),
                   ISNULL(LNFILE.LN_STATFLAGS,''),
                   ISNULL(CAST(LNFILE.LN_ENTDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_LOANDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_PURDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INTDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_CASHDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_FILEDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_RESCISSION_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_PART_RECOURSE_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_ORIG_1STPYDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_1STPYDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_JDDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_PLDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_PL_TRDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_ACCELDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_REDUDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_POFFDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_PO_TRDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_LEGALDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_BNKRPTDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_BNKRPT_DISMISS_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_BNKRPT_DISCHARGE_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_BNKRPT_REAFFIRM_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_DEBTAIDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_REPODATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_REDEEMDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_SOLDDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INS_CONFIRM_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_PRIOR_RN_FOR10_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_LAST_RN_FOR10_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_TRW_DOFD_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_LAST_PYPA_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_BT_TRDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_METRO2_LAST_PMT_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_FOLLOWUP_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_LAST_ACCRUAL_DATE
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_APRATE,
                   LNFILE.LN_EFFRATE,
                   LNFILE.LN_SMPRATE,
                   LNFILE.LN_YLDRATE,
                   LNFILE.LN_OVRDRATE,
                   LNFILE.LN_ENDORATE,
                   LNFILE.LN_JDRATE,
                   LNFILE.LN_ACCRATE,
                   LNFILE.LN_REDURATE,
                   LNFILE.LN_CONRATE,
                   LNFILE.LN_SERVPER,
                   LNFILE.LN_SELLPR,
                   LNFILE.LN_DOWNPY,
                   LNFILE.LN_TRADEIN,
                   LNFILE.LN_LNAMT,
                   LNFILE.LN_FINCHG,
                   LNFILE.LN_INTCHG,
                   LNFILE.LN_SERCHG,
                   LNFILE.LN_EXTCHG,
                   LNFILE.LN_MAINTFEE,
                   LNFILE.LN_POINTS,
                   LNFILE.LN_PROCEEDS,
                   LNFILE.LN_ORGTERM,
                   LNFILE.LN_1STPYAMT,
                   LNFILE.LN_NOREGPY,
                   LNFILE.LN_REGPYAMT,
                   LNFILE.LN_LASTPYAMT,
                   LNFILE.LN_PROCASH,
                   LNFILE.LN_TILLNO,
                   ISNULL(LNFILE.LN_STATE_FLAG,''),
                   LNFILE.LN_STATE_WORKER,
                   LNFILE.LN_STATE_WORKER2,
                   ISNULL(LNFILE.LN_FEECODE_1,''),
                   LNFILE.LN_FEEAMT_1,
                   ISNULL(LNFILE.LN_FEEOURS_1,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_1,''),
                   ISNULL(LNFILE.LN_FEECODE_2,''),
                   LNFILE.LN_FEEAMT_2,
                   ISNULL(LNFILE.LN_FEEOURS_2,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_2,''),
                   ISNULL(LNFILE.LN_FEECODE_3,''),
                   LNFILE.LN_FEEAMT_3,
                   ISNULL(LNFILE.LN_FEEOURS_3,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_3,''),
                   ISNULL(LNFILE.LN_FEECODE_4,''),
                   LNFILE.LN_FEEAMT_4,
                   ISNULL(LNFILE.LN_FEEOURS_4,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_4,''),
                   ISNULL(LNFILE.LN_FEECODE_5,''),
                   LNFILE.LN_FEEAMT_5,
                   ISNULL(LNFILE.LN_FEEOURS_5,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_5,''),
                   ISNULL(LNFILE.LN_FEECODE_6,''),
                   LNFILE.LN_FEEAMT_6,
                   ISNULL(LNFILE.LN_FEEOURS_6,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_6,''),
                   ISNULL(LNFILE.LN_FEECODE_7,''),
                   LNFILE.LN_FEEAMT_7,
                   ISNULL(LNFILE.LN_FEEOURS_7,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_7,''),
                   ISNULL(LNFILE.LN_FEECODE_8,''),
                   LNFILE.LN_FEEAMT_8,
                   ISNULL(LNFILE.LN_FEEOURS_8,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_8,''),
                   ISNULL(LNFILE.LN_FEECODE_9,''),
                   LNFILE.LN_FEEAMT_9,
                   ISNULL(LNFILE.LN_FEEOURS_9,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_9,''),
                   ISNULL(LNFILE.LN_FEECODE_10,''),
                   LNFILE.LN_FEEAMT_10,
                   ISNULL(LNFILE.LN_FEEOURS_10,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_10,''),
                   ISNULL(LNFILE.LN_FEECODE_11,''),
                   LNFILE.LN_FEEAMT_11,
                   ISNULL(LNFILE.LN_FEEOURS_11,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_11,''),
                   ISNULL(LNFILE.LN_FEECODE_12,''),
                   LNFILE.LN_FEEAMT_12,
                   ISNULL(LNFILE.LN_FEEOURS_12,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_12,''),
                   ISNULL(LNFILE.LN_FEECODE_13,''),
                   LNFILE.LN_FEEAMT_13,
                   ISNULL(LNFILE.LN_FEEOURS_13,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_13,''),
                   ISNULL(LNFILE.LN_FEECODE_14,''),
                   LNFILE.LN_FEEAMT_14,
                   ISNULL(LNFILE.LN_FEEOURS_14,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_14,''),
                   ISNULL(LNFILE.LN_FEECODE_15,''),
                   LNFILE.LN_FEEAMT_15,
                   ISNULL(LNFILE.LN_FEEOURS_15,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_15,''),
                   ISNULL(LNFILE.LN_FEECODE_16,''),
                   LNFILE.LN_FEEAMT_16,
                   ISNULL(LNFILE.LN_FEEOURS_16,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_16,''),
                   ISNULL(LNFILE.LN_FEECODE_17,''),
                   LNFILE.LN_FEEAMT_17,
                   ISNULL(LNFILE.LN_FEEOURS_17,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_17,''),
                   ISNULL(LNFILE.LN_FEECODE_18,''),
                   LNFILE.LN_FEEAMT_18,
                   ISNULL(LNFILE.LN_FEEOURS_18,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_18,''),
                   ISNULL(LNFILE.LN_FEECODE_19,''),
                   LNFILE.LN_FEEAMT_19,
                   ISNULL(LNFILE.LN_FEEOURS_19,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_19,''),
                   ISNULL(LNFILE.LN_FEECODE_20,''),
                   LNFILE.LN_FEEAMT_20,
                   ISNULL(LNFILE.LN_FEEOURS_20,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_20,''),
                   ISNULL(LNFILE.LN_INSURED,''),
                   ISNULL(LNFILE.LN_INSTYPES,''),
                   ISNULL(LNFILE.LN_INSELIGIBLE,''),
                   LNFILE.LN_INSCOMP_1,
                   ISNULL(LNFILE.LN_INSOURS_1,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_1
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_1
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_1,
                   LNFILE.LN_INSCOVR_1,
                   LNFILE.LN_INSCOMM_1,
                   LNFILE.LN_INSANTICERN_1,
                   LNFILE.LN_INSANTICADJ_1,
                   LNFILE.LN_INSUFERN_1,
                   LNFILE.LN_INSACCERN_1,
                   LNFILE.LN_INSEARN_ORDER_1,
                   ISNULL(LNFILE.LN_INSFINCHG_1,''),
                   LNFILE.LN_INSCOMP_2,
                   ISNULL(LNFILE.LN_INSOURS_2,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_2
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_2
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_2,
                   LNFILE.LN_INSCOVR_2,
                   LNFILE.LN_INSCOMM_2,
                   LNFILE.LN_INSANTICERN_2,
                   LNFILE.LN_INSANTICADJ_2,
                   LNFILE.LN_INSUFERN_2,
                   LNFILE.LN_INSACCERN_2,
                   LNFILE.LN_INSEARN_ORDER_2,
                   ISNULL(LNFILE.LN_INSFINCHG_2,''),
                   LNFILE.LN_INSCOMP_3,
                   ISNULL(LNFILE.LN_INSOURS_3,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_3
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_3
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_3,
                   LNFILE.LN_INSCOVR_3,
                   LNFILE.LN_INSCOMM_3,
                   LNFILE.LN_INSANTICERN_3,
                   LNFILE.LN_INSANTICADJ_3,
                   LNFILE.LN_INSUFERN_3,
                   LNFILE.LN_INSACCERN_3,
                   LNFILE.LN_INSEARN_ORDER_3,
                   ISNULL(LNFILE.LN_INSFINCHG_3,''),
                   LNFILE.LN_INSCOMP_4,
                   ISNULL(LNFILE.LN_INSOURS_4,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_4
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_4
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_4,
                   LNFILE.LN_INSCOVR_4,
                   LNFILE.LN_INSCOMM_4,
                   LNFILE.LN_INSANTICERN_4,
                   LNFILE.LN_INSANTICADJ_4,
                   LNFILE.LN_INSUFERN_4,
                   LNFILE.LN_INSACCERN_4,
                   LNFILE.LN_INSEARN_ORDER_4,
                   ISNULL(LNFILE.LN_INSFINCHG_4,''),
                   LNFILE.LN_INSCOMP_5,
                   ISNULL(LNFILE.LN_INSOURS_5,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_5
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_5
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_5,
                   LNFILE.LN_INSCOVR_5,
                   LNFILE.LN_INSCOMM_5,
                   LNFILE.LN_INSANTICERN_5,
                   LNFILE.LN_INSANTICADJ_5,
                   LNFILE.LN_INSUFERN_5,
                   LNFILE.LN_INSACCERN_5,
                   LNFILE.LN_INSEARN_ORDER_5,
                   ISNULL(LNFILE.LN_INSFINCHG_5,''),
                   LNFILE.LN_INSCOMP_6,
                   ISNULL(LNFILE.LN_INSOURS_6,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_6
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_6
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_6,
                   LNFILE.LN_INSCOVR_6,
                   LNFILE.LN_INSCOMM_6,
                   LNFILE.LN_INSANTICERN_6,
                   LNFILE.LN_INSANTICADJ_6,
                   LNFILE.LN_INSUFERN_6,
                   LNFILE.LN_INSACCERN_6,
                   LNFILE.LN_INSEARN_ORDER_6,
                   ISNULL(LNFILE.LN_INSFINCHG_6,''),
                   LNFILE.LN_INSCOMP_7,
                   ISNULL(LNFILE.LN_INSOURS_7,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_7
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_7
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_7,
                   LNFILE.LN_INSCOVR_7,
                   LNFILE.LN_INSCOMM_7,
                   LNFILE.LN_INSANTICERN_7,
                   LNFILE.LN_INSANTICADJ_7,
                   LNFILE.LN_INSUFERN_7,
                   LNFILE.LN_INSACCERN_7,
                   LNFILE.LN_INSEARN_ORDER_7,
                   ISNULL(LNFILE.LN_INSFINCHG_7,''),
                   LNFILE.LN_INSCOMP_8,
                   ISNULL(LNFILE.LN_INSOURS_8,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_8
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_8
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_8,
                   LNFILE.LN_INSCOVR_8,
                   LNFILE.LN_INSCOMM_8,
                   LNFILE.LN_INSANTICERN_8,
                   LNFILE.LN_INSANTICADJ_8,
                   LNFILE.LN_INSUFERN_8,
                   LNFILE.LN_INSACCERN_8,
                   LNFILE.LN_INSEARN_ORDER_8,
                   ISNULL(LNFILE.LN_INSFINCHG_8,''),
                   LNFILE.LN_INSCOMP_9,
                   ISNULL(LNFILE.LN_INSOURS_9,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_9
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_9
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_9,
                   LNFILE.LN_INSCOVR_9,
                   LNFILE.LN_INSCOMM_9,
                   LNFILE.LN_INSANTICERN_9,
                   LNFILE.LN_INSANTICADJ_9,
                   LNFILE.LN_INSUFERN_9,
                   LNFILE.LN_INSACCERN_9,
                   LNFILE.LN_INSEARN_ORDER_9,
                   ISNULL(LNFILE.LN_INSFINCHG_9,''),
                   LNFILE.LN_INSCOMP_10,
                   ISNULL(LNFILE.LN_INSOURS_10,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_10
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_10
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_10,
                   LNFILE.LN_INSCOVR_10,
                   LNFILE.LN_INSCOMM_10,
                   LNFILE.LN_INSANTICERN_10,
                   LNFILE.LN_INSANTICADJ_10,
                   LNFILE.LN_INSUFERN_10,
                   LNFILE.LN_INSACCERN_10,
                   LNFILE.LN_INSEARN_ORDER_10,
                   ISNULL(LNFILE.LN_INSFINCHG_10,''),
                   LNFILE.LN_PY_YYYYMM_RECDEL,
                   LNFILE.LN_MTD_PAYS_RECDEL,
                   ISNULL(LNFILE.LN_GARNISH_EXTR,''),
                   ISNULL(LNFILE.LN_SHIP_SAME_FG,''),
                   LNFILE.LN_AD_D_INS_COVR,
                   LNFILE.LN_TOT_PARTIAL_CHGOFF,
                   ISNULL(LNFILE.LN_LPO,''),
                   ISNULL(LNFILE.LN_REP_CD,''),
                   LNFILE.LN_EMP_ID,
                   ISNULL(LNFILE.LN_PROCESS_CD,''),
                   LNFILE.LN_WCBC_CREDITLIM,
                   ISNULL(LNFILE.LN_FOLLOWUP_CD,''),
                   ISNULL(LNFILE.LN_MIP_FG,''),
                   LNFILE.LN_MIP_MTD_CLAHPAID,
                   ISNULL(CAST(LNFILE.LN_MIP_CLPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_MIP_TOT_CLPAID,
                   ISNULL(CAST(LNFILE.LN_MIP_AHPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_MIP_TOT_AHPAID,
                   LNFILE.LN_IL_GROSS_INCOME,
                   LNFILE.LN_IL_GROSS_INCOME2,
                   ISNULL(LNFILE.LN_RESCISSION,''),
                   ISNULL(LNFILE.LN_INTERIM_TRW,''),
                   LNFILE.LN_DLNO,
                   LNFILE.LN_DLPOOL,
                   ISNULL(LNFILE.LN_DLRECOURSE,''),
                   LNFILE.LN_DLPARTRCAMT,
                   LNFILE.LN_ORIGPURAMT,
                   LNFILE.LN_PURBAL,
                   LNFILE.LN_DLRVPRIN_1,
                   LNFILE.LN_DLRVPRIN_2,
                   LNFILE.LN_DLRVPRIN_3,
                   LNFILE.LN_DLRVCHGS_1,
                   LNFILE.LN_DLRVCHGS_2,
                   LNFILE.LN_DLRVCHGS_3,
                   LNFILE.LN_DLPARVHELD,
                   LNFILE.LN_DLPARVPAID,
                   LNFILE.LN_DLDISC,
                   ISNULL(CAST(LNFILE.LN_LCPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INTPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_CURBAL,
                   LNFILE.LN_INTBAL,
                   LNFILE.LN_LCBAL,
                   LNFILE.LN_OTHBAL,
                   LNFILE.LN_OT2BAL,
                   LNFILE.LN_LCPAID,
                   LNFILE.LN_LCPARTIALS,
                   LNFILE.LN_STMT_COUNTER,
                   LNFILE.LN_PRIOR_STMT_COUNTER,
                   ISNULL(LNFILE.LN_FROZEN_REASON,''),
                   ISNULL(LNFILE.LN_COUPON_STATEMENT,''),
                   LNFILE.LN_MONTHEND_BAL,
                   LNFILE.LN_ANTICTERM,
                   LNFILE.LN_ANTICERN_1,
                   LNFILE.LN_ANTICADJ_1,
                   LNFILE.LN_UFERN_1,
                   LNFILE.LN_ACCERN_1,
                   LNFILE.LN_EARN_ORDER_1,
                   LNFILE.LN_ANTICERN_2,
                   LNFILE.LN_ANTICADJ_2,
                   LNFILE.LN_UFERN_2,
                   LNFILE.LN_ACCERN_2,
                   LNFILE.LN_EARN_ORDER_2,
                   LNFILE.LN_ANTICERN_3,
                   LNFILE.LN_ANTICADJ_3,
                   LNFILE.LN_UFERN_3,
                   LNFILE.LN_ACCERN_3,
                   LNFILE.LN_EARN_ORDER_3,
                   LNFILE.LN_ANTICERN_4,
                   LNFILE.LN_ANTICADJ_4,
                   LNFILE.LN_UFERN_4,
                   LNFILE.LN_ACCERN_4,
                   LNFILE.LN_EARN_ORDER_4,
                   LNFILE.LN_ANTICERN_5,
                   LNFILE.LN_ANTICADJ_5,
                   LNFILE.LN_UFERN_5,
                   LNFILE.LN_ACCERN_5,
                   LNFILE.LN_EARN_ORDER_5,
                   LNFILE.LN_ANTICERN_6,
                   LNFILE.LN_ANTICADJ_6,
                   LNFILE.LN_UFERN_6,
                   LNFILE.LN_ACCERN_6,
                   LNFILE.LN_EARN_ORDER_6,
                   LNFILE.LN_ANTICERN_7,
                   LNFILE.LN_ANTICADJ_7,
                   LNFILE.LN_UFERN_7,
                   LNFILE.LN_ACCERN_7,
                   LNFILE.LN_EARN_ORDER_7,
                   LNFILE.LN_ANTICERN_8,
                   LNFILE.LN_ANTICADJ_8,
                   LNFILE.LN_UFERN_8,
                   LNFILE.LN_ACCERN_8,
                   LNFILE.LN_EARN_ORDER_8,
                   LNFILE.LN_ANTICERN_9,
                   LNFILE.LN_ANTICADJ_9,
                   LNFILE.LN_UFERN_9,
                   LNFILE.LN_ACCERN_9,
                   LNFILE.LN_EARN_ORDER_9,
                   LNFILE.LN_ANTICERN_10,
                   LNFILE.LN_ANTICADJ_10,
                   LNFILE.LN_UFERN_10,
                   LNFILE.LN_ACCERN_10,
                   LNFILE.LN_EARN_ORDER_10,
                   LNFILE.LN_LMO_PRIOR_INTEREST,
                   LNFILE.LN_PRIOR_UNEARNED,
                   ISNULL(LNFILE.LN_ERNDELQ_STOP,''),
                   LNFILE.LN_NONACRERN,
                   LNFILE.LN_PRIOR_NONACRERN,
                   ISNULL(CAST(LNFILE.LN_ERN_IB_PDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_ERN_IB_PRINBAL,
                   LNFILE.LN_ERN_IB_INTOWE,
                   LNFILE.LN_TOTERN,
                   LNFILE.LN_TOTOTH,
                   LNFILE.LN_TOTINT,
                   LNFILE.LN_TOTLCHG,
                   LNFILE.LN_TOTDEF,
                   LNFILE.LN_TOTSERACCT,
                   LNFILE.LN_LCWAIVERS,
                   LNFILE.LN_INTWAIVERS,
                   LNFILE.LN_TOTPAYMNTD,
                   LNFILE.LN_TOTEXCPAYMNTD,
                   LNFILE.LN_TOTNODEF,
                   LNFILE.LN_YTDNODEF,
                   LNFILE.LN_TOTNODEFACT,
                   LNFILE.LN_YTDNODEFACT,
                   LNFILE.LN_TOTNOALDEL,
                   LNFILE.LN_YTDNOALDEL,
                   LNFILE.LN_YTDINTPD,
                   LNFILE.LN_LYRINTPD,
                   LNFILE.LN_TOTINTPD,
                   LNFILE.LN_RECASTINT,
                   LNFILE.LN_RECASTPRIN,
                   ISNULL(LNFILE.LN_CL_REBATE,''),
                   ISNULL(LNFILE.LN_AH_REBATE,''),
                   ISNULL(LNFILE.LN_PP_REBATE,''),
                   ISNULL(LNFILE.LN_INT_REBATE,''),
                   ISNULL(LNFILE.LN_SC_REBATE,''),
                   ISNULL(LNFILE.LN_MF_REBATE,''),
                   ISNULL(LNFILE.LN_DF_REBATE,''),
                   ISNULL(LNFILE.LN_EX_REBATE,''),
                   ISNULL(LNFILE.LN_O1_REBATE,''),
                   ISNULL(LNFILE.LN_O2_REBATE,''),
                   ISNULL(LNFILE.LN_O3_REBATE,''),
                   ISNULL(LNFILE.LN_O4_REBATE,''),
                   ISNULL(LNFILE.LN_O5_REBATE,''),
                   ISNULL(LNFILE.LN_O6_REBATE,''),
                   ISNULL(LNFILE.LN_O7_REBATE,''),
                   ISNULL(LNFILE.LN_UN_O7_REB,''),
                   ISNULL(LNFILE.LN_UN_O8_REB,''),
                   ISNULL(LNFILE.LN_UN_O9_REB,''),
                   ISNULL(LNFILE.LN_UN_O10_REB,''),
                   ISNULL(LNFILE.LN_PLREASON,''),
                   ISNULL(LNFILE.LN_COLLECTOR,''),
                   ISNULL(LNFILE.LN_ACTIONCD,''),
                   ISNULL(LNFILE.LN_ACTIONCD2,''),
                   ISNULL(LNFILE.LN_ATCODE,''),
                   ISNULL(LNFILE.LN_RECODE,''),
                   ISNULL(LNFILE.LN_ESCROW_FG,''),
                   LNFILE.LN_PROMDAYS_PASTDUE,
                   ISNULL(LNFILE.LN_PROMID,''),
                   ISNULL(CAST(LNFILE.LN_PROMDATE AS VARCHAR(10)),'0'),
                   LNFILE.LN_PROMAMT,
                   ISNULL(CAST(LNFILE.LN_PROMDATE2
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_DATE_PAID_LAST
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_AMT_PAID_LAST,
                   LNFILE.LN_LASTCFORMNO,
                   ISNULL(CAST(LNFILE.LN_LASTCFORMDATE
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_DELCNT,
                   LNFILE.LN_DELCNT2,
                   LNFILE.LN_CREDITLIM,
                   LNFILE.LN_CRSCORE,
                   LNFILE.LN_LASTSFORMNO,
                   ISNULL(CAST(LNFILE.LN_LASTSFORMDATE
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_SOLCNT,
                   ISNULL(LNFILE.LN_MESSAGE,''),
                   LNFILE.LN_OLDACCTNO,
                   LNFILE.LN_PRLNNO,
                   LNFILE.LN_PRBAL,
                   LNFILE.LN_PRINTDUE,
                   LNFILE.LN_PRFINCHGRB,
                   LNFILE.LN_PRINSRB,
                   ISNULL(LNFILE.LN_PRPOCD,''),
                   ISNULL(CAST(LNFILE.LN_PRPODATE AS VARCHAR(10)),'0'),
                   LNFILE.LN_PRDELINQAMT,
                   LNFILE.LN_PRLNAMT,
                   LNFILE.LN_PRPDUE_DAYS,
                   LNFILE.LN_PRDLDISC_UNEARN,
                   LNFILE.LN_PRCONPROFILE,
                   LNFILE.LN_SUM_ORIGPURAMT,
                   LNFILE.LN_SUM_LNAMT,
                   LNFILE.LN_SUM_FINCHG,
                   LNFILE.LN_SUM_INTCHG,
                   LNFILE.LN_SUM_ORGTERM,
                   LNFILE.LN_SUM_REGPYAMT,
                   LNFILE.LN_SUM_EXTCHG,
                   LNFILE.LN_SUM_SERCHG,
                   LNFILE.LN_SUM_MAINTFEE,
                   ISNULL(LNFILE.LN_UNITPER_CD,''),
                   LNFILE.LN_UNITPER_FREQ,
                   LNFILE.LN_DAYS_IN_UNITPER,
                   LNFILE.LN_UNITPER_PER_YEAR,
                   ISNULL(LNFILE.LN_REPOCD,''),
                   ISNULL(LNFILE.LN_BR_TFR_FG,''),
                   LNFILE.LN_O3INS_DLPROC,
                   LNFILE.LN_INSPREM_DLADJ,
                   LNFILE.LN_PLAMT,
                   LNFILE.LN_MTD_PRIN,
                   LNFILE.LN_MTD_LC,
                   LNFILE.LN_MTD_INT,
                   LNFILE.LN_MTD_OT,
                   LNFILE.LN_MTD_EX,
                   LNFILE.LN_BNKRPT_CHAPTER,
                   ISNULL(LNFILE.LN_BMO_CON_STATUS,''),
                   ISNULL(LNFILE.LN_BMO_REC_STATUS,''),
                   LNFILE.LN_APP_NUMBER,
                   ISNULL(LNFILE.LN_GIFTNET,''),
                   LNFILE.LN_GIFTNET_OUR_PO,
                   ISNULL(LNFILE.LN_PROC_DISBURSED,''),
                   ISNULL(LNFILE.LN_NOODDPY,''),
                   LNFILE.LN_CONPROFILE_1,
                   LNFILE.LN_CONPROFILE_2,
                   LNFILE.LN_CONPROFILE_3,
                   LNFILE.LN_CONPROFILE_4,
                   LNFILE.LN_CONPROFILE_5,
                   LNFILE.LN_RECPROFILE_1,
                   LNFILE.LN_RECPROFILE_2,
                   LNFILE.LN_RECPROFILE_3,
                   LNFILE.LN_RECPROFILE_4,
                   LNFILE.LN_RECPROFILE_5,
                   ISNULL(LNFILE.LN_TRW_STATUS,''),
                   ISNULL(LNFILE.LN_ORIGIN,''),
                   ISNULL(LNFILE.LN_PO_TRCD,''),
                   LNFILE.LN_NEXIS_CRSCORE,
                   ISNULL(LNFILE.LN_TRW_B_CONSUMER_IND,''),
                   ISNULL(LNFILE.LN_PARVHELD_PAID_FG,''),
                   ISNULL(LNFILE.LN_ACCT_FROZEN,''),
                   ISNULL(LNFILE.LN_TRW_SPECIAL_CODE,''),
                   ISNULL(LNFILE.LN_CREATION_STATUS,''),
                   LNFILE.LN_TOTNOLCHG,
                   ISNULL(LNFILE.LN_LBOX,''),
                   LNFILE.LN_EXTRA_PRIN_LFP,
                   LNFILE.LN_REMAIN_INT_SINCE_LFP,
                   LNFILE.LN_REMAIN_PRIN_SINCE_LFP,
                   ISNULL(LNFILE.LN_INTERIM_COUPON,''),
                   LNFILE.LN_DLCOMM_O3_CMSI,
                   LNFILE.LN_DLCOMM_PP_CMSI,
                   ISNULL(LNFILE.LN_TRW_J_CONSUMER_IND,''),
                   ISNULL(LNFILE.LN_ACH,''),
                   ISNULL(LNFILE.LN_1099_C_FG,''),
                   ISNULL(LNFILE.LN_TRW_COMPLIANCE_CODE,''),
                   ISNULL(LNFILE.LN_UNWIND_REASON_CODE,''),
                   ISNULL(LNFILE.LN_PBI_PBD_FG,''),
                   ISNULL(LNFILE.LN_OLD_ACCT_FORMAT_FG,''),
                   ISNULL(LNFILE.LN_APPROVED_BY_USER,''),
                   ISNULL(LNFILE.LN_APPROVED_BY_CODE,''),
                   ISNULL(CAST(LNFILE.LN_APPROVED_BY_DATE
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_APPROVED_BY_TIME,
                   ISNULL(CAST(LNFILE.LN_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_LTOUCH_TIME
            FROM DBO.LNFILE
            WHERE LNFILE.LN_OWNBR = :QLN1-WBEG-OWNBR
              AND LNFILE.LN_ACCTNO > :QLN1-WBEG-ACCTNO
              AND LNFILE.LN_ACCTNO <= :QLN1-WEND-ACCTNO 
            ORDER BY LNFILE.LN_OWNBR,
                     LNFILE.LN_ACCTNO
           END-EXEC.

           EXEC SQL
               OPEN LN1_CURSOR_GR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QLN1-WBEG-OWNBR
                          QLN1-WEND-OWNBR
                          QLN1-WBEG-ACCTNO
                          QLN1-WEND-ACCTNO.

           MOVE STAT---GOOD TO LN1-CURSOR-GR-STAT.

      *-----------------------------------------------------------------
       READ-LN1-FILE.
           PERFORM READ-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-SELECT-PATH TO E-FILE.
           MOVE LN1-KEY        TO E-KEYX.

           MOVE LN-OWNBR  TO QLN-OWNBR.
           MOVE LN-ACCTNO TO QLN-ACCTNO.

           EXEC SQL
            SELECT LNFILE.LN_OWNBR,
                   LNFILE.LN_ACCTNO,
                   LNFILE.LN_CLASS,
                   LNFILE.LN_SUBCLASS,
                   LNFILE.LN_LAWCODE,
                   LNFILE.LN_SSNO1,
                   ISNULL(LNFILE.LN_MAKERCD1,''),
                   LNFILE.LN_SSNO2,
                   ISNULL(LNFILE.LN_MAKERCD2,''),
                   LNFILE.LN_SSNO3,
                   ISNULL(LNFILE.LN_MAKERCD3,''),
                   LNFILE.LN_SSNO4,
                   ISNULL(LNFILE.LN_MAKERCD4,''),
                   ISNULL(LNFILE.LN_ORGST,''),
                   LNFILE.LN_ORGBR,
                   LNFILE.LN_SPRCLASS,
                   ISNULL(LNFILE.LN_POCD,''),
                   ISNULL(LNFILE.LN_SALEFIN,''),
                   ISNULL(LNFILE.LN_LOANTYPE,''),
                   ISNULL(LNFILE.LN_STATFLAGS,''),
                   ISNULL(CAST(LNFILE.LN_ENTDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_LOANDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_ORIG_1STPYDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_1STPYDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_POFFDATE AS VARCHAR(10)),'0'),
                   LNFILE.LN_INTCHG,
                   LNFILE.LN_SERCHG,
                   LNFILE.LN_EXTCHG,
                   LNFILE.LN_MAINTFEE,
                   LNFILE.LN_ORGTERM,
                   LNFILE.LN_1STPYAMT,
                   LNFILE.LN_NOREGPY,
                   LNFILE.LN_REGPYAMT,
                   LNFILE.LN_LASTPYAMT,
                   ISNULL(LNFILE.LN_FEECODE_1,''),
                   ISNULL(LNFILE.LN_FEECODE_2,''),
                   ISNULL(LNFILE.LN_FEECODE_3,''),
                   ISNULL(LNFILE.LN_FEECODE_4,''),
                   ISNULL(LNFILE.LN_FEECODE_5,''),
                   ISNULL(LNFILE.LN_FEECODE_6,''),
                   ISNULL(LNFILE.LN_FEECODE_7,''),
                   ISNULL(LNFILE.LN_FEECODE_8,''),
                   ISNULL(LNFILE.LN_FEECODE_9,''),
                   ISNULL(LNFILE.LN_FEECODE_10,''),
                   ISNULL(LNFILE.LN_FEECODE_11,''),
                   ISNULL(LNFILE.LN_FEECODE_12,''),
                   ISNULL(LNFILE.LN_FEECODE_13,''),
                   ISNULL(LNFILE.LN_FEECODE_14,''),
                   ISNULL(LNFILE.LN_FEECODE_15,''),
                   ISNULL(LNFILE.LN_FEECODE_16,''),
                   ISNULL(LNFILE.LN_FEECODE_17,''),
                   ISNULL(LNFILE.LN_FEECODE_18,''),
                   ISNULL(LNFILE.LN_FEECODE_19,''),
                   ISNULL(LNFILE.LN_FEECODE_20,''),
                   LNFILE.LN_FEEAMT_1,
                   LNFILE.LN_FEEAMT_2,
                   LNFILE.LN_FEEAMT_3,
                   LNFILE.LN_FEEAMT_4,
                   LNFILE.LN_FEEAMT_5,
                   LNFILE.LN_FEEAMT_6,
                   LNFILE.LN_FEEAMT_7,
                   LNFILE.LN_FEEAMT_8,
                   LNFILE.LN_FEEAMT_9,
                   LNFILE.LN_FEEAMT_10,
                   LNFILE.LN_FEEAMT_11,
                   LNFILE.LN_FEEAMT_12,
                   LNFILE.LN_FEEAMT_13,
                   LNFILE.LN_FEEAMT_14,
                   LNFILE.LN_FEEAMT_15,
                   LNFILE.LN_FEEAMT_16,
                   LNFILE.LN_FEEAMT_17,
                   LNFILE.LN_FEEAMT_18,
                   LNFILE.LN_FEEAMT_19,
                   LNFILE.LN_FEEAMT_20,
                   ISNULL(LNFILE.LN_FEEOURS_1,''),
                   ISNULL(LNFILE.LN_FEEOURS_2,''),
                   ISNULL(LNFILE.LN_FEEOURS_3,''),
                   ISNULL(LNFILE.LN_FEEOURS_4,''),
                   ISNULL(LNFILE.LN_FEEOURS_5,''),
                   ISNULL(LNFILE.LN_FEEOURS_6,''),
                   ISNULL(LNFILE.LN_FEEOURS_7,''),
                   ISNULL(LNFILE.LN_FEEOURS_8,''),
                   ISNULL(LNFILE.LN_FEEOURS_9,''),
                   ISNULL(LNFILE.LN_FEEOURS_10,''),
                   ISNULL(LNFILE.LN_FEEOURS_11,''),
                   ISNULL(LNFILE.LN_FEEOURS_12,''),
                   ISNULL(LNFILE.LN_FEEOURS_13,''),
                   ISNULL(LNFILE.LN_FEEOURS_14,''),
                   ISNULL(LNFILE.LN_FEEOURS_15,''),
                   ISNULL(LNFILE.LN_FEEOURS_16,''),
                   ISNULL(LNFILE.LN_FEEOURS_17,''),
                   ISNULL(LNFILE.LN_FEEOURS_18,''),
                   ISNULL(LNFILE.LN_FEEOURS_19,''),
                   ISNULL(LNFILE.LN_FEEOURS_20,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_1,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_2,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_3,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_4,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_5,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_6,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_7,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_8,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_9,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_10,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_11,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_12,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_13,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_14,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_15,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_16,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_17,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_18,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_19,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_20,''),
                   ISNULL(LNFILE.LN_INSURED,''),
                   ISNULL(LNFILE.LN_INSOURS_1,''),
                   ISNULL(LNFILE.LN_INSOURS_2,''),
                   ISNULL(LNFILE.LN_INSOURS_3,''),
                   ISNULL(LNFILE.LN_INSOURS_4,''),
                   ISNULL(LNFILE.LN_INSOURS_5,''),
                   ISNULL(LNFILE.LN_INSOURS_6,''),
                   ISNULL(LNFILE.LN_INSOURS_7,''),
                   ISNULL(LNFILE.LN_INSOURS_8,''),
                   ISNULL(LNFILE.LN_INSOURS_9,''),
                   ISNULL(LNFILE.LN_INSOURS_10,''),
                   LNFILE.LN_INSPREM_1,
                   LNFILE.LN_INSPREM_2,
                   LNFILE.LN_INSPREM_3,
                   LNFILE.LN_INSPREM_4,
                   LNFILE.LN_INSPREM_5,
                   LNFILE.LN_INSPREM_6,
                   LNFILE.LN_INSPREM_7,
                   LNFILE.LN_INSPREM_8,
                   LNFILE.LN_INSPREM_9,
                   LNFILE.LN_INSPREM_10,
                   ISNULL(LNFILE.LN_INSFINCHG_1,''),
                   ISNULL(LNFILE.LN_INSFINCHG_2,''),
                   ISNULL(LNFILE.LN_INSFINCHG_3,''),
                   ISNULL(LNFILE.LN_INSFINCHG_4,''),
                   ISNULL(LNFILE.LN_INSFINCHG_5,''),
                   ISNULL(LNFILE.LN_INSFINCHG_6,''),
                   ISNULL(LNFILE.LN_INSFINCHG_7,''),
                   ISNULL(LNFILE.LN_INSFINCHG_8,''),
                   ISNULL(LNFILE.LN_INSFINCHG_9,''),
                   ISNULL(LNFILE.LN_INSFINCHG_10,''),
                   LNFILE.LN_ANTICERN_1,
                   LNFILE.LN_ANTICERN_2,
                   LNFILE.LN_ANTICERN_3,
                   LNFILE.LN_ANTICERN_4,
                   LNFILE.LN_ANTICERN_5,
                   LNFILE.LN_ANTICERN_6,
                   LNFILE.LN_ANTICERN_7,
                   LNFILE.LN_ANTICERN_8,
                   LNFILE.LN_ANTICERN_9,
                   LNFILE.LN_ANTICERN_10,
                   LNFILE.LN_TOTNODEF,
                   LNFILE.LN_CREDITLIM,
                   LNFILE.LN_OLDACCTNO,
                   LNFILE.LN_SUM_ORGTERM,
                   ISNULL(LNFILE.LN_UNITPER_CD,''),
                   LNFILE.LN_UNITPER_FREQ
             INTO  
                   :QLN-OWNBR,
                   :QLN-ACCTNO,
                   :QLN-CLASS,
                   :QLN-SUBCLASS,
                   :QLN-LAWCODE,
                   :QLN-SSNO1,
                   :QLN-MAKERCD1,
                   :QLN-SSNO2,
                   :QLN-MAKERCD2,
                   :QLN-SSNO3,
                   :QLN-MAKERCD3,
                   :QLN-SSNO4,
                   :QLN-MAKERCD4,
                   :QLN-ORGST,
                   :QLN-ORGBR,
                   :QLN-SPRCLASS,
                   :QLN-POCD,
                   :QLN-SALEFIN,
                   :QLN-LOANTYPE,
                   :QLN-STATFLAGS,
                   :QLN-ENTDATE,
                   :QLN-LOANDATE,
                   :QLN-ORIG-1STPYDATE,
                   :QLN-1STPYDATE,
                   :QLN-POFFDATE,
                   :QLN-INTCHG,
                   :QLN-SERCHG,
                   :QLN-EXTCHG,
                   :QLN-MAINTFEE,
                   :QLN-ORGTERM,
                   :QLN-1STPYAMT,
                   :QLN-NOREGPY,
                   :QLN-REGPYAMT,
                   :QLN-LASTPYAMT,
                   :QLN-FEECODE-1, 
                   :QLN-FEECODE-2, 
                   :QLN-FEECODE-3, 
                   :QLN-FEECODE-4, 
                   :QLN-FEECODE-5, 
                   :QLN-FEECODE-6, 
                   :QLN-FEECODE-7, 
                   :QLN-FEECODE-8, 
                   :QLN-FEECODE-9, 
                   :QLN-FEECODE-10, 
                   :QLN-FEECODE-11, 
                   :QLN-FEECODE-12, 
                   :QLN-FEECODE-13, 
                   :QLN-FEECODE-14, 
                   :QLN-FEECODE-15, 
                   :QLN-FEECODE-16, 
                   :QLN-FEECODE-17, 
                   :QLN-FEECODE-18, 
                   :QLN-FEECODE-19, 
                   :QLN-FEECODE-20, 
                   :QLN-FEEAMT-1,
                   :QLN-FEEAMT-2,
                   :QLN-FEEAMT-3,
                   :QLN-FEEAMT-4,
                   :QLN-FEEAMT-5,
                   :QLN-FEEAMT-6,
                   :QLN-FEEAMT-7,
                   :QLN-FEEAMT-8,
                   :QLN-FEEAMT-9,
                   :QLN-FEEAMT-10,
                   :QLN-FEEAMT-11,
                   :QLN-FEEAMT-12,
                   :QLN-FEEAMT-13,
                   :QLN-FEEAMT-14,
                   :QLN-FEEAMT-15,
                   :QLN-FEEAMT-16,
                   :QLN-FEEAMT-17,
                   :QLN-FEEAMT-18,
                   :QLN-FEEAMT-19,
                   :QLN-FEEAMT-20,
                   :QLN-FEEOURS-1, 
                   :QLN-FEEOURS-2, 
                   :QLN-FEEOURS-3, 
                   :QLN-FEEOURS-4, 
                   :QLN-FEEOURS-5, 
                   :QLN-FEEOURS-6, 
                   :QLN-FEEOURS-7, 
                   :QLN-FEEOURS-8, 
                   :QLN-FEEOURS-9, 
                   :QLN-FEEOURS-10, 
                   :QLN-FEEOURS-11, 
                   :QLN-FEEOURS-12, 
                   :QLN-FEEOURS-13, 
                   :QLN-FEEOURS-14, 
                   :QLN-FEEOURS-15, 
                   :QLN-FEEOURS-16, 
                   :QLN-FEEOURS-17, 
                   :QLN-FEEOURS-18, 
                   :QLN-FEEOURS-19, 
                   :QLN-FEEOURS-20, 
                   :QLN-FEEFINCHG-1, 
                   :QLN-FEEFINCHG-2, 
                   :QLN-FEEFINCHG-3, 
                   :QLN-FEEFINCHG-4, 
                   :QLN-FEEFINCHG-5, 
                   :QLN-FEEFINCHG-6, 
                   :QLN-FEEFINCHG-7, 
                   :QLN-FEEFINCHG-8, 
                   :QLN-FEEFINCHG-9, 
                   :QLN-FEEFINCHG-10, 
                   :QLN-FEEFINCHG-11, 
                   :QLN-FEEFINCHG-12, 
                   :QLN-FEEFINCHG-13, 
                   :QLN-FEEFINCHG-14, 
                   :QLN-FEEFINCHG-15, 
                   :QLN-FEEFINCHG-16, 
                   :QLN-FEEFINCHG-17, 
                   :QLN-FEEFINCHG-18, 
                   :QLN-FEEFINCHG-19, 
                   :QLN-FEEFINCHG-20, 
                   :QLN-INSURED, 
                   :QLN-INSOURS-1, 
                   :QLN-INSOURS-2, 
                   :QLN-INSOURS-3, 
                   :QLN-INSOURS-4, 
                   :QLN-INSOURS-5, 
                   :QLN-INSOURS-6, 
                   :QLN-INSOURS-7, 
                   :QLN-INSOURS-8, 
                   :QLN-INSOURS-9, 
                   :QLN-INSOURS-10,
                   :QLN-INSPREM-1,
                   :QLN-INSPREM-2,
                   :QLN-INSPREM-3,
                   :QLN-INSPREM-4,
                   :QLN-INSPREM-5,
                   :QLN-INSPREM-6,
                   :QLN-INSPREM-7,
                   :QLN-INSPREM-8,
                   :QLN-INSPREM-9,
                   :QLN-INSPREM-10,
                   :QLN-INSFINCHG-1, 
                   :QLN-INSFINCHG-2, 
                   :QLN-INSFINCHG-3, 
                   :QLN-INSFINCHG-4, 
                   :QLN-INSFINCHG-5, 
                   :QLN-INSFINCHG-6, 
                   :QLN-INSFINCHG-7, 
                   :QLN-INSFINCHG-8, 
                   :QLN-INSFINCHG-9, 
                   :QLN-INSFINCHG-10,
                   :QLN-ANTICERN-1,
                   :QLN-ANTICERN-2,
                   :QLN-ANTICERN-3,
                   :QLN-ANTICERN-4,
                   :QLN-ANTICERN-5,
                   :QLN-ANTICERN-6,
                   :QLN-ANTICERN-7,
                   :QLN-ANTICERN-8,
                   :QLN-ANTICERN-9,
                   :QLN-ANTICERN-10,
                   :QLN-TOTNODEF,
                   :QLN-CREDITLIM,
                   :QLN-OLDACCTNO,
                   :QLN-SUM-ORGTERM,
                   :QLN-UNITPER-CD,
                   :QLN-UNITPER-FREQ
             FROM DBO.LNFILE
            WHERE LNFILE.LN_OWNBR  = :QLN-OWNBR
              AND LNFILE.LN_ACCTNO = :QLN-ACCTNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LN1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-LN-FIELDS.

      *-----------------------------------------------------------------
       READ-LN1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-SELECT-PATH TO E-FILE.
           MOVE LN1-KEY        TO E-KEYX.
           INITIALIZE QLN-REC.

           IF ( LN1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH LN1_CURSOR INTO :QLN-REC
              END-EXEC
           ELSE
           IF ( LN1-CURSOR-GR-STAT-GOOD )
              EXEC SQL
                   FETCH LN1_CURSOR_GR INTO :QLN-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LN1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-LN-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-LN1-FILE.

           PERFORM CLOSE-IT.
           IF ( LN1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE LN1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO LN1-CURSOR-STAT.

           IF ( LN1-CURSOR-GR-STAT-GOOD )
              EXEC SQL
                   CLOSE LN1_CURSOR_GR
              END-EXEC
              MOVE STAT---BAD TO LN1-CURSOR-GR-STAT.	  	  
	  
	  
      * COPY "LIBLP/LPBW1RN.CPY".
      * COPY "LIBLP/LPBW1RN.CPY".
      * COPYMEMBER: LIBLP/LPBW1RN
       LOAD-BW1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( BW-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( BW-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO BW-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO BW-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO BW-ALL-DATA
              MOVE BW-ALL-PATH             TO BW-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( BW-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO BW-ALL-BASE
              MOVE FILPATH-MACHINE         TO BW-ALL-MACHINE
              MOVE FILPATH-DATA            TO BW-ALL-DATA
              MOVE BW-ALL-PATH             TO BW-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-BWFILE NOT = "S" )
              MOVE BW-PATH-BEG             TO BW-SELECT-PATH-BEG
              MOVE BW-PATH-FILE            TO BW-SELECT-PATH-FILE
           ELSE
              MOVE BW-PATH-SQL             TO BW-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-BW1-FILE.
           PERFORM LOAD-BW1-FILE.
           PERFORM OPEN-IT.
           MOVE BW-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-BW1-FILE.
           PERFORM START-IT.
           MOVE BW-PATH-OWNBR  TO BW-OWNBR.
           MOVE BW-SELECT-PATH TO E-FILE.
           MOVE BW1-KEY        TO E-KEYX.

           IF ( BW1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-BW1-FILE.

           IF ( QBW1-WEND-SSNO = ZEROES    ) OR
              ( QBW1-WEND-SSNO NOT NUMERIC )
              MOVE BW-OWNBR             TO QBW1-WBEG-OWNBR
                                           QBW1-WEND-OWNBR
              MOVE BW-SSNO              TO QBW1-WBEG-SSNO
              MOVE ALL "9"              TO QBW1-WEND-SSNO.

           EXEC SQL
            DECLARE BW1_CURSOR CURSOR FOR
            SELECT BWFILE.BW_OWNBR,
                   BWFILE.BW_SSNO,
                   ISNULL(BWFILE.BW_LNAME,''),
                   ISNULL(BWFILE.BW_FNAME,''),
                   ISNULL(BWFILE.BW_MNAME,''),
                   ISNULL(BWFILE.BW_NICKNAME,''),
                   ISNULL(BWFILE.BW_GENERATION,''),
                   ISNULL(BWFILE.BW_ADR1,''),
                   ISNULL(BWFILE.BW_ADR2,''),
                   ISNULL(BWFILE.BW_CITY,''),
                   ISNULL(BWFILE.BW_STATE,''),
                   ISNULL(BWFILE.BW_ZIP,''),
                   ISNULL(CAST(BWFILE.BW_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(BWFILE.BW_REFDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(BWFILE.BW_BDATE AS VARCHAR(10)),'0'),
                   BWFILE.BW_PHONE,
                   BWFILE.BW_CELL_PHONE,
                   ISNULL(BWFILE.BW_NATIONALITY,''),
                   ISNULL(BWFILE.BW_GENDER,''),
                   BWFILE.BW_DEPEND,
                   ISNULL(BWFILE.BW_RENTCD,''),
                   ISNULL(BWFILE.BW_TIMEADDR,''),
                   ISNULL(BWFILE.BW_CREDITCD,''),
                   ISNULL(BWFILE.BW_STRTGCD,''),
                   ISNULL(BWFILE.BW_DWELLCD,''),
                   ISNULL(BWFILE.BW_COUNTY,''),
                   ISNULL(BWFILE.BW_ORIGIN,''),
                   ISNULL(BWFILE.BW_CENSUSCD,''),
                   BWFILE.BW_ORGSLSMN,
                   ISNULL(BWFILE.BW_REPCODE,''),
                   BWFILE.BW_SPSSNO,
                   ISNULL(BWFILE.BW_SPLNAME,''),
                   ISNULL(BWFILE.BW_SPFNAME,''),
                   ISNULL(CAST(BWFILE.BW_SP_BDATE AS VARCHAR(10)),'0'),
                   ISNULL(BWFILE.BW_EMPLOYER,''),
                   ISNULL(BWFILE.BW_EMPLPOS,''),
                   BWFILE.BW_EMPPHONE,
                   BWFILE.BW_EMPEXT,
                   BWFILE.BW_EMPYEARS,
                   BWFILE.BW_EMPPAY,
                   BWFILE.BW_EMPPAYOT,
                   ISNULL(BWFILE.BW_EMPPAYSR,''),
                   ISNULL(BWFILE.BW_EMPADDR1,''),
                   ISNULL(BWFILE.BW_EMPADDR2,''),
                   ISNULL(BWFILE.BW_EMPCITY,''),
                   ISNULL(BWFILE.BW_EMPSTATE,''),
                   ISNULL(BWFILE.BW_EMPZIP,''),
                   ISNULL(BWFILE.BW_WORKHRS,''),
                   BWFILE.BW_LOANS,
                   BWFILE.BW_INTYTD,
                   BWFILE.BW_INTLYR,
                   BWFILE.BW_LIASMLN,
                   BWFILE.BW_LIAOTH,
                   ISNULL(CAST(BWFILE.BW_LASTPODATE
                                                  AS VARCHAR(10)),'0'),
                   BWFILE.BW_PRLOANS,
                   BWFILE.BW_RE_INTYTD,
                   BWFILE.BW_RE_INTLYR,
                   BWFILE.BW_CREDIT_LIMIT,
                   BWFILE.BW_CREDIT_SCORE,
                   BWFILE.BW_CRED_SCORE_1,
                   BWFILE.BW_CRED_SCORE_2,
                   BWFILE.BW_CRED_SCORE_3,
                   BWFILE.BW_CRED_SCORE_4,
                   BWFILE.BW_BNKRPT_SCORE_1,
                   BWFILE.BW_BNKRPT_SCORE_2,
                   BWFILE.BW_BNKRPT_SCORE_3,
                   BWFILE.BW_BNKRPT_SCORE_4,
                   BWFILE.BW_LAST_FORM_NO,
                   ISNULL(CAST(BWFILE.BW_LAST_FORM_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(BWFILE.BW_PRIOR_RN_FOR10_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(BWFILE.BW_LAST_RN_FOR10_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(BWFILE.BW_ADR_PHONE_CHG_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(BWFILE.BW_BAD_ADR_FG,''),
                   ISNULL(BWFILE.BW_BAD_PHONE_FG,''),
                   ISNULL(BWFILE.BW_BAD_EMPPHONE_FG,''),
                   ISNULL(BWFILE.BW_BAD_MISC_PHONE1_FG,''),
                   ISNULL(BWFILE.BW_BAD_MISC_PHONE2_FG,''),
                   ISNULL(BWFILE.BW_BAD_CELL_FG,''),
                   BWFILE.BW_MISC_PHONE_1,
                   BWFILE.BW_MISC_EXT_1,
                   ISNULL(BWFILE.BW_MISC_PHONE_CD_1,''),
                   BWFILE.BW_MISC_PHONE_2,
                   BWFILE.BW_MISC_EXT_2,
                   ISNULL(BWFILE.BW_MISC_PHONE_CD_2,''),
                   BWFILE.BW_MISC_PHONE_3,
                   BWFILE.BW_MISC_EXT_3,
                   ISNULL(BWFILE.BW_MISC_PHONE_CD_3,''),
                   BWFILE.BW_MISC_PHONE_4,
                   BWFILE.BW_MISC_EXT_4,
                   ISNULL(BWFILE.BW_MISC_PHONE_CD_4,''),
                   BWFILE.BW_MISC_PHONE_5,
                   BWFILE.BW_MISC_EXT_5,
                   ISNULL(BWFILE.BW_MISC_PHONE_CD_5,''),
                   BWFILE.BW_MISC_PHONE_6,
                   BWFILE.BW_MISC_EXT_6,
                   ISNULL(BWFILE.BW_MISC_PHONE_CD_6,''),
                   ISNULL(BWFILE.BW_CONTACT_DESC_1,''),
                   BWFILE.BW_CONTACT_PHONE_1,
                   BWFILE.BW_CONTACT_EXT_1,
                   ISNULL(BWFILE.BW_CONTACT_RELATION_1,''),
                   ISNULL(BWFILE.BW_CONTACT_DESC_2,''),
                   BWFILE.BW_CONTACT_PHONE_2,
                   BWFILE.BW_CONTACT_EXT_2,
                   ISNULL(BWFILE.BW_CONTACT_RELATION_2,''),
                   ISNULL(BWFILE.BW_CONTACT_DESC_3,''),
                   BWFILE.BW_CONTACT_PHONE_3,
                   BWFILE.BW_CONTACT_EXT_3,
                   ISNULL(BWFILE.BW_CONTACT_RELATION_3,''),
                   ISNULL(BWFILE.BW_CONTACT_DESC_4,''),
                   BWFILE.BW_CONTACT_PHONE_4,
                   BWFILE.BW_CONTACT_EXT_4,
                   ISNULL(BWFILE.BW_CONTACT_RELATION_4,''),
                   ISNULL(BWFILE.BW_CONTACT_DESC_5,''),
                   BWFILE.BW_CONTACT_PHONE_5,
                   BWFILE.BW_CONTACT_EXT_5,
                   ISNULL(BWFILE.BW_CONTACT_RELATION_5,''),
                   ISNULL(BWFILE.BW_CONTACT_DESC_6,''),
                   BWFILE.BW_CONTACT_PHONE_6,
                   BWFILE.BW_CONTACT_EXT_6,
                   ISNULL(BWFILE.BW_CONTACT_RELATION_6,''),
                   ISNULL(BWFILE.BW_CONTACT_DESC_7,''),
                   BWFILE.BW_CONTACT_PHONE_7,
                   BWFILE.BW_CONTACT_EXT_7,
                   ISNULL(BWFILE.BW_CONTACT_RELATION_7,''),
                   ISNULL(BWFILE.BW_CONTACT_DESC_8,''),
                   BWFILE.BW_CONTACT_PHONE_8,
                   BWFILE.BW_CONTACT_EXT_8,
                   ISNULL(BWFILE.BW_CONTACT_RELATION_8,''),
                   ISNULL(BWFILE.BW_CONTACT_DESC_9,''),
                   BWFILE.BW_CONTACT_PHONE_9,
                   BWFILE.BW_CONTACT_EXT_9,
                   ISNULL(BWFILE.BW_CONTACT_RELATION_9,''),
                   ISNULL(BWFILE.BW_CONTACT_DESC_10,''),
                   BWFILE.BW_CONTACT_PHONE_10,
                   BWFILE.BW_CONTACT_EXT_10,
                   ISNULL(BWFILE.BW_CONTACT_RELATION_10,''),
                   ISNULL(BWFILE.BW_SRCD,''),
                   ISNULL(BWFILE.BW_CALL_ON_SUNDAY,''),
                   ISNULL(BWFILE.BW_SOLICIT,''),
                   ISNULL(BWFILE.BW_CONFIDENTIAL,''),
                   ISNULL(BWFILE.BW_PRIVACY_OPT_OUT,''),
                   ISNULL(BWFILE.BW_OPT_IN_EMAIL,''),
                   ISNULL(BWFILE.BW_OPT_IN_TEXT,''),
                   ISNULL(BWFILE.BW_CITY_TAX_CD,''),
                   ISNULL(CAST(BWFILE.BW_DEALTRACK_CHG_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(BWFILE.BW_DECEASED,''),
                   ISNULL(CAST(BWFILE.BW_DECEASED_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(BWFILE.BW_EMAIL,''),
                   ISNULL(BWFILE.BW_IDENT_NO,''),
                   ISNULL(BWFILE.BW_IDENT_TYPE,''),
                   ISNULL(BWFILE.BW_IDENT_STATE,''),
                   ISNULL(BWFILE.BW_MILITARY_FLAG,''),
                   BWFILE.BW_ORGBR
            FROM DBO.BWFILE
            WHERE BWFILE.BW_OWNBR >= :QBW1-WBEG-OWNBR
              AND BWFILE.BW_OWNBR <= :QBW1-WEND-OWNBR
              AND BWFILE.BW_SSNO >= :QBW1-WBEG-SSNO
              AND BWFILE.BW_SSNO <= :QBW1-WEND-SSNO
              ORDER BY BWFILE.BW_OWNBR,
                       BWFILE.BW_SSNO
           END-EXEC.

           EXEC SQL
               OPEN BW1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QBW1-WBEG-OWNBR 
                          QBW1-WEND-OWNBR
                          QBW1-WBEG-SSNO
                          QBW1-WEND-SSNO.

           MOVE STAT---GOOD TO BW1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-BW1-FILE.
           PERFORM READ-IT.
           MOVE BW-PATH-OWNBR  TO BW-OWNBR. 
           MOVE BW-SELECT-PATH TO E-FILE.
           MOVE BW1-KEY        TO E-KEYX.
		   INITIALIZE QBW-REC.

           MOVE BW-OWNBR   TO QBW-OWNBR.
           MOVE BW-SSNO    TO QBW-SSNO.

           EXEC SQL
             SELECT BWFILE.BW_OWNBR,
                    BWFILE.BW_SSNO,
                    ISNULL(BWFILE.BW_LNAME,''),
                    ISNULL(BWFILE.BW_FNAME,''),
                    ISNULL(BWFILE.BW_SOLICIT,'')
               INTO 
                    :QBW-OWNBR,
                    :QBW-SSNO,
                    :QBW-LNAME,
                    :QBW-FNAME,
                    :QBW-SOLICIT
            FROM DBO.BWFILE
            WHERE BWFILE.BW_OWNBR = :QBW-OWNBR
              AND BWFILE.BW_SSNO = :QBW-SSNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-BW1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-BW-FIELDS.

      *-----------------------------------------------------------------
       READ-BW1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE BW-PATH-OWNBR  TO BW-OWNBR.
           MOVE BW-SELECT-PATH TO E-FILE.
           MOVE BW1-KEY        TO E-KEYX.
           INITIALIZE QBW-REC.

           IF ( BW1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH BW1_CURSOR INTO :QBW-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-BW1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-BW-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-BW1-FILE.

           PERFORM CLOSE-IT.
           IF ( BW1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE BW1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO BW1-CURSOR-STAT.	


      * COPY "LIBLP/LPCDB1RN.CPY".
      * COPYMEMBER: LIBLP/LPCDB1RN
       LOAD-CDB1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( CDB-PATH-OVERRIDE = "Y" )
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( CDB-PATH-OVERRIDE = SPACES )
              MOVE EXT-FILPATH-BASE        TO CDB-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO CDB-ALL-MACHINE
              MOVE CDB-ALL-PATH             TO CDB-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( CDB-PATH-OVERRIDE = "B" )
              MOVE FILPATH-BASE            TO CDB-ALL-BASE
              MOVE FILPATH-MACHINE         TO CDB-ALL-MACHINE
              MOVE CDB-ALL-PATH             TO CDB-PATH.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-CDBFILE NOT = "S" )
              MOVE CDB-PATH-BEG             TO CDB-SELECT-PATH-BEG
              MOVE CDB-PATH-FILE            TO CDB-SELECT-PATH-FILE
           ELSE
              MOVE CDB-PATH-SQL             TO CDB-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-CDB1-FILE.
           PERFORM LOAD-CDB1-FILE.
           PERFORM OPEN-IT.
           MOVE CDB-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-CDB1-FILE.
           PERFORM START-IT.
           MOVE CDB-SELECT-PATH TO E-FILE.
           MOVE CDB1-KEY TO E-KEYX.

           IF ( CDB1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-CDB1-FILE.

           IF ( QCDB1-WEND-BRANCH = ZEROES    )  OR
              ( QCDB1-WEND-TYPE = SPACES )
              MOVE CDB-TYPE             TO QCDB1-WBEG-TYPE
              MOVE ALL "Z"              TO QCDB1-WEND-TYPE
              MOVE CDB-BRANCH           TO QCDB1-WBEG-BRANCH
              MOVE ALL "9"              TO QCDB1-WEND-BRANCH
              MOVE CDB-CODE             TO QCDB1-WBEG-CODE
              MOVE ALL "Z"              TO QCDB1-WEND-CODE.

           EXEC SQL
            DECLARE CDB1_CURSOR CURSOR FOR
              SELECT ISNULL(CDBFILE.CDB_TYPE,''),
                     CDBFILE.CDB_BRANCH,
                     ISNULL(CDBFILE.CDB_CODE,''),
                     ISNULL(CDBFILE.CDB_DESC,'')
                FROM DBO.CDBFILE
               WHERE CDBFILE.CDB_TYPE   >= :QCDB1-WBEG-TYPE
                 AND CDBFILE.CDB_TYPE   <= :QCDB1-WEND-TYPE
                 AND CDBFILE.CDB_BRANCH >= :QCDB1-WBEG-BRANCH
                 AND CDBFILE.CDB_BRANCH <= :QCDB1-WEND-BRANCH
                 AND CDBFILE.CDB_CODE   >= :QCDB1-WBEG-CODE
                 AND CDBFILE.CDB_CODE   <= :QCDB1-WEND-CODE
               ORDER BY CDBFILE.CDB_TYPE,
                        CDBFILE.CDB_BRANCH,
                        CDBFILE.CDB_CODE
           END-EXEC.

           EXEC SQL
               OPEN CDB1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE SPACES TO QCDB1-WBEG-TYPE
                          QCDB1-WEND-TYPE
                          QCDB1-WBEG-CODE
                          QCDB1-WEND-CODE.

           MOVE ZEROES TO QCDB1-WBEG-BRANCH
                          QCDB1-WEND-BRANCH.

           MOVE STAT---GOOD TO CDB1-CURSOR-STAT.

      *-----------------------------------------------------------------
      * 5/25/2023 - DUE TO THE AMOUNT OF TIME TO IT TOOK TO PERFORM
      *     READ-CDB1-FILE-NEXT, THIS PARAGRAPH IS BEING USED INSTEAD.
      *     IT HAS BEEN MODIFIED TO FIND ANY OCCURANCE OF THE FEE-CODE,
      *     REGARDLESS OF THE BRANCH, TO VERIFY THE FEE-CODE ENTERED IS 
      *     VALID.
      *-----------------------------------------------------------------

       READ-CDB1-FILE.
           PERFORM READ-IT.
           MOVE CDB-SELECT-PATH TO E-FILE.
           MOVE CDB1-KEY TO E-KEYX.

           MOVE CDB-TYPE    TO QCDB-TYPE. 
           MOVE CDB-BRANCH  TO QCDB-BRANCH.
           MOVE CDB-CODE    TO QCDB-CODE.

           EXEC SQL
            SELECT ISNULL(CDBFILE.CDB_TYPE,''),
                   MIN(CDBFILE.CDB_BRANCH),
                   ISNULL(CDBFILE.CDB_CODE,''),
                   MIN(ISNULL(CDBFILE.CDB_DESC,''))  
            INTO 
                  :QCDB-TYPE,
                  :QCDB-BRANCH,
                  :QCDB-CODE,
                  :QCDB-DESC
            FROM DBO.CDBFILE
            WHERE CDBFILE.CDB_TYPE   = :QCDB-TYPE
              AND CDBFILE.CDB_CODE   = :QCDB-CODE
            GROUP BY CDBFILE.CDB_TYPE,
                     CDBFILE.CDB_CODE 
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-CDB1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-CDB-FIELDS.

      *-----------------------------------------------------------------
       READ-CDB1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE CDB-SELECT-PATH TO E-FILE.
           MOVE CDB1-KEY TO E-KEYX.
           INITIALIZE QCDB-REC.

           IF ( CDB1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH CDB1_CURSOR INTO 
                     :QCDB-TYPE,
                     :QCDB-BRANCH,
                     :QCDB-CODE,
                     :QCDB-DESC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-CDB1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-CDB-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-CDB1-FILE.
           PERFORM CLOSE-IT.
           IF ( CDB1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE CDB1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO CDB1-CURSOR-STAT.




      * COPY "LIBLP/LPSP1RN.CPY".
      * COPYMEMBER: LIBLP/LPSP1RN
      ******************************************************************
       SET-ALL-TO-SP1.
           MOVE SPA1-KEY   TO SP1-KEY.
           MOVE SPA-AREA-A TO SP-AREA-A.
           MOVE SPB-AREA-B TO SP-AREA-B.
           MOVE SPC-AREA-C TO SP-AREA-C.

       SET-SP1-TO-ALL.
           MOVE SP1-KEY   TO SPA1-KEY.
           MOVE SP-AREA-A TO SPA-AREA-A.
           MOVE SP1-KEY   TO SPB1-KEY.
           MOVE SP-AREA-B TO SPB-AREA-B.
           MOVE SP1-KEY   TO SPC1-KEY.
           MOVE SP-AREA-C TO SPC-AREA-C.

       LOAD-SP1-FILE.
           PERFORM LOAD-SPA1-FILE.
           PERFORM LOAD-SPB1-FILE.
           PERFORM LOAD-SPC1-FILE.

      *-----------------------------------------------------------------
       OPEN-SP1-FILE.
           PERFORM OPEN-SPA1-FILE.
           IF ( IO-FG = 0 )
               MOVE " " TO IO-MULTI-FG
           ELSE
               MOVE "A" TO IO-MULTI-FG.
			   
      *         PERFORM OPEN-SPB1-FILE
      *         IF ( IO-FG = 0 )
      *            PERFORM OPEN-SPC1-FILE
      *            IF ( IO-FG = 0 )
      *               MOVE " " TO IO-MULTI-FG
      *            ELSE
      *               MOVE "C" TO IO-MULTI-FG
      *         ELSE
      *            MOVE "B" TO IO-MULTI-FG
      *      ELSE
      *         MOVE "A" TO IO-MULTI-FG.

      *-----------------------------------------------------------------
       START-SP1-FILE.
           PERFORM SET-SP1-TO-ALL.
           PERFORM START-SPA1-FILE.
           IF ( IO-FG NOT = 0 )
              MOVE "A" TO IO-MULTI-FG.
             
      *-----------------------------------------------------------------
       READ-SP1-FILE.
           PERFORM SET-SP1-TO-ALL.
           PERFORM READ-SPA1-FILE.
           IF ( IO-FG = 0 )
               PERFORM SET-ALL-TO-SP1
           ELSE
               MOVE "A" TO IO-MULTI-FG.
		   
	
      *        PERFORM READ-SPB1-FILE
      *        IF ( IO-FG = 0 )
      *           PERFORM READ-SPC1-FILE
      *           IF ( IO-FG = 0 )
      *              PERFORM SET-ALL-TO-SP1
      *           ELSE
      *              MOVE "C" TO IO-MULTI-FG
      *        ELSE
      *           MOVE "B" TO IO-MULTI-FG
      *     ELSE
      *        MOVE "A" TO IO-MULTI-FG.

      *-----------------------------------------------------------------
       READ-SP1-FILE-NEXT.
           PERFORM SET-SP1-TO-ALL.
           PERFORM READ-SPA1-FILE-NEXT.
           IF ( IO-FG = 0 )
              MOVE SPA1-KEY TO SPB1-KEY
              PERFORM READ-SPB1-FILE
              IF ( IO-FG = 0 )
                 MOVE SPA1-KEY TO SPC1-KEY
                 PERFORM READ-SPC1-FILE
                 IF ( IO-FG = 0 )
                    PERFORM SET-ALL-TO-SP1
                 ELSE
                    MOVE "C" TO IO-MULTI-FG
              ELSE
                 MOVE "B" TO IO-MULTI-FG
           ELSE
              MOVE "A" TO IO-MULTI-FG.

      *-----------------------------------------------------------------
       CLOSE-SP1-FILE.
           PERFORM CLOSE-SPA1-FILE.
      *     PERFORM CLOSE-SPB1-FILE.
      *     PERFORM CLOSE-SPC1-FILE.

      *================================================================E
       LOAD-SPA1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( SP-PATH-OVERRIDE = "Y" )
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( SP-PATH-OVERRIDE = SPACES )
              MOVE EXT-FILPATH-BASE        TO SPA-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO SPA-ALL-MACHINE
              MOVE SPA-ALL-PATH             TO SPA-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( SP-PATH-OVERRIDE = "B" )
              MOVE FILPATH-BASE            TO SPA-ALL-BASE
              MOVE FILPATH-MACHINE         TO SPA-ALL-MACHINE
              MOVE SPA-ALL-PATH             TO SPA-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-SPAFILE NOT = "S" )
              MOVE SPA-PATH-BEG          TO SPA-SELECT-PATH-BEG
              MOVE SPA-PATH-FILE         TO SPA-SELECT-PATH-FILE
           ELSE
              MOVE SPA-PATH-SQL          TO SPA-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-SPA1-FILE.
           PERFORM LOAD-SPA1-FILE.
           PERFORM OPEN-IT.
           MOVE SPA-SELECT-PATH TO E-FILE.
      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-SPA1-FILE.
           PERFORM START-IT.
           MOVE SPA-SELECT-PATH TO E-FILE.
           MOVE SPA1-KEY TO E-KEYX.

           IF ( SPA1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-SPA1-FILE.

           IF ( QSP1-WEND-LAWCODE = 0  ) OR
              ( QSP1-WEND-LAWCODE NOT NUMERIC)
              MOVE SPA-ORGST            TO QSP1-WBEG-ORGST 
              MOVE ALL "Z"              TO QSP1-WEND-ORGST 
              MOVE SPA-SPRCLASS         TO QSP1-WBEG-SPRCLASS
              MOVE ALL "9"              TO QSP1-WEND-SPRCLASS
              MOVE SPA-SUBCLASS         TO QSP1-WBEG-SUBCLASS
              MOVE ALL "9"              TO QSP1-WEND-SUBCLASS
              MOVE SPA-LAWCODE          TO QSP1-WBEG-LAWCODE
              MOVE ALL "9"              TO QSP1-WEND-LAWCODE.

           EXEC SQL
            DECLARE SPA1_CURSOR CURSOR FOR
            SELECT ISNULL(SPAFILE.SPA_ORGST,''),
                   SPAFILE.SPA_SPRCLASS,
                   SPAFILE.SPA_SUBCLASS,
                   SPAFILE.SPA_LAWCODE,
                   ISNULL(CAST(SPAFILE.SPA_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(SPAFILE.SPA_DESC,''),
                   ISNULL(SPAFILE.SPA_NOTES,''),
                   ISNULL(CAST(SPAFILE.SPA_EFFDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(SPAFILE.SPA_EXPDATE
                                                  AS VARCHAR(10)),'0'),
                   SPAFILE.SPA_EXPD_MONTHS,
                   SPAFILE.SPA_EXPD_DAYS,
                   ISNULL(SPAFILE.SPA_EXPD_FRMLA,''),
                   ISNULL(SPAFILE.SPA_EXPD_EXCEPT,''),
                   ISNULL(SPAFILE.SPA_EXPR_FRMLA,''),
                   ISNULL(SPAFILE.SPA_EXPR_REDU_FRMLA,''),
                   SPAFILE.SPA_EXPR_RATE_1,
                   SPAFILE.SPA_EXPR_BREAK_1,
                   SPAFILE.SPA_EXPR_YRTYPE_1,
                   ISNULL(SPAFILE.SPA_EXPR_INTMETH_1,''),
                   SPAFILE.SPA_EXPR_RATE_2,
                   SPAFILE.SPA_EXPR_BREAK_2,
                   SPAFILE.SPA_EXPR_YRTYPE_2,
                   ISNULL(SPAFILE.SPA_EXPR_INTMETH_2,''),
                   SPAFILE.SPA_EXPR_RATE_3,
                   SPAFILE.SPA_EXPR_BREAK_3,
                   SPAFILE.SPA_EXPR_YRTYPE_3,
                   ISNULL(SPAFILE.SPA_EXPR_INTMETH_3,''),
                   SPAFILE.SPA_EXPR_RATE_4,
                   SPAFILE.SPA_EXPR_BREAK_4,
                   SPAFILE.SPA_EXPR_YRTYPE_4,
                   ISNULL(SPAFILE.SPA_EXPR_INTMETH_4,''),
                   SPAFILE.SPA_EXPR_RATE_5,
                   SPAFILE.SPA_EXPR_BREAK_5,
                   SPAFILE.SPA_EXPR_YRTYPE_5,
                   ISNULL(SPAFILE.SPA_EXPR_INTMETH_5,''),
                   ISNULL(SPAFILE.SPA_EXPR_LCHG,''),
                   ISNULL(SPAFILE.SPA_JUDGFRMLA,''),
                   SPAFILE.SPA_JUDGRATE,
                   SPAFILE.SPA_JUDGYRTYPE,
                   ISNULL(SPAFILE.SPA_JUDGINTMETH,''),
                   ISNULL(SPAFILE.SPA_ACC_FRMLA,''),
                   SPAFILE.SPA_ACC_YRTYPE,
                   ISNULL(SPAFILE.SPA_ACC_INTMETH,''),
                   SPAFILE.SPA_ACC_RATE_1,
                   SPAFILE.SPA_ACC_BREAK_1,
                   SPAFILE.SPA_ACC_RATE_2,
                   SPAFILE.SPA_ACC_BREAK_2,
                   SPAFILE.SPA_ACC_RATE_3,
                   SPAFILE.SPA_ACC_BREAK_3,
                   SPAFILE.SPA_ACC_RATE_4,
                   SPAFILE.SPA_ACC_BREAK_4,
                   SPAFILE.SPA_ACC_RATE_5,
                   SPAFILE.SPA_ACC_BREAK_5,
                   ISNULL(SPAFILE.SPA_1098FRMLA,''),
                   ISNULL(SPAFILE.SPA_DISFRMLA,''),
                   ISNULL(SPAFILE.SPA_CONTRFRMLA,''),
                   ISNULL(SPAFILE.SPA_BANKRULE,''),
                   SPAFILE.SPA_CONTPDTH,
                   SPAFILE.SPA_FINCHG,
                   ISNULL(SPAFILE.SPA_BANKUNITPER_CD,''),
                   ISNULL(SPAFILE.SPA_TRWBANKRULE,''),
                   SPAFILE.SPA_TRWCONTPDTH,
                   ISNULL(SPAFILE.SPA_TRWBANKUNITPER_CD,''),
                   ISNULL(SPAFILE.SPA_RECFRMLA,''),
                   ISNULL(SPAFILE.SPA_RECPAYCD,''),
                   SPAFILE.SPA_RECPDTH,
                   ISNULL(SPAFILE.SPA_DISFRMLA_SPAY_FG,''),
                   SPAFILE.SPA_ERNFRMLA_1,
                   SPAFILE.SPA_ERNAMT_1,
                   SPAFILE.SPA_ERNPERCT_1,
                   ISNULL(SPAFILE.SPA_ERNPERCD_1,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_FG_1,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_START_CD_1,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_DFCD_1,''),
                   SPAFILE.SPA_ERN_DAYS_1,
                   SPAFILE.SPA_ERN_TERM_1,
                   ISNULL(SPAFILE.SPA_ERN_TYPE_1,''),
                   SPAFILE.SPA_ERNFRMLA_2,
                   SPAFILE.SPA_ERNAMT_2,
                   SPAFILE.SPA_ERNPERCT_2,
                   ISNULL(SPAFILE.SPA_ERNPERCD_2,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_FG_2,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_START_CD_2,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_DFCD_2,''),
                   SPAFILE.SPA_ERN_DAYS_2,
                   SPAFILE.SPA_ERN_TERM_2,
                   ISNULL(SPAFILE.SPA_ERN_TYPE_2,''),
                   SPAFILE.SPA_ERNFRMLA_3,
                   SPAFILE.SPA_ERNAMT_3,
                   SPAFILE.SPA_ERNPERCT_3,
                   ISNULL(SPAFILE.SPA_ERNPERCD_3,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_FG_3,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_START_CD_3,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_DFCD_3,''),
                   SPAFILE.SPA_ERN_DAYS_3,
                   SPAFILE.SPA_ERN_TERM_3,
                   ISNULL(SPAFILE.SPA_ERN_TYPE_3,''),
                   SPAFILE.SPA_ERNFRMLA_4,
                   SPAFILE.SPA_ERNAMT_4,
                   SPAFILE.SPA_ERNPERCT_4,
                   ISNULL(SPAFILE.SPA_ERNPERCD_4,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_FG_4,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_START_CD_4,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_DFCD_4,''),
                   SPAFILE.SPA_ERN_DAYS_4,
                   SPAFILE.SPA_ERN_TERM_4,
                   ISNULL(SPAFILE.SPA_ERN_TYPE_4,''),
                   SPAFILE.SPA_ERNFRMLA_5,
                   SPAFILE.SPA_ERNAMT_5,
                   SPAFILE.SPA_ERNPERCT_5,
                   ISNULL(SPAFILE.SPA_ERNPERCD_5,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_FG_5,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_START_CD_5,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_DFCD_5,''),
                   SPAFILE.SPA_ERN_DAYS_5,
                   SPAFILE.SPA_ERN_TERM_5,
                   ISNULL(SPAFILE.SPA_ERN_TYPE_5,''),
                   SPAFILE.SPA_ERNFRMLA_6,
                   SPAFILE.SPA_ERNAMT_6,
                   SPAFILE.SPA_ERNPERCT_6,
                   ISNULL(SPAFILE.SPA_ERNPERCD_6,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_FG_6,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_START_CD_6,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_DFCD_6,''),
                   SPAFILE.SPA_ERN_DAYS_6,
                   SPAFILE.SPA_ERN_TERM_6,
                   ISNULL(SPAFILE.SPA_ERN_TYPE_6,''),
                   SPAFILE.SPA_ERNFRMLA_7,
                   SPAFILE.SPA_ERNAMT_7,
                   SPAFILE.SPA_ERNPERCT_7,
                   ISNULL(SPAFILE.SPA_ERNPERCD_7,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_FG_7,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_START_CD_7,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_DFCD_7,''),
                   SPAFILE.SPA_ERN_DAYS_7,
                   SPAFILE.SPA_ERN_TERM_7,
                   ISNULL(SPAFILE.SPA_ERN_TYPE_7,''),
                   SPAFILE.SPA_ERNFRMLA_8,
                   SPAFILE.SPA_ERNAMT_8,
                   SPAFILE.SPA_ERNPERCT_8,
                   ISNULL(SPAFILE.SPA_ERNPERCD_8,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_FG_8,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_START_CD_8,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_DFCD_8,''),
                   SPAFILE.SPA_ERN_DAYS_8,
                   SPAFILE.SPA_ERN_TERM_8,
                   ISNULL(SPAFILE.SPA_ERN_TYPE_8,''),
                   SPAFILE.SPA_ERNFRMLA_9,
                   SPAFILE.SPA_ERNAMT_9,
                   SPAFILE.SPA_ERNPERCT_9,
                   ISNULL(SPAFILE.SPA_ERNPERCD_9,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_FG_9,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_START_CD_9,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_DFCD_9,''),
                   SPAFILE.SPA_ERN_DAYS_9,
                   SPAFILE.SPA_ERN_TERM_9,
                   ISNULL(SPAFILE.SPA_ERN_TYPE_9,''),
                   SPAFILE.SPA_ERNFRMLA_10,
                   SPAFILE.SPA_ERNAMT_10,
                   SPAFILE.SPA_ERNPERCT_10,
                   ISNULL(SPAFILE.SPA_ERNPERCD_10,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_FG_10,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_START_CD_10,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_DFCD_10,''),
                   SPAFILE.SPA_ERN_DAYS_10,
                   SPAFILE.SPA_ERN_TERM_10,
                   ISNULL(SPAFILE.SPA_ERN_TYPE_10,''),
                   ISNULL(SPAFILE.SPA_ERNACCEL_CD,''),
                   ISNULL(SPAFILE.SPA_ERNJD_CD,''),
                   ISNULL(SPAFILE.SPA_MIDSTRERNCD,''),
                   SPAFILE.SPA_EARN_ORDER_1,
                   SPAFILE.SPA_EARN_ORDER_2,
                   SPAFILE.SPA_EARN_ORDER_3,
                   SPAFILE.SPA_EARN_ORDER_4,
                   SPAFILE.SPA_EARN_ORDER_5,
                   SPAFILE.SPA_EARN_ORDER_6,
                   SPAFILE.SPA_EARN_ORDER_7,
                   SPAFILE.SPA_EARN_ORDER_8,
                   SPAFILE.SPA_EARN_ORDER_9,
                   SPAFILE.SPA_EARN_ORDER_10,
                   ISNULL(SPAFILE.SPA_ERNDELQFRMLA,''),
                   SPAFILE.SPA_ERNDELQ_START,
                   SPAFILE.SPA_ERNDELQ_STOP,
                   ISNULL(SPAFILE.SPA_1098_INC_LC,''),
                   SPAFILE.SPA_MAX_IB_ACR_DAYS,
                   SPAFILE.SPA_LCGRACE,
                   ISNULL(SPAFILE.SPA_LCGRACECD,''),
                   ISNULL(SPAFILE.SPA_LCTYPE,''),
                   SPAFILE.SPA_LCRATE,
                   ISNULL(SPAFILE.SPA_LCFRMLA,''),
                   SPAFILE.SPA_LCMINCHG,
                   SPAFILE.SPA_LCMAXAMT,
                   SPAFILE.SPA_LCDELFAC,
                   SPAFILE.SPA_LCYRTYPE,
                   SPAFILE.SPA_LC_CODE,
                   ISNULL(SPAFILE.SPA_LCUNITPER_CD,''),
                   SPAFILE.SPA_DEFFRMLA,
                   ISNULL(SPAFILE.SPA_DEFEXCEPT,''),
                   SPAFILE.SPA_DEFWORKER,
                   SPAFILE.SPA_DEFPERCD,
                   SPAFILE.SPA_DEFBASE,
                   ISNULL(SPAFILE.SPA_DEFPOLCD,''),
                   SPAFILE.SPA_DEFPOLMX_1,
                   SPAFILE.SPA_DEFPOLMX_2,
                   SPAFILE.SPA_DEFPOLMX_3,
                   SPAFILE.SPA_DEFMIN,
                   SPAFILE.SPA_DEFMAX,
                   ISNULL(SPAFILE.SPA_DEFUNITPER_CD,''),
                   SPAFILE.SPA_PRFORM_1,
                   ISNULL(SPAFILE.SPA_PRFLAG_1,''),
                   SPAFILE.SPA_PRFORM_2,
                   ISNULL(SPAFILE.SPA_PRFLAG_2,''),
                   SPAFILE.SPA_PRFORM_3,
                   ISNULL(SPAFILE.SPA_PRFLAG_3,''),
                   SPAFILE.SPA_PRFORM_4,
                   ISNULL(SPAFILE.SPA_PRFLAG_4,''),
                   SPAFILE.SPA_PRFORM_5,
                   ISNULL(SPAFILE.SPA_PRFLAG_5,''),
                   SPAFILE.SPA_PRFORM_6,
                   ISNULL(SPAFILE.SPA_PRFLAG_6,''),
                   SPAFILE.SPA_PRFORM_7,
                   ISNULL(SPAFILE.SPA_PRFLAG_7,''),
                   SPAFILE.SPA_PRFORM_8,
                   ISNULL(SPAFILE.SPA_PRFLAG_8,''),
                   SPAFILE.SPA_PRFORM_9,
                   ISNULL(SPAFILE.SPA_PRFLAG_9,''),
                   SPAFILE.SPA_PRFORM_10,
                   ISNULL(SPAFILE.SPA_PRFLAG_10,''),
                   SPAFILE.SPA_PRFORM_11,
                   ISNULL(SPAFILE.SPA_PRFLAG_11,''),
                   SPAFILE.SPA_PRFORM_12,
                   ISNULL(SPAFILE.SPA_PRFLAG_12,''),
                   SPAFILE.SPA_PRFORM_13,
                   ISNULL(SPAFILE.SPA_PRFLAG_13,''),
                   SPAFILE.SPA_PRFORM_14,
                   ISNULL(SPAFILE.SPA_PRFLAG_14,''),
                   SPAFILE.SPA_PRFORM_15,
                   ISNULL(SPAFILE.SPA_PRFLAG_15,''),
                   SPAFILE.SPA_PRFORM_16,
                   ISNULL(SPAFILE.SPA_PRFLAG_16,''),
                   SPAFILE.SPA_PRFORM_17,
                   ISNULL(SPAFILE.SPA_PRFLAG_17,''),
                   SPAFILE.SPA_PRFORM_18,
                   ISNULL(SPAFILE.SPA_PRFLAG_18,''),
                   SPAFILE.SPA_PRFORM_19,
                   ISNULL(SPAFILE.SPA_PRFLAG_19,''),
                   SPAFILE.SPA_PRFORM_20,
                   ISNULL(SPAFILE.SPA_PRFLAG_20,''),
                   ISNULL(SPAFILE.SPA_IMED_REC,''),
                   SPAFILE.SPA_SCFRMLA,
                   SPAFILE.SPA_SCRATE,
                   SPAFILE.SPA_SCMIN,
                   SPAFILE.SPA_SCMAX,
                   SPAFILE.SPA_MFFRMLA,
                   SPAFILE.SPA_MFRATE,
                   SPAFILE.SPA_MFMIN,
                   SPAFILE.SPA_MFMAX,
                   ISNULL(SPAFILE.SPA_INTMETHOD,''),
                   SPAFILE.SPA_YEARTYPE,
                   SPAFILE.SPA_PYEXFRMLA,
                   SPAFILE.SPA_PYEXSPCODE,
                   SPAFILE.SPA_PYEXYEARTP,
                   ISNULL(SPAFILE.SPA_PYEXFMDATE,''),
                   ISNULL(SPAFILE.SPA_PYEXBASECD,''),
                   SPAFILE.SPA_PYEXBASE,
                   SPAFILE.SPA_PYEXDAYS,
                   SPAFILE.SPA_CAL_SPFRMLA,
                   ISNULL(SPAFILE.SPA_CAL_RATETYPE_1,''),
                   ISNULL(SPAFILE.SPA_CAL_BREAKTYPE_1,''),
                   ISNULL(SPAFILE.SPA_CAL_CHGABLE_1,''),
                   SPAFILE.SPA_CAL_NORATES_1,
                   SPAFILE.SPA_CAL_TBFRMLA_1,
                   SPAFILE.SPA_CAL_RATE_1_1,
                   SPAFILE.SPA_CAL_BREAK_1_1,
                   SPAFILE.SPA_CAL_RATE_1_2,
                   SPAFILE.SPA_CAL_BREAK_1_2,
                   SPAFILE.SPA_CAL_RATE_1_3,
                   SPAFILE.SPA_CAL_BREAK_1_3,
                   SPAFILE.SPA_CAL_RATE_1_4,
                   SPAFILE.SPA_CAL_BREAK_1_4,
                   SPAFILE.SPA_CAL_RATE_1_5,
                   SPAFILE.SPA_CAL_BREAK_1_5,
                   ISNULL(SPAFILE.SPA_CAL_RATETYPE_2,''),
                   ISNULL(SPAFILE.SPA_CAL_BREAKTYPE_2,''),
                   ISNULL(SPAFILE.SPA_CAL_CHGABLE_2,''),
                   SPAFILE.SPA_CAL_NORATES_2,
                   SPAFILE.SPA_CAL_TBFRMLA_2,
                   SPAFILE.SPA_CAL_RATE_2_1,
                   SPAFILE.SPA_CAL_BREAK_2_1,
                   SPAFILE.SPA_CAL_RATE_2_2,
                   SPAFILE.SPA_CAL_BREAK_2_2,
                   SPAFILE.SPA_CAL_RATE_2_3,
                   SPAFILE.SPA_CAL_BREAK_2_3,
                   SPAFILE.SPA_CAL_RATE_2_4,
                   SPAFILE.SPA_CAL_BREAK_2_4,
                   SPAFILE.SPA_CAL_RATE_2_5,
                   SPAFILE.SPA_CAL_BREAK_2_5,
                   SPAFILE.SPA_MINSF_FRMLA,
                   SPAFILE.SPA_MINSF_CHG,
                   SPAFILE.SPA_MAXAPR_FRMLA,
                   SPAFILE.SPA_EDT_NOEDITS_1,
                   ISNULL(SPAFILE.SPA_EDT_TYPE_1_1,''),
                   ISNULL(SPAFILE.SPA_EDT_HL_1_1,''),
                   ISNULL(SPAFILE.SPA_EDT_COLL_1_1,''),
                   ISNULL(SPAFILE.SPA_EDT_CD_1_1,''),
                   SPAFILE.SPA_EDT_RATE_1_1,
                   ISNULL(SPAFILE.SPA_EDT_RATETYPE_1_1,''),
                   SPAFILE.SPA_EDT_AMT_1_1,
                   ISNULL(SPAFILE.SPA_EDT_AMTYPE_1_1,''),
                   SPAFILE.SPA_EDT_TERM_1_1,
                   ISNULL(SPAFILE.SPA_EDT_TYPE_1_2,''),
                   ISNULL(SPAFILE.SPA_EDT_HL_1_2,''),
                   ISNULL(SPAFILE.SPA_EDT_COLL_1_2,''),
                   ISNULL(SPAFILE.SPA_EDT_CD_1_2,''),
                   SPAFILE.SPA_EDT_RATE_1_2,
                   ISNULL(SPAFILE.SPA_EDT_RATETYPE_1_2,''),
                   SPAFILE.SPA_EDT_AMT_1_2,
                   ISNULL(SPAFILE.SPA_EDT_AMTYPE_1_2,''),
                   SPAFILE.SPA_EDT_TERM_1_2,
                   ISNULL(SPAFILE.SPA_EDT_TYPE_1_3,''),
                   ISNULL(SPAFILE.SPA_EDT_HL_1_3,''),
                   ISNULL(SPAFILE.SPA_EDT_COLL_1_3,''),
                   ISNULL(SPAFILE.SPA_EDT_CD_1_3,''),
                   SPAFILE.SPA_EDT_RATE_1_3,
                   ISNULL(SPAFILE.SPA_EDT_RATETYPE_1_3,''),
                   SPAFILE.SPA_EDT_AMT_1_3,
                   ISNULL(SPAFILE.SPA_EDT_AMTYPE_1_3,''),
                   SPAFILE.SPA_EDT_TERM_1_3,
                   ISNULL(SPAFILE.SPA_EDT_TYPE_1_4,''),
                   ISNULL(SPAFILE.SPA_EDT_HL_1_4,''),
                   ISNULL(SPAFILE.SPA_EDT_COLL_1_4,''),
                   ISNULL(SPAFILE.SPA_EDT_CD_1_4,''),
                   SPAFILE.SPA_EDT_RATE_1_4,
                   ISNULL(SPAFILE.SPA_EDT_RATETYPE_1_4,''),
                   SPAFILE.SPA_EDT_AMT_1_4,
                   ISNULL(SPAFILE.SPA_EDT_AMTYPE_1_4,''),
                   SPAFILE.SPA_EDT_TERM_1_4,
                   ISNULL(SPAFILE.SPA_EDT_TYPE_1_5,''),
                   ISNULL(SPAFILE.SPA_EDT_HL_1_5,''),
                   ISNULL(SPAFILE.SPA_EDT_COLL_1_5,''),
                   ISNULL(SPAFILE.SPA_EDT_CD_1_5,''),
                   SPAFILE.SPA_EDT_RATE_1_5,
                   ISNULL(SPAFILE.SPA_EDT_RATETYPE_1_5,''),
                   SPAFILE.SPA_EDT_AMT_1_5,
                   ISNULL(SPAFILE.SPA_EDT_AMTYPE_1_5,''),
                   SPAFILE.SPA_EDT_TERM_1_5,
                   ISNULL(SPAFILE.SPA_EDT_TYPE_1_6,''),
                   ISNULL(SPAFILE.SPA_EDT_HL_1_6,''),
                   ISNULL(SPAFILE.SPA_EDT_COLL_1_6,''),
                   ISNULL(SPAFILE.SPA_EDT_CD_1_6,''),
                   SPAFILE.SPA_EDT_RATE_1_6,
                   ISNULL(SPAFILE.SPA_EDT_RATETYPE_1_6,''),
                   SPAFILE.SPA_EDT_AMT_1_6,
                   ISNULL(SPAFILE.SPA_EDT_AMTYPE_1_6,''),
                   SPAFILE.SPA_EDT_TERM_1_6,
                   SPAFILE.SPA_EDT_NOEDITS_2,
                   ISNULL(SPAFILE.SPA_EDT_TYPE_2_1,''),
                   ISNULL(SPAFILE.SPA_EDT_HL_2_1,''),
                   ISNULL(SPAFILE.SPA_EDT_COLL_2_1,''),
                   ISNULL(SPAFILE.SPA_EDT_CD_2_1,''),
                   SPAFILE.SPA_EDT_RATE_2_1,
                   ISNULL(SPAFILE.SPA_EDT_RATETYPE_2_1,''),
                   SPAFILE.SPA_EDT_AMT_2_1,
                   ISNULL(SPAFILE.SPA_EDT_AMTYPE_2_1,''),
                   SPAFILE.SPA_EDT_TERM_2_1,
                   ISNULL(SPAFILE.SPA_EDT_TYPE_2_2,''),
                   ISNULL(SPAFILE.SPA_EDT_HL_2_2,''),
                   ISNULL(SPAFILE.SPA_EDT_COLL_2_2,''),
                   ISNULL(SPAFILE.SPA_EDT_CD_2_2,''),
                   SPAFILE.SPA_EDT_RATE_2_2,
                   ISNULL(SPAFILE.SPA_EDT_RATETYPE_2_2,''),
                   SPAFILE.SPA_EDT_AMT_2_2,
                   ISNULL(SPAFILE.SPA_EDT_AMTYPE_2_2,''),
                   SPAFILE.SPA_EDT_TERM_2_2,
                   ISNULL(SPAFILE.SPA_EDT_TYPE_2_3,''),
                   ISNULL(SPAFILE.SPA_EDT_HL_2_3,''),
                   ISNULL(SPAFILE.SPA_EDT_COLL_2_3,''),
                   ISNULL(SPAFILE.SPA_EDT_CD_2_3,''),
                   SPAFILE.SPA_EDT_RATE_2_3,
                   ISNULL(SPAFILE.SPA_EDT_RATETYPE_2_3,''),
                   SPAFILE.SPA_EDT_AMT_2_3,
                   ISNULL(SPAFILE.SPA_EDT_AMTYPE_2_3,''),
                   SPAFILE.SPA_EDT_TERM_2_3,
                   ISNULL(SPAFILE.SPA_EDT_TYPE_2_4,''),
                   ISNULL(SPAFILE.SPA_EDT_HL_2_4,''),
                   ISNULL(SPAFILE.SPA_EDT_COLL_2_4,''),
                   ISNULL(SPAFILE.SPA_EDT_CD_2_4,''),
                   SPAFILE.SPA_EDT_RATE_2_4,
                   ISNULL(SPAFILE.SPA_EDT_RATETYPE_2_4,''),
                   SPAFILE.SPA_EDT_AMT_2_4,
                   ISNULL(SPAFILE.SPA_EDT_AMTYPE_2_4,''),
                   SPAFILE.SPA_EDT_TERM_2_4,
                   ISNULL(SPAFILE.SPA_EDT_TYPE_2_5,''),
                   ISNULL(SPAFILE.SPA_EDT_HL_2_5,''),
                   ISNULL(SPAFILE.SPA_EDT_COLL_2_5,''),
                   ISNULL(SPAFILE.SPA_EDT_CD_2_5,''),
                   SPAFILE.SPA_EDT_RATE_2_5,
                   ISNULL(SPAFILE.SPA_EDT_RATETYPE_2_5,''),
                   SPAFILE.SPA_EDT_AMT_2_5,
                   ISNULL(SPAFILE.SPA_EDT_AMTYPE_2_5,''),
                   SPAFILE.SPA_EDT_TERM_2_5,
                   ISNULL(SPAFILE.SPA_EDT_TYPE_2_6,''),
                   ISNULL(SPAFILE.SPA_EDT_HL_2_6,''),
                   ISNULL(SPAFILE.SPA_EDT_COLL_2_6,''),
                   ISNULL(SPAFILE.SPA_EDT_CD_2_6,''),
                   SPAFILE.SPA_EDT_RATE_2_6,
                   ISNULL(SPAFILE.SPA_EDT_RATETYPE_2_6,''),
                   SPAFILE.SPA_EDT_AMT_2_6,
                   ISNULL(SPAFILE.SPA_EDT_AMTYPE_2_6,''),
                   SPAFILE.SPA_EDT_TERM_2_6
            FROM DBO.SPAFILE
            WHERE SPAFILE.SPA_ORGST >= :QSP1-WBEG-ORGST
              AND SPAFILE.SPA_ORGST <= :QSP1-WEND-ORGST
              AND SPAFILE.SPA_SPRCLASS >= :QSP1-WBEG-SPRCLASS
              AND SPAFILE.SPA_SPRCLASS <= :QSP1-WEND-SPRCLASS
              AND SPAFILE.SPA_SUBCLASS >= :QSP1-WBEG-SUBCLASS
              AND SPAFILE.SPA_SUBCLASS <= :QSP1-WEND-SUBCLASS
              AND SPAFILE.SPA_LAWCODE >= :QSP1-WBEG-LAWCODE 
              AND SPAFILE.SPA_LAWCODE <= :QSP1-WEND-LAWCODE 
            ORDER BY SPAFILE.SPA_ORGST,
                     SPAFILE.SPA_SPRCLASS,
                     SPAFILE.SPA_SUBCLASS,
                     SPAFILE.SPA_LAWCODE
           END-EXEC.

           EXEC SQL
               OPEN SPA1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE SPACES TO QSP1-WBEG-ORGST
                          QSP1-WEND-ORGST.

           MOVE ZEROES TO QSP1-WBEG-SPRCLASS
                          QSP1-WEND-SPRCLASS
                          QSP1-WBEG-SUBCLASS
                          QSP1-WEND-SUBCLASS
                          QSP1-WBEG-LAWCODE
                          QSP1-WEND-LAWCODE.

           MOVE STAT---GOOD TO SPA1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-SPA1-FILE.
           PERFORM READ-IT.
           MOVE SPA-SELECT-PATH TO E-FILE.
           MOVE SPA1-KEY TO E-KEYX.
           INITIALIZE QSPA-REC.
           INITIALIZE QSPB-REC.
           INITIALIZE QSPC-REC.

           MOVE SPA-ORGST            TO QSPA-ORGST.
           MOVE SPA-SPRCLASS         TO QSPA-SPRCLASS.
           MOVE SPA-SUBCLASS         TO QSPA-SUBCLASS.
           MOVE SPA-LAWCODE          TO QSPA-LAWCODE.

           EXEC SQL
            SELECT ISNULL(SPAFILE.SPA_ORGST,''),
                   SPAFILE.SPA_SPRCLASS,
                   SPAFILE.SPA_SUBCLASS,
                   SPAFILE.SPA_LAWCODE,
                   ISNULL(SPAFILE.SPA_EXPD_FRMLA,''),
                   SPAFILE.SPA_MFFRMLA,
                   ISNULL(SPAFILE.SPA_CAL_CHGABLE_1,''),
                   ISNULL(SPAFILE.SPA_CAL_CHGABLE_2,'')
              INTO  
                   :QSPA-ORGST,
                   :QSPA-SPRCLASS,
                   :QSPA-SUBCLASS,
                   :QSPA-LAWCODE,
                   :QSPA-EXPD-FRMLA,
				   :QSPA-MFFRMLA,
                   :QSPA-CAL-CHGABLE-1,
                   :QSPA-CAL-CHGABLE-2 
 
              FROM DBO.SPAFILE SPAFILE  
             WHERE ( SPAFILE.SPA_ORGST    = :QSPA-ORGST
               AND   SPAFILE.SPA_SPRCLASS = :QSPA-SPRCLASS
               AND   SPAFILE.SPA_SUBCLASS = :QSPA-SUBCLASS
               AND   SPAFILE.SPA_LAWCODE  = :QSPA-LAWCODE )		   
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-SPA1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-SPA-FIELDS
      *        PERFORM GET-SPB-FIELDS
              PERFORM GET-SPC-FIELDS.

      *-----------------------------------------------------------------
       READ-SPA1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE SPA-SELECT-PATH TO E-FILE.
           MOVE SPA1-KEY TO E-KEYX.
           INITIALIZE QSPA-REC.

           IF ( SPA1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH SPA1_CURSOR INTO :QSPA-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-SPA1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-SPA-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-SPA1-FILE.
           PERFORM CLOSE-IT.
           IF ( SPA1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE SPA1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO SPA1-CURSOR-STAT.

      *=================================================================
       LOAD-SPB1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( SP-PATH-OVERRIDE = "Y" )
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( SP-PATH-OVERRIDE = SPACES )
              MOVE EXT-FILPATH-BASE        TO SPB-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO SPB-ALL-MACHINE
              MOVE SPB-ALL-PATH             TO SPB-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( SP-PATH-OVERRIDE = "B" )
              MOVE FILPATH-BASE            TO SPB-ALL-BASE
              MOVE FILPATH-MACHINE         TO SPB-ALL-MACHINE
              MOVE SPB-ALL-PATH             TO SPB-PATH.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-SPBFILE NOT = "S" )
              MOVE SPB-PATH-BEG          TO SPB-SELECT-PATH-BEG
              MOVE SPB-PATH-FILE         TO SPB-SELECT-PATH-FILE
           ELSE
              MOVE SPB-PATH-SQL          TO SPB-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-SPB1-FILE.
           PERFORM LOAD-SPB1-FILE.
           PERFORM OPEN-IT.
           MOVE SPB-SELECT-PATH TO E-FILE.

      *-----------------------------------------------------------------
       READ-SPB1-FILE.
           PERFORM READ-IT.
           MOVE SPB-SELECT-PATH TO E-FILE.
           MOVE SPB1-KEY TO E-KEYX.

           MOVE SPB-ORGST            TO QSPB-ORGST.
           MOVE SPB-SPRCLASS         TO QSPB-SPRCLASS.
           MOVE SPB-SUBCLASS         TO QSPB-SUBCLASS.
           MOVE SPB-LAWCODE          TO QSPB-LAWCODE.

           EXEC SQL
            SELECT ISNULL(SPBFILE.SPB_ORGST,''),
                   SPBFILE.SPB_SPRCLASS,
                   SPBFILE.SPB_SUBCLASS,
                   SPBFILE.SPB_LAWCODE,
                   SPBFILE.SPB_INSCOM_1,
                   SPBFILE.SPB_INSRATECD_1,
                   SPBFILE.SPB_INSJFAC_1,
                   SPBFILE.SPB_INSDISCNT_1,
                   ISNULL(SPBFILE.SPB_INSCHGABL_1,''),
                   SPBFILE.SPB_INSTERM_1_1,
                   SPBFILE.SPB_INSRATE_1_1,
                   SPBFILE.SPB_INSTERM_1_2,
                   SPBFILE.SPB_INSRATE_1_2,
                   SPBFILE.SPB_INSTERM_1_3,
                   SPBFILE.SPB_INSRATE_1_3,
                   SPBFILE.SPB_INSTERM_1_4,
                   SPBFILE.SPB_INSRATE_1_4,
                   SPBFILE.SPB_INSTERM_1_5,
                   SPBFILE.SPB_INSRATE_1_5,
                   SPBFILE.SPB_INSTERM_1_6,
                   SPBFILE.SPB_INSRATE_1_6,
                   SPBFILE.SPB_INSTERM_1_7,
                   SPBFILE.SPB_INSRATE_1_7,
                   SPBFILE.SPB_INSTERM_1_8,
                   SPBFILE.SPB_INSRATE_1_8,
                   SPBFILE.SPB_INSTERM_1_9,
                   SPBFILE.SPB_INSRATE_1_9,
                   SPBFILE.SPB_INSTERM_1_10,
                   SPBFILE.SPB_INSRATE_1_10,
                   SPBFILE.SPB_INSTERM_1_11,
                   SPBFILE.SPB_INSRATE_1_11,
                   SPBFILE.SPB_INSTERM_1_12,
                   SPBFILE.SPB_INSRATE_1_12,
                   SPBFILE.SPB_INSTERM_1_13,
                   SPBFILE.SPB_INSRATE_1_13,
                   SPBFILE.SPB_INSTERM_1_14,
                   SPBFILE.SPB_INSRATE_1_14,
                   SPBFILE.SPB_INSTERM_1_15,
                   SPBFILE.SPB_INSRATE_1_15,
                   SPBFILE.SPB_INSTERM_1_16,
                   SPBFILE.SPB_INSRATE_1_16,
                   SPBFILE.SPB_INSTERM_1_17,
                   SPBFILE.SPB_INSRATE_1_17,
                   SPBFILE.SPB_INSTERM_1_18,
                   SPBFILE.SPB_INSRATE_1_18,
                   SPBFILE.SPB_INSTERM_1_19,
                   SPBFILE.SPB_INSRATE_1_19,
                   SPBFILE.SPB_INSTERM_1_20,
                   SPBFILE.SPB_INSRATE_1_20,
                   SPBFILE.SPB_INSMINPREM_1,
                   SPBFILE.SPB_INSMINCHGABL_1,
                   SPBFILE.SPB_INSMAXCHGABL_1,
                   SPBFILE.SPB_INSMAXBENIF_1,
                   SPBFILE.SPB_INSMAXTERM_1,
                   ISNULL(SPBFILE.SPB_INSCHGBEN_1,''),
                   SPBFILE.SPB_INSEXCEPT_1,
                   SPBFILE.SPB_INSNOVER_1,
                   SPBFILE.SPB_INSMINAGE_1_1,
                   SPBFILE.SPB_INSMAXAGE_1_1,
                   SPBFILE.SPB_INSMAXCOV_1_1,
                   SPBFILE.SPB_INSMAXTRM_1_1,
                   SPBFILE.SPB_INSMINAGE_1_2,
                   SPBFILE.SPB_INSMAXAGE_1_2,
                   SPBFILE.SPB_INSMAXCOV_1_2,
                   SPBFILE.SPB_INSMAXTRM_1_2,
                   SPBFILE.SPB_INSMINAGE_1_3,
                   SPBFILE.SPB_INSMAXAGE_1_3,
                   SPBFILE.SPB_INSMAXCOV_1_3,
                   SPBFILE.SPB_INSMAXTRM_1_3,
                   SPBFILE.SPB_INSMINAGE_1_4,
                   SPBFILE.SPB_INSMAXAGE_1_4,
                   SPBFILE.SPB_INSMAXCOV_1_4,
                   SPBFILE.SPB_INSMAXTRM_1_4,
                   SPBFILE.SPB_INSMINAGE_1_5,
                   SPBFILE.SPB_INSMAXAGE_1_5,
                   SPBFILE.SPB_INSMAXCOV_1_5,
                   SPBFILE.SPB_INSMAXTRM_1_5,
                   ISNULL(SPBFILE.SPB_INSFEECD_1,''),
                   ISNULL(SPBFILE.SPB_INSFEE_FRMLA_1,''),
                   ISNULL(SPBFILE.SPB_INSCHGABL_FG_1,''),
                   ISNULL(SPBFILE.SPB_INSREJECT_FG_1,''),
                   ISNULL(SPBFILE.SPB_INSFINCHG_1,''),
                   SPBFILE.SPB_INSCOM_2,
                   SPBFILE.SPB_INSRATECD_2,
                   SPBFILE.SPB_INSJFAC_2,
                   SPBFILE.SPB_INSDISCNT_2,
                   ISNULL(SPBFILE.SPB_INSCHGABL_2,''),
                   SPBFILE.SPB_INSTERM_2_1,
                   SPBFILE.SPB_INSRATE_2_1,
                   SPBFILE.SPB_INSTERM_2_2,
                   SPBFILE.SPB_INSRATE_2_2,
                   SPBFILE.SPB_INSTERM_2_3,
                   SPBFILE.SPB_INSRATE_2_3,
                   SPBFILE.SPB_INSTERM_2_4,
                   SPBFILE.SPB_INSRATE_2_4,
                   SPBFILE.SPB_INSTERM_2_5,
                   SPBFILE.SPB_INSRATE_2_5,
                   SPBFILE.SPB_INSTERM_2_6,
                   SPBFILE.SPB_INSRATE_2_6,
                   SPBFILE.SPB_INSTERM_2_7,
                   SPBFILE.SPB_INSRATE_2_7,
                   SPBFILE.SPB_INSTERM_2_8,
                   SPBFILE.SPB_INSRATE_2_8,
                   SPBFILE.SPB_INSTERM_2_9,
                   SPBFILE.SPB_INSRATE_2_9,
                   SPBFILE.SPB_INSTERM_2_10,
                   SPBFILE.SPB_INSRATE_2_10,
                   SPBFILE.SPB_INSTERM_2_11,
                   SPBFILE.SPB_INSRATE_2_11,
                   SPBFILE.SPB_INSTERM_2_12,
                   SPBFILE.SPB_INSRATE_2_12,
                   SPBFILE.SPB_INSTERM_2_13,
                   SPBFILE.SPB_INSRATE_2_13,
                   SPBFILE.SPB_INSTERM_2_14,
                   SPBFILE.SPB_INSRATE_2_14,
                   SPBFILE.SPB_INSTERM_2_15,
                   SPBFILE.SPB_INSRATE_2_15,
                   SPBFILE.SPB_INSTERM_2_16,
                   SPBFILE.SPB_INSRATE_2_16,
                   SPBFILE.SPB_INSTERM_2_17,
                   SPBFILE.SPB_INSRATE_2_17,
                   SPBFILE.SPB_INSTERM_2_18,
                   SPBFILE.SPB_INSRATE_2_18,
                   SPBFILE.SPB_INSTERM_2_19,
                   SPBFILE.SPB_INSRATE_2_19,
                   SPBFILE.SPB_INSTERM_2_20,
                   SPBFILE.SPB_INSRATE_2_20,
                   SPBFILE.SPB_INSMINPREM_2,
                   SPBFILE.SPB_INSMINCHGABL_2,
                   SPBFILE.SPB_INSMAXCHGABL_2,
                   SPBFILE.SPB_INSMAXBENIF_2,
                   SPBFILE.SPB_INSMAXTERM_2,
                   ISNULL(SPBFILE.SPB_INSCHGBEN_2,''),
                   SPBFILE.SPB_INSEXCEPT_2,
                   SPBFILE.SPB_INSNOVER_2,
                   SPBFILE.SPB_INSMINAGE_2_1,
                   SPBFILE.SPB_INSMAXAGE_2_1,
                   SPBFILE.SPB_INSMAXCOV_2_1,
                   SPBFILE.SPB_INSMAXTRM_2_1,
                   SPBFILE.SPB_INSMINAGE_2_2,
                   SPBFILE.SPB_INSMAXAGE_2_2,
                   SPBFILE.SPB_INSMAXCOV_2_2,
                   SPBFILE.SPB_INSMAXTRM_2_2,
                   SPBFILE.SPB_INSMINAGE_2_3,
                   SPBFILE.SPB_INSMAXAGE_2_3,
                   SPBFILE.SPB_INSMAXCOV_2_3,
                   SPBFILE.SPB_INSMAXTRM_2_3,
                   SPBFILE.SPB_INSMINAGE_2_4,
                   SPBFILE.SPB_INSMAXAGE_2_4,
                   SPBFILE.SPB_INSMAXCOV_2_4,
                   SPBFILE.SPB_INSMAXTRM_2_4,
                   SPBFILE.SPB_INSMINAGE_2_5,
                   SPBFILE.SPB_INSMAXAGE_2_5,
                   SPBFILE.SPB_INSMAXCOV_2_5,
                   SPBFILE.SPB_INSMAXTRM_2_5,
                   ISNULL(SPBFILE.SPB_INSFEECD_2,''),
                   ISNULL(SPBFILE.SPB_INSFEE_FRMLA_2,''),
                   ISNULL(SPBFILE.SPB_INSCHGABL_FG_2,''),
                   ISNULL(SPBFILE.SPB_INSREJECT_FG_2,''),
                   ISNULL(SPBFILE.SPB_INSFINCHG_2,''),
                   SPBFILE.SPB_INSCOM_3,
                   SPBFILE.SPB_INSRATECD_3,
                   SPBFILE.SPB_INSJFAC_3,
                   SPBFILE.SPB_INSDISCNT_3,
                   ISNULL(SPBFILE.SPB_INSCHGABL_3,''),
                   SPBFILE.SPB_INSTERM_3_1,
                   SPBFILE.SPB_INSRATE_3_1,
                   SPBFILE.SPB_INSTERM_3_2,
                   SPBFILE.SPB_INSRATE_3_2,
                   SPBFILE.SPB_INSTERM_3_3,
                   SPBFILE.SPB_INSRATE_3_3,
                   SPBFILE.SPB_INSTERM_3_4,
                   SPBFILE.SPB_INSRATE_3_4,
                   SPBFILE.SPB_INSTERM_3_5,
                   SPBFILE.SPB_INSRATE_3_5,
                   SPBFILE.SPB_INSTERM_3_6,
                   SPBFILE.SPB_INSRATE_3_6,
                   SPBFILE.SPB_INSTERM_3_7,
                   SPBFILE.SPB_INSRATE_3_7,
                   SPBFILE.SPB_INSTERM_3_8,
                   SPBFILE.SPB_INSRATE_3_8,
                   SPBFILE.SPB_INSTERM_3_9,
                   SPBFILE.SPB_INSRATE_3_9,
                   SPBFILE.SPB_INSTERM_3_10,
                   SPBFILE.SPB_INSRATE_3_10,
                   SPBFILE.SPB_INSTERM_3_11,
                   SPBFILE.SPB_INSRATE_3_11,
                   SPBFILE.SPB_INSTERM_3_12,
                   SPBFILE.SPB_INSRATE_3_12,
                   SPBFILE.SPB_INSTERM_3_13,
                   SPBFILE.SPB_INSRATE_3_13,
                   SPBFILE.SPB_INSTERM_3_14,
                   SPBFILE.SPB_INSRATE_3_14,
                   SPBFILE.SPB_INSTERM_3_15,
                   SPBFILE.SPB_INSRATE_3_15,
                   SPBFILE.SPB_INSTERM_3_16,
                   SPBFILE.SPB_INSRATE_3_16,
                   SPBFILE.SPB_INSTERM_3_17,
                   SPBFILE.SPB_INSRATE_3_17,
                   SPBFILE.SPB_INSTERM_3_18,
                   SPBFILE.SPB_INSRATE_3_18,
                   SPBFILE.SPB_INSTERM_3_19,
                   SPBFILE.SPB_INSRATE_3_19,
                   SPBFILE.SPB_INSTERM_3_20,
                   SPBFILE.SPB_INSRATE_3_20,
                   SPBFILE.SPB_INSMINPREM_3,
                   SPBFILE.SPB_INSMINCHGABL_3,
                   SPBFILE.SPB_INSMAXCHGABL_3,
                   SPBFILE.SPB_INSMAXBENIF_3,
                   SPBFILE.SPB_INSMAXTERM_3,
                   ISNULL(SPBFILE.SPB_INSCHGBEN_3,''),
                   SPBFILE.SPB_INSEXCEPT_3,
                   SPBFILE.SPB_INSNOVER_3,
                   SPBFILE.SPB_INSMINAGE_3_1,
                   SPBFILE.SPB_INSMAXAGE_3_1,
                   SPBFILE.SPB_INSMAXCOV_3_1,
                   SPBFILE.SPB_INSMAXTRM_3_1,
                   SPBFILE.SPB_INSMINAGE_3_2,
                   SPBFILE.SPB_INSMAXAGE_3_2,
                   SPBFILE.SPB_INSMAXCOV_3_2,
                   SPBFILE.SPB_INSMAXTRM_3_2,
                   SPBFILE.SPB_INSMINAGE_3_3,
                   SPBFILE.SPB_INSMAXAGE_3_3,
                   SPBFILE.SPB_INSMAXCOV_3_3,
                   SPBFILE.SPB_INSMAXTRM_3_3,
                   SPBFILE.SPB_INSMINAGE_3_4,
                   SPBFILE.SPB_INSMAXAGE_3_4,
                   SPBFILE.SPB_INSMAXCOV_3_4,
                   SPBFILE.SPB_INSMAXTRM_3_4,
                   SPBFILE.SPB_INSMINAGE_3_5,
                   SPBFILE.SPB_INSMAXAGE_3_5,
                   SPBFILE.SPB_INSMAXCOV_3_5,
                   SPBFILE.SPB_INSMAXTRM_3_5,
                   ISNULL(SPBFILE.SPB_INSFEECD_3,''),
                   ISNULL(SPBFILE.SPB_INSFEE_FRMLA_3,''),
                   ISNULL(SPBFILE.SPB_INSCHGABL_FG_3,''),
                   ISNULL(SPBFILE.SPB_INSREJECT_FG_3,''),
                   ISNULL(SPBFILE.SPB_INSFINCHG_3,''),
                   SPBFILE.SPB_INSCOM_4,
                   SPBFILE.SPB_INSRATECD_4,
                   SPBFILE.SPB_INSJFAC_4,
                   SPBFILE.SPB_INSDISCNT_4,
                   ISNULL(SPBFILE.SPB_INSCHGABL_4,''),
                   SPBFILE.SPB_INSTERM_4_1,
                   SPBFILE.SPB_INSRATE_4_1,
                   SPBFILE.SPB_INSTERM_4_2,
                   SPBFILE.SPB_INSRATE_4_2,
                   SPBFILE.SPB_INSTERM_4_3,
                   SPBFILE.SPB_INSRATE_4_3,
                   SPBFILE.SPB_INSTERM_4_4,
                   SPBFILE.SPB_INSRATE_4_4,
                   SPBFILE.SPB_INSTERM_4_5,
                   SPBFILE.SPB_INSRATE_4_5,
                   SPBFILE.SPB_INSTERM_4_6,
                   SPBFILE.SPB_INSRATE_4_6,
                   SPBFILE.SPB_INSTERM_4_7,
                   SPBFILE.SPB_INSRATE_4_7,
                   SPBFILE.SPB_INSTERM_4_8,
                   SPBFILE.SPB_INSRATE_4_8,
                   SPBFILE.SPB_INSTERM_4_9,
                   SPBFILE.SPB_INSRATE_4_9,
                   SPBFILE.SPB_INSTERM_4_10,
                   SPBFILE.SPB_INSRATE_4_10,
                   SPBFILE.SPB_INSTERM_4_11,
                   SPBFILE.SPB_INSRATE_4_11,
                   SPBFILE.SPB_INSTERM_4_12,
                   SPBFILE.SPB_INSRATE_4_12,
                   SPBFILE.SPB_INSTERM_4_13,
                   SPBFILE.SPB_INSRATE_4_13,
                   SPBFILE.SPB_INSTERM_4_14,
                   SPBFILE.SPB_INSRATE_4_14,
                   SPBFILE.SPB_INSTERM_4_15,
                   SPBFILE.SPB_INSRATE_4_15,
                   SPBFILE.SPB_INSTERM_4_16,
                   SPBFILE.SPB_INSRATE_4_16,
                   SPBFILE.SPB_INSTERM_4_17,
                   SPBFILE.SPB_INSRATE_4_17,
                   SPBFILE.SPB_INSTERM_4_18,
                   SPBFILE.SPB_INSRATE_4_18,
                   SPBFILE.SPB_INSTERM_4_19,
                   SPBFILE.SPB_INSRATE_4_19,
                   SPBFILE.SPB_INSTERM_4_20,
                   SPBFILE.SPB_INSRATE_4_20,
                   SPBFILE.SPB_INSMINPREM_4,
                   SPBFILE.SPB_INSMINCHGABL_4,
                   SPBFILE.SPB_INSMAXCHGABL_4,
                   SPBFILE.SPB_INSMAXBENIF_4,
                   SPBFILE.SPB_INSMAXTERM_4,
                   ISNULL(SPBFILE.SPB_INSCHGBEN_4,''),
                   SPBFILE.SPB_INSEXCEPT_4,
                   SPBFILE.SPB_INSNOVER_4,
                   SPBFILE.SPB_INSMINAGE_4_1,
                   SPBFILE.SPB_INSMAXAGE_4_1,
                   SPBFILE.SPB_INSMAXCOV_4_1,
                   SPBFILE.SPB_INSMAXTRM_4_1,
                   SPBFILE.SPB_INSMINAGE_4_2,
                   SPBFILE.SPB_INSMAXAGE_4_2,
                   SPBFILE.SPB_INSMAXCOV_4_2,
                   SPBFILE.SPB_INSMAXTRM_4_2,
                   SPBFILE.SPB_INSMINAGE_4_3,
                   SPBFILE.SPB_INSMAXAGE_4_3,
                   SPBFILE.SPB_INSMAXCOV_4_3,
                   SPBFILE.SPB_INSMAXTRM_4_3,
                   SPBFILE.SPB_INSMINAGE_4_4,
                   SPBFILE.SPB_INSMAXAGE_4_4,
                   SPBFILE.SPB_INSMAXCOV_4_4,
                   SPBFILE.SPB_INSMAXTRM_4_4,
                   SPBFILE.SPB_INSMINAGE_4_5,
                   SPBFILE.SPB_INSMAXAGE_4_5,
                   SPBFILE.SPB_INSMAXCOV_4_5,
                   SPBFILE.SPB_INSMAXTRM_4_5,
                   ISNULL(SPBFILE.SPB_INSFEECD_4,''),
                   ISNULL(SPBFILE.SPB_INSFEE_FRMLA_4,''),
                   ISNULL(SPBFILE.SPB_INSCHGABL_FG_4,''),
                   ISNULL(SPBFILE.SPB_INSREJECT_FG_4,''),
                   ISNULL(SPBFILE.SPB_INSFINCHG_4,''),
                   SPBFILE.SPB_INSCOM_5,
                   SPBFILE.SPB_INSRATECD_5,
                   SPBFILE.SPB_INSJFAC_5,
                   SPBFILE.SPB_INSDISCNT_5,
                   ISNULL(SPBFILE.SPB_INSCHGABL_5,''),
                   SPBFILE.SPB_INSTERM_5_1,
                   SPBFILE.SPB_INSRATE_5_1,
                   SPBFILE.SPB_INSTERM_5_2,
                   SPBFILE.SPB_INSRATE_5_2,
                   SPBFILE.SPB_INSTERM_5_3,
                   SPBFILE.SPB_INSRATE_5_3,
                   SPBFILE.SPB_INSTERM_5_4,
                   SPBFILE.SPB_INSRATE_5_4,
                   SPBFILE.SPB_INSTERM_5_5,
                   SPBFILE.SPB_INSRATE_5_5,
                   SPBFILE.SPB_INSTERM_5_6,
                   SPBFILE.SPB_INSRATE_5_6,
                   SPBFILE.SPB_INSTERM_5_7,
                   SPBFILE.SPB_INSRATE_5_7,
                   SPBFILE.SPB_INSTERM_5_8,
                   SPBFILE.SPB_INSRATE_5_8,
                   SPBFILE.SPB_INSTERM_5_9,
                   SPBFILE.SPB_INSRATE_5_9,
                   SPBFILE.SPB_INSTERM_5_10,
                   SPBFILE.SPB_INSRATE_5_10,
                   SPBFILE.SPB_INSTERM_5_11,
                   SPBFILE.SPB_INSRATE_5_11,
                   SPBFILE.SPB_INSTERM_5_12,
                   SPBFILE.SPB_INSRATE_5_12,
                   SPBFILE.SPB_INSTERM_5_13,
                   SPBFILE.SPB_INSRATE_5_13,
                   SPBFILE.SPB_INSTERM_5_14,
                   SPBFILE.SPB_INSRATE_5_14,
                   SPBFILE.SPB_INSTERM_5_15,
                   SPBFILE.SPB_INSRATE_5_15,
                   SPBFILE.SPB_INSTERM_5_16,
                   SPBFILE.SPB_INSRATE_5_16,
                   SPBFILE.SPB_INSTERM_5_17,
                   SPBFILE.SPB_INSRATE_5_17,
                   SPBFILE.SPB_INSTERM_5_18,
                   SPBFILE.SPB_INSRATE_5_18,
                   SPBFILE.SPB_INSTERM_5_19,
                   SPBFILE.SPB_INSRATE_5_19,
                   SPBFILE.SPB_INSTERM_5_20,
                   SPBFILE.SPB_INSRATE_5_20,
                   SPBFILE.SPB_INSMINPREM_5,
                   SPBFILE.SPB_INSMINCHGABL_5,
                   SPBFILE.SPB_INSMAXCHGABL_5,
                   SPBFILE.SPB_INSMAXBENIF_5,
                   SPBFILE.SPB_INSMAXTERM_5,
                   ISNULL(SPBFILE.SPB_INSCHGBEN_5,''),
                   SPBFILE.SPB_INSEXCEPT_5,
                   SPBFILE.SPB_INSNOVER_5,
                   SPBFILE.SPB_INSMINAGE_5_1,
                   SPBFILE.SPB_INSMAXAGE_5_1,
                   SPBFILE.SPB_INSMAXCOV_5_1,
                   SPBFILE.SPB_INSMAXTRM_5_1,
                   SPBFILE.SPB_INSMINAGE_5_2,
                   SPBFILE.SPB_INSMAXAGE_5_2,
                   SPBFILE.SPB_INSMAXCOV_5_2,
                   SPBFILE.SPB_INSMAXTRM_5_2,
                   SPBFILE.SPB_INSMINAGE_5_3,
                   SPBFILE.SPB_INSMAXAGE_5_3,
                   SPBFILE.SPB_INSMAXCOV_5_3,
                   SPBFILE.SPB_INSMAXTRM_5_3,
                   SPBFILE.SPB_INSMINAGE_5_4,
                   SPBFILE.SPB_INSMAXAGE_5_4,
                   SPBFILE.SPB_INSMAXCOV_5_4,
                   SPBFILE.SPB_INSMAXTRM_5_4,
                   SPBFILE.SPB_INSMINAGE_5_5,
                   SPBFILE.SPB_INSMAXAGE_5_5,
                   SPBFILE.SPB_INSMAXCOV_5_5,
                   SPBFILE.SPB_INSMAXTRM_5_5,
                   ISNULL(SPBFILE.SPB_INSFEECD_5,''),
                   ISNULL(SPBFILE.SPB_INSFEE_FRMLA_5,''),
                   ISNULL(SPBFILE.SPB_INSCHGABL_FG_5,''),
                   ISNULL(SPBFILE.SPB_INSREJECT_FG_5,''),
                   ISNULL(SPBFILE.SPB_INSFINCHG_5,''),
                   SPBFILE.SPB_INSCOM_6,
                   SPBFILE.SPB_INSRATECD_6,
                   SPBFILE.SPB_INSJFAC_6,
                   SPBFILE.SPB_INSDISCNT_6,
                   ISNULL(SPBFILE.SPB_INSCHGABL_6,''),
                   SPBFILE.SPB_INSTERM_6_1,
                   SPBFILE.SPB_INSRATE_6_1,
                   SPBFILE.SPB_INSTERM_6_2,
                   SPBFILE.SPB_INSRATE_6_2,
                   SPBFILE.SPB_INSTERM_6_3,
                   SPBFILE.SPB_INSRATE_6_3,
                   SPBFILE.SPB_INSTERM_6_4,
                   SPBFILE.SPB_INSRATE_6_4,
                   SPBFILE.SPB_INSTERM_6_5,
                   SPBFILE.SPB_INSRATE_6_5,
                   SPBFILE.SPB_INSTERM_6_6,
                   SPBFILE.SPB_INSRATE_6_6,
                   SPBFILE.SPB_INSTERM_6_7,
                   SPBFILE.SPB_INSRATE_6_7,
                   SPBFILE.SPB_INSTERM_6_8,
                   SPBFILE.SPB_INSRATE_6_8,
                   SPBFILE.SPB_INSTERM_6_9,
                   SPBFILE.SPB_INSRATE_6_9,
                   SPBFILE.SPB_INSTERM_6_10,
                   SPBFILE.SPB_INSRATE_6_10,
                   SPBFILE.SPB_INSTERM_6_11,
                   SPBFILE.SPB_INSRATE_6_11,
                   SPBFILE.SPB_INSTERM_6_12,
                   SPBFILE.SPB_INSRATE_6_12,
                   SPBFILE.SPB_INSTERM_6_13,
                   SPBFILE.SPB_INSRATE_6_13,
                   SPBFILE.SPB_INSTERM_6_14,
                   SPBFILE.SPB_INSRATE_6_14,
                   SPBFILE.SPB_INSTERM_6_15,
                   SPBFILE.SPB_INSRATE_6_15,
                   SPBFILE.SPB_INSTERM_6_16,
                   SPBFILE.SPB_INSRATE_6_16,
                   SPBFILE.SPB_INSTERM_6_17,
                   SPBFILE.SPB_INSRATE_6_17,
                   SPBFILE.SPB_INSTERM_6_18,
                   SPBFILE.SPB_INSRATE_6_18,
                   SPBFILE.SPB_INSTERM_6_19,
                   SPBFILE.SPB_INSRATE_6_19,
                   SPBFILE.SPB_INSTERM_6_20,
                   SPBFILE.SPB_INSRATE_6_20,
                   SPBFILE.SPB_INSMINPREM_6,
                   SPBFILE.SPB_INSMINCHGABL_6,
                   SPBFILE.SPB_INSMAXCHGABL_6,
                   SPBFILE.SPB_INSMAXBENIF_6,
                   SPBFILE.SPB_INSMAXTERM_6,
                   ISNULL(SPBFILE.SPB_INSCHGBEN_6,''),
                   SPBFILE.SPB_INSEXCEPT_6,
                   SPBFILE.SPB_INSNOVER_6,
                   SPBFILE.SPB_INSMINAGE_6_1,
                   SPBFILE.SPB_INSMAXAGE_6_1,
                   SPBFILE.SPB_INSMAXCOV_6_1,
                   SPBFILE.SPB_INSMAXTRM_6_1,
                   SPBFILE.SPB_INSMINAGE_6_2,
                   SPBFILE.SPB_INSMAXAGE_6_2,
                   SPBFILE.SPB_INSMAXCOV_6_2,
                   SPBFILE.SPB_INSMAXTRM_6_2,
                   SPBFILE.SPB_INSMINAGE_6_3,
                   SPBFILE.SPB_INSMAXAGE_6_3,
                   SPBFILE.SPB_INSMAXCOV_6_3,
                   SPBFILE.SPB_INSMAXTRM_6_3,
                   SPBFILE.SPB_INSMINAGE_6_4,
                   SPBFILE.SPB_INSMAXAGE_6_4,
                   SPBFILE.SPB_INSMAXCOV_6_4,
                   SPBFILE.SPB_INSMAXTRM_6_4,
                   SPBFILE.SPB_INSMINAGE_6_5,
                   SPBFILE.SPB_INSMAXAGE_6_5,
                   SPBFILE.SPB_INSMAXCOV_6_5,
                   SPBFILE.SPB_INSMAXTRM_6_5,
                   ISNULL(SPBFILE.SPB_INSFEECD_6,''),
                   ISNULL(SPBFILE.SPB_INSFEE_FRMLA_6,''),
                   ISNULL(SPBFILE.SPB_INSCHGABL_FG_6,''),
                   ISNULL(SPBFILE.SPB_INSREJECT_FG_6,''),
                   ISNULL(SPBFILE.SPB_INSFINCHG_6,''),
                   SPBFILE.SPB_INSCOM_7,
                   SPBFILE.SPB_INSRATECD_7,
                   SPBFILE.SPB_INSJFAC_7,
                   SPBFILE.SPB_INSDISCNT_7,
                   ISNULL(SPBFILE.SPB_INSCHGABL_7,''),
                   SPBFILE.SPB_INSTERM_7_1,
                   SPBFILE.SPB_INSRATE_7_1,
                   SPBFILE.SPB_INSTERM_7_2,
                   SPBFILE.SPB_INSRATE_7_2,
                   SPBFILE.SPB_INSTERM_7_3,
                   SPBFILE.SPB_INSRATE_7_3,
                   SPBFILE.SPB_INSTERM_7_4,
                   SPBFILE.SPB_INSRATE_7_4,
                   SPBFILE.SPB_INSTERM_7_5,
                   SPBFILE.SPB_INSRATE_7_5,
                   SPBFILE.SPB_INSTERM_7_6,
                   SPBFILE.SPB_INSRATE_7_6,
                   SPBFILE.SPB_INSTERM_7_7,
                   SPBFILE.SPB_INSRATE_7_7,
                   SPBFILE.SPB_INSTERM_7_8,
                   SPBFILE.SPB_INSRATE_7_8,
                   SPBFILE.SPB_INSTERM_7_9,
                   SPBFILE.SPB_INSRATE_7_9,
                   SPBFILE.SPB_INSTERM_7_10,
                   SPBFILE.SPB_INSRATE_7_10,
                   SPBFILE.SPB_INSTERM_7_11,
                   SPBFILE.SPB_INSRATE_7_11,
                   SPBFILE.SPB_INSTERM_7_12,
                   SPBFILE.SPB_INSRATE_7_12,
                   SPBFILE.SPB_INSTERM_7_13,
                   SPBFILE.SPB_INSRATE_7_13,
                   SPBFILE.SPB_INSTERM_7_14,
                   SPBFILE.SPB_INSRATE_7_14,
                   SPBFILE.SPB_INSTERM_7_15,
                   SPBFILE.SPB_INSRATE_7_15,
                   SPBFILE.SPB_INSTERM_7_16,
                   SPBFILE.SPB_INSRATE_7_16,
                   SPBFILE.SPB_INSTERM_7_17,
                   SPBFILE.SPB_INSRATE_7_17,
                   SPBFILE.SPB_INSTERM_7_18,
                   SPBFILE.SPB_INSRATE_7_18,
                   SPBFILE.SPB_INSTERM_7_19,
                   SPBFILE.SPB_INSRATE_7_19,
                   SPBFILE.SPB_INSTERM_7_20,
                   SPBFILE.SPB_INSRATE_7_20,
                   SPBFILE.SPB_INSMINPREM_7,
                   SPBFILE.SPB_INSMINCHGABL_7,
                   SPBFILE.SPB_INSMAXCHGABL_7,
                   SPBFILE.SPB_INSMAXBENIF_7,
                   SPBFILE.SPB_INSMAXTERM_7,
                   ISNULL(SPBFILE.SPB_INSCHGBEN_7,''),
                   SPBFILE.SPB_INSEXCEPT_7,
                   SPBFILE.SPB_INSNOVER_7,
                   SPBFILE.SPB_INSMINAGE_7_1,
                   SPBFILE.SPB_INSMAXAGE_7_1,
                   SPBFILE.SPB_INSMAXCOV_7_1,
                   SPBFILE.SPB_INSMAXTRM_7_1,
                   SPBFILE.SPB_INSMINAGE_7_2,
                   SPBFILE.SPB_INSMAXAGE_7_2,
                   SPBFILE.SPB_INSMAXCOV_7_2,
                   SPBFILE.SPB_INSMAXTRM_7_2,
                   SPBFILE.SPB_INSMINAGE_7_3,
                   SPBFILE.SPB_INSMAXAGE_7_3,
                   SPBFILE.SPB_INSMAXCOV_7_3,
                   SPBFILE.SPB_INSMAXTRM_7_3,
                   SPBFILE.SPB_INSMINAGE_7_4,
                   SPBFILE.SPB_INSMAXAGE_7_4,
                   SPBFILE.SPB_INSMAXCOV_7_4,
                   SPBFILE.SPB_INSMAXTRM_7_4,
                   SPBFILE.SPB_INSMINAGE_7_5,
                   SPBFILE.SPB_INSMAXAGE_7_5,
                   SPBFILE.SPB_INSMAXCOV_7_5,
                   SPBFILE.SPB_INSMAXTRM_7_5,
                   ISNULL(SPBFILE.SPB_INSFEECD_7,''),
                   ISNULL(SPBFILE.SPB_INSFEE_FRMLA_7,''),
                   ISNULL(SPBFILE.SPB_INSCHGABL_FG_7,''),
                   ISNULL(SPBFILE.SPB_INSREJECT_FG_7,''),
                   ISNULL(SPBFILE.SPB_INSFINCHG_7,''),
                   SPBFILE.SPB_INSCOM_8,
                   SPBFILE.SPB_INSRATECD_8,
                   SPBFILE.SPB_INSJFAC_8,
                   SPBFILE.SPB_INSDISCNT_8,
                   ISNULL(SPBFILE.SPB_INSCHGABL_8,''),
                   SPBFILE.SPB_INSTERM_8_1,
                   SPBFILE.SPB_INSRATE_8_1,
                   SPBFILE.SPB_INSTERM_8_2,
                   SPBFILE.SPB_INSRATE_8_2,
                   SPBFILE.SPB_INSTERM_8_3,
                   SPBFILE.SPB_INSRATE_8_3,
                   SPBFILE.SPB_INSTERM_8_4,
                   SPBFILE.SPB_INSRATE_8_4,
                   SPBFILE.SPB_INSTERM_8_5,
                   SPBFILE.SPB_INSRATE_8_5,
                   SPBFILE.SPB_INSTERM_8_6,
                   SPBFILE.SPB_INSRATE_8_6,
                   SPBFILE.SPB_INSTERM_8_7,
                   SPBFILE.SPB_INSRATE_8_7,
                   SPBFILE.SPB_INSTERM_8_8,
                   SPBFILE.SPB_INSRATE_8_8,
                   SPBFILE.SPB_INSTERM_8_9,
                   SPBFILE.SPB_INSRATE_8_9,
                   SPBFILE.SPB_INSTERM_8_10,
                   SPBFILE.SPB_INSRATE_8_10,
                   SPBFILE.SPB_INSTERM_8_11,
                   SPBFILE.SPB_INSRATE_8_11,
                   SPBFILE.SPB_INSTERM_8_12,
                   SPBFILE.SPB_INSRATE_8_12,
                   SPBFILE.SPB_INSTERM_8_13,
                   SPBFILE.SPB_INSRATE_8_13,
                   SPBFILE.SPB_INSTERM_8_14,
                   SPBFILE.SPB_INSRATE_8_14,
                   SPBFILE.SPB_INSTERM_8_15,
                   SPBFILE.SPB_INSRATE_8_15,
                   SPBFILE.SPB_INSTERM_8_16,
                   SPBFILE.SPB_INSRATE_8_16,
                   SPBFILE.SPB_INSTERM_8_17,
                   SPBFILE.SPB_INSRATE_8_17,
                   SPBFILE.SPB_INSTERM_8_18,
                   SPBFILE.SPB_INSRATE_8_18,
                   SPBFILE.SPB_INSTERM_8_19,
                   SPBFILE.SPB_INSRATE_8_19,
                   SPBFILE.SPB_INSTERM_8_20,
                   SPBFILE.SPB_INSRATE_8_20,
                   SPBFILE.SPB_INSMINPREM_8,
                   SPBFILE.SPB_INSMINCHGABL_8,
                   SPBFILE.SPB_INSMAXCHGABL_8,
                   SPBFILE.SPB_INSMAXBENIF_8,
                   SPBFILE.SPB_INSMAXTERM_8,
                   ISNULL(SPBFILE.SPB_INSCHGBEN_8,''),
                   SPBFILE.SPB_INSEXCEPT_8,
                   SPBFILE.SPB_INSNOVER_8,
                   SPBFILE.SPB_INSMINAGE_8_1,
                   SPBFILE.SPB_INSMAXAGE_8_1,
                   SPBFILE.SPB_INSMAXCOV_8_1,
                   SPBFILE.SPB_INSMAXTRM_8_1,
                   SPBFILE.SPB_INSMINAGE_8_2,
                   SPBFILE.SPB_INSMAXAGE_8_2,
                   SPBFILE.SPB_INSMAXCOV_8_2,
                   SPBFILE.SPB_INSMAXTRM_8_2,
                   SPBFILE.SPB_INSMINAGE_8_3,
                   SPBFILE.SPB_INSMAXAGE_8_3,
                   SPBFILE.SPB_INSMAXCOV_8_3,
                   SPBFILE.SPB_INSMAXTRM_8_3,
                   SPBFILE.SPB_INSMINAGE_8_4,
                   SPBFILE.SPB_INSMAXAGE_8_4,
                   SPBFILE.SPB_INSMAXCOV_8_4,
                   SPBFILE.SPB_INSMAXTRM_8_4,
                   SPBFILE.SPB_INSMINAGE_8_5,
                   SPBFILE.SPB_INSMAXAGE_8_5,
                   SPBFILE.SPB_INSMAXCOV_8_5,
                   SPBFILE.SPB_INSMAXTRM_8_5,
                   ISNULL(SPBFILE.SPB_INSFEECD_8,''),
                   ISNULL(SPBFILE.SPB_INSFEE_FRMLA_8,''),
                   ISNULL(SPBFILE.SPB_INSCHGABL_FG_8,''),
                   ISNULL(SPBFILE.SPB_INSREJECT_FG_8,''),
                   ISNULL(SPBFILE.SPB_INSFINCHG_8,''),
                   SPBFILE.SPB_INSCOM_9,
                   SPBFILE.SPB_INSRATECD_9,
                   SPBFILE.SPB_INSJFAC_9,
                   SPBFILE.SPB_INSDISCNT_9,
                   ISNULL(SPBFILE.SPB_INSCHGABL_9,''),
                   SPBFILE.SPB_INSTERM_9_1,
                   SPBFILE.SPB_INSRATE_9_1,
                   SPBFILE.SPB_INSTERM_9_2,
                   SPBFILE.SPB_INSRATE_9_2,
                   SPBFILE.SPB_INSTERM_9_3,
                   SPBFILE.SPB_INSRATE_9_3,
                   SPBFILE.SPB_INSTERM_9_4,
                   SPBFILE.SPB_INSRATE_9_4,
                   SPBFILE.SPB_INSTERM_9_5,
                   SPBFILE.SPB_INSRATE_9_5,
                   SPBFILE.SPB_INSTERM_9_6,
                   SPBFILE.SPB_INSRATE_9_6,
                   SPBFILE.SPB_INSTERM_9_7,
                   SPBFILE.SPB_INSRATE_9_7,
                   SPBFILE.SPB_INSTERM_9_8,
                   SPBFILE.SPB_INSRATE_9_8,
                   SPBFILE.SPB_INSTERM_9_9,
                   SPBFILE.SPB_INSRATE_9_9,
                   SPBFILE.SPB_INSTERM_9_10,
                   SPBFILE.SPB_INSRATE_9_10,
                   SPBFILE.SPB_INSTERM_9_11,
                   SPBFILE.SPB_INSRATE_9_11,
                   SPBFILE.SPB_INSTERM_9_12,
                   SPBFILE.SPB_INSRATE_9_12,
                   SPBFILE.SPB_INSTERM_9_13,
                   SPBFILE.SPB_INSRATE_9_13,
                   SPBFILE.SPB_INSTERM_9_14,
                   SPBFILE.SPB_INSRATE_9_14,
                   SPBFILE.SPB_INSTERM_9_15,
                   SPBFILE.SPB_INSRATE_9_15,
                   SPBFILE.SPB_INSTERM_9_16,
                   SPBFILE.SPB_INSRATE_9_16,
                   SPBFILE.SPB_INSTERM_9_17,
                   SPBFILE.SPB_INSRATE_9_17,
                   SPBFILE.SPB_INSTERM_9_18,
                   SPBFILE.SPB_INSRATE_9_18,
                   SPBFILE.SPB_INSTERM_9_19,
                   SPBFILE.SPB_INSRATE_9_19,
                   SPBFILE.SPB_INSTERM_9_20,
                   SPBFILE.SPB_INSRATE_9_20,
                   SPBFILE.SPB_INSMINPREM_9,
                   SPBFILE.SPB_INSMINCHGABL_9,
                   SPBFILE.SPB_INSMAXCHGABL_9,
                   SPBFILE.SPB_INSMAXBENIF_9,
                   SPBFILE.SPB_INSMAXTERM_9,
                   ISNULL(SPBFILE.SPB_INSCHGBEN_9,''),
                   SPBFILE.SPB_INSEXCEPT_9,
                   SPBFILE.SPB_INSNOVER_9,
                   SPBFILE.SPB_INSMINAGE_9_1,
                   SPBFILE.SPB_INSMAXAGE_9_1,
                   SPBFILE.SPB_INSMAXCOV_9_1,
                   SPBFILE.SPB_INSMAXTRM_9_1,
                   SPBFILE.SPB_INSMINAGE_9_2,
                   SPBFILE.SPB_INSMAXAGE_9_2,
                   SPBFILE.SPB_INSMAXCOV_9_2,
                   SPBFILE.SPB_INSMAXTRM_9_2,
                   SPBFILE.SPB_INSMINAGE_9_3,
                   SPBFILE.SPB_INSMAXAGE_9_3,
                   SPBFILE.SPB_INSMAXCOV_9_3,
                   SPBFILE.SPB_INSMAXTRM_9_3,
                   SPBFILE.SPB_INSMINAGE_9_4,
                   SPBFILE.SPB_INSMAXAGE_9_4,
                   SPBFILE.SPB_INSMAXCOV_9_4,
                   SPBFILE.SPB_INSMAXTRM_9_4,
                   SPBFILE.SPB_INSMINAGE_9_5,
                   SPBFILE.SPB_INSMAXAGE_9_5,
                   SPBFILE.SPB_INSMAXCOV_9_5,
                   SPBFILE.SPB_INSMAXTRM_9_5,
                   ISNULL(SPBFILE.SPB_INSFEECD_9,''),
                   ISNULL(SPBFILE.SPB_INSFEE_FRMLA_9,''),
                   ISNULL(SPBFILE.SPB_INSCHGABL_FG_9,''),
                   ISNULL(SPBFILE.SPB_INSREJECT_FG_9,''),
                   ISNULL(SPBFILE.SPB_INSFINCHG_9,''),
                   SPBFILE.SPB_INSCOM_10,
                   SPBFILE.SPB_INSRATECD_10,
                   SPBFILE.SPB_INSJFAC_10,
                   SPBFILE.SPB_INSDISCNT_10,
                   ISNULL(SPBFILE.SPB_INSCHGABL_10,''),
                   SPBFILE.SPB_INSTERM_10_1,
                   SPBFILE.SPB_INSRATE_10_1,
                   SPBFILE.SPB_INSTERM_10_2,
                   SPBFILE.SPB_INSRATE_10_2,
                   SPBFILE.SPB_INSTERM_10_3,
                   SPBFILE.SPB_INSRATE_10_3,
                   SPBFILE.SPB_INSTERM_10_4,
                   SPBFILE.SPB_INSRATE_10_4,
                   SPBFILE.SPB_INSTERM_10_5,
                   SPBFILE.SPB_INSRATE_10_5,
                   SPBFILE.SPB_INSTERM_10_6,
                   SPBFILE.SPB_INSRATE_10_6,
                   SPBFILE.SPB_INSTERM_10_7,
                   SPBFILE.SPB_INSRATE_10_7,
                   SPBFILE.SPB_INSTERM_10_8,
                   SPBFILE.SPB_INSRATE_10_8,
                   SPBFILE.SPB_INSTERM_10_9,
                   SPBFILE.SPB_INSRATE_10_9,
                   SPBFILE.SPB_INSTERM_10_10,
                   SPBFILE.SPB_INSRATE_10_10,
                   SPBFILE.SPB_INSTERM_10_11,
                   SPBFILE.SPB_INSRATE_10_11,
                   SPBFILE.SPB_INSTERM_10_12,
                   SPBFILE.SPB_INSRATE_10_12,
                   SPBFILE.SPB_INSTERM_10_13,
                   SPBFILE.SPB_INSRATE_10_13,
                   SPBFILE.SPB_INSTERM_10_14,
                   SPBFILE.SPB_INSRATE_10_14,
                   SPBFILE.SPB_INSTERM_10_15,
                   SPBFILE.SPB_INSRATE_10_15,
                   SPBFILE.SPB_INSTERM_10_16,
                   SPBFILE.SPB_INSRATE_10_16,
                   SPBFILE.SPB_INSTERM_10_17,
                   SPBFILE.SPB_INSRATE_10_17,
                   SPBFILE.SPB_INSTERM_10_18,
                   SPBFILE.SPB_INSRATE_10_18,
                   SPBFILE.SPB_INSTERM_10_19,
                   SPBFILE.SPB_INSRATE_10_19,
                   SPBFILE.SPB_INSTERM_10_20,
                   SPBFILE.SPB_INSRATE_10_20,
                   SPBFILE.SPB_INSMINPREM_10,
                   SPBFILE.SPB_INSMINCHGABL_10,
                   SPBFILE.SPB_INSMAXCHGABL_10,
                   SPBFILE.SPB_INSMAXBENIF_10,
                   SPBFILE.SPB_INSMAXTERM_10,
                   ISNULL(SPBFILE.SPB_INSCHGBEN_10,''),
                   SPBFILE.SPB_INSEXCEPT_10,
                   SPBFILE.SPB_INSNOVER_10,
                   SPBFILE.SPB_INSMINAGE_10_1,
                   SPBFILE.SPB_INSMAXAGE_10_1,
                   SPBFILE.SPB_INSMAXCOV_10_1,
                   SPBFILE.SPB_INSMAXTRM_10_1,
                   SPBFILE.SPB_INSMINAGE_10_2,
                   SPBFILE.SPB_INSMAXAGE_10_2,
                   SPBFILE.SPB_INSMAXCOV_10_2,
                   SPBFILE.SPB_INSMAXTRM_10_2,
                   SPBFILE.SPB_INSMINAGE_10_3,
                   SPBFILE.SPB_INSMAXAGE_10_3,
                   SPBFILE.SPB_INSMAXCOV_10_3,
                   SPBFILE.SPB_INSMAXTRM_10_3,
                   SPBFILE.SPB_INSMINAGE_10_4,
                   SPBFILE.SPB_INSMAXAGE_10_4,
                   SPBFILE.SPB_INSMAXCOV_10_4,
                   SPBFILE.SPB_INSMAXTRM_10_4,
                   SPBFILE.SPB_INSMINAGE_10_5,
                   SPBFILE.SPB_INSMAXAGE_10_5,
                   SPBFILE.SPB_INSMAXCOV_10_5,
                   SPBFILE.SPB_INSMAXTRM_10_5,
                   ISNULL(SPBFILE.SPB_INSFEECD_10,''),
                   ISNULL(SPBFILE.SPB_INSFEE_FRMLA_10,''),
                   ISNULL(SPBFILE.SPB_INSCHGABL_FG_10,''),
                   ISNULL(SPBFILE.SPB_INSREJECT_FG_10,''),
                   ISNULL(SPBFILE.SPB_INSFINCHG_10,''),
                   SPBFILE.SPB_INSCOM_11,
                   SPBFILE.SPB_INSRATECD_11,
                   SPBFILE.SPB_INSJFAC_11,
                   SPBFILE.SPB_INSDISCNT_11,
                   ISNULL(SPBFILE.SPB_INSCHGABL_11,''),
                   SPBFILE.SPB_INSTERM_11_1,
                   SPBFILE.SPB_INSRATE_11_1,
                   SPBFILE.SPB_INSTERM_11_2,
                   SPBFILE.SPB_INSRATE_11_2,
                   SPBFILE.SPB_INSTERM_11_3,
                   SPBFILE.SPB_INSRATE_11_3,
                   SPBFILE.SPB_INSTERM_11_4,
                   SPBFILE.SPB_INSRATE_11_4,
                   SPBFILE.SPB_INSTERM_11_5,
                   SPBFILE.SPB_INSRATE_11_5,
                   SPBFILE.SPB_INSTERM_11_6,
                   SPBFILE.SPB_INSRATE_11_6,
                   SPBFILE.SPB_INSTERM_11_7,
                   SPBFILE.SPB_INSRATE_11_7,
                   SPBFILE.SPB_INSTERM_11_8,
                   SPBFILE.SPB_INSRATE_11_8,
                   SPBFILE.SPB_INSTERM_11_9,
                   SPBFILE.SPB_INSRATE_11_9,
                   SPBFILE.SPB_INSTERM_11_10,
                   SPBFILE.SPB_INSRATE_11_10,
                   SPBFILE.SPB_INSTERM_11_11,
                   SPBFILE.SPB_INSRATE_11_11,
                   SPBFILE.SPB_INSTERM_11_12,
                   SPBFILE.SPB_INSRATE_11_12,
                   SPBFILE.SPB_INSTERM_11_13,
                   SPBFILE.SPB_INSRATE_11_13,
                   SPBFILE.SPB_INSTERM_11_14,
                   SPBFILE.SPB_INSRATE_11_14,
                   SPBFILE.SPB_INSTERM_11_15,
                   SPBFILE.SPB_INSRATE_11_15,
                   SPBFILE.SPB_INSTERM_11_16,
                   SPBFILE.SPB_INSRATE_11_16,
                   SPBFILE.SPB_INSTERM_11_17,
                   SPBFILE.SPB_INSRATE_11_17,
                   SPBFILE.SPB_INSTERM_11_18,
                   SPBFILE.SPB_INSRATE_11_18,
                   SPBFILE.SPB_INSTERM_11_19,
                   SPBFILE.SPB_INSRATE_11_19,
                   SPBFILE.SPB_INSTERM_11_20,
                   SPBFILE.SPB_INSRATE_11_20,
                   SPBFILE.SPB_INSMINPREM_11,
                   SPBFILE.SPB_INSMINCHGABL_11,
                   SPBFILE.SPB_INSMAXCHGABL_11,
                   SPBFILE.SPB_INSMAXBENIF_11,
                   SPBFILE.SPB_INSMAXTERM_11,
                   ISNULL(SPBFILE.SPB_INSCHGBEN_11,''),
                   SPBFILE.SPB_INSEXCEPT_11,
                   SPBFILE.SPB_INSNOVER_11,
                   SPBFILE.SPB_INSMINAGE_11_1,
                   SPBFILE.SPB_INSMAXAGE_11_1,
                   SPBFILE.SPB_INSMAXCOV_11_1,
                   SPBFILE.SPB_INSMAXTRM_11_1,
                   SPBFILE.SPB_INSMINAGE_11_2,
                   SPBFILE.SPB_INSMAXAGE_11_2,
                   SPBFILE.SPB_INSMAXCOV_11_2,
                   SPBFILE.SPB_INSMAXTRM_11_2,
                   SPBFILE.SPB_INSMINAGE_11_3,
                   SPBFILE.SPB_INSMAXAGE_11_3,
                   SPBFILE.SPB_INSMAXCOV_11_3,
                   SPBFILE.SPB_INSMAXTRM_11_3,
                   SPBFILE.SPB_INSMINAGE_11_4,
                   SPBFILE.SPB_INSMAXAGE_11_4,
                   SPBFILE.SPB_INSMAXCOV_11_4,
                   SPBFILE.SPB_INSMAXTRM_11_4,
                   SPBFILE.SPB_INSMINAGE_11_5,
                   SPBFILE.SPB_INSMAXAGE_11_5,
                   SPBFILE.SPB_INSMAXCOV_11_5,
                   SPBFILE.SPB_INSMAXTRM_11_5,
                   ISNULL(SPBFILE.SPB_INSFEECD_11,''),
                   ISNULL(SPBFILE.SPB_INSFEE_FRMLA_11,''),
                   ISNULL(SPBFILE.SPB_INSCHGABL_FG_11,''),
                   ISNULL(SPBFILE.SPB_INSREJECT_FG_11,''),
                   ISNULL(SPBFILE.SPB_INSFINCHG_11,''),
                   SPBFILE.SPB_INSCOM_12,
                   SPBFILE.SPB_INSRATECD_12,
                   SPBFILE.SPB_INSJFAC_12,
                   SPBFILE.SPB_INSDISCNT_12,
                   ISNULL(SPBFILE.SPB_INSCHGABL_12,''),
                   SPBFILE.SPB_INSTERM_12_1,
                   SPBFILE.SPB_INSRATE_12_1,
                   SPBFILE.SPB_INSTERM_12_2,
                   SPBFILE.SPB_INSRATE_12_2,
                   SPBFILE.SPB_INSTERM_12_3,
                   SPBFILE.SPB_INSRATE_12_3,
                   SPBFILE.SPB_INSTERM_12_4,
                   SPBFILE.SPB_INSRATE_12_4,
                   SPBFILE.SPB_INSTERM_12_5,
                   SPBFILE.SPB_INSRATE_12_5,
                   SPBFILE.SPB_INSTERM_12_6,
                   SPBFILE.SPB_INSRATE_12_6,
                   SPBFILE.SPB_INSTERM_12_7,
                   SPBFILE.SPB_INSRATE_12_7,
                   SPBFILE.SPB_INSTERM_12_8,
                   SPBFILE.SPB_INSRATE_12_8,
                   SPBFILE.SPB_INSTERM_12_9,
                   SPBFILE.SPB_INSRATE_12_9,
                   SPBFILE.SPB_INSTERM_12_10,
                   SPBFILE.SPB_INSRATE_12_10,
                   SPBFILE.SPB_INSTERM_12_11,
                   SPBFILE.SPB_INSRATE_12_11,
                   SPBFILE.SPB_INSTERM_12_12,
                   SPBFILE.SPB_INSRATE_12_12,
                   SPBFILE.SPB_INSTERM_12_13,
                   SPBFILE.SPB_INSRATE_12_13,
                   SPBFILE.SPB_INSTERM_12_14,
                   SPBFILE.SPB_INSRATE_12_14,
                   SPBFILE.SPB_INSTERM_12_15,
                   SPBFILE.SPB_INSRATE_12_15,
                   SPBFILE.SPB_INSTERM_12_16,
                   SPBFILE.SPB_INSRATE_12_16,
                   SPBFILE.SPB_INSTERM_12_17,
                   SPBFILE.SPB_INSRATE_12_17,
                   SPBFILE.SPB_INSTERM_12_18,
                   SPBFILE.SPB_INSRATE_12_18,
                   SPBFILE.SPB_INSTERM_12_19,
                   SPBFILE.SPB_INSRATE_12_19,
                   SPBFILE.SPB_INSTERM_12_20,
                   SPBFILE.SPB_INSRATE_12_20,
                   SPBFILE.SPB_INSMINPREM_12,
                   SPBFILE.SPB_INSMINCHGABL_12,
                   SPBFILE.SPB_INSMAXCHGABL_12,
                   SPBFILE.SPB_INSMAXBENIF_12,
                   SPBFILE.SPB_INSMAXTERM_12,
                   ISNULL(SPBFILE.SPB_INSCHGBEN_12,''),
                   SPBFILE.SPB_INSEXCEPT_12,
                   SPBFILE.SPB_INSNOVER_12,
                   SPBFILE.SPB_INSMINAGE_12_1,
                   SPBFILE.SPB_INSMAXAGE_12_1,
                   SPBFILE.SPB_INSMAXCOV_12_1,
                   SPBFILE.SPB_INSMAXTRM_12_1,
                   SPBFILE.SPB_INSMINAGE_12_2,
                   SPBFILE.SPB_INSMAXAGE_12_2,
                   SPBFILE.SPB_INSMAXCOV_12_2,
                   SPBFILE.SPB_INSMAXTRM_12_2,
                   SPBFILE.SPB_INSMINAGE_12_3,
                   SPBFILE.SPB_INSMAXAGE_12_3,
                   SPBFILE.SPB_INSMAXCOV_12_3,
                   SPBFILE.SPB_INSMAXTRM_12_3,
                   SPBFILE.SPB_INSMINAGE_12_4,
                   SPBFILE.SPB_INSMAXAGE_12_4,
                   SPBFILE.SPB_INSMAXCOV_12_4,
                   SPBFILE.SPB_INSMAXTRM_12_4,
                   SPBFILE.SPB_INSMINAGE_12_5,
                   SPBFILE.SPB_INSMAXAGE_12_5,
                   SPBFILE.SPB_INSMAXCOV_12_5,
                   SPBFILE.SPB_INSMAXTRM_12_5,
                   ISNULL(SPBFILE.SPB_INSFEECD_12,''),
                   ISNULL(SPBFILE.SPB_INSFEE_FRMLA_12,''),
                   ISNULL(SPBFILE.SPB_INSCHGABL_FG_12,''),
                   ISNULL(SPBFILE.SPB_INSREJECT_FG_12,''),
                   ISNULL(SPBFILE.SPB_INSFINCHG_12,'')
            INTO :QSPB-REC
            FROM DBO.SPBFILE
            WHERE SPBFILE.SPB_ORGST = :QSPB-ORGST
              AND SPBFILE.SPB_SPRCLASS = :QSPB-SPRCLASS
              AND SPBFILE.SPB_SUBCLASS = :QSPB-SUBCLASS
              AND SPBFILE.SPB_LAWCODE = :QSPB-LAWCODE 
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-SPB1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-SPB-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-SPB1-FILE.
           PERFORM CLOSE-IT.

      *=================================================================
       LOAD-SPC1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( SP-PATH-OVERRIDE = "Y" )
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( SP-PATH-OVERRIDE = SPACES )
              MOVE EXT-FILPATH-BASE        TO SPC-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO SPC-ALL-MACHINE
              MOVE SPC-ALL-PATH             TO SPC-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( SP-PATH-OVERRIDE = "B" )
              MOVE FILPATH-BASE            TO SPC-ALL-BASE
              MOVE FILPATH-MACHINE         TO SPC-ALL-MACHINE
              MOVE SPC-ALL-PATH             TO SPC-PATH.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-SPCFILE NOT = "S" )
              MOVE SPC-PATH-BEG          TO SPC-SELECT-PATH-BEG
              MOVE SPC-PATH-FILE         TO SPC-SELECT-PATH-FILE
           ELSE
              MOVE SPC-PATH-SQL          TO SPC-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-SPC1-FILE.
           PERFORM LOAD-SPC1-FILE.
           PERFORM OPEN-IT.
           MOVE SPC-SELECT-PATH TO E-FILE.

      *-----------------------------------------------------------------
       READ-SPC1-FILE.
           PERFORM READ-IT.
           MOVE SPC-SELECT-PATH TO E-FILE.
           MOVE SPC1-KEY TO E-KEYX.

           MOVE SPC-ORGST            TO QSPC-ORGST.
           MOVE SPC-SPRCLASS         TO QSPC-SPRCLASS.
           MOVE SPC-SUBCLASS         TO QSPC-SUBCLASS.
           MOVE SPC-LAWCODE          TO QSPC-LAWCODE.

           EXEC SQL
            SELECT ISNULL(SPCFILE.SPC_ORGST,''),
                   SPCFILE.SPC_SPRCLASS,
                   SPCFILE.SPC_SUBCLASS,
                   SPCFILE.SPC_LAWCODE,
                   SPCFILE.SPC_INSEARN_ORDER_1,
                   SPCFILE.SPC_INSEARN_ORDER_2,
                   SPCFILE.SPC_INSEARN_ORDER_3,
                   SPCFILE.SPC_INSEARN_ORDER_4,
                   SPCFILE.SPC_INSEARN_ORDER_5,
                   SPCFILE.SPC_INSEARN_ORDER_6,
                   SPCFILE.SPC_INSEARN_ORDER_7,
                   SPCFILE.SPC_INSEARN_ORDER_8,
                   SPCFILE.SPC_INSEARN_ORDER_9,
                   SPCFILE.SPC_INSEARN_ORDER_10,
                   SPCFILE.SPC_INSEARN_ORDER_11,
                   SPCFILE.SPC_INSEARN_ORDER_12,
                   ISNULL(SPCFILE.SPC_INSAH_JOINT_FG,''),
                   ISNULL(SPCFILE.SPC_INS_O1_EDIT,''),
                   ISNULL(SPCFILE.SPC_INS_O2_EDIT,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA_1,''),
                   ISNULL(SPCFILE.SPC_RBDTE_1_1,''),
                   SPCFILE.SPC_RBDAYS_1_1,
                   SPCFILE.SPC_RBYRTYPE_1_1,
                   ISNULL(SPCFILE.SPC_RBDTE_1_2,''),
                   SPCFILE.SPC_RBDAYS_1_2,
                   SPCFILE.SPC_RBYRTYPE_1_2,
                   SPCFILE.SPC_RBMINRET_1,
                   SPCFILE.SPC_RBNOMON_1,
                   SPCFILE.SPC_RBDEDCHG_1,
                   SPCFILE.SPC_RBRECAST_1,
                   SPCFILE.SPC_RBEARLY_1,
                   SPCFILE.SPC_RBDEFER_1,
                   SPCFILE.SPC_RBPCT_1,
                   SPCFILE.SPC_RBREDUC_1,
                   SPCFILE.SPC_RBMIN_1,
                   SPCFILE.SPC_RBSPOPT1_1,
                   SPCFILE.SPC_RBSPOPT2_1,
                   SPCFILE.SPC_RBMANAGER_1,
                   ISNULL(SPCFILE.SPC_RBUNITPER_CD_1,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA2_1,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA_2,''),
                   ISNULL(SPCFILE.SPC_RBDTE_2_1,''),
                   SPCFILE.SPC_RBDAYS_2_1,
                   SPCFILE.SPC_RBYRTYPE_2_1,
                   ISNULL(SPCFILE.SPC_RBDTE_2_2,''),
                   SPCFILE.SPC_RBDAYS_2_2,
                   SPCFILE.SPC_RBYRTYPE_2_2,
                   SPCFILE.SPC_RBMINRET_2,
                   SPCFILE.SPC_RBNOMON_2,
                   SPCFILE.SPC_RBDEDCHG_2,
                   SPCFILE.SPC_RBRECAST_2,
                   SPCFILE.SPC_RBEARLY_2,
                   SPCFILE.SPC_RBDEFER_2,
                   SPCFILE.SPC_RBPCT_2,
                   SPCFILE.SPC_RBREDUC_2,
                   SPCFILE.SPC_RBMIN_2,
                   SPCFILE.SPC_RBSPOPT1_2,
                   SPCFILE.SPC_RBSPOPT2_2,
                   SPCFILE.SPC_RBMANAGER_2,
                   ISNULL(SPCFILE.SPC_RBUNITPER_CD_2,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA2_2,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA_3,''),
                   ISNULL(SPCFILE.SPC_RBDTE_3_1,''),
                   SPCFILE.SPC_RBDAYS_3_1,
                   SPCFILE.SPC_RBYRTYPE_3_1,
                   ISNULL(SPCFILE.SPC_RBDTE_3_2,''),
                   SPCFILE.SPC_RBDAYS_3_2,
                   SPCFILE.SPC_RBYRTYPE_3_2,
                   SPCFILE.SPC_RBMINRET_3,
                   SPCFILE.SPC_RBNOMON_3,
                   SPCFILE.SPC_RBDEDCHG_3,
                   SPCFILE.SPC_RBRECAST_3,
                   SPCFILE.SPC_RBEARLY_3,
                   SPCFILE.SPC_RBDEFER_3,
                   SPCFILE.SPC_RBPCT_3,
                   SPCFILE.SPC_RBREDUC_3,
                   SPCFILE.SPC_RBMIN_3,
                   SPCFILE.SPC_RBSPOPT1_3,
                   SPCFILE.SPC_RBSPOPT2_3,
                   SPCFILE.SPC_RBMANAGER_3,
                   ISNULL(SPCFILE.SPC_RBUNITPER_CD_3,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA2_3,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA_4,''),
                   ISNULL(SPCFILE.SPC_RBDTE_4_1,''),
                   SPCFILE.SPC_RBDAYS_4_1,
                   SPCFILE.SPC_RBYRTYPE_4_1,
                   ISNULL(SPCFILE.SPC_RBDTE_4_2,''),
                   SPCFILE.SPC_RBDAYS_4_2,
                   SPCFILE.SPC_RBYRTYPE_4_2,
                   SPCFILE.SPC_RBMINRET_4,
                   SPCFILE.SPC_RBNOMON_4,
                   SPCFILE.SPC_RBDEDCHG_4,
                   SPCFILE.SPC_RBRECAST_4,
                   SPCFILE.SPC_RBEARLY_4,
                   SPCFILE.SPC_RBDEFER_4,
                   SPCFILE.SPC_RBPCT_4,
                   SPCFILE.SPC_RBREDUC_4,
                   SPCFILE.SPC_RBMIN_4,
                   SPCFILE.SPC_RBSPOPT1_4,
                   SPCFILE.SPC_RBSPOPT2_4,
                   SPCFILE.SPC_RBMANAGER_4,
                   ISNULL(SPCFILE.SPC_RBUNITPER_CD_4,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA2_4,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA_5,''),
                   ISNULL(SPCFILE.SPC_RBDTE_5_1,''),
                   SPCFILE.SPC_RBDAYS_5_1,
                   SPCFILE.SPC_RBYRTYPE_5_1,
                   ISNULL(SPCFILE.SPC_RBDTE_5_2,''),
                   SPCFILE.SPC_RBDAYS_5_2,
                   SPCFILE.SPC_RBYRTYPE_5_2,
                   SPCFILE.SPC_RBMINRET_5,
                   SPCFILE.SPC_RBNOMON_5,
                   SPCFILE.SPC_RBDEDCHG_5,
                   SPCFILE.SPC_RBRECAST_5,
                   SPCFILE.SPC_RBEARLY_5,
                   SPCFILE.SPC_RBDEFER_5,
                   SPCFILE.SPC_RBPCT_5,
                   SPCFILE.SPC_RBREDUC_5,
                   SPCFILE.SPC_RBMIN_5,
                   SPCFILE.SPC_RBSPOPT1_5,
                   SPCFILE.SPC_RBSPOPT2_5,
                   SPCFILE.SPC_RBMANAGER_5,
                   ISNULL(SPCFILE.SPC_RBUNITPER_CD_5,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA2_5,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA_6,''),
                   ISNULL(SPCFILE.SPC_RBDTE_6_1,''),
                   SPCFILE.SPC_RBDAYS_6_1,
                   SPCFILE.SPC_RBYRTYPE_6_1,
                   ISNULL(SPCFILE.SPC_RBDTE_6_2,''),
                   SPCFILE.SPC_RBDAYS_6_2,
                   SPCFILE.SPC_RBYRTYPE_6_2,
                   SPCFILE.SPC_RBMINRET_6,
                   SPCFILE.SPC_RBNOMON_6,
                   SPCFILE.SPC_RBDEDCHG_6,
                   SPCFILE.SPC_RBRECAST_6,
                   SPCFILE.SPC_RBEARLY_6,
                   SPCFILE.SPC_RBDEFER_6,
                   SPCFILE.SPC_RBPCT_6,
                   SPCFILE.SPC_RBREDUC_6,
                   SPCFILE.SPC_RBMIN_6,
                   SPCFILE.SPC_RBSPOPT1_6,
                   SPCFILE.SPC_RBSPOPT2_6,
                   SPCFILE.SPC_RBMANAGER_6,
                   ISNULL(SPCFILE.SPC_RBUNITPER_CD_6,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA2_6,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA_7,''),
                   ISNULL(SPCFILE.SPC_RBDTE_7_1,''),
                   SPCFILE.SPC_RBDAYS_7_1,
                   SPCFILE.SPC_RBYRTYPE_7_1,
                   ISNULL(SPCFILE.SPC_RBDTE_7_2,''),
                   SPCFILE.SPC_RBDAYS_7_2,
                   SPCFILE.SPC_RBYRTYPE_7_2,
                   SPCFILE.SPC_RBMINRET_7,
                   SPCFILE.SPC_RBNOMON_7,
                   SPCFILE.SPC_RBDEDCHG_7,
                   SPCFILE.SPC_RBRECAST_7,
                   SPCFILE.SPC_RBEARLY_7,
                   SPCFILE.SPC_RBDEFER_7,
                   SPCFILE.SPC_RBPCT_7,
                   SPCFILE.SPC_RBREDUC_7,
                   SPCFILE.SPC_RBMIN_7,
                   SPCFILE.SPC_RBSPOPT1_7,
                   SPCFILE.SPC_RBSPOPT2_7,
                   SPCFILE.SPC_RBMANAGER_7,
                   ISNULL(SPCFILE.SPC_RBUNITPER_CD_7,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA2_7,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA_8,''),
                   ISNULL(SPCFILE.SPC_RBDTE_8_1,''),
                   SPCFILE.SPC_RBDAYS_8_1,
                   SPCFILE.SPC_RBYRTYPE_8_1,
                   ISNULL(SPCFILE.SPC_RBDTE_8_2,''),
                   SPCFILE.SPC_RBDAYS_8_2,
                   SPCFILE.SPC_RBYRTYPE_8_2,
                   SPCFILE.SPC_RBMINRET_8,
                   SPCFILE.SPC_RBNOMON_8,
                   SPCFILE.SPC_RBDEDCHG_8,
                   SPCFILE.SPC_RBRECAST_8,
                   SPCFILE.SPC_RBEARLY_8,
                   SPCFILE.SPC_RBDEFER_8,
                   SPCFILE.SPC_RBPCT_8,
                   SPCFILE.SPC_RBREDUC_8,
                   SPCFILE.SPC_RBMIN_8,
                   SPCFILE.SPC_RBSPOPT1_8,
                   SPCFILE.SPC_RBSPOPT2_8,
                   SPCFILE.SPC_RBMANAGER_8,
                   ISNULL(SPCFILE.SPC_RBUNITPER_CD_8,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA2_8,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA_9,''),
                   ISNULL(SPCFILE.SPC_RBDTE_9_1,''),
                   SPCFILE.SPC_RBDAYS_9_1,
                   SPCFILE.SPC_RBYRTYPE_9_1,
                   ISNULL(SPCFILE.SPC_RBDTE_9_2,''),
                   SPCFILE.SPC_RBDAYS_9_2,
                   SPCFILE.SPC_RBYRTYPE_9_2,
                   SPCFILE.SPC_RBMINRET_9,
                   SPCFILE.SPC_RBNOMON_9,
                   SPCFILE.SPC_RBDEDCHG_9,
                   SPCFILE.SPC_RBRECAST_9,
                   SPCFILE.SPC_RBEARLY_9,
                   SPCFILE.SPC_RBDEFER_9,
                   SPCFILE.SPC_RBPCT_9,
                   SPCFILE.SPC_RBREDUC_9,
                   SPCFILE.SPC_RBMIN_9,
                   SPCFILE.SPC_RBSPOPT1_9,
                   SPCFILE.SPC_RBSPOPT2_9,
                   SPCFILE.SPC_RBMANAGER_9,
                   ISNULL(SPCFILE.SPC_RBUNITPER_CD_9,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA2_9,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA_10,''),
                   ISNULL(SPCFILE.SPC_RBDTE_10_1,''),
                   SPCFILE.SPC_RBDAYS_10_1,
                   SPCFILE.SPC_RBYRTYPE_10_1,
                   ISNULL(SPCFILE.SPC_RBDTE_10_2,''),
                   SPCFILE.SPC_RBDAYS_10_2,
                   SPCFILE.SPC_RBYRTYPE_10_2,
                   SPCFILE.SPC_RBMINRET_10,
                   SPCFILE.SPC_RBNOMON_10,
                   SPCFILE.SPC_RBDEDCHG_10,
                   SPCFILE.SPC_RBRECAST_10,
                   SPCFILE.SPC_RBEARLY_10,
                   SPCFILE.SPC_RBDEFER_10,
                   SPCFILE.SPC_RBPCT_10,
                   SPCFILE.SPC_RBREDUC_10,
                   SPCFILE.SPC_RBMIN_10,
                   SPCFILE.SPC_RBSPOPT1_10,
                   SPCFILE.SPC_RBSPOPT2_10,
                   SPCFILE.SPC_RBMANAGER_10,
                   ISNULL(SPCFILE.SPC_RBUNITPER_CD_10,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA2_10,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA_11,''),
                   ISNULL(SPCFILE.SPC_RBDTE_11_1,''),
                   SPCFILE.SPC_RBDAYS_11_1,
                   SPCFILE.SPC_RBYRTYPE_11_1,
                   ISNULL(SPCFILE.SPC_RBDTE_11_2,''),
                   SPCFILE.SPC_RBDAYS_11_2,
                   SPCFILE.SPC_RBYRTYPE_11_2,
                   SPCFILE.SPC_RBMINRET_11,
                   SPCFILE.SPC_RBNOMON_11,
                   SPCFILE.SPC_RBDEDCHG_11,
                   SPCFILE.SPC_RBRECAST_11,
                   SPCFILE.SPC_RBEARLY_11,
                   SPCFILE.SPC_RBDEFER_11,
                   SPCFILE.SPC_RBPCT_11,
                   SPCFILE.SPC_RBREDUC_11,
                   SPCFILE.SPC_RBMIN_11,
                   SPCFILE.SPC_RBSPOPT1_11,
                   SPCFILE.SPC_RBSPOPT2_11,
                   SPCFILE.SPC_RBMANAGER_11,
                   ISNULL(SPCFILE.SPC_RBUNITPER_CD_11,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA2_11,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA_12,''),
                   ISNULL(SPCFILE.SPC_RBDTE_12_1,''),
                   SPCFILE.SPC_RBDAYS_12_1,
                   SPCFILE.SPC_RBYRTYPE_12_1,
                   ISNULL(SPCFILE.SPC_RBDTE_12_2,''),
                   SPCFILE.SPC_RBDAYS_12_2,
                   SPCFILE.SPC_RBYRTYPE_12_2,
                   SPCFILE.SPC_RBMINRET_12,
                   SPCFILE.SPC_RBNOMON_12,
                   SPCFILE.SPC_RBDEDCHG_12,
                   SPCFILE.SPC_RBRECAST_12,
                   SPCFILE.SPC_RBEARLY_12,
                   SPCFILE.SPC_RBDEFER_12,
                   SPCFILE.SPC_RBPCT_12,
                   SPCFILE.SPC_RBREDUC_12,
                   SPCFILE.SPC_RBMIN_12,
                   SPCFILE.SPC_RBSPOPT1_12,
                   SPCFILE.SPC_RBSPOPT2_12,
                   SPCFILE.SPC_RBMANAGER_12,
                   ISNULL(SPCFILE.SPC_RBUNITPER_CD_12,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA2_12,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA_13,''),
                   ISNULL(SPCFILE.SPC_RBDTE_13_1,''),
                   SPCFILE.SPC_RBDAYS_13_1,
                   SPCFILE.SPC_RBYRTYPE_13_1,
                   ISNULL(SPCFILE.SPC_RBDTE_13_2,''),
                   SPCFILE.SPC_RBDAYS_13_2,
                   SPCFILE.SPC_RBYRTYPE_13_2,
                   SPCFILE.SPC_RBMINRET_13,
                   SPCFILE.SPC_RBNOMON_13,
                   SPCFILE.SPC_RBDEDCHG_13,
                   SPCFILE.SPC_RBRECAST_13,
                   SPCFILE.SPC_RBEARLY_13,
                   SPCFILE.SPC_RBDEFER_13,
                   SPCFILE.SPC_RBPCT_13,
                   SPCFILE.SPC_RBREDUC_13,
                   SPCFILE.SPC_RBMIN_13,
                   SPCFILE.SPC_RBSPOPT1_13,
                   SPCFILE.SPC_RBSPOPT2_13,
                   SPCFILE.SPC_RBMANAGER_13,
                   ISNULL(SPCFILE.SPC_RBUNITPER_CD_13,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA2_13,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA_14,''),
                   ISNULL(SPCFILE.SPC_RBDTE_14_1,''),
                   SPCFILE.SPC_RBDAYS_14_1,
                   SPCFILE.SPC_RBYRTYPE_14_1,
                   ISNULL(SPCFILE.SPC_RBDTE_14_2,''),
                   SPCFILE.SPC_RBDAYS_14_2,
                   SPCFILE.SPC_RBYRTYPE_14_2,
                   SPCFILE.SPC_RBMINRET_14,
                   SPCFILE.SPC_RBNOMON_14,
                   SPCFILE.SPC_RBDEDCHG_14,
                   SPCFILE.SPC_RBRECAST_14,
                   SPCFILE.SPC_RBEARLY_14,
                   SPCFILE.SPC_RBDEFER_14,
                   SPCFILE.SPC_RBPCT_14,
                   SPCFILE.SPC_RBREDUC_14,
                   SPCFILE.SPC_RBMIN_14,
                   SPCFILE.SPC_RBSPOPT1_14,
                   SPCFILE.SPC_RBSPOPT2_14,
                   SPCFILE.SPC_RBMANAGER_14,
                   ISNULL(SPCFILE.SPC_RBUNITPER_CD_14,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA2_14,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA_15,''),
                   ISNULL(SPCFILE.SPC_RBDTE_15_1,''),
                   SPCFILE.SPC_RBDAYS_15_1,
                   SPCFILE.SPC_RBYRTYPE_15_1,
                   ISNULL(SPCFILE.SPC_RBDTE_15_2,''),
                   SPCFILE.SPC_RBDAYS_15_2,
                   SPCFILE.SPC_RBYRTYPE_15_2,
                   SPCFILE.SPC_RBMINRET_15,
                   SPCFILE.SPC_RBNOMON_15,
                   SPCFILE.SPC_RBDEDCHG_15,
                   SPCFILE.SPC_RBRECAST_15,
                   SPCFILE.SPC_RBEARLY_15,
                   SPCFILE.SPC_RBDEFER_15,
                   SPCFILE.SPC_RBPCT_15,
                   SPCFILE.SPC_RBREDUC_15,
                   SPCFILE.SPC_RBMIN_15,
                   SPCFILE.SPC_RBSPOPT1_15,
                   SPCFILE.SPC_RBSPOPT2_15,
                   SPCFILE.SPC_RBMANAGER_15,
                   ISNULL(SPCFILE.SPC_RBUNITPER_CD_15,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA2_15,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA_16,''),
                   ISNULL(SPCFILE.SPC_RBDTE_16_1,''),
                   SPCFILE.SPC_RBDAYS_16_1,
                   SPCFILE.SPC_RBYRTYPE_16_1,
                   ISNULL(SPCFILE.SPC_RBDTE_16_2,''),
                   SPCFILE.SPC_RBDAYS_16_2,
                   SPCFILE.SPC_RBYRTYPE_16_2,
                   SPCFILE.SPC_RBMINRET_16,
                   SPCFILE.SPC_RBNOMON_16,
                   SPCFILE.SPC_RBDEDCHG_16,
                   SPCFILE.SPC_RBRECAST_16,
                   SPCFILE.SPC_RBEARLY_16,
                   SPCFILE.SPC_RBDEFER_16,
                   SPCFILE.SPC_RBPCT_16,
                   SPCFILE.SPC_RBREDUC_16,
                   SPCFILE.SPC_RBMIN_16,
                   SPCFILE.SPC_RBSPOPT1_16,
                   SPCFILE.SPC_RBSPOPT2_16,
                   SPCFILE.SPC_RBMANAGER_16,
                   ISNULL(SPCFILE.SPC_RBUNITPER_CD_16,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA2_16,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA_17,''),
                   ISNULL(SPCFILE.SPC_RBDTE_17_1,''),
                   SPCFILE.SPC_RBDAYS_17_1,
                   SPCFILE.SPC_RBYRTYPE_17_1,
                   ISNULL(SPCFILE.SPC_RBDTE_17_2,''),
                   SPCFILE.SPC_RBDAYS_17_2,
                   SPCFILE.SPC_RBYRTYPE_17_2,
                   SPCFILE.SPC_RBMINRET_17,
                   SPCFILE.SPC_RBNOMON_17,
                   SPCFILE.SPC_RBDEDCHG_17,
                   SPCFILE.SPC_RBRECAST_17,
                   SPCFILE.SPC_RBEARLY_17,
                   SPCFILE.SPC_RBDEFER_17,
                   SPCFILE.SPC_RBPCT_17,
                   SPCFILE.SPC_RBREDUC_17,
                   SPCFILE.SPC_RBMIN_17,
                   SPCFILE.SPC_RBSPOPT1_17,
                   SPCFILE.SPC_RBSPOPT2_17,
                   SPCFILE.SPC_RBMANAGER_17,
                   ISNULL(SPCFILE.SPC_RBUNITPER_CD_17,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA2_17,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA_18,''),
                   ISNULL(SPCFILE.SPC_RBDTE_18_1,''),
                   SPCFILE.SPC_RBDAYS_18_1,
                   SPCFILE.SPC_RBYRTYPE_18_1,
                   ISNULL(SPCFILE.SPC_RBDTE_18_2,''),
                   SPCFILE.SPC_RBDAYS_18_2,
                   SPCFILE.SPC_RBYRTYPE_18_2,
                   SPCFILE.SPC_RBMINRET_18,
                   SPCFILE.SPC_RBNOMON_18,
                   SPCFILE.SPC_RBDEDCHG_18,
                   SPCFILE.SPC_RBRECAST_18,
                   SPCFILE.SPC_RBEARLY_18,
                   SPCFILE.SPC_RBDEFER_18,
                   SPCFILE.SPC_RBPCT_18,
                   SPCFILE.SPC_RBREDUC_18,
                   SPCFILE.SPC_RBMIN_18,
                   SPCFILE.SPC_RBSPOPT1_18,
                   SPCFILE.SPC_RBSPOPT2_18,
                   SPCFILE.SPC_RBMANAGER_18,
                   ISNULL(SPCFILE.SPC_RBUNITPER_CD_18,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA2_18,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA_19,''),
                   ISNULL(SPCFILE.SPC_RBDTE_19_1,''),
                   SPCFILE.SPC_RBDAYS_19_1,
                   SPCFILE.SPC_RBYRTYPE_19_1,
                   ISNULL(SPCFILE.SPC_RBDTE_19_2,''),
                   SPCFILE.SPC_RBDAYS_19_2,
                   SPCFILE.SPC_RBYRTYPE_19_2,
                   SPCFILE.SPC_RBMINRET_19,
                   SPCFILE.SPC_RBNOMON_19,
                   SPCFILE.SPC_RBDEDCHG_19,
                   SPCFILE.SPC_RBRECAST_19,
                   SPCFILE.SPC_RBEARLY_19,
                   SPCFILE.SPC_RBDEFER_19,
                   SPCFILE.SPC_RBPCT_19,
                   SPCFILE.SPC_RBREDUC_19,
                   SPCFILE.SPC_RBMIN_19,
                   SPCFILE.SPC_RBSPOPT1_19,
                   SPCFILE.SPC_RBSPOPT2_19,
                   SPCFILE.SPC_RBMANAGER_19,
                   ISNULL(SPCFILE.SPC_RBUNITPER_CD_19,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA2_19,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA_20,''),
                   ISNULL(SPCFILE.SPC_RBDTE_20_1,''),
                   SPCFILE.SPC_RBDAYS_20_1,
                   SPCFILE.SPC_RBYRTYPE_20_1,
                   ISNULL(SPCFILE.SPC_RBDTE_20_2,''),
                   SPCFILE.SPC_RBDAYS_20_2,
                   SPCFILE.SPC_RBYRTYPE_20_2,
                   SPCFILE.SPC_RBMINRET_20,
                   SPCFILE.SPC_RBNOMON_20,
                   SPCFILE.SPC_RBDEDCHG_20,
                   SPCFILE.SPC_RBRECAST_20,
                   SPCFILE.SPC_RBEARLY_20,
                   SPCFILE.SPC_RBDEFER_20,
                   SPCFILE.SPC_RBPCT_20,
                   SPCFILE.SPC_RBREDUC_20,
                   SPCFILE.SPC_RBMIN_20,
                   SPCFILE.SPC_RBSPOPT1_20,
                   SPCFILE.SPC_RBSPOPT2_20,
                   SPCFILE.SPC_RBMANAGER_20,
                   ISNULL(SPCFILE.SPC_RBUNITPER_CD_20,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA2_20,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA_21,''),
                   ISNULL(SPCFILE.SPC_RBDTE_21_1,''),
                   SPCFILE.SPC_RBDAYS_21_1,
                   SPCFILE.SPC_RBYRTYPE_21_1,
                   ISNULL(SPCFILE.SPC_RBDTE_21_2,''),
                   SPCFILE.SPC_RBDAYS_21_2,
                   SPCFILE.SPC_RBYRTYPE_21_2,
                   SPCFILE.SPC_RBMINRET_21,
                   SPCFILE.SPC_RBNOMON_21,
                   SPCFILE.SPC_RBDEDCHG_21,
                   SPCFILE.SPC_RBRECAST_21,
                   SPCFILE.SPC_RBEARLY_21,
                   SPCFILE.SPC_RBDEFER_21,
                   SPCFILE.SPC_RBPCT_21,
                   SPCFILE.SPC_RBREDUC_21,
                   SPCFILE.SPC_RBMIN_21,
                   SPCFILE.SPC_RBSPOPT1_21,
                   SPCFILE.SPC_RBSPOPT2_21,
                   SPCFILE.SPC_RBMANAGER_21,
                   ISNULL(SPCFILE.SPC_RBUNITPER_CD_21,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA2_21,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA_22,''),
                   ISNULL(SPCFILE.SPC_RBDTE_22_1,''),
                   SPCFILE.SPC_RBDAYS_22_1,
                   SPCFILE.SPC_RBYRTYPE_22_1,
                   ISNULL(SPCFILE.SPC_RBDTE_22_2,''),
                   SPCFILE.SPC_RBDAYS_22_2,
                   SPCFILE.SPC_RBYRTYPE_22_2,
                   SPCFILE.SPC_RBMINRET_22,
                   SPCFILE.SPC_RBNOMON_22,
                   SPCFILE.SPC_RBDEDCHG_22,
                   SPCFILE.SPC_RBRECAST_22,
                   SPCFILE.SPC_RBEARLY_22,
                   SPCFILE.SPC_RBDEFER_22,
                   SPCFILE.SPC_RBPCT_22,
                   SPCFILE.SPC_RBREDUC_22,
                   SPCFILE.SPC_RBMIN_22,
                   SPCFILE.SPC_RBSPOPT1_22,
                   SPCFILE.SPC_RBSPOPT2_22,
                   SPCFILE.SPC_RBMANAGER_22,
                   ISNULL(SPCFILE.SPC_RBUNITPER_CD_22,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA2_22,''),
                   ISNULL(SPCFILE.SPC_RBCOMCD,''),
                   ISNULL(SPCFILE.SPC_ADDON_DISFRMLA,''),
                   ISNULL(SPCFILE.SPC_ADDON_WHOLE_FG,''),
                   ISNULL(SPCFILE.SPC_ADDON_ORG_MATURITY,''),
                   ISNULL(SPCFILE.SPC_CONSOLIDATE_RT_FG,''),
                   SPCFILE.SPC_1STPY_ADDON_DAYS,
                   SPCFILE.SPC_1STPY_ADDON_YRTYPE,
                   ISNULL(SPCFILE.SPC_ADDON_RATE_FG,''),
                   SPCFILE.SPC_ADDON_RATE,
                   ISNULL(SPCFILE.SPC_FEEFRMLA_1,''),
                   ISNULL(SPCFILE.SPC_FEECD_1,''),
                   ISNULL(SPCFILE.SPC_FEEID_1,''),
                   ISNULL(SPCFILE.SPC_FEEFRMLA_2,''),
                   ISNULL(SPCFILE.SPC_FEECD_2,''),
                   ISNULL(SPCFILE.SPC_FEEID_2,''),
                   ISNULL(SPCFILE.SPC_ROUND_PAY_FG,''),
                   ISNULL(SPCFILE.SPC_IRREGULAR_PAY_FG,''),
                   ISNULL(SPCFILE.SPC_DLRVCHGS_RB_CD_1,''),
                   ISNULL(SPCFILE.SPC_DLRVCHGS_RB_CD_2,''),
                   ISNULL(SPCFILE.SPC_DLRVCHGS_RB_CD_3,''),
                   ISNULL(SPCFILE.SPC_PARVHELD_RB_CD,''),
                   ISNULL(SPCFILE.SPC_PARVPAID_RB_CD,''),
                   SPCFILE.SPC_CONTR_CREDIT,
                   ISNULL(SPCFILE.SPC_CONTR_CREDIT_CD,''),
                   SPCFILE.SPC_CONTR_CREDIT_MIN_DEL_PER,
                   ISNULL(SPCFILE.SPC_OPT_CAL_CHGABLE,''),
                   SPCFILE.SPC_PBDEL_DAYS,
                   SPCFILE.SPC_PBINCR_PERCENT,
                   ISNULL(SPCFILE.SPC_CPOT30_CD,''),
                   ISNULL(SPCFILE.SPC_CPOTCHGOFF_CD,''),
                   ISNULL(SPCFILE.SPC_RDPOT30_CD,''),
                   ISNULL(SPCFILE.SPC_RDPOTCHGOFF_CD,''),
                   SPCFILE.SPC_MAX_ALLOW_DELINQ,
                   ISNULL(SPCFILE.SPC_RESCIND_BUS_DAYS,''),
                   ISNULL(SPCFILE.SPC_RESCIND_SATURDAY,''),
                   SPCFILE.SPC_MIN_CREDIT_SCORE,
                   ISNULL(SPCFILE.SPC_DUAL_IN_BRANCH,''),
                   ISNULL(SPCFILE.SPC_DUAL_IN_STATE,''),
                   ISNULL(SPCFILE.SPC_PACESETTER_FG,''),
                   SPCFILE.SPC_WAGE_ASSIGN_DAYS,
                   ISNULL(SPCFILE.SPC_CAL_PAYMENT_ROUND_FG,''),
                   ISNULL(SPCFILE.SPC_CAN_LOANS_FG,''),
                   ISNULL(SPCFILE.SPC_ALLOW_DELINQ_POLCD,''),
                   SPCFILE.SPC_YTD_MAX_ALLOW_DELINQ,
                   ISNULL(SPCFILE.SPC_LNCOST_FRMLA,''),
                   SPCFILE.SPC_LNCOST_AMT,
                   SPCFILE.SPC_DLDISC_RVPRIN1_PCNT,
                   SPCFILE.SPC_RENEWABLE_PAYS,
                   SPCFILE.SPC_RENEWABLE_DAYS,
                   ISNULL(SPCFILE.SPC_ALLOW_LN_BACK_DATE,''),
                   SPCFILE.SPC_DLDISC_RVPRIN1_AF_PCNT,
                   ISNULL(SPCFILE.SPC_CAL_MAX_AT_ESTIMATE,''),
                   ISNULL(SPCFILE.SPC_INSAH_RETRO,''),
                   ISNULL(SPCFILE.SPC_INSAH_DAYS_WAIT,''),
                   ISNULL(SPCFILE.SPC_ADDON_EXCEPT_FG,''),
                   ISNULL(SPCFILE.SPC_REPO_TRCD_1,''),
                   SPCFILE.SPC_REPO_FMNO_1_1,
                   SPCFILE.SPC_REPO_FMNO_1_2,
                   SPCFILE.SPC_REPO_FMNO_1_3,
                   SPCFILE.SPC_REPO_FMNO_1_4,
                   ISNULL(SPCFILE.SPC_REPO_TRCD_2,''),
                   SPCFILE.SPC_REPO_FMNO_2_1,
                   SPCFILE.SPC_REPO_FMNO_2_2,
                   SPCFILE.SPC_REPO_FMNO_2_3,
                   SPCFILE.SPC_REPO_FMNO_2_4,
                   ISNULL(SPCFILE.SPC_REPO_TRCD_3,''),
                   SPCFILE.SPC_REPO_FMNO_3_1,
                   SPCFILE.SPC_REPO_FMNO_3_2,
                   SPCFILE.SPC_REPO_FMNO_3_3,
                   SPCFILE.SPC_REPO_FMNO_3_4,
                   ISNULL(SPCFILE.SPC_CANCEL_ADDON_FG,''),
                   SPCFILE.SPC_DEFAULT_O2_INS_COMP,
                   ISNULL(SPCFILE.SPC_LC_ON_JUDGEMENTS,''),
                   SPCFILE.SPC_DELQ_IB_AGE,
                   SPCFILE.SPC_DELQ_IB_MAX_DAYS,
                   ISNULL(SPCFILE.SPC_INSURANCE_WRITTEN,''),
                   ISNULL(SPCFILE.SPC_INTERIM_COUPON,''),
                   ISNULL(SPCFILE.SPC_AUTO_ASSESS_LCHG,''),
                   SPCFILE.SPC_MAX_CASH_PROCEEDS,
                   ISNULL(SPCFILE.SPC_ALLOW_ZERO_PROC,''),
                   SPCFILE.SPC_BANKRULE_RATE,
                   ISNULL(SPCFILE.SPC_AUTO_SEND_TRCD_1,''),
                   SPCFILE.SPC_AUTO_SEND_FMNO_1_1,
                   SPCFILE.SPC_AUTO_SEND_FMNO_1_2,
                   SPCFILE.SPC_AUTO_SEND_FMNO_1_3,
                   SPCFILE.SPC_AUTO_SEND_FMNO_1_4,
                   ISNULL(SPCFILE.SPC_AUTO_SEND_TRCD_2,''),
                   SPCFILE.SPC_AUTO_SEND_FMNO_2_1,
                   SPCFILE.SPC_AUTO_SEND_FMNO_2_2,
                   SPCFILE.SPC_AUTO_SEND_FMNO_2_3,
                   SPCFILE.SPC_AUTO_SEND_FMNO_2_4,
                   SPCFILE.SPC_DLDISC_CONV_BF_PCNT,
                   ISNULL(SPCFILE.SPC_UNITPER_CD,''),
                   ISNULL(SPCFILE.SPC_RENEWAL_EDIT,''),
                   ISNULL(SPCFILE.SPC_ROUND_MFEE,''),
                   ISNULL(SPCFILE.SPC_ADV_TO_1ST_NO_CORR,''),
                   ISNULL(SPCFILE.SPC_LC_INT_ON_REPO_NON_PL,''),
                   SPCFILE.SPC_CONTRACTUAL_SET_NONACCRUAL,
                   SPCFILE.SPC_CONTRACTUAL_RESET_ACCRUAL,
                   SPCFILE.SPC_MONTHS_PAY_NAPP_COUNTER,
                   SPCFILE.SPC_SHORT_PAY_THRESHOLD,
                   SPCFILE.SPC_CASH_AVAIL_PERCENT,
                   ISNULL(SPCFILE.SPC_O1_JOINT_FG,''),
                   SPCFILE.SPC_NET_BAL_REQ,
                   SPCFILE.SPC_AMT_PROCEEDS_REQ,
                   SPCFILE.SPC_ALT_PREPAY_MONTHS,
                   SPCFILE.SPC_PERCENT_REGPAY_AVAIL,
                   SPCFILE.SPC_ALT_DEFFRMLA,
                   SPCFILE.SPC_ALT_DEFMIN,
                   SPCFILE.SPC_ALT_DEFMAX,
                   ISNULL(SPCFILE.SPC_NON_LOAN_FLAG,''),
                   ISNULL(SPCFILE.SPC_SKIP_TRW,'')
            INTO :QSPC-REC
            FROM DBO.SPCFILE
            WHERE SPCFILE.SPC_ORGST = :QSPC-ORGST
              AND SPCFILE.SPC_SPRCLASS = :QSPC-SPRCLASS
              AND SPCFILE.SPC_SUBCLASS = :QSPC-SUBCLASS
              AND SPCFILE.SPC_LAWCODE = :QSPC-LAWCODE 
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-SPC1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-SPC-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-SPC1-FILE.
           PERFORM CLOSE-IT.		  	  	  
	  
	  
	  
      * COPY "LIBLP/LPLTF1RN.CPY".
      * COPYMEMBER: LIBLP/LPLT1RN
       LOAD-LTF1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( LTF-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( LTF-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO LTF-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO LTF-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO LTF-ALL-DATA
              MOVE LTF-ALL-PATH             TO LTF-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( LTF-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO LTF-ALL-BASE
              MOVE FILPATH-MACHINE         TO LTF-ALL-MACHINE
              MOVE FILPATH-DATA            TO LTF-ALL-DATA
              MOVE LTF-ALL-PATH             TO LTF-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-LTFFILE NOT = "S" )
              MOVE LTF-PATH-BEG             TO LTF-SELECT-PATH-BEG
              MOVE LTF-PATH-FILE            TO LTF-SELECT-PATH-FILE
           ELSE
              MOVE LTF-PATH-SQL             TO LTF-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-LTF1-FILE.
           PERFORM LOAD-LTF1-FILE.
           PERFORM OPEN-IT.
           MOVE LTF-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-LTF1-FILE.
           PERFORM START-IT.
           MOVE LTF-PATH-OWNBR  TO LTF-BRNO.
           MOVE LTF-SELECT-PATH TO E-FILE.
           MOVE LTF1-KEY        TO E-KEYX.

           IF ( LTF1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-LTF1-FILE.

           IF ( QLTF1-WEND-BRNO = ZEROES    ) OR
              ( QLTF1-WEND-BRNO NOT NUMERIC )
              MOVE LTF-PATH-OWNBR        TO QLTF1-WBEG-BRNO
              MOVE LTF-PATH-OWNBR        TO QLTF1-WEND-BRNO
              MOVE LTF-ACCTNO            TO QLTF1-WBEG-ACCTNO
              MOVE ALL "9"               TO QLTF1-WEND-ACCTNO
              MOVE LTF-SEQNO             TO QLTF1-WBEG-SEQNO
              MOVE ALL "9"               TO QLTF1-WEND-SEQNO.

           EXEC SQL
            DECLARE LTF1_CURSOR CURSOR FOR
            SELECT LTFFILE.LTF_BRNO,
                   LTFFILE.LTF_ACCTNO,
                   LTFFILE.LTF_SEQNO,
                   ISNULL(CAST(LTFFILE.LTF_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(LTFFILE.LTF_FEECODE_1,''),
                   LTFFILE.LTF_FEECODE_PAYABLE_1,
                   ISNULL(LTFFILE.LTF_FEECODE_2,''),
                   LTFFILE.LTF_FEECODE_PAYABLE_2,
                   ISNULL(LTFFILE.LTF_FEECODE_3,''),
                   LTFFILE.LTF_FEECODE_PAYABLE_3,
                   ISNULL(LTFFILE.LTF_FEECODE_4,''),
                   LTFFILE.LTF_FEECODE_PAYABLE_4,
                   ISNULL(LTFFILE.LTF_FEECODE_5,''),
                   LTFFILE.LTF_FEECODE_PAYABLE_5,
                   ISNULL(LTFFILE.LTF_FEECODE_6,''),
                   LTFFILE.LTF_FEECODE_PAYABLE_6,
                   ISNULL(LTFFILE.LTF_FEECODE_7,''),
                   LTFFILE.LTF_FEECODE_PAYABLE_7,
                   ISNULL(LTFFILE.LTF_FEECODE_8,''),
                   LTFFILE.LTF_FEECODE_PAYABLE_8,
                   ISNULL(LTFFILE.LTF_FEECODE_9,''),
                   LTFFILE.LTF_FEECODE_PAYABLE_9,
                   ISNULL(LTFFILE.LTF_FEECODE_10,''),
                   LTFFILE.LTF_FEECODE_PAYABLE_10
            FROM DBO.LTFFILE
            WHERE LTFFILE.LTF_BRNO >= :QLTF1-WBEG-BRNO
              AND LTFFILE.LTF_BRNO <= :QLTF1-WEND-BRNO
              AND LTFFILE.LTF_ACCTNO >= :QLTF1-WBEG-ACCTNO
              AND LTFFILE.LTF_ACCTNO <= :QLTF1-WEND-ACCTNO
              AND LTFFILE.LTF_SEQNO >= :QLTF1-WBEG-SEQNO
              AND LTFFILE.LTF_SEQNO <= :QLTF1-WEND-SEQNO
            ORDER BY LTFFILE.LTF_BRNO,
                     LTFFILE.LTF_ACCTNO,
                     LTFFILE.LTF_SEQNO
           END-EXEC.

           EXEC SQL
               OPEN LTF1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QLTF1-WBEG-BRNO
                          QLTF1-WEND-BRNO
                          QLTF1-WBEG-ACCTNO
                          QLTF1-WEND-ACCTNO
                          QLTF1-WBEG-SEQNO
                          QLTF1-WEND-SEQNO.

           MOVE STAT---GOOD TO LTF1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-LTF1-FILE.
           PERFORM READ-IT.
           MOVE LTF-PATH-OWNBR  TO LTF-BRNO. 
           MOVE LTF-SELECT-PATH TO E-FILE.
           MOVE LTF1-KEY        TO E-KEYX.

           MOVE LTF-BRNO   TO QLTF-BRNO.
           MOVE LTF-ACCTNO TO QLTF-ACCTNO.
           MOVE LTF-SEQNO  TO QLTF-SEQNO.

           EXEC SQL
            SELECT LTFFILE.LTF_BRNO,
                   LTFFILE.LTF_ACCTNO,
                   LTFFILE.LTF_SEQNO,
                   ISNULL(LTFFILE.LTF_FEECODE_1,''),
                   ISNULL(LTFFILE.LTF_FEECODE_2,''),
                   ISNULL(LTFFILE.LTF_FEECODE_3,''),
                   ISNULL(LTFFILE.LTF_FEECODE_4,''),
                   ISNULL(LTFFILE.LTF_FEECODE_5,''),
                   ISNULL(LTFFILE.LTF_FEECODE_6,''),
                   ISNULL(LTFFILE.LTF_FEECODE_7,''),
                   ISNULL(LTFFILE.LTF_FEECODE_8,''),
                   ISNULL(LTFFILE.LTF_FEECODE_9,''),
                   ISNULL(LTFFILE.LTF_FEECODE_10,''),
                   LTFFILE.LTF_FEECODE_PAYABLE_1,
                   LTFFILE.LTF_FEECODE_PAYABLE_2,
                   LTFFILE.LTF_FEECODE_PAYABLE_3,
                   LTFFILE.LTF_FEECODE_PAYABLE_4,
                   LTFFILE.LTF_FEECODE_PAYABLE_5,
                   LTFFILE.LTF_FEECODE_PAYABLE_6,
                   LTFFILE.LTF_FEECODE_PAYABLE_7,
                   LTFFILE.LTF_FEECODE_PAYABLE_8,
                   LTFFILE.LTF_FEECODE_PAYABLE_9,
                   LTFFILE.LTF_FEECODE_PAYABLE_10
              INTO 
                   :QLTF-BRNO,
                   :QLTF-ACCTNO,
                   :QLTF-SEQNO,
                   :QLTF-FEECODE-1, 
                   :QLTF-FEECODE-2, 
                   :QLTF-FEECODE-3, 
                   :QLTF-FEECODE-4, 
                   :QLTF-FEECODE-5, 
                   :QLTF-FEECODE-6, 
                   :QLTF-FEECODE-7, 
                   :QLTF-FEECODE-8, 
                   :QLTF-FEECODE-9, 
                   :QLTF-FEECODE-10, 
                   :QLTF-FEECODE-PAYABLE-1,
                   :QLTF-FEECODE-PAYABLE-2,
                   :QLTF-FEECODE-PAYABLE-3,
                   :QLTF-FEECODE-PAYABLE-4,
                   :QLTF-FEECODE-PAYABLE-5,
                   :QLTF-FEECODE-PAYABLE-6,
                   :QLTF-FEECODE-PAYABLE-7,
                   :QLTF-FEECODE-PAYABLE-8,
                   :QLTF-FEECODE-PAYABLE-9,
                   :QLTF-FEECODE-PAYABLE-10
              FROM DBO.LTFFILE
             WHERE LTFFILE.LTF_BRNO   = :QLTF-BRNO
               AND LTFFILE.LTF_ACCTNO = :QLTF-ACCTNO
               AND LTFFILE.LTF_SEQNO  = :QLTF-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LTF1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-LTF-FIELDS.

      *-----------------------------------------------------------------
       READ-LTF1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE LTF-PATH-OWNBR  TO LTF-BRNO.
           MOVE LTF-SELECT-PATH TO E-FILE.
           MOVE LTF1-KEY        TO E-KEYX.
           INITIALIZE QLTF-REC.

           IF ( LTF1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH LTF1_CURSOR INTO :QLTF-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LTF1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-LTF-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-LTF1-FILE.

           PERFORM CLOSE-IT.
           IF ( LTF1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE LTF1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO LTF1-CURSOR-STAT.

	  
	  
	  
       COPY "LIBGB/GBWK1IN.CPY".
       COPY "LIBGB/GBWK2IN.CPY".


      * COPY "LIBLP/LPTRL1RN.CPY".
      * COPYMEMBER: LIBLP/LPTR1RN
       LOAD-TRL1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( TRL-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( TRL-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO TRL-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO TRL-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO TRL-ALL-DATA
              MOVE TRL-ALL-PATH             TO TRL-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( TRL-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO TRL-ALL-BASE
              MOVE FILPATH-MACHINE         TO TRL-ALL-MACHINE
              MOVE FILPATH-DATA            TO TRL-ALL-DATA
              MOVE TRL-ALL-PATH             TO TRL-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-TRLFILE NOT = "S" )
              MOVE TRL-PATH-BEG             TO TRL-SELECT-PATH-BEG
              MOVE TRL-PATH-FILE            TO TRL-SELECT-PATH-FILE
           ELSE
              MOVE TRL-PATH-SQL             TO TRL-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-TRL1-FILE.
           PERFORM LOAD-TRL1-FILE.
           PERFORM OPEN-IT.
           MOVE TRL-SELECT-PATH TO E-FILE.

           EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS TRL1_CONNECT
                    USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
           END-EXEC.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       START-TRL1-FILE.
           PERFORM START-IT.
           MOVE TRL-PATH-OWNBR  TO TL-BRNO.
           MOVE TRL-SELECT-PATH TO E-FILE.
           MOVE TRL1-KEY        TO E-KEYX.

           IF ( TRL1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-TRL1-FILE.

           IF ( QTRL1-WEND-LINE = ZEROES    ) OR
              ( QTRL1-WEND-LINE NOT NUMERIC )
              MOVE TRL-PATH-OWNBR        TO QTRL1-WBEG-BRNO
              MOVE TRL-PATH-OWNBR        TO QTRL1-WEND-BRNO
              MOVE TL-DATE               TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTRL1-WBEG-DATE
              MOVE JULIAN-END           TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTRL1-WEND-DATE
              MOVE TL-CLASS             TO QTRL1-WBEG-CLASS
              MOVE TL-ACCTNO            TO QTRL1-WBEG-ACCTNO
              MOVE TL-LAWCODE           TO QTRL1-WBEG-LAWCODE
              MOVE TL-LINE              TO QTRL1-WBEG-LINE
              MOVE ALL "9"              TO QTRL1-WEND-CLASS
                                           QTRL1-WEND-ACCTNO
                                           QTRL1-WEND-LAWCODE
                                           QTRL1-WEND-LINE.

           EXEC SQL SET CONNECTION TRL1_CONNECT END-EXEC.

           EXEC SQL
            DECLARE TRL1_CURSOR CURSOR FOR
                :RET-CODE = EXEC SP_RZMONF_TRLFILE_SELECT
              ( :QTRL1-WBEG-BRNO,    :QTRL1-WEND-BRNO,   
                :QTRL1-WBEG-DATE,    :QTRL1-WEND-DATE,   
                :QTRL1-WBEG-CLASS,   :QTRL1-WEND-CLASS,  
                :QTRL1-WBEG-ACCTNO,  :QTRL1-WEND-ACCTNO, 
                :QTRL1-WBEG-LAWCODE, :QTRL1-WEND-LAWCODE,
                :QTRL1-WBEG-LINE,    :QTRL1-WEND-LINE )  
           END-EXEC.

      *     EXEC SQL
      *      DECLARE TRL1_CURSOR CURSOR FOR
      *      SELECT TRLFILE.TL_BRNO,
      *             ISNULL(CAST(TRLFILE.TL_DATE AS VARCHAR(10)),'0'),
      *             TRLFILE.TL_CLASS,
      *             TRLFILE.TL_ACCTNO,
      *             TRLFILE.TL_LAWCODE,
      *             TRLFILE.TL_LINE,
      *             TRLFILE.TL_INSCOMP_1,
      *             TRLFILE.TL_INSCOMP_2,
      *             TRLFILE.TL_INSCOMP_3,
      *             TRLFILE.TL_INSCOMP_4,
      *             TRLFILE.TL_INSCOMP_5,
      *             TRLFILE.TL_INSCOMP_6,
      *             TRLFILE.TL_INSCOMP_7,
      *             TRLFILE.TL_INSCOMP_8,
      *             TRLFILE.TL_INSCOMP_9,
      *             TRLFILE.TL_INSCOMP_10,
      *             ISNULL(TRLFILE.TL_INSOURS_1,''),
      *             ISNULL(TRLFILE.TL_INSOURS_2,''),
      *             ISNULL(TRLFILE.TL_INSOURS_3,''),
      *             ISNULL(TRLFILE.TL_INSOURS_4,''),
      *             ISNULL(TRLFILE.TL_INSOURS_5,''),
      *             ISNULL(TRLFILE.TL_INSOURS_6,''),
      *             ISNULL(TRLFILE.TL_INSOURS_7,''),
      *             ISNULL(TRLFILE.TL_INSOURS_8,''),
      *             ISNULL(TRLFILE.TL_INSOURS_9,''),
      *             ISNULL(TRLFILE.TL_INSOURS_10,''),
      *             TRLFILE.TL_INSPREM_1,
      *             TRLFILE.TL_INSPREM_2,
      *             TRLFILE.TL_INSPREM_3,
      *             TRLFILE.TL_INSPREM_4,
      *             TRLFILE.TL_INSPREM_5,
      *             TRLFILE.TL_INSPREM_6,
      *             TRLFILE.TL_INSPREM_7,
      *             TRLFILE.TL_INSPREM_8,
      *             TRLFILE.TL_INSPREM_9,
      *             TRLFILE.TL_INSPREM_10,
      *             TRLFILE.TL_INSCOMM_1,
      *             TRLFILE.TL_INSCOMM_2,
      *             TRLFILE.TL_INSCOMM_3,
      *             TRLFILE.TL_INSCOMM_4,
      *             TRLFILE.TL_INSCOMM_5,
      *             TRLFILE.TL_INSCOMM_6,
      *             TRLFILE.TL_INSCOMM_7,
      *             TRLFILE.TL_INSCOMM_8,
      *             TRLFILE.TL_INSCOMM_9,
      *             TRLFILE.TL_INSCOMM_10,
      *             LNFILE.LN_OWNBR,
      *             LNFILE.LN_ACCTNO,
      *             LNFILE.LN_CLASS,
      *             LNFILE.LN_SUBCLASS,
      *             LNFILE.LN_LAWCODE,
      *             LNFILE.LN_SSNO1,
      *             ISNULL(LNFILE.LN_MAKERCD1,''),
      *             LNFILE.LN_SSNO2,
      *             ISNULL(LNFILE.LN_MAKERCD2,''),
      *             LNFILE.LN_SSNO3,
      *             ISNULL(LNFILE.LN_MAKERCD3,''),
      *             LNFILE.LN_SSNO4,
      *             ISNULL(LNFILE.LN_MAKERCD4,''),
      *             ISNULL(LNFILE.LN_ORGST,''),
      *             LNFILE.LN_SPRCLASS,
      *             ISNULL(LNFILE.LN_STATFLAGS,''),
      *             ISNULL(CAST(LNFILE.LN_ORIG_1STPYDATE
      *                                            AS VARCHAR(10)),'0'),
      *             ISNULL(CAST(LNFILE.LN_1STPYDATE
      *                                            AS VARCHAR(10)),'0'),
      *             LNFILE.LN_ORGTERM,
      *             ISNULL(LNFILE.LN_INSURED,''),
      *             ISNULL(LNFILE.LN_INSTYPES,''),
      *             LNFILE.LN_INSCOMP_1,
      *             LNFILE.LN_INSCOMP_2,
      *             LNFILE.LN_INSCOMP_3,
      *             LNFILE.LN_INSCOMP_4,
      *             LNFILE.LN_INSCOMP_5,
      *             LNFILE.LN_INSCOMP_6,
      *             LNFILE.LN_INSCOMP_7,
      *             LNFILE.LN_INSCOMP_8,
      *             LNFILE.LN_INSCOMP_9,
      *             LNFILE.LN_INSCOMP_10,
      *             ISNULL(CAST(LNFILE.LN_INSEFF_DATE_1
      *                                            AS VARCHAR(10)),'0'),
      *             ISNULL(CAST(LNFILE.LN_INSEFF_DATE_2
      *                                            AS VARCHAR(10)),'0'),
      *             ISNULL(CAST(LNFILE.LN_INSEFF_DATE_3
      *                                            AS VARCHAR(10)),'0'),
      *             ISNULL(CAST(LNFILE.LN_INSEFF_DATE_4
      *                                            AS VARCHAR(10)),'0'),
      *             ISNULL(CAST(LNFILE.LN_INSEFF_DATE_5
      *                                            AS VARCHAR(10)),'0'),
      *             ISNULL(CAST(LNFILE.LN_INSEFF_DATE_6
      *                                            AS VARCHAR(10)),'0'),
      *             ISNULL(CAST(LNFILE.LN_INSEFF_DATE_7
      *                                            AS VARCHAR(10)),'0'),
      *             ISNULL(CAST(LNFILE.LN_INSEFF_DATE_8
      *                                            AS VARCHAR(10)),'0'),
      *             ISNULL(CAST(LNFILE.LN_INSEFF_DATE_9
      *                                            AS VARCHAR(10)),'0'),
      *             ISNULL(CAST(LNFILE.LN_INSEFF_DATE_10
      *                                            AS VARCHAR(10)),'0'),
      *             ISNULL(CAST(LNFILE.LN_INSEXP_DATE_1
      *                                            AS VARCHAR(10)),'0'),
      *             ISNULL(CAST(LNFILE.LN_INSEXP_DATE_2
      *                                            AS VARCHAR(10)),'0'),
      *             ISNULL(CAST(LNFILE.LN_INSEXP_DATE_3
      *                                            AS VARCHAR(10)),'0'),
      *             ISNULL(CAST(LNFILE.LN_INSEXP_DATE_4
      *                                            AS VARCHAR(10)),'0'),
      *             ISNULL(CAST(LNFILE.LN_INSEXP_DATE_5
      *                                            AS VARCHAR(10)),'0'),
      *             ISNULL(CAST(LNFILE.LN_INSEXP_DATE_6
      *                                            AS VARCHAR(10)),'0'),
      *             ISNULL(CAST(LNFILE.LN_INSEXP_DATE_7
      *                                            AS VARCHAR(10)),'0'),
      *             ISNULL(CAST(LNFILE.LN_INSEXP_DATE_8
      *                                            AS VARCHAR(10)),'0'),
      *             ISNULL(CAST(LNFILE.LN_INSEXP_DATE_9
      *                                            AS VARCHAR(10)),'0'),
      *             ISNULL(CAST(LNFILE.LN_INSEXP_DATE_10
      *                                            AS VARCHAR(10)),'0'),
      *             LNFILE.LN_INSCOVR_1,
      *             LNFILE.LN_INSCOVR_2,
      *             LNFILE.LN_INSCOVR_3,
      *             LNFILE.LN_INSCOVR_4,
      *             LNFILE.LN_INSCOVR_5,
      *             LNFILE.LN_INSCOVR_6,
      *             LNFILE.LN_INSCOVR_7,
      *             LNFILE.LN_INSCOVR_8,
      *             LNFILE.LN_INSCOVR_9,
      *             LNFILE.LN_INSCOVR_10,
      *             LNFILE.LN_TOTNODEF,
      *             ISNULL(LNFILE.LN_CL_REBATE,''),
      *             ISNULL(LNFILE.LN_AH_REBATE,''),
      *             ISNULL(LNFILE.LN_PP_REBATE,''),
      *             ISNULL(LNFILE.LN_INT_REBATE,''),
      *             ISNULL(LNFILE.LN_SC_REBATE,''),
      *             ISNULL(LNFILE.LN_MF_REBATE,''),
      *             ISNULL(LNFILE.LN_DF_REBATE,''),
      *             ISNULL(LNFILE.LN_EX_REBATE,''),
      *             ISNULL(LNFILE.LN_O1_REBATE,''),
      *             ISNULL(LNFILE.LN_O2_REBATE,''),
      *             ISNULL(LNFILE.LN_O3_REBATE,''),
      *             ISNULL(LNFILE.LN_O4_REBATE,''),
      *             ISNULL(LNFILE.LN_O5_REBATE,''),
      *             ISNULL(LNFILE.LN_O6_REBATE,''),
      *             ISNULL(LNFILE.LN_O7_REBATE,''),
      *             ISNULL(LNFILE.LN_UN_O7_REB,''),
      *             ISNULL(LNFILE.LN_UN_O8_REB,''),
      *             ISNULL(LNFILE.LN_UN_O9_REB,''),
      *             ISNULL(LNFILE.LN_UN_O10_REB,''),
      *             LNFILE.LN_SUM_ORGTERM,
      *             ISNULL(LNFILE.LN_UNITPER_CD,''),
      *             LNFILE.LN_UNITPER_FREQ
      *        FROM DBO.TRLFILE TRLFILE, 
      *             DBO.LNFILE  LNFILE
      *       WHERE ( TRLFILE.TL_BRNO    >= :QTRL1-WBEG-BRNO
      *         AND   TRLFILE.TL_BRNO    <= :QTRL1-WEND-BRNO
      *         AND   CAST(TRLFILE.TL_DATE AS VARCHAR(10))
      *                                  >= :QTRL1-WBEG-DATE
      *         AND   CAST(TRLFILE.TL_DATE AS VARCHAR(10))
      *                                  <= :QTRL1-WEND-DATE
      *         AND   TRLFILE.TL_CLASS   >= :QTRL1-WBEG-CLASS
      *         AND   TRLFILE.TL_CLASS   <= :QTRL1-WEND-CLASS
      *         AND   TRLFILE.TL_ACCTNO  >= :QTRL1-WBEG-ACCTNO
      *         AND   TRLFILE.TL_ACCTNO  <= :QTRL1-WEND-ACCTNO
      *         AND   TRLFILE.TL_LAWCODE >= :QTRL1-WBEG-LAWCODE
      *         AND   TRLFILE.TL_LAWCODE <= :QTRL1-WEND-LAWCODE
      *         AND   TRLFILE.TL_LINE    >= :QTRL1-WBEG-LINE
      *         AND   TRLFILE.TL_LINE    <= :QTRL1-WEND-LINE )
      *         AND ( LNFILE.LN_OWNBR     = TRLFILE.TL_BRNO
      *         AND   LNFILE.LN_ACCTNO    = TRLFILE.TL_ACCTNO )
      *       ORDER BY TRLFILE.TL_BRNO,
      *                TRLFILE.TL_DATE,
      *                TRLFILE.TL_CLASS,
      *                TRLFILE.TL_ACCTNO,
      *                TRLFILE.TL_LAWCODE,
      *     			  TRLFILE.TL_LINE
      *     END-EXEC.

           EXEC SQL
               OPEN TRL1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QTRL1-WBEG-BRNO
                          QTRL1-WEND-BRNO
                          QTRL1-WBEG-CLASS
                          QTRL1-WEND-CLASS
                          QTRL1-WBEG-ACCTNO
                          QTRL1-WEND-ACCTNO
                          QTRL1-WBEG-LAWCODE
                          QTRL1-WEND-LAWCODE
                          QTRL1-WBEG-LINE
                          QTRL1-WEND-LINE.
           MOVE SPACES TO QTRL1-WBEG-DATE
                          QTRL1-WEND-DATE.


           MOVE STAT---GOOD TO TRL1-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       READ-TRL1-FILE.
           PERFORM READ-IT.
           MOVE TRL-PATH-OWNBR  TO TL-BRNO. 
           MOVE TRL-SELECT-PATH TO E-FILE.
           MOVE TRL1-KEY        TO E-KEYX.

           MOVE TL-BRNO             TO QTL-BRNO.
           MOVE TL-DATE             TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTL-DATE.
           MOVE TL-CLASS            TO QTL-CLASS.
           MOVE TL-ACCTNO           TO QTL-ACCTNO.
           MOVE TL-LAWCODE          TO QTL-LAWCODE.
           MOVE TL-LINE             TO QTL-LINE.

           EXEC SQL
            SELECT TRLFILE.TL_BRNO,
                   ISNULL(CAST(TRLFILE.TL_DATE AS VARCHAR(10)),'0'),
                   TRLFILE.TL_CLASS,
                   TRLFILE.TL_ACCTNO,
                   TRLFILE.TL_LAWCODE,
                   TRLFILE.TL_LINE,
                   ISNULL(TRLFILE.TL_SRCD,''),
                   ISNULL(TRLFILE.TL_FEECODE_1,''),
                   ISNULL(TRLFILE.TL_FEECODE_2,''),
                   ISNULL(TRLFILE.TL_FEECODE_3,''),
                   ISNULL(TRLFILE.TL_FEECODE_4,''),
                   ISNULL(TRLFILE.TL_FEECODE_5,''),
                   ISNULL(TRLFILE.TL_FEECODE_6,''),
                   ISNULL(TRLFILE.TL_FEECODE_7,''),
                   ISNULL(TRLFILE.TL_FEECODE_8,''),
                   ISNULL(TRLFILE.TL_FEECODE_9,''),
                   ISNULL(TRLFILE.TL_FEECODE_10,''),
                   ISNULL(TRLFILE.TL_FEECODE_11,''),
                   ISNULL(TRLFILE.TL_FEECODE_12,''),
                   ISNULL(TRLFILE.TL_FEECODE_13,''),
                   ISNULL(TRLFILE.TL_FEECODE_14,''),
                   ISNULL(TRLFILE.TL_FEECODE_15,''),
                   ISNULL(TRLFILE.TL_FEECODE_16,''),
                   ISNULL(TRLFILE.TL_FEECODE_17,''),
                   ISNULL(TRLFILE.TL_FEECODE_18,''),
                   ISNULL(TRLFILE.TL_FEECODE_19,''),
                   ISNULL(TRLFILE.TL_FEECODE_20,''),
                   TRLFILE.TL_FEEAMT_1,
                   TRLFILE.TL_FEEAMT_2,
                   TRLFILE.TL_FEEAMT_3,
                   TRLFILE.TL_FEEAMT_4,
                   TRLFILE.TL_FEEAMT_5,
                   TRLFILE.TL_FEEAMT_6,
                   TRLFILE.TL_FEEAMT_7,
                   TRLFILE.TL_FEEAMT_8,
                   TRLFILE.TL_FEEAMT_9,
                   TRLFILE.TL_FEEAMT_10,
                   TRLFILE.TL_FEEAMT_11,
                   TRLFILE.TL_FEEAMT_12,
                   TRLFILE.TL_FEEAMT_13,
                   TRLFILE.TL_FEEAMT_14,
                   TRLFILE.TL_FEEAMT_15,
                   TRLFILE.TL_FEEAMT_16,
                   TRLFILE.TL_FEEAMT_17,
                   TRLFILE.TL_FEEAMT_18,
                   TRLFILE.TL_FEEAMT_19,
                   TRLFILE.TL_FEEAMT_20
              INTO 
                  :QTL-BRNO,			       
				  :QTL-DATE,
                  :QTL-CLASS,
                  :QTL-ACCTNO,
                  :QTL-LAWCODE,
                  :QTL-LINE,
                  :QTL-SRCD, 
				  :QTL-FEECODE-1, 
				  :QTL-FEECODE-2,  
				  :QTL-FEECODE-3,  
				  :QTL-FEECODE-4,  
				  :QTL-FEECODE-5,  
				  :QTL-FEECODE-6,  
				  :QTL-FEECODE-7,  
				  :QTL-FEECODE-8,  
				  :QTL-FEECODE-9,  
				  :QTL-FEECODE-10,  
				  :QTL-FEECODE-11,  
				  :QTL-FEECODE-12,  
				  :QTL-FEECODE-13,  
				  :QTL-FEECODE-14,  
				  :QTL-FEECODE-15,  
				  :QTL-FEECODE-16,  
				  :QTL-FEECODE-17,  
				  :QTL-FEECODE-18,  
				  :QTL-FEECODE-19,  
				  :QTL-FEECODE-20,  
				  :QTL-FEEAMT-1, 
				  :QTL-FEEAMT-2, 
				  :QTL-FEEAMT-3, 
				  :QTL-FEEAMT-4, 
				  :QTL-FEEAMT-5, 
				  :QTL-FEEAMT-6, 
				  :QTL-FEEAMT-7, 
				  :QTL-FEEAMT-8, 
				  :QTL-FEEAMT-9, 
				  :QTL-FEEAMT-10, 
				  :QTL-FEEAMT-11, 
				  :QTL-FEEAMT-12, 
				  :QTL-FEEAMT-13, 
				  :QTL-FEEAMT-14, 
				  :QTL-FEEAMT-15, 
				  :QTL-FEEAMT-16, 
				  :QTL-FEEAMT-17, 
				  :QTL-FEEAMT-18, 
				  :QTL-FEEAMT-19, 
				  :QTL-FEEAMT-20    				   
              FROM DBO.TRLFILE
             WHERE TRLFILE.TL_BRNO    = :QTL-BRNO
               AND TRLFILE.TL_DATE    = :QTL-DATE
               AND TRLFILE.TL_CLASS   = :QTL-CLASS
               AND TRLFILE.TL_ACCTNO  = :QTL-ACCTNO
               AND TRLFILE.TL_LAWCODE = :QTL-LAWCODE
               AND TRLFILE.TL_LINE    = :QTL-LINE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TRL1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-TRL-FIELDS.

      *-----------------------------------------------------------------
       READ-TRL1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE TRL-PATH-OWNBR  TO TL-BRNO.
           MOVE TRL-SELECT-PATH TO E-FILE.
           MOVE TRL1-KEY        TO E-KEYX.
           INITIALIZE QTRL-REC.
 
           EXEC SQL SET CONNECTION TRL1_CONNECT END-EXEC.

           IF ( TRL1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH TRL1_CURSOR INTO  
                     :QTL-BRNO,
                     :QTL-DATE,
                     :QTL-CLASS,
                     :QTL-ACCTNO,
                     :QTL-LAWCODE,
                     :QTL-LINE,
                     :QTL-SRCD, 
                     :QTL-FEECODE-1, 
                     :QTL-FEECODE-2, 
                     :QTL-FEECODE-3, 
                     :QTL-FEECODE-4, 
                     :QTL-FEECODE-5, 
                     :QTL-FEECODE-6, 
                     :QTL-FEECODE-7, 
                     :QTL-FEECODE-8, 
                     :QTL-FEECODE-9, 
                     :QTL-FEECODE-10, 
                     :QTL-FEECODE-11, 
                     :QTL-FEECODE-12, 
                     :QTL-FEECODE-13, 
                     :QTL-FEECODE-14, 
                     :QTL-FEECODE-15, 
                     :QTL-FEECODE-16, 
                     :QTL-FEECODE-17, 
                     :QTL-FEECODE-18, 
                     :QTL-FEECODE-19, 
                     :QTL-FEECODE-20, 
                     :QTL-FEEAMT-1,
                     :QTL-FEEAMT-2,
                     :QTL-FEEAMT-3,
                     :QTL-FEEAMT-4,
                     :QTL-FEEAMT-5,
                     :QTL-FEEAMT-6,
                     :QTL-FEEAMT-7,
                     :QTL-FEEAMT-8,
                     :QTL-FEEAMT-9,
                     :QTL-FEEAMT-10,
                     :QTL-FEEAMT-11,
                     :QTL-FEEAMT-12,
                     :QTL-FEEAMT-13,
                     :QTL-FEEAMT-14,
                     :QTL-FEEAMT-15,
                     :QTL-FEEAMT-16,
                     :QTL-FEEAMT-17,
                     :QTL-FEEAMT-18,
                     :QTL-FEEAMT-19,
                     :QTL-FEEAMT-20   
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TRL1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-TRL-FIELDS.
			  
           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       CLOSE-TRL1-FILE.

           EXEC SQL SET CONNECTION TRL1_CONNECT END-EXEC.

           PERFORM CLOSE-IT.
           IF ( TRL1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE TRL1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO TRL1-CURSOR-STAT.		 
			  
           EXEC SQL SET CONNECTION C1 END-EXEC.	  
	  
      * COPY "LIBLP/LPTRS1RN.CPY".
      * COPYMEMBER: LIBLP/LPTR1RN
       LOAD-TRS1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( TRS-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( TRS-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO TRS-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO TRS-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO TRS-ALL-DATA
              MOVE TRS-ALL-PATH             TO TRS-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( TRS-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO TRS-ALL-BASE
              MOVE FILPATH-MACHINE         TO TRS-ALL-MACHINE
              MOVE FILPATH-DATA            TO TRS-ALL-DATA
              MOVE TRS-ALL-PATH             TO TRS-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-TRSFILE NOT = "S" )
              MOVE TRS-PATH-BEG             TO TRS-SELECT-PATH-BEG
              MOVE TRS-PATH-FILE            TO TRS-SELECT-PATH-FILE
           ELSE
              MOVE TRS-PATH-SQL             TO TRS-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-TRS1-FILE.
           PERFORM LOAD-TRS1-FILE.
           PERFORM OPEN-IT.
           MOVE TRS-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-TRS1-FILE.
           PERFORM START-IT.
           MOVE TRS-PATH-OWNBR  TO TS-BRNO.
           MOVE TRS-SELECT-PATH TO E-FILE.
           MOVE TRS1-KEY        TO E-KEYX.

           IF ( TRS1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-TRS1-FILE.

           IF ( QTRS1-WEND-LINE = ZEROES    ) OR
              ( QTRS1-WEND-LINE NOT NUMERIC )
              MOVE TRS-PATH-OWNBR        TO QTRS1-WBEG-BRNO
              MOVE TRS-PATH-OWNBR        TO QTRS1-WEND-BRNO
              MOVE TS-DATE               TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTRS1-WBEG-DATE
              MOVE JULIAN-END           TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTRS1-WEND-DATE
              MOVE TS-CLASS             TO QTRS1-WBEG-CLASS
              MOVE TS-ACCTNO            TO QTRS1-WBEG-ACCTNO
              MOVE TS-LAWCODE           TO QTRS1-WBEG-LAWCODE
              MOVE TS-LINE              TO QTRS1-WBEG-LINE
              MOVE ALL "9"              TO QTRS1-WEND-CLASS
                                           QTRS1-WEND-ACCTNO
                                           QTRS1-WEND-LAWCODE
                                           QTRS1-WEND-LINE.

           EXEC SQL		   
            DECLARE TRS1_CURSOR CURSOR FOR
            SELECT TRSFILE.TS_BRNO,
                   ISNULL(CAST(TRSFILE.TS_DATE AS VARCHAR(10)),'0'),
                   TRSFILE.TS_CLASS,
                   TRSFILE.TS_ACCTNO,
                   TRSFILE.TS_LAWCODE,
                   TRSFILE.TS_LINE,
                   ISNULL(TRSFILE.TS_SRCD,''),
                   ISNULL(TRSFILE.TS_FEECODE_1,''),
                   ISNULL(TRSFILE.TS_FEECODE_2,''),
                   ISNULL(TRSFILE.TS_FEECODE_3,''),
                   ISNULL(TRSFILE.TS_FEECODE_4,''),
                   ISNULL(TRSFILE.TS_FEECODE_5,''),
                   ISNULL(TRSFILE.TS_FEECODE_6,''),
                   ISNULL(TRSFILE.TS_FEECODE_7,''),
                   ISNULL(TRSFILE.TS_FEECODE_8,''),
                   ISNULL(TRSFILE.TS_FEECODE_9,''),
                   ISNULL(TRSFILE.TS_FEECODE_10,''),
                   ISNULL(TRSFILE.TS_FEECODE_11,''),
                   ISNULL(TRSFILE.TS_FEECODE_12,''),
                   ISNULL(TRSFILE.TS_FEECODE_13,''),
                   ISNULL(TRSFILE.TS_FEECODE_14,''),
                   ISNULL(TRSFILE.TS_FEECODE_15,''),
                   ISNULL(TRSFILE.TS_FEECODE_16,''),
                   ISNULL(TRSFILE.TS_FEECODE_17,''),
                   ISNULL(TRSFILE.TS_FEECODE_18,''),
                   ISNULL(TRSFILE.TS_FEECODE_19,''),
                   ISNULL(TRSFILE.TS_FEECODE_20,''),
                   TRSFILE.TS_FEEAMT_1,
                   TRSFILE.TS_FEEAMT_2,
                   TRSFILE.TS_FEEAMT_3,
                   TRSFILE.TS_FEEAMT_4,
                   TRSFILE.TS_FEEAMT_5,
                   TRSFILE.TS_FEEAMT_6,
                   TRSFILE.TS_FEEAMT_7,
                   TRSFILE.TS_FEEAMT_8,
                   TRSFILE.TS_FEEAMT_9,
                   TRSFILE.TS_FEEAMT_10,
                   TRSFILE.TS_FEEAMT_11,
                   TRSFILE.TS_FEEAMT_12,
                   TRSFILE.TS_FEEAMT_13,
                   TRSFILE.TS_FEEAMT_14,
                   TRSFILE.TS_FEEAMT_15,
                   TRSFILE.TS_FEEAMT_16,
                   TRSFILE.TS_FEEAMT_17,
                   TRSFILE.TS_FEEAMT_18,
                   TRSFILE.TS_FEEAMT_19,
                   TRSFILE.TS_FEEAMT_20
              FROM DBO.TRSFILE TRSFILE 
             WHERE ( TRSFILE.TS_BRNO    >= :QTRS1-WBEG-BRNO
               AND   TRSFILE.TS_BRNO    <= :QTRS1-WEND-BRNO
               AND   CAST(TRSFILE.TS_DATE AS VARCHAR(10))
                                        >= :QTRS1-WBEG-DATE
               AND   CAST(TRSFILE.TS_DATE AS VARCHAR(10))
                                        <= :QTRS1-WEND-DATE
               AND   TRSFILE.TS_CLASS   >= :QTRS1-WBEG-CLASS
               AND   TRSFILE.TS_CLASS   <= :QTRS1-WEND-CLASS
               AND   TRSFILE.TS_ACCTNO  >= :QTRS1-WBEG-ACCTNO
               AND   TRSFILE.TS_ACCTNO  <= :QTRS1-WEND-ACCTNO
               AND   TRSFILE.TS_LAWCODE >= :QTRS1-WBEG-LAWCODE
               AND   TRSFILE.TS_LAWCODE <= :QTRS1-WEND-LAWCODE
               AND   TRSFILE.TS_LINE    >= :QTRS1-WBEG-LINE
               AND   TRSFILE.TS_LINE    <= :QTRS1-WEND-LINE )
             ORDER BY TRSFILE.TS_BRNO,
                      TRSFILE.TS_DATE,
                      TRSFILE.TS_CLASS,
                      TRSFILE.TS_ACCTNO,
                      TRSFILE.TS_LAWCODE,
                      TRSFILE.TS_LINE			  
           END-EXEC.

           EXEC SQL
               OPEN TRS1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QTRS1-WBEG-BRNO
                          QTRS1-WEND-BRNO
                          QTRS1-WBEG-CLASS
                          QTRS1-WEND-CLASS
                          QTRS1-WBEG-ACCTNO
                          QTRS1-WEND-ACCTNO
                          QTRS1-WBEG-LAWCODE
                          QTRS1-WEND-LAWCODE
                          QTRS1-WBEG-LINE
                          QTRS1-WEND-LINE.
           MOVE SPACES TO QTRS1-WBEG-DATE
                          QTRS1-WEND-DATE.


           MOVE STAT---GOOD TO TRS1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-TRS1-FILE.
           PERFORM READ-IT.
           MOVE TRS-PATH-OWNBR  TO TS-BRNO. 
           MOVE TRS-SELECT-PATH TO E-FILE.
           MOVE TRS1-KEY        TO E-KEYX.

           MOVE TS-BRNO             TO QTS-BRNO.
           MOVE TS-DATE             TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTS-DATE.
           MOVE TS-CLASS            TO QTS-CLASS.
           MOVE TS-ACCTNO           TO QTS-ACCTNO.
           MOVE TS-LAWCODE          TO QTS-LAWCODE.
           MOVE TS-LINE             TO QTS-LINE.


           EXEC SQL
            SELECT TRSFILE.TS_BRNO,
                   ISNULL(CAST(TRSFILE.TS_DATE AS VARCHAR(10)),'0'),
                   TRSFILE.TS_CLASS,
                   TRSFILE.TS_ACCTNO,
                   TRSFILE.TS_LAWCODE,
                   TRSFILE.TS_LINE,
                   ISNULL(TRSFILE.TS_SRCD,''),
                   ISNULL(TRSFILE.TS_FEECODE_1,''),
                   ISNULL(TRSFILE.TS_FEECODE_2,''),
                   ISNULL(TRSFILE.TS_FEECODE_3,''),
                   ISNULL(TRSFILE.TS_FEECODE_4,''),
                   ISNULL(TRSFILE.TS_FEECODE_5,''),
                   ISNULL(TRSFILE.TS_FEECODE_6,''),
                   ISNULL(TRSFILE.TS_FEECODE_7,''),
                   ISNULL(TRSFILE.TS_FEECODE_8,''),
                   ISNULL(TRSFILE.TS_FEECODE_9,''),
                   ISNULL(TRSFILE.TS_FEECODE_10,''),
                   ISNULL(TRSFILE.TS_FEECODE_11,''),
                   ISNULL(TRSFILE.TS_FEECODE_12,''),
                   ISNULL(TRSFILE.TS_FEECODE_13,''),
                   ISNULL(TRSFILE.TS_FEECODE_14,''),
                   ISNULL(TRSFILE.TS_FEECODE_15,''),
                   ISNULL(TRSFILE.TS_FEECODE_16,''),
                   ISNULL(TRSFILE.TS_FEECODE_17,''),
                   ISNULL(TRSFILE.TS_FEECODE_18,''),
                   ISNULL(TRSFILE.TS_FEECODE_19,''),
                   ISNULL(TRSFILE.TS_FEECODE_20,''),
                   TRSFILE.TS_FEEAMT_1,
                   TRSFILE.TS_FEEAMT_2,
                   TRSFILE.TS_FEEAMT_3,
                   TRSFILE.TS_FEEAMT_4,
                   TRSFILE.TS_FEEAMT_5,
                   TRSFILE.TS_FEEAMT_6,
                   TRSFILE.TS_FEEAMT_7,
                   TRSFILE.TS_FEEAMT_8,
                   TRSFILE.TS_FEEAMT_9,
                   TRSFILE.TS_FEEAMT_10,
                   TRSFILE.TS_FEEAMT_11,
                   TRSFILE.TS_FEEAMT_12,
                   TRSFILE.TS_FEEAMT_13,
                   TRSFILE.TS_FEEAMT_14,
                   TRSFILE.TS_FEEAMT_15,
                   TRSFILE.TS_FEEAMT_16,
                   TRSFILE.TS_FEEAMT_17,
                   TRSFILE.TS_FEEAMT_18,
                   TRSFILE.TS_FEEAMT_19,
                   TRSFILE.TS_FEEAMT_20
              INTO 
                  :QTS-BRNO,
                  :QTS-DATE,
                  :QTS-CLASS,
                  :QTS-ACCTNO,
                  :QTS-LAWCODE,
                  :QTS-LINE,
                  :QTS-SRCD, 
                  :QTS-FEECODE-1, 
                  :QTS-FEECODE-2, 
                  :QTS-FEECODE-3, 
                  :QTS-FEECODE-4, 
                  :QTS-FEECODE-5, 
                  :QTS-FEECODE-6, 
                  :QTS-FEECODE-7, 
                  :QTS-FEECODE-8, 
                  :QTS-FEECODE-9, 
                  :QTS-FEECODE-10, 
                  :QTS-FEECODE-11, 
                  :QTS-FEECODE-12, 
                  :QTS-FEECODE-13, 
                  :QTS-FEECODE-14, 
                  :QTS-FEECODE-15, 
                  :QTS-FEECODE-16, 
                  :QTS-FEECODE-17, 
                  :QTS-FEECODE-18, 
                  :QTS-FEECODE-19, 
                  :QTS-FEECODE-20, 
                  :QTS-FEEAMT-1,
                  :QTS-FEEAMT-2,
                  :QTS-FEEAMT-3,
                  :QTS-FEEAMT-4,
                  :QTS-FEEAMT-5,
                  :QTS-FEEAMT-6,
                  :QTS-FEEAMT-7,
                  :QTS-FEEAMT-8,
                  :QTS-FEEAMT-9,
                  :QTS-FEEAMT-10,
                  :QTS-FEEAMT-11,
                  :QTS-FEEAMT-12,
                  :QTS-FEEAMT-13,
                  :QTS-FEEAMT-14,
                  :QTS-FEEAMT-15,
                  :QTS-FEEAMT-16,
                  :QTS-FEEAMT-17,
                  :QTS-FEEAMT-18,
                  :QTS-FEEAMT-19,
                  :QTS-FEEAMT-20
             FROM DBO.TRSFILE
            WHERE TRSFILE.TS_BRNO    = :QTS-BRNO
              AND TRSFILE.TS_DATE    = :QTS-DATE
              AND TRSFILE.TS_CLASS   = :QTS-CLASS
              AND TRSFILE.TS_ACCTNO  = :QTS-ACCTNO
              AND TRSFILE.TS_LAWCODE = :QTS-LAWCODE
              AND TRSFILE.TS_LINE    = :QTS-LINE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TRS1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-TRS-FIELDS.

      *-----------------------------------------------------------------
       READ-TRS1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE TRS-PATH-OWNBR  TO TS-BRNO.
           MOVE TRS-SELECT-PATH TO E-FILE.
           MOVE TRS1-KEY        TO E-KEYX.
           INITIALIZE QTRS-REC.
 

           IF ( TRS1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH TRS1_CURSOR INTO  
                     :QTS-BRNO,
                     :QTS-DATE,
                     :QTS-CLASS,
                     :QTS-ACCTNO,
                     :QTS-LAWCODE,
                     :QTS-LINE,
                     :QTS-SRCD, 
                     :QTS-FEECODE-1, 
                     :QTS-FEECODE-2, 
                     :QTS-FEECODE-3, 
                     :QTS-FEECODE-4, 
                     :QTS-FEECODE-5, 
                     :QTS-FEECODE-6, 
                     :QTS-FEECODE-7, 
                     :QTS-FEECODE-8, 
                     :QTS-FEECODE-9, 
                     :QTS-FEECODE-10, 
                     :QTS-FEECODE-11, 
                     :QTS-FEECODE-12, 
                     :QTS-FEECODE-13, 
                     :QTS-FEECODE-14, 
                     :QTS-FEECODE-15, 
                     :QTS-FEECODE-16, 
                     :QTS-FEECODE-17, 
                     :QTS-FEECODE-18, 
                     :QTS-FEECODE-19, 
                     :QTS-FEECODE-20, 
                     :QTS-FEEAMT-1,
                     :QTS-FEEAMT-2,
                     :QTS-FEEAMT-3,
                     :QTS-FEEAMT-4,
                     :QTS-FEEAMT-5,
                     :QTS-FEEAMT-6,
                     :QTS-FEEAMT-7,
                     :QTS-FEEAMT-8,
                     :QTS-FEEAMT-9,
                     :QTS-FEEAMT-10,
                     :QTS-FEEAMT-11,
                     :QTS-FEEAMT-12,
                     :QTS-FEEAMT-13,
                     :QTS-FEEAMT-14,
                     :QTS-FEEAMT-15,
                     :QTS-FEEAMT-16,
                     :QTS-FEEAMT-17,
                     :QTS-FEEAMT-18,
                     :QTS-FEEAMT-19,
                     :QTS-FEEAMT-20
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TRS1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-TRS-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-TRS1-FILE.

           PERFORM CLOSE-IT.
           IF ( TRS1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE TRS1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO TRS1-CURSOR-STAT.	  
	  

      *-----------------------------------------------------------------
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************
       END-RUN.
           PERFORM CLOSE-FILES.

           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.

           IF NO-RECORDS-FOUND
              IF EXT-ROUTE-BUF = "00" OR "70"
                 MOVE "NO REPORT FOR SELECTED RANGE" TO MESS
                 PERFORM SEND-MESS.

           MOVE WK1-PATH TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.
           MOVE WK2-PATH TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.

      *----------------------------------------------------------------
       EXIT-PROG.

           IF SV-BEG-BR NOT = 9999
              IF EXT-EOBUF-GROUP-FG = "Y"
                 MOVE SV-BEG-BR TO BEG-BRANCH
                 MOVE SV-END-BR TO END-BRANCH
                 MOVE SV-GROUP-FLAG TO GROUP-FLAG
                 MOVE SV-ENT-GROUP TO ENT-GROUP
              ELSE
                 MOVE SV-BEG-BR TO BEG-BRANCH
                 MOVE SV-END-BR TO END-BRANCH.

           IF EXT-ROUTE-BUF = "00"
              MOVE "LP/EMICMU" TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY NFMORP-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
