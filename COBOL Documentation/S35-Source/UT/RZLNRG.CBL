       IDENTIFICATION DIVISION.
       PROGRAM-ID.   AULNRG.
       DATE-WRITTEN. 042084
      ******************************************************************
      *                         (LP)
      *    DESCRIPTION:  AUDITOR LOAN REGISTER
      *                        (LNFILE)
      * WALKS ACROSS BRANCHES
      *
      *=================================================================
      * REV:
      * JTG 051297 CORRECTED TESTS ON PBD & PBI ACCOUNTS
      * MJD 011597 CONVERTED FOR A10, ADDED A10 DATE ROUTINES
      *            CHANGED USERID TO PIC 10
      *            ADDED O3 TO TOTALS AND DETAIL
      * CS  042798 ADDED "RB" AND "RO" WHERE PB/RN/SC TESTED
      * BAH 062598 LN-TILLNO CHANGED TO ALPHA, CHANGED D-TILLNO
      * MJD 990806 ADDED GP-INCL-OTH-IN-EARNED FLAG. IF "Y" THEN INCLUDE
      *            EARNINGS BUCKET 6 (OTHER) IN ACCUMULATED TOTALS
      *            OTHERWISE EXCLUDE BUCKET 6.
      * GLF 990923 REPLACED IO-FG NUMBERS WITH IO-FG 88 LEVEL PER DEVELOPMENT
      *            MEETING (082599)
      * BAH 991230 ADDED "MOVE "N" TO GROUP-FLAG" IN THE ALL-BR PARAGRAPH
      *            WAS GETTING "ERROR READING GRFILE" AT ARMED SERVICES..JUDY
      * BLV 000607 ADD AMT FINANCED RANGE, PRINT COLLATERAL OPTION,
      *            WORLD PR# 228, CHANGED VERSION TO "E"
      * BAH 000919 ADDED NEW BORROWER I,S,O AND FORMER BORROWER I,S,O FOR
      *            WORLD PR# 236
      * CS  020306 ADDED BEG/END PROCEEDS RANGE [WORLD PR# 293]
      * BLV 021022 LT-ALL-PATH NOT SET! WRONG COLLATERAL PRINTING,
      *            REGENCY
      * BLV 021205 FIX BUG IN TYPE TOTALS WHICH DIDN'T CHECK WK-BR SO
      *            CAUSED A BUNCH OF REPEATED TOTALS WHEN RUNNING BY
      *            GROUP & A LOWER # BRANCH OCCURS IN GROUP, WORLD
      * BAH 040812 ADDED TEST OF BW-CONFIDENTIAL, THIS FLAG OVERRIDES THE
      *            LOAN FLAG, IF THE LOAN IS "N", BUT THE BW IS "Y", THEN
      *            ITS CONFIDENTIAL ! PR# 2343
      * BAH 041223 PRINT POSTING TIME & USERID UNDER PRIOR LN#, WORLD PR# 365
      *  CS 081107 ADDED TEST ON GP-INCL-OTH-IN-EARNED = 'R'. IF GP-INCL-OTH
      *            IN-EARNED-R, DONT INCLUDE OTHER(ANTICERN(6) OR
      *            MAINTFEE(ANTICERN(4) IN EARNED/UNEARNED. REGACC USES
      *            MAINTFEE EARN TO AMORTIZE PARTIC HELD EXPENSE REGACC PR#210
      *  CS 090302 ADDED OPTION TO I/S/O VOIDED LOANS WORLD PR#630
      *            CHANGED VERSION CONTROL TO 'F'.
      * BAH 090303 IF VOIDED LOAN, PRINT THE VOIDED DATE (POFFDATE) INSTEAD
      *            OF PRIOR LOAN #, JIMR
      * BAH 100421 ALLOW P&L ACCOUNTS TO BE 'PBD' OR 'PBI', COMMENTED
      *            OUT P&L TEST IN THE SET-ACTIVITY-SUB ROUTINE. WORLD PR#683
      * BAH 100514 ALLOW "B" BOTH FOR PBD OPTION TO INCLUDE ONLY PBD AND
      *            PBI ACCOUNTS, ALSO TEST SPR VALUES FOR PBI OPTIONS
      *                                            WORLD PR# 688
      * BAH 150616 INCREASED TOTALS FOR LNAMT AND MFEE AND SKIP
      *            TRANSFERRED ACCOUNTS IN NEW BRANCH, WORLD PR# 1089
      * BAH 150618 DO NOT ADD LN-ANTICADJ FOR "BT" POCD, WORLD PR# 1089B
      * BAH 160118 SPLIT OUT LTFILE, PD0003
      *
      * KEC 2016.0315 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED CD-FEE???? TO CD-GC-????.
      *   CS 160830 CHANGED  WK-CLASS FROM 9(2) TO 9(3)
      *             SAVE-WK-CL TO    PIC 9(3).
      *             HOLD-CLASS TO     PIC 999   
      * BAH 170320 ACTIVATE 8 DIGIT DATES, PD0006
      *
      * KEC 2017.0605 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          ADDED 'LN' FILE PREFIX FOR WORKING STORAGE FIELDS.
      *   CS 170918 WORLD WILL BE CREATING NEW SOURCE CODES TO USE WITH
      *             WEB BUSINESS AND REPORT NEEDS TO ACCOMODATE NEW SOURCE
      *             CODES.NEW FB CODES ARE 'LF','WF', AND ANY CODE THAT 
      *             CONTAINS AN 'F' IN ANY POSITION  WORLD PR#1277
      *   CS 171106 SOURCE CODE 'RF' WAS NOT BEING VIEWED AS NEW BORROWER
      *             DONT PERFORM CHECK-FCOUNT IF RF,FB,LF,WF,PB,RN,SC,RB,RO
      *   CS 171107 FORMER BORROWER CODES LF AND WF  SKIPPED BECAUSE
      *             NOT PERFORMING  CHECK-FCOUNT FOR FB,WF,LF NOW IF ITS
      *             A KNOWN FB CODE, FORCE 1 TO FCOUNT PR#1284
      * BAH 181203 GLOBAL LNFILE, L2G
      * BAH 20190920 REMOVED ACCESS-CALL ON LNFILE
      * BAH 20200827 REMOVED RESET ON BR1 
      * BAH 20200902 WAS MISSING PR#1284 CHANGE ABOVE.
      * BAH 20220805 HARD CODED START
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
       COPY "LIBGB/GBFSWKI.CPY".
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".

      *-----------------------------------------------------------------
      * SUMMARY TOTALS WORK RECORD
      *-----------------------------------------------------------------
       FD  WK-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK-REC.
           03  WK-KEY.
               05  WK-BR    PIC 9(4).
               05  WK-CLASS PIC 9(3).
               05  WK-CODE  PIC X(3).
           03  WK-NOLOANS   PIC S9(7)     COMP-3.
           03  WK-LNAMT     PIC S9(9)V99  COMP-3.
           03  WK-INTCHG    PIC S9(9)V99  COMP-3.
           03  WK-SERCHG    PIC S9(7)V99  COMP-3.
           03  WK-EXTCHG    PIC S9(7)V99  COMP-3.
           03  WK-MFEE      PIC S9(7)V99  COMP-3.
           03  WK-STATE     PIC S9(7)V99  COMP-3.
           03  WK-EARNINGS  PIC S9(9)V99  COMP-3.
           03  WK-DL        PIC S9(7)V99  COMP-3.
           03  WK-LL        PIC S9(7)V99  COMP-3.
           03  WK-AH        PIC S9(7)V99  COMP-3.
           03  WK-PP        PIC S9(7)V99  COMP-3.
           03  WK-O1        PIC S9(7)V99  COMP-3.
           03  WK-O2        PIC S9(7)V99  COMP-3.
           03  WK-O3        PIC S9(7)V99  COMP-3.
           03  WK-FEES      PIC S9(7)V99  COMP-3.
           03  WK-PROCEEDS  PIC S9(7)V99  COMP-3.
           03  WK-PROCASH   PIC S9(7)V99  COMP-3.

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)UT/RZLNR9 S35 07/10/23-09:37:54 >".
       COPY "LIBLP/LP01CDB.CPY".
       COPY "LIBLP/LP01CDB_SQL.CPY".
       COPY "LIBLP/LPWSCDB.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01LP.CPY".
       COPY "LIBLP/LP01LP_SQL.CPY".
       COPY "LIBLP/LPWSLP.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01LC.CPY".
       COPY "LIBLP/LP01LC_SQL.CPY".
       COPY "LIBLP/LPWSLC.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBLP/LP01LTC.CPY".
       COPY "LIBLP/LP01LTC_SQL.CPY".
       COPY "LIBLP/LPWSLTC.CPY".
       01  WK-IFN               PIC X(2)     VALUE "WK".


      *-----------------------------------------------------------------
      * LINE WORKERS
      *-----------------------------------------------------------------
       01  LN-WORKERS.
           03  LN-FEED          PIC 999      VALUE 0.
           03  LN-COUNT         PIC 999      VALUE 99.
           03  LN-WK            PIC 999      VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

       01  ER-FEED              PIC 9        VALUE 1.

      *-----------------------------------------------------------------
      * MISC WORKERS
      *-----------------------------------------------------------------
       01  MSEL-LIST            PIC X(5)      VALUE "MSEL.".
       01  SUB                  PIC 9(2)  COMP.
       01  I-SUB                PIC 9(6)  COMP.
       01  LEV                  PIC 9(2)  COMP.
       01  ACT-SUB              PIC 9(2)  COMP.
       01  PBD-PBI              PIC X     VALUE "N".
       01  FINCHG               PIC S9(6)V99.
       01  SRCD-WORKER          PIC S9(6)V99 COMP-3.

       01  HOLD-BRNO            PIC 9(4)  VALUE 0.
       01  HOLD-BRNAME          PIC X(20) VALUE SPACES.
       01  HOLD-LN-CLASS        PIC 9(3)  VALUE 0.

       01  RECORDS-FOUND        PIC X     VALUE " ".
           88  NO-RECORDS-FOUND           VALUE "N".
           88  SKIP-RECORDS-FOUND         VALUE " ".

       01  C-S-Z           PIC X(66).
       01  STATE           PIC XX.
       01  ZIP             PIC X(10).

       01  SAVE-WK-KEY.
           03  SAVE-WK-BR       PIC 9(4).
           03  SAVE-WK-CL       PIC 9(3).
           03  SAVE-WK-CODE     PIC XXX.

       01  SUMMARY-FG           PIC X     VALUE "N".

       01  FIRST-TIME           PIC X        VALUE "Y".
       01  NEW-BRANCH           PIC X        VALUE "Y".
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".
       01  FCOUNT               PIC 9        VALUE ZEROS.
      *-----------------------------------------------------------------
      *  TOTALS LEV1 = CLASS TOTALS
      *            2 = BRANCH TOTALS
      *            3 = GRAND TOTALS
      *-----------------------------------------------------------------
       01  TOTALS-TAB.
           03  TOTAL-TBL       OCCURS 3.
               05  T-NOLOANS       PIC S9(9)     COMP-3.
               05  T-LNAMT         PIC S9(11)V99  COMP-3.
               05  T-INTCHG        PIC S9(11)V99  COMP-3.
               05  T-SERCHG        PIC S9(11)V99  COMP-3.
               05  T-EXTCHG        PIC S9(11)V99  COMP-3.
               05  T-MFEE          PIC S9(11)V99  COMP-3.
               05  T-STATE         PIC S9(11)V99  COMP-3.
               05  T-EARNINGS      PIC S9(11)V99  COMP-3.
               05  T-DL            PIC S9(11)V99  COMP-3.
               05  T-LL            PIC S9(11)V99  COMP-3.
               05  T-AH            PIC S9(11)V99  COMP-3.
               05  T-PP            PIC S9(11)V99  COMP-3.
               05  T-O1            PIC S9(11)V99  COMP-3.
               05  T-O2            PIC S9(11)V99  COMP-3.
               05  T-O3            PIC S9(11)V99  COMP-3.
               05  T-FEES          PIC S9(11)V99  COMP-3.
               05  T-PROCEEDS      PIC S9(11)V99  COMP-3.
               05  T-PROCASH       PIC S9(11)V99  COMP-3.

       01  NUM-BW               PIC 9(10).

       01  LEGEND               PIC X(70).
       COPY "LIBLP/EMSFRG_CMD.CPY".
       01  LEGEND-DTE-01.
           03  FILLER           PIC X(41)  VALUE
               " F7-EXIT, ^, V, ENTER BEGINNING LOANDATE:".
       01  LEGEND-DTE-02.
           03  FILLER           PIC X(38)  VALUE
               " F7-EXIT, ^, V, ENTER ENDING LOANDATE:".
       01  LEGEND-AC-01.
           03  FILLER           PIC X(40)  VALUE
               " F7-EXIT, ^, V, ENTER BEGINNING ACCOUNT ".
           03  FILLER           PIC X(17)  VALUE
               "NUMBER. (F3-ALL):".
       01  LEGEND-AC-02.
           03  FILLER           PIC X(37)  VALUE
               " F7-EXIT, ^, V, ENTER ENDING ACCOUNT ".
           03  FILLER           PIC X(8)  VALUE
               "NUMBER.:".
       01  LEGEND-CONFID.
           03  FILLER           PIC X(38)  VALUE
               "F7-EXIT, ^, V, ENTER CONFIDENTIAL CODE".
           03  FILLER           PIC X(9) VALUE
               " (I/S/O):".
       01  LEGEND-PBD.
           03  FILLER           PIC X(35)  VALUE
               "F7-EXIT, ^, V, ENTER PB DELINQUENT ".
           03  FILLER           PIC X(10) VALUE
               "(I/S/O/B):".
       01  LEGEND-PBI.
           03  FILLER           PIC X(33)  VALUE
               "F7-EXIT, ^, V, ENTER PB INCREASE ".
           03  FILLER           PIC X(8) VALUE
               "(I/S/O):".
       01  LEGEND-NB.
           03  FILLER           PIC X(33)  VALUE
               "F7-EXIT, ^, V, ENTER NB BORROWER ".
           03  FILLER           PIC X(8) VALUE
               "(I/S/O):".
       01  LEGEND-FB.
           03  FILLER           PIC X(33)  VALUE
               "F7-EXIT, ^, V, ENTER FB BORROWER ".
           03  FILLER           PIC X(8) VALUE
               "(I/S/O):".
       01  LEGEND-VOID.
           03  FILLER           PIC X(37)  VALUE
               "F7-EXIT, ^, V, ENTER VOIDED ACCOUNTS ".
           03  FILLER           PIC X(8) VALUE
               "(I/S/O):".
       01  LEGEND-FINANCED-01.
           03  FILLER           PIC X(41)  VALUE
               " F7-EXIT, ^, V, ENTER BEGINNING FINANCED ".
           03  FILLER           PIC X(17)  VALUE
               "AMOUNT  (F3-ALL):".
       01  LEGEND-FINANCED-02.
           03  FILLER           PIC X(38)  VALUE
               " F7-EXIT, ^, V, ENTER ENDING FINANCED ".
           03  FILLER           PIC X(20)  VALUE
               "AMOUNT          :".
      *-----------------------------------------------------------------
      *     S C R E E N   F I E L D S   *
      *-----------------------------------------------------------------
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  BEG-BRANCH      PIC 9(4).
           03  END-BRANCH      PIC 9(4).
           03  BEG-CLASS       PIC 9(3).
           03  END-CLASS       PIC 9(3).
           03  BEG-ACCTNO      PIC 9(6).
           03  END-ACCTNO      PIC 9(6).
           03  BEG-LOANDATE    PIC 9(8).
           03  END-LOANDATE    PIC 9(8).
           03  CONFIDENTIAL    PIC X.
           03  PBD             PIC X.
           03  PBI             PIC X.
           03  INCL-DETAIL     PIC X.
           03  GROUP-FLAG      PIC X.
           03  ENT-GROUP       PIC X(4).
           03  LOCAL-PRINT-FG  PIC X.
           03  BEG-FINANCED-AMT PIC 9999999.
           03  END-FINANCED-AMT PIC 9999999.
           03  PRCOL-FG         PIC X.
           03  NB-BORROWER      PIC X.
           03  FB-BORROWER      PIC X.
           03  BEG-PROCEEDS-AMT PIC 9999999.
           03  END-PROCEEDS-AMT PIC 9999999.
           03  INCL-VOIDS       PIC X.

       01  VERSION-CONTROL     PIC X VALUE "G".

       01  SV-BEG-BR           PIC 9999  VALUE 9999.
       01  SV-END-BR           PIC 9999  VALUE 9999.

      *-----------------------------------------------------------------
      *     P R I N T   B U F F E R S   *
      *-----------------------------------------------------------------

       01  HEAD1.
           03  H-DATE           PIC X(8)     VALUE " ".
           03  FILLER           PIC XX       VALUE " ".
           03  H-HR             PIC 99       VALUE 0.
           03  FILLER           PIC X        VALUE ":".
           03  H-MIN            PIC 99       VALUE 0.
           03  FILLER           PIC X(31)    VALUE " ".
           03  H-NAME           PIC X(40)    VALUE " ".
           03  FILLER           PIC X(37)    VALUE " ".
           03  FILLER           PIC X(5)     VALUE "PAGE ".
           03  H-PAGE-NO        PIC ZZZ9.

       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(36)    VALUE " ".
           03  FILLER           PIC X(21) VALUE
               "AUDITOR LOAN REGISTER".

       01  HEAD3.
           03  FILLER           PIC X(10) VALUE "BRANCH :".
           03  H-BRNO           PIC ZZZ9.
           03  FILLER           PIC X VALUE " ".
           03  H-BRDESC         PIC X(20).

       01  HEAD4.
           03  FILLER           PIC X(10) VALUE "CLASS  :".
           03  H-LCNO           PIC 99.
           03  FILLER           PIC X VALUE " ".
           03  H-LCDESC         PIC X(20).

       01  HEAD5.
           03  FILLER           PIC X(47)  VALUE " ".
           03  FILLER           PIC X(31)  VALUE
           "LOAN-AMT   FIN-CHG   INSURANCES".
           03  FILLER           PIC X(53)  VALUE
           " PRIOR-LN PRIOR-BAL INT-REB ----- FEES ----- PROCEEDS".

       01  HEAD6.
           03  FILLER           PIC X(48)  VALUE
           "ACCOUNT NO.   LOAN-DATE   APR  SC  CC PC  TRM   ".
           03  FILLER           PIC X(57)  VALUE
           " INTCHG  MAINTFEE               POSTED  INSUR-REB INT-DUE".
           03  FILLER           PIC X(26)  VALUE
           "   GL-NO  AMOUNT  PRO-CASH".
       01  HEAD6A.
           03  FILLER           PIC X(49)  VALUE " ".
           03  FILLER           PIC X(16)  VALUE
           "SERCHG  EARNINGS".
      *-----------------------------------------------------------------
      *    USED FOR STATE OF GA
      *-----------------------------------------------------------------
       01  HEAD6B.
           03  FILLER           PIC X(49)  VALUE " ".
           03  FILLER           PIC X(16)  VALUE
           "SERCHG  8%SERCHG".

       01  HEAD7.
           03  FILLER            PIC X(2) VALUE SPACES.
           03  FILLER            PIC X(56) VALUE
           "              LOAN      INTCHRG    8%SRVCH    EXTCHG    ".
           03  FILLER            PIC X(53) VALUE
           "OUR-DL    OUR-AH    OUR-O1    OUR-O3                 ".
           03  FILLER            PIC X(20) VALUE SPACES.

       01  HEAD8.
           03  FILLER            PIC X(2) VALUE " ".
           03  FILLER            PIC X(56) VALUE
           "LOANS        AMOUNT        MFEE    SRVCHRG              ".
           03  FILLER            PIC X(53) VALUE
           "OUR-LL    OUR-PP    OUR-O2    O-FEES      EARNINGS   ".
           03  FILLER            PIC X(20) VALUE
           "PROCASH     PROCEEDS".

       01  DETAIL-LINE          PIC X(132)  VALUE SPACES.
       01  DETAIL1 REDEFINES DETAIL-LINE.
           03  D-CLASS          PIC 99.
           03  D-H1             PIC X.
           03  D-NUMBER         PIC 9(6).
           03  D-H2             PIC X.
           03  D-SPCLASS        PIC 99.
           03  FILLER           PIC XX.
           03  D-LNDTE          PIC 99/99/99  BLANK ZERO.
           03  FILLER           PIC X.
           03  D-APR            PIC ZZ9.99   BLANK ZERO.
           03  FILLER           PIC XX.
           03  D-SRCD           PIC XXX.
           03  FILLER           PIC X.
           03  D-COLCD          PIC XX.
           03  FILLER           PIC X.
           03  D-PURCD          PIC XXX.
           03  FILLER           PIC X.
           03  D-TERM           PIC ZZ9.
           03  FILLER           PIC X.
           03  D-LNAMT          PIC Z(5)9.99-  BLANK ZERO.
           03  D-FINCHG         PIC Z(5)9.99-  BLANK ZERO.
           03  D-INSPREM        PIC Z(4)9.99-  BLANK ZERO.
           03  D-INSFLAG        PIC X.
           03  D-INSTYPE        PIC X.
           03  D-INSOURS        PIC X.
           03  FILLER           PIC X.
           03  D-USERID.
               05  D-PRLNNO     PIC Z(6)   BLANK ZERO.
               05  FILLER       PIC XX.
           03  D-TIME REDEFINES D-USERID.
               05  D-HH         PIC 99.
               05  D-MM         PIC 99.
               05  D-AMPM       PIC XX.
               05  FILLER       PIC XX.
           03  D-POFFDATE REDEFINES D-USERID PIC 99/99/99 BLANK ZERO.
           03  FILLER           PIC X.
           03  D-PRBAL          PIC Z(5)9.99- BLANK ZERO.
           03  D-INTRBS         PIC Z(3)9.99- BLANK ZERO.
           03  D-FEEGLNO        PIC X(8).
           03  D-FEEAMT         PIC Z(4).99-  BLANK ZERO.
           03  D-PROCEEDS       PIC Z(6).99-  BLANK ZERO.
           03  D-PROCEEDSX REDEFINES D-PROCEEDS.
               05  FILLER       PIC X(8).
               05  D-TILLNO     PIC X.
               05  FILLER       PIC X.

       01  DETAIL2 REDEFINES DETAIL-LINE.
           03  FILLER           PIC X.
           03  D-MAKERCD        PIC X.
           03  FILLER           PIC X.
           03  D-BW-SSNO        PIC 999/99/9999.
           03  FILLER           PIC X.
           03  D-BW-NAME        PIC X(30).

       01  DETAIL3 REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(2).
           03  D-BW-ADDR        PIC X(66).

      *-----------------------------------------------------------------
      * DETAIL LINE FOR OUTPUT OF AUTOMOTIVE COLLATERAL
      *-----------------------------------------------------------------
       01  D-LINE1  REDEFINES DETAIL-LINE.
           03  FILLER         PIC X.
           03  FILLER         PIC X(17).
           03  D-FIL-MAKE     PIC X(7).
           03  D-MAKE         PIC X(15).
           03  FILLER         PIC XX.
           03  D-FIL-MODEL    PIC X(8).
           03  D-MODEL        PIC X(15).
           03  FILLER         PIC XX.
           03  D-FIL-YEAR     PIC X(7).
           03  D-YEAR         PIC X(4).
           03  FILLER         PIC XX.
           03  D-FIL-ID       PIC X(8).
           03  D-ID           PIC X(20).
           03  FILLER         PIC X(24).

       01  SUMHEAD1 REDEFINES DETAIL-LINE.
           03  S-DCODE      PIC XXX.
           03  S-COLON      PIC X.

       01  SUMLINE1 REDEFINES DETAIL-LINE.
           03  S-NOLOANS        PIC Z(6)9-  BLANK ZERO.
           03  S-LNAMT          PIC Z(9)9.99-  BLANK ZERO.
           03  S-INTCHG         PIC Z(7)9.99-  BLANK ZERO.
           03  S-SERCHG         PIC Z(6)9.99-  BLANK ZERO.
           03  S-EXTCHG         PIC Z(5)9.99-  BLANK ZERO.
           03  S-CL2            PIC Z(5)9.99-  BLANK ZERO.
           03  S-AH             PIC Z(5)9.99-  BLANK ZERO.
           03  S-OT             PIC Z(5)9.99-  BLANK ZERO.
           03  S-FEEAMT         PIC Z(5)9.99-  BLANK ZERO.
           03  S-EARNINGS       PIC Z(9)9.99-  BLANK ZERO.
           03  S-PROCASH        PIC Z(5)9.99-  BLANK ZERO.
           03  S-PROCEEDS       PIC Z(8)9.99-  BLANK ZERO.


       01  GRAND1 REDEFINES DETAIL-LINE.
           03  G-TOTNAME        PIC X(18).
           03  G-DESC           PIC X(23).
           03  G-AMT            PIC ZZZZZ,ZZZ,ZZ9.99-  BLANK ZERO.
           03  G-NODEC REDEFINES G-AMT.
               05  G-NOLOANS    PIC ZZZZZ,ZZZ,ZZ9-  BLANK ZERO.
               05  FILLER       PIC X(3).

      *-----------------------------------------------------------------
       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(35).
           03  RANGE-SELECTION.
               04  R-NUM        PIC Z(9)9.
               04  R-NUM-REDEF2 REDEFINES R-NUM.
                   05  R-DATE   PIC 9999/99/99.

       COPY "LIBWI/PFSWINDOWSW.CPY".
       COPY "LIBWI/TSTINTW.CPY".
       COPY "LIBWI/GRSCANW.CPY".

       COPY "LIBLP/ARRAYSPW.CPY". 
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/EOSPOOLW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBLP/LPAMTSW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/PASSWDW.CPY".

       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/AULNRG_DEF.CPY".
       COPY "LIBLP/AULNRG_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBLP/AULNRG_SCN.CPY".

      ******************************************************************
      * P R O G R A M   C O N T R O L
      ******************************************************************
       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE WK-FILE.
       COPY "LIBGB/DECLRP.CPY".
      *     PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LTC1-FILE.
           PERFORM CLOSE-LP1-FILE.
           PERFORM CLOSE-LN3-FILE.
           PERFORM CLOSE-LC1-FILE.
           PERFORM CLOSE-CDB1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE WK-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ******************************************************************
       MAIN-MODULE SECTION.
      ******************************************************************

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "AULNRG" TO FORM-PROG VDU-FORM-NAME.

           PERFORM GET-GPENV.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

           IF GROUP-FLAG = "N"
              MOVE BEG-BRANCH TO BR-NO
              PERFORM START-BR-FILE
              GO TO NEXT-BRANCH-NO.

           MOVE ENT-GROUP TO GR-ID.
           PERFORM SEARCH-GR-INIT.

       NEXT-GROUP-LOC.
           IF GROUP-FLAG NOT = "Y"
              GO TO NEXT-BRANCH-NO.
           PERFORM SEARCH-GR-NEXT.
           IF WGR-SRESULT = 0
              GO TO END-ROUTINE.

           MOVE WGR-SRESULT TO BR-NO.
           PERFORM READ-BR-FILE.
           IF NOT IO-GOOD
              GO TO IO-ERROR.

           GO TO MOVE-BRANCH-PATH-INFO.
       NEXT-BRANCH-NO.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-BAD OR BR-NO > END-BRANCH
              GO TO END-ROUTINE.

       MOVE-BRANCH-PATH-INFO.
           IF LOCAL-PRINT-FG = "Y"
              PERFORM CLOSE-PR-FILE
              PERFORM CREATE-SPOOL-DIR
              IF STAT NOT = "00"
                 GO TO NEXT-GROUP-LOC.

           MOVE "Y" TO NEW-BRANCH.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO BW-PATH-OVERRIDE
                                              LN-PATH-OVERRIDE
                                              LP-PATH-OVERRIDE
                                              LTC-PATH-OVERRIDE.

           PERFORM OPEN-BW1-FILE.
           PERFORM OPEN-LN3-FILE.
           PERFORM OPEN-LP1-FILE.
           PERFORM OPEN-LTC1-FILE.

           MOVE BR-NO      TO LN-OWNBR
                              QLN3-WBEG-OWNBR
                              QLN3-WEND-OWNBR.
           MOVE BEG-CLASS  TO LN-CLASS
                              QLN3-WBEG-CLASS.
           MOVE END-CLASS  TO QLN3-WEND-CLASS.
           MOVE BEG-ACCTNO TO LN-ACCTNO
                              QLN3-WBEG-ACCTNO.
           MOVE END-ACCTNO TO QLN3-WEND-ACCTNO.

           PERFORM START-LN3-FILE.

       NEXT-LN.
           PERFORM READ-LN3-FILE-NEXT.
           IF IO-BAD OR (LN-OWNBR NOT = LN-PATH-OWNBR)
              GO TO END-OF-GROUP.

           PERFORM TRAP-INTERRUPT.

           IF LN-OWNBR > END-BRANCH
              GO TO END-OF-GROUP.

           IF LN-CLASS > END-CLASS
              GO TO END-OF-GROUP.

      * SKIP TRANSFERRED ACCOUNTS IN THE NEW BRANCH, NEED TO MATCH STATS
      * VOLUME. THIS REPORT IS TO SHOW INFO REGARDING THE LOAN THAT WAS MADE.
      * THE NEW OWNING BRANCH DID NOT MAKE THE LOAN.

           IF LN-BR-TFR-FG = "Y"
              GO TO NEXT-LN.

           IF INCL-VOIDS = "S"
              IF LN-POCD = "**"
                GO TO NEXT-LN.

           IF INCL-VOIDS = "O"
              IF LN-POCD NOT = "**"
                GO TO NEXT-LN.

           IF LN-ACCTNO < BEG-ACCTNO OR LN-ACCTNO > END-ACCTNO
              GO TO NEXT-LN.

           IF LN-LOANDATE < BEG-LOANDATE OR LN-LOANDATE > END-LOANDATE
              GO TO NEXT-LN.

      * ANY LN-SRCD THAT CONTAINS AN 'F' IS A FORMER BORROWER
      * EXCEPT FOR 'RF' WHICH IS A NEW BORROWER
      * REGARDING THE S/I/O FOR THE 'NB' AND 'FB'.
      * IF ITS NOT A PB, OR AN IDENTIFIED 'FB' AND DOESNT  
      * CONTAIN AN 'F' IN ANY POSITION IS A 'NB'

           MOVE 0 TO FCOUNT.

           IF LN-SRCD = "FB" OR "LF" OR "WF"
              MOVE 1 TO FCOUNT
           ELSE
              IF NOT (LN-SRCD = "RF" OR "CP" OR "DM" OR "ML" OR "NB" 
                     OR "TX" OR "WB" OR "WR" OR "PB" OR "RN" OR "SC"
                     OR "RB" OR "RO")
                 PERFORM CHECK-FCOUNT. 
              
      * TEST NEW BORROWERS

           IF NB-BORROWER = "O"
              IF LN-SRCD = "FB" OR "LF" OR "WF" OR
                           "PB" OR "RN" OR "SC" OR
                           "RB" OR "RO" OR FCOUNT > 0
                 GO TO NEXT-LN. 
              
           IF NB-BORROWER = "S"
              IF NOT (LN-SRCD = "FB" OR "PB" OR "RN" OR "SC" OR
                            "RB" OR "RO" OR "LF" OR "WF")
                             AND (FCOUNT = 0 )
                 GO TO NEXT-LN.

      * TEST FORMER BORROWERS

           IF FB-BORROWER = "O"
              IF LN-SRCD NOT = "FB"
                     AND NOT = "LF"
                     AND NOT = "WF"
                     IF ( FCOUNT NOT > 0)
                        GO TO NEXT-LN.

           IF FB-BORROWER = "S"
              IF LN-SRCD = "FB" OR "LF" OR "WF" OR  FCOUNT > 0
                 GO TO NEXT-LN.

           IF NOT (BEG-PROCEEDS-AMT = 0 AND END-PROCEEDS-AMT = 9999999)
             IF LN-PROCEEDS < BEG-PROCEEDS-AMT OR
                              LN-PROCEEDS > END-PROCEEDS-AMT
                GO TO NEXT-LN.

           PERFORM READ-SP-RECORD.

           IF NOT (BEG-FINANCED-AMT = 0 AND END-FINANCED-AMT = 9999999)
              PERFORM LOAN-CALCULATIONS
              IF FINANCED-AMOUNT < BEG-FINANCED-AMT OR
                  FINANCED-AMOUNT > END-FINANCED-AMT
                 GO TO NEXT-LN.

           PERFORM SET-ACTIVITY-SUB.

      * TEST PB DELINQUENTS

           IF PBD = "S"
              IF ACT-SUB = 5
                 GO TO NEXT-LN.

      * IF SELECTED "B" BOTH, YOU MUST COME OUT OF SET-ACTIVITY AS A 5
      * OR YOU WONT BE BOTH, SO CHECK THAT RIGHT HERE.
           IF PBD = "O" OR "B"
              IF ACT-SUB NOT = 5
                 GO TO NEXT-LN.

      * "B" WILL INCLUDE ONLY ACCOUNTS THAT ARE BOTH PBD AND PBI

      * IF STATUS IS PBD(5), NOW CHECK FOR PBI (6)
      * IF THE STATUS IS ALREADY PBI (6), YOU DO NOT NEED TO TEST FOR PBD
      *  BECAUSE IT WOULD HAVE CAME OUT OF THAT ROUTINE AS ACT-SUB 5 PBD
      *  IF IT WAS, THAT STATUS IS CHECKED FIRST AND THEN EXITS.

           IF PBD = "B"
              IF SP-PBINCR-PERCENT = 0
                 GO TO NEXT-LN
              ELSE
                 IF SP-PBINCR-PERCENT = 1
                    IF LN-LNAMT NOT > LN-PRLNAMT
                       GO TO NEXT-LN
                    ELSE
                       NEXT SENTENCE
                 ELSE
                    COMPUTE SRCD-WORKER ROUNDED =
                              LN-PRLNAMT - 0.01 + LN-PRLNAMT
                                   * SP-PBINCR-PERCENT / 100
                    IF LN-LNAMT NOT > SRCD-WORKER
                       GO TO NEXT-LN.

      * TEST PB INCREASES

           IF PBI = "S"
              IF ACT-SUB = 6
                 GO TO NEXT-LN.

           IF PBI = "O"
              IF ACT-SUB NOT = 6
                 GO TO NEXT-LN.

           MOVE LN-SSNO(1) TO BW-SSNO.
           PERFORM READ-BW1-FILE.
           IF IO-FG NOT = 0
              MOVE "N" TO BW-CONFIDENTIAL.

           IF CONFIDENTIAL = "S"
              IF (LN-CONFIDENTCD = "Y" OR BW-CONFIDENTIAL = "Y")
                 GO TO NEXT-LN.

      * BOTH FLAGS MUST NOT BE SET
           IF CONFIDENTIAL = "O"
              IF (LN-CONFIDENTCD NOT = "Y")
                 IF (BW-CONFIDENTIAL NOT = "Y")
                    GO TO NEXT-LN.

      * TEST FIRST TIME THROUGH

       CONTINUE-PROCESSING.
           IF FIRST-TIME = "Y"
              MOVE BR-NO TO HOLD-BRNO
              MOVE BR-NAME TO HOLD-BRNAME
              MOVE LN-CLASS TO HOLD-LN-CLASS
              PERFORM SET-CLASS-HEADINGS
              PERFORM SET-BRANCH-HEADINGS.

      * TEST BREAK IN BRANCH
           IF BR-NO NOT = HOLD-BRNO
              PERFORM CLASS-TOTALS
              PERFORM BRANCH-TOTALS
              MOVE BR-NO TO HOLD-BRNO
              MOVE BR-NAME TO HOLD-BRNAME
              MOVE LN-CLASS TO HOLD-LN-CLASS
              PERFORM SET-CLASS-HEADINGS
              PERFORM SET-BRANCH-HEADINGS
              PERFORM HEAD-PAGE.

      * TEST BREAK IN CLASS
           IF LN-CLASS NOT = HOLD-LN-CLASS
              PERFORM CLASS-TOTALS
              MOVE LN-CLASS TO HOLD-LN-CLASS
              PERFORM SET-CLASS-HEADINGS
              PERFORM HEAD-PAGE.

           MOVE LN-OWNBR  TO LP-BRNO.
           MOVE LN-ACCTNO TO LP-ACCTNO.
           MOVE 1         TO LP-SEQNO.
           PERFORM READ-LP1-FILE.
           IF IO-FG NOT = 0
              MOVE 0 TO LP-POSTTIME
              MOVE SPACES TO LP-USERID.

           PERFORM LN-DETAIL1.

           MOVE 1 TO LEV.
           PERFORM UPDATE-TABLE.
           PERFORM CREATE-WK.

           GO TO NEXT-LN.

       END-OF-GROUP.
           PERFORM CLOSE-LN3-FILE.
           PERFORM CLOSE-LP1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-LTC1-FILE.

           IF LOCAL-PRINT-FG = "Y"
              IF FIRST-TIME NOT = "Y"
                 PERFORM CLASS-TOTALS
                 PERFORM BRANCH-TOTALS
                 MOVE "Y" TO FIRST-TIME
                 MOVE 0 TO PAGE-NUMBER
              ELSE
                 MOVE 0 TO PAGE-NUMBER.

           GO TO NEXT-GROUP-LOC.

       END-ROUTINE.
           IF FIRST-TIME = "Y"
              GO TO END-PROGRAM.
           PERFORM CLASS-TOTALS.
           PERFORM BRANCH-TOTALS.

      * DONT WASTE ANOTHER PAGE OF TOTALS IF RUNNING FOR 1 BRANCH

           IF GROUP-FLAG NOT = "Y"
              IF BEG-BRANCH = END-BRANCH
                 PERFORM PRINT-RANGE-LINES
                 GO TO END-PROGRAM.

           IF LOCAL-PRINT-FG = "N"
              PERFORM GRAND-TOTALS
              PERFORM PRINT-RANGE-LINES.
           GO TO END-PROGRAM.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************
           MOVE AULNRG-QUEUE-POS TO Q-POS.
           MOVE AULNRG-COPIES-POS TO C-POS.
           MOVE AULNRG-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           PERFORM CLEAR-TOTAL-TBL VARYING SUB FROM 1 BY 1 UNTIL 
                      SUB > 3.

           MOVE 0 TO PAGE-NUMBER.

      * CREATE WORK FILE FOR TOTALS
           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           PERFORM OPEN-WK-FILE-OUTPUT.
           PERFORM CLOSE-WK-FILE.
           PERFORM OPEN-WK-FILE.

           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           IF EXT-ROUTE-BUF = "00"
              MOVE "REPORT IN PROGRESS............" TO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           IF BEG-BRANCH = 0
              IF END-BRANCH = 0
                 MOVE BEG-BRANCH TO SV-BEG-BR
                 MOVE END-BRANCH TO SV-END-BR
                 MOVE EXT-CONAME-USER-LOC TO BEG-BRANCH
                                             END-BRANCH.

           IF LOCAL-PRINT-FG = "N"
              PERFORM OPEN-PR-FILE.

           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-BR-FILE.
   
           PERFORM ARRAYSP-LOAD.  
      *     PERFORM OPEN-SP1-FILE.

           PERFORM OPEN-CDB1-FILE.
           PERFORM OPEN-LC1-FILE.

           PERFORM ENABLE-INTERRUPT.
           MOVE "N" TO RECORDS-FOUND.
           GO TO EXIT-INITIALIZE.

       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.
       EXIT-INITIALIZE.
           EXIT.

      ******************************************************************
       EO-EXEC-AUDIT SECTION.
      ******************************************************************
           MOVE EXT-EOBUF-DATE1 TO BEG-LOANDATE.
           MOVE EXT-EOBUF-DATE2 TO END-LOANDATE.
       EO-EXEC-AUDIT-EXIT.
           EXIT.

      ******************************************************************
       ENTER-PARAMETERS SECTION.
      ******************************************************************
       ENTER-PARM.
           IF VDUSTATUS = "1U"
              GO TO PROCEEDS-AMT-02.

       BR-01.
           MOVE LEGEND-BR-01 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A000BR1." TO VDU.
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
              MOVE 0 TO WGR-SRESULT
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
       BRANCH-IS-NUMERIC.
           MOVE "N" TO GROUP-FLAG.
           MOVE WGR-SRESULT TO BEG-BRANCH.
           MOVE "SN N040BR1." TO VDU.
           MOVE BEG-BRANCH TO VDUNUM0.
           PERFORM SCREENIO.

       BR-02.
           MOVE LEGEND-BR-02 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN040BR2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BR-02.
           MOVE VDUNUM0 TO END-BRANCH.
           IF BEG-BRANCH > END-BRANCH
              MOVE "INVALID BRANCH RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO BR-01.
           GO TO CHECK-BRANCH-SECURITY.

       ALL-BR.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "N" TO GROUP-FLAG.
       CHECK-BRANCH-SECURITY.
           PERFORM BR-RANGE-SECURITY.

           IF BR-SECURITY-OKAY = "N"
              GO TO BR-01.

       CL-01.
           MOVE LEGEND-CL-01 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN020CL1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-CL.
           IF VDUSTATUS = "1U" GO TO BR-02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO CL-01.
           MOVE VDUNUM0 TO BEG-CLASS.
       CL-02.
           MOVE LEGEND-CL-02 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN020CL2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO CL-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO CL-02.
           MOVE VDUNUM0 TO END-CLASS.
           IF BEG-CLASS > END-CLASS
              MOVE "INVALID CLASS RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO CL-01.
           GO TO ACCT-01.
       ALL-CL.
           MOVE "SNNN020CL1." TO VDU.
           MOVE 0 TO BEG-CLASS VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020CL2." TO VDU.
           MOVE 99 TO END-CLASS VDUNUM0.
           PERFORM SCREENIO.
       ACCT-01.
           MOVE LEGEND-AC-01 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN060ACCT01." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-ACCT.
           IF VDUSTATUS = "1U" GO TO CL-02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ACCT-01.
           MOVE VDUNUM0 TO BEG-ACCTNO.
       ACCT-02.
           MOVE LEGEND-AC-02 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN060ACCT02." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ACCT-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ACCT-02.
           MOVE VDUNUM0 TO END-ACCTNO.
           IF END-ACCTNO < BEG-ACCTNO
              MOVE "INVALID ACCOUNT RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO ACCT-01.
           GO TO BEG-DATE.
       ALL-ACCT.
           MOVE "SNNN060ACCT01." TO VDU.
           MOVE 0 TO BEG-ACCTNO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN060ACCT02." TO VDU.
           MOVE 999999 TO END-ACCTNO VDUNUM0.
           PERFORM SCREENIO.
       BEG-DATE.
           IF EXT-ROUTE-BUF NOT = "00"
              GO TO CONFIDENTIAL-ISO.
           MOVE LEGEND-DTE-01 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNM060BEGDATE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ACCT-02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BEG-DATE.
           MOVE VDU-DATE-YYYYMMDD TO BEG-LOANDATE.
       END-DATE.
           IF EXT-ROUTE-BUF NOT = "00"
              GO TO CONFIDENTIAL-ISO.
           MOVE LEGEND-DTE-02 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNM060ENDDATE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BEG-DATE.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO END-DATE.
           MOVE VDU-DATE-YYYYMMDD  TO END-LOANDATE.
           IF END-LOANDATE < BEG-LOANDATE
              MOVE "END DATE CANNOT BE PRIOR TO BEGINNING DATE" TO MESS
              PERFORM SEND-MESS
              GO TO BEG-DATE.
       CONFIDENTIAL-ISO.
           MOVE LEGEND-CONFID TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A010CONFID." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U"
              IF EXT-ROUTE-BUF = "00"
                 GO TO END-DATE
              ELSE
                 GO TO ACCT-02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO CONFIDENTIAL-ISO.
           MOVE VDUBUF TO CONFIDENTIAL.
           IF NOT (CONFIDENTIAL = "I" OR "S" OR "O")
              GO TO CONFIDENTIAL-ISO.
       PBD-ISO.
           MOVE LEGEND-PBD TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A010PBD." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO CONFIDENTIAL-ISO
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO PBD-ISO.
           MOVE VDUBUF TO PBD.
           IF NOT (PBD = "I" OR "S" OR "O" OR "B")
              GO TO PBD-ISO.
      * THIS "B" WILL PULL ONLY PBD AND PBI ACCOUNTS. THEY MUST BE BOTH.
           IF PBD = "B"
              MOVE "S  A010PBI." TO VDU
              MOVE "I" TO PBI VDUBUF
              PERFORM SCREENIO
              MOVE "S  A010NB." TO VDU
              MOVE "S" TO NB-BORROWER VDUBUF
              PERFORM SCREENIO
              MOVE "S  A010FB." TO VDU
              MOVE "S" TO FB-BORROWER VDUBUF
              PERFORM SCREENIO
              GO TO INCLDETAIL.

       PBI-ISO.
           MOVE LEGEND-PBI TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A010PBI." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO PBD-ISO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO PBI-ISO.
           MOVE VDUBUF TO PBI.
           IF NOT (PBI = "I" OR "S" OR "O")
              GO TO PBI-ISO.
           IF PBI = "O"
              IF PBD = "O"
                 GO TO PBI-ISO.

       NB-ISO.
           MOVE LEGEND-NB TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A010NB." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO PBI-ISO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO NB-ISO.
           MOVE VDUBUF TO NB-BORROWER.
           IF NOT (NB-BORROWER = "I" OR "S" OR "O")
              GO TO NB-ISO.

       FB-ISO.
           MOVE LEGEND-FB TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A010FB." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO NB-ISO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO FB-ISO.
           MOVE VDUBUF TO FB-BORROWER.
           IF NOT (FB-BORROWER = "I" OR "S" OR "O")
              GO TO FB-ISO.

       INCLDETAIL.
           MOVE LEGEND-INCLDETAIL TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A010INCLDETAIL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U"
              IF PBD = "B"
                 GO TO PBD-ISO
              ELSE
                 GO TO FB-ISO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO INCLDETAIL.
           MOVE VDUBUF TO INCL-DETAIL.
           IF NOT (INCL-DETAIL = "Y" OR "N")
              GO TO INCLDETAIL.

       PRINTCOL.
           IF ( INCL-DETAIL = "N" )
              MOVE "S  A010PRCOL." TO VDU
              MOVE "N" TO PRCOL-FG VDUBUF
              PERFORM SCREENIO
              GO TO FINANCED-AMT-01.

           MOVE LEGEND-PRINTCOL-01 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A010PRCOL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO INCLDETAIL.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO PRINTCOL.

           IF VDUBUF NOT = "Y" AND
              VDUBUF NOT = "N"
                 MOVE "ENTER 'Y' OR 'N' ONLY " TO MESS
                 PERFORM SEND-MESS
                 GO TO PRINTCOL
           ELSE
                 MOVE VDUBUF TO PRCOL-FG.
      
       VOID-LOANS-ISO.
           MOVE LEGEND-VOID TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A010VLOANS." TO VDU.
           PERFORM SCREENIO.

           IF F1-KEY GO TO EXIT-PARM.
           IF UP-KEY
              IF ( INCL-DETAIL = "N" )
                 GO TO INCLDETAIL
              ELSE
                 GO TO PRINTCOL.
           IF F7-KEY GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO VOID-LOANS-ISO.

           MOVE VDUBUF TO INCL-VOIDS.
           IF NOT (INCL-VOIDS = "I" OR "S" OR "O")
              GO TO VOID-LOANS-ISO.

      
       FINANCED-AMT-01.
           MOVE LEGEND-FINANCED-01 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN070AMTFIN1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO EXIT-PARM.
           IF VDUSTATUS = "16" GO ALL-FINANCED-AMTS.
           IF VDUSTATUS = "1U"
              GO TO VOID-LOANS-ISO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO FINANCED-AMT-01.

           MOVE VDUNUM0 TO BEG-FINANCED-AMT.
      
       FINANCED-AMT-02.
           MOVE LEGEND-FINANCED-02 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN070AMTFIN2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO EXIT-PARM.
           IF VDUSTATUS = "1U"
              GO TO FINANCED-AMT-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO FINANCED-AMT-02.
           MOVE VDUNUM0 TO END-FINANCED-AMT.
           GO TO PROCEEDS-AMT-01.
      
       ALL-FINANCED-AMTS.
           MOVE "SNNN070AMTFIN1." TO VDU.
           MOVE 0 TO BEG-FINANCED-AMT VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN070AMTFIN2." TO VDU.
           MOVE 9999999 TO END-FINANCED-AMT VDUNUM0.
           PERFORM SCREENIO.

       PROCEEDS-AMT-01.
           MOVE "ENNN070PROCEEDS1." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO EXIT-PARM.
           IF F3-KEY GO TO ALL-PROCEEDS.
           IF UP-KEY GO TO FINANCED-AMT-02.
           IF F7-KEY GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO PROCEEDS-AMT-01.

           MOVE VDUNUM0 TO BEG-PROCEEDS-AMT.

       PROCEEDS-AMT-02.
           MOVE "ENNN070PROCEEDS2." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO EXIT-PARM.
           IF UP-KEY GO TO PROCEEDS-AMT-01.
           IF F7-KEY GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO PROCEEDS-AMT-02.
           MOVE VDUNUM0 TO END-PROCEEDS-AMT.

           IF BEG-PROCEEDS-AMT > END-PROCEEDS-AMT
              MOVE "INVALID PROCEEDS RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO PROCEEDS-AMT-01.

           GO TO LOCAL-PRINT.
      
       ALL-PROCEEDS.
           MOVE "SNNN070PROCEEDS1." TO VDU.
           MOVE ZERO TO BEG-PROCEEDS-AMT VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN070PROCEEDS2." TO VDU.
           MOVE 9999999 TO END-PROCEEDS-AMT VDUNUM0.
           PERFORM SCREENIO.

       LOCAL-PRINT.
           IF EXT-ROUTE-BUF NOT = "40"
              IF EXT-ROUTE-BUF NOT = "50"
                 MOVE "N" TO LOCAL-PRINT-FG
                 GO TO END-PARM.

           IF BEG-BRANCH = END-BRANCH
              MOVE "N" TO LOCAL-PRINT-FG
              GO TO END-PARM.

       ASK-LOCAL.
           MOVE LEGEND-LOCAL-SPOOL TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "S  A170SPOOL-PROMPT." TO VDU.
           MOVE "SPOOL TO LOCAL  :"     TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A050YES-NO." TO VDU.
           MOVE "(Y/N)"           TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "E  A010LOCAL-SPOOL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO FINANCED-AMT-02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO ASK-LOCAL.

           IF VDUBUF NOT = "Y"
              IF VDUBUF NOT = "N"
                 GO TO ASK-LOCAL.

           MOVE VDUBUF TO LOCAL-PRINT-FG.

       END-PARM.
           MOVE LEGEND-ACCEPT TO LEGEND.
           PERFORM SEND-LEGEND.
       EXIT-PARM.
           EXIT.

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
      
           MOVE "SNNN060ACCT01." TO VDU.
           MOVE BEG-ACCTNO TO VDUNUM0.
           PERFORM SCREENIO.
      
           MOVE "SNNN060ACCT02." TO VDU.
           MOVE END-ACCTNO TO VDUNUM0.
           PERFORM SCREENIO.
      
           MOVE "SNNN020CL1." TO VDU.
           MOVE BEG-CLASS TO VDUNUM0.
           PERFORM SCREENIO.
      
           MOVE "SNNN020CL2." TO VDU.
           MOVE END-CLASS TO VDUNUM0.
           PERFORM SCREENIO.
      
           MOVE "S  A010CONFID." TO VDU.
           MOVE CONFIDENTIAL TO VDUBUF.
           PERFORM SCREENIO.
      
           MOVE "S  A010PBD." TO VDU.
           MOVE PBD TO VDUBUF.
           PERFORM SCREENIO.
      
           MOVE "S  A010PBI." TO VDU.
           MOVE PBI TO VDUBUF.
           PERFORM SCREENIO.
      
           MOVE "S  A010NB." TO VDU.
           MOVE NB-BORROWER TO VDUBUF.
           PERFORM SCREENIO.
      
           MOVE "S  A010FB." TO VDU.
           MOVE FB-BORROWER TO VDUBUF.
           PERFORM SCREENIO.
      
           MOVE "S  A010INCLDETAIL." TO VDU.
           MOVE INCL-DETAIL TO VDUBUF.
           PERFORM SCREENIO.
      
           MOVE "S  A010PRCOL." TO VDU.
           MOVE PRCOL-FG TO VDUBUF.
           PERFORM SCREENIO.
      
           MOVE "SNNN070AMTFIN1." TO VDU.
           MOVE BEG-FINANCED-AMT TO VDUNUM0.
           PERFORM SCREENIO.
      
           MOVE "SNNN070AMTFIN2." TO VDU.
           MOVE END-FINANCED-AMT TO VDUNUM0.
           PERFORM SCREENIO.
      
           MOVE "SNNN070PROCEEDS1." TO VDU.
           MOVE BEG-PROCEEDS-AMT TO VDUNUM0.
           PERFORM SCREENIO.
      
           MOVE "SNNN070PROCEEDS2." TO VDU.
           MOVE END-PROCEEDS-AMT TO VDUNUM0.
           PERFORM SCREENIO.
      
           MOVE "S  A170SPOOL-PROMPT." TO VDU.
           MOVE "SPOOL TO LOCAL  :"    TO VDUBUF.
           PERFORM SCREENIO.
      
           MOVE "S  A050YES-NO." TO VDU.
           MOVE "(Y/N)"          TO VDUBUF.
           PERFORM SCREENIO.
      
           MOVE "S  A010LOCAL-SPOOL." TO VDU.
           MOVE LOCAL-PRINT-FG TO VDUBUF.
           PERFORM SCREENIO.
      

      **********************
       LN-DETAIL1 SECTION.
      **********************
           IF FIRST-TIME = "Y"
              MOVE EXT-CONAME-RPT-LINES TO LN-COUNT
              MOVE 1 TO LN-FEED
              MOVE "N" TO FIRST-TIME
              MOVE "Y" TO RECORDS-FOUND.

           MOVE LN-CLASS  TO D-CLASS.
           MOVE "-"       TO D-H1.
           MOVE LN-ACCTNO TO D-NUMBER.
           MOVE "-"       TO D-H2.
           MOVE LN-LAWCODE  TO D-SPCLASS.
           MOVE LN-LOANDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-LNDTE.
           MOVE LN-APRATE   TO D-APR.

           MOVE LN-SRCD TO D-SRCD.

           PERFORM SET-ACTIVITY-SUB.
           IF LN-SRCD = "PB" OR "RN" OR "SC"
                            OR "RB" OR "RO"
              IF ACT-SUB = 5
                 MOVE "PBD" TO D-SRCD
              ELSE
              IF ACT-SUB = 6
                 MOVE "PBI" TO D-SRCD.

           MOVE LN-COLLCD TO D-COLCD.
           MOVE LN-PURCD TO D-PURCD.
           MOVE LN-ORGTERM TO D-TERM.
           MOVE LN-LNAMT TO D-LNAMT.
           MOVE LN-FINCHG TO D-FINCHG.

           MOVE 1 TO I-SUB.
           PERFORM SELECT-INSURANCE.
           IF I-SUB < 6
              MOVE LN-INSPREM(I-SUB) TO D-INSPREM
              MOVE LN-INSOURS(I-SUB) TO D-INSOURS.

           MOVE LN-PRLNNO TO D-PRLNNO.
      * IF VOIDS, PRINT THE VOIDED DATE INSTEAD OF PRLNNO
           IF LN-POCD = "**"
              MOVE LN-POFFDATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO D-POFFDATE.
           MOVE LN-PRBAL TO D-PRBAL.
           MOVE LN-PRFINCHGRB TO D-INTRBS.

           MOVE LN-FEECODE(1) TO CDB-CODE.
           PERFORM GET-CODE.
           MOVE LN-FEEAMT(1) TO D-FEEAMT.
           MOVE LN-PROCEEDS TO D-PROCEEDS.
           IF INCL-DETAIL = "Y"
              MOVE 2 TO LN-FEED
              PERFORM WRITE-DETAIL-LINE.

      *    -------------------------------------------------------------
      *    LINE 2
      *    -------------------------------------------------------------
           MOVE LN-MAKERCD(1) TO D-MAKERCD.
           MOVE LN-SSNO(1) TO NUM-BW.
           PERFORM MOVE-BORROWER.
           MOVE LN-INTCHG TO D-LNAMT.
      * LN-MAINTFEE - USE EARNINGS BECAUSE OF "MX" VAT SIZE
      *               BRANCH TRANSFER DONT INCLUDE ANTICADJ
           IF LN-POCD = "BT"
              MOVE LN-ANTICERN(4) TO D-FINCHG
           ELSE
              COMPUTE D-FINCHG = LN-ANTICERN(4) + LN-ANTICADJ(4).
           ADD 1 TO I-SUB.
           IF I-SUB < 6
              PERFORM SELECT-INSURANCE
              IF I-SUB < 6
                 MOVE LN-INSPREM(I-SUB) TO D-INSPREM
                 MOVE LN-INSOURS(I-SUB) TO D-INSOURS.

           MOVE LP-POSTTIME TO HH-MM-SS.
           IF H-AND-M NOT GREATER THAN 1159
              MOVE "AM" TO D-AMPM
           ELSE
              MOVE "PM" TO D-AMPM.
           IF H-AND-M IS NOT LESS THAN 1300
              SUBTRACT 1200 FROM H-AND-M.
           IF CUR-HH = 0
              MOVE 12 TO CUR-HH.
           MOVE CUR-HH TO D-HH.
           MOVE CUR-MM TO D-MM.

           MOVE LN-PRINSRB TO D-PRBAL.
           MOVE LN-PRINTDUE TO D-INTRBS.
           MOVE LN-FEECODE(2) TO CDB-CODE.
           PERFORM GET-CODE.
           MOVE LN-FEEAMT(2) TO D-FEEAMT.
           MOVE LN-PROCASH TO D-PROCEEDS.

           IF INCL-DETAIL = "Y"
              PERFORM WRITE-DETAIL-LINE.

      *    -------------------------------------------------------------
      *    LINE 3
      *    -------------------------------------------------------------
           MOVE BW-CITY TO C-S-Z.
           MOVE BW-STATE TO STATE.
           MOVE BW-ZIP TO ZIP.
           PERFORM CAT-CSZ.
           IF BW-ADR2 NOT = SPACES
              STRING BW-ADR1,", ",BW-ADR2 DELIMITED BY "   "
                 INTO D-BW-ADDR
           ELSE
              MOVE BW-ADR1 TO D-BW-ADDR.
           STRING D-BW-ADDR,"   ",C-S-Z DELIMITED BY "     "
               INTO D-BW-ADDR.

           MOVE LN-SERCHG TO D-LNAMT.
      *    -------------------------------------------------------------
      *    STATE OF GA, PRINT 8% SERCHG
      *    -------------------------------------------------------------
           IF BR-STATE = "GA"
              MOVE LN-STATE-WORKER TO D-FINCHG
              MOVE 0 TO D-FINCHG
           ELSE
              COMPUTE FINCHG = LN-ACCERN(1) + LN-ACCERN(2) +
                               LN-ACCERN(3) + LN-ACCERN(5)

              IF NOT( GP-INCL-OTH-IN-EARNED-N OR
                       GP-INCL-OTH-IN-EARNED-R)
                 ADD LN-ACCERN(6) TO FINCHG
              END-IF
              IF NOT (GP-INCL-OTH-IN-EARNED-R )
                 ADD LN-ACCERN(4) TO FINCHG
              END-IF
              MOVE FINCHG TO D-FINCHG.

           ADD 1 TO I-SUB.
           IF I-SUB < 6
              PERFORM SELECT-INSURANCE
              IF I-SUB < 6
                 MOVE LN-INSPREM(I-SUB) TO D-INSPREM
                 MOVE LN-INSOURS(I-SUB) TO D-INSOURS.

           MOVE LP-USERID TO D-USERID.

           MOVE LN-FEECODE(3) TO CDB-CODE.
           PERFORM GET-CODE.
           MOVE LN-FEEAMT(3) TO D-FEEAMT.
           IF LN-PROCASH NOT = 0
              MOVE LN-TILLNO TO D-TILLNO.

           IF DETAIL-LINE NOT = SPACES
              IF INCL-DETAIL = "Y"
                 PERFORM WRITE-DETAIL-LINE.

      *    -------------------------------------------------------------
      *    LINE 4
      *    -------------------------------------------------------------
           MOVE LN-MAKERCD(2) TO D-MAKERCD.
           MOVE LN-SSNO(2) TO NUM-BW.
           PERFORM MOVE-BORROWER.
           ADD 1 TO I-SUB.
           IF I-SUB < 6
              PERFORM SELECT-INSURANCE
              IF I-SUB < 6
                 MOVE LN-INSPREM(I-SUB) TO D-INSPREM
                 MOVE LN-INSOURS(I-SUB) TO D-INSOURS.
           MOVE LN-FEECODE(4) TO CDB-CODE.
           PERFORM GET-CODE.
           MOVE LN-FEEAMT(4) TO D-FEEAMT.
           IF DETAIL-LINE NOT = SPACES
              IF INCL-DETAIL = "Y"
                 PERFORM WRITE-DETAIL-LINE.

      *    -------------------------------------------------------------
      *    LINE 5
      *    -------------------------------------------------------------
           MOVE LN-MAKERCD(3) TO D-MAKERCD.
           MOVE LN-SSNO(3) TO NUM-BW.
           PERFORM MOVE-BORROWER.
           ADD 1 TO I-SUB.
           IF I-SUB < 6
              PERFORM SELECT-INSURANCE
              IF I-SUB < 6
                 MOVE LN-INSPREM(I-SUB) TO D-INSPREM
                 MOVE LN-INSOURS(I-SUB) TO D-INSOURS.
           MOVE LN-FEECODE(5) TO CDB-CODE.
           PERFORM GET-CODE.
           MOVE LN-FEEAMT(5) TO D-FEEAMT.
           IF DETAIL-LINE NOT = SPACES
              IF INCL-DETAIL = "Y"
                 PERFORM WRITE-DETAIL-LINE.

      *    -------------------------------------------------------------
      *    LINE 6
      *    -------------------------------------------------------------
           MOVE LN-MAKERCD(4) TO D-MAKERCD.
           MOVE LN-SSNO(4) TO NUM-BW.
           PERFORM MOVE-BORROWER.
           MOVE LN-FEECODE(6) TO CDB-CODE.
           PERFORM GET-CODE.
           MOVE LN-FEEAMT(6) TO D-FEEAMT.
           IF DETAIL-LINE NOT = SPACES
              IF INCL-DETAIL = "Y"
                 PERFORM WRITE-DETAIL-LINE.

      *    -------------------------------------------------------------
      *    LINE 7
      *    -------------------------------------------------------------
           MOVE LN-FEECODE(7) TO CDB-CODE.
           PERFORM GET-CODE.
           MOVE LN-FEEAMT(7) TO D-FEEAMT.
           IF DETAIL-LINE NOT = SPACES
              IF INCL-DETAIL = "Y"
                 PERFORM WRITE-DETAIL-LINE.

      *    -------------------------------------------------------------
      *    LINE 8
      *    -------------------------------------------------------------
           MOVE LN-FEECODE(8) TO CDB-CODE.
           PERFORM GET-CODE.
           MOVE LN-FEEAMT(8) TO D-FEEAMT.
           IF DETAIL-LINE NOT = SPACES
              IF INCL-DETAIL = "Y"
                 PERFORM WRITE-DETAIL-LINE.

      *    -------------------------------------------------------------
      *    LINE 9
      *    -------------------------------------------------------------
           MOVE LN-FEECODE(9) TO CDB-CODE.
           PERFORM GET-CODE.
           MOVE LN-FEEAMT(9) TO D-FEEAMT.
           IF DETAIL-LINE NOT = SPACES
              IF INCL-DETAIL = "Y"
                 PERFORM WRITE-DETAIL-LINE.

      *    -------------------------------------------------------------
      *    LINE 10
      *    -------------------------------------------------------------
           MOVE LN-FEECODE(10) TO CDB-CODE.
           PERFORM GET-CODE.
           MOVE LN-FEEAMT(10) TO D-FEEAMT.
           IF DETAIL-LINE NOT = SPACES
              IF INCL-DETAIL = "Y"
                 PERFORM WRITE-DETAIL-LINE.

           IF INCL-DETAIL = "Y"
              PERFORM PRINT-COLLATERAL.

       EXIT-LN-DETAIL.
           EXIT.

      ******************************************************************
       CREATE-WK SECTION.
      ******************************************************************

           MOVE LN-SRCD  TO SAVE-WK-CODE.
           MOVE LN-OWNBR TO SAVE-WK-BR.
           MOVE LN-CLASS TO SAVE-WK-CL.

       SAVE-WORK-KEY.
           MOVE SAVE-WK-KEY TO WK-KEY.
           PERFORM READ-WK-FILE.
           IF IO-GOOD
              GO TO CREATE-WK-UPDATE.

           MOVE SPACES      TO WK-REC.
           MOVE SAVE-WK-KEY TO WK-KEY.
           MOVE 0 TO WK-NOLOANS WK-LNAMT WK-EARNINGS WK-INTCHG
                     WK-SERCHG WK-EXTCHG WK-MFEE WK-STATE
                     WK-DL WK-LL WK-AH WK-PP WK-O1 WK-O2 WK-O3
                     WK-FEES WK-PROCEEDS WK-PROCASH.
           PERFORM WRITE-WK-FILE.
           IF NOT IO-GOOD
              GO TO IO-ERROR.

           GO TO SAVE-WORK-KEY.
       CREATE-WK-UPDATE.
           IF LN-LNAMT < 0
              SUBTRACT 1 FROM WK-NOLOANS
           ELSE
              ADD 1 TO WK-NOLOANS.
           ADD LN-LNAMT TO WK-LNAMT.
           ADD LN-INTCHG TO WK-INTCHG.
           ADD LN-SERCHG TO WK-SERCHG.
           ADD LN-EXTCHG TO WK-EXTCHG.
      * LN-MAINTFEE - USE ANTICERN(4) BECAUSE OF "MX" VAT SIZE
      *               BRANCH TRANSFER DONT INCLUDE ANTICADJ
           IF LN-POCD = "BT"
              COMPUTE WK-MFEE ROUNDED = WK-MFEE + LN-ANTICERN(4)
           ELSE
              COMPUTE WK-MFEE ROUNDED = WK-MFEE +
                                   LN-ANTICERN(4) + LN-ANTICADJ(4).
           IF BR-STATE = "GA"
              ADD LN-STATE-WORKER TO WK-STATE.
      *CS  COMPUTE WK-EARNINGS = WK-EARNINGS +
      *CS                        LN-ACCERN(1) + LN-ACCERN(2) +
      *CS                        LN-ACCERN(3) + LN-ACCERN(4) +
      *CS                        LN-ACCERN(5).

           COMPUTE WK-EARNINGS = WK-EARNINGS +
                                 LN-ACCERN(1) + LN-ACCERN(2) +
                                 LN-ACCERN(3) + LN-ACCERN(5).

           IF NOT (GP-INCL-OTH-IN-EARNED-N OR
                   GP-INCL-OTH-IN-EARNED-R)
              ADD LN-ACCERN(6) TO WK-EARNINGS.

           IF NOT (GP-INCL-OTH-IN-EARNED-R )
              ADD LN-ACCERN(4) TO WK-EARNINGS.

           IF LN-INSTYPES-DL
              IF LN-INSOURS(1) = "Y"
                 ADD LN-INSPREM(1) TO WK-DL.
           IF NOT LN-INSTYPES-DL
              IF LN-INSOURS(1) = "Y"
                 ADD LN-INSPREM(1) TO WK-LL.
           IF LN-INSOURS(2) = "Y"
              ADD LN-INSPREM(2) TO WK-AH.
           IF LN-INSOURS(3) = "Y"
              ADD LN-INSPREM(3) TO WK-PP.
           IF LN-INSOURS(4) = "Y"
              ADD LN-INSPREM(4) TO WK-O1.
           IF LN-INSOURS(5) = "Y"
              ADD LN-INSPREM(5) TO WK-O2.
           IF LN-INSOURS(6) = "Y"
              ADD LN-INSPREM(6) TO WK-O3.

           ADD LN-FEEAMT(1) LN-FEEAMT(2) LN-FEEAMT(3)
               LN-FEEAMT(4) LN-FEEAMT(5) LN-FEEAMT(6)
               LN-FEEAMT(7) LN-FEEAMT(8) LN-FEEAMT(9)
               LN-FEEAMT(10) TO WK-FEES.

           ADD LN-PROCEEDS TO WK-PROCEEDS.
           ADD LN-PROCASH TO WK-PROCASH.
           PERFORM REWRITE-WK-FILE.
       EXIT-CREATE-WK.
           EXIT.

      ******************************************************************
       MOVE-BORROWER SECTION.
      ******************************************************************
           IF NUM-BW = 0
              GO TO EXIT-MOVE-BORROWER.

           MOVE NUM-BW TO BW-SSNO D-BW-SSNO.
           INSPECT D-BW-SSNO REPLACING ALL "/" BY "-".

           PERFORM READ-BW1-FILE.
           IF IO-BAD
              MOVE "* * NOT FOUND * *" TO BW-LNAME
              MOVE " " TO BW-FNAME.

           MOVE SPACES TO D-BW-NAME.
           STRING BW-LNAME," ",BW-FNAME DELIMITED BY "  "
                 INTO D-BW-NAME.

       EXIT-MOVE-BORROWER.
           EXIT.

      ******************************************************************
       WRITE-DETAIL-LINE SECTION.
      ******************************************************************
           ADD LN-FEED LN-COUNT GIVING LN-WK.
           IF LN-WK GREATER THAN EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.
           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      ******************************************************************
       SET-BRANCH-HEADINGS SECTION.
      ******************************************************************
           MOVE HOLD-BRNO TO H-BRNO.
           MOVE HOLD-BRNAME TO H-BRDESC.

      ******************************************************************
       SET-CLASS-HEADINGS SECTION.
      ******************************************************************
           MOVE HOLD-LN-CLASS TO H-LCNO LC-CODE.
           PERFORM READ-LC1-FILE.
           IF NOT IO-GOOD
              MOVE "INVALID" TO LC-DESC.
           MOVE LC-DESC TO H-LCDESC.
           MOVE "N" TO SUMMARY-FG.

      ******************************************************************
       CLASS-TOTALS SECTION.
      ******************************************************************
           MOVE "Y" TO SUMMARY-FG.
           IF INCL-DETAIL = "Y"
              PERFORM WRITE-DETAIL-LINE
           ELSE
              MOVE SPACES TO DETAIL-LINE
              IF LN-COUNT = EXT-CONAME-RPT-LINES
                 PERFORM HEAD-PAGE.
           MOVE HEAD7 TO PR-REC.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.
           MOVE HEAD8 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE HOLD-BRNO     TO WK-BR.
           MOVE HOLD-LN-CLASS TO WK-CLASS.
           MOVE " " TO WK-CODE.
           PERFORM START-WK-FILE.
       PRINT-NEXT-CODE.
           PERFORM READ-WK-FILE-NEXT.
           IF IO-BAD
              PERFORM TYPE-TOTALS
              GO TO END-CLASS-TOTALS.

           IF WK-CLASS NOT = HOLD-LN-CLASS
              PERFORM TYPE-TOTALS
              GO TO END-CLASS-TOTALS.
           IF WK-BR NOT = HOLD-BRNO
              PERFORM TYPE-TOTALS
              GO TO END-CLASS-TOTALS.

           MOVE WK-CODE TO S-DCODE.
           MOVE ":"     TO S-COLON.

           MOVE WK-INTCHG TO S-INTCHG.
      * 8% SERVICE
           MOVE WK-STATE  TO S-SERCHG.
           MOVE WK-EXTCHG TO S-EXTCHG.
           MOVE WK-DL     TO S-CL2.
           MOVE WK-AH     TO S-AH.
           MOVE WK-O1     TO S-OT.
           MOVE WK-O3     TO S-FEEAMT.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE WK-NOLOANS  TO S-NOLOANS.
           MOVE WK-LNAMT    TO S-LNAMT.
           MOVE WK-MFEE     TO S-INTCHG.
           MOVE WK-SERCHG   TO S-SERCHG.
           MOVE WK-LL       TO S-CL2.
           MOVE WK-PP       TO S-AH.
           MOVE WK-O2       TO S-OT.
           MOVE WK-FEES     TO S-FEEAMT.
           MOVE WK-EARNINGS TO S-EARNINGS.
           MOVE WK-PROCASH  TO S-PROCASH.
           MOVE WK-PROCEEDS TO S-PROCEEDS.
           PERFORM WRITE-DETAIL-LINE.

           GO TO PRINT-NEXT-CODE.
       END-CLASS-TOTALS.
           MOVE 2 TO LEV.
           MOVE 1 TO SUB.
           ADD T-NOLOANS(1) TO T-NOLOANS(2).
           PERFORM UPDATE-TABLE2.
           MOVE 1 TO SUB.
           PERFORM CLEAR-TOTAL-TBL.
           MOVE "N" TO SUMMARY-FG.

      ******************************************************************
       BRANCH-TOTALS SECTION.
      ******************************************************************
           MOVE "Y" TO SUMMARY-FG.
           MOVE 0 TO HOLD-LN-CLASS.
           MOVE 2 TO LEV.
           MOVE "BRANCH TOTALS:" TO DETAIL-LINE.
           PERFORM PRINT-SUMMARY.
       END-BRANCH-TOTALS.
           MOVE 3 TO LEV.
           MOVE 2 TO SUB.
           ADD T-NOLOANS(2) TO T-NOLOANS(3).
           PERFORM UPDATE-TABLE2.
           MOVE 2 TO SUB.
           PERFORM CLEAR-TOTAL-TBL.
           MOVE "N" TO SUMMARY-FG.

      ******************************************************************
       GRAND-TOTALS SECTION.
      ******************************************************************
           MOVE "Y" TO SUMMARY-FG.
           MOVE 99 TO LN-FEED.
      
           MOVE "GRAND  TOTALS:"         TO G-TOTNAME.
           MOVE "NUMBER OF LOANS      :" TO G-DESC.
           MOVE T-NOLOANS(3) TO G-NOLOANS.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "LOAN AMOUNT          :" TO G-DESC.
           MOVE T-LNAMT(3) TO G-AMT.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "INTEREST CHARGES     :" TO G-DESC.
           MOVE T-INTCHG(3) TO G-AMT.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "SERVICE CHARGES      :" TO G-DESC.
           MOVE T-SERCHG(3) TO G-AMT.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "8% SERVICE CHARGE    :" TO G-DESC.
           MOVE T-STATE(3) TO G-AMT.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "EXTENSION            :" TO G-DESC.
           MOVE T-EXTCHG(3) TO G-AMT.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "MAINTENANCE FEES     :" TO G-DESC.
           MOVE T-MFEE(3) TO G-AMT.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "CREDIT LIFE DECLINING:" TO G-DESC.
           MOVE T-DL(3) TO G-AMT.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "CREDIT LIFE LEVEL    :" TO G-DESC.
           MOVE T-LL(3) TO G-AMT.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "ACCIDENT & HEALTH    :" TO G-DESC.
           MOVE T-AH(3) TO G-AMT.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "PERSONAL PROPERTY    :" TO G-DESC.
           MOVE T-PP(3) TO G-AMT.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "OTHER INSURANCE 1    :" TO G-DESC.
           MOVE T-O1(3) TO G-AMT.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "OTHER INSURANCE 2    :" TO G-DESC.
           MOVE T-O2(3) TO G-AMT.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "OTHER INSURANCE 3    :" TO G-DESC.
           MOVE T-O3(3) TO G-AMT.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "FEES                 :" TO G-DESC.
           MOVE T-FEES(3) TO G-AMT.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "EARNINGS             :" TO G-DESC.
           MOVE T-EARNINGS(3) TO G-AMT.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "CASH PROCEEDS        :" TO G-DESC.
           MOVE T-PROCASH(3) TO G-AMT.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "PROCEEDS             :" TO G-DESC.
           MOVE T-PROCEEDS(3) TO G-AMT.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE 5 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

       END-GRAND-TOTALS.
           MOVE 3 TO SUB.
           PERFORM CLEAR-TOTAL-TBL.
           MOVE "Y" TO SUMMARY-FG.

      ******************************************************************
       HEAD-PAGE SECTION.
      ******************************************************************
           ADD 1 TO PAGE-NUMBER.
           MOVE PAGE-NUMBER TO H-PAGE-NO.
           PERFORM GET-TIME.
           MOVE T-HH TO H-HR.
           MOVE T-MM TO H-MIN.
           MOVE HEAD1 TO PR-REC.
           MOVE 99 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE HEAD2 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE HEAD3 TO PR-REC.
           PERFORM WRITE-PR-REC.

           MOVE HEAD4 TO PR-REC.
           PERFORM WRITE-PR-REC.

           IF SUMMARY-FG = "N"
              IF INCL-DETAIL = "N"
                 MOVE 4 TO LN-COUNT
                 MOVE 2 TO LN-FEED
              ELSE
                 MOVE HEAD5 TO PR-REC
                 MOVE 2 TO LN-FEED
                 PERFORM WRITE-PR-REC
                 MOVE HEAD6 TO PR-REC
                 MOVE 1 TO LN-FEED
                 PERFORM WRITE-PR-REC
                 MOVE HEAD6A TO PR-REC
                 IF BR-STATE = "GA"
                    MOVE HEAD6B TO PR-REC
                    MOVE 1 TO LN-FEED
                    PERFORM WRITE-PR-REC
                    MOVE 7 TO LN-COUNT
                    MOVE 2 TO LN-FEED
                 ELSE
                    MOVE HEAD6A TO PR-REC
                    MOVE 1 TO LN-FEED
                    PERFORM WRITE-PR-REC
                    MOVE 7 TO LN-COUNT
                    MOVE 2 TO LN-FEED
           ELSE
              MOVE 4 TO LN-COUNT
              MOVE 2 TO LN-FEED.
       END-HEAD-PAGE.
           EXIT.

      ******************************************************************
       PRINT-RANGE-LINES SECTION.
      ******************************************************************
           MOVE 4 TO LN-FEED.
      
           IF GROUP-FLAG = "Y"
              MOVE "BRANCH GROUP" TO RANGE-LINE
              MOVE ENT-GROUP TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE
           ELSE
              MOVE "BEG BRANCH" TO RANGE-LINE
              MOVE BEG-BRANCH TO R-NUM
              PERFORM WRITE-DETAIL-LINE
              MOVE "END BRANCH" TO RANGE-LINE
              MOVE END-BRANCH TO R-NUM
              PERFORM WRITE-DETAIL-LINE.
      
           MOVE "BEG CLASS" TO RANGE-LINE.
           MOVE BEG-CLASS TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "END CLASS" TO RANGE-LINE.
           MOVE END-CLASS TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "BEG ACCOUNT" TO RANGE-LINE.
           MOVE BEG-ACCTNO TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "END ACCOUNT" TO RANGE-LINE.
           MOVE END-ACCTNO TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "BEG LOAN DATE" TO RANGE-LINE.
           MOVE BEG-LOANDATE TO R-DATE.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "END LOAN DATE" TO RANGE-LINE.
           MOVE END-LOANDATE TO R-DATE.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "CONFIDENTIALS" TO RANGE-LINE.
           MOVE CONFIDENTIAL TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "PB DELINQUENT" TO RANGE-LINE.
           MOVE PBD TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "PB INCREASE" TO RANGE-LINE.
           MOVE PBI TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "NEW BORROWER" TO RANGE-LINE.
           MOVE NB-BORROWER TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "FORMER BORROWER" TO RANGE-LINE.
           MOVE FB-BORROWER TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "INCLUDE DETAIL" TO RANGE-LINE.
           MOVE INCL-DETAIL TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "PRINT COLLATERAL" TO RANGE-LINE.
           MOVE PRCOL-FG TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "BEG FINANCED AMT " TO RANGE-LINE.
           MOVE BEG-FINANCED-AMT TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "END FINANCED AMT" TO RANGE-LINE.
           MOVE END-FINANCED-AMT TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "BEG PROCEEDS AMT " TO RANGE-LINE.
           MOVE BEG-PROCEEDS-AMT    TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "END PROCEEDS AMT " TO RANGE-LINE.
           MOVE END-PROCEEDS-AMT    TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.


      ******************************************************************
       PRINT-COLLATERAL SECTION.
      ******************************************************************
           IF PRCOL-FG = "Y"
              MOVE LN-OWNBR  TO LTC-BRNO
              MOVE LN-ACCTNO TO LTC-ACCTNO
              MOVE 1         TO LTC-SEQNO
              PERFORM READ-LTC1-FILE
              IF IO-FG = 0
                 MOVE "MAKE : " TO D-FIL-MAKE
                 MOVE "MODEL : " TO D-FIL-MODEL
                 MOVE "YEAR : " TO D-FIL-YEAR
                 MOVE "VIN # : " TO D-FIL-ID
                 MOVE LTC-MAKE(1) TO D-MAKE
                 MOVE LTC-MODEL(1) TO D-MODEL
                 MOVE LTC-YEAR(1) TO D-YEAR
                 MOVE LTC-VIN(1) TO D-ID
                 PERFORM WRITE-DETAIL-LINE
              ELSE
                 MOVE SPACES TO LTC-AUTO(2).

           IF PRCOL-FG = "Y"
              IF LTC-AUTO(2) NOT = SPACES
                 MOVE "MAKE : " TO D-FIL-MAKE
                 MOVE "MODEL : " TO D-FIL-MODEL
                 MOVE "YEAR : " TO D-FIL-YEAR
                 MOVE "VIN # : " TO D-FIL-ID
                 MOVE LTC-MAKE(2) TO D-MAKE
                 MOVE LTC-MODEL(2) TO D-MODEL
                 MOVE LTC-YEAR(2) TO D-YEAR
                 MOVE LTC-VIN(2) TO D-ID
                 PERFORM WRITE-DETAIL-LINE.

      ******************************************************************
       SELECT-INSURANCE SECTION.
      ******************************************************************
       SELECT-IT.

           IF I-SUB > 5
              GO TO EXIT-SELECT-INSURANCE.

           IF LN-INSPREM(I-SUB) = 0
              ADD 1 TO I-SUB
              GO TO SELECT-IT.

           IF I-SUB = 1
              PERFORM GET-CREDIT-LIFE-TAG 
           ELSE
              IF I-SUB = 2
                 MOVE "A" TO D-INSTYPE
              ELSE
                 IF I-SUB = 3
                    MOVE "P" TO D-INSTYPE
                 ELSE
                    IF I-SUB = 4
                       MOVE "1" TO D-INSTYPE
                    ELSE
                       IF I-SUB = 5
                          MOVE "2" TO D-INSTYPE.

       EXIT-SELECT-INSURANCE.
           EXIT.

      *************************************************
       SET-ACTIVITY-SUB SECTION.
      ******************************************************

      * 5 AND 6 ARE ALL THAT ARE USED IN THIS REPORT. JIM WANTS P&L ACCOUNTS
      * TO STILL BE ABLE TO BE PBD AND PBI FOR THE AUDITORS AT WORLD.
      *  SO I AM COMMENTING OUT THE P&L TEST.

      *    IF LN-PLCD = "P"
      *       MOVE 8 TO ACT-SUB
      *    ELSE
      *       IF LN-PLCD NOT = " "
      *          MOVE 9 TO ACT-SUB.

           MOVE "N" TO PBD-PBI.
           IF LN-SRCD = "NB"
              MOVE 1 TO ACT-SUB
           ELSE
              IF LN-SRCD = "RF"
                 MOVE 2 TO ACT-SUB
              ELSE
                 IF LN-SRCD = "FB"
                    MOVE 3 TO ACT-SUB
                 ELSE
                    IF NOT (LN-SRCD = "PB" OR "RN" OR "SC"
                                         OR "RB" OR "RO")
                       MOVE 7 TO ACT-SUB
                    ELSE
                       MOVE 4 TO ACT-SUB
                       IF LN-PRPDUE-DAYS > SP-PBDEL-DAYS
                          MOVE 5 TO ACT-SUB
                      ELSE
                          IF SP-PBINCR-PERCENT NOT = 0
                             IF SP-PBINCR-PERCENT = 1
                                IF LN-LNAMT > LN-PRLNAMT
                                   MOVE 6 TO ACT-SUB
                                END-IF
                             ELSE
                                COMPUTE SRCD-WORKER ROUNDED =
                                  LN-PRLNAMT - 0.01 + LN-PRLNAMT
                                      * SP-PBINCR-PERCENT / 100
                                IF LN-LNAMT > SRCD-WORKER
                                   MOVE 6 TO ACT-SUB.

       COPY "LIBWI/PFSWINDOWS.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBLP/ARRAYSP.CPY". 
       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/LPAMTS.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/EOSPOOL.CPY".
       COPY "LIBGB/BRSECURE.CPY".

      *********************************************************
       FORM-RESET SECTION.
      *********************************************************
           MOVE EXT-CONAME-CONAME TO AULNRG-CONAME.
           MOVE EXT-CONAME-SYSDATE TO AULNRG-SYSDATE.
           DISPLAY AULNRG-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE AULNRG-FORM-FIELDS TO WR-BUF.
           MOVE AULNRG-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       FORM-RESET-EXIT.
           EXIT.

      *********************************************************
       SETUP-LINK-INFO SECTION.
      *********************************************************
           MOVE AULNRG-HELP--FILE TO HELP-LINK-FILE.
           MOVE AULNRG-HELP--DIR  TO HELP-LINK-DIR.

      *********************************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/AULNRG_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISC-PARA SECTION.
      ******************************************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       CHECK-FCOUNT.
           INSPECT LN-SRCD  TALLYING FCOUNT FOR ALL "F".
       SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE MSEL-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       CLEAR-TOTAL-TBL.
           MOVE 0 TO T-NOLOANS(SUB) T-LNAMT(SUB) T-INTCHG(SUB)
                     T-SERCHG(SUB) T-EXTCHG(SUB) T-MFEE(SUB)
                     T-STATE(SUB) T-EARNINGS(SUB) T-DL(SUB) T-LL(SUB)
                     T-AH(SUB) T-PP(SUB) T-O1(SUB) T-O2(SUB) T-O3(SUB)
                     T-FEES(SUB) T-PROCEEDS(SUB) T-PROCASH(SUB).
       CAT-CSZ.
           INSPECT C-S-Z REPLACING FIRST "      " BY ", ++ +".
           INSPECT C-S-Z REPLACING FIRST "++" BY STATE.
           INSPECT C-S-Z REPLACING FIRST "+         " BY ZIP.
       GET-CODE.
           MOVE "GC"     TO CDB-TYPE.
           MOVE LN-OWNBR TO CDB-BRANCH.
      *--> SET BEFORE       CDB-CODE.
           PERFORM READ-CDB1-FILE.
           IF NOT IO-GOOD
              MOVE SPACES TO CDB-DESC.
           MOVE CDB-DESC TO D-FEEGLNO.
       READ-SP-RECORD.
           MOVE LN-ORGST    TO SP-ORGST.
           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           MOVE LN-LAWCODE  TO SP-LAWCODE.
           IF SP1-KEY NOT = LPWSSP-SP1-KEY
              PERFORM ARRAYSP-READ
      *        PERFORM READ-SP1-FILE
              IF IO-FG NOT = 0
                 PERFORM FILE-ERRORS
                 PERFORM SEND-MESS
                 MOVE SPACES TO LPWSSP-SP1-KEY
              ELSE
                 MOVE SP1-KEY TO LPWSSP-SP1-KEY.
       UPDATE-TABLE.
           IF LN-LNAMT < 0
              SUBTRACT 1 FROM T-NOLOANS(1)
           ELSE
              ADD 1 TO T-NOLOANS(1).
           ADD LN-LNAMT  TO T-LNAMT(LEV).
           ADD LN-INTCHG TO T-INTCHG(LEV).
           ADD LN-SERCHG TO T-SERCHG(LEV).
           ADD LN-EXTCHG TO T-EXTCHG(LEV).
      * LN-MAINTFEE - USE ANTICERN(4) BECAUSE OF "MX" VAT SIZE
      *               BRANCH TRANSFER DONT INCLUDE ANTICADJ
           IF LN-POCD = "BT"
              COMPUTE T-MFEE(LEV) ROUNDED = T-MFEE(LEV) +
                                            LN-ANTICERN(4)
           ELSE
              COMPUTE T-MFEE(LEV) ROUNDED = T-MFEE(LEV) +
                                   LN-ANTICERN(4) + LN-ANTICADJ(4).
           IF BR-STATE = "GA"
              ADD LN-STATE-WORKER TO T-STATE(LEV).
           COMPUTE T-EARNINGS(LEV) = T-EARNINGS(LEV) +
                                     LN-ACCERN(1) + LN-ACCERN(2) +
                                     LN-ACCERN(3) + LN-ACCERN(5).

           IF NOT (GP-INCL-OTH-IN-EARNED-N OR GP-INCL-OTH-IN-EARNED-R)
              ADD LN-ACCERN(6) TO T-EARNINGS(LEV).

           IF NOT (GP-INCL-OTH-IN-EARNED-R)
              ADD LN-ACCERN(4) TO T-EARNINGS(LEV).

           IF LN-INSTYPES-DL
              IF LN-INSOURS(1) = "Y"
                 ADD LN-INSPREM(1) TO T-DL(LEV).
           IF NOT LN-INSTYPES-DL
              IF LN-INSOURS(1) = "Y"
                 ADD LN-INSPREM(1) TO T-LL(LEV).
           IF LN-INSOURS(2) = "Y"
              ADD LN-INSPREM(2) TO T-AH(LEV).
           IF LN-INSOURS(3) = "Y"
              ADD LN-INSPREM(3) TO T-PP(LEV).
           IF LN-INSOURS(4) = "Y"
              ADD LN-INSPREM(4) TO T-O1(LEV).
           IF LN-INSOURS(5) = "Y"
              ADD LN-INSPREM(5) TO T-O2(LEV).
           IF LN-INSOURS(6) = "Y"
              ADD LN-INSPREM(6) TO T-O3(LEV).

           ADD LN-FEEAMT(1) LN-FEEAMT(2) LN-FEEAMT(3)
               LN-FEEAMT(4) LN-FEEAMT(5) LN-FEEAMT(6)
               LN-FEEAMT(7) LN-FEEAMT(8) LN-FEEAMT(9)
               LN-FEEAMT(10) TO T-FEES(LEV).
           ADD LN-PROCEEDS TO T-PROCEEDS(LEV).
           ADD LN-PROCASH TO T-PROCASH(LEV).
       UPDATE-TABLE2.
           ADD T-LNAMT(SUB) TO T-LNAMT(LEV).
           ADD T-INTCHG(SUB) TO T-INTCHG(LEV).
           ADD T-SERCHG(SUB) TO T-SERCHG(LEV).
           ADD T-EXTCHG(SUB) TO T-EXTCHG(LEV).
           ADD T-MFEE(SUB) TO T-MFEE(LEV).
           ADD T-STATE(SUB) TO T-STATE(LEV).
           ADD T-EARNINGS(SUB) TO T-EARNINGS(LEV).
           ADD T-LL(SUB) TO T-LL(LEV).
           ADD T-DL(SUB) TO T-DL(LEV).
           ADD T-AH(SUB) TO T-AH(LEV).
           ADD T-PP(SUB) TO T-PP(LEV).
           ADD T-O1(SUB) TO T-O1(LEV).
           ADD T-O2(SUB) TO T-O2(LEV).
           ADD T-O3(SUB) TO T-O3(LEV).
           ADD T-FEES(SUB) TO T-FEES(LEV).
           ADD T-PROCEEDS(SUB) TO T-PROCEEDS(LEV).
           ADD T-PROCASH(SUB) TO T-PROCASH(LEV).
       TYPE-TOTALS.
           MOVE 1 TO LEV.
           MOVE "CLASS  TOTALS:" TO DETAIL-LINE.
           PERFORM PRINT-SUMMARY.
       PRINT-SUMMARY.
           MOVE T-INTCHG(LEV) TO S-INTCHG.
      * GA 8% FEE
           MOVE T-STATE(LEV) TO S-SERCHG.
           MOVE T-EXTCHG(LEV) TO S-EXTCHG.
           MOVE T-DL(LEV) TO S-CL2.
           MOVE T-AH(LEV) TO S-AH.
           MOVE T-O1(LEV) TO S-OT.
           MOVE T-O3(LEV) TO S-FEEAMT.
           MOVE 3 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE T-NOLOANS(LEV) TO S-NOLOANS.
           MOVE T-LNAMT(LEV)   TO S-LNAMT.
           MOVE T-MFEE(LEV)    TO S-INTCHG.
           MOVE T-SERCHG(LEV)  TO S-SERCHG.
           MOVE T-LL(LEV) TO S-CL2.
           MOVE T-PP(LEV) TO S-AH.
           MOVE T-O2(LEV) TO S-OT.
           MOVE T-FEES(LEV) TO S-FEEAMT.
           MOVE T-EARNINGS(LEV) TO S-EARNINGS.
           MOVE T-PROCASH(LEV) TO S-PROCASH.
           MOVE T-PROCEEDS(LEV) TO S-PROCEEDS.

           PERFORM WRITE-DETAIL-LINE.
           MOVE 1 TO LN-FEED.
       GET-CREDIT-LIFE-TAG.
           IF LN-INSTYPES-DL
              MOVE "D" TO D-INSTYPE
              IF LN-INSURED = "M" OR "S"
                 MOVE "S" TO D-INSFLAG
              ELSE
                 MOVE "J" TO D-INSFLAG
              END-IF
           ELSE
              MOVE "L" TO D-INSTYPE
              IF LN-INSURED = "M" OR "S"
                 MOVE "S" TO D-INSFLAG
              ELSE
                 MOVE "J" TO D-INSFLAG.

      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLTCGS_SQL.CPY".
       COPY "LIBLP/LPLPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBLP/LPCDBGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".

       COPY "LIBGB/GBPRINT.CPY".
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
                   ISNULL(CAST(CDBFILE.CDB_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CDBFILE.CDB_DESC,''),
                   ISNULL(CDBFILE.CDB_CN_GROUP_ID,''),
                   CDBFILE.CDB_CN_PHONE,
                   CDBFILE.CDB_CN_PHONE_EXT,
                   CDBFILE.CDB_GC_GLFEENO,
                   ISNULL(CDBFILE.CDB_GC_GLFEEFINCHG,''),
                   ISNULL(CDBFILE.CDB_GC_GLFEE_INT_CHARGABLE,''),
                   ISNULL(CDBFILE.CDB_GC_FEE_GIFTNET,''),
                   CDBFILE.CDB_GC_FEE_PRODNO,
                   CDBFILE.CDB_GC_FEE_PRICE,
                   CDBFILE.CDB_GC_NEW_PRODNO,
                   CDBFILE.CDB_GC_FEE_MAXAMT,
                   CDBFILE.CDB_GC_ALT_GLFEENO,
                   CDBFILE.CDB_GC_COMM_GLFEENO,
                   CDBFILE.CDB_GC_COMM_PERCENT,
                   ISNULL(CDBFILE.CDB_GC_FEE_EARNINGS_FG,''),
                   CDBFILE.CDB_GC_BRANCH_COMM_AMOUNT
            FROM DBO.CDBFILE
            WHERE CDBFILE.CDB_TYPE >= :QCDB1-WBEG-TYPE
              AND CDBFILE.CDB_TYPE <= :QCDB1-WEND-TYPE
              AND CDBFILE.CDB_BRANCH >= :QCDB1-WBEG-BRANCH
              AND CDBFILE.CDB_BRANCH <= :QCDB1-WEND-BRANCH
              AND CDBFILE.CDB_CODE >= :QCDB1-WBEG-CODE
              AND CDBFILE.CDB_CODE <= :QCDB1-WEND-CODE
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
       READ-CDB1-FILE.
           PERFORM READ-IT.
           MOVE CDB-SELECT-PATH TO E-FILE.
           MOVE CDB1-KEY TO E-KEYX.

           MOVE CDB-TYPE    TO QCDB-TYPE. 
           MOVE CDB-BRANCH  TO QCDB-BRANCH.
           MOVE CDB-CODE    TO QCDB-CODE.

           EXEC SQL
            SELECT CDBFILE.CDB_TYPE,
                   CDBFILE.CDB_BRANCH,
                   CDBFILE.CDB_CODE,
                   CDBFILE.CDB_DESC
              INTO 
                   :QCDB-TYPE,
                   :QCDB-BRANCH,
                   :QCDB-CODE,
                   :QCDB-DESC
              FROM DBO.CDBFILE
             WHERE CDBFILE.CDB_TYPE   = :QCDB-TYPE
               AND CDBFILE.CDB_BRANCH = :QCDB-BRANCH
               AND CDBFILE.CDB_CODE   = :QCDB-CODE
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
                   FETCH CDB1_CURSOR INTO :QCDB-REC
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

           MOVE BW-OWNBR   TO QBW-OWNBR.
           MOVE BW-SSNO    TO QBW-SSNO.

           EXEC SQL
            SELECT BWFILE.BW_OWNBR,
                   BWFILE.BW_SSNO,
                   BWFILE.BW_LNAME, 
                   BWFILE.BW_FNAME, 
                   BWFILE.BW_ADR1, 
                   BWFILE.BW_ADR2, 
                   BWFILE.BW_CITY, 
                   BWFILE.BW_STATE, 
                   BWFILE.BW_ZIP,
                   BWFILE.BW_CONFIDENTIAL 
              INTO  
                  :QBW-OWNBR,
                  :QBW-SSNO,
                  :QBW-LNAME, 
                  :QBW-FNAME, 
                  :QBW-ADR1, 
                  :QBW-ADR2, 
                  :QBW-CITY, 
                  :QBW-STATE, 
                  :QBW-ZIP,
                  :QBW-CONFIDENTIAL                 
              FROM DBO.BWFILE
             WHERE BWFILE.BW_OWNBR = :QBW-OWNBR
               AND BWFILE.BW_SSNO  = :QBW-SSNO
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




      * COPY "LIBLP/LPLN3RN.CPY".
      * COPYMEMBER: LIBLP/LPLN3RN
      *-----------------------------------------------------------------
      *    STILL NEED TO LOAD THE FILE PATH WORKERS
      *-----------------------------------------------------------------
       LOAD-LN3-FILE.
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
       OPEN-LN3-FILE.
           PERFORM LOAD-LN3-FILE.
           PERFORM OPEN-IT.
           MOVE LN-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-LN3-FILE.
           PERFORM START-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-SELECT-PATH TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING LN-OWNBR, LN-CLASS,LN-ACCTNO
              DELIMITED BY " " INTO E-KEYX.

           IF ( LN3-CURSOR-STAT-GOOD )
              PERFORM CLOSE-LN3-FILE.

           IF ( QLN3-WEND-ACCTNO = ZEROES    ) OR
              ( QLN3-WEND-ACCTNO NOT NUMERIC )
              MOVE LN-OWNBR             TO QLN3-WBEG-OWNBR
              MOVE LN-OWNBR             TO QLN3-WEND-OWNBR
              MOVE LN-CLASS             TO QLN3-WBEG-CLASS
              MOVE ALL "9"              TO QLN3-WEND-CLASS
              MOVE LN-ACCTNO            TO QLN3-WBEG-ACCTNO
              MOVE ALL "9"              TO QLN3-WEND-ACCTNO.

           EXEC SQL
            DECLARE LN3_CURSOR CURSOR FOR
            SELECT LNFILE.LN_OWNBR,
                   LNFILE.LN_ACCTNO,
                   LNFILE.LN_CLASS,
                   LNFILE.LN_SUBCLASS,
                   LNFILE.LN_LAWCODE,
                   LNFILE.LN_SSNO1,
                   LNFILE.LN_MAKERCD1,
                   LNFILE.LN_SSNO2,
                   LNFILE.LN_MAKERCD2,
                   LNFILE.LN_SSNO3,
                   LNFILE.LN_MAKERCD3,
                   LNFILE.LN_SSNO4,
                   LNFILE.LN_MAKERCD4,
                   LNFILE.LN_ORGST,
                   LNFILE.LN_SPRCLASS,
                   LNFILE.LN_PURCD, 
                   LNFILE.LN_SRCD, 
                   LNFILE.LN_COLLCD, 
                   LNFILE.LN_POCD, 
                   LNFILE.LN_CONFIDENTCD, 
                   LNFILE.LN_LOANTYPE, 
                   CAST(LNFILE.LN_LOANDATE AS VARCHAR(10)), 
                   CAST(LNFILE.LN_POFFDATE AS VARCHAR(10)), 
                   LNFILE.LN_APRATE,
                   LNFILE.LN_LNAMT,
                   LNFILE.LN_FINCHG,
                   LNFILE.LN_INTCHG,
                   LNFILE.LN_SERCHG,
                   LNFILE.LN_EXTCHG,
                   LNFILE.LN_MAINTFEE,
                   LNFILE.LN_PROCEEDS,
                   LNFILE.LN_ORGTERM,
                   LNFILE.LN_1STPYAMT,
                   LNFILE.LN_NOREGPY,
                   LNFILE.LN_REGPYAMT,
                   LNFILE.LN_LASTPYAMT,
                   LNFILE.LN_PROCASH,
                   LNFILE.LN_TILLNO,
                   LNFILE.LN_STATE_WORKER,
                   LNFILE.LN_FEECODE_1, 
                   LNFILE.LN_FEECODE_2, 
                   LNFILE.LN_FEECODE_3, 
                   LNFILE.LN_FEECODE_4, 
                   LNFILE.LN_FEECODE_5, 
                   LNFILE.LN_FEECODE_6, 
                   LNFILE.LN_FEECODE_7, 
                   LNFILE.LN_FEECODE_8, 
                   LNFILE.LN_FEECODE_9, 
                   LNFILE.LN_FEECODE_10, 
                   LNFILE.LN_FEECODE_11, 
                   LNFILE.LN_FEECODE_12, 
                   LNFILE.LN_FEECODE_13, 
                   LNFILE.LN_FEECODE_14, 
                   LNFILE.LN_FEECODE_15, 
                   LNFILE.LN_FEECODE_16, 
                   LNFILE.LN_FEECODE_17, 
                   LNFILE.LN_FEECODE_18, 
                   LNFILE.LN_FEECODE_19, 
                   LNFILE.LN_FEECODE_20, 
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
                   LNFILE.LN_FEEOURS_1, 
                   LNFILE.LN_FEEOURS_2, 
                   LNFILE.LN_FEEOURS_3, 
                   LNFILE.LN_FEEOURS_4, 
                   LNFILE.LN_FEEOURS_5, 
                   LNFILE.LN_FEEOURS_6, 
                   LNFILE.LN_FEEOURS_7, 
                   LNFILE.LN_FEEOURS_8, 
                   LNFILE.LN_FEEOURS_9, 
                   LNFILE.LN_FEEOURS_10, 
                   LNFILE.LN_FEEOURS_11, 
                   LNFILE.LN_FEEOURS_12, 
                   LNFILE.LN_FEEOURS_13, 
                   LNFILE.LN_FEEOURS_14, 
                   LNFILE.LN_FEEOURS_15, 
                   LNFILE.LN_FEEOURS_16, 
                   LNFILE.LN_FEEOURS_17, 
                   LNFILE.LN_FEEOURS_18, 
                   LNFILE.LN_FEEOURS_19, 
                   LNFILE.LN_FEEOURS_20, 
                   LNFILE.LN_FEEFINCHG_1, 
                   LNFILE.LN_FEEFINCHG_2, 
                   LNFILE.LN_FEEFINCHG_3, 
                   LNFILE.LN_FEEFINCHG_4, 
                   LNFILE.LN_FEEFINCHG_5, 
                   LNFILE.LN_FEEFINCHG_6, 
                   LNFILE.LN_FEEFINCHG_7, 
                   LNFILE.LN_FEEFINCHG_8, 
                   LNFILE.LN_FEEFINCHG_9, 
                   LNFILE.LN_FEEFINCHG_10, 
                   LNFILE.LN_FEEFINCHG_11, 
                   LNFILE.LN_FEEFINCHG_12, 
                   LNFILE.LN_FEEFINCHG_13, 
                   LNFILE.LN_FEEFINCHG_14, 
                   LNFILE.LN_FEEFINCHG_15, 
                   LNFILE.LN_FEEFINCHG_16, 
                   LNFILE.LN_FEEFINCHG_17, 
                   LNFILE.LN_FEEFINCHG_18, 
                   LNFILE.LN_FEEFINCHG_19, 
                   LNFILE.LN_FEEFINCHG_20, 
                   LNFILE.LN_INSURED,
                   LNFILE.LN_INSTYPES,
                   LNFILE.LN_INSOURS_1, 
                   LNFILE.LN_INSOURS_2, 
                   LNFILE.LN_INSOURS_3, 
                   LNFILE.LN_INSOURS_4, 
                   LNFILE.LN_INSOURS_5, 
                   LNFILE.LN_INSOURS_6, 
                   LNFILE.LN_INSOURS_7, 
                   LNFILE.LN_INSOURS_8, 
                   LNFILE.LN_INSOURS_9, 
                   LNFILE.LN_INSOURS_10,
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
                   LNFILE.LN_INSFINCHG_1, 
                   LNFILE.LN_INSFINCHG_2, 
                   LNFILE.LN_INSFINCHG_3, 
                   LNFILE.LN_INSFINCHG_4, 
                   LNFILE.LN_INSFINCHG_5, 
                   LNFILE.LN_INSFINCHG_6, 
                   LNFILE.LN_INSFINCHG_7, 
                   LNFILE.LN_INSFINCHG_8, 
                   LNFILE.LN_INSFINCHG_9, 
                   LNFILE.LN_INSFINCHG_10,
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
                   LNFILE.LN_ANTICADJ_1,
                   LNFILE.LN_ANTICADJ_2,
                   LNFILE.LN_ANTICADJ_3,
                   LNFILE.LN_ANTICADJ_4,
                   LNFILE.LN_ANTICADJ_5,
                   LNFILE.LN_ANTICADJ_6,
                   LNFILE.LN_ANTICADJ_7,
                   LNFILE.LN_ANTICADJ_8,
                   LNFILE.LN_ANTICADJ_9,
                   LNFILE.LN_ANTICADJ_10,
                   LNFILE.LN_ACCERN_1,
                   LNFILE.LN_ACCERN_2,
                   LNFILE.LN_ACCERN_3,
                   LNFILE.LN_ACCERN_4,
                   LNFILE.LN_ACCERN_5,
                   LNFILE.LN_ACCERN_6,
                   LNFILE.LN_ACCERN_7,
                   LNFILE.LN_ACCERN_8,
                   LNFILE.LN_ACCERN_9,
                   LNFILE.LN_ACCERN_10,
                   LNFILE.LN_CREDITLIM,
                   LNFILE.LN_PRLNNO,
                   LNFILE.LN_PRBAL,
                   LNFILE.LN_PRINTDUE,
                   LNFILE.LN_PRFINCHGRB,
                   LNFILE.LN_PRINSRB,
                   LNFILE.LN_PRLNAMT,
                   LNFILE.LN_PRPDUE_DAYS,
                   LNFILE.LN_BR_TFR_FG
            FROM DBO.LNFILE
            WHERE LNFILE.LN_OWNBR  >= :QLN3-WBEG-OWNBR
              AND LNFILE.LN_OWNBR  <= :QLN3-WEND-OWNBR
              AND LNFILE.LN_CLASS  >= :QLN3-WBEG-CLASS
              AND LNFILE.LN_CLASS  <= :QLN3-WEND-CLASS
              AND LNFILE.LN_ACCTNO >= :QLN3-WBEG-ACCTNO
              AND LNFILE.LN_ACCTNO <= :QLN3-WEND-ACCTNO 
            ORDER BY LNFILE.LN_OWNBR,
                     LNFILE.LN_CLASS,
                     LNFILE.LN_ACCTNO
           END-EXEC.

           EXEC SQL
               OPEN LN3_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QLN3-WBEG-OWNBR
                          QLN3-WEND-OWNBR
                          QLN3-WBEG-CLASS
                          QLN3-WEND-CLASS
                          QLN3-WBEG-ACCTNO
                          QLN3-WEND-ACCTNO.

           MOVE STAT---GOOD TO LN3-CURSOR-STAT.

      *-----------------------------------------------------------------
      *START-LN3-FILE-GREATER.
      *    PERFORM START-IT.
      *    MOVE LN-PATH-OWNBR  TO LN-OWNBR.
      *    MOVE LN-SELECT-PATH TO E-FILE.
      *    MOVE SPACES         TO E-KEYX.
      *    STRING LN-OWNBR, LN-CLASS,LN-ACCTNO
      *       DELIMITED BY " " INTO E-KEYX.

      *    IF ( LN3-CURSOR-GR-STAT-GOOD )
      *       PERFORM CLOSE-LN3-FILE.

      *    IF ( QLN3-WEND-ACCTNO = ZEROES    ) OR
      *       ( QLN3-WEND-ACCTNO NOT NUMERIC )
      *       MOVE LN-OWNBR             TO QLN3-WBEG-OWNBR
      *       MOVE LN-OWNBR             TO QLN3-WEND-OWNBR
      *       MOVE LN-CLASS             TO QLN3-WBEG-CLASS
      *       MOVE ALL "9"              TO QLN3-WEND-CLASS
      *       MOVE LN-ACCTNO            TO QLN3-WBEG-ACCTNO
      *       MOVE ALL "9"              TO QLN3-WEND-ACCTNO.

      *    EXEC SQL
      *     DECLARE LN3_CURSOR_GR CURSOR FOR
      *     SELECT LNFILE.LN_OWNBR,
      *            LNFILE.LN_ACCTNO,
      *            LNFILE.LN_CLASS,
      *            LNFILE.LN_SUBCLASS,
      *            LNFILE.LN_LAWCODE,
      *            LNFILE.LN_SSNO1,
      *            ISNULL(LNFILE.LN_MAKERCD1,''),
      *            LNFILE.LN_SSNO2,
      *            ISNULL(LNFILE.LN_MAKERCD2,''),
      *            LNFILE.LN_SSNO3,
      *            ISNULL(LNFILE.LN_MAKERCD3,''),
      *            LNFILE.LN_SSNO4,
      *            ISNULL(LNFILE.LN_MAKERCD4,''),
      *            ISNULL(LNFILE.LN_ORGST,''),
      *            LNFILE.LN_ORGBR,
      *            ISNULL(LNFILE.LN_OWNST,''),
      *            LNFILE.LN_SPRCLASS,
      *            ISNULL(LNFILE.LN_CONVERCD,''),
      *            ISNULL(LNFILE.LN_MIDSTREAM,''),
      *            ISNULL(LNFILE.LN_PURCD,''),
      *            ISNULL(LNFILE.LN_SRCD,''),
      *            ISNULL(LNFILE.LN_COLLCD,''),
      *            ISNULL(LNFILE.LN_COLLID,''),
      *            ISNULL(LNFILE.LN_POCD,''),
      *            ISNULL(LNFILE.LN_PLCD,''),
      *            ISNULL(LNFILE.LN_PLTYPE,''),
      *            ISNULL(LNFILE.LN_CONFIDENTCD,''),
      *            ISNULL(LNFILE.LN_ALLOTCD,''),
      *            ISNULL(LNFILE.LN_RECASTCD,''),
      *            ISNULL(LNFILE.LN_SKIP_TRW,''),
      *            ISNULL(LNFILE.LN_STATUSFG,''),
      *            ISNULL(LNFILE.LN_SMALLOAN,''),
      *            ISNULL(LNFILE.LN_SALEFIN,''),
      *            ISNULL(LNFILE.LN_SERVACCT,''),
      *            ISNULL(LNFILE.LN_LOANTYPE,''),
      *            ISNULL(LNFILE.LN_ADDON_FG,''),
      *            ISNULL(LNFILE.LN_PAY_SCHLD_FG,''),
      *            LNFILE.LN_LC_CODE,
      *            ISNULL(LNFILE.LN_CAN_LOAN_NO,''),
      *            ISNULL(LNFILE.LN_STATFLAGS,''),
      *            ISNULL(CAST(LNFILE.LN_ENTDATE AS VARCHAR(10)),'0'),
      *            ISNULL(CAST(LNFILE.LN_LOANDATE AS VARCHAR(10)),'0'),
      *            ISNULL(CAST(LNFILE.LN_PURDATE AS VARCHAR(10)),'0'),
      *            ISNULL(CAST(LNFILE.LN_INTDATE AS VARCHAR(10)),'0'),
      *            ISNULL(CAST(LNFILE.LN_CASHDATE AS VARCHAR(10)),'0'),
      *            ISNULL(CAST(LNFILE.LN_FILEDATE AS VARCHAR(10)),'0'),
      *            ISNULL(CAST(LNFILE.LN_RESCISSION_DATE
      *                                           AS VARCHAR(10)),'0'),
      *            ISNULL(CAST(LNFILE.LN_PART_RECOURSE_DATE
      *                                           AS VARCHAR(10)),'0'),
      *            ISNULL(CAST(LNFILE.LN_ORIG_1STPYDATE
      *                                           AS VARCHAR(10)),'0'),
      *            ISNULL(CAST(LNFILE.LN_1STPYDATE
      *                                           AS VARCHAR(10)),'0'),
      *            ISNULL(CAST(LNFILE.LN_JDDATE AS VARCHAR(10)),'0'),
      *            ISNULL(CAST(LNFILE.LN_PLDATE AS VARCHAR(10)),'0'),
      *            ISNULL(CAST(LNFILE.LN_PL_TRDATE
      *                                           AS VARCHAR(10)),'0'),
      *            ISNULL(CAST(LNFILE.LN_ACCELDATE
      *                                           AS VARCHAR(10)),'0'),
      *            ISNULL(CAST(LNFILE.LN_REDUDATE AS VARCHAR(10)),'0'),
      *            ISNULL(CAST(LNFILE.LN_POFFDATE AS VARCHAR(10)),'0'),
      *            ISNULL(CAST(LNFILE.LN_PO_TRDATE
      *                                           AS VARCHAR(10)),'0'),
      *            ISNULL(CAST(LNFILE.LN_LEGALDATE
      *                                           AS VARCHAR(10)),'0'),
      *            ISNULL(CAST(LNFILE.LN_BNKRPTDATE
      *                                           AS VARCHAR(10)),'0'),
      *            ISNULL(CAST(LNFILE.LN_BNKRPT_DISMISS_DATE
      *                                           AS VARCHAR(10)),'0'),
      *            ISNULL(CAST(LNFILE.LN_BNKRPT_DISCHARGE_DATE
      *                                           AS VARCHAR(10)),'0'),
      *            ISNULL(CAST(LNFILE.LN_BNKRPT_REAFFIRM_DATE
      *                                           AS VARCHAR(10)),'0'),
      *            ISNULL(CAST(LNFILE.LN_DEBTAIDATE
      *                                           AS VARCHAR(10)),'0'),
      *            ISNULL(CAST(LNFILE.LN_REPODATE AS VARCHAR(10)),'0'),
      *            ISNULL(CAST(LNFILE.LN_REDEEMDATE
      *                                           AS VARCHAR(10)),'0'),
      *            ISNULL(CAST(LNFILE.LN_SOLDDATE AS VARCHAR(10)),'0'),
      *            ISNULL(CAST(LNFILE.LN_INS_CONFIRM_DATE
      *                                           AS VARCHAR(10)),'0'),
      *            ISNULL(CAST(LNFILE.LN_PRIOR_RN_FOR10_DATE
      *                                           AS VARCHAR(10)),'0'),
      *            ISNULL(CAST(LNFILE.LN_LAST_RN_FOR10_DATE
      *                                           AS VARCHAR(10)),'0'),
      *            ISNULL(CAST(LNFILE.LN_TRW_DOFD_DATE
      *                                           AS VARCHAR(10)),'0'),
      *            ISNULL(CAST(LNFILE.LN_LAST_PYPA_DATE
      *                                           AS VARCHAR(10)),'0'),
      *            ISNULL(CAST(LNFILE.LN_BT_TRDATE
      *                                           AS VARCHAR(10)),'0'),
      *            ISNULL(CAST(LNFILE.LN_METRO2_LAST_PMT_DATE
      *                                           AS VARCHAR(10)),'0'),
      *            ISNULL(CAST(LNFILE.LN_FOLLOWUP_DATE
      *                                           AS VARCHAR(10)),'0'),
      *            ISNULL(CAST(LNFILE.LN_LAST_ACCRUAL_DATE
      *                                           AS VARCHAR(10)),'0'),
      *            LNFILE.LN_APRATE,
      *            LNFILE.LN_EFFRATE,
      *            LNFILE.LN_SMPRATE,
      *            LNFILE.LN_YLDRATE,
      *            LNFILE.LN_OVRDRATE,
      *            LNFILE.LN_ENDORATE,
      *            LNFILE.LN_JDRATE,
      *            LNFILE.LN_ACCRATE,
      *            LNFILE.LN_REDURATE,
      *            LNFILE.LN_CONRATE,
      *            LNFILE.LN_SERVPER,
      *            LNFILE.LN_SELLPR,
      *            LNFILE.LN_DOWNPY,
      *            LNFILE.LN_TRADEIN,
      *            LNFILE.LN_LNAMT,
      *            LNFILE.LN_FINCHG,
      *            LNFILE.LN_INTCHG,
      *            LNFILE.LN_SERCHG,
      *            LNFILE.LN_EXTCHG,
      *            LNFILE.LN_MAINTFEE,
      *            LNFILE.LN_POINTS,
      *            LNFILE.LN_PROCEEDS,
      *            LNFILE.LN_ORGTERM,
      *            LNFILE.LN_1STPYAMT,
      *            LNFILE.LN_NOREGPY,
      *            LNFILE.LN_REGPYAMT,
      *            LNFILE.LN_LASTPYAMT,
      *            LNFILE.LN_PROCASH,
      *            LNFILE.LN_TILLNO,
      *            ISNULL(LNFILE.LN_STATE_FLAG,''),
      *            LNFILE.LN_STATE_WORKER,
      *            LNFILE.LN_STATE_WORKER2,
      *            ISNULL(LNFILE.LN_FEECODE_1,''),
      *            LNFILE.LN_FEEAMT_1,
      *            ISNULL(LNFILE.LN_FEEOURS_1,''),
      *            ISNULL(LNFILE.LN_FEEFINCHG_1,''),
      *            ISNULL(LNFILE.LN_FEECODE_2,''),
      *            LNFILE.LN_FEEAMT_2,
      *            ISNULL(LNFILE.LN_FEEOURS_2,''),
      *            ISNULL(LNFILE.LN_FEEFINCHG_2,''),
      *            ISNULL(LNFILE.LN_FEECODE_3,''),
      *            LNFILE.LN_FEEAMT_3,
      *            ISNULL(LNFILE.LN_FEEOURS_3,''),
      *            ISNULL(LNFILE.LN_FEEFINCHG_3,''),
      *            ISNULL(LNFILE.LN_FEECODE_4,''),
      *            LNFILE.LN_FEEAMT_4,
      *            ISNULL(LNFILE.LN_FEEOURS_4,''),
      *            ISNULL(LNFILE.LN_FEEFINCHG_4,''),
      *            ISNULL(LNFILE.LN_FEECODE_5,''),
      *            LNFILE.LN_FEEAMT_5,
      *            ISNULL(LNFILE.LN_FEEOURS_5,''),
      *            ISNULL(LNFILE.LN_FEEFINCHG_5,''),
      *            ISNULL(LNFILE.LN_FEECODE_6,''),
      *            LNFILE.LN_FEEAMT_6,
      *            ISNULL(LNFILE.LN_FEEOURS_6,''),
      *            ISNULL(LNFILE.LN_FEEFINCHG_6,''),
      *            ISNULL(LNFILE.LN_FEECODE_7,''),
      *            LNFILE.LN_FEEAMT_7,
      *            ISNULL(LNFILE.LN_FEEOURS_7,''),
      *            ISNULL(LNFILE.LN_FEEFINCHG_7,''),
      *            ISNULL(LNFILE.LN_FEECODE_8,''),
      *            LNFILE.LN_FEEAMT_8,
      *            ISNULL(LNFILE.LN_FEEOURS_8,''),
      *            ISNULL(LNFILE.LN_FEEFINCHG_8,''),
      *            ISNULL(LNFILE.LN_FEECODE_9,''),
      *            LNFILE.LN_FEEAMT_9,
      *            ISNULL(LNFILE.LN_FEEOURS_9,''),
      *            ISNULL(LNFILE.LN_FEEFINCHG_9,''),
      *            ISNULL(LNFILE.LN_FEECODE_10,''),
      *            LNFILE.LN_FEEAMT_10,
      *            ISNULL(LNFILE.LN_FEEOURS_10,''),
      *            ISNULL(LNFILE.LN_FEEFINCHG_10,''),
      *            ISNULL(LNFILE.LN_FEECODE_11,''),
      *            LNFILE.LN_FEEAMT_11,
      *            ISNULL(LNFILE.LN_FEEOURS_11,''),
      *            ISNULL(LNFILE.LN_FEEFINCHG_11,''),
      *            ISNULL(LNFILE.LN_FEECODE_12,''),
      *            LNFILE.LN_FEEAMT_12,
      *            ISNULL(LNFILE.LN_FEEOURS_12,''),
      *            ISNULL(LNFILE.LN_FEEFINCHG_12,''),
      *            ISNULL(LNFILE.LN_FEECODE_13,''),
      *            LNFILE.LN_FEEAMT_13,
      *            ISNULL(LNFILE.LN_FEEOURS_13,''),
      *            ISNULL(LNFILE.LN_FEEFINCHG_13,''),
      *            ISNULL(LNFILE.LN_FEECODE_14,''),
      *            LNFILE.LN_FEEAMT_14,
      *            ISNULL(LNFILE.LN_FEEOURS_14,''),
      *            ISNULL(LNFILE.LN_FEEFINCHG_14,''),
      *            ISNULL(LNFILE.LN_FEECODE_15,''),
      *            LNFILE.LN_FEEAMT_15,
      *            ISNULL(LNFILE.LN_FEEOURS_15,''),
      *            ISNULL(LNFILE.LN_FEEFINCHG_15,''),
      *            ISNULL(LNFILE.LN_FEECODE_16,''),
      *            LNFILE.LN_FEEAMT_16,
      *            ISNULL(LNFILE.LN_FEEOURS_16,''),
      *            ISNULL(LNFILE.LN_FEEFINCHG_16,''),
      *            ISNULL(LNFILE.LN_FEECODE_17,''),
      *            LNFILE.LN_FEEAMT_17,
      *            ISNULL(LNFILE.LN_FEEOURS_17,''),
      *            ISNULL(LNFILE.LN_FEEFINCHG_17,''),
      *            ISNULL(LNFILE.LN_FEECODE_18,''),
      *            LNFILE.LN_FEEAMT_18,
      *            ISNULL(LNFILE.LN_FEEOURS_18,''),
      *            ISNULL(LNFILE.LN_FEEFINCHG_18,''),
      *            ISNULL(LNFILE.LN_FEECODE_19,''),
      *            LNFILE.LN_FEEAMT_19,
      *            ISNULL(LNFILE.LN_FEEOURS_19,''),
      *            ISNULL(LNFILE.LN_FEEFINCHG_19,''),
      *            ISNULL(LNFILE.LN_FEECODE_20,''),
      *            LNFILE.LN_FEEAMT_20,
      *            ISNULL(LNFILE.LN_FEEOURS_20,''),
      *            ISNULL(LNFILE.LN_FEEFINCHG_20,''),
      *            ISNULL(LNFILE.LN_INSURED,''),
      *            ISNULL(LNFILE.LN_INSTYPES,''),
      *            ISNULL(LNFILE.LN_INSELIGIBLE,''),
      *            LNFILE.LN_INSCOMP_1,
      *            ISNULL(LNFILE.LN_INSOURS_1,''),
      *            ISNULL(CAST(LNFILE.LN_INSEFF_DATE_1
      *                                           AS VARCHAR(10)),'0'),
      *            ISNULL(CAST(LNFILE.LN_INSEXP_DATE_1
      *                                           AS VARCHAR(10)),'0'),
      *            LNFILE.LN_INSPREM_1,
      *            LNFILE.LN_INSCOVR_1,
      *            LNFILE.LN_INSCOMM_1,
      *            LNFILE.LN_INSANTICERN_1,
      *            LNFILE.LN_INSANTICADJ_1,
      *            LNFILE.LN_INSUFERN_1,
      *            LNFILE.LN_INSACCERN_1,
      *            LNFILE.LN_INSEARN_ORDER_1,
      *            ISNULL(LNFILE.LN_INSFINCHG_1,''),
      *            LNFILE.LN_INSCOMP_2,
      *            ISNULL(LNFILE.LN_INSOURS_2,''),
      *            ISNULL(CAST(LNFILE.LN_INSEFF_DATE_2
      *                                           AS VARCHAR(10)),'0'),
      *            ISNULL(CAST(LNFILE.LN_INSEXP_DATE_2
      *                                           AS VARCHAR(10)),'0'),
      *            LNFILE.LN_INSPREM_2,
      *            LNFILE.LN_INSCOVR_2,
      *            LNFILE.LN_INSCOMM_2,
      *            LNFILE.LN_INSANTICERN_2,
      *            LNFILE.LN_INSANTICADJ_2,
      *            LNFILE.LN_INSUFERN_2,
      *            LNFILE.LN_INSACCERN_2,
      *            LNFILE.LN_INSEARN_ORDER_2,
      *            ISNULL(LNFILE.LN_INSFINCHG_2,''),
      *            LNFILE.LN_INSCOMP_3,
      *            ISNULL(LNFILE.LN_INSOURS_3,''),
      *            ISNULL(CAST(LNFILE.LN_INSEFF_DATE_3
      *                                           AS VARCHAR(10)),'0'),
      *            ISNULL(CAST(LNFILE.LN_INSEXP_DATE_3
      *                                           AS VARCHAR(10)),'0'),
      *            LNFILE.LN_INSPREM_3,
      *            LNFILE.LN_INSCOVR_3,
      *            LNFILE.LN_INSCOMM_3,
      *            LNFILE.LN_INSANTICERN_3,
      *            LNFILE.LN_INSANTICADJ_3,
      *            LNFILE.LN_INSUFERN_3,
      *            LNFILE.LN_INSACCERN_3,
      *            LNFILE.LN_INSEARN_ORDER_3,
      *            ISNULL(LNFILE.LN_INSFINCHG_3,''),
      *            LNFILE.LN_INSCOMP_4,
      *            ISNULL(LNFILE.LN_INSOURS_4,''),
      *            ISNULL(CAST(LNFILE.LN_INSEFF_DATE_4
      *                                           AS VARCHAR(10)),'0'),
      *            ISNULL(CAST(LNFILE.LN_INSEXP_DATE_4
      *                                           AS VARCHAR(10)),'0'),
      *            LNFILE.LN_INSPREM_4,
      *            LNFILE.LN_INSCOVR_4,
      *            LNFILE.LN_INSCOMM_4,
      *            LNFILE.LN_INSANTICERN_4,
      *            LNFILE.LN_INSANTICADJ_4,
      *            LNFILE.LN_INSUFERN_4,
      *            LNFILE.LN_INSACCERN_4,
      *            LNFILE.LN_INSEARN_ORDER_4,
      *            ISNULL(LNFILE.LN_INSFINCHG_4,''),
      *            LNFILE.LN_INSCOMP_5,
      *            ISNULL(LNFILE.LN_INSOURS_5,''),
      *            ISNULL(CAST(LNFILE.LN_INSEFF_DATE_5
      *                                           AS VARCHAR(10)),'0'),
      *            ISNULL(CAST(LNFILE.LN_INSEXP_DATE_5
      *                                           AS VARCHAR(10)),'0'),
      *            LNFILE.LN_INSPREM_5,
      *            LNFILE.LN_INSCOVR_5,
      *            LNFILE.LN_INSCOMM_5,
      *            LNFILE.LN_INSANTICERN_5,
      *            LNFILE.LN_INSANTICADJ_5,
      *            LNFILE.LN_INSUFERN_5,
      *            LNFILE.LN_INSACCERN_5,
      *            LNFILE.LN_INSEARN_ORDER_5,
      *            ISNULL(LNFILE.LN_INSFINCHG_5,''),
      *            LNFILE.LN_INSCOMP_6,
      *            ISNULL(LNFILE.LN_INSOURS_6,''),
      *            ISNULL(CAST(LNFILE.LN_INSEFF_DATE_6
      *                                           AS VARCHAR(10)),'0'),
      *            ISNULL(CAST(LNFILE.LN_INSEXP_DATE_6
      *                                           AS VARCHAR(10)),'0'),
      *            LNFILE.LN_INSPREM_6,
      *            LNFILE.LN_INSCOVR_6,
      *            LNFILE.LN_INSCOMM_6,
      *            LNFILE.LN_INSANTICERN_6,
      *            LNFILE.LN_INSANTICADJ_6,
      *            LNFILE.LN_INSUFERN_6,
      *            LNFILE.LN_INSACCERN_6,
      *            LNFILE.LN_INSEARN_ORDER_6,
      *            ISNULL(LNFILE.LN_INSFINCHG_6,''),
      *            LNFILE.LN_INSCOMP_7,
      *            ISNULL(LNFILE.LN_INSOURS_7,''),
      *            ISNULL(CAST(LNFILE.LN_INSEFF_DATE_7
      *                                           AS VARCHAR(10)),'0'),
      *            ISNULL(CAST(LNFILE.LN_INSEXP_DATE_7
      *                                           AS VARCHAR(10)),'0'),
      *            LNFILE.LN_INSPREM_7,
      *            LNFILE.LN_INSCOVR_7,
      *            LNFILE.LN_INSCOMM_7,
      *            LNFILE.LN_INSANTICERN_7,
      *            LNFILE.LN_INSANTICADJ_7,
      *            LNFILE.LN_INSUFERN_7,
      *            LNFILE.LN_INSACCERN_7,
      *            LNFILE.LN_INSEARN_ORDER_7,
      *            ISNULL(LNFILE.LN_INSFINCHG_7,''),
      *            LNFILE.LN_INSCOMP_8,
      *            ISNULL(LNFILE.LN_INSOURS_8,''),
      *            ISNULL(CAST(LNFILE.LN_INSEFF_DATE_8
      *                                           AS VARCHAR(10)),'0'),
      *            ISNULL(CAST(LNFILE.LN_INSEXP_DATE_8
      *                                           AS VARCHAR(10)),'0'),
      *            LNFILE.LN_INSPREM_8,
      *            LNFILE.LN_INSCOVR_8,
      *            LNFILE.LN_INSCOMM_8,
      *            LNFILE.LN_INSANTICERN_8,
      *            LNFILE.LN_INSANTICADJ_8,
      *            LNFILE.LN_INSUFERN_8,
      *            LNFILE.LN_INSACCERN_8,
      *            LNFILE.LN_INSEARN_ORDER_8,
      *            ISNULL(LNFILE.LN_INSFINCHG_8,''),
      *            LNFILE.LN_INSCOMP_9,
      *            ISNULL(LNFILE.LN_INSOURS_9,''),
      *            ISNULL(CAST(LNFILE.LN_INSEFF_DATE_9
      *                                           AS VARCHAR(10)),'0'),
      *            ISNULL(CAST(LNFILE.LN_INSEXP_DATE_9
      *                                           AS VARCHAR(10)),'0'),
      *            LNFILE.LN_INSPREM_9,
      *            LNFILE.LN_INSCOVR_9,
      *            LNFILE.LN_INSCOMM_9,
      *            LNFILE.LN_INSANTICERN_9,
      *            LNFILE.LN_INSANTICADJ_9,
      *            LNFILE.LN_INSUFERN_9,
      *            LNFILE.LN_INSACCERN_9,
      *            LNFILE.LN_INSEARN_ORDER_9,
      *            ISNULL(LNFILE.LN_INSFINCHG_9,''),
      *            LNFILE.LN_INSCOMP_10,
      *            ISNULL(LNFILE.LN_INSOURS_10,''),
      *            ISNULL(CAST(LNFILE.LN_INSEFF_DATE_10
      *                                           AS VARCHAR(10)),'0'),
      *            ISNULL(CAST(LNFILE.LN_INSEXP_DATE_10
      *                                           AS VARCHAR(10)),'0'),
      *            LNFILE.LN_INSPREM_10,
      *            LNFILE.LN_INSCOVR_10,
      *            LNFILE.LN_INSCOMM_10,
      *            LNFILE.LN_INSANTICERN_10,
      *            LNFILE.LN_INSANTICADJ_10,
      *            LNFILE.LN_INSUFERN_10,
      *            LNFILE.LN_INSACCERN_10,
      *            LNFILE.LN_INSEARN_ORDER_10,
      *            ISNULL(LNFILE.LN_INSFINCHG_10,''),
      *            LNFILE.LN_PY_YYYYMM_RECDEL,
      *            LNFILE.LN_MTD_PAYS_RECDEL,
      *            ISNULL(LNFILE.LN_GARNISH_EXTR,''),
      *            ISNULL(LNFILE.LN_SHIP_SAME_FG,''),
      *            LNFILE.LN_AD_D_INS_COVR,
      *            LNFILE.LN_TOT_PARTIAL_CHGOFF,
      *            ISNULL(LNFILE.LN_LPO,''),
      *            ISNULL(LNFILE.LN_REP_CD,''),
      *            LNFILE.LN_EMP_ID,
      *            ISNULL(LNFILE.LN_PROCESS_CD,''),
      *            LNFILE.LN_WCBC_CREDITLIM,
      *            ISNULL(LNFILE.LN_FOLLOWUP_CD,''),
      *            ISNULL(LNFILE.LN_MIP_FG,''),
      *            LNFILE.LN_MIP_MTD_CLAHPAID,
      *            ISNULL(CAST(LNFILE.LN_MIP_CLPDTH_DATE
      *                                           AS VARCHAR(10)),'0'),
      *            LNFILE.LN_MIP_TOT_CLPAID,
      *            ISNULL(CAST(LNFILE.LN_MIP_AHPDTH_DATE
      *                                           AS VARCHAR(10)),'0'),
      *            LNFILE.LN_MIP_TOT_AHPAID,
      *            LNFILE.LN_IL_GROSS_INCOME,
      *            LNFILE.LN_IL_GROSS_INCOME2,
      *            ISNULL(LNFILE.LN_RESCISSION,''),
      *            ISNULL(LNFILE.LN_INTERIM_TRW,''),
      *            LNFILE.LN_DLNO,
      *            LNFILE.LN_DLPOOL,
      *            ISNULL(LNFILE.LN_DLRECOURSE,''),
      *            LNFILE.LN_DLPARTRCAMT,
      *            LNFILE.LN_ORIGPURAMT,
      *            LNFILE.LN_PURBAL,
      *            LNFILE.LN_DLRVPRIN_1,
      *            LNFILE.LN_DLRVPRIN_2,
      *            LNFILE.LN_DLRVPRIN_3,
      *            LNFILE.LN_DLRVCHGS_1,
      *            LNFILE.LN_DLRVCHGS_2,
      *            LNFILE.LN_DLRVCHGS_3,
      *            LNFILE.LN_DLPARVHELD,
      *            LNFILE.LN_DLPARVPAID,
      *            LNFILE.LN_DLDISC,
      *            ISNULL(CAST(LNFILE.LN_LCPDTH_DATE
      *                                           AS VARCHAR(10)),'0'),
      *            ISNULL(CAST(LNFILE.LN_INTPDTH_DATE
      *                                           AS VARCHAR(10)),'0'),
      *            LNFILE.LN_CURBAL,
      *            LNFILE.LN_INTBAL,
      *            LNFILE.LN_LCBAL,
      *            LNFILE.LN_OTHBAL,
      *            LNFILE.LN_OT2BAL,
      *            LNFILE.LN_LCPAID,
      *            LNFILE.LN_LCPARTIALS,
      *            LNFILE.LN_STMT_COUNTER,
      *            LNFILE.LN_PRIOR_STMT_COUNTER,
      *            ISNULL(LNFILE.LN_FROZEN_REASON,''),
      *            ISNULL(LNFILE.LN_COUPON_STATEMENT,''),
      *            LNFILE.LN_MONTHEND_BAL,
      *            LNFILE.LN_ANTICTERM,
      *            LNFILE.LN_ANTICERN_1,
      *            LNFILE.LN_ANTICADJ_1,
      *            LNFILE.LN_UFERN_1,
      *            LNFILE.LN_ACCERN_1,
      *            LNFILE.LN_EARN_ORDER_1,
      *            LNFILE.LN_ANTICERN_2,
      *            LNFILE.LN_ANTICADJ_2,
      *            LNFILE.LN_UFERN_2,
      *            LNFILE.LN_ACCERN_2,
      *            LNFILE.LN_EARN_ORDER_2,
      *            LNFILE.LN_ANTICERN_3,
      *            LNFILE.LN_ANTICADJ_3,
      *            LNFILE.LN_UFERN_3,
      *            LNFILE.LN_ACCERN_3,
      *            LNFILE.LN_EARN_ORDER_3,
      *            LNFILE.LN_ANTICERN_4,
      *            LNFILE.LN_ANTICADJ_4,
      *            LNFILE.LN_UFERN_4,
      *            LNFILE.LN_ACCERN_4,
      *            LNFILE.LN_EARN_ORDER_4,
      *            LNFILE.LN_ANTICERN_5,
      *            LNFILE.LN_ANTICADJ_5,
      *            LNFILE.LN_UFERN_5,
      *            LNFILE.LN_ACCERN_5,
      *            LNFILE.LN_EARN_ORDER_5,
      *            LNFILE.LN_ANTICERN_6,
      *            LNFILE.LN_ANTICADJ_6,
      *            LNFILE.LN_UFERN_6,
      *            LNFILE.LN_ACCERN_6,
      *            LNFILE.LN_EARN_ORDER_6,
      *            LNFILE.LN_ANTICERN_7,
      *            LNFILE.LN_ANTICADJ_7,
      *            LNFILE.LN_UFERN_7,
      *            LNFILE.LN_ACCERN_7,
      *            LNFILE.LN_EARN_ORDER_7,
      *            LNFILE.LN_ANTICERN_8,
      *            LNFILE.LN_ANTICADJ_8,
      *            LNFILE.LN_UFERN_8,
      *            LNFILE.LN_ACCERN_8,
      *            LNFILE.LN_EARN_ORDER_8,
      *            LNFILE.LN_ANTICERN_9,
      *            LNFILE.LN_ANTICADJ_9,
      *            LNFILE.LN_UFERN_9,
      *            LNFILE.LN_ACCERN_9,
      *            LNFILE.LN_EARN_ORDER_9,
      *            LNFILE.LN_ANTICERN_10,
      *            LNFILE.LN_ANTICADJ_10,
      *            LNFILE.LN_UFERN_10,
      *            LNFILE.LN_ACCERN_10,
      *            LNFILE.LN_EARN_ORDER_10,
      *            LNFILE.LN_LMO_PRIOR_INTEREST,
      *            LNFILE.LN_PRIOR_UNEARNED,
      *            ISNULL(LNFILE.LN_ERNDELQ_STOP,''),
      *            LNFILE.LN_NONACRERN,
      *            LNFILE.LN_PRIOR_NONACRERN,
      *            ISNULL(CAST(LNFILE.LN_ERN_IB_PDTH_DATE
      *                                           AS VARCHAR(10)),'0'),
      *            LNFILE.LN_ERN_IB_PRINBAL,
      *            LNFILE.LN_ERN_IB_INTOWE,
      *            LNFILE.LN_TOTERN,
      *            LNFILE.LN_TOTOTH,
      *            LNFILE.LN_TOTINT,
      *            LNFILE.LN_TOTLCHG,
      *            LNFILE.LN_TOTDEF,
      *            LNFILE.LN_TOTSERACCT,
      *            LNFILE.LN_LCWAIVERS,
      *            LNFILE.LN_INTWAIVERS,
      *            LNFILE.LN_TOTPAYMNTD,
      *            LNFILE.LN_TOTEXCPAYMNTD,
      *            LNFILE.LN_TOTNODEF,
      *            LNFILE.LN_YTDNODEF,
      *            LNFILE.LN_TOTNODEFACT,
      *            LNFILE.LN_YTDNODEFACT,
      *            LNFILE.LN_TOTNOALDEL,
      *            LNFILE.LN_YTDNOALDEL,
      *            LNFILE.LN_YTDINTPD,
      *            LNFILE.LN_LYRINTPD,
      *            LNFILE.LN_TOTINTPD,
      *            LNFILE.LN_RECASTINT,
      *            LNFILE.LN_RECASTPRIN,
      *            ISNULL(LNFILE.LN_CL_REBATE,''),
      *            ISNULL(LNFILE.LN_AH_REBATE,''),
      *            ISNULL(LNFILE.LN_PP_REBATE,''),
      *            ISNULL(LNFILE.LN_INT_REBATE,''),
      *            ISNULL(LNFILE.LN_SC_REBATE,''),
      *            ISNULL(LNFILE.LN_MF_REBATE,''),
      *            ISNULL(LNFILE.LN_DF_REBATE,''),
      *            ISNULL(LNFILE.LN_EX_REBATE,''),
      *            ISNULL(LNFILE.LN_O1_REBATE,''),
      *            ISNULL(LNFILE.LN_O2_REBATE,''),
      *            ISNULL(LNFILE.LN_O3_REBATE,''),
      *            ISNULL(LNFILE.LN_O4_REBATE,''),
      *            ISNULL(LNFILE.LN_O5_REBATE,''),
      *            ISNULL(LNFILE.LN_O6_REBATE,''),
      *            ISNULL(LNFILE.LN_O7_REBATE,''),
      *            ISNULL(LNFILE.LN_UN_O7_REB,''),
      *            ISNULL(LNFILE.LN_UN_O8_REB,''),
      *            ISNULL(LNFILE.LN_UN_O9_REB,''),
      *            ISNULL(LNFILE.LN_UN_O10_REB,''),
      *            ISNULL(LNFILE.LN_PLREASON,''),
      *            ISNULL(LNFILE.LN_COLLECTOR,''),
      *            ISNULL(LNFILE.LN_ACTIONCD,''),
      *            ISNULL(LNFILE.LN_ACTIONCD2,''),
      *            ISNULL(LNFILE.LN_ATCODE,''),
      *            ISNULL(LNFILE.LN_RECODE,''),
      *            ISNULL(LNFILE.LN_ESCROW_FG,''),
      *            LNFILE.LN_PROMDAYS_PASTDUE,
      *            ISNULL(LNFILE.LN_PROMID,''),
      *            ISNULL(CAST(LNFILE.LN_PROMDATE AS VARCHAR(10)),'0'),
      *            LNFILE.LN_PROMAMT,
      *            ISNULL(CAST(LNFILE.LN_PROMDATE2
      *                                           AS VARCHAR(10)),'0'),
      *            ISNULL(CAST(LNFILE.LN_DATE_PAID_LAST
      *                                           AS VARCHAR(10)),'0'),
      *            LNFILE.LN_AMT_PAID_LAST,
      *            LNFILE.LN_LASTCFORMNO,
      *            ISNULL(CAST(LNFILE.LN_LASTCFORMDATE
      *                                           AS VARCHAR(10)),'0'),
      *            LNFILE.LN_DELCNT,
      *            LNFILE.LN_DELCNT2,
      *            LNFILE.LN_CREDITLIM,
      *            LNFILE.LN_CRSCORE,
      *            LNFILE.LN_LASTSFORMNO,
      *            ISNULL(CAST(LNFILE.LN_LASTSFORMDATE
      *                                           AS VARCHAR(10)),'0'),
      *            LNFILE.LN_SOLCNT,
      *            ISNULL(LNFILE.LN_MESSAGE,''),
      *            LNFILE.LN_OLDACCTNO,
      *            LNFILE.LN_PRLNNO,
      *            LNFILE.LN_PRBAL,
      *            LNFILE.LN_PRINTDUE,
      *            LNFILE.LN_PRFINCHGRB,
      *            LNFILE.LN_PRINSRB,
      *            ISNULL(LNFILE.LN_PRPOCD,''),
      *            ISNULL(CAST(LNFILE.LN_PRPODATE AS VARCHAR(10)),'0'),
      *            LNFILE.LN_PRDELINQAMT,
      *            LNFILE.LN_PRLNAMT,
      *            LNFILE.LN_PRPDUE_DAYS,
      *            LNFILE.LN_PRDLDISC_UNEARN,
      *            LNFILE.LN_PRCONPROFILE,
      *            LNFILE.LN_SUM_ORIGPURAMT,
      *            LNFILE.LN_SUM_LNAMT,
      *            LNFILE.LN_SUM_FINCHG,
      *            LNFILE.LN_SUM_INTCHG,
      *            LNFILE.LN_SUM_ORGTERM,
      *            LNFILE.LN_SUM_REGPYAMT,
      *            LNFILE.LN_SUM_EXTCHG,
      *            LNFILE.LN_SUM_SERCHG,
      *            LNFILE.LN_SUM_MAINTFEE,
      *            ISNULL(LNFILE.LN_UNITPER_CD,''),
      *            LNFILE.LN_UNITPER_FREQ,
      *            LNFILE.LN_DAYS_IN_UNITPER,
      *            LNFILE.LN_UNITPER_PER_YEAR,
      *            ISNULL(LNFILE.LN_REPOCD,''),
      *            ISNULL(LNFILE.LN_BR_TFR_FG,''),
      *            LNFILE.LN_O3INS_DLPROC,
      *            LNFILE.LN_INSPREM_DLADJ,
      *            LNFILE.LN_PLAMT,
      *            LNFILE.LN_MTD_PRIN,
      *            LNFILE.LN_MTD_LC,
      *            LNFILE.LN_MTD_INT,
      *            LNFILE.LN_MTD_OT,
      *            LNFILE.LN_MTD_EX,
      *            LNFILE.LN_BNKRPT_CHAPTER,
      *            ISNULL(LNFILE.LN_BMO_CON_STATUS,''),
      *            ISNULL(LNFILE.LN_BMO_REC_STATUS,''),
      *            LNFILE.LN_APP_NUMBER,
      *            ISNULL(LNFILE.LN_GIFTNET,''),
      *            LNFILE.LN_GIFTNET_OUR_PO,
      *            ISNULL(LNFILE.LN_PROC_DISBURSED,''),
      *            ISNULL(LNFILE.LN_NOODDPY,''),
      *            LNFILE.LN_CONPROFILE_1,
      *            LNFILE.LN_CONPROFILE_2,
      *            LNFILE.LN_CONPROFILE_3,
      *            LNFILE.LN_CONPROFILE_4,
      *            LNFILE.LN_CONPROFILE_5,
      *            LNFILE.LN_RECPROFILE_1,
      *            LNFILE.LN_RECPROFILE_2,
      *            LNFILE.LN_RECPROFILE_3,
      *            LNFILE.LN_RECPROFILE_4,
      *            LNFILE.LN_RECPROFILE_5,
      *            ISNULL(LNFILE.LN_TRW_STATUS,''),
      *            ISNULL(LNFILE.LN_ORIGIN,''),
      *            ISNULL(LNFILE.LN_PO_TRCD,''),
      *            LNFILE.LN_NEXIS_CRSCORE,
      *            ISNULL(LNFILE.LN_TRW_B_CONSUMER_IND,''),
      *            ISNULL(LNFILE.LN_PARVHELD_PAID_FG,''),
      *            ISNULL(LNFILE.LN_ACCT_FROZEN,''),
      *            ISNULL(LNFILE.LN_TRW_SPECIAL_CODE,''),
      *            ISNULL(LNFILE.LN_CREATION_STATUS,''),
      *            LNFILE.LN_TOTNOLCHG,
      *            ISNULL(LNFILE.LN_LBOX,''),
      *            LNFILE.LN_EXTRA_PRIN_LFP,
      *            LNFILE.LN_REMAIN_INT_SINCE_LFP,
      *            LNFILE.LN_REMAIN_PRIN_SINCE_LFP,
      *            ISNULL(LNFILE.LN_INTERIM_COUPON,''),
      *            LNFILE.LN_DLCOMM_O3_CMSI,
      *            LNFILE.LN_DLCOMM_PP_CMSI,
      *            ISNULL(LNFILE.LN_TRW_J_CONSUMER_IND,''),
      *            ISNULL(LNFILE.LN_ACH,''),
      *            ISNULL(LNFILE.LN_1099_C_FG,''),
      *            ISNULL(LNFILE.LN_TRW_COMPLIANCE_CODE,''),
      *            ISNULL(LNFILE.LN_UNWIND_REASON_CODE,''),
      *            ISNULL(LNFILE.LN_PBI_PBD_FG,''),
      *            ISNULL(LNFILE.LN_OLD_ACCT_FORMAT_FG,''),
      *            ISNULL(LNFILE.LN_APPROVED_BY_USER,''),
      *            ISNULL(LNFILE.LN_APPROVED_BY_CODE,''),
      *            ISNULL(CAST(LNFILE.LN_APPROVED_BY_DATE
      *                                           AS VARCHAR(10)),'0'),
      *            LNFILE.LN_APPROVED_BY_TIME,
      *            ISNULL(CAST(LNFILE.LN_LTOUCH_DATE
      *                                           AS VARCHAR(10)),'0'),
      *            LNFILE.LN_LTOUCH_TIME
      *     FROM DBO.LNFILE
      *     WHERE LNFILE.LN_OWNBR = :QLN3-WBEG-OWNBR
      *       AND LNFILE.LN_CLASS > :QLN3-WBEG-CLASS
      *       AND LNFILE.LN_CLASS <= :QLN3-WEND-CLASS
      *       AND LNFILE.LN_ACCTNO > :QLN3-WBEG-ACCTNO
      *       AND LNFILE.LN_ACCTNO <= :QLN3-WEND-ACCTNO 
      *     ORDER BY LNFILE.LN_OWNBR,
      *              LNFILE.LN_CLASS,
      *              LNFILE.LN_ACCTNO
      *    END-EXEC.

      *    EXEC SQL
      *        OPEN LN3_CURSOR_GR
      *    END-EXEC.

      *    PERFORM SQL-IO-VALIDATION.

      *    MOVE ZEROES TO QLN3-WBEG-OWNBR
      *                   QLN3-WEND-OWNBR
      *                   QLN3-WBEG-CLASS
      *                   QLN3-WEND-CLASS
      *                   QLN3-WBEG-ACCTNO
      *                   QLN3-WEND-ACCTNO.

      *    MOVE STAT---GOOD TO LN3-CURSOR-GR-STAT.

      *-----------------------------------------------------------------
       READ-LN3-FILE.
           PERFORM READ-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-SELECT-PATH TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING LN-OWNBR, LN-CLASS,LN-ACCTNO
              DELIMITED BY " " INTO E-KEYX.

           MOVE LN-OWNBR  TO QLN-OWNBR.
           MOVE LN-CLASS  TO QLN-CLASS.
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
            INTO :QLN-REC
            FROM DBO.LNFILE
            WHERE LNFILE.LN_OWNBR  = :QLN-OWNBR
              AND LNFILE.LN_CLASS  = :QLN-CLASS
              AND LNFILE.LN_ACCTNO = :QLN-ACCTNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LN3-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-LN-FIELDS.

      *-----------------------------------------------------------------
       READ-LN3-FILE-NEXT.
           PERFORM READ-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-SELECT-PATH TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING LN-OWNBR, LN-CLASS,LN-ACCTNO
              DELIMITED BY " " INTO E-KEYX.
           INITIALIZE QLN-REC.

           IF ( LN3-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH LN3_CURSOR INTO 
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
                     :QLN-SPRCLASS,
                     :QLN-PURCD, 
                     :QLN-SRCD, 
                     :QLN-COLLCD, 
                     :QLN-POCD, 
                     :QLN-CONFIDENTCD, 
                     :QLN-LOANTYPE, 
                     :QLN-LOANDATE, 
                     :QLN-POFFDATE, 
                     :QLN-APRATE,
                     :QLN-LNAMT,
                     :QLN-FINCHG,
                     :QLN-INTCHG,
                     :QLN-SERCHG,
                     :QLN-EXTCHG,
                     :QLN-MAINTFEE,
                     :QLN-PROCEEDS,
                     :QLN-ORGTERM,
                     :QLN-1STPYAMT,
                     :QLN-NOREGPY,
                     :QLN-REGPYAMT,
                     :QLN-LASTPYAMT,
                     :QLN-PROCASH,
                     :QLN-TILLNO,
                     :QLN-STATE-WORKER,
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
                     :QLN-INSTYPES,
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
                     :QLN-ANTICADJ-1,
                     :QLN-ANTICADJ-2,
                     :QLN-ANTICADJ-3,
                     :QLN-ANTICADJ-4,
                     :QLN-ANTICADJ-5,
                     :QLN-ANTICADJ-6,
                     :QLN-ANTICADJ-7,
                     :QLN-ANTICADJ-8,
                     :QLN-ANTICADJ-9,
                     :QLN-ANTICADJ-10,
                     :QLN-ACCERN-1,
                     :QLN-ACCERN-2,
                     :QLN-ACCERN-3,
                     :QLN-ACCERN-4,
                     :QLN-ACCERN-5,
                     :QLN-ACCERN-6,
                     :QLN-ACCERN-7,
                     :QLN-ACCERN-8,
                     :QLN-ACCERN-9,
                     :QLN-ACCERN-10,
                     :QLN-CREDITLIM,
                     :QLN-PRLNNO,
                     :QLN-PRBAL,
                     :QLN-PRINTDUE,
                     :QLN-PRFINCHGRB,
                     :QLN-PRINSRB,
                     :QLN-PRLNAMT,
                     :QLN-PRPDUE-DAYS,
                     :QLN-BR-TFR-FG
              END-EXEC
      *    ELSE
      *    IF ( LN3-CURSOR-GR-STAT-GOOD )
      *       EXEC SQL
      *            FETCH LN3_CURSOR_GR INTO :QLN-REC
      *       END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LN3-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-LN-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-LN3-FILE.

           PERFORM CLOSE-IT.
           IF ( LN3-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE LN3_CURSOR
              END-EXEC
              MOVE STAT---BAD TO LN3-CURSOR-STAT.

      *    IF ( LN3-CURSOR-GR-STAT-GOOD )
      *       EXEC SQL
      *            CLOSE LN3_CURSOR_GR
      *       END-EXEC
      *       MOVE STAT---BAD TO LN3-CURSOR-GR-STAT.

	  
	  
      * COPY "LIBGB/GBBRRN.CPY".
      * COPYMEMBER: LIBGB/GBBRRN
      * 20220923 BAH ADDED BR_VERITEC_LICENSE #1570
      * 20230419 BAH ADDED BR-TEST-PP-AUTO-INSCOVR #1594A
       LOAD-BR-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( BR-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( BR-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO BR-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO BR-ALL-MACHINE
              MOVE BR-ALL-PATH             TO BR-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( BR-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO BR-ALL-BASE
              MOVE FILPATH-MACHINE         TO BR-ALL-MACHINE
              MOVE BR-ALL-PATH             TO BR-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-BRFILE NOT = "S" )
              MOVE BR-PATH-BEG             TO BR-SELECT-PATH-BEG
              MOVE BR-PATH-FILE            TO BR-SELECT-PATH-FILE
           ELSE
              MOVE BR-PATH-SQL             TO BR-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-BR-FILE.
           PERFORM LOAD-BR-FILE.
           PERFORM OPEN-IT.
           MOVE BR-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-BR-FILE.
           PERFORM START-IT.
           MOVE BR-SELECT-PATH TO E-FILE.
           MOVE BR-NO TO E-KEYX.

           IF ( BR-CURSOR-STAT-GOOD )
              PERFORM CLOSE-BR-FILE.

           IF ( QBR-WEND-NO = ZEROES    ) OR
              ( QBR-WEND-NO NOT NUMERIC )
              MOVE BR-NO                TO QBR-WBEG-NO
              MOVE ALL "9"              TO QBR-WEND-NO.

           EXEC SQL
            DECLARE BR_CURSOR CURSOR FOR
            SELECT BRFILE.BR_NO,
                   BRFILE.BR_NAME, 
                   BRFILE.BR_STATE, 
                   BRFILE.BR_DATA_PATH, 
                   BRFILE.BR_MACHINE,
                   BRFILE.BR_FORCE_VALID_CITY_COUNTY
            FROM DBO.BRFILE
            WHERE BRFILE.BR_NO >= :QBR-WBEG-NO
              AND BRFILE.BR_NO <= :QBR-WEND-NO
            ORDER BY BR_NO
           END-EXEC.

           EXEC SQL
               OPEN BR_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QBR-WBEG-NO
                          QBR-WEND-NO.

           MOVE STAT---GOOD TO BR-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-BR-FILE.
           PERFORM READ-IT.
           MOVE BR-SELECT-PATH TO E-FILE.
           MOVE BR-NO TO E-KEYX.

           MOVE BR-NO TO QBR-NO.

           EXEC SQL
            SELECT BRFILE.BR_NO,
                   BRFILE.BR_NAME, 
                   BRFILE.BR_STATE, 
                   BRFILE.BR_DATA_PATH, 
                   BRFILE.BR_MACHINE,
                   BRFILE.BR_FORCE_VALID_CITY_COUNTY
              INTO  
                  :QBR-NO,
                  :QBR-NAME, 
                  :QBR-STATE, 
                  :QBR-DATA-PATH, 
                  :QBR-MACHINE,
                  :QBR-FORCE-VALID-CITY-COUNTY
            FROM DBO.BRFILE
            WHERE BRFILE.BR_NO = :QBR-NO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO READ-BR-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-BR-FIELDS.

      *-----------------------------------------------------------------
       READ-BR-FILE-NEXT.
           PERFORM READ-IT.
           MOVE BR-SELECT-PATH TO E-FILE.
           MOVE BR-NO TO E-KEYX.
           INITIALIZE QBR-REC.

           IF ( BR-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH BR_CURSOR INTO  
                  :QBR-NO,
                  :QBR-NAME, 
                  :QBR-STATE, 
                  :QBR-DATA-PATH, 
                  :QBR-MACHINE,
                  :QBR-FORCE-VALID-CITY-COUNTY
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO READ-BR-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-BR-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-BR-FILE.
           PERFORM CLOSE-IT.
           IF ( BR-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE BR_CURSOR
              END-EXEC
              MOVE STAT---BAD TO BR-CURSOR-STAT.
 


       COPY "LIBGB/GBGR1RN.CPY".


      * COPY "LIBLP/LPLC1RN.CPY".
      * COPYMEMBER: LIBLP/LPLC1RN
      *-----------------------------------------------------------------
      *    STILL NEED TO LOAD THE FILE PATH WORKERS
      *-----------------------------------------------------------------
       LOAD-LC1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( LC-PATH-OVERRIDE = "Y" )
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( LC-PATH-OVERRIDE = SPACES )
              MOVE EXT-FILPATH-BASE        TO LC-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO LC-ALL-MACHINE
              MOVE LC-ALL-PATH             TO LC-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( LC-PATH-OVERRIDE = "B" )
              MOVE FILPATH-BASE            TO LC-ALL-BASE
              MOVE FILPATH-MACHINE         TO LC-ALL-MACHINE
              MOVE LC-ALL-PATH             TO LC-PATH.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-LCFILE NOT = "S" )
              MOVE LC-PATH-BEG             TO LC-SELECT-PATH-BEG
              MOVE LC-PATH-FILE            TO LC-SELECT-PATH-FILE
           ELSE
              MOVE LC-PATH-SQL             TO LC-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-LC1-FILE.
           PERFORM LOAD-LC1-FILE.
           PERFORM OPEN-IT.
           MOVE LC-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-LC1-FILE.
           PERFORM START-IT.
           MOVE LC-SELECT-PATH TO E-FILE.
           MOVE LC1-KEY TO E-KEYX.

           IF ( LC1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-LC1-FILE.

           IF ( QLC1-WEND-CODE = ZEROES    ) OR
              ( QLC1-WEND-CODE NOT NUMERIC )
              MOVE LC-CODE              TO QLC1-WBEG-CODE
              MOVE ALL "9"              TO QLC1-WEND-CODE.

           EXEC SQL
            DECLARE LC1_CURSOR CURSOR FOR
            SELECT LCFILE.LC_CODE,
                   ISNULL(CAST(LCFILE.LC_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(LCFILE.LC_DESC,''),
                   ISNULL(LCFILE.LC_SMALLOAN,''),
                   ISNULL(LCFILE.LC_SALEFIN,''),
                   ISNULL(LCFILE.LC_SERVACCT,''),
                   ISNULL(LCFILE.LC_LOANTYPE,''),
                   ISNULL(LCFILE.LC_PROF1,''),
                   ISNULL(LCFILE.LC_PROF2,''),
                   ISNULL(LCFILE.LC_SKIP_TRW,''),
                   ISNULL(LCFILE.LC_DISPLAY_ERN,''),
                   ISNULL(LCFILE.LC_LCARDFG,''),
                   ISNULL(LCFILE.LC_CALC_HI_CREDIT,''),
                   ISNULL(LCFILE.LC_STOP_ON_DLNO,''),
                   ISNULL(LCFILE.LC_SALES_FORCED_ENTRY,''),
                   ISNULL(LCFILE.LC_AFP_CLASS,''),
                   ISNULL(LCFILE.LC_CON_PDUE_DAYS,''),
                   ISNULL(LCFILE.LC_REC_PDUE_DAYS,''),
                   ISNULL(LCFILE.LC_GIFTNET,''),
                   ISNULL(LCFILE.LC_APP_REQ_FOR_LOAN,''),
                   LCFILE.LC_POOLID,
                   ISNULL(LCFILE.LC_USE_ALT_GL_FEE,''),
                   LCFILE.LC_PROGRP_CLASS,
                   ISNULL(LCFILE.LC_ALLOW_CASH_PROCEEDS,'')
            FROM DBO.LCFILE
            WHERE LCFILE.LC_CODE >= :QLC1-WBEG-CODE
              AND LCFILE.LC_CODE <= :QLC1-WEND-CODE
            ORDER BY LCFILE.LC_CODE
           END-EXEC.

           EXEC SQL
               OPEN LC1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QLC1-WBEG-CODE 
                          QLC1-WEND-CODE.

           MOVE STAT---GOOD TO LC1-CURSOR-STAT.

      *-----------------------------------------------------------------
       START-LC1-FILE-GREATER.
           PERFORM START-IT.
           MOVE LC-SELECT-PATH TO E-FILE.
           MOVE LC1-KEY TO E-KEYX.

           IF ( LC1-CURSOR-GR-STAT-GOOD )
              PERFORM CLOSE-LC1-FILE.

           IF ( QLC1-WEND-CODE = ZEROES    ) OR
              ( QLC1-WEND-CODE NOT NUMERIC )
              MOVE LC-CODE              TO QLC1-WBEG-CODE
              MOVE ALL "9"              TO QLC1-WEND-CODE.

           EXEC SQL
            DECLARE LC1_CURSOR_GR CURSOR FOR
            SELECT LCFILE.LC_CODE,
                   ISNULL(CAST(LCFILE.LC_LTOUCH_DATE
                                                   AS VARCHAR(10)),'0'),
                   ISNULL(LCFILE.LC_DESC,''),
                   ISNULL(LCFILE.LC_SMALLOAN,''),
                   ISNULL(LCFILE.LC_SALEFIN,''),
                   ISNULL(LCFILE.LC_SERVACCT,''),
                   ISNULL(LCFILE.LC_LOANTYPE,''),
                   ISNULL(LCFILE.LC_PROF1,''),
                   ISNULL(LCFILE.LC_PROF2,''),
                   ISNULL(LCFILE.LC_SKIP_TRW,''),
                   ISNULL(LCFILE.LC_DISPLAY_ERN,''),
                   ISNULL(LCFILE.LC_LCARDFG,''),
                   ISNULL(LCFILE.LC_CALC_HI_CREDIT,''),
                   ISNULL(LCFILE.LC_STOP_ON_DLNO,''),
                   ISNULL(LCFILE.LC_SALES_FORCED_ENTRY,''),
                   ISNULL(LCFILE.LC_AFP_CLASS,''),
                   ISNULL(LCFILE.LC_CON_PDUE_DAYS,''),
                   ISNULL(LCFILE.LC_REC_PDUE_DAYS,''),
                   ISNULL(LCFILE.LC_GIFTNET,''),
                   ISNULL(LCFILE.LC_APP_REQ_FOR_LOAN,''),
                   LCFILE.LC_POOLID,
                   ISNULL(LCFILE.LC_USE_ALT_GL_FEE,''),
                   LCFILE.LC_PROGRP_CLASS,
                   ISNULL(LCFILE.LC_ALLOW_CASH_PROCEEDS,'')
            FROM DBO.LCFILE
            WHERE LCFILE.LC_CODE >  :QLC1-WBEG-CODE
              AND LCFILE.LC_CODE <= :QLC1-WEND-CODE
            ORDER BY LCFILE.LC_CODE
           END-EXEC.

           EXEC SQL
               OPEN LC1_CURSOR_GR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QLC1-WBEG-CODE 
                          QLC1-WEND-CODE.

           MOVE STAT---GOOD TO LC1-CURSOR-GR-STAT.

      *-----------------------------------------------------------------
       READ-LC1-FILE.
           PERFORM READ-IT.
           MOVE LC-SELECT-PATH TO E-FILE.
           MOVE LC1-KEY TO E-KEYX.

           MOVE LC-CODE TO QLC-CODE.

           EXEC SQL
            SELECT LCFILE.LC_CODE,
                   LCFILE.LC_DESC 
              INTO 
                   :QLC-CODE,
                   :QLC-DESC 
              FROM DBO.LCFILE
             WHERE LCFILE.LC_CODE = :QLC-CODE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO READ-LC1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-LC-FIELDS.

      *-----------------------------------------------------------------
      *    STILL LOADING FILE WORKERS LIKE E-WORKERS
      *-----------------------------------------------------------------
       READ-LC1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE LC-SELECT-PATH TO E-FILE.
           MOVE LC1-KEY TO E-KEYX.
           INITIALIZE QLC-REC.

           IF ( LC1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH LC1_CURSOR INTO :QLC-REC
              END-EXEC
           ELSE
           IF ( LC1-CURSOR-GR-STAT-GOOD )
              EXEC SQL
                   FETCH LC1_CURSOR_GR INTO :QLC-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO READ-LC1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-LC-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-LC1-FILE.

           PERFORM CLOSE-IT.
           IF ( LC1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE LC1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO LC1-CURSOR-STAT.

           IF ( LC1-CURSOR-GR-STAT-GOOD )
              EXEC SQL
                   CLOSE LC1_CURSOR_GR
              END-EXEC
              MOVE STAT---BAD TO LC1-CURSOR-GR-STAT.


      * COPY "LIBLP/LPLP1RN.CPY".
      * COPYMEMBER: LIBLP/LPLP1RN
       LOAD-LP1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( LP-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( LP-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO LP-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO LP-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO LP-ALL-DATA
              MOVE LP-ALL-PATH             TO LP-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( LP-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO LP-ALL-BASE
              MOVE FILPATH-MACHINE         TO LP-ALL-MACHINE
              MOVE FILPATH-DATA            TO LP-ALL-DATA
              MOVE LP-ALL-PATH             TO LP-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-LPFILE NOT = "S" )
              MOVE LP-PATH-BEG             TO LP-SELECT-PATH-BEG
              MOVE LP-PATH-FILE            TO LP-SELECT-PATH-FILE
           ELSE
              MOVE LP-PATH-SQL             TO LP-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-LP1-FILE.
           PERFORM LOAD-LP1-FILE.
           PERFORM OPEN-IT.
           MOVE LP-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
      * ***NOTE*** THIS 'START' IS NOT FOR DISPLAYING MULTIPLE PAGES.
      * ONLY SHOULD BE USED ONCE PER PROGRAM/BRANCH/ACCOUNT LIKE IN
      * A STANDARD REPORT.  USE START-LP1-FILE-SCAN FOR DISPLAYING
      * MULTIPLE PAGES.
      *-----------------------------------------------------------------
       START-LP1-FILE.
           PERFORM START-IT.
           MOVE LP-PATH-OWNBR  TO LP-BRNO.
           MOVE LP-SELECT-PATH TO E-FILE.
           MOVE LP-PATH-OWNBR  TO LP-BRNO.

           IF ( LP1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-LP1-FILE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  DEFAULT THE WBEG AND WEND; TO GET A START TO WORK IN SQL LIKE
      *  IT DOES IN VISION FILES FOR MULTIPLE SCAN PAGES, NEED TO ADD
      *  ONE TO THE WHERE SCAN BEG (WSBEG).
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( QLP1-WEND-SEQNO = ZEROES    ) OR
              ( QLP1-WEND-SEQNO NOT NUMERIC )
              MOVE LP-PATH-OWNBR        TO QLP1-WBEG-BRNO
              MOVE LP-PATH-OWNBR        TO QLP1-WEND-BRNO
              MOVE LP-ACCTNO            TO QLP1-WBEG-ACCTNO
              MOVE ALL "9"              TO QLP1-WEND-ACCTNO
              MOVE LP-SEQNO             TO QLP1-WBEG-SEQNO
              MOVE ALL "9"              TO QLP1-WEND-SEQNO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           EXEC SQL
            DECLARE LP1_CURSOR CURSOR FOR
            SELECT LPFILE.LP_BRNO,
                   LPFILE.LP_ACCTNO,
                   LPFILE.LP_SEQNO,
                   ISNULL(CAST(LPFILE.LP_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(LPFILE.LP_USERID,''),
                   ISNULL(CAST(LPFILE.LP_POSTDATE AS VARCHAR(10)),'0'),
                   LPFILE.LP_POSTTIME,
                   ISNULL(CAST(LPFILE.LP_TRDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LPFILE.LP_PAYDATE AS VARCHAR(10)),'0'),
                   ISNULL(LPFILE.LP_TRCD,''),
                   ISNULL(LPFILE.LP_REFNO,''),
                   LPFILE.LP_TRAMT,
                   LPFILE.LP_APOTH,
                   LPFILE.LP_APOT2,
                   LPFILE.LP_APINT,
                   LPFILE.LP_APLC,
                   LPFILE.LP_APCUR,
                   LPFILE.LP_OTHBAL,
                   LPFILE.LP_OT2BAL,
                   LPFILE.LP_INTBAL,
                   LPFILE.LP_LCBAL,
                   LPFILE.LP_CURBAL,
                   LPFILE.LP_INTDUE,
                   LPFILE.LP_DLPROC,
                   LPFILE.LP_EARNED_1,
                   LPFILE.LP_EARNED_2,
                   LPFILE.LP_EARNED_3,
                   LPFILE.LP_INTPAID,
                   LPFILE.LP_SSNO,
                   ISNULL(CAST(LPFILE.LP_PDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LPFILE.LP_LCPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LPFILE.LP_INTPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   LPFILE.LP_TILLNO,
                   LPFILE.LP_APINTOWE,
                   LPFILE.LP_DLPARVHELD_ADJ,
                   LPFILE.LP_LT_SEQNO,
                   ISNULL(LPFILE.LP_IBPC,''),
                   ISNULL(LPFILE.LP_MANUAL_RB,''),
                   ISNULL(LPFILE.LP_INSTYPES,''),
                   ISNULL(LPFILE.LP_REV,''),
                   ISNULL(LPFILE.LP_REVTRCD,''),
                   LPFILE.LP_REVSEQNO,
                   ISNULL(LPFILE.LP_STATUSFG,''),
                   LPFILE.LP_NEWSTAT_RATE,
                   LPFILE.LP_LCPARTIALS,
                   LPFILE.LP_ADDON_ORIG_REGPY,
                   ISNULL(CAST(LPFILE.LP_NEW1ST_PYDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LPFILE.LP_DEF_DUEDATE
                                                  AS VARCHAR(10)),'0'),
                   LPFILE.LP_LCPAID,
                   ISNULL(CAST(LPFILE.LP_OLD1ST_PYDATE
                                                  AS VARCHAR(10)),'0'),
                   LPFILE.LP_DEF_TERM,
                   LPFILE.LP_ADDON_LAST_PYAMT,
                   LPFILE.LP_DLRVCHGS_ADJ_1,
                   LPFILE.LP_DLRVCHGS_ADJ_2,
                   LPFILE.LP_DLRVCHGS_ADJ_3,
                   LPFILE.LP_DLPARVPAID_ADJ,
                   LPFILE.LP_DLNO,
                   LPFILE.LP_PREPAY_PENALTY,
                   LPFILE.LP_PRORATED_INS_REB,
                   LPFILE.LP_POSTING_BR,
                   LPFILE.LP_NOLCHG,
                   ISNULL(LPFILE.LP_COLLECTOR,''),
                   LPFILE.LP_EXTRA_PRIN_LFP,
                   LPFILE.LP_REMAIN_INT_SINCE_LFP,
                   LPFILE.LP_REMAIN_PRIN_SINCE_LFP,
                   LPFILE.LP_APPLIED_TO_ESCROW,
                   LPFILE.LP_ESCROW_PREPAID,
                   LPFILE.LP_PRIOR_POOLID,
                   ISNULL(CAST(LPFILE.LP_ADDON_INTPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   LPFILE.LP_FROMCL,
                   LPFILE.LP_TOCL,
                   ISNULL(LPFILE.LP_ALT_PREPAY_FG,'')
            FROM DBO.LPFILE
            WHERE ( LPFILE.LP_BRNO   >= :QLP1-WBEG-BRNO
              AND   LPFILE.LP_ACCTNO >= :QLP1-WBEG-ACCTNO
              AND   LPFILE.LP_SEQNO  >= :QLP1-WBEG-SEQNO )
              AND ( LPFILE.LP_BRNO   <= :QLP1-WEND-BRNO
              AND   LPFILE.LP_ACCTNO <= :QLP1-WEND-ACCTNO
              AND   LPFILE.LP_SEQNO  <= :QLP1-WEND-SEQNO )
            ORDER BY LPFILE.LP_BRNO,
                     LPFILE.LP_ACCTNO,
                     LPFILE.LP_SEQNO
           END-EXEC.

           EXEC SQL
               OPEN LP1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QLP1-WBEG-BRNO 
                          QLP1-WEND-BRNO
                          QLP1-WBEG-ACCTNO
                          QLP1-WEND-ACCTNO
                          QLP1-WBEG-SEQNO
                          QLP1-WEND-SEQNO.

           MOVE STAT---GOOD TO LP1-CURSOR-STAT.

      *-----------------------------------------------------------------
      * ***NOTE*** THIS 'START' IS FOR USE IN DISPLAYING MULTIPLE PAGES.
      * WHEN GOING UP & DOWN DISPLAYING PAGES, NEED TO USE THIS 'START'.
      * IF FIRST USED A START-LP1-FILE-NOT-GREATER, YOU NEED TO ADD
      * ONE TO LP-SEQNO IN ORDER TO GET THE DISPLAY TO WORK
      * LIKE VISION FILES.
      *-----------------------------------------------------------------
       START-LP1-FILE-SCAN.

           PERFORM START-IT.
           MOVE LP-PATH-OWNBR  TO LP-BRNO.
           MOVE LP-SELECT-PATH TO E-FILE.
           MOVE LP-PATH-OWNBR  TO LP-BRNO.

           IF ( LP1-CURSOR-SC-STAT-GOOD )
              PERFORM CLOSE-LP1-FILE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  DEFAULT THE WBEG AND WEND; TO GET A START TO WORK IN SQL LIKE
      *  IT DOES IN VISION FILES FOR MULTIPLE SCAN PAGES, NEED TO ADD
      *  ONE TO THE WHERE SCAN BEG (WSBEG).
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( QLP1-WEND-SEQNO = ZEROES    ) OR
              ( QLP1-WEND-SEQNO NOT NUMERIC )
              MOVE LP-PATH-OWNBR        TO QLP1-WBEG-BRNO
              MOVE LP-PATH-OWNBR        TO QLP1-WEND-BRNO
              MOVE LP-ACCTNO            TO QLP1-WBEG-ACCTNO
              MOVE ALL "9"              TO QLP1-WEND-ACCTNO
              MOVE LP-SEQNO             TO QLP1-WBEG-SEQNO
              MOVE ALL "9"              TO QLP1-WEND-SEQNO
      *-   -   -{ ADD ONE TO WHERE SCAN BEG FIELDS }
              IF ( LP-BRNO = 9999 )
                 MOVE LP-BRNO           TO QLP1-WSBEG-BRNO
              ELSE
                 COMPUTE QLP1-WSBEG-BRNO = LP-BRNO + 1
              END-IF

              IF ( LP-ACCTNO = 99999999 )
                 MOVE LP-ACCTNO         TO QLP1-WSBEG-ACCTNO
              ELSE
                 COMPUTE QLP1-WSBEG-ACCTNO = LP-ACCTNO + 1.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           EXEC SQL
            DECLARE LP1_CURSOR_SC CURSOR FOR
            SELECT LPFILE.LP_BRNO,
                   LPFILE.LP_ACCTNO,
                   LPFILE.LP_SEQNO,
                   ISNULL(CAST(LPFILE.LP_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(LPFILE.LP_USERID,''),
                   ISNULL(CAST(LPFILE.LP_POSTDATE AS VARCHAR(10)),'0'),
                   LPFILE.LP_POSTTIME,
                   ISNULL(CAST(LPFILE.LP_TRDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LPFILE.LP_PAYDATE AS VARCHAR(10)),'0'),
                   ISNULL(LPFILE.LP_TRCD,''),
                   ISNULL(LPFILE.LP_REFNO,''),
                   LPFILE.LP_TRAMT,
                   LPFILE.LP_APOTH,
                   LPFILE.LP_APOT2,
                   LPFILE.LP_APINT,
                   LPFILE.LP_APLC,
                   LPFILE.LP_APCUR,
                   LPFILE.LP_OTHBAL,
                   LPFILE.LP_OT2BAL,
                   LPFILE.LP_INTBAL,
                   LPFILE.LP_LCBAL,
                   LPFILE.LP_CURBAL,
                   LPFILE.LP_INTDUE,
                   LPFILE.LP_DLPROC,
                   LPFILE.LP_EARNED_1,
                   LPFILE.LP_EARNED_2,
                   LPFILE.LP_EARNED_3,
                   LPFILE.LP_INTPAID,
                   LPFILE.LP_SSNO,
                   ISNULL(CAST(LPFILE.LP_PDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LPFILE.LP_LCPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LPFILE.LP_INTPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   LPFILE.LP_TILLNO,
                   LPFILE.LP_APINTOWE,
                   LPFILE.LP_DLPARVHELD_ADJ,
                   LPFILE.LP_LT_SEQNO,
                   ISNULL(LPFILE.LP_IBPC,''),
                   ISNULL(LPFILE.LP_MANUAL_RB,''),
                   ISNULL(LPFILE.LP_INSTYPES,''),
                   ISNULL(LPFILE.LP_REV,''),
                   ISNULL(LPFILE.LP_REVTRCD,''),
                   LPFILE.LP_REVSEQNO,
                   ISNULL(LPFILE.LP_STATUSFG,''),
                   LPFILE.LP_NEWSTAT_RATE,
                   LPFILE.LP_LCPARTIALS,
                   LPFILE.LP_ADDON_ORIG_REGPY,
                   ISNULL(CAST(LPFILE.LP_NEW1ST_PYDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LPFILE.LP_DEF_DUEDATE
                                                  AS VARCHAR(10)),'0'),
                   LPFILE.LP_LCPAID,
                   ISNULL(CAST(LPFILE.LP_OLD1ST_PYDATE
                                                  AS VARCHAR(10)),'0'),
                   LPFILE.LP_DEF_TERM,
                   LPFILE.LP_ADDON_LAST_PYAMT,
                   LPFILE.LP_DLRVCHGS_ADJ_1,
                   LPFILE.LP_DLRVCHGS_ADJ_2,
                   LPFILE.LP_DLRVCHGS_ADJ_3,
                   LPFILE.LP_DLPARVPAID_ADJ,
                   LPFILE.LP_DLNO,
                   LPFILE.LP_PREPAY_PENALTY,
                   LPFILE.LP_PRORATED_INS_REB,
                   LPFILE.LP_POSTING_BR,
                   LPFILE.LP_NOLCHG,
                   ISNULL(LPFILE.LP_COLLECTOR,''),
                   LPFILE.LP_EXTRA_PRIN_LFP,
                   LPFILE.LP_REMAIN_INT_SINCE_LFP,
                   LPFILE.LP_REMAIN_PRIN_SINCE_LFP,
                   LPFILE.LP_APPLIED_TO_ESCROW,
                   LPFILE.LP_ESCROW_PREPAID,
                   LPFILE.LP_PRIOR_POOLID,
                   ISNULL(CAST(LPFILE.LP_ADDON_INTPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   LPFILE.LP_FROMCL,
                   LPFILE.LP_TOCL,
                   ISNULL(LPFILE.LP_ALT_PREPAY_FG,'')
            FROM DBO.LPFILE
            WHERE (( LPFILE.LP_BRNO       >= :QLP1-WBEG-BRNO
              AND    LPFILE.LP_ACCTNO     >= :QLP1-WBEG-ACCTNO
              AND    LPFILE.LP_SEQNO      >= :QLP1-WBEG-SEQNO )
               OR  ( LPFILE.LP_BRNO       >= :QLP1-WBEG-BRNO
              AND    LPFILE.LP_ACCTNO     >= :QLP1-WSBEG-ACCTNO
              AND    LPFILE.LP_SEQNO      >= 0 )
               OR  ( LPFILE.LP_BRNO       >= :QLP1-WSBEG-BRNO
              AND    LPFILE.LP_ACCTNO     >= 0
              AND    LPFILE.LP_SEQNO      >= 0 ))
              AND (  LPFILE.LP_BRNO       <= :QLP1-WEND-BRNO
              AND    LPFILE.LP_ACCTNO     <= :QLP1-WEND-ACCTNO
              AND    LPFILE.LP_SEQNO      <= :QLP1-WEND-SEQNO )
            ORDER BY LPFILE.LP_BRNO,
                     LPFILE.LP_ACCTNO,
                     LPFILE.LP_SEQNO
           END-EXEC.

           EXEC SQL
               OPEN LP1_CURSOR_SC
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QLP1-WBEG-BRNO 
                          QLP1-WEND-BRNO
                          QLP1-WBEG-ACCTNO
                          QLP1-WEND-ACCTNO
                          QLP1-WBEG-SEQNO
                          QLP1-WEND-SEQNO
                          QLP1-WSBEG-BRNO
                          QLP1-WSBEG-ACCTNO.

           MOVE STAT---GOOD TO LP1-CURSOR-SC-STAT.

      *-----------------------------------------------------------------
      *  ***NOTE*** ONLY USE START-LP1-FILE-SCAN WHEN USING A 
      *  START-LP1-FILE-NOT-GREATER.  THIS IS FOR DISPLAYING MULTIPLE
      *  PAGE IN A UP & DOWN SCENARIO.
      *-----------------------------------------------------------------
       START-LP1-FILE-NOT-GREATER.
           PERFORM START-IT.
           MOVE LP-PATH-OWNBR  TO LP-BRNO.
           MOVE LP-SELECT-PATH TO E-FILE.
           MOVE LP1-KEY        TO E-KEYX.

           IF ( LP1-CURSOR-NG-STAT-GOOD )
              PERFORM CLOSE-LP1-FILE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  DEFAULT THE WBEG AND WEND
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( QLP1-WEND-SEQNO = ZEROES    ) OR
              ( QLP1-WEND-SEQNO NOT NUMERIC )
              MOVE LP-PATH-OWNBR        TO QLP1-WBEG-BRNO
              MOVE LP-PATH-OWNBR        TO QLP1-WEND-BRNO
              MOVE LP-ACCTNO            TO QLP1-WBEG-ACCTNO
              MOVE LP-ACCTNO            TO QLP1-WEND-ACCTNO
              MOVE ALL "0"              TO QLP1-WBEG-SEQNO
              MOVE LP-SEQNO             TO QLP1-WEND-SEQNO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           EXEC SQL
            DECLARE LP1_CURSOR_NG CURSOR FOR
            SELECT LPFILE.LP_BRNO,
                   LPFILE.LP_ACCTNO,
                   LPFILE.LP_SEQNO,
                   ISNULL(CAST(LPFILE.LP_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(LPFILE.LP_USERID,''),
                   ISNULL(CAST(LPFILE.LP_POSTDATE AS VARCHAR(10)),'0'),
                   LPFILE.LP_POSTTIME,
                   ISNULL(CAST(LPFILE.LP_TRDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LPFILE.LP_PAYDATE AS VARCHAR(10)),'0'),
                   ISNULL(LPFILE.LP_TRCD,''),
                   ISNULL(LPFILE.LP_REFNO,''),
                   LPFILE.LP_TRAMT,
                   LPFILE.LP_APOTH,
                   LPFILE.LP_APOT2,
                   LPFILE.LP_APINT,
                   LPFILE.LP_APLC,
                   LPFILE.LP_APCUR,
                   LPFILE.LP_OTHBAL,
                   LPFILE.LP_OT2BAL,
                   LPFILE.LP_INTBAL,
                   LPFILE.LP_LCBAL,
                   LPFILE.LP_CURBAL,
                   LPFILE.LP_INTDUE,
                   LPFILE.LP_DLPROC,
                   LPFILE.LP_EARNED_1,
                   LPFILE.LP_EARNED_2,
                   LPFILE.LP_EARNED_3,
                   LPFILE.LP_INTPAID,
                   LPFILE.LP_SSNO,
                   ISNULL(CAST(LPFILE.LP_PDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LPFILE.LP_LCPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LPFILE.LP_INTPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   LPFILE.LP_TILLNO,
                   LPFILE.LP_APINTOWE,
                   LPFILE.LP_DLPARVHELD_ADJ,
                   LPFILE.LP_LT_SEQNO,
                   ISNULL(LPFILE.LP_IBPC,''),
                   ISNULL(LPFILE.LP_MANUAL_RB,''),
                   ISNULL(LPFILE.LP_INSTYPES,''),
                   ISNULL(LPFILE.LP_REV,''),
                   ISNULL(LPFILE.LP_REVTRCD,''),
                   LPFILE.LP_REVSEQNO,
                   ISNULL(LPFILE.LP_STATUSFG,''),
                   LPFILE.LP_NEWSTAT_RATE,
                   LPFILE.LP_LCPARTIALS,
                   LPFILE.LP_ADDON_ORIG_REGPY,
                   ISNULL(CAST(LPFILE.LP_NEW1ST_PYDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LPFILE.LP_DEF_DUEDATE
                                                  AS VARCHAR(10)),'0'),
                   LPFILE.LP_LCPAID,
                   ISNULL(CAST(LPFILE.LP_OLD1ST_PYDATE
                                                  AS VARCHAR(10)),'0'),
                   LPFILE.LP_DEF_TERM,
                   LPFILE.LP_ADDON_LAST_PYAMT,
                   LPFILE.LP_DLRVCHGS_ADJ_1,
                   LPFILE.LP_DLRVCHGS_ADJ_2,
                   LPFILE.LP_DLRVCHGS_ADJ_3,
                   LPFILE.LP_DLPARVPAID_ADJ,
                   LPFILE.LP_DLNO,
                   LPFILE.LP_PREPAY_PENALTY,
                   LPFILE.LP_PRORATED_INS_REB,
                   LPFILE.LP_POSTING_BR,
                   LPFILE.LP_NOLCHG,
                   ISNULL(LPFILE.LP_COLLECTOR,''),
                   LPFILE.LP_EXTRA_PRIN_LFP,
                   LPFILE.LP_REMAIN_INT_SINCE_LFP,
                   LPFILE.LP_REMAIN_PRIN_SINCE_LFP,
                   LPFILE.LP_APPLIED_TO_ESCROW,
                   LPFILE.LP_ESCROW_PREPAID,
                   LPFILE.LP_PRIOR_POOLID,
                   ISNULL(CAST(LPFILE.LP_ADDON_INTPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   LPFILE.LP_FROMCL,
                   LPFILE.LP_TOCL,
                   ISNULL(LPFILE.LP_ALT_PREPAY_FG,'')
            FROM DBO.LPFILE
            WHERE LPFILE.LP_BRNO    = :QLP1-WEND-BRNO
              AND LPFILE.LP_ACCTNO  = :QLP1-WEND-ACCTNO
              AND LPFILE.LP_SEQNO  <= :QLP1-WEND-SEQNO 
            ORDER BY LPFILE.LP_BRNO DESC,
                     LPFILE.LP_ACCTNO DESC,
                     LPFILE.LP_SEQNO DESC
           END-EXEC.

           EXEC SQL
               OPEN LP1_CURSOR_NG
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QLP1-WBEG-BRNO 
                          QLP1-WEND-BRNO
                          QLP1-WBEG-ACCTNO
                          QLP1-WEND-ACCTNO
                          QLP1-WBEG-SEQNO
                          QLP1-WEND-SEQNO.

           MOVE STAT---GOOD TO LP1-CURSOR-NG-STAT.

      *-----------------------------------------------------------------
       READ-LP1-FILE.
           PERFORM READ-IT.
           MOVE LP-PATH-OWNBR  TO LP-BRNO. 
           MOVE LP-SELECT-PATH TO E-FILE.
           MOVE LP1-KEY        TO E-KEYX.

           MOVE LP-BRNO   TO QLP-BRNO.
           MOVE LP-ACCTNO TO QLP-ACCTNO.
           MOVE LP-SEQNO  TO QLP-SEQNO.

           EXEC SQL
            SELECT LPFILE.LP_BRNO,
                   LPFILE.LP_ACCTNO,
                   LPFILE.LP_SEQNO,
                   LPFILE.LP_USERID,
                   LPFILE.LP_POSTTIME
              INTO  
                  :QLP-BRNO,
                  :QLP-ACCTNO,
                  :QLP-SEQNO,
                  :QLP-USERID,
                  :QLP-POSTTIME
              FROM DBO.LPFILE
             WHERE LPFILE.LP_BRNO = :QLP-BRNO
               AND LPFILE.LP_ACCTNO = :QLP-ACCTNO
               AND LPFILE.LP_SEQNO = :QLP-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LP1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-LP-FIELDS.

      *-----------------------------------------------------------------
       READ-LP1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE LP-PATH-OWNBR  TO LP-BRNO.
           MOVE LP-SELECT-PATH TO E-FILE.
           MOVE LP1-KEY        TO E-KEYX.
           INITIALIZE QLP-REC.

           IF ( LP1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH LP1_CURSOR INTO :QLP-REC
              END-EXEC
           ELSE
           IF ( LP1-CURSOR-SC-STAT-GOOD )
              EXEC SQL
                   FETCH LP1_CURSOR_SC INTO :QLP-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LP1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-LP-FIELDS.

      *-----------------------------------------------------------------
       READ-LP1-FILE-PREVIOUS.
           PERFORM READ-IT.
           MOVE LP-PATH-OWNBR  TO LP-BRNO.
           MOVE LP-SELECT-PATH TO E-FILE.
           MOVE LP1-KEY        TO E-KEYX.
           INITIALIZE QLP-REC.

           IF ( LP1-CURSOR-NG-STAT-GOOD )
              EXEC SQL
                   FETCH LP1_CURSOR_NG INTO :QLP-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDPVCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LP1-FILE-PREVIOUS.

           IF ( IO-FG = 0 )
              PERFORM GET-LP-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-LP1-FILE.

           PERFORM CLOSE-IT.
           IF ( LP1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE LP1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO LP1-CURSOR-STAT.

           IF ( LP1-CURSOR-NG-STAT-GOOD )
              EXEC SQL
                   CLOSE LP1_CURSOR_NG
              END-EXEC
              MOVE STAT---BAD TO LP1-CURSOR-NG-STAT.

           IF ( LP1-CURSOR-SC-STAT-GOOD )
              EXEC SQL
                   CLOSE LP1_CURSOR_SC
              END-EXEC
              MOVE STAT---BAD TO LP1-CURSOR-SC-STAT.



       COPY "LIBLP/LPSP1RN.CPY".
 
      * COPY "LIBLP/LPLTC1RN.CPY".
      * COPYMEMBER: LIBLP/LPLTC1RN
       LOAD-LTC1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( LTC-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( LTC-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO LTC-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO LTC-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO LTC-ALL-DATA
              MOVE LTC-ALL-PATH             TO LTC-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( LTC-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO LTC-ALL-BASE
              MOVE FILPATH-MACHINE         TO LTC-ALL-MACHINE
              MOVE FILPATH-DATA            TO LTC-ALL-DATA
              MOVE LTC-ALL-PATH             TO LTC-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-LTCFILE NOT = "S" )
              MOVE LTC-PATH-BEG             TO LTC-SELECT-PATH-BEG
              MOVE LTC-PATH-FILE            TO LTC-SELECT-PATH-FILE
           ELSE
              MOVE LTC-PATH-SQL             TO LTC-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-LTC1-FILE.
           PERFORM LOAD-LTC1-FILE.
           PERFORM OPEN-IT.
           MOVE LTC-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-LTC1-FILE.
           PERFORM START-IT.
           MOVE LTC-PATH-OWNBR  TO LTC-BRNO.
           MOVE LTC-SELECT-PATH TO E-FILE.
           MOVE LTC1-KEY        TO E-KEYX.

           IF ( LTC1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-LTC1-FILE.

           IF ( QLTC1-WEND-BRNO = ZEROES    ) OR
              ( QLTC1-WEND-BRNO NOT NUMERIC )
              MOVE LTC-PATH-OWNBR        TO QLTC1-WBEG-BRNO
              MOVE LTC-PATH-OWNBR        TO QLTC1-WEND-BRNO
              MOVE LTC-ACCTNO            TO QLTC1-WBEG-ACCTNO
              MOVE ALL "9"               TO QLTC1-WEND-ACCTNO
              MOVE LTC-SEQNO             TO QLTC1-WBEG-SEQNO
              MOVE ALL "9"               TO QLTC1-WEND-SEQNO.

           EXEC SQL
            DECLARE LTC1_CURSOR CURSOR FOR
            SELECT LTCFILE.LTC_BRNO,
                   LTCFILE.LTC_ACCTNO,
                   LTCFILE.LTC_SEQNO,
                   ISNULL(CAST(LTCFILE.LTC_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(LTCFILE.LTC_DESC_1,''),
                   ISNULL(LTCFILE.LTC_MAKE_1,''),
                   ISNULL(LTCFILE.LTC_MODEL_1,''),
                   ISNULL(LTCFILE.LTC_YEAR_1,''),
                   ISNULL(LTCFILE.LTC_STYLE_1,''),
                   ISNULL(LTCFILE.LTC_CLASS_1,''),
                   ISNULL(LTCFILE.LTC_VIN_1,''),
                   ISNULL(LTCFILE.LTC_TITLE_NO_1,''),
                   LTCFILE.LTC_MILEAGE_1,
                   LTCFILE.LTC_VALUE_1,
                   LTCFILE.LTC_SELLPR_1,
                   LTCFILE.LTC_MISC_AMT_1,
                   ISNULL(LTCFILE.LTC_SENT_FLAG_1,''),
                   ISNULL(CAST(LTCFILE.LTC_VIN_CHG_DATE_1
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(LTCFILE.LTC_DESC_2,''),
                   ISNULL(LTCFILE.LTC_MAKE_2,''),
                   ISNULL(LTCFILE.LTC_MODEL_2,''),
                   ISNULL(LTCFILE.LTC_YEAR_2,''),
                   ISNULL(LTCFILE.LTC_STYLE_2,''),
                   ISNULL(LTCFILE.LTC_CLASS_2,''),
                   ISNULL(LTCFILE.LTC_VIN_2,''),
                   ISNULL(LTCFILE.LTC_TITLE_NO_2,''),
                   LTCFILE.LTC_MILEAGE_2,
                   LTCFILE.LTC_VALUE_2,
                   LTCFILE.LTC_SELLPR_2,
                   LTCFILE.LTC_MISC_AMT_2,
                   ISNULL(LTCFILE.LTC_SENT_FLAG_2,''),
                   ISNULL(CAST(LTCFILE.LTC_VIN_CHG_DATE_2
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(LTCFILE.LTC_DESC_3,''),
                   ISNULL(LTCFILE.LTC_MAKE_3,''),
                   ISNULL(LTCFILE.LTC_MODEL_3,''),
                   ISNULL(LTCFILE.LTC_YEAR_3,''),
                   ISNULL(LTCFILE.LTC_STYLE_3,''),
                   ISNULL(LTCFILE.LTC_CLASS_3,''),
                   ISNULL(LTCFILE.LTC_VIN_3,''),
                   ISNULL(LTCFILE.LTC_TITLE_NO_3,''),
                   LTCFILE.LTC_MILEAGE_3,
                   LTCFILE.LTC_VALUE_3,
                   LTCFILE.LTC_SELLPR_3,
                   LTCFILE.LTC_MISC_AMT_3,
                   ISNULL(LTCFILE.LTC_SENT_FLAG_3,''),
                   ISNULL(CAST(LTCFILE.LTC_VIN_CHG_DATE_3
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(LTCFILE.LTC_HHGS_1,''),
                   ISNULL(LTCFILE.LTC_HHGS_2,''),
                   ISNULL(LTCFILE.LTC_HHGS_3,'')
            FROM DBO.LTCFILE
            WHERE LTCFILE.LTC_BRNO   >= :QLTC1-WBEG-BRNO
              AND LTCFILE.LTC_BRNO   <= :QLTC1-WEND-BRNO
              AND LTCFILE.LTC_ACCTNO >= :QLTC1-WBEG-ACCTNO
              AND LTCFILE.LTC_ACCTNO <= :QLTC1-WEND-ACCTNO
              AND LTCFILE.LTC_SEQNO  >= :QLTC1-WBEG-SEQNO
              AND LTCFILE.LTC_SEQNO  <= :QLTC1-WEND-SEQNO
            ORDER BY LTCFILE.LTC_BRNO,
                     LTCFILE.LTC_ACCTNO,
                     LTCFILE.LTC_SEQNO
           END-EXEC.

           EXEC SQL
               OPEN LTC1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QLTC1-WBEG-BRNO
                          QLTC1-WEND-BRNO
                          QLTC1-WBEG-ACCTNO
                          QLTC1-WEND-ACCTNO
                          QLTC1-WBEG-SEQNO
                          QLTC1-WEND-SEQNO.

           MOVE STAT---GOOD TO LTC1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-LTC1-FILE.
           PERFORM READ-IT.
           MOVE LTC-PATH-OWNBR  TO LTC-BRNO. 
           MOVE LTC-SELECT-PATH TO E-FILE.
           MOVE LTC1-KEY        TO E-KEYX.

           MOVE LTC-BRNO   TO QLTC-BRNO.
           MOVE LTC-ACCTNO TO QLTC-ACCTNO.
           MOVE LTC-SEQNO  TO QLTC-SEQNO.

           EXEC SQL
            SELECT LTCFILE.LTC_BRNO,
                   LTCFILE.LTC_ACCTNO,
                   LTCFILE.LTC_SEQNO,
                   LTCFILE.LTC_DESC_1, 
                   LTCFILE.LTC_MAKE_1, 
                   LTCFILE.LTC_MODEL_1, 
                   LTCFILE.LTC_YEAR_1, 
                   LTCFILE.LTC_STYLE_1, 
                   LTCFILE.LTC_CLASS_1, 
                   LTCFILE.LTC_VIN_1, 
                   LTCFILE.LTC_TITLE_NO_1, 
                   LTCFILE.LTC_MILEAGE_1,
                   LTCFILE.LTC_VALUE_1,
                   LTCFILE.LTC_SELLPR_1,
                   LTCFILE.LTC_MISC_AMT_1,
                   LTCFILE.LTC_SENT_FLAG_1,
                   CAST(LTCFILE.LTC_VIN_CHG_DATE_1 AS VARCHAR(10)),
                   LTCFILE.LTC_DESC_2, 
                   LTCFILE.LTC_MAKE_2, 
                   LTCFILE.LTC_MODEL_2, 
                   LTCFILE.LTC_YEAR_2, 
                   LTCFILE.LTC_STYLE_2, 
                   LTCFILE.LTC_CLASS_2, 
                   LTCFILE.LTC_VIN_2, 
                   LTCFILE.LTC_TITLE_NO_2, 
                   LTCFILE.LTC_MILEAGE_2,
                   LTCFILE.LTC_VALUE_2,
                   LTCFILE.LTC_SELLPR_2,
                   LTCFILE.LTC_MISC_AMT_2,
                   LTCFILE.LTC_SENT_FLAG_2, 
                   CAST(LTCFILE.LTC_VIN_CHG_DATE_2 AS VARCHAR(10)),
                   LTCFILE.LTC_DESC_3, 
                   LTCFILE.LTC_MAKE_3,  
                   LTCFILE.LTC_MODEL_3, 
                   LTCFILE.LTC_YEAR_3, 
                   LTCFILE.LTC_STYLE_3, 
                   LTCFILE.LTC_CLASS_3, 
                   LTCFILE.LTC_VIN_3, 
                   LTCFILE.LTC_TITLE_NO_3, 
                   LTCFILE.LTC_MILEAGE_3,
                   LTCFILE.LTC_VALUE_3,
                   LTCFILE.LTC_SELLPR_3,
                   LTCFILE.LTC_MISC_AMT_3,
                   LTCFILE.LTC_SENT_FLAG_3, 
                   CAST(LTCFILE.LTC_VIN_CHG_DATE_3 AS VARCHAR(10))
              INTO  
                   :QLTC-BRNO,
                   :QLTC-ACCTNO,
                   :QLTC-SEQNO,
                   :QLTC-DESC-1, 
                   :QLTC-MAKE-1, 
                   :QLTC-MODEL-1, 
                   :QLTC-YEAR-1, 
                   :QLTC-STYLE-1, 
                   :QLTC-CLASS-1, 
                   :QLTC-VIN-1, 
                   :QLTC-TITLE-NO-1, 
                   :QLTC-MILEAGE-1,
                   :QLTC-VALUE-1,
                   :QLTC-SELLPR-1,
                   :QLTC-MISC-AMT-1,
                   :QLTC-SENT-FLAG-1,
                   :QLTC-VIN-CHG-DATE-1,
                   :QLTC-DESC-2, 
                   :QLTC-MAKE-2, 
                   :QLTC-MODEL-2, 
                   :QLTC-YEAR-2, 
                   :QLTC-STYLE-2, 
                   :QLTC-CLASS-2, 
                   :QLTC-VIN-2, 
                   :QLTC-TITLE-NO-2, 
                   :QLTC-MILEAGE-2,
                   :QLTC-VALUE-2,
                   :QLTC-SELLPR-2,
                   :QLTC-MISC-AMT-2,
                   :QLTC-SENT-FLAG-2, 
                   :QLTC-VIN-CHG-DATE-2,
                   :QLTC-DESC-3, 
                   :QLTC-MAKE-3,  
                   :QLTC-MODEL-3, 
                   :QLTC-YEAR-3, 
                   :QLTC-STYLE-3, 
                   :QLTC-CLASS-3, 
                   :QLTC-VIN-3, 
                   :QLTC-TITLE-NO-3, 
                   :QLTC-MILEAGE-3,
                   :QLTC-VALUE-3,
                   :QLTC-SELLPR-3,
                   :QLTC-MISC-AMT-3,
                   :QLTC-SENT-FLAG-3, 
                   :QLTC-VIN-CHG-DATE-3
              FROM DBO.LTCFILE
             WHERE LTCFILE.LTC_BRNO   = :QLTC-BRNO
               AND LTCFILE.LTC_ACCTNO = :QLTC-ACCTNO
               AND LTCFILE.LTC_SEQNO  = :QLTC-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LTC1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-LTC-FIELDS.

      *-----------------------------------------------------------------
       READ-LTC1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE LTC-PATH-OWNBR  TO LTC-BRNO.
           MOVE LTC-SELECT-PATH TO E-FILE.
           MOVE LTC1-KEY        TO E-KEYX.
           INITIALIZE QLTC-REC.

           IF ( LTC1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH LTC1_CURSOR INTO :QLTC-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LTC1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-LTC-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-LTC1-FILE.

           PERFORM CLOSE-IT.
           IF ( LTC1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE LTC1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO LTC1-CURSOR-STAT.

	   
       COPY "LIBGB/GBWKIN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************
       END-RUN.
           PERFORM CLOSE-FILES.

           IF ( SKIP-RECORDS-FOUND )
              GO TO EXIT-PROG.

           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.

           IF NO-RECORDS-FOUND
              MOVE "NO RECORDS FOUND FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS.

       EXIT-PROG.
           MOVE MKTEMP-BUF TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.

           IF SV-BEG-BR NOT = 9999
              MOVE SV-BEG-BR TO BEG-BRANCH
              MOVE SV-END-BR TO END-BRANCH.

           IF EXT-ROUTE-BUF = "00"
              MOVE EXIT-PATH TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY AULNRG-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
