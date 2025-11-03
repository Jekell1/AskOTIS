       IDENTIFICATION DIVISION.
       PROGRAM-ID.      LPLIST.
       DATE-WRITTEN.  12/22/94.
      *************************************************************************
      *    DIR :  LS
      *    DESC:  PAYMENT FILE
      *
      *    --WHEN TRANS-DATE IS EQUAL TO PAYMENT-DATE, THE
      *      TRANS-DATE IS NOT PRINTED.
      *    --WHEN LN-LOANTYPE IS EQUAL TO 'P', LATE-CHARGE IS
      *      PRINTED ABOVE INTEREST.  OTHERWISE, WHEN 'I',
      *      INTEREST IS PRINTED ABOVE LATECHARGE (APPLIED AND
      *      OWING).
      *
      * REV:
      *  JTG 082995 REVISED PRINT FORMAT
      *  JFF 102095 ADDED BORROWER LOAN SCAN
      *  MJD 071296 CHANDED PDTH DISPLAY FROM ZZ9999 TO 999999
      *             TO MATCH A50.
      *  JTG 101196 ADDED 'LXE-WORKER' & 'LXE-WORKER2' FOR LPCERN-- A50 VERSION
      *                   SP-ERNFRMLA 15 & 16, REAL INTEREST BEARINGING METHOD
      *  BAH 062998 TILLNO IS NOW ALPHANUMERIC
      *  MJD 000119 ADDED LP-NOLCHG (WORLD PR#218)
      *  AMM 070212 REFORMATTED ENTIRE LISTING
      *  AMW 071017 {PR#3093} TRITON; ADDED 'OLD' STYLE SUMMARY LISTING
      *             BACK TO PROGRAM.
      *   CS 100315 ADDED LP-APPLIED-TO-ESCROW
      *   CS 161031 ADDED LXA-START-PMT(4-9),LXA-END-PMT(4-9)
      *             LXA-NET-CHANGE(4-9)
      *  BAH 170504 ACTIVATE 8 DIGIT DATES, PD0006
      *   CS 180327 CHANGED D-COL-1-FG-2 FROM PIC X TO Z9 (CASH TILL)
      * BAH 190103 GLOBAL LPFILE
      * BAH 20190308 CHANGED LP-ADDON TO USE LPWSLP-LP-ADDON, MOVED LP-REFNO
      *              REDEFINITIONS TO LPWSLP COPY MEMBER
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
       I-O-CONTROL.

       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LS/LPLIST.CBL S35 02/21/24-12:02:39 >".

      *********************************************************
      *     F I L E   I F N
      *********************************************************
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
      *COPY "LIBLP/LP01FD.CPY".
      *COPY "LIBLP/LP01FD_SQL.CPY".
      *COPY "LIBLP/LPWSFD.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01LP.CPY".
       COPY "LIBLP/LP01LP_SQL.CPY".
       COPY "LIBLP/LPWSLP.CPY".
       COPY "LIBLP/LP01LX.CPY".
       COPY "LIBLP/LP01LX_SQL.CPY".
       COPY "LIBLP/LPWSLX.CPY".
       COPY "LIBLP/LP01LXE.CPY".
       COPY "LIBLP/LP01LXE_SQL.CPY".
       COPY "LIBLP/LPWSLXE.CPY".
       COPY "LIBLP/LP01LXG.CPY".
       COPY "LIBLP/LP01LXG_SQL.CPY".
       COPY "LIBLP/LPWSLXG.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".

      *********************************************************
      *   L I N E   W O R K E R S
      *********************************************************
       01  LN-FEED          PIC 99       VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.
       01  PAGE-LINE-NO         PIC 99       VALUE 10.

      *********************************************************
      *   M I S C .   W O R K E R S
      *********************************************************
       01  FIRST-TIME           PIC X        VALUE "Y".
       01  RECORDS-FOUND        PIC X         VALUE " ".
           88  NO-RECORDS-FOUND               VALUE "N".
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".
       01  HOLD-IO-FG           PIC 9        VALUE 0.
       01  HOLD-ACCTNO          PIC 9(8)     VALUE 0.
       01  LOAN-FG              PIC X(3)     VALUE "   ".
           88  LOAN-BEG                      VALUE "BEG".
           88  LOAN-END                      VALUE "END".
      *********************************************************
      *     S C R E E N   F I E L D S   *
      *********************************************************
       01  SCREEN-BUF          VALUE " ".
           03  ERRCD           PIC X.
           03  BEG-LP          PIC 9(6).
           03  END-LP          PIC 9(6).
           03  LIST-REVERSED   PIC X.
           03  DETAIL-SUM      PIC X.

      *********************************************************
      *     P R I N T   B U F F E R S
      *********************************************************

       01  HEAD1.
           03  H-DATE      PIC X(8)      VALUE " ".
           03  FILLER      PIC X         VALUE " ".
           03  H-HOUR      PIC 9(2)      VALUE ZERO.
           03  FILLER      PIC X         VALUE ":".
           03  H-MIN       PIC 9(2)      VALUE ZERO.
           03  FILLER      PIC X(34)     VALUE " ".
           03  H-CONAME    PIC X(40)     VALUE " ".
           03  FILLER      PIC X(35)     VALUE " ".
           03  FILLER      PIC X(6)      VALUE "PAGE: ".
           03  H-PAGE-NO   PIC 999       VALUE ZERO.

      *--------------------------------------------------------

       01  HEAD2.
           03  FILLER      PIC X(9)      VALUE "USER ID: ".
           03  H-USERID    PIC X(12)     VALUE " ".
           03  FILLER      PIC X(35)     VALUE " ".
           03  FILLER      PIC X(25)     VALUE
                           "LOAN PAYMENT FILE LISTING".
           03  FILLER      PIC X(15)     VALUE " ".
           03  FILLER      PIC X(11)     VALUE "BRANCH NO: ".
           03  H-BRNO      PIC 9(4)      VALUE ZERO.
           03  FILLER      PIC X         VALUE " ".
           03  H-BRDESC    PIC X(20)     VALUE " ".

      *--------------------------------------------------------

       01  HEAD3.
           03  H-CLASS     PIC 9(2)      VALUE ZERO.
           03  FILLER      PIC X         VALUE SPACE.
           03  H-ACCTNO    PIC 9(8)      VALUE ZERO.
           03  FILLER      PIC X         VALUE "-".
           03  H-SEQ-NO    PIC 9(2)      VALUE ZERO.
           03  FILLER      PIC X         VALUE " ".
           03  H-STAT-FG   PIC X(9)      VALUE " ".
           03  FILLER      PIC X(2)      VALUE " ".
           03  H-BWNAME    PIC X(33)     VALUE " ".
           03  FILLER      PIC X(73)     VALUE " ".

      *--------------------------------------------------------

       01  HEAD4.
           03  FILLER      PIC X(10)     VALUE " ".
           03  FILLER      PIC X(2)      VALUE "TR".
           03  FILLER      PIC X(11)    VALUE SPACES.
           03  H4-TRAMT    PIC X(5)      VALUE SPACES.
           03  FILLER      PIC X(4)     VALUE SPACES.

      *    03  FILLER      PIC X(20)     VALUE " ".
           03  FILLER      PIC X(8)      VALUE ALL "-".
           03  FILLER      PIC X(7)      VALUE "APPLIED".
           03  FILLER      PIC X(9)      VALUE ALL "-".
           03  FILLER      PIC X(19)     VALUE " ".
           03  FILLER      PIC X(9)      VALUE ALL "-".
           03  FILLER      PIC X(5)      VALUE "OWING".
           03  FILLER      PIC X(10)     VALUE ALL "-".
           03  FILLER      PIC X         VALUE " ".
           03  FILLER      PIC X         VALUE "D".
           03  FILLER      PIC X(19)     VALUE " ".
           03  FILLER      PIC X(2)      VALUE "IB".
           03  FILLER      PIC X(10)     VALUE " ".

      *--------------------------------------------------------

       01  HEAD5.
           03  FILLER      PIC X(2)      VALUE "LN".
           03  FILLER      PIC X         VALUE " ".
           03  FILLER      PIC X(6)      VALUE "PYDATE".
           03  FILLER      PIC X         VALUE " ".
           03  FILLER      PIC X(2)      VALUE "CD".
           03  FILLER      PIC X         VALUE " ".
           03  FILLER      PIC X(5)      VALUE "REFNO".
           03  FILLER      PIC X(4)      VALUE " ".
           03  H5-TRAMT-OR-ESCROW
                           PIC X(6)      VALUE SPACES.
      *    03  FILLER      PIC X(6)      VALUE "TR-AMT".
           03  FILLER      PIC X(4)      VALUE " ".
           03  FILLER      PIC X(5)      VALUE "OTHER".
           03  FILLER      PIC X(3)      VALUE " ".
           03  FILLER      PIC X(6)      VALUE "LC/INT".
           03  FILLER      PIC X(6)      VALUE " ".
           03  FILLER      PIC X(4)      VALUE "PRIN".
           03  FILLER      PIC X(4)      VALUE " ".
           03  FILLER      PIC X(4)      VALUE "PDTH".
           03  FILLER      PIC X         VALUE " ".
           03  FILLER      PIC X(6)      VALUE "LCPDTH".
           03  FILLER      PIC X(4)      VALUE " ".
           03  FILLER      PIC X(5)      VALUE "OTHER".
           03  FILLER      PIC X(3)      VALUE " ".
           03  FILLER      PIC X(6)      VALUE "LC/INT".
           03  FILLER      PIC X(3)      VALUE " ".
           03  FILLER      PIC X(7)      VALUE "BALANCE".
           03  FILLER      PIC X         VALUE " ".
           03  FILLER      PIC X         VALUE "#".
           03  FILLER      PIC X(3)      VALUE " ".
           03  FILLER      PIC X(6)      VALUE "LC-DUE".
           03  FILLER      PIC X(2)      VALUE " ".
           03  FILLER      PIC X(7)      VALUE "INT-DUE".
           03  FILLER      PIC X         VALUE " ".
           03  FILLER      PIC X(2)      VALUE "PC".
           03  FILLER      PIC X(6)      VALUE " ".
           03  FILLER      PIC X(4)      VALUE "SSNO".

      *--------------------------------------------------------
      *********************************************************
      *     D E T A I L - L I N E - W O R K E R S
      *********************************************************

       01  DETAIL-LINE          PIC X(132) VALUE " ".
       01  SUB-LINE-BREAK      REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(3).
           03  SUB-LINE         PIC X(129).
       01  RANGE-LINE          REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(22).
           03  RANGE-SELECTION  PIC X(16).

      * COLUMN 01-------------------------------------------------------
       01  DETAIL-LINE-01           REDEFINES DETAIL-LINE.
           03  FILLER               PIC X(01).
           03  D-COL-1-NUMBER       PIC X(03).
           03  FILLER               PIC X(01).
           03  D-COL-1-HEADING      PIC X(16).
           03  FILLER               PIC X(01).
           03  D-COLON-1            PIC X.
           03  FILLER               PIC X(01).
           03  D-COL-1-FIELD        PIC X(15).
           03  FILLER               REDEFINES D-COL-1-FIELD.
               05  D-COL-1-CODE     PIC X(02).
           03  FILLER               REDEFINES D-COL-1-FIELD.
               05  D-COL-1-DATE     PIC 99/99/99.
               05  FILLER           PIC X(01).
               05  D-COL-1-TIME     PIC X(06).
           03  FILLER               REDEFINES D-COL-1-FIELD.
               05  D-COL-1-POST-BR  PIC ZZZ9.
               05  FILLER           PIC X(01).
               05  D-COL-1-USER     PIC X(10).
           03  FILLER               REDEFINES D-COL-1-FIELD.
               05  D-COL-1-REFER    PIC X(05).
           03  FILLER               REDEFINES D-COL-1-FIELD.
               05  D-COL-1-FG       PIC X.
               05  FILLER           PIC X(03).
               05  D-COL-1-FG-2     PIC Z9.
           03  FILLER               REDEFINES D-COL-1-FIELD.
               05  D-COL-1-SSNO     PIC 999/99/9999.
           03  FILLER               REDEFINES D-COL-1-FIELD.
               05  D-2-AMOUNT       PIC ZZZZZZ9.99.

      * COLUMN 02-------------------------------------------------------
           03  FILLER               PIC X(05).
           03  D-COL-2-NUMBER       PIC X(03).
           03  FILLER               PIC X(01).
           03  D-COL-2-HEADING      PIC X(16).
           03  FILLER               PIC X(01).
           03  D-COLON-2            PIC X.
           03  FILLER               PIC X(01).
           03  D-COL-2-FIELD        PIC X(22).
           03  FILLER               REDEFINES D-COL-2-FIELD.
               05  D-COL-2-AMOUNT   PIC ZZZ,ZZ9.99.
               05  FILLER           PIC X(02).
               05  D-COL-2-AMOUNT-2 PIC ZZZ,ZZ9.99.
           03  FILLER               REDEFINES D-COL-2-FIELD.
               05  D-COL-2-FG-CODE  PIC X(03).
           03  FILLER               REDEFINES D-COL-2-FIELD.
               05  D-COL-2-PYMNT-1  PIC 9(03).
               05  FILLER           PIC X(03).
               05  D-COL-2-PYMNT-2  PIC 9(03).
               05  FILLER           PIC X(03).
               05  D-COL-2-PYMNT-3  PIC 9(03).
           03  FILLER               REDEFINES D-COL-2-FIELD.
               05  D-COL-2-DATE     PIC 99/99/99.

      * PAGE 01 COLUMN 01 BOTTOM----------------------------------------
       01  DETAIL-LINE-02           REDEFINES DETAIL-LINE.
           03  FILLER               PIC X(01).
           03  D-COL-1-GLNO-HEAD    PIC X(06).
           03  FILLER               PIC X(01).
           03  D-COL-1-GLNO-NUM     PIC 9999/9999999.
           03  FILLER               PIC X(01).
           03  D-COL-1-AMT-HEAD     PIC X(04).
           03  FILLER               PIC X(01).
           03  D-COL-1-AMT-NUM      PIC ZZ,ZZ9.99.

      * ACCOUNT NUMBER AND SEQUENCE NUMBER HEADING----------------------
       01  DETAIL-LINE-03           REDEFINES DETAIL-LINE.
           03  FILLER               PIC X(01).
           03  D03-BEG-LINES        PIC X(77).
           03  D03-ACCT-TEXT        PIC X(13).
           03  FILLER               PIC X(01).
           03  D03-ACCT-NO          PIC ZZZZZZZ9.
           03  FILLER               PIC X(01).
           03  D03-SEQ-TEXT         PIC X(07).
           03  FILLER               PIC X(01).
           03  D03-SEQ-NO           PIC 9999.
           03  FILLER               PIC X(01).
           03  D03-PAGE             PIC X(07).
           03  FILLER               PIC X(01).
           03  D03-END-LINES        PIC X(06).

      *********************************************************
      * OLD LISTING PRINTOUT
      *********************************************************
       01  ACTION1.
           03  A1-SEQNO       PIC ZZZ9         BLANK ZERO.
           03  FILLER         PIC X            VALUE " ".
           03  A1-PAYDATE     PIC 9(6)         BLANK ZERO.
           03  A1-REVERSED    PIC X            VALUE " ".
           03  A1-TRCD        PIC X(2)         VALUE " ".
           03  FILLER         PIC X            VALUE " ".
           03  A1-REFNO       PIC X(5)         VALUE " ".
           03  FILLER         PIC X            VALUE " ".

           03  A1-TR-AMT      PIC ZZZZZ9.99-   BLANK ZERO.
           03  A1-APP-OTHER   PIC ZZZZ9.99-    BLANK ZERO.
           03  A1-APP-LC-INT  PIC ZZZZ9.99-    BLANK ZERO.


           03  A1-APP-PRINC   PIC ZZZZZ9.99-   BLANK ZERO.
           03  FILLER         PIC X            VALUE " ".
           03  A1-PDTHRU      PIC 999999       BLANK ZERO.
           03  FILLER         PIC X            VALUE " ".
           03  A1-LC-PDTHRU   PIC 999999       BLANK ZERO.
           03  FILLER         PIC X            VALUE " ".
           03  A1-OW-OTHER    PIC ZZZZ9.99-    BLANK ZERO.
           03  A1-OW-LC-INT   PIC ZZZZ9.99-    BLANK ZERO.
           03  A1-OW-BALANCE  PIC ZZZZZ9.99    BLANK ZERO.
           03  FILLER         PIC X            VALUE " ".
           03  A1-TILL-NO     PIC X.
           03  FILLER         PIC X            VALUE " ".
           03  A1-LC-DUE      PIC ZZZZ9.99-    BLANK ZERO.
           03  A1-INT-DUE     PIC ZZZZ9.99-    BLANK ZERO.
           03  FILLER         PIC X            VALUE " ".
           03  A1-IB-PC       PIC X            VALUE " ".
           03  FILLER         PIC X            VALUE " ".
           03  A1-SOC-SEC     PIC 9(9)         BLANK ZERO.
      *--------------------------------------------------------

       01  ACTION2.
           03  FILLER         PIC X(3)         VALUE " ".
           03  A2-TR-DATE     PIC 9(6)         BLANK ZERO.
           03  FILLER         PIC X(12)        VALUE " ".
           03  A2-APP-ESCROW  PIC ZZZZZ9.99-   BLANK ZERO.
           03  FILLER         PIC X(9)         VALUE SPACES.


           03  A2-APP-LC-INT  PIC ZZZZ9.99-    BLANK ZERO.
           03  FILLER         PIC X(32)        VALUE " ".
           03  A2-OW-LC-INT   PIC ZZZZ9.99-    BLANK ZERO.
           03  FILLER         PIC X(42)        VALUE " ".

      *--------------------------------------------------------

       01  ACTION3.
           03  FILLER         PIC X(3)        VALUE " ".
           03  FILLER         PIC X(6)        VALUE "ST-FG:".
           03  FILLER         PIC X           VALUE " ".
           03  A3-STAT-FG     PIC X           VALUE " ".
           03  FILLER         PIC X(9)        VALUE " ".
           03  FILLER         PIC X(10)       VALUE "DL-PT-ADJ:".
           03  FILLER         PIC X           VALUE " ".
           03  A3-DL-PART-ADJ PIC ZZZZ9.99-   BLANK ZERO.
           03  FILLER         PIC X(4)        VALUE " ".
           03  FILLER         PIC X(8)        VALUE "LC-WORKS".
           03  FILLER         PIC X(20)       VALUE " ".
           03  FILLER         PIC X(4)        VALUE "INT:".
           03  FILLER         PIC X           VALUE " ".
           03  A3-IFU-INT     PIC ZZZZ9.99-   BLANK ZERO.
           03  FILLER         PIC X(4)        VALUE " ".
           03  FILLER         PIC X(6)        VALUE "ADDON:".
           03  FILLER         PIC X           VALUE " ".
           03  A3-IFU-ADDON   PIC ZZZZ9.99-   BLANK ZERO.
           03  FILLER         PIC X(4)        VALUE " ".
           03  A3-GL-ACCT     PIC 999/9999999 BLANK ZERO.
           03  FILLER         PIC X(2)        VALUE " ".
           03  A3-GL-AMOUNT   PIC ZZZZ9.99-   BLANK ZERO.

      *--------------------------------------------------------

       01  ACTION4.
           03  FILLER         PIC X(3)        VALUE " ".
           03  FILLER         PIC X(6)        VALUE "#LCHG:".
           03  FILLER         PIC X           VALUE " ".
           03  A4-NOLCHG      PIC Z(03)-.
           03  FILLER         PIC X(8)        VALUE SPACES.
           03  FILLER         PIC X(8)        VALUE "DL-PROC:".
           03  FILLER         PIC X           VALUE " ".
           03  A4-DL-PROCEED  PIC ZZZZ9.99-   BLANK ZERO.
           03  FILLER         PIC X(4)        VALUE " ".
           03  A4-LC-WORK-1   PIC ZZZZ9.99-   BLANK ZERO.
           03  FILLER         PIC X(3)        VALUE "   ".
           03  A4-EARN-WORK-1 PIC ZZZZZZ9.99- BLANK ZERO.
           03  FILLER         PIC X(6)        VALUE " ".
           03  FILLER         PIC X(3)        VALUE "SC:".
           03  FILLER         PIC X           VALUE " ".
           03  A4-IFU-SERVICE PIC ZZZZ9.99-   BLANK ZERO.
           03  FILLER         PIC X(7)        VALUE " ".
           03  FILLER         PIC X(3)        VALUE "MF:".
           03  FILLER         PIC X           VALUE " ".
           03  A4-IFU-MAINT   PIC ZZZZ9.99-   BLANK ZERO.
           03  FILLER         PIC X(4)        VALUE " ".
           03  A4-GL-ACCT     PIC 999/9999999 BLANK ZERO.
           03  FILLER         PIC X(2)        VALUE " ".
           03  A4-GL-AMOUNT   PIC ZZZZ9.99-   BLANK ZERO.

      *--------------------------------------------------------

       01  ACTION5.
           03  FILLER         PIC X(3)        VALUE " ".
           03  FILLER         PIC X(6)        VALUE "NW-ST:".
           03  FILLER         PIC X           VALUE " ".
           03  A5-NW-STAT-RT  PIC ZZ9.999     BLANK ZERO.
           03  FILLER         PIC X(4)        VALUE " ".
           03  FILLER         PIC X(9)        VALUE "INT-PAID:".
           03  FILLER         PIC X           VALUE " ".
           03  A5-INT-PAID    PIC ZZZZ9.99-   BLANK ZERO.
           03  FILLER         PIC X(4)        VALUE " ".
           03  A5-LC-WORK-2   PIC ZZZZ9.99-   BLANK ZERO.
           03  FILLER         PIC X(3)        VALUE " ".
           03  A5-EARN-WORK-2 PIC ZZZZZZ9.99- BLANK ZERO.
           03  FILLER         PIC X(4)        VALUE " ".
           03  FILLER         PIC X(5)        VALUE "DISC:".
           03  FILLER         PIC X           VALUE " ".
           03  A5-IFU-DISCT   PIC ZZZZ9.99-   BLANK ZERO.
           03  FILLER         PIC X(7)        VALUE " ".
           03  FILLER         PIC X(3)        VALUE "OT:".
           03  FILLER         PIC X           VALUE " ".
           03  A5-IFU-OTHER   PIC ZZZZ9.99-   BLANK ZERO.
           03  FILLER         PIC X(4)        VALUE " ".
           03  A5-GL-ACCT     PIC 999/9999999 BLANK ZERO.
           03  FILLER         PIC X(2)        VALUE " ".
           03  A5-GL-AMOUNT   PIC ZZZZ9.99-   BLANK ZERO.

      *--------------------------------------------------------
       COPY "LIBWI/BWLNSCW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBLP/LPSTATW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       01  LPLIST-WINDOW-HANDLE PIC X(10).

       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLS/LPLIST_DEF.CPY".
       COPY "LIBLS/LPLIST_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ****************************************
      *     P R O G R A M   C O N T R O L    *
      ****************************************
       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBLS/LPLIST_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE .
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-LXG1-FILE.
           PERFORM CLOSE-LXE1-FILE.
           PERFORM CLOSE-LX1-FILE.
           PERFORM CLOSE-LP1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      *********************************************************
      *              M A I N   M O D U L E :
      *********************************************************
       MAIN-MODULE SECTION.
           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".
           MOVE FORM-PATHNAME TO  FORM-PATH.
           MOVE "LPLIST" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS LPLIST-WINDOW-HANDLE.

      D    STOP MESS.
           PERFORM FORM-RESET.

           IF EXT-ROUTE-BUF NOT = "00"                                          
              MOVE "PROGRAM NOT BATCHABLE" TO MESS                              
              PERFORM SEND-MESS        
              MOVE "99" TO EXT-ROUTE-BUF                                        
              GO TO EXIT-PROG.

           PERFORM INITIALIZATION.

           IF ERRCD NOT = " "
              GO TO END-PROGRAM.

           MOVE BEG-LP TO HOLD-ACCTNO.

           MOVE 0      TO LN-OWNBR LN-ACCTNO.

           PERFORM OPEN-START-FILES.

       PROCESS-FILE-LISTING.
           PERFORM ACCESS-FILES.
           IF HOLD-IO-FG = 9 OR (LP-ACCTNO > END-LP) OR
                                 (LP-BRNO NOT = LP-PATH-OWNBR)
              GO TO END-ROUTINE.

           IF LIST-REVERSED = "N"
              IF LP-REV NOT = "Y" AND LP-TRCD NOT = "RV"
                 PERFORM DETAIL-LINE-PROCESS
              ELSE
                 NEXT SENTENCE
           ELSE
              PERFORM DETAIL-LINE-PROCESS.

           GO TO PROCESS-FILE-LISTING.

       END-ROUTINE.
           IF FIRST-TIME = "Y"
              MOVE "N" TO RECORDS-FOUND
              GO TO END-PROGRAM.

           PERFORM PRINT-RANGE-LINES.
           GO TO END-PROGRAM.

      *********************************************************
       ACCESS-FILES SECTION.
      *********************************************************

           PERFORM READ-LP1-FILE-NEXT.
           MOVE IO-FG TO HOLD-IO-FG.
           IF IO-FG = 9
              GO TO EXIT-ACCESS-FILES.

           IF (LP-ACCTNO > END-LP) OR (LP-BRNO NOT = LP-PATH-OWNBR)
              GO TO EXIT-ACCESS-FILES.

           IF LP-SEQNO = 1
              MOVE LP-SSNO TO BW-SSNO
              PERFORM READ-BW1-FILE
              MOVE LP-BRNO   TO LN-OWNBR
              MOVE LP-ACCTNO TO LN-ACCTNO
              PERFORM READ-LN1-FILE
              PERFORM GET-BRANCH-DESCRIPTION.

           MOVE LP-BRNO    TO  LX-BRNO.
           MOVE LP-ACCTNO  TO  LX-ACCTNO.
           MOVE LP-SEQNO   TO  LX-SEQNO.
           PERFORM READ-LX1-FILE.
           IF ( IO-BAD )
      * CLEAR FIELDS JUST FOR PRINTING
              PERFORM CLEAR-LX-FILE
              MOVE LP-BRNO    TO  LX-BRNO
              MOVE LP-ACCTNO  TO  LX-ACCTNO
              MOVE LP-SEQNO   TO  LX-SEQNO.

           MOVE LP-BRNO    TO  LXE-BRNO.
           MOVE LP-ACCTNO  TO  LXE-ACCTNO.
           MOVE LP-SEQNO   TO  LXE-SEQNO.
           PERFORM READ-LXE1-FILE.
           IF ( IO-FG = 0 ) AND ( DETAIL-SUM = "S" )
              PERFORM FILL-LXE-LINES
           ELSE
           IF ( IO-BAD ) AND ( DETAIL-SUM = "D" )
              MOVE ZEROES TO LXE-EARN(1)
                             LXE-EARN(2)
                             LXE-EARN(3)
                             LXE-WORKER
                             LXE-WORKER2.

           MOVE LP-BRNO    TO  LXG-BRNO.
           MOVE LP-ACCTNO  TO  LXG-ACCTNO.
           MOVE LP-SEQNO   TO  LXG-SEQNO.
           PERFORM READ-LXG1-FILE.
           IF ( IO-FG = 0 ) AND ( DETAIL-SUM = "S" )
              PERFORM FILL-LXG-LINES
           ELSE
           IF ( IO-BAD ) AND ( DETAIL-SUM = "D" )
              MOVE ZEROES TO LXG-GLNO(1)
                             LXG-GLNO(2)
                             LXG-GLNO(3)
                             LXG-GLAMT(1)
                             LXG-GLAMT(2)
                             LXG-GLAMT(3).


       EXIT-ACCESS-FILES.
           EXIT.

      *********************************************************
       INITIALIZATION SECTION.
      *********************************************************

           PERFORM GET-GPENV.

           MOVE " " TO ERRCD.

       GETPR.
           MOVE LPLIST-QUEUE-POS TO Q-POS.
           MOVE LPLIST-COPIES-POS TO C-POS.
           MOVE LPLIST-PRU-POS TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
           PERFORM PRU-IN.
           IF STAT NOT = "00"
              GO TO END-INIT.

       LP-01.
           MOVE "ENNN060LOANNO1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO GETPR.
           IF VDUSTATUS = "16" GO TO ALL-LP.
           IF VDUSTATUS = "1>" GO TO END-INIT.
           IF VDUSTATUS = "1<"
              MOVE "BEG" TO LOAN-FG
              PERFORM BWLN-SCAN
              IF IO-FG NOT = 0
              GO TO LP-01.
           IF VDUSTATUS NOT = "00" GO TO LP-01.
           MOVE VDUNUM0 TO BEG-LP.

       LP-02.
           MOVE "ENNN060LOANNO2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO GETPR.
           IF VDUSTATUS = "1U" GO TO LP-01.
           IF VDUSTATUS = "1>" GO TO END-INIT.
           IF VDUSTATUS = "1<"
              MOVE "END" TO LOAN-FG
              PERFORM BWLN-SCAN
              IF IO-FG NOT = 0
              GO TO LP-02.
           IF VDUSTATUS NOT = "00" GO TO LP-02.
           MOVE VDUNUM0 TO END-LP.
           IF BEG-LP > END-LP
              MOVE "INVALID LOAN NUMBER RANGE !" TO MESS
              PERFORM SEND-MESS
              GO TO LP-01.
           GO TO LIST-REVERSE.

       ALL-LP.
           MOVE "SNNN060LOANNO1." TO VDU.
           MOVE 0         TO  BEG-LP VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN060LOANNO2." TO VDU.
           MOVE 999999  TO  END-LP VDUNUM0.
           PERFORM SCREENIO.

       LIST-REVERSE.
           MOVE "E  A010LIST-REV." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO GETPR.
           IF VDUSTATUS = "1U" GO TO LP-02.
           IF VDUSTATUS = "1>" GO TO END-INIT.
           IF VDUSTATUS NOT = "00" GO TO LIST-REVERSE.
           MOVE VDUBUF TO LIST-REVERSED.
           IF LIST-REVERSED NOT = "Y" AND NOT = "N"
              GO TO LIST-REVERSE.

       DETAIL-SUM.
           MOVE "E  A010DET-SUM." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO GETPR.
           IF VDUSTATUS = "1U" GO TO LIST-REVERSE.
           IF VDUSTATUS = "1>" GO TO END-INIT.
           IF VDUSTATUS NOT = "00" GO TO DETAIL-SUM.
           MOVE VDUBUF TO DETAIL-SUM.
           IF DETAIL-SUM NOT = "D" AND NOT = "S"
              GO TO DETAIL-SUM.

       OK-YN.
           MOVE "E  A010ACCEPT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO GETPR.
           IF VDUSTATUS = "1U" GO TO DETAIL-SUM.
           IF VDUSTATUS = "1>" GO TO END-INIT.
           IF VDUSTATUS NOT = "00" GO TO OK-YN.
           IF VDUBUF = "N" GO TO LP-01.
           IF VDUBUF NOT = "Y" GO TO OK-YN.

           MOVE "LISTING IN PROGRESS...................." TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE EXT-CONAME-CONAME   TO  H-CONAME.
           MOVE EXT-CONAME-SYSDATE  TO  H-DATE.
           MOVE EXT-CONAME-USER-ID  TO  H-USERID.

           GO TO EXIT-INITIALIZE.

       END-INIT.
           MOVE 1 TO ERRCD.
       EXIT-INITIALIZE.
           EXIT.

      *********************************************************
       DETAIL-LINE-PROCESS SECTION.
      *********************************************************

           IF ( DETAIL-SUM = "D" )
              PERFORM PRINT-DETAIL-LISTING
           ELSE
              PERFORM PRINT-SUMMARY-LISTING.

       DETAIL-LINE-PROCESS-EXIT.
           EXIT.

      ******************************************************************
       PRINT-DETAIL-LISTING SECTION.
      ******************************************************************

           ADD 1 TO  PAGE-LINE-NO.
           MOVE 99 TO  LN-FEED.

           PERFORM PRINT-MAIN-HEADER-NEW.

      * PAGE 01 --------------------------------------------------
           MOVE ALL "-"                  TO D03-BEG-LINES.
           MOVE "( ACCOUNT NO:"          TO D03-ACCT-TEXT.
           MOVE LP-ACCTNO                TO D03-ACCT-NO.
           MOVE "SEQ NO:"                TO D03-SEQ-TEXT.
           MOVE LP-SEQNO                 TO D03-SEQ-NO.
           MOVE "PAGE 01"                TO D03-PAGE.
           MOVE ")-----"                 TO D03-END-LINES.

           MOVE 02 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.


      *-----------------------------------------------------------
           MOVE " 1."                    TO D-COL-1-NUMBER.
           MOVE "TRANSACTION CODE"       TO D-COL-1-HEADING.
           MOVE ":"                      TO D-COLON-1.
           MOVE LP-TRCD                  TO D-COL-1-CODE.
           MOVE "18."                    TO D-COL-2-NUMBER.
           MOVE "TRANSACTION AMT "       TO D-COL-2-HEADING.
           MOVE ":"                      TO D-COLON-2.
           MOVE LP-TRAMT                 TO D-COL-2-AMOUNT.

           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *-----------------------------------------------------------
           MOVE " 2."                    TO D-COL-1-NUMBER.
           MOVE "TRANSACTION DATE"       TO D-COL-1-HEADING.
           MOVE ":"                      TO D-COLON-1.
           IF LP-PAYDATE = LP-TRDATE
              MOVE LP-TRDATE             TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY           TO D-COL-1-DATE.
           MOVE "19."                    TO D-COL-2-NUMBER.
           MOVE "APPLIED INT/LATE"       TO D-COL-2-HEADING.
           MOVE ":"                      TO D-COLON-2.
           MOVE LP-APINT                 TO D-COL-2-AMOUNT.
           MOVE LP-APLC                  TO D-COL-2-AMOUNT-2.

           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *-----------------------------------------------------------
           MOVE " 3."                    TO D-COL-1-NUMBER.
           MOVE "PAYMENT DATE"           TO D-COL-1-HEADING.
           MOVE ":"                      TO D-COLON-1.
           MOVE LP-PAYDATE               TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY              TO D-COL-1-DATE.
           MOVE "20."                    TO D-COL-2-NUMBER.
           MOVE "   ''   OTHER/2"        TO D-COL-2-HEADING.
           MOVE ":"                      TO D-COLON-2.
           MOVE LP-APOTH                 TO D-COL-2-AMOUNT.
           MOVE LP-APOT2                 TO D-COL-2-AMOUNT-2.

           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *-----------------------------------------------------------
           MOVE " 4."                    TO D-COL-1-NUMBER.
           MOVE "POSTED DATE/TIME"       TO D-COL-1-HEADING.
           MOVE ":"                      TO D-COLON-1.
           MOVE LP-POSTDATE              TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY              TO D-COL-1-DATE.
           MOVE LP-POSTTIME              TO D-COL-1-TIME.
           MOVE "21."                    TO D-COL-2-NUMBER.
           MOVE "   ''   CURBAL"         TO D-COL-2-HEADING.
           MOVE ":"                      TO D-COLON-2.
           MOVE LP-APCUR                 TO D-COL-2-AMOUNT.

           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *-----------------------------------------------------------
           MOVE " 5."                    TO D-COL-1-NUMBER.
           MOVE "POSTING BR/USER"        TO D-COL-1-HEADING.
           MOVE ":"                      TO D-COLON-1.
           MOVE LP-POSTING-BR            TO D-COL-1-POST-BR.
           MOVE LP-USERID                TO D-COL-1-USER.
           MOVE "22."                    TO D-COL-2-NUMBER.
           MOVE "BALANCE OTHER/2"        TO D-COL-2-HEADING.
           MOVE ":"                      TO D-COLON-2.
           MOVE LP-OTHBAL                TO D-COL-2-AMOUNT.
           MOVE LP-OT2BAL                TO D-COL-2-AMOUNT-2.

           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *-----------------------------------------------------------
           MOVE " 6."                    TO D-COL-1-NUMBER.
           MOVE "REFERENCE"              TO D-COL-1-HEADING.
           MOVE ":"                      TO D-COLON-1.
           MOVE LP-REFNO                 TO D-COL-1-REFER.
           MOVE "23."                    TO D-COL-2-NUMBER.
           MOVE "   ''   INT/LATE"       TO D-COL-2-HEADING.
           MOVE ":"                      TO D-COLON-2.
           MOVE LP-INTBAL                TO D-COL-2-AMOUNT.
           MOVE LP-LCBAL                 TO D-COL-2-AMOUNT-2.

           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *-----------------------------------------------------------
           MOVE " 7."                    TO D-COL-1-NUMBER.
           MOVE "REV?"                   TO D-COL-1-HEADING.
           MOVE ":"                      TO D-COLON-1.
           MOVE LP-REV                   TO D-COL-1-FG.
           MOVE "24."                    TO D-COL-2-NUMBER.
           MOVE "   ''   CURBAL"         TO D-COL-2-HEADING.
           MOVE ":"                      TO D-COLON-2.
           MOVE LP-CURBAL                TO D-COL-2-AMOUNT.

           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *-----------------------------------------------------------
           MOVE "TRCD"                   TO D-COL-1-HEADING.
           MOVE ":"                      TO D-COLON-1.
           MOVE LP-REVTRCD               TO D-COL-1-CODE.
           MOVE "25."                    TO D-COL-2-NUMBER.
           MOVE "EARNED INT/ADDON"       TO D-COL-2-HEADING.
           MOVE ":"                      TO D-COLON-2.
           MOVE LP-EARNED(1)             TO D-COL-2-AMOUNT.
           MOVE LP-EARNED(2)             TO D-COL-2-AMOUNT-2.

           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *-----------------------------------------------------------
           MOVE "SEQNO"                  TO D-COL-1-HEADING.
           MOVE ":"                      TO D-COLON-1.
           MOVE LP-REVSEQNO              TO D-COL-1-POST-BR.
           MOVE "26."                    TO D-COL-2-NUMBER.
           MOVE "   ''  SERV/MFEE"       TO D-COL-2-HEADING.
           MOVE ":"                      TO D-COLON-2.
           MOVE LP-EARNED(3)             TO D-COL-2-AMOUNT.
           MOVE LXE-EARN(1)              TO D-COL-2-AMOUNT-2.

           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *-----------------------------------------------------------
           MOVE " 8."                    TO D-COL-1-NUMBER.
           MOVE "COLLECTOR"              TO D-COL-1-HEADING.
           MOVE ":"                      TO D-COLON-1.
           MOVE LP-COLLECTOR             TO D-COL-1-REFER.
           MOVE "27."                    TO D-COL-2-NUMBER.
           MOVE "   ''  DISC/OTHR"       TO D-COL-2-HEADING.
           MOVE ":"                      TO D-COLON-2.
           MOVE LXE-EARN(2)             TO D-COL-2-AMOUNT.
           MOVE LXE-EARN(3)             TO D-COL-2-AMOUNT-2.

           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *-----------------------------------------------------------
           MOVE " 9."                    TO D-COL-1-NUMBER.
           MOVE "STATUS FG/DRAWER"       TO D-COL-1-HEADING.
           MOVE ":"                      TO D-COLON-1.
           MOVE LP-STATUSFG              TO D-COL-1-FG.
           MOVE LP-TILLNO                TO D-COL-1-FG-2.
           MOVE "28."                    TO D-COL-2-NUMBER.
           MOVE "INTEREST PAID"          TO D-COL-2-HEADING.
           MOVE ":"                      TO D-COLON-2.
           MOVE LP-INTPAID               TO D-COL-2-AMOUNT.

           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *-----------------------------------------------------------
           MOVE "10."                    TO D-COL-1-NUMBER.
           MOVE "INTBEAR/PRECOMP"        TO D-COL-1-HEADING.
           MOVE ":"                      TO D-COLON-1.
           MOVE LP-IBPC                  TO D-COL-1-FG.
           MOVE "29."                    TO D-COL-2-NUMBER.
           MOVE "   ''    WK DUE"        TO D-COL-2-HEADING.
           MOVE ":"                      TO D-COLON-2.
           MOVE LP-INTDUE                TO D-COL-2-AMOUNT.

           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *-----------------------------------------------------------
           MOVE "11."                    TO D-COL-1-NUMBER.
           MOVE "MN REBATE/INSTYP"       TO D-COL-1-HEADING.
           MOVE ":"                      TO D-COLON-1.
           MOVE LP-MANUAL-RB             TO D-COL-1-FG.
           MOVE "30."                    TO D-COL-2-NUMBER.
           MOVE "   ''    WK DUE"        TO D-COL-2-HEADING.
           MOVE ":"                      TO D-COLON-2.
           IF LP-TRCD = "SP"
              MOVE LP-PRIOR-POOLID       TO D-COL-2-AMOUNT
           ELSE
              IF LP-TRCD = "RV" AND LP-REFNO = "SP   "
                 MOVE LP-PRIOR-POOLID    TO D-COL-2-AMOUNT
              ELSE
                 MOVE LP-APINTOWE        TO D-COL-2-AMOUNT.

           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *-----------------------------------------------------------
           MOVE "12."                    TO D-COL-1-NUMBER.
           MOVE "PAID THRU"              TO D-COL-1-HEADING.
           MOVE ":"                      TO D-COLON-1.
           MOVE LP-PDTH-DATE             TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY              TO D-COL-1-DATE.
           MOVE "31."                    TO D-COL-2-NUMBER.
           MOVE "NEW STATUS RATE"        TO D-COL-2-HEADING.
           MOVE ":"                      TO D-COLON-2.
           MOVE LP-NEWSTAT-RATE          TO D-COL-2-AMOUNT.

           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *-----------------------------------------------------------
           MOVE "LCPDTH"                 TO D-COL-1-HEADING.
           MOVE ":"                      TO D-COLON-1.
           MOVE LP-LCPDTH-DATE           TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY              TO D-COL-1-DATE.
           MOVE "32."                    TO D-COL-2-NUMBER.
           MOVE "LC WORKER #1/#2"        TO D-COL-2-HEADING.
           MOVE ":"                      TO D-COLON-2.
           MOVE LP-LCPARTIALS               TO D-COL-2-AMOUNT.
           MOVE LP-LCPAID               TO D-COL-2-AMOUNT-2.

           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *-----------------------------------------------------------
           MOVE "13."                    TO D-COL-1-NUMBER.
           MOVE "NO. LCHG COLLECT"       TO D-COL-1-HEADING.
           MOVE ":"                      TO D-COLON-1.
           MOVE LP-NOLCHG                TO D-COL-1-POST-BR.
           MOVE "33."                    TO D-COL-2-NUMBER.
           MOVE "DEALER PROCEEDS"        TO D-COL-2-HEADING.
           MOVE ":"                      TO D-COLON-2.
           MOVE LP-DLPROC                TO D-COL-2-AMOUNT.

           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *-----------------------------------------------------------
           MOVE "14."                    TO D-COL-1-NUMBER.
           MOVE "BORROWER SSNO"          TO D-COL-1-HEADING.
           MOVE ":"                      TO D-COLON-1.
           MOVE LP-SSNO                  TO D-COL-1-SSNO.
           INSPECT D-COL-1-SSNO REPLACING ALL "/" BY "-".
           MOVE "34."                    TO D-COL-2-NUMBER.
           MOVE "DLPARV HELD/PAID"       TO D-COL-2-HEADING.
           MOVE ":"                      TO D-COLON-2.
           IF LP-TRCD = "ET" OR "RT" OR "LP" OR "NP" OR "ST"
              MOVE LP-LT-SEQNO           TO D-COL-2-AMOUNT
           ELSE
              MOVE LP-REFNO TO LPWSLP-LP-REFNO
              IF LP-TRCD = "RV" AND (LPWSLP-LP-ADDON-TEXT = "ADD")
                 MOVE LP-LT-SEQNO        TO D-COL-2-AMOUNT
              ELSE
                 MOVE LP-DLPARVHELD-ADJ  TO D-COL-2-AMOUNT.
           MOVE LP-DLPARVPAID-ADJ        TO D-COL-2-AMOUNT.

           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *-----------------------------------------------------------
           MOVE "35."                    TO D-COL-2-NUMBER.
           MOVE "DLRSV ADJ #1"           TO D-COL-2-HEADING.
           MOVE ":"                      TO D-COLON-2.
           MOVE LP-DLRVCHGS-ADJ(1)       TO D-COL-2-AMOUNT.

           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *-----------------------------------------------------------
           MOVE "15."                    TO D-COL-1-NUMBER.
           MOVE "GLNO1:"                 TO D-COL-1-GLNO-HEAD.
           MOVE LXG-GLNO(1)             TO D-COL-1-GLNO-NUM.
           INSPECT D-COL-1-GLNO-NUM REPLACING ALL "/" BY "-".
           MOVE "AMT:"                   TO D-COL-1-AMT-HEAD.
           MOVE LXG-GLAMT(1)            TO D-COL-1-AMT-NUM.
           MOVE "DLRSV ADJ #2"           TO D-COL-2-HEADING.
           MOVE ":"                      TO D-COLON-2.
           MOVE LP-DLRVCHGS-ADJ(2)       TO D-COL-2-AMOUNT.

           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *-----------------------------------------------------------
           MOVE "16."                    TO D-COL-1-NUMBER.
           MOVE "GLNO2:"                 TO D-COL-1-GLNO-HEAD.
           MOVE LXG-GLNO(2)             TO D-COL-1-GLNO-NUM.
           INSPECT D-COL-1-GLNO-NUM REPLACING ALL "/" BY "-".
           MOVE "AMT:"                   TO D-COL-1-AMT-HEAD.
           MOVE LXG-GLAMT(2)            TO D-COL-1-AMT-NUM.
           MOVE "DLRSV ADJ #3"           TO D-COL-2-HEADING.
           MOVE ":"                      TO D-COLON-2.
           MOVE LP-DLRVCHGS-ADJ(3)       TO D-COL-2-AMOUNT.

           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *-----------------------------------------------------------
           MOVE "17."                    TO D-COL-1-NUMBER.
           MOVE "GLNO3:"                 TO D-COL-1-GLNO-HEAD.
           MOVE LXG-GLNO(3)             TO D-COL-1-GLNO-NUM.
           INSPECT D-COL-1-GLNO-NUM REPLACING ALL "/" BY "-".
           MOVE "AMT:"                   TO D-COL-1-AMT-HEAD.
           MOVE LXG-GLAMT(3)            TO D-COL-1-AMT-NUM.
           MOVE "36."                    TO D-COL-2-NUMBER.
           MOVE "IB EARN WKS 1&2"        TO D-COL-2-HEADING.
           MOVE ":"                      TO D-COLON-2.
           MOVE LXE-WORKER              TO D-COL-2-AMOUNT.
           MOVE LXE-WORKER2             TO D-COL-2-AMOUNT-2.

           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * PAGE 02 - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE ALL "-"                  TO D03-BEG-LINES.
           MOVE "( ACCOUNT NO:"          TO D03-ACCT-TEXT.
           MOVE LP-ACCTNO                TO D03-ACCT-NO.
           MOVE "SEQ NO:"                TO D03-SEQ-TEXT.
           MOVE LP-SEQNO                 TO D03-SEQ-NO.
           MOVE "PAGE 02"                TO D03-PAGE.
           MOVE ")-----"                 TO D03-END-LINES.

           MOVE 02 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *-----------------------------------------------------------
           MOVE " 1."                    TO D-COL-1-NUMBER.
           MOVE "PREPAY PENALTY"         TO D-COL-1-HEADING.
           MOVE ":"                      TO D-COLON-1.
           MOVE LP-PREPAY-PENALTY              TO D-2-AMOUNT.
           MOVE "20."                    TO D-COL-2-NUMBER.
           MOVE "SOURCE CODE"            TO D-COL-2-HEADING.
           MOVE ":"                      TO D-COLON-2.
           MOVE LXA-SRCD                TO D-COL-2-FG-CODE.

           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *-----------------------------------------------------------
           MOVE " 2."                    TO D-COL-1-NUMBER.
           MOVE "EXTRA PRIN LFP"         TO D-COL-1-HEADING.
           MOVE ":"                      TO D-COLON-1.
           MOVE LP-EXTRA-PRIN-LFP        TO D-2-AMOUNT.
           MOVE "21."                    TO D-COL-2-NUMBER.
           MOVE "START PYMT (1~3)"       TO D-COL-2-HEADING.
           MOVE ":"                      TO D-COLON-2.
           MOVE LXA-START-PMT(1)        TO D-COL-2-PYMNT-1.
           MOVE LXA-START-PMT(2)        TO D-COL-2-PYMNT-2.
           MOVE LXA-START-PMT(3)        TO D-COL-2-PYMNT-3.

           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *-----------------------------------------------------------
           MOVE " 3."                    TO D-COL-1-NUMBER.
           MOVE "INT  SINCE LFP"         TO D-COL-1-HEADING.
           MOVE ":"                      TO D-COLON-1.
           MOVE LP-REMAIN-INT-SINCE-LFP  TO D-2-AMOUNT.
           MOVE "22."                    TO D-COL-2-NUMBER.
           MOVE "END   PYMT (1~3)"       TO D-COL-2-HEADING.
           MOVE ":"                      TO D-COLON-2.
           MOVE LXA-END-PMT(1)          TO D-COL-2-PYMNT-1.
           MOVE LXA-END-PMT(2)          TO D-COL-2-PYMNT-2.
           MOVE LXA-END-PMT(3)          TO D-COL-2-PYMNT-3.

           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *-----------------------------------------------------------
           MOVE " 4."                    TO D-COL-1-NUMBER.
           MOVE "PRIN SINCE LFP"         TO D-COL-1-HEADING.
           MOVE ":"                      TO D-COLON-1.
           MOVE LP-REMAIN-PRIN-SINCE-LFP TO D-2-AMOUNT.
           MOVE "23."                    TO D-COL-2-NUMBER.
           MOVE "NET CHANGE (1~3)"       TO D-COL-2-HEADING.
           MOVE ":"                      TO D-COLON-2.
           MOVE LXA-NET-CHANGE(1)       TO D-COL-2-AMOUNT.
           MOVE LXA-NET-CHANGE(2)       TO D-COL-2-AMOUNT-2.

           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *-----------------------------------------------------------
           MOVE " 5."                    TO D-COL-1-NUMBER.
           MOVE "LXZ POST REGPY"         TO D-COL-1-HEADING.
           MOVE ":"                      TO D-COLON-1.
           MOVE LXZ-POST-REGPY          TO D-2-AMOUNT.

           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *-----------------------------------------------------------
           MOVE " 6."                    TO D-COL-1-NUMBER.
           MOVE "LXZ POST EXTRAPR"       TO D-COL-1-HEADING.
           MOVE ":"                      TO D-COLON-1.
           MOVE LXZ-POST-EXTRA-PRIN     TO D-2-AMOUNT.

           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *-----------------------------------------------------------
           MOVE " 7."                    TO D-COL-1-NUMBER.
           MOVE "LXZ POST OTH"           TO D-COL-1-HEADING.
           MOVE ":"                      TO D-COLON-1.
           MOVE LXZ-POST-OTH            TO D-2-AMOUNT.
           MOVE "24."                    TO D-COL-2-NUMBER.
           MOVE "COMPANY NUMBER"         TO D-COL-2-HEADING.
           MOVE ":"                      TO D-COLON-2.
           MOVE LXA-LN-INSCOMP          TO D-COL-2-PYMNT-1.

           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *-----------------------------------------------------------
           MOVE " 8."                    TO D-COL-1-NUMBER.
           MOVE "LXZ POST LCHG"          TO D-COL-1-HEADING.
           MOVE ":"                      TO D-COLON-1.
           MOVE LXZ-POST-LCHG           TO D-2-AMOUNT.
           MOVE "25."                    TO D-COL-2-NUMBER.
           MOVE "OURS      (Y/N)"        TO D-COL-2-HEADING.
           MOVE ":"                      TO D-COLON-2.
           MOVE LXA-LN-INSOURS          TO D-COL-2-FG-CODE.

           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *-----------------------------------------------------------
           MOVE " 9."                    TO D-COL-1-NUMBER.
           MOVE "LXZ PDUE APINT"         TO D-COL-1-HEADING.
           MOVE ":"                      TO D-COLON-1.
           MOVE LXZ-PDUE-APINT          TO D-2-AMOUNT.
           MOVE "26."                    TO D-COL-2-NUMBER.
           MOVE "EFFECTIVE DATE"         TO D-COL-2-HEADING.
           MOVE ":"                      TO D-COLON-2.
           MOVE LXA-LN-INSEFF-DATE      TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY              TO D-COL-2-DATE.

           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *-----------------------------------------------------------
           MOVE "10."                    TO D-COL-1-NUMBER.
           MOVE "LXZ PDUE APPRIN"        TO D-COL-1-HEADING.
           MOVE ":"                      TO D-COLON-1.
           MOVE LXZ-PDUE-APPRIN         TO D-2-AMOUNT.
           MOVE "27."                    TO D-COL-2-NUMBER.
           MOVE "EXPIRATION DATE"        TO D-COL-2-HEADING.
           MOVE ":"                      TO D-COLON-2.
           MOVE LXA-LN-INSEXP-DATE      TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY              TO D-COL-2-DATE.

           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *-----------------------------------------------------------
           MOVE "11."                    TO D-COL-1-NUMBER.
           MOVE "LXZ CUR APINT"          TO D-COL-1-HEADING.
           MOVE ":"                      TO D-COLON-1.
           MOVE LXZ-CUR-APINT           TO D-2-AMOUNT.
           MOVE "28."                    TO D-COL-2-NUMBER.
           MOVE "PREMIUM"                TO D-COL-2-HEADING.
           MOVE ":"                      TO D-COLON-2.
           MOVE LXA-LN-INSPREM          TO D-COL-2-AMOUNT.

           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *-----------------------------------------------------------
           MOVE "12."                    TO D-COL-1-NUMBER.
           MOVE "LXZ CUR APPRIN"         TO D-COL-1-HEADING.
           MOVE ":"                      TO D-COLON-1.
           MOVE LXZ-CUR-APPRIN          TO D-2-AMOUNT.
           MOVE "29."                    TO D-COL-2-NUMBER.
           MOVE "COVERAGE"               TO D-COL-2-HEADING.
           MOVE ":"                      TO D-COLON-2.
           MOVE LXA-LN-INSCOVR          TO D-COL-2-AMOUNT.

           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *-----------------------------------------------------------
           MOVE "13."                    TO D-COL-1-NUMBER.
           MOVE "INS TAX REB AMT"        TO D-COL-1-HEADING.
           MOVE ":"                      TO D-COLON-1.
           MOVE LP-PRORATED-INS-REB              TO D-2-AMOUNT.
           MOVE "30."                    TO D-COL-2-NUMBER.
           MOVE "COMMISSION"             TO D-COL-2-HEADING.
           MOVE ":"                      TO D-COLON-2.
           MOVE LXA-LN-INSCOMM          TO D-COL-2-AMOUNT.

           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *-----------------------------------------------------------
           MOVE "14."                    TO D-COL-1-NUMBER.
           MOVE "LXM MIP CL PAID"        TO D-COL-1-HEADING.
           MOVE ":"                      TO D-COLON-1.
           MOVE LXM-MIP-CLPAID          TO D-2-AMOUNT.
           MOVE "31."                    TO D-COL-2-NUMBER.
           MOVE "REBATE"                 TO D-COL-2-HEADING.
           MOVE ":"                      TO D-COLON-2.
           MOVE LXA-LN-INS-REBATE       TO D-COL-2-FG-CODE.

           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *-----------------------------------------------------------
           MOVE "15."                    TO D-COL-1-NUMBER.
           MOVE "LXM MIP CL PDTH"        TO D-COL-1-HEADING.
           MOVE ":"                      TO D-COLON-1.
           MOVE LXM-MIP-CLPDTH-DATE     TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY              TO D-COL-1-DATE.
           MOVE "32."                    TO D-COL-2-NUMBER.
           MOVE "INSURANCE CODES"        TO D-COL-2-HEADING.
           MOVE ":"                      TO D-COLON-2.
           MOVE LXA-LN-INSCODES         TO D-COL-2-FG-CODE.

           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *-----------------------------------------------------------
           MOVE "16."                    TO D-COL-1-NUMBER.
           MOVE "LXM MIP AH PAID"        TO D-COL-1-HEADING.
           MOVE ":"                      TO D-COLON-1.
           MOVE LXM-MIP-AHPAID          TO D-2-AMOUNT.


           MOVE "33."                    TO D-COL-2-NUMBER.
           MOVE "ESCROW AP/PREPD"        TO D-COL-2-HEADING.
           MOVE ":"                      TO D-COLON-2.
           MOVE LP-APPLIED-TO-ESCROW     TO D-COL-2-AMOUNT.

           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *-----------------------------------------------------------
           MOVE "17."                    TO D-COL-1-NUMBER.
           MOVE "LXM MIP AH PDTH"        TO D-COL-1-HEADING.
           MOVE ":"                      TO D-COLON-1.
           MOVE LXM-MIP-AHPDTH-DATE     TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY              TO D-COL-1-DATE.

           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *-----------------------------------------------------------
           MOVE "18."                    TO D-COL-1-NUMBER.
           MOVE "N/A"                    TO D-COL-1-HEADING.
           MOVE ":"                      TO D-COLON-1.

           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *-----------------------------------------------------------
           MOVE "19."                    TO D-COL-1-NUMBER.
           MOVE "N/A"                    TO D-COL-1-HEADING.
           MOVE ":"                      TO D-COLON-1.

           MOVE 01 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.


      ******************************************************************
       PRINT-SUMMARY-LISTING SECTION.
      ******************************************************************

           ADD  1  TO  PAGE-LINE-NO.
           MOVE 1  TO  LN-FEED.

           IF PAGE-LINE-NO > 8 OR LP-ACCTNO NOT = HOLD-ACCTNO
              MOVE LP-ACCTNO TO HOLD-ACCTNO
              MOVE 1  TO  PAGE-LINE-NO
              PERFORM PRINT-MAIN-HEADER-OLD.

           PERFORM FILL-ACTION-LINES.
           PERFORM WRITE-ACTION-LINES.

           GO TO EXIT-PRINT-SUMMARY-LISTING.

      *-----------------------------------------------------------
       FILL-ACTION-LINES.
           IF LP-REV = "Y"
              MOVE "*"          TO   A1-REVERSED
           ELSE
              MOVE SPACES       TO   A1-REVERSED.

           PERFORM CHECK-INTBEAR-PRECOMP.
           PERFORM CHECK-TRANS-DATE.

           MOVE LP-SEQNO        TO   A1-SEQNO.
           MOVE LP-PAYDATE      TO   DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY     TO   A1-PAYDATE.
           MOVE LP-TRCD         TO   A1-TRCD.
           MOVE LP-REFNO        TO   A1-REFNO.
           MOVE LP-TRAMT        TO   A1-TR-AMT.
           MOVE LP-APOTH        TO   A1-APP-OTHER.
           MOVE LP-APCUR        TO   A1-APP-PRINC.
           MOVE LP-PDTH-DATE    TO   DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY     TO   A1-PDTHRU.
           MOVE LP-LCPDTH-DATE  TO   DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY     TO   A1-LC-PDTHRU.
           MOVE LP-OTHBAL       TO   A1-OW-OTHER.
           MOVE LP-CURBAL       TO   A1-OW-BALANCE.
           MOVE LP-TILLNO       TO   A1-TILL-NO.
           MOVE LP-APINTOWE     TO   A1-LC-DUE.
           MOVE LP-INTDUE       TO   A1-INT-DUE.
           MOVE LP-IBPC         TO   A1-IB-PC.
           MOVE LP-SSNO         TO   A1-SOC-SEC.

           MOVE LP-STATUSFG     TO   A3-STAT-FG.
           MOVE LP-DLPARVHELD-ADJ   TO   A3-DL-PART-ADJ.
           MOVE LP-EARNED(1)    TO   A3-IFU-INT.
           MOVE LP-EARNED(3)    TO   A3-IFU-ADDON.

           MOVE LP-NOLCHG       TO   A4-NOLCHG.
           MOVE LP-DLPROC       TO   A4-DL-PROCEED.
           MOVE LP-LCPARTIALS      TO   A4-LC-WORK-1.

           MOVE LP-NEWSTAT-RATE TO   A5-NW-STAT-RT.
           MOVE LP-INTPAID      TO   A5-INT-PAID.
           MOVE LP-LCPAID      TO   A5-LC-WORK-2.
           MOVE LP-EARNED(2)    TO   A5-IFU-DISCT.

      *-----------------------------------------------------------
       FILL-LXE-LINES.
           MOVE LXE-WORKER      TO   A4-EARN-WORK-1.
           MOVE LXE-EARN(1)     TO   A4-IFU-SERVICE.
           MOVE LXE-EARN(2)     TO   A4-IFU-MAINT.

           MOVE LXE-WORKER2     TO   A5-EARN-WORK-2.
           MOVE LXE-EARN(3)     TO   A5-IFU-OTHER.

      *-----------------------------------------------------------
       FILL-LXG-LINES.
           MOVE LXG-GLNO(1)     TO   A3-GL-ACCT.
           INSPECT A3-GL-ACCT REPLACING ALL "/" BY "-".
           MOVE LXG-GLAMT(1)    TO   A3-GL-AMOUNT.

           MOVE LXG-GLNO(2)     TO   A4-GL-ACCT.
           INSPECT A4-GL-ACCT REPLACING ALL "/" BY "-".
           MOVE LXG-GLAMT(2)    TO   A4-GL-AMOUNT.

           MOVE LXG-GLNO(3)     TO   A5-GL-ACCT.
           INSPECT A5-GL-ACCT REPLACING ALL "/" BY "-".
           MOVE LXG-GLAMT(3)    TO   A5-GL-AMOUNT.

      *-----------------------------------------------------------
       WRITE-ACTION-LINES.
           MOVE 1         TO  LN-FEED.

           MOVE ACTION1   TO  DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           IF ACTION2 NOT = " "
              MOVE ACTION2   TO  DETAIL-LINE
              PERFORM WRITE-DETAIL-LINE.

           MOVE " " TO DETAIL-LINE.
           MOVE ALL "-" TO SUB-LINE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE ACTION3   TO  DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE ACTION4   TO  DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE ACTION5   TO  DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE ALL "-"   TO  DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           PERFORM CLEAR-ACTION-LINES.

      *-----------------------------------------------------------
       CLEAR-ACTION-LINES.
           MOVE SPACES  TO   ACTION1.
           MOVE SPACES  TO   ACTION2.
           MOVE SPACES  TO   LP-STATUSFG.
           MOVE ZERO    TO   A1-SEQNO.
           MOVE ZERO    TO   A1-PAYDATE.
           MOVE ZERO    TO   A1-TR-AMT.
           MOVE ZERO    TO   A1-APP-OTHER.
           MOVE ZERO    TO   A1-APP-LC-INT.
           MOVE ZERO    TO   A1-APP-PRINC.
           MOVE ZERO    TO   A1-PDTHRU.
           MOVE ZERO    TO   A1-LC-PDTHRU.
           MOVE ZERO    TO   A1-OW-OTHER.
           MOVE ZERO    TO   A1-OW-LC-INT.
           MOVE ZERO    TO   A1-OW-BALANCE.
           MOVE SPACES  TO   A1-TILL-NO.
           MOVE ZERO    TO   A1-LC-DUE.
           MOVE ZERO    TO   A1-INT-DUE.
           MOVE ZERO    TO   A1-SOC-SEC.
           MOVE ZERO    TO   A2-TR-DATE.
           MOVE ZERO    TO   A2-APP-LC-INT.
           MOVE ZERO    TO   A2-OW-LC-INT.
           MOVE ZERO    TO   A3-DL-PART-ADJ.
           MOVE ZERO    TO   A3-IFU-INT.
           MOVE ZERO    TO   A3-IFU-ADDON.
           MOVE ZERO    TO   A4-NOLCHG.
           MOVE ZERO    TO   A4-DL-PROCEED.
           MOVE ZERO    TO   A4-LC-WORK-1.
           MOVE ZERO    TO   A5-NW-STAT-RT.
           MOVE ZERO    TO   A5-INT-PAID.
           MOVE ZERO    TO   A5-LC-WORK-2.
           MOVE ZERO    TO   A5-IFU-DISCT.
           MOVE ZERO    TO   A4-IFU-SERVICE.
           MOVE ZERO    TO   A4-IFU-MAINT.
           MOVE ZERO    TO   A5-IFU-OTHER.
           MOVE ZERO    TO   A3-GL-ACCT.
           MOVE ZERO    TO   A3-GL-AMOUNT.
           MOVE ZERO    TO   A4-GL-ACCT.
           MOVE ZERO    TO   A4-GL-AMOUNT.
           MOVE ZERO    TO   A5-GL-ACCT.
           MOVE ZERO    TO   A5-GL-AMOUNT.

       EXIT-PRINT-SUMMARY-LISTING.
           EXIT.

      *********************************************************
       WRITE-DETAIL-LINE SECTION.
      *********************************************************
           MOVE DETAIL-LINE  TO  PR-REC.
           PERFORM WRITE-PR-REC.
           MOVE 1            TO  LN-FEED.
           MOVE SPACES       TO  DETAIL-LINE.

      *********************************************************
       PRINT-MAIN-HEADER-NEW SECTION.
      *********************************************************
           MOVE "N"         TO  FIRST-TIME.
           ADD 1            TO  PAGE-NUMBER.
           MOVE PAGE-NUMBER TO  H-PAGE-NO.

           PERFORM GET-TIME.
           MOVE T-HH        TO  H-HOUR.
           MOVE T-MM        TO  H-MIN.

           MOVE HEAD1       TO  PR-REC.
           MOVE 99          TO  LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE LN-OWNBR    TO  H-BRNO.
           MOVE BR-NAME     TO  H-BRDESC.
           MOVE HEAD2       TO  PR-REC.
           MOVE 1           TO  LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE ALL "-"     TO  PR-REC.
           PERFORM WRITE-PR-REC.

           MOVE 1 TO LN-FEED.

      *********************************************************
       PRINT-MAIN-HEADER-OLD SECTION.
      *********************************************************
           MOVE "N"         TO  FIRST-TIME.
           ADD 1            TO  PAGE-NUMBER.
           MOVE PAGE-NUMBER TO  H-PAGE-NO.

           PERFORM GET-TIME.
           MOVE T-HH        TO  H-HOUR.
           MOVE T-MM        TO  H-MIN.

           MOVE HEAD1       TO  PR-REC.
           MOVE 99          TO  LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE LN-OWNBR    TO  H-BRNO.
           MOVE BR-NAME     TO  H-BRDESC.
           MOVE HEAD2       TO  PR-REC.
           MOVE 1           TO  LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE SPACES TO H-BWNAME.
           STRING BW-LNAME,", ",BW-FNAME DELIMITED BY "   "
                INTO H-BWNAME.

           PERFORM LPSTAT-DISPLAY.
           MOVE LPS-CLASS      TO  H-CLASS.
           MOVE LPS-STATFLAGS  TO  H-STAT-FG.
           MOVE LP-ACCTNO      TO  H-ACCTNO.
           MOVE LP-SEQNO       TO  H-SEQ-NO.
           MOVE HEAD3          TO  PR-REC.
           MOVE 1              TO  LN-FEED.
           PERFORM WRITE-PR-REC.

           IF GP-ESCROW-FG = "Y"
              MOVE "TR-AMT" TO H4-TRAMT
           ELSE
              MOVE SPACES   TO H4-TRAMT.

           MOVE HEAD4       TO  PR-REC.
           MOVE 1           TO  LN-FEED.
           PERFORM WRITE-PR-REC.

           IF GP-ESCROW-FG = "Y"
              MOVE "ESCROW " TO H5-TRAMT-OR-ESCROW
           ELSE
              MOVE "TR-AMT"  TO H5-TRAMT-OR-ESCROW.

           MOVE HEAD5       TO  PR-REC.
           MOVE 1           TO  LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE ALL "-"     TO  PR-REC.
           PERFORM WRITE-PR-REC.

           MOVE 1 TO LN-FEED.

      *********************************************************
       PRINT-RANGE-LINES SECTION.
      *********************************************************
           MOVE 2                        TO  LN-FEED.
           MOVE "BEGINNING LOAN NUMBER"  TO  RANGE-LINE.
           MOVE BEG-LP                   TO  RANGE-SELECTION.

           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING LOAN NUMBER"     TO  RANGE-LINE.
           MOVE END-LP                   TO  RANGE-SELECTION.

           PERFORM WRITE-DETAIL-LINE.

      *********************************************************
       BWLN-SCAN SECTION.
      ********************************************************
           MOVE 0                  TO BWLNSC-ACCTNO.
           MOVE EXT-CONAME-SYSDATE TO BWLNSC-SYSDATE.
           MOVE "WI/BWLNSC " TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH BWLNSC-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF BWLNSC-STATUS NOT = "00" OR BWLNSC-ACCTNO = 0
              MOVE 9 TO IO-FG
              GO TO EXIT-BWLN-SCAN.

           IF ( LOAN-FG = "BEG" )
               MOVE "SNNN060LOANNO1." TO VDU
           ELSE
               MOVE "SNNN060LOANNO2." TO VDU.

           MOVE BWLNSC-ACCTNO TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE BWLNSC-ACCTNO TO VDUNUM0.
           MOVE 0 TO IO-FG.

       EXIT-BWLN-SCAN.
           EXIT.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBLP/LPSTAT.CPY".
       COPY "LIBLP/LPLXCL.CPY".

      ****************************************************
       FORM-RESET SECTION.
      ****************************************************
           MOVE EXT-CONAME-CONAME TO LPLIST-CONAME.
           MOVE EXT-CONAME-SYSDATE TO LPLIST-SYSDATE.
           DISPLAY LPLIST-SCREEN UPON LPLIST-WINDOW-HANDLE.
           MOVE LPLIST-FORM-FIELDS TO WR-BUF.
           MOVE LPLIST-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ****************************************************
       SETUP-LINK-INFO SECTION.
      ****************************************************
           MOVE LPLIST-HELP--FILE TO HELP-LINK-FILE.
           MOVE LPLIST-HELP--DIR  TO HELP-LINK-DIR.

      ****************************************************
       VDU-CONVERT-FIELD SECTION.
      ****************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLS/LPLIST_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *********************************************************
       MISC SECTION.
      *********************************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/ACCESS.CPY".
       GET-BRANCH-DESCRIPTION.
           MOVE LN-OWNBR TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-FG NOT = 0
              MOVE "*** NOT A FILE ***" TO BR-NAME.
      *--------------------------------------------------------
       OPEN-START-FILES.
           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-BW1-FILE.
           PERFORM OPEN-LN1-FILE.
           PERFORM OPEN-LP1-FILE.
           PERFORM OPEN-LX1-FILE.
           PERFORM OPEN-LXE1-FILE.
           PERFORM OPEN-LXG1-FILE.
           PERFORM OPEN-PR-FILE.

           MOVE LN-PATH-OWNBR TO LN-OWNBR.

           MOVE LP-PATH-OWNBR TO LP-BRNO
                                 QLP1-WBEG-BRNO
                                 QLP1-WEND-BRNO.
           MOVE BEG-LP        TO LP-ACCTNO
                                 QLP1-WBEG-ACCTNO
           MOVE END-LP        TO QLP1-WEND-ACCTNO.
           MOVE 0             TO LP-SEQNO
                                 QLP1-WBEG-SEQNO.
           MOVE ALL "9"       TO QLP1-WEND-SEQNO.

           PERFORM START-LP1-FILE.

      *-----------------------------------------------------------
       CHECK-INTBEAR-PRECOMP.
           IF LP-IBPC = "I"
              MOVE LP-APINT        TO   A1-APP-LC-INT
              MOVE LP-INTBAL       TO   A1-OW-LC-INT
              MOVE LP-APLC         TO   A2-APP-LC-INT
              MOVE LP-LCBAL        TO   A2-OW-LC-INT
              MOVE LP-APPLIED-TO-ESCROW
                                   TO   A2-APP-ESCROW
           ELSE
              MOVE LP-APINT        TO   A2-APP-LC-INT
              MOVE LP-INTBAL       TO   A2-OW-LC-INT
              MOVE LP-APLC         TO   A1-APP-LC-INT
              MOVE LP-LCBAL        TO   A1-OW-LC-INT.

      *-----------------------------------------------------------
       CHECK-TRANS-DATE.
           IF LP-PAYDATE = LP-TRDATE
              MOVE SPACES          TO   A2-TR-DATE
           ELSE
              MOVE LP-TRDATE       TO   DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
              MOVE DATE-MMDDYY     TO   A2-TR-DATE.

      *********************************************************
       IO-ROUTINE SECTION.
      *********************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLXGGS_SQL.CPY".
       COPY "LIBLP/LPLXEGS_SQL.CPY".
       COPY "LIBLP/LPLXGS_SQL.CPY".
       COPY "LIBLP/LPLPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBLP/LPLP1RN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPLX1RN.CPY".
       COPY "LIBLP/LPLXE1RN.CPY".
       COPY "LIBLP/LPLXG1RN.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      *********************************************************
       END-PROGRAM SECTION.
      *********************************************************
       END-RUN.
           PERFORM CLOSE-FILES.
           PERFORM PRU-OUT.
           IF NO-RECORDS-FOUND
              MOVE "NO RECORDS FOUND FOR SELECTED RANGE!" TO MESS
              PERFORM SEND-MESS.

       EXIT-PROG.
           MOVE "LP/LPL2MU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY LPLIST-SCREEN.
           DESTROY LPLIST-WINDOW-HANDLE.
           EXIT PROGRAM.
