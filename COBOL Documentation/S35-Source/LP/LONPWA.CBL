       IDENTIFICATION  DIVISION.
       PROGRAM-ID.       LONPWA.
       DATE-WRITTEN. 11/01/2002.
      ******************************************************************
      *  DESC:  LOAN PROCESSING
      *         BATCH PAYMENT OTHER CHARGE REFERENCE CODES PROCESSING
      *
      *  ???LN-LBOX FLAG MUST BE "Y"
      *
      *  AN EXCEPTION REPORT (CONTAINING BOTH SUCCESSFULLY AND UNSUCCESSFULLY
      *  UPDATED ACCOUNTS) WILL BE PUT IN THE 'EO' DIRECTORY WITH THE BATCH
      *  NAME OF 'OTH' 
      *
      *  NOTE: IN ORDER FOR THIS REPORT TO BE VIEWED USING BATCH REPORT
      *        VIEWER, A DUMMY BATCH MUST BE CREATED IN EOMAIN WITH
      *        THE NAME 'OTH' AND CONTAINING NO STEPS.
      *
      *   THIS PROGRAM CALLS LONPF7(OT) AND LONPF2 TO POST THE PAYMENTS
      *
      *   AT END OF EACH BRANCHES UPDATE THE BP FILE WILL BE DELETED
      *   REGARDLESS OF WHETHER OR NOT UPDATES WERE SUCCESSFUL.
      *   AS PER JIM ROSENAUR, ALL EXCEPTIONS, FOR ANY REASON WILL
      *   BE POSTED MANUALLY FROM THE EXCEPTION REPORT.
      *
      ******************************************************************
      * REV:
      * JTG 021011 ADDED NEW CODE FILE TYPE 'BR' - BATCH PAYMENT REFERENCE CODES
      *            **     ** COPIED FROM LONPW6.C                   REGACC #0006
      * JTG 021220 ADDED POFF-LCAP-BATCH-REFCD POFF-LCAP-BATCH-REFCD-LC-FG
      *            ADDED NEW CODE FILE TYPE 'BR'-BATCH PAYMENT REFERENCE CODES
      *            BATCH PAYMENT REVISIONS                        REGACC #0006
      * JTG 040331 ADDED LOGIC FOR POSTING BATCH PAYMENTS TO ACCOUNT'S
      *            WITH 'LN-OTBAL'                                 CENTRIX #2253
      * JTG 040407 **     ** ADDED GP-OTREF-NEXT-BATCH  900-909    CENTRIX #2254
      * BAH 050628 DONT TEST BR-BP-MAXCON, BR-BP-MAXREC, BR-LBOX-MAXCON
      *            OR BR-LBOX-MAXREC IF THEY ARE SET TO 999, SHOULDNT
      *            EXCLUDE ANYTHING IF SET TO 999, REGACC PL# 501
      * BLF 051121 AFTER UPDATE, RECONSTRUCT PATHNAME OF GLOBAL BTFILE & MOVE
      *            IT TO DONE DIRECTORY (ROUTINE = MOVE-BTFILE).  HERE IS THE
      *            PROBLEM:  COMPANY GETS GLOBAL FILE (BTFILE, NOW MAY BE
      *            ACHPMT, CCFILE, ETC), PUTS IN /USR/DATA/R1/ED & RUNS
      *            PROGRAM TO BREAK OUT INTO LOCAL BPFILES (FOR EXAMPLE,
      *            /USR/DATA/R1/XX0102/ED/B01021121123ACH1).  SINCE WE NOW HAVE
      *            IN MANY CASES A CORPORATE BRANCH RUNNING UPDATE FOR MANY
      *            LOCAL BRANCHES AND CORPORATE BRANCH MAY NOT HAVE ANY
      *            ACCOUNTS SO NO LOCAL BATCH PAYMENT FILE, WHEN THEY GO INTO
      *            "SET BATCH TO PROCESS", NO LOCAL BPFILES DISPLAY AS
      *            BATCHES TO SET, SO WE DISPLAY THE GLOBAL FILES.  BUT IT
      *            KEEPS DISPLAYING EVEN AFTER THE BATCH PAYMENT UPDATE IS DONE.
      *            SO WHEN UPDATE IS DONE, MOVE THE GLOBAL FILE & IT WON'T
      *            DISPLAY IN SET BATCH TO PROCESS.  FOR EXAMPLE:
      *            MV ../../ED/CCFIL1121123 ../../ED/DONE/CCFIL1121123
      *            CENTRIX, PL# 480
      * BLF 070213 FIX SPOOL NAME - WAS SENDING REPORT TO FORCED SPOOL DIRECTORY
      *            OF RECMMDDYY INSTEAD OF OTHMMDDYY, SO BPOTH PUT REPORT IN
      *            OTH & THIS REPORT WAS IN ANOTHER DIRECTORY, WORLD PR# 526
      * BLF 070501 BARB & CINDY, CLEAR HOLD-OVPAID-CHECK IN INITIALIZATION;
      *            MAIN EXCEPTION WAS NOT ADDING IF AMT IN HOLD-OVPAID-CHECK &
      *            IT COULD HAVE HUGE UNINITIALIZED AMT IN IT, SO EXCEPTION
      *            TOTALS COULD BE COMPLETELY JUNK, UNITED
      * BLM 130308 IF BR-BP-ALLOW-PMT-ACCT-OTHBAL = "2" (OR BR-LBOX-ALLOW
      *            PMT-ACCT-OTHBAL = "2"), REJECT ACCTS WITH OTHER 2 BALANCE
      *            IF I-AM-SOAUTO, SOAUTO PR# 4145
      * BLM 130619 IF GPI-AM-SOAUTO, ALLOW MULTIPLE OT'S TO POST TO AN
      *            ACCOUNT, SOAUTO PR# 4311
      *  CS 140527 REPLACED MKDIR-CMD WITH C$MAKEDIR
      *  BAH 140926 MOVE "F" TO FULL-DISPLAY INSTEAD OF "P", AUTOMOTIVE PL#860
      *  BLM 141110 GET RID OF CHDIR & SET EXT-FILPATH INSTEAD.  DO SET 
      *             ENVIRONMENT ON FIL & POSTNAME & SET BRANCH.  RESTORE ALL
      *             UPON EXIT.
      *   CS 140127 VDU-FORM-NAME WAS SET TO LONPW9 SO WAS GETTING A MISSING
      *             FIELD ON ACCEPT, CHANGED TO LONPWA, ALSO ADDED A FORM
      *             RESET AS THE SCREEN WAS NOT DISPLAYED
      *
      * KEC 2015.1120 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         FIXED RC-TRANS-DATE FROM YYYMMDD TO CCYYMMDD.
      *
      *  BAH 160128 SPLIT OUT LTFILE, PD0003
      *   CS 160222 REMOVE COPY LPWKLXE, ITS AN EARNINGS COPY MEMBER THAT IS
      *             PASSED IN LOAN PROCESSING VIA LP01WK USING WK-LX2-REC. 
      *             REPLACED WK-LX2-REC WITH WK-LXE-REC IN LP01WK 
      *             MOVED LPWKLXE TO LPWKLXE.NO
      *             SAVED VERSION LONPWA.160222
      *  BAH 160304 CHANGED TILLNO TO NUMERIC 99
      *  BLM 161024 INITIALIZE BP-SEQNO
      *  BLM 161024 INITIALIZE HOLD-NFE-CK-NUMBER HOLD-PRIOR-DF-POSTDATE,
      *             WERE COMP-6, NOW COMP. PF2 CHECKING FOR 2020, SO
      *             WRITING NF & MEMO FOR BATCH PMTS, LEAVING 2020 IN
      *             TR-PRIOR-DF-POSTDATE
      * BAH 170705 ACTIVATE 8 DIGIT DATES, PD0006
      *
      * KEC 2017.0912 {PR#01246} WORLD <A30> [JAY CISZEWSKI]
      *          WHEN ADDING THIS PR# TO A30, I NOTICED THAT A COPY
      *          MEMBER WAS NOT PROPERLY SETUP.  TO FIX IT I DID:
      *          - COPIED LIBLP/LPBP.CPY TO ../SV/LPBP.NO
      *          - MOVED  LIBLP/LPBP.CPY TO LIBLP/LPBP_CMD.CPY
      *          - EDITED LIBLP/LPBP_CMD.CPY TO LOWERCASE WHEN NEEDED
      *
      * BAH 171129 FIXED WK-LXE-REC
      * BAH 181211 GLOBAL LNFILE
      * BAH 190121 GLOBAL LXFILE
      * BAH 20190627 REMOVED LN-REVOLVING, PT0153
      * BAH 20190701 REMOVED U-INSOURS,AVAILABLE-CREDIT FROM LPPFBUF
      * BAH 20190723 CLEAR LXG-FUTURE FIELDS
      * BAH 20190926 REMOVED ACCESS-CALL LNFILE
      *  CS 20201008 ADD TEST ON BP-FROZEN-OVERRIDE. IF SET TO "Y", WILL ALLOW
      *              BATCH POSTING WHEN LN-ACCT-FROZEN = "Y", PR# 1465
      * BAH 20210302 WAS NOT CALLING LONPF7 AND F2 WITH CORRECT LINKAGE ITEMS
      * BAH 20211015 CLEAR HOLD-PRIOR-DF-POSTDATE
      *              WRITE ERROR TRPFILE LONPF2, STATE 22007
      *              CONVERSION FAILED WHEN CONVERTING DATE 
      * CS 20220404 IN SETUP-LP, MOVE BP-PAYDATE TO LP-PAYDATE INSTEAD OF USING
      *             TRANSDATE PR# 1550
      * BAH 20220520 ALLOW TO RUN IN BATCH #1550B
      * BAH 20220614 ONLY SEND-LEGEND FOR ROUTE-BUF 00 AND DONT ALLOW
      *              POSTING TO INACTIVE P&L, #1550C
      * BAH 20220804 ALLOW DUPLICATE "OT" TO POST, NO PAGE BREAK ON BRANCH
      *              ON THE EXCEPTION REPORT #1564
      * BAH 20220815 ONLY PRINT LAST 4 OF SSNO #1564
      * BAH 20221115 PRINT BP-PAYDATE INSTEAD OF PDTH, #1578
      * BAH 2025.0228 LXE-EARN(4) - (7) WERE NOT GETTING CLEARED LIKE
      *               LONPF1 DOES! S35Q-168
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBLP/LPFSWK.CPY".
       COPY "LIBLP/LPFSBP.CPY".
       COPY "LIBGB/GBFSPR.CPY".
       COPY "LIBLP/LPFSOP.CPY".
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBLP/LPFDWK.CPY".
       COPY "LIBLP/LPFDBP.CPY".
       COPY "LIBGB/GBFDPR.CPY".
       COPY "LIBLP/LPFDOP.CPY".

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LP/LONPWA.CBL S35 08/24/22-10:27:37 >".
      **************************************************
      *     P R O G R A M   W O R K E R S              *
      **************************************************
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
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
       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LP01CD_SQL.CPY".
       COPY "LIBLP/LPWSCD.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01LTI.CPY".
       COPY "LIBLP/LP01LTI_SQL.CPY".
       COPY "LIBLP/LPWSLTI.CPY".
       COPY "LIBLP/LP01LTP.CPY".
       COPY "LIBLP/LP01LTP_SQL.CPY".
       COPY "LIBLP/LPWSLTP.CPY".
       COPY "LIBGL/GL01GI.CPY".
       COPY "LIBGL/GL01GI_SQL.CPY".
       COPY "LIBGL/GLWSGI.CPY".
       COPY "LIBGB/GB01RC.CPY".
       COPY "LIBGB/GB01RC_SQL.CPY".
       COPY "LIBGB/GBWSRC.CPY".
       01  BP1-IFN             PIC X(3)     VALUE "BP1".
       01  WK-IFN              PIC X(2)     VALUE "WK".
       01  PR-IFN              PIC X(7)     VALUE "PRINTER".

       01  HOLD-PROG-BUF-BRANCH PIC 9(4)    COMP.

       01  WK-KEY              PIC 9(2)     VALUE 0.
       01  POSTING-ERRCD       PIC XX       VALUE " ".

       01  EXCEPTION-CODE      PIC X(02)    VALUE " ".
       01  LEV                 PIC 9(6)     COMP.

       01  ORIG-LN-LBOX        PIC X(01)    VALUE " ".
       01  ORIG-LN-ALLOTCD     PIC X(01)    VALUE " ".
       01  ORIG-BP-TRAMT       PIC S9(9)V99 VALUE 0.
       01  ORIG-LN-OT2BAL      PIC S9(9)V99 VALUE 0.

       01  BEG-BRANCH          PIC 9(04)    VALUE 0000.
       01  END-BRANCH          PIC 9(04)    VALUE 9999.

      *-----------------------------------------------------------------
      * ENVIROMENT: POSTNAME AS IT WAS WHEN USER ENTERED PROGRAM
      *-----------------------------------------------------------------
       01  ORIG-POSTNAME       PIC X(10).
       01  HOLD-EXT-FILPATH.
           03  FILLER                PIC X(13).
           03  HOLD-EXT-FILPATH-DATA PIC X(6).

      *-----------------------------------------------------------------
      * SPOOL DIRECTORY/FILENAME FOR EXCEPTION REPORT
      *-----------------------------------------------------------------
       01  EX-SPOOL.
           03  EX-SPOOL-BASE          PIC X(09).
           03  FILLER                 PIC X(01)   VALUE "/".
           03  EX-SPOOL-MACHINE       PIC X(02).
           03  FILLER                 PIC X(01)   VALUE "/".
           03  EX-SPOOL-DATA-PATH PIC X(6).
           03  FILLER             PIC X(04)  VALUE "/EO/".
           03  EX-SPOOL-PCODE     PIC X(03)  VALUE "OTH".
           03  EX-SPOOL-DATE      PIC 9(06).
           03  EX-SPOOL-FILE.
               05  FILLER         PIC X(01).
               05  EX-SPOOL-STEP  PIC 9(04).
               05  EX-SPOOL-NAME  PIC X(06).
               05  EX-SPOOL-SUF   PIC X(02).

       01  STEP-COUNT             PIC 9999 VALUE 100.
       01 WORK-USERID.
          03  WORK-SECURITY        PIC X.
          03  WORK-BR              PIC 9(4).
          03  WORK-NAME            PIC X(5).

      *********************************************************
      *    MOVE ../../ED/CCFILMMDDBBB TO DONE SUB-DIRECTORY
       01  BT-PATH.
           03  BT-PATH-BASE          PIC X(09).
           03  FILLER                PIC X(01)   VALUE "/".
           03  BT-PATH-MACHINE       PIC X(02).
           03  FILLER                PIC X(04)   VALUE "/ED/".
           03  BT-PATH-SHORT-NAME    PIC X(5).
           03  BT-PATH-BATCH-DATE    PIC 9(4).
           03  BT-PATH-BATCH-NO      PIC 9(3).
       01  BT-DONE-PATH.
           03  BT-DONE-DIRECTORY.
               05  BT-DONE-BASE          PIC X(09).
               05  FILLER                REDEFINES BT-DONE-BASE.
                   07  BT-DONE-BASE-USR  PIC X(05).
                   07  BT-DONE-BASE-DIR  PIC X(04).
               05  FILLER                PIC X(01)   VALUE "/".
               05  BT-DONE-MACHINE       PIC X(02).
               05  FILLER                PIC X(09)   VALUE "/ED/DONE/".
           03  BT-DONE-FILENAME          PIC X(12).

      **********************
      *   LINE WORKERS     *
      **********************
       01  LN-WORKERS.
           03  LN-FEED          PIC 99       VALUE 1.
           03  LN-COUNT         PIC 999      VALUE 99.
           03  LN-WK            PIC 999      VALUE 999.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

       01  BATCH-NO-LIST        PIC X(8)     VALUE "BATCHNO.".
       01  BATCH-REFCD-LIST     PIC X(11)    VALUE "BATCHREFCD.".
       01  SPOOL-REFCD-LIST     PIC X(8)     VALUE "BFREFCD.".
       01  FIRST-TIME           PIC X        VALUE "Y".
       01  NAME-WORKER          PIC X(34).
       01  FIRST-NAME           PIC X(16).
       01  NOT-ON-FILE-FG       PIC X.
       01  MATCH-SSNO           PIC X        VALUE " ".
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".
       01  MESS-BUF.
           03  FILLER           PIC X(25).
           03  MESS-BRNO        PIC Z(4) VALUE ZERO.
           03  FILLER           PIC X(11).

       01  SUB                 PIC 9(6)      COMP-3 VALUE 0.
       01  DUP-FLAG            PIC X         VALUE " ".
       01  OVPAID-FLAG         PIC X         VALUE " ".
       01  OVPAID-AMT          PIC S9(9)V99.
       01  TOTAL-TABLE OCCURS 2.
           03  TOTAL-POSTED        PIC 9(6)      COMP-3 VALUE 0.
           03  AMOUNT-POSTED       PIC S9(9)V99  COMP-3 VALUE 0.
           03  TOTAL-EXCEPTION     PIC 9(6)      COMP-3 VALUE 0.
           03  AMOUNT-EXCEPTION    PIC S9(9)V99  COMP-3 VALUE 0.
           03  TOTAL-DUPS          PIC 9(6)      COMP-3 VALUE 0.
           03  AMOUNT-DUPS         PIC S9(9)V99  COMP-3 VALUE 0.
           03  TOTAL-OVPAID        PIC 9(6)      COMP-3 VALUE 0.
           03  AMOUNT-OVPAID       PIC S9(9)V99  COMP-3 VALUE 0.

      ***********************************
      *     P R I N T   B U F F E R S   *
      ***********************************
       01  HEAD1.
           03  H-DATE           PIC X(8)     VALUE " ".
           03  FILLER           PIC XX       VALUE " ".
           03  H-HR             PIC 99       VALUE 0.
           03  FILLER           PIC X        VALUE ":".
           03  H-MIN            PIC 99       VALUE 0.
           03  FILLER           PIC X(38)    VALUE " ".
           03  H-NAME           PIC X(40)    VALUE " ".
           03  FILLER           PIC X(30)    VALUE " ".
           03  FILLER           PIC X(5)     VALUE "PAGE ".
           03  H-PAGE-NO        PIC ZZZ9.

       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(33)    VALUE " ".
           03  FILLER           PIC X(42)    VALUE
           "BATCH OTHER PROCESSING EXCEPTION REPORT".

       01  HEAD3.
           03  FILLER           PIC X(10)    VALUE "BATCH NO:".
           03  H-BATCH          PIC 999.


       01  HEAD4.
           03  FILLER      PIC X(19)    VALUE " ".
           03  FILLER      PIC X(38)    VALUE " ".
           03  FILLER      PIC X(11)    VALUE " ".
           03  FILLER      PIC X(26)    VALUE
            "-------- BALANCES --------".
           03  FILLER      PIC X(04)    VALUE " ".
           03  FILLER      PIC X(7)     VALUE "PAYMENT".

       01  HEAD5.
           03  FILLER      PIC X(6)     VALUE "BRNO".
           03  FILLER      PIC X(11)    VALUE "ACCOUNT NO.".
           03  FILLER      PIC X(6)     VALUE " ".
           03  FILLER      PIC X(10)    VALUE "SOC-SEC-NO".
           03  FILLER      PIC X(3)     VALUE " ".
           03  FILLER      PIC X(8)     VALUE "BORROWER".
           03  FILLER      PIC X(13)    VALUE " ".
           03  FILLER      PIC X(4)     VALUE "DATE".
           03  FILLER      PIC X(7)     VALUE " ".
           03  FILLER      PIC X(5)     VALUE "OTHER".
           03  FILLER      PIC X(4)     VALUE " ".
           03  FILLER      PIC X(6)     VALUE "INT/LC".
           03  FILLER      PIC X(2)     VALUE " ".
           03  FILLER      PIC X(9)     VALUE "PRINCIPAL".
           03  FILLER      PIC X(05)    VALUE " ".
           03  FILLER      PIC X(6)     VALUE "AMOUNT".
           03  FILLER      PIC X(2)     VALUE " ".
           03  FILLER      PIC X(9)     VALUE "EXCEPTION".

       01  DETAIL-LINE     PIC X(132) VALUE " ".
       01  D-LINE1 REDEFINES DETAIL-LINE.
           03  D-BRNO      PIC ZZZ9.
           03  FILLER      PIC XX.
           03  D-LOAN-NUM  PIC X(15).
           03  FILLER     REDEFINES D-LOAN-NUM.
               05  D-LNNO  PIC 9(8).
           03  FILLER      PIC XX.
           03  D-SSNO.
               05  D-SSNO1 PIC X(7).
               05  D-SSNO2 PIC 9999.
           03  FILLER      PIC XX.
           03  D-NAME      PIC X(18).
           03  FILLER      PIC X.
           03  D-DATE      PIC 99/99/99.
           03  D-TOTAL    REDEFINES D-DATE
                           PIC Z(8).
           03  FILLER      PIC X.
           03  D-OTHBAL    PIC ZZZZZ9.99-     BLANK ZERO.
           03  D-INTBAL    PIC ZZZZZ9.99-     BLANK ZERO.
           03  D-CURBAL    PIC ZZZZZZ9.99.
           03  D-TRAMT     PIC ZZZZZZZ9.99-.
           03  FILLER      PIC X.
           03  D-EXCEPT    PIC X(25).

       COPY "LIBLP/LPSTATW.CPY".
       COPY "LIBLP/LPBP_CMD.CPY".
       COPY "LIBLP/LPBPPATH.CPY".
       COPY "LIBLP/LPWKPATH.CPY".
       COPY "LIBGB/SPPDTHW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBLP/LPITRMW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBLP/SETCTBLW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/REBATEW.CPY".
       COPY "LIBLP/ADDONSPRW.CPY".
       COPY "LIBGB/AGEINGW.CPY".
       COPY "LIBLP/LPUPWKW.CPY".
       COPY "LIBLP/LPCEPPW.CPY".
       COPY "LIBLP/LPAMTSW.CPY".
       COPY "LIBLP/LPPDUEW.CPY".
       COPY "LIBLP/LPAPRSW.CPY".
       COPY "LIBLP/LPAPRZW.CPY".
       COPY "LIBLP/LPUPERW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/RBACTW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBLP/LPEDPATH.CPY".

       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/LONPWA_DEF.CPY".
       COPY "LIBLP/LONPWA_WKS.CPY".

      /*************************************************
      *   C O N T R O L   M O D U L E                  *
      **************************************************
       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBLP/LPLOANW.CPY".
       COPY "LIBLP/LPPFBUF.CPY".

       SCREEN SECTION.
       COPY "LIBLP/LONPWA_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME PROG-BUF UPDATE-BUF.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
              WK-FILE BP-FILE PR-FILE OP-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LTP1-FILE.
           PERFORM CLOSE-LTI1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-CD1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GI1-FILE.
           PERFORM CLOSE-RC2-FILE.
           PERFORM CLOSE-BR-FILE.
           CLOSE WK-FILE BP-FILE PR-FILE OP-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      ******************************************************************
      *
      *  PROGRAM CONTROL
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "LONPWA" TO VDU-FORM-NAME.

           IF (EXT-ROUTE-BUF = "00")
              PERFORM FORM-RESET.

           MOVE EXT-FILPATH TO HOLD-EXT-FILPATH.
           ACCEPT ORIG-POSTNAME FROM ENVIRONMENT "POSTNAME".

           PERFORM INITIALIZATION.
           IF ERRCD NOT = " "
              GO TO END-PROGRAM.

           PERFORM GET-GPENV.

           MOVE 0 TO BEG-BRANCH.
           MOVE 9999 TO END-BRANCH.

           MOVE BEG-BRANCH TO BR-NO.
           PERFORM START-BR-FILE.

       NEXT-GROUP-LOC.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-FG = 9 OR BR-NO > END-BRANCH
              GO TO END-ROUTINE.

      * ONLY PROCESS FILES FOR THE MACHINE I'M ON

           IF BR-MACHINE NOT = EXT-FILPATH-MACHINE
              GO TO NEXT-GROUP-LOC.

       MOVE-BRANCH-PATH-INFO.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.
           MOVE "B"                        TO LN-PATH-OVERRIDE
                                              BP-PATH-OVERRIDE.

      * SETUP BP FILE PATH
      * GET REFCD OF BP FILE IN LOCAL 'ED'

           MOVE " "           TO BP-ALL-SUBPATH.
           MOVE BR-NO         TO BP-ALL-BRANCH.
           MOVE TRANS-DATE    TO NUM-DATE.
           MOVE 0    TO NUM-CCYY.
           MOVE NUM-DATE      TO BP-ALL-DATE.
           MOVE BATCH         TO BP-ALL-BATCH-NO.
           MOVE BATCH-REFCD   TO BP-ALL-REFCD.
           PERFORM LOAD-BP1-FILE.
           MOVE BP-PATH       TO ACCESS-BUF.

      * PERFORM ACCESS CALL TO DETERMINE IF THE BRANCH HAS AN
      * ALLOTMENT/LBOX REFERENCE PAYMENT FILE (BP)

           PERFORM ACCESS-CALL.
           IF STAT NOT = "00"
              GO TO NEXT-GROUP-LOC.

      * DISPLAY STATUS FOR EVERY BRANCH THAT HAS A BP FILE, VALID OR NOT

           IF ( EXT-ROUTE-BUF = "00" )
              MOVE "UPDATE IN PROGRESS......." TO MESS-BUF
              MOVE BR-NO TO MESS-BRNO
              MOVE MESS-BUF TO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           PERFORM OPEN-LN1-FILE.
           PERFORM OPEN-BW1-FILE.

      * CHANGE THE EXTERNAL FILPATH TO THAT OF THE CURRENT BRANCH.

           MOVE BR-MACHINE            TO EXT-FILPATH-MACHINE.
           MOVE BR-DATA-PATH          TO EXT-FILPATH-DATA.
           SET ENVIRONMENT "FIL"      TO EXT-FILPATH-FIL.
           MOVE ORIG-POSTNAME         TO WORK-USERID.
           MOVE BR-NO                 TO WORK-BR.
           SET ENVIRONMENT "POSTNAME" TO WORK-USERID.
           MOVE BR-NO                 TO BRANCH.

      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      * WARNING * WARNING * WARNING * WARNING * WARNING * WARNING * WARNING *
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      *                                                                     *
      * FROM THIS POINT FORWARD THE USER HAS BE PLACED, LOGICALLY, INTO     *
      * THE BRANCH FOR WHICH HE/SHE IS POSTING FOR.                         *
      *                                                                     *
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      * WARNING * WARNING * WARNING * WARNING * WARNING * WARNING * WARNING *
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

      * TEST RC-STATUS FOR OPEN DAY 

           MOVE "A1"       TO RC-STATUS.
           MOVE BR-NO      TO RC-BRNO.
           MOVE TRANS-DATE TO RC-TRANS-DATE.

           PERFORM OPEN-RC2-FILE.
           PERFORM READ-RC2-FILE.
           PERFORM CLOSE-RC2-FILE.
           IF IO-FG NOT = 0
              MOVE "A1" TO EXCEPTION-CODE
              PERFORM BRANCH-EXCEPTION
              GO TO END-OF-GROUP.

      * DAY IS OPEN, RESERVE OP FILE SO THE DAY CANNOT BE CLOSED.

           MOVE BR-NO      TO OPEN-PATH-BRNO.
           MOVE TRANS-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO OPEN-PATH-TRDATE.

           PERFORM LOAD-OPEN-FILE.
           MOVE OPEN-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT NOT = "00"
              MOVE "OP" TO EXCEPTION-CODE
              PERFORM BRANCH-EXCEPTION
              GO TO END-OF-GROUP.

           PERFORM OPEN-OP-FILE.

           PERFORM OPEN-BP1-FILE.
           MOVE 0 TO BP-LNNO BP-SEQNO.
           PERFORM START-BP1-FILE.

       MAIN-READ-BP1-NEXT.
           PERFORM READ-BP1-FILE-NEXT.
           IF IO-FG NOT = 0
              PERFORM CLOSE-LN1-FILE
              PERFORM CLOSE-BW1-FILE
              GO TO END-OF-GROUP.

      * SKIP THE HEADER RECORD

           IF BP-LNNO = 0
      *       UNLOCK BP-FILE
              GO TO MAIN-READ-BP1-NEXT.

           MOVE " "      TO DUP-FLAG
                            OVPAID-FLAG.
           MOVE 0        TO OVPAID-AMT.
           MOVE BP-TRAMT TO ORIG-BP-TRAMT.

      * LNNO OF ALL 9'S ARE ALL ZEROS
           IF BP-LNNO = 99999999
              MOVE "Y" TO NOT-ON-FILE-FG
              MOVE "TOTAL OF ALL ZERO ACCTNO'S" TO D-EXCEPT
              PERFORM MAIN-EXCEPTION
              GO TO MAIN-READ-BP1-NEXT.

           IF BP-UPDFG = "U" OR "E"
      *       UNLOCK BP-FILE
              GO TO MAIN-READ-BP1-NEXT.

           MOVE " " TO NOT-ON-FILE-FG.
           MOVE BP-LNNO TO LN-ACCTNO.

      * PRINT OUT EACH DUPLICATE
      * NOTE, IN LONPW9, WE CHANGED TO ALLOW MULTIPLES FOR ALL CUSTOMERS
      * 7/18/16, BPREF/LONPW9 HAVE BEEN CHANGED TO ALLOW DUP PMTS ON
      *          A LOAN IN ONE BATCH, NEVER CHANGED THIS TO ALLOW DUP
      *          OT'S.

      *    IF BP-SEQNO > 01
      *       MOVE "Y" TO NOT-ON-FILE-FG DUP-FLAG
      *       MOVE "DUPLICATE PAYMENT RECEIVED" TO D-EXCEPT
      *       PERFORM MAIN-EXCEPTION
      *       GO TO MAIN-READ-BP1-NEXT.

           PERFORM READ-LN1-FILE.
           IF IO-FG NOT = 0
              MOVE "Y" TO NOT-ON-FILE-FG
              MOVE "ACCOUNT NOT ON FILE" TO D-EXCEPT
              PERFORM MAIN-EXCEPTION
              GO TO MAIN-READ-BP1-NEXT.

           MOVE "OT" TO HOLD-BP-TRCD.

           MOVE " " TO MATCH-SSNO.
           IF BP-SSNO = LN-SSNO(1)
              MOVE "Y" TO MATCH-SSNO
           ELSE
           IF BP-SSNO = LN-SSNO(2)
              MOVE "Y" TO MATCH-SSNO
           ELSE
           IF BP-SSNO = LN-SSNO(3)
              MOVE "Y" TO MATCH-SSNO
           ELSE
           IF BP-SSNO = LN-SSNO(4)
              MOVE "Y" TO MATCH-SSNO.
           IF MATCH-SSNO = " "
              MOVE "SSNO'S DO NOT MATCH" TO D-EXCEPT
              PERFORM MAIN-EXCEPTION
              GO TO MAIN-READ-BP1-NEXT.

           IF BP-TRAMT = 0
              MOVE "ZERO PAYMENT AMOUNT" TO D-EXCEPT
              PERFORM MAIN-EXCEPTION
              GO TO MAIN-READ-BP1-NEXT.

      * TEST IF ACCOUNT BELONGS TO POSTING BRANCH:

           IF LN-OWNBR NOT = BR-NO
              MOVE "ACCOUNT OWNED BY OTHER BR" TO D-EXCEPT
              PERFORM MAIN-EXCEPTION
              GO TO MAIN-READ-BP1-NEXT.
           
      * IF YOU HAVE THE LOAN FROZEN FLAG SET TO "Y", AND THE BATCH
      * PAYMENT OVERRIDE FLAG IS " " OR "N", IT WILL NOT POST
      * IF YOU HAVE "Y" IN THE BATCH PAYMENT FILE, IT WILL OVERRIDE
      * THE LOAN FLAG AND POST.
           IF BP-FROZEN-OVERRIDE NOT = "Y"
             IF LN-ACCT-FROZEN = "Y"
                 MOVE "ACCOUNT IS FROZEN" TO D-EXCEPT
                 PERFORM MAIN-EXCEPTION
                 GO TO MAIN-READ-BP1-NEXT.

           IF LN-PLCD = "I"
              MOVE "INACTIVE P&L ACCOUNT " TO D-EXCEPT
              PERFORM MAIN-EXCEPTION
              GO TO MAIN-READ-BP1-NEXT.

      * TEST IF JD ACCOUNTS ARE ALLOWED:

           IF ( (CD-BR-ALLOT-OPTION = "Y") AND
                (BR-BP-JD NOT = "Y"      ) )
              OR
              ( (CD-BR-LBOX-OPTION = "Y" ) AND
                (BR-LBOX-JD NOT = "Y"    ) )
              IF LN-JDDATE NOT = 0
                 MOVE "JUDGEMENT ACCOUNT " TO D-EXCEPT
                 PERFORM MAIN-EXCEPTION
                 GO TO MAIN-READ-BP1-NEXT.

      * TEST IF ACCOUNTS WITH MESSAGE ARE ALLOWED:

           IF ( (CD-BR-ALLOT-OPTION = "Y") AND
                (BR-BP-FLASH NOT = "Y"   ) )
              OR
              ( (CD-BR-LBOX-OPTION = "Y" ) AND
                (BR-LBOX-FLASH NOT = "Y" ) )
              IF LN-MESSAGE NOT = " "
                 MOVE "FLASHING MESSAGE  " TO D-EXCEPT
                 PERFORM MAIN-EXCEPTION
                 GO TO MAIN-READ-BP1-NEXT.

      * TEST IF ACCOUNTS WITH ACTION CODE ARE ALLOWED:

           IF ( (CD-BR-ALLOT-OPTION = "Y"  ) AND
                (BR-BP-ACTIONCD NOT = "Y"  ) )
              OR
              ( (CD-BR-LBOX-OPTION = "Y"   ) AND
                (BR-LBOX-ACTIONCD NOT = "Y") )
              IF LN-ACTIONCD NOT = " "
                 MOVE "ACTION CODE ON ACCOUNT" TO D-EXCEPT
                 PERFORM MAIN-EXCEPTION
                 GO TO MAIN-READ-BP1-NEXT.

           PERFORM GET-SPR.
           IF IO-FG NOT = 0
              MOVE "INVALID SPR RECORD" TO D-EXCEPT
              PERFORM MAIN-EXCEPTION
              GO TO MAIN-READ-BP1-NEXT.

      * TEST IF MAXIMUM CONTRACTUAL IS EXCEEDED:

           MOVE TRANS-DATE TO AGEING-DATE.
           PERFORM COMPUTE-CONTRACTUAL.
           PERFORM COMPUTE-RECENCY.

           IF CD-BR-ALLOT-OPTION = "Y"
              IF BR-BP-MAXCON NOT = 999
                 IF CONTRACTUAL > BR-BP-MAXCON
                    MOVE "EXCEEDS MAX CONTRACTUAL" TO D-EXCEPT
                    PERFORM MAIN-EXCEPTION
                    GO TO MAIN-READ-BP1-NEXT.


           IF CD-BR-LBOX-OPTION = "Y"
              IF BR-LBOX-MAXCON NOT = 999
                 IF CONTRACTUAL > BR-LBOX-MAXCON
                    MOVE "EXCEEDS MAX CONTRACTUAL" TO D-EXCEPT
                    PERFORM MAIN-EXCEPTION
                    GO TO MAIN-READ-BP1-NEXT.

      * TEST IF MAXIMUM RECENCY IS EXCEEDED:

           IF CD-BR-ALLOT-OPTION = "Y"
              IF BR-BP-MAXREC NOT = 999
                 IF RECENCY > BR-BP-MAXREC
                    MOVE "EXCEEDS MAX RECENCY" TO D-EXCEPT
                    PERFORM MAIN-EXCEPTION
                    GO TO MAIN-READ-BP1-NEXT.

           IF CD-BR-LBOX-OPTION = "Y"
              IF BR-LBOX-MAXREC NOT = 999
                 IF RECENCY > BR-LBOX-MAXREC
                    MOVE "EXCEEDS MAX RECENCY" TO D-EXCEPT
                    PERFORM MAIN-EXCEPTION
                    GO TO MAIN-READ-BP1-NEXT.

      * DON'T ALLOW POSTING WHEN LOAN HAS ZERO BALANCE

           IF LN-CURBAL = 0
              MOVE "ALREADY ZERO BALANCE" TO D-EXCEPT
              PERFORM MAIN-EXCEPTION
              GO TO MAIN-READ-BP1-NEXT.

      * TEST FOR VALID GENERAL LEDGER INTERFACE RECORD:

           MOVE LN-OWNBR TO GI-BRANCH.
           MOVE LN-CLASS TO GI-CLASS.
           PERFORM READ-GI1-FILE.
           IF IO-FG NOT = 0
              MOVE "MISSING G/L INTERFACE " TO D-EXCEPT
              PERFORM MAIN-EXCEPTION
              GO TO MAIN-READ-BP1-NEXT.

           MOVE " " TO POSTING-ERRCD
                       D-EXCEPT.

           PERFORM OTHER-CHG-POSTING.

           IF NOT (POSTING-ERRCD = " " OR "O")
              GO TO MAIN-READ-BP1-NEXT.

           PERFORM CLEAR-LEGEND.
           ADD ORIG-BP-TRAMT TO AMOUNT-POSTED(1) AMOUNT-POSTED(2).
           ADD 1             TO TOTAL-POSTED(1)  TOTAL-POSTED(2).

           MOVE "U" TO BP-UPDFG.
           PERFORM REWRITE-BP1-FILE.

           MOVE "N" TO FIRST-TIME.
           GO TO MAIN-READ-BP1-NEXT.

       END-OF-GROUP.
           MOVE 1 TO LEV.
           PERFORM PRINT-TOTALS.
           PERFORM CLOSE-BP1-FILE.
           PERFORM CLOSE-OP-FILE.

      * REMOVE THE BP FILE HERE FOR NORMAL EXITS

           MOVE BP-PATH TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.
           PERFORM MOVE-BTFILE.

           GO TO NEXT-GROUP-LOC.

       END-ROUTINE.
           MOVE 2 TO LEV.
           PERFORM PRINT-TOTALS.
           PERFORM RESTORE-SCREEN.
           PERFORM CLOSE-BP1-FILE.
           PERFORM CLOSE-OP-FILE.

      * REMOVE THE BP FILE HERE FOR NORMAL EXITS

           MOVE BP-PATH TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.

           MOVE 0 TO BATCH.
           GO TO END-PROGRAM.

      ******************************************************************
       OTHER-CHG-POSTING SECTION.
      ******************************************************************

      * SETUP-LP-RECORD

           MOVE "OT"     TO BP-TRCD.
           PERFORM SETUP-LP-RECORD.
           MOVE 0 TO HOLD-OVPAID-CHECK.

      * SETUP-LONPF-BUFFER
           PERFORM SETUP-LONPF-BUFFER.

      * OTHER CHARGES POSTING

           MOVE "LP/LONPF7" TO FORM-NAM.

           CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME
                                 PROG-BUF UPDATE-BUF.
           CANCEL FORM-PROGX.

      * TEST PROG-BUF (ERRCD) TO MAKE SURE LONPF7 DID NOT REJECT
      * THE PROPOSED TRANSACTION

           IF ERRCD NOT = " "
              MOVE "LONPF7: 'OT' NOT APPLIED   " TO D-EXCEPT
              PERFORM MAIN-EXCEPTION
              MOVE "E" TO POSTING-ERRCD
              GO TO OTHER-CHG-POSTING-EXIT.

      * UPDATE
           MOVE 0 TO HOLD-PRIOR-DF-POSTDATE.

           MOVE "LP/LONPF2" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME
                                 PROG-BUF UPDATE-BUF.
           CANCEL FORM-PROGX.

      * TEST PROG-BUF (ERRCD) TO MAKE SURE LONPF2 DID NOT REJECT
      * THE PROPOSED TRANSACTION

           IF ERRCD = "E" OR "X" OR "M"
              MOVE "LONPF2: PMT NOT APPLIED  " TO D-EXCEPT
              PERFORM MAIN-EXCEPTION
              MOVE "E" TO POSTING-ERRCD
              GO TO OTHER-CHG-POSTING-EXIT.

       OTHER-CHG-POSTING-EXIT.
           EXIT.

      **************************************************
       SETUP-LP-RECORD SECTION.
      **************************************************
           PERFORM CLEAR-LP-FILE.
           MOVE LN-STATUSFG    TO LP-STATUSFG.
           MOVE LN-ACCTNO      TO LP-ACCTNO.
           MOVE TRANS-DATE     TO LP-TRDATE.
           MOVE BP-PAYDATE     TO LP-PAYDATE.
           MOVE LN-SSNO(1)     TO LP-SSNO.
           MOVE LN-LCPDTH-DATE TO LP-LCPDTH-DATE.
      * BECAUSE CASH DRAWER IS NEVER REALY UPDATED DURING THE UPDATE
      * OF A "PA",ETC TRANSACTION, WE DECIDED TO DEFAULT THE DRAWER
      * NUMBER TO ZERO RATHER THAN MAKE THE USER ENTER A UNUSED #.
           MOVE 0              TO LP-TILLNO.
           MOVE BATCH-REFCD    TO LP-REFNO.
           MOVE HOLD-BP-TRCD   TO LP-TRCD.

           PERFORM CLEAR-LXG-FILE.

           MOVE LP-BRNO   TO LXG-BRNO.
           MOVE LP-ACCTNO TO LXG-ACCTNO.

      **************************************************
       SETUP-LONPF-BUFFER SECTION.
      **************************************************
           MOVE 0   TO ENTRY-FLAG BYPASS-AMT-ENT-FG.
           MOVE "F" TO FULL-DISPLAY.
           MOVE " " TO REV-REBATE FULL-PAGE-FG ENTER-AMT-FG
                       HOLD-DISPLAY-ERN
                       HOLD1-KEY-DOWN HOLD2-KEY-UP.

           MOVE LN-ORGST    TO SP-ORGST.
           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           MOVE LN-LAWCODE  TO SP-LAWCODE.

           MOVE 0 TO HOLD-PARTIALS
                     HOLD-LCPAID
                     HOLD-DLPARVHELD-ADJ
                     HOLD-AFPDLLP
                     HOLD-REPO-AUCTION-EXP
                     ADDON-INT-REBATE
                     ADDON-INT-EARNED
                     HOLD-PRIOR-DF-POSTDATE.

           MOVE LN-TOTINT     TO HOLD-LN-TOTINT.
           MOVE LN-TOTERN     TO HOLD-LN-TOTERN.
           MOVE LN-TOTDEF     TO HOLD-LN-TOTDFMNTS.
           MOVE LN-TOTLCHG    TO HOLD-LN-TOTLCHG.
           MOVE LN-TOTSERACCT TO HOLD-LN-TOTSERACCT.
           MOVE LN-JDRATE     TO HOLD-LN-JDRATE.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 13
              MOVE 0 TO REB-AMOUNT(SUB)
              IF SUB < 7
                 MOVE LN-ACCERN(SUB) TO HOLD-LN-ACCERN(SUB)
              END-IF
              IF SUB < 9
                 MOVE " " TO INS-RB-TBL(SUB)
              END-IF
           END-PERFORM.

           MOVE LN-ACCTNO  TO LOAN-KEY.
           MOVE LN-SSNO(1) TO BORROWER-KEY.
           MOVE BP-TRAMT   TO LP-INTDUE.
           PERFORM SAVE-SCREEN.

      ******************************************************************
       MAIN-EXCEPTION SECTION.
      ******************************************************************
           IF DUP-FLAG = "Y"
              ADD ORIG-BP-TRAMT     TO AMOUNT-DUPS(1)
                                       AMOUNT-DUPS(2)
              ADD 1 TO TOTAL-DUPS(1)
                       TOTAL-DUPS(2)
           ELSE
              IF (POSTING-ERRCD NOT = "O") AND
                 (HOLD-OVPAID-CHECK = 0  )
                 ADD ORIG-BP-TRAMT     TO AMOUNT-EXCEPTION(1)
                                          AMOUNT-EXCEPTION(2)
                 ADD 1 TO TOTAL-EXCEPTION(1)
                          TOTAL-EXCEPTION(2).

           MOVE " " TO DUP-FLAG.
           MOVE "N" TO FIRST-TIME.

           PERFORM DETAIL-LINE-PROCESS.
           MOVE "E" TO BP-UPDFG.
           PERFORM REWRITE-BP1-FILE.

       EXIT-MAIN-EXCEPTION.
           EXIT.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************
           MOVE USER-PID TO TEMP-PID.

           MOVE BRANCH TO HOLD-PROG-BUF-BRANCH.

           IF ( EXT-ROUTE-BUF = "00" )
              MOVE BATCH TO WR-BUF
              MOVE BATCH-NO-LIST TO WR-LIST
              PERFORM CALL-WRFORM
              MOVE BATCH-REFCD TO WR-BUF
              MOVE BATCH-REFCD-LIST TO WR-LIST
              PERFORM CALL-WRFORM
              MOVE BATCH-REFCD TO WR-BUF
              MOVE SPOOL-REFCD-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           MOVE " " TO ERRCD.

           IF BATCH < GP-OTREF-NEXT-BATCH-START OR
              BATCH > GP-OTREF-NEXT-BATCH-STOP
              IF ( EXT-ROUTE-BUF = "00" )
                 MOVE "INVALID REFCD BATCH NUMBER" TO MESS
                 PERFORM SEND-MESS
              END-IF
              MOVE "X" TO ERRCD
              GO TO END-INIT.

           IF ( EXT-ROUTE-BUF NOT = "00" )
              PERFORM CREATE-WK-FILE
              GO TO SKIP-OK-YN.

       OK-YN.
           MOVE "E  A010ACCEPT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO OK-YN.
           IF VDUSTATUS = "1>" GO TO END-INIT.
           IF VDUSTATUS NOT = "00" GO TO OK-YN.
           IF VDUBUF NOT = "Y" GO TO OK-YN.

           IF ( EXT-ROUTE-BUF = "00" )
              MOVE "UPDATE IN PROGRESS......." TO MESS-BUF
              MOVE 0 TO MESS-BRNO
              MOVE MESS-BUF TO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

       SKIP-OK-YN.
           MOVE EXT-CONAME-USER-ID TO H-USERID.
           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.

      * CREATE SPOOL DIRECTORY AND OPEN SPOOL FILE IN 'EO'

           PERFORM CREATE-SPOOL-DIR.
           IF ERRCD NOT = " "
              GO TO END-INIT.
           MOVE EX-SPOOL TO PRU-FILE.
           PERFORM OPEN-PR-FILE.
           IF IO-FG NOT = 0
              GO TO END-INIT.

           MOVE " " TO HOLD-SP1-KEY.
      * MUST LEAVE SP1-FILE OPEN BECAUSE ADDONSPR READS IT
           PERFORM OPEN-SP1-FILE.
           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GI1-FILE.

           MOVE BATCH-REFCD TO LP-REFNO.

      * GET CD REC FOR 'BATCH-REFCD'

           PERFORM OPEN-CD1-FILE.
           MOVE "BR"        TO CD-TYPE.
           MOVE BATCH-REFCD TO CD-CODE.
           PERFORM READ-CD1-FILE.
           PERFORM CLOSE-CD1-FILE.
           IF IO-BAD
              IF ( EXT-ROUTE-BUF = "00" )
                 MOVE "BATCH REFERENCE CODE NOT ON FILE" TO MESS
                 PERFORM SEND-MESS
              END-IF
              GO TO END-INIT
           ELSE
              MOVE CD-BR-LC-FG     TO BATCH-REFCD-LC-FG
              MOVE CD-BR-FILE-NAME TO BT-PATH-SHORT-NAME.

           MOVE 0 TO HOLD-OVPAID-CHECK.

           GO TO EXIT-INITIALIZE.
       END-INIT.
           MOVE 1 TO ERRCD.
       EXIT-INITIALIZE.
           EXIT.

      ******************************************************************
       DETAIL-LINE-PROCESS SECTION.
      ******************************************************************

      * ONLY PERFORMED FOR EXCEPTIONS

           IF NOT-ON-FILE-FG = "Y"
              MOVE BR-NO      TO D-BRNO
              MOVE BP-LNNO    TO D-LNNO
              MOVE "XXX-XX-"  TO D-SSNO1
              MOVE BP-SSNO    TO D-SSNO2
              PERFORM PROCESS-BORROWER
              MOVE BP-PAYDATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO D-DATE
              MOVE BP-TRAMT   TO D-TRAMT
              MOVE 2 TO LN-FEED
              PERFORM WRITE-DETAIL-LINE
              GO TO END-DETAIL-LN-PROCESS.

           MOVE BR-NO            TO D-BRNO.
           PERFORM PROCESS-BORROWER.
           PERFORM LPSTAT-DISPLAY.
           MOVE LPS-DISPLAY-LNNO TO D-LOAN-NUM.

      * ALWAYS DISPLAY SOCIAL FROM BP EVEN IF IT IS INVALID. IT WILL
      * MAKE IT EASIER TO BACKTRACK IF PAYMENT DOES NOT POST
           MOVE "XXX-XX-"     TO D-SSNO1.
           MOVE BP-SSNO       TO D-SSNO2.
           MOVE BP-PAYDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY   TO D-DATE.
           ADD LN-OTHBAL ORIG-LN-OT2BAL GIVING D-OTHBAL.
           ADD LN-INTBAL LN-LCBAL GIVING D-INTBAL.
           MOVE LN-CURBAL     TO D-CURBAL.
           MOVE ORIG-BP-TRAMT TO D-TRAMT.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

       END-DETAIL-LN-PROCESS.
           EXIT.

      ******************************************************************
       PROCESS-BORROWER SECTION.
      ******************************************************************
           MOVE " " TO D-NAME.

      * ALWAYS DISPLAY SOCIAL FROM BP EVEN IF IT IS INVALID. IT WILL
      * MAKE IT EASIER TO BACKTRACK IF PAYMENT DOES NOT POST

           MOVE BP-SSNO TO BW-SSNO.
           PERFORM READ-BW1-FILE.
           IF IO-FG NOT = 0
              MOVE "**NOT ON FILE**" TO D-NAME
              GO TO END-PROCESS-BORROWER.
           MOVE BW-LNAME TO NAME-WORKER.
           MOVE BW-FNAME TO FIRST-NAME.
           INSPECT NAME-WORKER REPLACING FIRST "  " BY ",?".
           INSPECT NAME-WORKER REPLACING FIRST "                "
               BY FIRST-NAME.
           INSPECT NAME-WORKER REPLACING ALL "?" BY " ".
           MOVE NAME-WORKER TO D-NAME.

       END-PROCESS-BORROWER.
           EXIT.

      ******************************************************************
       WRITE-DETAIL-LINE SECTION.
      ******************************************************************
           ADD LN-FEED LN-COUNT GIVING LN-WK.
           IF LN-WK > EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.
           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE " " TO DETAIL-LINE.

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

           MOVE BATCH TO H-BATCH.
           MOVE HEAD2 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE HEAD3 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE HEAD4 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE HEAD5 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

       END-HEAD-PAGE.
           MOVE 5 TO LN-COUNT.
           MOVE 2 TO LN-FEED.

      ***************************************************************
       SAVE-SCREEN SECTION.
      ***************************************************************
           MOVE 3 TO WK-KEY.
           PERFORM OPEN-WK-FILE.
           MOVE " " TO WK-SAVE-RECORD.
           MOVE 0 TO WK-ELE.

           MOVE LN-REC  TO WK-SAVE-RECORD.
           MOVE LP-REC  TO WK-LP-REC.
           MOVE LX-REC  TO WK-LX-REC.
           MOVE LXG-REC TO WK-LXG-REC.

           MOVE 0 TO LXE-EARN(1)
                     LXE-EARN(2)
                     LXE-EARN(3)
                     LXE-EARN(4)
                     LXE-EARN(5)
                     LXE-EARN(6)
                     LXE-EARN(7).
           IF SP-ERNFRMLA(1) = 15 OR SP-ERNFRMLA(5) = 16
              MOVE LN-ERN-IB-INTOWE TO LXE-WORKER
           ELSE
              MOVE 0 TO LXE-WORKER.
           MOVE 0 TO LXE-WORKER2.
           MOVE LXE-REC TO WK-LXE-REC.

           PERFORM WRITE-WK-FILE.
           IF IO-FG NOT = 0
              PERFORM REWRITE-WK-FILE.
       EXIT-SAVE-SCREEN.
           PERFORM CLOSE-WK-FILE.

      **************************************************************
       RESTORE-SCREEN SECTION.
      **************************************************************
           MOVE 3 TO WK-KEY.
           PERFORM OPEN-WK-FILE.
           PERFORM READ-WK-FILE.
           IF NEW-FORM NOT = OLD-FORM
              MOVE OLD-PROG-FORM TO WK-OLD-PROG-FORM
              MOVE NEW-PROG-FORM TO OLD-PROG-FORM.
           IF IO-FG = 0
              GO TO RESTORE-CLEAR.

           MOVE 0 TO WK-ELE.
           PERFORM WRITE-WK-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

           GO TO EXIT-RESTORE-SCREEN.
       RESTORE-CLEAR.
           MOVE WK-SAVE-RECORD TO LN-REC.
           MOVE WK-LP-REC      TO LP-REC.
           MOVE WK-LX-REC      TO LX-REC.
           MOVE WK-LXE-REC     TO LXE-REC.
           MOVE WK-LXG-REC     TO LXG-REC.
           MOVE 0              TO WK-ELE.
           PERFORM REWRITE-WK-FILE.
       EXIT-RESTORE-SCREEN.
           PERFORM CLOSE-WK-FILE.

      ******************************************************************
       PRINT-TOTALS SECTION.
      ******************************************************************

           IF LEV = 2
              MOVE "** GRAND TOTALS **" TO DETAIL-LINE
              MOVE 4 TO LN-FEED
              PERFORM WRITE-DETAIL-LINE.

           IF TOTAL-EXCEPTION(LEV) NOT = 0
              IF LEV = 2
                 MOVE "** TOTAL EXCEPTIONS BR# ALL  " TO DETAIL-LINE
              ELSE
                 MOVE "** TOTAL EXCEPTIONS BR# &&&&" TO DETAIL-LINE
                 INSPECT DETAIL-LINE REPLACING FIRST "&&&&" BY BR-NO
              END-IF
              MOVE AMOUNT-EXCEPTION(LEV) TO D-TRAMT
              MOVE TOTAL-EXCEPTION(LEV)  TO D-TOTAL
              MOVE 2 TO LN-FEED
              PERFORM WRITE-DETAIL-LINE.

           IF TOTAL-DUPS(LEV) NOT = 0
              IF LEV = 2
                 MOVE "** TOTAL DUPLICATES BR# ALL  " TO DETAIL-LINE
              ELSE
                 MOVE "** TOTAL DUPLICATES BR# &&&&" TO DETAIL-LINE
                 INSPECT DETAIL-LINE REPLACING FIRST "&&&&" BY BR-NO
              END-IF
              MOVE AMOUNT-DUPS(LEV) TO D-TRAMT
              MOVE TOTAL-DUPS(LEV)  TO D-TOTAL
              MOVE 2 TO LN-FEED
              PERFORM WRITE-DETAIL-LINE.

           IF TOTAL-OVPAID(LEV) NOT = 0
              IF LEV = 2
                 MOVE "** TOTAL OVERPAID   BR# ALL  " TO DETAIL-LINE
              ELSE
                 MOVE "** TOTAL OVERPAID   BR# &&&&" TO DETAIL-LINE
                 INSPECT DETAIL-LINE REPLACING FIRST "&&&&" BY BR-NO
              END-IF
              MOVE AMOUNT-OVPAID(LEV) TO D-TRAMT
              MOVE TOTAL-OVPAID(LEV)  TO D-TOTAL
              MOVE 2 TO LN-FEED
              PERFORM WRITE-DETAIL-LINE.

           IF LEV = 2
              MOVE "** TOTAL POSTED     BR# ALL  " TO DETAIL-LINE
           ELSE
              MOVE "** TOTAL POSTED     BR# &&&&" TO DETAIL-LINE
              INSPECT DETAIL-LINE REPLACING FIRST "&&&&" BY BR-NO.
           MOVE AMOUNT-POSTED(LEV) TO D-TRAMT.
           MOVE TOTAL-POSTED(LEV)  TO D-TOTAL.
           MOVE 2 TO LN-FEED.
           IF LEV = 2
              PERFORM WRITE-DETAIL-LINE
           ELSE
      * BRANCH TOTALS AND THERE ARE TOTALS POSTED, BUT NO EXCEPTIONS AT ALL,
      * DO NOT PRINT TOTAL-POSTED!
              IF TOTAL-POSTED(LEV) NOT = 0
                 IF TOTAL-EXCEPTION(LEV) NOT = 0 OR
                    TOTAL-DUPS(LEV) NOT = 0 OR
                     TOTAL-OVPAID(LEV) NOT = 0
              PERFORM WRITE-DETAIL-LINE.

           MOVE SPACES TO DETAIL-LINE.

      * CLEAR TOTALS AFTER PRINTING
      
           MOVE 0 TO AMOUNT-EXCEPTION(LEV) TOTAL-EXCEPTION(LEV)
                     AMOUNT-POSTED(LEV)    TOTAL-POSTED(LEV)
                     AMOUNT-DUPS(LEV)      TOTAL-DUPS(LEV)
                     AMOUNT-OVPAID(LEV)    TOTAL-OVPAID(LEV).
      * #1564 NO PAGE BREAK
      *    MOVE EXT-CONAME-RPT-LINES TO LN-COUNT.

       EXIT-PRINT-TOTALS.
           EXIT.

      ******************************************************************
      *
      *  CREATE SPOOL DIRECTORY IN /XXX/XXXX/R#/EO
      *
      * STORE SPOOL FILE NAME AS /XXX/XXXX/R$/EO/....
      * WE NEED A FULL PATH NAME HERE ELSE DIR WILL CHANGE WHEN WE
      * CHANGE DIRECTORIES TO ANOTHER BRANCH.
      ******************************************************************
       CREATE-SPOOL-DIR SECTION.
           MOVE " " TO ERRCD.
           MOVE EXT-FILPATH-BASE      TO EX-SPOOL-BASE.
           MOVE EXT-FILPATH-MACHINE   TO EX-SPOOL-MACHINE.
           MOVE HOLD-EXT-FILPATH-DATA TO EX-SPOOL-DATA-PATH.
           MOVE TRANS-DATE            TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY           TO EX-SPOOL-DATE.
           MOVE " "                   TO EX-SPOOL-FILE.
           MOVE EX-SPOOL              TO DIR-ACCESS-BUF.
           PERFORM DIR-ACCESS-CALL.
           IF STAT NOT = "00"
              CALL "C$MAKEDIR" USING DIR-ACCESS-BUF GIVING STAT-99
              IF STAT NOT = "00"
                 IF ( EXT-ROUTE-BUF = "00" )
                    MOVE "CANNOT CREATE SPOOL DIRECTORY" TO MESS
                    PERFORM SEND-MESS
                    MOVE DIR-ACCESS-BUF TO MESS
                    PERFORM SEND-MESS
                 END-IF
                 MOVE "E" TO ERRCD
                 GO TO EXIT-CREATE-SPOOL-DIR.

           MOVE "/"         TO EX-SPOOL-FILE.
           MOVE BATCH-REFCD TO EX-SPOOL-NAME.
           MOVE "   "       TO EX-SPOOL-SUF.

      *****************************************************************
      * THEY ARE ALLOWED TO RUN THIS  MORE THAN ONCE ON THE
      * SAME TRANSACTION DATE, SO CHECK TO SEE IF REPORT EXISTS, IF
      * SO, INCREMENT THE STEP # SO WE DON'T WIPE OUT PRIOR REPORTS
      *****************************************************************
           MOVE 100 TO STEP-COUNT.
       TRY-NEXT-STEP-NO.
           MOVE STEP-COUNT TO EX-SPOOL-STEP.
           MOVE EX-SPOOL TO DIR-ACCESS-BUF.
           PERFORM DIR-ACCESS-CALL.
           IF STAT = "00"
             IF (EX-SPOOL-NAME = "REPOEX")
                GO TO EXIT-CREATE-SPOOL-DIR
             ELSE
             IF STEP-COUNT < 999
                ADD 1 TO STEP-COUNT
                GO TO TRY-NEXT-STEP-NO.

       EXIT-CREATE-SPOOL-DIR.
           EXIT.

      ******************************************************************
       BRANCH-EXCEPTION SECTION.
      ******************************************************************

      * THIS ROUTINE IS PERFORM WHEN NONE OF THE BRANCH'S ACCOUNTS CAN
      * BE PROCESSED, DUE TO CLOSED DAYS, NO OP-FILE, ETC.

           IF NOT (EXCEPTION-CODE = "LN" OR "SU")
              PERFORM OPEN-BW1-FILE
              PERFORM OPEN-LN1-FILE.

           PERFORM OPEN-BP1-FILE.
           MOVE 0 TO BP-LNNO BP-SEQNO.
           PERFORM START-BP1-FILE.

       BRANCH-EXCEPTION-NEXT-BP.
           PERFORM READ-BP1-FILE-NEXT.
           IF IO-FG NOT = 0
              GO TO END-BRANCH-EXCEPTION.

      * SKIP THE HEADER RECORD
      
           IF BP-LNNO = 0
      *       UNLOCK BP-FILE
              GO TO BRANCH-EXCEPTION-NEXT-BP.

      * LNNO OF ALL 9'S ARE ALL ZEROS
      
           IF BP-LNNO = 99999999
              MOVE "Y" TO NOT-ON-FILE-FG
              MOVE "TOTAL OF ALL ZERO ACCTNO'S" TO D-EXCEPT
              PERFORM MAIN-EXCEPTION
              GO TO BRANCH-EXCEPTION-NEXT-BP.

           IF BP-UPDFG = "U" OR "E"
      *       UNLOCK BP-FILE
              GO TO BRANCH-EXCEPTION-NEXT-BP.

           IF (EXCEPTION-CODE = "LN" OR "SU")
              MOVE "Y" TO NOT-ON-FILE-FG
           ELSE
              MOVE BP-LNNO TO LN-ACCTNO
              PERFORM READ-LN1-FILE
              IF IO-FG NOT = 0
                 MOVE "Y" TO NOT-ON-FILE-FG
              ELSE
                 MOVE " " TO NOT-ON-FILE-FG.

           EVALUATE EXCEPTION-CODE
              WHEN 'A1' MOVE "DAY IS NOT OPEN          " TO D-EXCEPT
              WHEN 'LN' MOVE "LOAN FILE NOT FOUND      " TO D-EXCEPT
              WHEN 'OP' MOVE "CANNOT RESERVE OP FILE   " TO D-EXCEPT
              WHEN 'SU' MOVE "CANNOT ACCESS BRANCH     " TO D-EXCEPT
           END-EVALUATE.
           MOVE BP-TRAMT TO ORIG-BP-TRAMT.

           PERFORM MAIN-EXCEPTION.

           GO TO BRANCH-EXCEPTION-NEXT-BP.

       END-BRANCH-EXCEPTION.
           IF NOT (EXCEPTION-CODE = "LN" OR "SU")
              PERFORM CLOSE-LN1-FILE
              PERFORM CLOSE-BW1-FILE.

      ******************************************************************
       MOVE-BTFILE SECTION.
      ******************************************************************

      *  FOR EX:   MV ../../ED/EZPAY1121123 ../../ED/DONE/EZPAY1121123

           MOVE EXT-FILPATH-BASE    TO BT-PATH-BASE.
           MOVE EXT-FILPATH-MACHINE TO BT-PATH-MACHINE.
           MOVE TRANS-DATE TO NUM-DATE.
           MOVE 0    TO NUM-CCYY.
           MOVE NUM-DATE            TO BT-PATH-BATCH-DATE.
           MOVE BATCH               TO BT-PATH-BATCH-NO.
           MOVE BT-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT NOT = "00"
              GO TO EXIT-MOVE-BTFILE.

           MOVE EXT-FILPATH-BASE    TO BT-DONE-BASE.
           MOVE EXT-FILPATH-MACHINE TO BT-DONE-MACHINE.
           MOVE BT-DONE-DIRECTORY   TO DIR-ACCESS-BUF.
           PERFORM DIR-ACCESS-CALL.
           IF STAT NOT = "00"
      *       MOVE MKDIR-CMD  TO SYSTEM-BUF
      *       MOVE ACCESS-BUF TO MKDIR-FILE
      *       PERFORM SYSTEM-CALL
              CALL "C$MAKEDIR" USING DIR-ACCESS-BUF GIVING STAT-99
              IF STAT NOT = "00"
                 MOVE "CANNOT CREATE DONE DIRECTORY" TO MESS
                 PERFORM SEND-MESS
                 GO TO EXIT-MOVE-BTFILE.
           STRING MV-CMD
                  " "
                  BT-PATH
                  " "
                  BT-DONE-PATH
                  DELIMITED BY SIZE INTO SYSTEM-BUF.
           PERFORM SYSTEM-CALL.

       EXIT-MOVE-BTFILE.
            EXIT.


      ******************************************************************
       CREATE-WK-FILE SECTION.
      ******************************************************************

      *  CREATE WK-FILE, IF RUNNING FROM LPCRON, WORK FILE IN /USR/TMP
      *  WAS NOT CREATED IN LONPA0

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.

      * WK-FILE FOR LOAN PROCESSING USED PROCESS ID XL(PROCESS ID)

           MOVE PROCESS-ID-BUF TO USER-PID TEMP-PID.
           MOVE 9 TO IO-FG.
           PERFORM OPEN-IT.
           MOVE WK-IFN TO E-FILE.
       OPEN-WK-OUTPUT.
           OPEN OUTPUT WK-FILE.
           IF ( IO-FG = 8 )
              GO TO OPEN-WK-OUTPUT.
           IF ( IO-FG = 7 )
              PERFORM CLOSE-WK-FILE
              GO TO OPEN-WK-OUTPUT.
           PERFORM CLOSE-WK-FILE.

      ************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/LONPWA_EVA.CPY".
               WHEN OTHER   MOVE "2A" TO STAT.

      *************************************
       SETUP-LINK-INFO SECTION.
      *************************************
           MOVE LONPWA-HELP--FILE TO HELP-LINK-FILE.
           MOVE LONPWA-HELP--DIR TO HELP-LINK-DIR.


      *************************************
       FORM-RESET SECTION.
      *************************************
           MOVE LPLOAN-SYSDATE TO LONPWA-SYSDATE.
           MOVE EXT-CONAME-CONAME TO LONPWA-CONAME.
           DISPLAY LONPWA-SCREEN UPON TCLP-MAIN-WINDOW-HANDLE.
           MOVE LONPWA-FORM-FIELDS TO WR-BUF.
           MOVE LONPWA-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ******************************************************************
      *  MISCELLANEOUS SECTIONS
      ******************************************************************
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/SETCTBL.CPY".
       COPY "LIBLP/LPSTAT.CPY".
       COPY "LIBGB/SPPDTH.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBLP/LPITRM.CPY".
       COPY "LIBLP/LPAMTS.CPY".
       COPY "LIBGB/REBATE.CPY".
       COPY "LIBGB/AGEING.CPY".
       COPY "LIBLP/LPUPWK.CPY".
       COPY "LIBLP/LPCEPP.CPY".
       COPY "LIBLP/LPAPRS.CPY".
       COPY "LIBLP/LPAPRZ.CPY".
       COPY "LIBLP/LPUPER.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBLP/LPPDUE.CPY".
       COPY "LIBGB/RBACT.CPY".
       COPY "LIBLP/ADDONSPR.CPY".
       COPY "LIBLP/LPLPCL.CPY".
       COPY "LIBLP/LPLXGCL.CPY".

      ******************************************************************
       MISCELLANEOUS-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       GET-SPR.
           MOVE LN-ORGST    TO SP-ORGST.
           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           MOVE LN-LAWCODE  TO SP-LAWCODE.
           IF HOLD-SP1-KEY NOT = SP1-KEY
              MOVE SP1-KEY TO HOLD-SP1-KEY
              PERFORM READ-SP1-FILE.
       TEST-STAT.
           IF STAT NOT = "00"
              PERFORM FORM-IT
              GO TO IO-ERROR.
       CLEAR-LEGEND.
           MOVE " " TO MESS.
           IF EXT-ROUTE-BUF = "00"
              PERFORM SEND-LEGEND.
       SEND-LEGEND.
           MOVE "S  A000MSEL." TO VDU.
           MOVE MESS TO VDUBUF.
           PERFORM SCREENIO.

      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLXGGS_SQL.CPY".
       COPY "LIBLP/LPLXEGS_SQL.CPY".
       COPY "LIBLP/LPLXGS_SQL.CPY".
       COPY "LIBLP/LPLTPGS_SQL.CPY".
       COPY "LIBLP/LPLTIGS_SQL.CPY".
       COPY "LIBLP/LPLPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGL/GLGIGS_SQL.CPY".
       COPY "LIBGB/GBRCGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPEDPATHIO.CPY".
       COPY "LIBLP/LPOPIN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBGL/GLGI1RN.CPY".
       COPY "LIBLP/LPCD1RN.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBLP/LPLTP1RN.CPY".
       COPY "LIBLP/LPLTI1RN.CPY".
       COPY "LIBLP/LPWKI.CPY".
       COPY "LIBLP/LPBP1IN.CPY".
       COPY "LIBGB/GBRC2RN.CPY".

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

       END-PROGRAM.
           PERFORM CLOSE-FILES.

      * RESTORE USERS ENVIRONMENT BACK TO WHAT IT WAS WHEN HE/SHE
      * STARTED THE BATCH PROCEDURE.

           MOVE HOLD-EXT-FILPATH      TO EXT-FILPATH.
           SET ENVIRONMENT "FIL"      TO EXT-FILPATH-FIL.
           MOVE ORIG-POSTNAME         TO WORK-USERID.
           SET ENVIRONMENT "POSTNAME" TO WORK-USERID.
           MOVE EXT-FILPATH-BRANCH    TO BRANCH.

       EXIT-PROG.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY LONPWA-SCREEN.
           EXIT PROGRAM.
