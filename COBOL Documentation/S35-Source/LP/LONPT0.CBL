       IDENTIFICATION DIVISION.
       PROGRAM-ID.      LONPT0.
       DATE-WRITTEN.    012386.
      *****************************************************
      *                      (LP)
      *    DESCRIPTION:  DEALER BATCH CHECK RELEASE
      * REV:
      *  042895 BAH ADDED WINDOW TO SELECT INDIVIDUAL ACCTS
      *             TO RELEASE
      *  042895 BAH ADDED OPTION TO INCLUDE,SKIP OR ONLY
      *             NEW DEAL PROCEEDS CHECKS
      *  042895 BAH ADDED TEST OF DT-REVERSE-TRCD FOR VOIDS
      *  111095 BAH ADDED REMOVED OF WK-FILE
      *  120596 MJD CHANGED TO PERFORM LPSTAT-DISPLAY AFTER
      *             READING BW-FILE
      *  032097 BAH SEND-LEGEND'S WERE MESSING UP PIGGY BACK PRINT
      *  991220 BAH ADDED PERFORM CLOSE-PR-FILE TO DECLARATIVES, CENTRAL(TN)
      * GLB 000810 DLR:SUBSTITUTE COPY MEMBER LPCXCL TO CLEAR CX-RECORD
      * GLB 000810 DLR:FIX MOVE FROM DL-NUM-->CX-DLNO;TO DL-DLNO-->CX-DL
      * GLB 000901 ONLY INCLUDE ORIGINAL PROCEEDS RECORD, CALC BALANCE
      * GLB 000910 SAVE LAST ACCTNO/BATCHID IN CX-REC
      * GLB 010807 MOVE "B" TO CX-PAID; NEED TO KNOW THAT
      *            THIS IS A BATCH REQUEST IN ORDER TO DELETE CX-REC
      * GLB 010928 NOTE TO SELF: WI/DTCKRL WINDOW USED FOR TYPE "S"
      * GLB 010928 DON'T TEST DT-PAID = "B"; AS LONG AS STATUS = " "
      * GLB 011031 FIX LOGIC FOR CREATING WK-RECORDS; NO READ "NEXT"
      * GLB 011031 ADD ABILITY TO REQUEST "UNRELEASE" OF A BATCHID
      * GLB 011107 ADD "CHECK" TOTALS; PRINT DEALER TOTALS REGARDLESS
      *            OF CHECK-TO-DEALER FLAG
      * GLB 011107 ELIMINATE SETUP-MODULE SECTION; REARRANGE CODE
      * BAH 011115 BEG-LNNO = END-LNNO WAS NOT WORKING IF YOU DID F3-ALL
      *            CLASSES, IT NEEDED TO GO TO READ-NEXT
      * GLB 011120 BUG; READING DL-FILE USING SAVE-DLNO - WRONG PAYEE!
      * GLB 020109 BUGS WITH UNRELEASING-NOT REMOVING CX OR FM REQUESTS
      *            ENHANCE REPORTS, FIX MULTIPLE SPOOL NAMES
      * GLB 020114 ADD PASSWORD FOR UNRELEASING CHECK REQUESTS
      * BLV 020115 CHECK GLOBAL PARAMETER & DEALER FLAGS AND SKIP
      *            DIRECT DEPOSIT DEALERS, REGACC PR# 1691
      * GLB 020710 RESTORE ORIGINAL LOGIC TO CREATE WK-RECORDS; PROBLEM
      *           IF MULTIPLE DEALERS/1 CHECK AND NO ".00" DEALER MASTER
      * GLB 020710 WRONG BATCHID IN CX-RECORDS IF MULTIPLE DEALERS
      *           PROBABLY THERE ALL ALONG; DIDNT MATTER UNTIL
      *           AUTO UNRELEASE WAS ADDED
      * GLB 020710 PRINT FULL BATCHID BR/CLASS/BATCH#
      * BLV 020815 FIX "NO RECORDS FOUND" SEND-MESS LOCKING SCREEN IF
      *            PIGGY-BACK PRINTING PLUS A COUPLE OF SEND-MESSES IN
      *            UNRELEASE-BATCH SECTION
      * BLV 030102 IF 1 ACCT # RANGE SELECTED, FORCE BEG & ENDING CLASS
      * BLV 030328 PROBLEM IF YOU ANSWER "N" TO "CHECKS TO DEALER BRANCH"
      *            YOU MAY GET WRONG DEALER NAME ON CHECK.  LOOKED TO
      *            BETH & ME LIKE THIS NEVER WORKED? (TESTED A60 & IT
      *            PRINTED THE .01 NAME, NOT THE .00 NAME)  IT WORKED
      *            IN A15 FOR A WHILE, THEN GAIL FIXED A BUG 7/10/02 &
      *            IT STOPPED WORKING.  IT LOOKS TO ME LIKE IF YOU SAY
      *            "N" TO CHECK-TO-DLBR, GAIL ACCUMLATES  ALL TO THE
      *            LOWEST NUMBER IN THE WORK FILE (FOR EX. YOU RELEASE
      *            DT'S FOR 100.03 & 100.04, THEY WILL ALL ACUUM TO THE
      *            WK-REC FOR 100.03).  I'M NOT SURE IF THIS IS CORRECT;
      *            ALL I CHANGED WAS TO TRY TO READ THE DEALER WITH A
      *            DL-BR OF 0 IF CHECK-TO-DLBR WAS "N", SO IF THE
      *            .00 DEALER IS VALID HIS NAME & ADDR WILL GO TO CX &
      *            ON CHECK SUMMARY ON REPORT. (PROBLEM WAS AT EXPRESS)
      * GLB 030402 BUG; PRINTS THE "NEXT" DEALER NO. ON TOTALS
      *            IF CHECKS TO DEALER BRANCH = "N"
      * BAH 050802 MOVED GET-BR UP BEFORE PRU-IN, MENU-SELECTION NEEDS
      *            BRANCH FIELDS IN CASE YOU F7-EXIT
      * BAH 121130 PASSWORD F3 AT DEALER NUMBER, CLASS AND DATE RANGE
      *                                                REGENCY PR# 4155
      * BAH 140814 ADDED TEST FOR LPSMNU IN HELP
      * BAH 150707 SCREEN HAD PRINTER 1, DEFAULT TO BRANCH PRINTER PL#890
      *
      * KEC 2016.0210 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         FIXED KEYS & RECS FOR FD-REC.
      *         CHANGED FMNO/FORMNO/NEXTFM/SETFM FROM 3 TO 4 BYTES.
      *
      * BAH 170417 ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 181004 GLOBAL BWFILE
      * BAH 181010 GLOBAL CXFILE
      * BAH 181010 GLOBAL DLFILE
      * BAH 181029 GLOBAL DTFILE
      * BAH 181115 GLOBAL FMFILE
      * BAH 190313 MOVED EDIT-FM-INITKEY TO LPWSFM-EDIT-FM-INITKEY 
      * BAH 20190614 ADDED CLEAR-FM-FILE (LPFMCL) AND SET FM-USERID
      * BAH 2022.0314 REMOVED EXT-SPANISH-FORMS AND ALL MX FORMS
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
           SELECT WK-FILE ASSIGN WK-PATH
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY WK-KEY
                  FILE STATUS FILE-STAT.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".

      *************************************************
      *
      *     WKFILE - NOT THE SAME AS RELATIVE WKFILE
      *     USED IS LOAN PROCESSING
      *
      *     WKFILE - USED TO CAPTURE THE AMOUNT OF EACH
      *              CHECK THAT WILL BE CREATED FOR
      *              EACH DEALER/DEALER BRANCH
      *
      *************************************************
       FD  WK-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK-REC.
           03  WK-KEY.
               05  WK-DLNO       PIC 9(4)V99.
               05  WK-NUM-RED   REDEFINES WK-DLNO.
                   07  WK-DL     PIC 9(4).
                   07  WK-DLSUB  PIC 99.
               05  WK-CLASS      PIC 9(3).
           03  WK-DLBR           PIC 99.
           03  WK-NO-TRANS       PIC S9(5)    COMP.
           03  WK-UNPAID         PIC S9(9)V99 COMP.
           03  WK-LAST-ACCTNO    PIC 9(8).
           03  WK-CX-BRNO        PIC 9(4).
           03  WK-CX-TYPE        PIC X(2).
           03  WK-CX-SEQNO       PIC 9(4).

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LP/LONPT0.CBL J35 01/04/24-09:53:18 >".
      ****************************
      *     F I L E   I F N      *
      ****************************
       COPY "LIBLP/LP01DL.CPY".
       COPY "LIBLP/LP01DL_SQL.CPY".
       COPY "LIBLP/LPWSDL.CPY".
       COPY "LIBLP/LP01CX.CPY".
       COPY "LIBLP/LP01CX_SQL.CPY".
       COPY "LIBLP/LPWSCX.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01DT.CPY".
       COPY "LIBLP/LP01DT_SQL.CPY".
       COPY "LIBLP/LPWSDT.CPY".
       COPY "LIBLP/LP01FM.CPY".
       COPY "LIBLP/LP01FM_SQL.CPY".
       COPY "LIBLP/LPWSFM.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       01  PR-IFN               PIC X(7)    VALUE "PRINTER".
       01  WK-IFN               PIC XX      VALUE "WK".
       01  WK-PATH              PIC X(30)   VALUE " ".

      **********************
      *   LINE WORKERS     *
      **********************
       01  LN-FEED          PIC 99       VALUE 1.
       01  LN-COUNT         PIC 999      VALUE 99.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

       01  MSEL-LIST            PIC X(5)     VALUE "MSEL.".

       01  IO-TYPE              PIC X(3).

      **********************
      *   MISC WORKERS
      **********************
       01  FIRST-TIME           PIC X        VALUE "Y".
       01  FIRST-TIME-PRINT     PIC X        VALUE "Y".
       01  FIRST-BATCH          PIC X        VALUE "Y".
       01  SUMMARY-FG           PIC X        VALUE " ".
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".
       01  SUB                  PIC 9(6) COMP.
       01  LEV                  PIC 9(6) COMP.
       01  DEALER-COUNTER       PIC 99 VALUE 0.
       01  CLASS-COUNTER        PIC 99 VALUE 0.

       01  BATCHID-SEQ          PIC S9(5)    VALUE 0.

       01  W-REASON.
           03  FILLER           PIC X(7) VALUE
               "BATCH: ".
           03  W-RBATCHID       PIC 9(10).
           03  FILLER           PIC X(6) VALUE " DLR: ".
           03  W-RDL            PIC 9(4) VALUE ZERO.

       01  W-BATCHNO            PIC 9(10).
       01  W-BATCH             REDEFINES W-BATCHNO.
           03  W-BDLBR          PIC 9(4).
           03  W-BDLCL          PIC 99.
           03  W-BNUM           PIC 9(4).

       01  SAVE-CX1-KEY.
           03  SAVE-CX-BRNO     PIC 9(4)            VALUE 0.
           03  SAVE-CX-TYPE     PIC XX              VALUE " ".
           03  SAVE-CX-SEQNO    PIC 9(4)            VALUE 0.

       01  FIRST-DLNO           PIC 9999.
       01  SAVE-DLNO            PIC 9999V99.
       01  SAVE-X              REDEFINES SAVE-DLNO.
           03  SAVE-DL          PIC 9999.
           03  SAVE-DLBR        PIC 9(2).
       01  SAVE-DLN            REDEFINES SAVE-DLNO
                                PIC 9(6).
       01  SAVE-CLASS           PIC 9(3).

       01  HOLD-DL-NUM          PIC 9(6).
       01  HOLD-DL-NUM-RED REDEFINES HOLD-DL-NUM.
           03  HOLD-DL        PIC 9(4).
           03  HOLD-DL-BR     PIC 9(2).
       01  HOLD-DLNO REDEFINES HOLD-DL-NUM   PIC 9(4)V99.
       01  FORMAT-DLNO        PIC ZZZZ.99.
       01  FORMAT-DLNO2 REDEFINES FORMAT-DLNO.
           05  FORMAT-DL      PIC Z(4).
           05  FILLER         PIC X.
           05  FORMAT-DLBR    PIC 99.
       01  HOLD-CLASS         PIC 9(3).

       01  DETAIL-UNPAID         PIC S9(9)V99 COMP   VALUE +0.

      * GLB
       01  HOLD-DUE            PIC S9(9)V99.

       01  BEG-DLNO            PIC 9(4)V99 VALUE ZERO.
       01  END-DLNO            PIC 9(4)V99 VALUE ZERO.
       01  END-DLNUM  REDEFINES END-DLNO PIC 9(6).
       01  BEG-CLASS           PIC 9(2)    VALUE ZERO.
       01  END-CLASS           PIC 9(2)    VALUE ZERO.
       01  BEG-LNNO            PIC 9(6)    VALUE ZERO.
       01  END-LNNO            PIC 9(6)    VALUE ZERO.
       01  BEG-DATE            PIC 9(8)    VALUE ZERO.
       01  END-DATE            PIC 9(8)    VALUE ZERO.
       01  WS-TYPE             PIC X       VALUE " ".
       01  CHECK-TO-DLBR       PIC X       VALUE " ".
       01  NEW-DEALS           PIC X       VALUE " ".
       01  WS-BATCH            PIC 9(4)    VALUE ZERO.
       01  WS-ACCEPT           PIC X       VALUE " ".

       01  FORMAT-AREA.
           03  FORMAT1     PIC Z(10).
           03  FILLER      PIC XX.
           03  FORMAT2     PIC Z(10).
      ******************************************************************
      *
      *     P R I N T   B U F F E R S
      *
      ******************************************************************
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
           03  FILLER           PIC X(13)    VALUE " ".
           03  FILLER           PIC X(28)    VALUE
           "DEALER BATCH CHECK RELEASE, ".
           03  H-TYPE.
               05  H-PR-UP      PIC X(23).
               05  H-BATCHX     PIC X(12).
               05  H-BATCHNOX.
                   09  H-BATCHNO    PIC 9(10).
               05  H-BATCHNOX2 REDEFINES H-BATCHNOX.
                   09  H-BATCH-NO   PIC 9(4).
                   09  FILLER       PIC X(6).

       01  HEAD3.
           03  FILLER           PIC X(9)     VALUE "DEALER  :".
           03  FILLER           PIC XX       VALUE " ".
           03  H-DEALER         PIC 9999.99.
           03  H-DEALERX       REDEFINES H-DEALER.
               05  H-DEALER-2   PIC 9999.
               05  FILLER       PIC XXX.
           03  FILLER           PIC XX       VALUE " ".
           03  H-DEALERNM       PIC X(35).

       01  HEAD4.
           03  HEAD4-DLNO       PIC X(9)     VALUE " ".
           03  FILLER           PIC X(60)    VALUE
           "TR    DATE     ACCOUNT NO.    NAME".
           03  FILLER           PIC X(55)    VALUE
           " PROCEEDS       RELEASED           PAID".

       01  HEADC.
           03  FILLER           PIC X(41)    VALUE
           "   DEALER  CLASS  NAME                   ".
           03  FILLER           PIC X(28)    VALUE
           "NO. TRANS       CHECK AMOUNT".

      *****************************************
      *    D E T A I L   L I N E
      *****************************************
       01  DETAIL-LINE          PIC X(132) VALUE SPACES.

       01  D-LINE              REDEFINES DETAIL-LINE.
           03  D-DLNO           PIC Z(4).99.
           03  FILLER           PIC XX.
           03  D-TRCD           PIC XX.
           03  FILLER           PIC XX.
           03  D-DATE           PIC 99/99/99.
           03  FILLER           PIC XXX.
           03  D-LOAN-NUM       PIC X(13).
           03  FILLER           PIC XX.
           03  D-NAME           PIC X(16).
           03  FILLER           PIC X(9).
           03  D-PROCEEDS       PIC ZZZ,ZZZ,ZZZ.99-.
           03  D-RELEASED       PIC ZZZ,ZZZ,ZZZ.99-.
           03  D-PAYMENTS       PIC ZZZ,ZZZ,ZZZ.99-.

       01  S-LINE              REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(24).
           03  S-SUMNAME        PIC X(35).
           03  S-NO             PIC ZZZZ9.
           03  S-PROCEEDS       PIC ZZZ,ZZZ,ZZZ.99-.
           03  S-RELEASED       PIC ZZZ,ZZZ,ZZZ.99-.
           03  S-PAYMENTS       PIC ZZZ,ZZZ,ZZZ.99-.
           03  FILLER           PIC X(3).
           03  S-BATCHX         PIC X(9).
           03  S-BATCHNO        PIC 9(10).

       01  C-LINE             REDEFINES DETAIL-LINE.
           03  FILLER          PIC XX.
           03  C-DLNO          PIC ZZZ9.99.
           03  FILLER          PIC XXX.
           03  C-CLASS         PIC 99.
           03  FILLER          PIC X(4).
           03  C-NAME          PIC X(25).
           03  FILLER          PIC X.
           03  C-NO-TRANS      PIC ZZZZ9-.
           03  C-STAR1         PIC X.
           03  FILLER          PIC X(4).
           03  C-DOLLARX       PIC X.
           03  C-UNPAID        PIC ZZ,ZZZ,ZZ9.99-.
           03  C-STAR2         PIC X.
           03  FILLER          PIC X(2).
           03  C-BATCHX        PIC X(9).
           03  C-BATCHNO       PIC 9(10).
           03  C-COMMENT       PIC X(40).

       01  RANGE-LN            REDEFINES DETAIL-LINE.
           03  RANGE-LINE       PIC X(25).
           03  RANGE-SELECTION  PIC X(107).
           03  RANGE-DLNO      REDEFINES RANGE-SELECTION
                                PIC Z(4).99.
           03  RANGE-LNNO      REDEFINES RANGE-SELECTION
                                PIC 999999.
           03  RANGE-DATE      REDEFINES RANGE-SELECTION
                                PIC 9999/99/99.
           03  RANGE-BATCHNO   REDEFINES RANGE-SELECTION
                                PIC 999999.

      ******************************************************************
      *    T O T A L S
      ******************************************************************
      *
      *    TOTAL-TBL:
      *       LEV(1) =  BY DEALER/CLASS OR CHECK $
      *          (2) =  BY DEALER.SUB
      *          (3) =  BY TOTAL DEALER
      *          (4) =  BY BRANCH
      *
      ******************************************************************
       01  TOTAL-TABLE            VALUE ZEROS.
           03  TOTAL-TBL          OCCURS 4 TIMES.
               05  T-NO           PIC S9(7).
               05  T-PROCEEDS     PIC S9(9)V99.
               05  T-RELEASED     PIC S9(9)V99.
               05  T-PAYMENTS     PIC S9(9)V99.


       COPY "LIBWI/DTCKRLW.CPY".

       COPY "LIBWI/DTCKRL_CMD.CPY".
       COPY "LIBWI/DTCKR3W.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBLP/LPSTATW.CPY".
       COPY "LIBLP/LPMENUW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       01  UNRELEASE-SEQ    PIC 99 VALUE 0.
       01  F3-SEQ           PIC 99 VALUE 1.
      *COPY "LIBGB/PASSWD_CMD.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/LONPT_DEF.CPY".
       COPY "LIBLP/LPSMNU_DEF.CPY".
       COPY "LIBLP/LPLNMU_DEF.CPY".
       COPY "LIBLP/LPCAMU_DEF.CPY".
       COPY "LIBLP/LPVDMU_DEF.CPY".

       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBLP/LONPT_WKS.CPY".
       COPY "LIBLP/LPSMNU_WKS.CPY".
       COPY "LIBLP/LPLNMU_WKS.CPY".
       COPY "LIBLP/LPCAMU_WKS.CPY".
       COPY "LIBLP/LPVDMU_WKS.CPY".

       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBLP/LPLOANW.CPY".
       COPY "LIBLP/LPPFBUF.CPY".

       SCREEN SECTION.
       COPY "LIBLP/LONPT_SCN.CPY".
       COPY "LIBLP/LPSMNU_SCN.CPY".
       COPY "LIBLP/LPLNMU_SCN.CPY".
       COPY "LIBLP/LPCAMU_SCN.CPY".
       COPY "LIBLP/LPVDMU_SCN.CPY".


       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME PROG-BUF UPDATE-BUF.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
                     WK-FILE  PR-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-FM1-FILE.
           PERFORM CLOSE-DT3-FILE.
           PERFORM CLOSE-DT2-FILE.
           PERFORM CLOSE-DL1-FILE.
           PERFORM CLOSE-CX1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE WK-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      ******************************************
      *    MAIN MODULE
      ******************************************
       MAIN-MODULE SECTION.

           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "LONPT" TO VDU-FORM-NAME.

           PERFORM FORM-RESET.

      * FORM-PROG SET FOR PRU-IN:
           MOVE "LONPT0" TO FORM-PROG.

           PERFORM GET-BATCH-NO.

           PERFORM GET-GPENV.

           MOVE BRANCH TO BR-NO.
           PERFORM GET-BR.
           IF IO-FG = 9
              GO TO IO-ERROR.

       GET-PR.
           MOVE SPACES TO PRU-PATH.

           MOVE "S  A020PRU." TO VDU.
           MOVE BR-PRINTER(1) TO VDUBUF.
           PERFORM SCREENIO.

           MOVE LONPT-QUEUE-POS TO Q-POS.
           MOVE LONPT-COPIES-POS TO C-POS.
           MOVE LONPT-PRU-POS TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

           PERFORM PRU-IN.
           IF STAT NOT = "00"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN 
              DESTROY LONPT-SCREEN
              PERFORM MENU-SELECTION
              IF MENU-FG = 99
                 GO TO GET-PR
              ELSE
                 GO TO END-PROGRAM.

           PERFORM INITIALIZATION.

           IF IO-TYPE = "END"
              GO TO END-PROGRAM.
           IF IO-TYPE = "CLR"
              GO TO GET-PR.

           MOVE "CHECK RELEASE IN PROGRESS..........." TO MESS.
           IF WS-ACCEPT = "P"
              MOVE "CHECK RELEASE --- PRINT ONLY........" TO MESS.
           IF WS-ACCEPT = "X"
              MOVE "UNRELEASE IN PROGRESS .............." TO MESS.
           MOVE MESS TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           IF WS-ACCEPT = "X"
              PERFORM UNRELEASE-BATCH
           ELSE
              PERFORM BATCH-RELEASE.

       END-ROUTINE.
           IF FIRST-TIME NOT = "Y"
              IF WS-ACCEPT NOT = "X"
                 PERFORM PRINT-CLASS-CHECK-TOTALS
                 PERFORM PRINT-SUBDEALER-TOTALS
                 PERFORM PRINT-DEALER-TOTALS
                 PERFORM PRINT-BRANCH-TOTALS
                 MOVE "N" TO FIRST-BATCH
                 PERFORM CREATE-CHECK-INFO
              END-IF
              PERFORM PRINT-RANGE-LINES.

           PERFORM CLOSE-PR-FILE.
           PERFORM PRU-OUT.

           IF FIRST-TIME = "Y"
              MOVE "NO RECORDS FOUND FOR SELECTED RANGE!" TO MESS
              PERFORM SEND-MESS.

           MOVE SPACES TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           IF WS-ACCEPT = "X"
              PERFORM GET-BATCH-NO.

           IF WS-ACCEPT NOT = "P"
              MOVE WK-PATH TO ACCESS-BUF
              PERFORM REMOVE-WORKFILE
              GO TO GET-PR.

      * IF PRINT ONLY SELECTED, ALLOW VIEWING
      * OF CHECKS THAT WOULD HAVE BEEN WRITTEN

       VISUAL-CHECK-REVIEW.
           MOVE "REVIEW CHECKS THAT WOULD HAVE BEEN WRITTEN? Y/N"
              TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "E RA010ACCEPT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN 
              DESTROY LONPT-SCREEN
              PERFORM MENU-SELECTION
              IF MENU-FG = 99
                 GO TO VISUAL-CHECK-REVIEW
              ELSE
                 MOVE WK-PATH TO ACCESS-BUF
                 PERFORM REMOVE-WORKFILE
                GO TO END-PROGRAM.
           IF VDUSTATUS NOT = "00"
              GO TO VISUAL-CHECK-REVIEW.
           IF NOT (VDUBUF = "Y" OR "N")
              GO TO VISUAL-CHECK-REVIEW.
           IF VDUBUF = "N"
              MOVE WK-PATH TO ACCESS-BUF
              PERFORM REMOVE-WORKFILE
              COMPUTE WS-BATCH = WS-BATCH - 1
              GO TO GET-PR.

           PERFORM DTCKR3-DISPLAY.
           COMPUTE WS-BATCH = WS-BATCH - 1.

           MOVE WK-PATH TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.

           GO TO GET-PR.


      ***************************************
       INITIALIZATION SECTION.
      ***************************************

           MOVE SPACES TO IO-TYPE.
           MOVE SPACES TO WS-ACCEPT.
           MOVE SPACES TO HOLD-SP1-KEY.

      * GET TIME OF DAY FOR BATCH NO:
           IF FIRST-BATCH = "Y"
              GO TO ENTER-BEG-DLNO.

           COMPUTE WS-BATCH = WS-BATCH + 1.

           GO TO ENTER-BEG-DLNO.

       ENTER-UNBATCH.

      * TEST PASSWORD IF UNRELEASING CHECK REQUESTS
           MOVE UNRELEASE-SEQ TO PASSWDW-SEQ.
           MOVE "TO AUTHORIZE UNRELEASE" TO PASSWDW-PROMPT2.
           PERFORM VERIFY-PASSWORD.
           IF PASSWDW-STAT = "OK"
              GO TO CONT-UNBATCH.

           MOVE "NOT AUTHORIZED TO UNRELEASE CHECK REQUESTS" TO MESS.
           PERFORM SEND-MESS.

           MOVE "SN N100UNBATCH." TO VDU.
           MOVE 0 TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE " " TO WS-ACCEPT.
           GO TO ENTER-BEG-DLNO.

       CONT-UNBATCH.
           MOVE "X" TO WS-ACCEPT.
           MOVE "EN N100UNBATCH." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN 
              DESTROY LONPT-SCREEN
              PERFORM MENU-SELECTION
              IF MENU-FG = 99
                 GO TO ENTER-BEG-DLNO
              ELSE
                 GO TO END-INIT.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-INIT.
           IF VDUNUM0 = 0
              MOVE " " TO WS-ACCEPT
              PERFORM GET-BATCH-NO
              GO TO ENTER-BEG-DLNO.
           MOVE VDUNUM0 TO W-BATCHNO.
           PERFORM VERIFY-BATCH.
           IF IO-FG NOT = 9
              MOVE "UNABLE TO LOCATE BATCH ID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-UNBATCH.
           MOVE CX-BRNO  TO SAVE-CX-BRNO.
           MOVE CX-TYPE  TO SAVE-CX-TYPE.
           MOVE CX-SEQNO TO SAVE-CX-SEQNO.

           GO TO ENTER-ACCEPT.

       ENTER-BEG-DLNO.
           MOVE "F1-PRINTER, F3-ALL DEALERS, F7-MENU" TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "ENNN042DL1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-UNBATCH.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO EXIT-INITIALIZATION.
           IF VDUSTATUS = "16"
              MOVE F3-SEQ TO PASSWDW-SEQ
              MOVE "TO USE F3 ALL" TO PASSWDW-PROMPT2
              PERFORM VERIFY-PASSWORD
              IF PASSWDW-STAT NOT = "OK"
                 GO TO ENTER-BEG-DLNO
              ELSE
                 MOVE "SN N042DL1." TO VDU
                 MOVE 1.00 TO BEG-DLNO VDUNUM2
                 PERFORM SCREENIO
                 MOVE "SN N042DL2." TO VDU
                 MOVE 9999.99 TO END-DLNO VDUNUM2
                 PERFORM SCREENIO
                 GO TO ENTER-BEG-CLASS.
           IF VDUSTATUS = "1>"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN 
              DESTROY LONPT-SCREEN
              PERFORM MENU-SELECTION
              IF MENU-FG = 99
                 GO TO ENTER-BEG-DLNO
              ELSE
                 GO TO END-INIT.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ENTER-BEG-DLNO.
           IF VDUNUM2 = 0
              GO TO ENTER-BEG-DLNO.
           MOVE VDUNUM2 TO BEG-DLNO.

       ENTER-END-DLNO.
           MOVE "^-BACK, F1-RESET, F7-MENU" TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "ENNN042DL2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-BEG-DLNO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-INIT.
           IF VDUSTATUS = "1>"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN 
              DESTROY LONPT-SCREEN
              PERFORM MENU-SELECTION
              IF MENU-FG = 99
                 GO TO ENTER-END-DLNO
              ELSE
                 GO TO END-INIT.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ENTER-END-DLNO.
           IF VDUNUM2 = 0
              GO TO ENTER-END-DLNO.
           MOVE VDUNUM2 TO END-DLNO.

           IF BEG-DLNO > END-DLNO
              MOVE "INVALID DEALER RANGE!" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-DLNO.

       ENTER-BEG-CLASS.
           MOVE "^-BACK, F1-RESET, F3-ALL CLASSES, F7-MENU" TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "ENNN020CL1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-BEG-DLNO.
           IF VDUSTATUS = "10"
              GO TO ENTER-BEG-DLNO.
           IF VDUSTATUS = "16"
              MOVE F3-SEQ TO PASSWDW-SEQ
              MOVE "TO USE F3 ALL" TO PASSWDW-PROMPT2
              PERFORM VERIFY-PASSWORD
              IF PASSWDW-STAT NOT = "OK"
                 GO TO ENTER-BEG-CLASS
              ELSE
                 MOVE "SN N020CL1." TO VDU
                 MOVE 0 TO BEG-CLASS VDUNUM0
                 PERFORM SCREENIO
                 MOVE "SN N020CL2." TO VDU
                 MOVE 99 TO END-CLASS VDUNUM0
                 PERFORM SCREENIO
                 GO TO ENTER-BEG-LNNO.
           IF VDUSTATUS = "1>"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY LONPT-SCREEN
              PERFORM MENU-SELECTION
              IF MENU-FG = 99
                 GO TO ENTER-BEG-CLASS
              ELSE
                 GO TO END-INIT.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ENTER-BEG-CLASS.
           MOVE VDUNUM0 TO BEG-CLASS.

       ENTER-END-CLASS.
           MOVE "^-BACK, F1-RESET, F7-MENU" TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "ENNN020CL2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-BEG-CLASS.
           IF VDUSTATUS = "10"
              GO TO ENTER-BEG-DLNO.
           IF VDUSTATUS = "1>"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN 
              DESTROY LONPT-SCREEN
              PERFORM MENU-SELECTION
              IF MENU-FG = 99
                 GO TO ENTER-END-CLASS
              ELSE
                 GO TO END-INIT.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ENTER-END-CLASS.
           IF VDUNUM0 = 0
              GO TO ENTER-END-CLASS.
           MOVE VDUNUM0 TO END-CLASS.

           IF BEG-CLASS > END-CLASS
              MOVE "INVALID CLASS RANGE!" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-CLASS.

           IF WS-ACCEPT = "X"
              GO TO ENTER-ACCEPT.

       ENTER-BEG-LNNO.
           MOVE "^-BACK, F1-PRINTER, F3-ALL ACCOUNTS, F7-MENU" TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "ENNN060LN1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-END-CLASS.
           IF VDUSTATUS = "10"
              GO TO ENTER-BEG-DLNO.
           IF VDUSTATUS = "16"
              MOVE "SN N060LN1." TO VDU
              MOVE 0 TO BEG-LNNO VDUNUM0
              PERFORM SCREENIO
              MOVE "SN N060LN2." TO VDU
              MOVE 999999 TO END-LNNO VDUNUM0
              PERFORM SCREENIO
              GO TO ENTER-BEG-DATE.
           IF VDUSTATUS = "1>"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN 
              DESTROY LONPT-SCREEN
              PERFORM MENU-SELECTION
              IF MENU-FG = 99
                 GO TO ENTER-BEG-LNNO
              ELSE
                 GO TO END-INIT.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ENTER-BEG-LNNO.
           IF VDUNUM0 = 0
              GO TO ENTER-BEG-LNNO.
           MOVE VDUNUM0 TO BEG-LNNO.

       ENTER-END-LNNO.
           MOVE "^-BACK, F1-RESET, F7-MENU" TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "ENNN060LN2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-BEG-LNNO.
           IF VDUSTATUS = "10"
              GO TO ENTER-BEG-DLNO.
           IF VDUSTATUS = "1>"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN 
              DESTROY LONPT-SCREEN
              PERFORM MENU-SELECTION
              IF MENU-FG = 99
                 GO TO ENTER-END-LNNO
              ELSE
                 GO TO END-INIT.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ENTER-END-LNNO.
           IF VDUNUM0 = 0
              GO TO ENTER-END-LNNO.
           MOVE VDUNUM0 TO END-LNNO.

           IF BEG-LNNO > END-LNNO
              MOVE "INVALID ACCOUNT NO. RANGE!" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-LNNO.
           IF BEG-LNNO NOT = END-LNNO
              GO TO ENTER-BEG-DATE.

           MOVE BEG-LNNO TO LN-ACCTNO.
           PERFORM GET-LN.
           IF IO-FG = 9
              MOVE "ACCOUNT IS NOT ON FILE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-LNNO.

           IF LN-OWNBR NOT = BRANCH
              MOVE SPACES TO MESS
              STRING "ACCOUNT BELONGS TO BRANCH " LN-OWNBR
                                  DELIMITED BY SIZE INTO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-LNNO.

           MOVE "SN N020CL1." TO VDU.
           MOVE LN-CLASS TO BEG-CLASS VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SN N020CL2." TO VDU.
           MOVE LN-CLASS TO END-CLASS VDUNUM0.
           PERFORM SCREENIO.

       ENTER-BEG-DATE.
           MOVE "^-BACK, F1-RESET, F3-ALL DATES, F7-MENU" TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "ERNM060DT1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-END-LNNO.
           IF VDUSTATUS = "10"
              GO TO ENTER-BEG-DLNO.
           IF VDUSTATUS = "16"
              MOVE F3-SEQ TO PASSWDW-SEQ
              MOVE "TO USE F3 ALL" TO PASSWDW-PROMPT2
              PERFORM VERIFY-PASSWORD
              IF PASSWDW-STAT NOT = "OK"
                 GO TO ENTER-BEG-DATE
              ELSE
                 MOVE "S  A000DT1." TO VDU
                 MOVE EXT-JULIAN-BEG TO BEG-DATE DATE-YYYYMMDD
                 PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
                 MOVE DATE-MMDDYY TO VDU-DATE-EDIT
                 PERFORM SCREENIO
                 MOVE "S  A000DT2." TO VDU
                 MOVE EXT-JULIAN-END TO END-DATE DATE-YYYYMMDD
                 PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
                 MOVE DATE-MMDDYY TO VDU-DATE-EDIT
                 PERFORM SCREENIO
                 GO TO ENTER-TYPE.
           IF VDUSTATUS = "1>"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN 
              DESTROY LONPT-SCREEN
              PERFORM MENU-SELECTION
              IF MENU-FG = 99
                 GO TO ENTER-BEG-DATE
              ELSE
                 GO TO END-INIT.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ENTER-BEG-DATE.
           IF VDUNUM0 = 0
              GO TO ENTER-BEG-DATE.
           MOVE VDU-DATE-YYYYMMDD TO BEG-DATE.

       ENTER-END-DATE.
           MOVE "^-BACK, F1-RESET, F7-MENU" TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "ERNM060DT2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-BEG-DATE.
           IF VDUSTATUS = "10"
              GO TO ENTER-BEG-DLNO.
           IF VDUSTATUS = "1>"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN 
              DESTROY LONPT-SCREEN
              PERFORM MENU-SELECTION
              IF MENU-FG = 99
                 GO TO ENTER-END-DATE
              ELSE
                 GO TO END-INIT.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ENTER-END-DATE.
           IF VDUNUM0 = 0
              GO TO ENTER-END-DATE.
           MOVE VDU-DATE-YYYYMMDD TO END-DATE.

           IF END-DATE < BEG-DATE
              MOVE "INVALID DATE RANGE " TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-DATE.

       ENTER-TYPE.
           MOVE "^-BACK, F1-RESET, F3-ALL TYPES, F7-MENU" TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "E  A010TYPE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-END-DATE.
           IF VDUSTATUS = "10"
              GO TO ENTER-BEG-DLNO.
           IF VDUSTATUS = "1>"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN 
              DESTROY LONPT-SCREEN
              PERFORM MENU-SELECTION
              IF MENU-FG = 99
                 GO TO ENTER-TYPE
              ELSE
                 GO TO END-INIT.
           IF VDUSTATUS = "16"
              MOVE "S  A010TYPE." TO VDU
              MOVE "*" TO WS-TYPE VDUBUF
              PERFORM SCREENIO
              GO TO ENTER-CHECK-TO-DLBR.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ENTER-TYPE.
           IF NOT (VDUBUF = "P" OR "S" OR "*")
              GO TO ENTER-TYPE.
           IF VDUBUF = SPACES
              MOVE "S  A000TYPE." TO VDU
              MOVE "*" TO VDUBUF
              PERFORM SCREENIO.
           MOVE VDUBUF TO WS-TYPE.

       ENTER-CHECK-TO-DLBR.
           MOVE "^-BACK, F1-RESET, F7-MENU" TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "E  A010DLBR." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-TYPE.
           IF VDUSTATUS = "10"
              GO TO ENTER-BEG-DLNO.
           IF VDUSTATUS = "1>"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN 
              DESTROY LONPT-SCREEN
              PERFORM MENU-SELECTION
              IF MENU-FG = 99
                 GO TO ENTER-CHECK-TO-DLBR
              ELSE
                 GO TO END-INIT.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ENTER-CHECK-TO-DLBR.
           IF NOT (VDUBUF = "Y" OR "N")
              GO TO ENTER-CHECK-TO-DLBR.
           MOVE VDUBUF TO CHECK-TO-DLBR.
           IF CHECK-TO-DLBR = "Y"
              MOVE " " TO HEAD4-DLNO
           ELSE
              MOVE " DEALER" TO HEAD4-DLNO.

       ENTER-NEW-DEALS.
           MOVE "^-BACK, F1-RESET, F7-MENU" TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "E  A010NEWDL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-CHECK-TO-DLBR.
           IF VDUSTATUS = "10"
              GO TO ENTER-BEG-DLNO.
           IF VDUSTATUS = "1>"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN 
              DESTROY LONPT-SCREEN
               PERFORM MENU-SELECTION
               IF MENU-FG = 99
                  GO TO ENTER-NEW-DEALS
               ELSE
                  GO TO END-INIT.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ENTER-NEW-DEALS.
           IF NOT (VDUBUF = "I" OR "S" OR "O")
              GO TO ENTER-NEW-DEALS.
           MOVE VDUBUF TO NEW-DEALS.

       ENTER-BATCH.
           MOVE "^-BACK, F1-RESET, F7-MENU" TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "ENNN040BATCH." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-NEW-DEALS.
           IF VDUSTATUS = "10"
              GO TO ENTER-BEG-DLNO.
           IF VDUSTATUS = "1>"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN 
              DESTROY LONPT-SCREEN
               PERFORM MENU-SELECTION
               IF MENU-FG = 99
                  GO TO ENTER-BATCH
               ELSE
                  GO TO END-INIT.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ENTER-BATCH.
           MOVE VDUNUM0 TO WS-BATCH.
           MOVE 0 TO IO-FG.
           PERFORM VERIFY-BATCH.
           IF IO-FG NOT = 0
              MOVE "DUPLICATE BATCH NUMBER, ENTER AGAIN! " TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BATCH.

      *********************************************************
      *    P -  PRINT ONLY
      *    S -  SELECT INDIVIDUAL ACCOUNT TO RELEASE
      *         CALL WI/DTCKRL.C AND THAN UPDATE AND PRINT
      *    U -  UPDATE & PRINT
      *    X -  ACCEPT "UNRELEASE BATCH" OPTION
      *********************************************************
       ENTER-ACCEPT.
           MOVE "^-BACK, F1-RESET, F7-MENU" TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "E RA010ACCEPT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
               GO TO ENTER-BATCH.
           IF VDUSTATUS = "10"
              GO TO ENTER-BEG-DLNO.
           IF VDUSTATUS = "1>"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN 
              DESTROY LONPT-SCREEN
              PERFORM MENU-SELECTION
              IF MENU-FG = 99
                 GO TO ENTER-ACCEPT
              ELSE
                GO TO END-INIT.
           IF VDUSTATUS  = "1:"
              IF FIRST-BATCH = "N"
                 PERFORM DTCKR3-DISPLAY
                 GO TO ENTER-ACCEPT.
           IF VDUSTATUS NOT = "00"
               GO TO ENTER-ACCEPT.
           IF NOT (VDUBUF = "P" OR "S" OR "U" OR "X")
              GO TO ENTER-ACCEPT.
           IF WS-ACCEPT = "X"
              IF VDUBUF NOT = "X"
                 MOVE "REJECTING UNRELEASE REQUEST"
                 TO MESS PERFORM SEND-MESS
                 MOVE "SN N100UNBATCH." TO VDU
                 MOVE 0 TO VDUNUM0
                 PERFORM SCREENIO
                 MOVE " " TO WS-ACCEPT
                 GO TO ENTER-BEG-DLNO.
           IF WS-ACCEPT NOT = "X"
              IF VDUBUF = "X"
                 MOVE "X ONLY ACCEPTABLE IF UNRELEASING"
                 TO MESS PERFORM SEND-MESS
                 GO TO ENTER-BEG-DLNO.

           MOVE VDUBUF TO WS-ACCEPT.

      *-------------------------------------------------------
      *     S E L E C T   I N V O I C E   W I N D O W  DTCKRL
      * GLB 10/30/01 MOVE THIS CODE HERE SO DON'T NEED TO
      *      RE-ENTER EVERY PARAMETER IF EXIT WINDOW
      *-------------------------------------------------------
           IF WS-ACCEPT NOT = "S"
              GO TO FINISH-INITIALIZATION.

           MOVE BEG-DLNO       TO DTCKRL-BEG-DLNO.
           MOVE END-DLNO       TO DTCKRL-END-DLNO.
           MOVE BEG-CLASS      TO DTCKRL-BEG-CLASS.
           MOVE END-CLASS      TO DTCKRL-END-CLASS.
           MOVE BEG-LNNO       TO DTCKRL-BEG-LNNO.
           MOVE END-LNNO       TO DTCKRL-END-LNNO.
           MOVE BEG-DATE       TO DTCKRL-BEG-DATE.
           MOVE END-DATE       TO DTCKRL-END-DATE.
           MOVE WS-TYPE        TO DTCKRL-WS-TYPE.
           MOVE CHECK-TO-DLBR  TO DTCKRL-CHECK-TO-DLBR.
           MOVE NEW-DEALS      TO DTCKRL-NEW-DEALS.
           MOVE WS-BATCH       TO DTCKRL-WS-BATCH.
           MOVE BRANCH         TO DTCKRL-BRANCH.
           MOVE SPACES         TO DTCKRL-EOF.

           MOVE "WI/DTCKRL " TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH DTCKRL-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF DTCKRL-EOF = "Y"
              GO TO ENTER-ACCEPT.


       FINISH-INITIALIZATION.
           MOVE SPACES TO MESS.
           PERFORM SEND-LEGEND.

      * SETUP REPORT HEADER LINES:
           MOVE 0   TO PAGE-NUMBER.
           MOVE " " TO SUMMARY-FG.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           MOVE BR-NAME TO H-NAME.
           IF WS-ACCEPT = "P"
              MOVE "  ***** PRINT ONLY *****" TO H-PR-UP
           ELSE
           IF WS-ACCEPT = "U"
              MOVE "AUTO SELECT & UPDATE" TO H-TYPE
              MOVE ": BATCH NO: " TO H-BATCHX
              MOVE WS-BATCH TO H-BATCH-NO
      *       MOVE W-BATCHNO TO H-BATCHNO
           ELSE
           IF WS-ACCEPT = "S"
              MOVE "MANUAL SELECT & UPDATE" TO H-TYPE
              MOVE ": BATCH NO: " TO H-BATCHX
              MOVE WS-BATCH TO H-BATCH-NO
      *       MOVE W-BATCHNO TO H-BATCHNO
           ELSE
           IF WS-ACCEPT = "X"
              MOVE "-- U N R E L E A S E--" TO H-TYPE
              MOVE ": BATCH NO: " TO H-BATCHX
              MOVE W-BATCHNO TO H-BATCHNO.

           MOVE 99 TO LN-COUNT.
           MOVE "Y" TO FIRST-TIME.
           MOVE "Y" TO FIRST-TIME-PRINT.
           MOVE 0 TO DEALER-COUNTER CLASS-COUNTER.

           PERFORM ZERO-T-TOTALS.
           GO TO EXIT-INITIALIZATION.

       END-INIT.
           MOVE "END" TO IO-TYPE.
       EXIT-INITIALIZATION.
           EXIT.

      *************************************************
       BATCH-RELEASE SECTION.
      *************************************************

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           MOVE MKTEMP-BUF TO WK-PATH.
           PERFORM OPEN-WK-FILE-OUTPUT.
           PERFORM CLOSE-WK-FILE.
           PERFORM OPEN-WK-FILE.

           PERFORM OPEN-PR-FILE.
           PERFORM OPEN-DT3-FILE.

       BATCH-RELEASE-START.

           MOVE SPACES     TO DT-STATUS
                              QDT3-WBEG-STATUS
                              QDT3-WEND-STATUS.
           MOVE BRANCH     TO DT-BRNO
                              QDT3-WBEG-BRNO
                              QDT3-WEND-BRNO.
           MOVE BEG-DLNO   TO DT-DLNO SAVE-DLNO.
           MOVE DT-NUM     TO QDT3-WBEG-NUM.
           MOVE END-DLNUM  TO QDT3-WEND-NUM.

           MOVE BEG-CLASS  TO DT-CLASS SAVE-CLASS
                              QDT3-WBEG-CLASS.
           MOVE END-CLASS  TO QDT3-WEND-CLASS.
           MOVE BEG-LNNO   TO DT-ACCTNO
                              QDT3-WBEG-ACCTNO.
           MOVE END-LNNO   TO QDT3-WEND-ACCTNO.
           MOVE BEG-DATE   TO DT-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QDT3-WBEG-DATE.
           MOVE END-DATE   TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QDT3-WEND-DATE.

           MOVE 0          TO DT-SEQ
                              QDT3-WBEG-SEQ.
           MOVE ALL "9"    TO QDT3-WEND-SEQ.

           PERFORM START-DT3-FILE.
       BATCH-RELEASE-NEXT.
           PERFORM READ-DT3-FILE-NEXT.
           IF (IO-FG = 9) OR (DT-BRNO NOT = DT-PATH-OWNBR)
              GO TO EXIT-BATCH-RELEASE.

      * READING ON 3RD KEY, SO IF STATUS CHANGES FROM " " EXIT:
           IF DT-STATUS NOT = " "
              GO TO EXIT-BATCH-RELEASE.

      * FILE WAS STARTED ON BEGINNING DLNO, SO IF PAST ENDING, EXIT:
           IF DT-DLNO > END-DLNO
              GO TO EXIT-BATCH-RELEASE.

           IF DT-CLASS < BEG-CLASS OR DT-CLASS > END-CLASS
              GO TO BATCH-RELEASE-UNLOCK-NEXT.

      * NOTE: THE FOLLOWING 'IF' INSURES WHEN ONLY ONE
      *       ACCTNO IS SELECTED, NO UNNESSARY READS:
           IF BEG-LNNO = END-LNNO AND WS-ACCEPT NOT = "S"
              IF BEG-CLASS = END-CLASS
                 IF DT-ACCTNO < BEG-LNNO OR DT-ACCTNO > END-LNNO
                    GO TO EXIT-BATCH-RELEASE
                 ELSE
                    NEXT SENTENCE
              ELSE
                 IF DT-ACCTNO < BEG-LNNO OR DT-ACCTNO > END-LNNO
                    GO TO BATCH-RELEASE-UNLOCK-NEXT
                 ELSE
                    NEXT SENTENCE
           ELSE
              IF DT-ACCTNO < BEG-LNNO OR DT-ACCTNO > END-LNNO
                 GO TO BATCH-RELEASE-UNLOCK-NEXT.

      * GLB  09/14/00
      *    IF DT-PROCEEDS = DT-PAYMENTS
      *       GO TO BATCH-RELEASE-UNLOCK-NEXT.
           MOVE DT-PROCEEDS   TO HOLD-DUE.
           IF DT-REVERSE = "Y"
              MOVE 0 TO HOLD-DUE.
           COMPUTE HOLD-DUE = HOLD-DUE - DT-PAYMENTS.
           IF HOLD-DUE = 0
              GO TO BATCH-RELEASE-UNLOCK-NEXT.

      * GLB 11/07/01 WHAT ABOUT REVOLVING?
      *     NEED TO FIX DTCKRL ALSO
           IF NEW-DEALS = "O"
              IF DT-TRCD NOT = "NW"
                 IF DT-REVERSE-TRCD NOT = "NW"
                    GO TO BATCH-RELEASE-UNLOCK-NEXT.
           IF NEW-DEALS = "S"
              IF DT-TRCD = "NW" OR DT-REVERSE-TRCD = "NW"
                 GO TO BATCH-RELEASE-UNLOCK-NEXT.

           IF WS-TYPE = "P"
              IF DT-TYPE NOT = "P"
                 GO TO BATCH-RELEASE-UNLOCK-NEXT.
           IF WS-TYPE = "S"
              IF DT-TYPE NOT = "S"
                 GO TO BATCH-RELEASE-UNLOCK-NEXT.

           IF DT-DATE < BEG-DATE
              GO TO BATCH-RELEASE-UNLOCK-NEXT.

           IF DT-DATE > END-DATE
              GO TO BATCH-RELEASE-UNLOCK-NEXT.

           IF WS-ACCEPT = "S"
              IF DT-SELECTED NOT = "Y"
                 GO TO BATCH-RELEASE-UNLOCK-NEXT.

      * 1/15/02 IF GLOBAL OPTION FOR DIRECT DEPOSIT IS TURNED ON,
      *         CHECK DEALER & SKIP IF HE'S DIRECT DEPOSIT:

           IF GP-DL-ACH-FG = "Y"
              MOVE DT-BRNO TO DL-BRNO
              MOVE DT-DLNO TO DL-DLNO
              PERFORM GET-DL1
              IF DL-ACH-FG = "Y"
                 GO TO BATCH-RELEASE-UNLOCK-NEXT.

      * PROCESS-DETAIL:
           SUBTRACT DT-PAYMENTS FROM DT-PROCEEDS GIVING DETAIL-UNPAID.

           PERFORM UPDATE-WK-FILE.

      *    --NO UPDATE ON PRINT ONLY:
           IF WS-ACCEPT NOT = "P"
              PERFORM UPDATE-DT3-FILE.

           IF FIRST-TIME-PRINT = "Y"
              MOVE DT-DLNO  TO HOLD-DLNO
              MOVE DT-CLASS TO HOLD-CLASS
              PERFORM SET-DEALER-HEADINGS.

           IF (CHECK-TO-DLBR = "Y" AND DT-DLNO NOT = HOLD-DLNO)
              PERFORM PRINT-CLASS-CHECK-TOTALS
              PERFORM PRINT-SUBDEALER-TOTALS
              PERFORM PRINT-DEALER-TOTALS
              MOVE DT-CLASS TO HOLD-CLASS
              MOVE DT-DLNO  TO HOLD-DLNO
              ADD 1 TO DEALER-COUNTER
              PERFORM SET-DEALER-HEADINGS.

           IF (CHECK-TO-DLBR = "N" AND DT-DL NOT = HOLD-DL)
              PERFORM PRINT-CLASS-CHECK-TOTALS
              PERFORM PRINT-SUBDEALER-TOTALS
              PERFORM PRINT-DEALER-TOTALS
              MOVE DT-CLASS TO HOLD-CLASS
              MOVE DT-DLNO  TO HOLD-DLNO
              PERFORM SET-DEALER-HEADINGS.

           IF (CHECK-TO-DLBR = "N" AND DT-DLNO NOT = HOLD-DLNO)
              PERFORM PRINT-CLASS-CHECK-TOTALS
              PERFORM PRINT-SUBDEALER-TOTALS
              MOVE DT-CLASS TO HOLD-CLASS
              MOVE DT-DLNO  TO HOLD-DLNO
              PERFORM SET-DEALER-HEADINGS
              MOVE HEAD3 TO DETAIL-LINE
              PERFORM WRITE-DETAIL-LINE.

           IF DT-CLASS NOT = HOLD-CLASS
              PERFORM PRINT-CLASS-CHECK-TOTALS
              MOVE DT-CLASS TO HOLD-CLASS.

           PERFORM DETAIL-LINE-PROCESS.
           ADD 1 TO CLASS-COUNTER.

           MOVE "N" TO FIRST-TIME.
           GO TO BATCH-RELEASE-NEXT.

       BATCH-RELEASE-UNLOCK-NEXT.
      *    UNLOCK DT-FILE.
           GO TO BATCH-RELEASE-NEXT.

       EXIT-BATCH-RELEASE.
           PERFORM CLOSE-DT3-FILE.
           PERFORM CLOSE-WK-FILE.


      ********************************************
       UPDATE-WK-FILE SECTION.
      ********************************************

      * CREATE / UPDATE WORK FILE CHECK TOTALS

           MOVE DT-DL TO WK-DL.
           MOVE 0     TO WK-DLSUB.
           MOVE 0     TO WK-CLASS.

           PERFORM START-WK-FILE.

       FIND-NEXT-WK.
           PERFORM READ-WK-FILE-NEXT.
           IF IO-FG = 9
              GO TO CREATE-WK.

           IF CHECK-TO-DLBR = "N"
              IF WK-DL > DT-DL
                 UNLOCK WK-FILE
                 GO TO CREATE-WK.
           IF CHECK-TO-DLBR = "Y"
              IF WK-DLNO > DT-DLNO
                 UNLOCK WK-FILE
                 GO TO CREATE-WK.

           IF CHECK-TO-DLBR = "N"
              IF WK-DL NOT = DT-DL
                 UNLOCK WK-FILE
                 GO TO FIND-NEXT-WK.
           IF CHECK-TO-DLBR = "Y"
              IF WK-DLNO NOT = DT-DLNO
                 UNLOCK WK-FILE
                 GO TO FIND-NEXT-WK.
           IF WK-CLASS NOT = DT-CLASS
                 UNLOCK WK-FILE
                 GO TO FIND-NEXT-WK.

           ADD 1 TO WK-NO-TRANS.
           ADD DETAIL-UNPAID TO WK-UNPAID.
           MOVE WK-DLNO TO SAVE-DLNO.
           MOVE WK-CLASS TO SAVE-CLASS.
      * GLB 09/19/00 SAVE LAST DT-ACCTNO, MAY ONLY BE 1 ACCT
           MOVE DT-ACCTNO  TO WK-LAST-ACCTNO.
           MOVE DT-DLBR    TO WK-DLBR.
           PERFORM REWRITE-WK-FILE.
           GO TO EXIT-UPDATE-WK.


       CREATE-WK.
      *    MOVE DT-DLNO       TO FIRST-DLNO.
           MOVE 1 TO WK-NO-TRANS.
           MOVE DETAIL-UNPAID TO WK-UNPAID.
           MOVE DT-DLNO       TO WK-DLNO  SAVE-DLNO.
           MOVE DT-CLASS      TO WK-CLASS SAVE-CLASS.
           MOVE DT-ACCTNO     TO WK-LAST-ACCTNO.
           MOVE DT-DLBR       TO WK-DLBR.
           PERFORM WRITE-WK-FILE.
           GO TO EXIT-UPDATE-WK.

       EXIT-UPDATE-WK.
           EXIT.

      *****************************
       UPDATE-DT3-FILE SECTION.
      *****************************
           MOVE SAVE-DLBR TO W-BDLBR.
           MOVE SAVE-CLASS TO W-BDLCL.
           MOVE WS-BATCH TO W-BNUM.
           MOVE W-BATCHNO TO DT-BATCHID.
           IF BATCHID-SEQ NOT = 99999
              ADD 1 TO BATCHID-SEQ.
           MOVE BATCHID-SEQ TO DT-BATCHID-SEQ.
           SUBTRACT DT-PAYMENTS FROM DT-PROCEEDS
                                  GIVING DT-RELEASED.
           MOVE "R" TO DT-STATUS.
           IF WS-ACCEPT = "S"
              MOVE "R" TO DT-SELECTED.
           PERFORM REWRITE-DT3-FILE.

      **************************************************
       CREATE-CHECK-INFO SECTION.
      **************************************************
           MOVE "    **  CHECK SUMMARY  **" TO S-SUMNAME.
           MOVE 3 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "Y" TO FIRST-TIME-PRINT
                       SUMMARY-FG.
           MOVE 0 TO T-NO(1) T-RELEASED(1).

           PERFORM OPEN-WK-FILE.
           MOVE 0 TO WK-DLNO WK-CLASS.

           PERFORM START-WK-FILE.
       CREATE-CHECK-INFO-NEXT.
           PERFORM READ-WK-FILE-NEXT.
           IF IO-FG = 9
              GO TO EXIT-CREATE-CHECK-INFO.

      *    UNLOCK WK-FILE.
      *    CREATE CHECK REQUESTS CX-RECORDS
           PERFORM UPDATE-CHECKS.
           PERFORM REWRITE-WK-FILE.

           PERFORM PRINT-SUMMARY-LINE.
           MOVE "N" TO FIRST-TIME-PRINT.

           GO TO CREATE-CHECK-INFO-NEXT.

       EXIT-CREATE-CHECK-INFO.
           IF FIRST-TIME-PRINT = "N"
              MOVE "TOTAL CHECKS:" TO C-NAME
              MOVE "*"             TO C-STAR1 C-STAR2
              MOVE T-NO(1)         TO C-NO-TRANS
              MOVE "$"             TO C-DOLLARX
              MOVE T-RELEASED(1)   TO C-UNPAID
              MOVE 2 TO LN-FEED
              PERFORM WRITE-DETAIL-LINE.
           PERFORM CLOSE-WK-FILE.

      *******************************************
       UPDATE-CHECKS SECTION.
      *******************************************
           IF WK-UNPAID NOT > 0
                 PERFORM UNRELEASE-BATCH
                 GO TO EXIT-UPDATE-CHECKS.

      * NO UPDATE ON PRINT ONLY:
           IF WS-ACCEPT NOT = "P"
              PERFORM UPDATE-CX1-FILE
              MOVE CX-BRNO  TO WK-CX-BRNO
              MOVE CX-TYPE  TO WK-CX-TYPE
              MOVE CX-SEQNO TO WK-CX-SEQNO
              PERFORM UPDATE-FM1-FILE.

       EXIT-UPDATE-CHECKS.
           EXIT.

      *******************************************
       UPDATE-CX1-FILE SECTION.
      *******************************************
           MOVE "UPDATING CHECK REQUEST FILE........" TO MESS.
           PERFORM TEST-SEND-LEGEND.

           MOVE BRANCH TO SAVE-CX-BRNO.
           MOVE "DL"   TO SAVE-CX-TYPE.
           MOVE 1      TO SAVE-CX-SEQNO.

           PERFORM CLEAR-CX-FILE.

           MOVE WK-UNPAID TO CX-AMT.

           MOVE WK-CLASS   TO W-BDLCL.
           MOVE WK-DLSUB   TO W-BDLBR.
           MOVE WS-BATCH   TO W-BNUM.
           MOVE W-BATCHNO  TO CX-BATCHID.
           MOVE WK-DLNO    TO W-RDL.
           MOVE CX-BATCHID TO W-RBATCHID.
           MOVE W-REASON   TO CX-REASON.

           MOVE WK-CLASS TO CX-CLASS.
           MOVE 0 TO CX-CKNO CX-REF.
           MOVE WS-BATCH TO CX-REF.
           MOVE WK-DLNO TO CX-DLNO.

           MOVE TRANS-DATE     TO CX-DATE.
           MOVE WK-LAST-ACCTNO TO CX-ACCTNO.
           MOVE "B" TO CX-PAID.

           MOVE 0 TO CX-DT-SEQ.
           MOVE WK-DLNO   TO DL-DLNO.
           IF CHECK-TO-DLBR = "N"
              MOVE 0 TO DL-BR.

           PERFORM GET-DL1.
           IF IO-BAD
              IF CHECK-TO-DLBR = "N"
                 MOVE WK-DLNO       TO DL-DLNO
                 PERFORM GET-DL1.
           IF IO-GOOD
              MOVE DL-NAME TO CX-PAYEE
              MOVE DL-ADR1 TO CX-ADR1
              MOVE DL-ADR2 TO CX-ADR2
              MOVE DL-CITY TO CX-CITY
              MOVE DL-STATE TO CX-STATE
              MOVE DL-ZIP TO CX-ZIP.
           MOVE 0 TO SUB.
       ZERO-CX-GL.
           ADD 1 TO SUB.
           MOVE ZERO TO CX-GLNO(SUB) CX-GLAMT(SUB).
           IF SUB < 5
              GO TO ZERO-CX-GL.
           PERFORM OPEN-CX1-FILE.
       CREATE-CX-TEMP.
           MOVE SAVE-CX1-KEY TO CX1-KEY.
           PERFORM WRITE-CX1-FILE.
           IF IO-FG = 0
              GO TO EXIT-UPDATE-CX1.
           IF SAVE-CX-SEQNO < 9999
              ADD 1 TO SAVE-CX-SEQNO
              GO TO CREATE-CX-TEMP.
           MOVE "REQUEST # = 9999, REQUEST NOT CREATED!" TO MESS.
           PERFORM SEND-MESS.
       EXIT-UPDATE-CX1.
           PERFORM CLOSE-CX1-FILE.

      *******************************************
       UPDATE-FM1-FILE SECTION.
      *******************************************

           MOVE "UPDATING FORM REQUEST FILE............" TO MESS.
           PERFORM TEST-SEND-LEGEND.

           PERFORM OPEN-FM1-FILE.

           MOVE SPACES                 TO LPWSFM-EDIT-FM-INITKEY.
           MOVE SAVE-CX-BRNO           TO LPWSFM-EDIT-FM-BRNO.
           MOVE SAVE-CX-TYPE           TO LPWSFM-EDIT-FM-TYPE.
           MOVE SAVE-CX-SEQNO          TO LPWSFM-EDIT-FM-SEQNO.
           MOVE 005                    TO LPWSFM-EDIT-FM-CKFMNO FM-FMNO.
           MOVE FM-PATH-OWNBR          TO FM-BRNO.
           MOVE LPWSFM-EDIT-FM-INITKEY TO FM-INITKEY.

           PERFORM READ-FM1-FILE.

           PERFORM CLEAR-FM-FILE.

           MOVE FM-PATH-OWNBR          TO FM-BRNO.
           MOVE LPWSFM-EDIT-FM-INITKEY TO FM-INITKEY.
           MOVE 005                    TO FM-FMNO.

           MOVE 0                  TO FM-ZIP.
           MOVE SPACES             TO FM-PRINT.
           MOVE 0                  TO FM-STMT-COUNTER.
           MOVE EXT-CONAME-USER-ID TO FM-USERID.
           IF IO-FG = 0
              PERFORM REWRITE-FM1-FILE
           ELSE
              PERFORM WRITE-FM1-FILE
              IF IO-FG NOT = 0
                 GO TO IO-ERROR.

           MOVE SPACES TO MESS.
           PERFORM TEST-SEND-LEGEND.

      **********************************************
       UNRELEASE-BATCH SECTION.
      **********************************************

      * IF REQUESTING THE UNRELEASE
           IF WS-ACCEPT = "X"
              PERFORM OPEN-PR-FILE
              GO TO START-UNRELEASE.

      * IF UNRELEASING DUE TO 0 OR NEGATIVE CHECK
           IF WS-ACCEPT = "P"
              IF WK-UNPAID = 0
                 MOVE
                 "CHECK IS $0.00 - NO CHECK WILL BE CREATED!"
                                                      TO MESS
              ELSE
                 MOVE "CHECK IS NEGATIVE - NO CHECK WILL BE CREATED!"
                                                      TO MESS
              END-IF
              IF PRU-LP NOT = "VDU" AND NOT = "PRU"
                 PERFORM SEND-MESS
              END-IF
              GO TO EXIT-UNRELEASE-BATCH
           ELSE
              IF WK-UNPAID = 0
                 MOVE "CHECK IS $0.00 - NO CHECK CREATED-PRESS <ENTER>"
                                                      TO MESS
              ELSE
                 MOVE "CHECK IS NEGATIVE - UNRELEASING - PRESS <ENTER>"
                                                      TO MESS
              END-IF
              IF PRU-LP NOT = "VDU" AND NOT = "PRU"
                 PERFORM SEND-MESS
              END-IF.

       START-UNRELEASE.
           PERFORM OPEN-DT2-FILE.

           MOVE DT-PATH-OWNBR TO DT-BRNO
                                 QDT2-WBEG-BRNO
                                 QDT2-WEND-BRNO.
           MOVE W-BATCHNO     TO DT-BATCHID
                                 QDT2-WBEG-BATCHID
                                 QDT2-WEND-BATCHID.
           MOVE 0             TO DT-BATCHID-SEQ
                                 QDT2-WBEG-BATCHID-SEQ.
           MOVE ALL "9"       TO QDT2-WEND-BATCHID-SEQ.

           PERFORM START-DT2-FILE.
       UNRELEASE-BATCH-NEXT.
           PERFORM READ-DT2-FILE-NEXT.
           IF IO-FG NOT = 0 OR (DT-BRNO NOT = DT-PATH-OWNBR)
              GO TO FINISH-UNRELEASE-BATCH.

      *    IF DT-PAID NOT = "B"
      *       GO TO UNRELEASE-BATCH-UNLOCK-NEXT.

           IF DT-BATCHID NOT = W-BATCHNO
              GO TO FINISH-UNRELEASE-BATCH.

      * DT-STATUS = " "  -  CHECKS HAVE NOT BEEN RELEASED THRU BATCH
      * DT-STATUS = "R"  -  CHECKS HAVE BEEN RELEASED, WAITING TO PRINT
      * DT-STATUS = "P"  -  CHECKS HAVE BEEN PRINTED
           IF DT-STATUS NOT = "R"
              GO TO UNRELEASE-BATCH-UNLOCK-NEXT.

      *------------- REQUESTED UNRELEASE ----------------
           IF WS-ACCEPT NOT = "X"
              GO TO AUTO-UNRELEASE.

           IF FIRST-TIME-PRINT = "Y"
              MOVE DT-DLNO  TO HOLD-DLNO
              MOVE DT-CLASS TO HOLD-CLASS
              PERFORM SET-DEALER-HEADINGS.

           MOVE " " TO DT-STATUS DT-SELECTED.
           MOVE 0 TO DT-RELEASED DT-BATCHID DT-BATCHID-SEQ.
           PERFORM REWRITE-DT2-FILE.

           PERFORM DETAIL-LINE-PROCESS.

           ADD 1 TO CLASS-COUNTER.


      * ACCUMULATE DEALER, CHECK, BRANCH TOTALS:
           ADD 1                TO T-NO(1) T-NO(2) T-NO(3) T-NO(4).
           ADD DT-PROCEEDS      TO T-PROCEEDS(1) T-PROCEEDS(2)
                                T-PROCEEDS(3) T-PROCEEDS(4).
           ADD DT-RELEASED      TO T-RELEASED(1) T-RELEASED(2)
                                T-RELEASED(3) T-RELEASED(4).
           ADD DT-PAYMENTS      TO T-PAYMENTS(1) T-PAYMENTS(2)
                                T-PAYMENTS(3) T-PAYMENTS(4).


           MOVE "N" TO FIRST-TIME FIRST-TIME-PRINT.

           GO TO UNRELEASE-BATCH-NEXT.
      *--------------------------------------------------


      *------------- AUTOMATIC UNRELEASE ----------------
       AUTO-UNRELEASE.
           IF DT-DLNO NOT = WK-DLNO
              GO TO UNRELEASE-BATCH-UNLOCK-NEXT.
           IF DT-CLASS NOT = WK-CLASS
              GO TO UNRELEASE-BATCH-UNLOCK-NEXT.

      * IF THE CHECK WAS 0, UPDATE THE RECORDS AS IF THEY WERE PRINTED
      * DONT NEED TO PRINT A 0 CHECK, THEY ARE DONE
           IF WK-UNPAID = 0
              ADD DT-RELEASED TO DT-PAYMENTS
              MOVE "M"        TO DT-PAID
              MOVE "P"        TO DT-STATUS
              MOVE TRANS-DATE TO DT-CKDATE
              MOVE 0          TO DT-CKNO
           ELSE
              MOVE " " TO DT-STATUS
              MOVE 0 TO DT-RELEASED DT-BATCHID DT-BATCHID-SEQ.

           PERFORM REWRITE-DT2-FILE.

           MOVE "N" TO FIRST-TIME FIRST-TIME-PRINT.
           GO TO UNRELEASE-BATCH-NEXT.

       UNRELEASE-BATCH-UNLOCK-NEXT.
      *    UNLOCK DT-FILE.
           GO TO UNRELEASE-BATCH-NEXT.

       FINISH-UNRELEASE-BATCH.
           IF WS-ACCEPT NOT = "X"
              GO TO EXIT-UNRELEASE-BATCH.

           PERFORM PRINT-DEALER-TOTALS.

           PERFORM DELETE-CHECK-REQUESTS.

           MOVE "S  A000UNBATCH." TO VDU.
           MOVE SPACES TO VDUBUF.
           PERFORM SCREENIO.

       EXIT-UNRELEASE-BATCH.
           PERFORM CLOSE-DT2-FILE.

      ***************************************************
       VERIFY-BATCH SECTION.
      ***************************************************
           PERFORM OPEN-CX1-FILE.

           MOVE BRANCH TO CX-BRNO.
           MOVE SPACES TO CX-TYPE.
           MOVE 0      TO CX-SEQNO.

           PERFORM START-CX1-FILE.
       NEXT-VERIFY-CX.
           PERFORM READ-CX1-FILE-NEXT.
           IF IO-FG = 9 OR (CX-BRNO NOT = CX-PATH-OWNBR)         
              MOVE 0 TO IO-FG
              GO TO EXIT-VERIFY-BATCH.

           IF WS-ACCEPT = "X"
              IF CX-BATCHID = W-BATCHNO
                 MOVE 9 TO IO-FG
                 GO TO EXIT-VERIFY-BATCH
              END-IF
              GO TO NEXT-VERIFY-CX.
           MOVE CX-BATCHID TO W-BATCHNO.
           IF W-BNUM = WS-BATCH
              MOVE 9 TO IO-FG
              GO TO EXIT-VERIFY-BATCH.
           GO TO NEXT-VERIFY-CX.

       EXIT-VERIFY-BATCH.
           PERFORM CLOSE-CX1-FILE.

      ***************************************************
       DELETE-CHECK-REQUESTS SECTION.
      ***************************************************

      * DELETE CHECK REQUEST IF UNRELEASING BATCH

           PERFORM OPEN-CX1-FILE.

           MOVE BRANCH TO CX-BRNO.
           MOVE SPACES TO CX-TYPE.
           MOVE 0 TO CX-SEQNO.

           PERFORM START-CX1-FILE.
       NEXT-DELETE-CX.
           PERFORM READ-CX1-FILE-NEXT.
           IF IO-FG = 9 OR (CX-BRNO NOT = CX-PATH-OWNBR)
              GO TO EXIT-DELETE-BATCH.

           IF CX-BATCHID NOT = W-BATCHNO
              GO TO NEXT-DELETE-CX.

           PERFORM DELETE-CX1-FILE.

           PERFORM OPEN-FM1-FILE.

           MOVE FM-PATH-OWNBR          TO FM-BRNO.
           MOVE SPACES                 TO LPWSFM-EDIT-FM-INITKEY.
           MOVE CX-BRNO                TO LPWSFM-EDIT-FM-BRNO.
           MOVE CX-TYPE                TO LPWSFM-EDIT-FM-TYPE.
           MOVE CX-SEQNO               TO LPWSFM-EDIT-FM-SEQNO.
           MOVE 005                    TO LPWSFM-EDIT-FM-CKFMNO
                                          FM-FMNO.
           MOVE LPWSFM-EDIT-FM-INITKEY TO FM-INITKEY.
           PERFORM READ-FM1-FILE.
           IF IO-FG = 0
              PERFORM DELETE-FM1-FILE.

           PERFORM CLOSE-FM1-FILE.

           GO TO NEXT-DELETE-CX.

       EXIT-DELETE-BATCH.
           PERFORM CLOSE-CX1-FILE.

      **************************************************
       DETAIL-LINE-PROCESS SECTION.
      **************************************************

           IF FIRST-TIME-PRINT = "Y"
              MOVE EXT-CONAME-RPT-LINES TO LN-COUNT
              MOVE 1 TO LN-FEED
              MOVE "N" TO FIRST-TIME-PRINT.

           IF CHECK-TO-DLBR = "N"
              MOVE DT-DLNO TO D-DLNO.
           MOVE DT-TRCD TO D-TRCD.
           MOVE DT-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-DATE.

           MOVE DT-ACCTNO TO LN-ACCTNO.
           PERFORM GET-LN.
           IF IO-FG = 9
              MOVE "**** MISSING ACCOUNT ****" TO D-NAME
           ELSE
              PERFORM GET-SP
              PERFORM PROCESS-BORROWER
              PERFORM LPSTAT-DISPLAY
              MOVE LPS-DISPLAY-LNNO   TO D-LOAN-NUM.

           MOVE DT-PROCEEDS TO D-PROCEEDS.
           IF WS-ACCEPT = "P"
              COMPUTE D-RELEASED = DT-PROCEEDS - DT-PAYMENTS
           ELSE
              MOVE DT-RELEASED TO D-RELEASED.
           MOVE DT-PAYMENTS TO D-PAYMENTS.

      *----------------------------------------------------------------
           PERFORM WRITE-DETAIL-LINE.

      * ACCUMULATE DEALER TOTALS:
           ADD 1 TO T-NO(1).
           ADD DT-PROCEEDS TO T-PROCEEDS(1).
           IF WS-ACCEPT = "P"
              COMPUTE T-RELEASED(1) =
                   T-RELEASED(1) + DT-PROCEEDS - DT-PAYMENTS
           ELSE
              ADD DT-RELEASED TO T-RELEASED(1).
           ADD DT-PAYMENTS TO T-PAYMENTS(1).

       END-DETAIL-LINE-PROCESS.
           EXIT.

      *****************************************
       PRINT-SUMMARY-LINE SECTION.
      *****************************************
           MOVE 3 TO LN-FEED.

           MOVE WK-DLNO TO C-DLNO
                          DL-DLNO.
           IF CHECK-TO-DLBR = "N"
              MOVE 0 TO DL-BR.
           PERFORM GET-DL1.
           IF CHECK-TO-DLBR = "N"
              IF IO-BAD
                 MOVE WK-DLNO       TO DL-DLNO
                 PERFORM GET-DL1.
           IF IO-BAD
              MOVE SPACES TO DL-ADR1
              MOVE "*UNKNOWN DEALER*" TO DL-NAME.
           MOVE DL-NAME TO C-NAME.

           MOVE WK-CLASS     TO C-CLASS.
           MOVE WK-NO-TRANS  TO C-NO-TRANS.
           MOVE "$"          TO C-DOLLARX.
           MOVE WK-UNPAID    TO C-UNPAID.
           MOVE WK-DLSUB     TO W-BDLBR.
           MOVE WK-CLASS     TO W-BDLCL.
           MOVE WS-BATCH     TO W-BNUM.
           MOVE "BATCH NO:"  TO C-BATCHX.
           MOVE W-BATCHNO    TO C-BATCHNO.
           IF WK-UNPAID <= 0
              MOVE "  SKIPPED" TO C-COMMENT.
      *----------------------------------------------------------------
           PERFORM WRITE-DETAIL-LINE.
           MOVE DL-ADR1 TO C-NAME.
           PERFORM WRITE-DETAIL-LINE.

      * ACCUMULATE CHECK TOTALS:
           IF WK-UNPAID > 0
              ADD 1         TO T-NO(1)
              ADD WK-UNPAID TO T-RELEASED(1).

       END-PRINT-SUMMARY-LINE.
           EXIT.

      *****************************************
       PROCESS-BORROWER SECTION.
      *****************************************
           MOVE LN-SSNO(1) TO BW-SSNO.        
           PERFORM GET-BW.
           IF IO-FG = 9
              MOVE "**> BORROWER MISSING <**" TO D-NAME
           ELSE
              MOVE SPACES TO D-NAME
              STRING BW-LNAME,", ",BW-FNAME DELIMITED BY "   "
                 INTO D-NAME.

       END-PROCESS-BORROWER.
           EXIT.

      *****************************************
       HEAD-PAGE SECTION.
      *****************************************

           ADD 1            TO PAGE-NUMBER.
           MOVE PAGE-NUMBER TO H-PAGE-NO.
           PERFORM GET-TIME.
           MOVE T-HH        TO H-HR.
           MOVE T-MM        TO H-MIN.
           MOVE HEAD1       TO PR-REC.
           MOVE 99          TO LN-FEED.
           PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE HEAD2       TO PR-REC.
           MOVE 1           TO LN-FEED.
           PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE HEAD3       TO PR-REC.
           MOVE 1           TO LN-FEED.
           PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF SUMMARY-FG = "Y"
              MOVE HEADC    TO PR-REC
           ELSE
              MOVE HEAD4    TO PR-REC.
           MOVE 2           TO LN-FEED.
           PERFORM WRITE-PR-REC.
           MOVE 5           TO LN-COUNT.
           MOVE 2           TO LN-FEED.

       END-HEAD-PAGE.
           EXIT.

      *****************************************
       WRITE-DETAIL-LINE SECTION.
      *****************************************
           ADD LN-FEED       TO LN-COUNT.
           IF LN-COUNT > EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.

           MOVE DETAIL-LINE  TO PR-REC.
           PERFORM WRITE-PR-REC.
           MOVE 1            TO LN-FEED.
           MOVE SPACES       TO DETAIL-LINE.

      *****************************************
       PRINT-RANGE-LINES SECTION.
      *****************************************

           MOVE 4                      TO LN-FEED.
           IF WS-ACCEPT = "X"
              MOVE "UNRELEASED BATCH ID:" TO RANGE-LINE
              MOVE W-BATCHNO              TO RANGE-BATCHNO
              PERFORM WRITE-DETAIL-LINE
              GO TO PRINT-RANGE-ACCEPT.

           MOVE "BEGINNING DEALER   :" TO RANGE-LINE.
           MOVE BEG-DLNO               TO RANGE-DLNO.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "ENDING             :" TO RANGE-LINE.
           MOVE END-DLNO               TO RANGE-DLNO.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEGINNING ACCOUNT  :" TO RANGE-LINE.
           MOVE BEG-LNNO               TO RANGE-LNNO.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "ENDING             :" TO RANGE-LINE.
           MOVE END-LNNO               TO RANGE-LNNO.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEGINNING DATE     :" TO RANGE-LINE.
           MOVE BEG-DATE               TO RANGE-DATE.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "ENDING             :" TO RANGE-LINE.
           MOVE END-DATE               TO RANGE-DATE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PURCHASE/SERVICE   :" TO RANGE-LINE.
           MOVE WS-TYPE                TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "NEW DEALS          :" TO RANGE-LINE.
           MOVE NEW-DEALS              TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "CHECKS TO DEALER BR:" TO RANGE-LINE.
           MOVE CHECK-TO-DLBR              TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

       PRINT-RANGE-ACCEPT.
           MOVE "ACCEPT             :" TO RANGE-LINE.
           MOVE WS-ACCEPT              TO RANGE-SELECTION.
           IF WS-ACCEPT = "P" MOVE "-P- PRINT ONLY"
              TO RANGE-SELECTION.
           IF WS-ACCEPT = "S" MOVE "-S- SELECT AND UPDATE"
              TO RANGE-SELECTION.
           IF WS-ACCEPT = "U" MOVE "-U- UPDATE AND PRINT"
              TO RANGE-SELECTION.
           IF WS-ACCEPT = "X" MOVE "-X- UNSELECT CHECK REQUEST"
              TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE SPACES TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

      *****************************************
       PRINT-CLASS-CHECK-TOTALS SECTION.
      *****************************************

           MOVE 1 TO LN-FEED.
           MOVE "CHECK $$ CLASS XX:" TO S-SUMNAME.
           MOVE SPACES TO S-SUMNAME.
           MOVE HOLD-DLNO TO FORMAT-DLNO.
           STRING "DEALER:" FORMAT-DLNO " CLASS: " HOLD-CLASS
              " --------->" DELIMITED BY SIZE INTO S-SUMNAME.
           MOVE "BATCH NO:"  TO S-BATCHX.
           MOVE WK-DLSUB     TO W-BDLBR.
           MOVE HOLD-CLASS   TO W-BDLCL.
           MOVE WS-BATCH     TO W-BNUM.
           MOVE W-BATCHNO    TO S-BATCHNO.

           MOVE 1 TO LEV.
           PERFORM PRINT-TOTALS.

      * ACCUMULATE DEALER, CHECK, BRANCH TOTALS:
           ADD T-NO(1)       TO T-NO(2) T-NO(3) T-NO(4).
           ADD T-PROCEEDS(1) TO T-PROCEEDS(2)
                                T-PROCEEDS(3) T-PROCEEDS(4).
           ADD T-RELEASED(1) TO T-RELEASED(2)
                                T-RELEASED(3) T-RELEASED(4).
           ADD T-PAYMENTS(1) TO T-PAYMENTS(2)
                                T-PAYMENTS(3) T-PAYMENTS(4).

           IF T-RELEASED(1) < 0
                 STRING " -------> NOTE:    BECAUSE THE AMOUNT RELEASED"
                        " IS NEGATIVE, ALL TRANSACTIONS FOR THIS DEALER"
                        "-CLASS WILL BE UN-RELEASED    :NOTE <-------"
                    DELIMITED BY SIZE INTO DETAIL-LINE
                 MOVE 2 TO LN-FEED
                 PERFORM WRITE-DETAIL-LINE
                 STRING "                                   AND THESE "
                        "TRANSACTIONS ARE NOT INCLUDED IN THE TOTALS   "
                        "                                      "
                    DELIMITED BY SIZE INTO DETAIL-LINE
                 MOVE 1 TO LN-FEED
                 PERFORM WRITE-DETAIL-LINE
                SUBTRACT T-NO(1)       FROM T-NO(2) T-NO(3) T-NO(4)
                SUBTRACT T-PROCEEDS(1) FROM T-PROCEEDS(2)
                         T-PROCEEDS(3) T-PROCEEDS(4)
                SUBTRACT T-RELEASED(1) FROM T-RELEASED(2)
                         T-RELEASED(3) T-RELEASED(4)
                SUBTRACT T-PAYMENTS(1) FROM T-PAYMENTS(2)
                         T-PAYMENTS(3) T-PAYMENTS(4).

           MOVE ZEROS TO TOTAL-TBL(1).
           MOVE ZEROS TO CLASS-COUNTER.

      *****************************************
       PRINT-SUBDEALER-TOTALS SECTION.
      *****************************************

           MOVE 1 TO LN-FEED.
           IF (CHECK-TO-DLBR = "N")
              MOVE "DEALER XXXXXXX SUBTOTALS:--------->" TO S-SUMNAME
              MOVE HOLD-DLNO TO FORMAT-DLNO
              INSPECT S-SUMNAME REPLACING FIRST "XXXXXXX" BY FORMAT-DLNO
              MOVE 2 TO LEV
              PERFORM PRINT-TOTALS.

           MOVE ZEROS TO TOTAL-TBL(2).
           MOVE ZEROS TO DEALER-COUNTER.
           MOVE 2 TO LN-FEED.

      *****************************************
       PRINT-DEALER-TOTALS SECTION.
      *****************************************

           MOVE SPACES TO H-DEALERX.
           MOVE 2 TO LN-FEED.
           MOVE HOLD-DLNO TO FORMAT-DLNO.
           IF WS-ACCEPT = "X"
              MOVE "B A T C H     XXXXXXX TOTALS:----->" TO S-SUMNAME
           ELSE
              MOVE "D E A L E R   XXXXXXX TOTALS:----->" TO S-SUMNAME
              IF CHECK-TO-DLBR = "N"
                 MOVE FIRST-DLNO TO FORMAT-DLNO.
           INSPECT S-SUMNAME REPLACING FIRST "XXXXXXX" BY FORMAT-DLNO.
           MOVE 3 TO LEV.
           PERFORM PRINT-TOTALS.

           MOVE ZEROS TO TOTAL-TBL(3).
           MOVE EXT-CONAME-RPT-LINES TO LN-COUNT.

      *****************************************
       PRINT-BRANCH-TOTALS SECTION.
      *****************************************

           MOVE SPACES TO H-DEALERX.
           MOVE "**** ALL DEALERS ****" TO H-DEALERNM.
           MOVE EXT-CONAME-RPT-LINES TO LN-COUNT.
           MOVE 3 TO LN-FEED.
           MOVE "BRANCH TOTALS:" TO S-SUMNAME.
           MOVE 4 TO LEV.
           PERFORM PRINT-TOTALS.

           MOVE ZEROS TO TOTAL-TBL(4).

      *****************************************
       PRINT-TOTALS SECTION.
      *****************************************

           MOVE T-NO(LEV) TO S-NO.
           MOVE T-PROCEEDS(LEV) TO S-PROCEEDS.
           MOVE T-RELEASED(LEV) TO S-RELEASED.
           MOVE T-PAYMENTS(LEV) TO S-PAYMENTS.

           PERFORM WRITE-DETAIL-LINE.

      *************************************
       DTCKR3-DISPLAY SECTION.
      *************************************

           MOVE WK-PATH TO DTCKR3-WK-PATH.
           MOVE "WI/DTCKR3 " TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH DTCKR3-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.

       DTCKR3-DISPLAY-EXIT.
           EXIT.

      *******************************************************
       VERIFY-PASSWORD SECTION.
      *******************************************************
           MOVE " "  TO PASSWDW-STAT.
           MOVE "LONPT0" TO PASSWDW-PGM.
           MOVE "PLEASE ENTER THE PASSWORD," TO PASSWDW-PROMPT1.
           MOVE "CL/PASSWD" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
           CANCEL FORM-PROGX.

      *****************************************
       ZERO-T-TOTALS SECTION.
      *****************************************
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 4
              MOVE 0 TO T-NO(SUB) T-PROCEEDS(SUB)
                        T-RELEASED(SUB) T-PAYMENTS(SUB)
           END-PERFORM.

       END-ZERO-T-TOTALS.
           EXIT.

      *****************************************
       SET-DEALER-HEADINGS SECTION.
      *****************************************

           IF CHECK-TO-DLBR = "Y"
              MOVE HOLD-DLNO TO H-DEALER DL-DLNO
           ELSE
              MOVE HOLD-DLNO TO H-DEALER-2
              MOVE H-DEALER-2 TO DL-DLNO
      * GLB  04/02/03  SET FIRST DEALER NO. HERE, NOT IN UPDATE
              MOVE HOLD-DL       TO FIRST-DLNO.

           PERFORM OPEN-DL1-FILE.
           MOVE DL-PATH-OWNBR TO DL-BRNO.

           PERFORM START-DL1-FILE.
           PERFORM READ-DL1-FILE-NEXT.
           IF IO-FG = 9 OR (DL-BRNO NOT = DL-PATH-OWNBR)  
              MOVE "UNKNOWN DEALER" TO H-DEALERNM
           ELSE
              MOVE DL-NAME TO H-DEALERNM.

           PERFORM CLOSE-DL1-FILE.


       COPY "LIBLP/LPFMCL.CPY".
       COPY "LIBLP/LPCXCL.CPY".

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBLP/LPSTAT.CPY".
       COPY "LIBLP/LPMENU.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/GPENV.CPY".

      ****************************************************
       FORM-RESET SECTION.
      ****************************************************
           MOVE LPLOAN-SYSDATE TO LONPT-SYSDATE.
      * NEED TO CLEAR SCREEN ON F1
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY LONPT-SCREEN UPON TCLP-MAIN-WINDOW-HANDLE.
           MOVE LONPT-FORM-FIELDS TO WR-BUF.
           MOVE LONPT-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ****************************************************
       SETUP-LINK-INFO SECTION.
      ****************************************************
           IF VDU-FORM-NAME = "LPSMNU"
              MOVE LPSMNU-HELP--FILE TO HELP-LINK-FILE
              MOVE LPSMNU-HELP--DIR  TO HELP-LINK-DIR
           ELSE
              MOVE LONPT-HELP--FILE TO HELP-LINK-FILE
              MOVE LONPT-HELP--DIR TO HELP-LINK-DIR.

      ****************************************************
       VDU-CONVERT-FIELD SECTION.
      ****************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/LONPT_EVA.CPY".
              COPY "LIBLP/LPSMNU_EVA.CPY".
              COPY "LIBLP/LPLNMU_EVA.CPY".
              COPY "LIBLP/LPCAMU_EVA.CPY".
              COPY "LIBLP/LPVDMU_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ****************************************************
       MISC-PARAGRAPHS SECTION.
      ****************************************************
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/RMFILE.CPY".
       GET-BATCH-NO.
      * GET TIME OF DAY FOR BATCH NO:
           ACCEPT TIME-NOW FROM TIME.
           MOVE H-AND-M TO WS-BATCH.
           MOVE WS-BATCH TO WR-BUF.
           MOVE "BATCH." TO WR-LIST.
           PERFORM CALL-WRFORM.

       GET-BW.
           PERFORM OPEN-BW1-FILE.
           PERFORM READ-BW1-FILE.
           PERFORM CLOSE-BW1-FILE.
       GET-LN.
           PERFORM OPEN-LN1-FILE.
           PERFORM READ-LN1-FILE.
           PERFORM CLOSE-LN1-FILE.
       GET-SP.
           MOVE LN-ORGST TO SP-ORGST.
           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           MOVE LN-LAWCODE TO SP-LAWCODE.
           IF HOLD-SP1-KEY NOT = SP1-KEY
              PERFORM OPEN-SP1-FILE
              PERFORM READ-SP1-FILE
              PERFORM CLOSE-SP1-FILE
              IF IO-FG NOT = 0
                 GO TO IO-ERROR
              ELSE
                 MOVE SP1-KEY TO HOLD-SP1-KEY.
       GET-BR.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
       SEND-LEGEND.
           MOVE MESS TO WR-BUF.
           MOVE MSEL-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       TEST-SEND-LEGEND.
           IF PRU-LP NOT = "VDU" AND NOT = "PRU"
              MOVE MESS TO WR-BUF
              MOVE MSEL-LIST TO WR-LIST
              PERFORM CALL-WRFORM.
       GET-DL1.
           PERFORM OPEN-DL1-FILE.
           PERFORM READ-DL1-FILE.
           PERFORM CLOSE-DL1-FILE.

      **********************************************
       IO-ROUTINES SECTION.
      **********************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPFMGS_SQL.CPY".
       COPY "LIBLP/LPDTGS_SQL.CPY".
       COPY "LIBLP/LPDLGS_SQL.CPY".
       COPY "LIBLP/LPCXGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBLP/LPDL1RN.CPY".
       COPY "LIBLP/LPDT2IN.CPY".
       COPY "LIBLP/LPDT3IN.CPY".
       COPY "LIBLP/LPCX1IN.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPFM1IN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBWKIN.CPY".
       COPY "LIBGB/GBPRINT.CPY".

       COPY "LIBGB/FERRORS.CPY".
           MOVE "R" TO ROUTE-CD.

      **********************************************
       END-PROGRAM SECTION.
      **********************************************
       END-RUN.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY LONPT-SCREEN.
           EXIT PROGRAM.
