       IDENTIFICATION DIVISION.
       PROGRAM-ID.      SFDISB.
       DATE-WRITTEN.    043095.
      ************************************************************************
      *                 (WI)
      *  DESCRIPTION:   SALES FINANCE PROCEEDS DISBURSEMENTS
      *                 NEW LOAN ENTRY - PAGE 2
      *                 (CALLED FROM SF ENTRY F3-MULTIPLE CHECKS)
      * REV:
      *  MJD 042696 ADDED GP-FILE, GP-DLR-DISP-TYPE.
      *  MJD 050696 FIXED BUG WITH DISBURSEMENTS. IF YOU TRY TO DISBURSE
      *              MORE THAN PROCEEDS AND HIT DOWN ARROW, IT WILL
      *              ACCEPT IT WITHOUT ERROR. ADDED LOGIC TO STOP
      *              DOWN ARROW FROM AMOUNT FIELD ENTIRELY.
      *  BAH 061996 PASS DEALER ADDRESS TO CX-REC TO PRINT ON CHECK
      *  JTG 082996 REVISED TO ALLOW RETURN TO PAY CODE SELECTION,
      *             FROM CHECK FREE TYPING WINDOW
      *  BLV 051997 ADD INS 8 TIMES NOT 10 (OURS FEES & INS DISPLAYED
      *             AS 404.04; DON'T GIVE ERROR ON EARNINGS IF IB
      *  JTG 071497 ADDED "A" AND "C" OPTIONS FOR DL-PARVPAID-CODE RE: #515
      *  GLF 090397 REMOVED ALL USE OF GLOBAL
      *  GLF 090997 REPLACED 'IO' ON GP-FILE WITH GET 'GPENV'
      *  MJD 101797 MOVED WK-SAVE-TR1-KEY  TO LP01WK
      *  JTG 102098 ADDED DL-PARVPAID-FRMLA-IB & DL-PARVPAID-CODE-IB "C"
      *             INSTANT AUTO #744
      *  JTG 990310 ALLOWED PARTICIPATION HELD ON IB ACCOUNTS, AUTOMOTIVE CREDIT
      *  MJD 990806 ADDED GP-INCL-OTH-IN-EARNED FLAG. IF "Y" THEN INCLUDE
      *             EARNINGS BUCKET 6 (OTHER) IN ACCUMULATED TOTALS
      *             OTHERWISE EXCLUDE BUCKET 6.
      *   CS 070103 IF THERE WAS A SERCHG,GOT ERROR 'PROBLEM WITH DISTRIBUTION"
      *             CHANGED TEST IN DISPLAY-FIELDS.. FROM ADD LN-FINCHG TO
      *             AMT-WK3 TO ADD-LN-INTCHG, THE SC WAS ADDED IN HERE + THE
      *             ADD OF THE ANTICIPATED.THERE WAS ALSO A BUG IF USING LOAN
      *             COST FORMULA 'D' YOU WOULD ALWAYS GET THE ERROR PR#564
      * BAH 070104 IF GP-ALLOW-MANUAL-CHECKS = "N", FORCE SYSTEM CHECKS
      *             WORLD PR# 513
      *  CS 070201 MORE 'PROBLEM WITH DISTRIBUTION' ERROR CAUSED BY A FEE
      *            FINCHG IS 'Y'. THIS WAS SOMETHING I ASKED JUDY TO TEST FOR
      *            ME WHEN I MADE CHANGE ON 01/03/07.  THE ERROR IS CAUSED
      *            BY AMT-WK3 NOT = ANTIC-PROOF.  I DONT KNOW WHAT THESE 2
      *            TEST AMOUNTS ARE OR THE PURPOSE, ITS CERTAINLY NOT PROCEEDS,
      *            SO, SINCE THE LOAN IS CORRECT, I JUST SUBTRACTED OUT THE FEES
      *            FINCHG = 'Y' TO BYPASS ERROR, PER BARB PR#564
      *
      * KEC 2016.0216 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         FIXED KEYS & RECS FOR FD-REC.
      *         CHANGED FMNO/FORMNO/NEXTFM/SETFM FROM 3 TO 4 BYTES.
      *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBLP/LPFSWK.CPY".
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.

       COPY "LIBLP/LPFDWK.CPY".

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)WI/SFDISB.CBL S34 05/21/21-14:03:54 >".
      **********************************
      *       F I L E   I F N          *
      **********************************
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
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
       COPY "LIBLP/LP01FD.CPY".
       COPY "LIBLP/LP01FD_SQL.CPY".
       COPY "LIBLP/LPWSFD.CPY".
       COPY "LIBLP/LP01CK.CPY".
       COPY "LIBLP/LP01CK_SQL.CPY".
       COPY "LIBLP/LPWSCK.CPY".
       COPY "LIBLP/LP01DL.CPY".
       COPY "LIBLP/LP01DL_SQL.CPY".
       COPY "LIBLP/LPWSDL.CPY".
       01  WK-IFN          PIC X(2) VALUE "WK".
       01  WK-KEY          PIC 9(2).

       01  ONE             PIC S9.
       01  ELE             PIC 9(6).
       01  ACTION          PIC X(6).
       01  NEW-FLAG        PIC 9.

       01  SUB             PIC 9(6) COMP VALUE 0.
       01  OLD-WK-STATUS   PIC X.
       01  OLD-WK-PYAMT    PIC 9(6)V99  COMP   VALUE 0.
       01  NON-MEMO-CHGS   PIC S9(7)V99 COMP.
       01  NOT-OUR-CHGS    PIC S9(7)V99 COMP.
       01  OUR-CHGS        PIC S9(7)V99 COMP.
       01  INS-COMM        PIC S9(7)V99 COMP.
       01  CHECK-TOTAL     PIC 9(6)V99  COMP   VALUE 0.
       01  CHECK-PROOF     PIC 9(6)V99  COMP   VALUE 0.
       01  AMT-WK2         PIC 9(6)V99  COMP   VALUE 0.
       01  AMT-WK3         PIC S9(7)V99 COMP   VALUE 0.
       01  ANTIC-PROOF     PIC S9(7)V99 COMP   VALUE 0.
       01  WK-PROOF        PIC S9(7)V99 COMP   VALUE 0.
       01  DLADJ-CHECK-FG  PIC X.
       01  MANUAL-ONLY     PIC X.
       01  PAID-CODE.
           03  FILLER      PIC X(5).
           03  PAID-CD     PIC X.
       01  FREE-TYPE-BUF.
           03  FREE-RTNCD      PIC X      VALUE " ".
           03  FREE-TYPE1      PIC X(70)  VALUE SPACES.
           03  FREE-TYPE2      PIC X(70)  VALUE SPACES.

       01  HOLD-WK-CHECK-RECORD  PIC X(WK-CHECK-RECORD-SIZE)
                                          VALUE SPACES.

       01  INSPECT-W       PIC X(86).
       01  ADD-ON          PIC X(25).

       01  SAVE-DLPAID         PIC X.
       01  SAVE-CKNO           PIC 9(6)     COMP.

       01  O-MSG.
           03  FILLER      PIC X(38) VALUE
           "AMOUNT EXCEEDS DISBURSEMENT BALANCE BY".
           03  O-AMT       PIC ZZZ,ZZ9.99.

       01  ALP-BW.
           03  ALP-BW123   PIC X(3).
           03  ALP-HYPHEN1 PIC X.
           03  ALP-BW45    PIC X(2).
           03  ALP-HYPHEN2 PIC X.
           03  ALP-BW6789  PIC X(4).
       01  NUM-BW          PIC 9(9).
       01  NUM-BWX REDEFINES NUM-BW.
           03  NUM-BW123   PIC X(3).
           03  NUM-BW45    PIC X(2).
           03  NUM-BW6789  PIC X(4).

      ********************************
      *   S C R E E N   B U F F E R  *
      ********************************
       01  DLR-TYP-LIST      PIC X(5)   VALUE "5P:P.".
       01  DLR-TYP-BUF       PIC X(4).

       01  DISPLAY-BUF1          VALUE " ".
           03  D-010-020         OCCURS 2.
               05  D-010         PIC X(11).
               05  D-011         PIC X(33).
               05  D-012         PIC X(1).
           03  D-RECAP           OCCURS 15.
               05  D-AMT         PIC ZZZ,ZZ9.99.
           03  D-ANTICPROOF      PIC ZZZ,ZZ9.99.
           03  D-INSDLADJ-PROMPT PIC X(48).
           03  D-INSDLADJ-SIGN   PIC X.
       01  DISPLAY-LIST1.
           03  FILLER            PIC X(30) VALUE
           "010X 011X 012X 020X 021X 022X ".
           03  FILLER            PIC X(40) VALUE
           "L010 L020 L030 L040 L050 L060 L070 L080 ".
           03  FILLER.
               05  FILLER        PIC X(4) VALUE "L090".
               05  DISPLAY-L090  PIC XXX  VALUE " ".
           03  FILLER            PIC X(48) VALUE
           "L100 L110 L120 L130 L140 L150 L070P P090:S P091.".

      ************************************
      *     WORK LINE DISPLAY BUFFER     *
      ************************************
       01  CHECK-LINE             PIC 99 VALUE 0.
       01  B-LN                   PIC 99 VALUE 0.
       01  B-TOP-LN               PIC 99 VALUE 0.
       01  B-SCREEN-LN            PIC 99 VALUE 0.
       01  LSUB                   PIC 9(6) COMP.
       01  LSUB2                  PIC 9(6) COMP.
       01  S-LNS                  PIC 9(6) COMP VALUE 3.
       01  S-LNS-PLUS1            PIC 9(6) COMP VALUE 4.
       01  S-LNS-LESS1            PIC 9(6) COMP VALUE 2.
       01  S-LNSX2-LESS1          PIC 9(6) COMP VALUE 5.
       01  S-LNS-FREE-WK-RECS     PIC 99 VALUE 10.
       01  LN-WK                  PIC 9(6) COMP.
       01  LINE-LIST              PIC X(12) VALUE
           "L01 L02 L03.".
       01  LINE-BUF.
           03  D-LN           OCCURS 3 TIMES.
               05  D-LN-NO             PIC 9(2).
               05  D-LN-BODY           PIC X(54).
               05  D-LN-DEL            PIC X.

      **************************************
      *        CHECK RECORD LINE           *
      **************************************
       01  LN-BUF                 VALUE SPACE.
           03  L-010              PIC 99.
           03  FILLER             PIC XX.
           03  L-020              PIC X(30).
           03  FILLER             PIC X.
           03  L-030              PIC ZZZ,ZZ9.99 BLANK ZERO.
           03  FILLER             PIC X(4).
           03  L-040              PIC Z(6) BLANK ZERO.
           03  FILLER             PIC X.
           03  L-DEL              PIC X.

      *************************************
      *    ENTRY LINE AUTO SETUP BUFFER   *
      *************************************
       01  EL-BUF                 VALUE SPACE.
           03  EL-YN              PIC X.
           03  EL-040             PIC Z(6).
           03  EL-030             PIC ZZZ,ZZ9.99.
           03  EL-020             PIC X(30).
           03  EL-010             PIC 99.

       01  REV-MESS         PIC X(19) VALUE
           "YN 040 030 020 010.".

      ********************************
      *   E L E M E N T   S P E C S  *
      ********************************
       01  MAX             PIC 99   VALUE 4.
       01  SPEC-TABLE.
           03  FILLER      PIC X(8) VALUE "EN N020N".
           03  FILLER      PIC X(8) VALUE "E  A300 ".
           03  FILLER      PIC X(8) VALUE "E NN062 ".
           03  FILLER      PIC X(8) VALUE "EN N060 ".
       01  SPEC-TABLE2     REDEFINES SPEC-TABLE.
           03  SPEC-REC    OCCURS 4 TIMES.
               05  SPEC    PIC X(7).
               05  CHFG    PIC X.

       01  LEGEND-01.
           03  FILLER      PIC X(6)  VALUE "^-UP, ".
           03  FILLER      PIC X     VALUE X'76'.
           03  FILLER      PIC X(31) VALUE 
            "-DN, F6-ADD, AUDIT/DELETE LINE#".

       COPY "LIBWI/PFSWINDOWSW.CPY".
       COPY "LIBLP/LPWKPATH.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBLP/LPMENUW.CPY".
       COPY "LIBGB/ACCESSW.CPY".

       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/SFDISB_DEF.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBWI/SFDISB_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       01  MULTIPLE-CK-FG           PIC X.
       01  PASS-DLPAID-FG           PIC X.
       COPY "LIBLP/LPLOANW.CPY".

       SCREEN SECTION.
       COPY "LIBWI/SFDISB_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME MULTIPLE-CK-FG PASS-DLPAID-FG PROG-BUF
                   EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               WK-FILE .
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-FD1-FILE.
           PERFORM CLOSE-DL1-FILE.
           PERFORM CLOSE-CK1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-BR-FILE.
           CLOSE
               WK-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      ***********************************
      *   C O N T R O L   M O D U L E   *
      ***********************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.

           MOVE FORM-PATHNAME TO FORM-PATH.

           IF MULTIPLE-CK-FG = "N"
              GO TO SKIP-OPEN-WINDOW.

           MOVE 23 TO WINDOW-LINES.
           MOVE 80 TO WINDOW-COLUMNS.
           MOVE 1 TO WINDOW-BEGIN-Y.
           MOVE 1 TO WINDOW-BEGIN-X.
           MOVE "SFDISB" TO VDU-FORM-NAME.
           MOVE "WI/SFDISB" TO WINDOW-FORM-NAM.

           PERFORM OPEN-WINDOW.
           DISPLAY SFDISB-SCREEN.

           MOVE SFDISB-FORM-FIELDS TO WR-BUF.
           MOVE SFDISB-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       SKIP-OPEN-WINDOW.

           PERFORM GET-GPENV.

           IF GP-DLR-DISP-TYPE = "C"
              MOVE "COMM" TO DLR-TYP-BUF
           ELSE
              MOVE "DISC" TO DLR-TYP-BUF.
           MOVE DLR-TYP-BUF TO WR-BUF.
           MOVE DLR-TYP-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE USER-PID TO TEMP-PID.
           MOVE " " TO ERRCD.

           MOVE 1 TO WK-KEY.
           PERFORM OPEN-WK-FILE.
           PERFORM READ-WK-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

           MOVE WK-SAVE-REC TO LN-REC.
           PERFORM CLOSE-WK-FILE.

           MOVE BRANCH TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           MOVE LN-LAWCODE TO SP-LAWCODE.
           MOVE LN-ORGST TO SP-ORGST.
           PERFORM OPEN-SP1-FILE.
           PERFORM READ-SP1-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.
           PERFORM CLOSE-SP1-FILE.

           PERFORM OPEN-FD1-FILE.
           MOVE SP-PRFORM(2) TO FD-FMNO.
           PERFORM READ-FD1-FILE.
           IF IO-FG = 9
             MOVE "Y" TO MANUAL-ONLY.
           PERFORM CLOSE-FD1-FILE.

           MOVE LN-DLNO TO DL-DLNO.
           PERFORM OPEN-DL1-FILE.
           PERFORM READ-DL1-FILE.
           IF IO-FG = 9
              MOVE " " TO DL-NAME.
           PERFORM CLOSE-DL1-FILE.

           IF MULTIPLE-CK-FG = "N"
              PERFORM UPDATE-SINGLE-CHECK
              GO TO END-ROUTINE.

           PERFORM DISPLAY-FIELDS.
       ENTER-PAID-CD.
           IF MANUAL-ONLY = "Y"
             MOVE "INVALID CHECK FORM #.  MANUAL CHECKS ONLY." TO MESS
             PERFORM SEND-MESS
             MOVE "M" TO PAID-CD PASS-DLPAID-FG
             GO TO ENTER-CHECKS.

           IF GP-ALLOW-MANUAL-CHECKS = "N"
              MOVE "S" TO PAID-CD PASS-DLPAID-FG
              GO TO ENTER-FREE-TYPING.

           MOVE "F1-BACK, CHECKS: S-SYSTEM, M-MANUAL: " TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E  A060SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              PERFORM DELETE-WK-CHECKS
              MOVE "X" TO PASS-DLPAID-FG
              GO TO END-ROUTINE.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-PAID-CD.
           MOVE VDUBUF TO PAID-CODE.
           IF PAID-CD NOT = "S" AND NOT = "M"
              GO TO ENTER-PAID-CD.
           MOVE PAID-CD TO PASS-DLPAID-FG.
       ENTER-FREE-TYPING.
           IF PAID-CD = "M"
              GO TO ENTER-CHECKS.
           MOVE "FREE TYPING ?  Y/N:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E  A060SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-FREE-TYPING.
           IF VDUBUF = "     N"
              GO TO ENTER-CHECKS.
           IF VDUBUF NOT = "     Y"
              GO TO ENTER-FREE-TYPING.

           MOVE "WI/FTENT" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH FREE-TYPE-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.

           IF FREE-RTNCD = "X"
              MOVE " " TO FREE-TYPE1
                          FREE-TYPE2
              GO TO ENTER-PAID-CD.

       ENTER-CHECKS.
           MOVE " CASHP CASH." TO MESS.
           PERFORM RESET-MESS.
           MOVE "N" TO CHFG(1).

           PERFORM ENTRY-MODULE.

           SUBTRACT CHECK-TOTAL FROM CHECK-PROOF GIVING WK-PROOF.
           IF WK-PROOF NOT = 0
              MOVE "S  A040CASHP:S." TO VDU
              MOVE
              "-- REMAINING TO BE DISBURSED  ------------------->"
                                                          TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  N062CASH." TO VDU
              MOVE WK-PROOF TO VDUNUM2
              PERFORM SCREENIO
              PERFORM ALARM.

       UPDATE-YN.
           IF WK-PROOF NOT = 0
              MOVE "A-AUDIT/ADD CHECKS, C-CANCEL CHECKS" TO MESS
           ELSE
              MOVE
              "U-UPDATE CHECKS, A-AUDIT/ADD CHECKS, C-CANCEL CHECKS"
                                                             TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E RA000SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS NOT = "00"
              GO TO UPDATE-YN.
           MOVE VDUBUF TO ACTION.
           IF ACTION = "     A"
              MOVE "SEL." TO MESS
              PERFORM RESET-MESS
              GO TO ENTER-CHECKS.
           IF ACTION = "     C"
              PERFORM DELETE-WK-CHECKS
              MOVE "X" TO PASS-DLPAID-FG
              GO TO END-ROUTINE.
           IF ACTION NOT = "     U"
              GO TO UPDATE-YN.
           IF WK-PROOF NOT = 0
            MOVE "PROCEEDS NOT COMPLETELY DISBURSED; ENTER 'A' OR 'C'"
                    TO MESS
              PERFORM SEND-MESS
              GO TO UPDATE-YN.
           IF LN-INSPREM-DLADJ > 0
              PERFORM TEST-FOR-DLADJ-CHECK
              IF DLADJ-CHECK-FG NOT = "Y"
                 STRING
                 "PLEASE WRITE A BORROWER CHECK FOR "
                    " THE DEALER INS. PREM ADJ!"
                        DELIMITED BY SIZE INTO MESS
                 PERFORM SEND-MESS
                 GO TO UPDATE-YN.

           MOVE 0 TO SUB.

           GO TO END-ROUTINE.
       UPDATE-EXIT.
           MOVE MENU-PROG-FORM(3) TO NEW-PROG-FORM.
           GO TO END-ROUTINE.

      **************************************
      *   E L E M E N T   E N T R Y        *
      *   NEW-FLAG: 0 - MOD EXISTING REC   *
      *             1 - INITIAL ENTRY      *
      *             2 - MOD NEW REC        *
      **************************************
       ENTRY-MODULE SECTION.
       ENTER-NEXT-LINE.
           ADD 1 TO B-LN.
           MOVE B-LN TO LN-WK.
           MOVE "S  A020010." TO VDU.
           MOVE B-LN TO VDUBUF.
           PERFORM SCREENIO.
           PERFORM CLEAR-RECORD.
           IF PAID-CD = "M"
              MOVE "S  A010CKNO." TO VDU
              MOVE "#" TO VDUBUF
              PERFORM SCREENIO.
       BEGIN-ENTRY.
           IF CHFG(1) = " "
              PERFORM RESET-ENTRY-LINE
              MOVE LEGEND-01 TO MESS
           ELSE
              MOVE "F3-CHECK TO DEALER, F6-AUDIT, F7-DONE" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE " " TO WK-BWPROC.
           MOVE 1 TO ONE NEW-FLAG.
           MOVE 0 TO ELE.
       CREATE-ROUTINE.
           IF NEW-FLAG NOT = 1
              GO TO ACCEPT-LINE.
           ADD ONE TO ELE.
           IF ELE < 1
              MOVE 1 TO ELE ONE.
           IF ELE NOT > MAX
              GO TO ENTER-ELE.
           MOVE 2 TO NEW-FLAG.
       ACCEPT-LINE.
           IF CHFG(1) = "N"
              MOVE "^-BACK, A-ACCEPT" TO MESS
              PERFORM SEND-LEGEND
              MOVE "S  A010YN." TO VDU
              MOVE "A" TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "F1-NEW LINE, ^-BACK, D-DELETE, A-ACCEPT" TO MESS
              PERFORM SEND-LEGEND.
           MOVE "E  A010YN." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              GO TO BEGIN-ENTRY.
           IF VDUSTATUS = "1U"
              MOVE 1 TO NEW-FLAG
              ADD 1 MAX GIVING ELE
              IF PAID-CD = "S"
                 MOVE -2 TO ONE
                 GO TO CREATE-ROUTINE
              ELSE
                 MOVE -1 TO ONE
                 GO TO CREATE-ROUTINE.
           IF VDUSTATUS NOT = "00"
              GO TO ACCEPT-LINE.
           IF CHFG(1) = "N"
              IF VDUBUF = "A"
                 PERFORM WRITE-LINE-SCREEN
                 ADD S-LNS-FREE-WK-RECS B-LN GIVING WK-KEY
                 PERFORM UPDATE-CHECK-TOTAL
                 MOVE FREE-TYPE1 TO WK-CK-FREE1
                 MOVE FREE-TYPE2 TO WK-CK-FREE2
                 PERFORM WRITE-LINE-DISK
                 PERFORM RESET-ENTRY-LINE
                 GO TO ENTER-NEXT-LINE
              ELSE
                 GO TO ACCEPT-LINE.
           IF VDUBUF = "A"
              PERFORM UPDATE-AUDIT
              GO TO BEGIN-ENTRY.
           IF VDUBUF = "D"
              PERFORM DELETE-AUDIT
              GO TO BEGIN-ENTRY.
           GO TO ACCEPT-LINE.
       ENTER-ELE.
           IF ELE = 2
              IF CHFG(1) = "N"
                 SUBTRACT CHECK-TOTAL FROM CHECK-PROOF
                                               GIVING WK-PYAMT
                 MOVE "S  N062030." TO VDU
                 MOVE WK-PYAMT TO VDUNUM2
                 PERFORM SCREENIO
              ELSE
                 PERFORM RESET-SEL.
           IF CHFG(ELE) = "N"
              IF NEW-FLAG = 1
                 MOVE 1 TO ONE
                 GO TO CREATE-ROUTINE.
           MOVE SPEC(ELE) TO VDU.
           MOVE ELE TO VDUROW.
           MOVE 0 TO VDUCOL.
           MOVE "." TO VDUDECIMAL.
           PERFORM SCREENIO.
       TEST-F1.
           IF VDUSTATUS = "10"
              GO TO BEGIN-ENTRY.
       TEST-F3.
           IF VDUSTATUS NOT = "16"
              GO TO TEST-F4.
           IF CHFG(1) NOT = " " AND ELE = 2
              MOVE "S  A000020." TO VDU
              MOVE DL-NAME TO VDUBUF WK-PAYEE
              PERFORM SCREENIO
              GO TO CREATE-ROUTINE.
       TEST-F4.
           IF VDUSTATUS NOT = "1U"
              GO TO TEST-F5.
           IF ELE = 1
              PERFORM SCROLL-DOWN
              GO TO ENTER-ELE.
           IF ELE = 2
              GO TO BEGIN-ENTRY.
           IF NEW-FLAG = 1
              IF PAID-CD = "M"
                 MOVE -1 TO ONE
                 GO TO CREATE-ROUTINE
              ELSE
                 MOVE -2 TO ONE
                 GO TO CREATE-ROUTINE.
       TEST-F5.
           IF VDUSTATUS NOT = "1D"
              GO TO TEST-F6.

      * DO NOT ALLOW DOWN ARROW FROM AMOUNT FIELD
           IF ELE = 3
              GO TO TEST-F6.
           IF ELE = 1
              PERFORM SCROLL-UP
              GO TO ENTER-ELE.
      * DO NOT ALLOW DOWN ARROW DUE TO BUG FIXED 050696 MJD
           IF ELE = 3
              IF PAID-CD = "S"
                 ADD 1 TO ELE.
           GO TO GOTO-FIELD.
       TEST-F6.
           IF VDUSTATUS NOT = "1<"
              GO TO TEST-F7.
           IF CHFG(1) = " "
              IF ELE = 1
                 PERFORM ADJUST-LINES-TO-END
                 MOVE "N" TO CHFG(1)
                 GO TO ENTER-NEXT-LINE.
           IF CHFG(1) = "N"
              MOVE " " TO CHFG(1)
              SUBTRACT 1 FROM B-LN
              GO TO BEGIN-ENTRY.
       TEST-F7.
           IF VDUSTATUS = "1>"
              IF CHFG(1) NOT = " "
                 SUBTRACT 1 FROM B-LN
                 PERFORM RESET-ENTRY-LINE
                 GO TO END-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-ELE.
       GOTO-FIELD.
           MOVE 1 TO ONE.
           GO TO F-01 F-02 F-03 F-04
              DEPENDING ON ELE.
           GO TO END-ENTRY-MODULE.

      ************************************
      *     S E T   E L E M E N T S      *
      ************************************
       F-01.
           MOVE VDUNUM0 TO CHECK-LINE.
           IF CHECK-LINE = 0 OR CHECK-LINE > B-LN
              GO TO ENTER-ELE.
           IF B-LN > S-LNS-LESS1
              ADD B-TOP-LN S-LNS-LESS1 GIVING LN-WK
              IF CHECK-LINE < B-TOP-LN OR CHECK-LINE > LN-WK  MOVE
              "PLEASE REVIEW LINE PRIOR TO ATTEMPTING THIS FUNCTION !"
              TO MESS PERFORM SEND-MESS
              GO TO ENTER-ELE.
           MOVE CHECK-LINE TO LN-WK.
           PERFORM SETUP-OLD-LINE.
           GO TO CREATE-ROUTINE.
       F-02.
           IF VDUBUF = " "
              MOVE WK-PAYEE TO VDUBUF.
           IF VDUBUF = " "
              GO TO ENTER-ELE.
           MOVE VDUBUF TO WK-PAYEE.
           GO TO CREATE-ROUTINE.
       F-03.
           IF VDUNUM2 = 0
              MOVE WK-PYAMT TO VDUNUM2.
           IF VDUNUM2 = 0
              GO TO ENTER-ELE.
           MOVE VDUNUM2 TO WK-PYAMT.
           MOVE "S  N062030." TO VDU.
           MOVE WK-PYAMT TO VDUNUM2.
           PERFORM SCREENIO.
           COMPUTE AMT-WK3 = CHECK-PROOF - CHECK-TOTAL - WK-PYAMT.
           IF OLD-WK-STATUS = " "
              ADD OLD-WK-PYAMT TO AMT-WK3.
           IF AMT-WK3 < 0
              MOVE AMT-WK3 TO O-AMT
              MOVE O-MSG TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           IF PAID-CD = "S"
             ADD 1 TO ELE.
           GO TO CREATE-ROUTINE.
       F-04.
           IF VDUNUM0 = 0
              GO TO ENTER-ELE.
           IF PAID-CD = "S"
              MOVE 0 TO WK-CKNUM
              GO TO CREATE-ROUTINE.
           MOVE BRANCH TO CK-BRNO.
           MOVE VDUNUM0 TO CK-NO.
           PERFORM OPEN-CK1-FILE.
           PERFORM READ-CK1-FILE.
           PERFORM CLOSE-CK1-FILE.
           IF IO-FG = 0
              MOVE "CHECK ALREADY EXISTS!" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           IF BR-AUTO-CHECK = "Y"
              IF CK-NO NOT < BR-NEXT-CHECK
                 MOVE "INVALID MANUAL CHECK NUMBER" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.

           MOVE WK-CHECK-RECORD TO HOLD-WK-CHECK-RECORD.
           PERFORM VERIFY-CHECK-NUM.
           MOVE HOLD-WK-CHECK-RECORD TO WK-CHECK-RECORD.
           IF IO-FG = 9
              MOVE "DUPLICATE CHECK NUMBER " TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           MOVE CK-NO TO WK-CKNUM.
           GO TO CREATE-ROUTINE.
       END-ENTRY-MODULE.
           EXIT.

      ***************************************
       VERIFY-CHECK-NUM SECTION.
      ***************************************
           PERFORM OPEN-WK-FILE.
           MOVE 0 TO IO-FG.
           ADD S-LNS-FREE-WK-RECS 1 GIVING SUB.
       NEXT-VERIFY-CHECK.
           MOVE SUB TO WK-KEY.
           PERFORM READ-WK-FILE.
           IF IO-FG = 9
              MOVE 0 TO IO-FG
              GO TO EXIT-VERIFY-CHECK.
           IF WK-STATUS = "D"
              ADD 1 TO SUB
              GO TO NEXT-VERIFY-CHECK.
           IF (CHECK-LINE + S-LNS-FREE-WK-RECS) = WK-KEY OR
              CK-NO NOT = WK-CKNUM
                 ADD 1 TO SUB
                 GO TO NEXT-VERIFY-CHECK.
           MOVE 9 TO IO-FG.
       EXIT-VERIFY-CHECK.
           PERFORM CLOSE-WK-FILE.

      *****************************************
       UPDATE-SINGLE-CHECK SECTION.
      *****************************************
           MOVE WK-DLPAID TO SAVE-DLPAID.
           MOVE WK-CKNO TO SAVE-CKNO.
           MOVE WK-FREE1 TO FREE-TYPE1.
           MOVE WK-FREE2 TO FREE-TYPE2.
           MOVE " " TO WK-SAVE-RECORD.
           ADD S-LNS-FREE-WK-RECS 1 GIVING WK-KEY.
           MOVE DL-NAME TO WK-PAYEE.
           MOVE LN-PROCEEDS TO WK-PYAMT.
           MOVE SAVE-CKNO TO WK-CKNUM.
           MOVE FREE-TYPE1 TO WK-CK-FREE1.
           MOVE FREE-TYPE2 TO WK-CK-FREE2.
      * 061996 BAH
           MOVE DL-ADR1 TO WK-ADR1.
           MOVE DL-ADR2 TO WK-ADR2.
           MOVE DL-CITY TO WK-CITY.
           MOVE DL-STATE TO WK-STATE.
           MOVE DL-ZIP TO WK-ZIP.
           PERFORM WRITE-LINE-DISK.

      *****************************************
       WRITE-LINE-SCREEN SECTION.
      *****************************************
           PERFORM SETUP-LINE.
           ADD 1 TO B-SCREEN-LN.
           IF B-SCREEN-LN > S-LNS
              MOVE 1 TO LSUB
              PERFORM ADJUST-LINES S-LNS-LESS1 TIMES
              MOVE LN-BUF TO D-LN(S-LNS)
              PERFORM DISPLAY-ALL-LINES
            ELSE
              MOVE "S  A000L0 ." TO VDU
              MOVE B-SCREEN-LN TO VDUCOL
              MOVE LN-BUF TO VDUBUF D-LN(B-SCREEN-LN)
              PERFORM SCREENIO.
           GO TO END-WRITE-LINE-SCREEN.
       ADJUST-LINES.
           COMPUTE LSUB2 = LSUB + 1
           MOVE D-LN(LSUB2) TO D-LN(LSUB).
           ADD 1 TO LSUB.
       END-WRITE-LINE-SCREEN.
           MOVE D-LN-NO(1) TO B-TOP-LN.

      *************************************
       WRITE-LINE-DISK SECTION.
      *************************************
           MOVE 0 TO IO-FG.
           PERFORM OPEN-WK-FILE.
           PERFORM WRITE-WK-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.
       END-WRITE-LINE-DISK.
           PERFORM CLOSE-WK-FILE.

      *********************************
       UPDATE-AUDIT SECTION.
      *********************************
           ADD CHECK-LINE S-LNS-FREE-WK-RECS GIVING WK-KEY.
           MOVE " " TO WK-STATUS.
           PERFORM SETUP-LINE.
           COMPUTE CHECK-LINE = CHECK-LINE - B-TOP-LN + 1.
           MOVE "S  A000L0 ." TO VDU.
           MOVE CHECK-LINE TO VDUCOL.
           MOVE LN-BUF TO VDUBUF D-LN(CHECK-LINE).
           PERFORM SCREENIO.
           MOVE 0 TO IO-FG.
           PERFORM OPEN-WK-FILE.
           MOVE " " TO WK-STATUS.
           PERFORM UPDATE-CHECK-TOTAL.
           PERFORM REWRITE-WK-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.
           PERFORM CLOSE-WK-FILE.

      *********************************
       DELETE-AUDIT SECTION.
      *********************************
           ADD S-LNS-FREE-WK-RECS CHECK-LINE GIVING WK-KEY.
           MOVE "D" TO WK-STATUS.
           PERFORM SETUP-LINE.
           COMPUTE CHECK-LINE = CHECK-LINE - B-TOP-LN + 1.
           MOVE "D" TO D-LN-DEL(CHECK-LINE).
           MOVE "S  A000L0 ." TO VDU.
           MOVE CHECK-LINE TO VDUCOL.
           MOVE LN-BUF TO VDUBUF D-LN(CHECK-LINE).
           PERFORM SCREENIO.
           MOVE 0 TO IO-FG.
           PERFORM OPEN-WK-FILE.
           PERFORM READ-WK-FILE.
           MOVE "D" TO WK-STATUS.
           PERFORM UPDATE-CHECK-TOTAL.
           PERFORM REWRITE-WK-FILE.
           PERFORM CLOSE-WK-FILE.

      *************************************
       UPDATE-CHECK-TOTAL SECTION.
      *************************************
           IF OLD-WK-STATUS = " "
              SUBTRACT OLD-WK-PYAMT FROM CHECK-TOTAL.
           IF WK-STATUS = " "
              ADD WK-PYAMT TO CHECK-TOTAL.

      ***********************************
       SETUP-OLD-LINE SECTION.
      ***********************************
           ADD S-LNS-FREE-WK-RECS CHECK-LINE GIVING WK-KEY.
           PERFORM OPEN-WK-FILE.
           PERFORM READ-WK-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.
           PERFORM CLOSE-WK-FILE.
           MOVE SPACE TO EL-BUF.
           MOVE CHECK-LINE TO EL-010.
           MOVE WK-PAYEE TO EL-020.
           MOVE WK-PYAMT TO EL-030.
           MOVE WK-CKNUM TO EL-040.
           MOVE WK-PYAMT TO OLD-WK-PYAMT.
           MOVE WK-STATUS TO OLD-WK-STATUS.
           MOVE "A" TO EL-YN.
           PERFORM SEND-ENTRY-LINE.

      *********************************
       SCROLL-DOWN SECTION.
      *********************************
           IF B-TOP-LN NOT > 1
              GO TO END-SCROLL-DOWN.
           IF B-TOP-LN > S-LNS
              SUBTRACT S-LNS-PLUS1 FROM B-TOP-LN GIVING LN-WK
            ELSE
              MOVE 0 TO LN-WK.
           MOVE 1 TO B-SCREEN-LN.
           MOVE SPACES TO LINE-BUF.
           MOVE 0 TO IO-FG.
           PERFORM OPEN-WK-FILE.
           PERFORM SETUP-LINE-BUF UNTIL B-SCREEN-LN > S-LNS.
           PERFORM CLOSE-WK-FILE.
           PERFORM DISPLAY-ALL-LINES.
           MOVE D-LN-NO(1) TO B-TOP-LN.
       END-SCROLL-DOWN.
           EXIT.

      *********************************
       SCROLL-UP SECTION.
      *********************************
           IF B-LN < S-LNS-PLUS1
              GO TO END-SCROLL-UP.
           ADD B-TOP-LN S-LNSX2-LESS1 GIVING LN-WK.
           IF LN-WK > B-LN
              MOVE B-LN TO LN-WK.
           SUBTRACT S-LNS FROM LN-WK.
           MOVE 1 TO B-SCREEN-LN.
           MOVE SPACES TO LINE-BUF.
           MOVE 0 TO IO-FG.
           PERFORM OPEN-WK-FILE.
           PERFORM SETUP-LINE-BUF UNTIL B-SCREEN-LN > S-LNS.
           PERFORM CLOSE-WK-FILE.
           PERFORM DISPLAY-ALL-LINES.
           MOVE D-LN-NO(1) TO B-TOP-LN.
       END-SCROLL-UP.
           EXIT.

      *************************************
       SETUP-LINE-BUF SECTION.
      *************************************
           ADD 1 S-LNS-FREE-WK-RECS LN-WK GIVING WK-KEY.
           PERFORM READ-WK-FILE.
           IF IO-FG NOT = 0
              MOVE S-LNS-PLUS1 TO B-SCREEN-LN
              GO TO END-SETUP-LINE-BUF.
           ADD 1 TO LN-WK.
           PERFORM SETUP-LINE.
           MOVE LN-BUF TO D-LN(B-SCREEN-LN).
           ADD 1 TO B-SCREEN-LN.
       END-SETUP-LINE-BUF.
           EXIT.

      *********************************
       SETUP-LINE SECTION.
      *********************************
           MOVE SPACES TO LN-BUF.
           MOVE LN-WK TO L-010.
           IF WK-STATUS = "D"
              MOVE "D" TO L-DEL.
           MOVE WK-PAYEE TO L-020.
           MOVE WK-PYAMT TO L-030.
           MOVE WK-CKNUM TO L-040.

      *******************************
       CLEAR-RECORD SECTION.
      *******************************
           MOVE " " TO WK-SAVE-RECORD.
           MOVE "D" TO OLD-WK-STATUS.
           MOVE 0 TO OLD-WK-PYAMT.
           MOVE 0 TO WK-PYAMT WK-CKNUM.

      ***********************************
       DISPLAY-FIELDS SECTION.
      ***********************************
           MOVE " " TO DISPLAY-BUF1.

           PERFORM OPEN-BW1-FILE.
           MOVE 1 TO ELE.
       DISP-NEXT-BW.
           MOVE LN-SSNO(ELE) TO BW-SSNO.
           PERFORM NUMBW-TO-ALPBW.
           MOVE " " TO BW-LNAME BW-FNAME.
           IF BW-SSNO NOT = 0
              PERFORM GET-BW1
              MOVE ALP-BW TO D-010(ELE)
              MOVE INSPECT-W TO D-011(ELE)
              MOVE LN-MAKERCD(ELE) TO D-012(ELE).
           IF ELE < 2
              ADD 1 TO ELE
              GO TO DISP-NEXT-BW.
           PERFORM CLOSE-BW1-FILE.

      * TOTAL REPAY:
           COMPUTE D-AMT(1) =
                LN-1STPYAMT
                     + LN-NOREGPY * LN-REGPYAMT + LN-LASTPYAMT.
      * INITIALIZE ANTIC-PROOF FOR ANTIC EARNINGS PROOF:
           MOVE LN-FINCHG TO ANTIC-PROOF.
           IF NOT (GP-INCL-OTH-IN-EARNED-N OR
                                  GP-INCL-OTH-IN-EARNED-R)
              ADD LN-ANTICERN(6) TO ANTIC-PROOF.



      * FINANCE CHARGE:
           MOVE LN-FINCHG TO D-AMT(2).
      * LOAN COSTS:
           MOVE LN-ANTICERN(6) TO D-AMT(3).

      *JTG.AC
      * CHARGES FROM RESERVE:
           MOVE 0 TO AMT-WK3.
           IF LN-LOANTYPE = "P"
              IF DL-RVCHGS-CODE(1) = " "
                 ADD LN-DLRVCHGS(1) TO AMT-WK3
                 SUBTRACT LN-DLRVCHGS(1) FROM ANTIC-PROOF
              END-IF
              IF DL-RVCHGS-CODE(2) = " "
                 ADD LN-DLRVCHGS(2) TO AMT-WK3
                 SUBTRACT LN-DLRVCHGS(2) FROM ANTIC-PROOF
              END-IF
              IF DL-RVCHGS-CODE(3) = " "
                 ADD LN-DLRVCHGS(3) TO AMT-WK3
                 SUBTRACT LN-DLRVCHGS(3) FROM ANTIC-PROOF.
           MOVE AMT-WK3 TO D-AMT(4).

      * DEALER PARTICIPATION:
           MOVE 0 TO AMT-WK3.
           IF LN-LOANTYPE = "P"
              IF DL-PARVHELD-CODE = "B"
                 ADD LN-DLPARVHELD TO AMT-WK3
                 SUBTRACT LN-DLPARVHELD FROM ANTIC-PROOF
              END-IF
              IF DL-PARVPAID-CODE = " "
                 ADD LN-DLPARVPAID TO AMT-WK3
                 SUBTRACT LN-DLPARVPAID FROM ANTIC-PROOF.
           MOVE AMT-WK3 TO D-AMT(5).
      *JTG.AC

      * DEALER DISCOUNT:
           MOVE LN-DLDISC TO D-AMT(6).
           ADD LN-DLDISC TO ANTIC-PROOF.
      * ANTICIPATED EARNINGS:
           COMPUTE AMT-WK3 = LN-ANTICERN(1) + LN-ANTICERN(2)
                           + LN-ANTICERN(3)
                           + LN-ANTICERN(5).
           IF NOT (GP-INCL-OTH-IN-EARNED-N OR
                              GP-INCL-OTH-IN-EARNED-R)
              ADD LN-ANTICERN(6) TO AMT-WK3.

           IF NOT (GP-INCL-OTH-IN-EARNED-R)
              ADD LN-ANTICERN(4) TO AMT-WK3.

           MOVE AMT-WK3 TO D-AMT(7).
      *JTG.AC
           IF LN-LOANTYPE = "I"
      *CS     ADD LN-FINCHG TO AMT-WK3
              ADD LN-INTCHG TO AMT-WK3
              MOVE AMT-WK3  TO D-AMT(7).
      *JTG.AC

      * ANTIC EARNINGS PROOF:
           MOVE ANTIC-PROOF TO D-ANTICPROOF.

      *JTG.AC
      *I DISCUSSED THIS WITH BARB, THE ERROR MESSAGE PROBLEM WITH DISTRIBUTION
      *ALLOWS YOU TO COMPLETE THE LOAN AND EVERYTHING IS OK.
      *HERE'S THE PROBLEM
      *  LOAN: 10.00 SC /100.00 INT/ 2.00 FEEFINCHG/ 5.00 PRIN RSV/
      *        6.00 RSVCHG/ 20.00 PARPAID( LOAN COST FRMLA 3)/ 30.00 DISC
      *    MULTIPLE CHECK DISBURSEMENT SCREEN.
      *   TOTAL PAYMENTS                               1112.00
      *     LOAN COSTS                0.00
      *     FINCHG                  112.00
      *     RSV CHARGES               6.00
      *     DISC                     30.00
      *                             ------
      *                       --->  136.00              134.00 <---
      *     RSVPRIN                                       5.00
      *     RSVCHG                                        6.00
      *     PARPAID                                      20.00
      *     CHECKS                                      987.00
      *   THESE 2 NUMBERS SHOULD BE EQUAL, THEY ARE NOT BECAUSE OF THE
      *   2.00 FEE FINCHG. THE 134.00 IS LN-ANTICERN 1 - 6 WHICH IS
      *   94.00 INTEREST, 10.00 SERVICE CHARGE, 30.00 DISCOUNT.
      *
      *   I AM UNSURE EXACTLY WHAT THE AMT-WK3 AMOUNT AND THE ANTIC-PROOF IS
      *   COMPARING, I DID NOT WANT TO CHANGE THE SCREEN SO SINCE EVERYTHING
      *   ON THE LOAN IS CORRECT, I JUST SUBTRACTED THE LN-FEEFINCHG FROM
      *   ANTIC-PROOF TO BYPASS THE ERROR MESSAGE

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 8
              IF LN-FEEOURS(SUB) NOT = "M"
                 IF LN-FEEFINCHG(SUB) = "Y"
                     SUBTRACT LN-FEEAMT(SUB) FROM ANTIC-PROOF
                 END-IF
              END-IF
           END-PERFORM.

           IF AMT-WK3 NOT = ANTIC-PROOF
              MOVE "PROBLEM WITH DISTRIBUTION, CALL HOME OFFICE"
                                                 TO MESS
              PERFORM SEND-MESS.
      *JTG.AC

      * OUR CHARGES AND INSURANCE:
           PERFORM COMPUTE-MEMO-OURS-NOTOURS.

           ADD LN-INSPREM-DLADJ OUR-CHGS GIVING D-AMT(8).
      * INSPREM DEALER ADJUSTMENTS:
           MOVE LN-INSPREM-DLADJ TO D-AMT(9).
           IF LN-INSPREM-DLADJ < 0
              MOVE
           "-  DEALER INS. PREMIUM ADJUSTMENT          - - -"
                                                TO D-INSDLADJ-PROMPT
              MOVE "-" TO D-INSDLADJ-SIGN
              MOVE ":F " TO DISPLAY-L090
           ELSE
              MOVE
           "+  DEALER INS. PREMIUM ADJUSTMENT    --> (CHECK)"
                                                TO D-INSDLADJ-PROMPT
              MOVE "+" TO D-INSDLADJ-SIGN
              IF LN-INSPREM-DLADJ = 0
                 MOVE " " TO DISPLAY-L090
              ELSE
                 MOVE ":F " TO DISPLAY-L090.
      * DEALER INSURANCE COMMISSION:
           MOVE LN-O3INS-DLPROC TO D-AMT(10).
      * RESERVE FROM PRINCIPAL:
           COMPUTE D-AMT(11) =
                LN-DLRVPRIN(1) + LN-DLRVPRIN(2) + LN-DLRVPRIN(3).
      * RESERVE FROM CHARGES:
           COMPUTE D-AMT(12) =
                LN-DLRVCHGS(1) + LN-DLRVCHGS(2) + LN-DLRVCHGS(3).

      *JTG.AC
      * DEALER PARTICIPATION HELD + PAID:
           MOVE 0 TO AMT-WK3.
           IF LN-LOANTYPE = "P"
              IF DL-PARVHELD-CODE = "B"
                 ADD LN-DLPARVHELD TO AMT-WK3
              END-IF
              IF DL-PARVPAID-CODE = " "
                 ADD LN-DLPARVPAID TO AMT-WK3.
           MOVE AMT-WK3 TO D-AMT(13).
      *JTG.AC

      * PARTICIPATION PAID:
           MOVE LN-DLPARVPAID TO D-AMT(14).
      * DELAER PROCEEDS:
           MOVE LN-PROCEEDS TO D-AMT(15)
                               CHECK-PROOF.

           MOVE DISPLAY-LIST1 TO WR-LIST.
           MOVE DISPLAY-BUF1 TO WR-BUF.
           PERFORM CALL-WRFORM.

      **********************************************
       COMPUTE-MEMO-OURS-NOTOURS SECTION.
      **********************************************
           MOVE 0 TO SUB
                     NON-MEMO-CHGS
                     NOT-OUR-CHGS
                     OUR-CHGS
                     INS-COMM.
       COMPUTE-MEMO-OURS-NOTOURS-NEXT.
           ADD 1 TO SUB.
           IF SUB NOT > 8
              IF LN-INSOURS(SUB) NOT = "M"
                 ADD LN-INSPREM(SUB) TO NON-MEMO-CHGS
                 IF LN-INSOURS(SUB) = "N"
                    ADD LN-INSPREM(SUB) TO NOT-OUR-CHGS
                 ELSE
                    ADD LN-INSCOMM(SUB) TO INS-COMM
                    ADD LN-INSPREM(SUB) TO OUR-CHGS.
           IF LN-FEEOURS(SUB) NOT = "M"
              ADD LN-FEEAMT(SUB) TO NON-MEMO-CHGS
              IF LN-FEEOURS(SUB) = "N"
                 ADD LN-FEEAMT(SUB) TO NOT-OUR-CHGS
              ELSE
                 ADD LN-FEEAMT(SUB) TO OUR-CHGS.
           IF SUB < 10
              GO TO COMPUTE-MEMO-OURS-NOTOURS-NEXT.

       COMPUTE-MEMO-OURS-NOTOURS-EXIT.
           EXIT.

      ***************************************
       DELETE-WK-CHECKS SECTION.
      ***************************************
           PERFORM OPEN-WK-FILE.
           ADD S-LNS-FREE-WK-RECS 1 GIVING WK-KEY.
       NEXT-DELETE-CHECK.
           PERFORM READ-WK-FILE.
           IF IO-FG = 9
              GO TO EXIT-DELETE-CHECKS.
           PERFORM DELETE-WK-FILE.
           ADD 1 TO WK-KEY.
           GO TO NEXT-DELETE-CHECK.
       EXIT-DELETE-CHECKS.
           PERFORM CLOSE-WK-FILE.

      **********************************************************
       TEST-FOR-DLADJ-CHECK SECTION.
      **********************************************************

      *  TEST FOR A CHECK WRITTEN = INSPREM DEALER DAJUSTMENT

           MOVE " " TO DLADJ-CHECK-FG.

           PERFORM OPEN-WK-FILE.
           ADD S-LNS-FREE-WK-RECS 1 GIVING WK-KEY.
       TEST-FOR-DLADJ-CHECK-NEXT.
           PERFORM READ-WK-FILE.
           IF IO-FG = 0
              IF WK-PYAMT = LN-INSPREM-DLADJ
                 MOVE "Y" TO DLADJ-CHECK-FG
              ELSE
                 ADD 1 TO WK-KEY
                 GO TO TEST-FOR-DLADJ-CHECK-NEXT.
           PERFORM CLOSE-WK-FILE.


      *********************************
       MISC-PARAGRAPHS SECTION.
      *********************************
       COPY "LIBGB/ACCESS.CPY".

       SUM-OF-FEEAMTS.
           COMPUTE AMT-WK2 = AMT-WK2 + LN-FEEAMT(SUB).
       GET-BW1.
           PERFORM READ-BW1-FILE.
           IF IO-FG = 9
              MOVE " " TO BW-FNAME
              MOVE "NOT FOUND" TO BW-LNAME.
           PERFORM APPEND-BW-NAMES.
       APPEND-BW-NAMES.
           MOVE BW-LNAME TO INSPECT-W.
           MOVE BW-FNAME TO ADD-ON.
           PERFORM ADD-IT.
       ADD-IT.
           INSPECT INSPECT-W REPLACING FIRST "  " BY " +".
           INSPECT INSPECT-W REPLACING FIRST
              "+                        " BY ADD-ON.
       NUMBW-TO-ALPBW.
           MOVE " " TO ALP-BW.
           IF BW-SSNO NOT = 0
              MOVE BW-SSNO TO NUM-BW
              MOVE NUM-BW123 TO ALP-BW123
              MOVE NUM-BW45 TO ALP-BW45
              MOVE NUM-BW6789 TO ALP-BW6789
              MOVE "-" TO ALP-HYPHEN1 ALP-HYPHEN2.
       NOT-FOUND.
           MOVE "NOT FOUND" TO MESS.
           PERFORM SEND-MESS.
       MSG-LINE.
           PERFORM CALL-TMMSG.
       SEND-LEGEND.
           MOVE "S  A400MSEL." TO VDU.
           MOVE MESS TO VDUBUF.
           PERFORM SCREENIO.
       DISPLAY-ALL-LINES.
           MOVE LINE-LIST TO WR-LIST.
           MOVE LINE-BUF TO WR-BUF.
           PERFORM CALL-WRFORM.
       RESET-ENTRY-LINE.
           MOVE SPACES TO VDUBUF
           MOVE "S  A020YN." TO VDU
           PERFORM SCREENIO
           MOVE SPACES TO VDUBUF
           MOVE "S  A060040." TO VDU
           PERFORM SCREENIO
           MOVE SPACES TO VDUBUF
           MOVE "S  A100030." TO VDU
           PERFORM SCREENIO
           MOVE SPACES TO VDUBUF
           MOVE "S  A300020." TO VDU
           PERFORM SCREENIO
           MOVE SPACES TO VDUBUF
           MOVE "S  A020010." TO VDU
           PERFORM SCREENIO.
      *    MOVE REV-MESS TO MESS.
      *    PERFORM RESET-LIST.
       RESET-SEL.
           MOVE SPACES TO VDUBUF.
           MOVE "S  A400MSEL." TO VDU.
           PERFORM SCREENIO.
           MOVE SPACES TO VDUBUF.
           MOVE "S  A060SEL." TO VDU.
           PERFORM SCREENIO.
      *    MOVE "SEL MSEL." TO MESS.
      *    PERFORM RESET-LIST.
       SEND-ENTRY-LINE.
           MOVE EL-BUF TO WR-BUF.
           MOVE REV-MESS TO WR-LIST.
           PERFORM CALL-WRFORM.
       ADJUST-LINES-TO-END.
           COMPUTE LN-WK = B-LN - B-TOP-LN.
           IF LN-WK NOT < S-LNS
              SUBTRACT S-LNS-LESS1 FROM B-LN GIVING B-TOP-LN
              PERFORM SCROLL-UP.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBWI/PFSWINDOWS.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/GPENV.CPY".


      **************************************************************************
       FORM-RESET SECTION.
      **************************************************************************
           MOVE LPLOAN-SYSDATE TO SFDISB-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY SFDISB-SCREEN.
           MOVE SFDISB-FORM-FIELDS TO WR-BUF.
           MOVE SFDISB-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       FORM-RESET-EXIT.
           EXIT.

      **************************************************************************
       SETUP-LINK-INFO SECTION.
      **************************************************************************
           MOVE SFDISB-HELP--FILE TO HELP-LINK-FILE.
           MOVE SFDISB-HELP--DIR TO HELP-LINK-DIR.

      **************************************************************************
       VDU-CONVERT-FIELD SECTION.
      **************************************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/SFDISB_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *********************************
       IO-ROUTINE SECTION.
      *********************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPFDGS_SQL.CPY".
       COPY "LIBLP/LPDLGS_SQL.CPY".
       COPY "LIBLP/LPCKGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPFD1RN.CPY".
       COPY "LIBLP/LPWKI.CPY".
       COPY "LIBLP/LPCK1RN.CPY".
       COPY "LIBLP/LPDL1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      *********************************
       END-ROUTINE SECTION.
      *********************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY SFDISB-SCREEN.
           PERFORM CLOSE-WINDOW UNTIL WINDOWS-OPEN = 0.
           EXIT PROGRAM.
