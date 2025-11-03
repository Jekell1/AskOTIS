       IDENTIFICATION DIVISION.
       PROGRAM-ID.    LPINFO.
       AUTHOR.        KEC.
       DATE-WRITTEN.  07-20-2000.
      ******************************************************************
      *
      * IDENTIFICATION:  WI/LPINFO.C
      *
      * DESCRIPTION   :  TRANSACTION RECORD INQUIRY
      *                   DISPLAYS LOAN PAYMENT RECORD
      *
      * CALLED FROM   :  WI/RVLINE.C
      *
      *-----------------------------------------------------------------
      * REV:
      * KEC 010720 NEW
      * MG  010314 ADDED OPTION TO DISPLAY SECOND PAGE
      * MG  010720 ADDED F1-KEY AND F7-KEY OPTIONS TO WI/LPINF2
      * JTG 020117 CHANGED FIELDS FROM S9(6)V99 TO S9(7)V99     LENDMARK 
      * BLF 071008 FLDVAL FOR VALUE OF LPINFO-ARCHIVE-FG; IF "Y", PASSED ACCT.
      *            IS IN ARCHIVE DIRECTORY, SO SET UP ARCHIVE PATHS FOR LN,
      *            LP, LX, WORLD PR# 547
      *  CS 100305 ADDED APPLIED TO ESCROW, REGWAN PR# 3506
      *  CS 150113 A30, ERROR ON HELP, MOVED FORM-PATHNAME TO FORM-PATH
      * BAH 161104 CHANGE TILLNO TO Z9
      * BAH 170419 ACTIVATE 8 DIGIT DATES, PD0006
      * KEC 2017.0505 ADDED 'LX' (E) FILE PREFIX FOR WORKING STORAGE FIELDS.
      *               ADDED 'LX' (G) FILE PREFIX FOR WORKING STORAGE FIELDS.
      * BAH 190116 GLOBAL LXFILE, SPLIT
      * BAH 20190308 CHANGED LP-ADDON TO USE LPWSLP-LP-ADDON, MOVED LP-REFNO
      *              REDEFINITIONS TO LPWSLP COPY MEMBER
      * BAH 2019.0611 LP-PDTH-DATE WAS DISPLAYING IN F121 INSTEAD OF F12 AND
      *               LP-LCDPTH-DATE WAS MISSING! QA0034
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.

      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E - S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)WI/LPINFO.CBL S35 02/22/24-12:48:41 >".

      ******************************************************************
      *
      *    F I L E - W O R K E R S
      *
      ******************************************************************
       COPY "LIBLP/LP01LP.CPY".
       COPY "LIBLP/LP01LP_SQL.CPY".
       COPY "LIBLP/LPWSLP.CPY".
       COPY "LIBLP/LP01LXE.CPY".
       COPY "LIBLP/LP01LXE_SQL.CPY".
       COPY "LIBLP/LPWSLXE.CPY".
       COPY "LIBLP/LP01LXG.CPY".
       COPY "LIBLP/LP01LXG_SQL.CPY".
       COPY "LIBLP/LPWSLXG.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".

      *********************************************************
      *   LNFILE ARCHIVE PATH WORKERS
      *********************************************************
      *01  LN-ARCHIVE-PATH.
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  LN-ARCHIVE-DIR1        PIC X(3).
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  LN-ARCHIVE-DIRECTORY   PIC X(4).
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  LN-ARCHIVE-MACHINE     PIC XX.
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  LN-ARCHIVE-DATA        PIC X(6).
      *    03  FILLER                 PIC X(10) VALUE "/LP/LNFILE".

      *********************************************************
      *   LPFILE ARCHIVE PATH WORKERS
      *********************************************************
      *01  LP-ARCHIVE-PATH.
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  LP-ARCHIVE-DIR1        PIC X(3).
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  LP-ARCHIVE-DIRECTORY   PIC X(4).
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  LP-ARCHIVE-MACHINE     PIC XX.
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  LP-ARCHIVE-DATA        PIC X(6).
      *    03  FILLER                 PIC X(10) VALUE "/GB/LPFILE".

      *********************************************************
      *   LXFILE ARCHIVE PATH WORKERS
      *********************************************************
      *01  LX-ARCHIVE-PATH.
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  LX-ARCHIVE-DIR1        PIC X(3).
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  LX-ARCHIVE-DIRECTORY   PIC X(4).
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  LX-ARCHIVE-MACHINE     PIC XX.
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  LX-ARCHIVE-DATA        PIC X(6).
      *    03  FILLER                 PIC X(10) VALUE "/GB/LXFILE".

      ******************************************************************
      *
      *    M I S C E L L A N E O U S - W O R K E R S
      *
      ******************************************************************
       01  LPINFO-LINES          PIC 99    VALUE 22.
       01  LPINFO-COLUMNS        PIC 99    VALUE 78.
       01  LPINFO-BEGIN-Y        PIC 99    VALUE 2.
       01  LPINFO-BEGIN-X        PIC 99    VALUE 2.

       01  ERRCD                       PIC X(01)     VALUE SPACES.
       01  LEGEND                      PIC X(75)     VALUE SPACES.
       01  LEGEND-LIST                 PIC X(09)     VALUE "LEGEND:B.".

       01  LEGEND-OPTIONS.
           03  FILLER PIC X(30) VALUE " F2-PAGE 2, F7-EXIT           ".
           03  FILLER PIC X(30) VALUE "                              ".
           03  FILLER PIC X(15) VALUE "               ".

       01  WK-EARN                     PIC S9(5)V99.

       01  SUB                         PIC 9(6)  COMP-3.

       01  GL-SCREEN-EDIT  PIC 9999/9999999.
       01  GL-EDIT REDEFINES GL-SCREEN-EDIT.
           03  FILLER                  PIC X(4).
           03  GL-DASH1                PIC X.
           03  FILLER                  PIC 9(7).

       01  SS-GL-EDIT      PIC 999/99/9999.
       01  SS-EDIT REDEFINES SS-GL-EDIT.
           03  FILLER      PIC X(3).
           03  SS-DASH1    PIC X.
           03  FILLER      PIC X(2).
           03  SS-DASH2    PIC X.
           03  FILLER      PIC X(4).

      ******************************************************************
      *
      *    D I S P L A Y - W O R K E R S
      *
      ******************************************************************
       01  DISPLAY-BUF.
           03  FLPNO       PIC Z(07)9.
           03  FLPSEQ      PIC Z(03)9.
           03  F01         PIC XX.
           03  F02         PIC 99/99/99  BLANK ZERO.
           03  F03         PIC 99/99/99  BLANK ZERO.
           03  F04         PIC 99/99/99  BLANK ZERO.
           03  F041        PIC 99/99/99.
           03  F05         PIC 9(4).
           03  F051        PIC X(10).
           03  F06         PIC X(6).
           03  F07         PIC X.
           03  F08         PIC XX.
           03  F081        PIC ZZZ9.
           03  F09         PIC X.
           03  F091        PIC Z9.
           03  F10         PIC X.
           03  F11         PIC X.
           03  F12         PIC 99/99/99  BLANK ZERO.
           03  F121        PIC 99/99/99  BLANK ZERO.
           03  F13         PIC ZZZ-.

           03  F14         PIC X(11).
           03  F141        PIC X(33).

           03  F15         PIC X(12).
           03  F151        PIC ZZZZZ.ZZ-.
           03  F16         PIC X(12).
           03  F161        PIC ZZZZZ.ZZ-.
           03  F17         PIC X(12).
           03  F171        PIC ZZZZZ.ZZ-.

           03  F18         PIC ZZZZZZZ.ZZ-.
           03  F19         PIC ZZZZZ.ZZ-.
           03  F191        PIC ZZZZZ.ZZ-.
           03  F20         PIC ZZZZZ.ZZ-.
           03  F201        PIC ZZZZZ.ZZ-.
           03  FESCROW     PIC ZZZZZ.ZZ-.
           03  F21         PIC ZZZZZZ.ZZ-.
           03  F22         PIC ZZZZZ.ZZ-.
           03  F221        PIC ZZZZZ.ZZ-.
           03  F23         PIC ZZZZZ.ZZ-.
           03  F231        PIC ZZZZZ.ZZ-.
           03  F24         PIC ZZZZZZ.ZZ-.
           03  F25         PIC ZZZZZ.ZZ-.
           03  F251        PIC ZZZZZ.ZZ-.
           03  F26         PIC ZZZZZ.ZZ-.
           03  F261        PIC ZZZZZ.ZZ-.
           03  F27         PIC ZZZZZ.ZZ-.
           03  F271        PIC ZZZZZ.ZZ-.
           03  F28         PIC ZZZZZ.ZZ-.
           03  F29         PIC ZZZZZ.ZZ-.
           03  F30         PIC ZZZZZ.ZZ-.
           03  F30X REDEFINES F30 PIC Z(8)-.
           03  F31         PIC ZZZ.ZZZ.
           03  F32         PIC ZZZZZ.ZZ-.
           03  F321        PIC ZZZZZ.ZZ-.
           03  F33         PIC ZZZZZ.ZZ-.
           03  F34         PIC ZZZZZ.ZZ-.
           03  F34X       REDEFINES F34 PIC Z(8)-.
           03  F341        PIC ZZZZZ.ZZ-.
           03  F35         PIC ZZZ.ZZ-.
           03  F351        PIC ZZZ.ZZ-.
           03  F352        PIC ZZZ.ZZ-.
           03  F36         PIC ZZZZZ.ZZ-.
           03  F361        PIC ZZZZZ.ZZ-.

      *-----------------------------------------------------------------
       01  DISPLAY-LIST.
           03  FILLER      PIC X(14) VALUE
           "LPNO LPSEQ ".
           03  FILLER      PIC X(48) VALUE
           "010 020 030 040 041 050 051 060 070 080 081 090 ".
           03  FILLER      PIC X(56) VALUE
           "091 100 110 120 121 130 140 141 150 151 160 161 170 171 ".
           03  FILLER      PIC X(55) VALUE
           "180 190 191 200 201 ESCROW 210 220 221 230 231 240 250 ".
           03  FILLER      PIC X(56) VALUE
           "251 260 261 270 271 280 290 300 310 320 321 330 340 341 ".
           03  FILLER      PIC X(20) VALUE
           "350 351 352 360 361.".
      ******************************************************************
      *
      *     C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/GETENVW.CPY".

       COPY "LIBWI/LPINF2W.CPY".
       01  LPINFO-WINDOW-HANDLE    PIC X(10).
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/LPINFO_DEF.CPY".
       COPY "LIBWI/LPINFO_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBWI/LPINFOW.CPY".

       SCREEN SECTION.
       COPY "LIBWI/LPINFO_SCN.CPY".

      ******************************************************************
      *
      *     P R O C E D U R E - D I V I S I O N
      *
      ******************************************************************
       PROCEDURE DIVISION
           USING FORM-PATHNAME LPINFO-BUF EXIT-PATHNAME.

      ******************************************************************
      *
      *    M A I N - P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.

           MOVE SPACES TO ERRCD.

           MOVE SPACES TO VDU-WIMENU-SELECTION.
           MOVE "LPINFO" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   AT  SCREEN LINE LPINFO-BEGIN-Y * VDU-WIN-L-OFFSET
                   AT  SCREEN COL  LPINFO-BEGIN-X * VDU-WIN-C-OFFSET
                   SIZE    LPINFO-COLUMNS + VDU-WIN-W-MOD
                   LINES   LPINFO-LINES + VDU-WIN-H-MOD
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   BLANK SCREEN
                   HANDLE  IS LPINFO-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           PERFORM PRIMARY-DRIVER.

       MAIN-PROGRAM-EXIT.
           GO TO END-PROGRAM.

      ******************************************************************
       PRIMARY-DRIVER SECTION.
      ******************************************************************

           MOVE LPINFO-LP-ACCTNO TO LP-ACCTNO.
           MOVE LPINFO-LP-SEQNO  TO LP-SEQNO.
           PERFORM GET-LP1-RECORD.
           IF ( IO-BAD )
              MOVE " LOAN PAYMENT RECORD NOT FOUND!" TO MESS
              PERFORM SEND-MESS
              GO TO PRIMARY-DRIVER-EXIT.

           MOVE LPINFO-LP-ACCTNO TO LN-ACCTNO.
           PERFORM GET-LN1-RECORD.

           IF ( IO-BAD )
              MOVE " LOAN MASTER RECORD NOT FOUND!" TO MESS
              PERFORM SEND-MESS
              GO TO PRIMARY-DRIVER-EXIT.

           PERFORM DISPLAY-RECORD.
           MOVE LEGEND-OPTIONS TO LEGEND.
           PERFORM SEND-LEGEND.

      *=================================================================
       PRIMARY-DRIVER-OPTIONS.

           MOVE "E  A010ACCEPT." TO VDU.
           PERFORM SCREENIO.

           IF ( F2-KEY )
              PERFORM CALL-LPINF2-WINDOW
              IF LPINF2-VDUSTATUS = "1>"
                 GO TO MAIN-PROGRAM-EXIT
              ELSE
                 GO TO PRIMARY-DRIVER-OPTIONS.

           IF NOT ( F2-KEY OR F7-KEY )
              GO TO PRIMARY-DRIVER-OPTIONS.

       PRIMARY-DRIVER-EXIT.
           EXIT.

      ******************************************************************
       CALL-LPINF2-WINDOW SECTION.
      ******************************************************************

           MOVE LP-BRNO           TO LPINF2-LP-BRNO.
           MOVE LP-ACCTNO         TO LPINF2-LP-ACCTNO.
           MOVE LP-SEQNO          TO LPINF2-LP-SEQNO.
           MOVE SPACES            TO LPINF2-VDUSTATUS.
           MOVE LPINFO-ARCHIVE-FG TO LPINF2-ARCHIVE-FG.
           MOVE "WI/LPINF2 " TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH LPINF2-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.

       CALL-LPINF2-WINDOW-EXIT.
           EXIT.

      ******************************************************************
       DISPLAY-RECORD SECTION.
      ******************************************************************
           MOVE SPACES                     TO DISPLAY-BUF.
           MOVE LP-ACCTNO                  TO FLPNO.
           MOVE LP-SEQNO                   TO FLPSEQ.
           MOVE LP-TRCD                    TO F01.
           MOVE LP-TRDATE                  TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY                TO F02.
           MOVE LP-PAYDATE                 TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY                TO F03.
           MOVE LP-POSTDATE                TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY                TO F04.
           MOVE LP-POSTTIME                TO F041.
           INSPECT F041 REPLACING ALL "/" BY ":".
           MOVE LP-POSTING-BR              TO F05.
           MOVE LP-USERID                  TO F051.
           MOVE LP-REFNO                   TO F06.
           MOVE LP-REV                     TO F07.
           MOVE LP-REVTRCD                 TO F08.
           MOVE LP-REVSEQNO                TO F081.
           MOVE LP-STATUSFG                TO F09.
           MOVE LP-TILLNO                  TO F091.
           MOVE LP-IBPC                    TO F10.
           MOVE LP-MANUAL-RB               TO F11.
           MOVE LP-PDTH-DATE               TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY                TO F12.
           MOVE LP-LCPDTH-DATE             TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY                TO F121.

           MOVE LP-NOLCHG                  TO F13.

           MOVE LP-SSNO TO SS-GL-EDIT BW-SSNO.
           MOVE "-" TO SS-DASH1 SS-DASH2.
           MOVE SS-EDIT TO F14.
           IF LP-SSNO = 0
              GO TO DISPLAY-FIELDS-CONT.
           PERFORM OPEN-BW1-FILE.
           PERFORM READ-BW1-FILE.
           PERFORM CLOSE-BW1-FILE.
           IF IO-FG NOT = 0
              MOVE "** NOT ON FILE **" TO F141
           ELSE
              MOVE SPACES TO F141
              STRING BW-LNAME,", ",BW-FNAME DELIMITED BY "   "
                   INTO F141.

       DISPLAY-FIELDS-CONT.
           MOVE LP-TRAMT                   TO F18.
           MOVE LP-APINT                   TO F19.
           MOVE LP-APLC                    TO F191.
           MOVE LP-APOTH                   TO F20.
           MOVE LP-APOT2                   TO F201.
           MOVE LP-APCUR                   TO F21.
           MOVE LP-APPLIED-TO-ESCROW       TO FESCROW.
           MOVE LP-OTHBAL                  TO F22.
           MOVE LP-OT2BAL                  TO F221.
           MOVE LP-INTBAL                  TO F23.
           MOVE LP-LCBAL                   TO F231.
           MOVE LP-CURBAL                  TO F24.

           MOVE LP-INTPAID                 TO F28.
           MOVE LP-INTDUE                  TO F29.
           IF LP-TRCD = "SP"
              MOVE LP-PRIOR-POOLID TO F30X
           ELSE
              IF LP-TRCD = "RV" AND LP-REFNO = "SP   "
                 MOVE LP-PRIOR-POOLID TO F30X
              ELSE
                 MOVE LP-APINTOWE TO F30.

           MOVE LP-NEWSTAT-RATE            TO F31.
           MOVE LP-LCPARTIALS                 TO F32.
           MOVE LP-LCPAID                 TO F321.
           MOVE LP-DLPROC                  TO F33.

           MOVE LP-REFNO TO LPWSLP-LP-REFNO.
           IF LP-TRCD = "ET" OR "RT" OR "LP" OR "NP" OR "ST"
              MOVE LP-LT-SEQNO TO F34X
           ELSE
              IF LP-TRCD = "RV" AND LPWSLP-LP-ADDON-TEXT = "ADD"
                 MOVE LP-LT-SEQNO TO F34X
              ELSE
                 MOVE LP-DLPARVHELD-ADJ TO F34.

           MOVE LP-DLPARVPAID-ADJ          TO F341.
           MOVE LP-DLRVCHGS-ADJ(1)         TO F35.
           MOVE LP-DLRVCHGS-ADJ(2)         TO F351.
           MOVE LP-DLRVCHGS-ADJ(3)         TO F352.

           MOVE LP-BRNO   TO LXG-BRNO.
           MOVE LP-ACCTNO TO LXG-ACCTNO.
           MOVE LP-SEQNO  TO LXG-SEQNO.
           PERFORM OPEN-LXG1-FILE.
           PERFORM READ-LXG1-FILE.
           PERFORM CLOSE-LXG1-FILE.
           IF IO-FG NOT = 0
              MOVE 0 TO LXG-GLNO(1)  LXG-GLNO(2)  LXG-GLNO(3)
                        LXG-GLAMT(1) LXG-GLAMT(2) LXG-GLAMT(3).

           IF LXG-GLNO(1) NOT = 0
              MOVE LXG-GLNO(1)  TO GL-SCREEN-EDIT
              MOVE "-"          TO GL-DASH1
              MOVE GL-EDIT      TO F15
              MOVE LXG-GLAMT(1) TO F151.
           IF LXG-GLNO(2) NOT = 0
              MOVE LXG-GLNO(2)  TO GL-SCREEN-EDIT
              MOVE "-"          TO GL-DASH1
              MOVE GL-EDIT      TO F16
              MOVE LXG-GLAMT(2) TO F161.
           IF LXG-GLNO(3) NOT = 0
              MOVE LXG-GLNO(3)  TO GL-SCREEN-EDIT
              MOVE "-"          TO GL-DASH1
              MOVE GL-EDIT      TO F17
              MOVE LXG-GLAMT(3) TO F171.

           MOVE LP-BRNO   TO LXE-BRNO.
           MOVE LP-ACCTNO TO LXE-ACCTNO.
           MOVE LP-SEQNO  TO LXE-SEQNO.
           PERFORM OPEN-LXE1-FILE.
           PERFORM READ-LXE1-FILE.
           PERFORM CLOSE-LXE1-FILE.
           IF IO-FG NOT = 0
              MOVE 0 TO LXE-EARN(1) LXE-EARN(2) LXE-EARN(3)
                        LXE-WORKER  LXE-WORKER2.

           MOVE 1 TO SUB.
           PERFORM DISPLAY-EARNED.

           MOVE LXE-WORKER                 TO F36.
           MOVE LXE-WORKER2                TO F361.

           PERFORM SEND-DISPLAY.

       DISPLAY-RECORD-EXIT.
           EXIT.

      ******************************************************************
       DISPLAY-EARNED SECTION.
      ******************************************************************

       DISPLAY-EARN.
           IF LN-EARN-ORDER(SUB) = 1
              MOVE LP-EARNED(1) TO WK-EARN
           ELSE
           IF LN-EARN-ORDER(SUB) = 2
              MOVE LP-EARNED(2) TO WK-EARN
           ELSE
           IF LN-EARN-ORDER(SUB) = 3
              MOVE LP-EARNED(3) TO WK-EARN
           ELSE
           IF LN-EARN-ORDER(SUB) = 4
              MOVE LXE-EARN(1) TO WK-EARN
           ELSE
           IF LN-EARN-ORDER(SUB) = 5
              MOVE LXE-EARN(2) TO WK-EARN
           ELSE
           IF LN-EARN-ORDER(SUB) = 6
              MOVE LXE-EARN(3) TO WK-EARN.

           IF SUB = 1
              MOVE WK-EARN TO F25
           ELSE
           IF SUB = 2
              MOVE WK-EARN TO F251
           ELSE
           IF SUB = 3
              MOVE WK-EARN TO F26
           ELSE
           IF SUB = 4
              MOVE WK-EARN TO F261
           ELSE
           IF SUB = 5
              MOVE WK-EARN TO F27
           ELSE
           IF SUB = 6
              MOVE WK-EARN TO F271.
           ADD 1 TO SUB.
           IF SUB < 7
              GO TO DISPLAY-EARN.

       DISPLAY-EARNED-EXIT.
           EXIT.

      ******************************************************************
      *
      *     C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".

      *************************************************
       FORM-RESET SECTION.
      *************************************************
           DISPLAY LPINFO-SCREEN UPON LPINFO-WINDOW-HANDLE.
           MOVE LPINFO-FORM-FIELDS TO WR-BUF.
           MOVE LPINFO-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *************************************************
       SETUP-LINK-INFO SECTION.
      *************************************************
           MOVE LPINFO-HELP--FILE TO HELP-LINK-FILE.
           MOVE LPINFO-HELP--DIR  TO HELP-LINK-DIR.

      *************************************************
       VDU-CONVERT-FIELD SECTION.
      *************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/LPINFO_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISCELLANEOUS-PARAGRAPHS SECTION.
      ******************************************************************
      *-----------------------------------------------------------------
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
      *-----------------------------------------------------------------
       GET-LN1-RECORD.

           PERFORM OPEN-LN1-FILE.
           PERFORM READ-LN1-FILE.
           PERFORM CLOSE-LN1-FILE.

      *-----------------------------------------------------------------
       GET-LP1-RECORD.
           PERFORM OPEN-LP1-FILE.
           PERFORM READ-LP1-FILE.
           PERFORM CLOSE-LP1-FILE.

      *-----------------------------------------------------------------
       SEND-DISPLAY.
           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
      *-----------------------------------------------------------------
       SEND-LEGEND.

           MOVE LEGEND TO WR-BUF.
           MOVE LEGEND-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.


      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-LXG1-FILE.
           PERFORM CLOSE-LXE1-FILE.
           PERFORM CLOSE-LP1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-BW1-FILE.
       COPY "LIBLP/LPLXGGS_SQL.CPY".
       COPY "LIBLP/LPLXEGS_SQL.CPY".
       COPY "LIBLP/LPLPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBLP/LPLP1RN.CPY".
       COPY "LIBLP/LPLXE1RN.CPY".
       COPY "LIBLP/LPLXG1RN.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".

       COPY "LIBGB/FERRORS.CPY".

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************

           PERFORM CLOSE-FILES.

       EXIT-PROG.

       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY LPINFO-SCREEN.
           DESTROY LPINFO-WINDOW-HANDLE.
           EXIT PROGRAM.
