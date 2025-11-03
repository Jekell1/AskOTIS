       IDENTIFICATION DIVISION.
       PROGRAM-ID.    LPINF2.
       AUTHOR.        MG.
       DATE-WRITTEN.  03-11-2001.
      **************************************************************************
      *
      * IDENTIFICATION:  WI/LPINF2.C
      *
      * DESCRIPTION   :  TRANSACTION RECORD INQUIRY (PAGE 2)
      *                   DISPLAYS LOAN PAYMENT RECORD
      *
      * CALLED FROM   :  WI/LPINFO.C
      *
      * REV:
      * MG  010311 ***NEW***
      * MG  010720 ADDED F1-KEY AND F7-KEY OPTIONS
      * MG  020121 ADDED LXZ FIELDS FROM MN/LPMAN2
      * JTG 020322 ADDED "CT" COUNTY TAX CODE KENTUCKY AND PERCENTS
      *            ADDED "CY" CITY   TAX CODE KENTUCKY AND PERCENTS
      *            ADDED INS TAX REBATE AMOUNT WHICH HOLDS PRORATED PORTION
      *            OF INSURANCE REFUND                                WORLD #288
      *  JTG 020718 CHANGED FOR (MIP) MONTHLY INSURANCE PREMIUMS   REGENCY #1801
      *  BLF 071008 DO A FLDVAL FOR VALUE OF LPINFO-ARCHIVE-FG; IF "Y", PASSED 
      *             ACCT IS IN ARCHIVE DIRECTORY, SO SET UP ARCHIVE PATHS FOR
      *             LN, LP, LX, WORLD PR# 547
      *   CS 150318 ADDED MOVE OF FORM-PATHNAME TO FORM-PATH FOR HELP
      *  BAH 170419 ACTIVATE 8 DIGIT DATES, PD0006
      *  BAH 190116 GLOBAL LXFILE, SPLIT
      * BAH 2024.0311 CHANGE EXTRA PRIN LFP TO REPAY TRANS ID #1641
      * BAH 2025.0115 CHANGE 18 TO LXR-REB-AMT GA CLOSING REBATE #1697 S35Q-157
      **************************************************************************
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
           "@(#)WI/LPINF2.CBL S35 02/22/24-08:58:25 >".

      ******************************************************************
      *
      *    F I L E - W O R K E R S
      *
      ******************************************************************
       COPY "LIBLP/LP01LP.CPY".
       COPY "LIBLP/LP01LP_SQL.CPY".
       COPY "LIBLP/LPWSLP.CPY".
       COPY "LIBLP/LP01LX.CPY".
       COPY "LIBLP/LP01LX_SQL.CPY".
       COPY "LIBLP/LPWSLX.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".

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
       01  LPINF2-LINES          PIC 99    VALUE 22.
       01  LPINF2-COLUMNS        PIC 99    VALUE 78.
       01  LPINF2-BEGIN-Y        PIC 99    VALUE 2.
       01  LPINF2-BEGIN-X        PIC 99    VALUE 2.

       01  ERRCD                       PIC X(01)     VALUE SPACES.
       01  LEGEND                      PIC X(75)     VALUE SPACES.
       01  LEGEND-LIST                 PIC X(09)     VALUE "LEGEND:B.".

       01  LEGEND-OPTIONS.
           03  FILLER PIC X(30) VALUE " F7-EXIT                      ".
           03  FILLER PIC X(30) VALUE "                              ".
           03  FILLER PIC X(15) VALUE "               ".

      *=================================================================
      *
      *    D I S P L A Y - W O R K E R S
      *
      *=================================================================
       01  DISPLAY-BUF                VALUE SPACES.
           03  FLPNO                  PIC Z(07)9.
           03  FLPSEQ                 PIC Z(03)9.
           03  F01                    PIC ZZZZZZ9.99-  BLANK ZERO.
           03  F02                    PIC 9(10).
           03  F03                    PIC ZZZZZZ9.99-  BLANK ZERO.
           03  F04                    PIC ZZZZZZ9.99-  BLANK ZERO.
           03  F05                    PIC ZZZZZZ9.99-  BLANK ZERO.
           03  F06                    PIC ZZZZZZ9.99-  BLANK ZERO.
           03  F07                    PIC ZZZZZZ9.99-  BLANK ZERO.
           03  F08                    PIC ZZZZZZ9.99-  BLANK ZERO.
           03  F09                    PIC ZZZZZZ9.99-  BLANK ZERO.
           03  F10                    PIC ZZZZZZ9.99-  BLANK ZERO.
           03  F11                    PIC ZZZZZZ9.99-  BLANK ZERO.
           03  F12                    PIC ZZZZZZ9.99-  BLANK ZERO.
           03  F13                    PIC ZZZZZZ9.99-  BLANK ZERO.
           03  F14                    PIC ZZZZZZ9.99-  BLANK ZERO.
           03  F15                    PIC 99/99/99     BLANK ZERO.
           03  F16                    PIC ZZZZZZ9.99-  BLANK ZERO.
           03  F17                    PIC 99/99/99     BLANK ZERO.
           03  F18                    PIC ZZZZZZ9.99-  BLANK ZERO.
           03  F20                    PIC X(02).
           03  F21                    PIC 9(03) BLANK WHEN ZEROES.
           03  F211                   PIC 9(03) BLANK WHEN ZEROES.
           03  F212                   PIC 9(03) BLANK WHEN ZEROES.
           03  F22                    PIC 9(03) BLANK WHEN ZEROES.
           03  F221                   PIC 9(03) BLANK WHEN ZEROES.
           03  F222                   PIC 9(03) BLANK WHEN ZEROES.
           03  F23                    PIC ZZZ,ZZ9.99- BLANK WHEN ZEROES.
           03  F231                   PIC ZZZ,ZZ9.99- BLANK WHEN ZEROES.
           03  F232                   PIC ZZZ,ZZ9.99- BLANK WHEN ZEROES.
           03  F24                    PIC 9(02) BLANK WHEN ZEROES.
           03  F25                    PIC X(01).
           03  F26                    PIC 99/99/99 BLANK WHEN ZEROES.
           03  F27                    PIC 99/99/99 BLANK WHEN ZEROES.
           03  F28                    PIC Z,ZZ9.99- BLANK WHEN ZEROES.
           03  F29                    PIC ZZZ,ZZ9.99- BLANK WHEN ZEROES.
           03  F30                    PIC Z,ZZ9.99- BLANK WHEN ZEROES.
           03  F31                    PIC X(01).
           03  F32                    PIC X(03).

      *-----------------------------------------------------------------
       01  DISPLAY-LIST.
           03  FILLER      PIC X(14) VALUE
           "LPNO LPSEQ ".
           03  FILLER PIC X(20) VALUE "010 020 030 040 050 ".
           03  FILLER PIC X(20) VALUE "060 070 080 090 100 ".
           03  FILLER PIC X(20) VALUE "110 120 130 140 150 ".
           03  FILLER PIC X(16) VALUE "160 170 180 200 ".
           03  FILLER PIC X(12) VALUE "210 211 212 ".
           03  FILLER PIC X(12) VALUE "220 221 222 ".
           03  FILLER PIC X(12) VALUE "230 231 232 ".
           03  FILLER PIC X(08) VALUE "240 250 ".
           03  FILLER PIC X(20) VALUE "260 270 280 290 300 ".
           03  FILLER PIC X(20) VALUE "310 320.".
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

       01  LPINF2-WINDOW-HANDLE    PIC X(10).
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/LPINF2_DEF.CPY".
       COPY "LIBWI/LPINF2_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBWI/LPINF2W.CPY".

       SCREEN SECTION.
       COPY "LIBWI/LPINF2_SCN.CPY".

      ******************************************************************
      *
      *     P R O C E D U R E - D I V I S I O N
      *
      ******************************************************************
       PROCEDURE DIVISION
           USING FORM-PATHNAME LPINF2-BUF EXIT-PATHNAME.

      ******************************************************************
      *
      *    M A I N - P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.

      D    STOP MESS.
           MOVE SPACES TO ERRCD.
           MOVE SPACES TO VDU-WIMENU-SELECTION.
           MOVE "LPINF2" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   AT  SCREEN LINE LPINF2-BEGIN-Y * VDU-WIN-L-OFFSET
                   AT  SCREEN COL  LPINF2-BEGIN-X * VDU-WIN-C-OFFSET
                   SIZE    LPINF2-COLUMNS + VDU-WIN-W-MOD
                   LINES   LPINF2-LINES + VDU-WIN-H-MOD
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   BLANK SCREEN
                   HANDLE  IS LPINF2-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           PERFORM PRIMARY-DRIVER.

       MAIN-PROGRAM-EXIT.

           GO TO END-PROGRAM.


      ******************************************************************
       PRIMARY-DRIVER SECTION.
      ******************************************************************

           MOVE LPINF2-LP-BRNO   TO LP-BRNO.
           MOVE LPINF2-LP-ACCTNO TO LP-ACCTNO.
           MOVE LPINF2-LP-SEQNO  TO LP-SEQNO.
           PERFORM GET-LP1-RECORD.
           IF ( IO-BAD )
              MOVE " LOAN PAYMENT RECORD NOT FOUND!" TO MESS
              PERFORM SEND-MESS
              GO TO PRIMARY-DRIVER-END.

           MOVE LPINF2-LP-BRNO   TO LN-OWNBR.
           MOVE LPINF2-LP-ACCTNO TO LN-ACCTNO.
           PERFORM GET-LN1-RECORD.
           IF ( IO-BAD )
              MOVE " LOAN MASTER RECORD NOT FOUND!" TO MESS
              PERFORM SEND-MESS
              GO TO PRIMARY-DRIVER-END.

           PERFORM DISPLAY-RECORD.

           MOVE LEGEND-OPTIONS TO LEGEND.
           PERFORM SEND-LEGEND.

       PRIMARY-DRIVER-OPTIONS.

           MOVE "E  A010ACCEPT." TO VDU.
           PERFORM SCREENIO.

           IF NOT ( F1-KEY OR F7-KEY )
              GO PRIMARY-DRIVER-OPTIONS.

       PRIMARY-DRIVER-END.

           MOVE VDUSTATUS TO LPINF2-VDUSTATUS.

       PRIMARY-DRIVER-EXIT.
           EXIT.

      ******************************************************************
       DISPLAY-RECORD SECTION.
      ******************************************************************

           MOVE SPACES                     TO DISPLAY-BUF.
           MOVE LP-ACCTNO                  TO FLPNO.
           MOVE LP-SEQNO                   TO FLPSEQ.
           MOVE LP-PREPAY-PENALTY                TO F01.

           MOVE LP-REPAY-TRANS-ID          TO F02.

           IF LP-REMAIN-INT-SINCE-LFP = 2020202.02 OR 0202020.20
              MOVE ZEROES                  TO F03
           ELSE
              MOVE LP-REMAIN-INT-SINCE-LFP TO F03.

           IF LP-REMAIN-PRIN-SINCE-LFP = 2020202.02 OR 0202020.20
              MOVE ZEROES                  TO F04
           ELSE
              MOVE LP-REMAIN-PRIN-SINCE-LFP TO F04.

       DISPLAY-RECORD-LXZ.

           PERFORM OPEN-LX1-FILE.

           MOVE LPINF2-LP-BRNO   TO LX-BRNO.
           MOVE LPINF2-LP-ACCTNO TO LX-ACCTNO.
           MOVE LPINF2-LP-SEQNO  TO LX-SEQNO.
           PERFORM READ-LX1-FILE.
           IF ( IO-BAD )
              PERFORM CLEAR-LX-FILE.

           PERFORM CLOSE-LX1-FILE.

           MOVE LXZ-POST-REGPY             TO F05.
           MOVE LXZ-POST-EXTRA-PRIN        TO F06.
           MOVE LXZ-POST-OTH               TO F07.
           MOVE LXZ-POST-LCHG              TO F08.
           MOVE LXZ-PDUE-APINT             TO F09.
           MOVE LXZ-PDUE-APPRIN            TO F10.
           MOVE LXZ-CUR-APINT              TO F11.
           MOVE LXZ-CUR-APPRIN             TO F12.

           MOVE LP-PRORATED-INS-REB        TO F13.

           MOVE LXM-MIP-CLPAID             TO F14.
           MOVE LXM-MIP-CLPDTH-DATE        TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY                TO F15.
           MOVE LXM-MIP-AHPAID             TO F16.
           MOVE LXM-MIP-AHPDTH-DATE        TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY                TO F17.
           MOVE LXR-REB-AMT                TO F18.

           MOVE LXA-SRCD                   TO F20.
           MOVE LXA-START-PMT (1)          TO F21.
           MOVE LXA-START-PMT (2)          TO F211.
           MOVE LXA-START-PMT (3)          TO F212.
           MOVE LXA-END-PMT   (1)          TO F22.
           MOVE LXA-END-PMT   (2)          TO F221.
           MOVE LXA-END-PMT   (3)          TO F222.
           MOVE LXA-NET-CHANGE(1)          TO F23.
           MOVE LXA-NET-CHANGE(2)          TO F231.
           MOVE LXA-NET-CHANGE(3)          TO F232.
           MOVE LXA-LN-INSCOMP             TO F24.
           MOVE LXA-LN-INSOURS             TO F25.
           MOVE LXA-LN-INSEFF-DATE         TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY                TO F26.
           MOVE LXA-LN-INSEXP-DATE         TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY                TO F27.
           MOVE LXA-LN-INSPREM             TO F28.
           MOVE LXA-LN-INSCOVR             TO F29.
           MOVE LXA-LN-INSCOMM             TO F30.
           MOVE LXA-LN-INS-REBATE          TO F31.
           MOVE LXA-LN-INSCODES            TO F32.

           PERFORM SEND-DISPLAY.

       DISPLAY-RECORD-EXIT.
           EXIT.


      ******************************************************************
      *
      *     C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBLP/LPLXCL.CPY".

      *************************************************
       FORM-RESET SECTION.
      *************************************************
           DISPLAY LPINF2-SCREEN UPON LPINF2-WINDOW-HANDLE.
           MOVE LPINF2-FORM-FIELDS TO WR-BUF.
           MOVE LPINF2-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *************************************************
       SETUP-LINK-INFO SECTION.
      *************************************************
           MOVE LPINF2-HELP--FILE TO HELP-LINK-FILE.
           MOVE LPINF2-HELP--DIR  TO HELP-LINK-DIR.

      *************************************************
       VDU-CONVERT-FIELD SECTION.
      *************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/LPINF2_EVA.CPY".
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
           PERFORM CLOSE-LX1-FILE.
           PERFORM CLOSE-LP1-FILE.
           PERFORM CLOSE-LN1-FILE.
       COPY "LIBLP/LPLXGS_SQL.CPY".
       COPY "LIBLP/LPLPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLP1RN.CPY".
       COPY "LIBLP/LPLX1RN.CPY".
       COPY "LIBLP/LPLN1RN.CPY".

       COPY "LIBGB/FERRORS.CPY".

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************

           PERFORM CLOSE-FILES.

       EXIT-PROG.

       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY LPINF2-SCREEN.
           DESTROY LPINF2-WINDOW-HANDLE.
           EXIT PROGRAM.
