       IDENTIFICATION DIVISION.
       PROGRAM-ID.      TRSCAN.
      *****************************************************************
      *
      *  SCAN TRFILE FOR "A" ALL TYPES, CAN NOT SELECT A RECORD, VIEW ONLY
      *  CALLED FROM: MNTRMU
      *
      * REV.
      * 092795 KEC ADDED TR-CODE FOR "B"; LIBLP/LPFDTRB ADDED
      * 082396 BLV ADDED SIGN TO AMTS; TOOK AWAY COMMA (NEGATIVES
      *            NOT DISPLAYING
      * MJD 110596 ADDED TR-CODE FOR "J" AND "K"
      * BAH 990618 ADDED "ST"
      * MJD 010102 ADDED TR-CODE = "G" FOR ACCRUAL EARNINGS.  (PR#1375)
      * BLV 010822 CHANGED MAX-PAGES & PAGE-KEY TABLE FROM OCCURS 50
      *            TO 250, INCREASES PROGRAM SIZE BUT THIS IS NOT A
      *            COMMONLY RUN PROGRAM & WE FREQUENTLY "OVERFLOWED"
      * BAH 040205 ADDED TR-CODE = "I" FOR INSURANCE CLAIMS, WORLD PR# 341
      * BLF 061220 CHANGE CONVERSION SCAN TO DISPLAY TK-PURBAL INSTEAD
      *            OF TK-LNAMT, WORLD PR# 510
      * MJD 131122 CHANGED BRNO TO BRANCH IN THE FORM TO AVOID CONFLICT
      *            WITH SCN WORKER. (I.E. TRSCAN-BRNO)
      *  CS 160922 REMOVED LIBLP/LPFDTRG, MIKE CREATED THIS WHEN WE WERE
      *            THINKING ABOUT ADDING EARNINGS TO TRFILE. ITS NOT UPDATED
      *            CHANGED PAGE-KEY X(14) TO X(27)IN  PAGE-KEY-TABLE.
      *                    SELECT-KEY(X14) TO X(27) IN KEY-SELECT-TABLE
      *  BAH 170424 ACTIVATE 8 DIGIT DATES, PD0006
      *  BAH 20191007 REMOVED ACCESS-CALLS
      *  SMR 20230821 CHANGED REFERENCE OF TI-DATE TO TI-KEY-DATE  
      *  BAH 20240227 IF MANUALLY ENTER 0 AT THE DAY, WAS GIVING AN SQL ERROR
      *               IF SCAN-DAY 99 OR 0, START THE DATE AT DAY 1.
      * BAH 2025.0124 INVALID CHARACTER FOR CAST IF YOU RETURN THRU THE DAY
      *               MAKE SCAN-DAY 0 ACT LIKE F3 BY MOVING 99, AND
      *               VALIDATE MONTH BETWEEN 1 AND 12 S35Q-159
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSWKI.CPY".
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.

       FD  WK-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK-REC.
           03  WK-KEY.
               05  WK-DATE  PIC 9(8).
               05  WK-CODE  PIC X.
               05  WK-SEQ   PIC 9(5).
           03  WK-DISPLAY   PIC X(80).

      *****************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)WI/TRSCAN.CBL S35 05/01/24-13:37:46 >".

      *****************************************************************
      *    W O R K E R S
      *****************************************************************
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01TRB.CPY".
       COPY "LIBLP/LP01TRB_SQL.CPY".
       COPY "LIBLP/LPWSTRB.CPY".
       COPY "LIBLP/LP01TRC.CPY".
       COPY "LIBLP/LP01TRC_SQL.CPY".
       COPY "LIBLP/LPWSTRC.CPY".
       COPY "LIBLP/LP01TRD.CPY".
       COPY "LIBLP/LP01TRD_SQL.CPY".
       COPY "LIBLP/LPWSTRD.CPY".
       COPY "LIBLP/LP01TRI.CPY".
       COPY "LIBLP/LP01TRI_SQL.CPY".
       COPY "LIBLP/LPWSTRI.CPY".
       COPY "LIBLP/LP01TRJ.CPY".
       COPY "LIBLP/LP01TRJ_SQL.CPY".
       COPY "LIBLP/LPWSTRJ.CPY".
       COPY "LIBLP/LP01TRK.CPY".
       COPY "LIBLP/LP01TRK_SQL.CPY".
       COPY "LIBLP/LPWSTRK.CPY".
       COPY "LIBLP/LP01TRL.CPY".
       COPY "LIBLP/LP01TRL_SQL.CPY".
       COPY "LIBLP/LPWSTRL.CPY".
       COPY "LIBLP/LP01TRP.CPY".
       COPY "LIBLP/LP01TRP_SQL.CPY".
       COPY "LIBLP/LPWSTRP.CPY".
       COPY "LIBLP/LP01TRS.CPY".
       COPY "LIBLP/LP01TRS_SQL.CPY".
       COPY "LIBLP/LPWSTRS.CPY".
       COPY "LIBLP/LP01TRW.CPY".
       COPY "LIBLP/LP01TRW_SQL.CPY".
       COPY "LIBLP/LPWSTRW.CPY".
       COPY "LIBLP/LP01TRX.CPY".
       COPY "LIBLP/LP01TRX_SQL.CPY".
       COPY "LIBLP/LPWSTRX.CPY".

       01  WK-IFN                   PIC X(3) VALUE "WK".
       01  NAME-WORKER              PIC X(34).
       01  FIRST-NAME               PIC X(16).

       01  TRSCAN-LINES          PIC 99  VALUE 17.
       01  TRSCAN-COLUMNS        PIC 99  VALUE 80.
       01  TRSCAN-BEGIN-Y        PIC 99  VALUE 3.
       01  TRSCAN-BEGIN-X        PIC 99  VALUE 1.

       01  ERRCD                    PIC X    VALUE " ".
       01  ONE                      PIC S9   VALUE 1.
       01  SUB                      PIC 99.
       01  SCAN-BRANCH              PIC 9(4).
       01  SCAN-MMYY                PIC 9(4).
       01  SCAN-MMYYX REDEFINES SCAN-MMYY.
           03  SCAN-MM              PIC 99.
           03  SCAN-YY              PIC 99.
       01  SCAN-CODE                PIC X.
       01  SCAN-DAY                 PIC 99.
       01  BEGINNING-DATE           PIC 9(8).


       01  FLDSEL-STAT              PIC XX.
       01  FLDSEL-FIELD             PIC X(4)   VALUE "010.".

       01  FLDSEL-LIST.
           03  FILLER               PIC X(36)  VALUE
           "010 020 030 040 050 060 070 080 090.".
       01  FLDSEL-BUF.
           03  FLDSEL-S.
               05  FLDSEL-S-SSNO      PIC X(11).
               05  FILLER             PIC X(38).
               05  FLDSEL-S-OWNBR     PIC 9(4).
               05  FILLER             PIC X(23).
           03  FLDSEL-N               REDEFINES FLDSEL-S.
               05  FLDSEL-N-NAME      PIC X(22).
               05  FILLER             PIC X.
               05  FLDSEL-N-SSNO      PIC X(11).
               05  FILLER             PIC X(15).
               05  FLDSEL-N-OWNBR     PIC 9(4).
               05  FILLER             PIC X(23).

       01  FIRST-FIELD              PIC X(4)  VALUE "010.".
       01  LAST-FIELD               PIC X(4)  VALUE "090.".

       01  DISPLAY-LIST.
           03  FILLER               PIC X(36)  VALUE
           "010 020 030 040 050 060 070 080 090 ".
           03  FILLER               PIC X(20)  VALUE
           "PGNO ENDMORE:F HEAD.".
       01  DISPLAY-BUF.
           03  D-LINE               OCCURS 9 TIMES.
               05  D-DATE           PIC 99/99/99.
               05  FILLER           PIC XX.
               05  D-CODE           PIC X.
               05  FILLER           PIC XX.
      *                                                 DC-LINE = 63 BYTES
               05  DC-LINE.
                   07  DC-PAID      PIC X.
                   07  FILLER       PIC XX.
                   07  DC-VOID      PIC X.
                   07  FILLER       PIC X.
                   07  DC-CKNO      PIC ZZZZZ9.
                   07  FILLER       PIC X(4).
                   07  DC-REFNO     PIC ZZZZZ9.
                   07  DC-PAYEE     PIC X(30).
                   07  FILLER       PIC X.
                   07  DC-CKAMT     PIC ZZZZZZ9.99-.
               05  DG-LINE REDEFINES DC-LINE.
                   07  FILLER       PIC X(17).
                   07  DG-NAME      PIC X(20).
                   07  FILLER       PIC X(26).
               05  DW-LINE REDEFINES DC-LINE.
                   07  FILLER       PIC X(01).
                   07  DW-TILLNO    PIC X(01).
                   07  FILLER       PIC X(04).
                   07  DW-TIME      PIC 99/99.
                   07  FILLER       PIC X(5).
                   07  DW-USERID    PIC X(08).
                   07  FILLER       PIC X(15).
                   07  DW-AMOUNT    PIC ZZZZZZ9.99-.
                   07  FILLER       PIC X(4).
                   07  DW-OVERSHORT PIC ZZZZ9.99-.
               05  DP-LINE REDEFINES DC-LINE.
                   07  DP-CLASS     PIC 99.
                   07  DP-DASH1     PIC X.
                   07  DP-NUMBER    PIC 9(6).
                   07  DP-DASH2     PIC X.
                   07  DP-SPCLASS   PIC 99.
                   07  DP-PLCD      PIC X.
                   07  DP-PLTYPE    PIC X.
                   07  DP-JDCD      PIC X.
      *            07  DP-SEQNO     PIC 99.
                   07  FILLER       PIC X.
                   07  DP-TRCD      PIC XX.
                   07  FILLER       PIC X.
                   07  DP-NAME      PIC X(20).
                   07  FILLER       PIC X.
                   07  DP-SSNO      PIC 999/99/9999.
                   07  DP-TRAMT     PIC ZZZZZ9.99-.
               05  DL-LINE REDEFINES DC-LINE.
                   07  DL-CLASS     PIC 99.
                   07  DL-DASH1     PIC X.
                   07  DL-NUMBER    PIC 9(6).
                   07  DL-DASH2     PIC X.
                   07  DL-SPCLASS   PIC 99.
                   07  FILLER       PIC X(5).
                   07  FILLER       PIC X.
                   07  DL-TRCD      PIC XX.
                   07  FILLER       PIC X.
                   07  DL-NAME      PIC X(20).
                   07  FILLER       PIC X.
                   07  DL-SSNO      PIC 999/99/9999.
      * TRAMT STARTS IN 53
                   07  DL-TRAMT     PIC ZZZZZ9.99-.
               05  DI-LINE REDEFINES DC-LINE.
                   07  FILLER       PIC X(38).
                   07  DI-TIME      PIC 99/99.
                   07  FILLER       PIC X.
                   07  DI-USERID    PIC X(08).
                   07  FILLER       PIC X.
                   07  DI-TRAMT     PIC ZZZZZ9.99-.

           03  D-PAGENO             PIC ZZ9.
           03  D-END-MORE           PIC X(7).
           03  D-HEAD               PIC X(76).

       01  TRI-HEAD.
           03  FILLER               PIC X(51) VALUE
               "  DATE  CODE TYP PRODUC    DRAFT  NAME ".
           03  FILLER               PIC X(24) VALUE
               "TIME   USERID     AMOUNT".

       01  TRW-HEAD.
           03  FILLER               PIC X(39) VALUE
               "  DATE  CODE TIL   TIME       USERID   ".
           03  FILLER               PIC X(37) VALUE
               "            CLOSING BAL    OVER/SHORT".
       01  TRB-HEAD.
           03  FILLER               PIC X(39) VALUE
               "  DATE  CODE ACCTNO               NAME ".
           03  FILLER               PIC X(37) VALUE
               "                SOC SEC #      AMOUNT".

       01  TRJ-HEAD.
           03  FILLER               PIC X(39) VALUE
               "  DATE  CODE ACCTNO               NAME ".
           03  FILLER               PIC X(37) VALUE
               "                SOC SEC #      AMOUNT".

       01  TRK-HEAD.
           03  FILLER               PIC X(39) VALUE
               "  DATE  CODE ACCTNO               NAME ".
           03  FILLER               PIC X(37) VALUE
               "                SOC SEC #      AMOUNT".
       01  TRC-HEAD.
           03  FILLER               PIC X(35) VALUE
               "  DATE  CODE TYPE CHECK#  PAYEE    ".
           03  FILLER               PIC X(41) VALUE
               "                        REF #      AMOUNT".
       01  TRG-HEAD.
           03  FILLER               PIC X(39) VALUE
               "  DATE  CODE ACCTNO      TYPE NAME     ".
           03  FILLER               PIC X(37) VALUE
               "               SOC SEC #      AMOUNT".
       01  TRP-HEAD.
           03  FILLER               PIC X(39) VALUE
               "  DATE  CODE ACCTNO          TYPE NAME ".
           03  FILLER               PIC X(37) VALUE
               "                SOC SEC #      AMOUNT".
       01  TRL-HEAD.
           03  FILLER               PIC X(39) VALUE
               "  DATE  CODE ACCTNO               NAME ".
           03  FILLER               PIC X(37) VALUE
               "                SOC SEC #      AMOUNT".
       01  TRS-HEAD.
           03  FILLER               PIC X(39) VALUE
               "  DATE  CODE ACCTNO               NAME ".
           03  FILLER               PIC X(37) VALUE
               "                SOC SEC #      AMOUNT".
       01  TRX-HEAD.
           03  FILLER               PIC X(39) VALUE
               "  DATE  CODE ACCTNO               NAME ".
           03  FILLER               PIC X(37) VALUE
               "                SOC SEC #      AMOUNT".
       01  ALL-HEAD.
           03  FILLER               PIC X(35) VALUE
               "  DATE  CODE                       ".
           03  FILLER               PIC X(41) VALUE
               "                                   AMOUNT".

       01  CURRENT-PAGE             PIC S999.
       01  PAGE-COUNT               PIC 999.
       01  MAX-PAGES                PIC 999    VALUE 250.
      *01  PAGE-KEY-TABLE.
      *    03  PAGE-KEY             PIC X(14) OCCURS 250 TIMES.
      *01  KEY-SELECT-TABLE.
      *    03  SELECT-KEY           PIC X(14) OCCURS 9 TIMES.

       01  PAGE-KEY-TABLE.
           03  PAGE-KEY             PIC X(27) OCCURS 250 TIMES.
       01  KEY-SELECT-TABLE.
           03  SELECT-KEY           PIC X(27) OCCURS 9 TIMES.
      *****************************************************************
      *COPY "LIBGB/PHON400.CPY".

       COPY "LIBWI/TRSCAN_CMD.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       01 TRSCAN-WINDOW-HANDLE PIC X(10) VALUE SPACES.
       COPY "LIBGB/FLDSELW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/TRSCAN_DEF.CPY".
       COPY "LIBWI/TRSCAN_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/ACCESSW.CPY".

       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBWI/TRSCAN_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               WK-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-TRX1-FILE.
           PERFORM CLOSE-TRW1-FILE.
           PERFORM CLOSE-TRS1-FILE.
           PERFORM CLOSE-TRP1-FILE.
           PERFORM CLOSE-TRL1-FILE.
           PERFORM CLOSE-TRK1-FILE.
           PERFORM CLOSE-TRJ1-FILE.
           PERFORM CLOSE-TRI1-FILE.
           PERFORM CLOSE-TRD1-FILE.
           PERFORM CLOSE-TRC1-FILE.
           PERFORM CLOSE-TRB1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-BR-FILE.
           CLOSE
               WK-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      *****************************************************************
      *
      *    M A I N    P R O G R A M
      *
      *****************************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.
       COPY "LIBGB/CHKSEC.CPY".
           MOVE "TRSCAN" TO VDU-FORM-NAME.
           MOVE FORM-PATHNAME TO FORM-PATH.

           DISPLAY FLOATING GRAPHICAL WINDOW
                    SCREEN LINE TRSCAN-BEGIN-Y * VDU-WIN-L-OFFSET
                    SCREEN COL TRSCAN-BEGIN-X * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-SCAN,CONTROL VALUE IS VDU-WCTRL-SCAN
                   CELL SIZE IS LABEL FONT
                   SIZE TRSCAN-COLUMNS + VDU-WIN-W-MOD
                   LINES TRSCAN-COLUMNS
                   BLANK SCREEN
                   HANDLE IS TRSCAN-WINDOW-HANDLE.
           PERFORM FORM-RESET.

      *----------------------------------------------------------------
       MAIN-MODULE.
           MOVE " "      TO VDU-WIMENU-SELECTION.

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           IF STAT NOT = "00"
              MOVE "CANNOT MAKE WK SORT KEY FILE" TO MESS
              PERFORM SEND-MESS
              GO TO END-ROUTINE.

           PERFORM OPEN-WK-FILE-OUTPUT.
           PERFORM CLOSE-WK-FILE.
           PERFORM OPEN-WK-FILE.

      *----------------------------------------------------------------
      *    ENTER BRANCH
      *----------------------------------------------------------------
      *ENTER-BRANCH.
      *    MOVE LEGEND-01      TO MESS.
      *    PERFORM SEND-LEGEND.
      *    MOVE "ENNN040BRANCH." TO VDU.
      *    PERFORM SCREENIO.

      *    IF VDUSTATUS = "1>"
      *       GO TO END-ROUTINE.

      *    IF VDUSTATUS NOT = "00"
      *       GO TO ENTER-BRANCH.

      *    MOVE VDUNUM0 TO SCAN-BRANCH BR-NO.

           MOVE "SNNN040BRANCH." TO VDU.
           MOVE EXT-CONAME-USER-LOC TO SCAN-BRANCH BR-NO VDUNUM0.
           PERFORM SCREENIO.

           PERFORM GET-BR.
           IF IO-FG NOT = 0
              MOVE "INVALID BRANCH #, PLEASE RE-ENTER" TO MESS
              PERFORM SEND-MESS
              GO TO END-ROUTINE.

      *----------------------------------------------------------------
      *    ENTER MONTH
      *----------------------------------------------------------------
       ENTER-MONTH.
           MOVE LEGEND-02      TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENNN040MMYY." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>"
              GO TO END-ROUTINE.

           IF VDUSTATUS NOT = "00"
              GO TO ENTER-MONTH.

           MOVE VDUNUM0 TO SCAN-MMYY.
           IF SCAN-MM < 1 OR > 12
              MOVE "INVALID MMYY, PLEASE RE-ENTER" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-MONTH.

           MOVE "S  A000CODE." TO VDU.
           MOVE "A"            TO SCAN-CODE VDUBUF.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
      *    ENTER DAY
      *----------------------------------------------------------------
       ENTER-DAY.
           MOVE LEGEND-04     TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENNN020DAY." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "16"
              MOVE 99 TO SCAN-DAY
              GO TO BEGIN-SCAN.

           IF VDUSTATUS = "1>"
              GO TO END-ROUTINE.

           IF VDUSTATUS = "1U"
              GO TO ENTER-MONTH.

           IF VDUSTATUS NOT = "00"
              GO TO ENTER-DAY.

           MOVE VDUNUM0 TO SCAN-DAY.
           IF SCAN-DAY = 0
              MOVE 99 TO SCAN-DAY.

       BEGIN-SCAN.
           MOVE BR-MACHINE TO TRB-ALL-MACHINE
                              TRC-ALL-MACHINE
                              TRD-ALL-MACHINE
                              TRI-ALL-MACHINE
                              TRJ-ALL-MACHINE
                              TRK-ALL-MACHINE
                              TRL-ALL-MACHINE
                              TRP-ALL-MACHINE
                              TRS-ALL-MACHINE
                              TRW-ALL-MACHINE
                              TRX-ALL-MACHINE.
           MOVE BR-DATA-PATH TO TRB-ALL-DATA
                                TRC-ALL-DATA
                                TRD-ALL-DATA
                                TRI-ALL-DATA
                                TRJ-ALL-DATA
                                TRK-ALL-DATA
                                TRL-ALL-DATA
                                TRP-ALL-DATA
                                TRS-ALL-DATA
                                TRW-ALL-DATA
                                TRX-ALL-DATA.

           PERFORM LOAD-TRB1-FILE.
           PERFORM LOAD-TRC1-FILE.
           PERFORM LOAD-TRD1-FILE.
           PERFORM LOAD-TRI1-FILE.
           PERFORM LOAD-TRJ1-FILE.
           PERFORM LOAD-TRK1-FILE.
           PERFORM LOAD-TRL1-FILE.
           PERFORM LOAD-TRP1-FILE.
           PERFORM LOAD-TRS1-FILE.
           PERFORM LOAD-TRW1-FILE.
           PERFORM LOAD-TRX1-FILE.

      * ALL THESE SCANS LOAD THE WORK FILE TO DISPLAY

           MOVE SCAN-MMYY    TO DATE-MMYY.
           MOVE DATE-MMYY-MM TO DATE-MMDDYY-MM.
           MOVE DATE-MMYY-YY TO DATE-MMDDYY-YY.
           MOVE 1            TO DATE-MMDDYY-DD.
           IF SCAN-DAY NOT = 99 AND NOT = 0
              MOVE SCAN-DAY TO DATE-MMDDYY-DD.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO BEGINNING-DATE.
           IF DATE-YYYYMMDD = 0
              MOVE 19000101 TO BEGINNING-DATE.

           PERFORM SCAN-TRB-FILE.
           PERFORM SCAN-TRC-FILE.
           PERFORM SCAN-TRD-FILE.
           PERFORM SCAN-TRI-FILE.
           PERFORM SCAN-TRJ-FILE.
           PERFORM SCAN-TRK-FILE.
           PERFORM SCAN-TRL-FILE.
           PERFORM SCAN-TRP-FILE.
           PERFORM SCAN-TRS-FILE.
           PERFORM SCAN-TRW-FILE.
           PERFORM SCAN-TRX-FILE.
                                 
           PERFORM CLOSE-WK-FILE.

           PERFORM OPEN-WK-FILE.

           MOVE FIRST-FIELD TO FLDSEL-FIELD.
           MOVE 1           TO PAGE-COUNT ONE.
           MOVE 0           TO CURRENT-PAGE.
           MOVE SPACES      TO WK-KEY.

           MOVE BEGINNING-DATE TO WK-DATE.
           MOVE WK-KEY      TO PAGE-KEY(1).

      *----------------------------------------------------------------
       START-BUILD-PAGENO.
           ADD ONE TO CURRENT-PAGE.

           IF CURRENT-PAGE < 1
              MOVE FIRST-FIELD TO FLDSEL-FIELD
              MOVE 1           TO CURRENT-PAGE.

           IF CURRENT-PAGE > PAGE-COUNT
              MOVE FIRST-FIELD TO FLDSEL-FIELD
              MOVE 1           TO CURRENT-PAGE.

           MOVE SPACES TO DISPLAY-BUF.
           MOVE 1      TO SUB.
           MOVE PAGE-KEY(CURRENT-PAGE) TO WK-KEY.
           PERFORM START-WK-FILE.

      *----------------------------------------------------------------
      *    BUILD PAGE
      *----------------------------------------------------------------
       BUILD-PAGENO.
           PERFORM READ-WK-FILE-NEXT.
           IF IO-FG NOT = 0 OR SUB > 9
              GO TO DISPLAY-PAGE.

           MOVE WK-DISPLAY   TO D-LINE(SUB).
           MOVE WK-KEY       TO SELECT-KEY(SUB).

           ADD 1 TO SUB.
           GO TO BUILD-PAGENO.

      *----------------------------------------------------------------
      *    DISPLAY PAGE
      *----------------------------------------------------------------
       DISPLAY-PAGE.
           IF IO-FG NOT = 0
              IF SUB = 1
                 MOVE "NO RECORDS FOUND!" TO MESS
                 PERFORM SEND-LEGEND
                 MOVE DISPLAY-BUF TO WR-BUF
                 MOVE DISPLAY-LIST TO WR-LIST
                 PERFORM CALL-WRFORM
                 GO TO ENTER-MONTH.

           MOVE "MORE..." TO D-END-MORE.

           IF CURRENT-PAGE = PAGE-COUNT
              IF IO-FG = 0
                 IF PAGE-COUNT < MAX-PAGES
                    ADD 1 TO PAGE-COUNT
                    MOVE WK-KEY TO PAGE-KEY(PAGE-COUNT)
                 ELSE
                    MOVE "MAXIMUM PAGES REACHED!" TO MESS
                    PERFORM SEND-LEGEND
                    MOVE "OVRFLOW" TO D-END-MORE
              ELSE
                 MOVE "* END *" TO D-END-MORE.

           MOVE CURRENT-PAGE TO D-PAGENO.

           MOVE ALL-HEAD TO D-HEAD.
           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *----------------------------------------------------------------
      *    SELECT FIELD FROM PAGES
      *----------------------------------------------------------------
       SELECT-FIELD-FROM-DISPLAY.
           MOVE LEGEND-05 TO MESS.
           PERFORM SEND-LEGEND.
           PERFORM CALL-FLDSEL.

      * TEST UP ARROW:
           IF FLDSEL-STAT = "1U"
              IF CURRENT-PAGE = 1
                 PERFORM CALL-ALARM
                 GO TO SELECT-FIELD-FROM-DISPLAY
              ELSE
                 MOVE -1 TO ONE
                 MOVE LAST-FIELD TO FLDSEL-FIELD
                 GO TO START-BUILD-PAGENO.

      * TEST PAGE UP:
           IF FLDSEL-STAT = "1P"
              IF CURRENT-PAGE = 1
                 PERFORM CALL-ALARM
                 GO TO SELECT-FIELD-FROM-DISPLAY
              ELSE
                 MOVE -1 TO ONE
                 MOVE FIRST-FIELD TO FLDSEL-FIELD
                 GO TO START-BUILD-PAGENO.

      * TEST PAGE DOWN OR DOWN ARROW:
           IF FLDSEL-STAT = "1N" OR FLDSEL-STAT = "1D"
              IF CURRENT-PAGE NOT < PAGE-COUNT
                 PERFORM CALL-ALARM
                 GO TO SELECT-FIELD-FROM-DISPLAY
              ELSE
                 MOVE 1 TO ONE
                 MOVE FIRST-FIELD TO FLDSEL-FIELD
                 GO TO START-BUILD-PAGENO.

      * TEST HOME:
           IF FLDSEL-STAT = "1H"
              MOVE 0 TO CURRENT-PAGE
              MOVE 1 TO ONE
              MOVE FIRST-FIELD TO FLDSEL-FIELD
              GO TO START-BUILD-PAGENO.

           IF FLDSEL-STAT = "1>"
              GO TO END-ROUTINE.
           IF FLDSEL-STAT = "10"
              GO TO ENTER-DAY.
           IF NOT (FLDSEL-STAT = "00" OR "1<")
              PERFORM CALL-ALARM
              GO TO SELECT-FIELD-FROM-DISPLAY.

           GO TO SELECT-FIELD-FROM-DISPLAY.


      ***************************************************************
       SCAN-TRB-FILE SECTION.
      ***************************************************************

           PERFORM OPEN-TRB1-FILE.
           MOVE 0 TO WK-SEQ.

           MOVE BR-NO          TO TB-BRNO.
           MOVE BEGINNING-DATE TO TB-DATE.
           MOVE 0              TO TB-CLASS.
           MOVE 0              TO TB-ACCTNO TB-LAWCODE TB-LINE.

           PERFORM START-TRB1-FILE-SCAN.

       SCAN-TRB-NEXT.
           PERFORM READ-TRB1-FILE-NEXT.
           IF (IO-FG NOT = 0) OR (TRB-PATH-OWNBR NOT = TB-BRNO)
              GO TO EXIT-SCAN-TRB.

           MOVE TB-DATE TO DATE-YYYYMMDD.

           IF SCAN-DAY NOT = 99
              IF DATE-YYYYMMDD-DD NOT = SCAN-DAY
                 GO TO EXIT-SCAN-TRB.

           IF DATE-YYYYMMDD-MM NOT = SCAN-MM OR
               DATE-YYYYMMDD-YY NOT = SCAN-YY
                 GO TO EXIT-SCAN-TRB.

           MOVE SPACES TO D-LINE(1).

           MOVE TB-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY  TO D-DATE(1).
           MOVE "B"          TO D-CODE(1).

           MOVE TB-CLASS   TO DL-CLASS(1).
           MOVE "-"        TO DL-DASH1(1) DL-DASH2(1).
           MOVE TB-ACCTNO  TO DL-NUMBER(1).
           MOVE TB-LAWCODE TO DL-SPCLASS(1).
           MOVE TB-SSNO(1) TO BW-SSNO DL-SSNO(1).
           MOVE 1 TO SUB.
           PERFORM FIND-BORROWER.
           INSPECT DL-SSNO(1) REPLACING ALL "/" BY "-".
           MOVE TB-LNAMT   TO DL-TRAMT(1).

           MOVE TB-DATE   TO WK-DATE.
           MOVE "B"       TO WK-CODE.
           ADD 1          TO WK-SEQ.
           MOVE D-LINE(1) TO WK-DISPLAY.
           PERFORM WRITE-WK-FILE.

           GO TO SCAN-TRB-NEXT.

       EXIT-SCAN-TRB.
           PERFORM CLOSE-TRB1-FILE.

      ***************************************************************
       SCAN-TRC-FILE SECTION.
      ***************************************************************

           PERFORM OPEN-TRC1-FILE.
           MOVE 0 TO WK-SEQ.

           MOVE BR-NO          TO TC-BRNO.
           MOVE BEGINNING-DATE TO TC-KEY-DATE.
           MOVE 0              TO TC-CKNO TC-CLASS.
           MOVE 0              TO TC-SEQ.

           PERFORM START-TRC1-FILE-SCAN.

       SCAN-TRC-NEXT.
           PERFORM READ-TRC1-FILE-NEXT.
           IF (IO-FG NOT = 0) OR (TRC-PATH-OWNBR NOT = TC-BRNO)
              GO TO EXIT-SCAN-TRC.

           MOVE TC-KEY-DATE TO DATE-YYYYMMDD.

           IF SCAN-DAY NOT = 99
              IF DATE-YYYYMMDD-DD NOT = SCAN-DAY
                 GO TO EXIT-SCAN-TRC.

           IF DATE-YYYYMMDD-MM NOT = SCAN-MM OR
               DATE-YYYYMMDD-YY NOT = SCAN-YY
                 GO TO EXIT-SCAN-TRC.

           MOVE SPACES TO D-LINE(1).

           MOVE TC-KEY-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY  TO D-DATE(1).
           MOVE "C"          TO D-CODE(1).

           MOVE TC-PAID TO DC-PAID(1).
           IF TC-VOID = "Y"
              MOVE "*" TO DC-VOID(1).
           MOVE TC-CKNO  TO DC-CKNO(1).
           MOVE TC-AMT   TO DC-CKAMT(1).
           MOVE TC-PAYEE TO DC-PAYEE(1).
           IF TC-REF NOT = 0
              MOVE TC-REF TO DC-REFNO(1).

           MOVE TC-KEY-DATE TO WK-DATE.
           MOVE "C"         TO WK-CODE.
           ADD 1            TO WK-SEQ.
           MOVE D-LINE(1)   TO WK-DISPLAY.
           PERFORM WRITE-WK-FILE.

           GO TO SCAN-TRC-NEXT.

       EXIT-SCAN-TRC.
           PERFORM CLOSE-TRC1-FILE.

      ***************************************************************
       SCAN-TRD-FILE SECTION.
      ***************************************************************

           PERFORM OPEN-TRD1-FILE.
           MOVE 0 TO WK-SEQ.

           MOVE BR-NO          TO TD-BRNO.
           MOVE BEGINNING-DATE TO TD-DATE.

           PERFORM START-TRD1-FILE.

       SCAN-TRD-NEXT.
           PERFORM READ-TRD1-FILE-NEXT.
           IF (IO-FG NOT = 0) OR (TRD-PATH-OWNBR NOT = TD-BRNO)
              GO TO EXIT-SCAN-TRD.

           MOVE TD-DATE TO DATE-YYYYMMDD.

           IF SCAN-DAY NOT = 99
              IF DATE-YYYYMMDD-DD NOT = SCAN-DAY
                 GO TO EXIT-SCAN-TRD.

           IF DATE-YYYYMMDD-MM NOT = SCAN-MM OR
               DATE-YYYYMMDD-YY NOT = SCAN-YY
                 GO TO EXIT-SCAN-TRD.

           MOVE SPACES TO D-LINE(1).

           MOVE TD-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY  TO D-DATE(1).
           MOVE "D"          TO D-CODE(1).

           MOVE "DEPOSIT"  TO DC-PAYEE(1).
           MOVE TD-DEPOSIT TO DC-CKAMT(1).

           MOVE TD-DATE     TO WK-DATE.
           MOVE "D"         TO WK-CODE.
           ADD 1            TO WK-SEQ.
           MOVE D-LINE(1)   TO WK-DISPLAY.
           PERFORM WRITE-WK-FILE.

           GO TO SCAN-TRD-NEXT.

       EXIT-SCAN-TRD.
           PERFORM CLOSE-TRD1-FILE.

      ***************************************************************
       SCAN-TRI-FILE SECTION.
      ***************************************************************

           PERFORM OPEN-TRI1-FILE.
           MOVE 0 TO WK-SEQ.

           MOVE BR-NO          TO TI-BRNO.
           MOVE BEGINNING-DATE TO TI-KEY-DATE.
           MOVE 0              TO TI-SEQ.

           PERFORM START-TRI1-FILE-SCAN.
      D    STOP MESS.

       SCAN-TRI-NEXT.
           PERFORM READ-TRI1-FILE-NEXT.
           IF (IO-FG NOT = 0) OR (TRI-PATH-OWNBR NOT = TI-BRNO)
              GO TO EXIT-SCAN-TRI.

           MOVE TI-KEY-DATE TO DATE-YYYYMMDD.

           IF SCAN-DAY NOT = 99
              IF DATE-YYYYMMDD-DD NOT = SCAN-DAY
                 GO TO EXIT-SCAN-TRI.

           IF DATE-YYYYMMDD-MM NOT = SCAN-MM OR
               DATE-YYYYMMDD-YY NOT = SCAN-YY
                 GO TO EXIT-SCAN-TRI.

           MOVE SPACES TO D-LINE(1).

           MOVE TI-KEY-DATE  TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY  TO D-DATE(1).
           MOVE "I"          TO D-CODE(1).

      *  NUM-DATE 00HHMMSS
           MOVE TI-POSTTIME TO NUM-DATE.
           MOVE NUM-MO   TO NUM-DA.
           MOVE NUM-YR   TO NUM-MO.
           MOVE 0        TO NUM-CCYY.
           MOVE NUM-DATE TO DI-TIME(1).
           INSPECT DI-TIME(1) REPLACING ALL "/" BY ":".

           MOVE  TI-USERID   TO DI-USERID(1).
           MOVE  TI-AMOUNT   TO DI-TRAMT(1).

           MOVE TI-KEY-DATE TO WK-DATE.
           MOVE "I"         TO WK-CODE.
           ADD 1            TO WK-SEQ.
           MOVE D-LINE(1)   TO WK-DISPLAY.
           PERFORM WRITE-WK-FILE.

           GO TO SCAN-TRI-NEXT.

       EXIT-SCAN-TRI.
           PERFORM CLOSE-TRI1-FILE.

      ***************************************************************
       SCAN-TRJ-FILE SECTION.
      ***************************************************************

           PERFORM OPEN-TRJ1-FILE.
           MOVE 0 TO WK-SEQ.

           MOVE BR-NO          TO TJ-BRNO.
           MOVE BEGINNING-DATE TO TJ-DATE.
           MOVE 0              TO TJ-CLASS.
           MOVE 0              TO TJ-ACCTNO TJ-LAWCODE TJ-LINE.

           PERFORM START-TRJ1-FILE-SCAN.

       SCAN-TRJ-NEXT.
           PERFORM READ-TRJ1-FILE-NEXT.
           IF (IO-FG NOT = 0) OR (TRJ-PATH-OWNBR NOT = TJ-BRNO)
              GO TO EXIT-SCAN-TRJ.

           MOVE TJ-DATE TO DATE-YYYYMMDD.

           IF SCAN-DAY NOT = 99
              IF DATE-YYYYMMDD-DD NOT = SCAN-DAY
                 GO TO EXIT-SCAN-TRJ.

           IF DATE-YYYYMMDD-MM NOT = SCAN-MM OR
               DATE-YYYYMMDD-YY NOT = SCAN-YY
                 GO TO EXIT-SCAN-TRJ.

           MOVE SPACES TO D-LINE(1).

           MOVE TJ-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY  TO D-DATE(1).
           MOVE "J"          TO D-CODE(1).

           MOVE 1 TO SUB.
           MOVE TJ-CLASS   TO DL-CLASS(SUB).
           MOVE "-"        TO DL-DASH1(SUB) DL-DASH2(SUB).
           MOVE TJ-ACCTNO  TO DL-NUMBER(SUB).
           MOVE TJ-LAWCODE TO DL-SPCLASS(SUB).
           MOVE TJ-SSNO(1) TO BW-SSNO DL-SSNO(SUB).
           PERFORM FIND-BORROWER.
           INSPECT DL-SSNO(SUB) REPLACING ALL "/" BY "-".
           MOVE TJ-LNAMT   TO DL-TRAMT(SUB).

           MOVE TJ-DATE   TO WK-DATE.
           MOVE "J"       TO WK-CODE.
           ADD 1          TO WK-SEQ.
           MOVE D-LINE(1) TO WK-DISPLAY.
           PERFORM WRITE-WK-FILE.

           GO TO SCAN-TRJ-NEXT.

       EXIT-SCAN-TRJ.
           PERFORM CLOSE-TRJ1-FILE.

      ***************************************************************
       SCAN-TRK-FILE SECTION.
      ***************************************************************

           PERFORM OPEN-TRK1-FILE.
           MOVE 0 TO WK-SEQ.

           MOVE BR-NO          TO TK-BRNO.
           MOVE BEGINNING-DATE TO TK-DATE.
           MOVE 0              TO TK-CLASS.
           MOVE 0              TO TK-ACCTNO TK-LAWCODE TK-LINE.


           PERFORM START-TRK1-FILE-SCAN.

       SCAN-TRK-NEXT.
           PERFORM READ-TRK1-FILE-NEXT.
           IF (IO-FG NOT = 0) OR (TRK-PATH-OWNBR NOT = TK-BRNO)
              GO TO EXIT-SCAN-TRK.

           MOVE TK-DATE TO DATE-YYYYMMDD.

           IF SCAN-DAY NOT = 99
              IF DATE-YYYYMMDD-DD NOT = SCAN-DAY
                 GO TO EXIT-SCAN-TRK.

           IF DATE-YYYYMMDD-MM NOT = SCAN-MM OR
               DATE-YYYYMMDD-YY NOT = SCAN-YY
                 GO TO EXIT-SCAN-TRK.

           MOVE SPACES TO D-LINE(1).

           MOVE TK-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY  TO D-DATE(1).
           MOVE "K"          TO D-CODE(1).

           MOVE 1 TO SUB.
           MOVE TK-CLASS   TO DL-CLASS(SUB).
           MOVE "-"        TO DL-DASH1(SUB) DL-DASH2(SUB).
           MOVE TK-ACCTNO  TO DL-NUMBER(SUB).
           MOVE TK-LAWCODE TO DL-SPCLASS(SUB).
           MOVE TK-SSNO(1) TO BW-SSNO DL-SSNO(SUB).
           PERFORM FIND-BORROWER.
           INSPECT DL-SSNO(SUB) REPLACING ALL "/" BY "-".
           MOVE TK-LNAMT   TO DL-TRAMT(SUB).

           MOVE TK-DATE   TO WK-DATE.
           MOVE "K"       TO WK-CODE.
           ADD 1          TO WK-SEQ.
           MOVE D-LINE(1) TO WK-DISPLAY.
           PERFORM WRITE-WK-FILE.

           GO TO SCAN-TRK-NEXT.

       EXIT-SCAN-TRK.
           PERFORM CLOSE-TRK1-FILE.

      ***************************************************************
       SCAN-TRL-FILE SECTION.
      ***************************************************************

           PERFORM OPEN-TRL1-FILE.
           MOVE 0 TO WK-SEQ.

           MOVE BR-NO          TO TL-BRNO.
           MOVE BEGINNING-DATE TO TL-DATE.
           MOVE 0              TO TL-CLASS.
           MOVE 0              TO TL-ACCTNO TL-LAWCODE TL-LINE.

           PERFORM START-TRL1-FILE-SCAN.

       SCAN-TRL-NEXT.
           PERFORM READ-TRL1-FILE-NEXT.
           IF (IO-FG NOT = 0) OR (TRL-PATH-OWNBR NOT = TL-BRNO)
              GO TO EXIT-SCAN-TRL.

           MOVE TL-DATE TO DATE-YYYYMMDD.

           IF SCAN-DAY NOT = 99
              IF DATE-YYYYMMDD-DD NOT = SCAN-DAY
                 GO TO EXIT-SCAN-TRL.

           IF DATE-YYYYMMDD-MM NOT = SCAN-MM OR
               DATE-YYYYMMDD-YY NOT = SCAN-YY
                 GO TO EXIT-SCAN-TRL.

           MOVE SPACES TO D-LINE(1).

           MOVE TL-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY  TO D-DATE(1).
           MOVE "L"          TO D-CODE(1).

           MOVE 1 TO SUB.
           MOVE TL-CLASS   TO DL-CLASS(SUB).
           MOVE "-"        TO DL-DASH1(SUB) DL-DASH2(SUB).
           MOVE TL-ACCTNO  TO DL-NUMBER(SUB).
           MOVE TL-LAWCODE TO DL-SPCLASS(SUB).
           MOVE TL-SSNO(1) TO BW-SSNO DL-SSNO(SUB).
           PERFORM FIND-BORROWER.
           INSPECT DL-SSNO(SUB) REPLACING ALL "/" BY "-".
           MOVE TL-LNAMT   TO DL-TRAMT(SUB).

           MOVE TL-DATE   TO WK-DATE.
           MOVE "L"       TO WK-CODE.
           ADD 1          TO WK-SEQ.
           MOVE D-LINE(1) TO WK-DISPLAY.
           PERFORM WRITE-WK-FILE.

           GO TO SCAN-TRL-NEXT.

       EXIT-SCAN-TRL.
           PERFORM CLOSE-TRL1-FILE.

      ***************************************************************
       SCAN-TRP-FILE SECTION.
      ***************************************************************

           PERFORM OPEN-TRP1-FILE.
           MOVE 0 TO WK-SEQ.

           MOVE BR-NO          TO TP-BRNO.
           MOVE BEGINNING-DATE TO TP-DATE.
           MOVE 0              TO TP-CLASS.
           MOVE 0              TO TP-ACCTNO TP-LINE.

           PERFORM START-TRP1-FILE.

       SCAN-TRP-NEXT.
           PERFORM READ-TRP1-FILE-NEXT.
           IF (IO-FG NOT = 0) OR (TRP-PATH-OWNBR NOT = TP-BRNO)
              GO TO EXIT-SCAN-TRP.

           MOVE TP-DATE TO DATE-YYYYMMDD.

           IF SCAN-DAY NOT = 99
              IF DATE-YYYYMMDD-DD NOT = SCAN-DAY
                 GO TO EXIT-SCAN-TRP.

           IF DATE-YYYYMMDD-MM NOT = SCAN-MM OR
               DATE-YYYYMMDD-YY NOT = SCAN-YY
                 GO TO EXIT-SCAN-TRP.

           MOVE SPACES TO D-LINE(1).

           MOVE TP-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY  TO D-DATE(1).
           MOVE "P"          TO D-CODE(1).

           MOVE 1 TO SUB.

           MOVE TP-CLASS   TO DP-CLASS(SUB).
           MOVE "-"        TO DP-DASH1(SUB) DP-DASH2(SUB).
           MOVE TP-ACCTNO  TO DP-NUMBER(SUB).
           MOVE TP-LAWCODE TO DP-SPCLASS(SUB).
           MOVE TP-PLCD    TO DP-PLCD(SUB).
           MOVE TP-JDCD    TO DP-JDCD(SUB).

              MOVE TP-SSNO TO BW-SSNO DP-SSNO(SUB)
              PERFORM FIND-BORROWER
              INSPECT DP-SSNO(SUB) REPLACING ALL "/" BY "-"
              MOVE TP-TRCD TO DP-TRCD(SUB).

           IF TP-TRCD = "OT"
              MOVE TP-INTDUE TO TP-TRAMT
              MOVE 0         TO TP-INTDUE.

              IF TP-TRCD = "WL" OR "WI"
                 ADD TP-APINT TP-APLC GIVING DP-TRAMT(SUB)
              ELSE
                 MOVE TP-TRAMT TO DP-TRAMT(SUB)
                 IF ( (TP-TRCD = "RT" OR "ET" OR "LP" OR "NP" OR "ST")
                   OR (TP-TRCD = "RV" AND
                      (TP-ADDON-TEXT = "ADD" OR "ADJ" OR "CNL") ) )
                    PERFORM SETUP-D-TRAMT.

           MOVE TP-DATE   TO WK-DATE.
           MOVE "P"       TO WK-CODE.
           ADD 1          TO WK-SEQ.
           MOVE D-LINE(1) TO WK-DISPLAY.
           PERFORM WRITE-WK-FILE.

           GO TO SCAN-TRP-NEXT.

       EXIT-SCAN-TRP.
           PERFORM CLOSE-TRP1-FILE.

      ***************************************************************
       SCAN-TRS-FILE SECTION.
      ***************************************************************

           PERFORM OPEN-TRS1-FILE.
           MOVE 0 TO WK-SEQ.

           MOVE BR-NO          TO TS-BRNO.
           MOVE BEGINNING-DATE TO TS-DATE.
           MOVE 0              TO TS-CLASS.
           MOVE 0              TO TS-ACCTNO TS-LAWCODE TS-LINE.

           PERFORM START-TRS1-FILE-SCAN.

       SCAN-TRS-NEXT.
           PERFORM READ-TRS1-FILE-NEXT.
           IF (IO-FG NOT = 0) OR (TRS-PATH-OWNBR NOT = TS-BRNO)
              GO TO EXIT-SCAN-TRS.

           MOVE TS-DATE TO DATE-YYYYMMDD.

           IF SCAN-DAY NOT = 99
              IF DATE-YYYYMMDD-DD NOT = SCAN-DAY
                 GO TO EXIT-SCAN-TRS.

           IF DATE-YYYYMMDD-MM NOT = SCAN-MM OR
               DATE-YYYYMMDD-YY NOT = SCAN-YY
                 GO TO EXIT-SCAN-TRS.

           MOVE SPACES TO D-LINE(1).

           MOVE TS-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY  TO D-DATE(1).
           MOVE "S"          TO D-CODE(1).

           MOVE 1 TO SUB.
           MOVE TS-CLASS   TO DL-CLASS(SUB).
           MOVE "-"        TO DL-DASH1(SUB) DL-DASH2(SUB).
           MOVE TS-ACCTNO  TO DL-NUMBER(SUB).
           MOVE TS-LAWCODE TO DL-SPCLASS(SUB).
           MOVE TS-SSNO(1) TO BW-SSNO DL-SSNO(SUB).
           PERFORM FIND-BORROWER.
           INSPECT DL-SSNO(SUB) REPLACING ALL "/" BY "-".
           MOVE TS-LNAMT   TO DL-TRAMT(SUB).

           MOVE TS-DATE   TO WK-DATE.
           MOVE "S"       TO WK-CODE.
           ADD 1          TO WK-SEQ.
           MOVE D-LINE(1) TO WK-DISPLAY.
           PERFORM WRITE-WK-FILE.

           GO TO SCAN-TRS-NEXT.

       EXIT-SCAN-TRS.
           PERFORM CLOSE-TRS1-FILE.

      ***************************************************************
       SCAN-TRW-FILE SECTION.
      ***************************************************************

           PERFORM OPEN-TRW1-FILE.
           MOVE 0 TO WK-SEQ.

           MOVE BR-NO          TO TW-BRNO.
           MOVE BEGINNING-DATE TO TW-DATE.
           MOVE 0              TO TW-TILLNO.

           PERFORM START-TRW1-FILE-SCAN.

       SCAN-TRW-NEXT.
           PERFORM READ-TRW1-FILE-NEXT.
           IF (IO-FG NOT = 0) OR (TRW-PATH-OWNBR NOT = TW-BRNO)
              GO TO EXIT-SCAN-TRW.

           MOVE TW-DATE TO DATE-YYYYMMDD.

           IF SCAN-DAY NOT = 99
              IF DATE-YYYYMMDD-DD NOT = SCAN-DAY
                 GO TO EXIT-SCAN-TRW.

           IF DATE-YYYYMMDD-MM NOT = SCAN-MM OR
               DATE-YYYYMMDD-YY NOT = SCAN-YY
                 GO TO EXIT-SCAN-TRW.

           MOVE SPACES TO D-LINE(1).

           MOVE TW-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY  TO D-DATE(1).
           MOVE "W"          TO D-CODE(1).

           MOVE 1 TO SUB.
           MOVE  TW-TILLNO       TO DW-TILLNO(SUB).
           MOVE  TW-TIME(1)      TO DW-TIME(SUB).
           INSPECT DW-TIME(SUB) REPLACING ALL"/" BY":".
           MOVE  TW-POSTNAME(1)  TO DW-USERID(SUB).
           MOVE  TW-AMOUNT(1)    TO DW-AMOUNT(SUB).
           MOVE  TW-OVERSHORT(1) TO DW-OVERSHORT(SUB).

           MOVE TW-DATE     TO WK-DATE.
           MOVE "W"         TO WK-CODE.
           ADD 1            TO WK-SEQ.
           MOVE D-LINE(1)   TO WK-DISPLAY.
           PERFORM WRITE-WK-FILE.

           GO TO SCAN-TRW-NEXT.

       EXIT-SCAN-TRW.
           PERFORM CLOSE-TRW1-FILE.

      ***************************************************************
       SCAN-TRX-FILE SECTION.
      ***************************************************************

           PERFORM OPEN-TRX1-FILE.
           MOVE 0 TO WK-SEQ.

           MOVE BR-NO          TO TX-BRNO.
           MOVE BEGINNING-DATE TO TX-DATE.
           MOVE 0              TO TX-CLASS.
           MOVE 0              TO TX-ACCTNO TX-LAWCODE TX-LINE.

           PERFORM START-TRX1-FILE-SCAN.

       SCAN-TRX-NEXT.
           PERFORM READ-TRX1-FILE-NEXT.
           IF (IO-FG NOT = 0) OR (TRX-PATH-OWNBR NOT = TX-BRNO)
              GO TO EXIT-SCAN-TRX.

           MOVE TX-DATE TO DATE-YYYYMMDD.

           IF SCAN-DAY NOT = 99
              IF DATE-YYYYMMDD-DD NOT = SCAN-DAY
                 GO TO EXIT-SCAN-TRX.

           IF DATE-YYYYMMDD-MM NOT = SCAN-MM OR
               DATE-YYYYMMDD-YY NOT = SCAN-YY
                 GO TO EXIT-SCAN-TRX.

           MOVE SPACES TO D-LINE(1).

           MOVE TX-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY  TO D-DATE(1).
           MOVE "X"          TO D-CODE(1).

           MOVE 1 TO SUB.
           MOVE TX-CLASS   TO DL-CLASS(SUB).
           MOVE "-"        TO DL-DASH1(SUB) DL-DASH2(SUB).
           MOVE TX-ACCTNO  TO DL-NUMBER(SUB).
           MOVE TX-LAWCODE TO DL-SPCLASS(SUB).
           MOVE TX-SSNO(1) TO BW-SSNO DL-SSNO(SUB).
           PERFORM FIND-BORROWER.
           INSPECT DL-SSNO(SUB) REPLACING ALL "/" BY "-".
           MOVE TX-LNAMT   TO DL-TRAMT(SUB).

           MOVE TX-DATE   TO WK-DATE.
           MOVE "X"       TO WK-CODE.
           ADD 1          TO WK-SEQ.
           MOVE D-LINE(1) TO WK-DISPLAY.
           PERFORM WRITE-WK-FILE.

           GO TO SCAN-TRX-NEXT.

       EXIT-SCAN-TRX.
           PERFORM CLOSE-TRX1-FILE.

      *****************************************************************
      *
      *    P R I N T    B O R R O W E R    N A M E
      *
      *****************************************************************
       FIND-BORROWER SECTION.
           PERFORM OPEN-BW1-FILE.
           PERFORM READ-BW1-FILE.
           IF IO-FG = 9
               IF D-CODE(1) = "G"
                  MOVE "** NO BORROWER MASTER **" TO DG-NAME(SUB)
                  GO TO END-FIND-BORROWER
               ELSE
                  MOVE "** NO BORROWER MASTER **" TO DP-NAME(SUB)
                  GO TO END-FIND-BORROWER.

           MOVE BW-LNAME TO NAME-WORKER.
           INSPECT NAME-WORKER REPLACING FIRST "  " BY ",?".
           MOVE BW-FNAME TO FIRST-NAME.

           INSPECT NAME-WORKER REPLACING FIRST "                "
               BY FIRST-NAME.

           INSPECT NAME-WORKER REPLACING ALL "?" BY " ".
           IF D-CODE(1) = "G"
              MOVE NAME-WORKER TO DG-NAME(SUB)
           ELSE
              MOVE NAME-WORKER TO DP-NAME(SUB).

       END-FIND-BORROWER.
           PERFORM CLOSE-BW1-FILE.

      *************************************************************
       FORM-RESET SECTION.
      *************************************************************
           DISPLAY SPACES UPON TRSCAN-WINDOW-HANDLE
                               LINE 0 COL 0 ERASE SCREEN.
           DISPLAY TRSCAN-SCREEN UPON TRSCAN-WINDOW-HANDLE.
           MOVE TRSCAN-FORM-FIELDS TO WR-BUF.
           MOVE TRSCAN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *************************************************************
       SETUP-LINK-INFO SECTION.
      *************************************************************
           MOVE TRSCAN-HELP--FILE TO HELP-LINK-FILE.
           MOVE TRSCAN-HELP--DIR TO HELP-LINK-DIR.

      *************************************************************
       VDU-CONVERT-FIELD SECTION.
      *************************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/TRSCAN_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

       COPY "LIBGB/FLDSEL.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".

      *****************************************************************
      *
      *    M I S C E L L E N O U S
      *
      *****************************************************************
       MISC-PARAGRAPHS SECTION.
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/RMFILE.CPY".
       CALL-ALARM.
           DISPLAY OMITTED WITH BEEP.

      *----------------------------------------------------------------
       GET-BR.
           MOVE 0 TO IO-FG.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.

      *----------------------------------------------------------------
       SETUP-D-TRAMT.
           IF TP-IBPC = "I"
              COMPUTE DP-TRAMT(SUB) = TP-TRAMT * -1
           ELSE
              COMPUTE DP-TRAMT(SUB) = TP-APCUR * -1.

      *----------------------------------------------------------------
       SEND-LEGEND.
           MOVE "S  A600WSEL:D." TO VDU.
           MOVE MESS TO VDUBUF.
           PERFORM SCREENIO.

      *****************************************************************
      *
      *   I - O    R O U T I N E S
      *
      *****************************************************************
       IO-ROUTINES SECTION.
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPTRXGS_SQL.CPY".
       COPY "LIBLP/LPTRWGS_SQL.CPY".
       COPY "LIBLP/LPTRSGS_SQL.CPY".
       COPY "LIBLP/LPTRPGS_SQL.CPY".
       COPY "LIBLP/LPTRLGS_SQL.CPY".
       COPY "LIBLP/LPTRKGS_SQL.CPY".
       COPY "LIBLP/LPTRJGS_SQL.CPY".
       COPY "LIBLP/LPTRIGS_SQL.CPY".
       COPY "LIBLP/LPTRDGS_SQL.CPY".
       COPY "LIBLP/LPTRCGS_SQL.CPY".
       COPY "LIBLP/LPTRBGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBWKIN.CPY".
       COPY "LIBLP/LPTRB1RN.CPY".
       COPY "LIBLP/LPTRC1RN.CPY".
       COPY "LIBLP/LPTRD1RN.CPY".
       COPY "LIBLP/LPTRI1RN.CPY".
       COPY "LIBLP/LPTRJ1RN.CPY".
       COPY "LIBLP/LPTRK1RN.CPY".
       COPY "LIBLP/LPTRL1RN.CPY".
       COPY "LIBLP/LPTRP1RN.CPY".
       COPY "LIBLP/LPTRS1RN.CPY".
       COPY "LIBLP/LPTRW1RN.CPY".
       COPY "LIBLP/LPTRX1RN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      *****************************************************************
      *
      *    E N D   R O U T I N E
      *
      *****************************************************************
       END-ROUTINE SECTION.
           PERFORM CLOSE-FILES.
           MOVE MKTEMP-BUF TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.

      *----------------------------------------------------------------
       EXIT-PROG.

      *----------------------------------------------------------------
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY TRSCAN-SCREEN.
           DESTROY TRSCAN-WINDOW-HANDLE.
           MOVE FORM-PATH TO FORM-PATHNAME.

           EXIT PROGRAM.
