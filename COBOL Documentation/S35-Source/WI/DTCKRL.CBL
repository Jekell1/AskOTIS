       IDENTIFICATION DIVISION.
       PROGRAM-ID.      DTCKRL.
      **********************************************************************
      *                 (WI)
      *
      *  DESCRIPTION:   SELECT INDIVIDUAL ACCTS TO RELEASE
      *                 FROM DLR BATCH CHECK RELEASE (LONPT0)
      *
      *---------------------------------------------------------------------
      * N O T E:
      *
      *    DTCKRL.C  :  SETS ALL SELECTED ACCOUNTS TO DT-SELECTED = "Y"
      *    LONPT0.C  :  SETS ALL ACCOUNTS WITH DE-SELECTED = "Y" TO A "R"
      *---------------------------------------------------------------------
      * REV:
      *  061394 BAH ADDED OPTION TO INCLUDE,SKIP OR ONLY
      *             NEW DEAL PROCEED CHECKS FOR MIAMI
      *  073196 JTG REVISED TO USE SECONDARY KEY
      *  051697 JTG CHANGED TO MAKE READ-NEXT STOP WHEN AT END OF
      *             DT-STATUS = " ", UNRELEASED:
      * 020108 GLB CHANGE WK-FILE, WK-DLSUB
      * BAH 170417 ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 181029 GLOBAL DTFILE
      **********************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT WK-FILE ASSIGN WK-PATH
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY WK-KEY
                  FILE STATUS FILE-STAT.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
      *************************************************
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
           "@(#)WI/DTCKRL.CBL S35 05/02/24-14:30:32 >".
       COPY "LIBLP/LP01DT.CPY".
       COPY "LIBLP/LP01DT_SQL.CPY".
       COPY "LIBLP/LPWSDT.CPY".
       COPY "LIBLP/LP01DL.CPY".
       COPY "LIBLP/LP01DL_SQL.CPY".
       COPY "LIBLP/LPWSDL.CPY".
       01  WK-IFN                PIC XX      VALUE "WK".
       01  WK-PATH               PIC X(30)   VALUE " ".
       01  ERRCD                 PIC X       VALUE SPACES.

       01  DTCKRL-LINES          PIC 99  VALUE 23.
       01  DTCKRL-COLUMNS        PIC 99  VALUE 61.
       01  DTCKRL-BEGIN-Y        PIC 99  VALUE 1.
       01  DTCKRL-BEGIN-X        PIC 99  VALUE 10.

      *    **********************************
      *    *      PROGRAM WORKERS
      *    **********************************
       01  MSEL-LIST           PIC X(8)     VALUE "WSEL:D.".

       01  ONE                 PIC S9       VALUE 1.
       01  SUB                 PIC 999      VALUE 0.

       01  DETAIL-UNPAID        PIC S9(10)V99.
      * GLB
       01  HOLD-DUE            PIC S9(9)V99.

       01  FLDSEL-STAT         PIC XX.
       01  FLDSEL-FIELD        PIC X(4)     VALUE "010.".
       01  FLDSEL-LIST.
           03  FILLER          PIC X(36)    VALUE
               "010 020 030 040 050 060 070 080 090 ".
           03  FILLER      PIC X(24)        VALUE
               "100 110 120 130 140 150.".
      * SIZE 58
       01  FLDSEL-BUF.
           03  FILLER             PIC XXX.
           03  FLDSEL-TRCD        PIC XX.
           03  FILLER             PIC X.
           03  FLDSEL-DLNO        PIC ZZZZ.ZZ.
           03  FILLER             PIC XX.
           03  FLDSEL-CLASS       PIC Z9.
           03  FLDSEL-DASH        PIC X.
           03  FLDSEL-NUMBER      PIC 9(6).
           03  FILLER             PIC XXX.
           03  FLDSEL-DATE        PIC 99/99/99.
           03  FILLER             PIC XX.
           03  FLDSEL-PROCEEDS    PIC Z(6).99-.
           03  FILLER             PIC X(1).
           03  FLDSEL-SEQ         PIC Z(4).
           03  FILLER             PIC XXXX.
           03  FLDSEL-SEL         PIC X.
           03  FILLER             PIC X.

       01  FIRST-FIELD          PIC X(4)  VALUE "010.".
       01  LAST-FIELD           PIC X(4)  VALUE "150.".

       01  PROMPT-LIST          PIC X(6) VALUE "000:S.".

       01  DISPLAY-LIST.
           03  FILLER           PIC X(36)  VALUE
               "010 020 030 040 050 060 070 080 090 ".
           03  FILLER           PIC X(24)  VALUE
               "100 110 120 130 140 150 ".
           03  FILLER           PIC X(15)  VALUE
               "PGNO ENDMORE:F.".
       01  DISPLAY-BUF.
           03  D-LINE                 OCCURS 15 TIMES.
               05  FILLER             PIC XXX.
               05  D-TRCD             PIC XX.
               05  FILLER             PIC X.
               05  D-DLNO             PIC ZZZZ.ZZ.
               05  FILLER             PIC XX.
               05  D-CLASS            PIC Z9.
               05  D-DASH             PIC X.
               05  D-NUMBER           PIC 9(6).
               05  FILLER             PIC XXX.
               05  D-DATE             PIC 99/99/99.
               05  FILLER             PIC XX.
               05  D-PROCEEDS         PIC Z(6).99-.
               05  FILLER             PIC X(1).
               05  D-SEQ              PIC Z(4).
               05  FILLER             PIC XXXX.
               05  D-SEL              PIC X.
               05  FILLER             PIC X.
           03  D-PAGENO               PIC ZZ9.
           03  D-END-MORE             PIC X(7).

       01  PAGE-KEY-TABLE.
           03  PAGE-KEY               PIC X(33) OCCURS 50 TIMES.

       01  CURRENT-PAGE               PIC S999.
       01  PAGE-COUNT                 PIC 999.
       01  MAX-PAGES                  PIC 9(4)  VALUE 50.
      *--------------------------------------------
      ***********************************
      *    MORE INFO DISPLAY BUFFER
      ***********************************
       01  MORE-BUF          VALUE SPACES.
           03  F-DLNO        PIC ZZZ9.99.
           03  F-DLNAME      PIC X(25).
           03  F-CLASS       PIC 99.
           03  F-CLDESC      PIC X(17).
           03  F-ACCTNO      PIC 9(6).
           03  F-TRDATE      PIC 99/99/99.
           03  F-SEQ         PIC ZZ9.
           03  F01           PIC X.
           03  F02           PIC XX.
           03  F03           PIC Z(4).
           03  F04           PIC Z,ZZZ,ZZ9.99-   BLANK ZERO.
           03  F05           PIC Z,ZZZ,ZZ9.99-   BLANK ZERO.
           03  F06           PIC Z,ZZZ,ZZ9.99-   BLANK ZERO.
           03  F07           PIC Z,ZZZ,ZZ9.99-   BLANK ZERO.
           03  F08           PIC Z,ZZZ,ZZ9.99-   BLANK ZERO.
           03  F09           PIC Z,ZZZ,ZZ9.99-   BLANK ZERO.
           03  F10           PIC Z,ZZZ,ZZ9.99-   BLANK ZERO.
           03  F11           PIC Z,ZZZ,ZZ9.99-   BLANK ZERO.
           03  F12           PIC Z,ZZZ,ZZ9.99-   BLANK ZERO.
           03  F13           PIC Z,ZZZ,ZZ9.99-   BLANK ZERO.
           03  F14           PIC Z,ZZZ,ZZ9.99-   BLANK ZERO.
           03  F15           PIC Z(6).
           03  F16           PIC ZZ/ZZ/ZZ.
           03  F17           PIC 9(10)           BLANK ZERO.
           03  F18           PIC 9(5)            BLANK ZERO.
           03  F19           PIC X.
           03  F20           PIC X.
           03  F21           PIC X.
           03  F22           PIC XX.
           03  F23           PIC 9               BLANK ZERO.
           03  F24           PIC ZZZ,ZZZ.99.
           03  F25           PIC ZZZ,ZZZ.99-     BLANK ZERO.
           03  F26           PIC XX.
           03  F27           PIC X.
           03  F28           PIC 9(4).

       01  MORE-LIST.
           03  FILLER        PIC X(44) VALUE
           "DLNO DLNAME CLASS CLSDESC ACCTNO TRDATE SEQ ".
           03  FILLER        PIC X(40) VALUE
           "010 020 030 040 050 060 070 080 090 100 ".
           03  FILLER        PIC X(40) VALUE
           "110 120 130 140 150 160 170 180 190 200 ".
           03  FILLER        PIC X(32) VALUE
           "210 220 230 240 250 260 270 280.".

      *--------------------------------------------

       COPY "LIBWI/PFSWINDOWSW.CPY".
       COPY "LIBWI/DTCKRL_CMD.CPY".
       COPY "LIBWI/DTCKR3W.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/PHON400.CPY".
       COPY "LIBGB/GPENVW.CPY".
       01  DTCKRL-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/FLDSELW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/DTCKRL_DEF.CPY".
       COPY "LIBWI/DTCKR2_DEF.CPY".
       COPY "LIBWI/DTCKRL_WKS.CPY".
       COPY "LIBWI/DTCKR2_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBWI/DTCKRLW.CPY".

       SCREEN SECTION.
       COPY "LIBWI/DTCKRL_SCN.CPY".
       COPY "LIBWI/DTCKR2_SCN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME DTCKRL-BUF EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE
              WK-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-DT3-FILE.
           PERFORM CLOSE-DT1-FILE.
           PERFORM CLOSE-DL1-FILE.
           CLOSE
              WK-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      ****************************
      *      MAIN PROGRAM MODULE
      ****************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".

       MAIN-CONT.
      D    STOP MESS.

           MOVE " " TO VDU-WIMENU-SELECTION.

           MOVE "DTCKRL" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   AT LINE DTCKRL-BEGIN-Y
                   AT COL  DTCKRL-BEGIN-X
                   SIZE    DTCKRL-COLUMNS + VDU-WIN-W-MOD
                   LINES   DTCKRL-LINES + VDU-WIN-H-MOD
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   BLANK SCREEN
                   HANDLE  IS DTCKRL-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           PERFORM GET-GPENV.

      * CREATE TEMPORARY WORK FILE TO HOLD THE AMOUNT OF  DEALER CHECKS:
           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           MOVE MKTEMP-BUF TO WK-PATH.
           PERFORM OPEN-WK-FILE-OUTPUT.
           PERFORM CLOSE-WK-FILE.
           PERFORM OPEN-WK-FILE.

           MOVE HEAD-PROMPT TO WR-BUF.
           MOVE PROMPT-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      * CLEAR RUNNING TOTAL OF CHECKS AND DISPLAY:
           MOVE 0 TO DTCKRLW-CHECKS
                     DTCKRL-NO-TRANS.
           PERFORM DISPLAY-TOTAL-OF-CHECKS.

           PERFORM INQUIRY-DISPLAY.
           IF IO-FG = 9 AND SUB = 1
              GO TO END-ROUTINE.

      * THIS GO TO SHOULD NEVER BE REACHED:
           GO TO MAIN-CONT.


      *************************************************
       INQUIRY-DISPLAY SECTION.
      *************************************************
           MOVE " " TO DT-STATUS.

      * GLB 11/07/01 WE SHOULD HAVE THE CORRECT DTCKRL-BEG-CLASS
      *     AT THIS POINT.
      *
      * BARB & CINDY 12/20/02  WE DON'T UNDERSTAND GAIL'S COMMENT
      * ABOVE....SHE STARRED THIS OFF, BUT LONPT0 JUST MOVES IN
      * WHATEVER YOU SELECT FOR CLASS RANGE, SO IF YOU ENTER 1
      * SAME BEG & ENDING ACCT# AND F3 ALL CLASSES IT WILL NOT
      * SHOW UP, BUT I DON'T KNOW WHY THIS GOT STARRED OFF &
      * I'M AFRAID TO CHANGE IT....
      *
      *    IF DTCKRL-BEG-LNNO = DTCKRL-END-LNNO
      *       MOVE DTCKRL-LN-DLNO TO DTCKRL-BEG-DLNO DT-DLNO
      *                              DTCKRL-END-DLNO
      *       MOVE DTCKRL-LN-CLASS TO DTCKRL-BEG-LC DT-CLASS
      *                               DTCKRL-END-LC
      *       MOVE DTCKRL-BEG-LNNO TO DT-NUMBER
      *       MOVE 0               TO DT-SEQNO
      *       MOVE 0 TO DT-DATE DT-SEQ
      *    ELSE
      *       MOVE DTCKRL-BEG-DLNO TO DT-DLNO
      *       MOVE 01 TO DTCKRL-BEG-LC DT-CLASS
      *       MOVE 99 TO DTCKRL-END-LC
      *       MOVE DTCKRL-BEG-LNNO TO DT-NUMBER
      *       MOVE 0               TO DT-SEQNO
      *       MOVE 0 TO DT-DATE DT-SEQ.

           MOVE FIRST-FIELD TO FLDSEL-FIELD.
           MOVE 1 TO PAGE-COUNT ONE.
           MOVE 0 TO CURRENT-PAGE.

           PERFORM OPEN-DT3-FILE.
           MOVE DTCKRL-BRANCH      TO DT-BRNO.
           MOVE DTCKRL-BEG-DLNO    TO DT-DLNO.
           MOVE DTCKRL-BEG-CLASS   TO DT-CLASS.
           MOVE DTCKRL-BEG-LNNO    TO DT-ACCTNO.
           MOVE 0                  TO DT-DATE DT-SEQ.
           MOVE DT3-KEY TO PAGE-KEY(PAGE-COUNT).

       START-BUILD-PAGENO.
           ADD ONE TO CURRENT-PAGE.
           IF CURRENT-PAGE < 1
              MOVE FIRST-FIELD TO FLDSEL-FIELD
              MOVE 1 TO CURRENT-PAGE.
           IF CURRENT-PAGE > PAGE-COUNT
              MOVE FIRST-FIELD TO FLDSEL-FIELD
              MOVE 1 TO CURRENT-PAGE.

       CONT-BUILD-PAGENO.
           MOVE SPACES TO DISPLAY-BUF.
           MOVE 1 TO SUB.
           MOVE PAGE-KEY(CURRENT-PAGE) TO DT3-KEY.

           PERFORM START-DT3-FILE.

       BUILD-PAGENO.
           PERFORM READ-DT3-FILE-NEXT.
      *    UNLOCK DT-FILE.
           IF (IO-FG NOT = 0)
              GO TO DISPLAY-PAGE.

      * CHANGED TO MAKE READ-NEXT STOP WHEN AT END OF
      * DT-STATUS = " ", UNRELEASED:
           IF DT-STATUS NOT = " "
              MOVE 9 TO IO-FG
              GO TO DISPLAY-PAGE.

      * FILE WAS STARTED ON BEGINNING DLNO, SO IF PAST ENDING, EXIT:
           IF DT-DLNO > DTCKRL-END-DLNO
              MOVE 9 TO IO-FG
              GO TO DISPLAY-PAGE.

           IF DT-CLASS > DTCKRL-END-CLASS
              MOVE 9 TO IO-FG
              GO TO DISPLAY-PAGE.

           IF DTCKRL-BEG-LNNO = DTCKRL-END-LNNO
              IF DT-ACCTNO < DTCKRL-BEG-LNNO
                      OR DT-ACCTNO > DTCKRL-END-LNNO
                 MOVE 9 TO IO-FG
                 GO TO DISPLAY-PAGE
              ELSE
                 NEXT SENTENCE
           ELSE
              IF DT-ACCTNO < DTCKRL-BEG-LNNO
                      OR DT-ACCTNO > DTCKRL-END-LNNO
                 GO TO BUILD-PAGENO.

      * KEY IS BY STATUS, KEEP LOOKING
           IF (DT-BRNO NOT = DTCKRL-BRANCH)  
               GO TO BUILD-PAGENO.

           MOVE DT-PROCEEDS   TO HOLD-DUE.
           IF DT-REVERSE = "Y"
              MOVE 0 TO HOLD-DUE.
           COMPUTE HOLD-DUE = HOLD-DUE - DT-PAYMENTS.
           IF HOLD-DUE = 0
              GO TO BUILD-PAGENO.


           IF DTCKRL-NEW-DEALS = "O"
              IF DT-TRCD NOT = "NW"
                 IF DT-REVERSE-TRCD NOT = "NW"
                    GO TO BUILD-PAGENO.
           IF DTCKRL-NEW-DEALS = "S"
              IF DT-TRCD = "NW" OR DT-REVERSE-TRCD = "NW"
                 GO TO BUILD-PAGENO.

           IF DTCKRL-WS-TYPE = "P"
              IF DT-TYPE NOT = "P"
                 GO TO BUILD-PAGENO.
           IF DTCKRL-WS-TYPE = "S"
              IF DT-TYPE NOT = "S"
                 GO TO BUILD-PAGENO.

           IF DT-DATE < DTCKRL-BEG-DATE
              GO TO BUILD-PAGENO.

           IF DT-DATE > DTCKRL-END-DATE
              GO TO BUILD-PAGENO.

           IF SUB > 15
              GO TO DISPLAY-PAGE.

           MOVE DT-DATE     TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-DATE(SUB).
           MOVE DT-TRCD     TO D-TRCD(SUB).
           MOVE DT-DLNO     TO D-DLNO(SUB).
           MOVE DT-CLASS    TO D-CLASS(SUB).
           MOVE "-"         TO D-DASH(SUB).
           MOVE DT-ACCTNO   TO D-NUMBER(SUB).
           MOVE DT-PROCEEDS TO D-PROCEEDS(SUB).
           MOVE DT-SEQ      TO D-SEQ(SUB).
           IF DT-SELECTED = "R"
              MOVE " "      TO D-SEL(SUB)
           ELSE
              MOVE DT-SELECTED TO D-SEL(SUB).

           ADD 1 TO SUB.
           GO TO BUILD-PAGENO.

       DISPLAY-PAGE.
           IF IO-FG NOT = 0
              IF SUB = 1
                 MOVE "NO RECORDS FOUND!" TO MESS
                 PERFORM SEND-MESS
                 MOVE DISPLAY-BUF TO WR-BUF
                 MOVE DISPLAY-LIST TO WR-LIST
                 PERFORM CALL-WRFORM
                 MOVE "Y" TO DTCKRL-EOF
                 GO TO DTCKRL-END-INQUIRY-MODULE.

           MOVE "MORE..." TO D-END-MORE.
           IF CURRENT-PAGE = PAGE-COUNT
              IF IO-FG = 0
                 IF PAGE-COUNT < MAX-PAGES
                    ADD 1 TO PAGE-COUNT
                    MOVE DT3-KEY TO PAGE-KEY(PAGE-COUNT)
                 ELSE
                    MOVE "MAXIMUM PAGES REACHED!" TO MESS
                    PERFORM SEND-LEGEND
                    MOVE "OVRFLOW" TO D-END-MORE
              ELSE
                 MOVE "* END *" TO D-END-MORE.

           MOVE CURRENT-PAGE TO D-PAGENO.
           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       SELECT-FIELD-FROM-DISPLAY.
           MOVE LEGEND-01 TO MESS.
           PERFORM SEND-LEGEND.

           PERFORM CALL-FLDSEL.

      * TEST UP ARROW:
           IF FLDSEL-STAT = "1U"
              IF CURRENT-PAGE = 1
                 PERFORM ALARM
                 GO TO SELECT-FIELD-FROM-DISPLAY
              ELSE
                 MOVE -1 TO ONE
                 MOVE LAST-FIELD TO FLDSEL-FIELD
                 GO TO START-BUILD-PAGENO.

      * TEST PAGE UP:
           IF FLDSEL-STAT = "1P"
              IF CURRENT-PAGE = 1
                 PERFORM ALARM
                 GO TO SELECT-FIELD-FROM-DISPLAY
              ELSE
                 MOVE -1 TO ONE
                 MOVE FIRST-FIELD TO FLDSEL-FIELD
                 GO TO START-BUILD-PAGENO.

      * TEST PAGE DOWN OR DOWN ARROW:
           IF FLDSEL-STAT = "1N" OR FLDSEL-STAT = "1D"
              IF CURRENT-PAGE NOT < PAGE-COUNT
                 PERFORM ALARM
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

      * SELECT THIS CHECK
           IF FLDSEL-STAT = "16"
              PERFORM SELECT-THIS-CHECK
              GO TO CONT-BUILD-PAGENO.

           IF FLDSEL-STAT = "1>"
              IF DTCKRL-NO-TRANS = 0
                      AND DTCKRLW-CHECKS = 0
                 MOVE "Y" TO DTCKRL-EOF
                 GO TO END-ROUTINE
              ELSE
                 MOVE " ------ CONFIRM RELEASE BY TYPING 'YES':"
                                                            TO MESS
                 PERFORM SEND-LEGEND
                 PERFORM ALARM
                 MOVE "E  A030CONTINUE." TO VDU
                 PERFORM SCREENIO
                 IF VDUBUF = "YES"
                    GO TO END-ROUTINE.

           IF NOT (FLDSEL-STAT = "00" OR "1:" OR "1<")
              PERFORM ALARM
              GO TO SELECT-FIELD-FROM-DISPLAY.
           IF FLDSEL-STAT = "1:"
              PERFORM DTCKR3-DISPLAY.
           IF FLDSEL-STAT = "1<"
              PERFORM MORE-INFO.

           GO TO SELECT-FIELD-FROM-DISPLAY.

       DTCKRL-END-INQUIRY-MODULE.
           EXIT.

      **************************************************
       SELECT-THIS-CHECK SECTION.
      **************************************************
           MOVE DTCKRL-BRANCH TO DT-BRNO.
           MOVE FLDSEL-DLNO   TO DT-DLNO.
           MOVE FLDSEL-CLASS  TO DT-CLASS.
           MOVE FLDSEL-NUMBER TO DT-ACCTNO.
           MOVE FLDSEL-DATE   TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO DT-DATE.
           MOVE FLDSEL-SEQ    TO DT-SEQ.
           PERFORM READ-DT1-FILE.
           IF IO-FG NOT = 0
      *       UNLOCK DT-FILE
              MOVE "UNABLE TO SELECT CHECK" TO MESS
              PERFORM SEND-MESS
              GO TO SELECT-THIS-CHECK-EXIT.

      * 1/15/02 IF GLOBAL OPTION FOR DIRECT DEPOSIT IS TURNED ON,
      *         CHECK DEALER, DON'T ALLOW HERE IF HE'S DIRECT DEPOSIT:

           IF GP-DL-ACH-FG = "Y"
              MOVE DT-DLNO TO DL-DLNO
              PERFORM GET-DL
              IF DL-ACH-FG = "Y"
                  MOVE "CAN'T RELEASE HERE; DEALER IS DIRECT DEPOSIT"
                     TO MESS
                  PERFORM SEND-MESS
                  GO TO SELECT-THIS-CHECK-EXIT.

           IF DT-SELECTED = " " OR "R"
              MOVE "Y" TO DT-SELECTED
           ELSE
              IF DT-SELECTED = "Y"
                 MOVE " " TO DT-SELECTED.
           PERFORM REWRITE-DT1-FILE.

           SUBTRACT DT-PAYMENTS FROM DT-PROCEEDS GIVING DETAIL-UNPAID.

           PERFORM UPDATE-WK-FILE.

           PERFORM DISPLAY-TOTAL-OF-CHECKS.

       SELECT-THIS-CHECK-EXIT.
           EXIT.

      *************************************
       MORE-INFO SECTION.
      *************************************

           MOVE 22 TO WINDOW-LINES.
           MOVE 70 TO WINDOW-COLUMNS.
           MOVE 2 TO WINDOW-BEGIN-Y.
           MOVE 6 TO WINDOW-BEGIN-X.
           MOVE "WI/DTCKR2" TO WINDOW-FORM-NAM.
           MOVE "DTCKR2" TO VDU-FORM-NAME.

           PERFORM OPEN-WINDOW.
           DISPLAY DTCKR2-SCREEN.

           MOVE DTCKR2-FORM-FIELDS TO WR-BUF.
           MOVE DTCKR2-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE DTCKRL-BRANCH TO DT-BRNO.
           MOVE FLDSEL-DLNO   TO DT-DLNO.
           MOVE FLDSEL-CLASS  TO DT-CLASS.
           MOVE FLDSEL-NUMBER TO DT-ACCTNO.
           MOVE FLDSEL-DATE   TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO DT-DATE.
           MOVE FLDSEL-SEQ    TO DT-SEQ.
           PERFORM READ-DT1-FILE.
           IF IO-FG NOT = 0
      *       UNLOCK DT-FILE
              MOVE "UNABLE TO DISPLAY TRANSACTION" TO MESS
              PERFORM SEND-MESS
              GO TO MORE-INFO-CLOSE-WIN.
      *    UNLOCK DT-FILE.

           MOVE DT-DLNO TO F-DLNO
                           DL-DLNO.
           PERFORM GET-DL.

           MOVE DL-NAME   TO F-DLNAME.
           MOVE DT-CLASS  TO F-CLASS.
           MOVE SPACES    TO F-CLDESC.
           MOVE DT-ACCTNO TO F-ACCTNO.
           MOVE DT-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO F-TRDATE.
           MOVE DT-SEQ      TO F-SEQ.

           MOVE DT-TYPE        TO F01.
           MOVE DT-TRCD        TO F02.
           MOVE DT-LPSEQNO     TO F03.
           MOVE DT-DLRVPRIN(1) TO F04.
           MOVE DT-DLRVPRIN(2) TO F05.
           MOVE DT-DLRVPRIN(3) TO F06.
           MOVE DT-DLRVCHGS(1) TO F07.
           MOVE DT-DLRVCHGS(2) TO F08.
           MOVE DT-DLRVCHGS(3) TO F09.
           MOVE DT-DLPARVHELD  TO F10.
           MOVE DT-DLPARVPAID  TO F11.
           MOVE DT-PROCEEDS    TO F12.
           MOVE DT-RELEASED    TO F13.
           MOVE DT-PAYMENTS    TO F14.
           MOVE DT-CKNO        TO F15.
           MOVE DT-CKDATE      TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY    TO F16.
           MOVE DT-BATCHID     TO F17.
           MOVE DT-BATCHID-SEQ TO F18.
           MOVE DT-PAID        TO F19.
           MOVE DT-STATUS      TO F20.
           MOVE DT-REVERSE     TO F21.
           MOVE DT-REVERSE-TRCD TO F22.
           MOVE DT-DLRVNO      TO F23.
           MOVE DT-PURBAL      TO F24.
           MOVE DT-APCUR       TO F25.
           MOVE DT-P4P5-TRCD   TO F26.
           MOVE DT-SELECTED    TO F27.
           MOVE DT-BRNO        TO F28.

           MOVE MORE-BUF TO WR-BUF.
           MOVE MORE-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE "E  A010CONTINUE." TO VDU.
           PERFORM SCREENIO.

       MORE-INFO-CLOSE-WIN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY DTCKR2-SCREEN.
           PERFORM CLOSE-WINDOW.
           MOVE "DTCKRL" TO VDU-FORM-NAME.

       MORE-INFO-EXIT.
           EXIT.

      *************************************
       DTCKR3-DISPLAY SECTION.
      *************************************

      * DISPLAY OF DEALER CHECKS TO BE WRITTEN

           MOVE MKTEMP-BUF TO DTCKR3-WK-PATH.
           MOVE "WI/DTCKR3 " TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH DTCKR3-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.

       DTCKR3-DISPLAY-EXIT.
           EXIT.

      ********************************************
       UPDATE-WK-FILE SECTION.
      ********************************************

      * CREATE / UPDATE WORK FILE CHECK TOTALS

       FIND-WK-RECORD.
           IF DTCKRL-CHECK-TO-DLBR = "Y"
              MOVE DT-DLNO TO  WK-DLNO
           ELSE
              MOVE DT-DL TO WK-DL
              MOVE 0     TO WK-DLSUB.
           MOVE DT-CLASS TO WK-CLASS.
           PERFORM READ-WK-FILE.
           IF IO-FG = 9
              IF DTCKRL-CHECK-TO-DLBR = "Y"
                 MOVE DT-DLNO TO  WK-DLNO
              ELSE
                 MOVE DT-DL TO WK-DL
                 MOVE 0     TO WK-DLSUB
              END-IF
              MOVE DT-CLASS TO WK-CLASS
              MOVE 0 TO WK-NO-TRANS WK-UNPAID WK-LAST-ACCTNO
                        WK-DLBR
              PERFORM WRITE-WK-FILE
              GO TO FIND-WK-RECORD.

           IF DT-SELECTED = "Y"
              ADD DETAIL-UNPAID TO WK-UNPAID
                                   DTCKRLW-CHECKS
              ADD 1 TO WK-NO-TRANS
                       DTCKRL-NO-TRANS
           ELSE
              IF DT-SELECTED = " "
                 SUBTRACT DETAIL-UNPAID FROM WK-UNPAID
                                             DTCKRLW-CHECKS
                 SUBTRACT 1 FROM WK-NO-TRANS
                                 DTCKRL-NO-TRANS.

           IF WK-UNPAID = 0 AND WK-NO-TRANS = 0
              PERFORM DELETE-WK-FILE
           ELSE
              MOVE DT-DLBR    TO WK-DLBR
              MOVE DT-ACCTNO  TO WK-LAST-ACCTNO
              PERFORM REWRITE-WK-FILE.

       EXIT-UPDATE-WK.
           EXIT.

       COPY "LIBWI/PFSWINDOWS.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/FLDSEL.CPY".

      *************************************************
       FORM-RESET SECTION.
      *************************************************
           DISPLAY DTCKRL-SCREEN UPON DTCKRL-WINDOW-HANDLE.
           MOVE DTCKRL-FORM-FIELDS TO WR-BUF.
           MOVE DTCKRL-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       FORM-RESET-EXIT.
           EXIT.

      *************************************************
       SETUP-LINK-INFO SECTION.
      *************************************************
           IF VDU-FORM-NAME = "DTCKR2"
              MOVE DTCKR2-HELP--FILE TO HELP-LINK-FILE
              MOVE DTCKR2-HELP--DIR  TO HELP-LINK-DIR
           ELSE
              MOVE DTCKRL-HELP--FILE TO HELP-LINK-FILE
              MOVE DTCKRL-HELP--DIR  TO HELP-LINK-DIR.

      *************************************************
       VDU-CONVERT-FIELD SECTION.
      *************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/DTCKRL_EVA.CPY".
              COPY "LIBWI/DTCKR2_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *************************************************
       MISC-PARAGRAPHS SECTION.
      *************************************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       DISPLAY-TOTAL-OF-CHECKS.
           MOVE "SNNS060NOTRANS." TO VDU.
           MOVE DTCKRL-NO-TRANS TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "S  S092CHECKS." TO VDU.
           MOVE DTCKRLW-CHECKS TO VDUNUM2.
           PERFORM SCREENIO.
       SEND-LEGEND.
           MOVE MESS TO WR-BUF.
           MOVE MSEL-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       GET-DL.
           PERFORM OPEN-DL1-FILE.
           PERFORM READ-DL1-FILE.
           PERFORM CLOSE-DL1-FILE.
           IF IO-FG = 9
              MOVE "***** NOT ON FILE *****" TO DL-NAME.

      **********************************************
       IO-ROUTINES SECTION.
      **********************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPDTGS_SQL.CPY".
       COPY "LIBLP/LPDLGS_SQL.CPY".
       COPY "LIBLP/LPDT1IN.CPY".
       COPY "LIBLP/LPDT3IN.CPY".
       COPY "LIBLP/LPDL1RN.CPY".
       COPY "LIBGB/GBWKIN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      **********************************************
       END-ROUTINE SECTION.
      **********************************************
           PERFORM CLOSE-FILES.

           MOVE WK-PATH TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.

       EXIT-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY DTCKRL-SCREEN.
           DESTROY DTCKRL-WINDOW-HANDLE.
       STOP-RUN.
           EXIT PROGRAM.
