       IDENTIFICATION  DIVISION.
       PROGRAM-ID.       GQSCAN.
       AUTHOR.              MJD.
       DATE-WRITTEN. 2000/11/30.
      ******************************************************************
      *
      *           GLOBAL COLLECTOR QUEUE FILE WINDOW SCAN
      *                  CALLED FROM SPINQ
      *
      * REV:
      *  JTG 012596 CHANGED MORE WINDOW TO ALLOW F1-TOGGLE BETWEEN
      *             WORKED AND UNWORKED
      *  BLL 031296 CHANGED MORE WINDOW TO DISPLAY DEL, BAL; SEQ OUT
      *  BLL 032196 DON'T GO TO IO-ERROR IF NO GQ HEADER; SEND MESSAGE
      *  KEC 041096 ADDED OPTION TO CHANGE COLLECTOR QUEUE.
      *  KEC 041096 ADDED OPTION TO TOGGLING THE TYPE OF ACCOUNTS:
      *             (WORKED/UNWORKED/RESCHEDULED).  ALSO DISPLAYED THE
      *             THE RESCHEDULED DATE. (ALL IN THE SECONDARY WINDOW).
      *  KEC 042696 FIXED THE GQ-DATE CONVERSION (LIBSP/SPSETGQ).
      *  BLV 110596 ALLOW SELECTION OF STARTING DELINQUENCY IN MORE-INFO
      *  BLV 031397 DISPLAY RESULT CODE ON MORE WINDOW IF WORKED
      *  BLV 041497 CHECK ORDER OF DELINQUENCY USING BR-COL-QUEUE-LOW-TO-HI
      *  BLV 063097 COMPUTE CURRENT DELINQUENCY INSTEAD OF USING GQ
      *  BLV 070297 IF BROKEN PROMISES SELECTED AS FIRST IN QUEUE,
      *             DISPLAY "PRIORITY" & PROMISED DATE IN SCAN
      *  BLV 082697 ALL ACCTS SHOWING AS MATURED - SYSDATE WAS 9(6)
      *             INSTEAD OF X(8) - LPSTAT DISPLAY EXPECTED X(8)
      *  CLS 092397 ADDED DISPLAY OF LN-RECODE TO BROKEN PROMISE SCAN
      *  GLF 101697 ADDED COPY MEMBER: LIBLP/LPPDUE LIBLP/LPPDUEW
      *  BLV 000911 IN DETAIL INQUIRY, IF NOT A RESCHEDULE & USING TIME
      *             ZONE QUEUEING, DISPLAY CURRENT CUST TIME, TFC PR# 25
      *  *** ****** ****************************************************
      *  MJD 001130 COPIED OVER FROM WI/CQSCAN AND CONVERTED FOR GLOBAL
      *             (TFC PR#60)
      *  BLV 010105 ADDED BRANCH FILE & SETTING OF LOCAL PATHS, NO
      *             INFO WAS APPEARING ON LINES UNLESS ACCOUNT WAS IN
      *             FIL BRANCH
      *  BLV 010207 FIX LOOP IF NO DISPLAYABLE RECORDS OF ANY TYPE FOR
      *             A COLLECTOR
      *  BLV 010207 PUT IN CHANGES FOR TFC PR# 87, DON'T DISPLAY ANY
      *             ACCTS ON F6 MORE INFO IF THEY DON'T FALL WITHIN
      *             LEGAL HOURS, QUOTE ONLY, UNSTAR IF APPROVED
      *  BAH 160128 SPLIT OUT LTFILE, PD0003
      *
      * KEC 2017.0109 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          CHANGED GQSCAN-NUMBER TO GQSCAN-ACCTNO.
      *          ALSO REMOVED GQSCAN-SEQ.
      *  BAH 170418 ACTIVATE 8 DIGIT DATES, PD0006
      *   CS 190315 GQFILE SPLIT INTO 2 FILES, DETAIL RECORDS IN GQFILE,
      *             SUMMARY(HEADER) RECORDS GQSFILE
      *            BROKE OUT GQ-FILE TO DETAIL (GQ) AND SUMMARY(GQS)
      *            REPLACED PAGE-KEY WITH PKGQ2-KEY-TABLE, ALSO SEE
      *            SPWSGQ, NEEDED A VERSION OF KEY WITH NON-COMPED FIELDS
      *            TO POPULATE E-KEY (START-TIME IS COMPED, NO ONE KNOWS
      *            WHY. USE OF SET-PKGQ2-TO-CQ AND SET-GQ2-TO-PKGQ2
      *  190327 CS GQS-BRNO IS ALWAYS POPULATED WITH ZERO, PER KEN REMOVE
      *            FROM FILE LAYOUT
      *  20210506 CS REMOVE PKGQ2-FUTURE-KEY1,KEY2
      *
      * SMR 2024-0208 ADDED VGQFILE LOGIC TO RESOLVE DIFFICULTY WITH
      *          THE GQ2-KEY WORKING WITH A SCAN WINDOW AND/OR USING A
      *          START-GREATER IO PARAGRAPH.  USING A TEMPORARY VISION
      *          GQFILE HELPS KEEP THE SAME EXPECT RESULTS FROM A15.
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       COPY "LIBSP/SPFSVGQ.CPY".

       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.

       COPY "LIBSP/SPFDVGQ.CPY".
      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E - S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)WI/GQSCAN S35 03/07/24-13:13:20 >".

      ******************************************************************
      *
      *    I F N - W O R K E R S
      *
      ******************************************************************
       COPY "LIBSP/SP01GQ.CPY".
       COPY "LIBSP/SP01GQ_SQL.CPY".
       COPY "LIBSP/SPWSGQ.CPY".
       COPY "LIBSP/SPWSVGQ.CPY".
       COPY "LIBSP/SP01GQS.CPY".
       COPY "LIBSP/SP01GQS_SQL.CPY".
       COPY "LIBSP/SPWSGQS.CPY".
       COPY "LIBLP/LP01CDB.CPY".
       COPY "LIBLP/LP01CDB_SQL.CPY".
       COPY "LIBLP/LPWSCDB.CPY".
       COPY "LIBLP/LP01LTP.CPY".
       COPY "LIBLP/LP01LTP_SQL.CPY".
       COPY "LIBLP/LPWSLTP.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
      *-----------------------------------------------------------------
       01  GQSCAN-LINES               PIC 9(02)  VALUE 11.
       01  GQSCAN-COLUMNS             PIC 9(02)  VALUE 44.
       01  GQSCAN-BEGIN-Y             PIC 9(02)  VALUE 1.
       01  GQSCAN-BEGIN-X             PIC 9(02)  VALUE 1.
       01  ERRCD                   PIC X(1) VALUE SPACES.
       01  SCANTYPE                PIC X      VALUE "U".
       01  ONE                     PIC S9     VALUE 1.
       01  SUB                     PIC 99.
       01  WK-START-TIME           PIC 9(4).
       01  WK-START-TIMEX          REDEFINES WK-START-TIME.
           03  WK-START-HH         PIC 99.
           03  WK-START-MM         PIC 99.
       01  BAL-WK                  PIC 9(6).

       01  A-NAME                  PIC X(16).
       01  HOLD-DELINQUENCY        PIC 9(5) VALUE 0.
       01  HOLD-BRNO               PIC 9(4)      VALUE 0.
       01  RECORDS-FOUND           PIC X         VALUE "N".

      *=================================================================
      *
      *    W S - F I E L D S
      *
      *=================================================================
       01  WS-CHANGE-COLLID-FG     PIC X(1)           VALUE "N".
       01  WS-GQSCAN-COLLID        PIC X(3)           VALUE SPACES.
       01  WS-VDUSTATUS            PIC X(2)           VALUE SPACES.

      *=================================================================
      *
      *    D I S P L A Y - C Q S C A N
      *
      *=================================================================
       01  DISPLAY-GQSCAN-LIST.
           03  FILLER              PIC X(34)  VALUE
           "COLLID-NAME:R 010 020 030 040 050.".

      *-----------------------------------------------------------------
       01  DISPLAY-GQSCAN-BUF.
           03  D-COLLID-NAME       PIC X(29).
           03  D-GQ-TOT-REC        PIC ZZZZ9.
           03  D-GQ-TO-WORK        PIC ZZZZ9.
           03  D-GQ-RE-SCH         PIC ZZZZ9.
           03  D-GQ-WORKED         PIC ZZZZ9.
           03  D-GQ-NEXT-SEQNO     PIC ZZZZ9.

      *=================================================================
      *
      *    F L D S E L
      *
      *=================================================================
       01  FLDSEL-STAT             PIC XX.
       01  FLDSEL-FIELD            PIC X(4)   VALUE "010.".

      *-----------------------------------------------------------------
       01  FLDSEL-LIST.
           03  FILLER              PIC X(36)  VALUE
           "010 020 030 040 050 060 070 080 090.".

      *-----------------------------------------------------------------
       01  FLDSEL-BUF.
           03  FLDSEL-BRNO          PIC ZZZ9.
           03  FILLER               PIC X(01).
           03  FLDSEL-ACCTNO        PIC X(13).
           03  FILLER               REDEFINES FLDSEL-ACCTNO.
               05  FILLER           PIC X(3).
               05  FLDSEL-LN-ACCTNO PIC 9(6).
               05  FILLER           PIC X(4).
           03  FILLER               PIC X.
           03  FLDSEL-NAME          PIC X(17).
           03  FLDSEL-CON-DEL       PIC ZZZ99.
           03  FILLER               PIC X.
           03  FLDSEL-BAL           PIC ZZZ,ZZ9.
           03  FILLER               PIC XX.
           03  FLDSEL-HH            PIC 99.
           03  FLDSEL-COLON         PIC X.
           03  FLDSEL-MM            PIC 99.
           03  FILLER               PIC X.
           03  FLDSEL-AM-PM         PIC XX.
           03  FILLER               PIC X(2).
           03  FLDSEL-RESCH-DATE    PIC 99/99/99.
           03  FILLER               PIC X(4).
           03  FLDSEL-WORK-COLLID   PIC XXX.

      *-----------------------------------------------------------------
       01  FIRST-FIELD             PIC X(4)  VALUE "010.".
       01  LAST-FIELD              PIC X(4)  VALUE "090.".

      *=================================================================
      *
      *    D I S P L A Y
      *
      *=================================================================
       01  DATE-PROMPT-LIST.
           03  FILLER              PIC X(28)  VALUE
           "DATEPROMPT1:S DATEPROMPT2:S.".

       01  DATE-PROMPT-BUF.
           03  DATE-PROMPT1        PIC X(13)  VALUE
           "RE-SCHEDULED:".
           03  DATE-PROMPT2        PIC X(13)  VALUE
           "DATE     TIME".

       01  GQSCAN-WINDOW-HANDLE    PIC X(10).
       01  DISPLAY-LIST.
           03  FILLER              PIC X(51)  VALUE
           "010 020 030 040 050 060 070 080 090 PGNO ENDMORE:F.".

      *-----------------------------------------------------------------
       01  DISPLAY-BUF.
           03  D-LINE              OCCURS 9 TIMES.
               05  D-BRNO          PIC ZZZ9.
               05  FILLER          PIC X(01).
               05  D-LN-ACCTNO     PIC X(13).
               05  FILLER          PIC X.
               05  D-NAME          PIC X(15).
               05  D-CON-DEL       PIC ZZZ99.
               05  FILLER REDEFINES D-CON-DEL.
                   07  FILLER      PIC XX.
                   07  D-CON-DELX  PIC XXX.
               05  D-BAL           PIC ZZZ,ZZ9-.
               05  FILLER          PIC X(3).
               05  D-RESCH-DATE    PIC 99/99/99 BLANK WHEN ZEROES.
               05  FILLER          PIC X.
               05  D-TIME.
                   07  D-HH            PIC Z9.
                   07  D-COLON         PIC X.
                   07  D-MM            PIC 99.
                   07  FILLER          PIC X.
                   07  D-AM-PM         PIC XX.
               05  FILLER          PIC X.
               05  D-WORK-COLLID   PIC XXX.
               05  FILLER          PIC X(1).
               05  D-RESULT        PIC XXX.
           03  D-PAGENO            PIC ZZ9.
           03  D-END-MORE          PIC X(7).

      *-----------------------------------------------------------------
       01  CURRENT-PAGE            PIC S999.
       01  PAGE-COUNT              PIC 999.
       01  MAX-PAGES               PIC 999    VALUE 100.


      *-----------------------------------------------------------------
      *    A32 GQ2-KEY VERSION OF PAGE-KEY                1/31/2019
      *-----------------------------------------------------------------
       01 PKGQ2-KEY-TABLE                     OCCURS 100 TIMES.
           03  PKGQ2-GQ2-KEY                   PIC X(59).
           03  FILLER               REDEFINES PKGQ2-GQ2-KEY.
               05  PKGQ2-COLLID             PIC XXX.
               05  PKGQ2-REV-BEHAVIOR-RISK  PIC 9.
               05  PKGQ2-SEQ-FG             PIC X.
               05  PKGQ2-DATE               PIC 9(8).
               05  PKGQ2-START-TIME         PIC S9(4)   COMP-3.
               05  PKGQ2-SEQ-FIELDS.
                   07  PKGQ2-REV-DEL        PIC 9(5).
                   07  PKGQ2-MISC-KEY-FLD   PIC X(3).
                   07  PKGQ2-REV-PDUE-DAYS  PIC 9(5).
                   07  PKGQ2-REV-BAL        PIC 9(7).
                   07  PKGQ2-SEQ            PIC 9(3).  


      *-----------------------------------------------------------------
       01  PAGE-ACCT-01.
           03  PAGE-ACCT-TAB       OCCURS 100 TIMES.
               05  PAGE-ACCTNO     PIC 9(8) COMP-3.
       01  LEGEND-WK               PIC X(76) VALUE SPACES.
       01  MORE-PROMPT.
           03  FILLER              PIC X(44) VALUE
           "SELECT LOAN,F1-TOGGLE(BROKEN/UNWORKED/WORKED".
           03  FILLER              PIC X(32) VALUE
           "/RESCHED),F6-DELINQUENCY,F7-EXIT".


      *-----------------------------------------------------------------
      *01  MORE-LIST.
      *    03  FILLER     PIC X(39)  VALUE
      *    "TITLE ADR1 ADR2 ADR3 PHONE EMPL SPOUSE ".
      *    03  FILLER     PIC X(16)  VALUE
      *    "LIA OPEN CLOSED.".
      *01  MORE-BUF.
      *    03  M-NAME    PIC X(30).
      *    03  M-ADR1    PIC X(30).
      *    03  M-ADR2    PIC X(30).
      *    03  M-ADR3    PIC X(30).
      *    03  M-PHONE   PIC X(14).
      *    03  M-EMPL    PIC X(20).
      *    03  M-SPOUSE  PIC X(28).
      *    03  M-LIA     PIC Z,ZZZ,ZZ9.99.
      *    03  M-OPEN    PIC ZZ.
      *    03  M-CLOSED  PIC ZZ.
      *-----------------------------------------------------------------
      ******************************************************************
      *
      *     C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBWI/PFSWINDOWSW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/GETDLSW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBWI/GQSCN3W.CPY".
       COPY "LIBGB/AGEINGW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBLP/LPCEPPW.CPY".
       COPY "LIBLP/LPUPWKW.CPY".
       COPY "LIBGB/SPPDTHW.CPY".
       COPY "LIBGB/FLDSELW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/GQSCAN_DEF.CPY".
       COPY "LIBWI/GQSCN2_DEF.CPY".
       COPY "LIBWI/GQSCAN_WKS.CPY".
       COPY "LIBWI/GQSCN2_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBLP/LPPDUEW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBLP/LPSTATW.CPY".
      *-----------------------------------------------------------------
       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBWI/GQSCANW.CPY".

       SCREEN SECTION.
       COPY "LIBWI/GQSCAN_SCN.CPY".
       COPY "LIBWI/GQSCN2_SCN.CPY".

      ******************************************************************
      *
      *     P R O C E D U R E - D I V I S I O N
      *
      ******************************************************************
       PROCEDURE DIVISION
           USING FORM-PATHNAME GQSCAN-BUF EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
                VGQ-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-GQS1-FILE.
           PERFORM CLOSE-GQ2-FILE.
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LTP1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-CDB1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      ******************************************************************
      *
      *    M A I N - P R O G R A M - M O D U L E
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".

           PERFORM GET-GPENV.

           PERFORM OPEN-SP1-FILE.

      D    STOP GQSCAN-VGQ-PATH.
           MOVE "XX"          TO GQSCAN-STATUS.
           MOVE " "           TO VDU-WIMENU-SELECTION.

           MOVE GQSCAN-VGQ-PATH TO VGQ-PATH.
           MOVE "GQSCAN" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   AT LINE GQSCAN-BEGIN-Y
                   AT COL  GQSCAN-BEGIN-X
                   SIZE    GQSCAN-COLUMNS + VDU-WIN-W-MOD
                   LINES   GQSCAN-LINES + VDU-WIN-H-MOD
           COLOR IS VDU-WCOLOR-SCAN,CONTROL VALUE IS VDU-WCTRL-SCAN
                   CELL SIZE IS LABEL FONT
                   BLANK SCREEN
                   HANDLE  IS GQSCAN-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           MOVE GQSCAN-COLLID TO WS-GQSCAN-COLLID.

       ACCESS-COLLID.

           PERFORM OPEN-GQS1-FILE.

           MOVE GQSCAN-COLLID TO GQS-COLLID.
           PERFORM READ-GQS1-FILE.
           PERFORM CLOSE-GQS1-FILE.

           IF ( IO-FG = 9 )
              PERFORM INVALID-COLLID
              IF ( GQSCN3-STATUS = "1>" )
                 MOVE "XX" TO GQSCAN-STATUS
                 GO TO END-ROUTINE.

           MOVE SPACES        TO MESS.

           MOVE "CN"          TO CDB-TYPE.
           MOVE ZEROS         TO CDB-BRANCH.
           MOVE GQSCAN-COLLID TO CDB-CODE.
           PERFORM GET-CD1-FILE.
           IF ( IO-FG = 9 )
              STRING " ",GQSCAN-COLLID," - ","< NOT ON FILE >"
                     DELIMITED BY "  " INTO MESS
           ELSE
              STRING " ",GQSCAN-COLLID," - ",CDB-DESC
                     DELIMITED BY "  " INTO MESS.

           MOVE MESS       TO D-COLLID-NAME.
           MOVE GQS-TOT-REC TO D-GQ-TOT-REC.

           SUBTRACT GQS-WORKED-REC FROM GQS-TOT-REC
                                  GIVING D-GQ-TO-WORK.

           MOVE GQS-TIMED-REC TO D-GQ-RE-SCH.
           MOVE GQS-WORKED-REC TO D-GQ-WORKED.

           MOVE DISPLAY-GQSCAN-BUF TO WR-BUF.
           MOVE DISPLAY-GQSCAN-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE "9" TO FLDSEL-STAT.

       ENTER-SEL.

           MOVE "E  A010SEL." TO VDU.
           PERFORM SCREENIO.

           IF ( VDUSTATUS = "16" )
              PERFORM SELECT-COLLID
              IF ( IO-FG = 0 )
                 MOVE GQSCN3-COLLID TO GQSCAN-COLLID
                 MOVE "Y"           TO WS-CHANGE-COLLID-FG
                 GO TO ACCESS-COLLID
              ELSE
                 GO TO ACCESS-COLLID.

           IF ( VDUSTATUS = "1<" )
              PERFORM MORE-INFO
              IF ( WS-VDUSTATUS = "1>" )
                 GO TO ACCESS-COLLID
              ELSE
                 GO TO END-ROUTINE.

           IF ( VDUSTATUS NOT = "1>" )
              GO TO ENTER-SEL.

           MOVE VDUSTATUS TO FLDSEL-STAT.

           GO TO END-ROUTINE.

      ******************************************************************
       MORE-INFO SECTION.
      ******************************************************************
           PERFORM OPEN-BR-FILE.
           MOVE 17 TO WINDOW-LINES.
           MOVE 78 TO WINDOW-COLUMNS.
           MOVE 6 TO WINDOW-BEGIN-Y.
           MOVE 2 TO WINDOW-BEGIN-X.
           MOVE "WI/GQSCN2" TO WINDOW-FORM-NAM.
           MOVE "GQSCN2" TO VDU-FORM-NAME.

           PERFORM OPEN-WINDOW.
           DISPLAY GQSCN2-SCREEN.

           MOVE GQSCN2-FORM-FIELDS TO WR-BUF.
           MOVE GQSCN2-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
      *-----------------------------------------------------------------
       MORE-INFO-BEGIN.
           MOVE GQSCAN-BR-LOAN-NUM-FG TO BR-LOAN-NUM-FLAG.
           MOVE GQSCAN-SP-EXPD-FRMLA  TO SP-EXPD-FRMLA.
           MOVE GQSCAN-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD         TO AGEING-DATE DLS-DATE.

           PERFORM GET-DLS.

           MOVE "B" TO SCANTYPE.
           MOVE SPACES TO WS-VDUSTATUS.
           MOVE "N" TO RECORDS-FOUND.
           GO TO ENTER-SCANTYPE-CONT.

      *-----------------------------------------------------------------
      *    ENTER TYPE OF SCAN
      *-----------------------------------------------------------------
       ENTER-SCANTYPE.

           IF ( SCANTYPE = "B" )
              MOVE "U" TO SCANTYPE
           ELSE
           IF ( SCANTYPE = "U" )
              MOVE "R" TO SCANTYPE
           ELSE
           IF ( SCANTYPE = "R" )
              MOVE "W" TO SCANTYPE
           ELSE
              MOVE "B" TO SCANTYPE.

      *-----------------------------------------------------------------
       ENTER-SCANTYPE-CONT.

           MOVE "S  A230DESC." TO VDU.

           IF ( SCANTYPE = "U" )
              MOVE "OF UNWORKED ACCOUNTS" TO VDUBUF
           ELSE
           IF ( SCANTYPE = "W" )
              MOVE "OF WORKED ACCOUNTS" TO VDUBUF
           ELSE
           IF ( SCANTYPE = "B" )
              MOVE "OF BROKEN PROMISES" TO VDUBUF
           ELSE
              MOVE "OF RESCHEDULED ACCOUNTS" TO VDUBUF.

           PERFORM SCREENIO.

           IF SCANTYPE = "R"
              MOVE "RE-SCHEDULED:" TO DATE-PROMPT1
              MOVE "DATE     TIME" TO DATE-PROMPT2
           ELSE
           IF SCANTYPE = "B"
              IF GP-TIME-ZONE-Q NOT = "Y"
                 MOVE "PROMISED" TO DATE-PROMPT1
                 MOVE "DATE" TO DATE-PROMPT2
              ELSE
                 MOVE "PROM     CUST" TO DATE-PROMPT1
                 MOVE "DATE     TIME" TO DATE-PROMPT2
              END-IF
           ELSE
              IF GP-TIME-ZONE-Q NOT = "Y"
                 MOVE "QUEUED" TO DATE-PROMPT1
                 MOVE "DATE" TO DATE-PROMPT2
              ELSE
                 MOVE "QUEUED   CUST" TO DATE-PROMPT1
                 MOVE "DATE     TIME" TO DATE-PROMPT2.
           MOVE DATE-PROMPT-LIST TO WR-LIST.
           MOVE DATE-PROMPT-BUF TO WR-BUF.
           PERFORM CALL-WRFORM.

           MOVE 0 TO HOLD-DELINQUENCY.
           GO TO BEGIN-GQ-SCAN.

       ENTER-DELINQUENCY.
           MOVE "EN N040DEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              GO TO MORE-INFO-CLOSE-WINDOW.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-DELINQUENCY.
           IF GQSCAN-BR-LOW-TO-HI = "Y"
              MOVE VDUNUM0 TO HOLD-DELINQUENCY
           ELSE
              COMPUTE HOLD-DELINQUENCY = 99000 - VDUNUM0.

       BEGIN-GQ-SCAN.
           MOVE FIRST-FIELD TO FLDSEL-FIELD.
           MOVE 1 TO PAGE-COUNT ONE.
           MOVE 0 TO CURRENT-PAGE.
       
           IF (GQSCAN-VGQ-PATH = SPACES)
              PERFORM OPEN-GQ2-FILE
           ELSE
              PERFORM OPEN-VGQ1-FILE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE SPACES         TO PKGQ2-GQ2-KEY          (PAGE-COUNT).
           MOVE GQSCAN-COLLID  TO PKGQ2-COLLID           (PAGE-COUNT).
           MOVE ZEROES         TO PKGQ2-START-TIME       (PAGE-COUNT)
                                  PKGQ2-DATE             (PAGE-COUNT).
           MOVE HOLD-DELINQUENCY
                               TO PKGQ2-REV-DEL          (PAGE-COUNT).
           MOVE 0              TO PKGQ2-REV-PDUE-DAYS    (PAGE-COUNT).
           MOVE 0              TO PKGQ2-REV-BAL          (PAGE-COUNT).
           MOVE 0              TO PKGQ2-SEQ              (PAGE-COUNT).

           IF ( SCANTYPE = "R" )
              MOVE "R"         TO PKGQ2-SEQ-FG           (PAGE-COUNT)
           ELSE
           IF ( SCANTYPE = "B" ) OR ( SCANTYPE = "W" )
              MOVE "B"         TO PKGQ2-SEQ-FG           (PAGE-COUNT)
           ELSE
              MOVE "N"         TO PKGQ2-SEQ-FG           (PAGE-COUNT).

      ***  MOVE 1 TO PAGE-DATE(PAGE-COUNT).

      *-----------------------------------------------------------------
       START-BUILD-PAGENO.

           ADD ONE TO CURRENT-PAGE.

           IF CURRENT-PAGE < 1
              MOVE FIRST-FIELD TO FLDSEL-FIELD
              MOVE 1 TO CURRENT-PAGE.

           IF CURRENT-PAGE > PAGE-COUNT
              MOVE FIRST-FIELD TO FLDSEL-FIELD
              MOVE 1 TO CURRENT-PAGE.

           MOVE SPACES TO DISPLAY-BUF.
           MOVE 1 TO SUB.

           IF ( GQSCAN-VGQ-PATH NOT = SPACES )
              MOVE CURRENT-PAGE TO PAGE-COUNT.

           PERFORM SET-PKGQ2-TO-GQ2.

           IF GQ-DATE = 0
              MOVE 19000101 TO GQ-DATE.

           IF ( GQSCAN-VGQ-PATH = SPACES )
              PERFORM START-GQ2-FILE
           ELSE
              PERFORM LOAD-VGQ1-GQ2-KEY
              PERFORM START-VGQ1-FILE.
      
      D    STOP GQSCAN-VGQ-PATH.   
      *-----------------------------------------------------------------
      *    BUILD PAGE
      *-----------------------------------------------------------------
       BUILD-PAGENO.

           IF ( GQSCAN-VGQ-PATH = SPACES )
              PERFORM READ-GQ2-FILE-NEXT
           ELSE
              PERFORM READ-VGQ1-FILE-NEXT.

           IF IO-FG NOT = 0
              GO TO DISPLAY-PAGE.

           IF ( GQSCAN-VGQ-PATH = SPACES )
              MOVE VGQ-GQ-REC TO GQ-REC.

           IF GQ-COLLID NOT = GQSCAN-COLLID
              MOVE 9 TO IO-FG
              GO TO DISPLAY-PAGE.

           IF SUB = 1
              IF CURRENT-PAGE > 1
                 IF GQ-ACCTNO NOT = PAGE-ACCTNO(CURRENT-PAGE)
                    GO TO BUILD-PAGENO.

           IF ( SCANTYPE = "U" ) OR ( SCANTYPE = "R" )
              IF ( GQ-WORKED-FG = "Y" )
                 GO TO BUILD-PAGENO
              ELSE
                 NEXT SENTENCE
           ELSE
              IF ( SCANTYPE = "W" )
                 IF ( GQ-WORKED-FG NOT = "Y" )
                    GO TO BUILD-PAGENO.

           IF ( SCANTYPE = "B" )
              IF GQ-SEQ-FG NOT = "B"
                 MOVE 9 TO IO-FG
                 GO TO DISPLAY-PAGE.

           IF ( SCANTYPE = "B" )
              IF GQ-WORKED-FG = "Y"
                    GO TO BUILD-PAGENO.

           IF ( SCANTYPE = "U" )
              IF GQ-SEQ-FG NOT = "N"
                 MOVE 9 TO IO-FG
                 GO TO DISPLAY-PAGE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    A15 GQ2-KEY VERSION OF PAGE-KEY                KEC 2016.1006
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    IF HOLD-DELINQUENCY NOT = 0
      *       IF SCANTYPE = "U"
      *          MOVE GQ2-KEY TO PAGE-KEY(PAGE-COUNT)
      *          MOVE HOLD-DELINQUENCY TO PAGE-REV-DEL(PAGE-COUNT)
      *          MOVE 0 TO PAGE-REV-PDUE-DAYS(PAGE-COUNT)
      *                    PAGE-REV-BAL(PAGE-COUNT)
      *          MOVE PAGE-KEY(PAGE-COUNT) TO GQ2-KEY
      *          PERFORM START-GQ2-FILE
      *          MOVE 0 TO HOLD-DELINQUENCY
      *          GO TO BUILD-PAGENO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    A15 GQ2-KEY VERSION OF PAGE-KEY                KEC 2016.1006
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( HOLD-DELINQUENCY NOT = ZEROES )
              IF ( SCANTYPE = "U" )
                 PERFORM SET-GQ2-TO-PKGQ2 
                 MOVE HOLD-DELINQUENCY TO PKGQ2-REV-DEL  (PAGE-COUNT)
                 MOVE ZEROES       TO PKGQ2-REV-PDUE-DAYS(PAGE-COUNT)
                                      PKGQ2-REV-BAL      (PAGE-COUNT)
                                      PKGQ2-SEQ          (PAGE-COUNT)
                 PERFORM SET-PKGQ2-TO-GQ2
                 IF ( GQSCAN-VGQ-PATH = SPACES )
                    PERFORM START-GQ2-FILE
                    MOVE ZEROES               TO HOLD-DELINQUENCY
                    GO TO BUILD-PAGENO
                 ELSE
                    PERFORM LOAD-VGQ1-GQ2-KEY
                    PERFORM START-VGQ1-FILE
                    MOVE ZEROES               TO HOLD-DELINQUENCY
                    GO TO BUILD-PAGENO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF SUB > 9
              GO TO DISPLAY-PAGE.

           MOVE SPACES TO D-LINE(SUB).

           IF GQ-BRNO NOT = HOLD-BRNO
              PERFORM SET-FILE-PATHS.

           MOVE GQ-ACCTNO TO LN-ACCTNO.
           PERFORM GET-LN1-FILE.
           IF IO-FG = 9
              MOVE "<NOT ON FILE>" TO D-NAME(SUB)
           ELSE
              MOVE LN-OWNBR TO D-BRNO(SUB)
              PERFORM LPSTAT-DISPLAY
              MOVE LPS-DISPLAY-LNNO TO D-LN-ACCTNO(SUB)
              MOVE LN-SSNO(1) TO BW-SSNO
              PERFORM GET-BW1-FILE
              IF IO-FG = 9
                 MOVE "<NOT ON FILE>" TO D-NAME(SUB)
              ELSE
                 MOVE BW-LNAME TO MESS
                 MOVE BW-FNAME TO A-NAME
                 PERFORM SET-GQ-LNAME
                 MOVE MESS TO D-NAME(SUB).

           PERFORM GET-SP1-FILE.
           PERFORM COMPUTE-CONTRACTUAL.
           IF CPOT30 = "Y"
              MOVE "POT" TO D-CON-DELX(SUB)
           ELSE
           IF CPOT30 = "M"
              MOVE "MIS" TO D-CON-DELX(SUB)
           ELSE
           IF CPOTCHGOFF = "Y"
              MOVE "PC" TO D-CON-DELX(SUB)
           ELSE
           IF CPOTCHGOFF = "M"
              MOVE "PL" TO D-CON-DELX(SUB)
           ELSE
              MOVE CONTRACTUAL TO D-CON-DEL(SUB).

           COMPUTE BAL-WK ROUNDED = LN-CURBAL - LN-OT2BAL.
           MOVE BAL-WK TO D-BAL(SUB).

           MOVE GQ-DATE             TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY         TO D-RESCH-DATE(SUB).

      *PR87IF GP-I-AM-TFC
      *PR87   IF GP-TIME-ZONE-Q = "Y"
      *PR87      COMPUTE TIME-ZONE-FACTOR =
      *PR87         GQ-START-TIME - GP-TIME-ZONE-FACTOR
      *PR87      IF GP-TIME-ZONE-DLS = "Y"
      *PR87         IF DLS-FG = "Y"
      *PR87            SUBTRACT 1 FROM TIME-ZONE-FACTOR
      *PR87         END-IF
      *PR87      END-IF
      *PR87      ACCEPT TIME-NOW FROM TIME
      *PR87      ADD TIME-ZONE-FACTOR TO CUR-HH
      *PR87      IF CUR-HH > 23
      *PR87         SUBTRACT 24 FROM CUR-HH
      *PR87      END-IF
      *PR87      IF H-AND-M < GP-CALL-START-TIME OR
      *PR87         H-AND-M > GP-CALL-END-TIME
      *PR87         MOVE SPACES TO D-LINE(SUB)
      *PR87         GO TO BUILD-PAGENO.

           IF GQ-SEQ-FG = "B" OR GQ-SEQ-FG = "R"
              IF ( GQ-WORKED-FG = "Y" )
                 MOVE GQ-DATE-QUEUED TO DATE-YYYYMMDD
                 PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
                 MOVE DATE-MMDDYY TO D-RESCH-DATE(SUB).

           IF ( GQ-WORKED-FG = "Y" )
              MOVE LN-RECODE TO D-RESULT(SUB)
           ELSE
              MOVE SPACES TO D-RESULT(SUB).

           IF GQ-SEQ-FG = "N"
              IF GP-TIME-ZONE-Q NOT = "Y"
                 MOVE " " TO D-TIME(SUB)
              ELSE
                 COMPUTE TIME-ZONE-FACTOR =
                    GQ-START-TIME - GP-TIME-ZONE-FACTOR
                 IF GP-TIME-ZONE-DLS = "Y"
                    IF DLS-FG = "Y"
                       SUBTRACT 1 FROM TIME-ZONE-FACTOR
                    END-IF
                 END-IF
                 PERFORM ZONED-GET-TIME
                 MOVE TIME-EDIT TO D-TIME(SUB).

           IF GQ-SEQ-FG = "B"
              IF GQ-WORKED-FG NOT = "Y"
                 MOVE LN-PROMDATE TO DATE-YYYYMMDD
                 PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
                 MOVE DATE-MMDDYY TO D-RESCH-DATE(SUB)
                 MOVE "PRIORITY" TO D-TIME(SUB)
                 MOVE LN-RECODE TO D-RESULT(SUB).

           IF GQ-SEQ-FG = "B"
              IF GQ-WORKED-FG NOT = "Y"
                 IF GP-TIME-ZONE-Q NOT = "Y"
                    MOVE " " TO D-TIME(SUB)
                 ELSE
                    COMPUTE TIME-ZONE-FACTOR =
                       GQ-START-TIME - GP-TIME-ZONE-FACTOR
                    IF GP-TIME-ZONE-DLS = "Y"
                       IF DLS-FG = "Y"
                          SUBTRACT 1 FROM TIME-ZONE-FACTOR
                       END-IF
                    END-IF
                    PERFORM ZONED-GET-TIME
                    MOVE TIME-EDIT TO D-TIME(SUB).

           IF (GQ-START-TIME = 0) OR (GQ-SEQ-FG NOT = "R")
              OR (GQ-WORKED-FG = "Y")
              GO TO BUILD-PAGE-CONT.

           MOVE GQ-START-TIME TO WK-START-TIME.

           IF WK-START-HH > 11
              MOVE "PM" TO D-AM-PM(SUB)
           ELSE
              MOVE "AM" TO D-AM-PM(SUB).

           IF WK-START-HH > 12
              SUBTRACT 12 FROM WK-START-HH.

           MOVE WK-START-HH TO D-HH(SUB).
           MOVE ":" TO D-COLON(SUB).
           MOVE WK-START-MM TO D-MM(SUB).

      *-----------------------------------------------------------------
       BUILD-PAGE-CONT.

           MOVE "Y" TO RECORDS-FOUND.
           MOVE GQ-WORK-COLLID TO D-WORK-COLLID(SUB).
           ADD 1 TO SUB.
           GO TO BUILD-PAGENO.

      *-----------------------------------------------------------------
      *    DISPLAY PAGE
      *-----------------------------------------------------------------
       DISPLAY-PAGE.

           IF IO-FG NOT = 0
              IF SUB = 1
                 MOVE "NO RECORDS FOUND!" TO LEGEND-WK
                 PERFORM SEND-LEGEND
                 MOVE DISPLAY-LIST TO WR-LIST
                 MOVE DISPLAY-BUF TO WR-BUF
                 PERFORM CALL-WRFORM
                 IF (RECORDS-FOUND = "N" AND SCANTYPE = "W")
                    MOVE "1>" TO WS-VDUSTATUS
                    GO TO MORE-INFO-CLOSE-WINDOW
                 ELSE
                    GO TO ENTER-SCANTYPE.

           MOVE "MORE..." TO D-END-MORE.

           IF CURRENT-PAGE = PAGE-COUNT
              IF IO-FG = 0
                 IF PAGE-COUNT < MAX-PAGES
                    ADD 1 TO PAGE-COUNT
                    PERFORM SET-GQ2-TO-PKGQ2
                    MOVE GQ-ACCTNO TO PAGE-ACCTNO(PAGE-COUNT)
                 ELSE
                    MOVE "MAXIMUM PAGES REACHED!" TO LEGEND-WK
                    PERFORM SEND-LEGEND
                    MOVE "OVRFLOW" TO D-END-MORE
              ELSE
                 MOVE "* END *" TO D-END-MORE.

           MOVE CURRENT-PAGE TO D-PAGENO.
           MOVE DISPLAY-LIST TO WR-LIST.
           MOVE DISPLAY-BUF TO WR-BUF.
           PERFORM CALL-WRFORM.

      *-----------------------------------------------------------------
      *    SELECT FIELD FROM PAGES
      *-----------------------------------------------------------------
       SELECT-FIELD-FROM-DISPLAY.

           MOVE MORE-PROMPT TO LEGEND-WK.
           PERFORM SEND-LEGEND.

           PERFORM CALL-FLDSEL.

           MOVE FLDSEL-STAT TO WS-VDUSTATUS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * TEST UP ARROW:
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF FLDSEL-STAT = "1U"
              IF CURRENT-PAGE = 1
                 GO TO SELECT-FIELD-FROM-DISPLAY
              ELSE
                 MOVE -1 TO ONE
                 MOVE LAST-FIELD TO FLDSEL-FIELD
                 GO TO START-BUILD-PAGENO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * TEST PAGE UP:
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF FLDSEL-STAT = "1P"
              IF CURRENT-PAGE = 1
                 GO TO SELECT-FIELD-FROM-DISPLAY
              ELSE
                 MOVE -1 TO ONE
                 MOVE FIRST-FIELD TO FLDSEL-FIELD
                 GO TO START-BUILD-PAGENO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * TEST PAGE DOWN OR DOWN ARROW:
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF FLDSEL-STAT = "1N" OR FLDSEL-STAT = "1D"
              IF CURRENT-PAGE NOT < PAGE-COUNT
                 GO TO SELECT-FIELD-FROM-DISPLAY
              ELSE
                 MOVE 1 TO ONE
                 MOVE FIRST-FIELD TO FLDSEL-FIELD
                 GO TO START-BUILD-PAGENO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * TEST HOME:
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF FLDSEL-STAT = "1H"
              MOVE 0 TO CURRENT-PAGE
              MOVE 1 TO ONE
              MOVE FIRST-FIELD TO FLDSEL-FIELD
              GO TO START-BUILD-PAGENO.

           IF FLDSEL-STAT = "1>"
              GO TO MORE-INFO-CLOSE-WINDOW.

           IF FLDSEL-STAT = "10"
              GO TO ENTER-SCANTYPE.

           IF FLDSEL-STAT = "1<"
              GO TO ENTER-DELINQUENCY.

           IF NOT (FLDSEL-STAT = "00")
              GO TO SELECT-FIELD-FROM-DISPLAY.

           IF FLDSEL-NAME = "<NOT ON FILE>"
              MOVE "INVALID SELECTION" TO MESS
              PERFORM SEND-MESS
              GO TO SELECT-FIELD-FROM-DISPLAY.

           MOVE FLDSEL-BRNO      TO GQSCAN-BRANCH.
           MOVE FLDSEL-LN-ACCTNO TO GQSCAN-ACCTNO.
           MOVE "00"             TO GQSCAN-STATUS.

      *-----------------------------------------------------------------
       MORE-INFO-CLOSE-WINDOW.
           DESTROY GQSCN2-SCREEN.
           PERFORM CLOSE-WINDOW.
           MOVE "GQSCAN" TO VDU-FORM-NAME.

       MORE-INFO-EXIT.
           EXIT.

      ******************************************************************
       SELECT-COLLID SECTION.
      ******************************************************************

           MOVE SPACES        TO GQSCN3-BUF.
           MOVE GQSCAN-COLLID TO GQSCN3-COLLID.
           MOVE ZEROS         TO GQSCN3-BRANCH.
           MOVE "WI/GQSCN3 " TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH GQSCN3-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.

           IF ( GQSCN3-STATUS NOT = "00" )
              MOVE 9 TO IO-FG
           ELSE
              MOVE 0 TO IO-FG.

       EXIT-SELECT-COLLID.
           EXIT.

      ******************************************************************
       INVALID-COLLID SECTION.
      ******************************************************************

       SELECT-ANOTHER-COLLID.

           PERFORM SELECT-COLLID.
           MOVE GQSCN3-COLLID TO GQSCAN-COLLID.

           IF IO-FG NOT = 0
              IF ( GQSCN3-STATUS = "1>" )
                 GO TO EXIT-INVALID-COLLID
              ELSE
                 GO TO SELECT-ANOTHER-COLLID.

       EXIT-INVALID-COLLID.
           EXIT.

      **************************************
       SET-FILE-PATHS SECTION.
      **************************************
           MOVE GQ-BRNO TO HOLD-BRNO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           MOVE EXT-FILPATH-BASE TO FILPATH-BASE.
           MOVE BR-MACHINE       TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH     TO FILPATH-DATA.

           MOVE "B"        TO BW-PATH-OVERRIDE
                              LN-PATH-OVERRIDE
                              LTP-PATH-OVERRIDE.


      **************************************************************************
       FORM-RESET SECTION.
      **************************************************************************
           DISPLAY GQSCAN-SCREEN UPON GQSCAN-WINDOW-HANDLE.
           MOVE GQSCAN-FORM-FIELDS TO WR-BUF.
           MOVE GQSCAN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       FORM-RESET-EXIT.
           EXIT.

      **************************************************************************
       SETUP-LINK-INFO SECTION.
      **************************************************************************
           MOVE GQSCAN-HELP--FILE TO HELP-LINK-FILE.
           MOVE GQSCAN-HELP--DIR  TO HELP-LINK-DIR.

      **************************************************************************
       VDU-CONVERT-FIELD SECTION.
      **************************************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/GQSCAN_EVA.CPY".
              COPY "LIBWI/GQSCN2_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISC-PARAGRAPHS SECTION.
      ******************************************************************
      *-----------------------------------------------------------------
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/MKTEMP.CPY".
      *-----------------------------------------------------------------
       SET-GQ-LNAME.
           INSPECT MESS REPLACING FIRST "  " BY ",?".
           INSPECT MESS REPLACING FIRST "                "
               BY A-NAME.
           INSPECT MESS REPLACING ALL "?" BY " ".

      *-----------------------------------------------------------------
       GET-CD1-FILE.
           PERFORM OPEN-CDB1-FILE.
           PERFORM READ-CDB1-FILE.
           PERFORM CLOSE-CDB1-FILE.

      *-----------------------------------------------------------------
       GET-LN1-FILE.
           PERFORM OPEN-LN1-FILE.
           PERFORM READ-LN1-FILE.
           PERFORM CLOSE-LN1-FILE.

      *-----------------------------------------------------------------
       GET-BW1-FILE.
           PERFORM OPEN-BW1-FILE.
           PERFORM READ-BW1-FILE.
           PERFORM CLOSE-BW1-FILE.

      *-----------------------------------------------------------------
       GET-SP1-FILE.
           MOVE LN-ORGST TO SP-ORGST.
           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           MOVE LN-LAWCODE TO SP-LAWCODE.
           PERFORM READ-SP1-FILE
           IF IO-FG NOT = 0
              PERFORM FILE-ERRORS
              PERFORM SEND-MESS.
      *-----------------------------------------------------------------
       SEND-LEGEND.
           MOVE "S  A600WSEL:D." TO VDU.
           MOVE LEGEND-WK TO VDUBUF.
           PERFORM SCREENIO.

       SET-PKGQ2-TO-GQ2.
  
           MOVE PKGQ2-COLLID(PAGE-COUNT)         TO GQ-COLLID.
           MOVE PKGQ2-SEQ-FG(PAGE-COUNT)         TO GQ-SEQ-FG.
           MOVE PKGQ2-DATE(PAGE-COUNT)           TO GQ-DATE.
           MOVE PKGQ2-START-TIME(PAGE-COUNT)     TO GQ-START-TIME.
           MOVE PKGQ2-REV-DEL(PAGE-COUNT)        TO GQ-REV-DEL.
           MOVE PKGQ2-MISC-KEY-FLD(PAGE-COUNT)   TO GQ-MISC-KEY-FLD.
           MOVE PKGQ2-REV-PDUE-DAYS(PAGE-COUNT)  TO GQ-REV-PDUE-DAYS.
           MOVE PKGQ2-REV-BAL(PAGE-COUNT)        TO GQ-REV-BAL.
           MOVE PKGQ2-SEQ(PAGE-COUNT)            TO GQ-SEQ.
           IF GQ-DATE = 0
              MOVE 19000101                      TO GQ-DATE.
  
       SET-GQ2-TO-PKGQ2.

           MOVE GQ-COLLID            TO PKGQ2-COLLID(PAGE-COUNT).      
           MOVE GQ-SEQ-FG            TO PKGQ2-SEQ-FG(PAGE-COUNT).      
           MOVE GQ-DATE              TO PKGQ2-DATE(PAGE-COUNT).        
           MOVE GQ-START-TIME        TO PKGQ2-START-TIME(PAGE-COUNT).
           MOVE GQ-REV-DEL           TO PKGQ2-REV-DEL(PAGE-COUNT).
           MOVE GQ-MISC-KEY-FLD      TO PKGQ2-MISC-KEY-FLD(PAGE-COUNT).
           MOVE GQ-REV-PDUE-DAYS     TO PKGQ2-REV-PDUE-DAYS(PAGE-COUNT).
           MOVE GQ-REV-BAL           TO PKGQ2-REV-BAL(PAGE-COUNT).
           MOVE GQ-SEQ               TO PKGQ2-SEQ(PAGE-COUNT).


      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBWI/PFSWINDOWS.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/ZONEDTIMEIO.CPY".
       COPY "LIBGB/GETDLS.CPY".
       COPY "LIBGB/AGEING.CPY".
       COPY "LIBLP/LPCEPP.CPY".
       COPY "LIBGB/SPPDTH.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBLP/LPUPWK.CPY".
       COPY "LIBGB/FLDSEL.CPY".
       COPY "LIBLP/LPSTAT.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBLP/LPPDUE.CPY".
       COPY "LIBSP/SPFRVGQ.CPY".

      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBSP/SPGQSGS_SQL.CPY".
       COPY "LIBSP/SPGQGS_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLTPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPCDBGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBSP/SPGQS1RN.CPY".
       COPY "LIBSP/SPGQ1RN.CPY".
       COPY "LIBSP/SPGQ2RN.CPY".
       COPY "LIBSP/SPVGQ1IN.CPY".
       COPY "LIBLP/LPCDB1RN.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBLP/LPLTP1RN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      ******************************************************************
       END-ROUTINE SECTION.
      ******************************************************************

           PERFORM CLOSE-FILES.

           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY GQSCAN-SCREEN.
           DESTROY GQSCAN-WINDOW-HANDLE.
           PERFORM CLOSE-WINDOW UNTIL WINDOWS-OPEN = 0.

       EXIT-PROG.

       STOP-RUN.
           EXIT PROGRAM.
