       IDENTIFICATION DIVISION.
       PROGRAM-ID.      CJINQ.
      *******************************************************************
      *                 (WI)
      *
      *  DESCRIPTION:   DAILY COLLECTOR ACTIVITY INQUIRY
      *                  #14 ON COLLECTION PROCESSING MENU
      * REV:
      *  GLF 090997 REPLACED 'IO' ON GP-FILE WITH GET 'GPENV'
      *  GLF 022398 ADDED LIBGB/BRSECUREW.
      *  BLV 061499 REMOVE WORK FILE
      *  BAH 990805 EXPANDED WK-CUR-BAL AND WK-PROMISE-AMT FROM
      *             9(6)V99 TO 9(9)V99, TFC # 233
      *  BLV 020730 FIX BUFFER NOT GETTING CLEARED IF DOING SUMMARY FOR
      *             ALL COLLECTORS, EXTRA LINES FROM PRIOR INQUIRY
      *             MIGHT DISPLAY BELOW CURRENT LINES, FAIRCO
      *  BLF 070710 REMOVE ALL F5 MAILBOX OPTIONS.  IT WAS CAUSING
      *             "MISSING FIELD" ERRORS AT WORLD WHEN THE USER QUIT
      *             THE MAILBOX.  I THINK IT NEEDED TO HAVE THE CJINQ
      *             WINDOW SET BACK UP AFTER THE MALBOX CANCEL, BUT WE
      *             DIDN'T REALLY THINK THEY NEEDED THE F5 OPTION HERE.
      *  BAH 120815 CHANGED FLDVAL TO ACCEPT DATE AND UNAME CALL
      *   CS 140924 ADDED PERFORM FORM RESET, PROMPTS WERE NOT DISPLAYED
      *
      * KEC 2016.0328 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED CDSCAN-WINDOW-BUF TO HAVE MATCHING CD1-KEY
      *         LAYOUTS (FOR EACH CODE TYPE; CD-TYPE).
      *         CDSCAN-CD1-KEY IS THE NEW (PRIMARY) WITH REDEFINES OF
      *         CDSCAN-??-KEY (NEW).
      *  BAH 160602 CHANGED CJ-DATE FROM JULIAN TO YYYYMMDD
      *  BAH 170412 ACTIVATE 8 DIGIT DATES, PD0006
      *   CS 190118 GLOBAL CJFILE
      *  BAH 190128 SPLIT CDFILE, CHANGED CDSCAN TO CDBSCN FOR "CN"
      *  BAH 20191007 REMOVED ACCESS-CALL CJFILE
      *   MB 20250314 PERFORMANCE TUNING - ADDED STORED PROCEDURE
      *              ADDED STATE PARAMETER ARRAY PROCESSING  S35Q-183
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT WK-FILE ASSIGN MKTEMP-BUF
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY WK-KEY
                  ALTERNATE KEY WK2-KEY WITH DUPLICATES
                  FILE STATUS FILE-STAT.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.

      ******************************************************************
      *
      *    W O R K - F I L E
      *
      *-----------------------------------------------------------------
      *    COLLECTOR TOTALS:
      *         WK-BRNO        = HAS THE CURRENT BRANCH NUMBER.
      *         WK-COLLID      = HAS THE COLLECTOR ID.
      *         WK-RESULT-CODE = HAS A RESULT CODE.
      *    BRANCH TOTALS:
      *         WK-BRNO        = HAS THE CURRENT BRANCH NUMBER.
      *         WK-COLLID      = SPACES.
      *         WK-RESULT-CODE = HAS A RESULT CODE.
      *    GRAND TOTALS:
      *         WK-BRNO        = ZEROES.
      *         WK-COLLID      = SPACES.
      *         WK-RESULT-CODE = HAS A RESULT CODE.
      *-----------------------------------------------------------------
      *    WK-RES-NUMBER OCCURS 7 TIMES.
      *       1 = POT/CURRENT  4 = 90        7 = TOTALS
      *       2 = 30           5 = 120
      *       3 = 60           6 = 150+
      ******************************************************************
       FD  WK-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK-REC.
           03  WK2-KEY.
               05  WK-REV-COUNT        PIC 9(6).
               05  WK-KEY.
                   07  WK-RESULT-CODE  PIC X(3).
                   07  WK-COLLID.
                       09  WK-COLLID-TAB PIC X OCCURS 3.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           03  WK-COUNT                PIC 9(6).
           03  WK-CUR-BAL              PIC 9(9)V99.
           03  WK-PROMISE-AMT          PIC 9(9)V99.

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(40) VALUE
           "@(#)WI/CJINQ.CBL S35 02/22/24-12:51:28 >".
       COPY "LIBSP/SP01CJ.CPY".
       COPY "LIBSP/SP01CJ_SQL.CPY".
       COPY "LIBSP/SPWSCJ.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
      *    **********************************
      *    *      PROGRAM WORKERS
      *    **********************************
       01  WK-IFN            PIC X(2)  VALUE "WK".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  PASS-OFFSET                 PIC 9(9).
       01  PASS-DETSUM                 PIC X.
       EXEC SQL END DECLARE SECTION END-EXEC.

       01  CJ1-CONN-OPEN-SW            PIC X    VALUE "N".

       01  CJINQ-LINES           PIC 99   VALUE 22.
       01  CJINQ-COLUMNS         PIC 99   VALUE 78.
       01  CJINQ-BEGIN-Y         PIC 99   VALUE 2.
       01  CJINQ-BEGIN-X         PIC 99   VALUE 2.

       01  ERRCD             PIC X          VALUE SPACES.
       01  MSEL-LIST         PIC X(8)       VALUE "MSEL:P.".
       01  RESET-FG          PIC X          VALUE "Y".
       01  ONE               PIC S9.
       01  SUB               PIC 999 COMP   VALUE 0.
       01  IN-SUB            PIC 99         VALUE 0.
       01  OUT-SUB           PIC 99         VALUE 0.
       01  COLLECTOR-TABLE.
           03  COLLECTOR-TAB PIC X(3) OCCURS 8.
       01  COLLECTOR-SUB     PIC 999 COMP   VALUE 0.
       01  COL-WORK          PIC 999 COMP   VALUE 0.
       01  COL-COUNT         PIC 999 COMP   VALUE 0.
       01  HOLD-RESULT-CODE  PIC X(3)       VALUE SPACES.
       01  HOLD-WK2-KEY      PIC X(12).
       01  IO-TYPE           PIC X(3).

       01  TOT-SUM-COUNT         PIC 9(6)    VALUE 0.
       01  TOT-SUM-CUR-BAL       PIC 9(9)V99 VALUE 0.
       01  TOT-SUM-PROMISE-AMT   PIC 9(9)V99 VALUE 0.
       01  OTHER-SUM-COUNT       PIC 9(6)    VALUE 0.
       01  OTHER-SUM-CUR-BAL     PIC 9(9)V99 VALUE 0.
       01  OTHER-SUM-PROMISE-AMT PIC 9(9)V99 VALUE 0.
       01  OTHER-ALL-TABLE.
           03  OTHER-ALL-COUNT   PIC 9(6) OCCURS 9.
       01  TOTAL-ALL-TABLE.
           03  TOTAL-ALL-COUNT   PIC 9(6) OCCURS 9.
       01  OTHER-COL-COUNT       PIC 9(6)    VALUE 0.

       01  HOLD-SYSDATE        PIC 9(8).

       01  INQUIRY-RANGES.
           03  BEG-BRANCH      PIC 9(4).
           03  END-BRANCH      PIC 9(4).
           03  DATE1           PIC 9(8).
           03  DATE2           PIC 9(8).
           03  ENT-GROUP       PIC X(4).
           03  COL-ID          PIC X(3).
           03  INQ-TYPE        PIC X.

       01  FLDSEL-STAT     PIC XX.
       01  FLDSEL-LIST.
           03  FILLER      PIC X(40)  VALUE
               "010 020 030 040 050 060 070 080 090 100 ".
           03  FILLER      PIC X(20)  VALUE
               "110 120 130 140 150.".
       01  FLDSEL-BUF.
           03  FLDSEL-BRNO             PIC 9999.
           03  FILLER                  PIC XX.
           03  FLDSEL-DATE             PIC 99/99/99.
           03  FILLER                  PIC X(2).
           03  FLDSEL-STATUS           PIC X(9).
           03  FILLER                  PIC XXX.
           03  FLDSEL-RECAP-FG         PIC X(5).
           03  FILLER                  PIC X.
           03  FLDSEL-GL-FG            PIC X(5).
           03  FILLER                  PIC X.
           03  FLDSEL-EOD-BATCH-FG     PIC X(5).
           03  FILLER                  PIC X.
           03  FLDSEL-NIGHTLY-FUNDS-FG PIC X(5).
           03  FILLER                  PIC X(4).
           03  FLDSEL-EOM1-BATCH-FG    PIC X(5).
           03  FILLER                  PIC X(4).
           03  FLDSEL-EOM2-BATCH-FG    PIC X(5).
           03  FILLER                  PIC X(2).
           03  FLDSEL-OVER-SHORT       PIC ZZZZ9.99-.

       01  FIRST-FIELD          PIC X(4)  VALUE "010.".
       01  LAST-FIELD           PIC X(4)  VALUE "150.".

       01  FLDSEL-FIELD  PIC X(6)  VALUE "010.  ".
      *       THE ABOVE FIELD IS 2 EXTRA ON PURPOSE

       01  HEADING-LIST    PIC X(10) VALUE "HEADING:S.".
       01  HEADING-BUF     PIC X(76) VALUE SPACES.

       01  DETAIL-HEADING.
           03  FILLER        PIC X(9)    VALUE "  DATE   ".
           03  FILLER        PIC X(7)    VALUE " ACCT  ".
           03  FILLER        PIC X(8)    VALUE "BORROWER".
           03  FILLER        PIC X(2)    VALUE SPACES.
           03  FILLER        PIC X(3)    VALUE "ATV".
           03  FILLER        PIC X(1)    VALUE SPACES.
           03  FILLER        PIC X(3)    VALUE "RST".
           03  FILLER        PIC X(2)    VALUE SPACES.
           03  FILLER        PIC X(9)    VALUE "PROMDATE ".
           03  FILLER        PIC X(9)    VALUE " PROM-AMT".
           03  FILLER        PIC X(4)    VALUE " ACT".
           03  FILLER        PIC X(1)    VALUE SPACES.
           03  FILLER        PIC X(3)    VALUE "CON".
           03  FILLER        PIC X(1)    VALUE SPACES.
           03  FILLER        PIC X(3)    VALUE "REC".
           03  FILLER        PIC X(3)    VALUE SPACES.
           03  FILLER        PIC X(8)    VALUE "PAST-DUE".
           03  FILLER        PIC X(11)   VALUE SPACES.
       01  DISPLAY-LIST.
           03  FILLER      PIC X(40)  VALUE
               "010 020 030 040 050 060 070 080 090 100 ".
           03  FILLER      PIC X(35)  VALUE
               "110 120 130 140 150 PGNO ENDMORE:F.".
       01  DISPLAY-BUF.
           03  D-LINE                 OCCURS 15 TIMES.
               05  D-DATE             PIC 99/99/99.
               05  FILLER             PIC X(1).
               05  D-ACCTNO           PIC Z(5)9.
               05  FILLER             PIC X(1).
               05  D-BW-NAME          PIC X(10).
               05  FILLER             PIC X(1).
               05  D-ACTIVITY-CD      PIC X(3).
               05  FILLER             PIC X(1).
               05  D-RESULT-CD        PIC X(3).
               05  FILLER             PIC X(1).
               05  D-PROMISE-DATE1    PIC 99/99/99 BLANK WHEN ZERO.
               05  FILLER             PIC X(1).
               05  D-PROMISE-AMT      PIC ZZZZZ9.99 BLANK WHEN ZERO.
               05  FILLER             PIC X(1).
               05  D-ACTION-CD        PIC X(4).
               05  FILLER             PIC X(1).
               05  D-CON-STAT         PIC X(3).
               05  FILLER             PIC X(1).
               05  D-REC-STAT         PIC X(3).
               05  FILLER             PIC X(1).
               05  D-PASTDUE-BAL      PIC ZZZZZ9.99.

           03  D-PAGENO               PIC ZZ9.
           03  D-END-MORE             PIC X(6).

       01  SUMMARY-HEADING.
           03  FILLER        PIC X(8)     VALUE " RESULT ".
           03  FILLER        PIC X(8)     VALUE "NUMBER  ".
           03  FILLER        PIC X(14)    VALUE "   TOT BALANCE".
           03  FILLER        PIC X(14)    VALUE "  TOT PROMISED".
           03  FILLER        PIC X(44)    VALUE SPACES.

       01  SUMMARY-LIST.
           03  FILLER      PIC X(40)  VALUE
               "010 020 030 040 050 060 070 080 090 100 ".
           03  FILLER      PIC X(20)  VALUE
               "110 120 130 140 150.".

       01  SUMMARY-BUF.
           03  SUMMARY-TAB OCCURS 15.
               05  FILLER            PIC XX.
               05  D-SUM-RESULT      PIC X(6).
               05  D-SUM-COUNT       PIC ZZZZZ9.
               05  FILLER            PIC XX.
               05  D-SUM-CUR-BAL     PIC ZZZ,ZZZ,ZZZ.99.
               05  D-SUM-PROMISE-AMT PIC ZZZ,ZZZ,ZZZ.99.
               05  FILLER            PIC X(32).

       01  SUMMARY-ALL-HEADING.
           03  FILLER             PIC X(8)     VALUE " RESULT ".
           03  SUM-HEAD-TABLE     OCCURS 9.
               05  FILLER         PIC X(4)     VALUE SPACES.
               05  SUM-HEAD-COLID.
                   07  COLID-HEAD-TAB PIC X OCCURS 3.
           03  FILLER             PIC X(5)     VALUE SPACES.

       01  SUMMARY-ALL-BUF.
           03  SUMMARY-ALL-TAB OCCURS 15.
               05  FILLER            PIC XX.
               05  D-ALL-RESULT      PIC X(6).
               05  D-ALL-COUNT       PIC ZZZZZZZ OCCURS 9.
               05  FILLER            PIC X(5).

       01  PAGE-KEY-TABLE.
           03  PAGE-KEY               PIC X(21) OCCURS 600.
       01  CURRENT-PAGE               PIC S999.
       01  PAGE-COUNT                 PIC 999.
       01  MAX-PAGES                  PIC 9(4)  VALUE 600.


       01  GROUP-FLAG        PIC X.
       COPY "LIBGB/GRWORKW.CPY".

       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/PHON400.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBWI/CDBSCNW.CPY".
       COPY "LIBGB/FLDSELW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       01  CJINQ-WINDOW-HANDLE      PIC X(10).
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/CJINQ_DEF.CPY".
       COPY "LIBWI/CJINQ_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBWI/CJINQ_SCN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE
               WK-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-CJ1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-CJ1-CONNECTION.
           CLOSE
               WK-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      ****************************
       MAIN-PROGRAM SECTION.
      ****************************

           PERFORM SQL-CONNECT.

      D    STOP MESS.

           MOVE " " TO VDU-WIMENU-SELECTION.

           MOVE "CJINQ" TO VDU-FORM-NAME.

           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO HOLD-SYSDATE.

       COPY "LIBGB/CHKSEC.CPY".

              DISPLAY FLOATING GRAPHICAL WINDOW
                      AT  SCREEN LINE CJINQ-BEGIN-X * VDU-WIN-L-OFFSET
                      AT  SCREEN COL  CJINQ-BEGIN-Y * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                      SIZE    CJINQ-COLUMNS + VDU-WIN-W-MOD
                      LINES   CJINQ-LINES + VDU-WIN-H-MOD
                      BLANK SCREEN
                      HANDLE IS CJINQ-WINDOW-HANDLE.

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           PERFORM OPEN-WK-FILE-OUTPUT.
           PERFORM CLOSE-WK-FILE.

       MAIN-MODULE.
           PERFORM FORM-RESET.
           PERFORM OPEN-WK-FILE-OUTPUT.
           PERFORM CLOSE-WK-FILE.

           PERFORM REC-INQ-RANGES.
           IF IO-TYPE = "END"
              GO TO END-ROUTINE.
           IF IO-TYPE = "CLR"
              GO TO MAIN-MODULE.

           IF INQ-TYPE = "S"
              PERFORM SUMMARY-DISPLAY
           ELSE
              PERFORM INQUIRY-DISPLAY.

           GO TO MAIN-MODULE.

      ***************************
       REC-INQ-RANGES SECTION.
      ***************************
       REC-RANGES.
           MOVE SPACES TO IO-TYPE.
           MOVE "Y" TO RESET-FG.

           MOVE "SN N040BR1." TO VDU.
           MOVE EXT-CONAME-USER-LOC TO VDUNUM0 BEG-BRANCH END-BRANCH.
           PERFORM SCREENIO.

       BR-01.
           MOVE "F7, ENTER BRANCH #:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENNN040BR1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-REC-RANGES.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BR-01.

           MOVE VDUNUM0 TO BEG-BRANCH END-BRANCH.

           PERFORM GET-BR.
           IF IO-FG NOT = 0
              GO TO BR-01.

           MOVE EXT-FILPATH-BASE TO FILPATH-BASE.
           MOVE BR-MACHINE       TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH     TO FILPATH-DATA.

           MOVE "B"        TO CJ-PATH-OVERRIDE
                              LN-PATH-OVERRIDE
                              BW-PATH-OVERRIDE.

           PERFORM BR-RANGE-SECURITY.
           IF BR-SECURITY-OKAY = "N"
              GO TO BR-01.

       SEND-DEFAULT-DATES.
           MOVE "S  A080DATE1." TO VDU.
           MOVE HOLD-SYSDATE TO DATE-YYYYMMDD DATE1 DATE2.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO VDU-DATE-EDIT.
           PERFORM SCREENIO.
           MOVE "S  A080DATE2." TO VDU.
           MOVE DATE-MMDDYY TO VDU-DATE-EDIT.
           PERFORM SCREENIO.

       BEG-DATE.
           MOVE "F1, ENTER BEGINNING DATE :" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ERNM060DATE1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              GO TO REC-RANGES.
           IF VDUSTATUS = "1U"
              GO TO BR-01.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-REC-RANGES.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO BEG-DATE.

           MOVE VDU-DATE-YYYYMMDD TO DATE1.
           IF VDUNUM0 = 0
              MOVE "S  A080DATE1." TO VDU
              MOVE HOLD-SYSDATE TO DATE-YYYYMMDD DATE1
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO VDU-DATE-EDIT
              PERFORM SCREENIO.

       ENTER-DATE2.
           MOVE 0 TO DATE2.
           MOVE "F1, ENTER ENDING DATE:" TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "ERNM060DATE2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              GO TO REC-RANGES.
           IF VDUSTATUS = "1U"
              GO TO BEG-DATE.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-REC-RANGES.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ENTER-DATE2.

           MOVE VDU-DATE-YYYYMMDD TO DATE2.
           IF VDUNUM0 = 0
              MOVE "S  A080DATE2." TO VDU
              MOVE HOLD-SYSDATE TO DATE-YYYYMMDD DATE2
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO VDU-DATE-EDIT
              PERFORM SCREENIO.

           IF DATE2 < DATE1
              GO TO ENTER-DATE2.

       ENTER-COLID.
           MOVE
            "F1, F3-ALL COLLECTORS, F6-SCAN, ENTER COLLECTOR ID:"
                                                 TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E  A000COLID." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              GO TO REC-RANGES.
           IF VDUSTATUS = "1U"
              GO TO BEG-DATE.
           IF VDUSTATUS = "16"
              MOVE "~~~" TO COL-ID
              MOVE "S  A000COLID." TO VDU
              MOVE "ALL" TO VDUBUF
              PERFORM SCREENIO
              GO TO ENTER-TYPE.
           IF VDUSTATUS = "1<"
              MOVE "CN"                TO CDBSCN-TYPE
              MOVE EXT-CONAME-USER-LOC TO CDBSCN-BRANCH
              MOVE SPACES              TO CDBSCN-CODE
              MOVE "WI/CDBSCN" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH CDBSCN-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              IF ( CDBSCN-STATUS NOT = "00") OR (CDBSCN-CODE = SPACES) 
                 GO TO ENTER-COLID
              ELSE
                 MOVE "S  A000COLID." TO VDU
                 MOVE CDBSCN-CODE  TO VDUBUF COL-ID
                 PERFORM SCREENIO
                 GO TO ENTER-TYPE.

           IF VDUSTATUS = "1>"
             MOVE "END" TO IO-TYPE
             GO TO END-REC-RANGES.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ENTER-COLID.

           MOVE VDUBUF TO COL-ID.

       ENTER-TYPE.
           MOVE "F1, D-DETAIL, S-SUMMARY:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E  A000INQTYPE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              GO TO REC-RANGES.
           IF VDUSTATUS = "1U"
              GO TO ENTER-COLID.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-REC-RANGES.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ENTER-TYPE.
           IF VDUBUF NOT = "D"
              IF VDUBUF NOT = "S"
                 MOVE "PLEASE ENTER D OR S" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-TYPE.
           IF COL-ID = "~~~"
              IF VDUBUF NOT = "S"
                 MOVE "PLEASE USE SUMMARY OPTION FOR ALL COLLECTORS"
                                                         TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-TYPE.

           MOVE VDUBUF TO INQ-TYPE.

       END-REC-RANGES.
           EXIT.

      *************************************************
       INQUIRY-DISPLAY SECTION.
      *************************************************

           MOVE BEG-BRANCH TO CJ-BRNO.
           MOVE COL-ID     TO CJ-COLLID.
           MOVE DATE1      TO CJ-DATE.
           MOVE 0          TO CJ-START-TIME CJ-SEQNO.
           MOVE FIRST-FIELD TO FLDSEL-FIELD.
           MOVE 1 TO PAGE-COUNT ONE.
           MOVE 0 TO CURRENT-PAGE.

           PERFORM OPEN-CJ1-FILE.
           PERFORM OPEN-LN1-FILE.
           PERFORM OPEN-BW1-FILE.

           MOVE CJ1-KEY TO PAGE-KEY(PAGE-COUNT).

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
           MOVE PAGE-KEY(CURRENT-PAGE) TO CJ1-KEY.

           IF CJ-DATE = 0
              MOVE 19000101 TO CJ-DATE.

      D    STOP MESS.

           MOVE BR-NO TO QCJ1-WBEG-BRNO.

           MOVE COL-ID TO QCJ1-WBEG-COLLID
                          QCJ1-WEND-COLLID.

           MOVE DATE1     TO SQL-DATE-YYYYMMDD.
           IF DATE1 = 0
              MOVE 19000101 TO CJ-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QCJ1-WBEG-DATE.

           MOVE DATE2     TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QCJ1-WEND-DATE.

           MOVE 'D' TO PASS-DETSUM.
           IF CURRENT-PAGE = 1 
              MOVE 0 TO PASS-OFFSET
           ELSE
              COMPUTE PASS-OFFSET = (CURRENT-PAGE - 1) * 15.

           PERFORM START-CJ1-FILE.   *> SSP CJ LN BW  

      *************************************
       BUILD-PAGENO.
           PERFORM READ-CJ1-FILE-NEXT.  *> SSP CJ LN BW
           IF IO-FG NOT = 0             *> OR (CJ-BRNO NOT = CJ-PATH-OWNBR)
              PERFORM CLOSE-CJ1-FILE
              GO TO DISPLAY-PAGE.

           IF SUB > 15
              PERFORM CLOSE-CJ1-FILE
              GO TO DISPLAY-PAGE.

      * ALSO CHECKED IN SSP
           IF CJ-BRNO > END-BRANCH
              MOVE 9 TO IO-FG
              PERFORM CLOSE-CJ1-FILE
              GO TO DISPLAY-PAGE.

      * ALSO CHECKED IN SSP
           IF CJ-COLLID NOT = COL-ID
              MOVE 9 TO IO-FG
              PERFORM CLOSE-CJ1-FILE
              GO TO DISPLAY-PAGE.

      * ALSO CHECKED IN SSP
           IF CJ-DATE < DATE1 OR CJ-DATE > DATE2
              GO TO BUILD-PAGENO.

       MOVE-FIELDS.
           MOVE CJ-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY      TO D-DATE(SUB).
           MOVE CJ-ACCTNO        TO D-ACCTNO(SUB) LN-ACCTNO.
      *     PERFORM READ-LN1-FILE.
      *     IF ( IO-FG = 0 )
      *        MOVE LN-SSNO(1) TO BW-SSNO
      *        PERFORM READ-BW1-FILE
      *           IF ( IO-FG = 0 )
      *              MOVE BW-LNAME TO D-BW-NAME(SUB)
      *           ELSE
      *              MOVE "** NONE **" TO D-BW-NAME(SUB)
      *        ELSE
      *           MOVE "** NONE **" TO D-BW-NAME(SUB).
           IF BW-LNAME = "???"
              MOVE "** NONE **" TO D-BW-NAME(SUB)
           ELSE
              MOVE BW-LNAME TO D-BW-NAME(SUB).

           MOVE CJ-ACTIVITY-CD   TO D-ACTIVITY-CD(SUB).
           MOVE CJ-RESULT-CD     TO D-RESULT-CD(SUB).
           MOVE CJ-PROMISE-DATE1 TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY      TO D-PROMISE-DATE1(SUB).
           MOVE CJ-PROMISE-AMT   TO D-PROMISE-AMT(SUB).
           MOVE CJ-ACTION-CD     TO D-ACTION-CD(SUB).
           MOVE CJ-CON-STAT      TO D-CON-STAT(SUB).
           MOVE CJ-REC-STAT      TO D-REC-STAT(SUB).
           MOVE CJ-PAST-DUE      TO D-PASTDUE-BAL(SUB).

           ADD 1 TO SUB.
           GO TO BUILD-PAGENO.

      *************************************
      *    DISPLAY PAGE
      *************************************
       DISPLAY-PAGE.
           MOVE DETAIL-HEADING TO HEADING-BUF.
           MOVE HEADING-BUF TO WR-BUF.
           MOVE HEADING-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
           IF IO-FG NOT = 0
              IF SUB = 1
                 MOVE "NO RECORDS FOUND!" TO MESS
                 PERFORM SEND-MESS
                 MOVE DISPLAY-BUF TO WR-BUF
                 MOVE DISPLAY-LIST TO WR-LIST
                 PERFORM CALL-WRFORM
                 GO TO END-INQUIRY-MODULE.
           MOVE "MORE.." TO D-END-MORE.
           IF CURRENT-PAGE = PAGE-COUNT
              IF IO-FG = 0
                 IF PAGE-COUNT < MAX-PAGES
                    ADD 1 TO PAGE-COUNT
                    MOVE CJ1-KEY TO PAGE-KEY(PAGE-COUNT)
                 ELSE
                    MOVE "MAXIMUM PAGES REACHED!" TO MESS
                    PERFORM SEND-LEGEND
                    MOVE "OVRFLO" TO D-END-MORE
              ELSE
                 MOVE "*END*" TO D-END-MORE.
           MOVE CURRENT-PAGE TO D-PAGENO.
           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *************************************
      *    SELECT FIELD FROM PAGES
      *************************************
       SELECT-FIELD-FROM-DISPLAY.
           MOVE "F1, F7-EXIT, PGUP, PGDOWN:" TO MESS.
           PERFORM SEND-LEGEND.
           PERFORM CALL-FLDSEL.

      * TEST UP ARROW:
           IF FLDSEL-STAT = "1U"
              IF CURRENT-PAGE = 1
                 GO TO SELECT-FIELD-FROM-DISPLAY
              ELSE
                 MOVE -1 TO ONE
                 MOVE LAST-FIELD TO FLDSEL-FIELD
                 GO TO START-BUILD-PAGENO.

      * TEST PAGE UP:
           IF FLDSEL-STAT = "1P"
              IF CURRENT-PAGE = 1
                 GO TO SELECT-FIELD-FROM-DISPLAY
              ELSE
                 MOVE -1 TO ONE
                 MOVE FIRST-FIELD TO FLDSEL-FIELD
                 GO TO START-BUILD-PAGENO.

      * TEST PAGE DOWN OR DOWN ARROW:
           IF FLDSEL-STAT = "1N" OR FLDSEL-STAT = "1D"
              IF CURRENT-PAGE NOT < PAGE-COUNT
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
              MOVE "N" TO RESET-FG
              GO TO END-INQUIRY-MODULE.
           IF NOT (FLDSEL-STAT = "00" OR "1<")
              GO TO SELECT-FIELD-FROM-DISPLAY.

           GO TO SELECT-FIELD-FROM-DISPLAY.

       END-INQUIRY-MODULE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-BW1-FILE.

      ************************************************
       SUMMARY-DISPLAY SECTION.
      ************************************************

           MOVE 0 TO TOT-SUM-COUNT
                     TOT-SUM-CUR-BAL
                     TOT-SUM-PROMISE-AMT
                     OTHER-SUM-COUNT
                     OTHER-SUM-CUR-BAL
                     OTHER-SUM-PROMISE-AMT.

           MOVE ZEROS TO OTHER-ALL-TABLE TOTAL-ALL-TABLE.
           MOVE SPACES TO SUMMARY-BUF SUMMARY-ALL-BUF.

           PERFORM OPEN-CJ1-FILE.

      *     MOVE CJ-PATH-OWNBR TO CJ-BRNO
      *                           QCJ1-WBEG-BRNO
      *                           QCJ1-WEND-BRNO.

           MOVE BR-NO TO QCJ1-WBEG-BRNO.
           IF COL-ID = "~~~"
              MOVE SPACES  TO CJ-COLLID 
                              QCJ1-WBEG-COLLID
              MOVE ALL "Z" TO QCJ1-WEND-COLLID
           ELSE
              MOVE COL-ID TO CJ-COLLID
                             QCJ1-WBEG-COLLID
                             QCJ1-WEND-COLLID.

      *     MOVE QCJ1-WBEG-COLLID TO QCJ1-WSBEG-COLLID.
           MOVE DATE1     TO CJ-DATE SQL-DATE-YYYYMMDD.
           IF DATE1 = 0
              MOVE 19000101 TO CJ-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QCJ1-WBEG-DATE
                                       QCJ1-WSBEG-DATE.

           MOVE DATE2     TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QCJ1-WEND-DATE.

      *     MOVE 0         TO CJ-START-TIME CJ-SEQNO
      *                       QCJ1-WBEG-START-TIME
      *                       QCJ1-WSBEG-START-TIME
      *                       QCJ1-WBEG-SEQNO
      *                       QCJ1-WSBEG-SEQNO.
      *     MOVE ALL "9"   TO QCJ1-WEND-START-TIME
      *                       QCJ1-WEND-SEQNO.

           MOVE 0   TO PASS-OFFSET.
           MOVE 'S' TO PASS-DETSUM.

           PERFORM OPEN-WK-FILE.

      D    STOP MESS.
           PERFORM START-CJ1-FILE.

       NEXT-SUMMARY-CJ.
           PERFORM READ-CJ1-FILE-NEXT.  *> SSP  CJFILE
           IF IO-FG NOT = 0             *> OR (CJ-BRNO NOT = CJ-PATH-OWNBR)
              GO TO RESEQUENCE-WK-FILE.

      * ALSO CHECKED IN SSP
           IF CJ-BRNO > END-BRANCH
              MOVE 9 TO IO-FG
              GO TO RESEQUENCE-WK-FILE.

      * ALSO CHECKED IN SSP
           IF CJ-COLLID NOT = COL-ID
              IF COL-ID NOT = "~~~"
                 MOVE 9 TO IO-FG
                 GO TO RESEQUENCE-WK-FILE.

      * ALSO CHECKED IN SSP
           IF CJ-DATE < DATE1 OR CJ-DATE > DATE2
              GO TO NEXT-SUMMARY-CJ.

           MOVE CJ-RESULT-CD TO WK-RESULT-CODE.
           MOVE CJ-COLLID    TO WK-COLLID.
           PERFORM READ-WK-FILE.
           IF IO-FG = 0
              ADD 1              TO WK-COUNT
              ADD CJ-PROMISE-AMT TO WK-PROMISE-AMT
              ADD CJ-CUR-BAL     TO WK-CUR-BAL
              PERFORM REWRITE-WK-FILE
           ELSE
              MOVE 1              TO WK-COUNT
              MOVE CJ-PROMISE-AMT TO WK-PROMISE-AMT
              MOVE CJ-CUR-BAL     TO WK-CUR-BAL
              PERFORM WRITE-WK-FILE
              IF IO-FG NOT = 0
                 GO TO IO-ERROR.

           IF COL-ID NOT = "~~~"
              GO TO NEXT-SUMMARY-CJ.

           MOVE CJ-RESULT-CD TO WK-RESULT-CODE.
           MOVE "~~~"        TO WK-COLLID.
           PERFORM READ-WK-FILE.
           IF IO-FG = 0
              ADD 1              TO WK-COUNT
              ADD CJ-PROMISE-AMT TO WK-PROMISE-AMT
              ADD CJ-CUR-BAL     TO WK-CUR-BAL
              PERFORM REWRITE-WK-FILE
           ELSE
              MOVE 1              TO WK-COUNT
              MOVE CJ-PROMISE-AMT TO WK-PROMISE-AMT
              MOVE CJ-CUR-BAL     TO WK-CUR-BAL
              PERFORM WRITE-WK-FILE
              IF IO-FG NOT = 0
                 GO TO IO-ERROR.

           MOVE "~~~"     TO WK-RESULT-CODE.
           MOVE CJ-COLLID TO WK-COLLID.
           PERFORM READ-WK-FILE.
           IF IO-FG = 0
              ADD 1              TO WK-COUNT
              ADD CJ-PROMISE-AMT TO WK-PROMISE-AMT
              ADD CJ-CUR-BAL     TO WK-CUR-BAL
              PERFORM REWRITE-WK-FILE
           ELSE
              MOVE 1              TO WK-COUNT
              MOVE CJ-PROMISE-AMT TO WK-PROMISE-AMT
              MOVE CJ-CUR-BAL     TO WK-CUR-BAL
              PERFORM WRITE-WK-FILE
              IF IO-FG NOT = 0
                 GO TO IO-ERROR.

           GO TO NEXT-SUMMARY-CJ.

       RESEQUENCE-WK-FILE.
           PERFORM CLOSE-WK-FILE.
           PERFORM OPEN-WK-FILE.
           PERFORM CLOSE-CJ1-FILE.

       NEXT-WK-RESEQUENCE.
           PERFORM READ-WK-FILE-NEXT.
           IF IO-FG = 9
              GO TO DISPLAY-SUMMARY-PAGE.

           COMPUTE WK-REV-COUNT = 999999 - WK-COUNT.
           PERFORM REWRITE-WK-FILE.

           GO TO NEXT-WK-RESEQUENCE.
       DISPLAY-SUMMARY-PAGE.
           IF COL-ID = "~~~"
              GO TO DISPLAY-SUMMARY-ALL.

           MOVE 0 TO WK-REV-COUNT.
           MOVE SPACES TO WK-RESULT-CODE WK-COLLID.

           PERFORM START-WK2-FILE.
           MOVE 1 TO SUB.
       NEXT-WK.
           PERFORM READ-WK2-FILE-NEXT.
           IF IO-FG = 9
              GO TO TOTAL-WK.

           IF SUB < 14
              MOVE WK-RESULT-CODE TO D-SUM-RESULT(SUB)
              MOVE WK-COUNT TO D-SUM-COUNT(SUB)
              MOVE WK-CUR-BAL TO D-SUM-CUR-BAL(SUB)
              MOVE WK-PROMISE-AMT TO D-SUM-PROMISE-AMT(SUB)
              ADD WK-COUNT TO TOT-SUM-COUNT
              ADD WK-PROMISE-AMT TO TOT-SUM-PROMISE-AMT
              ADD WK-CUR-BAL TO TOT-SUM-CUR-BAL
              IF D-SUM-RESULT(SUB) = " "
                 MOVE "NONE" TO D-SUM-RESULT(SUB)
              END-IF
              ADD 1 TO SUB
           ELSE
              ADD WK-COUNT TO OTHER-SUM-COUNT
              ADD WK-PROMISE-AMT TO OTHER-SUM-PROMISE-AMT
              ADD WK-CUR-BAL TO OTHER-SUM-CUR-BAL.
           IF D-SUM-RESULT(SUB) = " "
              MOVE "NONE" TO D-SUM-RESULT(SUB).
           IF SUB < 15
              GO TO NEXT-WK.
       TOTAL-WK.
           IF SUB = 14
              MOVE "OTHER" TO D-SUM-RESULT(SUB)
              MOVE OTHER-SUM-COUNT TO D-SUM-COUNT(SUB)
              MOVE OTHER-SUM-CUR-BAL TO D-SUM-CUR-BAL(SUB)
              MOVE OTHER-SUM-PROMISE-AMT TO D-SUM-PROMISE-AMT(SUB)
              ADD OTHER-SUM-COUNT TO TOT-SUM-COUNT
              ADD OTHER-SUM-PROMISE-AMT TO TOT-SUM-PROMISE-AMT
              ADD OTHER-SUM-CUR-BAL TO TOT-SUM-CUR-BAL
              ADD 1 TO SUB.

           MOVE "TOTAL:" TO D-SUM-RESULT(SUB).
           MOVE TOT-SUM-COUNT TO D-SUM-COUNT(SUB).
           MOVE TOT-SUM-CUR-BAL TO D-SUM-CUR-BAL(SUB).
           MOVE TOT-SUM-PROMISE-AMT TO D-SUM-PROMISE-AMT(SUB).
           MOVE SUMMARY-HEADING TO HEADING-BUF.

           MOVE HEADING-BUF  TO WR-BUF.
           MOVE HEADING-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE SUMMARY-BUF  TO WR-BUF.
           MOVE SUMMARY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           GO TO SUMMARY-RESPONSE.

       DISPLAY-SUMMARY-ALL.
           MOVE 0 TO WK-REV-COUNT COL-COUNT.
           MOVE SPACES TO COLLECTOR-TABLE.
           MOVE SPACES TO WK-RESULT-CODE WK-COLLID.
           PERFORM START-WK2-FILE.
       NEXT-SET-COL-SUB.
           IF COL-COUNT > 6
              GO TO START-ALL-TOTAL-WK.
           PERFORM READ-WK2-FILE-NEXT.
           IF IO-FG = 9
              GO TO START-ALL-TOTAL-WK.

           IF WK-COLLID = "~~~"
              GO TO NEXT-SET-COL-SUB.
           IF WK-RESULT-CODE NOT = "~~~"
              GO TO NEXT-SET-COL-SUB.
           MOVE 1 TO COL-WORK.
       CHECK-COL-TABLE.
           IF COLLECTOR-TAB(COL-WORK) = SPACES
              MOVE WK-COLLID TO COLLECTOR-TAB(COL-WORK)
              PERFORM SETUP-COLLECTOR-HEAD
              ADD 1 TO COL-COUNT
              GO TO NEXT-SET-COL-SUB.
           IF WK-COLLID = COLLECTOR-TAB(COL-WORK)
              GO TO NEXT-SET-COL-SUB.
           IF COL-WORK < 7
              ADD 1 TO COL-WORK
              GO TO CHECK-COL-TABLE.
           GO TO NEXT-SET-COL-SUB.
       START-ALL-TOTAL-WK.
           MOVE "OTHER" TO SUM-HEAD-COLID(8).
           MOVE "TOTAL" TO SUM-HEAD-COLID(9).
           MOVE ZEROS TO OTHER-ALL-TABLE.
           MOVE 0 TO WK-REV-COUNT.
           MOVE SPACES TO WK-RESULT-CODE WK-COLLID.
           PERFORM START-WK2-FILE.
           MOVE 0 TO SUB.
       NEXT-WK-SET-SUB.
           PERFORM READ-WK2-FILE-NEXT.
           IF IO-FG = 9
              GO TO SUM-ALL-TOTAL-WK.
           IF WK-COLLID NOT = "~~~"
              GO TO NEXT-WK-SET-SUB.
           IF WK-RESULT-CODE = "~~~"
              GO TO NEXT-WK-SET-SUB.
           MOVE WK-RESULT-CODE TO HOLD-RESULT-CODE.
           MOVE WK2-KEY TO HOLD-WK2-KEY.
           MOVE 0 TO OTHER-COL-COUNT.

       SUMMARY-ALL-START-WK.
           IF SUB < 14
              ADD 1 TO SUB.
           MOVE 1 TO COLLECTOR-SUB.
           MOVE SPACES TO WK-COLLID.
           MOVE HOLD-RESULT-CODE TO WK-RESULT-CODE.
           PERFORM START-WK-FILE.
       NEXT-WK-SUM-ALL.
           PERFORM READ-WK-FILE-NEXT.
           IF IO-FG = 9 OR WK-RESULT-CODE NOT = HOLD-RESULT-CODE
              MOVE OTHER-COL-COUNT TO D-ALL-COUNT(SUB, 8)
              MOVE HOLD-WK2-KEY TO WK2-KEY
              PERFORM START-WK2-FILE-GREATER
              GO TO NEXT-WK-SET-SUB.
           IF WK-COLLID = "~~~"
              MOVE 9 TO COLLECTOR-SUB
           ELSE
              PERFORM FIND-COLLECTOR-SUB.

           IF SUB < 14
              MOVE WK-RESULT-CODE TO D-ALL-RESULT(SUB)
              MOVE WK-COUNT TO D-ALL-COUNT(SUB, COLLECTOR-SUB)
              ADD WK-COUNT TO TOT-SUM-COUNT
              IF D-ALL-RESULT(SUB) = " "
                 MOVE "NONE" TO D-ALL-RESULT(SUB)
              END-IF
              IF COLLECTOR-SUB = 8
                 ADD WK-COUNT TO OTHER-COL-COUNT
              END-IF
           ELSE
              ADD WK-COUNT TO OTHER-ALL-COUNT(COLLECTOR-SUB).
           ADD WK-COUNT TO TOTAL-ALL-COUNT(COLLECTOR-SUB).
           GO TO NEXT-WK-SUM-ALL.
       SUM-ALL-TOTAL-WK.
           IF SUB NOT = 14
              GO TO SUM-ALL-TOTAL-LINE.
           MOVE "OTHER" TO D-ALL-RESULT(SUB).
           MOVE 1 TO COLLECTOR-SUB.
       SUM-ALL-OTHER-LOOP.
           MOVE OTHER-ALL-COUNT(COLLECTOR-SUB)
                TO D-ALL-COUNT(SUB, COLLECTOR-SUB).
           IF COLLECTOR-SUB < 9
              ADD 1 TO COLLECTOR-SUB
              GO TO SUM-ALL-OTHER-LOOP.
       SUM-ALL-TOTAL-LINE.
           ADD 1 TO SUB.
           MOVE "TOTAL:" TO D-ALL-RESULT(SUB).
           MOVE 1 TO COLLECTOR-SUB.
       SUM-ALL-TOTAL-LOOP.
           MOVE TOTAL-ALL-COUNT(COLLECTOR-SUB)
                TO D-ALL-COUNT(SUB, COLLECTOR-SUB).
           IF COLLECTOR-SUB < 9
              ADD 1 TO COLLECTOR-SUB
              GO TO SUM-ALL-TOTAL-LOOP.
           MOVE SUMMARY-HEADING TO HEADING-BUF.
           MOVE SUMMARY-ALL-HEADING TO WR-BUF.
           MOVE HEADING-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
           MOVE SUMMARY-ALL-BUF TO WR-BUF.
           MOVE SUMMARY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       SUMMARY-RESPONSE.
           MOVE "PRESS <ENTER> TO CONTINUE." TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E  A000SEL." TO VDU.
           PERFORM SCREENIO.

      *************************************
       FIND-COLLECTOR-SUB SECTION.
      *************************************
           MOVE 1 TO COL-WORK.
       FIND-COL-SUB.
           IF WK-COLLID = COLLECTOR-TAB(COL-WORK)
              MOVE COL-WORK TO COLLECTOR-SUB
              GO TO EXIT-FIND-COL-SUB.
           IF COL-WORK < 7
              ADD 1 TO COL-WORK
              GO TO FIND-COL-SUB
           ELSE
              MOVE 8 TO COLLECTOR-SUB.
       EXIT-FIND-COL-SUB.
           EXIT.

      *************************************
       SETUP-COLLECTOR-HEAD SECTION.
      *************************************
           MOVE 3 TO IN-SUB OUT-SUB.
       SETUP-COL-HEAD.
           IF WK-COLLID-TAB(IN-SUB) NOT = " "
              MOVE WK-COLLID-TAB(IN-SUB) TO
                      COLID-HEAD-TAB(COL-WORK OUT-SUB)
              SUBTRACT 1 FROM OUT-SUB.
           SUBTRACT 1 FROM IN-SUB.
           IF IN-SUB > 0
              GO TO SETUP-COL-HEAD.
       EXIT-SETUP-COL-HEAD.
           EXIT.

      *--------------------------------------------
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/BRSECURE.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/FLDSEL.CPY".

      ***********************************************
       FORM-RESET SECTION.
      ***********************************************
           MOVE EXT-CONAME-CONAME TO CJINQ-CONAME.
           MOVE EXT-CONAME-SYSDATE TO CJINQ-SYSDATE.
           DISPLAY CJINQ-SCREEN UPON CJINQ-WINDOW-HANDLE.
           MOVE CJINQ-FORM-FIELDS TO WR-BUF.
           MOVE CJINQ-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ***********************************************
       SETUP-LINK-INFO SECTION.
      ***********************************************
           MOVE CJINQ-HELP--FILE TO HELP-LINK-FILE.
           MOVE CJINQ-HELP--DIR TO HELP-LINK-DIR.

      ***********************************************
       VDU-CONVERT-FIELD SECTION.
      ***********************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/CJINQ_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      **********************************
       MISC-PARAGRAPHS SECTION.
      **********************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/RMFILE.CPY".
       GET-BR.
           MOVE BEG-BRANCH TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
       SEND-LEGEND.
           MOVE MESS TO WR-BUF.
           MOVE MSEL-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      **********************************
       IO-ROUTINES SECTION.
      **********************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBSP/SPCJGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".

      * COPY "LIBSP/SPCJ1RN.CPY".
      *-----------------------------------------------------------------
      * COPYMEMBER: LIBSP/SPCJ1RN
       LOAD-CJ1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( CJ-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( CJ-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO CJ-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO CJ-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO CJ-ALL-DATA
              MOVE CJ-ALL-PATH             TO CJ-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( CJ-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO CJ-ALL-BASE
              MOVE FILPATH-MACHINE         TO CJ-ALL-MACHINE
              MOVE FILPATH-DATA            TO CJ-ALL-DATA
              MOVE CJ-ALL-PATH             TO CJ-PATH.

      *-----------------------------------------------------------------
       OPEN-CJ1-FILE.
           PERFORM LOAD-CJ1-FILE.
           PERFORM OPEN-IT.
           MOVE CJ-PATH-SQL TO E-FILE.

           IF CJ1-CONN-OPEN-SW = "N" 
               EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS CJ1_CONNECT
                    USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
               END-EXEC
               MOVE "Y" TO CJ1-CONN-OPEN-SW
               EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       START-CJ1-FILE.    *> SSP - CJ

           PERFORM START-IT.
           MOVE CJ-PATH-OWNBR  TO CJ-BRNO.
           MOVE CJ-PATH-SQL    TO E-FILE.
           MOVE CJ1-KEY        TO E-KEYX.

           IF ( CJ1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-CJ1-FILE.

           EXEC SQL SET CONNECTION CJ1_CONNECT END-EXEC.

           EXEC SQL
            DECLARE CJ1_CURSOR CURSOR FOR
                   :CJ1-RETURN-CODE = 
            EXEC SSP_CJINQ_CJFILE_SELECT
                (:QCJ1-WBEG-BRNO,  :QCJ1-WBEG-COLLID,
                 :QCJ1-WEND-COLLID,:QCJ1-WBEG-DATE,
                 :QCJ1-WEND-DATE,  :PASS-OFFSET, :PASS-DETSUM)
            END-EXEC.

           EXEC SQL
               OPEN CJ1_CURSOR 
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE STAT---GOOD TO CJ1-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       READ-CJ1-FILE-NEXT.    *> SSP - CJ 
           PERFORM READ-IT.
           MOVE CJ-PATH-OWNBR  TO CJ-BRNO.
           MOVE CJ-PATH-SQL    TO E-FILE.
           MOVE CJ1-KEY        TO E-KEYX.
           INITIALIZE QCJ-REC QBW-REC.

           EXEC SQL SET CONNECTION CJ1_CONNECT END-EXEC.

           IF ( CJ1-CURSOR-STAT-GOOD )
              EXEC SQL
                 FETCH CJ1_CURSOR INTO
                   :QCJ-BRNO,
                   :QCJ-COLLID, 
                   :QCJ-DATE,
                   :QCJ-START-TIME,
                   :QCJ-SEQNO,
                   :QCJ-ACCTNO,
                   :QCJ-ACTIVITY-CD, 
                   :QCJ-RESULT-CD, 
                   :QCJ-ACTION-CD, 
                   :QCJ-CON-STAT, 
                   :QCJ-REC-STAT, 
                   :QCJ-PROMISE-DATE1, 
                   :QCJ-PROMISE-AMT,
                   :QCJ-CUR-BAL,
                   :QCJ-PAST-DUE,
                   :QBW-LNAME
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDPVCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-CJ1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-CJ-FIELDS
              PERFORM GET-BW-FIELDS.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       CLOSE-CJ1-FILE.

           PERFORM CLOSE-IT.
           IF ( CJ1-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION CJ1_CONNECT END-EXEC
              EXEC SQL
                   CLOSE CJ1_CURSOR
              END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE STAT---BAD TO CJ1-CURSOR-STAT.

      *-----------------------------------------------------------------
       CLOSE-CJ1-CONNECTION.

           IF CJ1-CONN-OPEN-SW = "Y"
               EXEC SQL SET CONNECTION CJ1_CONNECT END-EXEC
               EXEC SQL COMMIT END-EXEC
               EXEC SQL DISCONNECT CJ1_CONNECT END-EXEC
               EXEC SQL SET CONNECTION C1 END-EXEC
               MOVE "N" TO CJ1-CONN-OPEN-SW.
      *-----------------------------------------------------------------

       OPEN-WK-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE WK-IFN TO E-FILE.
           OPEN OUTPUT WK-FILE.
           IF IO-FG = 8
              GO TO OPEN-WK-FILE-OUTPUT.
           IF IO-FG = 7
              CLOSE WK-FILE
              GO TO OPEN-WK-FILE-OUTPUT.
           UNLOCK WK-FILE.
       OPEN-WK-FILE.
           PERFORM OPEN-IT.
           MOVE WK-IFN TO E-FILE.
           OPEN I-O WK-FILE.
           IF IO-FG = 8
              GO TO OPEN-WK-FILE.
           IF IO-FG = 7
              CLOSE WK-FILE
              GO TO OPEN-WK-FILE.
           UNLOCK WK-FILE.
       READ-WK-FILE.
           PERFORM READ-IT.
           MOVE WK-IFN TO E-FILE.
           MOVE WK-KEY TO E-KEYX.
           READ WK-FILE.
           IF IO-FG = 8
              GO TO READ-WK-FILE.
       READ-WK-FILE-NEXT.
           PERFORM READ-IT.
           MOVE WK-IFN TO E-FILE.
           MOVE WK-KEY TO E-KEYX.
           READ WK-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-WK-FILE-NEXT.
       READ-WK-FILE-PREVIOUS.
           PERFORM READ-IT.
           MOVE WK-IFN TO E-FILE.
           MOVE WK-KEY TO E-KEYX.
           READ WK-FILE PREVIOUS.
           IF IO-FG = 8
              GO TO READ-WK-FILE-PREVIOUS.
       START-WK-FILE-LE.
           PERFORM START-IT.
           MOVE WK-IFN TO E-FILE.
           MOVE WK-KEY TO E-KEYX.
           START WK-FILE KEY <= WK-KEY.
           UNLOCK WK-FILE.
       START-WK-FILE-GREATER.
           PERFORM START-IT.
           MOVE WK-IFN TO E-FILE.
           MOVE WK-KEY TO E-KEYX.
           START WK-FILE KEY > WK-KEY.
           UNLOCK WK-FILE.
       START-WK-FILE.
           PERFORM START-IT.
           MOVE WK-IFN TO E-FILE.
           MOVE WK-KEY TO E-KEYX.
           START WK-FILE KEY NOT < WK-KEY.
           UNLOCK WK-FILE.
       WRITE-WK-FILE.
           PERFORM WRITE-IT.
           MOVE WK-IFN TO E-FILE.
           MOVE WK-KEY TO E-KEYX.
           WRITE WK-REC.
           IF IO-FG = 8
              GO TO WRITE-WK-FILE.
           UNLOCK WK-FILE.
       REWRITE-WK-FILE.
           PERFORM REWRITE-IT.
           MOVE WK-IFN TO E-FILE.
           MOVE WK-KEY TO E-KEYX.
           REWRITE WK-REC.
           IF IO-FG = 8
              GO TO REWRITE-WK-FILE.
           UNLOCK WK-FILE.
       DELETE-WK-FILE.
           PERFORM DELETE-IT.
           MOVE WK-IFN TO E-FILE.
           MOVE WK-KEY TO E-KEYX.
           DELETE WK-FILE.
           IF IO-FG = 8
              GO TO DELETE-WK-FILE.
           UNLOCK WK-FILE.
       CLOSE-WK-FILE.
           PERFORM CLOSE-IT.
           CLOSE WK-FILE.
       START-WK2-FILE-GREATER.
           PERFORM START-IT.
           MOVE WK-IFN TO E-FILE.
           MOVE WK2-KEY TO E-KEYX.
           START WK-FILE KEY > WK2-KEY.
           UNLOCK WK-FILE.
       START-WK2-FILE.
           PERFORM START-IT.
           MOVE WK-IFN TO E-FILE.
           MOVE WK2-KEY TO E-KEYX.
           START WK-FILE KEY NOT < WK2-KEY.
           UNLOCK WK-FILE.
       READ-WK2-FILE-NEXT.
           PERFORM READ-IT.
           MOVE WK-IFN TO E-FILE.
           MOVE WK2-KEY TO E-KEYX.
           READ WK-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-WK-FILE-NEXT.
       COPY "LIBGB/FERRORS.CPY".

      **********************************
       END-ROUTINE SECTION.
      **********************************
           PERFORM CLOSE-FILES.
           MOVE MKTEMP-BUF TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.
       EXIT-PROG.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY CJINQ-SCREEN.
           DESTROY CJINQ-WINDOW-HANDLE.
           EXIT PROGRAM.
