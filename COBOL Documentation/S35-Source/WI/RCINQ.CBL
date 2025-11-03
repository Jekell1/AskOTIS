       IDENTIFICATION DIVISION.
       PROGRAM-ID.      RCINQ.
      *****************************************************
      *                 (WI)
      *
      *  DESCRIPTION:   DAILY RECAP STATUS INQUIRY
      * REV:
      *  JTG 022494 ADDED GROUP FILE
      *  MJD 062295 ADDED CHECKS EXTRACTED INFO.
      *  BLL 062395 CHANGED TO WINDOW, MOVED TO WI, REMOVED BEGIN EOM STATUS,
      *             RANGE, CHANGED DISPLAY LINE, REMOVED EOM BEGIN OPTION
      *  MJD 091195 ADDED RC-EARNINGS-TIME, RC-EARNINGS-DATE, AND
      *             RC-LAST-ACCRUAL.
      *  BLL 091595 DISPLAY ONLY GL OUT OF BAL OPTION
      *  BLL 092295 FIX GROUP RANGE ON SEND RANGES
      *  BLL 121295 OUT OF BAL RANGE SHOULD INCLUDE BLANK GL FLAGS IF
      *             DAY HAS UPDATED.
      *  MJD 030496 FIXED BUG WITH GROUP SCAN. RCINQW WAS IN THE WINDOWSW
      *             REDEFINE ARED AND OVERWRITING GRSCANW.
      *  MJD 050296 CHANGED GP-KEY FROM 2 TO 1.
      *  BLV 022097 FIX RANGE-LIST, UPD & EARN OUT OF ORDER
      *  BLV 061297 HAD IO COPY MEMBER FOR RC; CHANGED TO INPUT
      *  GLF 090997 REPLACED 'IO' ON GP-FILE WITH GET 'GPENV'
      *  BLV 081098 FIXED EOM CREATED RC'S DISPLAYING AS GL OUT OF BALANCE
      *  BLV 000222 TRY TO IMPROVE SPEED BY ADDING A STARTING & ENDING
      *             STATUS; SHOULD STOP IO AFTER LAST SELECTED STATUS
      *             IS FOUND
      *  BAH 060201 REMOVED F5-MAIL, CAUSING LOCKUP
      *  BLM 100601 IN BUILD-PAGENO ROUTINE, CHECK IF RC-TRANS-DATE <
      *             SELECTED BEGINNING DATE, MOVE BEGINNING DATE TO KEY
      *             & START RC2, NEEDED TO SPEED UP INQUIRY, ESP. AT
      *             WORLD.  ALSO, FIX "MORE..." DISPLAYING INCORRECTLY
      *             AT END OF GL OUT-OF-BALANCE DISPLAY.
      *  BAH 140815 I CHANGED THE DEFAULT OF RESET-FG TO "N" BECAUSE IN
      *             A30 IT WIPES OUT INITIAL VALUES RIGHT AFTER THEY WERE
      *             DISPLAY WITH THE WRFORM IN REC-INQ-RANGES
      *   CS 150316 CHANGED SETUP FOR HELP ADDED RCINQ2
      *
      * KEC 2015.1125 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         FIXED RC-TRANS-DATE FROM YYYMMDD TO CCYYMMDD.
      *
      * KEC 2017.0321 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          FIXED PAGE-KEY TO BE 14 BYTES (WAS 13) MATCHING RC2-KEY
      * BAH 170421 ACTIVATE 8 DIGIT DATES, PD0006
      *
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

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(36) VALUE
           "@(#)WI/RCINQ S35 05/06/24-11:34:45 >".
       COPY "LIBGB/GB01RC.CPY".
       COPY "LIBGB/GB01RC_SQL.CPY".
       COPY "LIBGB/GBWSRC.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
      *    **********************************
      *    *      PROGRAM WORKERS
      *    **********************************
       01  HOLD-BR-KEY       PIC 9999  VALUE 0.

       01  RCINQ-LINES           PIC 99   VALUE 22.
       01  RCINQ-COLUMNS         PIC 99   VALUE 78.
       01  RCINQ-BEGIN-Y         PIC 99   VALUE 2.
       01  RCINQ-BEGIN-X         PIC 99   VALUE 2.
       01  ERRCD                PIC X VALUE " ".
       01  EOD-UPD           PIC X(9)       VALUE "EOD UPD".
       01  EOD-BATCH         PIC X(9)       VALUE "EOD BATCH".
       01  ITS-EOM-FG        PIC X          VALUE "N".
       01  MSEL-LIST         PIC X(8)       VALUE "MSEL:P.".
       01  RESET-FG          PIC X          VALUE "N".
       01  ONE               PIC S9.
       01  SUB               PIC 999 COMP   VALUE 0.
       01  IO-TYPE           PIC X(3).

       01  RANGE-LIST.
           03  FILLER        PIC X(29)  VALUE
           "BR1 BR2 DATE1 DATE2 OPEN UPD ".
           03  FILLER        PIC X(27)  VALUE
           "EARN EOMUPD DEPOSIT CHECKS.".
       01  RANGE-BUF.
           03  D-BEG-BRANCH    PIC X(4).
           03  D-END-BRANCH    PIC 9(4)  BLANK ZERO.
           03  D-DATE1         PIC 99/99/99.
           03  D-DATE2         PIC 99/99/99.
           03  D-OPEN          PIC X.
           03  D-UPDATED       PIC X.
           03  D-EARNINGS      PIC X.
           03  D-EOMUPD        PIC X.
           03  D-DEPOSIT       PIC X.
           03  D-CHECKS        PIC X.

       01  SYSDATE-NUM         PIC 9(8).

       01  BEG-BRANCH         PIC 9(4).
       01  END-BRANCH         PIC 9(4).
       01  BEG-DATE           PIC 9(8).
       01  END-DATE           PIC 9(8).
       01  ENT-GROUP          PIC X(4).

       01  INCLUDE-OPEN-FG         PIC X.
           88  INCLUDE-OPEN        VALUE "Y".
       01  INCLUDE-UPDATED-FG      PIC X.
           88  INCLUDE-UPDATED     VALUE "Y".
       01  INCLUDE-EARNINGS-FG     PIC X.
           88  INCLUDE-EARNINGS    VALUE "Y".
       01  INCLUDE-EOMUPD-FG       PIC X.
           88  INCLUDE-EOMUPD      VALUE "Y".
       01  INCLUDE-DEPOSIT-FG      PIC X.
           88  INCLUDE-DEPOSIT     VALUE "Y".
       01  INCLUDE-CHECKS-FG       PIC X.
           88  INCLUDE-CHECKS      VALUE "Y".

       01  BEG-STATUS              PIC XX.
       01  END-STATUS              PIC XX.

       01  ONLY-OUT-OF-BAL-FG      PIC X VALUE "N".

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

       01  DISPLAY-LIST.
           03  FILLER      PIC X(40)  VALUE
               "010 020 030 040 050 060 070 080 090 100 ".
           03  FILLER      PIC X(35)  VALUE
               "110 120 130 140 150 PGNO ENDMORE:F.".
       01  DISPLAY-BUF.
           03  D-LINE                 OCCURS 15 TIMES.
               05  D-BRNO             PIC 9999.
               05  FILLER             PIC XX.
               05  D-DATE             PIC 99/99/99.
               05  FILLER             PIC X(2).
               05  D-STATUS           PIC X(9).
               05  FILLER             PIC XXX.
               05  D-RECAP-FG         PIC X(5).
               05  FILLER             PIC XX.
               05  D-GL-FG            PIC X(5).
               05  FILLER             PIC X.
               05  D-EOD-BATCH-FG     PIC X(5).
               05  FILLER             PIC X.
               05  FILLER             PIC X(4).
               05  D-EOM1-BATCH-FG    PIC X(5).
               05  FILLER             PIC X(4).
               05  D-EOM2-BATCH-FG    PIC X(5).
               05  FILLER             PIC X(2).
               05  D-OVER-SHORT       PIC ZZZZ9.99-     BLANK ZERO.
           03  D-PAGENO               PIC ZZ9.
           03  D-END-MORE             PIC X(7).

       01  PAGE-KEY-TABLE.
           03  PAGE-KEY               PIC X(14) OCCURS 600.
       01  CURRENT-PAGE               PIC S999.
       01  PAGE-COUNT                 PIC 999.
       01  MAX-PAGES                  PIC 9(4)  VALUE 600.
      *--------------------------------------------
       01  MORE-LIST.
           03  FILLER               PIC X(43)  VALUE
           "TITLE DATE BRNO BRNAME 010 020 030 040 050 ".
           03  FILLER               PIC X(36)  VALUE
           "060 070 080 090 091 100 110 120 130.".
       01  MORE-BUF.
           03  M-TITLE              PIC X(30).
           03  M-DATE               PIC X(8).
           03  M-BRNO               PIC ZZZ9.
           03  M-BRNAME             PIC X(30).
           03  M-EOD-TABLE.
               05  M-EOD-TBL        OCCURS 6 TIMES.
                   07  M-EOD-TIME   PIC 99/99/99    BLANK ZERO.
                   07  FILLER       PIC X(10).
                   07  M-EOD-USERID PIC X(10).
                   07  M-EOD-DATE   PIC 99/99/99    BLANK ZERO.
           03  M-DEPOSIT            PIC Z,ZZZ,ZZ9.99-.
           03  M-OVER-SHORT         PIC Z,ZZZ,ZZ9.99-.
           03  M-CHECKS             PIC Z,ZZZ,ZZ9.99-.
           03  M-LAST-ACCRUE        PIC 99/99       BLANK ZERO.
           03  M-EOM-TABLE.
               05  M-EOM-TBL        OCCURS 3 TIMES.
                   07  M-EOM-TIME   PIC 99/99/99    BLANK ZERO.
                   07  FILLER       PIC X(12).
                   07  M-EOM-DATE   PIC 99/99/99    BLANK ZERO.
           03  M-ERROR-MSG          PIC X(20).


      *----------------------------------GROUP
       01  GROUP-FLAG        PIC X.
       COPY "LIBGB/GRWORKW.CPY".
      *----------------------------------GROUP

       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBWI/PFSWINDOWSW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/FLDSELW.CPY".
       COPY "LIBGB/PHON400.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBWI/GRSCANW.CPY".
       01  RCINQ-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/RCINQ_DEF.CPY".
       COPY "LIBWI/RCINQ2_DEF.CPY".
       COPY "LIBWI/RCINQ_WKS.CPY".
       COPY "LIBWI/RCINQ2_WKS.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBWI/RCINQW.CPY".

       SCREEN SECTION.
       COPY "LIBWI/RCINQ_SCN.CPY".
       COPY "LIBWI/RCINQ2_SCN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME RCINQ-BUF EXIT-PATHNAME.

      ****************************
      *      MAIN PROGRAM MODULE
      ****************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.

           MOVE "XX" TO RCINQ-STATUS.
           MOVE " " TO VDU-WIMENU-SELECTION.
           MOVE "RCINQ" TO VDU-FORM-NAME.

       COPY "LIBGB/CHKSEC.CPY".

           DISPLAY FLOATING GRAPHICAL WINDOW
               AT SCREEN LINE 02 * VDU-WIN-L-OFFSET
               AT SCREEN COL 02 * VDU-WIN-C-OFFSET
               CELL SIZE IS LABEL FONT
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
               SIZE 78 + VDU-WIN-W-MOD
               LINES 22 + VDU-WIN-H-MOD
               BLANK SCREEN
               HANDLE IS RCINQ-WINDOW-HANDLE.

           PERFORM FORM-RESET.

       MAIN-MODULE.
           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO SYSDATE-NUM.

           PERFORM TEST-FOR-EOM-TODAY.

       MAIN-CONT.
           IF RCINQ-ENTER-RANGES = "Y"
              PERFORM REC-INQ-RANGES
           ELSE
              PERFORM SEND-INQ-RANGES.
           IF IO-TYPE = "END"
              GO TO END-ROUTINE.
           IF IO-TYPE = "CLR"
              GO TO MAIN-CONT.

           IF GROUP-FLAG = "Y"
              PERFORM OPEN-GR1-FILE
              MOVE ENT-GROUP TO GR-ID
              PERFORM SEARCH-GR-INIT
              PERFORM CLOSE-GR1-FILE.

           PERFORM INQUIRY-DISPLAY.
           GO TO MAIN-CONT.

      *************************************
       REC-INQ-RANGES SECTION.
      *************************************
       REC-RANGES.
           MOVE SPACES TO IO-TYPE.
           IF RESET-FG = "Y"
              MOVE SPACES TO WR-BUF
              MOVE RANGE-LIST TO WR-LIST
              PERFORM CALL-WRFORM.
           MOVE "Y" TO RESET-FG.

       BR-01.
           MOVE "F7, F6-SCAN, ENTER GROUP OR BEG BRANCH #:" TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "E RA000BR1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "16" GO TO ALL-BR.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-REC-RANGES.
           IF VDUSTATUS = "1<"
              MOVE "WI/GRSCAN" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH GRSCAN-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              IF GRSCAN-STATUS NOT = "00"
                 GO TO BR-01
              ELSE
                 MOVE "S  A000BR1." TO VDU
                 MOVE GRSCAN-KEY TO VDUBUF GR1-KEY
                 PERFORM SCREENIO
                 MOVE "00" TO VDUSTATUS.

           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BR-01.

           MOVE VDUBUF TO WGR-WORD ENT-GROUP.
           IF WGR-WORD-ZERO
              MOVE 0 TO WGR-SRESULT
              GO TO BRANCH-IS-NUMERIC.
           MOVE 0 TO WGR-MIN-LEVEL.
           PERFORM CHECK-MEMBER.
           IF WGR-ERR > 0
              MOVE WGR-MSG(WGR-ERR) TO MESS
              PERFORM SEND-MESS
              GO TO BR-01.
           IF WGR-TYPE = "A"
              MOVE "S  A000BR2." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
              MOVE "Y" TO GROUP-FLAG
              MOVE 0 TO BEG-BRANCH
              MOVE 9999 TO END-BRANCH
              GO TO BEG-DATE.
       BRANCH-IS-NUMERIC.
           MOVE "N" TO GROUP-FLAG.
           MOVE WGR-SRESULT TO BEG-BRANCH.
           MOVE "SN N040BR1." TO VDU.
           MOVE BEG-BRANCH TO VDUNUM0.
           PERFORM SCREENIO.
       BR-02.
           MOVE "F7, ENTER END BRANCH #:"
              TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENNN040BR2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO REC-RANGES.
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-REC-RANGES.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO BR-02.
           MOVE VDUNUM0 TO END-BRANCH.
           IF BEG-BRANCH > END-BRANCH
              MOVE "INVALID BRANCH RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO BR-01.

           GO TO BEG-DATE.
       ALL-BR.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "N" TO GROUP-FLAG.

       BEG-DATE.
           MOVE "F1, ENTER BEGINNING DATE, F3-ALL:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ERNM060DATE1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              GO TO REC-RANGES.
           IF VDUSTATUS = "16"
              GO TO ALL-DATES.
           IF VDUSTATUS = "1U"
              GO TO BR-01.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-REC-RANGES.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO BEG-DATE.
           MOVE VDU-DATE-YYYYMMDD TO BEG-DATE.
           IF VDUNUM0 = 0
              MOVE "S  A080DATE1." TO VDU
              MOVE SYSDATE-NUM TO BEG-DATE DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO VDU-DATE-EDIT
              PERFORM SCREENIO.
       ENTER-DATE2.
           MOVE 0 TO END-DATE.
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
           MOVE VDU-DATE-YYYYMMDD TO END-DATE.
           IF VDUNUM0 = 0
              MOVE "S  A080DATE2." TO VDU
              MOVE SYSDATE-NUM TO END-DATE DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO VDU-DATE-EDIT
              PERFORM SCREENIO.

           IF END-DATE < BEG-DATE
              GO TO ENTER-DATE2.

           GO TO ENTER-OPEN.
       ALL-DATES.
           MOVE EXT-JULIAN-BEG TO BEG-DATE DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE "S  A000DATE1." TO VDU.
           MOVE DATE-MMDDYY TO VDU-DATE-EDIT.
           PERFORM SCREENIO.

           MOVE EXT-JULIAN-END TO END-DATE DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE "S  A000DATE2." TO VDU.
           MOVE DATE-MMDDYY TO VDU-DATE-EDIT.
           PERFORM SCREENIO.

       ENTER-OPEN.
           MOVE "F1, INCLUDE OPEN STATUSES?:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E  A000OPEN." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              GO TO REC-RANGES.
           IF VDUSTATUS = "1U"
              GO TO ENTER-DATE2.
           IF VDUSTATUS = "1>"
             MOVE "END" TO IO-TYPE
             GO TO END-REC-RANGES.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ENTER-OPEN.
           IF VDUBUF NOT = "Y"
              IF VDUBUF NOT = "N"
                 MOVE "PLEASE ENTER Y OR N" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-OPEN.
           MOVE VDUBUF TO INCLUDE-OPEN-FG.

       ENTER-UPD.
           MOVE "F1, INCLUDE UPDATED STATUSES?:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E  A000UPD." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              GO TO REC-RANGES.
           IF VDUSTATUS = "1U"
              GO TO ENTER-OPEN.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-REC-RANGES.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ENTER-UPD.
           IF VDUBUF NOT = "Y"
              IF VDUBUF NOT = "N"
                 MOVE "PLEASE ENTER Y OR N" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-UPD.
           MOVE VDUBUF TO INCLUDE-UPDATED-FG.
       ENTER-EARNINGS.
           MOVE "F1, INCLUDE EARNINGS STATUSES?:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E  A000EARN." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              GO TO REC-RANGES.
           IF VDUSTATUS = "1U"
              GO TO ENTER-UPD.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-REC-RANGES.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ENTER-EARNINGS.
           IF VDUBUF NOT = "Y"
              IF VDUBUF NOT = "N"
                 MOVE "PLEASE ENTER Y OR N" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-EARNINGS.
           MOVE VDUBUF TO INCLUDE-EARNINGS-FG.
       ENTER-EOMUPD.
           MOVE "F1, INCLUDE EOM UPDATED STATUSES?:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E  A000EOMUPD." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              GO TO REC-RANGES.
           IF VDUSTATUS = "1U"
              GO TO ENTER-EARNINGS.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-REC-RANGES.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ENTER-EOMUPD.
           IF VDUBUF NOT = "Y"
              IF VDUBUF NOT = "N"
                 MOVE "PLEASE ENTER Y OR N" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-EOMUPD.
           MOVE VDUBUF TO INCLUDE-EOMUPD-FG.
       ENTER-DEPOSIT.
           MOVE "F1, INCLUDE BANK DEPOSIT STATUSES?:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E  A000DEPOSIT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              GO TO REC-RANGES.
           IF VDUSTATUS = "1U"
              GO TO ENTER-EOMUPD.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-REC-RANGES.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ENTER-DEPOSIT.
           IF VDUBUF NOT = "Y"
              IF VDUBUF NOT = "N"
                 MOVE "PLEASE ENTER Y OR N" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-DEPOSIT.
           MOVE VDUBUF TO INCLUDE-DEPOSIT-FG.
       ENTER-CHKS-EXTRACTED.
           MOVE "F1, INCLUDE CHECKS EXTRACTED STATUSES?:"
              TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E  A000CHECKS." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              GO TO REC-RANGES.
           IF VDUSTATUS = "1U"
              GO TO ENTER-DEPOSIT.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-REC-RANGES.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ENTER-CHKS-EXTRACTED.
           IF VDUBUF NOT = "Y"
              IF VDUBUF NOT = "N"
                 MOVE "PLEASE ENTER Y OR N" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-CHKS-EXTRACTED.
           MOVE VDUBUF TO INCLUDE-CHECKS-FG.

       END-REC-RANGES.
           EXIT.

      **************************************
       SEND-INQ-RANGES SECTION.
      **************************************
           MOVE "Y" TO RCINQ-ENTER-RANGES.
           MOVE SPACES TO IO-TYPE.

           IF RCINQ-GROUP NUMERIC
              MOVE "N" TO GROUP-FLAG
              MOVE RCINQ-BEG-BRANCH TO BEG-BRANCH D-BEG-BRANCH
              MOVE RCINQ-END-BRANCH TO END-BRANCH D-END-BRANCH
           ELSE
              MOVE "Y" TO GROUP-FLAG
              MOVE RCINQ-GROUP TO ENT-GROUP D-BEG-BRANCH
              MOVE 0 TO D-END-BRANCH
              MOVE 0 TO BEG-BRANCH
              MOVE 9999 TO END-BRANCH.

           MOVE RCINQW-DATE1 TO DATE-YYYYMMDD BEG-DATE.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-DATE1.
           MOVE RCINQW-DATE2 TO DATE-YYYYMMDD END-DATE.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-DATE2.

           MOVE RCINQ-OPEN-FG TO INCLUDE-OPEN-FG
                                 D-OPEN.
           MOVE RCINQ-UPDATED-FG TO INCLUDE-UPDATED-FG
                                    D-UPDATED.
           MOVE RCINQ-EARNINGS-FG TO INCLUDE-EARNINGS-FG
                                    D-EARNINGS.
           MOVE RCINQ-EOMUPD-FG TO INCLUDE-EOMUPD-FG
                                   D-EOMUPD.
           MOVE RCINQ-DEPOSIT-FG TO INCLUDE-DEPOSIT-FG
                                    D-DEPOSIT.
           MOVE RCINQ-CHECKS-FG TO INCLUDE-CHECKS-FG
                                   D-CHECKS.
           MOVE RANGE-BUF TO WR-BUF.
           MOVE RANGE-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       EXIT-SEND-RANGES.
           EXIT.

      *************************************************
      *   ENTRY-MODULE
      *   NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *              1 - INITIAL NEW RECORD ENTRY
      *              2 - MODIFY NEW RECORD
      ************************************************
       INQUIRY-DISPLAY SECTION.

           MOVE SPACES TO RC-STATUS
                          BEG-STATUS QRC2-WBEG-STATUS.
           MOVE "M1" TO END-STATUS.
           IF NOT INCLUDE-CHECKS
              MOVE "K1" TO END-STATUS
              IF NOT INCLUDE-DEPOSIT
                 MOVE "I1" TO END-STATUS
                 IF NOT INCLUDE-EOMUPD
                    MOVE "G1" TO END-STATUS
                    IF NOT INCLUDE-EARNINGS
                       MOVE "D1" TO END-STATUS
                       IF NOT INCLUDE-UPDATED
                          MOVE "B1" TO END-STATUS.
           MOVE END-STATUS TO QRC2-WEND-STATUS.

           MOVE BEG-BRANCH TO RC-BRNO.
                              QRC2-WBEG-BRNO.
           MOVE END-BRANCH TO QRC2-WEND-BRNO.

           MOVE BEG-DATE   TO RC-TRANS-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QRC2-WBEG-TRANS-DATE.
           MOVE END-DATE   TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QRC2-WEND-TRANS-DATE.

           MOVE FIRST-FIELD TO FLDSEL-FIELD.
           MOVE 1           TO PAGE-COUNT ONE.
           MOVE 0           TO CURRENT-PAGE.

           PERFORM OPEN-RC2-FILE.

           MOVE RC2-KEY TO PAGE-KEY(PAGE-COUNT).
           MOVE "N" TO ONLY-OUT-OF-BAL-FG.

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
           MOVE PAGE-KEY(CURRENT-PAGE) TO RC2-KEY.

           PERFORM START-RC2-FILE.

      *************************************
      *    BUILD PAGE
      *************************************
       BUILD-PAGENO.
           PERFORM READ-RC2-FILE-NEXT.
           IF IO-FG NOT = 0
              GO TO DISPLAY-PAGE.

           IF RC-STATUS > END-STATUS
              MOVE 9 TO IO-FG
              GO TO DISPLAY-PAGE.

           IF SUB > 15
              GO TO DISPLAY-PAGE.

      *------------------------------------GROUP
           IF GROUP-FLAG = "Y"
              MOVE RC-BRNO TO WGR-SRESULT
              PERFORM SEARCH-GR-VERIFY
              IF WGR-ERR NOT = 0
                 GO TO BUILD-PAGENO
              ELSE
                 NEXT SENTENCE
           ELSE
              IF RC-BRNO < BEG-BRANCH OR > END-BRANCH
                 GO TO BUILD-PAGENO.
      *------------------------------------GROUP

           IF ( RC-TRANS-DATE < BEG-DATE ) OR
              ( RC-TRANS-DATE > END-DATE )
              GO TO BUILD-PAGENO.

      * A1 - OPEN
      * B1 - INPROCESS
      * C1 - UPDATED
      * D1 - EOD BATCH DONE
      * G1 - EOM EARNINGS DONE
      * H1 - EOM1 BATCH DONE
      * I1 - EOM UPDATE DONE
      * J1 - EOM2 BATCH DONE
      * K1 - BANK DEPOSIT
      * K2 - DEPOSIT TRANSMITTED
      * M1 - CHECKS EXTRACTED

           IF RC-STATUS = "A1" OR "B1"
              IF NOT INCLUDE-OPEN
                 GO TO BUILD-PAGENO.
           IF RC-STATUS = "C1" OR "D1"
              IF NOT INCLUDE-UPDATED
                 GO TO BUILD-PAGENO.
           IF RC-STATUS = "G1"
              IF NOT INCLUDE-EARNINGS
                 GO TO BUILD-PAGENO.
           IF RC-STATUS = "H1" OR "I1" OR "J1"
              IF NOT INCLUDE-EOMUPD
                 GO TO BUILD-PAGENO.
           IF RC-STATUS = "K1"
              IF NOT INCLUDE-DEPOSIT
                 GO TO BUILD-PAGENO.
           IF RC-STATUS = "M1"
              IF NOT INCLUDE-CHECKS
                 GO TO BUILD-PAGENO.

      *    IF ONLY-OUT-OF-BAL-FG = "Y"
      *       IF RC-GL-FG NOT = "N"
      *          GO TO BUILD-PAGENO.

           IF ONLY-OUT-OF-BAL-FG = "N"
              GO TO MOVE-FIELDS.
           IF RC-GL-FG = "N"
              GO TO MOVE-FIELDS.
           IF RC-GL-FG = " "
              IF RC-STATUS > "C1"
                 IF RC-ERROR-MSG NOT = "CREATED BY EOM"
                    GO TO MOVE-FIELDS.
           GO TO BUILD-PAGENO.

       MOVE-FIELDS.
           MOVE RC-BRNO TO D-BRNO(SUB).

           MOVE RC-TRANS-DATE       TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY         TO D-DATE(SUB).

           IF RC-STATUS = "A1"
              MOVE "OPEN" TO D-STATUS(SUB)
           ELSE
           IF RC-STATUS = "B1"
              MOVE "INPROC" TO D-STATUS(SUB)
           ELSE
           IF RC-STATUS = "C1"
              MOVE EOD-UPD TO D-STATUS(SUB)
           ELSE
           IF RC-STATUS = "D1"
              MOVE EOD-BATCH TO D-STATUS(SUB)
           ELSE
           IF RC-STATUS = "G1"
              MOVE "EARNINGS " TO D-STATUS(SUB)
           ELSE
           IF RC-STATUS = "H1"
              MOVE "EOM1 DONE" TO D-STATUS(SUB)
           ELSE
           IF RC-STATUS = "I1"
              MOVE "EOM UPD" TO D-STATUS(SUB)
           ELSE
           IF RC-STATUS = "J1"
              MOVE "EOM2 DONE" TO D-STATUS(SUB)
           ELSE
           IF RC-STATUS = "K1"
              MOVE "BANKDEPO" TO D-STATUS(SUB)
           ELSE
           IF RC-STATUS = "K2"
              MOVE "TRANSDEPO" TO D-STATUS(SUB)
           ELSE
           IF RC-STATUS = "M1"
              MOVE "EXTR CHKS" TO D-STATUS(SUB)
           ELSE
              MOVE RC-STATUS TO D-STATUS(SUB).

           MOVE RC-RECAP-FG TO D-RECAP-FG(SUB).
           MOVE RC-GL-FG TO D-GL-FG(SUB).
           MOVE RC-EOD-BATCH-FG TO D-EOD-BATCH-FG(SUB).
           MOVE RC-EOM1-BATCH-FG TO D-EOM1-BATCH-FG(SUB).
           IF RC-EOM1-BATCH-FG = " "
              IF RC-EOM-UPDATED-DATE NOT = 0
                 MOVE "Y" TO D-EOM1-BATCH-FG(SUB).
           MOVE RC-EOM2-BATCH-FG TO D-EOM2-BATCH-FG(SUB).
           MOVE RC-OVER-SHORT TO D-OVER-SHORT(SUB).

           ADD 1 TO SUB.
           GO TO BUILD-PAGENO.

      *************************************
      *    DISPLAY PAGE
      *************************************
       DISPLAY-PAGE.
           IF IO-FG NOT = 0
              IF SUB = 1
                 MOVE "NO RECORDS FOUND!" TO MESS
                 PERFORM SEND-MESS
                 MOVE DISPLAY-BUF TO WR-BUF
                 MOVE DISPLAY-LIST TO WR-LIST
                 PERFORM CALL-WRFORM
                 GO TO END-INQUIRY-MODULE.

           MOVE "MORE..." TO D-END-MORE.
           IF CURRENT-PAGE = PAGE-COUNT
              IF IO-FG = 0
                 IF PAGE-COUNT < MAX-PAGES
                    ADD 1 TO PAGE-COUNT
                    MOVE RC2-KEY TO PAGE-KEY(PAGE-COUNT)
                 ELSE
                    MOVE "MAXIMUM PAGES REACHED!" TO MESS
                    PERFORM SEND-LEGEND
                    MOVE "OVRFLOW" TO D-END-MORE
              ELSE
                 MOVE "* END *" TO D-END-MORE.

      D    STOP MESS.
           MOVE CURRENT-PAGE TO D-PAGENO.
           MOVE DISPLAY-BUF  TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *************************************
      *    SELECT FIELD FROM PAGES
      *************************************
       SELECT-FIELD-FROM-DISPLAY.
           MOVE
           "F1,F7-EXIT,F6-MORE INFO,F3-OUT OF BAL ONLY:" TO MESS
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

      D    STOP MESS.
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

           IF FLDSEL-STAT = "1>"
              GO TO END-ROUTINE.
           IF FLDSEL-STAT = "10"
              MOVE "N" TO RESET-FG
              GO TO END-INQUIRY-MODULE.
           IF FLDSEL-STAT = "16"
              MOVE 0 TO CURRENT-PAGE
              MOVE 1 TO ONE PAGE-COUNT
              MOVE FIRST-FIELD TO FLDSEL-FIELD
              MOVE "Y" TO ONLY-OUT-OF-BAL-FG
              GO TO START-BUILD-PAGENO.
           IF FLDSEL-STAT = "1<"
              PERFORM MORE-INFO
              GO TO SELECT-FIELD-FROM-DISPLAY.
           IF NOT (FLDSEL-STAT = "00" OR "1<")
              PERFORM ALARM
              GO TO SELECT-FIELD-FROM-DISPLAY.

           GO TO SELECT-FIELD-FROM-DISPLAY.

       END-INQUIRY-MODULE.
           EXIT.

      *************************************
       MORE-INFO SECTION.
      *************************************
           PERFORM CLOSE-RC2-FILE.
           PERFORM OPEN-RC1-FILE.

           PERFORM SET-RC1-KEY.
           PERFORM READ-RC1-FILE.
           PERFORM CLOSE-RC1-FILE.
           IF IO-FG NOT = 0
              MOVE "UNABLE TO READ RECAP FOR THIS DAY!" TO MESS
              PERFORM SEND-MESS
              GO TO MORE-INFO-EXIT.

           MOVE 22 TO WINDOW-LINES.
           MOVE 56 TO WINDOW-COLUMNS.
           MOVE 2 TO WINDOW-BEGIN-Y.
           MOVE 12 TO WINDOW-BEGIN-X.
           MOVE "RCINQ2" TO VDU-FORM-NAME.
           MOVE "WI/RCINQ2" TO WINDOW-FORM-NAM.

           PERFORM OPEN-WINDOW.
           DISPLAY RCINQ2-SCREEN.

           MOVE "DAILY BRANCH RECAP INFORMATION" TO M-TITLE.
           MOVE FLDSEL-DATE TO M-DATE.
           MOVE FLDSEL-BRNO TO M-BRNO.
           MOVE FLDSEL-BRNO TO BR-NO.
           PERFORM GET-BR.
           MOVE BR-NAME TO M-BRNAME.

           MOVE RC-OPENED-TIME TO M-EOD-TIME(1).
           MOVE RC-OP-USERID   TO M-EOD-USERID(1)
           MOVE RC-OPENED-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY    TO M-EOD-DATE(1).
           INSPECT M-EOD-TIME(1) REPLACING ALL "/" BY ":".

           MOVE RC-CLOSED-TIME TO M-EOD-TIME(2).
           MOVE RC-CL-USERID   TO M-EOD-USERID(2)
           MOVE RC-CLOSED-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY    TO M-EOD-DATE(2).
           INSPECT M-EOD-TIME(2) REPLACING ALL "/" BY ":".

           MOVE RC-UPDATED-TIME TO M-EOD-TIME(3).
           MOVE RC-UPDATED-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY     TO M-EOD-DATE(3).
           INSPECT M-EOD-TIME(3) REPLACING ALL "/" BY ":".

           MOVE RC-EOD-BATCH-TIME TO M-EOD-TIME(4).
           MOVE RC-EOD-BATCH-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY       TO M-EOD-DATE(4).
           INSPECT M-EOD-TIME(4) REPLACING ALL "/" BY ":".

           MOVE RC-FUNDS-TIME TO M-EOD-TIME(5).
           MOVE RC-FUNDS-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY   TO M-EOD-DATE(5).
           INSPECT M-EOD-TIME(5) REPLACING ALL "/" BY ":".

           MOVE RC-CHECK-TIME TO M-EOD-TIME(6).
           MOVE RC-CHECK-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY   TO M-EOD-DATE(6).
           INSPECT M-EOD-TIME(6) REPLACING ALL "/" BY ":".

           MOVE RC-DEPOSIT TO M-DEPOSIT.
           MOVE RC-OVER-SHORT TO M-OVER-SHORT.
           MOVE RC-CHECKS TO M-CHECKS.

           MOVE RC-LAST-ACCRUE TO M-LAST-ACCRUE.

           MOVE RC-EARNINGS-TIME TO M-EOM-TIME(1).
           MOVE RC-EARNINGS-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY      TO M-EOM-DATE(1).
           INSPECT M-EOM-TIME(1) REPLACING ALL "/" BY ":".

           MOVE RC-EOM-UPDATED-TIME TO M-EOM-TIME(2).
           MOVE RC-EOM-UPDATED-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY         TO M-EOM-DATE(2).
           INSPECT M-EOM-TIME(2) REPLACING ALL "/" BY ":".

           MOVE RC-EOM2-BATCH-TIME TO M-EOM-TIME(3).
           MOVE RC-EOM2-BATCH-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY        TO M-EOM-DATE(3).
           INSPECT M-EOM-TIME(3) REPLACING ALL "/" BY ":".

           MOVE RC-ERROR-MSG TO M-ERROR-MSG.

           MOVE MORE-BUF TO WR-BUF.
           MOVE MORE-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE "E  A010CONTINUE." TO VDU.
           PERFORM SCREENIO.

       MORE-INFO-CLOSE-WINDOW.
           PERFORM CLOSE-WINDOW.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY RCINQ2-SCREEN.
           MOVE "RCINQ" TO VDU-FORM-NAME.
       MORE-INFO-EXIT.
           PERFORM OPEN-RC2-FILE.

      *--------------------------------------------
       COPY "LIBWI/PFSWINDOWS.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/FLDSEL.CPY".
       COPY "LIBGB/GPENV.CPY".

      **********************************************
       TEST-FOR-EOM-TODAY SECTION.
      **********************************************
           MOVE "N" TO ITS-EOM-FG.

           PERFORM GET-GPENV.

           IF IO-FG = 0
              MOVE SYSDATE-NUM TO NUM-DATE
              PERFORM FIND-FISCAL-PERIOD
              IF SUB NOT = 0
                 IF SYSDATE-NUM = GP-END-FISCAL-DATE(SUB)
                    MOVE "Y" TO ITS-EOM-FG
                    MOVE "S  A180EOM:R." TO VDU
                    MOVE "** END OF MONTH **" TO VDUBUF
                    PERFORM SCREENIO.

      ****************************************************
      *    FIND FISCAL END OF PERIOD
      *
      *  1.  SET NUM-DATE = TODAY'S DATE
      *  2.  SUBSCRIPT TO GP-END-FISCAL-DATE IS IN SUB
      *  3.  IF NOT FOUND SUB = 0
      ****************************************************
       FIND-FISCAL-PERIOD SECTION.
           MOVE 1 TO SUB.

       FIND-FISCAL-PERIOD-AGAIN.
           IF SUB > 12
              MOVE 0 TO SUB
           ELSE
              MOVE GP-END-FISCAL-DATE(SUB) TO SYS-DATE
              IF NUM-MO NOT = S-MM
                 ADD 1 TO SUB
                 GO TO FIND-FISCAL-PERIOD-AGAIN.

      ********************************************************
       FORM-RESET SECTION.
      ********************************************************
           DISPLAY RCINQ-SCREEN UPON RCINQ-WINDOW-HANDLE.
           MOVE EXT-CONAME-SYSDATE TO RCINQ-SYSDATE.
           MOVE EXT-CONAME-CONAME  TO RCINQ-CONAME.
           MOVE RCINQ-FORM-FIELDS TO WR-BUF.
           MOVE RCINQ-FORM-LIST TO WR-LIST. 
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ********************************************************
       SETUP-LINK-INFO SECTION.
      ********************************************************
           IF VDU-FORM-NAME = "RCINQ2"
              MOVE RCINQ2-HELP--FILE TO HELP-LINK-FILE
              MOVE RCINQ2-HELP--DIR  TO HELP-LINK-DIR
           ELSE
              MOVE RCINQ-HELP--FILE TO HELP-LINK-FILE 
              MOVE RCINQ-HELP--DIR  TO HELP-LINK-DIR.

      ********************************************************
       VDU-CONVERT-FIELD SECTION.
      ********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/RCINQ_EVA.CPY".
              COPY "LIBWI/RCINQ2_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ************************************
       MISC-PARAGRAPHS SECTION.
      ************************************
       COPY "LIBGB/SYSTEM.CPY".
       SET-RC1-KEY.
           MOVE FLDSEL-BRNO       TO RC-BRNO.
           MOVE FLDSEL-DATE       TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD     TO RC-TRANS-DATE.

       GET-BR.
           IF BR-NO NOT = HOLD-BR-KEY
              PERFORM OPEN-BR-FILE
              PERFORM READ-BR-FILE
              PERFORM CLOSE-BR-FILE
              IF IO-FG = 0
                 MOVE BR-NO TO HOLD-BR-KEY.
       SEND-LEGEND.
            MOVE MESS TO WR-BUF.
           MOVE MSEL-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ***************************************
       IO-ROUTINES SECTION.
      ***************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-RC2-FILE.
           PERFORM CLOSE-RC1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBGB/GBRCGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBRC1RN.CPY".
       COPY "LIBGB/GBRC2RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      ***************************************
       END-ROUTINE SECTION.
      ***************************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY RCINQ-SCREEN.
           DESTROY RCINQ-WINDOW-HANDLE.
           PERFORM CLOSE-WINDOW UNTIL WINDOWS-OPEN = 0.

       STOP-RUN.
           EXIT PROGRAM.
