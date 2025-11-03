       IDENTIFICATION DIVISION.
       PROGRAM-ID.      WRMAN4.
      **************************************************************************
      *                 (MN)
      *
      *  DESCRIPTION:   WORLD RATING SHEET FILE MAINTENANCE - PAGE 4
      *                 SMALL LOANS PG 4
      * REV:
      * BLV 040119 FIXED SCAN KEY
      * GLB 040226 FIX DATE CONVERSION BUG;MOVE NUM-DATE TO DATE-MMDDYY
      * GLB 040226 ADD MAINTENANCE LOG
      * GLB 040317 GENERAL CLEANUP WHILE CREATING HELP
      *  CS 160509 CHANGED ORIGINAL/CHANGED REC TO INCLUDE WR IN NAME
      *  CS 170524 ACTIVATE 8 DIGIT DATES
      *  CS 20200218 FIXED SCANKEY
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
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)MN/WRMAN4.CBL S34 06/08/21-11:43:08 >".
      *******************************
      *       PROGRAM WORKERS       *
      *******************************
       COPY "LIBSP/SP01WR.CPY".
       COPY "LIBSP/SP01WR_SQL.CPY".
       COPY "LIBSP/SPWSWR.CPY".
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    ORIGINAL RATING SHEET RECORD
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       COPY "LIBSP/SP01WR.CPY" REPLACING
            LEADING "WR" BY "WROR".
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CHANGED  RATING SHEET RECORD
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       COPY "LIBSP/SP01WR.CPY" REPLACING
            LEADING "WR" BY "WRCH".
       01  WORK-ERRLOG-KEY.
           03  WORK-ERRLOG-WR1          PIC 9(10).
           03  FILLER                   PIC X.
           03  WORK-ERRLOG-FIELD        PIC X(3).
           03  FILLER                   PIC X.
           03  WORK-ERRLOG-DESC         PIC X(7).
           03  WORK-ERRLOG-ORG-ALP      PIC X(13).
           03  WORK-ERRLOG-ORG-N0 REDEFINES
                   WORK-ERRLOG-ORG-ALP  PIC Z(12)-.
           03  WORK-ERRLOG-ORG-N2 REDEFINES
                   WORK-ERRLOG-ORG-ALP  PIC Z(9).99-.

           03  WORK-ERRLOG-TO-X         PIC X(2).

           03  WORK-ERRLOG-CHG-ALP      PIC X(13).
           03  WORK-ERRLOG-CHG-N0 REDEFINES
                   WORK-ERRLOG-CHG-ALP  PIC Z(12)-.
           03  WORK-ERRLOG-CHG-N2 REDEFINES
                   WORK-ERRLOG-CHG-ALP  PIC Z(9).99-.

       01  WORK-FIELD-TABLE.
           03  WORK-FIELD-FILLER.
               05  FILLER     PIC X(7) VALUE "#30 DAY".
               05  FILLER     PIC X(7) VALUE "$30 DAY".
               05  FILLER     PIC X(7) VALUE "FLD 03 ".
               05  FILLER     PIC X(7) VALUE "#60 DAY".
               05  FILLER     PIC X(7) VALUE "$60 DAY".
               05  FILLER     PIC X(7) VALUE "FLD 06 ".
               05  FILLER     PIC X(7) VALUE "#90 DAY".
               05  FILLER     PIC X(7) VALUE "$90 DAY".
               05  FILLER     PIC X(7) VALUE "FLD 09 ".
               05  FILLER     PIC X(7) VALUE "FLD 10 ".
               05  FILLER     PIC X(7) VALUE "FLD 11 ".
               05  FILLER     PIC X(7) VALUE "FLD 12 ".
               05  FILLER     PIC X(7) VALUE "FLD 13 ".
       01  WORK-FIELD-TABLE2 REDEFINES WORK-FIELD-TABLE.
           03  WORK-FIELD-DESC  PIC X(7) OCCURS 13 TIMES.


       01  ONE               PIC S9.
       01  SUB               PIC 99     COMP-3 VALUE 0.
       01  ELE               PIC 99     VALUE 0.

       01  ACTION            VALUE SPACES.
           03  CHKEY         PIC X(4).
           03  CHNO          PIC 9(2).
           03  CHNO-X REDEFINES CHNO
                             PIC X(2).
       01  FORM-NUM1     REDEFINES ACTION.
           03  FORM-KEY1     PIC X(4).
           03  FORM-P1       PIC X.
           03  FORM-SEL1     PIC 9.
       01  FORM-NUM2      REDEFINES ACTION.
           03  FORM-KEY2     PIC X(3).
           03  FORM-P2       PIC X.
           03  FORM-SEL2     PIC 99.
               88  FORM-2-DIGITS  VALUE 10.
       01  FORM-MAX1         PIC 99 VALUE 9.
       01  FORM-MAX2         PIC 99 VALUE 11.
       01  CURRENT-FORM      PIC 99 VALUE 4.

       01  IO-TYPE           PIC X(3)   VALUE SPACES.
       01  NEW-FLAG          PIC 9      VALUE 0.
       01  FIRST-SCAN        PIC 9      VALUE 0.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    'PAGE-CHANGE-FLAG' IS FOR KNOWING IF COMING FROM ANOTHER PAGE
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  PAGE-CHANGE-FLAG      PIC X(1)    VALUE "N".
           88  PAGE-CHANGE-NO                VALUE "N".
           88  PAGE-CHANGE-YES               VALUE "Y".


       01  WRMAIN-LINK        VALUE SPACES.
           03  SAVEKEY       PIC X(10).
           03  NEW-REC       PIC X.
           03  ERRCD         PIC X.
           03  FORMSEL       PIC 99.
           03  WRMAIN-WINDOW-HANDLE PIC X(10).

       01  HOLD-WR1-KEY    PIC X(10)    VALUE SPACES.

      *    MOVE " ENTER YYMMBBBB OR F6 TO SCAN" TO MESS.
       01  SCAN-NUM-KEY       PIC 9(8).
       01  FILLER REDEFINES SCAN-NUM-KEY.
           03  SCAN-WR-YY   PIC 9(2).
           03  SCAN-WR-MM   PIC 9(2).
           03  SCAN-WR-BRNO   PIC 9(4).

       01  ORIGINAL-WR-REC    PIC X(WR-SIZE)   VALUE SPACES.
       01  CHANGED-WR-REC     PIC X(WR-SIZE)   VALUE SPACES.

       01  COMPUTED-FIELDS               OCCURS 2.
           03  WS-WR-GAIN-90-DAY         PIC S9(7)V99.
       01  COMPUTED-FIELDS2.
           03  WS-WR-PCT-30-DAYS         PIC S9(3)V99.
           03  WS-WR-PCT-60-DAYS         PIC S9(3)V99.
           03  WS-WR-PCT-90-DAYS         PIC S9(3)V99.
           03  WS-WR-NO-DEL-ACCTS        PIC S9(5).
           03  WS-WR-AMT-DEL-ACCTS       PIC S9(9)V99.
           03  WS-WR-PCT-DEL-ACCTS       PIC S9(3)V99.


      ******************************
      *    SCREEN DISPLAY BUFFER   *
      ******************************
       01  DISPLAY-BUF     VALUE SPACES.
           03  F01         PIC Z,ZZZ,ZZZ-.
           03  F02         PIC ZZZ,ZZZ,ZZZ.ZZ-.
           03  F03YTD      PIC ZZZ.ZZ-.
           03  F04         PIC Z,ZZZ,ZZZ-.
           03  F05         PIC ZZZ,ZZZ,ZZZ.ZZ-.
           03  F06YTD      PIC ZZZ.ZZ-.
           03  F07         PIC Z,ZZZ,ZZZ-.
           03  F08         PIC ZZZ,ZZZ,ZZZ.ZZ-.
           03  F081        PIC ZZZ,ZZZ,ZZZ.ZZ-.
           03  F082        PIC ZZZ,ZZZ,ZZZ.ZZ-.
           03  F09MTD      PIC ZZZ,ZZZ,ZZZ.ZZ-.
           03  F091YTD     PIC ZZZ,ZZZ,ZZZ.ZZ-.

           03  F10YTD      PIC ZZZ.ZZ-.
           03  F11YTD      PIC Z,ZZZ,ZZZ-.
           03  F12YTD      PIC ZZZ,ZZZ,ZZZ.ZZ-.
           03  F13YTD      PIC ZZZ.ZZ-.
           03  OUTS-BAL    PIC ZZZ,ZZZ,ZZZ.ZZ-.

       01  DISPLAY-LIST.
           03  FILLER      PIC X(46) VALUE
           "010 020 030YTD 040 050 060YTD 070 080 081 082 ".
           03  FILLER      PIC X(42) VALUE
           "090MTD 091YTD 100YTD 110YTD 120YTD 130YTD ".
           03  FILLER      PIC X(8) VALUE
           "OUTSBAL.".

      ********************************
      *    ELEMENT SPECIFICATIONS    *
      ********************************
       78  MAX                      VALUE 12.
       01  SPEC-TABLE.
      * 01.  WR-NO-30-DAY-ACCTS
           03  FILLER      PIC X(8) VALUE "E NS070 ".
      * 02.  WR-AMT-30-DAY-ACCTS
           03  FILLER      PIC X(8) VALUE "E NS092 ".
      * 03.  WS-WR-PCT-30-DAYS
           03  FILLER      PIC X(8) VALUE "E NS032N".
      * 04.  WR-NO-60-DAY-ACCTS
           03  FILLER      PIC X(8) VALUE "E NS070 ".
      * 05.  WR-AMT-60-DAY-ACCTS
           03  FILLER      PIC X(8) VALUE "E NS092 ".
      * 06.  WS-WR-PCT-60-DAYS
           03  FILLER      PIC X(8) VALUE "E NS032N".
      * 07.  WR-NO-90-DAY-ACCTS
           03  FILLER      PIC X(8) VALUE "E NS070 ".
      * 08.  WR-AMT-90-DAY-ACCTS
           03  FILLER      PIC X(8) VALUE "E NS092 ".
      * 09.  WS-WR-PCT-90-DAYS
           03  FILLER      PIC X(8) VALUE "E NS032N".
      * 10.  WS-WR-NO-DEL-ACCTS
           03  FILLER      PIC X(8) VALUE "E NS070N".
      * 11.  WR-AMT-DEL-ACCTS
           03  FILLER      PIC X(8) VALUE "E NS092N".
      * 12.  WS-WR-PCT-DEL-ACCTS
           03  FILLER      PIC X(8) VALUE "E NS032N".

       01  SPEC-TABLE2     REDEFINES SPEC-TABLE.
           03  SPEC-REC    OCCURS MAX TIMES.
               05  SPEC    PIC X(7).
               05  CHFG    PIC X.
      *

       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/WRMAN4_DEF.CPY".
       COPY "LIBMN/WRMAN4_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       01 FORM-LINK-AREA    PIC X(2000).

       SCREEN SECTION.
       COPY "LIBMN/WRMAN4_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME FORM-LINK-AREA EXIT-PATHNAME.

      *********************************************
       MAIN-PROGRAM SECTION.
      *********************************************
           PERFORM SQL-CONNECT.

           MOVE "WRMAN4" TO VDU-FORM-NAME.

       COPY "LIBGB/CHKSEC.CPY".
           MOVE " " TO VDU-WIMENU-SELECTION
                       VDU-PGMENU-SELECTION.

           MOVE FORM-LINK-AREA TO WRMAIN-LINK.

       MAIN-MODULE.
           PERFORM FORM-RESET.

           PERFORM REC-WR-MODULE.
           IF IO-TYPE = "END"
              MOVE "X" TO ERRCD
              GO TO END-ROUTINE.
           IF IO-TYPE = "CLR"
              GO TO MAIN-MODULE.

           PERFORM ENTRY-MODULE.

           IF IO-TYPE = "RWR"
              PERFORM WRITE-MODULE.

           IF NEW-REC = "Y"
              MOVE "P" TO FORM-P2
              COMPUTE FORM-SEL2 = (CURRENT-FORM + 1)
              IF FORM-SEL2 > FORM-MAX2
                 MOVE "N" TO NEW-REC
                 GO TO MAIN-MODULE.
           IF FORM-SEL1 NOT = 0
              IF FORM-P1 = "P"
                 MOVE WR1-KEY TO SAVEKEY
                 MOVE FORM-SEL1 TO FORMSEL
                 GO TO END-ROUTINE.
           IF FORM-SEL2 NOT = 0
              IF FORM-P2 = "P"
                 MOVE WR1-KEY TO SAVEKEY
                 MOVE FORM-SEL2 TO FORMSEL
                 GO TO END-ROUTINE.

           MOVE "N" TO NEW-REC.

           GO TO MAIN-MODULE.

      **********************************************
       REC-WR-MODULE SECTION.
      **********************************************
           MOVE 0 TO WR-YYYYMM WR-BRNO FIRST-SCAN.
           MOVE " " TO IO-TYPE.
           IF SAVEKEY NOT = " "
              MOVE "Y" TO PAGE-CHANGE-FLAG
              MOVE SAVEKEY TO WR1-KEY
              MOVE " " TO SAVEKEY
              MOVE "SN N040MMYY." TO VDU
              MOVE WR-YYYYMM TO DATE-YYYYMM
              PERFORM CONVERT-YYYYMM-TO-MMDDYY
              MOVE DATE-MMDDYY-MM TO DATE-MMYY-MM
              MOVE DATE-MMDDYY-YY TO DATE-MMYY-YY
              MOVE DATE-MMYY TO VDUNUM0
              PERFORM SCREENIO
              MOVE "SN N040BRNO." TO VDU
              MOVE WR-BRNO TO VDUNUM0
              PERFORM SCREENIO
              GO TO READ-WRFILE.

           MOVE "N" TO PAGE-CHANGE-FLAG.
           MOVE 0   TO NEW-FLAG.

       REC-WR-DATE.
           MOVE "EN N040MMYY." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
             MOVE "END" TO IO-TYPE
             GO TO END-REC-WR-MODULE.
           IF VDUSTATUS = "1<"
             PERFORM ENTER-SCANKEY
             GO TO END-REC-WR-MODULE.
           IF VDUSTATUS NOT = "00"
             GO TO REC-WR-DATE.
           MOVE VDUNUM0 TO DATE-MMYY.
           IF DATE-MMYY = 0
              GO TO REC-WR-DATE.
           PERFORM CONVERT-MMYY-TO-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO NUM-DATE.
           MOVE 1 TO NUM-DA.
           PERFORM TSTDAT.
           IF NUM-DATE = 0
              GO TO REC-WR-DATE.
      * GLB 02/26/04
           MOVE NUM-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-YYYYMM.
           MOVE DATE-YYYYMM TO WR-YYYYMM.
       REC-WR-BRNO.
           MOVE "EN N040BRNO." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" OR "1U"
              GO TO REC-WR-DATE.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-REC-WR-MODULE.
           IF VDUSTATUS = "1<"
              PERFORM ENTER-SCANKEY
              GO TO END-REC-WR-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO REC-WR-BRNO.
           MOVE VDUNUM0 TO WR-BRNO.
           IF WR-BRNO = 0
              GO TO REC-WR-BRNO.
           MOVE WR1-KEY TO HOLD-WR1-KEY.
       READ-WRFILE.
           PERFORM OPEN-WR1-FILE.
           PERFORM READ-WR1-FILE.
           PERFORM CLOSE-WR1-FILE.
           IF IO-BAD
              MOVE "NOT ON FILE!" TO MESS
              PERFORM SEND-MESS
              GO TO REC-WR-DATE.

           MOVE WR-REC TO ORIGINAL-WR-REC.
           PERFORM DISPLAY-FIELDS.
           IF NEW-REC = "Y"
              MOVE 1 TO NEW-FLAG
           ELSE
              MOVE 0 TO NEW-FLAG.

       END-REC-WR-MODULE.
           EXIT.

      **********************************************
       ENTER-SCANKEY SECTION.
      **********************************************
           MOVE " ENTER YYMMBBBB OR F6 TO SCAN" TO MESS.
           PERFORM CALL-TMMSG.

           PERFORM OPEN-WR1-FILE.
           PERFORM START-WR1-FILE.
       ENTER-KEY.
           MOVE "EN N080SCANKEY." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCANKEY.
           IF VDUSTATUS = "1<"
              GO TO READ-FILES-NEXT.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-KEY.
           IF VDUNUM0 = 0
              IF FIRST-SCAN NOT = 0
                 MOVE WR1-KEY TO HOLD-WR1-KEY
                 MOVE WR-REC TO ORIGINAL-WR-REC
                 GO TO CLOSE-SCANKEY
              ELSE
                 GO TO ENTER-KEY.

           MOVE VDUNUM0 TO SCAN-NUM-KEY.

           MOVE SCAN-WR-MM TO  DATE-MMDDYY-MM.
           MOVE SCAN-WR-YY TO  DATE-MMDDYY-YY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMM.

           MOVE DATE-YYYYMM  TO WR-YYYYMM.
           MOVE SCAN-WR-BRNO TO WR-BRNO.


           PERFORM START-WR1-FILE.
       READ-FILES-NEXT.
           PERFORM READ-WR1-FILE-NEXT.
           IF IO-BAD
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCANKEY.
      *    UNLOCK WR-FILE.

           MOVE "SN N040MMYY." TO VDU.
           MOVE WR-YYYYMM TO DATE-YYYYMM.
           PERFORM CONVERT-YYYYMM-TO-MMDDYY.
           MOVE DATE-MMDDYY-MM TO DATE-MMYY-MM.
           MOVE DATE-MMDDYY-YY TO DATE-MMYY-YY.
           MOVE DATE-MMYY TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SN N040BRNO." TO VDU.
           MOVE WR-BRNO TO VDUNUM0.
           PERFORM SCREENIO.

           PERFORM DISPLAY-FIELDS.

           MOVE 1 TO FIRST-SCAN.
           GO TO ENTER-KEY.
       CLOSE-SCANKEY.
           MOVE 0 TO FIRST-SCAN.
           PERFORM CLOSE-WR1-FILE.

           MOVE SPACES TO MESS.
           PERFORM CALL-TMMSG.
       END-SCANKEY.
           EXIT.

      ************************************************
      *   ENTRY-MODULE
      *   NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *              1 - INITIAL NEW RECORD ENTRY
      *              2 - MODIFY NEW RECORD
      ************************************************
       ENTRY-MODULE SECTION.
           MOVE SPACES TO IO-TYPE.
           MOVE 1 TO ONE.
           MOVE 0 TO ELE.
       CREATE-ROUTINE.
           IF NEW-FLAG NOT = 1
              GO TO MODIFY-ROUTINE.
           ADD ONE TO ELE.
           IF ELE < 1
              MOVE 1 TO ELE ONE.
           IF ELE NOT > MAX
              GO TO ENTER-ELE.
           MOVE 2 TO NEW-FLAG.
       MODIFY-ROUTINE.
           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO MODIFY-ROUTINE.
           MOVE VDUBUF TO ACTION.
           IF ACTION = SPACES
              MOVE "RWR" TO IO-TYPE
              GO TO EXIT-ENTRY-MODULE.
           IF ACTION = "DELETE"
      *       MOVE "DEL" TO IO-TYPE
      *       GO TO END-ENTRY-MODULE.
              MOVE "DELETE RECORD FROM PAGE NO. 1"
              TO MESS PERFORM SEND-MESS
              GO TO MODIFY-ROUTINE.

      ***************************************************************
      * FORM-NUM1 CHECKS FOR PAGES 1 THRU 9
      ***************************************************************
           IF FORM-KEY1 = " " AND FORM-P1 = "P" AND FORM-SEL1 NUMERIC
              IF FORM-SEL1 = 0 OR
                 FORM-SEL1 = CURRENT-FORM  OR FORM-SEL1 > FORM-MAX1
                 GO TO MODIFY-ROUTINE
              ELSE
                 MOVE "RWR" TO IO-TYPE
                 GO TO EXIT-ENTRY-MODULE.
      * FORM-NUM2 CHECKS FOR PAGES 10 THRU 99
           IF FORM-KEY2 = " " AND FORM-P2 = "P" AND FORM-SEL2 NUMERIC
              IF FORM-SEL2 = 0 OR
                 FORM-SEL2 = CURRENT-FORM  OR FORM-SEL2 > FORM-MAX2
                    GO TO MODIFY-ROUTINE
                 ELSE
                    MOVE "RWR" TO IO-TYPE
                    GO TO EXIT-ENTRY-MODULE.


           INSPECT ACTION REPLACING ALL SPACES BY ZEROS.
           IF CHNO-X NOT NUMERIC
              GO TO MODIFY-ROUTINE.
           IF CHNO = 0
              GO TO MODIFY-ROUTINE.
           IF CHNO > MAX
              GO TO MODIFY-ROUTINE.
           MOVE CHNO TO ELE.
       ENTER-ELE.
           IF CHKEY NOT = ZEROS
              IF CHFG(ELE) = " "
                 IF NEW-FLAG NOT = 1
                    GO TO MODIFY-ROUTINE.
           IF CHFG(ELE) = "N"
              IF NEW-FLAG = 1
                 GO TO CREATE-ROUTINE
              ELSE
                 GO TO MODIFY-ROUTINE.

           MOVE SPEC(ELE) TO VDU.
           MOVE ELE TO VDUROW.
           MOVE 0 TO VDUCOL.
           MOVE "." TO VDUDECIMAL.
           PERFORM SCREENIO.
       TEST-UP.
           IF VDUSTATUS = "1U"
              IF NEW-FLAG = 1
                 MOVE -1 TO ONE
                 GO TO CREATE-ROUTINE.
       TEST-DOWN.
           IF VDUSTATUS = "1D"
              MOVE 1 TO ONE
              GO TO GOTO-FIELD.
       TEST-F1.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-ELE.
           MOVE 1 TO ONE.
       GOTO-FIELD.
           GO TO F-01 F-02 F-03 F-04 F-05 F-06 F-07 F-08
                 F-09 F-10 F-11 F-12 F-13
                 DEPENDING ON ELE.
           GO TO END-ENTRY-MODULE.

      *=================================================================
      *
      *    S E T - R E C O R D - E L E M E N T S
      *
      *=================================================================
       F-01.
           MOVE VDUNUM0 TO WR-NO-30-DAY-ACCTS.
           GO TO CREATE-ROUTINE.
       F-02.
           MOVE VDUNUM2 TO WR-AMT-30-DAY-ACCTS.
           GO TO CREATE-ROUTINE.
       F-03.
      *    MOVE VDUNUM2 TO WS-WR-PCT-30-DAYS.
           GO TO CREATE-ROUTINE.
       F-04.
           MOVE VDUNUM0 TO WR-NO-60-DAY-ACCTS.
           GO TO CREATE-ROUTINE.
       F-05.
           MOVE VDUNUM2 TO WR-AMT-60-DAY-ACCTS.
           GO TO CREATE-ROUTINE.
       F-06.
      *    MOVE VDUNUM2 TO WS-WR-PCT-60-DAYS.
           GO TO CREATE-ROUTINE.
       F-07.
           MOVE VDUNUM0 TO WR-NO-90-DAY-ACCTS.
           GO TO CREATE-ROUTINE.
       F-08.
           MOVE VDUNUM2 TO WR-AMT-90-DAY-ACCTS.
       F-081.
           MOVE "E NS092  1." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-081.
           MOVE VDUNUM2 TO WR-ELM-AMT-90-DAY-ACCTS.
       F-082.
           MOVE "E NS092  2." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO F-081.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-082.
           MOVE VDUNUM2 TO WR-ELY-AMT-90-DAY-ACCTS.
           GO TO CREATE-ROUTINE.
       F-09.
           GO TO CREATE-ROUTINE.
       F-10.
           GO TO CREATE-ROUTINE.
       F-11.
           GO TO CREATE-ROUTINE.
       F-12.
           GO TO CREATE-ROUTINE.
       F-13.
           GO TO CREATE-ROUTINE.

       END-ENTRY-MODULE.
           MOVE 0 TO FORM-SEL1 FORM-SEL2.
       EXIT-ENTRY-MODULE.
           EXIT.

      ************************************************
       WRITE-MODULE SECTION.
      ************************************************
           PERFORM OPEN-WR1-FILE.
           MOVE WR-REC TO CHANGED-WR-REC.
           IF ORIGINAL-WR-REC = CHANGED-WR-REC
              GO TO END-WRITE-MODULE.
           PERFORM READ-WR1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-WRITE-MODULE.
           IF WR-REC NOT = ORIGINAL-WR-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-WRITE-MODULE.

      * GLB 02/26/04
           PERFORM LOG-MAINTENANCE-CHGS.

           MOVE CHANGED-WR-REC TO WR-REC.
           PERFORM REWRITE-WR1-FILE.
           GO TO END-WRITE-MODULE.

       END-WRITE-MODULE.
           PERFORM CLOSE-WR1-FILE.

      ************************************************
       LOG-MAINTENANCE-CHGS SECTION.
      ************************************************
           MOVE ORIGINAL-WR-REC TO WROR-REC.
           MOVE CHANGED-WR-REC  TO WRCH-REC.

           MOVE SPACES  TO WORK-ERRLOG-KEY.
           MOVE WR1-KEY TO WORK-ERRLOG-WR1.
           MOVE "TO"    TO  WORK-ERRLOG-TO-X.

           IF WROR-NO-30-DAY-ACCTS NOT = WRCH-NO-30-DAY-ACCTS
             MOVE "01 "                 TO WORK-ERRLOG-FIELD
             MOVE 1                     TO SUB
             MOVE WORK-FIELD-DESC(SUB)  TO WORK-ERRLOG-DESC
             MOVE WROR-NO-30-DAY-ACCTS      TO WORK-ERRLOG-ORG-N0
             MOVE WRCH-NO-30-DAY-ACCTS      TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF WROR-AMT-30-DAY-ACCTS NOT = WRCH-AMT-30-DAY-ACCTS
             MOVE "02 "                 TO WORK-ERRLOG-FIELD
             MOVE 2                     TO SUB
             MOVE WORK-FIELD-DESC(SUB)  TO WORK-ERRLOG-DESC
             MOVE WROR-AMT-30-DAY-ACCTS      TO WORK-ERRLOG-ORG-N2
             MOVE WRCH-AMT-30-DAY-ACCTS      TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF WROR-NO-60-DAY-ACCTS NOT = WRCH-NO-60-DAY-ACCTS
             MOVE "04 "                 TO WORK-ERRLOG-FIELD
             MOVE 4                     TO SUB
             MOVE WORK-FIELD-DESC(SUB)  TO WORK-ERRLOG-DESC
             MOVE WROR-NO-60-DAY-ACCTS      TO WORK-ERRLOG-ORG-N0
             MOVE WRCH-NO-60-DAY-ACCTS      TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF WROR-AMT-60-DAY-ACCTS NOT = WRCH-AMT-60-DAY-ACCTS
             MOVE "05 "                 TO WORK-ERRLOG-FIELD
             MOVE 5                     TO SUB
             MOVE WORK-FIELD-DESC(SUB)  TO WORK-ERRLOG-DESC
             MOVE WROR-AMT-60-DAY-ACCTS      TO WORK-ERRLOG-ORG-N2
             MOVE WRCH-AMT-60-DAY-ACCTS      TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF WROR-NO-90-DAY-ACCTS NOT = WRCH-NO-90-DAY-ACCTS
             MOVE "07 "                 TO WORK-ERRLOG-FIELD
             MOVE 7                     TO SUB
             MOVE WORK-FIELD-DESC(SUB)  TO WORK-ERRLOG-DESC
             MOVE WROR-NO-90-DAY-ACCTS      TO WORK-ERRLOG-ORG-N0
             MOVE WRCH-NO-90-DAY-ACCTS      TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF WROR-AMT-90-DAY-ACCTS NOT = WRCH-AMT-90-DAY-ACCTS
             MOVE "08 "                 TO WORK-ERRLOG-FIELD
             MOVE 8                     TO SUB
             MOVE WORK-FIELD-DESC(SUB)  TO WORK-ERRLOG-DESC
             MOVE WROR-AMT-90-DAY-ACCTS      TO WORK-ERRLOG-ORG-N2
             MOVE WRCH-AMT-90-DAY-ACCTS      TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.
           IF WROR-ELM-AMT-90-DAY-ACCTS NOT = WRCH-ELM-AMT-90-DAY-ACCTS
             MOVE "081"                 TO WORK-ERRLOG-FIELD
             MOVE 8                     TO SUB
             MOVE WORK-FIELD-DESC(SUB)  TO WORK-ERRLOG-DESC
             MOVE WROR-ELM-AMT-90-DAY-ACCTS      TO WORK-ERRLOG-ORG-N2
             MOVE WRCH-ELM-AMT-90-DAY-ACCTS      TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.
           IF WROR-ELY-AMT-90-DAY-ACCTS NOT = WRCH-ELY-AMT-90-DAY-ACCTS
             MOVE "082"                 TO WORK-ERRLOG-FIELD
             MOVE 8                     TO SUB
             MOVE WORK-FIELD-DESC(SUB)  TO WORK-ERRLOG-DESC
             MOVE WROR-ELY-AMT-90-DAY-ACCTS      TO WORK-ERRLOG-ORG-N2
             MOVE WRCH-ELY-AMT-90-DAY-ACCTS      TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.


      ************************************************
       LOG-FIELD-CHANGE SECTION.
      ************************************************
      * CODE TO WRITE CHANGE FILE.

           MOVE "X" TO ERRLOGW-ACTION.
           MOVE WORK-ERRLOG-KEY TO ERRLOGW-EOM-MSG.
           MOVE FORM-PATHNAME TO ERRLOGW-PROGX.

           MOVE "CL/ERRLOG" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PROGX.


      ************************************************
       DISPLAY-FIELDS SECTION.
      ************************************************
           MOVE SPACES TO DISPLAY-BUF.

           MOVE WR-NO-30-DAY-ACCTS           TO F01.

           MOVE WR-AMT-30-DAY-ACCTS          TO F02.

           COMPUTE WS-WR-PCT-30-DAYS ROUNDED =
              WR-AMT-30-DAY-ACCTS / WR-OUTS-BALANCE * 100.
           MOVE WS-WR-PCT-30-DAYS            TO F03YTD.

           MOVE WR-NO-60-DAY-ACCTS           TO F04.

           MOVE WR-AMT-60-DAY-ACCTS          TO F05.

           COMPUTE WS-WR-PCT-60-DAYS ROUNDED =
              WR-AMT-60-DAY-ACCTS / WR-OUTS-BALANCE * 100.
           MOVE WS-WR-PCT-60-DAYS            TO F06YTD.

           MOVE WR-NO-90-DAY-ACCTS           TO F07.

           MOVE WR-AMT-90-DAY-ACCTS          TO F08.
           MOVE WR-ELM-AMT-90-DAY-ACCTS      TO F081.
           MOVE WR-ELY-AMT-90-DAY-ACCTS      TO F082.

           COMPUTE WS-WR-GAIN-90-DAY(1) =
              WR-AMT-90-DAY-ACCTS - WR-ELM-AMT-90-DAY-ACCTS.
           MOVE WS-WR-GAIN-90-DAY(1)         TO F09MTD.
           COMPUTE WS-WR-GAIN-90-DAY(2) =
              WR-AMT-90-DAY-ACCTS - WR-ELY-AMT-90-DAY-ACCTS.
           MOVE WS-WR-GAIN-90-DAY(2)         TO F091YTD.

           COMPUTE WS-WR-PCT-90-DAYS ROUNDED =
              WR-AMT-90-DAY-ACCTS / WR-OUTS-BALANCE * 100.
           MOVE WS-WR-PCT-90-DAYS            TO F10YTD.

           COMPUTE WS-WR-NO-DEL-ACCTS =
              WR-NO-30-DAY-ACCTS +
              WR-NO-60-DAY-ACCTS +
              WR-NO-90-DAY-ACCTS.
           MOVE WS-WR-NO-DEL-ACCTS           TO F11YTD.

           COMPUTE WS-WR-AMT-DEL-ACCTS =
              WR-AMT-30-DAY-ACCTS +
              WR-AMT-60-DAY-ACCTS +
              WR-AMT-90-DAY-ACCTS.
           MOVE WS-WR-AMT-DEL-ACCTS           TO F12YTD.

           COMPUTE WS-WR-PCT-DEL-ACCTS ROUNDED =
              WS-WR-AMT-DEL-ACCTS / WR-OUTS-BALANCE * 100.
           MOVE WS-WR-PCT-DEL-ACCTS           TO F13YTD.

           MOVE WR-OUTS-BALANCE               TO OUTS-BAL.

       DISPLAY-FIELDS-EXIT.
           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *********************************************
       FORM-RESET SECTION.
      *********************************************
           MOVE EXT-CONAME-CONAME TO WRMAN4-CONAME.
           MOVE EXT-CONAME-SYSDATE TO WRMAN4-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY WRMAN4-SCREEN UPON WRMAIN-WINDOW-HANDLE.
           MOVE WRMAN4-FORM-FIELDS TO WR-BUF.
           MOVE WRMAN4-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *********************************************
       SETUP-LINK-INFO SECTION.
      *********************************************
           MOVE WRMAN4-HELP--FILE TO HELP-LINK-FILE.
           MOVE WRMAN4-HELP--DIR TO HELP-LINK-DIR.

      *********************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/WRMAN4_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ************************************************
       MISC-PARAGRAPHS SECTION.
      ************************************************
       COPY "LIBGB/ACCESS.CPY".
       RECORD-DELETED.
           MOVE " RECORD WAS DELETED!" TO MESS.
           PERFORM SEND-MESS.
       RECORD-CHANGED.
           MOVE " CHANGES WERE MADE TO THIS RECORD!" TO MESS.
           PERFORM SEND-MESS.
       CHANGE-AGAIN.
           MOVE " PLEASE RE-INPUT YOUR CHANGES!" TO MESS.
           PERFORM SEND-MESS.
       ALREADY-CREATED.
           MOVE " RECORD HAS BEEN ALREADY CREATED!" TO MESS.
           PERFORM SEND-MESS.
       UNDERGONE-CHANGE.
           MOVE
           " THIS RECORD HAS UNDERGONE CHANGE BY ANOTHER USER,"
           TO MESS.
           PERFORM SEND-MESS.
       ACTIVE-RECORD.
           MOVE " ACTIVE CUSTOMER CANNOT BE DELETED" TO MESS.
           PERFORM SEND-MESS.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".

      ************************************************
       IO-ROUTINES SECTION.
      ************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-WR1-FILE.
       COPY "LIBSP/SPWRGS_SQL.CPY".
       COPY "LIBSP/SPWR1IN.CPY".

       COPY "LIBGB/FERRORS.CPY".
      ************************************************
       END-ROUTINE SECTION.
      ************************************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY WRMAN4-SCREEN.
           MOVE WRMAIN-LINK TO FORM-LINK-AREA.
           EXIT PROGRAM.
