       IDENTIFICATION DIVISION.
       PROGRAM-ID.      WRMAN6.
      **************************************************************************
      *                 (MN)
      *
      *  DESCRIPTION:   WORLD RATING SHEET FILE MAINTENANCE - PAGE 6
      *                 WCBC PG 2
      * REV:
      * BLV 040119
      * GLB 040219 FIX BUGS BALANCING TO WORLD S/F RATING SHEET
      * GLB 040226 ADD MAINTENANCE LOG
      * GLB 040311 FIX UPDATE OF WR-WCBC-ELM-AMT-90-DAYS & ELY
      * GLB 040318 GENERAL CLEANUP WHILE CREATING HELP
      * GLB 040621 MODIFY COMPUTATION OF LINE 26, LINE 35, LINE 37
      *            (TOTAL INCOME, BAD DEBT)
      * GLB 040621 CHANGE DESCRIPTION LINE 35 FROM "BAD DEBT" TO
      *            "TOTAL BAD DEBT EXPENSE"
      * BLM 100511 CORRECT DISPLAY OF WCBC TOTAL BAD DEBT EXPENSE, NOW SHOULD
      *            MATCH LINE 35 ON SF RATING SHEET, WORLD PR# 687
      * BLM 100521 CORRECT DISPLAY OF WCBC BAD DEBT TO CUR, SHOULD NOW MATCH
      *            MATCH LINE 51 ON SF RATING SHEET, ADDED YTD. CORRECTED
      *            COMPUTATION OF MTD & YTD PROFIT, SHOULD MATCH LINES 37
      *            & 60 ON SF RATING SHEET.  WORLD PR# 687
      *  CS 160509 CHANGE ORIGINAL,CHANGED REC TO INCLUDE WR IN NAME
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
           "@(#)MN/WRMAN6.CBL S34 06/08/21-11:45:21 >".
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
               05  FILLER     PIC X(7) VALUE "$RECOVR".
               05  FILLER     PIC X(7) VALUE "$90 DAY".
               05  FILLER     PIC X(7) VALUE "$CHGOFF".
               05  FILLER     PIC X(7) VALUE "$BANKRT".
               05  FILLER     PIC X(7) VALUE "FLD 05 ".
               05  FILLER     PIC X(7) VALUE "FLD 06 ".
               05  FILLER     PIC X(7) VALUE "#30 DAY".
               05  FILLER     PIC X(7) VALUE "$30 DAY".
               05  FILLER     PIC X(7) VALUE "#60 DAY".
               05  FILLER     PIC X(7) VALUE "$60 DAY".
               05  FILLER     PIC X(7) VALUE "#90 DAY".
               05  FILLER     PIC X(7) VALUE "$90 DAY".
               05  FILLER     PIC X(7) VALUE "FLD 13 ".
               05  FILLER     PIC X(7) VALUE "FLD 14 ".
               05  FILLER     PIC X(7) VALUE "FLD 15 ".
               05  FILLER     PIC X(7) VALUE "FLD 16 ".
               05  FILLER     PIC X(7) VALUE "FLD 17 ".
               05  FILLER     PIC X(7) VALUE "FLD 18 ".
       01  WORK-FIELD-TABLE2 REDEFINES WORK-FIELD-TABLE.
           03  WORK-FIELD-DESC  PIC X(7) OCCURS 18 TIMES.


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
       01  CURRENT-FORM      PIC 99 VALUE 6.

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

       01  HOLD-WR1-KEY.
           03  HOLD-WR-YYYYMM PIC 9(6).
           03  HOLD-WR-BRNO   PIC 9(4).

      *    MOVE " ENTER YYMMBBBB OR F6 TO SCAN" TO MESS.
       01  SCAN-NUM-KEY       PIC 9(8).
       01  FILLER REDEFINES SCAN-NUM-KEY.
           03  SCAN-WR-YY     PIC 9(2).
           03  SCAN-WR-MM     PIC 9(2).
           03  SCAN-WR-BRNO   PIC 9(4).

       01  ORIGINAL-WR-REC    PIC X(WR-SIZE)   VALUE SPACES.
       01  CHANGED-WR-REC   PIC X(WR-SIZE)   VALUE SPACES.

       01  COMPUTED-FIELDS               OCCURS 2.
           03  WS-WR-WCBC-TOTAL-INCOME   PIC S9(9)V99.
           03  WS-WR-WCBC-BAD-DEBT       PIC S9(9)V99.
           03  WS-WR-WCBC-PROFIT         PIC S9(9)V99.
           03  WS-WR-WCBC-INCR-DECR-90   PIC S9(9)V99.
       01  WS-WR-WCBC-TOT-NO-DEL         PIC S9(5).
       01  WS-WR-WCBC-TOT-AMT-DEL        PIC S9(9)V99.
       01  WS-WR-WCBC-PCT-NO-DEL         PIC S9(3)V99.
       01  WS-WR-WCBC-PCT-AMT-DEL        PIC S9(3)V99.
       01  WS-WR-WCBC-PCT-BAD-DEBT-MTD   PIC S9(3)V99.
       01  WS-WR-WCBC-PCT-BAD-DEBT-YTD   PIC S9(3)V99.


      ******************************
      *    SCREEN DISPLAY BUFFER   *
      ******************************
       01  DISPLAY-BUF     VALUE SPACES.
           03  F01         PIC Z,ZZZ,ZZZ.ZZ-.
           03  F011        PIC Z,ZZZ,ZZZ.ZZ-.
           03  F02         PIC ZZZ,ZZZ,ZZZ.ZZ-.
           03  F021        PIC ZZZ,ZZZ,ZZZ.ZZ-.
           03  F022        PIC ZZZ,ZZZ,ZZZ.ZZ-.
           03  F023        PIC ZZZ,ZZZ,ZZZ.ZZ-.
           03  F03         PIC Z,ZZZ,ZZZ.ZZ-.
           03  F031        PIC Z,ZZZ,ZZZ.ZZ-.
           03  F04         PIC Z,ZZZ,ZZZ.ZZ-.
           03  F041        PIC Z,ZZZ,ZZZ.ZZ-.
           03  F05MTD      PIC ZZZ,ZZZ,ZZZ.ZZ-.
           03  F051YTD     PIC ZZZ,ZZZ,ZZZ.ZZ-.
           03  F06MTD      PIC ZZZ,ZZZ,ZZZ.ZZ-.
           03  F061YTD     PIC ZZZ,ZZZ,ZZZ.ZZ-.
           03  F07         PIC Z,ZZZ,ZZZ-.
           03  F08         PIC ZZZ,ZZZ,ZZZ.ZZ-.
           03  F09         PIC Z,ZZZ,ZZZ-.
           03  F10         PIC ZZZ,ZZZ,ZZZ.ZZ-.
           03  F11         PIC Z,ZZZ,ZZZ-.
           03  F12         PIC ZZZ,ZZZ,ZZZ.ZZ-.
           03  F131YTD     PIC Z,ZZZ,ZZZ-.
           03  F141YTD     PIC ZZZ,ZZZ,ZZZ.ZZ-.
           03  F151YTD     PIC ZZZ.ZZ-.
           03  F161YTD     PIC ZZZ.ZZ-.
           03  F171MTD     PIC ZZZ.ZZ-.
           03  F171YTD     PIC ZZZ.ZZ-.
           03  F18MTD      PIC ZZZ,ZZZ,ZZZ.ZZ-.
           03  F181YTD     PIC ZZZ,ZZZ,ZZZ.ZZ-.

       01  DISPLAY-LIST.
           03  FILLER      PIC X(40) VALUE
           "010 011 020 021 022 023 030 031 040 041".
           03  FILLER      PIC X(44) VALUE
           "050MTD 051YTD 060MTD 061YTD 070 080 090 100 ".
           03  FILLER      PIC X(22) VALUE
           "110 120 131YTD 141YTD ".
           03  FILLER      PIC X(42) VALUE
           "151YTD 161YTD 171MTD 171YTD 180MTD 181YTD.".

      ********************************
      *    ELEMENT SPECIFICATIONS    *
      ********************************
       78  MAX                      VALUE 18.
       01  SPEC-TABLE.
      * 01.  WR-WCBC-RECOVERY-INCOME
           03  FILLER      PIC X(8) VALUE "E NS072 ".
      * 02.  WR-WCBC-ELM-AMT-90-DAYS
           03  FILLER      PIC X(8) VALUE "E NS092 ".
      * 03.  WR-WCBC-CHARGEOFF-EXPENSE
           03  FILLER      PIC X(8) VALUE "E NS072 ".
      * 04.  WR-WCBC-BAD-DEBT-BANKRPCY
           03  FILLER      PIC X(8) VALUE "E NS072 ".
      * 05.  WS-WR-WCBC-BAD-DEBT      (DISPLAY ONLY)
           03  FILLER      PIC X(8) VALUE "E NS092N".
      * 06.  WS-WR-WCBC-PROFIT      (DISPLAY ONLY)
           03  FILLER      PIC X(8) VALUE "E NS092N".
      * 07.  WR-WCBC-NO-30-DAY-ACCTS
           03  FILLER      PIC X(8) VALUE "E NS070 ".
      * 08.  WR-WCBC-AMT-30-DAY-ACCTS
           03  FILLER      PIC X(8) VALUE "E NS092 ".
      * 09.  WR-WCBC-NO-60-DAY-ACCTS
           03  FILLER      PIC X(8) VALUE "E NS070 ".
      * 10.  WR-WCBC-AMT-60-DAY-ACCTS
           03  FILLER      PIC X(8) VALUE "E NS092 ".
      * 11.  WR-WCBC-NO-90-DAY-ACCTS
           03  FILLER      PIC X(8) VALUE "E NS070 ".
      * 12.  WR-WCBC-AMT-90-DAY-ACCTS
           03  FILLER      PIC X(8) VALUE "E NS092 ".
      * 13.  WS-WR-WCBC-NO-DEL-ACCTS (DISPLAY ONLY)
           03  FILLER      PIC X(8) VALUE "E NS050N".
      * 14.  WS-WR-WCBC-AMT-DEL-ACCTS (DISPLAY ONLY)
           03  FILLER      PIC X(8) VALUE "ENNS092N".
      * 15.  WS-WR-WCBC-PCT-NO-DEL    (DISPLAY ONLY)
           03  FILLER      PIC X(8) VALUE "E NS032N".
      * 16.  WS-WR-WCBC-PCT-AMT-DEL   (DISPLAY ONLY)
           03  FILLER      PIC X(8) VALUE "ENNS032N".
      * 17.  WS-WR-WCBC-PCT-BD-AR     (DISPLAY ONLY)
           03  FILLER      PIC X(8) VALUE "ENNS032N".
      * 18.  WS-WR-WCBC-PROFIT        (DISPLAY ONLY)
           03  FILLER      PIC X(8) VALUE "ENNS092N".
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
       COPY "LIBMN/WRMAN6_DEF.CPY".
       COPY "LIBMN/WRMAN6_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       01 FORM-LINK-AREA    PIC X(2000).

       SCREEN SECTION.
       COPY "LIBMN/WRMAN6_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME FORM-LINK-AREA EXIT-PATHNAME.

      *********************************************
       MAIN-PROGRAM SECTION.
      *********************************************
           PERFORM SQL-CONNECT.

           MOVE "WRMAN6" TO VDU-FORM-NAME.

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

      *********************************************
       REC-WR-MODULE SECTION.
      *********************************************
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

           MOVE NUM-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-YYYYMM.
           MOVE DATE-YYYYMM TO WR-YYYYMM HOLD-WR-YYYYMM.
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
           MOVE VDUNUM0 TO WR-BRNO HOLD-WR-BRNO.
           IF WR-BRNO = 0
              GO TO REC-WR-BRNO.
           MOVE WR1-KEY TO HOLD-WR1-KEY.
       READ-WRFILE.
           PERFORM OPEN-WR1-FILE.
           PERFORM READ-WR1-FILE.
           PERFORM CLOSE-WR1-FILE.
           IF IO-GOOD
              MOVE WR-REC TO ORIGINAL-WR-REC
              PERFORM DISPLAY-FIELDS
           ELSE
              PERFORM NEW-RECORD-SETUP.

       END-REC-WR-MODULE.

      *********************************************
       ENTER-SCANKEY SECTION.
      *********************************************
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
      *    IF ELE = 2 OR 3 OR 4 OR 10 OR 11
      *       PERFORM DISPLAY-FIELDS.
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
                 F-09 F-10 F-11 F-12 F-13 F-14 F-15
                 F-16 F-17 F-18
                 DEPENDING ON ELE.
           GO TO END-ENTRY-MODULE.

      *=================================================================
      *
      *    S E T - R E C O R D - E L E M E N T S
      *
      *=================================================================
       F-01.
           MOVE VDUNUM2 TO WR-WCBC-RECOVERY-INCOME(1).
       F-011.
           MOVE "E NS072  1." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-011.
           MOVE VDUNUM2 TO WR-WCBC-RECOVERY-INCOME(2).
           GO TO CREATE-ROUTINE.
       F-02.
           MOVE VDUNUM2 TO WR-WCBC-ELM-AMT-90-DAYS.
       F-021.
           MOVE "E NS092  1." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-021.
           MOVE VDUNUM2 TO WR-WCBC-ELY-AMT-90-DAYS.
           GO TO CREATE-ROUTINE.
       F-03.
           MOVE VDUNUM2 TO WR-WCBC-CHARGEOFF-EXPENSE(1).
       F-031.
           MOVE "E NS072  1." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-031.
           MOVE VDUNUM2 TO WR-WCBC-CHARGEOFF-EXPENSE(2).
           GO TO CREATE-ROUTINE.
       F-04.
           MOVE VDUNUM2 TO WR-WCBC-BAD-DEBT-BNKRPCY(1).
       F-041.
           MOVE "E NS072  1." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-041.
           MOVE VDUNUM2 TO WR-WCBC-BAD-DEBT-BNKRPCY(2).
           GO TO CREATE-ROUTINE.
       F-05.
           GO TO CREATE-ROUTINE.
       F-06.
           GO TO CREATE-ROUTINE.
       F-07.
           MOVE VDUNUM0 TO WR-WCBC-NO-30-DAY-ACCTS.
           GO TO CREATE-ROUTINE.
       F-08.
           MOVE VDUNUM2 TO WR-WCBC-AMT-30-DAY-ACCTS.
           GO TO CREATE-ROUTINE.
       F-09.
           MOVE VDUNUM0 TO WR-WCBC-NO-60-DAY-ACCTS.
           GO TO CREATE-ROUTINE.
       F-10.
           MOVE VDUNUM2 TO WR-WCBC-AMT-60-DAY-ACCTS.
           GO TO CREATE-ROUTINE.
       F-11.
           MOVE VDUNUM0 TO WR-WCBC-NO-90-DAY-ACCTS.
           GO TO CREATE-ROUTINE.
       F-12.
           MOVE VDUNUM2 TO WR-WCBC-AMT-90-DAY-ACCTS.
           GO TO CREATE-ROUTINE.
       F-13.
           GO TO CREATE-ROUTINE.
       F-14.
           GO TO CREATE-ROUTINE.
       F-15.
           GO TO CREATE-ROUTINE.
       F-16.
           GO TO CREATE-ROUTINE.
       F-17.
           GO TO CREATE-ROUTINE.
       F-18.
           GO TO CREATE-ROUTINE.
       END-ENTRY-MODULE.
           MOVE 0 TO FORM-SEL1 FORM-SEL2.
       EXIT-ENTRY-MODULE.
           EXIT.

      ************************************************
       WRITE-MODULE SECTION.
      ************************************************
           PERFORM OPEN-WR1-FILE.
           IF NEW-FLAG NOT = 0
              GO TO WRITE-RECORD.
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

      *WRITE-RECORD.

      *    GO TO END-WRITE-MODULE.
       WRITE-RECORD.
           PERFORM WRITE-WR1-FILE.
           IF IO-FG NOT = 0
              PERFORM ALREADY-CREATED
           ELSE
              MOVE "Y" TO NEW-REC.
       END-WRITE-MODULE.
           PERFORM CLOSE-WR1-FILE.

      ************************************************
       NEW-RECORD-SETUP SECTION.
      ************************************************
           MOVE 1 TO NEW-FLAG.
           MOVE "S  A000NEW." TO VDU.
           MOVE "* NEW RECORD *" TO VDUBUF.
           PERFORM SCREENIO.

           PERFORM CLEAR-WR-FILE.
           MOVE HOLD-WR1-KEY TO WR1-KEY.
           MOVE 0 TO ELE.

       NEW-RECORD-EXIT.
           EXIT.

      ************************************************
       LOG-MAINTENANCE-CHGS SECTION.
      ************************************************
           MOVE ORIGINAL-WR-REC TO WROR-REC.
           MOVE CHANGED-WR-REC  TO WRCH-REC.

           MOVE SPACES  TO WORK-ERRLOG-KEY.
           MOVE WR1-KEY TO WORK-ERRLOG-WR1.
           MOVE "TO"    TO  WORK-ERRLOG-TO-X.

           IF WROR-WCBC-RECOVERY-INCOME(1) NOT =
             WRCH-WCBC-RECOVERY-INCOME(1)
             MOVE "01 "                 TO WORK-ERRLOG-FIELD
             MOVE 1                     TO SUB
             MOVE WORK-FIELD-DESC(SUB)  TO WORK-ERRLOG-DESC
             MOVE WROR-WCBC-RECOVERY-INCOME(1) TO WORK-ERRLOG-ORG-N2
             MOVE WRCH-WCBC-RECOVERY-INCOME(1) TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.
           IF WROR-WCBC-RECOVERY-INCOME(2) NOT =
             WRCH-WCBC-RECOVERY-INCOME(2)
             MOVE "011"                 TO WORK-ERRLOG-FIELD
             MOVE 2                     TO SUB
             MOVE WORK-FIELD-DESC(SUB)  TO WORK-ERRLOG-DESC
             MOVE WROR-WCBC-RECOVERY-INCOME(2) TO WORK-ERRLOG-ORG-N2
             MOVE WRCH-WCBC-RECOVERY-INCOME(2) TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF WROR-WCBC-ELM-AMT-90-DAYS NOT =
             WRCH-WCBC-ELM-AMT-90-DAYS
             MOVE "02 "                 TO WORK-ERRLOG-FIELD
             MOVE 2                     TO SUB
             MOVE WORK-FIELD-DESC(SUB)  TO WORK-ERRLOG-DESC
             MOVE WROR-WCBC-ELM-AMT-90-DAYS TO WORK-ERRLOG-ORG-N2
             MOVE WRCH-WCBC-ELM-AMT-90-DAYS TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.
           IF WROR-WCBC-ELY-AMT-90-DAYS NOT =
             WRCH-WCBC-ELY-AMT-90-DAYS
             MOVE "021"                 TO WORK-ERRLOG-FIELD
             MOVE 2                     TO SUB
             MOVE WORK-FIELD-DESC(SUB)  TO WORK-ERRLOG-DESC
             MOVE WROR-WCBC-ELY-AMT-90-DAYS TO WORK-ERRLOG-ORG-N2
             MOVE WRCH-WCBC-ELY-AMT-90-DAYS TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

      *    COMPUTE WS-WR-WCBC-INCR-DECR-90(1) =
      *       WR-WCBC-AMT-90-DAY-ACCTS - WR-WCBC-ELM-AMT-90-DAYS.
      *    MOVE WS-WR-WCBC-INCR-DECR-90(1) TO F022.
      *    COMPUTE WS-WR-WCBC-INCR-DECR-90(2) =
      *       WR-WCBC-AMT-90-DAY-ACCTS - WR-WCBC-ELY-AMT-90-DAYS.
      *    MOVE WS-WR-WCBC-INCR-DECR-90(2) TO F023.

           IF WROR-WCBC-CHARGEOFF-EXPENSE(1) NOT =
             WRCH-WCBC-CHARGEOFF-EXPENSE(1)
             MOVE "03 "                 TO WORK-ERRLOG-FIELD
             MOVE 3                     TO SUB
             MOVE WORK-FIELD-DESC(SUB)  TO WORK-ERRLOG-DESC
             MOVE WROR-WCBC-CHARGEOFF-EXPENSE(1) TO WORK-ERRLOG-ORG-N2
             MOVE WRCH-WCBC-CHARGEOFF-EXPENSE(1) TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.
           IF WROR-WCBC-CHARGEOFF-EXPENSE(2) NOT =
             WRCH-WCBC-CHARGEOFF-EXPENSE(2)
             MOVE "031"                 TO WORK-ERRLOG-FIELD
             MOVE 3                     TO SUB
             MOVE WORK-FIELD-DESC(SUB)  TO WORK-ERRLOG-DESC
             MOVE WROR-WCBC-CHARGEOFF-EXPENSE(2) TO WORK-ERRLOG-ORG-N2
             MOVE WRCH-WCBC-CHARGEOFF-EXPENSE(2) TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF WROR-WCBC-BAD-DEBT-BNKRPCY(1) NOT =
             WRCH-WCBC-BAD-DEBT-BNKRPCY(1)
             MOVE "04 "                 TO WORK-ERRLOG-FIELD
             MOVE 4                     TO SUB
             MOVE WORK-FIELD-DESC(SUB)  TO WORK-ERRLOG-DESC
             MOVE WROR-WCBC-BAD-DEBT-BNKRPCY(1) TO WORK-ERRLOG-ORG-N2
             MOVE WRCH-WCBC-BAD-DEBT-BNKRPCY(1) TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.
           IF WROR-WCBC-BAD-DEBT-BNKRPCY(2) NOT =
             WRCH-WCBC-BAD-DEBT-BNKRPCY(2)
             MOVE "041"                 TO WORK-ERRLOG-FIELD
             MOVE 4                     TO SUB
             MOVE WORK-FIELD-DESC(SUB)  TO WORK-ERRLOG-DESC
             MOVE WROR-WCBC-BAD-DEBT-BNKRPCY(2) TO WORK-ERRLOG-ORG-N2
             MOVE WRCH-WCBC-BAD-DEBT-BNKRPCY(2) TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF WROR-WCBC-NO-30-DAY-ACCTS NOT =
             WRCH-WCBC-NO-30-DAY-ACCTS
             MOVE "07 "                 TO WORK-ERRLOG-FIELD
             MOVE 7                     TO SUB
             MOVE WORK-FIELD-DESC(SUB)  TO WORK-ERRLOG-DESC
             MOVE WROR-WCBC-NO-30-DAY-ACCTS TO WORK-ERRLOG-ORG-N0
             MOVE WRCH-WCBC-NO-30-DAY-ACCTS TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.
           IF WROR-WCBC-AMT-30-DAY-ACCTS NOT =
             WRCH-WCBC-AMT-30-DAY-ACCTS
             MOVE "08 "                 TO WORK-ERRLOG-FIELD
             MOVE 8                     TO SUB
             MOVE WORK-FIELD-DESC(SUB)  TO WORK-ERRLOG-DESC
             MOVE WROR-WCBC-AMT-30-DAY-ACCTS TO WORK-ERRLOG-ORG-N2
             MOVE WRCH-WCBC-AMT-30-DAY-ACCTS TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF WROR-WCBC-NO-60-DAY-ACCTS NOT =
             WRCH-WCBC-NO-60-DAY-ACCTS
             MOVE "09 "                 TO WORK-ERRLOG-FIELD
             MOVE 9                     TO SUB
             MOVE WORK-FIELD-DESC(SUB)  TO WORK-ERRLOG-DESC
             MOVE WROR-WCBC-NO-60-DAY-ACCTS TO WORK-ERRLOG-ORG-N0
             MOVE WRCH-WCBC-NO-60-DAY-ACCTS TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.
           IF WROR-WCBC-AMT-60-DAY-ACCTS NOT =
             WRCH-WCBC-AMT-60-DAY-ACCTS
             MOVE "10 "                 TO WORK-ERRLOG-FIELD
             MOVE 10                    TO SUB
             MOVE WORK-FIELD-DESC(SUB)  TO WORK-ERRLOG-DESC
             MOVE WROR-WCBC-AMT-60-DAY-ACCTS TO WORK-ERRLOG-ORG-N2
             MOVE WRCH-WCBC-AMT-60-DAY-ACCTS TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF WROR-WCBC-NO-90-DAY-ACCTS NOT =
             WRCH-WCBC-NO-90-DAY-ACCTS
             MOVE "11 "                 TO WORK-ERRLOG-FIELD
             MOVE 11                    TO SUB
             MOVE WORK-FIELD-DESC(SUB)  TO WORK-ERRLOG-DESC
             MOVE WROR-WCBC-NO-90-DAY-ACCTS TO WORK-ERRLOG-ORG-N0
             MOVE WRCH-WCBC-NO-90-DAY-ACCTS TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.
           IF WROR-WCBC-AMT-90-DAY-ACCTS NOT =
             WRCH-WCBC-AMT-90-DAY-ACCTS
             MOVE "12 "                 TO WORK-ERRLOG-FIELD
             MOVE 12                    TO SUB
             MOVE WORK-FIELD-DESC(SUB)  TO WORK-ERRLOG-DESC
             MOVE WROR-WCBC-AMT-90-DAY-ACCTS TO WORK-ERRLOG-ORG-N2
             MOVE WRCH-WCBC-AMT-90-DAY-ACCTS TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           MOVE WS-WR-WCBC-PROFIT(1) TO F18MTD.
           MOVE WS-WR-WCBC-PROFIT(2) TO F181YTD.

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

           MOVE WR-WCBC-RECOVERY-INCOME(1)    TO F01.
           MOVE WR-WCBC-RECOVERY-INCOME(2)    TO F011.

           MOVE WR-WCBC-ELM-AMT-90-DAYS       TO F02.
           MOVE WR-WCBC-ELY-AMT-90-DAYS       TO F021.
           COMPUTE WS-WR-WCBC-INCR-DECR-90(1) =
              WR-WCBC-AMT-90-DAY-ACCTS - WR-WCBC-ELM-AMT-90-DAYS.
           MOVE WS-WR-WCBC-INCR-DECR-90(1)    TO F022.
           COMPUTE WS-WR-WCBC-INCR-DECR-90(2) =
              WR-WCBC-AMT-90-DAY-ACCTS - WR-WCBC-ELY-AMT-90-DAYS.
           MOVE WS-WR-WCBC-INCR-DECR-90(2)    TO F023.

           MOVE WR-WCBC-CHARGEOFF-EXPENSE(1)  TO F03.
           MOVE WR-WCBC-CHARGEOFF-EXPENSE(2)  TO F031.

           MOVE WR-WCBC-BAD-DEBT-BNKRPCY(1)   TO F04.
           MOVE WR-WCBC-BAD-DEBT-BNKRPCY(2)   TO F041.

           COMPUTE WS-WR-WCBC-BAD-DEBT(1) =
             WS-WR-WCBC-INCR-DECR-90(1) +
             WR-WCBC-CHARGEOFF-EXPENSE(1) +
             WR-WCBC-BAD-DEBT-BNKRPCY(1) -
             WR-WCBC-RECOVERY-INCOME(1).
           MOVE WS-WR-WCBC-BAD-DEBT(1)       TO F05MTD.

           COMPUTE WS-WR-WCBC-BAD-DEBT(2) =
             WS-WR-WCBC-INCR-DECR-90(2) +
             WR-WCBC-CHARGEOFF-EXPENSE(2) +
             WR-WCBC-BAD-DEBT-BNKRPCY(2) -
             WR-WCBC-RECOVERY-INCOME(2).
           MOVE WS-WR-WCBC-BAD-DEBT(2)      TO F051YTD.

           COMPUTE WS-WR-WCBC-TOTAL-INCOME(1) =
              WR-WCBC-LATE-CHGS(1) +
              WR-WCBC-NET-FIN-CHGS(1) +
              WR-WCBC-SALES(1) -
              WR-WCBC-COST-OF-SALES(1) -
              WR-WCBC-EXPENSES(1).
           COMPUTE WS-WR-WCBC-PROFIT(1) =
              WS-WR-WCBC-TOTAL-INCOME(1) - WS-WR-WCBC-BAD-DEBT(1).
           MOVE WS-WR-WCBC-PROFIT(1)        TO F06MTD.

           COMPUTE WS-WR-WCBC-TOTAL-INCOME(2) =
              WR-WCBC-LATE-CHGS(2) +
              WR-WCBC-NET-FIN-CHGS(2) +
              WR-WCBC-SALES(2) -
              WR-WCBC-COST-OF-SALES(2) -
              WR-WCBC-EXPENSES(2).
           COMPUTE WS-WR-WCBC-PROFIT(2) =
              WS-WR-WCBC-TOTAL-INCOME(2) - WS-WR-WCBC-BAD-DEBT(2).
           MOVE WS-WR-WCBC-PROFIT(2)        TO F061YTD.

           MOVE WR-WCBC-NO-30-DAY-ACCTS     TO F07.

           MOVE WR-WCBC-AMT-30-DAY-ACCTS    TO F08.

           MOVE WR-WCBC-NO-60-DAY-ACCTS     TO F09.

           MOVE WR-WCBC-AMT-60-DAY-ACCTS    TO F10.

           MOVE WR-WCBC-NO-90-DAY-ACCTS     TO F11.

           MOVE WR-WCBC-AMT-90-DAY-ACCTS    TO F12.

           COMPUTE WS-WR-WCBC-TOT-NO-DEL =
             WR-WCBC-NO-30-DAY-ACCTS +
             WR-WCBC-NO-60-DAY-ACCTS +
             WR-WCBC-NO-90-DAY-ACCTS.
           MOVE WS-WR-WCBC-TOT-NO-DEL       TO F131YTD.

           COMPUTE WS-WR-WCBC-TOT-AMT-DEL =
             WR-WCBC-AMT-30-DAY-ACCTS +
             WR-WCBC-AMT-60-DAY-ACCTS +
             WR-WCBC-AMT-90-DAY-ACCTS.
           MOVE WS-WR-WCBC-TOT-AMT-DEL      TO F141YTD.

           IF WR-WCBC-CUR-NO-CUST NOT = 0
              COMPUTE WS-WR-WCBC-PCT-NO-DEL ROUNDED =
              WS-WR-WCBC-TOT-NO-DEL / WR-WCBC-CUR-NO-CUST
              * 100
           ELSE
              MOVE 0 TO WS-WR-WCBC-PCT-NO-DEL.
           MOVE WS-WR-WCBC-PCT-NO-DEL       TO F151YTD.

           IF WR-WCBC-OUTS-BALANCE NOT = 0
              COMPUTE WS-WR-WCBC-PCT-AMT-DEL ROUNDED =
              WS-WR-WCBC-TOT-AMT-DEL / WR-WCBC-OUTS-BALANCE
              * 100
           ELSE
              MOVE 0 TO WS-WR-WCBC-PCT-AMT-DEL.
           MOVE WS-WR-WCBC-PCT-AMT-DEL      TO F161YTD.

           IF WR-WCBC-OUTS-BALANCE NOT = 0
              COMPUTE WS-WR-WCBC-PCT-BAD-DEBT-MTD ROUNDED =
              WS-WR-WCBC-BAD-DEBT(1) / WR-WCBC-OUTS-BALANCE
              * 100
           ELSE
              MOVE 0 TO WS-WR-WCBC-PCT-BAD-DEBT-MTD.
           MOVE WS-WR-WCBC-PCT-BAD-DEBT-MTD TO F171MTD.

           IF WR-WCBC-OUTS-BALANCE NOT = 0
              COMPUTE WS-WR-WCBC-PCT-BAD-DEBT-YTD ROUNDED =
              WS-WR-WCBC-BAD-DEBT(2) / WR-WCBC-OUTS-BALANCE
              * 100
           ELSE
              MOVE 0 TO WS-WR-WCBC-PCT-BAD-DEBT-YTD.
           MOVE WS-WR-WCBC-PCT-BAD-DEBT-YTD TO F171YTD.

           MOVE WS-WR-WCBC-PROFIT(1)         TO F18MTD.
           MOVE WS-WR-WCBC-PROFIT(2)         TO F181YTD.



       DISPLAY-FIELDS-EXIT.
           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *********************************************
       FORM-RESET SECTION.
      *********************************************
           MOVE EXT-CONAME-CONAME TO WRMAN6-CONAME.
           MOVE EXT-CONAME-SYSDATE TO WRMAN6-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY WRMAN6-SCREEN UPON WRMAIN-WINDOW-HANDLE.
           MOVE WRMAN6-FORM-FIELDS TO WR-BUF.
           MOVE WRMAN6-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *********************************************
       SETUP-LINK-INFO SECTION.
      *********************************************
           MOVE WRMAN6-HELP--FILE TO HELP-LINK-FILE.
           MOVE WRMAN6-HELP--DIR TO HELP-LINK-DIR.

      *********************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/WRMAN6_EVA.CPY".
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
       COPY "LIBSP/SPWRCL.CPY".

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
           DESTROY WRMAN6-SCREEN.
           MOVE WRMAIN-LINK TO FORM-LINK-AREA.
           EXIT PROGRAM.
