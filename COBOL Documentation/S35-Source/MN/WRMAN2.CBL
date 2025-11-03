       IDENTIFICATION DIVISION.
       PROGRAM-ID.      WRMAN2.
      **************************************************************************
      *                 (MN)
      *
      *  DESCRIPTION:   WORLD RATING SHEET FILE MAINTENANCE - PAGE 2
      *                 SMALL LOANS PG 2
      * REV:
      * BLV 040112 CHANGED YTD PERCENTS FOR RATING SHEET LINES 30 & 32 TO BE
      *            CALCULATED, NOT ACCUMULATED & STORED IN WRFILE
      * BLV 040119 FIXED SCAN KEY
      * GLB 040226 FIX DATE CONVERSION BUG;MOVE NUM-DATE TO DATE-MMDDYY
      * GLB 040226 FIX PERCENTAGES (* 100)
      * GLB 040226 ADD MAINTENANCE LOG
      * GLB 040315 GENERAL CLEANUP WHILE CREATING HELP
      * GLB 040325 BUG; PAGING ERROR, FORM-MAX1 S/B 9 NOT 3
      * BLM 140815 CHANGED COMPUTATION OF MTD RATING SHEET PROFIT TO INCLUDE
      *            WCBC; NOTE, THIS IS A DISPLAYED FIELD, NOT A FIELD IN THE
      *            WRFILE.  WORLD PR# 992B
      * BLM 150518 RESTORED VERSION BEFORE PR# 992B.  NOW WCBC IS NOT IN
      *            CALCULATION OF RATING SHEET PROFIT.  WORLD PR# 1078
      *  CS 160509 CHANGED ORIGINAL/CHANGED REC TO INCLUDE WR
      *  CS 170522 ACTIVATE 8 DIGIT DATES
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
           "@(#)MN/WRMAN2.CBL S34 06/08/21-11:41:31 >".
      *******************************
      *       PROGRAM WORKERS       *
      *******************************
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
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
               05  FILLER     PIC X(7) VALUE "AMORTIZ".
               05  FILLER     PIC X(7) VALUE "ROBBERY".
               05  FILLER     PIC X(7) VALUE "FLD 03 ".
               05  FILLER     PIC X(7) VALUE "% PROF ".
               05  FILLER     PIC X(7) VALUE "INCOME ".
               05  FILLER     PIC X(7) VALUE "% INCR ".
               05  FILLER     PIC X(7) VALUE "INT/FEE".
               05  FILLER     PIC X(7) VALUE "%INT   ".
               05  FILLER     PIC X(7) VALUE "INS INC".
               05  FILLER     PIC X(7) VALUE "% INC  ".
               05  FILLER     PIC X(7) VALUE "EXPENSE".
               05  FILLER     PIC X(7) VALUE "FLD 12 ".
               05  FILLER     PIC X(7) VALUE "%EXPLED".
               05  FILLER     PIC X(7) VALUE "FLD 14 ".
               05  FILLER     PIC X(7) VALUE "PERSONL".
       01  WORK-FIELD-TABLE2 REDEFINES WORK-FIELD-TABLE.
           03  WORK-FIELD-DESC  PIC X(7) OCCURS 15 TIMES.


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
       01  CURRENT-FORM      PIC 99 VALUE 2.

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

      *    MOVE " ENTER YYMMBBBB OR F6 TO SCAN" TO MESS.
       01  SCAN-NUM-KEY       PIC 9(8). 
       01  FILLER REDEFINES SCAN-NUM-KEY.
           03  SCAN-WR-YY     PIC 9(2).
           03  SCAN-WR-MM     PIC 9(2).
           03  SCAN-WR-BRNO   PIC 9(4).

       01  ORIGINAL-WR-REC    PIC X(WR-SIZE)   VALUE SPACES.
       01  CHANGED-WR-REC     PIC X(WR-SIZE)   VALUE SPACES.

       01  COMPUTED-FIELDS               OCCURS 2.
           03  WS-WR-NO-TOT              PIC S9(5).
           03  WS-WR-RATING-SHEET-PROFIT PIC S9(7)V99.
           03  WS-WR-EXPENSE-PER-LOAN    PIC S9(3)V99.
           03  WS-WR-PCT-EXPENSE-INCOME  PIC S9(3)V99.
           03  WS-WR-PCT-PERSONNEL-INC   PIC S9(3)V99.

       01  WS-WR-PCT-INT-FEE-LEDGER      PIC S9(3)V99.
       01  WS-WR-PCT-INS-INC-LEDGER      PIC S9(3)V99.
       01  WS-WR-PCT-EXPENSE-LEDGER      PIC S9(3)V99.
       01  WS-WR-PCT-RATE-SHT-PROFIT     PIC S9(3)V99.
       01  WS-WR-PCT-RS-INC-LEDGER       PIC S9(3)V99.

      ******************************
      *    SCREEN DISPLAY BUFFER   *
      ******************************
       01  DISPLAY-BUF     VALUE SPACES.
           03  F01         PIC Z,ZZZ,ZZZ.ZZ-.
           03  F011        PIC Z,ZZZ,ZZZ.ZZ-.
           03  F02         PIC Z,ZZZ,ZZZ.ZZ-.
           03  F021        PIC Z,ZZZ,ZZZ.ZZ-.
           03  F03MTD      PIC Z,ZZZ,ZZZ.ZZ-.
           03  F031YTD     PIC Z,ZZZ,ZZZ.ZZ-.
           03  F04MTD      PIC ZZZ.ZZ-.
           03  F04         PIC ZZZ.ZZ-.
           03  F05         PIC Z,ZZZ,ZZZ.ZZ-.
           03  F051        PIC Z,ZZZ,ZZZ.ZZ-.
           03  F06MTD      PIC ZZZ.ZZ-.
           03  F06         PIC ZZZ.ZZ-.
           03  F07         PIC Z,ZZZ,ZZZ.ZZ-.
           03  F071        PIC Z,ZZZ,ZZZ.ZZ-.
           03  F08MTD      PIC ZZZ.ZZ-.
           03  F08         PIC ZZZ.ZZ-.
           03  F09         PIC Z,ZZZ,ZZZ.ZZ-.
           03  F091        PIC Z,ZZZ,ZZZ.ZZ-.
           03  F10MTD      PIC ZZZ.ZZ-.
           03  F10         PIC ZZZ.ZZ-.
           03  F11         PIC Z,ZZZ,ZZZ.ZZ-.
           03  F111        PIC Z,ZZZ,ZZZ.ZZ-.
           03  F12MTD      PIC ZZZ.ZZ-.
           03  F121YTD     PIC ZZZ.ZZ-.
           03  F13MTD      PIC ZZZ.ZZ-.
           03  F13         PIC ZZZ.ZZ-.
           03  F14MTD      PIC ZZZ.ZZ-.
           03  F141YTD     PIC ZZZ.ZZ-.
           03  F15         PIC Z,ZZZ,ZZZ.ZZ-.
           03  F151        PIC Z,ZZZ,ZZZ.ZZ-.
           03  F16MTD      PIC ZZZ.ZZ-.
           03  F161YTD     PIC ZZZ.ZZ-.
           03  FCUROUTS    PIC ZZZ,ZZZ,ZZZ.ZZ-.
           03  FELMOUTS    PIC ZZZ,ZZZ,ZZZ.ZZ-.
           03  FELYOUTS    PIC ZZZ,ZZZ,ZZZ.ZZ-.

       01  DISPLAY-LIST.
           03  FILLER      PIC X(41) VALUE
           "010 011 020 021 030MTD 031YTD 040MTD 040 ".
           03  FILLER      PIC X(38) VALUE
           "050 051 060MTD 060 070 071 080MTD 080 ".
           03  FILLER      PIC X(41) VALUE
           "090 091 100MTD 100 110 111 120MTD 121YTD ".
           03  FILLER      PIC X(47) VALUE
           "130MTD 130 140MTD 140YTD 150 151 160MTD 160YTD ".
           03  FILLER      PIC X(24) VALUE
           "CUROUTS ELMOUTS ELYOUTS.".

      ********************************
      *    ELEMENT SPECIFICATIONS    *
      ********************************
       78  MAX                      VALUE 16.
       01  SPEC-TABLE.
      * 01.  WR-INTANGIBLE-AMORT      .
           03  FILLER      PIC X(8) VALUE "E NS072 ".
      * 02.  WR-ROBBERIES             .
           03  FILLER      PIC X(8) VALUE "E NS072 ".
      * 03.  WR-RATING-SHEET-PROFIT   .
           03  FILLER      PIC X(8) VALUE "E NS072N".
      * 04.  WR-PCT-RATE-SHT-PROFIT   .
           03  FILLER      PIC X(8) VALUE "E NS032 ".
      * 05.  WR-RATING-SHEET-INCOME   .
           03  FILLER      PIC X(8) VALUE "E NS072 ".
      * 06.  WR-PCT-RS-INC-LEDGER     .
           03  FILLER      PIC X(8) VALUE "E NS032 ".
      * 07.  WR-INT-FEE-INCOME        .
           03  FILLER      PIC X(8) VALUE "E NS072 ".
      * 08.  WR-PCT-INT-FEE-LEDGER    .
           03  FILLER      PIC X(8) VALUE "E NS032 ".
      * 09.  WR-INSURANCE-INCOME      .
           03  FILLER      PIC X(8) VALUE "E NS072 ".
      * 10.  WR-PCT-INS-INC-LEDGER    .
           03  FILLER      PIC X(8) VALUE "E NS032 ".
      * 11.  WR-RATING-SHEET-EXPENSES .
           03  FILLER      PIC X(8) VALUE "E NS072 ".
      * 12.  WR-EXPENSE-PER-LOAN      .
           03  FILLER      PIC X(8) VALUE "E NS072N".
      * 13.  WR-PCT-EXPENSE-LEDGER    .
           03  FILLER      PIC X(8) VALUE "E NS032 ".
      * 14.  WR-PCT-EXPENSE-INCOME    .
           03  FILLER      PIC X(8) VALUE "E NS032N".
      * 15.  WR-PERSONNEL-EXPENSE     .
           03  FILLER      PIC X(8) VALUE "E NS072 ".
      * 16.  WR-PCT-PERSONNEL-INC  .
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
       COPY "LIBMN/WRMAN2_DEF.CPY".
       COPY "LIBMN/WRMAN2_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       01 FORM-LINK-AREA    PIC X(2000).

       SCREEN SECTION.
       COPY "LIBMN/WRMAN2_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME FORM-LINK-AREA EXIT-PATHNAME.

      *******************************************
       MAIN-PROGRAM SECTION.
      *******************************************
           PERFORM SQL-CONNECT.


           MOVE "WRMAN2" TO VDU-FORM-NAME.

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
           IF IO-TYPE = "CLR"
              GO TO MAIN-MODULE.

           IF NEW-REC = "Y"
              COMPUTE SUB = (CURRENT-FORM + 1)
              IF SUB > FORM-MAX2
                 MOVE "N" TO NEW-REC
                 GO TO MAIN-MODULE
              END-IF
              IF SUB <  (FORM-MAX1 + 1)
                 MOVE "P" TO FORM-P1
                 MOVE SUB TO FORM-SEL1
              ELSE
                 MOVE "P" TO FORM-P2
                 MOVE SUB TO FORM-SEL2.
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

      *******************************************
       REC-WR-MODULE SECTION.
      *******************************************
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
           MOVE VDUNUM0 TO WR-BRNO BR-NO.
           IF WR-BRNO = 0
              GO TO REC-WR-BRNO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF IO-BAD
              GO TO REC-WR-BRNO.
           MOVE "S  A000BRX." TO VDU.
           MOVE BR-NAME TO VDUBUF.
           PERFORM SCREENIO.
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

      *******************************************
       ENTER-SCANKEY SECTION.
      *******************************************
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
      * FORM-NUM1 CHECKS FOR PAGES 1 THRU 2
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
                 F-09 F-10 F-11 F-12 F-13 F-14 F-15 F-16
                 DEPENDING ON ELE.
           GO TO END-ENTRY-MODULE.

      *=================================================================
      *
      *    S E T - R E C O R D - E L E M E N T S
      *
      *=================================================================
       F-01.
           MOVE VDUNUM2 TO WR-INTANGIBLE-AMORT(1).
       F-011.
           MOVE "E NS072  1." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-011.
           MOVE VDUNUM2 TO WR-INTANGIBLE-AMORT(2).
           GO TO CREATE-ROUTINE.
       F-02.
           MOVE VDUNUM2 TO WR-ROBBERIES(1).
       F-021.
           MOVE "E NS072  1." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-021.
           MOVE VDUNUM2 TO WR-ROBBERIES(2).
           GO TO CREATE-ROUTINE.
       F-03.
      *    MOVE VDUNUM2 TO WR-RATING-SHEET-PROFIT(1).
           GO TO CREATE-ROUTINE.
       F-04.
           MOVE VDUNUM2 TO WR-YTD-PCT-RATE-SHT-PROFIT.
           GO TO CREATE-ROUTINE.
       F-05.
           MOVE VDUNUM2 TO WR-RATING-SHEET-INCOME(1).
       F-051.
           MOVE "E NS072  1." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-051.
           MOVE VDUNUM2 TO WR-RATING-SHEET-INCOME(2).
           GO TO CREATE-ROUTINE.
       F-06.
           MOVE VDUNUM2 TO WR-YTD-PCT-RS-INC-LEDGER.
           GO TO CREATE-ROUTINE.
       F-07.
           MOVE VDUNUM2 TO WR-INT-FEE-INCOME(1).
       F-071.
           MOVE "E NS072  1." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-071.
           MOVE VDUNUM2 TO WR-INT-FEE-INCOME(2).
           GO TO CREATE-ROUTINE.
       F-08.
           MOVE VDUNUM2 TO WR-YTD-PCT-INT-FEE-LEDGER.
           GO TO CREATE-ROUTINE.
       F-09.
           MOVE VDUNUM2 TO WR-INSURANCE-INCOME(1).
       F-091.
           MOVE "E NS072  1." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-091.
           MOVE VDUNUM2 TO WR-INSURANCE-INCOME(2).
           GO TO CREATE-ROUTINE.
       F-10.
           MOVE VDUNUM2 TO WR-YTD-PCT-INS-INC-LEDGER.
           GO TO CREATE-ROUTINE.
       F-11.
           MOVE VDUNUM2 TO WR-RATING-SHEET-EXPENSES(1).
       F-111.
           MOVE "E NS072  1." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-111.
           MOVE VDUNUM2 TO WR-RATING-SHEET-EXPENSES(2).
           GO TO CREATE-ROUTINE.
       F-12.
      *    MOVE VDUNUM2 TO WR-EXPENSE-PER-LOAN(1).
           GO TO CREATE-ROUTINE.
       F-13.
           MOVE VDUNUM2 TO WR-YTD-PCT-EXPENSE-LEDGER.
           GO TO CREATE-ROUTINE.
       F-14.
      *    MOVE VDUNUM2 TO WR-YTD-PCT-EXPENSE-INCOME.
           GO TO CREATE-ROUTINE.
       F-15.
           MOVE VDUNUM2 TO WR-PERSONNEL-EXPENSE(1).
       F-151.
           MOVE "E NS072  1." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-151.
           MOVE VDUNUM2 TO WR-PERSONNEL-EXPENSE(2).
           GO TO CREATE-ROUTINE.
       F-16.
      *    MOVE VDUNUM2 TO WR-YTD-PCT-PERSONNEL-INCOME.
           GO TO CREATE-ROUTINE.

       END-ENTRY-MODULE.
           MOVE 0 TO FORM-SEL1 FORM-SEL2.
       EXIT-ENTRY-MODULE.
           EXIT.

      ***********************************************
       WRITE-MODULE SECTION.
      ***********************************************
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

       WRITE-RECORD.

       END-WRITE-MODULE.
           PERFORM CLOSE-WR1-FILE.

      ***********************************************
       LOG-MAINTENANCE-CHGS SECTION.
      ***********************************************
           MOVE ORIGINAL-WR-REC TO WROR-REC.
           MOVE CHANGED-WR-REC  TO WRCH-REC.

           MOVE SPACES  TO WORK-ERRLOG-KEY.
           MOVE WR1-KEY TO WORK-ERRLOG-WR1.
           MOVE "TO"    TO  WORK-ERRLOG-TO-X.

           IF WROR-INTANGIBLE-AMORT(1) NOT = WRCH-INTANGIBLE-AMORT(1)
             MOVE "01 "                 TO WORK-ERRLOG-FIELD
             MOVE 1                     TO SUB
             MOVE WORK-FIELD-DESC(SUB)  TO WORK-ERRLOG-DESC
             MOVE WROR-INTANGIBLE-AMORT(1)      TO WORK-ERRLOG-ORG-N2
             MOVE WRCH-INTANGIBLE-AMORT(1)      TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.
           IF WROR-INTANGIBLE-AMORT(2) NOT = WRCH-INTANGIBLE-AMORT(2)
             MOVE "011"                 TO WORK-ERRLOG-FIELD
             MOVE 1                     TO SUB
             MOVE WORK-FIELD-DESC(SUB)  TO WORK-ERRLOG-DESC
             MOVE WROR-INTANGIBLE-AMORT(2)      TO WORK-ERRLOG-ORG-N2
             MOVE WRCH-INTANGIBLE-AMORT(2)      TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF WROR-ROBBERIES(1) NOT = WRCH-ROBBERIES(1)
             MOVE "02 "                 TO WORK-ERRLOG-FIELD
             MOVE 2                     TO SUB
             MOVE WORK-FIELD-DESC(SUB)  TO WORK-ERRLOG-DESC
             MOVE WROR-ROBBERIES(1)     TO WORK-ERRLOG-ORG-N2
             MOVE WRCH-ROBBERIES(1)     TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.
           IF WROR-ROBBERIES(2) NOT = WRCH-ROBBERIES(2)
             MOVE "021"                 TO WORK-ERRLOG-FIELD
             MOVE 2                     TO SUB
             MOVE WORK-FIELD-DESC(SUB)  TO WORK-ERRLOG-DESC
             MOVE WROR-ROBBERIES(2)     TO WORK-ERRLOG-ORG-N2
             MOVE WRCH-ROBBERIES(2)     TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF WROR-YTD-PCT-RATE-SHT-PROFIT NOT =
             WRCH-YTD-PCT-RATE-SHT-PROFIT
             MOVE "04 "                 TO WORK-ERRLOG-FIELD
             MOVE 4                     TO SUB
             MOVE WORK-FIELD-DESC(SUB)  TO WORK-ERRLOG-DESC
             MOVE WROR-YTD-PCT-RATE-SHT-PROFIT TO WORK-ERRLOG-ORG-N2
             MOVE WRCH-YTD-PCT-RATE-SHT-PROFIT TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF WROR-RATING-SHEET-INCOME(1) NOT =
             WRCH-RATING-SHEET-INCOME(1)
             MOVE "05 "                 TO WORK-ERRLOG-FIELD
             MOVE 5                     TO SUB
             MOVE WORK-FIELD-DESC(SUB)  TO WORK-ERRLOG-DESC
             MOVE WROR-RATING-SHEET-INCOME(1)  TO WORK-ERRLOG-ORG-N2
             MOVE WRCH-RATING-SHEET-INCOME(1)  TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.
           IF WROR-RATING-SHEET-INCOME(2) NOT =
             WRCH-RATING-SHEET-INCOME(2)
             MOVE "051"                 TO WORK-ERRLOG-FIELD
             MOVE 5                     TO SUB
             MOVE WORK-FIELD-DESC(SUB)  TO WORK-ERRLOG-DESC
             MOVE WROR-RATING-SHEET-INCOME(2)  TO WORK-ERRLOG-ORG-N2
             MOVE WRCH-RATING-SHEET-INCOME(2)  TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF WROR-YTD-PCT-RS-INC-LEDGER NOT =
             WRCH-YTD-PCT-RS-INC-LEDGER
             MOVE "06 "                 TO WORK-ERRLOG-FIELD
             MOVE 6                     TO SUB
             MOVE WORK-FIELD-DESC(SUB)  TO WORK-ERRLOG-DESC
             MOVE WROR-YTD-PCT-RS-INC-LEDGER  TO WORK-ERRLOG-ORG-N2
             MOVE WRCH-YTD-PCT-RS-INC-LEDGER  TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF WROR-INT-FEE-INCOME(1) NOT = WRCH-INT-FEE-INCOME(1)
             MOVE "07 "                 TO WORK-ERRLOG-FIELD
             MOVE 7                     TO SUB
             MOVE WORK-FIELD-DESC(SUB)  TO WORK-ERRLOG-DESC
             MOVE WROR-INT-FEE-INCOME(1)   TO WORK-ERRLOG-ORG-N2
             MOVE WRCH-INT-FEE-INCOME(1)   TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.
           IF WROR-INT-FEE-INCOME(2) NOT = WRCH-INT-FEE-INCOME(2)
             MOVE "071"                 TO WORK-ERRLOG-FIELD
             MOVE 7                     TO SUB
             MOVE WORK-FIELD-DESC(SUB)  TO WORK-ERRLOG-DESC
             MOVE WROR-INT-FEE-INCOME(2)  TO WORK-ERRLOG-ORG-N2
             MOVE WRCH-INT-FEE-INCOME(2)  TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF WROR-YTD-PCT-INT-FEE-LEDGER NOT =
             WRCH-YTD-PCT-INT-FEE-LEDGER
             MOVE "08 "                 TO WORK-ERRLOG-FIELD
             MOVE 8                     TO SUB
             MOVE WORK-FIELD-DESC(SUB)  TO WORK-ERRLOG-DESC
             MOVE WROR-YTD-PCT-INT-FEE-LEDGER  TO WORK-ERRLOG-ORG-N2
             MOVE WRCH-YTD-PCT-INT-FEE-LEDGER  TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF WROR-INSURANCE-INCOME(1) NOT = WRCH-INSURANCE-INCOME(1)
             MOVE "09 "                 TO WORK-ERRLOG-FIELD
             MOVE 9                     TO SUB
             MOVE WORK-FIELD-DESC(SUB)  TO WORK-ERRLOG-DESC
             MOVE WROR-INSURANCE-INCOME(1)   TO WORK-ERRLOG-ORG-N2
             MOVE WRCH-INSURANCE-INCOME(1)   TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.
           IF WROR-INSURANCE-INCOME(2) NOT = WRCH-INSURANCE-INCOME(2)
             MOVE "091"                 TO WORK-ERRLOG-FIELD
             MOVE 9                     TO SUB
             MOVE WORK-FIELD-DESC(SUB)  TO WORK-ERRLOG-DESC
             MOVE WROR-INSURANCE-INCOME(2)  TO WORK-ERRLOG-ORG-N2
             MOVE WRCH-INSURANCE-INCOME(2)  TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF WROR-YTD-PCT-INS-INC-LEDGER NOT =
             WRCH-YTD-PCT-INS-INC-LEDGER
             MOVE "10 "                 TO WORK-ERRLOG-FIELD
             MOVE 10                    TO SUB
             MOVE WORK-FIELD-DESC(SUB)  TO WORK-ERRLOG-DESC
             MOVE WROR-YTD-PCT-INS-INC-LEDGER  TO WORK-ERRLOG-ORG-N2
             MOVE WRCH-YTD-PCT-INS-INC-LEDGER  TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF WROR-RATING-SHEET-EXPENSES(1) NOT =
             WRCH-RATING-SHEET-EXPENSES(1)
             MOVE "11 "                 TO WORK-ERRLOG-FIELD
             MOVE 11                    TO SUB
             MOVE WORK-FIELD-DESC(SUB)  TO WORK-ERRLOG-DESC
             MOVE WROR-RATING-SHEET-EXPENSES(1)   TO WORK-ERRLOG-ORG-N2
             MOVE WRCH-RATING-SHEET-EXPENSES(1)   TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.
           IF WROR-RATING-SHEET-EXPENSES(2) NOT =
             WRCH-RATING-SHEET-EXPENSES(2)
             MOVE "111"                 TO WORK-ERRLOG-FIELD
             MOVE 11                    TO SUB
             MOVE WORK-FIELD-DESC(SUB)  TO WORK-ERRLOG-DESC
             MOVE WROR-RATING-SHEET-EXPENSES(2)  TO WORK-ERRLOG-ORG-N2
             MOVE WRCH-RATING-SHEET-EXPENSES(2)  TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF WROR-YTD-PCT-EXPENSE-LEDGER NOT =
             WRCH-YTD-PCT-EXPENSE-LEDGER
             MOVE "13 "                 TO WORK-ERRLOG-FIELD
             MOVE 13                    TO SUB
             MOVE WORK-FIELD-DESC(SUB)  TO WORK-ERRLOG-DESC
             MOVE WROR-YTD-PCT-EXPENSE-LEDGER  TO WORK-ERRLOG-ORG-N2
             MOVE WRCH-YTD-PCT-EXPENSE-LEDGER  TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF WROR-PERSONNEL-EXPENSE(1) NOT = WRCH-PERSONNEL-EXPENSE(1)
             MOVE "15 "                 TO WORK-ERRLOG-FIELD
             MOVE 15                    TO SUB
             MOVE WORK-FIELD-DESC(SUB)  TO WORK-ERRLOG-DESC
             MOVE WROR-PERSONNEL-EXPENSE(1)   TO WORK-ERRLOG-ORG-N2
             MOVE WRCH-PERSONNEL-EXPENSE(1)   TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.
           IF WROR-PERSONNEL-EXPENSE(2) NOT = WRCH-PERSONNEL-EXPENSE(2)
             MOVE "151"                 TO WORK-ERRLOG-FIELD
             MOVE 15                    TO SUB
             MOVE WORK-FIELD-DESC(SUB)  TO WORK-ERRLOG-DESC
             MOVE WROR-PERSONNEL-EXPENSE(2)  TO WORK-ERRLOG-ORG-N2
             MOVE WRCH-PERSONNEL-EXPENSE(2)  TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

      ***********************************************
       LOG-FIELD-CHANGE SECTION.
      ***********************************************
      * CODE TO WRITE CHANGE FILE.

           MOVE "X" TO ERRLOGW-ACTION.
           MOVE WORK-ERRLOG-KEY TO ERRLOGW-EOM-MSG.
           MOVE FORM-PATHNAME TO ERRLOGW-PROGX.

           MOVE "CL/ERRLOG" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PROGX.

      ***********************************************
       DISPLAY-FIELDS SECTION.
      ***********************************************
           MOVE SPACES TO DISPLAY-BUF.
           MOVE WR-INTANGIBLE-AMORT(1)       TO F01.
           MOVE WR-INTANGIBLE-AMORT(2)       TO F011.

           MOVE WR-ROBBERIES(1)              TO F02.
           MOVE WR-ROBBERIES(2)              TO F021.

           COMPUTE WS-WR-RATING-SHEET-PROFIT(1) =
                   WR-PRETAX-PROFIT(1)
                 + WR-INTEREST-ALLOCATION(1)
                 + WR-SUPERVISION(1)
                 + WR-GEN-RESERVE-ADJ(1)
                 + WR-INTANGIBLE-AMORT(1)
                 + WR-ROBBERIES(1).
           MOVE WS-WR-RATING-SHEET-PROFIT(1) TO F03MTD.
           COMPUTE WS-WR-RATING-SHEET-PROFIT(2) =
                   WR-PRETAX-PROFIT(2)
                 + WR-INTEREST-ALLOCATION(2)
                 + WR-SUPERVISION(2)
                 + WR-GEN-RESERVE-ADJ(2)
                 + WR-INTANGIBLE-AMORT(2)
                 + WR-ROBBERIES(2).
           MOVE WS-WR-RATING-SHEET-PROFIT(2) TO F031YTD.

           IF WR-ELM-OUTS-BALANCE NOT = 0
              COMPUTE WS-WR-PCT-RATE-SHT-PROFIT ROUNDED =
                 WS-WR-RATING-SHEET-PROFIT(1) / WR-ELM-OUTS-BALANCE
                 * 100
           ELSE
              MOVE 0 TO WS-WR-PCT-RATE-SHT-PROFIT.
           MOVE WS-WR-PCT-RATE-SHT-PROFIT    TO F04MTD.
           MOVE WR-YTD-PCT-RATE-SHT-PROFIT   TO F04.

           MOVE WR-RATING-SHEET-INCOME(1)    TO F05.
           MOVE WR-RATING-SHEET-INCOME(2)    TO F051.

           IF WR-ELM-OUTS-BALANCE NOT = 0
              COMPUTE WS-WR-PCT-RS-INC-LEDGER ROUNDED =
                 WR-RATING-SHEET-INCOME(1) / WR-ELM-OUTS-BALANCE
                 * 100
           ELSE
              MOVE 0 TO WS-WR-PCT-RS-INC-LEDGER.
           MOVE WS-WR-PCT-RS-INC-LEDGER   TO F06MTD.
           MOVE WR-YTD-PCT-RS-INC-LEDGER     TO F06.

           MOVE WR-INT-FEE-INCOME(1)         TO F07.
           MOVE WR-INT-FEE-INCOME(2)         TO F071.

           IF WR-ELM-OUTS-BALANCE NOT = 0
              COMPUTE WS-WR-PCT-INT-FEE-LEDGER ROUNDED =
                 WR-INT-FEE-INCOME(1) / WR-ELM-OUTS-BALANCE
                 * 100
           ELSE
              MOVE 0 TO WS-WR-PCT-INT-FEE-LEDGER.
           MOVE WS-WR-PCT-INT-FEE-LEDGER     TO F08MTD.
           MOVE WR-YTD-PCT-INT-FEE-LEDGER    TO F08.

           MOVE WR-INSURANCE-INCOME(1)       TO F09.
           MOVE WR-INSURANCE-INCOME(2)       TO F091.

           IF WR-ELM-OUTS-BALANCE NOT = 0
              COMPUTE WS-WR-PCT-INS-INC-LEDGER ROUNDED =
                 WR-INSURANCE-INCOME(1) / WR-ELM-OUTS-BALANCE
                 * 100
           ELSE
              MOVE 0 TO WS-WR-PCT-INS-INC-LEDGER.
           MOVE WS-WR-PCT-INS-INC-LEDGER    TO F10MTD.
           MOVE WR-YTD-PCT-INS-INC-LEDGER   TO F10.

           MOVE WR-RATING-SHEET-EXPENSES(1) TO F11.
           MOVE WR-RATING-SHEET-EXPENSES(2) TO F111.

           COMPUTE WS-WR-NO-TOT(1) =
                   WR-NO-NB(1) +
                   WR-NO-FB(1) +
                   WR-NO-PB(1).
           IF WS-WR-NO-TOT(1) NOT = 0
              COMPUTE WS-WR-EXPENSE-PER-LOAN(1) ROUNDED =
                 WR-RATING-SHEET-EXPENSES(1) / WS-WR-NO-TOT(1)
           ELSE
              MOVE 0 TO WS-WR-EXPENSE-PER-LOAN(1).
           MOVE WS-WR-EXPENSE-PER-LOAN(1)  TO F12MTD.
           COMPUTE WS-WR-NO-TOT(2) =
                   WR-NO-NB(2) +
                   WR-NO-FB(2) +
                   WR-NO-PB(2).
           IF WS-WR-NO-TOT(2) NOT = 0
              COMPUTE WS-WR-EXPENSE-PER-LOAN(2) ROUNDED =
                 WR-RATING-SHEET-EXPENSES(2) / WS-WR-NO-TOT(2)
           ELSE
              MOVE 0 TO WS-WR-EXPENSE-PER-LOAN(2).
           MOVE WS-WR-EXPENSE-PER-LOAN(2)  TO F121YTD.

           IF WR-ELM-OUTS-BALANCE NOT = 0
              COMPUTE WS-WR-PCT-EXPENSE-LEDGER ROUNDED =
                 WR-RATING-SHEET-EXPENSES(1) / WR-ELM-OUTS-BALANCE
                 * 100
           ELSE
              MOVE 0 TO WS-WR-PCT-EXPENSE-LEDGER.
           MOVE WS-WR-PCT-EXPENSE-LEDGER    TO F13MTD.
           MOVE WR-YTD-PCT-EXPENSE-LEDGER   TO F13.

           IF WR-RATING-SHEET-INCOME(1) NOT = 0
              COMPUTE WS-WR-PCT-EXPENSE-INCOME(1) ROUNDED =
                 WR-RATING-SHEET-EXPENSES(1) / WR-RATING-SHEET-INCOME(1)
                 * 100
           ELSE
              MOVE 0 TO WS-WR-PCT-EXPENSE-INCOME(1).
           MOVE WS-WR-PCT-EXPENSE-INCOME(1) TO F14MTD.

           IF WR-RATING-SHEET-INCOME(2) NOT = 0
              COMPUTE WS-WR-PCT-EXPENSE-INCOME(2) ROUNDED =
                 WR-RATING-SHEET-EXPENSES(2) / WR-RATING-SHEET-INCOME(2)
                 * 100
           ELSE
              MOVE 0 TO WS-WR-PCT-EXPENSE-INCOME(2).
           MOVE WS-WR-PCT-EXPENSE-INCOME(2)  TO F141YTD.

           MOVE WR-PERSONNEL-EXPENSE(1)      TO F15.
           MOVE WR-PERSONNEL-EXPENSE(2)      TO F151.

           IF WR-RATING-SHEET-INCOME(1) NOT = 0
              COMPUTE WS-WR-PCT-PERSONNEL-INC(1) ROUNDED =
                 WR-PERSONNEL-EXPENSE(1) / WR-RATING-SHEET-INCOME(1)
                 * 100
           ELSE
              MOVE 0 TO WS-WR-PCT-PERSONNEL-INC(1).
           MOVE WS-WR-PCT-PERSONNEL-INC(1)  TO F16MTD.

           IF WR-RATING-SHEET-INCOME(2) NOT = 0
              COMPUTE WS-WR-PCT-PERSONNEL-INC(2) ROUNDED =
                 WR-PERSONNEL-EXPENSE(2) / WR-RATING-SHEET-INCOME(2)
                 * 100
           ELSE
              MOVE 0 TO WS-WR-PCT-PERSONNEL-INC(2).
           MOVE WS-WR-PCT-PERSONNEL-INC(2)  TO F161YTD.

           MOVE WR-OUTS-BALANCE             TO FCUROUTS.
           MOVE WR-ELM-OUTS-BALANCE         TO FELMOUTS.
           MOVE WR-ELY-OUTS-BALANCE         TO FELYOUTS.

       DISPLAY-FIELDS-EXIT.
           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *********************************************
       FORM-RESET SECTION.
      *********************************************
           MOVE EXT-CONAME-CONAME TO WRMAN2-CONAME.
           MOVE EXT-CONAME-SYSDATE TO WRMAN2-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY WRMAN2-SCREEN UPON WRMAIN-WINDOW-HANDLE.
           MOVE WRMAN2-FORM-FIELDS TO WR-BUF.
           MOVE WRMAN2-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *********************************************
       SETUP-LINK-INFO SECTION.
      *********************************************
           MOVE WRMAN2-HELP--FILE TO HELP-LINK-FILE.
           MOVE WRMAN2-HELP--DIR TO HELP-LINK-DIR.

      *********************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/WRMAN2_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ***********************************************
       MISC-PARAGRAPHS SECTION.
      ***********************************************
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

      ******************************************
       IO-ROUTINES SECTION.
      ******************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-WR1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBSP/SPWRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBSP/SPWR1IN.CPY".
       COPY "LIBGB/GBBRRN.CPY".

       COPY "LIBGB/FERRORS.CPY".

      ***********************************************
       END-ROUTINE SECTION.
      ***********************************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY WRMAN2-SCREEN.
           MOVE WRMAIN-LINK TO FORM-LINK-AREA.
           EXIT PROGRAM.
