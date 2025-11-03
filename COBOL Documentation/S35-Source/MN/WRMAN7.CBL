       IDENTIFICATION DIVISION.
       PROGRAM-ID.      WRMAN7.
      **************************************************************************
      *                 (MN)
      *
      *  DESCRIPTION:   WORLD RATING SHEET FILE MAINTENANCE - PAGE 7
      *                 LARGE LOANS PG 1
      * REV:
      * BLV 040119 FIXED SCAN KEY
      * BLV 040220 WAS MOVING FIELD TO TO WR-OUTSTANDING-BALANCE INSTEAD OF
      *            WR-LARGE-OUTSTANDING-BALANCE!
      * GLB 040228 ADD MAINTENANCE LOG
      * GLB 040319 GENERAL CLEANUP WHILE CREATING HELP
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
           "@(#)MN/WRMAN7.CBL S34 06/08/21-11:45:53 >".
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
               05  FILLER     PIC X(7) VALUE "#LGCUST".
               05  FILLER     PIC X(7) VALUE "$OUTBAL".
               05  FILLER     PIC X(7) VALUE "FLD 03 ".
               05  FILLER     PIC X(7) VALUE "FLD 04 ".
               05  FILLER     PIC X(7) VALUE "#LG NB ".
               05  FILLER     PIC X(7) VALUE "#LG FB ".
               05  FILLER     PIC X(7) VALUE "#LG PB ".
               05  FILLER     PIC X(7) VALUE "FLD 08 ".
               05  FILLER     PIC X(7) VALUE "GRS VOL".
               05  FILLER     PIC X(7) VALUE "FLD 10 ".
               05  FILLER     PIC X(7) VALUE "#30 DAY".
               05  FILLER     PIC X(7) VALUE "$30 DAY".
               05  FILLER     PIC X(7) VALUE "#60 DAY".
               05  FILLER     PIC X(7) VALUE "$60 DAY".
               05  FILLER     PIC X(7) VALUE "#90 DAY".
               05  FILLER     PIC X(7) VALUE "$90 DAY".
       01  WORK-FIELD-TABLE2 REDEFINES WORK-FIELD-TABLE.
           03  WORK-FIELD-DESC  PIC X(7) OCCURS 16 TIMES.


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
       01  CURRENT-FORM      PIC 99 VALUE 7.

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
       01  CHANGED-WR-REC     PIC X(WR-SIZE)   VALUE SPACES.

       01  COMPUTED-FIELDS               OCCURS 2.
           03  WS-WR-LARGE-GAIN-CUST      PIC S9(5).
           03  WS-WR-LARGE-GAIN-BALANCE   PIC S9(9)V99.
           03  WS-WR-LARGE-PCT-VOLUME     PIC S9(3)V99.
       01  WS-WR-LARGE-AVG-UNPAID-BAL     PIC S9(7)V99.


      ******************************
      *    SCREEN DISPLAY BUFFER   *
      ******************************
       01  DISPLAY-BUF     VALUE SPACES.
           03  F01         PIC ZZ,ZZZ-.
           03  F011        PIC ZZ,ZZZ-.
           03  F012        PIC ZZ,ZZZ-.
           03  F02         PIC ZZZ,ZZZ,ZZZ.ZZ-.
           03  F021        PIC ZZZ,ZZZ,ZZZ.ZZ-.
           03  F022        PIC ZZZ,ZZZ,ZZZ.ZZ-.
           03  F03MTD      PIC ZZ,ZZZ-.
           03  F031YTD     PIC ZZ,ZZZ-.
           03  F04MTD      PIC ZZZ,ZZZ,ZZZ.ZZ-.
           03  F041YTD     PIC ZZZ,ZZZ,ZZZ.ZZ-.
           03  F05         PIC ZZ,ZZZ-.
           03  F051        PIC ZZ,ZZZ-.
           03  F06         PIC ZZ,ZZZ-.
           03  F061        PIC ZZ,ZZZ-.
           03  F07         PIC ZZ,ZZZ-.
           03  F071        PIC ZZ,ZZZ-.
           03  F08MTD      PIC ZZZ.ZZ-.
           03  F081YTD     PIC ZZZ.ZZ-.
           03  F09         PIC ZZZ,ZZZ,ZZZ.ZZ-.
           03  F091        PIC ZZZ,ZZZ,ZZZ.ZZ-.
           03  F101YTD     PIC ZZZ,ZZZ,ZZZ.ZZ-.
           03  F11         PIC Z,ZZZ,ZZZ-.
           03  F12         PIC ZZZ,ZZZ,ZZZ.ZZ-.
           03  F13         PIC Z,ZZZ,ZZZ-.
           03  F14         PIC ZZZ,ZZZ,ZZZ.ZZ-.
           03  F15         PIC Z,ZZZ,ZZZ-.
           03  F16         PIC ZZZ,ZZZ,ZZZ.ZZ-.
           03  F161        PIC ZZZ,ZZZ,ZZZ.ZZ-.
           03  F162        PIC ZZZ,ZZZ,ZZZ.ZZ-.

       01  DISPLAY-LIST.
           03  FILLER      PIC X(38) VALUE
           "010 011 012 020 021 022 030MTD 031YTD ".
           03  FILLER      PIC X(38) VALUE
           "040MTD 041YTD 050 051 060 061 070 071 ".
           03  FILLER      PIC X(33) VALUE
           "080MTD 081YTD 090 091 101YTD 110 ".
           03  FILLER      PIC X(16) VALUE
           "120 130 140 150 ".
           03  FILLER      PIC X(12) VALUE
           "160 161 162.".

      ********************************
      *    ELEMENT SPECIFICATIONS    *
      ********************************
       78  MAX                      VALUE 16.
       01  SPEC-TABLE.
      * 01.  WR-LARGE-CUR-NO-CUST
           03  FILLER      PIC X(8) VALUE "E NS050 ".
      * 02.  WR-LARGE-OUTS-BALANCE
           03  FILLER      PIC X(8) VALUE "E NS092 ".
      * 03.  WS-WR-LARGE-GAIN-CUST (DISPLAY ONLY)
           03  FILLER      PIC X(8) VALUE "E NS050N".
      * 04.  WS-WR-LARGE-GAIN-BALANCE (DISPLAY ONLY)
           03  FILLER      PIC X(8) VALUE "E NS092N".
      * 05.  WR-LARGE-NO-NB
           03  FILLER      PIC X(8) VALUE "E NS050 ".
      * 06.  WR-LARGE-NO-FB
           03  FILLER      PIC X(8) VALUE "E NS050 ".
      * 07.  WR-LARGE-NO-PB
           03  FILLER      PIC X(8) VALUE "E NS050 ".
      * 08.  WS-WR-LARGE-PCT-VOLUME  (DISPLAY ONLY)
           03  FILLER      PIC X(8) VALUE "E NS050N".
      * 09.  WR-LARGE-GROSS-VOLUME
           03  FILLER      PIC X(8) VALUE "E NS092 ".

      * 10.  WS-WR-LARGE-AVG-UNPAID-BAL (DISPLAY ONLY)
           03  FILLER      PIC X(8) VALUE "E NS092N".
      * 11.  WR-LARGE-NO-30-DAY-ACCTS
           03  FILLER      PIC X(8) VALUE "E NS070 ".
      * 12.  WR-LARGE-AMT-30-DAY-ACCTS
           03  FILLER      PIC X(8) VALUE "E NS092 ".
      * 13.  WR-LARGE-NO-60-DAY-ACCTS
           03  FILLER      PIC X(8) VALUE "E NS070 ".
      * 14.  WR-LARGE-AMT-60-DAY-ACCTS
           03  FILLER      PIC X(8) VALUE "E NS092 ".
      * 15.  WR-LARGE-NO-90-DAY-ACCTS
           03  FILLER      PIC X(8) VALUE "E NS070 ".
      * 16.  WR-LARGE-AMT-90-DAY-ACCTS
           03  FILLER      PIC X(8) VALUE "E NS092 ".
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
       COPY "LIBMN/WRMAN7_DEF.CPY".
       COPY "LIBMN/WRMAN7_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       01 FORM-LINK-AREA    PIC X(2000).

       SCREEN SECTION.
       COPY "LIBMN/WRMAN7_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME FORM-LINK-AREA EXIT-PATHNAME.

      *******************************************
       MAIN-PROGRAM SECTION.
      *******************************************
           PERFORM SQL-CONNECT.

           MOVE "WRMAN7" TO VDU-FORM-NAME.

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

      ***********************************************
       REC-WR-MODULE SECTION.
      ***********************************************
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

      ***********************************************
       ENTER-SCANKEY SECTION.
      ***********************************************
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
           IF ELE = 2 OR 3 OR 4 OR 10 OR 11
              PERFORM DISPLAY-FIELDS.
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
           MOVE VDUNUM0 TO WR-LARGE-CUR-NO-CUST.
       F-011.
           MOVE "E NS050  1." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-011.
           MOVE VDUNUM0 TO WR-LARGE-ELM-NO-CUST.
       F-012.
           MOVE "E NS050  2." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO F-011.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-012.
           MOVE VDUNUM0 TO WR-LARGE-ELY-NO-CUST.
           GO TO CREATE-ROUTINE.
       F-02.
           MOVE VDUNUM2 TO WR-LARGE-OUTS-BALANCE.
       F-021.
           MOVE "E NS092  1." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-021.
           MOVE VDUNUM2 TO WR-LARGE-ELM-OUTS-BAL.
       F-022.
           MOVE "E NS092  2." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO F-021.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-022.
           MOVE VDUNUM2 TO WR-LARGE-ELY-OUTS-BAL.
           GO TO CREATE-ROUTINE.
       F-03.
           GO TO CREATE-ROUTINE.
       F-04.
           GO TO CREATE-ROUTINE.
       F-05.
           MOVE VDUNUM0 TO WR-LARGE-NO-NB(1).
       F-051.
           MOVE "E NS050  1." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-051.
           MOVE VDUNUM0 TO WR-LARGE-NO-NB(2).
           GO TO CREATE-ROUTINE.
       F-06.
           MOVE VDUNUM0 TO WR-LARGE-NO-FB(1).
       F-061.
           MOVE "E NS050  1." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-061.
           MOVE VDUNUM0 TO WR-LARGE-NO-FB(2).
           GO TO CREATE-ROUTINE.
       F-07.
           MOVE VDUNUM0 TO WR-LARGE-NO-PB(1).
       F-071.
           MOVE "E NS050  1." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-071.
           MOVE VDUNUM0 TO WR-LARGE-NO-PB(2).
           GO TO CREATE-ROUTINE.
       F-08.
           GO TO CREATE-ROUTINE.
       F-09.
           MOVE VDUNUM2 TO WR-LARGE-GROSS-VOLUME(1).
       F-091.
           MOVE "E NS092  1." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-091.
           MOVE VDUNUM2 TO WR-LARGE-GROSS-VOLUME(2).
           GO TO CREATE-ROUTINE.

       F-10.
           GO TO CREATE-ROUTINE.
       F-11.
           MOVE VDUNUM0 TO WR-LARGE-NO-30-DAY-ACCTS.
           GO TO CREATE-ROUTINE.
       F-12.
           MOVE VDUNUM2 TO WR-LARGE-AMT-30-DAY-ACCTS.
           GO TO CREATE-ROUTINE.
       F-13.
           MOVE VDUNUM0 TO WR-LARGE-NO-60-DAY-ACCTS.
           GO TO CREATE-ROUTINE.
       F-14.
           MOVE VDUNUM2 TO WR-LARGE-AMT-60-DAY-ACCTS.
           GO TO CREATE-ROUTINE.
       F-15.
           MOVE VDUNUM0 TO WR-LARGE-NO-90-DAY-ACCTS.
           GO TO CREATE-ROUTINE.
       F-16.
           MOVE VDUNUM2 TO WR-LARGE-AMT-90-DAY-ACCTS.
       F-161.
           MOVE "E NS092  1." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-161.
           MOVE VDUNUM2 TO WR-LARGE-ELM-AMT-90-DAYS.
       F-162.
           MOVE "E NS092  2." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-162.
           MOVE VDUNUM2 TO WR-LARGE-ELY-AMT-90-DAYS.
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

       WRITE-RECORD.
           PERFORM WRITE-WR1-FILE.
           IF IO-FG NOT = 0
              PERFORM ALREADY-CREATED
           ELSE
              MOVE "Y" TO NEW-REC.
       END-WRITE-MODULE.
           PERFORM CLOSE-WR1-FILE.

      *****************************************
       LOG-MAINTENANCE-CHGS SECTION.
      *****************************************
           MOVE ORIGINAL-WR-REC TO WROR-REC.
           MOVE CHANGED-WR-REC  TO WRCH-REC.

           MOVE SPACES  TO WORK-ERRLOG-KEY.
           MOVE WR1-KEY TO WORK-ERRLOG-WR1.
           MOVE "TO"    TO  WORK-ERRLOG-TO-X.

           IF WROR-LARGE-CUR-NO-CUST NOT = WRCH-LARGE-CUR-NO-CUST
             MOVE "01 "                 TO WORK-ERRLOG-FIELD
             MOVE 1                     TO SUB
             MOVE WORK-FIELD-DESC(SUB)  TO WORK-ERRLOG-DESC
             MOVE WROR-LARGE-CUR-NO-CUST      TO WORK-ERRLOG-ORG-N0
             MOVE WRCH-LARGE-CUR-NO-CUST      TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.
           IF WROR-LARGE-ELM-NO-CUST NOT = WRCH-LARGE-ELM-NO-CUST
             MOVE "011"                 TO WORK-ERRLOG-FIELD
             MOVE 1                     TO SUB
             MOVE WORK-FIELD-DESC(SUB)  TO WORK-ERRLOG-DESC
             MOVE WROR-LARGE-ELM-NO-CUST      TO WORK-ERRLOG-ORG-N0
             MOVE WRCH-LARGE-ELM-NO-CUST      TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.
           IF WROR-LARGE-ELY-NO-CUST NOT = WRCH-LARGE-ELY-NO-CUST
             MOVE "012"                 TO WORK-ERRLOG-FIELD
             MOVE 1                     TO SUB
             MOVE WORK-FIELD-DESC(SUB)  TO WORK-ERRLOG-DESC
             MOVE WROR-LARGE-ELY-NO-CUST      TO WORK-ERRLOG-ORG-N0
             MOVE WRCH-LARGE-ELY-NO-CUST      TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF WROR-LARGE-OUTS-BALANCE NOT = WRCH-LARGE-OUTS-BALANCE
             MOVE "02 "                 TO WORK-ERRLOG-FIELD
             MOVE 2                     TO SUB
             MOVE WORK-FIELD-DESC(SUB)  TO WORK-ERRLOG-DESC
             MOVE WROR-LARGE-OUTS-BALANCE      TO WORK-ERRLOG-ORG-N2
             MOVE WRCH-LARGE-OUTS-BALANCE      TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.
           IF WROR-LARGE-ELM-OUTS-BAL NOT = WRCH-LARGE-ELM-OUTS-BAL
             MOVE "021"                 TO WORK-ERRLOG-FIELD
             MOVE 2                     TO SUB
             MOVE WORK-FIELD-DESC(SUB)  TO WORK-ERRLOG-DESC
             MOVE WROR-LARGE-ELM-OUTS-BAL      TO WORK-ERRLOG-ORG-N2
             MOVE WRCH-LARGE-ELM-OUTS-BAL      TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.
           IF WROR-LARGE-ELY-OUTS-BAL NOT = WRCH-LARGE-ELY-OUTS-BAL
             MOVE "022"                 TO WORK-ERRLOG-FIELD
             MOVE 2                     TO SUB
             MOVE WORK-FIELD-DESC(SUB)  TO WORK-ERRLOG-DESC
             MOVE WROR-LARGE-ELY-OUTS-BAL      TO WORK-ERRLOG-ORG-N2
             MOVE WRCH-LARGE-ELY-OUTS-BAL      TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF WROR-LARGE-NO-NB(1) NOT = WRCH-LARGE-NO-NB(1)
             MOVE "05 "                 TO WORK-ERRLOG-FIELD
             MOVE 5                     TO SUB
             MOVE WORK-FIELD-DESC(SUB)  TO WORK-ERRLOG-DESC
             MOVE WROR-LARGE-NO-NB(1)   TO WORK-ERRLOG-ORG-N2
             MOVE WRCH-LARGE-NO-NB(1)   TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.
           IF WROR-LARGE-NO-NB(2) NOT = WRCH-LARGE-NO-NB(2)
             MOVE "051"                 TO WORK-ERRLOG-FIELD
             MOVE 5                     TO SUB
             MOVE WORK-FIELD-DESC(SUB)  TO WORK-ERRLOG-DESC
             MOVE WROR-LARGE-NO-NB(2)  TO WORK-ERRLOG-ORG-N2
             MOVE WRCH-LARGE-NO-NB(2)  TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF WROR-LARGE-NO-FB(1) NOT = WRCH-LARGE-NO-FB(1)
             MOVE "06 "                 TO WORK-ERRLOG-FIELD
             MOVE 6                     TO SUB
             MOVE WORK-FIELD-DESC(SUB)  TO WORK-ERRLOG-DESC
             MOVE WROR-LARGE-NO-FB(1)   TO WORK-ERRLOG-ORG-N2
             MOVE WRCH-LARGE-NO-FB(1)   TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.
           IF WROR-LARGE-NO-FB(2) NOT = WRCH-LARGE-NO-FB(2)
             MOVE "061"                 TO WORK-ERRLOG-FIELD
             MOVE 6                     TO SUB
             MOVE WORK-FIELD-DESC(SUB)  TO WORK-ERRLOG-DESC
             MOVE WROR-LARGE-NO-FB(2)  TO WORK-ERRLOG-ORG-N2
             MOVE WRCH-LARGE-NO-FB(2)  TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF WROR-LARGE-NO-PB(1) NOT = WRCH-LARGE-NO-PB(1)
             MOVE "07 "                 TO WORK-ERRLOG-FIELD
             MOVE 7                     TO SUB
             MOVE WORK-FIELD-DESC(SUB)  TO WORK-ERRLOG-DESC
             MOVE WROR-LARGE-NO-PB(1)   TO WORK-ERRLOG-ORG-N2
             MOVE WRCH-LARGE-NO-PB(1)   TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.
           IF WROR-LARGE-NO-PB(2) NOT = WRCH-LARGE-NO-PB(2)
             MOVE "071"                 TO WORK-ERRLOG-FIELD
             MOVE 7                     TO SUB
             MOVE WORK-FIELD-DESC(SUB)  TO WORK-ERRLOG-DESC
             MOVE WROR-LARGE-NO-PB(2)  TO WORK-ERRLOG-ORG-N2
             MOVE WRCH-LARGE-NO-PB(2)  TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF WROR-LARGE-GROSS-VOLUME(1) NOT =
             WRCH-LARGE-GROSS-VOLUME(1)
             MOVE "09 "                 TO WORK-ERRLOG-FIELD
             MOVE 9                     TO SUB
             MOVE WORK-FIELD-DESC(SUB)  TO WORK-ERRLOG-DESC
             MOVE WROR-LARGE-GROSS-VOLUME(1)   TO WORK-ERRLOG-ORG-N2
             MOVE WRCH-LARGE-GROSS-VOLUME(1)   TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.
           IF WROR-LARGE-GROSS-VOLUME(2) NOT =
             WRCH-LARGE-GROSS-VOLUME(2)
             MOVE "091"                 TO WORK-ERRLOG-FIELD
             MOVE 9                     TO SUB
             MOVE WORK-FIELD-DESC(SUB)  TO WORK-ERRLOG-DESC
             MOVE WROR-LARGE-GROSS-VOLUME(2)  TO WORK-ERRLOG-ORG-N2
             MOVE WRCH-LARGE-GROSS-VOLUME(2)  TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

      *-----------------------------------------------

           IF WROR-LARGE-NO-30-DAY-ACCTS NOT =
             WRCH-LARGE-NO-30-DAY-ACCTS
             MOVE "11 "                 TO WORK-ERRLOG-FIELD
             MOVE 11                    TO SUB
             MOVE WORK-FIELD-DESC(SUB)  TO WORK-ERRLOG-DESC
             MOVE WROR-LARGE-NO-30-DAY-ACCTS   TO WORK-ERRLOG-ORG-N0
             MOVE WRCH-LARGE-NO-30-DAY-ACCTS   TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.
           IF WROR-LARGE-AMT-30-DAY-ACCTS NOT =
             WRCH-LARGE-AMT-30-DAY-ACCTS
             MOVE "12 "                 TO WORK-ERRLOG-FIELD
             MOVE 12                    TO SUB
             MOVE WORK-FIELD-DESC(SUB)  TO WORK-ERRLOG-DESC
             MOVE WROR-LARGE-AMT-30-DAY-ACCTS  TO WORK-ERRLOG-ORG-N2
             MOVE WRCH-LARGE-AMT-30-DAY-ACCTS  TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF WROR-LARGE-NO-60-DAY-ACCTS NOT =
             WRCH-LARGE-NO-60-DAY-ACCTS
             MOVE "13 "                 TO WORK-ERRLOG-FIELD
             MOVE 13                    TO SUB
             MOVE WORK-FIELD-DESC(SUB)  TO WORK-ERRLOG-DESC
             MOVE WROR-LARGE-NO-60-DAY-ACCTS   TO WORK-ERRLOG-ORG-N0
             MOVE WRCH-LARGE-NO-60-DAY-ACCTS   TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.
           IF WROR-LARGE-AMT-60-DAY-ACCTS NOT =
             WRCH-LARGE-AMT-60-DAY-ACCTS
             MOVE "14 "                 TO WORK-ERRLOG-FIELD
             MOVE 14                    TO SUB
             MOVE WORK-FIELD-DESC(SUB)  TO WORK-ERRLOG-DESC
             MOVE WROR-LARGE-AMT-60-DAY-ACCTS  TO WORK-ERRLOG-ORG-N2
             MOVE WRCH-LARGE-AMT-60-DAY-ACCTS  TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF WROR-LARGE-NO-90-DAY-ACCTS NOT =
             WRCH-LARGE-NO-90-DAY-ACCTS
             MOVE "15 "                 TO WORK-ERRLOG-FIELD
             MOVE 15                    TO SUB
             MOVE WORK-FIELD-DESC(SUB)  TO WORK-ERRLOG-DESC
             MOVE WROR-LARGE-NO-90-DAY-ACCTS   TO WORK-ERRLOG-ORG-N0
             MOVE WRCH-LARGE-NO-90-DAY-ACCTS   TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.
           IF WROR-LARGE-AMT-90-DAY-ACCTS NOT =
             WRCH-LARGE-AMT-90-DAY-ACCTS
             MOVE "16 "                 TO WORK-ERRLOG-FIELD
             MOVE 16                    TO SUB
             MOVE WORK-FIELD-DESC(SUB)  TO WORK-ERRLOG-DESC
             MOVE WROR-LARGE-AMT-90-DAY-ACCTS  TO WORK-ERRLOG-ORG-N2
             MOVE WRCH-LARGE-AMT-90-DAY-ACCTS  TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

      *****************************************
       LOG-FIELD-CHANGE SECTION.
      *****************************************
      * CODE TO WRITE CHANGE FILE.

           MOVE "X" TO ERRLOGW-ACTION.
           MOVE WORK-ERRLOG-KEY TO ERRLOGW-EOM-MSG.
           MOVE FORM-PATHNAME TO ERRLOGW-PROGX.

           MOVE "CL/ERRLOG" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PROGX.

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

      *************************************
       DISPLAY-FIELDS SECTION.
      *************************************
           MOVE SPACES TO DISPLAY-BUF.

           MOVE WR-LARGE-CUR-NO-CUST         TO F01.
           MOVE WR-LARGE-ELM-NO-CUST         TO F011.
           MOVE WR-LARGE-ELY-NO-CUST         TO F012.

           MOVE WR-LARGE-OUTS-BALANCE        TO F02.
           MOVE WR-LARGE-ELM-OUTS-BAL        TO F021.
           MOVE WR-LARGE-ELY-OUTS-BAL        TO F022.

           COMPUTE WS-WR-LARGE-GAIN-CUST(1) =
              WR-LARGE-CUR-NO-CUST - WR-LARGE-ELM-NO-CUST.
           MOVE WS-WR-LARGE-GAIN-CUST(1)     TO F03MTD.
           COMPUTE WS-WR-LARGE-GAIN-CUST(2) =
              WR-LARGE-CUR-NO-CUST - WR-LARGE-ELY-NO-CUST.
           MOVE WS-WR-LARGE-GAIN-CUST(2)     TO F031YTD.

           COMPUTE WS-WR-LARGE-GAIN-BALANCE(1) =
              WR-LARGE-OUTS-BALANCE - WR-LARGE-ELM-OUTS-BAL.
           MOVE WS-WR-LARGE-GAIN-BALANCE(1)  TO F04MTD.
           COMPUTE WS-WR-LARGE-GAIN-BALANCE(2) =
              WR-LARGE-OUTS-BALANCE - WR-LARGE-ELY-OUTS-BAL.
           MOVE WS-WR-LARGE-GAIN-BALANCE(2)  TO F041YTD.

           MOVE WR-LARGE-NO-NB(1)            TO F05.
           MOVE WR-LARGE-NO-NB(2)            TO F051.

           MOVE WR-LARGE-NO-FB(1)            TO F06.
           MOVE WR-LARGE-NO-FB(2)            TO F061.

           MOVE WR-LARGE-NO-PB(1)            TO F07.
           MOVE WR-LARGE-NO-PB(2)            TO F071.

           IF WR-LARGE-ELM-OUTS-BAL NOT = 0
              COMPUTE WS-WR-LARGE-PCT-VOLUME(1) ROUNDED =
                 WR-LARGE-GROSS-VOLUME(1) / WR-LARGE-ELM-OUTS-BAL
                 * 100
           ELSE
              MOVE 0 TO WS-WR-LARGE-PCT-VOLUME(1).
           MOVE WS-WR-LARGE-PCT-VOLUME(1)    TO F08MTD.

           IF WR-LARGE-ELY-OUTS-BAL NOT = 0
              COMPUTE WS-WR-LARGE-PCT-VOLUME(2) ROUNDED =
                 WR-LARGE-GROSS-VOLUME(2) / WR-LARGE-ELY-OUTS-BAL
                 * 100
           ELSE
              MOVE 0 TO WS-WR-LARGE-PCT-VOLUME(2).
           MOVE WS-WR-LARGE-PCT-VOLUME(2)    TO F081YTD.

           MOVE WR-LARGE-GROSS-VOLUME(1)     TO F09.
           MOVE WR-LARGE-GROSS-VOLUME(2)     TO F091.

           IF WR-LARGE-CUR-NO-CUST NOT = 0
              COMPUTE WS-WR-LARGE-AVG-UNPAID-BAL ROUNDED =
                 WR-LARGE-OUTS-BALANCE / WR-LARGE-CUR-NO-CUST
              ELSE
                 MOVE 0 TO WS-WR-LARGE-AVG-UNPAID-BAL.
           MOVE WS-WR-LARGE-AVG-UNPAID-BAL   TO F101YTD.

           MOVE WR-LARGE-NO-30-DAY-ACCTS     TO F11.

           MOVE WR-LARGE-AMT-30-DAY-ACCTS    TO F12.

           MOVE WR-LARGE-NO-60-DAY-ACCTS     TO F13.

           MOVE WR-LARGE-AMT-60-DAY-ACCTS    TO F14.

           MOVE WR-LARGE-NO-90-DAY-ACCTS     TO F15.

           MOVE WR-LARGE-AMT-90-DAY-ACCTS    TO F16.
           MOVE WR-LARGE-ELM-AMT-90-DAYS     TO F161.
           MOVE WR-LARGE-ELY-AMT-90-DAYS     TO F162.

       DISPLAY-FIELDS-EXIT.
           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *********************************************
       FORM-RESET SECTION.
      *********************************************
           MOVE EXT-CONAME-CONAME TO WRMAN7-CONAME.
           MOVE EXT-CONAME-SYSDATE TO WRMAN7-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY WRMAN7-SCREEN UPON WRMAIN-WINDOW-HANDLE.
           MOVE WRMAN7-FORM-FIELDS TO WR-BUF.
           MOVE WRMAN7-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *********************************************
       SETUP-LINK-INFO SECTION.
      *********************************************
           MOVE WRMAN7-HELP--FILE TO HELP-LINK-FILE.
           MOVE WRMAN7-HELP--DIR TO HELP-LINK-DIR.

      *********************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/WRMAN7_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *********************************************
       MISC-PARAGRAPHS SECTION.
      *********************************************
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

      *********************************************
       IO-ROUTINES SECTION.
      *********************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-WR1-FILE.
       COPY "LIBSP/SPWRGS_SQL.CPY".
       COPY "LIBSP/SPWR1IN.CPY".

       COPY "LIBGB/FERRORS.CPY".

      *********************************************
       END-ROUTINE SECTION.
      *********************************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY WRMAN7-SCREEN.
           MOVE WRMAIN-LINK TO FORM-LINK-AREA.
           EXIT PROGRAM.
