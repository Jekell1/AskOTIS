       IDENTIFICATION DIVISION.
       PROGRAM-ID.      WRMAN9.
      **************************************************************************
      *                 (MN)
      *
      *  DESCRIPTION:   WORLD RATING SHEET FILE MAINTENANCE - PAGE 8
      *                 INSURANCE PREMIUMS PG 2, # & AMT, MTD & YTD
      *
      * REV:
      * BLV 040119 FIXED SCAN KEY
      * GLB 040224 ADD WR-NF-RECOVERIES(1), (2)
      * GLB 040226 FIX DATE CONVERSION BUG;MOVE NUM-DATE TO DATE-MMDDYY
      * GLB 040301 ADD MAINTENANCE LOG
      * GLB 040326 GENERAL CLEANUP WHILE CREATING HELP
      *  CS 041119 ADD LIFE OF SOUTH NON FILE PREMIUMS
      *  CS 160509 CHANGED ORIGINAL/CHANGED REC TO HAVE WR
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
           "@(#)MN/WRMAN9.CBL S34 06/08/21-11:48:14 >".
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
               05  FILLER     PIC X(7) VALUE "LIFE PR".
               05  FILLER     PIC X(7) VALUE "A&H PR ".
               05  FILLER     PIC X(7) VALUE "PROP PR".
               05  FILLER     PIC X(7) VALUE "AUTO PR".
               05  FILLER     PIC X(7) VALUE "UNEMPL ".
               05  FILLER     PIC X(7) VALUE "A&D PR ".
               05  FILLER     PIC X(7) VALUE "LIEN PR".
               05  FILLER     PIC X(7) VALUE "NON FIL".
               05  FILLER     PIC X(7) VALUE "ROAD PR".
               05  FILLER     PIC X(7) VALUE "NF RECV".
               05  FILLER     PIC X(7) VALUE "LNF REC".

       01  WORK-FIELD-TABLE2 REDEFINES WORK-FIELD-TABLE.
           03  WORK-FIELD-DESC  PIC X(7) OCCURS 11 TIMES.


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
       01  CURRENT-FORM      PIC 99 VALUE 9.

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
           03  SCAN-WR-YY     PIC 9(2).
           03  SCAN-WR-MM     PIC 9(2).
           03  SCAN-WR-BRNO   PIC 9(4).

       01  ORIGINAL-WR-REC    PIC X(WR-SIZE)   VALUE SPACES.
       01  CHANGED-WR-REC     PIC X(WR-SIZE)   VALUE SPACES.

      ******************************
      *    SCREEN DISPLAY BUFFER   *
      ******************************
       01  DISPLAY-BUF     VALUE SPACES.
           03  F01         PIC Z,ZZZ,ZZZ.ZZ-.
           03  F011        PIC Z,ZZZ,ZZZ.ZZ-.
           03  F02         PIC Z,ZZZ,ZZZ.ZZ-.
           03  F021        PIC Z,ZZZ,ZZZ.ZZ-.
           03  F03         PIC Z,ZZZ,ZZZ.ZZ-.
           03  F031        PIC Z,ZZZ,ZZZ.ZZ-.
           03  F04         PIC Z,ZZZ,ZZZ.ZZ-.
           03  F041        PIC Z,ZZZ,ZZZ.ZZ-.
           03  F05         PIC Z,ZZZ,ZZZ.ZZ-.
           03  F051        PIC Z,ZZZ,ZZZ.ZZ-.
           03  F06         PIC Z,ZZZ,ZZZ.ZZ-.
           03  F061        PIC Z,ZZZ,ZZZ.ZZ-.
           03  F07         PIC Z,ZZZ,ZZZ.ZZ-.
           03  F071        PIC Z,ZZZ,ZZZ.ZZ-.
           03  F08         PIC Z,ZZZ,ZZZ.ZZ-.
           03  F081        PIC Z,ZZZ,ZZZ.ZZ-.
           03  F09         PIC Z,ZZZ,ZZZ.ZZ-.
           03  F091        PIC Z,ZZZ,ZZZ.ZZ-.
           03  F10         PIC Z,ZZZ,ZZZ.ZZ-.
           03  F101        PIC Z,ZZZ,ZZZ.ZZ-.
           03  F11         PIC Z,ZZZ,ZZZ.ZZ-.
           03  F111        PIC Z,ZZZ,ZZZ.ZZ-.

       01  DISPLAY-LIST.
           03  FILLER      PIC X(32) VALUE
           "010 011 020 021 030 031 040 041 ".
           03  FILLER      PIC X(32) VALUE
           "050 051 060 061 070 071 080 081 ".
           03  FILLER      PIC X(24) VALUE
           "090 091 100 101 110 111.".

      ********************************
      *    ELEMENT SPECIFICATIONS    *
      ********************************
       78  MAX                      VALUE 11.
       01  SPEC-TABLE.
      * 01.  LIFE REFUNDS
           03  FILLER      PIC X(8) VALUE "E NS072 ".
      * 02.  A&H REFUNDS
           03  FILLER      PIC X(8) VALUE "E NS072 ".
      * 03.  PROPERTY REFUNDS
           03  FILLER      PIC X(8) VALUE "E NS072 ".
      * 04.  AUTO REFUNDS
           03  FILLER      PIC X(8) VALUE "E NS072 ".
      * 05.  UNEMP REFUNDS
           03  FILLER      PIC X(8) VALUE "E NS072 ".
      * 06.  AD&D REFUNDS
           03  FILLER      PIC X(8) VALUE "E NS072 ".
      * 07.  LIEN PROTECTION REFUNDS
           03  FILLER      PIC X(8) VALUE "E NS072 ".
      * 08.  NON-FILE REFUNDS
           03  FILLER      PIC X(8) VALUE "E NS072 ".
      * 09.  ROADGARD REFUNDS
           03  FILLER      PIC X(8) VALUE "E NS072 ".
      * 10.  NF RECOVERIES
           03  FILLER      PIC X(8) VALUE "E NS072 ".
      * 11. LOTS NF RECOVERIES
           03  FILLER      PIC X(8) VALUE "E NS072 ".


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
       COPY "LIBMN/WRMAN9_DEF.CPY".
       COPY "LIBMN/WRMAN9_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       01 FORM-LINK-AREA    PIC X(2000).

       SCREEN SECTION.
       COPY "LIBMN/WRMAN9_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME FORM-LINK-AREA EXIT-PATHNAME.

      *********************************************
       MAIN-PROGRAM SECTION.
      *********************************************
           PERFORM SQL-CONNECT.

           MOVE "WRMAN9" TO VDU-FORM-NAME.

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
           MOVE WR-BRNO        TO VDUNUM0.
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
           GO TO F-01 F-02 F-03 F-04 F-05 F-06
                 F-07 F-08 F-09 F-10 F-11
                 DEPENDING ON ELE.
           GO TO END-ENTRY-MODULE.

      *=================================================================
      *
      *    S E T - R E C O R D - E L E M E N T S
      *
      *=================================================================
       F-01.
           MOVE VDUNUM2 TO WR-LIFE-REFS(1).
       F-011.
           MOVE "E NS072  1." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-011.
           MOVE VDUNUM2 TO WR-LIFE-REFS(2).
           GO TO CREATE-ROUTINE.
       F-02.
           MOVE VDUNUM2 TO WR-AH-REFS(1).
       F-021.
           MOVE "E NS072  1." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-021.
           MOVE VDUNUM2 TO WR-AH-REFS(2).
           GO TO CREATE-ROUTINE.
       F-03.
           MOVE VDUNUM2 TO WR-FIRE-REFS(1).
       F-031.
           MOVE "E NS072  1." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-031.
           MOVE VDUNUM2 TO WR-FIRE-REFS(2).
           GO TO CREATE-ROUTINE.
       F-04.
           MOVE VDUNUM2 TO WR-AUTO-REFS(1).
       F-041.
           MOVE "E NS072  1." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-041.
           MOVE VDUNUM2 TO WR-AUTO-REFS(2).
           GO TO CREATE-ROUTINE.
       F-05.
           MOVE VDUNUM2 TO WR-UNEMP-REFS(1).
       F-051.
           MOVE "E NS072  1." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-051.
           MOVE VDUNUM2 TO WR-UNEMP-REFS(2).
           GO TO CREATE-ROUTINE.
       F-06.
           MOVE VDUNUM2 TO WR-AD-D-REFS(1).
       F-061.
           MOVE "E NS072  1." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-061.
           MOVE VDUNUM2 TO WR-AD-D-REFS(2).
           GO TO CREATE-ROUTINE.
       F-07.
           MOVE VDUNUM2 TO WR-L-PROT-REFS(1).
       F-071.
           MOVE "E NS072  1." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-071.
           MOVE VDUNUM2 TO WR-L-PROT-REFS(2).
           GO TO CREATE-ROUTINE.
       F-08.
           MOVE VDUNUM2 TO WR-NON-FILE-REFS(1).
       F-081.
           MOVE "E NS072  1." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-081.
           MOVE VDUNUM2 TO WR-NON-FILE-REFS(2).
           GO TO CREATE-ROUTINE.
       F-09.
           MOVE VDUNUM2 TO WR-ROAD-GARD-REFS(1).
       F-091.
           MOVE "E NS072  1." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-091.
           MOVE VDUNUM2 TO WR-ROAD-GARD-REFS(2).
           GO TO CREATE-ROUTINE.

       F-10.
           MOVE VDUNUM2 TO WR-NF-RECOVERIES(1).
       F-101.
           MOVE "E NS072  1." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-101.
           MOVE VDUNUM2 TO WR-NF-RECOVERIES(2).
           GO TO CREATE-ROUTINE.

       F-11.
           MOVE VDUNUM2 TO WR-LOTS-NF-RECOVERIES(1).
       F-111.
           MOVE "E NS072  1." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-111.
           MOVE VDUNUM2 TO WR-LOTS-NF-RECOVERIES(2).
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

           IF WROR-LIFE-REFS(1) NOT =
             WRCH-LIFE-REFS(1)
             MOVE "01 "                 TO WORK-ERRLOG-FIELD
             MOVE 1                     TO SUB
             MOVE WORK-FIELD-DESC(SUB)  TO WORK-ERRLOG-DESC
             MOVE WROR-LIFE-REFS(1)  TO WORK-ERRLOG-ORG-N2
             MOVE WRCH-LIFE-REFS(1)  TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.
           IF WROR-LIFE-REFS(2) NOT =
             WRCH-LIFE-REFS(2)
             MOVE "012"                 TO WORK-ERRLOG-FIELD
             MOVE 1                     TO SUB
             MOVE WORK-FIELD-DESC(SUB)  TO WORK-ERRLOG-DESC
             MOVE WROR-LIFE-REFS(2)  TO WORK-ERRLOG-ORG-N2
             MOVE WRCH-LIFE-REFS(2)  TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF WROR-AH-REFS(1) NOT =
             WRCH-AH-REFS(1)
             MOVE "02 "                 TO WORK-ERRLOG-FIELD
             MOVE 2                     TO SUB
             MOVE WORK-FIELD-DESC(SUB)  TO WORK-ERRLOG-DESC
             MOVE WROR-AH-REFS(1)  TO WORK-ERRLOG-ORG-N2
             MOVE WRCH-AH-REFS(1)  TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.
           IF WROR-AH-REFS(2) NOT =
             WRCH-AH-REFS(2)
             MOVE "022"                 TO WORK-ERRLOG-FIELD
             MOVE 2                     TO SUB
             MOVE WORK-FIELD-DESC(SUB)  TO WORK-ERRLOG-DESC
             MOVE WROR-AH-REFS(2)  TO WORK-ERRLOG-ORG-N2
             MOVE WRCH-AH-REFS(2)  TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF WROR-FIRE-REFS(1) NOT =
             WRCH-FIRE-REFS(1)
             MOVE "03 "                 TO WORK-ERRLOG-FIELD
             MOVE 3                     TO SUB
             MOVE WORK-FIELD-DESC(SUB)  TO WORK-ERRLOG-DESC
             MOVE WROR-FIRE-REFS(1)  TO WORK-ERRLOG-ORG-N2
             MOVE WRCH-FIRE-REFS(1)  TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.
           IF WROR-FIRE-REFS(2) NOT =
             WRCH-FIRE-REFS(2)
             MOVE "032"                 TO WORK-ERRLOG-FIELD
             MOVE 3                     TO SUB
             MOVE WORK-FIELD-DESC(SUB)  TO WORK-ERRLOG-DESC
             MOVE WROR-FIRE-REFS(2)  TO WORK-ERRLOG-ORG-N2
             MOVE WRCH-FIRE-REFS(2)  TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF WROR-AUTO-REFS(1) NOT =
             WRCH-AUTO-REFS(1)
             MOVE "04 "                 TO WORK-ERRLOG-FIELD
             MOVE 4                     TO SUB
             MOVE WORK-FIELD-DESC(SUB)  TO WORK-ERRLOG-DESC
             MOVE WROR-AUTO-REFS(1)  TO WORK-ERRLOG-ORG-N2
             MOVE WRCH-AUTO-REFS(1)  TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.
           IF WROR-AUTO-REFS(2) NOT =
             WRCH-AUTO-REFS(2)
             MOVE "042"                 TO WORK-ERRLOG-FIELD
             MOVE 4                     TO SUB
             MOVE WORK-FIELD-DESC(SUB)  TO WORK-ERRLOG-DESC
             MOVE WROR-AUTO-REFS(2)  TO WORK-ERRLOG-ORG-N2
             MOVE WRCH-AUTO-REFS(2)  TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF WROR-UNEMP-REFS(1) NOT =
             WRCH-UNEMP-REFS(1)
             MOVE "05 "                 TO WORK-ERRLOG-FIELD
             MOVE 5                     TO SUB
             MOVE WORK-FIELD-DESC(SUB)  TO WORK-ERRLOG-DESC
             MOVE WROR-UNEMP-REFS(1)  TO WORK-ERRLOG-ORG-N2
             MOVE WRCH-UNEMP-REFS(1)  TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.
           IF WROR-UNEMP-REFS(2) NOT =
             WRCH-UNEMP-REFS(2)
             MOVE "052"                 TO WORK-ERRLOG-FIELD
             MOVE 5                     TO SUB
             MOVE WORK-FIELD-DESC(SUB)  TO WORK-ERRLOG-DESC
             MOVE WROR-UNEMP-REFS(2)  TO WORK-ERRLOG-ORG-N2
             MOVE WRCH-UNEMP-REFS(2)  TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF WROR-AD-D-REFS(1) NOT =
             WRCH-AD-D-REFS(1)
             MOVE "06 "                 TO WORK-ERRLOG-FIELD
             MOVE 6                     TO SUB
             MOVE WORK-FIELD-DESC(SUB)  TO WORK-ERRLOG-DESC
             MOVE WROR-AD-D-REFS(1)  TO WORK-ERRLOG-ORG-N2
             MOVE WRCH-AD-D-REFS(1)  TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.
           IF WROR-AD-D-REFS(2) NOT =
             WRCH-AD-D-REFS(2)
             MOVE "062"                 TO WORK-ERRLOG-FIELD
             MOVE 6                     TO SUB
             MOVE WORK-FIELD-DESC(SUB)  TO WORK-ERRLOG-DESC
             MOVE WROR-AD-D-REFS(2)  TO WORK-ERRLOG-ORG-N2
             MOVE WRCH-AD-D-REFS(2)  TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF WROR-L-PROT-REFS(1) NOT =
             WRCH-L-PROT-REFS(1)
             MOVE "07 "                 TO WORK-ERRLOG-FIELD
             MOVE 7                     TO SUB
             MOVE WORK-FIELD-DESC(SUB)  TO WORK-ERRLOG-DESC
             MOVE WROR-L-PROT-REFS(1)  TO WORK-ERRLOG-ORG-N2
             MOVE WRCH-L-PROT-REFS(1)  TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.
           IF WROR-L-PROT-REFS(2) NOT =
             WRCH-L-PROT-REFS(2)
             MOVE "072"                 TO WORK-ERRLOG-FIELD
             MOVE 7                     TO SUB
             MOVE WORK-FIELD-DESC(SUB)  TO WORK-ERRLOG-DESC
             MOVE WROR-L-PROT-REFS(2)  TO WORK-ERRLOG-ORG-N2
             MOVE WRCH-L-PROT-REFS(2)  TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.
             PERFORM LOG-FIELD-CHANGE.

           IF WROR-NON-FILE-REFS(1) NOT =
             WRCH-NON-FILE-REFS(1)
             MOVE "08 "                 TO WORK-ERRLOG-FIELD
             MOVE 8                     TO SUB
             MOVE WORK-FIELD-DESC(SUB)  TO WORK-ERRLOG-DESC
             MOVE WROR-NON-FILE-REFS(1)  TO WORK-ERRLOG-ORG-N2
             MOVE WRCH-NON-FILE-REFS(1)  TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.
           IF WROR-NON-FILE-REFS(2) NOT =
             WRCH-NON-FILE-REFS(2)
             MOVE "082"                 TO WORK-ERRLOG-FIELD
             MOVE 8                     TO SUB
             MOVE WORK-FIELD-DESC(SUB)  TO WORK-ERRLOG-DESC
             MOVE WROR-NON-FILE-REFS(2)  TO WORK-ERRLOG-ORG-N2
             MOVE WRCH-NON-FILE-REFS(2)  TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF WROR-ROAD-GARD-REFS(1) NOT =
             WRCH-ROAD-GARD-REFS(1)
             MOVE "09 "                 TO WORK-ERRLOG-FIELD
             MOVE 9                     TO SUB
             MOVE WORK-FIELD-DESC(SUB)  TO WORK-ERRLOG-DESC
             MOVE WROR-ROAD-GARD-REFS(1)  TO WORK-ERRLOG-ORG-N2
             MOVE WRCH-ROAD-GARD-REFS(1)  TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.
           IF WROR-ROAD-GARD-REFS(2) NOT =
             WRCH-ROAD-GARD-REFS(2)
             MOVE "092"                 TO WORK-ERRLOG-FIELD
             MOVE 9                     TO SUB
             MOVE WORK-FIELD-DESC(SUB)  TO WORK-ERRLOG-DESC
             MOVE WROR-ROAD-GARD-REFS(2)  TO WORK-ERRLOG-ORG-N2
             MOVE WRCH-ROAD-GARD-REFS(2)  TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.


           IF WROR-NF-RECOVERIES(1) NOT =
             WRCH-NF-RECOVERIES(1)
             MOVE "10 "                 TO WORK-ERRLOG-FIELD
             MOVE 10                    TO SUB
             MOVE WORK-FIELD-DESC(SUB)  TO WORK-ERRLOG-DESC
             MOVE WROR-NF-RECOVERIES(1)  TO WORK-ERRLOG-ORG-N2
             MOVE WRCH-NF-RECOVERIES(1)  TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.
           IF WROR-NF-RECOVERIES(2) NOT =
             WRCH-NF-RECOVERIES(2)
             MOVE "101"                 TO WORK-ERRLOG-FIELD
             MOVE 10                    TO SUB
             MOVE WORK-FIELD-DESC(SUB)  TO WORK-ERRLOG-DESC
             MOVE WROR-NF-RECOVERIES(2)  TO WORK-ERRLOG-ORG-N2
             MOVE WRCH-NF-RECOVERIES(2)  TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.


           IF WROR-LOTS-NF-RECOVERIES(1) NOT =
             WRCH-LOTS-NF-RECOVERIES(1)
             MOVE "11 "                 TO WORK-ERRLOG-FIELD
             MOVE 11                    TO SUB
             MOVE WORK-FIELD-DESC(SUB)  TO WORK-ERRLOG-DESC
             MOVE WROR-LOTS-NF-RECOVERIES(1)  TO WORK-ERRLOG-ORG-N2
             MOVE WRCH-LOTS-NF-RECOVERIES(1)  TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.
           IF WROR-LOTS-NF-RECOVERIES(2) NOT =
             WRCH-LOTS-NF-RECOVERIES(2)
             MOVE "111"                 TO WORK-ERRLOG-FIELD
             MOVE 11                    TO SUB
             MOVE WORK-FIELD-DESC(SUB)  TO WORK-ERRLOG-DESC
             MOVE WROR-LOTS-NF-RECOVERIES(2)  TO WORK-ERRLOG-ORG-N2
             MOVE WRCH-LOTS-NF-RECOVERIES(2)  TO WORK-ERRLOG-CHG-N2
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

           MOVE WR-LIFE-REFS(1)         TO F01.
           MOVE WR-LIFE-REFS(2)         TO F011.

           MOVE WR-AH-REFS(1)           TO F02.
           MOVE WR-AH-REFS(2)           TO F021.

           MOVE WR-FIRE-REFS(1)         TO F03.
           MOVE WR-FIRE-REFS(2)         TO F031.

           MOVE WR-AUTO-REFS(1)         TO F04.
           MOVE WR-AUTO-REFS(2)         TO F041.

           MOVE WR-UNEMP-REFS(1)        TO F05.
           MOVE WR-UNEMP-REFS(2)        TO F051.

           MOVE WR-AD-D-REFS(1)         TO F06.
           MOVE WR-AD-D-REFS(2)         TO F061.

           MOVE WR-L-PROT-REFS(1)       TO F07.
           MOVE WR-L-PROT-REFS(2)       TO F071.

           MOVE WR-NON-FILE-REFS(1)     TO F08.
           MOVE WR-NON-FILE-REFS(2)     TO F081.

           MOVE WR-ROAD-GARD-REFS(1)    TO F09.
           MOVE WR-ROAD-GARD-REFS(2)    TO F091.

           MOVE WR-NF-RECOVERIES(1)     TO F10.
           MOVE WR-NF-RECOVERIES(2)     TO F101.

           MOVE WR-LOTS-NF-RECOVERIES(1) TO F11.
           MOVE WR-LOTS-NF-RECOVERIES(2) TO F111.

       DISPLAY-FIELDS-EXIT.
           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *********************************************
       FORM-RESET SECTION.
      *********************************************
           MOVE EXT-CONAME-CONAME TO WRMAN9-CONAME.
           MOVE EXT-CONAME-SYSDATE TO WRMAN9-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY WRMAN9-SCREEN UPON WRMAIN-WINDOW-HANDLE.
           MOVE WRMAN9-FORM-FIELDS TO WR-BUF.
           MOVE WRMAN9-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *********************************************
       SETUP-LINK-INFO SECTION.
      *********************************************
           MOVE WRMAN9-HELP--FILE TO HELP-LINK-FILE.
           MOVE WRMAN9-HELP--DIR TO HELP-LINK-DIR.

      *********************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/WRMAN9_EVA.CPY".
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
           DESTROY WRMAN9-SCREEN.
           MOVE WRMAIN-LINK TO FORM-LINK-AREA.
           EXIT PROGRAM.
