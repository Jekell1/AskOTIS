       IDENTIFICATION  DIVISION.
       PROGRAM-ID.       TRWMAN.
       DATE-WRITTEN. 02/17/2000.
      ******************************************************************
      *                 (MN)
      *
      *  DESCRIPTION:   TRANSACTION FILE MAINTENANCE
      *                   CASH BALANCING RECORD - OPEN/CLOSE CASH DRAWER
      *
      *   NOTE: "W" TRANSACTION RECORDS ARE FOR RECORDING ONLY
      *             NOTHING HERE GET UPDATED TO GL.
      *             -------
      *
      * REV:
      *   MJD 000222 NEW PROGRAM FOR "W" TR RECORDS (WORLD PR#219)
      *  MJD 131121 A30 CONVERSION COMPLETED
      *  CS 161007 CHANGED HOLD-TRW-KEY TO HOLD-TRW-KEY,
      *            HOLD-FIELDS TO HOLD-TW
      *            CHANGED HARD CODE SIZE OF
      *             ORIGINAL-TR-REC/CHANGED-TR-REC
      *
      * KEC 2016.1026 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         REMOVED USING HOLD-TR1-KEY.
      *         ADDED LIBLP/LPWSTR (NEW) FOR LPWSTR-TR1-KEY (HOLD/SAVE).
      *         ADDED LIBLP/LPFRTR; REMOVED LIBLP/LPTRTMPW.
      *         ADDED LPFRTR-UPDATE-CHGLOG; REMOVED WRITE-TR-CHANGE-LOG.
      *
      * BAH 2016.1103 CHANGE TILLNO TO 9(2)
      * BAH 2019.0508 DO NOT ENTER BRANCH, FORCE BRANCH YOU ARE IN. QA0017
      * BAH 20190726 DISPLAY LPWSTR-TW-TILLNO AFTE SCANNING QA0056
      * JKC 2020-0616 ADDED TSTDAT CHECK ON LPWSTRW-TW-DATE.
      * BAH 2025.0124 VALIDATE MONTH ENTERED S35Q-159
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

      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)MN/TRWMAN.CBL S35 05/19/23-10:08:47 >".

       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01TRW.CPY".
       COPY "LIBLP/LP01TRW_SQL.CPY".
       COPY "LIBLP/LPWSTRW.CPY".

       01  ERRCD                       PIC X.

       01  SUB             PIC 999     VALUE 0.
       01  ONE             PIC S9.
       01  ELE             PIC 9(2).
       01  ACTION.
           03  CHKEY       PIC X(4).
           03  CHNO        PIC 9(2).
       01  IO-TYPE         PIC X(3).
       01  NEW-FLAG        PIC 9.

       01  SAVE-BRNO       PIC 9(4).
       01  SAVE-MMYY       PIC 9(4).
       01  SAVE-MMYYX REDEFINES SAVE-MMYY.
           03  SAVE-MM     PIC 99.
           03  SAVE-YY     PIC 99.

      *-----------------------------------------------------------------
      * TIME INPUT WORKERS
      *-----------------------------------------------------------------
       01  HOLD-TIME-WORKER PIC X(05).
       01  HOLD-TIME1 REDEFINES HOLD-TIME-WORKER.
           03  HOLD-HOUR1  PIC 9(02).
           03  HOLD-COLON  PIC X(01).
           03  HOLD-MIN1   PIC 9(02).
       01  HOLD-TIME2 REDEFINES HOLD-TIME-WORKER.
           03  HOLD-HOUR2  PIC 9(02).
           03  HOLD-MIN2   PIC 9(02).
           03  FILLER      PIC X(01).
      *-----------------------------------------------------------------
      * TIME OUTPUT WORKERS
      *-----------------------------------------------------------------
       01  VALIDATE-ERR    PIC X(01)  VALUE SPACE.
       01  HOLD-TIME4      PIC 9(4).
       01  HOLD-TIMEA REDEFINES HOLD-TIME4.
           03  HOLD-HOUR4      PIC 9(02).
           03  HOLD-MIN4       PIC 9(02).
       01  DISP-TIME.
           03  DISP-HOUR   PIC 9(02).
           03  DISP-COLON  PIC X(01).
           03  DISP-MIN    PIC 9(02).

       01  ORIGINAL-TRW-REC    PIC X(TRW-SIZE).
       01  CHANGED-TRW-REC     PIC X(TRW-SIZE).

      ******************************************************************
      *    *     SCREEN DISPLAY BUFFER      *
      ******************************************************************
       01  DISPLAY-BUF     VALUE SPACES.
           03  DISPLAY-TABLE OCCURS 20.
               05  F01         PIC 99/99        BLANK ZERO.
               05  F02         PIC X(08).
               05  F03         PIC Z(06)9.99-   BLANK ZERO.
               05  F04         PIC Z(04)9.99+   BLANK ZERO.
       01  DISPLAY-LIST.
           03  FILLER      PIC X(32) VALUE
               "010 011 012 013 020 021 022 023 ".
           03  FILLER      PIC X(32) VALUE
               "030 031 032 033 040 041 042 043 ".
           03  FILLER      PIC X(32) VALUE
               "050 051 052 053 060 061 062 063 ".
           03  FILLER      PIC X(32) VALUE
               "070 071 072 073 080 081 082 083 ".
           03  FILLER      PIC X(32) VALUE
               "090 091 092 093 100 101 102 103 ".
           03  FILLER      PIC X(32) VALUE
               "110 111 112 113 120 121 122 123 ".
           03  FILLER      PIC X(32) VALUE
               "130 131 132 133 140 141 142 143 ".
           03  FILLER      PIC X(32) VALUE
               "150 151 152 153 160 161 162 163 ".
           03  FILLER      PIC X(32) VALUE
               "170 171 172 173 180 181 182 183 ".
           03  FILLER      PIC X(32) VALUE
               "190 191 192 193 200 201 202 203.".

      ******************************************************************
      *    *   ELEMENT SPECIFICATIONS
      ******************************************************************
       78  MAX                     VALUE 20.
       01  SPEC-TABLE.
      * 1. TW-TIME(1) THRU TW-TIME(20)
           03  FILLER      PIC X(8) VALUE "E  A050 ".
           03  FILLER      PIC X(8) VALUE "E  A050 ".
           03  FILLER      PIC X(8) VALUE "E  A050 ".
           03  FILLER      PIC X(8) VALUE "E  A050 ".
           03  FILLER      PIC X(8) VALUE "E  A050 ".
           03  FILLER      PIC X(8) VALUE "E  A050 ".
           03  FILLER      PIC X(8) VALUE "E  A050 ".
           03  FILLER      PIC X(8) VALUE "E  A050 ".
           03  FILLER      PIC X(8) VALUE "E  A050 ".
           03  FILLER      PIC X(8) VALUE "E  A050 ".
           03  FILLER      PIC X(8) VALUE "E  A050 ".
           03  FILLER      PIC X(8) VALUE "E  A050 ".
           03  FILLER      PIC X(8) VALUE "E  A050 ".
           03  FILLER      PIC X(8) VALUE "E  A050 ".
           03  FILLER      PIC X(8) VALUE "E  A050 ".
           03  FILLER      PIC X(8) VALUE "E  A050 ".
           03  FILLER      PIC X(8) VALUE "E  A050 ".
           03  FILLER      PIC X(8) VALUE "E  A050 ".
           03  FILLER      PIC X(8) VALUE "E  A050 ".
           03  FILLER      PIC X(8) VALUE "E  A050 ".
       01  SPEC-TAB        REDEFINES SPEC-TABLE.
           03  SPEC-REC    OCCURS MAX TIMES.
               05  SPEC    PIC X(7).
               05  CHFG    PIC X.
      *
       01  KEY-BUF.
           03  D-BRANCH    PIC ZZZ9.
           03  D-MMYY.
               05  D-MM    PIC 99.
               05  D-YY    PIC 99.
           03  D-DAY       PIC 99.
           03  D-BRNO      PIC ZZZ9.
           03  D-CODE      PIC X.
           03  D-TILLNO    PIC Z9.
       01  KEY-LIST.
           03  FILLER      PIC X(33) VALUE
               "BRANCH MMYY DAY BRNO CODE TILLNO.".

      ******************************************************************
       COPY "LIBWI/TRWSCNW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       01 TRWMAN-WINDOW-HANDLE PIC X(10) VALUE SPACES.
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/TRWMAN_DEF.CPY".
       COPY "LIBMN/TRWMAN_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/ACCESSW.CPY".

       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBMN/TRWMAN_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

      ******************************************************************
      *
      *     M A I N    P R O G R A M    M O D U L E
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".
           MOVE "TRWMAN" TO VDU-FORM-NAME.
           MOVE FORM-PATHNAME TO FORM-PATH.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS TRWMAN-WINDOW-HANDLE.

            PERFORM FORM-RESET.

       MAIN-MODULE.
      D    STOP MESS.

       ENTER-BRANCH.

      * FORCE BRANCH YOU ARE IN
           MOVE "SNNN040BRANCH." TO VDU.
           MOVE EXT-FILPATH-BRANCH TO VDUNUM0 BR-NO SAVE-BRNO.
           PERFORM SCREENIO.
      * FORCE BRANCH YOU ARE IN

           PERFORM LOAD-BRANCH-MODULE.
           IF ( BR-PATH = SPACES )
              MOVE "INVALID BRANCH" TO MESS
              PERFORM SEND-MESS
              GO TO END-ROUTINE.

       ENTER-MONTH.
           MOVE "ENNN040MMYY." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>"
              GO TO END-ROUTINE.

           IF VDUSTATUS NOT = "00"
              GO TO ENTER-MONTH.

           MOVE VDUNUM0 TO SAVE-MMYY.
           IF SAVE-MM < 1 OR > 12
              MOVE "INVALID MMYY, PLEASE RE-ENTER" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-MONTH.

           PERFORM OPEN-TRW1-FILE.

       RESTART-MAINT.
           PERFORM FORM-RESET.

           PERFORM ENTER-KEYS-MODULE.
           IF IO-TYPE = "END"
              GO TO END-ROUTINE.
           IF IO-TYPE = "CLR"
             GO TO MAIN-MODULE.

           PERFORM ENTRY-MODULE.
           IF IO-TYPE = "RWR"
              PERFORM WRITE-MODULE.
           IF IO-TYPE = "DEL"
             PERFORM DELETE-MODULE.

           GO TO RESTART-MAINT.

      ******************************************************************
       ENTER-KEYS-MODULE SECTION.
      ******************************************************************
       ENTER-DAY.
           MOVE "F6 - SCAN"   TO MESS.
           PERFORM CALL-TMMSG.
           MOVE SPACES        TO IO-TYPE.

           MOVE 0 TO LPWSTRW-TW-BRNO LPWSTRW-TW-DATE LPWSTRW-TW-TILLNO.
           MOVE 0 TO NEW-FLAG.

           MOVE "ENNN020DAY." TO VDU.
           PERFORM SCREENIO.

           MOVE SPACES        TO MESS.
           PERFORM CALL-TMMSG.

           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTER-KEYS-MODULE.

           IF VDUSTATUS = "1U"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTER-KEYS-MODULE.

           IF VDUSTATUS = "1<"
              MOVE SAVE-BRNO   TO TRWSCN-BRNO
              MOVE SAVE-MMYY   TO TRWSCNW-MMYY
              MOVE "Y"         TO TRWSCN-RETURN
              MOVE "WI/TRWSCN " TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH TRWSCN-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              IF TRWSCN-STATUS NOT = "00"
                 GO TO ENTER-DAY
              ELSE
                 MOVE TRWSCN-TRW1-KEY TO LPWSTRW-TRW1-KEY
                 PERFORM DISPLAY-KEY
                 GO TO READ-TRANS-FILE.

           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-ENTER-KEYS-MODULE.

           IF VDUSTATUS NOT = "00"
              GO TO ENTER-DAY.

           MOVE VDUNUM0 TO DATE-MMDDYY-DD.
           MOVE SAVE-MM TO DATE-MMDDYY-MM.
           MOVE SAVE-YY TO DATE-MMDDYY-YY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO NUM-DATE.
           PERFORM TSTDAT.

           IF ( NUM-DATE = ZEROES )
              GO TO ENTER-DAY.

           MOVE NUM-DATE TO LPWSTRW-TW-DATE.

      *----------------------------------------------------------------
       ENTER-BRNO.

      * FORCE BRANCH YOU ARE IN
           MOVE "SNNN040BRNO." TO VDU.
           MOVE EXT-FILPATH-BRANCH TO VDUNUM0 BR-NO LPWSTRW-TW-BRNO
                                      TW-BRNO.
           PERFORM SCREENIO.
      * FORCE BRANCH YOU ARE IN

      *    MOVE "ENNN040BRNO." TO VDU.
      *    PERFORM SCREENIO.
      *    IF VDUSTATUS = "10"
      *       MOVE "CLR" TO IO-TYPE
      *       GO TO END-ENTER-KEYS-MODULE.
      *    IF VDUSTATUS = "1U"
      *       GO TO ENTER-DAY.
      *    IF VDUSTATUS = "1>"
      *       MOVE "END" TO IO-TYPE
      *       GO TO END-ENTER-KEYS-MODULE.
      *    IF VDUSTATUS NOT = "00"
      *       GO TO ENTER-BRNO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    DO *NOT* SET FILPATH-WORKERS FROM THIS READ-BR-FILE.
      *    THIS BRNO MAYBE DIFFERENT FROM THE ONE USED FOR THE
      *    PREVIOUS READ-BR-FILE USED TO GET TRANS-PATH.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    MOVE VDUNUM0 TO BR-NO LPWSTRW-TW-BRNO TW-BRNO.

           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF IO-FG NOT = 0
              MOVE "INVALID BRANCH" TO MESS
              PERFORM SEND-MESS
              MOVE "END" TO IO-TYPE
              GO TO END-ENTER-KEYS-MODULE.

           MOVE "S NA200BRDESC." TO VDU.
           MOVE BR-NAME TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S NA010CODE." TO VDU.
           MOVE "W"            TO VDUBUF.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
       ENTER-TILLNO.
           MOVE "E  N020TILLNO." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTER-KEYS-MODULE.

           IF VDUSTATUS = "1U"
              GO TO ENTER-DAY.

           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-ENTER-KEYS-MODULE.

           IF VDUSTATUS NOT = "00"
              GO TO ENTER-TILLNO.

           MOVE VDUNUM0 TO LPWSTRW-TW-TILLNO TW-TILLNO.

      *----------------------------------------------------------------
       READ-TRANS-FILE.
           PERFORM OPEN-TRW1-FILE.
           MOVE LPWSTRW-TRW1-KEY TO TRW1-KEY.
           PERFORM READ-TRW1-FILE.
           IF IO-FG = 0
              MOVE TRW-REC TO ORIGINAL-TRW-REC
              PERFORM DISPLAY-FIELDS
           ELSE
              PERFORM NEW-RECORD-SETUP.

       END-ENTER-KEYS-MODULE.
           EXIT.

      ******************************************************************
      *    E N T R Y    M O D U L E
      *
      *        NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *                   1 - INITIAL NEW RECORD ENTRY
      *                   2 - MODIFY NEW RECORD
      ******************************************************************
       ENTRY-MODULE SECTION.
           MOVE SPACES TO IO-TYPE.
           MOVE 1 TO ONE.

       CREATE-ROUTINE.
           IF NEW-FLAG NOT = 1
              GO TO MODIFY-ROUTINE.

           ADD ONE TO ELE.

           IF ELE < 1
              MOVE 1 TO ELE ONE.

           IF ELE NOT > MAX
              GO TO ENTER-ELE.

           MOVE 2 TO NEW-FLAG.

      *----------------------------------------------------------------
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
              GO TO END-ENTRY-MODULE.

           IF ACTION = "DELETE"
              MOVE "DEL" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

           INSPECT ACTION REPLACING ALL SPACES BY ZEROS.

           IF CHNO NOT NUMERIC
              GO TO MODIFY-ROUTINE.

           IF CHNO = 0
              GO TO MODIFY-ROUTINE.

           IF CHNO > MAX
              GO TO MODIFY-ROUTINE.

           MOVE CHNO TO ELE.

      *----------------------------------------------------------------
       ENTER-ELE.
           IF CHKEY NOT = ZEROS
              IF CHFG(ELE) = " "
                 IF NEW-FLAG NOT = 1
                    GO TO MODIFY-ROUTINE.

           IF NEW-FLAG = 0
              IF CHFG(ELE) = "N"
                 IF CHKEY NOT = EXT-CONAME-DAILY-PASSWORD
                    GO TO MODIFY-ROUTINE.

           MOVE SPEC(ELE)  TO VDU.
           MOVE ELE        TO VDUROW.
           MOVE 0          TO VDUCOL.
           MOVE "."        TO VDUDECIMAL.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
       TEST-F4.
           IF VDUSTATUS = "1U"
              IF NEW-FLAG = 1
                 MOVE -1 TO ONE
                 GO TO CREATE-ROUTINE.

      *----------------------------------------------------------------
       TEST-F5.
           IF VDUSTATUS = "1D"
              MOVE 1 TO ONE
              GO TO GOTO-FIELD.

      *----------------------------------------------------------------
       TEST-F1.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

           IF VDUSTATUS NOT = "00"
              GO TO ENTER-ELE.

           MOVE 1 TO ONE.

      *----------------------------------------------------------------
       GOTO-FIELD.
           GO TO F-01 F-01 F-01 F-01 F-01 F-01 F-01 F-01 F-01 F-01
                 F-01 F-01 F-01 F-01 F-01 F-01 F-01 F-01 F-01 F-01
                 DEPENDING ON ELE.
           GO TO END-ENTRY-MODULE.

      *-----------------------------------------------------------------
      *           SET RECORD ELEMENTS
      *-----------------------------------------------------------------
       F-01.
           MOVE VDUBUF TO HOLD-TIME-WORKER.
           PERFORM VALIDATE-TIME.
           IF VALIDATE-ERR NOT = SPACE
              GO TO ENTER-ELE.

           MOVE HOLD-TIME4 TO TW-TIME(ELE)

           MOVE SPEC(ELE)  TO VDU.
           MOVE "S"        TO VDU(1:1).
           MOVE ELE        TO VDUROW.
           MOVE 0          TO VDUCOL.
           MOVE "."        TO VDUDECIMAL.
           MOVE DISP-TIME  TO VDUBUF.
           PERFORM SCREENIO.

       F-011.
           MOVE "E  A080  1." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF UP-KEY GO TO ENTER-ELE.
           IF NOT (ENTER-KEY OR DOWN-KEY) GO TO F-011.
           MOVE VDUBUF TO TW-POSTNAME(ELE).

       F-012.
           MOVE "ENNS072  2." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF UP-KEY GO TO F-011.
           IF NOT (ENTER-KEY OR DOWN-KEY) GO TO F-012.
           MOVE VDUNUM2 TO TW-AMOUNT(ELE).

       F-013.
           MOVE "ENNS052  3." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF UP-KEY GO TO F-012.
           IF NOT (ENTER-KEY OR DOWN-KEY) GO TO F-013.
           MOVE VDUNUM2 TO TW-OVERSHORT(ELE).

           GO TO CREATE-ROUTINE.

       END-ENTRY-MODULE.
           EXIT.

      ******************************************************************
       WRITE-MODULE SECTION.
      ******************************************************************
           PERFORM OPEN-TRW1-FILE.

           IF NEW-FLAG NOT = 0
              GO TO WRITE-RECORD.

           MOVE TRW-REC TO CHANGED-TRW-REC.

           IF ORIGINAL-TRW-REC = CHANGED-TRW-REC
              GO TO END-WRITE-MODULE.

           PERFORM READ-TRW1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-WRITE-MODULE.

           IF TRW-REC NOT = ORIGINAL-TRW-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-WRITE-MODULE.

           MOVE CHANGED-TRW-REC TO TRW-REC.

           PERFORM REWRITE-TRW1-FILE.
           GO TO END-WRITE-MODULE.

       WRITE-RECORD.
           PERFORM WRITE-TRW1-FILE.
           IF IO-FG NOT = 0
              PERFORM ALREADY-CREATED.

       END-WRITE-MODULE.
           PERFORM CLOSE-TRW1-FILE.

      ******************************************************************
       DELETE-MODULE SECTION.
      ******************************************************************
           IF NEW-FLAG NOT = 0
              GO TO END-DELETE-MODULE.

           PERFORM OPEN-TRW1-FILE.
           PERFORM READ-TRW1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-DELETE-MODULE.

           IF TRW-REC NOT = ORIGINAL-TRW-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-CHANGED
              GO TO END-DELETE-MODULE.

           PERFORM DELETE-TRW1-FILE.

       END-DELETE-MODULE.
           PERFORM CLOSE-TRW1-FILE.

      ******************************************************************
       NEW-RECORD-SETUP SECTION.
      ******************************************************************
           MOVE 1                    TO NEW-FLAG.
           MOVE "S  A000NEW."        TO VDU.
           MOVE "*** NEW RECORD ***" TO VDUBUF.
           PERFORM SCREENIO.

           PERFORM CLEAR-TRW-FILE.
           MOVE LPWSTRW-TRW1-KEY TO TRW1-KEY.
           MOVE 0         TO ELE.

      ******************************************************************
       DISPLAY-FIELDS SECTION.
      ******************************************************************
           MOVE SPACES TO DISPLAY-BUF.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 20
              MOVE TW-TIME(SUB)         TO F01(SUB)
              INSPECT F01(SUB) REPLACING ALL "/" BY ":"
              MOVE TW-POSTNAME(SUB)     TO F02(SUB)
              MOVE TW-AMOUNT(SUB)       TO F03(SUB)
              MOVE TW-OVERSHORT(SUB)    TO F04(SUB)
           END-PERFORM.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       DISPLAY-FIELDS-EXIT.
           EXIT.

      ******************************************************************
       DISPLAY-KEY SECTION.
      ******************************************************************

           MOVE SAVE-BRNO        TO D-BRANCH.

           MOVE LPWSTRW-TW-DATE     TO DATE-YYYYMMDD.
           MOVE DATE-YYYYMMDD-MM    TO D-MM.
           MOVE DATE-YYYYMMDD-YY    TO D-YY.

           MOVE DATE-YYYYMMDD-DD    TO D-DAY.
           MOVE LPWSTRW-TW-BRNO     TO D-BRNO.
           MOVE "W"                 TO D-CODE.
           MOVE LPWSTRW-TW-TILLNO   TO D-TILLNO.

           MOVE KEY-BUF TO WR-BUF.
           MOVE KEY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *****************************************
       VALIDATE-TIME SECTION.
      *****************************************
           MOVE SPACE TO VALIDATE-ERR.

      * A VALID TIME MUST BE ENTERRED
           IF VDUBUF = SPACES
              MOVE ZEROS TO HOLD-TIME4
              GO TO VALIDATE-TIME-DISP.

      * TIME MAY BE ENTERRED AS "HHMM " OR HH:MM" (24 HOUR CLOCK)
           IF HOLD-COLON = ":"
              MOVE HOLD-HOUR1 TO HOLD-HOUR4
              MOVE HOLD-MIN1 TO HOLD-MIN4
           ELSE
              MOVE HOLD-HOUR2 TO HOLD-HOUR4
              MOVE HOLD-MIN2 TO HOLD-MIN4.

      * TEST THAT TIME IS NUMERIC
           IF HOLD-TIME4 NOT NUMERIC
              MOVE "PLEASE ENTER TIME IN HHMM FORMAT" TO MESS
              PERFORM SEND-MESS
              GO TO VALIDATE-TIME-ENDIT.

      * TEST VALID HOUR MINUTE COMBINATIONS
           IF HOLD-HOUR4 > 23 OR HOLD-MIN4 > 59
              MOVE "PLEASE ENTER TIME IN HHMM OR HH:MM FORMAT" TO MESS
              PERFORM SEND-MESS
              GO TO VALIDATE-TIME-ENDIT.

      * SETUP OUTPUT/DISPLAY FIELDS (VALID TIME IS IN HOLD-TIME4)
       VALIDATE-TIME-DISP.
           MOVE HOLD-HOUR4 TO DISP-HOUR.
           MOVE ":"        TO DISP-COLON.
           MOVE HOLD-MIN4  TO DISP-MIN.

            GO TO VALIDATE-TIME-EXIT.
       VALIDATE-TIME-ENDIT.
           MOVE "X" TO VALIDATE-ERR.
       VALIDATE-TIME-EXIT.
           EXIT.

      **********************************************
       FORM-RESET SECTION.
      **********************************************
           MOVE EXT-CONAME-CONAME TO TRWMAN-CONAME.
           MOVE EXT-CONAME-SYSDATE TO TRWMAN-SYSDATE.
           DISPLAY SPACES UPON TRWMAN-WINDOW-HANDLE
                               LINE 0 COL 0 ERASE SCREEN.
           DISPLAY TRWMAN-SCREEN UPON TRWMAN-WINDOW-HANDLE.
           IF SAVE-BRNO NOT = 0
              MOVE SAVE-BRNO TO TRWMAN-BRANCH
              MOVE SAVE-MMYY TO TRWMAN-MMYY.

           MOVE TRWMAN-FORM-FIELDS TO WR-BUF.
           MOVE TRWMAN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **********************************************
       SETUP-LINK-INFO SECTION.
      **********************************************
           MOVE TRWMAN-HELP--FILE TO HELP-LINK-FILE.
           MOVE TRWMAN-HELP--DIR TO HELP-LINK-DIR.

      **********************************************
       VDU-CONVERT-FIELD SECTION.
      **********************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/TRWMAN_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       LOAD-BRANCH-MODULE SECTION.
      ******************************************************************
     
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF ( IO-BAD )
              MOVE SPACES                  TO BR-PATH
              GO TO LOAD-BRANCH-MODULE-EXIT.
     
           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.
     
           MOVE "B"                        TO TRW-PATH-OVERRIDE.
     
       LOAD-BRANCH-MODULE-EXIT.
           EXIT.
     

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBLP/LPTRWCL.CPY".

      ******************************************************************
       MISC-PARAGRAPHS SECTION.
      ******************************************************************
       COPY "LIBGB/ACCESS.CPY".
      *----------------------------------------------------------------
       RECORD-DELETED.
           MOVE " RECORD WAS DELETED!" TO MESS.
           PERFORM SEND-MESS.

      *----------------------------------------------------------------
       RECORD-CHANGED.
           MOVE " CHANGES WERE MADE TO THIS RECORD!" TO MESS.
           PERFORM SEND-MESS.

      *----------------------------------------------------------------
       CHANGE-AGAIN.
           MOVE " PLEASE RE-INPUT YOUR CHANGES!" TO MESS.
           PERFORM SEND-MESS.

      *----------------------------------------------------------------
       ALREADY-CREATED.
           MOVE " RECORD HAS BEEN ALREADY CREATED!" TO MESS.
           PERFORM SEND-MESS.

      *----------------------------------------------------------------
       UNDERGONE-CHANGE.
           MOVE
           " THIS RECORD HAS UNDERGONE CHANGE BY ANOTHER USER,"
           TO MESS.
           PERFORM SEND-MESS.

      *----------------------------------------------------------------
       ACTIVE-RECORD.
           MOVE " ACTIVE CUSTOMER CANNOT BE DELETED" TO MESS.
           PERFORM SEND-MESS.

      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-TRW1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPTRWGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBLP/LPTRW1IN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      ******************************************************************
       END-ROUTINE SECTION.
      ******************************************************************
       END-RUN.
           PERFORM CLOSE-FILES.

       EXIT-PROG.
       STOP-RUN.
           DESTROY TRWMAN-WINDOW-HANDLE.
           MOVE FORM-PATH TO FORM-PATHNAME.
           EXIT PROGRAM.
