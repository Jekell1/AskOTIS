       IDENTIFICATION DIVISION.
       PROGRAM-ID.      EMCKR2.
       DATE-WRITTEN. 01-15-88
      ******************************************************************
      *                      (LP)
      *    DESCRIPTION:  END OF MONTH SUMMARYCHECK REGISTER BY BRANCH
      *
      *        PRINTS ALL TRC TYPES EXCEPT;
      *               "CA", "BK", "RR", "RC" "RP" AND "RD"
      *               "ID", "IW", "DB"
      *                   *** 01/06/2007***
      * *** THIS VERSION PRINTS 1 (ONE) LINE PER BRANCH TOTAL OF    ***
      * *** CHECKS WRITTEN. THERE ARE NO GROUP TOTALS, NO GL SUMMARY***
      *
      * REV:
      *  BAH 081795 IF LOAN OR SALES CHECK, ALWAYS PRINT ACCTNO IN REF
      *  KEC 091495 ADDED LOCAL-SPOOLING LOGIC.
      *  KEC 091595 CHANGE MILITARY TIME TO AM/PM ON HEADER.
      *  BAH 092895 RECORDS-FOUND INITIALIZED TO "N" INSTEAD OF " "
      *  KEC 101295 COMMENTED OUT DEFAULT BRANCH SETTING IN INITIZATION.
      *  KEC 101695 MADE CHANGE TO HEADER TO PRINT VALID BRANCH NAME.
      *  JFF 111095 CHANGED FORM TO INCLUDE LOCAL-SPOOLING QUESTION.
      *  KEC 113095 FIXED LOCAL-SPOOL FOR IF A BRANCH HAS NO DATA.
      *  KEC 113095 FIXED LOCAL-SPOOL TO NOT PRINT THE RANGE-LINES.
      *  JFF 121395 CHANGED "IF LOCAL-PRINT-FG = N" TO
      *             "IF LOCAL-PRINT-FG NOT = Y" IN INITIALIZATION.
      *  KEC 121395 ADDED LOGIC TO PRINT ERROR MESSAGE IF ERROR ON
      *             READING THE GI-FILE.
      *  BLV 060597 CHANGED FOR LOCAL SPOOL WITH NEW BATCH DRIVER - BREXEC
      *  GLF 090297 REMOVED ALL USE OF GLOBAL
      *  GLF 022098 DELETED BR-SECURITY-OK AND ADDED LIBGB/BRSECUREW.
      *  GLF 022798 DELETED "END OF MONTH" FROM TITLE
      *  BLV 031598 ADDED GRAND TOTAL OF CHECKS (WORLD)
      *  MJD 990115 ADDED NOTES TRFILE AND CHECK TO PROGRAM
      *  MJD 990401 ADDED ACCESS CALL FOR NI FILE.
      *  GLB 010702 TEST TO SKIP NEW TC-TYPE "RD" RECEIPT PROCEEDS
      *  BLV 020605 FIX TO GET CORRECT GL # FROM GI FOR NEW TYPE "AC",
      *             DEALER ACH DIRECT DEPOSIT, REGACC PR# 1797
      *  BAH 021008 FIXED RECORDS-FOUND TO NOT GIVE "NO RECORDS FOUND"
      *             ON A F7 EXIT
      *  BAH 040301 ADDED MISC CHECKS SKIP, INCLUDE OR ONLY FOR WORLD
      *             PR# 343, WILL NOT AFFECT BATCHES !!
      *  BAH 040903 REPLACE BR-NAME IN HEADING WITH BR-REGION-NUMBER
      *             FOR GP-I-AM-REGACC, REGACC PR# 77
      *   CS 070109 ****THIS VERSION PRINTS ONLY BRANCH TOTALS******
      *                 AND NO GL SUMMARY   PR#516
      *   CS 120320 ADD "ID" INTERIM DEPOSIT AND "IW" INTERIM WITHDRAWL TO
      *             TC-TYPES TO SKIP  WORLD PR#790
      *  BAH 170526 ACTIVATE 8 DIGIT DATES, PD0006
      *  BAH 181023 REMOVED NOTES
      *  BAH 20190924 REMOVED ACCESS-CALL TRCFILE
      *  BAH 2021.0318 SKIP "DB" DEBIT UMC #1490
      *  MB  20240624  PERFORMANCE TUNING, S35 STANDARDS  S35-71
      *                COMMENTED OUT USE OF TEMP WORK FILE
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
      *     SELECT WK2-FILE ASSIGN WK2-PATH
      *            ORGANIZATION INDEXED
      *            ACCESS DYNAMIC
      *            LOCK MODE AUTOMATIC WITH LOCK ON RECORD
      *            RECORD KEY WK2-KEY
      *            FILE STATUS FILE-STAT.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".

      * FD  WK2-FILE
      *     LABEL RECORDS ARE STANDARD.
      * 01  WK2-REC.
      *     03  WK2-KEY.
      *         05  WK2-BRANCH          PIC 9(4).
      *         05  WK2-CKNO            PIC 9(6).
      *         05  WK2-DATE            PIC 9(8).
      *         05  WK2-CLASS           PIC 9(3).
      *         05  WK2-SEQ             PIC 9(6).

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LP/EMCKR2.CBL S34 05/26/21-11:05:48 >".
      ****************************
      *   F I L E   P A T H S    *
      ****************************
       COPY "LIBLP/LP01TRC.CPY".
       COPY "LIBLP/LP01TRC_SQL.CPY".
       COPY "LIBLP/LPWSTRC.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  WS-TCAMTSUM                 PIC S9(9)V99.
       EXEC SQL END DECLARE SECTION END-EXEC.

       01  PR-IFN               PIC X(2)    VALUE "PR".
       01  WK2-IFN              PIC X(3)    VALUE "WK2".

      * 01  WK2-PATH             PIC X(35).

      **********************
      *   LINE WORKERS     *
      **********************
       01  LN-WORKERS.
           03  LN-FEED          PIC 99       VALUE 0.
           03  LN-COUNT         PIC 999      VALUE 99.
           03  LN-WK            PIC 999      VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

      **********************
      *   MISC WORKERS     *
      **********************
       01  FIRST-TIME           PIC X               VALUE "Y".
       01  RECORDS-FOUND        PIC X               VALUE " ".
           88  NO-RECORDS-FOUND                     VALUE "N".
       01  MSEL-LIST            PIC X(5)            VALUE "MSEL.".
       01  MESS-LIST            PIC X(5)            VALUE "MESS.".
       01  HOLD-BRNO            PIC 9(4)            VALUE 0.
       01  HOLD-NAME            PIC X(40)           VALUE SPACES.
       01  CK-TOTAL             PIC S9(9)V99 COMP-3 VALUE 0.
       01  GRAND-TOTAL          PIC S9(9)V99 COMP-3 VALUE 0.

      *-----------------------------------------------------------------
       COPY "LIBLP/EMSFRG_CMD.CPY".
      *-----------------------------------------------------------------
       01  LEGEND               PIC X(70).

      ***********************************
      *     S C R E E N   F I E L D S   *
      ***********************************
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  BEG-BRANCH      PIC 9(4).
           03  END-BRANCH      PIC 9(4).
           03  END-DATE.
               05  END-MONTH   PIC 99.
               05  END-YEAR    PIC 99.
           03  EDATE REDEFINES END-DATE PIC 9999.
           03  DAY-OF-MONTH    PIC 99.
           03  GROUP-FLAG      PIC X.
           03  ENT-GROUP       PIC X(4).
           03  LOCAL-PRINT-FG  PIC X(1).
           03  MISC-CHECKS     PIC X.

       01  VERSION-CONTROL     PIC X VALUE "B".
       01  BEGINNING-DATE      PIC 9(8).
       01  ENDING-DATE         PIC 9(8).

       01  SV-BEG-BR           PIC 9999  VALUE 9999.
       01  SV-END-BR           PIC 9999  VALUE 9999.
       01  SV-GROUP-FLAG       PIC X     VALUE SPACES.
       01  SV-ENT-GROUP        PIC X(4)  VALUE SPACES.
       01  SV-LOCAL-PRINT-FG   PIC X     VALUE SPACES.
       COPY "LIBGB/PASSWDW.CPY".

      ***********************************
      *     P R I N T   B U F F E R S   *
      ***********************************
       01  HEAD1.
           03  H-DATE           PIC X(8)     VALUE " ".
           03  FILLER           PIC XX       VALUE " ".
           03  H-TIME           PIC X(8)     VALUE " ".
           03  FILLER           PIC X(28)    VALUE " ".
           03  H-NAME           PIC X(40)    VALUE " ".
           03  FILLER           PIC X(37)    VALUE " ".
           03  FILLER           PIC X(5)     VALUE "PAGE ".
           03  H-PAGE-NO        PIC ZZZ9.

       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(26)    VALUE " ".
           03  FILLER           PIC X(39)    VALUE
           "      - - - CHECK REGISTER - - -       ".
           03  FILLER           PIC X(39)    VALUE  " ".
           03  FILLER           PIC X(8)  VALUE "* = VOID".

       01  HEAD4.
           03  FILLER      PIC X(17) VALUE "BRANCH           ".
           03  FILLER      PIC X(69) VALUE SPACES.
           03  FILLER      PIC X(6)  VALUE "AMOUNT".


      ******************************************************************
      *
      *    D E T A I L - L I N E
      *
      ******************************************************************
       01  DETAIL-LINE          PIC X(132) VALUE SPACES.

      *-----------------------------------------------------------------
       01  T-LINE REDEFINES DETAIL-LINE.
           03  FILLER      PIC X(2).
           03  T-NO        PIC 9(4).
           03  FILLER      PIC X(16).
           03  T-NAME      PIC X(40).
           03  FILLER      PIC X(16).
           03  T-CKTOTAL   PIC ZZZ,ZZZ,ZZZ.99-.

      *-----------------------------------------------------------------
       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(35).
           03  RANGE-SELECTION.
               04  R-NUM        PIC Z(5)9.
               04  FILLER REDEFINES R-NUM.
                   05  FILLER   PIC X(5).
                   05  R-FG     PIC X.

      ******************************************************************
       COPY "LIBWI/PFSWINDOWSW.CPY".
      * COPY "LIBWI/TSTINTW.CPY".
       COPY "LIBWI/GRSCANW.CPY".

       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/DATERW.CPY".
      * COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/EOSPOOLW.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/EMCKR2_DEF.CPY".
       COPY "LIBLP/EMCKR2_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBLP/EMCKR2_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE.    *> WK2-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-TRC1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
      *     CLOSE WK2-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      *************************************
       MAIN-MODULE SECTION.
      *************************************

           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "EMCKR2" TO FORM-PROG VDU-FORM-NAME.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

           MOVE "N" TO RECORDS-FOUND.

           IF GROUP-FLAG = "N"
              MOVE BEG-BRANCH TO BR-NO
              PERFORM START-BR-FILE
              GO TO NEXT-BRANCH-NO.

           MOVE ENT-GROUP TO GR-ID.
           PERFORM SEARCH-GR-INIT.
       NEXT-GROUP-LOC.
           IF GROUP-FLAG NOT = "Y"
              GO TO NEXT-BRANCH-NO.
           PERFORM SEARCH-GR-NEXT.
           IF WGR-SRESULT = 0
              GO TO END-ROUTINE.
           MOVE WGR-SRESULT TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.
           GO TO MOVE-BRANCH-PATH-INFO.
       NEXT-BRANCH-NO.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-FG = 9 OR BR-NO > END-BRANCH
              GO TO END-ROUTINE.

      *-----------------------------------------------------------------
       MOVE-BRANCH-PATH-INFO.
           IF ( LOCAL-PRINT-FG = "Y" )
              PERFORM CLOSE-PR-FILE
              PERFORM CREATE-SPOOL-DIR
              IF ( STAT NOT = "00" )
                 GO TO NEXT-GROUP-LOC.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO TRC-PATH-OVERRIDE.

           PERFORM OPEN-TRC1-FILE.

      *     PERFORM SORT-ROUTINE.

           IF ( EXT-EOBUF-ERRCD = "X" )
              GO TO NEXT-GROUP-LOC.

      *     PERFORM OPEN-WK2-FILE.
      *
      *-----------------------------------------------------------------
       NEXT-CK.
      *     PERFORM READ-WK2-FILE-NEXT.
      *     IF IO-FG = 9
      *        GO TO END-OF-GROUP.
      *
      *     PERFORM TRAP-INTERRUPT.
      * 
      *     MOVE WK2-DATE   TO TC-KEY-DATE.
      *     MOVE WK2-BRANCH TO TC-BRNO.
      *     MOVE WK2-CKNO   TO TC-CKNO.
      *     MOVE WK2-CLASS  TO TC-CLASS.
      *     MOVE WK2-SEQ    TO TC-SEQ.
      *
      *     PERFORM READ-TRC1-FILE.
      *     IF IO-FG NOT = 0
      *        MOVE "TRANSACTION RECORD MISSING SINCE SORT" TO MESS
      *        PERFORM SEND-MESS
      *        GO TO IO-ERROR.

           MOVE BR-NAME        TO HOLD-NAME.
           MOVE BR-NO          TO QTRC1-WBEG-BRNO HOLD-BRNO.
           MOVE BEGINNING-DATE TO TC-KEY-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRC1-WBEG-KEY-DATE.
           MOVE ENDING-DATE         TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRC1-WEND-KEY-DATE.

           MOVE 0 TO WS-TCAMTSUM.

           IF MISC-CHECKS = "S"
              PERFORM SUM-TRC1-FILE-SKIP
           ELSE
              IF MISC-CHECKS = "O"
                 PERFORM SUM-TRC1-FILE-ONLY
              ELSE
                 PERFORM SUM-TRC1-FILE.

           IF WS-TCAMTSUM NOT = 0
              MOVE "Y" TO RECORDS-FOUND
              ADD WS-TCAMTSUM TO CK-TOTAL GRAND-TOTAL
              PERFORM BRANCH-TOTALS.

      *     IF FIRST-TIME = "Y"
      *        MOVE "N" TO FIRST-TIME
      *        MOVE "Y" TO RECORDS-FOUND
      *        MOVE WK2-BRANCH TO HOLD-BRNO
      *        MOVE BR-NAME    TO HOLD-NAME.
      *
      *     IF WK2-BRANCH NOT = HOLD-BRNO
      *        PERFORM BRANCH-TOTALS
      *        MOVE WK2-BRANCH TO HOLD-BRNO
      *        MOVE BR-NAME    TO HOLD-NAME.
      *
      *     PERFORM DETAIL-LINE-PROCESS.

      *     GO TO NEXT-CK.

      *-----------------------------------------------------------------
       END-OF-GROUP.
      *     PERFORM CLOSE-TRC1-FILE.
      *     PERFORM CLOSE-WK2-FILE.
      *     PERFORM OPEN-WK2-FILE-OUTPUT.

           IF ( LOCAL-PRINT-FG = "Y" )
      *        IF ( FIRST-TIME NOT = "Y" )
      *           PERFORM BRANCH-TOTALS
      *           MOVE "Y" TO FIRST-TIME
      *           MOVE 0 TO PAGE-NUMBER
      *        ELSE
                 MOVE 0 TO PAGE-NUMBER.

           GO TO NEXT-GROUP-LOC.

      *-----------------------------------------------------------------
       END-ROUTINE.
           IF NO-RECORDS-FOUND
              GO TO END-PROGRAM.

           IF LOCAL-PRINT-FG = "N"
      *        PERFORM BRANCH-TOTALS
              PERFORM GRAND-TOTALS
              PERFORM PRINT-RANGE-LINES.

           GO TO END-PROGRAM.

      ***********************************
       INITIALIZATION SECTION.
      ***********************************
           MOVE EMCKR2-QUEUE-POS TO Q-POS.
           MOVE EMCKR2-COPIES-POS TO C-POS.
           MOVE EMCKR2-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           MOVE SPACES TO RECORDS-FOUND.

      * CREATE SORT KEY FILE FOR TRFILE OUTPUT
      *     MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
      *     PERFORM MKTEMP-CALL.
      *     IF STAT NOT = "00"
      *        MOVE "CANNOT MAKE SORT KEY FILE" TO MESS
      *        PERFORM SEND-MESS
      *        GO TO ENDIT.
      *     MOVE MKTEMP-BUF TO WK2-PATH.
      *     PERFORM OPEN-WK2-FILE-OUTPUT.

           MOVE EXT-CONAME-CONAME  TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           IF EXT-EOBUF-GROUP-FG = "Y"
              MOVE BEG-BRANCH TO SV-BEG-BR
              MOVE END-BRANCH TO SV-END-BR
              MOVE GROUP-FLAG TO SV-GROUP-FLAG
              MOVE ENT-GROUP TO SV-ENT-GROUP
              MOVE LOCAL-PRINT-FG TO SV-LOCAL-PRINT-FG
              MOVE EXT-EOBUF-BRNO TO BEG-BRANCH END-BRANCH
              MOVE "N" TO GROUP-FLAG LOCAL-PRINT-FG
              MOVE EXT-EOBUF-GROUP-PRPATH TO PRU-FILE
           ELSE
              IF ( BEG-BRANCH = 0 AND END-BRANCH = 0 )
                 MOVE BEG-BRANCH TO SV-BEG-BR
                 MOVE END-BRANCH TO SV-END-BR
                 MOVE EXT-CONAME-USER-LOC TO BEG-BRANCH END-BRANCH.

           IF EXT-ROUTE-BUF = "00"
              MOVE "CHECK REGISTER IN PROGRESS............." TO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

      * SET DATE RANGE FOR TESTING TR FILES
           MOVE END-MONTH TO DATE-MMDDYY-MM.
           MOVE END-YEAR  TO DATE-MMDDYY-YY.
           IF DAY-OF-MONTH = 99
              MOVE 1      TO DATE-MMDDYY-DD
           ELSE
              MOVE DAY-OF-MONTH TO DATE-MMDDYY-DD.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO BEGINNING-DATE.

           MOVE END-MONTH TO DATE-MMDDYY-MM.
           MOVE END-YEAR  TO DATE-MMDDYY-YY.
           IF DAY-OF-MONTH = 99
              MOVE 31      TO DATE-MMDDYY-DD
           ELSE
              MOVE DAY-OF-MONTH TO DATE-MMDDYY-DD.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO NDTE-DATE.
           PERFORM NEW-DATE-CALCULATION.
           MOVE NDTE-DATE TO ENDING-DATE.

           IF ( LOCAL-PRINT-FG NOT = "Y" )
              PERFORM OPEN-PR-FILE.

      *     PERFORM ENABLE-INTERRUPT.
           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.

           GO TO EXIT-INITIALIZE.
       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.
       EXIT-INITIALIZE.
           EXIT.

      **********************************************
       EO-EXEC-AUDIT SECTION.
      **********************************************
           MOVE EXT-EOBUF-DATE2 TO SYS-DATE.
           MOVE S-MM TO END-MONTH.
           MOVE S-YY TO END-YEAR.
           IF EXT-EOBUF-DATE1 NOT = EXT-EOBUF-DATE2
              MOVE 99 TO DAY-OF-MONTH
           ELSE
              MOVE S-DD TO DAY-OF-MONTH.

      **********************************************
       ENTER-PARAMETERS SECTION.
      **********************************************
       ENTER-PARM.

           IF VDUSTATUS = "1U"
              GO TO MISC-ISO.

       BR-01.
           MOVE LEGEND-BR-01 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "R  A000BR1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-BR.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1<"
              MOVE GRSCAN-BUF TO SCAN-BUF
              PERFORM CALL-WINDOW-PROGRAM
              MOVE SCAN-BUF TO GRSCAN-BUF
              IF SCAN-STATUS NOT = "00"
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
              GO TO CHECK-BRANCH-SECURITY.
       BRANCH-IS-NUMERIC.
           MOVE "N" TO GROUP-FLAG.
           MOVE WGR-SRESULT TO BEG-BRANCH.
           MOVE "SN N040BR1." TO VDU.
           MOVE BEG-BRANCH TO VDUNUM0.
           PERFORM SCREENIO.
       BR-02.
           MOVE LEGEND-BR-02 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN040BR2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO BR-02.
           MOVE VDUNUM0 TO END-BRANCH.
           IF BEG-BRANCH > END-BRANCH
              MOVE "INVALID BRANCH RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO BR-01.

           IF ( (BEG-BRANCH = 0) AND (END-BRANCH = 0) )
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE "USER BRANCH ONLY" TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO.

           GO TO CHECK-BRANCH-SECURITY.
       ALL-BR.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "N" TO GROUP-FLAG.
           MOVE "S  A160DISPLAY-USER." TO VDU.
           MOVE SPACES TO VDUBUF.
           PERFORM SCREENIO.

       CHECK-BRANCH-SECURITY.
           PERFORM BR-RANGE-SECURITY.
           IF BR-SECURITY-OKAY = "N"
              GO TO BR-01.

       DTE.
           IF EXT-ROUTE-BUF NOT = "00"
              GO TO DA-01.
           MOVE LEGEND-DTE TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN040ENDDATE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO DTE.
           MOVE VDUNUM0 TO EDATE.
           IF END-MONTH < 1 OR > 12
              MOVE "INVALID MONTH" TO MESS
              PERFORM SEND-MESS
              GO TO DTE.

       DA-01.
           MOVE LEGEND-DA-01 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE SPACES TO VDUSTATUS.
           MOVE "ENNN020DA1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16"
              MOVE "S  A020DA1." TO VDU
              MOVE "**" TO VDUBUF
              PERFORM SCREENIO
              MOVE 99 TO DAY-OF-MONTH
              GO TO MISC-ISO.
           IF VDUSTATUS = "1U"
              IF EXT-ROUTE-BUF = "00"
                 GO TO DTE
              ELSE
                 GO TO BR-02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO DTE.
           IF VDUNUM0 = 0 GO TO DA-01.
           MOVE VDUNUM0 TO DAY-OF-MONTH.
           IF DAY-OF-MONTH > 31  OR
              (DAY-OF-MONTH > 30 AND
              (END-MONTH = 2 OR 4 OR 6 OR 9 OR 11)) OR
              (DAY-OF-MONTH > 29 AND END-MONTH = 2)
              MOVE "DAY IS PAST LAST DAY OF MONTH" TO MESS
              PERFORM SEND-MESS
              GO TO DA-01.

      *-----------------------------------------------------------------
       MISC-ISO.
           MOVE LEGEND-NOTES-ISO TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A010MISC." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-02.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO MISC-ISO.

           MOVE VDUBUF TO MISC-CHECKS.

           IF NOT (MISC-CHECKS = "I" OR "S" OR "O")
              GO TO MISC-ISO.

      *-----------------------------------------------------------------
       LOCAL-PRINT.
           IF EXT-ROUTE-BUF NOT = "40"
              IF EXT-ROUTE-BUF NOT = "50"
                 MOVE "N" TO LOCAL-PRINT-FG
                 GO TO END-PARM.

           IF BEG-BRANCH = END-BRANCH
              MOVE "N" TO LOCAL-PRINT-FG
              GO TO END-PARM.

      *-----------------------------------------------------------------
       ASK-LOCAL.

           MOVE "S  A170SPOOL-PROMPT." TO VDU.
           MOVE "SPOOL TO LOCAL  :"    TO VDUBUF.
           PERFORM SCREENIO.
           MOVE LEGEND-LOCAL-SPOOL TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "S  A050YES-NO." TO VDU.
           MOVE "(Y/N)"          TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "E  A010LOCAL-SPOOL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-02.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO ASK-LOCAL.

           IF VDUBUF NOT = "Y"
              IF VDUBUF NOT = "N"
                 GO TO ASK-LOCAL.

           MOVE VDUBUF TO LOCAL-PRINT-FG.

       END-PARM.
           MOVE LEGEND-ACCEPT TO LEGEND.
           PERFORM SEND-LEGEND.

       EXIT-PARM.
           EXIT.

      *************************************************
       DISPLAY-PARAMETERS SECTION.
      *************************************************

           IF GROUP-FLAG = "Y"
              MOVE "S  A000BR1." TO VDU
              MOVE ENT-GROUP TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A000BR2." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "SNNN040BR1." TO VDU
              MOVE BEG-BRANCH TO VDUNUM0
              PERFORM SCREENIO
              MOVE "SNNN040BR2." TO VDU
              MOVE END-BRANCH TO VDUNUM0
              PERFORM SCREENIO.

           IF ( (BEG-BRANCH = 0) AND (END-BRANCH = 0) )
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE "USER BRANCH ONLY" TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO.

           MOVE "SNNN020DA1." TO VDU.
           MOVE DAY-OF-MONTH TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "S  A010MISC." TO VDU.
           MOVE MISC-CHECKS TO VDUBUF.
           PERFORM SCREENIO.
      *-------------------------------------------------------
           MOVE "S  A170SPOOL-PROMPT." TO VDU.
           MOVE "SPOOL TO LOCAL  :"    TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A050YES-NO." TO VDU.
           MOVE "(Y/N)"          TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A010LOCAL-SPOOL." TO VDU.
           MOVE LOCAL-PRINT-FG        TO VDUBUF.
           PERFORM SCREENIO.

      ***************************************
      *SORT-ROUTINE SECTION.
      ***************************************
      *     PERFORM ENABLE-INTERRUPT.
      *    MOVE BR-NO          TO TC-BRNO
      *                           QTRC1-WBEG-BRNO
      *                           QTRC1-WEND-BRNO.
      *    MOVE BEGINNING-DATE TO TC-KEY-DATE SQL-DATE-YYYYMMDD.
      *    PERFORM SQL-SET-DATE.
      *    MOVE SQL-DATE-YYYY-MM-DD TO QTRC1-WBEG-KEY-DATE.
      *
      *    MOVE ENDING-DATE         TO SQL-DATE-YYYYMMDD.
      *    PERFORM SQL-SET-DATE.
      *    MOVE SQL-DATE-YYYY-MM-DD TO QTRC1-WEND-KEY-DATE.
      *    MOVE 0                   TO TC-CKNO TC-CLASS TC-SEQ
      *                                QTRC1-WBEG-CKNO
      *                                QTRC1-WBEG-CLASS
      *                                QTRC1-WBEG-SEQ.
      *    MOVE ALL "9"             TO QTRC1-WEND-CKNO
      *                                QTRC1-WEND-CLASS
      *                                QTRC1-WEND-SEQ.
      *     PERFORM START-TRC1-FILE.
      * SORT-TR-NEXT.
      *     PERFORM READ-TRC1-FILE-NEXT.
      *     IF (IO-FG NOT = 0) OR (TRC-PATH-OWNBR NOT = TC-BRNO)
      *        GO TO EXIT-SORT-ROUTINE.
      *
      *     PERFORM NPR-TRAP-INTERRUPT.
      *
      *     IF TC-KEY-DATE > ENDING-DATE
      *        GO TO EXIT-SORT-ROUTINE.
      *
      *     IF TC-TYPE = "CA" OR "BK" OR "RR" OR "RC" OR
      *                  "RP" OR "RD" OR "ID" OR "IW" OR "DB"
      *                   GO TO SORT-TR-NEXT.
      *
      *     IF MISC-CHECKS = "S"
      *        IF TC-TYPE = "MS"
      *           GO TO SORT-TR-NEXT.
      *
      *     IF MISC-CHECKS = "O"
      *        IF TC-TYPE NOT = "MS"
      *           GO TO SORT-TR-NEXT.
      *
      *     MOVE TC-KEY-DATE TO WK2-DATE.
      *     MOVE TC-BRNO     TO WK2-BRANCH.
      *     MOVE TC-CKNO     TO WK2-CKNO.
      *     MOVE TC-CLASS    TO WK2-CLASS.
      *     MOVE TC-SEQ      TO WK2-SEQ.
      *     PERFORM WRITE-WK2-FILE.
      *     IF IO-FG NOT = 0
      *        GO TO IO-ERROR.
      *
      *     GO TO SORT-TR-NEXT.
      *
      *EXIT-SORT-ROUTINE.
      *     PERFORM DISABLE-INTERRUPT.
      *    PERFORM CLOSE-WK2-FILE.
      *
      ****************************************************
      * DETAIL-LINE-PROCESS SECTION.
      ****************************************************
      *     ADD TC-AMT TO CK-TOTAL GRAND-TOTAL.
      *
      * END-DETAIL-LN-PROCESS.
      *     EXIT.

      ****************************
       BRANCH-TOTALS SECTION.
      ****************************
           MOVE HOLD-BRNO        TO T-NO.
           MOVE HOLD-NAME        TO T-NAME.
           MOVE CK-TOTAL         TO T-CKTOTAL.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE 0 TO CK-TOTAL.


      ****************************
       GRAND-TOTALS SECTION.
      ****************************
           MOVE "GRAND TOTAL:" TO DETAIL-LINE.
           MOVE GRAND-TOTAL TO T-CKTOTAL.
           MOVE 4 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
       END-GRAND-TOTALS.
           EXIT.

      ****************************
       WRITE-DETAIL-LINE SECTION.
      ****************************
           ADD LN-FEED LN-COUNT GIVING LN-WK.
           IF LN-WK > EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.
           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      ******************************************************************
       HEAD-PAGE SECTION.
      ******************************************************************
           ADD 1 TO PAGE-NUMBER.
           MOVE PAGE-NUMBER TO H-PAGE-NO.
           PERFORM GET-TIME.
           MOVE TIME-EDIT TO H-TIME.

           MOVE HEAD1 TO PR-REC.
           MOVE 99 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE HEAD2 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
      
           MOVE HEAD4 TO PR-REC.
           MOVE 3 TO LN-FEED.
           PERFORM WRITE-PR-REC.
      
           MOVE 2 TO LN-FEED.

           MOVE 8 TO LN-COUNT.


      ******************************************************************
       PRINT-RANGE-LINES SECTION.
      ******************************************************************
           MOVE 4 TO LN-FEED.
           IF GROUP-FLAG = "Y"
              MOVE "BRANCH GROUP" TO RANGE-LINE
              MOVE ENT-GROUP TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE
           ELSE
              MOVE "BEG BRANCH" TO RANGE-LINE
              MOVE BEG-BRANCH TO R-NUM
              PERFORM WRITE-DETAIL-LINE
              MOVE "END BRANCH" TO RANGE-LINE
              MOVE END-BRANCH TO R-NUM
              PERFORM WRITE-DETAIL-LINE.

           MOVE "MONTH-END DATE" TO RANGE-LINE.
           MOVE EDATE TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "DAY OF MONTH" TO RANGE-LINE.
           MOVE DAY-OF-MONTH TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "MISC (ISO)" TO RANGE-LINE.
           MOVE MISC-CHECKS TO R-FG.
           PERFORM WRITE-DETAIL-LINE.


       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBWI/PFSWINDOWS.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/BRSECURE.CPY".
       COPY "LIBGB/EOSPOOL.CPY".

      *********************************************************
       FORM-RESET SECTION.
      *********************************************************
           MOVE EXT-CONAME-CONAME TO EMCKR2-CONAME.
           MOVE EXT-CONAME-SYSDATE TO EMCKR2-SYSDATE.
           DISPLAY EMCKR2-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE EMCKR2-FORM-FIELDS TO WR-BUF.
           MOVE EMCKR2-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *********************************************************
       SETUP-LINK-INFO SECTION.
      *********************************************************
           MOVE EMCKR2-HELP--FILE TO HELP-LINK-FILE.
           MOVE EMCKR2-HELP--DIR  TO HELP-LINK-DIR.

      *********************************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/EMCKR2_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ********************************************************
       MISC-ROUTINES SECTION.
      ********************************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/SYSTEM.CPY".
      * COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE MSEL-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ********************************************************
       IO-ROUTINE SECTION.
      ********************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPTRCGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
      * COPY "LIBGB/GBWK2IN.CPY".

       COPY "LIBLP/LPTRC1RN.CPY".
      *-----------------------------------------------------------------
       SUM-TRC1-FILE.
           PERFORM READ-IT.
           MOVE TRC-PATH-SQL TO E-FILE.
           MOVE SPACES      TO E-KEYX.

           EXEC SQL
            SELECT SUM(TC_AMT)
            INTO :WS-TCAMTSUM
            FROM DBO.TRCFILE
            WHERE TRCFILE.TC_BRNO  = :QTRC1-WBEG-BRNO
              AND TRCFILE.TC_KEY_DATE BETWEEN :QTRC1-WBEG-KEY-DATE
                                          AND :QTRC1-WEND-KEY-DATE
              AND TC_TYPE NOT IN 
               ('CA','BK','RR','RC','RP','RD','ID','IW','DB')

           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO SUM-TRC1-FILE.
      *-----------------------------------------------------------------

       SUM-TRC1-FILE-SKIP.
      * ALSO BYPASS TC_TYPE = 'MS'
           PERFORM READ-IT.
           MOVE TRC-PATH-SQL TO E-FILE.
           MOVE SPACES      TO E-KEYX.

           EXEC SQL
            SELECT SUM(TC_AMT)
            INTO :WS-TCAMTSUM
            FROM DBO.TRCFILE
            WHERE TRCFILE.TC_BRNO  = :QTRC1-WBEG-BRNO
              AND TRCFILE.TC_KEY_DATE BETWEEN :QTRC1-WBEG-KEY-DATE
                                          AND :QTRC1-WEND-KEY-DATE
              AND TC_TYPE NOT IN 
               ('CA','BK','RR','RC','RP','RD','ID','IW','DB','MS')

           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO SUM-TRC1-FILE-SKIP.
      *-----------------------------------------------------------------

       SUM-TRC1-FILE-ONLY.
      * ONLY INCLUDE TC_TYPE = 'MS'
           PERFORM READ-IT.
           MOVE TRC-PATH-SQL TO E-FILE.
           MOVE SPACES      TO E-KEYX.

           EXEC SQL
            SELECT SUM(TC_AMT)
            INTO :WS-TCAMTSUM
            FROM DBO.TRCFILE
            WHERE TRCFILE.TC_BRNO  = :QTRC1-WBEG-BRNO
              AND TRCFILE.TC_KEY_DATE BETWEEN :QTRC1-WBEG-KEY-DATE
                                          AND :QTRC1-WEND-KEY-DATE
              AND TC_TYPE = 'MS' 

           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO SUM-TRC1-FILE-ONLY.
      *-----------------------------------------------------------------


       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      ********************************************************
       END-PROGRAM SECTION.
      ********************************************************

           PERFORM CLOSE-FILES.

           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.

           IF NO-RECORDS-FOUND
              MOVE "NO RECORDS FOUND FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS.

      *     MOVE WK2-PATH TO ACCESS-BUF.
      *     PERFORM REMOVE-WORKFILE.
       EXIT-PROG.
           IF SV-BEG-BR NOT = 9999
              IF EXT-EOBUF-GROUP-FG = "Y"
                 MOVE SV-BEG-BR TO BEG-BRANCH
                 MOVE SV-END-BR TO END-BRANCH
                 MOVE SV-GROUP-FLAG TO GROUP-FLAG
                 MOVE SV-ENT-GROUP TO ENT-GROUP
                 MOVE SV-LOCAL-PRINT-FG TO LOCAL-PRINT-FG
              ELSE
                 MOVE SV-BEG-BR TO BEG-BRANCH
                 MOVE SV-END-BR TO END-BRANCH.

           IF EXT-ROUTE-BUF = "00"
              MOVE EXIT-PATH TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN 
              DESTROY EMCKR2-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
