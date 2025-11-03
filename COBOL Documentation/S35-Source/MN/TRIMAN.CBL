       IDENTIFICATION DIVISION.
       PROGRAM-ID.    TRIMAN.
       AUTHOR.        KEC.
       DATE-WRITTEN.  08-23-2005.
      ******************************************************************
      *
      * IDENTIFICATION:  MN/TRIMAN.C
      *
      * DESCRIPTION   :  TRANSACTION FILE MAINTENANCE - TYPE "I"
      *                     INSURANCE CLAIMS
      *
      *=================================================================
      * REV:
      * KEC 2005.0823 {PR#00419} WORLD <A15> [JIM ROSENAUER]
      *               ***NEW***
      * BLM 090320 ADD PMT TYPE, DAYS-PAID, FROM-DATE, TO-DATE,
      *            WORLD PR# 631
      * MJD 131206 A30 CONVERSION COMPLETED
      *
      * KEC 2016.1026 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         REMOVED USING HOLD-TR1-KEY.
      *         ADDED LIBLP/LPWSTR (NEW) FOR LPWSTR-TR1-KEY (HOLD/SAVE).
      *         ADDED LIBLP/LPFRTR; REMOVED LIBLP/LPTRTMPW.
      *         ADDED LPFRTR-UPDATE-CHGLOG; REMOVED WRITE-TR-CHANGE-LOG.
      *
      * KEC 2017.0120 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED TI-PRODUCER-ALP TO USE TI-PRODUCER;
      *         THE REDEFINES OF TI-PRODUCER WAS NOT USED ANYWHERE.
      *  CS 170508 ACTIVATE 8 DIGIT DATES
      * 20171030 BAH ACTIVATE 8 DIGIT DATES, PD0006
      * KEC 2018-1003 FIXED SETTING IC-PATH IN GET-IC1-PATH;
      *               IC-ALL-BASE WAS NOT BEING SET;
      *               FOUND WHILE DOING A32 
      * BAH 2019.0507 DO NOT ENTER BRANCH, FORCE BRANCH YOU ARE IN. QA0017
      * BAH 20190930 REMOVED ACCESS-CALL ICFILE
      * BAH 20200508 CHANGED TI-DRAFT-ALP TO TI-DRAFT (NUMERIC)
      * BAH 2023.1006 CHANGED SCREEN FIELD USERID TO MUST Y TO ALLOW
      *               LOWERCASE
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
      *
      *    W O R K I N G - S T O R A G E - S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)MN/TRIMAN.CBL S35 02/21/24-12:54:56 >".

       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01IC.CPY".
       COPY "LIBLP/LP01IC_SQL.CPY".
       COPY "LIBLP/LPWSIC.CPY".
       COPY "LIBLP/LP01TRI.CPY".
       COPY "LIBLP/LP01TRI_SQL.CPY".
       COPY "LIBLP/LPWSTRI.CPY".

       01  ERRCD                       PIC X         VALUE " ".
       01  ONE             PIC S9.
       01  ACTION.
           03  CHKEY       PIC X(4).
           03  CHNO        PIC 9(2).
       01  IO-TYPE         PIC X(3).
       01  NEW-FLAG        PIC 9.

       01  SAVE-BRNO       PIC 9(4) VALUE 0.
       01  SAVE-MMYY       PIC 9(4) VALUE 0.
       01  SAVE-MMYYX REDEFINES SAVE-MMYY.
           03  SAVE-MM     PIC 99.
           03  SAVE-YY     PIC 99.

       01  ORIGINAL-TRI-REC         PIC X(TRI-SIZE).
       01  CHANGED-TRI-REC          PIC X(TRI-SIZE).

       01  SYSDATE-YYYYMMDD            PIC 9(08)     VALUE ZEROES.

       01  WS-TIME-HHCMMCSS            PIC X(08)     VALUE SPACES.
       01  FILLER                      REDEFINES WS-TIME-HHCMMCSS.
           03  WS-TIME-HHCMMCSS-HH     PIC 9(02).
           03  WS-TIME-HHCMMCSS-C1     PIC X(01).
           03  WS-TIME-HHCMMCSS-MM     PIC 9(02).
           03  WS-TIME-HHCMMCSS-C2     PIC X(01).
           03  WS-TIME-HHCMMCSS-SS     PIC 9(02).

       01  WS-TIME-HHMMSS              PIC 9(06)     VALUE ZEROES.
       01  FILLER                      REDEFINES WS-TIME-HHMMSS.
           03  WS-TIME-HHMMSS-HH        PIC 9(02).
           03  WS-TIME-HHMMSS-MM        PIC 9(02).
           03  WS-TIME-HHMMSS-SS        PIC 9(02).

      ******************************************************************
      *
      *     D I S P L A Y - W O R K E R S
      *
      ******************************************************************
       01  DISPLAY-BUF                               VALUE SPACES.

               05  F01                 PIC 99/99/99.
               05  F02                 PIC 99/99/99.
               05  F03                 PIC X(08).
               05  F04                 PIC 9(02).
               05  F05                 PIC X(04).
               05  F06                 PIC 9(03).
               05  F07                 PIC ZZ,ZZZ,ZZ9.99-.
               05  F08                 PIC X(09).
               05  F09                 PIC X(15).
               05  F10                 PIC X(10).
               05  F11                 PIC 99/99/99.
               05  F12                 PIC 9(08).
               05  F13                 PIC 9(06).
               05  F14                 PIC X(10).
               05  F15                 PIC ZZZZ9.
               05  F16                 PIC 99/99/99.
               05  F17                 PIC 99/99/99.

      *=================================================================
       01  DISPLAY-LIST.
               05  FILLER PIC X(20) VALUE "010 020 030 040 050 ".
               05  FILLER PIC X(20) VALUE "060 070 080 090 100 ".
               05  FILLER PIC X(12) VALUE "110 120 130 ".
               05  FILLER PIC X(16) VALUE "140 150 160 170.".

       01  KEY-BUF.
           03  D-BRANCH    PIC ZZZ9.
           03  D-MMYY.
               05  D-MM    PIC 99.
               05  D-YY    PIC 99.
           03  D-DAY       PIC 99.
           03  D-BRNO      PIC ZZZ9.
           03  D-CODE      PIC X.
           03  D-SEQ       PIC Z9.
       01  KEY-LIST.
           03  FILLER      PIC X(32) VALUE
               "BRANCH MMYY DAY BRNO CODE SEQNO.".

      ******************************************************************
      *
      *    E L E M E N T - W O R K E R S
      *
      ******************************************************************
       78  MAX                                       VALUE 17.
       77  ELE                         PIC 9(02)     VALUE ZEROES.
           88  ELE-INSCOMP                           VALUE 04.
           88  ELE-VALID                             VALUE 01 THRU MAX.

       01  SPEC-TABLE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - -{01 THRU 10}
           03  FILLER PIC X(08) VALUE "ERNM060 ".
           03  FILLER PIC X(08) VALUE "E  A060 ".
           03  FILLER PIC X(08) VALUE "E  A100 ".
           03  FILLER PIC X(08) VALUE "ENNN020 ".
           03  FILLER PIC X(08) VALUE "E  A040 ".
           03  FILLER PIC X(08) VALUE "ENNN030 ".
           03  FILLER PIC X(08) VALUE "E NS082 ".
           03  FILLER PIC X(08) VALUE "E  A090 ".
           03  FILLER PIC X(08) VALUE "E  A150 ".
           03  FILLER PIC X(08) VALUE "E  A100 ".
      *- - - - - - - - - - - - - - - - - - - - - - - - - - -{11 THRU 13}
           03  FILLER PIC X(08) VALUE "ERNM060 ".
      * 12. TI-DRAFT
           03  FILLER PIC X(08) VALUE "ENNN080 ".
           03  FILLER PIC X(08) VALUE "ENNN060 ".
      * TI-PMT-TYPE
           03  FILLER PIC X(08) VALUE "E  A100 ".
      * TI-DAYS-PAID
           03  FILLER PIC X(08) VALUE "ENNN050 ".
      * TI-FROM-DATE
           03  FILLER PIC X(08) VALUE "ERNM060 ".
      * TI-TO-DATE
           03  FILLER PIC X(08) VALUE "ERNM060 ".

      *=================================================================
       01  SPEC-TAB                 REDEFINES SPEC-TABLE.
           03  SPEC-REC             OCCURS MAX.
               05  SPEC         PIC X(07).
               05  CHFG        PIC X(01).

      ******************************************************************
      *
      *    L E G E N D - W O R K E R S
      *
      ******************************************************************
       01  LEGEND                      PIC X(72)     VALUE SPACES.
       01  LEGEND-LIST                 PIC X(07)     VALUE "LEGEND.".

      *=================================================================
       01  LEGEND-ENTER-ELE.
           03  FILLER PIC X(30) VALUE "F1-RESET, ENTER FIELD INFORMAT".
           03  FILLER PIC X(30) VALUE "ION                           ".
           03  FILLER PIC X(14) VALUE "              ".

       01  LEGEND-ENTER-KEY-BRANCH.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, ENTER TRANS".
           03  FILLER PIC X(30) VALUE "ACTION FILE PATH BRANCH NUMBER".
           03  FILLER PIC X(14) VALUE "              ".

       01  LEGEND-ENTER-KEY-MMYY.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, ENTER TRANS".
           03  FILLER PIC X(30) VALUE "ACTION FILE PATH DATE (MM/YY) ".
           03  FILLER PIC X(14) VALUE "              ".

       01  LEGEND-ENTER-KEY-CODE.
           03  FILLER PIC X(30) VALUE "F1-RESET, F5-SEARCH, F6-SCAN, ".
           03  FILLER PIC X(30) VALUE "F7-EXIT, ENTER TRANSACTION COD".
           03  FILLER PIC X(14) VALUE "E             ".

       01  LEGEND-ENTER-KEY-BRNO.
           03  FILLER PIC X(30) VALUE "F1-RESET, F5-SEARCH, F6-SCAN, ".
           03  FILLER PIC X(30) VALUE "F7-EXIT, ENTER BRANCH NUMBER  ".
           03  FILLER PIC X(14) VALUE "              ".

       01  LEGEND-ENTER-KEY-DAY.
           03  FILLER PIC X(30) VALUE "F1-RESET, F5-SEARCH, F6-SCAN, ".
           03  FILLER PIC X(30) VALUE "F7-EXIT, ENTER DAY OF TRANSACT".
           03  FILLER PIC X(14) VALUE "ION           ".

       01  LEGEND-ENTER-KEY-SEQNO.
           03  FILLER PIC X(30) VALUE "F1-RESET, F5-SEARCH, F6-SCAN, ".
           03  FILLER PIC X(30) VALUE "F7-EXIT, ENTER SEQNO OF TRANSA".
           03  FILLER PIC X(14) VALUE "CTION         ".

       01  LEGEND-MODIFY-ROUTINE.
           03  FILLER PIC X(30) VALUE "                      F1-RESET".
           03  FILLER PIC X(30) VALUE ", ENTER-SAVE, ENTER FIELD NUMB".
           03  FILLER PIC X(14) VALUE "ER TO CHANGE: ".

       01  LEGEND-SEARCH-DRIVER.
           03  FILLER PIC X(30) VALUE "F1-RESET, F5-SCAN NEXT, <ENTER".
           03  FILLER PIC X(30) VALUE "> TO EDIT CURRENT RECORD, ENTE".
           03  FILLER PIC X(14) VALUE "R KEY TO START".


      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBWI/TRISCNW.CPY".
       01 TRIMAN-WINDOW-HANDLE PIC X(10) VALUE SPACES.
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/TRIMAN_DEF.CPY".
       COPY "LIBMN/TRIMAN_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/ACCESSW.CPY".

       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBMN/TRIMAN_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

      ******************************************************************
      *
      *    M A I N - P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.
           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".
           MOVE "TRIMAN" TO VDU-FORM-NAME.
           MOVE FORM-PATHNAME TO FORM-PATH.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS TRIMAN-WINDOW-HANDLE.

           PERFORM FORM-RESET.

       MAIN-MODULE.

       ENTER-BRANCH.

      * FORCE BRANCH YOU ARE IN
           MOVE "SNNN040BRANCH." TO VDU.
           MOVE EXT-FILPATH-BRANCH TO VDUNUM0 BR-NO SAVE-BRNO.
           PERFORM SCREENIO.
      * FORCE BRANCH YOU ARE IN

      *    MOVE "ENNN040BRANCH." TO VDU.
      *    PERFORM SCREENIO.
      *    IF VDUSTATUS = "1>"
      *       GO TO END-ROUTINE.
      *    IF VDUSTATUS NOT = "00"
      *       GO TO ENTER-BRANCH.
      *    MOVE VDUNUM0 TO BR-NO SAVE-BRNO.

           PERFORM LOAD-BRANCH-MODULE.
           IF ( BR-PATH = SPACES )
              MOVE "INVALID BRANCH" TO MESS
              PERFORM SEND-MESS
              GO TO END-ROUTINE.

      *----------------------------------------------------------------*
      *     E N T E R    M O N T H
      *----------------------------------------------------------------*
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

           PERFORM OPEN-TRI1-FILE.

       RESTART-MAINT.
           PERFORM FORM-RESET.

           PERFORM ENTER-KEYS-MODULE.

           IF IO-TYPE = "END"
              GO TO END-ROUTINE.

           IF IO-TYPE = "CLR"
             GO TO MAIN-MODULE.

           PERFORM ENTRY-MODULE.

           IF ( IO-TYPE = "RWR" )
              PERFORM WRITE-MODULE.

           IF ( IO-TYPE = "CLR" )
              GO TO MAIN-MODULE.

           IF ( IO-TYPE = "DEL" )
              PERFORM DELETE-MODULE.

           GO TO RESTART-MAINT.

      ******************************************************************
       NEW-RECORD-SETUP SECTION.
      ******************************************************************

           MOVE 1 TO NEW-FLAG.
           MOVE "S  A000NEW."              TO VDU.
           MOVE "*** NEW RECORD ***"       TO VDUBUF.
           PERFORM SCREENIO.

           PERFORM CLEAR-TRI-FILE.
           MOVE LPWSTRI-TRI1-KEY           TO TRI1-KEY.
           MOVE 0 TO ELE.

      ******************************************************************
       DELETE-MODULE SECTION.
      ******************************************************************

           IF ( NEW-FLAG NOT = 0 )
              GO TO END-DELETE-MODULE.

           PERFORM OPEN-TRI1-FILE.
           PERFORM READ-TRI1-FILE.
           IF ( IO-BAD )
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-DELETE-MODULE.

           IF ( TRI-REC NOT = ORIGINAL-TRI-REC )
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-CHANGED
              GO TO END-DELETE-MODULE.

           PERFORM DELETE-TRI1-FILE.

       END-DELETE-MODULE.
           PERFORM CLOSE-TRI1-FILE.

      ******************************************************************
       DISPLAY-FIELDS SECTION.
      ******************************************************************

           MOVE SPACES                     TO DISPLAY-BUF.

           MOVE TI-POSTDATE                TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY                TO F01.
 
           MOVE TI-POSTTIME                TO F02.
           INSPECT F02 REPLACING ALL "/" BY ":".

           MOVE TI-USERID                  TO F03.
           MOVE TI-INSCOMP                 TO F04.
           MOVE TI-WAIS                    TO F05.
           MOVE TI-TYPE                    TO F06
           MOVE TI-AMOUNT                  TO F07.
           MOVE TI-CLAIM-NUMBER            TO F08.
           MOVE TI-LNAME                   TO F09.
           MOVE TI-FNAME                   TO F10.
           MOVE TI-DATE                    TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY                TO F11.
           MOVE TI-DRAFT                   TO F12.
           MOVE TI-PRODUCER                TO F13.
           MOVE TI-PMT-TYPE                TO F14.
           MOVE TI-DAYS-PAID               TO F15.
           MOVE TI-FROM-DATE               TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY                TO F16.
           MOVE TI-TO-DATE                 TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY                TO F17.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       DISPLAY-FIELDS-EXIT.
           EXIT.

      ******************************************************************
       DISPLAY-KEY SECTION.
      ******************************************************************

           MOVE SAVE-BRNO           TO D-BRANCH.

           MOVE LPWSTRI-TI-KEY-DATE TO DATE-YYYYMMDD.
           MOVE DATE-YYYYMMDD-MM    TO D-MM.
           MOVE DATE-YYYYMMDD-YY    TO D-YY.


           MOVE DATE-YYYYMMDD-DD      TO D-DAY.
           MOVE LPWSTRI-TI-BRNO       TO D-BRNO.
           MOVE "I"                   TO D-CODE.
           MOVE LPWSTRI-TI-SEQ        TO D-SEQ.

           MOVE KEY-BUF TO WR-BUF.
           MOVE KEY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ******************************************************************
      *
      *    E N T R Y - M O D U L E
      *
      *-----------------------------------------------------------------
      *         NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *                    1 - INITIAL NEW RECORD ENTRY
      *                    2 - MODIFY NEW RECORD
      ******************************************************************
       ENTRY-MODULE SECTION.

           MOVE SPACES          TO IO-TYPE.
           MOVE 1               TO ONE.

       CREATE-ROUTINE.
           IF NEW-FLAG NOT = 1
              GO TO MODIFY-ROUTINE.

           ADD ONE TO ELE.

           IF ( ELE < 1   )
              MOVE 1 TO ELE ONE.
           IF ELE NOT > MAX
              GO TO ENTER-ELE.

           MOVE 2 TO NEW-FLAG.

      *-----------------------------------------------------------------
       MODIFY-ROUTINE.

           MOVE LEGEND-MODIFY-ROUTINE      TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "R RA060SEL."              TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

           IF VDUSTATUS NOT = "00"
              GO TO MODIFY-ROUTINE.

           MOVE VDUBUF TO ACTION.

           IF ( ACTION = SPACES )
              MOVE "RWR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

           IF ( ACTION = "DELETE" )
              MOVE "DEL" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

           INSPECT ACTION REPLACING ALL SPACES BY ZEROES.

           IF CHNO NOT NUMERIC
              GO TO MODIFY-ROUTINE.

           IF CHNO = 0
              GO TO MODIFY-ROUTINE.

           IF CHNO > MAX
              GO TO MODIFY-ROUTINE.

           MOVE CHNO TO ELE.

      *------------------------------------------------------------
       ENTER-ELE.

           MOVE LEGEND-ENTER-ELE           TO LEGEND.
           PERFORM SEND-LEGEND.

           IF CHKEY NOT = ZEROS
              IF CHFG(ELE) = " "
                 IF NEW-FLAG NOT = 1
                    GO TO MODIFY-ROUTINE.

           IF NEW-FLAG = 0
              IF CHFG(ELE) = "N"
                 IF CHKEY NOT = EXT-CONAME-DAILY-PASSWORD
                    GO TO MODIFY-ROUTINE.

           MOVE SPEC(ELE)      TO VDU.
           MOVE ELE            TO VDUROW.
           MOVE ZEROES         TO VDUCOL.
           MOVE "."            TO VDUDECIMAL.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
       TEST-F7.
           IF VDUSTATUS = "1>"
             IF ELE > 6 AND < 17
                MOVE 17 TO ELE
                GO TO ENTER-ELE.

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

      *-----------------------------------------------------------------
       GOTO-FIELD.

           GO TO F-01 F-02 F-03 F-04 F-05 F-06 F-07 F-08 F-09 F-10
                 F-11 F-12 F-13 F-14 F-15 F-16 F-17
                 DEPENDING ON ELE.

           GO TO END-ENTRY-MODULE.

      *****************************************************************
      *
      *    S E T - R E C O R D - E L E M E N T S
      *
      *****************************************************************
       F-01.

           IF ( VDUNUM0 = ZEROES )
              MOVE "S  8080###."           TO VDU
              MOVE ELE                     TO VDUROW
              MOVE ZEROES                  TO VDUCOL
              MOVE "."                     TO VDUDECIMAL
              MOVE SYSDATE-YYYYMMDD        TO VDU-DATE-YYYYMMDD
              PERFORM SCREENIO
              GO TO ENTER-ELE.

           MOVE VDU-DATE-YYYYMMDD          TO TI-POSTDATE.
           GO TO CREATE-ROUTINE.

       F-02.

           MOVE VDUBUF                     TO WS-TIME-HHCMMCSS.
           MOVE ZEROES                     TO WS-TIME-HHMMSS.
           PERFORM CONVERT-WS-TIME-DRIVER.

           IF ( WS-TIME-HHMMSS-HH = ZEROES ) AND
              ( WS-TIME-HHMMSS-MM = ZEROES ) AND
              ( WS-TIME-HHMMSS-SS = ZEROES )
              MOVE "S  A080###."           TO VDU
              MOVE ELE                     TO VDUROW
              MOVE ZEROES                  TO VDUCOL
              MOVE "."                     TO VDUDECIMAL
              MOVE "Y"                     TO TIME-MILITARY-FG
              PERFORM GET-TIME
              MOVE SPACES                  TO WS-TIME-HHCMMCSS
              MOVE HH-MM-SS                TO WS-TIME-HHMMSS
              PERFORM CONVERT-WS-TIME-DRIVER
              MOVE WS-TIME-HHCMMCSS        TO VDUBUF
              PERFORM SCREENIO
              GO TO ENTER-ELE.

           IF ( WS-TIME-HHMMSS-HH > 23 ) OR
              ( WS-TIME-HHMMSS-MM > 59 ) OR
              ( WS-TIME-HHMMSS-SS > 59 )
              GO TO ENTER-ELE.

           MOVE WS-TIME-HHMMSS             TO TI-POSTTIME.
           GO TO CREATE-ROUTINE.

       F-03.

           IF ( VDUBUF = SPACES )
              MOVE SPEC(ELE)        TO VDU
              MOVE "S"                     TO VDUACTION
              MOVE ELE                     TO VDUROW
              MOVE ZEROES                  TO VDUCOL
              MOVE "."                     TO VDUDECIMAL
              MOVE EXT-CONAME-USER-ID      TO VDUBUF
              PERFORM SCREENIO
              GO TO ENTER-ELE.

           MOVE VDUBUF                     TO TI-USERID.
           GO TO CREATE-ROUTINE.

       F-04.

           MOVE VDUNUM0                    TO IC-CODE.
           PERFORM GET-IC1-RECORD.
           IF ( IO-BAD )
              MOVE "ERROR: INS. COMPANY NOT ON FILE (F6-SCAN)" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           MOVE IC-CODE                    TO TI-INSCOMP.
           GO TO CREATE-ROUTINE.

       F-05.

           MOVE VDUBUF                     TO TI-WAIS.
           GO TO CREATE-ROUTINE.

       F-06.

           MOVE VDUNUM0                    TO TI-TYPE.

           IF NOT ( TI-TYPE-VALID )
              MOVE "ERROR: TYPE INVALID - F8-HELP" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

       F-07.

           IF ( VDUNUM0 = ZEROES )
              GO TO ENTER-ELE.

           MOVE VDUNUM2                    TO TI-AMOUNT.
           GO TO CREATE-ROUTINE.

       F-08.

           IF ( VDUBUF = SPACES )
              GO TO ENTER-ELE.

           MOVE VDUBUF                     TO TI-CLAIM-NUMBER.
           GO TO CREATE-ROUTINE.

       F-09.

           IF ( VDUBUF = SPACES )
              GO TO ENTER-ELE.

           MOVE VDUBUF                     TO TI-LNAME.
           GO TO CREATE-ROUTINE.

       F-10.

           IF ( VDUBUF = SPACES )
              GO TO ENTER-ELE.

           MOVE VDUBUF                     TO TI-FNAME.
           GO TO CREATE-ROUTINE.

       F-11.

           IF ( VDUNUM0 = ZEROES )
              MOVE "S  8080###."           TO VDU
              MOVE ELE                     TO VDUROW
              MOVE ZEROES                  TO VDUCOL
              MOVE "."                     TO VDUDECIMAL
              MOVE SYSDATE-YYYYMMDD        TO VDU-DATE-YYYYMMDD
              PERFORM SCREENIO
              GO TO ENTER-ELE.
         
           MOVE VDU-DATE-YYYYMMDD         TO TI-DATE.

           GO TO CREATE-ROUTINE.

       F-12.

           MOVE VDUNUM0                   TO TI-DRAFT.
           GO TO CREATE-ROUTINE.

       F-13.

           MOVE VDUNUM0                    TO TI-PRODUCER.
           GO TO CREATE-ROUTINE.

       F-14.
           MOVE VDUBUF                     TO TI-PMT-TYPE.
           GO TO CREATE-ROUTINE.

       F-15.
           MOVE VDUNUM0                    TO TI-DAYS-PAID.
           GO TO CREATE-ROUTINE.

       F-16.
           MOVE VDU-DATE-YYYYMMDD          TO TI-FROM-DATE.
           GO TO CREATE-ROUTINE.

       F-17.
           MOVE VDU-DATE-YYYYMMDD          TO TI-TO-DATE.
           GO TO CREATE-ROUTINE.

       END-ENTRY-MODULE.
           EXIT.

      ******************************************************************
       ENTER-KEYS-MODULE SECTION.
      ******************************************************************
       ENTER-DAY.
           MOVE "F6 - SCAN"   TO MESS.
           PERFORM CALL-TMMSG.
           MOVE SPACES        TO IO-TYPE.

           MOVE 0 TO LPWSTRI-TI-KEY-DATE LPWSTRI-TI-BRNO
                     LPWSTRI-TI-SEQ.
           MOVE 0             TO NEW-FLAG.

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
              MOVE SAVE-BRNO   TO TRISCN-BRNO
              MOVE SAVE-MMYY   TO TRISCNW-MMYY
              MOVE "Y"         TO TRISCN-RETURN
              MOVE "WI/TRISCN " TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH TRISCN-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              IF TRISCN-STATUS NOT = "00"
                 GO TO ENTER-DAY
              ELSE
                 MOVE TRISCN-TRI1-KEY TO LPWSTRI-TRI1-KEY
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

           MOVE DATE-YYYYMMDD TO LPWSTRI-TI-KEY-DATE.

      *----------------------------------------------------------------
       ENTER-BRNO.

      * FORCE BRANCH YOU ARE IN
           MOVE "SNNN040BRNO." TO VDU.
           MOVE EXT-FILPATH-BRANCH TO VDUNUM0 BR-NO LPWSTRI-TI-BRNO
                                     TI-BRNO.
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
      *    MOVE VDUNUM0 TO BR-NO LPWSTRI-TI-BRNO TI-BRNO.

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
           MOVE "I"            TO VDUBUF.
           PERFORM SCREENIO.

       ENTER-SEQ.
           MOVE "ENNN040SEQNO." TO VDU.
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
              GO TO ENTER-SEQ.

           MOVE VDUNUM0 TO LPWSTRI-TI-SEQ.

      *----------------------------------------------------------------
       READ-TRANS-FILE.
           PERFORM OPEN-TRI1-FILE.
           MOVE LPWSTRI-TRI1-KEY TO TRI1-KEY.
           PERFORM READ-TRI1-FILE.
           IF IO-FG = 0
              MOVE TRI-REC TO ORIGINAL-TRI-REC
              PERFORM DISPLAY-FIELDS
           ELSE
              PERFORM NEW-RECORD-SETUP.

       END-ENTER-KEYS-MODULE.
           EXIT.


      ******************************************************************
       WRITE-MODULE SECTION.
      ******************************************************************

           PERFORM OPEN-TRI1-FILE.

           IF ( NEW-FLAG NOT = 0 )
              GO TO WRITE-RECORD.

           MOVE TRI-REC  TO CHANGED-TRI-REC.

           IF ( ORIGINAL-TRI-REC = CHANGED-TRI-REC )
              GO TO END-WRITE-MODULE.

           PERFORM READ-TRI1-FILE.
           IF ( IO-BAD )
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-WRITE-MODULE.

           IF ( TRI-REC NOT = ORIGINAL-TRI-REC )
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-WRITE-MODULE.

           MOVE CHANGED-TRI-REC TO TRI-REC.

           PERFORM REWRITE-TRI1-FILE.
           MOVE "C"                        TO ERRLOGW-ACTION.
           MOVE TRI1-KEY                    TO ERRLOGW-KEY.
           MOVE FORM-PATHNAME              TO ERRLOGW-PROGX.
           MOVE "CL/ERRLOG"                TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PROGX.

           GO TO END-WRITE-MODULE.

      *---------------------------------------------------------------
       WRITE-RECORD.
           PERFORM WRITE-TRI1-FILE.
           IF ( IO-BAD )
              PERFORM ALREADY-CREATED.

       END-WRITE-MODULE.
           PERFORM CLOSE-TRI1-FILE.


      **********************************************************
       FORM-RESET SECTION.
      **********************************************************
           MOVE EXT-CONAME-CONAME TO TRIMAN-CONAME.
           MOVE EXT-CONAME-SYSDATE TO TRIMAN-SYSDATE.
           DISPLAY SPACES UPON TRIMAN-WINDOW-HANDLE
                               LINE 0 COL 0 ERASE SCREEN.
           DISPLAY TRIMAN-SCREEN UPON TRIMAN-WINDOW-HANDLE.
           IF SAVE-BRNO NOT = 0
              MOVE SAVE-BRNO TO TRIMAN-BRANCH
              MOVE SAVE-MMYY TO TRIMAN-MMYY.

           MOVE TRIMAN-FORM-FIELDS TO WR-BUF.
           MOVE TRIMAN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **********************************************************
       SETUP-LINK-INFO SECTION.
      **********************************************************
           MOVE TRIMAN-HELP--FILE TO HELP-LINK-FILE.
           MOVE TRIMAN-HELP--DIR TO HELP-LINK-DIR.

      **********************************************************
       VDU-CONVERT-FIELD SECTION.
      **********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/TRIMAN_EVA.CPY".
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
     
           MOVE "B"                        TO TRI-PATH-OVERRIDE.
     
       LOAD-BRANCH-MODULE-EXIT.
           EXIT.
     

      ******************************************************************
       CONVERT-WS-TIME-DRIVER SECTION.
      ******************************************************************

           IF ( WS-TIME-HHMMSS = ZEROES )
              GO TO CONVERT-WS-TIME-HHMMSS.

       CONVERT-WS-TIME-HHCMMCSS.

           MOVE WS-TIME-HHMMSS-HH          TO WS-TIME-HHCMMCSS-HH.
           MOVE ":"                        TO WS-TIME-HHCMMCSS-C1.
           MOVE WS-TIME-HHMMSS-MM          TO WS-TIME-HHCMMCSS-MM.
           MOVE ":"                        TO WS-TIME-HHCMMCSS-C2.
           MOVE WS-TIME-HHMMSS-SS          TO WS-TIME-HHCMMCSS-SS.

           GO TO CONVERT-WS-TIME-DVR-EXIT.

       CONVERT-WS-TIME-HHMMSS.

           IF ( WS-TIME-HHCMMCSS-HH NOT NUMERIC )
              MOVE ZEROES                  TO WS-TIME-HHCMMCSS-HH.

           IF ( WS-TIME-HHCMMCSS-MM NOT NUMERIC )
              MOVE ZEROES                  TO WS-TIME-HHCMMCSS-MM.

           IF ( WS-TIME-HHCMMCSS-SS NOT NUMERIC )
              MOVE ZEROES                  TO WS-TIME-HHCMMCSS-SS.

           MOVE WS-TIME-HHCMMCSS-HH        TO WS-TIME-HHMMSS-HH.
           MOVE WS-TIME-HHCMMCSS-MM        TO WS-TIME-HHMMSS-MM.
           MOVE WS-TIME-HHCMMCSS-SS        TO WS-TIME-HHMMSS-SS.

       CONVERT-WS-TIME-DVR-EXIT.
           EXIT.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/TIMEIO.CPY".

       COPY "LIBLP/LPTRICL.CPY".


      ******************************************************************
       MISCELLANEOUS-PARAGRAPHS SECTION.
      ******************************************************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
       GET-BR-RECORD.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.

      *-----------------------------------------------------------------
       GET-IC1-RECORD.

      * DO *NOT* SET FILPATH-WORKERS FROM THIS READ-BR-FILE.
      * THIS BRNO MAYBE DIFFERENT FROM THE ONE USED FOR THE
      * PREVIOUS READ-BR-FILE USED TO GET TRANS-PATH.

           MOVE TI-BRNO                    TO BR-NO.
           PERFORM GET-BR-RECORD.

           MOVE "Y"                        TO IC-PATH-OVERRIDE.
           MOVE EXT-FILPATH-BASE           TO IC-ALL-BASE.
           MOVE BR-MACHINE                 TO IC-ALL-MACHINE.
           MOVE BR-DATA-PATH               TO IC-ALL-DATA.
           MOVE IC-ALL-PATH                TO IC-PATH.
           PERFORM OPEN-IC1-FILE.
           PERFORM READ-IC1-FILE.
           PERFORM CLOSE-IC1-FILE.

      *-----------------------------------------------------------------
       SEND-LEGEND.

           MOVE LEGEND TO WR-BUF.
           MOVE LEGEND-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

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


      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-TRI1-FILE.
           PERFORM CLOSE-IC1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPTRIGS_SQL.CPY".
       COPY "LIBLP/LPICGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".

       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPIC1RN.CPY".
       COPY "LIBLP/LPTRI1IN.CPY".

       COPY "LIBGB/FERRORS.CPY".

      ******************************************************************
       END-ROUTINE SECTION.
      ******************************************************************
       END-RUN.

           PERFORM CLOSE-FILES.

       EXIT-PROG.

           MOVE FORM-PATH                  TO FORM-PATHNAME.

       STOP-RUN.
           DESTROY TRIMAN-WINDOW-HANDLE.
           MOVE FORM-PATH TO FORM-PATHNAME.
           EXIT PROGRAM.
