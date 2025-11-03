       IDENTIFICATION DIVISION.
       PROGRAM-ID.      INSTRP.
       DATE-WRITTEN.    081612.
      **********************************************************************
      *                      (SP)
      *    DESCRIPTION:  SOFTWARE INSTALLATION REPORT
      *    NEW: 8/16/12, WORLD PR# 815
      *  REV:
      *  20170926 BAH ACTIVATE 8 DIGIT DATES, PD0006
      **********************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
           SELECT LOG-FILE ASSIGN TO LOG-PATH
                           ORGANIZATION LINE SEQUENTIAL
                           ACCESS SEQUENTIAL
                           LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                           FILE STATUS FILE-STAT.
       COPY "LIBGB/GBFSWKI.CPY".
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".
       FD  LOG-FILE
           RECORD IS VARYING IN SIZE FROM 20 TO 80 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  LOG-REC.
           03  LOG-CHAR             PIC X OCCURS 80 TIMES.
       FD  WK-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK-REC.
           03  WK-KEY.
               05  WK-YYMMDD1   PIC 9(6).
               05  WK-PRNO      PIC X(8).
               05  WK-YYMMDD2   PIC 9(6).
               05  WK-SEQNO     PIC 9(4).
           03  WK-REST-OF-REC.
               05  WK-TIME.
                   07  WK-HH    PIC 99.
                   07  WK-COL1  PIC X.
                   07  WK-MM    PIC 99.
                   07  WK-COL2  PIC X.
                   07  WK-SS    PIC 99.
               05  FILLER       PIC X         VALUE " ".
               05  WK-USERID    PIC X(8).
               05  FILLER       PIC X(2)      VALUE "  ".
               05  WK-FILENAM   PIC X(25).
               05  FILLER       PIC X(19)     VALUE "  ".
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)SP/INSTRP.CBL J35 12/21/23-09:41:15 >".
      ****************************
      *     F I L E   I F N      *
      ****************************
       COPY "LIBSP/INSTRP_CMD.CPY".
       01  PR-IFN               PIC X(2)    VALUE "PR".
       01  WK-IFN               PIC X(2)    VALUE "WK".
       01  LOG-PATH             PIC X(21).

       01  LOG-WORK.
           03  LOG-PRNO           PIC X(8).
           03  FILLER REDEFINES LOG-PRNO.
               05  PR-CHAR        PIC X         OCCURS 8.
           03  FILLER             PIC X.
           03  LOG-DATE           PIC 99/99/99.
           03  FILLER REDEFINES LOG-DATE.
               05  LOG-MM         PIC 99.
               05  LOG-SLASH1     PIC X.
               05  LOG-DD         PIC 99.
               05  LOG-SLASH2     PIC X.
               05  LOG-YY         PIC 99.
           03  FILLER             PIC X.
           03  LOG-REST-OF-REC.
               05  LOG-TIME.
                   07  LOG-HH     PIC 99.
                   07  LOG-COL1   PIC X.
                   07  LOG-MM     PIC 99.
                   07  LOG-COL2   PIC X.
                   07  LOG-SS     PIC 99.
               05  FILLER         PIC X.
               05  LOG-USERID     PIC X(8).
               05  FILLER         PIC X(2).
               05  LOG-FILENAM.
                   07  FILLER     PIC X(5).
                   07  LOG-REL    PIC X(3).
                   07  FILLER     PIC X(17).
               05  FILLER         PIC X(19).
       01  LOG-DESC-WORK REDEFINES LOG-WORK.
           03  FILLER           PIC X(17).
           03  LOG-DESC-REL     PIC X(3).
           03  FILLER           PIC X.
           03  LOG-DESC         PIC X(59).

      **********************
      *   LINE WORKERS     *
      **********************
       01  LN-WORKERS.
           03  LN-FEED          PIC 99       VALUE 0.
           03  LN-COUNT         PIC 99       VALUE 0.
           03  LN-WK            PIC 99       VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

      **********************
      *   MISC WORKERS     *
      **********************
       01  FIRST-TIME           PIC X        VALUE "Y".
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".
       01  RECORDS-FOUND        PIC X        VALUE " ".
           88  NO-RECORDS-FOUND              VALUE "N".

       01  LOG-DATE-MMDDYY      PIC 9(6).
       01  HOLD-PRNO            PIC X(8) VALUE SPACES.

       01  HOLD-USERID.
           03  FILLER           PIC X.
           03  H-BR             PIC 9(4).
           03  FILLER           PIC X(7).

       01  HOLD-FIL.
           03  HOLD-XXX             PIC X(3).
           03  HOLD-SLASH1          PIC X.
           03  HOLD-XXXX            PIC X(4).
           03  HOLD-SLASH2          PIC X.
           03  HOLD-MACHINE         PIC X(2).
           03  HOLD-SLASH3          PIC X.
           03  HOLD-DATA-PATH       PIC X(6).

      ***********************************
      *     S C R E E N   F I E L D S   *
      ***********************************
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  BEG-PRNO        PIC X(8).
           03  END-PRNO        PIC X(8).
           03  BEG-DATE        PIC 9(8).
           03  END-DATE        PIC 9(8).
           03  RELEASEX        PIC X(3).
           03  PRINT-SEQ       PIC 9.
               88 ORDER-BY-PR   VALUE 1.
               88 ORDER-BY-DATE VALUE 2.

       01  VERSION-CONTROL     PIC X VALUE "A".

      ***********************************
      *     P R I N T   B U F F E R S   *
      ***********************************

       01  HEAD1.
           03  H-DATE           PIC X(8)     VALUE " ".
           03  FILLER           PIC XX       VALUE " ".
           03  H-HR             PIC 99       VALUE 0.
           03  FILLER           PIC X        VALUE ":".
           03  H-MIN            PIC 99       VALUE 0.
           03  FILLER           PIC X(31)    VALUE " ".
           03  H-NAME           PIC X(40)    VALUE " ".
           03  FILLER           PIC X(37)    VALUE " ".
           03  FILLER           PIC X(5)     VALUE "PAGE ".
           03  H-PAGE-NO        PIC ZZZ9.

       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(26)    VALUE " ".
           03  FILLER           PIC X(39)    VALUE
               "PROGRAMMING REQUEST INSTALLATION REPORT".
           03  FILLER           PIC X(47)    VALUE " ".

       01  HEAD3.
           03  FILLER      PIC X(50)    VALUE
           "PR #    DATE     TIME     USER      INSTALLED ITEM".

       01  DETAIL-LINE          PIC X(132) VALUE SPACES.

       01  D-LINE1 REDEFINES DETAIL-LINE.
           03  D-PRNO             PIC X(8).
           03  FILLER             PIC X(2).
           03  D-DATE             PIC 99/99/99.
           03  FILLER             PIC X(2).
           03  D-DESC             PIC X(63).

       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(50).
           03  RANGE-SELECTION  PIC X(15).
           03  RANGE-DATE REDEFINES RANGE-SELECTION.
               05 RANGE-DT      PIC 9999/99/99.
               05 FILLER        PIC X(5).


       COPY "LIBLP/LHSTATW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/INSTRP_DEF.CPY".
       COPY "LIBSP/INSTRP_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *****************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      *****************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBSP/INSTRP_SCN.CPY".

      *----------------------------------------------------------------
       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE LOG-FILE WK-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-PR-FILE.
           CLOSE
               LOG-FILE WK-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      *************************************
       MAIN-MODULE SECTION.
           PERFORM SQL-CONNECT.
      *************************************
       MAIN-ROUTINE.
      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "INSTRP" TO FORM-PROG VDU-FORM-NAME.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

       NEXT-LOG.
           PERFORM READ-LOG-FILE.
           IF IO-FG = 9
              GO TO PROCESS-SORT-FILE.

           MOVE LOG-REC TO LOG-WORK.
           IF LOG-PRNO = "********"
              GO TO NEXT-LOG.

           MOVE LOG-DATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-MMDDYY.
           MOVE DATE-MMDDYY TO LOG-DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO SYS-DATE.
           IF DATE-YYYYMMDD < BEG-DATE OR DATE-YYYYMMDD > END-DATE
               GO TO NEXT-LOG.

           IF RELEASEX = "***"
              GO TO CHECK-PR-RANGE.

           IF LOG-COL1 = ":"
              IF LOG-REL NOT = RELEASEX
                 GO TO NEXT-LOG
              END-IF
           ELSE
              IF LOG-DESC-REL NOT = RELEASEX
                 GO TO NEXT-LOG.

       CHECK-PR-RANGE.
           IF LOG-PRNO < BEG-PRNO OR LOG-PRNO > END-PRNO
             GO TO NEXT-LOG.

           MOVE LOG-PRNO TO WK-PRNO.
           MOVE LOG-DATE-MMDDYY TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYMMDD.
           IF ORDER-BY-PR
              MOVE 0           TO WK-YYMMDD1
              MOVE DATE-YYMMDD TO WK-YYMMDD2
           ELSE
              MOVE 0           TO WK-YYMMDD2
              MOVE DATE-YYMMDD TO WK-YYMMDD1.
           MOVE 0 TO WK-SEQNO.
       WRITE-SORT-REC.
           MOVE LOG-REST-OF-REC TO WK-REST-OF-REC.
           PERFORM WRITE-WK-FILE.
           IF IO-BAD
              IF E-CODE = "22"
                 ADD 1 TO WK-SEQNO
                 GO TO WRITE-SORT-REC.

           GO TO NEXT-LOG.

       PROCESS-SORT-FILE.
           PERFORM CLOSE-WK-FILE.
           PERFORM OPEN-WK-FILE.
       NEXT-WK.
           PERFORM READ-WK-FILE-NEXT.
           IF IO-BAD GO TO END-ROUTINE.
           IF WK-PRNO NOT = HOLD-PRNO
              MOVE WK-PRNO TO HOLD-PRNO
              MOVE 2 TO LN-FEED.
           MOVE WK-PRNO        TO D-PRNO.
           IF ORDER-BY-PR
              MOVE WK-YYMMDD2  TO DATE-YYMMDD
           ELSE
              MOVE WK-YYMMDD1  TO DATE-YYMMDD.
           PERFORM CONVERT-YYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY    TO D-DATE.
           MOVE WK-REST-OF-REC TO D-DESC.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "N" TO FIRST-TIME.

           GO TO NEXT-WK.

       END-ROUTINE.
           IF FIRST-TIME = "Y" AND
              EXT-ROUTE-BUF = "00"
              MOVE "N" TO RECORDS-FOUND
              GO TO END-PROGRAM.
           IF FIRST-TIME = "Y"
              MOVE SPACES TO PR-REC
              MOVE 99 TO LN-FEED
              PERFORM WRITE-PR-REC.
           PERFORM PRINT-RANGE-LINES.
           GO TO END-PROGRAM.

      /**********************************
      *   I N I T I A L I Z A T I O N   *
      ***********************************
       INITIALIZATION SECTION.
           MOVE INSTRP-QUEUE-POS TO Q-POS.
           MOVE INSTRP-COPIES-POS TO C-POS.
           MOVE INSTRP-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
       COPY "LIBGB/EOINIT.CPY".


           IF EXT-FILPATH-MACHINE = R1
              MOVE LOG-PATH-R1 TO LOG-PATH
           ELSE IF EXT-FILPATH-MACHINE = R2
              MOVE LOG-PATH-R2 TO LOG-PATH
           ELSE
              MOVE LOG-PATH-R3 TO LOG-PATH.
           PERFORM OPEN-LOG-FILE.

           ADD 1 EXT-CONAME-RPT-LINES GIVING LN-COUNT.

           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.


           IF EXT-ROUTE-BUF = "00"
              MOVE "REPORT IN PROGRESS.................." TO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           PERFORM OPEN-PR-FILE.

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           PERFORM OPEN-WK-FILE-OUTPUT.
           PERFORM CLOSE-WK-FILE.
           PERFORM OPEN-WK-FILE.

           GO TO EXIT-INITIALIZE.
       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.
       EXIT-INITIALIZE.
           EXIT.

       EO-EXEC-AUDIT SECTION.
           MOVE EXT-EOBUF-DATE1 TO BEG-DATE.
           MOVE EXT-EOBUF-DATE2 TO END-DATE.


       ENTER-PARAMETERS SECTION.
       ENTER-PARM.

           IF VDUSTATUS = "1U"
              GO TO ENTER-PRINT-SEQ.

       PR-01.
           MOVE "E  A080PR1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-PRS.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO PR-01.
           MOVE VDUBUF  TO BEG-PRNO.

       PR-02.
           MOVE "E  A080PR2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO PR-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO PR-02.
           MOVE VDUBUF  TO END-PRNO.
           IF BEG-PRNO > END-PRNO
              MOVE "INVALID PR NUMBER RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO PR-01.
           GO TO DT-01.

       ALL-PRS.
           MOVE "S  A080PR1." TO VDU.
           MOVE SPACES TO BEG-PRNO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A080PR2." TO VDU.
           MOVE ALL "Z" TO END-PRNO.
           CALL "C$TOLOWER" USING END-PRNO, VALUE 8.
           MOVE END-PRNO TO VDUBUF.
           PERFORM SCREENIO.

       DT-01.
           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO ENTER-REL.
           MOVE "ERNM060DT1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-DT.
           IF VDUSTATUS = "1U" GO TO PR-02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO DT-01.
           MOVE VDU-DATE-YYYYMMDD TO BEG-DATE.
       DT-02.
           MOVE "ERNM060DT2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO DT-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO DT-02.
           MOVE VDU-DATE-YYYYMMDD TO END-DATE.

           IF END-DATE < BEG-DATE
              MOVE "INVALID DATE RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO DT-01.

           GO TO ENTER-REL.
       ALL-DT.
           MOVE "S  8080DT1." TO VDU.
           MOVE EXT-JULIAN-BEG TO BEG-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
           MOVE "S  8080DT2." TO VDU.
           MOVE EXT-JULIAN-END TO END-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
       ENTER-REL.
           MOVE "E  A030REL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO DT-02.
           IF VDUSTATUS = "16"
              MOVE "***" TO RELEASEX
              GO TO ENTER-PRINT-SEQ.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO ENTER-REL.
           MOVE VDUBUF TO RELEASEX.
           IF RELEASEX NOT = "A15"
              IF RELEASEX NOT = "A99"
                 MOVE "PLEASE ENTER 'A15' OR 'A99'" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-REL.
       ENTER-PRINT-SEQ.
           MOVE "ENNN010ORDER." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ENTER-REL.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO ENTER-PRINT-SEQ.
           MOVE VDUNUM0 TO PRINT-SEQ.
           IF PRINT-SEQ NOT = 1
              IF PRINT-SEQ NOT = 2
                 MOVE "PLEASE ENTER '1' OR '2'" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-PRINT-SEQ.
       END-IT.

       EXIT-PARM.
           EXIT.

       DISPLAY-PARAMETERS SECTION.
       DISPLAY-PARM.
      *PR RANGE- - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN020PR1." TO VDU.
           MOVE BEG-PRNO TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN020PR2." TO VDU.
           MOVE END-PRNO TO VDUNUM0.
           PERFORM SCREENIO.

      *DATE RANGE - - - - - - - - - - - - - - - - - - - -
           MOVE "S  8080DT1." TO VDU.
           MOVE BEG-DATE TO VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

           MOVE "S  8080DT2." TO VDU.
           MOVE END-DATE TO VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

       DISPLAY-PARAMETERS-EXIT.
           EXIT.

      ****************************
      *    WRITE DETAIL LINE     *
      ****************************

       WRITE-DETAIL-LINE SECTION.
       WRITE-DET-LINE.
           ADD LN-FEED LN-COUNT GIVING LN-WK.
           IF LN-WK GREATER THAN EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.
           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      *******************************
      *        HEAD PAGE ROUTINE    *
      *******************************

       HEAD-PAGE SECTION.
       HEAD-ROUTINE.
           ADD 1 TO PAGE-NUMBER.
           MOVE PAGE-NUMBER TO H-PAGE-NO.
           PERFORM GET-TIME.
           MOVE T-HH TO H-HR.
           MOVE T-MM TO H-MIN.

           MOVE HEAD1 TO PR-REC.
           MOVE 99 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE HEAD2 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE HEAD3 TO PR-REC.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE 2 TO LN-FEED.
           MOVE 4 TO LN-COUNT.

      *******************************
      *       RANGE LINES           *
      *******************************

       PRINT-RANGE-LINES SECTION.
       PR-RANGE-LNS.
           MOVE 4 TO LN-FEED.
           MOVE "BEG PRNO"       TO RANGE-LINE.
           MOVE BEG-PRNO         TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "END PRNO"        TO RANGE-LINE.
           MOVE END-PRNO          TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEG DATE"        TO RANGE-LINE.
           MOVE BEG-DATE          TO RANGE-DT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "END DATE"        TO RANGE-LINE.
           MOVE END-DATE          TO RANGE-DT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "REPORT ORDER   " TO RANGE-LINE.
           IF ORDER-BY-PR
              MOVE "BY PR# BY DATE" TO RANGE-SELECTION
           ELSE
              MOVE "BY DATE BY PR#" TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

      *------------------------------------------------------------
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/SCREEN.CPY".

      *******************************
       FORM-RESET SECTION.
      *******************************
           MOVE EXT-CONAME-CONAME TO INSTRP-CONAME.
           MOVE EXT-CONAME-SYSDATE TO INSTRP-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY INSTRP-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE INSTRP-FORM-FIELDS TO WR-BUF.
           MOVE INSTRP-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *******************************
       SETUP-LINK-INFO SECTION.
      *******************************
           MOVE INSTRP-HELP--FILE TO HELP-LINK-FILE.
           MOVE INSTRP-HELP--DIR TO HELP-LINK-DIR.

      *******************************
       VDU-CONVERT-FIELD SECTION.
      *******************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/INSTRP_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *******************************
       MISC-PARA SECTION.
      *******************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/GETENV.CPY".
       CONVERT-MMDDYY-TO-YYMMDD.
           MOVE DATE-MMDDYY-MM TO DATE-YYMMDD-MM.
           MOVE DATE-MMDDYY-DD TO DATE-YYMMDD-DD.
           MOVE DATE-MMDDYY-YY TO DATE-YYMMDD-YY.

      *******************************
       IO-ROUTINE SECTION.
      *******************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       OPEN-LOG-FILE.
           PERFORM OPEN-IT.
           MOVE LOG-PATH TO E-FILE.
           OPEN INPUT LOG-FILE.
           IF IO-FG = 8
             GO TO OPEN-LOG-FILE.
           IF IO-FG = 7
             CLOSE LOG-FILE
             GO TO OPEN-LOG-FILE.
           MOVE SPACES TO LOG-REC.
       READ-LOG-FILE.
           PERFORM READ-IT.
           MOVE LOG-PATH TO E-FILE.
           MOVE SPACES TO E-KEYX.
           READ LOG-FILE.
       CLOSE-LOG-FILE.
           PERFORM CLOSE-IT.
           CLOSE LOG-FILE.
       COPY "LIBGB/GBWKIN.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

       END-PROGRAM SECTION.
       END-RUN.
           PERFORM CLOSE-FILES.
           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.
           IF NO-RECORDS-FOUND
              MOVE "NO RECORDS FOUND FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS.
       EXIT-PROG.
           IF EXT-ROUTE-BUF = "00"
              MOVE "SP/SPINMU" TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY INSTRP-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
