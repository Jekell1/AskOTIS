       IDENTIFICATION DIVISION.
       PROGRAM-ID.   SUMTR2.
       AUTHOR.       MJD.
       DATE-WRITTEN. 010596.
      *****************************************************************
      *                           (GL)
      *                          SUMTR2.C
      *  DESCRIPTION: GL SUMMARY TRIAL BALANCE; BY BRANCH BY ACCOUNT
      *               (CONVERTED TO A90 FROM TIRES 010596 MJD)
      *  REV:
      *  BLV 082297 BROUGHT FROM A90; REPLACES SUMBL2
      *  GLF 090497 REPLACED 'IO' ON GP-FILE WITH GET 'GPENV'
      *  MG  000207 FIXED VDUSTATUS/IO-FG/STAT PER DEVELOPMENT MEETING.
      *  BAH 011109 REMOVED "SORT IN PROGRESS" MESS, IT WAS MUCKING UP
      *             PIGGY BACK PRINTING
      *  BLM 090406 ADD OPTION TO PRINT SPANISH GL DESCRIPTIONS AND
      *             HEADINGS, WORLD PR# 616
      *  BLM 090410 ADD A FEW MORE TRANSLATIONS TO SPANISH HEADINGS,
      *             WORLD PR# 616
      *  BLM 090422 FIX GROUP LOGIC IN SORT ROUTINE, WAS ONLY
      *             PRINTING FIRST BRANCH IN GROUP, WORLD
      *             ALSO WORK ON I-O IN SORT ROUTINE, IF GL-BRANCH
      *             ALSO FIXED I-O IN SORT ROUTINE, WAS READING REST
      *             OF GL FILE AFTER BRANCH > END-BRANCH OR ACCT >
      *             END-ACCOUNT, PUT IN NORMAL GROUP/BRANCH LOGIC
      *  BLM 120111 ADD 1 TO OPENING & CLOSING PRINT FIELDS, INCREASE
      *             ALL WORKERS & TOTALS TO 9(10) FOR BILLIONS,
      *             WORLD PR# 783
      *   CS 120614 REPLACED LACCESS/LACCESSW WITH ACCESS/ACCESSW, USES
      *             C$FILEINFO, LONGER ACCESS-BUF OF 127, PREP FOR NEW
      *             PLATFORM  VERYANT
      *  BLM 170410 ACTIVATE 8 DIGIT DATES
      * BAH 20200827 REMOVED RESET ON FIRST FIELD
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
       COPY "LIBGB/GBFSWKI.CPY".
           SELECT WK2-FILE ASSIGN MKTEMP-BUF
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY WK2-KEY
                  FILE STATUS FILE-STAT.

       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".

      *****************************************************************
      *  CONSOLIDATE BY LAST 7 DIGITS
      *****************************************************************
       FD  WK-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK-REC.
           03  WK-KEY.
               05  WK-ACCOUNT  PIC 9(7).
               05  WK-BRANCH   PIC 9(4).
           03  WK-DESCRIPTION  PIC X(36).
           03  WK-TYPE         PIC X.
           03  WK-CLEAR        PIC X.
           03  WK-OPEN         PIC S9(9)V99.
           03  WK-MONTH        PIC S9(9)V99.
           03  WK-CLOSE        PIC S9(9)V99.
           03  WK-LOPEN        PIC S9(9)V99.
           03  WK-LMONTH       PIC S9(9)V99.
           03  WK-LCLOSE       PIC S9(9)V99.

      ***********************************
      * SORTED GLFILE BY ACCOUNT, BRANCH
      ***********************************
       FD  WK2-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK2-REC.
           03  WK2-KEY.
               05  WK2-GL-ACCOUNT          PIC 9(7).
               05  WK2-GL-BRANCH           PIC 9(4).

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)GL/SUMTR2.CBL S35 06/15/23-11:26:14 >".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBGL/GL01GL.CPY".
       COPY "LIBGL/GL01GL_SQL.CPY".
       COPY "LIBGL/GLWSGL.CPY".
       01  WK2-PATH               PIC X(35).
       01  WK-PATH                PIC X(35).

       01  WK-IFN                 PIC X(2)      VALUE "WK".
       01  WK2-IFN                PIC X(3)      VALUE "WK2".

       01  LN-WORKERS.
           03  LN-FEED            PIC 99        VALUE 0.
           03  LN-COUNT           PIC 99        VALUE 61.
           03  LN-WK              PIC 9(3)      VALUE 0.
       01  PAGE-NUMBER            PIC 9(6)      VALUE 0.

       01  HOLD-BRANCH            PIC 9(4)      VALUE 0.
       01  HOLD-ACCOUNT           PIC 9(7)      VALUE 0.

       01  FIRST-TIME             PIC X         VALUE "Y".
       01  SUB                    PIC 99        VALUE 0.

       01  RECORDS-FOUND          PIC X         VALUE " ".
           88  NO-RECORDS-FOUND                 VALUE "N".

      *----------------------------------------------------------------
      * DETAIL-EXISTS : SET TO "Y" IF DETAIL HAS BEEN PRINTED.
      *                  OR WOULD HAVE BEEN IF SUMMARY ONLY.
      *                  ADDED BECAUSE IF FIRST-TIME WAS "Y" THEN
      *                  TOTALS WERE BEING PRINTED IN END-ROUTINE EVEN
      *                  THOUGH NO RECORDS WERE PRINTED.
      *----------------------------------------------------------------
       01  DETAIL-EXISTS          PIC X         VALUE "N".

       01  DATE1                  PIC 9(8).
       01  FILLER REDEFINES DATE1.
           03  FILLER             PIC 9(6).
           03  DATE1-DD           PIC 9(2).
       01  DATE2                  PIC 9(8).

       COPY "LIBGB/EOBUF_EXT.CPY".
           03  BEG-BRANCH         PIC 9(4).
           03  END-BRANCH         PIC 9(4).
           03  BEG-ACCOUNT        PIC 9(7).
           03  END-ACCOUNT        PIC 9(7).
           03  CURRENT-PERIOD     PIC 99.
           03  GROUP-FLAG         PIC X.
           03  ENT-GROUP          PIC X(4).
           03  CONSOL-FLAG        PIC X.
           03  ZERO-BALS          PIC X.
       01  VERSION-CONTROL        PIC X         VALUE "A".

       01  MESS-LIST              PIC X(5)      VALUE "MESS.".

       01  HEAD1.
           03  H-DATE             PIC X(8).
           03  FILLER             PIC X         VALUE SPACES.
           03  H-TIME             PIC X(8).
           03  FILLER             PIC X(29)     VALUE SPACES.
           03  H-CONAME           PIC X(40).
           03  FILLER             PIC X(35)     VALUE SPACES.
           03  H-PAGE             PIC X(7)      VALUE "PAGE".
           03  H-PAGE-NO          PIC ZZZ9.
       01  HEAD2.
           03  FILLER             PIC X(10)     VALUE "USERID: ".
           03  H-USERID           PIC X(10).
           03  FILLER             PIC X(4)      VALUE SPACE.
           03  H-GROUP-ID         PIC X(12)     VALUE SPACES.
           03  FILLER             PIC X(11)     VALUE SPACES.
           03  FILLER             PIC X(26)     VALUE
               "G/L SUMMARY TRIAL BALANCE".
           03  FILLER             PIC X(11)     VALUE
               "FOR PERIOD ".
           03  MNTH-2             PIC Z9.
           03  FILLER             PIC X(40)     VALUE SPACES.
           03  FILLER             PIC X(6)      VALUE "SUMTR2".
       01  HEAD3.
           03  FILLER             PIC X(39)     VALUE SPACES.
           03  FILLER             PIC X(4)      VALUE "TP  ".
           03  FILLER             PIC X(42)     VALUE
           "- - - - C U R R E N T    Y E A R - - - - -".
           03  FILLER             PIC X(3)      VALUE SPACES.
           03  FILLER             PIC X(42)     VALUE
           "- - - - - - L A S T      Y E A R - - - - -".
       01  HEAD4.
           03  FILLER             PIC X(13)     VALUE
               "ACCOUNT BRN".
           03  FILLER             PIC X(34)     VALUE
               "DESCRIPTION".
           03  FILLER             PIC X(8)      VALUE "OPENING".
           03  FILLER             PIC X(9)      VALUE SPACES.
           03  FILLER             PIC X(5)      VALUE "MONTH".
           03  FILLER             PIC X(8)      VALUE SPACES.
           03  FILLER             PIC X(8)      VALUE "CLOSING".
           03  FILLER             PIC X(8)      VALUE SPACES.
           03  FILLER             PIC X(8)      VALUE "OPENING".
           03  FILLER             PIC X(9)      VALUE SPACES.
           03  FILLER             PIC X(5)      VALUE "MONTH".
           03  FILLER             PIC X(8)      VALUE SPACES.
           03  FILLER             PIC X(8)      VALUE "CLOSING".


       01  UNDER-LINE.
           03  FILLER                  PIC X(47) VALUE SPACES.
           03  FILLER                  PIC X(45) VALUE
               "---------------------------------------------".
           03  FILLER                  PIC X(40) VALUE
               "----------------------------------------".

       01  DETAIL-LINE            PIC X(132).
       01  DET-LINE1  REDEFINES DETAIL-LINE.
           03  D-ACCT             PIC 9(7).
           03  D-DASH             PIC X.
           03  D-BRANCH           PIC 9(4).
           03  FILLER             PIC X.
           03  D-DESC             PIC X(25).
           03  FILLER             PIC X.
           03  D-TYPE             PIC X.
           03  D-OPEN             PIC ZZZZ,ZZZ,ZZ9.99-.
           03  D-MONTH            PIC ZZZZZZ,ZZ9.99-.
           03  D-CLOSE            PIC ZZZZ,ZZZ,ZZ9.99-.
           03  D-LOPEN            PIC ZZZZ,ZZZ,ZZ9.99-.
           03  D-LMONTH           PIC ZZZZZZ,ZZ9.99-.
           03  D-LCLOSE           PIC ZZZZ,ZZZ,ZZ9.99-.
       01  DETAIL-LINE2 REDEFINES DETAIL-LINE.
           03  FILLER             PIC X(4).
           03  D-TOTALX           PIC X(15).
           03  D-ACCT2            PIC 9(7).
           03  FILLER             PIC X(16).
           03  FILLER             PIC X(90).
       01  RANGE-LINE     REDEFINES DETAIL-LINE.
           03  RANGE-DESCRIPTION  PIC X(17).
           03  FILLER             PIC X(2).
           03  RANGE-OPTION       PIC X(113).
           03  RANGE-DATEX REDEFINES RANGE-OPTION.
               05  RANGE-DATE     PIC 9999/99/99.
               05  FILLER         PIC X(103).
           03  RANGE-ACCOUNTX REDEFINES RANGE-OPTION.
               05  RANGE-ACCOUNT  PIC ZZZZZZZ9.
               05  FILLER         PIC X(105).
           03  RANGE-BRANCHX REDEFINES RANGE-OPTION.
               05  FILLER         PIC X(4).
               05  RANGE-BRANCH   PIC 9(4).
               05  RANGE-GROUP REDEFINES RANGE-BRANCH
                                  PIC X(4).
               05  FILLER         PIC X(105).
           03  RANGE-CODEX REDEFINES RANGE-OPTION.
               05 FILLER          PIC X(7).
               05 RANGE-CODE      PIC X.
           03  RANGE-CODENX REDEFINES RANGE-OPTION.
               05 FILLER          PIC X(6).
               05 RANGE-CODEN     PIC Z9.
               05 FILLER          PIC X(105).


       01  WORK-OPEN              PIC S9(10)V99.
       01  WORK-MONTH             PIC S9(10)V99.
       01  WORK-CLOSE             PIC S9(10)V99.
       01  WORK-LOPEN             PIC S9(10)V99.
       01  WORK-LMONTH            PIC S9(10)V99.
       01  WORK-LCLOSE            PIC S9(10)V99.

       01  BRANCH-TOTALS.
           03  BRANCH-OPEN        PIC S9(10)V99.
           03  BRANCH-MONTH       PIC S9(10)V99.
           03  BRANCH-CLOSE       PIC S9(10)V99.
           03  BRANCH-LOPEN       PIC S9(10)V99.
           03  BRANCH-LMONTH      PIC S9(10)V99.
           03  BRANCH-LCLOSE      PIC S9(10)V99.
           03  BRANCH-COUNTER     PIC 9(9).

       01  GRD-TOTALS.
           03  GRD-OPEN           PIC S9(10)V99.
           03  GRD-MONTH          PIC S9(10)V99.
           03  GRD-CLOSE          PIC S9(10)V99.
           03  GRD-LOPEN          PIC S9(10)V99.
           03  GRD-LMONTH         PIC S9(10)V99.
           03  GRD-LCLOSE         PIC S9(10)V99.


       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBWI/PFSWINDOWSW.CPY".
       COPY "LIBWI/TSTINTW.CPY".
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBGL/SUMTR2_DEF.CPY".
       COPY "LIBGL/SUMTR2_WKS.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".
       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBGL/SUMTR2_SCN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               WK-FILE PR-FILE WK2-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-GL1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           CLOSE
               WK-FILE PR-FILE WK2-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      *******************************************
       MAIN-PROGRAM SECTION.
      *******************************************
           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "SUMTR2" TO VDU-FORM-NAME.

           PERFORM GET-GPENV.

           PERFORM INITIALIZATION.

           PERFORM SORT-ROUTINE.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-ROUTINE.

           PERFORM OPEN-WK2-FILE.
           MOVE 0 TO WK2-GL-ACCOUNT WK2-GL-BRANCH.
           PERFORM START-WK2-FILE.
           PERFORM OPEN-GL1-FILE.

       NEXT-GL-MASTER.
           PERFORM TRAP-INTERRUPT.
           PERFORM READ-WK2-FILE-NEXT.
           IF IO-BAD
              GO TO END-ROUTINE.

           MOVE WK2-GL-ACCOUNT TO GL-ACCOUNT.
           MOVE WK2-GL-BRANCH  TO GL-BRANCH.
           PERFORM READ-GL1-FILE.
           IF IO-BAD
              GO TO NEXT-GL-MASTER.

           IF FIRST-TIME = "Y"
              MOVE "N" TO FIRST-TIME
              MOVE GL-BRANCH TO HOLD-BRANCH
              MOVE GL-ACCOUNT TO HOLD-ACCOUNT.

           IF GL-TYPE = "C"
              PERFORM W-REVERSE.
           IF GL-ACCOUNT NOT = HOLD-ACCOUNT
              PERFORM PRINT-ACCT-TOTALS
              MOVE GL-ACCOUNT TO HOLD-ACCOUNT.

           PERFORM MOVE-DETAIL-ROUTINE.
           PERFORM ADD-TOTALS-ROUTINE.

           PERFORM PRINT-DETAIL-ROUTINE.
           PERFORM BUILD-CONSOLIDATED-ACCOUNTS.

           GO TO NEXT-GL-MASTER.

       END-ROUTINE.
           IF FIRST-TIME = "Y" OR DETAIL-EXISTS = "N"
              MOVE "N" TO RECORDS-FOUND
              GO TO END-PROGRAM.

           IF CONSOL-FLAG = "N"
              PERFORM PRINT-ACCT-TOTALS
              PERFORM PRINT-GRAND-TOTALS
              PERFORM PRINT-GL-RANGES
              MOVE " " TO EXT-EOBUF-ERRCD
              GO TO END-PROGRAM.

           PERFORM PRINT-ACCT-TOTALS.
           PERFORM PRINT-GRAND-TOTALS.
           IF BEG-BRANCH = END-BRANCH
              GO TO END-ROUTINE2.
           MOVE EXT-CONAME-RPT-LINES TO LN-COUNT.

           MOVE 9999   TO HOLD-BRANCH.
           MOVE ZEROS  TO GRD-TOTALS.

           MOVE SPACES TO WK-REC.
           PERFORM START-WK-FILE.
       NEXT-CONSOL.
           PERFORM READ-WK-FILE-NEXT.
           IF IO-BAD
              GO TO END-ROUTINE2.

           PERFORM MOVE-CONSOL-ROUTINE.
           PERFORM ADD-TOTALS-ROUTINE.
           PERFORM PRINT-CONSOL-ROUTINE.

           GO TO NEXT-CONSOL.
       END-ROUTINE2.
           PERFORM PRINT-GRAND-TOTALS.
           PERFORM PRINT-GL-RANGES.
           MOVE " " TO EXT-EOBUF-ERRCD.

           GO TO END-PROGRAM.

      *****************************************************************
       PRINT-ACCT-TOTALS SECTION.
      *****************************************************************
           IF ZERO-BALS = "Y"
              IF BRANCH-OPEN = 0
                 AND BRANCH-MONTH = 0
                 AND BRANCH-CLOSE = 0
                 AND BRANCH-LOPEN = 0
                 AND BRANCH-LMONTH = 0
                 AND BRANCH-LCLOSE = 0
                     GO TO PRINT-ACCT-TOTALS-EXIT.

           MOVE UNDER-LINE TO PR-REC
           MOVE 1 TO LN-FEED
           PERFORM WRITE-PR-REC
           ADD LN-FEED TO LN-COUNT.

           MOVE HOLD-ACCOUNT TO D-ACCT2.
           MOVE "ACCOUNT TOTALS" TO D-TOTALX.
           MOVE BRANCH-OPEN TO D-OPEN.
           MOVE BRANCH-MONTH TO D-MONTH.
           MOVE BRANCH-CLOSE TO D-CLOSE.
           MOVE BRANCH-LOPEN TO D-LOPEN.
           MOVE BRANCH-LMONTH TO D-LMONTH.
           MOVE BRANCH-LCLOSE TO D-LCLOSE.
           MOVE 1 TO LN-FEED.
           IF BRANCH-COUNTER > 0
              PERFORM WRITE-DETAIL-LINE
           ELSE
              MOVE SPACES TO DETAIL-LINE.

       PRINT-ACCT-TOTALS-EXIT.
           MOVE 3 TO LN-FEED.
           MOVE ZEROS TO BRANCH-TOTALS.
           MOVE SPACES TO DETAIL-LINE.

      *****************************************************************
       PRINT-GRAND-TOTALS SECTION.
      *****************************************************************
           MOVE "GRAND  TOTALS:" TO D-DESC.
           MOVE GRD-OPEN TO D-OPEN.
           MOVE GRD-MONTH TO D-MONTH.
           MOVE GRD-CLOSE TO D-CLOSE.
           MOVE GRD-LOPEN TO D-LOPEN.
           MOVE GRD-LMONTH TO D-LMONTH.
           MOVE GRD-LCLOSE TO D-LCLOSE.
           MOVE 2 TO LN-FEED.
           IF ZERO-BALS = "Y"
              IF GRD-OPEN = 0
                 AND GRD-MONTH = 0
                 AND GRD-CLOSE = 0
                 AND GRD-LOPEN = 0
                 AND GRD-LMONTH = 0
                 AND GRD-LCLOSE = 0
                     GO TO PRINT-GRAND-TOTALS-EXIT.
           PERFORM WRITE-DETAIL-LINE.
       PRINT-GRAND-TOTALS-EXIT.
           EXIT.

      *****************************************************************
       BUILD-CONSOLIDATED-ACCOUNTS SECTION.
      *****************************************************************

       BUILD-CONSOL-ACCTS.
           IF CONSOL-FLAG = "N"
              GO TO EXIT-BUILD-CONSOL.

           IF BEG-BRANCH = END-BRANCH
              GO TO EXIT-BUILD-CONSOL.

           MOVE GL-ACCOUNT TO WK-ACCOUNT.
           MOVE 0          TO WK-BRANCH.
           PERFORM READ-WK-FILE.
           IF IO-BAD
              MOVE GL-ACCOUNT TO WK-ACCOUNT
              MOVE 0 TO WK-BRANCH
              MOVE GL-DESCRIPTION TO WK-DESCRIPTION
              MOVE GL-TYPE TO WK-TYPE
              MOVE GL-CLEAR TO WK-CLEAR
              MOVE 0 TO WK-OPEN WK-MONTH WK-CLOSE
                        WK-LOPEN WK-LMONTH WK-LCLOSE
              PERFORM WRITE-WK-FILE
              IF IO-BAD
                 GO TO IO-ERROR
              ELSE
                 GO TO BUILD-CONSOL-ACCTS.

           ADD WORK-OPEN TO WK-OPEN.
           ADD WORK-MONTH TO WK-MONTH.
           ADD WORK-CLOSE TO WK-CLOSE.
           ADD WORK-LOPEN TO WK-LOPEN.
           ADD WORK-LMONTH TO WK-LMONTH.
           ADD WORK-LCLOSE TO WK-LCLOSE.
           PERFORM REWRITE-WK-FILE.
           IF IO-BAD
              GO TO IO-ERROR.
       EXIT-BUILD-CONSOL.
           EXIT.

      *****************************************************************
       INITIALIZATION SECTION.
      *****************************************************************
           MOVE SUMTR2-QUEUE-POS TO Q-POS.
           MOVE SUMTR2-COPIES-POS TO C-POS.
           MOVE SUMTR2-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           IF EXT-ROUTE-BUF = "00"
              MOVE "GL SUMMARY REPORT IN PROGRESS ..." TO MESS
              MOVE MESS-LIST TO WR-LIST
              MOVE MESS TO WR-BUF
              PERFORM CALL-WRFORM.


           MOVE EXT-CONAME-CONAME TO H-CONAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE CURRENT-PERIOD TO MNTH-2.

           MOVE ZEROS TO BRANCH-TOTALS GRD-TOTALS.

           IF CONSOL-FLAG = "Y"
              MOVE MKTEMP-DEFAULT TO MKTEMP-BUF
              PERFORM MKTEMP-CALL
              MOVE MKTEMP-BUF TO WK-PATH
              PERFORM OPEN-WK-FILE-OUTPUT
              PERFORM CLOSE-WK-FILE
              PERFORM OPEN-WK-FILE.

           PERFORM OPEN-GL1-FILE.

           PERFORM OPEN-PR-FILE.
           PERFORM ENABLE-INTERRUPT.

      *****************************************************************
       EO-EXEC-AUDIT SECTION.
      *****************************************************************
           MOVE EXT-EOBUF-DATE1 TO DATE1.
           MOVE EXT-EOBUF-DATE2 TO DATE2.
           IF EXT-EOBUF-DATE2 < GP-BEG-FISCAL-DATE
              OR EXT-EOBUF-DATE2 > GP-END-FISCAL-DATE(12)
                 MOVE "UNABLE TO DETERMINE FISCAL PERIOD!" TO MESS
                 PERFORM SEND-MESS
                 GO TO END-PROGRAM.
           MOVE 0 TO CURRENT-PERIOD.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 12
              IF CURRENT-PERIOD = 0
                 IF EXT-EOBUF-DATE2 <= GP-END-FISCAL-DATE(SUB)
                    MOVE SUB TO CURRENT-PERIOD
                 END-IF
              END-IF
           END-PERFORM.

      *****************************************************************
       ENTER-PARAMETERS SECTION.
      *****************************************************************
       ENTER-RANGE.

       PERIOD-01.
           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO BR-01.

           MOVE "ENNN020PERIOD." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO EXIT-PARM.
           IF F7-KEY GO TO EXIT-PARM.
           IF NOT ENTER-KEY GO TO PERIOD-01.
           MOVE VDUNUM0 TO CURRENT-PERIOD.
           IF CURRENT-PERIOD < 1 OR CURRENT-PERIOD > 12
              GO TO PERIOD-01.

           MOVE GP-END-FISCAL-DATE(CURRENT-PERIOD) TO DATE2.
           IF CURRENT-PERIOD = 1
              MOVE GP-BEG-FISCAL-DATE TO DATE1
           ELSE
              MOVE GP-END-FISCAL-DATE(CURRENT-PERIOD) TO DATE1
              MOVE 1 TO DATE1-DD.
      *----------------------------------------------------------------
       DISPLAY-DATES.
           MOVE "S  A000DATE1." TO VDU.
           MOVE DATE1 TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO VDU-DATE-EDIT.
           PERFORM SCREENIO.
           MOVE "S  A000DATE2." TO VDU.
           MOVE DATE2 TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO VDU-DATE-EDIT.
           PERFORM SCREENIO.
           GO TO BR-01.
       BR-01.
           MOVE "E  A000BR1." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO ENTER-RANGE.
           IF UP-KEY GO TO PERIOD-01.
           IF F3-KEY GO TO ALL-BR.
           IF F7-KEY GO TO EXIT-PARM.

           IF F6-KEY
              MOVE GRSCAN-BUF TO SCAN-BUF
              PERFORM CALL-WINDOW-PROGRAM
              MOVE SCAN-BUF TO GRSCAN-BUF
              IF SCAN-STATUS NOT = "00"
                 GO TO BR-01
              ELSE
                 MOVE "S  A000BR1." TO VDU
                 MOVE GRSCAN-KEY TO VDUBUF
                 PERFORM SCREENIO
                 MOVE GRSCAN-KEY TO VDUBUF
                 MOVE "00" TO VDUSTATUS.

           IF NOT ENTER-KEY GO TO BR-01.

           MOVE VDUBUF TO WGR-WORD ENT-GROUP.
           IF WGR-WORD = SPACES
              GO TO BR-01.
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
              GO TO ACCT-01.
           MOVE "N" TO GROUP-FLAG.
           MOVE WGR-SRESULT TO BEG-BRANCH.

           MOVE "SN N040BR1." TO VDU.
           MOVE BEG-BRANCH TO VDUNUM0.
           PERFORM SCREENIO.
      *----------------------------------------------------------------
       BR-02.
           MOVE "ENNN040BR2." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO ENTER-RANGE.
           IF UP-KEY GO TO BR-01.
           IF F3-KEY GO TO ALL-BR.
           IF F7-KEY GO TO EXIT-PARM.
           IF NOT ENTER-KEY GO TO BR-02.
           MOVE VDUNUM0 TO END-BRANCH.
           IF BEG-BRANCH > END-BRANCH
              GO TO BR-01.
           GO TO ACCT-01.
      *----------------------------------------------------------------
       ALL-BR.
           MOVE "N" TO GROUP-FLAG.
           MOVE "SN N040BR1." TO VDU.
           MOVE 0 TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SN N040BR2." TO VDU.
           MOVE 9999 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.
      *----------------------------------------------------------------
       ACCT-01.
           MOVE "ENNN070ACCT1." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO ENTER-RANGE.
           IF UP-KEY GO TO BR-02.
           IF F3-KEY GO TO ALL-ACCTS.
           IF F7-KEY GO TO EXIT-PARM.
           IF NOT ENTER-KEY GO TO ACCT-01.
           MOVE VDUNUM0 TO BEG-ACCOUNT.
      *----------------------------------------------------------------
       ACCT-02.
           MOVE "ENNN070ACCT2." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO ENTER-RANGE.
           IF UP-KEY GO TO ACCT-01.
           IF F7-KEY GO TO EXIT-PARM.
           IF NOT ENTER-KEY GO TO ACCT-02.
           MOVE VDUNUM0 TO END-ACCOUNT.
           IF BEG-ACCOUNT > END-ACCOUNT
              GO TO ACCT-01.
           GO TO BR-CONSOL.
      *----------------------------------------------------------------
       ALL-ACCTS.
           MOVE "SN N070ACCT1." TO VDU.
           MOVE 0 TO BEG-ACCOUNT VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SN N070ACCT2." TO VDU.
           MOVE 9999999 TO END-ACCOUNT VDUNUM0.
           PERFORM SCREENIO.
      *----------------------------------------------------------------
       BR-CONSOL.
           MOVE "E  A000CON." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO ENTER-RANGE.
           IF UP-KEY GO TO ACCT-02.
           IF F7-KEY GO TO EXIT-PARM.
           IF NOT ENTER-KEY GO TO BR-CONSOL.
           MOVE VDUBUF TO CONSOL-FLAG.
           IF CONSOL-FLAG NOT = "Y"
              IF CONSOL-FLAG NOT = "N"
                 GO TO BR-CONSOL.
      *----------------------------------------------------------------
       ZBAL-01.
           MOVE "E  A010ZEROBAL." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO ENTER-RANGE.
           IF UP-KEY GO TO BR-CONSOL.
           IF F7-KEY GO TO EXIT-PARM.
           IF NOT ENTER-KEY GO TO ZBAL-01.
           MOVE VDUBUF TO ZERO-BALS.
           IF NOT ( ZERO-BALS = "Y" OR "N" )
              GO TO ZBAL-01.
       EXIT-PARM.
           EXIT.

      *****************************************************************
       DISPLAY-PARAMETERS SECTION.
      *****************************************************************
           IF GROUP-FLAG = "N"
              MOVE "SNNN040BR1." TO VDU
              MOVE BEG-BRANCH TO VDUNUM0
              PERFORM SCREENIO
              MOVE "SNNN040BR2." TO VDU
              MOVE END-BRANCH TO VDUNUM0
           ELSE
              MOVE "SNNA040BR1." TO VDU
              MOVE ENT-GROUP TO VDUBUF.
           PERFORM SCREENIO.
      *----------------------------------------------------------------
           MOVE "SNNN070ACCT1." TO VDU.
           MOVE BEG-ACCOUNT TO VDUNUM0.
           PERFORM SCREENIO.
      *----------------------------------------------------------------
           MOVE "SNNN070ACCT2." TO VDU.
           MOVE END-ACCOUNT TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "S  A010CON." TO VDU.
           MOVE CONSOL-FLAG TO VDUBUF.
           PERFORM SCREENIO.
      *----------------------------------------------------------------
           MOVE "S  A010ZEROBAL." TO VDU.
           MOVE ZERO-BALS TO VDUBUF.
           PERFORM SCREENIO.

       DISPLAY-PARAMETERS-EXIT.
           EXIT.

      *****************************************************************
       MOVE-DETAIL-ROUTINE SECTION.
      *****************************************************************
           MOVE GL-OPENBAL TO WORK-OPEN.
           MOVE GL-LYOPENBAL TO WORK-LOPEN.
           MOVE GL-CYBAL(CURRENT-PERIOD) TO WORK-MONTH.
           MOVE GL-LYBAL(CURRENT-PERIOD) TO WORK-LMONTH.
           COMPUTE SUB = CURRENT-PERIOD - 1.
           PERFORM CALC-OPENING UNTIL SUB = 0.
           COMPUTE WORK-CLOSE = WORK-OPEN + WORK-MONTH.
           COMPUTE WORK-LCLOSE = WORK-LOPEN + WORK-LMONTH.

      *****************************************************************
       MOVE-CONSOL-ROUTINE SECTION.
      *****************************************************************
           MOVE WK-OPEN TO WORK-OPEN.
           MOVE WK-LOPEN TO WORK-LOPEN.
           MOVE WK-MONTH TO WORK-MONTH.
           MOVE WK-LMONTH TO WORK-LMONTH.
           MOVE WK-CLOSE TO WORK-CLOSE.
           MOVE WK-LCLOSE TO WORK-LCLOSE.

      *****************************************************************
       ADD-TOTALS-ROUTINE SECTION.
      *****************************************************************

      * ADD TO LOCATION/BRANCH TOTALS
           ADD 1 TO BRANCH-COUNTER.
           ADD WORK-OPEN TO BRANCH-OPEN.
           ADD WORK-MONTH TO BRANCH-MONTH.
           ADD WORK-CLOSE TO BRANCH-CLOSE.
           ADD WORK-LOPEN TO BRANCH-LOPEN.
           ADD WORK-LMONTH TO BRANCH-LMONTH.
           ADD WORK-LCLOSE TO BRANCH-LCLOSE.

      * ADD TO GRAND TOTALS
           ADD WORK-OPEN TO GRD-OPEN.
           ADD WORK-MONTH TO GRD-MONTH.
           ADD WORK-CLOSE TO GRD-CLOSE.
           ADD WORK-LOPEN TO GRD-LOPEN.
           ADD WORK-LMONTH TO GRD-LMONTH.
           ADD WORK-LCLOSE TO GRD-LCLOSE.

      *****************************************************************
       PRINT-DETAIL-ROUTINE SECTION.
      *****************************************************************
           IF ZERO-BALS = "Y"
              IF WORK-OPEN = 0
                 AND WORK-MONTH = 0
                 AND WORK-CLOSE = 0
                 AND WORK-LOPEN = 0
                 AND WORK-LMONTH = 0
                 AND WORK-LCLOSE = 0
                    GO TO EXIT-PRINT-DETAIL.

           MOVE GL-BRANCH TO D-BRANCH.
           MOVE "-" TO D-DASH.
           MOVE GL-ACCOUNT TO D-ACCT.
           MOVE GL-DESCRIPTION TO D-DESC.
           MOVE GL-TYPE TO D-TYPE.

           MOVE WORK-OPEN TO D-OPEN.
           MOVE WORK-MONTH TO D-MONTH.
           MOVE WORK-CLOSE TO D-CLOSE.

           MOVE WORK-LOPEN TO D-LOPEN.
           MOVE WORK-LMONTH TO D-LMONTH.
           MOVE WORK-LCLOSE TO D-LCLOSE.

           MOVE "Y" TO DETAIL-EXISTS.
           PERFORM WRITE-DETAIL-LINE.

       EXIT-PRINT-DETAIL.
           EXIT.

      *****************************************************************
       PRINT-CONSOL-ROUTINE SECTION.
      *****************************************************************

           IF ZERO-BALS = "Y"
              IF WORK-OPEN = 0
                 AND WORK-MONTH = 0
                 AND WORK-CLOSE = 0
                 AND WORK-LOPEN = 0
                 AND WORK-LMONTH = 0
                 AND WORK-LCLOSE = 0
                    GO TO EXIT-PRINT-CONSOL.

           MOVE WK-ACCOUNT TO D-ACCT.
           MOVE WK-DESCRIPTION TO D-DESC.
           MOVE WK-TYPE TO D-TYPE.

           MOVE WORK-OPEN TO D-OPEN.
           MOVE WORK-MONTH TO D-MONTH.
           MOVE WORK-CLOSE TO D-CLOSE.

           MOVE WORK-LOPEN TO D-LOPEN.
           MOVE WORK-LMONTH TO D-LMONTH.
           MOVE WORK-LCLOSE TO D-LCLOSE.

           PERFORM WRITE-DETAIL-LINE.

       EXIT-PRINT-CONSOL.
           EXIT.

      *****************************************************************
       W-REVERSE SECTION.
      *****************************************************************
           MULTIPLY -1 BY GL-OPENBAL.
           MULTIPLY -1 BY GL-LYOPENBAL.
           MOVE 1 TO SUB.
       REVERSE-LOOP.
           MULTIPLY -1 BY GL-CYBAL(SUB).
           MULTIPLY -1 BY GL-LYBAL(SUB).
           ADD 1 TO SUB.
           IF SUB < 13
              GO TO REVERSE-LOOP.

      *****************************************************************
       WRITE-DETAIL-LINE SECTION.
      *****************************************************************
           ADD LN-FEED LN-COUNT GIVING LN-WK.
           IF LN-WK > EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.
           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      *****************************************************************
       HEAD-PAGE SECTION.
      *****************************************************************
           ADD 1 TO PAGE-NUMBER.
           MOVE PAGE-NUMBER TO H-PAGE-NO.
           PERFORM GET-TIME.
           MOVE TIME-EDIT TO H-TIME.
           MOVE EXT-CONAME-CONAME TO H-CONAME.
           MOVE EXT-CONAME-USER-ID TO H-USERID.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
      *----------------------------------------------------------------
           MOVE HEAD1 TO PR-REC.
           MOVE 99 TO LN-FEED.
           PERFORM WRITE-PR-REC.
      *----------------------------------------------------------------
           MOVE HOLD-BRANCH TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-BAD
              MOVE SPACES TO H-GROUP-ID BR-NAME.
           IF HOLD-BRANCH = 9999
              MOVE "CONSOLIDATED" TO H-GROUP-ID BR-NAME.
           MOVE HEAD2 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
      *----------------------------------------------------------------
           MOVE HEAD3 TO PR-REC.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.
      *----------------------------------------------------------------
           MOVE HEAD4 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
      *----------------------------------------------------------------
           MOVE 2 TO LN-FEED.
           MOVE 4 TO LN-COUNT.

      *****************************************************************
       PRINT-GL-RANGES SECTION.
      *****************************************************************

           MOVE 5 TO LN-FEED.
           MOVE "PERIOD          :" TO RANGE-DESCRIPTION.
           MOVE CURRENT-PERIOD      TO RANGE-CODEN.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "BEGINNING DATE  :" TO RANGE-DESCRIPTION.
           MOVE DATE1               TO RANGE-DATE.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "ENDING DATE     :" TO RANGE-DESCRIPTION.
           MOVE DATE2               TO RANGE-DATE.
           PERFORM WRITE-DETAIL-LINE.
      
           IF GROUP-FLAG = "Y"
              MOVE "GROUP RANGE     :" TO RANGE-DESCRIPTION
              MOVE ENT-GROUP          TO RANGE-GROUP
           ELSE
              MOVE "BEGINNNG BRANCH :" TO RANGE-DESCRIPTION
              MOVE BEG-BRANCH          TO RANGE-BRANCH
              PERFORM WRITE-DETAIL-LINE
              MOVE "ENDING BRANCH   :" TO RANGE-DESCRIPTION
              MOVE END-BRANCH          TO RANGE-BRANCH.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "BEG ACCOUNT     :" TO RANGE-DESCRIPTION.
           MOVE BEG-ACCOUNT         TO RANGE-ACCOUNT.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "END ACCOUNT     :" TO RANGE-DESCRIPTION.
           MOVE END-ACCOUNT         TO RANGE-ACCOUNT.
           PERFORM WRITE-DETAIL-LINE.
    
           MOVE "CONSOLIDATE     :" TO RANGE-DESCRIPTION.
           MOVE CONSOL-FLAG         TO RANGE-CODE.
           PERFORM WRITE-DETAIL-LINE.
     
           MOVE "SKIP ZERO BALS  :" TO RANGE-DESCRIPTION.
           MOVE ZERO-BALS           TO RANGE-CODE.
           PERFORM WRITE-DETAIL-LINE.

      *****************************************************************
       SORT-ROUTINE SECTION.
      *****************************************************************

      * CREATE WORK KEY FILE FOR GLFILE OUTPUT
           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           IF STAT-BAD
              MOVE "X" TO EXT-EOBUF-ERRCD
              GO TO EXIT-SORT-ROUTINE.
           MOVE MKTEMP-BUF TO WK2-PATH.
           PERFORM OPEN-WK2-FILE-OUTPUT.

           PERFORM OPEN-GL1-FILE.
           PERFORM OPEN-BR-FILE.

           IF GROUP-FLAG = "N"
              MOVE BEG-BRANCH TO BR-NO
              PERFORM START-BR-FILE
              GO TO NEXT-BRANCH-NO.

           PERFORM OPEN-GR1-FILE.
           MOVE ENT-GROUP TO GR-ID.
           PERFORM SEARCH-GR-INIT.

       NEXT-GROUP-LOC.
           IF GROUP-FLAG NOT = "Y"
              GO TO NEXT-BRANCH-NO.

           PERFORM SEARCH-GR-NEXT.
           IF WGR-SRESULT = 0
              GO TO SORT-OUT.

           MOVE WGR-SRESULT TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

           GO TO START-GL-MASTER.
       NEXT-BRANCH-NO.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-BAD OR BR-NO > END-BRANCH
              GO TO SORT-OUT.

       START-GL-MASTER.
           MOVE BR-NO       TO GL-BRANCH
                               QGL1-WBEG-BRANCH
                               QGL1-WEND-BRANCH.
           MOVE BEG-ACCOUNT TO GL-ACCOUNT
                               QGL1-WBEG-ACCOUNT.
           MOVE END-ACCOUNT TO QGL1-WEND-ACCOUNT.

           PERFORM START-GL1-FILE.

       SORT-GL-NEXT.
           PERFORM READ-GL1-FILE-NEXT.
           IF IO-BAD
              GO TO NEXT-GROUP-LOC.

           IF GL-BRANCH > BR-NO
              GO TO NEXT-GROUP-LOC.
           IF GL-ACCOUNT > END-ACCOUNT
              GO TO NEXT-GROUP-LOC.

           MOVE GL-ACCOUNT TO WK2-GL-ACCOUNT.
           MOVE GL-BRANCH TO WK2-GL-BRANCH.
           PERFORM WRITE-WK2-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

           GO TO SORT-GL-NEXT.
       SORT-OUT.
           PERFORM CLOSE-WK2-FILE.
           PERFORM CLOSE-GL1-FILE.

       EXIT-SORT-ROUTINE.
           EXIT.

      **********************************************************
       FORM-RESET SECTION.
      **********************************************************
           MOVE EXT-CONAME-CONAME TO SUMTR2-CONAME.
           MOVE EXT-CONAME-SYSDATE TO SUMTR2-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY SUMTR2-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE SUMTR2-FORM-FIELDS TO WR-BUF.
           MOVE SUMTR2-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **********************************************************
       SETUP-LINK-INFO SECTION.
      **********************************************************
           MOVE SUMTR2-HELP--FILE TO HELP-LINK-FILE.
           MOVE SUMTR2-HELP--DIR  TO HELP-LINK-DIR.

      **********************************************************
       VDU-CONVERT-FIELD SECTION.
      **********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBGL/SUMTR2_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBWI/PFSWINDOWS.CPY".

      *****************************************************************
       MISC SECTION.
      *****************************************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/RMFILE.CPY".
       CALC-OPENING.
           ADD GL-CYBAL(SUB) TO WORK-OPEN.
           ADD GL-LYBAL(SUB) TO WORK-LOPEN.
           SUBTRACT 1 FROM SUB.

      *****************************************************************
       IO-ROUTINES SECTION.
      *****************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGL/GLGLGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBWKIN.CPY".
       COPY "LIBGB/GBWK2IN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
	   
      * COPY "LIBGB/GBBRRN.CPY".
      * COPYMEMBER: LIBGB/GBBRRN
      * 20220923 BAH ADDED BR_VERITEC_LICENSE #1570
      * 20230419 BAH ADDED BR-TEST-PP-AUTO-INSCOVR #1594A
       LOAD-BR-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( BR-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( BR-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO BR-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO BR-ALL-MACHINE
              MOVE BR-ALL-PATH             TO BR-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( BR-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO BR-ALL-BASE
              MOVE FILPATH-MACHINE         TO BR-ALL-MACHINE
              MOVE BR-ALL-PATH             TO BR-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-BRFILE NOT = "S" )
              MOVE BR-PATH-BEG             TO BR-SELECT-PATH-BEG
              MOVE BR-PATH-FILE            TO BR-SELECT-PATH-FILE
           ELSE
              MOVE BR-PATH-SQL             TO BR-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-BR-FILE.
           PERFORM LOAD-BR-FILE.
           PERFORM OPEN-IT.
           MOVE BR-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-BR-FILE.
           PERFORM START-IT.
           MOVE BR-SELECT-PATH TO E-FILE.
           MOVE BR-NO TO E-KEYX.

           IF ( BR-CURSOR-STAT-GOOD )
              PERFORM CLOSE-BR-FILE.

           IF ( QBR-WEND-NO = ZEROES    ) OR
              ( QBR-WEND-NO NOT NUMERIC )
              MOVE BR-NO                TO QBR-WBEG-NO
              MOVE ALL "9"              TO QBR-WEND-NO.

           EXEC SQL
            DECLARE BR_CURSOR CURSOR FOR
            SELECT BRFILE.BR_NO,
                   BRFILE.BR_NAME
            FROM DBO.BRFILE
            WHERE BRFILE.BR_NO >= :QBR-WBEG-NO
              AND BRFILE.BR_NO <= :QBR-WEND-NO
            ORDER BY BR_NO
           END-EXEC.

           EXEC SQL
               OPEN BR_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QBR-WBEG-NO
                          QBR-WEND-NO.

           MOVE STAT---GOOD TO BR-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-BR-FILE.
           PERFORM READ-IT.
           MOVE BR-SELECT-PATH TO E-FILE.
           MOVE BR-NO TO E-KEYX.

           MOVE BR-NO TO QBR-NO.

           EXEC SQL
            SELECT BRFILE.BR_NO,
                   BRFILE.BR_NAME
              INTO 
                  :QBR-NO, 
                  :QBR-NAME
              FROM DBO.BRFILE
            WHERE BRFILE.BR_NO = :QBR-NO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO READ-BR-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-BR-FIELDS.

      *-----------------------------------------------------------------
       READ-BR-FILE-NEXT.
           PERFORM READ-IT.
           MOVE BR-SELECT-PATH TO E-FILE.
           MOVE BR-NO TO E-KEYX.
           INITIALIZE QBR-REC.

           IF ( BR-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH BR_CURSOR INTO 
                    :QBR-NO, 
                    :QBR-NAME
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO READ-BR-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-BR-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-BR-FILE.
           PERFORM CLOSE-IT.
           IF ( BR-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE BR_CURSOR
              END-EXEC
              MOVE STAT---BAD TO BR-CURSOR-STAT.
 


       COPY "LIBGB/GBPRINT.CPY".
      * COPY "LIBGL/GLGL1RN.CPY".
      * COPYMEMBER: LIBGL/GLGL1RN
       LOAD-GL1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( GL-PATH-OVERRIDE = "Y" )
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( GL-PATH-OVERRIDE = SPACES )
              MOVE EXT-FILPATH-BASE        TO GL-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO GL-ALL-MACHINE
              MOVE GL-ALL-PATH             TO GL-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( GL-PATH-OVERRIDE = "B" )
              MOVE FILPATH-BASE            TO GL-ALL-BASE
              MOVE FILPATH-MACHINE         TO GL-ALL-MACHINE
              MOVE GL-ALL-PATH             TO GL-PATH.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-GLFILE NOT = "S" )
              MOVE GL-PATH-BEG             TO GL-SELECT-PATH-BEG
              MOVE GL-PATH-FILE            TO GL-SELECT-PATH-FILE
           ELSE
              MOVE GL-PATH-SQL             TO GL-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-GL1-FILE.
           PERFORM LOAD-GL1-FILE.
           PERFORM OPEN-IT.
           MOVE GL-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-GL1-FILE.
           PERFORM START-IT.
           MOVE GL-SELECT-PATH TO E-FILE.
           MOVE GL1-KEY TO E-KEYX.

           IF ( GL1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-GL1-FILE.

           IF ( QGL1-WEND-ACCOUNT = ZEROES    ) OR
              ( QGL1-WEND-ACCOUNT NOT NUMERIC )
              MOVE GL-BRANCH        TO QGL1-WBEG-BRANCH
              MOVE GL-ACCOUNT       TO QGL1-WBEG-ACCOUNT
              MOVE ALL "9"          TO QGL1-WEND-BRANCH
                                       QGL1-WEND-ACCOUNT.

           EXEC SQL
            DECLARE GL1_CURSOR CURSOR FOR
            SELECT GLFILE.GL_BR_ACCOUNT,
                   GLFILE.GL_DESCRIPTION,
                   GLFILE.GL_OPENBAL,
                   GLFILE.GL_CYBAL_1,
                   GLFILE.GL_CYBAL_2,
                   GLFILE.GL_CYBAL_3,
                   GLFILE.GL_CYBAL_4,
                   GLFILE.GL_CYBAL_5,
                   GLFILE.GL_CYBAL_6,
                   GLFILE.GL_CYBAL_7,
                   GLFILE.GL_CYBAL_8,
                   GLFILE.GL_CYBAL_9,
                   GLFILE.GL_CYBAL_10,
                   GLFILE.GL_CYBAL_11,
                   GLFILE.GL_CYBAL_12,
                   GLFILE.GL_LYBAL_1,
                   GLFILE.GL_LYBAL_2,
                   GLFILE.GL_LYBAL_3,
                   GLFILE.GL_LYBAL_4,
                   GLFILE.GL_LYBAL_5,
                   GLFILE.GL_LYBAL_6,
                   GLFILE.GL_LYBAL_7,
                   GLFILE.GL_LYBAL_8,
                   GLFILE.GL_LYBAL_9,
                   GLFILE.GL_LYBAL_10,
                   GLFILE.GL_LYBAL_11,
                   GLFILE.GL_LYBAL_12,
                   GLFILE.GL_TYPE,
                   GLFILE.GL_CLEAR,
                   GLFILE.GL_LYOPENBAL
            FROM DBO.GLFILE
            WHERE GLFILE.GL_BR_ACCOUNT >= :QGL1-WBEG-BR-ACCOUNT
              AND GLFILE.GL_BR_ACCOUNT <= :QGL1-WEND-BR-ACCOUNT
            ORDER BY GLFILE.GL_BR_ACCOUNT
           END-EXEC.

           EXEC SQL
               OPEN GL1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QGL1-WBEG-BRANCH
                          QGL1-WBEG-ACCOUNT
                          QGL1-WEND-BRANCH
                          QGL1-WEND-ACCOUNT.

           MOVE STAT---GOOD TO GL1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-GL1-FILE.
           PERFORM READ-IT.
           MOVE GL-SELECT-PATH TO E-FILE.
           MOVE GL1-KEY TO E-KEYX.

           MOVE GL-BR-ACCOUNT TO QGL-BR-ACCOUNT.

           EXEC SQL
            SELECT GLFILE.GL_BR_ACCOUNT,
                   GLFILE.GL_DESCRIPTION,
                   GLFILE.GL_OPENBAL,
                   GLFILE.GL_CYBAL_1,
                   GLFILE.GL_CYBAL_2,
                   GLFILE.GL_CYBAL_3,
                   GLFILE.GL_CYBAL_4,
                   GLFILE.GL_CYBAL_5,
                   GLFILE.GL_CYBAL_6,
                   GLFILE.GL_CYBAL_7,
                   GLFILE.GL_CYBAL_8,
                   GLFILE.GL_CYBAL_9,
                   GLFILE.GL_CYBAL_10,
                   GLFILE.GL_CYBAL_11,
                   GLFILE.GL_CYBAL_12,
                   GLFILE.GL_LYBAL_1,
                   GLFILE.GL_LYBAL_2,
                   GLFILE.GL_LYBAL_3,
                   GLFILE.GL_LYBAL_4,
                   GLFILE.GL_LYBAL_5,
                   GLFILE.GL_LYBAL_6,
                   GLFILE.GL_LYBAL_7,
                   GLFILE.GL_LYBAL_8,
                   GLFILE.GL_LYBAL_9,
                   GLFILE.GL_LYBAL_10,
                   GLFILE.GL_LYBAL_11,
                   GLFILE.GL_LYBAL_12,
                   GLFILE.GL_TYPE,
                   GLFILE.GL_CLEAR,
                   GLFILE.GL_LYOPENBAL
              INTO  
                  :QGL-BR-ACCOUNT,
                  :QGL-DESCRIPTION,
                  :QGL-OPENBAL,
                  :QGL-CYBAL-1,
                  :QGL-CYBAL-2,
                  :QGL-CYBAL-3,
                  :QGL-CYBAL-4,
                  :QGL-CYBAL-5,
                  :QGL-CYBAL-6,
                  :QGL-CYBAL-7,
                  :QGL-CYBAL-8,
                  :QGL-CYBAL-9,
                  :QGL-CYBAL-10,
                  :QGL-CYBAL-11,
                  :QGL-CYBAL-12,
                  :QGL-LYBAL-1,
                  :QGL-LYBAL-2,
                  :QGL-LYBAL-3,
                  :QGL-LYBAL-4,
                  :QGL-LYBAL-5,
                  :QGL-LYBAL-6,
                  :QGL-LYBAL-7,
                  :QGL-LYBAL-8,
                  :QGL-LYBAL-9,
                  :QGL-LYBAL-10,
                  :QGL-LYBAL-11,
                  :QGL-LYBAL-12,
                  :QGL-TYPE,
                  :QGL-CLEAR,
                  :QGL-LYOPENBAL
              FROM DBO.GLFILE
            WHERE GLFILE.GL_BR_ACCOUNT = :QGL-BR-ACCOUNT
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-GL1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-GL-FIELDS.

      *-----------------------------------------------------------------
       READ-GL1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE GL-SELECT-PATH TO E-FILE.
           MOVE GL1-KEY TO E-KEYX.
           INITIALIZE QGL-REC.

           IF ( GL1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH GL1_CURSOR INTO 
                     :QGL-BR-ACCOUNT,
                     :QGL-DESCRIPTION,
                     :QGL-OPENBAL,
                     :QGL-CYBAL-1,
                     :QGL-CYBAL-2,
                     :QGL-CYBAL-3,
                     :QGL-CYBAL-4,
                     :QGL-CYBAL-5,
                     :QGL-CYBAL-6,
                     :QGL-CYBAL-7,
                     :QGL-CYBAL-8,
                     :QGL-CYBAL-9,
                     :QGL-CYBAL-10,
                     :QGL-CYBAL-11,
                     :QGL-CYBAL-12,
                     :QGL-LYBAL-1,
                     :QGL-LYBAL-2,
                     :QGL-LYBAL-3,
                     :QGL-LYBAL-4,
                     :QGL-LYBAL-5,
                     :QGL-LYBAL-6,
                     :QGL-LYBAL-7,
                     :QGL-LYBAL-8,
                     :QGL-LYBAL-9,
                     :QGL-LYBAL-10,
                     :QGL-LYBAL-11,
                     :QGL-LYBAL-12,
                     :QGL-TYPE,
                     :QGL-CLEAR,
                     :QGL-LYOPENBAL
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-GL1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-GL-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-GL1-FILE.
           PERFORM CLOSE-IT.
           IF ( GL1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE GL1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO GL1-CURSOR-STAT.


   
       COPY "LIBGB/FERRORS.CPY".
            MOVE "E" TO EXT-EOBUF-ERRCD.

      *****************************************************************
       END-PROGRAM SECTION.
      *****************************************************************
       END-PROG.
           PERFORM CLOSE-FILES.
           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.
           IF CONSOL-FLAG = "Y"
              MOVE WK-PATH TO ACCESS-BUF
              PERFORM REMOVE-WORKFILE.

           MOVE WK2-PATH TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.

           IF NO-RECORDS-FOUND
              MOVE "NO RECORDS FOUND FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS.
       EXIT-PROG.
           IF EXT-ROUTE-BUF = "00"
              MOVE EXIT-PATHNAME TO FORM-PATHNAME.
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY SUMTR2-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
