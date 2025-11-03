       IDENTIFICATION DIVISION.
       PROGRAM-ID.      MPOOL1.
      **************************************************************************
      *    MERCURY FINANCE 
      *
      * READ IN A FIXED LENGTH FILE FROM STEVE HAILEY, AND SET LN-DLPOOL
      * FILE HAS 0A ON END OF EACH RECORD
      * FILE=../../ED/SETPOOL   GLOBAL ED
      *
      * REV:
      **************************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
           SELECT SETPOOL-FILE ASSIGN TO SET-PATH
                           ORGANIZATION LINE SEQUENTIAL
                           ACCESS SEQUENTIAL
                           LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                           FILE STATUS FILE-STAT.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".

       FD  SETPOOL-FILE
           LABEL RECORDS ARE STANDARD.
       01  SETPOOL-REC.
           03  SETPOOL-BRNO        PIC 9(4).
           03  SETPOOL-BRNOX REDEFINES SETPOOL-BRNO PIC X(4).
           03  SETPOOL-NUMBER      PIC 9(6).
           03  SETPOOL-NUMBERX REDEFINES SETPOOL-NUMBER PIC X(6).
           03  SETPOOL-DLPOOL      PIC 9(5).       



       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)FX/MPOOL1.CBL S34 06/17/21-08:07:27 >".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".

      ******************************************************************
      *
      *    PROGRAM WORKERS
      *
      ******************************************************************
       01  HOLD-BRNO            PIC 9(4) VALUE 0.

       01  SET-PATH.
           03  SET-PATH-BASE       PIC X(9).
           03  FILLER              PIC X VALUE "/".
           03  SET-PATH-MACHINE    PIC X(2).
           03  FILLER              PIC X(11) VALUE "/ED/SETPOOL".
           
       01  FIRST-TIME           PIC X VALUE "Y".

       01  LN-WORKERS.
           03  LN-FEED          PIC 99     VALUE 0.
           03  LN-COUNT         PIC 999    VALUE 99.
           03  LN-WK            PIC 999    VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)   VALUE 0.
 
       01  SETPOOL-COUNTER      PIC 9(6)   VALUE 0 COMP-3.
       01  MISSING-COUNTER      PIC 9(6)   VALUE 0 COMP-3.
       01  REWRITE-COUNTER      PIC 9(6)   VALUE 0 COMP-3.
       01  BAD-BR-COUNTER       PIC 9(6)   VALUE 0 COMP-3.
       01  ACCESS-COUNTER       PIC 9(6)   VALUE 0 COMP-3.
 
       01  ERRCD            PIC X.

       01  ACCEPT-BUF       PIC X     VALUE SPACES.
       01  REPORT-ONLY      PIC X     VALUE SPACES.

       01  HOLD-SYSDATE    PIC 9(8).
 
       01  MESS-DISP-LIST    PIC X(4) VALUE "MES.".
       01  MESS-DISPLAY.
           03  MESS-TITLE       PIC X(21)  VALUE SPACES.

      *----------------------------------------------------------------
       01  HEAD1.
           03  H-DATE               PIC X(8) VALUE SPACES.
           03  FILLER               PIC XX   VALUE SPACES.
           03  H-HR                 PIC 99   VALUE 0.
           03  FILLER               PIC X    VALUE ":".
           03  H-MIN                PIC 99   VALUE 0.
           03  FILLER               PIC X(10) VALUE SPACES.
           03  H-NAME               PIC X(25).
           03  FILLER               PIC X(10).
           03  FILLER               PIC X(5)  VALUE "PAGE ".
           03  H-PAGE-NO            PIC ZZZ9.
       01  HEAD2.
           03  FILLER               PIC X(10) VALUE "USER ID:".
           03  H-USERID             PIC X(10).
           03  FILLER               PIC X(5) VALUE SPACES.
           03  H-TITLE              PIC X(40) VALUE
               "NEW POOL EXCEPTION LISTING".

 
       01  DETAIL-LINE              PIC X(80).
       01  DETAIL-LINE1 REDEFINES DETAIL-LINE.
           03  D-BRNO        PIC 9(4).
           03  FILLER        PIC X(2).
           03  D-NUMBER      PIC Z(6).
           03  FILLER        PIC X(2).
           03  D-POOL        PIC Z(5).
           03  FILLER        PIC X(2).
           03  D-TEXT        PIC X(20).

       01  DETAIL-LINE2 REDEFINES DETAIL-LINE.
           03  FILLER        PIC X(20).
           03  R-COUNTERX    PIC X(28).
           03  R-COUNTER     PIC Z(8)9.99.
           03  R-COUNTER-TEXT REDEFINES R-COUNTER PIC X(12).
           03  FILLER        PIC X(2).
 
 
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       01  MPOOL1-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBFX/MPOOL1_DEF.CPY".
       COPY "LIBFX/MPOOL1_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBFX/MPOOL1_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
              PR-FILE SETPOOL-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-BR-FILE.
           CLOSE
              PR-FILE SETPOOL-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      ******************************************************************
      *
      *    M A I N - P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "MPOOL1" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS MPOOL1-WINDOW-HANDLE.

       COPY "LIBGB/CHKSEC.CPY".

           PERFORM FORM-RESET.

           MOVE EXT-FILPATH-BASE    TO SET-PATH-BASE.
           MOVE EXT-FILPATH-MACHINE TO SET-PATH-MACHINE.
           MOVE SET-PATH            TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT NOT = "00"
              MOVE "../../ED/SETPOOL DOES NOT EXIST" TO MESS
              PERFORM SEND-MESS
              GO TO END-PROGRAM.

           PERFORM INITIALIZATION.
           IF ( ERRCD NOT = SPACES )
               GO TO END-PROGRAM.

           PERFORM SET-NEW-POOL.

           MOVE "N" TO FIRST-TIME.

           PERFORM PRINT-TOTALS.

           GO TO END-PROGRAM.

      *****************************
       SET-NEW-POOL SECTION.
      *****************************

           PERFORM OPEN-SETPOOL-FILE.

           MOVE "PROCESSING FILE:" TO MESS-TITLE.
           MOVE MESS-DISPLAY       TO WR-BUF.
           MOVE MESS-DISP-LIST     TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE 0 TO HOLD-BRNO.
           MOVE 0 TO SETPOOL-COUNTER.
       NEXT-SETPOOL.
           PERFORM READ-SETPOOL-FILE.
           IF IO-FG NOT = 0 
              PERFORM CLOSE-LN1-FILE
              PERFORM CLOSE-SETPOOL-FILE
              GO TO EXIT-SET-NEW-POOL.
 
           ADD 1 TO SETPOOL-COUNTER.

           INSPECT SETPOOL-BRNOX REPLACING ALL " " BY "0".
           IF SETPOOL-BRNOX NOT NUMERIC
              GO TO NEXT-SETPOOL.

           INSPECT SETPOOL-NUMBERX REPLACING ALL " " BY "0".
           IF SETPOOL-NUMBERX NOT NUMERIC
              GO TO NEXT-SETPOOL.

           IF HOLD-BRNO NOT = SETPOOL-BRNO
              PERFORM CLOSE-LN1-FILE
              MOVE SETPOOL-BRNO TO HOLD-BRNO BR-NO
              PERFORM READ-BR-FILE
              IF IO-FG NOT = 0
                 ADD 1 TO BAD-BR-COUNTER
                 MOVE 0 TO HOLD-BRNO
                 MOVE SETPOOL-BRNO TO D-BRNO
                 MOVE "INVALID BRANCH" TO D-TEXT
                 PERFORM WRITE-DETAIL-LINE
                 GO TO NEXT-SETPOOL
              ELSE
                 MOVE EXT-FILPATH-BASE TO FILPATH-BASE
                 MOVE BR-MACHINE       TO FILPATH-MACHINE
                 MOVE BR-DATA-PATH     TO FILPATH-DATA
                 MOVE "B"              TO LN-PATH-OVERRIDE
                 PERFORM OPEN-LN1-FILE.


           MOVE SETPOOL-BRNO   TO LN-OWNBR.
           MOVE SETPOOL-NUMBER TO LN-ACCTNO.
           PERFORM READ-LN1-FILE.
           IF IO-FG NOT = 0
              ADD 1 TO MISSING-COUNTER
              MOVE BR-NO          TO D-BRNO
              MOVE SETPOOL-NUMBER TO D-NUMBER
              MOVE SETPOOL-DLPOOL TO D-POOL
              MOVE "INVALID ACCOUNT NO." TO D-TEXT
              PERFORM WRITE-DETAIL-LINE
              GO TO NEXT-SETPOOL.

           MOVE SETPOOL-DLPOOL TO LN-DLPOOL.

           IF REPORT-ONLY = "N"
              PERFORM REWRITE-LN1-FILE
              ADD 1 TO REWRITE-COUNTER.

           GO TO NEXT-SETPOOL.

       EXIT-SET-NEW-POOL.
           EXIT.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************
           MOVE " " TO ERRCD.

       GETPR.
           MOVE MPOOL1-QUEUE-POS TO Q-POS.
           MOVE MPOOL1-COPIES-POS TO C-POS.
           MOVE MPOOL1-PRU-POS TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
           PERFORM PRU-IN.
           IF STAT NOT = "00"
              GO TO ENDIT.

       REPORT-YN.
           MOVE "E  A010REPORT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO GETPR.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" GO TO REPORT-YN.
           MOVE VDUBUF TO REPORT-ONLY.
           IF NOT (REPORT-ONLY = "Y" OR "N")
              GO TO REPORT-YN.

       ENTER-ACCEPT.
           MOVE "E  A010ACCEPT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO GETPR.
           IF VDUSTATUS = "1U" GO TO REPORT-YN.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" GO TO ENTER-ACCEPT.
           MOVE VDUBUF TO ACCEPT-BUF.
           IF ACCEPT-BUF = "N"
              GO TO ENDIT.
           IF ACCEPT-BUF NOT = "Y"
              GO TO ENTER-ACCEPT.

           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO HOLD-SYSDATE.

           MOVE SPACES TO DETAIL-LINE.
           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-PR-FILE.

           GO TO EXIT-INITIALIZE.
       ENDIT.
           MOVE "X" TO ERRCD.
       EXIT-INITIALIZE.
           EXIT.
 

      **************************************
       WRITE-DETAIL-LINE SECTION.
      **************************************
           ADD LN-COUNT LN-FEED GIVING LN-WK.
           IF LN-WK > EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.
           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      **************************************
       HEAD-PAGE SECTION.
      **************************************
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

       END-HEAD-PAGE.
           MOVE 4 TO LN-FEED.
           MOVE 2 TO LN-COUNT.

      ****************************************
       PRINT-TOTALS SECTION.
      ****************************************
           MOVE "REPORT ONLY ? " TO R-COUNTERX.
           MOVE REPORT-ONLY TO R-COUNTER-TEXT.
           MOVE 4 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "TOTAL RECORDS IN SETPOOL......." TO R-COUNTERX.
           MOVE SETPOOL-COUNTER TO R-COUNTER.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BAD ACCOUNT NUMBERS............" TO R-COUNTERX.
           MOVE MISSING-COUNTER TO R-COUNTER.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "LOAN RECORDS CHANGED..........." TO R-COUNTERX.
           MOVE REWRITE-COUNTER TO R-COUNTER.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "INVALID BRANCH................." TO R-COUNTERX.
           MOVE BAD-BR-COUNTER TO R-COUNTER.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ACCESS ON LN-FILE.............." TO R-COUNTERX.
           MOVE ACCESS-COUNTER TO R-COUNTER.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/DATER.CPY".

      ****************************************************
       FORM-RESET SECTION.
      ****************************************************
           MOVE EXT-CONAME-CONAME TO MPOOL1-CONAME.
           MOVE EXT-CONAME-SYSDATE TO MPOOL1-SYSDATE.
           DISPLAY MPOOL1-SCREEN UPON MPOOL1-WINDOW-HANDLE.
           MOVE MPOOL1-FORM-FIELDS TO WR-BUF.
           MOVE MPOOL1-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ****************************************************
       SETUP-LINK-INFO SECTION.
      ****************************************************
           MOVE MPOOL1-HELP--FILE TO HELP-LINK-FILE.
           MOVE MPOOL1-HELP--DIR  TO HELP-LINK-DIR.

      ****************************************************
       VDU-CONVERT-FIELD SECTION.
      ****************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBFX/MPOOL1_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ************************************
       MISC-PARAGRAPH SECTION.
      ************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/SYSTEM.CPY".

      ********************************
       IO-ROUTINE SECTION.
      *********************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       OPEN-SETPOOL-FILE.
           PERFORM OPEN-IT.
           MOVE "SETPOOL" TO E-FILE.
           OPEN INPUT SETPOOL-FILE.
           IF IO-FG = 8
              GO TO OPEN-SETPOOL-FILE.
           IF IO-FG = 7
              CLOSE SETPOOL-FILE
              GO TO OPEN-SETPOOL-FILE.
       READ-SETPOOL-FILE.
           PERFORM READ-IT.
           MOVE "SETPOOL" TO E-FILE.
           READ SETPOOL-FILE.
           IF IO-FG = 8
              GO TO READ-SETPOOL-FILE.
       CLOSE-SETPOOL-FILE.
           PERFORM CLOSE-IT.
           CLOSE SETPOOL-FILE.
       COPY "LIBLP/LPLN1IN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.
 
      *********************************
       END-PROGRAM SECTION.
      *********************************
       END-RUN.
           PERFORM CLOSE-FILES.
           IF FIRST-TIME NOT = "Y"
              PERFORM PRU-OUT.

       EXIT-PROG.
           MOVE "FX/FXMENU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY MPOOL1-SCREEN.
           DESTROY MPOOL1-WINDOW-HANDLE.
           EXIT PROGRAM.
