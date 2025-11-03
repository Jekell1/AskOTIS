       IDENTIFICATION DIVISION.
       PROGRAM-ID.      WR1080.
      **************************************************************************
      *    WORLD ACCEPTANCE
      *
      * READ IN A FIXED LENGTH FILE AND SET;
      *      LN-ACCT-FROZEN = " "
      *      LN-MESSAGE = SPACES
      *
      * FILE HAS 0A ON END OF EACH RECORD
      *
      * FILE=../../SP/CLEARFROZEN   GLOBAL SP
      *
      * NOTES: WR1071 SETS THE FROZEN FLAG AND MESSAGE TO "PENDING SALE"
      *        FROM A FILE NAMED SETFROZEN
      * REV:
      *  150507 BAH PR#1080
      *  150624 BLM FIXED BUG, WAS MOVING "B" TO LN-ALL-MACHINE INSTEAD
      *             LN-PATH-OVERRIDE
      * BAH 181203 GLOBAL LNFILE
      * BAH 190918 REMOVED ACCESS-CALL ON LNFILE
      * BAH 2024.0530 REMOVE BR-MACHINE NOT = EXT-FILPATH-MACHINE S35Q-56
      **************************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
           SELECT FROZEN-FILE ASSIGN TO FROZEN-PATH
                           ORGANIZATION LINE SEQUENTIAL
                           ACCESS SEQUENTIAL
                           LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                           FILE STATUS FILE-STAT.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".

       FD  FROZEN-FILE
           LABEL RECORDS ARE STANDARD.
       01  FROZEN-REC.
           03  FROZEN-BRNO        PIC 9(4).
           03  FROZEN-BRNOX REDEFINES FROZEN-BRNO PIC X(4).
           03  FROZEN-NUMBER      PIC 9(6).
           03  FROZEN-NUMBERX REDEFINES FROZEN-NUMBER PIC X(6).



       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)FX/WR1080.CBL S34 05/21/21-13:29:52 >".
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

       01  FROZEN-PATH.
           03  FROZEN-PATH-BASE    PIC X(9).
           03  FILLER              PIC X VALUE "/".
           03  FROZEN-PATH-MACHINE PIC X(2).
           03  FILLER              PIC X(15) VALUE "/SP/CLEARFROZEN".

       01  SAVE-FROZEN-PATH.
           03  FILLER           PIC X(27).
           03  SAVE-FROZEN-DOT  PIC X VALUE ".".
           03  SAVE-FROZEN-DATE PIC 9(6).

       01  LEGEND               PIC X(80) VALUE ZEROES.
       01  LEGEND-LIST          PIC X(09) VALUE "LEGEND:B.".


       01  FIRST-TIME           PIC X VALUE "Y".

       01  LN-WORKERS.
           03  LN-FEED          PIC 99     VALUE 0.
           03  LN-COUNT         PIC 999    VALUE 99.
           03  LN-WK            PIC 999    VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)   VALUE 0.
 
       01  FROZEN-COUNTER      PIC 9(6)   VALUE 0 COMP-3.
       01  MISSING-COUNTER      PIC 9(6)   VALUE 0 COMP-3.
       01  REWRITE-COUNTER      PIC 9(6)   VALUE 0 COMP-3.
       01  BAD-BR-COUNTER       PIC 9(6)   VALUE 0 COMP-3.
       01  ACCESS-COUNTER       PIC 9(6)   VALUE 0 COMP-3.
 
       01  ERRCD          PIC X.

 
       01  ACCEPT-BUF       PIC X     VALUE SPACES.

       01  HOLD-SYSDATE    PIC 9(8).
 
       01  MESS-DISP-LIST    PIC X(4) VALUE "MES.".
       01  MESS-DISPLAY.
           03  MESS-TITLE       PIC X(21)  VALUE SPACES.
           03  FILLER           PIC X(29)  VALUE SPACES.

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
               "CLEAR FROZEN EXCEPTION LISTING".

       01  DETAIL-LINE              PIC X(80).
       01  DETAIL-LINE1 REDEFINES DETAIL-LINE.
           03  D-BRNO        PIC 9(4).
           03  FILLER        PIC X(2).
           03  D-NUMBER      PIC Z(6).
           03  FILLER        PIC X(2).
           03  D-TEXT        PIC X(20).

       01  DETAIL-LINE2 REDEFINES DETAIL-LINE.
           03  FILLER        PIC X(20).
           03  R-COUNTERX    PIC X(28).
           03  R-COUNTER     PIC Z(8)9.99.
           03  FILLER        PIC X(2).
 
 
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/GETENVW.CPY".

       01  WR1080-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBFX/WR1080_DEF.CPY".
       COPY "LIBFX/WR1080_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBFX/WR1080_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
              PR-FILE FROZEN-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-BR-FILE.
           CLOSE PR-FILE FROZEN-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      ******************************************************************
      *
      *    M A I N - P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "WR1080" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS WR1080-WINDOW-HANDLE.

       COPY "LIBGB/CHKSEC.CPY".

           PERFORM FORM-RESET.

           MOVE EXT-FILPATH-BASE    TO FROZEN-PATH-BASE.
           MOVE EXT-FILPATH-MACHINE TO FROZEN-PATH-MACHINE.
           MOVE FROZEN-PATH         TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT NOT = "00"
              MOVE "../../SP/CLEARFROZEN DOES NOT EXIST" TO MESS
              PERFORM SEND-MESS
              GO TO END-PROGRAM.

           PERFORM INITIALIZATION.
           IF ( ERRCD NOT = SPACES )
               GO TO END-PROGRAM.


           PERFORM CLEAR-FROZEN.

           MOVE "N" TO FIRST-TIME.

           PERFORM PRINT-TOTALS.

           GO TO END-PROGRAM.

      *****************************
       CLEAR-FROZEN SECTION.
      *****************************

           PERFORM OPEN-FROZEN-FILE.

           MOVE "PROCESSING FILE:" TO MESS-TITLE.
           MOVE MESS-DISPLAY TO WR-BUF.
           MOVE MESS-DISP-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE 0 TO HOLD-BRNO.
           MOVE 0 TO FROZEN-COUNTER.
       NEXT-FROZEN.
           PERFORM READ-FROZEN-FILE.
           IF IO-FG NOT = 0 
              PERFORM CLOSE-LN1-FILE
              PERFORM CLOSE-FROZEN-FILE
              GO TO EXIT-FROZEN.
 
           ADD 1 TO FROZEN-COUNTER.

           INSPECT FROZEN-BRNOX REPLACING ALL " " BY "0".
           IF FROZEN-BRNOX NOT NUMERIC
              GO TO NEXT-FROZEN.

           INSPECT FROZEN-NUMBERX REPLACING ALL " " BY "0".
           IF FROZEN-NUMBERX NOT NUMERIC
              GO TO NEXT-FROZEN.

           IF HOLD-BRNO NOT = FROZEN-BRNO
              PERFORM CLOSE-LN1-FILE
              MOVE FROZEN-BRNO TO HOLD-BRNO BR-NO
              PERFORM READ-BR-FILE
              IF IO-FG NOT = 0
                 ADD 1 TO BAD-BR-COUNTER
                 MOVE 0 TO HOLD-BRNO
                 MOVE FROZEN-BRNO TO D-BRNO
                 MOVE "INVALID BRANCH" TO D-TEXT
                 PERFORM WRITE-DETAIL-LINE
                 GO TO NEXT-FROZEN
              ELSE
      *          IF BR-MACHINE NOT = EXT-FILPATH-MACHINE
      *             MOVE 0 TO HOLD-BRNO
      *             ADD 1 TO OTHER-MACHINE-COUNTER
      *             GO TO NEXT-FROZEN
      *          ELSE
                    MOVE EXT-FILPATH-BASE TO FILPATH-BASE
                    MOVE BR-MACHINE       TO FILPATH-MACHINE
                    MOVE BR-DATA-PATH     TO FILPATH-DATA
                    MOVE "B"              TO LN-PATH-OVERRIDE
                    PERFORM OPEN-LN1-FILE.

           MOVE FROZEN-BRNO   TO LN-OWNBR.
           MOVE FROZEN-NUMBER TO LN-ACCTNO.
           PERFORM READ-LN1-FILE.
           IF IO-FG NOT = 0
              ADD 1 TO MISSING-COUNTER
              MOVE BR-NO TO D-BRNO
              MOVE FROZEN-NUMBER TO D-NUMBER
              MOVE "INVALID ACCOUNT NO." TO D-TEXT
              PERFORM WRITE-DETAIL-LINE
              GO TO NEXT-FROZEN.

           IF LN-POCD = "**"
              MOVE BR-NO TO D-BRNO
              MOVE LN-ACCTNO TO D-NUMBER
              MOVE "VOID" TO D-TEXT
              PERFORM WRITE-DETAIL-LINE.

           MOVE SPACES TO LN-MESSAGE.
           MOVE " " TO LN-ACCT-FROZEN.

           ADD 1 TO REWRITE-COUNTER.
           PERFORM REWRITE-LN1-FILE.


           GO TO NEXT-FROZEN.

       EXIT-FROZEN.
           EXIT.


      ******************************************************************
      *
      *    I N I T I A L I Z A T I O N
      *
      ******************************************************************
       INITIALIZATION SECTION.
           MOVE " " TO ERRCD.

       GETPR.
           MOVE WR1080-QUEUE-POS TO Q-POS.
           MOVE WR1080-COPIES-POS TO C-POS.
           MOVE WR1080-PRU-POS TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
           PERFORM PRU-IN.
           IF STAT NOT = "00"
              GO TO ENDIT.

       ENTER-ACCEPT.
           MOVE "E  A010ACCEPT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO GETPR.
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
      *     WRITE DETAIL LINE
      **************************************
       WRITE-DETAIL-LINE SECTION.
       WRITE-DET-LINE.
           ADD LN-COUNT LN-FEED GIVING LN-WK.
           IF LN-WK > EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.
           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      **************************************
      *      HEAD PAGE ROUTINE
      **************************************
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

       END-HEAD-PAGE.
           MOVE 4 TO LN-FEED.
           MOVE 2 TO LN-COUNT.

      ****************************************
      *        PRINT TOTAL LINES
      ****************************************
       PRINT-TOTALS SECTION.

           MOVE "TOTAL RECORDS IN SETFROZEN......." TO R-COUNTERX.
           MOVE FROZEN-COUNTER TO R-COUNTER.
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
           MOVE EXT-CONAME-CONAME TO WR1080-CONAME.
           MOVE EXT-CONAME-SYSDATE TO WR1080-SYSDATE.
           DISPLAY WR1080-SCREEN UPON WR1080-WINDOW-HANDLE.
           MOVE WR1080-FORM-FIELDS TO WR-BUF. 
           MOVE WR1080-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM. 


      ****************************************************
       SETUP-LINK-INFO SECTION.
      ****************************************************
           MOVE WR1080-HELP--FILE TO HELP-LINK-FILE.
           MOVE WR1080-HELP--DIR  TO HELP-LINK-DIR.

      ****************************************************
       VDU-CONVERT-FIELD SECTION.
      ****************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBFX/WR1080_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      /***********************************
      *    MISC. PARAGRAPH SECTION
      ************************************
       MISC-PARAGRAPH SECTION.
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/GETENV.CPY".

       SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE LEGEND-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      /*******************************
      *      FILE I-O ROUTINES
      *********************************
       IO-ROUTINE SECTION.
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       OPEN-FROZEN-FILE.
           PERFORM OPEN-IT.
           MOVE "SETFROZEN" TO E-FILE.
           OPEN INPUT FROZEN-FILE.
           IF IO-FG = 8
              GO TO OPEN-FROZEN-FILE.
           IF IO-FG = 7
              CLOSE FROZEN-FILE
              GO TO OPEN-FROZEN-FILE.
       READ-FROZEN-FILE.
           PERFORM READ-IT.
           MOVE "SETFROZEN" TO E-FILE.
           READ FROZEN-FILE.
           IF IO-FG = 8
              GO TO READ-FROZEN-FILE.
       CLOSE-FROZEN-FILE.
           PERFORM CLOSE-IT.
           CLOSE FROZEN-FILE.
       COPY "LIBLP/LPLN1IN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.
 
       END-PROGRAM SECTION.
       END-RUN.
           PERFORM CLOSE-FILES.
           IF FIRST-TIME NOT = "Y"
              PERFORM PRU-OUT.

           MOVE FROZEN-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT = "00"
              MOVE FROZEN-PATH TO SAVE-FROZEN-PATH
              MOVE "." TO SAVE-FROZEN-DOT
              MOVE HOLD-SYSDATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-YYMMDD
              MOVE DATE-YYMMDD TO SAVE-FROZEN-DATE
              MOVE SPACES TO SYSTEM-BUF
              STRING MV-CMD," ",FROZEN-PATH," ",SAVE-FROZEN-PATH
                     DELIMITED BY "  " INTO SYSTEM-BUF
              PERFORM SYSTEM-CALL.

       EXIT-PROG.
           MOVE "FX/FXMENU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY WR1080-SCREEN.
           DESTROY WR1080-WINDOW-HANDLE.
           EXIT PROGRAM.
