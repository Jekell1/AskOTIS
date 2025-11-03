       IDENTIFICATION DIVISION.
       PROGRAM-ID.      BUYBAC.
      **************************************************************************
      *    WORLD ACCEPTANCE
      *
      * WORLD DEBT SALE BUY BACK, BECAUSE THE ACCOUNT DID NOT MEET ALL THE
      *   CRITERIA. MUST PUT THEM BACK TO HOW THEY WERE BEFORE THE SALE.
      *   MUST REPORT TO THE CREDIT BUREAU AGAIN.
      *
      * READ IN A FIXED LENGTH FILE AND SET;
      *      LN-SKIP-TRW = "B" - SO THE BUREAU CAN REPORT THEM BACK OPEN
      *      LN-ACCT-FROZEN = " "
      *      LN-DLPOOL = POOL ID IN THE INCOMING FILE
      *      LN-MESSAGE = "ACCOUNT REPURCHASED MMYY"
      *      LN-TRW-STATUS = "DA"
      *      LN-TRW-SPECIAL-CODE = "**"
      *      REMOVE THE BORROWER MEMO WITH THE SOLD TO NAME
      *
      * FILE HAS 0A ON END OF EACH RECORD
      *
      * FILE=../../SP/BUYBACK   GLOBAL SP
      *
      * REV:
      * 150428 BAH PR#1076
      * 150624 BLM FIXED BUG, WAS MOVING "B" TO LN & BM ALL-MACHINE INSTEAD
      *            PATH-OVERRIDE
      * BAH 2018.1203 GLOBAL LNFILE
      * BAH 2019.0918 REMOVED ACCESS-CALL ON LNFILE
      * BAH 2022.1024 CHANGE LN-TRW-STATUS FROM "97" TO "DA", THEY WANT THEM
      *               DELETED. PRINT ZERO BALANCE ACCOUNTS ON EXCEPTION #1574
      * BAH 2024.0125 MOVE FROM FX TO SP AND MAKE BATCHABLE #1640
      * BAH 2024.0530 REMOVE BR-MACHINE NOT = EXT-FILPATH-MACHINE S35Q-57
      **************************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
           SELECT BUYBACK-FILE ASSIGN BUYBACK-PATH
                           ORGANIZATION LINE SEQUENTIAL
                           ACCESS SEQUENTIAL
                           LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                           FILE STATUS FILE-STAT.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".

       FD  BUYBACK-FILE
           LABEL RECORDS ARE STANDARD.
       01  BUYBACK-REC.
           03  BUYBACK-BRNO        PIC 9(4).
           03  BUYBACK-BRNOX REDEFINES BUYBACK-BRNO PIC X(4).
           03  BUYBACK-NUMBER      PIC 9(6).
           03  BUYBACK-NUMBERX REDEFINES BUYBACK-NUMBER PIC X(6).
           03  BUYBACK-DLPOOL      PIC 9(5).
           03  BUYBACK-DLPOOLX REDEFINES BUYBACK-DLPOOL PIC X(5).
           03  BUYBACK-DLPOOLZ REDEFINES BUYBACK-DLPOOL.
               05  FILLER          PIC X.
               05  BUYBACK-MMYY    PIC 9(4).



       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)SP/BUYBAC.CBL S35 02/22/24-08:21:46 >".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".

      ******************************************************************
      *
      *    PROGRAM WORKERS
      *
      ******************************************************************
       01  HOLD-BRNO            PIC 9(4) VALUE 0.

       01  BUYBACK-PATH.
           03  BUY-PATH-BASE       PIC X(9).
           03  FILLER              PIC X VALUE "/".
           03  BUY-PATH-MACHINE    PIC X(2).
           03  FILLER              PIC X(11) VALUE "/SP/BUYBACK".

       01  FIRST-TIME           PIC X VALUE "Y".

       01  LN-WORKERS.
           03  LN-FEED          PIC 99     VALUE 0.
           03  LN-COUNT         PIC 999    VALUE 99.
           03  LN-WK            PIC 999    VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)   VALUE 0.
 
       01  BUYBACK-COUNTER      PIC 9(6)   VALUE 0 COMP-3.
       01  MISSING-COUNTER      PIC 9(6)   VALUE 0 COMP-3.
       01  REWRITE-COUNTER      PIC 9(6)   VALUE 0 COMP-3.
       01  BAD-BR-COUNTER       PIC 9(6)   VALUE 0 COMP-3.
       01  ACCESS-COUNTER       PIC 9(6)   VALUE 0 COMP-3.

       COPY "LIBGB/EOBUF_EXT.CPY".

       01  VERSION-CONTROL     PIC X    VALUE "A".
 
       01  ACCEPT-BUF       PIC X     VALUE SPACES.

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
               "DEBT SALE BUYBACK EXCEPTION LISTING".

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
 
 
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/BUYBAC_DEF.CPY".
       COPY "LIBSP/BUYBAC_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBSP/BUYBAC_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
              PR-FILE BUYBACK-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-BR-FILE.
           CLOSE
              PR-FILE BUYBACK-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ******************************************************************
      *
      *    M A I N - P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "BUYBAC" TO VDU-FORM-NAME.

           PERFORM INITIALIZATION.
           IF ( EXT-EOBUF-ERRCD NOT = SPACES )
               GO TO END-PROGRAM.

           PERFORM FIX-BUYBACK-ACCOUNTS.

           MOVE "N" TO FIRST-TIME.

           PERFORM PRINT-TOTALS.

           GO TO END-PROGRAM.

      *****************************
       FIX-BUYBACK-ACCOUNTS SECTION.
      *****************************

           PERFORM OPEN-BUYBACK-FILE.

           MOVE "PROCESSING FILE:" TO MESS-TITLE.
           MOVE MESS-DISPLAY TO WR-BUF.
           MOVE MESS-DISP-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE 0 TO HOLD-BRNO.
           MOVE 0 TO BUYBACK-COUNTER.
       NEXT-BUYBACK.
           PERFORM READ-BUYBACK-FILE.
           IF IO-FG NOT = 0 
              PERFORM CLOSE-LN1-FILE
              PERFORM CLOSE-BUYBACK-FILE
              GO TO EXIT-FIX-BUYBACK-ACCOUNTS.
 
           ADD 1 TO BUYBACK-COUNTER.

           INSPECT BUYBACK-BRNOX REPLACING ALL " " BY "0".
           IF BUYBACK-BRNOX NOT NUMERIC
              GO TO NEXT-BUYBACK.

           INSPECT BUYBACK-NUMBERX REPLACING ALL " " BY "0".
           IF BUYBACK-NUMBERX NOT NUMERIC
              GO TO NEXT-BUYBACK.

           INSPECT BUYBACK-DLPOOLX REPLACING ALL " " BY "0".
           IF BUYBACK-DLPOOLX NOT NUMERIC
              GO TO NEXT-BUYBACK.

           IF HOLD-BRNO NOT = BUYBACK-BRNO
              PERFORM CLOSE-LN1-FILE
              PERFORM CLOSE-BW1-FILE
              MOVE BUYBACK-BRNO TO HOLD-BRNO BR-NO
              PERFORM READ-BR-FILE
              IF IO-FG NOT = 0
                 ADD 1 TO BAD-BR-COUNTER
                 MOVE 0 TO HOLD-BRNO
                 MOVE BUYBACK-BRNO TO D-BRNO
                 MOVE "INVALID BRANCH" TO D-TEXT
                 PERFORM WRITE-DETAIL-LINE
                 GO TO NEXT-BUYBACK
              ELSE
      *          IF BR-MACHINE NOT = EXT-FILPATH-MACHINE
      *             MOVE 0 TO HOLD-BRNO
      *             ADD 1 TO OTHER-MACHINE-COUNTER
      *             GO TO NEXT-BUYBACK
      *          ELSE
                    MOVE EXT-FILPATH-BASE TO FILPATH-BASE
                    MOVE BR-MACHINE       TO FILPATH-MACHINE
                    MOVE BR-DATA-PATH     TO FILPATH-DATA
                    MOVE "B"              TO LN-PATH-OVERRIDE
                                             BW-PATH-OVERRIDE
                    PERFORM OPEN-LN1-FILE
                    PERFORM OPEN-BW1-FILE.

           MOVE BUYBACK-BRNO   TO LN-OWNBR.
           MOVE BUYBACK-NUMBER TO LN-ACCTNO.
           PERFORM READ-LN1-FILE.
           IF IO-FG NOT = 0
              ADD 1 TO MISSING-COUNTER
              MOVE BR-NO TO D-BRNO
              MOVE BUYBACK-NUMBER TO D-NUMBER
              MOVE "INVALID ACCOUNT NO." TO D-TEXT
              PERFORM WRITE-DETAIL-LINE
              GO TO NEXT-BUYBACK.

           IF LN-POCD = "**"
              MOVE BR-NO TO D-BRNO
              MOVE LN-ACCTNO TO D-NUMBER
              MOVE "VOID" TO D-TEXT
              PERFORM WRITE-DETAIL-LINE.

      * IF ACCOUNT IS ZERO BALANCE, UPDATE BUT PRINT ON DETAIL EXCEPTION
           IF LN-CURBAL = 0
              MOVE BR-NO                  TO D-BRNO
              MOVE LN-ACCTNO              TO D-NUMBER
              MOVE "MAY HAVE TRANSFERRED" TO D-TEXT
              PERFORM WRITE-DETAIL-LINE.

      * THIS WILL ALLOW THE CREDIT BUREAU PROGRAM TO KNOW WHAT TO DO
      * WITH THESE ACCOUNTS
           MOVE "B" TO LN-SKIP-TRW.

           MOVE SPACES TO LN-ACCT-FROZEN.

           MOVE BUYBACK-DLPOOL TO LN-DLPOOL.

           MOVE SPACES TO LN-MESSAGE.
           STRING "ACCOUNT REPURCHASED ",BUYBACK-MMYY DELIMITED BY
                      "  " INTO LN-MESSAGE.

           MOVE "**" TO LN-TRW-SPECIAL-CODE.

           MOVE "DA" TO LN-TRW-STATUS.

           ADD 1 TO REWRITE-COUNTER.
           PERFORM REWRITE-LN1-FILE.

           MOVE LN-SSNO(1) TO BW-SSNO.
           PERFORM READ-BW1-FILE.
           IF IO-FG = 0
              MOVE SPACES TO BW-CONTACT-DESC(10)
              PERFORM REWRITE-BW1-FILE.

           GO TO NEXT-BUYBACK.

       EXIT-FIX-BUYBACK-ACCOUNTS.
           EXIT.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************
           MOVE " " TO EXT-EOBUF-ERRCD.

           MOVE BUYBAC-QUEUE-POS  TO Q-POS.
           MOVE BUYBAC-COPIES-POS TO C-POS.
           MOVE BUYBAC-PRU-POS    TO P-POS.
           MOVE SPACES            TO Q-BUF C-BUF P-BUF.
       COPY "LIBGB/EOINIT.CPY".

           MOVE SPACES TO DETAIL-LINE.
           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-PR-FILE.

           GO TO EXIT-INITIALIZE.
       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.
       EXIT-INITIALIZE.
           EXIT.

      ******************************************************************
       ENTER-PARAMETERS SECTION.
      ******************************************************************
           MOVE EXT-FILPATH-BASE TO BUY-PATH-BASE.
           MOVE EXT-FILPATH-MACHINE TO BUY-PATH-MACHINE.
           MOVE BUYBACK-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT NOT = "00"
              MOVE "SP/BUYBACK DOES NOT EXIST" TO MESS
              PERFORM SEND-MESS
              MOVE "1>" TO VDUSTATUS
              GO TO EXIT-PARM.

       EXIT-PARM.
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

           MOVE "TOTAL RECORDS IN BUYBACK......." TO R-COUNTERX.
           MOVE BUYBACK-COUNTER TO R-COUNTER.
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


      *****************************************
       EO-EXEC-AUDIT SECTION.
      *****************************************
           MOVE EXT-FILPATH-BASE TO BUY-PATH-BASE.
           MOVE EXT-FILPATH-MACHINE TO BUY-PATH-MACHINE.
           MOVE BUYBACK-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT NOT = "00"
              MOVE "SP/BUYBACK DOES NOT EXIST" TO MESS
              PERFORM SEND-MESS
              MOVE "X" TO EXT-EOBUF-ERRCD.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/DATER.CPY".

      ****************************************************
       FORM-RESET SECTION.
      ****************************************************
           MOVE EXT-CONAME-CONAME TO BUYBAC-CONAME.
           MOVE EXT-CONAME-SYSDATE TO BUYBAC-SYSDATE.
           DISPLAY BUYBAC-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE BUYBAC-FORM-FIELDS TO WR-BUF.
           MOVE BUYBAC-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ****************************************************
       SETUP-LINK-INFO SECTION.
      ****************************************************
           MOVE BUYBAC-HELP--FILE TO HELP-LINK-FILE.
           MOVE BUYBAC-HELP--DIR  TO HELP-LINK-DIR.

      ****************************************************
       VDU-CONVERT-FIELD SECTION.
      ****************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
             COPY "LIBSP/BUYBAC_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ************************************
       MISC-PARAGRAPH SECTION.
      ************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/GETENV.CPY".
       DISPLAY-PARAMETERS.

      ********************************
       IO-ROUTINE SECTION.
      *********************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       OPEN-BUYBACK-FILE.
           PERFORM OPEN-IT.
           MOVE "BUYBACK" TO E-FILE.
           OPEN INPUT BUYBACK-FILE.
           IF IO-FG = 8
              GO TO OPEN-BUYBACK-FILE.
           IF IO-FG = 7
              CLOSE BUYBACK-FILE
              GO TO OPEN-BUYBACK-FILE.
       READ-BUYBACK-FILE.
           PERFORM READ-IT.
           MOVE "BUYBACK" TO E-FILE.
           READ BUYBACK-FILE.
           IF IO-FG = 8
              GO TO READ-BUYBACK-FILE.
       CLOSE-BUYBACK-FILE.
           PERFORM CLOSE-IT.
           CLOSE BUYBACK-FILE.
       COPY "LIBLP/LPBW1IN.CPY".
       COPY "LIBLP/LPLN1IN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.
 
      *********************************
       END-PROGRAM SECTION.
      *********************************
       END-RUN.
           PERFORM CLOSE-FILES.
           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.
       EXIT-PROG.
           IF EXT-ROUTE-BUF = "00"
              MOVE "SP/EXTRMU" TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY BUYBAC-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
