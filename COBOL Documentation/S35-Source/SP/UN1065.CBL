       IDENTIFICATION DIVISION.
       PROGRAM-ID.      UN1065.
      **************************************************************************
      *    WORLD ACCEPTANCE
      *
      * READ IN A FIXED LENGTH FILE AND UNSET THE CHANGES FROM WR1065
      *  CLEAR LN-DLPOOL
      *        LN-TRW-SPECIAL-CODE
      *        LN-MESSAGE
      *  MOVE "N" TO LN-ACCT-FROZEN
      *  CLEAR PURCHASER NAME IN BORROWER PRIORITY MEMO 10
      *
      * FILE HAS 0A ON END OF EACH RECORD
      *
      * FILE=../../SP/UNSETPOOL   GLOBAL SP
      *
      * REV:
      * BAH 2022.1130 UNSET THE CHANGES THAT FIX WR1065 DOES, THE BUYER
      *               DID NOT WANT SOME ACCOUNTS
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
           SELECT UNSETPOOL-FILE ASSIGN TO UNSET-PATH
                           ORGANIZATION LINE SEQUENTIAL
                           ACCESS SEQUENTIAL
                           LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                           FILE STATUS FILE-STAT.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".

       FD  UNSETPOOL-FILE
           LABEL RECORDS ARE STANDARD.
       01  UNSETPOOL-REC.
           03  UNSETPOOL-BRNO        PIC 9(4).
           03  UNSETPOOL-BRNOX REDEFINES UNSETPOOL-BRNO PIC X(4).
           03  UNSETPOOL-NUMBER      PIC 9(6).
           03  UNSETPOOL-NUMBERX REDEFINES UNSETPOOL-NUMBER PIC X(6).

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)SP/UN1065.CBL S35 01/25/24-12:30:15 >".
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

       01  FIRST-TIME           PIC X VALUE "Y".
       01  UNSET-PATH.
           03  UNSET-PATH-BASE       PIC X(9).
           03  FILLER                PIC X VALUE "/".
           03  UNSET-PATH-MACHINE    PIC X(2).
           03  FILLER                PIC X(13) VALUE "/SP/UNSETPOOL".
       01  SAVE-UNSETPOOL-PATH.
           03  SAVE-UNSET-PATH       PIC X(25).
           03  FILLER                PIC X     VALUE ".".
           03  SAVE-UNSETPOOL-DATE   PIC 9(6).

       01  LN-WORKERS.
           03  LN-FEED          PIC 99     VALUE 0.
           03  LN-COUNT         PIC 999    VALUE 99.
           03  LN-WK            PIC 999    VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)   VALUE 0.
 
       01  UNSETPOOL-COUNTER      PIC 9(6)   VALUE 0 COMP-3.
       01  MISSING-COUNTER      PIC 9(6)   VALUE 0 COMP-3.
       01  REWRITE-COUNTER      PIC 9(6)   VALUE 0 COMP-3.
       01  BAD-BR-COUNTER       PIC 9(6)   VALUE 0 COMP-3.
 
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  REPORT-ONLY     PIC X.

       01  VERSION-CONTROL     PIC X    VALUE "A".

      * YYYYMMDD
       01  TODAYS-DATE      PIC 9(8).
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
               "UNSET POOLID EXCEPTION LISTING".

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
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/UN1065_DEF.CPY".
       COPY "LIBSP/UN1065_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBSP/UN1065_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
              PR-FILE UNSETPOOL-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-BR-FILE.
           CLOSE PR-FILE UNSETPOOL-FILE.
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
           MOVE "UN1065" TO VDU-FORM-NAME.

         
           PERFORM INITIALIZATION.
           IF ( EXT-EOBUF-ERRCD NOT = SPACES )
               GO TO END-PROGRAM.

           PERFORM UNSET-POOL.

           PERFORM PRINT-TOTALS.

           GO TO END-PROGRAM.

      *****************************
       UNSET-POOL SECTION.
      *****************************

           PERFORM OPEN-UNSETPOOL-FILE.

           MOVE "PROCESSING FILE:" TO MESS-TITLE.
           MOVE MESS-DISPLAY TO WR-BUF.
           MOVE MESS-DISP-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      D    STOP MESS.
           MOVE 0 TO HOLD-BRNO.
           MOVE 0 TO UNSETPOOL-COUNTER.
       NEXT-UNSETPOOL.
           PERFORM READ-UNSETPOOL-FILE.
           IF IO-FG NOT = 0 
              PERFORM CLOSE-LN1-FILE
              PERFORM CLOSE-UNSETPOOL-FILE
              GO TO EXIT-UNSET-POOL.
 
           MOVE "N" TO FIRST-TIME.

           ADD 1 TO UNSETPOOL-COUNTER.

           INSPECT UNSETPOOL-BRNOX REPLACING ALL " " BY "0".
           IF UNSETPOOL-BRNOX NOT NUMERIC
              GO TO NEXT-UNSETPOOL.

           INSPECT UNSETPOOL-NUMBERX REPLACING ALL " " BY "0".
           IF UNSETPOOL-NUMBERX NOT NUMERIC
              GO TO NEXT-UNSETPOOL.

           IF HOLD-BRNO NOT = UNSETPOOL-BRNO
              PERFORM CLOSE-LN1-FILE
              MOVE UNSETPOOL-BRNO TO HOLD-BRNO BR-NO
              PERFORM READ-BR-FILE
              IF IO-FG NOT = 0
                 ADD 1 TO BAD-BR-COUNTER
                 MOVE 0 TO HOLD-BRNO
                 MOVE UNSETPOOL-BRNO TO D-BRNO
                 MOVE "INVALID BRANCH" TO D-TEXT
                 PERFORM WRITE-DETAIL-LINE
                 GO TO NEXT-UNSETPOOL
              ELSE
      *          IF BR-MACHINE NOT = EXT-FILPATH-MACHINE
      *             MOVE 0 TO HOLD-BRNO
      *             ADD 1 TO OTHER-MACHINE-COUNTER
      *             GO TO NEXT-UNSETPOOL
      *          ELSE
                    MOVE EXT-FILPATH-BASE TO FILPATH-BASE
                    MOVE BR-MACHINE       TO FILPATH-MACHINE
                    MOVE BR-DATA-PATH     TO FILPATH-DATA
                    MOVE "B"              TO LN-PATH-OVERRIDE
                                             BW-PATH-OVERRIDE
                    PERFORM OPEN-LN1-FILE
                    PERFORM OPEN-BW1-FILE.

           MOVE UNSETPOOL-BRNO   TO LN-OWNBR.
           MOVE UNSETPOOL-NUMBER TO LN-ACCTNO.
           PERFORM READ-LN1-FILE.
           IF IO-FG NOT = 0
              ADD 1 TO MISSING-COUNTER
              MOVE BR-NO TO D-BRNO
              MOVE UNSETPOOL-NUMBER TO D-NUMBER
              MOVE "INVALID ACCOUNT NO." TO D-TEXT
              PERFORM WRITE-DETAIL-LINE
              GO TO NEXT-UNSETPOOL.

           IF LN-POCD = "**"
              MOVE BR-NO TO D-BRNO
              MOVE LN-ACCTNO TO D-NUMBER
              MOVE "VOID" TO D-TEXT
              PERFORM WRITE-DETAIL-LINE.

           ADD 1 TO REWRITE-COUNTER.
           IF REPORT-ONLY = "Y"
              GO TO NEXT-UNSETPOOL.

           MOVE 0      TO LN-DLPOOL.
           MOVE SPACES TO LN-TRW-SPECIAL-CODE.
           MOVE SPACES TO LN-MESSAGE.
           MOVE "N" TO LN-ACCT-FROZEN.

           PERFORM REWRITE-LN1-FILE.

           MOVE LN-SSNO(1) TO BW-SSNO.
           PERFORM READ-BW1-FILE.
           IF IO-FG = 0
              MOVE SPACES TO BW-CONTACT-DESC(10)
              MOVE 0      TO BW-CONTACT-PHONE(10) BW-CONTACT-EXT(10)
              PERFORM REWRITE-BW1-FILE.

           GO TO NEXT-UNSETPOOL.

       EXIT-UNSET-POOL.
           EXIT.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************

           MOVE "Y" TO FIRST-TIME.

           MOVE " " TO EXT-EOBUF-ERRCD.

           MOVE UN1065-QUEUE-POS  TO Q-POS.
           MOVE UN1065-COPIES-POS TO C-POS.
           MOVE UN1065-PRU-POS    TO P-POS.
           MOVE SPACES            TO Q-BUF C-BUF P-BUF.
       COPY "LIBGB/EOINIT.CPY".

           ACCEPT TODAYS-DATE FROM CENTURY-DATE.

           MOVE SPACES TO DETAIL-LINE.
           MOVE EXT-CONAME-CONAME  TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-PR-FILE.

           IF REPORT-ONLY = "Y"
              MOVE "UNSET POOLID EXCEPTION LIST-REPORT ONLY"
                                              TO H-TITLE.
           GO TO EXIT-INITIALIZE.
       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.
       EXIT-INITIALIZE.
           EXIT.

      ******************************************************************
       ENTER-PARAMETERS SECTION.
      ******************************************************************
           MOVE EXT-FILPATH-BASE    TO UNSET-PATH-BASE.
           MOVE EXT-FILPATH-MACHINE TO UNSET-PATH-MACHINE.
           MOVE UNSET-PATH            TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT NOT = "00"
              MOVE "MISSING ../../SP/UNSETPOOL" TO MESS
              PERFORM SEND-MESS
              MOVE "1>" TO VDUSTATUS
              GO TO EXIT-PARM.

       REPORT-YN.
           MOVE "E  A010REPORTONLY." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>"
              MOVE "X" TO EXT-EOBUF-ERRCD
              GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" GO TO REPORT-YN.
           MOVE VDUBUF TO REPORT-ONLY.
           IF NOT (REPORT-ONLY = "Y" OR "N")
              GO TO REPORT-YN.

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

           MOVE "TOTAL RECORDS IN UNSETPOOL......." TO R-COUNTERX.
           MOVE UNSETPOOL-COUNTER TO R-COUNTER.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BAD ACCOUNT NUMBERS............" TO R-COUNTERX.
           MOVE MISSING-COUNTER TO R-COUNTER.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF REPORT-ONLY = "N"
              MOVE "LOAN RECORDS CHANGED..........." TO R-COUNTERX
           ELSE
              MOVE "LOAN RECORDS TO BE CHANGED....." TO R-COUNTERX.
           MOVE REWRITE-COUNTER TO R-COUNTER.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "INVALID BRANCH................." TO R-COUNTERX.
           MOVE BAD-BR-COUNTER TO R-COUNTER.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.


      ***********************************************
       EO-EXEC-AUDIT SECTION.
      ***********************************************
           MOVE EXT-FILPATH-BASE    TO UNSET-PATH-BASE.
           MOVE EXT-FILPATH-MACHINE TO UNSET-PATH-MACHINE.
           MOVE UNSET-PATH            TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT NOT = "00"
              MOVE "MISSING ../../SP/UNSETPOOL" TO MESS
              PERFORM SEND-MESS
              MOVE "X" TO EXT-EOBUF-ERRCD.

      *****************************************
       DISPLAY-PARAMETERS SECTION.
      *****************************************
           MOVE "S  A010REPORTONLY." TO VDU.
           MOVE REPORT-ONLY TO VDUBUF.
           PERFORM SCREENIO.
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/DATER.CPY".

      ****************************************************
       FORM-RESET SECTION.
      ****************************************************
           MOVE EXT-CONAME-CONAME TO UN1065-CONAME.
           MOVE EXT-CONAME-SYSDATE TO UN1065-SYSDATE.
           DISPLAY UN1065-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE UN1065-FORM-FIELDS TO WR-BUF. 
           MOVE UN1065-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM. 


      ****************************************************
       SETUP-LINK-INFO SECTION.
      ****************************************************
           MOVE UN1065-HELP--FILE TO HELP-LINK-FILE.
           MOVE UN1065-HELP--DIR  TO HELP-LINK-DIR.

      ****************************************************
       VDU-CONVERT-FIELD SECTION.
      ****************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/UN1065_EVA.CPY".
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
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       OPEN-UNSETPOOL-FILE.
           PERFORM OPEN-IT.
           MOVE "UNSETPOOL" TO E-FILE.
           OPEN INPUT UNSETPOOL-FILE.
           IF IO-FG = 8
              GO TO OPEN-UNSETPOOL-FILE.
           IF IO-FG = 7
              CLOSE UNSETPOOL-FILE
              GO TO OPEN-UNSETPOOL-FILE.
       READ-UNSETPOOL-FILE.
           PERFORM READ-IT.
           MOVE "UNSETPOOL" TO E-FILE.
           READ UNSETPOOL-FILE.
           IF IO-FG = 8
              GO TO READ-UNSETPOOL-FILE.
       CLOSE-UNSETPOOL-FILE.
           PERFORM CLOSE-IT.
           CLOSE UNSETPOOL-FILE.
       COPY "LIBLP/LPBW1IN.CPY".
       COPY "LIBLP/LPLN1IN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.
 
      ************************************
       END-PROGRAM SECTION.
      ************************************
       END-RUN.
           PERFORM CLOSE-FILES.
           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.

      * ONLY MOVE IT IF YOU RAN IT AND DIDNT JUST F7 OUT
           IF FIRST-TIME = "N"
             IF REPORT-ONLY = "N"
              MOVE UNSET-PATH TO ACCESS-BUF
              PERFORM ACCESS-CALL
              IF STAT = "00"
                 MOVE UNSET-PATH TO SAVE-UNSET-PATH
                 MOVE TODAYS-DATE TO DATE-YYYYMMDD
                 PERFORM CONVERT-YYYYMMDD-TO-YYMMDD
                 MOVE DATE-YYMMDD TO SAVE-UNSETPOOL-DATE
                 MOVE SPACES TO SYSTEM-BUF
                 STRING MV-CMD," ",UNSET-PATH," ",SAVE-UNSETPOOL-PATH
                        DELIMITED BY "  " INTO SYSTEM-BUF
                 PERFORM SYSTEM-CALL.

       EXIT-PROG.
           IF EXT-ROUTE-BUF = "00"
              MOVE "SP/EXTRMU" TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY UN1065-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
