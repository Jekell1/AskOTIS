       IDENTIFICATION DIVISION.
       PROGRAM-ID.      K2NAME.
      *****************************************************
      *****************************************************
      *   READ IN A FIXED LENGTH ASCII FILE TO SET LS-M-MISC-FIELD(5)
      *    WITH SELLER NAME FOR CREDIT BUREAU K2 SEGMENT ON CONVERTED
      *    ACCOUNTS
      *
      * INPUT FILE;
      *  OTIS BRANCH  (4)
      *  OTIS ACCOUNT (6)
      *  SELLER NAME (20)
      *
      *
      * 20191010 BAH WORLD, SET LS-M-MISC-FIELD(5) PR#1395
      *              NEED SELLER NAME FOR ACQUISITION ACCOUNTS IN THE K2
      *              SEGMENT OF THE METRO2 CREDIT BUREAU
      * 20191023 BAH DONT UPDATE RECORD-WRITTEN IF REPORT-ONLY PR#1395A
      * 20220921 BAH CLEAR-LSM-FILE #1570
      *****************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
           SELECT WRLD-FILE ASSIGN TO WRLD-PATH
                           ORGANIZATION LINE SEQUENTIAL
                           ACCESS SEQUENTIAL
                           LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                           FILE STATUS FILE-STAT.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".

       FD  WRLD-FILE
           LABEL RECORDS ARE STANDARD.
       01  WRLD-REC.
      *  OTIS BRANCH (4)
           03  W-BRNO           PIC 9(4).
           03  W-BRNOX REDEFINES W-BRNO PIC X(4).
      *  OTIS ACCOUNT (6)
           03  W-NUMBER         PIC 9(6).
           03  W-NUMBERX REDEFINES W-NUMBER PIC X(6).
      *  SELLER NAME
           03  W-SELLER         PIC X(20).


       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)FX/K2NAME.CBL S34 05/21/21-13:29:09 >".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01LSM.CPY".
       COPY "LIBLP/LP01LSM_SQL.CPY".
       COPY "LIBLP/LPWSLSM.CPY".

      ******************************************************************
      *
      *    PROGRAM WORKERS
      *
      ******************************************************************
       01  FIRST-TIME           PIC X VALUE "Y".

       01  HOLD-BRNO            PIC 9(4) VALUE 0.

       01  WRLD-PATH.
           03  WRLD-PATH-BASE    PIC X(9).
           03  FILLER              PIC X VALUE "/".
           03  WRLD-PATH-MACHINE PIC X(2).
           03  FILLER              PIC X(15) VALUE "/SP/K2NAME".


       01  LN-WORKERS.
           03  LN-FEED          PIC 99     VALUE 0.
           03  LN-COUNT         PIC 999    VALUE 99.
           03  LN-WK            PIC 999    VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)   VALUE 0.
 
       01  RECORD-COUNTER       PIC 9(6)      VALUE 0 COMP-3.
       01  RECORDS-WRITTEN      PIC 9(6)      VALUE 0 COMP-3.

       01  ERRCD          PIC X.
 
       01  REPORT-ONLY      PIC X     VALUE SPACES.

       01  HEAD1.
           03  H-DATE               PIC X(8) VALUE SPACES.
           03  FILLER               PIC XX   VALUE SPACES.
           03  H-HR                 PIC 99   VALUE 0.
           03  FILLER               PIC X    VALUE ":".
           03  H-MIN                PIC 99   VALUE 0.
           03  FILLER               PIC X(10) VALUE SPACES.
           03  H-NAME               PIC X(40).
           03  FILLER               PIC X(10).
           03  FILLER               PIC X(5)  VALUE "PAGE ".
           03  H-PAGE-NO            PIC ZZZ9.

       01  HEAD2.
           03  FILLER               PIC X(10) VALUE "USER ID:".
           03  H-USERID             PIC X(10).
           03  FILLER               PIC X(10) VALUE SPACES.
           03  H-TITLE              PIC X(50) VALUE
               "UPLOAD SELLER NAME INTO MISC TRAILER FOR K2".

       01  DETAIL-LINE              PIC X(80).
       01  DETAIL-LINE1 REDEFINES DETAIL-LINE.
           03  D-BRNO        PIC Z(4).
           03  FILLER        PIC X(2).
           03  D-NUMBER      PIC 9(6).
           03  FILLER        PIC X(24).
           03  D-TEXT        PIC X(30).

       01  DETAIL-LINE2 REDEFINES DETAIL-LINE.
           03  R-COUNTERX    PIC X(20).
           03  R-COUNTER     PIC Z(8)9.99.
           03  R-COUNTER-ALPHA REDEFINES R-COUNTER PIC X(12).
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

       01  K2NAME-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBFX/K2NAME_DEF.CPY".
       COPY "LIBFX/K2NAME_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBFX/K2NAME_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE WRLD-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-LSM1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-BR-FILE.
           CLOSE PR-FILE WRLD-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      ******************************************************************
      *
      *    M A I N - P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "K2NAME" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS K2NAME-WINDOW-HANDLE.

       COPY "LIBGB/CHKSEC.CPY".

           PERFORM FORM-RESET.

           MOVE EXT-FILPATH-BASE    TO WRLD-PATH-BASE.
           MOVE EXT-FILPATH-MACHINE TO WRLD-PATH-MACHINE.
           MOVE WRLD-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT NOT = "00"
              MOVE "../../SP/K2NAME DOES NOT EXIST" TO MESS
              PERFORM SEND-MESS
              GO TO END-PROGRAM.

           PERFORM INITIALIZATION.
           IF ERRCD NOT = " "
               GO TO END-PROGRAM.

           PERFORM SET-LN-FIELD.

           MOVE "N" TO FIRST-TIME.
           PERFORM PRINT-TOTALS.

           GO TO END-PROGRAM.

      *****************************
       SET-LN-FIELD SECTION.
      *****************************
           PERFORM OPEN-WRLD-FILE.

           MOVE 0 TO HOLD-BRNO.
           MOVE 0 TO RECORD-COUNTER.
       NEXT-REC.
           MOVE SPACES TO WRLD-REC.
           PERFORM READ-WRLD-FILE.
           IF IO-FG NOT = 0 
              GO TO EXIT-SET-LN.
 
           ADD 1 TO RECORD-COUNTER.

           INSPECT W-BRNOX      REPLACING ALL " " BY "0".
           INSPECT W-NUMBERX    REPLACING ALL " " BY "0".

           IF HOLD-BRNO NOT = W-BRNO
              MOVE W-BRNO TO HOLD-BRNO BR-NO
              PERFORM GET-LN-PATH
              IF IO-FG NOT = 0
                 MOVE 0                TO HOLD-BRNO
                 GO TO NEXT-REC
              ELSE
                 PERFORM OPEN-LN1-FILE
                 PERFORM OPEN-LSM1-FILE.

           MOVE W-BRNO   TO LN-OWNBR.
           MOVE W-NUMBER TO LN-ACCTNO.
           PERFORM READ-LN1-FILE.
           IF IO-FG NOT = 0
              MOVE BR-NO             TO D-BRNO
              MOVE W-NUMBER          TO D-NUMBER
              MOVE "MISSING ACCOUNT" TO D-TEXT
              PERFORM WRITE-DETAIL-LINE
              GO TO NEXT-REC.

           IF REPORT-ONLY = "Y"
              GO TO NEXT-REC.

           MOVE LN-OWNBR  TO LSM-BRNO.
           MOVE LN-ACCTNO TO LSM-ACCTNO.
           MOVE 1         TO LSM-SEQNO.
           PERFORM READ-LSM1-FILE.
           IF IO-FG = 0
              MOVE W-SELLER TO LSM-MISC-FIELD(5)
              PERFORM REWRITE-LSM1-FILE
           ELSE
              PERFORM CLEAR-LSM-FILE
              MOVE LN-OWNBR  TO LSM-BRNO
              MOVE LN-ACCTNO TO LSM-ACCTNO
              MOVE 1         TO LSM-SEQNO
              MOVE W-SELLER  TO LSM-MISC-FIELD(5)
              PERFORM WRITE-LSM1-FILE.

           ADD 1 TO RECORDS-WRITTEN.

           GO TO NEXT-REC.

       EXIT-SET-LN.
           EXIT.


      ******************************************************************
      *
      *    I N I T I A L I Z A T I O N
      *
      ******************************************************************
       INITIALIZATION SECTION.
           MOVE " " TO ERRCD.

       GETPR.
           MOVE K2NAME-QUEUE-POS  TO Q-POS.
           MOVE K2NAME-COPIES-POS TO C-POS.
           MOVE K2NAME-PRU-POS    TO P-POS.
           MOVE SPACES            TO Q-BUF C-BUF P-BUF.

           PERFORM PRU-IN.
           IF STAT NOT = "00"
              GO TO ENDIT.

       REPORT-YN.
           MOVE "E  A010REPONLY." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO GETPR.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" GO TO REPORT-YN.
           MOVE VDUBUF TO REPORT-ONLY.
           IF NOT (REPORT-ONLY = "Y" OR "N")
              GO TO REPORT-YN.

       OK-YN.
           MOVE "E  A010SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO GETPR.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS = "1U" GO TO REPORT-YN.
           IF VDUSTATUS NOT = "00" GO TO OK-YN.
           IF VDUBUF = "N" GO TO ENDIT.
           IF VDUBUF NOT = "Y" GO TO OK-YN.

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

           MOVE "REPORT ONLY ?" TO R-COUNTERX.
           MOVE REPORT-ONLY     TO R-COUNTER-ALPHA.
           MOVE 4 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "TOTAL RECORDS......." TO R-COUNTERX.
           MOVE RECORD-COUNTER         TO R-COUNTER.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "RECORDS FIXED....." TO R-COUNTERX.
           MOVE RECORDS-WRITTEN      TO R-COUNTER.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.


       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/LPLSMCL.CPY".

      ****************************************************
       FORM-RESET SECTION.
      ****************************************************
           MOVE EXT-CONAME-CONAME TO K2NAME-CONAME.
           MOVE EXT-CONAME-SYSDATE TO K2NAME-SYSDATE.
           DISPLAY K2NAME-SCREEN UPON K2NAME-WINDOW-HANDLE.
           MOVE K2NAME-FORM-FIELDS TO WR-BUF. 
           MOVE K2NAME-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM. 


      ****************************************************
       SETUP-LINK-INFO SECTION.
      ****************************************************
           MOVE K2NAME-HELP--FILE TO HELP-LINK-FILE.
           MOVE K2NAME-HELP--DIR  TO HELP-LINK-DIR.

      ****************************************************
       VDU-CONVERT-FIELD SECTION.
      ****************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBFX/K2NAME_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      /***********************************
      *    MISC. PARAGRAPH SECTION
      ************************************
       MISC-PARAGRAPH SECTION.
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/GETENV.CPY".
       GET-LN-PATH.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-LSM1-FILE.

           PERFORM READ-BR-FILE.
           IF IO-FG = 0
              MOVE EXT-FILPATH-BASE TO FILPATH-BASE
              MOVE BR-MACHINE       TO FILPATH-MACHINE
              MOVE BR-DATA-PATH     TO FILPATH-DATA
              MOVE "B"              TO LN-PATH-OVERRIDE
                                       LSM-PATH-OVERRIDE
           ELSE
              MOVE W-BRNO TO D-BRNO
              MOVE W-NUMBER TO D-NUMBER
              MOVE "INVALID BRANCH NO." TO D-TEXT
              PERFORM WRITE-DETAIL-LINE
              MOVE 9 TO IO-FG.


      /*******************************
      *      FILE I-O ROUTINES
      *********************************
       IO-ROUTINE SECTION.
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPLSMGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       OPEN-WRLD-FILE.
           PERFORM OPEN-IT.
           MOVE "K2NAME" TO E-FILE.
           OPEN INPUT WRLD-FILE.
           IF IO-FG = 8
              GO TO OPEN-WRLD-FILE.
           IF IO-FG = 7
              CLOSE WRLD-FILE
              GO TO OPEN-WRLD-FILE.
       READ-WRLD-FILE.
           PERFORM READ-IT.
           MOVE "K2NAME" TO E-FILE.
           READ WRLD-FILE.
           IF IO-FG = 8
              GO TO READ-WRLD-FILE.
       CLOSE-WRLD-FILE.
           PERFORM CLOSE-IT.
           CLOSE WRLD-FILE.
       COPY "LIBLP/LPLN1IN.CPY".
       COPY "LIBLP/LPLSM1IN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.
 
       END-PROGRAM SECTION.
       END-RUN.
           PERFORM CLOSE-FILES.
           PERFORM PRU-OUT.

       EXIT-PROG.
           MOVE "FX/FXMENU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY K2NAME-SCREEN.
           DESTROY K2NAME-WINDOW-HANDLE.
           EXIT PROGRAM.
