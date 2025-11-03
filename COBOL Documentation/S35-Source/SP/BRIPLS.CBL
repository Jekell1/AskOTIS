       IDENTIFICATION DIVISION.
       PROGRAM-ID. BRIPLS.
      *****************************************************************
      *                   (SP)
      *        LIST BRANCH INSURANCE PRODUCERS
      *
      * REV:
      * BLV 040421 FIX PRODUCER FILE KEY TO BE PRODUCER CODE & BRANCH NO:
      *            DUPLICATE CODES WEREN'T GETTING WRITTEN TO FILE SO THEY
      *            DIDN'T PRINT
      *************************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
           SELECT PRODUCER-FILE ASSIGN PRODUCER-PATH
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY PRODUCER-KEY
                  FILE STATUS FILE-STAT.

       DATA DIVISION.
       FILE SECTION.

       COPY "LIBGB/GBFDPR.CPY".

       FD  PRODUCER-FILE
           LABEL RECORDS ARE STANDARD.
       01  PRODUCER-REC.
           03  PRODUCER-KEY.
               05  PRODUCER-CODE          PIC 9(6).
               05  PRODUCER-BRNO          PIC 9(4).

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)SP/BRIPLS.CBL S35 04/15/22-15:31:14 >".

       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".

       01  PR-IFN             PIC X(3)   VALUE "PR".
       01  PRODUCER-IFN       PIC X(8)   VALUE "PRODUCER".

       01  PRODUCER-PATH.
           03  PRODUCER-PATH-BASE          PIC X(09).
           03  FILLER                      PIC X(01) VALUE "/".
           03  PRODUCER-PATH-MACHINE       PIC X(02).
           03  FILLER                      PIC X(28)
                                           VALUE "/ED/PRODUCER".

       01  LN-WORKERS.
           03  LN-FEED        PIC 999   VALUE 0.
           03  LN-COUNT       PIC 999   VALUE 99.
           03  LN-WK          PIC 999   VALUE 0.
       01  PAGE-NUMBER        PIC 999   VALUE 0.

       01  HOLD-PRODUCER      PIC 9(6) VALUE 0.

       01  WORK-ADR           PIC X(80).
       01  FIRST-TIME         PIC X VALUE "Y".
       01  RECORDS-FOUND      PIC X VALUE " ".
           88  NO-RECORDS-FOUND     VALUE "N".
       01  TOTAL-PRODUCERS    PIC 9(5) VALUE 0.
       01  TOTAL-DUPS         PIC 9(5) VALUE 0.

       01  MESS-LIST         PIC X(5) VALUE "MESS.".

       COPY "LIBGB/EOBUF_EXT.CPY".

       01  VERSION-CONTROL    PIC X VALUE "A".

       01  HEAD-1.
           03  HD-DATE        PIC X(8).
           03  FILLER         PIC X(3)    VALUE SPACES.
           03  HD-TIME        PIC X(8)    VALUE SPACES.
           03  FILLER         PIC X(30)   VALUE SPACES.
           03  HD-CONAME      PIC X(40).
           03  FILLER         PIC X(34)   VALUE SPACES.
           03  FILLER         PIC X(5)    VALUE "PAGE ".
           03  HD-PAGE        PIC ZZ9.

       01  HEAD-2.
           03  FILLER         PIC X(9)    VALUE "USERID:".
           03  HD-USERID      PIC X(10)   VALUE SPACES.
           03  FILLER         PIC X(35)   VALUE SPACES.
           03  FILLER         PIC X(34)   VALUE
               "BRANCH INSURANCE PRODUCER LISTING".

       01  HEAD-3.
           03  FILLER        PIC X(15) VALUE "NOTES ".
           03  FILLER        PIC X(10) VALUE "PRODUCER ".
           03  FILLER        PIC X(8) VALUE "BRANCH ".
           03  FILLER        PIC X(50) VALUE SPACES.

       01  DETAIL-LINE           VALUE SPACES.
           03  D-MEMO         PIC X(15).
           03  D-PRODUCER.
               05  D-PRODUCER-NUM PIC 9(8).
           03  FILLER         PIC XX.
           03  D-BRNO         PIC 9(4).
           03  FILLER         PIC X.
           03  D-BRNAME       PIC X(30).
           03  FILLER         PIC XX.
           03  D-BRADDR       PIC X(70).

       01  TOTAL-LINE REDEFINES DETAIL-LINE.
           03  FILLER         PIC X(20).
           03  D-TOT-TAG      PIC X(24).
           03  D-TOT-NO       PIC ZZZ,ZZ9.
           03  FILLER         PIC X(81).

       COPY "LIBGB/SYSTEMW.CPY".

       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/BRIPLS_DEF.CPY".
       COPY "LIBSP/BRIPLS_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBSP/BRIPLS_SCN.CPY".

      *-----------------------------------------------------------------
       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON 
                            PR-FILE PRODUCER-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE PRODUCER-FILE.
       COPY "LIBGB/DECLRP2.CPY".

     *****************************************************
       MAIN-PROGRAM SECTION.
     *****************************************************
           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "BRIPLS" TO VDU-FORM-NAME.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

      ****************************************************
      * THIS BUILDS AN INDEX FILE BY PRODUCER NUMBER, TO
      * EASILY FIND THE CORRESPONDING BRANCH NUMBER, SO EACH TIME
      * WE ARENT SEARCHING THE SYSTEM FOR WHAT BRANCH
      ****************************************************
           PERFORM BUILD-PRODUCER-BRNO-FILE.

           PERFORM OPEN-PRODUCER-FILE.

           MOVE 0 TO HOLD-PRODUCER.

       NEXT-PRODUCER.
           PERFORM READ-PRODUCER-FILE-NEXT.
           IF IO-BAD
              PERFORM GRAND-TOTALS
              GO TO END-ROUTINE.

           IF HOLD-PRODUCER NOT = 0
              IF PRODUCER-CODE = HOLD-PRODUCER
                 MOVE "DUPLICATE!!!" TO D-MEMO
                 ADD 1 TO TOTAL-DUPS.

           ADD 1 TO TOTAL-PRODUCERS.

           MOVE PRODUCER-CODE TO HOLD-PRODUCER D-PRODUCER-NUM.
           MOVE PRODUCER-BRNO TO D-BRNO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-BAD
              MOVE "****INVALID BRANCH*****" TO D-BRNAME
              GO TO PRINT-DET-LINE.

           MOVE BR-NAME TO D-BRNAME.
           MOVE BR-ADR1 TO WORK-ADR.
           IF BR-ADR2 NOT = SPACES
              INSPECT WORK-ADR REPLACING FIRST
                 "  " BY ", "
              INSPECT WORK-ADR REPLACING FIRST
                 "                        " BY BR-ADR2.
           INSPECT WORK-ADR REPLACING FIRST
              "  " BY ", ".
           INSPECT WORK-ADR REPLACING FIRST
              "               " BY BR-CITY.
           INSPECT WORK-ADR REPLACING FIRST
              "  " BY ", ".
           INSPECT WORK-ADR REPLACING FIRST
              "  " BY BR-STATE.
           INSPECT WORK-ADR REPLACING FIRST
              "          " BY BR-ZIP AFTER INITIAL "  ".
           MOVE WORK-ADR TO D-BRADDR.

       PRINT-DET-LINE.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           GO TO NEXT-PRODUCER.

       END-ROUTINE.
           IF FIRST-TIME = "Y"
              MOVE "N" TO RECORDS-FOUND
              GO TO END-PROGRAM.

           MOVE SPACES TO PR-REC.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           GO TO END-PROGRAM.

      ********************************
       BUILD-PRODUCER-BRNO-FILE SECTION.
      ********************************
           MOVE EXT-FILPATH-BASE      TO PRODUCER-PATH-BASE.
           MOVE EXT-FILPATH-MACHINE   TO PRODUCER-PATH-MACHINE.
           PERFORM OPEN-PRODUCER-FILE-OUTPUT.
           PERFORM CLOSE-PRODUCER-FILE.
           PERFORM OPEN-PRODUCER-FILE.

           MOVE 1 TO BR-NO.
           PERFORM START-BR-FILE.
       BUILD-NEXT-PRODUCER.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-FG NOT = 0
              GO TO EXIT-BUILD-PRODUCER.

           IF BR-INS-PRODUCER-NUM = 0
              GO TO BUILD-NEXT-PRODUCER.

           MOVE BR-INS-PRODUCER-NUM TO PRODUCER-CODE.
           MOVE BR-NO               TO PRODUCER-BRNO.
           PERFORM WRITE-PRODUCER-FILE.

           GO TO BUILD-NEXT-PRODUCER.
       EXIT-BUILD-PRODUCER.
           PERFORM CLOSE-PRODUCER-FILE.

      *******************
       HEAD-PAGE SECTION.
      *******************
           MOVE "N" TO FIRST-TIME.
           ADD 1 TO PAGE-NUMBER.
           MOVE PAGE-NUMBER TO HD-PAGE.
           PERFORM GET-TIME.
           MOVE TIME-EDIT TO HD-TIME.

           MOVE HEAD-1 TO PR-REC.
           MOVE 99 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE HEAD-2 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE HEAD-3 TO PR-REC.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE 3 TO LN-COUNT.
           MOVE 2 TO LN-FEED.

      ************************
       GRAND-TOTALS SECTION.
      ************************

           MOVE "GRAND TOTALS" TO TOTAL-LINE.

           MOVE "TOTAL PRODUCERS :" TO D-TOT-TAG.
           MOVE TOTAL-PRODUCERS TO D-TOT-NO.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "TOTAL DUPLICATES:" TO D-TOT-TAG.
           MOVE TOTAL-DUPS TO D-TOT-NO.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

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

      ****************************
       INITIALIZATION SECTION.
      ****************************
           MOVE BRIPLS-QUEUE-POS TO Q-POS.
           MOVE BRIPLS-COPIES-POS TO C-POS.
           MOVE BRIPLS-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
       COPY "LIBGB/EOINIT.CPY".

           PERFORM OPEN-BR-FILE.

           IF EXT-ROUTE-BUF = "00"
              MOVE "REPORT IN PROGRESS..............." TO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           PERFORM OPEN-PR-FILE.

           MOVE EXT-CONAME-CONAME TO HD-CONAME.
           MOVE EXT-CONAME-SYSDATE TO HD-DATE.
           MOVE EXT-CONAME-USER-ID TO HD-USERID.

           GO TO EXIT-INITIALIZE.

       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.
       EXIT-INITIALIZE.
           EXIT.

      ****************************
       EO-EXEC-AUDIT SECTION.
      ****************************
           EXIT.

      ****************************
       ENTER-PARAMETERS SECTION.
      ****************************
           EXIT.
       EXIT-PARM.
           EXIT.

      ****************************
       DISPLAY-PARAMETERS SECTION.
      ****************************
           EXIT.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/TIMEIO.CPY".

      ************************************************
       FORM-RESET SECTION.
      ************************************************
           MOVE EXT-CONAME-CONAME TO BRIPLS-CONAME.
           MOVE EXT-CONAME-SYSDATE TO BRIPLS-SYSDATE.
           DISPLAY BRIPLS-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE BRIPLS-FORM-FIELDS TO WR-BUF.
           MOVE BRIPLS-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE BRIPLS-HELP--FILE TO HELP-LINK-FILE.
           MOVE BRIPLS-HELP--DIR TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/BRIPLS_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ****************************
       MISC SECTION.
      ****************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/SYSTEM.CPY".

      *********************************
       IO-ROUTINES SECTION.
      *********************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRRN.CPY".

       OPEN-PRODUCER-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE PRODUCER-IFN TO E-FILE.
           OPEN OUTPUT PRODUCER-FILE.
           IF IO-FG = 8
              GO TO OPEN-PRODUCER-FILE-OUTPUT.
           IF IO-FG = 7
              CLOSE PRODUCER-FILE
              GO TO OPEN-PRODUCER-FILE-OUTPUT.
           UNLOCK PRODUCER-FILE.
       OPEN-PRODUCER-FILE.
           PERFORM OPEN-IT.
           MOVE PRODUCER-IFN TO E-FILE.
           OPEN I-O PRODUCER-FILE.
           IF IO-FG = 8
              GO TO OPEN-PRODUCER-FILE.
           IF IO-FG = 7
              CLOSE PRODUCER-FILE
              GO TO OPEN-PRODUCER-FILE.
           UNLOCK PRODUCER-FILE.
       READ-PRODUCER-FILE.
           PERFORM READ-IT.
           MOVE PRODUCER-IFN TO E-FILE.
           MOVE PRODUCER-KEY TO E-KEYX.
           READ PRODUCER-FILE.
           IF IO-FG = 8
              GO TO READ-PRODUCER-FILE.
       READ-PRODUCER-FILE-NEXT.
           PERFORM READ-IT.
           MOVE PRODUCER-IFN TO E-FILE.
           MOVE PRODUCER-KEY TO E-KEYX.
           READ PRODUCER-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-PRODUCER-FILE.
       WRITE-PRODUCER-FILE.
           PERFORM WRITE-IT.
           MOVE PRODUCER-IFN TO E-FILE.
           MOVE PRODUCER-KEY TO E-KEYX.
           WRITE PRODUCER-REC.
           IF IO-FG = 8
              GO TO WRITE-PRODUCER-FILE.
           UNLOCK PRODUCER-FILE.
       CLOSE-PRODUCER-FILE.
           PERFORM CLOSE-IT.
           CLOSE PRODUCER-FILE.
       COPY "LIBGB/FERRORS.CPY".

      *********************************
       END-PROGRAM SECTION.
      *********************************
       END-RUN.
           PERFORM CLOSE-FILES.
           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.
           IF NO-RECORDS-FOUND
              MOVE "NO RECORDS FOUND FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS.

       EXIT-PROG.
           IF EXT-ROUTE-BUF = "00"
              MOVE "SP/WRLDMU" TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY BRIPLS-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
