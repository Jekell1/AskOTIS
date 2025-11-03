       IDENTIFICATION DIVISION.
       PROGRAM-ID.      PEVERI.
       AUTHOR.             BAH.
       DATE-WRITTEN.  05-31-2024.
      *****************************************************************
      *    DIR :  SP
      *    DESC:  VERIFY PASSWORD EXCEPTION USERIDS EXIST IN /ETC/PASSWD
      *
      *           FOR KEVIN TO VERIFY THAT USERID'S THAT ARE IN THE
      *           PEFILE (PASSWORD EXCEPTION) TRULY STILL EXIST
      *
      *  REV:
      *   2024.0531 BAH NEW #1659 S35Q-60
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
           SELECT WK-FILE ASSIGN WK-PATH
                  ORGANIZATION LINE SEQUENTIAL
                  ACCESS SEQUENTIAL
                  FILE STATUS FILE-STAT.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".
       FD  WK-FILE  LABEL RECORDS ARE STANDARD.
       01  WK-REC               PIC X(160) VALUE SPACES.

      **********************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)SP/PEVERI S35 06/03/24-11:14:05 >".
       COPY "LIBGB/GB01PE.CPY".
       COPY "LIBGB/GB01PE_SQL.CPY".
       COPY "LIBGB/GBWSPE.CPY".
       01  PR-IFN               PIC X(2)    VALUE "PR".
       01  WK-IFN               PIC X(3)  VALUE "WK1".

       01  WK-PATH             PIC X(35) VALUE SPACES.
      *----------------------------------------------------------------
      *   LINE WORKERS
      *----------------------------------------------------------------
       01  LN-WORKERS.
           03  LN-FEED          PIC 99       VALUE 0.
           03  LN-COUNT         PIC 99       VALUE 3.
           03  LN-WK            PIC 99       VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

      *----------------------------------------------------------------
      *   MISC WORKERS
      *----------------------------------------------------------------
       01  FIRST-TIME           PIC X        VALUE "Y".
       01  RECORDS-FOUND        PIC X         VALUE " ".
           88  NO-RECORDS-FOUND               VALUE "N".
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".

       COPY "LIBGB/EOBUF_EXT.CPY".
       01  VERSION-CONTROL      PIC X VALUE "A".

      *----------------------------------------------------------------
      *     P R I N T   B U F F E R S
      *----------------------------------------------------------------

       01  HEAD1.
           03  H-DATE           PIC X(8)     VALUE " ".
           03  FILLER           PIC XX       VALUE " ".
           03  H-HR             PIC 99       VALUE 0.
           03  FILLER           PIC X        VALUE ":".
           03  H-MIN            PIC 99       VALUE 0.
           03  FILLER           PIC X(8)     VALUE " ".
           03  H-NAME           PIC X(40)    VALUE " ".
           03  FILLER           PIC X(8)     VALUE " ".
           03  FILLER           PIC X(5)     VALUE "PAGE ".
           03  H-PAGE-NO        PIC ZZZ9.

       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(10)    VALUE " ".
           03  FILLER           PIC X(50)    VALUE
           "PASSWORD EXCEPTION VERIFY LOGIN".

       01  HEAD3.
           03  FILLER           PIC X(2)  VALUE SPACES. 
           03  FILLER           PIC X(10)  VALUE "PROGRAM   ".
           03  FILLER           PIC X(3).
           03  FILLER           PIC X(3)   VALUE "SEQ".
           03  FILLER           PIC X(5).
           03  FILLER           PIC X(15)  VALUE "EXCEPTION LOGIN".

       01  DETAIL-LINE          PIC X(80) VALUE SPACES.

       01  D-LINE1 REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(03).
           03  D-PGM            PIC X(10).
           03  FILLER           PIC X(3).
           03  D-SEQ            PIC Z9.
           03  FILLER           PIC X(5).
           03  D-LOGIN          PIC X(8).
           03  FILLER           PIC X(7).

       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBSP/PEVERI_CMD.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/PEVERI_DEF.CPY".
       COPY "LIBSP/PEVERI_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ****************************************
      *     P R O G R A M   C O N T R O L    *
      ****************************************
       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBSP/PEVERI_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE WK-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-PE1-FILE.
           CLOSE PR-FILE WK-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      *****************************************************************
       MAIN-MODULE SECTION.
      *****************************************************************
           PERFORM SQL-CONNECT.

      D    STOP MESS.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "PEVERI" TO VDU-FORM-NAME.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           IF STAT NOT = "00"
              MOVE SPACES TO WK-PATH
           ELSE
              MOVE MKTEMP-BUF TO WK-PATH.

       NEXT-PE.
           PERFORM READ-PE1-FILE-NEXT.
           IF IO-FG = 9
              GO TO END-ROUTINE.

           IF PE-LOGIN = SPACES
              GO TO NEXT-PE.

      * CREATE /USR/TMP/XXX (UNIQUE WKFILE)
           MOVE PE-LOGIN TO GREP-USERID.
           MOVE WK-PATH  TO GREP-WK-PATH.
           MOVE GREP-BUF TO SYSTEM-BUF.
           PERFORM SYSTEM-CALL.

      * DONT CHECK SYSTEM CALL STATUS
      * SYSTEM CALL IS GIVING STAT "01" IF GREP DOESNT FIND LOGIN
      * BUT CREATES A 0 FILE

           PERFORM OPEN-WK-FILE-INPUT.
           PERFORM READ-WK-FILE-NEXT.
           PERFORM CLOSE-WK-FILE.
      * IF GOOD READ, THEN THE USER EXIST IN /ETC/PASSWD
      * IF NO RECORD, THE USER DOES NOT EXIST IN /ETC/PASSWD
           IF IO-FG = 0
              GO TO NEXT-PE.

           PERFORM DETAIL-LINE-PROCESS.

           GO TO NEXT-PE.

       END-ROUTINE.

           GO TO END-PROGRAM.

      *****************************************************************
       INITIALIZATION SECTION.
      *****************************************************************

           MOVE PEVERI-QUEUE-POS  TO Q-POS.
           MOVE PEVERI-COPIES-POS TO C-POS.
           MOVE PEVERI-PRU-POS    TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
        COPY  "LIBGB/EOINIT.CPY".

           ADD 1 EXT-CONAME-RPT-LINES GIVING LN-COUNT.

           PERFORM OPEN-PE1-FILE.

           MOVE SPACES TO PE-PGM PE-LOGIN.
           MOVE 0      TO PE-SEQ.

           PERFORM START-PE1-FILE.

           MOVE EXT-CONAME-CONAME  TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           IF EXT-ROUTE-BUF = "00"
              MOVE "LISTING IN PROGRESS...................." TO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           PERFORM OPEN-PR-FILE.

           GO TO EXIT-INITIALIZE.
       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.
       EXIT-INITIALIZE.
           EXIT.

      *****************************************************************
       DETAIL-LINE-PROCESS SECTION.
      *****************************************************************

           IF FIRST-TIME = "Y"
              PERFORM HEAD-PAGE
              MOVE "N" TO FIRST-TIME.

           MOVE PE-PGM          TO D-PGM.
           MOVE PE-SEQ          TO D-SEQ.
           MOVE PE-LOGIN        TO D-LOGIN.

           PERFORM WRITE-DETAIL-LINE.

       END-DETAIL-LN-PROCESS.
           EXIT.

      ******************************************************************
       WRITE-DETAIL-LINE SECTION.
      ******************************************************************
           ADD LN-FEED LN-COUNT GIVING LN-WK.
           IF LN-WK GREATER THAN EXT-CONAME-RPT-LINES
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

       END-HEAD-PAGE.
           MOVE 2 TO LN-FEED.
           MOVE 3 TO LN-COUNT.


       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".

      ****************************************************
       FORM-RESET SECTION.
      ****************************************************
           MOVE EXT-CONAME-CONAME TO PEVERI-CONAME.
           MOVE EXT-CONAME-SYSDATE TO PEVERI-SYSDATE.
           DISPLAY PEVERI-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE PEVERI-FORM-FIELDS TO WR-BUF.
           MOVE PEVERI-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ****************************************************
       SETUP-LINK-INFO SECTION.
      ****************************************************
           MOVE PEVERI-HELP--FILE TO HELP-LINK-FILE.
           MOVE PEVERI-HELP--DIR  TO HELP-LINK-DIR.

      ****************************************************
       VDU-CONVERT-FIELD SECTION.
      ****************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/PEVERI_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *****************************************************************
       MISC SECTION.
      *****************************************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/RMFILE.CPY".
       DISPLAY-PARAMETERS.
       ENTER-PARAMETERS.
       EO-EXEC-AUDIT.

      *****************************************************************
       IO-ROUTINE SECTION.
      *****************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/GBPEGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBPE1RN.CPY".
       COPY "LIBGB/GBWKLSIO.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      *****************************************************************
       END-PROGRAM SECTION.
      *****************************************************************
       END-RUN.
           PERFORM CLOSE-FILES.
           MOVE WK-PATH TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.
           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.
           IF NO-RECORDS-FOUND
              MOVE "NO RECORDS FOUND FOR SELECTED RANGE!" TO MESS
              PERFORM SEND-MESS.
       EXIT-PROG.
           IF EXT-ROUTE-BUF = "00"
              MOVE "GB/SEMENU" TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY PEVERI-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
