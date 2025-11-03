       IDENTIFICATION DIVISION.
       PROGRAM-ID.      CDEXTR.
       DATE-WRITTEN.    111511.
      ******************************************************************
      *                      (SP)
      *    DESCRIPTION:  DATA WAREHOUSE CODE FILES EXTRACTION
      *                    CDFILE AND CDBFILE
      *
      * NOTE: ALL DATES ARE YYYYMMDD
      *       ALL INCLUDE CR,LF
      *
      * CREATES OUTPUT FILE /USR/EXTRACT/CODES   RSZ=
      *  ALL CODES FROM CDFILE WITH A DESCRIPTION
      *
      * REV:
      * BAH 111114 NEW REGACC PR# 301
      * BAH 150615 EXPANDED XCD-KEY TO 9(9) FROM 9(7), WORLD PR# 1088
      *
      * KEC 2016.0309 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED CD-TYPE "LO" TO USE NEW LAYOUT (CD-LO-REC).
      *         CHANGED CD-CL???? TO BE CD-CL-????.
      *         CHANGED CD-ACTION???? TO BE CD-ACTION-????.
      *         CHANGED CD-AC-RE-REC TO BE SPLIT INTO CD-AT-REC &
      *         CD-RE-REC.  FIELDS CD-??-ACTIONCD, CD-??-MEMO-REQUIRED,
      *         CD-??-SUPERVISOR-ONLY ARE IDENTICAL (LOCATION, NAME,
      *         SIZE) IN SITUATIONS WERE NOT SURE WHICH TYPE CD-REC
      *         WAS READ.
      *         CHANGED CD-FC???? TO CD-FC-????
      *         CHANGED CD-POOL???? TO CD-PL-????
      *         CHANGED CD-PX???? TO CD-PX-????
      *         CHANGED CD-RC-CODE TO NO LONGER USE HASH.
      *         REMOVED CD-RC-CODE-ALPHA; USING CD-RC-CODE NOW.
      *
      * KEC 2018.0511 PR#00000 PARADATA A30 JAY & BETH
      *         REMOVED USE OF ENT-GROUP AND/OR GROUP-FLAG.
      *         THIS WAS DONE IN RELATION TO THE EOBUF EXTERNAL ISSUE
      *         WHICH IF NOT BEING SET WILL CAUSE ISSUES FROM THOSE
      *         BYTE POSITIONS COULD BE SET FROM PREVIOUS
      *         PROGRAM WITH EOBUF THAT SET SOMETHING IN THOSE BYTES.
      *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
       COPY "LIBEX/EXFSXCD.CPY".
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".
       COPY "LIBEX/EXFDXCD.CPY".
      ******************************************************************
      *
      *     W O R K I N G - S T O R A G E   S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)SP/CDEXTR.CBL S35 05/02/23-10:17:18 >".

      ******************************************************************
      *
      *     F I L E   S P E C S
      *
      ******************************************************************

       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LP01CD_SQL.CPY".
       COPY "LIBLP/LPWSCD.CPY".
       COPY "LIBLP/LP01CDB.CPY".
       COPY "LIBLP/LP01CDB_SQL.CPY".
       COPY "LIBLP/LPWSCDB.CPY".
      *----------------------------------------------------------------
       01  XCD-KEY       PIC 9(9)     VALUE 0.

       01  XCD-PATH.
           03  XCD-DIRECTORY      PIC X(13) VALUE "/USR/EXTRACT/".
           03  XCD-FILENAME       PIC X(5) VALUE  "CODES".

      ******************************************************************
      *
      *   LINE WORKERS
      *
      ******************************************************************
       01  LN-WORKERS.
           03  LN-FEED          PIC 99       VALUE 0.
           03  LN-COUNT         PIC 99       VALUE 61.
           03  LN-WK            PIC 99       VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

       01  WS-POOLID            PIC 9(5).
       01  WS-POOLIDX REDEFINES WS-POOLID PIC X(5).

       01  EXITPROG             PIC X(9)     VALUE "SP/DWEXMU".

       01  STARTING-TIME        PIC X(8)     VALUE SPACES.


      ******************************************************************
      *
      *     S C R E E N   F I E L D S
      *
      ******************************************************************
       COPY "LIBGB/EOBUF_EXT.CPY".
      *-----------------------------------------------------------------

       01  VERSION-CONTROL     PIC X    VALUE "A".

      *-----------------------------------------------------------------
       COPY "LIBGB/PASSWDW.CPY".
      *-----------------------------------------------------------------

      ******************************************************************
      *
      *     P R I N T   B U F F E R S
      *
      ******************************************************************
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

      *-----------------------------------------------------------------
       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(23)    VALUE " ".
           03  H-TITLE          PIC X(28)    VALUE
           "CODE FILE EXTRACTION".


      **********************
      *   MISC WORKERS     *
      **********************
       01  FIRST-TIME           PIC X        VALUE "Y".
       01  RECORDS-FOUND        PIC X        VALUE " ".
           88  NO-RECORDS-FOUND              VALUE "N".
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".

       01  RECORD-COUNT      PIC 9(9)      VALUE 0.

       01  DETAIL-LINE          PIC X(132) VALUE SPACES.

       01  RANGE-LN REDEFINES DETAIL-LINE.
           03  RANGE-LINE        PIC X(22).
           03  RANGE-SELECTION   PIC X(110).

       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/CDEXTR_DEF.CPY".
       COPY "LIBSP/CDEXTR_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *****************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      *****************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBSP/CDEXTR_SCN.CPY".

      *----------------------------------------------------------------
       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE XCD-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-CDB1-FILE.
           PERFORM CLOSE-CD1-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE
               XCD-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ******************************************************************
      *
      *    M A I N   M O D U L E
      *
      ******************************************************************
       MAIN-MODULE SECTION.
           PERFORM SQL-CONNECT.
      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "CDEXTR" TO VDU-FORM-NAME.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

           PERFORM OPEN-CD1-FILE.

           MOVE SPACES TO CD-TYPE CD-CODE.
           PERFORM START-CD1-FILE.

      *----------------------------------------------------------------
       NEXT-CD.
           PERFORM READ-CD1-FILE-NEXT.
           IF IO-FG = 9
              GO TO PROCESS-CDB-FILE.

           IF CD-TYPE = "CT" OR "CY" OR "GC" OR "LR" OR "LV"
              GO TO NEXT-CD.

           PERFORM FILL-WORK-FLDS.

           GO TO NEXT-CD.

      *----------------------------------------------------------------
       PROCESS-CDB-FILE.
           PERFORM OPEN-CDB1-FILE.

           MOVE SPACES TO CDB-TYPE CDB-CODE.
           MOVE 0      TO CDB-BRANCH.
           PERFORM START-CDB1-FILE.

      *----------------------------------------------------------------
       NEXT-CDB.
           PERFORM READ-CDB1-FILE-NEXT.
           IF IO-FG = 9
              GO TO END-ROUTINE.


           IF CDB-TYPE = "GC" 
              GO TO NEXT-CDB.

           PERFORM FILL-WORK-CDB-FLDS.

           GO TO NEXT-CDB.


       END-ROUTINE.

           IF NO-RECORDS-FOUND
              GO TO END-PROGRAM.

           PERFORM HEAD-PAGE.
           PERFORM PRINT-RANGE-LINES.

           GO TO END-PROGRAM.


      *****************************
       FILL-WORK-FLDS SECTION.
      *****************************
           MOVE SPACES              TO XCD-REC.

           MOVE CD-TYPE TO XCD-TYPE.
           MOVE CD-CODE TO XCD-CODE.
           MOVE CD-DESC TO XCD-DESCRIPTION.

           IF CD-TYPE = "AC"
              MOVE "ACTION CODE"  TO XCD-TYPE-DESC
           ELSE
           IF CD-TYPE = "AS"
              MOVE "ADVERTISING SOURCE" TO XCD-TYPE-DESC
           ELSE
           IF CD-TYPE = "AT"
              MOVE "ACTIVITY CODE" TO XCD-TYPE-DESC
           ELSE
           IF CD-TYPE = "BR"
              MOVE "BATCH REFERENCE" TO XCD-TYPE-DESC
           ELSE
           IF CD-TYPE = "BS"
              MOVE "PAY REFERENCE" TO XCD-TYPE-DESC
           ELSE
           IF CD-TYPE = "BW"
              MOVE "BORROWER HOUSING" TO XCD-TYPE-DESC
           ELSE
           IF CD-TYPE = "CH"
              MOVE "CHARGE OFF" TO XCD-TYPE-DESC
           ELSE
           IF CD-TYPE = "CL"
              MOVE "CAN LOAN GROUP" TO XCD-TYPE-DESC
           ELSE
           IF CD-TYPE = "CO"
              MOVE "COLLATERAL CODE" TO XCD-TYPE-DESC
           ELSE
           IF CD-TYPE = "DF"
              MOVE "DEFERMENT REASON" TO XCD-TYPE-DESC
           ELSE
           IF CD-TYPE = "EA"
              MOVE "EXTRACTION/ACTION GROUP" TO XCD-TYPE-DESC
           ELSE
           IF CD-TYPE = "EL"
              MOVE "EXTENDED LCHG INFO" TO XCD-TYPE-DESC
              MOVE CD-EL-CODE TO XCD-CODE
           ELSE
           IF CD-TYPE = "FC"
              MOVE "FORM CHARGES" TO XCD-TYPE-DESC
           ELSE
           IF CD-TYPE = "FU"
              MOVE "FOLLOW-UP REASON" TO XCD-TYPE-DESC
           ELSE
           IF CD-TYPE = "FZ"
              MOVE "FROZEN REASON" TO XCD-TYPE-DESC
           ELSE
           IF CD-TYPE = "ID"
              MOVE "KNOW YOU CUSTOMER ID" TO XCD-TYPE-DESC
           ELSE
           IF CD-TYPE = "JT"
              MOVE "JOB TYPE" TO XCD-TYPE-DESC
           ELSE
           IF CD-TYPE = "LO"
              MOVE "LPO " TO XCD-TYPE-DESC
           ELSE
           IF CD-TYPE = "LS"
              MOVE "REPO REASON" TO XCD-TYPE-DESC
           ELSE
           IF CD-TYPE = "ML"
              MOVE "MAIL LIST" TO XCD-TYPE-DESC
           ELSE
           IF CD-TYPE = "PA"
              MOVE "PAYROLL TIMESHEET" TO XCD-TYPE-DESC
           ELSE
           IF CD-TYPE = "PC"
              MOVE "PROCESSING CODE" TO XCD-TYPE-DESC
           ELSE
           IF CD-TYPE = "PH"
              MOVE "PHONE NUMBER" TO XCD-TYPE-DESC
           ELSE
           IF CD-TYPE = "PL"
              MOVE "POOL ID" TO XCD-TYPE-DESC
              MOVE CD-PL-POOLID TO WS-POOLID
              MOVE WS-POOLIDX TO XCD-CODE
           ELSE
           IF CD-TYPE = "PO"
              MOVE "PAYOFF" TO XCD-TYPE-DESC
           ELSE
           IF CD-TYPE = "PR"
              MOVE "PRODUCT CODE" TO XCD-TYPE-DESC
           ELSE
           IF CD-TYPE = "PS"
              MOVE "PAYMENT RESCHEDULE" TO XCD-TYPE-DESC
           ELSE
           IF CD-TYPE = "PU"
              MOVE "PURPOSE" TO XCD-TYPE-DESC
           ELSE
           IF CD-TYPE = "PX"
              MOVE "P&L TYPE CODES" TO XCD-TYPE-DESC
           ELSE
           IF CD-TYPE = "RC"
              MOVE "REP CODES" TO XCD-TYPE-DESC
           ELSE
           IF CD-TYPE = "RE"
              MOVE "COLLECTION RESULT" TO XCD-TYPE-DESC
           ELSE
           IF CD-TYPE = "RF"
              MOVE "TURN DOWN" TO XCD-TYPE-DESC
           ELSE
           IF CD-TYPE = "SR"
              MOVE "SOURCE" TO XCD-TYPE-DESC
           ELSE
           IF CD-TYPE = "TC"
              MOVE "TRANSACTION REF" TO XCD-TYPE-DESC
           ELSE
           IF CD-TYPE = "TS"
              MOVE "TRW SPECIAL CODES" TO XCD-TYPE-DESC
           ELSE
           IF CD-TYPE = "UW"
              MOVE "UNWIND" TO XCD-TYPE-DESC.

           MOVE X"0D"                   TO XCD-CR.
           MOVE X"0A"                   TO XCD-LF.

           ADD 1 TO XCD-KEY.
           PERFORM WRITE-XCD-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

           ADD 1 TO RECORD-COUNT.

           MOVE "N" TO FIRST-TIME.
           MOVE "Y" TO RECORDS-FOUND.

       FILL-WORK-FLDS-EXIT.
           EXIT.


      *****************************
       FILL-WORK-CDB-FLDS SECTION.
      *****************************
           MOVE SPACES              TO XCD-REC.

           MOVE CDB-TYPE   TO XCD-TYPE.
           MOVE CDB-CODE   TO XCD-CODE.
           MOVE CDB-DESC   TO XCD-DESCRIPTION.
           MOVE CDB-BRANCH TO XCD-BRANCH.

      * "CN"
           MOVE "COLLECTOR ID" TO XCD-TYPE-DESC

           MOVE X"0D"                   TO XCD-CR.
           MOVE X"0A"                   TO XCD-LF.

           ADD 1 TO XCD-KEY.
           PERFORM WRITE-XCD-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

           ADD 1 TO RECORD-COUNT.

           MOVE "N" TO FIRST-TIME.
           MOVE "Y" TO RECORDS-FOUND.

       FILL-WORK-CDB-FLDS-EXIT.
           EXIT.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************
           MOVE CDEXTR-QUEUE-POS TO Q-POS.
           MOVE CDEXTR-COPIES-POS TO C-POS.
           MOVE CDEXTR-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           PERFORM GET-TIME.
           MOVE TIME-EDIT TO STARTING-TIME.

           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           IF EXT-ROUTE-BUF = "70"
              MOVE EXT-EOBUF-MESS TO H-TITLE.

           MOVE " " TO EXT-EOBUF-ERRCD.

           IF EXT-ROUTE-BUF = "00"
              MOVE "EXTRACTION IN PROGRESS................" TO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           PERFORM OPEN-PR-FILE.

           CALL "C$TOLOWER" USING  XCD-DIRECTORY, VALUE 13.
           PERFORM OPEN-XCD-FILE-OUTPUT.
           PERFORM CLOSE-XCD-FILE.
           PERFORM OPEN-XCD-FILE.

           GO TO EXIT-INITIALIZE.

       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.

       EXIT-INITIALIZE.
           EXIT.

      ******************************************************************
       EO-EXEC-AUDIT SECTION.
      ******************************************************************

       EXIT-EO-EXEC-AUDIT.
           EXIT.

      ******************************************************************
       ENTER-PARAMETERS SECTION.
      ******************************************************************
       ENTER-PARM.

           IF EXT-CONAME-USER-LOC NOT NUMERIC
              MOVE "INVALID USER" TO MESS
              PERFORM SEND-MESS
              GO TO ENDIT.

       EXIT-PARM.
           EXIT.

      ******************************************************************
       DISPLAY-PARAMETERS SECTION.
      ******************************************************************


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

      ******************************************************************
       PRINT-RANGE-LINES SECTION.
      ******************************************************************
           MOVE 4 TO LN-FEED.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "STARTING TIME      :" TO RANGE-LINE.
           MOVE STARTING-TIME         TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "ENDING   TIME      :" TO RANGE-LINE.
           PERFORM GET-TIME.
           MOVE TIME-EDIT             TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "# RECORDS EXTRACTED:" TO RANGE-LINE.
           MOVE RECORD-COUNT TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      ******************************************************************
       HEAD-PAGE SECTION.
      ******************************************************************
           ADD 1 TO PAGE-NUMBER.
           MOVE PAGE-NUMBER TO H-PAGE-NO.
           PERFORM GET-TIME.
           MOVE T-HH TO H-HR.
           MOVE T-MM TO H-MIN.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE HEAD1 TO PR-REC.
           MOVE 99 TO LN-FEED.
           PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE HEAD2 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

       END-HEAD-PAGE.
           MOVE 3 TO LN-COUNT.
           MOVE 2 TO LN-FEED.

      ******************************************************************
      *
      *     C O P Y    L I B R A R Y    S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/TIMEIO.CPY".

      *********************************************************
       FORM-RESET SECTION.
      *********************************************************
           MOVE EXT-CONAME-CONAME TO CDEXTR-CONAME.
           MOVE EXT-CONAME-SYSDATE TO CDEXTR-SYSDATE.
           DISPLAY CDEXTR-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE CDEXTR-FORM-FIELDS TO WR-BUF.
           MOVE CDEXTR-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *********************************************************
       SETUP-LINK-INFO SECTION.
      *********************************************************
           MOVE CDEXTR-HELP--FILE TO HELP-LINK-FILE.
           MOVE CDEXTR-HELP--DIR  TO HELP-LINK-DIR.

      *********************************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/CDEXTR_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISC-PARA SECTION.
      ******************************************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
       SEND-LEGEND.
       INVALID-RANGE.
           MOVE "INVALID RANGE" TO MESS.
           PERFORM SEND-MESS.
       CONDITIONAL-PRINT.
           IF DETAIL-LINE NOT = " "
              MOVE 1 TO LN-FEED
              PERFORM WRITE-DETAIL-LINE.

      ******************************************************************
       IO-ROUTINE SECTION.
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPCDBGS_SQL.CPY".
       COPY "LIBLP/LPCDGS_SQL.CPY".
      ******************************************************************
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBLP/LPCD1RN.CPY".
       COPY "LIBLP/LPCDB1RN.CPY".
       OPEN-XCD-FILE.
           PERFORM OPEN-IT.
           MOVE XCD-PATH TO E-FILE.
           OPEN I-O XCD-FILE.
           IF IO-FG = 8
             GO TO OPEN-XCD-FILE.
           IF IO-FG = 7
             CLOSE XCD-FILE
             GO TO OPEN-XCD-FILE.
      *    UNLOCK XCD-FILE.
       OPEN-XCD-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE XCD-PATH TO E-FILE.
           OPEN OUTPUT XCD-FILE.
           IF IO-FG = 8
             GO TO OPEN-XCD-FILE.
           IF IO-FG = 7
             CLOSE XCD-FILE
             GO TO OPEN-XCD-FILE.
      *    UNLOCK XCD-FILE.
       WRITE-XCD-FILE.
           PERFORM WRITE-IT.
           MOVE XCD-PATH TO E-FILE.
           MOVE XCD-KEY TO E-KEYX.
           WRITE XCD-REC.
           IF IO-FG = 8
             GO TO WRITE-XCD-FILE.
      *    UNLOCK XCD-FILE.
       CLOSE-XCD-FILE.
           PERFORM CLOSE-IT.
           CLOSE XCD-FILE.
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************
       END-RUN.
           PERFORM CLOSE-FILES.

           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.

           IF NO-RECORDS-FOUND
              MOVE "NO RECORDS FOUND FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS.

       EXIT-PROG.

           IF EXT-ROUTE-BUF = "00"
              MOVE EXITPROG TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY CDEXTR-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
