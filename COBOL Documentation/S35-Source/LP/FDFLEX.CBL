       IDENTIFICATION DIVISION.
       PROGRAM-ID.      FDFLEX.
       DATE-WRITTEN.    012085.
      ******************************************************************
      *
      *  DIR : LP
      *  DESC: FLEXIBLE LOAN INQUIRY
      *          CALLED FROM FDMENU (6) AND LPIQMU (7)
      *
      *  REV:
      *   KEC 112596 CHANGED LN-POCD FOR RESCISSION.
      *   GLF 990909 CHANGE SSNO ENTRY TO USE NEW VDUCLASS "X" EDIT
      *   BAH 181207 GLOBAL LNFILE
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.

      /
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LP/FDFLEX.CBL S35 02/20/24-14:06:10 >".
      **********************************
      *       F I L E   I F N          *
      **********************************
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".

       01  IO-TYPE           PIC X(3).
       01  FIRST-SCAN        PIC 9 VALUE 0.
       01  NAME-WORKER       PIC X(40) VALUE SPACES.
       01  NAME              PIC X(16) VALUE SPACES.

       01  NUM-BW            PIC 9(9).
       01  NUM-BW-WK         REDEFINES NUM-BW.
           03  NUM-BW1       PIC 999.
           03  NUM-BW2       PIC 99.
           03  NUM-BW3       PIC 9999.
       01  ALP-BW.
           03  ALP-BW1       PIC 999.
           03  ALP-HYPHEN1   PIC X.
           03  ALP-BW2       PIC 99.
           03  ALP-HYPHEN2   PIC X.
           03  ALP-BW3       PIC 9999.

      ************************************
      *     C O M M O N   M E M O R Y    *
      ************************************
       01  FORM-LINK-AREA          VALUE SPACES.
           03  ERRCD           PIC X.
           03  LOAN-KEY        PIC 9(8).
           03  BORROWER-KEY    PIC 9(9).
           03  BRANCH          PIC 9(4).
           03  FILLER          PIC X.
           03  NEW-PROG-SUB    PIC X.
           03  INQ-LINES       PIC 99.
           03  INQ-FORM        PIC 999.
           03  FDFLEX-WINDOW-HANDLE PIC X(10).

       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/FDFLEX_DEF.CPY".
       COPY "LIBLP/FDFLEX_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBLP/FDFLEX_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

      *********************************
       MAIN-PROGRAM SECTION.
      *********************************

           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".

           MOVE EXT-CONAME-USER-LOC TO BRANCH.

           MOVE "FDFLEX" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
      *             WITH NO SCROLL
      *             WITH NO WRAP
                   HANDLE IS FDFLEX-WINDOW-HANDLE.

           PERFORM FORM-RESET.

       MAIN-MODULE.
           PERFORM ENTER-KEYS.
           IF IO-TYPE = "END"
              GO TO END-ROUTINE.

           IF IO-TYPE = "PST"
              MOVE 15 TO INQ-LINES
              MOVE 000 TO INQ-FORM
              MOVE "LP/LONPS3" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH FORM-LINK-AREA
                                   EXIT-PATHNAME
              CANCEL FORM-PROGX.

           GO TO MAIN-MODULE.

      ************************************
       ENTER-KEYS SECTION.
      ************************************
       ENTER-KEYS-NO.
           MOVE SPACES TO IO-TYPE LN-REC.
           MOVE "ENTER LOAN #, F6-SCAN, F7-EXIT" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "EN N060LNNO." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1<"
              PERFORM ENTER-SCANKEY
              IF IO-TYPE NOT = "PST"
                 GO TO END-ENTER-KEYS
              ELSE
                 GO TO ENTER-BWNO.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              MOVE "X" TO ERRCD
              GO TO END-ENTER-KEYS.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-KEYS-NO.
           IF VDUNUM0 = 0
              GO TO ENTER-KEYS-NO.

           MOVE BRANCH  TO LN-OWNBR.
           MOVE VDUNUM0 TO LN-ACCTNO.

           PERFORM OPEN-LN1-FILE.
           PERFORM READ-LN1-FILE.
           PERFORM CLOSE-LN1-FILE.
           IF IO-FG = 9
              GO TO ENTER-KEYS.

           IF ( LN-POCD-VOID )
              MOVE "ACCOUNT HAS BEEN DELETED" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-KEYS-NO.

           IF ( LN-POCD-INRESCISSION )
              MOVE "ACCOUNT IS IN RESCISSION" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-KEYS-NO.

           IF ( LN-POCD-RESCIND )
              MOVE "ACCOUNT HAS BEEN RESCINDED" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-KEYS-NO.

           MOVE LN-ACCTNO TO LOAN-KEY.
       ENTER-BWNO.
           MOVE "ENTER BORROWER, <ENTER>=1ST BORR, ^=LOAN" TO MESS.
           PERFORM SEND-LEGEND.

           MOVE 0 TO WR-BUF.
           MOVE "BWNO." TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE "ERNX090BWNO." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-KEYS-NO.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              MOVE "X" TO ERRCD
              GO TO END-ENTER-KEYS.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-BWNO.
           IF VDUNUM0 = 0
              MOVE LN-SSNO(1) TO VDUNUM0.
           PERFORM DISPLAY-BORROWER.
           IF IO-FG = 9
              MOVE "NOT FOUND" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BWNO.
           MOVE "PST" TO IO-TYPE.
       END-ENTER-KEYS.
           EXIT.

      *********************************************
       ENTER-SCANKEY SECTION.
      *********************************************
           MOVE "ENTER LOAN #, F6-SCAN" TO MESS.
           PERFORM SEND-LEGEND.

           PERFORM OPEN-LN1-FILE.

           MOVE LN-PATH-OWNBR TO LN-OWNBR.
           MOVE 0             TO LN-ACCTNO.

           PERFORM START-LN1-FILE.
       ENTER-KEY.
           MOVE "EN N080SCANKEY." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCANKEY.
           IF VDUSTATUS = "1<"
              GO TO READ-FILES-NEXT.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-KEY.
           IF VDUNUM0 = 0
              IF FIRST-SCAN NOT = 0
                 MOVE "PST" TO IO-TYPE
                 GO TO CLOSE-SCANKEY
              ELSE
                 GO TO ENTER-KEY.

           MOVE VDUNUM0       TO LN-ACCTNO.
           PERFORM START-LN1-FILE.

       READ-FILES-NEXT.
           PERFORM READ-LN1-FILE-NEXT.
           IF IO-BAD OR (LN-OWNBR NOT = LN-PATH-OWNBR)
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCANKEY.

      *    UNLOCK LN-FILE.
           IF ( LN-POCD-NONLOAN )
              GO TO READ-FILES-NEXT.

           MOVE LN-ACCTNO TO LOAN-KEY.
           MOVE "SN N060LNNO." TO VDU.
           MOVE LN-ACCTNO TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE LN-SSNO(1) TO VDUNUM0.
           PERFORM DISPLAY-BORROWER.

           MOVE 1 TO FIRST-SCAN.
           GO TO ENTER-KEY.
       CLOSE-SCANKEY.
           MOVE 0 TO FIRST-SCAN.
           PERFORM CLOSE-LN1-FILE.
       EXIT-SCANKEY.
           EXIT.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".

      *********************************************************
       FORM-RESET SECTION.
      *********************************************************
           DISPLAY FDFLEX-SCREEN UPON FDFLEX-WINDOW-HANDLE.
           MOVE EXT-CONAME-CONAME TO FDFLEX-CONAME.
           MOVE EXT-CONAME-SYSDATE TO FDFLEX-SYSDATE.
           MOVE FDFLEX-FORM-FIELDS TO WR-BUF.
           MOVE FDFLEX-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *********************************************************
       SETUP-LINK-INFO SECTION.
      *********************************************************
           MOVE FDFLEX-HELP--FILE TO HELP-LINK-FILE.
           MOVE FDFLEX-HELP--DIR  TO HELP-LINK-DIR.

      *********************************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/FDFLEX_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *********************************
       MISC-PARAGRAPHS SECTION.
      *********************************
       COPY "LIBGB/ACCESS.CPY".
       SEND-LEGEND.
           MOVE "S  A000MSEL." TO VDU.
           MOVE MESS TO VDUBUF.
           PERFORM SCREENIO.
       NUMBW-TO-ALPBW.
           MOVE NUM-BW1 TO ALP-BW1.
           MOVE NUM-BW2 TO ALP-BW2.
           MOVE NUM-BW3 TO ALP-BW3.
           MOVE "-" TO ALP-HYPHEN1 ALP-HYPHEN2.
       DISPLAY-BORROWER.
           MOVE VDUNUM0 TO BW-SSNO BORROWER-KEY NUM-BW.
           MOVE "S  X000BWNO." TO VDU.
           MOVE BORROWER-KEY TO VDU-SSNO-EDIT.
           PERFORM SCREENIO.
           PERFORM OPEN-BW1-FILE
           PERFORM READ-BW1-FILE
           PERFORM CLOSE-BW1-FILE
           MOVE "S  A000BWNOX." TO VDU.
           MOVE BW-LNAME TO NAME-WORKER.
           MOVE BW-FNAME TO NAME.
           INSPECT NAME-WORKER REPLACING FIRST "   " BY ", +".
           INSPECT NAME-WORKER REPLACING FIRST "+               "
              BY NAME.
           IF IO-FG = 9
              MOVE "NOT FOUND" TO NAME-WORKER.
           MOVE NAME-WORKER TO VDUBUF.
           PERFORM SCREENIO.

      *********************************
       IO-ROUTINES SECTION.
      *********************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-BW1-FILE.
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      *********************************
       END-ROUTINE SECTION.
      *********************************
       END-RUN.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           MOVE EXIT-PATH TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
      *    DESTROY FDFLEX-SCREEN.
           DESTROY FDFLEX-WINDOW-HANDLE.
           EXIT PROGRAM.
