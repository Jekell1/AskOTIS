       IDENTIFICATION DIVISION.
       PROGRAM-ID.      ELLIST.
       DATE-WRITTEN.    061592.
      **************************************************************
      *                      (LP)
      *    DESCRIPTION:  EXTENDED LATE CHARGE FILE LIST
      *
      * REV:
      *  BAH 110922 REPLACED RRFORM WITH SCREEN
      **************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".
      ****************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LS/ELLIST.CBL S34 06/10/21-12:40:27 >".
      ****************************
       77  SCCS-IDENTIFIER PIC X(55) VALUE
           "@(#)LS/ELLIST A15 03/27/95-10:57:13 09/01/11-08:32:56 >".
       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LP01CD_SQL.CPY".
       COPY "LIBLP/LPWSCD.CPY".

      **********************
      *   LINE WORKERS     *
      **********************
       01  LN-WORKERS.
           03  LN-FEED          PIC 99       VALUE 0.
           03  LN-COUNT         PIC 999      VALUE 99.
           03  LN-WK            PIC 999      VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

      **********************
      *   MISC WORKERS     *
      **********************
       01  FIRST-TIME           PIC X        VALUE "Y".
       01  RECORDS-FOUND        PIC X         VALUE " ".
           88  NO-RECORDS-FOUND               VALUE "N".
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".

      ***********************************
      *     S C R E E N   F I E L D S   *
      ***********************************
       01  ERRCD           PIC X.

       01  OK                   PIC X        VALUE " ".

      ***********************************
      *     P R I N T   B U F F E R S   *
      ***********************************
       01  HEAD1.
           03  H-DATE           PIC X(8)     VALUE " ".
           03  FILLER           PIC XX       VALUE " ".
           03  H-HR             PIC 99       VALUE 0.
           03  FILLER           PIC X        VALUE ":".
           03  H-MIN            PIC 99       VALUE 0.
           03  FILLER           PIC X(4)     VALUE " ".
           03  H-NAME           PIC X(40)    VALUE " ".
           03  FILLER           PIC X(12)    VALUE " ".
           03  FILLER           PIC X(5)     VALUE "PAGE ".
           03  H-PAGE-NO        PIC ZZZ9.

       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(3)    VALUE " ".
           03  FILLER           PIC X(33)    VALUE
           "EXTENDED LATE CHARGE FILE LISTING".

       01  HEAD3.
           03  FILLER      PIC X(8)     VALUE "CODE".
           03  FILLER      PIC X(7)     VALUE "FRMLA".
           03  FILLER      PIC X(8)     VALUE "CODE".
           03  FILLER      PIC X(9)     VALUE "RATE".
           03  FILLER      PIC X(10)    VALUE "MIN".
           03  FILLER      PIC X(6)     VALUE "MAX".
           03  FILLER      PIC X(8)     VALUE "GRACE".
           03  FILLER      PIC X(7)     VALUE "YEAR".
           03  FILLER      PIC X(4)     VALUE "DELQ".

       01  DETAIL-LINE          PIC X(80) VALUE SPACES.

       01  D-LINE1 REDEFINES DETAIL-LINE.
           03  FILLER        PIC X.
           03  D-CODE        PIC Z9.
           03  FILLER        PIC X(7).
           03  D-FORMULA     PIC X.
           03  FILLER        PIC X(5).
           03  D-TYPE        PIC X.
           03  FILLER        PIC X(3).
           03  D-RATE        PIC ZZZZ.999.
           03  FILLER        PIC X(2).
           03  D-MIN         PIC ZZ.99.
           03  FILLER        PIC X(3).
           03  D-MAX         PIC ZZZZ.99.
           03  FILLER        PIC X(3).
           03  D-GRACE       PIC Z9.
           03  FILLER        PIC X.
           03  D-GRACECD     PIC X.
           03  FILLER        PIC X(5).
           03  D-YEAR        PIC ZZ9.
           03  FILLER        PIC X(4).
           03  D-DELINQ      PIC ZZ9.

       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/GETPRW.CPY".
       01  ELLIST-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLS/ELLIST_DEF.CPY".
       COPY "LIBLS/ELLIST_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      /***************************************
      *     P R O G R A M   C O N T R O L    *
      ****************************************
       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBLS/ELLIST_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-CD1-FILE.
           PERFORM CLOSE-PR-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      *************************************
       MAIN-MODULE SECTION.
      *************************************
           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "ELLIST" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS ELLIST-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           PERFORM INITIALIZATION.
           IF ERRCD NOT = " "
              GO TO END-PROGRAM.

       NEXT-CD.
           PERFORM READ-CD1-FILE-NEXT.
           IF IO-FG = 9
              GO TO END-ROUTINE.

           IF CD-TYPE NOT = "EL"
              GO TO END-ROUTINE.

           PERFORM DETAIL-LINE-PROCESS.

           GO TO NEXT-CD.
       END-ROUTINE.
           IF FIRST-TIME = "Y"
              MOVE "N" TO RECORDS-FOUND
              GO TO END-PROGRAM.
           GO TO END-PROGRAM.

      ***********************************
       INITIALIZATION SECTION.
      ***********************************
           MOVE " " TO ERRCD.
       GETPR.
           MOVE ELLIST-QUEUE-POS TO Q-POS.
           MOVE ELLIST-COPIES-POS TO C-POS.
           MOVE ELLIST-PRU-POS TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
           PERFORM PRU-IN.
           IF STAT NOT = "00"
              GO TO END-INIT.
       OK-YN.
           MOVE "E  A010ACCEPT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO GETPR.
           IF VDUSTATUS = "1>" GO TO END-INIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO OK-YN.
           MOVE VDUBUF TO OK.

           IF OK = "N" GO TO END-INIT.
           IF OK NOT = "Y" GO TO OK-YN.

           MOVE "LISTING IN PROGRESS...................." TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           PERFORM OPEN-PR-FILE.
           PERFORM OPEN-CD1-FILE.

           MOVE "EL"    TO CD-TYPE
                           QCD1-WBEG-TYPE
                           QCD1-WEND-TYPE.
           MOVE 0       TO CD-EL-CODE.
           MOVE SPACES  TO CD-EL-FILLER.
           MOVE CD-CODE TO QCD1-WBEG-CODE.
           MOVE ALL "Z" TO QCD1-WEND-CODE.

           PERFORM START-CD1-FILE.

           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           GO TO EXIT-INITIALIZE.
       END-INIT.
           MOVE 1 TO ERRCD.
       EXIT-INITIALIZE.
           EXIT.

      ****************************
       DETAIL-LINE-PROCESS SECTION.
      ****************************
           ADD 2 LN-COUNT GIVING LN-WK.
           IF LN-WK > EXT-CONAME-RPT-LINES
               PERFORM HEAD-PAGE.

           MOVE 2 TO LN-FEED.
           MOVE CD-EL-CODE   TO D-CODE.
           MOVE CD-ELFRMLA   TO D-FORMULA.
           MOVE CD-ELTYPE    TO D-TYPE.
           MOVE CD-ELRATE    TO D-RATE.
           MOVE CD-ELMINCHG  TO D-MIN.
           MOVE CD-ELMAXAMT  TO D-MAX.
           MOVE CD-ELGRACE   TO D-GRACE.
           MOVE CD-ELGRACECD TO D-GRACECD.
           MOVE CD-ELYRTYPE  TO D-YEAR.
           MOVE CD-ELDELFAC  TO D-DELINQ.

           PERFORM WRITE-DETAIL-LINE.
       END-DETAIL-LN-PROCESS.
           EXIT.

      ****************************
       WRITE-DETAIL-LINE SECTION.
      ****************************
       WRITE-DET-LINE.
           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      *******************************
       HEAD-PAGE SECTION.
      *******************************
       HEAD-ROUTINE.
           MOVE "N" TO FIRST-TIME.
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
           MOVE 5 TO LN-COUNT.
           MOVE 2 TO LN-FEED.

       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".

      ****************************************************
       FORM-RESET SECTION.
      ****************************************************
           MOVE EXT-CONAME-CONAME TO ELLIST-CONAME.
           MOVE EXT-CONAME-SYSDATE TO ELLIST-SYSDATE.
           DISPLAY ELLIST-SCREEN UPON ELLIST-WINDOW-HANDLE.
           MOVE ELLIST-FORM-FIELDS TO WR-BUF.
           MOVE ELLIST-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ****************************************************
       SETUP-LINK-INFO SECTION.
      ****************************************************
           MOVE ELLIST-HELP--FILE TO HELP-LINK-FILE.
           MOVE ELLIST-HELP--DIR  TO HELP-LINK-DIR.

      ****************************************************
       VDU-CONVERT-FIELD SECTION.
      ****************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLS/ELLIST_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *******************************
       MISC SECTION.
      *******************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/ACCESS.CPY".

      *******************************
       IO-ROUTINE SECTION.
      *******************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBLP/LPCD1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      *******************************
       END-PROGRAM SECTION.
      *******************************
       END-RUN.
           PERFORM CLOSE-FILES.
           PERFORM PRU-OUT.
           IF NO-RECORDS-FOUND
              MOVE "NO RECORDS FOUND FOR SELECTED RANGE!" TO MESS
              PERFORM SEND-MESS.
       EXIT-PROG.
           MOVE "LP/LPLSMU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DESTROY ELLIST-WINDOW-HANDLE.
           EXIT PROGRAM.
