       IDENTIFICATION DIVISION.
       PROGRAM-ID.      IRLIST.
       DATE-WRITTEN. 08-88.
      ****************************************************************
      *                      (LS)
      *    DESCRIPTION:  INSURANCE RATE TABLE FILE LISTING
      *
      * REV:
      *  JTG 051596 CHANGED SP-INS-RATE-FILE VALUES TO 71 THRU 99
      *  BAH 20190723 CHANGED IR-KEY FROM 99 TO 999 PT0147
      ****************************************************************
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

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LS/IRLIST.CBL S34 06/10/21-12:47:53 >".

       COPY "LIBLP/LP01IR.CPY".
       COPY "LIBLP/LP01IR_SQL.CPY".
       COPY "LIBLP/LPWSIR.CPY".

      **********************
      *   LINE WORKERS     *
      **********************
       01  LN-WORKERS.
           03  LN-FEED          PIC 99       VALUE 0.
           03  LN-COUNT         PIC 999      VALUE 99.
           03  LN-WK            PIC 999      VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.
       01  KOUNT                PIC 999.

      **********************
      *   MISC WORKERS     *
      **********************
       01  FIRST-TIME      PIC X        VALUE "Y".
       01  MESS-LIST       PIC X(5)     VALUE "MESS.".

      ***********************************
      *     S C R E E N   F I E L D S   *
      ***********************************
       01  SCREEN-BUF          VALUE " ".
           03  ERRCD           PIC X.
           03  BEG-CODE        PIC 9(3).
           03  END-CODE        PIC 9(3).

       01  ACCEPT-BUF      PIC X    VALUE " ".

      ***********************************
      *     P R I N T   B U F F E R S   *
      ***********************************

       01  HEAD1.
           03  H-DATE           PIC X(8)     VALUE " ".
           03  FILLER           PIC XX       VALUE " ".
           03  H-HR             PIC 99       VALUE 0.
           03  FILLER           PIC X        VALUE ":".
           03  H-MIN            PIC 99       VALUE 0.
           03  FILLER           PIC X(16)    VALUE " ".
           03  H-NAME           PIC X(40)    VALUE " ".
           03  FILLER           PIC X(05)    VALUE " ".
           03  FILLER           PIC X(5)     VALUE "PAGE ".
           03  H-PAGE-NO        PIC ZZZ9.

       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(5)     VALUE " ".
           03  FILLER           PIC X(45)    VALUE
           "- - - INSURANCE RATE TABLE FILE LISTING - - -".

       01  HEAD3.
           03  FILLER      PIC X(10)    VALUE "RATE CODE ".
           03  H-CODE      PIC 9(3).

       01  HEAD4.
           03  FILLER      PIC X(15)    VALUE "TERM---RATE    ".
           03  FILLER      PIC X(15)    VALUE "TERM---RATE    ".
           03  FILLER      PIC X(15)    VALUE "TERM---RATE    ".
           03  FILLER      PIC X(15)    VALUE "TERM---RATE    ".
           03  FILLER      PIC X(15)    VALUE "TERM---RATE    ".
           03  FILLER      PIC X(15)    VALUE "TERM---RATE    ".

       01  DETAIL-LINE          PIC X(132) VALUE SPACES.

       01  D-LINE1 REDEFINES DETAIL-LINE.
           03  D-NUM-1     PIC ZZ9.
           03  FILLER      PIC XX.
           03  D-RATE-1    PIC Z9.999.
           03  FILLER      PIC XXXX.
           03  D-NUM-2     PIC ZZ9.
           03  FILLER      PIC XX.
           03  D-RATE-2    PIC Z9.999.
           03  FILLER      PIC XXXX.
           03  D-NUM-3     PIC ZZ9.
           03  FILLER      PIC XX.
           03  D-RATE-3    PIC Z9.999.
           03  FILLER      PIC XXXX.
           03  D-NUM-4     PIC ZZ9.
           03  FILLER      PIC XX.
           03  D-RATE-4    PIC Z9.999.
           03  FILLER      PIC XXXX.
           03  D-NUM-5     PIC ZZ9.
           03  FILLER      PIC XX.
           03  D-RATE-5    PIC Z9.999.
           03  FILLER      PIC XXXX.
           03  D-NUM-6     PIC ZZ9.
           03  FILLER      PIC XX.
           03  D-RATE-6    PIC Z9.999.
           03  FILLER      PIC XXXX.

       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(20).
           03  RANGE-SELECTION  PIC X(16).

       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       01  IRLIST-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLS/IRLIST_DEF.CPY".
       COPY "LIBLS/IRLIST_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ****************************************
      *     P R O G R A M   C O N T R O L    *
      ****************************************
       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBLS/IRLIST_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
                PR-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-IR-FILE.
           PERFORM CLOSE-PR-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      *************************************
       MAIN-MODULE SECTION.
      *************************************
           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "IRLIST" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS IRLIST-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           IF EXT-ROUTE-BUF NOT = "00"                                          
              MOVE "PROGRAM NOT BATCHABLE" TO MESS                              
              PERFORM SEND-MESS        
              MOVE "99" TO EXT-ROUTE-BUF                                        
              GO TO END-PROGRAM.                                                

           PERFORM INITIALIZATION.
           IF ERRCD NOT = " "
              GO TO END-PROGRAM.
       NEXT-BW.
           PERFORM READ-IR-FILE-NEXT.
           IF IR-KEY > END-CODE
               GO TO END-ROUTINE.
           IF IO-FG = 9
               GO TO END-ROUTINE.

           PERFORM DETAIL-LINE-PROCESS.

           GO TO NEXT-BW.
       END-ROUTINE.
           IF FIRST-TIME = "Y"
              MOVE " NO RECORDS FOUND FOR SELECTED RANGE !" TO MESS
              PERFORM SEND-MESS.

           PERFORM PRINT-RANGE-LINES.

           GO TO END-PROGRAM.

      ***********************************
       INITIALIZATION SECTION.
      ***********************************
           MOVE " " TO ERRCD.
       SETRP.
           MOVE IRLIST-QUEUE-POS TO Q-POS.
           MOVE IRLIST-COPIES-POS TO C-POS.
           MOVE IRLIST-PRU-POS TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
           PERFORM PRU-IN.
           IF STAT NOT = "00"
              GO TO ENDIT.
       CD-01.
           MOVE "ENNN030CODE1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF VDUSTATUS = "16" GO TO ALL-CD.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CD-01.
           MOVE VDUNUM0 TO BEG-CODE.
           IF BEG-CODE < 71
              MOVE "INVALID CODE" TO MESS
              PERFORM SEND-MESS
              GO TO CD-01.
       CD-02.
           MOVE "ENNN030CODE2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF VDUSTATUS = "1U" GO TO CD-01.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CD-02.
           MOVE VDUNUM0 TO END-CODE.
           IF END-CODE < 71
              MOVE "INVALID CODE" TO MESS
              PERFORM SEND-MESS
              GO TO CD-02.

           IF BEG-CODE > END-CODE
               MOVE " INVALID CODE RANGE !" TO MESS
               PERFORM SEND-MESS
               GO TO CD-01.

           SUBTRACT 70 FROM BEG-CODE END-CODE.
           GO TO OK-YN.
       ALL-CD.
           MOVE "SNNN030CODE1." TO VDU.
           MOVE 71 TO BEG-CODE VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN030CODE2." TO VDU.
           MOVE 999 TO END-CODE VDUNUM0.
           PERFORM SCREENIO.
           SUBTRACT 70 FROM BEG-CODE END-CODE.

       OK-YN.
           MOVE " " TO ERRCD.
           MOVE "E  A010ACCEPT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U" GO TO CD-02.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO OK-YN.
           MOVE VDUBUF TO ACCEPT-BUF.

           IF ACCEPT-BUF = "N" GO TO CD-01.
           IF ACCEPT-BUF NOT = "Y" GO TO OK-YN.

           MOVE "LISTING IN PROGRESS...................." TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           PERFORM OPEN-PR-FILE.
           PERFORM OPEN-IR-FILE.

           MOVE BEG-CODE TO IR-KEY.
           PERFORM START-IR-FILE.

           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           GO TO EXIT-INITIALIZE.
       ENDIT.
           MOVE "X" TO ERRCD.
       EXIT-INITIALIZE.
           EXIT.

      ****************************
       DETAIL-LINE-PROCESS SECTION.
      ****************************
           ADD 6 LN-COUNT GIVING LN-WK.
           IF LN-WK > EXT-CONAME-RPT-LINES
               PERFORM HEAD-PAGE.
           MOVE 2 TO LN-FEED.

           PERFORM D-LINE VARYING KOUNT FROM 1 BY 1
               UNTIL KOUNT > 20.

           ADD 1 TO IR-KEY H-CODE.
           MOVE 93 TO LN-COUNT.
       END-DETAIL-LN-PROCESS.
           EXIT.

      ****************************
       D-LINE SECTION.
      ****************************
           SET IRIX TO KOUNT.
           MOVE IRIX TO D-NUM-1.
           MOVE IR-INSRATE(IRIX) TO D-RATE-1.
           SET IRIX UP BY 20.
           MOVE IRIX TO D-NUM-2.
           MOVE IR-INSRATE(IRIX) TO D-RATE-2.
           SET IRIX UP BY 20.
           MOVE IRIX TO D-NUM-3.
           MOVE IR-INSRATE(IRIX) TO D-RATE-3.
           SET IRIX UP BY 20.
           MOVE IRIX TO D-NUM-4.
           MOVE IR-INSRATE(IRIX) TO D-RATE-4.
           SET IRIX UP BY 20.
           MOVE IRIX TO D-NUM-5.
           MOVE IR-INSRATE(IRIX) TO D-RATE-5.
           SET IRIX UP BY 20.
           MOVE IRIX TO D-NUM-6.
           MOVE IR-INSRATE(IRIX) TO D-RATE-6.
           SET IRIX UP BY 20.
           PERFORM WRITE-DETAIL-LINE.

      ****************************
       WRITE-DETAIL-LINE SECTION.
      ****************************
           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      *******************************
       HEAD-PAGE SECTION.
      *******************************
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

           COMPUTE H-CODE = IR-KEY + 70.
           MOVE HEAD3 TO PR-REC.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE HEAD4 TO PR-REC.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.

       END-HEAD-PAGE.
           MOVE 4 TO LN-COUNT.
           MOVE 2 TO LN-FEED.

      *******************************
       PRINT-RANGE-LINES SECTION.
      *******************************
           ADD 70 TO BEG-CODE END-CODE.
           MOVE 2 TO LN-FEED.
           MOVE "BEGINNING CODE" TO RANGE-LINE.
           MOVE BEG-CODE TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "ENDING CODE" TO RANGE-LINE.
           MOVE END-CODE TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".

      ****************************************************
       FORM-RESET SECTION.
      ****************************************************
           MOVE EXT-CONAME-CONAME TO IRLIST-CONAME.
           MOVE EXT-CONAME-SYSDATE TO IRLIST-SYSDATE.
           DISPLAY IRLIST-SCREEN UPON IRLIST-WINDOW-HANDLE.
           MOVE IRLIST-FORM-FIELDS TO WR-BUF.
           MOVE IRLIST-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ****************************************************
       SETUP-LINK-INFO SECTION.
      ****************************************************
           MOVE IRLIST-HELP--FILE TO HELP-LINK-FILE.
           MOVE IRLIST-HELP--DIR  TO HELP-LINK-DIR.

      ****************************************************
       VDU-CONVERT-FIELD SECTION.
      ****************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLS/IRLIST_EVA.CPY".
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
       COPY "LIBLP/LPIRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBLP/LPIRRN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      *******************************
       END-PROGRAM SECTION.
      *******************************
       END-RUN.
           PERFORM CLOSE-FILES.
           PERFORM PRU-OUT.
       EXIT-PROG.
           MOVE "LP/LPL2MU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY IRLIST-SCREEN.
           DESTROY IRLIST-WINDOW-HANDLE.
           EXIT PROGRAM.
