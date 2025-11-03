       IDENTIFICATION DIVISION.
       PROGRAM-ID.      LCLIST.
       DATE-WRITTEN. 02-10-84
      **************************************************************
      *                      (LP)
      *    DESCRIPTION:  LOAN CLASS FILE LIST
      *
      * REV:
      *  JTG 113088 ADDED HIGH CREDIT FLAG
      *  JTG 070191 REMOVE REF TO LC-ADDON-CLASS LC-ORIG-CLASS
      *  BAH 091592 REMOVED LC-COUPON-FG
      *  JFF 101995 ADDED CLASS SCAN
      *  GLF 072996 ADDED STOP DLR #, A&D CLASS, CON/DAYS PAST DUE
      *      AND REC/DAYS PAST DUE
      *  JTG 081497 ADDED SET OF LN-GIFTNET
      *   CS 031398 ADDED LC-APP-REQ-FOR-LOAN
      *  KEC 090298 ADDED LC-REVOLVING
      *  BAH 20190625 REMOVED LC-REVOLVING, PT#0153
      *
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

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LS/LCLIST.CBL S34 06/10/21-12:48:14 >".
       COPY "LIBLP/LP01LC.CPY".
       COPY "LIBLP/LP01LC_SQL.CPY".
       COPY "LIBLP/LPWSLC.CPY".
       01  PR-IFN               PIC X(11)    VALUE "0APRINT".

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
       01  CLASS-FG             PIC X(3)     VALUE "   ".
           88  CLASS-BEG                     VALUE "BEG".
           88  CLASS-END                     VALUE "END".

       01  ERRCD           PIC X.
       01  BEG-LC          PIC 99.
       01  END-LC          PIC 99.

       01  OK                   PIC X        VALUE " ".

      *=================================================================
      *
      *    H E A D E R - B U F F E R S
      *
      *=================================================================
       01  HEAD1.
           03  H-DATE                    PIC X(08)     VALUE SPACES.
           03  FILLER                    PIC X(02)     VALUE SPACES.
           03  H-HR                      PIC 9(02)     VALUE ZEROES.
           03  FILLER                    PIC X(01)     VALUE ":".
           03  H-MIN                     PIC 9(02)     VALUE ZEROES.
           03  FILLER                    PIC X(32)     VALUE SPACES.
           03  H-NAME                    PIC X(40)     VALUE SPACES.
           03  FILLER                    PIC X(15)     VALUE SPACES.
           03  FILLER                    PIC X(08)     VALUE "PAGE ".
           03  H-PAGE-NO                 PIC ZZZ9.

      *-----------------------------------------------------------------
       01  HEAD2.
           03  FILLER                    PIC X(10)     VALUE "USER ID:".
           03  H-USERID                  PIC X(10).
           03  FILLER                    PIC X(30)     VALUE SPACES.
           03  FILLER                    PIC X(32)     VALUE
           "- - LOAN CLASS FILE LISTING - -".

      *-----------------------------------------------------------------
       01  HEAD3.
           03  FILLER PIC X(30) VALUE "                    SMAL  SALE".
           03  FILLER PIC X(30) VALUE "  REVO  SERV        LEDG  DELQ".
           03  FILLER PIC X(30) VALUE "  DELQ  DISP  HIGH  SKIP  STOP".
           03  FILLER PIC X(30) VALUE "   A&D   CON/DAYS  REC/DAYS  A".
           03  FILLER PIC X(12) VALUE "PP.  GIFT   ".

      *-----------------------------------------------------------------
       01  HEAD4.
           03  FILLER PIC X(30) VALUE "----LOAN  CLASS---- LOAN  FIN ".
           03  FILLER PIC X(30) VALUE "  CRED  ACCT  I/P   CARD  PRF1".
           03  FILLER PIC X(30) VALUE "  PRF2  EARN  CRED  TRW   DLR#".
           03  FILLER PIC X(30) VALUE "  CLASS  PAST DUE  PAST DUE  R".
           03  FILLER PIC X(12) VALUE "EQ.  NET    ".

      *=================================================================
      *
      *    D E T A I L - L I N E
      *
      *=================================================================
       01  DETAIL-LINE                      PIC X(132)     VALUE SPACES.

      *-----------------------------------------------------------------
       01  D-LINE1                REDEFINES DETAIL-LINE.
           03  D-CODE                       PIC Z9.
           03  FILLER                       PIC X(01).
           03  D-DESC                       PIC X(15).
           03  FILLER                       PIC X(03).
           03  D-SMALLOAN                   PIC X(01).
           03  FILLER                       PIC X(05).
           03  D-SALEFIN                    PIC X(01).
           03  FILLER                       PIC X(11).
           03  D-SERVACCT                   PIC X(01).
           03  FILLER                       PIC X(05).
           03  D-LOANTYPE                   PIC X(01).
           03  FILLER                       PIC X(05).
           03  D-LEDGER                     PIC X(01).
           03  FILLER                       PIC X(05).
           03  D-PROF1                      PIC X(01).
           03  FILLER                       PIC X(05).
           03  D-PROF2                      PIC X(01).
           03  FILLER                       PIC X(05).
           03  D-DISPE                      PIC X(01).
           03  FILLER                       PIC X(05).
           03  D-HIGHCR                     PIC X(01).
           03  FILLER                       PIC X(05).
           03  D-SKIPTRW                    PIC X(01).
           03  FILLER                       PIC X(05).
           03  D-STOPDLR                    PIC X(01).
           03  FILLER                       PIC X(06).
           03  D-ADCLS                      PIC X(01).
           03  FILLER                       PIC X(07).
           03  D-CON                        PIC X(01).
           03  FILLER                       PIC X(09).
           03  D-REC                        PIC X(01).
           03  FILLER                       PIC X(07).
           03  D-APP-REQ                    PIC X(01).
           03  FILLER                       PIC X(05).
           03  D-GIFTNET                    PIC X(01).
           03  FILLER                       PIC X(05).

      *-----------------------------------------------------------------
       01  RANGE-LINE             REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(22).
           03  RANGE-SELECTION              PIC X(16).
           03  FILLER                       PIC X(94).

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBWI/LCSCANW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/GETPRW.CPY".

       01  LCLIST-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLS/LCLIST_DEF.CPY".
       COPY "LIBLS/LCLIST_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ****************************************
      *     P R O G R A M   C O N T R O L    *
      ****************************************
       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBLS/LCLIST_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-LC1-FILE.
           PERFORM CLOSE-PR-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      *************************************
       MAIN-MODULE SECTION.
      *************************************
           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "LCLIST" TO VDU-FORM-NAME.

           DISPLAY STANDARD GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS LCLIST-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           PERFORM INITIALIZATION.
           IF ERRCD NOT = " "
              GO TO END-PROGRAM.

       NEXT-LC.
           PERFORM READ-LC1-FILE-NEXT.
           IF IO-FG = 9
              GO TO END-ROUTINE.

           IF LC-CODE > END-LC
              GO TO END-ROUTINE.

           PERFORM DETAIL-LINE-PROCESS.

           GO TO NEXT-LC.
       END-ROUTINE.
           IF FIRST-TIME = "Y"
              MOVE "N" TO RECORDS-FOUND
              GO TO END-PROGRAM.

           PERFORM PRINT-RANGE-LINES.
           GO TO END-PROGRAM.

      ***********************************
       INITIALIZATION SECTION.
      ***********************************
           MOVE " " TO ERRCD.
       GETPR.
           MOVE LCLIST-QUEUE-POS TO Q-POS.
           MOVE LCLIST-COPIES-POS TO C-POS.
           MOVE LCLIST-PRU-POS TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
           PERFORM PRU-IN.
           IF STAT NOT = "00"
              GO TO END-INIT.
        LC-01.
           MOVE "ENNN020CODE1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO GETPR.
           IF VDUSTATUS = "16" GO TO ALL-LC.
           IF VDUSTATUS = "1<"
              MOVE "BEG" TO CLASS-FG
              PERFORM CLASS-SCAN
              IF IO-FG NOT = 0
                 GO TO LC-01.
           IF VDUSTATUS = "1>" GO TO END-INIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO LC-01.
           MOVE VDUNUM0 TO BEG-LC.

       LC-02.
           MOVE "ENNN020CODE2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO GETPR.
           IF VDUSTATUS = "1U" GO TO LC-01.
           IF VDUSTATUS = "1<"
              MOVE "END" TO CLASS-FG
              PERFORM CLASS-SCAN
              IF IO-FG NOT = 0
                 GO TO LC-02.
           IF VDUSTATUS = "1>" GO TO END-INIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO LC-02.
           MOVE VDUNUM0 TO END-LC.
           IF BEG-LC > END-LC
              MOVE "INVALID LOAN CLASS RANGE !" TO MESS
              PERFORM SEND-MESS
              GO TO LC-01.
           GO TO OK-YN.
       ALL-LC.
           MOVE "SNNN020CODE1." TO VDU.
           MOVE 0 TO BEG-LC VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020CODE2." TO VDU.
           MOVE 99 TO END-LC VDUNUM0.
           PERFORM SCREENIO.
       OK-YN.
           MOVE "E  A010ACCEPT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO GETPR.
           IF VDUSTATUS = "1U" GO TO LC-02.
           IF VDUSTATUS = "1>" GO TO END-INIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO OK-YN.
           MOVE VDUBUF TO OK.
           IF OK = "N" GO TO LC-01.
           IF OK NOT = "Y" GO TO OK-YN.

           MOVE "LISTING IN PROGRESS...................." TO MESS.
           MOVE MESS TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           PERFORM OPEN-LC1-FILE.
           MOVE BEG-LC TO LC-CODE.
           PERFORM START-LC1-FILE.

           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           PERFORM OPEN-PR-FILE.

           GO TO EXIT-INITIALIZE.
       END-INIT.
           MOVE 1 TO ERRCD.
       EXIT-INITIALIZE.
           EXIT.

      ******************************************************************
       DETAIL-LINE-PROCESS SECTION.
      ******************************************************************

           MOVE LC-CODE                   TO D-CODE.
           MOVE LC-DESC                   TO D-DESC.
           MOVE LC-SMALLOAN               TO D-SMALLOAN.
           MOVE LC-SALEFIN                TO D-SALEFIN.
           MOVE LC-SERVACCT               TO D-SERVACCT.
           MOVE LC-LOANTYPE               TO D-LOANTYPE.
           MOVE LC-LCARDFG                TO D-LEDGER.
           MOVE LC-PROF1                  TO D-PROF1.
           MOVE LC-PROF2                  TO D-PROF2.
           MOVE LC-DISPLAY-ERN            TO D-DISPE.
           MOVE LC-CALC-HI-CREDIT         TO D-HIGHCR.
           MOVE LC-SKIP-TRW               TO D-SKIPTRW.
           MOVE LC-STOP-ON-DLNO           TO D-STOPDLR.
           MOVE LC-AFP-CLASS              TO D-ADCLS.
           MOVE LC-CON-PDUE-DAYS          TO D-CON.
           MOVE LC-REC-PDUE-DAYS          TO D-REC.
           MOVE LC-APP-REQ-FOR-LOAN       TO D-APP-REQ.
           MOVE LC-GIFTNET                TO D-GIFTNET.

           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

       EXIT-DETAIL-LINE-PROCESS.
           EXIT.

      ****************************
       WRITE-DETAIL-LINE SECTION.
      ****************************
           ADD LN-FEED LN-COUNT GIVING LN-WK.
           IF ( LN-WK > EXT-CONAME-RPT-LINES )
              PERFORM HEAD-PAGE.
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

           MOVE HEAD3 TO PR-REC.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE HEAD4 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

       END-HEAD-PAGE.
           MOVE 5 TO LN-COUNT.
           MOVE 2 TO LN-FEED.

      *******************************
       PRINT-RANGE-LINES SECTION.
      *******************************
           MOVE 2 TO LN-FEED.
           MOVE "BEGINNING LOAN CLASS" TO RANGE-LINE.
           MOVE BEG-LC TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "ENDING LOAN CLASS" TO RANGE-LINE.
           MOVE END-LC TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

      ****************************************************
       CLASS-SCAN SECTION.
      ****************************************************
           MOVE 0      TO LCSCAN-KEY.
           MOVE SPACES TO LCSCAN-VERIFY.
           MOVE "WI/LCSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH LCSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF LCSCAN-STATUS NOT = "00" OR LCSCAN-KEY = 0
               MOVE 9 TO IO-FG
               GO TO EXIT-CLASS-SCAN.
           IF ( CLASS-FG = "BEG" )
              MOVE "SN N020CODE1." TO VDU
           ELSE
              MOVE "SN N020CODE2." TO VDU.

           MOVE LCSCAN-KEY TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE LCSCAN-KEY TO VDUNUM0.
           MOVE 0 TO IO-FG.

       EXIT-CLASS-SCAN.
           EXIT.

       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".

      *****************************************************
       FORM-RESET SECTION.
      *****************************************************
           MOVE EXT-CONAME-CONAME TO LCLIST-CONAME.
           MOVE EXT-CONAME-SYSDATE TO LCLIST-SYSDATE.
           DISPLAY LCLIST-SCREEN UPON LCLIST-WINDOW-HANDLE.
           MOVE LCLIST-FORM-FIELDS TO WR-BUF.
           MOVE LCLIST-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *****************************************************
       SETUP-LINK-INFO SECTION.
      *****************************************************
           MOVE LCLIST-HELP--FILE TO HELP-LINK-FILE.
           MOVE LCLIST-HELP--DIR  TO HELP-LINK-DIR.

      *****************************************************
       VDU-CONVERT-FIELD SECTION.
      *****************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLS/LCLIST_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ***********************************************************
       MISC SECTION.
      ***********************************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/ACCESS.CPY".

      ***********************************************************
       IO-ROUTINE SECTION.
      ***********************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBLP/LPLC1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      ***********************************************************
       END-PROGRAM SECTION.
      ***********************************************************
       END-RUN.
           PERFORM CLOSE-FILES.
           PERFORM PRU-OUT.
           IF NO-RECORDS-FOUND
              MOVE "NO RECORDS FOUND FOR SELECTED RANGE!" TO MESS
              PERFORM SEND-MESS.
       EXIT-PROG.
           MOVE "LP/LPL2MU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY LCLIST-SCREEN.
           DESTROY LCLIST-WINDOW-HANDLE.
           EXIT PROGRAM.
