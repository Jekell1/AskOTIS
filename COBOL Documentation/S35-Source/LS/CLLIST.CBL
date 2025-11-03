       IDENTIFICATION DIVISION.
       PROGRAM-ID.      CLLIST.
       DATE-WRITTEN. 02-10-84
      **************************************************************
      *
      *    DESCRIPTION:  CANNED LOAN LISTING
      *
      * REV:
      *************************************************************
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
           "@(#)LS/CLLIST.CBL S35 04/11/22-11:15:30 >".
       COPY "LIBLP/LP01CL.CPY".
       COPY "LIBLP/LP01CL_SQL.CPY".
       COPY "LIBLP/LPWSCL.CPY".
       01  PR-IFN               PIC XX      VALUE "PR".

       COPY "LIBLP/LP01PD.CPY".

      **********************
      *   LINE WORKERS     *
      **********************
       01  LN-WORKERS.
           03  LN-FEED          PIC 99       VALUE 0.
           03  LN-COUNT         PIC 99       VALUE 41.
           03  LN-WK            PIC 99       VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

      **********************
      *   MISC WORKERS     *
      **********************
       01  I-SUB                PIC 9(6)     COMP-3 VALUE 0.
       01  FIRST-TIME           PIC X        VALUE "Y".
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".

       01  RECORDS-FOUND        PIC X     VALUE "N".
           88  NO-RECORDS-FOUND           VALUE "N".

      ***********************************
      *     S C R E E N   F I E L D S   *
      ***********************************
       01  SCREEN-BUF          VALUE " ".
           03  ERRCD           PIC X.
           03  GROUP           PIC X(4).
           03  BEG-NO          PIC X(2).
           03  END-NO          PIC X(2).


      ******************************************************************
      *
      *     P R I N T   B U F F E R S
      *
      ******************************************************************
       01  HEAD1.
           03  H-DATE           PIC X(8)     VALUE " ".
           03  FILLER           PIC XX       VALUE " ".
           03  H-TIME           PIC X(8)     VALUE " ".
           03  FILLER           PIC X(28)    VALUE " ".
           03  H-NAME           PIC X(40)    VALUE " ".
           03  FILLER           PIC X(37)    VALUE " ".
           03  FILLER           PIC X(5)     VALUE "PAGE ".
           03  H-PAGE-NO        PIC ZZZ9.

      *-----------------------------------------------------------------
       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(31)    VALUE " ".
           03  FILLER           PIC X(38) VALUE
               "     CANNED LOAN LISTING ".

      *-----------------------------------------------------------------
       01  HEAD5.
           03  FILLER           PIC X(39)  VALUE
           "DESCRIPTION                           ".
           03  FILLER           PIC X(40)  VALUE
           "                                        ".
           03  FILLER           PIC X(53)  VALUE
           "                                                     ".

      *-----------------------------------------------------------------
       01  HEAD6.
           03  FILLER           PIC X(38)  VALUE
           "CAN NO.  APR   SC CC  TRM    LN-AMT   ".
           03  FILLER           PIC X(40)  VALUE
           "INTCHG     FINCHG 1ST-PAYMT -INSURANCES-".
           03  FILLER           PIC X(53)  VALUE
           "  ---FEES---   AMT-FIN   PROCEEDS                    ".

      *-----------------------------------------------------------------
       01  HEAD7.
           03  FILLER           PIC X(38)  VALUE
           "       EFFRATE    PC         SERCHG  1".
           03  FILLER           PIC X(40)  VALUE
           "STPYEXT    MFEES  REG-PAYMT             ".
           03  FILLER           PIC X(53)  VALUE
           "              TOT-REPAY                              ".
      *     12345678901234567890123456789012345678901234567890123

      ******************************************************************
      *
      *    D E T A I L - L I N E
      *
      ******************************************************************
       01  DETAIL-LINE          PIC X(132)  VALUE SPACES.

      *-----------------------------------------------------------------
       01  DETAIL1 REDEFINES DETAIL-LINE.
           03  D-DESC           PIC X(33).

      *-----------------------------------------------------------------
       01  DETAIL2 REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(2).
           03  D-CAN            PIC 99.
           03  FILLER           PIC X(3).
           03  D-APR            PIC ZZ9.999    BLANK ZERO.
           03  FILLER           PIC X.
           03  D-SRCD           PIC XX.
           03  FILLER           PIC X.
           03  D-COLCD          PIC XXX.
           03  FILLER           PIC X.
           03  D-TERM           PIC ZZ9.
           03  FILLER           PIC X.
           03  D-LNAMT          PIC Z(5)9.99-  BLANK ZERO.
           03  D-INTCHG         PIC Z(5)9.99-  BLANK ZERO.
           03  D-FINCHG         PIC Z(5)9.99-  BLANK ZERO.
           03  D-PYMTS          PIC Z(5)9.99-  BLANK ZERO.
           03  D-INSPREM        PIC Z(4)9.99-  BLANK ZERO.
           03  D-INSFLAG        PIC X.
           03  D-INSTYPE        PIC X.
           03  D-INSOURS        PIC X.
           03  FILLER           PIC X(2).
           03  D-FEEGLNO        PIC 99.
           03  FILLER           PIC X.
           03  D-FEEAMT         PIC Z(3)9.99-  BLANK ZERO.
           03  FILLER           PIC X.
           03  D-AMTFIN         PIC Z(5)9.99-  BLANK ZERO.
           03  D-PROCEEDS       PIC Z(5)9.99-  BLANK ZERO.

      *-----------------------------------------------------------------
       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(35).
           03  RANGE-SELECTION.
               04  R-CHAR        PIC X(6).

       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
       01  CLLIST-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLS/CLLIST_DEF.CPY".
       COPY "LIBLS/CLLIST_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      /***************************************
      *     P R O G R A M   C O N T R O L    *
      ****************************************
       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBLS/CLLIST_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE.
               
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-CL1-FILE.
           PERFORM CLOSE-PR-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      *************************************
       MAIN-MODULE SECTION.
      *************************************
      D    STOP FORM-PATHNAME.

           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "CLLIST" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS CLLIST-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           PERFORM INITIALIZATION.
           IF ERRCD NOT = " "
              GO TO END-PROGRAM.

           MOVE GROUP   TO CL-GROUP-ID
                           QCL1-WBEG-GROUP
                           QCL1-WEND-GROUP.
           MOVE BEG-NO  TO CL-CAN-NO
                           QCL1-WBEG-CAN-NO.
           MOVE END-NO  TO QCL1-WEND-CAN-NO.

           PERFORM START-CL1-FILE.

       NEXT-CL.
           PERFORM READ-CL1-FILE-NEXT.
           IF IO-FG = 9
              GO TO END-ROUTINE.

           MOVE CL-PD-REC TO PD-REC.

           IF CL-GROUP-ID NOT = GROUP
              GO TO NEXT-CL.

           IF CL-CAN-NO < BEG-NO OR CL-CAN-NO > END-NO
              GO TO NEXT-CL.

           PERFORM DETAIL-LINE-PROCESS.

           GO TO NEXT-CL.
       END-ROUTINE.
           IF FIRST-TIME = "Y"
              MOVE " NO RECORDS FOUND FOR SELECTED RANGE !" TO MESS
              PERFORM SEND-MESS
              MOVE SPACES TO PR-REC
              MOVE 99 TO LN-FEED
              PERFORM WRITE-PR-REC.

           PERFORM PRINT-RANGE-LINES.
           GO TO END-PROGRAM.

      ***********************************
       INITIALIZATION SECTION.
      ***********************************

           PERFORM ENTER-PARAMETERS.

           IF ERRCD = "X"
              GO TO ENDIT.

           MOVE 99 TO LN-COUNT.

           PERFORM OPEN-CL1-FILE.

           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           MOVE "LISTING IN PROGRESS...................." TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           PERFORM OPEN-PR-FILE.
           GO TO EXIT-INITIALIZE.
       ENDIT.
           MOVE "X" TO ERRCD.
       EXIT-INITIALIZE.
           EXIT.

      ******************************************************************
       ENTER-PARAMETERS SECTION.
      ******************************************************************

       ENTER-PRINT-RANGE.
           MOVE CLLIST-QUEUE-POS TO Q-POS.
           MOVE CLLIST-COPIES-POS TO C-POS.
           MOVE CLLIST-PRU-POS TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

           PERFORM PRU-IN.

           IF ( STAT NOT = "00" )
              GO TO ENDIT.

       
       GROUP.
           MOVE "E  A040GROUP." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO ENTER-PRINT-RANGE.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" GO TO GROUP.
           IF GROUP = ZEROES GO TO GROUP.
           MOVE VDUBUF TO GROUP.
       NO-01.
           MOVE "E  A020BEG-NO." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO ENTER-PRINT-RANGE.
           IF VDUSTATUS = "16" GO TO ALL-NO.
           IF VDUSTATUS = "1U" GO TO GROUP.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" GO TO NO-01.
           MOVE VDUBUF TO BEG-NO.
           IF BEG-NO = SPACES GO TO NO-01.
       NO-02.
           MOVE "E  A020END-NO." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO ENTER-PRINT-RANGE.
           IF VDUSTATUS = "1U" GO TO NO-01.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" GO TO NO-02.
           MOVE VDUBUF TO END-NO.
           IF END-NO = 0 GO TO NO-02.
           GO TO ACCEPT-YN.
       ALL-NO.
           MOVE "S  A020BEG-NO." TO VDU.
           MOVE SPACES TO BEG-NO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A020END-NO." TO VDU.
           MOVE ALL "Z" TO END-NO.
           CALL "C$TOLOWER" USING END-NO, VALUE 2.
           MOVE END-NO TO VDUBUF.
           PERFORM SCREENIO.
           GO TO ACCEPT-YN.
       ACCEPT-YN.
           MOVE SPACES TO ERRCD.
           MOVE "E  A010PRINT-ACCEPT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO ENTER-PRINT-RANGE.
           IF VDUSTATUS = "1U" GO TO NO-02.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" GO TO ACCEPT-YN.
           IF VDUBUF = "N" GO TO GROUP.
           IF VDUBUF NOT = "Y" GO TO ACCEPT-YN.
           IF VDUBUF = "Y" GO TO EXIT-PARM.

       
       ENDIT.

           MOVE "X" TO ERRCD.

       
       EXIT-PARM.
           EXIT.

      *********************************************
       DETAIL-LINE-PROCESS SECTION.
      *********************************************

           IF FIRST-TIME = "Y"
              MOVE "N" TO FIRST-TIME
              MOVE EXT-CONAME-RPT-LINES TO LN-COUNT
              MOVE 1 TO LN-FEED
              MOVE "Y" TO RECORDS-FOUND.

      * LINE1

           MOVE CL-DESC TO D-DESC.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * LINE2

           MOVE CL-CAN-NO TO D-CAN.
           MOVE PD-APRATE TO D-APR.
           MOVE PD-SRCD TO D-SRCD.
           MOVE PD-COLLCD TO D-COLCD.
           MOVE PD-ORGTERM TO D-TERM.
           MOVE PD-LNAMT TO D-LNAMT.
           MOVE PD-INTCHG TO D-INTCHG.
           MOVE PD-FINCHG TO D-FINCHG.
           MOVE PD-1STPYAMT TO D-PYMTS.

           MOVE 1 TO I-SUB.
           PERFORM SELECT-INSURANCE.
           IF I-SUB < 7
              MOVE PD-INSPREM(I-SUB) TO D-INSPREM
              MOVE PD-INSOURS(I-SUB) TO D-INSOURS.

           MOVE PD-FEECODE(1) TO D-FEEGLNO.
           MOVE PD-FEEAMT(1) TO D-FEEAMT.
           MOVE PD-FINAMT TO D-AMTFIN.
           MOVE PD-PROCEEDS TO D-PROCEEDS.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * LINE3

           MOVE PD-EFFRATE TO D-APR.
           MOVE PD-PURCD TO D-COLCD.
           MOVE PD-SERCHG TO D-LNAMT.
           MOVE PD-EXTCHG TO D-INTCHG.
           MOVE PD-MAINTFEE TO D-FINCHG.
           MOVE PD-REGPYAMT TO D-PYMTS.

           ADD 1 TO I-SUB.
           IF I-SUB < 7
              PERFORM SELECT-INSURANCE
              IF I-SUB < 7
                 MOVE PD-INSPREM(I-SUB) TO D-INSPREM
                 MOVE PD-INSOURS(I-SUB) TO D-INSOURS.

           MOVE PD-FEECODE(2) TO D-FEEGLNO.
           MOVE PD-FEEAMT(2) TO D-FEEAMT.

           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * LINE4

           ADD 1 TO I-SUB.
           IF I-SUB < 7
              PERFORM SELECT-INSURANCE
              IF I-SUB < 7
                 MOVE PD-INSPREM(I-SUB) TO D-INSPREM
                 MOVE PD-INSOURS(I-SUB) TO D-INSOURS.

           MOVE PD-FEECODE(3) TO D-FEEGLNO.
           MOVE PD-FEEAMT(3) TO D-FEEAMT.

           IF DETAIL-LINE NOT = SPACES
              MOVE 1 TO LN-FEED
              PERFORM WRITE-DETAIL-LINE.
       
      * LINE5

           ADD 1 TO I-SUB.
           IF I-SUB < 7
              PERFORM SELECT-INSURANCE
              IF I-SUB < 7
                 MOVE PD-INSPREM(I-SUB) TO D-INSPREM
                 MOVE PD-INSOURS(I-SUB) TO D-INSOURS.

           MOVE PD-FEECODE(4) TO D-FEEGLNO.
           MOVE PD-FEEAMT(4) TO D-FEEAMT.

           IF DETAIL-LINE NOT = SPACES
              MOVE 1 TO LN-FEED
              PERFORM WRITE-DETAIL-LINE.

       
      * LINE6
       
           ADD 1 TO I-SUB.
           IF I-SUB < 7
              PERFORM SELECT-INSURANCE
              IF I-SUB < 7
                 MOVE PD-INSPREM(I-SUB) TO D-INSPREM
                 MOVE PD-INSOURS(I-SUB) TO D-INSOURS.

           MOVE PD-FEECODE(5) TO D-FEEGLNO.
           MOVE PD-FEEAMT(5) TO D-FEEAMT.

           IF DETAIL-LINE NOT = SPACES
              MOVE 1 TO LN-FEED
              PERFORM WRITE-DETAIL-LINE.

      * LINE7
       
           ADD 1 TO I-SUB.
           IF I-SUB < 7
              PERFORM SELECT-INSURANCE
              IF I-SUB < 7
                 MOVE PD-INSPREM(I-SUB) TO D-INSPREM
                 MOVE PD-INSOURS(I-SUB) TO D-INSOURS.

           MOVE PD-FEECODE(6) TO D-FEEGLNO.
           MOVE PD-FEEAMT(6) TO D-FEEAMT.

           IF DETAIL-LINE NOT = SPACES
              MOVE 1 TO LN-FEED
              PERFORM WRITE-DETAIL-LINE.
       
      * LINE8

           MOVE PD-FEECODE(7) TO D-FEEGLNO.
           MOVE PD-FEEAMT(7) TO D-FEEAMT.

           IF DETAIL-LINE NOT = SPACES
              PERFORM WRITE-DETAIL-LINE.

           MOVE PD-FEECODE(8) TO D-FEEGLNO.
           MOVE PD-FEEAMT(8) TO D-FEEAMT.

           IF DETAIL-LINE NOT = SPACES
              PERFORM WRITE-DETAIL-LINE.

           MOVE PD-FEECODE(9) TO D-FEEGLNO.
           MOVE PD-FEEAMT(9) TO D-FEEAMT.

           IF DETAIL-LINE NOT = SPACES
              PERFORM WRITE-DETAIL-LINE.

           MOVE PD-FEECODE(10) TO D-FEEGLNO.
           MOVE PD-FEEAMT(10) TO D-FEEAMT.

           IF DETAIL-LINE NOT = SPACES
              MOVE 1 TO LN-FEED
              PERFORM WRITE-DETAIL-LINE.

           MOVE SPACES TO DETAIL-LINE.

       END-DETAIL-LN-PROCESS.
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

      ******************************************************************
       HEAD-PAGE SECTION.
      ******************************************************************
           ADD 1 TO PAGE-NUMBER.
           MOVE PAGE-NUMBER TO H-PAGE-NO.
           PERFORM GET-TIME.
           MOVE TIME-EDIT TO H-TIME.
           MOVE HEAD1 TO PR-REC.
           MOVE 99 TO LN-FEED.
           PERFORM WRITE-PR-REC.
       
           MOVE HEAD2 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
       
           MOVE HEAD5 TO PR-REC.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.
       
           MOVE HEAD6 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
       
           MOVE HEAD7 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

       END-HEAD-PAGE.
           MOVE 2 TO LN-FEED.
           MOVE 6 TO LN-COUNT.

      *******************************
       PRINT-RANGE-LINES SECTION.
      *******************************

           MOVE "GROUP" TO RANGE-LINE.
           MOVE GROUP TO R-CHAR.
           MOVE 4 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEG CAN NO" TO RANGE-LINE.
           MOVE BEG-NO TO R-CHAR.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "END CAN NO" TO RANGE-LINE.
           MOVE END-NO TO R-CHAR.
           PERFORM WRITE-DETAIL-LINE.

           GO TO END-RANGE.

       END-RANGE.
           EXIT.

      *****************************
       SELECT-INSURANCE SECTION.
      *****************************
       SELECT-IT.

           IF I-SUB > 6
              GO TO EXIT-SELECT-INSURANCE.

           IF PD-INSPREM(I-SUB) = 0
              ADD 1 TO I-SUB
              GO TO SELECT-IT.

           IF I-SUB = 1
              PERFORM GET-CREDIT-LIFE-TAG
           ELSE
           IF I-SUB = 2
              MOVE "A" TO D-INSTYPE
           ELSE
           IF I-SUB = 3
              MOVE "P" TO D-INSTYPE
           ELSE
           IF I-SUB = 4
              MOVE "1" TO D-INSTYPE
           ELSE
           IF I-SUB = 5
              MOVE "2" TO D-INSTYPE
           ELSE
           IF I-SUB = 6
              MOVE "3" TO D-INSTYPE.

       EXIT-SELECT-INSURANCE.
           EXIT.


      ****************************************************
       FORM-RESET SECTION.
      ****************************************************
           MOVE EXT-CONAME-CONAME TO CLLIST-CONAME.
           MOVE EXT-CONAME-SYSDATE TO CLLIST-SYSDATE.
           DISPLAY CLLIST-SCREEN UPON CLLIST-WINDOW-HANDLE.
           MOVE CLLIST-FORM-FIELDS TO WR-BUF.
           MOVE CLLIST-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ****************************************************
       SETUP-LINK-INFO SECTION.
      ****************************************************
           MOVE CLLIST-HELP--FILE TO HELP-LINK-FILE.
           MOVE CLLIST-HELP--DIR  TO HELP-LINK-DIR.

      ****************************************************
       VDU-CONVERT-FIELD SECTION.
      ****************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLS/CLLIST_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/SCREEN.CPY".

      *********************************
       MISC SECTION.
      *********************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/ACCESS.CPY".
      *----------------------------------------------------------
       CHECK-PAGE.
           ADD 1 TO LN-COUNT.
           IF LN-COUNT > 40
               PERFORM HEAD-PAGE.
           IF LN-COUNT = 2
               MOVE 6 TO LN-FEED
           ELSE
               MOVE 4 TO LN-FEED.
      *-----------------------------------------------------------------
       GET-CREDIT-LIFE-TAG.
           IF PD-INSTYPES-DL
              MOVE "D" TO D-INSTYPE
              IF PD-INSURED = "M" OR "S"
                 MOVE "S" TO D-INSFLAG
              ELSE
                 MOVE "J" TO D-INSFLAG
           ELSE
              MOVE "L" TO D-INSTYPE
              IF PD-INSURED = "M" OR "S"
                 MOVE "S" TO D-INSFLAG
              ELSE
                 MOVE "J" TO D-INSFLAG.

      *****************************************************
       IO-ROUTINE SECTION.
      *****************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPCLGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBLP/LPCL1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      *****************************************************
       END-PROGRAM SECTION.
      *****************************************************
       END-RUN.
           PERFORM CLOSE-FILES.
           PERFORM PRU-OUT.
       EXIT-PROG.
           MOVE "LP/LPLSMU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY CLLIST-SCREEN.
           DESTROY CLLIST-WINDOW-HANDLE.
           EXIT PROGRAM.
