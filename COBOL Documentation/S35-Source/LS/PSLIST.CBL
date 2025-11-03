       IDENTIFICATION DIVISION.
       PROGRAM-ID.      PSLIST.
       DATE-WRITTEN.  02/09/95.
      ******************************************************************
      *
      *    DIR :  LS
      *    DESC:  PAYMENT SCHEDULE
      *
      *    --WHEN TRANS-DATE IS EQUAL TO PAYMENT-DATE, THE
      *      TRANS-DATE IS NOT PRINTED.
      *    --WHEN LN-LOANTYPE IS EQUAL TO 'P', LATE-CHARGE IS
      *      PRINTED ABOVE INTEREST.  OTHERWISE, WHEN 'I',
      *      INTEREST IS PRINTED ABOVE LATECHARGE (APPLIED AND
      *      OWING).
      *
      *=================================================================
      * REV:
      * JFF 102095 ADDED BORROWER LOAN SCAN
      * BAH 160125 SPLIT OUT LTFILE, PD0003
      * BAH 170504 ACTIVATE 8 DIGIT DATES, PD0006
      *
      * KEC 2017.0601 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          REMOVED HOLD-ACCTNO SINCE USED NOT USED.
      *
      * BAH 180312 FIXED ACCOUNT NUMBER PRINTING INCORRECTLY
      * BAH 181211 GLOBAL LNFILE
      * BAH 20220808 HARD CODED START
      ******************************************************************
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
           "@(#)LS/PSLIST.CBL S35 02/21/24-12:06:23 >".

      *********************************************************
      *     F I L E   I F N
      *********************************************************
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01LTP.CPY".
       COPY "LIBLP/LP01LTP_SQL.CPY".
       COPY "LIBLP/LPWSLTP.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".

      *********************************************************
      *   L I N E   W O R K E R S
      *********************************************************
       01  LN-FEED          PIC 99       VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.
       01  PAGE-LINE-NO         PIC 99       VALUE 10.

      *********************************************************
      *   M I S C .   W O R K E R S
      *********************************************************
       01  AT-SIGN              PIC X(3)     VALUE " @ ".
       01  END-PERFORM-PFL      PIC X        VALUE "N".
       01  FIRST-TIME           PIC X        VALUE "Y".
       01  HOLD-IO-FG           PIC 9        VALUE 0.
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".
       01  RECORDS-FOUND        PIC X         VALUE " ".
           88  NO-RECORDS-FOUND               VALUE "N".

       01  SUB-1                PIC 99       VALUE ZERO.

       01  WS-PAY-SCH-TABLE.
           03  WS-PAY-SCHEDULE  PIC X(20)       OCCURS 17 TIMES.
           03  WS-PAYSCH-TERM   PIC ZZ9         OCCURS 17 TIMES.
           03  WS-PAYSCH-PYMNT  PIC ZZZ,ZZ9.99  OCCURS 17 TIMES.

       01  WS-TABLE-COUNT       PIC 99/.
       01  LOAN-FG              PIC X(3)     VALUE "   ".
           88  LOAN-BEG                      VALUE "BEG".
           88  LOAN-END                      VALUE "END".
      *********************************************************
      *     S C R E E N   F I E L D S
      *********************************************************
       01  SCREEN-BUF          VALUE " ".
           03  ERRCD           PIC X.
           03  BEG-LN          PIC 9(6).
           03  END-LN          PIC 9(6).

       01  OK                   PIC X        VALUE " ".

      *********************************************************
      *     P R I N T   B U F F E R S
      *********************************************************

       01  HEAD1.
           03  H-DATE      PIC X(8)      VALUE " ".
           03  FILLER      PIC X         VALUE " ".
           03  H-HOUR      PIC 9(2)      VALUE ZERO.
           03  FILLER      PIC X         VALUE ":".
           03  H-MIN       PIC 9(2)      VALUE ZERO.
           03  FILLER      PIC X(34)     VALUE " ".
           03  H-CONAME    PIC X(40)     VALUE " ".
           03  FILLER      PIC X(35)     VALUE " ".
           03  FILLER      PIC X(6)      VALUE "PAGE: ".
           03  H-PAGE-NO   PIC 999       VALUE ZERO.

      *--------------------------------------------------------

       01  HEAD2.
           03  FILLER      PIC X(9)      VALUE "USER ID:".
           03  H-USERID    PIC X(12)     VALUE SPACES.
           03  FILLER      PIC X(36)     VALUE SPACES.
           03  FILLER      PIC X(24)     VALUE
                                         "PAYMENT SCHEDULE LISTING".
           03  FILLER      PIC X(51)     VALUE SPACES.

      *--------------------------------------------------------

       01  HEAD3.
           03 FILLER       PIC X(11)     VALUE "LOAN NUMBER".
           03 FILLER       PIC X(3)      VALUE SPACES.
           03 FILLER       PIC X(9)      VALUE "SCHEDULE:".
           03 FILLER       PIC X(108)    VALUE SPACES.

      *--------------------------------------------------------

       01  ACTION1.
           03  A1-ACCTNO   PIC 9(8)  BLANK WHEN ZERO.
           03  FILLER      PIC X(4)      VALUE SPACES.
           03  A1-NUMBER-1 PIC X(20)     VALUE SPACES.
           03  FILLER      PIC X(4)      VALUE SPACES.
           03  A1-NUMBER-2 PIC X(20)     VALUE SPACES.
           03  FILLER      PIC X(4)      VALUE SPACES.
           03  A1-NUMBER-3 PIC X(20)     VALUE SPACES.
           03  FILLER      PIC X(4)      VALUE SPACES.
           03  A1-NUMBER-4 PIC X(20)     VALUE SPACES.
           03  FILLER      PIC X(4)      VALUE SPACES.
           03  A1-NUMBER-5 PIC X(20)     VALUE SPACES.
           03  FILLER      PIC X(1)      VALUE SPACES.

      *--------------------------------------------------------

       01  ACTION2.
           03  FILLER      PIC X(12)     VALUE SPACES.
           03  A2-NUMBER-1 PIC X(20)     VALUE SPACES.
           03  FILLER      PIC X(4)      VALUE SPACES.
           03  A2-NUMBER-2 PIC X(20)     VALUE SPACES.
           03  FILLER      PIC X(4)      VALUE SPACES.
           03  A2-NUMBER-3 PIC X(20)     VALUE SPACES.
           03  FILLER      PIC X(4)      VALUE SPACES.
           03  A2-NUMBER-4 PIC X(20)     VALUE SPACES.
           03  FILLER      PIC X(4)      VALUE SPACES.
           03  A2-NUMBER-5 PIC X(20)     VALUE SPACES.
           03  FILLER      PIC X(1)      VALUE SPACES.

      *--------------------------------------------------------

       01  ACTION3.
           03  FILLER      PIC X(12)     VALUE SPACES.
           03  A3-NUMBER-1 PIC X(20)     VALUE SPACES.
           03  FILLER      PIC X(4)      VALUE SPACES.
           03  A3-NUMBER-2 PIC X(20)     VALUE SPACES.
           03  FILLER      PIC X(4)      VALUE SPACES.
           03  A3-NUMBER-3 PIC X(20)     VALUE SPACES.
           03  FILLER      PIC X(4)      VALUE SPACES.
           03  A3-NUMBER-4 PIC X(20)     VALUE SPACES.
           03  FILLER      PIC X(4)      VALUE SPACES.
           03  A3-NUMBER-5 PIC X(20)     VALUE SPACES.
           03  FILLER      PIC X(1)      VALUE SPACES.

      *--------------------------------------------------------

       01  ACTION4.
           03  FILLER      PIC X(12)     VALUE SPACES.
           03  A4-NUMBER-1 PIC X(20)     VALUE SPACES.
           03  FILLER      PIC X(4)      VALUE SPACES.
           03  A4-NUMBER-2 PIC X(20)     VALUE SPACES.
           03  FILLER      PIC X(73)      VALUE SPACES.

      *--------------------------------------------------------

       01  DETAIL-LINE          PIC X(132) VALUE " ".

       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(22).
           03  RANGE-SELECTION  PIC X(16).

       COPY "LIBWI/BWLNSCW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/DATERW.CPY".
       01  PSLIST-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLS/PSLIST_DEF.CPY".
       COPY "LIBLS/PSLIST_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      /***************************************
      *     P R O G R A M   C O N T R O L    *
      ****************************************
       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBLS/PSLIST_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-LTP1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-PR-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      *********************************************************
      *
      *              M A I N   M O D U L E :
      *
      *********************************************************
       MAIN-MODULE SECTION.
           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".

           MOVE FORM-PATHNAME TO  FORM-PATH.
           MOVE "PSLIST" TO VDU-FORM-NAME.
           PERFORM INITIALIZATION.

           IF ERRCD NOT = " "
              GO TO END-PROGRAM.

           PERFORM OPEN-LN1-FILE.
           PERFORM OPEN-LTP1-FILE.
           PERFORM OPEN-PR-FILE.

           MOVE LN-PATH-OWNBR TO LN-OWNBR
                                 QLN1-WBEG-OWNBR
                                 QLN1-WEND-OWNBR.
           MOVE BEG-LN        TO LN-ACCTNO
                                 QLN1-WBEG-ACCTNO.
           MOVE END-LN        TO QLN1-WEND-ACCTNO.

           PERFORM START-LN1-FILE.

           PERFORM PROCESS-FILE-LISTING UNTIL END-PERFORM-PFL = "Y"

           GO TO END-ROUTINE.

      *--------------------------------------------------------
       PROCESS-FILE-LISTING.
           PERFORM ACCESS-FILES.
           IF HOLD-IO-FG = 9 OR LN-ACCTNO > END-LN
              MOVE "Y" TO END-PERFORM-PFL
           ELSE
              IF LN-PAY-SCHLD-FG = "Y"
                 PERFORM DETAIL-LINE-PROCESS.

      *--------------------------------------------------------
       END-ROUTINE.
           IF FIRST-TIME = "Y"
              MOVE "N" TO RECORDS-FOUND
              GO TO END-PROGRAM.

           PERFORM PRINT-RANGE-LINES.
           GO TO END-PROGRAM.

      *********************************************************
       ACCESS-FILES SECTION.
      *********************************************************

           PERFORM READ-LN1-FILE-NEXT.
           MOVE IO-FG TO HOLD-IO-FG.
           IF IO-BAD OR (LN-OWNBR NOT = LN-PATH-OWNBR)
                      OR (LN-ACCTNO > END-LN)
              GO TO END-ACCESS-FILES
           ELSE
              IF LN-PAY-SCHLD-FG = "Y"
                 PERFORM NEXT-LT.

           GO TO END-ACCESS-FILES.

       NEXT-LT.
           MOVE LN-OWNBR   TO LTP-BRNO.
           MOVE LN-ACCTNO  TO LTP-ACCTNO.
           MOVE 1          TO LTP-SEQNO.
           PERFORM READ-LTP1-FILE.

       END-ACCESS-FILES.
           EXIT.

      *********************************************************
       INITIALIZATION SECTION.
      *********************************************************
           MOVE "PSLIST" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS PSLIST-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           MOVE " " TO ERRCD.

       GETPR.
           MOVE PSLIST-QUEUE-POS TO Q-POS.
           MOVE PSLIST-COPIES-POS TO C-POS.
           MOVE PSLIST-PRU-POS TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
           PERFORM PRU-IN.
           IF STAT NOT = "00"
              GO TO END-INIT.

       LN-01.
           MOVE "ENNN060LOANNO1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO END-INIT.
           IF VDUSTATUS = "10" GO TO GETPR.
           IF VDUSTATUS = "16" GO TO ALL-LN.
           IF VDUSTATUS = "1<"
              MOVE "BEG" TO LOAN-FG
              PERFORM BWLN-SCAN
              IF IO-FG NOT = 0
              GO TO LN-01.
           IF VDUSTATUS NOT = "00" GO TO LN-01.
           MOVE VDUNUM0 TO BEG-LN.

      *--------------------------------------------------------
       LN-02.
           MOVE "ENNN060LOANNO2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO GETPR.
           IF VDUSTATUS = "1U" GO TO LN-01.
           IF VDUSTATUS = "1>" GO TO END-INIT.
           IF VDUSTATUS = "1<"
              MOVE "END" TO LOAN-FG
              PERFORM BWLN-SCAN
              IF IO-FG NOT = 0
              GO TO LN-02.
           IF VDUSTATUS NOT = "00" GO TO LN-02.
           MOVE VDUNUM0 TO END-LN.
           IF BEG-LN > END-LN
              MOVE "INVALID LOAN NUMBER RANGE !" TO MESS
              PERFORM SEND-MESS
              GO TO LN-01.
           GO TO OK-YN.

       ALL-LN.
           MOVE "SNNN060LOANNO1." TO VDU.
           MOVE 0       TO  BEG-LN VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN060LOANNO2." TO VDU.
           MOVE 999999  TO  END-LN VDUNUM0.
           PERFORM SCREENIO.

       OK-YN.
           MOVE "E  A010ACCEPT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO GETPR.
           IF VDUSTATUS = "1U" GO TO LN-02.
           IF VDUSTATUS = "1>" GO TO END-INIT.
           IF VDUSTATUS NOT = "00" GO TO OK-YN.
           MOVE VDUBUF TO OK.
           IF OK = "N" GO TO LN-01.
           IF OK NOT = "Y" GO TO OK-YN.

           IF EXT-ROUTE-BUF = "00"
              MOVE "LISTING IN PROGRESS.................." TO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           MOVE EXT-CONAME-CONAME TO H-CONAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           GO TO EXIT-INITIALIZE.

       END-INIT.
           MOVE 1 TO ERRCD.
       EXIT-INITIALIZE.
           EXIT.

      *********************************************************
       DETAIL-LINE-PROCESS SECTION.
      *********************************************************

           ADD  1  TO  PAGE-LINE-NO.
           MOVE 1  TO  LN-FEED.
           MOVE 1  TO  SUB-1.

           IF PAGE-LINE-NO > 9
              MOVE 1  TO  PAGE-LINE-NO
              PERFORM PRINT-MAIN-HEADER.

           PERFORM FILL-ACTION-LINES.

           PERFORM WRITE-ACTION-LINES.

           PERFORM CLEAR-FIELDS.

           GO TO EXIT-DETAIL-LINE-PROCESS.

      *-----------------------------------------------------------
       FILL-ACTION-LINES.
           PERFORM FILL-SCHEDULE UNTIL SUB-1 > 17.
           MOVE WS-PAY-SCHEDULE(1)  TO A1-NUMBER-1.
           MOVE WS-PAY-SCHEDULE(2)  TO A1-NUMBER-2.
           MOVE WS-PAY-SCHEDULE(3)  TO A1-NUMBER-3.
           MOVE WS-PAY-SCHEDULE(4)  TO A1-NUMBER-4.
           MOVE WS-PAY-SCHEDULE(5)  TO A1-NUMBER-5.
           MOVE WS-PAY-SCHEDULE(6)  TO A2-NUMBER-1.
           MOVE WS-PAY-SCHEDULE(7)  TO A2-NUMBER-2.
           MOVE WS-PAY-SCHEDULE(8)  TO A2-NUMBER-3.
           MOVE WS-PAY-SCHEDULE(9)  TO A2-NUMBER-4.
           MOVE WS-PAY-SCHEDULE(10) TO A2-NUMBER-5.
           MOVE WS-PAY-SCHEDULE(11) TO A3-NUMBER-1.
           MOVE WS-PAY-SCHEDULE(12) TO A3-NUMBER-2.
           MOVE WS-PAY-SCHEDULE(13) TO A3-NUMBER-3.
           MOVE WS-PAY-SCHEDULE(14) TO A3-NUMBER-4.
           MOVE WS-PAY-SCHEDULE(15) TO A3-NUMBER-5.
           MOVE WS-PAY-SCHEDULE(16) TO A4-NUMBER-1.
           MOVE WS-PAY-SCHEDULE(17) TO A4-NUMBER-2.

      *-----------------------------------------------------------
       FILL-SCHEDULE.
           IF LTP-SCHLD-PAYMNT(SUB-1) NOT = 0
                                     AND LTP-SCHLD-TERM(SUB-1) NOT = 0
              MOVE SUB-1 TO WS-TABLE-COUNT
              MOVE LTP-SCHLD-TERM(SUB-1) TO WS-PAYSCH-TERM(SUB-1)
              MOVE LTP-SCHLD-PAYMNT(SUB-1) TO WS-PAYSCH-PYMNT(SUB-1)
              INSPECT WS-TABLE-COUNT REPLACING ALL "/" BY "."
              STRING WS-TABLE-COUNT DELIMITED BY SIZE
                     WS-PAYSCH-TERM(SUB-1) DELIMITED BY SIZE
                     AT-SIGN DELIMITED BY SIZE
                     WS-PAYSCH-PYMNT(SUB-1) DELIMITED BY SIZE
                     INTO WS-PAY-SCHEDULE(SUB-1).
           ADD 1 TO SUB-1.

      *-----------------------------------------------------------
       WRITE-ACTION-LINES.
           MOVE 1         TO  LN-FEED.

           MOVE LN-ACCTNO TO A1-ACCTNO.
           MOVE ACTION1   TO  DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE ACTION2   TO  DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE ACTION3   TO  DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE ACTION4   TO  DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE ALL "-"   TO  DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           PERFORM CLEAR-ACTION-LINES.

      *-----------------------------------------------------------
       CLEAR-ACTION-LINES.
           MOVE " "  TO   ACTION1.
           MOVE " "  TO   ACTION2.
           MOVE " "  TO   ACTION3.
           MOVE " "  TO   ACTION4.
           MOVE ZERO    TO   A1-ACCTNO.

      *-----------------------------------------------------------
       CLEAR-FIELDS.
           MOVE SPACES TO WS-PAY-SCHEDULE(1).
           MOVE SPACES TO WS-PAY-SCHEDULE(2).
           MOVE SPACES TO WS-PAY-SCHEDULE(3).
           MOVE SPACES TO WS-PAY-SCHEDULE(4).
           MOVE SPACES TO WS-PAY-SCHEDULE(5).
           MOVE SPACES TO WS-PAY-SCHEDULE(6).
           MOVE SPACES TO WS-PAY-SCHEDULE(7).
           MOVE SPACES TO WS-PAY-SCHEDULE(8).
           MOVE SPACES TO WS-PAY-SCHEDULE(9).
           MOVE SPACES TO WS-PAY-SCHEDULE(10).
           MOVE SPACES TO WS-PAY-SCHEDULE(11).
           MOVE SPACES TO WS-PAY-SCHEDULE(12).
           MOVE SPACES TO WS-PAY-SCHEDULE(13).
           MOVE SPACES TO WS-PAY-SCHEDULE(14).
           MOVE SPACES TO WS-PAY-SCHEDULE(15).
           MOVE SPACES TO WS-PAY-SCHEDULE(16)
           MOVE SPACES TO WS-PAY-SCHEDULE(17).

       EXIT-DETAIL-LINE-PROCESS.
           EXIT.

      *********************************************************
       WRITE-DETAIL-LINE SECTION.
      *********************************************************
           MOVE DETAIL-LINE  TO  PR-REC.
           PERFORM WRITE-PR-REC.
           MOVE 1            TO  LN-FEED.
           MOVE " "       TO  DETAIL-LINE.

      *********************************************************
       PRINT-MAIN-HEADER SECTION.
      *********************************************************
           MOVE "N"         TO  FIRST-TIME.
           ADD 1            TO  PAGE-NUMBER.
           MOVE PAGE-NUMBER TO  H-PAGE-NO.

           PERFORM GET-TIME.
           MOVE T-HH        TO  H-HOUR.
           MOVE T-MM        TO  H-MIN.

           MOVE HEAD1       TO  PR-REC.
           MOVE 99          TO  LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE HEAD2       TO  PR-REC.
           MOVE 1           TO  LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE HEAD3          TO  PR-REC.
           MOVE 1              TO  LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE ALL "-"     TO  PR-REC.
           PERFORM WRITE-PR-REC.

           MOVE 1         TO  LN-FEED.
       EXIT-HEAD-PAGE.
           EXIT.

      *********************************************************
       PRINT-RANGE-LINES SECTION.
      *********************************************************
           MOVE 2                        TO  LN-FEED.
           MOVE "BEGINNING LOAN NUMBER"   TO  RANGE-LINE.
           MOVE BEG-LN                   TO  RANGE-SELECTION.

           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING LOAN NUMBER"      TO  RANGE-LINE.
           MOVE END-LN              TO  RANGE-SELECTION.

           PERFORM WRITE-DETAIL-LINE.

      ********************************************************
       BWLN-SCAN SECTION.
      ********************************************************
           MOVE 0                  TO BWLNSC-ACCTNO.
           MOVE EXT-CONAME-SYSDATE TO BWLNSC-SYSDATE.
           MOVE "WI/BWLNSC " TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH BWLNSC-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF BWLNSC-STATUS NOT = "00" OR BWLNSC-ACCTNO = 0
                MOVE 9 TO IO-FG
                GO TO EXIT-BWLN-SCAN.
           IF ( LOAN-FG = "BEG" )
               MOVE "SNNN060LOANNO1." TO VDU
           ELSE
               MOVE "SNNN060LOANNO2." TO VDU.
           MOVE BWLNSC-ACCTNO TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE BWLNSC-ACCTNO TO VDUNUM0.
           MOVE 0 TO IO-FG.

       EXIT-BWLN-SCAN.
           EXIT.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/TIMEIO.CPY".

      ****************************************************
       FORM-RESET SECTION.
      ****************************************************
           MOVE EXT-CONAME-CONAME TO PSLIST-CONAME.
           MOVE EXT-CONAME-SYSDATE TO PSLIST-SYSDATE.
           DISPLAY PSLIST-SCREEN UPON PSLIST-WINDOW-HANDLE.
           MOVE PSLIST-FORM-FIELDS TO WR-BUF.
           MOVE PSLIST-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ****************************************************
       SETUP-LINK-INFO SECTION.
      ****************************************************
           MOVE PSLIST-HELP--FILE TO HELP-LINK-FILE.
           MOVE PSLIST-HELP--DIR  TO HELP-LINK-DIR.

      ****************************************************
       VDU-CONVERT-FIELD SECTION.
      ****************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLS/PSLIST_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *********************************************************
       MISC SECTION.
      *********************************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/ACCESS.CPY".

      *********************************************************
       IO-ROUTINE SECTION.
      *********************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLTPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBLP/LPLTP1RN.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      *********************************************************
       END-PROGRAM SECTION.
      *********************************************************
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
           DESTROY PSLIST-WINDOW-HANDLE.
           EXIT PROGRAM.
