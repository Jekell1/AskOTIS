       IDENTIFICATION DIVISION.
       PROGRAM-ID.      FXMIDA.
       DATE-WRITTEN.    090910.
      ******************************************************************
      *
      *  DESCRIPTION:  FIX LATE CHARGE PAID THRU (LCPDTH)
      *                DETERMINED BY SPR
      *
      *=================================================================
      * REV:
      * 100115 LP-ALL-PATH WAS NOT BEING SET, REGACC PR# 263
      * BAH 160128 SPLIT OUT LTFILE, PD0003
      *
      * KEC 2017.0522 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          THE FIELD CONVERT-DATE IS DEFINED AS AN INTERNAL FIELD
      *          BUT ALSO IN LIBGB/DATERW (NEW FOR A30).  SO CHANGED
      *          THE INTERNAL FIELD AS WSS-CONVERT-DATE.
      * BAH 181203 GLOBAL LNFILE
      *
      * KEC 2019-0123 A32 - GPFILE GP-REC MODIFICATION 
      *
      *  2019.0531 BAH REMOVED ENDING BRANCH AND GROUP LOGIC, ONLY ALLOW
      *                BRANCHES > 8000 AND < 8099 WITH BR-LR-GROUP-COLLID "X"
      *                SAFEGUARDS TO ONLY RUN FOR CONVERSION BRANCH  #1375
      * BAH 190918 REMOVED ACCESS-CALL ON LNFILE
      * BAH 2022.1004 HARD CODED STARTS
      * BAH 20240129 CALL CRNOR2 AND WRITE NOTICE RECORD
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
       DATA DIVISION.
       FILE SECTION.
 
       COPY "LIBGB/GBFDPR.CPY".
 
      ******************************************
      *      WORKING STORAGE SECTION
      ******************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)FX/FXMIDA.CBL S34 05/21/21-08:33:07 >".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01LTP.CPY".
       COPY "LIBLP/LP01LTP_SQL.CPY".
       COPY "LIBLP/LPWSLTP.CPY".
       COPY "LIBLP/LP01NO.CPY".
       COPY "LIBLP/LP01NO_SQL.CPY".
       COPY "LIBLP/LPWSNO.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01LP.CPY".
       COPY "LIBLP/LP01LP_SQL.CPY".
       COPY "LIBLP/LPWSLP.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GP.CPY".
       COPY "LIBGB/GB01GP_SQL.CPY".
       COPY "LIBGB/GBWSGP.CPY".
       COPY "LIBGB/GBWSGR.CPY".

       01  PR-IFN            PIC X(2) VALUE "PR".

       01  ERRCD             PIC X.
       01  BEG-BRANCH        PIC 9999 VALUE 0.
       01  BR1               PIC 9999 VALUE 0.
       01  ACCEPT-BUF        PIC X.

       01  WSS-CONVERT-DATE  PIC 9(8) COMP.
       01  PAYMENT-DUE-DATE  PIC 9(8) COMP.
       01  ASSESS-THRU-DATE  PIC 9(8) COMP.
      * YYYYMMDD
       01  TODAYS-DATE       PIC 9(8).

      **********************
      *   LINE WORKERS     *
      **********************
       01  LN-WORKERS.
           03  LN-FEED          PIC 99       VALUE 0.
           03  LN-COUNT         PIC 999      VALUE 99.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.
 
       01  MESS-LIST         PIC X(5) VALUE "MESS.".
       01  MESS-BUF          PIC X(40) VALUE SPACES.
 
      ***********************************
      *     P R I N T   B U F F E R S   *
      ***********************************
       01  HEAD1.
           03  H-DATE           PIC X(8)     VALUE " ".
           03  FILLER           PIC XX       VALUE " ".
           03  H-HR             PIC 99       VALUE 0.
           03  FILLER           PIC X        VALUE ":".
           03  H-MIN            PIC 99       VALUE 0.
           03  FILLER           PIC X(5)     VALUE " ".
           03  H-NAME           PIC X(60)    VALUE " ".
           03  FILLER           PIC X(12)    VALUE " ".
           03  FILLER           PIC X(5)     VALUE "PAGE ".
           03  H-PAGE-NO        PIC ZZZ9.
 
       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(4)     VALUE " ".
           03  H-TITLE          PIC X(55)    VALUE " ".
           03  FILLER           PIC X(14)    VALUE "CONVERT DATE: ".
           03  H-CONVERT        PIC 99/99/99.
 
       01  HEAD3.
           03  FILLER           PIC X(10)    VALUE "BRANCH:".
           03  H-BRNO           PIC Z999.
           03  FILLER           PIC X(2).
           03  H-BRNAME         PIC X(45)    VALUE " ".
 
       01  HEAD4.
           03  FILLER      PIC X(31)  VALUE " ".
           03  FILLER      PIC X(19)  VALUE "                   ".
           03  FILLER      PIC X(14)  VALUE " ".
           03  FILLER      PIC X(22)  VALUE " ".
           03  FILLER      PIC X(20)  VALUE " ".
 
       01  HEAD5.
           03  FILLER      PIC X(31)  VALUE " ACCOUNT  NAME".
           03  FILLER      PIC X(19)  VALUE "   PDTH      LCPDTH".
           03  FILLER      PIC X(14)  VALUE "    STATUS OF ".
           03  FILLER      PIC X(22)  VALUE "SET                   ".
           03  FILLER      PIC X(20)  VALUE "              ".
 
       01  DETAIL-LINE       PIC X(132) VALUE SPACES.
 
       01  D-LINE1          REDEFINES DETAIL-LINE.
           03  D-ACCTNO      PIC 99999999.
           03  FILLER        PIC XX.
           03  D-NAME        PIC X(20).
           03  D-PDTH        PIC 99/99/99.
           03  FILLER        PIC X(4).
           03  D-LCPDTH      PIC 99/99/99.
           03  FILLER        PIC X(4).
           03  D-STATUS      PIC X(40).

 
       COPY "LIBWI/PFSWINDOWSW.CPY".
       COPY "LIBWI/TIMERW.CPY".

       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/SPPDTHW.CPY".
       COPY "LIBCL/CRNOR2W.CPY".
       COPY "LIBLP/LPPOFFW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/GETPRW.CPY".
 
       01  FXMIDA-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBFX/FXMIDA_DEF.CPY".
       COPY "LIBWI/TIMER_DEF.CPY".
       COPY "LIBFX/FXMIDA_WKS.CPY".
       COPY "LIBWI/TIMER_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".
       COPY "LIBGB/GETFMW.CPY".
       
       SCREEN SECTION.
       COPY "LIBFX/FXMIDA_SCN.CPY".
       COPY "LIBWI/TIMER_SCN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
                PR-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LTP1-FILE.
           PERFORM CLOSE-NO1-FILE.
           PERFORM CLOSE-LP1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GP-FILE.
           PERFORM CLOSE-BR-FILE.
           CLOSE PR-FILE.
       COPY "LIBGB/DECLAR2.CPY".
 
      **********************************
      *       MAIN PROGRAM CONTROL
      **********************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

           MOVE " " TO ERRCD.

           MOVE "FXMIDA" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS FXMIDA-WINDOW-HANDLE.

       COPY "LIBGB/CHKSEC.CPY".
           PERFORM FORM-RESET.


       GETPR.
           MOVE FXMIDA-QUEUE-POS TO Q-POS.
           MOVE FXMIDA-COPIES-POS TO C-POS.
           MOVE FXMIDA-PRU-POS TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
           PERFORM PRU-IN.
           IF STAT NOT = "00"
              GO TO END-ROUTINE.

       ENTER-BR1.
           MOVE "ENNN040BR1." TO VDU.
           PERFORM SCREENIO.
           IF ( VDUSTATUS = "1>" )
              GO TO END-ROUTINE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ENTER-BR1.

           MOVE VDUNUM0 TO BR1 BEG-BRANCH.
           IF BEG-BRANCH < 8000 OR > 8099
              MOVE "INVALID BRANCH" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BR1.

           PERFORM OPEN-BR-FILE.
           MOVE BEG-BRANCH TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-FG NOT = 0
              MOVE "INVALID BRANCH" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BR1.

           IF BR-LR-GROUP-COLLID NOT = "X"
              MOVE "BRANCH NOT SET AS CONVERSION BRANCH" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BR1.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO BW-PATH-OVERRIDE
                                              LN-PATH-OVERRIDE
                                              LP-PATH-OVERRIDE
                                              LTP-PATH-OVERRIDE
                                              NO-PATH-OVERRIDE.

      *-----------------------------------------------------------------
       ENTER-CONVERT.
           MOVE "ERNM060CONVERT." TO VDU.
           PERFORM SCREENIO.
           IF ( VDUSTATUS = "1>" )
              GO TO END-ROUTINE.
           IF ( VDUSTATUS = "1U" )
              GO TO ENTER-BR1.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ENTER-CONVERT.

           MOVE VDU-DATE-YYYYMMDD TO WSS-CONVERT-DATE.

       ENTER-ACCEPT.
           MOVE "E  A010ACCEPT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U" GO TO ENTER-CONVERT.
           IF VDUSTATUS = "1>" GO TO END-ROUTINE.
           IF VDUSTATUS NOT = "00" GO TO ENTER-ACCEPT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - -{ TIMER  }
           IF ( VDUBUF = "T" )
              PERFORM TIMER-ROUTINE
              MOVE TIMER-ACCEPT-YN TO VDUBUF
              IF ( VDUBUF = "Y" )
                 MOVE "U" TO VDUBUF.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF VDUBUF = "N" GO TO END-ROUTINE.

           IF NOT (VDUBUF = "P" OR "U")
              GO TO ENTER-ACCEPT.

           MOVE VDUBUF TO ACCEPT-BUF.

           MOVE EXT-CONAME-CONAME  TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID  TO H-USERID.
           IF ACCEPT-BUF = "U"
              MOVE "LATE CHARGE PAID THRU FIX, PRINT & UPDATE"
                                                    TO H-TITLE
           ELSE
              MOVE "LATE CHARGE PAID THRU FIX, PRINT ONLY"
                                                    TO H-TITLE.
           MOVE WSS-CONVERT-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY      TO H-CONVERT.

           PERFORM OPEN-PR-FILE.

           PERFORM OPEN-SP1-FILE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    NOTE: NEED TO READ THE GPFILE HERE AND NOT USE GPENV;
      *          SOME GP FIELDS MAY HAVE CHANGED SINCE GPENV WAS SET SO
      *          NEED TO MAKE SURE WE HAVE THE LATEST UP-TO-DATE GP-REC.
      *          KEC 2019-0123 A32 - GPFILE GP-REC MODIFICATION 
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 1 TO GP-KEY.
           PERFORM OPEN-GP-FILE.
           PERFORM READ-GP-FILE.
           PERFORM CLOSE-GP-FILE.

           ACCEPT TODAYS-DATE FROM CENTURY-DATE.

           PERFORM GLOBAL-UPDATE.

       END-ROUTINE.
           GO TO END-PROGRAM.

      *******************************
       GLOBAL-UPDATE SECTION.
      *******************************

           PERFORM OPEN-BW1-FILE.
           PERFORM OPEN-NO1-FILE.
 
           MOVE BR-NO TO H-BRNO.
           MOVE BR-NAME TO H-BRNAME.
           PERFORM HEAD-PAGE.

           PERFORM OPEN-LN1-FILE.
           MOVE LN-PATH-OWNBR TO LN-OWNBR.
           MOVE 0             TO LN-ACCTNO.                   

           PERFORM START-LN1-FILE.

           MOVE "PROGRAM IN PROGRESS..........." TO MESS-BUF.
           PERFORM SEND-LEGEND.
 
       GLOBAL-UPDATE-NEXT-LN.
           PERFORM READ-LN1-FILE-NEXT.
           IF IO-FG = 9 OR (LN-OWNBR NOT = LN-PATH-OWNBR)
              GO TO GLOBAL-UPDATE-EXIT.

      * SKIP LOANS
           IF ( LN-POCD-NONLOAN   ) OR
              ( LN-CURBAL = 0     )
      *         UNLOCK LN-FILE
                GO TO GLOBAL-UPDATE-NEXT-LN.

      * SKIP LOANS ALREADY POSTED TO SINCE CONVERSION
           IF LN-DATE-PAID-LAST NOT = 0
              IF LN-DATE-PAID-LAST > WSS-CONVERT-DATE
                 GO TO GLOBAL-UPDATE-NEXT-LN.

      * READ STATE PARAMETER RECORD
           MOVE LN-ORGST    TO SP-ORGST.
           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           MOVE LN-LAWCODE  TO SP-LAWCODE.
           PERFORM READ-SP1-FILE.
           IF IO-FG NOT = 0
      *       UNLOCK LN-FILE
              GO TO GLOBAL-UPDATE-NEXT-LN.

           IF SP-LCFRMLA = " "
      *       UNLOCK LN-FILE
              GO TO GLOBAL-UPDATE-NEXT-LN.

           MOVE LN-REC TO HOLD-LN-REC.

      * GET PAID THRU:
           MOVE LN-1STPYDATE TO PDTH-DATE-WORK.
           PERFORM PAID-THRU-CALCULATION.
           MOVE PDTH-DATE-FULL  TO LN-LCPDTH-DATE.

      * GET PAYMENT DUE DATE:
           MOVE PDTH-DATE-FULL  TO NDTE-DATE.
           MOVE LN-UNITPER-CD   TO DATER-UNITPER-CD.
           MOVE LN-UNITPER-FREQ TO DATER-UNITPER-FREQ.
           MOVE 1               TO NDTE-HOLD.
           PERFORM INCREMENT-UNITPER.
           MOVE NDTE-DATE       TO PAYMENT-DUE-DATE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    SKIP LOAN WHEN LOAN PAYMENT DUE DATE (07/24/03) IS ON OR AFTER
      *    CONVERSION DATE (07/31/03)
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF PAYMENT-DUE-DATE NOT < WSS-CONVERT-DATE
              MOVE "SKIPPED" TO D-STATUS
              PERFORM PRINT-LOAN
              IF ACCEPT-BUF = "U"
                 PERFORM FILE-UPDATES
              END-IF
              GO TO GLOBAL-UPDATE-NEXT-LN.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    PROCESS LOAN WHEN LOAN DUE DATE (07/24/03) IS PRIOR TO
      *    CONVERSION DATE (07/31/03), WHICH MAKES LOAN ONE OR MORE
      *    DAYS PAST DUE
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      * DETERMINE ASSESS-THRU-DATE:
           MOVE WSS-CONVERT-DATE TO NDTE-DATE.
           MOVE SP-LCGRACE   TO NDTE-HOLD.
           MULTIPLY -1 BY       NDTE-HOLD.
           PERFORM INCREMENT-DAYS.
           MOVE NDTE-DATE    TO ASSESS-THRU-DATE.
           
           MOVE LN-ACCTNO        TO CRNOR2-ACCTNO.
           MOVE WSS-CONVERT-DATE TO CRNOR2-SYSDATE.
           MOVE "PO"             TO CRNOR2-LPTRCD.
           MOVE "N"              TO CRNOR2-LPREC-FG.
           MOVE "CL/CRNOR2"   TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CRNOR2-LINK NO-REC
                                 POFF-WORKERS LN-REC.
           CANCEL FORM-PROGX.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    WHEN ACCOUNT'S LAST PAYMENT DUE DATE (NO-LDUE) IS 
      *    PRIOR OR ON ASSESS THRU DATE ACCOUNT HAS RUN THROUGH BATCH
      *    AND HAD LATE CHARGES ADDED TO LN-LCBAL
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF NO-LDUE-DATE NOT > ASSESS-THRU-DATE
              MOVE NO-LDUE-DATE       TO LN-LCPDTH-DATE
              MOVE "RAN THRU BATCH HAD LCHG PUT IN OWING" TO D-STATUS
              PERFORM PRINT-LOAN
              IF ACCEPT-BUF = "U"
                 PERFORM FILE-UPDATES
              END-IF
              GO TO GLOBAL-UPDATE-NEXT-LN.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    WHEN ACCOUNT'S LAST PAYMENT DUE DATE (NO-LDUE) IS AFTER
      *    ASSESS THRU DATE, BUT THE ACCOUNTS PAYMENT DUE DATE
      *    IS PRIOR TO LAST DUE LATE CHARGES WERE PUT INTO OWING
      *    ON A PRIOR RUN
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF PAYMENT-DUE-DATE < NO-LDUE-DATE
              MOVE NO-LDUE-DATE TO NDTE-DATE
              MOVE -1      TO NDTE-HOLD
              PERFORM INCREMENT-MONTHS
              MOVE NDTE-DATE TO LN-LCPDTH-DATE
              MOVE "LCHG PUT IN OWING ON  P R I O R  RUN" TO D-STATUS
              PERFORM PRINT-LOAN
              IF ACCEPT-BUF = "U"
                 PERFORM FILE-UPDATES
              END-IF
              GO TO GLOBAL-UPDATE-NEXT-LN.
 
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    WHEN ACCOUNT'S PAYMENT DUE DATE IS AFTER
      *    ASSESS THRU DATE ACCOUNT HAS  N O T  RUN THROUGH BATCH
      *    AND  N O T  HAD LATE CHARGES ADDED TO LN-LCBAL
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "N O  LATE CHARGE PUT IN OWING" TO D-STATUS.
           PERFORM PRINT-LOAN.
           IF ACCEPT-BUF = "U"
              PERFORM FILE-UPDATES
              GO TO GLOBAL-UPDATE-NEXT-LN.
              
           GO TO GLOBAL-UPDATE-NEXT-LN.

       GLOBAL-UPDATE-EXIT.
      *    PERFORM PRINT-TOTALS.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-NO1-FILE.

      ******************************************************************
       PROCESS-LP-FILE SECTION.
      ******************************************************************

      * OPEN-LP1 NEEDS TO STAY HERE, CREATE-NOREC OPENS AND CLOSES
           PERFORM OPEN-LP1-FILE.

           MOVE LN-OWNBR  TO LP-BRNO
                             QLP1-WBEG-BRNO
                             QLP1-WEND-BRNO.
           MOVE LN-ACCTNO TO LP-ACCTNO
                             QLP1-WBEG-ACCTNO
                             QLP1-WEND-ACCTNO.
           MOVE ALL "9"   TO LP-SEQNO
                             QLP1-WEND-SEQNO.
           MOVE ALL "0"   TO QLP1-WBEG-SEQNO

           PERFORM START-LP1-FILE-NOT-GREATER.

       PROCESS-LP-FILE-PREVIOUS.
           PERFORM READ-LP1-FILE-PREVIOUS.
           IF IO-BAD
              GO TO PROCESS-LP-FILE-EXIT.

           IF LP-BRNO NOT = LN-OWNBR OR LP-ACCTNO NOT = LN-ACCTNO
              GO TO PROCESS-LP-FILE-EXIT.

           IF LP-TRCD = "BF"
      * SET PAID THRU:
              MOVE LN-1STPYDATE TO PDTH-DATE-WORK
              PERFORM PAID-THRU-CALCULATION
              MOVE PDTH-DATE-FULL TO LP-PDTH-DATE
              MOVE LN-LCPDTH-DATE TO LP-LCPDTH-DATE
              PERFORM REWRITE-LP1-FILE
              GO TO PROCESS-LP-FILE-EXIT.

           GO TO PROCESS-LP-FILE-PREVIOUS.

       PROCESS-LP-FILE-EXIT.
           PERFORM CLOSE-LP1-FILE.

      ***********************************************
       UPDATE-NOTICE-FILE SECTION.
      ***********************************************
           MOVE LN-OWNBR  TO NO-BRNO.
           MOVE LN-ACCTNO TO NO-ACCTNO CRNOR2-ACCTNO.
           PERFORM READ-NO1-FILE.

      * SET IT BACK TO "PY", WAS SET TO "PO" ABOVE AND USED A DIFFERENT DATE
           MOVE TODAYS-DATE    TO CRNOR2-SYSDATE.
           MOVE "PY"           TO CRNOR2-LPTRCD.
           MOVE " "            TO CRNOR2-LPREC-FG.
           MOVE "CL/CRNOR2"    TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CRNOR2-LINK NO-REC
                                 POFF-WORKERS LN-REC.
           CANCEL FORM-PROGX.

           MOVE LN-OWNBR  TO NO-BRNO.
           MOVE LN-ACCTNO TO NO-ACCTNO.
           IF IO-FG = 0
              PERFORM REWRITE-NO1-FILE
           ELSE
              PERFORM WRITE-NO1-FILE.

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
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
 
           MOVE HEAD4 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
 
           MOVE HEAD5 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
 
       END-HEAD-PAGE.
           MOVE 7 TO LN-COUNT.
           MOVE 2 TO LN-FEED.


      ****************************************************
       FORM-RESET SECTION.
      ****************************************************
           MOVE EXT-CONAME-CONAME TO FXMIDA-CONAME.
           MOVE EXT-CONAME-SYSDATE TO FXMIDA-SYSDATE.
           DISPLAY FXMIDA-SCREEN UPON FXMIDA-WINDOW-HANDLE.
           MOVE FXMIDA-FORM-FIELDS TO WR-BUF. 
           MOVE FXMIDA-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ****************************************************
       SETUP-LINK-INFO SECTION.
      ****************************************************
           MOVE FXMIDA-HELP--FILE TO HELP-LINK-FILE.
           MOVE FXMIDA-HELP--DIR  TO HELP-LINK-DIR.

      ****************************************************
       VDU-CONVERT-FIELD SECTION.
      ****************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBFX/FXMIDA_EVA.CPY".
              COPY "LIBWI/TIMER_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************
       MISC-PARA SECTION.
      ******************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
       FILE-UPDATES.
           IF LN-REC NOT = HOLD-LN-REC
              PERFORM REWRITE-LN1-FILE
              PERFORM UPDATE-NOTICE-FILE
              PERFORM PROCESS-LP-FILE.
       SEND-LEGEND.
           MOVE MESS-BUF TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       INVALID-RANGE.
           MOVE "INVALID RANGE" TO MESS.
           PERFORM SEND-MESS.
       PRINT-LOAN.
           MOVE LN-ACCTNO TO D-ACCTNO.
           MOVE LN-SSNO(1) TO BW-SSNO.
           PERFORM READ-BW1-FILE.
           IF IO-FG NOT = 0
              MOVE SPACES TO BW-LNAME.
           MOVE BW-LNAME TO D-NAME.
           MOVE PDTH-DATE-FULL TO D-PDTH.

           MOVE LN-LCPDTH-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-LCPDTH.

           PERFORM WRITE-DETAIL-LINE.

       COPY "LIBWI/PFSWINDOWS.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBWI/TIMER.CPY".
       COPY "LIBGB/SPPDTH.CPY".
           
      *******************************************
       IO-ROUTINE SECTION.
      *******************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLTPGS_SQL.CPY".
       COPY "LIBLP/LPNOGS_SQL.CPY".
       COPY "LIBLP/LPLPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBGPGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBLP/LPLN1IN.CPY".
       COPY "LIBLP/LPLP1IN.CPY".
       COPY "LIBLP/LPLTP1RN.CPY".
       COPY "LIBLP/LPNO1IN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGPRN.CPY".
       COPY "LIBGB/GBPRINT.CPY".
 
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.
 
      *******************************************
       END-PROGRAM SECTION.
      *******************************************
       END-RUN.
           PERFORM CLOSE-FILES.
           PERFORM PRU-OUT.
       EXIT-PROG.
           MOVE EXIT-PATH TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY FXMIDA-SCREEN.
           DESTROY FXMIDA-WINDOW-HANDLE.
           EXIT PROGRAM.
