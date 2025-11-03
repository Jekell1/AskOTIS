       IDENTIFICATION DIVISION.
       PROGRAM-ID.      MISPAY.
       DATE-WRITTEN.    09/08/2022.
      *****************************************************
      *  DESC: UPDATE LS-M-MISPAY-90 FOR ILLINOIS LOANS
      *        ILMEXT UPDATE NEEDS THIS FIELD - VERITEC
      *
      * TO BE RAN NIGHTLY
      * DRIVES OFF THE LOAN MASTER
      * ONLY GET LOANS MADE ON OR AFTER 8/1/22
      * ONLY ORGST "IL"
      * SKIP VOIDS AND PAIDOUTS AND CLASS 50
      * 1STPYDA MUST = TODAYS DAY OR (TODAY-DD > 28 AND 1STPYDA = 29,30,31)
      * SET LS-M-MISPAY-90 TO THE SCHEDULED PAYMENT DATE THAT IS 3 MONTHS
      *     PAST DUE (TODAYS-DATE - 3MO) OR NDTE-DATE
      *
      * REV:
      * 20220908 BAH NEW #1570
      * 20231117 BAH REMOVED CRNOR2 AND PERFORM WHAT WAS NEEDED FOR
      *              TRUE-DUE-DATE
      * 2024.1004 BAH REMOVE TEST OF BR-STATE NOT = "IL", USING VIRTUAL
      *               BRANCHES AND BR-STATE WILL BE "SC" #1681
      * 2025.0430 RMZ PERFORMANCE TUNING - ADDED STORED PROCEDURE. (S35Q-203)
      *
      *****************************************************
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
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)SP/MISPAY S35 10/17/22-13:26:33 >".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBLP/LP01LSM.CPY".
       COPY "LIBLP/LP01LSM_SQL.CPY".
       COPY "LIBLP/LPWSLSM.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01LTP.CPY".
       COPY "LIBLP/LP01LTP_SQL.CPY".
       COPY "LIBLP/LPWSLTP.CPY".

      *    **********************************
      *    *      SQL WORKERS
      *    **********************************

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.  
       01  QWS-TODAYS-DATE      PIC X(10).
       EXEC SQL END DECLARE SECTION END-EXEC.

       01  LN3-CONN-OPEN-SW     PIC X        VALUE "N".


      *    **********************************
      *    *      PROGRAM WORKERS
      *    **********************************

       01  TOTAL-MISPAY        PIC 9(9)   VALUE 0.
       01  TOTAL-BRANCH-MISPAY PIC 9(7)   VALUE 0.
       01  WS-TRUE-DUE-DATE    PIC 9(8)   VALUE 0.

       01  HOLD-BRNO           PIC 9(4)   VALUE 0.
       01  LN-WORKERS.
           03  LN-FEED         PIC 99     VALUE 0.
           03  LN-COUNT        PIC 999    VALUE 99.
           03  LN-WK           PIC 999    VALUE 0.
       01  PAGE-NUMBER         PIC 9(4)   VALUE 0.

       01  HEAD1.
           03  H-DATE           PIC X(8)     VALUE " ".
           03  FILLER           PIC XX       VALUE " ".
           03  H-HR             PIC 99       VALUE 0.
           03  FILLER           PIC X        VALUE ":".
           03  H-MIN            PIC 99       VALUE 0.
           03  FILLER           PIC X(6)     VALUE " ".
           03  H-NAME           PIC X(40)    VALUE " ".
           03  FILLER           PIC X(5)     VALUE " ".
           03  FILLER           PIC X(5)     VALUE "PAGE ".
           03  H-PAGE-NO        PIC ZZZ9.

       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(6)     VALUE " ".
           03  FILLER           PIC X(45)    VALUE
           "   - - SET MISPAY 90 - - ".

       01  HEAD3.
           03  FILLER           PIC X(70)     VALUE
               "BRNO   NUMBER   1ST PAY    TRUE DUE    MISPAY".

       01  DETAIL-LINE          PIC X(80) VALUE SPACES.
       01  D-LINE REDEFINES DETAIL-LINE.
           03  D-BRNO           PIC Z(4).
           03  FILLER           PIC X(3).
           03  D-ACCTNO         PIC Z(6).
           03  FILLER           PIC X(3).
           03  D-1STPYDATE      PIC 99/99/99.
           03  FILLER           PIC X(3).
           03  D-TRUE-DUE       PIC 99/99/99.
           03  FILLER           PIC X(3).
           03  D-MISPAY         PIC 99/99/99.
           03  FILLER           PIC X(3).

       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03  RANGE-SELECTION  PIC X(30).
           03  FILLER           PIC X(5).
           03  RANGE1           PIC X(10).
           03  RANGE1N REDEFINES RANGE1 PIC 9(10).
           03  FILLER           PIC X(2).
           03  RANGE2N          PIC 9(10).

       COPY "LIBGB/EOBUF_EXT.CPY".
           03  BEG-BR           PIC 9(4).
           03  END-BR           PIC 9(4).
           03  BEG-CL           PIC 9(3).
           03  END-CL           PIC 9(3).
           03  GROUP-FLAG       PIC X.
           03  ENT-GROUP        PIC X(4).
           03  TODAYS-DATE      PIC 9(8).
           03  TODAYS-DATEX REDEFINES TODAYS-DATE.
               05  TODAYS-YYYY  PIC 9(4).
               05  TODAYS-MM    PIC 9(2).
                   88 TODAYS-MM-TEST  VALUE 4,6,11.
               05  TODAYS-DD    PIC 9(2).
           03  REPORT-FLAG      PIC X.

       01  VERSION-CONTROL      PIC X VALUE "A".

       01  HOLD-SP1-KEY         VALUE SPACES.
           03  HOLD-ORGST       PIC XX.
           03  HOLD-SPRCLASS    PIC 99.
           03  HOLD-SUBCLASS    PIC 99.
           03  HOLD-LAWCODE     PIC 99.

       01  MESS-LIST            PIC X(5) VALUE "MESS.".
       01  DISPLAY-BUF.
           03  FILLER           PIC X(12) VALUE "PROCESSING..".
           03  DISPLAY-BRNO     PIC Z(4).
           03  FILLER           PIC X(34) VALUE SPACES.

       COPY "LIBLP/LPPOFFW.CPY".
       COPY "LIBGB/SPPDTHW.CPY".
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/MISPAY_DEF.CPY".
       COPY "LIBSP/MISPAY_WKS.CPY".
       COPY "LIBLP/ARRAYSPW.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *****************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      *****************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBSP/MISPAY_SCN.CPY".

      *----------------------------------------------------------------
       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LN3-FILE.
           PERFORM CLOSE-LN3-CONNECTION.
           PERFORM CLOSE-LSM1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM SQL-CLOSE-UPDATE-CONNECTION.
           CLOSE
               PR-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      *******************************************
       MAIN-MODULE SECTION.
      *******************************************
           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "MISPAY" TO VDU-FORM-NAME.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

           PERFORM GET-GPENV.

           IF GROUP-FLAG = "N"
              MOVE BEG-BR TO BR-NO
              PERFORM START-BR-FILE
              GO TO NEXT-BRANCH-NO.

           MOVE ENT-GROUP TO GR-ID.
           PERFORM SEARCH-GR-INIT.
       NEXT-GROUP-LOC.
           IF GROUP-FLAG NOT = "Y"
              GO TO NEXT-BRANCH-NO.
           PERFORM SEARCH-GR-NEXT.
           IF WGR-SRESULT = 0
              GO TO END-ROUTINE.

           MOVE WGR-SRESULT TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           GO TO MOVE-BRANCH-PATH-INFO.
       NEXT-BRANCH-NO.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-FG = 9 OR BR-NO > END-BR
              GO TO END-ROUTINE.

       MOVE-BRANCH-PATH-INFO.

           MOVE EXT-FILPATH-BASE TO FILPATH-BASE.
           MOVE BR-MACHINE       TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH     TO FILPATH-DATA.

           MOVE "B"        TO LN-PATH-OVERRIDE
                              LSM-PATH-OVERRIDE
                              LTP-PATH-OVERRIDE.

           IF EXT-ROUTE-BUF = "00"
              MOVE LN-OWNBR TO DISPLAY-BRNO
              MOVE DISPLAY-BUF TO WR-BUF
              MOVE MESS-LIST   TO WR-LIST
              PERFORM CALL-WRFORM.

           IF HOLD-BRNO NOT = BR-NO
              IF HOLD-BRNO NOT = 0
                 PERFORM PRINT-BR-TOTALS
              ELSE
                 MOVE BR-NO TO HOLD-BRNO.

           PERFORM OPEN-LN3-FILE.
           PERFORM OPEN-LSM1-FILE.

           MOVE BR-NO  TO LN-OWNBR
                          QLN3-WBEG-OWNBR
                          QLN3-WEND-OWNBR.
           MOVE BEG-CL TO LN-CLASS
                          QLN3-WBEG-CLASS.
           MOVE END-CL TO QLN3-WEND-CLASS.
           MOVE 0      TO LN-ACCTNO
                          QLN3-WBEG-ACCTNO.
           MOVE ALL "9" TO QLN3-WEND-ACCTNO.

           PERFORM START-LN3-FILE. *> SSP - LNFILE
       NEXT-LN.
           PERFORM READ-LN3-FILE-NEXT. *> EMBEDDED - LNFILE
           IF IO-FG = 9 OR (LN-OWNBR NOT = LN-PATH-OWNBR)
              GO TO END-OF-GROUP.

           IF LN-OWNBR > END-BR
              GO TO END-OF-GROUP.

      * SKIP VOIDS
           IF LN-POCD = "**"
              GO TO NEXT-LN.

      * SKIP PAIDOUTS
           IF LN-POFFDATE NOT = 0
              GO TO NEXT-LN.

      * SKIP CLASS 50
           IF LN-CLASS = 50
              GO TO NEXT-LN.

      * ONLY FOR STATE OF "IL"
           IF LN-ORGST NOT = "IL"
              GO TO NEXT-LN.

           IF LN-CLASS < BEG-CL OR LN-CLASS > END-CL
              GO TO NEXT-LN.

      * IF 1STPAYMENT "DAY" MATCHES TODAYS "DAY", TEST FOR MISPAY 90

      *JANUARY HAS ALL 31 DAYS, TEST FOR OCTOBER DAYS WILL ALL WORK.
      *FEBRUARY HAS EITHER 28 OR 29 DAYS. TEST FOR NOVEMBER WILL NEED TO
      *       SOMETIMES FIND 29 ACCOUNTS AND ALWAYS FIND 30 AND 31 ACCOUNTS.
      *MARCH HAS 31 DAYS, TEST FOR DECEMBER DAYS WILL ALL WORK.
      *APRIL HAS 30 DAYS, TEST FOR JANUARY WILL NEED TO LOOK FOR 31ST.
      *MAY HAS 31 DAYS, TEST FOR FEBRUARY DAYS WILL ALL WORK.
      *JUNE HAS 30 DAYS, TEST FOR MARCH WILL NEED TO LOOK FOR 31ST.
      *JULY HAS 31 DAYS, TEST FOR APRIL DAYS WILL ALL WORK.
      *AUGUST HAS 31 DAYS, TEST FOR MAY DAYS WILL ALL WORK.
      *SEPTEMBER HAS 30 DAYS, TEST FOR JUNE DAYS WILL ALL WORK.
      *OCTOBER HAS 31 DAYS, TEST FOR JULY DAYS WILL ALL WORK.
      *NOVEMBER HAS 30 DAYS, TEST FOR AUGUST WILL NEED TO LOOK FOR 31ST.
      *DECEMBER HAS 31 DAYS ,TEST FOR SEPTEMBER DAYS WILL ALL WORK.
      *

      * EXAMPLE FOR 1STPY NOT MATCHING TODAYS DAY
      * 1STPYDATE 10/1/22   TODAY 1/12/23   MISPAY -3 MONTHS 10/12/22
      * HE DOESNT MATCH THE FIRST IF, SO GOES TO THE ELSE
      * DOESNT MATCH EITHER MONTH CHECK IN THOSE NEXT TO "IF" STATEMENTS
      * SO HE WAS FALLING THROUGH AND GETTING SET, WHEN HE SHOULD NOT
      * BECAUSE HIS 1STPYDA DOES NOT MATCH, BUT YOU HAVE TO CHECK THOSE
      * UNUSUAL MONTH CASES FIRST.

           IF TODAYS-DD = LN-1STPYDA
              NEXT SENTENCE
           ELSE
              IF (TODAYS-MM = 2 AND TODAYS-DD > 27) AND
                     NOT (LN-1STPYDA = 29 OR 30 OR 31)
                 GO TO NEXT-LN
              ELSE
      * TODAYS-MM-TEST  VALUE 4,6,11.
                 IF (TODAYS-MM-TEST AND TODAYS-DD = 30) AND
                             (LN-1STPYDA NOT = 31)
                    GO TO NEXT-LN.

      * HAVE TO CHECK ONE LAST TIME AFTER THE UNUSUAL CHECKS FOR THE MONTHS
      * SEE EXAMPLE ABOVE
           IF TODAYS-DD NOT = LN-1STPYDA
              IF NOT (TODAYS-MM = 2 OR 4 OR 6 OR 11)
                 GO TO NEXT-LN.

      * SKIP LOANS PRIOR TO 8/1/22
           MOVE LN-LOANDATE TO SYS-DATE.
           IF LN-CONVERCD = "Y"
              MOVE LN-ENTDATE TO SYS-DATE.
           IF SYS-DATE < 20220801
              GO TO NEXT-LN.

           MOVE LN-ORGST     TO LPWSSP-ORGST.
           MOVE LN-SPRCLASS  TO LPWSSP-SPRCLASS.
           MOVE LN-SUBCLASS  TO LPWSSP-SUBCLASS.
           MOVE LN-LAWCODE   TO LPWSSP-LAWCODE.
           IF LPWSSP-SP1-KEY NOT = SP1-KEY
              MOVE LPWSSP-SP1-KEY TO SP1-KEY
      *       PERFORM READ-SP1-FILE
              PERFORM ARRAYSP-READ
              IF IO-FG NOT = 0
                 GO TO NEXT-LN.

           MOVE LN-1STPYDATE TO PDTH-DATE-WORK.
           PERFORM PAID-THRU-CALCULATION.
           MOVE PDTH-DATE-FULL TO NDTE-DATE.
           IF LN-UNITPER-CD = "M"
              MOVE LN-1STPYDA TO NDTE-DD.
           MOVE 1 TO NDTE-HOLD.
           PERFORM INCREMENT-UNITPER.
           MOVE NDTE-DATE TO WS-TRUE-DUE-DATE.

      * USING 90 DAYS DIDNT LAND ON A PAY CYCLE, NEED TO USE MONTHS.
           MOVE TODAYS-DATE TO NDTE-DATE.
           MOVE -3          TO NDTE-HOLD.
           MOVE LN-UNITPER-CD   TO DATER-UNITPER-CD.
           MOVE LN-UNITPER-FREQ TO DATER-UNITPER-FREQ.
           PERFORM INCREMENT-UNITPER.

      * ADJUST NDTE-DD TO BE THE ACTUAL DUE DAY! CHECK THE SAME MONTHS
      * WE TEST FOR SKIPPING THE ACCOUNT RIGHT AFTER THE LOAN READ.
      *

      * TODAY FEBRUARY GIVE NDTE NOVEMBER, ALLOW FOR DUE DAY 28,29,30
           IF (TODAYS-MM = 2 AND TODAYS-DD > 27)
              IF LN-1STPYDA = 28 OR 29 OR 30
                 MOVE LN-1STPYDA TO NDTE-DD
              ELSE
                 MOVE 30         TO NDTE-DD.

      * TODAY APRIL, JUNE AND NOVEMBER ON THE 30TH, CHECK IF DUE DAY IS 31
           IF (TODAYS-MM-TEST AND TODAYS-DD = 30)
              IF LN-1STPYDA = 31
                 MOVE LN-1STPYDA    TO NDTE-DD.

      * IF TRUE DUE DATE IS <= (TODAY - 3 MONTHS), THEN UPDATE LS-M-MISPAY-90
      * IF >, SKIP IT
           IF WS-TRUE-DUE-DATE > NDTE-DATE
              GO TO NEXT-LN.

           ADD 1 TO TOTAL-MISPAY TOTAL-BRANCH-MISPAY.

           IF REPORT-FLAG = "Y" OR "O"
              PERFORM DETAIL-LINE-PRINT.

           IF REPORT-FLAG = "Y" OR "N"
              PERFORM UPDATE-LS1-FILE.

           GO TO NEXT-LN.

       END-OF-GROUP.
           PERFORM CLOSE-LN3-FILE.
           PERFORM CLOSE-LSM1-FILE.

           GO TO NEXT-GROUP-LOC.

       END-ROUTINE.
           IF HOLD-BRNO NOT = BR-NO
              IF HOLD-BRNO NOT = 0
                 PERFORM PRINT-BR-TOTALS.

           PERFORM PRINT-RANGE-LINES.

           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           GO TO END-PROGRAM.

      *******************************************
       INITIALIZATION SECTION.
      *******************************************
           MOVE MISPAY-QUEUE-POS TO Q-POS.
           MOVE MISPAY-COPIES-POS TO C-POS.
           MOVE MISPAY-PRU-POS TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
       COPY "LIBGB/EOINIT.CPY".

           IF EXT-ROUTE-BUF = "00"
              MOVE "MISPAY 90 SET IN PROCESS" TO WR-BUF
              MOVE MESS-LIST                TO WR-LIST
              PERFORM CALL-WRFORM.

           PERFORM OPEN-PR-FILE.
           MOVE EXT-CONAME-CONAME  TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           MOVE 0 TO TOTAL-MISPAY HOLD-BRNO.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
      *    PERFORM OPEN-SP1-FILE.
           PERFORM ARRAYSP-LOAD.

           PERFORM SQL-OPEN-UPDATE-CONNECTION.

           GO TO EXIT-INITIALIZE.
       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.
       EXIT-INITIALIZE.
           EXIT.

      *******************************************
       ENTER-PARAMETERS SECTION.
      *******************************************
       ENTER-PARM.

           IF VDUSTATUS = "1U"
              GO TO ENTER-REPORT.

           IF EXT-ROUTE-BUF = "00"
              MOVE EXT-CONAME-SYSDATE TO ALP-DATE
              PERFORM CONVERT-ALPDATE-TO-YYYYMMDD
              MOVE "S  8000DATE1." TO VDU
              MOVE DATE-YYYYMMDD TO VDU-DATE-YYYYMMDD TODAYS-DATE
              PERFORM SCREENIO.

       BR-01.
           MOVE "E  A000BR1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-BR.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF VDUSTATUS = "1<"
              MOVE "WI/GRSCAN" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH GRSCAN-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              IF GRSCAN-STATUS NOT = "00"
                 GO TO BR-01
              ELSE
                 MOVE "S  A000BR1." TO VDU
                 MOVE GRSCAN-KEY TO VDUBUF GR1-KEY
                 PERFORM SCREENIO
                 MOVE "00" TO VDUSTATUS.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BR-01.

           MOVE VDUBUF TO WGR-WORD ENT-GROUP.
           IF WGR-WORD-ZERO
              GO TO BRANCH-IS-NUMERIC.
           MOVE 0 TO WGR-MIN-LEVEL.
           PERFORM CHECK-MEMBER.
           IF WGR-ERR > 0
              MOVE WGR-MSG(WGR-ERR) TO MESS
              PERFORM SEND-MESS
              GO TO BR-01.
           IF WGR-TYPE = "A"
              MOVE "S  A000BR2." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
              MOVE "Y" TO GROUP-FLAG
              MOVE 0 TO BEG-BR
              MOVE 9999 TO END-BR
              GO TO CL-01.
       BRANCH-IS-NUMERIC.
           MOVE "N" TO GROUP-FLAG.
           MOVE WGR-SRESULT TO BEG-BR.
           MOVE "SN N040BR1." TO VDU.
           MOVE BEG-BR TO VDUNUM0.
           PERFORM SCREENIO.
       BR-02.
           MOVE "ENNN040BR2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO BR-02.
           MOVE VDUNUM0 TO END-BR.
           IF BEG-BR > END-BR
              MOVE "INVALID BRANCH RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO BR-01.
           GO TO CL-01.
       ALL-BR.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BEG-BR VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO END-BR VDUNUM0.
           PERFORM SCREENIO.
           MOVE "N" TO GROUP-FLAG.
       CL-01.
           MOVE "ENNN020LC1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-CL.
           IF VDUSTATUS = "1U" GO TO BR-02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CL-01.
           MOVE VDUNUM0 TO BEG-CL.
       CL-02.
           MOVE "ENNN020LC2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO CL-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CL-02.
           MOVE VDUNUM0 TO END-CL.
           IF BEG-CL > END-CL
              MOVE "INVALID CLASS RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO CL-01.
           GO TO ENTER-REPORT.
       ALL-CL.
           MOVE "SNNN020LC1." TO VDU.
           MOVE 0 TO BEG-CL VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020LC2." TO VDU.
           MOVE 99 TO END-CL VDUNUM0.
           PERFORM SCREENIO.
           GO TO ENTER-REPORT.
       ENTER-DATE.
           IF EXT-ROUTE-BUF NOT = "00"
              GO TO EXIT-PARM.

           MOVE "ERNM060DATE1." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO CL-02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ENTER-DATE.

           MOVE VDU-DATE-YYYYMMDD TO TODAYS-DATE.

           IF TODAYS-DATE = 0
              MOVE EXT-CONAME-SYSDATE TO ALP-DATE
              PERFORM CONVERT-ALPDATE-TO-YYYYMMDD
              MOVE "S  8000DATE1." TO VDU
              MOVE DATE-YYYYMMDD TO VDU-DATE-YYYYMMDD TODAYS-DATE
              PERFORM SCREENIO.

       ENTER-REPORT.
           MOVE "E  A010REPORT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U"
              IF EXT-ROUTE-BUF = "40" OR "50"
                 GO TO CL-02
              ELSE
                 GO TO ENTER-DATE.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ENTER-REPORT.

           MOVE VDUBUF TO REPORT-FLAG.
           IF NOT (REPORT-FLAG = "Y" OR "N" OR "O")
              GO TO ENTER-REPORT.


       EXIT-PARM.
           EXIT.

      ****************************************
       EO-EXEC-AUDIT SECTION.
      ****************************************
           MOVE EXT-EOBUF-DATE2 TO TODAYS-DATE.

      ****************************************
       DISPLAY-PARAMETERS SECTION.
      ****************************************
           IF GROUP-FLAG = "Y"
              MOVE "S  A000BR1." TO VDU
              MOVE ENT-GROUP TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A000BR2." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "SNNN040BR1." TO VDU
              MOVE BEG-BR TO VDUNUM0
              PERFORM SCREENIO
              MOVE "SNNN040BR2." TO VDU
              MOVE END-BR TO VDUNUM0
              PERFORM SCREENIO.

           MOVE "SNNN020LC1." TO VDU.
           MOVE BEG-CL TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020LC2." TO VDU.
           MOVE END-CL TO VDUNUM0.
           PERFORM SCREENIO.

      ***************************************
       UPDATE-LS1-FILE SECTION.
      ***************************************
           MOVE LN-OWNBR  TO LSM-BRNO.
           MOVE LN-ACCTNO TO LSM-ACCTNO.
           MOVE 1         TO LSM-SEQNO.
           PERFORM READ-LSM1-FILE. *> EMBEDDED - LSMFILE
           IF IO-FG = 0
      * THEY WANT THE SCHEDULED PAYMENT DATE THAT IS 90 DAYS PAST DUE
              MOVE NDTE-DATE TO LSM-MISPAY-90-DATE 
XXX   *       PERFORM REWRITE-LSM1-FILE *> EMBEDDED - LSMFILE
           ELSE
              PERFORM CLEAR-LSM-FILE
              MOVE LN-OWNBR  TO LSM-BRNO
              MOVE LN-ACCTNO TO LSM-ACCTNO
              MOVE 1         TO LSM-SEQNO
XXX           MOVE NDTE-DATE TO LSM-MISPAY-90-DATE.
XXX   *       PERFORM WRITE-LSM1-FILE. *> EMBEDDED - LSMFILE

      ***************************************
       PRINT-BR-TOTALS SECTION.
      ***************************************
           MOVE "TOTAL MISPAYS IN BRANCH &&&&" TO RANGE-SELECTION.
           INSPECT RANGE-SELECTION REPLACING FIRST "&&&&" BY HOLD-BRNO.
           MOVE TOTAL-BRANCH-MISPAY       TO RANGE1N.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 0     TO TOTAL-BRANCH-MISPAY.
           MOVE BR-NO TO HOLD-BRNO.
           MOVE 2     TO LN-FEED.

      ***************************************
       DETAIL-LINE-PRINT SECTION.
      ***************************************
           MOVE BR-NO            TO D-BRNO.
           MOVE LN-ACCTNO        TO D-ACCTNO.
           MOVE LN-1STPYDATE     TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY      TO D-1STPYDATE.
           MOVE WS-TRUE-DUE-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY      TO D-TRUE-DUE.
      * (TODAY -3 MONTHS)
           MOVE NDTE-DATE        TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY      TO D-MISPAY.
           PERFORM WRITE-DETAIL-LINE.

      ****************************************
       WRITE-DETAIL-LINE SECTION.
      ****************************************
           ADD LN-FEED LN-COUNT GIVING LN-WK.
           IF LN-WK GREATER THAN EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.
           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      ****************************
       HEAD-PAGE SECTION.
      ****************************
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

      *******************************
       PRINT-RANGE-LINES SECTION.
      *******************************

           MOVE 2 TO LN-FEED.
           IF GROUP-FLAG = "Y"
              MOVE "BRANCH GROUP" TO RANGE-SELECTION
              MOVE ENT-GROUP TO RANGE1
              PERFORM WRITE-DETAIL-LINE
           ELSE
              MOVE "BEG BR - END BR" TO RANGE-SELECTION
              MOVE BEG-BR TO RANGE1N
              MOVE END-BR TO RANGE2N
              PERFORM WRITE-DETAIL-LINE.

           MOVE "BEG CLASS - END CLASS" TO RANGE-SELECTION.
           MOVE BEG-CL TO RANGE1N
           MOVE END-CL TO RANGE2N
           PERFORM WRITE-DETAIL-LINE.

           MOVE "TODAYS DATE" TO RANGE-SELECTION.
           MOVE TODAYS-DATE   TO RANGE1.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "REPORT ?"    TO RANGE-SELECTION.
           MOVE REPORT-FLAG   TO RANGE1.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "TOTAL MISPAYS SET" TO RANGE-SELECTION.
           MOVE TOTAL-MISPAY   TO RANGE1N.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/ARRAYSP.CPY".
       COPY "LIBGB/SPPDTH.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBLP/LPLSMCL.CPY".
       COPY "LIBGB/GPENV.CPY".


      *********************************************************
       FORM-RESET SECTION.
      *********************************************************
           MOVE EXT-CONAME-CONAME TO MISPAY-CONAME.
           MOVE EXT-CONAME-SYSDATE TO MISPAY-SYSDATE.
           DISPLAY MISPAY-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE MISPAY-FORM-FIELDS TO WR-BUF.
           MOVE MISPAY-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *********************************************************
       SETUP-LINK-INFO SECTION.
      *********************************************************
           MOVE MISPAY-HELP--FILE TO HELP-LINK-FILE.
           MOVE MISPAY-HELP--DIR  TO HELP-LINK-DIR.

      *********************************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/MISPAY_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *********************************************************
       MISC-ROUTINES SECTION.
      *********************************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/MKTEMP.CPY".

      *********************************
       IO-ROUTINE SECTION.
      *********************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLSMGS_SQL.CPY".
       COPY "LIBLP/LPLTPGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".

      *COPY "LIBLP/LPLN3RN.CPY".
      * COPYMEMBER: LIBLP/LPLN3RN
      *-----------------------------------------------------------------
      *    STILL NEED TO LOAD THE FILE PATH WORKERS
      *-----------------------------------------------------------------
       LOAD-LN3-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( LN-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( LN-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO LN-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO LN-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO LN-ALL-DATA
              MOVE LN-ALL-PATH             TO LN-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( LN-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO LN-ALL-BASE
              MOVE FILPATH-MACHINE         TO LN-ALL-MACHINE
              MOVE FILPATH-DATA            TO LN-ALL-DATA
              MOVE LN-ALL-PATH             TO LN-PATH.

      *-----------------------------------------------------------------
       OPEN-LN3-FILE.
           PERFORM LOAD-LN3-FILE.
           PERFORM OPEN-IT.
           MOVE LN-PATH-SQL TO E-FILE.

           IF LN3-CONN-OPEN-SW = "N" 
              MOVE "Y"         TO LN3-CONN-OPEN-SW
              EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS LN3_CONNECT 
                       USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
              END-EXEC.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       START-LN3-FILE. *> SSP - LNFILE
           PERFORM START-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-PATH-SQL    TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING LN-OWNBR, LN-CLASS,LN-ACCTNO
              DELIMITED BY " " INTO E-KEYX.

           IF ( LN3-CURSOR-STAT-GOOD )
              PERFORM CLOSE-LN3-FILE.

           IF ( QLN3-WEND-ACCTNO = ZEROES    ) OR
              ( QLN3-WEND-ACCTNO NOT NUMERIC )
              MOVE LN-OWNBR             TO QLN3-WBEG-OWNBR
              MOVE LN-OWNBR             TO QLN3-WEND-OWNBR
              MOVE LN-CLASS             TO QLN3-WBEG-CLASS
              MOVE ALL "9"              TO QLN3-WEND-CLASS
              MOVE LN-ACCTNO            TO QLN3-WBEG-ACCTNO
              MOVE ALL "9"              TO QLN3-WEND-ACCTNO.
    

      *  STORED PROCEDURE:
      *
      *   SELECT LNFILE.LN_OWNBR,
      *          LNFILE.LN_ACCTNO,
      *          LNFILE.LN_CLASS,
      *          LNFILE.LN_SUBCLASS,
      *          LNFILE.LN_LAWCODE,
      *          LNFILE.LN_ORGST,
      *          LNFILE.LN_SPRCLASS,
      *          LNFILE.LN_CONVERCD, 
      *          LNFILE.LN_POCD, 
      *          LNFILE.LN_PAY_SCHLD_FG, 
      *          CAST(LNFILE.LN_ENTDATE    AS VARCHAR(10)), 
      *          CAST(LNFILE.LN_LOANDATE   AS VARCHAR(10)), 
      *          CAST(LNFILE.LN_1STPYDATE  AS VARCHAR(10)), 
      *          CAST(LNFILE.LN_POFFDATE   AS VARCHAR(10)), 
      *          LNFILE.LN_1STPYAMT,
      *          LNFILE.LN_NOREGPY,
      *          LNFILE.LN_REGPYAMT,
      *          LNFILE.LN_LASTPYAMT,
      *          LNFILE.LN_TOTPAYMNTD,
      *          LNFILE.LN_TOTNODEF,
      *          LNFILE.LN_TOTNOALDEL,
      *          LNFILE.LN_UNITPER_CD, 
      *          LNFILE.LN_UNITPER_FREQ
      *
      *     FROM DBO.LNFILE  LNFILE
      *
      *    WHERE (( LNFILE.LN_OWNBR         = @QLN3_WBEG_OWNBR
      *      AND    LNFILE.LN_CLASS   BETWEEN @QLN3_WBEG_CLASS
      *                                   AND @QLN3_WEND_CLASS  ) 
      *      AND  ( LNFILE.LN_POCD         != '**' )
      *      AND  ( LNFILE.LN_POFFDATE     != '1900-01-01' )
      *      AND  ( LNFILE.LN_CLASS        != 50 )
      *      AND  ( LNFILE.LN_ORGST         = 'IL' )
      *      AND  ((LNFILE.LN_CONVERCD      = 'Y'
      *      AND    LNFILE.LN_ENTDATE       > '2022-07-31' )
      *       OR   (LNFILE.LN_CONVERCD     != 'Y'
      *      AND    LNFILE.LN_LOANDATE      > '2022-07-31' )))
      *
      *    ORDER BY LNFILE.LN_OWNBR,
      *             LNFILE.LN_CLASS,
      *             LNFILE.LN_ACCTNO; 


           EXEC SQL SET CONNECTION LN3_CONNECT END-EXEC.

           EXEC SQL
            DECLARE LN3_CURSOR CURSOR FOR
                :LN3-RETURN-CODE = EXEC SSP_MISPAY_LNFILE_SELECT 
              ( :QLN3-WBEG-OWNBR, 
                :QLN3-WBEG-CLASS, 
                :QLN3-WEND-CLASS )
           END-EXEC.
 
           EXEC SQL
               OPEN LN3_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

      *    MOVE ZEROES TO QLN3-WBEG-OWNBR
      *                   QLN3-WEND-OWNBR
      *                   QLN3-WBEG-CLASS
      *                   QLN3-WEND-CLASS
      *                   QLN3-WBEG-ACCTNO
      *                   QLN3-WEND-ACCTNO.

           MOVE STAT---GOOD TO LN3-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC. 

      *-----------------------------------------------------------------
       READ-LN3-FILE-NEXT. *> EMBEDDED - LNFILE
           PERFORM READ-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-PATH-SQL    TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING LN-OWNBR, LN-CLASS,LN-ACCTNO
              DELIMITED BY " " INTO E-KEYX.
           INITIALIZE QLN-REC.
    
           EXEC SQL SET CONNECTION LN3_CONNECT END-EXEC.

              EXEC SQL 
                   FETCH NEXT LN3_CURSOR INTO
                     :QLN-OWNBR,
                     :QLN-ACCTNO,
                     :QLN-CLASS,
                     :QLN-SUBCLASS,
                     :QLN-LAWCODE,
                     :QLN-ORGST,
                     :QLN-SPRCLASS,
                     :QLN-CONVERCD, 
                     :QLN-POCD, 
                     :QLN-PAY-SCHLD-FG, 
                     :QLN-ENTDATE, 
                     :QLN-LOANDATE,  
                     :QLN-1STPYDATE, 
                     :QLN-POFFDATE, 
                     :QLN-1STPYAMT,
                     :QLN-NOREGPY,
                     :QLN-REGPYAMT,
                     :QLN-LASTPYAMT,
                     :QLN-TOTPAYMNTD,
                     :QLN-TOTNODEF,
                     :QLN-TOTNOALDEL,
                     :QLN-UNITPER-CD, 
                     :QLN-UNITPER-FREQ
              END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
               GO TO READ-LN3-FILE-NEXT.

           IF ( IO-FG = 0 )
               PERFORM GET-LN-FIELDS.
 
          EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       CLOSE-LN3-FILE.

           PERFORM CLOSE-IT.
           IF ( LN3-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION LN3_CONNECT END-EXEC
              EXEC SQL
                   CLOSE LN3_CURSOR
              END-EXEC
              MOVE STAT---BAD TO LN3-CURSOR-STAT
              EXEC SQL SET CONNECTION C1 END-EXEC.
  
      *-----------------------------------------------------------------
       CLOSE-LN3-CONNECTION.

           IF LN3-CONN-OPEN-SW = "Y"
              EXEC SQL SET CONNECTION LN3_CONNECT END-EXEC
              EXEC SQL COMMIT END-EXEC
              EXEC SQL DISCONNECT LN3_CONNECT END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE "N" TO LN3-CONN-OPEN-SW.

      *-----------------------------------------------------------------

      *COPY "LIBLP/LPLSM1IN.CPY".
      * COPYMEMBER: LIBLP/LPLSM1IN
      * 20220923 BAH ADDED LSM-MISPAY-90-DATE #1570
      * 20240116 BAH ADDED LSM-SECOND-BEN INFO #1637
       LOAD-LSM1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( LSM-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( LSM-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO LSM-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO LSM-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO LSM-ALL-DATA
              MOVE LSM-ALL-PATH             TO LSM-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( LSM-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO LSM-ALL-BASE
              MOVE FILPATH-MACHINE         TO LSM-ALL-MACHINE
              MOVE FILPATH-DATA            TO LSM-ALL-DATA
              MOVE LSM-ALL-PATH             TO LSM-PATH.

      *-----------------------------------------------------------------
       OPEN-LSM1-FILE.
           PERFORM LOAD-LSM1-FILE.
           PERFORM OPEN-IT.
           MOVE LSM-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       READ-LSM1-FILE. *> EMBEDDED - LSMFILE
           PERFORM READ-IT.
           MOVE LSM-PATH-OWNBR  TO LSM-BRNO. 
           MOVE LSM-PATH-SQL    TO E-FILE.
           MOVE LSM1-KEY        TO E-KEYX.
           INITIALIZE QLSM-REC.

           MOVE LSM-BRNO   TO QLSM-BRNO.
           MOVE LSM-ACCTNO TO QLSM-ACCTNO.
           MOVE LSM-SEQNO TO QLSM-SEQNO.

           EXEC SQL
            SELECT LSMFILE.LSM_BRNO,
                   LSMFILE.LSM_ACCTNO,
                   LSMFILE.LSM_SEQNO,
                   CAST(LSMFILE.LSM_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   LSMFILE.LSM_MISC_FIELD_1,
                   LSMFILE.LSM_MISC_FIELD_2,
                   LSMFILE.LSM_MISC_FIELD_3,
                   LSMFILE.LSM_MISC_FIELD_4,
                   LSMFILE.LSM_MISC_FIELD_5,
                   LSMFILE.LSM_MISC_FIELD_6,
                   LSMFILE.LSM_MISC_FIELD_7,
                   LSMFILE.LSM_MISC_FIELD_8,
                   LSMFILE.LSM_MISC_FIELD_9,
                   LSMFILE.LSM_MISC_FIELD_10,
                   LSMFILE.LSM_MISC_FIELD_11,
                   LSMFILE.LSM_MISC_FIELD_12,
                   LSMFILE.LSM_MISC_FIELD_13,
                   LSMFILE.LSM_MISC_FIELD_14,
                   LSMFILE.LSM_MISC_FIELD_15,
                   LSMFILE.LSM_MISC_FIELD_16,
                   LSMFILE.LSM_MISC_FIELD_17,
                   LSMFILE.LSM_MISC_FIELD_18,
                   LSMFILE.LSM_MISC_FIELD_19,
                   LSMFILE.LSM_MISC_FIELD_20,
                   LSMFILE.LSM_MISC_FIELD_21,
                   LSMFILE.LSM_MISC_FIELD_22,
                   LSMFILE.LSM_MISC_FIELD_23,
                   LSMFILE.LSM_MISC_FIELD_24,
                   LSMFILE.LSM_MISC_FIELD_25,
                   LSMFILE.LSM_MISC_FIELD_26,
                   LSMFILE.LSM_MISC_FIELD_27,
                   LSMFILE.LSM_MISC_FIELD_28,
                   LSMFILE.LSM_MISC_FIELD_29,
                   LSMFILE.LSM_MISC_FIELD_30,
                   LSMFILE.LSM_MISC_FIELD_31,
                   LSMFILE.LSM_MISC_FIELD_32,
                   LSMFILE.LSM_TRANS_FROM_BRNO,
                   LSMFILE.LSM_TRANS_FROM_ACCTNO,
                   CAST(LSMFILE.LSM_TRANS_FROM_DATE
                                       AS VARCHAR(10)),
                   CAST(LSMFILE.LSM_MISPAY_90_DATE
                                       AS VARCHAR(10)),
                   LSMFILE.LSM_SECOND_BEN_NAME,
                   LSMFILE.LSM_SECOND_BEN_ADR,
                   LSMFILE.LSM_SECOND_BEN_CITY,
                   LSMFILE.LSM_SECOND_BEN_ST,
                   LSMFILE.LSM_SECOND_BEN_ZIP,
                   LSMFILE.LSM_SECOND_BEN_PHONE,
                   LSMFILE.LSM_SECOND_BEN_RELATION
            INTO :QLSM-REC
            FROM DBO.LSMFILE
            WHERE LSMFILE.LSM_BRNO = :QLSM-BRNO
              AND LSMFILE.LSM_ACCTNO = :QLSM-ACCTNO
              AND LSMFILE.LSM_SEQNO = :QLSM-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LSM1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-LSM-FIELDS.

      *-----------------------------------------------------------------
       WRITE-LSM1-FILE. *> EMBEDDED - LSMFILE
XXX   *
XXX   *    MOVE LSM-PATH-OWNBR  TO LSM-BRNO.
XXX   *    ACCEPT LSM-LTOUCH-DATE FROM CENTURY-DATE.
XXX   *    PERFORM WRITE-IT.
XXX   *    MOVE LSM-PATH-SQL    TO E-FILE.
XXX   *    MOVE LSM1-KEY        TO E-KEYX.
XXX   *    PERFORM SET-LSM-FIELDS.
XXX   *
XXX   *    MOVE '2' TO SQL-UPDATE-CONN-USE-SW. *> USE UPDATE_CONNECT
XXX   *    PERFORM SQL-SWAP-UPDATE-CONNECTION.
XXX   *
XXX   *    EXEC SQL
XXX   *     INSERT INTO DBO.LSMFILE
XXX   *      (LSMFILE.LSM_BRNO,
XXX   *       LSMFILE.LSM_ACCTNO,
XXX   *       LSMFILE.LSM_SEQNO,
XXX   *       LSMFILE.LSM_LTOUCH_DATE,
XXX   *       LSMFILE.LSM_MISC_FIELD_1,
XXX   *       LSMFILE.LSM_MISC_FIELD_2,
XXX   *       LSMFILE.LSM_MISC_FIELD_3,
XXX   *       LSMFILE.LSM_MISC_FIELD_4,
XXX   *       LSMFILE.LSM_MISC_FIELD_5,
XXX   *       LSMFILE.LSM_MISC_FIELD_6,
XXX   *       LSMFILE.LSM_MISC_FIELD_7,
XXX   *       LSMFILE.LSM_MISC_FIELD_8,
XXX   *       LSMFILE.LSM_MISC_FIELD_9,
XXX   *       LSMFILE.LSM_MISC_FIELD_10,
XXX   *       LSMFILE.LSM_MISC_FIELD_11,
XXX   *       LSMFILE.LSM_MISC_FIELD_12,
XXX   *       LSMFILE.LSM_MISC_FIELD_13,
XXX   *       LSMFILE.LSM_MISC_FIELD_14,
XXX   *       LSMFILE.LSM_MISC_FIELD_15,
XXX   *       LSMFILE.LSM_MISC_FIELD_16,
XXX   *       LSMFILE.LSM_MISC_FIELD_17,
XXX   *       LSMFILE.LSM_MISC_FIELD_18,
XXX   *       LSMFILE.LSM_MISC_FIELD_19,
XXX   *       LSMFILE.LSM_MISC_FIELD_20,
XXX   *       LSMFILE.LSM_MISC_FIELD_21,
XXX   *       LSMFILE.LSM_MISC_FIELD_22,
XXX   *       LSMFILE.LSM_MISC_FIELD_23,
XXX   *       LSMFILE.LSM_MISC_FIELD_24,
XXX   *       LSMFILE.LSM_MISC_FIELD_25,
XXX   *       LSMFILE.LSM_MISC_FIELD_26,
XXX   *       LSMFILE.LSM_MISC_FIELD_27,
XXX   *       LSMFILE.LSM_MISC_FIELD_28,
XXX   *       LSMFILE.LSM_MISC_FIELD_29,
XXX   *       LSMFILE.LSM_MISC_FIELD_30,
XXX   *       LSMFILE.LSM_MISC_FIELD_31,
XXX   *       LSMFILE.LSM_MISC_FIELD_32,
XXX   *       LSMFILE.LSM_TRANS_FROM_BRNO,
XXX   *       LSMFILE.LSM_TRANS_FROM_ACCTNO,
XXX   *       LSMFILE.LSM_TRANS_FROM_DATE,
XXX   *       LSMFILE.LSM_MISPAY_90_DATE,
XXX   *       LSMFILE.LSM_SECOND_BEN_NAME,
XXX   *       LSMFILE.LSM_SECOND_BEN_ADR,
XXX   *       LSMFILE.LSM_SECOND_BEN_CITY,
XXX   *       LSMFILE.LSM_SECOND_BEN_ST,
XXX   *       LSMFILE.LSM_SECOND_BEN_ZIP,
XXX   *       LSMFILE.LSM_SECOND_BEN_PHONE,
XXX   *       LSMFILE.LSM_SECOND_BEN_RELATION)
XXX   *     VALUES
XXX   *      (:QLSM-BRNO,
XXX   *       :QLSM-ACCTNO,
XXX   *       :QLSM-SEQNO,
XXX   *       :QLSM-LTOUCH-DATE,
XXX   *       :QLSM-MISC-FIELD-1,
XXX   *       :QLSM-MISC-FIELD-2,
XXX   *       :QLSM-MISC-FIELD-3,
XXX   *       :QLSM-MISC-FIELD-4,
XXX   *       :QLSM-MISC-FIELD-5,
XXX   *       :QLSM-MISC-FIELD-6,
XXX   *       :QLSM-MISC-FIELD-7,
XXX   *       :QLSM-MISC-FIELD-8,
XXX   *       :QLSM-MISC-FIELD-9,
XXX   *       :QLSM-MISC-FIELD-10,
XXX   *       :QLSM-MISC-FIELD-11,
XXX   *       :QLSM-MISC-FIELD-12,
XXX   *       :QLSM-MISC-FIELD-13,
XXX   *       :QLSM-MISC-FIELD-14,
XXX   *       :QLSM-MISC-FIELD-15,
XXX   *       :QLSM-MISC-FIELD-16,
XXX   *       :QLSM-MISC-FIELD-17,
XXX   *       :QLSM-MISC-FIELD-18,
XXX   *       :QLSM-MISC-FIELD-19,
XXX   *       :QLSM-MISC-FIELD-20,
XXX   *       :QLSM-MISC-FIELD-21,
XXX   *       :QLSM-MISC-FIELD-22,
XXX   *       :QLSM-MISC-FIELD-23,
XXX   *       :QLSM-MISC-FIELD-24,
XXX   *       :QLSM-MISC-FIELD-25,
XXX   *       :QLSM-MISC-FIELD-26,
XXX   *       :QLSM-MISC-FIELD-27,
XXX   *       :QLSM-MISC-FIELD-28,
XXX   *       :QLSM-MISC-FIELD-29,
XXX   *       :QLSM-MISC-FIELD-30,
XXX   *       :QLSM-MISC-FIELD-31,
XXX   *       :QLSM-MISC-FIELD-32,
XXX   *       :QLSM-TRANS-FROM-BRNO,
XXX   *       :QLSM-TRANS-FROM-ACCTNO,
XXX   *       :QLSM-TRANS-FROM-DATE,
XXX   *       :QLSM-MISPAY-90-DATE,
XXX   *       :QLSM-SECOND-BEN-NAME,
XXX   *       :QLSM-SECOND-BEN-ADR,
XXX   *       :QLSM-SECOND-BEN-CITY,
XXX   *       :QLSM-SECOND-BEN-STATE,
XXX   *       :QLSM-SECOND-BEN-ZIP,
XXX   *       :QLSM-SECOND-BEN-PHONE,
XXX   *       :QLSM-SECOND-BEN-RELATION)
XXX   *    END-EXEC.
XXX   *
XXX   *    PERFORM SQL-IO-VALIDATION.
XXX   *
XXX   *    IF ( IO-FG = 8 )     *> LOCKED
XXX   *       GO TO WRITE-LSM1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

           MOVE  '1' TO SQL-UPDATE-CONN-USE-SW. *> USE C1 CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

      *-----------------------------------------------------------------
       REWRITE-LSM1-FILE. *> EMBEDDED - LSMFILE

XXX   *    MOVE LSM-PATH-OWNBR  TO LSM-BRNO.
XXX   *    ACCEPT LSM-LTOUCH-DATE FROM CENTURY-DATE.
XXX   *    PERFORM REWRITE-IT.
XXX   *    MOVE LSM-PATH-SQL    TO E-FILE.
XXX   *    MOVE LSM1-KEY        TO E-KEYX.
XXX   *    PERFORM SET-LSM-FIELDS.
XXX   *
XXX   *    MOVE '2' TO SQL-UPDATE-CONN-USE-SW. *> USE UPDATE_CONNECT
XXX   *    PERFORM SQL-SWAP-UPDATE-CONNECTION.
XXX   *
XXX   *    EXEC SQL
XXX   *     UPDATE DBO.LSMFILE
XXX   *      SET
XXX   *       LSMFILE.LSM_BRNO = :QLSM-BRNO,
XXX   *       LSMFILE.LSM_ACCTNO = :QLSM-ACCTNO,
XXX   *       LSMFILE.LSM_SEQNO = :QLSM-SEQNO,
XXX   *       LSMFILE.LSM_LTOUCH_DATE = :QLSM-LTOUCH-DATE,
XXX   *       LSMFILE.LSM_MISC_FIELD_1 = :QLSM-MISC-FIELD-1,
XXX   *       LSMFILE.LSM_MISC_FIELD_2 = :QLSM-MISC-FIELD-2,
XXX   *       LSMFILE.LSM_MISC_FIELD_3 = :QLSM-MISC-FIELD-3,
XXX   *       LSMFILE.LSM_MISC_FIELD_4 = :QLSM-MISC-FIELD-4,
XXX   *       LSMFILE.LSM_MISC_FIELD_5 = :QLSM-MISC-FIELD-5,
XXX   *       LSMFILE.LSM_MISC_FIELD_6 = :QLSM-MISC-FIELD-6,
XXX   *       LSMFILE.LSM_MISC_FIELD_7 = :QLSM-MISC-FIELD-7,
XXX   *       LSMFILE.LSM_MISC_FIELD_8 = :QLSM-MISC-FIELD-8,
XXX   *       LSMFILE.LSM_MISC_FIELD_9 = :QLSM-MISC-FIELD-9,
XXX   *       LSMFILE.LSM_MISC_FIELD_10 = :QLSM-MISC-FIELD-10,
XXX   *       LSMFILE.LSM_MISC_FIELD_11 = :QLSM-MISC-FIELD-11,
XXX   *       LSMFILE.LSM_MISC_FIELD_12 = :QLSM-MISC-FIELD-12,
XXX   *       LSMFILE.LSM_MISC_FIELD_13 = :QLSM-MISC-FIELD-13,
XXX   *       LSMFILE.LSM_MISC_FIELD_14 = :QLSM-MISC-FIELD-14,
XXX   *       LSMFILE.LSM_MISC_FIELD_15 = :QLSM-MISC-FIELD-15,
XXX   *       LSMFILE.LSM_MISC_FIELD_16 = :QLSM-MISC-FIELD-16,
XXX   *       LSMFILE.LSM_MISC_FIELD_17 = :QLSM-MISC-FIELD-17,
XXX   *       LSMFILE.LSM_MISC_FIELD_18 = :QLSM-MISC-FIELD-18,
XXX   *       LSMFILE.LSM_MISC_FIELD_19 = :QLSM-MISC-FIELD-19,
XXX   *       LSMFILE.LSM_MISC_FIELD_20 = :QLSM-MISC-FIELD-20,
XXX   *       LSMFILE.LSM_MISC_FIELD_21 = :QLSM-MISC-FIELD-21,
XXX   *       LSMFILE.LSM_MISC_FIELD_22 = :QLSM-MISC-FIELD-22,
XXX   *       LSMFILE.LSM_MISC_FIELD_23 = :QLSM-MISC-FIELD-23,
XXX   *       LSMFILE.LSM_MISC_FIELD_24 = :QLSM-MISC-FIELD-24,
XXX   *       LSMFILE.LSM_MISC_FIELD_25 = :QLSM-MISC-FIELD-25,
XXX   *       LSMFILE.LSM_MISC_FIELD_26 = :QLSM-MISC-FIELD-26,
XXX   *       LSMFILE.LSM_MISC_FIELD_27 = :QLSM-MISC-FIELD-27,
XXX   *       LSMFILE.LSM_MISC_FIELD_28 = :QLSM-MISC-FIELD-28,
XXX   *       LSMFILE.LSM_MISC_FIELD_29 = :QLSM-MISC-FIELD-29,
XXX   *       LSMFILE.LSM_MISC_FIELD_30 = :QLSM-MISC-FIELD-30,
XXX   *       LSMFILE.LSM_MISC_FIELD_31 = :QLSM-MISC-FIELD-31,
XXX   *       LSMFILE.LSM_MISC_FIELD_32 = :QLSM-MISC-FIELD-32,
XXX   *       LSMFILE.LSM_TRANS_FROM_BRNO = :QLSM-TRANS-FROM-BRNO,
XXX   *       LSMFILE.LSM_TRANS_FROM_ACCTNO =
XXX   *                                   :QLSM-TRANS-FROM-ACCTNO,
XXX   *       LSMFILE.LSM_TRANS_FROM_DATE = :QLSM-TRANS-FROM-DATE,
XXX   *       LSMFILE.LSM_MISPAY_90_DATE = :QLSM-MISPAY-90-DATE,
XXX   *       LSMFILE.LSM_SECOND_BEN_NAME = :QLSM-SECOND-BEN-NAME,
XXX   *       LSMFILE.LSM_SECOND_BEN_ADR = :QLSM-SECOND-BEN-ADR,
XXX   *       LSMFILE.LSM_SECOND_BEN_CITY = :QLSM-SECOND-BEN-CITY,
XXX   *       LSMFILE.LSM_SECOND_BEN_ST = :QLSM-SECOND-BEN-STATE,
XXX   *       LSMFILE.LSM_SECOND_BEN_ZIP = :QLSM-SECOND-BEN-ZIP,
XXX   *       LSMFILE.LSM_SECOND_BEN_PHONE =
XXX   *                                    :QLSM-SECOND-BEN-PHONE,
XXX   *       LSMFILE.LSM_SECOND_BEN_RELATION =
XXX   *                                  :QLSM-SECOND-BEN-RELATION
XXX   *      WHERE LSMFILE.LSM_BRNO = :QLSM-BRNO
XXX   *        AND LSMFILE.LSM_ACCTNO = :QLSM-ACCTNO
XXX   *        AND LSMFILE.LSM_SEQNO = :QLSM-SEQNO
XXX   *    END-EXEC.
XXX   *
XXX   *    PERFORM SQL-IO-VALIDATION.
XXX   *
XXX   *    IF ( IO-FG = 8 )     *> LOCKED
XXX   *       GO TO REWRITE-LSM1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

           MOVE  '1' TO SQL-UPDATE-CONN-USE-SW. *> USE C1 CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

      *-----------------------------------------------------------------
       CLOSE-LSM1-FILE.

           PERFORM CLOSE-IT.

      *    IF ( LSM1-CURSOR-STAT-GOOD )
      *       EXEC SQL
      *            CLOSE LSM1_CURSOR
      *       END-EXEC
      *       MOVE STAT---BAD TO LSM1-CURSOR-STAT.

      *-----------------------------------------------------------------

       COPY "LIBLP/LPLTP1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

       END-PROGRAM SECTION.
       END-RUN.
           PERFORM CLOSE-FILES.
           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.
       EXIT-PROG.
           IF EXT-ROUTE-BUF = "00"
              MOVE "SP/EXTRMU" TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY MISPAY-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
