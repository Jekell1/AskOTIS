      *  A30 CONVERSION COMPLETED
       IDENTIFICATION DIVISION.
       PROGRAM-ID.      SRBAC5.
      *****************************************************
      *
      *    CREATES PIPE DELIMITED /USR/EXTRACT/             
      *  REV:   RBACT ACTUARIAL                           
      *  CS TX/01/98/01 REBATE IN COMMON      TAKEN FROM SRBAC5
      *****************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".

           SELECT WK-RBACT-FILE ASSIGN OUTPUT WK-REBATE-PATH
                  ORGANIZATION LINE SEQUENTIAL
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  FILE STATUS FILE-STAT.


       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".

      **************************************************
      *   LOAN ASCII FILE    RSZ =  (INCLUDES 0D0A)
      **************************************************
       FD  WK-RBACT-FILE
           RECORD IS VARYING IN SIZE FROM 5 TO 2000 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  WK-RBACT-REC.
           03  WK-RBACT-BYTE        PIC X OCCURS 2000 TIMES.


       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)UT/SEBAC5 S35 03/31/23-09:23:47 >".

      ****************************
      *     F I L E   S P E C S  *
      ****************************
       COPY "LIBGB/GB01GB.CPY".
       COPY "LIBGB/GB01GB_SQL.CPY".
       COPY "LIBGB/GBWSGB.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01LTP.CPY".
       COPY "LIBLP/LP01LTP_SQL.CPY".
       COPY "LIBLP/LPWSLTP.CPY".
       COPY "LIBLP/LP01LTI.CPY".
       COPY "LIBLP/LP01LTI_SQL.CPY".
       COPY "LIBLP/LPWSLTI.CPY".


       01  WK-RBACT-PATH.
           03  WK-RBACT-PATH1                  PIC X(12) 
                VALUE "/USR/EXTRACT".
           03  WK-RBACT-FILENAME               PIC X(8)  
                 VALUE "/RBACT.".                    

           03  WK-RBACT-MACHINE                PIC X(2).
           03  FILLER                    PIC X VALUE ".". 
           03  WK-RBACT-PATH-YR                PIC 99.
           03  WK-RBACT-PATH-MO                PIC 99.
           03  WK-RBACT-PATH-DA                PIC 99.

       01  WS-RBACT-REC.
           03  WS-RBACT-LN-BRACCT                PIC Z(14).
           03  WS-RBACT-LN-ACCTNO                PIC Z(10).
           03  WS-RBACT-LN-OWNBR                 PIC Z(6).
           03  WS-RBACT-SP1-KEY                  PIC X(9).

           03  WS-RBACT-ORIG-1STPAY              PIC 99/99/9999.
           03  WS-RBACT-INTDATE                  PIC 99/99/9999.
           03  WS-RBACT-RATE                     PIC 9(3).9(3).
           03  WS-RBACT-TOTNODEF                 PIC 9(3)-.
           03  WS-RBACT-ORGTERM                  PIC 9(5)-.
           03  WS-RBACT-1STPYAMT                 PIC 9(6).99.
           03  WS-RBACT-REGPYAMT                 PIC 9(6).99.
           03  WS-RBACT-LASTPYAMT                PIC 9(6).99-.
           03  WS-RBACT-UNITPER-CD               PIC X.
           03  WS-RBACT-UNITPER-FREQ             PIC 999.
           03  WS-RBACT-TOTCHG                   PIC 9(6).99-.
           03  WS-RBACT-RBDATE                   PIC 99/99/9999.
           03  WS-RBACT-ELAPSED-MONTHS           PIC 9(5)-.
           03  WS-RBACT-ELAPSED-REM              PIC 9(5).9999999999-.
           03  WS-RBACT-ELAPSED-DAYS             PIC 9(5)-.
           03  WS-RBACT-SKIPDEF-FG               PIC X.
           03  WS-RBACT-EARN-FULL-MONTH          PIC X.
           03  WS-RBACT-PENALTY-FG       PIC X.
           03  WS-RBACT-REB-SUB                  PIC 99.
           03  WS-RBACT-REB-SUB2                 PIC 99.                      
           03  WS-RBACT-MESS                     PIC X(6).
           03  WS-RBACT-RBACTF-ADJ               PIC 999.9(15)-.
           03  WS-RBACT-RBACTF                   PIC 9V9(15)-.
           03  WS-RBACT-RBACTT                   PIC 999-.
           03  WS-RBACT-RBACTI                   PIC 999.9(15)-.
           03  WS-RBACT-RBACTADJ                 PIC 999.9(15)-. 
           03  WS-RBACT-RBACTRP-A--N             PIC 999.9(15)-.
           03  WS-RBACT-RBACTRP-N                PIC 999-.
           03  WS-RBACT-RBACTLP                  PIC 999999.99-.
           03  WS-RBACT-RBACTLP-N                PIC 999-.
           03  WS-RBACT-RBACTFP                  PIC 999999.99-.
           03  WS-RBACT-RBACTLP-FAC              PIC 999.9(15)-.
           03  WS-RBACT-RBACTPV                  PIC 9999999.99-.
           03  WS-RBACT-RBACT-PRES-VALUE         PIC 9999999.99-.
           03  WS-RBACT-RBACT-REBATE            PIC 9999999.99-.

       01  WK-RBACT-IFN        PIC X(5)     VALUE "RBACT".


       01  HOLD-SP1-KEY                PIC X(9)     VALUE SPACES.
       01  FILLER            REDEFINES HOLD-SP1-KEY.
           03  HOLD-SP-ORGST           PIC X(2).
           03  HOLD-SP-SPRCLASS        PIC 9(3).
           03  HOLD-SP-SUBCLASS        PIC 9(2).
           03  HOLD-SP-LAWCODE         PIC 9(2).

       01  HOLD-FIL-PATH.
           03  FILLER           PIC X(9).
           03  HOLD-FIL-MACHINE PIC X(2).
           03  FILLER           PIC X(7). 

       01  HOLD-PDTH-E-OPTION   PIC X.
       01  HOLD-NOCAP-FG        PIC X.
       01  HOLD-DATE-CAP        PIC X.
       01  HOLD-INCDEF-FG       PIC X.
       01  HOLD-INCALDEL-FG     PIC X.
       01  BW-SOLICIT           PIC X.
       01  HOLD-BR-LOAN-NUM-FLAG PIC X.

       01  SUB                  PIC 99.
       01  JUSTIFY-TYPE     PIC X VALUE "L".

       01  TODAYS-DATE      PIC 9(8).
 
       01  WS-BRACCT.
           03  WS-OWNBR     PIC Z(6).
           03  WS-NUMBER    PIC 9(6).
       01  WS-LN-ACCTNO  PIC 9(8).
       01  WS-LN-OWNBR   PIC 9(4).

       01  CR-1          PIC X VALUE X"0D".
      **********************
      *   LINE WORKERS     *
      **********************
       01  LN-WORKERS.
           03  LN-FEED          PIC 99   COMP VALUE 0.
           03  LN-COUNT         PIC 999  COMP VALUE 99.
           03  LN-WK            PIC 999  COMP VALUE 0.
       01  PAGE-NUMBER          PIC 9(4) COMP VALUE 0.

      **********************
      *   MISC WORKERS     *
      **********************
       01  FIRST-TIME           PIC X        VALUE "Y".
 
       01  RECORDS-FOUND        PIC X        VALUE " ".
           88  NO-RECORDS-FOUND              VALUE "N".

       01  MESS-LIST            PIC X(5) VALUE "MESS.".
       01  DISPLAY-BUF.
           03  FILLER           PIC X(12) VALUE "PROCESSING..".
           03  DISPLAY-BRNO     PIC Z(4).
           03  FILLER           PIC X(7).
           03  FILLER           PIC X(17) VALUE SPACES.

       01  TOTAL-RECORDS        PIC S9(13)  COMP-3 VALUE 0.
       01  TOTAL-BYPASSED       PIC S9(13)  COMP-3 VALUE 0.
       01  TOTAL-INCLUDED       PIC S9(13)  COMP-3 VALUE 0.
    

      ***********************************
      *     S C R E E N   F I E L D S   *
      ***********************************
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  BEG-BR          PIC 9999.
           03  END-BR          PIC 9999.
           03  BEG-CLASS       PIC 999.
           03  END-CLASS       PIC 999.
           03  BEG-DATE        PIC 9(8).
           03  END-DATE        PIC 9(8).
           03  BEG-ACCTNO      PIC 9(6).
           03  END-ACCTNO      PIC 9(6).

           03  GROUP-FLAG      PIC X.
           03  ENT-GROUP       PIC X(4).

       01  VERSION-CONTROL     PIC X VALUE "B".

       01  SV-BEG-BR           PIC 9(4)   VALUE 9999.
       01  SV-END-BR           PIC 9(4)   VALUE 9999.

      ***********************************
      *     P R I N T   B U F F E R S   *
      ***********************************
       01  HEAD1.
           03  H-DATE           PIC X(8)     VALUE " ".
           03  FILLER           PIC XX       VALUE " ".
           03  H-HR             PIC 99       VALUE 0.
           03  FILLER           PIC X        VALUE ":".
           03  H-MIN            PIC 99       VALUE 0.
           03  FILLER           PIC X(8)    VALUE " ".
           03  H-NAME           PIC X(40)    VALUE " ".
           03  FILLER           PIC X(8)    VALUE " ".
           03  FILLER           PIC X(5)     VALUE "PAGE:".
           03  H-PAGE-NO        PIC ZZZ9.

       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(12).
           03  FILLER           PIC X(13)    VALUE " ".

       01  HEAD3.
           03  FILLER           PIC X(9)    VALUE "BRANCH NO".
           03  FILLER           PIC X(3)    VALUE SPACES.
           03  FILLER           PIC X(13)   VALUE "TOTAL RECORDS".
           03  FILLER           PIC X(3)    VALUE SPACES.
           03  FILLER           PIC X(12)   VALUE "TOTAL LN AMT".
           03  FILLER           PIC X(3)    VALUE SPACES.
           03  FILLER           PIC X(11)   VALUE "TOTAL FILES".


       01  DETAIL-LINE          PIC X(80) VALUE SPACES.
       01  DETAIL-LINE1 REDEFINES DETAIL-LINE.
           03  D-BRNO           PIC Z(4).
           03  FILLER           PIC X(2).
           03  D-NUMBER         PIC Z(6).
           03  FILLER           PIC X(2).
           03  D-NAME           PIC X(15).
           03  FILLER           PIC X(2).
           03  D-REPODATE       PIC 99/99/99.
           03  FILLER           PIC X(5).
           03  D-CURBAL         PIC Z(6).99.

       01  DETAIL-LINE2 REDEFINES DETAIL-LINE.
           03  D-TOTAL-BRNO     PIC Z(4).
           03  FILLER           PIC X(10).
           03  D-TOTAL-REC      PIC Z(7).
           03  FILLER           PIC X(20).
           03  D-TOTAL-AMT      PIC Z(9).99.

       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03  RANGE-SELECTION  PIC X(30).
           03  FILLER           PIC X(5).
           03  RANGE1           PIC X(10).
           03  RANGE1N REDEFINES RANGE1 PIC 9(10).
           03  RANGE1D REDEFINES RANGE1 PIC ZZ99/99/99.
           03  FILLER           PIC X(2).
           03  RANGE2           PIC X(10).
           03  RANGE2N REDEFINES RANGE2 PIC 9(10).
           03  RANGE2D REDEFINES RANGE2 PIC ZZ99/99/99.

       COPY "LIBWI/PFSWINDOWSW.CPY".
       COPY "LIBWI/TSTINTW.CPY".
       COPY "LIBWI/GRSCANW.CPY".

       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".

       COPY "LIBGB/AGEINGW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/REBATEW.CPY".
       COPY "LIBGB/RBACTW.CPY".
       COPY "LIBGB/SPPDTHW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBLP/LPAPRSW.CPY".
       COPY "LIBLP/LPAPRZW.CPY".
       COPY "LIBLP/SETCTBLW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBLP/LPUPERW.CPY".
       COPY "LIBLP/LPUPWKW.CPY".
       COPY "LIBLP/LPXDTEW.CPY".
       COPY "LIBLP/LPITRMW.CPY".
       COPY "LIBLP/LPAMTSW.CPY".
       COPY "LIBLP/LPLCHGW.CPY".
       COPY "LIBLP/LPCEPPW.CPY". 
       COPY "LIBLP/LPIBPCW.CPY".
       COPY "LIBLP/LPCDELW.CPY".
       COPY "LIBLP/LPLCASW.CPY".
       COPY "LIBLP/LPLCAPW.CPY".
       COPY "LIBLP/LPRATEW.CPY".
       COPY "LIBLP/LPPDUEW.CPY".
       COPY "LIBLP/LPSCHDW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/TESTR2_DEF.CPY".
       COPY "LIBSP/TESTR2_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *****************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      *****************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBSP/TESTR2_SCN.CPY".

      *----------------------------------------------------------------
       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE WK-RBACT-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LTP1-FILE.
           PERFORM CLOSE-LTI1-FILE.
           PERFORM CLOSE-LN3-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-GB-FILE.
           PERFORM CLOSE-BR-FILE.
           CLOSE
               PR-FILE WK-RBACT-FILE.

       COPY "LIBGB/DECLRP2.CPY".

      *************************************
       MAIN-MODULE SECTION.
           PERFORM SQL-CONNECT.
      *************************************
      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "TESTR2" TO VDU-FORM-NAME.

           PERFORM GET-GPENV.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

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
           PERFORM NPR-TRAP-INTERRUPT.

           PERFORM READ-BR-FILE-NEXT.
           IF IO-FG = 9 OR BR-NO > END-BR
              GO TO END-ROUTINE.
       MOVE-BRANCH-PATH-INFO.
           MOVE EXT-FILPATH-BASE TO FILPATH-BASE.
           MOVE BR-MACHINE       TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH     TO FILPATH-DATA.
           MOVE "B"        TO LN-PATH-OVERRIDE
                              GB-PATH-OVERRIDE
                             LTI-PATH-OVERRIDE
                             LTP-PATH-OVERRIDE.

      * NEED FOR LCAS ROUTINES (REPLACES GLOBRD)
           PERFORM OPEN-GB-FILE.
           MOVE GB-PATH-OWNBR TO GB-BRNO.
           MOVE 1             TO GB-NO.
           PERFORM READ-GB-FILE.
           PERFORM CLOSE-GB-FILE.
           IF IO-BAD
              GO TO END-OF-GROUP.

           PERFORM LOAD-LN3-FILE.
           PERFORM LOAD-LTP1-FILE.
           PERFORM LOAD-LTI1-FILE.

           PERFORM OPEN-LN3-FILE.
           PERFORM OPEN-LTP1-FILE.
           PERFORM OPEN-LTI1-FILE.


           PERFORM GLOBAL-UPDATE.

           PERFORM CLOSE-LN3-FILE.

       END-OF-GROUP.
           GO TO NEXT-GROUP-LOC.

       END-ROUTINE.

           PERFORM PRINT-RANGE-LINES.
           GO TO END-PROGRAM.

      **************************
       GLOBAL-UPDATE SECTION.
      **************************
           IF EXT-ROUTE-BUF = "00"
              IF PRU-LP NOT = "VDU" AND NOT = "PRU"
                 MOVE BR-NO TO DISPLAY-BRNO
                 MOVE DISPLAY-BUF TO WR-BUF
                 MOVE MESS-LIST   TO WR-LIST
                 PERFORM CALL-WRFORM.

           MOVE BR-NO TO LN-OWNBR.
           MOVE 0     TO LN-CLASS LN-ACCTNO.
           PERFORM START-LN3-FILE.
       NEXT-LN.
           PERFORM READ-LN3-FILE-NEXT.
*L2G       IF IO-BAD OR (LN-OWNBR NOT = LN-PATH-OWNBR)
              GO TO EXIT-GLOBAL-UPDATE.
           ADD 1 TO TOTAL-RECORDS.
           MOVE TODAYS-DATE TO IBPC-DATE.
           IF LN-ORGST NOT = "TX"
              GO TO NEXT-LN.

           IF LN-OWNBR > END-BR
              GO TO EXIT-GLOBAL-UPDATE.

           IF LN-CLASS < BEG-CLASS OR LN-CLASS > END-CLASS
              ADD 1 TO  TOTAL-BYPASSED
              GO TO NEXT-LN.

           IF LN-LOANDATE < BEG-DATE OR LN-LOANDATE > END-DATE
              ADD 1 TO  TOTAL-BYPASSED
              GO TO NEXT-LN.

           IF LN-ACCTNO < BEG-ACCTNO OR LN-ACCTNO > END-ACCTNO
              ADD 1 TO  TOTAL-BYPASSED
              GO TO NEXT-LN.
           
           IF LN-POCD-NONLOAN
              ADD 1 TO  TOTAL-BYPASSED
              GO TO NEXT-LN.

      * EXCLUDE ZERO BALANCES
           IF LN-CURBAL = 0 AND LN-INTBAL = 0 AND LN-LCBAL = 0
               AND LN-OTHBAL = 0
              ADD 1 TO  TOTAL-BYPASSED
              GO TO NEXT-LN.

      *CHECK FOR NO PAYOFF DATE ON PL.
           IF LN-PLDATE NOT = ZEROS
              IF LN-POFFDATE NOT = 0
                 ADD 1 TO TOTAL-BYPASSED
                 GO TO NEXT-LN.

      * EXCLUDE PL < 01/01/2010
           IF LN-PLDATE NOT = ZEROS
              IF LN-PLDATE NOT > 20100101
                 ADD 1 TO  TOTAL-BYPASSED
                 GO TO NEXT-LN.

%%         IF TODAYS-DATE < LN-LOANDATE 
              GO TO NEXT-LN.

       GET-SPR.
           MOVE LN-ORGST     TO SP-ORGST.
           MOVE LN-SPRCLASS  TO SP-SPRCLASS.
           MOVE LN-SUBCLASS  TO SP-SUBCLASS.
           MOVE LN-LAWCODE   TO SP-LAWCODE.
           PERFORM READ-SP1-FILE
           IF IO-FG NOT = 0
               ADD 1 TO TOTAL-BYPASSED
               GO TO NEXT-LN.
           MOVE SP1-KEY TO HOLD-SP1-KEY.           
           IF SP-CAL-RATETYPE(1) = "Z"
               GO TO NEXT-LN.
      *    IF NOT(SP1-KEY = "TX0019901" OR SP1-KEY = "TX0019801"
      *              OR SP1-KEY = "TX0010106" OR SP1-KEY = "TX0019701")
      *        GO TO NEXT-LN.
 
           PERFORM CREATE-RBACT-WORK.  

           GO TO NEXT-LN.

       EXIT-GLOBAL-UPDATE.
           EXIT.





      /**********************************
      *   I N I T I A L I Z A T I O N   *
      ***********************************
       INITIALIZATION SECTION.
           MOVE TESTR2-QUEUE-POS TO Q-POS.
           MOVE TESTR2-COPIES-POS TO C-POS.
           MOVE TESTR2-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-SP1-FILE.

           PERFORM OPEN-PR-FILE.
           PERFORM ENABLE-INTERRUPT.

           IF BEG-BR = 0
              IF END-BR = 0
                 MOVE BEG-BR TO SV-BEG-BR
                 MOVE END-BR TO SV-END-BR
                 MOVE EXT-CONAME-USER-LOC TO BEG-BR
                                             END-BR.

           MOVE 0 TO TOTAL-RECORDS.

           MOVE EXT-CONAME-USER-ID TO H-USERID.
           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD  TO TODAYS-DATE.

           MOVE "FIL" TO GETENV-BUF.
           PERFORM GETENV-CALL.
           MOVE GETENV-BUF TO  HOLD-FIL-PATH.
           IF STAT NOT = "00"
              GO TO IO-ERROR. 


           MOVE HOLD-FIL-MACHINE TO WK-RBACT-MACHINE.

           MOVE TODAYS-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY-DD TO WK-RBACT-PATH-DA.
           MOVE DATE-MMDDYY-MM TO WK-RBACT-PATH-MO.
           MOVE DATE-MMDDYY-YY TO WK-RBACT-PATH-YR.

           CALL "C$TOLOWER" USING WK-RBACT-PATH1,VALUE 12.
           PERFORM OPEN-WK-RBACT-FILE-OUTPUT.
      *-----------------------------------------------
           GO TO INITIALIZE-EXIT.

       INITIALIZE-ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.
       INITIALIZE-EXIT.
           EXIT.



      ****************************************
      *    SETUP DATES FOR BATCH PROCESS
      ****************************************
       EO-EXEC-AUDIT SECTION.
           MOVE EXT-EOBUF-DATE1 TO BEG-DATE.
           MOVE EXT-EOBUF-DATE2 TO END-DATE.

      ****************************************
      *       ENTER REPORT PARAMETERS
      ****************************************
       ENTER-PARAMETERS SECTION.
       ENTER-PARM.
      * FOR RE-ENTRY ON F4 FROM ACCEPT.

       BR-01.
           MOVE "E RA000BR1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-BR.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1<"
              MOVE GRSCAN-BUF TO SCAN-BUF
              PERFORM CALL-WINDOW-PROGRAM
              MOVE SCAN-BUF TO GRSCAN-BUF
              IF SCAN-STATUS NOT = "00"
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
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BR-02.
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
           MOVE "ENNN020CL1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-CL.
           IF VDUSTATUS = "1U" GO TO BR-02.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO CL-01.
           MOVE VDUNUM0 TO BEG-CLASS.
       CL-02.
           MOVE "ENNN020CL2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO CL-01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO CL-02.
           MOVE VDUNUM0 TO END-CLASS.
           IF BEG-CLASS > END-CLASS
              PERFORM INVALID-RANGE
              GO TO CL-01.
           GO TO BEGDATE-01.
       ALL-CL.
           MOVE "SNNN020CL1." TO VDU.
           MOVE 0 TO BEG-CLASS VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020CL2." TO VDU.
           MOVE 99 TO END-CLASS VDUNUM0.
           PERFORM SCREENIO.
       BEGDATE-01.
           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO EXIT-PARM.

           MOVE "ERNM060DATE1." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO CL-02.
           IF VDUSTATUS = "16" GO TO ALL-DATE.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BEGDATE-01.

           MOVE VDU-DATE-YYYYMMDD TO BEG-DATE.
      *-----------------------------------------------------------
       ENDDATE-02.

           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO EXIT-PARM.

           MOVE "ERNM060DATE2." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BEGDATE-01.
           IF NOT (VDUSTATUS = "00" OR "LD") GO TO ENDDATE-02.

           MOVE VDU-DATE-YYYYMMDD  TO END-DATE.

      * MAKE SURE THE DATE RANGE IS VALID.

           IF BEG-DATE NOT = 0
              MOVE BEG-DATE  TO NUM-DATE
              MOVE END-DATE TO SYS-DATE
              PERFORM CMPDAT
              IF SYS-IS-LT
                 MOVE "INVALID DATE RANGE" TO MESS
                 PERFORM SEND-MESS
                 GO TO BEGDATE-01.

           GO TO ENTER-BEG-ACCTNO.            

      *----------------------------------------------------------
       ALL-DATE.

           MOVE "S  8080DATE1." TO VDU.
           MOVE JULIAN-BEG TO BEG-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
           MOVE "S  8080DATE2."   TO VDU.
           MOVE JULIAN-END TO END-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

      *----------------------------------------------------------
       ENTER-BEG-ACCTNO.         
           MOVE "ENNN060ACCT1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "16" GO TO ENTER-ALL-ACCTNO 
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U"
              IF EXT-ROUTE-BUF = "00"
                 GO TO ENDDATE-02 
              ELSE
                 GO TO CL-02.
           MOVE VDUNUM0 TO BEG-ACCTNO.

      *----------------------------------------------------------
       ENTER-END-ACCTNO.
           MOVE "ENNN060ACCT2." TO VDU.
           PERFORM SCREENIO.
           
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U"
               GO TO ENTER-BEG-ACCTNO.
           MOVE VDUNUM0 TO END-ACCTNO.
           
           GO TO EXIT-PARM.
      *-----------------------------------------------------------
       ENTER-ALL-ACCTNO.
           MOVE "SNNN060ACCT1." TO VDU.
           MOVE 0 TO BEG-ACCTNO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN060ACCT2." TO VDU.
           MOVE 999999 TO END-ACCTNO VDUNUM0.
           PERFORM SCREENIO.          


      *============================================
       EXIT-PARM.
           EXIT.


      ****************************************
      *       DISPLAY REPORT PARAMETERS
      ****************************************
       DISPLAY-PARAMETERS SECTION.

           IF GROUP-FLAG = "Y"
              MOVE "S  A000BR1." TO VDU
              MOVE ENT-GROUP     TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A000BR2." TO VDU
              MOVE SPACES        TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "SNNN040BR1." TO VDU
              MOVE BEG-BR    TO VDUNUM0
              PERFORM SCREENIO
              MOVE "SNNN040BR2." TO VDU
              MOVE END-BR    TO VDUNUM0
              PERFORM SCREENIO.

           MOVE "SN N020CL1." TO VDU.
           MOVE BEG-CLASS TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SN N020CL2." TO VDU.
           MOVE END-CLASS TO VDUNUM0.
           PERFORM SCREENIO.
    
      ****************************
       CREATE-RBACT-WORK SECTION.
      ****************************
           MOVE SPACES TO WS-RBACT-REC WK-RBACT-REC.
           PERFORM SET-COMMON.
           MOVE WS-BRACCT TO WS-RBACT-LN-BRACCT.
           CALL "C$JUSTIFY" USING WS-RBACT-LN-BRACCT,JUSTIFY-TYPE.

           MOVE WS-LN-ACCTNO TO WS-RBACT-LN-ACCTNO.
           CALL "C$JUSTIFY" USING WS-RBACT-LN-ACCTNO, JUSTIFY-TYPE.

           MOVE WS-LN-OWNBR       TO WS-RBACT-LN-OWNBR.
           CALL "C$JUSTIFY" USING WS-RBACT-LN-OWNBR, JUSTIFY-TYPE.

           MOVE SP1-KEY TO WS-RBACT-SP1-KEY.
          
           MOVE 7   TO REB-SUB.
           MOVE "F" TO SP-RBFRMLA(REB-SUB).
           MOVE 0 TO REB-REBATE REB-PRES-VALUE.
           MOVE TODAYS-DATE TO REB-PAYDATE.
           MOVE LN-ORGTERM TO REB-ORGTERM.
           MOVE "RI" TO REB-LPTRCD.
           MOVE LN-INTCHG TO REB-TOTCHG.
           MOVE 0 TO SP-RBMINRET(7), REB-WORKER.
*NEW       MOVE 0 TO SP-RBNOMON(7)
                     SP-RBDEDCHG(7)
                     SP-RBSPOPT1(7)
                     SP-RBEARLY(7)
                     SP-RBREDUC(7).

      * ONLY CLEAR FOR INTEREST REBATE, OTHERWISE PAYOFF WILL WIPE IT OUT
      * WITH OTHER REBATES
           IF REB-SUB = 7
              MOVE 0 TO REB-MS-PENALTY.

           
           IF REB-ADDON-FG NOT = "Y"
              MOVE LN-ORIG-1STPYDATE TO REB-LN-ORIG-1STPYDATE
              MOVE LN-PURDATE     TO REB-LN-PURDATE
              MOVE LN-LOANDATE    TO REB-LN-LOANDATE
              MOVE LN-INTDATE     TO REB-LN-INTDATE
              MOVE LN-APRATE      TO REB-LN-APRATE
              MOVE LN-TOTNODEF    TO REB-LN-TOTNODEF.
              MOVE LN-SALEFIN     TO REB-LN-SALEFIN

              MOVE LN-SMPRATE     TO REB-LN-SMPRATE
              MOVE LN-ORGTERM     TO REB-LN-ORGTERM
              MOVE LN-1STPYAMT    TO REB-LN-1STPYAMT
              MOVE LN-REGPYAMT    TO REB-LN-REGPYAMT
              MOVE LN-LASTPYAMT   TO REB-LN-LASTPYAMT.

      *
      ***********   SET UNIT PERIOD INFO   **************************
            MOVE LN-UNITPER-CD      TO UPER-UNITPER-CD.
            MOVE LN-UNITPER-FREQ    TO UPER-UNITPER-FREQ.
            MOVE REB-LN-INTDATE     TO UPER-INTDATE.
            MOVE REB-LN-ORIG-1STPYDATE TO UPER-1STPYDATE.
            MOVE REB-ORGTERM        TO UPER-TERM.
            PERFORM UPER-CALCULATION.

            IF REB-SKIPDEF-FG NOT = "Y"
              IF SP-RBFRMLA(REB-SUB) = "D" OR "F"
                 ADD LN-EXTCHG LN-SERCHG TO REB-TOTCHG. 


           PERFORM SELECT-SP-ELAPSED-TIMTBL.
           IF JULIAN-DATE = 0
              GO TO CREATE-RBACT-WORK-EXIT.
      * GET RBACT REFERENCE DATE:
           PERFORM REBATE-REFDATE.
           IF REB-REFDATE = 0
               MOVE 0 TO REB-SUB
               GO TO CREATE-RBACT-WORK-EXIT.
      * CALCULATE ELAPSED TIME FROM REF THRU PAYDATE:
           PERFORM CALC-ELAPSED-REF-THRU-PODATE.
           IF JULIAN-DATE = 0
              MOVE 0 TO REB-SUB
              GO TO CREATE-RBACT-WORK-EXIT.

      * TEST TO SEE IF PAYOFF IS PRIOR TO REF-DATE:
           IF REB-ELAPSED-DAYS NOT > 0
              IF NOT (REB-HOLD-RBDTE = "C" OR "D")
                            OR SP-RBEARLY(REB-SUB) = 7
                 MOVE 0 TO REB-ELAPSED-REM
                           REB-ELAPSED-MONTHS
              ELSE
                 IF REB-PAYDATE NOT = REB-LN-INTDATE
                    MOVE 0 TO REB-ELAPSED-MONTHS
                    MOVE 1 TO REB-ELAPSED-REM.

           PERFORM REBATE-TEST-SWING.

           PERFORM REBATE-DEDUCT-CHARGE.

           IF REB-TOTCHG NOT > 0 OR SP-RBFRMLA(REB-SUB) = " "
              GO TO CREATE-RBACT-WORK-EXIT.
           IF REB-ADDON-FG = "Y" AND REB-SUB = 7
              NEXT SENTENCE
           ELSE
              IF REB-DEDCHG > REB-TOTCHG
                 GO TO CREATE-RBACT-WORK-EXIT.

NO    * SKIP THIS TEST FOR (0 I)
      *    IF NOT ( SP-RBFRMLA(REB-SUB)  = "0" AND
      *             SP-RBFRMLA2(REB-SUB) = "I"
      *           )
      * BELOW THIS FLOOR CONSIDERED FULLY EARNED
      *       IF REB-REMTERM < SP-RBNOMON(REB-SUB)
      *          IF NOT (SP-RBFRMLA(REB-SUB) = "D" OR "F")
      *             GO TO CREATE-RBACT-WORK-EXIT.

           PERFORM UPDATE-RBACT-WORK-REC.
       CREATE-RBACT-WORK-EXIT.
           EXIT.

       UPDATE-RBACT-WORK-REC SECTION.
           IF SP-RBFRMLA(REB-SUB) = "F"
              PERFORM REBATE-ACTUARIAL-F-06.

      * TN ACTUARIAL REFUND LIKE 'F' USING EQUIVALENT SIMPLE RATE
           IF SP-RBFRMLA2(REB-SUB) = "6"
              PERFORM REBATE-ACTUARIAL-F-06.
            
           PERFORM REBATE-ADJUST-REB-REBATE.


      *----OUTPUT---
           MOVE "OUTPUT"   TO       WS-RBACT-MESS.
           MOVE RBACTF-ADJ  TO      WS-RBACT-RBACTF-ADJ.
           MOVE RBACTF      TO      WS-RBACT-RBACTF.
           MOVE RBACTT      TO      WS-RBACT-RBACTT.
           MOVE RBACTI     TO       WS-RBACT-RBACTI.
           MOVE RBACTADJ   TO       WS-RBACT-RBACTADJ.
           MOVE RBACTFP    TO       WS-RBACT-RBACTFP.
           MOVE RBACTRP-A--N TO     WS-RBACT-RBACTRP-A--N.
           MOVE RBACTRP-N  TO       WS-RBACT-RBACTRP-N.
           MOVE RBACTLP    TO       WS-RBACT-RBACTLP.
           MOVE RBACTLP-N  TO       WS-RBACT-RBACTLP-N.
           MOVE RBACTFP    TO       WS-RBACT-RBACTFP.
           MOVE RBACTLP-FAC TO      WS-RBACT-RBACTLP-FAC.
           MOVE RBACTPV    TO       WS-RBACT-RBACTPV.
           MOVE RBACT-PRES-VALUE TO WS-RBACT-RBACT-PRES-VALUE.
           MOVE RBACT-REBATE TO     WS-RBACT-RBACT-REBATE.


       SETUP-WS-RBACT-REC.
               STRING
               WS-RBACT-LN-BRACCT             ,"|"
               WS-RBACT-LN-ACCTNO             ,"|"
               WS-RBACT-LN-OWNBR              ,"|"
               WS-RBACT-SP1-KEY               ,"|"              
               WS-RBACT-ORIG-1STPAY           ,"|"                
               WS-RBACT-INTDATE               ,"|"                
               WS-RBACT-RATE                  ,"|"                             
               WS-RBACT-TOTNODEF              ,"|"                
               WS-RBACT-ORGTERM               ,"|"                
               WS-RBACT-1STPYAMT              ,"|"                
               WS-RBACT-REGPYAMT              ,"|"                
               WS-RBACT-LASTPYAMT             ,"|"                
               WS-RBACT-UNITPER-CD            ,"|"                
               WS-RBACT-UNITPER-FREQ          ,"|"                            
               WS-RBACT-TOTCHG                ,"|"                
               WS-RBACT-RBDATE                ,"|"                
               WS-RBACT-ELAPSED-MONTHS        ,"|"                
               WS-RBACT-ELAPSED-REM           ,"|"              
               WS-RBACT-ELAPSED-DAYS          ,"|"                
               WS-RBACT-SKIPDEF-FG            ,"|"                
               WS-RBACT-EARN-FULL-MONTH       ,"|"                
               WS-RBACT-PENALTY-FG            ,"|"                
               WS-RBACT-REB-SUB               ,"|"
               WS-RBACT-REB-SUB2              ,"|"
               WS-RBACT-MESS                  ,"|"
               WS-RBACT-RBACTF-ADJ            ,"|"              
               WS-RBACT-RBACTF                ,"|"              
               WS-RBACT-RBACTT                ,"|"              
               WS-RBACT-RBACTI                ,"|"              
               WS-RBACT-RBACTADJ              ,"|"               
               WS-RBACT-RBACTRP-A--N          ,"|"              
               WS-RBACT-RBACTRP-N             ,"|"              
               WS-RBACT-RBACTLP               ,"|"              
               WS-RBACT-RBACTLP-N             ,"|"              
               WS-RBACT-RBACTFP               ,"|"              
               WS-RBACT-RBACTLP-FAC           ,"|"              
               WS-RBACT-RBACTPV               ,"|"               
               WS-RBACT-RBACT-PRES-VALUE      ,"|"
               WS-RBACT-RBACT-REBATE DELIMITED BY "  " INTO
                         WK-RBACT-REC.
           PERFORM WRITE-WK-RBACT-FILE.
           IF IO-FG NOT = 0
               GO TO IO-ERROR.
       SKIP-TO.
         

      *=============================================================
       SET-COMMON SECTION.
           MOVE 0 TO WS-OWNBR WS-NUMBER WS-LN-ACCTNO WS-LN-OWNBR.
           MOVE LN-OWNBR   TO WS-OWNBR.
           MOVE LN-ACCTNO  TO WS-NUMBER.
           
           MOVE LN-ACCTNO  TO WS-LN-ACCTNO.
           MOVE LN-OWNBR   TO WS-LN-OWNBR.

      *=============================================================
       REBATE-DEDUCT-CHARGE SECTION.
           IF REB-ADDON-FG = "Y" AND REB-SUB = 7
              MOVE 0 TO REB-DEDCHG
           ELSE
              MOVE 0 TO SP-RBDEDCHG(7), REB-DEDCHG.

       REBATE-ACTUARIAL-F-06 SECTION.
           MOVE REB-LN-ORIG-1STPYDATE  TO RBACT-ORIG-1STPAY 
                                          DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY TO WS-RBACT-ORIG-1STPAY.

           MOVE REB-LN-INTDATE      TO RBACT-INTDATE, DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY TO WS-RBACT-INTDATE.    

           IF SP-RBFRMLA(REB-SUB) = "F"
              MOVE REB-LN-APRATE    TO RBACT-RATE
           ELSE
              MOVE REB-LN-SMPRATE   TO RBACT-RATE.
           MOVE RBACT-RATE TO WS-RBACT-RATE.

           MOVE REB-LN-TOTNODEF     TO RBACT-TOTNODEF 
                                       WS-RBACT-TOTNODEF.
           MOVE REB-LN-ORGTERM      TO RBACT-ORGTERM
                                       WS-RBACT-ORGTERM.
           MOVE REB-LN-1STPYAMT     TO RBACT-1STPYAMT
                                       WS-RBACT-1STPYAMT.
           MOVE REB-LN-REGPYAMT     TO RBACT-REGPYAMT
                                       WS-RBACT-REGPYAMT.
           MOVE REB-LN-LASTPYAMT    TO RBACT-LASTPYAMT
                                       WS-RBACT-LASTPYAMT.
           MOVE LN-UNITPER-CD       TO RBACT-UNITPER-CD 
                                       WS-RBACT-UNITPER-CD.
           MOVE LN-UNITPER-FREQ     TO RBACT-UNITPER-FREQ
                                       WS-RBACT-UNITPER-FREQ.
           MOVE REB-TOTCHG          TO RBACT-TOTCHG 
                                       WS-RBACT-TOTCHG.
           MOVE REB-PAYDATE         TO RBACT-RBDATE 
                                       WS-RBACT-RBDATE.
           MOVE REB-ELAPSED-MONTHS  TO RBACT-ELAPSED-MONTHS 
                                       WS-RBACT-ELAPSED-MONTHS.
           MOVE REB-ELAPSED-REM     TO RBACT-ELAPSED-REM 
                                       WS-RBACT-ELAPSED-REM.
           MOVE REB-ELAPSED-DAYS    TO RBACT-ELAPSED-DAYS
                                       WS-RBACT-ELAPSED-DAYS.
           MOVE REB-SKIPDEF-FG      TO RBACT-SKIPDEF-FG 
                                       WS-RBACT-SKIPDEF-FG.
           MOVE REB-EARN-FULL-MONTH TO RBACT-EARN-FULL-MONTH 
                                       WS-RBACT-EARN-FULL-MONTH.
           MOVE REB-SUB             TO RBACT-SUB 
                                       WS-RBACT-REB-SUB.
           MOVE REB-SUB2            TO RBACT-SUB2 
                                       WS-RBACT-REB-SUB2.

           MOVE " "                 TO RBACT-PENALTY-FG 
                                       WS-RBACT-PENALTY-FG.

           PERFORM RBACT-ACTUARIAL.

           MOVE RBACT-REBATE     TO REB-REBATE.
           MOVE RBACT-PRES-VALUE TO REB-PRES-VALUE.

       REBATE-ACTUARIAL-F-EXIT.
           EXIT.

       REBATE-REFDATE SECTION.
      * = 0 SIGNALS ERROR FOR SP-RBDATE / DATE CALC:
           MOVE 0 TO REB-REFDATE.
           MOVE SP-RBDTE(REB-SUB REB-SUB2) TO REB-HOLD-RBDTE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    WHEN NON MONTHLY PAPER AMD SP-RBUNITPER-CD = 'B'
      *                            JTG 010518 PAACO PR#1466
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF SP-RBUNITPER-CD(REB-SUB) = "B" AND
              (NOT (LN-UNITPER-CD = "M" AND LN-UNITPER-FREQ = 1 ))
              MOVE LN-UNITPER-CD       TO DATER-UNITPER-CD
              MOVE LN-UNITPER-FREQ     TO DATER-UNITPER-FREQ
              MOVE REB-LN-ORIG-1STPYDATE  TO NDTE-DATE
              MOVE -1                  TO NDTE-HOLD
              PERFORM INCREMENT-UNITPER
              MOVE NDTE-DATE           TO REB-REFDATE
              GO TO REBATE-REFDATE-EXIT.

           IF SP-RBEARLY(REB-SUB) = 1
              PERFORM REBATE-EP1.

           IF REB-HOLD-RBDTE = "A"
              MOVE REB-LN-LOANDATE TO REB-REFDATE.
           IF REB-HOLD-RBDTE = "B"
              MOVE REB-LN-INTDATE  TO REB-REFDATE.

      * LOGIC FOR EARNINGS CALCULATIONS:
      * WHEN 'X' USE REB-LN-INTDATE UNLESS SALES FIN. USE PURCHDATE
           IF REB-HOLD-RBDTE = "X"
              IF REB-LN-SALEFIN = "Y"
                 MOVE REB-LN-PURDATE TO REB-REFDATE
              ELSE
                 MOVE REB-LN-INTDATE TO REB-REFDATE.

           IF NOT (REB-HOLD-RBDTE = "C" OR "D")
              GO TO REBATE-REFDATE-EXIT.

      * ONE PMT INTERVAL PRIOR TO (TRIGGER) FIRST PMT DATE:
           MOVE REB-TRIGGER-DATE TO NDTE-DATE.
           MULTIPLY -1 BY 1 GIVING NDTE-HOLD.
           PERFORM INCREMENT-MONTHS.
           MOVE NDTE-DATE TO REB-REFDATE.
       REBATE-REFDATE-EXIT.
           EXIT.

      ****************************************************
      * FIND ELAPSED DD MM & REM ON REFERENCE THRU PAYOFF
      ****************************************************
       CALC-ELAPSED-REF-THRU-PODATE SECTION.
           MOVE REB-PAYDATE TO SYS-DATE.
      * TEST FOR MISSISSIPPI REBATE 20 OR 90 DAY OPTION,
      * IF SO ADD 20 OR 90 DAYS TO PAYDATE:
           IF SP-RBSPOPT1(REB-SUB) = 3
              MOVE 20 TO REB-ELAPSED-REM
           ELSE
              IF SP-RBSPOPT1(REB-SUB) = 4
                 MOVE 90 TO REB-ELAPSED-REM.
           IF SP-RBSPOPT1(REB-SUB) = 3 OR 4
              MOVE REB-PAYDATE TO NUM-DATE
              PERFORM JUL
              ADD REB-ELAPSED-REM TO JULIAN-DATE
              PERFORM CJUL
              MOVE NUM-DATE TO SYS-DATE.
           MOVE REB-REFDATE TO NUM-DATE.
      *    - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    WHEN SP-RBUNITPER-CD(REB-SUB) = "B"
      *       IF OTHER THAN MONTHLY PAY CODE OR PAYFREQ
      *          COMPUTATION OF ELAPSED-MONTHS, ETC WILL BE DONE ON
      *          UNIT PERIOD BASES
           IF SP-RBUNITPER-CD(REB-SUB) = "B" AND
              (NOT (LN-UNITPER-CD = "M" AND LN-UNITPER-FREQ = 1 ))
              MOVE LN-UNITPER-CD       TO DATER-UNITPER-CD
              MOVE LN-UNITPER-FREQ     TO DATER-UNITPER-FREQ
              PERFORM TIMUPER
              MOVE ELAPSED-UNITPER     TO REB-ELAPSED-MONTHS
              MOVE ELAPSED-UNITPER-REM TO REB-ELAPSED-REM
              MOVE ELAPSED-DAYS        TO REB-ELAPSED-DAYS
      *    - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           ELSE
              MOVE SP-RBYRTYPE(REB-SUB, REB-SUB2) TO ELAPSED-YRTYPE
              PERFORM TIMALL
              MOVE ELAPSED-MONTHS TO REB-ELAPSED-MONTHS
              MOVE ELAPSED-REM    TO REB-ELAPSED-REM
              MOVE ELAPSED-DAYS   TO REB-ELAPSED-DAYS.

       REBATE-TEST-SWING SECTION.
           MOVE " " TO REB-SWING.
           IF (NOT
                  ( SP-RBSPOPT1(REB-SUB) = 7
                          AND ( (LN-REPOCD = "X" OR "S")
                                   OR (REB-LPTRCD = "PB" OR "RN"
                                       OR "RB" OR "RO")
                              )
                  ) OR
                  ( SP-RBSPOPT1(REB-SUB) = 18
                          AND ( (LN-REPOCD = "X" OR "S") OR
                                 (REB-LPTRCD = "PB" OR "RN" OR
                                               "RB" OR "RO") OR
                                  (REB-ELAPSED-DAYS NOT > 60)
                              )
                  )
              )
              IF (
                   (SP-RBDAYS(REB-SUB REB-SUB2) NOT = 0) AND
                                     (SP-RBEARLY(REB-SUB) NOT = 4)
                 )
               OR
                 (
                   (SP-RBEARLY(REB-SUB) = 4) AND
                         (NOT (REB-LPTRCD = "PB" OR "RN" OR "SC"
                                                OR "RB" OR "RO"))
                 )
               OR
                 (
                   (REB-EARN-FULL-MONTH = "B" OR "C")
                 )
                 MOVE "Y" TO REB-SWING
                 IF (REB-ELAPSED-REM NOT < SP-RBDAYS(REB-SUB REB-SUB2))
                           OR (REB-EARN-FULL-MONTH = "B" OR "C")
                    ADD 1 TO REB-ELAPSED-MONTHS
                    IF SP-RBEARLY(REB-SUB) NOT = 5
                       MOVE 0 TO REB-ELAPSED-REM
                    END-IF
                 ELSE
                    MOVE 0 TO REB-ELAPSED-REM.

      * SETUP REMAINING TERM:
           COMPUTE REB-REMTERM = REB-ORGTERM - REB-ELAPSED-MONTHS.

      * ADJUST TERMS BASED ON SP-RBDEFER CODE:
      * DEFERMENTS NOT CONSIDERED FOR INSURANCE:
           IF REB-SUB = REB-INTSUB
            OR (REB-SUB = REB-DEFSUB AND SP-RBFRMLA(REB-SUB) =
                1 OR "V" OR "W")
              IF SP-RBFRMLA(REB-SUB) NOT = "2"
                                  AND NOT = "C"
                                   AND NOT = "D"
                                    AND NOT = "F"
                                     AND NOT = "H"
                                      AND NOT = "I"
                                       AND NOT = "J"
                 IF SP-RBDEFER(REB-SUB) NOT = 2
                    ADD REB-LN-TOTNODEF TO REB-REMTERM
                    IF SP-RBDEFER(REB-SUB) = 0
                       ADD REB-LN-TOTNODEF TO REB-ORGTERM.
 
       REBATE-TRIGGER-DATE SECTION.
           MOVE REB-LN-ORIG-1STPYDATE TO REB-TRIGGER-DATE.

           IF SP-RBEARLY(REB-SUB) = 8
              MOVE REB-ORGTERM TO REB-HOLD-ORGTERM
              MOVE REB-LN-INTDATE TO NDTE-DATE
              MOVE 1 TO NDTE-HOLD
              PERFORM INCREMENT-MONTHS
              MOVE NDTE-DATE TO REB-TRIGGER-DATE
              IF REB-SUB = 7 OR 8 OR 9
                 PERFORM REBATE-GET-EXTDAYS
                 IF REB-TOTAL-EXTDAYS NOT < 30
                    COMPUTE REB-ORGTERM = REB-ORGTERM +
                                          (REB-TOTAL-EXTDAYS / 30).


       REBATE-GET-EXTDAYS SECTION.
           MOVE SP-PYEXYEARTP TO ELAPSED-YRTYPE.
           MOVE REB-LN-ORIG-1STPYDATE TO SYS-DATE.
           IF SP-PYEXFMDATE-LOAN-DT
              MOVE REB-LN-LOANDATE TO NUM-DATE
           ELSE
              MOVE REB-LN-INTDATE TO NUM-DATE.
           PERFORM TIMALL.
           SUBTRACT 30 FROM ELAPSED-DAYS GIVING REB-TOTAL-EXTDAYS.




       SELECT-SP-ELAPSED-TIMTBL SECTION.
           PERFORM REBATE-TRIGGER-DATE.
           MOVE REB-PAYDATE TO NUM-DATE.
           MOVE REB-TRIGGER-DATE TO SYS-DATE.
           MOVE 365 TO ELAPSED-YRTYPE.
           PERFORM TIMALL.

           IF (NOT (SP-RBSPOPT1(REB-SUB) = 3 OR 4)) AND
                                  (SP-RBEARLY(REB-SUB) NOT = 8)
              MOVE 1 TO REB-SUB2
           ELSE
              MOVE 0 TO REB-SUB2.

           IF JULIAN-DATE = 0
              MOVE 0 TO REB-SUB
           ELSE
              IF ELAPSED-DAYS NOT < REB-SUB2
                 MOVE 1 TO REB-SUB2
              ELSE
                 MOVE 2 TO REB-SUB2.

       
       REBATE-ADJUST-REB-REBATE SECTION.
      * TAKE A PERCENT OF COMPUTED REBATE:
           COMPUTE REB-REBATE ROUNDED =
                         REB-REBATE * (SP-RBPCT(REB-SUB) / 100).
           


           IF REB-ADDON-FG = "Y" AND REB-SUB = 7
              GO TO REBATE-ADJUST-REB-REBATE-FLOOR.

           COMPUTE REB-WORK2 = REB-TOTCHG - REB-REBATE.
           IF REB-ADDON-FG NOT = "Y" AND REB-SUB = 7
              IF LN-ADDON-FG = "Y"
                 IF LN-CONVERCD = "X"
                    COMPUTE REB-WORK2 = LN-ANTICERN(1) - REB-REBATE.

           IF REB-WORK2 < REB-WORKER
              COMPUTE REB-REBATE =
                        REB-WORK2 + REB-REBATE - REB-WORKER.

       REBATE-ADJUST-REB-REBATE-FLOOR.
      * NEXT STMT NECESSARY, AS < SP-RBMIN CHECK IS CONDITIONAL
           IF REB-REBATE < 0
              MOVE 0 TO REB-REBATE.

       REBATE-EP1 SECTION.
           IF LN-EXTCHG = 0
              MOVE "B" TO REB-HOLD-RBDTE
           ELSE
              IF REB-HOLD-RBDTE = "D"
                 MOVE "D" TO REB-HOLD-RBDTE
              ELSE
                 MOVE "C" TO REB-HOLD-RBDTE.
      * ^^^^^^^^^^
      * THE ABOVE LOGIS IS CONFUSING TO ME, PRIOR TO ADDING "D" THE ELSE
      * JUST SET THE VALUE TO "C" TO USE A LATER DATE DUE TO EXTENSION.
      * FROM THIS POINT FORWARD 'C' AND 'D' ARE THE SAME SO THE ONLY ONE
      * AFFECTED IS 'A' WHICH WILL CONTINUE TO BE FORCED TO A 'C AS IT
      * DID BEFORE.

       REBATE-EP2 SECTION.
           COMPUTE REB-EARN-FOR-1MON ROUNDED =
                         (REB-ORGTERM * LN-INTCHG) /
                             (REB-ORGTERM * (REB-ORGTERM + 1) / 2).
           IF REB-REBATE NOT < 0
              COMPUTE REB-REBATE = LN-INTCHG - REB-EARN-FOR-1MON
              MOVE REB-PAYDATE TO NUM-DATE
              MOVE REB-LN-ORIG-1STPYDATE TO SYS-DATE
              MOVE SP-RBYRTYPE(REB-SUB,REB-SUB2) TO ELAPSED-YRTYPE
              PERFORM TIMALL
              IF ELAPSED-DAYS > 30
                 SUBTRACT 30 FROM ELAPSED-DAYS GIVING
                                                 REB-UNUSED-EXTDAYS
                 PERFORM REBATE-EP2-REBATE-EXTCHG
                 GO TO REBATE-EP2-EXIT
              ELSE
                 PERFORM REBATE-EP2-REBATE-CHG
                 GO TO REBATE-EP2-EXIT.

       REBATE-EP2-REBATE-CHG.
           IF REB-REBATE NOT < 0
              IF ELAPSED-DAYS > 0
                 COMPUTE REB-PRORATA-30DAY-PCT =
                                         (ELAPSED-DAYS / 30) + .0009
                 COMPUTE REB-REBATE ROUNDED = REB-REBATE +
                           REB-EARN-FOR-1MON * REB-PRORATA-30DAY-PCT.

       REBATE-EP2-REBATE-EXTCHG.
           PERFORM REBATE-GET-EXTDAYS.
           IF REB-TOTAL-EXTDAYS > 0
              COMPUTE REB-REBATE ROUNDED = LN-INTCHG +
                       REB-UNUSED-EXTDAYS / REB-TOTAL-EXTDAYS *
                                                           LN-EXTCHG.

       REBATE-EP2-EXIT.
           EXIT.


      ***************************
       PRINT-RANGE-LINES SECTION.
      ***************************

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
           MOVE BEG-CLASS TO RANGE1N.
           MOVE END-CLASS TO RANGE2N.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEG DATE - END DATE" TO RANGE-SELECTION.
           MOVE BEG-DATE TO RANGE1D.
           MOVE END-DATE TO RANGE2D.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "TOTAL RECORDS READ" TO RANGE-SELECTION.
           MOVE TOTAL-RECORDS TO RANGE1N.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "TOTAL RECORDS OUT OF RANGE" TO RANGE-SELECTION.
           MOVE TOTAL-BYPASSED TO RANGE1N.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "TOTAL RECORDS SUBMITTED" TO RANGE-SELECTION.
           MOVE TOTAL-INCLUDED TO RANGE1N.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      ****************************
      *    WRITE DETAIL LINE     *
      ****************************
       WRITE-DETAIL-LINE SECTION.
       WRITE-DET-LINE.
           ADD LN-FEED LN-COUNT GIVING LN-WK.
           IF LN-WK > EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.
           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.
       EXIT-WRITE-DETAIL-LINE.
           EXIT.

      *******************************
      *    HEAD PAGE ROUTINE        *
      *******************************
       HEAD-PAGE SECTION.
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

       END-HEAD-PAGE.
           MOVE 4 TO LN-COUNT.
           MOVE 2 TO LN-FEED.



       COPY "LIBGB/GPENV.CPY".
       COPY "LIBWI/PFSWINDOWS.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/AGEING.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/SCREEN.CPY".
      *COPY "LIBGB/REBATE.CPY".
       COPY "LIBGB/RBACT.CPY". 
       COPY "LIBGB/SPPDTH.CPY".
%%     COPY "LIBLP/LPAPRS.CPY".
%%     COPY "LIBLP/LPAPRZ.CPY".
%%     COPY "LIBLP/LPITRM.CPY".
       COPY "LIBLP/SETCTBL.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBLP/LPUPWK.CPY".
       COPY "LIBLP/LPUPER.CPY".
       COPY "LIBLP/LPXDTE.CPY".
       COPY "LIBLP/LPAMTS.CPY".
       COPY "LIBLP/LPLCHG.CPY".
       COPY "LIBLP/LPCEPP.CPY".
       COPY "LIBLP/LPIBPC.CPY".
      *COPY "LIBLP/LPTBNO.CPY".
      *COPY "LIBLP/LNCOST.CPY".
      *COPY "LIBLP/LPEARN.CPY".
       COPY "LIBLP/LPCDEL.CPY".
      *COPY "LIBLP/LPTDEL.CPY".
       COPY "LIBLP/LPLCAS.CPY".
       COPY "LIBLP/LPLCAP.CPY".
       COPY "LIBLP/LPRATE.CPY".
       COPY "LIBLP/LPSCHD.CPY".
       COPY "LIBLP/LPPDUE.CPY".



      ************************************************
       FORM-RESET SECTION.
      ************************************************
           MOVE EXT-CONAME-CONAME TO TESTR2-CONAME.
           MOVE EXT-CONAME-SYSDATE TO TESTR2-SYSDATE.
           DISPLAY TESTR2-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE TESTR2-FORM-FIELDS TO WR-BUF. |MJD001
           MOVE TESTR2-FORM-LIST TO WR-LIST. |MJD001
           PERFORM CALL-WRFORM. |MJD001

       FORM-RESET-EXIT.
           EXIT.


      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE TESTR2-HELP--FILE TO HELP-LINK-FILE.
           MOVE TESTR2-HELP--DIR TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/TESTR2_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.


      ***********************
       MISC-ROUTINES SECTION.
      ***********************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/GETPR.CPY".
       INVALID-RANGE.
           MOVE "INVALID RANGE" TO MESS.
           PERFORM SEND-MESS.
       SEND-LEGEND.
           MOVE "S  A400MSEL." TO VDU.
           MOVE MESS TO VDUBUF.
           PERFORM SCREENIO.
       COPY "LIBLP/LPLTPCL.CPY".

      **********************
       IO-ROUTINE SECTION.
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLTPGS_SQL.CPY".
       COPY "LIBLP/LPLTIGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBGBGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
      **********************
       COPY "LIBGB/GBGBRN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBLP/LPLN3RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBLP/LPLTP1RN.CPY".
       COPY "LIBLP/LPLTI1RN.CPY".


       OPEN-WK-RBACT-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE WK-RBACT-IFN TO E-FILE.
           OPEN OUTPUT WK-RBACT-FILE.
           IF IO-FG = 8
              GO TO OPEN-WK-RBACT-FILE-OUTPUT.
           IF IO-FG = 7
              CLOSE WK-RBACT-FILE
              GO TO OPEN-WK-RBACT-FILE-OUTPUT.
       WRITE-WK-RBACT-FILE.
           PERFORM WRITE-IT.
           MOVE WK-RBACT-IFN TO E-FILE.
           WRITE WK-RBACT-REC.
           IF IO-FG = 8
              GO TO WRITE-WK-RBACT-FILE.
       CLOSE-WK-RBACT-FILE.
           PERFORM CLOSE-IT.
           CLOSE WK-RBACT-FILE.


       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

       END-PROGRAM SECTION.
       END-RUN.
           PERFORM CLOSE-FILES.
           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.
           IF NO-RECORDS-FOUND
              MOVE "NO REPORT FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS.
       EXIT-PROG.
           IF SV-BEG-BR NOT = 9999
              MOVE SV-BEG-BR TO BEG-BR
              MOVE SV-END-BR TO END-BR.
           IF EXT-ROUTE-BUF = "00"
              MOVE "SP/EXTRMU" TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY TESTR2-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
