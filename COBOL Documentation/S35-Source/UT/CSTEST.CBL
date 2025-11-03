      * A30 CONVERSION COMPLETED
       IDENTIFICATION DIVISION.
       PROGRAM-ID.      CSTEST.
      *****************************************************
      * CINDY
      *
      *    CREATES PIPE DELIMITED /USR/EXTRACT/CSTEST    
      *              CSTEST.R1.YYMMDD     
      *        TEST MATURITY LP MDTE
      *  REV:
      *****************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
           SELECT WK-MATURITY-FILE ASSIGN OUTPUT WK-MATURITY-PATH
                  ORGANIZATION LINE SEQUENTIAL
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  FILE STATUS FILE-STAT.


           SELECT WK-LP-FILE ASSIGN OUTPUT WK-LP-PATH
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
       FD  WK-MATURITY-FILE
           RECORD IS VARYING IN SIZE FROM 5 TO 2000 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  WK-MATURITY-REC.
           03  WK-MATURITY-BYTE        PIC X OCCURS 2000 TIMES.

      *FD  WK-EXPIR-FILE
      *    RECORD IS VARYING IN SIZE FROM 5 TO 2000 CHARACTERS
      *    LABEL RECORDS ARE STANDARD.
      *01  WK-EXPIR-REC.
      *    03  WK-EXPIR-BYTE        PIC X OCCURS 2000 TIMES.


       FD  WK-LP-FILE
           RECORD IS VARYING IN SIZE FROM 5 TO 2000 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  WK-LP-REC.
           03  WK-LP-BYTE                PIC X OCCURS 2000 TIMES.


       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)UT/CSTEST S35 03/31/23-09:23:15 >".

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
       COPY "LIBLP/LP01LTP.CPY".
       COPY "LIBLP/LP01LTP_SQL.CPY".
       COPY "LIBLP/LPWSLTP.CPY".
       COPY "LIBLP/LP01LTB.CPY".
       COPY "LIBLP/LP01LTB_SQL.CPY".
       COPY "LIBLP/LPWSLTB.CPY".
       COPY "LIBLP/LP01LTI.CPY".
       COPY "LIBLP/LP01LTI_SQL.CPY".
       COPY "LIBLP/LPWSLTI.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01LP.CPY".
       COPY "LIBLP/LP01LP_SQL.CPY".
       COPY "LIBLP/LPWSLP.CPY".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    W K - P A T H
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  WK-MATURITY-PATH.
           03  WK-MATURITY-PATH1                  PIC X(12) 
                VALUE "/USR/EXTRACT".
           03  WK-MATURITY-FILENAME               PIC X(10) 
                 VALUE "/MATURITY.".                    

      *01  WK-EXPIR-PATH.
      *    03  WK-EXPIR-PATH1                  PIC X(12) 
      *         VALUE "/USR/EXTRACT".
      *    03  WK-MATURITY-FILENAME               PIC X(12) 
      *          VALUE "/EXPIRATION.".                  

           03  WK-MACHINE                PIC X(2).
           03  WK-PATH-YR                PIC 99.
           03  WK-PATH-MO                PIC 99.
           03  WK-PATH-DA                PIC 99.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    W K - L P - P A T H
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

       01  WK-LP-PATH.
           03  WK-LP-PATH1                  PIC X(12) 
                VALUE "/USR/EXTRACT".
           03  WK-LP-FILENAME               PIC X(9) 
                 VALUE "/CSTEST2.".                      

           03  WK-LP-MACHINE                PIC XX.
           03  FILLER                       PIC X VALUE ".".
           03  WK-LP-PATH-YR                PIC 99.
           03  WK-LP-PATH-MO                PIC 99.
           03  WK-LP-PATH-DA                PIC 99.

       01  WS-MATURITY-REC.
           03  WS-MATURITY-LN-BRACCTNO              PIC Z(14).
           03  WS-MATURITY-LN-ACCTNO                PIC Z(10).
           03  WS-MATURITY-LN-OWNBR                 PIC Z(6).
      *INPUT TO LPMDTE
           03  WS-MATURITY-SYSDATE                  PIC 99/99/99.
           03  WS-MATURITY-DESC1                    PIC X(13).
           03  WS-MATURITY-MDTE-FLAG                PIC X.
      *'Y' USES ORIG TERM, " " USES SUM-ORGTERM
           03  WS-MATURITY-MDTE-ORGTERM-FG          PIC X.
           03  WS-MATURITY-MDTE-DC-FLAG             PIC X.
           03  WS-MATURITY-LN-ORGTERM               PIC ZZ9.
           03  WS-MATURITY-LN-TOTNODEF              PIC Z9-.
           03  WS-MATURITY-LN-1STPYDATE             PIC 99/99/99.
           03  WS-MATURITY-LN-SUM-ORGTERM           PIC ZZ9.
           03  WS-MATURITY-LN-ORIG-1STPYDATE        PIC 99/99/99.
      *LN-1STPYDATE + DF IF MDTE-ORGTERM-FG = 'Y'
      *INPUT TO INCREMENT-UNITPER (DATER)
           03  WS-NDTE-HOLD                         PIC 99.
           03  WS-MATURITY-LN-UNITPER-CD            PIC X.
           03  WS-MATURITY-LN-UNITPER-FREQ          PIC 9.
      *OUPUT
           03  WS-MATURITY-DESC2                    PIC X(6).
           03  WS-MATURITY-MDTE-DATE                PIC 99/99/99.


      *01  WS-EXPIR-REC.
      *    03  WS-EXPIR-LN-BRACCTNO              PIC Z(14).
      *    03  WS-EXPIR-LN-ACCTNO                PIC Z(10).
      *    03  WS-EXPIR-LN-OWNBR                 PIC Z(6).
      *INPUT TO LPXDTE
      *    03  WS-EXPIR-MATURITY-DATE           PIC 99/99/99.
      *    03  WS-SP-EXPD-MONTHS                PIC Z9.


       01  WS-LP-REC.
           03  WS-LP-BRACCTNO                PIC Z(14).
           03  WS-LP-OWNBR                   PIC Z(6).
           03  WS-LP-NUMBER                  PIC Z(10).
           03  WS-LP-SEQNO                   PIC Z(7).
           03  WS-LP-PAYDATE                 PIC 9(08).
           03  WS-LP-TRAMT                   PIC Z(9).99.
           03  WS-LP-TRCD                    PIC X(2).

       01  WK-MATURITY-IFN        PIC X(5)     VALUE "WKMAT".
       01  WK-LP-IFN        PIC X(4)     VALUE "WKLP".


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

       01  JUSTIFY-TYPE     PIC X VALUE "L".
       01  NXDUE-REBATE     PIC S9(7)V99 COMP-3.
       01  PRIOR-REBATE     PIC S9(7)V99 COMP-3.
       01  PER-DIEM-RATE    PIC 999V999.

       01  TODAYS-DATE      PIC 9(8).
 
       01  WS-BRACCT.
           03  WS-OWNBR     PIC Z(6).
           03  WS-NUMBER    PIC 9(6).

       01  ZIP-WORK             PIC X(10).
       01  X REDEFINES ZIP-WORK.
           03  FILLER           PIC X(6).
           03  PART-2           PIC X(4).
       01  X1 REDEFINES ZIP-WORK.
           03  FILLER           PIC X(5).
           03  PART-2-A         PIC X(4).
           03  PART-2-FILLER    PIC X.
       01  X3 REDEFINES ZIP-WORK.
           03  PART-3-ZIP       PIC X(5).
           03  PART-3-DASH      PIC X(1).
           03  PART-3-EXT       PIC X(4).

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
       COPY "LIBWI/LCSCANW.CPY".
       COPY "LIBWI/GRSCANW.CPY".

       COPY "LIBLP/LPWKPATH.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBCL/CRNOR2W.CPY".
       COPY "LIBLP/LP01NO.CPY".
       COPY "LIBLP/LPPOFFW.CPY".
       COPY "LIBLP/LPAMTSW.CPY".
       COPY "LIBGB/SPPDTHW.CPY".
       COPY "LIBGB/RBACTW.CPY".
       COPY "LIBGB/REBATEW.CPY".
       COPY "LIBLP/LPRATEW.CPY".
       COPY "LIBLP/LPIBPCW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBLP/LPXDTEW.CPY".
       COPY "LIBLP/SETCTBLW.CPY".
       COPY "LIBLP/LPAPRSW.CPY".
       COPY "LIBLP/LPAPRZW.CPY".
       COPY "LIBLP/LPITRMW.CPY".
       COPY "LIBLP/LPUPERW.CPY".
       COPY "LIBLP/LPUPWKW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBUT/CSTEST_DEF.CPY".
       COPY "LIBUT/CSTEST_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *****************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      *****************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBUT/CSTEST_SCN.CPY".

      *----------------------------------------------------------------
       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE WK-LP-FILE WK-MATURITY-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LTP1-FILE.
           PERFORM CLOSE-LTI1-FILE.
           PERFORM CLOSE-LTB1-FILE.
           PERFORM CLOSE-LP1-FILE.
           PERFORM CLOSE-LN3-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-GB-FILE.
           PERFORM CLOSE-BR-FILE.
           CLOSE
               PR-FILE WK-LP-FILE WK-MATURITY-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      *************************************
       MAIN-MODULE SECTION.
           PERFORM SQL-CONNECT.
      *************************************
      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "CSTEST" TO VDU-FORM-NAME.

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
                              LTP-PATH-OVERRIDE
                              LTB-PATH-OVERRIDE
                              LTI-PATH-OVERRIDE
                              GB-PATH-OVERRIDE
                              BW-PATH-OVERRIDE
                              LP-PATH-OVERRIDE.

           PERFORM OPEN-LN3-FILE.
           PERFORM OPEN-LTP1-FILE.
           PERFORM OPEN-LTB1-FILE.
           PERFORM OPEN-BW1-FILE.
           PERFORM OPEN-LP1-FILE.

           PERFORM GLOBAL-UPDATE.

           PERFORM CLOSE-LN3-FILE.
           PERFORM CLOSE-LTP1-FILE.
           PERFORM CLOSE-LTB1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-LP1-FILE.

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

       GET-SPR.
           MOVE LN-ORGST     TO HOLD-SP-ORGST.
           MOVE LN-SPRCLASS  TO HOLD-SP-SPRCLASS.
           MOVE LN-SUBCLASS  TO HOLD-SP-SUBCLASS.
           MOVE LN-LAWCODE   TO HOLD-SP-LAWCODE.
           IF HOLD-SP1-KEY NOT = SP1-KEY
              MOVE HOLD-SP1-KEY TO SP1-KEY
              PERFORM READ-SP1-FILE
              IF IO-FG NOT = 0
                 ADD 1 TO TOTAL-BYPASSED
                 GO TO NEXT-LN.

           PERFORM SETUP-MATURITY-WORK.   
      *    PERFORM CREATE-LP-WORK.

           GO TO NEXT-LN.

       EXIT-GLOBAL-UPDATE.
           EXIT.

      /**********************************
      *   I N I T I A L I Z A T I O N   *
      ***********************************
       INITIALIZATION SECTION.
           MOVE CSTEST-QUEUE-POS TO Q-POS.
           MOVE CSTEST-COPIES-POS TO C-POS.
           MOVE CSTEST-PRU-POS   TO P-POS.
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


           MOVE HOLD-FIL-MACHINE TO WK-MACHINE
                                    WK-LP-MACHINE.

      D    STOP MESS.
           MOVE TODAYS-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY-DD TO WK-PATH-DA.
           MOVE DATE-MMDDYY-MM TO WK-PATH-MO.
           MOVE DATE-MMDDYY-YY TO WK-PATH-YR.

           CALL "C$TOLOWER" USING WK-MATURITY-PATH1,VALUE 12.
           PERFORM OPEN-WK-MATURITY-FILE-OUTPUT.

      *    CALL "C$TOLOWER" USING WK-LP-PATH1,VALUE 12.
      *    PERFORM OPEN-WK-LP-FILE-OUTPUT.

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

      *----------------------------------------------------------
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
       SETUP-MATURITY-WORK SECTION.
      ****************************
      *    MOVE LN-ACCTNO          TO CRNOR2-ACCTNO.
      *    MOVE TODAYS-DATE        TO CRNOR2-SYSDATE.
      *    MOVE "CL/CRNOR2" TO FORM-NAM.
      *    CALL FORM-PROGX USING FORM-PATH CRNOR2-LINK
      *                          NO-REC POFF-WORKERS
      *                          LN-REC.

      *    CANCEL FORM-PROGX.

      *    MOVE LN-SSNO(1) TO BW-SSNO.
      *    PERFORM READ-BW1-FILE.
      *    IF IO-FG NOT = 0
      *       GO TO CREATE-LN-WORK-EXIT.

           MOVE SPACES TO WK-MATURITY-REC WS-MATURITY-REC.

           MOVE LN-OWNBR   TO WS-OWNBR.
           MOVE LN-ACCTNO  TO WS-NUMBER.
           CALL "C$JUSTIFY" USING WS-BRACCT, JUSTIFY-TYPE.
           
           MOVE WS-BRACCT TO WS-MATURITY-LN-BRACCTNO.
           
           MOVE LN-ACCTNO  TO WS-MATURITY-LN-ACCTNO.
           CALL "C$JUSTIFY" USING WS-MATURITY-LN-ACCTNO, 
                                               JUSTIFY-TYPE.
           MOVE LN-OWNBR   TO WS-MATURITY-LN-OWNBR.
           CALL "C$JUSTIFY" USING WS-MATURITY-LN-OWNBR, JUSTIFY-TYPE.
        
      *ORIGINAL LOAN TERM
           MOVE "ORIG MATURITY"   TO WS-MATURITY-DESC1.
           MOVE "Y"                TO 
                                WS-MATURITY-MDTE-FLAG MDTE-FLAG.
           MOVE "Y"               TO WS-MATURITY-MDTE-ORGTERM-FG
                                         MDTE-ORGTERM-FG.
           MOVE "Y"                TO WS-MATURITY-MDTE-DC-FLAG 
                                         MDTE-DC-FLAG.
           PERFORM UPDATE-MATURITY-WORK-REC.                               
           
           MOVE "UPD MATURITY"     TO WS-MATURITY-DESC1.
           MOVE " "                TO WS-MATURITY-MDTE-FLAG
                                                    MDTE-FLAG.
           MOVE " "              TO WS-MATURITY-MDTE-ORGTERM-FG
                                         MDTE-ORGTERM-FG.
           MOVE " "                TO WS-MATURITY-MDTE-DC-FLAG 
                                         MDTE-DC-FLAG.
           PERFORM UPDATE-MATURITY-WORK-REC.                               
 
       EXIT-SETUP-MATURITY-WORK.         
           EXIT.

%%     UPDATE-MATURITY-WORK-REC SECTION.
      *INPUT TO MATURITY (LPMDTE)
      *    MOVE "ORIG MATURITY"   TO WS-DESC1.
           MOVE TODAYS-DATE TO DATE-YYYYMMDD.    
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY   TO WS-MATURITY-SYSDATE.
      *ORIGINAL LOAN TERM
      *    MOVE "Y"                TO WS-MATURITY-MDTE-FLAG.
      *    MOVE "Y"                TO WS-MATURITY-MDTE-ORGTERM-FG.
      *    MOVE "Y"                TO WS-MATURITY-MDTE-DC-FLAG.
           MOVE LN-ORGTERM         TO WS-MATURITY-LN-ORGTERM.
           CALL "C$JUSTIFY" USING WS-MATURITY-LN-ORGTERM
                                             ,JUSTIFY-TYPE.

           MOVE LN-SUM-ORGTERM     TO WS-MATURITY-LN-SUM-ORGTERM.
           CALL "C$JUSTIFY" USING WS-MATURITY-LN-SUM-ORGTERM, 
                                                      JUSTIFY-TYPE.

           MOVE LN-TOTNODEF         TO WS-MATURITY-LN-TOTNODEF.
           CALL "C$JUSTIFY" USING WS-MATURITY-LN-TOTNODEF,
                                                    JUSTIFY-TYPE.

           MOVE LN-1STPYDATE       TO DATE-YYYYMMDD.     
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY        TO WS-MATURITY-LN-1STPYDATE.

           MOVE LN-ORIG-1STPYDATE  TO DATE-YYYYMMDD.        
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY        TO WS-MATURITY-LN-ORIG-1STPYDATE.

           PERFORM MATURITY-DATE-CALCULATION.
           MOVE "OUPUT"            TO WS-MATURITY-DESC2.
           MOVE MDTE-DATE          TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY        TO WS-MATURITY-MDTE-DATE. 
           STRING
              WS-MATURITY-LN-BRACCTNO         ,"|"
              WS-MATURITY-LN-ACCTNO           ,"|"
              WS-MATURITY-LN-OWNBR            ,"|"
              WS-MATURITY-DESC1               ,"|"
              WS-MATURITY-SYSDATE             ,"|"
              WS-MATURITY-MDTE-FLAG           ,"|"
              WS-MATURITY-MDTE-ORGTERM-FG     ,"|"
              WS-MATURITY-MDTE-DC-FLAG        ,"|"
              WS-MATURITY-LN-ORGTERM          ,"|"
              WS-MATURITY-LN-TOTNODEF         ,"|"
              WS-MATURITY-LN-1STPYDATE        ,"|"
              WS-MATURITY-LN-SUM-ORGTERM      ,"|"
              WS-MATURITY-LN-ORIG-1STPYDATE   ,"|"
              WS-NDTE-HOLD                    ,"|"
              WS-MATURITY-LN-UNITPER-CD       ,"|",
              WS-MATURITY-LN-UNITPER-FREQ     ,"|"
      *OUPUT
              WS-MATURITY-DESC2               ,"|"
              WS-MATURITY-MDTE-DATE,CR-1
                 DELIMITED BY "  " INTO WK-MATURITY-REC.
           PERFORM WRITE-WK-MATURITY-FILE.

           IF IO-FG NOT = 0
              GO TO IO-ERROR.
           MOVE "N" TO FIRST-TIME.
           ADD 1 TO TOTAL-INCLUDED.

       UPDATE-WORK-EXIT.
           EXIT.



      ****************************
       CREATE-LP-WORK SECTION.
      ****************************
*L2G       MOVE LP-PATH-OWNBR TO LP-BRNO.
           MOVE LN-ACCTNO     TO LP-ACCTNO.
           MOVE 0             TO LP-SEQNO.
           PERFORM START-LP1-FILE.
       NEXT-LP.
           PERFORM READ-LP1-FILE-NEXT.
           IF IO-BAD
              GO TO CREATE-LP-WORK-EXIT.
*L2G       IF (LP-ACCTNO NOT = LN-ACCTNO) OR
                (LP-BRNO NOT = LP-PATH-OWNBR)
              GO TO CREATE-LP-WORK-EXIT.
        
           IF NOT (LP-TRCD = "PY" OR "PA" OR "RV")
              GO TO NEXT-LP.

           IF LP-TRCD = "RV" 
              IF  LP-REVTRCD NOT = "PA"             
                  AND LP-REVTRCD NOT = "PY"
                    GO TO NEXT-LP.


           MOVE SPACES TO WK-LP-REC WS-LP-REC.
           MOVE LN-OWNBR TO WS-OWNBR.
           MOVE LN-ACCTNO TO WS-NUMBER.
           CALL "C$JUSTIFY" USING WS-BRACCT,JUSTIFY-TYPE.

           MOVE WS-BRACCT TO WS-LP-BRACCTNO.


           MOVE LN-OWNBR    TO WS-LP-OWNBR.
           CALL "C$JUSTIFY" USING WS-LP-OWNBR, JUSTIFY-TYPE.

           MOVE LP-ACCTNO   TO WS-LP-NUMBER.
           CALL "C$JUSTIFY" USING WS-LP-NUMBER,JUSTIFY-TYPE.

           MOVE LP-SEQNO    TO WS-LP-SEQNO.
           CALL "C$JUSTIFY" USING WS-LP-SEQNO,JUSTIFY-TYPE.

           MOVE LP-PAYDATE  TO WS-LP-PAYDATE. 

           MOVE LP-TRAMT    TO WS-LP-TRAMT.
           CALL "C$JUSTIFY" USING WS-LP-TRAMT,JUSTIFY-TYPE.

           MOVE LP-TRCD     TO WS-LP-TRCD.

           STRING
              WS-LP-BRACCTNO         ,"|"
              WS-LP-OWNBR            ,"|"
              WS-LP-NUMBER           ,"|"
              WS-LP-SEQNO            ,"|"
              WS-LP-PAYDATE          ,"|"
              WS-LP-TRAMT            ,"|"
              WS-LP-TRCD,CR-1             
              DELIMITED BY "  " INTO WK-LP-REC.
              PERFORM WRITE-WK-LP-FILE.

              IF IO-FG NOT = 0
                 GO TO IO-ERROR.
           
           GO TO NEXT-LP.


       CREATE-LP-WORK-EXIT.
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

      ********************************************************
      *IB-REBATE SECTION.
      ********************************************************

      *    MOVE "PY"             TO RATE-LPTRCD.
      *    MOVE TODAYS-DATE      TO RATE-DATE IBPC-DATE.
      *    MOVE LN-1STPYDATE     TO RATE-DATE IBPC-DATE.
      *    MOVE LN-MAKERCD(1)    TO RATE-MAKERCD.
      *    PERFORM SET-PY-RATE
      *    COMPUTE PER-DIEM-RATE ROUNDED = INDU-WORKER / INDU-WK
      *    COMPUTE WS-INT-PORTION-OF-PAY = PER-DIEM-RATE * 30.
      *    CALL "C$JUSTIFY" USING WS-INT-PORTION-OF-PAY, JUSTIFY-TYPE.

      ********************************************************
       PRE-COMPUTE-REBATE SECTION.
      ********************************************************

      *    MOVE 0 TO REB-REBATE NXDUE-REBATE PRIOR-REBATE.

      * REBATE ALREADY GIVEN OR INTEREST CANCELLED
      *    IF NOT (LN-INT-REBATE = " " OR "C")
      *       MOVE 0 TO WS-INT-PORTION-OF-PAY
      *       CALL "C$JUSTIFY" USING WS-INT-PORTION-OF-PAY, JUSTIFY-TYPE
      *       GO TO EXIT-PRE-COMPUTE-REBATE.

      * CALCULATE AN INTEREST REBATE AS OF NO-NXDUE

      *    MOVE NO-NXDUE-DATE TO REB-PAYDATE
      *    MOVE LN-ORGTERM    TO REB-ORGTERM
      *    MOVE 7             TO REB-SUB
      *    MOVE "RI"          TO REB-LPTRCD
      *    MOVE LN-INTCHG     TO REB-TOTCHG
      *    PERFORM REBATE.
      *    MOVE REB-REBATE TO NXDUE-REBATE.

      * FOR ACCOUNTS THAT HAVE NOT MADE THEIR FIRST PAYMENT (NO-NXDUE =
      * THE FIRST PAYMENT DUE DATE, USE THE ORIGINAL INTEREST + EXTENSION
      * CHARGE (RATHER THAN TRYING TO CALC A REBATE) AND TAKE THE DIFFERENCE
      * BETWEEN THAT AND AN RI ON THE FIRST DUE (NO-NXDUE)
      *    IF NO-NXDUE-DATE = LN-1STPYDATE
      *       COMPUTE WS-INT-PORTION-OF-PAY =
      *           (LN-INTCHG + LN-EXTCHG ) - NXDUE-REBATE
      *       CALL "C$JUSTIFY" USING WS-INT-PORTION-OF-PAY, JUSTIFY-TYPE
      *       GO TO EXIT-PRE-COMPUTE-REBATE.

      * NOW CALCULATE AN INTEREST REBATE AS OF ONE PAY PERIOD PRIOR
      * (1 MONTH USING SAME DAY OF MONTH)
      *    MOVE -1            TO NDTE-HOLD.
      *    MOVE NO-NXDUE-DATE TO NDTE-DATE.
      *    PERFORM INCREMENT-MONTHS.

      *    MOVE NDTE-DATE  TO REB-PAYDATE
      *    MOVE LN-ORGTERM TO REB-ORGTERM
      *    MOVE 7          TO REB-SUB
      *    MOVE "RI"       TO REB-LPTRCD
      *    MOVE LN-INTCHG  TO REB-TOTCHG
      *    PERFORM REBATE.
      *    MOVE REB-REBATE TO PRIOR-REBATE.

      *    COMPUTE WS-INT-PORTION-OF-PAY = NXDUE-REBATE - PRIOR-REBATE.
      *    CALL "C$JUSTIFY" USING WS-INT-PORTION-OF-PAY, JUSTIFY-TYPE.

      *EXIT-PRE-COMPUTE-REBATE.
      *    EXIT.


       COPY "LIBGB/GPENV.CPY".
       COPY "LIBWI/PFSWINDOWS.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/SPPDTH.CPY".
       COPY "LIBLP/LPAMTS.CPY".
       COPY "LIBLP/LPRATE.CPY".
       COPY "LIBLP/LPIBPC.CPY".
       COPY "LIBGB/RBACT.CPY".
       COPY "LIBGB/REBATE.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBLP/LPXDTE.CPY".
       COPY "LIBLP/SETCTBL.CPY".
       COPY "LIBLP/LPAPRS.CPY".
       COPY "LIBLP/LPAPRZ.CPY".
       COPY "LIBLP/LPITRM.CPY".
       COPY "LIBLP/LPUPWK.CPY".
       COPY "LIBLP/LPUPER.CPY".


      ************************************************
       FORM-RESET SECTION.
      ************************************************
           MOVE EXT-CONAME-CONAME TO CSTEST-CONAME.
           MOVE EXT-CONAME-SYSDATE TO CSTEST-SYSDATE.
           DISPLAY CSTEST-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE CSTEST-FORM-FIELDS TO WR-BUF. |MJD001
           MOVE CSTEST-FORM-LIST TO WR-LIST. |MJD001
           PERFORM CALL-WRFORM. |MJD001

       FORM-RESET-EXIT.
           EXIT.


      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE CSTEST-HELP--FILE TO HELP-LINK-FILE.
           MOVE CSTEST-HELP--DIR TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBUT/CSTEST_EVA.CPY".
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

      **********************
       IO-ROUTINE SECTION.
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLTPGS_SQL.CPY".
       COPY "LIBLP/LPLTIGS_SQL.CPY".
       COPY "LIBLP/LPLTBGS_SQL.CPY".
       COPY "LIBLP/LPLPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBGBGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
      **********************
       COPY "LIBGB/GBGBRN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBLP/LPLN3RN.CPY".
       COPY "LIBLP/LPLTP1RN.CPY".
       COPY "LIBLP/LPLTB1RN.CPY".
       COPY "LIBLP/LPLTI1RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBLP/LPLP1RN.CPY".

       OPEN-WK-MATURITY-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE WK-MATURITY-IFN TO E-FILE.
           OPEN OUTPUT WK-MATURITY-FILE.
           IF IO-FG = 8
              GO TO OPEN-WK-MATURITY-FILE-OUTPUT.
           IF IO-FG = 7
              CLOSE WK-MATURITY-FILE
              GO TO OPEN-WK-MATURITY-FILE-OUTPUT.
       WRITE-WK-MATURITY-FILE.
           PERFORM WRITE-IT.
           MOVE WK-MATURITY-IFN TO E-FILE.
           WRITE WK-MATURITY-REC.
           IF IO-FG = 8
              GO TO WRITE-WK-MATURITY-FILE.
       CLOSE-WK-MATURITY-FILE.
           PERFORM CLOSE-IT.
           CLOSE WK-MATURITY-FILE.

       OPEN-WK-LP-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE WK-LP-IFN TO E-FILE.
           OPEN OUTPUT WK-LP-FILE.
           IF IO-FG = 8
              GO TO OPEN-WK-LP-FILE-OUTPUT.
           IF IO-FG = 7
              CLOSE WK-LP-FILE
              GO TO OPEN-WK-LP-FILE-OUTPUT.
       WRITE-WK-LP-FILE.
           PERFORM WRITE-IT.
           MOVE WK-LP-IFN TO E-FILE.
           WRITE WK-LP-REC.
           IF IO-FG = 8
              GO TO WRITE-WK-LP-FILE.
       CLOSE-WK-LP-FILE.
           PERFORM CLOSE-IT.
           CLOSE WK-LP-FILE.

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
              DESTROY CSTEST-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
