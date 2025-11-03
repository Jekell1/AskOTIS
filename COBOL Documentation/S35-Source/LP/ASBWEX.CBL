       IDENTIFICATION DIVISION.
       PROGRAM-ID.      ASBWEX.
       DATE-WRITTEN. 112888
      ************************************************************************
      *    DIR :  LP
      *    DESC:  ADVERTISING/SOLICITATION BORROWER SELECTION
      * REV:
      *  MJD 051795 ADDED F6 - FORM SCAN OPTION.
      *  MJD 120497 EXPANDED BW-LOANS TO PIC 9(2)
      *  GLF 990923 REPLACED IO-FG NUMBERS WITH IO-FG 88 LEVEL PER DEVELOPMENT
      *             MEETING (082599)
      *  BAH 040318 ADDED BEG AND END CREDIT SCORE, PACE PR# 2263
      *  BAH 040419 ADDED ORIGIN FOR CASHCO, PR# 2273
      *  BLV 040817 ADDED GROUP LOGIC, CROSS DATA PATHS, ADD CORPORATE
      *             PRINTING OPTION, PR# 1030
      * BAH 071008 WORLD NO LONGER SOLICITING BORROWERS IN KANSAS PR#557
      *
      * KEC 2016.0205 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         FIXED KEYS & RECS FOR FD-REC.
      *         CHANGED FMNO/FORMNO/NEXTFM/SETFM FROM 3 TO 4 BYTES.
      *
      * KEC 2017.0119 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          CHANGED FD-SMARTPORT TO FD-OPTION [88 LEVELS].
      *
      * BAH 170316 ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 180116 USE LPBAGE TO CALCULATE AGE SO ITS CONSISTENT #1298
      * BAH 181002 GLOBAL BWFILE
      * BAH 190312 MOVED FM-BRNO2 INTO LPWSFM-EDIT-FM-INITKEY TO REMOVE THE
      *            REDEFINE IN THE FM-REC
      * BAH 190613 ADDED CLEAR-FM-FILE (LPFMCL) AND SET FM-USERID
      * BAH 20190919 REMOVED ACCESS-CALL ON BWFILE
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
           "@(#)LP/ASBWEX.CBL S35 02/20/24-15:25:50 >".
      ****************************
      *     F I L E   S P E C S  *
      ****************************
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01FM.CPY".
       COPY "LIBLP/LP01FM_SQL.CPY".
       COPY "LIBLP/LPWSFM.CPY".
       COPY "LIBLP/LP01FD.CPY".
       COPY "LIBLP/LP01FD_SQL.CPY".
       COPY "LIBLP/LPWSFD.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       01  PR-IFN        PIC X(7)    VALUE "PRINTER".

       01  SEL-TYPE      VALUE SPACES.
           03  SEL-FMNO  PIC 9(4).
           03  SEL-FMNOX REDEFINES SEL-FMNO
                         PIC X(4).

      **********************
      *   LINE WORKERS     *
      **********************
       01  LN-FEED              PIC 99       VALUE 0.
       01  LN-COUNT             PIC 999      VALUE 99.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

      **********************
      *   MISC WORKERS
      **********************
       01  SUB                  PIC 999.
       01  SUB1                 PIC 999.
       01  MATCH-MAX            PIC 99       VALUE 0.
       01  MATCH-OK             PIC X.
       01  MATCH1-CODE.
           03  MATCH1           OCCURS 10 PIC X.
       01  MATCH2-CODE.
           03  MATCH2           OCCURS 10 PIC X.
       01  ORIGIN-WK.
           03  ORIGIN-CHAR      PIC X        OCCURS 4.
       01  ORIGIN-SUB           PIC 9.
       01  ORIGIN-MATCH         PIC X.

       01  HOLD-FM1-KEY         PIC X(28)     VALUE SPACES.
       01  REF-YEARS            PIC 999.
       01  TIME-ALP             PIC X.
       01  TIME-WORK            PIC 9999.
       01  TIME-WK-TAB REDEFINES TIME-WORK.
           03  TIME-WK          OCCURS 4 TIMES
                                PIC 9.
       01  FIRST-TIME           PIC X        VALUE "Y".
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".
       01  TRANS-DATE           PIC 9(8)     COMP-3 VALUE 0.
       01  BR-TOTAL-BORR        PIC 9(7)     COMP-3 VALUE 0.
       01  GR-TOTAL-BORR        PIC 9(7)     COMP-3 VALUE 0.

      ***********************************
      *     S C R E E N   F I E L D S   *
      ***********************************
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  SSNO1           PIC 999999999.
           03  SSNO2           PIC 999999999.
           03  BEG-BRANCH      PIC 9(4).
           03  END-BRANCH      PIC 9(4).
           03  STATE           PIC XX.
           03  BEGZIP          PIC X(10).
           03  ENDZIP          PIC X(10).
           03  COUNTY          PIC XXX.
           03  DWELL           PIC XX.
           03  CENSUS          PIC X(10).
           03  SMSA            PIC X(6).
           03  RENT            PIC X(4).
           03  YRADDR          PIC 99.
           03  MINAGE          PIC 99.
           03  MAXAGE          PIC 99.
           03  MINEMPL         PIC 99.
           03  MINPAY          PIC 9(6).
           03  MINTPAY         PIC 9(6).
           03  MINLIA          PIC 9(6).
           03  MAXLIA          PIC 9(6).
           03  MPO             PIC 99.
           03  BEGSLSMN        PIC 9999.
           03  ENDSLSMN        PIC 9999.
           03  BEGREP          PIC X(4).
           03  ENDREP          PIC X(4).
           03  MARRIED         PIC X.
           03  FORM            PIC 999.
           03  REPT            PIC X.
           03  BEG-CRSCORE     PIC 9(3).
           03  END-CRSCORE     PIC 9(3).
           03  ORIGINX.
               05  ORIGIN-CD   PIC X OCCURS 4.
           03  GROUP-FLAG       PIC X.
           03  ENT-GROUP        PIC X(4).
           03  CORP-FM-FG       PIC X.

       01  VERSION-CONTROL     PIC X    VALUE "D".

       01  SV-BEG-BR           PIC 9(4)       VALUE 9999.
       01  SV-END-BR           PIC 9(4)       VALUE 9999.
       01  SV-GROUP-FLAG       PIC X          VALUE SPACES.
       01  SV-ENT-GROUP        PIC X(4)       VALUE SPACES.

      ***********************************
      *     P R I N T   B U F F E R S   *
      ***********************************
       01  HEAD1.
           03  H-DATE           PIC X(8)     VALUE " ".
           03  FILLER           PIC XX       VALUE " ".
           03  H-HR             PIC 99       VALUE 0.
           03  FILLER           PIC X        VALUE ":".
           03  H-MIN            PIC 99       VALUE 0.
           03  FILLER           PIC X(31)    VALUE " ".
           03  H-NAME           PIC X(40)    VALUE " ".
           03  FILLER           PIC X(37)    VALUE " ".
           03  FILLER           PIC X(5)     VALUE "PAGE ".
           03  H-PAGE-NO        PIC ZZZ9.

       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(19)    VALUE " ".
           03  FILLER           PIC X(54)    VALUE
           "ADVERTISING / SOLICITATION BORROWER SELECTION REPORT".

       01  HEAD3.
           03  FILLER      PIC X(65)    VALUE SPACES.
           03  FILLER      PIC X(30)    VALUE
               " RENT  YRS   MONTHLY     OTHER".
           03  FILLER      PIC X(36)    VALUE
               "             NO      TOTAL    LAST".
       01  HEAD4.
           03  FILLER      PIC X(31)    VALUE
               "BR # BORROWER NAME".
           03  FILLER      PIC X(26)    VALUE
               "SPOUSE NAME".
           03  FILLER      PIC X(33)    VALUE
               "HOME PHONE    CODE EMP       PAY ".
           03  FILLER      PIC X(42)    VALUE
               "      PAY SOURCE LOANS  LIABILITY   PAYOFF".

       01  DETAIL-LINE     PIC X(132) VALUE SPACES.
       01  D-LINE1         REDEFINES DETAIL-LINE.
           03  D-BRNO      PIC ZZZ9.
           03  FILLER      PIC X.
           03  D-NAME      PIC X(25).
           03  FILLER      PIC X.
           03  D-SPNAME    PIC X(25).
           03  FILLER      PIC X.
           03  D-PHONE     PIC 999/999/9999.
           03  FILLER      PIC X.
           03  D-RENTCD    PIC X(4).
           03  FILLER      PIC XXX.
           03  D-YREMP     PIC Z9.
           03  FILLER      PIC X.
           03  D-PAY       PIC ZZ,ZZ9.99.
           03  FILLER      PIC X.
           03  D-PAYOT     PIC ZZ,ZZ9.99.
           03  FILLER      PIC X.
           03  D-PAYSR     PIC X(8).
           03  FILLER      PIC X(2).
           03  D-LOANS     PIC Z9.
           03  FILLER      PIC X.
           03  D-LIA       PIC ZZZ,ZZ9.99.
           03  FILLER      PIC X.
           03  D-LASTPOFF  PIC 99/99/99 BLANK ZERO.

       01  D-TOTAL-LINE    REDEFINES DETAIL-LINE.
           03  FILLER      PIC X(28).
           03  D-TOT-BORR  PIC ZZZZ,ZZ9.

       01  RANGE-LN REDEFINES DETAIL-LINE.
           03  RANGE-LINE       PIC X(30).
           03  RANGE-SELECTION  PIC X(102).
           03  RANGE-TABLE      REDEFINES RANGE-SELECTION.
               05  RANGE-TAB    OCCURS 9
                                PIC XXXXX.
               05  FILLER       PIC X(57).

       COPY "LIBWI/FDSCANW.CPY".
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBLP/LPBAGEW.CPY".

       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/ASBWEX_DEF.CPY".
       COPY "LIBLP/ASBWEX_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      /***************************************
      *                                      *
      *     P R O G R A M   C O N T R O L    *
      *                                      *
      ****************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBLP/ASBWEX_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-FM1-FILE.
           PERFORM CLOSE-FD1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      *************************************
       MAIN-MODULE SECTION.
      *************************************

           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "ASBWEX" TO VDU-FORM-NAME.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

           IF GROUP-FLAG = "N"
              MOVE BEG-BRANCH TO BR-NO
              PERFORM START-BR-FILE
              GO TO NEXT-BRANCH-NO.

           MOVE ENT-GROUP TO GR-ID.
           PERFORM SEARCH-GR-INIT.

      *----------------------------------------------------------------
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

      *----------------------------------------------------------------
       NEXT-BRANCH-NO.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-FG = 9 OR BR-NO > END-BRANCH
              GO TO END-ROUTINE.

      *----------------------------------------------------------------
       MOVE-BRANCH-PATH-INFO.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO BW-PATH-OVERRIDE.

      * IF NOT CORPORATE FORM PRINTING, SET EACH BRANCH FM PATH
           IF CORP-FM-FG = "N"
              MOVE "B"                     TO FM-PATH-OVERRIDE
           ELSE
      * IF CORPORATE PRINTING, USE CURRENT LOGIN BRANCH (EXTERNALS)
              MOVE EXT-FILPATH-BASE        TO FM-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO FM-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO FM-ALL-DATA
              MOVE FM-ALL-PATH             TO FM-PATH
              MOVE "Y"                     TO FM-PATH-OVERRIDE.


           IF EXT-ROUTE-BUF = "00"
              MOVE "BORROWER EXTRACTION IN PROGRESS BRANCH &&&&"
                 TO MESS
              INSPECT MESS REPLACING FIRST "&&&&" BY BR-NO
              MOVE MESS TO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           PERFORM OPEN-BW1-FILE.

           MOVE BW-PATH-OWNBR TO BW-OWNBR
                                 QBW1-WBEG-OWNBR
                                 QBW1-WEND-OWNBR.
           MOVE SSNO1         TO BW-SSNO
                                 QBW1-WBEG-SSNO.
           MOVE SSNO2         TO QBW1-WEND-SSNO.

           PERFORM START-BW1-FILE.
       NEXT-SSNO.
           PERFORM READ-BW1-FILE-NEXT.
           IF (IO-FG = 9) OR (BW-OWNBR NOT = BW-PATH-OWNBR) 
              GO TO END-OF-GROUP.

           IF BW-SSNO > SSNO2
              GO TO END-OF-GROUP.

      * 10/8/2007 PR# 557
           IF BW-STATE = "KS"
              GO TO NEXT-SSNO.

      * SKIP CONFIDENTIAL ACCOUNTS:
           IF BW-CONFIDENTIAL = "Y"
              GO TO NEXT-SSNO.

      * SKIP NON SOLICIT ACCOUNTS:
           IF BW-SOLICIT NOT = "Y"
              GO TO NEXT-SSNO.

      * TEST BRANCH RANGE:
           IF BW-OWNBR < BEG-BRANCH OR > END-BRANCH
              GO TO NEXT-SSNO.

      * TEST-STATE-CODE:
           MOVE BW-STATE TO MATCH1-CODE.
           MOVE STATE TO MATCH2-CODE.
           MOVE 2 TO MATCH-MAX.
           PERFORM TEST-FOR-MATCH.
           IF MATCH-OK NOT = "Y"
              GO TO NEXT-SSNO.

      * TEST-ZIP-CODE:
           IF BW-ZIP < BEGZIP
                         OR > ENDZIP
              GO TO NEXT-SSNO.

      * TEST-COUNTY-CODE:
           MOVE BW-COUNTY TO MATCH1-CODE.
           MOVE COUNTY TO MATCH2-CODE.
           MOVE 3 TO MATCH-MAX.
           PERFORM TEST-FOR-MATCH.
           IF MATCH-OK NOT = "Y"
              GO TO NEXT-SSNO.

      * TEST-DWELL-CODE:
           MOVE BW-DWELLCD TO MATCH1-CODE.
           MOVE DWELL TO MATCH2-CODE.
           MOVE 2 TO MATCH-MAX.
           PERFORM TEST-FOR-MATCH.
           IF MATCH-OK NOT = "Y"
              GO TO NEXT-SSNO.

      * TEST-CENSUS-CODE:
           MOVE BW-CENSUSCD TO MATCH1-CODE.
           MOVE CENSUS TO MATCH2-CODE.
           MOVE 10 TO MATCH-MAX.
           PERFORM TEST-FOR-MATCH.
           IF MATCH-OK NOT = "Y"
              GO TO NEXT-SSNO.

      * TEST-SMSA-CODE:
           MOVE BW-STRTGCD TO MATCH1-CODE.
           MOVE SMSA TO MATCH2-CODE.
           MOVE 6 TO MATCH-MAX.
           PERFORM TEST-FOR-MATCH.
           IF MATCH-OK NOT = "Y"
              GO TO NEXT-SSNO.

      * TEST-RENT-CODE:
           MOVE BW-RENTCD TO MATCH1-CODE.
           MOVE RENT TO MATCH2-CODE.
           MOVE 6 TO MATCH-MAX.
           PERFORM TEST-FOR-MATCH.
           IF MATCH-OK NOT = "Y"
              GO TO NEXT-SSNO.

      * COMPUTE NUMBER OF YEARS ELAPSED SINCE BW-REFDATE:
           MOVE BW-REFDATE TO NUM-DATE.
           MOVE TRANS-DATE TO SYS-DATE.
           PERFORM TIM360.
           DIVIDE 12 INTO ELAPSED-MONTHS GIVING REF-YEARS.

           IF REF-YEARS < 0
              MOVE 0 TO REF-YEARS.

      * TEST MINIMUM YEARS AT ADDRESS:
       TEST-BW-YR-ADDR.
           IF YRADDR = 0
              GO TO TEST-BW-AGE.

           MOVE BW-TIMEADDR TO MATCH1-CODE.
           MOVE 0 TO TIME-WORK.
           MOVE 4 TO SUB SUB1.
           PERFORM TEST-BW-YR-ADDR-MATCH 4 TIMES.
           IF TIME-ALP = "M"
              DIVIDE 12 INTO TIME-WORK
           ELSE
              IF TIME-ALP = "W"
                 DIVIDE 52 INTO TIME-WORK
              ELSE
                 IF TIME-ALP = "D"
                    DIVIDE 360 INTO TIME-WORK.

           IF (TIME-WORK + REF-YEARS) < YRADDR
              GO TO NEXT-SSNO.
           GO TO TEST-BW-AGE.

       TEST-BW-YR-ADDR-MATCH.
           IF MATCH1(SUB1) NUMERIC
              MOVE MATCH1(SUB1) TO TIME-WK(SUB)
              SUBTRACT 1 FROM SUB
           ELSE
              MOVE MATCH1(SUB1) TO TIME-ALP.
           SUBTRACT 1 FROM SUB1.

      * TEST BORROWER AGE:
       TEST-BW-AGE.
           IF MINAGE = 0 AND MAXAGE = 99
              GO TO TEST-BW-YR-EMPL.

           MOVE BW-BDATE   TO BAGE-DATE.
           MOVE BW-REFDATE TO BAGE-REFDATE.
           MOVE TRANS-DATE TO BAGE-TODAY.
           PERFORM BAGE-CALCULATION.

           IF BAGE-AGE < MINAGE OR > MAXAGE
              GO TO NEXT-SSNO.

      * TEST MINIMUM YEARS EMPLOYED:
       TEST-BW-YR-EMPL.
           IF (BW-EMPYEARS + REF-YEARS) < MINEMPL
              GO TO NEXT-SSNO.

      * TEST MINIMUM PAY PER MONTH:
           IF BW-EMPPAY < MINPAY
              GO TO NEXT-SSNO.

      * TEST MINIMUM TOTAL PAY PER MONTH:
           IF (BW-EMPPAY + BW-EMPPAYOT) < MINTPAY
              GO TO NEXT-SSNO.

      * TEST TOTAL LIABILITY:
           IF (BW-LIASMLN + BW-LIAOTH) < MINLIA OR > MAXLIA
              GO TO NEXT-SSNO.

      * TEST CREDIT SCORE
           IF BW-CREDIT-SCORE < BEG-CRSCORE OR > END-CRSCORE
              GO TO NEXT-SSNO.

      * TEST BORROWER ORIGIN (CASHCO PR# 2273)
           IF ORIGINX NOT = "????"
              MOVE BW-ORIGIN TO ORIGIN-WK
              PERFORM ORIGIN-CD-CHECK
              IF ORIGIN-MATCH NOT = "Y"
                 GO TO NEXT-SSNO.

      * TEST MINIMUM MONTHS SINCE LAST PAYOFF:
           IF MPO NOT = 99
              MOVE BW-LASTPODATE TO NUM-DATE
              MOVE TRANS-DATE    TO SYS-DATE
              PERFORM TIM360
              IF ELAPSED-MONTHS > MPO
                 GO TO NEXT-SSNO.

      * TEST SALESMAN RANGE:
           IF BW-ORGSLSMN < BEGSLSMN OR > ENDSLSMN
              GO TO NEXT-SSNO.

      * TEST REP RANGE:
           IF BW-REPCODE < BEGREP OR > ENDREP
              GO TO NEXT-SSNO.

      * TEST FOR MARRIED:
           IF MARRIED NOT = "I"
              IF (BW-SPSSNO = 0 AND BW-SPLNAME = SPACES
                                     AND BW-SPFNAME = SPACES)
                 IF MARRIED = "O"
                    GO TO NEXT-SSNO
                 ELSE
                    NEXT SENTENCE
              ELSE
                 IF MARRIED = "S"
                    GO TO NEXT-SSNO.

           ADD 1 TO BR-TOTAL-BORR
                    GR-TOTAL-BORR.
           IF REPT = "Y"
              PERFORM DETAIL-LINE-PROCESS.

           MOVE "N" TO FIRST-TIME.

           IF FORM NOT = 0
              PERFORM CREATE-FM-REC.

           GO TO NEXT-SSNO.

       END-OF-GROUP.
           PERFORM CLOSE-BW1-FILE.

           MOVE "TOTAL BORROWERS BR# &&&&:" TO DETAIL-LINE.
           INSPECT DETAIL-LINE REPLACING FIRST "&&&&" BY BR-NO.
           MOVE BR-TOTAL-BORR TO D-TOT-BORR.
           MOVE 3 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE 0 TO BR-TOTAL-BORR.
           MOVE 99 TO LN-COUNT.

           GO TO NEXT-GROUP-LOC.

       END-ROUTINE.
           IF FIRST-TIME = "Y"
              MOVE " NO RECORDS FOUND FOR SELECTED RANGE !" TO MESS
              PERFORM SEND-MESS
              MOVE SPACES TO PR-REC
              MOVE 99 TO LN-FEED
              PERFORM WRITE-PR-REC
              PERFORM PRINT-RANGE-LINES
              GO TO END-PROGRAM.

           MOVE "TOTAL BORROWERS SELECTED:" TO DETAIL-LINE.
           MOVE GR-TOTAL-BORR TO D-TOT-BORR.
           MOVE 3 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           PERFORM PRINT-RANGE-LINES.
           GO TO END-PROGRAM.

      *****************************************************
       INITIALIZATION SECTION.
      *****************************************************
           MOVE ASBWEX-QUEUE-POS TO Q-POS.
           MOVE ASBWEX-COPIES-POS TO C-POS.
           MOVE ASBWEX-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
       COPY "LIBGB/EOINIT.CPY".

       SET-HEAD.
           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO TRANS-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           IF EXT-ROUTE-BUF = "00"
              MOVE "BORROWER SOLICITATION RUN IN PROGRESS..." TO MESS
              PERFORM SEND-LEGEND.

           PERFORM OPEN-PR-FILE.
           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.

           GO TO EXIT-INITIALIZE.
       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.
       EXIT-INITIALIZE.
           EXIT.

      *********************************************
      *    SETUP REPORT DATES FOR BATCH
      *********************************************
       EO-EXEC-AUDIT SECTION.

      *********************************************
      *    ENTER REPORT/SELECTION FIELDS
      *********************************************
       ENTER-PARAMETERS SECTION.
       ENTER-PARM.
           IF VDUSTATUS = "1U"
              GO TO REC-REPT.

      *----------------------------------------------------------------
       REC-SSNO1.
           MOVE "ENTER BEGINNING SOC SEC, F3-ALL:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENNN090SSNO1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-SSNO.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-SSNO1.
           MOVE VDUNUM0 TO SSNO1.
       REC-SSNO2.
           MOVE "ENTER ENDING SOC SEC:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENNN090SSNO2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-SSNO1.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-SSNO2.
           MOVE VDUNUM0 TO SSNO2.
           IF SSNO2 = ZERO GO TO REC-SSNO2.
           IF SSNO1 > SSNO2
              GO TO REC-SSNO1.
           GO TO END-SSNO.

       ALL-SSNO.
           MOVE "SNNN090SSNO1." TO VDU.
           MOVE ZERO TO SSNO1 VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN090SSNO2." TO VDU.
           MOVE 999999999 TO SSNO2 VDUNUM0.
           PERFORM SCREENIO.
       END-SSNO.

       BR-01.
           MOVE "E  A040BR1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-SSNO1.
           IF VDUSTATUS = "16" GO TO ALL-BR.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
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
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO BR-01.

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
              MOVE 0 TO BEG-BRANCH
              MOVE 9999 TO END-BRANCH
              GO TO CHECK-BRANCH-SECURITY.

      *----------------------------------------------------------------
       BRANCH-IS-NUMERIC.
           MOVE "N"           TO GROUP-FLAG.
           MOVE WGR-SRESULT   TO BEG-BRANCH.
           MOVE "SN N040BR1." TO VDU.
           MOVE BEG-BRANCH    TO VDUNUM0.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
       BR-02.
           MOVE "ENNN040BR2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO BR-02.
           MOVE VDUNUM0 TO END-BRANCH.
           IF BEG-BRANCH > END-BRANCH
              MOVE "INVALID BRANCH RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO BR-01.
           GO TO CHECK-BRANCH-SECURITY.
       ALL-BR.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "N" TO GROUP-FLAG.
      *-----------------------------------------------------------------
       CHECK-BRANCH-SECURITY.
           PERFORM BR-RANGE-SECURITY.

           IF BR-SECURITY-OKAY = "N"
              GO TO BR-01.
      *----------------------------------------------------------------
       REC-STATE.
           MOVE "ENTER STATE CODE:   (?-WILD)" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E  A020STATE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-STATE.
           MOVE VDUBUF TO STATE.

       REC-BEGZIP.
           MOVE "ENTER BEGINNING ZIP CODE, F3-ALL:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E  A100ZIP1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-STATE.
           IF VDUSTATUS = "16" GO TO ALL-ZIP.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-BEGZIP.
           MOVE VDUBUF TO BEGZIP.

       REC-ENDZIP.
           MOVE "ENTER ENDING ZIP CODE:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E  A100ZIP2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-BEGZIP.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-ENDZIP.
           MOVE VDUBUF TO ENDZIP.
           IF ENDZIP = SPACES GO TO REC-ENDZIP.
           IF BEGZIP > ENDZIP
              GO TO REC-BEGZIP.
           GO TO END-ZIP.

       ALL-ZIP.
           MOVE "S  A100ZIP1." TO VDU.
           MOVE SPACES TO BEGZIP VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A100ZIP2." TO VDU.
           MOVE "ZZZZZZZZZZ" TO ENDZIP VDUBUF.
           PERFORM SCREENIO.
       END-ZIP.

       REC-COUNTY.
           MOVE "ENTER COUNTY CODE:    (?-WILD)" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E  A030COUNTY." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-BEGZIP.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-COUNTY.
           MOVE VDUBUF TO COUNTY.

       REC-DWELL.
           MOVE "ENTER DWELLING CODE:    (?-WILD)" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E  A020DWELL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-COUNTY.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-DWELL.
           MOVE VDUBUF TO DWELL.

       REC-CENSUS.
           MOVE "ENTER CENSUS CODE:    (?-WILD)" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E  A100CENSUS." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-DWELL.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-CENSUS.
           MOVE VDUBUF TO CENSUS.

       REC-SMSA.
           MOVE "ENTER SMSA CODE:    (?-WILD)" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E  A060SMSA." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-CENSUS.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-SMSA.
           MOVE VDUBUF TO SMSA.

       REC-RENT.
           MOVE "ENTER RENT CODE:    (?-WILD)" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E  A040RENT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-SMSA.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-RENT.
           MOVE VDUBUF TO RENT.

       REC-YRADDR.
           MOVE "ENTER MINIMUM YEARS AT ADDRESS:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENNN020YRADDR." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-RENT.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-YRADDR.
           MOVE VDUNUM0 TO YRADDR.

       REC-MINAGE.
           MOVE "ENTER BEGINNING BORROWER AGE, F3-ALL:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENNN020MINAGE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-YRADDR.
           IF VDUSTATUS = "16" GO TO ALL-AGE.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-MINAGE.
           MOVE VDUNUM0 TO MINAGE.
       REC-MAXAGE.
           MOVE "ENTER ENDING BORROWER AGE:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENNN020MAXAGE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-MINAGE.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-MAXAGE.
           MOVE VDUNUM0 TO MAXAGE.
           IF MAXAGE = ZERO GO TO REC-MAXAGE.
           IF MINAGE > MAXAGE
              GO TO REC-MINAGE.
           GO TO END-AGE.

       ALL-AGE.
           MOVE "SNNN020MINAGE." TO VDU.
           MOVE ZERO TO MINAGE VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020MAXAGE." TO VDU.
           MOVE 99 TO MAXAGE VDUNUM0.
           PERFORM SCREENIO.
       END-AGE.

       REC-MINEMPL.
           MOVE "ENTER MINIMUM YEARS EMPLOYED:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENNN020MINEMPL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-MINAGE.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-MINEMPL.
           MOVE VDUNUM0 TO MINEMPL.

       REC-MINPAY.
           MOVE "ENTER MINIMUM PAY PER MONTH:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENNN050MINPAY." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-MINEMPL.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-MINPAY.
           MOVE VDUNUM0 TO MINPAY.

       REC-MINTPAY.
           MOVE "ENTER MINIMUM TOTAL PAY PER MONTH:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENNN050MINTPAY." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-MINPAY.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-MINTPAY.
           MOVE VDUNUM0 TO MINTPAY.

       REC-MINLIA.
           MOVE "ENTER MINIMUM TOTAL LIABILITY, F3-ALL:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENNN060MINLIA." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-MINTPAY.
           IF VDUSTATUS = "16" GO TO ALL-LIA.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-MINLIA.
           MOVE VDUNUM0 TO MINLIA.
       REC-MAXLIA.
           MOVE "ENTER MAXIMUM TOTAL LIABILITY:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENNN060MAXLIA." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-MINLIA.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-MAXLIA.
           MOVE VDUNUM0 TO MAXLIA.
           IF MAXLIA = ZERO GO TO REC-MAXLIA.
           IF MINLIA > MAXLIA
              GO TO REC-MINLIA.
           GO TO END-LIA.

       ALL-LIA.
           MOVE "SNNN060MINLIA." TO VDU.
           MOVE ZERO TO MINLIA VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN060MAXLIA." TO VDU.
           MOVE 999999 TO MAXLIA VDUNUM0.
           PERFORM SCREENIO.
       END-LIA.

       REC-BEG-SCORE.
           MOVE "ENTER BEGINNING CREDIT SCORE, F3-ALL:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENNN030SCORE1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-MINLIA.
           IF VDUSTATUS = "16" GO TO ALL-SCORES.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-BEG-SCORE.
           MOVE VDUNUM0 TO BEG-CRSCORE.
       REC-END-SCORE.
           MOVE "ENTER ENDING CREDIT SCORE:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENNN030SCORE2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-BEG-SCORE.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-END-SCORE.
           MOVE VDUNUM0 TO END-CRSCORE.
           IF BEG-CRSCORE > END-CRSCORE
              GO TO REC-BEG-SCORE.
           GO TO END-SCORE.

       ALL-SCORES.
           MOVE "SNNN030SCORE1." TO VDU.
           MOVE 0 TO BEG-CRSCORE VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN030SCORE2." TO VDU.
           MOVE 999 TO END-CRSCORE VDUNUM0.
           PERFORM SCREENIO.

       END-SCORE.
       REC-MPO.
           MOVE "ENTER MIN MONTHS SINCE PAIDOFF, 99=SKIP:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENNN020MPO." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-BEG-SCORE.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-MPO.
           MOVE VDUNUM0 TO MPO.

       REC-BEGSLSMN.
           MOVE "ENTER BEGINNING SALESMAN, F3-ALL:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENNN040BEGSLSMN." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-MPO.
           IF VDUSTATUS = "16" GO TO ALL-SLSMN.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-BEGSLSMN.
           MOVE VDUNUM0 TO BEGSLSMN.
       REC-ENDSLSMN.
           MOVE "ENTER ENDING SALESMAN:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENNN040ENDSLSMN." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-BEGSLSMN.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-ENDSLSMN.
           MOVE VDUNUM0 TO ENDSLSMN.
           IF ENDSLSMN = 0
              GO TO REC-BEGSLSMN.
           IF BEGSLSMN > ENDSLSMN
              GO TO REC-BEGSLSMN.
           GO TO END-SLSMN.

       ALL-SLSMN.
           MOVE "SNNN040BEGSLSMN." TO VDU.
           MOVE 0 TO  BEGSLSMN VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040ENDSLSMN." TO VDU.
           MOVE 9999 TO ENDSLSMN VDUNUM0.
           PERFORM SCREENIO.
       END-SLSMN.

       REC-BEGREP.
           MOVE "ENTER BEGINNING REP CODE, F3-ALL:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E  A040BEGREP." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-BEGSLSMN.
           IF VDUSTATUS = "16" GO TO ALL-REP.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-BEGREP.
           MOVE VDUBUF TO BEGREP.
       REC-ENDREP.
           MOVE "ENTER ENDING REP CODE:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E  A040ENDREP." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-BEGREP.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-ENDREP.
           MOVE VDUBUF TO ENDREP.
           IF BEGREP > ENDREP
              GO TO REC-BEGREP.
           GO TO END-REP.

       ALL-REP.
           MOVE "S  A040BEGREP." TO VDU.
           MOVE SPACES TO BEGREP VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A040ENDREP." TO VDU.
           MOVE "ZZZZ" TO ENDREP VDUBUF.
           PERFORM SCREENIO.
       END-REP.

       REC-MARRIED.
           MOVE "MARRIED? ('I'-INCLUDE,'S'-SKIP,'O'-ONLY):"
                                                           TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E  A010MARRIED." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-BEGREP.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-MARRIED.
           MOVE VDUBUF TO MARRIED.
           IF NOT (MARRIED = "I" OR "S" OR "O")
              GO TO REC-MARRIED.

       REC-ORIGIN.
           MOVE "ENTER ORIGIN CODE:  (?-WILD)" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E  A040ORIGIN." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-ORIGIN.
           IF VDUSTATUS = "1U" GO TO REC-MARRIED.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO REC-ORIGIN.

           MOVE VDUBUF TO ORIGINX.
           GO TO REC-FORM.

       ALL-ORIGIN.
           MOVE "S  A040ORIGIN." TO VDU.
           MOVE "????" TO ORIGINX VDUBUF.
           PERFORM SCREENIO.

       REC-FORM.
           MOVE "ENTER FORM TO BE SENT:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENNN030FORM." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-ORIGIN.
           IF VDUSTATUS = "1<"
              PERFORM FORM-SCAN
              IF NOT IO-GOOD
                 GO TO REC-FORM
              ELSE
                 MOVE SEL-FMNO TO VDUNUM0
           ELSE
              IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-FORM.

           MOVE VDUNUM0 TO FORM FD-FMNO.
           IF FORM = 0
              GO TO REC-REPT.

           PERFORM OPEN-FD1-FILE.
           PERFORM READ-FD1-FILE.
           PERFORM CLOSE-FD1-FILE.
           IF NOT IO-GOOD
              GO TO REC-FORM
           ELSE
              PERFORM VALIDATE-FORM
              IF IO-FG NOT = 0
                 GO TO REC-FORM.


       CORP-FM.
      * IF BUILDING EXTRACTION FILE (LONPEX), MUST USE CORP PRINT FLAG
           IF ( FD-OPTION-EXTRACTION )
              MOVE "S  A010CORPFM." TO VDU
              MOVE "Y" TO CORP-FM-FG VDUBUF
              PERFORM SCREENIO
              GO TO EXIT-PARM.
           IF BEG-BRANCH = END-BRANCH
              MOVE "S  A010CORPFM." TO VDU
              MOVE "N" TO CORP-FM-FG VDUBUF
              PERFORM SCREENIO
              GO TO EXIT-PARM.
           MOVE "E  A010CORPFM." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-FORM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CORP-FM.
           MOVE VDUBUF TO CORP-FM-FG.
           IF CORP-FM-FG NOT = "Y"
              IF CORP-FM-FG NOT = "N"
                 MOVE "PLEASE ENTER 'Y' OR 'N'" TO MESS
                 PERFORM SEND-MESS
                 GO TO CORP-FM.

       REC-REPT.
           MOVE "INCLUDE SELECTED BORROWER REPORT:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E  A010REPT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO CORP-FM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-REPT.
           MOVE VDUBUF TO REPT.
           IF NOT (REPT = "Y" OR "N")
              GO TO REC-REPT.
           IF FORM = 0
              IF REPT = "N"
                 GO TO REC-REPT.

       EXIT-PARM.
           EXIT.

      ******************************
       VALIDATE-FORM SECTION.
      ******************************
           MOVE 0 TO IO-FG.

           IF FD-1STFILE NOT = 2
              MOVE "FORM 1ST FILE MUST BE BORROWER" TO MESS
              PERFORM SEND-MESS
              MOVE 9 TO IO-FG.

           IF FD-SECURITY-PROFILE NOT = " "
              IF EXT-CONAME-USER-PROFILE-CD > FD-SECURITY-PROFILE
                 MOVE "YOU ARE NOT AUTHORIZED TO REQUEST THIS FORM."
                    TO MESS
                 PERFORM SEND-MESS
                 MOVE 9 TO IO-FG.

           IF FD-VALID-BRANCH NOT = SPACES
              IF FD-VALID-BRANCH NUMERIC
                 IF EXT-CONAME-USER-LOC NOT = FD-VALID-BRANCH
                    MOVE "NOT A VALID BRANCH" TO MESS
                    PERFORM SEND-MESS
                    MOVE 9 TO IO-FG
                 ELSE
                    NEXT SENTENCE
              ELSE
                 MOVE FD-VALID-BRANCH TO GR-ID
                 PERFORM OPEN-GR1-FILE
                 PERFORM SEARCH-GR-INIT
                 MOVE EXT-CONAME-USER-LOC TO WGR-SRESULT
                 PERFORM SEARCH-GR-VERIFY
                 IF WGR-ERR > 0
                    MOVE "NOT A VALID BRANCH" TO MESS
                    PERFORM SEND-MESS
                    MOVE 9 TO IO-FG.

       EXIT-VALIDATE-FORM.
           EXIT.
      *********************************************
      *    DISPLAY REPORT PARAMETERS
      *********************************************
       DISPLAY-PARAMETERS SECTION.
       DISPLAY-PARM.

           IF GROUP-FLAG = "Y"
              MOVE "S  A000BR1." TO VDU
              MOVE ENT-GROUP     TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A000BR2." TO VDU
              MOVE SPACES        TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "SNNN040BR1." TO VDU
              MOVE BEG-BRANCH    TO VDUNUM0
              PERFORM SCREENIO
              MOVE "SNNN040BR2." TO VDU
              MOVE END-BRANCH    TO VDUNUM0
              PERFORM SCREENIO.

           MOVE "SNNN090SSNO1." TO VDU.
           MOVE SSNO1 TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN090SSNO2." TO VDU.
           MOVE SSNO2 TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN040BR1." TO VDU.
           MOVE BEG-BRANCH TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN040BR2." TO VDU.
           MOVE END-BRANCH TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "S  A020STATE." TO VDU.
           MOVE STATE TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A100ZIP1." TO VDU.
           MOVE BEGZIP TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A100ZIP2." TO VDU.
           MOVE ENDZIP TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A030COUNTY." TO VDU.
           MOVE COUNTY TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A020DWELL." TO VDU.
           MOVE DWELL TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A100CENSUS." TO VDU.
           MOVE CENSUS TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A060SMSA." TO VDU.
           MOVE SMSA TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A040RENT." TO VDU.
           MOVE RENT TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "SNNN020YRADDR." TO VDU.
           MOVE YRADDR TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN020MINAGE." TO VDU.
           MOVE MINAGE TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN020MAXAGE." TO VDU.
           MOVE MAXAGE TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN020MINEMPL." TO VDU.
           MOVE MINEMPL TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN050MINPAY." TO VDU.
           MOVE MINPAY TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN050MINTPAY." TO VDU.
           MOVE MINTPAY TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN060MINLIA." TO VDU.
           MOVE MINLIA TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN060MAXLIA." TO VDU.
           MOVE MAXLIA TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN030SCORE1." TO VDU.
           MOVE BEG-CRSCORE TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN030SCORE2." TO VDU.
           MOVE END-CRSCORE TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN020MPO." TO VDU.
           MOVE MPO TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN040BEGSLSMN." TO VDU.
           MOVE BEGSLSMN TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN040ENDSLSMN." TO VDU.
           MOVE ENDSLSMN TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "S  A040BEGREP." TO VDU.
           MOVE BEGREP TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A040ENDREP." TO VDU.
           MOVE ENDREP TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A010MARRIED." TO VDU.
           MOVE MARRIED TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A040ORIGIN." TO VDU.
           MOVE ORIGINX TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "SNNN030FORM." TO VDU.
           MOVE FORM TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "S  A010CORPFM." TO VDU.
           MOVE CORP-FM-FG TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A010REPT." TO VDU.
           MOVE REPT TO VDUBUF.
           PERFORM SCREENIO.

      /***************************
      *   DETAIL LINE PROCESS    *
      ****************************
       DETAIL-LINE-PROCESS SECTION.
           IF FIRST-TIME = "Y"
              MOVE EXT-CONAME-RPT-LINES TO LN-COUNT
              MOVE 1 TO LN-FEED.

           MOVE BR-NO TO D-BRNO.
      * BORROWER NAME:
           MOVE SPACES TO D-NAME.
           STRING BW-LNAME,", ",BW-FNAME DELIMITED BY "   "
                     INTO D-NAME.
      * SPOUSES NAME:
           IF BW-SPLNAME NOT = SPACES AND
               BW-SPFNAME NOT = SPACES
              MOVE SPACES TO D-SPNAME
              STRING BW-SPLNAME,", ",BW-SPFNAME DELIMITED BY "   "
                      INTO D-SPNAME.

           IF BW-PHONE NOT = 0
              MOVE BW-PHONE TO D-PHONE
              INSPECT D-PHONE REPLACING ALL "/" BY "-".

           MOVE BW-RENTCD           TO D-RENTCD.
           ADD REF-YEARS BW-EMPYEARS GIVING D-YREMP.

           IF BW-EMPPAY NOT = 0
              MOVE BW-EMPPAY        TO D-PAY.

           IF BW-EMPPAYOT NOT = 0
              MOVE BW-EMPPAYOT      TO D-PAYOT
              MOVE BW-EMPPAYSR      TO D-PAYSR.

           MOVE BW-LOANS            TO D-LOANS.
           ADD BW-LIASMLN BW-LIAOTH GIVING D-LIA.
           MOVE BW-LASTPODATE       TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY         TO D-LASTPOFF.
           PERFORM WRITE-DETAIL-LINE.
       END-DETAIL-LN-PROCESS.
           EXIT.

      /***************************
      *   CREATE FORM REQUEST
      ****************************
       CREATE-FM-REC SECTION.

           IF FORM = 0
              GO TO END-CREATE-FM.

           PERFORM OPEN-FM1-FILE.

           PERFORM CLEAR-FM-FILE.

           MOVE SPACES TO LPWSFM-EDIT-FM-INITKEY.


           MOVE BW-SSNO                TO LPWSFM-EDIT-FM-SSNO.
      * CUTSHEET PRINT WONT WORK IF BRNO2 IS FILLED IN !!! IF THEY USE
      * CUSTSHEET, DO NOT ANSWER "Y" TO CORPORATE PRINT !!
           IF CORP-FM-FG = "Y"
              MOVE BW-OWNBR            TO LPWSFM-EDIT-FM-BW-BRNO2.

           MOVE FM-PATH-OWNBR          TO FM-BRNO. 
           MOVE LPWSFM-EDIT-FM-INITKEY TO FM-INITKEY.
           MOVE FORM                   TO FM-FMNO.

           MOVE FM1-KEY TO HOLD-FM1-KEY.
           PERFORM READ-FM1-FILE.

           MOVE BW-ZIP             TO FM-ZIP.
           MOVE SPACES             TO FM-PRINT.
           MOVE 0                  TO FM-STMT-COUNTER.
           MOVE TRANS-DATE         TO FM-TRDATE.
           MOVE EXT-CONAME-USER-ID TO FM-USERID.

           IF IO-GOOD
              PERFORM REWRITE-FM1-FILE
           ELSE
              MOVE HOLD-FM1-KEY TO FM1-KEY
              PERFORM WRITE-FM1-FILE
              IF NOT IO-GOOD
                 GO TO IO-ERROR.

           PERFORM CLOSE-FM1-FILE.
       END-CREATE-FM.
           EXIT.

      **************************
       ORIGIN-CD-CHECK SECTION.
      **************************
           MOVE "Y" TO ORIGIN-MATCH.
           MOVE 0 TO ORIGIN-SUB.

       ORIGIN-NEXT-CHAR.
           ADD 1 TO ORIGIN-SUB.

           IF ORIGIN-CHAR(ORIGIN-SUB) NOT = ORIGIN-CD(ORIGIN-SUB) AND
              ORIGIN-CD(ORIGIN-SUB) NOT = "?"
              MOVE "N" TO ORIGIN-MATCH
              GO TO ORIGIN-CD-CHECK-EXIT.

           IF ORIGIN-SUB < 4
              GO TO ORIGIN-NEXT-CHAR.

       ORIGIN-CD-CHECK-EXIT.
           EXIT.

      /******************************
      *        HEAD PAGE ROUTINE
      *******************************
       HEAD-PAGE SECTION.
           ADD 1 TO PAGE-NUMBER.
           MOVE PAGE-NUMBER TO H-PAGE-NO.
           PERFORM GET-TIME.
           MOVE T-HH TO H-HR.
           MOVE T-MM TO H-MIN.
           MOVE HEAD1 TO PR-REC.
           MOVE 99 TO LN-FEED.
           PERFORM WRITE-PR-REC.
           MOVE 1 TO LN-FEED.
           MOVE HEAD2 TO PR-REC.
           PERFORM WRITE-PR-REC.
           MOVE 2 TO LN-FEED.
           MOVE HEAD3 TO PR-REC.
           PERFORM WRITE-PR-REC.
           MOVE 1 TO LN-FEED.
           MOVE HEAD4 TO PR-REC.
           PERFORM WRITE-PR-REC.
           MOVE 7 TO LN-COUNT
           MOVE 2 TO LN-FEED.
       END-HEAD-PAGE.
           EXIT.

      ****************************
      *    WRITE DETAIL LINE     *
      ****************************
       WRITE-DETAIL-LINE SECTION.
           ADD LN-FEED TO LN-COUNT.
           IF LN-COUNT > EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.
           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      *******************************
      *       RANGE LINES           *
      *******************************
       PRINT-RANGE-LINES SECTION.
           MOVE 4 TO LN-FEED.

           MOVE "BEGINNING SSNO              :" TO RANGE-LINE.
           MOVE SSNO1                           TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING                      :" TO RANGE-LINE.
           MOVE SSNO2                           TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           IF GROUP-FLAG = "Y"
              MOVE "GROUP                       :" TO RANGE-LINE
              MOVE ENT-GROUP                       TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE
           ELSE
              MOVE "BEGINNING BRANCH            :" TO RANGE-LINE
              MOVE BEG-BRANCH                      TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE
              MOVE "ENDING                      :" TO RANGE-LINE
              MOVE END-BRANCH                      TO RANGE-SELECTION.

           MOVE "STATE                       :" TO RANGE-LINE.
           MOVE STATE                           TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEGIN ZIP CODE              :" TO RANGE-LINE.
           MOVE BEGZIP                          TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING ZIP CODE             :" TO RANGE-LINE.
           MOVE ENDZIP                          TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "COUNTY CODE                 :" TO RANGE-LINE.
           MOVE COUNTY                          TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "DWELLING CODE               :" TO RANGE-LINE.
           MOVE DWELL                           TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "CENSUS TRACK CODE           :" TO RANGE-LINE.
           MOVE CENSUS                          TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "S.M.S.A. CODE               :" TO RANGE-LINE.
           MOVE SMSA                            TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "RENT / OWN CODE             :" TO RANGE-LINE.
           MOVE RENT                            TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "MINIMUM YEARS ADDRESS       :" TO RANGE-LINE.
           MOVE YRADDR                          TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "MINIMUM BORROWER AGE        :" TO RANGE-LINE.
           MOVE MINAGE                          TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "MAXIMUM BORROWER AGE        :" TO RANGE-LINE.
           MOVE MAXAGE                          TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "MINIMUM YEARS EMPLOYED      :" TO RANGE-LINE.
           MOVE MINEMPL                         TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "MINIMUM PAY/MONTH           :" TO RANGE-LINE.
           MOVE MINPAY                          TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "MINIMUM TOTAL PAY/MONTH     :" TO RANGE-LINE.
           MOVE MINTPAY                         TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "MINIMUM TOTAL LIABILITY     :" TO RANGE-LINE.
           MOVE MINLIA                          TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "MAXIMUM TOTAL LIABILITY     :" TO RANGE-LINE.
           MOVE MAXLIA                          TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEGINNING CREDIT SCORE      :" TO RANGE-LINE.
           MOVE BEG-CRSCORE                     TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "ENDING    CREDIT SCORE      :" TO RANGE-LINE.
           MOVE END-CRSCORE                     TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "MIN MONTHS SINCE PAYOFF     :" TO RANGE-LINE.
           MOVE MPO                             TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEGINNING SALESMAN          :" TO RANGE-LINE.
           MOVE BEGSLSMN                       TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING SALESMAN             :" TO RANGE-LINE.
           MOVE ENDSLSMN                       TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEGINNING SALES REP         :" TO RANGE-LINE.
           MOVE BEGREP                          TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING SALES REP            :" TO RANGE-LINE.
           MOVE ENDREP                         TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "MARRIED   (I,S,O)           :" TO RANGE-LINE.
           MOVE MARRIED                        TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ORIGIN                      :" TO RANGE-LINE.
           MOVE ORIGINX                         TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "FORM REQUEST NO             :" TO RANGE-LINE.
           MOVE FORM                           TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "CORPORATE FORM PRINTING     :" TO RANGE-LINE.
           MOVE CORP-FM-FG                      TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "REPORT (Y/N)                :" TO RANGE-LINE.
           MOVE REPT                           TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

       END-RANGE.
           EXIT.

       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/BRSECURE.CPY".
       COPY "LIBLP/LPBAGE.CPY".
       COPY "LIBLP/LPFMCL.CPY".

      ********************************************
      *    TEST FOR MATCH USING WILD CARDS
      *
      *    MATCH1-CODE - FIELD TO BE TESTED
      *    MATCH2-CODE - FIELD CONTAINING WILD CARDS
      *    MATCH-MAX   - SIZE OF FIELD TO BE TESTED
      *
      *    MATCH-OK    - 'Y' SUCESSFUL MATCH
      ********************************************
       TEST-FOR-MATCH SECTION.
           MOVE "Y" TO MATCH-OK.
           MOVE 1 TO SUB.
       TEST-FOR-MATCH-AGAIN.
           IF MATCH2(SUB) NOT = "?"
              IF MATCH2(SUB) NOT = MATCH1(SUB)
                 MOVE " " TO MATCH-OK
                 GO TO EXIT-TEST-FOR-MATCH.
           IF SUB < MATCH-MAX
              ADD 1 TO SUB
              GO TO TEST-FOR-MATCH-AGAIN.
       EXIT-TEST-FOR-MATCH.
           EXIT.

      ******************************************
       FORM-SCAN SECTION.
      ******************************************
           MOVE 0   TO FDSCAN-KEY.
           MOVE "C" TO FDSCAN-FMTYPE.
           MOVE "S" TO FDSCAN-ALLOW-CHG.
           MOVE "WI/FDSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH FDSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF FDSCAN-STATUS NOT = "00" OR FDSCAN-KEY = " "
               MOVE 9 TO IO-FG
               GO TO EXIT-FORM-SCAN.
           MOVE "S  A030FORM." TO VDU.
           MOVE FDSCAN-KEY-ALP TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "00" TO VDUSTATUS.
           MOVE FDSCAN-KEY-ALP TO SEL-FMNOX.

       EXIT-FORM-SCAN.
           EXIT.

      *********************************************
       FORM-RESET SECTION.
      *********************************************
           MOVE EXT-CONAME-CONAME TO ASBWEX-CONAME.
           MOVE EXT-CONAME-SYSDATE TO ASBWEX-SYSDATE.
           DISPLAY ASBWEX-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE ASBWEX-FORM-FIELDS TO WR-BUF.
           MOVE ASBWEX-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.


      *********************************************
       SETUP-LINK-INFO SECTION.
      *********************************************
           MOVE ASBWEX-HELP--FILE TO HELP-LINK-FILE.
           MOVE ASBWEX-HELP--DIR  TO HELP-LINK-DIR.

      *********************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/ASBWEX_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ************************************************
      *    MISC ROUTINES SECTION
      ************************************************
       MISC SECTION.
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       SEND-LEGEND.
           MOVE MESS TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       CLEAR-LEGEND.
           MOVE SPACES TO MESS.
           PERFORM SEND-LEGEND.

      ************************************
       IO-ROUTINE SECTION.
      ************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPFMGS_SQL.CPY".
       COPY "LIBLP/LPFDGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPFM1IN.CPY".
       COPY "LIBLP/LPFD1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      ************************************
       END-PROGRAM SECTION.
      ************************************
       END-RUN.
           PERFORM CLOSE-FILES.
           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.
       EXIT-PROG.
           IF SV-BEG-BR NOT = 9999
              IF EXT-EOBUF-GROUP-FG = "Y"
                 MOVE SV-BEG-BR TO BEG-BRANCH
                 MOVE SV-END-BR TO END-BRANCH
                 MOVE SV-GROUP-FLAG TO GROUP-FLAG
                 MOVE SV-ENT-GROUP TO ENT-GROUP
              ELSE
                 MOVE SV-BEG-BR TO BEG-BRANCH
                 MOVE SV-END-BR TO END-BRANCH.
           IF EXT-ROUTE-BUF = "00"
              MOVE "LP/ASMENU" TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY ASBWEX-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
