       IDENTIFICATION DIVISION.
       PROGRAM-ID.      BWEXTR.
       DATE-WRITTEN.    121201.
      **************************************************************************
      *                      (SP)
      *    DESCRIPTION:  DATA WAREHOUSE BORROWER EXTRACTION
      *
      * CREATES AN OUTPUT EXTRACT FILE  /USR/EXTRACT/BORROWER
      *
      * RSZ 1430 INCLUDING CR,LF
      *   CB.FXA SP BWEXTR    TO GET THE XFD LAYOUTS
      *
      * REV:
      *     011212 REGIONAL ACCEPTANCE, PR 1675
      * BLV 021025 FIXED MISC. PHONE TABLE, MOVES WERE MESSED UP!
      * BAH 030728 ADDED XBW-BAD-PHONE-FG AND XBW-BAD-EMMPHONE-FG REGACC PR# 36
      * BAH 040428 ADDED BORROWER TRAILER FIELDS, BXFILE, LENDMARK PR# 2292
      *  CS 040907 ADD BR-REGION-NUMBER TO XBW OUTPUT FILE [PR#76 REGACC]
      * BAH 060203 BIRTHDATE WAS GETTING 2043 AND 2042, CAN NOT PERFORM
      *            MMMDDYY-TO-YYYYMMDD, USED 50 AS CUTOFF DATE, DO MANUALLY
      *  BAH 060802 ADDED XBW-BAD-MISC-PHONE1-FG, PHONE2-FG, REGACC PR# 142
      * BAH 080218 ADDED XBW-CITY-TAX-CODE FOR LENDMARK PR# 3155
      * BAH 090903 ADDED XBW-CELL-PHONE  PL# 664
      * BAH 100608 ADDED BW-BAD-CELL-FG FOR REGACC PR# 266
      * BAH 150615 EXPANDED XBW-KEY TO 9(9) FROM 9(7), WORLD PR# 1088
      * BAH 160201 ADDED CUSTOMER INFO FILE I/S/O, WORLD PR# 1134
      * BAH 170808 ACTIVATE 8 DIGIT DATES, PD0006
      *  CS 180504 ADD OPTION FOR CUSTOMER INFO FILE I/S/O ITS IN A15,
      *            CHANGED FILLER ON RANGE-SELNUM REDEFINE, TOO LONG BY 1
      *  CS 180529 FIXED XCI-ACCTNO TO INCLUDE THE 01SEQ TO MATCH A15
      * BAH 180809 XCI-DATE-AT-PRESENT-ADR WAS YYYYMMDD WHICH IS CORRECT,
      *            HOWEVER, A15 WAS WRONG AND IS SENDING MMDDYYYY SO A30
      *            NEEDS TO MATCH AS MMDDYYYY
      * BAH 20190722 WAS NOT PRINTING "# CUSTINFO RECORDS :" IN RANGE LINES
      *              LIKE A15, QA0060
      * BAH 20200827 REMOVED RESET ON FIRST FIELD
      * RMZ 20231002 PERFORMANCE TUNING - ADDED STORED PROCEDURE
      * BAH 2024.1111 ADDED BW-CEASE-DESIST-DATE BW-CEASE-DESIST-TYPE #1686
      *               AND BW-DEALTRACK-CHG-DATE WAS MISSING  S35Q-145
      * BAH 2025.0113 ADDED BW-UMC-EXPIRE-DATE AND BW-UMC-PR-EXPIRE-DATE #1687
      *                                                      S35Q-146
      **************************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
       COPY "LIBEX/EXFSXBW.CPY".
       COPY "LIBEX/EXFSXCI.CPY".
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".
       COPY "LIBEX/EXFDXBW.CPY".
       COPY "LIBEX/EXFDXCI.CPY".

      ******************************************************************
      *
      *     W O R K I N G - S T O R A G E   S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)SP/BWEXTR.CBL S35 03/28/24-09:21:26 >".

      ******************************************************************
      *
      *     F I L E   S P E C S
      *
      ******************************************************************
       01  XBW-PATH.
           03  XBW-DIRECTORY      PIC X(13) VALUE "/USR/EXTRACT/".
           03  XBW-FILENAME       PIC X(8) VALUE "BORROWER".

       01  XCI-PATH.
           03  XCI-DIRECTORY      PIC X(13) VALUE "/USR/EXTRACT/".
           03  XCI-FILENAME       PIC X(8)  VALUE "CUSTINFO".
            
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01BX.CPY".
       COPY "LIBLP/LP01BX_SQL.CPY".
       COPY "LIBLP/LPWSBX.CPY".
       COPY "LIBLP/LP01CI.CPY".
       COPY "LIBLP/LP01CI_SQL.CPY".
       COPY "LIBLP/LPWSCI.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
      *----------------------------------------------------------------
       01  XBW-KEY       PIC 9(9)     VALUE 0.
       01  XCI-KEY       PIC 9(9)     VALUE 0.

       01  BW-CONN-OPEN-SW      PIC X    VALUE "N".
       01  CI-CONN-OPEN-SW      PIC X    VALUE "N".

      ******************************************************************
      *
      *   LINE WORKERS
      *
      ******************************************************************
       01  LN-WORKERS.
           03  LN-FEED          PIC 99       VALUE 0.
           03  LN-COUNT         PIC 99       VALUE 61.
           03  LN-WK            PIC 99       VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

       01  STARTING-TIME        PIC X(8)     VALUE SPACES.

      ******************************************************************
      *
      *     S C R E E N   F I E L D S
      *
      ******************************************************************
       COPY "LIBGB/EOBUF_EXT.CPY".
      *-----------------------------------------------------------------
           03  BEG-BRANCH             PIC 9(4).
           03  END-BRANCH             PIC 9(4).
           03  GROUP-FLAG             PIC X.
           03  ENT-GROUP              PIC X(4).
           03  CUST-INFO              PIC X.

       01  VERSION-CONTROL     PIC X    VALUE "B".

       01  SV-BEG-BR           PIC 9999 VALUE 9999.
       01  SV-END-BR           PIC 9999 VALUE 9999.
       01  SV-GROUP-FLAG       PIC X     VALUE SPACES.
       01  SV-ENT-GROUP        PIC X(4)  VALUE SPACES.

      ******************************************************************
      *
      *     P R I N T   B U F F E R S
      *
      ******************************************************************
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

      *-----------------------------------------------------------------
       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(26)    VALUE " ".
           03  H-TITLE          PIC X(24)    VALUE
           "BORROWER FILE EXTRACTION".

      **********************
      *   MISC WORKERS     *
      **********************
       01  SUB                  PIC 9(6)     COMP VALUE 0.
       01  RECORDS-FOUND        PIC X        VALUE " ".
           88  NO-RECORDS-FOUND              VALUE "N".
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".
       01  WK-DISPLAY-MESS.
           03  FILLER           PIC X(29)    VALUE
           "BUILDING EXTRACTION FILE....".
           03  DISPLAY-MESS-BR  PIC 9(4).
           03  FILLER           PIC X(10).

       01  RECORD-COUNT           PIC 9(9)      VALUE 0.
       01  CI-RECORD-COUNT        PIC 9(9)      VALUE 0.

       01  DETAIL-LINE          PIC X(132) VALUE SPACES.

       01  RANGE-LN REDEFINES DETAIL-LINE.
           03  RANGE-LINE        PIC X(21).
           03  RANGE-SELECTION   PIC X(111).

       01  WS-A15-ACCTNO.
           03  WS-A15-NUMBER     PIC 9(6).
           03  WS-A15-SEQNO      PIC 9(2).

       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/BWEXTR_DEF.CPY".
       COPY "LIBSP/BWEXTR_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBSP/BWEXTR_SCN.CPY".

      *-----------------------------------------------------------------
       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE XBW-FILE XCI-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-CI1-FILE.
           PERFORM CLOSE-CI1-CONNECTION.
           PERFORM CLOSE-BX1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-BW1-CONNECTION.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE
               XBW-FILE XCI-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ******************************************************************
      *
      *    M A I N   M O D U L E
      *
      ******************************************************************
       MAIN-MODULE SECTION.
           PERFORM SQL-CONNECT.
      D    STOP MESS.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "BWEXTR" TO FORM-PROG VDU-FORM-NAME.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

           IF GROUP-FLAG = "N"
              MOVE BEG-BRANCH TO BR-NO
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
           IF IO-FG = 9 OR BR-NO > END-BRANCH
              GO TO END-ROUTINE.

       MOVE-BRANCH-PATH-INFO.
           MOVE EXT-FILPATH-BASE TO FILPATH-BASE.
           MOVE BR-MACHINE       TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH     TO FILPATH-DATA.

           MOVE "B" TO BW-PATH-OVERRIDE
                       BX-PATH-OVERRIDE 
                       CI-PATH-OVERRIDE.

           IF EXT-ROUTE-BUF = "00"
              MOVE SPACES TO WR-BUF
              STRING "EXTRACTION IN PROGRESS.......", BR-NO
                     DELIMITED "  " INTO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           IF PRU-LP NOT = "VDU" AND NOT = "PRU"
              IF EXT-ROUTE-BUF = "00"
                 MOVE BR-NO TO DISPLAY-MESS-BR
                 MOVE WK-DISPLAY-MESS TO MESS
                 PERFORM SEND-LEGEND.

           IF CUST-INFO NOT = "O"
              PERFORM BORROWER-EXTRACT.

           IF CUST-INFO NOT = "S"
              PERFORM CUSTOMER-INFO-EXTRACT.

       END-OF-GROUP.
           GO TO NEXT-GROUP-LOC.


       END-ROUTINE.
           IF NO-RECORDS-FOUND
              GO TO END-PROGRAM.

           PERFORM HEAD-PAGE.
           PERFORM PRINT-RANGE-LINES.

           GO TO END-PROGRAM.

      *******************************************
       BORROWER-EXTRACT SECTION.
      *******************************************
           PERFORM OPEN-BW1-FILE.
           PERFORM OPEN-BX1-FILE.

           MOVE BW-PATH-OWNBR TO BW-OWNBR.
           MOVE 0             TO BW-SSNO.

           MOVE BW-PATH-OWNBR TO BW-OWNBR.
           PERFORM START-BW1-FILE. *> SSP - BW,BX

      *----------------------------------------------------------------
       NEXT-BW.
           PERFORM READ-BW1-FILE-NEXT. *> EMBEDDED - BW,BX
           IF IO-FG = 9 OR (BW-OWNBR NOT = BW-PATH-OWNBR)
              GO TO END-BORROWER-EXTRACT.

       FILL-WORK-FLDS.
           MOVE SPACES              TO XBW-REC.

           MOVE BR-REGION-NUMBER    TO XBW-REGION-NUMBER.
           MOVE BR-NO               TO XBW-BRNO.
           MOVE BW-SSNO             TO XBW-SSNO.
           MOVE BW-LNAME            TO XBW-LNAME.
           MOVE BW-FNAME            TO XBW-FNAME.
           MOVE BW-ADR1             TO XBW-ADR1.
           MOVE BW-ADR2             TO XBW-ADR2.
           MOVE BW-CITY             TO XBW-CITY.
           MOVE BW-STATE            TO XBW-STATE.
           MOVE BW-ZIP              TO XBW-ZIP.
           MOVE BW-REFDATE          TO XBW-REFDATE.
           MOVE BW-BDATE            TO XBW-AGE.

           MOVE BW-PHONE            TO XBW-PHONE.
           MOVE BW-DEPEND           TO XBW-DEPEND.
           MOVE BW-RENTCD           TO XBW-RENTCD.
           MOVE BW-TIMEADDR         TO XBW-TIMEADDR.
           MOVE BW-CREDITCD         TO XBW-CREDITCD.
           MOVE BW-SOLICIT          TO XBW-SOLICIT.
           MOVE BW-STRTGCD          TO XBW-STRTGCD.
           MOVE BW-DWELLCD          TO XBW-DWELLCD.
           MOVE BW-COUNTY           TO XBW-COUNTY.
           MOVE BW-CENSUSCD         TO XBW-CENSUSCD.
           MOVE BW-ORGSLSMN         TO XBW-ORGSLSMN.
           MOVE BW-REPCODE          TO XBW-REPCODE.
           MOVE BW-SPSSNO           TO XBW-SPSSNO.
           MOVE BW-SPLNAME          TO XBW-SPLNAME.
           MOVE BW-SPFNAME          TO XBW-SPFNAME.
           MOVE BW-EMPLOYER         TO XBW-EMPLOYER.
           MOVE BW-EMPLPOS          TO XBW-EMPLPOS.
           MOVE BW-EMPPHONE         TO XBW-EMPPHONE.
           MOVE BW-EMPEXT           TO XBW-EMPEXT.
           MOVE BW-EMPYEARS         TO XBW-EMPYEARS.
           MOVE BW-EMPPAY           TO XBW-EMPPAY.
           MOVE BW-EMPPAYOT         TO XBW-EMPPAYOT.
           MOVE BW-EMPPAYSR         TO XBW-EMPPAYSR.
           MOVE BW-EMPADDR1         TO XBW-EMPADDR1.
           MOVE BW-EMPADDR2         TO XBW-EMPADDR2.
           MOVE BW-LOANS            TO XBW-LOANS.
           MOVE BW-INTYTD           TO XBW-INTYTD.
           INSPECT XBW-INTYTD REPLACING LEADING "+" BY " ".
           MOVE BW-INTLYR           TO XBW-INTLYR.
           INSPECT XBW-INTLYR REPLACING LEADING "+" BY " ".
           MOVE BW-LIASMLN          TO XBW-LIASMLN.
           INSPECT XBW-LIASMLN REPLACING LEADING "+" BY " ".
           MOVE BW-LIAOTH           TO XBW-LIAOTH.
           INSPECT XBW-LIAOTH REPLACING LEADING "+" BY " ".

           MOVE BW-LASTPODATE       TO XBW-LSTPODATE.
           MOVE BW-CONFIDENTIAL     TO XBW-CONFIDENTIAL.
           MOVE BW-SP-BDATE         TO XBW-SPAGE.

           MOVE BW-ORGBR            TO XBW-BRANCH.
           MOVE BW-ORIGIN           TO XBW-ORIGIN.
           MOVE BW-WORKHRS          TO XBW-WORKHRS.
           MOVE BW-PRLOANS          TO XBW-PRLOANS.
           MOVE BW-RE-INTYTD        TO XBW-RE-INTYTD.
           INSPECT XBW-RE-INTYTD REPLACING LEADING "+" BY " ".
           MOVE BW-RE-INTLYR        TO XBW-RE-INTLYR.
           INSPECT XBW-RE-INTLYR REPLACING LEADING "+" BY " ".
           MOVE BW-CREDIT-SCORE     TO XBW-CREDIT-SCORE.
           MOVE BW-LAST-FORM-NO     TO XBW-LAST-FORM-NO.

           MOVE BW-LAST-FORM-DATE      TO XBW-LAST-FORM-DATE.
           MOVE BW-PRIOR-RN-FOR10-DATE TO XBW-PRIOR-RN-FOR10.
           MOVE BW-LAST-RN-FOR10-DATE  TO XBW-LAST-RN-FOR10.
           MOVE BW-SRCD             TO XBW-SRCD.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
              MOVE BW-MISC-PHONE(SUB)    TO XBW-MISC-PHONE(SUB)
              MOVE BW-MISC-EXT(SUB)      TO XBW-MISC-EXT(SUB)
              MOVE BW-MISC-PHONE-CD(SUB) TO XBW-MISC-PHONE-CD(SUB)
           END-PERFORM.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 10
              MOVE BW-CONTACT-DESC(SUB)  TO XBW-CONTACT-DESC(SUB)
              MOVE BW-CONTACT-PHONE(SUB) TO XBW-CONTACT-PHONE(SUB)
              MOVE BW-CONTACT-EXT(SUB)   TO XBW-CONTACT-EXT(SUB)
           END-PERFORM.

           MOVE BW-CREDIT-LIMIT     TO XBW-CREDIT-LIMIT.
           MOVE BW-PRIVACY-OPT-OUT  TO XBW-PRIVACY-OPT-OUT.

           MOVE BW-BAD-PHONE-FG     TO XBW-BAD-PHONE-FG.
           MOVE BW-BAD-EMPPHONE-FG  TO XBW-BAD-EMPPHONE-FG.
           MOVE BW-BAD-ADR-FG       TO  XBW-BAD-ADR-FG.
           MOVE BW-CALL-ON-SUNDAY   TO  XBW-CALL-ON-SUNDAY.

           MOVE BW-EMAIL TO XBX-EMAIL.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 4
               MOVE BW-CRED-SCORE(SUB)   TO XBX-CREDIT-SCORE(SUB)
               MOVE BW-BNKRPT-SCORE(SUB) TO XBX-BNKRPT-SCORE(SUB)
           END-PERFORM.

           MOVE BW-SSNO TO BX-SSNO.
      *    PERFORM READ-BX1-FILE.
      *    IF IO-FG NOT = 0
           IF BX-SSNO   = 0
              PERFORM CLEAR-BX-FILE.

           MOVE BX-ALT-ADR1 TO XBX-ALT-ADR1.
           MOVE BX-ALT-ADR2 TO XBX-ALT-ADR2.
           MOVE BX-ALT-CITY TO XBX-ALT-CITY.
           MOVE BX-ALT-STATE TO XBX-ALT-STATE.
           MOVE BX-ALT-ZIP TO XBX-ALT-ZIP.

           MOVE BW-BAD-MISC-PHONE1-FG TO XBW-BAD-MISC-PHONE1-FG.
           MOVE BW-BAD-MISC-PHONE2-FG TO XBW-BAD-MISC-PHONE2-FG.

           MOVE BW-CITY-TAX-CD TO XBW-CITY-TAX-CD.
           MOVE BW-CELL-PHONE TO XBW-CELL-PHONE.
           MOVE BW-BAD-CELL-FG TO XBW-BAD-CELL-FG.

           MOVE BW-DEALTRACK-CHG-DATE TO XBW-DEALTRACK-CHG-DATE.
           MOVE BW-CEASE-DESIST-DATE  TO XBW-CEASE-DESIST-DATE.
           MOVE BW-CEASE-DESIST-TYPE  TO XBW-CEASE-DESIST-TYPE.
           MOVE BW-UMC-EXPIRE-DATE    TO XBW-UMC-EXPIRE-DATE.
           MOVE BW-UMC-PR-EXPIRE-DATE TO XBW-UMC-PR-EXPIRE-DATE.

           MOVE X"0D" TO XBW-CR.
           MOVE X"0A" TO XBW-LF.

           ADD 1 TO XBW-KEY.
           PERFORM WRITE-XBW-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

           ADD 1 TO RECORD-COUNT.

           MOVE "Y" TO RECORDS-FOUND.

           GO TO NEXT-BW.

       END-BORROWER-EXTRACT.
      D    STOP MESS.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-BX1-FILE.

       EXIT-BORROWER-EXTRACT.
           EXIT.

      *******************************************
       CUSTOMER-INFO-EXTRACT SECTION.
      *******************************************
           PERFORM OPEN-CI1-FILE.

           MOVE CI-PATH-OWNBR TO CI-BRNO.
           MOVE 0             TO CI-ACCTNO.

           PERFORM START-CI1-FILE. *> SSP - CIFILE

       NEXT-CI.

           PERFORM READ-CI1-FILE-NEXT. *> EMBEDDED - CIFILE
           IF IO-FG = 9 OR (CI-BRNO NOT = CI-PATH-OWNBR)
              GO TO EXIT-EXTRACT-CUSTOMER-INFO.

           MOVE SPACES TO XCI-REC.

           MOVE BR-NO               TO XCI-BRNO.
      *NEEDS THE 01 SEQ TO MATCH A15
      *    MOVE CI-ACCTNO           TO XCI-ACCTNO.
           MOVE CI-ACCTNO           TO WS-A15-NUMBER.
           MOVE 01                  TO WS-A15-SEQNO.
           MOVE WS-A15-ACCTNO       TO XCI-ACCTNO.

           MOVE CI-SSNO             TO XCI-SSNO.
           MOVE CI-SSNO-2           TO XCI-SSNO-2.
      * ALREADY 8 DIGIT DATE
      *    MOVE CI-DATE-UPDATED     TO DATE-MMDDYYYY.
      *    PERFORM CONVERT-MMDDYYYY-TO-YYYYMMDD.
      *    MOVE DATE-YYYYMMDD    TO XCI-DATE-UPDATED.
           MOVE CI-DATE-UPDATED  TO XCI-DATE-UPDATED.

           MOVE CI-AMT-REQUESTED    TO XCI-AMT-REQUESTED.
           MOVE CI-MARITAL-STAT     TO XCI-MARITAL-STAT.
           MOVE CI-YEARS-IN-AREA    TO XCI-YEARS-IN-AREA.

      * A15 HAS THIS DATE AS MMDDYYYY AND WE NEED A30 TO MATCH IT,
      * EVEN THOUGH IT IS WRONG. KEVIN SAID TO DO THIS.
      *    MOVE CI-DATE-AT-PRESENT-ADR TO XCI-DATE-AT-PRESENT-ADR.
           MOVE CI-DATE-AT-PRESENT-ADR TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY TO XCI-DATE-AT-PRESENT-ADR.
           MOVE CI-MONTHLY-RENT-MTG TO XCI-MONTHLY-RENT-MTG.
           MOVE CI-FAIR-MARKET-VALUE   TO XCI-FAIR-MARKET-VALUE.
           MOVE CI-MTG-BALANCE      TO XCI-MTG-BALANCE.
           MOVE CI-CHECKING-ACCOUNT TO XCI-CHECKING-ACCOUNT.
           MOVE CI-SAVINGS-ACCOUNT  TO XCI-SAVINGS-ACCOUNT.
           MOVE CI-TOTAL-MO-INCOME  TO XCI-TOTAL-MO-INCOME.
           MOVE CI-LESS-MO-PAY      TO XCI-LESS-MO-PAY.

      * ALREADY 8 DIGIT DATE
      *    MOVE CI-DATE-HIRE-1 TO DATE-MMDDYYYY.
      *    PERFORM CONVERT-MMDDYYYY-TO-YYYYMMDD.
      *    MOVE DATE-YYYYMMDD    TO XCI-DATE-HIRE-1.
           MOVE CI-DATE-HIRE-1 TO   XCI-DATE-HIRE-1.
           MOVE CI-POSITION-CODE-1 TO XCI-POSITION-CODE-1.
           MOVE CI-GROSS-MO-INCOME-1 TO XCI-GROSS-MO-INCOME-1.

      * ALREADY 8 DIGIT DATE
      *    MOVE CI-DATE-HIRE-2 TO DATE-MMDDYYYY.
      *    PERFORM CONVERT-MMDDYYYY-TO-YYYYMMDD.
      *    MOVE DATE-YYYYMMDD    TO XCI-DATE-HIRE-2.
           MOVE CI-DATE-HIRE-2 TO   XCI-DATE-HIRE-2.
           MOVE CI-POSITION-CODE-2 TO XCI-POSITION-CODE-2.
           MOVE CI-GROSS-MO-INCOME-2 TO XCI-GROSS-MO-INCOME-2.

           MOVE X"0D" TO XCI-CR.
           MOVE X"0A" TO XCI-LF.

           ADD 1 TO XCI-KEY.
           PERFORM WRITE-XCI-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

           ADD 1 TO CI-RECORD-COUNT.
           MOVE "Y" TO RECORDS-FOUND.

           GO TO NEXT-CI.

       EXIT-EXTRACT-CUSTOMER-INFO.
           PERFORM CLOSE-CI1-FILE.

      *******************************************
       INITIALIZATION SECTION.
      *******************************************
           MOVE BWEXTR-QUEUE-POS TO Q-POS.
           MOVE BWEXTR-COPIES-POS TO C-POS.
           MOVE BWEXTR-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".
           CALL "C$TOLOWER" USING XBW-DIRECTORY, VALUE 13.
           MOVE XBW-DIRECTORY TO DIR-ACCESS-BUF.
           PERFORM DIR-ACCESS-CALL
           IF STAT NOT = "00"
              MOVE "EXTRACTION DIRECTORY DOES NOT EXIST!" TO MESS
              PERFORM SEND-MESS
              GO TO ENDIT.

           PERFORM GET-TIME.
           MOVE TIME-EDIT TO STARTING-TIME.

           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           IF EXT-ROUTE-BUF = "70"
              MOVE EXT-EOBUF-MESS TO H-TITLE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF EXT-EOBUF-GROUP-FG = "Y"
              MOVE BEG-BRANCH TO SV-BEG-BR
              MOVE END-BRANCH TO SV-END-BR
              MOVE GROUP-FLAG TO SV-GROUP-FLAG
              MOVE ENT-GROUP TO SV-ENT-GROUP
              MOVE EXT-EOBUF-BRNO TO BEG-BRANCH END-BRANCH
              MOVE "N" TO GROUP-FLAG
              MOVE EXT-EOBUF-GROUP-PRPATH TO PRU-FILE
           ELSE
              IF ( BEG-BRANCH = 0 AND END-BRANCH = 0 )
                 MOVE BEG-BRANCH TO SV-BEG-BR
                 MOVE END-BRANCH TO SV-END-BR
                 MOVE EXT-CONAME-USER-LOC TO BEG-BRANCH END-BRANCH.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           MOVE " " TO EXT-EOBUF-ERRCD.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.

           IF EXT-ROUTE-BUF = "00"
              MOVE "EXTRACTION IN PROGRESS......." TO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           PERFORM OPEN-PR-FILE.

           IF CUST-INFO NOT = "O"
              PERFORM OPEN-XBW-FILE-OUTPUT
              PERFORM CLOSE-XBW-FILE
              PERFORM OPEN-XBW-FILE.

           IF CUST-INFO NOT = "S"
              CALL "C$TOLOWER" USING XCI-DIRECTORY, VALUE 13
              PERFORM OPEN-XCI-FILE-OUTPUT
              PERFORM CLOSE-XCI-FILE
              PERFORM OPEN-XCI-FILE.

           GO TO EXIT-INITIALIZE.

       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.

       EXIT-INITIALIZE.
           EXIT.

      *******************************************
       EO-EXEC-AUDIT SECTION.
      *******************************************

       EXIT-EO-EXEC-AUDIT.
           EXIT.

      *******************************************
       ENTER-PARAMETERS SECTION.
      *******************************************
       ENTER-PARM.

      * DONT GO FURTHER IF NOT A VALID LOGIN
      * NEED USERS BRANCH FOR BR-FILE
           IF EXT-CONAME-USER-LOC NOT NUMERIC
              MOVE "INVALID USER" TO MESS
              PERFORM SEND-MESS
              GO TO ENDIT.

      *----------------------------------------------------------------
       BR-01.
           MOVE "E  A000BR1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-BR.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.

           IF VDUSTATUS = "1<"
              MOVE "WI/GRSCAN" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH GRSCAN-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              IF GRSCAN-STATUS NOT = "00"
                 GO TO BR-01
              ELSE
                 MOVE "S NA000BR1." TO VDU
                 MOVE GRSCAN-KEY TO VDUBUF GR1-KEY
                 PERFORM SCREENIO
                 MOVE "00" TO VDUSTATUS.

           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BR-01.

           MOVE VDUBUF TO WGR-WORD ENT-GROUP.

           IF WGR-WORD-ZERO
              MOVE 0 TO WGR-SRESULT
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
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
              MOVE "Y" TO GROUP-FLAG
              MOVE 0 TO BEG-BRANCH
              MOVE 9999 TO END-BRANCH
              GO TO CHECK-BRANCH-SECURITY.

      *----------------------------------------------------------------
       BRANCH-IS-NUMERIC.
           MOVE "N" TO GROUP-FLAG.
           MOVE WGR-SRESULT TO BEG-BRANCH.
           MOVE "SN N040BR1." TO VDU.
           MOVE BEG-BRANCH TO VDUNUM0.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
       BR-02.
           MOVE "ENNN040BR2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BR-02.
           MOVE VDUNUM0 TO END-BRANCH.

           IF BEG-BRANCH > END-BRANCH
              MOVE "INVALID BRANCH RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO BR-01.

           IF ( (BEG-BRANCH = 0) AND (END-BRANCH = 0) )
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE "USER BRANCH ONLY" TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO.

           GO TO CHECK-BRANCH-SECURITY.

      *----------------------------------------------------------------
       ALL-BR.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.

           MOVE "S NA160DISPLAY-USER." TO VDU.
           MOVE SPACES TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "N" TO GROUP-FLAG.

      *----------------------------------------------------------------
       CHECK-BRANCH-SECURITY.
           PERFORM BR-RANGE-SECURITY.

           IF BR-SECURITY-OKAY = "N"
              GO TO BR-01.

      *----------------------------------------------------------------
       ENTER-CUST-INFO.
           MOVE "E  A010CUSTINFO." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ENTER-CUST-INFO.
           MOVE VDUBUF TO CUST-INFO.
           IF CUST-INFO NOT = "I" AND NOT = "S" AND NOT = "O"
              GO TO ENTER-CUST-INFO.

       EXIT-PARM.
           EXIT.

      *******************************************
       DISPLAY-PARAMETERS SECTION.
      *******************************************

           IF GROUP-FLAG = "Y"
              MOVE "S  A040BR1." TO VDU
              MOVE ENT-GROUP TO VDUNUM0
              PERFORM SCREENIO
              MOVE "SNNA040BR2." TO VDU
              MOVE SPACES TO VDUNUM0
              PERFORM SCREENIO
           ELSE
              MOVE "SNNN040BR1." TO VDU
              MOVE BEG-BRANCH TO VDUNUM0
              PERFORM SCREENIO
              MOVE "SNNN040BR2." TO VDU
              MOVE END-BRANCH TO VDUNUM0
              PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( (BEG-BRANCH = 0) AND (END-BRANCH = 0) )
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE "USER BRANCH ONLY" TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO.

      *******************************************
       WRITE-DETAIL-LINE SECTION.
      *******************************************
           ADD LN-FEED LN-COUNT GIVING LN-WK.

           IF LN-WK > EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.

           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      *******************************************
       PRINT-RANGE-LINES SECTION.
      *******************************************
           MOVE 4 TO LN-FEED.
       
           IF GROUP-FLAG = "Y"
              MOVE "BEGINNING BRANCH   :" TO RANGE-LINE
              MOVE ENT-GROUP              TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE
              MOVE "ENDING BRANCH      :" TO RANGE-LINE
              MOVE SPACES                 TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE
           ELSE
              MOVE "BEGINNING BRANCH   :" TO RANGE-LINE
              MOVE BEG-BRANCH             TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE
              MOVE "ENDING BRANCH      :" TO RANGE-LINE
              MOVE END-BRANCH             TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE.
       
           MOVE "STARTING TIME      :" TO RANGE-LINE.
           MOVE STARTING-TIME         TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
       
           MOVE "ENDING   TIME      :" TO RANGE-LINE.
           PERFORM GET-TIME.
           MOVE TIME-EDIT             TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
       
           MOVE "# RECORDS EXTRACTED:" TO RANGE-LINE.
           MOVE RECORD-COUNT TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
       
           MOVE "# CUSTINFO RECORDS    :" TO RANGE-LINE.
           MOVE CI-RECORD-COUNT TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

      *******************************************
       HEAD-PAGE SECTION.
      *******************************************

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
           MOVE 3 TO LN-COUNT.
           MOVE 2 TO LN-FEED.

      ******************************************************************
      *
      *     C O P Y    L I B R A R Y    S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/BRSECURE.CPY".
       COPY "LIBLP/LPBXCL.CPY".

      ************************************************
       FORM-RESET SECTION.
      ************************************************
           MOVE EXT-CONAME-CONAME TO BWEXTR-CONAME.
           MOVE EXT-CONAME-SYSDATE TO BWEXTR-SYSDATE.
           DISPLAY BWEXTR-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE BWEXTR-FORM-FIELDS TO WR-BUF.
           MOVE BWEXTR-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE BWEXTR-HELP--FILE TO HELP-LINK-FILE.
           MOVE BWEXTR-HELP--DIR TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/BWEXTR_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISC-PARA SECTION.
      ******************************************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
       SEND-LEGEND.
       INVALID-RANGE.
           MOVE "INVALID RANGE" TO MESS.
           PERFORM SEND-MESS.
       CONDITIONAL-PRINT.
           IF DETAIL-LINE NOT = " "
              MOVE 1 TO LN-FEED
              PERFORM WRITE-DETAIL-LINE.

      ******************************************************************
       IO-ROUTINE SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPCIGS_SQL.CPY".
       COPY "LIBLP/LPBXGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBEX/EXXBWI.CPY".
       COPY "LIBEX/EXXCII.CPY".

      * COPY "LIBLP/LPBW1RN.CPY".
      * COPYMEMBER: LIBLP/LPBW1RN
       LOAD-BW1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( BW-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( BW-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO BW-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO BW-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO BW-ALL-DATA
              MOVE BW-ALL-PATH             TO BW-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( BW-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO BW-ALL-BASE
              MOVE FILPATH-MACHINE         TO BW-ALL-MACHINE
              MOVE FILPATH-DATA            TO BW-ALL-DATA
              MOVE BW-ALL-PATH             TO BW-PATH.

      *-----------------------------------------------------------------
       OPEN-BW1-FILE.
           PERFORM LOAD-BW1-FILE.
           PERFORM OPEN-IT.
           MOVE BW-PATH-SQL TO E-FILE.

            IF BW-CONN-OPEN-SW = "N"
              MOVE "Y" TO BW-CONN-OPEN-SW
              EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS BW_CONNECT
                    USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
           END-EXEC.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       START-BW1-FILE. *> SSP - BW,BX
           PERFORM START-IT.
           MOVE BW-PATH-OWNBR  TO BW-OWNBR.
           MOVE BW-PATH-SQL    TO E-FILE.
           MOVE BW1-KEY        TO E-KEYX.

           IF ( BW1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-BW1-FILE.

           IF ( QBW1-WEND-SSNO = ZEROES    ) OR
              ( QBW1-WEND-SSNO NOT NUMERIC )
              MOVE BW-OWNBR             TO QBW1-WBEG-OWNBR
                                           QBW1-WEND-OWNBR
              MOVE BW-SSNO              TO QBW1-WBEG-SSNO
              MOVE ALL "9"              TO QBW1-WEND-SSNO.

           MOVE BR-NO                   TO QBW1-WBEG-OWNBR.


      *  STORED PROCEDURE
      *
      *   SELECT BWFILE.BW_OWNBR,
      *          BWFILE.BW_SSNO,
      *          BWFILE.BW_LNAME, 
      *          BWFILE.BW_FNAME, 
      *          BWFILE.BW_ADR1,  
      *          BWFILE.BW_ADR2, 
      *          BWFILE.BW_CITY, 
      *          BWFILE.BW_STATE, 
      *          BWFILE.BW_ZIP, 
      *          CAST(BWFILE.BW_REFDATE AS VARCHAR(10)),
      *          CAST(BWFILE.BW_BDATE   AS VARCHAR(10)),        
      *          BWFILE.BW_PHONE,
      *          BWFILE.BW_CELL_PHONE,
      *          BWFILE.BW_DEPEND,
      *          BWFILE.BW_RENTCD, 
      *          BWFILE.BW_TIMEADDR, 
      *          BWFILE.BW_CREDITCD, 
      *          BWFILE.BW_STRTGCD,  
      *          BWFILE.BW_DWELLCD, 
      *          BWFILE.BW_COUNTY, 
      *          BWFILE.BW_ORIGIN, 
      *          BWFILE.BW_CENSUSCD, 
      *          BWFILE.BW_ORGSLSMN,
      *          BWFILE.BW_REPCODE, 
      *          BWFILE.BW_SPSSNO,
      *          BWFILE.BW_SPLNAME, 
      *          BWFILE.BW_SPFNAME, 
      *          CAST(BWFILE.BW_SP_BDATE AS VARCHAR(10)), 
      *          BWFILE.BW_EMPLOYER, 
      *          BWFILE.BW_EMPLPOS, 
      *          BWFILE.BW_EMPPHONE,
      *          BWFILE.BW_EMPEXT,
      *          BWFILE.BW_EMPYEARS,
      *          BWFILE.BW_EMPPAY,
      *          BWFILE.BW_EMPPAYOT,
      *          BWFILE.BW_EMPPAYSR, 
      *          BWFILE.BW_EMPADDR1, 
      *          BWFILE.BW_EMPADDR2, 
      *          BWFILE.BW_WORKHRS, 
      *          BWFILE.BW_LOANS,
      *          BWFILE.BW_INTYTD,
      *          BWFILE.BW_INTLYR,
      *          BWFILE.BW_LIASMLN,
      *          BWFILE.BW_LIAOTH,
      *          CAST(BWFILE.BW_LASTPODATE AS VARCHAR(10)), 
      *          BWFILE.BW_PRLOANS,
      *          BWFILE.BW_RE_INTYTD,
      *          BWFILE.BW_RE_INTLYR,
      *          BWFILE.BW_CREDIT_LIMIT,
      *          BWFILE.BW_CREDIT_SCORE,
      *          BWFILE.BW_CRED_SCORE_1,
      *          BWFILE.BW_CRED_SCORE_2,
      *          BWFILE.BW_CRED_SCORE_3,
      *          BWFILE.BW_CRED_SCORE_4,
      *          BWFILE.BW_BNKRPT_SCORE_1,
      *          BWFILE.BW_BNKRPT_SCORE_2,
      *          BWFILE.BW_BNKRPT_SCORE_3,
      *          BWFILE.BW_BNKRPT_SCORE_4,
      *          BWFILE.BW_LAST_FORM_NO,
      *          CAST(BWFILE.BW_LAST_FORM_DATE      AS VARCHAR(10)), 
      *          CAST(BWFILE.BW_PRIOR_RN_FOR10_DATE AS VARCHAR(10)), 
      *          CAST(BWFILE.BW_LAST_RN_FOR10_DATE  AS VARCHAR(10)), 
      *          BWFILE.BW_BAD_ADR_FG,    
      *          BWFILE.BW_BAD_PHONE_FG, 
      *          BWFILE.BW_BAD_EMPPHONE_FG, 
      *          BWFILE.BW_BAD_MISC_PHONE1_FG, 
      *          BWFILE.BW_BAD_MISC_PHONE2_FG, 
      *          BWFILE.BW_BAD_CELL_FG, 
      *          BWFILE.BW_MISC_PHONE_1,
      *          BWFILE.BW_MISC_PHONE_2,
      *          BWFILE.BW_MISC_PHONE_3,
      *          BWFILE.BW_MISC_PHONE_4,
      *          BWFILE.BW_MISC_PHONE_5,
      *          BWFILE.BW_MISC_PHONE_6,
      *          BWFILE.BW_MISC_EXT_1,
      *          BWFILE.BW_MISC_EXT_2,
      *          BWFILE.BW_MISC_EXT_3,
      *          BWFILE.BW_MISC_EXT_4,
      *          BWFILE.BW_MISC_EXT_5,
      *          BWFILE.BW_MISC_EXT_6,
      *          BWFILE.BW_MISC_PHONE_CD_1, 
      *          BWFILE.BW_MISC_PHONE_CD_2, 
      *          BWFILE.BW_MISC_PHONE_CD_3, 
      *          BWFILE.BW_MISC_PHONE_CD_4, 
      *          BWFILE.BW_MISC_PHONE_CD_5, 
      *          BWFILE.BW_MISC_PHONE_CD_6, 
      *          BWFILE.BW_CONTACT_DESC_1, 
      *          BWFILE.BW_CONTACT_DESC_2, 
      *          BWFILE.BW_CONTACT_DESC_3, 
      *          BWFILE.BW_CONTACT_DESC_4, 
      *          BWFILE.BW_CONTACT_DESC_5, 
      *          BWFILE.BW_CONTACT_DESC_6, 
      *          BWFILE.BW_CONTACT_DESC_7, 
      *          BWFILE.BW_CONTACT_DESC_8, 
      *          BWFILE.BW_CONTACT_DESC_9, 
      *          BWFILE.BW_CONTACT_DESC_10, 
      *          BWFILE.BW_CONTACT_PHONE_1,
      *          BWFILE.BW_CONTACT_PHONE_2,
      *          BWFILE.BW_CONTACT_PHONE_3,
      *          BWFILE.BW_CONTACT_PHONE_4,
      *          BWFILE.BW_CONTACT_PHONE_5,
      *          BWFILE.BW_CONTACT_PHONE_6,
      *          BWFILE.BW_CONTACT_PHONE_7,
      *          BWFILE.BW_CONTACT_PHONE_8,
      *          BWFILE.BW_CONTACT_PHONE_9,
      *          BWFILE.BW_CONTACT_PHONE_10,
      *          BWFILE.BW_CONTACT_EXT_1,
      *          BWFILE.BW_CONTACT_EXT_2,
      *          BWFILE.BW_CONTACT_EXT_3,
      *          BWFILE.BW_CONTACT_EXT_4,
      *          BWFILE.BW_CONTACT_EXT_5,
      *          BWFILE.BW_CONTACT_EXT_6,
      *          BWFILE.BW_CONTACT_EXT_7,
      *          BWFILE.BW_CONTACT_EXT_8,
      *          BWFILE.BW_CONTACT_EXT_9,
      *          BWFILE.BW_CONTACT_EXT_10,
      *          BWFILE.BW_SRCD, 
      *          BWFILE.BW_CALL_ON_SUNDAY, 
      *          BWFILE.BW_SOLICIT, 
      *          BWFILE.BW_CONFIDENTIAL, 
      *          BWFILE.BW_PRIVACY_OPT_OUT, 
      *          BWFILE.BW_CITY_TAX_CD, 
      *          BWFILE.BW_EMAIL, 
      *          BWFILE.BW_ORGBR,
      *          BWFILE.BW-DEALTRACK-CHG-DATE,
      *          BWFILE.BW-CEASE-DESIST-DATE,
      *          BWFILE.BW-CEASE-DESIST-TYPE,
      *          BWFILE.BW-UMC-EXPIRE-DATE,
      *          BWFILE.BW-UMC-PR-EXPIRE-DATE,
      *          ISNULL(BXFILE.BX_BRNO, 0),
      *          ISNULL(BXFILE.BX_SSNO, 0),
      *          ISNULL(BXFILE.BX_ALT_ADR1,' '),
      *          ISNULL(BXFILE.BX_ALT_ADR2,' '),
      *          ISNULL(BXFILE.BX_ALT_CITY,' '),
      *          ISNULL(BXFILE.BX_ALT_STATE,' '),
      *          ISNULL(BXFILE.BX_ALT_ZIP,' '),
      *          ISNULL(CAST(BXFILE.BX_KYC_ISSDATE
      *                                AS VARCHAR(10)),'1900-01-01'),
      *          ISNULL(CAST(BXFILE.BX_KYC_EXPDATE
      *                                AS VARCHAR(10)),'1900-01-01'),
      *          ISNULL(CAST(BXFILE.BX_KYC_LAST_VERDATE
      *                                AS VARCHAR(10)),'1900-01-01')
      *     FROM DBO.BWFILE BWFILE 
      *     LEFT OUTER JOIN
      *          DBO.BXFILE BXFILE
      *       ON ( BXFILE.BX_BRNO       = BWFILE.BW_OWNBR
      *      AND   BXFILE.BX_SSNO       = BWFILE.BW_SSNO )
      *  
      *    WHERE ( BWFILE.BW_OWNBR      = @QBW1_WBEG_OWNBR )   
      *  
      *    ORDER BY BWFILE.BW_OWNBR,
      *             BWFILE.BW_SSNO  
 
           EXEC SQL SET CONNECTION BW_CONNECT END-EXEC.

           EXEC SQL
            DECLARE BW1_CURSOR CURSOR FOR
                :BW1-RETURN-CODE = EXEC SSP_BWEXTR_BWFILE_SELECT
              ( :QBW1-WBEG-OWNBR )                    
           END-EXEC.


           EXEC SQL
               OPEN BW1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QBW1-WBEG-OWNBR 
                          QBW1-WEND-OWNBR
                          QBW1-WBEG-SSNO
                          QBW1-WEND-SSNO.

           MOVE STAT---GOOD TO BW1-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       READ-BW1-FILE-NEXT. *> EMBEDDED - BW,BX
           PERFORM READ-IT.
           MOVE BW-PATH-OWNBR  TO BW-OWNBR.
           MOVE BW-PATH-SQL    TO E-FILE.
           MOVE BW1-KEY        TO E-KEYX.
           INITIALIZE QBW-REC.
           INITIALIZE QBX-REC.

           EXEC SQL SET CONNECTION BW_CONNECT END-EXEC.

           IF ( BW1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH BW1_CURSOR 
                    INTO 
                     :QBW-OWNBR,
                     :QBW-SSNO,
                     :QBW-LNAME, 
                     :QBW-FNAME, 
                     :QBW-ADR1,  
                     :QBW-ADR2, 
                     :QBW-CITY, 
                     :QBW-STATE, 
                     :QBW-ZIP, 
                     :QBW-REFDATE,
                     :QBW-BDATE,        
                     :QBW-PHONE,
                     :QBW-CELL-PHONE,
                     :QBW-DEPEND,
                     :QBW-RENTCD, 
                     :QBW-TIMEADDR, 
                     :QBW-CREDITCD, 
                     :QBW-STRTGCD,  
                     :QBW-DWELLCD, 
                     :QBW-COUNTY, 
                     :QBW-ORIGIN, 
                     :QBW-CENSUSCD, 
                     :QBW-ORGSLSMN,
                     :QBW-REPCODE, 
                     :QBW-SPSSNO,
                     :QBW-SPLNAME, 
                     :QBW-SPFNAME, 
                     :QBW-SP-BDATE, 
                     :QBW-EMPLOYER, 
                     :QBW-EMPLPOS, 
                     :QBW-EMPPHONE,
                     :QBW-EMPEXT,
                     :QBW-EMPYEARS,
                     :QBW-EMPPAY,
                     :QBW-EMPPAYOT,
                     :QBW-EMPPAYSR, 
                     :QBW-EMPADDR1, 
                     :QBW-EMPADDR2, 
                     :QBW-WORKHRS, 
                     :QBW-LOANS,
                     :QBW-INTYTD,
                     :QBW-INTLYR,
                     :QBW-LIASMLN,
                     :QBW-LIAOTH,
                     :QBW-LASTPODATE, 
                     :QBW-PRLOANS,
                     :QBW-RE-INTYTD,
                     :QBW-RE-INTLYR,
                     :QBW-CREDIT-LIMIT,
                     :QBW-CREDIT-SCORE,
                     :QBW-CRED-SCORE-1,
                     :QBW-CRED-SCORE-2,
                     :QBW-CRED-SCORE-3,
                     :QBW-CRED-SCORE-4,
                     :QBW-BNKRPT-SCORE-1,
                     :QBW-BNKRPT-SCORE-2,
                     :QBW-BNKRPT-SCORE-3,
                     :QBW-BNKRPT-SCORE-4,
                     :QBW-LAST-FORM-NO,
                     :QBW-LAST-FORM-DATE, 
                     :QBW-PRIOR-RN-FOR10-DATE, 
                     :QBW-LAST-RN-FOR10-DATE, 
                     :QBW-BAD-ADR-FG,    
                     :QBW-BAD-PHONE-FG, 
                     :QBW-BAD-EMPPHONE-FG, 
                     :QBW-BAD-MISC-PHONE1-FG, 
                     :QBW-BAD-MISC-PHONE2-FG, 
                     :QBW-BAD-CELL-FG, 
                     :QBW-MISC-PHONE-1,
                     :QBW-MISC-PHONE-2,
                     :QBW-MISC-PHONE-3,
                     :QBW-MISC-PHONE-4,
                     :QBW-MISC-PHONE-5,
                     :QBW-MISC-PHONE-6,
                     :QBW-MISC-EXT-1,
                     :QBW-MISC-EXT-2,
                     :QBW-MISC-EXT-3,
                     :QBW-MISC-EXT-4,
                     :QBW-MISC-EXT-5,
                     :QBW-MISC-EXT-6,
                     :QBW-MISC-PHONE-CD-1, 
                     :QBW-MISC-PHONE-CD-2, 
                     :QBW-MISC-PHONE-CD-3, 
                     :QBW-MISC-PHONE-CD-4, 
                     :QBW-MISC-PHONE-CD-5, 
                     :QBW-MISC-PHONE-CD-6, 
                     :QBW-CONTACT-DESC-1, 
                     :QBW-CONTACT-DESC-2, 
                     :QBW-CONTACT-DESC-3, 
                     :QBW-CONTACT-DESC-4, 
                     :QBW-CONTACT-DESC-5, 
                     :QBW-CONTACT-DESC-6, 
                     :QBW-CONTACT-DESC-7, 
                     :QBW-CONTACT-DESC-8, 
                     :QBW-CONTACT-DESC-9, 
                     :QBW-CONTACT-DESC-10, 
                     :QBW-CONTACT-PHONE-1,
                     :QBW-CONTACT-PHONE-2,
                     :QBW-CONTACT-PHONE-3,
                     :QBW-CONTACT-PHONE-4,
                     :QBW-CONTACT-PHONE-5,
                     :QBW-CONTACT-PHONE-6,
                     :QBW-CONTACT-PHONE-7,
                     :QBW-CONTACT-PHONE-8,
                     :QBW-CONTACT-PHONE-9,
                     :QBW-CONTACT-PHONE-10,
                     :QBW-CONTACT-EXT-1,
                     :QBW-CONTACT-EXT-2,
                     :QBW-CONTACT-EXT-3,
                     :QBW-CONTACT-EXT-4,
                     :QBW-CONTACT-EXT-5,
                     :QBW-CONTACT-EXT-6,
                     :QBW-CONTACT-EXT-7,
                     :QBW-CONTACT-EXT-8,
                     :QBW-CONTACT-EXT-9,
                     :QBW-CONTACT-EXT-10,
                     :QBW-SRCD, 
                     :QBW-CALL-ON-SUNDAY, 
                     :QBW-SOLICIT, 
                     :QBW-CONFIDENTIAL, 
                     :QBW-PRIVACY-OPT-OUT, 
                     :QBW-CITY-TAX-CD, 
                     :QBW-EMAIL, 
                     :QBW-ORGBR,
                     :QBW-DEALTRACK-CHG-DATE,
                     :QBW-CEASE-DESIST-DATE,
                     :QBW-CEASE-DESIST-TYPE,
                     :QBW-UMC-EXPIRE-DATE,
                     :QBW-UMC-PR-EXPIRE-DATE,
                     :QBX-BRNO,
                     :QBX-SSNO,
                     :QBX-ALT-ADR1, 
                     :QBX-ALT-ADR2, 
                     :QBX-ALT-CITY, 
                     :QBX-ALT-STATE,  
                     :QBX-ALT-ZIP, 
                     :QBX-KYC-ISSDATE,
                     :QBX-KYC-EXPDATE,
                     :QBX-KYC-LAST-VERDATE
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-BW1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-BW-FIELDS
              PERFORM GET-BX-FIELDS.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       CLOSE-BW1-FILE.

           PERFORM CLOSE-IT.
           IF ( BW1-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION BW_CONNECT END-EXEC
              EXEC SQL
                   CLOSE BW1_CURSOR
              END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC 
              MOVE STAT---BAD TO BW1-CURSOR-STAT.

      *-----------------------------------------------------------------
       CLOSE-BW1-CONNECTION.

           IF BW-CONN-OPEN-SW = "Y"
              EXEC SQL SET CONNECTION BW_CONNECT END-EXEC
              EXEC SQL COMMIT END-EXEC
              EXEC SQL DISCONNECT BW_CONNECT END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE "N" TO BW-CONN-OPEN-SW.


      * COPY "LIBLP/LPCI1RN.CPY".
      * COPYMEMBER: LIBLP/LPCI1RN
       LOAD-CI1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( CI-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( CI-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO CI-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO CI-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO CI-ALL-DATA
              MOVE CI-ALL-PATH             TO CI-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( CI-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO CI-ALL-BASE
              MOVE FILPATH-MACHINE         TO CI-ALL-MACHINE
              MOVE FILPATH-DATA            TO CI-ALL-DATA
              MOVE CI-ALL-PATH             TO CI-PATH.

      *-----------------------------------------------------------------
       OPEN-CI1-FILE.
           PERFORM LOAD-CI1-FILE.
           PERFORM OPEN-IT.
           MOVE CI-PATH-SQL TO E-FILE.
   
            IF CI-CONN-OPEN-SW = "N"
              MOVE "Y" TO CI-CONN-OPEN-SW
              EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS CI_CONNECT
                    USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
              END-EXEC.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       START-CI1-FILE. *> SSP - CIFILE
           PERFORM START-IT.
           MOVE CI-PATH-OWNBR  TO CI-BRNO.
           MOVE CI-PATH-SQL    TO E-FILE.
           MOVE CI1-KEY        TO E-KEYX.

           IF ( CI1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-CI1-FILE.

           IF ( QCI1-WEND-ACCTNO = ZEROES    ) OR
              ( QCI1-WEND-ACCTNO NOT NUMERIC )
              MOVE CI-BRNO              TO QCI1-WBEG-BRNO
              MOVE CI-BRNO              TO QCI1-WEND-BRNO
              MOVE CI-ACCTNO            TO QCI1-WBEG-ACCTNO
              MOVE ALL "9"              TO QCI1-WEND-ACCTNO.

      *  STORED PROCEDURE:
      *
      *   SELECT CIFILE.CI_BRNO,
      *          CIFILE.CI_ACCTNO,
      *          CIFILE.CI_SSNO,
      *          CIFILE.CI_SSNO_2,
      *          CAST(CIFILE.CI_DATE_UPDATED   AS VARCHAR(10)),
      *          CIFILE.CI_AMT_REQUESTED,
      *          CIFILE.CI_MARITAL_STAT,
      *          CIFILE.CI_YEARS_IN_AREA,
      *          CAST(CIFILE.CI_DATE_AT_PRESENT_ADR
      *                                         AS VARCHAR(10)),
      *          CIFILE.CI_MONTHLY_RENT_MTG,
      *          CIFILE.CI_FAIR_MARKET_VALUE,
      *          CIFILE.CI_MTG_BALANCE,
      *          CIFILE.CI_CHECKING_ACCOUNT,
      *          CIFILE.CI_SAVINGS_ACCOUNT,
      *          CIFILE.CI_TOTAL_MO_INCOME,
      *          CIFILE.CI_LESS_MO_PAY,
      *          CAST(CIFILE.CI_DATE_HIRE_1     AS VARCHAR(10)), 
      *          CIFILE.CI_POSITION_CODE_1,
      *          CIFILE.CI_GROSS_MO_INCOME_1,
      *          CAST(CIFILE.CI_DATE_HIRE_2     AS VARCHAR(10)), 
      *          CIFILE.CI_POSITION_CODE_2,
      *          CIFILE.CI_GROSS_MO_INCOME_2
      *      
      *     FROM DBO.CIFILE CIFILE 
      *    WHERE ( CIFILE.CI_BRNO = @QCI1_WBEG_BRNO )   
      *    ORDER BY CIFILE.CI_BRNO,
      *             CIFILE.CI_SSNO 

           EXEC SQL SET CONNECTION CI_CONNECT END-EXEC.

           EXEC SQL
            DECLARE CI1_CURSOR CURSOR FOR
                :CI1-RETURN-CODE = EXEC SSP_BWEXTR_CIFILE_SELECT
              ( :QCI1-WBEG-BRNO )                    
           END-EXEC.

           EXEC SQL
               OPEN CI1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QCI1-WBEG-BRNO 
                          QCI1-WEND-BRNO
                          QCI1-WBEG-ACCTNO
                          QCI1-WEND-ACCTNO.

           MOVE STAT---GOOD TO CI1-CURSOR-STAT.
        
           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       READ-CI1-FILE-NEXT. *> EMBEDDED - CIFILE
           PERFORM READ-IT.
           MOVE CI-PATH-OWNBR  TO CI-BRNO.
           MOVE CI-PATH-SQL    TO E-FILE.
           MOVE CI1-KEY        TO E-KEYX.
           INITIALIZE QCI-REC.

           EXEC SQL SET CONNECTION CI_CONNECT END-EXEC.

           IF ( CI1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH CI1_CURSOR 
                    INTO 
                     :QCI-BRNO,
                     :QCI-ACCTNO,
                     :QCI-SSNO,
                     :QCI-SSNO-2,
                     :QCI-DATE-UPDATED,
                     :QCI-AMT-REQUESTED,
                     :QCI-MARITAL-STAT,
                     :QCI-YEARS-IN-AREA,
                     :QCI-DATE-AT-PRESENT-ADR,
                     :QCI-MONTHLY-RENT-MTG,
                     :QCI-FAIR-MARKET-VALUE,
                     :QCI-MTG-BALANCE,
                     :QCI-CHECKING-ACCOUNT,
                     :QCI-SAVINGS-ACCOUNT,
                     :QCI-TOTAL-MO-INCOME,
                     :QCI-LESS-MO-PAY,
                     :QCI-DATE-HIRE-1, 
                     :QCI-POSITION-CODE-1,
                     :QCI-GROSS-MO-INCOME-1,
                     :QCI-DATE-HIRE-2, 
                     :QCI-POSITION-CODE-2,
                     :QCI-GROSS-MO-INCOME-2
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-CI1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-CI-FIELDS.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       CLOSE-CI1-FILE.

           PERFORM CLOSE-IT.
           IF ( CI1-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION CI_CONNECT END-EXEC
              EXEC SQL
                   CLOSE CI1_CURSOR
              END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC 
              MOVE STAT---BAD TO CI1-CURSOR-STAT.

      *-----------------------------------------------------------------
       CLOSE-CI1-CONNECTION.

           IF CI-CONN-OPEN-SW = "Y"
              EXEC SQL SET CONNECTION CI_CONNECT END-EXEC
              EXEC SQL COMMIT END-EXEC
              EXEC SQL DISCONNECT CI_CONNECT END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE "N" TO CI-CONN-OPEN-SW.



       COPY "LIBLP/LPBX1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************
       END-RUN.
           PERFORM CLOSE-FILES.

           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.

           IF NO-RECORDS-FOUND
              MOVE "NO RECORDS FOUND FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS.

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
              MOVE EXIT-PATH TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY BWEXTR-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
