       IDENTIFICATION DIVISION.
       PROGRAM-ID.   CHLIST.
       DATE-WRITTEN. 112789
      *****************************************************
      *                      (LS)
      *    DESCRIPTION:  MONTHLY CHANGE FILE REGISTER
      * REV:
      *  BLV 061097 ADD GROUP, CROSS DATA PATHS, NEW LOCAL SPOOL
      *  BAH 011998 AUDIT DIDNT SET DAY-OF-MONTH, PR# 167
      *  BAH 060998 IF MORE THAN 1 DAY IN FILE, WAS NOT FINDING DAY OTHER
      *             THAN THE FIRST...
      *  MJD 990201 ADDED CH-CODE "E" ACCOUNT FROZEN FLAG
      *  CS  990204 ADDED CH-CODE "G" REVOLVING CHARGES FLAGS
      *  CS  990222 ADDED CH-CODE "I" REVOLVING LN-INDEX-ADJUST-FACTORS
      *             AND CH-CODE  "J" FOR INSURANCE CODE CHANGES
      *  BLV 000314 ADDED CH-CODE OF "W" FOR CHANGING
      *             LN-WCBC-CREDITLIM, WORLD PR# 225
      *  BAH 000915 REMOVED THE ACCESS-CALL IN ENTER-PARAMETERS, IF YOUR
      *             DEFAULT BRANCH DID NOT HAVE A CHFILE, YOU COULD NOT
      *             RUN THIS...MADE IT LIKE THE EMTR REGISTERS...
      *  MJD 000918 ADDED "Y" ALLOTMENT/LBOX PLAN CHANGES (MFC PR#227)
      *  BAH 010319 ADDED BRANCH SECURITY PASSWORD TESTING (BRSECURE)
      *  BAH 011114 D-REF-SSNO WAS CUTTING OFF LAST DIGIT, JUDY
      *  BAH 020617 CLEANED UP THE COLLATERAL CHANGES RECORD, CREATE
      *             ONE FOR EACH FIELD THAT CHANGES, REGACC PL# 362
      *  BAH 020626 ADDED "P" FOR CHARGE OFF REASON CODE, REGACC PR# 1775
      *  CS  020911 ADDED "N" FOR LOAN ORIGIN CHANGES  [MODER PR#1876]
      *  BAH 030127 ADDED "Q" FOR INVESTOR CHANGES (REGWAN PR# 1975)
      *  BAH 030128 ADDED "R" FOR NOTE MASTER CHANGES (REGWAN PR# 1975)
      *  BLM 100302 ADDED "O" FOR ESCROW FLAG CHANGES (REGWAN PR# 3506)
      *  BAH 101007 ADDED NAME/ADR CHANGES ONLY FLAG FOR REGENCY TO DO
      *             ADDRESS VERIFICATIONS, PR# 3639
      *   CS 110225 ADDED MODERN 'CEASE & DESIST' WHEN THE CHANGES LOG IS
      *             WRITTEN IT SHARES THE SAME CH-CODE OF 'C' (CONFIDENTIAL)
      *             NEED TO WRITE THE DESCRIPTION TO CH-FILE IF CEASE &
      *             DESIST   PR#3720  MODERN
      *  BAH 111107 ADDED "1" FOR GROSS INCOME CHANGES (WORLD PR# 776)
      *   CS 130426 ADDED "2" FOR TDR (DEBT RESTRUCTURE) REGACC PR#338
      *   CS 170329 ADDED "3" FOR LN-SRCD CHANGES PR WORLD PR#1238
      *  BAH 180328 ADDED "C" "DUAL LOAN OVERRIDE" FOR WORLD #1308
      *  BAH 20190930 REMOVED ACCESS-CALL CHFILE
      *  BAH 2022.0114 ADD "4" CH-CODE FOR COLLATERAL CD (COLLCD) #1536
      *  BAH 2022.0426 ADD "5" CH-CODE CHANGE SECURITIZATION POOL #1551
      *  BAH 2023.0926 CHANGED "Q" FROM INVESTOR TO CHLOAN CHANGE SSNO'S #1625
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
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LS/CHLIST.CBL S35 02/23/24-11:43:50 >".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01CH.CPY".
       COPY "LIBLP/LP01CH_SQL.CPY".
       COPY "LIBLP/LPWSCH.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01DL.CPY".
       COPY "LIBLP/LP01DL_SQL.CPY".
       COPY "LIBLP/LPWSDL.CPY".


      **********************
      *   LINE WORKERS     *
      **********************
       01  LN-WORKERS.
           03  LN-FEED          PIC 999      VALUE 0.
           03  LN-COUNT         PIC 999      VALUE 99.
           03  LN-WK            PIC 999      VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

       01  ER-FEED              PIC 9        VALUE 1.

      **********************
      *   MISC WORKERS     *
      **********************
       01  HOLD-BRNO            PIC 9(4)  VALUE 0.
       01  HOLD-CODE            PIC X(3)  VALUE SPACES.
       01  RECORDS-FOUND        PIC X     VALUE " ".
           88  NO-RECORDS-FOUND           VALUE "N".

       01  FIRST-TIME           PIC X        VALUE "Y".
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".
       01  MSEL-LIST            PIC X(5)     VALUE "MSEL.".

       78  WS-QUOTE                       VALUE X'22'.
       01  D-ALLOT-LBOX.
           03  FILLER           PIC X(04) VALUE "A = ".
           03  FILLER           PIC X(01) VALUE WS-QUOTE.
           03  D-OLD-ALLOTCD    PIC X(01) VALUE SPACES.
           03  FILLER           PIC X(01) VALUE WS-QUOTE.
           03  FILLER           PIC X(07) VALUE " / L = ".
           03  FILLER           PIC X(01) VALUE WS-QUOTE.
           03  D-OLD-LBOX       PIC X(01) VALUE SPACES.
           03  FILLER           PIC X(01) VALUE WS-QUOTE.
           03  FILLER           PIC X(06).
           03  FILLER           PIC X(04) VALUE "A = ".
           03  FILLER           PIC X(01) VALUE WS-QUOTE.
           03  D-NEW-ALLOTCD    PIC X(01) VALUE SPACES.
           03  FILLER           PIC X(01) VALUE WS-QUOTE.
           03  FILLER           PIC X(07) VALUE " / L = ".
           03  FILLER           PIC X(01) VALUE WS-QUOTE.
           03  D-NEW-LBOX       PIC X(01) VALUE SPACES.
           03  FILLER           PIC X(01) VALUE WS-QUOTE.
           03  FILLER           PIC X(08).

       01  LEGEND               PIC X(70).
       COPY "LIBLP/EMSFRG_CMD.CPY".
      ***********************************
      *     S C R E E N   F I E L D S   *
      ***********************************
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  BEG-BRANCH      PIC 9(4).
           03  END-BRANCH      PIC 9(4).
           03  END-DATE.
               05  END-MONTH   PIC 99.
               05  END-YEAR    PIC 99.
           03  EDATE REDEFINES END-DATE PIC 9999.
           03  DAY-OF-MONTH    PIC 99.
           03  GROUP-FLAG      PIC X.
           03  ENT-GROUP       PIC X(4).
           03  NAME-FLAG       PIC X.

       01  VERSION-CONTROL     PIC X VALUE "B".
       01  SV-BEG-BR           PIC 9999  VALUE 9999.
       01  SV-END-BR           PIC 9999  VALUE 9999.
       01  SV-GROUP-FLAG       PIC X     VALUE SPACES.
       01  SV-ENT-GROUP        PIC X(4)  VALUE SPACES.
       01  RPT-END-DATE                       PIC 9(8)     VALUE 0.
       01  BEGINNING-DATE                     PIC 9(8)     VALUE 0.

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
           03  FILLER           PIC X(30)    VALUE " ".
           03  FILLER           PIC X(29) VALUE
               "MONTHLY CHANGES REGISTER FOR ".
           03  H-MO             PIC 99.
           03  FILLER           PIC X VALUE "/".
           03  H-YR             PIC 99.

       01  HEAD3.
           03  FILLER           PIC X(10) VALUE "BRANCH :".
           03  H-BRNO           PIC 9(4).
           03  FILLER           PIC X VALUE " ".
           03  H-BRDESC         PIC X(20).
           03  FILLER           PIC X(46) VALUE " ".

       01  HEAD4.
           03  FILLER           PIC X(11)    VALUE
              "DAY   TIME ".
           03  FILLER           PIC X(26)    VALUE
              "    USER       DESCRIPTION".
           03  FILLER           PIC X(24)    VALUE
              "                OLD     ".
           03  FILLER           PIC X(22)    VALUE
              "                NEW   ".
           03  FILLER           PIC X(49)    VALUE
              "                   REFERENCE".


       01  DETAIL-LINE          VALUE SPACES.
           03  FILLER           PIC X(132).

       01  D-LINE1 REDEFINES DETAIL-LINE.
           03  D-DAY            PIC Z9.
           03  FILLER           PIC X(2).
           03  D-HR             PIC Z9.
           03  D-COLON1         PIC X.
           03  D-MIN            PIC 99.
           03  D-COLON2         PIC X.
           03  D-SEC            PIC 99.
           03  FILLER           PIC X(2).
           03  D-USERID         PIC X(10).
           03  FILLER           PIC X(2).
           03  REMAINING-LINE.
               05  D-DESC               PIC X(25).
               05  D-DESCRIPTION-PART REDEFINES D-DESC.
                   07  D-DESC-PART1      PIC X(10).
                   07  D-DESC-PART2      PIC X(15).
               05  FILLER               PIC X(2).
               05  D-OLD-NEW.
                   07  D-OLD.
                       09  D-OLD-CREDIT PIC Z(6).
                       09  FILLER       PIC X(16).
                       09  FILLER       PIC X(2).
                   07  D-NEW.
                       09  D-NEW-CREDIT PIC Z(6).
                       09  FILLER       PIC X(18).
               05  D-SOCIAL REDEFINES D-OLD-NEW.
                   07  D-OSSNO          PIC 999/99/9999.
                   07  FILLER           PIC X(13).
                   07  D-NSSNO          PIC 999/99/9999.
               05  D-DATE REDEFINES D-OLD-NEW.
                   07  D-ODATE          PIC 99/99/99 BLANK ZERO.
                   07  FILLER           PIC X(16).
                   07  D-NDATE          PIC 99/99/99 BLANK ZERO.
               05  D-FROZEN REDEFINES D-OLD-NEW.
                   07  FILLER           PIC X(2).
                   07  D-OLD-FROZEN       PIC X.
                   07  FILLER           PIC X(23).
                   07  D-NEW-FROZEN       PIC X.
               05  D-CONFID REDEFINES D-OLD-NEW.
                   07  D-OCONF          PIC XX.
                   07  FILLER           PIC X(21).
                   07  D-NCONF          PIC XX.
               05  D-DEALER REDEFINES D-OLD-NEW.
                   07  D-ODLNO          PIC ZZZ9.99.
                   07  FILLER           PIC X(17).
                   07  D-NDLNO          PIC ZZZ9.99.
               05  D-LOAN-ORIGIN REDEFINES D-OLD-NEW.
                   07  FILLER             PIC X(2).
                   07  D-OLD-LOAN-ORIGIN  PIC X(4).
                   07  FILLER             PIC X(20).
                   07  D-NEW-LOAN-ORIGIN  PIC X(4).
                   07  FILLER             PIC X(18).
               05  D-ESCROW REDEFINES D-OLD-NEW.
                   07  FILLER             PIC X(2).
                   07  D-OLD-ESCROW-FG    PIC X.
                   07  FILLER             PIC X(23).
                   07  D-NEW-ESCROW-FG    PIC X.
               05  D-TDRS REDEFINES D-OLD-NEW.
                   07  D-OLD-TDR          PIC X.
                   07  FILLER             PIC X.
                   07  D-OLD-TDR-DATE     PIC 99/99/99.
                   07  FILLER             PIC X(14).
                   07  D-NEW-TDR          PIC X.
                   07  FILLER             PIC X.
                   07  D-NEW-TDR-DATE     PIC 99/99/99.
                   07  FILLER             PIC X(13).
               05  D-LOAN-SRCD   REDEFINES D-OLD-NEW.
                   07  FILLER             PIC X(2).
                   07  D-OLD-LOAN-SRCD    PIC X(2).
                   07  FILLER             PIC X(20).
                   07  D-NEW-LOAN-SRCD    PIC X(2).
                   07  FILLER             PIC X(18).


               05  D-REFERENCE.
                   07  FILLER           PIC X.
                   07  D-NAME           PIC X(17).
                   07  FILLER           PIC X.
                   07  D-REFNO.
                       09  D-ACCTNO     PIC ZZ9(6) BLANK ZERO.
                       09  FILLER       PIC X(3).
                   07  FILLER REDEFINES D-REFNO.
                       09  D-REF-SSNO   PIC 999/99/9999.

       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(35).
           03  RANGE-SELECTION.
               04  R-NUM        PIC Z(5)9.

       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/GETPRW.CPY".


       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLS/CHLIST_DEF.CPY".
       COPY "LIBLS/CHLIST_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBLS/CHLIST_SCN.CPY".


      /***************************************
      *     P R O G R A M   C O N T R O L    *
      ****************************************
       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-DL1-FILE.
           PERFORM CLOSE-CH-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      **********************************
       MAIN-MODULE SECTION.
           PERFORM SQL-CONNECT.
      **********************************
      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "CHLIST" TO VDU-FORM-NAME.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

           IF GROUP-FLAG = "N"
              MOVE BEG-BRANCH TO BR-NO
              PERFORM START-BR-FILE
              GO TO NEXT-BRANCH-NO.

           MOVE ENT-GROUP TO GR-ID.
           PERFORM SEARCH-GR-INIT.

      *-----------------------------------------------------------------
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

      *-----------------------------------------------------------------
       NEXT-BRANCH-NO.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-FG = 9 OR BR-NO > END-BRANCH
              GO TO END-ROUTINE.

      *-----------------------------------------------------------------
       MOVE-BRANCH-PATH-INFO.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO BW-PATH-OVERRIDE
                                              CH-PATH-OVERRIDE
                                              LN-PATH-OVERRIDE.

           PERFORM OPEN-CH-FILE.
           PERFORM OPEN-BW1-FILE.
           PERFORM OPEN-LN1-FILE.

           MOVE SPACES         TO CH-REC.

           MOVE CH-PATH-OWNBR       TO CH-BRNO
                                       QCH-WBEG-BRNO
                                       QCH-WEND-BRNO.
           MOVE BEGINNING-DATE      TO CH-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QCH-WBEG-DATE.
           MOVE RPT-END-DATE        TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE
           MOVE SQL-DATE-YYYY-MM-DD TO QCH-WEND-DATE.

           MOVE CH-CODE             TO QCH-WBEG-CODE.
           MOVE ALL "Z"             TO QCH-WEND-CODE.
           MOVE CH-USERID           TO QCH-WBEG-USERID.
           MOVE ALL "Z"             TO QCH-WEND-USERID
           MOVE 0                   TO CH-TIME
                                       CH-SEQ
                                       QCH-WBEG-TIME
                                       QCH-WBEG-SEQ.
           MOVE ALL "9"             TO QCH-WEND-TIME
                                       QCH-WEND-SEQ.

           PERFORM START-CH-FILE.

       NEXT-TR-REPORT.
           PERFORM READ-CH-FILE-NEXT.
           IF IO-FG = 9 OR (CH-PATH-OWNBR NOT = CH-BRNO)
              GO TO END-OF-GROUP.

           IF CH-DATE > RPT-END-DATE
              GO TO END-OF-GROUP.

           IF NAME-FLAG = "Y"
              IF CH-CODE NOT = "M" 
                 GO TO NEXT-TR-REPORT
              ELSE
                 IF NOT (CH-DESCRIPT = "BORROWER LAST NAME" OR
                                       "BORROWER FIRST NAME" OR
                                       "ADDRESS 1" OR "ADDRESS 2" OR
                                       "CITY" OR "STATE" OR "ZIP" OR
                                       "INVESTOR LAST NAME" OR
                                       "INVESTOR FIRST NAME" OR
                                       "INVESTOR ADDRESS 1" OR
                                       "INVESTOR ADDRESS 2" OR
                                       "INVESTOR CITY" OR
                                       "INVESTOR STATE" OR
                                       "INVESTOR ZIPCODE" OR
                                       "IT KYC ADR1" OR "IT KYC ADR2" OR
                                       "IT KYC CITY" OR
                                       "IT KYC STATE" OR "IT KYC ZIP" OR
                                       "BX KYC ADR1" OR "BX KYC ADR2" OR
                                       "BX KYC CITY" OR
                                       "BX KYC STATE" OR "BX KYC ZIP" OR
                                       "CEASE & DESIST")
                    GO TO NEXT-TR-REPORT.

           IF FIRST-TIME = "Y"
              MOVE CH-BRNO TO HOLD-BRNO
              MOVE CH-CODE TO HOLD-CODE
              PERFORM SET-BRANCH-HEADINGS.

           IF CH-CODE NOT = HOLD-CODE
              MOVE CH-CODE TO HOLD-CODE
              MOVE 3 TO LN-FEED.

           IF CH-BRNO NOT = HOLD-BRNO
              MOVE CH-BRNO TO HOLD-BRNO
              PERFORM SET-BRANCH-HEADINGS
              PERFORM HEAD-PAGE.

           PERFORM DETAIL-LINE-PROCESS.

           GO TO NEXT-TR-REPORT.

       END-OF-GROUP.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-CH-FILE.

           GO TO NEXT-GROUP-LOC.

       END-ROUTINE.
           IF FIRST-TIME = "Y"
              MOVE "N" TO RECORDS-FOUND
              GO TO END-PROGRAM.

           PERFORM PRINT-RANGE-LINES
           GO TO END-PROGRAM.

      ***********************************
       INITIALIZATION SECTION.
      ***********************************
           MOVE CHLIST-QUEUE-POS TO Q-POS.
           MOVE CHLIST-COPIES-POS TO C-POS.
           MOVE CHLIST-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           IF EXT-ROUTE-BUF NOT = "00"
              MOVE EXT-EOBUF-DATE2 TO SYS-DATE
              MOVE S-MM TO END-MONTH
              MOVE S-YY TO END-YEAR.

           MOVE 0 TO PAGE-NUMBER.

       SET-HEAD.
           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

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
                 MOVE EXT-CONAME-USER-LOC  TO BEG-BRANCH END-BRANCH.

           IF EXT-ROUTE-BUF = "00"
              MOVE "REPORT IN PROGRESS............" TO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           MOVE END-MONTH TO DATE-MMDDYY-MM.
           MOVE END-YEAR  TO DATE-MMDDYY-YY.
           IF DAY-OF-MONTH = 99
              MOVE 01        TO DATE-MMDDYY-DD
           ELSE
              MOVE DAY-OF-MONTH TO DATE-MMDDYY-DD.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO BEGINNING-DATE.

           MOVE END-MONTH TO DATE-MMDDYY-MM.
           MOVE END-YEAR  TO DATE-MMDDYY-YY.
           IF DAY-OF-MONTH = 99
              MOVE 31      TO DATE-MMDDYY-DD
           ELSE
              MOVE DAY-OF-MONTH TO DATE-MMDDYY-DD.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO NDTE-DATE.
           PERFORM NEW-DATE-CALCULATION.
           MOVE NDTE-DATE     TO RPT-END-DATE.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-PR-FILE.

           GO TO EXIT-INITIALIZE.
       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.
       EXIT-INITIALIZE.
           EXIT.


      *****************************
       EO-EXEC-AUDIT SECTION.
      *****************************

           MOVE EXT-EOBUF-DATE2 TO SYS-DATE.
           MOVE S-MM TO END-MONTH.
           MOVE S-YY TO END-YEAR.

           IF EXT-EOBUF-DATE1 NOT = EXT-EOBUF-DATE2
              MOVE 99 TO DAY-OF-MONTH
           ELSE
              MOVE S-DD TO DAY-OF-MONTH.

      ***********************************
       ENTER-PARAMETERS SECTION.
      ***********************************
       ENTER-PARM.

           IF VDUSTATUS = "1U"
              GO TO ENTER-NAME-FLAG.

      * DEFAULT TO USER BRANCH
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE 0 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
       BR-01.
           MOVE LEGEND-BR-01 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A040BR1." TO VDU.
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
           MOVE LEGEND-BR-02 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN040BR2." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-01.
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
           MOVE 0             TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999          TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "N"           TO GROUP-FLAG.

       CHECK-BRANCH-SECURITY.
           PERFORM BR-RANGE-SECURITY.
           IF BR-SECURITY-OKAY = "N"
              GO TO BR-01.
      *----------------------------------------------------------------
       DTE.
           IF EXT-ROUTE-BUF NOT = "00"
              GO TO DA-01.
           MOVE LEGEND-DTE TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN040ENDDATE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO DTE.
           MOVE VDUNUM0 TO EDATE.

           IF END-MONTH < 1 OR > 12
              MOVE "INVALID MONTH" TO MESS
              PERFORM SEND-MESS
              GO TO DTE.

       DA-01.
           MOVE LEGEND-DA-01 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN020DA1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16"
              MOVE "S  A020DA1." TO VDU
              MOVE "**" TO VDUBUF
              PERFORM SCREENIO
              MOVE 99 TO DAY-OF-MONTH
              GO TO ENTER-NAME-FLAG.
           IF VDUSTATUS = "1U"
              IF EXT-ROUTE-BUF = "00"
                 GO TO DTE
              ELSE
                 GO TO BR-02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO DTE.
           IF VDUNUM0 = 0 GO TO DA-01.
           MOVE VDUNUM0 TO DAY-OF-MONTH.
           IF DAY-OF-MONTH > 31  OR
              (DAY-OF-MONTH > 30 AND
              (END-MONTH = 2 OR 4 OR 6 OR 9 OR 11)) OR
              (DAY-OF-MONTH > 29 AND END-MONTH = 2)
              MOVE "DAY IS PAST LAST DAY OF MONTH" TO MESS
              PERFORM SEND-MESS
              GO TO DA-01.

       ENTER-NAME-FLAG.
           MOVE SPACES TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A010NAME." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO DA-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO ENTER-NAME-FLAG.
           IF VDUBUF NOT = "Y" AND NOT = "N"
              GO TO ENTER-NAME-FLAG.
           MOVE VDUBUF TO NAME-FLAG.

       END-PARM.
           MOVE LEGEND-ACCEPT TO LEGEND.
           PERFORM SEND-LEGEND.

       EXIT-PARM.
           EXIT.

      ************************************
       DISPLAY-PARAMETERS SECTION.
      ************************************

           IF GROUP-FLAG = "Y"
              MOVE "S  A000BR1." TO VDU
              MOVE ENT-GROUP TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A000BR2." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "SNNN040BR1." TO VDU
              MOVE BEG-BRANCH TO VDUNUM0
              PERFORM SCREENIO
              MOVE "SNNN040BR2." TO VDU
              MOVE END-BRANCH TO VDUNUM0
              PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( (BEG-BRANCH = 0) AND (END-BRANCH = 0) )
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE "USER BRANCH ONLY" TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO.
           MOVE "SNNN020DA1." TO VDU.
           MOVE DAY-OF-MONTH TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "S  A010NAME." TO VDU.
           MOVE NAME-FLAG TO VDUBUF.
           PERFORM SCREENIO.

      **************************
       PRINT-RANGE-LINES SECTION.
      **************************
           MOVE 4 TO LN-FEED.

           IF GROUP-FLAG = "Y"
              MOVE "BRANCH GROUP" TO RANGE-LINE
              MOVE ENT-GROUP TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE
           ELSE
              MOVE "BEG BRANCH" TO RANGE-LINE
              MOVE BEG-BRANCH TO R-NUM
              PERFORM WRITE-DETAIL-LINE
              MOVE "END BRANCH" TO RANGE-LINE
              MOVE END-BRANCH TO R-NUM
              PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "MONTH-END DATE" TO RANGE-LINE.
           MOVE EDATE TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "DAY OF MONTH" TO RANGE-LINE.
           MOVE DAY-OF-MONTH TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "NAME/ADR FLAG" TO RANGE-LINE.
           MOVE NAME-FLAG TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      ****************************
       DETAIL-LINE-PROCESS SECTION.
      ****************************
           IF FIRST-TIME = "Y"
              MOVE EXT-CONAME-RPT-LINES TO LN-COUNT
              MOVE 1 TO LN-FEED
              MOVE "N" TO FIRST-TIME.

           MOVE CH-DATE TO DATE-YYYYMMDD.
           MOVE DATE-YYYYMMDD-DD TO D-DAY.
           MOVE CH-USERID        TO D-USERID.
           MOVE CH-HR            TO D-HR.
           MOVE ":"              TO D-COLON1 D-COLON2.
           MOVE CH-MIN           TO D-MIN.
           MOVE CH-SEC           TO D-SEC.

           MOVE CH-DESCRIPT TO D-DESC.

      * NEED TO KEEP THIS IN HERE BECAUSE THE A30/A32 UPGRADE, A30 FILES
      * DID NOT HAVE CH-DESCRIPT FILLED IN!!

           IF CH-CODE = "C"
              IF CH-DESCRIPT = "DUAL LOAN OVERRIDE" OR
                                        "CEASE & DESIST"
                  MOVE CH-DESCRIPT TO D-DESC
              ELSE
                  MOVE "CONFIDENTIAL STATUS CHANGE" TO D-DESC
              END-IF
           ELSE
           IF CH-CODE = "D"
              MOVE "DEBT AID STATUS CHANGE" TO D-DESC
           ELSE
           IF CH-CODE = "E"
              MOVE "FROZEN ACCOUNT CHANGE" TO D-DESC
           ELSE
           IF CH-CODE = "F"
              MOVE "INSUR. CONFIRMATION DATE" TO D-DESC
           ELSE
           IF CH-CODE = "H"
              MOVE "HIGH CREDIT LIMIT CHANGE" TO D-DESC
           ELSE
           IF CH-CODE = "K"
              MOVE "CREDIT BUREAU REPORTING" TO D-DESC
           ELSE
           IF CH-CODE = "L"
              MOVE "LEGAL STATUS CHANGE" TO D-DESC
           ELSE
           IF CH-CODE = "M" OR "T" OR "Q" OR "R" OR "Z"
              MOVE CH-DESCRIPT TO D-DESC
           ELSE
           IF CH-CODE = "N"
              MOVE "LOAN ORIGIN CHANGE"       TO D-DESC
           ELSE
           IF CH-CODE = "O"
              MOVE "ESCROW INACTIVE CHANGE"   TO D-DESC
           ELSE
           IF CH-CODE = "P"
              MOVE "CHARGE OFF REASON" TO D-DESC
           ELSE
           IF CH-CODE = "W"
              MOVE "WCBC CREDIT LIMIT CHANGE" TO D-DESC
           ELSE
           IF CH-CODE = "X"
              MOVE "REPO STATUS CHANGE" TO D-DESC
           ELSE
           IF CH-CODE = "Y"
              MOVE "ALLOT/LOCKBOX PLAN CHANGE" TO D-DESC
           ELSE
           IF CH-CODE = "1"
              MOVE "GROSS INCOME CHANGE" TO D-DESC
           ELSE
           IF CH-CODE = "2"
              MOVE "TDR"         TO D-DESC-PART1
           ELSE
           IF CH-CODE = "3"
              MOVE "LOAN SOURCE CD CHANGE" TO D-DESC
           ELSE
           IF CH-CODE = "4"
              MOVE "COLLATERAL CODE CHANGE" TO D-DESC
           ELSE
           IF CH-CODE = "5"
              MOVE "LOAN SECURITIZATION POOL" TO D-DESC.


      * CREDIT, DEBT, REPO, LEGL, BANKRPT, CONFIDENTIAL, FROZEN
      * INDEX RATE ADJUSTMENTS, INSURANCE CODE CHANGES, LOAN ORIGIN
      * GROSS INCOME, SECURITIZATION
           IF CH-CODE = "B" OR "C" OR "D" OR "E" OR "F" OR
                        "H" OR "K" OR "L" OR "N" OR
                        "O" OR "P" OR "T" OR "W" OR "X" OR "Z" OR
                        "5" OR "1" OR "3" OR "4"
              MOVE CH-ACCTNO TO D-ACCTNO LN-ACCTNO
              PERFORM GET-LN-BW.

           IF CH-CODE = "H" OR "W" OR "1"
              MOVE CH-ORG-NODEC TO D-OLD-CREDIT
              MOVE CH-NEW-NODEC TO D-NEW-CREDIT
              GO TO DETAIL-LINE-WRITE.

           IF CH-CODE = "Y"
              MOVE CH-ORG-ALLOTCD TO D-OLD-ALLOTCD
              MOVE CH-NEW-ALLOTCD TO D-NEW-ALLOTCD
              MOVE CH-ORG-LBOX    TO D-OLD-LBOX
              MOVE CH-NEW-LBOX    TO D-NEW-LBOX
              MOVE D-ALLOT-LBOX   TO D-OLD-NEW.

           IF CH-CODE = "E"
              MOVE CH-ORG-CHAR TO D-OLD-FROZEN
              MOVE CH-NEW-CHAR TO D-NEW-FROZEN
              GO TO DETAIL-LINE-WRITE.

           IF CH-CODE = "N" OR "5"
              MOVE CH-ORG-CHAR TO D-OLD-LOAN-ORIGIN
              MOVE CH-NEW-CHAR TO D-NEW-LOAN-ORIGIN
              GO TO DETAIL-LINE-WRITE.

           IF CH-CODE = "O"
              MOVE CH-ORG-CHAR TO D-OLD-ESCROW-FG
              MOVE CH-NEW-CHAR TO D-NEW-ESCROW-FG
              GO TO DETAIL-LINE-WRITE.

           IF CH-CODE = "B"
              MOVE CH-ORG-DATE TO D-ODATE
              MOVE CH-NEW-DATE TO D-NDATE
              GO TO DETAIL-LINE-WRITE.

           IF CH-CODE = "D" OR "X" OR "L" OR "B" OR "F"
              MOVE CH-ORG-NODEC TO D-ODATE
              MOVE CH-NEW-NODEC TO D-NDATE
              GO TO DETAIL-LINE-WRITE.

      * COLLATERAL TRAILER  "T"
      * DEFICIENCY TRAILER  "Z" AND PAID THRU (DEFERMENT TRAILER)

           IF CH-CODE = "T" OR "Z"
              MOVE CH-ORG-CHAR TO D-OLD
              MOVE CH-NEW-CHAR TO D-NEW
              GO TO DETAIL-LINE-WRITE.

           IF CH-CODE = "C"
              MOVE CH-ORG-CHAR TO D-OCONF
              MOVE CH-NEW-CHAR TO D-NCONF
              GO TO DETAIL-LINE-WRITE.

           IF CH-CODE = "K" OR "P"
              MOVE CH-ORG-CHAR TO D-OCONF
              MOVE CH-NEW-CHAR TO D-NCONF
              GO TO DETAIL-LINE-WRITE.

           IF CH-CODE = "A"
              MOVE "DEALER NUMBER CHANGE" TO D-DESC
              MOVE CH-ORG-2DEC TO D-ODLNO DL-DLNO
              MOVE CH-NEW-2DEC TO D-NDLNO
              PERFORM OPEN-DL1-FILE
              PERFORM READ-DL1-FILE
              PERFORM CLOSE-DL1-FILE
              IF IO-FG = 0
                 MOVE DL-NAME TO D-NAME
                 GO TO DETAIL-LINE-WRITE
              ELSE
                 GO TO DETAIL-LINE-WRITE.

           IF CH-CODE = "S"
              MOVE "SOCIAL SECURITY # CHANGE" TO D-DESC
              MOVE CH-ORG-SSNO TO D-OSSNO
              INSPECT D-OSSNO REPLACING ALL "/" BY "-"
              MOVE CH-NEW-SSNO TO D-NSSNO BW-SSNO
              INSPECT D-NSSNO REPLACING ALL "/" BY "-"
              PERFORM GET-BW
              GO TO DETAIL-LINE-WRITE.

           IF CH-CODE = "M"
              MOVE CH-ORG-CHAR TO D-OLD
              MOVE CH-NEW-CHAR TO D-NEW
              MOVE CH-SSNO TO BW-SSNO
                              D-REF-SSNO
              INSPECT D-REF-SSNO REPLACING ALL "/" BY "-"
              PERFORM GET-BW.

      * "Q" IS NOW CHLOAN CHANGE SSNO'S
           IF CH-CODE = "Q"
              MOVE CH-ORG-SSNO TO D-OSSNO
              INSPECT D-OSSNO REPLACING ALL "/" BY "-"
              MOVE CH-NEW-SSNO TO D-NSSNO
              INSPECT D-NSSNO REPLACING ALL "/" BY "-"
              MOVE CH-NEW-SSNO TO BW-SSNO
              MOVE CH-ACCTNO   TO D-ACCTNO
              GO TO DETAIL-LINE-WRITE.

           IF CH-CODE = "2"
               MOVE CH-TDR-1-2      TO D-DESC-PART2
               MOVE CH-ORG-TDR      TO D-OLD-TDR
               MOVE CH-ORG-TDR-DATE TO D-OLD-TDR-DATE
               MOVE CH-NEW-TDR      TO D-NEW-TDR
               MOVE CH-NEW-TDR-DATE TO D-NEW-TDR-DATE
               MOVE CH-ACCTNO       TO D-ACCTNO.

           IF CH-CODE = "3" OR "4"
              MOVE CH-ORG-CHAR      TO D-OLD-LOAN-SRCD
              MOVE CH-NEW-CHAR      TO D-NEW-LOAN-SRCD.

       DETAIL-LINE-WRITE.
           PERFORM WRITE-DETAIL-LINE.

      ****************************
       WRITE-DETAIL-LINE SECTION.
      ****************************
           ADD LN-FEED LN-COUNT GIVING LN-WK.
           IF LN-WK GREATER THAN EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.
           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      ********************************
       SET-BRANCH-HEADINGS SECTION.
      *******************************
           MOVE HOLD-BRNO TO H-BRNO.
           MOVE BR-NAME TO H-BRDESC.

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
           MOVE END-MONTH TO H-MO.
           MOVE END-YEAR TO H-YR.
           MOVE HEAD2 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
           MOVE HEAD3 TO PR-REC.
           PERFORM WRITE-PR-REC.
           MOVE HEAD4 TO PR-REC.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.
           MOVE 5 TO LN-COUNT.
           MOVE 2 TO LN-FEED.
       END-HEAD-PAGE.
           EXIT.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/BRSECURE.CPY".

      ******************************************************
       FORM-RESET SECTION.
      ******************************************************
           MOVE EXT-CONAME-CONAME TO CHLIST-CONAME.
           MOVE EXT-CONAME-SYSDATE TO CHLIST-SYSDATE.
           DISPLAY CHLIST-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE CHLIST-FORM-FIELDS TO WR-BUF.
           MOVE CHLIST-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       FORM-RESET-EXIT.
           EXIT.


      ******************************************************
       SETUP-LINK-INFO SECTION.
      ******************************************************
           MOVE CHLIST-HELP--FILE TO HELP-LINK-FILE.
           MOVE CHLIST-HELP--DIR TO HELP-LINK-DIR.

      ******************************************************
       VDU-CONVERT-FIELD SECTION.
      ******************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLS/CHLIST_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.


      **************************************
       MISC-PARA SECTION.
      **************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       SEND-LEGEND.
           MOVE MSEL-LIST TO WR-LIST.
           MOVE LEGEND    TO WR-BUF.
           PERFORM CALL-WRFORM.

       GET-LN-BW.
           PERFORM READ-LN1-FILE.
           IF IO-FG = 0
              MOVE LN-SSNO(1) TO BW-SSNO
              PERFORM GET-BW.
       GET-BW.
           PERFORM READ-BW1-FILE.
           IF IO-FG = 0
              STRING BW-LNAME,", ",BW-FNAME DELIMITED BY "   "
                    INTO D-NAME.

      ******************************************
       IO-ROUTINES SECTION.
      ******************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPDLGS_SQL.CPY".
       COPY "LIBLP/LPCHGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBLP/LPCHRN.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBLP/LPDL1RN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      ***************************************************
       END-PROGRAM SECTION.
      ***************************************************
       END-RUN.
           PERFORM CLOSE-FILES.

           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.

           IF NO-RECORDS-FOUND
              MOVE "NO RECORDS FOUND FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS.

           MOVE MKTEMP-BUF TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.
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
              MOVE "LP/LPEMRG" TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY CHLIST-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
