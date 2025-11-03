       IDENTIFICATION DIVISION.
       PROGRAM-ID.      ILUEXT.
       DATE-WRITTEN.    09/08/2022.
      *************************************************************************
      *
      *   ILLINOIS SMALL LOAN UPDATE EXTRACT VERITEC
      *
      * NOTE: MISPAYS HAVE THEIR OWN EXTRACT UPDATE - ILMEXT
      *
      * LOOP THROUGH TRFILE BASED UPON A MONTH RANGE, WILL NOT CROSS MONTHS
      * CREATES A WORK FILE WITH BRNO/ACCT IF ACCOUNT HAD A VALID EVENT
      *    (THIS IS DONE SO WE CAN THEN GO READ THE LPFILE, TRFILE DOES
      *     NOT REWRITE ORIGINAL TRANSACTION IF IT WAS VOIDED)
      * END OF TRFILE FOR BRANCH
      * LOOP THROUGH WORK FILE
      * LOOP THROUGH PAYMENT FILE BACKWARDS, STOP AFTER BEG-DATE REACHED
      * CHECK ALL EVENTS AGAIN, SKIPPING ALL REVERSALS OR REVERSED
      *
      * ONLY INCLUDES LN-ORGST = "IL"
      * ONLY INCLUDES TRANSACTIONS FOR OWNING BRANCH
      * SKIPS LOANDATE PRIOR TO 8/1/2022
      * SKIPS CLASS 50 AND 55
      *
      * INCLUDES THE FOLLOWING TRANSACTIONS;
      *   PO, PB, PA, PW, RN, AC, PL, P2, P3, SX, SS, SV, OT
      *   OR ANY TRANSACTIONS THAT HAD A LATE CHARGE ASSESSED
      *   THE "PA" IS ONLY INCLUDED IF IT PAID THE ACCOUNT
      *   OFF OR HAD LATE CHG ASSESSED
      *
      * REPORTS THE FOLLOWING EVENTS;
      * LP - LATE PAYMENT        (TP-LCDUE)
      * LA - LOAN ACCELERATED    (AC)
      * CL - CLOSED              (PO,PB,PW,PA,RN)
      * WO - WRITE OFF           (PL,P2,P3)
      * IS - INVOLUNTARY REPO    (SX, LS-VOL-INVOL 'I')
      * VS - VOLUNTARY SURRENDER (SV, LS-VOL-INVOL 'V')
      * SS - SALE OF SECURITY    (SS)
      * RS - RETURN OF SECURITY  (SV)
      * NF - NSF FEE CHARGED     (OT, REFCD 'NSFEE')
      * CR - CANCELLED/RESCINDED (TL RECS LNAMT < 0)
      *
      * VERITEC NOTED THE UNIQUE LOAN ID IS NOT REQUIRED, ONLY THE
      *   TRANSACTION ID (VERITEC-TRANS-ID)
      *
      * REV:
      *  20220908 BAH NEW LAYOUT, NEW GUIDELINES #1570
      *  20221024 BAH CHANGE ALL TRDATE TO PAYDATE FOR BACKDATING #1570B
      *  20221108 BAH TOTAL AMOUNT PAID WITH RENEWALS SHOULD INCLUDE THE
      *           AMOUNT ROLLED INTO THE NEW LOAN USED TO CLOSE OUT THE
      *           REPORTED ACCOUNT, #1577
      *  20231009 RMZ PERFORMANCE TUNING - ADDED STORED PROCEDURE
      *  20240628 SKIP CLASS 55 #1662 S35Q-75
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
       COPY "LIBGB/GBFSWKI.CPY".
       COPY "LIBSP/SPFSILLINOIS.CPY".

       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".
       COPY "LIBSP/SPFDILLINOIS.CPY".
      * WK RECORD
       FD  WK-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK-REC.
           03  WK-KEY.
               05  WK-BRNO         PIC 9(4).
               05  WK-ACCTNO       PIC 9(8).

      ******************************************************************
      *
      *     W O R K I N G - S T O R A G E   S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)SP/ILUEXT.CBL S35 07/08/24-09:27:32 >".

       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01TRP.CPY".
       COPY "LIBLP/LP01TRP_SQL.CPY".
       COPY "LIBLP/LPWSTRP.CPY".
       COPY "LIBLP/LP01TRL.CPY".
       COPY "LIBLP/LP01TRL_SQL.CPY".
       COPY "LIBLP/LPWSTRL.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01LP.CPY".
       COPY "LIBLP/LP01LP_SQL.CPY".
       COPY "LIBLP/LPWSLP.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01LSM.CPY".
       COPY "LIBLP/LP01LSM_SQL.CPY".
       COPY "LIBLP/LPWSLSM.CPY".
       COPY "LIBLP/LP01LSX.CPY".
       COPY "LIBLP/LP01LSX_SQL.CPY".
       COPY "LIBLP/LPWSLSX.CPY".
       COPY "LIBSP/SPWSILLINOIS.CPY".
       COPY "LIBSP/ILLPATH.CPY".

      *----------------------------------------------------------------
       01  WK-IFN        PIC XX       VALUE "WK".

       01  JUSTIFY-LEFT          PIC X          VALUE "L".

       01  WORKING-AMOUNT        PIC S9(7)V99 COMP-3.
       01  VERITEC-TRANS-ID      PIC X(12).
       01  SAVE-EVENT-DATE       PIC 9(8).

       01  TRL1-CONN-OPEN-SW      PIC X        VALUE "N".
       01  TRP1-CONN-OPEN-SW      PIC X        VALUE "N".

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
           03  BEG-DATE               PIC 9(8).
           03  END-DATE               PIC 9(8).
           03  BEG-BRANCH             PIC 9(4).
           03  END-BRANCH             PIC 9(4).
           03  GROUP-FLAG             PIC X.
           03  ENT-GROUP              PIC X(4).

       01  VERSION-CONTROL     PIC X    VALUE "A".

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
           03  FILLER           PIC X(8)    VALUE " ".
           03  H-NAME           PIC X(40)    VALUE " ".
           03  FILLER           PIC X(8)    VALUE " ".
           03  FILLER           PIC X(5)     VALUE "PAGE ".
           03  H-PAGE-NO        PIC ZZZ9.

      *-----------------------------------------------------------------
       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(3)     VALUE " ".
           03  H-TITLE          PIC X(37)    VALUE
           "ILLINOIS SMALL LOAN UPDATE EXTRACTION".


      **********************
      *   MISC WORKERS     *
      **********************
       01  FIRST-TIME           PIC X        VALUE "Y".
       01  RECORDS-FOUND        PIC X        VALUE " ".
           88  NO-RECORDS-FOUND              VALUE "N".

       01  MESS-LIST            PIC X(5)     VALUE "MESS.".
       01  WK-DISPLAY-MESS.
           03  FILLER           PIC X(29)    VALUE
           "BUILDING EXTRACTION FILE....".
           03  DISPLAY-MESS-BR  PIC 9(4).
           03  FILLER           PIC X(10).

       01  RECORD-COUNT           PIC 9(9)      VALUE 0.

       01  DETAIL-LINE          PIC X(80) VALUE SPACES.

       01  RANGE-LN REDEFINES DETAIL-LINE.
           03  RANGE-LINE        PIC X(21).
           03  RANGE-SELECTION   PIC X(59).
           03  RANGE-SELNUM     REDEFINES RANGE-SELECTION.
               05  RANGE-DATE    PIC 9999/99/99.

       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBLP/TFRMEMOW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/ILUEXT_DEF.CPY".
       COPY "LIBSP/ILUEXT_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *****************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      *****************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBSP/ILUEXT_SCN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE WK-FILE ILLINOIS-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-TRP1-FILE.
           PERFORM CLOSE-TRP1-CONNECTION.
           PERFORM CLOSE-TRL1-FILE.
           PERFORM CLOSE-TRL1-CONNECTION.
           PERFORM CLOSE-LSM1-FILE.
           PERFORM CLOSE-LSX1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-LP1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE
               WK-FILE ILLINOIS-FILE.
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
           MOVE "ILUEXT" TO FORM-PROG VDU-FORM-NAME.

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

       NEXT-BRANCH-NO.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-FG = 9 OR BR-NO > END-BRANCH
              GO TO END-ROUTINE.

       MOVE-BRANCH-PATH-INFO.

           IF EXT-ROUTE-BUF = "00"
              MOVE SPACES TO WR-BUF
              STRING "EXTRACTION IN PROGRESS...........", BR-NO
                     DELIMITED "  " INTO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           MOVE EXT-FILPATH-BASE TO FILPATH-BASE.
           MOVE BR-MACHINE       TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH     TO FILPATH-DATA.

           MOVE "B"        TO TRP-PATH-OVERRIDE
                              TRL-PATH-OVERRIDE
                              LN-PATH-OVERRIDE
                              LP-PATH-OVERRIDE
                              BW-PATH-OVERRIDE
                              LSX-PATH-OVERRIDE
                              LSM-PATH-OVERRIDE.

           PERFORM OPEN-LN1-FILE.
           PERFORM OPEN-LSM1-FILE.
           PERFORM OPEN-LSX1-FILE.
           PERFORM OPEN-BW1-FILE.
           PERFORM OPEN-LP1-FILE.

           IF PRU-LP NOT = "VDU" AND NOT = "PRU"
              MOVE BR-NO TO DISPLAY-MESS-BR
              MOVE WK-DISPLAY-MESS TO MESS
              PERFORM SEND-LEGEND.

           PERFORM OPEN-WK-FILE-OUTPUT.
           PERFORM CLOSE-WK-FILE.
           PERFORM OPEN-WK-FILE.

           PERFORM SORT-TRL-FILE.
           PERFORM SORT-TRP-FILE.

      ******************************************
           PERFORM UPDATE-EVENTS.
      ******************************************

       END-OF-GROUP.
           PERFORM CLOSE-TRP1-FILE.
           PERFORM CLOSE-TRL1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-LP1-FILE.
           PERFORM CLOSE-LSM1-FILE.
           PERFORM CLOSE-LSX1-FILE.
           PERFORM CLOSE-WK-FILE.

           GO TO NEXT-GROUP-LOC.

       END-ROUTINE.

           IF NO-RECORDS-FOUND
              GO TO END-PROGRAM.

           PERFORM HEAD-PAGE.
           PERFORM PRINT-RANGE-LINES.

           GO TO END-PROGRAM.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************
           MOVE ILUEXT-QUEUE-POS TO Q-POS.
           MOVE ILUEXT-COPIES-POS TO C-POS.
           MOVE ILUEXT-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

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
              MOVE "EXTRACTION IN PROGRESS..........." TO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           PERFORM OPEN-PR-FILE.

           CALL "C$TOLOWER" USING ILU-FTP-PATH, VALUE 9.
           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD-DD TO ILU-PATH-DD.
           MOVE DATE-YYYYMMDD-MM TO ILU-PATH-MM.
           MOVE DATE-YYYYMMDD-YY TO ILU-PATH-YY.

           PERFORM LOAD-ILU-FILE.
           MOVE ILU-PATH TO ACCESS-BUF ILLINOIS-PATH.
           PERFORM ACCESS-CALL.
           IF STAT NOT = "00"
              GO TO CONTINUE-INITIALIZE.

           IF EXT-ROUTE-BUF NOT = "00"
              GO TO CONTINUE-INITIALIZE.
       OVERWRITE-QUESTION.
           MOVE "ILU FILE EXISTS FOR THIS DATE. CONTINUE? (Y/N)"
                           TO MESS.
           MOVE MESS TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE "E  A000ANS." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS NOT = "00" GO TO OVERWRITE-QUESTION.
           IF VDUBUF = "Y" GO TO CONTINUE-INITIALIZE.
           IF VDUBUF NOT = "N"
              GO TO OVERWRITE-QUESTION.
           GO TO ENDIT.

       CONTINUE-INITIALIZE.
           PERFORM OPEN-ILLINOIS-FILE-OUTPUT.

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           IF STAT NOT = "00"
              MOVE "CANNOT MAKE WORK FILE" TO MESS
              PERFORM SEND-MESS
              GO TO ENDIT.

           PERFORM OPEN-WK-FILE-OUTPUT.
           PERFORM CLOSE-WK-FILE.
           PERFORM OPEN-WK-FILE.

           GO TO EXIT-INITIALIZE.

       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.

       EXIT-INITIALIZE.
           EXIT.

      ******************************************************************
       EO-EXEC-AUDIT SECTION.
      ******************************************************************
           MOVE EXT-EOBUF-DATE1 TO BEG-DATE.
           MOVE EXT-EOBUF-DATE2 TO END-DATE.
      * BEGIN DATE CAN NOT BE PRIOR TO 8/1/22
           IF EXT-EOBUF-DATE1 < 20220801
              MOVE 20220801 TO BEG-DATE
              MOVE 20220831 TO END-DATE.

       EXIT-EO-EXEC-AUDIT.
           EXIT.

      ******************************************************************
       ENTER-PARAMETERS SECTION.
      ******************************************************************
       ENTER-PARM.

           IF VDUSTATUS = "1U"
              IF NOT (EXT-ROUTE-BUF = "40" OR "50")
                 GO TO ENDDT.

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
       BEGDT.

           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO EXIT-PARM.

           MOVE "ERNM060BEGDATE." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BEGDT.

           MOVE VDU-DATE-YYYYMMDD TO BEG-DATE.

      * ONLY GET LOANS 8/1/22 AND BEYOND
           IF BEG-DATE < 20220801
              MOVE "S  A080BEGDATE." TO VDU
              MOVE 20220801          TO BEG-DATE DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO VDU-DATE-EDIT
              PERFORM SCREENIO
              MOVE BEG-DATE        TO VDUNUM0.

      * DEFAULT END-DATE TO LAST DAY OF MONTH FROM BEG-DATE
           MOVE BEG-DATE TO NDTE-DATE.
           MOVE 66       TO NDTE-DD.
           PERFORM NEW-DATE-CALCULATION.
           MOVE "S  A080ENDDATE." TO VDU.
           MOVE NDTE-DATE TO END-DATE DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO VDU-DATE-EDIT.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
       ENDDT.

           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO EXIT-PARM.

           MOVE "ERNM060ENDDATE." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1U" GO TO BR-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BEGDT.

           MOVE VDU-DATE-YYYYMMDD TO END-DATE.

           IF BEG-DATE NOT = 0
              IF END-DATE < BEG-DATE
                 MOVE "INVALID DATE RANGE" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENDDT.

           MOVE BEG-DATE TO NUM-DATE.
           MOVE END-DATE TO SYS-DATE.
           IF (NUM-MO NOT = S-MM )OR (NUM-YR NOT = S-YY)
              MOVE "DATE RANGE CAN NOT CROSS MONTHS" TO MESS
              PERFORM SEND-MESS
              GO TO BEGDT.

       EXIT-PARM.
           EXIT.

      ******************************************************************
       DISPLAY-PARAMETERS SECTION.
      ******************************************************************

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

           IF ( (BEG-BRANCH = 0) AND (END-BRANCH = 0) )
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE "USER BRANCH ONLY" TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO.

      ************************************
       SORT-TRL-FILE SECTION.
      ************************************
           PERFORM OPEN-TRL1-FILE.

           MOVE BR-NO               TO TL-BRNO
                                       QTRL1-WBEG-BRNO
                                       QTRL1-WEND-BRNO.
           MOVE BEG-DATE            TO TL-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRL1-WBEG-DATE.
           MOVE END-DATE            TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRL1-WEND-DATE.
           MOVE 0                   TO TL-CLASS
                                       QTRL1-WBEG-CLASS.
           MOVE ALL "9"             TO QTRL1-WEND-CLASS.
           MOVE 0                   TO TL-ACCTNO TL-LAWCODE
                                       TL-LINE
                                       QTRL1-WBEG-ACCTNO
                                       QTRL1-WBEG-LAWCODE
                                       QTRL1-WBEG-LINE.
           MOVE ALL "9"        TO QTRL1-WEND-ACCTNO
                                  QTRL1-WEND-LAWCODE
                                  QTRL1-WEND-LINE.

           PERFORM START-TRL1-FILE. *> SSP - TRL,LN,LSM
       SORT-NEXT-TRL.
           PERFORM READ-TRL1-FILE-NEXT. *> EMBEDDED - TRP,LN,LSM
           IF (IO-FG NOT = 0) OR (TRL-PATH-OWNBR NOT = TL-BRNO)
              GO TO EXIT-SORT-TRL-FILE.

           IF TL-DATE > END-DATE
              GO TO EXIT-SORT-TRL-FILE.

           IF TL-BRNO > END-BRANCH
              GO TO EXIT-SORT-TRL-FILE.

      * IF LNAMT IS POSITIVE, ITS A NEW LOAN, WE ONLY WANT VOIDS
           IF TL-LNAMT > 0
              GO TO SORT-NEXT-TRL.

           IF TL-CLASS = 50 OR 55
              GO TO SORT-NEXT-TRL.

      *    MOVE TL-BRNO   TO LN-OWNBR.
      *    MOVE TL-ACCTNO TO LN-ACCTNO.
      *    PERFORM READ-LN1-FILE.
      *    IF IO-FG NOT = 0
           IF TL-BRNO NOT = LN-OWNBR
              GO TO SORT-NEXT-TRL.

           IF LN-ORGST NOT = "IL"
              GO TO SORT-NEXT-TRL.

      * IF NO PAYOFF DATE, THEN IT WASNT VOIDED
           IF LN-POFFDATE = 0
              GO TO SORT-NEXT-TRL.
             
      * IF VOIDED SAME MONTH/YEAR AS BOOKED, SKIP IT, IT WAS NEVER REPORTED
      * IN THE NEW LOAN EXTRACT
           MOVE LN-LOANDATE TO NUM-DATE.
           MOVE LN-POFFDATE TO SYS-DATE.
           IF (NUM-MO = S-MM AND NUM-YR = S-YY)
              GO TO SORT-NEXT-TRL.

      * GET VERITEC ID AND LOOK FOR TRANSFER INFO
      *    PERFORM GET-LS-M-RECORD.
           MOVE LSM-MISC-FIELD(15)  TO VERITEC-TRANS-ID.

           MOVE LN-POFFDATE TO SAVE-EVENT-DATE.

           PERFORM CREATE-SHARED-FIELDS.

      * CANCELLED OR RESCINDED
           MOVE "CR"       TO ILU-EVENT.

           PERFORM STRING-AND-WRITE.

           GO TO SORT-NEXT-TRL.

       EXIT-SORT-TRL-FILE.
           EXIT.


      ************************************
       SORT-TRP-FILE SECTION.
      ************************************

           PERFORM OPEN-TRP1-FILE.

           MOVE BR-NO               TO TP-BRNO
                                       QTRP1-WBEG-BRNO
                                       QTRP1-WEND-BRNO.
           MOVE BEG-DATE            TO TP-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRP1-WBEG-DATE.
           MOVE END-DATE            TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRP1-WEND-DATE.
           MOVE 0                   TO TP-CLASS
                                       QTRP1-WBEG-CLASS.
           MOVE ALL "9"             TO QTRP1-WEND-CLASS.
           MOVE 0                   TO TP-ACCTNO TP-LINE
                                       QTRP1-WBEG-ACCTNO
                                       QTRP1-WBEG-LINE.
           MOVE ALL "9"             TO QTRP1-WEND-ACCTNO
                                       QTRP1-WEND-LINE.

           MOVE TP-DATE              TO NDTE-DATE
           MOVE 1                    TO NDTE-HOLD
           PERFORM INCREMENT-DAYS
           MOVE NDTE-DATE            TO SQL-DATE-YYYYMMDD
           PERFORM SQL-SET-DATE
           MOVE SQL-DATE-YYYY-MM-DD  TO QTRP1-WSBEG-DATE.

           IF ( TP-CLASS = 999 )
              MOVE TP-CLASS          TO QTRP1-WSBEG-CLASS
           ELSE
              COMPUTE QTRP1-WSBEG-CLASS  = TP-CLASS + 1.

           IF ( TP-ACCTNO = 99999999 )
              MOVE TP-ACCTNO         TO QTRP1-WSBEG-ACCTNO
           ELSE
              COMPUTE QTRP1-WSBEG-ACCTNO = TP-ACCTNO + 1.

           IF ( TP-LINE = 999 )
              MOVE TP-LINE           TO QTRP1-WSBEG-LINE
           ELSE
              COMPUTE QTRP1-WSBEG-LINE   = TP-LINE + 1.

           PERFORM START-TRP1-FILE. *> SSP - TRP,LN,LSM

       SORT-NEXT-TRP.
           PERFORM READ-TRP1-FILE-NEXT. *> EMBEDDED - TRP,LN,LSM
           IF (IO-FG NOT = 0) OR (TRP-PATH-OWNBR NOT = TP-BRNO)
              GO TO EXIT-SORT-TRP-FILE.

           IF TP-DATE > END-DATE
              GO TO EXIT-SORT-TRP-FILE.

      * SKIP REVERSALS
           IF TP-REV = "Y"
              GO TO SORT-NEXT-TRP.

           IF NOT (TP-TRCD = "PO" OR "PB" OR "PA" OR "PW" OR "RN"
                           OR "AC" OR "PL" OR "P2" OR "P3" OR "SX"
                            OR "SS" OR "SV" OR "OT")
      * HAVE TO CHECK ALL TRANSACTIONS FOR LATE PAYMENT
              IF TP-LCDUE = 0 OR 2020202.02 OR 0202020.20
                 GO TO SORT-NEXT-TRP.

      * ONLY WANT "PA" THAT PAID OUT THE ACCOUNT (CL) OR ASSESSED LATE CHARGE
           IF TP-TRCD = "PA"
              IF TP-CURBAL NOT = 0
                 IF TP-LCDUE = 0 OR 2020202.02 OR 0202020.20
                    GO TO SORT-NEXT-TRP.

           IF TP-CLASS = 50 OR 55
              GO TO SORT-NEXT-TRP.

      * SKIP INTERBRANCH IF POSTING BRANCH
           IF TP-OTHBR-FG = "C"
              GO TO SORT-NEXT-TRP.

      *    MOVE TP-BRNO   TO LN-OWNBR.
      *    MOVE TP-ACCTNO TO LN-ACCTNO.
      *    PERFORM READ-LN1-FILE.
      *    IF IO-BAD
           IF TP-BRNO NOT = LN-OWNBR
              GO TO SORT-NEXT-TRP.

           IF LN-ORGST NOT = "IL"
              GO TO SORT-NEXT-TRP.

      * THIS NEW PROGRAM START WITH ALL LOANS MADE ON OR AFTER 8/1/22

           IF LN-CONVERCD = "Y"
              MOVE LN-ENTDATE TO NUM-DATE
           ELSE
              MOVE LN-LOANDATE TO NUM-DATE.
           IF NUM-DATE < 20220801
              GO TO SORT-NEXT-TRP.

      * SOME TYPE OF EVENT TOOK PLACE, SAVE IN A WORK FILE TO LATER READ
      * THE PAYMENT FILE TO SEE IF ANY WERE REVERSED AND SHOULD BE SKIPPED
           MOVE LN-OWNBR  TO WK-BRNO.
           MOVE LN-ACCTNO TO WK-ACCTNO.
           PERFORM READ-WK-FILE.
           IF IO-FG NOT = 0
              MOVE LN-OWNBR  TO WK-BRNO
              MOVE LN-ACCTNO TO WK-ACCTNO
              PERFORM WRITE-WK-FILE.

           GO TO SORT-NEXT-TRP.

       EXIT-SORT-TRP-FILE.
           EXIT.

      ****************************************
       UPDATE-EVENTS SECTION.
      ****************************************
           MOVE 0 TO WK-BRNO WK-ACCTNO.
           PERFORM START-WK-FILE.
       UPDATE-EVENTS-NEXT-WK.
           PERFORM READ-WK-FILE-NEXT.
           IF IO-FG NOT = 0
              GO TO UPDATE-EVENTS-EXIT.

           MOVE WK-BRNO   TO LN-OWNBR.
           MOVE WK-ACCTNO TO LN-ACCTNO.
           PERFORM READ-LN1-FILE. *> EMBEDDED - LNFILE
           IF IO-FG NOT = 0
              GO TO UPDATE-EVENTS-NEXT-WK.
           
           MOVE LN-OWNBR  TO LP-BRNO
                             QLP1-WBEG-BRNO
                             QLP1-WEND-BRNO.
           MOVE LN-ACCTNO TO LP-ACCTNO
                             QLP1-WBEG-ACCTNO
                             QLP1-WEND-ACCTNO.
           MOVE ALL "9"   TO LP-SEQNO
                             QLP1-WEND-SEQNO.
           MOVE ALL "0"   TO QLP1-WBEG-SEQNO.

           PERFORM START-LP1-FILE-NOT-GREATER. *> EMBEDDED - LPFILE
       UPDATE-EVENTS-NEXT-LP.
           PERFORM READ-LP1-FILE-PREVIOUS. *> EMBEDDED - LPFILE
           IF IO-FG NOT = 0
              GO TO UPDATE-EVENTS-NEXT-WK.

      * DONT WANT TO HIT SEQ 1 AND BE A "PB", THINKS ITS A PAIDOUT!
           IF LP-ACCTNO NOT = LN-ACCTNO OR
               LP-SEQNO = 1
              GO TO UPDATE-EVENTS-NEXT-WK.

      * CAN STOP IF YOU PASS 8/1/2022
           IF LP-PAYDATE < 20220801
              GO TO UPDATE-EVENTS-NEXT-WK.

      * SKIP REVERSALS
           IF LP-REV = "Y" OR LP-TRCD = "RV"
              GO TO UPDATE-EVENTS-NEXT-LP.

           IF NOT (LP-TRCD = "PO" OR "PB" OR "PA" OR "PW" OR "RN"
                           OR "AC" OR "PL" OR "P2" OR "P3" OR "SX" 
                            OR "SS" OR "SV" OR "OT")
      * HAVE TO CHECK ALL TRANSACTIONS FOR LATE PAYMENT (ASSESSED ONLY)
              IF LP-APINTOWE = 0 
                 GO TO UPDATE-EVENTS-NEXT-LP.

      * ONLY WANT "PA" THAT PAID OUT THE ACCOUNT (CL) OR ASSESSED LATE CHARGE
           IF LP-TRCD = "PA"
              IF LP-CURBAL NOT = 0 
                 IF LP-APINTOWE = 0 
                    GO TO UPDATE-EVENTS-NEXT-LP.

           IF LP-PAYDATE < BEG-DATE
      * READING BACKWARDS, YOU ARE DONE WHEN YOU ARE LESS THAN
              GO TO UPDATE-EVENTS-NEXT-WK.

           IF LP-PAYDATE > END-DATE
      * READING BACKWARDS, YOU NEED NEXT LP
              GO TO UPDATE-EVENTS-NEXT-LP.
             
      * GET VERITEC ID AND LOOK FOR TRANSFER INFO
           PERFORM GET-LS-M-RECORD.
 
      ******************
      ******************
      * SET TP FIELDS SO WE CAN SHARE THE UPDATE SECTION!
      ******************
      ******************

           MOVE LN-ACCTNO   TO TP-ACCTNO.
           MOVE LP-PAYDATE  TO TP-PAYDATE.
           MOVE LP-APINTOWE TO TP-LCDUE.
           MOVE LP-TRCD     TO TP-TRCD.
           MOVE LP-CURBAL   TO TP-CURBAL.
           MOVE LP-REFNO    TO TP-REFNO.
           MOVE LP-INTDUE   TO TP-INTDUE.
           MOVE LP-APCUR    TO TP-APCUR.

           PERFORM CREATE-UPDATE-RECORD.
         
      * KEEP LOOKING FOR ANY AND ALL TRANSACTIONS FROM THIS ACCTNO
           GO TO UPDATE-EVENTS-NEXT-LP.

       UPDATE-EVENTS-EXIT.
           EXIT.

      ******************************************************************
       WRITE-DETAIL-LINE SECTION.
      ******************************************************************
           ADD LN-FEED LN-COUNT GIVING LN-WK.

           IF LN-WK > EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.

           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      ******************************************************************
       PRINT-RANGE-LINES SECTION.
      ******************************************************************
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
      
           MOVE "BEGINNING DATE     :" TO RANGE-LINE.
           MOVE BEG-DATE               TO RANGE-DATE.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "ENDING DATE        :" TO RANGE-LINE.
           MOVE END-DATE               TO RANGE-DATE.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "STARTING TIME      :" TO RANGE-LINE.
           MOVE STARTING-TIME         TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "ENDING   TIME      :" TO RANGE-LINE.
           PERFORM GET-TIME.
           MOVE TIME-EDIT             TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "OUTPUT FILE PATH" TO RANGE-LINE.
           MOVE ILU-PATH           TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "# RECORDS EXTRACTED:" TO RANGE-LINE.
           MOVE RECORD-COUNT TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

      ******************************************************************
       HEAD-PAGE SECTION.
      ******************************************************************

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

      ***********************************
       CREATE-UPDATE-RECORD SECTION.
      ***********************************

           MOVE TP-PAYDATE TO SAVE-EVENT-DATE.

           PERFORM CREATE-SHARED-FIELDS.

      * LATE PAYMENT - HAVE TO CREATE AN EVENT FOR THE LATE PAYMENT "LP"
      *                BUT ALSO MIGHT NEED ANOTHER EVENT CREATED.
           IF NOT (TP-LCDUE = 0 OR 2020202.02 OR 0202020.20)
              MOVE "LP"       TO ILU-EVENT
              MOVE TP-LCDUE   TO ILU-FEE-AMOUNT
              PERFORM STRING-AND-WRITE
      * TEST IF THERE COULD BE ANOTHER EVENT, IF NOT, THEN YOU ARE DONE
              IF NOT (TP-TRCD = "PO" OR "PB" OR "PA" OR "PW" OR "RN"
                           OR "AC" OR "PL" OR "P2" OR "P3" OR "SX" 
                            OR "SS" OR "SV" OR "OT")
                 GO TO EXIT-CREATE-UPDATE
              ELSE
      * NEED TO MAKE SURE IT ISNT A "PA" AGAIN, WITHOUT A PAYOUT
                 IF (TP-TRCD = "PA" AND TP-CURBAL NOT = 0)
                    GO TO EXIT-CREATE-UPDATE
                 ELSE
      * GET THE NEXT EVENT RECORD STARTED
                    PERFORM CREATE-SHARED-FIELDS.

      * INVOLUNTARY/VOLUNTARY SURRENDER
           IF TP-TRCD = "SX" 
              PERFORM GET-LS-REPO
              IF LSX-VOLUNTARY-FG = "I"
                 MOVE "IS"    TO ILU-EVENT
              ELSE
                 MOVE "VS"    TO ILU-EVENT.

      * SALE OF SECURITY
           IF TP-TRCD = "SS"
              MOVE "SS"       TO ILU-EVENT
              MOVE "N"        TO ILU-DEFICIENCY-WAIVED
              IF TP-CURBAL = 0
                 MOVE "N"     TO ILU-SURPLUS-DEFICIENCY
              ELSE
                 MOVE "Y"     TO ILU-SURPLUS-DEFICIENCY.

      * RETURN OF SECURITY TO CUSTOMER
           IF TP-TRCD = "SV"
              MOVE "RS"       TO ILU-EVENT.
  
      * ACCELERATED
           IF TP-TRCD = "AC"
              MOVE "LA"       TO ILU-EVENT
              MOVE TP-CURBAL  TO ILU-DUE-AMOUNT.

      * CLOSED
           IF (TP-TRCD = "PO" OR "PB" OR "PW" OR "RN") OR
                 (TP-TRCD = "PA" AND TP-CURBAL = 0)
              MOVE "CL"          TO ILU-EVENT
              PERFORM SET-CLOSED-REASON
      * TOTAL AMOUNT PAID WITH RENEWALS SHOULD INCLUDE THE AMOUNT ROLLED
      * INTO THE NEW LOAN USED TO CLOSE OUT THE REPORTED ACCOUNT
              IF TP-TRCD = "PB" OR "RN"
                 COMPUTE WORKING-AMOUNT = LN-TOTPAYMNTD + TP-APCUR
                 MOVE WORKING-AMOUNT TO ILU-TOTAL-PAID
              ELSE
      * VERITEC; USE TOTAL OF ALL THE PAYS MADE ON THE LOAN OVER ITS LIFE
                 MOVE LN-TOTPAYMNTD TO ILU-TOTAL-PAID.

      * P3 = BK
           IF TP-TRCD = "PL" OR "P2" OR "P3"
              MOVE "WO"          TO ILU-EVENT
              MOVE "N"           TO ILU-DEBT-SOLD
              IF TP-TRCD = "PL" OR "P2"
                 MOVE "Y" TO ILU-DEBT-CONTINUING
              ELSE
                 MOVE "N" TO ILU-DEBT-CONTINUING.


      * NSF FEE CHARGED
           IF TP-TRCD = "OT" AND TP-REFNO = "NSFEE"
              MOVE "NF"      TO ILU-EVENT
              MOVE TP-INTDUE TO ILU-NSF-FEE.


           PERFORM STRING-AND-WRITE.

       EXIT-CREATE-UPDATE.
           EXIT.


      *********************************************
       CREATE-SHARED-FIELDS SECTION.
      *********************************************

           MOVE SPACES TO ILLINOIS-FIELDS.
 
           MOVE "UPDATE"           TO ILN-ACTION.
           MOVE BR-VERITEC-LICENSE TO ILN-LICENSE-ID.

      * VERITEC NOTED THE UNIQUE LOAN ID IS NOT REQUIRED, ONLY THE
      *   TRANSACTION ID (VERITEC-TRANS-ID), STILL USING THOUGH
           MOVE LN-OWNBR           TO ILN-LENDER-BRNO.
           MOVE LN-ACCTNO          TO ILN-ACCTNO.

           MOVE LN-SSNO(1) TO BW-SSNO.
           PERFORM READ-BW1-FILE. *> EMBEDDED - BWFILE
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           MOVE BW-SSNO            TO ILN-SSNO(1).
           MOVE "SSN"              TO ILN-ID-TYPE(1).
           MOVE BW-FNAME           TO ILN-FNAME(1).
           MOVE BW-LNAME           TO ILN-LNAME(1).
           MOVE BW-ADR1            TO ILN-ADR1(1).
           MOVE BW-ADR2            TO ILN-ADR2(1).
           MOVE BW-CITY            TO ILN-CITY(1).
           MOVE BW-STATE           TO ILN-STATE(1).
           MOVE BW-ZIP             TO ILN-ZIP(1).

      * VERITEC NOTED THE UNIQUE LOAN ID IS NOT REQUIRED, ONLY THE
      *   TRANSACTION ID (VERITEC-TRANS-ID)

      * THIS FIELD WILL IDENTIFY THE ACCOUNT EVEN IF TRANFERRED. IT
      * WILL STAY WITH THE ACCOUNT

           MOVE VERITEC-TRANS-ID TO ILU-TRANS-ID.

           MOVE SAVE-EVENT-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY   TO ILU-EVENT-DATE.

      **********************************************
       STRING-AND-WRITE SECTION.
      **********************************************

      * THE STRINGS ARE SPLIT UP INTO SECTIONS JUST TO MAKE IT SAFER.

           MOVE SPACES TO ILN-HEADER-INFO.
           STRING ILN-ACTION, "|",
                  ILN-LICENSE-ID, "|",
                  ILN-LENDER-ID, "|",
                  ILN-CUST-COUNT, "|"
              DELIMITED BY "  " INTO ILN-HEADER-INFO.

           MOVE SPACES TO ILN-CUST-INFO.
      * COULDNT GET A PERFORM VARYING TO WORK HERE.
           STRING ILN-SSNO(1), "|",
                  ILN-ID-TYPE(1), "|",
                  ILN-FNAME(1), "|",
                  ILN-LNAME(1), "|",
                  ILN-ADR1(1), "|",
                  ILN-ADR2(1), "|",
                  ILN-CITY(1), "|",
                  ILN-STATE(1), "|",
                  ILN-ZIP(1), "|",

                  ILN-SSNO(2), "|",
                  ILN-ID-TYPE(2), "|",
                  ILN-FNAME(2), "|",
                  ILN-LNAME(2), "|",
                  ILN-ADR1(2), "|",
                  ILN-ADR2(2), "|",
                  ILN-CITY(2), "|",
                  ILN-STATE(2), "|",
                  ILN-ZIP(2), "|",

                  ILN-SSNO(3), "|",
                  ILN-ID-TYPE(3), "|",
                  ILN-FNAME(3), "|",
                  ILN-LNAME(3), "|",
                  ILN-ADR1(3), "|",
                  ILN-ADR2(3), "|",
                  ILN-CITY(3), "|",
                  ILN-STATE(3), "|",
                  ILN-ZIP(3), "|",

                  ILN-SSNO(4), "|",
                  ILN-ID-TYPE(4), "|",
                  ILN-FNAME(4), "|",
                  ILN-LNAME(4), "|",
                  ILN-ADR1(4), "|",
                  ILN-ADR2(4), "|",
                  ILN-CITY(4), "|",
                  ILN-STATE(4), "|",
                  ILN-ZIP(4), "|"
              DELIMITED BY "  " INTO ILN-CUST-INFO.

           MOVE SPACES TO ILN-LOAN-INFO.
           STRING ILN-LOAN-TYPE, "|",
                  ILN-AGREEMENT-DATE, "|",
                  ILN-MATURITY-DATE, "|",
                  ILN-PRINCIPAL-AMOUNT, "|",
                  ILN-TOTAL-AMOUNT, "|",
                  ILN-INTEREST-TYPE, "|",
                  ILN-NUMBER-OF-PAYS, "|",
                  ILN-SCHEDULED-PAYMENT, "|",
                  ILN-SECURITY, "|",
                  ILN-SECURITY-DESC, "|",
                  ILN-APRATE, "|",
                  ILN-PLPA-APRATE, "|",
                  ILN-PAYOFF, "|",
                  ILN-VIN, "|"
              DELIMITED BY "  " INTO ILN-LOAN-INFO.

           MOVE SPACES TO ILU-UPDATE-INFO.
           CALL "C$JUSTIFY" USING ILU-FEE-AMOUNT, JUSTIFY-LEFT.
           CALL "C$JUSTIFY" USING ILU-DUE-AMOUNT, JUSTIFY-LEFT.
           CALL "C$JUSTIFY" USING ILU-TOTAL-PAID, JUSTIFY-LEFT.
           CALL "C$JUSTIFY" USING ILU-NSF-FEE, JUSTIFY-LEFT.
           STRING ILU-TRANS-ID, "|",
                  ILU-EVENT, "|",
                  ILU-EVENT-DATE, "|",
                  ILU-FEE-AMOUNT, "|",
                  ILU-DUE-AMOUNT, "|",
                  ILU-TOTAL-PAID, "|",
                  ILU-CLOSE-REASON, "|",
                  ILU-CLOSE-DESC, "|",
                  ILU-DEBT-SOLD, "|",
                  ILU-DEBT-CONTINUING, "|",
                  ILU-SURPLUS-DEFICIENCY, "|",
                  ILU-DEFICIENCY-WAIVED, "|",
                  ILU-NSF-FEE
              DELIMITED BY "  " INTO ILU-UPDATE-INFO.

           MOVE SPACES TO ILLINOIS-REC.

           STRING ILN-HEADER-INFO,
                  ILN-CUST-INFO,
                  ILN-LOAN-INFO,
                  ILU-UPDATE-INFO,
                  ILN-CARRAGE-RET
              DELIMITED BY "  " INTO ILLINOIS-REC.

           PERFORM WRITE-ILLINOIS-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

           ADD 1    TO RECORD-COUNT.
           MOVE "N" TO FIRST-TIME.
           MOVE "Y" TO RECORDS-FOUND.

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

      ************************************************
       FORM-RESET SECTION.
      ************************************************
           MOVE EXT-CONAME-CONAME TO ILUEXT-CONAME.
           MOVE EXT-CONAME-SYSDATE TO ILUEXT-SYSDATE.
           DISPLAY ILUEXT-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE ILUEXT-FORM-FIELDS TO WR-BUF.
           MOVE ILUEXT-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.


      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE ILUEXT-HELP--FILE TO HELP-LINK-FILE.
           MOVE ILUEXT-HELP--DIR TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/ILUEXT_EVA.CPY".
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
       GET-LS-M-RECORD.
           MOVE LN-OWNBR  TO LSM-BRNO.
           MOVE LN-ACCTNO TO LSM-ACCTNO.
           MOVE 1         TO LSM-SEQNO.
           PERFORM READ-LSM1-FILE. *> EMBEDDED - LSMFILE
           IF IO-FG NOT = 0
              MOVE SPACES TO VERITEC-TRANS-ID
           ELSE
              MOVE LSM-MISC-FIELD(15)    TO VERITEC-TRANS-ID.
      *----------------------------------------------------------------
       SET-CLOSED-REASON.
           IF TP-TRCD = "PA" OR "PO"
              MOVE "PF"       TO ILU-CLOSE-REASON
           ELSE
              MOVE "OT"       TO ILU-CLOSE-REASON
      * CLOSED REASON DESCRIPTION ONLY SET IF "OT"
              IF TP-TRCD = "PW"
                 MOVE "CHARGE OFF BALANCE WAIVED" TO ILU-CLOSE-DESC
              ELSE
                 MOVE "REFINANCED" TO ILU-CLOSE-DESC.

       GET-LS-REPO.
           MOVE LN-OWNBR  TO LSX-BRNO.
           MOVE LN-ACCTNO TO LSX-ACCTNO.
           MOVE 1         TO LSX-SEQNO.
           PERFORM READ-LSX1-FILE. *> EMBEDDED - LSXFILE
           IF IO-FG NOT = 0
              MOVE SPACES TO LSX-VOLUNTARY-FG.
       SEND-LEGEND.
           MOVE MESS TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
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
       COPY "LIBLP/LPTRPGS_SQL.CPY".
       COPY "LIBLP/LPTRLGS_SQL.CPY".
       COPY "LIBLP/LPLSMGS_SQL.CPY".
       COPY "LIBLP/LPLSXGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLPGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".

      * COPY "LIBLP/LPTRP1RN.CPY".
      * COPYMEMBER: LIBLP/LPTRP1RN
      * 20220923 BAH ADDED TP-LCDUE *1570
       LOAD-TRP1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( TRP-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( TRP-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO TRP-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO TRP-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO TRP-ALL-DATA
              MOVE TRP-ALL-PATH            TO TRP-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( TRP-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO TRP-ALL-BASE
              MOVE FILPATH-MACHINE         TO TRP-ALL-MACHINE
              MOVE FILPATH-DATA            TO TRP-ALL-DATA
              MOVE TRP-ALL-PATH            TO TRP-PATH.

      *-----------------------------------------------------------------
       OPEN-TRP1-FILE.
           PERFORM LOAD-TRP1-FILE.
           PERFORM OPEN-IT.
           MOVE TRP-PATH-SQL TO E-FILE.

           IF TRP1-CONN-OPEN-SW = "N"
              MOVE "Y" TO TRP1-CONN-OPEN-SW
              EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS TRP1_CONNECT
                       USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
              END-EXEC.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       START-TRP1-FILE. *> SSP - TRP,LN,LSM

           PERFORM START-IT.
           MOVE TRP-PATH-OWNBR  TO TP-BRNO.
           MOVE TRP-PATH-SQL    TO E-FILE.
           MOVE TRP1-KEY        TO E-KEYX.

           IF ( TRP1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-TRP1-FILE.

           IF ( QTRP1-WEND-LINE = ZEROES    ) OR
              ( QTRP1-WEND-LINE NOT NUMERIC )
              MOVE TRP-PATH-OWNBR        TO QTRP1-WBEG-BRNO
              MOVE TRP-PATH-OWNBR        TO QTRP1-WEND-BRNO
              MOVE TP-DATE               TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTRP1-WBEG-DATE
              MOVE 99991231         TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTRP1-WEND-DATE
              MOVE TP-CLASS             TO QTRP1-WBEG-CLASS
              MOVE TP-ACCTNO            TO QTRP1-WBEG-ACCTNO
              MOVE TP-LINE              TO QTRP1-WBEG-LINE
              MOVE ALL "9"              TO QTRP1-WEND-CLASS
                                           QTRP1-WEND-ACCTNO
                                           QTRP1-WEND-LINE
      *- - - -{ ADD ONE TO WHERE SCAN CONTINUE FIELDS
              MOVE TP-DATE              TO NDTE-DATE
              MOVE 1                    TO NDTE-HOLD
              PERFORM INCREMENT-DAYS
              MOVE NDTE-DATE            TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTRP1-WSBEG-DATE

              IF ( TP-CLASS = 999 )
                 MOVE TP-CLASS          TO QTRP1-WSBEG-CLASS
              ELSE
                 COMPUTE QTRP1-WSBEG-CLASS  = TP-CLASS + 1
              END-IF

              IF ( TP-ACCTNO = 99999999 )
                 MOVE TP-ACCTNO         TO QTRP1-WSBEG-ACCTNO
              ELSE
                 COMPUTE QTRP1-WSBEG-ACCTNO = TP-ACCTNO + 1
              END-IF

              IF ( TP-LINE = 999 )
                 MOVE TP-LINE           TO QTRP1-WSBEG-LINE
              ELSE
                 COMPUTE QTRP1-WSBEG-LINE   = TP-LINE + 1.

      * STORED PROCEDURE SELECT STATEMENT: 
      *
      *    SELECT TRPFILE.TP_BRNO,
      *           CAST(TRPFILE.TP_DATE    AS VARCHAR(10)),
      *           TRPFILE.TP_CLASS,
      *           TRPFILE.TP_ACCTNO,
      *           TRPFILE.TP_LINE,
      *           TRPFILE.TP_TRCD,
      *           TRPFILE.TP_REFNO,
      *           CAST(TRPFILE.TP_PAYDATE AS VARCHAR(10)),
      *           TRPFILE.TP_INTDUE,
      *           TRPFILE.TP_APCUR,
      *           TRPFILE.TP_CURBAL,
      *           TRPFILE.TP_OTHBR_FG,
      *           TRPFILE.TP_REV,
      *           TRPFILE.TP_LCDUE,
      *           ISNULL(LNFILE.LN_OWNBR, 0),
      *           ISNULL(LNFILE.LN_ACCTNO, 0),
      *           ISNULL(LNFILE.LN_SSNO1, 0),
      *           ISNULL(LNFILE.LN_MAKERCD1,''),
      *           ISNULL(LNFILE.LN_SSNO2, 0),
      *           ISNULL(LNFILE.LN_MAKERCD2,''),
      *           ISNULL(LNFILE.LN_SSNO3, 0), 
      *           ISNULL(LNFILE.LN_MAKERCD3,''),
      *           ISNULL(LNFILE.LN_SSNO4, 0),
      *           ISNULL(LNFILE.LN_MAKERCD4,''),
      *           ISNULL(LNFILE.LN_ORGST,''),
      *           ISNULL(LNFILE.LN_CONVERCD,''),
      *           ISNULL(CAST(LNFILE.LN_ENTDATE  AS VARCHAR(10)),'1900-01-01'),
      *           ISNULL(CAST(LNFILE.LN_LOANDATE AS VARCHAR(10)),'1900-01-01'),
      *           ISNULL(CAST(LNFILE.LN_POFFDATE AS VARCHAR(10)),'1900-01-01'),
      *           ISNULL(LNFILE.LN_APRATE, 0),
      *           ISNULL(LNFILE.LN_TOTPAYMNTD, 0),
      *           ISNULL(LSMFILE.LSM_BRNO, 0), 
      *           ISNULL(LSMFILE.LSM_ACCTNO, 0),
      *           ISNULL(LSMFILE.LSM_SEQNO, 0),
      *           ISNULL(LSMFILE.LSM_MISC_FIELD_15,' ')
      * 
      *      FROM DBO.TRPFILE  TRPFILE
      * 
      *      LEFT OUTER JOIN 
      *           DBO.LNFILE   LNFILE
      *        ON (  TRPFILE.TP_BRNO          = LNFILE.LN_OWNBR	
      *       AND    TRPFILE.TP_ACCTNO        = LNFILE.LN_ACCTNO
      *       AND    LNFILE.LN_ORGST          = 'IL'
      *       AND (( LNFILE.LN_CONVERCD       = 'Y'
      *       AND    LNFILE.LN_ENTDATE        > '2022-08-01' )
      *        OR  ( LNFILE.LN_CONVERCD      != 'Y'
      *       AND    LNFILE.LN_LOANDATE       > '2022-08-01' )))
      * 
      *      LEFT OUTER JOIN 
      *           DBO.LSMFILE  LSMFILE
      *        ON (  LSMFILE.LSM_BRNO        = LNFILE.LN_OWNBR	
      *       AND    LSMFILE.LSM_ACCTNO      = LNFILE.LN_ACCTNO
      *       AND    LSMFILE.LSM_SEQNO       = 1 )
      * 
      *     WHERE (  TRPFILE.TP_BRNO         = @QTRP1_WBEG_BRNO 
      *       AND    TRPFILE.TP_DATE   BETWEEN @QTRP1_WBEG_DATE
      *                                    AND @QTRP1_WEND_DATE
      *       AND    TRPFILE.TP_REV         != 'Y'
      *       AND    TRPFILE.TP_CLASS   NOT IN (50, 55)
      *       AND    TRPFILE.TP_OTHBR_FG    != 'C' )
      * 
      *     ORDER BY TRPFILE.TP_BRNO,
      *              TRPFILE.TP_DATE,
      *              TRPFILE.TP_CLASS,
      *              TRPFILE.TP_ACCTNO,
      *              TRPFILE.TP_LINE;  

           EXEC SQL SET CONNECTION TRP1_CONNECT END-EXEC.

           EXEC SQL
            DECLARE TRP1_CURSOR CURSOR FOR
                :TRP1-RETURN-CODE = EXEC SSP_ILUEXT_TRPFILE_SELECT
              ( :QTRP1-WBEG-BRNO,   
                :QTRP1-WBEG-DATE,   
                :QTRP1-WEND-DATE )  
           END-EXEC.

           EXEC SQL
               OPEN TRP1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QTRP1-WBEG-BRNO
                          QTRP1-WEND-BRNO
                          QTRP1-WBEG-CLASS
                          QTRP1-WEND-CLASS
                          QTRP1-WBEG-ACCTNO
                          QTRP1-WEND-ACCTNO
                          QTRP1-WBEG-LINE
                          QTRP1-WEND-LINE
                          QTRP1-WSBEG-CLASS
                          QTRP1-WSBEG-ACCTNO
                          QTRP1-WSBEG-LINE.
           MOVE SPACES TO QTRP1-WBEG-DATE
                          QTRP1-WEND-DATE
                          QTRP1-WSBEG-DATE.

           MOVE STAT---GOOD TO TRP1-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       READ-TRP1-FILE-NEXT. *> EMBEDDED - TRP,LN,LSM
           PERFORM READ-IT.
           MOVE TRP-PATH-OWNBR  TO TP-BRNO.
           MOVE TRP-PATH-SQL    TO E-FILE.
           MOVE TRP1-KEY        TO E-KEYX.
           INITIALIZE QTRP-REC.
           INITIALIZE QLN-REC.
           INITIALIZE QLSM-REC.

           EXEC SQL SET CONNECTION TRP1_CONNECT END-EXEC.

           IF ( TRP1-CURSOR-STAT-GOOD )
              EXEC SQL
                FETCH TRP1_CURSOR 
                  INTO 
                   :QTP-BRNO,
                   :QTP-DATE,     
                   :QTP-CLASS,        
                   :QTP-ACCTNO,       
                   :QTP-LINE,         
                   :QTP-TRCD,         
                   :QTP-REFNO,        
                   :QTP-PAYDATE,  
                   :QTP-INTDUE,
                   :QTP-APCUR,
                   :QTP-CURBAL,
                   :QTP-OTHBR-FG,
                   :QTP-REV,
                   :QTP-LCDUE,
                   :QLN-OWNBR,  
                   :QLN-ACCTNO,  
                   :QLN-SSNO1,  
                   :QLN-MAKERCD1, 
                   :QLN-SSNO2,  
                   :QLN-MAKERCD2, 
                   :QLN-SSNO3,   
                   :QLN-MAKERCD3, 
                   :QLN-SSNO4,  
                   :QLN-MAKERCD4, 
                   :QLN-ORGST, 
                   :QLN-CONVERCD, 
                   :QLN-ENTDATE,  
                   :QLN-LOANDATE, 
                   :QLN-POFFDATE, 
                   :QLN-APRATE,  
                   :QLN-TOTPAYMNTD, 
                   :QLSM-BRNO,   
                   :QLSM-ACCTNO,  
                   :QLSM-SEQNO,  
                   :QLSM-MISC-FIELD-15
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TRP1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-TRP-FIELDS
              PERFORM GET-LN-FIELDS
              PERFORM GET-LSM-FIELDS.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       CLOSE-TRP1-FILE.

           PERFORM CLOSE-IT.
           IF (( TRP1-CURSOR-STAT-GOOD ) AND
               ( TRP1-CONN-OPEN-SW = "Y" ))
              EXEC SQL SET CONNECTION TRP1_CONNECT  END-EXEC
              EXEC SQL
                  CLOSE TRP1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO TRP1-CURSOR-STAT  
              EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       CLOSE-TRP1-CONNECTION.

           IF TRP1-CONN-OPEN-SW = "Y"
              EXEC SQL SET CONNECTION TRP1_CONNECT END-EXEC
              EXEC SQL COMMIT END-EXEC
              EXEC SQL DISCONNECT TRP1_CONNECT END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE "N" TO TRP1-CONN-OPEN-SW.


      * COPY "LIBLP/LPTRL1RN.CPY".
      * COPYMEMBER: LIBLP/LPTR1RN
       LOAD-TRL1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( TRL-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( TRL-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO TRL-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO TRL-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO TRL-ALL-DATA
              MOVE TRL-ALL-PATH             TO TRL-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( TRL-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO TRL-ALL-BASE
              MOVE FILPATH-MACHINE         TO TRL-ALL-MACHINE
              MOVE FILPATH-DATA            TO TRL-ALL-DATA
              MOVE TRL-ALL-PATH             TO TRL-PATH.

      *-----------------------------------------------------------------
       OPEN-TRL1-FILE.
           PERFORM LOAD-TRL1-FILE.
           PERFORM OPEN-IT.
           MOVE TRL-PATH-SQL TO E-FILE.

           IF TRL1-CONN-OPEN-SW = "N"
               EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS TRL1_CONNECT
                        USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
               END-EXEC
               EXEC SQL SET CONNECTION C1 END-EXEC
               MOVE 'Y' TO TRL1-CONN-OPEN-SW.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-TRL1-FILE. *> - TRL,LN,LSM
           PERFORM START-IT.
           MOVE TRL-PATH-OWNBR  TO TL-BRNO.
           MOVE TRL-PATH-SQL    TO E-FILE.
           MOVE TRL1-KEY        TO E-KEYX.

           IF ( TRL1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-TRL1-FILE.

      *     IF ( QTRL1-WEND-LINE = ZEROES    ) OR
      *        ( QTRL1-WEND-LINE NOT NUMERIC )
      *        MOVE TRL-PATH-OWNBR        TO QTRL1-WBEG-BRNO
      *        MOVE TRL-PATH-OWNBR        TO QTRL1-WEND-BRNO
      *        MOVE TL-DATE               TO SQL-DATE-YYYYMMDD
      *        PERFORM SQL-SET-DATE
      *        MOVE SQL-DATE-YYYY-MM-DD  TO QTRL1-WBEG-DATE
      *        MOVE JULIAN-END           TO SQL-DATE-YYYYMMDD
      *        PERFORM SQL-SET-DATE
      *        MOVE SQL-DATE-YYYY-MM-DD  TO QTRL1-WEND-DATE
      *        MOVE TL-CLASS             TO QTRL1-WBEG-CLASS
      *        MOVE TL-ACCTNO            TO QTRL1-WBEG-ACCTNO
      *        MOVE TL-LAWCODE           TO QTRL1-WBEG-LAWCODE
      *        MOVE TL-LINE              TO QTRL1-WBEG-LINE
      *        MOVE ALL "9"              TO QTRL1-WEND-CLASS
      *                                     QTRL1-WEND-ACCTNO
      *                                     QTRL1-WEND-LAWCODE
      *                                     QTRL1-WEND-LINE.

      * STORED PROCEDURE:         
      *                                          
      *   SELECT TRLFILE.TL_BRNO,
      *          CAST(TRLFILE.TL_DATE AS VARCHAR(10)),
      *          TRLFILE.TL_CLASS,
      *          TRLFILE.TL_ACCTNO,
      *          TRLFILE.TL_LAWCODE,
      *          TRLFILE.TL_LINE,
      *          TRLFILE.TL_LNAMT,
      *          ISNULL(LNFILE.LN_OWNBR, 0),
      *          ISNULL(LNFILE.LN_ACCTNO, 0),
      *          ISNULL(LNFILE.LN_SSNO1, 0),
      *          ISNULL(LNFILE.LN_MAKERCD1,''),
      *          ISNULL(LNFILE.LN_SSNO2, 0),
      *          ISNULL(LNFILE.LN_MAKERCD2,''),
      *          ISNULL(LNFILE.LN_SSNO3, 0), 
      *          ISNULL(LNFILE.LN_MAKERCD3,''),
      *          ISNULL(LNFILE.LN_SSNO4, 0),
      *          ISNULL(LNFILE.LN_MAKERCD4,''),
      *          ISNULL(LNFILE.LN_ORGST,''),
      *          ISNULL(LNFILE.LN_CONVERCD,''),
      *          ISNULL(CAST(LNFILE.LN_ENTDATE  AS VARCHAR(10)),'1900-01-01'),
      *          ISNULL(CAST(LNFILE.LN_LOANDATE AS VARCHAR(10)),'1900-01-01'),
      *          ISNULL(CAST(LNFILE.LN_POFFDATE AS VARCHAR(10)),'1900-01-01'),
      *          ISNULL(LNFILE.LN_APRATE, 0),
      *          ISNULL(LNFILE.LN_TOTPAYMNTD, 0),
      *          ISNULL(LSMFILE.LSM_BRNO, 0), 
      *          ISNULL(LSMFILE.LSM_ACCTNO, 0),
      *          ISNULL(LSMFILE.LSM_SEQNO, 0),
      *          ISNULL(LSMFILE.LSM_MISC_FIELD_15,' ')
      * 
      *     FROM DBO.TRLFILE  TRLFILE
      * 
      *     LEFT OUTER JOIN 
      *          DBO.LNFILE LNFILE
      *       ON (  TRLFILE.TL_BRNO          = LNFILE.LN_OWNBR	
      *      AND    TRLFILE.TL_ACCTNO        = LNFILE.LN_ACCTNO
      *      AND    LNFILE.LN_ORGST          = 'IL'
      *      AND    LNFILE.LN_POFFDATE      != '1900-01-01'
      *      AND    SUBSTRING(CAST(LNFILE.LN_LOANDATE AS VARCHAR(10)), 1, 7)
      *                                     !=  
      *             SUBSTRING(CAST(LNFILE.LN_POFFDATE AS VARCHAR(10)), 1, 7) )
      * 
      *     LEFT OUTER JOIN 
      *          DBO.LSMFILE LSMFILE
      *       ON (  LSMFILE.LSM_BRNO        = LNFILE.LN_OWNBR	
      *      AND    LSMFILE.LSM_ACCTNO      = LNFILE.LN_ACCTNO
      *      AND    LSMFILE.LSM_SEQNO       = 1 )
      * 
      *    WHERE (  TRLFILE.TL_BRNO         = @QTRL1_WBEG_BRNO 
      *      AND    TRLFILE.TL_DATE   BETWEEN @QTRL1_WBEG_DATE
      *                                   AND @QTRL1_WEND_DATE
      *      AND    TRLFILE.TL_LNAMT       <= 0
      *      AND    TRLFILE.TL_CLASS   NOT IN (50, 55))
      * 
      *    ORDER BY TRLFILE.TL_BRNO,
      *             TRLFILE.TL_DATE,
      *             TRLFILE.TL_CLASS,
      *             TRLFILE.TL_ACCTNO,
      *             TRLFILE.TL_LAWCODE,
      *             TRLFILE.TL_LINE; 

           EXEC SQL SET CONNECTION TRL1_CONNECT END-EXEC.

           EXEC SQL
            DECLARE TRL1_CURSOR CURSOR FOR
               :TRL1-RETURN-CODE = EXEC SSP_ILUEXT_TRLFILE_SELECT
              (:QTRL1-WBEG-BRNO,  
               :QTRL1-WBEG-DATE,  
               :QTRL1-WEND-DATE )
           END-EXEC.

           EXEC SQL
               OPEN TRL1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QTRL1-WBEG-BRNO
                          QTRL1-WBEG-CLASS
                          QTRL1-WEND-CLASS.
           MOVE SPACES TO QTRL1-WBEG-DATE
                          QTRL1-WEND-DATE.

           MOVE STAT---GOOD TO TRL1-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       READ-TRL1-FILE-NEXT. *> EMBEDDED - TRL,LN,LSM
           PERFORM READ-IT.
           MOVE TRL-PATH-OWNBR  TO TL-BRNO.
           MOVE TRL-PATH-SQL    TO E-FILE.
           MOVE TRL1-KEY        TO E-KEYX.
           INITIALIZE QTRL-REC.
           INITIALIZE QLN-REC.
           INITIALIZE QLSM-REC.
 
           IF ( TRL1-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION TRL1_CONNECT END-EXEC
              EXEC SQL
                FETCH TRL1_CURSOR 
                  INTO 
                   :QTL-BRNO,
                   :QTL-DATE,
                   :QTL-CLASS,
                   :QTL-ACCTNO,
                   :QTL-LAWCODE,
                   :QTL-LINE,
                   :QTL-LNAMT,
                   :QLN-OWNBR,  
                   :QLN-ACCTNO,  
                   :QLN-SSNO1,  
                   :QLN-MAKERCD1, 
                   :QLN-SSNO2,  
                   :QLN-MAKERCD2, 
                   :QLN-SSNO3,   
                   :QLN-MAKERCD3, 
                   :QLN-SSNO4,  
                   :QLN-MAKERCD4, 
                   :QLN-ORGST, 
                   :QLN-CONVERCD, 
                   :QLN-ENTDATE,  
                   :QLN-LOANDATE, 
                   :QLN-POFFDATE, 
                   :QLN-APRATE,  
                   :QLN-TOTPAYMNTD, 
                   :QLSM-BRNO,   
                   :QLSM-ACCTNO,  
                   :QLSM-SEQNO,  
                   :QLSM-MISC-FIELD-15
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           EXEC SQL SET CONNECTION C1 END-EXEC.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TRL1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-TRL-FIELDS 
              PERFORM GET-LN-FIELDS
              PERFORM GET-LSM-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-TRL1-FILE.
           PERFORM CLOSE-IT.
           IF ( TRL1-CURSOR-STAT-GOOD )
               EXEC SQL SET CONNECTION TRL1_CONNECT END-EXEC
               EXEC SQL CLOSE TRL1_CURSOR END-EXEC
               EXEC SQL SET CONNECTION C1 END-EXEC
               MOVE STAT---BAD TO TRL1-CURSOR-STAT.

      *-----------------------------------------------------------------
       CLOSE-TRL1-CONNECTION.
           IF TRL1-CONN-OPEN-SW = "Y"
               EXEC SQL SET CONNECTION TRL1_CONNECT END-EXEC
               EXEC SQL COMMIT END-EXEC
               EXEC SQL DISCONNECT TRL1_CONNECT END-EXEC
               EXEC SQL SET CONNECTION C1 END-EXEC
               MOVE "N" TO TRL1-CONN-OPEN-SW.


      * COPY "LIBLP/LPLN1RN.CPY".
      * COPYMEMBER: LIBLP/LPLN1RN
       LOAD-LN1-FILE.
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
       OPEN-LN1-FILE.
           PERFORM LOAD-LN1-FILE.
           PERFORM OPEN-IT.
           MOVE LN-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       READ-LN1-FILE. *> EMBEDDED - LNFILE
           PERFORM READ-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-PATH-SQL    TO E-FILE.
           MOVE LN1-KEY        TO E-KEYX.
           INITIALIZE QLN-REC.

           MOVE LN-OWNBR  TO QLN-OWNBR.
           MOVE LN-ACCTNO TO QLN-ACCTNO.

           EXEC SQL
            SELECT LNFILE.LN_OWNBR,
                   LNFILE.LN_ACCTNO,
                   LNFILE.LN_SSNO1,
                   LNFILE.LN_MAKERCD1, 
                   LNFILE.LN_SSNO2,
                   LNFILE.LN_MAKERCD2, 
                   LNFILE.LN_SSNO3,
                   LNFILE.LN_MAKERCD3, 
                   LNFILE.LN_SSNO4,
                   LNFILE.LN_MAKERCD4, 
                   LNFILE.LN_ORGST, 
                   LNFILE.LN_CONVERCD, 
                   CAST(LNFILE.LN_ENTDATE AS VARCHAR(10)), 
                   CAST(LNFILE.LN_LOANDATE AS VARCHAR(10)), 
                   CAST(LNFILE.LN_POFFDATE AS VARCHAR(10)), 
                   LNFILE.LN_APRATE,
                   LNFILE.LN_TOTPAYMNTD
              INTO  
                  :QLN-OWNBR,
                  :QLN-ACCTNO,
                  :QLN-SSNO1,
                  :QLN-MAKERCD1, 
                  :QLN-SSNO2,
                  :QLN-MAKERCD2, 
                  :QLN-SSNO3,
                  :QLN-MAKERCD3, 
                  :QLN-SSNO4,
                  :QLN-MAKERCD4, 
                  :QLN-ORGST, 
                  :QLN-CONVERCD, 
                  :QLN-ENTDATE, 
                  :QLN-LOANDATE, 
                  :QLN-POFFDATE, 
                  :QLN-APRATE,
                  :QLN-TOTPAYMNTD
              FROM DBO.LNFILE
             WHERE LNFILE.LN_OWNBR  = :QLN-OWNBR
               AND LNFILE.LN_ACCTNO = :QLN-ACCTNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LN1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-LN-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-LN1-FILE.
           PERFORM CLOSE-IT.
 

      * COPY "LIBLP/LPLP1RN.CPY".
      * COPYMEMBER: LIBLP/LPLP1RN
       LOAD-LP1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( LP-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( LP-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO LP-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO LP-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO LP-ALL-DATA
              MOVE LP-ALL-PATH             TO LP-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( LP-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO LP-ALL-BASE
              MOVE FILPATH-MACHINE         TO LP-ALL-MACHINE
              MOVE FILPATH-DATA            TO LP-ALL-DATA
              MOVE LP-ALL-PATH             TO LP-PATH.

      *-----------------------------------------------------------------
       OPEN-LP1-FILE.
           PERFORM LOAD-LP1-FILE.
           PERFORM OPEN-IT.
           MOVE LP-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
      *  ***NOTE*** ONLY USE START-LP1-FILE-SCAN WHEN USING A 
      *  START-LP1-FILE-NOT-GREATER.  THIS IS FOR DISPLAYING MULTIPLE
      *  PAGE IN A UP & DOWN SCENARIO.
      *-----------------------------------------------------------------
       START-LP1-FILE-NOT-GREATER. *> EMBEDDED - LPFILE
           PERFORM START-IT.
           MOVE LP-PATH-OWNBR  TO LP-BRNO.
           MOVE LP-PATH-SQL    TO E-FILE.
           MOVE LP1-KEY        TO E-KEYX.

           IF ( LP1-CURSOR-NG-STAT-GOOD )
              PERFORM CLOSE-LP1-FILE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  DEFAULT THE WBEG AND WEND
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( QLP1-WEND-SEQNO = ZEROES    ) OR
              ( QLP1-WEND-SEQNO NOT NUMERIC )
              MOVE LP-PATH-OWNBR        TO QLP1-WBEG-BRNO
              MOVE LP-PATH-OWNBR        TO QLP1-WEND-BRNO
              MOVE LP-ACCTNO            TO QLP1-WBEG-ACCTNO
              MOVE LP-ACCTNO            TO QLP1-WEND-ACCTNO
              MOVE ALL "0"              TO QLP1-WBEG-SEQNO
              MOVE LP-SEQNO             TO QLP1-WEND-SEQNO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           EXEC SQL
             DECLARE LP1_CURSOR_NG CURSOR FOR
              SELECT LPFILE.LP_BRNO,
                     LPFILE.LP_ACCTNO,
                     LPFILE.LP_SEQNO,
                     CAST(LPFILE.LP_PAYDATE AS VARCHAR(10)), 
                     LPFILE.LP_TRCD, 
                     LPFILE.LP_REFNO, 
                     LPFILE.LP_APCUR,
                     LPFILE.LP_CURBAL,
                     LPFILE.LP_INTDUE,
                     LPFILE.LP_APINTOWE,
                     LPFILE.LP_REV 
              FROM DBO.LPFILE
              WHERE LPFILE.LP_BRNO    = :QLP1-WEND-BRNO
                AND LPFILE.LP_ACCTNO  = :QLP1-WEND-ACCTNO
                AND LPFILE.LP_SEQNO  <= :QLP1-WEND-SEQNO 
              ORDER BY LPFILE.LP_BRNO DESC,
                       LPFILE.LP_ACCTNO DESC,
                       LPFILE.LP_SEQNO DESC
           END-EXEC.

           EXEC SQL
               OPEN LP1_CURSOR_NG
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QLP1-WBEG-BRNO 
                          QLP1-WEND-BRNO
                          QLP1-WBEG-ACCTNO
                          QLP1-WEND-ACCTNO
                          QLP1-WBEG-SEQNO
                          QLP1-WEND-SEQNO.

           MOVE STAT---GOOD TO LP1-CURSOR-NG-STAT.

      *-----------------------------------------------------------------
       READ-LP1-FILE-PREVIOUS. *> EMBEDDED - LPFILE
           PERFORM READ-IT.
           MOVE LP-PATH-OWNBR  TO LP-BRNO.
           MOVE LP-PATH-SQL    TO E-FILE.
           MOVE LP1-KEY        TO E-KEYX.
           INITIALIZE QLP-REC.

           IF ( LP1-CURSOR-NG-STAT-GOOD )
              EXEC SQL
                FETCH LP1_CURSOR_NG 
                  INTO 
                    :QLP-BRNO,
                    :QLP-ACCTNO,
                    :QLP-SEQNO,
                    :QLP-PAYDATE,  
                    :QLP-TRCD, 
                    :QLP-REFNO, 
                    :QLP-APCUR,
                    :QLP-CURBAL,
                    :QLP-INTDUE,
                    :QLP-APINTOWE,
                    :QLP-REV
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDPVCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LP1-FILE-PREVIOUS.

           IF ( IO-FG = 0 )
              PERFORM GET-LP-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-LP1-FILE.
           PERFORM CLOSE-IT.
 
           IF ( LP1-CURSOR-NG-STAT-GOOD )
              EXEC SQL
                   CLOSE LP1_CURSOR_NG
              END-EXEC
              MOVE STAT---BAD TO LP1-CURSOR-NG-STAT.


 
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

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       READ-BW1-FILE. *> EMBEDDED - BWFILE
           PERFORM READ-IT.
           MOVE BW-PATH-OWNBR  TO BW-OWNBR. 
           MOVE BW-PATH-SQL    TO E-FILE.
           MOVE BW1-KEY        TO E-KEYX.
           INITIALIZE QBW-REC.

           MOVE BW-OWNBR   TO QBW-OWNBR.
           MOVE BW-SSNO    TO QBW-SSNO.

           EXEC SQL
            SELECT BWFILE.BW_OWNBR,
                   BWFILE.BW_SSNO,
                   BWFILE.BW_LNAME, 
                   BWFILE.BW_FNAME, 
                   BWFILE.BW_ADR1, 
                   BWFILE.BW_ADR2, 
                   BWFILE.BW_CITY, 
                   BWFILE.BW_STATE, 
                   BWFILE.BW_ZIP 
              INTO  
                  :QBW-OWNBR,
                  :QBW-SSNO,
                  :QBW-LNAME, 
                  :QBW-FNAME, 
                  :QBW-ADR1,  
                  :QBW-ADR2,  
                  :QBW-CITY,  
                  :QBW-STATE, 
                  :QBW-ZIP 
              FROM DBO.BWFILE
             WHERE BWFILE.BW_OWNBR = :QBW-OWNBR
               AND BWFILE.BW_SSNO  = :QBW-SSNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-BW1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-BW-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-BW1-FILE.
           PERFORM CLOSE-IT.
 

      * COPY "LIBLP/LPLSM1RN.CPY".
      * COPYMEMBER: LIBLP/LPLS1RN
      * 20220922 BAH ADDED LSM-MISPAY-90-DATE #1570
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
                   LSMFILE.LSM_MISC_FIELD_15
              INTO  
                  :QLSM-BRNO,
                  :QLSM-ACCTNO,
                  :QLSM-SEQNO,
                  :QLSM-MISC-FIELD-15
              FROM DBO.LSMFILE
             WHERE LSMFILE.LSM_BRNO   = :QLSM-BRNO
               AND LSMFILE.LSM_ACCTNO = :QLSM-ACCTNO
               AND LSMFILE.LSM_SEQNO  = :QLSM-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LSM1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-LSM-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-LSM1-FILE.
           PERFORM CLOSE-IT.
 

      * COPY "LIBLP/LPLSX1RN.CPY".
      * COPYMEMBER: LIBLP/LPLSX1RN
       LOAD-LSX1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( LSX-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( LSX-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO LSX-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO LSX-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO LSX-ALL-DATA
              MOVE LSX-ALL-PATH             TO LSX-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( LSX-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO LSX-ALL-BASE
              MOVE FILPATH-MACHINE         TO LSX-ALL-MACHINE
              MOVE FILPATH-DATA            TO LSX-ALL-DATA
              MOVE LSX-ALL-PATH             TO LSX-PATH.

      *-----------------------------------------------------------------
       OPEN-LSX1-FILE.
           PERFORM LOAD-LSX1-FILE.
           PERFORM OPEN-IT.
           MOVE LSX-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       READ-LSX1-FILE. *> EMBEDDED - LSXFILE
           PERFORM READ-IT.
           MOVE LSX-PATH-OWNBR  TO LSX-BRNO. 
           MOVE LSX-PATH-SQL    TO E-FILE.
           MOVE LSX1-KEY        TO E-KEYX.
           INITIALIZE QLSX-REC.

           MOVE LSX-BRNO   TO QLSX-BRNO.
           MOVE LSX-ACCTNO TO QLSX-ACCTNO.
           MOVE LSX-SEQNO TO QLSX-SEQNO.

           EXEC SQL
            SELECT LSXFILE.LSX_BRNO,
                   LSXFILE.LSX_ACCTNO,
                   LSXFILE.LSX_SEQNO,
                   LSXFILE.LSX_VOLUNTARY_FG
              INTO  
                  :QLSX-BRNO,
                  :QLSX-ACCTNO,
                  :QLSX-SEQNO,
                  :QLSX-VOLUNTARY-FG
              FROM DBO.LSXFILE
             WHERE LSXFILE.LSX_BRNO = :QLSX-BRNO
               AND LSXFILE.LSX_ACCTNO = :QLSX-ACCTNO
               AND LSXFILE.LSX_SEQNO = :QLSX-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LSX1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-LSX-FIELDS.

      *-----------------------------------------------------------------
        CLOSE-LSX1-FILE.
           PERFORM CLOSE-IT.
 
       COPY "LIBGB/GBWKIN.CPY".
       COPY "LIBSP/SPILLINOISIN.CPY".
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
              MOVE "SP/EXTRMU" TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY ILUEXT-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
