       IDENTIFICATION DIVISION.
       PROGRAM-ID.       PRENOT.
       DATE-WRITTEN.   02/05/94.
      *****************************************************************
      *                      (SP)
      *
      * DESCRIPTION:  CREATES /USR/DATA/R1/SP/ACFILE
      *               TRANSMISSION OF DEPOSIT INFORMATION IN ACH FORMAT.
      *
      * REV:
      *  MJD 062895 ADDED COPY MEMBERS LIBLP/POSTDATE AND POSTDATEW.
      *             SET RC-FUNDS-DATE AND RC-FUNDS-TIME.
      *  MJD 071095 ADDED REPORT ONLY OPTION, DATE SUFFIX TO DATE,
      *             RANGE-LINES, AND DATE & BRANCH RANGE LOGIC.
      *  BLL 080295 MAKE EFFECTIVE DATE YYMMDD, CHANGE ORIGIN FROM
      *             BANK ROUTING # TO FED TAX ID
      *  BLL 080395 PRINT TOTAL, DON'T CREATE DETAIL TRANSMISSION
      *             IF DEPOSIT AMT IS 0
      *  BLL 080495 IF RC-DEPOSIT IS NEGATIVE, USE TRAN CODE OF 22
      *             TO CREDIT BRANCH BANK; ADD TRACING NUMBERS,
      *             PRINT BATCH COUNT, RECORD COUNT
      *  MJD 080395 ADDED PERFORM OPEN-GR1-FILE AND SEARCH-GR-INT TO
      *               INITIALIZE ROUTINE.
      *  GLF 090997 REPLACED 'IO' ON GP-FILE WITH GET 'GPENV'
      *  MJD 032098 ADDED GETENV(W) FOR USE IN LIBLP/POSTDATE
      *  BLV 041800 MAKE MORE GENERIC, CHECK I-AM-REGENCY SO CAN BE
      *             USED BY REGENCY FOR PRENOTE TEST, REGWAN PR# 1141
      *
      * KEC 2017.0110 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          REMOVED THE FOLLOWING [PIC 9(7)] SINCE NOT USED.
      *             HOLD-RC2-KEY.
      *             HOLD-RC-STATUS
      *             HOLD-RC-BRNO
      *             HOLD-RC-DATE
      *             BEG-RC-DATE
      *             END-RC-DATE
      *
      *  20171013 BAH ACTIVATE 8 DIGIT DATES, PD0006
      *  BAH 20200831 REMOVED RESET ON BR1 
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBSP/SPFSAC.CPY".
       COPY "LIBGB/GBFSPR.CPY".
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBSP/SPFDAC.CPY".
       COPY "LIBGB/GBFDPR.CPY".

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)SP/PRENOT.CBL S34 06/20/21-20:39:36 >".
      ****************************
      *     F I L E   I F N      *
      ****************************
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".

       01  AC-PATH-OVERRIDE          PIC X(01)   VALUE SPACES.
       01  AC-PATH                   PIC X(35)   VALUE SPACES.
       01  AC-PATH-FORMAT.
           03  AC-PATH-BASE          PIC X(09).
           03  FILLER                PIC X(01)   VALUE "/".
           03  AC-PATH-MACHINE       PIC X(02).
           03  FILLER                PIC X(11)   VALUE "/SP/PRENOT.".
           03  AC-PATH-DATE-YYMMDD   PIC 9(06).

       01  PR-IFN               PIC X(2)    VALUE "PR".
       01  AC-KEY               PIC 9(5)    VALUE 1.

       01  REC-COUNT            PIC 9(5) VALUE 1.

       01  WK-TIME.
           03  WK-HH            PIC 99.
           03  WK-MN            PIC 99.

       01  WK-RECS-REM          PIC 99.
       01  WK-TRACE-NUMBER      PIC 9(7) COMP-3.
       01  WK-BATCH-NO          PIC 9(5) COMP-3.
       01  WK-GR-TOT-DEPOSIT    PIC 9(11)V99 COMP-3.
       01  WK-GR-TOT-DEBIT      PIC S9(11)V99 COMP-3.
       01  WK-GR-TOT-CREDIT     PIC S9(11)V99 COMP-3.
       01  WK-TOT-DEPOSIT       PIC S9(11)V99 COMP-3.
       01  WK-TOT-DEBIT         PIC S9(11)V99 COMP-3.
       01  WK-TOT-CREDIT        PIC S9(11)V99 COMP-3.
       01  WK-NO-SIGN           PIC 9(11)V99 COMP-3.
       01  WK-BATCH-COUNT       PIC 9(5) COMP-3.
       01  WK-TOT-BATCH-COUNT   PIC 9(5) COMP-3.
       01  WK-BIG-HASH.
           03  WK-PRE-HASH      PIC 9(4)    VALUE 0.
           03  WK-HASH          PIC 9(10)   VALUE 0.
       01  WK-X-HASH REDEFINES WK-BIG-HASH
                                PIC 9(14).

       01  WK-T-HASH            PIC 9(14)   VALUE 0.
       01  WK-ROUTING-NO        PIC 9(9).
       01  WK-ROUTINGX REDEFINES WK-ROUTING-NO.
           03  WK-ROUTE1        PIC 9(8).
           03  WK-CHECK-DIGIT   PIC 9.

       01  RECORDS-FOUND        PIC X         VALUE " ".
           88  NO-RECORDS-FOUND               VALUE "N".
       01  MESS-LIST            PIC X(5) VALUE "MESS.".
       01  LEGEND               PIC X(70)     VALUE SPACES.
       01  REPORT-FG            PIC X.

      *****************************************************************
      *   LINE WORKERS     *
      *****************************************************************
       01  LN-WORKERS.
           03  LN-FEED          PIC 99       VALUE 0.
           03  LN-COUNT         PIC 999      VALUE 99.
           03  LN-WK            PIC 999      VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

      ***********************************
      *     P R I N T   B U F F E R S   *
      ***********************************
       01  HEAD1.
           03  H-DATE           PIC X(8)     VALUE " ".
           03  FILLER           PIC XX       VALUE " ".
           03  H-HR             PIC 99       VALUE 0.
           03  FILLER           PIC X        VALUE ":".
           03  H-MIN            PIC 99       VALUE 0.
           03  FILLER           PIC X(5)    VALUE " ".
           03  H-NAME           PIC X(40)    VALUE " ".
           03  FILLER           PIC X(11)    VALUE " ".
           03  FILLER           PIC X(5)     VALUE "PAGE ".
           03  H-PAGE-NO        PIC ZZZ9.

       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(12).
           03  FILLER           PIC X(8)    VALUE " ".
           03  FILLER           PIC X(22) VALUE
               "BRANCH DEPOSIT LISTING".

       01  HEAD3.
           03  FILLER           PIC X(43)    VALUE
               "BR #  BRANCH NAME".
           03  FILLER           PIC X(46)    VALUE
               "BANK ACCOUNT #        ROUTING #       ".

       01  DETAIL-LINE          VALUE SPACES.
           03  FILLER           PIC X(132).

       01  D-LINE1 REDEFINES DETAIL-LINE.
           03  D-BRNO           PIC Z999.
           03  FILLER           PIC X(2).
           03  D-BRNAME         PIC X(25).
           03  FILLER           PIC X(12).
           03  D-BANK-ACCTNO    PIC X(17).
           03  FILLER           PIC X(5).
           03  D-ROUTING-NO     PIC 9(9).

       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(35).
           03  RANGE-SELECTION.
               04  FILLER       PIC XX.
               04  R-NUM        PIC 9(8).
           03  R-DATE REDEFINES RANGE-SELECTION
                                PIC 9999/99/99.
           03  R-FLAGX REDEFINES RANGE-SELECTION.
               04  FILLER       PIC X(7).
               04  R-FLAG       PIC X.

      *****************************************************************
      *     S C R E E N   F I E L D S   *
      *****************************************************************
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  BEG-BRANCH       PIC 9(4).
           03  END-BRANCH       PIC 9(4).
           03  BEG-DATE         PIC 9(8).
           03  END-DATE         PIC 9(8).
           03  EFFECTIVE-DATE   PIC 9(8).
           03  GROUP-FLAG       PIC X.
           03  ENT-GROUP        PIC X(4).

       01 VERSION-CONTROL       PIC X VALUE "B".

       01  SV-BEG-BR            PIC 9(4)  VALUE 9999.
       01  SV-END-BR            PIC 9(4)  VALUE 9999.

       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBLP/RANGE_CMD.CPY".
       COPY "LIBLP/POSTDATEW.CPY".
       COPY "LIBGB/GETENVW.CPY".

       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/PRENOT_DEF.CPY".
       COPY "LIBSP/PRENOT_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *****************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      *****************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBSP/PRENOT_SCN.CPY".

      *----------------------------------------------------------------
       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
                AC-FILE PR-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           CLOSE
                AC-FILE PR-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ******************************************************************
       MAIN-MODULE SECTION.
      ******************************************************************
           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".
      D    STOP MESS.
           MOVE "PRENOT" TO VDU-FORM-NAME.

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

       NEXT-BR.
           IF BR-NO > END-BRANCH
              GO TO END-OF-GROUP.

           IF BR-NO < BEG-BRANCH
              GO TO END-OF-GROUP.

           PERFORM 6-ENTRY-DETAIL.
           PERFORM DETAIL-LINE-ROUTINE.

           GO TO END-OF-GROUP.

       END-OF-GROUP.
           GO TO NEXT-GROUP-LOC.

       END-ROUTINE.
           PERFORM CLOSING-ROUTINES.
           GO TO END-PROGRAM.

      *****************************************************************
       INITIALIZATION SECTION.
      *****************************************************************
           MOVE PRENOT-QUEUE-POS TO Q-POS.
           MOVE PRENOT-COPIES-POS TO C-POS.
           MOVE PRENOT-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           IF BEG-BRANCH = 0
              IF END-BRANCH = 0
                 MOVE BEG-BRANCH TO SV-BEG-BR
                 MOVE END-BRANCH TO SV-END-BR
                 MOVE EXT-CONAME-USER-LOC TO BEG-BRANCH
                                             END-BRANCH.

           IF EXT-ROUTE-BUF = "00"
              MOVE "PRENOTIFICATION EXTRACTION IN PROGRESS.." TO LEGEND
              PERFORM SEND-LEGEND.

           PERFORM OPEN-PR-FILE.

           PERFORM OPEN-BR-FILE.
           IF REPORT-FG NOT = "Y"
              PERFORM OPEN-AC-FILE-OUTPUT
              PERFORM CLOSE-AC-FILE
              PERFORM OPEN-AC-FILE.

           PERFORM GET-GPENV.

           PERFORM 1-FILE-HEADER.

           PERFORM 5-COMPANY-BATCH-HEADER.


           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-BR-FILE.

           GO TO EXIT-INITIALIZE.

       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.

       EXIT-INITIALIZE.
           EXIT.

      *****************************************************************
       EO-EXEC-AUDIT SECTION.
      *****************************************************************
           ACCEPT END-DATE FROM CENTURY-DATE.
           MOVE END-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-YYMMDD.
           MOVE DATE-YYMMDD TO AC-PATH-DATE-YYMMDD.

      *****************************************************************
       ENTER-PARAMETERS SECTION.
      *****************************************************************
       ENTER-PARM.

           IF VDUSTATUS = "1U"
              GO TO REPORT-ONLY.

      *----------------------------------------------------------------
       BR-01.
           MOVE LEGEND-BR-01 TO LEGEND.
           PERFORM SEND-LEGEND.
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
                 MOVE "S  A000BR1." TO VDU
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
              MOVE "Y" TO GROUP-FLAG
              MOVE 0 TO BEG-BRANCH
              MOVE 9999 TO END-BRANCH
              GO TO REPORT-ONLY.
       BRANCH-IS-NUMERIC.
           MOVE "N" TO GROUP-FLAG.
           MOVE WGR-SRESULT TO BEG-BRANCH.
           MOVE "SN N040BR1." TO VDU.
           MOVE BEG-BRANCH TO VDUNUM0.
           PERFORM SCREENIO.
       BR-02.
           MOVE LEGEND-BR-02 TO LEGEND.
           PERFORM SEND-LEGEND.
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

           IF ( (BEG-BRANCH = 0) AND (END-BRANCH = 0) )
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE "USER BRANCH ONLY" TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO.

           GO TO REPORT-ONLY.
       ALL-BR.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "N" TO GROUP-FLAG.
           MOVE "S  A160DISPLAY-USER." TO VDU.
           MOVE SPACES TO VDUBUF.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
       REPORT-ONLY.
           MOVE LEGEND-REPORT-ONLY TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E NA010REPORTONLY."    TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO REPORT-ONLY.
           IF VDUBUF NOT = "Y" AND VDUBUF NOT = "N"
              GO TO REPORT-ONLY.
           MOVE VDUBUF TO REPORT-FG.

       EFFECTIVE-DT.
           IF REPORT-FG = "Y"
              GO TO EXIT-PARM.

           MOVE "ENTER EFFECTIVE TRANSACTION DATE" TO LEGEND.
           PERFORM SEND-LEGEND.

           PERFORM SET-POSTDATE.
           MOVE POST-DATE TO NUM-DATE.
           PERFORM WEEKDAY.
           IF DAY-IS-FRI
              MOVE 3 TO NDTE-HOLD
           ELSE IF DAY-IS-SAT
              MOVE 2 TO NDTE-HOLD
           ELSE
              MOVE 1 TO NDTE-HOLD.
           MOVE NUM-DATE TO NDTE-DATE.
           MOVE "D" TO DATER-UNITPER-CD.
           PERFORM INCREMENT-UNITPER.

           MOVE "S  8000EFFECTIVE." TO VDU.
           MOVE NDTE-DATE TO VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

           MOVE "ERNM060EFFECTIVE." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REPORT-ONLY.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO EFFECTIVE-DT.

           MOVE VDU-DATE-YYYYMMDD TO EFFECTIVE-DATE.

           MOVE SPACES TO LEGEND.
           PERFORM SEND-LEGEND.

           IF REPORT-FG NOT = "Y"
              ACCEPT END-DATE FROM CENTURY-DATE
              MOVE END-DATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-YYMMDD
              MOVE DATE-YYMMDD TO AC-PATH-DATE-YYMMDD
              PERFORM LOAD-AC-FILE
              MOVE AC-PATH TO ACCESS-BUF
              PERFORM ACCESS-CALL
              IF STAT = "00"
                 MOVE "PRENOT.&&&&&& ALREADY EXISTS!" TO MESS
                 INSPECT MESS REPLACING FIRST "&&&&&&" BY
                                AC-PATH-DATE-YYMMDD
                 PERFORM SEND-MESS
                 MOVE "CONTINUING WILL OVERWRITE FILE!" TO MESS
                 PERFORM SEND-MESS
              END-IF.

           GO TO EXIT-PARM.

       EXIT-PARM.
           EXIT.

      *****************************************************************
       DISPLAY-PARAMETERS SECTION.
      *****************************************************************

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

      **************************************************
       CLOSING-ROUTINES SECTION.
      **************************************************
           IF REC-COUNT < 3
              MOVE "N" TO RECORDS-FOUND
              GO TO EXIT-CLOSING-ROUTINES
           ELSE
              MOVE "Y" TO RECORDS-FOUND.

           PERFORM 8-COMPANY-BATCH-CONTROL.

           PERFORM 9-FILE-CONTROL.
           IF WK-RECS-REM NOT = 0
              COMPUTE WK-RECS-REM = 10 - WK-RECS-REM
              PERFORM PAD-FILE WK-RECS-REM TIMES.
           PERFORM PRINT-TOTAL-ROUTINE.
           PERFORM PRINT-RANGE-LINES.
        EXIT-CLOSING-ROUTINES.
           EXIT.

      **************************************************
       6-ENTRY-DETAIL SECTION.
      **************************************************

           MOVE SPACE TO AC-REC.
           MOVE 6 TO AC-REC-TYPE.

      *  IF NEGATIVE DEPOSIT, TRAN-CODE 22 WILL PUT MONEY BACK
      *  INTO ACCOUNT (NOTE AC-DEPOSIT-AMOUNT IS UNSIGNED

           MOVE 28 TO AC-TRAN-CODE.
           MOVE 0 TO WK-NO-SIGN.
           ADD 0 TO WK-TOT-CREDIT.

           MOVE BR-BANK-ROUTING-NO TO AC-BANK-RT-NUMBER.
           ADD AC-BANK-RT-NO       TO WK-X-HASH WK-T-HASH.
           MOVE BR-BANK-ACCTNO     TO AC-BANK-ACCOUNT-NUMBER.
           MOVE 0                  TO AC-DEPOSIT-AMOUNT.
           ADD 0                   TO WK-TOT-DEPOSIT.
           MOVE BR-NO              TO AC-NAME.
           INSPECT AC-NAME REPLACING FIRST "               "
                        BY BR-CITY AFTER INITIAL " ".
           INSPECT AC-NAME REPLACING FIRST "  " BY ", ".
           INSPECT AC-NAME REPLACING FIRST "  "
                        BY BR-STATE AFTER INITIAL " ".
           MOVE AC-NAME            TO AC-ID-NUMBER.
           MOVE 0                  TO AC-ADDENDA-REC-IND.
           MOVE GP-BANK-ROUTING-NO TO WK-ROUTING-NO.
           MOVE WK-ROUTE1          TO AC-TRACE-PREFIX.
           COMPUTE AC-TRACE-NUMBER = WK-BATCH-COUNT + 1.

           MOVE X"0D"     TO AC-CR.
           MOVE X"0A"     TO AC-LF.

           IF REPORT-FG NOT = "Y"
              PERFORM WRITE-AC-FILE.
           ADD 1 TO AC-KEY.
           ADD 1 TO REC-COUNT WK-BATCH-COUNT.

       EXIT-6-ENTRY-DETAIL.
           EXIT.


      *****************************************************************
       8-COMPANY-BATCH-CONTROL SECTION.
      *****************************************************************

      * CREATE REC TYPE 8: COMPANY/BATCH CONTROL

           MOVE SPACE              TO AC-REC.
           MOVE 8                  TO AC-REC-TYPE.
           MOVE 200                TO AC-SERVICE-CLASS.
           MOVE WK-BATCH-COUNT     TO AC-ENTRY-COUNT.
           ADD WK-BATCH-COUNT      TO WK-TOT-BATCH-COUNT.
           MOVE WK-HASH            TO AC-ENTRY-HASH.
           MOVE WK-TOT-DEBIT       TO AC-TOTAL-DEBIT.
           MOVE WK-TOT-CREDIT      TO AC-TOTAL-CREDIT.
           ADD WK-TOT-DEPOSIT      TO WK-GR-TOT-DEPOSIT.
           ADD WK-TOT-DEBIT        TO WK-GR-TOT-DEBIT.
           ADD WK-TOT-CREDIT       TO WK-GR-TOT-CREDIT.
           MOVE 1                  TO AC-COMPANY-PREFIX2.
           MOVE GP-FED-TAX-ID      TO AC-FED-TAX-ID2.
           MOVE GP-BANK-ROUTING-NO TO WK-ROUTING-NO.
           MOVE WK-ROUTE1          TO AC-BANK-IDENTIFICATION.
           MOVE WK-BATCH-NO        TO AC-BATCH-NUMBER.

           MOVE X"0D"     TO AC-CR.
           MOVE X"0A"     TO AC-LF.

           IF REPORT-FG NOT = "Y"
              PERFORM WRITE-AC-FILE.
           ADD 1 TO AC-KEY.
           ADD 1 TO REC-COUNT.

       EXIT-8-COMPANY-BATCH-CONTROL.
           EXIT.

      *****************************************************************
       9-FILE-CONTROL SECTION.
      *****************************************************************

      * CREATE REC TYPE 9: TRAILER

           MOVE SPACES             TO AC-REC.
           MOVE 9                  TO AC-REC-TYPE.
           MOVE WK-BATCH-NO        TO AC-BATCH-COUNT.
           DIVIDE REC-COUNT BY 10 GIVING AC-BLOCK-COUNT
                               REMAINDER WK-RECS-REM.
           ADD 1                   TO AC-BLOCK-COUNT.
           MOVE WK-TOT-BATCH-COUNT TO AC-09-ENTRY-COUNT.
           MOVE WK-T-HASH          TO WK-X-HASH.
           MOVE WK-HASH            TO AC-09-ENTRY-HASH.
           MOVE WK-GR-TOT-CREDIT   TO AC-09-TOTAL-CREDIT.
           MOVE WK-GR-TOT-DEBIT    TO AC-09-TOTAL-DEBIT.

           MOVE X"0D"     TO AC-CR.
           MOVE X"0A"     TO AC-LF.

           IF REPORT-FG NOT = "Y"
              PERFORM WRITE-AC-FILE.
           ADD 1 TO AC-KEY.
           ADD 1 TO REC-COUNT.

       EXIT-9-FILE-CONTROL.
           EXIT.

      **************************************************
       PAD-FILE SECTION.
      **************************************************

           MOVE ALL "9" TO AC-REC.

           MOVE X"0D"     TO AC-CR.
           MOVE X"0A"     TO AC-LF.

           IF REPORT-FG NOT = "Y"
              PERFORM WRITE-AC-FILE.
           ADD 1 TO AC-KEY.
           ADD 1 TO REC-COUNT.

       EXIT-PAD-FILE.
           EXIT.

      *****************************************************************
       DETAIL-LINE-ROUTINE SECTION.
      *****************************************************************
           MOVE BR-NAME TO D-BRNAME.
           MOVE BR-NO TO D-BRNO.
           MOVE BR-BANK-ACCTNO TO D-BANK-ACCTNO.
           MOVE BR-BANK-ROUTING-NO TO D-ROUTING-NO.
           PERFORM WRITE-DETAIL-LINE.

      *****************************************************************
       PRINT-TOTAL-ROUTINE SECTION.
      *****************************************************************

           MOVE "TOTAL DEPOSIT" TO D-BRNAME.
           MOVE 3 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "TOTAL DEBITS " TO D-BRNAME.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "TOTAL CREDIT " TO D-BRNAME.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      ****************************
       WRITE-DETAIL-LINE SECTION.
      ****************************
           ADD LN-FEED LN-COUNT GIVING LN-WK.
           IF LN-WK > EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.
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
           PERFORM WRITE-PR-REC.
           MOVE 5 TO LN-COUNT.
           MOVE 2 TO LN-FEED.
       END-HEAD-PAGE.
           EXIT.

      ***************************************
       1-FILE-HEADER SECTION.
      ***************************************

           MOVE SPACE TO AC-REC.
           MOVE 0 TO WK-BATCH-NO WK-GR-TOT-DEPOSIT
                     WK-GR-TOT-DEBIT WK-GR-TOT-CREDIT
                     WK-TRACE-NUMBER WK-TOT-BATCH-COUNT.
           MOVE 1                  TO AC-REC-TYPE AC-PRIORITY-CODE.
           MOVE GP-BANK-ROUTING-NO TO AC-DESTINATION.
           MOVE 1                  TO AC-ORIGIN-PREFIX.
           MOVE GP-FED-TAX-ID      TO AC-ORIGIN-TAX-ID.

           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYMMDD.
           MOVE DATE-YYMMDD        TO AC-CREATE-DATE-YYMMDD.
           MOVE "Y" TO TIME-MILITARY-FG.
           PERFORM GET-TIME.
           MOVE T-HH TO WK-HH.
           MOVE T-MM TO WK-MN.
           MOVE WK-TIME             TO AC-CREATE-TIME-HHMM.
           MOVE " "                 TO AC-FILE-MODIFIER.
           MOVE 94                  TO AC-REC-SIZE.
           MOVE 10                  TO AC-BLOCKING.
           MOVE 1                   TO AC-FORMAT-CODE.
           MOVE "FIRST UNION - NC"  TO AC-DESTINATION-NAME.
           MOVE "WORLD ACCEPTANCE " TO AC-ORIGIN-NAME.

           MOVE X"0D"     TO AC-CR.
           MOVE X"0A"     TO AC-LF.

           IF REPORT-FG NOT = "Y"
              PERFORM WRITE-AC-FILE.
           ADD 1 TO AC-KEY.
           ADD 1 TO REC-COUNT.

       EXIT-1-FILE-HEADER.
           EXIT.

      ***************************************
       5-COMPANY-BATCH-HEADER SECTION.
      ***************************************

           MOVE SPACES        TO AC-REC.
           MOVE 0 TO WK-TOT-DEPOSIT WK-BATCH-COUNT WK-X-HASH
                     WK-TOT-DEBIT WK-TOT-CREDIT.
           MOVE 5             TO AC-REC-TYPE.
           MOVE 200           TO AC-SERVICE-CLASS.
           MOVE GP-CORP-NAME  TO AC-COMPANY-NAME.
           MOVE 1             TO AC-COMPANY-PREFIX.
           MOVE GP-FED-TAX-ID TO AC-FED-TAX-ID.
           MOVE "CCD"         TO AC-ENTRY-CLASS.
           MOVE "CONC DEBIT"  TO AC-COMPANY-ENTRY-DESC.

           MOVE EFFECTIVE-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-YYMMDD.
           MOVE DATE-YYMMDD   TO AC-EFFECTIVE-ENTRY-DATE.
           MOVE 1             TO AC-ORIG-STATUS.
           MOVE GP-BANK-ROUTING-NO TO WK-ROUTING-NO.
           MOVE WK-ROUTE1     TO AC-BANK-IDENTIFICATION.
           ADD 1 TO WK-BATCH-NO.
           MOVE WK-BATCH-NO   TO AC-BATCH-NUMBER.

           MOVE X"0D"     TO AC-CR.
           MOVE X"0A"     TO AC-LF.

           IF REPORT-FG NOT = "Y"
              PERFORM WRITE-AC-FILE.
           ADD 1 TO AC-KEY.
           ADD 1 TO REC-COUNT.

       EXIT-5-COMPANY-BATCH-HEADER.
           EXIT.

      ***************************************
       PRINT-RANGE-LINES SECTION.
      ***************************************

           MOVE 4 TO LN-FEED.
           MOVE "TOTAL NO. OF DETAIL RECORDS:" TO RANGE-LINE.
           MOVE WK-BATCH-COUNT TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 2 TO LN-FEED.
           MOVE "TOTAL NO. OF RECORDS:" TO RANGE-LINE.
           MOVE REC-COUNT TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 1 TO LN-FEED.
           MOVE "BEGINNING BRANCH" TO RANGE-LINE.
           MOVE BEG-BRANCH TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING BRANCH" TO RANGE-LINE.
           MOVE END-BRANCH TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "REPORT ONLY" TO RANGE-LINE.
           MOVE REPORT-FG TO R-FLAG.
           PERFORM WRITE-DETAIL-LINE.


       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBLP/POSTDATE.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/DATER.CPY".

      ************************************************
       FORM-RESET SECTION.
      ************************************************
           MOVE EXT-CONAME-CONAME TO PRENOT-CONAME.
           MOVE EXT-CONAME-SYSDATE TO PRENOT-SYSDATE.
           DISPLAY PRENOT-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE PRENOT-FORM-FIELDS TO WR-BUF.
           MOVE PRENOT-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE PRENOT-HELP--FILE TO HELP-LINK-FILE.
           MOVE PRENOT-HELP--DIR TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/PRENOT_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *************************************************************
       MISC-ROUTINE SECTION.
      *************************************************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/GETENV.CPY".
       SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *************************************************************
       IO-ROUTINE SECTION.
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
      *************************************************************
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBSP/SPACI.CPY".
       COPY "LIBGB/GBPRINT.CPY".

       COPY "LIBGB/FERRORS.CPY".

      ********************************************************
       END-PROGRAM SECTION.
      ********************************************************
       END-RUN.

           PERFORM CLOSE-FILES.

           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.

           IF NO-RECORDS-FOUND
              MOVE " NO RECORDS FOUND FOR SELECTED RANGE !" TO MESS
              PERFORM SEND-MESS
           ELSE
              IF WK-GR-TOT-DEBIT NOT = WK-GR-TOT-CREDIT
                 MOVE "TOTALS DEBITS AND CREDITS NOT EQUAL!!!" TO MESS
                 PERFORM SEND-MESS.

       EXIT-PROG.
           IF SV-BEG-BR NOT = 9999
              MOVE SV-BEG-BR TO BEG-BRANCH
              MOVE SV-END-BR TO END-BRANCH.

           IF EXT-ROUTE-BUF = "00"
              MOVE "SP/BANKMU" TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY PRENOT-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
