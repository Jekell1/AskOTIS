       IDENTIFICATION DIVISION.
       PROGRAM-ID.      BWLNST.
       DATE-WRITTEN.   02-17-14.
      ******************************************************************
      *    DESC:  BORROWER BY STATE REPORT
      *
      * DESCRIPTION:
      *
      *  - PROVIDE LOAN INFO TOTALS BY STATE IN BORROWER RECORD,
      *  - WITH OPTIONAL DETAIL
      *
      * REV:
      *  140217 BLM NEW FOR WORLD # 968
      *  140820 BLM ADD OPTIONAL ASCII FILE EXTRACTION, WORLD PR# 1015
      *  BAH 170510 ACTIVATE 8 DIGIT DATES, PD0006
      *  BAH 181003 GLOBAL BWFILE
      *  BAH 181204 GLOBAL LNFILE
      *  BAH 20190922 REMOVED ACCESS-CALL LNFILE
      *  BAH 20220103 CLEAR BWLNST-REC, BWNAME WAS HOLDING CHARACTERS
      *               FROM PRIOR RECORDS WITH LONGER NAMES DUE TO THE STRING
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
           SELECT BWLNST-FILE ASSIGN BWLNST-PATH
                  ORGANIZATION RELATIVE
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RELATIVE KEY BWLNST-KEY
                  FILE STATUS FILE-STAT.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".

      * ASCII FILE EXTRACTION
       FD  BWLNST-FILE
           LABEL RECORDS ARE STANDARD.
       01  BWLNST-REC.
           03  BWLNST-BRNO                  PIC 9(4).
           03  BWLNST-LOANNO                PIC 9(6).
           03  BWLNST-CLASS                 PIC 99.
           03  BWLNST-BWNAME                PIC X(34).
           03  BWLNST-ADR1                  PIC X(24).
           03  BWLNST-ADR2                  PIC X(24).
           03  BWLNST-CITY                  PIC X(15).
           03  BWLNST-STATE                 PIC X(2).
           03  BWLNST-ZIP                   PIC X(10).
           03  BWLNST-LOANDATE-MMDDYY       PIC 9(6).
           03  BWLNST-LNAMT                 PIC 9(6).99.
           03  BWLNST-CURBAL                PIC 9(6).99.
           03  BWLNST-CR                    PIC X.
           03  BWLNST-LF                    PIC X.

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LP/BWLNST.CBL S35 02/20/24-13:20:34 >".
      ****************************
      *     F I L E   S P E C S  *
      ****************************
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01ST.CPY".
       COPY "LIBLP/LP01ST_SQL.CPY".
       COPY "LIBLP/LPWSST.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       01  WK1-IFN       PIC X(3)    VALUE "WK1".
       01  BWLNST-KEY    PIC 9(8)     VALUE 0.
       01  BWLNST-PATH.
           03  BWLNST-PATH-BASE         PIC X(9).
           03  FILLER                   PIC X(1)   VALUE "/".
           03  BWLNST-PATH-MACHINE      PIC X(02).
           03  FILLER                   PIC X(11)  VALUE "/SP/BWLNST.".
           03  BWLNST-PATH-STATE        PIC XX.
           03  FILLER                   PIC X      VALUE ".".
           03  BWLNST-PATH-DATE-YYMMDD  PIC 9(6).

      **********************
      *   LINE WORKERS     *
      **********************
       01  LN-FEED          PIC 99       VALUE 2.
       01  LN-COUNT         PIC 999      VALUE 99.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

      **********************
      *   MISC WORKERS     *
      **********************
       01  DISPLAY-MESS.
           03  FILLER           PIC X(25)    VALUE
           "REPORT IN PROGRESS.......".
           03  DISPLAY-MESS-BR  PIC 9999.
           03  FILLER           PIC X        VALUE " ".
           03  DISPLAY-MESS-ST  PIC XX.
           03  FILLER           PIC X(7)     VALUE " ".
       01  FIRST-TIME           PIC X        VALUE "Y".
       01  RECORDS-FOUND        PIC X        VALUE " ".
           88  NO-RECORDS-FOUND              VALUE "N".
           88  SKIP-RECORDS-FOUND            VALUE " ".

       01  LEGEND               PIC X(80)     VALUE SPACES.
       01  LEGEND-LIST          PIC X(09)     VALUE "LEGEND:B.".
       01  SUB                  PIC 9(6)     COMP-3.
       01  WK-TOTAL             PIC 9(10)    VALUE 0.

      *   TOTALS SUB 1 = BRANCH
      *              2 = GRAND
       01  TOTALS-TABLE.
           03  TOTAL-TBL              OCCURS 2.
               05  TOT-NO-OPEN        PIC 9(9)      COMP-3.
               05  TOT-NO-PAIDOUT     PIC 9(9)      COMP-3.
               05  TOT-NO-PL-ACT      PIC 9(9)      COMP-3.
               05  TOT-NO-PL-INACT    PIC 9(9)      COMP-3.
               05  TOT-LNAMT          PIC S9(10)V99 COMP-3.
               05  TOT-CURBAL         PIC S9(10)V99 COMP-3.

      ***********************************
      *     S C R E E N   F I E L D S   *
      ***********************************
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  BEG-BRANCH      PIC 9999.
           03  END-BRANCH      PIC 9999.
           03  STATE           PIC XX.
           03  PAIDOUTS        PIC X.
           03  PL              PIC X.
           03  PLTYPE          PIC X.
           03  PLACTIVE        PIC X.
           03  SUM-ONLY        PIC X.
           03  GROUP-FLAG      PIC X.
           03  ENT-GROUP       PIC X(4).
           03  CREATE-FILE-FG  PIC X.

       01  VERSION-CONTROL     PIC X VALUE "A".

       01  SV-BEG-BR           PIC 9999  VALUE 9999.
       01  SV-END-BR           PIC 9999  VALUE 9999.
       01  SV-GROUP-FLAG       PIC X     VALUE SPACES.
       01  SV-ENT-GROUP        PIC X(4)  VALUE SPACES.

      ***********************************
      *     P R I N T   B U F F E R S   *
      ***********************************
       01  HEAD1.
           03  H-DATE           PIC X(8)     VALUE " ".
           03  FILLER           PIC XX       VALUE " ".
           03  H-HR             PIC 99       VALUE 0.
           03  FILLER           PIC X        VALUE ":".
           03  H-MIN            PIC 99       VALUE 0.
           03  FILLER           PIC X(30)    VALUE " ".
           03  H-NAME           PIC X(40)    VALUE " ".
           03  FILLER           PIC X(38)    VALUE " ".
           03  FILLER           PIC X(5)     VALUE "PAGE ".
           03  H-PAGE-NO        PIC ZZZ9.

       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(22)    VALUE " ".
           03  H-TITLE          PIC X(32)    VALUE
           "LOANS BY BORROWER STATE REPORT: ".
           03  H-STATE          PIC X(16).
           03  FILLER           PIC X(42) VALUE " ".

       01  HEAD3.
           03  FILLER      PIC X(70)    VALUE
               "BRNO   ACCOUNT #  CLASS  BORROWER NAME".
           03  FILLER      PIC X(21)    VALUE
               "CITY".
           03  FILLER      PIC X(40)    VALUE
               "LOANDATE         LOAN AMT    CUR BALANCE".

       01  DETAIL-LINE     PIC X(132) VALUE SPACES.
       01  D-LINE1         REDEFINES DETAIL-LINE.
           03  D-BRNO      PIC ZZZ9.
           03  FILLER      PIC XXX.
           03  D-LOANNO    PIC ZZZZZ9.
           03  FILLER      PIC X(5).
           03  D-CLASS     PIC 99.
           03  FILLER      PIC X(5).
           03  D-BWNAME    PIC X(43).
           03  FILLER      PIC X(1).
           03  D-CITY      PIC X(20).
           03  FILLER      PIC X(2).
           03  D-LOANDATE  PIC 99/99/99.
           03  FILLER      PIC X(3).
           03  D-LNAMT     PIC ZZZ,ZZZ,ZZ9.99-.
           03  D-CURBAL    PIC ZZZ,ZZZ,ZZ9.99-     BLANK ZERO.

       01  TOTAL-LINE REDEFINES DETAIL-LINE.
           03  D-T-DESC             PIC X(14).
           03  D-T-NO-OPEN          PIC ZZZ,ZZZ,ZZ9.
           03  D-T-PROMPT1          PIC X(4).
           03  D-T-NO-PAIDOUT       PIC ZZZ,ZZZ,ZZ9.
           03  D-T-PROMPT2          PIC X(5).
           03  D-T-NO-PL            PIC ZZZ,ZZZ,ZZ9.
           03  D-T-PROMPT3          PIC X(5).
           03  D-T-NO-PL-ACT        PIC ZZZ,ZZZ,ZZ9.
           03  D-T-PROMPT4          PIC X(4).
           03  D-T-NO-PL-INACT      PIC ZZ,ZZZ,ZZ9.
           03  D-T-PROMPT5          PIC X(5).
           03  D-T-NO-TOTAL         PIC ZZZ,ZZZ,ZZ9.
           03  D-T-LNAMT            PIC ZZZ,ZZZ,ZZ9.99-   BLANK ZERO.
           03  D-T-CURBAL           PIC ZZZ,ZZZ,ZZ9.99-   BLANK ZERO.

       01  RANGE-LN REDEFINES DETAIL-LINE.
           03  RANGE-LINE       PIC X(20).
           03  RANGE-SELECTION  PIC X(112).

       01  SUMMARY-HEAD.
           03  FILLER      PIC X(15)     VALUE SPACES.
           03  FILLER      PIC X(42)    VALUE
            "      OPEN        PAIDOUT             P&L ".
           03  FILLER      PIC X(46)    VALUE
            "        P&L ACT     P&L INACT           TOTAL ".
           03  FILLER      PIC X(29)    VALUE
            "  LOAN AMOUNT    CUR BALANCE ".

       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBLP/LPWKPATH.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/BWLNST_DEF.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBLP/BWLNST_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".


      ****************************************
      *                                      *
      *     P R O G R A M   C O N T R O L    *
      *                                      *
      ****************************************
       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBLP/BWLNST_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE BWLNST-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-ST1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE BWLNST-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      *************************************
       MAIN-MODULE SECTION.
      *************************************

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "BWLNST" TO FORM-PROG VDU-FORM-NAME.

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

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO BW-PATH-OVERRIDE
                                              LN-PATH-OVERRIDE.

           IF PRU-LP NOT = "VDU" AND NOT = "PRU"
              IF EXT-ROUTE-BUF = "00"
                 MOVE BR-NO TO DISPLAY-MESS-BR
                 MOVE BR-STATE TO DISPLAY-MESS-ST
                 MOVE DISPLAY-MESS TO MESS
                 PERFORM SEND-LEGEND.

           PERFORM OPEN-BW1-FILE.
           PERFORM OPEN-LN1-FILE.

           MOVE LN-PATH-OWNBR TO LN-OWNBR.
           MOVE 0             TO LN-ACCTNO.

           PERFORM START-LN1-FILE.
       NEXT-LN.
           PERFORM READ-LN1-FILE-NEXT.
           IF IO-FG = 9 OR (LN-OWNBR NOT = LN-PATH-OWNBR)
              GO TO END-OF-GROUP.

           IF LN-OWNBR > END-BRANCH
              GO TO END-OF-GROUP.

           IF ( LN-POCD-NONLOAN )
              GO TO NEXT-LN.

           IF PAIDOUTS = "O"
              IF ( LN-CURBAL NOT = 0 ) OR
                 ( LN-INTBAL NOT = 0 ) OR
                 ( LN-LCBAL  NOT = 0 ) OR
                 ( LN-OTHBAL NOT = 0 )
                     GO TO NEXT-LN.

           IF PAIDOUTS = "S"
              IF LN-CURBAL = 0
                 IF LN-INTBAL = 0
                    IF LN-LCBAL = 0
                       IF LN-OTHBAL = 0
                          GO TO NEXT-LN.

           IF PL = "O"
              IF LN-PLDATE = 0
                 GO TO NEXT-LN.

           IF PL = "S"
              IF LN-PLDATE NOT = 0
                 GO TO NEXT-LN.

           IF LN-PLDATE NOT = 0
              IF PLACTIVE = "A"
                 IF LN-PLCD NOT = "P"
                    GO TO NEXT-LN.

           IF LN-PLDATE NOT = 0
              IF PLACTIVE = "I"
                 IF LN-PLCD NOT = "I"
                    IF LN-PLCD NOT = "W"
                       GO TO NEXT-LN.

           IF LN-PLDATE NOT = 0
              IF PLTYPE NOT = "A"
                 IF LN-PLTYPE NOT = PLTYPE
                    GO TO NEXT-LN.

           MOVE LN-SSNO(1) TO BW-SSNO.
           PERFORM READ-BW1-FILE.
           IF IO-FG NOT = 0
              GO TO NEXT-LN.

           IF BW-STATE NOT = STATE
              GO TO NEXT-LN.

           IF LN-PLDATE NOT = 0
              IF PLACTIVE = "A"
                 ADD 1 TO TOT-NO-PL-ACT(1)
                          TOT-NO-PL-ACT(2)
              ELSE
                 ADD 1 TO TOT-NO-PL-INACT(1)
                          TOT-NO-PL-INACT(2)
              END-IF
           ELSE
              IF (LN-CURBAL = 0 AND LN-INTBAL = 0
                    AND LN-LCBAL = 0 AND LN-OTHBAL = 0)
                 ADD 1 TO TOT-NO-PAIDOUT(1)
                          TOT-NO-PAIDOUT(2)
              ELSE
                 ADD 1 TO TOT-NO-OPEN(1)
                          TOT-NO-OPEN(2).
           ADD LN-LNAMT  TO TOT-LNAMT(1)
                            TOT-LNAMT(2).
           ADD LN-CURBAL TO TOT-CURBAL(1)
                            TOT-CURBAL(2).

           IF SUM-ONLY = "D"
              PERFORM DETAIL-LINE-PROCESS.
           IF CREATE-FILE-FG = "Y"
              PERFORM CREATE-EXTRACT-RECORD.
           GO TO NEXT-LN.

       END-OF-GROUP.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM BRANCH-TOTALS.
           GO TO NEXT-GROUP-LOC.

       END-ROUTINE.
           IF FIRST-TIME = "Y"
              MOVE "N" TO RECORDS-FOUND
              GO TO END-PROGRAM
           ELSE
              MOVE "Y" TO RECORDS-FOUND.
           PERFORM GRAND-TOTALS.

           PERFORM PRINT-RANGE-LINES.

           GO TO END-PROGRAM.

      ***********************************
       INITIALIZATION SECTION.
      ***********************************
           MOVE BWLNST-QUEUE-POS TO Q-POS.
           MOVE BWLNST-COPIES-POS TO C-POS.
           MOVE BWLNST-PRU-POS TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
       COPY "LIBGB/EOINIT.CPY".

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

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 2
               MOVE 0 TO TOT-NO-OPEN(SUB)
                         TOT-NO-PAIDOUT(SUB)
                         TOT-NO-PL-ACT(SUB)
                         TOT-NO-PL-INACT(SUB)
                         TOT-LNAMT(SUB)
                         TOT-CURBAL(SUB)
           END-PERFORM.

       SET-HEAD.
           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           MOVE STATE TO ST-CODE.
           PERFORM OPEN-ST1-FILE.
           PERFORM READ-ST1-FILE.
           PERFORM CLOSE-ST1-FILE.
           MOVE ST-DESC TO H-STATE.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.

           IF CREATE-FILE-FG = "N"
              GO TO CONTINUE-INIT.
      * CREATE OUTPUT ASCII FILE:
           IF EXT-ROUTE-BUF NOT = "00"
              IF EXT-ROUTE-BUF NOT = "70"
                 GO TO CONTINUE-INIT.

           MOVE EXT-FILPATH-BASE    TO BWLNST-PATH-BASE.
           MOVE EXT-FILPATH-MACHINE TO BWLNST-PATH-MACHINE.
           MOVE STATE               TO BWLNST-PATH-STATE.
           MOVE EXT-CONAME-SYSDATE  TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYMMDD.
           MOVE DATE-YYMMDD         TO BWLNST-PATH-DATE-YYMMDD.
           MOVE BWLNST-PATH         TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT NOT = "00"
              PERFORM OPEN-BWLNST-FILE-OUTPUT
              GO TO CONTINUE-INIT.

           IF EXT-ROUTE-BUF = "70"
              PERFORM OPEN-BWLNST-FILE-OUTPUT
              GO TO CONTINUE-INIT.
              
        INITIALIZATION-ASK.
           PERFORM ALARM.
           MOVE "FILE ALREADY EXISTS, CONTINUE (Y/N):" TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "R RA010CONT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS NOT = "00"
              GO TO INITIALIZATION-ASK.
           IF VDUBUF = "N"
              MOVE "E" TO EXT-EOBUF-ERRCD
              GO TO EXIT-INITIALIZE.
           IF VDUBUF NOT = "Y"
              GO TO INITIALIZATION-ASK.

           MOVE SPACES TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE SPACES TO VDUBUF.
           MOVE "S  A010CONT." TO VDU.
           PERFORM SCREENIO.
         
           PERFORM OPEN-BWLNST-FILE-OUTPUT.

       CONTINUE-INIT.
           PERFORM OPEN-PR-FILE.

           GO TO EXIT-INITIALIZE.
       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.
       EXIT-INITIALIZE.
           EXIT.

      ***********************************
       EO-EXEC-AUDIT SECTION.
      ***********************************
           EXIT.

      ***********************************
       ENTER-PARAMETERS SECTION.
      ***********************************
       ENTER-PARM.

           IF VDUSTATUS = "1U"
              GO TO SUMYN.

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
              GO TO ST-01.
       BRANCH-IS-NUMERIC.
           MOVE "N" TO GROUP-FLAG.
           MOVE WGR-SRESULT TO BEG-BRANCH.
           MOVE "SN N040BR1." TO VDU.
           MOVE BEG-BRANCH TO VDUNUM0.
           PERFORM SCREENIO.
       BR-02.
           MOVE "ENNN040BR2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO BR-02.
           MOVE VDUNUM0 TO END-BRANCH.
           IF BEG-BRANCH > END-BRANCH
              MOVE " INVALID BRANCH RANGE !" TO MESS
              PERFORM SEND-MESS
              GO TO BR-01.
           GO TO ST-01.
       ALL-BR.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "N" TO GROUP-FLAG.

       ST-01.
           MOVE "E RA020ST1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ST-01.

           MOVE VDUBUF TO STATE.

      *----------------------------------------------------------------
       PAIDOUTS-YN.
           MOVE "E  A010PAIDOUTS." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ST-01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO PAIDOUTS-YN.

           MOVE VDUBUF TO PAIDOUTS.

           IF PAIDOUTS NOT = "S"
              IF PAIDOUTS NOT = "I"
                 IF PAIDOUTS NOT = "O"
                    GO TO PAIDOUTS-YN.

           IF PL NOT = "S" GO TO PLYN.

      *----------------------------------------------------------------
       PLYN.
           MOVE "E  A010PL." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO PAIDOUTS-YN.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO PLYN.

           MOVE VDUBUF TO PL.

           IF PL = "I" OR "O"
              GO TO PLTYPEYN.

           IF PL NOT = "S" GO TO PLYN.

           MOVE "S  A010ACTIVE." TO VDU.
           MOVE "B"              TO PLACTIVE VDUBUF.
           PERFORM SCREENIO.
           GO TO SUMYN.
      *----------------------------------------------------------------
       PLTYPEYN.
           MOVE "E  A010PLTYPE." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO PLYN.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO PLTYPEYN.

           MOVE VDUBUF TO PLTYPE.

           IF NOT (PLTYPE = "A" OR "P" OR "2" OR "3" OR "4" OR "5")
              GO TO PLTYPEYN.

      *----------------------------------------------------------------
       ACTVYN.
           MOVE "E  A010ACTIVE." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO PLTYPEYN.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ACTVYN.

           MOVE VDUBUF TO PLACTIVE.

           IF PLACTIVE NOT = "I" AND NOT = "A" AND NOT = "B"
              GO TO ACTVYN.
      *----------------------------------------------------------------
       EXTRACTYN.
           MOVE "E  A010EXTRACT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ACTVYN.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO SUMYN.
           MOVE VDUBUF TO CREATE-FILE-FG.
           IF CREATE-FILE-FG NOT = "Y" 
             IF CREATE-FILE-FG NOT = "N" 
                 MOVE "PLEASE ENTER 'Y' OR 'N'" TO MESS
                 PERFORM SEND-MESS
                 GO TO EXTRACTYN.
      *----------------------------------------------------------------

      *----------------------------------------------------------------
       SUMYN.
           MOVE "E  A010SUMONLY." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO EXTRACTYN.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO SUMYN.
           MOVE VDUBUF TO SUM-ONLY.
           IF SUM-ONLY NOT = "S"
              IF SUM-ONLY NOT = "D"
                 MOVE "PLEASE ENTER S(SUMMARY) OR D(DETAIL)" TO MESS
                 PERFORM SEND-MESS
                 GO TO SUMYN.

       EXIT-PARM.
           EXIT.

      ***********************************
       DISPLAY-PARAMETERS SECTION.
      ***********************************

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
           MOVE "S  A020ST1." TO VDU.
           MOVE STATE TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A010PL." TO VDU.
           MOVE PL TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A010PLTYPE." TO VDU.
           MOVE PLTYPE TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A010PLACTIVE." TO VDU.
           MOVE PLACTIVE TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A010EXTRACT." TO VDU.
           MOVE CREATE-FILE-FG TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A010SUMONLY." TO VDU.
           MOVE SUM-ONLY TO VDUBUF.
           PERFORM SCREENIO.

      ****************************
       DETAIL-LINE-PROCESS SECTION.
      ****************************

           MOVE LN-OWNBR     TO D-BRNO.
           MOVE LN-ACCTNO    TO D-LOANNO.
           MOVE LN-CLASS     TO D-CLASS.
           STRING BW-LNAME, ", ", BW-FNAME
              DELIMITED BY "   " INTO D-BWNAME.
           MOVE LN-LOANDATE  TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY  TO D-LOANDATE.
           MOVE LN-LNAMT     TO D-LNAMT.
           MOVE LN-CURBAL    TO D-CURBAL.
           MOVE BW-CITY      TO D-CITY.
           INSPECT D-CITY REPLACING FIRST "    " BY ", &&".
           INSPECT D-CITY REPLACING FIRST "&&" BY BW-STATE.

           PERFORM WRITE-DETAIL-LINE.

       END-DETAIL-LN-PROCESS.
           EXIT.

 
      ******************************
       CREATE-EXTRACT-RECORD SECTION.
      ******************************
 
           MOVE SPACES       TO  BWLNST-REC.

           MOVE LN-OWNBR     TO BWLNST-BRNO.
           MOVE LN-ACCTNO    TO BWLNST-LOANNO.
           MOVE LN-CLASS     TO BWLNST-CLASS.
           STRING BW-LNAME, ", ", BW-FNAME
              DELIMITED BY "   " INTO BWLNST-BWNAME.
           MOVE BW-ADR1      TO BWLNST-ADR1.
           MOVE BW-ADR2      TO BWLNST-ADR2.
           MOVE BW-CITY      TO BWLNST-CITY.
           MOVE BW-STATE     TO BWLNST-STATE.
           MOVE BW-ZIP       TO BWLNST-ZIP.
           MOVE LN-LOANDATE  TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY  TO BWLNST-LOANDATE-MMDDYY.
           MOVE LN-LNAMT     TO BWLNST-LNAMT.
           MOVE LN-CURBAL    TO BWLNST-CURBAL.
           MOVE X"0D"        TO BWLNST-CR.
           MOVE X"0A"        TO BWLNST-LF.
           ADD 1             TO BWLNST-KEY.
           PERFORM WRITE-BWLNST-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

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
           IF SUM-ONLY = "S"
              GO TO HEAD-SUMMARY.
           MOVE 2 TO LN-FEED.
           MOVE HEAD3 TO PR-REC.
           PERFORM WRITE-PR-REC.
           MOVE 4 TO LN-COUNT.
           MOVE 2 TO LN-FEED.
           GO TO END-HEAD-PAGE.
       HEAD-SUMMARY.
           MOVE 2 TO LN-FEED.
           MOVE SUMMARY-HEAD TO PR-REC.
           PERFORM WRITE-PR-REC.
           MOVE 4 TO LN-COUNT.
           MOVE 2 TO LN-FEED.
       END-HEAD-PAGE.
           EXIT.

      ****************************
       WRITE-DETAIL-LINE SECTION.
      ****************************
           IF FIRST-TIME = "Y"
              MOVE EXT-CONAME-RPT-LINES TO LN-COUNT
              MOVE "N" TO FIRST-TIME.
           ADD LN-FEED TO LN-COUNT.
           IF LN-COUNT > EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.
           MOVE DETAIL-LINE TO PR-REC
           PERFORM WRITE-PR-REC.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      *******************************
       PRINT-RANGE-LINES SECTION.
      *******************************

           MOVE 5 TO LN-FEED.
           IF GROUP-FLAG = "Y"
              MOVE "BRANCH GROUP       :" TO RANGE-LINE
              MOVE ENT-GROUP              TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE
           ELSE
              MOVE "BEGINNING BRANCH   :" TO RANGE-LINE
              MOVE BEG-BRANCH             TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE
              MOVE "ENDING             :" TO RANGE-LINE
              MOVE END-BRANCH             TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE.

           MOVE "STATE              :"    TO RANGE-LINE.
           MOVE STATE                     TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "INCLUDE PAIDOUTS?  :"    TO RANGE-LINE.
           MOVE PAIDOUTS                  TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "INCLUDE P&L'S   ?  :"    TO RANGE-LINE.
           MOVE PL                        TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "P&L TYPE           :"    TO RANGE-LINE.
           MOVE PLTYPE                    TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "P&L ACTIVE/INACTIVE:"    TO RANGE-LINE.
           MOVE PLACTIVE                  TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF CREATE-FILE-FG = "Y"
              MOVE "EXTRACT FILE PATH:" TO RANGE-LINE
              MOVE BWLNST-PATH TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           MOVE "SUMMARY OR DETAIL  :"    TO RANGE-LINE.
           MOVE SUM-ONLY                  TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

      *******************************
       GRAND-TOTALS SECTION.
      *******************************
           MOVE SPACES TO DETAIL-LINE.

           MOVE "&& STATE TOT :" TO D-T-DESC.
           INSPECT D-T-DESC REPLACING FIRST "&&" BY STATE.
           MOVE 2 TO SUB.
           PERFORM PRINT-TOTALS.

      *******************************
       BRANCH-TOTALS SECTION.
      *******************************
           MOVE SPACES TO DETAIL-LINE.
           IF TOT-NO-OPEN(1) = 0
               IF TOT-NO-PAIDOUT(1) = 0
                 IF TOT-NO-PL-ACT(1) = 0
                   IF TOT-NO-PL-INACT(1) = 0
                     IF TOT-LNAMT(1) = 0
                       IF TOT-CURBAL(1) = 0
                          GO TO EXIT-BRANCH-TOTALS.

           MOVE "BR# &&&& TOT :" TO D-T-DESC.
           INSPECT D-T-DESC REPLACING FIRST "&&&&" BY BR-NO.
           MOVE 1 TO SUB.
           MOVE 2 TO LN-FEED.
           PERFORM PRINT-TOTALS.

           MOVE 0 TO TOT-NO-OPEN(1)
                     TOT-NO-PAIDOUT(1)
                     TOT-NO-PL-ACT(1)
                     TOT-NO-PL-INACT(1)
                     TOT-LNAMT(1)
                     TOT-CURBAL(1).

           MOVE 2 TO LN-FEED.

       EXIT-BRANCH-TOTALS.
           EXIT.

      *******************************
       PRINT-TOTALS SECTION.
      *******************************
           IF FIRST-TIME = "Y"
              PERFORM HEAD-PAGE
              MOVE "N" TO FIRST-TIME.
           IF SUM-ONLY = "D"
              MOVE 2 TO LN-FEED
              MOVE SUMMARY-HEAD TO PR-REC
              PERFORM WRITE-PR-REC
              ADD 2 TO LN-COUNT
              MOVE 1 TO LN-FEED
           ELSE
              MOVE 2 TO LN-FEED.
           MOVE TOT-NO-OPEN(SUB)     TO D-T-NO-OPEN.
           MOVE TOT-NO-PAIDOUT(SUB)  TO D-T-NO-PAIDOUT.
           COMPUTE WK-TOTAL =
              TOT-NO-PL-ACT(SUB) + TOT-NO-PL-INACT(SUB).
           MOVE WK-TOTAL             TO D-T-NO-PL.
           MOVE TOT-NO-PL-ACT(SUB)   TO D-T-NO-PL-ACT.
           MOVE TOT-NO-PL-INACT(SUB) TO D-T-NO-PL-INACT.
           COMPUTE WK-TOTAL =
              TOT-NO-OPEN(SUB)   + TOT-NO-PAIDOUT(SUB) +
              TOT-NO-PL-ACT(SUB) + TOT-NO-PL-INACT(SUB).
           MOVE WK-TOTAL             TO D-T-NO-TOTAL.
           MOVE TOT-LNAMT(SUB)       TO D-T-LNAMT.
           MOVE TOT-CURBAL(SUB)      TO D-T-CURBAL.
           PERFORM WRITE-DETAIL-LINE.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".

      ****************************************************
       FORM-RESET SECTION.
      ****************************************************
           MOVE EXT-CONAME-CONAME TO BWLNST-CONAME.
           MOVE EXT-CONAME-SYSDATE TO BWLNST-SYSDATE.
           DISPLAY BWLNST-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE BWLNST-FORM-FIELDS TO WR-BUF.
           MOVE BWLNST-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ****************************************************
       SETUP-LINK-INFO SECTION.
      ****************************************************
           MOVE BWLNST-HELP--FILE TO HELP-LINK-FILE.
           MOVE BWLNST-HELP--DIR  TO HELP-LINK-DIR.

      ****************************************************
       VDU-CONVERT-FIELD SECTION.
      ****************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/BWLNST_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ************************************************
       MISC-PARAGRAPHS SECTION.
      ************************************************
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/RMFILE.CPY".
       SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE LEGEND-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
 

      ****************************************
       IO-ROUTINE SECTION.
      ****************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPSTGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBLP/LPST1IN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
 
       OPEN-BWLNST-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE BWLNST-PATH TO E-FILE.
           OPEN OUTPUT BWLNST-FILE.
           IF IO-FG = 8
              GO TO OPEN-BWLNST-FILE-OUTPUT.
           IF IO-FG = 7
              CLOSE BWLNST-FILE
              GO TO OPEN-BWLNST-FILE-OUTPUT.
           UNLOCK BWLNST-FILE.
       OPEN-BWLNST-FILE.
           PERFORM OPEN-IT.
           MOVE BWLNST-PATH TO E-FILE.
           OPEN I-O BWLNST-FILE.
           IF IO-FG = 8
              GO TO OPEN-BWLNST-FILE.
           IF IO-FG = 7
              CLOSE BWLNST-FILE
              GO TO OPEN-BWLNST-FILE.
           UNLOCK BWLNST-FILE.
       WRITE-BWLNST-FILE.
           PERFORM WRITE-IT.
           MOVE BWLNST-PATH TO E-FILE.
           MOVE BWLNST-KEY TO E-KEYX.
           WRITE BWLNST-REC.
           IF IO-FG = 8
              GO TO WRITE-BWLNST-FILE.
           UNLOCK BWLNST-FILE.
       CLOSE-BWLNST-FILE.
           PERFORM CLOSE-IT.
           CLOSE BWLNST-FILE.
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      ****************************************
       END-PROGRAM SECTION.
      ****************************************
       END-RUN.
           PERFORM CLOSE-FILES.

           IF ( SKIP-RECORDS-FOUND )
              GO TO EXIT-PROG.

           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.
           IF NO-RECORDS-FOUND
              MOVE "NO RECORDS FOUND FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS.
       EXIT-PROG.

           MOVE MKTEMP-BUF TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.

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
              MOVE "LP/LPR2MU" TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY BWLNST-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
