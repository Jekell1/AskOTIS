       IDENTIFICATION DIVISION.
       PROGRAM-ID.      INSAUD.
       DATE-WRITTEN.  052207.
      ******************************************************************
      *
      *                        (LP)
      * DESCRIPTION:    INSURANCE AUDIT REPORT.
      *                 WORLD PR# 543
      *
      * REV:
      *  BAH 160125 REMOVE LTFILE PD0003
      * BAH 170605 ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 181210 GLOBAL LNFILE
      * BAH 20190922 REMOVED ACCESS-CALL LNFILE
      *  BAH 2024.0815 ELIMINATE PRINTING OF SSNO #1673 S35Q-118
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
           "@(#)LP/INSAUD.CBL S35 02/21/24-10:32:43 >".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
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
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".

      **********************
      *   LINE WORKERS     *
      **********************
       01  LN-WORKERS.
           03  LN-FEED          PIC 99       VALUE 0.
           03  LN-COUNT         PIC 999      VALUE 99.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

      **********************
      *   MISC WORKERS     *
      **********************
       01  DISPLAY-MESS.
           03  FILLER           PIC X(18)    VALUE
           "IN PROGRESS.......".
           03  DISPLAY-MESS-BR  PIC 9(4).
           03  FILLER           PIC X        VALUE " ".
           03  DISPLAY-MESS-CL  PIC 99.
           03  FILLER           PIC X        VALUE "-".
           03  DISPLAY-MESS-LN  PIC 9(6).

       01  FIRST-TIME           PIC X        VALUE "Y".
       01  RECORDS-FOUND        PIC X        VALUE " ".
           88  NO-RECORDS-FOUND              VALUE "N".
       01  BR-TOTALS            PIC X        VALUE "N".
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".
       01  NAME-WORKER          PIC X(34).
       01  FIRST-NAME           PIC X(16).
       01  HOLD-BRNO            PIC 9(4)     VALUE 0.
       01  SUB                  PIC 9(6) COMP-3.
       01  HOLD-LN1-KEY         PIC 9(12).
       01  W-LOAN-NUM.
           03  FILLER           PIC XXX.
           03  LOAN-NUM         PIC X(13).

      *   TOTALS SUB 1 = CLASS
      *              2 = BRANCH
       01  TOTALS-TABLE.
           03  TOTAL-TBL               OCCURS 2.
               05  T-NOACCTS           PIC 9(6)         COMP-3.
       01  CLASS-FG                    PIC X(3)    VALUE "   ".
           88  CLASS-BEG                           VALUE "BEG".
           88  CLASS-END                           VALUE "END".
       01  TOT-CUR-INS-COVERAGES       PIC 9.
       01  TOT-PRIOR-INS-COVERAGES     PIC 9.
       01  COV-TABLE OCCURS 6.
           03  CUR-COV                 PIC X.
           03  PRIOR-COV               PIC X.
      ***********************************
      *     S C R E E N   F I E L D S   *
      ***********************************
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  BEG-BRANCH       PIC 9(4).
           03  END-BRANCH       PIC 9(4).
           03  BEG-CLASS        PIC 9(3).
           03  END-CLASS        PIC 9(3).
           03  BEG-DATE         PIC 9(8).
           03  END-DATE         PIC 9(8).
           03  DROPPED-INS-FG   PIC X.
           03  GROUP-FLAG       PIC X.
           03  ENT-GROUP        PIC X(4).

       01  VERSION-CONTROL     PIC X VALUE "A".

       01  SV-BEG-BR           PIC 9999  VALUE 9999.
       01  SV-END-BR           PIC 9999  VALUE 9999.
       01  SV-GROUP-FLAG       PIC X     VALUE SPACES.
       01  SV-ENT-GROUP        PIC X(4)  VALUE SPACES.

       COPY "LIBGB/PASSWDW.CPY".

      ***********************************
      *     P R I N T   B U F F E R S   *
      ***********************************
       01  HEAD1.
           03  H-DATE           PIC X(8)     VALUE " ".
           03  FILLER           PIC XX       VALUE " ".
           03  H-TIME           PIC X(8)     VALUE " ".
           03  FILLER           PIC X(27)    VALUE " ".
           03  H-NAME           PIC X(40)    VALUE " ".
           03  FILLER           PIC X(38)    VALUE " ".
           03  FILLER           PIC X(5)     VALUE "PAGE ".
           03  H-PAGE-NO        PIC ZZZ9.

       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(35)    VALUE " ".
           03  FILLER           PIC X(22)    VALUE
           "INSURANCE AUDIT REPORT".

       01  HEAD3.
           03  FILLER      PIC X(8)     VALUE "BRANCH :".
           03  FILLER      PIC XX       VALUE " ".
           03  H-BRNO      PIC 9(4).
           03  H-BRNOX     REDEFINES H-BRNO
                           PIC X(4).
           03  FILLER      PIC X        VALUE " ".
           03  H-BRDESC    PIC X(20).
           03  FILLER      PIC X(97)    VALUE " ".

       01  HEAD4.
           03  FILLER      PIC X(17)  VALUE "ACCOUNT NO.".
           03  FILLER      PIC X(13)  VALUE SPACES.
           03  FILLER      PIC X(22)  VALUE "NAME                  ".
           03  FILLER      PIC X(10)  VALUE "LOAN DATE ".
           03  FILLER      PIC X(10)  VALUE "  LOAN AMT".
           03  FILLER      PIC X(11)  VALUE " PRIOR LN# ".
           03  FILLER      PIC X(19)  VALUE "PRIOR INS COVERAGE ".
           03  FILLER      PIC X(20)  VALUE "CURRENT INS COVERAGE".
           03  FILLER      PIC X(10)  VALUE "  PROCEEDS".

       01  HEAD5.
           03  FILLER      PIC X(83)  VALUE SPACES.
           03  FILLER      PIC X(19)  VALUE "L  A  P  1  2  3   ".
           03  FILLER      PIC X(20)  VALUE "L  A  P  1  2  3    ".
           03  FILLER      PIC X(10)  VALUE SPACES.

       01  DETAIL-LINE         PIC X(132) VALUE SPACES.
       01  D-LINE1             REDEFINES DETAIL-LINE.
           03  D-CLASS         PIC 99.
           03  D-DASH          PIC X.
           03  D-LOAN-NUM      PIC X(13).
           03  FILLER          PIC X(14).
           03  D-BWNAME        PIC X(22).
           03  FILLER          PIC X.
           03  D-LNDATE        PIC 99/99/99       BLANK ZERO.
           03  FILLER          PIC X.
           03  D-LNAMT         PIC ZZZ,ZZ9.99     BLANK ZERO.
           03  FILLER          PIC XX.
           03  D-PRIOR-NO      PIC ZZZZZ9.
           03  FILLER          PIC XXX.
           03  D-PRIOR-COV-TABLE OCCURS 6.
               05  D-PRIOR-COV PIC X.
               05  FILLER      PIC XX.
           03  FILLER          PIC XX.
           03  D-CUR-COV-TABLE OCCURS 6.
               05  D-CUR-COV   PIC X.
               05  FILLER      PIC XX.
           03  FILLER          PIC X.
           03  D-PROCEEDS      PIC ZZZ,ZZ9.99     BLANK ZERO.

       01  TOTAL-LINE               REDEFINES DETAIL-LINE.
           03  D-T-DESC             PIC X(20).
           03  D-T-NOACCTS          PIC ZZZZZ9.

       01  RANGE-LN                 REDEFINES DETAIL-LINE.
           03  RANGE-LINE           PIC X(20).
           03  RANGE-SELECTION      PIC X(112).

       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBWI/LCSCANW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBLP/LPSTATW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       COPY "LIBGB/SYSTEMW.CPY".
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBLP/EMSFRG_CMD.CPY".

       01  MSEL-LIST            PIC X(5)  VALUE "MSEL.".
       01  LEGEND               PIC X(70).
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/INSAUD_DEF.CPY".
       COPY "LIBLP/INSAUD_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *****************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      *****************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBLP/INSAUD_SCN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ******************************************************************
      *
      *    M A I N - M O D U L E
      *
      ******************************************************************
       MAIN-MODULE SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "INSAUD" TO FORM-PROG VDU-FORM-NAME.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

           MOVE "N" TO RECORDS-FOUND.

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

       MOVE-BRANCH-PATH-INFO.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO BW-PATH-OVERRIDE
                                              LN-PATH-OVERRIDE.

           PERFORM OPEN-BW1-FILE.
           PERFORM OPEN-LN1-FILE.

           MOVE LN-PATH-OWNBR TO LN-OWNBR.
           MOVE 0             TO LN-ACCTNO.

           PERFORM START-LN1-FILE.

       NEXT-LN.
           PERFORM READ-LN1-FILE-NEXT.
           IF IO-BAD OR (LN-OWNBR NOT = LN-PATH-OWNBR)
              GO TO END-OF-GROUP.

           IF LN-OWNBR > END-BRANCH
              GO TO END-OF-GROUP.

           IF ( LN-CLASS < BEG-CLASS OR LN-CLASS > END-CLASS )
              GO TO NEXT-LN.

           IF NOT (LN-SRCD = "PB" OR "RN" OR "SC" OR "RB" OR "RO")
              GO TO NEXT-LN.

           IF ( LN-POCD-NONLOAN )
              GO TO NEXT-LN.

      * VERIFY BETWEEN BEGINNING AND ENDING DATE
           IF LN-LOANDATE < BEG-DATE OR LN-LOANDATE > END-DATE
              GO TO NEXT-LN.

           MOVE 0 TO TOT-CUR-INS-COVERAGES.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
              IF LN-INSPREM(SUB) > 0
                 ADD 1 TO TOT-CUR-INS-COVERAGES
                 MOVE "Y" TO CUR-COV(SUB)
              ELSE
                 MOVE "N" TO CUR-COV(SUB)
              END-IF
           END-PERFORM.

           MOVE LN1-KEY   TO HOLD-LN1-KEY.

      *******************************************
      * READ PRIOR LOAN
      *******************************************
           MOVE LN-PRLNNO TO LN-ACCTNO.
           PERFORM READ-LN1-FILE.
           IF IO-BAD
      *       MOVE HOLD-LN1-KEY TO LN1-KEY
      *       PERFORM START-LN1-FILE-GREATER
              GO TO NEXT-LN.

           MOVE 0 TO TOT-PRIOR-INS-COVERAGES.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
              IF LN-INSPREM(SUB) > 0
                 ADD 1 TO TOT-PRIOR-INS-COVERAGES
                 MOVE "Y" TO PRIOR-COV(SUB)
              ELSE
                 MOVE "N" TO PRIOR-COV(SUB)
              END-IF
           END-PERFORM.

           IF DROPPED-INS-FG = "Y"
              IF TOT-PRIOR-INS-COVERAGES NOT > TOT-CUR-INS-COVERAGES
      *          MOVE HOLD-LN1-KEY TO LN1-KEY
      *          PERFORM START-LN1-FILE-GREATER
                 GO TO NEXT-LN.

           IF PRU-LP NOT = "VDU" AND NOT = "PRU"
              PERFORM SEND-ACCOUNT.

      * NEED TO GET ORIGINAL LOAN BACK IN THE BUFFER, NOT THE PRIOR
           MOVE HOLD-LN1-KEY TO LN1-KEY.
           PERFORM START-LN1-FILE.
           PERFORM READ-LN1-FILE-NEXT.
           IF IO-BAD OR (LN-OWNBR NOT = LN-PATH-OWNBR)
              GO TO IO-ERROR.

           IF FIRST-TIME = "Y"
              MOVE LN-OWNBR TO HOLD-BRNO
              PERFORM SET-BRANCH-HEADINGS.

           IF LN-OWNBR NOT = HOLD-BRNO
              MOVE "Y" TO BR-TOTALS
              PERFORM BRANCH-TOTALS
              MOVE EXT-CONAME-RPT-LINES TO LN-COUNT
              MOVE LN-OWNBR TO HOLD-BRNO
              PERFORM SET-BRANCH-HEADINGS.

           PERFORM GET-SP1-RECORD.

           PERFORM DETAIL-LINE-PROCESS.
           GO TO NEXT-LN.

      *-----------------------------------------------------------------
       END-OF-GROUP.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-LN1-FILE.

           GO TO NEXT-GROUP-LOC.

      *-----------------------------------------------------------------
       END-ROUTINE.
           IF NO-RECORDS-FOUND
              GO TO END-PROGRAM.

           MOVE "Y" TO BR-TOTALS.

           PERFORM BRANCH-TOTALS.
           PERFORM GRAND-TOTALS.
           PERFORM PRINT-RANGE-LINES.

           GO TO END-PROGRAM.

      ***********************************
       DETAIL-LINE-PROCESS SECTION.
      ***********************************

           IF FIRST-TIME = "Y"
              MOVE 1 TO LN-FEED
              MOVE EXT-CONAME-RPT-LINES TO LN-COUNT
              MOVE "Y" TO RECORDS-FOUND
              MOVE "N" TO FIRST-TIME.

           MOVE LN-CLASS          TO D-CLASS.
           MOVE "-"               TO D-DASH.
           PERFORM LPSTAT-DISPLAY.
           MOVE LPS-DISPLAY-LNNO  TO W-LOAN-NUM.
           MOVE LOAN-NUM          TO D-LOAN-NUM.
           PERFORM PROCESS-BORROWER.
           MOVE LN-LOANDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY       TO D-LNDATE.
           MOVE LN-LNAMT          TO D-LNAMT.
           MOVE LN-PRLNNO         TO D-PRIOR-NO.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
              MOVE PRIOR-COV(SUB) TO D-PRIOR-COV(SUB)
              MOVE CUR-COV(SUB)   TO D-CUR-COV(SUB)
           END-PERFORM.
           MOVE LN-PROCEEDS       TO D-PROCEEDS.

           PERFORM WRITE-DETAIL-LINE.

           ADD 1 TO T-NOACCTS(1) T-NOACCTS(2).

       DETAIL-LINE-PROCESS-EXIT.
           EXIT.

      ***********************************
       INITIALIZATION SECTION.
      ***********************************
           MOVE INSAUD-QUEUE-POS TO Q-POS.
           MOVE INSAUD-COPIES-POS TO C-POS.
           MOVE INSAUD-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           MOVE SPACES TO RECORDS-FOUND.

           MOVE 0 TO T-NOACCTS(1)   T-NOACCTS(2).

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
                 MOVE EXT-CONAME-USER-LOC TO BEG-BRANCH END-BRANCH.

           MOVE " " TO LEGEND.
           PERFORM SEND-MSEL.

           IF EXT-ROUTE-BUF = "00"
              MOVE "IN PROGRESS......." TO MESS
              PERFORM SEND-LEGEND.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-SP1-FILE.

           PERFORM OPEN-PR-FILE.

           GO TO EXIT-INITIALIZE.

       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.

       EXIT-INITIALIZE.
           EXIT.

      *****************************************
       EO-EXEC-AUDIT SECTION.
      *****************************************
           MOVE EXT-EOBUF-DATE1 TO BEG-DATE.
           MOVE EXT-EOBUF-DATE2 TO END-DATE.

      *****************************************
       ENTER-PARAMETERS SECTION.
      *****************************************
       ENTER-PARM.

           IF VDUSTATUS = "1U"
              GO TO DROPPED-INS-YN.

       BR-01.
           MOVE "F7-EXIT, F1, F3-ALL, ^, ENTER BEGINNING BRANCH"
                                                     TO LEGEND.
           PERFORM SEND-MSEL.
           MOVE "R  A000BR1." TO VDU.
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
              GO TO CHECK-BRANCH-SECURITY.
       BRANCH-IS-NUMERIC.
           MOVE "N" TO GROUP-FLAG.
           MOVE WGR-SRESULT TO BEG-BRANCH.
           MOVE "SN N040BR1." TO VDU.
           MOVE BEG-BRANCH TO VDUNUM0.
           PERFORM SCREENIO.
       BR-02.
           MOVE "F7-EXIT, F1, ^-BACK, ENTER ENDING BRANCH" TO LEGEND.
           PERFORM SEND-MSEL.
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
       ALL-BR.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "S  A160DISPLAY-USER." TO VDU.
           MOVE SPACES TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "N" TO GROUP-FLAG.

       CHECK-BRANCH-SECURITY.
           PERFORM BR-RANGE-SECURITY.
           IF BR-SECURITY-OKAY = "N"
              GO TO BR-01.

       CL-01.
           MOVE "F7-EXIT, F1, F3-ALL, ^, ENTER BEGINNING CLASS"
                                                     TO LEGEND.
           PERFORM SEND-MSEL.
           MOVE "ENNN020CL1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-CL.
           IF VDUSTATUS = "1U" GO TO BR-02.
           IF VDUSTATUS = "1<"
              MOVE "BEG" TO CLASS-FG
              PERFORM CLASS-SCAN
              IF IO-FG NOT = 0
              GO TO CL-01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO CL-01.
           MOVE VDUNUM0 TO BEG-CLASS.
       CL-02.
           MOVE "F7-EXIT, F1, ^-BACK, ENTER ENDING CLASS" TO LEGEND.
           PERFORM SEND-MSEL.
           MOVE "ENNN020CL2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO CL-01.
           IF VDUSTATUS = "1<"
              MOVE "END" TO CLASS-FG
              PERFORM CLASS-SCAN
              IF IO-FG NOT = 0
              GO TO CL-02.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO CL-02.
           MOVE VDUNUM0 TO END-CLASS.
           IF BEG-CLASS > END-CLASS
              MOVE " INVALID LOAN CLASS RANGE !" TO MESS
              PERFORM SEND-MESS
              GO TO CL-01.
           GO TO BEGDATE-01.
       ALL-CL.
           MOVE "SNNN020CL1." TO VDU.
           MOVE 0 TO BEG-CLASS VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020CL2." TO VDU.
           MOVE 99 TO END-CLASS VDUNUM0.
           PERFORM SCREENIO.

      *---------------------------------------------------------------
       BEGDATE-01.
           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO DROPPED-INS-YN.

           MOVE "F7-EXIT, F1, ^-BACK, ENTER BEGINNING LOAN DATE"
             TO LEGEND.
           PERFORM SEND-MSEL.
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
              GO TO DROPPED-INS-YN.

           MOVE "F7-EXIT, F1, ^-BACK, ENTER ENDING LOAN DATE"
             TO LEGEND.
           PERFORM SEND-MSEL.
           MOVE "ERNM060DATE2." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BEGDATE-01.
           IF NOT (VDUSTATUS = "00" OR "LD") GO TO ENDDATE-02.

           MOVE VDU-DATE-YYYYMMDD TO END-DATE.

      * MAKE SURE THE DATE RANGE IS VALID.

           IF BEG-DATE NOT = 0
              IF END-DATE < BEG-DATE
                 MOVE "INVALID DATE RANGE" TO MESS
                 PERFORM SEND-MESS
                 GO TO BEGDATE-01.

           GO TO DROPPED-INS-YN.

      *----------------------------------------------------------
       ALL-DATE.

           MOVE "S  8080DATE1." TO VDU.
           MOVE EXT-JULIAN-BEG TO BEG-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
           MOVE "S  8080DATE2."   TO VDU.
           MOVE EXT-JULIAN-END TO END-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
       DROPPED-INS-YN.
           MOVE "F7-EXIT, F1, ^-BACK, ENTER Y, N :" TO LEGEND.
           PERFORM SEND-MSEL.
           MOVE "E NA010DROPPED." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ENDDATE-02.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO DROPPED-INS-YN.
           MOVE VDUBUF TO DROPPED-INS-FG.
           IF DROPPED-INS-FG NOT = "Y"
              IF DROPPED-INS-FG NOT = "N"
                 MOVE "PLEASE ENTER 'Y' TO 'N'" TO MESS
                 PERFORM SEND-MESS
                 GO TO DROPPED-INS-YN.

       END-PARM.
           MOVE "F7-EXIT, F1, ^-BACK, ACCEPT (Y/N):" TO LEGEND.
           PERFORM SEND-MSEL.

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

           MOVE "SN N020CL1." TO VDU.
           MOVE BEG-CLASS TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SN N020CL2." TO VDU.
           MOVE END-CLASS TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "S  A010DROPPED." TO VDU.
           MOVE DROPPED-INS-FG TO VDUBUF.
           PERFORM SCREENIO.

      *******************************
       PRINT-RANGE-LINES SECTION.
      *******************************
           MOVE 5 TO LN-FEED.

           IF GROUP-FLAG = "Y"
              MOVE "BRANCH GROUP" TO RANGE-LINE
              MOVE ENT-GROUP TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE
           ELSE
              MOVE "BEG BRANCH" TO RANGE-LINE
              MOVE BEG-BRANCH TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE
              MOVE "END BRANCH" TO RANGE-LINE
              MOVE END-BRANCH TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING CLASS    :" TO RANGE-LINE.
           MOVE BEG-CLASS              TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "ENDING             :" TO RANGE-LINE.
           MOVE END-CLASS              TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEGINNING LOAN DATE:" TO RANGE-LINE.
           MOVE BEG-DATE               TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY            TO VDU-DATE-EDIT.
           MOVE VDU-DATE-EDIT          TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "ENDING    LOAN DATE:" TO RANGE-LINE.
           MOVE END-DATE               TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY            TO VDU-DATE-EDIT.
           MOVE VDU-DATE-EDIT          TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "DROPPED INS ONLY   :" TO RANGE-LINE.
           MOVE DROPPED-INS-FG         TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

      **************************************
       PROCESS-BORROWER SECTION.
      **************************************
           MOVE LN-SSNO(1) TO BW-SSNO.
           PERFORM READ-BW1-FILE.
           IF IO-FG NOT = 0
              MOVE "**NOT ON FILE**" TO D-BWNAME
              GO TO END-PROCESS-BORROWER.

           MOVE BW-LNAME TO NAME-WORKER.
           MOVE BW-FNAME TO FIRST-NAME.
           INSPECT NAME-WORKER REPLACING FIRST "  " BY ",?".
           INSPECT NAME-WORKER REPLACING FIRST "                "
               BY FIRST-NAME.
           INSPECT NAME-WORKER REPLACING ALL "?" BY " ".
           MOVE NAME-WORKER TO D-BWNAME.

       END-PROCESS-BORROWER.
           EXIT.

      *******************************
       HEAD-PAGE SECTION.
      *******************************
           ADD 1 TO PAGE-NUMBER.
           MOVE PAGE-NUMBER TO H-PAGE-NO.
           PERFORM GET-TIME.
           MOVE TIME-EDIT TO H-TIME.

           MOVE HEAD1 TO PR-REC.
           MOVE 99 TO LN-FEED.
           PERFORM WRITE-PR-REC.
           MOVE HEAD2 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
           MOVE HEAD3 TO PR-REC.
           PERFORM WRITE-PR-REC.
           MOVE HEAD4 TO PR-REC.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.
           MOVE HEAD5 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE 6 TO LN-COUNT.
           MOVE 2 TO LN-FEED.
       END-HEAD-PAGE.
           EXIT.

      ****************************
       WRITE-DETAIL-LINE SECTION.
      ****************************
           ADD LN-FEED TO LN-COUNT.
           IF LN-COUNT > EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.
           MOVE DETAIL-LINE TO PR-REC
           PERFORM WRITE-PR-REC.

           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      ****************************************
       BRANCH-TOTALS SECTION.
      ****************************************
           MOVE 2 TO LN-FEED.
           MOVE "BRANCH TOTALS:" TO D-T-DESC.
           MOVE T-NOACCTS(1) TO D-T-NOACCTS.
           PERFORM WRITE-DETAIL-LINE.
           MOVE 1 TO LN-FEED.
       END-BRANCH-TOTALS.
           MOVE 0 TO T-NOACCTS(1).

      ****************************************
       GRAND-TOTALS SECTION.
      ****************************************
           IF BEG-BRANCH = END-BRANCH
              GO TO EXIT-GRAND-TOTALS.

           MOVE 2 TO LN-FEED.
           MOVE "GRAND TOTALS :" TO D-T-DESC.
           MOVE T-NOACCTS(2) TO D-T-NOACCTS.
           PERFORM WRITE-DETAIL-LINE.

       EXIT-GRAND-TOTALS.
           EXIT.

      ***************************************************************
       CLASS-SCAN SECTION.
      ***************************************************************
           MOVE 0      TO LCSCAN-KEY.
           MOVE SPACES TO LCSCAN-VERIFY.
           MOVE "WI/LCSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH LCSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF LCSCAN-STATUS NOT = "00" OR LCSCAN-KEY = 0
               MOVE 9 TO IO-FG
               GO TO EXIT-CLASS-SCAN.
           IF ( CLASS-FG = "BEG" )
              MOVE "SNNN020CL1." TO VDU
           ELSE
              MOVE "SNNN020CL2." TO VDU.
           MOVE LCSCAN-KEY TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE LCSCAN-KEY TO VDUNUM0.
           MOVE 0 TO IO-FG.
      *---------------------------------------------------------
       EXIT-CLASS-SCAN.
           EXIT.
      **********************************************************
      *
      *    C O P Y   L I B R A R Y   S E C T I O N
      *
      **********************************************************
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBLP/LPSTAT.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/BRSECURE.CPY".

      **************************************************
       FORM-RESET SECTION.
      **************************************************
           MOVE EXT-CONAME-CONAME TO INSAUD-CONAME.
           MOVE EXT-CONAME-SYSDATE TO INSAUD-SYSDATE.
           DISPLAY INSAUD-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE INSAUD-FORM-FIELDS TO WR-BUF.
           MOVE INSAUD-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.


      **************************************************
       SETUP-LINK-INFO SECTION.
      **************************************************
           MOVE INSAUD-HELP--FILE TO HELP-LINK-FILE.
           MOVE INSAUD-HELP--DIR TO HELP-LINK-DIR.

      **************************************************
       VDU-CONVERT-FIELD SECTION.
      **************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/INSAUD_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *********************************************************
       MISC-PARAGRAPHS SECTION.
      *********************************************************
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
       GET-SP1-RECORD.
           MOVE LN-ORGST    TO SP-ORGST.
           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           MOVE LN-LAWCODE  TO SP-LAWCODE.
           IF LPWSSP-SP1-KEY NOT = SP1-KEY
              MOVE SP1-KEY TO LPWSSP-SP1-KEY
              PERFORM READ-SP1-FILE
              IF IO-FG NOT = 0
                 GO TO IO-ERROR.
       SEND-ACCOUNT.
           MOVE LN-OWNBR TO DISPLAY-MESS-BR.
           MOVE LN-CLASS TO DISPLAY-MESS-CL.
           MOVE LN-ACCTNO TO DISPLAY-MESS-LN.
           MOVE DISPLAY-MESS TO MESS.
           PERFORM SEND-LEGEND.
       SET-BRANCH-HEADINGS.
           MOVE HOLD-BRNO TO H-BRNO.
           MOVE BR-NAME TO H-BRDESC.
       SEND-LEGEND.
           MOVE MESS TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       SEND-MSEL.
           MOVE LEGEND TO WR-BUF.
           MOVE MSEL-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ***********************************************************
       IO-ROUTINE SECTION.
      ***********************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      ***********************************************************
       END-PROGRAM SECTION.
      ***********************************************************
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
      *------------------------------------------------------------

           IF SV-BEG-BR NOT = 9999
              IF EXT-EOBUF-GROUP-FG = "Y"
                 MOVE SV-BEG-BR TO BEG-BRANCH
                 MOVE SV-END-BR TO END-BRANCH
                 MOVE SV-GROUP-FLAG TO GROUP-FLAG
                 MOVE SV-ENT-GROUP TO ENT-GROUP
              ELSE
                 MOVE SV-BEG-BR TO BEG-BRANCH
                 MOVE SV-END-BR TO END-BRANCH.

      *------------------------------------------------------------
           IF EXT-ROUTE-BUF = "00"
              MOVE "LP/LPR2MU" TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY INSAUD-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
