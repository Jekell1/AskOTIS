       IDENTIFICATION DIVISION.
       PROGRAM-ID.      GATXIN.
       DATE-WRITTEN.    12/10/04..
      *****************************************************************
      *                      (LP)
      *    DESCRIPTION:  GEORGIA TAX REPORTS
      *                  ALL INSURANCES
      *                  POLICIES ISSUED, PREMIUMS CHARGED, PREMIUMS
      *                     REFUNDED, NET PREMIUMS
      *                  (TAKEN FROM ICMORP)
      *
      * REV:
      *  BAH 041210 NEW FOR WORLD GA TAX REPORT FORM, WORLD PR# 364
      *   CS 050106 BRANCH TOTALS NET PREM WERE WRONG. REFUNDS ARE NEGATIVE
      *             SO WHEN NET WAS COMPUTED AS [PREM - REFUNDS] ENDED UP
      *             ADDING THEM
      *             FOR EX: PREMIUMS 1.58  REFUNDS 10.00- NET WAS 11.58
      *             COMPANY TOTALS WERE COMPUTING NET AS ISSUES - REFUNDS
      *  BAH 170602 ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 180119 REMOVED LPBAGE, NOT USED, PL#986
      * BAH 181207 GLOBAL LNFILE
      * BAH 20190919 REMOVED ACCESS-CALL ON LNFILE, TRP, TRL, TRS
      * BAH 20200827 REMOVED RESET ON BR1 
      * BLM 20210810 REMOVE TP-CODE, TP-POOLID NO LONGER IN KEY
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
       COPY "LIBGB/GBFSWKI.CPY".

       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.

       COPY "LIBGB/GBFDPR.CPY".

       FD  WK-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK-REC.
           03  WK-KEY.
               05  WORK-OWNBR       PIC 9(4).

           03  WORK-ISS-CL      PIC S9(7)    COMP-3.
           03  WORK-NEW-CL      PIC S9(5)V99 COMP-3.
           03  WORK-CAN-CL      PIC S9(5)V99 COMP-3.
           03  WORK-ISS-AH      PIC S9(7)    COMP-3.
           03  WORK-NEW-AH      PIC S9(5)V99 COMP-3.
           03  WORK-CAN-AH      PIC S9(5)V99 COMP-3.
           03  WORK-ISS-PP      PIC S9(7)    COMP-3.
           03  WORK-NEW-PP      PIC S9(5)V99 COMP-3.
           03  WORK-CAN-PP      PIC S9(5)V99 COMP-3.
           03  WORK-ISS-O1      PIC S9(7)    COMP-3.
           03  WORK-NEW-O1      PIC S9(5)V99 COMP-3.
           03  WORK-CAN-O1      PIC S9(5)V99 COMP-3.
           03  WORK-ISS-O2      PIC S9(7)    COMP-3.
           03  WORK-NEW-O2      PIC S9(5)V99 COMP-3.
           03  WORK-CAN-O2      PIC S9(5)V99 COMP-3.
           03  WORK-ISS-O3      PIC S9(7)    COMP-3.
           03  WORK-NEW-O3      PIC S9(5)V99 COMP-3.
           03  WORK-CAN-O3      PIC S9(5)V99 COMP-3.
           03  WORK-ISS-NF      PIC S9(7)    COMP-3.
           03  WORK-NEW-NF      PIC S9(5)V99 COMP-3.
           03  WORK-CAN-NF      PIC S9(5)V99 COMP-3.
      ******************************************************************
      *
      *     W O R K I N G - S T O R A G E    S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(43) VALUE
           "@(#)./LP/GATXIN.CBL S34 08/11/21-11:41:08 >".

       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01TRL.CPY".
       COPY "LIBLP/LP01TRL_SQL.CPY".
       COPY "LIBLP/LPWSTRL.CPY".
       COPY "LIBLP/LP01TRP.CPY".
       COPY "LIBLP/LP01TRP_SQL.CPY".
       COPY "LIBLP/LPWSTRP.CPY".
       COPY "LIBLP/LP01TRS.CPY".
       COPY "LIBLP/LP01TRS_SQL.CPY".
       COPY "LIBLP/LPWSTRS.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
      *-----------------------------------------------------------------
       01  WK-IFN               PIC X(2)     VALUE "WK".


      ******************************************************************
      *   LINE WORKERS
      ******************************************************************
       01  LN-WORKERS.
           03  LN-FEED          PIC 999      VALUE 0.
           03  LN-COUNT         PIC 999      VALUE 99.
           03  LN-WK            PIC 999      VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

      ******************************************************************
      *   MISC WORKERS
      ******************************************************************
       01  SUB                  PIC 9(2)     COMP-3.
      *    --------------------------------------------------------
       01  FIRST-TIME           PIC X        VALUE "Y".
       01  RECORDS-FOUND        PIC X        VALUE " ".
           88  NO-RECORDS-FOUND              VALUE "N".
       01  MESS-LIST            PIC X(5)     VALUE "MSEL.".
       01  TOT-PREM             PIC S9(9)V99 COMP-3 VALUE 0.
       01  TOT-REF              PIC S9(9)V99 COMP-3 VALUE 0.
       01  DASH-LINE            PIC X(80)   VALUE ALL "-".
       01  HOLD-NON-FILE        PIC S9(5)V99 COMP-3 VALUE 0.

       01  RPT-END-DATE         PIC 9(8)     COMP-3 VALUE 0.
       01  BEGINNING-DATE       PIC 9(8)     COMP-3 VALUE 0.

      * COMPANY TOTALS
       01  LIFE-ISS             PIC S9(7)    COMP-3.
       01  LIFE-PREM            PIC S9(7)V99 COMP-3.
       01  LIFE-CAN             PIC S9(7)V99 COMP-3.
       01  AH-ISS               PIC S9(7)    COMP-3.
       01  AH-PREM              PIC S9(7)V99 COMP-3.
       01  AH-CAN               PIC S9(7)V99 COMP-3.
       01  PP-ISS               PIC S9(7)    COMP-3.
       01  PP-PREM              PIC S9(7)V99 COMP-3.
       01  PP-CAN               PIC S9(7)V99 COMP-3.
       01  O1-ISS               PIC S9(7)    COMP-3.
       01  O1-PREM              PIC S9(7)V99 COMP-3.
       01  O1-CAN               PIC S9(7)V99 COMP-3.
       01  O2-ISS               PIC S9(7)    COMP-3.
       01  O2-PREM              PIC S9(7)V99 COMP-3.
       01  O2-CAN               PIC S9(7)V99 COMP-3.
       01  O3-ISS               PIC S9(7)    COMP-3.
       01  O3-PREM              PIC S9(7)V99 COMP-3.
       01  O3-CAN               PIC S9(7)V99 COMP-3.
       01  NF-ISS               PIC S9(7)    COMP-3.
       01  NF-PREM              PIC S9(7)V99 COMP-3.
       01  NF-CAN               PIC S9(7)V99 COMP-3.


       01  CLASS-FG             PIC X(3)    VALUE "   ".
           88  CLASS-BEG                    VALUE "BEG".
           88  CLASS-END                    VALUE "END".
      ******************************************************************
      *     S C R E E N   F I E L D S
      ******************************************************************
      *-----------------------------------------------------------------
       COPY "LIBGB/EOBUF_EXT.CPY".
      *-----------------------------------------------------------------
           03  BEG-BRANCH      PIC 9(4).
           03  END-BRANCH      PIC 9(4).
           03  BEG-IC          PIC 99.
           03  END-IC          PIC 99.
           03  BEG-CLASS          PIC 9(3).
           03  END-CLASS          PIC 9(3).
           03  BEG-ST          PIC XX.
           03  END-ST          PIC XX.
           03  BEG-DATE.
               05  BEG-MONTH   PIC 9(2).
               05  BEG-YEAR    PIC 9(2).
           03  BDATE REDEFINES BEG-DATE PIC 9(4).
           03  END-DATE.
               05  END-MONTH   PIC 9(2).
               05  END-YEAR    PIC 9(2).
           03  EDATE REDEFINES END-DATE PIC 9(4).
           03  FEE-CODE        PIC XX.
           03  GROUP-FLAG      PIC X.
           03  ENT-GROUP       PIC X(4).

       01  VERSION-CONTROL     PIC X      VALUE "A".

       01  SV-BEG-BR           PIC 9999   VALUE 9999.
       01  SV-END-BR           PIC 9999   VALUE 9999.
       01  SV-GROUP-FLAG       PIC X     VALUE SPACES.
       01  SV-ENT-GROUP        PIC X(4)  VALUE SPACES.


      ******************************************************************
      *     P R I N T   B U F F E R S
      ******************************************************************
       01  HEAD1.
           03  H-DATE           PIC X(8)     VALUE " ".
           03  FILLER           PIC XX       VALUE " ".
           03  H-HR             PIC 99       VALUE 0.
           03  FILLER           PIC X        VALUE ":".
           03  H-MIN            PIC 99       VALUE 0.
           03  FILLER           PIC X(5)    VALUE " ".

           03  H-NAME           PIC X(40)    VALUE " ".

           03  FILLER           PIC X(11)    VALUE " ".
           03  FILLER           PIC X(5)     VALUE "PAGE:".
           03  H-PAGE-NO        PIC ZZZ9.

      *-----------------------------------------------------------------
       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(3)    VALUE " ".
           03  FILLER           PIC X(40) VALUE
           "---  GEORGIA LOAN TAX REPORT  ---".

      *-----------------------------------------------------------------
       01  HEAD3.
           03  FILLER           PIC X(29)  VALUE " ".
           03  H-DATE1          PIC 99/99/99.
           03  FILLER           PIC X(6) VALUE " THRU ".
           03  H-DATE2          PIC 99/99/99.

       01  HEAD4.
           03  FILLER           PIC X(10) VALUE "BRANCH :".
           03  H-BRNO           PIC 9(4).
           03  FILLER           PIC X(3) VALUE SPACES.
           03  H-BRNAME         PIC X(20) VALUE " ".

       01  HEAD-COMPANY.
           03  H-PAGE-DESC      PIC X(40) VALUE SPACES.
      *-----------------------------------------------------------------
       01  HEAD5.
           03  FILLER           PIC X(16) VALUE SPACES.
           03  FILLER           PIC X(52) VALUE
           "POLICIES       PREMIUMS".
           03  FILLER           PIC X(10) VALUE "NET".

       01  HEAD6.
           03  FILLER           PIC X(18) VALUE SPACES.
           03  FILLER           PIC X(45) VALUE
           "ISSUED        WRITTEN         REFUNDS".
           03  FILLER           PIC X(10) VALUE "PREMIUMS".


      ******************************************************************
      *    D E T A I L - L I N E
      ******************************************************************
       01  DETAIL-LINE          PIC X(80) VALUE SPACES.

      *-----------------------------------------------------------------
       01  DETAIL1 REDEFINES DETAIL-LINE.
           03  D-TAG            PIC X(15).
           03  D-QTY            PIC Z,ZZZ,ZZ9-.
           03  FILLER           PIC X(2).
           03  D-PREM           PIC Z,ZZZ,ZZ9.99-.
           03  FILLER           PIC X(3).
           03  D-REF            PIC Z,ZZZ,ZZ9.99-.
           03  FILLER           PIC X(3).
           03  D-NET            PIC Z,ZZZ,ZZ9.99-.


      *-----------------------------------------------------------------
       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(20).
           03  RANGE-SEL        PIC X(112).
      ******************************************************************
      *
      *      C O P Y    L I B R A R Y    W O R K E R S
      *
      ******************************************************************
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBWI/LCSCANW.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/GATXIN_DEF.CPY".
       COPY "LIBLP/GATXIN_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *****************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      *****************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBLP/GATXIN_SCN.CPY".

      *----------------------------------------------------------------
       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE WK-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-TRS1-FILE.
           PERFORM CLOSE-TRP1-FILE.
           PERFORM CLOSE-TRL1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE WK-FILE.
       COPY "LIBGB/DECLRP2.CPY".


      ******************************************************************
      *
      *     M A I N    M O D U L E
      *
      ******************************************************************
       MAIN-MODULE SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "GATXIN" TO VDU-FORM-NAME.

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
              GO TO PROCESS-REPORT.

           MOVE WGR-SRESULT TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           GO TO MOVE-BRANCH-PATH-INFO.

      *-----------------------------------------------------------------
       NEXT-BRANCH-NO.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-FG = 9 OR BR-NO > END-BRANCH
              GO TO PROCESS-REPORT.

      *-----------------------------------------------------------------
       MOVE-BRANCH-PATH-INFO.
           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO LN-PATH-OVERRIDE
                                              TRP-PATH-OVERRIDE
                                              TRL-PATH-OVERRIDE
                                              TRS-PATH-OVERRIDE.

           PERFORM OPEN-LN1-FILE.

           PERFORM SORT-TRP-FILE.
           PERFORM SORT-TRL-FILE.
           PERFORM SORT-TRS-FILE.

      *-----------------------------------------------------------------
       END-OF-GROUP.
           PERFORM CLOSE-LN1-FILE.
           GO TO NEXT-GROUP-LOC.

      *-----------------------------------------------------------------
       PROCESS-REPORT.
           PERFORM CLOSE-WK-FILE.
           PERFORM OPEN-WK-FILE.

           PERFORM RUN-REPORT.

      *-----------------------------------------------------------------
       END-ROUTINE.
           IF FIRST-TIME = "Y"
              MOVE "N" TO RECORDS-FOUND
           ELSE
              PERFORM COMPANY-TOTALS
              PERFORM RANGE-LINES.

           GO TO END-PROGRAM.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************
           MOVE GATXIN-QUEUE-POS TO Q-POS.
           MOVE GATXIN-COPIES-POS TO C-POS.
           MOVE GATXIN-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           IF STAT NOT = "00"
              MOVE "CANNOT MAKE WORK FILE" TO MESS
              PERFORM SEND-MESS
              GO TO INITIALIZE-ENDIT.

           PERFORM OPEN-WK-FILE-OUTPUT.
           PERFORM CLOSE-WK-FILE.
           PERFORM OPEN-WK-FILE.

           IF EXT-ROUTE-BUF = "00"
              MOVE "INSURANCE REPORT IN PROGRESS......." TO MESS
              PERFORM SEND-LEGEND.

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

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-PR-FILE.

           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.
           MOVE BEG-MONTH   TO DATE-MMDDYY-MM.
           MOVE BEG-YEAR    TO DATE-MMDDYY-YY.
           MOVE 1           TO DATE-MMDDYY-DD.
           MOVE DATE-MMDDYY TO H-DATE1.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO BEGINNING-DATE.

           MOVE END-MONTH TO DATE-MMDDYY-MM.
           MOVE END-YEAR  TO DATE-MMDDYY-YY.
           MOVE 50        TO DATE-MMDDYY-DD.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO NDTE-DATE.
           PERFORM NEW-DATE-CALCULATION.
           MOVE NDTE-DATE TO RPT-END-DATE DATE-YYYYMMDD.

           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO H-DATE2.

           GO TO INITIALIZE-EXIT.

       INITIALIZE-ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.

       INITIALIZE-EXIT.
           EXIT.

      ******************************************************************
       EO-EXEC-AUDIT SECTION.
      ******************************************************************
           MOVE EXT-EOBUF-DATE1 TO NUM-DATE.
           MOVE NUM-MO TO BEG-MONTH.
           MOVE NUM-YR TO BEG-YEAR.
           MOVE EXT-EOBUF-DATE2 TO NUM-DATE.
           MOVE NUM-MO TO END-MONTH.
           MOVE NUM-YR TO END-YEAR.

      ******************************************************************
       ENTER-PARAMETERS SECTION.
      ******************************************************************

      * FOR RE-ENTRY ON F4 FROM ACCEPT.

           IF VDUSTATUS = "1U" GO TO ENTER-FEE-CODE.

      *-----------------------------------------------------------------
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
              GO TO CHECK-BRANCH-SECURITY.

      *-----------------------------------------------------------------
       BRANCH-IS-NUMERIC.
           MOVE "N" TO GROUP-FLAG.
           MOVE WGR-SRESULT TO BEG-BRANCH.
           MOVE "SN N040BR1." TO VDU.
           MOVE BEG-BRANCH TO VDUNUM0.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       BR-02.
           MOVE "ENNN040BR2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-BR.
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BR-02.
           MOVE VDUNUM0 TO END-BRANCH.
           IF BEG-BRANCH > END-BRANCH
              PERFORM INVALID-RANGE
              GO TO BR-01.

           GO TO CHECK-BRANCH-SECURITY.

      *-----------------------------------------------------------------
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

      *-----------------------------------------------------------------
       IC-01.
           IF ( (BEG-BRANCH = 0) AND (END-BRANCH = 0) )
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE "USER BRANCH ONLY" TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO.

           MOVE "ENNN020IC1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-IC.
           IF VDUSTATUS = "1U" GO TO BR-02.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO IC-01.
           MOVE VDUNUM0 TO BEG-IC.

      *-----------------------------------------------------------------
       IC-02.
           MOVE "ENNN020IC2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-IC.
           IF VDUSTATUS = "1U" GO TO IC-01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO IC-02.
           MOVE VDUNUM0 TO END-IC.
           IF BEG-IC NOT > END-IC GO TO LC-01.
           PERFORM INVALID-RANGE.
           GO TO IC-01.

      *-----------------------------------------------------------------
       ALL-IC.
           MOVE "SNNN020IC1." TO VDU.
           MOVE 1 TO BEG-IC VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020IC2." TO VDU.
           MOVE 99 TO END-IC VDUNUM0.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       LC-01.
           MOVE "ENNN020LC1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-LC.
           IF VDUSTATUS = "1U" GO TO IC-02.
           IF VDUSTATUS = "1<"
              MOVE "BEG" TO CLASS-FG
              PERFORM CLASS-SCAN
              IF IO-FG NOT = 0
              GO TO LC-01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO IC-02.
           MOVE VDUNUM0 TO BEG-CLASS.

      *-----------------------------------------------------------------
       LC-02.
           MOVE "ENNN020LC2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-LC.
           IF VDUSTATUS = "1U" GO TO LC-01.
           IF VDUSTATUS = "1<"
              MOVE "END" TO CLASS-FG
              PERFORM CLASS-SCAN
              IF IO-FG NOT = 0
              GO TO LC-02.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO LC-02.
           MOVE VDUNUM0 TO END-CLASS.
           IF BEG-CLASS NOT > END-CLASS GO TO ST-01.
           PERFORM INVALID-RANGE.
           GO TO LC-01.

      *-----------------------------------------------------------------
       ALL-LC.
           MOVE "SNNN020LC1." TO VDU.
           MOVE 1 TO BEG-CLASS VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020LC2." TO VDU.
           MOVE 99 TO END-CLASS VDUNUM0.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       ST-01.
           MOVE "E  A020ST1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-ST.
           IF VDUSTATUS = "1U" GO TO LC-02.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ST-01.
           MOVE VDUBUF TO BEG-ST.

      *-----------------------------------------------------------------
       ST-02.
           MOVE "E  A020ST2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-ST.
           IF VDUSTATUS = "1U" GO TO ST-01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ST-02.
           MOVE VDUBUF TO END-ST.
           IF BEG-ST > END-ST
              PERFORM INVALID-RANGE
              GO TO ST-01.

           GO TO ENTER-BEG-DATE.

      *-----------------------------------------------------------------
       ALL-ST.
           MOVE "S  A020ST1." TO VDU.
           MOVE "  " TO BEG-ST VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A020ST2." TO VDU.
           MOVE ALL "Z" TO END-ST.
           CALL "C$TOLOWER" USING END-ST, VALUE 2.
           MOVE END-ST TO VDUBUF.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------

       ENTER-BEG-DATE.
           IF EXT-ROUTE-BUF NOT = "00"
              GO TO ENTER-FEE-CODE.

           MOVE "ENNN040DT1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ST-02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO ENTER-BEG-DATE.
           MOVE VDUNUM0 TO BDATE.

           IF BEG-MONTH < 1 OR > 12
              MOVE "INVALID MONTH" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-DATE.

       ENTER-END-DATE.
           IF EXT-ROUTE-BUF NOT = "00"
              GO TO ENTER-FEE-CODE.

           MOVE "ENNN040DT2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ENTER-BEG-DATE.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO ENTER-END-DATE.
           MOVE VDUNUM0 TO EDATE.

           IF END-MONTH < 1 OR > 12
              MOVE "INVALID MONTH" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-END-DATE.

           MOVE BEG-MONTH TO DATE-MMDDYY-MM.
           MOVE 1         TO DATE-MMDDYY-DD.
           MOVE BEG-YEAR  TO DATE-MMDDYY-YY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO NUM-DATE.

           MOVE END-MONTH TO DATE-MMDDYY-MM.
           MOVE 1         TO DATE-MMDDYY-DD.
           MOVE END-YEAR  TO DATE-MMDDYY-YY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO SYS-DATE.

           IF SYS-DATE < NUM-DATE
              MOVE "INVALID RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-END-DATE.

       ENTER-FEE-CODE.
           MOVE "E  A020FEE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ENTER-END-DATE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ENTER-FEE-CODE.
           MOVE VDUBUF TO FEE-CODE.

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
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN020IC1." TO VDU.
           MOVE BEG-IC TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN020IC2." TO VDU.
           MOVE END-IC TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN020LC1." TO VDU.
           MOVE BEG-CLASS TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN020LC2." TO VDU.
           MOVE END-CLASS TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A020ST1." TO VDU.
           MOVE BEG-ST TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A020ST2." TO VDU.
           MOVE END-ST TO VDUBUF.
           PERFORM SCREENIO.

      *********************
       RUN-REPORT SECTION.
      *********************
           MOVE 0 TO LIFE-ISS LIFE-PREM LIFE-CAN AH-PREM AH-CAN
                     AH-ISS PP-ISS O1-ISS O2-ISS O3-ISS NF-ISS
                     PP-PREM PP-CAN O1-PREM O1-CAN O2-PREM O2-CAN
                     O3-PREM O3-CAN NF-PREM NF-CAN.

           MOVE 0 TO WORK-OWNBR.
           PERFORM START-WK-FILE.
       KEY-FILE-LOOP.
           PERFORM READ-WK-FILE-NEXT.
           IF IO-FG = 9
              GO TO END-RUN-REPORT.

           IF WORK-NEW-CL = 0 AND
               WORK-CAN-CL = 0 AND
                WORK-NEW-AH = 0 AND
                 WORK-CAN-AH = 0 AND
                  WORK-NEW-PP = 0 AND
                   WORK-CAN-PP = 0 AND
                    WORK-NEW-O1 = 0 AND
                     WORK-CAN-O1 = 0 AND
                      WORK-NEW-O2 = 0 AND
                       WORK-CAN-O2 = 0 AND
                        WORK-NEW-O3 = 0 AND
                         WORK-CAN-O3 = 0 AND
                          WORK-NEW-NF = 0 AND
                           WORK-CAN-NF = 0
               GO TO KEY-FILE-LOOP.

           ADD WORK-ISS-CL TO LIFE-ISS.
           ADD WORK-NEW-CL TO LIFE-PREM.
           ADD WORK-CAN-CL TO LIFE-CAN.
           ADD WORK-ISS-AH TO AH-ISS.
           ADD WORK-NEW-AH TO AH-PREM.
           ADD WORK-CAN-AH TO AH-CAN.
           ADD WORK-ISS-PP TO PP-ISS.
           ADD WORK-NEW-PP TO PP-PREM.
           ADD WORK-CAN-PP TO PP-CAN.
           ADD WORK-ISS-O1 TO O1-ISS.
           ADD WORK-NEW-O1 TO O1-PREM.
           ADD WORK-CAN-O1 TO O1-CAN.
           ADD WORK-ISS-O2 TO O2-ISS.
           ADD WORK-NEW-O2 TO O2-PREM.
           ADD WORK-CAN-O2 TO O2-CAN.
           ADD WORK-ISS-O3 TO O3-ISS.
           ADD WORK-NEW-O3 TO O3-PREM.
           ADD WORK-CAN-O3 TO O3-CAN.
           ADD WORK-ISS-NF TO NF-ISS.
           ADD WORK-NEW-NF TO NF-PREM.
           ADD WORK-CAN-NF TO NF-CAN.

           PERFORM BR-TOTALS.


           GO TO KEY-FILE-LOOP.

       END-RUN-REPORT.
           EXIT.

      ******************************************************************
       SORT-TRP-FILE SECTION.
      ******************************************************************
           PERFORM OPEN-TRP1-FILE.

           MOVE BR-NO               TO TP-BRNO
                                       QTRP1-WBEG-BRNO
                                       QTRP1-WEND-BRNO.
           MOVE BEGINNING-DATE      TO TP-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRP1-WBEG-DATE.
           MOVE RPT-END-DATE        TO  SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRP1-WEND-DATE.

           MOVE BEG-CLASS           TO TP-CLASS
                                       QTRP1-WBEG-CLASS.
           MOVE END-CLASS           TO QTRP1-WEND-CLASS.
           MOVE 0                   TO TP-ACCTNO TP-LINE
                                       QTRP1-WBEG-ACCTNO
                                       QTRP1-WBEG-LINE.
           MOVE ALL "9"             TO QTRP1-WEND-ACCTNO
                                       QTRP1-WEND-LINE.

           MOVE TP-DATE              TO NDTE-DATE.
           MOVE 1                    TO NDTE-HOLD.
           PERFORM INCREMENT-DAYS.
           MOVE NDTE-DATE            TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
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

           PERFORM START-TRP1-FILE.

       SORT-TRP-NEXT.
           PERFORM READ-TRP1-FILE-NEXT.
           IF (IO-FG NOT = 0) OR (TRP-PATH-OWNBR NOT = TP-BRNO)
              GO TO SORT-TRP-EXIT.

           IF TP-DATE > RPT-END-DATE
              GO TO SORT-TRP-EXIT.

           IF TP-CLASS < BEG-CLASS OR TP-CLASS > END-CLASS
              GO TO SORT-TRP-NEXT.

           IF NOT (TP-ADDON-TYPE = "CL" OR "AH" OR "PP" OR
                                   "O1" OR "O2" OR "O3")
              IF NOT (TP-TRCD = "RC" OR "RA" OR "RP" OR "R1"
                             OR "R2" OR "R3")
                 GO TO SORT-TRP-NEXT.

           IF TP-INSCOMP < BEG-IC OR TP-INSCOMP > END-IC
              GO TO SORT-TRP-NEXT
           ELSE
              IF TP-INSOURS NOT = "Y"
                 GO TO SORT-TRP-NEXT.

           MOVE TP-BRNO   TO LN-OWNBR.
           MOVE TP-ACCTNO TO LN-ACCTNO.
           PERFORM READ-LN1-FILE.
           IF IO-FG NOT = 0
              GO TO SORT-TRP-NEXT.

           IF LN-ORGST < BEG-ST OR LN-ORGST > END-ST
              GO TO SORT-TRP-NEXT.


      * ONLY REPORT FOR THE OWNING BRANCH ("G")
      *    IF TP-CODE = "P"
              IF TP-OTHBR-FG = "C"
                 GO TO SORT-TRP-NEXT
      * THE "P" TRANSACTIONS ARE ONLY ADDONS AND REBATES/CANCELS
      * NEED TO SUBTRACT OUT THE INSURANCE TAX, BUT CHECK FOR
      * SPACES HERE SO WE CAN JUST SUBTRACT IT FURTHER DOWN
              ELSE
                 IF TP-INS-TAX-RB = 2020202.02
                    MOVE 0 TO TP-INS-TAX-RB.

           MOVE TP-BRNO TO WORK-OWNBR.
           PERFORM READ-WK-FILE.
           IF IO-FG NOT = 0
              PERFORM CLEAR-WORK-FILE.


           MOVE TP-BRNO TO WORK-OWNBR.

           IF TP-ADDON-TYPE = "CL" AND TP-ADDON-TEXT = "ADD"
              COMPUTE WORK-NEW-CL = WORK-NEW-CL + TP-APCUR
           ELSE
           IF TP-ADDON-TYPE = "AH" AND TP-ADDON-TEXT = "ADD"
              COMPUTE WORK-NEW-AH = WORK-NEW-AH + TP-APCUR
           ELSE
           IF TP-ADDON-TYPE = "PP" AND TP-ADDON-TEXT = "ADD"
              COMPUTE WORK-NEW-PP = WORK-NEW-PP + TP-APCUR
           ELSE
           IF TP-ADDON-TYPE = "O1" AND TP-ADDON-TEXT = "ADD"
              COMPUTE WORK-NEW-O1 = WORK-NEW-O1 + TP-APCUR
           ELSE
           IF TP-ADDON-TYPE = "O2" AND TP-ADDON-TEXT = "ADD"
              COMPUTE WORK-NEW-O2 = WORK-NEW-O2 + TP-APCUR
           ELSE
           IF TP-ADDON-TYPE = "O3" AND TP-ADDON-TEXT = "ADD"
              COMPUTE WORK-NEW-O3 = WORK-NEW-O3 + TP-APCUR
           ELSE
           IF TP-ADDON-TYPE = "CL" AND TP-ADDON-TEXT = "CNL"
              COMPUTE WORK-CAN-CL = WORK-CAN-CL + TP-APCUR
           ELSE
           IF TP-ADDON-TYPE = "AH" AND TP-ADDON-TEXT = "CNL"
              COMPUTE WORK-CAN-AH = WORK-CAN-AH + TP-APCUR
           ELSE
           IF TP-ADDON-TYPE = "PP" AND TP-ADDON-TEXT = "CNL"
              COMPUTE WORK-CAN-PP = WORK-CAN-PP + TP-APCUR
           ELSE
           IF TP-ADDON-TYPE = "O1" AND TP-ADDON-TEXT = "CNL"
              COMPUTE WORK-CAN-O1 = WORK-CAN-O1 + TP-APCUR
           ELSE
           IF TP-ADDON-TYPE = "O2" AND TP-ADDON-TEXT = "CNL"
              COMPUTE WORK-CAN-O2 = WORK-CAN-O2 + TP-APCUR
           ELSE
           IF TP-ADDON-TYPE = "O3" AND TP-ADDON-TEXT = "CNL"
              COMPUTE WORK-CAN-O3 = WORK-CAN-O3 + TP-APCUR
           ELSE
           IF TP-TRCD = "RC"
              COMPUTE WORK-CAN-CL = WORK-CAN-CL +
                           ((TP-TRAMT - TP-INS-TAX-RB) * -1)
           ELSE
           IF TP-TRCD = "RA"
              COMPUTE WORK-CAN-AH = WORK-CAN-AH +
                                ((TP-TRAMT - TP-INS-TAX-RB) * -1)
           ELSE
           IF TP-TRCD = "RP"
              COMPUTE WORK-CAN-PP = WORK-CAN-PP +
                                ((TP-TRAMT - TP-INS-TAX-RB) * -1)
           ELSE
           IF TP-TRCD = "R1"
              COMPUTE WORK-CAN-O1 = WORK-CAN-O1 +
                                ((TP-TRAMT - TP-INS-TAX-RB) * -1)
           ELSE
           IF TP-TRCD = "R2"
              COMPUTE WORK-CAN-O2 = WORK-CAN-O2 +
                                ((TP-TRAMT - TP-INS-TAX-RB) * -1)
           ELSE
           IF TP-TRCD = "R3"
              COMPUTE WORK-CAN-O3 = WORK-CAN-O3 +
                               ((TP-TRAMT - TP-INS-TAX-RB) * -1).

           IF IO-FG = 0
              PERFORM REWRITE-WK-FILE
           ELSE
              PERFORM WRITE-WK-FILE.

           GO TO SORT-TRP-NEXT.

       SORT-TRP-EXIT.
           EXIT.

      ******************************************************************
       SORT-TRL-FILE SECTION.
      ******************************************************************
           PERFORM OPEN-TRL1-FILE.

           MOVE BR-NO               TO TL-BRNO
                                       QTRL1-WBEG-BRNO
                                       QTRL1-WEND-BRNO.
           MOVE BEGINNING-DATE      TO TL-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRL1-WBEG-DATE.
           MOVE RPT-END-DATE        TO  SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRL1-WEND-DATE.
           MOVE BEG-CLASS           TO TL-CLASS
                                       QTRL1-WBEG-CLASS.
           MOVE END-CLASS           TO QTRL1-WEND-CLASS.
           MOVE 0                   TO TL-ACCTNO TL-LAWCODE TL-LINE
                                       QTRL1-WBEG-ACCTNO
                                       QTRL1-WBEG-LAWCODE
                                       QTRL1-WBEG-LINE.
           MOVE ALL "9"             TO QTRL1-WEND-ACCTNO
                                       QTRL1-WEND-LAWCODE
                                       QTRL1-WEND-LINE.

           PERFORM START-TRL1-FILE.

       SORT-TRL-NEXT.
           PERFORM READ-TRL1-FILE-NEXT.
           IF (IO-FG NOT = 0) OR (TRL-PATH-OWNBR NOT = TL-BRNO)
              GO TO SORT-TRL-EXIT.

           IF TL-DATE > RPT-END-DATE
              GO TO SORT-TRL-EXIT.

           IF TL-CLASS < BEG-CLASS OR TL-CLASS > END-CLASS
              GO TO SORT-TRL-NEXT.

           IF TL-INSCOMP(1) = 0 AND TL-INSCOMP(2) = 0 AND
               TL-INSCOMP(3) = 0 AND TL-INSCOMP(4) = 0 AND
                TL-INSCOMP(5) = 0 AND TL-INSCOMP(6) = 0
              GO TO SORT-TRL-NEXT.

           PERFORM FIND-TL-FEE.

           IF TL-INSCOMP(1) < BEG-IC OR TL-INSCOMP(1) > END-IC
            IF TL-INSCOMP(2) < BEG-IC OR TL-INSCOMP(2) > END-IC
             IF TL-INSCOMP(3) < BEG-IC OR TL-INSCOMP(3) > END-IC
              IF TL-INSCOMP(4) < BEG-IC OR TL-INSCOMP(4) > END-IC
               IF TL-INSCOMP(5) < BEG-IC OR TL-INSCOMP(5) > END-IC
                IF TS-INSCOMP(6) < BEG-IC OR TS-INSCOMP(6) > END-IC
                 IF HOLD-NON-FILE = 0
                    GO TO SORT-TRL-NEXT.


           IF TL-INSOURS(1) NOT = "Y" AND
               TL-INSOURS(2) NOT = "Y" AND
                TL-INSOURS(3) NOT = "Y" AND
                 TL-INSOURS(4) NOT = "Y" AND
                  TL-INSOURS(5) NOT = "Y" AND
                   TL-INSOURS(6) NOT = "Y"
                     IF HOLD-NON-FILE = 0
                        GO TO SORT-TRL-NEXT.


           MOVE TL-BRNO   TO LN-OWNBR.
           MOVE TL-ACCTNO TO LN-ACCTNO.
           PERFORM READ-LN1-FILE.
           IF IO-FG NOT = 0
              GO TO SORT-TRL-NEXT.

           IF LN-ORGST < BEG-ST OR LN-ORGST > END-ST
              GO TO SORT-TRL-NEXT.

           MOVE TL-BRNO TO WORK-OWNBR.
           PERFORM READ-WK-FILE.
           IF IO-FG NOT = 0
               PERFORM CLEAR-WORK-FILE.

      * JUST USING THESE TO DETERMINE IF A NEW LOAN OR VOID FOR
      * ADDING TO THE ISSUE COUNTER AT THE END
           MOVE 0 TO LIFE-PREM AH-PREM PP-PREM O1-PREM O2-PREM O3-PREM
                      NF-PREM.

           MOVE TL-BRNO TO WORK-OWNBR.

           IF (TL-INSOURS(1) = "Y" ) AND
                 (TL-INSCOMP(1) NOT < BEG-IC AND NOT > END-IC)
              COMPUTE WORK-NEW-CL = WORK-NEW-CL + TL-INSPREM(1)
              MOVE TL-INSPREM(1) TO LIFE-PREM
           END-IF
           IF (TL-INSOURS(2) = "Y" ) AND
                 (TL-INSCOMP(2) NOT < BEG-IC AND NOT > END-IC)
              COMPUTE WORK-NEW-AH = WORK-NEW-AH + TL-INSPREM(2)
              MOVE TL-INSPREM(2) TO AH-PREM
           END-IF
           IF (TL-INSOURS(3) = "Y" ) AND
                 (TL-INSCOMP(3) NOT < BEG-IC AND NOT > END-IC)
              COMPUTE WORK-NEW-PP = WORK-NEW-PP + TL-INSPREM(3)
              MOVE TL-INSPREM(3) TO PP-PREM
           END-IF
           IF (TL-INSOURS(4) = "Y" ) AND
                 (TL-INSCOMP(4) NOT < BEG-IC AND NOT > END-IC)
              COMPUTE WORK-NEW-O1 = WORK-NEW-O1 + TL-INSPREM(4)
              MOVE TL-INSPREM(4) TO O1-PREM
           END-IF
           IF (TL-INSOURS(5) = "Y" ) AND
                 (TL-INSCOMP(5) NOT < BEG-IC AND NOT > END-IC)
              COMPUTE WORK-NEW-O2 = WORK-NEW-O2 + TL-INSPREM(5)
              MOVE TL-INSPREM(5) TO O2-PREM
           END-IF
           IF (TL-INSOURS(6) = "Y" ) AND
                 (TL-INSCOMP(6) NOT < BEG-IC AND NOT > END-IC)
              MOVE TL-INSPREM(6) TO O3-PREM
              COMPUTE WORK-NEW-O3 = WORK-NEW-O3 + TL-INSPREM(6)
           END-IF
           IF HOLD-NON-FILE NOT = 0
             MOVE HOLD-NON-FILE TO NF-PREM
             COMPUTE WORK-NEW-NF = WORK-NEW-NF + HOLD-NON-FILE.

           IF LIFE-PREM > 0
              ADD 1 TO WORK-ISS-CL
           ELSE
              IF LIFE-PREM < 0
                 SUBTRACT 1 FROM WORK-ISS-CL.
           IF AH-PREM > 0
              ADD 1 TO WORK-ISS-AH
           ELSE
              IF AH-PREM < 0
                 SUBTRACT 1 FROM WORK-ISS-AH.
           IF PP-PREM > 0
              ADD 1 TO WORK-ISS-PP
           ELSE
              IF PP-PREM < 0
                 SUBTRACT 1 FROM WORK-ISS-PP.
           IF O1-PREM > 0
              ADD 1 TO WORK-ISS-O1
           ELSE
              IF O1-PREM < 0
                 SUBTRACT 1 FROM WORK-ISS-O1.
           IF O2-PREM > 0
              ADD 1 TO WORK-ISS-O2
           ELSE
              IF O2-PREM < 0
                 SUBTRACT 1 FROM WORK-ISS-O2.
           IF O3-PREM > 0
              ADD 1 TO WORK-ISS-O3
           ELSE
              IF O3-PREM < 0
                 SUBTRACT 1 FROM WORK-ISS-O3.
           IF NF-PREM > 0
              ADD 1 TO WORK-ISS-NF
           ELSE
              IF NF-PREM < 0
                 SUBTRACT 1 FROM WORK-ISS-NF.

           IF IO-FG = 0
              PERFORM REWRITE-WK-FILE
           ELSE
              PERFORM WRITE-WK-FILE.

           GO TO SORT-TRL-NEXT.

       SORT-TRL-EXIT.
           EXIT.

      ******************************************************************
       SORT-TRS-FILE SECTION.
      ******************************************************************
           PERFORM OPEN-TRS1-FILE.

           MOVE BR-NO               TO TS-BRNO
                                       QTRS1-WBEG-BRNO
                                       QTRS1-WEND-BRNO.
           MOVE BEGINNING-DATE      TO TS-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRS1-WBEG-DATE.
           MOVE RPT-END-DATE        TO  SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRS1-WEND-DATE.
           MOVE BEG-CLASS           TO TS-CLASS
                                       QTRS1-WBEG-CLASS.
           MOVE END-CLASS           TO QTRS1-WEND-CLASS.
           MOVE 0                   TO TS-ACCTNO TS-LAWCODE TS-LINE
                                       QTRS1-WBEG-ACCTNO
                                       QTRS1-WBEG-LAWCODE
                                       QTRS1-WBEG-LINE.
           MOVE ALL "9"             TO QTRS1-WEND-ACCTNO
                                       QTRS1-WEND-LAWCODE
                                       QTRS1-WEND-LINE.
           PERFORM START-TRS1-FILE.

       SORT-TRS-NEXT.
           PERFORM READ-TRS1-FILE-NEXT.
           IF (IO-FG NOT = 0) OR (TRS-PATH-OWNBR NOT = TS-BRNO)
              GO TO SORT-TRS-EXIT.

           IF TS-DATE > RPT-END-DATE
              GO TO SORT-TRS-EXIT.

           IF TS-CLASS < BEG-CLASS OR TS-CLASS > END-CLASS
              GO TO SORT-TRS-NEXT.

           IF TS-INSCOMP(1) = 0 AND TS-INSCOMP(2) = 0 AND
               TS-INSCOMP(3) = 0 AND TS-INSCOMP(4) = 0 AND
                TS-INSCOMP(5) = 0 AND TS-INSCOMP(6) = 0 AND
                 TS-FEECODE(1) = SPACES
              GO TO SORT-TRS-NEXT.

           PERFORM FIND-TS-FEE.

           IF TS-INSCOMP(1) < BEG-IC OR TS-INSCOMP(1) > END-IC
            IF TS-INSCOMP(2) < BEG-IC OR TS-INSCOMP(2) > END-IC
             IF TS-INSCOMP(3) < BEG-IC OR TS-INSCOMP(3) > END-IC
              IF TS-INSCOMP(4) < BEG-IC OR TS-INSCOMP(4) > END-IC
               IF TS-INSCOMP(5) < BEG-IC OR TS-INSCOMP(5) > END-IC
                IF TS-INSCOMP(6) < BEG-IC OR TS-INSCOMP(6) > END-IC
                 IF HOLD-NON-FILE = 0
                   GO TO SORT-TRS-NEXT.

      * ALREADY SKIPPED INVALID COMPANIES OR NO INSURANCE
            IF TS-INSOURS(1) NOT = "Y" AND
                TS-INSOURS(2) NOT = "Y" AND
                 TS-INSOURS(3) NOT = "Y" AND
                  TS-INSOURS(4) NOT = "Y" AND
                   TS-INSOURS(5) NOT = "Y" AND
                    TS-INSOURS(6) NOT = "Y"
                      IF HOLD-NON-FILE = 0
                         GO TO SORT-TRS-NEXT.

           MOVE TS-BRNO   TO LN-OWNBR.
           MOVE TS-ACCTNO TO LN-ACCTNO.
           PERFORM READ-LN1-FILE.
           IF IO-FG NOT = 0
              GO TO SORT-TRS-NEXT.

           IF LN-ORGST < BEG-ST OR LN-ORGST > END-ST
              GO TO SORT-TRS-NEXT.

           MOVE TS-BRNO TO WORK-OWNBR.
           PERFORM READ-WK-FILE.
           IF IO-FG NOT = 0
              PERFORM CLEAR-WORK-FILE.

      * JUST USING THESE TO DETERMINE IF A NEW LOAN OR VOID FOR
      * ADDING TO THE ISSUE COUNTER AT THE END
           MOVE 0 TO LIFE-PREM AH-PREM PP-PREM O1-PREM O2-PREM O3-PREM
                      NF-PREM.

           MOVE TS-BRNO TO WORK-OWNBR.

           IF (TS-INSOURS(1) = "Y" ) AND
                (TS-INSCOMP(1) NOT < BEG-IC AND NOT > END-IC)
              COMPUTE WORK-NEW-CL = WORK-NEW-CL + TS-INSPREM(1)
              MOVE TS-INSPREM(1) TO LIFE-PREM
           END-IF
           IF (TS-INSOURS(2) = "Y" ) AND
                (TS-INSCOMP(2) NOT < BEG-IC AND NOT > END-IC)
              COMPUTE WORK-NEW-AH = WORK-NEW-AH + TS-INSPREM(2)
              MOVE TS-INSPREM(2) TO AH-PREM
           END-IF
           IF (TS-INSOURS(3) = "Y" ) AND
                 (TS-INSCOMP(3) NOT < BEG-IC AND NOT > END-IC)
              COMPUTE WORK-NEW-PP = WORK-NEW-PP + TS-INSPREM(3)
              MOVE TS-INSPREM(3) TO PP-PREM
           END-IF
           IF (TS-INSOURS(4) = "Y" ) AND
                 (TS-INSCOMP(4) NOT < BEG-IC AND NOT > END-IC)
              COMPUTE WORK-NEW-O1 = WORK-NEW-O1 + TS-INSPREM(4)
              MOVE TS-INSPREM(4) TO O1-PREM
           END-IF
           IF (TS-INSOURS(5) = "Y" ) AND
                 (TS-INSCOMP(5) NOT < BEG-IC AND NOT > END-IC)
              COMPUTE WORK-NEW-O2 = WORK-NEW-O2 + TS-INSPREM(5)
              MOVE TS-INSPREM(5) TO O2-PREM
           END-IF
           IF (TS-INSOURS(6) = "Y" ) AND
                 (TS-INSCOMP(6) NOT < BEG-IC AND NOT > END-IC)
              MOVE TS-INSPREM(6) TO O3-PREM
              COMPUTE WORK-NEW-O3 = WORK-NEW-O3 + TS-INSPREM(6)
           END-IF
           IF HOLD-NON-FILE NOT = 0
             MOVE HOLD-NON-FILE TO NF-PREM
             COMPUTE WORK-NEW-NF = WORK-NEW-NF + HOLD-NON-FILE.

           IF LIFE-PREM > 0
              ADD 1 TO WORK-ISS-CL
           ELSE
              IF LIFE-PREM < 0
                 SUBTRACT 1 FROM WORK-ISS-CL.
           IF AH-PREM > 0
              ADD 1 TO WORK-ISS-AH
           ELSE
              IF AH-PREM < 0
                 SUBTRACT 1 FROM WORK-ISS-AH.
           IF PP-PREM > 0
              ADD 1 TO WORK-ISS-PP
           ELSE
              IF PP-PREM < 0
                 SUBTRACT 1 FROM WORK-ISS-PP.
           IF O1-PREM > 0
              ADD 1 TO WORK-ISS-O1
           ELSE
              IF O1-PREM < 0
                 SUBTRACT 1 FROM WORK-ISS-O1.
           IF O2-PREM > 0
              ADD 1 TO WORK-ISS-O2
           ELSE
              IF O2-PREM < 0
                 SUBTRACT 1 FROM WORK-ISS-O2.
           IF O3-PREM > 0
              ADD 1 TO WORK-ISS-O3
           ELSE
              IF O3-PREM < 0
                 SUBTRACT 1 FROM WORK-ISS-O3.
           IF NF-PREM > 0
              ADD 1 TO WORK-ISS-NF
           ELSE
              IF NF-PREM < 0
                 SUBTRACT 1 FROM WORK-ISS-NF.

           IF IO-FG = 0
              PERFORM REWRITE-WK-FILE
           ELSE
              PERFORM WRITE-WK-FILE.

           GO TO SORT-TRS-NEXT.

       SORT-TRS-EXIT.
           EXIT.

      **********************
       FIND-TL-FEE SECTION.
      **********************
           MOVE 1 TO SUB.
           MOVE 0 TO HOLD-NON-FILE.
       NEXT-TL-FEE.
           IF SUB > 10
              GO TO EXIT-TL-FEE.
           IF TL-FEECODE(SUB) = SPACES
              ADD 1 TO SUB
              GO TO NEXT-TL-FEE.

           IF TL-FEECODE(SUB) = FEE-CODE
              MOVE TL-FEEAMT(SUB) TO HOLD-NON-FILE.

           ADD 1 TO SUB.
           GO TO NEXT-TL-FEE.
       EXIT-TL-FEE.
           EXIT.

      **********************
       FIND-TS-FEE SECTION.
      **********************
           MOVE 1 TO SUB.
           MOVE 0 TO HOLD-NON-FILE.
       NEXT-TS-FEE.
           IF SUB > 10
              GO TO EXIT-TS-FEE.
           IF TS-FEECODE(SUB) = SPACES
              ADD 1 TO SUB
              GO TO NEXT-TS-FEE.

           IF TS-FEECODE(SUB) = FEE-CODE
              MOVE TS-FEEAMT(SUB) TO HOLD-NON-FILE.

           ADD 1 TO SUB.
           GO TO NEXT-TS-FEE.
       EXIT-TS-FEE.
           EXIT.

      ******************************************************************
       BR-TOTALS SECTION.
      ******************************************************************

           MOVE WORK-OWNBR TO BR-NO H-BRNO.
           PERFORM READ-BR-FILE.
           IF IO-FG = 9
              MOVE "* INVALID *" TO BR-NAME.

           MOVE BR-NAME TO H-BRNAME.
           MOVE 99 TO LN-COUNT.

           MOVE SPACES TO DETAIL-LINE.

           MOVE "CREDIT LIFE :" TO D-TAG.
           MOVE WORK-ISS-CL TO D-QTY.
           MOVE WORK-NEW-CL TO D-PREM.
           MOVE WORK-CAN-CL TO D-REF.
           COMPUTE D-NET = WORK-NEW-CL + WORK-CAN-CL.
      *    COMPUTE D-NET = WORK-NEW-CL - WORK-CAN-CL.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "A & H       :" TO D-TAG.
           MOVE WORK-ISS-AH TO D-QTY.
           MOVE WORK-NEW-AH TO D-PREM.
           MOVE WORK-CAN-AH TO D-REF.
           COMPUTE D-NET = WORK-NEW-AH + WORK-CAN-AH.
      *    COMPUTE D-NET = WORK-NEW-AH - WORK-CAN-AH.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PROPERTY    :" TO D-TAG.
           MOVE WORK-ISS-PP TO D-QTY.
           MOVE WORK-NEW-PP TO D-PREM.
           MOVE WORK-CAN-PP TO D-REF.
           COMPUTE D-NET = WORK-NEW-PP + WORK-CAN-PP.
      *    COMPUTE D-NET = WORK-NEW-PP - WORK-CAN-PP.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "OTHER 1     :" TO D-TAG.
           MOVE WORK-ISS-O1 TO D-QTY.
           MOVE WORK-NEW-O1 TO D-PREM.
           MOVE WORK-CAN-O1 TO D-REF.
           COMPUTE D-NET = WORK-NEW-O1 + WORK-CAN-O1.
      *    COMPUTE D-NET = WORK-NEW-O1 - WORK-CAN-O1.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "OTHER 2     :" TO D-TAG.
           MOVE WORK-ISS-O2 TO D-QTY.
           MOVE WORK-NEW-O2 TO D-PREM.
           MOVE WORK-CAN-O2 TO D-REF.
           COMPUTE D-NET = WORK-NEW-O2 + WORK-CAN-O2.
      *    COMPUTE D-NET = WORK-NEW-O2 - WORK-CAN-O2.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "OTHER 3     :" TO D-TAG.
           MOVE WORK-ISS-O3 TO D-QTY.
           MOVE WORK-NEW-O3 TO D-PREM.
           MOVE WORK-CAN-O3 TO D-REF.
           COMPUTE D-NET = WORK-NEW-O3 + WORK-CAN-O3.
      *    COMPUTE D-NET = WORK-NEW-O3 - WORK-CAN-O3.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "NON-FILE    :" TO D-TAG.
           MOVE WORK-ISS-NF TO D-QTY.
           MOVE WORK-NEW-NF TO D-PREM.
           MOVE WORK-CAN-NF TO D-REF.
           COMPUTE D-NET = WORK-NEW-NF + WORK-CAN-NF.
      *    COMPUTE D-NET = WORK-NEW-NF - WORK-CAN-NF.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.


           MOVE "** TOTAL    :" TO D-TAG.
           COMPUTE D-QTY = WORK-ISS-CL + WORK-ISS-AH + WORK-ISS-PP
                         + WORK-ISS-O1 + WORK-ISS-O2 + WORK-ISS-O3
                         + WORK-ISS-NF.
           COMPUTE D-PREM TOT-PREM =
                            WORK-NEW-CL + WORK-NEW-AH + WORK-NEW-PP
                          + WORK-NEW-O1 + WORK-NEW-O2 + WORK-NEW-O3
                          + WORK-NEW-NF.
           COMPUTE D-REF TOT-REF =
                           WORK-CAN-CL + WORK-CAN-AH + WORK-CAN-PP
                         + WORK-CAN-O1 + WORK-CAN-O2 + WORK-CAN-O3
                         + WORK-CAN-NF.
           COMPUTE D-NET = TOT-PREM + TOT-REF.
           MOVE 3 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      ******************************************************************
       COMPANY-TOTALS SECTION.
      ******************************************************************

           MOVE 99 TO LN-COUNT.
           MOVE 0 TO H-BRNO WORK-OWNBR.

           MOVE SPACES TO DETAIL-LINE.
           MOVE "CREDIT LIFE :" TO D-TAG.
           MOVE LIFE-ISS TO D-QTY.
           MOVE LIFE-PREM TO D-PREM.
           MOVE LIFE-CAN TO D-REF.
           COMPUTE D-NET = LIFE-PREM + LIFE-CAN
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE SPACES TO DETAIL-LINE.
           MOVE "A & H       :" TO D-TAG.
           MOVE AH-ISS TO D-QTY.
           MOVE AH-PREM TO D-PREM.
           MOVE AH-CAN TO D-REF.
           COMPUTE D-NET = AH-PREM + AH-CAN.
      *    COMPUTE D-NET = AH-ISS - AH-CAN.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE SPACES TO DETAIL-LINE.
           MOVE "PROPERTY    :" TO D-TAG.
           MOVE PP-ISS TO D-QTY.
           MOVE PP-PREM TO D-PREM.
           MOVE PP-CAN TO D-REF.
           COMPUTE D-NET = PP-PREM + PP-CAN.
      *    COMPUTE D-NET = PP-ISS - PP-CAN.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE SPACES TO DETAIL-LINE.
           MOVE "OTHER 1     :" TO D-TAG.
           MOVE O1-ISS TO D-QTY.
           MOVE O1-PREM TO D-PREM.
           MOVE O1-CAN TO D-REF.
           COMPUTE D-NET = O1-PREM + O1-CAN.
      *    COMPUTE D-NET = O1-ISS - O1-CAN.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE SPACES TO DETAIL-LINE.
           MOVE "OTHER 2     :" TO D-TAG.
           MOVE O2-ISS TO D-QTY.
           MOVE O2-PREM TO D-PREM.
           MOVE O2-CAN TO D-REF.
           COMPUTE D-NET = O2-PREM + O2-CAN.
      *    COMPUTE D-NET = O2-ISS - O2-CAN.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE SPACES TO DETAIL-LINE.
           MOVE "OTHER 3     :" TO D-TAG.
           MOVE O3-ISS TO D-QTY.
           MOVE O3-PREM TO D-PREM.
           MOVE O3-CAN TO D-REF.
           COMPUTE D-NET = O3-PREM + O3-CAN.
      *    COMPUTE D-NET = O3-ISS - O3-CAN.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE SPACES TO DETAIL-LINE.
           MOVE "NON-FILE    :" TO D-TAG.
           MOVE NF-ISS TO D-QTY.
           MOVE NF-PREM TO D-PREM.
           MOVE NF-CAN TO D-REF.
           COMPUTE D-NET = NF-PREM + NF-CAN.
      *    COMPUTE D-NET = NF-ISS - NF-CAN.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "** TOTAL    :" TO D-TAG.
           COMPUTE D-QTY = LIFE-ISS + AH-ISS + PP-ISS
                         + O1-ISS + O2-ISS + O3-ISS + NF-ISS.
           COMPUTE D-PREM TOT-PREM =
                            LIFE-PREM + AH-PREM + PP-PREM
                          + O1-PREM + O2-PREM + O3-PREM + NF-PREM.
           COMPUTE D-REF TOT-REF =
                           LIFE-CAN + AH-CAN + PP-CAN
                         + O1-CAN + O2-CAN + O3-CAN + NF-CAN.
           COMPUTE D-NET = TOT-PREM + TOT-REF.
      *    COMPUTE D-NET = TOT-PREM - TOT-REF.
           MOVE 3 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.


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
       HEAD-PAGE SECTION.
      ******************************************************************
           MOVE "N" TO FIRST-TIME.
           ADD 1 TO PAGE-NUMBER.
           MOVE PAGE-NUMBER TO H-PAGE-NO.
           PERFORM GET-TIME.
           MOVE T-HH TO H-HR.
           MOVE T-MM TO H-MIN.

           MOVE HEAD1 TO PR-REC.
           MOVE 99 TO LN-FEED.
           PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE HEAD2 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE HEAD3 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
           IF H-BRNO NOT = 0
      * BRANCH NO AND NAME
              MOVE HEAD4 TO PR-REC
              MOVE 2 TO LN-FEED
              PERFORM WRITE-PR-REC
           ELSE
              MOVE "C O M P A N Y   T O T A L S:" TO H-PAGE-DESC
              MOVE HEAD-COMPANY TO PR-REC
              MOVE 2 TO LN-FEED
              PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           MOVE HEAD5 TO PR-REC
           MOVE 2 TO LN-FEED
           PERFORM WRITE-PR-REC.
           MOVE HEAD6 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE DASH-LINE TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

       END-HEAD-PAGE.
           MOVE 8 TO LN-COUNT.
           MOVE 2 TO LN-FEED.

      ******************************************************************
       RANGE-LINES SECTION.
      ******************************************************************

           MOVE 5 TO LN-FEED.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF GROUP-FLAG = "Y"
              MOVE "BRANCH GROUP" TO RANGE-LINE
              MOVE ENT-GROUP TO RANGE-SEL
              PERFORM WRITE-DETAIL-LINE
           ELSE
              MOVE "BEG BRANCH" TO RANGE-LINE
              MOVE BEG-BRANCH TO RANGE-SEL
              PERFORM WRITE-DETAIL-LINE
              MOVE "END BRANCH" TO RANGE-LINE
              MOVE END-BRANCH TO RANGE-SEL
              PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEG INS CO" TO RANGE-LINE.
           MOVE BEG-IC TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "END INS CO" TO RANGE-LINE.
           MOVE END-IC TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEG ORIG STATE" TO RANGE-LINE.
           MOVE BEG-ST TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "END ORIG STATE" TO RANGE-LINE.
           MOVE END-ST TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEG LOAN CLASS" TO RANGE-LINE.
           MOVE BEG-CLASS TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "END LOAN CLASS" TO RANGE-LINE.
           MOVE END-CLASS TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGIN DATE" TO RANGE-LINE.
           MOVE BDATE TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "END   DATE" TO RANGE-LINE.
           MOVE EDATE TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "NON FILE FEE-CODE" TO RANGE-LINE.
           MOVE FEE-CODE TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.

      ******************************************************************
      *
      *     C O P Y    L I B R A R Y     S E C T I O N
      *
      ******************************************************************
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/BRSECURE.CPY".

      ***********************************************************
       CLASS-SCAN SECTION.
      ***********************************************************
           MOVE 0      TO LCSCAN-KEY.
           MOVE SPACES TO LCSCAN-VERIFY.
           MOVE "WI/LCSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH LCSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF LCSCAN-STATUS NOT = "00" OR LCSCAN-KEY = 0
               MOVE 9 TO IO-FG
               GO TO EXIT-CLASS-SCAN.
           IF ( CLASS-FG= "BEG" )
              MOVE "SNNN020LC1." TO VDU
           ELSE
              MOVE "SNNN020LC2." TO VDU.
           MOVE LCSCAN-KEY TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE LCSCAN-KEY TO VDUNUM0.
           MOVE 0 TO IO-FG.

       EXIT-CLASS-SCAN.
           EXIT.

      **************************************************
       FORM-RESET SECTION.
      **************************************************
           MOVE EXT-CONAME-CONAME TO GATXIN-CONAME.
           MOVE EXT-CONAME-SYSDATE TO GATXIN-SYSDATE.
           DISPLAY GATXIN-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE GATXIN-FORM-FIELDS TO WR-BUF.
           MOVE GATXIN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      **************************************************
       SETUP-LINK-INFO SECTION.
      **************************************************
           MOVE GATXIN-HELP--FILE TO HELP-LINK-FILE.
           MOVE GATXIN-HELP--DIR TO HELP-LINK-DIR.

      **************************************************
       VDU-CONVERT-FIELD SECTION.
      **************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/GATXIN_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISC-PARA SECTION.
      ******************************************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/RMFILE.CPY".
       CLEAR-WORK-FILE.
               MOVE 0 TO WORK-NEW-CL WORK-CAN-CL WORK-ISS-CL
                         WORK-NEW-AH WORK-CAN-AH WORK-ISS-AH
                         WORK-NEW-PP WORK-CAN-PP WORK-ISS-PP
                         WORK-NEW-O1 WORK-CAN-O1 WORK-ISS-O1
                         WORK-NEW-O2 WORK-CAN-O2 WORK-ISS-O2
                         WORK-NEW-O3 WORK-CAN-O3 WORK-ISS-O3
                         WORK-NEW-NF WORK-CAN-NF WORK-ISS-NF.

      *-----------------------------------------------------------------
       INVALID-RANGE.
           MOVE "INVALID RANGE" TO MESS.
           PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       SEND-LEGEND.
           MOVE MESS TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPTRSGS_SQL.CPY".
       COPY "LIBLP/LPTRPGS_SQL.CPY".
       COPY "LIBLP/LPTRLGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBLP/LPTRP1RN.CPY".
       COPY "LIBLP/LPTRL1RN.CPY".
       COPY "LIBLP/LPTRS1RN.CPY".
       COPY "LIBGB/GBWKIN.CPY".

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
              MOVE "NO REPORT FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS.

           MOVE MKTEMP-BUF TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.

      *-----------------------------------------------------------------
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
              MOVE "LP/GATXMU" TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY GATXIN-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
