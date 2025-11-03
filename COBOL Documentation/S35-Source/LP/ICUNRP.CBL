      * A30 CONVERSION COMPLETED
       IDENTIFICATION DIVISION.
       PROGRAM-ID.      ICUNRP.
       DATE-WRITTEN.    081988.
      ******************************************************************
      *   DIR : LP
      *   DESC: UNEARNED INSURANCE PREMIUM REPORT
      *                         OR
      *         UNEARNED INSURANCE COMMISSION REPORT
      *
      *
      *       COMMISSION INCOME = BEGIN + NEW COMM - REBATES - ENDING
      *
      *
      * REVS:
      *
      *
      *         SELECTABLE
      *         REPORT TYPE: 1. CL AH PP
      *                      2. O1 O2
      *                      3. O3 O4
      *
      *   THIS REPORT PRODUCES THE FOLLOWING:
      *     1. THE TOTAL NO. OF ACCOUNTS WITH INSURANCE
      *        STILL INEFFECT AND THE ORIGINAL PREMIUM
      *        OR COMMISSION OF THESE ACCOUNTS.
      *     2. THE ENDING UNEARNED FOR THE PREVIOUS
      *        PERIOD ENDING.
      *     3. THE ENDING UNEARNED FOR THIS PERIOD.
      *----------------------------------------------------------------------
      *                            EXAMPLE
      *
      *                                      NEW    CANCEL
      *        PERIOD INCOME    =  BEGIN  + PREM  - REBATE - ENDING
      *
      *----------------------------------------------------------------------
      *        USING PRORATA 10
      *        MONTHS 1ST SLICE              NEW    CANCEL   PERIOD
      * DATE   IN 2ND MONTH        BEGIN  + PREM  - REBATE - INCOME = ENDING
      *----------------------------------------------------------------------
      *063088                                                       =   0.00*
      *----------------------------------------------------------------------
      *071088  #110 PREM $50.00
      *073188                       0.00   50.00                    =  50.00*
      *----------------------------------------------------------------------
      *081288  #111 PREM 100.00
      *083188                      50.00  100.00               5.00 FOR #110
      *                                                             = 145.00*
      *----------------------------------------------------------------------
      *091388  #112 PREM 200.00
      *091488  #110 RB    47.00
      *093088                     145.00  200.00     47.00    10.00 FOR #111
      *                                                       -2.00 FOR #110
      *                                                             = 290.00*
      *----------------------------------------------------------------------
      *
      *071095 KEC ADDED OPTION #3. O3 & O4
      *090795 KEC ADDED GROUP LOGIC.
      *100695 JFF ADDED CLASS SCAN
      *BLV 060597 CHANGED FOR LOCAL SPOOL WITH NEW BATCH DRIVER - BREXEC
      *GLF 090297 REMOVED ALL USE OF GLOBAL
      *GLF 091499 ADD INCLUDE, SKIP, ONLY OF P&L ACCOUNTS, DEFAULTS TO INCLUDE
      *           P&L. ADDED PER JR
      *  BAH 010320 ADDED BRANCH SECURITY PASSWORD TESTING (BRSECURE)
      *  BLF 050721 ADDED PARAMETER TO PRINT OR NOT PRINT BRANCH TOTALS,
      *             WORLD PR# 413
      *   CS 161031 AT END-RUN, ADDED IF EXT-ROUTE-BUF = "00" PERFORM PRU-OUT
      * BAH 170605 ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 2020-0827 REMOVED RESET ON BR1 
      * JKC 2022-0809 PR#1565 SECURITIZATION - ADDED FILE EXTRACTION
      *               ADDED EXPORT-FILE-YNO
      *
      * JKC 2024-0503 COMMENTING OUT THE 'PERFORM PRU-OUT.'; IT WAS NOT
      *         NEED SINCE IT WAS BEING DONE IN ICUNR1/2/3; IT WAS
      *         CAUSING DOUBLE DISPLAY OF THE TIMER WINDOW SINCE IT IS
      *         DONE WITHIN PRU-OUT.
      *
      * JKC 2025-0422 REMOVED A 'PERFORM SQL-DISCONNECT' SINCE IT WAS
      *         FOR A TEST THAT GOT LEFT IN.
      *
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
      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E    S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LP/ICUNRP.CBL S35 04/22/25-09:50:05 >".

      ******************************************************************
      *
      *    P R O G R A M S - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
      *-----------------------------------------------------------------

       01  MESS-LIST           PIC X(5)  VALUE "MSEL.".
       01  CLASS-FG            PIC X(3)  VALUE "   ".
           88  CLASS-BEG                 VALUE "BEG".
           88  CLASS-END                 VALUE "END".
       01  LN-FEED          PIC 99       VALUE 0.

      ******************************************************************
      *
      *     S C R E E N   F I E L D S
      *
      ******************************************************************
       COPY "LIBGB/EOBUF_EXT.CPY".
       COPY "LIBCL/ICUNRPW.CPY".

       01  VERSION-CONTROL        PIC X    VALUE "B".

      ******************************************************************
      *
      *     C O P Y   L I B R A R Y    W O R K E R S
      *
      ******************************************************************
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBWI/LCSCANW.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       01  EXPORT-FILE-SEQ             PIC 9(2)      VALUE 01.
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/ICUNRP_DEF.CPY".
       COPY "LIBLP/ICUNRP_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBLP/ICUNRP_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
            USE AFTER ERROR PROCEDURE ON
                PR-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ******************************************************************
      *
      *    M A I N    M O D U L E
      *
      ******************************************************************
       MAIN-MODULE SECTION.

      D    STOP MESS.
           PERFORM SQL-CONNECT.

           MOVE "ICUNRP" TO VDU-FORM-NAME.

           PERFORM INITIALIZATION.

           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

           PERFORM RUN-REPORT.

           GO TO END-PROGRAM.

      ******************************************************************
      *
      *    I N I T I A L I Z A T I O N
      *
      ******************************************************************
       INITIALIZATION SECTION.
           MOVE ICUNRP-QUEUE-POS TO Q-POS.
           MOVE ICUNRP-COPIES-POS TO C-POS.
           MOVE ICUNRP-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

      *----------------------------------------------------------------
       COPY "LIBGB/EOINIT.CPY".
      *----------------------------------------------------------------

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

           IF EXT-ROUTE-BUF = "00"
      * SET REPORT ENDING DATE:
              MOVE MM TO DATE-MMDDYY-MM
              MOVE 33 TO DATE-MMDDYY-DD
              MOVE YY TO DATE-MMDDYY-YY
              PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
              MOVE DATE-YYYYMMDD TO NDTE-DATE
              PERFORM NEW-DATE-CALCULATION
              MOVE NDTE-DATE TO REPORT-ENDING-DATE
      * SET PRIOR MONTH ENDING DATE:
              MOVE 33 TO NDTE-DD
              MOVE -1 TO NDTE-HOLD
              PERFORM INCREMENT-MONTHS
              MOVE NDTE-DATE TO PRIOR-ENDING-DATE.

           GO TO INITIALIZE-EXIT.

      *----------------------------------------------------------------
       INITIALIZE-ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.

       INITIALIZE-EXIT.
           EXIT.

      ******************************************************************
      *
      *    SETUP DATES FOR BATCH PROCESS
      *
      ******************************************************************
       EO-EXEC-AUDIT SECTION.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * GET REPORT ENDING DATE:
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           MOVE EXT-EOBUF-DATE2 TO NDTE-DATE.
           MOVE NDTE-MM TO MM.
           MOVE 33 TO NDTE-DD.
           MOVE NDTE-YY TO YY.
           PERFORM NEW-DATE-CALCULATION.
           MOVE NDTE-DATE TO REPORT-ENDING-DATE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * GET PRIOR MONTH ENDING DATE:
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           MOVE 33 TO NDTE-DD.
           MOVE -1 TO NDTE-HOLD.
           PERFORM INCREMENT-MONTHS.
           MOVE NDTE-DATE TO PRIOR-ENDING-DATE.

      ******************************************************************
      *
      *    ENTER REPORT PARAMETERS
      *
      ******************************************************************
       ENTER-PARAMETERS SECTION.
      *----------------------------------------------------------------
       ENTER-PARAM.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * FOR RE-ENTRY ON F4 FROM ACCEPT.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           IF VDUSTATUS = "1U" GO TO SO-01.

      *----------------------------------------------------------------
       WH-01.
           MOVE "ENNN010WHICH." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO WH-01.
           MOVE VDUNUM0 TO WH-INS.

      *----------------------------------------------------------------
       BR-01.
           MOVE "E  A000BR1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-BR.

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

      *----------------------------------------------------------------
       IC-01.
           MOVE "ENNN020IC1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-IC.
           IF VDUSTATUS = "1U" GO TO BR-02.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO IC-01.
           MOVE VDUNUM0 TO BEG-IC.

      *----------------------------------------------------------------
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

      *----------------------------------------------------------------
       ALL-IC.
           MOVE "SNNN020IC1." TO VDU.
           MOVE 1 TO BEG-IC VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020IC2." TO VDU.
           MOVE 99 TO END-IC VDUNUM0.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
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
           MOVE VDUNUM0 TO BEG-LC.

      *----------------------------------------------------------------
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
           MOVE VDUNUM0 TO END-LC.
           IF BEG-LC NOT > END-LC GO TO ST-01.
           PERFORM INVALID-RANGE.
           GO TO LC-01.

      *----------------------------------------------------------------
       ALL-LC.
           MOVE "SNNN020LC1." TO VDU.
           MOVE 1 TO BEG-LC VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020LC2." TO VDU.
           MOVE 99 TO END-LC VDUNUM0.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
       ST-01.
           MOVE "E  A020ST1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-ST.
           IF VDUSTATUS = "1U" GO TO LC-02.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ST-01.
           MOVE VDUBUF TO BEG-ST.

      *----------------------------------------------------------------
       ST-02.
           MOVE "E  A020ST2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-ST.
           IF VDUSTATUS = "1U" GO TO ST-01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ST-02.
           MOVE VDUBUF TO END-ST.
           IF BEG-ST NOT > END-ST GO TO DT-01.
           PERFORM INVALID-RANGE.
           GO TO ST-01.

      *----------------------------------------------------------------
       ALL-ST.
           MOVE "S  A020ST1." TO VDU.
           MOVE "  " TO BEG-ST VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A020ST2." TO VDU.
           MOVE ALL "Z" TO END-ST.
           CALL "C$TOLOWER" USING END-ST, VALUE 2.
           MOVE END-ST TO VDUBUF.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
       DT-01.
           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO ZBAL-01.

           MOVE "ENNN040DT1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ST-02.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO DT-01.
           MOVE VDUNUM0 TO MMYY.

           IF MM < 1 OR MM > 12
              MOVE "INVALID MONTH" TO MESS
              PERFORM SEND-MESS
              GO TO DT-01.

      *----------------------------------------------------------------
       ZBAL-01.
           MOVE "E  A010ZBAL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.

           IF VDUSTATUS = "1U"
              IF EXT-ROUTE-BUF = "40" OR "50"
                 GO TO ST-02
              ELSE
                 GO TO DT-01.

           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ZBAL-01.
           MOVE VDUBUF TO ZBAL-ONLY.
           IF ZBAL-ONLY = "Y" OR "N" GO TO PL-01.
           IF ZBAL-ONLY NOT = "Y" OR "N" GO TO ZBAL-01.

      *----------------------------------------------------------------
       PL-01.
           MOVE "E  A010PL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.

           IF VDUSTATUS = "1U"
                 GO TO ZBAL-01.

           IF NOT (VDUSTATUS = "00" OR "1D") GO TO PL-01.
           MOVE VDUBUF TO PL.
           IF PL = "I" OR "S" OR "O" GO TO AD-01.
           IF PL NOT = "I" OR "S" OR "O"
                       GO TO PL-01.

      *----------------------------------------------------------------
       AD-01.
           MOVE "E  A010AD1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.

           IF VDUSTATUS = "1U"
                    GO TO PL-01.

           IF NOT (VDUSTATUS = "00" OR "1D") GO TO AD-01.

           IF NOT ( VDUBUF = "Y" OR "N" )
              GO TO AD-01.

           MOVE VDUBUF TO ACCOUNT-DETAIL.

      *----------------------------------------------------------------
       ENTER-EXPORT-FILE-YNO.

           IF ( UP-KEY )
              IF ( ACCOUNT-DETAIL NOT = "Y" )
                 GO TO AD-01.

           IF ( ACCOUNT-DETAIL NOT = "Y" )
              MOVE "S  A010EXPORT-YNO." TO VDU
              MOVE "N"                  TO VDUBUF EXPORT-FILE-YNO
              PERFORM SCREENIO
              GO TO SO-01.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "E  A010EXPORT-YNO." TO VDU.
           PERFORM SCREENIO.

           IF ( UP-KEY ) GO TO AD-01.
           IF ( F1-KEY ) GO TO EXIT-PARM.
           IF ( F7-KEY ) GO TO EXIT-PARM.
           IF ( NOT ENTER-KEY ) GO TO ENTER-EXPORT-FILE-YNO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( VDUBUF = "Y" OR "N" OR "O" )
              MOVE VDUBUF TO EXPORT-FILE-YNO
           ELSE
              MOVE "ERROR: (Y)ES, (N)O OR (O)NLY" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-EXPORT-FILE-YNO.

           IF ( EXPORT-FILE-YNO NOT = "N" )
              MOVE EXPORT-FILE-SEQ               TO PASSWDW-SEQ
              MOVE "FOR CREATING A EXPORT FILE:" TO PASSWDW-PROMPT2
              PERFORM VERIFY-PASSWORD
              IF PASSWDW-STAT NOT = "OK"
                 GO TO ENTER-EXPORT-FILE-YNO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXPORT-FILE-YNO NOT = "O" )
              GO TO SO-01.

           MOVE "N" TO SUMMARY-ONLY
                       PRINT-BRANCH-TOTS.

           PERFORM DISPLAY-PARAMETERS.
           GO TO EXIT-PARM.

      *----------------------------------------------------------------
       SO-01.

           MOVE "E  A010SO1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ENTER-EXPORT-FILE-YNO.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO SO-01.
           MOVE VDUBUF TO SUMMARY-ONLY.
           IF NOT (SUMMARY-ONLY = "Y" OR "N") GO TO SO-01.

      *----------------------------------------------------------------
       BRTOTS-YN.
           IF ACCOUNT-DETAIL = "Y"
              MOVE "S  A010BRTOTS." TO VDU
              MOVE "Y" TO PRINT-BRANCH-TOTS VDUBUF
              PERFORM SCREENIO
              GO TO EXIT-PARM.
           MOVE "E  A010BRTOTS." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.

           IF VDUSTATUS = "1U" GO TO SO-01.

           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BRTOTS-YN.
           MOVE VDUBUF TO PRINT-BRANCH-TOTS.
           IF PRINT-BRANCH-TOTS NOT = "Y"
              IF PRINT-BRANCH-TOTS NOT = "N"
                 GO TO BRTOTS-YN.

      *----------------------------------------------------------------
       EXIT-PARM.
           EXIT.

      ******************************************************************
      *
      *   DISPLAY REPORT PARAMETERS
      *
      ******************************************************************
       DISPLAY-PARAMETERS SECTION.
      *----------------------------------------------------------------
       DISPLAY-PARM.
           MOVE "SNNN010WHICH." TO VDU.
           MOVE WH-INS TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
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
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN020IC1." TO VDU.
           MOVE BEG-IC TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN020IC2." TO VDU.
           MOVE END-IC TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN020LC1." TO VDU.
           MOVE BEG-LC TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN020LC2." TO VDU.
           MOVE END-LC TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A020ST1." TO VDU.
           MOVE BEG-ST TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A020ST2." TO VDU.
           MOVE END-ST TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010ZBAL." TO VDU.
           MOVE ZBAL-ONLY TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010PL." TO VDU.
           MOVE PL TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010AD1." TO VDU.
           MOVE ACCOUNT-DETAIL TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010EXPORT-YNO." TO VDU.
           MOVE EXPORT-FILE-YNO TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010SO1." TO VDU.
           MOVE SUMMARY-ONLY TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010BRTOTS." TO VDU.
           MOVE PRINT-BRANCH-TOTS TO VDUBUF.
           PERFORM SCREENIO.

      ******************************************************************
      *
      *    RUN REPORT SECTION
      *
      ******************************************************************
       RUN-REPORT SECTION.

           IF WH-INS = 1
              MOVE "CL/ICUNR1" TO FORM-NAM
           ELSE
           IF WH-INS = 2
              MOVE "CL/ICUNR2" TO FORM-NAM
           ELSE
              MOVE "CL/ICUNR3" TO FORM-NAM.

           CALL FORM-PROGX 
              USING FORM-PATHNAME PRU-PATH  PRU-LINK EXIT-PATHNAME.
           CANCEL FORM-PROGX.

      ***************************************************************
      *
      *    C L A S S    S C A N    R O U T I N E
      *
      ***************************************************************
       CLASS-SCAN SECTION.
           MOVE 0      TO LCSCAN-KEY.
           MOVE SPACES TO LCSCAN-VERIFY.
           MOVE "WI/LCSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH LCSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF LCSCAN-STATUS NOT = "00" OR LCSCAN-KEY = 0
               MOVE 9 TO IO-FG
               GO TO EXIT-CLASS-SCAN.
           IF ( CLASS-FG = "BEG" )
              MOVE "SNNN020LC1." TO VDU
           ELSE
              MOVE "SNNN020LC2." TO VDU.
           MOVE LCSCAN-KEY TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE LCSCAN-KEY TO VDUNUM0.
           MOVE 0 TO IO-FG.

       EXIT-CLASS-SCAN.
           EXIT.

      ******************************************************************
      *
      *     C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/BRSECURE.CPY".

      *********************************************************
       FORM-RESET SECTION.
      *********************************************************
           MOVE EXT-CONAME-CONAME TO ICUNRP-CONAME.
           MOVE EXT-CONAME-SYSDATE TO ICUNRP-SYSDATE.
           DISPLAY ICUNRP-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE ICUNRP-FORM-FIELDS TO WR-BUF.
           MOVE ICUNRP-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       FORM-RESET-EXIT.
           EXIT.

      *********************************************************
       SETUP-LINK-INFO SECTION.
      *********************************************************
           MOVE ICUNRP-HELP--FILE TO HELP-LINK-FILE.
           MOVE ICUNRP-HELP--DIR  TO HELP-LINK-DIR.

      *********************************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/ICUNRP_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
      *
      *    M I S C   P A R A G R A P H S
      *
      ******************************************************************
       MISC-PARA SECTION.
      *----------------------------------------------------------------
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/GETPR.CPY".
      *----------------------------------------------------------------
       SEND-LEGEND.
           MOVE MESS TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *----------------------------------------------------------------
       INVALID-RANGE.
           MOVE "INVALID RANGE ENTERED" TO MESS.
           PERFORM SEND-MESS.

      *=================================================================
       VERIFY-PASSWORD.

           MOVE " "                          TO PASSWDW-STAT.
           MOVE "ICUNRP"                     TO PASSWDW-PGM.
           MOVE "PLEASE ENTER THE PASSWORD," TO PASSWDW-PROMPT1.
           MOVE "CL/PASSWD"                  TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
           CANCEL FORM-PATH.

      ******************************************************************
      *
      *    I O - R O U T I N E S
      *
      ******************************************************************
       IO-ROUTINES SECTION.
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBGB/GBPRINT.CPY".

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      ******************************************************************
      *
      *    E N D    P R O G R A M
      *
      ******************************************************************
       END-PROGRAM SECTION.
      *-----------------------------------------------------------------
       EXIT-PROG.
           PERFORM CLOSE-FILES.

      *KC  IF EXT-ROUTE-BUF = "00"
      *KC     PERFORM PRU-OUT.

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
              DESTROY ICUNRP-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
