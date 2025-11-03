      * A30 CONVERSION COMPLETED
       IDENTIFICATION DIVISION.
       PROGRAM-ID.      FEUNRP.
       DATE-WRITTEN.    11012004.
      **************************************************************************
      *   DIR : LP
      *   DESC: UNEARNED FEE CODE REPORT
      *
      *         INCOME = BEGIN + NEW FEES - ENDING
      *
      *
      *    UP TO 3 ENTERED FEE CODES PER REPORT
      *    FEE CODES AND EARNED METHODS ENTERED ON SCREEN
      *
      *   THIS REPORT PRODUCES THE FOLLOWING:
      *     1. THE TOTAL NO. OF ACCOUNTS WITH FEE CODES
      *        STILL INEFFECT AND THE ORIGINAL FEE AMOUNT
      *        OF THESE ACCOUNTS.
      *     2. THE ENDING UNEARNED FOR THE PREVIOUS
      *        PERIOD ENDING.
      *     3. THE ENDING UNEARNED FOR THIS PERIOD.
      *----------------------------------------------------------------------
      *        EXAMPLE
      *
      *                                      NEW
      *        PERIOD INCOME    =  BEGIN + FEECD  - ENDING
      *
      *----------------------------------------------------------------------
      *        USING PRORATA 10
      *        MONTHS 1ST SLICE               NEW   PERIOD
      * DATE   IN 2ND MONTH        BEGIN  + FEECD - INCOME = ENDING
      *----------------------------------------------------------------------
      *063088                                                       =   0.00*
      *----------------------------------------------------------------------
      *071088  #110 FEECD  $50.00
      *073188                       0.00    50.00                   =  50.00*
      *----------------------------------------------------------------------
      *081288  #111 FEECD $100.00
      *083188                      50.00   100.00              5.00 FOR #110
      *                                                             = 145.00*
      *----------------------------------------------------------------------
      *091388  #112 FEECD $200.00
      *093088                     145.00   200.00    47.00    10.00 FOR #111
      *                                                       -2.00 FOR #110
      *                                                             = 290.00*
      *----------------------------------------------------------------------
      *
      * REVS:
      * KEC 071095 ADDED OPTION #3. O3 & O4
      * KEC 090795 ADDED GROUP LOGIC.
      * JFF 100695 ADDED CLASS SCAN
      * BLV 060597 CHANGED FOR LOCAL SPOOL WITH NEW BATCH DRIVER - BREXEC
      * GLF 090297 REMOVED ALL USE OF GLOBAL
      * GLF 091499 ADD INCLUDE, SKIP, ONLY OF P&L ACCOUNTS, DEFAULTS TO INCLUDE
      *            P&L. ADDED PER JR
      * BAH 010320 ADDED BRANCH SECURITY PASSWORD TESTING (BRSECURE)
      * JTG 041101 *NEW*
      *            PROGRAMMED UNEARNED FEE CODE REPORT             LENDMARK 2387
      * JTG 050113 ADDED 'LOAN ENTERED START DATE' TO REPORT       LENDMARK 2444
      *  CS 050317 TEST ON ZBAL NEEDS TO GO AWAY.  PAID OUTS STILL HAVE
      *            REMAINING UNEARNED (BECAUSE THERE IS NO REBATE) & TRANSFERS
      *            REFLECTED IN MULTIPLE BRANCHES, BUT JIM USED THIS OPTION
      *            TO FIND THE PROBLEM... SO I JUST MADE IT SKIP THE FIELD &
      *            MUST UP ARROW TO GET TO IT UNTIL LENDMARK SAYS ITS OK
      *                                                      [PR#2487 LENDMARK]
      *  BAH 170531 ACTIVATE 8 DIGIT DATES, PD0006
      *  BAH 20200827 REMOVED RESET ON BR1 
      **************************************************************************
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
       77  SCCS-IDENTIFIER PIC X(55) VALUE
           "@(#)LP/FEUNRP A30 07/02/14-08:51:03 09/18/14-13:49:54 >".

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
       01  FIRST-TIME          PIC X     VALUE "Y".

      ******************************************************************
      *
      *     L I N E - W O R K E R S
      *
      ******************************************************************
       01  LN-FEED          PIC 99       VALUE 0.

      ******************************************************************
      *
      *     S C R E E N   F I E L D S
      *
      ******************************************************************
       COPY "LIBGB/EOBUF_EXT.CPY".
       COPY "LIBCL/FEUNRPW.CPY".

       01  VERSION-CONTROL        PIC X    VALUE "A".

      ******************************************************************
      *
      *     C O P Y   L I B R A R Y    W O R K E R S
      *
      ******************************************************************
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBWI/LCSCANW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/FEUNRP_DEF.CPY".
       COPY "LIBLP/FEUNRP_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBLP/FEUNRP_SCN.CPY".

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

           PERFORM SQL-CONNECT.

           MOVE "FEUNRP" TO VDU-FORM-NAME.

           PERFORM INITIALIZATION.

           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

           PERFORM RUN-REPORT.

           GO TO END-PROGRAM.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************
           MOVE FEUNRP-QUEUE-POS TO Q-POS.
           MOVE FEUNRP-COPIES-POS TO C-POS.
           MOVE FEUNRP-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

           MOVE "Y" TO FIRST-TIME.
           MOVE "N" TO ZBAL-ONLY.
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

           IF EXT-ROUTE-BUF = "00"
      * SET REPORT ENDING DATE:
              MOVE MM TO DATE-MMDDYY-MM
              MOVE 33 TO DATE-MMDDYY-DD
              MOVE YY TO DATE-MMDDYY-YY
              PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
              MOVE 33 TO DATE-YYYYMMDD-DD
              MOVE DATE-YYYYMMDD TO NDTE-DATE
              PERFORM NEW-DATE-CALCULATION
              MOVE NDTE-DATE TO REPORT-ENDING-DATE
      *  SET PRIOR MONTH ENDING DATE:
              MOVE 33 TO NDTE-DD
              MOVE -1 TO NDTE-HOLD
              PERFORM INCREMENT-MONTHS
              MOVE NDTE-DATE TO PRIOR-ENDING-DATE.

           GO TO INITIALIZE-EXIT.

       INITIALIZE-ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.

       INITIALIZE-EXIT.
           EXIT.

      ******************************************************************
       EO-EXEC-AUDIT SECTION.
      ******************************************************************

           MOVE EXT-EOBUF-DATE2 TO NDTE-DATE.
           MOVE NDTE-MM TO MM.
           MOVE 33 TO NDTE-DD.
           MOVE NDTE-YY TO YY.
           PERFORM NEW-DATE-CALCULATION.
           MOVE NDTE-DATE TO REPORT-ENDING-DATE.

      * GET PRIOR MONTH ENDING DATE:
           MOVE 33 TO NDTE-DD.
           MOVE -1 TO NDTE-HOLD.
           PERFORM INCREMENT-MONTHS.
           MOVE NDTE-DATE TO PRIOR-ENDING-DATE.

      ******************************************************************
       ENTER-PARAMETERS SECTION.
      ******************************************************************
       ENTER-PARAM.

      * FOR RE-ENTRY ON F4 FROM ACCEPT.

           IF VDUSTATUS = "1U" GO TO SO-01.

      *----------------------------------------------------------------
       FEE-1.
           MOVE "E  A020FEE1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO FEE-1.
           MOVE VDUBUF TO FEE-CODE(1).
           IF FEE-CODE(1) = " "
              MOVE "MUST ENTER A FEE CODE" TO MESS
              PERFORM SEND-MESS
              GO TO FEE-1.

       FRMLA-1.
           MOVE "ENNN020FRMLA1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO FEE-1.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO FRMLA-1.
           MOVE VDUNUM0 TO REBATE-METHOD(1).
           IF NOT (REBATE-METHOD(1) = 1 OR 2 OR 6 OR 7)
              MOVE "MUST ENTER AN EARN METHOD (1, 2, 6, 7)"
                                                           TO MESS
              PERFORM SEND-MESS
              GO TO FRMLA-1.

      *----------------------------------------------------------------
       FEE-2.
           MOVE "E  A020FEE2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO FRMLA-1.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO FEE-2.
           MOVE VDUBUF TO FEE-CODE(2).
           IF FEE-CODE(2) = " "
              MOVE " " TO FEE-CODE(2)
                          FEE-CODE(3)
              MOVE 0 TO REBATE-METHOD(2)
                        REBATE-METHOD(3)
              MOVE "S  A020FEE2." TO VDU
              PERFORM SCREENIO
              MOVE "S  A020FRMLA2." TO VDU
              PERFORM SCREENIO
              MOVE "S  A020FEE3." TO VDU
              PERFORM SCREENIO
              MOVE "S  A020FRMLA3." TO VDU
              PERFORM SCREENIO
              GO TO BR-01.

       FRMLA-2.
           MOVE "ENNN020FRMLA2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO FEE-2.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO FRMLA-2.
           MOVE VDUNUM0 TO REBATE-METHOD(2).
           IF NOT (REBATE-METHOD(2) = 1 OR 2 OR 6 OR 7)
              MOVE "MUST ENTER AN EARN METHOD (1,2, 6, 7)"
                                                           TO MESS
              PERFORM SEND-MESS
              GO TO FRMLA-2.

      *----------------------------------------------------------------
       FEE-3.
           MOVE "E  A020FEE3." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO FRMLA-2.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO FEE-3.
           MOVE VDUBUF TO FEE-CODE(3).
           IF FEE-CODE(3) = " "
              MOVE " " TO FEE-CODE(3)
              MOVE 0   TO REBATE-METHOD(3)
              MOVE "S  A020FEE3." TO VDU
              PERFORM SCREENIO
              MOVE "S  A020FRMLA3." TO VDU
              PERFORM SCREENIO
              GO TO BR-01.

       FRMLA-3.
           MOVE "ENNN020FRMLA3." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO FEE-3.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO FRMLA-3.
           MOVE VDUNUM0 TO REBATE-METHOD(3).
           IF NOT (REBATE-METHOD(3) = 1 OR 2 OR 6 OR 7)
              MOVE "MUST ENTER AN EARN METHOD (1, 2, 6, 7)"
                                                           TO MESS
              PERFORM SEND-MESS
              GO TO FRMLA-3.

      *----------------------------------------------------------------
       BR-01.
           MOVE "E  A000BR1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-BR.
           IF VDUSTATUS = "1U" GO TO FEE-1.

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
       LC-01.
           MOVE "ENNN020LC1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-LC.
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF VDUSTATUS = "1<"
              MOVE "BEG" TO CLASS-FG
              PERFORM CLASS-SCAN
              IF IO-FG NOT = 0
              GO TO LC-01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO LC-01.
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
           IF BEG-LC > END-LC
              PERFORM INVALID-RANGE
              GO TO LC-01.
           GO TO DT-01.

      *----------------------------------------------------------------
       ALL-LC.
           MOVE "SNNN020LC1." TO VDU.
           MOVE 1 TO BEG-LC VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020LC2." TO VDU.
           MOVE 99 TO END-LC VDUNUM0.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
       DT-01.
           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO ENTER-START.

           MOVE "ENNN040DT1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO LC-01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO DT-01.
           MOVE VDUNUM0 TO MMYY.

           IF MM < 1 OR MM > 12
              MOVE "INVALID MONTH" TO MESS
              PERFORM SEND-MESS
              GO TO DT-01.

      *----------------------------------------------------------------
       ENTER-START.
           MOVE "E NM060ENTERED." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U"
              IF EXT-ROUTE-BUF = "40" OR "50"
                 GO TO LC-02
              ELSE
                 GO TO DT-01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ENTER-START.
           MOVE VDU-DATE-YYYYMMDD TO ENTERED-START-DATE.

      *----------------------------------------------------------------
       ZBAL-01.
           IF FIRST-TIME = "Y"
              MOVE "N" TO FIRST-TIME
              GO TO PL-01.

           MOVE "E  A010ZBAL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U"
              GO TO ENTER-START.

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
           MOVE VDUBUF TO ACCOUNT-DETAIL.
           IF ACCOUNT-DETAIL = "Y" GO TO SO-01.
           IF ACCOUNT-DETAIL NOT = "N" GO TO AD-01.

      *----------------------------------------------------------------
       SO-01.
           MOVE "E  A010SO1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO AD-01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO SO-01.
           MOVE VDUBUF TO SUMMARY-ONLY.
           IF NOT (SUMMARY-ONLY = "Y" OR "N") GO TO SO-01.

       EXIT-PARM.
           EXIT.

      ******************************************************************
       DISPLAY-PARAMETERS SECTION.
      ******************************************************************
           IF FEE-CODE(1) NOT = " "
              MOVE "S  A000FEE1." TO VDU
              MOVE FEE-CODE(1) TO VDUBUF
              PERFORM SCREENIO
              MOVE "SNNN020FRMLA1." TO VDU
              MOVE REBATE-METHOD(1) TO VDUNUM0
              PERFORM SCREENIO.
           IF FEE-CODE(2) NOT = " "
              MOVE "S  A000FEE2." TO VDU
              MOVE FEE-CODE(2) TO VDUBUF
              PERFORM SCREENIO
              MOVE "SNNN020FRMLA2." TO VDU
              MOVE REBATE-METHOD(2) TO VDUNUM0
              PERFORM SCREENIO.
           IF FEE-CODE(3) NOT = " "
              MOVE "S  A000FEE3." TO VDU
              MOVE FEE-CODE(3) TO VDUBUF
              PERFORM SCREENIO
              MOVE "SNNN020FRMLA3." TO VDU
              MOVE REBATE-METHOD(3) TO VDUNUM0
              PERFORM SCREENIO.
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
           MOVE "SNNN020LC1." TO VDU.
           MOVE BEG-LC TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN020LC2." TO VDU.
           MOVE END-LC TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  8080ENTERED." TO VDU.
           MOVE ENTERED-START-DATE TO VDU-DATE-YYYYMMDD.
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
           MOVE "S  A010SO1." TO VDU.
           MOVE SUMMARY-ONLY TO VDUBUF.
           PERFORM SCREENIO.

      ******************************************************************
       RUN-REPORT SECTION.
      ******************************************************************

           MOVE "CL/FEUNR1" TO FORM-NAM.
           CALL FORM-PROGX   USING FORM-PATHNAME PRU-PATH EXIT-PATHNAME.
           CANCEL FORM-PROGX.

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
           MOVE EXT-CONAME-CONAME TO FEUNRP-CONAME.
           MOVE EXT-CONAME-SYSDATE TO FEUNRP-SYSDATE.
           DISPLAY FEUNRP-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE FEUNRP-FORM-FIELDS TO WR-BUF.
           MOVE FEUNRP-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       FORM-RESET-EXIT.
           EXIT.

      *********************************************************
       SETUP-LINK-INFO SECTION.
      *********************************************************
           MOVE FEUNRP-HELP--FILE TO HELP-LINK-FILE.
           MOVE FEUNRP-HELP--DIR  TO HELP-LINK-DIR.

      *********************************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/FEUNRP_EVA.CPY".
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

      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************
       EXIT-PROG.
           PERFORM CLOSE-FILES.

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
              DESTROY FEUNRP-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
