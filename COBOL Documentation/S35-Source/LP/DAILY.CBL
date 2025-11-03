       IDENTIFICATION DIVISION.
       PROGRAM-ID.      DAILY.
       DATE-WRITTEN. 121493.
      **************************************************************************
      *                 (LP)
      *  DESCRIPTION:  DAILY MISPAY DELINQUENCY CONTRACTUAL UPDATE
      *                 IF TODAYS DATE MATCHES ANY OF THE GP FISCAL DATES,
      *                 IT WILL NOT RUN. TO PREVENT FROM RUNNING ON THE
      *                 LAST DAY OF THE MONTH.
      *
      * REV:
      *  JTG 012694 CORRECT UPDATE OF MISPAYS
      *  JTG 022694 FIXED TO SKIP PROCESSING IF END OF MONTH
      *  JTG 032194 CORRECTED MISPAY CALC WHEN ACCOUNTS BOOKED THIS
      *             MONTH MADE A PAYMENT
      *  JTG 041994 CORRECTED MISPAY CALC WHEN ACCOUNTS BOOKED THIS
      *             MONTH WERE P&L'ED
      *  JTG 050594 CORRECTED MISPAY CALC WHEN ACCOUNTS BOOKED THIS
      *             MONTH WERE CONVERTED
      *  KEC 091096 FIXED PRINTING "NO REPORT GENERATED" WHEN EXITING.
      *  JTG 091897 MOVE FROM A90 TO A10
      *  MJD 030698 CHANGED TO CREATE DSFILE IN ONE DOES NOT EXIST.
      *  JTG 041598 CHANGED THE DEFAULT TO REBUILD RECENCY DELINQUENCY STATS
      *  JTG 052698 ADDED LOGIC TO SKIP REPO ACCOUNTS AND NOT INCLUDE OT2 MONEYS
      *             IN UPDATE OF DELINQUENCY
      *  JTG 061598 ADDED LOGIC TO UPDATE BANKRUPT DELINQUENT STATISTICS
      *  MJD 990416 CHANGED TO INCLUDE/EXCLUDE O2 IN DELINQ BASED
      *             ON GP-DELINQ-WITH-O2
      *  JTG 990720 ADDED LOGIC FOR LN-ACTIVE-FG     REGENCY REVOLVING #45
      *  GLF 990924 REPLACED IO-FG NUMBERS WITH IO-FG 88 LEVEL PER DEVELOPMENT
      *             MEETING (082599)
      *  BLM 080722 ADD UNLOCKS TO CLEAR-BY ROUTINE; IN CLEAR ROUTINE WHEN
      *             BRANCH # CHANGED THE CURRENT RECORD FOR THE NEXT BRANCH
      *             WAS LOCKED UNTIL THE UPDATE ROUTINE FOUND THE FIRST LOAN
      *             TO UPDATE & READ BY.  IF NEXT BRANCH RUNNING END-OF-DAY
      *             AT THE SAME TIME THEY GOT BY RECORD LOCKED.  WORLD.
      *  BAH 160125 SPLIT OUT LTFILE, PD0003
      *  BLM 160314 FIX FOR NEW BY-DATE FORMAT, PD0003
      *  BLM 160512 FIX FOR NEW DS-DATE FORMAT, PD0003
      *
      * KEC 2016.0929 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED BY-DATE TO BY-DATE-CCYYMM FOR CONSISTENCY.
      *
      * KEC 2017.0105 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          CHANGED DS-DATE TO DS-DATE-CCYYMM FOR CONSISTENCY
      *          WITH THE DATE FIELDS IN BY & UD FILE.
      *
      * 20171030 BAH ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 181207 GLOBAL LNFILE
      * KEC 190212 A32 F2SL - REVISED FOR SPLITTING THE BYFILE.
      *            REMOVED LOAN APP FIELDS IN BY & TY.
      * BAH 20190625 REMOVED LN-REVOLVING, PT0153
      * BAH 20191014 AND CLS, WRONG DATE FOR STATS WHEN BACKDATING IN 
      *              DAILY PROCESSING, SET TODAY-IS-DATE CORRECTLY
      * RMZ 20231113 PERFORMANCE TUNING - ADDED STORED PROCEDURE
      * RMZ 20240806 ADDED LTOUCH-DATE TO UPDATE SCRIPTS. (S35Q-108)
      * BAH 20250317 TODAY-IS-DATE WAS NOT SET CORRECTLY WHEN RUNNING FROM
      *              THE MENU, BATCH WAS CORRECT. THE MENU VERSION WAS 
      *              MOVING AN ALP-DATE TO A 9(8) COMP DATE. S35Q-178
      * RMZ 20250317 COMMENTED OUT ALL DSFILE CALLS/UPDATES. CORRECTED
      *              BUGS DURING UPDATE TESTING. (S35Q-167)
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".

      ******************************************
      *    WORKING STORAGE SECTION
      ******************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(36) VALUE
           "@(#)LP/DAILY S35 03/17/25-10:50:15 >".
       COPY "LIBLP/LP01DS.CPY".
       COPY "LIBLP/LP01DS_SQL.CPY".
       COPY "LIBLP/LPWSDS.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01BY.CPY".
       COPY "LIBLP/LP01BYA.CPY".
       COPY "LIBLP/LP01BYB.CPY".
       COPY "LIBLP/LP01BY_SQL.CPY".
       COPY "LIBLP/LPWSBY.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBLP/LP01LTI.CPY".
       COPY "LIBLP/LP01LTI_SQL.CPY".
       COPY "LIBLP/LPWSLTI.CPY".
       COPY "LIBLP/LP01LTP.CPY".
       COPY "LIBLP/LP01LTP_SQL.CPY".
       COPY "LIBLP/LPWSLTP.CPY".
       01  PR-IFN            PIC X(2)     VALUE "PR".
       01  PR-OPEN-FG        PIC X        VALUE " ".
   
       01  LN1-CONN-OPEN-SW  PIC X        VALUE "N".

      * IF YOU BACKDATE IN DAILY PROCESSING, THIS WILL BE YOUR DATE
       01  TODAY-IS-DATE            PIC 9(8)     COMP.
       01  TODAYS-BY-DATE-CCYYMM    PIC 9(6)     COMP.
       01  EOM-DATE                 PIC 9(8)     COMP.
       01  FIX-RECENCY-FG           PIC X    VALUE " ".

       01  RECORDS-FOUND        PIC X        VALUE " ".
           88  NO-RECORDS-FOUND              VALUE "N".
           88  SKIP-RECORDS-FOUND            VALUE " ".

      ***********************
      *    LINE WORKERS
      ***********************
       01  LN-FEED       PIC 999   COMP  VALUE 0.
       01  LN-COUNT      PIC 999   COMP  VALUE 99.
       01  PAGE-NUMBER   PIC 9999  COMP  VALUE 0.

      ***********************************
      *     P R I N T   B U F F E R S
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
           03  H-USERID         PIC X(12).
           03  FILLER           PIC X(14)    VALUE " ".
           03  FILLER           PIC X(50)    VALUE
           "DELINQUENCY / UNEARNED DEALER UPDATE EXCEPTIONS".

       01  HEAD3.
           03  FILLER      PIC X(18)    VALUE
               "ACCOUNT NO.".
           03  FILLER      PIC X(50)    VALUE
               "DESCRIPTION OF PROBLEM".
           03  FILLER      PIC X(50)    VALUE
               "ACTION TAKEN".

       01  DETAIL-LINE     PIC X(132) VALUE SPACES.
       01  FILLER          REDEFINES DETAIL-LINE.
           03  D-ACCTNO    PIC 999999.99.
           03  FILLER      PIC X(9).
           03  D-ERROR      PIC X(50).
           03  D-ACTION    PIC X(50).

      ******************************************
      *    SCREEN FIELDS
      ******************************************
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  AGED-DATE     PIC 9(8).

       01  VERSION-CONTROL   PIC X    VALUE "A".

       01  MESS-LIST         PIC X(5) VALUE "MESS.".

       01  HOLD-DS1-KEY          PIC X(19)    VALUE SPACES.
       01  HOLD-DS-DATE-CCYYMM   PIC 9(6).
       01  SAVE-OUTBAL           PIC S9(7)V99 VALUE 0.
       01  SUB                   PIC 9(6)     COMP.

       COPY "LIBGB/DATERW.CPY".
       COPY "LIBLP/LPUPERW.CPY".
       COPY "LIBLP/LPUPWKW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBLP/LPCEPPW.CPY".
       COPY "LIBLP/LPITRMW.CPY".
       COPY "LIBGB/AGEINGW.CPY".
       COPY "LIBLP/LPPDUEW.CPY".
       COPY "LIBGB/SPPDTHW.CPY".
       COPY "LIBGB/REBATEW.CPY".
       COPY "LIBLP/LPAPRZW.CPY".
       COPY "LIBLP/LPAPRSW.CPY".
       COPY "LIBLP/LPAMTSW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBLP/LPEARNW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/RBACTW.CPY".
       COPY "LIBLP/SETCTBLW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBLP/LPBYCLW.CPY".
       COPY "LIBLP/LPDSCLW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/DAILY_DEF.CPY".
       COPY "LIBLP/DAILY_WKS.CPY".
       COPY "LIBLP/ARRAYSPW.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBLP/DAILY_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
              PR-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LTP1-FILE.
           PERFORM CLOSE-LTI1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-LN1-CONNECTION.
           PERFORM CLOSE-DS1-FILE.
           PERFORM CLOSE-BY1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM SQL-CLOSE-UPDATE-CONNECTION.
           PERFORM CLOSE-PR-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ******************************************
      *       MAIN PROGRAM CONTROL
      ******************************************
       MAIN-PROGRAM SECTION.


           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "DAILY" TO VDU-FORM-NAME.

           MOVE "Y" TO FIX-RECENCY-FG.

           PERFORM INITIALIZATION.

           MOVE "N" TO RECORDS-FOUND.

           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

           PERFORM GET-GPENV.

           MOVE EXT-CONAME-USER-LOC TO BR-NO.
           PERFORM GET-BR.
           IF IO-BAD
              MOVE "MISSING BRANCH RECORD" TO D-ERROR
              MOVE "DAILY UPDATE SKIPPED" TO D-ACTION
              PERFORM PRINT-EXCEPTION
              GO TO END-PROGRAM.

      *    PERFORM OPEN-SP1-FILE.
           PERFORM ARRAYSP-LOAD.

      * SKIP PROCESSING DAILY IF ITS THE LAST DAY OF THE MONTH:
           MOVE TODAY-IS-DATE TO NUM-DATE.
           PERFORM FIND-FISCAL-PERIOD.
           IF SUB NOT = 0
              IF TODAY-IS-DATE = GP-END-FISCAL-DATE(SUB)
                 GO TO END-PROGRAM.

           PERFORM CLEAR-BYFILE.
      *    PERFORM CLEAR-TODAYS-DSFILE.
           PERFORM UPDATE-BY-DS-STATS.

           GO TO END-PROGRAM.

      *****************************************************
       INITIALIZATION SECTION.
      *****************************************************
           MOVE DAILY-QUEUE-POS TO Q-POS.
           MOVE DAILY-COPIES-POS TO C-POS.
           MOVE DAILY-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

      * IF RUNNING FROM A $ SIGN OR MENU, USE THE SYSTEM DATE, IF NOT,
      * YOU ARE RUNNING FROM EOD (LONPR0), AND THE AUDIT SECTION WILL
      * SET TODAY-IS-DATE WITH YOUR TRANSACTION DATE IN CASE OF BACKDATING.
      * KM ACTUAL DATE WAS A DAY IN OCTOBER, HE WENT BACK TO SEPT AND 
      * UPDATED FOR A DAY IN SEPT.  JUST SO HAPPENS, HAD NO STATS FOR
      * OCTOBER, SO PRODUCED A REPORT WITH ERRORS BUT IT WOULD HAVE AFFECTED
      * BOTH BRANCH /DEALER STATS.
           IF EXT-ROUTE-BUF = "00"
              MOVE EXT-CONAME-SYSDATE TO ALP-DATE
              PERFORM CONVERT-ALPDATE-TO-YYYYMMDD
              MOVE DATE-YYYYMMDD TO TODAY-IS-DATE.

      * SETUP EOM-DATE, AS LAST DAY OF LAST MONTH:
           MOVE TODAY-IS-DATE TO NDTE-DATE.
           MOVE 35            TO NDTE-DD.
           MOVE -1            TO NDTE-HOLD.
           PERFORM INCREMENT-MONTHS.
           MOVE NDTE-DATE TO EOM-DATE.

      * SET AGEING DATE, GET MISPAYS RE: TOMMORROW:
           MOVE TODAY-IS-DATE TO NDTE-DATE.
           MOVE 1             TO NDTE-HOLD.
           PERFORM INCREMENT-DAYS.
           MOVE NDTE-DATE TO AGED-DATE
                             AGEING-DATE.

           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           PERFORM SQL-OPEN-UPDATE-CONNECTION.

      *****************************************************
       EO-EXEC-AUDIT SECTION.
      *****************************************************
           MOVE EXT-EOBUF-DATE2 TO TODAY-IS-DATE.

      *****************************************************
       ENTER-PARAMETERS SECTION.
      *****************************************************

       EXIT-PARM.
           EXIT.

      *****************************************************
       DISPLAY-PARAMETERS SECTION.
      *****************************************************

      *****************************************************
       CLEAR-BYFILE SECTION.
      *****************************************************
           PERFORM OPEN-BY1-FILE.

           MOVE TODAY-IS-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-YYYYMM.
           MOVE DATE-YYYYMM TO BY-DATE-CCYYMM TODAYS-BY-DATE-CCYYMM
                               QBY1-WBEG-DATE-CCYYMM
                               QBY1-WEND-DATE-CCYYMM.

           MOVE EXT-CONAME-USER-LOC TO BY-BRNO
                                       QBY1-WBEG-BRNO
                                       QBY1-WEND-BRNO.
         
           MOVE 0                   TO BY-CLASS
                                       QBY1-WBEG-CLASS.
           MOVE ALL "9"             TO QBY1-WEND-CLASS.

           PERFORM REWRITE-BY1-FILE-CLEAR-BYCON. *> EMBEDDED - BYFILE

           IF FIX-RECENCY-FG = "Y"
              PERFORM REWRITE-BY1-FILE-CLEAR-BYREC. *> EMBEDDED - BYFILE

      *    PERFORM START-BY1-FILE.

      *CLEAR-BYFILE-NEXT.
      *    PERFORM READ-BY1-FILE-NEXT.
      *    IF IO-BAD
      *       GO TO CLEAR-BYFILE-EXIT.

      *    IF BY-DATE-CCYYMM NOT = TODAYS-BY-DATE-CCYYMM
      *       UNLOCK BYA-FILE     *> F2SL
      *       UNLOCK BYB-FILE     *> F2SL
      *       GO TO CLEAR-BYFILE-EXIT.

      *    IF BY-BRNO NOT = EXT-CONAME-USER-LOC
      *       UNLOCK BYA-FILE     *> F2SL
      *       UNLOCK BYB-FILE     *> F2SL
      *       GO TO CLEAR-BYFILE-EXIT.

      * CLEAR BYNOW, CONTRACTUAL ONLY:
      *    PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 10
      *        MOVE 0 TO BY-UNITS        (BYNOW BYCON SUB)
      *                  BY-OUTBAL       (BYNOW BYCON SUB)
      *        MOVE 0 TO BY-UNITS-BNKRPT (BYNOW BYCON SUB)
      *                  BY-OUTBAL-BNKRPT(BYNOW BYCON SUB)
      *    END-PERFORM.


      *-----------------------------------------------------
      *    RECENCY TEST MODE LOGIC ONLY
      *-----------------------------------------------------
      * CLEAR RECENCY:
      *    IF FIX-RECENCY-FG = "Y"
      *       PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 10
      *           MOVE 0 TO BY-UNITS        (BYNOW BYREC SUB)
      *                     BY-OUTBAL       (BYNOW BYREC SUB)
      *           MOVE 0 TO BY-UNITS-BNKRPT (BYNOW BYREC SUB)
      *                     BY-OUTBAL-BNKRPT(BYNOW BYREC SUB)
      *       END-PERFORM.


      *    PERFORM REWRITE-BY1-FILE.
      *    GO TO CLEAR-BYFILE-NEXT.


       CLEAR-BYFILE-EXIT.
           EXIT.

      *****************************************************
       CLEAR-TODAYS-DSFILE SECTION.
      *****************************************************
           PERFORM OPEN-DS1-FILE.

           MOVE TODAY-IS-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-YYYYMM.
           MOVE DATE-YYYYMM TO DS-DATE-CCYYMM HOLD-DS-DATE-CCYYMM
                               QDS1-WBEG-DATE-CCYYMM
                               QDS1-WEND-DATE-CCYYMM.
           MOVE EXT-CONAME-USER-LOC TO DS-BRANCH
                                       QDS1-WBEG-BRANCH
                                       QDS1-WEND-BRANCH.
           MOVE 0                   TO DS-DLNO
                                       DS-CLASS
                                       QDS1-WBEG-NUM
                                       QDS1-WBEG-CLASS.
           MOVE ALL "9"             TO QDS1-WEND-NUM
                                       QDS1-WEND-CLASS.

           PERFORM REWRITE-DS1-FILE-CLEAR-BYCON. *> EMBEDDED - DSFILE

           IF FIX-RECENCY-FG = "Y"
              PERFORM REWRITE-DS1-FILE-CLEAR-BYREC. *> EMBEDDED - DSFILE
  
      *    PERFORM START-DS1-FILE.

      *CLEAR-TODAYS-DSFILE-NEXT.
      *    PERFORM READ-DS1-FILE-NEXT.
      *    IF IO-BAD 
      *       GO TO CLEAR-TODAYS-DSFILE-EXIT.

      *    IF DS-DATE-CCYYMM NOT = HOLD-DS-DATE-CCYYMM
      *       GO TO CLEAR-TODAYS-DSFILE-EXIT.
      *    IF DS-BRANCH NOT = EXT-CONAME-USER-LOC
      *       GO TO CLEAR-TODAYS-DSFILE-EXIT.

      * CLEAR DSNOW, CONTRACTUAL ONLY:
      *    PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 10
      *        MOVE 0 TO DS-UNITS        (DSNOW DSCON SUB)
      *                  DS-OUTBAL       (DSNOW DSCON SUB)
      *                  DS-UNITS-BNKRPT (DSNOW DSCON SUB)
      *                  DS-OUTBAL-BNKRPT(DSNOW DSCON SUB)
      *    END-PERFORM.


      *-----------------------------------------------------
      *    RECENCY TEST MODE LOGIC ONLY
      *-----------------------------------------------------
      * CLEAR RECENCY:
      *    IF FIX-RECENCY-FG = "Y"
      *       PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 10
      *           MOVE 0 TO DS-UNITS        (DSNOW DSREC SUB)
      *                     DS-OUTBAL       (DSNOW DSREC SUB)
      *                     DS-UNITS-BNKRPT (DSNOW DSREC SUB)
      *                     DS-OUTBAL-BNKRPT(DSNOW DSREC SUB)
      *       END-PERFORM.

      *-----------------------------------------------------

      *    PERFORM REWRITE-DS1-FILE.
      *    GO TO CLEAR-TODAYS-DSFILE-NEXT.

       CLEAR-TODAYS-DSFILE-EXIT.
           EXIT.

      *****************************************************
       UPDATE-BY-DS-STATS SECTION.
      *****************************************************
           IF EXT-ROUTE-BUF = "00"
              MOVE "DAILY MISSPAY DELINQUENCY UPDATE...." TO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           PERFORM OPEN-LN1-FILE.

           MOVE LN-PATH-OWNBR TO LN-OWNBR.
           MOVE 0             TO LN-ACCTNO.


           PERFORM START-LN1-FILE. *> SSP - LNFILE


       UPDATE-BY-DS-STATS-NEXT.

           PERFORM READ-LN1-FILE-NEXT. *> EMBEDDED - LNFILE
           IF IO-BAD OR (LN-OWNBR NOT = LN-PATH-OWNBR)
              GO TO UPDATE-BY-DS-STATS-EXIT.

           IF ( LN-POCD-NONLOAN )
                OR (LN-CURBAL = 0)
                 OR (LN-PLCD NOT = " ")
                  OR (LN-REPOCD = "X" OR "S")
              GO TO UPDATE-BY-DS-STATS-NEXT.

           IF LN-OWNBR NOT = EXT-CONAME-USER-LOC 
              GO TO UPDATE-BY-DS-STATS-NEXT.

           MOVE LN-ORGST    TO SP-ORGST.
           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           MOVE LN-LAWCODE  TO SP-LAWCODE.
           IF LPWSSP-SP1-KEY NOT = SP1-KEY
      *       PERFORM READ-SP1-FILE
              PERFORM ARRAYSP-READ
              IF IO-BAD
                 MOVE "MISSING STATE PARAMETER RECORD" TO D-ERROR
                 MOVE "LOAN SKIPPED" TO D-ACTION
                 PERFORM PRINT-EXCEPTION
                 GO TO UPDATE-BY-DS-STATS-NEXT
              ELSE
                 MOVE SP1-KEY TO LPWSSP-SP1-KEY.

           PERFORM COMPUTE-CONTRACTUAL.

      ******************************************************
      *    RECENCY FIX MODE LOGIC ONLY
      ******************************************************
           IF FIX-RECENCY-FG = "Y"
              PERFORM COMPUTE-RECENCY.

           IF GP-DELINQ-WITH-O2 = "Y"
              MOVE LN-CURBAL TO SAVE-OUTBAL
           ELSE
              SUBTRACT LN-OT2BAL FROM LN-CURBAL GIVING SAVE-OUTBAL.

      *    PERFORM UPDATE-DSFILE.
           PERFORM UPDATE-BYFILE.

           GO TO UPDATE-BY-DS-STATS-NEXT.

       UPDATE-BY-DS-STATS-EXIT.
           EXIT.

      ***********************************************************
       UPDATE-BYFILE SECTION.
      ***********************************************************

           MOVE TODAY-IS-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-YYYYMM.
           MOVE DATE-YYYYMM TO BY-DATE-CCYYMM TODAYS-BY-DATE-CCYYMM.
           MOVE LN-OWNBR TO BY-BRNO.
           MOVE LN-CLASS TO BY-CLASS.

           PERFORM READ-BY1-FILE. *> EMBEDDED - BYFILE
           IF IO-BAD
              MOVE "BRANCH STATISTICS RECORD MISSING" TO D-ERROR
              MOVE "LOAN SKIPPED" TO D-ACTION
              PERFORM PRINT-EXCEPTION
      *KC     UNLOCK BY-FILE      *> F2SL
      *       UNLOCK BYA-FILE     *> F2SL
      *       UNLOCK BYB-FILE     *> F2SL
              GO TO UPDATE-BYFILE-EXIT.

           ADD 1 TO BY-UNITS(BYNOW BYCON CSUB).
           ADD SAVE-OUTBAL TO BY-OUTBAL(BYNOW BYCON CSUB).
           IF LN-BNKRPTDATE NOT = 0
              ADD 1  TO BY-UNITS-BNKRPT(BYNOW BYCON CSUB)
              ADD SAVE-OUTBAL TO BY-OUTBAL-BNKRPT(BYNOW BYCON CSUB).

      *----------------------------------------------------------
      *    RECENCY FIX MODE LOGIC ONLY
      *----------------------------------------------------------
           IF FIX-RECENCY-FG = "Y"
              ADD 1 TO BY-UNITS(BYNOW BYREC RDSUB)
              ADD SAVE-OUTBAL TO BY-OUTBAL(BYNOW BYREC RDSUB)
              IF LN-BNKRPTDATE NOT = 0
                 ADD 1 TO BY-UNITS-BNKRPT(BYNOW BYREC RDSUB)
                 ADD SAVE-OUTBAL
                        TO BY-OUTBAL-BNKRPT(BYNOW BYREC RDSUB).

           PERFORM REWRITE-BY1-FILE. *> EMBEDDED - BYFILE

       UPDATE-BYFILE-EXIT.
           EXIT.

      ***********************************************************
       UPDATE-DSFILE SECTION.
      ***********************************************************

           MOVE TODAY-IS-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-YYYYMM.
           MOVE DATE-YYYYMM TO DS-DATE-CCYYMM.
           MOVE LN-OWNBR TO DS-BRANCH.
           MOVE LN-DLNO TO DS-DLNO.
           MOVE LN-CLASS TO DS-CLASS.

           PERFORM READ-DS1-FILE. *> EMBEDDED - DSFILE
           IF IO-BAD
              MOVE DS1-KEY TO HOLD-DS1-KEY
              PERFORM CLEAR-DS-FILE
              MOVE HOLD-DS1-KEY TO DS1-KEY
              PERFORM WRITE-DS1-FILE *> EMBEDDED - DSFILE
              IF IO-BAD
                 MOVE "DEALER STATISTICS RECORD MISSING" TO D-ERROR
                 MOVE "LOAN SKIPPED" TO D-ACTION
                 PERFORM PRINT-EXCEPTION
      *          UNLOCK DS-FILE
                 GO TO UPDATE-DSFILE-EXIT.

           ADD 1           TO DS-UNITS(DSNOW DSCON CSUB).
           ADD SAVE-OUTBAL TO DS-OUTBAL(DSNOW DSCON CSUB).
           IF LN-BNKRPTDATE NOT = 0
              ADD 1           TO DS-UNITS-BNKRPT(DSNOW DSCON CSUB)
              ADD SAVE-OUTBAL TO DS-OUTBAL-BNKRPT(DSNOW DSCON CSUB).

      *----------------------------------------------------------
      *    RECENCY FIX MODE LOGIC ONLY
      *----------------------------------------------------------
           IF FIX-RECENCY-FG = "Y"
              ADD 1           TO DS-UNITS(DSNOW DSREC RDSUB)
              ADD SAVE-OUTBAL TO DS-OUTBAL(DSNOW DSREC RDSUB)
              IF LN-BNKRPTDATE NOT = 0
                 ADD 1 TO DS-UNITS-BNKRPT(DSNOW DSREC RDSUB)
                 ADD SAVE-OUTBAL
                     TO DS-OUTBAL-BNKRPT(DSNOW DSREC RDSUB).

           PERFORM REWRITE-DS1-FILE. *> EMBEDDED - DSFILE

       UPDATE-DSFILE-EXIT.
           EXIT.


      ****************************************************
      *    FIND FISCAL END OF PERIOD
      *
      *  1.  SET NUM-DATE = TODAY'S DATE
      *  2.  SUBSCRIPT TO GP-GL-END-FISCAL IS IN SUB
      *  3.  IF NO MATCH SUB = 0
      ****************************************************
       FIND-FISCAL-PERIOD SECTION.
           MOVE 1 TO SUB.
       FIND-FISCAL-PERIOD-AGAIN.
           IF SUB > 12
              MOVE 0 TO SUB
           ELSE
              MOVE GP-END-FISCAL-DATE(SUB) TO SYS-DATE
              IF NUM-MO NOT = S-MM
                 ADD 1 TO SUB
                 GO TO FIND-FISCAL-PERIOD-AGAIN.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/ARRAYSP.CPY".
       COPY "LIBLP/LPUPER.CPY".
       COPY "LIBLP/LPUPWK.CPY".
       COPY "LIBLP/LPCEPP.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBLP/LPITRM.CPY".
       COPY "LIBGB/AGEING.CPY".
       COPY "LIBLP/LPPDUE.CPY".
       COPY "LIBGB/SPPDTH.CPY".
       COPY "LIBGB/REBATE.CPY".
       COPY "LIBGB/RBACT.CPY".
       COPY "LIBLP/LPAPRZ.CPY".
       COPY "LIBLP/LPAPRS.CPY".
       COPY "LIBLP/LPAMTS.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBLP/LPEARN.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBLP/SETCTBL.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBLP/LPDSCL.CPY".

      *********************************************************
       FORM-RESET SECTION.
      *********************************************************
           MOVE EXT-CONAME-CONAME TO DAILY-CONAME.
           MOVE EXT-CONAME-SYSDATE TO DAILY-SYSDATE.
           DISPLAY DAILY-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE DAILY-FORM-FIELDS TO WR-BUF.
           MOVE DAILY-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *********************************************************
       SETUP-LINK-INFO SECTION.
      *********************************************************
           MOVE DAILY-HELP--FILE TO HELP-LINK-FILE.
           MOVE DAILY-HELP--DIR  TO HELP-LINK-DIR.

      *********************************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/DAILY_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      **************************************
       MISC-PARAGRAPHS SECTION.
      **************************************
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/GETPR.CPY".
       GET-BR.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.

      *******************************************
       PRINT-EXCEPTION SECTION.
      *******************************************
           IF PR-OPEN-FG NOT = "Y"
              MOVE "Y" TO PR-OPEN-FG
              PERFORM OPEN-PR-FILE
              PERFORM HEAD-PAGE.

           MOVE LN-ACCTNO TO D-ACCTNO.
           PERFORM WRITE-DETAIL-LINE.

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
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.
           MOVE HEAD3 TO PR-REC.
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
           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      *******************************************
       IO-ROUTINE SECTION.
      *******************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLTPGS_SQL.CPY".
       COPY "LIBLP/LPLTIGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPDSGS_SQL.CPY".
       COPY "LIBLP/LPBYGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBPRINT.CPY".

      * COPY "LIBLP/LPDS1IN.CPY".
      * COPYMEMBER: LIBLP/LPDS1IN
       LOAD-DS1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( DS-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( DS-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO DS-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO DS-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO DS-ALL-DATA
              MOVE DS-ALL-PATH             TO DS-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( DS-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO DS-ALL-BASE
              MOVE FILPATH-MACHINE         TO DS-ALL-MACHINE
              MOVE FILPATH-DATA            TO DS-ALL-DATA
              MOVE DS-ALL-PATH             TO DS-PATH.

      *-----------------------------------------------------------------
       OPEN-DS1-FILE.
           PERFORM LOAD-DS1-FILE.
           PERFORM OPEN-IT.
           MOVE DS-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-DS1-FILE.
           PERFORM START-IT.
           MOVE DS-PATH-OWNBR  TO DS-BRANCH.
           MOVE DS-PATH-SQL    TO E-FILE.
           MOVE DS1-KEY        TO E-KEYX.

           IF ( DS1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-DS1-FILE.

           IF ( QDS1-WEND-CLASS = ZEROES    ) OR
              ( QDS1-WEND-CLASS NOT NUMERIC )
              MOVE DS-PATH-OWNBR        TO QDS1-WBEG-BRANCH
              MOVE DS-PATH-OWNBR        TO QDS1-WEND-BRANCH
              MOVE DS-DATE-CCYYMM       TO QDS1-WBEG-DATE-CCYYMM
              MOVE ALL "9"              TO QDS1-WEND-DATE-CCYYMM
                                           QDS1-WEND-NUM
                                           QDS1-WEND-CLASS
              MOVE DS-NUM               TO QDS1-WBEG-NUM
              MOVE DS-CLASS             TO QDS1-WBEG-CLASS.


           EXEC SQL
            DECLARE DS1_CURSOR CURSOR FOR
            SELECT DSFILE.DS_DATE_CCYYMM,
                   DSFILE.DS_BRANCH,
                   DSFILE.DS_NUM,
                   DSFILE.DS_CLASS,
                   DSFILE.DS_DLRVPRIN_1,
                   DSFILE.DS_DLRVPRIN_2,
                   DSFILE.DS_DLRVPRIN_3,
                   DSFILE.DS_DLRVCHGS_1,
                   DSFILE.DS_DLRVCHGS_2,
                   DSFILE.DS_DLRVCHGS_3,
                   DSFILE.DS_DLPARVHELD,
                   DSFILE.DS_UNPAIDCK,
                   DSFILE.DS_DLDISC,
                   DSFILE.DS_DLPARVPAID,
                   DSFILE.DS_SUMWGTTM,
                   DSFILE.DS_SUMWGTAV,
                   DSFILE.DS_SUMWGTTA,
                   DSFILE.DS_RCOURSE_UNITS_1,
                   DSFILE.DS_RCOURSE_UNITS_2,
                   DSFILE.DS_RCOURSE_UNITS_3,
                   DSFILE.DS_RCOURSE_AMT_1,
                   DSFILE.DS_RCOURSE_AMT_2,
                   DSFILE.DS_RCOURSE_AMT_3,
                   DSFILE.DS_NPURUNIT_1,
                   DSFILE.DS_NPURUNIT_2,
                   DSFILE.DS_NPURUNIT_3,
                   DSFILE.DS_NPURAMT_1,
                   DSFILE.DS_NPURAMT_2,
                   DSFILE.DS_NPURAMT_3,
                   DSFILE.DS_OPURUNIT_1,
                   DSFILE.DS_OPURUNIT_2,
                   DSFILE.DS_OPURUNIT_3,
                   DSFILE.DS_OPURAMT_1,
                   DSFILE.DS_OPURAMT_2,
                   DSFILE.DS_OPURAMT_3,
                   DSFILE.DS_RNEWUNIT_1,
                   DSFILE.DS_RNEWUNIT_2,
                   DSFILE.DS_RNEWUNIT_3,
                   DSFILE.DS_RNEWAMT_1,
                   DSFILE.DS_RNEWAMT_2,
                   DSFILE.DS_RNEWAMT_3,
                   DSFILE.DS_PAIDUNIT_1,
                   DSFILE.DS_PAIDUNIT_2,
                   DSFILE.DS_PAIDUNIT_3,
                   DSFILE.DS_PAIDAMT_1,
                   DSFILE.DS_PAIDAMT_2,
                   DSFILE.DS_PAIDAMT_3,
                   DSFILE.DS_TODLUNIT_1,
                   DSFILE.DS_TODLUNIT_2,
                   DSFILE.DS_TODLUNIT_3,
                   DSFILE.DS_TODLAMT_1,
                   DSFILE.DS_TODLAMT_2,
                   DSFILE.DS_TODLAMT_3,
                   DSFILE.DS_OUTSUNIT_1,
                   DSFILE.DS_OUTSUNIT_2,
                   DSFILE.DS_OUTSUNIT_3,
                   DSFILE.DS_OUTSAMT_1,
                   DSFILE.DS_OUTSAMT_2,
                   DSFILE.DS_OUTSAMT_3,
                   DSFILE.DS_PLOSUNIT_1,
                   DSFILE.DS_PLOSUNIT_2,
                   DSFILE.DS_PLOSUNIT_3,
                   DSFILE.DS_PLOSAMT_1,
                   DSFILE.DS_PLOSAMT_2,
                   DSFILE.DS_PLOSAMT_3,
                   DSFILE.DS_RCPLUNIT_1,
                   DSFILE.DS_RCPLUNIT_2,
                   DSFILE.DS_RCPLUNIT_3,
                   DSFILE.DS_RCPLAMT_1,
                   DSFILE.DS_RCPLAMT_2,
                   DSFILE.DS_RCPLAMT_3,
                   DSFILE.DS_JDOSUNIT_1,
                   DSFILE.DS_JDOSUNIT_2,
                   DSFILE.DS_JDOSUNIT_3,
                   DSFILE.DS_JDOSAMT_1,
                   DSFILE.DS_JDOSAMT_2,
                   DSFILE.DS_JDOSAMT_3,
                   CAST(DSFILE.DS_DELUNEARN_DATE
                                                  AS VARCHAR(10)),
                   DSFILE.DS_PROCEEDS_1,
                   DSFILE.DS_PROCEEDS_2,
                   DSFILE.DS_NO_REPO_1_1_1,
                   DSFILE.DS_AMT_REPO_1_1_1,
                   DSFILE.DS_NO_REPO_1_1_2,
                   DSFILE.DS_AMT_REPO_1_1_2,
                   DSFILE.DS_NO_REPO_1_1_3,
                   DSFILE.DS_AMT_REPO_1_1_3,
                   DSFILE.DS_NO_REPO_1_1_4,
                   DSFILE.DS_AMT_REPO_1_1_4,
                   DSFILE.DS_NO_REPO_1_1_5,
                   DSFILE.DS_AMT_REPO_1_1_5,
                   DSFILE.DS_NO_REPO_1_2_1,
                   DSFILE.DS_AMT_REPO_1_2_1,
                   DSFILE.DS_NO_REPO_1_2_2,
                   DSFILE.DS_AMT_REPO_1_2_2,
                   DSFILE.DS_NO_REPO_1_2_3,
                   DSFILE.DS_AMT_REPO_1_2_3,
                   DSFILE.DS_NO_REPO_1_2_4,
                   DSFILE.DS_AMT_REPO_1_2_4,
                   DSFILE.DS_NO_REPO_1_2_5,
                   DSFILE.DS_AMT_REPO_1_2_5,
                   DSFILE.DS_NO_REPO_2_1_1,
                   DSFILE.DS_AMT_REPO_2_1_1,
                   DSFILE.DS_NO_REPO_2_1_2,
                   DSFILE.DS_AMT_REPO_2_1_2,
                   DSFILE.DS_NO_REPO_2_1_3,
                   DSFILE.DS_AMT_REPO_2_1_3,
                   DSFILE.DS_NO_REPO_2_1_4,
                   DSFILE.DS_AMT_REPO_2_1_4,
                   DSFILE.DS_NO_REPO_2_1_5,
                   DSFILE.DS_AMT_REPO_2_1_5,
                   DSFILE.DS_NO_REPO_2_2_1,
                   DSFILE.DS_AMT_REPO_2_2_1,
                   DSFILE.DS_NO_REPO_2_2_2,
                   DSFILE.DS_AMT_REPO_2_2_2,
                   DSFILE.DS_NO_REPO_2_2_3,
                   DSFILE.DS_AMT_REPO_2_2_3,
                   DSFILE.DS_NO_REPO_2_2_4,
                   DSFILE.DS_AMT_REPO_2_2_4,
                   DSFILE.DS_NO_REPO_2_2_5,
                   DSFILE.DS_AMT_REPO_2_2_5,
                   DSFILE.DS_NO_REPO_3_1_1,
                   DSFILE.DS_AMT_REPO_3_1_1,
                   DSFILE.DS_NO_REPO_3_1_2,
                   DSFILE.DS_AMT_REPO_3_1_2,
                   DSFILE.DS_NO_REPO_3_1_3,
                   DSFILE.DS_AMT_REPO_3_1_3,
                   DSFILE.DS_NO_REPO_3_1_4,
                   DSFILE.DS_AMT_REPO_3_1_4,
                   DSFILE.DS_NO_REPO_3_1_5,
                   DSFILE.DS_AMT_REPO_3_1_5,
                   DSFILE.DS_NO_REPO_3_2_1,
                   DSFILE.DS_AMT_REPO_3_2_1,
                   DSFILE.DS_NO_REPO_3_2_2,
                   DSFILE.DS_AMT_REPO_3_2_2,
                   DSFILE.DS_NO_REPO_3_2_3,
                   DSFILE.DS_AMT_REPO_3_2_3,
                   DSFILE.DS_NO_REPO_3_2_4,
                   DSFILE.DS_AMT_REPO_3_2_4,
                   DSFILE.DS_NO_REPO_3_2_5,
                   DSFILE.DS_AMT_REPO_3_2_5,
                   DSFILE.DS_UNITS_1_1_1,
                   DSFILE.DS_OUTBAL_1_1_1,
                   DSFILE.DS_UNITS_1_1_2,
                   DSFILE.DS_OUTBAL_1_1_2,
                   DSFILE.DS_UNITS_1_1_3,
                   DSFILE.DS_OUTBAL_1_1_3,
                   DSFILE.DS_UNITS_1_1_4,
                   DSFILE.DS_OUTBAL_1_1_4,
                   DSFILE.DS_UNITS_1_1_5,
                   DSFILE.DS_OUTBAL_1_1_5,
                   DSFILE.DS_UNITS_1_1_6,
                   DSFILE.DS_OUTBAL_1_1_6,
                   DSFILE.DS_UNITS_1_1_7,
                   DSFILE.DS_OUTBAL_1_1_7,
                   DSFILE.DS_UNITS_1_1_8,
                   DSFILE.DS_OUTBAL_1_1_8,
                   DSFILE.DS_UNITS_1_1_9,
                   DSFILE.DS_OUTBAL_1_1_9,
                   DSFILE.DS_UNITS_1_1_10,
                   DSFILE.DS_OUTBAL_1_1_10,
                   DSFILE.DS_UNITS_1_2_1,
                   DSFILE.DS_OUTBAL_1_2_1,
                   DSFILE.DS_UNITS_1_2_2,
                   DSFILE.DS_OUTBAL_1_2_2,
                   DSFILE.DS_UNITS_1_2_3,
                   DSFILE.DS_OUTBAL_1_2_3,
                   DSFILE.DS_UNITS_1_2_4,
                   DSFILE.DS_OUTBAL_1_2_4,
                   DSFILE.DS_UNITS_1_2_5,
                   DSFILE.DS_OUTBAL_1_2_5,
                   DSFILE.DS_UNITS_1_2_6,
                   DSFILE.DS_OUTBAL_1_2_6,
                   DSFILE.DS_UNITS_1_2_7,
                   DSFILE.DS_OUTBAL_1_2_7,
                   DSFILE.DS_UNITS_1_2_8,
                   DSFILE.DS_OUTBAL_1_2_8,
                   DSFILE.DS_UNITS_1_2_9,
                   DSFILE.DS_OUTBAL_1_2_9,
                   DSFILE.DS_UNITS_1_2_10,
                   DSFILE.DS_OUTBAL_1_2_10,
                   DSFILE.DS_UNITS_2_1_1,
                   DSFILE.DS_OUTBAL_2_1_1,
                   DSFILE.DS_UNITS_2_1_2,
                   DSFILE.DS_OUTBAL_2_1_2,
                   DSFILE.DS_UNITS_2_1_3,
                   DSFILE.DS_OUTBAL_2_1_3,
                   DSFILE.DS_UNITS_2_1_4,
                   DSFILE.DS_OUTBAL_2_1_4,
                   DSFILE.DS_UNITS_2_1_5,
                   DSFILE.DS_OUTBAL_2_1_5,
                   DSFILE.DS_UNITS_2_1_6,
                   DSFILE.DS_OUTBAL_2_1_6,
                   DSFILE.DS_UNITS_2_1_7,
                   DSFILE.DS_OUTBAL_2_1_7,
                   DSFILE.DS_UNITS_2_1_8,
                   DSFILE.DS_OUTBAL_2_1_8,
                   DSFILE.DS_UNITS_2_1_9,
                   DSFILE.DS_OUTBAL_2_1_9,
                   DSFILE.DS_UNITS_2_1_10,
                   DSFILE.DS_OUTBAL_2_1_10,
                   DSFILE.DS_UNITS_2_2_1,
                   DSFILE.DS_OUTBAL_2_2_1,
                   DSFILE.DS_UNITS_2_2_2,
                   DSFILE.DS_OUTBAL_2_2_2,
                   DSFILE.DS_UNITS_2_2_3,
                   DSFILE.DS_OUTBAL_2_2_3,
                   DSFILE.DS_UNITS_2_2_4,
                   DSFILE.DS_OUTBAL_2_2_4,
                   DSFILE.DS_UNITS_2_2_5,
                   DSFILE.DS_OUTBAL_2_2_5,
                   DSFILE.DS_UNITS_2_2_6,
                   DSFILE.DS_OUTBAL_2_2_6,
                   DSFILE.DS_UNITS_2_2_7,
                   DSFILE.DS_OUTBAL_2_2_7,
                   DSFILE.DS_UNITS_2_2_8,
                   DSFILE.DS_OUTBAL_2_2_8,
                   DSFILE.DS_UNITS_2_2_9,
                   DSFILE.DS_OUTBAL_2_2_9,
                   DSFILE.DS_UNITS_2_2_10,
                   DSFILE.DS_OUTBAL_2_2_10,
                   DSFILE.DS_UNITS_3_1_1,
                   DSFILE.DS_OUTBAL_3_1_1,
                   DSFILE.DS_UNITS_3_1_2,
                   DSFILE.DS_OUTBAL_3_1_2,
                   DSFILE.DS_UNITS_3_1_3,
                   DSFILE.DS_OUTBAL_3_1_3,
                   DSFILE.DS_UNITS_3_1_4,
                   DSFILE.DS_OUTBAL_3_1_4,
                   DSFILE.DS_UNITS_3_1_5,
                   DSFILE.DS_OUTBAL_3_1_5,
                   DSFILE.DS_UNITS_3_1_6,
                   DSFILE.DS_OUTBAL_3_1_6,
                   DSFILE.DS_UNITS_3_1_7,
                   DSFILE.DS_OUTBAL_3_1_7,
                   DSFILE.DS_UNITS_3_1_8,
                   DSFILE.DS_OUTBAL_3_1_8,
                   DSFILE.DS_UNITS_3_1_9,
                   DSFILE.DS_OUTBAL_3_1_9,
                   DSFILE.DS_UNITS_3_1_10,
                   DSFILE.DS_OUTBAL_3_1_10,
                   DSFILE.DS_UNITS_3_2_1,
                   DSFILE.DS_OUTBAL_3_2_1,
                   DSFILE.DS_UNITS_3_2_2,
                   DSFILE.DS_OUTBAL_3_2_2,
                   DSFILE.DS_UNITS_3_2_3,
                   DSFILE.DS_OUTBAL_3_2_3,
                   DSFILE.DS_UNITS_3_2_4,
                   DSFILE.DS_OUTBAL_3_2_4,
                   DSFILE.DS_UNITS_3_2_5,
                   DSFILE.DS_OUTBAL_3_2_5,
                   DSFILE.DS_UNITS_3_2_6,
                   DSFILE.DS_OUTBAL_3_2_6,
                   DSFILE.DS_UNITS_3_2_7,
                   DSFILE.DS_OUTBAL_3_2_7,
                   DSFILE.DS_UNITS_3_2_8,
                   DSFILE.DS_OUTBAL_3_2_8,
                   DSFILE.DS_UNITS_3_2_9,
                   DSFILE.DS_OUTBAL_3_2_9,
                   DSFILE.DS_UNITS_3_2_10,
                   DSFILE.DS_OUTBAL_3_2_10,
                   DSFILE.DS_UNITS_BNKRPT_1_1_1,
                   DSFILE.DS_OUTBAL_BNKRPT_1_1_1,
                   DSFILE.DS_UNITS_BNKRPT_1_1_2,
                   DSFILE.DS_OUTBAL_BNKRPT_1_1_2,
                   DSFILE.DS_UNITS_BNKRPT_1_1_3,
                   DSFILE.DS_OUTBAL_BNKRPT_1_1_3,
                   DSFILE.DS_UNITS_BNKRPT_1_1_4,
                   DSFILE.DS_OUTBAL_BNKRPT_1_1_4,
                   DSFILE.DS_UNITS_BNKRPT_1_1_5,
                   DSFILE.DS_OUTBAL_BNKRPT_1_1_5,
                   DSFILE.DS_UNITS_BNKRPT_1_1_6,
                   DSFILE.DS_OUTBAL_BNKRPT_1_1_6,
                   DSFILE.DS_UNITS_BNKRPT_1_1_7,
                   DSFILE.DS_OUTBAL_BNKRPT_1_1_7,
                   DSFILE.DS_UNITS_BNKRPT_1_1_8,
                   DSFILE.DS_OUTBAL_BNKRPT_1_1_8,
                   DSFILE.DS_UNITS_BNKRPT_1_1_9,
                   DSFILE.DS_OUTBAL_BNKRPT_1_1_9,
                   DSFILE.DS_UNITS_BNKRPT_1_1_10,
                   DSFILE.DS_OUTBAL_BNKRPT_1_1_10,
                   DSFILE.DS_UNITS_BNKRPT_1_2_1,
                   DSFILE.DS_OUTBAL_BNKRPT_1_2_1,
                   DSFILE.DS_UNITS_BNKRPT_1_2_2,
                   DSFILE.DS_OUTBAL_BNKRPT_1_2_2,
                   DSFILE.DS_UNITS_BNKRPT_1_2_3,
                   DSFILE.DS_OUTBAL_BNKRPT_1_2_3,
                   DSFILE.DS_UNITS_BNKRPT_1_2_4,
                   DSFILE.DS_OUTBAL_BNKRPT_1_2_4,
                   DSFILE.DS_UNITS_BNKRPT_1_2_5,
                   DSFILE.DS_OUTBAL_BNKRPT_1_2_5,
                   DSFILE.DS_UNITS_BNKRPT_1_2_6,
                   DSFILE.DS_OUTBAL_BNKRPT_1_2_6,
                   DSFILE.DS_UNITS_BNKRPT_1_2_7,
                   DSFILE.DS_OUTBAL_BNKRPT_1_2_7,
                   DSFILE.DS_UNITS_BNKRPT_1_2_8,
                   DSFILE.DS_OUTBAL_BNKRPT_1_2_8,
                   DSFILE.DS_UNITS_BNKRPT_1_2_9,
                   DSFILE.DS_OUTBAL_BNKRPT_1_2_9,
                   DSFILE.DS_UNITS_BNKRPT_1_2_10,
                   DSFILE.DS_OUTBAL_BNKRPT_1_2_10,
                   DSFILE.DS_UNITS_BNKRPT_2_1_1,
                   DSFILE.DS_OUTBAL_BNKRPT_2_1_1,
                   DSFILE.DS_UNITS_BNKRPT_2_1_2,
                   DSFILE.DS_OUTBAL_BNKRPT_2_1_2,
                   DSFILE.DS_UNITS_BNKRPT_2_1_3,
                   DSFILE.DS_OUTBAL_BNKRPT_2_1_3,
                   DSFILE.DS_UNITS_BNKRPT_2_1_4,
                   DSFILE.DS_OUTBAL_BNKRPT_2_1_4,
                   DSFILE.DS_UNITS_BNKRPT_2_1_5,
                   DSFILE.DS_OUTBAL_BNKRPT_2_1_5,
                   DSFILE.DS_UNITS_BNKRPT_2_1_6,
                   DSFILE.DS_OUTBAL_BNKRPT_2_1_6,
                   DSFILE.DS_UNITS_BNKRPT_2_1_7,
                   DSFILE.DS_OUTBAL_BNKRPT_2_1_7,
                   DSFILE.DS_UNITS_BNKRPT_2_1_8,
                   DSFILE.DS_OUTBAL_BNKRPT_2_1_8,
                   DSFILE.DS_UNITS_BNKRPT_2_1_9,
                   DSFILE.DS_OUTBAL_BNKRPT_2_1_9,
                   DSFILE.DS_UNITS_BNKRPT_2_1_10,
                   DSFILE.DS_OUTBAL_BNKRPT_2_1_10,
                   DSFILE.DS_UNITS_BNKRPT_2_2_1,
                   DSFILE.DS_OUTBAL_BNKRPT_2_2_1,
                   DSFILE.DS_UNITS_BNKRPT_2_2_2,
                   DSFILE.DS_OUTBAL_BNKRPT_2_2_2,
                   DSFILE.DS_UNITS_BNKRPT_2_2_3,
                   DSFILE.DS_OUTBAL_BNKRPT_2_2_3,
                   DSFILE.DS_UNITS_BNKRPT_2_2_4,
                   DSFILE.DS_OUTBAL_BNKRPT_2_2_4,
                   DSFILE.DS_UNITS_BNKRPT_2_2_5,
                   DSFILE.DS_OUTBAL_BNKRPT_2_2_5,
                   DSFILE.DS_UNITS_BNKRPT_2_2_6,
                   DSFILE.DS_OUTBAL_BNKRPT_2_2_6,
                   DSFILE.DS_UNITS_BNKRPT_2_2_7,
                   DSFILE.DS_OUTBAL_BNKRPT_2_2_7,
                   DSFILE.DS_UNITS_BNKRPT_2_2_8,
                   DSFILE.DS_OUTBAL_BNKRPT_2_2_8,
                   DSFILE.DS_UNITS_BNKRPT_2_2_9,
                   DSFILE.DS_OUTBAL_BNKRPT_2_2_9,
                   DSFILE.DS_UNITS_BNKRPT_2_2_10,
                   DSFILE.DS_OUTBAL_BNKRPT_2_2_10,
                   DSFILE.DS_UNITS_BNKRPT_3_1_1,
                   DSFILE.DS_OUTBAL_BNKRPT_3_1_1,
                   DSFILE.DS_UNITS_BNKRPT_3_1_2,
                   DSFILE.DS_OUTBAL_BNKRPT_3_1_2,
                   DSFILE.DS_UNITS_BNKRPT_3_1_3,
                   DSFILE.DS_OUTBAL_BNKRPT_3_1_3,
                   DSFILE.DS_UNITS_BNKRPT_3_1_4,
                   DSFILE.DS_OUTBAL_BNKRPT_3_1_4,
                   DSFILE.DS_UNITS_BNKRPT_3_1_5,
                   DSFILE.DS_OUTBAL_BNKRPT_3_1_5,
                   DSFILE.DS_UNITS_BNKRPT_3_1_6,
                   DSFILE.DS_OUTBAL_BNKRPT_3_1_6,
                   DSFILE.DS_UNITS_BNKRPT_3_1_7,
                   DSFILE.DS_OUTBAL_BNKRPT_3_1_7,
                   DSFILE.DS_UNITS_BNKRPT_3_1_8,
                   DSFILE.DS_OUTBAL_BNKRPT_3_1_8,
                   DSFILE.DS_UNITS_BNKRPT_3_1_9,
                   DSFILE.DS_OUTBAL_BNKRPT_3_1_9,
                   DSFILE.DS_UNITS_BNKRPT_3_1_10,
                   DSFILE.DS_OUTBAL_BNKRPT_3_1_10,
                   DSFILE.DS_UNITS_BNKRPT_3_2_1,
                   DSFILE.DS_OUTBAL_BNKRPT_3_2_1,
                   DSFILE.DS_UNITS_BNKRPT_3_2_2,
                   DSFILE.DS_OUTBAL_BNKRPT_3_2_2,
                   DSFILE.DS_UNITS_BNKRPT_3_2_3,
                   DSFILE.DS_OUTBAL_BNKRPT_3_2_3,
                   DSFILE.DS_UNITS_BNKRPT_3_2_4,
                   DSFILE.DS_OUTBAL_BNKRPT_3_2_4,
                   DSFILE.DS_UNITS_BNKRPT_3_2_5,
                   DSFILE.DS_OUTBAL_BNKRPT_3_2_5,
                   DSFILE.DS_UNITS_BNKRPT_3_2_6,
                   DSFILE.DS_OUTBAL_BNKRPT_3_2_6,
                   DSFILE.DS_UNITS_BNKRPT_3_2_7,
                   DSFILE.DS_OUTBAL_BNKRPT_3_2_7,
                   DSFILE.DS_UNITS_BNKRPT_3_2_8,
                   DSFILE.DS_OUTBAL_BNKRPT_3_2_8,
                   DSFILE.DS_UNITS_BNKRPT_3_2_9,
                   DSFILE.DS_OUTBAL_BNKRPT_3_2_9,
                   DSFILE.DS_UNITS_BNKRPT_3_2_10,
                   DSFILE.DS_OUTBAL_BNKRPT_3_2_10,
                   DSFILE.DS_UNEARNED_AMT_1_1_1,
                   DSFILE.DS_UNEARNED_AMT_1_1_2,
                   DSFILE.DS_UNEARNED_AMT_1_1_3,
                   DSFILE.DS_UNEARNED_AMT_1_1_4,
                   DSFILE.DS_UNEARNED_AMT_1_1_5,
                   DSFILE.DS_UNEARNED_AMT_1_1_6,
                   DSFILE.DS_UNEARNED_AMT_1_1_7,
                   DSFILE.DS_UNEARNED_AMT_1_1_8,
                   DSFILE.DS_UNEARNED_AMT_1_1_9,
                   DSFILE.DS_UNEARNED_AMT_1_1_10,
                   DSFILE.DS_UNEARNED_AMT_1_2_1,
                   DSFILE.DS_UNEARNED_AMT_1_2_2,
                   DSFILE.DS_UNEARNED_AMT_1_2_3,
                   DSFILE.DS_UNEARNED_AMT_1_2_4,
                   DSFILE.DS_UNEARNED_AMT_1_2_5,
                   DSFILE.DS_UNEARNED_AMT_1_2_6,
                   DSFILE.DS_UNEARNED_AMT_1_2_7,
                   DSFILE.DS_UNEARNED_AMT_1_2_8,
                   DSFILE.DS_UNEARNED_AMT_1_2_9,
                   DSFILE.DS_UNEARNED_AMT_1_2_10,
                   DSFILE.DS_UNEARNED_AMT_2_1_1,
                   DSFILE.DS_UNEARNED_AMT_2_1_2,
                   DSFILE.DS_UNEARNED_AMT_2_1_3,
                   DSFILE.DS_UNEARNED_AMT_2_1_4,
                   DSFILE.DS_UNEARNED_AMT_2_1_5,
                   DSFILE.DS_UNEARNED_AMT_2_1_6,
                   DSFILE.DS_UNEARNED_AMT_2_1_7,
                   DSFILE.DS_UNEARNED_AMT_2_1_8,
                   DSFILE.DS_UNEARNED_AMT_2_1_9,
                   DSFILE.DS_UNEARNED_AMT_2_1_10,
                   DSFILE.DS_UNEARNED_AMT_2_2_1,
                   DSFILE.DS_UNEARNED_AMT_2_2_2,
                   DSFILE.DS_UNEARNED_AMT_2_2_3,
                   DSFILE.DS_UNEARNED_AMT_2_2_4,
                   DSFILE.DS_UNEARNED_AMT_2_2_5,
                   DSFILE.DS_UNEARNED_AMT_2_2_6,
                   DSFILE.DS_UNEARNED_AMT_2_2_7,
                   DSFILE.DS_UNEARNED_AMT_2_2_8,
                   DSFILE.DS_UNEARNED_AMT_2_2_9,
                   DSFILE.DS_UNEARNED_AMT_2_2_10,
                   DSFILE.DS_UNEARNED_AMT_3_1_1,
                   DSFILE.DS_UNEARNED_AMT_3_1_2,
                   DSFILE.DS_UNEARNED_AMT_3_1_3,
                   DSFILE.DS_UNEARNED_AMT_3_1_4,
                   DSFILE.DS_UNEARNED_AMT_3_1_5,
                   DSFILE.DS_UNEARNED_AMT_3_1_6,
                   DSFILE.DS_UNEARNED_AMT_3_1_7,
                   DSFILE.DS_UNEARNED_AMT_3_1_8,
                   DSFILE.DS_UNEARNED_AMT_3_1_9,
                   DSFILE.DS_UNEARNED_AMT_3_1_10,
                   DSFILE.DS_UNEARNED_AMT_3_2_1,
                   DSFILE.DS_UNEARNED_AMT_3_2_2,
                   DSFILE.DS_UNEARNED_AMT_3_2_3,
                   DSFILE.DS_UNEARNED_AMT_3_2_4,
                   DSFILE.DS_UNEARNED_AMT_3_2_5,
                   DSFILE.DS_UNEARNED_AMT_3_2_6,
                   DSFILE.DS_UNEARNED_AMT_3_2_7,
                   DSFILE.DS_UNEARNED_AMT_3_2_8,
                   DSFILE.DS_UNEARNED_AMT_3_2_9,
                   DSFILE.DS_UNEARNED_AMT_3_2_10,
                   DSFILE.DS_DLDISC_VOL_1,
                   DSFILE.DS_DLDISC_VOL_2,
                   DSFILE.DS_DLDISC_VOL_3,
                   DSFILE.DS_AFPDUEDL_MEMO,
                   DSFILE.DS_UNPAID_MANUAL,
                   CAST(DSFILE.DS_LTOUCH_DATE
                                                  AS VARCHAR(10))
            FROM DBO.DSFILE
            WHERE DSFILE.DS_DATE_CCYYMM >= :QDS1-WBEG-DATE-CCYYMM
              AND DSFILE.DS_DATE_CCYYMM <= :QDS1-WEND-DATE-CCYYMM
              AND DSFILE.DS_BRANCH = :QDS1-WBEG-BRANCH
              AND DSFILE.DS_NUM >= :QDS1-WBEG-NUM
              AND DSFILE.DS_NUM <= :QDS1-WEND-NUM
              AND DSFILE.DS_CLASS >= :QDS1-WBEG-CLASS
              AND DSFILE.DS_CLASS <= :QDS1-WEND-CLASS
            ORDER BY DSFILE.DS_DATE_CCYYMM,
                     DSFILE.DS_BRANCH,
                     DSFILE.DS_NUM,
                     DSFILE.DS_CLASS
           END-EXEC.

           EXEC SQL
               OPEN DS1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QDS1-WBEG-BRANCH 
                          QDS1-WEND-BRANCH
                          QDS1-WBEG-DATE-CCYYMM
                          QDS1-WEND-DATE-CCYYMM
                          QDS1-WBEG-NUM
                          QDS1-WEND-NUM
                          QDS1-WBEG-CLASS
                          QDS1-WEND-CLASS.
 
           MOVE STAT---GOOD TO DS1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-DS1-FILE.
           PERFORM READ-IT.
           MOVE DS-PATH-OWNBR  TO DS-BRANCH. 
           MOVE DS-PATH-SQL    TO E-FILE.
           MOVE DS1-KEY        TO E-KEYX.
           INITIALIZE QDS-REC.  

           MOVE DS-DATE-CCYYMM  TO QDS-DATE-CCYYMM.
           MOVE DS-BRANCH       TO QDS-BRANCH.
           MOVE DS-NUM          TO QDS-NUM.
           MOVE DS-CLASS        TO QDS-CLASS.

           EXEC SQL
            SELECT DSFILE.DS_DATE_CCYYMM,
                   DSFILE.DS_BRANCH,
                   DSFILE.DS_NUM,
                   DSFILE.DS_CLASS,
                   DSFILE.DS_DLRVPRIN_1,
                   DSFILE.DS_DLRVPRIN_2,
                   DSFILE.DS_DLRVPRIN_3,
                   DSFILE.DS_DLRVCHGS_1,
                   DSFILE.DS_DLRVCHGS_2,
                   DSFILE.DS_DLRVCHGS_3,
                   DSFILE.DS_DLPARVHELD,
                   DSFILE.DS_UNPAIDCK,
                   DSFILE.DS_DLDISC,
                   DSFILE.DS_DLPARVPAID,
                   DSFILE.DS_SUMWGTTM,
                   DSFILE.DS_SUMWGTAV,
                   DSFILE.DS_SUMWGTTA,
                   DSFILE.DS_RCOURSE_UNITS_1,
                   DSFILE.DS_RCOURSE_UNITS_2,
                   DSFILE.DS_RCOURSE_UNITS_3,
                   DSFILE.DS_RCOURSE_AMT_1,
                   DSFILE.DS_RCOURSE_AMT_2,
                   DSFILE.DS_RCOURSE_AMT_3,
                   DSFILE.DS_NPURUNIT_1,
                   DSFILE.DS_NPURUNIT_2,
                   DSFILE.DS_NPURUNIT_3,
                   DSFILE.DS_NPURAMT_1,
                   DSFILE.DS_NPURAMT_2,
                   DSFILE.DS_NPURAMT_3,
                   DSFILE.DS_OPURUNIT_1,
                   DSFILE.DS_OPURUNIT_2,
                   DSFILE.DS_OPURUNIT_3,
                   DSFILE.DS_OPURAMT_1,
                   DSFILE.DS_OPURAMT_2,
                   DSFILE.DS_OPURAMT_3,
                   DSFILE.DS_RNEWUNIT_1,
                   DSFILE.DS_RNEWUNIT_2,
                   DSFILE.DS_RNEWUNIT_3,
                   DSFILE.DS_RNEWAMT_1,
                   DSFILE.DS_RNEWAMT_2,
                   DSFILE.DS_RNEWAMT_3,
                   DSFILE.DS_PAIDUNIT_1,
                   DSFILE.DS_PAIDUNIT_2,
                   DSFILE.DS_PAIDUNIT_3,
                   DSFILE.DS_PAIDAMT_1,
                   DSFILE.DS_PAIDAMT_2,
                   DSFILE.DS_PAIDAMT_3,
                   DSFILE.DS_TODLUNIT_1,
                   DSFILE.DS_TODLUNIT_2,
                   DSFILE.DS_TODLUNIT_3,
                   DSFILE.DS_TODLAMT_1,
                   DSFILE.DS_TODLAMT_2,
                   DSFILE.DS_TODLAMT_3,
                   DSFILE.DS_OUTSUNIT_1,
                   DSFILE.DS_OUTSUNIT_2,
                   DSFILE.DS_OUTSUNIT_3,
                   DSFILE.DS_OUTSAMT_1,
                   DSFILE.DS_OUTSAMT_2,
                   DSFILE.DS_OUTSAMT_3,
                   DSFILE.DS_PLOSUNIT_1,
                   DSFILE.DS_PLOSUNIT_2,
                   DSFILE.DS_PLOSUNIT_3,
                   DSFILE.DS_PLOSAMT_1,
                   DSFILE.DS_PLOSAMT_2,
                   DSFILE.DS_PLOSAMT_3,
                   DSFILE.DS_RCPLUNIT_1,
                   DSFILE.DS_RCPLUNIT_2,
                   DSFILE.DS_RCPLUNIT_3,
                   DSFILE.DS_RCPLAMT_1,
                   DSFILE.DS_RCPLAMT_2,
                   DSFILE.DS_RCPLAMT_3,
                   DSFILE.DS_JDOSUNIT_1,
                   DSFILE.DS_JDOSUNIT_2,
                   DSFILE.DS_JDOSUNIT_3,
                   DSFILE.DS_JDOSAMT_1,
                   DSFILE.DS_JDOSAMT_2,
                   DSFILE.DS_JDOSAMT_3,
                   CAST(DSFILE.DS_DELUNEARN_DATE
                                                  AS VARCHAR(10)),
                   DSFILE.DS_PROCEEDS_1,
                   DSFILE.DS_PROCEEDS_2,
                   DSFILE.DS_NO_REPO_1_1_1,
                   DSFILE.DS_AMT_REPO_1_1_1,
                   DSFILE.DS_NO_REPO_1_1_2,
                   DSFILE.DS_AMT_REPO_1_1_2,
                   DSFILE.DS_NO_REPO_1_1_3,
                   DSFILE.DS_AMT_REPO_1_1_3,
                   DSFILE.DS_NO_REPO_1_1_4,
                   DSFILE.DS_AMT_REPO_1_1_4,
                   DSFILE.DS_NO_REPO_1_1_5,
                   DSFILE.DS_AMT_REPO_1_1_5,
                   DSFILE.DS_NO_REPO_1_2_1,
                   DSFILE.DS_AMT_REPO_1_2_1,
                   DSFILE.DS_NO_REPO_1_2_2,
                   DSFILE.DS_AMT_REPO_1_2_2,
                   DSFILE.DS_NO_REPO_1_2_3,
                   DSFILE.DS_AMT_REPO_1_2_3,
                   DSFILE.DS_NO_REPO_1_2_4,
                   DSFILE.DS_AMT_REPO_1_2_4,
                   DSFILE.DS_NO_REPO_1_2_5,
                   DSFILE.DS_AMT_REPO_1_2_5,
                   DSFILE.DS_NO_REPO_2_1_1,
                   DSFILE.DS_AMT_REPO_2_1_1,
                   DSFILE.DS_NO_REPO_2_1_2,
                   DSFILE.DS_AMT_REPO_2_1_2,
                   DSFILE.DS_NO_REPO_2_1_3,
                   DSFILE.DS_AMT_REPO_2_1_3,
                   DSFILE.DS_NO_REPO_2_1_4,
                   DSFILE.DS_AMT_REPO_2_1_4,
                   DSFILE.DS_NO_REPO_2_1_5,
                   DSFILE.DS_AMT_REPO_2_1_5,
                   DSFILE.DS_NO_REPO_2_2_1,
                   DSFILE.DS_AMT_REPO_2_2_1,
                   DSFILE.DS_NO_REPO_2_2_2,
                   DSFILE.DS_AMT_REPO_2_2_2,
                   DSFILE.DS_NO_REPO_2_2_3,
                   DSFILE.DS_AMT_REPO_2_2_3,
                   DSFILE.DS_NO_REPO_2_2_4,
                   DSFILE.DS_AMT_REPO_2_2_4,
                   DSFILE.DS_NO_REPO_2_2_5,
                   DSFILE.DS_AMT_REPO_2_2_5,
                   DSFILE.DS_NO_REPO_3_1_1,
                   DSFILE.DS_AMT_REPO_3_1_1,
                   DSFILE.DS_NO_REPO_3_1_2,
                   DSFILE.DS_AMT_REPO_3_1_2,
                   DSFILE.DS_NO_REPO_3_1_3,
                   DSFILE.DS_AMT_REPO_3_1_3,
                   DSFILE.DS_NO_REPO_3_1_4,
                   DSFILE.DS_AMT_REPO_3_1_4,
                   DSFILE.DS_NO_REPO_3_1_5,
                   DSFILE.DS_AMT_REPO_3_1_5,
                   DSFILE.DS_NO_REPO_3_2_1,
                   DSFILE.DS_AMT_REPO_3_2_1,
                   DSFILE.DS_NO_REPO_3_2_2,
                   DSFILE.DS_AMT_REPO_3_2_2,
                   DSFILE.DS_NO_REPO_3_2_3,
                   DSFILE.DS_AMT_REPO_3_2_3,
                   DSFILE.DS_NO_REPO_3_2_4,
                   DSFILE.DS_AMT_REPO_3_2_4,
                   DSFILE.DS_NO_REPO_3_2_5,
                   DSFILE.DS_AMT_REPO_3_2_5,
                   DSFILE.DS_UNITS_1_1_1,
                   DSFILE.DS_OUTBAL_1_1_1,
                   DSFILE.DS_UNITS_1_1_2,
                   DSFILE.DS_OUTBAL_1_1_2,
                   DSFILE.DS_UNITS_1_1_3,
                   DSFILE.DS_OUTBAL_1_1_3,
                   DSFILE.DS_UNITS_1_1_4,
                   DSFILE.DS_OUTBAL_1_1_4,
                   DSFILE.DS_UNITS_1_1_5,
                   DSFILE.DS_OUTBAL_1_1_5,
                   DSFILE.DS_UNITS_1_1_6,
                   DSFILE.DS_OUTBAL_1_1_6,
                   DSFILE.DS_UNITS_1_1_7,
                   DSFILE.DS_OUTBAL_1_1_7,
                   DSFILE.DS_UNITS_1_1_8,
                   DSFILE.DS_OUTBAL_1_1_8,
                   DSFILE.DS_UNITS_1_1_9,
                   DSFILE.DS_OUTBAL_1_1_9,
                   DSFILE.DS_UNITS_1_1_10,
                   DSFILE.DS_OUTBAL_1_1_10,
                   DSFILE.DS_UNITS_1_2_1,
                   DSFILE.DS_OUTBAL_1_2_1,
                   DSFILE.DS_UNITS_1_2_2,
                   DSFILE.DS_OUTBAL_1_2_2,
                   DSFILE.DS_UNITS_1_2_3,
                   DSFILE.DS_OUTBAL_1_2_3,
                   DSFILE.DS_UNITS_1_2_4,
                   DSFILE.DS_OUTBAL_1_2_4,
                   DSFILE.DS_UNITS_1_2_5,
                   DSFILE.DS_OUTBAL_1_2_5,
                   DSFILE.DS_UNITS_1_2_6,
                   DSFILE.DS_OUTBAL_1_2_6,
                   DSFILE.DS_UNITS_1_2_7,
                   DSFILE.DS_OUTBAL_1_2_7,
                   DSFILE.DS_UNITS_1_2_8,
                   DSFILE.DS_OUTBAL_1_2_8,
                   DSFILE.DS_UNITS_1_2_9,
                   DSFILE.DS_OUTBAL_1_2_9,
                   DSFILE.DS_UNITS_1_2_10,
                   DSFILE.DS_OUTBAL_1_2_10,
                   DSFILE.DS_UNITS_2_1_1,
                   DSFILE.DS_OUTBAL_2_1_1,
                   DSFILE.DS_UNITS_2_1_2,
                   DSFILE.DS_OUTBAL_2_1_2,
                   DSFILE.DS_UNITS_2_1_3,
                   DSFILE.DS_OUTBAL_2_1_3,
                   DSFILE.DS_UNITS_2_1_4,
                   DSFILE.DS_OUTBAL_2_1_4,
                   DSFILE.DS_UNITS_2_1_5,
                   DSFILE.DS_OUTBAL_2_1_5,
                   DSFILE.DS_UNITS_2_1_6,
                   DSFILE.DS_OUTBAL_2_1_6,
                   DSFILE.DS_UNITS_2_1_7,
                   DSFILE.DS_OUTBAL_2_1_7,
                   DSFILE.DS_UNITS_2_1_8,
                   DSFILE.DS_OUTBAL_2_1_8,
                   DSFILE.DS_UNITS_2_1_9,
                   DSFILE.DS_OUTBAL_2_1_9,
                   DSFILE.DS_UNITS_2_1_10,
                   DSFILE.DS_OUTBAL_2_1_10,
                   DSFILE.DS_UNITS_2_2_1,
                   DSFILE.DS_OUTBAL_2_2_1,
                   DSFILE.DS_UNITS_2_2_2,
                   DSFILE.DS_OUTBAL_2_2_2,
                   DSFILE.DS_UNITS_2_2_3,
                   DSFILE.DS_OUTBAL_2_2_3,
                   DSFILE.DS_UNITS_2_2_4,
                   DSFILE.DS_OUTBAL_2_2_4,
                   DSFILE.DS_UNITS_2_2_5,
                   DSFILE.DS_OUTBAL_2_2_5,
                   DSFILE.DS_UNITS_2_2_6,
                   DSFILE.DS_OUTBAL_2_2_6,
                   DSFILE.DS_UNITS_2_2_7,
                   DSFILE.DS_OUTBAL_2_2_7,
                   DSFILE.DS_UNITS_2_2_8,
                   DSFILE.DS_OUTBAL_2_2_8,
                   DSFILE.DS_UNITS_2_2_9,
                   DSFILE.DS_OUTBAL_2_2_9,
                   DSFILE.DS_UNITS_2_2_10,
                   DSFILE.DS_OUTBAL_2_2_10,
                   DSFILE.DS_UNITS_3_1_1,
                   DSFILE.DS_OUTBAL_3_1_1,
                   DSFILE.DS_UNITS_3_1_2,
                   DSFILE.DS_OUTBAL_3_1_2,
                   DSFILE.DS_UNITS_3_1_3,
                   DSFILE.DS_OUTBAL_3_1_3,
                   DSFILE.DS_UNITS_3_1_4,
                   DSFILE.DS_OUTBAL_3_1_4,
                   DSFILE.DS_UNITS_3_1_5,
                   DSFILE.DS_OUTBAL_3_1_5,
                   DSFILE.DS_UNITS_3_1_6,
                   DSFILE.DS_OUTBAL_3_1_6,
                   DSFILE.DS_UNITS_3_1_7,
                   DSFILE.DS_OUTBAL_3_1_7,
                   DSFILE.DS_UNITS_3_1_8,
                   DSFILE.DS_OUTBAL_3_1_8,
                   DSFILE.DS_UNITS_3_1_9,
                   DSFILE.DS_OUTBAL_3_1_9,
                   DSFILE.DS_UNITS_3_1_10,
                   DSFILE.DS_OUTBAL_3_1_10,
                   DSFILE.DS_UNITS_3_2_1,
                   DSFILE.DS_OUTBAL_3_2_1,
                   DSFILE.DS_UNITS_3_2_2,
                   DSFILE.DS_OUTBAL_3_2_2,
                   DSFILE.DS_UNITS_3_2_3,
                   DSFILE.DS_OUTBAL_3_2_3,
                   DSFILE.DS_UNITS_3_2_4,
                   DSFILE.DS_OUTBAL_3_2_4,
                   DSFILE.DS_UNITS_3_2_5,
                   DSFILE.DS_OUTBAL_3_2_5,
                   DSFILE.DS_UNITS_3_2_6,
                   DSFILE.DS_OUTBAL_3_2_6,
                   DSFILE.DS_UNITS_3_2_7,
                   DSFILE.DS_OUTBAL_3_2_7,
                   DSFILE.DS_UNITS_3_2_8,
                   DSFILE.DS_OUTBAL_3_2_8,
                   DSFILE.DS_UNITS_3_2_9,
                   DSFILE.DS_OUTBAL_3_2_9,
                   DSFILE.DS_UNITS_3_2_10,
                   DSFILE.DS_OUTBAL_3_2_10,
                   DSFILE.DS_UNITS_BNKRPT_1_1_1,
                   DSFILE.DS_OUTBAL_BNKRPT_1_1_1,
                   DSFILE.DS_UNITS_BNKRPT_1_1_2,
                   DSFILE.DS_OUTBAL_BNKRPT_1_1_2,
                   DSFILE.DS_UNITS_BNKRPT_1_1_3,
                   DSFILE.DS_OUTBAL_BNKRPT_1_1_3,
                   DSFILE.DS_UNITS_BNKRPT_1_1_4,
                   DSFILE.DS_OUTBAL_BNKRPT_1_1_4,
                   DSFILE.DS_UNITS_BNKRPT_1_1_5,
                   DSFILE.DS_OUTBAL_BNKRPT_1_1_5,
                   DSFILE.DS_UNITS_BNKRPT_1_1_6,
                   DSFILE.DS_OUTBAL_BNKRPT_1_1_6,
                   DSFILE.DS_UNITS_BNKRPT_1_1_7,
                   DSFILE.DS_OUTBAL_BNKRPT_1_1_7,
                   DSFILE.DS_UNITS_BNKRPT_1_1_8,
                   DSFILE.DS_OUTBAL_BNKRPT_1_1_8,
                   DSFILE.DS_UNITS_BNKRPT_1_1_9,
                   DSFILE.DS_OUTBAL_BNKRPT_1_1_9,
                   DSFILE.DS_UNITS_BNKRPT_1_1_10,
                   DSFILE.DS_OUTBAL_BNKRPT_1_1_10,
                   DSFILE.DS_UNITS_BNKRPT_1_2_1,
                   DSFILE.DS_OUTBAL_BNKRPT_1_2_1,
                   DSFILE.DS_UNITS_BNKRPT_1_2_2,
                   DSFILE.DS_OUTBAL_BNKRPT_1_2_2,
                   DSFILE.DS_UNITS_BNKRPT_1_2_3,
                   DSFILE.DS_OUTBAL_BNKRPT_1_2_3,
                   DSFILE.DS_UNITS_BNKRPT_1_2_4,
                   DSFILE.DS_OUTBAL_BNKRPT_1_2_4,
                   DSFILE.DS_UNITS_BNKRPT_1_2_5,
                   DSFILE.DS_OUTBAL_BNKRPT_1_2_5,
                   DSFILE.DS_UNITS_BNKRPT_1_2_6,
                   DSFILE.DS_OUTBAL_BNKRPT_1_2_6,
                   DSFILE.DS_UNITS_BNKRPT_1_2_7,
                   DSFILE.DS_OUTBAL_BNKRPT_1_2_7,
                   DSFILE.DS_UNITS_BNKRPT_1_2_8,
                   DSFILE.DS_OUTBAL_BNKRPT_1_2_8,
                   DSFILE.DS_UNITS_BNKRPT_1_2_9,
                   DSFILE.DS_OUTBAL_BNKRPT_1_2_9,
                   DSFILE.DS_UNITS_BNKRPT_1_2_10,
                   DSFILE.DS_OUTBAL_BNKRPT_1_2_10,
                   DSFILE.DS_UNITS_BNKRPT_2_1_1,
                   DSFILE.DS_OUTBAL_BNKRPT_2_1_1,
                   DSFILE.DS_UNITS_BNKRPT_2_1_2,
                   DSFILE.DS_OUTBAL_BNKRPT_2_1_2,
                   DSFILE.DS_UNITS_BNKRPT_2_1_3,
                   DSFILE.DS_OUTBAL_BNKRPT_2_1_3,
                   DSFILE.DS_UNITS_BNKRPT_2_1_4,
                   DSFILE.DS_OUTBAL_BNKRPT_2_1_4,
                   DSFILE.DS_UNITS_BNKRPT_2_1_5,
                   DSFILE.DS_OUTBAL_BNKRPT_2_1_5,
                   DSFILE.DS_UNITS_BNKRPT_2_1_6,
                   DSFILE.DS_OUTBAL_BNKRPT_2_1_6,
                   DSFILE.DS_UNITS_BNKRPT_2_1_7,
                   DSFILE.DS_OUTBAL_BNKRPT_2_1_7,
                   DSFILE.DS_UNITS_BNKRPT_2_1_8,
                   DSFILE.DS_OUTBAL_BNKRPT_2_1_8,
                   DSFILE.DS_UNITS_BNKRPT_2_1_9,
                   DSFILE.DS_OUTBAL_BNKRPT_2_1_9,
                   DSFILE.DS_UNITS_BNKRPT_2_1_10,
                   DSFILE.DS_OUTBAL_BNKRPT_2_1_10,
                   DSFILE.DS_UNITS_BNKRPT_2_2_1,
                   DSFILE.DS_OUTBAL_BNKRPT_2_2_1,
                   DSFILE.DS_UNITS_BNKRPT_2_2_2,
                   DSFILE.DS_OUTBAL_BNKRPT_2_2_2,
                   DSFILE.DS_UNITS_BNKRPT_2_2_3,
                   DSFILE.DS_OUTBAL_BNKRPT_2_2_3,
                   DSFILE.DS_UNITS_BNKRPT_2_2_4,
                   DSFILE.DS_OUTBAL_BNKRPT_2_2_4,
                   DSFILE.DS_UNITS_BNKRPT_2_2_5,
                   DSFILE.DS_OUTBAL_BNKRPT_2_2_5,
                   DSFILE.DS_UNITS_BNKRPT_2_2_6,
                   DSFILE.DS_OUTBAL_BNKRPT_2_2_6,
                   DSFILE.DS_UNITS_BNKRPT_2_2_7,
                   DSFILE.DS_OUTBAL_BNKRPT_2_2_7,
                   DSFILE.DS_UNITS_BNKRPT_2_2_8,
                   DSFILE.DS_OUTBAL_BNKRPT_2_2_8,
                   DSFILE.DS_UNITS_BNKRPT_2_2_9,
                   DSFILE.DS_OUTBAL_BNKRPT_2_2_9,
                   DSFILE.DS_UNITS_BNKRPT_2_2_10,
                   DSFILE.DS_OUTBAL_BNKRPT_2_2_10,
                   DSFILE.DS_UNITS_BNKRPT_3_1_1,
                   DSFILE.DS_OUTBAL_BNKRPT_3_1_1,
                   DSFILE.DS_UNITS_BNKRPT_3_1_2,
                   DSFILE.DS_OUTBAL_BNKRPT_3_1_2,
                   DSFILE.DS_UNITS_BNKRPT_3_1_3,
                   DSFILE.DS_OUTBAL_BNKRPT_3_1_3,
                   DSFILE.DS_UNITS_BNKRPT_3_1_4,
                   DSFILE.DS_OUTBAL_BNKRPT_3_1_4,
                   DSFILE.DS_UNITS_BNKRPT_3_1_5,
                   DSFILE.DS_OUTBAL_BNKRPT_3_1_5,
                   DSFILE.DS_UNITS_BNKRPT_3_1_6,
                   DSFILE.DS_OUTBAL_BNKRPT_3_1_6,
                   DSFILE.DS_UNITS_BNKRPT_3_1_7,
                   DSFILE.DS_OUTBAL_BNKRPT_3_1_7,
                   DSFILE.DS_UNITS_BNKRPT_3_1_8,
                   DSFILE.DS_OUTBAL_BNKRPT_3_1_8,
                   DSFILE.DS_UNITS_BNKRPT_3_1_9,
                   DSFILE.DS_OUTBAL_BNKRPT_3_1_9,
                   DSFILE.DS_UNITS_BNKRPT_3_1_10,
                   DSFILE.DS_OUTBAL_BNKRPT_3_1_10,
                   DSFILE.DS_UNITS_BNKRPT_3_2_1,
                   DSFILE.DS_OUTBAL_BNKRPT_3_2_1,
                   DSFILE.DS_UNITS_BNKRPT_3_2_2,
                   DSFILE.DS_OUTBAL_BNKRPT_3_2_2,
                   DSFILE.DS_UNITS_BNKRPT_3_2_3,
                   DSFILE.DS_OUTBAL_BNKRPT_3_2_3,
                   DSFILE.DS_UNITS_BNKRPT_3_2_4,
                   DSFILE.DS_OUTBAL_BNKRPT_3_2_4,
                   DSFILE.DS_UNITS_BNKRPT_3_2_5,
                   DSFILE.DS_OUTBAL_BNKRPT_3_2_5,
                   DSFILE.DS_UNITS_BNKRPT_3_2_6,
                   DSFILE.DS_OUTBAL_BNKRPT_3_2_6,
                   DSFILE.DS_UNITS_BNKRPT_3_2_7,
                   DSFILE.DS_OUTBAL_BNKRPT_3_2_7,
                   DSFILE.DS_UNITS_BNKRPT_3_2_8,
                   DSFILE.DS_OUTBAL_BNKRPT_3_2_8,
                   DSFILE.DS_UNITS_BNKRPT_3_2_9,
                   DSFILE.DS_OUTBAL_BNKRPT_3_2_9,
                   DSFILE.DS_UNITS_BNKRPT_3_2_10,
                   DSFILE.DS_OUTBAL_BNKRPT_3_2_10,
                   DSFILE.DS_UNEARNED_AMT_1_1_1,
                   DSFILE.DS_UNEARNED_AMT_1_1_2,
                   DSFILE.DS_UNEARNED_AMT_1_1_3,
                   DSFILE.DS_UNEARNED_AMT_1_1_4,
                   DSFILE.DS_UNEARNED_AMT_1_1_5,
                   DSFILE.DS_UNEARNED_AMT_1_1_6,
                   DSFILE.DS_UNEARNED_AMT_1_1_7,
                   DSFILE.DS_UNEARNED_AMT_1_1_8,
                   DSFILE.DS_UNEARNED_AMT_1_1_9,
                   DSFILE.DS_UNEARNED_AMT_1_1_10,
                   DSFILE.DS_UNEARNED_AMT_1_2_1,
                   DSFILE.DS_UNEARNED_AMT_1_2_2,
                   DSFILE.DS_UNEARNED_AMT_1_2_3,
                   DSFILE.DS_UNEARNED_AMT_1_2_4,
                   DSFILE.DS_UNEARNED_AMT_1_2_5,
                   DSFILE.DS_UNEARNED_AMT_1_2_6,
                   DSFILE.DS_UNEARNED_AMT_1_2_7,
                   DSFILE.DS_UNEARNED_AMT_1_2_8,
                   DSFILE.DS_UNEARNED_AMT_1_2_9,
                   DSFILE.DS_UNEARNED_AMT_1_2_10,
                   DSFILE.DS_UNEARNED_AMT_2_1_1,
                   DSFILE.DS_UNEARNED_AMT_2_1_2,
                   DSFILE.DS_UNEARNED_AMT_2_1_3,
                   DSFILE.DS_UNEARNED_AMT_2_1_4,
                   DSFILE.DS_UNEARNED_AMT_2_1_5,
                   DSFILE.DS_UNEARNED_AMT_2_1_6,
                   DSFILE.DS_UNEARNED_AMT_2_1_7,
                   DSFILE.DS_UNEARNED_AMT_2_1_8,
                   DSFILE.DS_UNEARNED_AMT_2_1_9,
                   DSFILE.DS_UNEARNED_AMT_2_1_10,
                   DSFILE.DS_UNEARNED_AMT_2_2_1,
                   DSFILE.DS_UNEARNED_AMT_2_2_2,
                   DSFILE.DS_UNEARNED_AMT_2_2_3,
                   DSFILE.DS_UNEARNED_AMT_2_2_4,
                   DSFILE.DS_UNEARNED_AMT_2_2_5,
                   DSFILE.DS_UNEARNED_AMT_2_2_6,
                   DSFILE.DS_UNEARNED_AMT_2_2_7,
                   DSFILE.DS_UNEARNED_AMT_2_2_8,
                   DSFILE.DS_UNEARNED_AMT_2_2_9,
                   DSFILE.DS_UNEARNED_AMT_2_2_10,
                   DSFILE.DS_UNEARNED_AMT_3_1_1,
                   DSFILE.DS_UNEARNED_AMT_3_1_2,
                   DSFILE.DS_UNEARNED_AMT_3_1_3,
                   DSFILE.DS_UNEARNED_AMT_3_1_4,
                   DSFILE.DS_UNEARNED_AMT_3_1_5,
                   DSFILE.DS_UNEARNED_AMT_3_1_6,
                   DSFILE.DS_UNEARNED_AMT_3_1_7,
                   DSFILE.DS_UNEARNED_AMT_3_1_8,
                   DSFILE.DS_UNEARNED_AMT_3_1_9,
                   DSFILE.DS_UNEARNED_AMT_3_1_10,
                   DSFILE.DS_UNEARNED_AMT_3_2_1,
                   DSFILE.DS_UNEARNED_AMT_3_2_2,
                   DSFILE.DS_UNEARNED_AMT_3_2_3,
                   DSFILE.DS_UNEARNED_AMT_3_2_4,
                   DSFILE.DS_UNEARNED_AMT_3_2_5,
                   DSFILE.DS_UNEARNED_AMT_3_2_6,
                   DSFILE.DS_UNEARNED_AMT_3_2_7,
                   DSFILE.DS_UNEARNED_AMT_3_2_8,
                   DSFILE.DS_UNEARNED_AMT_3_2_9,
                   DSFILE.DS_UNEARNED_AMT_3_2_10,
                   DSFILE.DS_DLDISC_VOL_1,
                   DSFILE.DS_DLDISC_VOL_2,
                   DSFILE.DS_DLDISC_VOL_3,
                   DSFILE.DS_AFPDUEDL_MEMO,
                   DSFILE.DS_UNPAID_MANUAL,
                   CAST(DSFILE.DS_LTOUCH_DATE
                                                  AS VARCHAR(10))
            INTO :QDS-REC
            FROM DBO.DSFILE
            WHERE DSFILE.DS_DATE_CCYYMM = :QDS-DATE-CCYYMM
              AND DSFILE.DS_BRANCH = :QDS-BRANCH
              AND DSFILE.DS_NUM = :QDS-NUM
              AND DSFILE.DS_CLASS = :QDS-CLASS
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-DS1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-DS-FIELDS.

      *-----------------------------------------------------------------
       READ-DS1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE DS-PATH-OWNBR  TO DS-BRANCH.
           MOVE DS-PATH-SQL    TO E-FILE.
           MOVE DS1-KEY        TO E-KEYX.
           INITIALIZE QDS-REC.

           IF ( DS1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH DS1_CURSOR INTO :QDS-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-DS1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-DS-FIELDS.

      *-----------------------------------------------------------------
       WRITE-DS1-FILE.

           MOVE DS-PATH-OWNBR  TO DS-BRANCH.
           ACCEPT DS-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE DS-PATH-SQL    TO E-FILE.
           MOVE DS1-KEY        TO E-KEYX.
           PERFORM SET-DS-FIELDS.

           MOVE '2' TO SQL-UPDATE-CONN-USE-SW. *> USE UPDATE_CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

           EXEC SQL
            INSERT INTO DBO.DSFILE
             (DSFILE.DS_DATE_CCYYMM,
              DSFILE.DS_BRANCH,
              DSFILE.DS_NUM,
              DSFILE.DS_CLASS,
              DSFILE.DS_DLRVPRIN_1,
              DSFILE.DS_DLRVPRIN_2,
              DSFILE.DS_DLRVPRIN_3,
              DSFILE.DS_DLRVCHGS_1,
              DSFILE.DS_DLRVCHGS_2,
              DSFILE.DS_DLRVCHGS_3,
              DSFILE.DS_DLPARVHELD,
              DSFILE.DS_UNPAIDCK,
              DSFILE.DS_DLDISC,
              DSFILE.DS_DLPARVPAID,
              DSFILE.DS_SUMWGTTM,
              DSFILE.DS_SUMWGTAV,
              DSFILE.DS_SUMWGTTA,
              DSFILE.DS_RCOURSE_UNITS_1,
              DSFILE.DS_RCOURSE_UNITS_2,
              DSFILE.DS_RCOURSE_UNITS_3,
              DSFILE.DS_RCOURSE_AMT_1,
              DSFILE.DS_RCOURSE_AMT_2,
              DSFILE.DS_RCOURSE_AMT_3,
              DSFILE.DS_NPURUNIT_1,
              DSFILE.DS_NPURUNIT_2,
              DSFILE.DS_NPURUNIT_3,
              DSFILE.DS_NPURAMT_1,
              DSFILE.DS_NPURAMT_2,
              DSFILE.DS_NPURAMT_3,
              DSFILE.DS_OPURUNIT_1,
              DSFILE.DS_OPURUNIT_2,
              DSFILE.DS_OPURUNIT_3,
              DSFILE.DS_OPURAMT_1,
              DSFILE.DS_OPURAMT_2,
              DSFILE.DS_OPURAMT_3,
              DSFILE.DS_RNEWUNIT_1,
              DSFILE.DS_RNEWUNIT_2,
              DSFILE.DS_RNEWUNIT_3,
              DSFILE.DS_RNEWAMT_1,
              DSFILE.DS_RNEWAMT_2,
              DSFILE.DS_RNEWAMT_3,
              DSFILE.DS_PAIDUNIT_1,
              DSFILE.DS_PAIDUNIT_2,
              DSFILE.DS_PAIDUNIT_3,
              DSFILE.DS_PAIDAMT_1,
              DSFILE.DS_PAIDAMT_2,
              DSFILE.DS_PAIDAMT_3,
              DSFILE.DS_TODLUNIT_1,
              DSFILE.DS_TODLUNIT_2,
              DSFILE.DS_TODLUNIT_3,
              DSFILE.DS_TODLAMT_1,
              DSFILE.DS_TODLAMT_2,
              DSFILE.DS_TODLAMT_3,
              DSFILE.DS_OUTSUNIT_1,
              DSFILE.DS_OUTSUNIT_2,
              DSFILE.DS_OUTSUNIT_3,
              DSFILE.DS_OUTSAMT_1,
              DSFILE.DS_OUTSAMT_2,
              DSFILE.DS_OUTSAMT_3,
              DSFILE.DS_PLOSUNIT_1,
              DSFILE.DS_PLOSUNIT_2,
              DSFILE.DS_PLOSUNIT_3,
              DSFILE.DS_PLOSAMT_1,
              DSFILE.DS_PLOSAMT_2,
              DSFILE.DS_PLOSAMT_3,
              DSFILE.DS_RCPLUNIT_1,
              DSFILE.DS_RCPLUNIT_2,
              DSFILE.DS_RCPLUNIT_3,
              DSFILE.DS_RCPLAMT_1,
              DSFILE.DS_RCPLAMT_2,
              DSFILE.DS_RCPLAMT_3,
              DSFILE.DS_JDOSUNIT_1,
              DSFILE.DS_JDOSUNIT_2,
              DSFILE.DS_JDOSUNIT_3,
              DSFILE.DS_JDOSAMT_1,
              DSFILE.DS_JDOSAMT_2,
              DSFILE.DS_JDOSAMT_3,
              DSFILE.DS_DELUNEARN_DATE,
              DSFILE.DS_PROCEEDS_1,
              DSFILE.DS_PROCEEDS_2,
              DSFILE.DS_NO_REPO_1_1_1,
              DSFILE.DS_AMT_REPO_1_1_1,
              DSFILE.DS_NO_REPO_1_1_2,
              DSFILE.DS_AMT_REPO_1_1_2,
              DSFILE.DS_NO_REPO_1_1_3,
              DSFILE.DS_AMT_REPO_1_1_3,
              DSFILE.DS_NO_REPO_1_1_4,
              DSFILE.DS_AMT_REPO_1_1_4,
              DSFILE.DS_NO_REPO_1_1_5,
              DSFILE.DS_AMT_REPO_1_1_5,
              DSFILE.DS_NO_REPO_1_2_1,
              DSFILE.DS_AMT_REPO_1_2_1,
              DSFILE.DS_NO_REPO_1_2_2,
              DSFILE.DS_AMT_REPO_1_2_2,
              DSFILE.DS_NO_REPO_1_2_3,
              DSFILE.DS_AMT_REPO_1_2_3,
              DSFILE.DS_NO_REPO_1_2_4,
              DSFILE.DS_AMT_REPO_1_2_4,
              DSFILE.DS_NO_REPO_1_2_5,
              DSFILE.DS_AMT_REPO_1_2_5,
              DSFILE.DS_NO_REPO_2_1_1,
              DSFILE.DS_AMT_REPO_2_1_1,
              DSFILE.DS_NO_REPO_2_1_2,
              DSFILE.DS_AMT_REPO_2_1_2,
              DSFILE.DS_NO_REPO_2_1_3,
              DSFILE.DS_AMT_REPO_2_1_3,
              DSFILE.DS_NO_REPO_2_1_4,
              DSFILE.DS_AMT_REPO_2_1_4,
              DSFILE.DS_NO_REPO_2_1_5,
              DSFILE.DS_AMT_REPO_2_1_5,
              DSFILE.DS_NO_REPO_2_2_1,
              DSFILE.DS_AMT_REPO_2_2_1,
              DSFILE.DS_NO_REPO_2_2_2,
              DSFILE.DS_AMT_REPO_2_2_2,
              DSFILE.DS_NO_REPO_2_2_3,
              DSFILE.DS_AMT_REPO_2_2_3,
              DSFILE.DS_NO_REPO_2_2_4,
              DSFILE.DS_AMT_REPO_2_2_4,
              DSFILE.DS_NO_REPO_2_2_5,
              DSFILE.DS_AMT_REPO_2_2_5,
              DSFILE.DS_NO_REPO_3_1_1,
              DSFILE.DS_AMT_REPO_3_1_1,
              DSFILE.DS_NO_REPO_3_1_2,
              DSFILE.DS_AMT_REPO_3_1_2,
              DSFILE.DS_NO_REPO_3_1_3,
              DSFILE.DS_AMT_REPO_3_1_3,
              DSFILE.DS_NO_REPO_3_1_4,
              DSFILE.DS_AMT_REPO_3_1_4,
              DSFILE.DS_NO_REPO_3_1_5,
              DSFILE.DS_AMT_REPO_3_1_5,
              DSFILE.DS_NO_REPO_3_2_1,
              DSFILE.DS_AMT_REPO_3_2_1,
              DSFILE.DS_NO_REPO_3_2_2,
              DSFILE.DS_AMT_REPO_3_2_2,
              DSFILE.DS_NO_REPO_3_2_3,
              DSFILE.DS_AMT_REPO_3_2_3,
              DSFILE.DS_NO_REPO_3_2_4,
              DSFILE.DS_AMT_REPO_3_2_4,
              DSFILE.DS_NO_REPO_3_2_5,
              DSFILE.DS_AMT_REPO_3_2_5,
              DSFILE.DS_UNITS_1_1_1,
              DSFILE.DS_OUTBAL_1_1_1,
              DSFILE.DS_UNITS_1_1_2,
              DSFILE.DS_OUTBAL_1_1_2,
              DSFILE.DS_UNITS_1_1_3,
              DSFILE.DS_OUTBAL_1_1_3,
              DSFILE.DS_UNITS_1_1_4,
              DSFILE.DS_OUTBAL_1_1_4,
              DSFILE.DS_UNITS_1_1_5,
              DSFILE.DS_OUTBAL_1_1_5,
              DSFILE.DS_UNITS_1_1_6,
              DSFILE.DS_OUTBAL_1_1_6,
              DSFILE.DS_UNITS_1_1_7,
              DSFILE.DS_OUTBAL_1_1_7,
              DSFILE.DS_UNITS_1_1_8,
              DSFILE.DS_OUTBAL_1_1_8,
              DSFILE.DS_UNITS_1_1_9,
              DSFILE.DS_OUTBAL_1_1_9,
              DSFILE.DS_UNITS_1_1_10,
              DSFILE.DS_OUTBAL_1_1_10,
              DSFILE.DS_UNITS_1_2_1,
              DSFILE.DS_OUTBAL_1_2_1,
              DSFILE.DS_UNITS_1_2_2,
              DSFILE.DS_OUTBAL_1_2_2,
              DSFILE.DS_UNITS_1_2_3,
              DSFILE.DS_OUTBAL_1_2_3,
              DSFILE.DS_UNITS_1_2_4,
              DSFILE.DS_OUTBAL_1_2_4,
              DSFILE.DS_UNITS_1_2_5,
              DSFILE.DS_OUTBAL_1_2_5,
              DSFILE.DS_UNITS_1_2_6,
              DSFILE.DS_OUTBAL_1_2_6,
              DSFILE.DS_UNITS_1_2_7,
              DSFILE.DS_OUTBAL_1_2_7,
              DSFILE.DS_UNITS_1_2_8,
              DSFILE.DS_OUTBAL_1_2_8,
              DSFILE.DS_UNITS_1_2_9,
              DSFILE.DS_OUTBAL_1_2_9,
              DSFILE.DS_UNITS_1_2_10,
              DSFILE.DS_OUTBAL_1_2_10,
              DSFILE.DS_UNITS_2_1_1,
              DSFILE.DS_OUTBAL_2_1_1,
              DSFILE.DS_UNITS_2_1_2,
              DSFILE.DS_OUTBAL_2_1_2,
              DSFILE.DS_UNITS_2_1_3,
              DSFILE.DS_OUTBAL_2_1_3,
              DSFILE.DS_UNITS_2_1_4,
              DSFILE.DS_OUTBAL_2_1_4,
              DSFILE.DS_UNITS_2_1_5,
              DSFILE.DS_OUTBAL_2_1_5,
              DSFILE.DS_UNITS_2_1_6,
              DSFILE.DS_OUTBAL_2_1_6,
              DSFILE.DS_UNITS_2_1_7,
              DSFILE.DS_OUTBAL_2_1_7,
              DSFILE.DS_UNITS_2_1_8,
              DSFILE.DS_OUTBAL_2_1_8,
              DSFILE.DS_UNITS_2_1_9,
              DSFILE.DS_OUTBAL_2_1_9,
              DSFILE.DS_UNITS_2_1_10,
              DSFILE.DS_OUTBAL_2_1_10,
              DSFILE.DS_UNITS_2_2_1,
              DSFILE.DS_OUTBAL_2_2_1,
              DSFILE.DS_UNITS_2_2_2,
              DSFILE.DS_OUTBAL_2_2_2,
              DSFILE.DS_UNITS_2_2_3,
              DSFILE.DS_OUTBAL_2_2_3,
              DSFILE.DS_UNITS_2_2_4,
              DSFILE.DS_OUTBAL_2_2_4,
              DSFILE.DS_UNITS_2_2_5,
              DSFILE.DS_OUTBAL_2_2_5,
              DSFILE.DS_UNITS_2_2_6,
              DSFILE.DS_OUTBAL_2_2_6,
              DSFILE.DS_UNITS_2_2_7,
              DSFILE.DS_OUTBAL_2_2_7,
              DSFILE.DS_UNITS_2_2_8,
              DSFILE.DS_OUTBAL_2_2_8,
              DSFILE.DS_UNITS_2_2_9,
              DSFILE.DS_OUTBAL_2_2_9,
              DSFILE.DS_UNITS_2_2_10,
              DSFILE.DS_OUTBAL_2_2_10,
              DSFILE.DS_UNITS_3_1_1,
              DSFILE.DS_OUTBAL_3_1_1,
              DSFILE.DS_UNITS_3_1_2,
              DSFILE.DS_OUTBAL_3_1_2,
              DSFILE.DS_UNITS_3_1_3,
              DSFILE.DS_OUTBAL_3_1_3,
              DSFILE.DS_UNITS_3_1_4,
              DSFILE.DS_OUTBAL_3_1_4,
              DSFILE.DS_UNITS_3_1_5,
              DSFILE.DS_OUTBAL_3_1_5,
              DSFILE.DS_UNITS_3_1_6,
              DSFILE.DS_OUTBAL_3_1_6,
              DSFILE.DS_UNITS_3_1_7,
              DSFILE.DS_OUTBAL_3_1_7,
              DSFILE.DS_UNITS_3_1_8,
              DSFILE.DS_OUTBAL_3_1_8,
              DSFILE.DS_UNITS_3_1_9,
              DSFILE.DS_OUTBAL_3_1_9,
              DSFILE.DS_UNITS_3_1_10,
              DSFILE.DS_OUTBAL_3_1_10,
              DSFILE.DS_UNITS_3_2_1,
              DSFILE.DS_OUTBAL_3_2_1,
              DSFILE.DS_UNITS_3_2_2,
              DSFILE.DS_OUTBAL_3_2_2,
              DSFILE.DS_UNITS_3_2_3,
              DSFILE.DS_OUTBAL_3_2_3,
              DSFILE.DS_UNITS_3_2_4,
              DSFILE.DS_OUTBAL_3_2_4,
              DSFILE.DS_UNITS_3_2_5,
              DSFILE.DS_OUTBAL_3_2_5,
              DSFILE.DS_UNITS_3_2_6,
              DSFILE.DS_OUTBAL_3_2_6,
              DSFILE.DS_UNITS_3_2_7,
              DSFILE.DS_OUTBAL_3_2_7,
              DSFILE.DS_UNITS_3_2_8,
              DSFILE.DS_OUTBAL_3_2_8,
              DSFILE.DS_UNITS_3_2_9,
              DSFILE.DS_OUTBAL_3_2_9,
              DSFILE.DS_UNITS_3_2_10,
              DSFILE.DS_OUTBAL_3_2_10,
              DSFILE.DS_UNITS_BNKRPT_1_1_1,
              DSFILE.DS_OUTBAL_BNKRPT_1_1_1,
              DSFILE.DS_UNITS_BNKRPT_1_1_2,
              DSFILE.DS_OUTBAL_BNKRPT_1_1_2,
              DSFILE.DS_UNITS_BNKRPT_1_1_3,
              DSFILE.DS_OUTBAL_BNKRPT_1_1_3,
              DSFILE.DS_UNITS_BNKRPT_1_1_4,
              DSFILE.DS_OUTBAL_BNKRPT_1_1_4,
              DSFILE.DS_UNITS_BNKRPT_1_1_5,
              DSFILE.DS_OUTBAL_BNKRPT_1_1_5,
              DSFILE.DS_UNITS_BNKRPT_1_1_6,
              DSFILE.DS_OUTBAL_BNKRPT_1_1_6,
              DSFILE.DS_UNITS_BNKRPT_1_1_7,
              DSFILE.DS_OUTBAL_BNKRPT_1_1_7,
              DSFILE.DS_UNITS_BNKRPT_1_1_8,
              DSFILE.DS_OUTBAL_BNKRPT_1_1_8,
              DSFILE.DS_UNITS_BNKRPT_1_1_9,
              DSFILE.DS_OUTBAL_BNKRPT_1_1_9,
              DSFILE.DS_UNITS_BNKRPT_1_1_10,
              DSFILE.DS_OUTBAL_BNKRPT_1_1_10,
              DSFILE.DS_UNITS_BNKRPT_1_2_1,
              DSFILE.DS_OUTBAL_BNKRPT_1_2_1,
              DSFILE.DS_UNITS_BNKRPT_1_2_2,
              DSFILE.DS_OUTBAL_BNKRPT_1_2_2,
              DSFILE.DS_UNITS_BNKRPT_1_2_3,
              DSFILE.DS_OUTBAL_BNKRPT_1_2_3,
              DSFILE.DS_UNITS_BNKRPT_1_2_4,
              DSFILE.DS_OUTBAL_BNKRPT_1_2_4,
              DSFILE.DS_UNITS_BNKRPT_1_2_5,
              DSFILE.DS_OUTBAL_BNKRPT_1_2_5,
              DSFILE.DS_UNITS_BNKRPT_1_2_6,
              DSFILE.DS_OUTBAL_BNKRPT_1_2_6,
              DSFILE.DS_UNITS_BNKRPT_1_2_7,
              DSFILE.DS_OUTBAL_BNKRPT_1_2_7,
              DSFILE.DS_UNITS_BNKRPT_1_2_8,
              DSFILE.DS_OUTBAL_BNKRPT_1_2_8,
              DSFILE.DS_UNITS_BNKRPT_1_2_9,
              DSFILE.DS_OUTBAL_BNKRPT_1_2_9,
              DSFILE.DS_UNITS_BNKRPT_1_2_10,
              DSFILE.DS_OUTBAL_BNKRPT_1_2_10,
              DSFILE.DS_UNITS_BNKRPT_2_1_1,
              DSFILE.DS_OUTBAL_BNKRPT_2_1_1,
              DSFILE.DS_UNITS_BNKRPT_2_1_2,
              DSFILE.DS_OUTBAL_BNKRPT_2_1_2,
              DSFILE.DS_UNITS_BNKRPT_2_1_3,
              DSFILE.DS_OUTBAL_BNKRPT_2_1_3,
              DSFILE.DS_UNITS_BNKRPT_2_1_4,
              DSFILE.DS_OUTBAL_BNKRPT_2_1_4,
              DSFILE.DS_UNITS_BNKRPT_2_1_5,
              DSFILE.DS_OUTBAL_BNKRPT_2_1_5,
              DSFILE.DS_UNITS_BNKRPT_2_1_6,
              DSFILE.DS_OUTBAL_BNKRPT_2_1_6,
              DSFILE.DS_UNITS_BNKRPT_2_1_7,
              DSFILE.DS_OUTBAL_BNKRPT_2_1_7,
              DSFILE.DS_UNITS_BNKRPT_2_1_8,
              DSFILE.DS_OUTBAL_BNKRPT_2_1_8,
              DSFILE.DS_UNITS_BNKRPT_2_1_9,
              DSFILE.DS_OUTBAL_BNKRPT_2_1_9,
              DSFILE.DS_UNITS_BNKRPT_2_1_10,
              DSFILE.DS_OUTBAL_BNKRPT_2_1_10,
              DSFILE.DS_UNITS_BNKRPT_2_2_1,
              DSFILE.DS_OUTBAL_BNKRPT_2_2_1,
              DSFILE.DS_UNITS_BNKRPT_2_2_2,
              DSFILE.DS_OUTBAL_BNKRPT_2_2_2,
              DSFILE.DS_UNITS_BNKRPT_2_2_3,
              DSFILE.DS_OUTBAL_BNKRPT_2_2_3,
              DSFILE.DS_UNITS_BNKRPT_2_2_4,
              DSFILE.DS_OUTBAL_BNKRPT_2_2_4,
              DSFILE.DS_UNITS_BNKRPT_2_2_5,
              DSFILE.DS_OUTBAL_BNKRPT_2_2_5,
              DSFILE.DS_UNITS_BNKRPT_2_2_6,
              DSFILE.DS_OUTBAL_BNKRPT_2_2_6,
              DSFILE.DS_UNITS_BNKRPT_2_2_7,
              DSFILE.DS_OUTBAL_BNKRPT_2_2_7,
              DSFILE.DS_UNITS_BNKRPT_2_2_8,
              DSFILE.DS_OUTBAL_BNKRPT_2_2_8,
              DSFILE.DS_UNITS_BNKRPT_2_2_9,
              DSFILE.DS_OUTBAL_BNKRPT_2_2_9,
              DSFILE.DS_UNITS_BNKRPT_2_2_10,
              DSFILE.DS_OUTBAL_BNKRPT_2_2_10,
              DSFILE.DS_UNITS_BNKRPT_3_1_1,
              DSFILE.DS_OUTBAL_BNKRPT_3_1_1,
              DSFILE.DS_UNITS_BNKRPT_3_1_2,
              DSFILE.DS_OUTBAL_BNKRPT_3_1_2,
              DSFILE.DS_UNITS_BNKRPT_3_1_3,
              DSFILE.DS_OUTBAL_BNKRPT_3_1_3,
              DSFILE.DS_UNITS_BNKRPT_3_1_4,
              DSFILE.DS_OUTBAL_BNKRPT_3_1_4,
              DSFILE.DS_UNITS_BNKRPT_3_1_5,
              DSFILE.DS_OUTBAL_BNKRPT_3_1_5,
              DSFILE.DS_UNITS_BNKRPT_3_1_6,
              DSFILE.DS_OUTBAL_BNKRPT_3_1_6,
              DSFILE.DS_UNITS_BNKRPT_3_1_7,
              DSFILE.DS_OUTBAL_BNKRPT_3_1_7,
              DSFILE.DS_UNITS_BNKRPT_3_1_8,
              DSFILE.DS_OUTBAL_BNKRPT_3_1_8,
              DSFILE.DS_UNITS_BNKRPT_3_1_9,
              DSFILE.DS_OUTBAL_BNKRPT_3_1_9,
              DSFILE.DS_UNITS_BNKRPT_3_1_10,
              DSFILE.DS_OUTBAL_BNKRPT_3_1_10,
              DSFILE.DS_UNITS_BNKRPT_3_2_1,
              DSFILE.DS_OUTBAL_BNKRPT_3_2_1,
              DSFILE.DS_UNITS_BNKRPT_3_2_2,
              DSFILE.DS_OUTBAL_BNKRPT_3_2_2,
              DSFILE.DS_UNITS_BNKRPT_3_2_3,
              DSFILE.DS_OUTBAL_BNKRPT_3_2_3,
              DSFILE.DS_UNITS_BNKRPT_3_2_4,
              DSFILE.DS_OUTBAL_BNKRPT_3_2_4,
              DSFILE.DS_UNITS_BNKRPT_3_2_5,
              DSFILE.DS_OUTBAL_BNKRPT_3_2_5,
              DSFILE.DS_UNITS_BNKRPT_3_2_6,
              DSFILE.DS_OUTBAL_BNKRPT_3_2_6,
              DSFILE.DS_UNITS_BNKRPT_3_2_7,
              DSFILE.DS_OUTBAL_BNKRPT_3_2_7,
              DSFILE.DS_UNITS_BNKRPT_3_2_8,
              DSFILE.DS_OUTBAL_BNKRPT_3_2_8,
              DSFILE.DS_UNITS_BNKRPT_3_2_9,
              DSFILE.DS_OUTBAL_BNKRPT_3_2_9,
              DSFILE.DS_UNITS_BNKRPT_3_2_10,
              DSFILE.DS_OUTBAL_BNKRPT_3_2_10,
              DSFILE.DS_UNEARNED_AMT_1_1_1,
              DSFILE.DS_UNEARNED_AMT_1_1_2,
              DSFILE.DS_UNEARNED_AMT_1_1_3,
              DSFILE.DS_UNEARNED_AMT_1_1_4,
              DSFILE.DS_UNEARNED_AMT_1_1_5,
              DSFILE.DS_UNEARNED_AMT_1_1_6,
              DSFILE.DS_UNEARNED_AMT_1_1_7,
              DSFILE.DS_UNEARNED_AMT_1_1_8,
              DSFILE.DS_UNEARNED_AMT_1_1_9,
              DSFILE.DS_UNEARNED_AMT_1_1_10,
              DSFILE.DS_UNEARNED_AMT_1_2_1,
              DSFILE.DS_UNEARNED_AMT_1_2_2,
              DSFILE.DS_UNEARNED_AMT_1_2_3,
              DSFILE.DS_UNEARNED_AMT_1_2_4,
              DSFILE.DS_UNEARNED_AMT_1_2_5,
              DSFILE.DS_UNEARNED_AMT_1_2_6,
              DSFILE.DS_UNEARNED_AMT_1_2_7,
              DSFILE.DS_UNEARNED_AMT_1_2_8,
              DSFILE.DS_UNEARNED_AMT_1_2_9,
              DSFILE.DS_UNEARNED_AMT_1_2_10,
              DSFILE.DS_UNEARNED_AMT_2_1_1,
              DSFILE.DS_UNEARNED_AMT_2_1_2,
              DSFILE.DS_UNEARNED_AMT_2_1_3,
              DSFILE.DS_UNEARNED_AMT_2_1_4,
              DSFILE.DS_UNEARNED_AMT_2_1_5,
              DSFILE.DS_UNEARNED_AMT_2_1_6,
              DSFILE.DS_UNEARNED_AMT_2_1_7,
              DSFILE.DS_UNEARNED_AMT_2_1_8,
              DSFILE.DS_UNEARNED_AMT_2_1_9,
              DSFILE.DS_UNEARNED_AMT_2_1_10,
              DSFILE.DS_UNEARNED_AMT_2_2_1,
              DSFILE.DS_UNEARNED_AMT_2_2_2,
              DSFILE.DS_UNEARNED_AMT_2_2_3,
              DSFILE.DS_UNEARNED_AMT_2_2_4,
              DSFILE.DS_UNEARNED_AMT_2_2_5,
              DSFILE.DS_UNEARNED_AMT_2_2_6,
              DSFILE.DS_UNEARNED_AMT_2_2_7,
              DSFILE.DS_UNEARNED_AMT_2_2_8,
              DSFILE.DS_UNEARNED_AMT_2_2_9,
              DSFILE.DS_UNEARNED_AMT_2_2_10,
              DSFILE.DS_UNEARNED_AMT_3_1_1,
              DSFILE.DS_UNEARNED_AMT_3_1_2,
              DSFILE.DS_UNEARNED_AMT_3_1_3,
              DSFILE.DS_UNEARNED_AMT_3_1_4,
              DSFILE.DS_UNEARNED_AMT_3_1_5,
              DSFILE.DS_UNEARNED_AMT_3_1_6,
              DSFILE.DS_UNEARNED_AMT_3_1_7,
              DSFILE.DS_UNEARNED_AMT_3_1_8,
              DSFILE.DS_UNEARNED_AMT_3_1_9,
              DSFILE.DS_UNEARNED_AMT_3_1_10,
              DSFILE.DS_UNEARNED_AMT_3_2_1,
              DSFILE.DS_UNEARNED_AMT_3_2_2,
              DSFILE.DS_UNEARNED_AMT_3_2_3,
              DSFILE.DS_UNEARNED_AMT_3_2_4,
              DSFILE.DS_UNEARNED_AMT_3_2_5,
              DSFILE.DS_UNEARNED_AMT_3_2_6,
              DSFILE.DS_UNEARNED_AMT_3_2_7,
              DSFILE.DS_UNEARNED_AMT_3_2_8,
              DSFILE.DS_UNEARNED_AMT_3_2_9,
              DSFILE.DS_UNEARNED_AMT_3_2_10,
              DSFILE.DS_DLDISC_VOL_1,
              DSFILE.DS_DLDISC_VOL_2,
              DSFILE.DS_DLDISC_VOL_3,
              DSFILE.DS_AFPDUEDL_MEMO,
              DSFILE.DS_UNPAID_MANUAL,
              DSFILE.DS_LTOUCH_DATE)
            VALUES
             (:QDS-DATE-CCYYMM,
              :QDS-BRANCH,
              :QDS-NUM,
              :QDS-CLASS,
              :QDS-DLRVPRIN-1,
              :QDS-DLRVPRIN-2,
              :QDS-DLRVPRIN-3,
              :QDS-DLRVCHGS-1,
              :QDS-DLRVCHGS-2,
              :QDS-DLRVCHGS-3,
              :QDS-DLPARVHELD,
              :QDS-UNPAIDCK,
              :QDS-DLDISC,
              :QDS-DLPARVPAID,
              :QDS-SUMWGTTM,
              :QDS-SUMWGTAV,
              :QDS-SUMWGTTA,
              :QDS-RCOURSE-UNITS-1,
              :QDS-RCOURSE-UNITS-2,
              :QDS-RCOURSE-UNITS-3,
              :QDS-RCOURSE-AMT-1,
              :QDS-RCOURSE-AMT-2,
              :QDS-RCOURSE-AMT-3,
              :QDS-NPURUNIT-1,
              :QDS-NPURUNIT-2,
              :QDS-NPURUNIT-3,
              :QDS-NPURAMT-1,
              :QDS-NPURAMT-2,
              :QDS-NPURAMT-3,
              :QDS-OPURUNIT-1,
              :QDS-OPURUNIT-2,
              :QDS-OPURUNIT-3,
              :QDS-OPURAMT-1,
              :QDS-OPURAMT-2,
              :QDS-OPURAMT-3,
              :QDS-RNEWUNIT-1,
              :QDS-RNEWUNIT-2,
              :QDS-RNEWUNIT-3,
              :QDS-RNEWAMT-1,
              :QDS-RNEWAMT-2,
              :QDS-RNEWAMT-3,
              :QDS-PAIDUNIT-1,
              :QDS-PAIDUNIT-2,
              :QDS-PAIDUNIT-3,
              :QDS-PAIDAMT-1,
              :QDS-PAIDAMT-2,
              :QDS-PAIDAMT-3,
              :QDS-TODLUNIT-1,
              :QDS-TODLUNIT-2,
              :QDS-TODLUNIT-3,
              :QDS-TODLAMT-1,
              :QDS-TODLAMT-2,
              :QDS-TODLAMT-3,
              :QDS-OUTSUNIT-1,
              :QDS-OUTSUNIT-2,
              :QDS-OUTSUNIT-3,
              :QDS-OUTSAMT-1,
              :QDS-OUTSAMT-2,
              :QDS-OUTSAMT-3,
              :QDS-PLOSUNIT-1,
              :QDS-PLOSUNIT-2,
              :QDS-PLOSUNIT-3,
              :QDS-PLOSAMT-1,
              :QDS-PLOSAMT-2,
              :QDS-PLOSAMT-3,
              :QDS-RCPLUNIT-1,
              :QDS-RCPLUNIT-2,
              :QDS-RCPLUNIT-3,
              :QDS-RCPLAMT-1,
              :QDS-RCPLAMT-2,
              :QDS-RCPLAMT-3,
              :QDS-JDOSUNIT-1,
              :QDS-JDOSUNIT-2,
              :QDS-JDOSUNIT-3,
              :QDS-JDOSAMT-1,
              :QDS-JDOSAMT-2,
              :QDS-JDOSAMT-3,
              :QDS-DELUNEARN-DATE,
              :QDS-PROCEEDS-1,
              :QDS-PROCEEDS-2,
              :QDS-NO-REPO-1-1-1,
              :QDS-AMT-REPO-1-1-1,
              :QDS-NO-REPO-1-1-2,
              :QDS-AMT-REPO-1-1-2,
              :QDS-NO-REPO-1-1-3,
              :QDS-AMT-REPO-1-1-3,
              :QDS-NO-REPO-1-1-4,
              :QDS-AMT-REPO-1-1-4,
              :QDS-NO-REPO-1-1-5,
              :QDS-AMT-REPO-1-1-5,
              :QDS-NO-REPO-1-2-1,
              :QDS-AMT-REPO-1-2-1,
              :QDS-NO-REPO-1-2-2,
              :QDS-AMT-REPO-1-2-2,
              :QDS-NO-REPO-1-2-3,
              :QDS-AMT-REPO-1-2-3,
              :QDS-NO-REPO-1-2-4,
              :QDS-AMT-REPO-1-2-4,
              :QDS-NO-REPO-1-2-5,
              :QDS-AMT-REPO-1-2-5,
              :QDS-NO-REPO-2-1-1,
              :QDS-AMT-REPO-2-1-1,
              :QDS-NO-REPO-2-1-2,
              :QDS-AMT-REPO-2-1-2,
              :QDS-NO-REPO-2-1-3,
              :QDS-AMT-REPO-2-1-3,
              :QDS-NO-REPO-2-1-4,
              :QDS-AMT-REPO-2-1-4,
              :QDS-NO-REPO-2-1-5,
              :QDS-AMT-REPO-2-1-5,
              :QDS-NO-REPO-2-2-1,
              :QDS-AMT-REPO-2-2-1,
              :QDS-NO-REPO-2-2-2,
              :QDS-AMT-REPO-2-2-2,
              :QDS-NO-REPO-2-2-3,
              :QDS-AMT-REPO-2-2-3,
              :QDS-NO-REPO-2-2-4,
              :QDS-AMT-REPO-2-2-4,
              :QDS-NO-REPO-2-2-5,
              :QDS-AMT-REPO-2-2-5,
              :QDS-NO-REPO-3-1-1,
              :QDS-AMT-REPO-3-1-1,
              :QDS-NO-REPO-3-1-2,
              :QDS-AMT-REPO-3-1-2,
              :QDS-NO-REPO-3-1-3,
              :QDS-AMT-REPO-3-1-3,
              :QDS-NO-REPO-3-1-4,
              :QDS-AMT-REPO-3-1-4,
              :QDS-NO-REPO-3-1-5,
              :QDS-AMT-REPO-3-1-5,
              :QDS-NO-REPO-3-2-1,
              :QDS-AMT-REPO-3-2-1,
              :QDS-NO-REPO-3-2-2,
              :QDS-AMT-REPO-3-2-2,
              :QDS-NO-REPO-3-2-3,
              :QDS-AMT-REPO-3-2-3,
              :QDS-NO-REPO-3-2-4,
              :QDS-AMT-REPO-3-2-4,
              :QDS-NO-REPO-3-2-5,
              :QDS-AMT-REPO-3-2-5,
              :QDS-UNITS-1-1-1,
              :QDS-OUTBAL-1-1-1,
              :QDS-UNITS-1-1-2,
              :QDS-OUTBAL-1-1-2,
              :QDS-UNITS-1-1-3,
              :QDS-OUTBAL-1-1-3,
              :QDS-UNITS-1-1-4,
              :QDS-OUTBAL-1-1-4,
              :QDS-UNITS-1-1-5,
              :QDS-OUTBAL-1-1-5,
              :QDS-UNITS-1-1-6,
              :QDS-OUTBAL-1-1-6,
              :QDS-UNITS-1-1-7,
              :QDS-OUTBAL-1-1-7,
              :QDS-UNITS-1-1-8,
              :QDS-OUTBAL-1-1-8,
              :QDS-UNITS-1-1-9,
              :QDS-OUTBAL-1-1-9,
              :QDS-UNITS-1-1-10,
              :QDS-OUTBAL-1-1-10,
              :QDS-UNITS-1-2-1,
              :QDS-OUTBAL-1-2-1,
              :QDS-UNITS-1-2-2,
              :QDS-OUTBAL-1-2-2,
              :QDS-UNITS-1-2-3,
              :QDS-OUTBAL-1-2-3,
              :QDS-UNITS-1-2-4,
              :QDS-OUTBAL-1-2-4,
              :QDS-UNITS-1-2-5,
              :QDS-OUTBAL-1-2-5,
              :QDS-UNITS-1-2-6,
              :QDS-OUTBAL-1-2-6,
              :QDS-UNITS-1-2-7,
              :QDS-OUTBAL-1-2-7,
              :QDS-UNITS-1-2-8,
              :QDS-OUTBAL-1-2-8,
              :QDS-UNITS-1-2-9,
              :QDS-OUTBAL-1-2-9,
              :QDS-UNITS-1-2-10,
              :QDS-OUTBAL-1-2-10,
              :QDS-UNITS-2-1-1,
              :QDS-OUTBAL-2-1-1,
              :QDS-UNITS-2-1-2,
              :QDS-OUTBAL-2-1-2,
              :QDS-UNITS-2-1-3,
              :QDS-OUTBAL-2-1-3,
              :QDS-UNITS-2-1-4,
              :QDS-OUTBAL-2-1-4,
              :QDS-UNITS-2-1-5,
              :QDS-OUTBAL-2-1-5,
              :QDS-UNITS-2-1-6,
              :QDS-OUTBAL-2-1-6,
              :QDS-UNITS-2-1-7,
              :QDS-OUTBAL-2-1-7,
              :QDS-UNITS-2-1-8,
              :QDS-OUTBAL-2-1-8,
              :QDS-UNITS-2-1-9,
              :QDS-OUTBAL-2-1-9,
              :QDS-UNITS-2-1-10,
              :QDS-OUTBAL-2-1-10,
              :QDS-UNITS-2-2-1,
              :QDS-OUTBAL-2-2-1,
              :QDS-UNITS-2-2-2,
              :QDS-OUTBAL-2-2-2,
              :QDS-UNITS-2-2-3,
              :QDS-OUTBAL-2-2-3,
              :QDS-UNITS-2-2-4,
              :QDS-OUTBAL-2-2-4,
              :QDS-UNITS-2-2-5,
              :QDS-OUTBAL-2-2-5,
              :QDS-UNITS-2-2-6,
              :QDS-OUTBAL-2-2-6,
              :QDS-UNITS-2-2-7,
              :QDS-OUTBAL-2-2-7,
              :QDS-UNITS-2-2-8,
              :QDS-OUTBAL-2-2-8,
              :QDS-UNITS-2-2-9,
              :QDS-OUTBAL-2-2-9,
              :QDS-UNITS-2-2-10,
              :QDS-OUTBAL-2-2-10,
              :QDS-UNITS-3-1-1,
              :QDS-OUTBAL-3-1-1,
              :QDS-UNITS-3-1-2,
              :QDS-OUTBAL-3-1-2,
              :QDS-UNITS-3-1-3,
              :QDS-OUTBAL-3-1-3,
              :QDS-UNITS-3-1-4,
              :QDS-OUTBAL-3-1-4,
              :QDS-UNITS-3-1-5,
              :QDS-OUTBAL-3-1-5,
              :QDS-UNITS-3-1-6,
              :QDS-OUTBAL-3-1-6,
              :QDS-UNITS-3-1-7,
              :QDS-OUTBAL-3-1-7,
              :QDS-UNITS-3-1-8,
              :QDS-OUTBAL-3-1-8,
              :QDS-UNITS-3-1-9,
              :QDS-OUTBAL-3-1-9,
              :QDS-UNITS-3-1-10,
              :QDS-OUTBAL-3-1-10,
              :QDS-UNITS-3-2-1,
              :QDS-OUTBAL-3-2-1,
              :QDS-UNITS-3-2-2,
              :QDS-OUTBAL-3-2-2,
              :QDS-UNITS-3-2-3,
              :QDS-OUTBAL-3-2-3,
              :QDS-UNITS-3-2-4,
              :QDS-OUTBAL-3-2-4,
              :QDS-UNITS-3-2-5,
              :QDS-OUTBAL-3-2-5,
              :QDS-UNITS-3-2-6,
              :QDS-OUTBAL-3-2-6,
              :QDS-UNITS-3-2-7,
              :QDS-OUTBAL-3-2-7,
              :QDS-UNITS-3-2-8,
              :QDS-OUTBAL-3-2-8,
              :QDS-UNITS-3-2-9,
              :QDS-OUTBAL-3-2-9,
              :QDS-UNITS-3-2-10,
              :QDS-OUTBAL-3-2-10,
              :QDS-UNITS-BNKRPT-1-1-1,
              :QDS-OUTBAL-BNKRPT-1-1-1,
              :QDS-UNITS-BNKRPT-1-1-2,
              :QDS-OUTBAL-BNKRPT-1-1-2,
              :QDS-UNITS-BNKRPT-1-1-3,
              :QDS-OUTBAL-BNKRPT-1-1-3,
              :QDS-UNITS-BNKRPT-1-1-4,
              :QDS-OUTBAL-BNKRPT-1-1-4,
              :QDS-UNITS-BNKRPT-1-1-5,
              :QDS-OUTBAL-BNKRPT-1-1-5,
              :QDS-UNITS-BNKRPT-1-1-6,
              :QDS-OUTBAL-BNKRPT-1-1-6,
              :QDS-UNITS-BNKRPT-1-1-7,
              :QDS-OUTBAL-BNKRPT-1-1-7,
              :QDS-UNITS-BNKRPT-1-1-8,
              :QDS-OUTBAL-BNKRPT-1-1-8,
              :QDS-UNITS-BNKRPT-1-1-9,
              :QDS-OUTBAL-BNKRPT-1-1-9,
              :QDS-UNITS-BNKRPT-1-1-10,
              :QDS-OUTBAL-BNKRPT-1-1-10,
              :QDS-UNITS-BNKRPT-1-2-1,
              :QDS-OUTBAL-BNKRPT-1-2-1,
              :QDS-UNITS-BNKRPT-1-2-2,
              :QDS-OUTBAL-BNKRPT-1-2-2,
              :QDS-UNITS-BNKRPT-1-2-3,
              :QDS-OUTBAL-BNKRPT-1-2-3,
              :QDS-UNITS-BNKRPT-1-2-4,
              :QDS-OUTBAL-BNKRPT-1-2-4,
              :QDS-UNITS-BNKRPT-1-2-5,
              :QDS-OUTBAL-BNKRPT-1-2-5,
              :QDS-UNITS-BNKRPT-1-2-6,
              :QDS-OUTBAL-BNKRPT-1-2-6,
              :QDS-UNITS-BNKRPT-1-2-7,
              :QDS-OUTBAL-BNKRPT-1-2-7,
              :QDS-UNITS-BNKRPT-1-2-8,
              :QDS-OUTBAL-BNKRPT-1-2-8,
              :QDS-UNITS-BNKRPT-1-2-9,
              :QDS-OUTBAL-BNKRPT-1-2-9,
              :QDS-UNITS-BNKRPT-1-2-10,
              :QDS-OUTBAL-BNKRPT-1-2-10,
              :QDS-UNITS-BNKRPT-2-1-1,
              :QDS-OUTBAL-BNKRPT-2-1-1,
              :QDS-UNITS-BNKRPT-2-1-2,
              :QDS-OUTBAL-BNKRPT-2-1-2,
              :QDS-UNITS-BNKRPT-2-1-3,
              :QDS-OUTBAL-BNKRPT-2-1-3,
              :QDS-UNITS-BNKRPT-2-1-4,
              :QDS-OUTBAL-BNKRPT-2-1-4,
              :QDS-UNITS-BNKRPT-2-1-5,
              :QDS-OUTBAL-BNKRPT-2-1-5,
              :QDS-UNITS-BNKRPT-2-1-6,
              :QDS-OUTBAL-BNKRPT-2-1-6,
              :QDS-UNITS-BNKRPT-2-1-7,
              :QDS-OUTBAL-BNKRPT-2-1-7,
              :QDS-UNITS-BNKRPT-2-1-8,
              :QDS-OUTBAL-BNKRPT-2-1-8,
              :QDS-UNITS-BNKRPT-2-1-9,
              :QDS-OUTBAL-BNKRPT-2-1-9,
              :QDS-UNITS-BNKRPT-2-1-10,
              :QDS-OUTBAL-BNKRPT-2-1-10,
              :QDS-UNITS-BNKRPT-2-2-1,
              :QDS-OUTBAL-BNKRPT-2-2-1,
              :QDS-UNITS-BNKRPT-2-2-2,
              :QDS-OUTBAL-BNKRPT-2-2-2,
              :QDS-UNITS-BNKRPT-2-2-3,
              :QDS-OUTBAL-BNKRPT-2-2-3,
              :QDS-UNITS-BNKRPT-2-2-4,
              :QDS-OUTBAL-BNKRPT-2-2-4,
              :QDS-UNITS-BNKRPT-2-2-5,
              :QDS-OUTBAL-BNKRPT-2-2-5,
              :QDS-UNITS-BNKRPT-2-2-6,
              :QDS-OUTBAL-BNKRPT-2-2-6,
              :QDS-UNITS-BNKRPT-2-2-7,
              :QDS-OUTBAL-BNKRPT-2-2-7,
              :QDS-UNITS-BNKRPT-2-2-8,
              :QDS-OUTBAL-BNKRPT-2-2-8,
              :QDS-UNITS-BNKRPT-2-2-9,
              :QDS-OUTBAL-BNKRPT-2-2-9,
              :QDS-UNITS-BNKRPT-2-2-10,
              :QDS-OUTBAL-BNKRPT-2-2-10,
              :QDS-UNITS-BNKRPT-3-1-1,
              :QDS-OUTBAL-BNKRPT-3-1-1,
              :QDS-UNITS-BNKRPT-3-1-2,
              :QDS-OUTBAL-BNKRPT-3-1-2,
              :QDS-UNITS-BNKRPT-3-1-3,
              :QDS-OUTBAL-BNKRPT-3-1-3,
              :QDS-UNITS-BNKRPT-3-1-4,
              :QDS-OUTBAL-BNKRPT-3-1-4,
              :QDS-UNITS-BNKRPT-3-1-5,
              :QDS-OUTBAL-BNKRPT-3-1-5,
              :QDS-UNITS-BNKRPT-3-1-6,
              :QDS-OUTBAL-BNKRPT-3-1-6,
              :QDS-UNITS-BNKRPT-3-1-7,
              :QDS-OUTBAL-BNKRPT-3-1-7,
              :QDS-UNITS-BNKRPT-3-1-8,
              :QDS-OUTBAL-BNKRPT-3-1-8,
              :QDS-UNITS-BNKRPT-3-1-9,
              :QDS-OUTBAL-BNKRPT-3-1-9,
              :QDS-UNITS-BNKRPT-3-1-10,
              :QDS-OUTBAL-BNKRPT-3-1-10,
              :QDS-UNITS-BNKRPT-3-2-1,
              :QDS-OUTBAL-BNKRPT-3-2-1,
              :QDS-UNITS-BNKRPT-3-2-2,
              :QDS-OUTBAL-BNKRPT-3-2-2,
              :QDS-UNITS-BNKRPT-3-2-3,
              :QDS-OUTBAL-BNKRPT-3-2-3,
              :QDS-UNITS-BNKRPT-3-2-4,
              :QDS-OUTBAL-BNKRPT-3-2-4,
              :QDS-UNITS-BNKRPT-3-2-5,
              :QDS-OUTBAL-BNKRPT-3-2-5,
              :QDS-UNITS-BNKRPT-3-2-6,
              :QDS-OUTBAL-BNKRPT-3-2-6,
              :QDS-UNITS-BNKRPT-3-2-7,
              :QDS-OUTBAL-BNKRPT-3-2-7,
              :QDS-UNITS-BNKRPT-3-2-8,
              :QDS-OUTBAL-BNKRPT-3-2-8,
              :QDS-UNITS-BNKRPT-3-2-9,
              :QDS-OUTBAL-BNKRPT-3-2-9,
              :QDS-UNITS-BNKRPT-3-2-10,
              :QDS-OUTBAL-BNKRPT-3-2-10,
              :QDS-UNEARNED-AMT-1-1-1,
              :QDS-UNEARNED-AMT-1-1-2,
              :QDS-UNEARNED-AMT-1-1-3,
              :QDS-UNEARNED-AMT-1-1-4,
              :QDS-UNEARNED-AMT-1-1-5,
              :QDS-UNEARNED-AMT-1-1-6,
              :QDS-UNEARNED-AMT-1-1-7,
              :QDS-UNEARNED-AMT-1-1-8,
              :QDS-UNEARNED-AMT-1-1-9,
              :QDS-UNEARNED-AMT-1-1-10,
              :QDS-UNEARNED-AMT-1-2-1,
              :QDS-UNEARNED-AMT-1-2-2,
              :QDS-UNEARNED-AMT-1-2-3,
              :QDS-UNEARNED-AMT-1-2-4,
              :QDS-UNEARNED-AMT-1-2-5,
              :QDS-UNEARNED-AMT-1-2-6,
              :QDS-UNEARNED-AMT-1-2-7,
              :QDS-UNEARNED-AMT-1-2-8,
              :QDS-UNEARNED-AMT-1-2-9,
              :QDS-UNEARNED-AMT-1-2-10,
              :QDS-UNEARNED-AMT-2-1-1,
              :QDS-UNEARNED-AMT-2-1-2,
              :QDS-UNEARNED-AMT-2-1-3,
              :QDS-UNEARNED-AMT-2-1-4,
              :QDS-UNEARNED-AMT-2-1-5,
              :QDS-UNEARNED-AMT-2-1-6,
              :QDS-UNEARNED-AMT-2-1-7,
              :QDS-UNEARNED-AMT-2-1-8,
              :QDS-UNEARNED-AMT-2-1-9,
              :QDS-UNEARNED-AMT-2-1-10,
              :QDS-UNEARNED-AMT-2-2-1,
              :QDS-UNEARNED-AMT-2-2-2,
              :QDS-UNEARNED-AMT-2-2-3,
              :QDS-UNEARNED-AMT-2-2-4,
              :QDS-UNEARNED-AMT-2-2-5,
              :QDS-UNEARNED-AMT-2-2-6,
              :QDS-UNEARNED-AMT-2-2-7,
              :QDS-UNEARNED-AMT-2-2-8,
              :QDS-UNEARNED-AMT-2-2-9,
              :QDS-UNEARNED-AMT-2-2-10,
              :QDS-UNEARNED-AMT-3-1-1,
              :QDS-UNEARNED-AMT-3-1-2,
              :QDS-UNEARNED-AMT-3-1-3,
              :QDS-UNEARNED-AMT-3-1-4,
              :QDS-UNEARNED-AMT-3-1-5,
              :QDS-UNEARNED-AMT-3-1-6,
              :QDS-UNEARNED-AMT-3-1-7,
              :QDS-UNEARNED-AMT-3-1-8,
              :QDS-UNEARNED-AMT-3-1-9,
              :QDS-UNEARNED-AMT-3-1-10,
              :QDS-UNEARNED-AMT-3-2-1,
              :QDS-UNEARNED-AMT-3-2-2,
              :QDS-UNEARNED-AMT-3-2-3,
              :QDS-UNEARNED-AMT-3-2-4,
              :QDS-UNEARNED-AMT-3-2-5,
              :QDS-UNEARNED-AMT-3-2-6,
              :QDS-UNEARNED-AMT-3-2-7,
              :QDS-UNEARNED-AMT-3-2-8,
              :QDS-UNEARNED-AMT-3-2-9,
              :QDS-UNEARNED-AMT-3-2-10,
              :QDS-DLDISC-VOL-1,
              :QDS-DLDISC-VOL-2,
              :QDS-DLDISC-VOL-3,
              :QDS-AFPDUEDL-MEMO,
              :QDS-UNPAID-MANUAL,
              :QDS-LTOUCH-DATE)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-DS1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

           MOVE  '1' TO SQL-UPDATE-CONN-USE-SW. *> USE C1 CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

      *-----------------------------------------------------------------
       REWRITE-DS1-FILE.

           MOVE DS-PATH-OWNBR  TO DS-BRANCH.
           ACCEPT DS-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE DS-PATH-SQL    TO E-FILE.
           MOVE DS1-KEY        TO E-KEYX.
           PERFORM SET-DS-FIELDS.

           MOVE '2' TO SQL-UPDATE-CONN-USE-SW. *> USE UPDATE_CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

           EXEC SQL
            UPDATE DBO.DSFILE
             SET
              DSFILE.DS_DATE_CCYYMM = :QDS-DATE-CCYYMM,
              DSFILE.DS_BRANCH = :QDS-BRANCH,
              DSFILE.DS_NUM = :QDS-NUM,
              DSFILE.DS_CLASS = :QDS-CLASS,
              DSFILE.DS_DLRVPRIN_1 = :QDS-DLRVPRIN-1,
              DSFILE.DS_DLRVPRIN_2 = :QDS-DLRVPRIN-2,
              DSFILE.DS_DLRVPRIN_3 = :QDS-DLRVPRIN-3,
              DSFILE.DS_DLRVCHGS_1 = :QDS-DLRVCHGS-1,
              DSFILE.DS_DLRVCHGS_2 = :QDS-DLRVCHGS-2,
              DSFILE.DS_DLRVCHGS_3 = :QDS-DLRVCHGS-3,
              DSFILE.DS_DLPARVHELD = :QDS-DLPARVHELD,
              DSFILE.DS_UNPAIDCK = :QDS-UNPAIDCK,
              DSFILE.DS_DLDISC = :QDS-DLDISC,
              DSFILE.DS_DLPARVPAID = :QDS-DLPARVPAID,
              DSFILE.DS_SUMWGTTM = :QDS-SUMWGTTM,
              DSFILE.DS_SUMWGTAV = :QDS-SUMWGTAV,
              DSFILE.DS_SUMWGTTA = :QDS-SUMWGTTA,
              DSFILE.DS_RCOURSE_UNITS_1 = :QDS-RCOURSE-UNITS-1,
              DSFILE.DS_RCOURSE_UNITS_2 = :QDS-RCOURSE-UNITS-2,
              DSFILE.DS_RCOURSE_UNITS_3 = :QDS-RCOURSE-UNITS-3,
              DSFILE.DS_RCOURSE_AMT_1 = :QDS-RCOURSE-AMT-1,
              DSFILE.DS_RCOURSE_AMT_2 = :QDS-RCOURSE-AMT-2,
              DSFILE.DS_RCOURSE_AMT_3 = :QDS-RCOURSE-AMT-3,
              DSFILE.DS_NPURUNIT_1 = :QDS-NPURUNIT-1,
              DSFILE.DS_NPURUNIT_2 = :QDS-NPURUNIT-2,
              DSFILE.DS_NPURUNIT_3 = :QDS-NPURUNIT-3,
              DSFILE.DS_NPURAMT_1 = :QDS-NPURAMT-1,
              DSFILE.DS_NPURAMT_2 = :QDS-NPURAMT-2,
              DSFILE.DS_NPURAMT_3 = :QDS-NPURAMT-3,
              DSFILE.DS_OPURUNIT_1 = :QDS-OPURUNIT-1,
              DSFILE.DS_OPURUNIT_2 = :QDS-OPURUNIT-2,
              DSFILE.DS_OPURUNIT_3 = :QDS-OPURUNIT-3,
              DSFILE.DS_OPURAMT_1 = :QDS-OPURAMT-1,
              DSFILE.DS_OPURAMT_2 = :QDS-OPURAMT-2,
              DSFILE.DS_OPURAMT_3 = :QDS-OPURAMT-3,
              DSFILE.DS_RNEWUNIT_1 = :QDS-RNEWUNIT-1,
              DSFILE.DS_RNEWUNIT_2 = :QDS-RNEWUNIT-2,
              DSFILE.DS_RNEWUNIT_3 = :QDS-RNEWUNIT-3,
              DSFILE.DS_RNEWAMT_1 = :QDS-RNEWAMT-1,
              DSFILE.DS_RNEWAMT_2 = :QDS-RNEWAMT-2,
              DSFILE.DS_RNEWAMT_3 = :QDS-RNEWAMT-3,
              DSFILE.DS_PAIDUNIT_1 = :QDS-PAIDUNIT-1,
              DSFILE.DS_PAIDUNIT_2 = :QDS-PAIDUNIT-2,
              DSFILE.DS_PAIDUNIT_3 = :QDS-PAIDUNIT-3,
              DSFILE.DS_PAIDAMT_1 = :QDS-PAIDAMT-1,
              DSFILE.DS_PAIDAMT_2 = :QDS-PAIDAMT-2,
              DSFILE.DS_PAIDAMT_3 = :QDS-PAIDAMT-3,
              DSFILE.DS_TODLUNIT_1 = :QDS-TODLUNIT-1,
              DSFILE.DS_TODLUNIT_2 = :QDS-TODLUNIT-2,
              DSFILE.DS_TODLUNIT_3 = :QDS-TODLUNIT-3,
              DSFILE.DS_TODLAMT_1 = :QDS-TODLAMT-1,
              DSFILE.DS_TODLAMT_2 = :QDS-TODLAMT-2,
              DSFILE.DS_TODLAMT_3 = :QDS-TODLAMT-3,
              DSFILE.DS_OUTSUNIT_1 = :QDS-OUTSUNIT-1,
              DSFILE.DS_OUTSUNIT_2 = :QDS-OUTSUNIT-2,
              DSFILE.DS_OUTSUNIT_3 = :QDS-OUTSUNIT-3,
              DSFILE.DS_OUTSAMT_1 = :QDS-OUTSAMT-1,
              DSFILE.DS_OUTSAMT_2 = :QDS-OUTSAMT-2,
              DSFILE.DS_OUTSAMT_3 = :QDS-OUTSAMT-3,
              DSFILE.DS_PLOSUNIT_1 = :QDS-PLOSUNIT-1,
              DSFILE.DS_PLOSUNIT_2 = :QDS-PLOSUNIT-2,
              DSFILE.DS_PLOSUNIT_3 = :QDS-PLOSUNIT-3,
              DSFILE.DS_PLOSAMT_1 = :QDS-PLOSAMT-1,
              DSFILE.DS_PLOSAMT_2 = :QDS-PLOSAMT-2,
              DSFILE.DS_PLOSAMT_3 = :QDS-PLOSAMT-3,
              DSFILE.DS_RCPLUNIT_1 = :QDS-RCPLUNIT-1,
              DSFILE.DS_RCPLUNIT_2 = :QDS-RCPLUNIT-2,
              DSFILE.DS_RCPLUNIT_3 = :QDS-RCPLUNIT-3,
              DSFILE.DS_RCPLAMT_1 = :QDS-RCPLAMT-1,
              DSFILE.DS_RCPLAMT_2 = :QDS-RCPLAMT-2,
              DSFILE.DS_RCPLAMT_3 = :QDS-RCPLAMT-3,
              DSFILE.DS_JDOSUNIT_1 = :QDS-JDOSUNIT-1,
              DSFILE.DS_JDOSUNIT_2 = :QDS-JDOSUNIT-2,
              DSFILE.DS_JDOSUNIT_3 = :QDS-JDOSUNIT-3,
              DSFILE.DS_JDOSAMT_1 = :QDS-JDOSAMT-1,
              DSFILE.DS_JDOSAMT_2 = :QDS-JDOSAMT-2,
              DSFILE.DS_JDOSAMT_3 = :QDS-JDOSAMT-3,
              DSFILE.DS_DELUNEARN_DATE =
                 :QDS-DELUNEARN-DATE,
              DSFILE.DS_PROCEEDS_1 = :QDS-PROCEEDS-1,
              DSFILE.DS_PROCEEDS_2 = :QDS-PROCEEDS-2,
              DSFILE.DS_NO_REPO_1_1_1 = :QDS-NO-REPO-1-1-1,
              DSFILE.DS_AMT_REPO_1_1_1 = :QDS-AMT-REPO-1-1-1,
              DSFILE.DS_NO_REPO_1_1_2 = :QDS-NO-REPO-1-1-2,
              DSFILE.DS_AMT_REPO_1_1_2 = :QDS-AMT-REPO-1-1-2,
              DSFILE.DS_NO_REPO_1_1_3 = :QDS-NO-REPO-1-1-3,
              DSFILE.DS_AMT_REPO_1_1_3 = :QDS-AMT-REPO-1-1-3,
              DSFILE.DS_NO_REPO_1_1_4 = :QDS-NO-REPO-1-1-4,
              DSFILE.DS_AMT_REPO_1_1_4 = :QDS-AMT-REPO-1-1-4,
              DSFILE.DS_NO_REPO_1_1_5 = :QDS-NO-REPO-1-1-5,
              DSFILE.DS_AMT_REPO_1_1_5 = :QDS-AMT-REPO-1-1-5,
              DSFILE.DS_NO_REPO_1_2_1 = :QDS-NO-REPO-1-2-1,
              DSFILE.DS_AMT_REPO_1_2_1 = :QDS-AMT-REPO-1-2-1,
              DSFILE.DS_NO_REPO_1_2_2 = :QDS-NO-REPO-1-2-2,
              DSFILE.DS_AMT_REPO_1_2_2 = :QDS-AMT-REPO-1-2-2,
              DSFILE.DS_NO_REPO_1_2_3 = :QDS-NO-REPO-1-2-3,
              DSFILE.DS_AMT_REPO_1_2_3 = :QDS-AMT-REPO-1-2-3,
              DSFILE.DS_NO_REPO_1_2_4 = :QDS-NO-REPO-1-2-4,
              DSFILE.DS_AMT_REPO_1_2_4 = :QDS-AMT-REPO-1-2-4,
              DSFILE.DS_NO_REPO_1_2_5 = :QDS-NO-REPO-1-2-5,
              DSFILE.DS_AMT_REPO_1_2_5 = :QDS-AMT-REPO-1-2-5,
              DSFILE.DS_NO_REPO_2_1_1 = :QDS-NO-REPO-2-1-1,
              DSFILE.DS_AMT_REPO_2_1_1 = :QDS-AMT-REPO-2-1-1,
              DSFILE.DS_NO_REPO_2_1_2 = :QDS-NO-REPO-2-1-2,
              DSFILE.DS_AMT_REPO_2_1_2 = :QDS-AMT-REPO-2-1-2,
              DSFILE.DS_NO_REPO_2_1_3 = :QDS-NO-REPO-2-1-3,
              DSFILE.DS_AMT_REPO_2_1_3 = :QDS-AMT-REPO-2-1-3,
              DSFILE.DS_NO_REPO_2_1_4 = :QDS-NO-REPO-2-1-4,
              DSFILE.DS_AMT_REPO_2_1_4 = :QDS-AMT-REPO-2-1-4,
              DSFILE.DS_NO_REPO_2_1_5 = :QDS-NO-REPO-2-1-5,
              DSFILE.DS_AMT_REPO_2_1_5 = :QDS-AMT-REPO-2-1-5,
              DSFILE.DS_NO_REPO_2_2_1 = :QDS-NO-REPO-2-2-1,
              DSFILE.DS_AMT_REPO_2_2_1 = :QDS-AMT-REPO-2-2-1,
              DSFILE.DS_NO_REPO_2_2_2 = :QDS-NO-REPO-2-2-2,
              DSFILE.DS_AMT_REPO_2_2_2 = :QDS-AMT-REPO-2-2-2,
              DSFILE.DS_NO_REPO_2_2_3 = :QDS-NO-REPO-2-2-3,
              DSFILE.DS_AMT_REPO_2_2_3 = :QDS-AMT-REPO-2-2-3,
              DSFILE.DS_NO_REPO_2_2_4 = :QDS-NO-REPO-2-2-4,
              DSFILE.DS_AMT_REPO_2_2_4 = :QDS-AMT-REPO-2-2-4,
              DSFILE.DS_NO_REPO_2_2_5 = :QDS-NO-REPO-2-2-5,
              DSFILE.DS_AMT_REPO_2_2_5 = :QDS-AMT-REPO-2-2-5,
              DSFILE.DS_NO_REPO_3_1_1 = :QDS-NO-REPO-3-1-1,
              DSFILE.DS_AMT_REPO_3_1_1 = :QDS-AMT-REPO-3-1-1,
              DSFILE.DS_NO_REPO_3_1_2 = :QDS-NO-REPO-3-1-2,
              DSFILE.DS_AMT_REPO_3_1_2 = :QDS-AMT-REPO-3-1-2,
              DSFILE.DS_NO_REPO_3_1_3 = :QDS-NO-REPO-3-1-3,
              DSFILE.DS_AMT_REPO_3_1_3 = :QDS-AMT-REPO-3-1-3,
              DSFILE.DS_NO_REPO_3_1_4 = :QDS-NO-REPO-3-1-4,
              DSFILE.DS_AMT_REPO_3_1_4 = :QDS-AMT-REPO-3-1-4,
              DSFILE.DS_NO_REPO_3_1_5 = :QDS-NO-REPO-3-1-5,
              DSFILE.DS_AMT_REPO_3_1_5 = :QDS-AMT-REPO-3-1-5,
              DSFILE.DS_NO_REPO_3_2_1 = :QDS-NO-REPO-3-2-1,
              DSFILE.DS_AMT_REPO_3_2_1 = :QDS-AMT-REPO-3-2-1,
              DSFILE.DS_NO_REPO_3_2_2 = :QDS-NO-REPO-3-2-2,
              DSFILE.DS_AMT_REPO_3_2_2 = :QDS-AMT-REPO-3-2-2,
              DSFILE.DS_NO_REPO_3_2_3 = :QDS-NO-REPO-3-2-3,
              DSFILE.DS_AMT_REPO_3_2_3 = :QDS-AMT-REPO-3-2-3,
              DSFILE.DS_NO_REPO_3_2_4 = :QDS-NO-REPO-3-2-4,
              DSFILE.DS_AMT_REPO_3_2_4 = :QDS-AMT-REPO-3-2-4,
              DSFILE.DS_NO_REPO_3_2_5 = :QDS-NO-REPO-3-2-5,
              DSFILE.DS_AMT_REPO_3_2_5 = :QDS-AMT-REPO-3-2-5,
              DSFILE.DS_UNITS_1_1_1 = :QDS-UNITS-1-1-1,
              DSFILE.DS_OUTBAL_1_1_1 = :QDS-OUTBAL-1-1-1,
              DSFILE.DS_UNITS_1_1_2 = :QDS-UNITS-1-1-2,
              DSFILE.DS_OUTBAL_1_1_2 = :QDS-OUTBAL-1-1-2,
              DSFILE.DS_UNITS_1_1_3 = :QDS-UNITS-1-1-3,
              DSFILE.DS_OUTBAL_1_1_3 = :QDS-OUTBAL-1-1-3,
              DSFILE.DS_UNITS_1_1_4 = :QDS-UNITS-1-1-4,
              DSFILE.DS_OUTBAL_1_1_4 = :QDS-OUTBAL-1-1-4,
              DSFILE.DS_UNITS_1_1_5 = :QDS-UNITS-1-1-5,
              DSFILE.DS_OUTBAL_1_1_5 = :QDS-OUTBAL-1-1-5,
              DSFILE.DS_UNITS_1_1_6 = :QDS-UNITS-1-1-6,
              DSFILE.DS_OUTBAL_1_1_6 = :QDS-OUTBAL-1-1-6,
              DSFILE.DS_UNITS_1_1_7 = :QDS-UNITS-1-1-7,
              DSFILE.DS_OUTBAL_1_1_7 = :QDS-OUTBAL-1-1-7,
              DSFILE.DS_UNITS_1_1_8 = :QDS-UNITS-1-1-8,
              DSFILE.DS_OUTBAL_1_1_8 = :QDS-OUTBAL-1-1-8,
              DSFILE.DS_UNITS_1_1_9 = :QDS-UNITS-1-1-9,
              DSFILE.DS_OUTBAL_1_1_9 = :QDS-OUTBAL-1-1-9,
              DSFILE.DS_UNITS_1_1_10 = :QDS-UNITS-1-1-10,
              DSFILE.DS_OUTBAL_1_1_10 = :QDS-OUTBAL-1-1-10,
              DSFILE.DS_UNITS_1_2_1 = :QDS-UNITS-1-2-1,
              DSFILE.DS_OUTBAL_1_2_1 = :QDS-OUTBAL-1-2-1,
              DSFILE.DS_UNITS_1_2_2 = :QDS-UNITS-1-2-2,
              DSFILE.DS_OUTBAL_1_2_2 = :QDS-OUTBAL-1-2-2,
              DSFILE.DS_UNITS_1_2_3 = :QDS-UNITS-1-2-3,
              DSFILE.DS_OUTBAL_1_2_3 = :QDS-OUTBAL-1-2-3,
              DSFILE.DS_UNITS_1_2_4 = :QDS-UNITS-1-2-4,
              DSFILE.DS_OUTBAL_1_2_4 = :QDS-OUTBAL-1-2-4,
              DSFILE.DS_UNITS_1_2_5 = :QDS-UNITS-1-2-5,
              DSFILE.DS_OUTBAL_1_2_5 = :QDS-OUTBAL-1-2-5,
              DSFILE.DS_UNITS_1_2_6 = :QDS-UNITS-1-2-6,
              DSFILE.DS_OUTBAL_1_2_6 = :QDS-OUTBAL-1-2-6,
              DSFILE.DS_UNITS_1_2_7 = :QDS-UNITS-1-2-7,
              DSFILE.DS_OUTBAL_1_2_7 = :QDS-OUTBAL-1-2-7,
              DSFILE.DS_UNITS_1_2_8 = :QDS-UNITS-1-2-8,
              DSFILE.DS_OUTBAL_1_2_8 = :QDS-OUTBAL-1-2-8,
              DSFILE.DS_UNITS_1_2_9 = :QDS-UNITS-1-2-9,
              DSFILE.DS_OUTBAL_1_2_9 = :QDS-OUTBAL-1-2-9,
              DSFILE.DS_UNITS_1_2_10 = :QDS-UNITS-1-2-10,
              DSFILE.DS_OUTBAL_1_2_10 = :QDS-OUTBAL-1-2-10,
              DSFILE.DS_UNITS_2_1_1 = :QDS-UNITS-2-1-1,
              DSFILE.DS_OUTBAL_2_1_1 = :QDS-OUTBAL-2-1-1,
              DSFILE.DS_UNITS_2_1_2 = :QDS-UNITS-2-1-2,
              DSFILE.DS_OUTBAL_2_1_2 = :QDS-OUTBAL-2-1-2,
              DSFILE.DS_UNITS_2_1_3 = :QDS-UNITS-2-1-3,
              DSFILE.DS_OUTBAL_2_1_3 = :QDS-OUTBAL-2-1-3,
              DSFILE.DS_UNITS_2_1_4 = :QDS-UNITS-2-1-4,
              DSFILE.DS_OUTBAL_2_1_4 = :QDS-OUTBAL-2-1-4,
              DSFILE.DS_UNITS_2_1_5 = :QDS-UNITS-2-1-5,
              DSFILE.DS_OUTBAL_2_1_5 = :QDS-OUTBAL-2-1-5,
              DSFILE.DS_UNITS_2_1_6 = :QDS-UNITS-2-1-6,
              DSFILE.DS_OUTBAL_2_1_6 = :QDS-OUTBAL-2-1-6,
              DSFILE.DS_UNITS_2_1_7 = :QDS-UNITS-2-1-7,
              DSFILE.DS_OUTBAL_2_1_7 = :QDS-OUTBAL-2-1-7,
              DSFILE.DS_UNITS_2_1_8 = :QDS-UNITS-2-1-8,
              DSFILE.DS_OUTBAL_2_1_8 = :QDS-OUTBAL-2-1-8,
              DSFILE.DS_UNITS_2_1_9 = :QDS-UNITS-2-1-9,
              DSFILE.DS_OUTBAL_2_1_9 = :QDS-OUTBAL-2-1-9,
              DSFILE.DS_UNITS_2_1_10 = :QDS-UNITS-2-1-10,
              DSFILE.DS_OUTBAL_2_1_10 = :QDS-OUTBAL-2-1-10,
              DSFILE.DS_UNITS_2_2_1 = :QDS-UNITS-2-2-1,
              DSFILE.DS_OUTBAL_2_2_1 = :QDS-OUTBAL-2-2-1,
              DSFILE.DS_UNITS_2_2_2 = :QDS-UNITS-2-2-2,
              DSFILE.DS_OUTBAL_2_2_2 = :QDS-OUTBAL-2-2-2,
              DSFILE.DS_UNITS_2_2_3 = :QDS-UNITS-2-2-3,
              DSFILE.DS_OUTBAL_2_2_3 = :QDS-OUTBAL-2-2-3,
              DSFILE.DS_UNITS_2_2_4 = :QDS-UNITS-2-2-4,
              DSFILE.DS_OUTBAL_2_2_4 = :QDS-OUTBAL-2-2-4,
              DSFILE.DS_UNITS_2_2_5 = :QDS-UNITS-2-2-5,
              DSFILE.DS_OUTBAL_2_2_5 = :QDS-OUTBAL-2-2-5,
              DSFILE.DS_UNITS_2_2_6 = :QDS-UNITS-2-2-6,
              DSFILE.DS_OUTBAL_2_2_6 = :QDS-OUTBAL-2-2-6,
              DSFILE.DS_UNITS_2_2_7 = :QDS-UNITS-2-2-7,
              DSFILE.DS_OUTBAL_2_2_7 = :QDS-OUTBAL-2-2-7,
              DSFILE.DS_UNITS_2_2_8 = :QDS-UNITS-2-2-8,
              DSFILE.DS_OUTBAL_2_2_8 = :QDS-OUTBAL-2-2-8,
              DSFILE.DS_UNITS_2_2_9 = :QDS-UNITS-2-2-9,
              DSFILE.DS_OUTBAL_2_2_9 = :QDS-OUTBAL-2-2-9,
              DSFILE.DS_UNITS_2_2_10 = :QDS-UNITS-2-2-10,
              DSFILE.DS_OUTBAL_2_2_10 = :QDS-OUTBAL-2-2-10,
              DSFILE.DS_UNITS_3_1_1 = :QDS-UNITS-3-1-1,
              DSFILE.DS_OUTBAL_3_1_1 = :QDS-OUTBAL-3-1-1,
              DSFILE.DS_UNITS_3_1_2 = :QDS-UNITS-3-1-2,
              DSFILE.DS_OUTBAL_3_1_2 = :QDS-OUTBAL-3-1-2,
              DSFILE.DS_UNITS_3_1_3 = :QDS-UNITS-3-1-3,
              DSFILE.DS_OUTBAL_3_1_3 = :QDS-OUTBAL-3-1-3,
              DSFILE.DS_UNITS_3_1_4 = :QDS-UNITS-3-1-4,
              DSFILE.DS_OUTBAL_3_1_4 = :QDS-OUTBAL-3-1-4,
              DSFILE.DS_UNITS_3_1_5 = :QDS-UNITS-3-1-5,
              DSFILE.DS_OUTBAL_3_1_5 = :QDS-OUTBAL-3-1-5,
              DSFILE.DS_UNITS_3_1_6 = :QDS-UNITS-3-1-6,
              DSFILE.DS_OUTBAL_3_1_6 = :QDS-OUTBAL-3-1-6,
              DSFILE.DS_UNITS_3_1_7 = :QDS-UNITS-3-1-7,
              DSFILE.DS_OUTBAL_3_1_7 = :QDS-OUTBAL-3-1-7,
              DSFILE.DS_UNITS_3_1_8 = :QDS-UNITS-3-1-8,
              DSFILE.DS_OUTBAL_3_1_8 = :QDS-OUTBAL-3-1-8,
              DSFILE.DS_UNITS_3_1_9 = :QDS-UNITS-3-1-9,
              DSFILE.DS_OUTBAL_3_1_9 = :QDS-OUTBAL-3-1-9,
              DSFILE.DS_UNITS_3_1_10 = :QDS-UNITS-3-1-10,
              DSFILE.DS_OUTBAL_3_1_10 = :QDS-OUTBAL-3-1-10,
              DSFILE.DS_UNITS_3_2_1 = :QDS-UNITS-3-2-1,
              DSFILE.DS_OUTBAL_3_2_1 = :QDS-OUTBAL-3-2-1,
              DSFILE.DS_UNITS_3_2_2 = :QDS-UNITS-3-2-2,
              DSFILE.DS_OUTBAL_3_2_2 = :QDS-OUTBAL-3-2-2,
              DSFILE.DS_UNITS_3_2_3 = :QDS-UNITS-3-2-3,
              DSFILE.DS_OUTBAL_3_2_3 = :QDS-OUTBAL-3-2-3,
              DSFILE.DS_UNITS_3_2_4 = :QDS-UNITS-3-2-4,
              DSFILE.DS_OUTBAL_3_2_4 = :QDS-OUTBAL-3-2-4,
              DSFILE.DS_UNITS_3_2_5 = :QDS-UNITS-3-2-5,
              DSFILE.DS_OUTBAL_3_2_5 = :QDS-OUTBAL-3-2-5,
              DSFILE.DS_UNITS_3_2_6 = :QDS-UNITS-3-2-6,
              DSFILE.DS_OUTBAL_3_2_6 = :QDS-OUTBAL-3-2-6,
              DSFILE.DS_UNITS_3_2_7 = :QDS-UNITS-3-2-7,
              DSFILE.DS_OUTBAL_3_2_7 = :QDS-OUTBAL-3-2-7,
              DSFILE.DS_UNITS_3_2_8 = :QDS-UNITS-3-2-8,
              DSFILE.DS_OUTBAL_3_2_8 = :QDS-OUTBAL-3-2-8,
              DSFILE.DS_UNITS_3_2_9 = :QDS-UNITS-3-2-9,
              DSFILE.DS_OUTBAL_3_2_9 = :QDS-OUTBAL-3-2-9,
              DSFILE.DS_UNITS_3_2_10 = :QDS-UNITS-3-2-10,
              DSFILE.DS_OUTBAL_3_2_10 = :QDS-OUTBAL-3-2-10,
              DSFILE.DS_UNITS_BNKRPT_1_1_1 =
                   :QDS-UNITS-BNKRPT-1-1-1,
              DSFILE.DS_OUTBAL_BNKRPT_1_1_1 =
                   :QDS-OUTBAL-BNKRPT-1-1-1,
              DSFILE.DS_UNITS_BNKRPT_1_1_2 =
                   :QDS-UNITS-BNKRPT-1-1-2,
              DSFILE.DS_OUTBAL_BNKRPT_1_1_2 =
                   :QDS-OUTBAL-BNKRPT-1-1-2,
              DSFILE.DS_UNITS_BNKRPT_1_1_3 =
                   :QDS-UNITS-BNKRPT-1-1-3,
              DSFILE.DS_OUTBAL_BNKRPT_1_1_3 =
                   :QDS-OUTBAL-BNKRPT-1-1-3,
              DSFILE.DS_UNITS_BNKRPT_1_1_4 =
                   :QDS-UNITS-BNKRPT-1-1-4,
              DSFILE.DS_OUTBAL_BNKRPT_1_1_4 =
                   :QDS-OUTBAL-BNKRPT-1-1-4,
              DSFILE.DS_UNITS_BNKRPT_1_1_5 =
                   :QDS-UNITS-BNKRPT-1-1-5,
              DSFILE.DS_OUTBAL_BNKRPT_1_1_5 =
                   :QDS-OUTBAL-BNKRPT-1-1-5,
              DSFILE.DS_UNITS_BNKRPT_1_1_6 =
                   :QDS-UNITS-BNKRPT-1-1-6,
              DSFILE.DS_OUTBAL_BNKRPT_1_1_6 =
                   :QDS-OUTBAL-BNKRPT-1-1-6,
              DSFILE.DS_UNITS_BNKRPT_1_1_7 =
                   :QDS-UNITS-BNKRPT-1-1-7,
              DSFILE.DS_OUTBAL_BNKRPT_1_1_7 =
                   :QDS-OUTBAL-BNKRPT-1-1-7,
              DSFILE.DS_UNITS_BNKRPT_1_1_8 =
                   :QDS-UNITS-BNKRPT-1-1-8,
              DSFILE.DS_OUTBAL_BNKRPT_1_1_8 =
                   :QDS-OUTBAL-BNKRPT-1-1-8,
              DSFILE.DS_UNITS_BNKRPT_1_1_9 =
                   :QDS-UNITS-BNKRPT-1-1-9,
              DSFILE.DS_OUTBAL_BNKRPT_1_1_9 =
                   :QDS-OUTBAL-BNKRPT-1-1-9,
              DSFILE.DS_UNITS_BNKRPT_1_1_10 =
                   :QDS-UNITS-BNKRPT-1-1-10,
              DSFILE.DS_OUTBAL_BNKRPT_1_1_10 =
                   :QDS-OUTBAL-BNKRPT-1-1-10,
              DSFILE.DS_UNITS_BNKRPT_1_2_1 =
                   :QDS-UNITS-BNKRPT-1-2-1,
              DSFILE.DS_OUTBAL_BNKRPT_1_2_1 =
                   :QDS-OUTBAL-BNKRPT-1-2-1,
              DSFILE.DS_UNITS_BNKRPT_1_2_2 =
                   :QDS-UNITS-BNKRPT-1-2-2,
              DSFILE.DS_OUTBAL_BNKRPT_1_2_2 =
                   :QDS-OUTBAL-BNKRPT-1-2-2,
              DSFILE.DS_UNITS_BNKRPT_1_2_3 =
                   :QDS-UNITS-BNKRPT-1-2-3,
              DSFILE.DS_OUTBAL_BNKRPT_1_2_3 =
                   :QDS-OUTBAL-BNKRPT-1-2-3,
              DSFILE.DS_UNITS_BNKRPT_1_2_4 =
                   :QDS-UNITS-BNKRPT-1-2-4,
              DSFILE.DS_OUTBAL_BNKRPT_1_2_4 =
                   :QDS-OUTBAL-BNKRPT-1-2-4,
              DSFILE.DS_UNITS_BNKRPT_1_2_5 =
                   :QDS-UNITS-BNKRPT-1-2-5,
              DSFILE.DS_OUTBAL_BNKRPT_1_2_5 =
                   :QDS-OUTBAL-BNKRPT-1-2-5,
              DSFILE.DS_UNITS_BNKRPT_1_2_6 =
                   :QDS-UNITS-BNKRPT-1-2-6,
              DSFILE.DS_OUTBAL_BNKRPT_1_2_6 =
                   :QDS-OUTBAL-BNKRPT-1-2-6,
              DSFILE.DS_UNITS_BNKRPT_1_2_7 =
                   :QDS-UNITS-BNKRPT-1-2-7,
              DSFILE.DS_OUTBAL_BNKRPT_1_2_7 =
                   :QDS-OUTBAL-BNKRPT-1-2-7,
              DSFILE.DS_UNITS_BNKRPT_1_2_8 =
                   :QDS-UNITS-BNKRPT-1-2-8,
              DSFILE.DS_OUTBAL_BNKRPT_1_2_8 =
                   :QDS-OUTBAL-BNKRPT-1-2-8,
              DSFILE.DS_UNITS_BNKRPT_1_2_9 =
                   :QDS-UNITS-BNKRPT-1-2-9,
              DSFILE.DS_OUTBAL_BNKRPT_1_2_9 =
                   :QDS-OUTBAL-BNKRPT-1-2-9,
              DSFILE.DS_UNITS_BNKRPT_1_2_10 =
                   :QDS-UNITS-BNKRPT-1-2-10,
              DSFILE.DS_OUTBAL_BNKRPT_1_2_10 =
                   :QDS-OUTBAL-BNKRPT-1-2-10,
              DSFILE.DS_UNITS_BNKRPT_2_1_1 =
                   :QDS-UNITS-BNKRPT-2-1-1,
              DSFILE.DS_OUTBAL_BNKRPT_2_1_1 =
                   :QDS-OUTBAL-BNKRPT-2-1-1,
              DSFILE.DS_UNITS_BNKRPT_2_1_2 =
                   :QDS-UNITS-BNKRPT-2-1-2,
              DSFILE.DS_OUTBAL_BNKRPT_2_1_2 =
                   :QDS-OUTBAL-BNKRPT-2-1-2,
              DSFILE.DS_UNITS_BNKRPT_2_1_3 =
                   :QDS-UNITS-BNKRPT-2-1-3,
              DSFILE.DS_OUTBAL_BNKRPT_2_1_3 =
                   :QDS-OUTBAL-BNKRPT-2-1-3,
              DSFILE.DS_UNITS_BNKRPT_2_1_4 =
                   :QDS-UNITS-BNKRPT-2-1-4,
              DSFILE.DS_OUTBAL_BNKRPT_2_1_4 =
                   :QDS-OUTBAL-BNKRPT-2-1-4,
              DSFILE.DS_UNITS_BNKRPT_2_1_5 =
                   :QDS-UNITS-BNKRPT-2-1-5,
              DSFILE.DS_OUTBAL_BNKRPT_2_1_5 =
                   :QDS-OUTBAL-BNKRPT-2-1-5,
              DSFILE.DS_UNITS_BNKRPT_2_1_6 =
                   :QDS-UNITS-BNKRPT-2-1-6,
              DSFILE.DS_OUTBAL_BNKRPT_2_1_6 =
                   :QDS-OUTBAL-BNKRPT-2-1-6,
              DSFILE.DS_UNITS_BNKRPT_2_1_7 =
                   :QDS-UNITS-BNKRPT-2-1-7,
              DSFILE.DS_OUTBAL_BNKRPT_2_1_7 =
                   :QDS-OUTBAL-BNKRPT-2-1-7,
              DSFILE.DS_UNITS_BNKRPT_2_1_8 =
                   :QDS-UNITS-BNKRPT-2-1-8,
              DSFILE.DS_OUTBAL_BNKRPT_2_1_8 =
                   :QDS-OUTBAL-BNKRPT-2-1-8,
              DSFILE.DS_UNITS_BNKRPT_2_1_9 =
                   :QDS-UNITS-BNKRPT-2-1-9,
              DSFILE.DS_OUTBAL_BNKRPT_2_1_9 =
                   :QDS-OUTBAL-BNKRPT-2-1-9,
              DSFILE.DS_UNITS_BNKRPT_2_1_10 =
                   :QDS-UNITS-BNKRPT-2-1-10,
              DSFILE.DS_OUTBAL_BNKRPT_2_1_10 =
                   :QDS-OUTBAL-BNKRPT-2-1-10,
              DSFILE.DS_UNITS_BNKRPT_2_2_1 =
                   :QDS-UNITS-BNKRPT-2-2-1,
              DSFILE.DS_OUTBAL_BNKRPT_2_2_1 =
                   :QDS-OUTBAL-BNKRPT-2-2-1,
              DSFILE.DS_UNITS_BNKRPT_2_2_2 =
                   :QDS-UNITS-BNKRPT-2-2-2,
              DSFILE.DS_OUTBAL_BNKRPT_2_2_2 =
                   :QDS-OUTBAL-BNKRPT-2-2-2,
              DSFILE.DS_UNITS_BNKRPT_2_2_3 =
                   :QDS-UNITS-BNKRPT-2-2-3,
              DSFILE.DS_OUTBAL_BNKRPT_2_2_3 =
                   :QDS-OUTBAL-BNKRPT-2-2-3,
              DSFILE.DS_UNITS_BNKRPT_2_2_4 =
                   :QDS-UNITS-BNKRPT-2-2-4,
              DSFILE.DS_OUTBAL_BNKRPT_2_2_4 =
                   :QDS-OUTBAL-BNKRPT-2-2-4,
              DSFILE.DS_UNITS_BNKRPT_2_2_5 =
                   :QDS-UNITS-BNKRPT-2-2-5,
              DSFILE.DS_OUTBAL_BNKRPT_2_2_5 =
                   :QDS-OUTBAL-BNKRPT-2-2-5,
              DSFILE.DS_UNITS_BNKRPT_2_2_6 =
                   :QDS-UNITS-BNKRPT-2-2-6,
              DSFILE.DS_OUTBAL_BNKRPT_2_2_6 =
                   :QDS-OUTBAL-BNKRPT-2-2-6,
              DSFILE.DS_UNITS_BNKRPT_2_2_7 =
                   :QDS-UNITS-BNKRPT-2-2-7,
              DSFILE.DS_OUTBAL_BNKRPT_2_2_7 =
                   :QDS-OUTBAL-BNKRPT-2-2-7,
              DSFILE.DS_UNITS_BNKRPT_2_2_8 =
                   :QDS-UNITS-BNKRPT-2-2-8,
              DSFILE.DS_OUTBAL_BNKRPT_2_2_8 =
                   :QDS-OUTBAL-BNKRPT-2-2-8,
              DSFILE.DS_UNITS_BNKRPT_2_2_9 =
                   :QDS-UNITS-BNKRPT-2-2-9,
              DSFILE.DS_OUTBAL_BNKRPT_2_2_9 =
                   :QDS-OUTBAL-BNKRPT-2-2-9,
              DSFILE.DS_UNITS_BNKRPT_2_2_10 =
                   :QDS-UNITS-BNKRPT-2-2-10,
              DSFILE.DS_OUTBAL_BNKRPT_2_2_10 =
                   :QDS-OUTBAL-BNKRPT-2-2-10,
              DSFILE.DS_UNITS_BNKRPT_3_1_1 =
                   :QDS-UNITS-BNKRPT-3-1-1,
              DSFILE.DS_OUTBAL_BNKRPT_3_1_1 =
                   :QDS-OUTBAL-BNKRPT-3-1-1,
              DSFILE.DS_UNITS_BNKRPT_3_1_2 =
                   :QDS-UNITS-BNKRPT-3-1-2,
              DSFILE.DS_OUTBAL_BNKRPT_3_1_2 =
                   :QDS-OUTBAL-BNKRPT-3-1-2,
              DSFILE.DS_UNITS_BNKRPT_3_1_3 =
                   :QDS-UNITS-BNKRPT-3-1-3,
              DSFILE.DS_OUTBAL_BNKRPT_3_1_3 =
                   :QDS-OUTBAL-BNKRPT-3-1-3,
              DSFILE.DS_UNITS_BNKRPT_3_1_4 =
                   :QDS-UNITS-BNKRPT-3-1-4,
              DSFILE.DS_OUTBAL_BNKRPT_3_1_4 =
                   :QDS-OUTBAL-BNKRPT-3-1-4,
              DSFILE.DS_UNITS_BNKRPT_3_1_5 =
                   :QDS-UNITS-BNKRPT-3-1-5,
              DSFILE.DS_OUTBAL_BNKRPT_3_1_5 =
                   :QDS-OUTBAL-BNKRPT-3-1-5,
              DSFILE.DS_UNITS_BNKRPT_3_1_6 =
                   :QDS-UNITS-BNKRPT-3-1-6,
              DSFILE.DS_OUTBAL_BNKRPT_3_1_6 =
                   :QDS-OUTBAL-BNKRPT-3-1-6,
              DSFILE.DS_UNITS_BNKRPT_3_1_7 =
                   :QDS-UNITS-BNKRPT-3-1-7,
              DSFILE.DS_OUTBAL_BNKRPT_3_1_7 =
                   :QDS-OUTBAL-BNKRPT-3-1-7,
              DSFILE.DS_UNITS_BNKRPT_3_1_8 =
                   :QDS-UNITS-BNKRPT-3-1-8,
              DSFILE.DS_OUTBAL_BNKRPT_3_1_8 =
                   :QDS-OUTBAL-BNKRPT-3-1-8,
              DSFILE.DS_UNITS_BNKRPT_3_1_9 =
                   :QDS-UNITS-BNKRPT-3-1-9,
              DSFILE.DS_OUTBAL_BNKRPT_3_1_9 =
                   :QDS-OUTBAL-BNKRPT-3-1-9,
              DSFILE.DS_UNITS_BNKRPT_3_1_10 =
                   :QDS-UNITS-BNKRPT-3-1-10,
              DSFILE.DS_OUTBAL_BNKRPT_3_1_10 =
                   :QDS-OUTBAL-BNKRPT-3-1-10,
              DSFILE.DS_UNITS_BNKRPT_3_2_1 =
                   :QDS-UNITS-BNKRPT-3-2-1,
              DSFILE.DS_OUTBAL_BNKRPT_3_2_1 =
                   :QDS-OUTBAL-BNKRPT-3-2-1,
              DSFILE.DS_UNITS_BNKRPT_3_2_2 =
                   :QDS-UNITS-BNKRPT-3-2-2,
              DSFILE.DS_OUTBAL_BNKRPT_3_2_2 =
                   :QDS-OUTBAL-BNKRPT-3-2-2,
              DSFILE.DS_UNITS_BNKRPT_3_2_3 =
                   :QDS-UNITS-BNKRPT-3-2-3,
              DSFILE.DS_OUTBAL_BNKRPT_3_2_3 =
                   :QDS-OUTBAL-BNKRPT-3-2-3,
              DSFILE.DS_UNITS_BNKRPT_3_2_4 =
                   :QDS-UNITS-BNKRPT-3-2-4,
              DSFILE.DS_OUTBAL_BNKRPT_3_2_4 =
                   :QDS-OUTBAL-BNKRPT-3-2-4,
              DSFILE.DS_UNITS_BNKRPT_3_2_5 =
                   :QDS-UNITS-BNKRPT-3-2-5,
              DSFILE.DS_OUTBAL_BNKRPT_3_2_5 =
                   :QDS-OUTBAL-BNKRPT-3-2-5,
              DSFILE.DS_UNITS_BNKRPT_3_2_6 =
                   :QDS-UNITS-BNKRPT-3-2-6,
              DSFILE.DS_OUTBAL_BNKRPT_3_2_6 =
                   :QDS-OUTBAL-BNKRPT-3-2-6,
              DSFILE.DS_UNITS_BNKRPT_3_2_7 =
                   :QDS-UNITS-BNKRPT-3-2-7,
              DSFILE.DS_OUTBAL_BNKRPT_3_2_7 =
                   :QDS-OUTBAL-BNKRPT-3-2-7,
              DSFILE.DS_UNITS_BNKRPT_3_2_8 =
                   :QDS-UNITS-BNKRPT-3-2-8,
              DSFILE.DS_OUTBAL_BNKRPT_3_2_8 =
                   :QDS-OUTBAL-BNKRPT-3-2-8,
              DSFILE.DS_UNITS_BNKRPT_3_2_9 =
                   :QDS-UNITS-BNKRPT-3-2-9,
              DSFILE.DS_OUTBAL_BNKRPT_3_2_9 =
                   :QDS-OUTBAL-BNKRPT-3-2-9,
              DSFILE.DS_UNITS_BNKRPT_3_2_10 =
                   :QDS-UNITS-BNKRPT-3-2-10,
              DSFILE.DS_OUTBAL_BNKRPT_3_2_10 =
                   :QDS-OUTBAL-BNKRPT-3-2-10,
              DSFILE.DS_UNEARNED_AMT_1_1_1 =
                   :QDS-UNEARNED-AMT-1-1-1,
              DSFILE.DS_UNEARNED_AMT_1_1_2 =
                   :QDS-UNEARNED-AMT-1-1-2,
              DSFILE.DS_UNEARNED_AMT_1_1_3 =
                   :QDS-UNEARNED-AMT-1-1-3,
              DSFILE.DS_UNEARNED_AMT_1_1_4 =
                   :QDS-UNEARNED-AMT-1-1-4,
              DSFILE.DS_UNEARNED_AMT_1_1_5 =
                   :QDS-UNEARNED-AMT-1-1-5,
              DSFILE.DS_UNEARNED_AMT_1_1_6 =
                   :QDS-UNEARNED-AMT-1-1-6,
              DSFILE.DS_UNEARNED_AMT_1_1_7 =
                   :QDS-UNEARNED-AMT-1-1-7,
              DSFILE.DS_UNEARNED_AMT_1_1_8 =
                   :QDS-UNEARNED-AMT-1-1-8,
              DSFILE.DS_UNEARNED_AMT_1_1_9 =
                   :QDS-UNEARNED-AMT-1-1-9,
              DSFILE.DS_UNEARNED_AMT_1_1_10 =
                   :QDS-UNEARNED-AMT-1-1-10,
              DSFILE.DS_UNEARNED_AMT_1_2_1 =
                   :QDS-UNEARNED-AMT-1-2-1,
              DSFILE.DS_UNEARNED_AMT_1_2_2 =
                   :QDS-UNEARNED-AMT-1-2-2,
              DSFILE.DS_UNEARNED_AMT_1_2_3 =
                   :QDS-UNEARNED-AMT-1-2-3,
              DSFILE.DS_UNEARNED_AMT_1_2_4 =
                   :QDS-UNEARNED-AMT-1-2-4,
              DSFILE.DS_UNEARNED_AMT_1_2_5 =
                   :QDS-UNEARNED-AMT-1-2-5,
              DSFILE.DS_UNEARNED_AMT_1_2_6 =
                   :QDS-UNEARNED-AMT-1-2-6,
              DSFILE.DS_UNEARNED_AMT_1_2_7 =
                   :QDS-UNEARNED-AMT-1-2-7,
              DSFILE.DS_UNEARNED_AMT_1_2_8 =
                   :QDS-UNEARNED-AMT-1-2-8,
              DSFILE.DS_UNEARNED_AMT_1_2_9 =
                   :QDS-UNEARNED-AMT-1-2-9,
              DSFILE.DS_UNEARNED_AMT_1_2_10 =
                   :QDS-UNEARNED-AMT-1-2-10,
              DSFILE.DS_UNEARNED_AMT_2_1_1 =
                   :QDS-UNEARNED-AMT-2-1-1,
              DSFILE.DS_UNEARNED_AMT_2_1_2 =
                   :QDS-UNEARNED-AMT-2-1-2,
              DSFILE.DS_UNEARNED_AMT_2_1_3 =
                   :QDS-UNEARNED-AMT-2-1-3,
              DSFILE.DS_UNEARNED_AMT_2_1_4 =
                   :QDS-UNEARNED-AMT-2-1-4,
              DSFILE.DS_UNEARNED_AMT_2_1_5 =
                   :QDS-UNEARNED-AMT-2-1-5,
              DSFILE.DS_UNEARNED_AMT_2_1_6 =
                   :QDS-UNEARNED-AMT-2-1-6,
              DSFILE.DS_UNEARNED_AMT_2_1_7 =
                   :QDS-UNEARNED-AMT-2-1-7,
              DSFILE.DS_UNEARNED_AMT_2_1_8 =
                   :QDS-UNEARNED-AMT-2-1-8,
              DSFILE.DS_UNEARNED_AMT_2_1_9 =
                   :QDS-UNEARNED-AMT-2-1-9,
              DSFILE.DS_UNEARNED_AMT_2_1_10 =
                   :QDS-UNEARNED-AMT-2-1-10,
              DSFILE.DS_UNEARNED_AMT_2_2_1 =
                   :QDS-UNEARNED-AMT-2-2-1,
              DSFILE.DS_UNEARNED_AMT_2_2_2 =
                   :QDS-UNEARNED-AMT-2-2-2,
              DSFILE.DS_UNEARNED_AMT_2_2_3 =
                   :QDS-UNEARNED-AMT-2-2-3,
              DSFILE.DS_UNEARNED_AMT_2_2_4 =
                   :QDS-UNEARNED-AMT-2-2-4,
              DSFILE.DS_UNEARNED_AMT_2_2_5 =
                   :QDS-UNEARNED-AMT-2-2-5,
              DSFILE.DS_UNEARNED_AMT_2_2_6 =
                   :QDS-UNEARNED-AMT-2-2-6,
              DSFILE.DS_UNEARNED_AMT_2_2_7 =
                   :QDS-UNEARNED-AMT-2-2-7,
              DSFILE.DS_UNEARNED_AMT_2_2_8 =
                   :QDS-UNEARNED-AMT-2-2-8,
              DSFILE.DS_UNEARNED_AMT_2_2_9 =
                   :QDS-UNEARNED-AMT-2-2-9,
              DSFILE.DS_UNEARNED_AMT_2_2_10 =
                   :QDS-UNEARNED-AMT-2-2-10,
              DSFILE.DS_UNEARNED_AMT_3_1_1 =
                   :QDS-UNEARNED-AMT-3-1-1,
              DSFILE.DS_UNEARNED_AMT_3_1_2 =
                   :QDS-UNEARNED-AMT-3-1-2,
              DSFILE.DS_UNEARNED_AMT_3_1_3 =
                   :QDS-UNEARNED-AMT-3-1-3,
              DSFILE.DS_UNEARNED_AMT_3_1_4 =
                   :QDS-UNEARNED-AMT-3-1-4,
              DSFILE.DS_UNEARNED_AMT_3_1_5 =
                   :QDS-UNEARNED-AMT-3-1-5,
              DSFILE.DS_UNEARNED_AMT_3_1_6 =
                   :QDS-UNEARNED-AMT-3-1-6,
              DSFILE.DS_UNEARNED_AMT_3_1_7 =
                   :QDS-UNEARNED-AMT-3-1-7,
              DSFILE.DS_UNEARNED_AMT_3_1_8 =
                   :QDS-UNEARNED-AMT-3-1-8,
              DSFILE.DS_UNEARNED_AMT_3_1_9 =
                   :QDS-UNEARNED-AMT-3-1-9,
              DSFILE.DS_UNEARNED_AMT_3_1_10 =
                   :QDS-UNEARNED-AMT-3-1-10,
              DSFILE.DS_UNEARNED_AMT_3_2_1 =
                   :QDS-UNEARNED-AMT-3-2-1,
              DSFILE.DS_UNEARNED_AMT_3_2_2 =
                   :QDS-UNEARNED-AMT-3-2-2,
              DSFILE.DS_UNEARNED_AMT_3_2_3 =
                   :QDS-UNEARNED-AMT-3-2-3,
              DSFILE.DS_UNEARNED_AMT_3_2_4 =
                   :QDS-UNEARNED-AMT-3-2-4,
              DSFILE.DS_UNEARNED_AMT_3_2_5 =
                   :QDS-UNEARNED-AMT-3-2-5,
              DSFILE.DS_UNEARNED_AMT_3_2_6 =
                   :QDS-UNEARNED-AMT-3-2-6,
              DSFILE.DS_UNEARNED_AMT_3_2_7 =
                   :QDS-UNEARNED-AMT-3-2-7,
              DSFILE.DS_UNEARNED_AMT_3_2_8 =
                   :QDS-UNEARNED-AMT-3-2-8,
              DSFILE.DS_UNEARNED_AMT_3_2_9 =
                   :QDS-UNEARNED-AMT-3-2-9,
              DSFILE.DS_UNEARNED_AMT_3_2_10 =
                   :QDS-UNEARNED-AMT-3-2-10,
              DSFILE.DS_DLDISC_VOL_1 = :QDS-DLDISC-VOL-1,
              DSFILE.DS_DLDISC_VOL_2 = :QDS-DLDISC-VOL-2,
              DSFILE.DS_DLDISC_VOL_3 = :QDS-DLDISC-VOL-3,
              DSFILE.DS_AFPDUEDL_MEMO = :QDS-AFPDUEDL-MEMO,
              DSFILE.DS_UNPAID_MANUAL = :QDS-UNPAID-MANUAL,
              DSFILE.DS_LTOUCH_DATE = :QDS-LTOUCH-DATE
             WHERE DSFILE.DS_DATE_CCYYMM = :QDS-DATE-CCYYMM
               AND DSFILE.DS_BRANCH = :QDS-BRANCH
               AND DSFILE.DS_NUM = :QDS-NUM
               AND DSFILE.DS_CLASS = :QDS-CLASS
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-DS1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

           MOVE  '1' TO SQL-UPDATE-CONN-USE-SW. *> USE C1 CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

      *-----------------------------------------------------------------
       CLOSE-DS1-FILE.
           PERFORM CLOSE-IT.

      *-----------------------------------------------------------------
       REWRITE-DS1-FILE-CLEAR-BYCON.

           MOVE DS-PATH-OWNBR  TO DS-BRANCH.
           ACCEPT DS-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE DS-PATH-SQL    TO E-FILE.
           MOVE DS1-KEY        TO E-KEYX.
           PERFORM SET-DS-FIELDS.

           MOVE '2' TO SQL-UPDATE-CONN-USE-SW. *> USE UPDATE_CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

           EXEC SQL
            UPDATE DBO.DSFILE
             SET
              DSFILE.DS_UNITS_1_2_1          = 0,
              DSFILE.DS_OUTBAL_1_2_1         = 0,
              DSFILE.DS_UNITS_1_2_2          = 0,
              DSFILE.DS_OUTBAL_1_2_2         = 0,
              DSFILE.DS_UNITS_1_2_3          = 0,
              DSFILE.DS_OUTBAL_1_2_3         = 0,
              DSFILE.DS_UNITS_1_2_4          = 0,
              DSFILE.DS_OUTBAL_1_2_4         = 0,
              DSFILE.DS_UNITS_1_2_5          = 0,
              DSFILE.DS_OUTBAL_1_2_5         = 0,
              DSFILE.DS_UNITS_1_2_6          = 0,
              DSFILE.DS_OUTBAL_1_2_6         = 0,
              DSFILE.DS_UNITS_1_2_7          = 0,
              DSFILE.DS_OUTBAL_1_2_7         = 0,
              DSFILE.DS_UNITS_1_2_8          = 0,
              DSFILE.DS_OUTBAL_1_2_8         = 0,
              DSFILE.DS_UNITS_1_2_9          = 0,
              DSFILE.DS_OUTBAL_1_2_9         = 0,
              DSFILE.DS_UNITS_1_2_10         = 0,
              DSFILE.DS_OUTBAL_1_2_10        = 0, 
              DSFILE.DS_UNITS_BNKRPT_1_2_1   = 0,
              DSFILE.DS_OUTBAL_BNKRPT_1_2_1  = 0,
              DSFILE.DS_UNITS_BNKRPT_1_2_2   = 0,
              DSFILE.DS_OUTBAL_BNKRPT_1_2_2  = 0,
              DSFILE.DS_UNITS_BNKRPT_1_2_3   = 0,
              DSFILE.DS_OUTBAL_BNKRPT_1_2_3  = 0,
              DSFILE.DS_UNITS_BNKRPT_1_2_4   = 0,
              DSFILE.DS_OUTBAL_BNKRPT_1_2_4  = 0,
              DSFILE.DS_UNITS_BNKRPT_1_2_5   = 0,
              DSFILE.DS_OUTBAL_BNKRPT_1_2_5  = 0,
              DSFILE.DS_UNITS_BNKRPT_1_2_6   = 0,
              DSFILE.DS_OUTBAL_BNKRPT_1_2_6  = 0,
              DSFILE.DS_UNITS_BNKRPT_1_2_7   = 0,
              DSFILE.DS_OUTBAL_BNKRPT_1_2_7  = 0,
              DSFILE.DS_UNITS_BNKRPT_1_2_8   = 0,
              DSFILE.DS_OUTBAL_BNKRPT_1_2_8  = 0,
              DSFILE.DS_UNITS_BNKRPT_1_2_9   = 0,
              DSFILE.DS_OUTBAL_BNKRPT_1_2_9  = 0,
              DSFILE.DS_UNITS_BNKRPT_1_2_10  = 0,
              DSFILE.DS_OUTBAL_BNKRPT_1_2_10 = 0,
              DSFILE.DS_LTOUCH_DATE          = :QDS-LTOUCH-DATE
             WHERE DSFILE.DS_DATE_CCYYMM     = :QDS-DATE-CCYYMM
               AND DSFILE.DS_BRANCH          = :QDS-BRANCH
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-DS1-FILE-CLEAR-BYCON.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

           MOVE  '1' TO SQL-UPDATE-CONN-USE-SW. *> USE C1 CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

      *-----------------------------------------------------------------
       REWRITE-DS1-FILE-CLEAR-BYREC.

           MOVE DS-PATH-OWNBR  TO DS-BRANCH.
           ACCEPT DS-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE DS-PATH-SQL    TO E-FILE.
           MOVE DS1-KEY        TO E-KEYX.
           PERFORM SET-DS-FIELDS.

           MOVE '2' TO SQL-UPDATE-CONN-USE-SW. *> USE UPDATE_CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

           EXEC SQL
            UPDATE DBO.DSFILE
             SET
              DSFILE.DS_UNITS_1_1_1          = 0,
              DSFILE.DS_OUTBAL_1_1_1         = 0,
              DSFILE.DS_UNITS_1_1_2          = 0,
              DSFILE.DS_OUTBAL_1_1_2         = 0,
              DSFILE.DS_UNITS_1_1_3          = 0,
              DSFILE.DS_OUTBAL_1_1_3         = 0,
              DSFILE.DS_UNITS_1_1_4          = 0,
              DSFILE.DS_OUTBAL_1_1_4         = 0,
              DSFILE.DS_UNITS_1_1_5          = 0,
              DSFILE.DS_OUTBAL_1_1_5         = 0,
              DSFILE.DS_UNITS_1_1_6          = 0,
              DSFILE.DS_OUTBAL_1_1_6         = 0,
              DSFILE.DS_UNITS_1_1_7          = 0,
              DSFILE.DS_OUTBAL_1_1_7         = 0,
              DSFILE.DS_UNITS_1_1_8          = 0,
              DSFILE.DS_OUTBAL_1_1_8         = 0,
              DSFILE.DS_UNITS_1_1_9          = 0,
              DSFILE.DS_OUTBAL_1_1_9         = 0,
              DSFILE.DS_UNITS_1_1_10         = 0,
              DSFILE.DS_OUTBAL_1_1_10        = 0,
              DSFILE.DS_UNITS_BNKRPT_1_1_1   = 0,
              DSFILE.DS_OUTBAL_BNKRPT_1_1_1  = 0,
              DSFILE.DS_UNITS_BNKRPT_1_1_2   = 0,
              DSFILE.DS_OUTBAL_BNKRPT_1_1_2  = 0,
              DSFILE.DS_UNITS_BNKRPT_1_1_3   = 0,
              DSFILE.DS_OUTBAL_BNKRPT_1_1_3  = 0,
              DSFILE.DS_UNITS_BNKRPT_1_1_4   = 0,
              DSFILE.DS_OUTBAL_BNKRPT_1_1_4  = 0,
              DSFILE.DS_UNITS_BNKRPT_1_1_5   = 0,
              DSFILE.DS_OUTBAL_BNKRPT_1_1_5  = 0,
              DSFILE.DS_UNITS_BNKRPT_1_1_6   = 0,
              DSFILE.DS_OUTBAL_BNKRPT_1_1_6  = 0,
              DSFILE.DS_UNITS_BNKRPT_1_1_7   = 0,
              DSFILE.DS_OUTBAL_BNKRPT_1_1_7  = 0,
              DSFILE.DS_UNITS_BNKRPT_1_1_8   = 0,
              DSFILE.DS_OUTBAL_BNKRPT_1_1_8  = 0,
              DSFILE.DS_UNITS_BNKRPT_1_1_9   = 0,
              DSFILE.DS_OUTBAL_BNKRPT_1_1_9  = 0,
              DSFILE.DS_UNITS_BNKRPT_1_1_10  = 0,
              DSFILE.DS_OUTBAL_BNKRPT_1_1_10 = 0,
              DSFILE.DS_LTOUCH_DATE          = :QDS-LTOUCH-DATE
             WHERE DSFILE.DS_DATE_CCYYMM     = :QDS-DATE-CCYYMM
               AND DSFILE.DS_BRANCH          = :QDS-BRANCH
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-DS1-FILE-CLEAR-BYREC.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

           MOVE  '1' TO SQL-UPDATE-CONN-USE-SW. *> USE C1 CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

      *-----------------------------------------------------------------


      * COPY "LIBLP/LPBY1IN.CPY".
      * COPYMEMBER: LIBLP/LPBY1IN
      ******************************************************************
      *
      * DESCRIPTION: BRANCH STATISTICS FILE - IO ROUTINES - A THRU B
      *
      * FILE KEY   : #1 = DATE-CCYYMM, BRNO, CLASS
      *
      * FILE PATH  : /USR/DATA/R#/LP/BYAFILE
      *              /USR/DATA/R#/LP/BYBFILE
      *
      * ATTENTION  : BY-PATH-OVERRIDE CONTROLS ALL FILES - A THRU B
      *              ^^^^^^^^^^^^^^^^
      *              THERE IS NO BYA-PATH-OVERRIDE OR BYB-PATH-OVERRIDE
      *
      *=================================================================
      * REV:
      * KEC 2019-0205 ***NEW*** TO A32; MODIFIED FROM A30
      * KEC 2021-0524 ADDED ACUSQL VERSION
      ******************************************************************
       SET-ALL-TO-BY1.
           MOVE BYA1-KEY   TO BY1-KEY.
           MOVE BYA-AREA-A TO BY-AREA-A.
           MOVE BYB-AREA-B TO BY-AREA-B.

      *-----------------------------------------------------------------
       SET-BY1-TO-ALL.
           MOVE BY1-KEY    TO BYA1-KEY BYB1-KEY.
           MOVE BY-AREA-A  TO BYA-AREA-A.
           MOVE BY-AREA-B  TO BYB-AREA-B.

      *-----------------------------------------------------------------
       LOAD-BY1-FILE.
           PERFORM LOAD-BYA1-FILE.
           PERFORM LOAD-BYB1-FILE.

      *-----------------------------------------------------------------
       OPEN-BY1-FILE.
           PERFORM OPEN-BYA1-FILE.
           IF ( IO-FG = 0 )
              PERFORM OPEN-BYB1-FILE
              IF ( IO-FG = 0 )
                 MOVE " " TO IO-MULTI-FG
              ELSE
                 MOVE "B" TO IO-MULTI-FG
           ELSE
              MOVE "A" TO IO-MULTI-FG.

      *-----------------------------------------------------------------
       START-BY1-FILE.
           PERFORM SET-BY1-TO-ALL.
           PERFORM START-BYA1-FILE.
           IF ( IO-FG NOT = 0 )
              MOVE "A" TO IO-MULTI-FG.
             
      *-----------------------------------------------------------------
       READ-BY1-FILE.
           PERFORM SET-BY1-TO-ALL.
           PERFORM READ-BYA1-FILE. *> EMBEDDED - BYAFILE
           IF ( IO-FG = 0 )
              PERFORM READ-BYB1-FILE
              IF ( IO-FG = 0 )
                 PERFORM SET-ALL-TO-BY1
               ELSE
                 MOVE "B" TO IO-MULTI-FG
           ELSE
              MOVE "A" TO IO-MULTI-FG.
             
      *-----------------------------------------------------------------
       READ-BY1-FILE-NEXT.
           PERFORM SET-BY1-TO-ALL.
           PERFORM READ-BYA1-FILE-NEXT.
           IF ( IO-FG = 0 )
              MOVE BYA1-KEY TO BYB1-KEY
              PERFORM READ-BYB1-FILE
              IF ( IO-FG = 0 )
                 PERFORM SET-ALL-TO-BY1
               ELSE
                 MOVE "B" TO IO-MULTI-FG
           ELSE
              MOVE "A" TO IO-MULTI-FG.
             
      *-----------------------------------------------------------------
       WRITE-BY1-FILE.
           PERFORM SET-BY1-TO-ALL.
           PERFORM WRITE-BYA1-FILE.
           IF ( IO-FG = 0 )
              PERFORM WRITE-BYB1-FILE
              IF ( IO-FG = 0 )
                 PERFORM SET-ALL-TO-BY1
               ELSE
                 MOVE "B" TO IO-MULTI-FG
           ELSE
              MOVE "A" TO IO-MULTI-FG.

      *-----------------------------------------------------------------
       REWRITE-BY1-FILE.
           PERFORM SET-BY1-TO-ALL.
           PERFORM REWRITE-BYA1-FILE.
           IF ( IO-FG = 0 )
              PERFORM REWRITE-BYB1-FILE
              IF ( IO-FG = 0 )
                 PERFORM SET-ALL-TO-BY1
               ELSE
                 MOVE "B" TO IO-MULTI-FG
           ELSE
              MOVE "A" TO IO-MULTI-FG.

      *-----------------------------------------------------------------
       DELETE-BY1-FILE.
           PERFORM SET-BY1-TO-ALL.
           PERFORM DELETE-BYA1-FILE.
           IF ( IO-FG = 0 )
              PERFORM DELETE-BYB1-FILE
              IF ( IO-FG = 0 )
                 PERFORM SET-ALL-TO-BY1
               ELSE
                 MOVE "B" TO IO-MULTI-FG
           ELSE
              MOVE "A" TO IO-MULTI-FG.

      *-----------------------------------------------------------------
       CLOSE-BY1-FILE.

           PERFORM CLOSE-IT.
           PERFORM CLOSE-BYA1-FILE.
           PERFORM CLOSE-BYB1-FILE.

      *=================================================================
       LOAD-BYA1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( BY-PATH-OVERRIDE = "Y" )
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( BY-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO BYA-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO BYA-ALL-MACHINE
              MOVE BYA-ALL-PATH            TO BYA-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( BY-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO BYA-ALL-BASE
              MOVE FILPATH-MACHINE         TO BYA-ALL-MACHINE
              MOVE BYA-ALL-PATH            TO BYA-PATH.

      *-----------------------------------------------------------------
       OPEN-BYA1-FILE.
           PERFORM LOAD-BYA1-FILE.
           PERFORM OPEN-IT.
           MOVE BYA-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-BYA1-FILE. *> EMBEDDED - BYAFILE
           PERFORM START-IT.
           MOVE BYA-PATH-SQL TO E-FILE.
           MOVE BYA1-KEY TO E-KEYX.

           IF ( BYA1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-BYA1-FILE.

           IF ( QBY1-WEND-DATE-CCYYMM = ZEROES ) OR
              ( QBY1-WEND-DATE-CCYYMM NOT NUMERIC )
              MOVE BY-DATE-CCYYMM       TO QBY1-WBEG-DATE-CCYYMM
              MOVE ALL "9"              TO QBY1-WEND-DATE-CCYYMM 
              MOVE BY-BRNO              TO QBY1-WBEG-BRNO
              MOVE ALL "9"              TO QBY1-WEND-BRNO
              MOVE BY-CLASS             TO QBY1-WBEG-CLASS
              MOVE ALL "9"              TO QBY1-WEND-CLASS.

           EXEC SQL
            DECLARE BYA1_CURSOR CURSOR FOR
            SELECT BYAFILE.BYA_DATE_CCYYMM,
                   BYAFILE.BYA_BRNO,
                   BYAFILE.BYA_CLASS,
                   CAST(BYAFILE.BYA_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   BYAFILE.BYA_NO_1_1_1,
                   BYAFILE.BYA_AMT_1_1_1,
                   BYAFILE.BYA_O2_1_1_1,
                   BYAFILE.BYA_INT_1_1_1,
                   BYAFILE.BYA_NO_1_1_2,
                   BYAFILE.BYA_AMT_1_1_2,
                   BYAFILE.BYA_O2_1_1_2,
                   BYAFILE.BYA_INT_1_1_2,
                   BYAFILE.BYA_NO_1_1_3,
                   BYAFILE.BYA_AMT_1_1_3,
                   BYAFILE.BYA_O2_1_1_3,
                   BYAFILE.BYA_INT_1_1_3,
                   BYAFILE.BYA_NO_1_1_4,
                   BYAFILE.BYA_AMT_1_1_4,
                   BYAFILE.BYA_O2_1_1_4,
                   BYAFILE.BYA_INT_1_1_4,
                   BYAFILE.BYA_NO_1_1_5,
                   BYAFILE.BYA_AMT_1_1_5,
                   BYAFILE.BYA_O2_1_1_5,
                   BYAFILE.BYA_INT_1_1_5,
                   BYAFILE.BYA_NO_1_1_6,
                   BYAFILE.BYA_AMT_1_1_6,
                   BYAFILE.BYA_O2_1_1_6,
                   BYAFILE.BYA_INT_1_1_6,
                   BYAFILE.BYA_NO_1_1_7,
                   BYAFILE.BYA_AMT_1_1_7,
                   BYAFILE.BYA_O2_1_1_7,
                   BYAFILE.BYA_INT_1_1_7,
                   BYAFILE.BYA_NO_1_1_8,
                   BYAFILE.BYA_AMT_1_1_8,
                   BYAFILE.BYA_O2_1_1_8,
                   BYAFILE.BYA_INT_1_1_8,
                   BYAFILE.BYA_NO_1_1_9,
                   BYAFILE.BYA_AMT_1_1_9,
                   BYAFILE.BYA_O2_1_1_9,
                   BYAFILE.BYA_INT_1_1_9,
                   BYAFILE.BYA_NO_1_1_10,
                   BYAFILE.BYA_AMT_1_1_10,
                   BYAFILE.BYA_O2_1_1_10,
                   BYAFILE.BYA_INT_1_1_10,
                   BYAFILE.BYA_NO_1_1_11,
                   BYAFILE.BYA_AMT_1_1_11,
                   BYAFILE.BYA_O2_1_1_11,
                   BYAFILE.BYA_INT_1_1_11,
                   BYAFILE.BYA_NO_1_2_1,
                   BYAFILE.BYA_AMT_1_2_1,
                   BYAFILE.BYA_O2_1_2_1,
                   BYAFILE.BYA_INT_1_2_1,
                   BYAFILE.BYA_NO_1_2_2,
                   BYAFILE.BYA_AMT_1_2_2,
                   BYAFILE.BYA_O2_1_2_2,
                   BYAFILE.BYA_INT_1_2_2,
                   BYAFILE.BYA_NO_1_2_3,
                   BYAFILE.BYA_AMT_1_2_3,
                   BYAFILE.BYA_O2_1_2_3,
                   BYAFILE.BYA_INT_1_2_3,
                   BYAFILE.BYA_NO_1_2_4,
                   BYAFILE.BYA_AMT_1_2_4,
                   BYAFILE.BYA_O2_1_2_4,
                   BYAFILE.BYA_INT_1_2_4,
                   BYAFILE.BYA_NO_1_2_5,
                   BYAFILE.BYA_AMT_1_2_5,
                   BYAFILE.BYA_O2_1_2_5,
                   BYAFILE.BYA_INT_1_2_5,
                   BYAFILE.BYA_NO_1_2_6,
                   BYAFILE.BYA_AMT_1_2_6,
                   BYAFILE.BYA_O2_1_2_6,
                   BYAFILE.BYA_INT_1_2_6,
                   BYAFILE.BYA_NO_1_2_7,
                   BYAFILE.BYA_AMT_1_2_7,
                   BYAFILE.BYA_O2_1_2_7,
                   BYAFILE.BYA_INT_1_2_7,
                   BYAFILE.BYA_NO_1_2_8,
                   BYAFILE.BYA_AMT_1_2_8,
                   BYAFILE.BYA_O2_1_2_8,
                   BYAFILE.BYA_INT_1_2_8,
                   BYAFILE.BYA_NO_1_2_9,
                   BYAFILE.BYA_AMT_1_2_9,
                   BYAFILE.BYA_O2_1_2_9,
                   BYAFILE.BYA_INT_1_2_9,
                   BYAFILE.BYA_NO_1_2_10,
                   BYAFILE.BYA_AMT_1_2_10,
                   BYAFILE.BYA_O2_1_2_10,
                   BYAFILE.BYA_INT_1_2_10,
                   BYAFILE.BYA_NO_1_2_11,
                   BYAFILE.BYA_AMT_1_2_11,
                   BYAFILE.BYA_O2_1_2_11,
                   BYAFILE.BYA_INT_1_2_11,
                   BYAFILE.BYA_NO_2_1_1,
                   BYAFILE.BYA_AMT_2_1_1,
                   BYAFILE.BYA_O2_2_1_1,
                   BYAFILE.BYA_INT_2_1_1,
                   BYAFILE.BYA_NO_2_1_2,
                   BYAFILE.BYA_AMT_2_1_2,
                   BYAFILE.BYA_O2_2_1_2,
                   BYAFILE.BYA_INT_2_1_2,
                   BYAFILE.BYA_NO_2_1_3,
                   BYAFILE.BYA_AMT_2_1_3,
                   BYAFILE.BYA_O2_2_1_3,
                   BYAFILE.BYA_INT_2_1_3,
                   BYAFILE.BYA_NO_2_1_4,
                   BYAFILE.BYA_AMT_2_1_4,
                   BYAFILE.BYA_O2_2_1_4,
                   BYAFILE.BYA_INT_2_1_4,
                   BYAFILE.BYA_NO_2_1_5,
                   BYAFILE.BYA_AMT_2_1_5,
                   BYAFILE.BYA_O2_2_1_5,
                   BYAFILE.BYA_INT_2_1_5,
                   BYAFILE.BYA_NO_2_1_6,
                   BYAFILE.BYA_AMT_2_1_6,
                   BYAFILE.BYA_O2_2_1_6,
                   BYAFILE.BYA_INT_2_1_6,
                   BYAFILE.BYA_NO_2_1_7,
                   BYAFILE.BYA_AMT_2_1_7,
                   BYAFILE.BYA_O2_2_1_7,
                   BYAFILE.BYA_INT_2_1_7,
                   BYAFILE.BYA_NO_2_1_8,
                   BYAFILE.BYA_AMT_2_1_8,
                   BYAFILE.BYA_O2_2_1_8,
                   BYAFILE.BYA_INT_2_1_8,
                   BYAFILE.BYA_NO_2_1_9,
                   BYAFILE.BYA_AMT_2_1_9,
                   BYAFILE.BYA_O2_2_1_9,
                   BYAFILE.BYA_INT_2_1_9,
                   BYAFILE.BYA_NO_2_1_10,
                   BYAFILE.BYA_AMT_2_1_10,
                   BYAFILE.BYA_O2_2_1_10,
                   BYAFILE.BYA_INT_2_1_10,
                   BYAFILE.BYA_NO_2_1_11,
                   BYAFILE.BYA_AMT_2_1_11,
                   BYAFILE.BYA_O2_2_1_11,
                   BYAFILE.BYA_INT_2_1_11,
                   BYAFILE.BYA_NO_2_2_1,
                   BYAFILE.BYA_AMT_2_2_1,
                   BYAFILE.BYA_O2_2_2_1,
                   BYAFILE.BYA_INT_2_2_1,
                   BYAFILE.BYA_NO_2_2_2,
                   BYAFILE.BYA_AMT_2_2_2,
                   BYAFILE.BYA_O2_2_2_2,
                   BYAFILE.BYA_INT_2_2_2,
                   BYAFILE.BYA_NO_2_2_3,
                   BYAFILE.BYA_AMT_2_2_3,
                   BYAFILE.BYA_O2_2_2_3,
                   BYAFILE.BYA_INT_2_2_3,
                   BYAFILE.BYA_NO_2_2_4,
                   BYAFILE.BYA_AMT_2_2_4,
                   BYAFILE.BYA_O2_2_2_4,
                   BYAFILE.BYA_INT_2_2_4,
                   BYAFILE.BYA_NO_2_2_5,
                   BYAFILE.BYA_AMT_2_2_5,
                   BYAFILE.BYA_O2_2_2_5,
                   BYAFILE.BYA_INT_2_2_5,
                   BYAFILE.BYA_NO_2_2_6,
                   BYAFILE.BYA_AMT_2_2_6,
                   BYAFILE.BYA_O2_2_2_6,
                   BYAFILE.BYA_INT_2_2_6,
                   BYAFILE.BYA_NO_2_2_7,
                   BYAFILE.BYA_AMT_2_2_7,
                   BYAFILE.BYA_O2_2_2_7,
                   BYAFILE.BYA_INT_2_2_7,
                   BYAFILE.BYA_NO_2_2_8,
                   BYAFILE.BYA_AMT_2_2_8,
                   BYAFILE.BYA_O2_2_2_8,
                   BYAFILE.BYA_INT_2_2_8,
                   BYAFILE.BYA_NO_2_2_9,
                   BYAFILE.BYA_AMT_2_2_9,
                   BYAFILE.BYA_O2_2_2_9,
                   BYAFILE.BYA_INT_2_2_9,
                   BYAFILE.BYA_NO_2_2_10,
                   BYAFILE.BYA_AMT_2_2_10,
                   BYAFILE.BYA_O2_2_2_10,
                   BYAFILE.BYA_INT_2_2_10,
                   BYAFILE.BYA_NO_2_2_11,
                   BYAFILE.BYA_AMT_2_2_11,
                   BYAFILE.BYA_O2_2_2_11,
                   BYAFILE.BYA_INT_2_2_11,
                   BYAFILE.BYA_NO_3_1_1,
                   BYAFILE.BYA_AMT_3_1_1,
                   BYAFILE.BYA_O2_3_1_1,
                   BYAFILE.BYA_INT_3_1_1,
                   BYAFILE.BYA_NO_3_1_2,
                   BYAFILE.BYA_AMT_3_1_2,
                   BYAFILE.BYA_O2_3_1_2,
                   BYAFILE.BYA_INT_3_1_2,
                   BYAFILE.BYA_NO_3_1_3,
                   BYAFILE.BYA_AMT_3_1_3,
                   BYAFILE.BYA_O2_3_1_3,
                   BYAFILE.BYA_INT_3_1_3,
                   BYAFILE.BYA_NO_3_1_4,
                   BYAFILE.BYA_AMT_3_1_4,
                   BYAFILE.BYA_O2_3_1_4,
                   BYAFILE.BYA_INT_3_1_4,
                   BYAFILE.BYA_NO_3_1_5,
                   BYAFILE.BYA_AMT_3_1_5,
                   BYAFILE.BYA_O2_3_1_5,
                   BYAFILE.BYA_INT_3_1_5,
                   BYAFILE.BYA_NO_3_1_6,
                   BYAFILE.BYA_AMT_3_1_6,
                   BYAFILE.BYA_O2_3_1_6,
                   BYAFILE.BYA_INT_3_1_6,
                   BYAFILE.BYA_NO_3_1_7,
                   BYAFILE.BYA_AMT_3_1_7,
                   BYAFILE.BYA_O2_3_1_7,
                   BYAFILE.BYA_INT_3_1_7,
                   BYAFILE.BYA_NO_3_1_8,
                   BYAFILE.BYA_AMT_3_1_8,
                   BYAFILE.BYA_O2_3_1_8,
                   BYAFILE.BYA_INT_3_1_8,
                   BYAFILE.BYA_NO_3_1_9,
                   BYAFILE.BYA_AMT_3_1_9,
                   BYAFILE.BYA_O2_3_1_9,
                   BYAFILE.BYA_INT_3_1_9,
                   BYAFILE.BYA_NO_3_1_10,
                   BYAFILE.BYA_AMT_3_1_10,
                   BYAFILE.BYA_O2_3_1_10,
                   BYAFILE.BYA_INT_3_1_10,
                   BYAFILE.BYA_NO_3_1_11,
                   BYAFILE.BYA_AMT_3_1_11,
                   BYAFILE.BYA_O2_3_1_11,
                   BYAFILE.BYA_INT_3_1_11,
                   BYAFILE.BYA_NO_3_2_1,
                   BYAFILE.BYA_AMT_3_2_1,
                   BYAFILE.BYA_O2_3_2_1,
                   BYAFILE.BYA_INT_3_2_1,
                   BYAFILE.BYA_NO_3_2_2,
                   BYAFILE.BYA_AMT_3_2_2,
                   BYAFILE.BYA_O2_3_2_2,
                   BYAFILE.BYA_INT_3_2_2,
                   BYAFILE.BYA_NO_3_2_3,
                   BYAFILE.BYA_AMT_3_2_3,
                   BYAFILE.BYA_O2_3_2_3,
                   BYAFILE.BYA_INT_3_2_3,
                   BYAFILE.BYA_NO_3_2_4,
                   BYAFILE.BYA_AMT_3_2_4,
                   BYAFILE.BYA_O2_3_2_4,
                   BYAFILE.BYA_INT_3_2_4,
                   BYAFILE.BYA_NO_3_2_5,
                   BYAFILE.BYA_AMT_3_2_5,
                   BYAFILE.BYA_O2_3_2_5,
                   BYAFILE.BYA_INT_3_2_5,
                   BYAFILE.BYA_NO_3_2_6,
                   BYAFILE.BYA_AMT_3_2_6,
                   BYAFILE.BYA_O2_3_2_6,
                   BYAFILE.BYA_INT_3_2_6,
                   BYAFILE.BYA_NO_3_2_7,
                   BYAFILE.BYA_AMT_3_2_7,
                   BYAFILE.BYA_O2_3_2_7,
                   BYAFILE.BYA_INT_3_2_7,
                   BYAFILE.BYA_NO_3_2_8,
                   BYAFILE.BYA_AMT_3_2_8,
                   BYAFILE.BYA_O2_3_2_8,
                   BYAFILE.BYA_INT_3_2_8,
                   BYAFILE.BYA_NO_3_2_9,
                   BYAFILE.BYA_AMT_3_2_9,
                   BYAFILE.BYA_O2_3_2_9,
                   BYAFILE.BYA_INT_3_2_9,
                   BYAFILE.BYA_NO_3_2_10,
                   BYAFILE.BYA_AMT_3_2_10,
                   BYAFILE.BYA_O2_3_2_10,
                   BYAFILE.BYA_INT_3_2_10,
                   BYAFILE.BYA_NO_3_2_11,
                   BYAFILE.BYA_AMT_3_2_11,
                   BYAFILE.BYA_O2_3_2_11,
                   BYAFILE.BYA_INT_3_2_11,
                   BYAFILE.BYA_NO_REPO_1_1_1,
                   BYAFILE.BYA_AMT_REPO_1_1_1,
                   BYAFILE.BYA_O2_REPO_1_1_1,
                   BYAFILE.BYA_INT_REPO_1_1_1,
                   BYAFILE.BYA_NO_REPO_1_1_2,
                   BYAFILE.BYA_AMT_REPO_1_1_2,
                   BYAFILE.BYA_O2_REPO_1_1_2,
                   BYAFILE.BYA_INT_REPO_1_1_2,
                   BYAFILE.BYA_NO_REPO_1_1_3,
                   BYAFILE.BYA_AMT_REPO_1_1_3,
                   BYAFILE.BYA_O2_REPO_1_1_3,
                   BYAFILE.BYA_INT_REPO_1_1_3,
                   BYAFILE.BYA_NO_REPO_1_1_4,
                   BYAFILE.BYA_AMT_REPO_1_1_4,
                   BYAFILE.BYA_O2_REPO_1_1_4,
                   BYAFILE.BYA_INT_REPO_1_1_4,
                   BYAFILE.BYA_NO_REPO_1_1_5,
                   BYAFILE.BYA_AMT_REPO_1_1_5,
                   BYAFILE.BYA_O2_REPO_1_1_5,
                   BYAFILE.BYA_INT_REPO_1_1_5,
                   BYAFILE.BYA_NO_REPO_1_2_1,
                   BYAFILE.BYA_AMT_REPO_1_2_1,
                   BYAFILE.BYA_O2_REPO_1_2_1,
                   BYAFILE.BYA_INT_REPO_1_2_1,
                   BYAFILE.BYA_NO_REPO_1_2_2,
                   BYAFILE.BYA_AMT_REPO_1_2_2,
                   BYAFILE.BYA_O2_REPO_1_2_2,
                   BYAFILE.BYA_INT_REPO_1_2_2,
                   BYAFILE.BYA_NO_REPO_1_2_3,
                   BYAFILE.BYA_AMT_REPO_1_2_3,
                   BYAFILE.BYA_O2_REPO_1_2_3,
                   BYAFILE.BYA_INT_REPO_1_2_3,
                   BYAFILE.BYA_NO_REPO_1_2_4,
                   BYAFILE.BYA_AMT_REPO_1_2_4,
                   BYAFILE.BYA_O2_REPO_1_2_4,
                   BYAFILE.BYA_INT_REPO_1_2_4,
                   BYAFILE.BYA_NO_REPO_1_2_5,
                   BYAFILE.BYA_AMT_REPO_1_2_5,
                   BYAFILE.BYA_O2_REPO_1_2_5,
                   BYAFILE.BYA_INT_REPO_1_2_5,
                   BYAFILE.BYA_NO_REPO_2_1_1,
                   BYAFILE.BYA_AMT_REPO_2_1_1,
                   BYAFILE.BYA_O2_REPO_2_1_1,
                   BYAFILE.BYA_INT_REPO_2_1_1,
                   BYAFILE.BYA_NO_REPO_2_1_2,
                   BYAFILE.BYA_AMT_REPO_2_1_2,
                   BYAFILE.BYA_O2_REPO_2_1_2,
                   BYAFILE.BYA_INT_REPO_2_1_2,
                   BYAFILE.BYA_NO_REPO_2_1_3,
                   BYAFILE.BYA_AMT_REPO_2_1_3,
                   BYAFILE.BYA_O2_REPO_2_1_3,
                   BYAFILE.BYA_INT_REPO_2_1_3,
                   BYAFILE.BYA_NO_REPO_2_1_4,
                   BYAFILE.BYA_AMT_REPO_2_1_4,
                   BYAFILE.BYA_O2_REPO_2_1_4,
                   BYAFILE.BYA_INT_REPO_2_1_4,
                   BYAFILE.BYA_NO_REPO_2_1_5,
                   BYAFILE.BYA_AMT_REPO_2_1_5,
                   BYAFILE.BYA_O2_REPO_2_1_5,
                   BYAFILE.BYA_INT_REPO_2_1_5,
                   BYAFILE.BYA_NO_REPO_2_2_1,
                   BYAFILE.BYA_AMT_REPO_2_2_1,
                   BYAFILE.BYA_O2_REPO_2_2_1,
                   BYAFILE.BYA_INT_REPO_2_2_1,
                   BYAFILE.BYA_NO_REPO_2_2_2,
                   BYAFILE.BYA_AMT_REPO_2_2_2,
                   BYAFILE.BYA_O2_REPO_2_2_2,
                   BYAFILE.BYA_INT_REPO_2_2_2,
                   BYAFILE.BYA_NO_REPO_2_2_3,
                   BYAFILE.BYA_AMT_REPO_2_2_3,
                   BYAFILE.BYA_O2_REPO_2_2_3,
                   BYAFILE.BYA_INT_REPO_2_2_3,
                   BYAFILE.BYA_NO_REPO_2_2_4,
                   BYAFILE.BYA_AMT_REPO_2_2_4,
                   BYAFILE.BYA_O2_REPO_2_2_4,
                   BYAFILE.BYA_INT_REPO_2_2_4,
                   BYAFILE.BYA_NO_REPO_2_2_5,
                   BYAFILE.BYA_AMT_REPO_2_2_5,
                   BYAFILE.BYA_O2_REPO_2_2_5,
                   BYAFILE.BYA_INT_REPO_2_2_5,
                   BYAFILE.BYA_NO_REPO_3_1_1,
                   BYAFILE.BYA_AMT_REPO_3_1_1,
                   BYAFILE.BYA_O2_REPO_3_1_1,
                   BYAFILE.BYA_INT_REPO_3_1_1,
                   BYAFILE.BYA_NO_REPO_3_1_2,
                   BYAFILE.BYA_AMT_REPO_3_1_2,
                   BYAFILE.BYA_O2_REPO_3_1_2,
                   BYAFILE.BYA_INT_REPO_3_1_2,
                   BYAFILE.BYA_NO_REPO_3_1_3,
                   BYAFILE.BYA_AMT_REPO_3_1_3,
                   BYAFILE.BYA_O2_REPO_3_1_3,
                   BYAFILE.BYA_INT_REPO_3_1_3,
                   BYAFILE.BYA_NO_REPO_3_1_4,
                   BYAFILE.BYA_AMT_REPO_3_1_4,
                   BYAFILE.BYA_O2_REPO_3_1_4,
                   BYAFILE.BYA_INT_REPO_3_1_4,
                   BYAFILE.BYA_NO_REPO_3_1_5,
                   BYAFILE.BYA_AMT_REPO_3_1_5,
                   BYAFILE.BYA_O2_REPO_3_1_5,
                   BYAFILE.BYA_INT_REPO_3_1_5,
                   BYAFILE.BYA_NO_REPO_3_2_1,
                   BYAFILE.BYA_AMT_REPO_3_2_1,
                   BYAFILE.BYA_O2_REPO_3_2_1,
                   BYAFILE.BYA_INT_REPO_3_2_1,
                   BYAFILE.BYA_NO_REPO_3_2_2,
                   BYAFILE.BYA_AMT_REPO_3_2_2,
                   BYAFILE.BYA_O2_REPO_3_2_2,
                   BYAFILE.BYA_INT_REPO_3_2_2,
                   BYAFILE.BYA_NO_REPO_3_2_3,
                   BYAFILE.BYA_AMT_REPO_3_2_3,
                   BYAFILE.BYA_O2_REPO_3_2_3,
                   BYAFILE.BYA_INT_REPO_3_2_3,
                   BYAFILE.BYA_NO_REPO_3_2_4,
                   BYAFILE.BYA_AMT_REPO_3_2_4,
                   BYAFILE.BYA_O2_REPO_3_2_4,
                   BYAFILE.BYA_INT_REPO_3_2_4,
                   BYAFILE.BYA_NO_REPO_3_2_5,
                   BYAFILE.BYA_AMT_REPO_3_2_5,
                   BYAFILE.BYA_O2_REPO_3_2_5,
                   BYAFILE.BYA_INT_REPO_3_2_5,
                   BYAFILE.BYA_NO_PL_1_1_1,
                   BYAFILE.BYA_AMT_PL_1_1_1,
                   BYAFILE.BYA_O2_PL_1_1_1,
                   BYAFILE.BYA_INT_PL_1_1_1,
                   BYAFILE.BYA_NO_PL_1_1_2,
                   BYAFILE.BYA_AMT_PL_1_1_2,
                   BYAFILE.BYA_O2_PL_1_1_2,
                   BYAFILE.BYA_INT_PL_1_1_2,
                   BYAFILE.BYA_NO_PL_1_1_3,
                   BYAFILE.BYA_AMT_PL_1_1_3,
                   BYAFILE.BYA_O2_PL_1_1_3,
                   BYAFILE.BYA_INT_PL_1_1_3,
                   BYAFILE.BYA_NO_PL_1_1_4,
                   BYAFILE.BYA_AMT_PL_1_1_4,
                   BYAFILE.BYA_O2_PL_1_1_4,
                   BYAFILE.BYA_INT_PL_1_1_4,
                   BYAFILE.BYA_NO_PL_1_1_5,
                   BYAFILE.BYA_AMT_PL_1_1_5,
                   BYAFILE.BYA_O2_PL_1_1_5,
                   BYAFILE.BYA_INT_PL_1_1_5,
                   BYAFILE.BYA_NO_PL_1_1_6,
                   BYAFILE.BYA_AMT_PL_1_1_6,
                   BYAFILE.BYA_O2_PL_1_1_6,
                   BYAFILE.BYA_INT_PL_1_1_6,
                   BYAFILE.BYA_NO_PL_1_1_7,
                   BYAFILE.BYA_AMT_PL_1_1_7,
                   BYAFILE.BYA_O2_PL_1_1_7,
                   BYAFILE.BYA_INT_PL_1_1_7,
                   BYAFILE.BYA_NO_PL_1_1_8,
                   BYAFILE.BYA_AMT_PL_1_1_8,
                   BYAFILE.BYA_O2_PL_1_1_8,
                   BYAFILE.BYA_INT_PL_1_1_8,
                   BYAFILE.BYA_NO_PL_1_1_9,
                   BYAFILE.BYA_AMT_PL_1_1_9,
                   BYAFILE.BYA_O2_PL_1_1_9,
                   BYAFILE.BYA_INT_PL_1_1_9,
                   BYAFILE.BYA_NO_PL_1_1_10,
                   BYAFILE.BYA_AMT_PL_1_1_10,
                   BYAFILE.BYA_O2_PL_1_1_10,
                   BYAFILE.BYA_INT_PL_1_1_10,
                   BYAFILE.BYA_NO_PL_1_2_1,
                   BYAFILE.BYA_AMT_PL_1_2_1,
                   BYAFILE.BYA_O2_PL_1_2_1,
                   BYAFILE.BYA_INT_PL_1_2_1,
                   BYAFILE.BYA_NO_PL_1_2_2,
                   BYAFILE.BYA_AMT_PL_1_2_2,
                   BYAFILE.BYA_O2_PL_1_2_2,
                   BYAFILE.BYA_INT_PL_1_2_2,
                   BYAFILE.BYA_NO_PL_1_2_3,
                   BYAFILE.BYA_AMT_PL_1_2_3,
                   BYAFILE.BYA_O2_PL_1_2_3,
                   BYAFILE.BYA_INT_PL_1_2_3,
                   BYAFILE.BYA_NO_PL_1_2_4,
                   BYAFILE.BYA_AMT_PL_1_2_4,
                   BYAFILE.BYA_O2_PL_1_2_4,
                   BYAFILE.BYA_INT_PL_1_2_4,
                   BYAFILE.BYA_NO_PL_1_2_5,
                   BYAFILE.BYA_AMT_PL_1_2_5,
                   BYAFILE.BYA_O2_PL_1_2_5,
                   BYAFILE.BYA_INT_PL_1_2_5,
                   BYAFILE.BYA_NO_PL_1_2_6,
                   BYAFILE.BYA_AMT_PL_1_2_6,
                   BYAFILE.BYA_O2_PL_1_2_6,
                   BYAFILE.BYA_INT_PL_1_2_6,
                   BYAFILE.BYA_NO_PL_1_2_7,
                   BYAFILE.BYA_AMT_PL_1_2_7,
                   BYAFILE.BYA_O2_PL_1_2_7,
                   BYAFILE.BYA_INT_PL_1_2_7,
                   BYAFILE.BYA_NO_PL_1_2_8,
                   BYAFILE.BYA_AMT_PL_1_2_8,
                   BYAFILE.BYA_O2_PL_1_2_8,
                   BYAFILE.BYA_INT_PL_1_2_8,
                   BYAFILE.BYA_NO_PL_1_2_9,
                   BYAFILE.BYA_AMT_PL_1_2_9,
                   BYAFILE.BYA_O2_PL_1_2_9,
                   BYAFILE.BYA_INT_PL_1_2_9,
                   BYAFILE.BYA_NO_PL_1_2_10,
                   BYAFILE.BYA_AMT_PL_1_2_10,
                   BYAFILE.BYA_O2_PL_1_2_10,
                   BYAFILE.BYA_INT_PL_1_2_10,
                   BYAFILE.BYA_NO_PL_2_1_1,
                   BYAFILE.BYA_AMT_PL_2_1_1,
                   BYAFILE.BYA_O2_PL_2_1_1,
                   BYAFILE.BYA_INT_PL_2_1_1,
                   BYAFILE.BYA_NO_PL_2_1_2,
                   BYAFILE.BYA_AMT_PL_2_1_2,
                   BYAFILE.BYA_O2_PL_2_1_2,
                   BYAFILE.BYA_INT_PL_2_1_2,
                   BYAFILE.BYA_NO_PL_2_1_3,
                   BYAFILE.BYA_AMT_PL_2_1_3,
                   BYAFILE.BYA_O2_PL_2_1_3,
                   BYAFILE.BYA_INT_PL_2_1_3,
                   BYAFILE.BYA_NO_PL_2_1_4,
                   BYAFILE.BYA_AMT_PL_2_1_4,
                   BYAFILE.BYA_O2_PL_2_1_4,
                   BYAFILE.BYA_INT_PL_2_1_4,
                   BYAFILE.BYA_NO_PL_2_1_5,
                   BYAFILE.BYA_AMT_PL_2_1_5,
                   BYAFILE.BYA_O2_PL_2_1_5,
                   BYAFILE.BYA_INT_PL_2_1_5,
                   BYAFILE.BYA_NO_PL_2_1_6,
                   BYAFILE.BYA_AMT_PL_2_1_6,
                   BYAFILE.BYA_O2_PL_2_1_6,
                   BYAFILE.BYA_INT_PL_2_1_6,
                   BYAFILE.BYA_NO_PL_2_1_7,
                   BYAFILE.BYA_AMT_PL_2_1_7,
                   BYAFILE.BYA_O2_PL_2_1_7,
                   BYAFILE.BYA_INT_PL_2_1_7,
                   BYAFILE.BYA_NO_PL_2_1_8,
                   BYAFILE.BYA_AMT_PL_2_1_8,
                   BYAFILE.BYA_O2_PL_2_1_8,
                   BYAFILE.BYA_INT_PL_2_1_8,
                   BYAFILE.BYA_NO_PL_2_1_9,
                   BYAFILE.BYA_AMT_PL_2_1_9,
                   BYAFILE.BYA_O2_PL_2_1_9,
                   BYAFILE.BYA_INT_PL_2_1_9,
                   BYAFILE.BYA_NO_PL_2_1_10,
                   BYAFILE.BYA_AMT_PL_2_1_10,
                   BYAFILE.BYA_O2_PL_2_1_10,
                   BYAFILE.BYA_INT_PL_2_1_10,
                   BYAFILE.BYA_NO_PL_2_2_1,
                   BYAFILE.BYA_AMT_PL_2_2_1,
                   BYAFILE.BYA_O2_PL_2_2_1,
                   BYAFILE.BYA_INT_PL_2_2_1,
                   BYAFILE.BYA_NO_PL_2_2_2,
                   BYAFILE.BYA_AMT_PL_2_2_2,
                   BYAFILE.BYA_O2_PL_2_2_2,
                   BYAFILE.BYA_INT_PL_2_2_2,
                   BYAFILE.BYA_NO_PL_2_2_3,
                   BYAFILE.BYA_AMT_PL_2_2_3,
                   BYAFILE.BYA_O2_PL_2_2_3,
                   BYAFILE.BYA_INT_PL_2_2_3,
                   BYAFILE.BYA_NO_PL_2_2_4,
                   BYAFILE.BYA_AMT_PL_2_2_4,
                   BYAFILE.BYA_O2_PL_2_2_4,
                   BYAFILE.BYA_INT_PL_2_2_4,
                   BYAFILE.BYA_NO_PL_2_2_5,
                   BYAFILE.BYA_AMT_PL_2_2_5,
                   BYAFILE.BYA_O2_PL_2_2_5,
                   BYAFILE.BYA_INT_PL_2_2_5,
                   BYAFILE.BYA_NO_PL_2_2_6,
                   BYAFILE.BYA_AMT_PL_2_2_6,
                   BYAFILE.BYA_O2_PL_2_2_6,
                   BYAFILE.BYA_INT_PL_2_2_6,
                   BYAFILE.BYA_NO_PL_2_2_7,
                   BYAFILE.BYA_AMT_PL_2_2_7,
                   BYAFILE.BYA_O2_PL_2_2_7,
                   BYAFILE.BYA_INT_PL_2_2_7,
                   BYAFILE.BYA_NO_PL_2_2_8,
                   BYAFILE.BYA_AMT_PL_2_2_8,
                   BYAFILE.BYA_O2_PL_2_2_8,
                   BYAFILE.BYA_INT_PL_2_2_8,
                   BYAFILE.BYA_NO_PL_2_2_9,
                   BYAFILE.BYA_AMT_PL_2_2_9,
                   BYAFILE.BYA_O2_PL_2_2_9,
                   BYAFILE.BYA_INT_PL_2_2_9,
                   BYAFILE.BYA_NO_PL_2_2_10,
                   BYAFILE.BYA_AMT_PL_2_2_10,
                   BYAFILE.BYA_O2_PL_2_2_10,
                   BYAFILE.BYA_INT_PL_2_2_10,
                   BYAFILE.BYA_NO_PL_3_1_1,
                   BYAFILE.BYA_AMT_PL_3_1_1,
                   BYAFILE.BYA_O2_PL_3_1_1,
                   BYAFILE.BYA_INT_PL_3_1_1,
                   BYAFILE.BYA_NO_PL_3_1_2,
                   BYAFILE.BYA_AMT_PL_3_1_2,
                   BYAFILE.BYA_O2_PL_3_1_2,
                   BYAFILE.BYA_INT_PL_3_1_2,
                   BYAFILE.BYA_NO_PL_3_1_3,
                   BYAFILE.BYA_AMT_PL_3_1_3,
                   BYAFILE.BYA_O2_PL_3_1_3,
                   BYAFILE.BYA_INT_PL_3_1_3,
                   BYAFILE.BYA_NO_PL_3_1_4,
                   BYAFILE.BYA_AMT_PL_3_1_4,
                   BYAFILE.BYA_O2_PL_3_1_4,
                   BYAFILE.BYA_INT_PL_3_1_4,
                   BYAFILE.BYA_NO_PL_3_1_5,
                   BYAFILE.BYA_AMT_PL_3_1_5,
                   BYAFILE.BYA_O2_PL_3_1_5,
                   BYAFILE.BYA_INT_PL_3_1_5,
                   BYAFILE.BYA_NO_PL_3_1_6,
                   BYAFILE.BYA_AMT_PL_3_1_6,
                   BYAFILE.BYA_O2_PL_3_1_6,
                   BYAFILE.BYA_INT_PL_3_1_6,
                   BYAFILE.BYA_NO_PL_3_1_7,
                   BYAFILE.BYA_AMT_PL_3_1_7,
                   BYAFILE.BYA_O2_PL_3_1_7,
                   BYAFILE.BYA_INT_PL_3_1_7,
                   BYAFILE.BYA_NO_PL_3_1_8,
                   BYAFILE.BYA_AMT_PL_3_1_8,
                   BYAFILE.BYA_O2_PL_3_1_8,
                   BYAFILE.BYA_INT_PL_3_1_8,
                   BYAFILE.BYA_NO_PL_3_1_9,
                   BYAFILE.BYA_AMT_PL_3_1_9,
                   BYAFILE.BYA_O2_PL_3_1_9,
                   BYAFILE.BYA_INT_PL_3_1_9,
                   BYAFILE.BYA_NO_PL_3_1_10,
                   BYAFILE.BYA_AMT_PL_3_1_10,
                   BYAFILE.BYA_O2_PL_3_1_10,
                   BYAFILE.BYA_INT_PL_3_1_10,
                   BYAFILE.BYA_NO_PL_3_2_1,
                   BYAFILE.BYA_AMT_PL_3_2_1,
                   BYAFILE.BYA_O2_PL_3_2_1,
                   BYAFILE.BYA_INT_PL_3_2_1,
                   BYAFILE.BYA_NO_PL_3_2_2,
                   BYAFILE.BYA_AMT_PL_3_2_2,
                   BYAFILE.BYA_O2_PL_3_2_2,
                   BYAFILE.BYA_INT_PL_3_2_2,
                   BYAFILE.BYA_NO_PL_3_2_3,
                   BYAFILE.BYA_AMT_PL_3_2_3,
                   BYAFILE.BYA_O2_PL_3_2_3,
                   BYAFILE.BYA_INT_PL_3_2_3,
                   BYAFILE.BYA_NO_PL_3_2_4,
                   BYAFILE.BYA_AMT_PL_3_2_4,
                   BYAFILE.BYA_O2_PL_3_2_4,
                   BYAFILE.BYA_INT_PL_3_2_4,
                   BYAFILE.BYA_NO_PL_3_2_5,
                   BYAFILE.BYA_AMT_PL_3_2_5,
                   BYAFILE.BYA_O2_PL_3_2_5,
                   BYAFILE.BYA_INT_PL_3_2_5,
                   BYAFILE.BYA_NO_PL_3_2_6,
                   BYAFILE.BYA_AMT_PL_3_2_6,
                   BYAFILE.BYA_O2_PL_3_2_6,
                   BYAFILE.BYA_INT_PL_3_2_6,
                   BYAFILE.BYA_NO_PL_3_2_7,
                   BYAFILE.BYA_AMT_PL_3_2_7,
                   BYAFILE.BYA_O2_PL_3_2_7,
                   BYAFILE.BYA_INT_PL_3_2_7,
                   BYAFILE.BYA_NO_PL_3_2_8,
                   BYAFILE.BYA_AMT_PL_3_2_8,
                   BYAFILE.BYA_O2_PL_3_2_8,
                   BYAFILE.BYA_INT_PL_3_2_8,
                   BYAFILE.BYA_NO_PL_3_2_9,
                   BYAFILE.BYA_AMT_PL_3_2_9,
                   BYAFILE.BYA_O2_PL_3_2_9,
                   BYAFILE.BYA_INT_PL_3_2_9,
                   BYAFILE.BYA_NO_PL_3_2_10,
                   BYAFILE.BYA_AMT_PL_3_2_10,
                   BYAFILE.BYA_O2_PL_3_2_10,
                   BYAFILE.BYA_INT_PL_3_2_10,
                   BYAFILE.BYA_DLRVPRIN_1_1_1,
                   BYAFILE.BYA_DLRVPRIN_1_1_2,
                   BYAFILE.BYA_DLRVPRIN_1_1_3,
                   BYAFILE.BYA_DLRVCHGS_1_1_1,
                   BYAFILE.BYA_DLRVCHGS_1_1_2,
                   BYAFILE.BYA_DLRVCHGS_1_1_3,
                   BYAFILE.BYA_DLPARVHELD_1_1,
                   BYAFILE.BYA_DLRVPRIN_1_2_1,
                   BYAFILE.BYA_DLRVPRIN_1_2_2,
                   BYAFILE.BYA_DLRVPRIN_1_2_3,
                   BYAFILE.BYA_DLRVCHGS_1_2_1,
                   BYAFILE.BYA_DLRVCHGS_1_2_2,
                   BYAFILE.BYA_DLRVCHGS_1_2_3,
                   BYAFILE.BYA_DLPARVHELD_1_2,
                   BYAFILE.BYA_DLRVPRIN_2_1_1,
                   BYAFILE.BYA_DLRVPRIN_2_1_2,
                   BYAFILE.BYA_DLRVPRIN_2_1_3,
                   BYAFILE.BYA_DLRVCHGS_2_1_1,
                   BYAFILE.BYA_DLRVCHGS_2_1_2,
                   BYAFILE.BYA_DLRVCHGS_2_1_3,
                   BYAFILE.BYA_DLPARVHELD_2_1,
                   BYAFILE.BYA_DLRVPRIN_2_2_1,
                   BYAFILE.BYA_DLRVPRIN_2_2_2,
                   BYAFILE.BYA_DLRVPRIN_2_2_3,
                   BYAFILE.BYA_DLRVCHGS_2_2_1,
                   BYAFILE.BYA_DLRVCHGS_2_2_2,
                   BYAFILE.BYA_DLRVCHGS_2_2_3,
                   BYAFILE.BYA_DLPARVHELD_2_2,
                   BYAFILE.BYA_DLRVPRIN_3_1_1,
                   BYAFILE.BYA_DLRVPRIN_3_1_2,
                   BYAFILE.BYA_DLRVPRIN_3_1_3,
                   BYAFILE.BYA_DLRVCHGS_3_1_1,
                   BYAFILE.BYA_DLRVCHGS_3_1_2,
                   BYAFILE.BYA_DLRVCHGS_3_1_3,
                   BYAFILE.BYA_DLPARVHELD_3_1,
                   BYAFILE.BYA_DLRVPRIN_3_2_1,
                   BYAFILE.BYA_DLRVPRIN_3_2_2,
                   BYAFILE.BYA_DLRVPRIN_3_2_3,
                   BYAFILE.BYA_DLRVCHGS_3_2_1,
                   BYAFILE.BYA_DLRVCHGS_3_2_2,
                   BYAFILE.BYA_DLRVCHGS_3_2_3,
                   BYAFILE.BYA_DLPARVHELD_3_2
            FROM DBO.BYAFILE
            WHERE BYAFILE.BYA_DATE_CCYYMM >= :QBY1-WBEG-DATE-CCYYMM
              AND BYAFILE.BYA_DATE_CCYYMM <= :QBY1-WEND-DATE-CCYYMM
              AND BYAFILE.BYA_BRNO >= :QBY1-WBEG-BRNO
              AND BYAFILE.BYA_BRNO <= :QBY1-WEND-BRNO 
              AND BYAFILE.BYA_CLASS >= :QBY1-WBEG-CLASS
              AND BYAFILE.BYA_CLASS <= :QBY1-WEND-CLASS
            ORDER BY BYAFILE.BYA_DATE_CCYYMM,
                     BYAFILE.BYA_BRNO,
                     BYAFILE.BYA_CLASS
           END-EXEC.

           EXEC SQL
               OPEN BYA1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QBY1-WBEG-DATE-CCYYMM
                          QBY1-WEND-DATE-CCYYMM
                          QBY1-WBEG-BRNO
                          QBY1-WEND-BRNO
                          QBY1-WBEG-CLASS
                          QBY1-WEND-CLASS.

           MOVE STAT---GOOD TO BYA1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-BYA1-FILE. *> EMBEDDED - BYAFILE
           PERFORM READ-IT.
           MOVE BYA-PATH-SQL TO E-FILE.
           MOVE BYA1-KEY TO E-KEYX.
           INITIALIZE QBYA-REC.

           MOVE BY-DATE-CCYYMM      TO QBYA-DATE-CCYYMM.
           MOVE BY-BRNO             TO QBYA-BRNO.
           MOVE BY-CLASS            TO QBYA-CLASS.

           EXEC SQL
            SELECT BYAFILE.BYA_DATE_CCYYMM,
                   BYAFILE.BYA_BRNO,
                   BYAFILE.BYA_CLASS,
                   CAST(BYAFILE.BYA_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   BYAFILE.BYA_NO_1_1_1,
                   BYAFILE.BYA_AMT_1_1_1,
                   BYAFILE.BYA_O2_1_1_1,
                   BYAFILE.BYA_INT_1_1_1,
                   BYAFILE.BYA_NO_1_1_2,
                   BYAFILE.BYA_AMT_1_1_2,
                   BYAFILE.BYA_O2_1_1_2,
                   BYAFILE.BYA_INT_1_1_2,
                   BYAFILE.BYA_NO_1_1_3,
                   BYAFILE.BYA_AMT_1_1_3,
                   BYAFILE.BYA_O2_1_1_3,
                   BYAFILE.BYA_INT_1_1_3,
                   BYAFILE.BYA_NO_1_1_4,
                   BYAFILE.BYA_AMT_1_1_4,
                   BYAFILE.BYA_O2_1_1_4,
                   BYAFILE.BYA_INT_1_1_4,
                   BYAFILE.BYA_NO_1_1_5,
                   BYAFILE.BYA_AMT_1_1_5,
                   BYAFILE.BYA_O2_1_1_5,
                   BYAFILE.BYA_INT_1_1_5,
                   BYAFILE.BYA_NO_1_1_6,
                   BYAFILE.BYA_AMT_1_1_6,
                   BYAFILE.BYA_O2_1_1_6,
                   BYAFILE.BYA_INT_1_1_6,
                   BYAFILE.BYA_NO_1_1_7,
                   BYAFILE.BYA_AMT_1_1_7,
                   BYAFILE.BYA_O2_1_1_7,
                   BYAFILE.BYA_INT_1_1_7,
                   BYAFILE.BYA_NO_1_1_8,
                   BYAFILE.BYA_AMT_1_1_8,
                   BYAFILE.BYA_O2_1_1_8,
                   BYAFILE.BYA_INT_1_1_8,
                   BYAFILE.BYA_NO_1_1_9,
                   BYAFILE.BYA_AMT_1_1_9,
                   BYAFILE.BYA_O2_1_1_9,
                   BYAFILE.BYA_INT_1_1_9,
                   BYAFILE.BYA_NO_1_1_10,
                   BYAFILE.BYA_AMT_1_1_10,
                   BYAFILE.BYA_O2_1_1_10,
                   BYAFILE.BYA_INT_1_1_10,
                   BYAFILE.BYA_NO_1_1_11,
                   BYAFILE.BYA_AMT_1_1_11,
                   BYAFILE.BYA_O2_1_1_11,
                   BYAFILE.BYA_INT_1_1_11,
                   BYAFILE.BYA_NO_1_2_1,
                   BYAFILE.BYA_AMT_1_2_1,
                   BYAFILE.BYA_O2_1_2_1,
                   BYAFILE.BYA_INT_1_2_1,
                   BYAFILE.BYA_NO_1_2_2,
                   BYAFILE.BYA_AMT_1_2_2,
                   BYAFILE.BYA_O2_1_2_2,
                   BYAFILE.BYA_INT_1_2_2,
                   BYAFILE.BYA_NO_1_2_3,
                   BYAFILE.BYA_AMT_1_2_3,
                   BYAFILE.BYA_O2_1_2_3,
                   BYAFILE.BYA_INT_1_2_3,
                   BYAFILE.BYA_NO_1_2_4,
                   BYAFILE.BYA_AMT_1_2_4,
                   BYAFILE.BYA_O2_1_2_4,
                   BYAFILE.BYA_INT_1_2_4,
                   BYAFILE.BYA_NO_1_2_5,
                   BYAFILE.BYA_AMT_1_2_5,
                   BYAFILE.BYA_O2_1_2_5,
                   BYAFILE.BYA_INT_1_2_5,
                   BYAFILE.BYA_NO_1_2_6,
                   BYAFILE.BYA_AMT_1_2_6,
                   BYAFILE.BYA_O2_1_2_6,
                   BYAFILE.BYA_INT_1_2_6,
                   BYAFILE.BYA_NO_1_2_7,
                   BYAFILE.BYA_AMT_1_2_7,
                   BYAFILE.BYA_O2_1_2_7,
                   BYAFILE.BYA_INT_1_2_7,
                   BYAFILE.BYA_NO_1_2_8,
                   BYAFILE.BYA_AMT_1_2_8,
                   BYAFILE.BYA_O2_1_2_8,
                   BYAFILE.BYA_INT_1_2_8,
                   BYAFILE.BYA_NO_1_2_9,
                   BYAFILE.BYA_AMT_1_2_9,
                   BYAFILE.BYA_O2_1_2_9,
                   BYAFILE.BYA_INT_1_2_9,
                   BYAFILE.BYA_NO_1_2_10,
                   BYAFILE.BYA_AMT_1_2_10,
                   BYAFILE.BYA_O2_1_2_10,
                   BYAFILE.BYA_INT_1_2_10,
                   BYAFILE.BYA_NO_1_2_11,
                   BYAFILE.BYA_AMT_1_2_11,
                   BYAFILE.BYA_O2_1_2_11,
                   BYAFILE.BYA_INT_1_2_11,
                   BYAFILE.BYA_NO_2_1_1,
                   BYAFILE.BYA_AMT_2_1_1,
                   BYAFILE.BYA_O2_2_1_1,
                   BYAFILE.BYA_INT_2_1_1,
                   BYAFILE.BYA_NO_2_1_2,
                   BYAFILE.BYA_AMT_2_1_2,
                   BYAFILE.BYA_O2_2_1_2,
                   BYAFILE.BYA_INT_2_1_2,
                   BYAFILE.BYA_NO_2_1_3,
                   BYAFILE.BYA_AMT_2_1_3,
                   BYAFILE.BYA_O2_2_1_3,
                   BYAFILE.BYA_INT_2_1_3,
                   BYAFILE.BYA_NO_2_1_4,
                   BYAFILE.BYA_AMT_2_1_4,
                   BYAFILE.BYA_O2_2_1_4,
                   BYAFILE.BYA_INT_2_1_4,
                   BYAFILE.BYA_NO_2_1_5,
                   BYAFILE.BYA_AMT_2_1_5,
                   BYAFILE.BYA_O2_2_1_5,
                   BYAFILE.BYA_INT_2_1_5,
                   BYAFILE.BYA_NO_2_1_6,
                   BYAFILE.BYA_AMT_2_1_6,
                   BYAFILE.BYA_O2_2_1_6,
                   BYAFILE.BYA_INT_2_1_6,
                   BYAFILE.BYA_NO_2_1_7,
                   BYAFILE.BYA_AMT_2_1_7,
                   BYAFILE.BYA_O2_2_1_7,
                   BYAFILE.BYA_INT_2_1_7,
                   BYAFILE.BYA_NO_2_1_8,
                   BYAFILE.BYA_AMT_2_1_8,
                   BYAFILE.BYA_O2_2_1_8,
                   BYAFILE.BYA_INT_2_1_8,
                   BYAFILE.BYA_NO_2_1_9,
                   BYAFILE.BYA_AMT_2_1_9,
                   BYAFILE.BYA_O2_2_1_9,
                   BYAFILE.BYA_INT_2_1_9,
                   BYAFILE.BYA_NO_2_1_10,
                   BYAFILE.BYA_AMT_2_1_10,
                   BYAFILE.BYA_O2_2_1_10,
                   BYAFILE.BYA_INT_2_1_10,
                   BYAFILE.BYA_NO_2_1_11,
                   BYAFILE.BYA_AMT_2_1_11,
                   BYAFILE.BYA_O2_2_1_11,
                   BYAFILE.BYA_INT_2_1_11,
                   BYAFILE.BYA_NO_2_2_1,
                   BYAFILE.BYA_AMT_2_2_1,
                   BYAFILE.BYA_O2_2_2_1,
                   BYAFILE.BYA_INT_2_2_1,
                   BYAFILE.BYA_NO_2_2_2,
                   BYAFILE.BYA_AMT_2_2_2,
                   BYAFILE.BYA_O2_2_2_2,
                   BYAFILE.BYA_INT_2_2_2,
                   BYAFILE.BYA_NO_2_2_3,
                   BYAFILE.BYA_AMT_2_2_3,
                   BYAFILE.BYA_O2_2_2_3,
                   BYAFILE.BYA_INT_2_2_3,
                   BYAFILE.BYA_NO_2_2_4,
                   BYAFILE.BYA_AMT_2_2_4,
                   BYAFILE.BYA_O2_2_2_4,
                   BYAFILE.BYA_INT_2_2_4,
                   BYAFILE.BYA_NO_2_2_5,
                   BYAFILE.BYA_AMT_2_2_5,
                   BYAFILE.BYA_O2_2_2_5,
                   BYAFILE.BYA_INT_2_2_5,
                   BYAFILE.BYA_NO_2_2_6,
                   BYAFILE.BYA_AMT_2_2_6,
                   BYAFILE.BYA_O2_2_2_6,
                   BYAFILE.BYA_INT_2_2_6,
                   BYAFILE.BYA_NO_2_2_7,
                   BYAFILE.BYA_AMT_2_2_7,
                   BYAFILE.BYA_O2_2_2_7,
                   BYAFILE.BYA_INT_2_2_7,
                   BYAFILE.BYA_NO_2_2_8,
                   BYAFILE.BYA_AMT_2_2_8,
                   BYAFILE.BYA_O2_2_2_8,
                   BYAFILE.BYA_INT_2_2_8,
                   BYAFILE.BYA_NO_2_2_9,
                   BYAFILE.BYA_AMT_2_2_9,
                   BYAFILE.BYA_O2_2_2_9,
                   BYAFILE.BYA_INT_2_2_9,
                   BYAFILE.BYA_NO_2_2_10,
                   BYAFILE.BYA_AMT_2_2_10,
                   BYAFILE.BYA_O2_2_2_10,
                   BYAFILE.BYA_INT_2_2_10,
                   BYAFILE.BYA_NO_2_2_11,
                   BYAFILE.BYA_AMT_2_2_11,
                   BYAFILE.BYA_O2_2_2_11,
                   BYAFILE.BYA_INT_2_2_11,
                   BYAFILE.BYA_NO_3_1_1,
                   BYAFILE.BYA_AMT_3_1_1,
                   BYAFILE.BYA_O2_3_1_1,
                   BYAFILE.BYA_INT_3_1_1,
                   BYAFILE.BYA_NO_3_1_2,
                   BYAFILE.BYA_AMT_3_1_2,
                   BYAFILE.BYA_O2_3_1_2,
                   BYAFILE.BYA_INT_3_1_2,
                   BYAFILE.BYA_NO_3_1_3,
                   BYAFILE.BYA_AMT_3_1_3,
                   BYAFILE.BYA_O2_3_1_3,
                   BYAFILE.BYA_INT_3_1_3,
                   BYAFILE.BYA_NO_3_1_4,
                   BYAFILE.BYA_AMT_3_1_4,
                   BYAFILE.BYA_O2_3_1_4,
                   BYAFILE.BYA_INT_3_1_4,
                   BYAFILE.BYA_NO_3_1_5,
                   BYAFILE.BYA_AMT_3_1_5,
                   BYAFILE.BYA_O2_3_1_5,
                   BYAFILE.BYA_INT_3_1_5,
                   BYAFILE.BYA_NO_3_1_6,
                   BYAFILE.BYA_AMT_3_1_6,
                   BYAFILE.BYA_O2_3_1_6,
                   BYAFILE.BYA_INT_3_1_6,
                   BYAFILE.BYA_NO_3_1_7,
                   BYAFILE.BYA_AMT_3_1_7,
                   BYAFILE.BYA_O2_3_1_7,
                   BYAFILE.BYA_INT_3_1_7,
                   BYAFILE.BYA_NO_3_1_8,
                   BYAFILE.BYA_AMT_3_1_8,
                   BYAFILE.BYA_O2_3_1_8,
                   BYAFILE.BYA_INT_3_1_8,
                   BYAFILE.BYA_NO_3_1_9,
                   BYAFILE.BYA_AMT_3_1_9,
                   BYAFILE.BYA_O2_3_1_9,
                   BYAFILE.BYA_INT_3_1_9,
                   BYAFILE.BYA_NO_3_1_10,
                   BYAFILE.BYA_AMT_3_1_10,
                   BYAFILE.BYA_O2_3_1_10,
                   BYAFILE.BYA_INT_3_1_10,
                   BYAFILE.BYA_NO_3_1_11,
                   BYAFILE.BYA_AMT_3_1_11,
                   BYAFILE.BYA_O2_3_1_11,
                   BYAFILE.BYA_INT_3_1_11,
                   BYAFILE.BYA_NO_3_2_1,
                   BYAFILE.BYA_AMT_3_2_1,
                   BYAFILE.BYA_O2_3_2_1,
                   BYAFILE.BYA_INT_3_2_1,
                   BYAFILE.BYA_NO_3_2_2,
                   BYAFILE.BYA_AMT_3_2_2,
                   BYAFILE.BYA_O2_3_2_2,
                   BYAFILE.BYA_INT_3_2_2,
                   BYAFILE.BYA_NO_3_2_3,
                   BYAFILE.BYA_AMT_3_2_3,
                   BYAFILE.BYA_O2_3_2_3,
                   BYAFILE.BYA_INT_3_2_3,
                   BYAFILE.BYA_NO_3_2_4,
                   BYAFILE.BYA_AMT_3_2_4,
                   BYAFILE.BYA_O2_3_2_4,
                   BYAFILE.BYA_INT_3_2_4,
                   BYAFILE.BYA_NO_3_2_5,
                   BYAFILE.BYA_AMT_3_2_5,
                   BYAFILE.BYA_O2_3_2_5,
                   BYAFILE.BYA_INT_3_2_5,
                   BYAFILE.BYA_NO_3_2_6,
                   BYAFILE.BYA_AMT_3_2_6,
                   BYAFILE.BYA_O2_3_2_6,
                   BYAFILE.BYA_INT_3_2_6,
                   BYAFILE.BYA_NO_3_2_7,
                   BYAFILE.BYA_AMT_3_2_7,
                   BYAFILE.BYA_O2_3_2_7,
                   BYAFILE.BYA_INT_3_2_7,
                   BYAFILE.BYA_NO_3_2_8,
                   BYAFILE.BYA_AMT_3_2_8,
                   BYAFILE.BYA_O2_3_2_8,
                   BYAFILE.BYA_INT_3_2_8,
                   BYAFILE.BYA_NO_3_2_9,
                   BYAFILE.BYA_AMT_3_2_9,
                   BYAFILE.BYA_O2_3_2_9,
                   BYAFILE.BYA_INT_3_2_9,
                   BYAFILE.BYA_NO_3_2_10,
                   BYAFILE.BYA_AMT_3_2_10,
                   BYAFILE.BYA_O2_3_2_10,
                   BYAFILE.BYA_INT_3_2_10,
                   BYAFILE.BYA_NO_3_2_11,
                   BYAFILE.BYA_AMT_3_2_11,
                   BYAFILE.BYA_O2_3_2_11,
                   BYAFILE.BYA_INT_3_2_11,
                   BYAFILE.BYA_NO_REPO_1_1_1,
                   BYAFILE.BYA_AMT_REPO_1_1_1,
                   BYAFILE.BYA_O2_REPO_1_1_1,
                   BYAFILE.BYA_INT_REPO_1_1_1,
                   BYAFILE.BYA_NO_REPO_1_1_2,
                   BYAFILE.BYA_AMT_REPO_1_1_2,
                   BYAFILE.BYA_O2_REPO_1_1_2,
                   BYAFILE.BYA_INT_REPO_1_1_2,
                   BYAFILE.BYA_NO_REPO_1_1_3,
                   BYAFILE.BYA_AMT_REPO_1_1_3,
                   BYAFILE.BYA_O2_REPO_1_1_3,
                   BYAFILE.BYA_INT_REPO_1_1_3,
                   BYAFILE.BYA_NO_REPO_1_1_4,
                   BYAFILE.BYA_AMT_REPO_1_1_4,
                   BYAFILE.BYA_O2_REPO_1_1_4,
                   BYAFILE.BYA_INT_REPO_1_1_4,
                   BYAFILE.BYA_NO_REPO_1_1_5,
                   BYAFILE.BYA_AMT_REPO_1_1_5,
                   BYAFILE.BYA_O2_REPO_1_1_5,
                   BYAFILE.BYA_INT_REPO_1_1_5,
                   BYAFILE.BYA_NO_REPO_1_2_1,
                   BYAFILE.BYA_AMT_REPO_1_2_1,
                   BYAFILE.BYA_O2_REPO_1_2_1,
                   BYAFILE.BYA_INT_REPO_1_2_1,
                   BYAFILE.BYA_NO_REPO_1_2_2,
                   BYAFILE.BYA_AMT_REPO_1_2_2,
                   BYAFILE.BYA_O2_REPO_1_2_2,
                   BYAFILE.BYA_INT_REPO_1_2_2,
                   BYAFILE.BYA_NO_REPO_1_2_3,
                   BYAFILE.BYA_AMT_REPO_1_2_3,
                   BYAFILE.BYA_O2_REPO_1_2_3,
                   BYAFILE.BYA_INT_REPO_1_2_3,
                   BYAFILE.BYA_NO_REPO_1_2_4,
                   BYAFILE.BYA_AMT_REPO_1_2_4,
                   BYAFILE.BYA_O2_REPO_1_2_4,
                   BYAFILE.BYA_INT_REPO_1_2_4,
                   BYAFILE.BYA_NO_REPO_1_2_5,
                   BYAFILE.BYA_AMT_REPO_1_2_5,
                   BYAFILE.BYA_O2_REPO_1_2_5,
                   BYAFILE.BYA_INT_REPO_1_2_5,
                   BYAFILE.BYA_NO_REPO_2_1_1,
                   BYAFILE.BYA_AMT_REPO_2_1_1,
                   BYAFILE.BYA_O2_REPO_2_1_1,
                   BYAFILE.BYA_INT_REPO_2_1_1,
                   BYAFILE.BYA_NO_REPO_2_1_2,
                   BYAFILE.BYA_AMT_REPO_2_1_2,
                   BYAFILE.BYA_O2_REPO_2_1_2,
                   BYAFILE.BYA_INT_REPO_2_1_2,
                   BYAFILE.BYA_NO_REPO_2_1_3,
                   BYAFILE.BYA_AMT_REPO_2_1_3,
                   BYAFILE.BYA_O2_REPO_2_1_3,
                   BYAFILE.BYA_INT_REPO_2_1_3,
                   BYAFILE.BYA_NO_REPO_2_1_4,
                   BYAFILE.BYA_AMT_REPO_2_1_4,
                   BYAFILE.BYA_O2_REPO_2_1_4,
                   BYAFILE.BYA_INT_REPO_2_1_4,
                   BYAFILE.BYA_NO_REPO_2_1_5,
                   BYAFILE.BYA_AMT_REPO_2_1_5,
                   BYAFILE.BYA_O2_REPO_2_1_5,
                   BYAFILE.BYA_INT_REPO_2_1_5,
                   BYAFILE.BYA_NO_REPO_2_2_1,
                   BYAFILE.BYA_AMT_REPO_2_2_1,
                   BYAFILE.BYA_O2_REPO_2_2_1,
                   BYAFILE.BYA_INT_REPO_2_2_1,
                   BYAFILE.BYA_NO_REPO_2_2_2,
                   BYAFILE.BYA_AMT_REPO_2_2_2,
                   BYAFILE.BYA_O2_REPO_2_2_2,
                   BYAFILE.BYA_INT_REPO_2_2_2,
                   BYAFILE.BYA_NO_REPO_2_2_3,
                   BYAFILE.BYA_AMT_REPO_2_2_3,
                   BYAFILE.BYA_O2_REPO_2_2_3,
                   BYAFILE.BYA_INT_REPO_2_2_3,
                   BYAFILE.BYA_NO_REPO_2_2_4,
                   BYAFILE.BYA_AMT_REPO_2_2_4,
                   BYAFILE.BYA_O2_REPO_2_2_4,
                   BYAFILE.BYA_INT_REPO_2_2_4,
                   BYAFILE.BYA_NO_REPO_2_2_5,
                   BYAFILE.BYA_AMT_REPO_2_2_5,
                   BYAFILE.BYA_O2_REPO_2_2_5,
                   BYAFILE.BYA_INT_REPO_2_2_5,
                   BYAFILE.BYA_NO_REPO_3_1_1,
                   BYAFILE.BYA_AMT_REPO_3_1_1,
                   BYAFILE.BYA_O2_REPO_3_1_1,
                   BYAFILE.BYA_INT_REPO_3_1_1,
                   BYAFILE.BYA_NO_REPO_3_1_2,
                   BYAFILE.BYA_AMT_REPO_3_1_2,
                   BYAFILE.BYA_O2_REPO_3_1_2,
                   BYAFILE.BYA_INT_REPO_3_1_2,
                   BYAFILE.BYA_NO_REPO_3_1_3,
                   BYAFILE.BYA_AMT_REPO_3_1_3,
                   BYAFILE.BYA_O2_REPO_3_1_3,
                   BYAFILE.BYA_INT_REPO_3_1_3,
                   BYAFILE.BYA_NO_REPO_3_1_4,
                   BYAFILE.BYA_AMT_REPO_3_1_4,
                   BYAFILE.BYA_O2_REPO_3_1_4,
                   BYAFILE.BYA_INT_REPO_3_1_4,
                   BYAFILE.BYA_NO_REPO_3_1_5,
                   BYAFILE.BYA_AMT_REPO_3_1_5,
                   BYAFILE.BYA_O2_REPO_3_1_5,
                   BYAFILE.BYA_INT_REPO_3_1_5,
                   BYAFILE.BYA_NO_REPO_3_2_1,
                   BYAFILE.BYA_AMT_REPO_3_2_1,
                   BYAFILE.BYA_O2_REPO_3_2_1,
                   BYAFILE.BYA_INT_REPO_3_2_1,
                   BYAFILE.BYA_NO_REPO_3_2_2,
                   BYAFILE.BYA_AMT_REPO_3_2_2,
                   BYAFILE.BYA_O2_REPO_3_2_2,
                   BYAFILE.BYA_INT_REPO_3_2_2,
                   BYAFILE.BYA_NO_REPO_3_2_3,
                   BYAFILE.BYA_AMT_REPO_3_2_3,
                   BYAFILE.BYA_O2_REPO_3_2_3,
                   BYAFILE.BYA_INT_REPO_3_2_3,
                   BYAFILE.BYA_NO_REPO_3_2_4,
                   BYAFILE.BYA_AMT_REPO_3_2_4,
                   BYAFILE.BYA_O2_REPO_3_2_4,
                   BYAFILE.BYA_INT_REPO_3_2_4,
                   BYAFILE.BYA_NO_REPO_3_2_5,
                   BYAFILE.BYA_AMT_REPO_3_2_5,
                   BYAFILE.BYA_O2_REPO_3_2_5,
                   BYAFILE.BYA_INT_REPO_3_2_5,
                   BYAFILE.BYA_NO_PL_1_1_1,
                   BYAFILE.BYA_AMT_PL_1_1_1,
                   BYAFILE.BYA_O2_PL_1_1_1,
                   BYAFILE.BYA_INT_PL_1_1_1,
                   BYAFILE.BYA_NO_PL_1_1_2,
                   BYAFILE.BYA_AMT_PL_1_1_2,
                   BYAFILE.BYA_O2_PL_1_1_2,
                   BYAFILE.BYA_INT_PL_1_1_2,
                   BYAFILE.BYA_NO_PL_1_1_3,
                   BYAFILE.BYA_AMT_PL_1_1_3,
                   BYAFILE.BYA_O2_PL_1_1_3,
                   BYAFILE.BYA_INT_PL_1_1_3,
                   BYAFILE.BYA_NO_PL_1_1_4,
                   BYAFILE.BYA_AMT_PL_1_1_4,
                   BYAFILE.BYA_O2_PL_1_1_4,
                   BYAFILE.BYA_INT_PL_1_1_4,
                   BYAFILE.BYA_NO_PL_1_1_5,
                   BYAFILE.BYA_AMT_PL_1_1_5,
                   BYAFILE.BYA_O2_PL_1_1_5,
                   BYAFILE.BYA_INT_PL_1_1_5,
                   BYAFILE.BYA_NO_PL_1_1_6,
                   BYAFILE.BYA_AMT_PL_1_1_6,
                   BYAFILE.BYA_O2_PL_1_1_6,
                   BYAFILE.BYA_INT_PL_1_1_6,
                   BYAFILE.BYA_NO_PL_1_1_7,
                   BYAFILE.BYA_AMT_PL_1_1_7,
                   BYAFILE.BYA_O2_PL_1_1_7,
                   BYAFILE.BYA_INT_PL_1_1_7,
                   BYAFILE.BYA_NO_PL_1_1_8,
                   BYAFILE.BYA_AMT_PL_1_1_8,
                   BYAFILE.BYA_O2_PL_1_1_8,
                   BYAFILE.BYA_INT_PL_1_1_8,
                   BYAFILE.BYA_NO_PL_1_1_9,
                   BYAFILE.BYA_AMT_PL_1_1_9,
                   BYAFILE.BYA_O2_PL_1_1_9,
                   BYAFILE.BYA_INT_PL_1_1_9,
                   BYAFILE.BYA_NO_PL_1_1_10,
                   BYAFILE.BYA_AMT_PL_1_1_10,
                   BYAFILE.BYA_O2_PL_1_1_10,
                   BYAFILE.BYA_INT_PL_1_1_10,
                   BYAFILE.BYA_NO_PL_1_2_1,
                   BYAFILE.BYA_AMT_PL_1_2_1,
                   BYAFILE.BYA_O2_PL_1_2_1,
                   BYAFILE.BYA_INT_PL_1_2_1,
                   BYAFILE.BYA_NO_PL_1_2_2,
                   BYAFILE.BYA_AMT_PL_1_2_2,
                   BYAFILE.BYA_O2_PL_1_2_2,
                   BYAFILE.BYA_INT_PL_1_2_2,
                   BYAFILE.BYA_NO_PL_1_2_3,
                   BYAFILE.BYA_AMT_PL_1_2_3,
                   BYAFILE.BYA_O2_PL_1_2_3,
                   BYAFILE.BYA_INT_PL_1_2_3,
                   BYAFILE.BYA_NO_PL_1_2_4,
                   BYAFILE.BYA_AMT_PL_1_2_4,
                   BYAFILE.BYA_O2_PL_1_2_4,
                   BYAFILE.BYA_INT_PL_1_2_4,
                   BYAFILE.BYA_NO_PL_1_2_5,
                   BYAFILE.BYA_AMT_PL_1_2_5,
                   BYAFILE.BYA_O2_PL_1_2_5,
                   BYAFILE.BYA_INT_PL_1_2_5,
                   BYAFILE.BYA_NO_PL_1_2_6,
                   BYAFILE.BYA_AMT_PL_1_2_6,
                   BYAFILE.BYA_O2_PL_1_2_6,
                   BYAFILE.BYA_INT_PL_1_2_6,
                   BYAFILE.BYA_NO_PL_1_2_7,
                   BYAFILE.BYA_AMT_PL_1_2_7,
                   BYAFILE.BYA_O2_PL_1_2_7,
                   BYAFILE.BYA_INT_PL_1_2_7,
                   BYAFILE.BYA_NO_PL_1_2_8,
                   BYAFILE.BYA_AMT_PL_1_2_8,
                   BYAFILE.BYA_O2_PL_1_2_8,
                   BYAFILE.BYA_INT_PL_1_2_8,
                   BYAFILE.BYA_NO_PL_1_2_9,
                   BYAFILE.BYA_AMT_PL_1_2_9,
                   BYAFILE.BYA_O2_PL_1_2_9,
                   BYAFILE.BYA_INT_PL_1_2_9,
                   BYAFILE.BYA_NO_PL_1_2_10,
                   BYAFILE.BYA_AMT_PL_1_2_10,
                   BYAFILE.BYA_O2_PL_1_2_10,
                   BYAFILE.BYA_INT_PL_1_2_10,
                   BYAFILE.BYA_NO_PL_2_1_1,
                   BYAFILE.BYA_AMT_PL_2_1_1,
                   BYAFILE.BYA_O2_PL_2_1_1,
                   BYAFILE.BYA_INT_PL_2_1_1,
                   BYAFILE.BYA_NO_PL_2_1_2,
                   BYAFILE.BYA_AMT_PL_2_1_2,
                   BYAFILE.BYA_O2_PL_2_1_2,
                   BYAFILE.BYA_INT_PL_2_1_2,
                   BYAFILE.BYA_NO_PL_2_1_3,
                   BYAFILE.BYA_AMT_PL_2_1_3,
                   BYAFILE.BYA_O2_PL_2_1_3,
                   BYAFILE.BYA_INT_PL_2_1_3,
                   BYAFILE.BYA_NO_PL_2_1_4,
                   BYAFILE.BYA_AMT_PL_2_1_4,
                   BYAFILE.BYA_O2_PL_2_1_4,
                   BYAFILE.BYA_INT_PL_2_1_4,
                   BYAFILE.BYA_NO_PL_2_1_5,
                   BYAFILE.BYA_AMT_PL_2_1_5,
                   BYAFILE.BYA_O2_PL_2_1_5,
                   BYAFILE.BYA_INT_PL_2_1_5,
                   BYAFILE.BYA_NO_PL_2_1_6,
                   BYAFILE.BYA_AMT_PL_2_1_6,
                   BYAFILE.BYA_O2_PL_2_1_6,
                   BYAFILE.BYA_INT_PL_2_1_6,
                   BYAFILE.BYA_NO_PL_2_1_7,
                   BYAFILE.BYA_AMT_PL_2_1_7,
                   BYAFILE.BYA_O2_PL_2_1_7,
                   BYAFILE.BYA_INT_PL_2_1_7,
                   BYAFILE.BYA_NO_PL_2_1_8,
                   BYAFILE.BYA_AMT_PL_2_1_8,
                   BYAFILE.BYA_O2_PL_2_1_8,
                   BYAFILE.BYA_INT_PL_2_1_8,
                   BYAFILE.BYA_NO_PL_2_1_9,
                   BYAFILE.BYA_AMT_PL_2_1_9,
                   BYAFILE.BYA_O2_PL_2_1_9,
                   BYAFILE.BYA_INT_PL_2_1_9,
                   BYAFILE.BYA_NO_PL_2_1_10,
                   BYAFILE.BYA_AMT_PL_2_1_10,
                   BYAFILE.BYA_O2_PL_2_1_10,
                   BYAFILE.BYA_INT_PL_2_1_10,
                   BYAFILE.BYA_NO_PL_2_2_1,
                   BYAFILE.BYA_AMT_PL_2_2_1,
                   BYAFILE.BYA_O2_PL_2_2_1,
                   BYAFILE.BYA_INT_PL_2_2_1,
                   BYAFILE.BYA_NO_PL_2_2_2,
                   BYAFILE.BYA_AMT_PL_2_2_2,
                   BYAFILE.BYA_O2_PL_2_2_2,
                   BYAFILE.BYA_INT_PL_2_2_2,
                   BYAFILE.BYA_NO_PL_2_2_3,
                   BYAFILE.BYA_AMT_PL_2_2_3,
                   BYAFILE.BYA_O2_PL_2_2_3,
                   BYAFILE.BYA_INT_PL_2_2_3,
                   BYAFILE.BYA_NO_PL_2_2_4,
                   BYAFILE.BYA_AMT_PL_2_2_4,
                   BYAFILE.BYA_O2_PL_2_2_4,
                   BYAFILE.BYA_INT_PL_2_2_4,
                   BYAFILE.BYA_NO_PL_2_2_5,
                   BYAFILE.BYA_AMT_PL_2_2_5,
                   BYAFILE.BYA_O2_PL_2_2_5,
                   BYAFILE.BYA_INT_PL_2_2_5,
                   BYAFILE.BYA_NO_PL_2_2_6,
                   BYAFILE.BYA_AMT_PL_2_2_6,
                   BYAFILE.BYA_O2_PL_2_2_6,
                   BYAFILE.BYA_INT_PL_2_2_6,
                   BYAFILE.BYA_NO_PL_2_2_7,
                   BYAFILE.BYA_AMT_PL_2_2_7,
                   BYAFILE.BYA_O2_PL_2_2_7,
                   BYAFILE.BYA_INT_PL_2_2_7,
                   BYAFILE.BYA_NO_PL_2_2_8,
                   BYAFILE.BYA_AMT_PL_2_2_8,
                   BYAFILE.BYA_O2_PL_2_2_8,
                   BYAFILE.BYA_INT_PL_2_2_8,
                   BYAFILE.BYA_NO_PL_2_2_9,
                   BYAFILE.BYA_AMT_PL_2_2_9,
                   BYAFILE.BYA_O2_PL_2_2_9,
                   BYAFILE.BYA_INT_PL_2_2_9,
                   BYAFILE.BYA_NO_PL_2_2_10,
                   BYAFILE.BYA_AMT_PL_2_2_10,
                   BYAFILE.BYA_O2_PL_2_2_10,
                   BYAFILE.BYA_INT_PL_2_2_10,
                   BYAFILE.BYA_NO_PL_3_1_1,
                   BYAFILE.BYA_AMT_PL_3_1_1,
                   BYAFILE.BYA_O2_PL_3_1_1,
                   BYAFILE.BYA_INT_PL_3_1_1,
                   BYAFILE.BYA_NO_PL_3_1_2,
                   BYAFILE.BYA_AMT_PL_3_1_2,
                   BYAFILE.BYA_O2_PL_3_1_2,
                   BYAFILE.BYA_INT_PL_3_1_2,
                   BYAFILE.BYA_NO_PL_3_1_3,
                   BYAFILE.BYA_AMT_PL_3_1_3,
                   BYAFILE.BYA_O2_PL_3_1_3,
                   BYAFILE.BYA_INT_PL_3_1_3,
                   BYAFILE.BYA_NO_PL_3_1_4,
                   BYAFILE.BYA_AMT_PL_3_1_4,
                   BYAFILE.BYA_O2_PL_3_1_4,
                   BYAFILE.BYA_INT_PL_3_1_4,
                   BYAFILE.BYA_NO_PL_3_1_5,
                   BYAFILE.BYA_AMT_PL_3_1_5,
                   BYAFILE.BYA_O2_PL_3_1_5,
                   BYAFILE.BYA_INT_PL_3_1_5,
                   BYAFILE.BYA_NO_PL_3_1_6,
                   BYAFILE.BYA_AMT_PL_3_1_6,
                   BYAFILE.BYA_O2_PL_3_1_6,
                   BYAFILE.BYA_INT_PL_3_1_6,
                   BYAFILE.BYA_NO_PL_3_1_7,
                   BYAFILE.BYA_AMT_PL_3_1_7,
                   BYAFILE.BYA_O2_PL_3_1_7,
                   BYAFILE.BYA_INT_PL_3_1_7,
                   BYAFILE.BYA_NO_PL_3_1_8,
                   BYAFILE.BYA_AMT_PL_3_1_8,
                   BYAFILE.BYA_O2_PL_3_1_8,
                   BYAFILE.BYA_INT_PL_3_1_8,
                   BYAFILE.BYA_NO_PL_3_1_9,
                   BYAFILE.BYA_AMT_PL_3_1_9,
                   BYAFILE.BYA_O2_PL_3_1_9,
                   BYAFILE.BYA_INT_PL_3_1_9,
                   BYAFILE.BYA_NO_PL_3_1_10,
                   BYAFILE.BYA_AMT_PL_3_1_10,
                   BYAFILE.BYA_O2_PL_3_1_10,
                   BYAFILE.BYA_INT_PL_3_1_10,
                   BYAFILE.BYA_NO_PL_3_2_1,
                   BYAFILE.BYA_AMT_PL_3_2_1,
                   BYAFILE.BYA_O2_PL_3_2_1,
                   BYAFILE.BYA_INT_PL_3_2_1,
                   BYAFILE.BYA_NO_PL_3_2_2,
                   BYAFILE.BYA_AMT_PL_3_2_2,
                   BYAFILE.BYA_O2_PL_3_2_2,
                   BYAFILE.BYA_INT_PL_3_2_2,
                   BYAFILE.BYA_NO_PL_3_2_3,
                   BYAFILE.BYA_AMT_PL_3_2_3,
                   BYAFILE.BYA_O2_PL_3_2_3,
                   BYAFILE.BYA_INT_PL_3_2_3,
                   BYAFILE.BYA_NO_PL_3_2_4,
                   BYAFILE.BYA_AMT_PL_3_2_4,
                   BYAFILE.BYA_O2_PL_3_2_4,
                   BYAFILE.BYA_INT_PL_3_2_4,
                   BYAFILE.BYA_NO_PL_3_2_5,
                   BYAFILE.BYA_AMT_PL_3_2_5,
                   BYAFILE.BYA_O2_PL_3_2_5,
                   BYAFILE.BYA_INT_PL_3_2_5,
                   BYAFILE.BYA_NO_PL_3_2_6,
                   BYAFILE.BYA_AMT_PL_3_2_6,
                   BYAFILE.BYA_O2_PL_3_2_6,
                   BYAFILE.BYA_INT_PL_3_2_6,
                   BYAFILE.BYA_NO_PL_3_2_7,
                   BYAFILE.BYA_AMT_PL_3_2_7,
                   BYAFILE.BYA_O2_PL_3_2_7,
                   BYAFILE.BYA_INT_PL_3_2_7,
                   BYAFILE.BYA_NO_PL_3_2_8,
                   BYAFILE.BYA_AMT_PL_3_2_8,
                   BYAFILE.BYA_O2_PL_3_2_8,
                   BYAFILE.BYA_INT_PL_3_2_8,
                   BYAFILE.BYA_NO_PL_3_2_9,
                   BYAFILE.BYA_AMT_PL_3_2_9,
                   BYAFILE.BYA_O2_PL_3_2_9,
                   BYAFILE.BYA_INT_PL_3_2_9,
                   BYAFILE.BYA_NO_PL_3_2_10,
                   BYAFILE.BYA_AMT_PL_3_2_10,
                   BYAFILE.BYA_O2_PL_3_2_10,
                   BYAFILE.BYA_INT_PL_3_2_10,
                   BYAFILE.BYA_DLRVPRIN_1_1_1,
                   BYAFILE.BYA_DLRVPRIN_1_1_2,
                   BYAFILE.BYA_DLRVPRIN_1_1_3,
                   BYAFILE.BYA_DLRVCHGS_1_1_1,
                   BYAFILE.BYA_DLRVCHGS_1_1_2,
                   BYAFILE.BYA_DLRVCHGS_1_1_3,
                   BYAFILE.BYA_DLPARVHELD_1_1,
                   BYAFILE.BYA_DLRVPRIN_1_2_1,
                   BYAFILE.BYA_DLRVPRIN_1_2_2,
                   BYAFILE.BYA_DLRVPRIN_1_2_3,
                   BYAFILE.BYA_DLRVCHGS_1_2_1,
                   BYAFILE.BYA_DLRVCHGS_1_2_2,
                   BYAFILE.BYA_DLRVCHGS_1_2_3,
                   BYAFILE.BYA_DLPARVHELD_1_2,
                   BYAFILE.BYA_DLRVPRIN_2_1_1,
                   BYAFILE.BYA_DLRVPRIN_2_1_2,
                   BYAFILE.BYA_DLRVPRIN_2_1_3,
                   BYAFILE.BYA_DLRVCHGS_2_1_1,
                   BYAFILE.BYA_DLRVCHGS_2_1_2,
                   BYAFILE.BYA_DLRVCHGS_2_1_3,
                   BYAFILE.BYA_DLPARVHELD_2_1,
                   BYAFILE.BYA_DLRVPRIN_2_2_1,
                   BYAFILE.BYA_DLRVPRIN_2_2_2,
                   BYAFILE.BYA_DLRVPRIN_2_2_3,
                   BYAFILE.BYA_DLRVCHGS_2_2_1,
                   BYAFILE.BYA_DLRVCHGS_2_2_2,
                   BYAFILE.BYA_DLRVCHGS_2_2_3,
                   BYAFILE.BYA_DLPARVHELD_2_2,
                   BYAFILE.BYA_DLRVPRIN_3_1_1,
                   BYAFILE.BYA_DLRVPRIN_3_1_2,
                   BYAFILE.BYA_DLRVPRIN_3_1_3,
                   BYAFILE.BYA_DLRVCHGS_3_1_1,
                   BYAFILE.BYA_DLRVCHGS_3_1_2,
                   BYAFILE.BYA_DLRVCHGS_3_1_3,
                   BYAFILE.BYA_DLPARVHELD_3_1,
                   BYAFILE.BYA_DLRVPRIN_3_2_1,
                   BYAFILE.BYA_DLRVPRIN_3_2_2,
                   BYAFILE.BYA_DLRVPRIN_3_2_3,
                   BYAFILE.BYA_DLRVCHGS_3_2_1,
                   BYAFILE.BYA_DLRVCHGS_3_2_2,
                   BYAFILE.BYA_DLRVCHGS_3_2_3,
                   BYAFILE.BYA_DLPARVHELD_3_2
            INTO :QBYA-REC
            FROM DBO.BYAFILE
            WHERE BYAFILE.BYA_DATE_CCYYMM = :QBYA-DATE-CCYYMM
              AND BYAFILE.BYA_BRNO = :QBYA-BRNO 
              AND BYAFILE.BYA_CLASS = :QBYA-CLASS
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-BYA1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-BYA-FIELDS.

      *-----------------------------------------------------------------
       READ-BYA1-FILE-NEXT. *> EMBEDDED - BYAFILE
           PERFORM READ-IT.
           MOVE BYA-PATH-SQL TO E-FILE.
           MOVE BYA1-KEY TO E-KEYX.
           INITIALIZE QBYA-REC.

           IF ( BYA1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH BYA1_CURSOR INTO :QBYA-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-BYA1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-BYA-FIELDS.

      *-----------------------------------------------------------------
       WRITE-BYA1-FILE.

           ACCEPT BYA-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE BYA-PATH-SQL TO E-FILE.
           MOVE BYA1-KEY TO E-KEYX.
           PERFORM SET-BYA-FIELDS.

           MOVE '2' TO SQL-UPDATE-CONN-USE-SW. *> USE UPDATE_CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

           EXEC SQL
            INSERT INTO DBO.BYAFILE
             (BYAFILE.BYA_DATE_CCYYMM,
              BYAFILE.BYA_BRNO,
              BYAFILE.BYA_CLASS,
              BYAFILE.BYA_LTOUCH_DATE,
              BYAFILE.BYA_NO_1_1_1,
              BYAFILE.BYA_AMT_1_1_1,
              BYAFILE.BYA_O2_1_1_1,
              BYAFILE.BYA_INT_1_1_1,
              BYAFILE.BYA_NO_1_1_2,
              BYAFILE.BYA_AMT_1_1_2,
              BYAFILE.BYA_O2_1_1_2,
              BYAFILE.BYA_INT_1_1_2,
              BYAFILE.BYA_NO_1_1_3,
              BYAFILE.BYA_AMT_1_1_3,
              BYAFILE.BYA_O2_1_1_3,
              BYAFILE.BYA_INT_1_1_3,
              BYAFILE.BYA_NO_1_1_4,
              BYAFILE.BYA_AMT_1_1_4,
              BYAFILE.BYA_O2_1_1_4,
              BYAFILE.BYA_INT_1_1_4,
              BYAFILE.BYA_NO_1_1_5,
              BYAFILE.BYA_AMT_1_1_5,
              BYAFILE.BYA_O2_1_1_5,
              BYAFILE.BYA_INT_1_1_5,
              BYAFILE.BYA_NO_1_1_6,
              BYAFILE.BYA_AMT_1_1_6,
              BYAFILE.BYA_O2_1_1_6,
              BYAFILE.BYA_INT_1_1_6,
              BYAFILE.BYA_NO_1_1_7,
              BYAFILE.BYA_AMT_1_1_7,
              BYAFILE.BYA_O2_1_1_7,
              BYAFILE.BYA_INT_1_1_7,
              BYAFILE.BYA_NO_1_1_8,
              BYAFILE.BYA_AMT_1_1_8,
              BYAFILE.BYA_O2_1_1_8,
              BYAFILE.BYA_INT_1_1_8,
              BYAFILE.BYA_NO_1_1_9,
              BYAFILE.BYA_AMT_1_1_9,
              BYAFILE.BYA_O2_1_1_9,
              BYAFILE.BYA_INT_1_1_9,
              BYAFILE.BYA_NO_1_1_10,
              BYAFILE.BYA_AMT_1_1_10,
              BYAFILE.BYA_O2_1_1_10,
              BYAFILE.BYA_INT_1_1_10,
              BYAFILE.BYA_NO_1_1_11,
              BYAFILE.BYA_AMT_1_1_11,
              BYAFILE.BYA_O2_1_1_11,
              BYAFILE.BYA_INT_1_1_11,
              BYAFILE.BYA_NO_1_2_1,
              BYAFILE.BYA_AMT_1_2_1,
              BYAFILE.BYA_O2_1_2_1,
              BYAFILE.BYA_INT_1_2_1,
              BYAFILE.BYA_NO_1_2_2,
              BYAFILE.BYA_AMT_1_2_2,
              BYAFILE.BYA_O2_1_2_2,
              BYAFILE.BYA_INT_1_2_2,
              BYAFILE.BYA_NO_1_2_3,
              BYAFILE.BYA_AMT_1_2_3,
              BYAFILE.BYA_O2_1_2_3,
              BYAFILE.BYA_INT_1_2_3,
              BYAFILE.BYA_NO_1_2_4,
              BYAFILE.BYA_AMT_1_2_4,
              BYAFILE.BYA_O2_1_2_4,
              BYAFILE.BYA_INT_1_2_4,
              BYAFILE.BYA_NO_1_2_5,
              BYAFILE.BYA_AMT_1_2_5,
              BYAFILE.BYA_O2_1_2_5,
              BYAFILE.BYA_INT_1_2_5,
              BYAFILE.BYA_NO_1_2_6,
              BYAFILE.BYA_AMT_1_2_6,
              BYAFILE.BYA_O2_1_2_6,
              BYAFILE.BYA_INT_1_2_6,
              BYAFILE.BYA_NO_1_2_7,
              BYAFILE.BYA_AMT_1_2_7,
              BYAFILE.BYA_O2_1_2_7,
              BYAFILE.BYA_INT_1_2_7,
              BYAFILE.BYA_NO_1_2_8,
              BYAFILE.BYA_AMT_1_2_8,
              BYAFILE.BYA_O2_1_2_8,
              BYAFILE.BYA_INT_1_2_8,
              BYAFILE.BYA_NO_1_2_9,
              BYAFILE.BYA_AMT_1_2_9,
              BYAFILE.BYA_O2_1_2_9,
              BYAFILE.BYA_INT_1_2_9,
              BYAFILE.BYA_NO_1_2_10,
              BYAFILE.BYA_AMT_1_2_10,
              BYAFILE.BYA_O2_1_2_10,
              BYAFILE.BYA_INT_1_2_10,
              BYAFILE.BYA_NO_1_2_11,
              BYAFILE.BYA_AMT_1_2_11,
              BYAFILE.BYA_O2_1_2_11,
              BYAFILE.BYA_INT_1_2_11,
              BYAFILE.BYA_NO_2_1_1,
              BYAFILE.BYA_AMT_2_1_1,
              BYAFILE.BYA_O2_2_1_1,
              BYAFILE.BYA_INT_2_1_1,
              BYAFILE.BYA_NO_2_1_2,
              BYAFILE.BYA_AMT_2_1_2,
              BYAFILE.BYA_O2_2_1_2,
              BYAFILE.BYA_INT_2_1_2,
              BYAFILE.BYA_NO_2_1_3,
              BYAFILE.BYA_AMT_2_1_3,
              BYAFILE.BYA_O2_2_1_3,
              BYAFILE.BYA_INT_2_1_3,
              BYAFILE.BYA_NO_2_1_4,
              BYAFILE.BYA_AMT_2_1_4,
              BYAFILE.BYA_O2_2_1_4,
              BYAFILE.BYA_INT_2_1_4,
              BYAFILE.BYA_NO_2_1_5,
              BYAFILE.BYA_AMT_2_1_5,
              BYAFILE.BYA_O2_2_1_5,
              BYAFILE.BYA_INT_2_1_5,
              BYAFILE.BYA_NO_2_1_6,
              BYAFILE.BYA_AMT_2_1_6,
              BYAFILE.BYA_O2_2_1_6,
              BYAFILE.BYA_INT_2_1_6,
              BYAFILE.BYA_NO_2_1_7,
              BYAFILE.BYA_AMT_2_1_7,
              BYAFILE.BYA_O2_2_1_7,
              BYAFILE.BYA_INT_2_1_7,
              BYAFILE.BYA_NO_2_1_8,
              BYAFILE.BYA_AMT_2_1_8,
              BYAFILE.BYA_O2_2_1_8,
              BYAFILE.BYA_INT_2_1_8,
              BYAFILE.BYA_NO_2_1_9,
              BYAFILE.BYA_AMT_2_1_9,
              BYAFILE.BYA_O2_2_1_9,
              BYAFILE.BYA_INT_2_1_9,
              BYAFILE.BYA_NO_2_1_10,
              BYAFILE.BYA_AMT_2_1_10,
              BYAFILE.BYA_O2_2_1_10,
              BYAFILE.BYA_INT_2_1_10,
              BYAFILE.BYA_NO_2_1_11,
              BYAFILE.BYA_AMT_2_1_11,
              BYAFILE.BYA_O2_2_1_11,
              BYAFILE.BYA_INT_2_1_11,
              BYAFILE.BYA_NO_2_2_1,
              BYAFILE.BYA_AMT_2_2_1,
              BYAFILE.BYA_O2_2_2_1,
              BYAFILE.BYA_INT_2_2_1,
              BYAFILE.BYA_NO_2_2_2,
              BYAFILE.BYA_AMT_2_2_2,
              BYAFILE.BYA_O2_2_2_2,
              BYAFILE.BYA_INT_2_2_2,
              BYAFILE.BYA_NO_2_2_3,
              BYAFILE.BYA_AMT_2_2_3,
              BYAFILE.BYA_O2_2_2_3,
              BYAFILE.BYA_INT_2_2_3,
              BYAFILE.BYA_NO_2_2_4,
              BYAFILE.BYA_AMT_2_2_4,
              BYAFILE.BYA_O2_2_2_4,
              BYAFILE.BYA_INT_2_2_4,
              BYAFILE.BYA_NO_2_2_5,
              BYAFILE.BYA_AMT_2_2_5,
              BYAFILE.BYA_O2_2_2_5,
              BYAFILE.BYA_INT_2_2_5,
              BYAFILE.BYA_NO_2_2_6,
              BYAFILE.BYA_AMT_2_2_6,
              BYAFILE.BYA_O2_2_2_6,
              BYAFILE.BYA_INT_2_2_6,
              BYAFILE.BYA_NO_2_2_7,
              BYAFILE.BYA_AMT_2_2_7,
              BYAFILE.BYA_O2_2_2_7,
              BYAFILE.BYA_INT_2_2_7,
              BYAFILE.BYA_NO_2_2_8,
              BYAFILE.BYA_AMT_2_2_8,
              BYAFILE.BYA_O2_2_2_8,
              BYAFILE.BYA_INT_2_2_8,
              BYAFILE.BYA_NO_2_2_9,
              BYAFILE.BYA_AMT_2_2_9,
              BYAFILE.BYA_O2_2_2_9,
              BYAFILE.BYA_INT_2_2_9,
              BYAFILE.BYA_NO_2_2_10,
              BYAFILE.BYA_AMT_2_2_10,
              BYAFILE.BYA_O2_2_2_10,
              BYAFILE.BYA_INT_2_2_10,
              BYAFILE.BYA_NO_2_2_11,
              BYAFILE.BYA_AMT_2_2_11,
              BYAFILE.BYA_O2_2_2_11,
              BYAFILE.BYA_INT_2_2_11,
              BYAFILE.BYA_NO_3_1_1,
              BYAFILE.BYA_AMT_3_1_1,
              BYAFILE.BYA_O2_3_1_1,
              BYAFILE.BYA_INT_3_1_1,
              BYAFILE.BYA_NO_3_1_2,
              BYAFILE.BYA_AMT_3_1_2,
              BYAFILE.BYA_O2_3_1_2,
              BYAFILE.BYA_INT_3_1_2,
              BYAFILE.BYA_NO_3_1_3,
              BYAFILE.BYA_AMT_3_1_3,
              BYAFILE.BYA_O2_3_1_3,
              BYAFILE.BYA_INT_3_1_3,
              BYAFILE.BYA_NO_3_1_4,
              BYAFILE.BYA_AMT_3_1_4,
              BYAFILE.BYA_O2_3_1_4,
              BYAFILE.BYA_INT_3_1_4,
              BYAFILE.BYA_NO_3_1_5,
              BYAFILE.BYA_AMT_3_1_5,
              BYAFILE.BYA_O2_3_1_5,
              BYAFILE.BYA_INT_3_1_5,
              BYAFILE.BYA_NO_3_1_6,
              BYAFILE.BYA_AMT_3_1_6,
              BYAFILE.BYA_O2_3_1_6,
              BYAFILE.BYA_INT_3_1_6,
              BYAFILE.BYA_NO_3_1_7,
              BYAFILE.BYA_AMT_3_1_7,
              BYAFILE.BYA_O2_3_1_7,
              BYAFILE.BYA_INT_3_1_7,
              BYAFILE.BYA_NO_3_1_8,
              BYAFILE.BYA_AMT_3_1_8,
              BYAFILE.BYA_O2_3_1_8,
              BYAFILE.BYA_INT_3_1_8,
              BYAFILE.BYA_NO_3_1_9,
              BYAFILE.BYA_AMT_3_1_9,
              BYAFILE.BYA_O2_3_1_9,
              BYAFILE.BYA_INT_3_1_9,
              BYAFILE.BYA_NO_3_1_10,
              BYAFILE.BYA_AMT_3_1_10,
              BYAFILE.BYA_O2_3_1_10,
              BYAFILE.BYA_INT_3_1_10,
              BYAFILE.BYA_NO_3_1_11,
              BYAFILE.BYA_AMT_3_1_11,
              BYAFILE.BYA_O2_3_1_11,
              BYAFILE.BYA_INT_3_1_11,
              BYAFILE.BYA_NO_3_2_1,
              BYAFILE.BYA_AMT_3_2_1,
              BYAFILE.BYA_O2_3_2_1,
              BYAFILE.BYA_INT_3_2_1,
              BYAFILE.BYA_NO_3_2_2,
              BYAFILE.BYA_AMT_3_2_2,
              BYAFILE.BYA_O2_3_2_2,
              BYAFILE.BYA_INT_3_2_2,
              BYAFILE.BYA_NO_3_2_3,
              BYAFILE.BYA_AMT_3_2_3,
              BYAFILE.BYA_O2_3_2_3,
              BYAFILE.BYA_INT_3_2_3,
              BYAFILE.BYA_NO_3_2_4,
              BYAFILE.BYA_AMT_3_2_4,
              BYAFILE.BYA_O2_3_2_4,
              BYAFILE.BYA_INT_3_2_4,
              BYAFILE.BYA_NO_3_2_5,
              BYAFILE.BYA_AMT_3_2_5,
              BYAFILE.BYA_O2_3_2_5,
              BYAFILE.BYA_INT_3_2_5,
              BYAFILE.BYA_NO_3_2_6,
              BYAFILE.BYA_AMT_3_2_6,
              BYAFILE.BYA_O2_3_2_6,
              BYAFILE.BYA_INT_3_2_6,
              BYAFILE.BYA_NO_3_2_7,
              BYAFILE.BYA_AMT_3_2_7,
              BYAFILE.BYA_O2_3_2_7,
              BYAFILE.BYA_INT_3_2_7,
              BYAFILE.BYA_NO_3_2_8,
              BYAFILE.BYA_AMT_3_2_8,
              BYAFILE.BYA_O2_3_2_8,
              BYAFILE.BYA_INT_3_2_8,
              BYAFILE.BYA_NO_3_2_9,
              BYAFILE.BYA_AMT_3_2_9,
              BYAFILE.BYA_O2_3_2_9,
              BYAFILE.BYA_INT_3_2_9,
              BYAFILE.BYA_NO_3_2_10,
              BYAFILE.BYA_AMT_3_2_10,
              BYAFILE.BYA_O2_3_2_10,
              BYAFILE.BYA_INT_3_2_10,
              BYAFILE.BYA_NO_3_2_11,
              BYAFILE.BYA_AMT_3_2_11,
              BYAFILE.BYA_O2_3_2_11,
              BYAFILE.BYA_INT_3_2_11,
              BYAFILE.BYA_NO_REPO_1_1_1,
              BYAFILE.BYA_AMT_REPO_1_1_1,
              BYAFILE.BYA_O2_REPO_1_1_1,
              BYAFILE.BYA_INT_REPO_1_1_1,
              BYAFILE.BYA_NO_REPO_1_1_2,
              BYAFILE.BYA_AMT_REPO_1_1_2,
              BYAFILE.BYA_O2_REPO_1_1_2,
              BYAFILE.BYA_INT_REPO_1_1_2,
              BYAFILE.BYA_NO_REPO_1_1_3,
              BYAFILE.BYA_AMT_REPO_1_1_3,
              BYAFILE.BYA_O2_REPO_1_1_3,
              BYAFILE.BYA_INT_REPO_1_1_3,
              BYAFILE.BYA_NO_REPO_1_1_4,
              BYAFILE.BYA_AMT_REPO_1_1_4,
              BYAFILE.BYA_O2_REPO_1_1_4,
              BYAFILE.BYA_INT_REPO_1_1_4,
              BYAFILE.BYA_NO_REPO_1_1_5,
              BYAFILE.BYA_AMT_REPO_1_1_5,
              BYAFILE.BYA_O2_REPO_1_1_5,
              BYAFILE.BYA_INT_REPO_1_1_5,
              BYAFILE.BYA_NO_REPO_1_2_1,
              BYAFILE.BYA_AMT_REPO_1_2_1,
              BYAFILE.BYA_O2_REPO_1_2_1,
              BYAFILE.BYA_INT_REPO_1_2_1,
              BYAFILE.BYA_NO_REPO_1_2_2,
              BYAFILE.BYA_AMT_REPO_1_2_2,
              BYAFILE.BYA_O2_REPO_1_2_2,
              BYAFILE.BYA_INT_REPO_1_2_2,
              BYAFILE.BYA_NO_REPO_1_2_3,
              BYAFILE.BYA_AMT_REPO_1_2_3,
              BYAFILE.BYA_O2_REPO_1_2_3,
              BYAFILE.BYA_INT_REPO_1_2_3,
              BYAFILE.BYA_NO_REPO_1_2_4,
              BYAFILE.BYA_AMT_REPO_1_2_4,
              BYAFILE.BYA_O2_REPO_1_2_4,
              BYAFILE.BYA_INT_REPO_1_2_4,
              BYAFILE.BYA_NO_REPO_1_2_5,
              BYAFILE.BYA_AMT_REPO_1_2_5,
              BYAFILE.BYA_O2_REPO_1_2_5,
              BYAFILE.BYA_INT_REPO_1_2_5,
              BYAFILE.BYA_NO_REPO_2_1_1,
              BYAFILE.BYA_AMT_REPO_2_1_1,
              BYAFILE.BYA_O2_REPO_2_1_1,
              BYAFILE.BYA_INT_REPO_2_1_1,
              BYAFILE.BYA_NO_REPO_2_1_2,
              BYAFILE.BYA_AMT_REPO_2_1_2,
              BYAFILE.BYA_O2_REPO_2_1_2,
              BYAFILE.BYA_INT_REPO_2_1_2,
              BYAFILE.BYA_NO_REPO_2_1_3,
              BYAFILE.BYA_AMT_REPO_2_1_3,
              BYAFILE.BYA_O2_REPO_2_1_3,
              BYAFILE.BYA_INT_REPO_2_1_3,
              BYAFILE.BYA_NO_REPO_2_1_4,
              BYAFILE.BYA_AMT_REPO_2_1_4,
              BYAFILE.BYA_O2_REPO_2_1_4,
              BYAFILE.BYA_INT_REPO_2_1_4,
              BYAFILE.BYA_NO_REPO_2_1_5,
              BYAFILE.BYA_AMT_REPO_2_1_5,
              BYAFILE.BYA_O2_REPO_2_1_5,
              BYAFILE.BYA_INT_REPO_2_1_5,
              BYAFILE.BYA_NO_REPO_2_2_1,
              BYAFILE.BYA_AMT_REPO_2_2_1,
              BYAFILE.BYA_O2_REPO_2_2_1,
              BYAFILE.BYA_INT_REPO_2_2_1,
              BYAFILE.BYA_NO_REPO_2_2_2,
              BYAFILE.BYA_AMT_REPO_2_2_2,
              BYAFILE.BYA_O2_REPO_2_2_2,
              BYAFILE.BYA_INT_REPO_2_2_2,
              BYAFILE.BYA_NO_REPO_2_2_3,
              BYAFILE.BYA_AMT_REPO_2_2_3,
              BYAFILE.BYA_O2_REPO_2_2_3,
              BYAFILE.BYA_INT_REPO_2_2_3,
              BYAFILE.BYA_NO_REPO_2_2_4,
              BYAFILE.BYA_AMT_REPO_2_2_4,
              BYAFILE.BYA_O2_REPO_2_2_4,
              BYAFILE.BYA_INT_REPO_2_2_4,
              BYAFILE.BYA_NO_REPO_2_2_5,
              BYAFILE.BYA_AMT_REPO_2_2_5,
              BYAFILE.BYA_O2_REPO_2_2_5,
              BYAFILE.BYA_INT_REPO_2_2_5,
              BYAFILE.BYA_NO_REPO_3_1_1,
              BYAFILE.BYA_AMT_REPO_3_1_1,
              BYAFILE.BYA_O2_REPO_3_1_1,
              BYAFILE.BYA_INT_REPO_3_1_1,
              BYAFILE.BYA_NO_REPO_3_1_2,
              BYAFILE.BYA_AMT_REPO_3_1_2,
              BYAFILE.BYA_O2_REPO_3_1_2,
              BYAFILE.BYA_INT_REPO_3_1_2,
              BYAFILE.BYA_NO_REPO_3_1_3,
              BYAFILE.BYA_AMT_REPO_3_1_3,
              BYAFILE.BYA_O2_REPO_3_1_3,
              BYAFILE.BYA_INT_REPO_3_1_3,
              BYAFILE.BYA_NO_REPO_3_1_4,
              BYAFILE.BYA_AMT_REPO_3_1_4,
              BYAFILE.BYA_O2_REPO_3_1_4,
              BYAFILE.BYA_INT_REPO_3_1_4,
              BYAFILE.BYA_NO_REPO_3_1_5,
              BYAFILE.BYA_AMT_REPO_3_1_5,
              BYAFILE.BYA_O2_REPO_3_1_5,
              BYAFILE.BYA_INT_REPO_3_1_5,
              BYAFILE.BYA_NO_REPO_3_2_1,
              BYAFILE.BYA_AMT_REPO_3_2_1,
              BYAFILE.BYA_O2_REPO_3_2_1,
              BYAFILE.BYA_INT_REPO_3_2_1,
              BYAFILE.BYA_NO_REPO_3_2_2,
              BYAFILE.BYA_AMT_REPO_3_2_2,
              BYAFILE.BYA_O2_REPO_3_2_2,
              BYAFILE.BYA_INT_REPO_3_2_2,
              BYAFILE.BYA_NO_REPO_3_2_3,
              BYAFILE.BYA_AMT_REPO_3_2_3,
              BYAFILE.BYA_O2_REPO_3_2_3,
              BYAFILE.BYA_INT_REPO_3_2_3,
              BYAFILE.BYA_NO_REPO_3_2_4,
              BYAFILE.BYA_AMT_REPO_3_2_4,
              BYAFILE.BYA_O2_REPO_3_2_4,
              BYAFILE.BYA_INT_REPO_3_2_4,
              BYAFILE.BYA_NO_REPO_3_2_5,
              BYAFILE.BYA_AMT_REPO_3_2_5,
              BYAFILE.BYA_O2_REPO_3_2_5,
              BYAFILE.BYA_INT_REPO_3_2_5,
              BYAFILE.BYA_NO_PL_1_1_1,
              BYAFILE.BYA_AMT_PL_1_1_1,
              BYAFILE.BYA_O2_PL_1_1_1,
              BYAFILE.BYA_INT_PL_1_1_1,
              BYAFILE.BYA_NO_PL_1_1_2,
              BYAFILE.BYA_AMT_PL_1_1_2,
              BYAFILE.BYA_O2_PL_1_1_2,
              BYAFILE.BYA_INT_PL_1_1_2,
              BYAFILE.BYA_NO_PL_1_1_3,
              BYAFILE.BYA_AMT_PL_1_1_3,
              BYAFILE.BYA_O2_PL_1_1_3,
              BYAFILE.BYA_INT_PL_1_1_3,
              BYAFILE.BYA_NO_PL_1_1_4,
              BYAFILE.BYA_AMT_PL_1_1_4,
              BYAFILE.BYA_O2_PL_1_1_4,
              BYAFILE.BYA_INT_PL_1_1_4,
              BYAFILE.BYA_NO_PL_1_1_5,
              BYAFILE.BYA_AMT_PL_1_1_5,
              BYAFILE.BYA_O2_PL_1_1_5,
              BYAFILE.BYA_INT_PL_1_1_5,
              BYAFILE.BYA_NO_PL_1_1_6,
              BYAFILE.BYA_AMT_PL_1_1_6,
              BYAFILE.BYA_O2_PL_1_1_6,
              BYAFILE.BYA_INT_PL_1_1_6,
              BYAFILE.BYA_NO_PL_1_1_7,
              BYAFILE.BYA_AMT_PL_1_1_7,
              BYAFILE.BYA_O2_PL_1_1_7,
              BYAFILE.BYA_INT_PL_1_1_7,
              BYAFILE.BYA_NO_PL_1_1_8,
              BYAFILE.BYA_AMT_PL_1_1_8,
              BYAFILE.BYA_O2_PL_1_1_8,
              BYAFILE.BYA_INT_PL_1_1_8,
              BYAFILE.BYA_NO_PL_1_1_9,
              BYAFILE.BYA_AMT_PL_1_1_9,
              BYAFILE.BYA_O2_PL_1_1_9,
              BYAFILE.BYA_INT_PL_1_1_9,
              BYAFILE.BYA_NO_PL_1_1_10,
              BYAFILE.BYA_AMT_PL_1_1_10,
              BYAFILE.BYA_O2_PL_1_1_10,
              BYAFILE.BYA_INT_PL_1_1_10,
              BYAFILE.BYA_NO_PL_1_2_1,
              BYAFILE.BYA_AMT_PL_1_2_1,
              BYAFILE.BYA_O2_PL_1_2_1,
              BYAFILE.BYA_INT_PL_1_2_1,
              BYAFILE.BYA_NO_PL_1_2_2,
              BYAFILE.BYA_AMT_PL_1_2_2,
              BYAFILE.BYA_O2_PL_1_2_2,
              BYAFILE.BYA_INT_PL_1_2_2,
              BYAFILE.BYA_NO_PL_1_2_3,
              BYAFILE.BYA_AMT_PL_1_2_3,
              BYAFILE.BYA_O2_PL_1_2_3,
              BYAFILE.BYA_INT_PL_1_2_3,
              BYAFILE.BYA_NO_PL_1_2_4,
              BYAFILE.BYA_AMT_PL_1_2_4,
              BYAFILE.BYA_O2_PL_1_2_4,
              BYAFILE.BYA_INT_PL_1_2_4,
              BYAFILE.BYA_NO_PL_1_2_5,
              BYAFILE.BYA_AMT_PL_1_2_5,
              BYAFILE.BYA_O2_PL_1_2_5,
              BYAFILE.BYA_INT_PL_1_2_5,
              BYAFILE.BYA_NO_PL_1_2_6,
              BYAFILE.BYA_AMT_PL_1_2_6,
              BYAFILE.BYA_O2_PL_1_2_6,
              BYAFILE.BYA_INT_PL_1_2_6,
              BYAFILE.BYA_NO_PL_1_2_7,
              BYAFILE.BYA_AMT_PL_1_2_7,
              BYAFILE.BYA_O2_PL_1_2_7,
              BYAFILE.BYA_INT_PL_1_2_7,
              BYAFILE.BYA_NO_PL_1_2_8,
              BYAFILE.BYA_AMT_PL_1_2_8,
              BYAFILE.BYA_O2_PL_1_2_8,
              BYAFILE.BYA_INT_PL_1_2_8,
              BYAFILE.BYA_NO_PL_1_2_9,
              BYAFILE.BYA_AMT_PL_1_2_9,
              BYAFILE.BYA_O2_PL_1_2_9,
              BYAFILE.BYA_INT_PL_1_2_9,
              BYAFILE.BYA_NO_PL_1_2_10,
              BYAFILE.BYA_AMT_PL_1_2_10,
              BYAFILE.BYA_O2_PL_1_2_10,
              BYAFILE.BYA_INT_PL_1_2_10,
              BYAFILE.BYA_NO_PL_2_1_1,
              BYAFILE.BYA_AMT_PL_2_1_1,
              BYAFILE.BYA_O2_PL_2_1_1,
              BYAFILE.BYA_INT_PL_2_1_1,
              BYAFILE.BYA_NO_PL_2_1_2,
              BYAFILE.BYA_AMT_PL_2_1_2,
              BYAFILE.BYA_O2_PL_2_1_2,
              BYAFILE.BYA_INT_PL_2_1_2,
              BYAFILE.BYA_NO_PL_2_1_3,
              BYAFILE.BYA_AMT_PL_2_1_3,
              BYAFILE.BYA_O2_PL_2_1_3,
              BYAFILE.BYA_INT_PL_2_1_3,
              BYAFILE.BYA_NO_PL_2_1_4,
              BYAFILE.BYA_AMT_PL_2_1_4,
              BYAFILE.BYA_O2_PL_2_1_4,
              BYAFILE.BYA_INT_PL_2_1_4,
              BYAFILE.BYA_NO_PL_2_1_5,
              BYAFILE.BYA_AMT_PL_2_1_5,
              BYAFILE.BYA_O2_PL_2_1_5,
              BYAFILE.BYA_INT_PL_2_1_5,
              BYAFILE.BYA_NO_PL_2_1_6,
              BYAFILE.BYA_AMT_PL_2_1_6,
              BYAFILE.BYA_O2_PL_2_1_6,
              BYAFILE.BYA_INT_PL_2_1_6,
              BYAFILE.BYA_NO_PL_2_1_7,
              BYAFILE.BYA_AMT_PL_2_1_7,
              BYAFILE.BYA_O2_PL_2_1_7,
              BYAFILE.BYA_INT_PL_2_1_7,
              BYAFILE.BYA_NO_PL_2_1_8,
              BYAFILE.BYA_AMT_PL_2_1_8,
              BYAFILE.BYA_O2_PL_2_1_8,
              BYAFILE.BYA_INT_PL_2_1_8,
              BYAFILE.BYA_NO_PL_2_1_9,
              BYAFILE.BYA_AMT_PL_2_1_9,
              BYAFILE.BYA_O2_PL_2_1_9,
              BYAFILE.BYA_INT_PL_2_1_9,
              BYAFILE.BYA_NO_PL_2_1_10,
              BYAFILE.BYA_AMT_PL_2_1_10,
              BYAFILE.BYA_O2_PL_2_1_10,
              BYAFILE.BYA_INT_PL_2_1_10,
              BYAFILE.BYA_NO_PL_2_2_1,
              BYAFILE.BYA_AMT_PL_2_2_1,
              BYAFILE.BYA_O2_PL_2_2_1,
              BYAFILE.BYA_INT_PL_2_2_1,
              BYAFILE.BYA_NO_PL_2_2_2,
              BYAFILE.BYA_AMT_PL_2_2_2,
              BYAFILE.BYA_O2_PL_2_2_2,
              BYAFILE.BYA_INT_PL_2_2_2,
              BYAFILE.BYA_NO_PL_2_2_3,
              BYAFILE.BYA_AMT_PL_2_2_3,
              BYAFILE.BYA_O2_PL_2_2_3,
              BYAFILE.BYA_INT_PL_2_2_3,
              BYAFILE.BYA_NO_PL_2_2_4,
              BYAFILE.BYA_AMT_PL_2_2_4,
              BYAFILE.BYA_O2_PL_2_2_4,
              BYAFILE.BYA_INT_PL_2_2_4,
              BYAFILE.BYA_NO_PL_2_2_5,
              BYAFILE.BYA_AMT_PL_2_2_5,
              BYAFILE.BYA_O2_PL_2_2_5,
              BYAFILE.BYA_INT_PL_2_2_5,
              BYAFILE.BYA_NO_PL_2_2_6,
              BYAFILE.BYA_AMT_PL_2_2_6,
              BYAFILE.BYA_O2_PL_2_2_6,
              BYAFILE.BYA_INT_PL_2_2_6,
              BYAFILE.BYA_NO_PL_2_2_7,
              BYAFILE.BYA_AMT_PL_2_2_7,
              BYAFILE.BYA_O2_PL_2_2_7,
              BYAFILE.BYA_INT_PL_2_2_7,
              BYAFILE.BYA_NO_PL_2_2_8,
              BYAFILE.BYA_AMT_PL_2_2_8,
              BYAFILE.BYA_O2_PL_2_2_8,
              BYAFILE.BYA_INT_PL_2_2_8,
              BYAFILE.BYA_NO_PL_2_2_9,
              BYAFILE.BYA_AMT_PL_2_2_9,
              BYAFILE.BYA_O2_PL_2_2_9,
              BYAFILE.BYA_INT_PL_2_2_9,
              BYAFILE.BYA_NO_PL_2_2_10,
              BYAFILE.BYA_AMT_PL_2_2_10,
              BYAFILE.BYA_O2_PL_2_2_10,
              BYAFILE.BYA_INT_PL_2_2_10,
              BYAFILE.BYA_NO_PL_3_1_1,
              BYAFILE.BYA_AMT_PL_3_1_1,
              BYAFILE.BYA_O2_PL_3_1_1,
              BYAFILE.BYA_INT_PL_3_1_1,
              BYAFILE.BYA_NO_PL_3_1_2,
              BYAFILE.BYA_AMT_PL_3_1_2,
              BYAFILE.BYA_O2_PL_3_1_2,
              BYAFILE.BYA_INT_PL_3_1_2,
              BYAFILE.BYA_NO_PL_3_1_3,
              BYAFILE.BYA_AMT_PL_3_1_3,
              BYAFILE.BYA_O2_PL_3_1_3,
              BYAFILE.BYA_INT_PL_3_1_3,
              BYAFILE.BYA_NO_PL_3_1_4,
              BYAFILE.BYA_AMT_PL_3_1_4,
              BYAFILE.BYA_O2_PL_3_1_4,
              BYAFILE.BYA_INT_PL_3_1_4,
              BYAFILE.BYA_NO_PL_3_1_5,
              BYAFILE.BYA_AMT_PL_3_1_5,
              BYAFILE.BYA_O2_PL_3_1_5,
              BYAFILE.BYA_INT_PL_3_1_5,
              BYAFILE.BYA_NO_PL_3_1_6,
              BYAFILE.BYA_AMT_PL_3_1_6,
              BYAFILE.BYA_O2_PL_3_1_6,
              BYAFILE.BYA_INT_PL_3_1_6,
              BYAFILE.BYA_NO_PL_3_1_7,
              BYAFILE.BYA_AMT_PL_3_1_7,
              BYAFILE.BYA_O2_PL_3_1_7,
              BYAFILE.BYA_INT_PL_3_1_7,
              BYAFILE.BYA_NO_PL_3_1_8,
              BYAFILE.BYA_AMT_PL_3_1_8,
              BYAFILE.BYA_O2_PL_3_1_8,
              BYAFILE.BYA_INT_PL_3_1_8,
              BYAFILE.BYA_NO_PL_3_1_9,
              BYAFILE.BYA_AMT_PL_3_1_9,
              BYAFILE.BYA_O2_PL_3_1_9,
              BYAFILE.BYA_INT_PL_3_1_9,
              BYAFILE.BYA_NO_PL_3_1_10,
              BYAFILE.BYA_AMT_PL_3_1_10,
              BYAFILE.BYA_O2_PL_3_1_10,
              BYAFILE.BYA_INT_PL_3_1_10,
              BYAFILE.BYA_NO_PL_3_2_1,
              BYAFILE.BYA_AMT_PL_3_2_1,
              BYAFILE.BYA_O2_PL_3_2_1,
              BYAFILE.BYA_INT_PL_3_2_1,
              BYAFILE.BYA_NO_PL_3_2_2,
              BYAFILE.BYA_AMT_PL_3_2_2,
              BYAFILE.BYA_O2_PL_3_2_2,
              BYAFILE.BYA_INT_PL_3_2_2,
              BYAFILE.BYA_NO_PL_3_2_3,
              BYAFILE.BYA_AMT_PL_3_2_3,
              BYAFILE.BYA_O2_PL_3_2_3,
              BYAFILE.BYA_INT_PL_3_2_3,
              BYAFILE.BYA_NO_PL_3_2_4,
              BYAFILE.BYA_AMT_PL_3_2_4,
              BYAFILE.BYA_O2_PL_3_2_4,
              BYAFILE.BYA_INT_PL_3_2_4,
              BYAFILE.BYA_NO_PL_3_2_5,
              BYAFILE.BYA_AMT_PL_3_2_5,
              BYAFILE.BYA_O2_PL_3_2_5,
              BYAFILE.BYA_INT_PL_3_2_5,
              BYAFILE.BYA_NO_PL_3_2_6,
              BYAFILE.BYA_AMT_PL_3_2_6,
              BYAFILE.BYA_O2_PL_3_2_6,
              BYAFILE.BYA_INT_PL_3_2_6,
              BYAFILE.BYA_NO_PL_3_2_7,
              BYAFILE.BYA_AMT_PL_3_2_7,
              BYAFILE.BYA_O2_PL_3_2_7,
              BYAFILE.BYA_INT_PL_3_2_7,
              BYAFILE.BYA_NO_PL_3_2_8,
              BYAFILE.BYA_AMT_PL_3_2_8,
              BYAFILE.BYA_O2_PL_3_2_8,
              BYAFILE.BYA_INT_PL_3_2_8,
              BYAFILE.BYA_NO_PL_3_2_9,
              BYAFILE.BYA_AMT_PL_3_2_9,
              BYAFILE.BYA_O2_PL_3_2_9,
              BYAFILE.BYA_INT_PL_3_2_9,
              BYAFILE.BYA_NO_PL_3_2_10,
              BYAFILE.BYA_AMT_PL_3_2_10,
              BYAFILE.BYA_O2_PL_3_2_10,
              BYAFILE.BYA_INT_PL_3_2_10,
              BYAFILE.BYA_DLRVPRIN_1_1_1,
              BYAFILE.BYA_DLRVPRIN_1_1_2,
              BYAFILE.BYA_DLRVPRIN_1_1_3,
              BYAFILE.BYA_DLRVCHGS_1_1_1,
              BYAFILE.BYA_DLRVCHGS_1_1_2,
              BYAFILE.BYA_DLRVCHGS_1_1_3,
              BYAFILE.BYA_DLPARVHELD_1_1,
              BYAFILE.BYA_DLRVPRIN_1_2_1,
              BYAFILE.BYA_DLRVPRIN_1_2_2,
              BYAFILE.BYA_DLRVPRIN_1_2_3,
              BYAFILE.BYA_DLRVCHGS_1_2_1,
              BYAFILE.BYA_DLRVCHGS_1_2_2,
              BYAFILE.BYA_DLRVCHGS_1_2_3,
              BYAFILE.BYA_DLPARVHELD_1_2,
              BYAFILE.BYA_DLRVPRIN_2_1_1,
              BYAFILE.BYA_DLRVPRIN_2_1_2,
              BYAFILE.BYA_DLRVPRIN_2_1_3,
              BYAFILE.BYA_DLRVCHGS_2_1_1,
              BYAFILE.BYA_DLRVCHGS_2_1_2,
              BYAFILE.BYA_DLRVCHGS_2_1_3,
              BYAFILE.BYA_DLPARVHELD_2_1,
              BYAFILE.BYA_DLRVPRIN_2_2_1,
              BYAFILE.BYA_DLRVPRIN_2_2_2,
              BYAFILE.BYA_DLRVPRIN_2_2_3,
              BYAFILE.BYA_DLRVCHGS_2_2_1,
              BYAFILE.BYA_DLRVCHGS_2_2_2,
              BYAFILE.BYA_DLRVCHGS_2_2_3,
              BYAFILE.BYA_DLPARVHELD_2_2,
              BYAFILE.BYA_DLRVPRIN_3_1_1,
              BYAFILE.BYA_DLRVPRIN_3_1_2,
              BYAFILE.BYA_DLRVPRIN_3_1_3,
              BYAFILE.BYA_DLRVCHGS_3_1_1,
              BYAFILE.BYA_DLRVCHGS_3_1_2,
              BYAFILE.BYA_DLRVCHGS_3_1_3,
              BYAFILE.BYA_DLPARVHELD_3_1,
              BYAFILE.BYA_DLRVPRIN_3_2_1,
              BYAFILE.BYA_DLRVPRIN_3_2_2,
              BYAFILE.BYA_DLRVPRIN_3_2_3,
              BYAFILE.BYA_DLRVCHGS_3_2_1,
              BYAFILE.BYA_DLRVCHGS_3_2_2,
              BYAFILE.BYA_DLRVCHGS_3_2_3,
              BYAFILE.BYA_DLPARVHELD_3_2)
            VALUES
             (:QBYA-DATE-CCYYMM,
              :QBYA-BRNO,
              :QBYA-CLASS,
              :QBYA-LTOUCH-DATE,
              :QBYA-NO-1-1-1,
              :QBYA-AMT-1-1-1,
              :QBYA-O2-1-1-1,
              :QBYA-INT-1-1-1,
              :QBYA-NO-1-1-2,
              :QBYA-AMT-1-1-2,
              :QBYA-O2-1-1-2,
              :QBYA-INT-1-1-2,
              :QBYA-NO-1-1-3,
              :QBYA-AMT-1-1-3,
              :QBYA-O2-1-1-3,
              :QBYA-INT-1-1-3,
              :QBYA-NO-1-1-4,
              :QBYA-AMT-1-1-4,
              :QBYA-O2-1-1-4,
              :QBYA-INT-1-1-4,
              :QBYA-NO-1-1-5,
              :QBYA-AMT-1-1-5,
              :QBYA-O2-1-1-5,
              :QBYA-INT-1-1-5,
              :QBYA-NO-1-1-6,
              :QBYA-AMT-1-1-6,
              :QBYA-O2-1-1-6,
              :QBYA-INT-1-1-6,
              :QBYA-NO-1-1-7,
              :QBYA-AMT-1-1-7,
              :QBYA-O2-1-1-7,
              :QBYA-INT-1-1-7,
              :QBYA-NO-1-1-8,
              :QBYA-AMT-1-1-8,
              :QBYA-O2-1-1-8,
              :QBYA-INT-1-1-8,
              :QBYA-NO-1-1-9,
              :QBYA-AMT-1-1-9,
              :QBYA-O2-1-1-9,
              :QBYA-INT-1-1-9,
              :QBYA-NO-1-1-10,
              :QBYA-AMT-1-1-10,
              :QBYA-O2-1-1-10,
              :QBYA-INT-1-1-10,
              :QBYA-NO-1-1-11,
              :QBYA-AMT-1-1-11,
              :QBYA-O2-1-1-11,
              :QBYA-INT-1-1-11,
              :QBYA-NO-1-2-1,
              :QBYA-AMT-1-2-1,
              :QBYA-O2-1-2-1,
              :QBYA-INT-1-2-1,
              :QBYA-NO-1-2-2,
              :QBYA-AMT-1-2-2,
              :QBYA-O2-1-2-2,
              :QBYA-INT-1-2-2,
              :QBYA-NO-1-2-3,
              :QBYA-AMT-1-2-3,
              :QBYA-O2-1-2-3,
              :QBYA-INT-1-2-3,
              :QBYA-NO-1-2-4,
              :QBYA-AMT-1-2-4,
              :QBYA-O2-1-2-4,
              :QBYA-INT-1-2-4,
              :QBYA-NO-1-2-5,
              :QBYA-AMT-1-2-5,
              :QBYA-O2-1-2-5,
              :QBYA-INT-1-2-5,
              :QBYA-NO-1-2-6,
              :QBYA-AMT-1-2-6,
              :QBYA-O2-1-2-6,
              :QBYA-INT-1-2-6,
              :QBYA-NO-1-2-7,
              :QBYA-AMT-1-2-7,
              :QBYA-O2-1-2-7,
              :QBYA-INT-1-2-7,
              :QBYA-NO-1-2-8,
              :QBYA-AMT-1-2-8,
              :QBYA-O2-1-2-8,
              :QBYA-INT-1-2-8,
              :QBYA-NO-1-2-9,
              :QBYA-AMT-1-2-9,
              :QBYA-O2-1-2-9,
              :QBYA-INT-1-2-9,
              :QBYA-NO-1-2-10,
              :QBYA-AMT-1-2-10,
              :QBYA-O2-1-2-10,
              :QBYA-INT-1-2-10,
              :QBYA-NO-1-2-11,
              :QBYA-AMT-1-2-11,
              :QBYA-O2-1-2-11,
              :QBYA-INT-1-2-11,
              :QBYA-NO-2-1-1,
              :QBYA-AMT-2-1-1,
              :QBYA-O2-2-1-1,
              :QBYA-INT-2-1-1,
              :QBYA-NO-2-1-2,
              :QBYA-AMT-2-1-2,
              :QBYA-O2-2-1-2,
              :QBYA-INT-2-1-2,
              :QBYA-NO-2-1-3,
              :QBYA-AMT-2-1-3,
              :QBYA-O2-2-1-3,
              :QBYA-INT-2-1-3,
              :QBYA-NO-2-1-4,
              :QBYA-AMT-2-1-4,
              :QBYA-O2-2-1-4,
              :QBYA-INT-2-1-4,
              :QBYA-NO-2-1-5,
              :QBYA-AMT-2-1-5,
              :QBYA-O2-2-1-5,
              :QBYA-INT-2-1-5,
              :QBYA-NO-2-1-6,
              :QBYA-AMT-2-1-6,
              :QBYA-O2-2-1-6,
              :QBYA-INT-2-1-6,
              :QBYA-NO-2-1-7,
              :QBYA-AMT-2-1-7,
              :QBYA-O2-2-1-7,
              :QBYA-INT-2-1-7,
              :QBYA-NO-2-1-8,
              :QBYA-AMT-2-1-8,
              :QBYA-O2-2-1-8,
              :QBYA-INT-2-1-8,
              :QBYA-NO-2-1-9,
              :QBYA-AMT-2-1-9,
              :QBYA-O2-2-1-9,
              :QBYA-INT-2-1-9,
              :QBYA-NO-2-1-10,
              :QBYA-AMT-2-1-10,
              :QBYA-O2-2-1-10,
              :QBYA-INT-2-1-10,
              :QBYA-NO-2-1-11,
              :QBYA-AMT-2-1-11,
              :QBYA-O2-2-1-11,
              :QBYA-INT-2-1-11,
              :QBYA-NO-2-2-1,
              :QBYA-AMT-2-2-1,
              :QBYA-O2-2-2-1,
              :QBYA-INT-2-2-1,
              :QBYA-NO-2-2-2,
              :QBYA-AMT-2-2-2,
              :QBYA-O2-2-2-2,
              :QBYA-INT-2-2-2,
              :QBYA-NO-2-2-3,
              :QBYA-AMT-2-2-3,
              :QBYA-O2-2-2-3,
              :QBYA-INT-2-2-3,
              :QBYA-NO-2-2-4,
              :QBYA-AMT-2-2-4,
              :QBYA-O2-2-2-4,
              :QBYA-INT-2-2-4,
              :QBYA-NO-2-2-5,
              :QBYA-AMT-2-2-5,
              :QBYA-O2-2-2-5,
              :QBYA-INT-2-2-5,
              :QBYA-NO-2-2-6,
              :QBYA-AMT-2-2-6,
              :QBYA-O2-2-2-6,
              :QBYA-INT-2-2-6,
              :QBYA-NO-2-2-7,
              :QBYA-AMT-2-2-7,
              :QBYA-O2-2-2-7,
              :QBYA-INT-2-2-7,
              :QBYA-NO-2-2-8,
              :QBYA-AMT-2-2-8,
              :QBYA-O2-2-2-8,
              :QBYA-INT-2-2-8,
              :QBYA-NO-2-2-9,
              :QBYA-AMT-2-2-9,
              :QBYA-O2-2-2-9,
              :QBYA-INT-2-2-9,
              :QBYA-NO-2-2-10,
              :QBYA-AMT-2-2-10,
              :QBYA-O2-2-2-10,
              :QBYA-INT-2-2-10,
              :QBYA-NO-2-2-11,
              :QBYA-AMT-2-2-11,
              :QBYA-O2-2-2-11,
              :QBYA-INT-2-2-11,
              :QBYA-NO-3-1-1,
              :QBYA-AMT-3-1-1,
              :QBYA-O2-3-1-1,
              :QBYA-INT-3-1-1,
              :QBYA-NO-3-1-2,
              :QBYA-AMT-3-1-2,
              :QBYA-O2-3-1-2,
              :QBYA-INT-3-1-2,
              :QBYA-NO-3-1-3,
              :QBYA-AMT-3-1-3,
              :QBYA-O2-3-1-3,
              :QBYA-INT-3-1-3,
              :QBYA-NO-3-1-4,
              :QBYA-AMT-3-1-4,
              :QBYA-O2-3-1-4,
              :QBYA-INT-3-1-4,
              :QBYA-NO-3-1-5,
              :QBYA-AMT-3-1-5,
              :QBYA-O2-3-1-5,
              :QBYA-INT-3-1-5,
              :QBYA-NO-3-1-6,
              :QBYA-AMT-3-1-6,
              :QBYA-O2-3-1-6,
              :QBYA-INT-3-1-6,
              :QBYA-NO-3-1-7,
              :QBYA-AMT-3-1-7,
              :QBYA-O2-3-1-7,
              :QBYA-INT-3-1-7,
              :QBYA-NO-3-1-8,
              :QBYA-AMT-3-1-8,
              :QBYA-O2-3-1-8,
              :QBYA-INT-3-1-8,
              :QBYA-NO-3-1-9,
              :QBYA-AMT-3-1-9,
              :QBYA-O2-3-1-9,
              :QBYA-INT-3-1-9,
              :QBYA-NO-3-1-10,
              :QBYA-AMT-3-1-10,
              :QBYA-O2-3-1-10,
              :QBYA-INT-3-1-10,
              :QBYA-NO-3-1-11,
              :QBYA-AMT-3-1-11,
              :QBYA-O2-3-1-11,
              :QBYA-INT-3-1-11,
              :QBYA-NO-3-2-1,
              :QBYA-AMT-3-2-1,
              :QBYA-O2-3-2-1,
              :QBYA-INT-3-2-1,
              :QBYA-NO-3-2-2,
              :QBYA-AMT-3-2-2,
              :QBYA-O2-3-2-2,
              :QBYA-INT-3-2-2,
              :QBYA-NO-3-2-3,
              :QBYA-AMT-3-2-3,
              :QBYA-O2-3-2-3,
              :QBYA-INT-3-2-3,
              :QBYA-NO-3-2-4,
              :QBYA-AMT-3-2-4,
              :QBYA-O2-3-2-4,
              :QBYA-INT-3-2-4,
              :QBYA-NO-3-2-5,
              :QBYA-AMT-3-2-5,
              :QBYA-O2-3-2-5,
              :QBYA-INT-3-2-5,
              :QBYA-NO-3-2-6,
              :QBYA-AMT-3-2-6,
              :QBYA-O2-3-2-6,
              :QBYA-INT-3-2-6,
              :QBYA-NO-3-2-7,
              :QBYA-AMT-3-2-7,
              :QBYA-O2-3-2-7,
              :QBYA-INT-3-2-7,
              :QBYA-NO-3-2-8,
              :QBYA-AMT-3-2-8,
              :QBYA-O2-3-2-8,
              :QBYA-INT-3-2-8,
              :QBYA-NO-3-2-9,
              :QBYA-AMT-3-2-9,
              :QBYA-O2-3-2-9,
              :QBYA-INT-3-2-9,
              :QBYA-NO-3-2-10,
              :QBYA-AMT-3-2-10,
              :QBYA-O2-3-2-10,
              :QBYA-INT-3-2-10,
              :QBYA-NO-3-2-11,
              :QBYA-AMT-3-2-11,
              :QBYA-O2-3-2-11,
              :QBYA-INT-3-2-11,
              :QBYA-NO-REPO-1-1-1,
              :QBYA-AMT-REPO-1-1-1,
              :QBYA-O2-REPO-1-1-1,
              :QBYA-INT-REPO-1-1-1,
              :QBYA-NO-REPO-1-1-2,
              :QBYA-AMT-REPO-1-1-2,
              :QBYA-O2-REPO-1-1-2,
              :QBYA-INT-REPO-1-1-2,
              :QBYA-NO-REPO-1-1-3,
              :QBYA-AMT-REPO-1-1-3,
              :QBYA-O2-REPO-1-1-3,
              :QBYA-INT-REPO-1-1-3,
              :QBYA-NO-REPO-1-1-4,
              :QBYA-AMT-REPO-1-1-4,
              :QBYA-O2-REPO-1-1-4,
              :QBYA-INT-REPO-1-1-4,
              :QBYA-NO-REPO-1-1-5,
              :QBYA-AMT-REPO-1-1-5,
              :QBYA-O2-REPO-1-1-5,
              :QBYA-INT-REPO-1-1-5,
              :QBYA-NO-REPO-1-2-1,
              :QBYA-AMT-REPO-1-2-1,
              :QBYA-O2-REPO-1-2-1,
              :QBYA-INT-REPO-1-2-1,
              :QBYA-NO-REPO-1-2-2,
              :QBYA-AMT-REPO-1-2-2,
              :QBYA-O2-REPO-1-2-2,
              :QBYA-INT-REPO-1-2-2,
              :QBYA-NO-REPO-1-2-3,
              :QBYA-AMT-REPO-1-2-3,
              :QBYA-O2-REPO-1-2-3,
              :QBYA-INT-REPO-1-2-3,
              :QBYA-NO-REPO-1-2-4,
              :QBYA-AMT-REPO-1-2-4,
              :QBYA-O2-REPO-1-2-4,
              :QBYA-INT-REPO-1-2-4,
              :QBYA-NO-REPO-1-2-5,
              :QBYA-AMT-REPO-1-2-5,
              :QBYA-O2-REPO-1-2-5,
              :QBYA-INT-REPO-1-2-5,
              :QBYA-NO-REPO-2-1-1,
              :QBYA-AMT-REPO-2-1-1,
              :QBYA-O2-REPO-2-1-1,
              :QBYA-INT-REPO-2-1-1,
              :QBYA-NO-REPO-2-1-2,
              :QBYA-AMT-REPO-2-1-2,
              :QBYA-O2-REPO-2-1-2,
              :QBYA-INT-REPO-2-1-2,
              :QBYA-NO-REPO-2-1-3,
              :QBYA-AMT-REPO-2-1-3,
              :QBYA-O2-REPO-2-1-3,
              :QBYA-INT-REPO-2-1-3,
              :QBYA-NO-REPO-2-1-4,
              :QBYA-AMT-REPO-2-1-4,
              :QBYA-O2-REPO-2-1-4,
              :QBYA-INT-REPO-2-1-4,
              :QBYA-NO-REPO-2-1-5,
              :QBYA-AMT-REPO-2-1-5,
              :QBYA-O2-REPO-2-1-5,
              :QBYA-INT-REPO-2-1-5,
              :QBYA-NO-REPO-2-2-1,
              :QBYA-AMT-REPO-2-2-1,
              :QBYA-O2-REPO-2-2-1,
              :QBYA-INT-REPO-2-2-1,
              :QBYA-NO-REPO-2-2-2,
              :QBYA-AMT-REPO-2-2-2,
              :QBYA-O2-REPO-2-2-2,
              :QBYA-INT-REPO-2-2-2,
              :QBYA-NO-REPO-2-2-3,
              :QBYA-AMT-REPO-2-2-3,
              :QBYA-O2-REPO-2-2-3,
              :QBYA-INT-REPO-2-2-3,
              :QBYA-NO-REPO-2-2-4,
              :QBYA-AMT-REPO-2-2-4,
              :QBYA-O2-REPO-2-2-4,
              :QBYA-INT-REPO-2-2-4,
              :QBYA-NO-REPO-2-2-5,
              :QBYA-AMT-REPO-2-2-5,
              :QBYA-O2-REPO-2-2-5,
              :QBYA-INT-REPO-2-2-5,
              :QBYA-NO-REPO-3-1-1,
              :QBYA-AMT-REPO-3-1-1,
              :QBYA-O2-REPO-3-1-1,
              :QBYA-INT-REPO-3-1-1,
              :QBYA-NO-REPO-3-1-2,
              :QBYA-AMT-REPO-3-1-2,
              :QBYA-O2-REPO-3-1-2,
              :QBYA-INT-REPO-3-1-2,
              :QBYA-NO-REPO-3-1-3,
              :QBYA-AMT-REPO-3-1-3,
              :QBYA-O2-REPO-3-1-3,
              :QBYA-INT-REPO-3-1-3,
              :QBYA-NO-REPO-3-1-4,
              :QBYA-AMT-REPO-3-1-4,
              :QBYA-O2-REPO-3-1-4,
              :QBYA-INT-REPO-3-1-4,
              :QBYA-NO-REPO-3-1-5,
              :QBYA-AMT-REPO-3-1-5,
              :QBYA-O2-REPO-3-1-5,
              :QBYA-INT-REPO-3-1-5,
              :QBYA-NO-REPO-3-2-1,
              :QBYA-AMT-REPO-3-2-1,
              :QBYA-O2-REPO-3-2-1,
              :QBYA-INT-REPO-3-2-1,
              :QBYA-NO-REPO-3-2-2,
              :QBYA-AMT-REPO-3-2-2,
              :QBYA-O2-REPO-3-2-2,
              :QBYA-INT-REPO-3-2-2,
              :QBYA-NO-REPO-3-2-3,
              :QBYA-AMT-REPO-3-2-3,
              :QBYA-O2-REPO-3-2-3,
              :QBYA-INT-REPO-3-2-3,
              :QBYA-NO-REPO-3-2-4,
              :QBYA-AMT-REPO-3-2-4,
              :QBYA-O2-REPO-3-2-4,
              :QBYA-INT-REPO-3-2-4,
              :QBYA-NO-REPO-3-2-5,
              :QBYA-AMT-REPO-3-2-5,
              :QBYA-O2-REPO-3-2-5,
              :QBYA-INT-REPO-3-2-5,
              :QBYA-NO-PL-1-1-1,
              :QBYA-AMT-PL-1-1-1,
              :QBYA-O2-PL-1-1-1,
              :QBYA-INT-PL-1-1-1,
              :QBYA-NO-PL-1-1-2,
              :QBYA-AMT-PL-1-1-2,
              :QBYA-O2-PL-1-1-2,
              :QBYA-INT-PL-1-1-2,
              :QBYA-NO-PL-1-1-3,
              :QBYA-AMT-PL-1-1-3,
              :QBYA-O2-PL-1-1-3,
              :QBYA-INT-PL-1-1-3,
              :QBYA-NO-PL-1-1-4,
              :QBYA-AMT-PL-1-1-4,
              :QBYA-O2-PL-1-1-4,
              :QBYA-INT-PL-1-1-4,
              :QBYA-NO-PL-1-1-5,
              :QBYA-AMT-PL-1-1-5,
              :QBYA-O2-PL-1-1-5,
              :QBYA-INT-PL-1-1-5,
              :QBYA-NO-PL-1-1-6,
              :QBYA-AMT-PL-1-1-6,
              :QBYA-O2-PL-1-1-6,
              :QBYA-INT-PL-1-1-6,
              :QBYA-NO-PL-1-1-7,
              :QBYA-AMT-PL-1-1-7,
              :QBYA-O2-PL-1-1-7,
              :QBYA-INT-PL-1-1-7,
              :QBYA-NO-PL-1-1-8,
              :QBYA-AMT-PL-1-1-8,
              :QBYA-O2-PL-1-1-8,
              :QBYA-INT-PL-1-1-8,
              :QBYA-NO-PL-1-1-9,
              :QBYA-AMT-PL-1-1-9,
              :QBYA-O2-PL-1-1-9,
              :QBYA-INT-PL-1-1-9,
              :QBYA-NO-PL-1-1-10,
              :QBYA-AMT-PL-1-1-10,
              :QBYA-O2-PL-1-1-10,
              :QBYA-INT-PL-1-1-10,
              :QBYA-NO-PL-1-2-1,
              :QBYA-AMT-PL-1-2-1,
              :QBYA-O2-PL-1-2-1,
              :QBYA-INT-PL-1-2-1,
              :QBYA-NO-PL-1-2-2,
              :QBYA-AMT-PL-1-2-2,
              :QBYA-O2-PL-1-2-2,
              :QBYA-INT-PL-1-2-2,
              :QBYA-NO-PL-1-2-3,
              :QBYA-AMT-PL-1-2-3,
              :QBYA-O2-PL-1-2-3,
              :QBYA-INT-PL-1-2-3,
              :QBYA-NO-PL-1-2-4,
              :QBYA-AMT-PL-1-2-4,
              :QBYA-O2-PL-1-2-4,
              :QBYA-INT-PL-1-2-4,
              :QBYA-NO-PL-1-2-5,
              :QBYA-AMT-PL-1-2-5,
              :QBYA-O2-PL-1-2-5,
              :QBYA-INT-PL-1-2-5,
              :QBYA-NO-PL-1-2-6,
              :QBYA-AMT-PL-1-2-6,
              :QBYA-O2-PL-1-2-6,
              :QBYA-INT-PL-1-2-6,
              :QBYA-NO-PL-1-2-7,
              :QBYA-AMT-PL-1-2-7,
              :QBYA-O2-PL-1-2-7,
              :QBYA-INT-PL-1-2-7,
              :QBYA-NO-PL-1-2-8,
              :QBYA-AMT-PL-1-2-8,
              :QBYA-O2-PL-1-2-8,
              :QBYA-INT-PL-1-2-8,
              :QBYA-NO-PL-1-2-9,
              :QBYA-AMT-PL-1-2-9,
              :QBYA-O2-PL-1-2-9,
              :QBYA-INT-PL-1-2-9,
              :QBYA-NO-PL-1-2-10,
              :QBYA-AMT-PL-1-2-10,
              :QBYA-O2-PL-1-2-10,
              :QBYA-INT-PL-1-2-10,
              :QBYA-NO-PL-2-1-1,
              :QBYA-AMT-PL-2-1-1,
              :QBYA-O2-PL-2-1-1,
              :QBYA-INT-PL-2-1-1,
              :QBYA-NO-PL-2-1-2,
              :QBYA-AMT-PL-2-1-2,
              :QBYA-O2-PL-2-1-2,
              :QBYA-INT-PL-2-1-2,
              :QBYA-NO-PL-2-1-3,
              :QBYA-AMT-PL-2-1-3,
              :QBYA-O2-PL-2-1-3,
              :QBYA-INT-PL-2-1-3,
              :QBYA-NO-PL-2-1-4,
              :QBYA-AMT-PL-2-1-4,
              :QBYA-O2-PL-2-1-4,
              :QBYA-INT-PL-2-1-4,
              :QBYA-NO-PL-2-1-5,
              :QBYA-AMT-PL-2-1-5,
              :QBYA-O2-PL-2-1-5,
              :QBYA-INT-PL-2-1-5,
              :QBYA-NO-PL-2-1-6,
              :QBYA-AMT-PL-2-1-6,
              :QBYA-O2-PL-2-1-6,
              :QBYA-INT-PL-2-1-6,
              :QBYA-NO-PL-2-1-7,
              :QBYA-AMT-PL-2-1-7,
              :QBYA-O2-PL-2-1-7,
              :QBYA-INT-PL-2-1-7,
              :QBYA-NO-PL-2-1-8,
              :QBYA-AMT-PL-2-1-8,
              :QBYA-O2-PL-2-1-8,
              :QBYA-INT-PL-2-1-8,
              :QBYA-NO-PL-2-1-9,
              :QBYA-AMT-PL-2-1-9,
              :QBYA-O2-PL-2-1-9,
              :QBYA-INT-PL-2-1-9,
              :QBYA-NO-PL-2-1-10,
              :QBYA-AMT-PL-2-1-10,
              :QBYA-O2-PL-2-1-10,
              :QBYA-INT-PL-2-1-10,
              :QBYA-NO-PL-2-2-1,
              :QBYA-AMT-PL-2-2-1,
              :QBYA-O2-PL-2-2-1,
              :QBYA-INT-PL-2-2-1,
              :QBYA-NO-PL-2-2-2,
              :QBYA-AMT-PL-2-2-2,
              :QBYA-O2-PL-2-2-2,
              :QBYA-INT-PL-2-2-2,
              :QBYA-NO-PL-2-2-3,
              :QBYA-AMT-PL-2-2-3,
              :QBYA-O2-PL-2-2-3,
              :QBYA-INT-PL-2-2-3,
              :QBYA-NO-PL-2-2-4,
              :QBYA-AMT-PL-2-2-4,
              :QBYA-O2-PL-2-2-4,
              :QBYA-INT-PL-2-2-4,
              :QBYA-NO-PL-2-2-5,
              :QBYA-AMT-PL-2-2-5,
              :QBYA-O2-PL-2-2-5,
              :QBYA-INT-PL-2-2-5,
              :QBYA-NO-PL-2-2-6,
              :QBYA-AMT-PL-2-2-6,
              :QBYA-O2-PL-2-2-6,
              :QBYA-INT-PL-2-2-6,
              :QBYA-NO-PL-2-2-7,
              :QBYA-AMT-PL-2-2-7,
              :QBYA-O2-PL-2-2-7,
              :QBYA-INT-PL-2-2-7,
              :QBYA-NO-PL-2-2-8,
              :QBYA-AMT-PL-2-2-8,
              :QBYA-O2-PL-2-2-8,
              :QBYA-INT-PL-2-2-8,
              :QBYA-NO-PL-2-2-9,
              :QBYA-AMT-PL-2-2-9,
              :QBYA-O2-PL-2-2-9,
              :QBYA-INT-PL-2-2-9,
              :QBYA-NO-PL-2-2-10,
              :QBYA-AMT-PL-2-2-10,
              :QBYA-O2-PL-2-2-10,
              :QBYA-INT-PL-2-2-10,
              :QBYA-NO-PL-3-1-1,
              :QBYA-AMT-PL-3-1-1,
              :QBYA-O2-PL-3-1-1,
              :QBYA-INT-PL-3-1-1,
              :QBYA-NO-PL-3-1-2,
              :QBYA-AMT-PL-3-1-2,
              :QBYA-O2-PL-3-1-2,
              :QBYA-INT-PL-3-1-2,
              :QBYA-NO-PL-3-1-3,
              :QBYA-AMT-PL-3-1-3,
              :QBYA-O2-PL-3-1-3,
              :QBYA-INT-PL-3-1-3,
              :QBYA-NO-PL-3-1-4,
              :QBYA-AMT-PL-3-1-4,
              :QBYA-O2-PL-3-1-4,
              :QBYA-INT-PL-3-1-4,
              :QBYA-NO-PL-3-1-5,
              :QBYA-AMT-PL-3-1-5,
              :QBYA-O2-PL-3-1-5,
              :QBYA-INT-PL-3-1-5,
              :QBYA-NO-PL-3-1-6,
              :QBYA-AMT-PL-3-1-6,
              :QBYA-O2-PL-3-1-6,
              :QBYA-INT-PL-3-1-6,
              :QBYA-NO-PL-3-1-7,
              :QBYA-AMT-PL-3-1-7,
              :QBYA-O2-PL-3-1-7,
              :QBYA-INT-PL-3-1-7,
              :QBYA-NO-PL-3-1-8,
              :QBYA-AMT-PL-3-1-8,
              :QBYA-O2-PL-3-1-8,
              :QBYA-INT-PL-3-1-8,
              :QBYA-NO-PL-3-1-9,
              :QBYA-AMT-PL-3-1-9,
              :QBYA-O2-PL-3-1-9,
              :QBYA-INT-PL-3-1-9,
              :QBYA-NO-PL-3-1-10,
              :QBYA-AMT-PL-3-1-10,
              :QBYA-O2-PL-3-1-10,
              :QBYA-INT-PL-3-1-10,
              :QBYA-NO-PL-3-2-1,
              :QBYA-AMT-PL-3-2-1,
              :QBYA-O2-PL-3-2-1,
              :QBYA-INT-PL-3-2-1,
              :QBYA-NO-PL-3-2-2,
              :QBYA-AMT-PL-3-2-2,
              :QBYA-O2-PL-3-2-2,
              :QBYA-INT-PL-3-2-2,
              :QBYA-NO-PL-3-2-3,
              :QBYA-AMT-PL-3-2-3,
              :QBYA-O2-PL-3-2-3,
              :QBYA-INT-PL-3-2-3,
              :QBYA-NO-PL-3-2-4,
              :QBYA-AMT-PL-3-2-4,
              :QBYA-O2-PL-3-2-4,
              :QBYA-INT-PL-3-2-4,
              :QBYA-NO-PL-3-2-5,
              :QBYA-AMT-PL-3-2-5,
              :QBYA-O2-PL-3-2-5,
              :QBYA-INT-PL-3-2-5,
              :QBYA-NO-PL-3-2-6,
              :QBYA-AMT-PL-3-2-6,
              :QBYA-O2-PL-3-2-6,
              :QBYA-INT-PL-3-2-6,
              :QBYA-NO-PL-3-2-7,
              :QBYA-AMT-PL-3-2-7,
              :QBYA-O2-PL-3-2-7,
              :QBYA-INT-PL-3-2-7,
              :QBYA-NO-PL-3-2-8,
              :QBYA-AMT-PL-3-2-8,
              :QBYA-O2-PL-3-2-8,
              :QBYA-INT-PL-3-2-8,
              :QBYA-NO-PL-3-2-9,
              :QBYA-AMT-PL-3-2-9,
              :QBYA-O2-PL-3-2-9,
              :QBYA-INT-PL-3-2-9,
              :QBYA-NO-PL-3-2-10,
              :QBYA-AMT-PL-3-2-10,
              :QBYA-O2-PL-3-2-10,
              :QBYA-INT-PL-3-2-10,
              :QBYA-DLRVPRIN-1-1-1,
              :QBYA-DLRVPRIN-1-1-2,
              :QBYA-DLRVPRIN-1-1-3,
              :QBYA-DLRVCHGS-1-1-1,
              :QBYA-DLRVCHGS-1-1-2,
              :QBYA-DLRVCHGS-1-1-3,
              :QBYA-DLPARVHELD-1-1,
              :QBYA-DLRVPRIN-1-2-1,
              :QBYA-DLRVPRIN-1-2-2,
              :QBYA-DLRVPRIN-1-2-3,
              :QBYA-DLRVCHGS-1-2-1,
              :QBYA-DLRVCHGS-1-2-2,
              :QBYA-DLRVCHGS-1-2-3,
              :QBYA-DLPARVHELD-1-2,
              :QBYA-DLRVPRIN-2-1-1,
              :QBYA-DLRVPRIN-2-1-2,
              :QBYA-DLRVPRIN-2-1-3,
              :QBYA-DLRVCHGS-2-1-1,
              :QBYA-DLRVCHGS-2-1-2,
              :QBYA-DLRVCHGS-2-1-3,
              :QBYA-DLPARVHELD-2-1,
              :QBYA-DLRVPRIN-2-2-1,
              :QBYA-DLRVPRIN-2-2-2,
              :QBYA-DLRVPRIN-2-2-3,
              :QBYA-DLRVCHGS-2-2-1,
              :QBYA-DLRVCHGS-2-2-2,
              :QBYA-DLRVCHGS-2-2-3,
              :QBYA-DLPARVHELD-2-2,
              :QBYA-DLRVPRIN-3-1-1,
              :QBYA-DLRVPRIN-3-1-2,
              :QBYA-DLRVPRIN-3-1-3,
              :QBYA-DLRVCHGS-3-1-1,
              :QBYA-DLRVCHGS-3-1-2,
              :QBYA-DLRVCHGS-3-1-3,
              :QBYA-DLPARVHELD-3-1,
              :QBYA-DLRVPRIN-3-2-1,
              :QBYA-DLRVPRIN-3-2-2,
              :QBYA-DLRVPRIN-3-2-3,
              :QBYA-DLRVCHGS-3-2-1,
              :QBYA-DLRVCHGS-3-2-2,
              :QBYA-DLRVCHGS-3-2-3,
              :QBYA-DLPARVHELD-3-2)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-BYA1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

           MOVE  '1' TO SQL-UPDATE-CONN-USE-SW. *> USE C1 CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

      *-----------------------------------------------------------------
       REWRITE-BYA1-FILE. *> EMBEDDED - BYAFILE

           ACCEPT BYA-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE BYA-PATH-SQL TO E-FILE.
           MOVE BYA1-KEY TO E-KEYX.
           PERFORM SET-BYA-FIELDS.

           MOVE BY-DATE-CCYYMM      TO QBYA-DATE-CCYYMM.
           MOVE BY-BRNO             TO QBYA-BRNO.
           MOVE BY-CLASS            TO QBYA-CLASS.

           MOVE '2' TO SQL-UPDATE-CONN-USE-SW. *> USE UPDATE_CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

           EXEC SQL
            UPDATE DBO.BYAFILE
             SET
              BYAFILE.BYA_DATE_CCYYMM = :QBYA-DATE-CCYYMM,
              BYAFILE.BYA_BRNO = :QBYA-BRNO,
              BYAFILE.BYA_CLASS = :QBYA-CLASS,
              BYAFILE.BYA_LTOUCH_DATE = :QBYA-LTOUCH-DATE,
              BYAFILE.BYA_NO_1_1_1 = :QBYA-NO-1-1-1,
              BYAFILE.BYA_AMT_1_1_1 = :QBYA-AMT-1-1-1,
              BYAFILE.BYA_O2_1_1_1 = :QBYA-O2-1-1-1,
              BYAFILE.BYA_INT_1_1_1 = :QBYA-INT-1-1-1,
              BYAFILE.BYA_NO_1_1_2 = :QBYA-NO-1-1-2,
              BYAFILE.BYA_AMT_1_1_2 = :QBYA-AMT-1-1-2,
              BYAFILE.BYA_O2_1_1_2 = :QBYA-O2-1-1-2,
              BYAFILE.BYA_INT_1_1_2 = :QBYA-INT-1-1-2,
              BYAFILE.BYA_NO_1_1_3 = :QBYA-NO-1-1-3,
              BYAFILE.BYA_AMT_1_1_3 = :QBYA-AMT-1-1-3,
              BYAFILE.BYA_O2_1_1_3 = :QBYA-O2-1-1-3,
              BYAFILE.BYA_INT_1_1_3 = :QBYA-INT-1-1-3,
              BYAFILE.BYA_NO_1_1_4 = :QBYA-NO-1-1-4,
              BYAFILE.BYA_AMT_1_1_4 = :QBYA-AMT-1-1-4,
              BYAFILE.BYA_O2_1_1_4 = :QBYA-O2-1-1-4,
              BYAFILE.BYA_INT_1_1_4 = :QBYA-INT-1-1-4,
              BYAFILE.BYA_NO_1_1_5 = :QBYA-NO-1-1-5,
              BYAFILE.BYA_AMT_1_1_5 = :QBYA-AMT-1-1-5,
              BYAFILE.BYA_O2_1_1_5 = :QBYA-O2-1-1-5,
              BYAFILE.BYA_INT_1_1_5 = :QBYA-INT-1-1-5,
              BYAFILE.BYA_NO_1_1_6 = :QBYA-NO-1-1-6,
              BYAFILE.BYA_AMT_1_1_6 = :QBYA-AMT-1-1-6,
              BYAFILE.BYA_O2_1_1_6 = :QBYA-O2-1-1-6,
              BYAFILE.BYA_INT_1_1_6 = :QBYA-INT-1-1-6,
              BYAFILE.BYA_NO_1_1_7 = :QBYA-NO-1-1-7,
              BYAFILE.BYA_AMT_1_1_7 = :QBYA-AMT-1-1-7,
              BYAFILE.BYA_O2_1_1_7 = :QBYA-O2-1-1-7,
              BYAFILE.BYA_INT_1_1_7 = :QBYA-INT-1-1-7,
              BYAFILE.BYA_NO_1_1_8 = :QBYA-NO-1-1-8,
              BYAFILE.BYA_AMT_1_1_8 = :QBYA-AMT-1-1-8,
              BYAFILE.BYA_O2_1_1_8 = :QBYA-O2-1-1-8,
              BYAFILE.BYA_INT_1_1_8 = :QBYA-INT-1-1-8,
              BYAFILE.BYA_NO_1_1_9 = :QBYA-NO-1-1-9,
              BYAFILE.BYA_AMT_1_1_9 = :QBYA-AMT-1-1-9,
              BYAFILE.BYA_O2_1_1_9 = :QBYA-O2-1-1-9,
              BYAFILE.BYA_INT_1_1_9 = :QBYA-INT-1-1-9,
              BYAFILE.BYA_NO_1_1_10 = :QBYA-NO-1-1-10,
              BYAFILE.BYA_AMT_1_1_10 = :QBYA-AMT-1-1-10,
              BYAFILE.BYA_O2_1_1_10 = :QBYA-O2-1-1-10,
              BYAFILE.BYA_INT_1_1_10 = :QBYA-INT-1-1-10,
              BYAFILE.BYA_NO_1_1_11 = :QBYA-NO-1-1-11,
              BYAFILE.BYA_AMT_1_1_11 = :QBYA-AMT-1-1-11,
              BYAFILE.BYA_O2_1_1_11 = :QBYA-O2-1-1-11,
              BYAFILE.BYA_INT_1_1_11 = :QBYA-INT-1-1-11,
              BYAFILE.BYA_NO_1_2_1 = :QBYA-NO-1-2-1,
              BYAFILE.BYA_AMT_1_2_1 = :QBYA-AMT-1-2-1,
              BYAFILE.BYA_O2_1_2_1 = :QBYA-O2-1-2-1,
              BYAFILE.BYA_INT_1_2_1 = :QBYA-INT-1-2-1,
              BYAFILE.BYA_NO_1_2_2 = :QBYA-NO-1-2-2,
              BYAFILE.BYA_AMT_1_2_2 = :QBYA-AMT-1-2-2,
              BYAFILE.BYA_O2_1_2_2 = :QBYA-O2-1-2-2,
              BYAFILE.BYA_INT_1_2_2 = :QBYA-INT-1-2-2,
              BYAFILE.BYA_NO_1_2_3 = :QBYA-NO-1-2-3,
              BYAFILE.BYA_AMT_1_2_3 = :QBYA-AMT-1-2-3,
              BYAFILE.BYA_O2_1_2_3 = :QBYA-O2-1-2-3,
              BYAFILE.BYA_INT_1_2_3 = :QBYA-INT-1-2-3,
              BYAFILE.BYA_NO_1_2_4 = :QBYA-NO-1-2-4,
              BYAFILE.BYA_AMT_1_2_4 = :QBYA-AMT-1-2-4,
              BYAFILE.BYA_O2_1_2_4 = :QBYA-O2-1-2-4,
              BYAFILE.BYA_INT_1_2_4 = :QBYA-INT-1-2-4,
              BYAFILE.BYA_NO_1_2_5 = :QBYA-NO-1-2-5,
              BYAFILE.BYA_AMT_1_2_5 = :QBYA-AMT-1-2-5,
              BYAFILE.BYA_O2_1_2_5 = :QBYA-O2-1-2-5,
              BYAFILE.BYA_INT_1_2_5 = :QBYA-INT-1-2-5,
              BYAFILE.BYA_NO_1_2_6 = :QBYA-NO-1-2-6,
              BYAFILE.BYA_AMT_1_2_6 = :QBYA-AMT-1-2-6,
              BYAFILE.BYA_O2_1_2_6 = :QBYA-O2-1-2-6,
              BYAFILE.BYA_INT_1_2_6 = :QBYA-INT-1-2-6,
              BYAFILE.BYA_NO_1_2_7 = :QBYA-NO-1-2-7,
              BYAFILE.BYA_AMT_1_2_7 = :QBYA-AMT-1-2-7,
              BYAFILE.BYA_O2_1_2_7 = :QBYA-O2-1-2-7,
              BYAFILE.BYA_INT_1_2_7 = :QBYA-INT-1-2-7,
              BYAFILE.BYA_NO_1_2_8 = :QBYA-NO-1-2-8,
              BYAFILE.BYA_AMT_1_2_8 = :QBYA-AMT-1-2-8,
              BYAFILE.BYA_O2_1_2_8 = :QBYA-O2-1-2-8,
              BYAFILE.BYA_INT_1_2_8 = :QBYA-INT-1-2-8,
              BYAFILE.BYA_NO_1_2_9 = :QBYA-NO-1-2-9,
              BYAFILE.BYA_AMT_1_2_9 = :QBYA-AMT-1-2-9,
              BYAFILE.BYA_O2_1_2_9 = :QBYA-O2-1-2-9,
              BYAFILE.BYA_INT_1_2_9 = :QBYA-INT-1-2-9,
              BYAFILE.BYA_NO_1_2_10 = :QBYA-NO-1-2-10,
              BYAFILE.BYA_AMT_1_2_10 = :QBYA-AMT-1-2-10,
              BYAFILE.BYA_O2_1_2_10 = :QBYA-O2-1-2-10,
              BYAFILE.BYA_INT_1_2_10 = :QBYA-INT-1-2-10,
              BYAFILE.BYA_NO_1_2_11 = :QBYA-NO-1-2-11,
              BYAFILE.BYA_AMT_1_2_11 = :QBYA-AMT-1-2-11,
              BYAFILE.BYA_O2_1_2_11 = :QBYA-O2-1-2-11,
              BYAFILE.BYA_INT_1_2_11 = :QBYA-INT-1-2-11,
              BYAFILE.BYA_NO_2_1_1 = :QBYA-NO-2-1-1,
              BYAFILE.BYA_AMT_2_1_1 = :QBYA-AMT-2-1-1,
              BYAFILE.BYA_O2_2_1_1 = :QBYA-O2-2-1-1,
              BYAFILE.BYA_INT_2_1_1 = :QBYA-INT-2-1-1,
              BYAFILE.BYA_NO_2_1_2 = :QBYA-NO-2-1-2,
              BYAFILE.BYA_AMT_2_1_2 = :QBYA-AMT-2-1-2,
              BYAFILE.BYA_O2_2_1_2 = :QBYA-O2-2-1-2,
              BYAFILE.BYA_INT_2_1_2 = :QBYA-INT-2-1-2,
              BYAFILE.BYA_NO_2_1_3 = :QBYA-NO-2-1-3,
              BYAFILE.BYA_AMT_2_1_3 = :QBYA-AMT-2-1-3,
              BYAFILE.BYA_O2_2_1_3 = :QBYA-O2-2-1-3,
              BYAFILE.BYA_INT_2_1_3 = :QBYA-INT-2-1-3,
              BYAFILE.BYA_NO_2_1_4 = :QBYA-NO-2-1-4,
              BYAFILE.BYA_AMT_2_1_4 = :QBYA-AMT-2-1-4,
              BYAFILE.BYA_O2_2_1_4 = :QBYA-O2-2-1-4,
              BYAFILE.BYA_INT_2_1_4 = :QBYA-INT-2-1-4,
              BYAFILE.BYA_NO_2_1_5 = :QBYA-NO-2-1-5,
              BYAFILE.BYA_AMT_2_1_5 = :QBYA-AMT-2-1-5,
              BYAFILE.BYA_O2_2_1_5 = :QBYA-O2-2-1-5,
              BYAFILE.BYA_INT_2_1_5 = :QBYA-INT-2-1-5,
              BYAFILE.BYA_NO_2_1_6 = :QBYA-NO-2-1-6,
              BYAFILE.BYA_AMT_2_1_6 = :QBYA-AMT-2-1-6,
              BYAFILE.BYA_O2_2_1_6 = :QBYA-O2-2-1-6,
              BYAFILE.BYA_INT_2_1_6 = :QBYA-INT-2-1-6,
              BYAFILE.BYA_NO_2_1_7 = :QBYA-NO-2-1-7,
              BYAFILE.BYA_AMT_2_1_7 = :QBYA-AMT-2-1-7,
              BYAFILE.BYA_O2_2_1_7 = :QBYA-O2-2-1-7,
              BYAFILE.BYA_INT_2_1_7 = :QBYA-INT-2-1-7,
              BYAFILE.BYA_NO_2_1_8 = :QBYA-NO-2-1-8,
              BYAFILE.BYA_AMT_2_1_8 = :QBYA-AMT-2-1-8,
              BYAFILE.BYA_O2_2_1_8 = :QBYA-O2-2-1-8,
              BYAFILE.BYA_INT_2_1_8 = :QBYA-INT-2-1-8,
              BYAFILE.BYA_NO_2_1_9 = :QBYA-NO-2-1-9,
              BYAFILE.BYA_AMT_2_1_9 = :QBYA-AMT-2-1-9,
              BYAFILE.BYA_O2_2_1_9 = :QBYA-O2-2-1-9,
              BYAFILE.BYA_INT_2_1_9 = :QBYA-INT-2-1-9,
              BYAFILE.BYA_NO_2_1_10 = :QBYA-NO-2-1-10,
              BYAFILE.BYA_AMT_2_1_10 = :QBYA-AMT-2-1-10,
              BYAFILE.BYA_O2_2_1_10 = :QBYA-O2-2-1-10,
              BYAFILE.BYA_INT_2_1_10 = :QBYA-INT-2-1-10,
              BYAFILE.BYA_NO_2_1_11 = :QBYA-NO-2-1-11,
              BYAFILE.BYA_AMT_2_1_11 = :QBYA-AMT-2-1-11,
              BYAFILE.BYA_O2_2_1_11 = :QBYA-O2-2-1-11,
              BYAFILE.BYA_INT_2_1_11 = :QBYA-INT-2-1-11,
              BYAFILE.BYA_NO_2_2_1 = :QBYA-NO-2-2-1,
              BYAFILE.BYA_AMT_2_2_1 = :QBYA-AMT-2-2-1,
              BYAFILE.BYA_O2_2_2_1 = :QBYA-O2-2-2-1,
              BYAFILE.BYA_INT_2_2_1 = :QBYA-INT-2-2-1,
              BYAFILE.BYA_NO_2_2_2 = :QBYA-NO-2-2-2,
              BYAFILE.BYA_AMT_2_2_2 = :QBYA-AMT-2-2-2,
              BYAFILE.BYA_O2_2_2_2 = :QBYA-O2-2-2-2,
              BYAFILE.BYA_INT_2_2_2 = :QBYA-INT-2-2-2,
              BYAFILE.BYA_NO_2_2_3 = :QBYA-NO-2-2-3,
              BYAFILE.BYA_AMT_2_2_3 = :QBYA-AMT-2-2-3,
              BYAFILE.BYA_O2_2_2_3 = :QBYA-O2-2-2-3,
              BYAFILE.BYA_INT_2_2_3 = :QBYA-INT-2-2-3,
              BYAFILE.BYA_NO_2_2_4 = :QBYA-NO-2-2-4,
              BYAFILE.BYA_AMT_2_2_4 = :QBYA-AMT-2-2-4,
              BYAFILE.BYA_O2_2_2_4 = :QBYA-O2-2-2-4,
              BYAFILE.BYA_INT_2_2_4 = :QBYA-INT-2-2-4,
              BYAFILE.BYA_NO_2_2_5 = :QBYA-NO-2-2-5,
              BYAFILE.BYA_AMT_2_2_5 = :QBYA-AMT-2-2-5,
              BYAFILE.BYA_O2_2_2_5 = :QBYA-O2-2-2-5,
              BYAFILE.BYA_INT_2_2_5 = :QBYA-INT-2-2-5,
              BYAFILE.BYA_NO_2_2_6 = :QBYA-NO-2-2-6,
              BYAFILE.BYA_AMT_2_2_6 = :QBYA-AMT-2-2-6,
              BYAFILE.BYA_O2_2_2_6 = :QBYA-O2-2-2-6,
              BYAFILE.BYA_INT_2_2_6 = :QBYA-INT-2-2-6,
              BYAFILE.BYA_NO_2_2_7 = :QBYA-NO-2-2-7,
              BYAFILE.BYA_AMT_2_2_7 = :QBYA-AMT-2-2-7,
              BYAFILE.BYA_O2_2_2_7 = :QBYA-O2-2-2-7,
              BYAFILE.BYA_INT_2_2_7 = :QBYA-INT-2-2-7,
              BYAFILE.BYA_NO_2_2_8 = :QBYA-NO-2-2-8,
              BYAFILE.BYA_AMT_2_2_8 = :QBYA-AMT-2-2-8,
              BYAFILE.BYA_O2_2_2_8 = :QBYA-O2-2-2-8,
              BYAFILE.BYA_INT_2_2_8 = :QBYA-INT-2-2-8,
              BYAFILE.BYA_NO_2_2_9 = :QBYA-NO-2-2-9,
              BYAFILE.BYA_AMT_2_2_9 = :QBYA-AMT-2-2-9,
              BYAFILE.BYA_O2_2_2_9 = :QBYA-O2-2-2-9,
              BYAFILE.BYA_INT_2_2_9 = :QBYA-INT-2-2-9,
              BYAFILE.BYA_NO_2_2_10 = :QBYA-NO-2-2-10,
              BYAFILE.BYA_AMT_2_2_10 = :QBYA-AMT-2-2-10,
              BYAFILE.BYA_O2_2_2_10 = :QBYA-O2-2-2-10,
              BYAFILE.BYA_INT_2_2_10 = :QBYA-INT-2-2-10,
              BYAFILE.BYA_NO_2_2_11 = :QBYA-NO-2-2-11,
              BYAFILE.BYA_AMT_2_2_11 = :QBYA-AMT-2-2-11,
              BYAFILE.BYA_O2_2_2_11 = :QBYA-O2-2-2-11,
              BYAFILE.BYA_INT_2_2_11 = :QBYA-INT-2-2-11,
              BYAFILE.BYA_NO_3_1_1 = :QBYA-NO-3-1-1,
              BYAFILE.BYA_AMT_3_1_1 = :QBYA-AMT-3-1-1,
              BYAFILE.BYA_O2_3_1_1 = :QBYA-O2-3-1-1,
              BYAFILE.BYA_INT_3_1_1 = :QBYA-INT-3-1-1,
              BYAFILE.BYA_NO_3_1_2 = :QBYA-NO-3-1-2,
              BYAFILE.BYA_AMT_3_1_2 = :QBYA-AMT-3-1-2,
              BYAFILE.BYA_O2_3_1_2 = :QBYA-O2-3-1-2,
              BYAFILE.BYA_INT_3_1_2 = :QBYA-INT-3-1-2,
              BYAFILE.BYA_NO_3_1_3 = :QBYA-NO-3-1-3,
              BYAFILE.BYA_AMT_3_1_3 = :QBYA-AMT-3-1-3,
              BYAFILE.BYA_O2_3_1_3 = :QBYA-O2-3-1-3,
              BYAFILE.BYA_INT_3_1_3 = :QBYA-INT-3-1-3,
              BYAFILE.BYA_NO_3_1_4 = :QBYA-NO-3-1-4,
              BYAFILE.BYA_AMT_3_1_4 = :QBYA-AMT-3-1-4,
              BYAFILE.BYA_O2_3_1_4 = :QBYA-O2-3-1-4,
              BYAFILE.BYA_INT_3_1_4 = :QBYA-INT-3-1-4,
              BYAFILE.BYA_NO_3_1_5 = :QBYA-NO-3-1-5,
              BYAFILE.BYA_AMT_3_1_5 = :QBYA-AMT-3-1-5,
              BYAFILE.BYA_O2_3_1_5 = :QBYA-O2-3-1-5,
              BYAFILE.BYA_INT_3_1_5 = :QBYA-INT-3-1-5,
              BYAFILE.BYA_NO_3_1_6 = :QBYA-NO-3-1-6,
              BYAFILE.BYA_AMT_3_1_6 = :QBYA-AMT-3-1-6,
              BYAFILE.BYA_O2_3_1_6 = :QBYA-O2-3-1-6,
              BYAFILE.BYA_INT_3_1_6 = :QBYA-INT-3-1-6,
              BYAFILE.BYA_NO_3_1_7 = :QBYA-NO-3-1-7,
              BYAFILE.BYA_AMT_3_1_7 = :QBYA-AMT-3-1-7,
              BYAFILE.BYA_O2_3_1_7 = :QBYA-O2-3-1-7,
              BYAFILE.BYA_INT_3_1_7 = :QBYA-INT-3-1-7,
              BYAFILE.BYA_NO_3_1_8 = :QBYA-NO-3-1-8,
              BYAFILE.BYA_AMT_3_1_8 = :QBYA-AMT-3-1-8,
              BYAFILE.BYA_O2_3_1_8 = :QBYA-O2-3-1-8,
              BYAFILE.BYA_INT_3_1_8 = :QBYA-INT-3-1-8,
              BYAFILE.BYA_NO_3_1_9 = :QBYA-NO-3-1-9,
              BYAFILE.BYA_AMT_3_1_9 = :QBYA-AMT-3-1-9,
              BYAFILE.BYA_O2_3_1_9 = :QBYA-O2-3-1-9,
              BYAFILE.BYA_INT_3_1_9 = :QBYA-INT-3-1-9,
              BYAFILE.BYA_NO_3_1_10 = :QBYA-NO-3-1-10,
              BYAFILE.BYA_AMT_3_1_10 = :QBYA-AMT-3-1-10,
              BYAFILE.BYA_O2_3_1_10 = :QBYA-O2-3-1-10,
              BYAFILE.BYA_INT_3_1_10 = :QBYA-INT-3-1-10,
              BYAFILE.BYA_NO_3_1_11 = :QBYA-NO-3-1-11,
              BYAFILE.BYA_AMT_3_1_11 = :QBYA-AMT-3-1-11,
              BYAFILE.BYA_O2_3_1_11 = :QBYA-O2-3-1-11,
              BYAFILE.BYA_INT_3_1_11 = :QBYA-INT-3-1-11,
              BYAFILE.BYA_NO_3_2_1 = :QBYA-NO-3-2-1,
              BYAFILE.BYA_AMT_3_2_1 = :QBYA-AMT-3-2-1,
              BYAFILE.BYA_O2_3_2_1 = :QBYA-O2-3-2-1,
              BYAFILE.BYA_INT_3_2_1 = :QBYA-INT-3-2-1,
              BYAFILE.BYA_NO_3_2_2 = :QBYA-NO-3-2-2,
              BYAFILE.BYA_AMT_3_2_2 = :QBYA-AMT-3-2-2,
              BYAFILE.BYA_O2_3_2_2 = :QBYA-O2-3-2-2,
              BYAFILE.BYA_INT_3_2_2 = :QBYA-INT-3-2-2,
              BYAFILE.BYA_NO_3_2_3 = :QBYA-NO-3-2-3,
              BYAFILE.BYA_AMT_3_2_3 = :QBYA-AMT-3-2-3,
              BYAFILE.BYA_O2_3_2_3 = :QBYA-O2-3-2-3,
              BYAFILE.BYA_INT_3_2_3 = :QBYA-INT-3-2-3,
              BYAFILE.BYA_NO_3_2_4 = :QBYA-NO-3-2-4,
              BYAFILE.BYA_AMT_3_2_4 = :QBYA-AMT-3-2-4,
              BYAFILE.BYA_O2_3_2_4 = :QBYA-O2-3-2-4,
              BYAFILE.BYA_INT_3_2_4 = :QBYA-INT-3-2-4,
              BYAFILE.BYA_NO_3_2_5 = :QBYA-NO-3-2-5,
              BYAFILE.BYA_AMT_3_2_5 = :QBYA-AMT-3-2-5,
              BYAFILE.BYA_O2_3_2_5 = :QBYA-O2-3-2-5,
              BYAFILE.BYA_INT_3_2_5 = :QBYA-INT-3-2-5,
              BYAFILE.BYA_NO_3_2_6 = :QBYA-NO-3-2-6,
              BYAFILE.BYA_AMT_3_2_6 = :QBYA-AMT-3-2-6,
              BYAFILE.BYA_O2_3_2_6 = :QBYA-O2-3-2-6,
              BYAFILE.BYA_INT_3_2_6 = :QBYA-INT-3-2-6,
              BYAFILE.BYA_NO_3_2_7 = :QBYA-NO-3-2-7,
              BYAFILE.BYA_AMT_3_2_7 = :QBYA-AMT-3-2-7,
              BYAFILE.BYA_O2_3_2_7 = :QBYA-O2-3-2-7,
              BYAFILE.BYA_INT_3_2_7 = :QBYA-INT-3-2-7,
              BYAFILE.BYA_NO_3_2_8 = :QBYA-NO-3-2-8,
              BYAFILE.BYA_AMT_3_2_8 = :QBYA-AMT-3-2-8,
              BYAFILE.BYA_O2_3_2_8 = :QBYA-O2-3-2-8,
              BYAFILE.BYA_INT_3_2_8 = :QBYA-INT-3-2-8,
              BYAFILE.BYA_NO_3_2_9 = :QBYA-NO-3-2-9,
              BYAFILE.BYA_AMT_3_2_9 = :QBYA-AMT-3-2-9,
              BYAFILE.BYA_O2_3_2_9 = :QBYA-O2-3-2-9,
              BYAFILE.BYA_INT_3_2_9 = :QBYA-INT-3-2-9,
              BYAFILE.BYA_NO_3_2_10 = :QBYA-NO-3-2-10,
              BYAFILE.BYA_AMT_3_2_10 = :QBYA-AMT-3-2-10,
              BYAFILE.BYA_O2_3_2_10 = :QBYA-O2-3-2-10,
              BYAFILE.BYA_INT_3_2_10 = :QBYA-INT-3-2-10,
              BYAFILE.BYA_NO_3_2_11 = :QBYA-NO-3-2-11,
              BYAFILE.BYA_AMT_3_2_11 = :QBYA-AMT-3-2-11,
              BYAFILE.BYA_O2_3_2_11 = :QBYA-O2-3-2-11,
              BYAFILE.BYA_INT_3_2_11 = :QBYA-INT-3-2-11,
              BYAFILE.BYA_NO_REPO_1_1_1 = :QBYA-NO-REPO-1-1-1,
              BYAFILE.BYA_AMT_REPO_1_1_1 = :QBYA-AMT-REPO-1-1-1,
              BYAFILE.BYA_O2_REPO_1_1_1 = :QBYA-O2-REPO-1-1-1,
              BYAFILE.BYA_INT_REPO_1_1_1 = :QBYA-INT-REPO-1-1-1,
              BYAFILE.BYA_NO_REPO_1_1_2 = :QBYA-NO-REPO-1-1-2,
              BYAFILE.BYA_AMT_REPO_1_1_2 = :QBYA-AMT-REPO-1-1-2,
              BYAFILE.BYA_O2_REPO_1_1_2 = :QBYA-O2-REPO-1-1-2,
              BYAFILE.BYA_INT_REPO_1_1_2 = :QBYA-INT-REPO-1-1-2,
              BYAFILE.BYA_NO_REPO_1_1_3 = :QBYA-NO-REPO-1-1-3,
              BYAFILE.BYA_AMT_REPO_1_1_3 = :QBYA-AMT-REPO-1-1-3,
              BYAFILE.BYA_O2_REPO_1_1_3 = :QBYA-O2-REPO-1-1-3,
              BYAFILE.BYA_INT_REPO_1_1_3 = :QBYA-INT-REPO-1-1-3,
              BYAFILE.BYA_NO_REPO_1_1_4 = :QBYA-NO-REPO-1-1-4,
              BYAFILE.BYA_AMT_REPO_1_1_4 = :QBYA-AMT-REPO-1-1-4,
              BYAFILE.BYA_O2_REPO_1_1_4 = :QBYA-O2-REPO-1-1-4,
              BYAFILE.BYA_INT_REPO_1_1_4 = :QBYA-INT-REPO-1-1-4,
              BYAFILE.BYA_NO_REPO_1_1_5 = :QBYA-NO-REPO-1-1-5,
              BYAFILE.BYA_AMT_REPO_1_1_5 = :QBYA-AMT-REPO-1-1-5,
              BYAFILE.BYA_O2_REPO_1_1_5 = :QBYA-O2-REPO-1-1-5,
              BYAFILE.BYA_INT_REPO_1_1_5 = :QBYA-INT-REPO-1-1-5,
              BYAFILE.BYA_NO_REPO_1_2_1 = :QBYA-NO-REPO-1-2-1,
              BYAFILE.BYA_AMT_REPO_1_2_1 = :QBYA-AMT-REPO-1-2-1,
              BYAFILE.BYA_O2_REPO_1_2_1 = :QBYA-O2-REPO-1-2-1,
              BYAFILE.BYA_INT_REPO_1_2_1 = :QBYA-INT-REPO-1-2-1,
              BYAFILE.BYA_NO_REPO_1_2_2 = :QBYA-NO-REPO-1-2-2,
              BYAFILE.BYA_AMT_REPO_1_2_2 = :QBYA-AMT-REPO-1-2-2,
              BYAFILE.BYA_O2_REPO_1_2_2 = :QBYA-O2-REPO-1-2-2,
              BYAFILE.BYA_INT_REPO_1_2_2 = :QBYA-INT-REPO-1-2-2,
              BYAFILE.BYA_NO_REPO_1_2_3 = :QBYA-NO-REPO-1-2-3,
              BYAFILE.BYA_AMT_REPO_1_2_3 = :QBYA-AMT-REPO-1-2-3,
              BYAFILE.BYA_O2_REPO_1_2_3 = :QBYA-O2-REPO-1-2-3,
              BYAFILE.BYA_INT_REPO_1_2_3 = :QBYA-INT-REPO-1-2-3,
              BYAFILE.BYA_NO_REPO_1_2_4 = :QBYA-NO-REPO-1-2-4,
              BYAFILE.BYA_AMT_REPO_1_2_4 = :QBYA-AMT-REPO-1-2-4,
              BYAFILE.BYA_O2_REPO_1_2_4 = :QBYA-O2-REPO-1-2-4,
              BYAFILE.BYA_INT_REPO_1_2_4 = :QBYA-INT-REPO-1-2-4,
              BYAFILE.BYA_NO_REPO_1_2_5 = :QBYA-NO-REPO-1-2-5,
              BYAFILE.BYA_AMT_REPO_1_2_5 = :QBYA-AMT-REPO-1-2-5,
              BYAFILE.BYA_O2_REPO_1_2_5 = :QBYA-O2-REPO-1-2-5,
              BYAFILE.BYA_INT_REPO_1_2_5 = :QBYA-INT-REPO-1-2-5,
              BYAFILE.BYA_NO_REPO_2_1_1 = :QBYA-NO-REPO-2-1-1,
              BYAFILE.BYA_AMT_REPO_2_1_1 = :QBYA-AMT-REPO-2-1-1,
              BYAFILE.BYA_O2_REPO_2_1_1 = :QBYA-O2-REPO-2-1-1,
              BYAFILE.BYA_INT_REPO_2_1_1 = :QBYA-INT-REPO-2-1-1,
              BYAFILE.BYA_NO_REPO_2_1_2 = :QBYA-NO-REPO-2-1-2,
              BYAFILE.BYA_AMT_REPO_2_1_2 = :QBYA-AMT-REPO-2-1-2,
              BYAFILE.BYA_O2_REPO_2_1_2 = :QBYA-O2-REPO-2-1-2,
              BYAFILE.BYA_INT_REPO_2_1_2 = :QBYA-INT-REPO-2-1-2,
              BYAFILE.BYA_NO_REPO_2_1_3 = :QBYA-NO-REPO-2-1-3,
              BYAFILE.BYA_AMT_REPO_2_1_3 = :QBYA-AMT-REPO-2-1-3,
              BYAFILE.BYA_O2_REPO_2_1_3 = :QBYA-O2-REPO-2-1-3,
              BYAFILE.BYA_INT_REPO_2_1_3 = :QBYA-INT-REPO-2-1-3,
              BYAFILE.BYA_NO_REPO_2_1_4 = :QBYA-NO-REPO-2-1-4,
              BYAFILE.BYA_AMT_REPO_2_1_4 = :QBYA-AMT-REPO-2-1-4,
              BYAFILE.BYA_O2_REPO_2_1_4 = :QBYA-O2-REPO-2-1-4,
              BYAFILE.BYA_INT_REPO_2_1_4 = :QBYA-INT-REPO-2-1-4,
              BYAFILE.BYA_NO_REPO_2_1_5 = :QBYA-NO-REPO-2-1-5,
              BYAFILE.BYA_AMT_REPO_2_1_5 = :QBYA-AMT-REPO-2-1-5,
              BYAFILE.BYA_O2_REPO_2_1_5 = :QBYA-O2-REPO-2-1-5,
              BYAFILE.BYA_INT_REPO_2_1_5 = :QBYA-INT-REPO-2-1-5,
              BYAFILE.BYA_NO_REPO_2_2_1 = :QBYA-NO-REPO-2-2-1,
              BYAFILE.BYA_AMT_REPO_2_2_1 = :QBYA-AMT-REPO-2-2-1,
              BYAFILE.BYA_O2_REPO_2_2_1 = :QBYA-O2-REPO-2-2-1,
              BYAFILE.BYA_INT_REPO_2_2_1 = :QBYA-INT-REPO-2-2-1,
              BYAFILE.BYA_NO_REPO_2_2_2 = :QBYA-NO-REPO-2-2-2,
              BYAFILE.BYA_AMT_REPO_2_2_2 = :QBYA-AMT-REPO-2-2-2,
              BYAFILE.BYA_O2_REPO_2_2_2 = :QBYA-O2-REPO-2-2-2,
              BYAFILE.BYA_INT_REPO_2_2_2 = :QBYA-INT-REPO-2-2-2,
              BYAFILE.BYA_NO_REPO_2_2_3 = :QBYA-NO-REPO-2-2-3,
              BYAFILE.BYA_AMT_REPO_2_2_3 = :QBYA-AMT-REPO-2-2-3,
              BYAFILE.BYA_O2_REPO_2_2_3 = :QBYA-O2-REPO-2-2-3,
              BYAFILE.BYA_INT_REPO_2_2_3 = :QBYA-INT-REPO-2-2-3,
              BYAFILE.BYA_NO_REPO_2_2_4 = :QBYA-NO-REPO-2-2-4,
              BYAFILE.BYA_AMT_REPO_2_2_4 = :QBYA-AMT-REPO-2-2-4,
              BYAFILE.BYA_O2_REPO_2_2_4 = :QBYA-O2-REPO-2-2-4,
              BYAFILE.BYA_INT_REPO_2_2_4 = :QBYA-INT-REPO-2-2-4,
              BYAFILE.BYA_NO_REPO_2_2_5 = :QBYA-NO-REPO-2-2-5,
              BYAFILE.BYA_AMT_REPO_2_2_5 = :QBYA-AMT-REPO-2-2-5,
              BYAFILE.BYA_O2_REPO_2_2_5 = :QBYA-O2-REPO-2-2-5,
              BYAFILE.BYA_INT_REPO_2_2_5 = :QBYA-INT-REPO-2-2-5,
              BYAFILE.BYA_NO_REPO_3_1_1 = :QBYA-NO-REPO-3-1-1,
              BYAFILE.BYA_AMT_REPO_3_1_1 = :QBYA-AMT-REPO-3-1-1,
              BYAFILE.BYA_O2_REPO_3_1_1 = :QBYA-O2-REPO-3-1-1,
              BYAFILE.BYA_INT_REPO_3_1_1 = :QBYA-INT-REPO-3-1-1,
              BYAFILE.BYA_NO_REPO_3_1_2 = :QBYA-NO-REPO-3-1-2,
              BYAFILE.BYA_AMT_REPO_3_1_2 = :QBYA-AMT-REPO-3-1-2,
              BYAFILE.BYA_O2_REPO_3_1_2 = :QBYA-O2-REPO-3-1-2,
              BYAFILE.BYA_INT_REPO_3_1_2 = :QBYA-INT-REPO-3-1-2,
              BYAFILE.BYA_NO_REPO_3_1_3 = :QBYA-NO-REPO-3-1-3,
              BYAFILE.BYA_AMT_REPO_3_1_3 = :QBYA-AMT-REPO-3-1-3,
              BYAFILE.BYA_O2_REPO_3_1_3 = :QBYA-O2-REPO-3-1-3,
              BYAFILE.BYA_INT_REPO_3_1_3 = :QBYA-INT-REPO-3-1-3,
              BYAFILE.BYA_NO_REPO_3_1_4 = :QBYA-NO-REPO-3-1-4,
              BYAFILE.BYA_AMT_REPO_3_1_4 = :QBYA-AMT-REPO-3-1-4,
              BYAFILE.BYA_O2_REPO_3_1_4 = :QBYA-O2-REPO-3-1-4,
              BYAFILE.BYA_INT_REPO_3_1_4 = :QBYA-INT-REPO-3-1-4,
              BYAFILE.BYA_NO_REPO_3_1_5 = :QBYA-NO-REPO-3-1-5,
              BYAFILE.BYA_AMT_REPO_3_1_5 = :QBYA-AMT-REPO-3-1-5,
              BYAFILE.BYA_O2_REPO_3_1_5 = :QBYA-O2-REPO-3-1-5,
              BYAFILE.BYA_INT_REPO_3_1_5 = :QBYA-INT-REPO-3-1-5,
              BYAFILE.BYA_NO_REPO_3_2_1 = :QBYA-NO-REPO-3-2-1,
              BYAFILE.BYA_AMT_REPO_3_2_1 = :QBYA-AMT-REPO-3-2-1,
              BYAFILE.BYA_O2_REPO_3_2_1 = :QBYA-O2-REPO-3-2-1,
              BYAFILE.BYA_INT_REPO_3_2_1 = :QBYA-INT-REPO-3-2-1,
              BYAFILE.BYA_NO_REPO_3_2_2 = :QBYA-NO-REPO-3-2-2,
              BYAFILE.BYA_AMT_REPO_3_2_2 = :QBYA-AMT-REPO-3-2-2,
              BYAFILE.BYA_O2_REPO_3_2_2 = :QBYA-O2-REPO-3-2-2,
              BYAFILE.BYA_INT_REPO_3_2_2 = :QBYA-INT-REPO-3-2-2,
              BYAFILE.BYA_NO_REPO_3_2_3 = :QBYA-NO-REPO-3-2-3,
              BYAFILE.BYA_AMT_REPO_3_2_3 = :QBYA-AMT-REPO-3-2-3,
              BYAFILE.BYA_O2_REPO_3_2_3 = :QBYA-O2-REPO-3-2-3,
              BYAFILE.BYA_INT_REPO_3_2_3 = :QBYA-INT-REPO-3-2-3,
              BYAFILE.BYA_NO_REPO_3_2_4 = :QBYA-NO-REPO-3-2-4,
              BYAFILE.BYA_AMT_REPO_3_2_4 = :QBYA-AMT-REPO-3-2-4,
              BYAFILE.BYA_O2_REPO_3_2_4 = :QBYA-O2-REPO-3-2-4,
              BYAFILE.BYA_INT_REPO_3_2_4 = :QBYA-INT-REPO-3-2-4,
              BYAFILE.BYA_NO_REPO_3_2_5 = :QBYA-NO-REPO-3-2-5,
              BYAFILE.BYA_AMT_REPO_3_2_5 = :QBYA-AMT-REPO-3-2-5,
              BYAFILE.BYA_O2_REPO_3_2_5 = :QBYA-O2-REPO-3-2-5,
              BYAFILE.BYA_INT_REPO_3_2_5 = :QBYA-INT-REPO-3-2-5,
              BYAFILE.BYA_NO_PL_1_1_1 = :QBYA-NO-PL-1-1-1,
              BYAFILE.BYA_AMT_PL_1_1_1 = :QBYA-AMT-PL-1-1-1,
              BYAFILE.BYA_O2_PL_1_1_1 = :QBYA-O2-PL-1-1-1,
              BYAFILE.BYA_INT_PL_1_1_1 = :QBYA-INT-PL-1-1-1,
              BYAFILE.BYA_NO_PL_1_1_2 = :QBYA-NO-PL-1-1-2,
              BYAFILE.BYA_AMT_PL_1_1_2 = :QBYA-AMT-PL-1-1-2,
              BYAFILE.BYA_O2_PL_1_1_2 = :QBYA-O2-PL-1-1-2,
              BYAFILE.BYA_INT_PL_1_1_2 = :QBYA-INT-PL-1-1-2,
              BYAFILE.BYA_NO_PL_1_1_3 = :QBYA-NO-PL-1-1-3,
              BYAFILE.BYA_AMT_PL_1_1_3 = :QBYA-AMT-PL-1-1-3,
              BYAFILE.BYA_O2_PL_1_1_3 = :QBYA-O2-PL-1-1-3,
              BYAFILE.BYA_INT_PL_1_1_3 = :QBYA-INT-PL-1-1-3,
              BYAFILE.BYA_NO_PL_1_1_4 = :QBYA-NO-PL-1-1-4,
              BYAFILE.BYA_AMT_PL_1_1_4 = :QBYA-AMT-PL-1-1-4,
              BYAFILE.BYA_O2_PL_1_1_4 = :QBYA-O2-PL-1-1-4,
              BYAFILE.BYA_INT_PL_1_1_4 = :QBYA-INT-PL-1-1-4,
              BYAFILE.BYA_NO_PL_1_1_5 = :QBYA-NO-PL-1-1-5,
              BYAFILE.BYA_AMT_PL_1_1_5 = :QBYA-AMT-PL-1-1-5,
              BYAFILE.BYA_O2_PL_1_1_5 = :QBYA-O2-PL-1-1-5,
              BYAFILE.BYA_INT_PL_1_1_5 = :QBYA-INT-PL-1-1-5,
              BYAFILE.BYA_NO_PL_1_1_6 = :QBYA-NO-PL-1-1-6,
              BYAFILE.BYA_AMT_PL_1_1_6 = :QBYA-AMT-PL-1-1-6,
              BYAFILE.BYA_O2_PL_1_1_6 = :QBYA-O2-PL-1-1-6,
              BYAFILE.BYA_INT_PL_1_1_6 = :QBYA-INT-PL-1-1-6,
              BYAFILE.BYA_NO_PL_1_1_7 = :QBYA-NO-PL-1-1-7,
              BYAFILE.BYA_AMT_PL_1_1_7 = :QBYA-AMT-PL-1-1-7,
              BYAFILE.BYA_O2_PL_1_1_7 = :QBYA-O2-PL-1-1-7,
              BYAFILE.BYA_INT_PL_1_1_7 = :QBYA-INT-PL-1-1-7,
              BYAFILE.BYA_NO_PL_1_1_8 = :QBYA-NO-PL-1-1-8,
              BYAFILE.BYA_AMT_PL_1_1_8 = :QBYA-AMT-PL-1-1-8,
              BYAFILE.BYA_O2_PL_1_1_8 = :QBYA-O2-PL-1-1-8,
              BYAFILE.BYA_INT_PL_1_1_8 = :QBYA-INT-PL-1-1-8,
              BYAFILE.BYA_NO_PL_1_1_9 = :QBYA-NO-PL-1-1-9,
              BYAFILE.BYA_AMT_PL_1_1_9 = :QBYA-AMT-PL-1-1-9,
              BYAFILE.BYA_O2_PL_1_1_9 = :QBYA-O2-PL-1-1-9,
              BYAFILE.BYA_INT_PL_1_1_9 = :QBYA-INT-PL-1-1-9,
              BYAFILE.BYA_NO_PL_1_1_10 = :QBYA-NO-PL-1-1-10,
              BYAFILE.BYA_AMT_PL_1_1_10 = :QBYA-AMT-PL-1-1-10,
              BYAFILE.BYA_O2_PL_1_1_10 = :QBYA-O2-PL-1-1-10,
              BYAFILE.BYA_INT_PL_1_1_10 = :QBYA-INT-PL-1-1-10,
              BYAFILE.BYA_NO_PL_1_2_1 = :QBYA-NO-PL-1-2-1,
              BYAFILE.BYA_AMT_PL_1_2_1 = :QBYA-AMT-PL-1-2-1,
              BYAFILE.BYA_O2_PL_1_2_1 = :QBYA-O2-PL-1-2-1,
              BYAFILE.BYA_INT_PL_1_2_1 = :QBYA-INT-PL-1-2-1,
              BYAFILE.BYA_NO_PL_1_2_2 = :QBYA-NO-PL-1-2-2,
              BYAFILE.BYA_AMT_PL_1_2_2 = :QBYA-AMT-PL-1-2-2,
              BYAFILE.BYA_O2_PL_1_2_2 = :QBYA-O2-PL-1-2-2,
              BYAFILE.BYA_INT_PL_1_2_2 = :QBYA-INT-PL-1-2-2,
              BYAFILE.BYA_NO_PL_1_2_3 = :QBYA-NO-PL-1-2-3,
              BYAFILE.BYA_AMT_PL_1_2_3 = :QBYA-AMT-PL-1-2-3,
              BYAFILE.BYA_O2_PL_1_2_3 = :QBYA-O2-PL-1-2-3,
              BYAFILE.BYA_INT_PL_1_2_3 = :QBYA-INT-PL-1-2-3,
              BYAFILE.BYA_NO_PL_1_2_4 = :QBYA-NO-PL-1-2-4,
              BYAFILE.BYA_AMT_PL_1_2_4 = :QBYA-AMT-PL-1-2-4,
              BYAFILE.BYA_O2_PL_1_2_4 = :QBYA-O2-PL-1-2-4,
              BYAFILE.BYA_INT_PL_1_2_4 = :QBYA-INT-PL-1-2-4,
              BYAFILE.BYA_NO_PL_1_2_5 = :QBYA-NO-PL-1-2-5,
              BYAFILE.BYA_AMT_PL_1_2_5 = :QBYA-AMT-PL-1-2-5,
              BYAFILE.BYA_O2_PL_1_2_5 = :QBYA-O2-PL-1-2-5,
              BYAFILE.BYA_INT_PL_1_2_5 = :QBYA-INT-PL-1-2-5,
              BYAFILE.BYA_NO_PL_1_2_6 = :QBYA-NO-PL-1-2-6,
              BYAFILE.BYA_AMT_PL_1_2_6 = :QBYA-AMT-PL-1-2-6,
              BYAFILE.BYA_O2_PL_1_2_6 = :QBYA-O2-PL-1-2-6,
              BYAFILE.BYA_INT_PL_1_2_6 = :QBYA-INT-PL-1-2-6,
              BYAFILE.BYA_NO_PL_1_2_7 = :QBYA-NO-PL-1-2-7,
              BYAFILE.BYA_AMT_PL_1_2_7 = :QBYA-AMT-PL-1-2-7,
              BYAFILE.BYA_O2_PL_1_2_7 = :QBYA-O2-PL-1-2-7,
              BYAFILE.BYA_INT_PL_1_2_7 = :QBYA-INT-PL-1-2-7,
              BYAFILE.BYA_NO_PL_1_2_8 = :QBYA-NO-PL-1-2-8,
              BYAFILE.BYA_AMT_PL_1_2_8 = :QBYA-AMT-PL-1-2-8,
              BYAFILE.BYA_O2_PL_1_2_8 = :QBYA-O2-PL-1-2-8,
              BYAFILE.BYA_INT_PL_1_2_8 = :QBYA-INT-PL-1-2-8,
              BYAFILE.BYA_NO_PL_1_2_9 = :QBYA-NO-PL-1-2-9,
              BYAFILE.BYA_AMT_PL_1_2_9 = :QBYA-AMT-PL-1-2-9,
              BYAFILE.BYA_O2_PL_1_2_9 = :QBYA-O2-PL-1-2-9,
              BYAFILE.BYA_INT_PL_1_2_9 = :QBYA-INT-PL-1-2-9,
              BYAFILE.BYA_NO_PL_1_2_10 = :QBYA-NO-PL-1-2-10,
              BYAFILE.BYA_AMT_PL_1_2_10 = :QBYA-AMT-PL-1-2-10,
              BYAFILE.BYA_O2_PL_1_2_10 = :QBYA-O2-PL-1-2-10,
              BYAFILE.BYA_INT_PL_1_2_10 = :QBYA-INT-PL-1-2-10,
              BYAFILE.BYA_NO_PL_2_1_1 = :QBYA-NO-PL-2-1-1,
              BYAFILE.BYA_AMT_PL_2_1_1 = :QBYA-AMT-PL-2-1-1,
              BYAFILE.BYA_O2_PL_2_1_1 = :QBYA-O2-PL-2-1-1,
              BYAFILE.BYA_INT_PL_2_1_1 = :QBYA-INT-PL-2-1-1,
              BYAFILE.BYA_NO_PL_2_1_2 = :QBYA-NO-PL-2-1-2,
              BYAFILE.BYA_AMT_PL_2_1_2 = :QBYA-AMT-PL-2-1-2,
              BYAFILE.BYA_O2_PL_2_1_2 = :QBYA-O2-PL-2-1-2,
              BYAFILE.BYA_INT_PL_2_1_2 = :QBYA-INT-PL-2-1-2,
              BYAFILE.BYA_NO_PL_2_1_3 = :QBYA-NO-PL-2-1-3,
              BYAFILE.BYA_AMT_PL_2_1_3 = :QBYA-AMT-PL-2-1-3,
              BYAFILE.BYA_O2_PL_2_1_3 = :QBYA-O2-PL-2-1-3,
              BYAFILE.BYA_INT_PL_2_1_3 = :QBYA-INT-PL-2-1-3,
              BYAFILE.BYA_NO_PL_2_1_4 = :QBYA-NO-PL-2-1-4,
              BYAFILE.BYA_AMT_PL_2_1_4 = :QBYA-AMT-PL-2-1-4,
              BYAFILE.BYA_O2_PL_2_1_4 = :QBYA-O2-PL-2-1-4,
              BYAFILE.BYA_INT_PL_2_1_4 = :QBYA-INT-PL-2-1-4,
              BYAFILE.BYA_NO_PL_2_1_5 = :QBYA-NO-PL-2-1-5,
              BYAFILE.BYA_AMT_PL_2_1_5 = :QBYA-AMT-PL-2-1-5,
              BYAFILE.BYA_O2_PL_2_1_5 = :QBYA-O2-PL-2-1-5,
              BYAFILE.BYA_INT_PL_2_1_5 = :QBYA-INT-PL-2-1-5,
              BYAFILE.BYA_NO_PL_2_1_6 = :QBYA-NO-PL-2-1-6,
              BYAFILE.BYA_AMT_PL_2_1_6 = :QBYA-AMT-PL-2-1-6,
              BYAFILE.BYA_O2_PL_2_1_6 = :QBYA-O2-PL-2-1-6,
              BYAFILE.BYA_INT_PL_2_1_6 = :QBYA-INT-PL-2-1-6,
              BYAFILE.BYA_NO_PL_2_1_7 = :QBYA-NO-PL-2-1-7,
              BYAFILE.BYA_AMT_PL_2_1_7 = :QBYA-AMT-PL-2-1-7,
              BYAFILE.BYA_O2_PL_2_1_7 = :QBYA-O2-PL-2-1-7,
              BYAFILE.BYA_INT_PL_2_1_7 = :QBYA-INT-PL-2-1-7,
              BYAFILE.BYA_NO_PL_2_1_8 = :QBYA-NO-PL-2-1-8,
              BYAFILE.BYA_AMT_PL_2_1_8 = :QBYA-AMT-PL-2-1-8,
              BYAFILE.BYA_O2_PL_2_1_8 = :QBYA-O2-PL-2-1-8,
              BYAFILE.BYA_INT_PL_2_1_8 = :QBYA-INT-PL-2-1-8,
              BYAFILE.BYA_NO_PL_2_1_9 = :QBYA-NO-PL-2-1-9,
              BYAFILE.BYA_AMT_PL_2_1_9 = :QBYA-AMT-PL-2-1-9,
              BYAFILE.BYA_O2_PL_2_1_9 = :QBYA-O2-PL-2-1-9,
              BYAFILE.BYA_INT_PL_2_1_9 = :QBYA-INT-PL-2-1-9,
              BYAFILE.BYA_NO_PL_2_1_10 = :QBYA-NO-PL-2-1-10,
              BYAFILE.BYA_AMT_PL_2_1_10 = :QBYA-AMT-PL-2-1-10,
              BYAFILE.BYA_O2_PL_2_1_10 = :QBYA-O2-PL-2-1-10,
              BYAFILE.BYA_INT_PL_2_1_10 = :QBYA-INT-PL-2-1-10,
              BYAFILE.BYA_NO_PL_2_2_1 = :QBYA-NO-PL-2-2-1,
              BYAFILE.BYA_AMT_PL_2_2_1 = :QBYA-AMT-PL-2-2-1,
              BYAFILE.BYA_O2_PL_2_2_1 = :QBYA-O2-PL-2-2-1,
              BYAFILE.BYA_INT_PL_2_2_1 = :QBYA-INT-PL-2-2-1,
              BYAFILE.BYA_NO_PL_2_2_2 = :QBYA-NO-PL-2-2-2,
              BYAFILE.BYA_AMT_PL_2_2_2 = :QBYA-AMT-PL-2-2-2,
              BYAFILE.BYA_O2_PL_2_2_2 = :QBYA-O2-PL-2-2-2,
              BYAFILE.BYA_INT_PL_2_2_2 = :QBYA-INT-PL-2-2-2,
              BYAFILE.BYA_NO_PL_2_2_3 = :QBYA-NO-PL-2-2-3,
              BYAFILE.BYA_AMT_PL_2_2_3 = :QBYA-AMT-PL-2-2-3,
              BYAFILE.BYA_O2_PL_2_2_3 = :QBYA-O2-PL-2-2-3,
              BYAFILE.BYA_INT_PL_2_2_3 = :QBYA-INT-PL-2-2-3,
              BYAFILE.BYA_NO_PL_2_2_4 = :QBYA-NO-PL-2-2-4,
              BYAFILE.BYA_AMT_PL_2_2_4 = :QBYA-AMT-PL-2-2-4,
              BYAFILE.BYA_O2_PL_2_2_4 = :QBYA-O2-PL-2-2-4,
              BYAFILE.BYA_INT_PL_2_2_4 = :QBYA-INT-PL-2-2-4,
              BYAFILE.BYA_NO_PL_2_2_5 = :QBYA-NO-PL-2-2-5,
              BYAFILE.BYA_AMT_PL_2_2_5 = :QBYA-AMT-PL-2-2-5,
              BYAFILE.BYA_O2_PL_2_2_5 = :QBYA-O2-PL-2-2-5,
              BYAFILE.BYA_INT_PL_2_2_5 = :QBYA-INT-PL-2-2-5,
              BYAFILE.BYA_NO_PL_2_2_6 = :QBYA-NO-PL-2-2-6,
              BYAFILE.BYA_AMT_PL_2_2_6 = :QBYA-AMT-PL-2-2-6,
              BYAFILE.BYA_O2_PL_2_2_6 = :QBYA-O2-PL-2-2-6,
              BYAFILE.BYA_INT_PL_2_2_6 = :QBYA-INT-PL-2-2-6,
              BYAFILE.BYA_NO_PL_2_2_7 = :QBYA-NO-PL-2-2-7,
              BYAFILE.BYA_AMT_PL_2_2_7 = :QBYA-AMT-PL-2-2-7,
              BYAFILE.BYA_O2_PL_2_2_7 = :QBYA-O2-PL-2-2-7,
              BYAFILE.BYA_INT_PL_2_2_7 = :QBYA-INT-PL-2-2-7,
              BYAFILE.BYA_NO_PL_2_2_8 = :QBYA-NO-PL-2-2-8,
              BYAFILE.BYA_AMT_PL_2_2_8 = :QBYA-AMT-PL-2-2-8,
              BYAFILE.BYA_O2_PL_2_2_8 = :QBYA-O2-PL-2-2-8,
              BYAFILE.BYA_INT_PL_2_2_8 = :QBYA-INT-PL-2-2-8,
              BYAFILE.BYA_NO_PL_2_2_9 = :QBYA-NO-PL-2-2-9,
              BYAFILE.BYA_AMT_PL_2_2_9 = :QBYA-AMT-PL-2-2-9,
              BYAFILE.BYA_O2_PL_2_2_9 = :QBYA-O2-PL-2-2-9,
              BYAFILE.BYA_INT_PL_2_2_9 = :QBYA-INT-PL-2-2-9,
              BYAFILE.BYA_NO_PL_2_2_10 = :QBYA-NO-PL-2-2-10,
              BYAFILE.BYA_AMT_PL_2_2_10 = :QBYA-AMT-PL-2-2-10,
              BYAFILE.BYA_O2_PL_2_2_10 = :QBYA-O2-PL-2-2-10,
              BYAFILE.BYA_INT_PL_2_2_10 = :QBYA-INT-PL-2-2-10,
              BYAFILE.BYA_NO_PL_3_1_1 = :QBYA-NO-PL-3-1-1,
              BYAFILE.BYA_AMT_PL_3_1_1 = :QBYA-AMT-PL-3-1-1,
              BYAFILE.BYA_O2_PL_3_1_1 = :QBYA-O2-PL-3-1-1,
              BYAFILE.BYA_INT_PL_3_1_1 = :QBYA-INT-PL-3-1-1,
              BYAFILE.BYA_NO_PL_3_1_2 = :QBYA-NO-PL-3-1-2,
              BYAFILE.BYA_AMT_PL_3_1_2 = :QBYA-AMT-PL-3-1-2,
              BYAFILE.BYA_O2_PL_3_1_2 = :QBYA-O2-PL-3-1-2,
              BYAFILE.BYA_INT_PL_3_1_2 = :QBYA-INT-PL-3-1-2,
              BYAFILE.BYA_NO_PL_3_1_3 = :QBYA-NO-PL-3-1-3,
              BYAFILE.BYA_AMT_PL_3_1_3 = :QBYA-AMT-PL-3-1-3,
              BYAFILE.BYA_O2_PL_3_1_3 = :QBYA-O2-PL-3-1-3,
              BYAFILE.BYA_INT_PL_3_1_3 = :QBYA-INT-PL-3-1-3,
              BYAFILE.BYA_NO_PL_3_1_4 = :QBYA-NO-PL-3-1-4,
              BYAFILE.BYA_AMT_PL_3_1_4 = :QBYA-AMT-PL-3-1-4,
              BYAFILE.BYA_O2_PL_3_1_4 = :QBYA-O2-PL-3-1-4,
              BYAFILE.BYA_INT_PL_3_1_4 = :QBYA-INT-PL-3-1-4,
              BYAFILE.BYA_NO_PL_3_1_5 = :QBYA-NO-PL-3-1-5,
              BYAFILE.BYA_AMT_PL_3_1_5 = :QBYA-AMT-PL-3-1-5,
              BYAFILE.BYA_O2_PL_3_1_5 = :QBYA-O2-PL-3-1-5,
              BYAFILE.BYA_INT_PL_3_1_5 = :QBYA-INT-PL-3-1-5,
              BYAFILE.BYA_NO_PL_3_1_6 = :QBYA-NO-PL-3-1-6,
              BYAFILE.BYA_AMT_PL_3_1_6 = :QBYA-AMT-PL-3-1-6,
              BYAFILE.BYA_O2_PL_3_1_6 = :QBYA-O2-PL-3-1-6,
              BYAFILE.BYA_INT_PL_3_1_6 = :QBYA-INT-PL-3-1-6,
              BYAFILE.BYA_NO_PL_3_1_7 = :QBYA-NO-PL-3-1-7,
              BYAFILE.BYA_AMT_PL_3_1_7 = :QBYA-AMT-PL-3-1-7,
              BYAFILE.BYA_O2_PL_3_1_7 = :QBYA-O2-PL-3-1-7,
              BYAFILE.BYA_INT_PL_3_1_7 = :QBYA-INT-PL-3-1-7,
              BYAFILE.BYA_NO_PL_3_1_8 = :QBYA-NO-PL-3-1-8,
              BYAFILE.BYA_AMT_PL_3_1_8 = :QBYA-AMT-PL-3-1-8,
              BYAFILE.BYA_O2_PL_3_1_8 = :QBYA-O2-PL-3-1-8,
              BYAFILE.BYA_INT_PL_3_1_8 = :QBYA-INT-PL-3-1-8,
              BYAFILE.BYA_NO_PL_3_1_9 = :QBYA-NO-PL-3-1-9,
              BYAFILE.BYA_AMT_PL_3_1_9 = :QBYA-AMT-PL-3-1-9,
              BYAFILE.BYA_O2_PL_3_1_9 = :QBYA-O2-PL-3-1-9,
              BYAFILE.BYA_INT_PL_3_1_9 = :QBYA-INT-PL-3-1-9,
              BYAFILE.BYA_NO_PL_3_1_10 = :QBYA-NO-PL-3-1-10,
              BYAFILE.BYA_AMT_PL_3_1_10 = :QBYA-AMT-PL-3-1-10,
              BYAFILE.BYA_O2_PL_3_1_10 = :QBYA-O2-PL-3-1-10,
              BYAFILE.BYA_INT_PL_3_1_10 = :QBYA-INT-PL-3-1-10,
              BYAFILE.BYA_NO_PL_3_2_1 = :QBYA-NO-PL-3-2-1,
              BYAFILE.BYA_AMT_PL_3_2_1 = :QBYA-AMT-PL-3-2-1,
              BYAFILE.BYA_O2_PL_3_2_1 = :QBYA-O2-PL-3-2-1,
              BYAFILE.BYA_INT_PL_3_2_1 = :QBYA-INT-PL-3-2-1,
              BYAFILE.BYA_NO_PL_3_2_2 = :QBYA-NO-PL-3-2-2,
              BYAFILE.BYA_AMT_PL_3_2_2 = :QBYA-AMT-PL-3-2-2,
              BYAFILE.BYA_O2_PL_3_2_2 = :QBYA-O2-PL-3-2-2,
              BYAFILE.BYA_INT_PL_3_2_2 = :QBYA-INT-PL-3-2-2,
              BYAFILE.BYA_NO_PL_3_2_3 = :QBYA-NO-PL-3-2-3,
              BYAFILE.BYA_AMT_PL_3_2_3 = :QBYA-AMT-PL-3-2-3,
              BYAFILE.BYA_O2_PL_3_2_3 = :QBYA-O2-PL-3-2-3,
              BYAFILE.BYA_INT_PL_3_2_3 = :QBYA-INT-PL-3-2-3,
              BYAFILE.BYA_NO_PL_3_2_4 = :QBYA-NO-PL-3-2-4,
              BYAFILE.BYA_AMT_PL_3_2_4 = :QBYA-AMT-PL-3-2-4,
              BYAFILE.BYA_O2_PL_3_2_4 = :QBYA-O2-PL-3-2-4,
              BYAFILE.BYA_INT_PL_3_2_4 = :QBYA-INT-PL-3-2-4,
              BYAFILE.BYA_NO_PL_3_2_5 = :QBYA-NO-PL-3-2-5,
              BYAFILE.BYA_AMT_PL_3_2_5 = :QBYA-AMT-PL-3-2-5,
              BYAFILE.BYA_O2_PL_3_2_5 = :QBYA-O2-PL-3-2-5,
              BYAFILE.BYA_INT_PL_3_2_5 = :QBYA-INT-PL-3-2-5,
              BYAFILE.BYA_NO_PL_3_2_6 = :QBYA-NO-PL-3-2-6,
              BYAFILE.BYA_AMT_PL_3_2_6 = :QBYA-AMT-PL-3-2-6,
              BYAFILE.BYA_O2_PL_3_2_6 = :QBYA-O2-PL-3-2-6,
              BYAFILE.BYA_INT_PL_3_2_6 = :QBYA-INT-PL-3-2-6,
              BYAFILE.BYA_NO_PL_3_2_7 = :QBYA-NO-PL-3-2-7,
              BYAFILE.BYA_AMT_PL_3_2_7 = :QBYA-AMT-PL-3-2-7,
              BYAFILE.BYA_O2_PL_3_2_7 = :QBYA-O2-PL-3-2-7,
              BYAFILE.BYA_INT_PL_3_2_7 = :QBYA-INT-PL-3-2-7,
              BYAFILE.BYA_NO_PL_3_2_8 = :QBYA-NO-PL-3-2-8,
              BYAFILE.BYA_AMT_PL_3_2_8 = :QBYA-AMT-PL-3-2-8,
              BYAFILE.BYA_O2_PL_3_2_8 = :QBYA-O2-PL-3-2-8,
              BYAFILE.BYA_INT_PL_3_2_8 = :QBYA-INT-PL-3-2-8,
              BYAFILE.BYA_NO_PL_3_2_9 = :QBYA-NO-PL-3-2-9,
              BYAFILE.BYA_AMT_PL_3_2_9 = :QBYA-AMT-PL-3-2-9,
              BYAFILE.BYA_O2_PL_3_2_9 = :QBYA-O2-PL-3-2-9,
              BYAFILE.BYA_INT_PL_3_2_9 = :QBYA-INT-PL-3-2-9,
              BYAFILE.BYA_NO_PL_3_2_10 = :QBYA-NO-PL-3-2-10,
              BYAFILE.BYA_AMT_PL_3_2_10 = :QBYA-AMT-PL-3-2-10,
              BYAFILE.BYA_O2_PL_3_2_10 = :QBYA-O2-PL-3-2-10,
              BYAFILE.BYA_INT_PL_3_2_10 = :QBYA-INT-PL-3-2-10,
              BYAFILE.BYA_DLRVPRIN_1_1_1 = :QBYA-DLRVPRIN-1-1-1,
              BYAFILE.BYA_DLRVPRIN_1_1_2 = :QBYA-DLRVPRIN-1-1-2,
              BYAFILE.BYA_DLRVPRIN_1_1_3 = :QBYA-DLRVPRIN-1-1-3,
              BYAFILE.BYA_DLRVCHGS_1_1_1 = :QBYA-DLRVCHGS-1-1-1,
              BYAFILE.BYA_DLRVCHGS_1_1_2 = :QBYA-DLRVCHGS-1-1-2,
              BYAFILE.BYA_DLRVCHGS_1_1_3 = :QBYA-DLRVCHGS-1-1-3,
              BYAFILE.BYA_DLPARVHELD_1_1 = :QBYA-DLPARVHELD-1-1,
              BYAFILE.BYA_DLRVPRIN_1_2_1 = :QBYA-DLRVPRIN-1-2-1,
              BYAFILE.BYA_DLRVPRIN_1_2_2 = :QBYA-DLRVPRIN-1-2-2,
              BYAFILE.BYA_DLRVPRIN_1_2_3 = :QBYA-DLRVPRIN-1-2-3,
              BYAFILE.BYA_DLRVCHGS_1_2_1 = :QBYA-DLRVCHGS-1-2-1,
              BYAFILE.BYA_DLRVCHGS_1_2_2 = :QBYA-DLRVCHGS-1-2-2,
              BYAFILE.BYA_DLRVCHGS_1_2_3 = :QBYA-DLRVCHGS-1-2-3,
              BYAFILE.BYA_DLPARVHELD_1_2 = :QBYA-DLPARVHELD-1-2,
              BYAFILE.BYA_DLRVPRIN_2_1_1 = :QBYA-DLRVPRIN-2-1-1,
              BYAFILE.BYA_DLRVPRIN_2_1_2 = :QBYA-DLRVPRIN-2-1-2,
              BYAFILE.BYA_DLRVPRIN_2_1_3 = :QBYA-DLRVPRIN-2-1-3,
              BYAFILE.BYA_DLRVCHGS_2_1_1 = :QBYA-DLRVCHGS-2-1-1,
              BYAFILE.BYA_DLRVCHGS_2_1_2 = :QBYA-DLRVCHGS-2-1-2,
              BYAFILE.BYA_DLRVCHGS_2_1_3 = :QBYA-DLRVCHGS-2-1-3,
              BYAFILE.BYA_DLPARVHELD_2_1 = :QBYA-DLPARVHELD-2-1,
              BYAFILE.BYA_DLRVPRIN_2_2_1 = :QBYA-DLRVPRIN-2-2-1,
              BYAFILE.BYA_DLRVPRIN_2_2_2 = :QBYA-DLRVPRIN-2-2-2,
              BYAFILE.BYA_DLRVPRIN_2_2_3 = :QBYA-DLRVPRIN-2-2-3,
              BYAFILE.BYA_DLRVCHGS_2_2_1 = :QBYA-DLRVCHGS-2-2-1,
              BYAFILE.BYA_DLRVCHGS_2_2_2 = :QBYA-DLRVCHGS-2-2-2,
              BYAFILE.BYA_DLRVCHGS_2_2_3 = :QBYA-DLRVCHGS-2-2-3,
              BYAFILE.BYA_DLPARVHELD_2_2 = :QBYA-DLPARVHELD-2-2,
              BYAFILE.BYA_DLRVPRIN_3_1_1 = :QBYA-DLRVPRIN-3-1-1,
              BYAFILE.BYA_DLRVPRIN_3_1_2 = :QBYA-DLRVPRIN-3-1-2,
              BYAFILE.BYA_DLRVPRIN_3_1_3 = :QBYA-DLRVPRIN-3-1-3,
              BYAFILE.BYA_DLRVCHGS_3_1_1 = :QBYA-DLRVCHGS-3-1-1,
              BYAFILE.BYA_DLRVCHGS_3_1_2 = :QBYA-DLRVCHGS-3-1-2,
              BYAFILE.BYA_DLRVCHGS_3_1_3 = :QBYA-DLRVCHGS-3-1-3,
              BYAFILE.BYA_DLPARVHELD_3_1 = :QBYA-DLPARVHELD-3-1,
              BYAFILE.BYA_DLRVPRIN_3_2_1 = :QBYA-DLRVPRIN-3-2-1,
              BYAFILE.BYA_DLRVPRIN_3_2_2 = :QBYA-DLRVPRIN-3-2-2,
              BYAFILE.BYA_DLRVPRIN_3_2_3 = :QBYA-DLRVPRIN-3-2-3,
              BYAFILE.BYA_DLRVCHGS_3_2_1 = :QBYA-DLRVCHGS-3-2-1,
              BYAFILE.BYA_DLRVCHGS_3_2_2 = :QBYA-DLRVCHGS-3-2-2,
              BYAFILE.BYA_DLRVCHGS_3_2_3 = :QBYA-DLRVCHGS-3-2-3,
              BYAFILE.BYA_DLPARVHELD_3_2 = :QBYA-DLPARVHELD-3-2
             WHERE BYAFILE.BYA_DATE_CCYYMM = :QBYA-DATE-CCYYMM
               AND BYAFILE.BYA_BRNO = :QBYA-BRNO 
               AND BYAFILE.BYA_CLASS = :QBYA-CLASS
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-BYA1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

           MOVE  '1' TO SQL-UPDATE-CONN-USE-SW. *> USE C1 CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

      *-----------------------------------------------------------------
       DELETE-BYA1-FILE. *> EMBEDDED - BYAFILE

           PERFORM DELETE-IT.
           MOVE BYA-PATH-SQL TO E-FILE.
           MOVE BYA1-KEY TO E-KEYX.
           PERFORM SET-BYA-FIELDS.

           EXEC SQL
            DELETE FROM DBO.BYAFILE
             WHERE BYAFILE.BYA_DATE_CCYYMM = :QBYA-DATE-CCYYMM
               AND BYAFILE.BYA_BRNO = :QBYA-BRNO 
               AND BYAFILE.BYA_CLASS = :QBYA-CLASS
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-BYA1-FILE.

      *-----------------------------------------------------------------
       CLOSE-BYA1-FILE.
           IF ( BYA1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE BYA1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO BYA1-CURSOR-STAT.

      *=================================================================
       LOAD-BYB1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( BY-PATH-OVERRIDE = "Y" )
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( BY-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO BYB-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO BYB-ALL-MACHINE
              MOVE BYB-ALL-PATH            TO BYB-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( BY-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO BYB-ALL-BASE
              MOVE FILPATH-MACHINE         TO BYB-ALL-MACHINE
              MOVE BYB-ALL-PATH            TO BYB-PATH.

      *-----------------------------------------------------------------
       OPEN-BYB1-FILE.
           PERFORM LOAD-BYB1-FILE.
           PERFORM OPEN-IT.
           MOVE BYB-PATH-SQL TO E-FILE.

      *-----------------------------------------------------------------
       READ-BYB1-FILE. *> EMBEDDED - BYBFILE
           PERFORM READ-IT.
           MOVE BYB-PATH-SQL TO E-FILE.
           MOVE BYB1-KEY TO E-KEYX.
           INITIALIZE QBYB-REC.

           MOVE BY-DATE-CCYYMM      TO QBYB-DATE-CCYYMM.
           MOVE BY-BRNO             TO QBYB-BRNO.
           MOVE BY-CLASS            TO QBYB-CLASS.

           EXEC SQL
            SELECT BYBFILE.BYB_DATE_CCYYMM,
                   BYBFILE.BYB_BRNO,
                   BYBFILE.BYB_CLASS,
                   BYBFILE.BYB_PL_PRINCOL_1_1,
                   BYBFILE.BYB_PL_INTCOL_1_1,
                   BYBFILE.BYB_PL_LATECOL_1_1,
                   BYBFILE.BYB_PL_O2COL_1_1,
                   BYBFILE.BYB_PL_PRINCOL_1_2,
                   BYBFILE.BYB_PL_INTCOL_1_2,
                   BYBFILE.BYB_PL_LATECOL_1_2,
                   BYBFILE.BYB_PL_O2COL_1_2,
                   BYBFILE.BYB_PL_PRINCOL_1_3,
                   BYBFILE.BYB_PL_INTCOL_1_3,
                   BYBFILE.BYB_PL_LATECOL_1_3,
                   BYBFILE.BYB_PL_O2COL_1_3,
                   BYBFILE.BYB_PL_PRINCOL_1_4,
                   BYBFILE.BYB_PL_INTCOL_1_4,
                   BYBFILE.BYB_PL_LATECOL_1_4,
                   BYBFILE.BYB_PL_O2COL_1_4,
                   BYBFILE.BYB_PL_PRINCOL_1_5,
                   BYBFILE.BYB_PL_INTCOL_1_5,
                   BYBFILE.BYB_PL_LATECOL_1_5,
                   BYBFILE.BYB_PL_O2COL_1_5,
                   BYBFILE.BYB_AMT_SOLD_1,
                   BYBFILE.BYB_AMT_TRANS_1,
                   BYBFILE.BYB_AMT_PURCH_1,
                   BYBFILE.BYB_NO_SOLD_1,
                   BYBFILE.BYB_NO_TRANS_1,
                   BYBFILE.BYB_NO_PURCH_1,
                   BYBFILE.BYB_OTHCOL_1,
                   BYBFILE.BYB_OT2COL_1,
                   BYBFILE.BYB_INTCOL_1,
                   BYBFILE.BYB_LATECOL_1,
                   BYBFILE.BYB_PRINCOL_1,
                   BYBFILE.BYB_DEFCOL_1,
                   BYBFILE.BYB_RNPRIN_1,
                   BYBFILE.BYB_INTREB_1,
                   BYBFILE.BYB_CLREB1_1,
                   BYBFILE.BYB_CLREB2_1,
                   BYBFILE.BYB_CLREB3_1,
                   BYBFILE.BYB_CLREB4_1,
                   BYBFILE.BYB_AHREB_1,
                   BYBFILE.BYB_PPREB_1,
                   BYBFILE.BYB_O1REB_1,
                   BYBFILE.BYB_O2REB_1,
                   BYBFILE.BYB_O3REB_1,
                   BYBFILE.BYB_O4REB_1,
                   BYBFILE.BYB_O5REB_1,
                   BYBFILE.BYB_O6REB_1,
                   BYBFILE.BYB_O7REB_1,
                   BYBFILE.BYB_SCREB_1,
                   BYBFILE.BYB_MFREB_1,
                   BYBFILE.BYB_DFREB_1,
                   BYBFILE.BYB_UN7_REB_1,
                   BYBFILE.BYB_UN8_REB_1,
                   BYBFILE.BYB_UN9_REB_1,
                   BYBFILE.BYB_UN10_REB_1,
                   BYBFILE.BYB_PART_CHGOFF_1,
                   BYBFILE.BYB_ALLOTMENTS_1,
                   BYBFILE.BYB_INS_NO_ELIG_1_1,
                   BYBFILE.BYB_INS_NO_WROTE_1_1,
                   BYBFILE.BYB_INS_NO_NOTWROTE_1_1,
                   BYBFILE.BYB_INS_AMT_WROTE_1_1,
                   BYBFILE.BYB_INS_NO_ELIG_1_2,
                   BYBFILE.BYB_INS_NO_WROTE_1_2,
                   BYBFILE.BYB_INS_NO_NOTWROTE_1_2,
                   BYBFILE.BYB_INS_AMT_WROTE_1_2,
                   BYBFILE.BYB_INS_NO_ELIG_1_3,
                   BYBFILE.BYB_INS_NO_WROTE_1_3,
                   BYBFILE.BYB_INS_NO_NOTWROTE_1_3,
                   BYBFILE.BYB_INS_AMT_WROTE_1_3,
                   BYBFILE.BYB_INS_NO_ELIG_1_4,
                   BYBFILE.BYB_INS_NO_WROTE_1_4,
                   BYBFILE.BYB_INS_NO_NOTWROTE_1_4,
                   BYBFILE.BYB_INS_AMT_WROTE_1_4,
                   BYBFILE.BYB_INS_NO_ELIG_1_5,
                   BYBFILE.BYB_INS_NO_WROTE_1_5,
                   BYBFILE.BYB_INS_NO_NOTWROTE_1_5,
                   BYBFILE.BYB_INS_AMT_WROTE_1_5,
                   BYBFILE.BYB_INS_NO_ELIG_1_6,
                   BYBFILE.BYB_INS_NO_WROTE_1_6,
                   BYBFILE.BYB_INS_NO_NOTWROTE_1_6,
                   BYBFILE.BYB_INS_AMT_WROTE_1_6,
                   BYBFILE.BYB_INS_NO_ELIG_1_7,
                   BYBFILE.BYB_INS_NO_WROTE_1_7,
                   BYBFILE.BYB_INS_NO_NOTWROTE_1_7,
                   BYBFILE.BYB_INS_AMT_WROTE_1_7,
                   BYBFILE.BYB_INS_NO_ELIG_1_8,
                   BYBFILE.BYB_INS_NO_WROTE_1_8,
                   BYBFILE.BYB_INS_NO_NOTWROTE_1_8,
                   BYBFILE.BYB_INS_AMT_WROTE_1_8,
                   BYBFILE.BYB_INS_NO_ELIG_1_9,
                   BYBFILE.BYB_INS_NO_WROTE_1_9,
                   BYBFILE.BYB_INS_NO_NOTWROTE_1_9,
                   BYBFILE.BYB_INS_AMT_WROTE_1_9,
                   BYBFILE.BYB_INS_NO_ELIG_1_10,
                   BYBFILE.BYB_INS_NO_WROTE_1_10,
                   BYBFILE.BYB_INS_NO_NOTWROTE_1_10,
                   BYBFILE.BYB_INS_AMT_WROTE_1_10,
                   BYBFILE.BYB_FEE_NO_ELIG_1_1,
                   BYBFILE.BYB_FEE_NO_WROTE_1_1,
                   BYBFILE.BYB_FEE_NO_NOTWROTE_1_1,
                   BYBFILE.BYB_FEE_AMT_WROTE_1_1,
                   BYBFILE.BYB_FEE_NO_ELIG_1_2,
                   BYBFILE.BYB_FEE_NO_WROTE_1_2,
                   BYBFILE.BYB_FEE_NO_NOTWROTE_1_2,
                   BYBFILE.BYB_FEE_AMT_WROTE_1_2,
                   BYBFILE.BYB_FEE_NO_ELIG_1_3,
                   BYBFILE.BYB_FEE_NO_WROTE_1_3,
                   BYBFILE.BYB_FEE_NO_NOTWROTE_1_3,
                   BYBFILE.BYB_FEE_AMT_WROTE_1_3,
                   BYBFILE.BYB_OTH_NO_COL_1,
                   BYBFILE.BYB_OT2_NO_COL_1,
                   BYBFILE.BYB_INT_NO_COL_1,
                   BYBFILE.BYB_LATE_NO_COL_1,
                   BYBFILE.BYB_OTH_NO_WAIVED_1,
                   BYBFILE.BYB_OT2_NO_WAIVED_1,
                   BYBFILE.BYB_INT_NO_WAIVED_1,
                   BYBFILE.BYB_LATE_NO_WAIVED_1,
                   BYBFILE.BYB_OTH_AMT_WAIVED_1,
                   BYBFILE.BYB_OT2_AMT_WAIVED_1,
                   BYBFILE.BYB_INT_AMT_WAIVED_1,
                   BYBFILE.BYB_LATE_AMT_WAIVED_1,
                   BYBFILE.BYB_PL_PRINCOL_2_1,
                   BYBFILE.BYB_PL_INTCOL_2_1,
                   BYBFILE.BYB_PL_LATECOL_2_1,
                   BYBFILE.BYB_PL_O2COL_2_1,
                   BYBFILE.BYB_PL_PRINCOL_2_2,
                   BYBFILE.BYB_PL_INTCOL_2_2,
                   BYBFILE.BYB_PL_LATECOL_2_2,
                   BYBFILE.BYB_PL_O2COL_2_2,
                   BYBFILE.BYB_PL_PRINCOL_2_3,
                   BYBFILE.BYB_PL_INTCOL_2_3,
                   BYBFILE.BYB_PL_LATECOL_2_3,
                   BYBFILE.BYB_PL_O2COL_2_3,
                   BYBFILE.BYB_PL_PRINCOL_2_4,
                   BYBFILE.BYB_PL_INTCOL_2_4,
                   BYBFILE.BYB_PL_LATECOL_2_4,
                   BYBFILE.BYB_PL_O2COL_2_4,
                   BYBFILE.BYB_PL_PRINCOL_2_5,
                   BYBFILE.BYB_PL_INTCOL_2_5,
                   BYBFILE.BYB_PL_LATECOL_2_5,
                   BYBFILE.BYB_PL_O2COL_2_5,
                   BYBFILE.BYB_AMT_SOLD_2,
                   BYBFILE.BYB_AMT_TRANS_2,
                   BYBFILE.BYB_AMT_PURCH_2,
                   BYBFILE.BYB_NO_SOLD_2,
                   BYBFILE.BYB_NO_TRANS_2,
                   BYBFILE.BYB_NO_PURCH_2,
                   BYBFILE.BYB_OTHCOL_2,
                   BYBFILE.BYB_OT2COL_2,
                   BYBFILE.BYB_INTCOL_2,
                   BYBFILE.BYB_LATECOL_2,
                   BYBFILE.BYB_PRINCOL_2,
                   BYBFILE.BYB_DEFCOL_2,
                   BYBFILE.BYB_RNPRIN_2,
                   BYBFILE.BYB_INTREB_2,
                   BYBFILE.BYB_CLREB1_2,
                   BYBFILE.BYB_CLREB2_2,
                   BYBFILE.BYB_CLREB3_2,
                   BYBFILE.BYB_CLREB4_2,
                   BYBFILE.BYB_AHREB_2,
                   BYBFILE.BYB_PPREB_2,
                   BYBFILE.BYB_O1REB_2,
                   BYBFILE.BYB_O2REB_2,
                   BYBFILE.BYB_O3REB_2,
                   BYBFILE.BYB_O4REB_2,
                   BYBFILE.BYB_O5REB_2,
                   BYBFILE.BYB_O6REB_2,
                   BYBFILE.BYB_O7REB_2,
                   BYBFILE.BYB_SCREB_2,
                   BYBFILE.BYB_MFREB_2,
                   BYBFILE.BYB_DFREB_2,
                   BYBFILE.BYB_UN7_REB_2,
                   BYBFILE.BYB_UN8_REB_2,
                   BYBFILE.BYB_UN9_REB_2,
                   BYBFILE.BYB_UN10_REB_2,
                   BYBFILE.BYB_PART_CHGOFF_2,
                   BYBFILE.BYB_ALLOTMENTS_2,
                   BYBFILE.BYB_INS_NO_ELIG_2_1,
                   BYBFILE.BYB_INS_NO_WROTE_2_1,
                   BYBFILE.BYB_INS_NO_NOTWROTE_2_1,
                   BYBFILE.BYB_INS_AMT_WROTE_2_1,
                   BYBFILE.BYB_INS_NO_ELIG_2_2,
                   BYBFILE.BYB_INS_NO_WROTE_2_2,
                   BYBFILE.BYB_INS_NO_NOTWROTE_2_2,
                   BYBFILE.BYB_INS_AMT_WROTE_2_2,
                   BYBFILE.BYB_INS_NO_ELIG_2_3,
                   BYBFILE.BYB_INS_NO_WROTE_2_3,
                   BYBFILE.BYB_INS_NO_NOTWROTE_2_3,
                   BYBFILE.BYB_INS_AMT_WROTE_2_3,
                   BYBFILE.BYB_INS_NO_ELIG_2_4,
                   BYBFILE.BYB_INS_NO_WROTE_2_4,
                   BYBFILE.BYB_INS_NO_NOTWROTE_2_4,
                   BYBFILE.BYB_INS_AMT_WROTE_2_4,
                   BYBFILE.BYB_INS_NO_ELIG_2_5,
                   BYBFILE.BYB_INS_NO_WROTE_2_5,
                   BYBFILE.BYB_INS_NO_NOTWROTE_2_5,
                   BYBFILE.BYB_INS_AMT_WROTE_2_5,
                   BYBFILE.BYB_INS_NO_ELIG_2_6,
                   BYBFILE.BYB_INS_NO_WROTE_2_6,
                   BYBFILE.BYB_INS_NO_NOTWROTE_2_6,
                   BYBFILE.BYB_INS_AMT_WROTE_2_6,
                   BYBFILE.BYB_INS_NO_ELIG_2_7,
                   BYBFILE.BYB_INS_NO_WROTE_2_7,
                   BYBFILE.BYB_INS_NO_NOTWROTE_2_7,
                   BYBFILE.BYB_INS_AMT_WROTE_2_7,
                   BYBFILE.BYB_INS_NO_ELIG_2_8,
                   BYBFILE.BYB_INS_NO_WROTE_2_8,
                   BYBFILE.BYB_INS_NO_NOTWROTE_2_8,
                   BYBFILE.BYB_INS_AMT_WROTE_2_8,
                   BYBFILE.BYB_INS_NO_ELIG_2_9,
                   BYBFILE.BYB_INS_NO_WROTE_2_9,
                   BYBFILE.BYB_INS_NO_NOTWROTE_2_9,
                   BYBFILE.BYB_INS_AMT_WROTE_2_9,
                   BYBFILE.BYB_INS_NO_ELIG_2_10,
                   BYBFILE.BYB_INS_NO_WROTE_2_10,
                   BYBFILE.BYB_INS_NO_NOTWROTE_2_10,
                   BYBFILE.BYB_INS_AMT_WROTE_2_10,
                   BYBFILE.BYB_FEE_NO_ELIG_2_1,
                   BYBFILE.BYB_FEE_NO_WROTE_2_1,
                   BYBFILE.BYB_FEE_NO_NOTWROTE_2_1,
                   BYBFILE.BYB_FEE_AMT_WROTE_2_1,
                   BYBFILE.BYB_FEE_NO_ELIG_2_2,
                   BYBFILE.BYB_FEE_NO_WROTE_2_2,
                   BYBFILE.BYB_FEE_NO_NOTWROTE_2_2,
                   BYBFILE.BYB_FEE_AMT_WROTE_2_2,
                   BYBFILE.BYB_FEE_NO_ELIG_2_3,
                   BYBFILE.BYB_FEE_NO_WROTE_2_3,
                   BYBFILE.BYB_FEE_NO_NOTWROTE_2_3,
                   BYBFILE.BYB_FEE_AMT_WROTE_2_3,
                   BYBFILE.BYB_OTH_NO_COL_2,
                   BYBFILE.BYB_OT2_NO_COL_2,
                   BYBFILE.BYB_INT_NO_COL_2,
                   BYBFILE.BYB_LATE_NO_COL_2,
                   BYBFILE.BYB_OTH_NO_WAIVED_2,
                   BYBFILE.BYB_OT2_NO_WAIVED_2,
                   BYBFILE.BYB_INT_NO_WAIVED_2,
                   BYBFILE.BYB_LATE_NO_WAIVED_2,
                   BYBFILE.BYB_OTH_AMT_WAIVED_2,
                   BYBFILE.BYB_OT2_AMT_WAIVED_2,
                   BYBFILE.BYB_INT_AMT_WAIVED_2,
                   BYBFILE.BYB_LATE_AMT_WAIVED_2,
                   BYBFILE.BYB_UNITS_1_1_1,
                   BYBFILE.BYB_OUTBAL_1_1_1,
                   BYBFILE.BYB_UNITS_1_1_2,
                   BYBFILE.BYB_OUTBAL_1_1_2,
                   BYBFILE.BYB_UNITS_1_1_3,
                   BYBFILE.BYB_OUTBAL_1_1_3,
                   BYBFILE.BYB_UNITS_1_1_4,
                   BYBFILE.BYB_OUTBAL_1_1_4,
                   BYBFILE.BYB_UNITS_1_1_5,
                   BYBFILE.BYB_OUTBAL_1_1_5,
                   BYBFILE.BYB_UNITS_1_1_6,
                   BYBFILE.BYB_OUTBAL_1_1_6,
                   BYBFILE.BYB_UNITS_1_1_7,
                   BYBFILE.BYB_OUTBAL_1_1_7,
                   BYBFILE.BYB_UNITS_1_1_8,
                   BYBFILE.BYB_OUTBAL_1_1_8,
                   BYBFILE.BYB_UNITS_1_1_9,
                   BYBFILE.BYB_OUTBAL_1_1_9,
                   BYBFILE.BYB_UNITS_1_1_10,
                   BYBFILE.BYB_OUTBAL_1_1_10,
                   BYBFILE.BYB_UNITS_1_2_1,
                   BYBFILE.BYB_OUTBAL_1_2_1,
                   BYBFILE.BYB_UNITS_1_2_2,
                   BYBFILE.BYB_OUTBAL_1_2_2,
                   BYBFILE.BYB_UNITS_1_2_3,
                   BYBFILE.BYB_OUTBAL_1_2_3,
                   BYBFILE.BYB_UNITS_1_2_4,
                   BYBFILE.BYB_OUTBAL_1_2_4,
                   BYBFILE.BYB_UNITS_1_2_5,
                   BYBFILE.BYB_OUTBAL_1_2_5,
                   BYBFILE.BYB_UNITS_1_2_6,
                   BYBFILE.BYB_OUTBAL_1_2_6,
                   BYBFILE.BYB_UNITS_1_2_7,
                   BYBFILE.BYB_OUTBAL_1_2_7,
                   BYBFILE.BYB_UNITS_1_2_8,
                   BYBFILE.BYB_OUTBAL_1_2_8,
                   BYBFILE.BYB_UNITS_1_2_9,
                   BYBFILE.BYB_OUTBAL_1_2_9,
                   BYBFILE.BYB_UNITS_1_2_10,
                   BYBFILE.BYB_OUTBAL_1_2_10,
                   BYBFILE.BYB_UNITS_2_1_1,
                   BYBFILE.BYB_OUTBAL_2_1_1,
                   BYBFILE.BYB_UNITS_2_1_2,
                   BYBFILE.BYB_OUTBAL_2_1_2,
                   BYBFILE.BYB_UNITS_2_1_3,
                   BYBFILE.BYB_OUTBAL_2_1_3,
                   BYBFILE.BYB_UNITS_2_1_4,
                   BYBFILE.BYB_OUTBAL_2_1_4,
                   BYBFILE.BYB_UNITS_2_1_5,
                   BYBFILE.BYB_OUTBAL_2_1_5,
                   BYBFILE.BYB_UNITS_2_1_6,
                   BYBFILE.BYB_OUTBAL_2_1_6,
                   BYBFILE.BYB_UNITS_2_1_7,
                   BYBFILE.BYB_OUTBAL_2_1_7,
                   BYBFILE.BYB_UNITS_2_1_8,
                   BYBFILE.BYB_OUTBAL_2_1_8,
                   BYBFILE.BYB_UNITS_2_1_9,
                   BYBFILE.BYB_OUTBAL_2_1_9,
                   BYBFILE.BYB_UNITS_2_1_10,
                   BYBFILE.BYB_OUTBAL_2_1_10,
                   BYBFILE.BYB_UNITS_2_2_1,
                   BYBFILE.BYB_OUTBAL_2_2_1,
                   BYBFILE.BYB_UNITS_2_2_2,
                   BYBFILE.BYB_OUTBAL_2_2_2,
                   BYBFILE.BYB_UNITS_2_2_3,
                   BYBFILE.BYB_OUTBAL_2_2_3,
                   BYBFILE.BYB_UNITS_2_2_4,
                   BYBFILE.BYB_OUTBAL_2_2_4,
                   BYBFILE.BYB_UNITS_2_2_5,
                   BYBFILE.BYB_OUTBAL_2_2_5,
                   BYBFILE.BYB_UNITS_2_2_6,
                   BYBFILE.BYB_OUTBAL_2_2_6,
                   BYBFILE.BYB_UNITS_2_2_7,
                   BYBFILE.BYB_OUTBAL_2_2_7,
                   BYBFILE.BYB_UNITS_2_2_8,
                   BYBFILE.BYB_OUTBAL_2_2_8,
                   BYBFILE.BYB_UNITS_2_2_9,
                   BYBFILE.BYB_OUTBAL_2_2_9,
                   BYBFILE.BYB_UNITS_2_2_10,
                   BYBFILE.BYB_OUTBAL_2_2_10,
                   BYBFILE.BYB_UNITS_3_1_1,
                   BYBFILE.BYB_OUTBAL_3_1_1,
                   BYBFILE.BYB_UNITS_3_1_2,
                   BYBFILE.BYB_OUTBAL_3_1_2,
                   BYBFILE.BYB_UNITS_3_1_3,
                   BYBFILE.BYB_OUTBAL_3_1_3,
                   BYBFILE.BYB_UNITS_3_1_4,
                   BYBFILE.BYB_OUTBAL_3_1_4,
                   BYBFILE.BYB_UNITS_3_1_5,
                   BYBFILE.BYB_OUTBAL_3_1_5,
                   BYBFILE.BYB_UNITS_3_1_6,
                   BYBFILE.BYB_OUTBAL_3_1_6,
                   BYBFILE.BYB_UNITS_3_1_7,
                   BYBFILE.BYB_OUTBAL_3_1_7,
                   BYBFILE.BYB_UNITS_3_1_8,
                   BYBFILE.BYB_OUTBAL_3_1_8,
                   BYBFILE.BYB_UNITS_3_1_9,
                   BYBFILE.BYB_OUTBAL_3_1_9,
                   BYBFILE.BYB_UNITS_3_1_10,
                   BYBFILE.BYB_OUTBAL_3_1_10,
                   BYBFILE.BYB_UNITS_3_2_1,
                   BYBFILE.BYB_OUTBAL_3_2_1,
                   BYBFILE.BYB_UNITS_3_2_2,
                   BYBFILE.BYB_OUTBAL_3_2_2,
                   BYBFILE.BYB_UNITS_3_2_3,
                   BYBFILE.BYB_OUTBAL_3_2_3,
                   BYBFILE.BYB_UNITS_3_2_4,
                   BYBFILE.BYB_OUTBAL_3_2_4,
                   BYBFILE.BYB_UNITS_3_2_5,
                   BYBFILE.BYB_OUTBAL_3_2_5,
                   BYBFILE.BYB_UNITS_3_2_6,
                   BYBFILE.BYB_OUTBAL_3_2_6,
                   BYBFILE.BYB_UNITS_3_2_7,
                   BYBFILE.BYB_OUTBAL_3_2_7,
                   BYBFILE.BYB_UNITS_3_2_8,
                   BYBFILE.BYB_OUTBAL_3_2_8,
                   BYBFILE.BYB_UNITS_3_2_9,
                   BYBFILE.BYB_OUTBAL_3_2_9,
                   BYBFILE.BYB_UNITS_3_2_10,
                   BYBFILE.BYB_OUTBAL_3_2_10,
                   BYBFILE.BYB_UNITS_BNKRPT_1_1_1,
                   BYBFILE.BYB_OUTBAL_BNKRPT_1_1_1,
                   BYBFILE.BYB_UNITS_BNKRPT_1_1_2,
                   BYBFILE.BYB_OUTBAL_BNKRPT_1_1_2,
                   BYBFILE.BYB_UNITS_BNKRPT_1_1_3,
                   BYBFILE.BYB_OUTBAL_BNKRPT_1_1_3,
                   BYBFILE.BYB_UNITS_BNKRPT_1_1_4,
                   BYBFILE.BYB_OUTBAL_BNKRPT_1_1_4,
                   BYBFILE.BYB_UNITS_BNKRPT_1_1_5,
                   BYBFILE.BYB_OUTBAL_BNKRPT_1_1_5,
                   BYBFILE.BYB_UNITS_BNKRPT_1_1_6,
                   BYBFILE.BYB_OUTBAL_BNKRPT_1_1_6,
                   BYBFILE.BYB_UNITS_BNKRPT_1_1_7,
                   BYBFILE.BYB_OUTBAL_BNKRPT_1_1_7,
                   BYBFILE.BYB_UNITS_BNKRPT_1_1_8,
                   BYBFILE.BYB_OUTBAL_BNKRPT_1_1_8,
                   BYBFILE.BYB_UNITS_BNKRPT_1_1_9,
                   BYBFILE.BYB_OUTBAL_BNKRPT_1_1_9,
                   BYBFILE.BYB_UNITS_BNKRPT_1_1_10,
                   BYBFILE.BYB_OUTBAL_BNKRPT_1_1_10,
                   BYBFILE.BYB_UNITS_BNKRPT_1_2_1,
                   BYBFILE.BYB_OUTBAL_BNKRPT_1_2_1,
                   BYBFILE.BYB_UNITS_BNKRPT_1_2_2,
                   BYBFILE.BYB_OUTBAL_BNKRPT_1_2_2,
                   BYBFILE.BYB_UNITS_BNKRPT_1_2_3,
                   BYBFILE.BYB_OUTBAL_BNKRPT_1_2_3,
                   BYBFILE.BYB_UNITS_BNKRPT_1_2_4,
                   BYBFILE.BYB_OUTBAL_BNKRPT_1_2_4,
                   BYBFILE.BYB_UNITS_BNKRPT_1_2_5,
                   BYBFILE.BYB_OUTBAL_BNKRPT_1_2_5,
                   BYBFILE.BYB_UNITS_BNKRPT_1_2_6,
                   BYBFILE.BYB_OUTBAL_BNKRPT_1_2_6,
                   BYBFILE.BYB_UNITS_BNKRPT_1_2_7,
                   BYBFILE.BYB_OUTBAL_BNKRPT_1_2_7,
                   BYBFILE.BYB_UNITS_BNKRPT_1_2_8,
                   BYBFILE.BYB_OUTBAL_BNKRPT_1_2_8,
                   BYBFILE.BYB_UNITS_BNKRPT_1_2_9,
                   BYBFILE.BYB_OUTBAL_BNKRPT_1_2_9,
                   BYBFILE.BYB_UNITS_BNKRPT_1_2_10,
                   BYBFILE.BYB_OUTBAL_BNKRPT_1_2_10,
                   BYBFILE.BYB_UNITS_BNKRPT_2_1_1,
                   BYBFILE.BYB_OUTBAL_BNKRPT_2_1_1,
                   BYBFILE.BYB_UNITS_BNKRPT_2_1_2,
                   BYBFILE.BYB_OUTBAL_BNKRPT_2_1_2,
                   BYBFILE.BYB_UNITS_BNKRPT_2_1_3,
                   BYBFILE.BYB_OUTBAL_BNKRPT_2_1_3,
                   BYBFILE.BYB_UNITS_BNKRPT_2_1_4,
                   BYBFILE.BYB_OUTBAL_BNKRPT_2_1_4,
                   BYBFILE.BYB_UNITS_BNKRPT_2_1_5,
                   BYBFILE.BYB_OUTBAL_BNKRPT_2_1_5,
                   BYBFILE.BYB_UNITS_BNKRPT_2_1_6,
                   BYBFILE.BYB_OUTBAL_BNKRPT_2_1_6,
                   BYBFILE.BYB_UNITS_BNKRPT_2_1_7,
                   BYBFILE.BYB_OUTBAL_BNKRPT_2_1_7,
                   BYBFILE.BYB_UNITS_BNKRPT_2_1_8,
                   BYBFILE.BYB_OUTBAL_BNKRPT_2_1_8,
                   BYBFILE.BYB_UNITS_BNKRPT_2_1_9,
                   BYBFILE.BYB_OUTBAL_BNKRPT_2_1_9,
                   BYBFILE.BYB_UNITS_BNKRPT_2_1_10,
                   BYBFILE.BYB_OUTBAL_BNKRPT_2_1_10,
                   BYBFILE.BYB_UNITS_BNKRPT_2_2_1,
                   BYBFILE.BYB_OUTBAL_BNKRPT_2_2_1,
                   BYBFILE.BYB_UNITS_BNKRPT_2_2_2,
                   BYBFILE.BYB_OUTBAL_BNKRPT_2_2_2,
                   BYBFILE.BYB_UNITS_BNKRPT_2_2_3,
                   BYBFILE.BYB_OUTBAL_BNKRPT_2_2_3,
                   BYBFILE.BYB_UNITS_BNKRPT_2_2_4,
                   BYBFILE.BYB_OUTBAL_BNKRPT_2_2_4,
                   BYBFILE.BYB_UNITS_BNKRPT_2_2_5,
                   BYBFILE.BYB_OUTBAL_BNKRPT_2_2_5,
                   BYBFILE.BYB_UNITS_BNKRPT_2_2_6,
                   BYBFILE.BYB_OUTBAL_BNKRPT_2_2_6,
                   BYBFILE.BYB_UNITS_BNKRPT_2_2_7,
                   BYBFILE.BYB_OUTBAL_BNKRPT_2_2_7,
                   BYBFILE.BYB_UNITS_BNKRPT_2_2_8,
                   BYBFILE.BYB_OUTBAL_BNKRPT_2_2_8,
                   BYBFILE.BYB_UNITS_BNKRPT_2_2_9,
                   BYBFILE.BYB_OUTBAL_BNKRPT_2_2_9,
                   BYBFILE.BYB_UNITS_BNKRPT_2_2_10,
                   BYBFILE.BYB_OUTBAL_BNKRPT_2_2_10,
                   BYBFILE.BYB_UNITS_BNKRPT_3_1_1,
                   BYBFILE.BYB_OUTBAL_BNKRPT_3_1_1,
                   BYBFILE.BYB_UNITS_BNKRPT_3_1_2,
                   BYBFILE.BYB_OUTBAL_BNKRPT_3_1_2,
                   BYBFILE.BYB_UNITS_BNKRPT_3_1_3,
                   BYBFILE.BYB_OUTBAL_BNKRPT_3_1_3,
                   BYBFILE.BYB_UNITS_BNKRPT_3_1_4,
                   BYBFILE.BYB_OUTBAL_BNKRPT_3_1_4,
                   BYBFILE.BYB_UNITS_BNKRPT_3_1_5,
                   BYBFILE.BYB_OUTBAL_BNKRPT_3_1_5,
                   BYBFILE.BYB_UNITS_BNKRPT_3_1_6,
                   BYBFILE.BYB_OUTBAL_BNKRPT_3_1_6,
                   BYBFILE.BYB_UNITS_BNKRPT_3_1_7,
                   BYBFILE.BYB_OUTBAL_BNKRPT_3_1_7,
                   BYBFILE.BYB_UNITS_BNKRPT_3_1_8,
                   BYBFILE.BYB_OUTBAL_BNKRPT_3_1_8,
                   BYBFILE.BYB_UNITS_BNKRPT_3_1_9,
                   BYBFILE.BYB_OUTBAL_BNKRPT_3_1_9,
                   BYBFILE.BYB_UNITS_BNKRPT_3_1_10,
                   BYBFILE.BYB_OUTBAL_BNKRPT_3_1_10,
                   BYBFILE.BYB_UNITS_BNKRPT_3_2_1,
                   BYBFILE.BYB_OUTBAL_BNKRPT_3_2_1,
                   BYBFILE.BYB_UNITS_BNKRPT_3_2_2,
                   BYBFILE.BYB_OUTBAL_BNKRPT_3_2_2,
                   BYBFILE.BYB_UNITS_BNKRPT_3_2_3,
                   BYBFILE.BYB_OUTBAL_BNKRPT_3_2_3,
                   BYBFILE.BYB_UNITS_BNKRPT_3_2_4,
                   BYBFILE.BYB_OUTBAL_BNKRPT_3_2_4,
                   BYBFILE.BYB_UNITS_BNKRPT_3_2_5,
                   BYBFILE.BYB_OUTBAL_BNKRPT_3_2_5,
                   BYBFILE.BYB_UNITS_BNKRPT_3_2_6,
                   BYBFILE.BYB_OUTBAL_BNKRPT_3_2_6,
                   BYBFILE.BYB_UNITS_BNKRPT_3_2_7,
                   BYBFILE.BYB_OUTBAL_BNKRPT_3_2_7,
                   BYBFILE.BYB_UNITS_BNKRPT_3_2_8,
                   BYBFILE.BYB_OUTBAL_BNKRPT_3_2_8,
                   BYBFILE.BYB_UNITS_BNKRPT_3_2_9,
                   BYBFILE.BYB_OUTBAL_BNKRPT_3_2_9,
                   BYBFILE.BYB_UNITS_BNKRPT_3_2_10,
                   BYBFILE.BYB_OUTBAL_BNKRPT_3_2_10,
                   BYBFILE.BYB_SUMWGTTM,
                   BYBFILE.BYB_SUMWGTAV,
                   BYBFILE.BYB_SUMWGTTA,
                   BYBFILE.BYB_IB_ACCRUED,
                   BYBFILE.BYB_DLDISC_1,
                   BYBFILE.BYB_DLDISC_2,
                   BYBFILE.BYB_DLDISC_3,
                   BYBFILE.BYB_STD_POS,
                   BYBFILE.BYB_30_DAY_MOVED,
                   BYBFILE.BYB_PROJ_INCREASE,
                   BYBFILE.BYB_PROJ_PROFIT,
                   BYBFILE.BYB_PROCEEDS_1,
                   BYBFILE.BYB_PROCEEDS_2,
                   BYBFILE.BYB_LOCKBOX_1,
                   BYBFILE.BYB_LOCKBOX_2,
                   BYBFILE.BYB_UNEARNED_AMT_1_1_1,
                   BYBFILE.BYB_UNEARNED_AMT_1_1_2,
                   BYBFILE.BYB_UNEARNED_AMT_1_1_3,
                   BYBFILE.BYB_UNEARNED_AMT_1_1_4,
                   BYBFILE.BYB_UNEARNED_AMT_1_1_5,
                   BYBFILE.BYB_UNEARNED_AMT_1_1_6,
                   BYBFILE.BYB_UNEARNED_AMT_1_1_7,
                   BYBFILE.BYB_UNEARNED_AMT_1_1_8,
                   BYBFILE.BYB_UNEARNED_AMT_1_1_9,
                   BYBFILE.BYB_UNEARNED_AMT_1_1_10,
                   BYBFILE.BYB_UNEARNED_AMT_1_2_1,
                   BYBFILE.BYB_UNEARNED_AMT_1_2_2,
                   BYBFILE.BYB_UNEARNED_AMT_1_2_3,
                   BYBFILE.BYB_UNEARNED_AMT_1_2_4,
                   BYBFILE.BYB_UNEARNED_AMT_1_2_5,
                   BYBFILE.BYB_UNEARNED_AMT_1_2_6,
                   BYBFILE.BYB_UNEARNED_AMT_1_2_7,
                   BYBFILE.BYB_UNEARNED_AMT_1_2_8,
                   BYBFILE.BYB_UNEARNED_AMT_1_2_9,
                   BYBFILE.BYB_UNEARNED_AMT_1_2_10,
                   BYBFILE.BYB_UNEARNED_AMT_2_1_1,
                   BYBFILE.BYB_UNEARNED_AMT_2_1_2,
                   BYBFILE.BYB_UNEARNED_AMT_2_1_3,
                   BYBFILE.BYB_UNEARNED_AMT_2_1_4,
                   BYBFILE.BYB_UNEARNED_AMT_2_1_5,
                   BYBFILE.BYB_UNEARNED_AMT_2_1_6,
                   BYBFILE.BYB_UNEARNED_AMT_2_1_7,
                   BYBFILE.BYB_UNEARNED_AMT_2_1_8,
                   BYBFILE.BYB_UNEARNED_AMT_2_1_9,
                   BYBFILE.BYB_UNEARNED_AMT_2_1_10,
                   BYBFILE.BYB_UNEARNED_AMT_2_2_1,
                   BYBFILE.BYB_UNEARNED_AMT_2_2_2,
                   BYBFILE.BYB_UNEARNED_AMT_2_2_3,
                   BYBFILE.BYB_UNEARNED_AMT_2_2_4,
                   BYBFILE.BYB_UNEARNED_AMT_2_2_5,
                   BYBFILE.BYB_UNEARNED_AMT_2_2_6,
                   BYBFILE.BYB_UNEARNED_AMT_2_2_7,
                   BYBFILE.BYB_UNEARNED_AMT_2_2_8,
                   BYBFILE.BYB_UNEARNED_AMT_2_2_9,
                   BYBFILE.BYB_UNEARNED_AMT_2_2_10,
                   BYBFILE.BYB_UNEARNED_AMT_3_1_1,
                   BYBFILE.BYB_UNEARNED_AMT_3_1_2,
                   BYBFILE.BYB_UNEARNED_AMT_3_1_3,
                   BYBFILE.BYB_UNEARNED_AMT_3_1_4,
                   BYBFILE.BYB_UNEARNED_AMT_3_1_5,
                   BYBFILE.BYB_UNEARNED_AMT_3_1_6,
                   BYBFILE.BYB_UNEARNED_AMT_3_1_7,
                   BYBFILE.BYB_UNEARNED_AMT_3_1_8,
                   BYBFILE.BYB_UNEARNED_AMT_3_1_9,
                   BYBFILE.BYB_UNEARNED_AMT_3_1_10,
                   BYBFILE.BYB_UNEARNED_AMT_3_2_1,
                   BYBFILE.BYB_UNEARNED_AMT_3_2_2,
                   BYBFILE.BYB_UNEARNED_AMT_3_2_3,
                   BYBFILE.BYB_UNEARNED_AMT_3_2_4,
                   BYBFILE.BYB_UNEARNED_AMT_3_2_5,
                   BYBFILE.BYB_UNEARNED_AMT_3_2_6,
                   BYBFILE.BYB_UNEARNED_AMT_3_2_7,
                   BYBFILE.BYB_UNEARNED_AMT_3_2_8,
                   BYBFILE.BYB_UNEARNED_AMT_3_2_9,
                   BYBFILE.BYB_UNEARNED_AMT_3_2_10
            INTO :QBYB-REC
            FROM DBO.BYBFILE
            WHERE BYBFILE.BYB_DATE_CCYYMM = :QBYB-DATE-CCYYMM
              AND BYBFILE.BYB_BRNO = :QBYB-BRNO
              AND BYBFILE.BYB_CLASS = :QBYB-CLASS
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-BYB1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-BYB-FIELDS.

      *-----------------------------------------------------------------
       WRITE-BYB1-FILE. *> EMBEDDED - BYBFILE

           PERFORM WRITE-IT.
           MOVE BYB-PATH-SQL TO E-FILE.
           MOVE BYB1-KEY TO E-KEYX.
           PERFORM SET-BYB-FIELDS.

           MOVE '2' TO SQL-UPDATE-CONN-USE-SW. *> USE UPDATE_CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

           EXEC SQL
            INSERT INTO DBO.BYBFILE
             (BYBFILE.BYB_DATE_CCYYMM,
              BYBFILE.BYB_BRNO,
              BYBFILE.BYB_CLASS,
              BYBFILE.BYB_PL_PRINCOL_1_1,
              BYBFILE.BYB_PL_INTCOL_1_1,
              BYBFILE.BYB_PL_LATECOL_1_1,
              BYBFILE.BYB_PL_O2COL_1_1,
              BYBFILE.BYB_PL_PRINCOL_1_2,
              BYBFILE.BYB_PL_INTCOL_1_2,
              BYBFILE.BYB_PL_LATECOL_1_2,
              BYBFILE.BYB_PL_O2COL_1_2,
              BYBFILE.BYB_PL_PRINCOL_1_3,
              BYBFILE.BYB_PL_INTCOL_1_3,
              BYBFILE.BYB_PL_LATECOL_1_3,
              BYBFILE.BYB_PL_O2COL_1_3,
              BYBFILE.BYB_PL_PRINCOL_1_4,
              BYBFILE.BYB_PL_INTCOL_1_4,
              BYBFILE.BYB_PL_LATECOL_1_4,
              BYBFILE.BYB_PL_O2COL_1_4,
              BYBFILE.BYB_PL_PRINCOL_1_5,
              BYBFILE.BYB_PL_INTCOL_1_5,
              BYBFILE.BYB_PL_LATECOL_1_5,
              BYBFILE.BYB_PL_O2COL_1_5,
              BYBFILE.BYB_AMT_SOLD_1,
              BYBFILE.BYB_AMT_TRANS_1,
              BYBFILE.BYB_AMT_PURCH_1,
              BYBFILE.BYB_NO_SOLD_1,
              BYBFILE.BYB_NO_TRANS_1,
              BYBFILE.BYB_NO_PURCH_1,
              BYBFILE.BYB_OTHCOL_1,
              BYBFILE.BYB_OT2COL_1,
              BYBFILE.BYB_INTCOL_1,
              BYBFILE.BYB_LATECOL_1,
              BYBFILE.BYB_PRINCOL_1,
              BYBFILE.BYB_DEFCOL_1,
              BYBFILE.BYB_RNPRIN_1,
              BYBFILE.BYB_INTREB_1,
              BYBFILE.BYB_CLREB1_1,
              BYBFILE.BYB_CLREB2_1,
              BYBFILE.BYB_CLREB3_1,
              BYBFILE.BYB_CLREB4_1,
              BYBFILE.BYB_AHREB_1,
              BYBFILE.BYB_PPREB_1,
              BYBFILE.BYB_O1REB_1,
              BYBFILE.BYB_O2REB_1,
              BYBFILE.BYB_O3REB_1,
              BYBFILE.BYB_O4REB_1,
              BYBFILE.BYB_O5REB_1,
              BYBFILE.BYB_O6REB_1,
              BYBFILE.BYB_O7REB_1,
              BYBFILE.BYB_SCREB_1,
              BYBFILE.BYB_MFREB_1,
              BYBFILE.BYB_DFREB_1,
              BYBFILE.BYB_UN7_REB_1,
              BYBFILE.BYB_UN8_REB_1,
              BYBFILE.BYB_UN9_REB_1,
              BYBFILE.BYB_UN10_REB_1,
              BYBFILE.BYB_PART_CHGOFF_1,
              BYBFILE.BYB_ALLOTMENTS_1,
              BYBFILE.BYB_INS_NO_ELIG_1_1,
              BYBFILE.BYB_INS_NO_WROTE_1_1,
              BYBFILE.BYB_INS_NO_NOTWROTE_1_1,
              BYBFILE.BYB_INS_AMT_WROTE_1_1,
              BYBFILE.BYB_INS_NO_ELIG_1_2,
              BYBFILE.BYB_INS_NO_WROTE_1_2,
              BYBFILE.BYB_INS_NO_NOTWROTE_1_2,
              BYBFILE.BYB_INS_AMT_WROTE_1_2,
              BYBFILE.BYB_INS_NO_ELIG_1_3,
              BYBFILE.BYB_INS_NO_WROTE_1_3,
              BYBFILE.BYB_INS_NO_NOTWROTE_1_3,
              BYBFILE.BYB_INS_AMT_WROTE_1_3,
              BYBFILE.BYB_INS_NO_ELIG_1_4,
              BYBFILE.BYB_INS_NO_WROTE_1_4,
              BYBFILE.BYB_INS_NO_NOTWROTE_1_4,
              BYBFILE.BYB_INS_AMT_WROTE_1_4,
              BYBFILE.BYB_INS_NO_ELIG_1_5,
              BYBFILE.BYB_INS_NO_WROTE_1_5,
              BYBFILE.BYB_INS_NO_NOTWROTE_1_5,
              BYBFILE.BYB_INS_AMT_WROTE_1_5,
              BYBFILE.BYB_INS_NO_ELIG_1_6,
              BYBFILE.BYB_INS_NO_WROTE_1_6,
              BYBFILE.BYB_INS_NO_NOTWROTE_1_6,
              BYBFILE.BYB_INS_AMT_WROTE_1_6,
              BYBFILE.BYB_INS_NO_ELIG_1_7,
              BYBFILE.BYB_INS_NO_WROTE_1_7,
              BYBFILE.BYB_INS_NO_NOTWROTE_1_7,
              BYBFILE.BYB_INS_AMT_WROTE_1_7,
              BYBFILE.BYB_INS_NO_ELIG_1_8,
              BYBFILE.BYB_INS_NO_WROTE_1_8,
              BYBFILE.BYB_INS_NO_NOTWROTE_1_8,
              BYBFILE.BYB_INS_AMT_WROTE_1_8,
              BYBFILE.BYB_INS_NO_ELIG_1_9,
              BYBFILE.BYB_INS_NO_WROTE_1_9,
              BYBFILE.BYB_INS_NO_NOTWROTE_1_9,
              BYBFILE.BYB_INS_AMT_WROTE_1_9,
              BYBFILE.BYB_INS_NO_ELIG_1_10,
              BYBFILE.BYB_INS_NO_WROTE_1_10,
              BYBFILE.BYB_INS_NO_NOTWROTE_1_10,
              BYBFILE.BYB_INS_AMT_WROTE_1_10,
              BYBFILE.BYB_FEE_NO_ELIG_1_1,
              BYBFILE.BYB_FEE_NO_WROTE_1_1,
              BYBFILE.BYB_FEE_NO_NOTWROTE_1_1,
              BYBFILE.BYB_FEE_AMT_WROTE_1_1,
              BYBFILE.BYB_FEE_NO_ELIG_1_2,
              BYBFILE.BYB_FEE_NO_WROTE_1_2,
              BYBFILE.BYB_FEE_NO_NOTWROTE_1_2,
              BYBFILE.BYB_FEE_AMT_WROTE_1_2,
              BYBFILE.BYB_FEE_NO_ELIG_1_3,
              BYBFILE.BYB_FEE_NO_WROTE_1_3,
              BYBFILE.BYB_FEE_NO_NOTWROTE_1_3,
              BYBFILE.BYB_FEE_AMT_WROTE_1_3,
              BYBFILE.BYB_OTH_NO_COL_1,
              BYBFILE.BYB_OT2_NO_COL_1,
              BYBFILE.BYB_INT_NO_COL_1,
              BYBFILE.BYB_LATE_NO_COL_1,
              BYBFILE.BYB_OTH_NO_WAIVED_1,
              BYBFILE.BYB_OT2_NO_WAIVED_1,
              BYBFILE.BYB_INT_NO_WAIVED_1,
              BYBFILE.BYB_LATE_NO_WAIVED_1,
              BYBFILE.BYB_OTH_AMT_WAIVED_1,
              BYBFILE.BYB_OT2_AMT_WAIVED_1,
              BYBFILE.BYB_INT_AMT_WAIVED_1,
              BYBFILE.BYB_LATE_AMT_WAIVED_1,
              BYBFILE.BYB_PL_PRINCOL_2_1,
              BYBFILE.BYB_PL_INTCOL_2_1,
              BYBFILE.BYB_PL_LATECOL_2_1,
              BYBFILE.BYB_PL_O2COL_2_1,
              BYBFILE.BYB_PL_PRINCOL_2_2,
              BYBFILE.BYB_PL_INTCOL_2_2,
              BYBFILE.BYB_PL_LATECOL_2_2,
              BYBFILE.BYB_PL_O2COL_2_2,
              BYBFILE.BYB_PL_PRINCOL_2_3,
              BYBFILE.BYB_PL_INTCOL_2_3,
              BYBFILE.BYB_PL_LATECOL_2_3,
              BYBFILE.BYB_PL_O2COL_2_3,
              BYBFILE.BYB_PL_PRINCOL_2_4,
              BYBFILE.BYB_PL_INTCOL_2_4,
              BYBFILE.BYB_PL_LATECOL_2_4,
              BYBFILE.BYB_PL_O2COL_2_4,
              BYBFILE.BYB_PL_PRINCOL_2_5,
              BYBFILE.BYB_PL_INTCOL_2_5,
              BYBFILE.BYB_PL_LATECOL_2_5,
              BYBFILE.BYB_PL_O2COL_2_5,
              BYBFILE.BYB_AMT_SOLD_2,
              BYBFILE.BYB_AMT_TRANS_2,
              BYBFILE.BYB_AMT_PURCH_2,
              BYBFILE.BYB_NO_SOLD_2,
              BYBFILE.BYB_NO_TRANS_2,
              BYBFILE.BYB_NO_PURCH_2,
              BYBFILE.BYB_OTHCOL_2,
              BYBFILE.BYB_OT2COL_2,
              BYBFILE.BYB_INTCOL_2,
              BYBFILE.BYB_LATECOL_2,
              BYBFILE.BYB_PRINCOL_2,
              BYBFILE.BYB_DEFCOL_2,
              BYBFILE.BYB_RNPRIN_2,
              BYBFILE.BYB_INTREB_2,
              BYBFILE.BYB_CLREB1_2,
              BYBFILE.BYB_CLREB2_2,
              BYBFILE.BYB_CLREB3_2,
              BYBFILE.BYB_CLREB4_2,
              BYBFILE.BYB_AHREB_2,
              BYBFILE.BYB_PPREB_2,
              BYBFILE.BYB_O1REB_2,
              BYBFILE.BYB_O2REB_2,
              BYBFILE.BYB_O3REB_2,
              BYBFILE.BYB_O4REB_2,
              BYBFILE.BYB_O5REB_2,
              BYBFILE.BYB_O6REB_2,
              BYBFILE.BYB_O7REB_2,
              BYBFILE.BYB_SCREB_2,
              BYBFILE.BYB_MFREB_2,
              BYBFILE.BYB_DFREB_2,
              BYBFILE.BYB_UN7_REB_2,
              BYBFILE.BYB_UN8_REB_2,
              BYBFILE.BYB_UN9_REB_2,
              BYBFILE.BYB_UN10_REB_2,
              BYBFILE.BYB_PART_CHGOFF_2,
              BYBFILE.BYB_ALLOTMENTS_2,
              BYBFILE.BYB_INS_NO_ELIG_2_1,
              BYBFILE.BYB_INS_NO_WROTE_2_1,
              BYBFILE.BYB_INS_NO_NOTWROTE_2_1,
              BYBFILE.BYB_INS_AMT_WROTE_2_1,
              BYBFILE.BYB_INS_NO_ELIG_2_2,
              BYBFILE.BYB_INS_NO_WROTE_2_2,
              BYBFILE.BYB_INS_NO_NOTWROTE_2_2,
              BYBFILE.BYB_INS_AMT_WROTE_2_2,
              BYBFILE.BYB_INS_NO_ELIG_2_3,
              BYBFILE.BYB_INS_NO_WROTE_2_3,
              BYBFILE.BYB_INS_NO_NOTWROTE_2_3,
              BYBFILE.BYB_INS_AMT_WROTE_2_3,
              BYBFILE.BYB_INS_NO_ELIG_2_4,
              BYBFILE.BYB_INS_NO_WROTE_2_4,
              BYBFILE.BYB_INS_NO_NOTWROTE_2_4,
              BYBFILE.BYB_INS_AMT_WROTE_2_4,
              BYBFILE.BYB_INS_NO_ELIG_2_5,
              BYBFILE.BYB_INS_NO_WROTE_2_5,
              BYBFILE.BYB_INS_NO_NOTWROTE_2_5,
              BYBFILE.BYB_INS_AMT_WROTE_2_5,
              BYBFILE.BYB_INS_NO_ELIG_2_6,
              BYBFILE.BYB_INS_NO_WROTE_2_6,
              BYBFILE.BYB_INS_NO_NOTWROTE_2_6,
              BYBFILE.BYB_INS_AMT_WROTE_2_6,
              BYBFILE.BYB_INS_NO_ELIG_2_7,
              BYBFILE.BYB_INS_NO_WROTE_2_7,
              BYBFILE.BYB_INS_NO_NOTWROTE_2_7,
              BYBFILE.BYB_INS_AMT_WROTE_2_7,
              BYBFILE.BYB_INS_NO_ELIG_2_8,
              BYBFILE.BYB_INS_NO_WROTE_2_8,
              BYBFILE.BYB_INS_NO_NOTWROTE_2_8,
              BYBFILE.BYB_INS_AMT_WROTE_2_8,
              BYBFILE.BYB_INS_NO_ELIG_2_9,
              BYBFILE.BYB_INS_NO_WROTE_2_9,
              BYBFILE.BYB_INS_NO_NOTWROTE_2_9,
              BYBFILE.BYB_INS_AMT_WROTE_2_9,
              BYBFILE.BYB_INS_NO_ELIG_2_10,
              BYBFILE.BYB_INS_NO_WROTE_2_10,
              BYBFILE.BYB_INS_NO_NOTWROTE_2_10,
              BYBFILE.BYB_INS_AMT_WROTE_2_10,
              BYBFILE.BYB_FEE_NO_ELIG_2_1,
              BYBFILE.BYB_FEE_NO_WROTE_2_1,
              BYBFILE.BYB_FEE_NO_NOTWROTE_2_1,
              BYBFILE.BYB_FEE_AMT_WROTE_2_1,
              BYBFILE.BYB_FEE_NO_ELIG_2_2,
              BYBFILE.BYB_FEE_NO_WROTE_2_2,
              BYBFILE.BYB_FEE_NO_NOTWROTE_2_2,
              BYBFILE.BYB_FEE_AMT_WROTE_2_2,
              BYBFILE.BYB_FEE_NO_ELIG_2_3,
              BYBFILE.BYB_FEE_NO_WROTE_2_3,
              BYBFILE.BYB_FEE_NO_NOTWROTE_2_3,
              BYBFILE.BYB_FEE_AMT_WROTE_2_3,
              BYBFILE.BYB_OTH_NO_COL_2,
              BYBFILE.BYB_OT2_NO_COL_2,
              BYBFILE.BYB_INT_NO_COL_2,
              BYBFILE.BYB_LATE_NO_COL_2,
              BYBFILE.BYB_OTH_NO_WAIVED_2,
              BYBFILE.BYB_OT2_NO_WAIVED_2,
              BYBFILE.BYB_INT_NO_WAIVED_2,
              BYBFILE.BYB_LATE_NO_WAIVED_2,
              BYBFILE.BYB_OTH_AMT_WAIVED_2,
              BYBFILE.BYB_OT2_AMT_WAIVED_2,
              BYBFILE.BYB_INT_AMT_WAIVED_2,
              BYBFILE.BYB_LATE_AMT_WAIVED_2,
              BYBFILE.BYB_UNITS_1_1_1,
              BYBFILE.BYB_OUTBAL_1_1_1,
              BYBFILE.BYB_UNITS_1_1_2,
              BYBFILE.BYB_OUTBAL_1_1_2,
              BYBFILE.BYB_UNITS_1_1_3,
              BYBFILE.BYB_OUTBAL_1_1_3,
              BYBFILE.BYB_UNITS_1_1_4,
              BYBFILE.BYB_OUTBAL_1_1_4,
              BYBFILE.BYB_UNITS_1_1_5,
              BYBFILE.BYB_OUTBAL_1_1_5,
              BYBFILE.BYB_UNITS_1_1_6,
              BYBFILE.BYB_OUTBAL_1_1_6,
              BYBFILE.BYB_UNITS_1_1_7,
              BYBFILE.BYB_OUTBAL_1_1_7,
              BYBFILE.BYB_UNITS_1_1_8,
              BYBFILE.BYB_OUTBAL_1_1_8,
              BYBFILE.BYB_UNITS_1_1_9,
              BYBFILE.BYB_OUTBAL_1_1_9,
              BYBFILE.BYB_UNITS_1_1_10,
              BYBFILE.BYB_OUTBAL_1_1_10,
              BYBFILE.BYB_UNITS_1_2_1,
              BYBFILE.BYB_OUTBAL_1_2_1,
              BYBFILE.BYB_UNITS_1_2_2,
              BYBFILE.BYB_OUTBAL_1_2_2,
              BYBFILE.BYB_UNITS_1_2_3,
              BYBFILE.BYB_OUTBAL_1_2_3,
              BYBFILE.BYB_UNITS_1_2_4,
              BYBFILE.BYB_OUTBAL_1_2_4,
              BYBFILE.BYB_UNITS_1_2_5,
              BYBFILE.BYB_OUTBAL_1_2_5,
              BYBFILE.BYB_UNITS_1_2_6,
              BYBFILE.BYB_OUTBAL_1_2_6,
              BYBFILE.BYB_UNITS_1_2_7,
              BYBFILE.BYB_OUTBAL_1_2_7,
              BYBFILE.BYB_UNITS_1_2_8,
              BYBFILE.BYB_OUTBAL_1_2_8,
              BYBFILE.BYB_UNITS_1_2_9,
              BYBFILE.BYB_OUTBAL_1_2_9,
              BYBFILE.BYB_UNITS_1_2_10,
              BYBFILE.BYB_OUTBAL_1_2_10,
              BYBFILE.BYB_UNITS_2_1_1,
              BYBFILE.BYB_OUTBAL_2_1_1,
              BYBFILE.BYB_UNITS_2_1_2,
              BYBFILE.BYB_OUTBAL_2_1_2,
              BYBFILE.BYB_UNITS_2_1_3,
              BYBFILE.BYB_OUTBAL_2_1_3,
              BYBFILE.BYB_UNITS_2_1_4,
              BYBFILE.BYB_OUTBAL_2_1_4,
              BYBFILE.BYB_UNITS_2_1_5,
              BYBFILE.BYB_OUTBAL_2_1_5,
              BYBFILE.BYB_UNITS_2_1_6,
              BYBFILE.BYB_OUTBAL_2_1_6,
              BYBFILE.BYB_UNITS_2_1_7,
              BYBFILE.BYB_OUTBAL_2_1_7,
              BYBFILE.BYB_UNITS_2_1_8,
              BYBFILE.BYB_OUTBAL_2_1_8,
              BYBFILE.BYB_UNITS_2_1_9,
              BYBFILE.BYB_OUTBAL_2_1_9,
              BYBFILE.BYB_UNITS_2_1_10,
              BYBFILE.BYB_OUTBAL_2_1_10,
              BYBFILE.BYB_UNITS_2_2_1,
              BYBFILE.BYB_OUTBAL_2_2_1,
              BYBFILE.BYB_UNITS_2_2_2,
              BYBFILE.BYB_OUTBAL_2_2_2,
              BYBFILE.BYB_UNITS_2_2_3,
              BYBFILE.BYB_OUTBAL_2_2_3,
              BYBFILE.BYB_UNITS_2_2_4,
              BYBFILE.BYB_OUTBAL_2_2_4,
              BYBFILE.BYB_UNITS_2_2_5,
              BYBFILE.BYB_OUTBAL_2_2_5,
              BYBFILE.BYB_UNITS_2_2_6,
              BYBFILE.BYB_OUTBAL_2_2_6,
              BYBFILE.BYB_UNITS_2_2_7,
              BYBFILE.BYB_OUTBAL_2_2_7,
              BYBFILE.BYB_UNITS_2_2_8,
              BYBFILE.BYB_OUTBAL_2_2_8,
              BYBFILE.BYB_UNITS_2_2_9,
              BYBFILE.BYB_OUTBAL_2_2_9,
              BYBFILE.BYB_UNITS_2_2_10,
              BYBFILE.BYB_OUTBAL_2_2_10,
              BYBFILE.BYB_UNITS_3_1_1,
              BYBFILE.BYB_OUTBAL_3_1_1,
              BYBFILE.BYB_UNITS_3_1_2,
              BYBFILE.BYB_OUTBAL_3_1_2,
              BYBFILE.BYB_UNITS_3_1_3,
              BYBFILE.BYB_OUTBAL_3_1_3,
              BYBFILE.BYB_UNITS_3_1_4,
              BYBFILE.BYB_OUTBAL_3_1_4,
              BYBFILE.BYB_UNITS_3_1_5,
              BYBFILE.BYB_OUTBAL_3_1_5,
              BYBFILE.BYB_UNITS_3_1_6,
              BYBFILE.BYB_OUTBAL_3_1_6,
              BYBFILE.BYB_UNITS_3_1_7,
              BYBFILE.BYB_OUTBAL_3_1_7,
              BYBFILE.BYB_UNITS_3_1_8,
              BYBFILE.BYB_OUTBAL_3_1_8,
              BYBFILE.BYB_UNITS_3_1_9,
              BYBFILE.BYB_OUTBAL_3_1_9,
              BYBFILE.BYB_UNITS_3_1_10,
              BYBFILE.BYB_OUTBAL_3_1_10,
              BYBFILE.BYB_UNITS_3_2_1,
              BYBFILE.BYB_OUTBAL_3_2_1,
              BYBFILE.BYB_UNITS_3_2_2,
              BYBFILE.BYB_OUTBAL_3_2_2,
              BYBFILE.BYB_UNITS_3_2_3,
              BYBFILE.BYB_OUTBAL_3_2_3,
              BYBFILE.BYB_UNITS_3_2_4,
              BYBFILE.BYB_OUTBAL_3_2_4,
              BYBFILE.BYB_UNITS_3_2_5,
              BYBFILE.BYB_OUTBAL_3_2_5,
              BYBFILE.BYB_UNITS_3_2_6,
              BYBFILE.BYB_OUTBAL_3_2_6,
              BYBFILE.BYB_UNITS_3_2_7,
              BYBFILE.BYB_OUTBAL_3_2_7,
              BYBFILE.BYB_UNITS_3_2_8,
              BYBFILE.BYB_OUTBAL_3_2_8,
              BYBFILE.BYB_UNITS_3_2_9,
              BYBFILE.BYB_OUTBAL_3_2_9,
              BYBFILE.BYB_UNITS_3_2_10,
              BYBFILE.BYB_OUTBAL_3_2_10,
              BYBFILE.BYB_UNITS_BNKRPT_1_1_1,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_1_1,
              BYBFILE.BYB_UNITS_BNKRPT_1_1_2,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_1_2,
              BYBFILE.BYB_UNITS_BNKRPT_1_1_3,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_1_3,
              BYBFILE.BYB_UNITS_BNKRPT_1_1_4,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_1_4,
              BYBFILE.BYB_UNITS_BNKRPT_1_1_5,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_1_5,
              BYBFILE.BYB_UNITS_BNKRPT_1_1_6,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_1_6,
              BYBFILE.BYB_UNITS_BNKRPT_1_1_7,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_1_7,
              BYBFILE.BYB_UNITS_BNKRPT_1_1_8,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_1_8,
              BYBFILE.BYB_UNITS_BNKRPT_1_1_9,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_1_9,
              BYBFILE.BYB_UNITS_BNKRPT_1_1_10,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_1_10,
              BYBFILE.BYB_UNITS_BNKRPT_1_2_1,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_2_1,
              BYBFILE.BYB_UNITS_BNKRPT_1_2_2,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_2_2,
              BYBFILE.BYB_UNITS_BNKRPT_1_2_3,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_2_3,
              BYBFILE.BYB_UNITS_BNKRPT_1_2_4,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_2_4,
              BYBFILE.BYB_UNITS_BNKRPT_1_2_5,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_2_5,
              BYBFILE.BYB_UNITS_BNKRPT_1_2_6,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_2_6,
              BYBFILE.BYB_UNITS_BNKRPT_1_2_7,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_2_7,
              BYBFILE.BYB_UNITS_BNKRPT_1_2_8,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_2_8,
              BYBFILE.BYB_UNITS_BNKRPT_1_2_9,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_2_9,
              BYBFILE.BYB_UNITS_BNKRPT_1_2_10,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_2_10,
              BYBFILE.BYB_UNITS_BNKRPT_2_1_1,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_1_1,
              BYBFILE.BYB_UNITS_BNKRPT_2_1_2,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_1_2,
              BYBFILE.BYB_UNITS_BNKRPT_2_1_3,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_1_3,
              BYBFILE.BYB_UNITS_BNKRPT_2_1_4,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_1_4,
              BYBFILE.BYB_UNITS_BNKRPT_2_1_5,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_1_5,
              BYBFILE.BYB_UNITS_BNKRPT_2_1_6,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_1_6,
              BYBFILE.BYB_UNITS_BNKRPT_2_1_7,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_1_7,
              BYBFILE.BYB_UNITS_BNKRPT_2_1_8,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_1_8,
              BYBFILE.BYB_UNITS_BNKRPT_2_1_9,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_1_9,
              BYBFILE.BYB_UNITS_BNKRPT_2_1_10,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_1_10,
              BYBFILE.BYB_UNITS_BNKRPT_2_2_1,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_2_1,
              BYBFILE.BYB_UNITS_BNKRPT_2_2_2,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_2_2,
              BYBFILE.BYB_UNITS_BNKRPT_2_2_3,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_2_3,
              BYBFILE.BYB_UNITS_BNKRPT_2_2_4,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_2_4,
              BYBFILE.BYB_UNITS_BNKRPT_2_2_5,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_2_5,
              BYBFILE.BYB_UNITS_BNKRPT_2_2_6,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_2_6,
              BYBFILE.BYB_UNITS_BNKRPT_2_2_7,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_2_7,
              BYBFILE.BYB_UNITS_BNKRPT_2_2_8,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_2_8,
              BYBFILE.BYB_UNITS_BNKRPT_2_2_9,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_2_9,
              BYBFILE.BYB_UNITS_BNKRPT_2_2_10,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_2_10,
              BYBFILE.BYB_UNITS_BNKRPT_3_1_1,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_1_1,
              BYBFILE.BYB_UNITS_BNKRPT_3_1_2,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_1_2,
              BYBFILE.BYB_UNITS_BNKRPT_3_1_3,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_1_3,
              BYBFILE.BYB_UNITS_BNKRPT_3_1_4,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_1_4,
              BYBFILE.BYB_UNITS_BNKRPT_3_1_5,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_1_5,
              BYBFILE.BYB_UNITS_BNKRPT_3_1_6,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_1_6,
              BYBFILE.BYB_UNITS_BNKRPT_3_1_7,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_1_7,
              BYBFILE.BYB_UNITS_BNKRPT_3_1_8,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_1_8,
              BYBFILE.BYB_UNITS_BNKRPT_3_1_9,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_1_9,
              BYBFILE.BYB_UNITS_BNKRPT_3_1_10,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_1_10,
              BYBFILE.BYB_UNITS_BNKRPT_3_2_1,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_2_1,
              BYBFILE.BYB_UNITS_BNKRPT_3_2_2,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_2_2,
              BYBFILE.BYB_UNITS_BNKRPT_3_2_3,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_2_3,
              BYBFILE.BYB_UNITS_BNKRPT_3_2_4,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_2_4,
              BYBFILE.BYB_UNITS_BNKRPT_3_2_5,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_2_5,
              BYBFILE.BYB_UNITS_BNKRPT_3_2_6,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_2_6,
              BYBFILE.BYB_UNITS_BNKRPT_3_2_7,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_2_7,
              BYBFILE.BYB_UNITS_BNKRPT_3_2_8,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_2_8,
              BYBFILE.BYB_UNITS_BNKRPT_3_2_9,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_2_9,
              BYBFILE.BYB_UNITS_BNKRPT_3_2_10,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_2_10,
              BYBFILE.BYB_SUMWGTTM,
              BYBFILE.BYB_SUMWGTAV,
              BYBFILE.BYB_SUMWGTTA,
              BYBFILE.BYB_IB_ACCRUED,
              BYBFILE.BYB_DLDISC_1,
              BYBFILE.BYB_DLDISC_2,
              BYBFILE.BYB_DLDISC_3,
              BYBFILE.BYB_STD_POS,
              BYBFILE.BYB_30_DAY_MOVED,
              BYBFILE.BYB_PROJ_INCREASE,
              BYBFILE.BYB_PROJ_PROFIT,
              BYBFILE.BYB_PROCEEDS_1,
              BYBFILE.BYB_PROCEEDS_2,
              BYBFILE.BYB_LOCKBOX_1,
              BYBFILE.BYB_LOCKBOX_2,
              BYBFILE.BYB_UNEARNED_AMT_1_1_1,
              BYBFILE.BYB_UNEARNED_AMT_1_1_2,
              BYBFILE.BYB_UNEARNED_AMT_1_1_3,
              BYBFILE.BYB_UNEARNED_AMT_1_1_4,
              BYBFILE.BYB_UNEARNED_AMT_1_1_5,
              BYBFILE.BYB_UNEARNED_AMT_1_1_6,
              BYBFILE.BYB_UNEARNED_AMT_1_1_7,
              BYBFILE.BYB_UNEARNED_AMT_1_1_8,
              BYBFILE.BYB_UNEARNED_AMT_1_1_9,
              BYBFILE.BYB_UNEARNED_AMT_1_1_10,
              BYBFILE.BYB_UNEARNED_AMT_1_2_1,
              BYBFILE.BYB_UNEARNED_AMT_1_2_2,
              BYBFILE.BYB_UNEARNED_AMT_1_2_3,
              BYBFILE.BYB_UNEARNED_AMT_1_2_4,
              BYBFILE.BYB_UNEARNED_AMT_1_2_5,
              BYBFILE.BYB_UNEARNED_AMT_1_2_6,
              BYBFILE.BYB_UNEARNED_AMT_1_2_7,
              BYBFILE.BYB_UNEARNED_AMT_1_2_8,
              BYBFILE.BYB_UNEARNED_AMT_1_2_9,
              BYBFILE.BYB_UNEARNED_AMT_1_2_10,
              BYBFILE.BYB_UNEARNED_AMT_2_1_1,
              BYBFILE.BYB_UNEARNED_AMT_2_1_2,
              BYBFILE.BYB_UNEARNED_AMT_2_1_3,
              BYBFILE.BYB_UNEARNED_AMT_2_1_4,
              BYBFILE.BYB_UNEARNED_AMT_2_1_5,
              BYBFILE.BYB_UNEARNED_AMT_2_1_6,
              BYBFILE.BYB_UNEARNED_AMT_2_1_7,
              BYBFILE.BYB_UNEARNED_AMT_2_1_8,
              BYBFILE.BYB_UNEARNED_AMT_2_1_9,
              BYBFILE.BYB_UNEARNED_AMT_2_1_10,
              BYBFILE.BYB_UNEARNED_AMT_2_2_1,
              BYBFILE.BYB_UNEARNED_AMT_2_2_2,
              BYBFILE.BYB_UNEARNED_AMT_2_2_3,
              BYBFILE.BYB_UNEARNED_AMT_2_2_4,
              BYBFILE.BYB_UNEARNED_AMT_2_2_5,
              BYBFILE.BYB_UNEARNED_AMT_2_2_6,
              BYBFILE.BYB_UNEARNED_AMT_2_2_7,
              BYBFILE.BYB_UNEARNED_AMT_2_2_8,
              BYBFILE.BYB_UNEARNED_AMT_2_2_9,
              BYBFILE.BYB_UNEARNED_AMT_2_2_10,
              BYBFILE.BYB_UNEARNED_AMT_3_1_1,
              BYBFILE.BYB_UNEARNED_AMT_3_1_2,
              BYBFILE.BYB_UNEARNED_AMT_3_1_3,
              BYBFILE.BYB_UNEARNED_AMT_3_1_4,
              BYBFILE.BYB_UNEARNED_AMT_3_1_5,
              BYBFILE.BYB_UNEARNED_AMT_3_1_6,
              BYBFILE.BYB_UNEARNED_AMT_3_1_7,
              BYBFILE.BYB_UNEARNED_AMT_3_1_8,
              BYBFILE.BYB_UNEARNED_AMT_3_1_9,
              BYBFILE.BYB_UNEARNED_AMT_3_1_10,
              BYBFILE.BYB_UNEARNED_AMT_3_2_1,
              BYBFILE.BYB_UNEARNED_AMT_3_2_2,
              BYBFILE.BYB_UNEARNED_AMT_3_2_3,
              BYBFILE.BYB_UNEARNED_AMT_3_2_4,
              BYBFILE.BYB_UNEARNED_AMT_3_2_5,
              BYBFILE.BYB_UNEARNED_AMT_3_2_6,
              BYBFILE.BYB_UNEARNED_AMT_3_2_7,
              BYBFILE.BYB_UNEARNED_AMT_3_2_8,
              BYBFILE.BYB_UNEARNED_AMT_3_2_9,
              BYBFILE.BYB_UNEARNED_AMT_3_2_10)
            VALUES
             (:QBYB-DATE-CCYYMM,
              :QBYB-BRNO,
              :QBYB-CLASS,
              :QBYB-PL-PRINCOL-1-1,
              :QBYB-PL-INTCOL-1-1,
              :QBYB-PL-LATECOL-1-1,
              :QBYB-PL-O2COL-1-1,
              :QBYB-PL-PRINCOL-1-2,
              :QBYB-PL-INTCOL-1-2,
              :QBYB-PL-LATECOL-1-2,
              :QBYB-PL-O2COL-1-2,
              :QBYB-PL-PRINCOL-1-3,
              :QBYB-PL-INTCOL-1-3,
              :QBYB-PL-LATECOL-1-3,
              :QBYB-PL-O2COL-1-3,
              :QBYB-PL-PRINCOL-1-4,
              :QBYB-PL-INTCOL-1-4,
              :QBYB-PL-LATECOL-1-4,
              :QBYB-PL-O2COL-1-4,
              :QBYB-PL-PRINCOL-1-5,
              :QBYB-PL-INTCOL-1-5,
              :QBYB-PL-LATECOL-1-5,
              :QBYB-PL-O2COL-1-5,
              :QBYB-AMT-SOLD-1,
              :QBYB-AMT-TRANS-1,
              :QBYB-AMT-PURCH-1,
              :QBYB-NO-SOLD-1,
              :QBYB-NO-TRANS-1,
              :QBYB-NO-PURCH-1,
              :QBYB-OTHCOL-1,
              :QBYB-OT2COL-1,
              :QBYB-INTCOL-1,
              :QBYB-LATECOL-1,
              :QBYB-PRINCOL-1,
              :QBYB-DEFCOL-1,
              :QBYB-RNPRIN-1,
              :QBYB-INTREB-1,
              :QBYB-CLREB1-1,
              :QBYB-CLREB2-1,
              :QBYB-CLREB3-1,
              :QBYB-CLREB4-1,
              :QBYB-AHREB-1,
              :QBYB-PPREB-1,
              :QBYB-O1REB-1,
              :QBYB-O2REB-1,
              :QBYB-O3REB-1,
              :QBYB-O4REB-1,
              :QBYB-O5REB-1,
              :QBYB-O6REB-1,
              :QBYB-O7REB-1,
              :QBYB-SCREB-1,
              :QBYB-MFREB-1,
              :QBYB-DFREB-1,
              :QBYB-UN7-REB-1,
              :QBYB-UN8-REB-1,
              :QBYB-UN9-REB-1,
              :QBYB-UN10-REB-1,
              :QBYB-PART-CHGOFF-1,
              :QBYB-ALLOTMENTS-1,
              :QBYB-INS-NO-ELIG-1-1,
              :QBYB-INS-NO-WROTE-1-1,
              :QBYB-INS-NO-NOTWROTE-1-1,
              :QBYB-INS-AMT-WROTE-1-1,
              :QBYB-INS-NO-ELIG-1-2,
              :QBYB-INS-NO-WROTE-1-2,
              :QBYB-INS-NO-NOTWROTE-1-2,
              :QBYB-INS-AMT-WROTE-1-2,
              :QBYB-INS-NO-ELIG-1-3,
              :QBYB-INS-NO-WROTE-1-3,
              :QBYB-INS-NO-NOTWROTE-1-3,
              :QBYB-INS-AMT-WROTE-1-3,
              :QBYB-INS-NO-ELIG-1-4,
              :QBYB-INS-NO-WROTE-1-4,
              :QBYB-INS-NO-NOTWROTE-1-4,
              :QBYB-INS-AMT-WROTE-1-4,
              :QBYB-INS-NO-ELIG-1-5,
              :QBYB-INS-NO-WROTE-1-5,
              :QBYB-INS-NO-NOTWROTE-1-5,
              :QBYB-INS-AMT-WROTE-1-5,
              :QBYB-INS-NO-ELIG-1-6,
              :QBYB-INS-NO-WROTE-1-6,
              :QBYB-INS-NO-NOTWROTE-1-6,
              :QBYB-INS-AMT-WROTE-1-6,
              :QBYB-INS-NO-ELIG-1-7,
              :QBYB-INS-NO-WROTE-1-7,
              :QBYB-INS-NO-NOTWROTE-1-7,
              :QBYB-INS-AMT-WROTE-1-7,
              :QBYB-INS-NO-ELIG-1-8,
              :QBYB-INS-NO-WROTE-1-8,
              :QBYB-INS-NO-NOTWROTE-1-8,
              :QBYB-INS-AMT-WROTE-1-8,
              :QBYB-INS-NO-ELIG-1-9,
              :QBYB-INS-NO-WROTE-1-9,
              :QBYB-INS-NO-NOTWROTE-1-9,
              :QBYB-INS-AMT-WROTE-1-9,
              :QBYB-INS-NO-ELIG-1-10,
              :QBYB-INS-NO-WROTE-1-10,
              :QBYB-INS-NO-NOTWROTE-1-10,
              :QBYB-INS-AMT-WROTE-1-10,
              :QBYB-FEE-NO-ELIG-1-1,
              :QBYB-FEE-NO-WROTE-1-1,
              :QBYB-FEE-NO-NOTWROTE-1-1,
              :QBYB-FEE-AMT-WROTE-1-1,
              :QBYB-FEE-NO-ELIG-1-2,
              :QBYB-FEE-NO-WROTE-1-2,
              :QBYB-FEE-NO-NOTWROTE-1-2,
              :QBYB-FEE-AMT-WROTE-1-2,
              :QBYB-FEE-NO-ELIG-1-3,
              :QBYB-FEE-NO-WROTE-1-3,
              :QBYB-FEE-NO-NOTWROTE-1-3,
              :QBYB-FEE-AMT-WROTE-1-3,
              :QBYB-OTH-NO-COL-1,
              :QBYB-OT2-NO-COL-1,
              :QBYB-INT-NO-COL-1,
              :QBYB-LATE-NO-COL-1,
              :QBYB-OTH-NO-WAIVED-1,
              :QBYB-OT2-NO-WAIVED-1,
              :QBYB-INT-NO-WAIVED-1,
              :QBYB-LATE-NO-WAIVED-1,
              :QBYB-OTH-AMT-WAIVED-1,
              :QBYB-OT2-AMT-WAIVED-1,
              :QBYB-INT-AMT-WAIVED-1,
              :QBYB-LATE-AMT-WAIVED-1,
              :QBYB-PL-PRINCOL-2-1,
              :QBYB-PL-INTCOL-2-1,
              :QBYB-PL-LATECOL-2-1,
              :QBYB-PL-O2COL-2-1,
              :QBYB-PL-PRINCOL-2-2,
              :QBYB-PL-INTCOL-2-2,
              :QBYB-PL-LATECOL-2-2,
              :QBYB-PL-O2COL-2-2,
              :QBYB-PL-PRINCOL-2-3,
              :QBYB-PL-INTCOL-2-3,
              :QBYB-PL-LATECOL-2-3,
              :QBYB-PL-O2COL-2-3,
              :QBYB-PL-PRINCOL-2-4,
              :QBYB-PL-INTCOL-2-4,
              :QBYB-PL-LATECOL-2-4,
              :QBYB-PL-O2COL-2-4,
              :QBYB-PL-PRINCOL-2-5,
              :QBYB-PL-INTCOL-2-5,
              :QBYB-PL-LATECOL-2-5,
              :QBYB-PL-O2COL-2-5,
              :QBYB-AMT-SOLD-2,
              :QBYB-AMT-TRANS-2,
              :QBYB-AMT-PURCH-2,
              :QBYB-NO-SOLD-2,
              :QBYB-NO-TRANS-2,
              :QBYB-NO-PURCH-2,
              :QBYB-OTHCOL-2,
              :QBYB-OT2COL-2,
              :QBYB-INTCOL-2,
              :QBYB-LATECOL-2,
              :QBYB-PRINCOL-2,
              :QBYB-DEFCOL-2,
              :QBYB-RNPRIN-2,
              :QBYB-INTREB-2,
              :QBYB-CLREB1-2,
              :QBYB-CLREB2-2,
              :QBYB-CLREB3-2,
              :QBYB-CLREB4-2,
              :QBYB-AHREB-2,
              :QBYB-PPREB-2,
              :QBYB-O1REB-2,
              :QBYB-O2REB-2,
              :QBYB-O3REB-2,
              :QBYB-O4REB-2,
              :QBYB-O5REB-2,
              :QBYB-O6REB-2,
              :QBYB-O7REB-2,
              :QBYB-SCREB-2,
              :QBYB-MFREB-2,
              :QBYB-DFREB-2,
              :QBYB-UN7-REB-2,
              :QBYB-UN8-REB-2,
              :QBYB-UN9-REB-2,
              :QBYB-UN10-REB-2,
              :QBYB-PART-CHGOFF-2,
              :QBYB-ALLOTMENTS-2,
              :QBYB-INS-NO-ELIG-2-1,
              :QBYB-INS-NO-WROTE-2-1,
              :QBYB-INS-NO-NOTWROTE-2-1,
              :QBYB-INS-AMT-WROTE-2-1,
              :QBYB-INS-NO-ELIG-2-2,
              :QBYB-INS-NO-WROTE-2-2,
              :QBYB-INS-NO-NOTWROTE-2-2,
              :QBYB-INS-AMT-WROTE-2-2,
              :QBYB-INS-NO-ELIG-2-3,
              :QBYB-INS-NO-WROTE-2-3,
              :QBYB-INS-NO-NOTWROTE-2-3,
              :QBYB-INS-AMT-WROTE-2-3,
              :QBYB-INS-NO-ELIG-2-4,
              :QBYB-INS-NO-WROTE-2-4,
              :QBYB-INS-NO-NOTWROTE-2-4,
              :QBYB-INS-AMT-WROTE-2-4,
              :QBYB-INS-NO-ELIG-2-5,
              :QBYB-INS-NO-WROTE-2-5,
              :QBYB-INS-NO-NOTWROTE-2-5,
              :QBYB-INS-AMT-WROTE-2-5,
              :QBYB-INS-NO-ELIG-2-6,
              :QBYB-INS-NO-WROTE-2-6,
              :QBYB-INS-NO-NOTWROTE-2-6,
              :QBYB-INS-AMT-WROTE-2-6,
              :QBYB-INS-NO-ELIG-2-7,
              :QBYB-INS-NO-WROTE-2-7,
              :QBYB-INS-NO-NOTWROTE-2-7,
              :QBYB-INS-AMT-WROTE-2-7,
              :QBYB-INS-NO-ELIG-2-8,
              :QBYB-INS-NO-WROTE-2-8,
              :QBYB-INS-NO-NOTWROTE-2-8,
              :QBYB-INS-AMT-WROTE-2-8,
              :QBYB-INS-NO-ELIG-2-9,
              :QBYB-INS-NO-WROTE-2-9,
              :QBYB-INS-NO-NOTWROTE-2-9,
              :QBYB-INS-AMT-WROTE-2-9,
              :QBYB-INS-NO-ELIG-2-10,
              :QBYB-INS-NO-WROTE-2-10,
              :QBYB-INS-NO-NOTWROTE-2-10,
              :QBYB-INS-AMT-WROTE-2-10,
              :QBYB-FEE-NO-ELIG-2-1,
              :QBYB-FEE-NO-WROTE-2-1,
              :QBYB-FEE-NO-NOTWROTE-2-1,
              :QBYB-FEE-AMT-WROTE-2-1,
              :QBYB-FEE-NO-ELIG-2-2,
              :QBYB-FEE-NO-WROTE-2-2,
              :QBYB-FEE-NO-NOTWROTE-2-2,
              :QBYB-FEE-AMT-WROTE-2-2,
              :QBYB-FEE-NO-ELIG-2-3,
              :QBYB-FEE-NO-WROTE-2-3,
              :QBYB-FEE-NO-NOTWROTE-2-3,
              :QBYB-FEE-AMT-WROTE-2-3,
              :QBYB-OTH-NO-COL-2,
              :QBYB-OT2-NO-COL-2,
              :QBYB-INT-NO-COL-2,
              :QBYB-LATE-NO-COL-2,
              :QBYB-OTH-NO-WAIVED-2,
              :QBYB-OT2-NO-WAIVED-2,
              :QBYB-INT-NO-WAIVED-2,
              :QBYB-LATE-NO-WAIVED-2,
              :QBYB-OTH-AMT-WAIVED-2,
              :QBYB-OT2-AMT-WAIVED-2,
              :QBYB-INT-AMT-WAIVED-2,
              :QBYB-LATE-AMT-WAIVED-2,
              :QBYB-UNITS-1-1-1,
              :QBYB-OUTBAL-1-1-1,
              :QBYB-UNITS-1-1-2,
              :QBYB-OUTBAL-1-1-2,
              :QBYB-UNITS-1-1-3,
              :QBYB-OUTBAL-1-1-3,
              :QBYB-UNITS-1-1-4,
              :QBYB-OUTBAL-1-1-4,
              :QBYB-UNITS-1-1-5,
              :QBYB-OUTBAL-1-1-5,
              :QBYB-UNITS-1-1-6,
              :QBYB-OUTBAL-1-1-6,
              :QBYB-UNITS-1-1-7,
              :QBYB-OUTBAL-1-1-7,
              :QBYB-UNITS-1-1-8,
              :QBYB-OUTBAL-1-1-8,
              :QBYB-UNITS-1-1-9,
              :QBYB-OUTBAL-1-1-9,
              :QBYB-UNITS-1-1-10,
              :QBYB-OUTBAL-1-1-10,
              :QBYB-UNITS-1-2-1,
              :QBYB-OUTBAL-1-2-1,
              :QBYB-UNITS-1-2-2,
              :QBYB-OUTBAL-1-2-2,
              :QBYB-UNITS-1-2-3,
              :QBYB-OUTBAL-1-2-3,
              :QBYB-UNITS-1-2-4,
              :QBYB-OUTBAL-1-2-4,
              :QBYB-UNITS-1-2-5,
              :QBYB-OUTBAL-1-2-5,
              :QBYB-UNITS-1-2-6,
              :QBYB-OUTBAL-1-2-6,
              :QBYB-UNITS-1-2-7,
              :QBYB-OUTBAL-1-2-7,
              :QBYB-UNITS-1-2-8,
              :QBYB-OUTBAL-1-2-8,
              :QBYB-UNITS-1-2-9,
              :QBYB-OUTBAL-1-2-9,
              :QBYB-UNITS-1-2-10,
              :QBYB-OUTBAL-1-2-10,
              :QBYB-UNITS-2-1-1,
              :QBYB-OUTBAL-2-1-1,
              :QBYB-UNITS-2-1-2,
              :QBYB-OUTBAL-2-1-2,
              :QBYB-UNITS-2-1-3,
              :QBYB-OUTBAL-2-1-3,
              :QBYB-UNITS-2-1-4,
              :QBYB-OUTBAL-2-1-4,
              :QBYB-UNITS-2-1-5,
              :QBYB-OUTBAL-2-1-5,
              :QBYB-UNITS-2-1-6,
              :QBYB-OUTBAL-2-1-6,
              :QBYB-UNITS-2-1-7,
              :QBYB-OUTBAL-2-1-7,
              :QBYB-UNITS-2-1-8,
              :QBYB-OUTBAL-2-1-8,
              :QBYB-UNITS-2-1-9,
              :QBYB-OUTBAL-2-1-9,
              :QBYB-UNITS-2-1-10,
              :QBYB-OUTBAL-2-1-10,
              :QBYB-UNITS-2-2-1,
              :QBYB-OUTBAL-2-2-1,
              :QBYB-UNITS-2-2-2,
              :QBYB-OUTBAL-2-2-2,
              :QBYB-UNITS-2-2-3,
              :QBYB-OUTBAL-2-2-3,
              :QBYB-UNITS-2-2-4,
              :QBYB-OUTBAL-2-2-4,
              :QBYB-UNITS-2-2-5,
              :QBYB-OUTBAL-2-2-5,
              :QBYB-UNITS-2-2-6,
              :QBYB-OUTBAL-2-2-6,
              :QBYB-UNITS-2-2-7,
              :QBYB-OUTBAL-2-2-7,
              :QBYB-UNITS-2-2-8,
              :QBYB-OUTBAL-2-2-8,
              :QBYB-UNITS-2-2-9,
              :QBYB-OUTBAL-2-2-9,
              :QBYB-UNITS-2-2-10,
              :QBYB-OUTBAL-2-2-10,
              :QBYB-UNITS-3-1-1,
              :QBYB-OUTBAL-3-1-1,
              :QBYB-UNITS-3-1-2,
              :QBYB-OUTBAL-3-1-2,
              :QBYB-UNITS-3-1-3,
              :QBYB-OUTBAL-3-1-3,
              :QBYB-UNITS-3-1-4,
              :QBYB-OUTBAL-3-1-4,
              :QBYB-UNITS-3-1-5,
              :QBYB-OUTBAL-3-1-5,
              :QBYB-UNITS-3-1-6,
              :QBYB-OUTBAL-3-1-6,
              :QBYB-UNITS-3-1-7,
              :QBYB-OUTBAL-3-1-7,
              :QBYB-UNITS-3-1-8,
              :QBYB-OUTBAL-3-1-8,
              :QBYB-UNITS-3-1-9,
              :QBYB-OUTBAL-3-1-9,
              :QBYB-UNITS-3-1-10,
              :QBYB-OUTBAL-3-1-10,
              :QBYB-UNITS-3-2-1,
              :QBYB-OUTBAL-3-2-1,
              :QBYB-UNITS-3-2-2,
              :QBYB-OUTBAL-3-2-2,
              :QBYB-UNITS-3-2-3,
              :QBYB-OUTBAL-3-2-3,
              :QBYB-UNITS-3-2-4,
              :QBYB-OUTBAL-3-2-4,
              :QBYB-UNITS-3-2-5,
              :QBYB-OUTBAL-3-2-5,
              :QBYB-UNITS-3-2-6,
              :QBYB-OUTBAL-3-2-6,
              :QBYB-UNITS-3-2-7,
              :QBYB-OUTBAL-3-2-7,
              :QBYB-UNITS-3-2-8,
              :QBYB-OUTBAL-3-2-8,
              :QBYB-UNITS-3-2-9,
              :QBYB-OUTBAL-3-2-9,
              :QBYB-UNITS-3-2-10,
              :QBYB-OUTBAL-3-2-10,
              :QBYB-UNITS-BNKRPT-1-1-1,
              :QBYB-OUTBAL-BNKRPT-1-1-1,
              :QBYB-UNITS-BNKRPT-1-1-2,
              :QBYB-OUTBAL-BNKRPT-1-1-2,
              :QBYB-UNITS-BNKRPT-1-1-3,
              :QBYB-OUTBAL-BNKRPT-1-1-3,
              :QBYB-UNITS-BNKRPT-1-1-4,
              :QBYB-OUTBAL-BNKRPT-1-1-4,
              :QBYB-UNITS-BNKRPT-1-1-5,
              :QBYB-OUTBAL-BNKRPT-1-1-5,
              :QBYB-UNITS-BNKRPT-1-1-6,
              :QBYB-OUTBAL-BNKRPT-1-1-6,
              :QBYB-UNITS-BNKRPT-1-1-7,
              :QBYB-OUTBAL-BNKRPT-1-1-7,
              :QBYB-UNITS-BNKRPT-1-1-8,
              :QBYB-OUTBAL-BNKRPT-1-1-8,
              :QBYB-UNITS-BNKRPT-1-1-9,
              :QBYB-OUTBAL-BNKRPT-1-1-9,
              :QBYB-UNITS-BNKRPT-1-1-10,
              :QBYB-OUTBAL-BNKRPT-1-1-10,
              :QBYB-UNITS-BNKRPT-1-2-1,
              :QBYB-OUTBAL-BNKRPT-1-2-1,
              :QBYB-UNITS-BNKRPT-1-2-2,
              :QBYB-OUTBAL-BNKRPT-1-2-2,
              :QBYB-UNITS-BNKRPT-1-2-3,
              :QBYB-OUTBAL-BNKRPT-1-2-3,
              :QBYB-UNITS-BNKRPT-1-2-4,
              :QBYB-OUTBAL-BNKRPT-1-2-4,
              :QBYB-UNITS-BNKRPT-1-2-5,
              :QBYB-OUTBAL-BNKRPT-1-2-5,
              :QBYB-UNITS-BNKRPT-1-2-6,
              :QBYB-OUTBAL-BNKRPT-1-2-6,
              :QBYB-UNITS-BNKRPT-1-2-7,
              :QBYB-OUTBAL-BNKRPT-1-2-7,
              :QBYB-UNITS-BNKRPT-1-2-8,
              :QBYB-OUTBAL-BNKRPT-1-2-8,
              :QBYB-UNITS-BNKRPT-1-2-9,
              :QBYB-OUTBAL-BNKRPT-1-2-9,
              :QBYB-UNITS-BNKRPT-1-2-10,
              :QBYB-OUTBAL-BNKRPT-1-2-10,
              :QBYB-UNITS-BNKRPT-2-1-1,
              :QBYB-OUTBAL-BNKRPT-2-1-1,
              :QBYB-UNITS-BNKRPT-2-1-2,
              :QBYB-OUTBAL-BNKRPT-2-1-2,
              :QBYB-UNITS-BNKRPT-2-1-3,
              :QBYB-OUTBAL-BNKRPT-2-1-3,
              :QBYB-UNITS-BNKRPT-2-1-4,
              :QBYB-OUTBAL-BNKRPT-2-1-4,
              :QBYB-UNITS-BNKRPT-2-1-5,
              :QBYB-OUTBAL-BNKRPT-2-1-5,
              :QBYB-UNITS-BNKRPT-2-1-6,
              :QBYB-OUTBAL-BNKRPT-2-1-6,
              :QBYB-UNITS-BNKRPT-2-1-7,
              :QBYB-OUTBAL-BNKRPT-2-1-7,
              :QBYB-UNITS-BNKRPT-2-1-8,
              :QBYB-OUTBAL-BNKRPT-2-1-8,
              :QBYB-UNITS-BNKRPT-2-1-9,
              :QBYB-OUTBAL-BNKRPT-2-1-9,
              :QBYB-UNITS-BNKRPT-2-1-10,
              :QBYB-OUTBAL-BNKRPT-2-1-10,
              :QBYB-UNITS-BNKRPT-2-2-1,
              :QBYB-OUTBAL-BNKRPT-2-2-1,
              :QBYB-UNITS-BNKRPT-2-2-2,
              :QBYB-OUTBAL-BNKRPT-2-2-2,
              :QBYB-UNITS-BNKRPT-2-2-3,
              :QBYB-OUTBAL-BNKRPT-2-2-3,
              :QBYB-UNITS-BNKRPT-2-2-4,
              :QBYB-OUTBAL-BNKRPT-2-2-4,
              :QBYB-UNITS-BNKRPT-2-2-5,
              :QBYB-OUTBAL-BNKRPT-2-2-5,
              :QBYB-UNITS-BNKRPT-2-2-6,
              :QBYB-OUTBAL-BNKRPT-2-2-6,
              :QBYB-UNITS-BNKRPT-2-2-7,
              :QBYB-OUTBAL-BNKRPT-2-2-7,
              :QBYB-UNITS-BNKRPT-2-2-8,
              :QBYB-OUTBAL-BNKRPT-2-2-8,
              :QBYB-UNITS-BNKRPT-2-2-9,
              :QBYB-OUTBAL-BNKRPT-2-2-9,
              :QBYB-UNITS-BNKRPT-2-2-10,
              :QBYB-OUTBAL-BNKRPT-2-2-10,
              :QBYB-UNITS-BNKRPT-3-1-1,
              :QBYB-OUTBAL-BNKRPT-3-1-1,
              :QBYB-UNITS-BNKRPT-3-1-2,
              :QBYB-OUTBAL-BNKRPT-3-1-2,
              :QBYB-UNITS-BNKRPT-3-1-3,
              :QBYB-OUTBAL-BNKRPT-3-1-3,
              :QBYB-UNITS-BNKRPT-3-1-4,
              :QBYB-OUTBAL-BNKRPT-3-1-4,
              :QBYB-UNITS-BNKRPT-3-1-5,
              :QBYB-OUTBAL-BNKRPT-3-1-5,
              :QBYB-UNITS-BNKRPT-3-1-6,
              :QBYB-OUTBAL-BNKRPT-3-1-6,
              :QBYB-UNITS-BNKRPT-3-1-7,
              :QBYB-OUTBAL-BNKRPT-3-1-7,
              :QBYB-UNITS-BNKRPT-3-1-8,
              :QBYB-OUTBAL-BNKRPT-3-1-8,
              :QBYB-UNITS-BNKRPT-3-1-9,
              :QBYB-OUTBAL-BNKRPT-3-1-9,
              :QBYB-UNITS-BNKRPT-3-1-10,
              :QBYB-OUTBAL-BNKRPT-3-1-10,
              :QBYB-UNITS-BNKRPT-3-2-1,
              :QBYB-OUTBAL-BNKRPT-3-2-1,
              :QBYB-UNITS-BNKRPT-3-2-2,
              :QBYB-OUTBAL-BNKRPT-3-2-2,
              :QBYB-UNITS-BNKRPT-3-2-3,
              :QBYB-OUTBAL-BNKRPT-3-2-3,
              :QBYB-UNITS-BNKRPT-3-2-4,
              :QBYB-OUTBAL-BNKRPT-3-2-4,
              :QBYB-UNITS-BNKRPT-3-2-5,
              :QBYB-OUTBAL-BNKRPT-3-2-5,
              :QBYB-UNITS-BNKRPT-3-2-6,
              :QBYB-OUTBAL-BNKRPT-3-2-6,
              :QBYB-UNITS-BNKRPT-3-2-7,
              :QBYB-OUTBAL-BNKRPT-3-2-7,
              :QBYB-UNITS-BNKRPT-3-2-8,
              :QBYB-OUTBAL-BNKRPT-3-2-8,
              :QBYB-UNITS-BNKRPT-3-2-9,
              :QBYB-OUTBAL-BNKRPT-3-2-9,
              :QBYB-UNITS-BNKRPT-3-2-10,
              :QBYB-OUTBAL-BNKRPT-3-2-10,
              :QBYB-SUMWGTTM,
              :QBYB-SUMWGTAV,
              :QBYB-SUMWGTTA,
              :QBYB-IB-ACCRUED,
              :QBYB-DLDISC-1,
              :QBYB-DLDISC-2,
              :QBYB-DLDISC-3,
              :QBYB-STD-POS,
              :QBYB-30-DAY-MOVED,
              :QBYB-PROJ-INCREASE,
              :QBYB-PROJ-PROFIT,
              :QBYB-PROCEEDS-1,
              :QBYB-PROCEEDS-2,
              :QBYB-LOCKBOX-1,
              :QBYB-LOCKBOX-2,
              :QBYB-UNEARNED-AMT-1-1-1,
              :QBYB-UNEARNED-AMT-1-1-2,
              :QBYB-UNEARNED-AMT-1-1-3,
              :QBYB-UNEARNED-AMT-1-1-4,
              :QBYB-UNEARNED-AMT-1-1-5,
              :QBYB-UNEARNED-AMT-1-1-6,
              :QBYB-UNEARNED-AMT-1-1-7,
              :QBYB-UNEARNED-AMT-1-1-8,
              :QBYB-UNEARNED-AMT-1-1-9,
              :QBYB-UNEARNED-AMT-1-1-10,
              :QBYB-UNEARNED-AMT-1-2-1,
              :QBYB-UNEARNED-AMT-1-2-2,
              :QBYB-UNEARNED-AMT-1-2-3,
              :QBYB-UNEARNED-AMT-1-2-4,
              :QBYB-UNEARNED-AMT-1-2-5,
              :QBYB-UNEARNED-AMT-1-2-6,
              :QBYB-UNEARNED-AMT-1-2-7,
              :QBYB-UNEARNED-AMT-1-2-8,
              :QBYB-UNEARNED-AMT-1-2-9,
              :QBYB-UNEARNED-AMT-1-2-10,
              :QBYB-UNEARNED-AMT-2-1-1,
              :QBYB-UNEARNED-AMT-2-1-2,
              :QBYB-UNEARNED-AMT-2-1-3,
              :QBYB-UNEARNED-AMT-2-1-4,
              :QBYB-UNEARNED-AMT-2-1-5,
              :QBYB-UNEARNED-AMT-2-1-6,
              :QBYB-UNEARNED-AMT-2-1-7,
              :QBYB-UNEARNED-AMT-2-1-8,
              :QBYB-UNEARNED-AMT-2-1-9,
              :QBYB-UNEARNED-AMT-2-1-10,
              :QBYB-UNEARNED-AMT-2-2-1,
              :QBYB-UNEARNED-AMT-2-2-2,
              :QBYB-UNEARNED-AMT-2-2-3,
              :QBYB-UNEARNED-AMT-2-2-4,
              :QBYB-UNEARNED-AMT-2-2-5,
              :QBYB-UNEARNED-AMT-2-2-6,
              :QBYB-UNEARNED-AMT-2-2-7,
              :QBYB-UNEARNED-AMT-2-2-8,
              :QBYB-UNEARNED-AMT-2-2-9,
              :QBYB-UNEARNED-AMT-2-2-10,
              :QBYB-UNEARNED-AMT-3-1-1,
              :QBYB-UNEARNED-AMT-3-1-2,
              :QBYB-UNEARNED-AMT-3-1-3,
              :QBYB-UNEARNED-AMT-3-1-4,
              :QBYB-UNEARNED-AMT-3-1-5,
              :QBYB-UNEARNED-AMT-3-1-6,
              :QBYB-UNEARNED-AMT-3-1-7,
              :QBYB-UNEARNED-AMT-3-1-8,
              :QBYB-UNEARNED-AMT-3-1-9,
              :QBYB-UNEARNED-AMT-3-1-10,
              :QBYB-UNEARNED-AMT-3-2-1,
              :QBYB-UNEARNED-AMT-3-2-2,
              :QBYB-UNEARNED-AMT-3-2-3,
              :QBYB-UNEARNED-AMT-3-2-4,
              :QBYB-UNEARNED-AMT-3-2-5,
              :QBYB-UNEARNED-AMT-3-2-6,
              :QBYB-UNEARNED-AMT-3-2-7,
              :QBYB-UNEARNED-AMT-3-2-8,
              :QBYB-UNEARNED-AMT-3-2-9,
              :QBYB-UNEARNED-AMT-3-2-10)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-BYB1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

           MOVE  '1' TO SQL-UPDATE-CONN-USE-SW. *> USE C1 CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

      *-----------------------------------------------------------------
       REWRITE-BYB1-FILE. *> EMBEDDED - BYBFILE

           PERFORM REWRITE-IT.
           MOVE BYB-PATH-SQL TO E-FILE.
           MOVE BYB1-KEY TO E-KEYX.
           PERFORM SET-BYB-FIELDS.

           MOVE BY-DATE-CCYYMM      TO QBYB-DATE-CCYYMM.
           MOVE BY-BRNO             TO QBYB-BRNO.
           MOVE BY-CLASS            TO QBYB-CLASS.

           MOVE '2' TO SQL-UPDATE-CONN-USE-SW. *> USE UPDATE_CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

           EXEC SQL
            UPDATE DBO.BYBFILE
             SET
              BYBFILE.BYB_DATE_CCYYMM = :QBYB-DATE-CCYYMM,
              BYBFILE.BYB_BRNO = :QBYB-BRNO,
              BYBFILE.BYB_CLASS = :QBYB-CLASS,
              BYBFILE.BYB_PL_PRINCOL_1_1 = :QBYB-PL-PRINCOL-1-1,
              BYBFILE.BYB_PL_INTCOL_1_1 = :QBYB-PL-INTCOL-1-1,
              BYBFILE.BYB_PL_LATECOL_1_1 = :QBYB-PL-LATECOL-1-1,
              BYBFILE.BYB_PL_O2COL_1_1 = :QBYB-PL-O2COL-1-1,
              BYBFILE.BYB_PL_PRINCOL_1_2 = :QBYB-PL-PRINCOL-1-2,
              BYBFILE.BYB_PL_INTCOL_1_2 = :QBYB-PL-INTCOL-1-2,
              BYBFILE.BYB_PL_LATECOL_1_2 = :QBYB-PL-LATECOL-1-2,
              BYBFILE.BYB_PL_O2COL_1_2 = :QBYB-PL-O2COL-1-2,
              BYBFILE.BYB_PL_PRINCOL_1_3 = :QBYB-PL-PRINCOL-1-3,
              BYBFILE.BYB_PL_INTCOL_1_3 = :QBYB-PL-INTCOL-1-3,
              BYBFILE.BYB_PL_LATECOL_1_3 = :QBYB-PL-LATECOL-1-3,
              BYBFILE.BYB_PL_O2COL_1_3 = :QBYB-PL-O2COL-1-3,
              BYBFILE.BYB_PL_PRINCOL_1_4 = :QBYB-PL-PRINCOL-1-4,
              BYBFILE.BYB_PL_INTCOL_1_4 = :QBYB-PL-INTCOL-1-4,
              BYBFILE.BYB_PL_LATECOL_1_4 = :QBYB-PL-LATECOL-1-4,
              BYBFILE.BYB_PL_O2COL_1_4 = :QBYB-PL-O2COL-1-4,
              BYBFILE.BYB_PL_PRINCOL_1_5 = :QBYB-PL-PRINCOL-1-5,
              BYBFILE.BYB_PL_INTCOL_1_5 = :QBYB-PL-INTCOL-1-5,
              BYBFILE.BYB_PL_LATECOL_1_5 = :QBYB-PL-LATECOL-1-5,
              BYBFILE.BYB_PL_O2COL_1_5 = :QBYB-PL-O2COL-1-5,
              BYBFILE.BYB_AMT_SOLD_1 = :QBYB-AMT-SOLD-1,
              BYBFILE.BYB_AMT_TRANS_1 = :QBYB-AMT-TRANS-1,
              BYBFILE.BYB_AMT_PURCH_1 = :QBYB-AMT-PURCH-1,
              BYBFILE.BYB_NO_SOLD_1 = :QBYB-NO-SOLD-1,
              BYBFILE.BYB_NO_TRANS_1 = :QBYB-NO-TRANS-1,
              BYBFILE.BYB_NO_PURCH_1 = :QBYB-NO-PURCH-1,
              BYBFILE.BYB_OTHCOL_1 = :QBYB-OTHCOL-1,
              BYBFILE.BYB_OT2COL_1 = :QBYB-OT2COL-1,
              BYBFILE.BYB_INTCOL_1 = :QBYB-INTCOL-1,
              BYBFILE.BYB_LATECOL_1 = :QBYB-LATECOL-1,
              BYBFILE.BYB_PRINCOL_1 = :QBYB-PRINCOL-1,
              BYBFILE.BYB_DEFCOL_1 = :QBYB-DEFCOL-1,
              BYBFILE.BYB_RNPRIN_1 = :QBYB-RNPRIN-1,
              BYBFILE.BYB_INTREB_1 = :QBYB-INTREB-1,
              BYBFILE.BYB_CLREB1_1 = :QBYB-CLREB1-1,
              BYBFILE.BYB_CLREB2_1 = :QBYB-CLREB2-1,
              BYBFILE.BYB_CLREB3_1 = :QBYB-CLREB3-1,
              BYBFILE.BYB_CLREB4_1 = :QBYB-CLREB4-1,
              BYBFILE.BYB_AHREB_1 = :QBYB-AHREB-1,
              BYBFILE.BYB_PPREB_1 = :QBYB-PPREB-1,
              BYBFILE.BYB_O1REB_1 = :QBYB-O1REB-1,
              BYBFILE.BYB_O2REB_1 = :QBYB-O2REB-1,
              BYBFILE.BYB_O3REB_1 = :QBYB-O3REB-1,
              BYBFILE.BYB_O4REB_1 = :QBYB-O4REB-1,
              BYBFILE.BYB_O5REB_1 = :QBYB-O5REB-1,
              BYBFILE.BYB_O6REB_1 = :QBYB-O6REB-1,
              BYBFILE.BYB_O7REB_1 = :QBYB-O7REB-1,
              BYBFILE.BYB_SCREB_1 = :QBYB-SCREB-1,
              BYBFILE.BYB_MFREB_1 = :QBYB-MFREB-1,
              BYBFILE.BYB_DFREB_1 = :QBYB-DFREB-1,
              BYBFILE.BYB_UN7_REB_1 = :QBYB-UN7-REB-1,
              BYBFILE.BYB_UN8_REB_1 = :QBYB-UN8-REB-1,
              BYBFILE.BYB_UN9_REB_1 = :QBYB-UN9-REB-1,
              BYBFILE.BYB_UN10_REB_1 = :QBYB-UN10-REB-1,
              BYBFILE.BYB_PART_CHGOFF_1 = :QBYB-PART-CHGOFF-1,
              BYBFILE.BYB_ALLOTMENTS_1 = :QBYB-ALLOTMENTS-1,
              BYBFILE.BYB_INS_NO_ELIG_1_1 = :QBYB-INS-NO-ELIG-1-1,
              BYBFILE.BYB_INS_NO_WROTE_1_1 =
                   :QBYB-INS-NO-WROTE-1-1,
              BYBFILE.BYB_INS_NO_NOTWROTE_1_1 =
                   :QBYB-INS-NO-NOTWROTE-1-1,
              BYBFILE.BYB_INS_AMT_WROTE_1_1 =
                   :QBYB-INS-AMT-WROTE-1-1,
              BYBFILE.BYB_INS_NO_ELIG_1_2 = :QBYB-INS-NO-ELIG-1-2,
              BYBFILE.BYB_INS_NO_WROTE_1_2 =
                   :QBYB-INS-NO-WROTE-1-2,
              BYBFILE.BYB_INS_NO_NOTWROTE_1_2 =
                   :QBYB-INS-NO-NOTWROTE-1-2,
              BYBFILE.BYB_INS_AMT_WROTE_1_2 =
                   :QBYB-INS-AMT-WROTE-1-2,
              BYBFILE.BYB_INS_NO_ELIG_1_3 = :QBYB-INS-NO-ELIG-1-3,
              BYBFILE.BYB_INS_NO_WROTE_1_3 =
                   :QBYB-INS-NO-WROTE-1-3,
              BYBFILE.BYB_INS_NO_NOTWROTE_1_3 =
                   :QBYB-INS-NO-NOTWROTE-1-3,
              BYBFILE.BYB_INS_AMT_WROTE_1_3 =
                   :QBYB-INS-AMT-WROTE-1-3,
              BYBFILE.BYB_INS_NO_ELIG_1_4 = :QBYB-INS-NO-ELIG-1-4,
              BYBFILE.BYB_INS_NO_WROTE_1_4 =
                   :QBYB-INS-NO-WROTE-1-4,
              BYBFILE.BYB_INS_NO_NOTWROTE_1_4 =
                   :QBYB-INS-NO-NOTWROTE-1-4,
              BYBFILE.BYB_INS_AMT_WROTE_1_4 =
                   :QBYB-INS-AMT-WROTE-1-4,
              BYBFILE.BYB_INS_NO_ELIG_1_5 = :QBYB-INS-NO-ELIG-1-5,
              BYBFILE.BYB_INS_NO_WROTE_1_5 =
                   :QBYB-INS-NO-WROTE-1-5,
              BYBFILE.BYB_INS_NO_NOTWROTE_1_5 =
                   :QBYB-INS-NO-NOTWROTE-1-5,
              BYBFILE.BYB_INS_AMT_WROTE_1_5 =
                   :QBYB-INS-AMT-WROTE-1-5,
              BYBFILE.BYB_INS_NO_ELIG_1_6 = :QBYB-INS-NO-ELIG-1-6,
              BYBFILE.BYB_INS_NO_WROTE_1_6 =
                   :QBYB-INS-NO-WROTE-1-6,
              BYBFILE.BYB_INS_NO_NOTWROTE_1_6 =
                   :QBYB-INS-NO-NOTWROTE-1-6,
              BYBFILE.BYB_INS_AMT_WROTE_1_6 =
                   :QBYB-INS-AMT-WROTE-1-6,
              BYBFILE.BYB_INS_NO_ELIG_1_7 = :QBYB-INS-NO-ELIG-1-7,
              BYBFILE.BYB_INS_NO_WROTE_1_7 =
                   :QBYB-INS-NO-WROTE-1-7,
              BYBFILE.BYB_INS_NO_NOTWROTE_1_7 =
                   :QBYB-INS-NO-NOTWROTE-1-7,
              BYBFILE.BYB_INS_AMT_WROTE_1_7 =
                   :QBYB-INS-AMT-WROTE-1-7,
              BYBFILE.BYB_INS_NO_ELIG_1_8 = :QBYB-INS-NO-ELIG-1-8,
              BYBFILE.BYB_INS_NO_WROTE_1_8 =
                   :QBYB-INS-NO-WROTE-1-8,
              BYBFILE.BYB_INS_NO_NOTWROTE_1_8 =
                   :QBYB-INS-NO-NOTWROTE-1-8,
              BYBFILE.BYB_INS_AMT_WROTE_1_8 =
                   :QBYB-INS-AMT-WROTE-1-8,
              BYBFILE.BYB_INS_NO_ELIG_1_9 = :QBYB-INS-NO-ELIG-1-9,
              BYBFILE.BYB_INS_NO_WROTE_1_9 =
                   :QBYB-INS-NO-WROTE-1-9,
              BYBFILE.BYB_INS_NO_NOTWROTE_1_9 =
                   :QBYB-INS-NO-NOTWROTE-1-9,
              BYBFILE.BYB_INS_AMT_WROTE_1_9 =
                   :QBYB-INS-AMT-WROTE-1-9,
              BYBFILE.BYB_INS_NO_ELIG_1_10 =
                   :QBYB-INS-NO-ELIG-1-10,
              BYBFILE.BYB_INS_NO_WROTE_1_10 =
                   :QBYB-INS-NO-WROTE-1-10,
              BYBFILE.BYB_INS_NO_NOTWROTE_1_10 =
                   :QBYB-INS-NO-NOTWROTE-1-10,
              BYBFILE.BYB_INS_AMT_WROTE_1_10 =
                   :QBYB-INS-AMT-WROTE-1-10,
              BYBFILE.BYB_FEE_NO_ELIG_1_1 = :QBYB-FEE-NO-ELIG-1-1,
              BYBFILE.BYB_FEE_NO_WROTE_1_1 =
                   :QBYB-FEE-NO-WROTE-1-1,
              BYBFILE.BYB_FEE_NO_NOTWROTE_1_1 =
                   :QBYB-FEE-NO-NOTWROTE-1-1,
              BYBFILE.BYB_FEE_AMT_WROTE_1_1 =
                   :QBYB-FEE-AMT-WROTE-1-1,
              BYBFILE.BYB_FEE_NO_ELIG_1_2 = :QBYB-FEE-NO-ELIG-1-2,
              BYBFILE.BYB_FEE_NO_WROTE_1_2 =
                   :QBYB-FEE-NO-WROTE-1-2,
              BYBFILE.BYB_FEE_NO_NOTWROTE_1_2 =
                   :QBYB-FEE-NO-NOTWROTE-1-2,
              BYBFILE.BYB_FEE_AMT_WROTE_1_2 =
                   :QBYB-FEE-AMT-WROTE-1-2,
              BYBFILE.BYB_FEE_NO_ELIG_1_3 = :QBYB-FEE-NO-ELIG-1-3,
              BYBFILE.BYB_FEE_NO_WROTE_1_3 =
                   :QBYB-FEE-NO-WROTE-1-3,
              BYBFILE.BYB_FEE_NO_NOTWROTE_1_3 =
                   :QBYB-FEE-NO-NOTWROTE-1-3,
              BYBFILE.BYB_FEE_AMT_WROTE_1_3 =
                   :QBYB-FEE-AMT-WROTE-1-3,
              BYBFILE.BYB_OTH_NO_COL_1 = :QBYB-OTH-NO-COL-1,
              BYBFILE.BYB_OT2_NO_COL_1 = :QBYB-OT2-NO-COL-1,
              BYBFILE.BYB_INT_NO_COL_1 = :QBYB-INT-NO-COL-1,
              BYBFILE.BYB_LATE_NO_COL_1 = :QBYB-LATE-NO-COL-1,
              BYBFILE.BYB_OTH_NO_WAIVED_1 = :QBYB-OTH-NO-WAIVED-1,
              BYBFILE.BYB_OT2_NO_WAIVED_1 = :QBYB-OT2-NO-WAIVED-1,
              BYBFILE.BYB_INT_NO_WAIVED_1 = :QBYB-INT-NO-WAIVED-1,
              BYBFILE.BYB_LATE_NO_WAIVED_1 =
                   :QBYB-LATE-NO-WAIVED-1,
              BYBFILE.BYB_OTH_AMT_WAIVED_1 =
                   :QBYB-OTH-AMT-WAIVED-1,
              BYBFILE.BYB_OT2_AMT_WAIVED_1 =
                   :QBYB-OT2-AMT-WAIVED-1,
              BYBFILE.BYB_INT_AMT_WAIVED_1 =
                   :QBYB-INT-AMT-WAIVED-1,
              BYBFILE.BYB_LATE_AMT_WAIVED_1 =
                   :QBYB-LATE-AMT-WAIVED-1,
              BYBFILE.BYB_PL_PRINCOL_2_1 = :QBYB-PL-PRINCOL-2-1,
              BYBFILE.BYB_PL_INTCOL_2_1 = :QBYB-PL-INTCOL-2-1,
              BYBFILE.BYB_PL_LATECOL_2_1 = :QBYB-PL-LATECOL-2-1,
              BYBFILE.BYB_PL_O2COL_2_1 = :QBYB-PL-O2COL-2-1,
              BYBFILE.BYB_PL_PRINCOL_2_2 = :QBYB-PL-PRINCOL-2-2,
              BYBFILE.BYB_PL_INTCOL_2_2 = :QBYB-PL-INTCOL-2-2,
              BYBFILE.BYB_PL_LATECOL_2_2 = :QBYB-PL-LATECOL-2-2,
              BYBFILE.BYB_PL_O2COL_2_2 = :QBYB-PL-O2COL-2-2,
              BYBFILE.BYB_PL_PRINCOL_2_3 = :QBYB-PL-PRINCOL-2-3,
              BYBFILE.BYB_PL_INTCOL_2_3 = :QBYB-PL-INTCOL-2-3,
              BYBFILE.BYB_PL_LATECOL_2_3 = :QBYB-PL-LATECOL-2-3,
              BYBFILE.BYB_PL_O2COL_2_3 = :QBYB-PL-O2COL-2-3,
              BYBFILE.BYB_PL_PRINCOL_2_4 = :QBYB-PL-PRINCOL-2-4,
              BYBFILE.BYB_PL_INTCOL_2_4 = :QBYB-PL-INTCOL-2-4,
              BYBFILE.BYB_PL_LATECOL_2_4 = :QBYB-PL-LATECOL-2-4,
              BYBFILE.BYB_PL_O2COL_2_4 = :QBYB-PL-O2COL-2-4,
              BYBFILE.BYB_PL_PRINCOL_2_5 = :QBYB-PL-PRINCOL-2-5,
              BYBFILE.BYB_PL_INTCOL_2_5 = :QBYB-PL-INTCOL-2-5,
              BYBFILE.BYB_PL_LATECOL_2_5 = :QBYB-PL-LATECOL-2-5,
              BYBFILE.BYB_PL_O2COL_2_5 = :QBYB-PL-O2COL-2-5,
              BYBFILE.BYB_AMT_SOLD_2 = :QBYB-AMT-SOLD-2,
              BYBFILE.BYB_AMT_TRANS_2 = :QBYB-AMT-TRANS-2,
              BYBFILE.BYB_AMT_PURCH_2 = :QBYB-AMT-PURCH-2,
              BYBFILE.BYB_NO_SOLD_2 = :QBYB-NO-SOLD-2,
              BYBFILE.BYB_NO_TRANS_2 = :QBYB-NO-TRANS-2,
              BYBFILE.BYB_NO_PURCH_2 = :QBYB-NO-PURCH-2,
              BYBFILE.BYB_OTHCOL_2 = :QBYB-OTHCOL-2,
              BYBFILE.BYB_OT2COL_2 = :QBYB-OT2COL-2,
              BYBFILE.BYB_INTCOL_2 = :QBYB-INTCOL-2,
              BYBFILE.BYB_LATECOL_2 = :QBYB-LATECOL-2,
              BYBFILE.BYB_PRINCOL_2 = :QBYB-PRINCOL-2,
              BYBFILE.BYB_DEFCOL_2 = :QBYB-DEFCOL-2,
              BYBFILE.BYB_RNPRIN_2 = :QBYB-RNPRIN-2,
              BYBFILE.BYB_INTREB_2 = :QBYB-INTREB-2,
              BYBFILE.BYB_CLREB1_2 = :QBYB-CLREB1-2,
              BYBFILE.BYB_CLREB2_2 = :QBYB-CLREB2-2,
              BYBFILE.BYB_CLREB3_2 = :QBYB-CLREB3-2,
              BYBFILE.BYB_CLREB4_2 = :QBYB-CLREB4-2,
              BYBFILE.BYB_AHREB_2 = :QBYB-AHREB-2,
              BYBFILE.BYB_PPREB_2 = :QBYB-PPREB-2,
              BYBFILE.BYB_O1REB_2 = :QBYB-O1REB-2,
              BYBFILE.BYB_O2REB_2 = :QBYB-O2REB-2,
              BYBFILE.BYB_O3REB_2 = :QBYB-O3REB-2,
              BYBFILE.BYB_O4REB_2 = :QBYB-O4REB-2,
              BYBFILE.BYB_O5REB_2 = :QBYB-O5REB-2,
              BYBFILE.BYB_O6REB_2 = :QBYB-O6REB-2,
              BYBFILE.BYB_O7REB_2 = :QBYB-O7REB-2,
              BYBFILE.BYB_SCREB_2 = :QBYB-SCREB-2,
              BYBFILE.BYB_MFREB_2 = :QBYB-MFREB-2,
              BYBFILE.BYB_DFREB_2 = :QBYB-DFREB-2,
              BYBFILE.BYB_UN7_REB_2 = :QBYB-UN7-REB-2,
              BYBFILE.BYB_UN8_REB_2 = :QBYB-UN8-REB-2,
              BYBFILE.BYB_UN9_REB_2 = :QBYB-UN9-REB-2,
              BYBFILE.BYB_UN10_REB_2 = :QBYB-UN10-REB-2,
              BYBFILE.BYB_PART_CHGOFF_2 = :QBYB-PART-CHGOFF-2,
              BYBFILE.BYB_ALLOTMENTS_2 = :QBYB-ALLOTMENTS-2,
              BYBFILE.BYB_INS_NO_ELIG_2_1 = :QBYB-INS-NO-ELIG-2-1,
              BYBFILE.BYB_INS_NO_WROTE_2_1 =
                   :QBYB-INS-NO-WROTE-2-1,
              BYBFILE.BYB_INS_NO_NOTWROTE_2_1 =
                   :QBYB-INS-NO-NOTWROTE-2-1,
              BYBFILE.BYB_INS_AMT_WROTE_2_1 =
                   :QBYB-INS-AMT-WROTE-2-1,
              BYBFILE.BYB_INS_NO_ELIG_2_2 = :QBYB-INS-NO-ELIG-2-2,
              BYBFILE.BYB_INS_NO_WROTE_2_2 =
                   :QBYB-INS-NO-WROTE-2-2,
              BYBFILE.BYB_INS_NO_NOTWROTE_2_2 =
                   :QBYB-INS-NO-NOTWROTE-2-2,
              BYBFILE.BYB_INS_AMT_WROTE_2_2 =
                   :QBYB-INS-AMT-WROTE-2-2,
              BYBFILE.BYB_INS_NO_ELIG_2_3 = :QBYB-INS-NO-ELIG-2-3,
              BYBFILE.BYB_INS_NO_WROTE_2_3 =
                   :QBYB-INS-NO-WROTE-2-3,
              BYBFILE.BYB_INS_NO_NOTWROTE_2_3 =
                   :QBYB-INS-NO-NOTWROTE-2-3,
              BYBFILE.BYB_INS_AMT_WROTE_2_3 =
                   :QBYB-INS-AMT-WROTE-2-3,
              BYBFILE.BYB_INS_NO_ELIG_2_4 = :QBYB-INS-NO-ELIG-2-4,
              BYBFILE.BYB_INS_NO_WROTE_2_4 =
                   :QBYB-INS-NO-WROTE-2-4,
              BYBFILE.BYB_INS_NO_NOTWROTE_2_4 =
                   :QBYB-INS-NO-NOTWROTE-2-4,
              BYBFILE.BYB_INS_AMT_WROTE_2_4 =
                   :QBYB-INS-AMT-WROTE-2-4,
              BYBFILE.BYB_INS_NO_ELIG_2_5 = :QBYB-INS-NO-ELIG-2-5,
              BYBFILE.BYB_INS_NO_WROTE_2_5 =
                   :QBYB-INS-NO-WROTE-2-5,
              BYBFILE.BYB_INS_NO_NOTWROTE_2_5 =
                   :QBYB-INS-NO-NOTWROTE-2-5,
              BYBFILE.BYB_INS_AMT_WROTE_2_5 =
                   :QBYB-INS-AMT-WROTE-2-5,
              BYBFILE.BYB_INS_NO_ELIG_2_6 = :QBYB-INS-NO-ELIG-2-6,
              BYBFILE.BYB_INS_NO_WROTE_2_6 =
                   :QBYB-INS-NO-WROTE-2-6,
              BYBFILE.BYB_INS_NO_NOTWROTE_2_6 =
                   :QBYB-INS-NO-NOTWROTE-2-6,
              BYBFILE.BYB_INS_AMT_WROTE_2_6 =
                   :QBYB-INS-AMT-WROTE-2-6,
              BYBFILE.BYB_INS_NO_ELIG_2_7 = :QBYB-INS-NO-ELIG-2-7,
              BYBFILE.BYB_INS_NO_WROTE_2_7 =
                   :QBYB-INS-NO-WROTE-2-7,
              BYBFILE.BYB_INS_NO_NOTWROTE_2_7 =
                   :QBYB-INS-NO-NOTWROTE-2-7,
              BYBFILE.BYB_INS_AMT_WROTE_2_7 =
                   :QBYB-INS-AMT-WROTE-2-7,
              BYBFILE.BYB_INS_NO_ELIG_2_8 = :QBYB-INS-NO-ELIG-2-8,
              BYBFILE.BYB_INS_NO_WROTE_2_8 =
                   :QBYB-INS-NO-WROTE-2-8,
              BYBFILE.BYB_INS_NO_NOTWROTE_2_8 =
                   :QBYB-INS-NO-NOTWROTE-2-8,
              BYBFILE.BYB_INS_AMT_WROTE_2_8 =
                   :QBYB-INS-AMT-WROTE-2-8,
              BYBFILE.BYB_INS_NO_ELIG_2_9 = :QBYB-INS-NO-ELIG-2-9,
              BYBFILE.BYB_INS_NO_WROTE_2_9 =
                   :QBYB-INS-NO-WROTE-2-9,
              BYBFILE.BYB_INS_NO_NOTWROTE_2_9 =
                   :QBYB-INS-NO-NOTWROTE-2-9,
              BYBFILE.BYB_INS_AMT_WROTE_2_9 =
                   :QBYB-INS-AMT-WROTE-2-9,
              BYBFILE.BYB_INS_NO_ELIG_2_10 =
                   :QBYB-INS-NO-ELIG-2-10,
              BYBFILE.BYB_INS_NO_WROTE_2_10 =
                   :QBYB-INS-NO-WROTE-2-10,
              BYBFILE.BYB_INS_NO_NOTWROTE_2_10 =
                   :QBYB-INS-NO-NOTWROTE-2-10,
              BYBFILE.BYB_INS_AMT_WROTE_2_10 =
                   :QBYB-INS-AMT-WROTE-2-10,
              BYBFILE.BYB_FEE_NO_ELIG_2_1 = :QBYB-FEE-NO-ELIG-2-1,
              BYBFILE.BYB_FEE_NO_WROTE_2_1 =
                   :QBYB-FEE-NO-WROTE-2-1,
              BYBFILE.BYB_FEE_NO_NOTWROTE_2_1 =
                   :QBYB-FEE-NO-NOTWROTE-2-1,
              BYBFILE.BYB_FEE_AMT_WROTE_2_1 =
                   :QBYB-FEE-AMT-WROTE-2-1,
              BYBFILE.BYB_FEE_NO_ELIG_2_2 = :QBYB-FEE-NO-ELIG-2-2,
              BYBFILE.BYB_FEE_NO_WROTE_2_2 =
                   :QBYB-FEE-NO-WROTE-2-2,
              BYBFILE.BYB_FEE_NO_NOTWROTE_2_2 =
                   :QBYB-FEE-NO-NOTWROTE-2-2,
              BYBFILE.BYB_FEE_AMT_WROTE_2_2 =
                   :QBYB-FEE-AMT-WROTE-2-2,
              BYBFILE.BYB_FEE_NO_ELIG_2_3 = :QBYB-FEE-NO-ELIG-2-3,
              BYBFILE.BYB_FEE_NO_WROTE_2_3 =
                   :QBYB-FEE-NO-WROTE-2-3,
              BYBFILE.BYB_FEE_NO_NOTWROTE_2_3 =
                   :QBYB-FEE-NO-NOTWROTE-2-3,
              BYBFILE.BYB_FEE_AMT_WROTE_2_3 =
                   :QBYB-FEE-AMT-WROTE-2-3,
              BYBFILE.BYB_OTH_NO_COL_2 = :QBYB-OTH-NO-COL-2,
              BYBFILE.BYB_OT2_NO_COL_2 = :QBYB-OT2-NO-COL-2,
              BYBFILE.BYB_INT_NO_COL_2 = :QBYB-INT-NO-COL-2,
              BYBFILE.BYB_LATE_NO_COL_2 = :QBYB-LATE-NO-COL-2,
              BYBFILE.BYB_OTH_NO_WAIVED_2 = :QBYB-OTH-NO-WAIVED-2,
              BYBFILE.BYB_OT2_NO_WAIVED_2 = :QBYB-OT2-NO-WAIVED-2,
              BYBFILE.BYB_INT_NO_WAIVED_2 = :QBYB-INT-NO-WAIVED-2,
              BYBFILE.BYB_LATE_NO_WAIVED_2 =
                   :QBYB-LATE-NO-WAIVED-2,
              BYBFILE.BYB_OTH_AMT_WAIVED_2 =
                   :QBYB-OTH-AMT-WAIVED-2,
              BYBFILE.BYB_OT2_AMT_WAIVED_2 =
                   :QBYB-OT2-AMT-WAIVED-2,
              BYBFILE.BYB_INT_AMT_WAIVED_2 =
                   :QBYB-INT-AMT-WAIVED-2,
              BYBFILE.BYB_LATE_AMT_WAIVED_2 =
                   :QBYB-LATE-AMT-WAIVED-2,
              BYBFILE.BYB_UNITS_1_1_1 = :QBYB-UNITS-1-1-1,
              BYBFILE.BYB_OUTBAL_1_1_1 = :QBYB-OUTBAL-1-1-1,
              BYBFILE.BYB_UNITS_1_1_2 = :QBYB-UNITS-1-1-2,
              BYBFILE.BYB_OUTBAL_1_1_2 = :QBYB-OUTBAL-1-1-2,
              BYBFILE.BYB_UNITS_1_1_3 = :QBYB-UNITS-1-1-3,
              BYBFILE.BYB_OUTBAL_1_1_3 = :QBYB-OUTBAL-1-1-3,
              BYBFILE.BYB_UNITS_1_1_4 = :QBYB-UNITS-1-1-4,
              BYBFILE.BYB_OUTBAL_1_1_4 = :QBYB-OUTBAL-1-1-4,
              BYBFILE.BYB_UNITS_1_1_5 = :QBYB-UNITS-1-1-5,
              BYBFILE.BYB_OUTBAL_1_1_5 = :QBYB-OUTBAL-1-1-5,
              BYBFILE.BYB_UNITS_1_1_6 = :QBYB-UNITS-1-1-6,
              BYBFILE.BYB_OUTBAL_1_1_6 = :QBYB-OUTBAL-1-1-6,
              BYBFILE.BYB_UNITS_1_1_7 = :QBYB-UNITS-1-1-7,
              BYBFILE.BYB_OUTBAL_1_1_7 = :QBYB-OUTBAL-1-1-7,
              BYBFILE.BYB_UNITS_1_1_8 = :QBYB-UNITS-1-1-8,
              BYBFILE.BYB_OUTBAL_1_1_8 = :QBYB-OUTBAL-1-1-8,
              BYBFILE.BYB_UNITS_1_1_9 = :QBYB-UNITS-1-1-9,
              BYBFILE.BYB_OUTBAL_1_1_9 = :QBYB-OUTBAL-1-1-9,
              BYBFILE.BYB_UNITS_1_1_10 = :QBYB-UNITS-1-1-10,
              BYBFILE.BYB_OUTBAL_1_1_10 = :QBYB-OUTBAL-1-1-10,
              BYBFILE.BYB_UNITS_1_2_1 = :QBYB-UNITS-1-2-1,
              BYBFILE.BYB_OUTBAL_1_2_1 = :QBYB-OUTBAL-1-2-1,
              BYBFILE.BYB_UNITS_1_2_2 = :QBYB-UNITS-1-2-2,
              BYBFILE.BYB_OUTBAL_1_2_2 = :QBYB-OUTBAL-1-2-2,
              BYBFILE.BYB_UNITS_1_2_3 = :QBYB-UNITS-1-2-3,
              BYBFILE.BYB_OUTBAL_1_2_3 = :QBYB-OUTBAL-1-2-3,
              BYBFILE.BYB_UNITS_1_2_4 = :QBYB-UNITS-1-2-4,
              BYBFILE.BYB_OUTBAL_1_2_4 = :QBYB-OUTBAL-1-2-4,
              BYBFILE.BYB_UNITS_1_2_5 = :QBYB-UNITS-1-2-5,
              BYBFILE.BYB_OUTBAL_1_2_5 = :QBYB-OUTBAL-1-2-5,
              BYBFILE.BYB_UNITS_1_2_6 = :QBYB-UNITS-1-2-6,
              BYBFILE.BYB_OUTBAL_1_2_6 = :QBYB-OUTBAL-1-2-6,
              BYBFILE.BYB_UNITS_1_2_7 = :QBYB-UNITS-1-2-7,
              BYBFILE.BYB_OUTBAL_1_2_7 = :QBYB-OUTBAL-1-2-7,
              BYBFILE.BYB_UNITS_1_2_8 = :QBYB-UNITS-1-2-8,
              BYBFILE.BYB_OUTBAL_1_2_8 = :QBYB-OUTBAL-1-2-8,
              BYBFILE.BYB_UNITS_1_2_9 = :QBYB-UNITS-1-2-9,
              BYBFILE.BYB_OUTBAL_1_2_9 = :QBYB-OUTBAL-1-2-9,
              BYBFILE.BYB_UNITS_1_2_10 = :QBYB-UNITS-1-2-10,
              BYBFILE.BYB_OUTBAL_1_2_10 = :QBYB-OUTBAL-1-2-10,
              BYBFILE.BYB_UNITS_2_1_1 = :QBYB-UNITS-2-1-1,
              BYBFILE.BYB_OUTBAL_2_1_1 = :QBYB-OUTBAL-2-1-1,
              BYBFILE.BYB_UNITS_2_1_2 = :QBYB-UNITS-2-1-2,
              BYBFILE.BYB_OUTBAL_2_1_2 = :QBYB-OUTBAL-2-1-2,
              BYBFILE.BYB_UNITS_2_1_3 = :QBYB-UNITS-2-1-3,
              BYBFILE.BYB_OUTBAL_2_1_3 = :QBYB-OUTBAL-2-1-3,
              BYBFILE.BYB_UNITS_2_1_4 = :QBYB-UNITS-2-1-4,
              BYBFILE.BYB_OUTBAL_2_1_4 = :QBYB-OUTBAL-2-1-4,
              BYBFILE.BYB_UNITS_2_1_5 = :QBYB-UNITS-2-1-5,
              BYBFILE.BYB_OUTBAL_2_1_5 = :QBYB-OUTBAL-2-1-5,
              BYBFILE.BYB_UNITS_2_1_6 = :QBYB-UNITS-2-1-6,
              BYBFILE.BYB_OUTBAL_2_1_6 = :QBYB-OUTBAL-2-1-6,
              BYBFILE.BYB_UNITS_2_1_7 = :QBYB-UNITS-2-1-7,
              BYBFILE.BYB_OUTBAL_2_1_7 = :QBYB-OUTBAL-2-1-7,
              BYBFILE.BYB_UNITS_2_1_8 = :QBYB-UNITS-2-1-8,
              BYBFILE.BYB_OUTBAL_2_1_8 = :QBYB-OUTBAL-2-1-8,
              BYBFILE.BYB_UNITS_2_1_9 = :QBYB-UNITS-2-1-9,
              BYBFILE.BYB_OUTBAL_2_1_9 = :QBYB-OUTBAL-2-1-9,
              BYBFILE.BYB_UNITS_2_1_10 = :QBYB-UNITS-2-1-10,
              BYBFILE.BYB_OUTBAL_2_1_10 = :QBYB-OUTBAL-2-1-10,
              BYBFILE.BYB_UNITS_2_2_1 = :QBYB-UNITS-2-2-1,
              BYBFILE.BYB_OUTBAL_2_2_1 = :QBYB-OUTBAL-2-2-1,
              BYBFILE.BYB_UNITS_2_2_2 = :QBYB-UNITS-2-2-2,
              BYBFILE.BYB_OUTBAL_2_2_2 = :QBYB-OUTBAL-2-2-2,
              BYBFILE.BYB_UNITS_2_2_3 = :QBYB-UNITS-2-2-3,
              BYBFILE.BYB_OUTBAL_2_2_3 = :QBYB-OUTBAL-2-2-3,
              BYBFILE.BYB_UNITS_2_2_4 = :QBYB-UNITS-2-2-4,
              BYBFILE.BYB_OUTBAL_2_2_4 = :QBYB-OUTBAL-2-2-4,
              BYBFILE.BYB_UNITS_2_2_5 = :QBYB-UNITS-2-2-5,
              BYBFILE.BYB_OUTBAL_2_2_5 = :QBYB-OUTBAL-2-2-5,
              BYBFILE.BYB_UNITS_2_2_6 = :QBYB-UNITS-2-2-6,
              BYBFILE.BYB_OUTBAL_2_2_6 = :QBYB-OUTBAL-2-2-6,
              BYBFILE.BYB_UNITS_2_2_7 = :QBYB-UNITS-2-2-7,
              BYBFILE.BYB_OUTBAL_2_2_7 = :QBYB-OUTBAL-2-2-7,
              BYBFILE.BYB_UNITS_2_2_8 = :QBYB-UNITS-2-2-8,
              BYBFILE.BYB_OUTBAL_2_2_8 = :QBYB-OUTBAL-2-2-8,
              BYBFILE.BYB_UNITS_2_2_9 = :QBYB-UNITS-2-2-9,
              BYBFILE.BYB_OUTBAL_2_2_9 = :QBYB-OUTBAL-2-2-9,
              BYBFILE.BYB_UNITS_2_2_10 = :QBYB-UNITS-2-2-10,
              BYBFILE.BYB_OUTBAL_2_2_10 = :QBYB-OUTBAL-2-2-10,
              BYBFILE.BYB_UNITS_3_1_1 = :QBYB-UNITS-3-1-1,
              BYBFILE.BYB_OUTBAL_3_1_1 = :QBYB-OUTBAL-3-1-1,
              BYBFILE.BYB_UNITS_3_1_2 = :QBYB-UNITS-3-1-2,
              BYBFILE.BYB_OUTBAL_3_1_2 = :QBYB-OUTBAL-3-1-2,
              BYBFILE.BYB_UNITS_3_1_3 = :QBYB-UNITS-3-1-3,
              BYBFILE.BYB_OUTBAL_3_1_3 = :QBYB-OUTBAL-3-1-3,
              BYBFILE.BYB_UNITS_3_1_4 = :QBYB-UNITS-3-1-4,
              BYBFILE.BYB_OUTBAL_3_1_4 = :QBYB-OUTBAL-3-1-4,
              BYBFILE.BYB_UNITS_3_1_5 = :QBYB-UNITS-3-1-5,
              BYBFILE.BYB_OUTBAL_3_1_5 = :QBYB-OUTBAL-3-1-5,
              BYBFILE.BYB_UNITS_3_1_6 = :QBYB-UNITS-3-1-6,
              BYBFILE.BYB_OUTBAL_3_1_6 = :QBYB-OUTBAL-3-1-6,
              BYBFILE.BYB_UNITS_3_1_7 = :QBYB-UNITS-3-1-7,
              BYBFILE.BYB_OUTBAL_3_1_7 = :QBYB-OUTBAL-3-1-7,
              BYBFILE.BYB_UNITS_3_1_8 = :QBYB-UNITS-3-1-8,
              BYBFILE.BYB_OUTBAL_3_1_8 = :QBYB-OUTBAL-3-1-8,
              BYBFILE.BYB_UNITS_3_1_9 = :QBYB-UNITS-3-1-9,
              BYBFILE.BYB_OUTBAL_3_1_9 = :QBYB-OUTBAL-3-1-9,
              BYBFILE.BYB_UNITS_3_1_10 = :QBYB-UNITS-3-1-10,
              BYBFILE.BYB_OUTBAL_3_1_10 = :QBYB-OUTBAL-3-1-10,
              BYBFILE.BYB_UNITS_3_2_1 = :QBYB-UNITS-3-2-1,
              BYBFILE.BYB_OUTBAL_3_2_1 = :QBYB-OUTBAL-3-2-1,
              BYBFILE.BYB_UNITS_3_2_2 = :QBYB-UNITS-3-2-2,
              BYBFILE.BYB_OUTBAL_3_2_2 = :QBYB-OUTBAL-3-2-2,
              BYBFILE.BYB_UNITS_3_2_3 = :QBYB-UNITS-3-2-3,
              BYBFILE.BYB_OUTBAL_3_2_3 = :QBYB-OUTBAL-3-2-3,
              BYBFILE.BYB_UNITS_3_2_4 = :QBYB-UNITS-3-2-4,
              BYBFILE.BYB_OUTBAL_3_2_4 = :QBYB-OUTBAL-3-2-4,
              BYBFILE.BYB_UNITS_3_2_5 = :QBYB-UNITS-3-2-5,
              BYBFILE.BYB_OUTBAL_3_2_5 = :QBYB-OUTBAL-3-2-5,
              BYBFILE.BYB_UNITS_3_2_6 = :QBYB-UNITS-3-2-6,
              BYBFILE.BYB_OUTBAL_3_2_6 = :QBYB-OUTBAL-3-2-6,
              BYBFILE.BYB_UNITS_3_2_7 = :QBYB-UNITS-3-2-7,
              BYBFILE.BYB_OUTBAL_3_2_7 = :QBYB-OUTBAL-3-2-7,
              BYBFILE.BYB_UNITS_3_2_8 = :QBYB-UNITS-3-2-8,
              BYBFILE.BYB_OUTBAL_3_2_8 = :QBYB-OUTBAL-3-2-8,
              BYBFILE.BYB_UNITS_3_2_9 = :QBYB-UNITS-3-2-9,
              BYBFILE.BYB_OUTBAL_3_2_9 = :QBYB-OUTBAL-3-2-9,
              BYBFILE.BYB_UNITS_3_2_10 = :QBYB-UNITS-3-2-10,
              BYBFILE.BYB_OUTBAL_3_2_10 = :QBYB-OUTBAL-3-2-10,
              BYBFILE.BYB_UNITS_BNKRPT_1_1_1 =
                   :QBYB-UNITS-BNKRPT-1-1-1,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_1_1 =
                   :QBYB-OUTBAL-BNKRPT-1-1-1,
              BYBFILE.BYB_UNITS_BNKRPT_1_1_2 =
                   :QBYB-UNITS-BNKRPT-1-1-2,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_1_2 =
                   :QBYB-OUTBAL-BNKRPT-1-1-2,
              BYBFILE.BYB_UNITS_BNKRPT_1_1_3 =
                   :QBYB-UNITS-BNKRPT-1-1-3,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_1_3 =
                   :QBYB-OUTBAL-BNKRPT-1-1-3,
              BYBFILE.BYB_UNITS_BNKRPT_1_1_4 =
                   :QBYB-UNITS-BNKRPT-1-1-4,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_1_4 =
                   :QBYB-OUTBAL-BNKRPT-1-1-4,
              BYBFILE.BYB_UNITS_BNKRPT_1_1_5 =
                   :QBYB-UNITS-BNKRPT-1-1-5,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_1_5 =
                   :QBYB-OUTBAL-BNKRPT-1-1-5,
              BYBFILE.BYB_UNITS_BNKRPT_1_1_6 =
                   :QBYB-UNITS-BNKRPT-1-1-6,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_1_6 =
                   :QBYB-OUTBAL-BNKRPT-1-1-6,
              BYBFILE.BYB_UNITS_BNKRPT_1_1_7 =
                   :QBYB-UNITS-BNKRPT-1-1-7,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_1_7 =
                   :QBYB-OUTBAL-BNKRPT-1-1-7,
              BYBFILE.BYB_UNITS_BNKRPT_1_1_8 =
                   :QBYB-UNITS-BNKRPT-1-1-8,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_1_8 =
                   :QBYB-OUTBAL-BNKRPT-1-1-8,
              BYBFILE.BYB_UNITS_BNKRPT_1_1_9 =
                   :QBYB-UNITS-BNKRPT-1-1-9,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_1_9 =
                   :QBYB-OUTBAL-BNKRPT-1-1-9,
              BYBFILE.BYB_UNITS_BNKRPT_1_1_10 =
                   :QBYB-UNITS-BNKRPT-1-1-10,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_1_10 =
                   :QBYB-OUTBAL-BNKRPT-1-1-10,
              BYBFILE.BYB_UNITS_BNKRPT_1_2_1 =
                   :QBYB-UNITS-BNKRPT-1-2-1,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_2_1 =
                   :QBYB-OUTBAL-BNKRPT-1-2-1,
              BYBFILE.BYB_UNITS_BNKRPT_1_2_2 =
                   :QBYB-UNITS-BNKRPT-1-2-2,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_2_2 =
                   :QBYB-OUTBAL-BNKRPT-1-2-2,
              BYBFILE.BYB_UNITS_BNKRPT_1_2_3 =
                   :QBYB-UNITS-BNKRPT-1-2-3,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_2_3 =
                   :QBYB-OUTBAL-BNKRPT-1-2-3,
              BYBFILE.BYB_UNITS_BNKRPT_1_2_4 =
                   :QBYB-UNITS-BNKRPT-1-2-4,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_2_4 =
                   :QBYB-OUTBAL-BNKRPT-1-2-4,
              BYBFILE.BYB_UNITS_BNKRPT_1_2_5 =
                   :QBYB-UNITS-BNKRPT-1-2-5,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_2_5 =
                   :QBYB-OUTBAL-BNKRPT-1-2-5,
              BYBFILE.BYB_UNITS_BNKRPT_1_2_6 =
                   :QBYB-UNITS-BNKRPT-1-2-6,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_2_6 =
                   :QBYB-OUTBAL-BNKRPT-1-2-6,
              BYBFILE.BYB_UNITS_BNKRPT_1_2_7 =
                   :QBYB-UNITS-BNKRPT-1-2-7,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_2_7 =
                   :QBYB-OUTBAL-BNKRPT-1-2-7,
              BYBFILE.BYB_UNITS_BNKRPT_1_2_8 =
                   :QBYB-UNITS-BNKRPT-1-2-8,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_2_8 =
                   :QBYB-OUTBAL-BNKRPT-1-2-8,
              BYBFILE.BYB_UNITS_BNKRPT_1_2_9 =
                   :QBYB-UNITS-BNKRPT-1-2-9,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_2_9 =
                   :QBYB-OUTBAL-BNKRPT-1-2-9,
              BYBFILE.BYB_UNITS_BNKRPT_1_2_10 =
                   :QBYB-UNITS-BNKRPT-1-2-10,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_2_10 =
                   :QBYB-OUTBAL-BNKRPT-1-2-10,
              BYBFILE.BYB_UNITS_BNKRPT_2_1_1 =
                   :QBYB-UNITS-BNKRPT-2-1-1,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_1_1 =
                   :QBYB-OUTBAL-BNKRPT-2-1-1,
              BYBFILE.BYB_UNITS_BNKRPT_2_1_2 =
                   :QBYB-UNITS-BNKRPT-2-1-2,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_1_2 =
                   :QBYB-OUTBAL-BNKRPT-2-1-2,
              BYBFILE.BYB_UNITS_BNKRPT_2_1_3 =
                   :QBYB-UNITS-BNKRPT-2-1-3,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_1_3 =
                   :QBYB-OUTBAL-BNKRPT-2-1-3,
              BYBFILE.BYB_UNITS_BNKRPT_2_1_4 =
                   :QBYB-UNITS-BNKRPT-2-1-4,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_1_4 =
                   :QBYB-OUTBAL-BNKRPT-2-1-4,
              BYBFILE.BYB_UNITS_BNKRPT_2_1_5 =
                   :QBYB-UNITS-BNKRPT-2-1-5,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_1_5 =
                   :QBYB-OUTBAL-BNKRPT-2-1-5,
              BYBFILE.BYB_UNITS_BNKRPT_2_1_6 =
                   :QBYB-UNITS-BNKRPT-2-1-6,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_1_6 =
                   :QBYB-OUTBAL-BNKRPT-2-1-6,
              BYBFILE.BYB_UNITS_BNKRPT_2_1_7 =
                   :QBYB-UNITS-BNKRPT-2-1-7,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_1_7 =
                   :QBYB-OUTBAL-BNKRPT-2-1-7,
              BYBFILE.BYB_UNITS_BNKRPT_2_1_8 =
                   :QBYB-UNITS-BNKRPT-2-1-8,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_1_8 =
                   :QBYB-OUTBAL-BNKRPT-2-1-8,
              BYBFILE.BYB_UNITS_BNKRPT_2_1_9 =
                   :QBYB-UNITS-BNKRPT-2-1-9,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_1_9 =
                   :QBYB-OUTBAL-BNKRPT-2-1-9,
              BYBFILE.BYB_UNITS_BNKRPT_2_1_10 =
                   :QBYB-UNITS-BNKRPT-2-1-10,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_1_10 =
                   :QBYB-OUTBAL-BNKRPT-2-1-10,
              BYBFILE.BYB_UNITS_BNKRPT_2_2_1 =
                   :QBYB-UNITS-BNKRPT-2-2-1,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_2_1 =
                   :QBYB-OUTBAL-BNKRPT-2-2-1,
              BYBFILE.BYB_UNITS_BNKRPT_2_2_2 =
                   :QBYB-UNITS-BNKRPT-2-2-2,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_2_2 =
                   :QBYB-OUTBAL-BNKRPT-2-2-2,
              BYBFILE.BYB_UNITS_BNKRPT_2_2_3 =
                   :QBYB-UNITS-BNKRPT-2-2-3,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_2_3 =
                   :QBYB-OUTBAL-BNKRPT-2-2-3,
              BYBFILE.BYB_UNITS_BNKRPT_2_2_4 =
                   :QBYB-UNITS-BNKRPT-2-2-4,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_2_4 =
                   :QBYB-OUTBAL-BNKRPT-2-2-4,
              BYBFILE.BYB_UNITS_BNKRPT_2_2_5 =
                   :QBYB-UNITS-BNKRPT-2-2-5,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_2_5 =
                   :QBYB-OUTBAL-BNKRPT-2-2-5,
              BYBFILE.BYB_UNITS_BNKRPT_2_2_6 =
                   :QBYB-UNITS-BNKRPT-2-2-6,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_2_6 =
                   :QBYB-OUTBAL-BNKRPT-2-2-6,
              BYBFILE.BYB_UNITS_BNKRPT_2_2_7 =
                   :QBYB-UNITS-BNKRPT-2-2-7,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_2_7 =
                   :QBYB-OUTBAL-BNKRPT-2-2-7,
              BYBFILE.BYB_UNITS_BNKRPT_2_2_8 =
                   :QBYB-UNITS-BNKRPT-2-2-8,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_2_8 =
                   :QBYB-OUTBAL-BNKRPT-2-2-8,
              BYBFILE.BYB_UNITS_BNKRPT_2_2_9 =
                   :QBYB-UNITS-BNKRPT-2-2-9,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_2_9 =
                   :QBYB-OUTBAL-BNKRPT-2-2-9,
              BYBFILE.BYB_UNITS_BNKRPT_2_2_10 =
                   :QBYB-UNITS-BNKRPT-2-2-10,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_2_10 =
                   :QBYB-OUTBAL-BNKRPT-2-2-10,
              BYBFILE.BYB_UNITS_BNKRPT_3_1_1 =
                   :QBYB-UNITS-BNKRPT-3-1-1,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_1_1 =
                   :QBYB-OUTBAL-BNKRPT-3-1-1,
              BYBFILE.BYB_UNITS_BNKRPT_3_1_2 =
                   :QBYB-UNITS-BNKRPT-3-1-2,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_1_2 =
                   :QBYB-OUTBAL-BNKRPT-3-1-2,
              BYBFILE.BYB_UNITS_BNKRPT_3_1_3 =
                   :QBYB-UNITS-BNKRPT-3-1-3,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_1_3 =
                   :QBYB-OUTBAL-BNKRPT-3-1-3,
              BYBFILE.BYB_UNITS_BNKRPT_3_1_4 =
                   :QBYB-UNITS-BNKRPT-3-1-4,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_1_4 =
                   :QBYB-OUTBAL-BNKRPT-3-1-4,
              BYBFILE.BYB_UNITS_BNKRPT_3_1_5 =
                   :QBYB-UNITS-BNKRPT-3-1-5,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_1_5 =
                   :QBYB-OUTBAL-BNKRPT-3-1-5,
              BYBFILE.BYB_UNITS_BNKRPT_3_1_6 =
                   :QBYB-UNITS-BNKRPT-3-1-6,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_1_6 =
                   :QBYB-OUTBAL-BNKRPT-3-1-6,
              BYBFILE.BYB_UNITS_BNKRPT_3_1_7 =
                   :QBYB-UNITS-BNKRPT-3-1-7,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_1_7 =
                   :QBYB-OUTBAL-BNKRPT-3-1-7,
              BYBFILE.BYB_UNITS_BNKRPT_3_1_8 =
                   :QBYB-UNITS-BNKRPT-3-1-8,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_1_8 =
                   :QBYB-OUTBAL-BNKRPT-3-1-8,
              BYBFILE.BYB_UNITS_BNKRPT_3_1_9 =
                   :QBYB-UNITS-BNKRPT-3-1-9,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_1_9 =
                   :QBYB-OUTBAL-BNKRPT-3-1-9,
              BYBFILE.BYB_UNITS_BNKRPT_3_1_10 =
                   :QBYB-UNITS-BNKRPT-3-1-10,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_1_10 =
                   :QBYB-OUTBAL-BNKRPT-3-1-10,
              BYBFILE.BYB_UNITS_BNKRPT_3_2_1 =
                   :QBYB-UNITS-BNKRPT-3-2-1,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_2_1 =
                   :QBYB-OUTBAL-BNKRPT-3-2-1,
              BYBFILE.BYB_UNITS_BNKRPT_3_2_2 =
                   :QBYB-UNITS-BNKRPT-3-2-2,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_2_2 =
                   :QBYB-OUTBAL-BNKRPT-3-2-2,
              BYBFILE.BYB_UNITS_BNKRPT_3_2_3 =
                   :QBYB-UNITS-BNKRPT-3-2-3,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_2_3 =
                   :QBYB-OUTBAL-BNKRPT-3-2-3,
              BYBFILE.BYB_UNITS_BNKRPT_3_2_4 =
                   :QBYB-UNITS-BNKRPT-3-2-4,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_2_4 =
                   :QBYB-OUTBAL-BNKRPT-3-2-4,
              BYBFILE.BYB_UNITS_BNKRPT_3_2_5 =
                   :QBYB-UNITS-BNKRPT-3-2-5,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_2_5 =
                   :QBYB-OUTBAL-BNKRPT-3-2-5,
              BYBFILE.BYB_UNITS_BNKRPT_3_2_6 =
                   :QBYB-UNITS-BNKRPT-3-2-6,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_2_6 =
                   :QBYB-OUTBAL-BNKRPT-3-2-6,
              BYBFILE.BYB_UNITS_BNKRPT_3_2_7 =
                   :QBYB-UNITS-BNKRPT-3-2-7,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_2_7 =
                   :QBYB-OUTBAL-BNKRPT-3-2-7,
              BYBFILE.BYB_UNITS_BNKRPT_3_2_8 =
                   :QBYB-UNITS-BNKRPT-3-2-8,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_2_8 =
                   :QBYB-OUTBAL-BNKRPT-3-2-8,
              BYBFILE.BYB_UNITS_BNKRPT_3_2_9 =
                   :QBYB-UNITS-BNKRPT-3-2-9,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_2_9 =
                   :QBYB-OUTBAL-BNKRPT-3-2-9,
              BYBFILE.BYB_UNITS_BNKRPT_3_2_10 =
                   :QBYB-UNITS-BNKRPT-3-2-10,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_2_10 =
                   :QBYB-OUTBAL-BNKRPT-3-2-10,
              BYBFILE.BYB_SUMWGTTM = :QBYB-SUMWGTTM,
              BYBFILE.BYB_SUMWGTAV = :QBYB-SUMWGTAV,
              BYBFILE.BYB_SUMWGTTA = :QBYB-SUMWGTTA,
              BYBFILE.BYB_IB_ACCRUED = :QBYB-IB-ACCRUED,
              BYBFILE.BYB_DLDISC_1 = :QBYB-DLDISC-1,
              BYBFILE.BYB_DLDISC_2 = :QBYB-DLDISC-2,
              BYBFILE.BYB_DLDISC_3 = :QBYB-DLDISC-3,
              BYBFILE.BYB_STD_POS = :QBYB-STD-POS,
              BYBFILE.BYB_30_DAY_MOVED = :QBYB-30-DAY-MOVED,
              BYBFILE.BYB_PROJ_INCREASE = :QBYB-PROJ-INCREASE,
              BYBFILE.BYB_PROJ_PROFIT = :QBYB-PROJ-PROFIT,
              BYBFILE.BYB_PROCEEDS_1 = :QBYB-PROCEEDS-1,
              BYBFILE.BYB_PROCEEDS_2 = :QBYB-PROCEEDS-2,
              BYBFILE.BYB_LOCKBOX_1 = :QBYB-LOCKBOX-1,
              BYBFILE.BYB_LOCKBOX_2 = :QBYB-LOCKBOX-2,
              BYBFILE.BYB_UNEARNED_AMT_1_1_1 =
                   :QBYB-UNEARNED-AMT-1-1-1,
              BYBFILE.BYB_UNEARNED_AMT_1_1_2 =
                   :QBYB-UNEARNED-AMT-1-1-2,
              BYBFILE.BYB_UNEARNED_AMT_1_1_3 =
                   :QBYB-UNEARNED-AMT-1-1-3,
              BYBFILE.BYB_UNEARNED_AMT_1_1_4 =
                   :QBYB-UNEARNED-AMT-1-1-4,
              BYBFILE.BYB_UNEARNED_AMT_1_1_5 =
                   :QBYB-UNEARNED-AMT-1-1-5,
              BYBFILE.BYB_UNEARNED_AMT_1_1_6 =
                   :QBYB-UNEARNED-AMT-1-1-6,
              BYBFILE.BYB_UNEARNED_AMT_1_1_7 =
                   :QBYB-UNEARNED-AMT-1-1-7,
              BYBFILE.BYB_UNEARNED_AMT_1_1_8 =
                   :QBYB-UNEARNED-AMT-1-1-8,
              BYBFILE.BYB_UNEARNED_AMT_1_1_9 =
                   :QBYB-UNEARNED-AMT-1-1-9,
              BYBFILE.BYB_UNEARNED_AMT_1_1_10 =
                   :QBYB-UNEARNED-AMT-1-1-10,
              BYBFILE.BYB_UNEARNED_AMT_1_2_1 =
                   :QBYB-UNEARNED-AMT-1-2-1,
              BYBFILE.BYB_UNEARNED_AMT_1_2_2 =
                   :QBYB-UNEARNED-AMT-1-2-2,
              BYBFILE.BYB_UNEARNED_AMT_1_2_3 =
                   :QBYB-UNEARNED-AMT-1-2-3,
              BYBFILE.BYB_UNEARNED_AMT_1_2_4 =
                   :QBYB-UNEARNED-AMT-1-2-4,
              BYBFILE.BYB_UNEARNED_AMT_1_2_5 =
                   :QBYB-UNEARNED-AMT-1-2-5,
              BYBFILE.BYB_UNEARNED_AMT_1_2_6 =
                   :QBYB-UNEARNED-AMT-1-2-6,
              BYBFILE.BYB_UNEARNED_AMT_1_2_7 =
                   :QBYB-UNEARNED-AMT-1-2-7,
              BYBFILE.BYB_UNEARNED_AMT_1_2_8 =
                   :QBYB-UNEARNED-AMT-1-2-8,
              BYBFILE.BYB_UNEARNED_AMT_1_2_9 =
                   :QBYB-UNEARNED-AMT-1-2-9,
              BYBFILE.BYB_UNEARNED_AMT_1_2_10 =
                   :QBYB-UNEARNED-AMT-1-2-10,
              BYBFILE.BYB_UNEARNED_AMT_2_1_1 =
                   :QBYB-UNEARNED-AMT-2-1-1,
              BYBFILE.BYB_UNEARNED_AMT_2_1_2 =
                   :QBYB-UNEARNED-AMT-2-1-2,
              BYBFILE.BYB_UNEARNED_AMT_2_1_3 =
                   :QBYB-UNEARNED-AMT-2-1-3,
              BYBFILE.BYB_UNEARNED_AMT_2_1_4 =
                   :QBYB-UNEARNED-AMT-2-1-4,
              BYBFILE.BYB_UNEARNED_AMT_2_1_5 =
                   :QBYB-UNEARNED-AMT-2-1-5,
              BYBFILE.BYB_UNEARNED_AMT_2_1_6 =
                   :QBYB-UNEARNED-AMT-2-1-6,
              BYBFILE.BYB_UNEARNED_AMT_2_1_7 =
                   :QBYB-UNEARNED-AMT-2-1-7,
              BYBFILE.BYB_UNEARNED_AMT_2_1_8 =
                   :QBYB-UNEARNED-AMT-2-1-8,
              BYBFILE.BYB_UNEARNED_AMT_2_1_9 =
                   :QBYB-UNEARNED-AMT-2-1-9,
              BYBFILE.BYB_UNEARNED_AMT_2_1_10 =
                   :QBYB-UNEARNED-AMT-2-1-10,
              BYBFILE.BYB_UNEARNED_AMT_2_2_1 =
                   :QBYB-UNEARNED-AMT-2-2-1,
              BYBFILE.BYB_UNEARNED_AMT_2_2_2 =
                   :QBYB-UNEARNED-AMT-2-2-2,
              BYBFILE.BYB_UNEARNED_AMT_2_2_3 =
                   :QBYB-UNEARNED-AMT-2-2-3,
              BYBFILE.BYB_UNEARNED_AMT_2_2_4 =
                   :QBYB-UNEARNED-AMT-2-2-4,
              BYBFILE.BYB_UNEARNED_AMT_2_2_5 =
                   :QBYB-UNEARNED-AMT-2-2-5,
              BYBFILE.BYB_UNEARNED_AMT_2_2_6 =
                   :QBYB-UNEARNED-AMT-2-2-6,
              BYBFILE.BYB_UNEARNED_AMT_2_2_7 =
                   :QBYB-UNEARNED-AMT-2-2-7,
              BYBFILE.BYB_UNEARNED_AMT_2_2_8 =
                   :QBYB-UNEARNED-AMT-2-2-8,
              BYBFILE.BYB_UNEARNED_AMT_2_2_9 =
                   :QBYB-UNEARNED-AMT-2-2-9,
              BYBFILE.BYB_UNEARNED_AMT_2_2_10 =
                   :QBYB-UNEARNED-AMT-2-2-10,
              BYBFILE.BYB_UNEARNED_AMT_3_1_1 =
                   :QBYB-UNEARNED-AMT-3-1-1,
              BYBFILE.BYB_UNEARNED_AMT_3_1_2 =
                   :QBYB-UNEARNED-AMT-3-1-2,
              BYBFILE.BYB_UNEARNED_AMT_3_1_3 =
                   :QBYB-UNEARNED-AMT-3-1-3,
              BYBFILE.BYB_UNEARNED_AMT_3_1_4 =
                   :QBYB-UNEARNED-AMT-3-1-4,
              BYBFILE.BYB_UNEARNED_AMT_3_1_5 =
                   :QBYB-UNEARNED-AMT-3-1-5,
              BYBFILE.BYB_UNEARNED_AMT_3_1_6 =
                   :QBYB-UNEARNED-AMT-3-1-6,
              BYBFILE.BYB_UNEARNED_AMT_3_1_7 =
                   :QBYB-UNEARNED-AMT-3-1-7,
              BYBFILE.BYB_UNEARNED_AMT_3_1_8 =
                   :QBYB-UNEARNED-AMT-3-1-8,
              BYBFILE.BYB_UNEARNED_AMT_3_1_9 =
                   :QBYB-UNEARNED-AMT-3-1-9,
              BYBFILE.BYB_UNEARNED_AMT_3_1_10 =
                   :QBYB-UNEARNED-AMT-3-1-10,
              BYBFILE.BYB_UNEARNED_AMT_3_2_1 =
                   :QBYB-UNEARNED-AMT-3-2-1,
              BYBFILE.BYB_UNEARNED_AMT_3_2_2 =
                   :QBYB-UNEARNED-AMT-3-2-2,
              BYBFILE.BYB_UNEARNED_AMT_3_2_3 =
                   :QBYB-UNEARNED-AMT-3-2-3,
              BYBFILE.BYB_UNEARNED_AMT_3_2_4 =
                   :QBYB-UNEARNED-AMT-3-2-4,
              BYBFILE.BYB_UNEARNED_AMT_3_2_5 =
                   :QBYB-UNEARNED-AMT-3-2-5,
              BYBFILE.BYB_UNEARNED_AMT_3_2_6 =
                   :QBYB-UNEARNED-AMT-3-2-6,
              BYBFILE.BYB_UNEARNED_AMT_3_2_7 =
                   :QBYB-UNEARNED-AMT-3-2-7,
              BYBFILE.BYB_UNEARNED_AMT_3_2_8 =
                   :QBYB-UNEARNED-AMT-3-2-8,
              BYBFILE.BYB_UNEARNED_AMT_3_2_9 =
                   :QBYB-UNEARNED-AMT-3-2-9,
              BYBFILE.BYB_UNEARNED_AMT_3_2_10 =
                   :QBYB-UNEARNED-AMT-3-2-10
             WHERE BYBFILE.BYB_DATE_CCYYMM = :QBYB-DATE-CCYYMM
               AND BYBFILE.BYB_BRNO = :QBYB-BRNO
               AND BYBFILE.BYB_CLASS = :QBYB-CLASS
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-BYB1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

           MOVE  '1' TO SQL-UPDATE-CONN-USE-SW. *> USE C1 CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

      *-----------------------------------------------------------------
       DELETE-BYB1-FILE. *> EMBEDDED - BYBFILE

           PERFORM DELETE-IT.
           MOVE BYB-PATH-SQL TO E-FILE.
           MOVE BYB1-KEY TO E-KEYX.
           PERFORM SET-BYB-FIELDS.

           EXEC SQL
            DELETE FROM DBO.BYBFILE
             WHERE BYBFILE.BYB_DATE_CCYYMM = :QBYB-DATE-CCYYMM
               AND BYBFILE.BYB_BRNO = :QBYB-BRNO
               AND BYBFILE.BYB_CLASS = :QBYB-CLASS
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-BYB1-FILE.

           MOVE  '1' TO SQL-UPDATE-CONN-USE-SW. *> USE C1 CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

      *-----------------------------------------------------------------
       CLOSE-BYB1-FILE.

      *-----------------------------------------------------------------
       REWRITE-BY1-FILE-CLEAR-BYCON. *> EMBEDDED - BYFILE

           PERFORM REWRITE-IT.
           MOVE BYB-PATH-SQL TO E-FILE.
           MOVE BYB1-KEY TO E-KEYX.
           PERFORM SET-BYB-FIELDS.

           MOVE BY-DATE-CCYYMM TO QBYB-DATE-CCYYMM.
           MOVE BY-BRNO        TO QBYB-BRNO.

           MOVE '2' TO SQL-UPDATE-CONN-USE-SW. *> USE UPDATE_CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

           EXEC SQL
            UPDATE DBO.BYBFILE
             SET
              BYBFILE.BYB_UNITS_1_2_1          = 0,
              BYBFILE.BYB_OUTBAL_1_2_1         = 0,
              BYBFILE.BYB_UNITS_1_2_2          = 0,
              BYBFILE.BYB_OUTBAL_1_2_2         = 0,
              BYBFILE.BYB_UNITS_1_2_3          = 0,
              BYBFILE.BYB_OUTBAL_1_2_3         = 0,
              BYBFILE.BYB_UNITS_1_2_4          = 0,
              BYBFILE.BYB_OUTBAL_1_2_4         = 0,
              BYBFILE.BYB_UNITS_1_2_5          = 0,
              BYBFILE.BYB_OUTBAL_1_2_5         = 0,
              BYBFILE.BYB_UNITS_1_2_6          = 0,
              BYBFILE.BYB_OUTBAL_1_2_6         = 0,
              BYBFILE.BYB_UNITS_1_2_7          = 0,
              BYBFILE.BYB_OUTBAL_1_2_7         = 0,
              BYBFILE.BYB_UNITS_1_2_8          = 0,
              BYBFILE.BYB_OUTBAL_1_2_8         = 0,
              BYBFILE.BYB_UNITS_1_2_9          = 0,
              BYBFILE.BYB_OUTBAL_1_2_9         = 0,
              BYBFILE.BYB_UNITS_1_2_10         = 0,
              BYBFILE.BYB_OUTBAL_1_2_10        = 0,
              BYBFILE.BYB_UNITS_BNKRPT_1_2_1   = 0,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_2_1  = 0,
              BYBFILE.BYB_UNITS_BNKRPT_1_2_2   = 0,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_2_2  = 0,
              BYBFILE.BYB_UNITS_BNKRPT_1_2_3   = 0,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_2_3  = 0,
              BYBFILE.BYB_UNITS_BNKRPT_1_2_4   = 0,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_2_4  = 0,
              BYBFILE.BYB_UNITS_BNKRPT_1_2_5   = 0, 
              BYBFILE.BYB_OUTBAL_BNKRPT_1_2_5  = 0,
              BYBFILE.BYB_UNITS_BNKRPT_1_2_6   = 0,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_2_6  = 0,
              BYBFILE.BYB_UNITS_BNKRPT_1_2_7   = 0,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_2_7  = 0,
              BYBFILE.BYB_UNITS_BNKRPT_1_2_8   = 0, 
              BYBFILE.BYB_OUTBAL_BNKRPT_1_2_8  = 0,
              BYBFILE.BYB_UNITS_BNKRPT_1_2_9   = 0,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_2_9  = 0,
              BYBFILE.BYB_UNITS_BNKRPT_1_2_10  = 0,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_2_10 = 0
             WHERE BYBFILE.BYB_DATE_CCYYMM     = :QBYB-DATE-CCYYMM
               AND BYBFILE.BYB_BRNO            = :QBYB-BRNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-BY1-FILE-CLEAR-BYCON.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

           MOVE  '1' TO SQL-UPDATE-CONN-USE-SW. *> USE C1 CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

      *-----------------------------------------------------------------
       REWRITE-BY1-FILE-CLEAR-BYREC. *> EMBEDDED - BYFILE

           PERFORM REWRITE-IT.
           MOVE BYB-PATH-SQL TO E-FILE.
           MOVE BYB1-KEY TO E-KEYX.
           PERFORM SET-BYB-FIELDS.

           MOVE BY-DATE-CCYYMM TO QBYB-DATE-CCYYMM.
           MOVE BY-BRNO        TO QBYB-BRNO.

           MOVE '2' TO SQL-UPDATE-CONN-USE-SW. *> USE UPDATE_CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

           EXEC SQL
            UPDATE DBO.BYBFILE
             SET 
              BYBFILE.BYB_UNITS_1_1_1          = 0,
              BYBFILE.BYB_OUTBAL_1_1_1         = 0,
              BYBFILE.BYB_UNITS_1_1_2          = 0,
              BYBFILE.BYB_OUTBAL_1_1_2         = 0,
              BYBFILE.BYB_UNITS_1_1_3          = 0,
              BYBFILE.BYB_OUTBAL_1_1_3         = 0,
              BYBFILE.BYB_UNITS_1_1_4          = 0,
              BYBFILE.BYB_OUTBAL_1_1_4         = 0,
              BYBFILE.BYB_UNITS_1_1_5          = 0,
              BYBFILE.BYB_OUTBAL_1_1_5         = 0,
              BYBFILE.BYB_UNITS_1_1_6          = 0,
              BYBFILE.BYB_OUTBAL_1_1_6         = 0,
              BYBFILE.BYB_UNITS_1_1_7          = 0,
              BYBFILE.BYB_OUTBAL_1_1_7         = 0,
              BYBFILE.BYB_UNITS_1_1_8          = 0,
              BYBFILE.BYB_OUTBAL_1_1_8         = 0,
              BYBFILE.BYB_UNITS_1_1_9          = 0,
              BYBFILE.BYB_OUTBAL_1_1_9         = 0,
              BYBFILE.BYB_UNITS_1_1_10         = 0,
              BYBFILE.BYB_OUTBAL_1_1_10        = 0,
              BYBFILE.BYB_UNITS_BNKRPT_1_1_1   = 0,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_1_1  = 0,
              BYBFILE.BYB_UNITS_BNKRPT_1_1_2   = 0,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_1_2  = 0,
              BYBFILE.BYB_UNITS_BNKRPT_1_1_3   = 0,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_1_3  = 0,
              BYBFILE.BYB_UNITS_BNKRPT_1_1_4   = 0,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_1_4  = 0,
              BYBFILE.BYB_UNITS_BNKRPT_1_1_5   = 0,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_1_5  = 0,
              BYBFILE.BYB_UNITS_BNKRPT_1_1_6   = 0,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_1_6  = 0,
              BYBFILE.BYB_UNITS_BNKRPT_1_1_7   = 0,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_1_7  = 0,
              BYBFILE.BYB_UNITS_BNKRPT_1_1_8   = 0,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_1_8  = 0,
              BYBFILE.BYB_UNITS_BNKRPT_1_1_9   = 0,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_1_9  = 0,
              BYBFILE.BYB_UNITS_BNKRPT_1_1_10  = 0,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_1_10 = 0
             WHERE BYBFILE.BYB_DATE_CCYYMM     = :QBYB-DATE-CCYYMM
               AND BYBFILE.BYB_BRNO            = :QBYB-BRNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-BY1-FILE-CLEAR-BYREC.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

           MOVE  '1' TO SQL-UPDATE-CONN-USE-SW. *> USE C1 CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

      *-----------------------------------------------------------------


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

           IF LN1-CONN-OPEN-SW = "N" 
              MOVE "Y"         TO LN1-CONN-OPEN-SW
              EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS LN1_CONNECT 
                       USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
              END-EXEC.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       START-LN1-FILE. *> SSP - LNFILE
           PERFORM START-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-PATH-SQL    TO E-FILE.
           MOVE LN1-KEY        TO E-KEYX.

           IF ( LN1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-LN1-FILE.

           IF ( QLN1-WEND-OWNBR = ZEROES    ) OR
              ( QLN1-WEND-OWNBR NOT NUMERIC )
              MOVE LN-OWNBR             TO QLN1-WBEG-OWNBR
              MOVE LN-OWNBR             TO QLN1-WEND-OWNBR
              MOVE LN-ACCTNO            TO QLN1-WBEG-ACCTNO
              MOVE ALL "9"              TO QLN1-WEND-ACCTNO.

      *  STORED PROCEDURE:
      *
      *   SELECT LNFILE.LN_OWNBR,
      *          LNFILE.LN_ACCTNO,
      *          LNFILE.LN_CLASS,
      *          LNFILE.LN_SUBCLASS,
      *          LNFILE.LN_LAWCODE,
      *          LNFILE.LN_ORGST, 
      *          LNFILE.LN_SPRCLASS,
      *          LNFILE.LN_CONVERCD, 
      *          LNFILE.LN_POCD, 
      *          LNFILE.LN_PLCD, 
      *          LNFILE.LN_SALEFIN, 
      *          LNFILE.LN_SERVACCT, 
      *          LNFILE.LN_LOANTYPE, 
      *          LNFILE.LN_ADDON_FG, 
      *          LNFILE.LN_PAY_SCHLD_FG, 
      *          CAST(LNFILE.LN_LOANDATE       AS VARCHAR(10)), 
      *          CAST(LNFILE.LN_PURDATE        AS VARCHAR(10)), 
      *          CAST(LNFILE.LN_INTDATE        AS VARCHAR(10)), 
      *          CAST(LNFILE.LN_CASHDATE       AS VARCHAR(10)), 
      *          CAST(LNFILE.LN_ORIG_1STPYDATE AS VARCHAR(10)), 
      *          CAST(LNFILE.LN_1STPYDATE      AS VARCHAR(10)), 
      *          CAST(LNFILE.LN_BNKRPTDATE     AS VARCHAR(10)), 
      *          LNFILE.LN_APRATE,
      *          LNFILE.LN_EFFRATE,
      *          LNFILE.LN_SMPRATE,
      *          LNFILE.LN_OVRDRATE,
      *          LNFILE.LN_LNAMT,
      *          LNFILE.LN_FINCHG,
      *          LNFILE.LN_INTCHG,
      *          LNFILE.LN_SERCHG,
      *          LNFILE.LN_EXTCHG,
      *          LNFILE.LN_MAINTFEE,
      *          LNFILE.LN_POINTS,
      *          LNFILE.LN_ORGTERM,
      *          LNFILE.LN_1STPYAMT,
      *          LNFILE.LN_NOREGPY,
      *          LNFILE.LN_REGPYAMT,
      *          LNFILE.LN_LASTPYAMT,
      *          LNFILE.LN_FEECODE_1, 
      *          LNFILE.LN_FEECODE_2, 
      *          LNFILE.LN_FEECODE_3, 
      *          LNFILE.LN_FEECODE_4, 
      *          LNFILE.LN_FEECODE_5, 
      *          LNFILE.LN_FEECODE_6, 
      *          LNFILE.LN_FEECODE_7, 
      *          LNFILE.LN_FEECODE_8, 
      *          LNFILE.LN_FEECODE_9, 
      *          LNFILE.LN_FEECODE_10, 
      *          LNFILE.LN_FEECODE_11, 
      *          LNFILE.LN_FEECODE_12, 
      *          LNFILE.LN_FEECODE_13, 
      *          LNFILE.LN_FEECODE_14, 
      *          LNFILE.LN_FEECODE_15, 
      *          LNFILE.LN_FEECODE_16, 
      *          LNFILE.LN_FEECODE_17, 
      *          LNFILE.LN_FEECODE_18, 
      *          LNFILE.LN_FEECODE_19, 
      *          LNFILE.LN_FEECODE_20, 
      *          LNFILE.LN_FEEAMT_1,
      *          LNFILE.LN_FEEAMT_2,
      *          LNFILE.LN_FEEAMT_3,
      *          LNFILE.LN_FEEAMT_4,
      *          LNFILE.LN_FEEAMT_5,
      *          LNFILE.LN_FEEAMT_6,
      *          LNFILE.LN_FEEAMT_7,
      *          LNFILE.LN_FEEAMT_8,
      *          LNFILE.LN_FEEAMT_9,
      *          LNFILE.LN_FEEAMT_10,
      *          LNFILE.LN_FEEAMT_11,
      *          LNFILE.LN_FEEAMT_12,
      *          LNFILE.LN_FEEAMT_13,
      *          LNFILE.LN_FEEAMT_14,
      *          LNFILE.LN_FEEAMT_15,
      *          LNFILE.LN_FEEAMT_16,
      *          LNFILE.LN_FEEAMT_17,
      *          LNFILE.LN_FEEAMT_18,
      *          LNFILE.LN_FEEAMT_19,
      *          LNFILE.LN_FEEAMT_20,
      *          LNFILE.LN_FEEOURS_1, 
      *          LNFILE.LN_FEEOURS_2, 
      *          LNFILE.LN_FEEOURS_3, 
      *          LNFILE.LN_FEEOURS_4, 
      *          LNFILE.LN_FEEOURS_5, 
      *          LNFILE.LN_FEEOURS_6, 
      *          LNFILE.LN_FEEOURS_7, 
      *          LNFILE.LN_FEEOURS_8, 
      *          LNFILE.LN_FEEOURS_9, 
      *          LNFILE.LN_FEEOURS_10, 
      *          LNFILE.LN_FEEOURS_11, 
      *          LNFILE.LN_FEEOURS_12, 
      *          LNFILE.LN_FEEOURS_13, 
      *          LNFILE.LN_FEEOURS_14, 
      *          LNFILE.LN_FEEOURS_15, 
      *          LNFILE.LN_FEEOURS_16, 
      *          LNFILE.LN_FEEOURS_17, 
      *          LNFILE.LN_FEEOURS_18, 
      *          LNFILE.LN_FEEOURS_19, 
      *          LNFILE.LN_FEEOURS_20, 
      *          LNFILE.LN_FEEFINCHG_1, 
      *          LNFILE.LN_FEEFINCHG_2, 
      *          LNFILE.LN_FEEFINCHG_3, 
      *          LNFILE.LN_FEEFINCHG_4, 
      *          LNFILE.LN_FEEFINCHG_5, 
      *          LNFILE.LN_FEEFINCHG_6, 
      *          LNFILE.LN_FEEFINCHG_7, 
      *          LNFILE.LN_FEEFINCHG_8, 
      *          LNFILE.LN_FEEFINCHG_9, 
      *          LNFILE.LN_FEEFINCHG_10, 
      *          LNFILE.LN_FEEFINCHG_11, 
      *          LNFILE.LN_FEEFINCHG_12, 
      *          LNFILE.LN_FEEFINCHG_13, 
      *          LNFILE.LN_FEEFINCHG_14, 
      *          LNFILE.LN_FEEFINCHG_15, 
      *          LNFILE.LN_FEEFINCHG_16, 
      *          LNFILE.LN_FEEFINCHG_17, 
      *          LNFILE.LN_FEEFINCHG_18, 
      *          LNFILE.LN_FEEFINCHG_19, 
      *          LNFILE.LN_FEEFINCHG_20, 
      *          LNFILE.LN_INSURED, 
      *          LNFILE.LN_INSTYPES, 
      *          LNFILE.LN_INSCOMP_1,
      *          LNFILE.LN_INSCOMP_2,
      *          LNFILE.LN_INSCOMP_3,
      *          LNFILE.LN_INSCOMP_4,
      *          LNFILE.LN_INSCOMP_5,
      *          LNFILE.LN_INSCOMP_6,
      *          LNFILE.LN_INSCOMP_7,
      *          LNFILE.LN_INSCOMP_8,
      *          LNFILE.LN_INSCOMP_9,
      *          LNFILE.LN_INSCOMP_10,
      *          LNFILE.LN_INSOURS_1, 
      *          LNFILE.LN_INSOURS_2, 
      *          LNFILE.LN_INSOURS_3, 
      *          LNFILE.LN_INSOURS_4, 
      *          LNFILE.LN_INSOURS_5, 
      *          LNFILE.LN_INSOURS_6, 
      *          LNFILE.LN_INSOURS_7, 
      *          LNFILE.LN_INSOURS_8, 
      *          LNFILE.LN_INSOURS_9, 
      *          LNFILE.LN_INSOURS_10, 
      *          CAST(LNFILE.LN_INSEFF_DATE_1  AS VARCHAR(10)), 
      *          CAST(LNFILE.LN_INSEFF_DATE_2  AS VARCHAR(10)), 
      *          CAST(LNFILE.LN_INSEFF_DATE_3  AS VARCHAR(10)), 
      *          CAST(LNFILE.LN_INSEFF_DATE_4  AS VARCHAR(10)), 
      *          CAST(LNFILE.LN_INSEFF_DATE_5  AS VARCHAR(10)), 
      *          CAST(LNFILE.LN_INSEFF_DATE_6  AS VARCHAR(10)), 
      *          CAST(LNFILE.LN_INSEFF_DATE_7  AS VARCHAR(10)), 
      *          CAST(LNFILE.LN_INSEFF_DATE_8  AS VARCHAR(10)), 
      *          CAST(LNFILE.LN_INSEFF_DATE_9  AS VARCHAR(10)), 
      *          CAST(LNFILE.LN_INSEFF_DATE_10 AS VARCHAR(10)), 
      *          CAST(LNFILE.LN_INSEXP_DATE_1  AS VARCHAR(10)), 
      *          CAST(LNFILE.LN_INSEXP_DATE_2  AS VARCHAR(10)), 
      *          CAST(LNFILE.LN_INSEXP_DATE_3  AS VARCHAR(10)), 
      *          CAST(LNFILE.LN_INSEXP_DATE_4  AS VARCHAR(10)), 
      *          CAST(LNFILE.LN_INSEXP_DATE_5  AS VARCHAR(10)), 
      *          CAST(LNFILE.LN_INSEXP_DATE_6  AS VARCHAR(10)), 
      *          CAST(LNFILE.LN_INSEXP_DATE_7  AS VARCHAR(10)), 
      *          CAST(LNFILE.LN_INSEXP_DATE_8  AS VARCHAR(10)), 
      *          CAST(LNFILE.LN_INSEXP_DATE_9  AS VARCHAR(10)), 
      *          CAST(LNFILE.LN_INSEXP_DATE_10 AS VARCHAR(10)), 
      *          LNFILE.LN_INSPREM_1,
      *          LNFILE.LN_INSPREM_2,
      *          LNFILE.LN_INSPREM_3,
      *          LNFILE.LN_INSPREM_4,
      *          LNFILE.LN_INSPREM_5,
      *          LNFILE.LN_INSPREM_6,
      *          LNFILE.LN_INSPREM_7,
      *          LNFILE.LN_INSPREM_8,
      *          LNFILE.LN_INSPREM_9,
      *          LNFILE.LN_INSPREM_10,
      *          LNFILE.LN_INSCOVR_1,
      *          LNFILE.LN_INSCOVR_2,
      *          LNFILE.LN_INSCOVR_3,
      *          LNFILE.LN_INSCOVR_4,
      *          LNFILE.LN_INSCOVR_5,
      *          LNFILE.LN_INSCOVR_6,
      *          LNFILE.LN_INSCOVR_7,
      *          LNFILE.LN_INSCOVR_8,
      *          LNFILE.LN_INSCOVR_9,
      *          LNFILE.LN_INSCOVR_10,
      *          LNFILE.LN_INSFINCHG_1, 
      *          LNFILE.LN_INSFINCHG_2, 
      *          LNFILE.LN_INSFINCHG_3, 
      *          LNFILE.LN_INSFINCHG_4, 
      *          LNFILE.LN_INSFINCHG_5, 
      *          LNFILE.LN_INSFINCHG_6, 
      *          LNFILE.LN_INSFINCHG_7, 
      *          LNFILE.LN_INSFINCHG_8, 
      *          LNFILE.LN_INSFINCHG_9, 
      *          LNFILE.LN_INSFINCHG_10, 
      *          LNFILE.LN_PY_YYYYMM_RECDEL,
      *          LNFILE.LN_MTD_PAYS_RECDEL,
      *          LNFILE.LN_DLNO,
      *          LNFILE.LN_CURBAL,
      *          LNFILE.LN_OT2BAL,
      *          LNFILE.LN_ANTICERN_1,
      *          LNFILE.LN_ANTICERN_2,
      *          LNFILE.LN_ANTICERN_3,
      *          LNFILE.LN_ANTICERN_4,
      *          LNFILE.LN_ANTICERN_5,
      *          LNFILE.LN_ANTICERN_6,
      *          LNFILE.LN_ANTICERN_7,
      *          LNFILE.LN_ANTICERN_8,
      *          LNFILE.LN_ANTICERN_9,
      *          LNFILE.LN_ANTICERN_10,
      *          LNFILE.LN_ANTICADJ_1,
      *          LNFILE.LN_ANTICADJ_2,
      *          LNFILE.LN_ANTICADJ_3,
      *          LNFILE.LN_ANTICADJ_4,
      *          LNFILE.LN_ANTICADJ_5,
      *          LNFILE.LN_ANTICADJ_6,
      *          LNFILE.LN_ANTICADJ_7,
      *          LNFILE.LN_ANTICADJ_8,
      *          LNFILE.LN_ANTICADJ_9,
      *          LNFILE.LN_ANTICADJ_10,
      *          LNFILE.LN_UFERN_1,
      *          LNFILE.LN_UFERN_2,
      *          LNFILE.LN_UFERN_3,
      *          LNFILE.LN_UFERN_4,
      *          LNFILE.LN_UFERN_5,
      *          LNFILE.LN_UFERN_6,
      *          LNFILE.LN_UFERN_7,
      *          LNFILE.LN_UFERN_8,
      *          LNFILE.LN_UFERN_9,
      *          LNFILE.LN_UFERN_10,
      *          LNFILE.LN_ACCERN_1,
      *          LNFILE.LN_ACCERN_2,
      *          LNFILE.LN_ACCERN_3,
      *          LNFILE.LN_ACCERN_4,
      *          LNFILE.LN_ACCERN_5,
      *          LNFILE.LN_ACCERN_6,
      *          LNFILE.LN_ACCERN_7,
      *          LNFILE.LN_ACCERN_8,
      *          LNFILE.LN_ACCERN_9,
      *          LNFILE.LN_ACCERN_10,
      *          LNFILE.LN_TOTINT,
      *          LNFILE.LN_TOTDEF,
      *          LNFILE.LN_TOTPAYMNTD,
      *          LNFILE.LN_TOTNODEF,
      *          LNFILE.LN_TOTNOALDEL,
      *          LNFILE.LN_CL_REBATE, 
      *          LNFILE.LN_AH_REBATE, 
      *          LNFILE.LN_PP_REBATE, 
      *          LNFILE.LN_INT_REBATE, 
      *          LNFILE.LN_SC_REBATE, 
      *          LNFILE.LN_MF_REBATE, 
      *          LNFILE.LN_DF_REBATE, 
      *          LNFILE.LN_EX_REBATE, 
      *          LNFILE.LN_O1_REBATE, 
      *          LNFILE.LN_O2_REBATE, 
      *          LNFILE.LN_O3_REBATE, 
      *          LNFILE.LN_O4_REBATE, 
      *          LNFILE.LN_O5_REBATE, 
      *          LNFILE.LN_O6_REBATE, 
      *          LNFILE.LN_O7_REBATE, 
      *          LNFILE.LN_UN_O7_REB, 
      *          LNFILE.LN_UN_O8_REB, 
      *          LNFILE.LN_UN_O9_REB, 
      *          LNFILE.LN_UN_O10_REB, 
      *          CAST(LNFILE.LN_DATE_PAID_LAST AS VARCHAR(10)), 
      *          LNFILE.LN_CREDITLIM,
      *          LNFILE.LN_OLDACCTNO,
      *          LNFILE.LN_PRLNNO,
      *          LNFILE.LN_SUM_FINCHG,
      *          LNFILE.LN_SUM_ORGTERM,
      *          LNFILE.LN_UNITPER_CD, 
      *          LNFILE.LN_UNITPER_FREQ,
      *          LNFILE.LN_REPOCD 
      *  
      *     FROM DBO.LNFILE  LNFILE
      *  
      *    WHERE  LNFILE.LN_OWNBR       = @QLN1_WBEG_OWNBR 
      *      AND  LNFILE.LN_POCD   NOT IN ('**', '*I', '*R', '*U') 
      *      AND  LNFILE.LN_CURBAL     != 0
      *      AND  LNFILE.LN_PLCD        = ' '
      *      AND  LNFILE.LN_REPOCD NOT IN ('X', 'S')
      *  
      *    ORDER BY LNFILE.LN_OWNBR,
      *             LNFILE.LN_CLASS,
      *             LNFILE.LN_ACCTNO; 

           EXEC SQL SET CONNECTION LN1_CONNECT END-EXEC.

           EXEC SQL
            DECLARE LN1_CURSOR CURSOR FOR
                :LN1-RETURN-CODE = EXEC SSP_DAILY_LNFILE_SELECT 
              ( :QLN1-WBEG-OWNBR )
           END-EXEC.

           EXEC SQL
               OPEN LN1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QLN1-WBEG-OWNBR
                          QLN1-WEND-OWNBR
                          QLN1-WBEG-ACCTNO
                          QLN1-WEND-ACCTNO.

           MOVE STAT---GOOD TO LN1-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC. 

      *-----------------------------------------------------------------
       READ-LN1-FILE-NEXT. *> EMBEDDED - LNFILE
           PERFORM READ-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-PATH-SQL    TO E-FILE.
           MOVE LN1-KEY        TO E-KEYX.
           INITIALIZE QLN-REC.

           EXEC SQL SET CONNECTION LN1_CONNECT END-EXEC.

           IF ( LN1-CURSOR-STAT-GOOD )
              EXEC SQL
                FETCH LN1_CURSOR 
                 INTO 
                  :QLN-OWNBR,
                  :QLN-ACCTNO,
                  :QLN-CLASS,
                  :QLN-SUBCLASS,
                  :QLN-LAWCODE,
                  :QLN-ORGST, 
                  :QLN-SPRCLASS,
                  :QLN-CONVERCD, 
                  :QLN-POCD, 
                  :QLN-PLCD, 
                  :QLN-SALEFIN, 
                  :QLN-SERVACCT, 
                  :QLN-LOANTYPE, 
                  :QLN-ADDON-FG, 
                  :QLN-PAY-SCHLD-FG, 
                  :QLN-LOANDATE, 
                  :QLN-PURDATE, 
                  :QLN-INTDATE, 
                  :QLN-CASHDATE, 
                  :QLN-ORIG-1STPYDATE, 
                  :QLN-1STPYDATE, 
                  :QLN-BNKRPTDATE, 
                  :QLN-APRATE,
                  :QLN-EFFRATE,
                  :QLN-SMPRATE,
                  :QLN-OVRDRATE,
                  :QLN-LNAMT,
                  :QLN-FINCHG,
                  :QLN-INTCHG,
                  :QLN-SERCHG,
                  :QLN-EXTCHG,
                  :QLN-MAINTFEE,
                  :QLN-POINTS,
                  :QLN-ORGTERM,
                  :QLN-1STPYAMT,
                  :QLN-NOREGPY,
                  :QLN-REGPYAMT,
                  :QLN-LASTPYAMT,
                  :QLN-FEECODE-1, 
                  :QLN-FEECODE-2, 
                  :QLN-FEECODE-3, 
                  :QLN-FEECODE-4, 
                  :QLN-FEECODE-5, 
                  :QLN-FEECODE-6, 
                  :QLN-FEECODE-7, 
                  :QLN-FEECODE-8, 
                  :QLN-FEECODE-9, 
                  :QLN-FEECODE-10, 
                  :QLN-FEECODE-11, 
                  :QLN-FEECODE-12, 
                  :QLN-FEECODE-13, 
                  :QLN-FEECODE-14, 
                  :QLN-FEECODE-15, 
                  :QLN-FEECODE-16, 
                  :QLN-FEECODE-17, 
                  :QLN-FEECODE-18, 
                  :QLN-FEECODE-19, 
                  :QLN-FEECODE-20, 
                  :QLN-FEEAMT-1,
                  :QLN-FEEAMT-2,
                  :QLN-FEEAMT-3,
                  :QLN-FEEAMT-4,
                  :QLN-FEEAMT-5,
                  :QLN-FEEAMT-6,
                  :QLN-FEEAMT-7,
                  :QLN-FEEAMT-8,
                  :QLN-FEEAMT-9,
                  :QLN-FEEAMT-10,
                  :QLN-FEEAMT-11,
                  :QLN-FEEAMT-12,
                  :QLN-FEEAMT-13,
                  :QLN-FEEAMT-14,
                  :QLN-FEEAMT-15,
                  :QLN-FEEAMT-16,
                  :QLN-FEEAMT-17,
                  :QLN-FEEAMT-18,
                  :QLN-FEEAMT-19,
                  :QLN-FEEAMT-20,
                  :QLN-FEEOURS-1, 
                  :QLN-FEEOURS-2, 
                  :QLN-FEEOURS-3, 
                  :QLN-FEEOURS-4, 
                  :QLN-FEEOURS-5, 
                  :QLN-FEEOURS-6, 
                  :QLN-FEEOURS-7, 
                  :QLN-FEEOURS-8, 
                  :QLN-FEEOURS-9, 
                  :QLN-FEEOURS-10, 
                  :QLN-FEEOURS-11, 
                  :QLN-FEEOURS-12, 
                  :QLN-FEEOURS-13, 
                  :QLN-FEEOURS-14, 
                  :QLN-FEEOURS-15, 
                  :QLN-FEEOURS-16, 
                  :QLN-FEEOURS-17, 
                  :QLN-FEEOURS-18, 
                  :QLN-FEEOURS-19, 
                  :QLN-FEEOURS-20, 
                  :QLN-FEEFINCHG-1, 
                  :QLN-FEEFINCHG-2, 
                  :QLN-FEEFINCHG-3, 
                  :QLN-FEEFINCHG-4, 
                  :QLN-FEEFINCHG-5, 
                  :QLN-FEEFINCHG-6, 
                  :QLN-FEEFINCHG-7, 
                  :QLN-FEEFINCHG-8, 
                  :QLN-FEEFINCHG-9, 
                  :QLN-FEEFINCHG-10, 
                  :QLN-FEEFINCHG-11, 
                  :QLN-FEEFINCHG-12, 
                  :QLN-FEEFINCHG-13, 
                  :QLN-FEEFINCHG-14, 
                  :QLN-FEEFINCHG-15, 
                  :QLN-FEEFINCHG-16, 
                  :QLN-FEEFINCHG-17, 
                  :QLN-FEEFINCHG-18, 
                  :QLN-FEEFINCHG-19, 
                  :QLN-FEEFINCHG-20, 
                  :QLN-INSURED, 
                  :QLN-INSTYPES, 
                  :QLN-INSCOMP-1,
                  :QLN-INSCOMP-2,
                  :QLN-INSCOMP-3,
                  :QLN-INSCOMP-4,
                  :QLN-INSCOMP-5,
                  :QLN-INSCOMP-6,
                  :QLN-INSCOMP-7,
                  :QLN-INSCOMP-8,
                  :QLN-INSCOMP-9,
                  :QLN-INSCOMP-10,
                  :QLN-INSOURS-1, 
                  :QLN-INSOURS-2, 
                  :QLN-INSOURS-3, 
                  :QLN-INSOURS-4, 
                  :QLN-INSOURS-5, 
                  :QLN-INSOURS-6, 
                  :QLN-INSOURS-7, 
                  :QLN-INSOURS-8, 
                  :QLN-INSOURS-9, 
                  :QLN-INSOURS-10, 
                  :QLN-INSEFF-DATE-1,  
                  :QLN-INSEFF-DATE-2, 
                  :QLN-INSEFF-DATE-3, 
                  :QLN-INSEFF-DATE-4, 
                  :QLN-INSEFF-DATE-5, 
                  :QLN-INSEFF-DATE-6, 
                  :QLN-INSEFF-DATE-7, 
                  :QLN-INSEFF-DATE-8, 
                  :QLN-INSEFF-DATE-9, 
                  :QLN-INSEFF-DATE-10, 
                  :QLN-INSEXP-DATE-1, 
                  :QLN-INSEXP-DATE-2, 
                  :QLN-INSEXP-DATE-3, 
                  :QLN-INSEXP-DATE-4, 
                  :QLN-INSEXP-DATE-5, 
                  :QLN-INSEXP-DATE-6, 
                  :QLN-INSEXP-DATE-7, 
                  :QLN-INSEXP-DATE-8, 
                  :QLN-INSEXP-DATE-9, 
                  :QLN-INSEXP-DATE-10, 
                  :QLN-INSPREM-1,
                  :QLN-INSPREM-2,
                  :QLN-INSPREM-3,
                  :QLN-INSPREM-4,
                  :QLN-INSPREM-5,
                  :QLN-INSPREM-6,
                  :QLN-INSPREM-7,
                  :QLN-INSPREM-8,
                  :QLN-INSPREM-9,
                  :QLN-INSPREM-10,
                  :QLN-INSCOVR-1,
                  :QLN-INSCOVR-2,
                  :QLN-INSCOVR-3,
                  :QLN-INSCOVR-4,
                  :QLN-INSCOVR-5,
                  :QLN-INSCOVR-6,
                  :QLN-INSCOVR-7,
                  :QLN-INSCOVR-8,
                  :QLN-INSCOVR-9,
                  :QLN-INSCOVR-10,
                  :QLN-INSFINCHG-1, 
                  :QLN-INSFINCHG-2, 
                  :QLN-INSFINCHG-3, 
                  :QLN-INSFINCHG-4, 
                  :QLN-INSFINCHG-5, 
                  :QLN-INSFINCHG-6, 
                  :QLN-INSFINCHG-7, 
                  :QLN-INSFINCHG-8, 
                  :QLN-INSFINCHG-9, 
                  :QLN-INSFINCHG-10, 
                  :QLN-PY-YYYYMM-RECDEL,
                  :QLN-MTD-PAYS-RECDEL,
                  :QLN-DLNO,
                  :QLN-CURBAL,
                  :QLN-OT2BAL,
                  :QLN-ANTICERN-1,
                  :QLN-ANTICERN-2,
                  :QLN-ANTICERN-3,
                  :QLN-ANTICERN-4,
                  :QLN-ANTICERN-5,
                  :QLN-ANTICERN-6,
                  :QLN-ANTICERN-7,
                  :QLN-ANTICERN-8,
                  :QLN-ANTICERN-9,
                  :QLN-ANTICERN-10,
                  :QLN-ANTICADJ-1,
                  :QLN-ANTICADJ-2,
                  :QLN-ANTICADJ-3,
                  :QLN-ANTICADJ-4,
                  :QLN-ANTICADJ-5,
                  :QLN-ANTICADJ-6,
                  :QLN-ANTICADJ-7,
                  :QLN-ANTICADJ-8,
                  :QLN-ANTICADJ-9,
                  :QLN-ANTICADJ-10,
                  :QLN-UFERN-1,
                  :QLN-UFERN-2,
                  :QLN-UFERN-3,
                  :QLN-UFERN-4,
                  :QLN-UFERN-5,
                  :QLN-UFERN-6,
                  :QLN-UFERN-7,
                  :QLN-UFERN-8,
                  :QLN-UFERN-9,
                  :QLN-UFERN-10,
                  :QLN-ACCERN-1,
                  :QLN-ACCERN-2,
                  :QLN-ACCERN-3,
                  :QLN-ACCERN-4,
                  :QLN-ACCERN-5,
                  :QLN-ACCERN-6,
                  :QLN-ACCERN-7,
                  :QLN-ACCERN-8,
                  :QLN-ACCERN-9,
                  :QLN-ACCERN-10,
                  :QLN-TOTINT,
                  :QLN-TOTDEF,
                  :QLN-TOTPAYMNTD,
                  :QLN-TOTNODEF,
                  :QLN-TOTNOALDEL,
                  :QLN-CL-REBATE, 
                  :QLN-AH-REBATE, 
                  :QLN-PP-REBATE, 
                  :QLN-INT-REBATE, 
                  :QLN-SC-REBATE, 
                  :QLN-MF-REBATE, 
                  :QLN-DF-REBATE, 
                  :QLN-EX-REBATE, 
                  :QLN-O1-REBATE, 
                  :QLN-O2-REBATE, 
                  :QLN-O3-REBATE, 
                  :QLN-O4-REBATE, 
                  :QLN-O5-REBATE, 
                  :QLN-O6-REBATE, 
                  :QLN-O7-REBATE, 
                  :QLN-UN-O7-REB, 
                  :QLN-UN-O8-REB, 
                  :QLN-UN-O9-REB, 
                  :QLN-UN-O10-REB, 
                  :QLN-DATE-PAID-LAST, 
                  :QLN-CREDITLIM,
                  :QLN-OLDACCTNO,
                  :QLN-PRLNNO,
                  :QLN-SUM-FINCHG,
                  :QLN-SUM-ORGTERM,
                  :QLN-UNITPER-CD, 
                  :QLN-UNITPER-FREQ,
                  :QLN-REPOCD 
             END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LN1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-LN-FIELDS.

          EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       CLOSE-LN1-FILE.

           PERFORM CLOSE-IT.
           IF ( LN1-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION LN1_CONNECT END-EXEC
              EXEC SQL
                   CLOSE LN1_CURSOR
              END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE STAT---BAD TO LN1-CURSOR-STAT.

      *-----------------------------------------------------------------
       CLOSE-LN1-CONNECTION.

           IF LN1-CONN-OPEN-SW = "Y"
              EXEC SQL SET CONNECTION LN1_CONNECT END-EXEC
              EXEC SQL COMMIT END-EXEC
              EXEC SQL DISCONNECT LN1_CONNECT END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE "N" TO LN1-CONN-OPEN-SW.

      *-----------------------------------------------------------------

       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBLP/LPLTP1RN.CPY".
       COPY "LIBLP/LPLTI1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      *******************************************
       END-PROGRAM SECTION.
      *******************************************
       END-RUN.
           PERFORM CLOSE-FILES.

           IF PR-OPEN-FG = "Y"
              IF ( SKIP-RECORDS-FOUND )
                 GO TO EXIT-PROG.

           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.

       EXIT-PROG.
           IF EXT-ROUTE-BUF = "00"
              MOVE "LP/LPEDMU" TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY DAILY-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.

           EXIT PROGRAM.
