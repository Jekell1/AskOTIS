       IDENTIFICATION DIVISION.
       PROGRAM-ID.      EOYUPD.
       DATE-WRITTEN. 053184.
      *****************************************************************
      *                 (LP)
      *  DESCRIPTION:  END OF YEAR CLEAR
      *                    CALLED BY UPDEOY
      *
      *      **** DOES NOT RUN IN BATCH ****
      * REV:
      *  MJD 091895 ADDED DS-DATE LOGIC.
      *  MJD 122895 ADDED FILE PATH LOGIC.  MODIFIED TO PROCESS ONLY ONE
      *             BRANCH PER CALL. BRANCH PASSED IN VIA FORM.
      *             ADDED REPO TABLES TO DS AND BY UPDATES.
      *             CHANGED BY-INS-TYPE CLEAR LOOP TO SUBSCRIPT TO 10
      *             ADDED ERR LOG ENTRY IF ANY ONE FILE DOES NOT
      *             GET UPDATED.
      *  MJD 012496 ADDED UPDATE/CLEAR OF NEW FIELDS IN BY-REC:
      *                O2 AND INTEREST, PL TABLE, DLR RESERVES,
      *                LOANS SOLD/TRANS/PURCH, P&L /PRIN/INT/LATE
      *                COLLECTED.
      *  BLL 012696 CHANGED BY CLEAR - DLR TABLE FORMAT, ADD PL-O2COL
      *  MJD 020196 ADDED BY-PL-O2COL.
      *  KEC 111496 CHANGED LN-POCD FOR RESCISSION.
      *  KEC 030797 CHANGED TO HANDLE NEW APPLICATIONS STATISTICS IN
      *              BY-FILE, DS-FILE AND TY-FILE.
      *  BLV 052197 ADD BY-NO-MADE, BY-NO-TURNUSDOWN &
      *             DS-NO-MADE, DS-NO-TURNUSDOWN FOR APP STATS
      *  MJD 070297 ADDED BY-DLDISC AND DS-DLDISC-VOL.
      *  MJD 051998 CHANGED BY-EXREB TO BE BY-PART-CHGOFF
      *  JTG 062998 CHANGED DS-CNVT.... TO DS-RNEW....
      *                     DS-SVOS.... TO DS-TODL....
      *  MJD 000117 FIXED BUG. ERR MESSAGES WERE PRINTED FOR BY/DS
      *             WHEN RUNNING CALANDER AND LN/BW ERRORS WERE PRINTED
      *             CHEN RUNNING FISCAL.
      *  MJD 000322 REMOVED DS-CNEARN/EARNTAKN/EARNDUE/CNUNEARN/ERNOTTAK
      *             ADDED DS-UNEARNED-AMT (MERC #223)
      *  MJD 000329 ADDED BY-UNEARNED-AMT (MERC #223)
      *  MJD 000331 ADDED DS-PROCEEDS (MERC PR#223)
      *  MJD 000929 ADDED BY-LOCKBOX (MFC PR#237)
      *  BLF 060821 IF GLOBAL PARAMETER FOR PAYEE PROCESSING IS TURNED
      *             ON, CLEAR PAFILE YTD IF DOING END OF YEAR FOR
      *             CALENDAR OR BOTH.  DEVELOPEMNT PR# 2633
      *  BLF 090212 IF WORLD MEXICO, RESET GB-NEXT-RECEIPT TO 1,
      *             WORLD PR# 624
      *  BLM 100203 ADDED PROCESSING FOR ESCROW, ESFILE, REGWAN PR# 3506
      *   CS 151104 ADDED ES-DISBURSED-CNT-TAXES, CLEAR AT END OF YEAR, COLTN
      *  BLM 160314 FIX FOR NEW BY-DATE FORMAT, PD0003
      *  BLM 160512 FIX FOR NEW DS-DATE FORMAT, FIX DS-UNEARNED, PD0003
      *
      * KEC 2016.0929 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED BY-DATE TO BY-DATE-CCYYMM FOR CONSISTENCY.
      *
      * KEC 2017.0105 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          CHANGED DS-DATE TO DS-DATE-CCYYMM FOR CONSISTENCY
      *          WITH THE DATE FIELDS IN BY & UD FILE.
      *
      * BAH 170531 ACTIVATE 8 DIGIT DATES, PD0006
      * BLM 180228 FIX TO CLEAR YTD BY-FEE-TYPE TABLE AND ADJUST ROUTINES
      *            FOR BY-UNEARNED-AMT & DS-UNEARNED-AMT OCCURING 10 NOT 6
      * BAH 180928 GLOBAL BWFILE
      * BAH 181015 GLOBAL DSFILE
      * BAH 181207 GLOBAL LNFILE
      * AMM 190206 GLOBAL PAFILE
      *  CS 190207 GLOBAL ESFILE
      * KEC 190212 A32 F2SL - REVISED FOR SPLITTING THE BYFILE.
      *            REMOVED LOAN APP FIELDS IN BY & TY.
      *  CS 190423 REMOVE DS APP STATS
      *  CS 190625 REMOVED REFERENCE TO MEXICO
      *  BAH 20190924 REMOVED ACCESS-CALL LNFILE
      * BAH 20230908 REMOVED PA-FILE
      * RMZ 20240304 PERFORMANCE TUNING - ADDED STORED PROCEDURES
      * RMZ 20240612 LOCKING ISSUE - ADDED STORED PROCEDURE/TEMP TABLE
      *              PROCESS, TO AVOID SQL DATABASE LOCK. (S35Q-67) 
      * RMZ 20240806 ADDED LTOUCH-DATE TO UPDATE SCRIPTS. (S35Q-108)
      * RMZ 20250206 COMMENTED OUT REWRITE-DS1-FILE.      (S35Q-163) 
      * RMZ 20250416 * COMMENTED OUT ALL PROCESSING OF DSFILE AND 
      *                ESFILE.
      *              * BECAUSE THE LNFILE & BWFILE ARE STRAIGHT FORWARD
      *                UPDATES, THE PROGRAM WAS CHANGED TO NOT START/READ/
      *                UPDATE EACH ROW ON EITHER TABLE, IT IS NOW DOING TWO
      *                MASS-UPDATES FOR THE ENTIRE BRANCH FOR EACH TABLE,
      *                ADDING IN THE RESTRICTION IN THE "WHERE CLAUSE" 
      *                FOR LNFILE.
      *              * COMMENTED OUT THE UPDATE TO BIFILE IN THE UPDATE 
      *                OF LNFILE. THE FLAG IS NEVER SET TO 'Y' SO IT SHOULD
      *                NOT EXECUTE. BUT IF IT DID, IT WOULD NOT WORK 
      *                CORRECTLY WITH A MASS-UPDATE. (S35Q-163)
      *
      *********************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       FILE SECTION.

      *****************************************************************
      *      WORKING STORAGE SECTION
      *****************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LP/EOYUPD.CBL S35 04/16/25-16:04:27 >".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01BY.CPY".
       COPY "LIBLP/LP01BYA.CPY".
       COPY "LIBLP/LP01BYB.CPY".
       COPY "LIBLP/LP01BY_SQL.CPY".
       COPY "LIBLP/LPWSBY.CPY".
      *COPY "LIBLP/LP01DS.CPY".
      *COPY "LIBLP/LP01DS_SQL.CPY".
      *COPY "LIBLP/LPWSDS.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
      *COPY "LIBLP/LP01ES.CPY".
      *COPY "LIBLP/LP01ES_SQL.CPY".
      *COPY "LIBLP/LPWSES.CPY".

       01  BW1-CONN-OPEN-SW            PIC X     VALUE "N".
       01  DS1-CONN-OPEN-SW            PIC X     VALUE "N".
       01  ES1-CONN-OPEN-SW            PIC X     VALUE "N".
       01  LN1-CONN-OPEN-SW            PIC X     VALUE "N".
       01  BY1-CONN-OPEN-SW            PIC X     VALUE "N".

       01  SUB               PIC 9(6) COMP-3.

       01  LN-FOUND-FG       PIC X    VALUE "N".
       01  BW-FOUND-FG       PIC X    VALUE "N".
       01  PA-FOUND-FG       PIC X    VALUE "N".
       01  DS-FOUND-FG       PIC X    VALUE "N".
       01  BY-FOUND-FG       PIC X    VALUE "N".
       01  ES-FOUND-FG       PIC X    VALUE "N".

       COPY "LIBGB/EOBUF_EXT.CPY".

       01  YEAR-BUF          PIC 9(2) VALUE 0.
       01  MM-BUF            PIC 9(2) VALUE 0.
       01  NEXT-MO-BY-DATE-CCYYMM   PIC 9(6) VALUE 0.
       01  HOLD-DS-DATE-CCYYMM      PIC 9(6) VALUE 0.

       01  MESS-LIST         PIC X(5) VALUE "MESS.".
       01  MESS-BUF          PIC X(50) VALUE SPACES.

       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/EOYUPD_DEF.CPY".
       COPY "LIBLP/EOYUPD_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

      *****************************************************************
       COPY "LIBGB/GETFMW.CPY".
       01  EOY-LINK.
           03  YRTYPE-BUF        PIC X.
           03  BRANCH-BUF        PIC 9(4).
           03  TEST-DATE         PIC X(5).
           03  TEST-DATEX REDEFINES TEST-DATE.
               05  TEST-MM       PIC 99.
               05  TEST-DASH     PIC X.
               05  TEST-YY       PIC 99.
           03  ERRCD-BUF         PIC X.
           03  EOYUPD-WINDOW-HANDLE  PIC X(10).

       SCREEN SECTION.
       COPY "LIBLP/EOYUPD_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EOY-LINK.

      *****************************************************************
      *       MAIN PROGRAM CONTROL
      *****************************************************************
       MAIN-MODULE SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "EOYUPD" TO VDU-FORM-NAME.

      *----------------------------------------------------------------
      * YRTYPE-BUF : TYPE OF UPDATE  C - CALENDER
      *                              F - FISCAL
      *                              B - BOTH
      * YEAR-BUF   : YEAR TO UPDATE/CLEAR.
      * MM-BUF     : MONTH TO UPDATE/CLEAR.
      * BRANCH-BUF : BRANCH TO UPDATE-CLEAR.
      *----------------------------------------------------------------
           MOVE TEST-MM TO MM-BUF.
           MOVE TEST-YY TO YEAR-BUF.

           PERFORM GET-GPENV.

           MOVE BRANCH-BUF TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.
           PERFORM CLOSE-BR-FILE.

       MOVE-BRANCH-PATH-INFO.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO BW-PATH-OVERRIDE
                                              BY-PATH-OVERRIDE
      *                                       DS-PATH-OVERRIDE
      *                                       ES-PATH-OVERRIDE
                                              LN-PATH-OVERRIDE.

           PERFORM SQL-OPEN-UPDATE-CONNECTION.

      *----------------------------------------------------------------
      * CALENDAR - C - : UPDATE / CLEAR BORROWER AND LOAN FILES.
      * FISCAL   - F - : UPDATE / CLEAR DEALER AND BRANCH STAT FILES.
      * BOTH     - B - : PERFORM BOTH CALENDAR AND FISCAL UPDATES.
      *-----------------------------------------------------------------

           IF YRTYPE-BUF = "C" OR "B"
              PERFORM UPDATE-LN-FILE
              PERFORM UPDATE-BW-FILE.
      *       PERFORM UPDATE-ES-FILE.

           IF YRTYPE-BUF = "F" OR "B"
      *       PERFORM UPDATE-DS-FILE
              PERFORM UPDATE-BY-FILE.

      *----------------------------------------------------------------
      * WRITE ERROR LOG IF ANY ONE FILE IS NOT PROCESSED.
      *----------------------------------------------------------------
           IF YRTYPE-BUF = "C" OR "B"
              IF LN-FOUND-FG NOT = "Y"
                MOVE "WARNING : LN-FILE NOT UPDATED" TO ERRLOGW-EOM-MSG
              ELSE
                MOVE "LN-FILE PROCESSED SUCCESSFULLY" TO ERRLOGW-EOM-MSG
              END-IF
              PERFORM LOG-ERROR
              IF BW-FOUND-FG NOT = "Y"
                MOVE "WARNING : BW-FILE NOT UPDATED" TO ERRLOGW-EOM-MSG
              ELSE
                MOVE "BW-FILE PROCESSED SUCCESSFULLY" TO ERRLOGW-EOM-MSG
              END-IF
              PERFORM LOG-ERROR
              IF PA-FOUND-FG NOT = "Y"
                MOVE "WARNING : PA-FILE NOT UPDATED" TO ERRLOGW-EOM-MSG
              ELSE
                MOVE "PA-FILE PROCESSED SUCCESSFULLY" TO ERRLOGW-EOM-MSG
              END-IF
              PERFORM LOG-ERROR
      *       IF ES-FOUND-FG NOT = "Y"
      *         MOVE "WARNING : ES-FILE NOT UPDATED" TO ERRLOGW-EOM-MSG
      *       ELSE
      *         MOVE "ES-FILE PROCESSED SUCCESSFULLY" TO ERRLOGW-EOM-MSG
      *       END-IF
              PERFORM LOG-ERROR.

           IF YRTYPE-BUF = "F" OR "B"
      *       IF DS-FOUND-FG NOT = "Y"
      *         MOVE "WARNING : DS-FILE NOT UPDATED" TO ERRLOGW-EOM-MSG
      *       ELSE
      *         MOVE "DS-FILE PROCESSED SUCCESSFULLY" TO ERRLOGW-EOM-MSG
      *       END-IF
              PERFORM LOG-ERROR
              IF BY-FOUND-FG NOT = "Y"
                MOVE "WARNING : BY-FILE NOT UPDATED" TO ERRLOGW-EOM-MSG
              ELSE
                MOVE "BY-FILE PROCESSED SUCCESSFULLY" TO ERRLOGW-EOM-MSG
              END-IF
              PERFORM LOG-ERROR.

           GO TO END-ROUTINE.


      *****************************************************************
      *UPDATE-DS-FILE SECTION.
      *****************************************************************
      *    MOVE "CLEARING DEALER STATISTICS EOY ....&&&&" TO MESS-BUF.
      *    INSPECT MESS-BUF REPLACING FIRST "&&&&" BY BR-NO.
      *    MOVE MESS-BUF TO WR-BUF.
      *    MOVE MESS-LIST TO WR-LIST.
      *    PERFORM CALL-WRFORM.

      *    PERFORM OPEN-DS1-FILE.

      * SETUP FIRST DAY OF THE NEXT MONTH:
      *    MOVE YEAR-BUF TO DATE-MMDDYY-YY.
      *    MOVE MM-BUF   TO DATE-MMDDYY-MM.
      *    MOVE 01       TO DATE-MMDDYY-DD.
      *    PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.

      *    MOVE DATE-YYYYMMDD TO NDTE-DATE.
      *    MOVE 31            TO NDTE-DD.
      *    PERFORM NEW-DATE-CALCULATION.
      *    MOVE 1 TO NDTE-HOLD.
      *    PERFORM INCREMENT-MONTHS.
      *    MOVE NDTE-DATE TO DATE-YYYYMMDD.
      *    PERFORM CONVERT-YYYYMMDD-TO-YYYYMM.

      * START DS-FILE WITH NEXT MONTHS KEY, TO CLEAR EOY
      *    MOVE DATE-YYYYMM   TO DS-DATE-CCYYMM HOLD-DS-DATE-CCYYMM
      *                          QDS1-WBEG-DATE-CCYYMM
      *                          QDS1-WEND-DATE-CCYYMM.
      *    MOVE DS-PATH-OWNBR TO DS-BRANCH
      *                          QDS1-WBEG-BRANCH
      *                          QDS1-WEND-BRANCH.
      *    MOVE 0             TO DS-DLNO DS-CLASS
      *                          QDS1-WBEG-NUM
      *                          QDS1-WBEG-CLASS.
      *    MOVE ALL "9"       TO QDS1-WEND-NUM
      *                          QDS1-WEND-CLASS.

      *    PERFORM START-DS1-FILE. *> SSP - DSFILE

      *READ-DEALER-STAT-NEXT.
      *    PERFORM READ-DS1-FILE-NEXT. *> EMBEDDED - DSFILE
      *    IF IO-BAD OR (DS-BRANCH NOT = DS-PATH-OWNBR)
      *       PERFORM CLOSE-DS1-FILE
      *       GO TO UPDATE-DS-FILE-EXIT.

      *    IF DS-DATE-CCYYMM > HOLD-DS-DATE-CCYYMM
      *       PERFORM CLOSE-DS1-FILE
      *       GO TO UPDATE-DS-FILE-EXIT.

      *    IF DS-BRANCH > BR-NO
      *       PERFORM CLOSE-DS1-FILE
      *       GO TO UPDATE-DS-FILE-EXIT.

      *    PERFORM UPDATE-DS-REPO-CLR VARYING SUB FROM 1 BY 1
      *             UNTIL SUB > 5.

      * MOVE CUR-NET TO ELY-NET AND MOVE YTD-VOL TO LYTD-VOL
      * CLEAR YTD-VOL
      *    PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 10
      *      MOVE DS-UNEARNED-AMT(1 1 SUB) TO DS-UNEARNED-AMT(3 1 SUB)
      *      MOVE DS-UNEARNED-AMT(2 2 SUB) TO DS-UNEARNED-AMT(3 2 SUB)
      *      MOVE 0 TO DS-UNEARNED-AMT(2 2 SUB)
      *    END-PERFORM.

      *    MOVE DS-OUTSUNIT(1) TO DS-OUTSUNIT(3).
      *    MOVE DS-PLOSUNIT(1) TO DS-PLOSUNIT(3).
      *    MOVE DS-RCPLUNIT(1) TO DS-RCPLUNIT(3).
      *    MOVE DS-JDOSUNIT(1) TO DS-JDOSUNIT(3).
      *    MOVE DS-NPURUNIT(2) TO DS-NPURUNIT(3).
      *    MOVE DS-OPURUNIT(2) TO DS-OPURUNIT(3).
      *    MOVE DS-RNEWUNIT(2) TO DS-RNEWUNIT(3).
      *    MOVE DS-PAIDUNIT(2) TO DS-PAIDUNIT(3).
      *    MOVE DS-TODLUNIT(2) TO DS-TODLUNIT(3).

      *    MOVE DS-OUTSAMT(1)  TO DS-OUTSAMT(3).
      *    MOVE DS-PLOSAMT(1)  TO DS-PLOSAMT(3).
      *    MOVE DS-RCPLAMT(1)  TO DS-RCPLAMT(3).
      *    MOVE DS-JDOSAMT(1)  TO DS-JDOSAMT(3).
      *    MOVE DS-NPURAMT(2) TO DS-NPURAMT(3).
      *    MOVE DS-OPURAMT(2) TO DS-OPURAMT(3).
      *    MOVE DS-RNEWAMT(2) TO DS-RNEWAMT(3).
      *    MOVE DS-PAIDAMT(2) TO DS-PAIDAMT(3).
      *    MOVE DS-TODLAMT(2) TO DS-TODLAMT(3).

      *    MOVE 0 TO DS-NPURAMT(2) DS-NPURUNIT(2).
      *    MOVE 0 TO DS-OPURAMT(2) DS-OPURUNIT(2).
      *    MOVE 0 TO DS-RNEWAMT(2) DS-RNEWUNIT(2).
      *    MOVE 0 TO DS-PAIDAMT(2) DS-PAIDUNIT(2).
      *    MOVE 0 TO DS-TODLAMT(2) DS-TODLUNIT(2).

      * SET DS-DLDISC-VOL(LYR) = DS-DLDISC-VOL(YTD)
      *    MOVE DS-DLDISC-VOL(2) TO DS-DLDISC-VOL(3).
      *    MOVE 0 TO DS-DLDISC-VOL(2).

      * CLEAR PROCEEDS YTD.
      *    MOVE 0 TO DS-PROCEEDS(2).

      *    MOVE "Y" TO DS-FOUND-FG.
      *    PERFORM REWRITE-DS1-FILE. *> EMBEDDED - DSFILE
      *    GO TO READ-DEALER-STAT-NEXT.

      *----------------------------------------------------------------
      *UPDATE-DS-REPO-CLR.
      *    MOVE DS-NO-REPO(1 1 SUB) TO DS-NO-REPO(3 1 SUB).
      *    MOVE DS-NO-REPO(2 2 SUB) TO DS-NO-REPO(3 2 SUB).
      *    MOVE DS-AMT-REPO(1 1 SUB) TO DS-AMT-REPO(3 1 SUB).
      *    MOVE DS-AMT-REPO(2 2 SUB) TO DS-AMT-REPO(3 2 SUB).
      *    MOVE 0 TO DS-NO-REPO(2 2 SUB)
      *              DS-AMT-REPO(2 2 SUB).
 
      *UPDATE-DS-FILE-EXIT.
      *    EXIT.

      *****************************************************************
       UPDATE-LN-FILE SECTION.
      *****************************************************************
           PERFORM OPEN-LN1-FILE.

           MOVE "UPDATING LOAN INTEREST ............&&&&" TO MESS-BUF.
           INSPECT MESS-BUF REPLACING FIRST "&&&&" BY BR-NO.
           MOVE MESS-BUF TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE LN-PATH-OWNBR TO LN-OWNBR.
           MOVE 0             TO LN-ACCTNO.

      *    PERFORM START-LN1-FILE. *> SSP - LNFILE
      *READ-LOAN-NEXT.
      *    PERFORM READ-LN1-FILE-NEXT. *> EMBEDDED - LNFILE
      *    IF IO-BAD OR (LN-OWNBR NOT = LN-PATH-OWNBR)
      *       PERFORM CLOSE-LN1-FILE
      *       GO TO UPDATE-LN-FILE-EXIT.
      *
      *    IF ( LN-POCD-NONLOAN )
      *       GO TO READ-LOAN-NEXT.
      *
      *    MOVE LN-YTDINTPD TO LN-LYRINTPD.
      *    MOVE 0 TO LN-YTDINTPD.

           PERFORM REWRITE-LN1-FILE. *> EMBEDDED - LNFILE
           MOVE "Y" TO LN-FOUND-FG.
      *    GO TO READ-LOAN-NEXT.

       UPDATE-LN-FILE-EXIT.
           EXIT.

      *****************************************************************
       UPDATE-BW-FILE SECTION.
      *****************************************************************
           PERFORM OPEN-BW1-FILE.

           MOVE "UPDATING BORROWER INTEREST ........&&&&" TO MESS-BUF.
           INSPECT MESS-BUF REPLACING FIRST "&&&&" BY BR-NO.
           MOVE MESS-BUF TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE BW-PATH-OWNBR TO BW-OWNBR.
           MOVE 0             TO BW-SSNO.       

      *    PERFORM START-BW1-FILE. *> SSP - BWFILE             
      *READ-BORROWER-NEXT.
      *    PERFORM READ-BW1-FILE-NEXT. *> EMBEDDED - BWFILE
      *    IF (IO-FG = 9) OR (BW-OWNBR NOT = BW-PATH-OWNBR) 
      *       PERFORM CLOSE-BW1-FILE
      *       GO TO UPDATE-BW-FILE-EXIT.
      *
      *    MOVE BW-INTYTD    TO BW-INTLYR.
      *    MOVE BW-RE-INTYTD TO BW-RE-INTLYR.
      *
      *    MOVE 0 TO BW-INTYTD BW-RE-INTYTD.

           PERFORM REWRITE-BW1-FILE. *> EMBEDDED - BWFILE
           MOVE "Y" TO BW-FOUND-FG.

      *    GO TO READ-BORROWER-NEXT.

       UPDATE-BW-FILE-EXIT.
           EXIT.

      *********************************************************
      *  MOVE ESCROW YTD STATS TO LAST YEAR & CLEAR YTD IF
      *  CALENDAR OR BOTH & GLOBAL OPTION FOR ESCROW IS "Y"
      *********************************************************
      *UPDATE-ES-FILE SECTION.
      *    IF GP-ESCROW-FG NOT = "Y"
      *       GO TO UPDATE-ES-FILE-EXIT.

      *    MOVE "UPDATING ESCROW FILE ........&&&&" TO MESS-BUF.
      *    INSPECT MESS-BUF REPLACING FIRST "&&&&" BY BR-NO.
      *    MOVE MESS-BUF TO WR-BUF.
      *    MOVE MESS-LIST TO WR-LIST.
      *    PERFORM CALL-WRFORM.

      *    PERFORM OPEN-ES1-FILE.
      *    MOVE ES-PATH-OWNBR TO ES-BRNO.
      *    MOVE 0             TO ES-ACCTNO.

      *    PERFORM START-ES1-FILE. *> SSP - ESFILE
      *NEXT-ESCROW.
      *    PERFORM READ-ES1-FILE-NEXT. *> EMBEDDED - ESFILE
      *    IF (IO-BAD) OR (ES-BRNO NOT = ES-PATH-OWNBR)
      *       PERFORM CLOSE-ES1-FILE
      *       GO TO UPDATE-ES-FILE-EXIT.

      *    MOVE ES-PREPAID(1)         TO ES-PREPAID(2).
      *    MOVE ES-COLLECTED(1)       TO ES-COLLECTED(2).
      *    MOVE ES-DISBURSED-TAXES(1) TO ES-DISBURSED-TAXES(2).
      *    MOVE ES-DISBURSED-INS(1)   TO ES-DISBURSED-INS(2).
      *    MOVE ES-DISBURSED-PMI(1)   TO ES-DISBURSED-PMI(2).
      *    MOVE ES-DISBURSED-OTHER(1) TO ES-DISBURSED-OTHER(2).
      *    MOVE ES-DISBURSED-REF(1)   TO ES-DISBURSED-REF(2).
      *    MOVE ES-DISBURSED-CNT-TAXES(1) TO
      *                                  ES-DISBURSED-CNT-TAXES(2).
      *    MOVE 0 TO ES-PREPAID(1)
      *              ES-COLLECTED(1)
      *              ES-DISBURSED-TAXES(1)
      *              ES-DISBURSED-INS(1)
      *              ES-DISBURSED-PMI(1)
      *              ES-DISBURSED-OTHER(1)
      *              ES-DISBURSED-REF(1)
      *              ES-DISBURSED-CNT-TAXES(1).
      *    PERFORM REWRITE-ES1-FILE. *> EMBEDDED - ESFILE
      *    MOVE "Y" TO ES-FOUND-FG.
      *    GO TO NEXT-ESCROW.
      *UPDATE-ES-FILE-EXIT.
      *    EXIT.

      *****************************************************************
       UPDATE-BY-FILE SECTION.
      *****************************************************************
           PERFORM OPEN-BY1-FILE.

           MOVE "CLEARING BRANCH STATISTICS EOY ....&&&&" TO MESS-BUF.
           INSPECT MESS-BUF REPLACING FIRST "&&&&" BY BR-NO.
           MOVE MESS-BUF TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      * SETUP FIRST DAY OF THE NEXT MONTH:
           MOVE YEAR-BUF TO DATE-MMDDYY-YY.
           MOVE MM-BUF   TO DATE-MMDDYY-MM.
           MOVE 01       TO DATE-MMDDYY-DD.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.

           MOVE DATE-YYYYMMDD TO NDTE-DATE.
           MOVE 31            TO NDTE-DD.
           PERFORM NEW-DATE-CALCULATION.

           MOVE 1 TO NDTE-HOLD.
           PERFORM INCREMENT-MONTHS.

      * SETUP BY-DATE-CCYYMM, YYYYMM
           MOVE NDTE-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-YYYYMM.

      * START BY-FILE WITH NEXT MONTHS KEY, TO CLEAR EOY
           MOVE DATE-YYYYMM TO BY-DATE-CCYYMM NEXT-MO-BY-DATE-CCYYMM
                               QBY1-WBEG-DATE-CCYYMM
                               QBY1-WEND-DATE-CCYYMM.

           MOVE BR-NO       TO BY-BRNO
                               QBY1-WBEG-BRNO
                               QBY1-WEND-BRNO.
           MOVE 0           TO BY-CLASS
                               QBY1-WBEG-CLASS.
           MOVE ALL "9"     TO QBY1-WEND-CLASS.

           PERFORM START-BY1-FILE. *> SSP - BYFILE

       UPDATE-BY-FILE-NEXT.
           PERFORM READ-BY1-FILE-NEXT. *> EMBEDDED - BYFILE
           IF IO-FG = 9
              PERFORM CLOSE-BY1-FILE
              GO TO UPDATE-BY-FILE-EXIT.

           IF BY-DATE-CCYYMM > NEXT-MO-BY-DATE-CCYYMM
              PERFORM CLOSE-BY1-FILE
              GO TO UPDATE-BY-FILE-EXIT.

           IF BY-BRNO > BR-NO
              PERFORM CLOSE-BY1-FILE
              GO TO UPDATE-BY-FILE-EXIT.

      * ONLY CLEAR THE NEXT MONTHS YTD FIGURES, WHICH WAS CREATED BY EOM
           PERFORM UPDATE-BY-PL-COLLECTED
              VARYING SUB FROM 1 BY 1 UNTIL SUB > 5.

           MOVE 0 TO BY-NO-SOLD(2)    BY-AMT-SOLD(2)
                     BY-NO-TRANS(2)   BY-AMT-TRANS(2)
                     BY-NO-PURCH(2)   BY-AMT-PURCH(2) BY-INTREB(2)
                     BY-CLREB1(2)     BY-CLREB2(2)    BY-AHREB(2)
                     BY-PPREB(2)      BY-OTHCOL(2)    BY-OT2COL(2)
                     BY-INTCOL(2)     BY-LATECOL(2)   BY-RNPRIN(2)
                     BY-O1REB(2)      BY-O2REB(2)     BY-O3REB(2)
                     BY-O4REB(2)      BY-O5REB(2)     BY-O6REB(2)
                     BY-O7REB(2)      BY-PRINCOL(2)   BY-DEFCOL(2)
                     BY-CLREB3(2)     BY-CLREB4(2)    BY-SCREB(2)
                     BY-MFREB(2)      BY-DFREB(2)     BY-PART-CHGOFF(2)
                     BY-ALLOTMENTS(2) BY-LOCKBOX(2)   BY-PROCEEDS(2).

      * UPDATE / CLEAR BY INSURANCE TYPE TABLE.
           PERFORM UPDATE-BY-FILE-INS-CLR
                 VARYING SUB FROM 1 BY 1
                    UNTIL SUB > 10.

           PERFORM UPDATE-BY-FILE-FEE-CLR
                 VARYING SUB FROM 1 BY 1
                    UNTIL SUB > 3.

           MOVE 0 TO BY-OTH-NO-COL(2)
                     BY-OT2-NO-COL(2)
                     BY-INT-NO-COL(2)
                     BY-LATE-NO-COL(2).
           MOVE 0 TO BY-OTH-NO-WAIVED(2)
                     BY-OT2-NO-WAIVED(2)
                     BY-INT-NO-WAIVED(2)
                     BY-LATE-NO-WAIVED(2).
           MOVE 0 TO BY-OTH-AMT-WAIVED(2)
                     BY-OT2-AMT-WAIVED(2)
                     BY-INT-AMT-WAIVED(2)
                     BY-LATE-AMT-WAIVED(2).

      *----------------------------------------------------------------
      * UPDATE / CLEAR PERIOD TABLE
      *----------------------------------------------------------------
           PERFORM UPDATE-BY-FILE-CLR
                 VARYING SUB FROM 1 BY 1
                    UNTIL SUB > 11.

      *----------------------------------------------------------------
      * UPDATE / CLEAR REPO TABLE
      *----------------------------------------------------------------
           PERFORM UPDATE-BY-REPO-CLR
                 VARYING SUB FROM 1 BY 1
                    UNTIL SUB > 5.

      *----------------------------------------------------------------
      * UPDATE / CLEAR PL TABLE
      *----------------------------------------------------------------
           PERFORM UPDATE-BY-PL-CLR
                 VARYING SUB FROM 1 BY 1
                    UNTIL SUB > 10.

      *    -------------------------------------------------------------
      *    MOVE CUR-NET TO ELY-NET AND MOVE MTD-VOL TO LYT-VOL
      *    CLEAR YTD-VOL
      *    -------------------------------------------------------------
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 10
              MOVE BY-UNEARNED-AMT(1 1 SUB) TO BY-UNEARNED-AMT(3 1 SUB)
              MOVE BY-UNEARNED-AMT(2 2 SUB) TO BY-UNEARNED-AMT(3 2 SUB)
              MOVE 0 TO BY-UNEARNED-AMT(2 2 SUB)
           END-PERFORM.

      *----------------------------------------------------------------
      * UPDATE / CLEAR DLR TABLE
      *----------------------------------------------------------------
           PERFORM BY-BRANCH-FILE-DLR-CLR
              VARYING SUB FROM  1 BY 1
                 UNTIL SUB > 3.
           MOVE BY-DLPARVHELD(1 1) TO BY-DLPARVHELD(3 1).
           MOVE BY-DLPARVHELD(2 2) TO BY-DLPARVHELD(3 2).
           MOVE 0 TO BY-DLPARVHELD(2 2).
           MOVE BY-DLDISC(2) TO BY-DLDISC(3).
           MOVE 0 TO BY-DLDISC(2).

           MOVE "Y" TO BY-FOUND-FG.
           PERFORM REWRITE-BY1-FILE. *> EMBEDDED - BYFILE
           GO TO UPDATE-BY-FILE-NEXT.

      *----------------------------------------------------------------
       UPDATE-BY-PL-COLLECTED.
           MOVE 0 TO BY-PL-PRINCOL(2 SUB)
                     BY-PL-INTCOL(2 SUB)
                     BY-PL-LATECOL(2 SUB)
                     BY-PL-O2COL(2 SUB).

       UPDATE-BY-FILE-CLR.
           MOVE BY-NO(1 1 SUB) TO BY-NO(3 1 SUB).
           MOVE BY-NO(2 2 SUB) TO BY-NO(3 2 SUB).
           MOVE BY-AMT(1 1 SUB) TO BY-AMT(3 1 SUB).
           MOVE BY-AMT(2 2 SUB) TO BY-AMT(3 2 SUB).
           MOVE BY-O2(1 1 SUB) TO BY-O2(3 1 SUB).
           MOVE BY-O2(2 2 SUB) TO BY-O2(3 2 SUB).
           MOVE BY-INT(1 1 SUB) TO BY-INT(3 1 SUB).
           MOVE BY-INT(2 2 SUB) TO BY-INT(3 2 SUB).
           MOVE 0 TO BY-NO(2 2 SUB)
                     BY-AMT(2 2 SUB)
                     BY-INT(2 2 SUB)
                     BY-O2(2 2 SUB).

       UPDATE-BY-REPO-CLR.
           MOVE BY-NO-REPO(1 1 SUB) TO BY-NO-REPO(3 1 SUB).
           MOVE BY-NO-REPO(2 2 SUB) TO BY-NO-REPO(3 2 SUB).
           MOVE BY-AMT-REPO(1 1 SUB) TO BY-AMT-REPO(3 1 SUB).
           MOVE BY-AMT-REPO(2 2 SUB) TO BY-AMT-REPO(3 2 SUB).
           MOVE BY-O2-REPO(1 1 SUB) TO BY-O2-REPO(3 1 SUB).
           MOVE BY-O2-REPO(2 2 SUB) TO BY-O2-REPO(3 2 SUB).
           MOVE BY-INT-REPO(1 1 SUB) TO BY-INT-REPO(3 1 SUB).
           MOVE BY-INT-REPO(2 2 SUB) TO BY-INT-REPO(3 2 SUB).
           MOVE 0 TO BY-NO-REPO(2 2 SUB)
                     BY-AMT-REPO(2 2 SUB)
                     BY-O2-REPO(2 2 SUB)
                     BY-INT-REPO(2 2 SUB).

       UPDATE-BY-PL-CLR.
           MOVE BY-NO-PL(1 1 SUB) TO BY-NO-PL(3 1 SUB).
           MOVE BY-NO-PL(2 2 SUB) TO BY-NO-PL(3 2 SUB).
           MOVE BY-AMT-PL(1 1 SUB) TO BY-AMT-PL(3 1 SUB).
           MOVE BY-AMT-PL(2 2 SUB) TO BY-AMT-PL(3 2 SUB).
           MOVE BY-O2-PL(1 1 SUB) TO BY-O2-PL(3 1 SUB).
           MOVE BY-O2-PL(2 2 SUB) TO BY-O2-PL(3 2 SUB).
           MOVE BY-INT-PL(1 1 SUB) TO BY-INT-PL(3 1 SUB).
           MOVE BY-INT-PL(2 2 SUB) TO BY-INT-PL(3 2 SUB).
           MOVE 0 TO BY-NO-PL(2 2 SUB)
                     BY-AMT-PL(2 2 SUB)
                     BY-O2-PL(2 2 SUB)
                     BY-INT-PL(2 2 SUB).

       BY-BRANCH-FILE-DLR-CLR.
           MOVE BY-DLRVPRIN(1 1 SUB) TO BY-DLRVPRIN(3 1 SUB).
           MOVE BY-DLRVPRIN(2 2 SUB) TO BY-DLRVPRIN(3 2 SUB).
           MOVE BY-DLRVCHGS(1 1 SUB) TO BY-DLRVCHGS(3 1 SUB).
           MOVE BY-DLRVCHGS(2 2 SUB) TO BY-DLRVCHGS(3 2 SUB).
           MOVE 0 TO BY-DLRVPRIN(2 2 SUB)
                     BY-DLRVCHGS(2 2 SUB).

       UPDATE-BY-FILE-INS-CLR.
           MOVE 0 TO BY-INS-NO-ELIG(2 SUB)
                     BY-INS-NO-WROTE(2 SUB)
                     BY-INS-NO-NOTWROTE(2 SUB)
                     BY-INS-AMT-WROTE(2 SUB).

       UPDATE-BY-FILE-FEE-CLR.
           MOVE 0 TO BY-FEE-NO-ELIG(2 SUB)
                     BY-FEE-NO-WROTE(2 SUB)
                     BY-FEE-NO-NOTWROTE(2 SUB)
                     BY-FEE-AMT-WROTE(2 SUB).

       UPDATE-BY-FILE-EXIT.
           EXIT.

      *****************************************************************
      *    LOG ERROR IN GLOBAL LG DIRECTORY
      *      LOG FILE SAVED AS EOY.YYMMDD.
      *****************************************************************
       LOG-ERROR SECTION.
           MOVE "Y" TO ERRLOGW-ACTION.
           MOVE BR-NO TO ERRLOGW-BRNO.
           MOVE FORM-PATHNAME TO ERRLOGW-PROGX.
           MOVE "CL/ERRLOG" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PROGX.

      *----------------------------------------------------------------
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GPENV.CPY".

      *********************************************************
      *FORM-RESET SECTION.
      *********************************************************
      *    DISPLAY EOYUPD-SCREEN UPON EOYUPD-WINDOW-HANDLE.
      *    MOVE EOYUPD-FORM-FIELDS TO WR-BUF. 
      *    MOVE EOYUPD-FORM-LIST TO WR-LIST.
      *    PERFORM CALL-WRFORM.

      *********************************************************
       SETUP-LINK-INFO SECTION.
      *********************************************************
           MOVE EOYUPD-HELP--FILE TO HELP-LINK-FILE.
           MOVE EOYUPD-HELP--DIR TO HELP-LINK-DIR.

      *********************************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/EOYUPD_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *****************************************************************
       MISCELLANIOUS-PARAGRAPHS SECTION.
      *****************************************************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".

      *****************************************************************
       IO-ROUTINE SECTION.
      *****************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLRP_SQL.CPY".
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-LN1-CONNECTION.
      *    PERFORM CLOSE-ES1-FILE. 
      *    PERFORM CLOSE-ES1-CONNECTION.
      *    PERFORM CLOSE-DS1-FILE.
      *    PERFORM CLOSE-DS1-CONNECTION.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-BW1-CONNECTION.
           PERFORM CLOSE-BY1-FILE.
           PERFORM CLOSE-BY1-CONNECTION.
           PERFORM CLOSE-BR-FILE.
           PERFORM SQL-CLOSE-UPDATE-CONNECTION.
       COPY "LIBLP/LPLNGS_SQL.CPY".
      *COPY "LIBLP/LPESGS_SQL.CPY".
      *COPY "LIBLP/LPDSGS_SQL.CPY".
       COPY "LIBLP/LPBYGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".

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
           PERFORM START-BYA1-FILE. *> SSP - BYAFILE
           IF ( IO-FG NOT = 0 )
              MOVE "A" TO IO-MULTI-FG.
             
      *-----------------------------------------------------------------
       READ-BY1-FILE.
           PERFORM SET-BY1-TO-ALL.
           PERFORM READ-BYA1-FILE.
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

           IF BY1-CONN-OPEN-SW = "N" 
              MOVE "Y"         TO BY1-CONN-OPEN-SW
              EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS BY1_CONNECT 
                       USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
              END-EXEC.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       START-BYA1-FILE. *> SSP - BYAFILE
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

      *  STORED PROCEDURE:
      *
      *  SELECT BYAFILE.BYA_DATE_CCYYMM,
      *         BYAFILE.BYA_BRNO,
      *         BYAFILE.BYA_CLASS,
      *         CAST(BYAFILE.BYA_LTOUCH_DATE AS VARCHAR(10)) AS BYA_LTOUCH_DATE,
      *         BYAFILE.BYA_NO_1_1_1,
      *         BYAFILE.BYA_AMT_1_1_1,
      *         BYAFILE.BYA_O2_1_1_1,
      *         BYAFILE.BYA_INT_1_1_1,
      *         BYAFILE.BYA_NO_1_1_2,
      *         BYAFILE.BYA_AMT_1_1_2,
      *         BYAFILE.BYA_O2_1_1_2,
      *         BYAFILE.BYA_INT_1_1_2,
      *         BYAFILE.BYA_NO_1_1_3,
      *         BYAFILE.BYA_AMT_1_1_3,
      *         BYAFILE.BYA_O2_1_1_3,
      *         BYAFILE.BYA_INT_1_1_3,
      *         BYAFILE.BYA_NO_1_1_4,
      *         BYAFILE.BYA_AMT_1_1_4,
      *         BYAFILE.BYA_O2_1_1_4,
      *         BYAFILE.BYA_INT_1_1_4,
      *         BYAFILE.BYA_NO_1_1_5,
      *         BYAFILE.BYA_AMT_1_1_5,
      *         BYAFILE.BYA_O2_1_1_5,
      *         BYAFILE.BYA_INT_1_1_5,
      *         BYAFILE.BYA_NO_1_1_6,
      *         BYAFILE.BYA_AMT_1_1_6,
      *         BYAFILE.BYA_O2_1_1_6,
      *         BYAFILE.BYA_INT_1_1_6,
      *         BYAFILE.BYA_NO_1_1_7,
      *         BYAFILE.BYA_AMT_1_1_7,
      *         BYAFILE.BYA_O2_1_1_7,
      *         BYAFILE.BYA_INT_1_1_7,
      *         BYAFILE.BYA_NO_1_1_8,
      *         BYAFILE.BYA_AMT_1_1_8,
      *         BYAFILE.BYA_O2_1_1_8,
      *         BYAFILE.BYA_INT_1_1_8,
      *         BYAFILE.BYA_NO_1_1_9,
      *         BYAFILE.BYA_AMT_1_1_9,
      *         BYAFILE.BYA_O2_1_1_9,
      *         BYAFILE.BYA_INT_1_1_9,
      *         BYAFILE.BYA_NO_1_1_10,
      *         BYAFILE.BYA_AMT_1_1_10,
      *         BYAFILE.BYA_O2_1_1_10,
      *         BYAFILE.BYA_INT_1_1_10,
      *         BYAFILE.BYA_NO_1_1_11,
      *         BYAFILE.BYA_AMT_1_1_11,
      *         BYAFILE.BYA_O2_1_1_11,
      *         BYAFILE.BYA_INT_1_1_11,
      *         BYAFILE.BYA_NO_1_2_1,
      *         BYAFILE.BYA_AMT_1_2_1,
      *         BYAFILE.BYA_O2_1_2_1,
      *         BYAFILE.BYA_INT_1_2_1,
      *         BYAFILE.BYA_NO_1_2_2,
      *         BYAFILE.BYA_AMT_1_2_2,
      *         BYAFILE.BYA_O2_1_2_2,
      *         BYAFILE.BYA_INT_1_2_2,
      *         BYAFILE.BYA_NO_1_2_3,
      *         BYAFILE.BYA_AMT_1_2_3,
      *         BYAFILE.BYA_O2_1_2_3,
      *         BYAFILE.BYA_INT_1_2_3,
      *         BYAFILE.BYA_NO_1_2_4,
      *         BYAFILE.BYA_AMT_1_2_4,
      *         BYAFILE.BYA_O2_1_2_4,
      *         BYAFILE.BYA_INT_1_2_4,
      *         BYAFILE.BYA_NO_1_2_5,
      *         BYAFILE.BYA_AMT_1_2_5,
      *         BYAFILE.BYA_O2_1_2_5,
      *         BYAFILE.BYA_INT_1_2_5,
      *         BYAFILE.BYA_NO_1_2_6,
      *         BYAFILE.BYA_AMT_1_2_6,
      *         BYAFILE.BYA_O2_1_2_6,
      *         BYAFILE.BYA_INT_1_2_6,
      *         BYAFILE.BYA_NO_1_2_7,
      *         BYAFILE.BYA_AMT_1_2_7,
      *         BYAFILE.BYA_O2_1_2_7,
      *         BYAFILE.BYA_INT_1_2_7,
      *         BYAFILE.BYA_NO_1_2_8,
      *         BYAFILE.BYA_AMT_1_2_8,
      *         BYAFILE.BYA_O2_1_2_8,
      *         BYAFILE.BYA_INT_1_2_8,
      *         BYAFILE.BYA_NO_1_2_9,
      *         BYAFILE.BYA_AMT_1_2_9,
      *         BYAFILE.BYA_O2_1_2_9,
      *         BYAFILE.BYA_INT_1_2_9,
      *         BYAFILE.BYA_NO_1_2_10,
      *         BYAFILE.BYA_AMT_1_2_10,
      *         BYAFILE.BYA_O2_1_2_10,
      *         BYAFILE.BYA_INT_1_2_10,
      *         BYAFILE.BYA_NO_1_2_11,
      *         BYAFILE.BYA_AMT_1_2_11,
      *         BYAFILE.BYA_O2_1_2_11,
      *         BYAFILE.BYA_INT_1_2_11,
      *         BYAFILE.BYA_NO_2_1_1,
      *         BYAFILE.BYA_AMT_2_1_1,
      *         BYAFILE.BYA_O2_2_1_1,
      *         BYAFILE.BYA_INT_2_1_1,
      *         BYAFILE.BYA_NO_2_1_2,
      *         BYAFILE.BYA_AMT_2_1_2,
      *         BYAFILE.BYA_O2_2_1_2,
      *         BYAFILE.BYA_INT_2_1_2,
      *         BYAFILE.BYA_NO_2_1_3,
      *         BYAFILE.BYA_AMT_2_1_3,
      *         BYAFILE.BYA_O2_2_1_3,
      *         BYAFILE.BYA_INT_2_1_3,
      *         BYAFILE.BYA_NO_2_1_4,
      *         BYAFILE.BYA_AMT_2_1_4,
      *         BYAFILE.BYA_O2_2_1_4,
      *         BYAFILE.BYA_INT_2_1_4,
      *         BYAFILE.BYA_NO_2_1_5,
      *         BYAFILE.BYA_AMT_2_1_5,
      *         BYAFILE.BYA_O2_2_1_5,
      *         BYAFILE.BYA_INT_2_1_5,
      *         BYAFILE.BYA_NO_2_1_6,
      *         BYAFILE.BYA_AMT_2_1_6,
      *         BYAFILE.BYA_O2_2_1_6,
      *         BYAFILE.BYA_INT_2_1_6,
      *         BYAFILE.BYA_NO_2_1_7,
      *         BYAFILE.BYA_AMT_2_1_7,
      *         BYAFILE.BYA_O2_2_1_7,
      *         BYAFILE.BYA_INT_2_1_7,
      *         BYAFILE.BYA_NO_2_1_8,
      *         BYAFILE.BYA_AMT_2_1_8,
      *         BYAFILE.BYA_O2_2_1_8,
      *         BYAFILE.BYA_INT_2_1_8,
      *         BYAFILE.BYA_NO_2_1_9,
      *         BYAFILE.BYA_AMT_2_1_9,
      *         BYAFILE.BYA_O2_2_1_9,
      *         BYAFILE.BYA_INT_2_1_9,
      *         BYAFILE.BYA_NO_2_1_10,
      *         BYAFILE.BYA_AMT_2_1_10,
      *         BYAFILE.BYA_O2_2_1_10,
      *         BYAFILE.BYA_INT_2_1_10,
      *         BYAFILE.BYA_NO_2_1_11,
      *         BYAFILE.BYA_AMT_2_1_11,
      *         BYAFILE.BYA_O2_2_1_11,
      *         BYAFILE.BYA_INT_2_1_11,
      *         BYAFILE.BYA_NO_2_2_1,
      *         BYAFILE.BYA_AMT_2_2_1,
      *         BYAFILE.BYA_O2_2_2_1,
      *         BYAFILE.BYA_INT_2_2_1,
      *         BYAFILE.BYA_NO_2_2_2,
      *         BYAFILE.BYA_AMT_2_2_2,
      *         BYAFILE.BYA_O2_2_2_2,
      *         BYAFILE.BYA_INT_2_2_2,
      *         BYAFILE.BYA_NO_2_2_3,
      *         BYAFILE.BYA_AMT_2_2_3,
      *         BYAFILE.BYA_O2_2_2_3,
      *         BYAFILE.BYA_INT_2_2_3,
      *         BYAFILE.BYA_NO_2_2_4,
      *         BYAFILE.BYA_AMT_2_2_4,
      *         BYAFILE.BYA_O2_2_2_4,
      *         BYAFILE.BYA_INT_2_2_4,
      *         BYAFILE.BYA_NO_2_2_5,
      *         BYAFILE.BYA_AMT_2_2_5,
      *         BYAFILE.BYA_O2_2_2_5,
      *         BYAFILE.BYA_INT_2_2_5,
      *         BYAFILE.BYA_NO_2_2_6,
      *         BYAFILE.BYA_AMT_2_2_6,
      *         BYAFILE.BYA_O2_2_2_6,
      *         BYAFILE.BYA_INT_2_2_6,
      *         BYAFILE.BYA_NO_2_2_7,
      *         BYAFILE.BYA_AMT_2_2_7,
      *         BYAFILE.BYA_O2_2_2_7,
      *         BYAFILE.BYA_INT_2_2_7,
      *         BYAFILE.BYA_NO_2_2_8,
      *         BYAFILE.BYA_AMT_2_2_8,
      *         BYAFILE.BYA_O2_2_2_8,
      *         BYAFILE.BYA_INT_2_2_8,
      *         BYAFILE.BYA_NO_2_2_9,
      *         BYAFILE.BYA_AMT_2_2_9,
      *         BYAFILE.BYA_O2_2_2_9,
      *         BYAFILE.BYA_INT_2_2_9,
      *         BYAFILE.BYA_NO_2_2_10,
      *         BYAFILE.BYA_AMT_2_2_10,
      *         BYAFILE.BYA_O2_2_2_10,
      *         BYAFILE.BYA_INT_2_2_10,
      *         BYAFILE.BYA_NO_2_2_11,
      *         BYAFILE.BYA_AMT_2_2_11,
      *         BYAFILE.BYA_O2_2_2_11,
      *         BYAFILE.BYA_INT_2_2_11,
      *         BYAFILE.BYA_NO_3_1_1,
      *         BYAFILE.BYA_AMT_3_1_1,
      *         BYAFILE.BYA_O2_3_1_1,
      *         BYAFILE.BYA_INT_3_1_1,
      *         BYAFILE.BYA_NO_3_1_2,
      *         BYAFILE.BYA_AMT_3_1_2,
      *         BYAFILE.BYA_O2_3_1_2,
      *         BYAFILE.BYA_INT_3_1_2,
      *         BYAFILE.BYA_NO_3_1_3,
      *         BYAFILE.BYA_AMT_3_1_3,
      *         BYAFILE.BYA_O2_3_1_3,
      *         BYAFILE.BYA_INT_3_1_3,
      *         BYAFILE.BYA_NO_3_1_4,
      *         BYAFILE.BYA_AMT_3_1_4,
      *         BYAFILE.BYA_O2_3_1_4,
      *         BYAFILE.BYA_INT_3_1_4,
      *         BYAFILE.BYA_NO_3_1_5,
      *         BYAFILE.BYA_AMT_3_1_5,
      *         BYAFILE.BYA_O2_3_1_5,
      *         BYAFILE.BYA_INT_3_1_5,
      *         BYAFILE.BYA_NO_3_1_6,
      *         BYAFILE.BYA_AMT_3_1_6,
      *         BYAFILE.BYA_O2_3_1_6,
      *         BYAFILE.BYA_INT_3_1_6,
      *         BYAFILE.BYA_NO_3_1_7,
      *         BYAFILE.BYA_AMT_3_1_7,
      *         BYAFILE.BYA_O2_3_1_7,
      *         BYAFILE.BYA_INT_3_1_7,
      *         BYAFILE.BYA_NO_3_1_8,
      *         BYAFILE.BYA_AMT_3_1_8,
      *         BYAFILE.BYA_O2_3_1_8,
      *         BYAFILE.BYA_INT_3_1_8,
      *         BYAFILE.BYA_NO_3_1_9,
      *         BYAFILE.BYA_AMT_3_1_9,
      *         BYAFILE.BYA_O2_3_1_9,
      *         BYAFILE.BYA_INT_3_1_9,
      *         BYAFILE.BYA_NO_3_1_10,
      *         BYAFILE.BYA_AMT_3_1_10,
      *         BYAFILE.BYA_O2_3_1_10,
      *         BYAFILE.BYA_INT_3_1_10,
      *         BYAFILE.BYA_NO_3_1_11,
      *         BYAFILE.BYA_AMT_3_1_11,
      *         BYAFILE.BYA_O2_3_1_11,
      *         BYAFILE.BYA_INT_3_1_11,
      *         BYAFILE.BYA_NO_3_2_1,
      *         BYAFILE.BYA_AMT_3_2_1,
      *         BYAFILE.BYA_O2_3_2_1,
      *         BYAFILE.BYA_INT_3_2_1,
      *         BYAFILE.BYA_NO_3_2_2,
      *         BYAFILE.BYA_AMT_3_2_2,
      *         BYAFILE.BYA_O2_3_2_2,
      *         BYAFILE.BYA_INT_3_2_2,
      *         BYAFILE.BYA_NO_3_2_3,
      *         BYAFILE.BYA_AMT_3_2_3,
      *         BYAFILE.BYA_O2_3_2_3,
      *         BYAFILE.BYA_INT_3_2_3,
      *         BYAFILE.BYA_NO_3_2_4,
      *         BYAFILE.BYA_AMT_3_2_4,
      *         BYAFILE.BYA_O2_3_2_4,
      *         BYAFILE.BYA_INT_3_2_4,
      *         BYAFILE.BYA_NO_3_2_5,
      *         BYAFILE.BYA_AMT_3_2_5,
      *         BYAFILE.BYA_O2_3_2_5,
      *         BYAFILE.BYA_INT_3_2_5,
      *         BYAFILE.BYA_NO_3_2_6,
      *         BYAFILE.BYA_AMT_3_2_6,
      *         BYAFILE.BYA_O2_3_2_6,
      *         BYAFILE.BYA_INT_3_2_6,
      *         BYAFILE.BYA_NO_3_2_7,
      *         BYAFILE.BYA_AMT_3_2_7,
      *         BYAFILE.BYA_O2_3_2_7,
      *         BYAFILE.BYA_INT_3_2_7,
      *         BYAFILE.BYA_NO_3_2_8,
      *         BYAFILE.BYA_AMT_3_2_8,
      *         BYAFILE.BYA_O2_3_2_8,
      *         BYAFILE.BYA_INT_3_2_8,
      *         BYAFILE.BYA_NO_3_2_9,
      *         BYAFILE.BYA_AMT_3_2_9,
      *         BYAFILE.BYA_O2_3_2_9,
      *         BYAFILE.BYA_INT_3_2_9,
      *         BYAFILE.BYA_NO_3_2_10,
      *         BYAFILE.BYA_AMT_3_2_10,
      *         BYAFILE.BYA_O2_3_2_10,
      *         BYAFILE.BYA_INT_3_2_10,
      *         BYAFILE.BYA_NO_3_2_11,
      *         BYAFILE.BYA_AMT_3_2_11,
      *         BYAFILE.BYA_O2_3_2_11,
      *         BYAFILE.BYA_INT_3_2_11,
      *         BYAFILE.BYA_NO_REPO_1_1_1,
      *         BYAFILE.BYA_AMT_REPO_1_1_1,
      *         BYAFILE.BYA_O2_REPO_1_1_1,
      *         BYAFILE.BYA_INT_REPO_1_1_1,
      *         BYAFILE.BYA_NO_REPO_1_1_2,
      *         BYAFILE.BYA_AMT_REPO_1_1_2,
      *         BYAFILE.BYA_O2_REPO_1_1_2,
      *         BYAFILE.BYA_INT_REPO_1_1_2,
      *         BYAFILE.BYA_NO_REPO_1_1_3,
      *         BYAFILE.BYA_AMT_REPO_1_1_3,
      *         BYAFILE.BYA_O2_REPO_1_1_3,
      *         BYAFILE.BYA_INT_REPO_1_1_3,
      *         BYAFILE.BYA_NO_REPO_1_1_4,
      *         BYAFILE.BYA_AMT_REPO_1_1_4,
      *         BYAFILE.BYA_O2_REPO_1_1_4,
      *         BYAFILE.BYA_INT_REPO_1_1_4,
      *         BYAFILE.BYA_NO_REPO_1_1_5,
      *         BYAFILE.BYA_AMT_REPO_1_1_5,
      *         BYAFILE.BYA_O2_REPO_1_1_5,
      *         BYAFILE.BYA_INT_REPO_1_1_5,
      *         BYAFILE.BYA_NO_REPO_1_2_1,
      *         BYAFILE.BYA_AMT_REPO_1_2_1,
      *         BYAFILE.BYA_O2_REPO_1_2_1,
      *         BYAFILE.BYA_INT_REPO_1_2_1,
      *         BYAFILE.BYA_NO_REPO_1_2_2,
      *         BYAFILE.BYA_AMT_REPO_1_2_2,
      *         BYAFILE.BYA_O2_REPO_1_2_2,
      *         BYAFILE.BYA_INT_REPO_1_2_2,
      *         BYAFILE.BYA_NO_REPO_1_2_3,
      *         BYAFILE.BYA_AMT_REPO_1_2_3,
      *         BYAFILE.BYA_O2_REPO_1_2_3,
      *         BYAFILE.BYA_INT_REPO_1_2_3,
      *         BYAFILE.BYA_NO_REPO_1_2_4,
      *         BYAFILE.BYA_AMT_REPO_1_2_4,
      *         BYAFILE.BYA_O2_REPO_1_2_4,
      *         BYAFILE.BYA_INT_REPO_1_2_4,
      *         BYAFILE.BYA_NO_REPO_1_2_5,
      *         BYAFILE.BYA_AMT_REPO_1_2_5,
      *         BYAFILE.BYA_O2_REPO_1_2_5,
      *         BYAFILE.BYA_INT_REPO_1_2_5,
      *         BYAFILE.BYA_NO_REPO_2_1_1,
      *         BYAFILE.BYA_AMT_REPO_2_1_1,
      *         BYAFILE.BYA_O2_REPO_2_1_1,
      *         BYAFILE.BYA_INT_REPO_2_1_1,
      *         BYAFILE.BYA_NO_REPO_2_1_2,
      *         BYAFILE.BYA_AMT_REPO_2_1_2,
      *         BYAFILE.BYA_O2_REPO_2_1_2,
      *         BYAFILE.BYA_INT_REPO_2_1_2,
      *         BYAFILE.BYA_NO_REPO_2_1_3,
      *         BYAFILE.BYA_AMT_REPO_2_1_3,
      *         BYAFILE.BYA_O2_REPO_2_1_3,
      *         BYAFILE.BYA_INT_REPO_2_1_3,
      *         BYAFILE.BYA_NO_REPO_2_1_4,
      *         BYAFILE.BYA_AMT_REPO_2_1_4,
      *         BYAFILE.BYA_O2_REPO_2_1_4,
      *         BYAFILE.BYA_INT_REPO_2_1_4,
      *         BYAFILE.BYA_NO_REPO_2_1_5,
      *         BYAFILE.BYA_AMT_REPO_2_1_5,
      *         BYAFILE.BYA_O2_REPO_2_1_5,
      *         BYAFILE.BYA_INT_REPO_2_1_5,
      *         BYAFILE.BYA_NO_REPO_2_2_1,
      *         BYAFILE.BYA_AMT_REPO_2_2_1,
      *         BYAFILE.BYA_O2_REPO_2_2_1,
      *         BYAFILE.BYA_INT_REPO_2_2_1,
      *         BYAFILE.BYA_NO_REPO_2_2_2,
      *         BYAFILE.BYA_AMT_REPO_2_2_2,
      *         BYAFILE.BYA_O2_REPO_2_2_2,
      *         BYAFILE.BYA_INT_REPO_2_2_2,
      *         BYAFILE.BYA_NO_REPO_2_2_3,
      *         BYAFILE.BYA_AMT_REPO_2_2_3,
      *         BYAFILE.BYA_O2_REPO_2_2_3,
      *         BYAFILE.BYA_INT_REPO_2_2_3,
      *         BYAFILE.BYA_NO_REPO_2_2_4,
      *         BYAFILE.BYA_AMT_REPO_2_2_4,
      *         BYAFILE.BYA_O2_REPO_2_2_4,
      *         BYAFILE.BYA_INT_REPO_2_2_4,
      *         BYAFILE.BYA_NO_REPO_2_2_5,
      *         BYAFILE.BYA_AMT_REPO_2_2_5,
      *         BYAFILE.BYA_O2_REPO_2_2_5,
      *         BYAFILE.BYA_INT_REPO_2_2_5,
      *         BYAFILE.BYA_NO_REPO_3_1_1,
      *         BYAFILE.BYA_AMT_REPO_3_1_1,
      *         BYAFILE.BYA_O2_REPO_3_1_1,
      *         BYAFILE.BYA_INT_REPO_3_1_1,
      *         BYAFILE.BYA_NO_REPO_3_1_2,
      *         BYAFILE.BYA_AMT_REPO_3_1_2,
      *         BYAFILE.BYA_O2_REPO_3_1_2,
      *         BYAFILE.BYA_INT_REPO_3_1_2,
      *         BYAFILE.BYA_NO_REPO_3_1_3,
      *         BYAFILE.BYA_AMT_REPO_3_1_3,
      *         BYAFILE.BYA_O2_REPO_3_1_3,
      *         BYAFILE.BYA_INT_REPO_3_1_3,
      *         BYAFILE.BYA_NO_REPO_3_1_4,
      *         BYAFILE.BYA_AMT_REPO_3_1_4,
      *         BYAFILE.BYA_O2_REPO_3_1_4,
      *         BYAFILE.BYA_INT_REPO_3_1_4,
      *         BYAFILE.BYA_NO_REPO_3_1_5,
      *         BYAFILE.BYA_AMT_REPO_3_1_5,
      *         BYAFILE.BYA_O2_REPO_3_1_5,
      *         BYAFILE.BYA_INT_REPO_3_1_5,
      *         BYAFILE.BYA_NO_REPO_3_2_1,
      *         BYAFILE.BYA_AMT_REPO_3_2_1,
      *         BYAFILE.BYA_O2_REPO_3_2_1,
      *         BYAFILE.BYA_INT_REPO_3_2_1,
      *         BYAFILE.BYA_NO_REPO_3_2_2,
      *         BYAFILE.BYA_AMT_REPO_3_2_2,
      *         BYAFILE.BYA_O2_REPO_3_2_2,
      *         BYAFILE.BYA_INT_REPO_3_2_2,
      *         BYAFILE.BYA_NO_REPO_3_2_3,
      *         BYAFILE.BYA_AMT_REPO_3_2_3,
      *         BYAFILE.BYA_O2_REPO_3_2_3,
      *         BYAFILE.BYA_INT_REPO_3_2_3,
      *         BYAFILE.BYA_NO_REPO_3_2_4,
      *         BYAFILE.BYA_AMT_REPO_3_2_4,
      *         BYAFILE.BYA_O2_REPO_3_2_4,
      *         BYAFILE.BYA_INT_REPO_3_2_4,
      *         BYAFILE.BYA_NO_REPO_3_2_5,
      *         BYAFILE.BYA_AMT_REPO_3_2_5,
      *         BYAFILE.BYA_O2_REPO_3_2_5,
      *         BYAFILE.BYA_INT_REPO_3_2_5,
      *         BYAFILE.BYA_NO_PL_1_1_1,
      *         BYAFILE.BYA_AMT_PL_1_1_1,
      *         BYAFILE.BYA_O2_PL_1_1_1,
      *         BYAFILE.BYA_INT_PL_1_1_1,
      *         BYAFILE.BYA_NO_PL_1_1_2,
      *         BYAFILE.BYA_AMT_PL_1_1_2,
      *         BYAFILE.BYA_O2_PL_1_1_2,
      *         BYAFILE.BYA_INT_PL_1_1_2,
      *         BYAFILE.BYA_NO_PL_1_1_3,
      *         BYAFILE.BYA_AMT_PL_1_1_3,
      *         BYAFILE.BYA_O2_PL_1_1_3,
      *         BYAFILE.BYA_INT_PL_1_1_3,
      *         BYAFILE.BYA_NO_PL_1_1_4,
      *         BYAFILE.BYA_AMT_PL_1_1_4,
      *         BYAFILE.BYA_O2_PL_1_1_4,
      *         BYAFILE.BYA_INT_PL_1_1_4,
      *         BYAFILE.BYA_NO_PL_1_1_5,
      *         BYAFILE.BYA_AMT_PL_1_1_5,
      *         BYAFILE.BYA_O2_PL_1_1_5,
      *         BYAFILE.BYA_INT_PL_1_1_5,
      *         BYAFILE.BYA_NO_PL_1_1_6,
      *         BYAFILE.BYA_AMT_PL_1_1_6,
      *         BYAFILE.BYA_O2_PL_1_1_6,
      *         BYAFILE.BYA_INT_PL_1_1_6,
      *         BYAFILE.BYA_NO_PL_1_1_7,
      *         BYAFILE.BYA_AMT_PL_1_1_7,
      *         BYAFILE.BYA_O2_PL_1_1_7,
      *         BYAFILE.BYA_INT_PL_1_1_7,
      *         BYAFILE.BYA_NO_PL_1_1_8,
      *         BYAFILE.BYA_AMT_PL_1_1_8,
      *         BYAFILE.BYA_O2_PL_1_1_8,
      *         BYAFILE.BYA_INT_PL_1_1_8,
      *         BYAFILE.BYA_NO_PL_1_1_9,
      *         BYAFILE.BYA_AMT_PL_1_1_9,
      *         BYAFILE.BYA_O2_PL_1_1_9,
      *         BYAFILE.BYA_INT_PL_1_1_9,
      *         BYAFILE.BYA_NO_PL_1_1_10,
      *         BYAFILE.BYA_AMT_PL_1_1_10,
      *         BYAFILE.BYA_O2_PL_1_1_10,
      *         BYAFILE.BYA_INT_PL_1_1_10,
      *         BYAFILE.BYA_NO_PL_1_2_1,
      *         BYAFILE.BYA_AMT_PL_1_2_1,
      *         BYAFILE.BYA_O2_PL_1_2_1,
      *         BYAFILE.BYA_INT_PL_1_2_1,
      *         BYAFILE.BYA_NO_PL_1_2_2,
      *         BYAFILE.BYA_AMT_PL_1_2_2,
      *         BYAFILE.BYA_O2_PL_1_2_2,
      *         BYAFILE.BYA_INT_PL_1_2_2,
      *         BYAFILE.BYA_NO_PL_1_2_3,
      *         BYAFILE.BYA_AMT_PL_1_2_3,
      *         BYAFILE.BYA_O2_PL_1_2_3,
      *         BYAFILE.BYA_INT_PL_1_2_3,
      *         BYAFILE.BYA_NO_PL_1_2_4,
      *         BYAFILE.BYA_AMT_PL_1_2_4,
      *         BYAFILE.BYA_O2_PL_1_2_4,
      *         BYAFILE.BYA_INT_PL_1_2_4,
      *         BYAFILE.BYA_NO_PL_1_2_5,
      *         BYAFILE.BYA_AMT_PL_1_2_5,
      *         BYAFILE.BYA_O2_PL_1_2_5,
      *         BYAFILE.BYA_INT_PL_1_2_5,
      *         BYAFILE.BYA_NO_PL_1_2_6,
      *         BYAFILE.BYA_AMT_PL_1_2_6,
      *         BYAFILE.BYA_O2_PL_1_2_6,
      *         BYAFILE.BYA_INT_PL_1_2_6,
      *         BYAFILE.BYA_NO_PL_1_2_7,
      *         BYAFILE.BYA_AMT_PL_1_2_7,
      *         BYAFILE.BYA_O2_PL_1_2_7,
      *         BYAFILE.BYA_INT_PL_1_2_7,
      *         BYAFILE.BYA_NO_PL_1_2_8,
      *         BYAFILE.BYA_AMT_PL_1_2_8,
      *         BYAFILE.BYA_O2_PL_1_2_8,
      *         BYAFILE.BYA_INT_PL_1_2_8,
      *         BYAFILE.BYA_NO_PL_1_2_9,
      *         BYAFILE.BYA_AMT_PL_1_2_9,
      *         BYAFILE.BYA_O2_PL_1_2_9,
      *         BYAFILE.BYA_INT_PL_1_2_9,
      *         BYAFILE.BYA_NO_PL_1_2_10,
      *         BYAFILE.BYA_AMT_PL_1_2_10,
      *         BYAFILE.BYA_O2_PL_1_2_10,
      *         BYAFILE.BYA_INT_PL_1_2_10,
      *         BYAFILE.BYA_NO_PL_2_1_1,
      *         BYAFILE.BYA_AMT_PL_2_1_1,
      *         BYAFILE.BYA_O2_PL_2_1_1,
      *         BYAFILE.BYA_INT_PL_2_1_1,
      *         BYAFILE.BYA_NO_PL_2_1_2,
      *         BYAFILE.BYA_AMT_PL_2_1_2,
      *         BYAFILE.BYA_O2_PL_2_1_2,
      *         BYAFILE.BYA_INT_PL_2_1_2,
      *         BYAFILE.BYA_NO_PL_2_1_3,
      *         BYAFILE.BYA_AMT_PL_2_1_3,
      *         BYAFILE.BYA_O2_PL_2_1_3,
      *         BYAFILE.BYA_INT_PL_2_1_3,
      *         BYAFILE.BYA_NO_PL_2_1_4,
      *         BYAFILE.BYA_AMT_PL_2_1_4,
      *         BYAFILE.BYA_O2_PL_2_1_4,
      *         BYAFILE.BYA_INT_PL_2_1_4,
      *         BYAFILE.BYA_NO_PL_2_1_5,
      *         BYAFILE.BYA_AMT_PL_2_1_5,
      *         BYAFILE.BYA_O2_PL_2_1_5,
      *         BYAFILE.BYA_INT_PL_2_1_5,
      *         BYAFILE.BYA_NO_PL_2_1_6,
      *         BYAFILE.BYA_AMT_PL_2_1_6,
      *         BYAFILE.BYA_O2_PL_2_1_6,
      *         BYAFILE.BYA_INT_PL_2_1_6,
      *         BYAFILE.BYA_NO_PL_2_1_7,
      *         BYAFILE.BYA_AMT_PL_2_1_7,
      *         BYAFILE.BYA_O2_PL_2_1_7,
      *         BYAFILE.BYA_INT_PL_2_1_7,
      *         BYAFILE.BYA_NO_PL_2_1_8,
      *         BYAFILE.BYA_AMT_PL_2_1_8,
      *         BYAFILE.BYA_O2_PL_2_1_8,
      *         BYAFILE.BYA_INT_PL_2_1_8,
      *         BYAFILE.BYA_NO_PL_2_1_9,
      *         BYAFILE.BYA_AMT_PL_2_1_9,
      *         BYAFILE.BYA_O2_PL_2_1_9,
      *         BYAFILE.BYA_INT_PL_2_1_9,
      *         BYAFILE.BYA_NO_PL_2_1_10,
      *         BYAFILE.BYA_AMT_PL_2_1_10,
      *         BYAFILE.BYA_O2_PL_2_1_10,
      *         BYAFILE.BYA_INT_PL_2_1_10,
      *         BYAFILE.BYA_NO_PL_2_2_1,
      *         BYAFILE.BYA_AMT_PL_2_2_1,
      *         BYAFILE.BYA_O2_PL_2_2_1,
      *         BYAFILE.BYA_INT_PL_2_2_1,
      *         BYAFILE.BYA_NO_PL_2_2_2,
      *         BYAFILE.BYA_AMT_PL_2_2_2,
      *         BYAFILE.BYA_O2_PL_2_2_2,
      *         BYAFILE.BYA_INT_PL_2_2_2,
      *         BYAFILE.BYA_NO_PL_2_2_3,
      *         BYAFILE.BYA_AMT_PL_2_2_3,
      *         BYAFILE.BYA_O2_PL_2_2_3,
      *         BYAFILE.BYA_INT_PL_2_2_3,
      *         BYAFILE.BYA_NO_PL_2_2_4,
      *         BYAFILE.BYA_AMT_PL_2_2_4,
      *         BYAFILE.BYA_O2_PL_2_2_4,
      *         BYAFILE.BYA_INT_PL_2_2_4,
      *         BYAFILE.BYA_NO_PL_2_2_5,
      *         BYAFILE.BYA_AMT_PL_2_2_5,
      *         BYAFILE.BYA_O2_PL_2_2_5,
      *         BYAFILE.BYA_INT_PL_2_2_5,
      *         BYAFILE.BYA_NO_PL_2_2_6,
      *         BYAFILE.BYA_AMT_PL_2_2_6,
      *         BYAFILE.BYA_O2_PL_2_2_6,
      *         BYAFILE.BYA_INT_PL_2_2_6,
      *         BYAFILE.BYA_NO_PL_2_2_7,
      *         BYAFILE.BYA_AMT_PL_2_2_7,
      *         BYAFILE.BYA_O2_PL_2_2_7,
      *         BYAFILE.BYA_INT_PL_2_2_7,
      *         BYAFILE.BYA_NO_PL_2_2_8,
      *         BYAFILE.BYA_AMT_PL_2_2_8,
      *         BYAFILE.BYA_O2_PL_2_2_8,
      *         BYAFILE.BYA_INT_PL_2_2_8,
      *         BYAFILE.BYA_NO_PL_2_2_9,
      *         BYAFILE.BYA_AMT_PL_2_2_9,
      *         BYAFILE.BYA_O2_PL_2_2_9,
      *         BYAFILE.BYA_INT_PL_2_2_9,
      *         BYAFILE.BYA_NO_PL_2_2_10,
      *         BYAFILE.BYA_AMT_PL_2_2_10,
      *         BYAFILE.BYA_O2_PL_2_2_10,
      *         BYAFILE.BYA_INT_PL_2_2_10,
      *         BYAFILE.BYA_NO_PL_3_1_1,
      *         BYAFILE.BYA_AMT_PL_3_1_1,
      *         BYAFILE.BYA_O2_PL_3_1_1,
      *         BYAFILE.BYA_INT_PL_3_1_1,
      *         BYAFILE.BYA_NO_PL_3_1_2,
      *         BYAFILE.BYA_AMT_PL_3_1_2,
      *         BYAFILE.BYA_O2_PL_3_1_2,
      *         BYAFILE.BYA_INT_PL_3_1_2,
      *         BYAFILE.BYA_NO_PL_3_1_3,
      *         BYAFILE.BYA_AMT_PL_3_1_3,
      *         BYAFILE.BYA_O2_PL_3_1_3,
      *         BYAFILE.BYA_INT_PL_3_1_3,
      *         BYAFILE.BYA_NO_PL_3_1_4,
      *         BYAFILE.BYA_AMT_PL_3_1_4,
      *         BYAFILE.BYA_O2_PL_3_1_4,
      *         BYAFILE.BYA_INT_PL_3_1_4,
      *         BYAFILE.BYA_NO_PL_3_1_5,
      *         BYAFILE.BYA_AMT_PL_3_1_5,
      *         BYAFILE.BYA_O2_PL_3_1_5,
      *         BYAFILE.BYA_INT_PL_3_1_5,
      *         BYAFILE.BYA_NO_PL_3_1_6,
      *         BYAFILE.BYA_AMT_PL_3_1_6,
      *         BYAFILE.BYA_O2_PL_3_1_6,
      *         BYAFILE.BYA_INT_PL_3_1_6,
      *         BYAFILE.BYA_NO_PL_3_1_7,
      *         BYAFILE.BYA_AMT_PL_3_1_7,
      *         BYAFILE.BYA_O2_PL_3_1_7,
      *         BYAFILE.BYA_INT_PL_3_1_7,
      *         BYAFILE.BYA_NO_PL_3_1_8,
      *         BYAFILE.BYA_AMT_PL_3_1_8,
      *         BYAFILE.BYA_O2_PL_3_1_8,
      *         BYAFILE.BYA_INT_PL_3_1_8,
      *         BYAFILE.BYA_NO_PL_3_1_9,
      *         BYAFILE.BYA_AMT_PL_3_1_9,
      *         BYAFILE.BYA_O2_PL_3_1_9,
      *         BYAFILE.BYA_INT_PL_3_1_9,
      *         BYAFILE.BYA_NO_PL_3_1_10,
      *         BYAFILE.BYA_AMT_PL_3_1_10,
      *         BYAFILE.BYA_O2_PL_3_1_10,
      *         BYAFILE.BYA_INT_PL_3_1_10,
      *         BYAFILE.BYA_NO_PL_3_2_1,
      *         BYAFILE.BYA_AMT_PL_3_2_1,
      *         BYAFILE.BYA_O2_PL_3_2_1,
      *         BYAFILE.BYA_INT_PL_3_2_1,
      *         BYAFILE.BYA_NO_PL_3_2_2,
      *         BYAFILE.BYA_AMT_PL_3_2_2,
      *         BYAFILE.BYA_O2_PL_3_2_2,
      *         BYAFILE.BYA_INT_PL_3_2_2,
      *         BYAFILE.BYA_NO_PL_3_2_3,
      *         BYAFILE.BYA_AMT_PL_3_2_3,
      *         BYAFILE.BYA_O2_PL_3_2_3,
      *         BYAFILE.BYA_INT_PL_3_2_3,
      *         BYAFILE.BYA_NO_PL_3_2_4,
      *         BYAFILE.BYA_AMT_PL_3_2_4,
      *         BYAFILE.BYA_O2_PL_3_2_4,
      *         BYAFILE.BYA_INT_PL_3_2_4,
      *         BYAFILE.BYA_NO_PL_3_2_5,
      *         BYAFILE.BYA_AMT_PL_3_2_5,
      *         BYAFILE.BYA_O2_PL_3_2_5,
      *         BYAFILE.BYA_INT_PL_3_2_5,
      *         BYAFILE.BYA_NO_PL_3_2_6,
      *         BYAFILE.BYA_AMT_PL_3_2_6,
      *         BYAFILE.BYA_O2_PL_3_2_6,
      *         BYAFILE.BYA_INT_PL_3_2_6,
      *         BYAFILE.BYA_NO_PL_3_2_7,
      *         BYAFILE.BYA_AMT_PL_3_2_7,
      *         BYAFILE.BYA_O2_PL_3_2_7,
      *         BYAFILE.BYA_INT_PL_3_2_7,
      *         BYAFILE.BYA_NO_PL_3_2_8,
      *         BYAFILE.BYA_AMT_PL_3_2_8,
      *         BYAFILE.BYA_O2_PL_3_2_8,
      *         BYAFILE.BYA_INT_PL_3_2_8,
      *         BYAFILE.BYA_NO_PL_3_2_9,
      *         BYAFILE.BYA_AMT_PL_3_2_9,
      *         BYAFILE.BYA_O2_PL_3_2_9,
      *         BYAFILE.BYA_INT_PL_3_2_9,
      *         BYAFILE.BYA_NO_PL_3_2_10,
      *         BYAFILE.BYA_AMT_PL_3_2_10,
      *         BYAFILE.BYA_O2_PL_3_2_10,
      *         BYAFILE.BYA_INT_PL_3_2_10,
      *         BYAFILE.BYA_DLRVPRIN_1_1_1,
      *         BYAFILE.BYA_DLRVPRIN_1_1_2,
      *         BYAFILE.BYA_DLRVPRIN_1_1_3,
      *         BYAFILE.BYA_DLRVCHGS_1_1_1,
      *         BYAFILE.BYA_DLRVCHGS_1_1_2,
      *         BYAFILE.BYA_DLRVCHGS_1_1_3,
      *         BYAFILE.BYA_DLPARVHELD_1_1,
      *         BYAFILE.BYA_DLRVPRIN_1_2_1,
      *         BYAFILE.BYA_DLRVPRIN_1_2_2,
      *         BYAFILE.BYA_DLRVPRIN_1_2_3,
      *         BYAFILE.BYA_DLRVCHGS_1_2_1,
      *         BYAFILE.BYA_DLRVCHGS_1_2_2,
      *         BYAFILE.BYA_DLRVCHGS_1_2_3,
      *         BYAFILE.BYA_DLPARVHELD_1_2,
      *         BYAFILE.BYA_DLRVPRIN_2_1_1,
      *         BYAFILE.BYA_DLRVPRIN_2_1_2,
      *         BYAFILE.BYA_DLRVPRIN_2_1_3,
      *         BYAFILE.BYA_DLRVCHGS_2_1_1,
      *         BYAFILE.BYA_DLRVCHGS_2_1_2,
      *         BYAFILE.BYA_DLRVCHGS_2_1_3,
      *         BYAFILE.BYA_DLPARVHELD_2_1,
      *         BYAFILE.BYA_DLRVPRIN_2_2_1,
      *         BYAFILE.BYA_DLRVPRIN_2_2_2,
      *         BYAFILE.BYA_DLRVPRIN_2_2_3,
      *         BYAFILE.BYA_DLRVCHGS_2_2_1,
      *         BYAFILE.BYA_DLRVCHGS_2_2_2,
      *         BYAFILE.BYA_DLRVCHGS_2_2_3,
      *         BYAFILE.BYA_DLPARVHELD_2_2,
      *         BYAFILE.BYA_DLRVPRIN_3_1_1,
      *         BYAFILE.BYA_DLRVPRIN_3_1_2,
      *         BYAFILE.BYA_DLRVPRIN_3_1_3,
      *         BYAFILE.BYA_DLRVCHGS_3_1_1,
      *         BYAFILE.BYA_DLRVCHGS_3_1_2,
      *         BYAFILE.BYA_DLRVCHGS_3_1_3,
      *         BYAFILE.BYA_DLPARVHELD_3_1,
      *         BYAFILE.BYA_DLRVPRIN_3_2_1,
      *         BYAFILE.BYA_DLRVPRIN_3_2_2,
      *         BYAFILE.BYA_DLRVPRIN_3_2_3,
      *         BYAFILE.BYA_DLRVCHGS_3_2_1,
      *         BYAFILE.BYA_DLRVCHGS_3_2_2,
      *         BYAFILE.BYA_DLRVCHGS_3_2_3,
      *         BYAFILE.BYA_DLPARVHELD_3_2
      *  
      *    INTO #TEMP_EOYUPD_BYAFILE
      *
      *    FROM DBO.BYAFILE 
      *   
      *   WHERE BYAFILE.BYA_DATE_CCYYMM BETWEEN @QBY1_WBEG_DATE  
      *                                     AND @QBY1_WEND_DATE
      *     AND BYAFILE.BYA_BRNO        BETWEEN @QBY1_WBEG_BRNO  
      *                                     AND @QBY1_WEND_BRNO
      *     AND BYAFILE.BYA_CLASS       BETWEEN @QBY1_WBEG_CLASS 
      *                                     AND @QBY1_WEND_CLASS;

           EXEC SQL SET CONNECTION BY1_CONNECT END-EXEC.

           EXEC SQL
            DECLARE BYA1_CURSOR CURSOR FOR
                :BY1-RETURN-CODE = EXEC SSP_EOYUPD_BYAFILE_SELECT 
              ( :QBY1-WBEG-DATE-CCYYMM,
                :QBY1-WEND-DATE-CCYYMM,
                :QBY1-WBEG-BRNO,
                :QBY1-WEND-BRNO, 
                :QBY1-WBEG-CLASS,      
                :QBY1-WEND-CLASS )     
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

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       READ-BYA1-FILE.
           PERFORM READ-IT.
           MOVE BYA-PATH-SQL TO E-FILE.
           MOVE BYA1-KEY TO E-KEYX.
           INITIALIZE QBYA-REC.

           MOVE BYA-DATE-CCYYMM      TO QBYA-DATE-CCYYMM.
           MOVE BYA-BRNO             TO QBYA-BRNO.
           MOVE BYA-CLASS            TO QBYA-CLASS.

           EXEC SQL
            SELECT BYAFILE.BYA_DATE_CCYYMM,
                   BYAFILE.BYA_BRNO,
                   BYAFILE.BYA_CLASS,
                   CAST(BYAFILE.BYA_LTOUCH_DATE AS VARCHAR(10)),
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
       READ-BYA1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE BYA-PATH-SQL TO E-FILE.
           MOVE BYA1-KEY TO E-KEYX.
           INITIALIZE QBYA-REC.

           EXEC SQL SET CONNECTION BY1_CONNECT END-EXEC.

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

           EXEC SQL SET CONNECTION C1 END-EXEC.

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

           MOVE '1' TO SQL-UPDATE-CONN-USE-SW. *> USE C1 CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

      *-----------------------------------------------------------------
       REWRITE-BYA1-FILE.

           ACCEPT BYA-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE BYA-PATH-SQL TO E-FILE.
           MOVE BYA1-KEY TO E-KEYX.
           PERFORM SET-BYA-FIELDS.

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

           MOVE '1' TO SQL-UPDATE-CONN-USE-SW. *> USE C1 CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

      *-----------------------------------------------------------------
       DELETE-BYA1-FILE.

           PERFORM DELETE-IT.
           MOVE BYA-PATH-SQL TO E-FILE.
           MOVE BYA1-KEY TO E-KEYX.
           PERFORM SET-BYA-FIELDS.

           MOVE '2' TO SQL-UPDATE-CONN-USE-SW. *> USE UPDATE_CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

           EXEC SQL
            DELETE FROM DBO.BYAFILE
             WHERE BYAFILE.BYA_DATE_CCYYMM = :QBYA-DATE-CCYYMM
               AND BYAFILE.BYA_BRNO = :QBYA-BRNO 
               AND BYAFILE.BYA_CLASS = :QBYA-CLASS
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-BYA1-FILE.

           MOVE '1' TO SQL-UPDATE-CONN-USE-SW. *> USE C1 CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

      *-----------------------------------------------------------------
       CLOSE-BYA1-FILE.

           IF ( BYA1-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION BY1_CONNECT END-EXEC
              EXEC SQL
                   CLOSE BYA1_CURSOR
              END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
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
       READ-BYB1-FILE.
           PERFORM READ-IT.
           MOVE BYB-PATH-SQL TO E-FILE.
           MOVE BYB1-KEY TO E-KEYX.
           INITIALIZE QBYB-REC.

           MOVE BYB-DATE-CCYYMM      TO QBYB-DATE-CCYYMM.
           MOVE BYB-BRNO             TO QBYB-BRNO.
           MOVE BYB-CLASS            TO QBYB-CLASS.

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
       WRITE-BYB1-FILE.

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

           MOVE '1' TO SQL-UPDATE-CONN-USE-SW. *> USE C1 CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

      *-----------------------------------------------------------------
       REWRITE-BYB1-FILE.

           PERFORM REWRITE-IT.
           MOVE BYB-PATH-SQL TO E-FILE.
           MOVE BYB1-KEY TO E-KEYX.
           PERFORM SET-BYB-FIELDS.

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

           MOVE '1' TO SQL-UPDATE-CONN-USE-SW. *> USE C1 CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

      *-----------------------------------------------------------------
       DELETE-BYB1-FILE.

           MOVE '2' TO SQL-UPDATE-CONN-USE-SW. *> USE UPDATE_CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

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

           MOVE '1' TO SQL-UPDATE-CONN-USE-SW. *> USE C1 CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

      *-----------------------------------------------------------------
       CLOSE-BYB1-FILE.

      *-----------------------------------------------------------------
       CLOSE-BY1-CONNECTION.

           IF BY1-CONN-OPEN-SW = "Y"
               EXEC SQL SET CONNECTION BY1_CONNECT END-EXEC
               EXEC SQL COMMIT END-EXEC
               EXEC SQL DISCONNECT BY1_CONNECT END-EXEC.
      *        EXEC SQL SET CONNECTION C1 END-EXEC
      *        MOVE "N" TO BY1-CONN-OPEN-SW.

      *-----------------------------------------------------------------


      * COPY "LIBLP/LPDS1IN.CPY".
      * COPYMEMBER: LIBLP/LPDS1IN
      *LOAD-DS1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
      *    IF ( DS-PATH-OVERRIDE = "Y" ) 
      *       NEXT SENTENCE
      *    ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
      *    IF ( DS-PATH-OVERRIDE = SPACES ) 
      *       MOVE EXT-FILPATH-BASE        TO DS-ALL-BASE
      *       MOVE EXT-FILPATH-MACHINE     TO DS-ALL-MACHINE
      *       MOVE EXT-FILPATH-DATA        TO DS-ALL-DATA
      *       MOVE DS-ALL-PATH             TO DS-PATH
      *    ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
      *    IF ( DS-PATH-OVERRIDE = "B" ) 
      *       MOVE FILPATH-BASE            TO DS-ALL-BASE
      *       MOVE FILPATH-MACHINE         TO DS-ALL-MACHINE
      *       MOVE FILPATH-DATA            TO DS-ALL-DATA
      *       MOVE DS-ALL-PATH             TO DS-PATH.

      *-----------------------------------------------------------------
      *OPEN-DS1-FILE.
      *    PERFORM LOAD-DS1-FILE.
      *    PERFORM OPEN-IT.
      *    MOVE DS-PATH-SQL TO E-FILE.

      *    IF DS1-CONN-OPEN-SW = "N" 
      *       MOVE "Y"         TO DS1-CONN-OPEN-SW
      *       EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS DS1_CONNECT 
      *                USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
      *       END-EXEC.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *    EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
      *START-DS1-FILE.
      *    PERFORM START-IT.
      *    MOVE DS-PATH-OWNBR  TO DS-BRANCH.
      *    MOVE DS-PATH-SQL    TO E-FILE.
      *    MOVE DS1-KEY        TO E-KEYX.

      *    IF ( DS1-CURSOR-STAT-GOOD )
      *       PERFORM CLOSE-DS1-FILE.

      *    IF ( QDS1-WEND-CLASS = ZEROES    ) OR
      *       ( QDS1-WEND-CLASS NOT NUMERIC )
      *       MOVE DS-PATH-OWNBR        TO QDS1-WBEG-BRANCH
      *       MOVE DS-PATH-OWNBR        TO QDS1-WEND-BRANCH
      *       MOVE DS-DATE-CCYYMM       TO QDS1-WBEG-DATE-CCYYMM
      *       MOVE ALL "9"              TO QDS1-WEND-DATE-CCYYMM
      *                                    QDS1-WEND-NUM
      *                                    QDS1-WEND-CLASS
      *       MOVE DS-NUM               TO QDS1-WBEG-NUM
      *       MOVE DS-CLASS             TO QDS1-WBEG-CLASS.

      *  STORED PROCEDURE:
      *
      *   (SELECT DSFILE.DS_DATE_CCYYMM,
      *          DSFILE.DS_BRANCH,
      *          DSFILE.DS_NUM,
      *          DSFILE.DS_CLASS,
      *          DSFILE.DS_NPURUNIT_1,
      *          DSFILE.DS_NPURUNIT_2,
      *          DSFILE.DS_NPURUNIT_3,
      *          DSFILE.DS_NPURAMT_1,
      *          DSFILE.DS_NPURAMT_2,
      *          DSFILE.DS_NPURAMT_3,
      *          DSFILE.DS_OPURUNIT_1,
      *          DSFILE.DS_OPURUNIT_2,
      *          DSFILE.DS_OPURUNIT_3,
      *          DSFILE.DS_OPURAMT_1,
      *          DSFILE.DS_OPURAMT_2,
      *          DSFILE.DS_OPURAMT_3,
      *          DSFILE.DS_RNEWUNIT_1,
      *          DSFILE.DS_RNEWUNIT_2,
      *          DSFILE.DS_RNEWUNIT_3,
      *          DSFILE.DS_RNEWAMT_1,
      *          DSFILE.DS_RNEWAMT_2,
      *          DSFILE.DS_RNEWAMT_3,
      *          DSFILE.DS_PAIDUNIT_1,
      *          DSFILE.DS_PAIDUNIT_2,
      *          DSFILE.DS_PAIDUNIT_3,
      *          DSFILE.DS_PAIDAMT_1,
      *          DSFILE.DS_PAIDAMT_2,
      *          DSFILE.DS_PAIDAMT_3,
      *          DSFILE.DS_TODLUNIT_1,
      *          DSFILE.DS_TODLUNIT_2,
      *          DSFILE.DS_TODLUNIT_3,
      *          DSFILE.DS_TODLAMT_1,
      *          DSFILE.DS_TODLAMT_2,
      *          DSFILE.DS_TODLAMT_3,
      *          DSFILE.DS_OUTSUNIT_1,
      *          DSFILE.DS_OUTSUNIT_2,
      *          DSFILE.DS_OUTSUNIT_3,
      *          DSFILE.DS_OUTSAMT_1,
      *          DSFILE.DS_OUTSAMT_2,
      *          DSFILE.DS_OUTSAMT_3,
      *          DSFILE.DS_PLOSUNIT_1,
      *          DSFILE.DS_PLOSUNIT_2,
      *          DSFILE.DS_PLOSUNIT_3,
      *          DSFILE.DS_PLOSAMT_1,
      *          DSFILE.DS_PLOSAMT_2,
      *          DSFILE.DS_PLOSAMT_3,
      *          DSFILE.DS_RCPLUNIT_1,
      *          DSFILE.DS_RCPLUNIT_2,
      *          DSFILE.DS_RCPLUNIT_3,
      *          DSFILE.DS_RCPLAMT_1,
      *          DSFILE.DS_RCPLAMT_2,
      *          DSFILE.DS_RCPLAMT_3,
      *          DSFILE.DS_JDOSUNIT_1,
      *          DSFILE.DS_JDOSUNIT_2,
      *          DSFILE.DS_JDOSUNIT_3,
      *          DSFILE.DS_JDOSAMT_1,
      *          DSFILE.DS_JDOSAMT_2,
      *          DSFILE.DS_JDOSAMT_3,
      *          DSFILE.DS_PROCEEDS_1,
      *          DSFILE.DS_PROCEEDS_2,
      *          DSFILE.DS_NO_REPO_1_1_1,
      *          DSFILE.DS_NO_REPO_1_1_2,
      *          DSFILE.DS_NO_REPO_1_1_3,
      *          DSFILE.DS_NO_REPO_1_1_4,
      *          DSFILE.DS_NO_REPO_1_1_5,
      *          DSFILE.DS_NO_REPO_1_2_1,
      *          DSFILE.DS_NO_REPO_1_2_2,
      *          DSFILE.DS_NO_REPO_1_2_3,
      *          DSFILE.DS_NO_REPO_1_2_4,
      *          DSFILE.DS_NO_REPO_1_2_5,
      *          DSFILE.DS_NO_REPO_2_1_1,
      *          DSFILE.DS_NO_REPO_2_1_2,
      *          DSFILE.DS_NO_REPO_2_1_3,
      *          DSFILE.DS_NO_REPO_2_1_4,
      *          DSFILE.DS_NO_REPO_2_1_5,
      *          DSFILE.DS_NO_REPO_2_2_1,
      *          DSFILE.DS_NO_REPO_2_2_2,
      *          DSFILE.DS_NO_REPO_2_2_3,
      *          DSFILE.DS_NO_REPO_2_2_4,
      *          DSFILE.DS_NO_REPO_2_2_5,
      *          DSFILE.DS_NO_REPO_3_1_1,
      *          DSFILE.DS_NO_REPO_3_1_2,
      *          DSFILE.DS_NO_REPO_3_1_3,
      *          DSFILE.DS_NO_REPO_3_1_4,
      *          DSFILE.DS_NO_REPO_3_1_5,
      *          DSFILE.DS_NO_REPO_3_2_1,
      *          DSFILE.DS_NO_REPO_3_2_2,
      *          DSFILE.DS_NO_REPO_3_2_3,
      *          DSFILE.DS_NO_REPO_3_2_4,
      *          DSFILE.DS_NO_REPO_3_2_5,
      *          DSFILE.DS_AMT_REPO_1_1_1,
      *          DSFILE.DS_AMT_REPO_1_1_2,
      *          DSFILE.DS_AMT_REPO_1_1_3,
      *          DSFILE.DS_AMT_REPO_1_1_4,
      *          DSFILE.DS_AMT_REPO_1_1_5,
      *          DSFILE.DS_AMT_REPO_1_2_1,
      *          DSFILE.DS_AMT_REPO_1_2_2,
      *          DSFILE.DS_AMT_REPO_1_2_3,
      *          DSFILE.DS_AMT_REPO_1_2_4,
      *          DSFILE.DS_AMT_REPO_1_2_5,
      *          DSFILE.DS_AMT_REPO_2_1_1,
      *          DSFILE.DS_AMT_REPO_2_1_2,
      *          DSFILE.DS_AMT_REPO_2_1_3,
      *          DSFILE.DS_AMT_REPO_2_1_4,
      *          DSFILE.DS_AMT_REPO_2_1_5,
      *          DSFILE.DS_AMT_REPO_2_2_1,
      *          DSFILE.DS_AMT_REPO_2_2_2,
      *          DSFILE.DS_AMT_REPO_2_2_3,
      *          DSFILE.DS_AMT_REPO_2_2_4,
      *          DSFILE.DS_AMT_REPO_2_2_5,
      *          DSFILE.DS_AMT_REPO_3_1_1,
      *          DSFILE.DS_AMT_REPO_3_1_2,
      *          DSFILE.DS_AMT_REPO_3_1_3,
      *          DSFILE.DS_AMT_REPO_3_1_4,
      *          DSFILE.DS_AMT_REPO_3_1_5,
      *          DSFILE.DS_AMT_REPO_3_2_1,
      *          DSFILE.DS_AMT_REPO_3_2_2,
      *          DSFILE.DS_AMT_REPO_3_2_3,
      *          DSFILE.DS_AMT_REPO_3_2_4,
      *          DSFILE.DS_AMT_REPO_3_2_5,
      *          DSFILE.DS_UNEARNED_AMT_1_1_1,
      *          DSFILE.DS_UNEARNED_AMT_1_1_2,
      *          DSFILE.DS_UNEARNED_AMT_1_1_3,
      *          DSFILE.DS_UNEARNED_AMT_1_1_4,
      *          DSFILE.DS_UNEARNED_AMT_1_1_5,
      *          DSFILE.DS_UNEARNED_AMT_1_1_6,
      *          DSFILE.DS_UNEARNED_AMT_1_1_7,
      *          DSFILE.DS_UNEARNED_AMT_1_1_8,
      *          DSFILE.DS_UNEARNED_AMT_1_1_9,
      *          DSFILE.DS_UNEARNED_AMT_1_1_10,
      *          DSFILE.DS_UNEARNED_AMT_1_2_1,
      *          DSFILE.DS_UNEARNED_AMT_1_2_2,
      *          DSFILE.DS_UNEARNED_AMT_1_2_3,
      *          DSFILE.DS_UNEARNED_AMT_1_2_4,
      *          DSFILE.DS_UNEARNED_AMT_1_2_5,
      *          DSFILE.DS_UNEARNED_AMT_1_2_6,
      *          DSFILE.DS_UNEARNED_AMT_1_2_7,
      *          DSFILE.DS_UNEARNED_AMT_1_2_8,
      *          DSFILE.DS_UNEARNED_AMT_1_2_9,
      *          DSFILE.DS_UNEARNED_AMT_1_2_10,
      *          DSFILE.DS_UNEARNED_AMT_2_1_1,
      *          DSFILE.DS_UNEARNED_AMT_2_1_2,
      *          DSFILE.DS_UNEARNED_AMT_2_1_3,
      *          DSFILE.DS_UNEARNED_AMT_2_1_4,
      *          DSFILE.DS_UNEARNED_AMT_2_1_5,
      *          DSFILE.DS_UNEARNED_AMT_2_1_6,
      *          DSFILE.DS_UNEARNED_AMT_2_1_7,
      *          DSFILE.DS_UNEARNED_AMT_2_1_8,
      *          DSFILE.DS_UNEARNED_AMT_2_1_9,
      *          DSFILE.DS_UNEARNED_AMT_2_1_10,
      *          DSFILE.DS_UNEARNED_AMT_2_2_1,
      *          DSFILE.DS_UNEARNED_AMT_2_2_2,
      *          DSFILE.DS_UNEARNED_AMT_2_2_3,
      *          DSFILE.DS_UNEARNED_AMT_2_2_4,
      *          DSFILE.DS_UNEARNED_AMT_2_2_5,
      *          DSFILE.DS_UNEARNED_AMT_2_2_6,
      *          DSFILE.DS_UNEARNED_AMT_2_2_7,
      *          DSFILE.DS_UNEARNED_AMT_2_2_8,
      *          DSFILE.DS_UNEARNED_AMT_2_2_9,
      *          DSFILE.DS_UNEARNED_AMT_2_2_10,
      *          DSFILE.DS_UNEARNED_AMT_3_1_1,
      *          DSFILE.DS_UNEARNED_AMT_3_1_2,
      *          DSFILE.DS_UNEARNED_AMT_3_1_3,
      *          DSFILE.DS_UNEARNED_AMT_3_1_4,
      *          DSFILE.DS_UNEARNED_AMT_3_1_5,
      *          DSFILE.DS_UNEARNED_AMT_3_1_6,
      *          DSFILE.DS_UNEARNED_AMT_3_1_7,
      *          DSFILE.DS_UNEARNED_AMT_3_1_8,
      *          DSFILE.DS_UNEARNED_AMT_3_1_9,
      *          DSFILE.DS_UNEARNED_AMT_3_1_10,
      *          DSFILE.DS_UNEARNED_AMT_3_2_1,
      *          DSFILE.DS_UNEARNED_AMT_3_2_2,
      *          DSFILE.DS_UNEARNED_AMT_3_2_3,
      *          DSFILE.DS_UNEARNED_AMT_3_2_4,
      *          DSFILE.DS_UNEARNED_AMT_3_2_5,
      *          DSFILE.DS_UNEARNED_AMT_3_2_6,
      *          DSFILE.DS_UNEARNED_AMT_3_2_7,
      *          DSFILE.DS_UNEARNED_AMT_3_2_8,
      *          DSFILE.DS_UNEARNED_AMT_3_2_9,
      *          DSFILE.DS_UNEARNED_AMT_3_2_10,
      *          DSFILE.DS_DLDISC_VOL_1,
      *          DSFILE.DS_DLDISC_VOL_2,
      *          DSFILE.DS_DLDISC_VOL_3
      * 
      *     FROM DBO.DSFILE DSFILE
      *
      *    WHERE DSFILE.DS_BRANCH            = @QDS1_WBEG_BRANCH
      *      AND DSFILE.DS_DATE_CCYYMM BETWEEN @QDS1_WBEG_DATE_CCYYMM
      *                                    AND @QDS1_WEND_DATE_CCYYMM
      *      AND DSFILE.DS_NUM         BETWEEN @QDS1_WBEG_NUM
      *                                    AND @QDS1_WEND_NUM
      *      AND DSFILE.DS_CLASS       BETWEEN @QDS1_WBEG_CLASS
      *                                    AND @QDS1_WEND_CLASS) TEMP;

      *    EXEC SQL SET CONNECTION DS1_CONNECT END-EXEC.

      *    EXEC SQL
      *     DECLARE DS1_CURSOR CURSOR FOR
      *         :DS1-RETURN-CODE = EXEC SSP_EOYUPD_DSFILE_SELECT 
      *       ( :QDS1-WBEG-BRANCH,      
      *         :QDS1-WBEG-DATE-CCYYMM, 
      *         :QDS1-WEND-DATE-CCYYMM,
      *         :QDS1-WBEG-NUM,        
      *         :QDS1-WEND-NUM,        
      *         :QDS1-WBEG-CLASS,      
      *         :QDS1-WEND-CLASS )      
      *    END-EXEC.


      *    EXEC SQL
      *        OPEN DS1_CURSOR
      *    END-EXEC.

      *    PERFORM SQL-IO-VALIDATION.

      *    MOVE ZEROES TO QDS1-WBEG-BRANCH 
      *                   QDS1-WEND-BRANCH
      *                   QDS1-WBEG-DATE-CCYYMM
      *                   QDS1-WEND-DATE-CCYYMM
      *                   QDS1-WBEG-NUM
      *                   QDS1-WEND-NUM
      *                   QDS1-WBEG-CLASS
      *                   QDS1-WEND-CLASS.
 
      *    MOVE STAT---GOOD TO DS1-CURSOR-STAT.

      *    EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
      *READ-DS1-FILE-NEXT.
      *    PERFORM READ-IT.
      *    MOVE DS-PATH-OWNBR  TO DS-BRANCH.
      *    MOVE DS-PATH-SQL    TO E-FILE.
      *    MOVE DS1-KEY        TO E-KEYX.
      *    INITIALIZE QDS-REC.

      *    EXEC SQL SET CONNECTION DS1_CONNECT END-EXEC.

      *    IF ( DS1-CURSOR-STAT-GOOD )
      *       EXEC SQL
      *         FETCH DS1_CURSOR 
      *          INTO  
      *            :QDS-DATE-CCYYMM,
      *            :QDS-BRANCH,
      *            :QDS-NUM,
      *            :QDS-CLASS,
      *            :QDS-NPURUNIT-1,
      *            :QDS-NPURUNIT-2,
      *            :QDS-NPURUNIT-3,
      *            :QDS-NPURAMT-1,
      *            :QDS-NPURAMT-2,
      *            :QDS-NPURAMT-3,
      *            :QDS-OPURUNIT-1,
      *            :QDS-OPURUNIT-2,
      *            :QDS-OPURUNIT-3,
      *            :QDS-OPURAMT-1,
      *            :QDS-OPURAMT-2,
      *            :QDS-OPURAMT-3,
      *            :QDS-RNEWUNIT-1,
      *            :QDS-RNEWUNIT-2,
      *            :QDS-RNEWUNIT-3,
      *            :QDS-RNEWAMT-1,
      *            :QDS-RNEWAMT-2,
      *            :QDS-RNEWAMT-3,
      *            :QDS-PAIDUNIT-1,
      *            :QDS-PAIDUNIT-2,
      *            :QDS-PAIDUNIT-3,
      *            :QDS-PAIDAMT-1,
      *            :QDS-PAIDAMT-2,
      *            :QDS-PAIDAMT-3,
      *            :QDS-TODLUNIT-1,
      *            :QDS-TODLUNIT-2,
      *            :QDS-TODLUNIT-3,
      *            :QDS-TODLAMT-1,
      *            :QDS-TODLAMT-2,
      *            :QDS-TODLAMT-3,
      *            :QDS-OUTSUNIT-1,
      *            :QDS-OUTSUNIT-2,
      *            :QDS-OUTSUNIT-3,
      *            :QDS-OUTSAMT-1,
      *            :QDS-OUTSAMT-2,
      *            :QDS-OUTSAMT-3,
      *            :QDS-PLOSUNIT-1,
      *            :QDS-PLOSUNIT-2,
      *            :QDS-PLOSUNIT-3,
      *            :QDS-PLOSAMT-1,
      *            :QDS-PLOSAMT-2,
      *            :QDS-PLOSAMT-3,
      *            :QDS-RCPLUNIT-1,
      *            :QDS-RCPLUNIT-2,
      *            :QDS-RCPLUNIT-3,
      *            :QDS-RCPLAMT-1,
      *            :QDS-RCPLAMT-2,
      *            :QDS-RCPLAMT-3,
      *            :QDS-JDOSUNIT-1,
      *            :QDS-JDOSUNIT-2,
      *            :QDS-JDOSUNIT-3,
      *            :QDS-JDOSAMT-1,
      *            :QDS-JDOSAMT-2,
      *            :QDS-JDOSAMT-3,
      *            :QDS-PROCEEDS-1,
      *            :QDS-PROCEEDS-2,
      *            :QDS-NO-REPO-1-1-1,
      *            :QDS-NO-REPO-1-1-2,
      *            :QDS-NO-REPO-1-1-3,
      *            :QDS-NO-REPO-1-1-4,
      *            :QDS-NO-REPO-1-1-5,
      *            :QDS-NO-REPO-1-2-1,
      *            :QDS-NO-REPO-1-2-2,
      *            :QDS-NO-REPO-1-2-3,
      *            :QDS-NO-REPO-1-2-4,
      *            :QDS-NO-REPO-1-2-5,
      *            :QDS-NO-REPO-2-1-1,
      *            :QDS-NO-REPO-2-1-2,
      *            :QDS-NO-REPO-2-1-3,
      *            :QDS-NO-REPO-2-1-4,
      *            :QDS-NO-REPO-2-1-5,
      *            :QDS-NO-REPO-2-2-1,
      *            :QDS-NO-REPO-2-2-2,
      *            :QDS-NO-REPO-2-2-3,
      *            :QDS-NO-REPO-2-2-4,
      *            :QDS-NO-REPO-2-2-5,
      *            :QDS-NO-REPO-3-1-1,
      *            :QDS-NO-REPO-3-1-2,
      *            :QDS-NO-REPO-3-1-3,
      *            :QDS-NO-REPO-3-1-4,
      *            :QDS-NO-REPO-3-1-5,
      *            :QDS-NO-REPO-3-2-1,
      *            :QDS-NO-REPO-3-2-2,
      *            :QDS-NO-REPO-3-2-3,
      *            :QDS-NO-REPO-3-2-4,
      *            :QDS-NO-REPO-3-2-5,
      *            :QDS-AMT-REPO-1-1-1,
      *            :QDS-AMT-REPO-1-1-2,
      *            :QDS-AMT-REPO-1-1-3,
      *            :QDS-AMT-REPO-1-1-4,
      *            :QDS-AMT-REPO-1-1-5,
      *            :QDS-AMT-REPO-1-2-1,
      *            :QDS-AMT-REPO-1-2-2,
      *            :QDS-AMT-REPO-1-2-3,
      *            :QDS-AMT-REPO-1-2-4,
      *            :QDS-AMT-REPO-1-2-5,
      *            :QDS-AMT-REPO-2-1-1,
      *            :QDS-AMT-REPO-2-1-2,
      *            :QDS-AMT-REPO-2-1-3,
      *            :QDS-AMT-REPO-2-1-4,
      *            :QDS-AMT-REPO-2-1-5,
      *            :QDS-AMT-REPO-2-2-1,
      *            :QDS-AMT-REPO-2-2-2,
      *            :QDS-AMT-REPO-2-2-3,
      *            :QDS-AMT-REPO-2-2-4,
      *            :QDS-AMT-REPO-2-2-5,
      *            :QDS-AMT-REPO-3-1-1,
      *            :QDS-AMT-REPO-3-1-2,
      *            :QDS-AMT-REPO-3-1-3,
      *            :QDS-AMT-REPO-3-1-4,
      *            :QDS-AMT-REPO-3-1-5,
      *            :QDS-AMT-REPO-3-2-1,
      *            :QDS-AMT-REPO-3-2-2,
      *            :QDS-AMT-REPO-3-2-3,
      *            :QDS-AMT-REPO-3-2-4,
      *            :QDS-AMT-REPO-3-2-5,
      *            :QDS-UNEARNED-AMT-1-1-1,
      *            :QDS-UNEARNED-AMT-1-1-2,
      *            :QDS-UNEARNED-AMT-1-1-3,
      *            :QDS-UNEARNED-AMT-1-1-4,
      *            :QDS-UNEARNED-AMT-1-1-5,
      *            :QDS-UNEARNED-AMT-1-1-6,
      *            :QDS-UNEARNED-AMT-1-1-7,
      *            :QDS-UNEARNED-AMT-1-1-8,
      *            :QDS-UNEARNED-AMT-1-1-9,
      *            :QDS-UNEARNED-AMT-1-1-10,
      *            :QDS-UNEARNED-AMT-1-2-1,
      *            :QDS-UNEARNED-AMT-1-2-2,
      *            :QDS-UNEARNED-AMT-1-2-3,
      *            :QDS-UNEARNED-AMT-1-2-4,
      *            :QDS-UNEARNED-AMT-1-2-5,
      *            :QDS-UNEARNED-AMT-1-2-6,
      *            :QDS-UNEARNED-AMT-1-2-7,
      *            :QDS-UNEARNED-AMT-1-2-8,
      *            :QDS-UNEARNED-AMT-1-2-9,
      *            :QDS-UNEARNED-AMT-1-2-10,
      *            :QDS-UNEARNED-AMT-2-1-1,
      *            :QDS-UNEARNED-AMT-2-1-2,
      *            :QDS-UNEARNED-AMT-2-1-3,
      *            :QDS-UNEARNED-AMT-2-1-4,
      *            :QDS-UNEARNED-AMT-2-1-5,
      *            :QDS-UNEARNED-AMT-2-1-6,
      *            :QDS-UNEARNED-AMT-2-1-7,
      *            :QDS-UNEARNED-AMT-2-1-8,
      *            :QDS-UNEARNED-AMT-2-1-9,
      *            :QDS-UNEARNED-AMT-2-1-10,
      *            :QDS-UNEARNED-AMT-2-2-1,
      *            :QDS-UNEARNED-AMT-2-2-2,
      *            :QDS-UNEARNED-AMT-2-2-3,
      *            :QDS-UNEARNED-AMT-2-2-4,
      *            :QDS-UNEARNED-AMT-2-2-5,
      *            :QDS-UNEARNED-AMT-2-2-6,
      *            :QDS-UNEARNED-AMT-2-2-7,
      *            :QDS-UNEARNED-AMT-2-2-8,
      *            :QDS-UNEARNED-AMT-2-2-9,
      *            :QDS-UNEARNED-AMT-2-2-10,
      *            :QDS-UNEARNED-AMT-3-1-1,
      *            :QDS-UNEARNED-AMT-3-1-2,
      *            :QDS-UNEARNED-AMT-3-1-3,
      *            :QDS-UNEARNED-AMT-3-1-4,
      *            :QDS-UNEARNED-AMT-3-1-5,
      *            :QDS-UNEARNED-AMT-3-1-6,
      *            :QDS-UNEARNED-AMT-3-1-7,
      *            :QDS-UNEARNED-AMT-3-1-8,
      *            :QDS-UNEARNED-AMT-3-1-9,
      *            :QDS-UNEARNED-AMT-3-1-10,
      *            :QDS-UNEARNED-AMT-3-2-1,
      *            :QDS-UNEARNED-AMT-3-2-2,
      *            :QDS-UNEARNED-AMT-3-2-3,
      *            :QDS-UNEARNED-AMT-3-2-4,
      *            :QDS-UNEARNED-AMT-3-2-5,
      *            :QDS-UNEARNED-AMT-3-2-6,
      *            :QDS-UNEARNED-AMT-3-2-7,
      *            :QDS-UNEARNED-AMT-3-2-8,
      *            :QDS-UNEARNED-AMT-3-2-9,
      *            :QDS-UNEARNED-AMT-3-2-10,
      *            :QDS-DLDISC-VOL-1,
      *            :QDS-DLDISC-VOL-2,
      *            :QDS-DLDISC-VOL-3
      *       END-EXEC
      *    ELSE
      *       MOVE 9        TO SQLCODE
      *       MOVE "RDNXCR" TO SQLSTATE.

      *    PERFORM SQL-IO-VALIDATION.

      *    IF ( IO-FG = 8 )     *>LOCKED
      *       GO TO READ-DS1-FILE-NEXT.

      *    IF ( IO-FG = 0 )
      *       PERFORM GET-DS-FIELDS.

      *    EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
      *REWRITE-DS1-FILE.

      *    MOVE DS-PATH-OWNBR  TO DS-BRANCH.
      *    ACCEPT DS-LTOUCH-DATE FROM CENTURY-DATE.
      *    PERFORM REWRITE-IT.
      *    MOVE DS-PATH-SQL    TO E-FILE.
      *    MOVE DS1-KEY        TO E-KEYX.
      *    PERFORM SET-DS-FIELDS.

      *    MOVE '2' TO SQL-UPDATE-CONN-USE-SW. *> USE UPDATE_CONNECT
      *    PERFORM SQL-SWAP-UPDATE-CONNECTION.

      *    EXEC SQL
      *     UPDATE DBO.DSFILE
      *      SET
      *          DSFILE.DS_DATE_CCYYMM         = :QDS-DATE-CCYYMM,
      *          DSFILE.DS_BRANCH              = :QDS-BRANCH,
      *          DSFILE.DS_NUM                 = :QDS-NUM,
      *          DSFILE.DS_CLASS               = :QDS-CLASS,
      *          DSFILE.DS_NPURUNIT_1          = :QDS-NPURUNIT-1,
      *          DSFILE.DS_NPURUNIT_2          = :QDS-NPURUNIT-2,
      *          DSFILE.DS_NPURUNIT_3          = :QDS-NPURUNIT-3,
      *          DSFILE.DS_NPURAMT_1           = :QDS-NPURAMT-1,
      *          DSFILE.DS_NPURAMT_2           = :QDS-NPURAMT-2,
      *          DSFILE.DS_NPURAMT_3           = :QDS-NPURAMT-3,
      *          DSFILE.DS_OPURUNIT_1          = :QDS-OPURUNIT-1,
      *          DSFILE.DS_OPURUNIT_2          = :QDS-OPURUNIT-2,
      *          DSFILE.DS_OPURUNIT_3          = :QDS-OPURUNIT-3,
      *          DSFILE.DS_OPURAMT_1           = :QDS-OPURAMT-1,        
      *          DSFILE.DS_OPURAMT_2           = :QDS-OPURAMT-2,
      *          DSFILE.DS_OPURAMT_3           = :QDS-OPURAMT-3,
      *          DSFILE.DS_RNEWUNIT_1          = :QDS-RNEWUNIT-1,
      *          DSFILE.DS_RNEWUNIT_2          = :QDS-RNEWUNIT-2,
      *          DSFILE.DS_RNEWUNIT_3          = :QDS-RNEWUNIT-3,
      *          DSFILE.DS_RNEWAMT_1           = :QDS-RNEWAMT-1,
      *          DSFILE.DS_RNEWAMT_2           = :QDS-RNEWAMT-2,
      *          DSFILE.DS_RNEWAMT_3           = :QDS-RNEWAMT-3,
      *          DSFILE.DS_PAIDUNIT_1          = :QDS-PAIDUNIT-1,
      *          DSFILE.DS_PAIDUNIT_2          = :QDS-PAIDUNIT-2,
      *          DSFILE.DS_PAIDUNIT_3          = :QDS-PAIDUNIT-3,
      *          DSFILE.DS_PAIDAMT_1           = :QDS-PAIDAMT-1,
      *          DSFILE.DS_PAIDAMT_2           = :QDS-PAIDAMT-2,
      *          DSFILE.DS_PAIDAMT_3           = :QDS-PAIDAMT-3,
      *          DSFILE.DS_TODLUNIT_1          = :QDS-TODLUNIT-1,
      *          DSFILE.DS_TODLUNIT_2          = :QDS-TODLUNIT-2,
      *          DSFILE.DS_TODLUNIT_3          = :QDS-TODLUNIT-3,
      *          DSFILE.DS_TODLAMT_1           = :QDS-TODLAMT-1,
      *          DSFILE.DS_TODLAMT_2           = :QDS-TODLAMT-2,
      *          DSFILE.DS_TODLAMT_3           = :QDS-TODLAMT-3,
      *          DSFILE.DS_OUTSUNIT_1          = :QDS-OUTSUNIT-1,
      *          DSFILE.DS_OUTSUNIT_2          = :QDS-OUTSUNIT-2,
      *          DSFILE.DS_OUTSUNIT_3          = :QDS-OUTSUNIT-3,
      *          DSFILE.DS_OUTSAMT_1           = :QDS-OUTSAMT-1,
      *          DSFILE.DS_OUTSAMT_2           = :QDS-OUTSAMT-2,
      *          DSFILE.DS_OUTSAMT_3           = :QDS-OUTSAMT-3,
      *          DSFILE.DS_PLOSUNIT_1          = :QDS-PLOSUNIT-1,
      *          DSFILE.DS_PLOSUNIT_2          = :QDS-PLOSUNIT-2,
      *          DSFILE.DS_PLOSUNIT_3          = :QDS-PLOSUNIT-3,
      *          DSFILE.DS_PLOSAMT_1           = :QDS-PLOSAMT-1,
      *          DSFILE.DS_PLOSAMT_2           = :QDS-PLOSAMT-2,
      *          DSFILE.DS_PLOSAMT_3           = :QDS-PLOSAMT-3,
      *          DSFILE.DS_RCPLUNIT_1          = :QDS-RCPLUNIT-1,
      *          DSFILE.DS_RCPLUNIT_2          = :QDS-RCPLUNIT-2,
      *          DSFILE.DS_RCPLUNIT_3          = :QDS-RCPLUNIT-3,
      *          DSFILE.DS_RCPLAMT_1           = :QDS-RCPLAMT-1,
      *          DSFILE.DS_RCPLAMT_2           = :QDS-RCPLAMT-2,
      *          DSFILE.DS_RCPLAMT_3           = :QDS-RCPLAMT-3,
      *          DSFILE.DS_JDOSUNIT_1          = :QDS-JDOSUNIT-1,
      *          DSFILE.DS_JDOSUNIT_2          = :QDS-JDOSUNIT-2,
      *          DSFILE.DS_JDOSUNIT_3          = :QDS-JDOSUNIT-3,
      *          DSFILE.DS_JDOSAMT_1           = :QDS-JDOSAMT-1,
      *          DSFILE.DS_JDOSAMT_2           = :QDS-JDOSAMT-2,
      *          DSFILE.DS_JDOSAMT_3           = :QDS-JDOSAMT-3,
      *          DSFILE.DS_PROCEEDS_1          = :QDS-PROCEEDS-1,
      *          DSFILE.DS_PROCEEDS_2          = :QDS-PROCEEDS-2,
      *          DSFILE.DS_NO_REPO_1_1_1       = :QDS-NO-REPO-1-1-1,
      *          DSFILE.DS_NO_REPO_1_1_2       = :QDS-NO-REPO-1-1-2,
      *          DSFILE.DS_NO_REPO_1_1_3       = :QDS-NO-REPO-1-1-3,
      *          DSFILE.DS_NO_REPO_1_1_4       = :QDS-NO-REPO-1-1-4,
      *          DSFILE.DS_NO_REPO_1_1_5       = :QDS-NO-REPO-1-1-5,
      *          DSFILE.DS_NO_REPO_1_2_1       = :QDS-NO-REPO-1-2-1,
      *          DSFILE.DS_NO_REPO_1_2_2       = :QDS-NO-REPO-1-2-2,
      *          DSFILE.DS_NO_REPO_1_2_3       = :QDS-NO-REPO-1-2-3,
      *          DSFILE.DS_NO_REPO_1_2_4       = :QDS-NO-REPO-1-2-4,
      *          DSFILE.DS_NO_REPO_1_2_5       = :QDS-NO-REPO-1-2-5,
      *          DSFILE.DS_NO_REPO_2_1_1       = :QDS-NO-REPO-2-1-1,
      *          DSFILE.DS_NO_REPO_2_1_2       = :QDS-NO-REPO-2-1-2,
      *          DSFILE.DS_NO_REPO_2_1_3       = :QDS-NO-REPO-2-1-3,
      *          DSFILE.DS_NO_REPO_2_1_4       = :QDS-NO-REPO-2-1-4,
      *          DSFILE.DS_NO_REPO_2_1_5       = :QDS-NO-REPO-2-1-5,
      *          DSFILE.DS_NO_REPO_2_2_1       = :QDS-NO-REPO-2-2-1,
      *          DSFILE.DS_NO_REPO_2_2_2       = :QDS-NO-REPO-2-2-2,
      *          DSFILE.DS_NO_REPO_2_2_3       = :QDS-NO-REPO-2-2-3,
      *          DSFILE.DS_NO_REPO_2_2_4       = :QDS-NO-REPO-2-2-4,
      *          DSFILE.DS_NO_REPO_2_2_5       = :QDS-NO-REPO-2-2-5,
      *          DSFILE.DS_NO_REPO_3_1_1       = :QDS-NO-REPO-3-1-1,
      *          DSFILE.DS_NO_REPO_3_1_2       = :QDS-NO-REPO-3-1-2,
      *          DSFILE.DS_NO_REPO_3_1_3       = :QDS-NO-REPO-3-1-3,
      *          DSFILE.DS_NO_REPO_3_1_4       = :QDS-NO-REPO-3-1-4,
      *          DSFILE.DS_NO_REPO_3_1_5       = :QDS-NO-REPO-3-1-5,
      *          DSFILE.DS_NO_REPO_3_2_1       = :QDS-NO-REPO-3-2-1,
      *          DSFILE.DS_NO_REPO_3_2_2       = :QDS-NO-REPO-3-2-2,
      *          DSFILE.DS_NO_REPO_3_2_3       = :QDS-NO-REPO-3-2-3,
      *          DSFILE.DS_NO_REPO_3_2_4       = :QDS-NO-REPO-3-2-4,
      *          DSFILE.DS_NO_REPO_3_2_5       = :QDS-NO-REPO-3-2-5,
      *          DSFILE.DS_AMT_REPO_1_1_1      = :QDS-AMT-REPO-1-1-1,
      *          DSFILE.DS_AMT_REPO_1_1_2      = :QDS-AMT-REPO-1-1-2,
      *          DSFILE.DS_AMT_REPO_1_1_3      = :QDS-AMT-REPO-1-1-3,
      *          DSFILE.DS_AMT_REPO_1_1_4      = :QDS-AMT-REPO-1-1-4,
      *          DSFILE.DS_AMT_REPO_1_1_5      = :QDS-AMT-REPO-1-1-5,
      *          DSFILE.DS_AMT_REPO_1_2_1      = :QDS-AMT-REPO-1-2-1,
      *          DSFILE.DS_AMT_REPO_1_2_2      = :QDS-AMT-REPO-1-2-2,
      *          DSFILE.DS_AMT_REPO_1_2_3      = :QDS-AMT-REPO-1-2-3,
      *          DSFILE.DS_AMT_REPO_1_2_4      = :QDS-AMT-REPO-1-2-4,
      *          DSFILE.DS_AMT_REPO_1_2_5      = :QDS-AMT-REPO-1-2-5,
      *          DSFILE.DS_AMT_REPO_2_1_1      = :QDS-AMT-REPO-2-1-1,
      *          DSFILE.DS_AMT_REPO_2_1_2      = :QDS-AMT-REPO-2-1-2,
      *          DSFILE.DS_AMT_REPO_2_1_3      = :QDS-AMT-REPO-2-1-3,
      *          DSFILE.DS_AMT_REPO_2_1_4      = :QDS-AMT-REPO-2-1-4,
      *          DSFILE.DS_AMT_REPO_2_1_5      = :QDS-AMT-REPO-2-1-5,
      *          DSFILE.DS_AMT_REPO_2_2_1      = :QDS-AMT-REPO-2-2-1,
      *          DSFILE.DS_AMT_REPO_2_2_2      = :QDS-AMT-REPO-2-2-2,
      *          DSFILE.DS_AMT_REPO_2_2_3      = :QDS-AMT-REPO-2-2-3,
      *          DSFILE.DS_AMT_REPO_2_2_4      = :QDS-AMT-REPO-2-2-4,
      *          DSFILE.DS_AMT_REPO_2_2_5      = :QDS-AMT-REPO-2-2-5,
      *          DSFILE.DS_AMT_REPO_3_1_1      = :QDS-AMT-REPO-3-1-1,
      *          DSFILE.DS_AMT_REPO_3_1_2      = :QDS-AMT-REPO-3-1-2,
      *          DSFILE.DS_AMT_REPO_3_1_3      = :QDS-AMT-REPO-3-1-3,
      *          DSFILE.DS_AMT_REPO_3_1_4      = :QDS-AMT-REPO-3-1-4,
      *          DSFILE.DS_AMT_REPO_3_1_5      = :QDS-AMT-REPO-3-1-5,
      *          DSFILE.DS_AMT_REPO_3_2_1      = :QDS-AMT-REPO-3-2-1,
      *          DSFILE.DS_AMT_REPO_3_2_2      = :QDS-AMT-REPO-3-2-2,
      *          DSFILE.DS_AMT_REPO_3_2_3      = :QDS-AMT-REPO-3-2-3,
      *          DSFILE.DS_AMT_REPO_3_2_4      = :QDS-AMT-REPO-3-2-4,
      *          DSFILE.DS_AMT_REPO_3_2_5      = :QDS-AMT-REPO-3-2-5,
      *          DSFILE.DS_UNEARNED_AMT_1_1_1  = 
      *                :QDS-UNEARNED-AMT-1-1-1,
      *          DSFILE.DS_UNEARNED_AMT_1_1_2  = 
      *                :QDS-UNEARNED-AMT-1-1-2,
      *          DSFILE.DS_UNEARNED_AMT_1_1_3  = 
      *                :QDS-UNEARNED-AMT-1-1-3,
      *          DSFILE.DS_UNEARNED_AMT_1_1_4  = 
      *                :QDS-UNEARNED-AMT-1-1-4,
      *          DSFILE.DS_UNEARNED_AMT_1_1_5  = 
      *                :QDS-UNEARNED-AMT-1-1-5,
      *          DSFILE.DS_UNEARNED_AMT_1_1_6  = 
      *                :QDS-UNEARNED-AMT-1-1-6,
      *          DSFILE.DS_UNEARNED_AMT_1_1_7  = 
      *                :QDS-UNEARNED-AMT-1-1-7,
      *          DSFILE.DS_UNEARNED_AMT_1_1_8  = 
      *                :QDS-UNEARNED-AMT-1-1-8,
      *          DSFILE.DS_UNEARNED_AMT_1_1_9  = 
      *                :QDS-UNEARNED-AMT-1-1-9,
      *          DSFILE.DS_UNEARNED_AMT_1_1_10 = 
      *                :QDS-UNEARNED-AMT-1-1-10,
      *          DSFILE.DS_UNEARNED_AMT_1_2_1  = 
      *                :QDS-UNEARNED-AMT-1-2-1,
      *          DSFILE.DS_UNEARNED_AMT_1_2_2  = 
      *                :QDS-UNEARNED-AMT-1-2-2,
      *          DSFILE.DS_UNEARNED_AMT_1_2_3  = 
      *                :QDS-UNEARNED-AMT-1-2-3,
      *          DSFILE.DS_UNEARNED_AMT_1_2_4  = 
      *                :QDS-UNEARNED-AMT-1-2-4,
      *          DSFILE.DS_UNEARNED_AMT_1_2_5  = 
      *                :QDS-UNEARNED-AMT-1-2-5,
      *          DSFILE.DS_UNEARNED_AMT_1_2_6  = 
      *                :QDS-UNEARNED-AMT-1-2-6,
      *          DSFILE.DS_UNEARNED_AMT_1_2_7  = 
      *                :QDS-UNEARNED-AMT-1-2-7,
      *          DSFILE.DS_UNEARNED_AMT_1_2_8  = 
      *                :QDS-UNEARNED-AMT-1-2-8,
      *          DSFILE.DS_UNEARNED_AMT_1_2_9  = 
      *                :QDS-UNEARNED-AMT-1-2-9,
      *          DSFILE.DS_UNEARNED_AMT_1_2_10 = 
      *                :QDS-UNEARNED-AMT-1-2-10,
      *          DSFILE.DS_UNEARNED_AMT_2_1_1  = 
      *                :QDS-UNEARNED-AMT-2-1-1,
      *          DSFILE.DS_UNEARNED_AMT_2_1_2  = 
      *                :QDS-UNEARNED-AMT-2-1-2,
      *          DSFILE.DS_UNEARNED_AMT_2_1_3  = 
      *                :QDS-UNEARNED-AMT-2-1-3,
      *          DSFILE.DS_UNEARNED_AMT_2_1_4  = 
      *                :QDS-UNEARNED-AMT-2-1-4,
      *          DSFILE.DS_UNEARNED_AMT_2_1_5  = 
      *                :QDS-UNEARNED-AMT-2-1-5,
      *          DSFILE.DS_UNEARNED_AMT_2_1_6  = 
      *                :QDS-UNEARNED-AMT-2-1-6,
      *          DSFILE.DS_UNEARNED_AMT_2_1_7  = 
      *                :QDS-UNEARNED-AMT-2-1-7,
      *          DSFILE.DS_UNEARNED_AMT_2_1_8  = 
      *                :QDS-UNEARNED-AMT-2-1-8,
      *          DSFILE.DS_UNEARNED_AMT_2_1_9  = 
      *                :QDS-UNEARNED-AMT-2-1-9,
      *          DSFILE.DS_UNEARNED_AMT_2_1_10 = 
      *                :QDS-UNEARNED-AMT-2-1-10,
      *          DSFILE.DS_UNEARNED_AMT_2_2_1  = 
      *                :QDS-UNEARNED-AMT-2-2-1,
      *          DSFILE.DS_UNEARNED_AMT_2_2_2  = 
      *                :QDS-UNEARNED-AMT-2-2-2,
      *          DSFILE.DS_UNEARNED_AMT_2_2_3  = 
      *                :QDS-UNEARNED-AMT-2-2-3,
      *          DSFILE.DS_UNEARNED_AMT_2_2_4  = 
      *                :QDS-UNEARNED-AMT-2-2-4,
      *          DSFILE.DS_UNEARNED_AMT_2_2_5  = 
      *                :QDS-UNEARNED-AMT-2-2-5,
      *          DSFILE.DS_UNEARNED_AMT_2_2_6  = 
      *                :QDS-UNEARNED-AMT-2-2-6,
      *          DSFILE.DS_UNEARNED_AMT_2_2_7  = 
      *                :QDS-UNEARNED-AMT-2-2-7,
      *          DSFILE.DS_UNEARNED_AMT_2_2_8  = 
      *                :QDS-UNEARNED-AMT-2-2-8,
      *          DSFILE.DS_UNEARNED_AMT_2_2_9  = 
      *                :QDS-UNEARNED-AMT-2-2-9,
      *          DSFILE.DS_UNEARNED_AMT_2_2_10 = 
      *                :QDS-UNEARNED-AMT-2-2-10,
      *          DSFILE.DS_UNEARNED_AMT_3_1_1  = 
      *                :QDS-UNEARNED-AMT-3-1-1,
      *          DSFILE.DS_UNEARNED_AMT_3_1_2  = 
      *                :QDS-UNEARNED-AMT-3-1-2,
      *          DSFILE.DS_UNEARNED_AMT_3_1_3  = 
      *                :QDS-UNEARNED-AMT-3-1-3,
      *          DSFILE.DS_UNEARNED_AMT_3_1_4  = 
      *                :QDS-UNEARNED-AMT-3-1-4,
      *          DSFILE.DS_UNEARNED_AMT_3_1_5  = 
      *                :QDS-UNEARNED-AMT-3-1-5,
      *          DSFILE.DS_UNEARNED_AMT_3_1_6  = 
      *                :QDS-UNEARNED-AMT-3-1-6,
      *          DSFILE.DS_UNEARNED_AMT_3_1_7  = 
      *                :QDS-UNEARNED-AMT-3-1-7,
      *          DSFILE.DS_UNEARNED_AMT_3_1_8  = 
      *                :QDS-UNEARNED-AMT-3-1-8,
      *          DSFILE.DS_UNEARNED_AMT_3_1_9  = 
      *                :QDS-UNEARNED-AMT-3-1-9,
      *          DSFILE.DS_UNEARNED_AMT_3_1_10 = 
      *                :QDS-UNEARNED-AMT-3-1-10,
      *          DSFILE.DS_UNEARNED_AMT_3_2_1  = 
      *                :QDS-UNEARNED-AMT-3-2-1,
      *          DSFILE.DS_UNEARNED_AMT_3_2_2  = 
      *                :QDS-UNEARNED-AMT-3-2-2,
      *          DSFILE.DS_UNEARNED_AMT_3_2_3  = 
      *                :QDS-UNEARNED-AMT-3-2-3,
      *          DSFILE.DS_UNEARNED_AMT_3_2_4  = 
      *                :QDS-UNEARNED-AMT-3-2-4,
      *          DSFILE.DS_UNEARNED_AMT_3_2_5  = 
      *                :QDS-UNEARNED-AMT-3-2-5,
      *          DSFILE.DS_UNEARNED_AMT_3_2_6  = 
      *                :QDS-UNEARNED-AMT-3-2-6,
      *          DSFILE.DS_UNEARNED_AMT_3_2_7  = 
      *                :QDS-UNEARNED-AMT-3-2-7,
      *          DSFILE.DS_UNEARNED_AMT_3_2_8  = 
      *                :QDS-UNEARNED-AMT-3-2-8,
      *          DSFILE.DS_UNEARNED_AMT_3_2_9  = 
      *                :QDS-UNEARNED-AMT-3-2-9,
      *          DSFILE.DS_UNEARNED_AMT_3_2_10 = 
      *                :QDS-UNEARNED-AMT-3-2-10,
      *          DSFILE.DS_DLDISC_VOL_1        = :QDS-DLDISC-VOL-1,
      *          DSFILE.DS_DLDISC_VOL_2        = :QDS-DLDISC-VOL-2,
      *          DSFILE.DS_DLDISC_VOL_3        = :QDS-DLDISC-VOL-3,
      *          DSFILE.DS_LTOUCH_DATE         = :QDS-LTOUCH-DATE 
      *      WHERE DSFILE.DS_DATE_CCYYMM = :QDS-DATE-CCYYMM
      *        AND DSFILE.DS_BRANCH      = :QDS-BRANCH
      *        AND DSFILE.DS_NUM         = :QDS-NUM
      *        AND DSFILE.DS_CLASS       = :QDS-CLASS
      *    END-EXEC.

      *    PERFORM SQL-IO-VALIDATION.

      *    IF ( IO-FG = 8 )     *> LOCKED
      *       GO TO REWRITE-DS1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *    MOVE '1' TO SQL-UPDATE-CONN-USE-SW. *> USE C1 CONNECT
      *    PERFORM SQL-SWAP-UPDATE-CONNECTION.

      *-----------------------------------------------------------------
      *CLOSE-DS1-FILE.

      *    PERFORM CLOSE-IT.

      *    IF ( DS1-CURSOR-STAT-GOOD )
      *       EXEC SQL SET CONNECTION DS1_CONNECT END-EXEC
      *       EXEC SQL
      *            CLOSE DS1_CURSOR
      *       END-EXEC
      *       EXEC SQL SET CONNECTION C1 END-EXEC
      *       MOVE STAT---BAD TO DS1-CURSOR-STAT.

      *-----------------------------------------------------------------
      *CLOSE-DS1-CONNECTION.

      *    IF DS1-CONN-OPEN-SW = "Y"
      *       EXEC SQL SET CONNECTION DS1_CONNECT END-EXEC
      *       EXEC SQL COMMIT END-EXEC
      *       EXEC SQL DISCONNECT DS1_CONNECT END-EXEC
      *       EXEC SQL SET CONNECTION C1 END-EXEC
      *       MOVE "N" TO DS1-CONN-OPEN-SW.
   
 
      * COPY "LIBLP/LPLN1IN.CPY".
      * COPYMEMBER: LIBLP/LPLN1IN
      ************************************************************
      *                (LPLN1IN)
      * REV:
      *  JTG 081497 CHANGED LN-SALEFIN TO LN-GIFTNET WORLD A90
      *  BLV 020698 ADDED LN-SALEFIN BACK TO BICREA CALL
      *  MJD 030398 ADDED LN-PLCD AND LN-CONVERCD TO BICREA CALL
      *  BAH 121023 ADDED FILPATH
      *  BAH 04/22/2016 ADDED LN-SMALLOAN TO BICREA CALL, REGENCY PR#5030
      *  BAH 20180316 ADDED LN-PLDATE TO BICREA CALL, WORLD #1308
      *  BAH 181130 GLOBAL LNFILE
      ************************************************************
      *UPDATE-BI-FILE.
      *    IF IO-FG = 0
      *       MOVE "CL/BICREA" TO FORM-NAM
      *       CALL FORM-PROGX USING FORM-PATH
      *                            STAT
      *                            LN-ACCTNO
      *                            LN-BWTABLE
      *                            LN-OWNBR
      *                            LN-ORGST
      *                            LN-SALEFIN
      *                            LN-GIFTNET
      *                            LN-ENTDATE
      *                            LN-POFFDATE
      *                            LN-PLCD
      *                            LN-CONVERCD
      *                            "000000000"
      *                            LN-SMALLOAN
      *                            LN-PLDATE
      *      CANCEL FORM-PROGX.
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

           IF ( QLN1-WEND-ACCTNO = ZEROES    ) OR
              ( QLN1-WEND-ACCTNO NOT NUMERIC )
              MOVE LN-OWNBR             TO QLN1-WBEG-OWNBR
              MOVE LN-OWNBR             TO QLN1-WEND-OWNBR
              MOVE LN-ACCTNO            TO QLN1-WBEG-ACCTNO
              MOVE ALL "9"              TO QLN1-WEND-ACCTNO.

      *  STORED PROCEDURE:
      *
      *   (SELECT LNFILE.LN_OWNBR,
      *           LNFILE.LN_ACCTNO,
      *           LNFILE.LN_POCD,
      *           LNFILE.LN_YTDINTPD,
      *           LNFILE.LN_LYRINTPD 
      *  
      *      FROM DBO.LNFILE LNFILE
      *  
      *     WHERE LNFILE.LN_OWNBR        = @QLN1_WBEG_OWNBR
      *       AND LNFILE.LN_ACCTNO BETWEEN @QLN1_WBEG_ACCTNO
      *                                AND @QLN1_WEND_ACCTNO
      *       AND LNFILE.LN_POCD    NOT IN ('**', '*I', '*R', '*U') ) TEMP;

           EXEC SQL SET CONNECTION LN1_CONNECT END-EXEC.

           EXEC SQL
            DECLARE LN1_CURSOR CURSOR FOR
                :LN1-RETURN-CODE = EXEC SSP_EOYUPD_LNFILE_SELECT 
              ( :QLN1-WBEG-OWNBR,   
                :QLN1-WBEG-ACCTNO,  
                :QLN1-WEND-ACCTNO  )      
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
                    :QLN-POCD,
                    :QLN-YTDINTPD,
                    :QLN-LYRINTPD                       
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
       REWRITE-LN1-FILE.

           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           ACCEPT LN-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE LN-PATH-SQL    TO E-FILE.
           MOVE LN1-KEY        TO E-KEYX.
           PERFORM SET-LN-FIELDS.

           MOVE '2' TO SQL-UPDATE-CONN-USE-SW. *> USE UPDATE_CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

           EXEC SQL
            UPDATE DBO.LNFILE
              SET
                LNFILE.LN_LYRINTPD    = LNFILE.LN_YTDINTPD,
                LNFILE.LN_LTOUCH_DATE = :QLN-LTOUCH-DATE
            WHERE LNFILE.LN_OWNBR     = :QLN-OWNBR
              AND LNFILE.LN_POCD    NOT IN ('**', '*I', '*R', '*U')
           END-EXEC.
           PERFORM SQL-IO-VALIDATION.

           EXEC SQL
            UPDATE DBO.LNFILE
              SET
                LNFILE.LN_YTDINTPD    = 0,
                LNFILE.LN_LTOUCH_DATE = :QLN-LTOUCH-DATE
            WHERE LNFILE.LN_OWNBR     = :QLN-OWNBR
              AND LNFILE.LN_POCD    NOT IN ('**', '*I', '*R', '*U')
           END-EXEC.
           PERFORM SQL-IO-VALIDATION.

      *    IF ( IO-FG = 8 )     *> LOCKED
      *       GO TO REWRITE-LN1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION

      * RMZ  04-16-2025: COMMENTED OUT THE UPDATE-BI-FILE BECAUSE UPDATING
      *                  THE ABOVE UPDATED COLUMNS WOULD NOT EFFECT BIFILE.
      *
      *      IF LN-BI-FG = "Y"
      *         MOVE "R" TO STAT
      *         PERFORM UPDATE-BI-FILE.

           MOVE '1' TO SQL-UPDATE-CONN-USE-SW. *> USE C1 CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

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


      * COPY "LIBLP/LPBW1IN.CPY".
      * COPYMEMBER: LIBLP/LPBW1IN
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

           IF BW1-CONN-OPEN-SW = "N" 
              MOVE "Y"         TO BW1-CONN-OPEN-SW
              EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS BW1_CONNECT 
                       USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
              END-EXEC.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       START-BW1-FILE. 
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

      *  STORED PROCEDURE:
      *
      *   (SELECT BWFILE.BW_OWNBR,
      *           BWFILE.BW_SSNO,
      *           BWFILE.BW_INTYTD,
      *           BWFILE.BW_INTLYR,
      *           BWFILE.BW_RE_INTYTD,
      *           BWFILE.BW_RE_INTLYR 
      * 
      *      FROM DBO.BWFILE BWFILE
      * 
      *     WHERE BWFILE.BW_OWNBR      = @QBW1_WBEG_OWNBR
      *       AND BWFILE.BW_SSNO BETWEEN @QBW1_WBEG_SSNO
      *                              AND @QBW1_WEND_SSNO ) TEMP;

           EXEC SQL SET CONNECTION BW1_CONNECT END-EXEC.

           EXEC SQL
            DECLARE BW1_CURSOR CURSOR FOR
                :BW1-RETURN-CODE = EXEC SSP_EOYUPD_BWFILE_SELECT 
              ( :QBW1-WBEG-OWNBR,  
                :QBW1-WBEG-SSNO,   
                :QBW1-WEND-SSNO  )      
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
       READ-BW1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE BW-PATH-OWNBR  TO BW-OWNBR.
           MOVE BW-PATH-SQL    TO E-FILE.
           MOVE BW1-KEY        TO E-KEYX.
           INITIALIZE QBW-REC.

           EXEC SQL SET CONNECTION BW1_CONNECT END-EXEC.

           IF ( BW1-CURSOR-STAT-GOOD )
              EXEC SQL
                FETCH BW1_CURSOR 
                  INTO  
                    :QBW-OWNBR,
                    :QBW-SSNO,
                    :QBW-INTYTD,
                    :QBW-INTLYR,
                    :QBW-RE-INTYTD,
                    :QBW-RE-INTLYR 
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-BW1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-BW-FIELDS.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       REWRITE-BW1-FILE.

           MOVE BW-PATH-OWNBR TO BW-OWNBR. 
           ACCEPT BW-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE BW-PATH-SQL TO E-FILE.

      *    MOVE SPACES         TO BW-VISION-KEY2
      *                           BW-VISION-KEY3
      *                           BW-VISION-KEY4.
      *    STRING BW-OWNBR, BW-LNAME, BW-FNAME
      *           DELIMITED BY SIZE INTO BW-VISION-KEY2.
      *    STRING BW-OWNBR, BW-SPSSNO
      *           DELIMITED BY SIZE INTO BW-VISION-KEY3.
      *    STRING BW-OWNBR, BW-SPLNAME, BW-SPFNAME
      *           DELIMITED BY SIZE INTO BW-VISION-KEY4.
      *    MOVE BW1-KEY        TO E-KEYX BW-VISION-KEY1.

           MOVE BW1-KEY        TO E-KEYX.
           PERFORM SET-BW-FIELDS.

           MOVE '2' TO SQL-UPDATE-CONN-USE-SW. *> USE UPDATE_CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

           EXEC SQL
            UPDATE DBO.BWFILE
              SET 
                BWFILE.BW_INTLYR      = BWFILE.BW_INTYTD,   
                BWFILE.BW_RE_INTLYR   = BWFILE.BW_RE_INTYTD,
                BWFILE.BW_LTOUCH_DATE = :QBW-LTOUCH-DATE
            WHERE BWFILE.BW_OWNBR     = :QBW-OWNBR
           END-EXEC.
           PERFORM SQL-IO-VALIDATION.

           EXEC SQL
            UPDATE DBO.BWFILE
              SET
                BWFILE.BW_INTYTD      = 0,      
                BWFILE.BW_RE_INTYTD   = 0,
                BWFILE.BW_LTOUCH_DATE = :QBW-LTOUCH-DATE
            WHERE BWFILE.BW_OWNBR     = :QBW-OWNBR
           END-EXEC.
           PERFORM SQL-IO-VALIDATION.

      *    IF ( IO-FG = 8 )     *> LOCKED
      *       GO TO REWRITE-BW1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

           MOVE '1' TO SQL-UPDATE-CONN-USE-SW. *> USE C1 CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

      *-----------------------------------------------------------------
       CLOSE-BW1-FILE.

           PERFORM CLOSE-IT.

           IF ( BW1-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION BW1_CONNECT END-EXEC
              EXEC SQL
                   CLOSE BW1_CURSOR
              END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE STAT---BAD TO BW1-CURSOR-STAT.

      *-----------------------------------------------------------------
       CLOSE-BW1-CONNECTION.

           IF BW1-CONN-OPEN-SW = "Y"
              EXEC SQL SET CONNECTION BW1_CONNECT END-EXEC
              EXEC SQL COMMIT END-EXEC
              EXEC SQL DISCONNECT BW1_CONNECT END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE "N" TO BW1-CONN-OPEN-SW.


      * COPY "LIBLP/LPES1IN.CPY".
      * COPYMEMBER: LIBLP/LPES1IN
      *LOAD-ES1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
      *    IF ( ES-PATH-OVERRIDE = "Y" ) 
      *       NEXT SENTENCE
      *    ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
      *    IF ( ES-PATH-OVERRIDE = SPACES ) 
      *       MOVE EXT-FILPATH-BASE        TO ES-ALL-BASE
      *       MOVE EXT-FILPATH-MACHINE     TO ES-ALL-MACHINE
      *       MOVE EXT-FILPATH-DATA        TO ES-ALL-DATA
      *       MOVE ES-ALL-PATH             TO ES-PATH
      *    ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
      *    IF ( ES-PATH-OVERRIDE = "B" ) 
      *       MOVE FILPATH-BASE            TO ES-ALL-BASE
      *       MOVE FILPATH-MACHINE         TO ES-ALL-MACHINE
      *       MOVE FILPATH-DATA            TO ES-ALL-DATA
      *       MOVE ES-ALL-PATH             TO ES-PATH.

      *-----------------------------------------------------------------
      *OPEN-ES1-FILE.
      *    PERFORM LOAD-ES1-FILE.
      *    PERFORM OPEN-IT.
      *    MOVE ES-PATH-SQL TO E-FILE.

      *    IF ES1-CONN-OPEN-SW = "N" 
      *       MOVE "Y"         TO ES1-CONN-OPEN-SW
      *       EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS ES1_CONNECT 
      *                USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
      *       END-EXEC.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *    EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
      *START-ES1-FILE.
      *    PERFORM START-IT.
      *    MOVE ES-PATH-OWNBR  TO ES-BRNO.
      *    MOVE ES-PATH-SQL    TO E-FILE.
      *    MOVE ES1-KEY        TO E-KEYX.

      *    IF ( ES1-CURSOR-STAT-GOOD )
      *       PERFORM CLOSE-ES1-FILE.

      *    IF ( QES1-WEND-ACCTNO = ZEROES    ) OR
      *       ( QES1-WEND-ACCTNO NOT NUMERIC )
      *       MOVE ES-PATH-OWNBR        TO QES1-WBEG-BRNO
      *       MOVE ES-PATH-OWNBR        TO QES1-WEND-BRNO
      *       MOVE ES-ACCTNO            TO QES1-WBEG-ACCTNO
      *       MOVE ALL "9"              TO QES1-WEND-ACCTNO.

      *    EXEC SQL SET CONNECTION ES1_CONNECT END-EXEC.

      *    EXEC SQL
      *     DECLARE ES1_CURSOR CURSOR FOR
      *         :ES1-RETURN-CODE = EXEC SSP_UTNOFL_ESFILE_SELECT 
      *       ( :QES1-WBEG-BRNO,  
      *         :QES1-WBEG-ACCTNO, 
      *         :QES1-WEND-ACCTNO )
      *    END-EXEC.

      *    EXEC SQL
      *        OPEN ES1_CURSOR
      *    END-EXEC.

      *    PERFORM SQL-IO-VALIDATION.

      *    MOVE ZEROES TO QES1-WBEG-BRNO 
      *                   QES1-WEND-BRNO
      *                   QES1-WBEG-ACCTNO
      *                   QES1-WEND-ACCTNO.

      *    MOVE STAT---GOOD TO ES1-CURSOR-STAT.

      *    EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
      *READ-ES1-FILE-NEXT.
      *    PERFORM READ-IT.
      *    MOVE ES-PATH-OWNBR  TO ES-BRNO.
      *    MOVE ES-PATH-SQL    TO E-FILE.
      *    MOVE ES1-KEY        TO E-KEYX.
      *    INITIALIZE QES-REC.

      *    EXEC SQL SET CONNECTION ES1_CONNECT END-EXEC.

      *    IF ( ES1-CURSOR-STAT-GOOD )
      *       EXEC SQL
      *            FETCH ES1_CURSOR INTO :QES-REC
      *       END-EXEC
      *    ELSE
      *       MOVE 9        TO SQLCODE
      *       MOVE "RDNXCR" TO SQLSTATE.

      *    PERFORM SQL-IO-VALIDATION.

      *    IF ( IO-FG = 8 )     *>LOCKED
      *       GO TO READ-ES1-FILE-NEXT.

      *    IF ( IO-FG = 0 )
      *       PERFORM GET-ES-FIELDS.

      *    EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
      *REWRITE-ES1-FILE.

      *    MOVE ES-PATH-OWNBR  TO ES-BRNO.
      *    ACCEPT ES-LTOUCH-DATE FROM CENTURY-DATE.
      *    PERFORM REWRITE-IT.
      *    MOVE ES-PATH-SQL    TO E-FILE.
      *    MOVE ES1-KEY        TO E-KEYX.
      *    PERFORM SET-ES-FIELDS.

      *    MOVE '2' TO SQL-UPDATE-CONN-USE-SW. *> USE UPDATE_CONNECT
      *    PERFORM SQL-SWAP-UPDATE-CONNECTION.

      *    EXEC SQL
      *     UPDATE DBO.ESFILE
      *      SET
      *       ESFILE.ES_BRNO = :QES-BRNO,
      *       ESFILE.ES_ACCTNO = :QES-ACCTNO,
      *       ESFILE.ES_LTOUCH_DATE =
      *          :QES-LTOUCH-DATE,
      *       ESFILE.ES_SCHD_EFF_DATE_1 =
      *          :QES-SCHD-EFF-DATE-1,
      *       ESFILE.ES_SCHD_1STPYDATE_1 =
      *          :QES-SCHD-1STPYDATE-1,
      *       ESFILE.ES_SCHD_ESCROW_1 = :QES-SCHD-ESCROW-1,
      *       ESFILE.ES_SCHD_TERM_1 = :QES-SCHD-TERM-1,
      *       ESFILE.ES_SCHD_COLLECTED_1 = :QES-SCHD-COLLECTED-1,
      *       ESFILE.ES_SCHD_EFF_DATE_2 =
      *          :QES-SCHD-EFF-DATE-2,
      *       ESFILE.ES_SCHD_1STPYDATE_2 =
      *          :QES-SCHD-1STPYDATE-2,
      *       ESFILE.ES_SCHD_ESCROW_2 = :QES-SCHD-ESCROW-2,
      *       ESFILE.ES_SCHD_TERM_2 = :QES-SCHD-TERM-2,
      *       ESFILE.ES_SCHD_COLLECTED_2 = :QES-SCHD-COLLECTED-2,
      *       ESFILE.ES_SCHD_EFF_DATE_3 =
      *          :QES-SCHD-EFF-DATE-3,
      *       ESFILE.ES_SCHD_1STPYDATE_3 =
      *          :QES-SCHD-1STPYDATE-3,
      *       ESFILE.ES_SCHD_ESCROW_3 = :QES-SCHD-ESCROW-3,
      *       ESFILE.ES_SCHD_TERM_3 = :QES-SCHD-TERM-3,
      *       ESFILE.ES_SCHD_COLLECTED_3 = :QES-SCHD-COLLECTED-3,
      *       ESFILE.ES_SCHD_EFF_DATE_4 =
      *          :QES-SCHD-EFF-DATE-4,
      *       ESFILE.ES_SCHD_1STPYDATE_4 =
      *          :QES-SCHD-1STPYDATE-4,
      *       ESFILE.ES_SCHD_ESCROW_4 = :QES-SCHD-ESCROW-4,
      *       ESFILE.ES_SCHD_TERM_4 = :QES-SCHD-TERM-4,
      *       ESFILE.ES_SCHD_COLLECTED_4 = :QES-SCHD-COLLECTED-4,
      *       ESFILE.ES_SCHD_EFF_DATE_5 =
      *          :QES-SCHD-EFF-DATE-5,
      *       ESFILE.ES_SCHD_1STPYDATE_5 =
      *          :QES-SCHD-1STPYDATE-5,
      *       ESFILE.ES_SCHD_ESCROW_5 = :QES-SCHD-ESCROW-5,
      *       ESFILE.ES_SCHD_TERM_5 = :QES-SCHD-TERM-5,
      *       ESFILE.ES_SCHD_COLLECTED_5 = :QES-SCHD-COLLECTED-5,
      *       ESFILE.ES_SCHD_EFF_DATE_6 =
      *          :QES-SCHD-EFF-DATE-6,
      *       ESFILE.ES_SCHD_1STPYDATE_6 =
      *          :QES-SCHD-1STPYDATE-6,
      *       ESFILE.ES_SCHD_ESCROW_6 = :QES-SCHD-ESCROW-6,
      *       ESFILE.ES_SCHD_TERM_6 = :QES-SCHD-TERM-6,
      *       ESFILE.ES_SCHD_COLLECTED_6 = :QES-SCHD-COLLECTED-6,
      *       ESFILE.ES_SCHD_EFF_DATE_7 =
      *          :QES-SCHD-EFF-DATE-7,
      *       ESFILE.ES_SCHD_1STPYDATE_7 =
      *          :QES-SCHD-1STPYDATE-7,
      *       ESFILE.ES_SCHD_ESCROW_7 = :QES-SCHD-ESCROW-7,
      *       ESFILE.ES_SCHD_TERM_7 = :QES-SCHD-TERM-7,
      *       ESFILE.ES_SCHD_COLLECTED_7 = :QES-SCHD-COLLECTED-7,
      *       ESFILE.ES_SCHD_EFF_DATE_8 =
      *          :QES-SCHD-EFF-DATE-8,
      *       ESFILE.ES_SCHD_1STPYDATE_8 =
      *          :QES-SCHD-1STPYDATE-8,
      *       ESFILE.ES_SCHD_ESCROW_8 = :QES-SCHD-ESCROW-8,
      *       ESFILE.ES_SCHD_TERM_8 = :QES-SCHD-TERM-8,
      *       ESFILE.ES_SCHD_COLLECTED_8 = :QES-SCHD-COLLECTED-8,
      *       ESFILE.ES_SCHD_EFF_DATE_9 =
      *          :QES-SCHD-EFF-DATE-9,
      *       ESFILE.ES_SCHD_1STPYDATE_9 =
      *          :QES-SCHD-1STPYDATE-9,
      *       ESFILE.ES_SCHD_ESCROW_9 = :QES-SCHD-ESCROW-9,
      *       ESFILE.ES_SCHD_TERM_9 = :QES-SCHD-TERM-9,
      *       ESFILE.ES_SCHD_COLLECTED_9 = :QES-SCHD-COLLECTED-9,
      *       ESFILE.ES_SCHD_EFF_DATE_10 =
      *          :QES-SCHD-EFF-DATE-10,
      *       ESFILE.ES_SCHD_1STPYDATE_10 =
      *          :QES-SCHD-1STPYDATE-10,
      *       ESFILE.ES_SCHD_ESCROW_10 = :QES-SCHD-ESCROW-10,
      *       ESFILE.ES_SCHD_TERM_10 = :QES-SCHD-TERM-10,
      *       ESFILE.ES_SCHD_COLLECTED_10 =
      *            :QES-SCHD-COLLECTED-10,
      *       ESFILE.ES_SCHD_EFF_DATE_11 =
      *          :QES-SCHD-EFF-DATE-11,
      *       ESFILE.ES_SCHD_1STPYDATE_11 =
      *          :QES-SCHD-1STPYDATE-11,
      *       ESFILE.ES_SCHD_ESCROW_11 = :QES-SCHD-ESCROW-11,
      *       ESFILE.ES_SCHD_TERM_11 = :QES-SCHD-TERM-11,
      *       ESFILE.ES_SCHD_COLLECTED_11 =
      *            :QES-SCHD-COLLECTED-11,
      *       ESFILE.ES_SCHD_EFF_DATE_12 =
      *          :QES-SCHD-EFF-DATE-12,
      *       ESFILE.ES_SCHD_1STPYDATE_12 =
      *          :QES-SCHD-1STPYDATE-12,
      *       ESFILE.ES_SCHD_ESCROW_12 = :QES-SCHD-ESCROW-12,
      *       ESFILE.ES_SCHD_TERM_12 = :QES-SCHD-TERM-12,
      *       ESFILE.ES_SCHD_COLLECTED_12 =
      *            :QES-SCHD-COLLECTED-12,
      *       ESFILE.ES_SCHD_EFF_DATE_13 =
      *          :QES-SCHD-EFF-DATE-13,
      *       ESFILE.ES_SCHD_1STPYDATE_13 =
      *          :QES-SCHD-1STPYDATE-13,
      *       ESFILE.ES_SCHD_ESCROW_13 = :QES-SCHD-ESCROW-13,
      *       ESFILE.ES_SCHD_TERM_13 = :QES-SCHD-TERM-13,
      *       ESFILE.ES_SCHD_COLLECTED_13 =
      *            :QES-SCHD-COLLECTED-13,
      *       ESFILE.ES_SCHD_EFF_DATE_14 =
      *          :QES-SCHD-EFF-DATE-14,
      *       ESFILE.ES_SCHD_1STPYDATE_14 =
      *          :QES-SCHD-1STPYDATE-14,
      *       ESFILE.ES_SCHD_ESCROW_14 = :QES-SCHD-ESCROW-14,
      *       ESFILE.ES_SCHD_TERM_14 = :QES-SCHD-TERM-14,
      *       ESFILE.ES_SCHD_COLLECTED_14 =
      *            :QES-SCHD-COLLECTED-14,
      *       ESFILE.ES_SCHD_EFF_DATE_15 =
      *          :QES-SCHD-EFF-DATE-15,
      *       ESFILE.ES_SCHD_1STPYDATE_15 =
      *          :QES-SCHD-1STPYDATE-15,
      *       ESFILE.ES_SCHD_ESCROW_15 = :QES-SCHD-ESCROW-15,
      *       ESFILE.ES_SCHD_TERM_15 = :QES-SCHD-TERM-15,
      *       ESFILE.ES_SCHD_COLLECTED_15 =
      *            :QES-SCHD-COLLECTED-15,
      *       ESFILE.ES_SCHD_EFF_DATE_16 =
      *          :QES-SCHD-EFF-DATE-16,
      *       ESFILE.ES_SCHD_1STPYDATE_16 =
      *          :QES-SCHD-1STPYDATE-16,
      *       ESFILE.ES_SCHD_ESCROW_16 = :QES-SCHD-ESCROW-16,
      *       ESFILE.ES_SCHD_TERM_16 = :QES-SCHD-TERM-16,
      *       ESFILE.ES_SCHD_COLLECTED_16 =
      *            :QES-SCHD-COLLECTED-16,
      *       ESFILE.ES_SCHD_EFF_DATE_17 =
      *          :QES-SCHD-EFF-DATE-17,
      *       ESFILE.ES_SCHD_1STPYDATE_17 =
      *          :QES-SCHD-1STPYDATE-17,
      *       ESFILE.ES_SCHD_ESCROW_17 = :QES-SCHD-ESCROW-17,
      *       ESFILE.ES_SCHD_TERM_17 = :QES-SCHD-TERM-17,
      *       ESFILE.ES_SCHD_COLLECTED_17 =
      *            :QES-SCHD-COLLECTED-17,
      *       ESFILE.ES_SCHD_EFF_DATE_18 =
      *          :QES-SCHD-EFF-DATE-18,
      *       ESFILE.ES_SCHD_1STPYDATE_18 =
      *          :QES-SCHD-1STPYDATE-18,
      *       ESFILE.ES_SCHD_ESCROW_18 = :QES-SCHD-ESCROW-18,
      *       ESFILE.ES_SCHD_TERM_18 = :QES-SCHD-TERM-18,
      *       ESFILE.ES_SCHD_COLLECTED_18 =
      *            :QES-SCHD-COLLECTED-18,
      *       ESFILE.ES_SCHD_EFF_DATE_19 =
      *          :QES-SCHD-EFF-DATE-19,
      *       ESFILE.ES_SCHD_1STPYDATE_19 =
      *          :QES-SCHD-1STPYDATE-19,
      *       ESFILE.ES_SCHD_ESCROW_19 = :QES-SCHD-ESCROW-19,
      *       ESFILE.ES_SCHD_TERM_19 = :QES-SCHD-TERM-19,
      *       ESFILE.ES_SCHD_COLLECTED_19 =
      *            :QES-SCHD-COLLECTED-19,
      *       ESFILE.ES_SCHD_EFF_DATE_20 =
      *          :QES-SCHD-EFF-DATE-20,
      *       ESFILE.ES_SCHD_1STPYDATE_20 =
      *          :QES-SCHD-1STPYDATE-20,
      *       ESFILE.ES_SCHD_ESCROW_20 = :QES-SCHD-ESCROW-20,
      *       ESFILE.ES_SCHD_TERM_20 = :QES-SCHD-TERM-20,
      *       ESFILE.ES_SCHD_COLLECTED_20 =
      *            :QES-SCHD-COLLECTED-20,
      *       ESFILE.ES_SCHD_EFF_DATE_21 =
      *          :QES-SCHD-EFF-DATE-21,
      *       ESFILE.ES_SCHD_1STPYDATE_21 =
      *          :QES-SCHD-1STPYDATE-21,
      *       ESFILE.ES_SCHD_ESCROW_21 = :QES-SCHD-ESCROW-21,
      *       ESFILE.ES_SCHD_TERM_21 = :QES-SCHD-TERM-21,
      *       ESFILE.ES_SCHD_COLLECTED_21 =
      *            :QES-SCHD-COLLECTED-21,
      *       ESFILE.ES_SCHD_EFF_DATE_22 =
      *          :QES-SCHD-EFF-DATE-22,
      *       ESFILE.ES_SCHD_1STPYDATE_22 =
      *          :QES-SCHD-1STPYDATE-22,
      *       ESFILE.ES_SCHD_ESCROW_22 = :QES-SCHD-ESCROW-22,
      *       ESFILE.ES_SCHD_TERM_22 = :QES-SCHD-TERM-22,
      *       ESFILE.ES_SCHD_COLLECTED_22 =
      *            :QES-SCHD-COLLECTED-22,
      *       ESFILE.ES_SCHD_EFF_DATE_23 =
      *          :QES-SCHD-EFF-DATE-23,
      *       ESFILE.ES_SCHD_1STPYDATE_23 =
      *          :QES-SCHD-1STPYDATE-23,
      *       ESFILE.ES_SCHD_ESCROW_23 = :QES-SCHD-ESCROW-23,
      *       ESFILE.ES_SCHD_TERM_23 = :QES-SCHD-TERM-23,
      *       ESFILE.ES_SCHD_COLLECTED_23 =
      *            :QES-SCHD-COLLECTED-23,
      *       ESFILE.ES_SCHD_EFF_DATE_24 =
      *          :QES-SCHD-EFF-DATE-24,
      *       ESFILE.ES_SCHD_1STPYDATE_24 =
      *          :QES-SCHD-1STPYDATE-24,
      *       ESFILE.ES_SCHD_ESCROW_24 = :QES-SCHD-ESCROW-24,
      *       ESFILE.ES_SCHD_TERM_24 = :QES-SCHD-TERM-24,
      *       ESFILE.ES_SCHD_COLLECTED_24 =
      *            :QES-SCHD-COLLECTED-24,
      *       ESFILE.ES_SCHD_EFF_DATE_25 =
      *          :QES-SCHD-EFF-DATE-25,
      *       ESFILE.ES_SCHD_1STPYDATE_25 =
      *          :QES-SCHD-1STPYDATE-25,
      *       ESFILE.ES_SCHD_ESCROW_25 = :QES-SCHD-ESCROW-25,
      *       ESFILE.ES_SCHD_TERM_25 = :QES-SCHD-TERM-25,
      *       ESFILE.ES_SCHD_COLLECTED_25 =
      *            :QES-SCHD-COLLECTED-25,
      *       ESFILE.ES_SCHD_EFF_DATE_26 =
      *          :QES-SCHD-EFF-DATE-26,
      *       ESFILE.ES_SCHD_1STPYDATE_26 =
      *          :QES-SCHD-1STPYDATE-26,
      *       ESFILE.ES_SCHD_ESCROW_26 = :QES-SCHD-ESCROW-26,
      *       ESFILE.ES_SCHD_TERM_26 = :QES-SCHD-TERM-26,
      *       ESFILE.ES_SCHD_COLLECTED_26 =
      *            :QES-SCHD-COLLECTED-26,
      *       ESFILE.ES_SCHD_EFF_DATE_27 =
      *          :QES-SCHD-EFF-DATE-27,
      *       ESFILE.ES_SCHD_1STPYDATE_27 =
      *          :QES-SCHD-1STPYDATE-27,
      *       ESFILE.ES_SCHD_ESCROW_27 = :QES-SCHD-ESCROW-27,
      *       ESFILE.ES_SCHD_TERM_27 = :QES-SCHD-TERM-27,
      *       ESFILE.ES_SCHD_COLLECTED_27 =
      *            :QES-SCHD-COLLECTED-27,
      *       ESFILE.ES_SCHD_EFF_DATE_28 =
      *          :QES-SCHD-EFF-DATE-28,
      *       ESFILE.ES_SCHD_1STPYDATE_28 =
      *          :QES-SCHD-1STPYDATE-28,
      *       ESFILE.ES_SCHD_ESCROW_28 = :QES-SCHD-ESCROW-28,
      *       ESFILE.ES_SCHD_TERM_28 = :QES-SCHD-TERM-28,
      *       ESFILE.ES_SCHD_COLLECTED_28 =
      *            :QES-SCHD-COLLECTED-28,
      *       ESFILE.ES_SCHD_EFF_DATE_29 =
      *          :QES-SCHD-EFF-DATE-29,
      *       ESFILE.ES_SCHD_1STPYDATE_29 =
      *          :QES-SCHD-1STPYDATE-29,
      *       ESFILE.ES_SCHD_ESCROW_29 = :QES-SCHD-ESCROW-29,
      *       ESFILE.ES_SCHD_TERM_29 = :QES-SCHD-TERM-29,
      *       ESFILE.ES_SCHD_COLLECTED_29 =
      *            :QES-SCHD-COLLECTED-29,
      *       ESFILE.ES_SCHD_EFF_DATE_30 =
      *          :QES-SCHD-EFF-DATE-30,
      *       ESFILE.ES_SCHD_1STPYDATE_30 =
      *          :QES-SCHD-1STPYDATE-30,
      *       ESFILE.ES_SCHD_ESCROW_30 = :QES-SCHD-ESCROW-30,
      *       ESFILE.ES_SCHD_TERM_30 = :QES-SCHD-TERM-30,
      *       ESFILE.ES_SCHD_COLLECTED_30 =
      *            :QES-SCHD-COLLECTED-30,
      *       ESFILE.ES_SCHD_EFF_DATE_31 =
      *          :QES-SCHD-EFF-DATE-31,
      *       ESFILE.ES_SCHD_1STPYDATE_31 =
      *          :QES-SCHD-1STPYDATE-31,
      *       ESFILE.ES_SCHD_ESCROW_31 = :QES-SCHD-ESCROW-31,
      *       ESFILE.ES_SCHD_TERM_31 = :QES-SCHD-TERM-31,
      *       ESFILE.ES_SCHD_COLLECTED_31 =
      *            :QES-SCHD-COLLECTED-31,
      *       ESFILE.ES_SCHD_EFF_DATE_32 =
      *          :QES-SCHD-EFF-DATE-32,
      *       ESFILE.ES_SCHD_1STPYDATE_32 =
      *          :QES-SCHD-1STPYDATE-32,
      *       ESFILE.ES_SCHD_ESCROW_32 = :QES-SCHD-ESCROW-32,
      *       ESFILE.ES_SCHD_TERM_32 = :QES-SCHD-TERM-32,
      *       ESFILE.ES_SCHD_COLLECTED_32 =
      *            :QES-SCHD-COLLECTED-32,
      *       ESFILE.ES_SCHD_EFF_DATE_33 =
      *          :QES-SCHD-EFF-DATE-33,
      *       ESFILE.ES_SCHD_1STPYDATE_33 =
      *          :QES-SCHD-1STPYDATE-33,
      *       ESFILE.ES_SCHD_ESCROW_33 = :QES-SCHD-ESCROW-33,
      *       ESFILE.ES_SCHD_TERM_33 = :QES-SCHD-TERM-33,
      *       ESFILE.ES_SCHD_COLLECTED_33 =
      *            :QES-SCHD-COLLECTED-33,
      *       ESFILE.ES_SCHD_EFF_DATE_34 =
      *          :QES-SCHD-EFF-DATE-34,
      *       ESFILE.ES_SCHD_1STPYDATE_34 =
      *          :QES-SCHD-1STPYDATE-34,
      *       ESFILE.ES_SCHD_ESCROW_34 = :QES-SCHD-ESCROW-34,
      *       ESFILE.ES_SCHD_TERM_34 = :QES-SCHD-TERM-34,
      *       ESFILE.ES_SCHD_COLLECTED_34 =
      *            :QES-SCHD-COLLECTED-34,
      *       ESFILE.ES_SCHD_EFF_DATE_35 =
      *          :QES-SCHD-EFF-DATE-35,
      *       ESFILE.ES_SCHD_1STPYDATE_35 =
      *          :QES-SCHD-1STPYDATE-35,
      *       ESFILE.ES_SCHD_ESCROW_35 = :QES-SCHD-ESCROW-35,
      *       ESFILE.ES_SCHD_TERM_35 = :QES-SCHD-TERM-35,
      *       ESFILE.ES_SCHD_COLLECTED_35 =
      *            :QES-SCHD-COLLECTED-35,
      *       ESFILE.ES_SCHD_EFF_DATE_36 =
      *          :QES-SCHD-EFF-DATE-36,
      *       ESFILE.ES_SCHD_1STPYDATE_36 =
      *          :QES-SCHD-1STPYDATE-36,
      *       ESFILE.ES_SCHD_ESCROW_36 = :QES-SCHD-ESCROW-36,
      *       ESFILE.ES_SCHD_TERM_36 = :QES-SCHD-TERM-36,
      *       ESFILE.ES_SCHD_COLLECTED_36 =
      *            :QES-SCHD-COLLECTED-36,
      *       ESFILE.ES_SCHD_EFF_DATE_37 =
      *          :QES-SCHD-EFF-DATE-37,
      *       ESFILE.ES_SCHD_1STPYDATE_37 =
      *          :QES-SCHD-1STPYDATE-37,
      *       ESFILE.ES_SCHD_ESCROW_37 = :QES-SCHD-ESCROW-37,
      *       ESFILE.ES_SCHD_TERM_37 = :QES-SCHD-TERM-37,
      *       ESFILE.ES_SCHD_COLLECTED_37 =
      *            :QES-SCHD-COLLECTED-37,
      *       ESFILE.ES_SCHD_EFF_DATE_38 =
      *          :QES-SCHD-EFF-DATE-38,
      *       ESFILE.ES_SCHD_1STPYDATE_38 =
      *          :QES-SCHD-1STPYDATE-38,
      *       ESFILE.ES_SCHD_ESCROW_38 = :QES-SCHD-ESCROW-38,
      *       ESFILE.ES_SCHD_TERM_38 = :QES-SCHD-TERM-38,
      *       ESFILE.ES_SCHD_COLLECTED_38 =
      *            :QES-SCHD-COLLECTED-38,
      *       ESFILE.ES_SCHD_EFF_DATE_39 =
      *          :QES-SCHD-EFF-DATE-39,
      *       ESFILE.ES_SCHD_1STPYDATE_39 =
      *          :QES-SCHD-1STPYDATE-39,
      *       ESFILE.ES_SCHD_ESCROW_39 = :QES-SCHD-ESCROW-39,
      *       ESFILE.ES_SCHD_TERM_39 = :QES-SCHD-TERM-39,
      *       ESFILE.ES_SCHD_COLLECTED_39 =
      *            :QES-SCHD-COLLECTED-39,
      *       ESFILE.ES_SCHD_EFF_DATE_40 =
      *          :QES-SCHD-EFF-DATE-40,
      *       ESFILE.ES_SCHD_1STPYDATE_40 =
      *          :QES-SCHD-1STPYDATE-40,
      *       ESFILE.ES_SCHD_ESCROW_40 = :QES-SCHD-ESCROW-40,
      *       ESFILE.ES_SCHD_TERM_40 = :QES-SCHD-TERM-40,
      *       ESFILE.ES_SCHD_COLLECTED_40 =
      *            :QES-SCHD-COLLECTED-40,
      *       ESFILE.ES_BALANCE = :QES-BALANCE,
      *       ESFILE.ES_PREPAID_1 = :QES-PREPAID-1,
      *       ESFILE.ES_COLLECTED_1 = :QES-COLLECTED-1,
      *       ESFILE.ES_DISBURSED_TAXES_1 =
      *            :QES-DISBURSED-TAXES-1,
      *       ESFILE.ES_DISBURSED_INS_1 = :QES-DISBURSED-INS-1,
      *       ESFILE.ES_DISBURSED_PMI_1 = :QES-DISBURSED-PMI-1,
      *       ESFILE.ES_DISBURSED_OTHER_1 =
      *            :QES-DISBURSED-OTHER-1,
      *       ESFILE.ES_DISBURSED_REF_1 = :QES-DISBURSED-REF-1,
      *       ESFILE.ES_DISBURSED_CNT_TAXES_1 =
      *            :QES-DISBURSED-CNT-TAXES-1,
      *       ESFILE.ES_DISBURSED_MISC1_1 =
      *            :QES-DISBURSED-MISC1-1,
      *       ESFILE.ES_DISBURSED_MISC2_1 =
      *            :QES-DISBURSED-MISC2-1,
      *       ESFILE.ES_PREPAID_2 = :QES-PREPAID-2,
      *       ESFILE.ES_COLLECTED_2 = :QES-COLLECTED-2,
      *       ESFILE.ES_DISBURSED_TAXES_2 =
      *            :QES-DISBURSED-TAXES-2,
      *       ESFILE.ES_DISBURSED_INS_2 = :QES-DISBURSED-INS-2,
      *       ESFILE.ES_DISBURSED_PMI_2 = :QES-DISBURSED-PMI-2,
      *       ESFILE.ES_DISBURSED_OTHER_2 =
      *            :QES-DISBURSED-OTHER-2,
      *       ESFILE.ES_DISBURSED_REF_2 = :QES-DISBURSED-REF-2,
      *       ESFILE.ES_DISBURSED_CNT_TAXES_2 =
      *            :QES-DISBURSED-CNT-TAXES-2,
      *       ESFILE.ES_DISBURSED_MISC1_2 =
      *            :QES-DISBURSED-MISC1-2,
      *       ESFILE.ES_DISBURSED_MISC2_2 =
      *            :QES-DISBURSED-MISC2-2,
      *       ESFILE.ES_PREPAID_3 = :QES-PREPAID-3,
      *       ESFILE.ES_COLLECTED_3 = :QES-COLLECTED-3,
      *       ESFILE.ES_DISBURSED_TAXES_3 =
      *            :QES-DISBURSED-TAXES-3,
      *       ESFILE.ES_DISBURSED_INS_3 = :QES-DISBURSED-INS-3,
      *       ESFILE.ES_DISBURSED_PMI_3 = :QES-DISBURSED-PMI-3,
      *       ESFILE.ES_DISBURSED_OTHER_3 =
      *            :QES-DISBURSED-OTHER-3,
      *       ESFILE.ES_DISBURSED_REF_3 = :QES-DISBURSED-REF-3,
      *       ESFILE.ES_DISBURSED_CNT_TAXES_3 =
      *            :QES-DISBURSED-CNT-TAXES-3,
      *       ESFILE.ES_DISBURSED_MISC1_3 =
      *            :QES-DISBURSED-MISC1-3,
      *       ESFILE.ES_DISBURSED_MISC2_3 =
      *            :QES-DISBURSED-MISC2-3,
      *       ESFILE.ES_MISC_AMT_1 = :QES-MISC-AMT-1,
      *       ESFILE.ES_EXP_DATE_1 =
      *          :QES-EXP-DATE-1,
      *       ESFILE.ES_MISC_AMT_2 = :QES-MISC-AMT-2,
      *       ESFILE.ES_EXP_DATE_2 =
      *          :QES-EXP-DATE-2,
      *       ESFILE.ES_MISC_AMT_3 = :QES-MISC-AMT-3,
      *       ESFILE.ES_EXP_DATE_3 =
      *          :QES-EXP-DATE-3,
      *       ESFILE.ES_MISC_AMT_4 = :QES-MISC-AMT-4,
      *       ESFILE.ES_EXP_DATE_4 =
      *          :QES-EXP-DATE-4,
      *       ESFILE.ES_MISC_AMT_5 = :QES-MISC-AMT-5,
      *       ESFILE.ES_EXP_DATE_5 =
      *          :QES-EXP-DATE-5,
      *       ESFILE.ES_MISC_AMT_6 = :QES-MISC-AMT-6,
      *       ESFILE.ES_EXP_DATE_6 = :QES-EXP-DATE-6
      *      WHERE ESFILE.ES_BRNO = :QES-BRNO
      *        AND ESFILE.ES_ACCTNO = :QES-ACCTNO
      *    END-EXEC.

      *    PERFORM SQL-IO-VALIDATION.

      *    IF ( IO-FG = 8 )     *> LOCKED
      *       GO TO REWRITE-ES1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *    MOVE '1' TO SQL-UPDATE-CONN-USE-SW. *> USE C1 CONNECT
      *    PERFORM SQL-SWAP-UPDATE-CONNECTION.

      *-----------------------------------------------------------------
      *CLOSE-ES1-FILE.

      *    PERFORM CLOSE-IT.

      *    IF ( ES1-CURSOR-STAT-GOOD )
      *       EXEC SQL SET CONNECTION ES1_CONNECT END-EXEC
      *       EXEC SQL
      *            CLOSE ES1_CURSOR
      *       END-EXEC
      *       EXEC SQL SET CONNECTION C1 END-EXEC
      *       MOVE STAT---BAD TO ES1-CURSOR-STAT.

      *-----------------------------------------------------------------
      *CLOSE-ES1-CONNECTION.

      *    IF ES1-CONN-OPEN-SW = "Y"
      *        EXEC SQL SET CONNECTION ES1_CONNECT END-EXEC
      *        EXEC SQL COMMIT END-EXEC
      *        EXEC SQL DISCONNECT ES1_CONNECT END-EXEC
      *        EXEC SQL SET CONNECTION C1 END-EXEC
      *        MOVE "N" TO ES1-CONN-OPEN-SW.

      *-----------------------------------------------------------------
 
       COPY "LIBGB/FERRORS.CPY".
           MOVE "Y" TO ERRCD-BUF.

      *****************************************************************
       END-ROUTINE SECTION.
      *****************************************************************
       END-RUN.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           EXIT PROGRAM.
