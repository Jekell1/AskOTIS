       IDENTIFICATION DIVISION.
       PROGRAM-ID.      LONPEX.
       DATE-WRITTEN.    052102.
      ******************************************************************
      *
      * NOTE::::::::
      *   THIS PROGRAM IS CALLED FROM LP/BATCHP (BATCH PROCESSING CRON JOB).
      *     BATCHP IS JUST A DRIVER PROGRAM THAT CAN RUN IN BATCH TO
      *     CALL THIS PROGRAM. 
      *
      *  DOC DESIGN FORM MUST HAVE "E" IN SMARTPORT FOR AN EXTRACTION
      *  PROGRAM = EX
      *  YOU CAN DO AN F3 FORM REQUEST FROM PAYMENT POSTING TO TEST
      *  THIS EXTRACTION FILE GETS CREATED WHILE YOU PRINT FROM BATCH FORM
      *
      *
      *  DESCRIPTION:  FORMS FILE EXTRACTION
      *  CREATES:
      *     ../../ED/DOCEXT.YYMMDD, FILE CANT EXIST, DOES NOT APPEND
      *            RSZ 660
      *
      *
      *=================================================================
      * REV:
      *  020529 BAH NEW, REGACC PR# 1800
      *  020613 BAH ALLOW FORM RANGE TO WORK
      *  020830 BAH ADD BOTH HHGS TO END OF OUTPUT FILE, REGACC PR#1888
      *  020830 BAH ADD LSX-RB-AMT, REGACC PR# 1896
      *  021204 BAH ADD LSX-OTH-EXP-CKAMT(1) ("RE" AMOUNTS), REGACC
      *  021219 BAH MOVE LSX-REDEEM-DATE TO WK-LN-REDEEMDATE IF LS EXISTS
      *  030107 BAH ADD LSX-SOLD-TO-AMT, LSX-10DAY-COST AND LN-CURBAL
      *             REGACC PR# 14
      *  030324 BAH ADDED A CHECK FOR FD-1STFILE = 13 TO CREATE AN APP
      *             ASCII FILE, LENDMARK PR# 2043
      *  030429 BAH ADDED UPDATE OF LN-DELCNT AND LN-SOLCNT AND ADDED
      *             UPDATE OF LOAN TRIAD TRAILER, REGACC PR# 26
      *  030506 BAH ADDED PROMDTE, PROMAMT, PROMDATE2 AND CREDLIMIT
      *             LENDMARK PR# 2076
      *  030530 BAH ADDED LIENHOLDER INFO, LT "L" RECORDS, REGACC PR# 20
      *  030904 BAH ADDED RIGHT TO CURE FIELDS, WK-PAST-DUE-TABLE
      *             REGACC, PR# 37
      *  031016 BAH ADDED WK-LN-OTHBAL, KEVIN, REGACC
      *  040119 BAH ADDED NEW BRANCH TRAILER FILE BR-FILE, ADD
      *             BR-COLLECT-MANAGER, PHONE AND EXT TO EXTRACT FILE
      *             EXPANDED WK-REC TO 2160
      *  040607 BAH HAD A BUNCH OF BAD "GO TO READ-FM2-FILE-NEXT" INSTEAD
      *             OF "GO TO READ-FM2FILE-NEXT"....KEVIN
      *  040629 CS  ADD DEALER (DL-NAME, DL-ADR1, DL-ADR2,DL-CITY, DL-STATE,
      *             DL-ZIP LT-L-TITLE-NO,LT-L-TITLE-RCV-DATE,
      *             LTL-TITLE-LIEN-DATE [PR# 66 REGIONAL ACCEPT] * RSZ = 2246
      *  040721 JTG CHANGED LOGIC TO COMPUTE CURE-DATE
      *                                       AUTO CREDIT EXPRESS
      *  040722 JTG ADDED LOGIC TO CORRECT 'CURE-PMT-TBL-IDX'
      *             GOING TO '0' CAUSING INDEX OUT OF BOUND
      *  060203 BAH UPDATE RIGHT TO CURE DATES IN TRIAD TRAILER
      *             LR-CL-RTC-DATE-1, DATE-2, REGACC PR# 121
      *  060224 BAH ADDED SOME FIELDS, REGACC PR# 123
      *  060724 BAH UPDATE LSD-DEF-LETTER-SENT, REGACC PR# 138
      *  061110 BAH ADDED REG-DL-DBA-NAME, REGACC ONLY PR# 155
      *  070220 BAH ADDED BR-CC FIELDS FOR COLLECTION CENTER REGACC PR# 164
      *  070301  CS ADDED LR-CL-ASSESSMENT,LR-CL-LAS-AL-DATE PR#152-R REGIONAL
      *  070420 BAH ADDED REG-LN-ACH FOR REGACC PR# 171
      *  070716  CS ADDED REG-LR-ENVELOPE-MAKER-COMAKER PR#177 REGACC
      *  070912  CS ADDED ADDITIONAL LS-D FIELDS, LSD-CALC FIELDS FOR
      *             REINSTATEMENT TO REGREGACC PR#180
      *  070920  CS ADD A AMPM INDENTIFIER FOR LSD-AUCTN-TIME-AMPM PER KEVIN
      *  070925  CS PER KEVIN REMOVE SALES PRICE & MAKE IT FILLER SINCE REGACC
      *             CREATED THE LAYOUT PR#180 REGACC
      *  071016 BAH REMOVED THE DECIMAL FROM REG-LR-LAST-AL-ASSESS-AMOUNT
      *  071025 BAH ADDED REG-LSX-AGENT-PHONE AND REG-LSX-STORED FIELDS
      *  080131 BAH PUT THE ASSUMED DECIMAL BACK IN REG-LR-LAST-AL-ASSESS-AMOUNT
      *  080828 MJD ADDED LOGIC TO ACCOUNT FOR DEFERMENTS WHEN CREATING
      *             THE PAYMENT SCHEDULE TABLE USED TO SEARCH FOR CURE DATE.
      *             AS PER KEVIN WE DECIDED TO BUMP THE FIRST PAYMENT
      *             IN THE TABLE BY THE NUMBER OF DERFERNENTS AND LET
      *             THE REMAINING DATES FALL INTO LINE BY PERIOD
      *             IF THE CURE DATE IS NOT FOUND, RETURN MDTE-DATE
      *             AND CURBAL IN THE WORK WORK FIELDS.
      *  081028 BAH ADDED REG-LN-COUPON-STATEMENT FLAG, MAKER AND COMAKER
      *              EMAILS FOR REGACC ONLY , PR# 224
      *  090121 BAH ADDED REG-LT-L-TITLE-NAME, ADR,CITY,STATE AND ZIP
      *                                       REGACC ONLY PR# 235
      *  090415  CS CHANGED COLLECTIONS FORM DATE FROM SYS-DATE TO TRANS-DATE
      *             SYS-DATE WAS BEING ALTERED    KEVIN PR#652
      *  100604 BAH ADDED REG-LN-ORGST FOR REGACC ONLY, REGACC PR# 269
      *  100921 BAH ADDED REG-LT-L-TITLE-ADR2, REGACC PR# 272
      *  110225 BAH MOVE UPDATED MATURITY TO REG-LSD-CALC-NXDUE-AFT-REPO
      *             IF ACCOUNT IS PAST MATURITY, REGACC PR# 284
      *  120111 BAH HAD MATURITY DATE TEST BACKWARDS FROM ABOVE CHANGE PR# 284
      *  120223 BAH WRITE A TOTAL RECORD FOR REGACC ONLY, PR# 304
      *  121030 BAH ADDED FIELDS FOR REGACC ONLY PR# 328
      *  130221  CS CHANGED DESC OF 'DEFERMENT' TO 'EXTENSION' PR#328A
      *  130222  CS CHANGED CASE-BUF-1 TO "E" FROM "D" PR#328A
      *  130327 BAH ADDED LS-Y FIELDS AND MORE LS-D FIELDS FOR REGACC PR# 333
      *  130328 BAH CHANGED HARD CODED FMNO 511 TO FT-FORM-RIGHT-TO-CURE
      *                                           REGACC PR# 337
      *  130423 BAH SKIP BANKRUPTS EXCEPT FMNO 515, MARK FM'S AS PRINTED AND
      *             CREATE A MEMO, REGACC PR# 339
      *  140627 BAH USE TDUEPOFF FOR REG-TOTNXDUEAMT IF ON OR PAST MATURITY
      *                                                   REGACC PR# 354
      *  141124 BAH ADDED REG-LN-CLASS, REGACC PR#361
      *  151119 BAH ADDED REG-LN-MAKERCD-1 THRU 4, REGACC PR# 379
      *
      * KEC 2016.0119 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         FIXED KEYS & RECS FOR FD-REC.
      *         ADDED NEW FDB-FILE; WAS IN FD-FILE.
      *         REMOVED FT-FILE; MERGED INTO FD-FILE.
      *
      *  BAH 160118 SPLIT OUT LTFILE, PD0003
      *  160128 BAH ADD FORMS 511, 512 AND 513 TO 515 TEST FOR BANKRUPTS(339)
      *             REGACC PR380
      *  BAH 160209 SPLIT OUT LSFILE, PD0003
      *
      * KEC 2016.0211 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         FIXED KEYS & RECS FOR FD-REC.
      *         CHANGED FMNO/FORMNO/NEXTFM/SETFM FROM 3 TO 4 BYTES.
      *         *NOTE* THE FMNO IN THE REG-REC, APP-REC & WK-REC 
      *                WERE NOT CHANGED SO NOT TO DISTORT VENDORS.
      *
      * BAH 160218 EXPANDED LM-REC ADDED LM-BRNO, PD0003
      *
      * KEC 2016.0311 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         ADDED MOVING SPACES TO CD-RF-FILLER.
      *
      * KEC 2016.0506 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED AM-APPNO TO AM-APP-NUMBER TO MATCH MA1-KEY.
      *         CHANGED MA-APPNO TO MA-APP-NUMBER TO MATCH MA1-KEY.
      *
      * KEC 2016.0524 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED USE OF MA-APPDATE & MA-STATUS-DATE
      *          TO USE CCYYMMDD.
      *
      * KEC 2016.0818 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         REMOVED USE OF THE BA-FILE [LIBLA/LAFDMX].
      *         REVISED LA-REC LAYOUT.
      *
      * KEC 2016.0914 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         SET LR-BRNO USING LN-OWNBR.
      *
      * KEC 2017.0119 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          CHANGED FD-SMARTPORT TO FD-OPTION [88 LEVELS].
      *
      * BAH 170613 ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 170721 NEW LAYOUT FOR WORLD, PR#1260
      * BAH 180202 CHANGES TO BE CALLED FROM BATCHP FOR CRON JOB CALLING
      *            BATCHP WHICH CALLS LONPEX, WORLD PR#1301
      * BAH 180807 ADDED REG-LN-SELLPR, REG-AMTFIN, REG-VEHICLE-SOLD-DATE
      *            FOR REGACC PR# 393
      * KEC 180824 LOAN APP MODULE REMOVAL FROM A30 [LARM30]
      * BAH 180917 ADDED ST-MAX-TAX-LOAN-AMT, ST-NSF-FEE-AMT #1334
      * BAH 181115 GLOBAL FMFILE
      * BAH 181128 GLOBAL LMFILE
      * BAH 181210 GLOBAL LNFILE
      * BAH 190130 1STPYDATE OF 8/31/18 WAS PASSING 9/31/18 TO T-DATE-DUE
      *            HAD SOME FEBRUARY LOGIC MOVING THE ORIG DAY IN AND NOT
      *            CHECKING IF THAT DATE EXIST! REGACC PL#998
      * BAH 190213 PER DIEM RATE IS USING INDU-WORKER WHICH DOES NOT GET
      *            CLEARED EACH TIME AND CAN EXIT THE SET-PY-RATE ROUTINE
      *            WITH PRIOR ACCOUNTS INFO, REGACC PL#999
      * BAH 190312 MOVED FM-BRNO2 INTO LPWSFM-EDIT-FM-INITKEY TO REMOVE THE
      *            REDEFINE IN THE FM-REC
      * BAH 2019.0709 REMOVED GP-LR-TRAILER-TRIAD, PT0151 
      * BAH 2019.0922 REMOVED ACCESS-CALL LNFILE
      * BAH 2019.1120 ADDED WRLD-PROCEEDS, NEED TO REPORT LOAN PROCEEDS FOR
      *              ALL CLASSES EXCEPT CLASS 2. CLASS 2 WILL REPORT AMTFIN
      *              AS THESE ARE LIVE CHECK LOANS WITH NO PROCEEDS. #1402
      * BAH 2024.0312 ONLY CALL CRNOR2 IF NOTICE NOT FOUND
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
           SELECT WORLD-FILE ASSIGN TO WORLD-PATH
                  ORGANIZATION SEQUENTIAL
                  ACCESS SEQUENTIAL
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  FILE STATUS FILE-STAT.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.

       COPY "LIBGB/GBFDPR.CPY".
       FD  WORLD-FILE
           LABEL RECORDS ARE STANDARD.
       01  WORLD-REC.
           03  WRLD-FMNO                   PIC 9(3).
           03  WRLD-BRNO                   PIC 9(4).
           03  WRLD-LN-ACCTNO              PIC 9(6).
           03  WRLD-DATE                   PIC 9(8).
           03  WRLD-BR-NO                  PIC 9(4).
           03  WRLD-BR-NAME                PIC X(40).
           03  WRLD-BR-ADR1                PIC X(24).
           03  WRLD-BR-ADR2                PIC X(24).
           03  WRLD-BR-CITY                PIC X(15).
           03  WRLD-BR-STATE               PIC XX.
           03  WRLD-BR-ZIP                 PIC X(10).
           03  WRLD-BR-PHONE               PIC 9(10).
           03  WRLD-BR-MANAGER             PIC X(25).
           03  WRLD-GB-NAME                PIC X(40).
           03  WRLD-MAX-LOAN-AVAIL         PIC 9(6).99.

           03  WRLD-BW-1-FNAME             PIC X(16).
           03  WRLD-BW-1-LNAME             PIC X(16).
           03  WRLD-BW-1-ADR1              PIC X(24).
           03  WRLD-BW-1-ADR2              PIC X(24).
           03  WRLD-BW-1-CITY              PIC X(15).
           03  WRLD-BW-1-STATE             PIC X(2).
           03  WRLD-BW-1-ZIP               PIC X(10).
           03  WRLD-BW-1-REFDATE           PIC 9(8).

           03  WRLD-BW-2-FNAME             PIC X(16).
           03  WRLD-BW-2-LNAME             PIC X(16).
           03  WRLD-BW-2-ADR1              PIC X(24).
           03  WRLD-BW-2-ADR2              PIC X(24).
           03  WRLD-BW-2-CITY              PIC X(15).
           03  WRLD-BW-2-STATE             PIC X(2).
           03  WRLD-BW-2-ZIP               PIC X(10).
           03  WRLD-BW-2-REFDATE           PIC 9(8).

           03  WRLD-LN-ACCTNO2             PIC 9(6).
           03  WRLD-LN-LOANDATE            PIC 9(8).
           03  WRLD-LN-1STPYDATE           PIC 9(8).
           03  WRLD-LN-MDTE                PIC 9(8).

           03  WRLD-LN-APRATE              PIC 9(3).9(3).
           03  WRLD-LN-EFFRATE             PIC 9(3).9(3).
           03  WRLD-LN-ORGTERM             PIC 9(3).
           03  WRLD-LN-1STPYAMT            PIC 9(6).99.
           03  WRLD-LN-REGPYAMT            PIC 9(6).99.
           03  WRLD-LN-LASTPYAMT           PIC 9(6).99.

           03  WRLD-NO-SCHED-PAYMENT       PIC 9(6).99.
           03  WRLD-FINCHG                 PIC 9(6).99.
           03  WRLD-TOTAL-REPAY            PIC 9(6).99.
           03  WRLD-AMT-FINANCED           PIC 9(6).99.

           03  WRLD-DATE-PAID-LAST         PIC 9(8).
           03  WRLD-AMT-PAID-LAST          PIC 9(7).99.
           03  WRLD-LN-CURBAL              PIC 9(7).99.
           03  WRLD-CURRENT-DUEDATE        PIC 9(8).
           03  WRLD-TOTAL-AMT-DUE          PIC 9(7).99.
           03  WRLD-LOAN-PAYOFF            PIC 9(7).99.
           03  WRLD-AVAIL-CREDIT           PIC 9(7).99.
           03  WRLD-MAX-TAX-LOAN-AMT       PIC 9(7).99.
           03  WRLD-NSF-FEE-AMT            PIC 9(5).99.
           03  WRLD-PROCEEDS               PIC 9(7).99.
           03  WRLD-CR            PIC X.
           03  WRLD-LF            PIC X.


       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)LP/LONPEX S35 02/17/22-14:24:31 >".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GB.CPY".
       COPY "LIBGB/GB01GB_SQL.CPY".
       COPY "LIBGB/GBWSGB.CPY".
       COPY "LIBLP/LP01NO.CPY".
       COPY "LIBLP/LP01NO_SQL.CPY".
       COPY "LIBLP/LPWSNO.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01LTI.CPY".
       COPY "LIBLP/LP01LTI_SQL.CPY".
       COPY "LIBLP/LPWSLTI.CPY".
       COPY "LIBLP/LP01LP.CPY".
       COPY "LIBLP/LP01LP_SQL.CPY".
       COPY "LIBLP/LPWSLP.CPY".
       COPY "LIBLP/LP01LM.CPY".
       COPY "LIBLP/LP01LM_SQL.CPY".
       COPY "LIBLP/LPWSLM.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01FM.CPY".
       COPY "LIBLP/LP01FM_SQL.CPY".
       COPY "LIBLP/LPWSFM.CPY".
       COPY "LIBLP/LP01LTP.CPY".
       COPY "LIBLP/LP01LTP_SQL.CPY".
       COPY "LIBLP/LPWSLTP.CPY".
       COPY "LIBLP/LP01FD.CPY".
       COPY "LIBLP/LP01FD_SQL.CPY".
       COPY "LIBLP/LPWSFD.CPY".
       COPY "LIBLP/LP01ST.CPY".
       COPY "LIBLP/LP01ST_SQL.CPY".
       COPY "LIBLP/LPWSST.CPY".

       01  WORLD-PATH.
           03  WORLD-PATH-BASE           PIC X(09).
           03  FILLER                    PIC X(01)   VALUE "/".
           03  WORLD-PATH-MACHINE        PIC X(02).
           03  FILLER                    PIC X(11) VALUE "/ED/DOCEXT.".
           03  WORLD-YR                  PIC 99.
           03  WORLD-MO                  PIC 99.
           03  WORLD-DA                  PIC 99.


       01  ADD-ON          PIC X(25).
       01  SUB             PIC 9(6) COMP-3   VALUE 0.
       01  C-S-Z           PIC X(85).
       01  STATE           PIC XX.
       01  ZIP             PIC X(10).

       01  RECORD-COUNTER  PIC 9(9) COMP-3 VALUE 0.
       01  MSEL-LIST       PIC X(5) VALUE "MSEL.".

       01  HOLD-FM2-KEY.
           03  HOLD-FM2-BRNO PIC 9(4).
           03  HOLD-FM2-FMNO PIC 9(4).
           03  HOLD-FM2-ZIP  PIC X(15).
       01  HOLD-FM1-KEY      PIC X(28).


       01  HOLD-ST-CODE                PIC XX       VALUE SPACES.

       01  SAVE-LM1-KEY         VALUE SPACES.
           03  SAVE-LM-BRNO     PIC 9(4).
           03  SAVE-LM-ACCTNO   PIC 9(8).
           03  SAVE-LM-DATE     PIC 9(8).
           03  SAVE-LM-SEQ      PIC 9(3).
       01  SAVE-LM-MEMO.
           03  FILLER           PIC X(7)  VALUE "FORM # ".
           03  MEMO-FMNO        PIC 9(4)  VALUE 0.
           03  FILLER           PIC X(3)  VALUE ",  ".
           03  MEMO-DESC        PIC X(25) VALUE SPACES.
           03  MEMO-CHG-CD      PIC X(22) VALUE SPACES.

       01  PAYMENT-TABLE        VALUE LOW-VALUES.
           03  PMT-TABLE        OCCURS 360.
               05  T-DATE-DUE   PIC 9(8)     COMP-3.
               05  T-AMT-DUE    PIC 9(6)V99  COMP-3.
               05  L-DATE-DUE   PIC 9(8)     COMP-3.
               05  L-AMT-DUE    PIC 9(6)V99  COMP-3.

      *****************************************************************
      *
      *     C O N T R A C T   B U F F E R
      *
      *****************************************************************
       01  DETAIL-LINE             VALUE " ".
           03  PRINT-AREA          PIC X(80).

      *****************************************************************
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBLP/LPMENUW.CPY".
       COPY "LIBLP/LPSCHDW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBLP/LPAMTSW.CPY".
       COPY "LIBLP/LPUPWKW.CPY".
       COPY "LIBLP/LPLCHGW.CPY".
       COPY "LIBCL/CRNOR2W.CPY".

       COPY "LIBLP/LPPOFFW.CPY".
       COPY "LIBGB/ACCESSW.CPY".

       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/LPPBAT_DEF.CPY".
       COPY "LIBLP/LPPBAT_WKS.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBLP/LPLOANW.CPY".
       COPY "LIBLP/LPIPBUF.CPY".
       COPY "LIBGB/GETPRW.CPY".

       SCREEN SECTION.
       COPY "LIBLP/LPPBAT_SCN.CPY".
      *****************************************************************
      *
      *      P R O C E D U R E    D I V I S I O N
      *
      *****************************************************************
       PROCEDURE DIVISION
             USING FORM-PATHNAME PROG-BUF IP-BUF PRU-PATH EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
              PR-FILE WORLD-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-ST1-FILE.
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LTP1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-LTI1-FILE.
           PERFORM CLOSE-LP1-FILE.
           PERFORM CLOSE-LM1-FILE.
           PERFORM CLOSE-FM2-FILE.
           PERFORM CLOSE-FM1-FILE.
           PERFORM CLOSE-FD1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GB-FILE.
           PERFORM CLOSE-BR-FILE.
           CLOSE WORLD-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      *****************************************************************
      *
      *   C O N T R O L   M O D U L E   *
      *
      *****************************************************************
       MAIN-PROGRAM SECTION.

      D    STOP MESS.

           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.

      * ONLY RAN FROM BATCH FORM PRINT
           MOVE "LPPBAT" TO VDU-FORM-NAME.

           IF IP-BEG-FMNOX = SPACES AND IP-END-FMNOX = SPACES
              MOVE 0 TO IP-BEG-FMNO IP-END-FMNO.

           MOVE EXT-FILPATH-BASE    TO WORLD-PATH-BASE.
           MOVE EXT-FILPATH-MACHINE TO WORLD-PATH-MACHINE.

           MOVE SPACES TO IP-PROG.
           MOVE 0 TO RECORD-COUNTER.

           PERFORM GET-GPENV.

           MOVE IP-FMNO TO FD-FMNO.
           PERFORM GET-FD.

           PERFORM OPEN-FM1-FILE.
           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-SP1-FILE.
           PERFORM OPEN-ST1-FILE.

           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD-MM TO WORLD-MO.
           MOVE DATE-YYYYMMDD-DD TO WORLD-DA.
           MOVE DATE-YYYYMMDD-YY TO WORLD-YR.
           MOVE DATE-YYYYMMDD TO TRANS-DATE.

      * WORLD WILL ONLY CREATE A NEW FILE, IF IT EXISTS ALREADY IT
      * WILL EXIT
           MOVE WORLD-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT NOT = "00"
              PERFORM OPEN-WORLD-FILE-OUTPUT
           ELSE
              MOVE "FILE ALREADY EXISTS" TO MESS
              PERFORM SEND-MESS
              GO TO END-ROUTINE.

           MOVE "EXTRACTION IN PROGRESS........" TO WR-BUF.
           MOVE MSEL-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.


      * FOR CORPORATE PRINTING, IT WILL BE A CORPORATE LOGIN RUNNING
      * THIS SO THE BRANCH PATH WILL BE CORPORATE.
      * FM-BRNO WILL BE CORPORATE AND FM-BRNO2 WILL BE THE OWNING BRANCH 

      * KEY 2 = BRNO, FMNO, ZIP

           MOVE FM-PATH-OWNBR TO FM-BRNO.
           MOVE IP-FMNO       TO FM-FMNO.
           MOVE SPACES        TO FM-ZIP.

           PERFORM START-FM2-FILE.
       READ-FM2FILE-NEXT.
           PERFORM READ-FM2-FILE-NEXT.
           IF IO-FG NOT = 0 OR (FM-BRNO NOT = FM-PATH-OWNBR)
              GO TO END-ROUTINE.

           IF ( FM-FMNO NOT = IP-FMNO )
              IF ( IP-BEG-FMNO NOT = ZEROES ) AND
                 ( IP-END-FMNO NOT = ZEROES )
                 IF ( FM-FMNO < IP-BEG-FMNO ) OR
                    ( FM-FMNO > IP-END-FMNO )
                    GO TO END-ROUTINE
                 ELSE
                    MOVE FM-FMNO TO FD-FMNO
                    PERFORM GET-FD
                    IF ( FD-OPTION = "E" ) AND ( FD-PROG = "EX" )
                       GO TO BYPASS-FM
                    ELSE
                       GO TO READ-FM2FILE-NEXT
              ELSE
                 GO TO END-ROUTINE
            ELSE
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * EVEN IF FORM # MATCHES, IT MIGHT NOT BE THE RIGHT FMTYPE
      * FROM WITHIN THE RANGE OF FORMS
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
              IF ( IP-BEG-FMNO NOT = ZEROES ) AND
                 ( IP-END-FMNO NOT = ZEROES )
                 MOVE FM-FMNO TO FD-FMNO
                 PERFORM GET-FD
                 IF ( FD-OPTION NOT = "E" ) OR ( FD-PROG NOT = "EX" )
                    GO TO READ-FM2FILE-NEXT.

       BYPASS-FM.

           MOVE FM-INITKEY TO LPWSFM-EDIT-FM-INITKEY.

           IF BRANCH NOT = 0
              IF FM-BRNO NOT = BRANCH
                 GO TO READ-FM2FILE-NEXT.

           IF PRINT-REPRINT = "P"
              IF FM-PRINT = "Y"
                 GO TO READ-FM2FILE-NEXT.

           IF PRINT-REPRINT = "R"
              IF FM-PRINT NOT = "Y"
                 GO TO READ-FM2FILE-NEXT.

           PERFORM GET-FILE-PATHS.
           IF ERRCD = "X"
              GO TO READ-FM2FILE-NEXT.

           MOVE LPWSFM-EDIT-FM-ACCTNO TO LN-ACCTNO.
           PERFORM OPEN-LN1-FILE.
           PERFORM READ-LN1-FILE.
           PERFORM CLOSE-LN1-FILE.
           IF IO-FG = 9
              GO TO READ-FM2FILE-NEXT.

           MOVE LN-ORGST     TO SP-ORGST.
           MOVE LN-SPRCLASS  TO SP-SPRCLASS.
           MOVE LN-SUBCLASS  TO SP-SUBCLASS.
           MOVE LN-LAWCODE   TO SP-LAWCODE.
           IF LPWSSP-SP1-KEY NOT = SP1-KEY
              MOVE SP1-KEY TO LPWSSP-SP1-KEY
              PERFORM READ-SP1-FILE
              IF IO-FG = 9
                 GO TO READ-FM2FILE-NEXT.

           PERFORM LOAN-CALCULATIONS.

           PERFORM BUILD-PAYMENT-TABLE.

           PERFORM BUILD-WORLD-EXTRACTION.

           IF FD-INCR-DELCNT  = "Y" 
              PERFORM UPDATE-LOAN-RECORD.

           IF FD-LNMEMO = "Y"
              PERFORM UPDATE-LOAN-MEMO.

       CONTINUE-LOOPING.
           MOVE FM-BRNO TO HOLD-FM2-BRNO.
           MOVE FM-FMNO TO HOLD-FM2-FMNO.
           MOVE FM-ZIP  TO HOLD-FM2-ZIP.
           MOVE FM1-KEY TO HOLD-FM1-KEY.

           PERFORM READ-FM1-FILE.
           IF IO-FG = 0
              MOVE "Y" TO FM-PRINT
              PERFORM REWRITE-FM1-FILE
              GO TO READ-FM2FILE-NEXT.

           GO TO END-ROUTINE.

      ****************************
       GET-FILE-PATHS SECTION.
      ****************************
           MOVE " " TO ERRCD.

      * THIS WILL GET THE OWNING BRANCHES FILES, EVEN IF CORPORATE PRINT
           MOVE LPWSFM-EDIT-FM-BRNO2 TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-FG = 9
              MOVE "X" TO ERRCD
              GO TO EXIT-GET-FILE-PATHS.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO BW-PATH-OVERRIDE 
                                              GB-PATH-OVERRIDE
                                              NO-PATH-OVERRIDE
                                              LM-PATH-OVERRIDE
                                              LN-PATH-OVERRIDE
                                              LP-PATH-OVERRIDE
                                              LTI-PATH-OVERRIDE
                                              LTP-PATH-OVERRIDE.

           PERFORM OPEN-GB-FILE.
           MOVE BR-NO TO GB-BRNO.
           MOVE 1     TO GB-NO.
           PERFORM READ-GB-FILE.
           PERFORM CLOSE-GB-FILE.

       EXIT-GET-FILE-PATHS.
           EXIT.

      ******************************
       BUILD-PAYMENT-TABLE SECTION.
      ******************************
           MOVE ALL LOW-VALUES TO PAYMENT-TABLE.
           MOVE 0 TO SUB.

           IF LN-PAY-SCHLD-FG = "Y"
              PERFORM GET-LT-P-REC
              IF IO-FG = 0
                 PERFORM UNPACK-PAYMENT-SCHEDULE
              ELSE
                 MOVE "PAYMENT SCHEDULE MISSING" TO MESS
                 PERFORM SEND-MESS
                 GO TO IO-ERROR.

           MOVE LN-1STPYDATE TO NUM-DATE.
           MOVE NUM-DA TO DTE-HOLD.

      **********************************************************
      * WE NEED TO ACCOUNT FOR DEFERMENTS PLACED ON THE LOAN
      * BUMP FIRST PAY DATE UP BY THE NUMBER OF DEFERMENTS
      * REMAINING DATES WILL FALL INTO LINE
      **********************************************************
           MOVE NUM-DATE      TO NDTE-DATE.
           MOVE LN-TOTNODEF   TO NDTE-HOLD.
           MOVE LN-UNITPER-CD TO DATER-UNITPER-CD.
           PERFORM INCREMENT-UNITPER.
           MOVE NDTE-DATE TO NUM-DATE.
           IF LN-UNITPER-CD = "M"
              IF NUM-MO NOT = 2
                 MOVE DTE-HOLD TO NUM-DA
      * 1STPYDATE OF 8/31/2018 WAS PASSING 9/31/2018 TO T-DATE-DUE
                 MOVE NUM-DATE TO NDTE-DATE
                 MOVE 0 TO NDTE-HOLD
                 PERFORM NEW-DATE-CALCULATION
                 MOVE NDTE-DATE TO NUM-DATE.

           MOVE LN-SUM-REGPYAMT TO LCHG-LATE
                                   LCHG-CURPAY.
           MOVE NUM-DATE TO LCHG-LCPDTH-DATE.
           PERFORM LATE-CHARGE-CALCULATION.

       BUILD-PAYMENT-CONT.
           ADD 1 TO SUB.
           IF SUB > LN-SUM-ORGTERM OR SUB > 360
              GO TO BUILD-PAYMENT-EXIT.

           MOVE NUM-DATE TO T-DATE-DUE(SUB).

           IF LN-PAY-SCHLD-FG = "Y"
              MOVE SCHD-AMT(SUB) TO T-AMT-DUE(SUB)
                                    L-AMT-DUE(SUB)
           ELSE
           IF SUB = 1
              MOVE LN-1STPYAMT TO T-AMT-DUE(SUB)
                                  L-AMT-DUE(SUB)
           ELSE
              IF SUB NOT = LN-ORGTERM
                 MOVE LN-REGPYAMT TO T-AMT-DUE(SUB)
                                     L-AMT-DUE(SUB)
              ELSE
                 MOVE LN-LASTPYAMT TO T-AMT-DUE(SUB)
                                      L-AMT-DUE(SUB).

           IF L-AMT-DUE(SUB) NOT = LCHG-CURPAY
              MOVE L-AMT-DUE(SUB) TO LCHG-CURPAY
                                     LCHG-LATE
              MOVE T-DATE-DUE(SUB) TO LCHG-LCPDTH-DATE
              PERFORM LATE-CHARGE-CALCULATION.
           ADD LCHG-CHARGE TO L-AMT-DUE(SUB).

           MOVE T-DATE-DUE(SUB) TO NDTE-DATE.
      * UPWK-LC-GRACE-FAC IS COMPUTED IN LATE-CHARGE-CALC
           IF SP-LCUNITPER-CD = "A"
              COMPUTE NDTE-HOLD = SP-LCGRACE * UPWK-LC-GRACE-FAC * .01
           ELSE
              MOVE SP-LCGRACE TO NDTE-HOLD.
           PERFORM INCREMENT-DAYS.
           MOVE NDTE-DATE TO L-DATE-DUE(SUB).

           MOVE T-DATE-DUE(SUB) TO NDTE-DATE.
           MOVE 1 TO NDTE-HOLD.
           MOVE LN-UNITPER-CD TO DATER-UNITPER-CD.
           PERFORM INCREMENT-UNITPER.
           MOVE NDTE-DATE TO NUM-DATE.
           IF LN-UNITPER-CD = "M"
              IF NUM-MO NOT = 2
                 MOVE DTE-HOLD TO NUM-DA
      * 1STPYDATE OF 8/31/2018 WAS PASSING 9/31/2018 TO T-DATE-DUE
                 MOVE NUM-DATE TO NDTE-DATE
                 MOVE 0 TO NDTE-HOLD
                 PERFORM NEW-DATE-CALCULATION
                 MOVE NDTE-DATE TO NUM-DATE.

           GO TO BUILD-PAYMENT-CONT.

       BUILD-PAYMENT-EXIT.
           EXIT.


      ******************************
       BUILD-WORLD-EXTRACTION SECTION.
      ******************************
           MOVE SPACES TO WORLD-REC.

           MOVE FM-FMNO   TO WRLD-FMNO.
           MOVE BR-NO     TO WRLD-BRNO WRLD-BR-NO.
           MOVE LN-ACCTNO TO WRLD-LN-ACCTNO.
           MOVE TRANS-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY TO WRLD-DATE.

           MOVE BR-NAME TO WRLD-BR-NAME.
           MOVE BR-ADR1 TO WRLD-BR-ADR1.
           MOVE BR-ADR2 TO WRLD-BR-ADR2.
           MOVE BR-CITY TO WRLD-BR-CITY.
           MOVE BR-STATE TO WRLD-BR-STATE.
           MOVE BR-ZIP TO WRLD-BR-ZIP.
           MOVE BR-PHONE TO WRLD-BR-PHONE.
           MOVE BR-MANAGER TO WRLD-BR-MANAGER.
           MOVE GB-COMPANY-NAME TO WRLD-GB-NAME.

           MOVE LN-ORGST TO HOLD-ST-CODE.
           IF HOLD-ST-CODE NOT = ST-CODE
              MOVE HOLD-ST-CODE TO ST-CODE
              PERFORM READ-ST1-FILE
              IF IO-FG = 9
                 MOVE 0 TO ST-MAX-LOAN-AVAIL
                           ST-MAX-TAX-LOAN-AMT
                           ST-NSF-FEE-AMT.

           MOVE ST-MAX-LOAN-AVAIL   TO WRLD-MAX-LOAN-AVAIL.
           MOVE ST-MAX-TAX-LOAN-AMT TO WRLD-MAX-TAX-LOAN-AMT.
           MOVE ST-NSF-FEE-AMT      TO WRLD-NSF-FEE-AMT.

           PERFORM OPEN-BW1-FILE.
           MOVE LN-SSNO(1) TO BW-SSNO.
           PERFORM GET-BW1.

           MOVE BW-FNAME   TO WRLD-BW-1-FNAME.
           MOVE BW-LNAME   TO WRLD-BW-1-LNAME.
           MOVE BW-ADR1    TO WRLD-BW-1-ADR1.
           MOVE BW-ADR2    TO WRLD-BW-1-ADR2.
           MOVE BW-CITY    TO WRLD-BW-1-CITY.
           MOVE BW-STATE   TO WRLD-BW-1-STATE.
           MOVE BW-ZIP     TO WRLD-BW-1-ZIP.
           MOVE BW-REFDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY TO WRLD-BW-1-REFDATE.

           MOVE LN-SSNO(2) TO BW-SSNO.
           PERFORM GET-BW1.

           MOVE BW-FNAME   TO WRLD-BW-2-FNAME.
           MOVE BW-LNAME   TO WRLD-BW-2-LNAME.
           MOVE BW-ADR1    TO WRLD-BW-2-ADR1.
           MOVE BW-ADR2    TO WRLD-BW-2-ADR2.
           MOVE BW-CITY    TO WRLD-BW-2-CITY.
           MOVE BW-STATE   TO WRLD-BW-2-STATE.
           MOVE BW-ZIP     TO WRLD-BW-2-ZIP.
           MOVE BW-REFDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY TO WRLD-BW-2-REFDATE.

           MOVE LN-ACCTNO    TO WRLD-LN-ACCTNO2.
           MOVE LN-LOANDATE  TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY  TO WRLD-LN-LOANDATE.
           MOVE LN-1STPYDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY TO WRLD-LN-1STPYDATE.
           PERFORM MATURITY-DATE-CALCULATION.
           MOVE MDTE-DATE    TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY    TO WRLD-LN-MDTE.
           MOVE LN-APRATE    TO WRLD-LN-APRATE.
           MOVE LN-EFFRATE   TO WRLD-LN-EFFRATE.
           MOVE LN-ORGTERM   TO WRLD-LN-ORGTERM.
           MOVE LN-1STPYAMT  TO WRLD-LN-1STPYAMT.
           MOVE LN-REGPYAMT  TO WRLD-LN-REGPYAMT.
           MOVE LN-LASTPYAMT TO WRLD-LN-LASTPYAMT.

           PERFORM OPEN-NO1-FILE.
           MOVE LN-OWNBR  TO NO-BRNO.
           MOVE LN-ACCTNO TO NO-ACCTNO CRNOR2-ACCTNO.
           PERFORM READ-NO1-FILE.
           IF IO-FG NOT = 0
              PERFORM CREATE-NOTICE-RECORD.

           MOVE NO-SCHED-PAYMENT TO WRLD-NO-SCHED-PAYMENT.
           MOVE LN-FINCHG        TO WRLD-FINCHG.
           MOVE TOTAL-REPAY      TO WRLD-TOTAL-REPAY.
           MOVE FINANCED-AMOUNT  TO WRLD-AMT-FINANCED.

           MOVE LN-DATE-PAID-LAST TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY TO WRLD-DATE-PAID-LAST.
           MOVE LN-AMT-PAID-LAST  TO WRLD-AMT-PAID-LAST.

           MOVE LN-CURBAL        TO WRLD-LN-CURBAL.
           MOVE NO-TRUE-DUE-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY TO WRLD-CURRENT-DUEDATE.
           MOVE NO-TDUEAMT       TO WRLD-TOTAL-AMT-DUE.
           MOVE NO-TDUEPOFF      TO WRLD-LOAN-PAYOFF.
           MOVE NO-AVAILCREDIT   TO WRLD-AVAIL-CREDIT.

      * WE NEED TO REPORT LOAN PROCEEDS FOR ALL CLASSES EXCEPT CLASS 2.
      * CLASS 2 WILL REPORT AMTFIN AS THESE ARE LIVE CHECK LOANS WITH
      * NO PROCEEDS.
           IF LN-CLASS NOT = 2
              MOVE LN-PROCEEDS     TO WRLD-PROCEEDS
           ELSE
              MOVE FINANCED-AMOUNT TO WRLD-PROCEEDS.

           MOVE X"0D" TO WRLD-CR.
           MOVE X"0A" TO WRLD-LF.

           PERFORM WRITE-WORLD-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

       EXIT-BUILD-WORLD-EXTRACTION.
           EXIT.


      *****************************
       UPDATE-LOAN-RECORD SECTION.
      *****************************

           MOVE FM-INITKEY TO LPWSFM-EDIT-FM-INITKEY.
           MOVE LPWSFM-EDIT-FM-ACCTNO TO LN-ACCTNO.
           PERFORM OPEN-LN1-FILE.
           PERFORM READ-LN1-FILE.
           IF IO-FG = 9
              GO TO EXIT-UPDATE-LOAN-RECORD.

           IF FD-FMTYPE = "S"
              ADD 1 TO LN-SOLCNT
           ELSE
              IF FD-FMTYPE = "C"
                 ADD 1 TO LN-DELCNT2.

           PERFORM REWRITE-LN1-FILE.

       EXIT-UPDATE-LOAN-RECORD.
           PERFORM CLOSE-LN1-FILE.


      *****************************
       UPDATE-LOAN-MEMO SECTION.
      *****************************

           PERFORM OPEN-LM1-FILE.

           MOVE LM-PATH-OWNBR TO SAVE-LM-BRNO.
           MOVE LN-ACCTNO     TO SAVE-LM-ACCTNO.
           COMPUTE SAVE-LM-DATE = 99999999 - TRANS-DATE.
           MOVE 1             TO SAVE-LM-SEQ.

       CREATE-MEMO.
           MOVE SAVE-LM1-KEY TO LM1-KEY.
           PERFORM READ-LM1-FILE.
           IF ( IO-FG = 0 )
              IF ( LM-SEQ < 999 )
                 ADD 1 TO SAVE-LM-SEQ
                 GO TO CREATE-MEMO
              ELSE
                 GO TO UPDATE-MEMO-EXIT.

           MOVE SPACES       TO LM-REC.
           MOVE SAVE-LM1-KEY TO LM1-KEY.
           MOVE FD-FMNO      TO MEMO-FMNO.
           MOVE FD-DESC      TO MEMO-DESC.
           MOVE SPACES       TO MEMO-CHG-CD.

           MOVE SAVE-LM-MEMO TO LM-MEMO.

           PERFORM WRITE-LM1-FILE.

      *-----------------------------------------------------------------
       UPDATE-MEMO-EXIT.
           PERFORM CLOSE-LM1-FILE.

       COPY "LIBLP/LPSCHD.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/LPAMTS.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBLP/LPLCHG.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBLP/LPUPWK.CPY".

      **************************************************
       VDU-CONVERT-FIELD SECTION.
      **************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/LPPBAT_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *********************************
       MISC-PARAGRAPHS SECTION.
      *********************************
       COPY "LIBGB/ACCESS.CPY".
       FORM-RESET.
       SETUP-LINK-INFO.
       CREATE-NOTICE-RECORD.
           MOVE TRANS-DATE TO CRNOR2-SYSDATE.
           MOVE "  "       TO CRNOR2-LPTRCD.
           MOVE LN-ACCTNO  TO CRNOR2-ACCTNO.
           MOVE "N"        TO CRNOR2-LPREC-FG.
           MOVE "CL/CRNOR2"   TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CRNOR2-LINK NO-REC
                                 POFF-WORKERS LN-REC.
           CANCEL FORM-PROGX.
       GET-FD.
           PERFORM OPEN-FD1-FILE.
           PERFORM READ-FD1-FILE.
           IF IO-FG NOT = 0
               MOVE "N" TO FD-LNMEMO
               MOVE SPACES TO FD-DESC FD-FMTYPE.
           PERFORM CLOSE-FD1-FILE.
       GET-BW1.
           PERFORM READ-BW1-FILE.
           IF IO-FG = 9
              MOVE SPACES TO BW-REC
              MOVE 0 TO BW-PHONE BW-SSNO BW-BDATE BW-REFDATE.
       ADD-IT.
           INSPECT C-S-Z REPLACING FIRST "  " BY " +".
           INSPECT C-S-Z REPLACING FIRST
              "+                        " BY ADD-ON.
       CAT-CSZ.
           INSPECT C-S-Z REPLACING FIRST "      " BY ", ++ +".
           INSPECT C-S-Z REPLACING FIRST "++" BY STATE.
           INSPECT C-S-Z REPLACING FIRST "+         " BY ZIP.

      ***************************************
       IO-ROUTINE SECTION.
      ***************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPSTGS_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLTPGS_SQL.CPY".
       COPY "LIBLP/LPNOGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLTIGS_SQL.CPY".
       COPY "LIBLP/LPLPGS_SQL.CPY".
       COPY "LIBLP/LPLMGS_SQL.CPY".
       COPY "LIBLP/LPFMGS_SQL.CPY".
       COPY "LIBLP/LPFDGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBGBGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGBRN.CPY".


       OPEN-WORLD-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE WORLD-PATH TO E-FILE.
           OPEN OUTPUT WORLD-FILE.
           IF IO-FG = 8
              GO TO OPEN-WORLD-FILE-OUTPUT.
           IF IO-FG = 7
              CLOSE WORLD-FILE
              GO TO OPEN-WORLD-FILE-OUTPUT.
       OPEN-WORLD-FILE-EXTEND.
           PERFORM OPEN-IT.
           MOVE WORLD-PATH TO E-FILE.
           OPEN EXTEND WORLD-FILE.
           IF IO-FG = 8
              GO TO OPEN-WORLD-FILE-EXTEND.
           IF IO-FG = 7
              CLOSE WORLD-FILE
              GO TO OPEN-WORLD-FILE-EXTEND.
       WRITE-WORLD-FILE.
           PERFORM WRITE-IT.
           MOVE WORLD-PATH TO E-FILE.
           WRITE WORLD-REC.
           IF IO-FG = 8
              GO TO WRITE-WORLD-FILE.
       CLOSE-WORLD-FILE.
           PERFORM CLOSE-IT.
           CLOSE WORLD-FILE.

       COPY "LIBLP/LPLTP1RN.CPY".
       COPY "LIBLP/LPFD1RN.CPY".
       COPY "LIBLP/LPST1RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPNO1RN.CPY".
       COPY "LIBLP/LPLN1IN.CPY".
       COPY "LIBLP/LPLTI1RN.CPY".
       COPY "LIBLP/LPLP1RN.CPY".
       COPY "LIBLP/LPLM1IN.CPY".
       COPY "LIBLP/LPFM1IN.CPY".
       COPY "LIBLP/LPFM2RN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      **************************
       END-ROUTINE SECTION.
      **************************

           PERFORM CLOSE-FILES.

       EXIT-PROG.
       STOP-RUN.
           EXIT PROGRAM.
