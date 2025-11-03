       IDENTIFICATION DIVISION.
       PROGRAM-ID.      CRNOR2.
       DATE-WRITTEN.    012392.
      **************************************************************************
      *   DIRECTORY  : CL
      *
      *    IT SETS UP THE NECESSARY ALL PATHS.
      ********************************************************************
      *
      *   DESCRIPTION: CALLED PROGRAM TO BUILD NOFILE RECORD
      *                AND SETUP ALL PAYOFF (POFF-WORKERS)
      *
      *
      *   HOW TO USE:
      *     MOVE  ACCOUNT NO     TO CRNOR2-ACCTNO
      *     MOVE  SYSTEM DATE    TO CRNOR2-SYSDATE
      *     SET   PAYOFF CODE    TO CRNOR2-LPTRCD          ----> 'PY' BY DEFAULT
      *     SET     " " OR "Y"   TO CRNOR2-GET-PAYOFF-ONLY ----> ' '  BY DEFAULT
      *     SET     " " OR "Y"   TO CRNOR2-RECAST-FG       ----> ' '  BY DEFAULT
      *     SET     " " OR "Y"   TO CRNOR2-DEF-STOP   ----> ' '  BY DEFAULT
      *     SET     " " OR "N"   TO CRNOR2-LPREC-FG 'N'  NO READ OF LPFILE
      *     MOVE "CL/CRNOR2" TO FORM-NAM
      *     CALL FORM-PROGX USING FORM-PATH CRNOR2-LINK NO-REC POFF-WORKERS
      *                                     LN-REC.
      *     CANCEL FORM-PROGX
      *
      *   OUTPUT:
      *     THE NO-REC AREA IS BUILT  
      *     IF AN ERROR OCCURS THE NO-REC AREA IS SET TO ZEROS
      *     ALONG WITH;
      *        CRNOR2-AVCR-RESTRICT-FG      PIC X.
      *        CRNOR2-CDEL-ARREARAGE        PIC S9(7)V99.
      *        CRNOR2-INDU-PENALTY          PIC S9(5)V99.
      *        CRNOR2-CPOT30                PIC X      VALUE " ".
      *        CRNOR2-RDPOT30               PIC X      VALUE " ".
      *        CRNOR2-CPOTCHGOFF            PIC X      VALUE " ".
      *        CRNOR2-RDPOTCHGOFF           PIC X      VALUE " ".
      *        CRNOR2-CSUB                  PIC S9(6)  VALUE 0.
      *        CRNOR2-RSUB                  PIC S9(6)  VALUE 0.
      *        CRNOR2-CDEL-DELINQUENT       PIC S9(7)V99.
      *
      *   COPY: LIBCL/CRNOR2W
      *
      * REV:
      *  JTG 061695 ADDED SET OF BR-NOT-OURS-REFUND
      *  KEC 083195 ADDED LIBLP/LPNOCL
      *  BAH 011696 ADDED CRNOR2-LPTRCD TO GET A TRUE "PO" PAYOFF
      *  MJD 071696 ADDED LIBLP/LPUPWK FOR CHANGES IN LIBLP/AGEING
      *  JTG 102896 ADDED CRNOR2-GET-PAYOFF-ONLY, FOR PAYOFF GOOD THRU ROUTINE
      *  BAH 121396 SKIP RESCISSION LOANS
      *  GLF 050797 ADDED COPY MEMBERS: SETCTBLW AND SETCTBL
      *  MJD 051597 ADDED LPPOF2 COPY MEMBER (PREVIOUSLY WAS PART OF LPPOFF)
      *  JTG 052097 ADDED 'LIBLP/ADDONSPR' TO GET CORRECT SPR RE: INS RB
      *  JTG 111797 ADDED LOGIC FOR LIBLP/DEFPOL RE: BR-NODEF-1ST-X-MONTHS #167
      *  JTG 122297 ADDED SET OF CRNO-RECAST-FG FROM CRNOR2-RECAST-FG
      *             #617 RE: IDEAL A48 TO A60
      *  BAH 030325 ADDED SET OF LT-PATH AND LP-PATH FOR CROSSING BRANCHES
      *  BAH 160125 SPLIT OUT LTFILE, PD0003
      *  BAH 161102 ADDED READ-BW1-FILE FOR CHANGE TO LPAVCR WORLD PR1199
      *             SC AVAILABLE CREDIT RESTRICTION
      * BAH 2018.07.02 ADDED ADDONSPRW 
      * BAH 1028.07.17 CHANGED TO READ THE SPR INSTEAD OF PASSING IT.
      *                LONPS3 DOC DESIGN HAD ACCOUNT # BUT NOT SPR. CRNORC
      *                READ THE SPR INSTEAD OF PASSING SO I THOUGHT THIS WAS
      *                OK TO GO BACK TO.
      * BAH 181221 GLOBAL LPFILE
      * BAH 190211 SPLIT SPFILE
      * BAH 20190625 REMOVED LN-REVOLVING, PT0153
      * BAH 2019.0708 WAS TESTING NXDUE INSTEAD OF LDUE FOR PRIOR TO 1STPYDATE
      *               SO LDUE WAS NOT GETTING ZEROED OUT! PR#1375
      * BAH 20220802 HARD CODED START LP
      * BAH 20220818 REMOVED GLOBRD, READ GB-FILE INSTEAD
      * BAH 20240201 REMOVED NO-FMNO, NO-PRINTED AND NO-WAGE-ASSIGN-DELINQ
      *              ADDED FIELDS AT END OF REC
      * BAH 20240617 CALLING FROM LNMAN2, YOU DO NOT HAVE A 1STPYDATE AND IT
      *              CAUSES PAID THRU TO GIVE YOU A BOGUS DATE AND NEVER COME
      *              OUT , YOU END UP IN A LOOP, SET TO ORIG-1STPYDATE IF IT
      *              EXIST, OR EXIT WITH AN INITIALIZED NO-REC S35Q-65
      **************************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)CL/CRNOR2 S35 06/24/24-09:32:04 >".
      **************************************************
      *     P R O G R A M   W O R K E R S              *
      **************************************************
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GB.CPY".
       COPY "LIBGB/GB01GB_SQL.CPY".
       COPY "LIBGB/GBWSGB.CPY".
       COPY "LIBLP/LP01LP.CPY".
       COPY "LIBLP/LP01LP_SQL.CPY".
       COPY "LIBLP/LPWSLP.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01LTI.CPY".
       COPY "LIBLP/LP01LTI_SQL.CPY".
       COPY "LIBLP/LPWSLTI.CPY".
       COPY "LIBLP/LP01LTP.CPY".
       COPY "LIBLP/LP01LTP_SQL.CPY".
       COPY "LIBLP/LPWSLTP.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBLP/LPCRNOW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBLP/SETCTBLW.CPY".
       COPY "LIBGB/SPPDTHW.CPY".
       COPY "LIBLP/ADDONSPRW.CPY".
       COPY "LIBLP/LPSCHDW.CPY".
       COPY "LIBLP/LPLCHGW.CPY".
       COPY "LIBLP/LPLCAPW.CPY".
       COPY "LIBLP/LPLCASW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBLP/LPXDTEW.CPY".
       COPY "LIBLP/LPDEFW.CPY".
       COPY "LIBLP/DEFPOLW.CPY".
       COPY "LIBLP/LPCDELW.CPY".
       COPY "LIBLP/LPIBPCW.CPY".
       COPY "LIBLP/LPRATEW.CPY".
       COPY "LIBLP/LPAVCRW.CPY".
       COPY "LIBGB/REBATEW.CPY".
       COPY "LIBLP/LPAMTSW.CPY".
       COPY "LIBGB/AGEINGW.CPY".
       COPY "LIBLP/LPUPWKW.CPY".
       COPY "LIBLP/LPCEPPW.CPY".
       COPY "LIBLP/LPAPRSW.CPY".
       COPY "LIBLP/LPAPRZW.CPY".
       COPY "LIBLP/LPUPERW.CPY".
       COPY "LIBLP/LPITRMW.CPY".
       COPY "LIBLP/LPLOANW.CPY".
       COPY "LIBGB/RBACTW.CPY".
       COPY "LIBLP/LPPDUEW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".

      /*************************************************
      *   C O N T R O L   M O D U L E                  *
      **************************************************
       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBCL/CRNOR2W.CPY".
       COPY "LIBLP/LP01NO.CPY".
       COPY "LIBLP/LPPOFFW.CPY".
       COPY "LIBLP/LP01LN.CPY".
       PROCEDURE DIVISION
           USING FORM-PATHNAME CRNOR2-LINK NO-REC POFF-WORKERS
                               LN-REC EXIT-PATHNAME.

      **************************************************
      *      P R O G R A M   C O N T R O L             *
      **************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.

           MOVE SPACES TO ERRCD.

           PERFORM GET-GPENV.

           MOVE LN-OWNBR TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.

           MOVE LN-ORGST    TO SP-ORGST.
           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           MOVE LN-LAWCODE  TO SP-LAWCODE.
      * MUST LEAVE SP1-FILE OPEN BECAUSE ADDONSPR READS IT
           PERFORM OPEN-SP1-FILE.
           PERFORM READ-SP1-FILE.
           IF IO-BAD
              PERFORM CREATE-NOREC-INITIALIZE
              GO TO END-ROUTINE.

           MOVE EXT-FILPATH-BASE TO FILPATH-BASE.
           MOVE BR-MACHINE       TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH     TO FILPATH-DATA.

           MOVE "B"        TO LTP-PATH-OVERRIDE
                              LTI-PATH-OVERRIDE
                              LP-PATH-OVERRIDE
                              BW-PATH-OVERRIDE
                              GB-PATH-OVERRIDE.

      * NEED FOR LCAS ROUTINES (REPLACES GLOBRD)
           PERFORM OPEN-GB-FILE.
           MOVE GB-PATH-OWNBR TO GB-BRNO.
           MOVE 1             TO GB-NO.
           PERFORM READ-GB-FILE.
           PERFORM CLOSE-GB-FILE.
           IF IO-BAD
              PERFORM CREATE-NOREC-INITIALIZE
              GO TO END-ROUTINE.

           MOVE LN-SSNO(1) TO BW-SSNO.
           PERFORM OPEN-BW1-FILE.
           PERFORM READ-BW1-FILE.
           PERFORM CLOSE-BW1-FILE.

           MOVE CRNOR2-SYSDATE         TO CRNO-PAYDATE.
           MOVE LN-MAKERCD(1)          TO CRNO-MAKERCD.
           MOVE CRNOR2-LPTRCD          TO CRNO-LPTRCD.
           MOVE CRNOR2-GET-PAYOFF-ONLY TO CRNO-GET-PAYOFF-ONLY.
           MOVE CRNOR2-RECAST-FG       TO CRNO-RECAST-FG.
           MOVE CRNOR2-DEF-STOP        TO CRNO-DEF-STOP.
           MOVE CRNOR2-LPREC-FG        TO CRNO-LPREC-FG.

      * CALLING FROM LNMAN2, YOU DO NOT HAVE A 1STPYDATE AND IT CAUSES PAID THRU
      * TO GIVE YOU A BOGUS DATE AND NEVER COME OUT , YOU END UP IN A LOOP
           IF LN-1STPYDATE = 0
              IF LN-ORIG-1STPYDATE NOT = 0
                 MOVE LN-ORIG-1STPYDATE TO LN-1STPYDATE
              ELSE
                 PERFORM CREATE-NOREC-INITIALIZE
                 GO TO END-ROUTINE.

           PERFORM CREATE-NOREC.

           MOVE LN-ACCTNO       TO NO-ACCTNO.
           MOVE LN-OWNBR        TO NO-BRNO.
      * COLAST NEEDED THIS FOR DEFERMENTS
           MOVE CRNO-AVCR-RESTRICT-FG TO NO-AVCR-RESTRICT-FG.
           MOVE INDU-PENALTY          TO NO-INDU-PENALTY.
           MOVE CPOT30                TO NO-CPOT30.
           MOVE RDPOT30               TO NO-RDPOT30.
           MOVE CPOTCHGOFF            TO NO-CPOTCHGOFF.
           MOVE RDPOTCHGOFF           TO NO-RDPOTCHGOFF.
           MOVE CSUB                  TO NO-CSUB.
           MOVE RSUB                  TO NO-RSUB.

      * RESET DEFAULTS:
           MOVE " " TO CRNOR2-GET-PAYOFF-ONLY.

           GO TO END-ROUTINE.

      ***********************************************************************
      *         CREATE ALL FIELDS FOR NOTICE RECORD
      *
      *   DESC: THIS ROUTINE GENERATES ALL THE FIELDS
      *         FOR THE NOTICE RECORD:
      *
      *   IN  : CRNO-PAYDATE             DATE OF PAYMENT
      *         CRNO-MAKERCD             LN-MAKERCD(X)
      *         CRNO-LPTRCD              (PY) BY DEFAULT VALUE CLAUSE
      *         CRNO-LPREC-FG            " " - GET LAST LPREC KEY
      *                                  "N" - BYPASS GET OF LPREC KEY
      *                                        NOTE: FLAG IS CLEARED TO SPACES
      *                                              UPON EXIT OF ROUTINE
      *         CRNO-DEF-STOP       "Y" - STOP FOR DEFERMENT REBATES MESS
      *         CRNO-RECAST-FG           "Y" - ALLOW RECASTING
      *                                        NOTE: FLAG IS CLEARED TO SPACES
      *                                              UPON EXIT OF ROUTINE
      *         CRNO-GET-PAYOFF-ONLY     "Y" - GET PAYOFF INFO ONLY
      *                                        NOTE: FLAG IS CLEARED TO SPACES
      *                                              UPON EXIT OF ROUTINE
      *
      *   OUT : NO-REC (ALL FIELDS LESS KEY)
      *
      ******************************************************************
       CREATE-NOREC SECTION.
           PERFORM CREATE-NOREC-INITIALIZE.

      * COMPUTE CONTRACTUAL & RECENCY:
           MOVE CRNO-PAYDATE TO AGEING-DATE.
           PERFORM COMPUTE-CONTRACTUAL.
           MOVE CONTRACTUAL TO NO-TRUCONTR.
           PERFORM COMPUTE-RECENCY.
           MOVE RECENCY TO NO-RECENCY.
           MOVE RECDEL  TO NO-RECDEL.

      * COMPUTE TOTAL OF ALL BALANCES:
           COMPUTE NO-TDUETBAL =
                LN-CURBAL + LN-OTHBAL + LN-INTBAL + LN-LCBAL.

      * WORLD PR# 254, IF ACCOUNT PAID AHEAD BECAUSE OF LATE CHARGES,
      * THE TODAY DUE WOULD BE 0, BUT NEXT DUE WOULD BE 3 PAYMENTS
      * PLUS LATE CHARGES, THEY WANT TO SEE 1 PAYMENT DUE, AND NO LATE
           MOVE SP-CONTRFRMLA TO CRNO-HOLD-CONTRFRMLA.
           IF GP-IGNORE-LCHG-NXDUE = "Y"
              IF SP-CONTRFRMLA-TOTAL
                 MOVE " " TO SP-CONTRFRMLA
                 MOVE LN-TOTPAYMNTD TO CRNO-HOLD-TOTPAYMNTD
                 COMPUTE LN-TOTPAYMNTD = LN-TOTPAYMNTD - LN-TOTLCHG.

      * COMPUTE CONTRACTUAL AND LATE CHARGE
      * PAID THRU DATES:
           MOVE LN-1STPYDATE TO PDTH-DATE-WORK.
           PERFORM PAID-THRU-CALCULATION.
           MOVE PDTH-DATE-FULL TO NO-PDTH-DATE.

           MOVE LN-LCPDTH-DATE TO NO-LCPDTH-DATE.

      *  RETURNS NEXT SCHEDULED PAYMENT AMOUNT
      *  AND IF PAYMENT IS PARTIALLY PAID, IT STILL RETURNS IT
           MOVE PDTH-CUR-SCHD-PAY TO NO-SCHED-PAYMENT.

      ***********************************************
      *    COMPUTE TODAY DUE FIGURES:
      ***********************************************
      * IF WDT, AVAIL CREDIT ROUTINE PERFORMS PAYOFF-LOAN ROUTINE FOR
      * A PB PAYOFF TO GET NETDUE FOR THE CURRENT AVAILABLE FIELD,
      * SO DONT DO PAYOFF ROUTINE HERE
           IF BR-AVAIL-CREDIT NOT = "S"
              MOVE CRNO-PAYDATE TO NO-TDUE-DATE
                                   POFF-PAYDATE
              MOVE CRNO-MAKERCD TO POFF-MAKERCD
              MOVE CRNO-DEF-STOP TO POFF-DEF-STOP
              MOVE CRNO-LPTRCD TO POFF-LPTRCD
              MOVE CRNO-RECAST-FG TO POFF-RECAST-FG
              PERFORM PAYOFF-LOAN-ROUTINE
              MOVE POFF-NETDUE TO NO-TDUEPOFF.


           MOVE POFF-NETDUE TO AVCR-POFF-NETDUE.
           MOVE CRNO-PAYDATE TO AVCR-PAYDATE.
      * ---THIS FLAG STOPS COMPUTE-AVAILABLE-CREDIT ROUTINE
      *    FROM COMPUTING A PAYOFF:
           MOVE "Y" TO AVCR-POFF-FG.
           MOVE CRNO-DEF-STOP TO AVCR-DEF-STOP.
           PERFORM COMPUTE-AVAILABLE-CREDIT.
           MOVE AVCR-AVAILCREDIT TO NO-AVAILCREDIT.
           MOVE AVCR-RESTRICT-FG TO CRNO-AVCR-RESTRICT-FG.
           MOVE AVCR-CURRENT-AVAIL TO NO-CURRENT-AVAIL.

      * IF WDT, AVAIL CREDIT ROUTINE PERFORMS PAYOFF-LOAN ROUTINE FOR
      * A PB PAYOFF TO GET NETDUE FOR THE CURRENT AVAILABLE FIELD,
      * SO MUST DO PAYOFF ROUTINE HERE
           IF BR-AVAIL-CREDIT = "S"
              MOVE CRNO-PAYDATE TO NO-TDUE-DATE POFF-PAYDATE
              MOVE CRNO-MAKERCD TO POFF-MAKERCD
              MOVE CRNO-DEF-STOP TO POFF-DEF-STOP
              MOVE CRNO-LPTRCD TO POFF-LPTRCD
              MOVE CRNO-RECAST-FG TO POFF-RECAST-FG
              PERFORM PAYOFF-LOAN-ROUTINE
              MOVE POFF-NETDUE TO NO-TDUEPOFF.

      * TEST TO GET PAYOFF ONLY:
           IF CRNO-GET-PAYOFF-ONLY = "Y"
              GO TO CREATE-NOREC-REC-EXIT.

      * COMPUTE PRINCIPAL PAYMENTS DUE TODAY:
           MOVE LN-1STPYDATE TO CEPP1-DATE.
           MOVE NO-TDUE-DATE TO CEPP2-DATE.
           PERFORM CONTRACTUAL-DELINQUENCY.
           IF CDEL-DELINQUENT < 0
              SUBTRACT CDEL-DELINQUENT FROM 0
                                   GIVING NO-TDUECONTR.
           IF CDEL-ARREARAGE < 0
              SUBTRACT CDEL-ARREARAGE FROM 0
                                   GIVING NO-TDUEPRIN
                                          NO-TDUEAMT.
      * SET CONTRACTUAL PRINCIPLE (DELINQUENCY) FOR
      * (PC) ACCOUNTS.
            IF CDEL-DELINQUENT < 0
               IF IBPC-FG = "P"
                  SUBTRACT CDEL-DELINQUENT FROM 0
                                        GIVING NO-TDUEDELINQ.

      * SETUP LC OR INT AMOUNTS:
           ADD POFF-LCDUE TO NO-TDUELC
                             NO-TDUEAMT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * JTG 000310 CHANGED NO-LCHG-INTDUE TO REFLECT ALL LATE CHARGES
      *            PLUS INTEREST DUE TODAY:
      *
           MOVE LN-LCPDTH-DATE TO LCHG-LCPDTH-DATE.
           MOVE PDTH-CUR-SCHD-PAY TO LCHG-CURPAY
                                     LCHG-LATE.
           PERFORM LATE-CHARGE-CALCULATION.
           MOVE LCHG-CHARGE TO NO-LATE-CHG.

           ADD POFF-INTDUE POFF-LCDUE GIVING NO-LCHG-INTDUE.
      *
      *
      *   - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *   ONLY TAKE LATE CHARGES WHEN THE ACCOUNT IS NOT ACCELERATED
      *   AND IT IS IN A PRECOMP STATUS OR IS LOAN CLASS TYPE IB
      *    IF (LN-ACCELDATE = 0) AND
      *       (IBPC-FG = "P" OR LN-LOANTYPE = "I")
      *       MOVE LN-LCPDTH-DATE TO LCHG-LCPDTH-DATE
      *       MOVE PDTH-CUR-SCHD-PAY TO LCHG-CURPAY
      *                                 LCHG-LATE
      *       PERFORM LATE-CHARGE-CALCULATION
      *       ADD POFF-INTDUE LCHG-CHARGE GIVING NO-LCHG-INTDUE
      *       MOVE LCHG-CHARGE TO NO-LATE-CHG
      *    ELSE
      *       MOVE POFF-LCDUE TO NO-LATE-CHG
      *       ADD POFF-INTDUE POFF-LCDUE GIVING NO-LCHG-INTDUE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    SETUP TRUE PAST DUE DAYS
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE CRNO-PAYDATE TO PDUE-TODAY-DATE.
           MOVE LN-1STPYDATE TO PDUE-1STPYDATE.
           PERFORM PAST-DUE-DAYS-CALCULATION.
           MOVE PDUE-DAYS TO NO-PAST-DUE-DAYS.

      * COMPUTE MATURITY DATE:
           PERFORM MATURITY-DATE-CALCULATION.
           IF NO-TDUE-DATE > MDTE-DATE
              MOVE NO-TDUEPOFF TO NO-TDUEAMT.

      ***********************************************
      *    SETUP LAST AND NEXT DUE DATES:
      ***********************************************
           MOVE LN-UNITPER-CD TO DATER-UNITPER-CD.
           MOVE LN-UNITPER-FREQ TO DATER-UNITPER-FREQ.

           MOVE LN-1STPYDATE TO NDTE-DATE.
           MOVE NDTE-DD      TO CRNO-HOLDDA.
           COMPUTE NDTE-HOLD = LN-TOTNODEF + LN-TOTNOALDEL - 1.
           PERFORM INCREMENT-UNITPER.

           MOVE NDTE-DATE    TO NUM-DATE.
           MOVE NO-TDUE-DATE TO SYS-DATE.
           PERFORM TIMUPER.
           SUBTRACT 1 FROM ELAPSED-UNITPER GIVING NDTE-HOLD.

       CREATE-NOREC-INCR-DATE.
           IF LN-UNITPER-CD = "M"
              MOVE CRNO-HOLDDA TO NDTE-DD.

           MOVE NDTE-DATE TO NO-LDUE-DATE NUM-DATE.
           PERFORM INCREMENT-UNITPER.

      * 1STPYDATE 10/14/01 "SEMI"
      * TODAY = 3/26/02   LCPDTH 3/14/02
      * THE INQUIRIES WERE SHOWING  LAST DUE = 3/13 (SHOULD BE 14)
      *                            TODAY DUE = 3/26
      *                             NEXT DUE = 3/28 (SHOULD BE 29)
      * 1STPYDATE 1/30/02 "SEMI" LOANDATE 1/15/02
      * TODAY = 3/26/02   LCPDTH 2/28/02
      * THE INQUIRIES WERE SHOWING  LAST DUE = 3/13 (SHOULD BE 15)
      *                            TODAY DUE = 3/26
      *                             NEXT DUE = 3/28 (SHOULD BE 30)
      * BAH PL# 357 (SAME LOGIC IN LPLCAP FROM JTG AND CS)
      *
      *  2/16/2022 CINDY - IF USING A 1ST PAY DAY OF 30TH, NEXT DUE
      *             DATE WILL BE 3/13 AND SHOULD BE THE 15TH.
           IF LN-UNITPER-CD = "S"
              IF (NUM-DA = 28 OR 29) AND NDTE-MM = 03
                 IF LN-1STPYDA = 14 OR 15
                    MOVE LN-1STPYDA TO NDTE-DD
                 ELSE
                 IF LN-1STPYDA = 29 OR 30 OR 31
                    SUBTRACT NUM-DA FROM LN-1STPYDA GIVING NDTE-HOLD
                    PERFORM INCREMENT-DAYS.

      * BAH PL# 357

           IF NO-TDUE-DATE = NDTE-DATE
              MOVE 1 TO NDTE-HOLD
              PERFORM INCREMENT-UNITPER
              MOVE NDTE-DATE TO NO-NXDUE-DATE
           ELSE
              IF NO-TDUE-DATE < NDTE-DATE
                 MOVE NDTE-DATE TO NO-NXDUE-DATE
              ELSE
                 MOVE 1 TO NDTE-HOLD
                 GO TO CREATE-NOREC-INCR-DATE.

      * ADJUST DUE DATES TO PAYDATE:
           IF LN-UNITPER-CD = "M"
              MOVE NO-LDUE-DATE TO NDTE-DATE
              MOVE CRNO-HOLDDA TO NDTE-DD
              PERFORM NEW-DATE-CALCULATION
              MOVE NDTE-DATE TO NO-LDUE-DATE
              MOVE NO-NXDUE-DATE TO NDTE-DATE
              MOVE CRNO-HOLDDA TO NDTE-DD
              PERFORM NEW-DATE-CALCULATION
              MOVE NDTE-DATE TO NO-NXDUE-DATE.

      * TEST IF NEXT DUE IS PRIOR TO PAID THRU:
           IF (NO-NXDUE-DATE > NO-PDTH-DATE) OR
              (BR-PDTH-NXDUE = "Y" AND (NO-NXDUE-DATE = NO-PDTH-DATE) )
              NEXT SENTENCE
           ELSE
              MOVE NO-PDTH-DATE TO NDTE-DATE
              MOVE 1 TO NDTE-HOLD
              IF LN-UNITPER-CD = "M"
                 MOVE CRNO-HOLDDA TO NDTE-DD
                 PERFORM INCREMENT-UNITPER
                 MOVE NDTE-DATE TO NO-NXDUE-DATE
              ELSE
                 PERFORM INCREMENT-UNITPER
                 MOVE NDTE-DATE TO NO-NXDUE-DATE.

      * TEST IF AFTER MATURITY DATE:
           IF NO-LDUE-DATE NOT < MDTE-DATE
              MOVE MDTE-DATE TO NO-LDUE-DATE
              MOVE 0 TO NO-NXDUE-DATE
              GO TO NOTICE-RECORD-LDUE
           ELSE
              IF NO-NXDUE-DATE > MDTE-DATE
                 MOVE 0 TO NO-NXDUE-DATE
                 GO TO NOTICE-RECORD-LDUE.

      * TEST IF PRIOR TO 1STPYDATE:
           IF LN-1STPYDATE > NO-NXDUE-DATE
              MOVE 0 TO NO-LDUE-DATE
                        NO-NXDUE-DATE
           ELSE
              IF LN-1STPYDATE > NO-LDUE-DATE
                 MOVE 0 TO NO-LDUE-DATE.

      ***********************************************
      *    COMPUTE LAST DUE FIGURES:
      ***********************************************
       NOTICE-RECORD-LDUE.
           IF NO-LDUE-DATE NOT = 0
              MOVE LN-1STPYDATE TO CEPP1-DATE
              MOVE NO-LDUE-DATE TO CEPP2-DATE

      *     ....................................................
      *     SET CDEL-LPCRNO-FG TO LET CONTRACTUAL-DELINQUENCY
      *     ROUTINE COMPUTE CORRECT AMOUNT DUE WHEN LAST DUE
      *     DATE DOSN'T FALL ON MONTHLY CYCLE EX. PAYMENTS DUE
      *     ON 31TH AND LAST DUE IS 02/28
              MOVE "Y" TO CDEL-LPCRNO-FG
              PERFORM CONTRACTUAL-DELINQUENCY
              MOVE " " TO CDEL-LPCRNO-FG
      *     ....................................................

              IF CDEL-DELINQUENT < 0
                 SUBTRACT CDEL-DELINQUENT FROM 0
                                      GIVING NO-LDUECONTR
              END-IF
              IF CDEL-ARREARAGE < 0
                 SUBTRACT CDEL-ARREARAGE FROM 0
                                      GIVING NO-LDUEAMT
                 MOVE 0 TO LCAP-OWE
      *   - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *         ONLY TAKE LATE CHARGES WHEN THE ACCOUNT IS NOT ACCELERATED
      *         AND IT IS IN A PRECOMP STATUS OR IS LOAN CLASS TYPE IB
                 IF (LN-ACCELDATE = 0) AND
                    (IBPC-FG = "P" OR LN-LOANTYPE = "I")
                    MOVE NO-LDUE-DATE TO LCAP-PAYDATE
                    MOVE LN-1STPYDATE TO LCAP-1STPYDATE
                    MOVE "AL" TO LCAP-LPTRCD
                    MOVE 0 TO LCAP-LPAPLC
                              LCAP-LPAPINT
                    PERFORM LATE-CHARGE-APPLY
                 END-IF
                 ADD LN-LCBAL LCAP-OWE GIVING NO-LDUELC
                 ADD NO-LDUELC TO NO-LDUEAMT.

      ***********************************************
      *    COMPUTE NEXT DUE FIGURES:
      ***********************************************
       NOTICE-RECORD-NXDUE.
           IF NO-NXDUE-DATE NOT = 0
              MOVE LN-1STPYDATE TO CEPP1-DATE
              MOVE NO-NXDUE-DATE TO CEPP2-DATE
      *     ....................................................
      *     SET CDEL-LPCRNO-FG TO LET CONTRACTUAL-DELINQUENCY
      *     ROUTINE COMPUTE CORRECT AMOUNT DUE WHEN NEXT DUE
      *     DATE DOSN'T FALL ON MONTHLY CYCLE EX. PAYMENTS DUE
      *     ON 31TH AND LAST DUE IS 02/28
              MOVE "Y" TO CDEL-LPCRNO-FG
              PERFORM CONTRACTUAL-DELINQUENCY
              MOVE " " TO CDEL-LPCRNO-FG
      *     ....................................................
              IF CDEL-DELINQUENT < 0
                 SUBTRACT CDEL-DELINQUENT FROM 0
                                      GIVING NO-NXDUECONTR
              END-IF
              IF CDEL-ARREARAGE < 0
                 SUBTRACT CDEL-ARREARAGE FROM 0
                                      GIVING NO-NXDUEAMT
                 MOVE 0 TO LCAP-OWE
      *         - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *         ONLY TAKE LATE CHARGES WHEN THE ACCOUNT IS NOT ACCELERATED
      *         AND IT IS IN A PRECOMP STATUS OR IS LOAN CLASS TYPE IB
                 IF (LN-ACCELDATE = 0) AND
                    (IBPC-FG = "P" OR LN-LOANTYPE = "I")
                    MOVE NO-NXDUE-DATE TO LCAP-PAYDATE
                    MOVE LN-1STPYDATE TO LCAP-1STPYDATE
                    MOVE "AL" TO LCAP-LPTRCD
                    MOVE 0 TO LCAP-LPAPLC
                              LCAP-LPAPINT
                    PERFORM LATE-CHARGE-APPLY
                 END-IF
                 ADD LN-LCBAL LCAP-OWE GIVING NO-NXDUELC
                 ADD NO-NXDUELC TO NO-NXDUEAMT.

      ***********************************************
      *    SETUP TRUE DUE DATE BASED ON PAID THRU
      ***********************************************
       UPDATE-TRUE-DUE-DATE.
           MOVE NO-PDTH-DATE TO NDTE-DATE.
      * ADDED IF 3/16/98, 1STPYDATE 7/30/97 , PAID THRU 2/28/98,
      * NEXT DUE 3/30/98 BUT TRUEDUE WAS SHOWING 3/28/98
           IF LN-UNITPER-CD = "M"
              MOVE CRNO-HOLDDA TO NDTE-DD.
           MOVE 1 TO NDTE-HOLD.
           PERFORM INCREMENT-UNITPER.
           MOVE NDTE-DATE TO NO-TRUE-DUE-DATE.

      ***********************************************
      *    SETUP DEFERMENT AMOUNT FOR TODAY
      ***********************************************
           IF LN-CURBAL NOT = 0
              MOVE "DF" TO DEFPOL-TRCD
              MOVE CRNO-PAYDATE TO DEFPOL-DATE
              PERFORM VALIDATE-DF-POLICY
              IF DEFPOL-MAXIMUM = " "
                 MOVE 1            TO DEF-NODEF
                 MOVE CRNO-PAYDATE TO DEF-PAYDATE
                 MOVE POFF-INTDUE  TO DEF-TOTAL-INTDUE
                 PERFORM DEF-COMPUTE-DEFERMENT
                 MOVE DEF-AMOUNT   TO NO-DEFERMENT.


      ***********************************************
      *    SETUP LAST PAYMENT INFORMATION
      ***********************************************
       UPDATE-LAST-PAYMENT.
           MOVE 0 TO NO-DATE-PAID-LAST
                     NO-AMT-PAID-LAST.
           IF CRNO-LPREC-FG NOT = " "
              GO TO CREATE-NOREC-REC-EXIT.

           PERFORM OPEN-LP1-FILE.

           MOVE LP-PATH-OWNBR TO LP-BRNO
                                 QLP1-WBEG-BRNO
                                 QLP1-WEND-BRNO.
           MOVE LN-ACCTNO     TO LP-ACCTNO
                                 QLP1-WBEG-ACCTNO
                                 QLP1-WEND-ACCTNO.
           MOVE 99999         TO LP-SEQNO
                                 QLP1-WEND-SEQNO.
           MOVE ALL "0"       TO QLP1-WBEG-SEQNO.

           PERFORM START-LP1-FILE-NOT-GREATER.
           PERFORM READ-LP1-FILE-PREVIOUS.
           PERFORM CLOSE-LP1-FILE.
           IF IO-BAD OR (LP-BRNO NOT = LP-PATH-OWNBR)
                      OR (LN-ACCTNO NOT = LP-ACCTNO)
                       OR (LP-SEQNO = 1)
              GO TO CREATE-NOREC-REC-EXIT.

           MOVE LP-PAYDATE TO NO-DATE-PAID-LAST.
           MOVE LP-TRAMT   TO NO-AMT-PAID-LAST.
           MOVE LP1-KEY    TO NO-LSTLPKEY.

       CREATE-NOREC-REC-EXIT.
           MOVE " " TO CRNO-LPREC-FG
                       CRNO-RECAST-FG
                       CRNO-GET-PAYOFF-ONLY
                       CRNO-DEF-STOP.
           IF GP-IGNORE-LCHG-NXDUE = "Y"
              IF CRNO-HOLD-CONTRFRMLA = "A"
                 MOVE CRNO-HOLD-CONTRFRMLA TO SP-CONTRFRMLA
                 MOVE CRNO-HOLD-TOTPAYMNTD TO LN-TOTPAYMNTD.

      ***************************************
       CREATE-NOREC-INITIALIZE SECTION.
      ***************************************
           PERFORM CLEAR-NO-FILE.

           MOVE LN-OWNBR  TO NO-BRNO.
           MOVE LN-ACCTNO TO NO-ACCTNO.

      /*************************************
      *     VARIOUS COPY MEMBERS
      ************************************
       COPY "LIBLP/LPNOCL.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/SETCTBL.CPY".
       COPY "LIBLP/LPITRM.CPY".
       COPY "LIBGB/REBATE.CPY".
       COPY "LIBGB/SPPDTH.CPY".
       COPY "LIBLP/LPSCHD.CPY".
       COPY "LIBLP/LPLCHG.CPY".
       COPY "LIBLP/LPLCAP.CPY".
       COPY "LIBLP/LPAVCR.CPY".
       COPY "LIBLP/LPLCAS.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBLP/LPXDTE.CPY".
       COPY "LIBLP/LPPOFF.CPY".
       COPY "LIBLP/LPPOF2.CPY".
       COPY "LIBLP/LPDEF.CPY".
       COPY "LIBLP/DEFPOL.CPY".
       COPY "LIBLP/LPIBPC.CPY".
       COPY "LIBLP/LPRATE.CPY".
       COPY "LIBLP/LPCDEL.CPY".
       COPY "LIBGB/AGEING.CPY".
       COPY "LIBLP/LPUPWK.CPY".
       COPY "LIBLP/LPCEPP.CPY".
       COPY "LIBLP/LPAMTS.CPY".
       COPY "LIBLP/LPAPRS.CPY".
       COPY "LIBLP/LPAPRZ.CPY".
       COPY "LIBLP/LPUPER.CPY".
       COPY "LIBLP/LPTDEL.CPY".
       COPY "LIBGB/RBACT.CPY".
       COPY "LIBLP/LPPDUE.CPY".
       COPY "LIBLP/ADDONSPR.CPY".
       COPY "LIBGB/GPENV.CPY".

      ********************************************
       MISC-ROUTINES SECTION.
      ********************************************
       FORM-RESET.
       SETUP-LINK-INFO.
       VDU-CONVERT-FIELD.
           MOVE "00" TO STAT.

      ********************************************
       IO-ROUTINES SECTION.
      ********************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LTP1-FILE.
           PERFORM CLOSE-LTI1-FILE.
           PERFORM CLOSE-LP1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-GB-FILE.
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLTPGS_SQL.CPY".
       COPY "LIBLP/LPLTIGS_SQL.CPY".
       COPY "LIBLP/LPLPGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBGBGS_SQL.CPY".
      ********************************************
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGBRN.CPY".
       COPY "LIBLP/LPLP1RN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPLTP1RN.CPY".
       COPY "LIBLP/LPLTI1RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.
       END-ROUTINE.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           EXIT PROGRAM.
