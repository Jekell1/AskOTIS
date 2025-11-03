       IDENTIFICATION DIVISION.
       PROGRAM-ID.      LONPD0.
       DATE-WRITTEN.    012484.
      *************************************************************************
      *                 (LP)
      *  DESCRIPTION:   DIRECT LOANS (NON SALES FINANCE)
      *                 NEW LOAN ENTRY - PAGE 2
      *
      * * * * * * * * * * *
      * WRITES REPAY NEW LOANS (11/28/2022) #1579
      * THERE IS A PROCESS SCANNING THIS DIRECTORY TO SEND
      * THESE IMMEDIATELY TO REPAY SO THEY CAN SET UP
      * AUTOMATIC PAYMENTS WHILE THE CUSTOMER IS STILL IN THE OFFICE
      * /USR/XXXX/R1/FTP//REPAY/NEWLOANS/BRNO.NUMBER
      *                                  1234.123456
      *   (FTP AND REPAY ARE LOWERCASE)
      * * * * * * * * * * *
      *
      * REV:
      *  JTG 033195 REMOVED REF: NO ODD PAYMENTS
      *  SLC 05??95 ADDED BR-NO-CASH-PROCEEDS
      *  062795 MJD ADDED COPY MEMBERS LIBLP/POSTDATE AND POSTDATEW.
      *             ADDED TC-POSTDATE, TC-POSTTIME, TC-USERID.
      *  011796 BAH F3-CHECK TO BORROWER THEN UP ARROW AND CHANGE
      *             NAME WAS NOT CLEARING THE BWPROC FLAG.
      *  030196 BAH CHANGED THE INSPECT OF LNAME, FNAME TO ALLOW
      *             FOR 2 SPACES BETWEEN NAMES, CHARLENE  M. DAVIS WAS
      *             COMING OUT AS CHARLENE +M., THE INSPECT WAS NOT
      *             ALLOWING 2 SPACES
      *  MJD 050696 FIXED BUG WITH DISBURSEMENTS. IF YOU TRY TO DISBURSE
      *              MORE THAN PROCEEDS AND HIT DOWN ARROW, IT WILL
      *              ACCEPT IT WITHOUT ERROR. ADDED LOGIC TO STOP
      *              DOWN ARROW FROM AMOUNT FIELD ENTIRELY.
      *  JTG 073196 ADDED CLEAR OF CX-DT-SEQ
      *  JTG 082996 ADDED FREE TYPING WINDOW, FTENT.C
      *  BAH 112596 ADDED RESCISSION, ADVANCE MTG, (#401)
      *  011597 BAH COMMENTED OUT CHANGE I MADE ON 01/17/96 FOR
      *             UP ARROWING AFTER PRESSING F3-CHECK TO BORROWER
      *             IF YOU PRESS F3 ONCE, YOU ARE GETTING CHECK
      *             TO BORROWER, NO MATTER WHAT, AS PER JR AND WDT
      *  082097 JTG ADDED LN-PROC-DISBURSED TO VERIFY THAT PAGE 2 WAS
      *             PROCESSED AND PROCEEDS WERE TRULY DISBURSED
      *  GLF 090297 REMOVED ALL USE OF GLOBAL
      *  MJD 101797 MOVED WK-SAVE-TR1-KEY TO LP01WK
      *  BAH 103097 INSPREM(6) WAS GETTING ADDED INTO PROCEEDS BECAUSE WE
      *             WERE ONLY LOOKING AT 5 INSURANCES !!!
      *  JTG 021698 ADDED SP-RBEARLY = 19, FOR WORLD OF SOUTH CAROLINA
      *  MJD 032098 ADDED GETENV(W) FOR USE IN LIBLP/POSTDATE
      *  BAH 032598 ADDED BR-IMMEDIATE-DOC-PRINT TO GO DIRECTLY TO DOCUMENT
      *             PRINT SCREEN, AFTER DISBURSEMENT, FOR WORLD , FROM A90
      *   CS 042998 ADDED NEW RENEWAL SOURCE CODES "RB" AND "RO"
      *  BAH 062598 TILLNO IS NOW ALPHA
      *  JTG 081798 CORRECTED TEST TO A  'AND CA-TILLNO NOT ALPHABETIC-UPPER'
      *  091198 BAH ADD BW-FNAME TO CX-REASON, TC-REASON AND CK-REASON
      *             EAST TENN, PR# 745
      *  111098 CS  IF GP-I-AM-MERC FORCE A SYSTEM CHECK
      *  MJD 990622 ALLOW LOAN CHECK TO USE SYSTEM DATE IF
      *             BR-ALLOW-SYS-AS-EFFDATE = "Y".
      *  MJD 990719 REGIONAL MANAGEMENT PR#899
      *             PROGRAM OPTION ON NEW LOANS WITH PROCEEDS OVER $XXX THE
      *             DISBURSEMENT MUST BE MADE BY CHECK.
      *  GLB 990720 ADD CASH DRAWER PASSWORD OPTION TO ENTER DRAWER NO.
      *             BR-PASSWORD-DRAWER(Y/N) & CA-PASSWORD (4 CHAR PASSWORD)
      *             REGMGM PR# 901
      *  BAH 000427 REGIONAL MANAGEMENT (REGMGM) PR# 1167
      *             200 BRANCHES CASH PROCEEDS MAX $300 ON "NB" AND "FB" ONLY
      *             100/300 BRNO CASH PROCEEDS MAX $300 ON "PB" ONLY
      *  BAH 000531 ADDED NEW VALUE FOR BR-NO-CASH-PROCEEDS "B" TO ALLOW CASH
      *             PROCEEDS ON PB ACCTS ONLY, SUNBELT PR# 1213
      *  BAH 000731 ADDED CODE TO CREATE A TY-REC IF IT DOESNT EXIST, WITH
      *             A RESCISSION LOAN, THIS CAN NOW BE THE FIRST ACCESS OF
      *             THE TY RECORD !! (FOUND ON MY OWN)
      * GLB 010702 DLR:SUBSTITUTE COPY MEMBER LPCXCL TO CLEAR CX-RECORD
      * GLB 010702 DLR:SUBSTITUTE COPY MEMBER LPCKCL TO CLEAR CK-RECORD
      * GLB 010702 UPDATE NEW FIELDS --> CX-SSNO, CK-SSNO
      * BAH 040622 CHANGED EDIT FOR REGMGM CASH ADVANCE LIMIT FROM
      *            IF (BRANCH > 99 AND < 200) OR (BRANCH > 299 AND < 400), TO
      *            IF (BRANCH > 99 AND < 200) OR (BRANCH > 299 AND < 700)
      *            REGMGM PR# 2310
      * JTG 050907 CHANGED TO TEST FOR MEXICO WHEN RECOMPUTING AMOUNT FINANCED
      *            USE LN-ANTICERN(4) INSTEAD OF STORED TRUNCTATED
      *            LN-MAINTFEE * LN-ORGTERM WHICH DOSEN'T EQUAL THE COMPUTED
      *            'VAT' VALUE ADDED TAX IN LN-ANTICERN(4)          WORLD #0408
      * BAH 051116 ALLOW LOANS/ACTION TO UP ARROW FROM F3 CHECK TO BORROWER
      *            AND ADD CO-BORROWER'S NAME AND STILL PRINT AS AMOUNT GIVEN
      *            TO ME DIRECTLY, LOANS PR# 2650
      * BAH 060818 ADDED F2 FROM PAYEE NAME TO POP OPEN A WINDOW (WI/PAYEE)
      *            TO ENTER A 55 LONG PAYEE NAME WHICH WILL BE SPLIT INTO
      *            CX-PAYEE(30) AND CX-ADR1(25), LENDMARK NEWS PR# 2809
      * BAH 060830 ALLOW EXCEL TO UP ARROW FROM F3 CHECK TO BORROWER AND
      *            ADD CO-BORROWER'S NAME AND STILL PRINT AS AMOUNT GIVEN
      *            TO ME DIRECTLY, EXCEL PL# 551
      * BAH 070104 IF GP-ALLOW-MANUAL-CHECKS = "N", FORCE SYSTEM CHECKS
      *             WORLD PR# 513
      * BAH 070124 IF GP-I-AM-WORLD, DO NOT ALLOW F2-EXPANDED PAYEE, PR# 522
      * BAH 071010 ADDED LC-ALLOW-CASH-PROCEEDS, REGMGM PR# 3074
      * BAH 071127 ADDED SP-MAX-CASH-PROCEEDS, REGMGM PR# 3110, WILL BE TESTED
      *            ONLY IF YOU PASS THE GP-AMT-CASH-PROCEEDS TEST FIRST
      * BAH 140214 ELIMINATED HARD CODING FOR CHECKS ONLY ON "NB" AND "FB" AND
      *            CHANGED GP-AMT-CASH-PROCEEDS TO TEST AS A MAX CASH FOR
      *            WORLD AND FORCE THE EXCESS AMOUNT AS A CHECK, WORLD PR# 969
      * BAH 140328 PUT THE CHECK BACK IN FOR "NB" AND "FB" FORCED TO CHECK
      *            PROCEEDS, WORLD PR# 969A
      * BAH 140814 ADDED TEST FOR LPSMNU IN HELP
      * BAH 150924 DO NOT ALLOW CASH PROCEEDS ON "PBD" ACCOUNTS, WORLD PR# 1102
      *
      * KEC 2016.0208 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         FIXED KEYS & RECS FOR FD-REC.
      *         CHANGED FMNO/FORMNO/NEXTFM/SETFM FROM 3 TO 4 BYTES.
      * BLM 160225 FIX FOR NEW TY-DATE FORMAT, YYYYMMDD INSTEAD OF YYYMMDD
      * BAH 160304 CHANGED TILLNO FROM PIC X TO PIC 99, CHANGED SCREEN ENTRY
      *
      * KEC 2016.1026 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         REMOVED USING SAVE-TR1-KEY.
      *         ADDED LIBLP/LPWSTR (NEW) FOR LPWSTR-TR1-KEY (HOLD/SAVE).
      * BAH 170120 ADDED 2 SPANISH PROMPTS BASED ON BR-STATE "MX", AND PASS
      *            "S" IN FREE-RTNCD SO FTENT KNOWS TO DISPLAY SPANISH
      *                                                     WORLD PR#1218
      * BAH 170613 ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 180301 F4-DEBIT CARD PROCEEDS, NETSPEND WORLD #1306
      * BAH 180313 DO NOT ALLOW MIXED PROCEEDS OF CHECKS AND DEBIT CARD. #1306
      *  CS 180320 PER KEVIN'S EMAIL, FORCE THEM TO ENTER 10 DIGITS WOLRD #1306
      *            CHANGED THE INPUT TO ALPHANUMERIC, MOVED VDUBUF TO 
      *            WORKING STORAGE TEST-DEBIT-ENTRY-X, AND CHECK IF NUMERIC 
      *  CS 180321 KEVIN M TALKED TO VENDOR, A ZERO WILL NEVER BE THE FIRST
      *            DIGIT SO KEVEN WANTS ME TO CHANGE BACK TO NUMERIC
      *            INPUT, CHANGED TO TEST VDUNUM0 > 999999999 #1306
      * BAH 180326 CHANGE UPDATE/AUDIT MESSAGE IF USING DEBIT CARDS, #1306A
      * BAH 180514 CLEAR SAVE-DEBIT-CARD-NUMBER AFTER ERROR MESSAGES REGARDING
      *            CASH PROCEEDS NOT ALLOWED, AT ENTER-CHECKS. WORLD PR#1319
      * BAH 180906 IF THEY PRESSED F7 AND HAVE PROCEEDS LEFT, THEN
      *            SAVE-DEBIT-CARD-NUMBER NEEDS TO GET CLEARED, #1333
      * BAH 180927 AUDITING AFTER PRESSING F7-DONE WAS WIPING OUT
      *            SAVE-DEBIT-CARD-NUMBER BUT ALLOWING THEM TO PRESS RETURN
      *            THRU THE DEBIT:(NAME) AND THINKINGIT WAS STILL A DEBIT
      *            CARD, SO LT WAS WRITTEN WITH AN AMOUNT BUT NO NUMBER
      *            AND GL WAS OFF. SEND THIS AUDIT TYPE BACK TO A DIFFERENT
      *            SPOT. (AUDIT-CHECKS-AFTER-DONE), WORLD PR#1340
      * BAH 181115 GLOBAL FMFILE
      * BAH 190313 MOVED EDIT-FM-INITKEY TO LPWSFM-EDIT-FM-INITKEY 
      * BAH 20190614 ADDED CLEAR-FM-FILE (LPFMCL) AND SET FM-USERID
      * BAH 20190712 ADDED CLEAR-LTD-FILE (LPLTDCL) FUTURE FIELDS NOT SET
      * BAH 20200602 202020 GL ENTRIES IF 0 PROCEEDS AND USE F4-DEBIT CARD
      *              DONT ALLOW F4-DEBIT CARD WITH 0 PROCEEDS, #1439
      * BAH 20211123 SPLIT DEBIT CARD PROCEEDS, ALLOW THEM TO HAVE A DEBIT
      *              CARD ALONG WITH OTHER PROCEEDS. WORKS LIKE CASH, WHEN
      *              YOU PRESS F7 WITH PROCEEDS LEFT, WILL ASK FOR CASH OR
      *              DEBIT. #1527
      * BAH 20211202 CHANGE "ENTER NETSPEND TRANS #" TO "ENTER CONFIRMATION #"
      *                                                          #1527A
      * BAH 20220103 UP ARROW AND FUNCTION KEYS WERE CAUSING OUT OF BALANCE
      *              ENTRY.                                      #1527B
      * BAH 20221128 REMOVED UPDATE REPAY FROM LONPC2 AND PUT IN HERE AS IT
      *              NOW NEEDS FUND TO DEBIT AMOUNT. ADDED 2 NEW FIELDS TO
      *              END OF REPAY RECORD WS-REPAY-F2D-AMOUNT AND
      *              WS-REPAY-IS-FUNDABLE    #1646
      * BAH 20240429 ADD WS-REPAY-NO-NXDUEAMT AND WS-REPAY-CLASS TO END OF
      *              REPAY RECORD #1655
      ***********************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBLP/LPFSWK.CPY".

      * REPAY NEW LOAN FILE
           SELECT WK-REPAY-LN-FILE ASSIGN OUTPUT WK-REPAY-LN-PATH
                  ORGANIZATION LINE SEQUENTIAL
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  FILE STATUS FILE-STAT.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.

       COPY "LIBLP/LPFDWK.CPY".

      **************************************************
      *  REPAY LOAN ASCII FILE    RSZ =  (INCLUDES 0D0A)
      **************************************************
       FD  WK-REPAY-LN-FILE
           RECORD IS VARYING IN SIZE FROM 5 TO 2000 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  WK-REPAY-LN-REC.
           03  WK-REPAY-LN-BYTE          PIC X OCCURS 2000 TIMES.

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LP/LONPD0.CBL S34 11/29/21-08:58:49 >".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01LTD.CPY".
       COPY "LIBLP/LP01LTD_SQL.CPY".
       COPY "LIBLP/LPWSLTD.CPY".
       COPY "LIBLP/LP01LTI.CPY".
       COPY "LIBLP/LP01LTI_SQL.CPY".
       COPY "LIBLP/LPWSLTI.CPY".
       COPY "LIBLP/LP01NO.CPY".
       COPY "LIBLP/LP01NO_SQL.CPY".
       COPY "LIBLP/LPWSNO.CPY".
       COPY "LIBLP/LP01LC.CPY".
       COPY "LIBLP/LP01LC_SQL.CPY".
       COPY "LIBLP/LPWSLC.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01CA.CPY".
       COPY "LIBLP/LP01CA_SQL.CPY".
       COPY "LIBLP/LPWSCA.CPY".
       COPY "LIBLP/LP01TY.CPY".
       COPY "LIBLP/LP01TY_SQL.CPY".
       COPY "LIBLP/LPWSTY.CPY".
       COPY "LIBLP/LP01CX.CPY".
       COPY "LIBLP/LP01CX_SQL.CPY".
       COPY "LIBLP/LPWSCX.CPY".
       COPY "LIBLP/LP01FD.CPY".
       COPY "LIBLP/LP01FD_SQL.CPY".
       COPY "LIBLP/LPWSFD.CPY".
       COPY "LIBLP/LP01FM.CPY".
       COPY "LIBLP/LP01FM_SQL.CPY".
       COPY "LIBLP/LPWSFM.CPY".
       COPY "LIBLP/LP01CK.CPY".
       COPY "LIBLP/LP01CK_SQL.CPY".
       COPY "LIBLP/LPWSCK.CPY".
      *-----------------------------------------------------------------
       COPY "LIBLP/LP01TRC.CPY".
       COPY "LIBLP/LP01TRC_SQL.CPY".
       COPY "LIBLP/LPWSTRC.CPY".
       COPY "LIBLP/LP01TRL.CPY".
       COPY "LIBLP/LP01TRL_SQL.CPY".
       COPY "LIBLP/LPWSTRL.CPY".
       COPY "LIBLP/LPWKPATH.CPY".

       01  WK-KEY          PIC 9(2).
       01  WK-REPAY-LN-IFN PIC X(7)     VALUE "REPAYLN".

      *-----------------------------------------------------------------
      * REPAY NEW LOANS
      * /USR/DATA/R1/FTP/REPAY/NEWLOANS/BRNO.NUMBER
      *                                     1234.123456
       01  WK-REPAY-LN-PATH.
           03  FILLER                    PIC X      VALUE "/".
           03  WK-REPAY-LN-PATH-BASE     PIC X(9).
           03  FILLER                    PIC X      VALUE "/".
           03  WK-REPAY-LN-PATH-MACHINE  PIC X(2).
           03  WK-REPAY-LN-PATH-DIR      PIC X(11) VALUE "/FTP/REPAY/".
           03  WK-REPAY-LN-FILENAME      PIC X(9)  VALUE "NEWLOANS/".
           03  WK-REPAY-LN-PATH-BRNO     PIC 9999.
           03  FILLER                    PIC X VALUE ".".
           03  WK-REPAY-LN-PATH-NUMBER   PIC 9(6).

      * REPAY RECORD
       01  WS-REPAY-LN-REC.
           03  WS-REPAY-LN-BRACCTNO              PIC Z(12).
           03  WS-REPAY-LN-NUMBER                PIC Z(8).
           03  WS-REPAY-LN-OWNBR                 PIC Z(6).
           03  WS-REPAY-LN-SSNO1                 PIC 9(9).
           03  WS-REPAY-BW-FNAME                 PIC X(16).
           03  WS-REPAY-BW-LNAME                 PIC X(16).
           03  WS-REPAY-BW-ADR1                  PIC X(24).
           03  WS-REPAY-BW-ADR2                  PIC X(24).
           03  WS-REPAY-BW-CITY                  PIC X(15).
           03  WS-REPAY-BW-STATE                 PIC X(2).
           03  WS-REPAY-BW-ZIP                   PIC X(09).
           03  WS-REPAY-BW-AGE                   PIC 9(8).
           03  WS-REPAY-BW-PHONE                 PIC 9(10).
           03  WS-REPAY-BW-CELL-PHONE            PIC 9(10).
           03  WS-REPAY-LN-LOANDATE              PIC 9(8).
           03  WS-REPAY-LN-LNAMT                 PIC Z(8).99.
           03  WS-REPAY-LN-BAL-OR-PO             PIC Z(8).99.
           03  WS-REPAY-LN-1STPYAMT              PIC Z(8).99.
           03  WS-REPAY-LN-REGPYAMT              PIC Z(8).99.
           03  WS-REPAY-LN-AMT-PAID-LAST         PIC Z(8).99.     
           03  WS-REPAY-LN-DATE-PAID-LAST        PIC 9(8).
           03  WS-REPAY-LN-ORGTERM               PIC Z(5). 
           03  WS-REPAY-NO-TRUE-DUE-DATE         PIC 9(8).
           03  WS-REPAY-NO-TDUEAMT               PIC Z(8).99.
           03  WS-REPAY-LN-PAST-DUE-AMT          PIC Z(8).99.
           03  WS-REPAY-CUSTOM                   PIC X.
           03  WS-REPAY-AVAIL-CREDIT-FG          PIC X.
           03  WS-REPAY-TDUE-PAYOFF              PIC Z(8).99.
           03  WS-REPAY-MAX-PAY-AMT              PIC Z(8).99.
           03  WS-REPAY-1STPYDATE                PIC 9(8).
           03  WS-REPAY-NO-NXDUE-DATE            PIC 9(8).
           03  WS-REPAY-INT-PORTION-OF-PAY       PIC Z(8).99.
           03  WS-REPAY-NO-PAST-DUE-DAYS         PIC Z(5).
           03  WS-REPAY-NO-TDUELC                PIC Z(8).99.
           03  WS-REPAY-BW-CO-FNAME              PIC X(37).
           03  WS-REPAY-BW-CO-LNAME              PIC X(37).
           03  WS-REPAY-IS-RESTRICTED            PIC X.
           03  WS-REPAY-BRANCH-STATE             PIC XX.
           03  WS-REPAY-F2D-AMOUNT               PIC Z(8).99.
           03  WS-REPAY-IS-FUNDABLE              PIC X.
           03  WS-REPAY-NO-NXDUEAMT              PIC Z(8).99.
           03  WS-REPAY-CLASS                    PIC 99.


      * REPAY WORKERS
       01  CR-1             PIC X VALUE X"0D".
       01  JUSTIFY-TYPE     PIC X VALUE "L".
       01  NXDUE-REBATE     PIC S9(7)V99 COMP-3.
       01  PRIOR-REBATE     PIC S9(7)V99 COMP-3.
       01  PER-DIEM-RATE    PIC 999V999.
       01  WS-BRACCT.
           03  WS-OWNBR     PIC Z(6).
           03  WS-NUMBER    PIC 9(6).
       01  ZIP-WORK         PIC X(10).
       01  X REDEFINES ZIP-WORK.
           03  FILLER           PIC X(6).
           03  PART-2           PIC X(4).
       01  X1 REDEFINES ZIP-WORK.
           03  FILLER           PIC X(5).
           03  PART-2-A         PIC X(4).
           03  PART-2-JUNK      PIC X.
       01  X3 REDEFINES ZIP-WORK.
           03  PART-3-ZIP       PIC X(5).
           03  PART-3-DASH      PIC X(1).
           03  PART-3-EXT       PIC X(4).

       01  ONE             PIC S9.
       01  ELE             PIC 9(6).
       01  ACTION          PIC X(6).
       01  NEW-FLAG        PIC 9.

       01  DISPLAY-AMOUNT  PIC $$$$$$9.99.

      * MUST MATCH LINKAGE IN CL/REBL01.CBL
       01  REBL-WORKERS.
           03  REBL-LN-ACCTNO           PIC 9(8)       VALUE 0.
           03  REBL-LN-SSNO             PIC 9(9)       VALUE 0.
           03  REBL-TRANS-DATE          PIC 9(8)       VALUE 0.
           03  REBL-REBATE-ADJ          PIC S9(7)V99   VALUE 0.
           03  REBL-MEMO                PIC X(60)      VALUE SPACES.
           03  REBL-BR-NOT-OURS-REFUND  PIC X          VALUE " ".

       01  WS-SYSTEM-DATEX    PIC X(08).
       01  WS-SYSTEM-DATE REDEFINES WS-SYSTEM-DATEX PIC 9(8).
       01  SUB             PIC 9(6) COMP-3 VALUE 0.
       01  OLD-WK-STATUS   PIC X.
       01  OLD-WK-PYAMT    PIC 9(6)V99  COMP-3 VALUE 0.
       01  NON-MEMO-INS    PIC 9(6)V99  COMP-3 VALUE 0.
       01  CHECK-TOTAL     PIC 9(6)V99  COMP-3 VALUE 0.
       01  AMT-WK1         PIC 9(6)V99  COMP-3 VALUE 0.
       01  AMT-WK2         PIC 9(6)V99  COMP-3 VALUE 0.
       01  AMT-WK3         PIC S9(7)V99 COMP-3 VALUE 0.
       01  TILL-CASH       PIC S9(7)V99 COMP-3 VALUE 0.
       01  CASH-TEST-AMT   PIC 9(6)V99  COMP-3 VALUE 0.
       01  TILLNO          PIC 99              VALUE 0.
       01  CASH-PROCEEDS-OK  PIC X             VALUE SPACES.
       01  MANUAL-ONLY     PIC X.
       01  WK-REASON.
           03  FILLER      PIC X(12).
           03  WK-WHO      PIC X(18).
       01  PAID-CODE.
           03  FILLER      PIC X(5).
           03  PAID-CD     PIC X.
       01  FREE-TYPE-BUF.
           03  FREE-RTNCD      PIC X       VALUE " ".
           03  FREE-TYPE1      PIC X(70)   VALUE SPACES.
           03  FREE-TYPE2      PIC X(70)   VALUE SPACES.

       01  SAVE-DEBIT-CARD-NUMBER PIC 9(10) COMP-3 VALUE 0.
       01  SAVE-DEBIT-CARD-AMOUNT PIC S9(7)V99 COMP-3 VALUE 0.
       01  HOLD-WK-CHECK-RECORD  PIC X(WK-CHECK-RECORD-SIZE)
                                           VALUE SPACES.

       01  INSPECT-W       PIC X(86).
       01  ADD-ON          PIC X(25).
       01  SAVE-BW-FNAME   PIC X(16).
       01  SAVE-BW-LNAME   PIC X(16).
       01  SAVE-CX1-KEY.
           03  SAVE-CX-BRNO    PIC 9(4).
           03  SAVE-CX-TYPE    PIC XX.
           03  SAVE-CX-SEQNO   PIC 9(4).

       01  SAVE-WK-KEY        PIC 9(6).

       01  O-MSG.
           03  FILLER      PIC X(38) VALUE
           "AMOUNT EXCEEDS DISBURSEMENT BALANCE BY".
           03  O-AMT       PIC ZZZ,ZZ9.99.

       01  ALP-BW.
           03  ALP-BW123   PIC X(3).
           03  ALP-HYPHEN1 PIC X.
           03  ALP-BW45    PIC X(2).
           03  ALP-HYPHEN2 PIC X.
           03  ALP-BW6789  PIC X(4).
       01  NUM-BW          PIC 9(9).
       01  NUM-BWX REDEFINES NUM-BW.
           03  NUM-BW123   PIC X(3).
           03  NUM-BW45    PIC X(2).
           03  NUM-BW6789  PIC X(4).

      ********************************
      *   S C R E E N   B U F F E R  *
      ********************************
       01  DISPLAY-BUF1        VALUE " ".
           03  D-CLASS         PIC 99.
           03  D-HYPHEN1       PIC X.
           03  D-ACCTNO        PIC 9(6).
           03  D-HYPHEN2       PIC X.
           03  D-LAWCODE       PIC 99.
           03  D-010-040       OCCURS 4.
               05  D-010       PIC X(11).
               05  D-011       PIC X(33).
               05  D-012       PIC X(1).
           03  D-RECAP         OCCURS 7.
               05  D-AMT       PIC ZZZ,ZZ9.99.
           03  D-DUE           PIC ZZZ,ZZ9.99-.
           03  D-RESCISSION    PIC X(15).
       01  DISPLAY-LIST1.
           03  FILLER          PIC X(35) VALUE
           "LNNO 010X 011X 012X 020X 021X 022X ".
           03  FILLER          PIC X(45) VALUE
           "030X 031X 032X 040X 041X 042X L010 L020 L030 ".
           03  FILLER          PIC X(38) VALUE
           "L040 070 L080 L090 L100 R1.".

      ************************************
      *     WORK LINE DISPLAY BUFFER     *
      ************************************
       01  CHECK-LINE             PIC 99 VALUE 0.
       01  B-LN                   PIC 99 VALUE 0.
       01  B-TOP-LN               PIC 99 VALUE 0.
       01  B-SCREEN-LN            PIC 99 VALUE 0.
       01  LSUB                   PIC 9(6) COMP-3.
       01  LSUB2                  PIC 9(6) COMP-3.
       01  S-LNS                  PIC 9(6) COMP-3 VALUE 3.
       01  S-LNS-PLUS1            PIC 9(6) COMP-3 VALUE 4.
       01  S-LNS-LESS1            PIC 9(6) COMP-3 VALUE 2.
       01  S-LNSX2-LESS1          PIC 9(6) COMP-3 VALUE 5.
       01  S-LNS-FREE-WK-RECS     PIC 99 VALUE 10.
       01  LN-WK                  PIC 9(6) COMP-3.
       01  LINE-LIST              PIC X(12) VALUE
           "L01 L02 L03.".
       01  LINE-BUF.
           03  D-LN           OCCURS 3 TIMES.
               05  D-LN-NO             PIC 9(2).
               05  D-LN-BODY           PIC X(54).
               05  D-LN-DEL            PIC X.

      **************************************
      *        CHECK RECORD LINE           *
      **************************************
       01  LN-BUF                 VALUE SPACE.
           03  L-010              PIC 99.
           03  FILLER             PIC XX.
           03  L-020              PIC X(30).
           03  FILLER             PIC X.
           03  L-030              PIC ZZZ,ZZ9.99 BLANK ZERO.
           03  FILLER             PIC X(4).
           03  L-040              PIC Z(6) BLANK ZERO.
           03  FILLER             PIC X.
           03  L-DEL              PIC X.
       01  REV-MESS         PIC X(19) VALUE
           "YN 040 030 020 010.".

      *************************************
      *    ENTRY LINE AUTO SETUP BUFFER   *
      *************************************
       01  EL-BUF                 VALUE SPACE.
           03  EL-YN              PIC X.
           03  EL-040             PIC Z(6).
           03  EL-030             PIC ZZZ,ZZ9.99.
           03  EL-020             PIC X(30).
           03  EL-010             PIC 99.

      ********************************
      *   E L E M E N T   S P E C S  *
      ********************************
       01  MAX             PIC 99   VALUE 4.
       01  SPEC-TABLE.
           03  FILLER      PIC X(8) VALUE "EN N020N".
           03  FILLER      PIC X(8) VALUE "E  A300 ".
           03  FILLER      PIC X(8) VALUE "E NN062 ".
           03  FILLER      PIC X(8) VALUE "EN N060 ".
       01  SPEC-TABLE2     REDEFINES SPEC-TABLE.
           03  SPEC-REC    OCCURS 4 TIMES.
               05  SPEC    PIC X(7).
               05  CHFG    PIC X.

       01  LEGEND-01.
           03  FILLER                 PIC X(6) VALUE "^-UP, ".
           03  FILLER                 PIC X    VALUE X'76'.
           03  FILLER                 PIC X(31) VALUE
           "-DN, F6-ADD, AUDIT/DELETE LINE#".

       COPY "LIBGB/CAPASSWDW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBLP/LPMENUW.CPY".
       COPY "LIBLP/LPSETTYW.CPY".
       COPY "LIBGB/REBATEW.CPY".
       COPY "LIBLP/LPIBPCW.CPY".
       COPY "LIBLP/LPRATEW.CPY".
       COPY "LIBLP/LPUPWKW.CPY".
       COPY "LIBGB/RBACTW.CPY".
       COPY "LIBLP/LPAPRSW.CPY".
       COPY "LIBLP/LPAPRZW.CPY".
       COPY "LIBLP/LPITRMW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBLP/LPXDTEW.CPY".
       COPY "LIBLP/SETCTBLW.CPY".
       COPY "LIBLP/LPUPERW.CPY".
       COPY "LIBLP/LPAMTSW.CPY".
       COPY "LIBLP/LPSRCDW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBLP/POSTDATEW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/GPENVW.CPY".

       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/LONPD_DEF.CPY".
       COPY "LIBLP/LPSMNU_DEF.CPY".
       COPY "LIBLP/LPLNMU_DEF.CPY".
       COPY "LIBLP/LPCAMU_DEF.CPY".
       COPY "LIBLP/LPVDMU_DEF.CPY".
       COPY "LIBWI/PAYEE_DEF.CPY".
       COPY "LIBLP/LONPD_WKS.CPY".
       COPY "LIBLP/LPSMNU_WKS.CPY".
       COPY "LIBLP/LPLNMU_WKS.CPY".
       COPY "LIBLP/LPCAMU_WKS.CPY".
       COPY "LIBLP/LPVDMU_WKS.CPY".
       COPY "LIBWI/PAYEE_WKS.CPY".

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBLP/LPLOANW.CPY".
       COPY "LIBLP/LPPFBUF.CPY".

       SCREEN SECTION.
       COPY "LIBLP/LONPD_SCN.CPY".
       COPY "LIBLP/LPSMNU_SCN.CPY".
       COPY "LIBLP/LPLNMU_SCN.CPY".
       COPY "LIBLP/LPCAMU_SCN.CPY".
       COPY "LIBLP/LPVDMU_SCN.CPY".
       COPY "LIBWI/PAYEE_SCN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME PROG-BUF UPDATE-BUF.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               WK-FILE WK-REPAY-LN-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-TY1-FILE.
           PERFORM CLOSE-TRL1-FILE.
           PERFORM CLOSE-TRC1-FILE.
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-NO1-FILE.
           PERFORM CLOSE-LTD1-FILE.
           PERFORM CLOSE-LTI1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-LC1-FILE.
           PERFORM CLOSE-FM1-FILE.
           PERFORM CLOSE-FD1-FILE.
           PERFORM CLOSE-CX1-FILE.
           PERFORM CLOSE-CK1-FILE.
           PERFORM CLOSE-CA1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-BR-FILE.
           CLOSE WK-FILE WK-REPAY-LN-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      ***********************************
      *   C O N T R O L   M O D U L E   *
      ***********************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "LONPD" TO VDU-FORM-NAME.

           PERFORM FORM-RESET.

           MOVE USER-PID TO TEMP-PID.

           MOVE " " TO ERRCD.

           PERFORM GET-GPENV.

           PERFORM READ-LOAN-RECORD.

           MOVE BRANCH TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

           MOVE LN-ORGST TO SP-ORGST.
           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           MOVE LN-LAWCODE TO SP-LAWCODE.
           PERFORM OPEN-SP1-FILE.
           PERFORM READ-SP1-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.
           PERFORM CLOSE-SP1-FILE.

           MOVE LN-CLASS TO LC-CODE.
           PERFORM OPEN-LC1-FILE.
           PERFORM READ-LC1-FILE.
           IF IO-FG = 9
              MOVE "Y" TO LC-ALLOW-CASH-PROCEEDS.
           PERFORM CLOSE-LC1-FILE.

       RELOAD-REBL.
      * TEST FOR ADJUSTMENT OF INTEREST REBATE FOR MISSISSIPPI:
      *                                         OR SOURTH CAROLINA:
      *                                          OR ALABAMA
           IF NOT (LN-SRCD = "PB" OR "RN" OR "SC"
                                OR "RB" OR "RO")
            IF SP-RBEARLY(7) = 3 OR 19 OR 16
              PERFORM CLOSE-LN1-FILE
      * CANT HAVE LN RECORD LOCKED
              MOVE LN-ACCTNO TO REBL-LN-ACCTNO
              MOVE LN-SSNO(1) TO REBL-LN-SSNO
              MOVE TRANS-DATE TO REBL-TRANS-DATE
              MOVE 0 TO REBL-REBATE-ADJ
              MOVE BR-NOT-OURS-REFUND TO REBL-BR-NOT-OURS-REFUND
              MOVE "CL/REBL01" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH SP-REC REBL-WORKERS
                   ON OVERFLOW
                      MOVE "<REBL01.C OVERFLOW> PRESS ENTER:" TO MESS
                      PERFORM SEND-MESS
                      GO TO RELOAD-REBL.

           IF NOT (LN-SRCD = "PB" OR "RN" OR "SC"
                                OR "RB" OR "RO")
            IF SP-RBEARLY(7) = 3 OR 19 OR 16
              CANCEL FORM-PROGX
      * MUST GET LN RECORD AGAIN, BECAUSE WE CLOSED IT AND REBL01 CHANGES IT
              PERFORM READ-LOAN-RECORD.

           PERFORM OPEN-FD1-FILE.
           MOVE SP-PRFORM(2) TO FD-FMNO.
           PERFORM READ-FD1-FILE.
           IF IO-FG = 9
             MOVE "Y" TO MANUAL-ONLY.
           PERFORM CLOSE-FD1-FILE.

      * DONT HAVE A LPWSTR-TR1-KEY IF RESCISSION, NOR DO YOU NEED TO UPDATE
      * THE TR1 CASH PROCEEDS !!
           IF NOT LN-POCD-INRESCISSION
              MOVE 4 TO WK-KEY
              PERFORM OPEN-WK-FILE
              PERFORM READ-WK-FILE
              IF IO-FG = 9
                 GO TO IO-ERROR
              ELSE
                 MOVE WK-SAVE-TR-KEY TO LPWSTRL-TRL1-KEY
                 PERFORM DELETE-WK-FILE
                 PERFORM CLOSE-WK-FILE.

           PERFORM DISPLAY-FIELDS.

           MOVE LN-SSNO(1) TO BW-SSNO.
           PERFORM OPEN-BW1-FILE.
           PERFORM READ-BW1-FILE.
           IF IO-FG = 9
              MOVE " " TO BW-FNAME BW-LNAME.
           PERFORM CLOSE-BW1-FILE.
           MOVE BW-FNAME TO SAVE-BW-FNAME.
           MOVE BW-LNAME TO SAVE-BW-LNAME.
       ENTER-PAID-CD.
           IF MANUAL-ONLY = "Y"
             MOVE "INVALID CHECK FORM #.  MANUAL CHECKS ONLY." TO MESS
             PERFORM SEND-MESS
             MOVE "M" TO PAID-CD
             GO TO ENTER-CHECKS.

           IF GP-ALLOW-MANUAL-CHECKS = "N"
             MOVE "S" TO PAID-CD
             GO TO ENTER-FREE-TYPING.

           MOVE "CHECKS: S-SYSTEM, M-MANUAL: " TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "E  A060SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-PAID-CD.
           MOVE VDUBUF TO PAID-CODE.
           IF PAID-CD NOT = "S" AND NOT = "M"
              GO TO ENTER-PAID-CD.

      * FORCING SYSTEM CHECKS FOR NOW 11/25/96 BAH
           IF LN-POCD-INRESCISSION
              IF PAID-CD NOT = "S"
                 MOVE "RESCISSION ACCOUNT - MUST DO SYSTEM CHECK(S)"
                      TO MESS
                 PERFORM SEND-MESS
                 MOVE "S" TO PAID-CD.

       ENTER-FREE-TYPING.
           IF PAID-CD = "M"
              GO TO ENTER-CHECKS.

           MOVE "FREE TYPING ?  Y/N:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-FREE-TYPING.
           IF VDUBUF = "     N"
              GO TO ENTER-CHECKS.
           IF VDUBUF NOT = "     Y"
              GO TO ENTER-FREE-TYPING.

      * FREE TYPING WINDOW:

           MOVE "WI/FTENT" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH FREE-TYPE-BUF.
           CANCEL FORM-PROGX.
           IF FREE-RTNCD = "X"
              MOVE SPACES TO FREE-TYPE1
                             FREE-TYPE2
              GO TO ENTER-PAID-CD.

       ENTER-CHECKS.
           MOVE 0 TO SAVE-DEBIT-CARD-NUMBER
                     SAVE-DEBIT-CARD-AMOUNT.
           MOVE SPACES TO WK-BWPROC.

           MOVE "L110." TO MESS.
           PERFORM RESET-MESS.
           MOVE "N" TO CHFG(1).

           PERFORM ENTRY-MODULE.

      * MUST STILL CHECK THIS HERE. IF YOU PRESS F7 FROM THE CHECK AMT,
      * IT CLEARS THAT AMOUNT AND EXITS TO HERE AND VDUNUM2 IS 0 AND IT
      * SHOULDNT BE. EXAMPLE, ENTER F3 TO BORROWER, HALF THE PROCEEDS, THEN
      * ENTER A VENDOR, SITTING ON AMOUNT LEFT TO DISBURSE, YOU PRESS F7
      * IT ZEROS OUT THAT AMOUNT WHICH LEAVES CASH TO DISBURSE, BUT WAS COMING
      * HERE AND THINKING IT WAS 0, NO CASH TO DISBURSE
           SUBTRACT CHECK-TOTAL FROM AMT-WK1 GIVING VDUNUM2.

           MOVE 0 TO TILL-CASH LN-PROCASH.
           MOVE 0 TO LN-TILLNO.

           MOVE 0 TO SAVE-DEBIT-CARD-NUMBER
                     SAVE-DEBIT-CARD-AMOUNT.

      * NO CASH PROCEEDS, GO TO UPDATE
           IF VDUNUM2 = 0
              GO TO UPDATE-YN.

      * TEST FOR CASH OR DEBIT CARD
           IF NOT BR-USE-DEBIT-CARDS = "Y"
              GO TO CONTINUE-WITH-CASH.

       ENTER-CASH-OR-DEBIT.
           MOVE "C-CASH PROCEEDS OR D-DEBIT PROCEEDS" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E RA000SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-CHECKS.

           MOVE VDUBUF TO ACTION.
           IF ACTION = "     D"
              SUBTRACT CHECK-TOTAL FROM AMT-WK1
                       GIVING SAVE-DEBIT-CARD-AMOUNT
              PERFORM ENTER-DEBIT-CARD-INFO
              GO TO UPDATE-YN.
           IF ACTION NOT = "     C"
              GO TO ENTER-CASH-OR-DEBIT.


       CONTINUE-WITH-CASH.
           MOVE "S  N062L110." TO VDU.
           SUBTRACT CHECK-TOTAL FROM AMT-WK1 GIVING VDUNUM2.
           PERFORM SCREENIO.

           MOVE VDUNUM2 TO TILL-CASH LN-PROCASH.

      * TEST FOR MISSISSIPPI REBATE ADJ, AND IF SO
      * INSURE THAT ADJUSTMENT IS PAID IN FULL BY CASH
      * OR IN FULL BY CHECK:
           IF SP-RBEARLY(7) = 3 OR 19 OR 16
              IF LN-STATE-WORKER > 0
                 IF LN-PROCASH < LN-STATE-WORKER
                    MOVE
                     "IF PAYING INTEREST REBATE ADJUSTMENT BY CASH"
                                                               TO MESS
                    PERFORM SEND-MESS
                    MOVE "THE FULL AMOUNT MUST BE PAID!" TO MESS
                    PERFORM SEND-MESS
                    GO TO ENTER-CHECKS.


      * RESCISSION ACCOUNTS CAN ONLY HAVE SYSTEM CHECKS !!
           IF (BR-NO-CASH-PROCEEDS = "Y") OR (LN-POCD-INRESCISSION)
              MOVE "CAN NOT HAVE CASH PROCEEDS" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-CHECKS.

      ****************************************************************
      * WORLD TEST FOR CASH VERSUS CHECK PROCEEDS
      *       THEY ALLOW CASH PROCEEDS ON PB LOANS UP TO THE LIMIT SET
      *       IN THE GLOBAL FILE, HOWEVER, IF PBD, FORCE CHECK, NO CASH
      *       'NB' AND 'FB' MUST BE CHECK PROCEEDS
      ****************************************************************

      * SUNBELT #1213
           IF (BR-NO-CASH-PROCEEDS = "B")
      * FORCE CHECK PROCEEDS ON EVERYTHING EXCEPT A "PB"
              IF (LN-SRCD NOT = "PB")
                 MOVE "CASH PROCEEDS ONLY ALLOWED ON 'PB' ACCTS" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-CHECKS
              ELSE
      * "PB"
      *  FORCE PBD TO CHECK PROCEEDS ONLY
                 PERFORM GET-SRCD-SUB
                 IF SRCD-IS-PBD
                    MOVE "NO CASH PROCEEDS ALLOWED ON 'PBD' ACCTS"
                                                             TO MESS
                    PERFORM SEND-MESS
                    GO TO ENTER-CHECKS.

           IF LC-ALLOW-CASH-PROCEEDS = "N"
              MOVE "THIS CLASS CAN'T HAVE CASH PROCEEDS" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-CHECKS.

      * TEST CEILING ON CASH ALLOWED TO DISBURSE
           IF GP-AMT-CASH-PROCEEDS NUMERIC
              MOVE "Y" TO CASH-PROCEEDS-OK
              PERFORM TEST-CASH-PROCEEDS
              IF CASH-PROCEEDS-OK NOT = "Y"
                 GO TO ENTER-CHECKS.

           PERFORM ENTER-TILLNO.
           IF VDUSTATUS = "10" OR "1U"
              MOVE 0 TO TILL-CASH LN-PROCASH
              MOVE 0 TO TILLNO
              GO TO ENTER-CHECKS.

       UPDATE-YN.
           MOVE "U-UPDATE CHECKS, A-AUDIT/ADD CHECKS" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E RA000SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS NOT = "00"
              GO TO UPDATE-YN.
           MOVE VDUBUF TO ACTION.
           IF ACTION = "     A"
              MOVE "SEL." TO MESS
              PERFORM RESET-MESS
              MOVE 0 TO TILL-CASH LN-PROCASH
              MOVE 0 TO TILLNO
              MOVE 0 TO SAVE-DEBIT-CARD-NUMBER SAVE-DEBIT-CARD-AMOUNT
              MOVE "S  A000CONF." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  N062DEBIT." TO VDU
              MOVE 0 TO VDUNUM2
              PERFORM SCREENIO
              GO TO ENTER-CHECKS.
           IF ACTION NOT = "     U"
              GO TO UPDATE-YN.

           PERFORM CHECK-UPDATE.

      * DEBIT CARD PROCEEDS
           IF SAVE-DEBIT-CARD-AMOUNT NOT = 0
              PERFORM UPDATE-LT-DEBIT-CARD.

           MOVE 0 TO SUB.
           MOVE "Y" TO LN-PROC-DISBURSED.
           PERFORM REWRITE-LN1-FILE.

           PERFORM UPDATE-REPAY-FILE.

      * RESCISSION LOANS HAVE NO TRANSACTION RECORDS YET !!
           IF LN-POCD-INRESCISSION
               GO TO CONTRACT-PRINT.

           MOVE "UPDATING TRANSACTION CASH PROCEEDS........" TO MESS.
           PERFORM SEND-LEGEND.

           MOVE LPWSTRL-TRL1-KEY TO TRL1-KEY.
           PERFORM OPEN-TRL1-FILE.
           PERFORM READ-TRL1-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

           IF SP-RBEARLY(7) = 3 OR 19 OR 16
              MOVE "A"             TO TL-LPEOD5-FG
              MOVE LN-OLDACCTNO    TO TL-RESTRICTD-ACCTNO
              MOVE LN-STATE-WORKER TO TL-STATE-WORKER.

           MOVE LN-PROCASH             TO TL-PROCASH.
           MOVE LN-TILLNO              TO TL-TILLNO.
           IF SAVE-DEBIT-CARD-NUMBER NOT = 0
              MOVE LTD-DEBIT-CARD-AMT  TO TL-DEBIT-CARD-AMT.

           PERFORM REWRITE-TRL1-FILE.
           PERFORM CLOSE-TRL1-FILE.

           IF BR-IMMEDIATE-DOC-PRINT NOT = "N"
              GO TO CONTRACT-PRINT-BYPASS.
       CONTRACT-PRINT.
           MOVE "IMMEDIATE CONTRACT PRINTING? (Y/N)     :" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E RA060SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS NOT = "00"
              GO TO CONTRACT-PRINT.
           IF VDUBUF = "     N"
              GO TO UPDATE-EXIT.
           IF VDUBUF NOT = "     Y"
              GO TO CONTRACT-PRINT.
       CONTRACT-PRINT-BYPASS.
           MOVE MENU-PROG-FORM(3) TO OLD-PROG-FORM.
           MOVE MENU-PROG-FORM(7) TO NEW-PROG-FORM.
           MOVE SPACES TO ERRCD.
           GO TO END-ROUTINE.
       UPDATE-EXIT.
           MOVE MENU-PROG-FORM(3) TO NEW-PROG-FORM.
           GO TO END-ROUTINE.

      **************************************
      *   E L E M E N T   E N T R Y        *
      *   NEW-FLAG: 0 - MOD EXISTING REC   *
      *             1 - INITIAL ENTRY      *
      *             2 - MOD NEW REC        *
      **************************************
       ENTRY-MODULE SECTION.
       ENTER-NEXT-LINE.
           ADD 1 TO B-LN.
           MOVE B-LN TO LN-WK.
           MOVE "S  A020010." TO VDU.
           MOVE B-LN TO VDUBUF.
           PERFORM SCREENIO.
           PERFORM CLEAR-RECORD.
           IF PAID-CD = "M"
              MOVE "S  A010CKNO." TO VDU
              MOVE "#" TO VDUBUF
              PERFORM SCREENIO.
       BEGIN-ENTRY.
           IF CHFG(1) = " "
              PERFORM RESET-ENTRY-LINE
              MOVE LEGEND-01 TO MESS
      * ^-UP, V-DN, F6-ADD, AUDIT/DELETE LINE#
           ELSE
              MOVE "F2-EXPAND,F3-TO BORROWER,F6-AUD,F7-DONE"
                                                       TO MESS.
           PERFORM SEND-LEGEND.

           MOVE " " TO WK-BWPROC.
           MOVE 1 TO ONE NEW-FLAG.
           MOVE 0 TO ELE.
       CREATE-ROUTINE.
           IF NEW-FLAG NOT = 1
              GO TO ACCEPT-LINE.
           ADD ONE TO ELE.
           IF ELE < 1
              MOVE 1 TO ELE ONE.
           IF ELE NOT > MAX
              GO TO ENTER-ELE.
           MOVE 2 TO NEW-FLAG.
       ACCEPT-LINE.
           IF CHFG(1) = "N"
              MOVE "S  A010YN." TO VDU
              MOVE "A" TO VDUBUF
              PERFORM SCREENIO
      * ^-BACK, A-ACCEPT
              MOVE "^-BACK, A-ACCEPT" TO MESS
           ELSE
              MOVE "F1-NEW LINE, ^-BACK, D-DELETE, A-ACCEPT" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E  A010YN." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              GO TO BEGIN-ENTRY.
           IF VDUSTATUS = "1U"
              MOVE 1 TO NEW-FLAG
              ADD 1 MAX GIVING ELE
              IF PAID-CD = "S" 
                 MOVE -2 TO ONE
                 GO TO CREATE-ROUTINE
              ELSE
                 MOVE -1 TO ONE
                 GO TO CREATE-ROUTINE.
           IF VDUSTATUS NOT = "00"
              GO TO ACCEPT-LINE.
           IF CHFG(1) = "N"
              IF VDUBUF = "A"
                 PERFORM WRITE-LINE-SCREEN
                 ADD S-LNS-FREE-WK-RECS B-LN GIVING WK-KEY
                 PERFORM UPDATE-CHECK-TOTAL
                 PERFORM WRITE-LINE-DISK
                 PERFORM RESET-ENTRY-LINE
                 GO TO ENTER-NEXT-LINE
              ELSE
                 GO TO ACCEPT-LINE.
           IF VDUBUF = "A"
              PERFORM UPDATE-AUDIT
              GO TO BEGIN-ENTRY.
           IF VDUBUF = "D"
              PERFORM DELETE-AUDIT
              GO TO BEGIN-ENTRY.
           GO TO ACCEPT-LINE.
       ENTER-ELE.
           IF ELE = 2
              IF CHFG(1) = "N"
                 SUBTRACT CHECK-TOTAL FROM AMT-WK1 GIVING WK-PYAMT
                 MOVE "S  N062030." TO VDU
                 MOVE WK-PYAMT TO VDUNUM2
                 PERFORM SCREENIO
              ELSE
                 PERFORM RESET-SEL.
           IF CHFG(ELE) = "N"
              IF NEW-FLAG = 1
                 MOVE 1 TO ONE
                 GO TO CREATE-ROUTINE.
           MOVE SPEC(ELE) TO VDU.
           MOVE ELE TO VDUROW.
           MOVE 0 TO VDUCOL.
           MOVE "." TO VDUDECIMAL.
           PERFORM SCREENIO.
       TEST-F1.
           IF VDUSTATUS = "10"
              GO TO BEGIN-ENTRY.
       TEST-F2.
           IF VDUSTATUS NOT = "12"
              GO TO TEST-F3.
      *    IF NOT GP-I-AM-WORLD
      *       IF CHFG(1) NOT = " " AND ELE = 2
      *          PERFORM ENTER-LONG-PAYEE-NAME
      *          MOVE "S  A000020." TO VDU
      *          MOVE WK-PAYEE TO VDUBUF
      *          PERFORM SCREENIO
      *          GO TO CREATE-ROUTINE.

       TEST-F3.
           IF VDUSTATUS NOT = "16"
              GO TO TEST-F4.
           IF CHFG(1) NOT = " " AND ELE = 2
              MOVE SAVE-BW-FNAME TO INSPECT-W
              MOVE SAVE-BW-LNAME TO ADD-ON
              PERFORM ADD-IT
              MOVE "S  A300020." TO VDU
              MOVE INSPECT-W TO VDUBUF WK-PAYEE
              PERFORM SCREENIO
              MOVE SPACES TO WK-ADR1
              MOVE "Y"    TO WK-BWPROC
              GO TO CREATE-ROUTINE.
       TEST-F4.
           IF VDUSTATUS NOT = "18"
              GO TO UP-ARROW.
       UP-ARROW.
           IF VDUSTATUS NOT = "1U"
              GO TO TEST-F5.
           IF ELE = 1
              PERFORM SCROLL-DOWN
              GO TO ENTER-ELE.
           IF ELE = 2
              GO TO BEGIN-ENTRY.
           IF NEW-FLAG = 1
              IF PAID-CD = "M"
                 MOVE -1 TO ONE
                 GO TO CREATE-ROUTINE
              ELSE
                 MOVE -2 TO ONE
                 GO TO CREATE-ROUTINE.
       TEST-F5.
           IF VDUSTATUS NOT = "1D"
              GO TO TEST-F6.
      *    ------------------------------------------------------------
      *    DO NOT ALLOW DOWN ARROW FROM AMOUNT FIELD
      *    ------------------------------------------------------------
           IF ELE = 3
              GO TO TEST-F6.
           IF ELE = 1
              PERFORM SCROLL-UP
              GO TO ENTER-ELE.
           GO TO GOTO-FIELD.
       TEST-F6.
           IF VDUSTATUS NOT = "1<"
              GO TO TEST-F7.
      * F6-ADD
           IF CHFG(1) = " "
              IF ELE = 1
                 PERFORM ADJUST-LINES-TO-END
                 MOVE "N" TO CHFG(1)
                 GO TO ENTER-NEXT-LINE.
      * F6-AUDIT
           IF CHFG(1) = "N"
              MOVE " " TO CHFG(1)
              SUBTRACT 1 FROM B-LN
              GO TO BEGIN-ENTRY.
       TEST-F7.
           IF VDUSTATUS = "1>"
              IF CHFG(1) NOT = " "
                 SUBTRACT 1 FROM B-LN
                 PERFORM RESET-ENTRY-LINE
                 GO TO END-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-ELE.
       GOTO-FIELD.
           MOVE 1 TO ONE.
           GO TO F-01 F-02 F-03 F-04
              DEPENDING ON ELE.
           GO TO END-ENTRY-MODULE.

      ************************************
      *     S E T   E L E M E N T S      *
      ************************************
       F-01.
           MOVE VDUNUM0 TO CHECK-LINE.
           IF CHECK-LINE = 0 OR CHECK-LINE > B-LN
              GO TO ENTER-ELE.
           IF B-LN > S-LNS-LESS1
              ADD B-TOP-LN S-LNS-LESS1 GIVING LN-WK
              IF CHECK-LINE < B-TOP-LN OR CHECK-LINE > LN-WK
                 MOVE
               "PLEASE REVIEW LINE PRIOR TO ATTEMPTING THIS FUNCTION !"
                  TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           MOVE CHECK-LINE TO LN-WK.
           PERFORM SETUP-OLD-LINE.
           GO TO CREATE-ROUTINE.
       F-02.
           IF VDUBUF = " "
              MOVE WK-PAYEE TO VDUBUF.
           IF VDUBUF = " "
              GO TO ENTER-ELE.
      * SOME UP ARROW TO ADD SPOUSES NAME, STILL WANT AS CHECK TO BORROWER!
           IF WK-BWPROC = "Y"
              IF VDUBUF NOT = WK-PAYEE
                 MOVE " " TO WK-BWPROC.

           MOVE VDUBUF TO WK-PAYEE.

      * MUST CLEAR OUT ADR1 FROM F2-LONG NAME, IN CASE THEY ENTERED
      * THAT THEN CHANGED THEIR MIND
           MOVE SPACES TO WK-ADR1.

           GO TO CREATE-ROUTINE.
       F-03.
           IF VDUNUM2 = 0
              MOVE WK-PYAMT TO VDUNUM2.
           IF VDUNUM2 = 0
              GO TO ENTER-ELE.
           MOVE VDUNUM2 TO WK-PYAMT.

           MOVE "S  N062030." TO VDU.
           MOVE WK-PYAMT TO VDUNUM2.
           PERFORM SCREENIO.

           COMPUTE AMT-WK3 = AMT-WK1 - CHECK-TOTAL - WK-PYAMT.
           IF OLD-WK-STATUS = " "
              ADD OLD-WK-PYAMT TO AMT-WK3.
           IF AMT-WK3 < 0
              MOVE AMT-WK3 TO O-AMT
              MOVE O-MSG TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

      * DONT STOP ON CHECK NUMBER FOR SYSTEM CHECKS
           IF PAID-CD = "S" 
             ADD 1 TO ELE.

           GO TO CREATE-ROUTINE.
       F-04.
           IF VDUNUM0 = 0
              GO TO ENTER-ELE.

      * DONT ENTER CHECK NUMBER FOR SYSTEM CHECKS
           IF PAID-CD = "S"
              MOVE 0 TO WK-CKNUM
              GO TO CREATE-ROUTINE.

           MOVE BRANCH TO CK-BRNO.
           MOVE VDUNUM0 TO CK-NO.
           PERFORM OPEN-CK1-FILE.
           PERFORM READ-CK1-FILE.
           PERFORM CLOSE-CK1-FILE.
           IF IO-FG = 0
              MOVE "CHECK ALREADY EXISTS!" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           IF BR-AUTO-CHECK = "Y"
              IF CK-NO NOT < BR-NEXT-CHECK
                 MOVE "INVALID MANUAL CHECK NUMBER" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.

           MOVE WK-CHECK-RECORD TO HOLD-WK-CHECK-RECORD.
           PERFORM VERIFY-CHECK-NUM.
           MOVE HOLD-WK-CHECK-RECORD TO WK-CHECK-RECORD.
           IF IO-FG = 9
              MOVE "DUPLICATE CHECK NUMBER " TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           MOVE CK-NO TO WK-CKNUM.

           GO TO CREATE-ROUTINE.
       END-ENTRY-MODULE.
           EXIT.

      ***************************************
       ENTER-DEBIT-CARD-INFO SECTION.
      ***************************************

           MOVE "S  N062DEBIT." TO VDU.
           MOVE SAVE-DEBIT-CARD-AMOUNT TO VDUNUM2.
           PERFORM SCREENIO.
       ENTER-DEBIT-NUMBER.
           MOVE "ENTER CONFIRMATION # " TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "ENNN100CONF." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS NOT = "00" GO TO ENTER-DEBIT-NUMBER.
           MOVE VDUNUM0 TO SAVE-DEBIT-CARD-NUMBER.
           IF VDUNUM0 = 0
              GO TO ENTER-DEBIT-NUMBER.


           IF VDUNUM0 NOT > 999999999
              MOVE "ENTER 10 DIGITS, LEAD 0 NOT ALLOWED" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-DEBIT-NUMBER.

       EXIT-ENTER-DEBIT-CARD.
           EXIT.

      ***************************************
      *ENTER-LONG-PAYEE-NAME SECTION.
      ***************************************
      *    MOVE 7 TO WINDOW-LINES.
      *    MOVE 70 TO WINDOW-COLUMNS.
      *    MOVE 5 TO WINDOW-BEGIN-Y.
      *    MOVE 2 TO WINDOW-BEGIN-X.
      *    MOVE "PAYEE" TO VDU-FORM-NAME.
      *    MOVE "WI/PAYEE" TO WINDOW-FORM-NAM.

      *    PERFORM OPEN-WINDOW.
      *    DISPLAY PAYEE-SCREEN.

      *    MOVE PAYEE-FORM-FIELDS TO WR-BUF.
      *    MOVE PAYEE-FORM-LIST   TO WR-LIST.
      *    PERFORM CALL-WRFORM.

      *    IF WK-PAYEE NOT = SPACES
      *       MOVE "S  A030NAME." TO VDU
      *       MOVE WK-PAYEE TO VDUBUF
      *       PERFORM SCREENIO
      *       MOVE "S  A025ADR1." TO VDU
      *       MOVE WK-ADR1 TO VDUBUF
      *       PERFORM SCREENIO.

      *ENTER-PAYEE-NAME.
      *    MOVE "E  A030NAME." TO VDU.
      *    PERFORM SCREENIO.
      *    IF VDUSTATUS = "1>" GO TO EXIT-ENTER-LONG-PAYEE.
      *    IF VDUSTATUS NOT = "00" GO TO ENTER-PAYEE-NAME.
      *    MOVE VDUBUF TO WK-PAYEE.

      *ENTER-PAYEE-ADR1.
      *    MOVE "E  A025ADR1." TO VDU.
      *    PERFORM SCREENIO.
      *    IF VDUSTATUS = "1>" GO TO EXIT-ENTER-LONG-PAYEE.
      *    IF VDUSTATUS = "1U" GO TO ENTER-PAYEE-NAME.
      *    IF VDUSTATUS NOT = "00" GO TO ENTER-PAYEE-ADR1.
      *    MOVE VDUBUF TO WK-ADR1.


      *EXIT-ENTER-LONG-PAYEE.
      *    DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
      *    DESTROY PAYEE-SCREEN.
      *    PERFORM CLOSE-WINDOW.
      *    MOVE "LONPD" TO VDU-FORM-NAME.


      ***************************************
       VERIFY-CHECK-NUM SECTION.
      ***************************************
           PERFORM OPEN-WK-FILE.
           MOVE 0 TO IO-FG.
           ADD S-LNS-FREE-WK-RECS 1 GIVING SAVE-WK-KEY.
       NEXT-VERIFY-CHECK.
           MOVE SAVE-WK-KEY TO WK-KEY.
           PERFORM READ-WK-FILE.
           IF IO-FG = 9
              MOVE 0 TO IO-FG
              GO TO EXIT-VERIFY-CHECK.
           IF WK-STATUS = "D"
              ADD 1 TO SAVE-WK-KEY
              GO TO NEXT-VERIFY-CHECK.
           IF (CHECK-LINE + S-LNS-FREE-WK-RECS) = WK-KEY OR
              CK-NO NOT = WK-CKNUM
                 ADD 1 TO SAVE-WK-KEY
                 GO TO NEXT-VERIFY-CHECK.
           MOVE 9 TO IO-FG.
       EXIT-VERIFY-CHECK.
           PERFORM CLOSE-WK-FILE.

      *****************************************
       WRITE-LINE-SCREEN SECTION.
      *****************************************
           PERFORM SETUP-LINE.
           ADD 1 TO B-SCREEN-LN.
           IF B-SCREEN-LN > S-LNS
              MOVE 1 TO LSUB
              PERFORM ADJUST-LINES S-LNS-LESS1 TIMES
              MOVE LN-BUF TO D-LN(S-LNS)
              PERFORM DISPLAY-ALL-LINES
            ELSE
              MOVE "S  A000L0 ." TO VDU
              MOVE B-SCREEN-LN TO VDUCOL
              MOVE LN-BUF TO VDUBUF D-LN(B-SCREEN-LN)
              PERFORM SCREENIO.
           GO TO END-WRITE-LINE-SCREEN.
       ADJUST-LINES.
           COMPUTE LSUB2 = LSUB + 1
           MOVE D-LN(LSUB2) TO D-LN(LSUB).
           ADD 1 TO LSUB.
       END-WRITE-LINE-SCREEN.
           MOVE D-LN-NO(1) TO B-TOP-LN.

      *************************************
       WRITE-LINE-DISK SECTION.
      *************************************
           MOVE 0 TO IO-FG.
           PERFORM OPEN-WK-FILE.
           PERFORM WRITE-WK-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.
       END-WRITE-LINE-DISK.
           PERFORM CLOSE-WK-FILE.

      *********************************
       UPDATE-LT-DEBIT-CARD SECTION.
      *********************************
           PERFORM OPEN-LTD1-FILE.
           
           PERFORM CLEAR-LTD-FILE.

           MOVE LN-OWNBR  TO LTD-BRNO.
           MOVE LN-ACCTNO TO LTD-ACCTNO.
           MOVE 1         TO LTD-SEQNO.

           MOVE SAVE-DEBIT-CARD-NUMBER TO LTD-DEBIT-CARD-NUMBER.
           MOVE SAVE-DEBIT-CARD-AMOUNT TO LTD-DEBIT-CARD-AMT.
           MOVE LN-LOANDATE            TO LTD-LOANDATE.

           PERFORM WRITE-LTD1-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.
           PERFORM CLOSE-LTD1-FILE.


      *********************************
       UPDATE-AUDIT SECTION.
      *********************************
           ADD CHECK-LINE S-LNS-FREE-WK-RECS GIVING WK-KEY.
           MOVE " " TO WK-STATUS.
           PERFORM SETUP-LINE.
           COMPUTE CHECK-LINE = CHECK-LINE - B-TOP-LN + 1.
           MOVE "S  A000L0 ." TO VDU.
           MOVE CHECK-LINE TO VDUCOL.
           MOVE LN-BUF TO VDUBUF D-LN(CHECK-LINE).
           PERFORM SCREENIO.
           MOVE 0 TO IO-FG.
           PERFORM OPEN-WK-FILE.
           MOVE " " TO WK-STATUS.
           PERFORM UPDATE-CHECK-TOTAL.
           PERFORM REWRITE-WK-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.
           PERFORM CLOSE-WK-FILE.

      *********************************
       DELETE-AUDIT SECTION.
      *********************************
           ADD S-LNS-FREE-WK-RECS CHECK-LINE GIVING WK-KEY.
           MOVE "D" TO WK-STATUS.
           PERFORM SETUP-LINE.
           COMPUTE CHECK-LINE = CHECK-LINE - B-TOP-LN + 1.
           MOVE "D" TO D-LN-DEL(CHECK-LINE).
           MOVE "S  A000L0 ." TO VDU.
           MOVE CHECK-LINE TO VDUCOL.
           MOVE LN-BUF TO VDUBUF D-LN(CHECK-LINE).
           PERFORM SCREENIO.

           MOVE 0 TO IO-FG.
           PERFORM OPEN-WK-FILE.
           PERFORM READ-WK-FILE.

           MOVE "D" TO WK-STATUS.

           PERFORM UPDATE-CHECK-TOTAL.
           PERFORM REWRITE-WK-FILE.
           PERFORM CLOSE-WK-FILE.

      *************************************
       UPDATE-CHECK-TOTAL SECTION.
      *************************************
           IF OLD-WK-STATUS = " "
              SUBTRACT OLD-WK-PYAMT FROM CHECK-TOTAL.
           IF WK-STATUS = " "
              ADD WK-PYAMT TO CHECK-TOTAL.

      ***********************************
       SETUP-OLD-LINE SECTION.
      ***********************************
           ADD S-LNS-FREE-WK-RECS CHECK-LINE GIVING WK-KEY.
           PERFORM OPEN-WK-FILE.
           PERFORM READ-WK-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.
           PERFORM CLOSE-WK-FILE.
           MOVE SPACE TO EL-BUF.
           MOVE CHECK-LINE TO EL-010.
           MOVE WK-PAYEE TO EL-020.
           MOVE WK-PYAMT TO EL-030.
           MOVE WK-CKNUM TO EL-040.
           MOVE WK-PYAMT TO OLD-WK-PYAMT.
           MOVE WK-STATUS TO OLD-WK-STATUS.
           MOVE "A" TO EL-YN.
           PERFORM SEND-ENTRY-LINE.

      *********************************
       SCROLL-DOWN SECTION.
      *********************************
           IF B-TOP-LN NOT > 1
              GO TO END-SCROLL-DOWN.
           IF B-TOP-LN > S-LNS
              SUBTRACT S-LNS-PLUS1 FROM B-TOP-LN GIVING LN-WK
            ELSE
              MOVE 0 TO LN-WK.
           MOVE 1 TO B-SCREEN-LN.
           MOVE SPACES TO LINE-BUF.
           MOVE 0 TO IO-FG.
           PERFORM OPEN-WK-FILE.
           PERFORM SETUP-LINE-BUF UNTIL B-SCREEN-LN > S-LNS.
           PERFORM CLOSE-WK-FILE.
           PERFORM DISPLAY-ALL-LINES.
           MOVE D-LN-NO(1) TO B-TOP-LN.
       END-SCROLL-DOWN.
           EXIT.

      *********************************
       SCROLL-UP SECTION.
      *********************************
           IF B-LN < S-LNS-PLUS1
              GO TO END-SCROLL-UP.
           ADD B-TOP-LN S-LNSX2-LESS1 GIVING LN-WK.
           IF LN-WK > B-LN
              MOVE B-LN TO LN-WK.
           SUBTRACT S-LNS FROM LN-WK.
           MOVE 1 TO B-SCREEN-LN.
           MOVE SPACES TO LINE-BUF.
           MOVE 0 TO IO-FG.
           PERFORM OPEN-WK-FILE.
           PERFORM SETUP-LINE-BUF UNTIL B-SCREEN-LN > S-LNS.
           PERFORM CLOSE-WK-FILE.
           PERFORM DISPLAY-ALL-LINES.
           MOVE D-LN-NO(1) TO B-TOP-LN.
       END-SCROLL-UP.
           EXIT.

      *************************************
       SETUP-LINE-BUF SECTION.
      *************************************
           ADD 1 S-LNS-FREE-WK-RECS LN-WK GIVING WK-KEY.
           PERFORM READ-WK-FILE.
           IF IO-FG NOT = 0
              MOVE S-LNS-PLUS1 TO B-SCREEN-LN
              GO TO END-SETUP-LINE-BUF.
           ADD 1 TO LN-WK.
           PERFORM SETUP-LINE.
           MOVE LN-BUF TO D-LN(B-SCREEN-LN).
           ADD 1 TO B-SCREEN-LN.
       END-SETUP-LINE-BUF.
           EXIT.

      *********************************
       SETUP-LINE SECTION.
      *********************************
           MOVE SPACES TO LN-BUF.
           MOVE LN-WK TO L-010.
           IF WK-STATUS = "D"
              MOVE "D" TO L-DEL.
           MOVE WK-PAYEE TO L-020.
           MOVE WK-PYAMT TO L-030.
           MOVE WK-CKNUM TO L-040.

      *******************************
       CLEAR-RECORD SECTION.
      *******************************
           MOVE " " TO WK-SAVE-RECORD.
           MOVE "D" TO OLD-WK-STATUS.
           MOVE 0 TO OLD-WK-PYAMT.
           MOVE 0 TO WK-PYAMT WK-CKNUM.

      ***********************************
       DISPLAY-FIELDS SECTION.
      ***********************************

           MOVE " "        TO DISPLAY-BUF1.
           MOVE LN-CLASS   TO D-CLASS.
           MOVE "-"        TO D-HYPHEN1 D-HYPHEN2.
           MOVE LN-ACCTNO  TO D-ACCTNO.
           MOVE LN-LAWCODE TO D-LAWCODE.

           PERFORM OPEN-BW1-FILE.
           MOVE 1 TO ELE.
       DISP-NEXT-BW.
           MOVE LN-SSNO(ELE) TO BW-SSNO.
           PERFORM NUMBW-TO-ALPBW.
           MOVE " " TO BW-LNAME BW-FNAME.
           IF BW-SSNO NOT = 0
              PERFORM GET-BW1
              MOVE ALP-BW TO D-010(ELE)
              MOVE INSPECT-W TO D-011(ELE)
              MOVE LN-MAKERCD(ELE) TO D-012(ELE).
           IF ELE < 4
              ADD 1 TO ELE
              GO TO DISP-NEXT-BW.
           PERFORM CLOSE-BW1-FILE.

      * TOTAL PAYMENTS
           COMPUTE D-AMT(1) = LN-1STPYAMT +
                              LN-NOREGPY * LN-REGPYAMT + LN-LASTPYAMT.
      * FINANCE CHARGE
           MOVE LN-FINCHG TO D-AMT(2).
      * AMOUNT FINANCED
           COMPUTE D-AMT(3)
                   AMT-WK1 = LN-1STPYAMT +
                             LN-NOREGPY * LN-REGPYAMT + LN-LASTPYAMT
                           - LN-EXTCHG - LN-SERCHG - LN-INTCHG
                           - LN-MAINTFEE * LN-ORGTERM.
      * INSURANCE PREMIUMS
           MOVE 0 TO NON-MEMO-INS.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
              IF LN-INSOURS(SUB) NOT = "M"
                 ADD LN-INSPREM(SUB) TO NON-MEMO-INS
              END-IF
           END-PERFORM.
           MOVE NON-MEMO-INS TO D-AMT(4).

      * COMBINED FEES
           MOVE 0 TO AMT-WK2.
           PERFORM SUM-OF-FEEAMTS VARYING SUB FROM 1 BY 1
                                         UNTIL SUB > 10.
           MOVE AMT-WK2 TO D-AMT(5).

           ADD NON-MEMO-INS TO AMT-WK2.
      * TOTAL INSURANCE AND FEES
           MOVE AMT-WK2 TO D-AMT(6).

           SUBTRACT AMT-WK2 FROM AMT-WK1.
           COMPUTE AMT-WK3 = LN-PRBAL + LN-PRINTDUE
              - LN-PRFINCHGRB - LN-PRINSRB.
      * DUE ON PRIOR ACCOUNT
           MOVE AMT-WK3 TO D-DUE.

           SUBTRACT AMT-WK3 FROM AMT-WK1.
      * MISSISSIPPI INTEREST REBATE ADJ:
      * S.CAROLINA
      * ALABAMA
           IF SP-RBEARLY(7) = 3 OR 19 OR 16
              ADD LN-STATE-WORKER TO AMT-WK1.
      * LOAN PROCEEDS TO DISBURSE
           MOVE AMT-WK1 TO D-AMT(7).

           IF LN-POCD-INRESCISSION
              MOVE " * RESCISSION *" TO D-RESCISSION.

           MOVE DISPLAY-BUF1  TO WR-BUF.
           MOVE DISPLAY-LIST1 TO WR-LIST.
           PERFORM CALL-WRFORM.

      * MISSISSIPPI INTEREST REBATE ADJ:
           IF SP-RBEARLY(7) = 3 OR 19 OR 16
              IF LN-STATE-WORKER NOT = 0
                 MOVE REBL-MEMO TO MESS
                 PERFORM SEND-MESS
                 MOVE "ADJUSTMENT ADDED TO TOTAL DISBURSEMENT" TO MESS
                 PERFORM SEND-MESS.


      *********************************
       CHECK-UPDATE SECTION.
      *********************************

           MOVE "UPDATING CHECK RECORDS..............." TO MESS.
           PERFORM SEND-LEGEND.

           PERFORM OPEN-CA1-FILE.
           PERFORM OPEN-WK-FILE.
           PERFORM OPEN-FM1-FILE.
      * ONLY NEED TY IF MANUAL CHECKS, TO UPDATE TY-CKWRITTEN
           IF PAID-CD = "M"
              PERFORM GET-TY1-FILE.

           ADD S-LNS-FREE-WK-RECS 1 GIVING SAVE-WK-KEY.

       NEXT-CHECK-CREATE.
           MOVE SAVE-WK-KEY TO WK-KEY.
           PERFORM READ-WK-FILE.
           IF IO-FG = 9
              PERFORM CLOSE-WK-FILE
              GO TO FINISH-FINAL-UPDATES.

           IF WK-STATUS = "D"
              PERFORM DELETE-WK-FILE
              ADD 1 TO SAVE-WK-KEY
              GO TO NEXT-CHECK-CREATE.

      * IF MANUAL CHECKS, DONT CREATE CX OR FM
           IF PAID-CD = "M"
              GO TO UPDATE-MANUAL-CHECKS.

      * SYSTEM CHECKS

           PERFORM OPEN-CX1-FILE.
           MOVE BRANCH TO SAVE-CX-BRNO.
           MOVE "LN"   TO SAVE-CX-TYPE.
           MOVE 1      TO SAVE-CX-SEQNO.

           PERFORM CLEAR-CX-FILE.

           MOVE LN-CLASS TO CX-CLASS.
           MOVE WK-PYAMT TO CX-AMT.
           MOVE WK-PAYEE TO CX-PAYEE.
           MOVE WK-ADR1 TO CX-ADR1.
           MOVE WK-BWPROC TO CX-BWPROC.
           MOVE SPACES TO CX-ADR2 CX-CITY CX-STATE CX-ZIP.
           MOVE LN-ACCTNO TO CX-REF.
           MOVE "LOAN CHECK: " TO WK-REASON.
           STRING BW-LNAME", ",BW-FNAME DELIMITED BY "  " INTO WK-WHO.
           MOVE WK-REASON TO CX-REASON.
           MOVE LN-ACCTNO TO CX-ACCTNO.
           MOVE PAID-CD TO CX-PAID.
           MOVE "D0" TO CX-PGM.
           MOVE FREE-TYPE1 TO CX-FREE1.
           MOVE FREE-TYPE2 TO CX-FREE2.

           MOVE TRANS-DATE TO CX-DATE.
           MOVE BW-SSNO TO CX-SSNO.

           IF BR-ALLOW-SYS-AS-EFFDATE = "Y"
              PERFORM GET-SYSTEM-DATE
              IF WS-SYSTEM-DATE < TRANS-DATE
                 IF WS-SYSTEM-DATE = LN-LOANDATE
                    MOVE WS-SYSTEM-DATE TO CX-DATE.

           IF LN-POCD-INRESCISSION
              MOVE "Y" TO CX-RESCISSION.
       WRITE-CX-TEMP.
           MOVE SAVE-CX1-KEY TO CX1-KEY.
           PERFORM WRITE-CX1-FILE.
           IF IO-FG = 0
              PERFORM CLOSE-CX1-FILE
              PERFORM DELETE-WK-FILE
              ADD 1 TO SAVE-WK-KEY
              GO TO CREATE-FORM-REQUEST.

           IF SAVE-CX-SEQNO < 9999
              ADD 1 TO SAVE-CX-SEQNO
              GO TO WRITE-CX-TEMP.

           MOVE "REQUEST # = 9999.  REQUEST NOT CREATED!" TO MESS.
           PERFORM SEND-MESS.
           GO TO NEXT-CHECK-CREATE.

       CREATE-FORM-REQUEST.
           MOVE FM-PATH-OWNBR TO FM-BRNO.

           MOVE SPACES                 TO LPWSFM-EDIT-FM-INITKEY.
           MOVE CX-BRNO                TO LPWSFM-EDIT-FM-BRNO.
           MOVE CX-TYPE                TO LPWSFM-EDIT-FM-TYPE.
           MOVE CX-SEQNO               TO LPWSFM-EDIT-FM-SEQNO.
           MOVE FD-FMNO                TO LPWSFM-EDIT-FM-CKFMNO FM-FMNO.
           MOVE LPWSFM-EDIT-FM-INITKEY TO FM-INITKEY.
           PERFORM READ-FM1-FILE.

           PERFORM CLEAR-FM-FILE.

           MOVE FM-PATH-OWNBR          TO FM-BRNO.
           MOVE LPWSFM-EDIT-FM-INITKEY TO FM-INITKEY.
           MOVE FD-FMNO                TO FM-FMNO.

           MOVE CX-ZIP             TO FM-ZIP.
           MOVE " "                TO FM-PRINT.
           MOVE 0                  TO FM-STMT-COUNTER.
           MOVE TRANS-DATE         TO FM-TRDATE.
           MOVE EXT-CONAME-USER-ID TO FM-USERID.

           IF IO-FG = 0
              PERFORM REWRITE-FM1-FILE
              GO TO NEXT-CHECK-CREATE.

           IF FM-FMNO NOT = 0
              PERFORM WRITE-FM1-FILE
              IF IO-FG = 9
                 GO TO IO-ERROR.

      * THESE ARE SYSTEM CHECKS, SO DOES NOT CREATE CK OR TRC
           GO TO NEXT-CHECK-CREATE.

       UPDATE-MANUAL-CHECKS.

           PERFORM UPDATE-CHECK-FILE.
           PERFORM UPDATE-TRANSACTION-FILE.
           ADD WK-PYAMT TO TY-CKWRITTEN(1).

           PERFORM DELETE-WK-FILE.
           PERFORM CLOSE-CK1-FILE.
           ADD 1 TO SAVE-WK-KEY.
           GO TO NEXT-CHECK-CREATE.

       FINISH-FINAL-UPDATES.
           IF PAID-CD = "M"
              PERFORM REWRITE-TY1-FILE
              PERFORM CLOSE-TY1-FILE
              IF IO-FG = 9
                 GO TO IO-ERROR.

           IF TILL-CASH = 0
              GO TO EXIT-UPDATE-CHECKS.

           MOVE "UPDATING CASH DRAWER..................." TO MESS.
           PERFORM SEND-LEGEND.

           MOVE BRANCH     TO CA-BRNO.
           MOVE TRANS-DATE TO CA-DATE.
           MOVE TILLNO     TO CA-TILLNO.
           PERFORM READ-CA1-FILE.
           IF IO-FG = 9
              MOVE "DRAWER DELETED, UPDATE SKIPPED !" TO MESS
              PERFORM SEND-MESS
              GO TO EXIT-UPDATE-CHECKS.

           SUBTRACT TILL-CASH FROM CA-RUNNING.

           PERFORM REWRITE-CA1-FILE.
       EXIT-UPDATE-CHECKS.
           PERFORM CLOSE-CA1-FILE.

      *********************************
       ENTER-TILLNO SECTION.
      *********************************
       ENTER-TILL.
           MOVE "F1-CANCEL, ENTER DRAWER # FOR CASH PROC" TO MESS.
           IF BR-PASSWORD-DRAWER = "Y"
              MOVE "F1-CANCEL, ENTER DRAWER PASSWRD FOR CASH"
                                                          TO MESS.
           PERFORM SEND-LEGEND.
           PERFORM ALARM.
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       ACCEPT-ENTER-TILLNO.
           MOVE BRANCH     TO CA-BRNO.
           MOVE TRANS-DATE TO CA-DATE.
           MOVE 0          TO CA-TILLNO.
           MOVE CA1-KEY TO HOLD-CA1-KEY.
           IF BR-PASSWORD-DRAWER = "Y"
              PERFORM ENTER-CASH-DRAWER-PASSWORD
              IF CAPASSWD-OKAY = "N"
                 GO TO ACCEPT-ENTER-TILLNO
              END-IF
              IF CAPASSWD-OKAY = "V"
                 GO TO ACCEPT-CHECK-VDUSTATUS
              END-IF
              IF CAPASSWD-OKAY = "Y"
                 MOVE "SN N020TILLNO." TO VDU
                 MOVE CA-TILLNO TO VDUNUM0
                 PERFORM SCREENIO
              END-IF
           ELSE
              MOVE "EN N020TILLNO." TO VDU
              PERFORM SCREENIO.
       ACCEPT-CHECK-VDUSTATUS.
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF VDUSTATUS = "10" OR "1U"
              GO TO EXIT-ENTER-TILLNO.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-TILL.

           MOVE VDUNUM0 TO CA-TILLNO LN-TILLNO.
           IF CA-TILLNO = 0
              GO TO ENTER-TILL.

           MOVE BRANCH     TO CA-BRNO.
           MOVE TRANS-DATE TO CA-DATE.
           PERFORM OPEN-CA1-FILE.
           PERFORM READ-CA1-FILE.
           PERFORM CLOSE-CA1-FILE.
           IF IO-FG = 9
              MOVE "INVALID DRAWER #" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-TILL.
           IF CA-BALBY NOT = " "
              MOVE "DRAWER PREVIOUSLY CLOSED !" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-TILL.
           MOVE CA-TILLNO TO TILLNO.

       EXIT-ENTER-TILLNO.
           EXIT.

      ******************************************************************
       GET-SYSTEM-DATE SECTION.
      ******************************************************************
           MOVE "SYSDATE_YYMMDD" TO GETENV-BUF.
           PERFORM GETENV-CALL.
           IF STAT = "00"
              MOVE GETENV-BUF TO DATE-YYMMDD-ALP
              PERFORM CONVERT-YYMMDD-TO-YYYYMMDD
              MOVE DATE-YYYYMMDD TO WS-SYSTEM-DATE
           ELSE
              ACCEPT WS-SYSTEM-DATEX FROM CENTURY-DATE.

      *******************************
       TEST-CASH-PROCEEDS SECTION.
      *******************************

           MOVE GP-AMT-CASH-PROCEEDS TO DISPLAY-AMOUNT.

      * WORLD WANTS THE GP-AMT-CASH-PROCEEDS TO JUST BE A MAX FOR THE CASH
      * AND ALLOW THE REST AS A CHECK, LN-PROCASH AT THIS POINT IS WHAT IS
      * LEFTOVER AFTER TALLYING CHECKS, PRESSING F7-DONE FORCES YOU TO DO
      * CASH AND THAT IS WHAT IS IN LN-PROCASH
      *    MOVE LN-PROCEEDS TO CASH-TEST-AMT.

           MOVE LN-PROCASH TO CASH-TEST-AMT.

           IF CASH-TEST-AMT NOT > GP-AMT-CASH-PROCEEDS
      * YOU PASSED THE TEST AGAINST GP, NOW TEST THE SPR MAX
      * NOT USING IT, SKIP IT
              IF SP-MAX-CASH-PROCEEDS = 202020.20 OR 020202.02
                 GO TO EXIT-TEST-CASH-PROCEEDS
              ELSE
                 IF LN-PROCEEDS NOT > SP-MAX-CASH-PROCEEDS
      * PASSED GP AND SPR, EXIT CLEANLY WITH NO MESSAGE
                    GO TO EXIT-TEST-CASH-PROCEEDS
                 ELSE
      * FAILED SPR MAX, SO DISPLAY THAT AMOUNT AS FAILED
      * FALLS OUT OF THIS IF
                    MOVE SP-MAX-CASH-PROCEEDS TO DISPLAY-AMOUNT.


      * YOU ONLY MAKE IT HERE IF THE AMOUNT IS GREATER THAN THE GP MAX
      * OR THE SPR CASH MAX

      * WORLD DOESN'T USE THE SPR MAX
      *   WORLD HAS 1000.00 IN THE GP-AMT-CASH-PROCEEDS
      *                0.00 IN THE SP-MAX-CASH-PROCEEDS
      *    AND BR-NO-CASH-PROCEEDS = "B" SO ONLY "PB" CAN DISBURSE CASH
      *                               "FB" AND "NB" MUST BE CHECKS
      *        THIS FLAG IS CHECKED UP ABOVE WHEN THEY PRESS F7 DONE AND
      *            THERE IS A REMAINDER FOR CASH
      *        ONLY "PB" FOR WORLD MAKES IT INTO THIS SECTION

      * WORLD ALSO USES THIS OPTION, SO "NB" AND "FB" MUST BE A CHECK
      * "NB" AND "FB" DONT EVEN MAKE IT TO THIS SECTION IF FLAG IS "B"
           IF (BR-NO-CASH-PROCEEDS = "B")
              MOVE SPACES TO MESS
              STRING "EXCEEDS MAX CASH DISB OF" DISPLAY-AMOUNT
                   DELIMITED BY SIZE INTO MESS
              PERFORM SEND-MESS
              MOVE "N" TO CASH-PROCEEDS-OK
              GO TO EXIT-TEST-CASH-PROCEEDS
           ELSE
              PERFORM TEST-CASH-MESSAGE
              GO TO EXIT-TEST-CASH-PROCEEDS.

      * NEW LOANS WITH PROCEEDS OVER $XXX, THE DISBURSEMENT MUST BE MADE
      * BY CHECK
           IF NOT (LN-SRCD = "PB" OR "RN" OR "SC" OR "RB" OR "RO")
              PERFORM TEST-CASH-MESSAGE
              GO TO EXIT-TEST-CASH-PROCEEDS.

       TEST-CASH-MESSAGE.

            MOVE SPACES TO MESS.
            STRING "CAN ONLY DISBURSE CASH IF PROCEEDS ARE UNDER"
                   DISPLAY-AMOUNT DELIMITED BY SIZE INTO MESS.
            PERFORM SEND-MESS.
            MOVE "N" TO CASH-PROCEEDS-OK.


       EXIT-TEST-CASH-PROCEEDS.
           EXIT.

      ******************************************
       UPDATE-CHECK-FILE SECTION.
      ******************************************
           PERFORM OPEN-CK1-FILE.

           PERFORM CLEAR-CK-FILE.

           MOVE BRANCH      TO CK-BRNO.
           MOVE WK-CKNUM    TO CK-NO.
           MOVE "LN"        TO CK-TYPE.
           MOVE LN-CLASS    TO CK-CLASS.
           MOVE TRANS-DATE  TO CK-DATE.
           IF BR-ALLOW-SYS-AS-EFFDATE = "Y"
              PERFORM GET-SYSTEM-DATE
              IF WS-SYSTEM-DATE < TRANS-DATE
                 IF WS-SYSTEM-DATE = LN-LOANDATE
                    MOVE WS-SYSTEM-DATE TO CK-DATE.
           MOVE WK-PYAMT       TO CK-AMT.
           MOVE WK-PAYEE       TO CK-PAYEE.
           MOVE LN-ACCTNO      TO CK-REF.
      * "Y" F3-CHECK TO BORROWER 
           MOVE WK-BWPROC      TO CK-BWPROC.
           MOVE "LOAN CHECK: " TO WK-REASON.
           STRING BW-LNAME", ",BW-FNAME DELIMITED BY "  " INTO WK-WHO.
           MOVE WK-REASON      TO CK-REASON.
           MOVE LN-ACCTNO      TO CK-ACCTNO.
           MOVE "D0"           TO CK-PGM.
           MOVE PAID-CD        TO CK-PAID.
           MOVE FREE-TYPE1     TO CK-FREE1.
           MOVE FREE-TYPE2     TO CK-FREE2.
       
       UPDATE-CHECK-TRY-AGAIN.
           PERFORM WRITE-CK1-FILE.
           IF IO-FG = 0
              GO TO EXIT-UPDATE-CHECK-FILE.

           MOVE "CHECK RECORD NOT CREATED!" TO MESS.
           PERFORM SEND-MESS.

       EXIT-UPDATE-CHECK-FILE.
           EXIT.

      ******************************************
       UPDATE-TRANSACTION-FILE SECTION.
      ******************************************

           PERFORM OPEN-TRC1-FILE.

           PERFORM CLEAR-TRC-FILE.

           MOVE BRANCH     TO TC-BRNO.
           MOVE TRANS-DATE TO TC-KEY-DATE.
           MOVE WK-CKNUM   TO TC-CKNO.
           MOVE LN-CLASS   TO TC-CLASS.
           MOVE 0          TO TC-SEQ.

           MOVE "LN"       TO TC-TYPE.
           MOVE TRANS-DATE TO TC-DATE.
           MOVE WK-PYAMT   TO TC-AMT.
           MOVE LN-ACCTNO  TO TC-REF.
           MOVE WK-PAYEE   TO TC-PAYEE.
           MOVE "LOAN CHECK: " TO WK-REASON.
           STRING BW-LNAME", ",BW-FNAME DELIMITED BY "  " INTO WK-WHO.
           MOVE WK-REASON   TO TC-REASON.
           MOVE LN-ACCTNO   TO TC-ACCTNO.
           MOVE PAID-CD     TO TC-PAID.
           MOVE "D0"        TO TC-PGM.
           MOVE LN-SPRCLASS TO TC-SPRCLASS.

           PERFORM SET-POSTDATE.
           MOVE POST-USERID TO TC-USERID.
           MOVE POST-TIME TO TC-POSTTIME.
           MOVE POST-DATE TO TC-POSTDATE.

           PERFORM WRITE-TRC1-FILE.
           IF IO-FG NOT = 0
              MOVE "CHECK REGISTER NOT UPDATED!" TO MESS
              PERFORM SEND-MESS
              PERFORM CLOSE-TRC1-FILE.

      ********************************************************
       PRE-COMPUTE-REBATE SECTION.
      ********************************************************

           MOVE 0 TO REB-REBATE NXDUE-REBATE PRIOR-REBATE.

      * REBATE ALREADY GIVEN OR INTEREST CANCELLED
           IF NOT (LN-INT-REBATE = " " OR "C")
              MOVE 0 TO WS-REPAY-INT-PORTION-OF-PAY
              CALL "C$JUSTIFY" USING WS-REPAY-INT-PORTION-OF-PAY,
                                                  JUSTIFY-TYPE
              GO TO EXIT-PRE-COMPUTE-REBATE.

      * CALCULATE AN INTEREST REBATE AS OF NO-NXDUE

           MOVE NO-NXDUE-DATE TO REB-PAYDATE.
           MOVE LN-ORGTERM    TO REB-ORGTERM.
           MOVE 7             TO REB-SUB.
           MOVE "RI"          TO REB-LPTRCD.
           MOVE LN-INTCHG     TO REB-TOTCHG.
           PERFORM REBATE.
           MOVE REB-REBATE    TO NXDUE-REBATE.

      * FOR ACCOUNTS THAT HAVE NOT MADE THEIR FIRST PAYMENT (NO-NXDUE =
      * THE FIRST PAYMENT DUE DATE, USE THE ORIGINAL INTEREST + EXTENSION
      * CHARGE (RATHER THAN TRYING TO CALC A REBATE) AND TAKE THE DIFFERENCE
      * BETWEEN THAT AND AN RI ON THE FIRST DUE (NO-NXDUE)
           IF NO-NXDUE-DATE = LN-1STPYDATE
              COMPUTE WS-REPAY-INT-PORTION-OF-PAY =
                  (LN-INTCHG + LN-EXTCHG ) - NXDUE-REBATE
              CALL "C$JUSTIFY" USING WS-REPAY-INT-PORTION-OF-PAY,
                                                            JUSTIFY-TYPE
              GO TO EXIT-PRE-COMPUTE-REBATE.

      * NOW CALCULATE AN INTEREST REBATE AS OF ONE PAY PERIOD PRIOR
      * (1 MONTH USING SAME DAY OF MONTH)
           MOVE -1            TO NDTE-HOLD.
           MOVE NO-NXDUE-DATE TO NDTE-DATE.
           PERFORM INCREMENT-MONTHS.

           MOVE NDTE-DATE  TO REB-PAYDATE
           MOVE LN-ORGTERM TO REB-ORGTERM
           MOVE 7          TO REB-SUB
           MOVE "RI"       TO REB-LPTRCD
           MOVE LN-INTCHG  TO REB-TOTCHG
           PERFORM REBATE.
           MOVE REB-REBATE TO PRIOR-REBATE.

           COMPUTE WS-REPAY-INT-PORTION-OF-PAY =
                                       (NXDUE-REBATE - PRIOR-REBATE).
           CALL "C$JUSTIFY" USING WS-REPAY-INT-PORTION-OF-PAY,
                                                         JUSTIFY-TYPE.

       EXIT-PRE-COMPUTE-REBATE.
           EXIT.

      ***************************************************
      * /USR/DATA/R1/FTP/REPAY/NEWLOANS/BRNO.NUMBER
      *                                 1234.123456
      * THERE IS A PROCESS SCANNING THIS DIRECTORY TO SEND
      * THESE IMMEDIATELY TO REPAY SO THEY CAN SET UP
      * AUTOMATIC PAYMENTS WHILE THE CUSTOMER IS STILL IN THE OFFICE
      ***************************************************
       UPDATE-REPAY-FILE SECTION.
      ***************************************************
           MOVE "UPDATING REPAY FILE.........." TO MESS.
           PERFORM SEND-LEGEND.

      * USR/DATA/R1
           MOVE EXT-FILPATH-BASE    TO WK-REPAY-LN-PATH-BASE.
           MOVE EXT-FILPATH-MACHINE TO WK-REPAY-LN-PATH-MACHINE.
           CALL "C$TOLOWER" USING WK-REPAY-LN-PATH-DIR, VALUE 10.
           MOVE BR-NO               TO WK-REPAY-LN-PATH-BRNO.
           MOVE LN-ACCTNO           TO WK-REPAY-LN-PATH-NUMBER.

           PERFORM OPEN-WK-REPAY-LN-FILE-OUTPUT.

           PERFORM OPEN-BW1-FILE.
           MOVE LN-SSNO(1) TO BW-SSNO.
           PERFORM READ-BW1-FILE.
           PERFORM CLOSE-BW1-FILE.

           PERFORM OPEN-NO1-FILE.
           MOVE LN-OWNBR  TO NO-BRNO.
           MOVE LN-ACCTNO TO NO-ACCTNO.
           PERFORM READ-NO1-FILE.
           IF IO-FG NOT = 0
              PERFORM CLEAR-NO-FILE.
           PERFORM CLOSE-NO1-FILE.

           MOVE SPACES TO WK-REPAY-LN-REC WS-REPAY-LN-REC.

           MOVE LN-OWNBR   TO WS-OWNBR.
           MOVE LN-ACCTNO  TO WS-NUMBER.
           CALL "C$JUSTIFY" USING WS-BRACCT, JUSTIFY-TYPE.

           MOVE WS-BRACCT TO WS-REPAY-LN-BRACCTNO.

           MOVE LN-ACCTNO  TO WS-REPAY-LN-NUMBER.
           CALL "C$JUSTIFY" USING WS-REPAY-LN-NUMBER, JUSTIFY-TYPE.

           MOVE LN-OWNBR   TO WS-REPAY-LN-OWNBR.
           CALL "C$JUSTIFY" USING WS-REPAY-LN-OWNBR, JUSTIFY-TYPE.

           IF LN-CURBAL > NO-TDUEPOFF
              MOVE LN-CURBAL TO WS-REPAY-LN-BAL-OR-PO
           ELSE
              MOVE NO-TDUEPOFF TO WS-REPAY-LN-BAL-OR-PO.
           CALL "C$JUSTIFY" USING WS-REPAY-LN-BAL-OR-PO, JUSTIFY-TYPE.

           MOVE LN-1STPYAMT   TO WS-REPAY-LN-1STPYAMT.
           CALL "C$JUSTIFY" USING WS-REPAY-LN-1STPYAMT, JUSTIFY-TYPE.

           MOVE LN-REGPYAMT   TO WS-REPAY-LN-REGPYAMT.
           CALL "C$JUSTIFY" USING WS-REPAY-LN-REGPYAMT, JUSTIFY-TYPE.

           MOVE LN-AMT-PAID-LAST   TO WS-REPAY-LN-AMT-PAID-LAST.
           CALL "C$JUSTIFY" USING WS-REPAY-LN-AMT-PAID-LAST,
                                                        JUSTIFY-TYPE.

           MOVE LN-DATE-PAID-LAST  TO WS-REPAY-LN-DATE-PAID-LAST.

           MOVE LN-ORGTERM         TO WS-REPAY-LN-ORGTERM.
           CALL "C$JUSTIFY" USING WS-REPAY-LN-ORGTERM, JUSTIFY-TYPE.

           MOVE LN-LOANDATE        TO WS-REPAY-LN-LOANDATE.

           MOVE LN-LNAMT        TO WS-REPAY-LN-LNAMT.
           CALL "C$JUSTIFY" USING WS-REPAY-LN-LNAMT, JUSTIFY-TYPE.

           MOVE NO-TRUE-DUE-DATE TO WS-REPAY-NO-TRUE-DUE-DATE.

           MOVE NO-TDUEAMT     TO WS-REPAY-NO-TDUEAMT.
           CALL "C$JUSTIFY" USING WS-REPAY-NO-TDUEAMT, JUSTIFY-TYPE.

           IF NO-TDUEAMT > LN-REGPYAMT
              COMPUTE WS-REPAY-LN-PAST-DUE-AMT =
                      NO-TDUEAMT - LN-REGPYAMT
           ELSE
              MOVE 0 TO WS-REPAY-LN-PAST-DUE-AMT.
           CALL "C$JUSTIFY" USING WS-REPAY-LN-PAST-DUE-AMT, JUSTIFY-TYPE.

           MOVE NO-TDUEPOFF TO WS-REPAY-TDUE-PAYOFF.
           CALL "C$JUSTIFY" USING WS-REPAY-TDUE-PAYOFF, JUSTIFY-TYPE.

      * MAXPAYMENTAMT = (NO-TDUEPOFF) -
      *                 (MINIMUM PAY AMT + OVERPAY-BUFFER)
      * MINIMUM = 25.00
      * OVERPAY BUFFER = 1.00

           IF NO-TDUEPOFF < 26
              MOVE 0 TO WS-REPAY-MAX-PAY-AMT
           ELSE
              COMPUTE WS-REPAY-MAX-PAY-AMT = NO-TDUEPOFF - 26.00.
           CALL "C$JUSTIFY" USING WS-REPAY-MAX-PAY-AMT, JUSTIFY-TYPE.

           MOVE LN-1STPYDATE TO WS-REPAY-1STPYDATE.

           MOVE NO-NXDUE-DATE TO WS-REPAY-NO-NXDUE-DATE.

           MOVE NO-TDUELC     TO WS-REPAY-NO-TDUELC.
           CALL "C$JUSTIFY" USING WS-REPAY-NO-TDUELC, JUSTIFY-TYPE.

           MOVE NO-PAST-DUE-DAYS TO WS-REPAY-NO-PAST-DUE-DAYS.
           CALL "C$JUSTIFY" USING WS-REPAY-NO-PAST-DUE-DAYS,
                                                      JUSTIFY-TYPE.

      * THE CFPB HAS DECIDED TO MOVE FORWARD WITH A REQUIREMENT THAT ANY
      * SCHEDULED ELECTRONIC PAYMENT NEEDS TO HAVE THE FIRST PAYMENT
      * SCHEDULED ITEMIZED WITH PAYMENT AMOUNT, INTEREST AMOUNT, AND
      * PRINCIPAL AMOUNT. ADD FIELDS FOR NEXT SCHEDULED DUE DATE,
      * CALCULATED INTERST PORTION, AND UPDATED 1ST PAYMENT DUE DATE.

      *    ADD UPDATED FIRST PAYMENT DATE, NEXT SCHEDULED DUE DATE, AND
      *    INTEREST PORTION OF PAYMENT. FOR PRECOMPUTED STATUS, CALCULATE
      *    AN INTEREST REBATE AS OF NO-NXDUE, THEN CALCULATE AN INTEREST
      *    REBATE AS OF ONE PAY PERIOD PRIOR (1 MONTH USING SAME DAY OF
      *    MONTH) AND THE DIFFERENCE IS THE AMOUNT TO SEND IN INTEREST
      *    PORTION OF PAYMENT. FOR ACCOUNTS IN SIMPLE INTEREST STATUS
      *    (ACCELERATED OR PAST MATURITY) CALCULATE A PER-DIEM BASED ON THE
      *    CURRENT BALANCE AND RATE USING 365 YRTPE AND MULITPLY BY 30 DAYS

           MOVE TRANS-DATE TO IBPC-DATE.
           PERFORM IBPC-TEST.

           IF NOT ( IBPC-FG = "P" AND IBPC-EXPIRED-FLAG = " " )
              MOVE "PY"             TO RATE-LPTRCD
              MOVE TRANS-DATE       TO RATE-DATE IBPC-DATE
              MOVE LN-MAKERCD(1)    TO RATE-MAKERCD
              PERFORM SET-PY-RATE
              COMPUTE PER-DIEM-RATE ROUNDED = INDU-WORKER / INDU-WK
              COMPUTE WS-REPAY-INT-PORTION-OF-PAY = PER-DIEM-RATE * 30
              CALL "C$JUSTIFY" USING WS-REPAY-INT-PORTION-OF-PAY,
                                                          JUSTIFY-TYPE
           ELSE
              PERFORM PRE-COMPUTE-REBATE.

           MOVE LN-SSNO(1) TO WS-REPAY-LN-SSNO1.

           MOVE BW-FNAME   TO WS-REPAY-BW-FNAME.
           MOVE BW-LNAME   TO WS-REPAY-BW-LNAME.
           MOVE BW-ADR1    TO WS-REPAY-BW-ADR1.
           MOVE BW-ADR2    TO WS-REPAY-BW-ADR2.
           MOVE BW-CITY    TO WS-REPAY-BW-CITY.
           MOVE BW-STATE   TO WS-REPAY-BW-STATE.
           MOVE BW-ZIP     TO ZIP-WORK.
      * IF ZIP IS IN XXXXX-XXXX FORMAT THEN SHIFT EXT TO PUT INTO
      *    XXXXXXXXX FORMAT
           IF PART-3-DASH = "-"
              MOVE PART-2 TO PART-2-A
              MOVE SPACE  TO PART-2-JUNK.
           CALL "C$JUSTIFY" USING ZIP-WORK, JUSTIFY-TYPE.
           MOVE ZIP-WORK TO WS-REPAY-BW-ZIP.

           MOVE BW-BDATE TO WS-REPAY-BW-AGE.

           MOVE BW-PHONE        TO WS-REPAY-BW-PHONE.
           MOVE BW-CELL-PHONE   TO WS-REPAY-BW-CELL-PHONE.
           IF WS-REPAY-BW-CELL-PHONE = 2020202020 OR
               WS-REPAY-BW-CELL-PHONE = 0202020202
                 MOVE ZEROS TO WS-REPAY-BW-CELL-PHONE.
           IF NO-AVAILCREDIT > 0
              MOVE "Y" TO WS-REPAY-AVAIL-CREDIT-FG
           ELSE
              MOVE "N" TO WS-REPAY-AVAIL-CREDIT-FG.

      * WHEN THE CUSTOM-FLAG IS "Y", ALWAYS SET THE ACCEPT PAYMENT INDICATOR
      * (CUSTOM) TO "Y" FOR ALL LOANS, EVEN P&L.
      * OTHERWISE, THE ACCEPT PAYMENT INDICATOR WILL REMAIN "N" FOR LOANS
      * WITH A P&L DATE. THESE ARE ALL BRAND NEW LOANS
           MOVE "Y" TO WS-REPAY-CUSTOM.

           IF LN-SSNO(2) NOT = 0
              MOVE LN-SSNO(2) TO BW-SSNO
              PERFORM OPEN-BW1-FILE
              PERFORM READ-BW1-FILE
              PERFORM CLOSE-BW1-FILE
              IF IO-FG = 0
                 MOVE BW-FNAME TO WS-REPAY-BW-CO-FNAME
                 MOVE BW-LNAME TO WS-REPAY-BW-CO-LNAME.

           MOVE "N" TO WS-REPAY-IS-RESTRICTED.

           MOVE BR-STATE TO WS-REPAY-BRANCH-STATE.

      * FUND TO DEBIT
           PERFORM OPEN-LTD1-FILE.
           MOVE LN-OWNBR  TO LTD-BRNO.
           MOVE LN-ACCTNO TO LTD-ACCTNO.
           MOVE 1         TO LTD-SEQNO.
           PERFORM READ-LTD1-FILE.
           PERFORM CLOSE-LTD1-FILE.
           IF IO-FG NOT = 0
              MOVE 0 TO LTD-DEBIT-CARD-AMT.

      * F2DAMOUNT - AMT THAT IS FUNDABLE VIA FUND TO DEBIT. USE TRAILER AMOUNT
      * IS FUNDABLE;
      *   0 LOAN PROCEEDS ARE FUNDED VIA CHECK - DEBIT TRAILER DOESNT EXIST OR 0
      *   1 LOAN PROCEEDS ARE FUNDED VIA F2D   - DEBIT TRAILER AMT NOT 0/EXIST
      *   1 LOAN PROCEEDS ARE FUNDED VIA F2D AND CHECK - DEBIT TRAILER AMT
      *                                                            NOT 0/EXIST

           MOVE LTD-DEBIT-CARD-AMT TO WS-REPAY-F2D-AMOUNT.
           CALL "C$JUSTIFY" USING WS-REPAY-F2D-AMOUNT, JUSTIFY-TYPE.

           IF LTD-DEBIT-CARD-AMT NOT = 0
              MOVE "1" TO WS-REPAY-IS-FUNDABLE
           ELSE
              MOVE "0" TO WS-REPAY-IS-FUNDABLE.

           MOVE NO-NXDUEAMT TO WS-REPAY-NO-NXDUEAMT.
           CALL "C$JUSTIFY" USING WS-REPAY-NO-NXDUEAMT, JUSTIFY-TYPE.

           MOVE LN-CLASS    TO WS-REPAY-CLASS.

           STRING
              WS-REPAY-LN-BRACCTNO         ,"|"
              WS-REPAY-LN-NUMBER           ,"|"
              WS-REPAY-LN-OWNBR            ,"|"
              WS-REPAY-LN-SSNO1            ,"|"
              WS-REPAY-BW-FNAME            ,"|"
              WS-REPAY-BW-LNAME            ,"|"
              WS-REPAY-BW-ADR1             ,"|"
              WS-REPAY-BW-ADR2             ,"|"
              WS-REPAY-BW-CITY             ,"|"
              WS-REPAY-BW-STATE            ,"|"
              WS-REPAY-BW-ZIP              ,"|"
              WS-REPAY-BW-AGE              ,"|"
              WS-REPAY-BW-PHONE            ,"|"
              WS-REPAY-BW-CELL-PHONE       ,"|"
              WS-REPAY-LN-LOANDATE         ,"|"
              WS-REPAY-LN-LNAMT            ,"|"
              WS-REPAY-LN-BAL-OR-PO        ,"|"
              WS-REPAY-LN-1STPYAMT         ,"|"
              WS-REPAY-LN-REGPYAMT         ,"|"
              WS-REPAY-LN-AMT-PAID-LAST    ,"|"
              WS-REPAY-LN-DATE-PAID-LAST   ,"|"
              WS-REPAY-LN-ORGTERM          ,"|"
              WS-REPAY-NO-TRUE-DUE-DATE    ,"|"
              WS-REPAY-NO-TDUEAMT          ,"|"
              WS-REPAY-LN-PAST-DUE-AMT     ,"|"
              WS-REPAY-CUSTOM              ,"|"
              WS-REPAY-AVAIL-CREDIT-FG     ,"|"
              WS-REPAY-TDUE-PAYOFF         ,"|"
              WS-REPAY-MAX-PAY-AMT         ,"|"
              WS-REPAY-1STPYDATE           ,"|"
              WS-REPAY-NO-NXDUE-DATE       ,"|"
              WS-REPAY-INT-PORTION-OF-PAY  ,"|"
              WS-REPAY-NO-PAST-DUE-DAYS    ,"|"
              WS-REPAY-NO-TDUELC           ,"|"
              WS-REPAY-BW-CO-FNAME         ,"|"
              WS-REPAY-BW-CO-LNAME         ,"|"
              WS-REPAY-IS-RESTRICTED       ,"|"
              WS-REPAY-BRANCH-STATE        ,"|"
              WS-REPAY-F2D-AMOUNT          ,"|"
              WS-REPAY-IS-FUNDABLE         ,"|"
              WS-REPAY-NO-NXDUEAMT         ,"|"
              WS-REPAY-CLASS,CR-1
              DELIMITED BY "  " INTO WK-REPAY-LN-REC.

           PERFORM WRITE-WK-REPAY-LN-FILE.

       UPDATE-REPAY-EXIT.
           EXIT.

      ****************************************************
       FORM-RESET SECTION.
      ****************************************************
           MOVE LPLOAN-SYSDATE TO LONPD-SYSDATE.
      * NEED TO CLEAR SCREEN ON F1
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY LONPD-SCREEN UPON TCLP-MAIN-WINDOW-HANDLE.
           MOVE LONPD-FORM-FIELDS TO WR-BUF.
           MOVE LONPD-FORM-LIST   TO WR-LIST.
           PERFORM CALL-WRFORM.
           
       FORM-RESET-EXIT.
           EXIT.

      ****************************************************
       SETUP-LINK-INFO SECTION.
      ****************************************************
           IF VDU-FORM-NAME = "LPSMNU"
              MOVE LPSMNU-HELP--FILE TO HELP-LINK-FILE
              MOVE LPSMNU-HELP--DIR  TO HELP-LINK-DIR
           ELSE
              MOVE LONPD-HELP--FILE TO HELP-LINK-FILE
              MOVE LONPD-HELP--DIR TO HELP-LINK-DIR.

      *******************************************************
       VDU-CONVERT-FIELD SECTION.
      ******************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/LONPD_EVA.CPY".
              COPY "LIBLP/LPSMNU_EVA.CPY".
              COPY "LIBLP/LPLNMU_EVA.CPY".
              COPY "LIBLP/LPCAMU_EVA.CPY".
              COPY "LIBLP/LPVDMU_EVA.CPY".
              COPY "LIBWI/PAYEE_EVA.CPY".
           END-EVALUATE.

      *********************************
       MISC-PARAGRAPHS SECTION.
      *********************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/CHKSEC.CPY".
       SET-TY-KEY.
           MOVE LN-OWNBR   TO TY-BRNO.
           MOVE TRANS-DATE TO TY-DATE.
           MOVE LN-CLASS   TO TY-CLASS.
       GET-TY1-FILE.
           PERFORM OPEN-TY1-FILE.
           PERFORM SET-TY-KEY.
           PERFORM READ-TY1-FILE.
           IF IO-FG = 9
              PERFORM CLEAR-TY-FILE
              PERFORM SET-TY-KEY
              PERFORM WRITE-TY1-FILE
              IF IO-FG = 9 AND E-CODE NOT = "22"
                 GO TO IO-ERROR
              ELSE
                 PERFORM SET-TY-KEY
                 PERFORM READ-TY1-FILE
                 IF IO-FG NOT = 0
                    GO TO IO-ERROR.

       READ-LOAN-RECORD.
           MOVE LOAN-KEY TO LN-ACCTNO.
           PERFORM OPEN-LN1-FILE.
           PERFORM READ-LN1-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.
       SUM-OF-FEEAMTS.
           COMPUTE AMT-WK2 = AMT-WK2 + LN-FEEAMT(SUB).
       GET-BW1.
           PERFORM READ-BW1-FILE.
           IF IO-FG = 9
              MOVE " " TO BW-FNAME
              MOVE "NOT FOUND" TO BW-LNAME.
           PERFORM APPEND-BW-NAMES.
       APPEND-BW-NAMES.
           MOVE BW-LNAME TO INSPECT-W.
           MOVE BW-FNAME TO ADD-ON.
           PERFORM ADD-IT.
       ADD-IT.
           INSPECT INSPECT-W REPLACING FIRST "   " BY "  +".
           INSPECT INSPECT-W REPLACING FIRST
              " +                       " BY ADD-ON.
       NUMBW-TO-ALPBW.
           MOVE " " TO ALP-BW.
           IF BW-SSNO NOT = 0
              MOVE BW-SSNO TO NUM-BW
              MOVE NUM-BW123 TO ALP-BW123
              MOVE NUM-BW45 TO ALP-BW45
              MOVE NUM-BW6789 TO ALP-BW6789
              MOVE "-" TO ALP-HYPHEN1 ALP-HYPHEN2.
       NOT-FOUND.
           MOVE "NOT FOUND" TO MESS.
           PERFORM SEND-MESS.
      *MSG-LINE.
      *    CALL TMMSG USING MESS.
       SEND-LEGEND.
           MOVE "S  A400MSEL." TO VDU.
           MOVE MESS TO VDUBUF.
           PERFORM SCREENIO.
       DISPLAY-ALL-LINES.
           MOVE LINE-BUF  TO WR-BUF.
           MOVE LINE-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       RESET-LIST.
           MOVE SPACES TO WR-BUF.
           MOVE MESS TO WR-LIST.
           PERFORM CALL-WRFORM.
       RESET-ENTRY-LINE.
           MOVE REV-MESS TO MESS.
           PERFORM RESET-LIST.
       RESET-SEL.
           MOVE "SEL MSEL." TO MESS.
           PERFORM RESET-LIST.
       SEND-ENTRY-LINE.
           MOVE EL-BUF TO WR-BUF.
           MOVE REV-MESS TO WR-LIST.
           PERFORM CALL-WRFORM.
       ADJUST-LINES-TO-END.
           COMPUTE LN-WK = B-LN - B-TOP-LN.
           IF LN-WK NOT < S-LNS
              SUBTRACT S-LNS-LESS1 FROM B-LN GIVING B-TOP-LN
              PERFORM SCROLL-UP.

       COPY "LIBLP/POSTDATE.CPY".
       COPY "LIBLP/LPSRCD.CPY".
       COPY "LIBGB/CAPASSWD.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/REBATE.CPY".
       COPY "LIBLP/LPIBPC.CPY".
       COPY "LIBLP/LPRATE.CPY".
       COPY "LIBLP/LPUPWK.CPY".
       COPY "LIBGB/RBACT.CPY".
       COPY "LIBLP/LPAPRS.CPY".
       COPY "LIBLP/LPAPRZ.CPY".
       COPY "LIBLP/LPITRM.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBLP/LPXDTE.CPY".
       COPY "LIBLP/SETCTBL.CPY".
       COPY "LIBLP/LPUPER.CPY".
       COPY "LIBLP/LPAMTS.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBLP/LPFMCL.CPY".
       COPY "LIBLP/LPNOCL.CPY".
       COPY "LIBLP/LPCKCL.CPY".
       COPY "LIBLP/LPCXCL.CPY".
       COPY "LIBLP/LPTYCL.CPY".
       COPY "LIBLP/LPTRCCL.CPY".
       COPY "LIBLP/LPLTDCL.CPY".

      *********************************
       IO-ROUTINE SECTION.
      *********************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPTYGS_SQL.CPY".
       COPY "LIBLP/LPTRLGS_SQL.CPY".
       COPY "LIBLP/LPTRCGS_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPNOGS_SQL.CPY".
       COPY "LIBLP/LPLTDGS_SQL.CPY".
       COPY "LIBLP/LPLTIGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBLP/LPFMGS_SQL.CPY".
       COPY "LIBLP/LPFDGS_SQL.CPY".
       COPY "LIBLP/LPCXGS_SQL.CPY".
       COPY "LIBLP/LPCKGS_SQL.CPY".
       COPY "LIBLP/LPCAGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPLN1IN.CPY".
       COPY "LIBLP/LPLC1RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBLP/LPNO1RN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPTY1IN.CPY".
       COPY "LIBLP/LPCA1IN.CPY".
       COPY "LIBLP/LPFD1RN.CPY".
       COPY "LIBLP/LPFM1IN.CPY".
       COPY "LIBLP/LPWKI.CPY".
       COPY "LIBLP/LPCX1IN.CPY".
       COPY "LIBLP/LPTRC1IN.CPY".
       COPY "LIBLP/LPTRL1IN.CPY".
       COPY "LIBLP/LPCK1IN.CPY".
       COPY "LIBLP/LPLTD1IN.CPY".
       COPY "LIBLP/LPLTI1RN.CPY".
      * REPAY NEW LOANS
       OPEN-WK-REPAY-LN-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE WK-REPAY-LN-IFN TO E-FILE.
           OPEN OUTPUT WK-REPAY-LN-FILE.
           IF IO-FG = 8
              GO TO OPEN-WK-REPAY-LN-FILE-OUTPUT.
           IF IO-FG = 7
              CLOSE WK-REPAY-LN-FILE
              GO TO OPEN-WK-REPAY-LN-FILE-OUTPUT.
       WRITE-WK-REPAY-LN-FILE.
           PERFORM WRITE-IT.
           MOVE WK-REPAY-LN-IFN TO E-FILE.
           WRITE WK-REPAY-LN-REC.
           IF IO-FG = 8
              GO TO WRITE-WK-REPAY-LN-FILE.
       CLOSE-WK-REPAY-LN-FILE.
           PERFORM CLOSE-IT.
           CLOSE WK-REPAY-LN-FILE.
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      ***************************************************
       END-ROUTINE SECTION.
      ***************************************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY LONPD-SCREEN.
           EXIT PROGRAM.
