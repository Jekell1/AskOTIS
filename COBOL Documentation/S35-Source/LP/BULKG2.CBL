       IDENTIFICATION DIVISION.
       PROGRAM-ID.      BULKG2.
       DATE-WRITTEN.    6/10/03.
      ******************************************************************
      *
      *    DIR :  LP
      *    DESC:  BULK TRANSFER BY GROUP UPDATE
      *           TRANSFER LOANS SELECTED IN BULKG1.C
      *           REGACC PR# 0022
      *
      *****************************************************************
      * N O T E:
      *   PLEASE VERIFY THAT CLAST2 DOES NOT NEED YOUR CHANGE !!
      *   THEY ARE VERY SIMILAR PROGRAMS.
      *
      *   BULKT2 MAY NEED YOUR CHANGE !
      *   THEY ARE VERY SIMILAR PROGRAMS.
      *
      * N O T E:
      *   (NO GL ENTRIES) ARE POSTED FOR ACCOUNTS TRANSFERED WITH
      *                   RESERVE PRIN
      *                   RESERVE CHARGES
      *                   PARTICIPATION HELD
      *                   PARTICIPATION PAID
      *   MERCURY DID NOT WANT TO TRANSFER THEM.  RE: CINDY SHAFFER
      *
      *****************************************************************
      * REV:
      * BARB 061303 NEW REGACC PR# 0022
      * BAH 040819 DO NOT CREATE FORM REQUEST IF LOAN OR BORROWER IS
      *            CONFIDENTIAL, PR# 2343
      * BAH 050324 ADDED THIS CHANGE FROM BULKT2 BLV 031216 14 ERROR
      *            SITUATIONS WENT TO BULK-TRANSFER-ERR-EXIT, WHICH
      *            GIVES A GENERIC "AN ERROR HAS OCCURED DURING THE TRANSFER"
      *            MESSAGE.  I THOUGHT IT MIGHT BE HELPFUL TO ACTUALLY SEND
      *            A MESSAGE SAYING WHAT THE #$%@ PROBLEM IS!
      * BAH 050324 ADDED THIS CHANGE FROM BULKT2 BAH 040518 MOVE TO-LN-ACCTNO
      *            TO MA-ACCTNO, IN CASE ACCOUNT NUMBER WAS CHANGED !!
      *            LENDMARK PL# 441
      * BAH 050324 DONT SET BW-LOANS = 1 IF PAIDOUT ACCOUNT, CENTRIX PL# 471
      *            DONT SUBTRACT 1 FROM TRANSFERRING BRANCH EITHER
      * BLF 050603 PUT ANOTHER CHECK TO VERIFY DAY IS OPEN FOR 'TO' BRANCH
      *            AFTER ENTER-PARAMETERS, RIGHT BEFORE TAG IS CREATED,
      *            DOUBLE CHECK WAS ALREADY IN 'FROM' LOOP, PL# 495
      * BAH 060906 FROM BULKT2, REGACC WANTS FORM REQUEST CREATE IN THE "TO"
      *            BRANCH, PR#132
      * BAH 060906 TRANSFER-LX-REC WAS MISSING RECORDS BECAUSE IT WAS ADDING
      *            1 TO THE LX-SEQNO FOR THE NEXT RECORD, BUT THE SEQNO IS
      *            BEFORE THE LX-CODE, SO YOU MIGHT HAVE SEQNO 105, CODES A
      *            AND G, AND YOU WOULD MISS THE G ! REGACC KEVIN
      * ****
      * 2008/01/15 JUST BROUGHT THE FOLLOWING CHANGES ALL OVER FROM BULKT2
      * ****
      *  BLF 070216 CHANGED SAVE-BM-SEQ TO SAVE-FROM-BM-SEQ AND
      *             CHANGED SAVE--TO--BM-SEQ TO SAVE-TO-BM-SEQ;
      *             WORLD FOUND A DEAD LOOP IN THE CREATION ROUTINE FOR
      *             BORROWER MEMOS.  THEY WERE TRANSFERRING PAID OUT ACCOUNTS,
      *             SO LOTS OF ACCOUNTS FOR EACH BORROWER.  ONE BORROWER HAD
      *             99 PRIORITY MEMOS IN TARGET DIRECTORY, SO WHEN ROUTINE
      *             ADDED 1 TO SAVE-BM-DATE, THE DATE WAS 99999999 SINCE IT
      *             WAS A PRIORITY MEMO, AND ROUTINE LOOPED BACK TO THE 1ST
      *             MEMO FOR THAT BORROWER!  DID NOT FIX TODAY, JIM WILL
      *             SUBMIT REQUEST NEXT WEEK REVISING HOW WE TRANSFER
      *             BORROWER MEMOS.  BARB & CINDY
      *  BLF 070219 FIXED LOOP DESCRIBED ABOVE, IN BOTH BORROWER & LOAN MEMO
      *             ROUTINES - IF SEQ & DATE ARE ALL 9'S, GET OUT OF ROUTINE
      *  BAH 070323 FIRST REMOVE ALL THE BW MEMOS IN THE "TO" FILE, THEN
      *             WRITE THE NEW MEMOS. JIM R PR# 2925
      *  BLF 070816 IF GP-PURGE-TO-ARCHIVE = "Y", CHECK TO SEE IF ACCT #
      *             ALREADY EXISTS IN ARCHIVED LOAN FILE FOR "TO" BRANCH;
      *             IF SO, SKIP TO NEXT NUMBER, WORLD PR# 547
      * ****
      * 2008/01/15 JUST BROUGHT THE ABOVE CHANGES ALL OVER FROM BULKT2
      * ****
      *  BAH 080115 IF GP-I-AM-REGACC AND P&L, REMOVE LN-ACCT-FROZEN AND REASON
      *             REGACC PR# 196
      *   CS 081110 ADDED TEST ON GP-INCL-OTH-IN-EARNED. IF 'R' DONT' INCLUDE
      *             OTHER(ANTIC6) OR MAINTFEE(ANTIC4) IN EARNED/UNEARNED.
      *             REGACC USES MAINTFEE TO AMORTIZE PARTICIPATION HELD
      *             EXPENSE REGACC PR#210
      *  BAH 090501 MISSING PAYMENT SCHEDULE MESSAGE FROM CRNOR2 BECAUSE THE
      *             LN-REC THAT GETS PASSED TO CRNOR2 HAD THE LN-OWNBR PUT
      *             BACK TO THE ORIG OWNING BRANCH BEFORE THE TRANSFER AND THE
      *             ACCOUNT # HAD CHANGED AND THE ORIG OWNING BRANCH DID NOT
      *             HAVE THE LT FOR THE NEW ACCOUNT, NEEDED NEW BRANCH IN
      *             THE LN-OWNBR FIELD. MOVED CREATE-FM-REC BEFORE THE MOVE
      *             OF THE LN-REC BACK TO ORIGINAL. REGACC
      *  BAH 110113 BORROWER MEMO WAS TRYING TO REPORT A CHANGE IN ACCOUNT
      *             NUMBER BUT YOU DIDNT KNOW YET SO IT WAS REPORTING A BOGUS
      *             ACCOUNT NUMBER. REMOVED THE ACCOUNT # PORTION, DONT CARE.
      *             JUST REPORT THE BRANCH NUMBERS, REGACC PL# 712
      *  BAH 110225 FORM REQUESTS FOR REGACC WERE NOT CORRECT IF ACCOUNT #
      *             CHANGED IN THE "TO" BRANCH, WAS WRITING FM WITH OLD ACCTNO
      *             PL# 726, REGACC
      *   CS 120201 IF GP-USE-ACCRUAL-EARN-FILE 'Y',CREATE THE ACCRUAL EARNINGS
      *             (AE) FOR THE MONTHEND DATE OF THE TRANSFER GETTING THE
      *             PRIOR MONTH IB ACCRUAL AMOUNT PR#299 (SEE LONPF2 COMMENTS)
      *  CS 120307 IF LN-ESCROW-FG = "Y" OR "I", TRANSFER ES REGENCY PL#768
      * KEC 130205 MADE CHANGES TO LOAN APPS - ADDED MX-FILE. REGECY #4196
      *  CS 131209 ADDED NEW VALUE IF GP-USE-ACCRUAL-EARN-FILE = "R" REGACC #347
      *  CS 150422 USE THE 'RC-FILE' TO DETERMINE IF A DAY IS OPEN, INSTEAD
      *             OF AN ACCESS CALL ON THE OP-PATH
      * BAH 150810 ADDED GP-TRANSFER-CONPROFILE, REGMGM PR# 4799
      * KEC 2015.1120 FIXED RC-TRANS-DATE FROM YYYMMDD TO CCYYMMDD.
      *  CS 2015.1210 IN TRANSFER-LP-REC TO NEW BRANCH, SET UP LP-BRNO AS TO-BR
      *               IN FROM BRANCH, SET LP-BRNO = FROM-BRANCH
      * BAH 2016.0118 SPLIT OUT LTFILE, PD0003
      * BAH 2016.0209 SPLIT OUT LSFILE, PD0003
      *  CS 2016.0216 MOVE TO-BR TO LX-BRNO
      * BAH 2016.0218 EXPANDED LM-REC AND BM-REC BRNO, PD0003
      * KEC 2016.0219 FIXED KEYS & RECS FOR FD-REC.
      *               CHANGED FMNO/FORMNO/NEXTFM/SETFM FROM 3 TO 4 BYTES.
      * BLM 2016.0224 FIX FOR NEW TY DATE FORMAT
      * KEC 2016.0411 CHANGED MA-BRANCH TO MA-BRNO; MOVED BEFORE MA1-KEY.
      * KEC 2016.0506 CHANGED AM-APPNO TO AM-APP-NUMBER TO MATCH MA1-KEY.
      *               CHANGED MA-APPNO TO MA-APP-NUMBER TO MATCH MA1-KEY.
      * KEC 2016.0509 CHANGED LN-APPNO TO USE LN-APP-NUMBER FOR MA1-KEY.
      * BLM 2016.0511 FIX FOR NEW DS-DATE FORMAT, FIX DS-UNEARNED, PD0003
      *  CS 2016.0512 ADD MOVE OF TO-BR TO ES-BRNO
      *  CS 2016.0513 SET FR-OWNBR TO AE-BRNO IN FROM-BRANCH, DONT SEE TO BRANCH
      *               CREATED FOR AEFILE
      *  CS 2016.0517 SET RL-BRNO FROM TO-BRNO
      * MJD 2016.0609 ADDED SET/UNSET OF BICREA-CALLED-FROM (EXTERNAL VAR).
      *               IT WAS MISSING ANS WAS BEING TESTING IN BICREA.CBL
      * KEC 2016.0620 REMOVED USE OF THE MX-FILE [LIBLA/LAFDMX].
      *               REMOVED USE OF OLD STATUS ROUTINES [LIBLA/MASTATUS(W)].
      *               ADDED USE OF NEW MA-STATUS-INFO FIELDS; AS WELL AS
      *               ADDED USE OF NEW ROUTINES [LIBLA/LAFRMA & LAWSMA].
      * KEC 2016.0818 REMOVED USE OF THE BA-FILE [LIBLA/LAFDMX].
      *               REVISED LA-REC LAYOUT.
      * BAH 2016.0825 DO NOT OVERWRITE LN-POFFDATE AND LN-DATE-PAID-LAST ON
      *               PAIDOUT ACCOUNTS, WORLD PR#1178
      * KEC 2016.0914 SET LR-BRNO USING LN-OWNBR.
      * KEC 2016.1026 REMOVED USING HOLD-TR1-KEY.
      *               ADDED LIBLP/LPWSTR (NEW) FOR LPWSTR-TR1-KEY (HOLD/SAVE).
      * KEC 2016.1209 CHANGED AE-DATE AS ACTIVE CCYYMMDD.
      * KEC 2017.0105 CHANGED DS-DATE TO DS-DATE-CCYYMM FOR CONSISTENCY
      *               WITH THE DATE FIELDS IN BY & UD FILE.
      * KEC 2017.0119 CHANGED FD-SMARTPORT TO FD-OPTION [88 LEVELS].
      * KEC 2017.0425 REMOVED 'LE' FILE SINCE NOT USED.
      * BAH 2017.0509 ACTIVATE 8 DIGIT DATES, PD0006
      * KEC 2017.0607 ADDED 'LN' FILE PREFIX FOR WORKING STORAGE FIELDS.
      * KEC 2017.0613 ADDED LIBLP/TFRMEMOW; USED TO CREATE/CHECK MEMOS (BM/LM)
      *  CS 2018.0329 FORCED 0 TO TB-LAWCODE TO MATCH A15. NOT SURE WHY THIS
      *               IS NOT PD-LAWCODE, BUT ITS NOT SET IN A15 OTHER THAN
      *               IN THE CLEAR ROUTINE AND ITS ALWAYS 0
      * KEC 2018.0823 LOAN APP MODULE REMOVAL FROM A30 [LARM30]
      * BAH 2018.1003 GLOBAL BWFILE
      * BAH 2018.1004 GLOBAL BXFILE, BMFILE
      * BAH 2018.1030 GLOBAL GBFILE
      *  CS 2018.1107 COMMENTED AEFILE LOGIC
      *  CS 2018.1107 FIXED ACCESS CALL ON GLOBAL LSL-FILE 
      *               ADD TEST ON LSL-BRNO NOT = PD-OWNBR ON IO-CHECK FOR LSL
      *  CS 2018.1112 FIXED ACCESS CALL ON GLOBAL LSJ-FILE 
      *            ADD TEST ON LSJ-BRNO NOT = PD-OWNBR ON IO-CHECK FOR LSL
      * BAH 2018.1112 GLOBAL FILES
      * BAH 2019.0118 GLOBAL LXFILE, SPLIT
      * BAH 2019.0312 MOVED FM-BRNO2 INTO LPWSFM-EDIT-FM-INITKEY TO REMOVE THE
      *              REDEFINE IN THE FM-REC
      * BAH 2019.0403 SPLIT OUT TRFILE
      * BAH 2019.0613 ADDED CLEAR-FM-FILE (LPFMCL) AND SET FM-USERID
      * BAH 2019.0625 REMOVED LN-REVOLVING, PT0153
      * BAH 2019.0628 REMOVED RT AND RL REVOLVING
      * BAH 2019.0709 REMOVED LRFILE PT0151
      * BAH 2019.0919 REMOVED ACCESS-CALL ON ALL FILES
      *  CS 2019.1004 ADDED ESFILE TO DECLARATIVES
      *  CS 2019.1008 THE WORK FILE (GRBULK####) IS NOW IN THE GLOBAL DP
      *               DIRECTORY. BULKG2 CREATES 3 UPDATE TAGS, GRBULKUPD####,
      *               A TAG REPLACING GR WITH EACH BRANCH IN THE GROUP AND 
      *               INCLUDES THE NAME OF THE GROUP FOR EXAMPLE, IF FROM
      *               GROUP CS (MEMBERS BR# 3 AND 10), THE 2 TAGS WOULD BE
      *               0003CSBULKUPD0001, 0010CSBULKUPD0001, AND THE GROUP
      *               TAG CSBULKUPD0001
      *  CS 2019.1015 SAVED BULKG2.191015
      * BAH 2022.0321 UPDATE LSM-TRANS-FROM-BRNO, ACCTNO AND DATE TO TRACK
      *               WHERE THE TO TRANSFER CAME FROM FOR METRO2 "L1"  #1547
      * BAH 2022.0524 ADDED ATB-ACTIONCD2, SECURITIZATION #1551
      * BAH 2022.0802 COMMENTED OUT LOGUID
      * BAH 2022.0803 HARD CODED START
      * BAH 2022.0921 CLEAR-LSM-FILE #1570
      * BAH 2024.0130 ADDED TRANSFER-NO-REC
      * BAH 2024.0405 ADDED UPDATE-FRNO-FILE IN FROM BRANCH AS PAIDOUT
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
       COPY "LIBLP/LPFSBULK.CPY".

      *    SELECT ARCH-LN-FILE ASSIGN TO LN-ARCHIVE-PATH
      *           ORGANIZATION INDEXED
      *           ACCESS DYNAMIC
      *           LOCK MODE AUTOMATIC WITH LOCK ON RECORD
      *           RECORD KEY ARCH-LN1-KEY
      *           ALTERNATE RECORD KEY ARCH-LN3-KEY
      *                              = ARCH-LN-OWNBR
      *                                ARCH-LN-CLASS
      *                                ARCH-LN-ACCTNO
      *                                        WITH DUPLICATES
      *           ALTERNATE RECORD KEY ARCH-LN4-KEY WITH DUPLICATES
      *           ALTERNATE RECORD KEY ARCH-LN5-KEY WITH DUPLICATES
      *           ALTERNATE RECORD KEY ARCH-LN6-KEY WITH DUPLICATES
      *           ALTERNATE RECORD KEY ARCH-LN7-KEY WITH DUPLICATES
      *           FILE STATUS FILE-STAT.

       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".
       COPY "LIBLP/LPFDBULK.CPY".
      ********************************************************************
      *   ARCHIVED LOAN MASTER FILE
      ********************************************************************
      *FD  ARCH-LN-FILE
      *    LABEL RECORDS ARE STANDARD.
      *COPY "LIBLP/LP01LN.CPY" REPLACING LEADING "LN" BY "ARCH-LN"
      *                              TRAILING "STFG" BY "ARCH-LN-STFG".
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LP/BULKG2.CBL S35 02/20/24-14:06:59 >".
      ****************************
      *     F I L E   S P E C S  *
      ****************************
      * MOVES LN-REC TO PD-REC FOR STORAGE
       COPY "LIBLP/LP01PD.CPY".
       COPY "LIBLP/LP01ES.CPY".
       COPY "LIBLP/LP01ES_SQL.CPY".
       COPY "LIBLP/LPWSES.CPY".
       COPY "LIBLP/LP01TRB.CPY".
       COPY "LIBLP/LP01TRB_SQL.CPY".
       COPY "LIBLP/LPWSTRB.CPY".
       COPY "LIBGB/GB01GB.CPY".
       COPY "LIBGB/GB01GB_SQL.CPY".
       COPY "LIBGB/GBWSGB.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBGB/GB01RC.CPY".
       COPY "LIBGB/GB01RC_SQL.CPY".
       COPY "LIBGB/GBWSRC.CPY".
       COPY "LIBLP/LP01LX.CPY".
       COPY "LIBLP/LP01LX_SQL.CPY".
       COPY "LIBLP/LPWSLX.CPY".
       COPY "LIBLP/LP01LXE.CPY".
       COPY "LIBLP/LP01LXE_SQL.CPY".
       COPY "LIBLP/LPWSLXE.CPY".
       COPY "LIBLP/LP01LXG.CPY".
       COPY "LIBLP/LP01LXG_SQL.CPY".
       COPY "LIBLP/LPWSLXG.CPY".
       COPY "LIBLP/LP01LC.CPY".
       COPY "LIBLP/LP01LC_SQL.CPY".
       COPY "LIBLP/LPWSLC.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01LM.CPY".
       COPY "LIBLP/LP01LM_SQL.CPY".
       COPY "LIBLP/LPWSLM.CPY".
       COPY "LIBLP/LP01LP.CPY".
       COPY "LIBLP/LP01LP_SQL.CPY".
       COPY "LIBLP/LPWSLP.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01BM.CPY".
       COPY "LIBLP/LP01BM_SQL.CPY".
       COPY "LIBLP/LPWSBM.CPY".
       COPY "LIBLP/LP01TY.CPY".
       COPY "LIBLP/LP01TY_SQL.CPY".
       COPY "LIBLP/LPWSTY.CPY".
       COPY "LIBLP/LP01DS.CPY".
       COPY "LIBLP/LP01DS_SQL.CPY".
       COPY "LIBLP/LPWSDS.CPY".
       COPY "LIBLP/LP01FD.CPY".
       COPY "LIBLP/LP01FD_SQL.CPY".
       COPY "LIBLP/LPWSFD.CPY".
       COPY "LIBLP/LP01FM.CPY".
       COPY "LIBLP/LP01FM_SQL.CPY".
       COPY "LIBLP/LPWSFM.CPY".
       COPY "LIBLP/LP01NO.CPY".
       COPY "LIBLP/LP01NO_SQL.CPY".
       COPY "LIBLP/LPWSNO.CPY".
       COPY "LIBLP/LP01LTA.CPY".
       COPY "LIBLP/LP01LTA_SQL.CPY".
       COPY "LIBLP/LPWSLTA.CPY".
       COPY "LIBLP/LP01LTB.CPY".
       COPY "LIBLP/LP01LTB_SQL.CPY".
       COPY "LIBLP/LPWSLTB.CPY".
       COPY "LIBLP/LP01LTC.CPY".
       COPY "LIBLP/LP01LTC_SQL.CPY".
       COPY "LIBLP/LPWSLTC.CPY".
       COPY "LIBLP/LP01LTD.CPY".
       COPY "LIBLP/LP01LTD_SQL.CPY".
       COPY "LIBLP/LPWSLTD.CPY".
       COPY "LIBLP/LP01LTF.CPY".
       COPY "LIBLP/LP01LTF_SQL.CPY".
       COPY "LIBLP/LPWSLTF.CPY".
       COPY "LIBLP/LP01LTI.CPY".
       COPY "LIBLP/LP01LTI_SQL.CPY".
       COPY "LIBLP/LPWSLTI.CPY".
       COPY "LIBLP/LP01LTL.CPY".
       COPY "LIBLP/LP01LTL_SQL.CPY".
       COPY "LIBLP/LPWSLTL.CPY".
       COPY "LIBLP/LP01LTP.CPY".
       COPY "LIBLP/LP01LTP_SQL.CPY".
       COPY "LIBLP/LPWSLTP.CPY".
       COPY "LIBLP/LPWSIX.CPY".
       COPY "LIBLP/LP01LSD.CPY".
       COPY "LIBLP/LP01LSD_SQL.CPY".
       COPY "LIBLP/LPWSLSD.CPY".
       COPY "LIBLP/LP01LSF.CPY".
       COPY "LIBLP/LP01LSF_SQL.CPY".
       COPY "LIBLP/LPWSLSF.CPY".
       COPY "LIBLP/LP01LSG.CPY".
       COPY "LIBLP/LP01LSG_SQL.CPY".
       COPY "LIBLP/LPWSLSG.CPY".
       COPY "LIBLP/LP01LSJ.CPY".
       COPY "LIBLP/LP01LSJ_SQL.CPY".
       COPY "LIBLP/LPWSLSJ.CPY".
       COPY "LIBLP/LP01LSL.CPY".
       COPY "LIBLP/LP01LSL_SQL.CPY".
       COPY "LIBLP/LPWSLSL.CPY".
       COPY "LIBLP/LP01LSM.CPY".
       COPY "LIBLP/LP01LSM_SQL.CPY".
       COPY "LIBLP/LPWSLSM.CPY".
       COPY "LIBLP/LP01LSP.CPY".
       COPY "LIBLP/LP01LSP_SQL.CPY".
       COPY "LIBLP/LPWSLSP.CPY".
       COPY "LIBLP/LP01LSR.CPY".
       COPY "LIBLP/LP01LSR_SQL.CPY".
       COPY "LIBLP/LPWSLSR.CPY".
       COPY "LIBLP/LP01LSX.CPY".
       COPY "LIBLP/LP01LSX_SQL.CPY".
       COPY "LIBLP/LPWSLSX.CPY".
       COPY "LIBLP/LP01BX.CPY".
       COPY "LIBLP/LP01BX_SQL.CPY".
       COPY "LIBLP/LPWSBX.CPY".
       COPY "LIBLP/LPWSBULKG.CPY".
      *-----------------------------------------------------------------
       COPY "LIBLP/LPWKPATH.CPY".
      *-----------------------------------------------------------------

       01  PR-IFN          PIC X(2) VALUE "PR".
       01  WK-IFN          PIC X(2) VALUE "WK".

       01  HOLD-FM1-KEY     PIC X(28)     VALUE SPACES.
       01  HOLD-FM-PATH     PIC X(50)    VALUE SPACES.

      *********************************************************
      *   LNFILE ARCHIVE PATH WORKERS
      *********************************************************
      *01  LN-ARCHIVE-PATH.
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  LN-ARCHIVE-DIR1        PIC X(3).
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  LN-ARCHIVE-DIRECTORY   PIC X(4).
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  LN-ARCHIVE-MACHINE     PIC XX.
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  LN-ARCHIVE-DATA        PIC X(6).
      *    03  FILLER                 PIC X(10) VALUE "/LP/LNFILE".

       01  SAVE-BRANCH                PIC 9(4) VALUE 0.
       01  TEST-OPEN-BRANCH           PIC 9(4) VALUE 0.

       COPY "LIBLP/BULKGRW.CPY".

      **********************
      *   LINE WORKERS     *
      **********************
       01  LN-FEED              PIC 99    VALUE 0.
       01  LN-COUNT             PIC 999   VALUE 99.
       01  PAGE-NUMBER          PIC 9(4)  VALUE 0.

      **********************
      *   MISC WORKERS     *
      **********************
       01  FROM-BRANCH                 PIC 9(4)      VALUE 0.
       01  FROM-MACHINE                PIC XX.
       01  FROM-DATA-PATH              PIC X(6).

       01  WK-GLNO                 PIC 9(11).
       01  WK-GLNO-X              REDEFINES WK-GLNO.
           03  WK-GLNO-BR          PIC 9(4).
           03  WK-GLNO-NO          PIC 9(7).

       01  FIRST-TIME           PIC X        VALUE "Y".
       01  RECORDS-FOUND        PIC X        VALUE " ".
           88  NO-RECORDS-FOUND              VALUE "N".
       01  SUMMARY-FG           PIC X        VALUE "N".
       01  BR-OPEN-FG           PIC X.

       01  PL-ONLY              PIC X        VALUE " ".
       01  ZBAL                 PIC X        VALUE " ".
       01  ONHAND               PIC X        VALUE " ".
       01  SOLD                 PIC X        VALUE " ".
       01  REDEEM               PIC X        VALUE " ".

       01  TO-STATE             PIC XX.
       01  ERR-FG               PIC X        VALUE " ".
       01  RTN-FG               PIC X        VALUE " ".
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".
       01  REC-EXISTS           PIC X.
       01  NAME-WORKER          PIC X(34).
       01  FIRST-NAME           PIC X(16).
       01  TRANS-DATE           PIC 9(8)     COMP-3 VALUE 0.
       01  BW-SUB               PIC 999.
       01  SAVE-LM-DATE         PIC 9(8).
       01  SAVE-LM-SEQ          PIC 9(3).
           COPY "LIBLP/LP01BM.CPY" REPLACING LEADING "BM" BY "HOLD-BM".
       01  SAVE-BM-DATE         PIC 9(8).
       01  SAVE-FROM-BM-SEQ     PIC 9(3).
       01  SAVE-TO-BM-SEQ       PIC 9(3).
       01  HOLD-LP-PDTH-DATE    PIC 9(8).
       01  HOLD-LP-IBPC         PIC X.
       01  HOLD-TB-LINE         PIC 999.
       01  HOLD-TO-BRNAME       PIC X(40).
       01  PART-RC-AMT          PIC S9(8)V99.
       01  REF.
           03  FILLER           PIC X(1).
           03  REF-BR           PIC 9(4).
       01  SAVE-WORKERS.
           03  SAVE-UNEARN      OCCURS 6 TIMES
                                PIC S9(7)V99   COMP.

       01  HOLD-LN-OWNBR        PIC 9(4)     VALUE 0.
       01  HOLD-LN-CLASS        PIC 9(3)     VALUE 0.

       01  HOLD-LPS-NUM.
           03  H-LPS-CLASS      PIC XXX.
           03  H-LPS-NUM        PIC X(13).

       01  TO-LN-ACCTNO         PIC 9(8).

       01  SAVE-LP-SEQNO        PIC 9(5).
       01  SAVE-LX-SEQNO        PIC 9(5).
       01  SAVE-LT-SEQNO        PIC 9(3).
       01  SAVE-LS-SEQNO        PIC 99.

       01  SAVE-RDSUB           PIC 99       VALUE 0.
       01  SAVE-CSUB            PIC 99       VALUE 0.
       01  SAVE-PSUB            PIC 99       VALUE 0.
       01  SAVE-OUTBAL          PIC S9(7)V99 VALUE 0.

       01  UNEARN-INT           PIC S9(7)V99 COMP-3 VALUE 0.
       01  UNEARN-SC            PIC S9(7)V99 COMP-3 VALUE 0.
       01  UNEARN-MF            PIC S9(7)V99 COMP-3 VALUE 0.

       01  EXCEPTION-MISSING    PIC X(43) VALUE
           "                  MISSING SINCE SELECTION!!".
       01  EXCEPTION-VOID       PIC X(44) VALUE
           "                    VOIDED SINCE SELECTION!!".
       01  EXCEPTION-RESC       PIC X(44) VALUE
           "                RESCISSION SINCE SELECTION!!".
       01  EXCEPTION-BT         PIC X(44) VALUE
           "          NO LONGER BULKED SINCE SELECTION!!".
       01  EXCEPTION-PL         PIC X(39) VALUE
           "                  P&L SINCE SELECTION!!".
       01  EXCEPTION-NOT-PL     PIC X(43) VALUE
           "                  NOT P&L SINCE SELECTION!!".
       01  EXCEPTION-ZERO       PIC X(44) VALUE
           "                  ZERO BAL SINCE SELECTION!!".
       01  EXCEPTION-NOT-ZERO   PIC X(48) VALUE
           "                  NOT ZERO BAL SINCE SELECTION!!".
       01  EXCEPTION-REPO       PIC X(40) VALUE
           "                  REPO SINCE SELECTION!!".
      ********************************************
      *               T O T A L S                *
      ********************************************
      *
      *    TOTAL-TBL:
      *       LEV(1) =  BY LOAN CLASS
      *          (2) =  BY BRANCH
      *          (3) =  BY COMPANY
      *
      *    T-AGETBL :
      *          (1) =   30   DAYS OLD    CSUB = 1
      *          (2) =   60   DAYS OLD    CSUB = 2
      *          (3) =   90   DAYS OLD    CSUB = 3
      *          (4) =  120   DAYS OLD    CSUB = 4
      *          (4) =  150   DAYS OLD    CSUB = 5
      *          (4) =  180   DAYS OLD    CSUB = 6
      *          (4) =  210   DAYS OLD    CSUB = 7
      *          (5) =  <M>   MISS PAYS   CSUB = 8
      *          (6) =  <P>   POTENTIALS  CSUB = 9
      *          (7) =  CUR   DAYS OLD    CSUB = 10
      *
      *    T-TYPETBL:
      *       TYP(1) = CONTRACTUAL
      *          (2) = RECENCY
      *          (3) = PAID AHEAD
      *                CONTRACTUALLY CURRENT
      *                    AND DELINQ-RECENCY
      *
      ********************************************
       01  TOTAL-TABLE            VALUE ZEROS.
           03  TOTAL-TBL          OCCURS 3 TIMES.
               05  T-AGETBL       OCCURS 10 TIMES.
                   07  T-TYPETBL  OCCURS 3 TIMES.
                       09  T-NO   PIC S9(5).
                       09  T-BAL  PIC S9(9)V99.
                       09  T-INT  PIC S9(7)V99.
                       09  T-OTH  PIC S9(7)V99.
                       09  T-UNE  PIC S9(7)V99.
                       09  T-ERN  PIC S9(7)V99.
                       09  T-IER  PIC S9(7)V99.
       01  TT-TOTALS.
           03  TT-TYPETBL         OCCURS 3 TIMES.
               05  TT-NOS         PIC S9(5)    COMP-3.
               05  TT-BAL         PIC S9(9)V99 COMP-3.
               05  TT-INT         PIC S9(7)V99 COMP-3.
               05  TT-OTH         PIC S9(7)V99 COMP-3.
               05  TT-UNE         PIC S9(7)V99 COMP-3.
               05  TT-ERN         PIC S9(7)V99 COMP-3.
               05  TT-IER         PIC S9(7)V99 COMP-3.

       01  LEV                    PIC 9(6) COMP-3.
       01  AGE                    PIC 9(6) COMP-3.
       01  TYP                    PIC 9(6) COMP-3.
       01  SUB                    PIC 9(6) COMP-3.
       01  SUBX                   PIC 9(6) COMP.
       01  SUBY                   PIC 9(6) COMP.

       01  DD-TOTAL-TABLE.
           03  DD-TOT-TBL         OCCURS 32 TIMES.
               05  DD-NO          PIC S9(5)    COMP-3.
               05  DD-BAL         PIC S9(9)V99 COMP-3.
               05  DD-INT         PIC S9(7)V99 COMP-3.
               05  DD-OTH         PIC S9(7)V99 COMP-3.
               05  DD-UNE         PIC S9(7)V99 COMP-3.

       01  NM-TOTAL-TABLE.
           03  NM-TOT-TBL         OCCURS 27 TIMES.
               05  NM-NO          PIC S9(5)    COMP-3.
               05  NM-BAL         PIC S9(9)V99 COMP-3.
               05  NM-INT         PIC S9(7)V99 COMP-3.
               05  NM-OTH         PIC S9(7)V99 COMP-3.
               05  NM-UNE         PIC S9(7)V99 COMP-3.


       01  AGE-DESC-TABLE.
           03  FILLER             PIC X(12) VALUE "    30 DAYS:".
           03  FILLER             PIC X(12) VALUE "    60 DAYS:".
           03  FILLER             PIC X(12) VALUE "    90 DAYS:".
           03  FILLER             PIC X(12) VALUE "   120 DAYS:".
           03  FILLER             PIC X(12) VALUE "   150 DAYS:".
           03  FILLER             PIC X(12) VALUE "   180 DAYS:".
           03  FILLER             PIC X(12) VALUE "   210 +   :".
           03  FILLER             PIC X(12) VALUE "   <M>     :".
           03  FILLER             PIC X(12) VALUE "   <P>     :".
           03  FILLER             PIC X(12) VALUE "   CURRENT :".
       01  AGE-DESC-TBL           REDEFINES AGE-DESC-TABLE.
           03  AGE-DESC           OCCURS 10 TIMES
                                  PIC X(12).

      ***********************************
      *     S C R E E N   F I E L D S   *
      ***********************************
       01  ERRCD                  PIC X.

       01  FORMNO                 PIC 9(4).
       01  INTERIM-COUPON         PIC X.

      ***********************************
      *     P R I N T   B U F F E R S   *
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
           03  FILLER           PIC X(24)    VALUE " ".
           03  FILLER           PIC X(26)    VALUE
           "BULK TRANSFER UPDATE FROM ".
           03  H-FRGR           PIC ZZZ9.
           03  FILLER           PIC X(4)     VALUE
           " TO ".
           03  H-TOBR           PIC ZZZ9.

       01  HEAD3.
           03  FILLER      PIC X(8)     VALUE "BRANCH :".
           03  FILLER      PIC XX       VALUE " ".
           03  H-BRNO      PIC 9999.
           03  H-BRNOX     REDEFINES H-BRNO PIC XXXX.
           03  FILLER      PIC X        VALUE " ".
           03  H-BRDESC    PIC X(20).
           03  FILLER      PIC X(26)    VALUE
           "           TFR TO DEALER: ".
           03  H-TO-DLNO   PIC 9999.99.
           03  FILLER      PIC X(51)    VALUE " ".
           03  FILLER      PIC X(20)    VALUE
           "99.99.99 - LOAN DATE".

       01  HEAD4.
           03  FILLER      PIC X(8)     VALUE "CLASS  :".
           03  FILLER      PIC XX       VALUE " ".
           03  H-LCNO      PIC Z99.
           03  FILLER      PIC X        VALUE " ".
           03  H-LCDESC    PIC X(20).

       01  HEAD5.
           03  FILLER      PIC X(48)    VALUE " ".
           03  HEAD5XX.
               05  FILLER     PIC X(31)    VALUE
               "LAST  REC-    RECENCY ".
               05  FILLER     PIC X(15)    VALUE
               "   CONTRACTUAL".
           03  HEAD5X      REDEFINES HEAD5XX.
               05  FILLER  PIC X(23).
               05  HEAD5T  PIC X(8).
               05  FILLER  PIC X(15).
           03  FILLER      PIC X(38)    VALUE
               "    PAID   -------  BALANCES  -------".

       01  HEAD6.
           03  FILLER      PIC X(39)    VALUE
               "ACCOUNT NO.  DUE  BORROWER".
           03  HEAD6XX     PIC X(45)    VALUE
               "DEALER PAYMENT ENCY    PROFILE   CHARGES    P".
           03  FILLER      REDEFINES HEAD6XX.
               05  FILLER  PIC X(33).
               05  HEAD6T  PIC X(7).
               05  FILLER  PIC X(5).
           03  FILLER      PIC X(48)    VALUE
               "ROFILE   AGE  THRU  OTH-DUE INT/LC-DUE PRINCIPAL".

       01  DETAIL-LINE        PIC X(132) VALUE SPACES.
       01  D-LINE1            REDEFINES DETAIL-LINE.
           03  D-LOAN-NUM     PIC X(13).
           03  FILLER         REDEFINES D-LOAN-NUM.
               05  D-NUMBER   PIC 9(6).
               05  FILLER     PIC X(7).
           03  FILLER         PIC X.
           03  D-DUEDAY       PIC 99.
           03  FILLER         PIC XX.
           03  D-NAME         PIC X(19).
           03  FILLER         PIC X.
           03  D-DLNO         PIC ZZZ9.99.
           03  FILLER         PIC X.
           03  D-PAYDATE      PIC 99/99/99.
           03  FILLER         REDEFINES D-PAYDATE.
               05  FILLER     PIC XX.
               05  D-DASH1    PIC X.
               05  FILLER     PIC XX.
               05  D-DASH2    PIC X.
               05  FILLER     PIC XX.
           03  D-RECENCYX.
               05  D-RECENCY  PIC ZZZZ.
           03  FILLER         PIC X.
           03  D-PYRECPRO     PIC 9(12).
           03  D-XPYRECPRO    REDEFINES D-PYRECPRO
                              PIC X(12).
           03  D-UNEARNED     PIC ZZZZ9.99-.
           03  D-PROFILE      PIC 9(12).
           03  D-XPROFILE     REDEFINES D-PROFILE
                              PIC X(12).
           03  D-CONTUALX.
               05  D-CONTUAL  PIC ZZZZ.
           03  D-PDTHRU       PIC ZZ9999.
           03  D-OTHBAL       PIC ZZZZZ9.99-.
           03  D-INTBAL       PIC ZZZZZ9.99-.
           03  D-CURBAL       PIC ZZZZZ9.99-.
       01  D-LINE-X           REDEFINES DETAIL-LINE.
           03  FILLER         PIC X(18).
           03  D-NAME-X       PIC X(50).

       01  SUMMARY-LINE1 REDEFINES DETAIL-LINE.
           03  S-SUMNAME        PIC X(20).

       01  SUMMARY-LINE2 REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(46).
           03  S-AGETYPE        PIC X(80).

       01  SUMMARY-LINE3 REDEFINES DETAIL-LINE.
           03  S-NAME           PIC X(12).
           03  S-NO             PIC ZZZZ9-.
           03  S-NO-P           PIC ZZ9.99-.
           03  S-OTH            PIC ZZZZZ9.99-.
           03  S-OTH-P          PIC ZZ9.99-.
           03  S-INT            PIC ZZZZZ9.99-.
           03  S-INT-P          PIC ZZ9.99-.
           03  S-BAL            PIC ZZZZZZZ9.99-.
           03  S-BAL-P          PIC ZZ9.99-.
           03  S-UNE            PIC ZZZZZZ9.99-.
           03  S-UNE-P          PIC ZZ9.99-.
           03  S-IER            PIC ZZZZZZ9.99-.
           03  S-IER-P          PIC ZZ9.99-.
           03  S-ERN            PIC ZZZZZZ9.99-.
           03  S-ERN-P          PIC ZZ9.99-.

       01  RANGE-LN REDEFINES DETAIL-LINE.
           03  RANGE-LINE       PIC X(20).
           03  RANGE-SELECTION  PIC X(112).

       01  SUMMARY1.
           03  FILLER           PIC X(15)  VALUE SPACES.
           03  FILLER           PIC X(48)  VALUE
           "NO----%     OTH DUE----%  INT/LC DUE----%".
           03  FILLER           PIC X(17)  VALUE
           "BAL DUE----%   UN".
           03  FILLER           PIC X(51)  VALUE
           "EARN-INT----%    UNEARN-SC----%    UNEARN-MF----%".

       COPY "LIBLP/POSTDATEW.CPY".
       COPY "LIBLP/TFRMEMOW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBCL/CREAIXW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/AGEINGW.CPY".
       COPY "LIBLP/LPUPWKW.CPY".
       COPY "LIBLP/LPDSCLW.CPY".
       COPY "LIBGB/SPPDTHW.CPY".
       COPY "LIBLP/LPPOFFW.CPY".
       COPY "LIBCL/CRNOR2W.CPY".
       COPY "LIBLP/LPPDUEW.CPY".
       COPY "LIBLP/LPEARNW.CPY".
       COPY "LIBLP/LPIBPCW.CPY".
       COPY "LIBLP/LPCEPPW.CPY".
       COPY "LIBLP/LPSTATW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBLP/LPSETTYW.CPY".
       COPY "LIBLP/LPXDTEW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBWI/FDSCANW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBLP/LPSRCDW.CPY".
       COPY "LIBGB/PASSWDW.CPY".

       01  BULKG2-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/BULKG2_DEF.CPY".
       COPY "LIBLP/BULKG2_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      /***************************************
      *                                      *
      *     P R O G R A M   C O N T R O L    *
      *                                      *
      ****************************************
       COPY "LIBGB/GETFMW.CPY".
       01  TRANSFER-BUF.
           03  FR-GR              PIC X(4).
           03  TO-BR              PIC 9999.
           03  TO-MACHINE         PIC XX.
           03  TO-DATA-PATH       PIC X(6).
           03  TO-DLNO            PIC 9(4)V99.

       SCREEN SECTION.
       COPY "LIBLP/BULKG2_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME TRANSFER-BUF EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE  WK-FILE.

       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-TY1-FILE.
           PERFORM CLOSE-TRB1-FILE.
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-NO1-FILE.
           PERFORM CLOSE-LXG1-FILE.
           PERFORM CLOSE-LXE1-FILE.
           PERFORM CLOSE-LX1-FILE.
           PERFORM CLOSE-LTP1-FILE.
           PERFORM CLOSE-LTL1-FILE.
           PERFORM CLOSE-LTI1-FILE.
           PERFORM CLOSE-LTF1-FILE.
           PERFORM CLOSE-LTD1-FILE.
           PERFORM CLOSE-LTC1-FILE.
           PERFORM CLOSE-LTB1-FILE.
           PERFORM CLOSE-LTA1-FILE.
           PERFORM CLOSE-LSX1-FILE.
           PERFORM CLOSE-LSR1-FILE.
           PERFORM CLOSE-LSP1-FILE.
           PERFORM CLOSE-LSM1-FILE.
           PERFORM CLOSE-LSL1-FILE.
           PERFORM CLOSE-LSJ1-FILE.
           PERFORM CLOSE-LSG1-FILE.
           PERFORM CLOSE-LSF1-FILE.
           PERFORM CLOSE-LSD1-FILE.
           PERFORM CLOSE-LP1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-LM1-FILE.
           PERFORM CLOSE-LC1-FILE.
           PERFORM CLOSE-FM1-FILE.
           PERFORM CLOSE-FD1-FILE.
           PERFORM CLOSE-ES1-FILE.
           PERFORM CLOSE-DS1-FILE.
           PERFORM CLOSE-BX1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-BM1-FILE.
           PERFORM CLOSE-RC1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-GB-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE WK-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      *************************************
       MAIN-MODULE SECTION.
      *************************************

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE "BULKG2" TO VDU-FORM-NAME FORM-PROG.
           MOVE FORM-PATHNAME TO FORM-PATH.

      * MADE FLOATING SO IT CAN RETURN TO BULKTR STILL FILLED OUT
           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS BULKG2-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           PERFORM OPEN-GB-FILE.
           MOVE 1             TO GB-NO.
           PERFORM READ-GB-FILE.
           PERFORM CLOSE-GB-FILE.

           PERFORM GET-GPENV.

           PERFORM INITIALIZATION.
           IF ERRCD NOT = " "
              GO TO END-PROGRAM.

           PERFORM BULK-TRANSFER.

           IF FIRST-TIME = "N"
              PERFORM CLASS-TOTALS
              PERFORM COMPANY-TOTALS.
           PERFORM PRINT-RANGE-LINES.

           GO TO END-PROGRAM.


      ******************************************
       BULK-TRANSFER SECTION.
      ******************************************
           PERFORM OPEN-WK-FILE.

           MOVE 0 TO WK-BRNO
                     WK-CLASS
                     WK-ACCTNO.
       BULK-TRANSFER-START.
           MOVE "D" TO WK-TYPE.
           PERFORM START-WK-FILE.
       BULK-TRANSFER-NEXT.
           PERFORM READ-WK-FILE-NEXT.
           IF IO-BAD
              PERFORM CLOSE-WK-FILE
              MOVE BULK-PATH TO ACCESS-BUF
              PERFORM REMOVE-WORKFILE

              PERFORM REMOVE-UPDATE-TAGS
              IF NOT (PRU-LP = "VDU" OR "PRU")
                 MOVE "BULK TRANSFER IS COMPLETE!!!!" TO MESS
                 PERFORM SEND-MESS
                 GO TO BULK-TRANSFER-EXIT
              ELSE
                 GO TO BULK-TRANSFER-EXIT.

           IF WK-BRNO NOT = FROM-BRANCH
              MOVE WK-BRNO TO BR-NO FROM-BRANCH
              PERFORM READ-BR-FILE
              MOVE BR-MACHINE TO FROM-MACHINE
              MOVE BR-DATA-PATH TO FROM-DATA-PATH.

      * LT-PATH MUST BE SET FOR PAYMENT SCHEDULES, COMPUTE PAID THRU

           MOVE EXT-FILPATH-BASE           TO LN-ALL-BASE
                                              LTA-ALL-BASE
                                              LTB-ALL-BASE
                                              LTC-ALL-BASE
                                              LTD-ALL-BASE
                                              LTF-ALL-BASE
                                              LTI-ALL-BASE
                                              LTL-ALL-BASE
                                              LTP-ALL-BASE.
           MOVE FROM-MACHINE               TO LN-ALL-MACHINE
                                              LTA-ALL-MACHINE
                                              LTB-ALL-MACHINE
                                              LTC-ALL-MACHINE
                                              LTD-ALL-MACHINE
                                              LTF-ALL-MACHINE
                                              LTI-ALL-MACHINE
                                              LTL-ALL-MACHINE
                                              LTP-ALL-MACHINE.
           MOVE FROM-DATA-PATH             TO LN-ALL-DATA
                                              LTA-ALL-DATA
                                              LTB-ALL-DATA
                                              LTC-ALL-DATA
                                              LTD-ALL-DATA
                                              LTF-ALL-DATA
                                              LTI-ALL-DATA
                                              LTL-ALL-DATA
                                              LTP-ALL-DATA.
           MOVE LN-ALL-PATH                TO LN-PATH.
           MOVE LTA-ALL-PATH               TO LTA-PATH.
           MOVE LTB-ALL-PATH               TO LTB-PATH.
           MOVE LTC-ALL-PATH               TO LTC-PATH.
           MOVE LTD-ALL-PATH               TO LTD-PATH.
           MOVE LTF-ALL-PATH               TO LTF-PATH.
           MOVE LTI-ALL-PATH               TO LTI-PATH.
           MOVE LTL-ALL-PATH               TO LTL-PATH.
           MOVE LTP-ALL-PATH               TO LTP-PATH.

           MOVE "Y"                        TO LN-PATH-OVERRIDE
                                              LTA-PATH-OVERRIDE
                                              LTB-PATH-OVERRIDE
                                              LTC-PATH-OVERRIDE
                                              LTD-PATH-OVERRIDE
                                              LTF-PATH-OVERRIDE
                                              LTI-PATH-OVERRIDE
                                              LTL-PATH-OVERRIDE
                                              LTP-PATH-OVERRIDE.

           PERFORM OPEN-LN1-FILE.
           MOVE WK-BRNO   TO LN-OWNBR.
           MOVE WK-ACCTNO TO LN-ACCTNO.
           PERFORM READ-LN1-FILE.
           PERFORM CLOSE-LN1-FILE.
           IF IO-BAD
              MOVE EXCEPTION-MISSING TO DETAIL-LINE
              PERFORM EXCEPTION-ROUTINE
              GO TO BULK-TRANSFER-NEXT.

           IF LN-POCD-VOID
              MOVE EXCEPTION-VOID TO DETAIL-LINE
              PERFORM EXCEPTION-ROUTINE
              GO TO BULK-TRANSFER-NEXT.
           IF LN-POCD-INRESCISSION OR LN-POCD-RESCIND OR LN-POCD-PB-RESC
              MOVE EXCEPTION-RESC TO DETAIL-LINE
              PERFORM EXCEPTION-ROUTINE
              GO TO BULK-TRANSFER-NEXT.

           IF LN-POCD = "BT"
              MOVE EXCEPTION-BT TO DETAIL-LINE
              PERFORM EXCEPTION-ROUTINE
              GO TO BULK-TRANSFER-NEXT.

      * TEST FOR OPTION OF ZERO BALANCE ONLY:

           IF ZBAL = "O"
              IF LN-CURBAL NOT = 0
                 MOVE EXCEPTION-NOT-ZERO TO DETAIL-LINE
                 PERFORM EXCEPTION-ROUTINE
                 GO TO BULK-TRANSFER-NEXT
              END-IF
           ELSE
      * TEST FOR OPTION OF NO ZERO BALANCE ACCOUNTS:
           IF ZBAL = "S"
              IF LN-CURBAL = 0
                 MOVE EXCEPTION-ZERO TO DETAIL-LINE
                 PERFORM EXCEPTION-ROUTINE
                 GO TO BULK-TRANSFER-NEXT.

      * TEST FOR OPTION OF P&L ONLY:

           IF PL-ONLY = "O"
              IF LN-PLCD = " " OR LN-PLDATE = 0
                 MOVE EXCEPTION-NOT-PL TO DETAIL-LINE
                 PERFORM EXCEPTION-ROUTINE
                 GO TO BULK-TRANSFER-NEXT
              END-IF
           ELSE
      * TEST FOR OPTION OF NO P&L ACCOUNTS:
           IF PL-ONLY = "S"
              IF (LN-PLCD NOT = " ") OR (LN-PLDATE NOT = 0)
                 MOVE EXCEPTION-PL TO DETAIL-LINE
                 PERFORM EXCEPTION-ROUTINE
                 GO TO BULK-TRANSFER-NEXT.

           IF ONHAND = "O"
              IF LN-REPOCD NOT = "X"
                 MOVE EXCEPTION-REPO TO DETAIL-LINE
                 PERFORM EXCEPTION-ROUTINE
                 GO TO BULK-TRANSFER-NEXT.

           IF SOLD = "O"
              IF LN-REPOCD NOT = "S"
                 MOVE EXCEPTION-REPO TO DETAIL-LINE
                 PERFORM EXCEPTION-ROUTINE
                 GO TO BULK-TRANSFER-NEXT.

           IF REDEEM = "O"
              IF LN-REPOCD NOT = "V"
                 MOVE EXCEPTION-REPO TO DETAIL-LINE
                 PERFORM EXCEPTION-ROUTINE
                 GO TO BULK-TRANSFER-NEXT.

           IF ONHAND = "S"
              IF LN-REPOCD = "X"
                 MOVE EXCEPTION-REPO TO DETAIL-LINE
                 PERFORM EXCEPTION-ROUTINE
                 GO TO BULK-TRANSFER-NEXT.

           IF SOLD = "S"
              IF LN-REPOCD = "S"
                 MOVE EXCEPTION-REPO TO DETAIL-LINE
                 PERFORM EXCEPTION-ROUTINE
                 GO TO BULK-TRANSFER-NEXT.

           IF REDEEM = "S"
              IF LN-REPOCD = "V"
                 MOVE EXCEPTION-REPO TO DETAIL-LINE
                 PERFORM EXCEPTION-ROUTINE
                 GO TO BULK-TRANSFER-NEXT.

           IF NOT (PRU-LP = "VDU" OR "PRU")
              MOVE " " TO MESS
              STRING "PROGRAM IN PROGRESS..." LN-OWNBR " " PD-ACCTNO
                      DELIMITED BY SIZE INTO MESS
              PERFORM SEND-LEGEND.

      * SAVE UNEARNED LOAN AMOUNTS:

           PERFORM COMPUTE-EARNINGS.
           MOVE EARN-UNEARN(1) TO SAVE-UNEARN(1).
           MOVE EARN-UNEARN(2) TO SAVE-UNEARN(2).
           MOVE EARN-UNEARN(3) TO SAVE-UNEARN(3).
           MOVE EARN-UNEARN(4) TO SAVE-UNEARN(4).
           MOVE EARN-UNEARN(5) TO SAVE-UNEARN(5).
           MOVE EARN-UNEARN(6) TO SAVE-UNEARN(6).

      * INSURE ALL SOC-SEC-NO'S IN LOAN RECORD ARE UNIQUE

           PERFORM VARYING SUBX FROM 1 BY 1 UNTIL SUBX > 4
              ADD 1 TO SUBX GIVING SUBY
              PERFORM VARYING SUB FROM SUBY BY 1 UNTIL SUB > 4
                 IF (LN-SSNO(SUB) NOT = 0)
                          AND (LN-SSNO(SUB) = LN-SSNO(SUBX))
                    MOVE 0 TO LN-SSNO(SUB)
                    MOVE " " TO LN-MAKERCD(SUB)
                 END-IF
              END-PERFORM
           END-PERFORM.

      * INSURE NO EMPTY SOC-SEC-NO SLOTS

           PERFORM 2 TIMES
              IF (LN-SSNO(2) = 0)
                       AND ( (LN-SSNO(3) NOT = 0)
                                    OR (LN-SSNO(4) NOT = 0)
                           )
                 MOVE LN-SSNO(3) TO LN-SSNO(2)
                 MOVE LN-MAKERCD(3) TO LN-MAKERCD(2)
                 MOVE LN-SSNO(4) TO LN-SSNO(3)
                 MOVE LN-MAKERCD(4) TO LN-MAKERCD(3)
                 MOVE 0 TO LN-SSNO(4)
                 MOVE " " TO LN-MAKERCD(4)
              END-IF
           END-PERFORM.

           IF LN-SSNO(3) = 0 AND LN-SSNO(4) NOT = 0
              MOVE LN-SSNO(4) TO LN-SSNO(3)
              MOVE LN-MAKERCD(4) TO LN-MAKERCD(3)
              MOVE 0 TO LN-SSNO(4)
              MOVE " " TO LN-MAKERCD(4).

      * STORE PICTURE OF LOAN RECORD IN SCRATCH:
           MOVE LN-REC TO PD-RECORD.

      * ESTABLISH CONTRACTUAL/RECENCY FOR UPDATE OF TY & DS
           MOVE LN-ORGST    TO SP-ORGST.
           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           MOVE LN-LAWCODE  TO SP-LAWCODE.
           IF LPWSSP-SP1-KEY NOT = SP1-KEY
              MOVE SP1-KEY TO LPWSSP-SP1-KEY
              PERFORM READ-SP1-FILE
              IF NOT IO-GOOD
                 GO TO IO-ERROR.

           PERFORM ESTABLISH-ORIG-TY-DS-NUMBERS.

      * GET PAID THRU AND IBPC-FG
           MOVE LN-1STPYDATE TO PDTH-DATE-WORK.
           PERFORM PAID-THRU-CALCULATION.
           MOVE PDTH-DATE-FULL TO HOLD-LP-PDTH-DATE.
           MOVE LN-1STPYDATE TO IBPC-DATE.
           PERFORM IBPC-TEST.
           MOVE IBPC-FG TO HOLD-LP-IBPC.

      * TRANSFER LOAN BORROWERS TO THE TO->BW-FILE,
      * AND BORROWERS MEMO TO THE TO->BM-FILE:

           MOVE EXT-FILPATH-BASE           TO BM-ALL-BASE.
           MOVE FROM-MACHINE               TO BM-ALL-MACHINE.
           MOVE FROM-DATA-PATH             TO BM-ALL-DATA.
           MOVE BM-ALL-PATH                TO BM-PATH.
           MOVE "Y"                        TO BM-PATH-OVERRIDE.

           MOVE 1 TO BW-SUB.
       BULK-TRANSFER-BORROWERS.
           IF PD-SSNO(BW-SUB) NOT = 0
              PERFORM TRANSFER-BW-REC
              IF ERR-FG NOT = " "
                 MOVE "ERROR IN TRANSFER-BW" TO MESS
                 PERFORM SEND-MESS
                 GO TO BULK-TRANSFER-ERR-EXIT
              ELSE
                 PERFORM TRANSFER-BX-REC
                 PERFORM TRANSFER-BM-REC
                 IF ERR-FG NOT = " "
                    MOVE "ERROR IN TRANSFER-BX OR BM" TO MESS
                    PERFORM SEND-MESS
                    GO TO BULK-TRANSFER-ERR-EXIT
                 ELSE
                    PERFORM CREATE-TOBM-FILE
                    IF ERR-FG NOT = " "
                       MOVE "ERROR IN CREATE TOBM" TO MESS
                       PERFORM SEND-MESS
                       GO TO BULK-TRANSFER-ERR-EXIT.
           IF BW-SUB < 4
              ADD 1 TO BW-SUB
              GO TO BULK-TRANSFER-BORROWERS.

      *----------------------------------------------------
      * TRANSFER LOAN TO THE TO->LN-FILE:
      *----------------------------------------------------
           MOVE PD-ACCTNO TO TO-LN-ACCTNO.
           PERFORM TRANSFER-LN-REC.

      *--------------------------------------------------
      * TRANSFER ESCROW (ES) TO-> ES-FILE:
      *--------------------------------------------------
           IF LN-ESCROW-FG = "Y" OR LN-ESCROW-FG = "I"
               PERFORM TRANSFER-ES-REC
               IF ERR-FG NOT = " "
                  MOVE "ERROR IN TRANSFER-ES" TO MESS
                  PERFORM SEND-MESS
                  GO TO BULK-TRANSFER-ERR-EXIT.

      *----------------------------------------------------
      * TRANSFER PAYMENT RECORDS TO THE TO->LP-FILE:
      *----------------------------------------------------
           PERFORM TRANSFER-LP-REC.
           IF ERR-FG NOT = " "
              MOVE "ERROR IN TRANSFER-LP" TO MESS
              PERFORM SEND-MESS
              GO TO BULK-TRANSFER-ERR-EXIT.

      *----------------------------------------------------
      * TRANSFER PAYMENT TRAILERS TO THE TO->LX-FILE:
      *----------------------------------------------------
           PERFORM TRANSFER-LX-REC.
           IF ERR-FG NOT = " "
              MOVE "ERROR IN TRANSFER-LX" TO MESS
              PERFORM SEND-MESS
              GO TO BULK-TRANSFER-ERR-EXIT.
           PERFORM TRANSFER-LXG-REC.
           IF ERR-FG NOT = " "
              MOVE "ERROR IN TRANSFER-LX" TO MESS
              PERFORM SEND-MESS
              GO TO BULK-TRANSFER-ERR-EXIT.
           PERFORM TRANSFER-LXE-REC.
           IF ERR-FG NOT = " "
              MOVE "ERROR IN TRANSFER-LX" TO MESS
              PERFORM SEND-MESS
              GO TO BULK-TRANSFER-ERR-EXIT.

      *----------------------------------------------------
      * TRANSFER LOAN TRAILERS TO THE TO->LT-FILE:
      *----------------------------------------------------
           PERFORM TRANSFER-LTA-RECORDS.
           IF ERR-FG NOT = " "
              MOVE "ERROR IN TRANSFER-LT-A" TO MESS
              PERFORM SEND-MESS
              GO TO BULK-TRANSFER-ERR-EXIT.

           PERFORM TRANSFER-LTB-RECORDS.
           IF ERR-FG NOT = " "
              MOVE "ERROR IN TRANSFER-LT-B" TO MESS
              PERFORM SEND-MESS
              GO TO BULK-TRANSFER-ERR-EXIT.

           PERFORM TRANSFER-LTC-RECORDS.
           IF ERR-FG NOT = " "
              MOVE "ERROR IN TRANSFER-LT-C" TO MESS
              PERFORM SEND-MESS
              GO TO BULK-TRANSFER-ERR-EXIT.

      * DEBIT CARD PROCEEDS
           PERFORM TRANSFER-LTD-RECORDS.
           IF ERR-FG NOT = " "
              MOVE "ERROR IN TRANSFER-LT-D" TO MESS
              PERFORM SEND-MESS
              GO TO BULK-TRANSFER-ERR-EXIT.

           PERFORM TRANSFER-LTF-RECORDS.
           IF ERR-FG NOT = " "
              MOVE "ERROR IN TRANSFER-LT-F" TO MESS
              PERFORM SEND-MESS
              GO TO BULK-TRANSFER-ERR-EXIT.

           PERFORM TRANSFER-LTI-RECORDS.
           IF ERR-FG NOT = " "
              MOVE "ERROR IN TRANSFER-LT-I" TO MESS
              PERFORM SEND-MESS
              GO TO BULK-TRANSFER-ERR-EXIT.

           PERFORM TRANSFER-LTL-RECORDS.
           IF ERR-FG NOT = " "
              MOVE "ERROR IN TRANSFER-LT-L" TO MESS
              PERFORM SEND-MESS
              GO TO BULK-TRANSFER-ERR-EXIT.

           PERFORM TRANSFER-LTP-RECORDS.
           IF ERR-FG NOT = " "
              MOVE "ERROR IN TRANSFER-LT-P" TO MESS
              PERFORM SEND-MESS
              GO TO BULK-TRANSFER-ERR-EXIT.

      *----------------------------------------------------
      * TRANSFER LOAN TRAILERS TO THE TO->LS-FILE:
      *----------------------------------------------------
           PERFORM TRANSFER-LSD-REC.
           IF ERR-FG NOT = " "
              MOVE "ERROR IN TRANSFER-LSD" TO MESS
              PERFORM SEND-MESS
              GO TO BULK-TRANSFER-ERR-EXIT.

           PERFORM TRANSFER-LSF-REC.
           IF ERR-FG NOT = " "
              MOVE "ERROR IN TRANSFER-LSF" TO MESS
              PERFORM SEND-MESS
              GO TO BULK-TRANSFER-ERR-EXIT.

           PERFORM TRANSFER-LSG-REC.
           IF ERR-FG NOT = " "
              MOVE "ERROR IN TRANSFER-LSG" TO MESS
              PERFORM SEND-MESS
              GO TO BULK-TRANSFER-ERR-EXIT.

           PERFORM TRANSFER-LSJ-REC.
           IF ERR-FG NOT = " "
              MOVE "ERROR IN TRANSFER-LSJ" TO MESS
              PERFORM SEND-MESS
              GO TO BULK-TRANSFER-ERR-EXIT.

           PERFORM TRANSFER-LSL-REC.
           IF ERR-FG NOT = " "
              MOVE "ERROR IN TRANSFER-LSL" TO MESS
              PERFORM SEND-MESS
              GO TO BULK-TRANSFER-ERR-EXIT.

           PERFORM TRANSFER-LSM-REC.
           IF ERR-FG NOT = " "
              MOVE "ERROR IN TRANSFER-LSM" TO MESS
              PERFORM SEND-MESS
              GO TO BULK-TRANSFER-ERR-EXIT.

           PERFORM TRANSFER-LSP-REC.
           IF ERR-FG NOT = " "
              MOVE "ERROR IN TRANSFER-LSP" TO MESS
              PERFORM SEND-MESS
              GO TO BULK-TRANSFER-ERR-EXIT.

           PERFORM TRANSFER-LSR-REC.
           IF ERR-FG NOT = " "
              MOVE "ERROR IN TRANSFER-LSR" TO MESS
              PERFORM SEND-MESS
              GO TO BULK-TRANSFER-ERR-EXIT.

           PERFORM TRANSFER-LSX-REC.
           IF ERR-FG NOT = " "
              MOVE "ERROR IN TRANSFER-LSX" TO MESS
              PERFORM SEND-MESS
              GO TO BULK-TRANSFER-ERR-EXIT.

           PERFORM TRANSFER-NO-REC.
           IF ERR-FG NOT = " "
              MOVE "ERROR IN TRANSFER-NO" TO MESS
              PERFORM SEND-MESS
              GO TO BULK-TRANSFER-ERR-EXIT.

      *----------------------------------------------------
      * UPDATE NEW TRANSFER FROM FIELDS IN THE TO-BRANCH FOR METRO2
      *  THIS MUST FOLLOW TRANSFER OF LS-RECS OR WRITE ERROR WILL OCCCUR
      *  IN THE TRANSFER-LS-RECS IF IT ALREADY HAD A "M" RECORDS THAT
      *  TRIES TO TRANSFER
      *----------------------------------------------------
           PERFORM UPDATE-TOLS-MISC.

      *----------------------------------------------------
      * TRANSFER LOAN MEMO TO THE TO->LM-FILE:
      *----------------------------------------------------
           PERFORM TRANSFER-LM-REC.
           IF ERR-FG NOT = " "
              MOVE "ERROR IN TRANSFER-LM" TO MESS
              PERFORM SEND-MESS
              GO TO BULK-TRANSFER-ERR-EXIT.


      * AT THIS POINT, LN-REC HAS THE NEW LOAN IN IT WHICH MAY OR MAY
      * NOT HAVE A NEW ACCTNO ! THIS IS IMPORTANT THAT THIS STAY HERE
      * BECAUSE CREATE NOTICE MUST HAVE NEW LOAN SO THERE IT HAS
      * THE CORRECT AMOUNTS AND A BALANCE !

           IF FORMNO NOT = 0
              PERFORM CREATE-FM-REC.

      *----------------------------------------------------
      * CLOSE OUT LOAN IN THE FROM->LN-FILE:
      *----------------------------------------------------
           PERFORM CLOSE-FRLN-FILE.

      *----------------------------------------------------
      * CLOSE OUT BORROWERS IN THE FROM->BW-FILE:
      *----------------------------------------------------
           MOVE 1 TO BW-SUB.
       BULK-CLOSE-BORROWERS.
           IF PD-SSNO(BW-SUB) NOT = 0
              PERFORM CLOSE-FRBW-FILE
              IF ERR-FG NOT = " "
                 MOVE "ERROR IN AFTER CLOSE-FRBW" TO MESS
                 PERFORM SEND-MESS
                 GO TO BULK-TRANSFER-ERR-EXIT
              ELSE
                 PERFORM CREATE-FRBM-FILE.
           IF BW-SUB < 4
              ADD 1 TO BW-SUB
              GO TO BULK-CLOSE-BORROWERS.

      *----------------------------------------------------
      * CREATE PAYMENT RECORD IN THE FROM->LP-FILE:
      *----------------------------------------------------
           PERFORM CREATE-FRLP-FILE.

      *----------------------------------------------------
      * CREATE LOAN MEMO RECORD IN THE FROM->LM-FILE:
      *----------------------------------------------------
           PERFORM CREATE-FRLM-FILE.

      *----------------------------------------------------
      * CREATE TRANSACTION RECORD IN THE FROM->TRB-FILE:
      *----------------------------------------------------
           PERFORM CREATE-FRTR-FILE.
      *----------------------------------------------------
      * CREATE THE ACCRUAL EARNINGS RECORD FOR THE LAST MONTH
      * IB ACCRUAL AMOUNT IN THE FROM BRANCH.
      *-----------------------------------------------------
      *    IF GP-USE-ACCRUAL-EARN-FILE = "Y" OR "R"
      *       MOVE EXT-FILPATH-BASE        TO AE-ALL-BASE
      *       MOVE FROM-DATA-PATH          TO AE-ALL-DATA
      *       MOVE FROM-MACHINE            TO AE-ALL-MACHINE
      *       MOVE AE-ALL-PATH             TO AE-PATH
      *       MOVE "Y"                     TO AE-PATH-OVERRIDE
      *       PERFORM UPDATE-FRAE-FILE.

      *----------------------------------------------------
      * UPDATE DEALER STATISTICS IN THE FROM->DS-FILE
      *----------------------------------------------------
           PERFORM UPDATE-FRDS-FILE.

      *----------------------------------------------------
      * UPDATE TODAYS TOTALS IN THE FROM->TY-FILE
      *----------------------------------------------------
           PERFORM UPDATE-FRTY-FILE.

      *----------------------------------------------------
      * CREATE LOAN MEMO RECORD IN THE TO->LM-FILE:
      *----------------------------------------------------
           PERFORM CREATE-TOLM-FILE.

      *----------------------------------------------------
      * CREATE TRANSACTION RECORD IN THE TO->TRB-FILE:
      *----------------------------------------------------
           PERFORM CREATE-TOTR-FILE.

      *----------------------------------------------------
      * UPDATE DEALER STATISTICS IN THE TO->DS-FILE
      *----------------------------------------------------
           PERFORM UPDATE-TODS-FILE.

      *----------------------------------------------------
      * UPDATE TODAYS TOTALS IN THE TO->TY-FILE
      *----------------------------------------------------
           PERFORM UPDATE-TOTY-FILE.

      *----------------------------------------------------
      * UPDATE NOTICE RECORD IN FROM BRANCH, AS IT IS NOW PAIDOUT
      *----------------------------------------------------
           PERFORM UPDATE-FRNO-FILE.

      *----------------------------------------------------
      * REMOVE WK RECORD FOR THIS ACCOUNT
      *----------------------------------------------------
           PERFORM DELETE-WK-FILE.

      *----------------------------------------------------
      * REDUCE HEADER RECORD WORK FILE:
      *----------------------------------------------------
           MOVE " " TO WK-TYPE.
           MOVE 0 TO WK-BRNO
                     WK-CLASS
                     WK-ACCTNO.
           PERFORM START-WK-FILE.
           PERFORM READ-WK-FILE-NEXT.
           IF (IO-BAD) OR (WK-TYPE NOT = " ")
              GO TO IO-ERROR.

           SUBTRACT 1 FROM WK-NOSEL.
           SUBTRACT PD-CURBAL FROM WK-AMTSEL.
           PERFORM REWRITE-WK-FILE.

           IF NOT (PRU-LP = "VDU" OR "PRU")
              MOVE "SNNN060NOTFR." TO VDU
              MOVE WK-NOSEL TO VDUNUM0
              PERFORM SCREENIO
              MOVE "SNNN072AMTTFR." TO VDU
              MOVE WK-AMTSEL TO VDUNUM2
              PERFORM SCREENIO.

           IF FIRST-TIME = "Y"
              MOVE "Y" TO RECORDS-FOUND
              MOVE LN-OWNBR TO HOLD-LN-OWNBR
              MOVE LN-CLASS TO HOLD-LN-CLASS
              MOVE 0 TO LC-CODE
              PERFORM SET-CLASS-HEADINGS
              PERFORM SET-BRANCH-HEADINGS.
           IF LN-CLASS NOT = HOLD-LN-CLASS
              PERFORM CLASS-TOTALS
              MOVE LN-CLASS TO HOLD-LN-CLASS
              PERFORM SET-CLASS-HEADINGS.


      * PUT "FROM" ORIGINAL BACK INTO LN-REC BUT WITH NEW ACCTNO
           MOVE PD-RECORD TO LN-REC.
           MOVE TO-LN-ACCTNO TO LN-ACCTNO.

           PERFORM DETAIL-LINE-PROCESS.

           ADD 1 PD-ACCTNO GIVING WK-ACCTNO.
           GO TO BULK-TRANSFER-START.

       BULK-TRANSFER-ERR-EXIT.
           IF NOT (PRU-LP = "VDU" OR "PRU")
              MOVE " " TO MESS
              MOVE "AN ERROR HAS OCCURED, DURING THE TRANSFER!!"
                                                       TO MESS
              PERFORM SEND-MESS
              MOVE "MAKE NOTE OF THE ACCOUNT NO. !!" TO MESS
              PERFORM SEND-MESS.
       BULK-TRANSFER-EXIT.
           EXIT.

      ********************************************
      *    TRANSFER LOAN RECORD TO TO->LN-FILE
      ********************************************
       TRANSFER-LN-REC SECTION.
           MOVE EXT-FILPATH-BASE           TO LN-ALL-BASE.
           MOVE TO-DATA-PATH               TO LN-ALL-DATA.
           MOVE TO-MACHINE                 TO LN-ALL-MACHINE.
           MOVE LN-ALL-PATH                TO LN-PATH.
           MOVE "Y"                        TO LN-PATH-OVERRIDE.


      * DETERMINE IF FROM->LOAN NUMBER EXISTS IN TO->LOAN FILE:
           PERFORM OPEN-LN1-FILE.
           MOVE TO-BR        TO LN-OWNBR.
           MOVE TO-LN-ACCTNO TO LN-ACCTNO.
           PERFORM READ-LN1-FILE.
           IF IO-BAD
              MOVE PD-RECORD TO LN-REC
              MOVE TO-LN-ACCTNO TO LN-ACCTNO
              MOVE TO-BR TO LN-OWNBR
              IF TO-DLNO NOT = 0
                 MOVE TO-DLNO TO LN-DLNO
              END-IF
              MOVE TO-STATE TO LN-OWNST
              MOVE "Y" TO LN-BR-TFR-FG
              PERFORM SET-TOBR-STATUS-FG
              PERFORM SET-TOBR-CONPROFILE
              PERFORM WRITE-LN1-FILE
              GO TO TRANSFER-LN-REC-EXIT.
      *    ELSE
      *       UNLOCK LN-FILE.

      * ASSIGN NEW LOAN NUMBER:
       TRANSFER-LN-REC-ASSIGN-NO.
           MOVE EXT-FILPATH-BASE           TO GB-ALL-BASE.
           MOVE TO-DATA-PATH               TO GB-ALL-DATA.
           MOVE TO-MACHINE                 TO GB-ALL-MACHINE.
           MOVE GB-ALL-PATH                TO GB-PATH.
           MOVE "Y"                        TO GB-PATH-OVERRIDE.

           PERFORM OPEN-GB-FILE.
           MOVE 1             TO GB-NO.
           PERFORM READ-GB-FILE.
           IF IO-BAD
              PERFORM IO-ERROR
              GO TO END-PROGRAM.

           IF (LN-SALEFIN = "Y") AND (GB-NEXT-SF < 989999)
      * DO NOT ALLOW 0 ACCOUNT NUMBER
              IF GB-NEXT-SF = 0
                 ADD 1 TO GB-NEXT-SF
              END-IF
              MOVE GB-NEXT-SF TO TO-LN-ACCTNO
              ADD 1 TO GB-NEXT-SF
              PERFORM TRANSFER-LN-REC-UPD
              IF RTN-FG = "N"
                 GO TO TRANSFER-LN-REC-ASSIGN-NO
              ELSE
                 GO TO TRANSFER-LN-REC-EXIT.

           IF LN-SALEFIN NOT = "Y"
              IF LN-SMALLOAN = "Y"
                        AND GB-NEXT-SMALL < 989999
      * DO NOT ALLOW 0 ACCOUNT NUMBER
                 IF GB-NEXT-SMALL = 0
                    ADD 1 TO GB-NEXT-SMALL
                 END-IF
                 MOVE GB-NEXT-SMALL TO TO-LN-ACCTNO
                 ADD 1 TO GB-NEXT-SMALL
                 PERFORM TRANSFER-LN-REC-UPD
                 IF RTN-FG = "N"
                    GO TO TRANSFER-LN-REC-ASSIGN-NO
                 ELSE
                    GO TO TRANSFER-LN-REC-EXIT
              ELSE
                 IF LN-SMALLOAN = "N"
                           AND GB-NEXT-OTHER < 989999
      * DO NOT ALLOW 0 ACCOUNT NUMBER
                    IF GB-NEXT-OTHER = 0
                       ADD 1 TO GB-NEXT-OTHER
                    END-IF
                    MOVE GB-NEXT-OTHER TO TO-LN-ACCTNO
                    ADD 1 TO GB-NEXT-OTHER
                    PERFORM TRANSFER-LN-REC-UPD
                    IF RTN-FG = "N"
                       GO TO TRANSFER-LN-REC-ASSIGN-NO
                    ELSE
                       GO TO TRANSFER-LN-REC-EXIT.

           IF NOT (PRU-LP = "VDU" OR "PRU")
              MOVE "GLOBAL LOAN COUNTER NOT < 990000 !" TO MESS
              PERFORM SEND-MESS.
           GO TO IO-ERROR.

       TRANSFER-LN-REC-EXIT.
           PERFORM CLOSE-LN1-FILE.

      *********************************************
       TRANSFER-LN-REC-UPD SECTION.
      *********************************************
           PERFORM REWRITE-GB-FILE.
           PERFORM CLOSE-GB-FILE.

           MOVE TO-BR        TO LN-OWNBR.
           MOVE TO-LN-ACCTNO TO LN-ACCTNO.
           PERFORM READ-LN1-FILE.
           IF IO-GOOD
      *       UNLOCK LN-FILE
              MOVE "N" TO RTN-FG
           ELSE
              MOVE PD-RECORD TO LN-REC
              MOVE TO-LN-ACCTNO TO LN-ACCTNO
              MOVE PD-ACCTNO TO LN-OLDACCTNO
              IF TO-DLNO NOT = 0
                 MOVE TO-DLNO TO LN-DLNO
              END-IF
              MOVE TO-BR TO LN-OWNBR
              MOVE TO-STATE TO LN-OWNST
              MOVE "Y" TO LN-BR-TFR-FG
              PERFORM SET-TOBR-STATUS-FG
              PERFORM SET-TOBR-CONPROFILE
              PERFORM WRITE-LN1-FILE
              MOVE " " TO RTN-FG.

       EXIT-TRANSFER-LN-REC-UPD.
           EXIT.


      *********************************************
       TRANSFER-ES-REC SECTION.
      *********************************************
           MOVE " " TO ERR-FG.

           MOVE EXT-FILPATH-BASE           TO ES-ALL-BASE.
           MOVE FROM-DATA-PATH             TO ES-ALL-DATA.
           MOVE FROM-MACHINE               TO ES-ALL-MACHINE.
           MOVE ES-ALL-PATH                TO ES-PATH.
           MOVE "Y"                        TO ES-PATH-OVERRIDE.

           PERFORM OPEN-ES1-FILE.
           MOVE ES-PATH-OWNBR TO ES-BRNO.
           MOVE PD-ACCTNO TO ES-ACCTNO.
           PERFORM READ-ES1-FILE.
           PERFORM CLOSE-ES1-FILE.
           IF IO-BAD
              GO TO TRANSFER-ES-REC-EXIT.

           MOVE EXT-FILPATH-BASE           TO ES-ALL-BASE.
           MOVE TO-DATA-PATH               TO ES-ALL-DATA.
           MOVE TO-MACHINE                 TO ES-ALL-MACHINE.
           MOVE ES-ALL-PATH                TO ES-PATH.
           MOVE "Y"                        TO ES-PATH-OVERRIDE.
           PERFORM OPEN-ES1-FILE.

           MOVE TO-BR        TO ES-BRNO.
           MOVE TO-LN-ACCTNO TO ES-ACCTNO.
           PERFORM WRITE-ES1-FILE.
           IF IO-BAD
               GO TO IO-ERROR.

           PERFORM CLOSE-ES1-FILE.

       TRANSFER-ES-REC-EXIT.
           EXIT.

      *************************************************
       TRANSFER-LP-REC SECTION.
      *************************************************
           MOVE " " TO ERR-FG.
           MOVE EXT-FILPATH-BASE           TO LP-ALL-BASE.
           MOVE FROM-DATA-PATH             TO LP-ALL-DATA.
           MOVE FROM-MACHINE               TO LP-ALL-MACHINE.
           MOVE LP-ALL-PATH                TO LP-PATH.
           MOVE "Y"                        TO LP-PATH-OVERRIDE.

           MOVE 0 TO SAVE-LP-SEQNO.

       TRANSFER-LP-REC-NEXT.
      * MUST SET PATH EVERYTIME BECAUSE IT SWITCHES 'FROM' AND 'TO'
           MOVE EXT-FILPATH-BASE           TO LP-ALL-BASE.
           MOVE FROM-DATA-PATH             TO LP-ALL-DATA.
           MOVE FROM-MACHINE               TO LP-ALL-MACHINE.
           MOVE LP-ALL-PATH                TO LP-PATH.
           MOVE "Y"                        TO LP-PATH-OVERRIDE.

           PERFORM OPEN-LP1-FILE.


           MOVE LP-PATH-OWNBR TO LP-BRNO
                                 QLP1-WBEG-BRNO
                                 QLP1-WEND-BRNO.
           MOVE PD-ACCTNO     TO LP-ACCTNO
                                 QLP1-WBEG-ACCTNO
                                 QLP1-WEND-ACCTNO.
           MOVE SAVE-LP-SEQNO TO LP-SEQNO
                                 QLP1-WBEG-SEQNO.
           MOVE ALL "9"       TO QLP1-WEND-SEQNO.

           PERFORM START-LP1-FILE.

           PERFORM READ-LP1-FILE-NEXT.
           IF IO-BAD OR (LP-BRNO NOT = LP-PATH-OWNBR)
                      OR (LP-ACCTNO NOT = PD-ACCTNO)
              PERFORM CLOSE-LP1-FILE
              GO TO TRANSFER-LP-REC-EXIT.

           PERFORM CLOSE-LP1-FILE.
           MOVE LP-SEQNO TO SAVE-LP-SEQNO.

      * WRITE PAYMENT RECORD INTO TO->LP-FILE:
           MOVE EXT-FILPATH-BASE           TO LP-ALL-BASE.
           MOVE TO-DATA-PATH               TO LP-ALL-DATA.
           MOVE TO-MACHINE                 TO LP-ALL-MACHINE.
           MOVE LP-ALL-PATH                TO LP-PATH.
           MOVE "Y"                        TO LP-PATH-OVERRIDE.

           PERFORM OPEN-LP1-FILE.
           MOVE LP-PATH-OWNBR TO LP-BRNO.
           MOVE TO-LN-ACCTNO  TO LP-ACCTNO.
           MOVE SAVE-LP-SEQNO TO LP-SEQNO.
           PERFORM WRITE-LP1-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

           PERFORM CLOSE-LP1-FILE.
           ADD 1 TO SAVE-LP-SEQNO.
           GO TO TRANSFER-LP-REC-NEXT.

       TRANSFER-LP-REC-EXIT.
           EXIT.


      *************************************************
       TRANSFER-LX-REC SECTION.
      *************************************************

           MOVE " " TO ERR-FG.
           MOVE EXT-FILPATH-BASE           TO LX-ALL-BASE.
           MOVE FROM-DATA-PATH             TO LX-ALL-DATA.
           MOVE FROM-MACHINE               TO LX-ALL-MACHINE.
           MOVE LX-ALL-PATH                TO LX-PATH.
           MOVE "Y"                        TO LX-PATH-OVERRIDE.

           MOVE EXT-FILPATH-BASE           TO LX-ALL-BASE.
           MOVE FROM-DATA-PATH             TO LX-ALL-DATA.
           MOVE FROM-MACHINE               TO LX-ALL-MACHINE.
           MOVE LX-ALL-PATH                TO LX-PATH.
           MOVE "Y"                        TO LX-PATH-OVERRIDE.

           PERFORM OPEN-LX1-FILE.

           MOVE PD-OWNBR  TO LX-BRNO
                             QLX1-WBEG-BRNO
                             QLX1-WEND-BRNO.
           MOVE PD-ACCTNO TO LX-ACCTNO
                             QLX1-WBEG-ACCTNO
                             QLX1-WEND-ACCTNO.
           MOVE 0         TO LX-SEQNO
                             QLX1-WBEG-SEQNO.
           MOVE ALL "9"   TO QLX1-WEND-SEQNO.

           PERFORM START-LX1-FILE.

       TRANSFER-LX-REC-NEXT.
           PERFORM READ-LX1-FILE-NEXT.
           IF IO-BAD OR (LX-ACCTNO NOT = PD-ACCTNO) OR
                         (LX-BRNO NOT = PD-OWNBR)
              PERFORM CLOSE-LX1-FILE
              GO TO TRANSFER-LX-REC-EXIT.
           PERFORM CLOSE-LX1-FILE.

           MOVE LX-SEQNO TO SAVE-LX-SEQNO.

      * WRITE PAYMENT TRAILER INTO TO->LX-FILE:
           MOVE EXT-FILPATH-BASE           TO LX-ALL-BASE.
           MOVE TO-DATA-PATH               TO LX-ALL-DATA.
           MOVE TO-MACHINE                 TO LX-ALL-MACHINE.
           MOVE LX-ALL-PATH                TO LX-PATH.
           MOVE "Y"                        TO LX-PATH-OVERRIDE.

           PERFORM OPEN-LX1-FILE.
           MOVE TO-BR         TO LX-BRNO.
           MOVE TO-LN-ACCTNO  TO LX-ACCTNO.
           MOVE SAVE-LX-SEQNO TO LX-SEQNO.

           PERFORM WRITE-LX1-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

           PERFORM CLOSE-LX1-FILE.

      * GET THIS RECORD BACK IN THE BUFFER FOR "FROM" FILE
           MOVE EXT-FILPATH-BASE           TO LX-ALL-BASE.
           MOVE FROM-DATA-PATH             TO LX-ALL-DATA.
           MOVE FROM-MACHINE               TO LX-ALL-MACHINE.
           MOVE LX-ALL-PATH                TO LX-PATH.
           MOVE "Y"                        TO LX-PATH-OVERRIDE.

           PERFORM OPEN-LX1-FILE.


           MOVE PD-OWNBR  TO LX-BRNO
                             QLX1-WBEG-BRNO
                             QLX1-WEND-BRNO.
           MOVE PD-ACCTNO TO LX-ACCTNO
                             QLX1-WBEG-ACCTNO
                             QLX1-WEND-ACCTNO.
           MOVE SAVE-LX-SEQNO TO LX-SEQNO
                                QLX1-WBEG-SEQNO.
           MOVE ALL "9"       TO QLX1-WEND-SEQNO.

           PERFORM START-LX1-FILE.
           PERFORM READ-LX1-FILE-NEXT.

           GO TO TRANSFER-LX-REC-NEXT.

       TRANSFER-LX-REC-EXIT.
           EXIT.

      *******************************************
       TRANSFER-LXG-REC SECTION.
      *******************************************
           MOVE " " TO ERR-FG.

           MOVE EXT-FILPATH-BASE           TO LXG-ALL-BASE.
           MOVE FROM-DATA-PATH             TO LXG-ALL-DATA.
           MOVE FROM-MACHINE               TO LXG-ALL-MACHINE.
           MOVE LXG-ALL-PATH               TO LXG-PATH.
           MOVE "Y"                        TO LXG-PATH-OVERRIDE.

           MOVE EXT-FILPATH-BASE           TO LXG-ALL-BASE.
           MOVE FROM-DATA-PATH             TO LXG-ALL-DATA.
           MOVE FROM-MACHINE               TO LXG-ALL-MACHINE.
           MOVE LXG-ALL-PATH               TO LXG-PATH.
           MOVE "Y"                        TO LXG-PATH-OVERRIDE.

           PERFORM OPEN-LXG1-FILE.

           MOVE PD-OWNBR  TO LXG-BRNO
                             QLXG1-WBEG-BRNO
                             QLXG1-WEND-BRNO.
           MOVE PD-ACCTNO TO LXG-ACCTNO
                             QLXG1-WBEG-ACCTNO
                             QLXG1-WEND-ACCTNO.
           MOVE 0         TO LXG-SEQNO
                             QLXG1-WBEG-SEQNO.
           MOVE ALL "9"   TO QLXG1-WEND-SEQNO.

           PERFORM START-LXG1-FILE.

       TRANSFER-LXG-REC-NEXT.
           PERFORM READ-LXG1-FILE-NEXT.
           IF IO-BAD OR (LXG-ACCTNO NOT = PD-ACCTNO) OR
                         (LXG-BRNO NOT = PD-OWNBR)
              PERFORM CLOSE-LXG1-FILE
              GO TO TRANSFER-LXG-REC-EXIT.
           PERFORM CLOSE-LXG1-FILE.

           MOVE LXG-SEQNO TO SAVE-LX-SEQNO.

      * WRITE PAYMENT TRAILER INTO TO->LXG-FILE:
           MOVE EXT-FILPATH-BASE           TO LXG-ALL-BASE.
           MOVE TO-DATA-PATH               TO LXG-ALL-DATA.
           MOVE TO-MACHINE                 TO LXG-ALL-MACHINE.
           MOVE LXG-ALL-PATH               TO LXG-PATH.
           MOVE "Y"                        TO LXG-PATH-OVERRIDE.

           PERFORM OPEN-LXG1-FILE.
           MOVE TO-BR         TO LXG-BRNO.
           MOVE TO-LN-ACCTNO  TO LXG-ACCTNO.
           MOVE SAVE-LX-SEQNO TO LXG-SEQNO.

      * MUST CHANGE LXG-GLNO (FROM BRANCH NO) TO (TO BRANCH NO):
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 3
              MOVE LXG-GLNO(SUB) TO WK-GLNO
              IF WK-GLNO-BR = FROM-BRANCH
                 MOVE TO-BR TO WK-GLNO-BR
                 MOVE WK-GLNO TO LXG-GLNO(SUB)
              END-IF
           END-PERFORM.

           PERFORM WRITE-LXG1-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

           PERFORM CLOSE-LXG1-FILE.

      * GET THIS RECORD BACK IN THE BUFFER FOR "FROM" FILE
           MOVE EXT-FILPATH-BASE           TO LXG-ALL-BASE.
           MOVE FROM-DATA-PATH             TO LXG-ALL-DATA.
           MOVE FROM-MACHINE               TO LXG-ALL-MACHINE.
           MOVE LXG-ALL-PATH               TO LXG-PATH.
           MOVE "Y"                        TO LXG-PATH-OVERRIDE.

           PERFORM OPEN-LXG1-FILE.

           MOVE PD-OWNBR  TO LXG-BRNO
                             QLXG1-WBEG-BRNO
                             QLXG1-WEND-BRNO.
           MOVE PD-ACCTNO TO LXG-ACCTNO
                             QLXG1-WBEG-ACCTNO
                             QLXG1-WEND-ACCTNO.
           MOVE SAVE-LX-SEQNO TO LXG-SEQNO
                                 QLXG1-WBEG-SEQNO.
           MOVE ALL "9"       TO QLXG1-WEND-SEQNO.

           PERFORM START-LXG1-FILE.
           PERFORM READ-LXG1-FILE-NEXT.

           GO TO TRANSFER-LXG-REC-NEXT.

       TRANSFER-LXG-REC-EXIT.
           EXIT.

      *******************************************
       TRANSFER-LXE-REC SECTION.
      *******************************************
           MOVE " " TO ERR-FG.

           MOVE EXT-FILPATH-BASE           TO LXE-ALL-BASE.
           MOVE FROM-DATA-PATH             TO LXE-ALL-DATA.
           MOVE FROM-MACHINE               TO LXE-ALL-MACHINE.
           MOVE LXE-ALL-PATH               TO LXE-PATH.
           MOVE "Y"                        TO LXE-PATH-OVERRIDE.

           MOVE EXT-FILPATH-BASE           TO LXE-ALL-BASE.
           MOVE FROM-DATA-PATH             TO LXE-ALL-DATA.
           MOVE FROM-MACHINE               TO LXE-ALL-MACHINE.
           MOVE LXE-ALL-PATH               TO LXE-PATH.
           MOVE "Y"                        TO LXE-PATH-OVERRIDE.

           PERFORM OPEN-LXE1-FILE.

           MOVE PD-OWNBR  TO LXE-BRNO
                             QLXE1-WBEG-BRNO
                             QLXE1-WEND-BRNO.
           MOVE PD-ACCTNO TO LXE-ACCTNO
                             QLXE1-WBEG-ACCTNO
                             QLXE1-WEND-ACCTNO.
           MOVE 0         TO LXE-SEQNO
                             QLXE1-WBEG-SEQNO.
           MOVE ALL "9"   TO QLXE1-WEND-SEQNO.

           PERFORM START-LXE1-FILE.

       TRANSFER-LXE-REC-NEXT.
           PERFORM READ-LXE1-FILE-NEXT.
           IF IO-BAD OR (LXE-ACCTNO NOT = PD-ACCTNO) OR
                         (LXE-BRNO NOT = PD-OWNBR)
              PERFORM CLOSE-LXE1-FILE
              GO TO TRANSFER-LXE-REC-EXIT.
           PERFORM CLOSE-LXE1-FILE.

           MOVE LXE-SEQNO TO SAVE-LX-SEQNO.

      * WRITE PAYMENT TRAILER INTO TO->LXE-FILE:
           MOVE EXT-FILPATH-BASE           TO LXE-ALL-BASE.
           MOVE TO-DATA-PATH               TO LXE-ALL-DATA.
           MOVE TO-MACHINE                 TO LXE-ALL-MACHINE.
           MOVE LXE-ALL-PATH               TO LXE-PATH.
           MOVE "Y"                        TO LXE-PATH-OVERRIDE.

           PERFORM OPEN-LXE1-FILE.
           MOVE TO-BR         TO LXE-BRNO.
           MOVE TO-LN-ACCTNO  TO LXE-ACCTNO.
           MOVE SAVE-LX-SEQNO TO LXE-SEQNO.

           PERFORM WRITE-LXE1-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

           PERFORM CLOSE-LXE1-FILE.

      * GET THIS RECORD BACK IN THE BUFFER FOR "FROM" FILE
           MOVE EXT-FILPATH-BASE           TO LXE-ALL-BASE.
           MOVE FROM-DATA-PATH             TO LXE-ALL-DATA.
           MOVE FROM-MACHINE               TO LXE-ALL-MACHINE.
           MOVE LXE-ALL-PATH               TO LXE-PATH.
           MOVE "Y"                        TO LXE-PATH-OVERRIDE.

           PERFORM OPEN-LXE1-FILE.

           MOVE PD-OWNBR  TO LXE-BRNO
                             QLXE1-WBEG-BRNO
                             QLXE1-WEND-BRNO.
           MOVE PD-ACCTNO TO LXE-ACCTNO
                             QLXE1-WBEG-ACCTNO
                             QLXE1-WEND-ACCTNO.
           MOVE SAVE-LX-SEQNO TO LXE-SEQNO
                                 QLXE1-WBEG-SEQNO.
           MOVE ALL "9"       TO QLXE1-WEND-SEQNO.

           PERFORM START-LXE1-FILE.
           PERFORM READ-LXE1-FILE-NEXT.

           GO TO TRANSFER-LXE-REC-NEXT.

       TRANSFER-LXE-REC-EXIT.
           EXIT.

      *************************************************
       TRANSFER-LTA-RECORDS SECTION.
      *************************************************

           MOVE " " TO ERR-FG.

           MOVE EXT-FILPATH-BASE           TO LTA-ALL-BASE.
           MOVE FROM-DATA-PATH             TO LTA-ALL-DATA.
           MOVE FROM-MACHINE               TO LTA-ALL-MACHINE.
           MOVE LTA-ALL-PATH               TO LTA-PATH.
           MOVE "Y"                        TO LTA-PATH-OVERRIDE.

           MOVE 0 TO SAVE-LT-SEQNO.

       TRANSFER-LTA-REC-NEXT.
           MOVE EXT-FILPATH-BASE           TO LTA-ALL-BASE.
           MOVE FROM-DATA-PATH             TO LTA-ALL-DATA.
           MOVE FROM-MACHINE               TO LTA-ALL-MACHINE.
           MOVE LTA-ALL-PATH               TO LTA-PATH.
           MOVE "Y"                        TO LTA-PATH-OVERRIDE.

           PERFORM OPEN-LTA1-FILE.

           MOVE LTA-PATH-OWNBR TO LTA-BRNO
                                  QLTA1-WBEG-BRNO
                                  QLTA1-WEND-BRNO.
           MOVE PD-ACCTNO      TO LTA-ACCTNO
                                  QLTA1-WBEG-ACCTNO
                                  QLTA1-WEND-ACCTNO.
           MOVE SAVE-LT-SEQNO  TO LTA-SEQNO
                                  QLTA1-WBEG-SEQNO.
           MOVE ALL "9"        TO QLTA1-WEND-SEQNO.

           PERFORM START-LTA1-FILE.

           PERFORM READ-LTA1-FILE-NEXT.
           PERFORM CLOSE-LTA1-FILE.
           IF IO-BAD OR (LTA-BRNO NOT = LTA-PATH-OWNBR)  
                 OR (LTA-ACCTNO NOT = PD-ACCTNO)
              GO TO TRANSFER-LTA-REC-EXIT.

           MOVE LTA-SEQNO TO SAVE-LT-SEQNO.

      * WRITE PAYMENT RECORD INTO TO->LT-FILE:
           MOVE EXT-FILPATH-BASE           TO LTA-ALL-BASE.
           MOVE TO-MACHINE                 TO LTA-ALL-MACHINE.
           MOVE TO-DATA-PATH               TO LTA-ALL-DATA.
           MOVE LTA-ALL-PATH               TO LTA-PATH.
           MOVE "Y"                        TO LTA-PATH-OVERRIDE.

           PERFORM OPEN-LTA1-FILE.
           MOVE TO-BR          TO LTA-BRNO.
           MOVE TO-LN-ACCTNO   TO LTA-ACCTNO.
           MOVE SAVE-LT-SEQNO  TO LTA-SEQNO.
           PERFORM WRITE-LTA1-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

           PERFORM CLOSE-LTA1-FILE.
           ADD 1 TO SAVE-LT-SEQNO.
           GO TO TRANSFER-LTA-REC-NEXT.

       TRANSFER-LTA-REC-EXIT.
           EXIT.

      *************************************************
       TRANSFER-LTB-RECORDS SECTION.
      *************************************************

           MOVE " " TO ERR-FG.

           MOVE EXT-FILPATH-BASE           TO LTB-ALL-BASE.
           MOVE FROM-DATA-PATH             TO LTB-ALL-DATA.
           MOVE FROM-MACHINE               TO LTB-ALL-MACHINE.
           MOVE LTB-ALL-PATH               TO LTB-PATH.
           MOVE "Y"                        TO LTB-PATH-OVERRIDE.

           MOVE 0 TO SAVE-LT-SEQNO.

       TRANSFER-LTB-REC-NEXT.
           MOVE EXT-FILPATH-BASE           TO LTB-ALL-BASE.
           MOVE FROM-DATA-PATH             TO LTB-ALL-DATA.
           MOVE FROM-MACHINE               TO LTB-ALL-MACHINE.
           MOVE LTB-ALL-PATH               TO LTB-PATH.
           MOVE "Y"                        TO LTB-PATH-OVERRIDE.

           PERFORM OPEN-LTB1-FILE.


           MOVE LTB-PATH-OWNBR TO LTB-BRNO
                                  QLTB1-WBEG-BRNO
                                  QLTB1-WEND-BRNO.
           MOVE PD-ACCTNO      TO LTB-ACCTNO
                                  QLTB1-WBEG-ACCTNO
                                  QLTB1-WEND-ACCTNO.
           MOVE SAVE-LT-SEQNO  TO LTB-SEQNO
                                  QLTB1-WBEG-SEQNO.
           MOVE ALL "9"        TO QLTB1-WEND-SEQNO.

           PERFORM START-LTB1-FILE.

           PERFORM READ-LTB1-FILE-NEXT.
           PERFORM CLOSE-LTB1-FILE.
           IF IO-BAD OR (LTB-BRNO NOT = LTB-PATH-OWNBR)
                     OR (LTB-ACCTNO NOT = PD-ACCTNO)
              GO TO TRANSFER-LTB-REC-EXIT.

           MOVE LTB-SEQNO TO SAVE-LT-SEQNO.

      * WRITE PAYMENT RECORD INTO TO->LT-FILE:
           MOVE EXT-FILPATH-BASE           TO LTB-ALL-BASE.
           MOVE TO-MACHINE                 TO LTB-ALL-MACHINE.
           MOVE TO-DATA-PATH               TO LTB-ALL-DATA.
           MOVE LTB-ALL-PATH               TO LTB-PATH.
           MOVE "Y"                        TO LTB-PATH-OVERRIDE.

           PERFORM OPEN-LTB1-FILE.
           MOVE TO-BR          TO LTB-BRNO.
           MOVE TO-LN-ACCTNO   TO LTB-ACCTNO.
           MOVE SAVE-LT-SEQNO  TO LTB-SEQNO.
           PERFORM WRITE-LTB1-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

           PERFORM CLOSE-LTB1-FILE.
           ADD 1 TO SAVE-LT-SEQNO.
           GO TO TRANSFER-LTB-REC-NEXT.

       TRANSFER-LTB-REC-EXIT.
           EXIT.

      ****************************************************
       TRANSFER-LTC-RECORDS SECTION.
      ****************************************************
           MOVE " " TO ERR-FG.

           MOVE EXT-FILPATH-BASE           TO LTC-ALL-BASE.
           MOVE FROM-DATA-PATH             TO LTC-ALL-DATA.
           MOVE FROM-MACHINE               TO LTC-ALL-MACHINE.
           MOVE LTC-ALL-PATH               TO LTC-PATH.
           MOVE "Y"                        TO LTC-PATH-OVERRIDE.

           MOVE 0 TO SAVE-LT-SEQNO.

       TRANSFER-LTC-REC-NEXT.
           MOVE EXT-FILPATH-BASE           TO LTC-ALL-BASE.
           MOVE FROM-DATA-PATH             TO LTC-ALL-DATA.
           MOVE FROM-MACHINE               TO LTC-ALL-MACHINE.
           MOVE LTC-ALL-PATH               TO LTC-PATH.
           MOVE "Y"                        TO LTC-PATH-OVERRIDE.

           PERFORM OPEN-LTC1-FILE.

           MOVE LTC-PATH-OWNBR TO LTC-BRNO
                                  QLTC1-WBEG-BRNO
                                  QLTC1-WEND-BRNO.
           MOVE PD-ACCTNO      TO LTC-ACCTNO
                                  QLTC1-WBEG-ACCTNO
                                  QLTC1-WEND-ACCTNO.
           MOVE SAVE-LT-SEQNO  TO LTC-SEQNO
                                  QLTC1-WBEG-SEQNO.
           MOVE ALL "9"        TO QLTC1-WEND-SEQNO.

           PERFORM START-LTC1-FILE.

           PERFORM READ-LTC1-FILE-NEXT.
           PERFORM CLOSE-LTC1-FILE.
           IF IO-BAD OR (LTC-BRNO NOT = LTC-PATH-OWNBR) OR  
                         (LTC-ACCTNO NOT = PD-ACCTNO)
              GO TO TRANSFER-LTC-REC-EXIT.

           MOVE LTC-SEQNO TO SAVE-LT-SEQNO.

      * WRITE PAYMENT RECORD INTO TO->LT-FILE:
           MOVE EXT-FILPATH-BASE           TO LTC-ALL-BASE
                                              IX-ALL-BASE.
           MOVE TO-MACHINE                 TO LTC-ALL-MACHINE
                                              IX-ALL-MACHINE.
           MOVE TO-DATA-PATH               TO LTC-ALL-DATA
                                              IX-ALL-DATA.
           MOVE IX-ALL-PATH                TO IX-PATH CREAIX-IX-PATH.
           MOVE LTC-ALL-PATH               TO LTC-PATH.
           MOVE "Y"                        TO LTC-PATH-OVERRIDE
                                              IX-PATH-OVERRIDE.

           PERFORM OPEN-LTC1-FILE.
           MOVE TO-BR          TO LTC-BRNO.
           MOVE TO-LN-ACCTNO   TO LTC-ACCTNO.
           MOVE SAVE-LT-SEQNO  TO LTC-SEQNO.
           PERFORM WRITE-LTC1-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

           PERFORM CLOSE-LTC1-FILE.
           ADD 1 TO SAVE-LT-SEQNO.
           GO TO TRANSFER-LTC-REC-NEXT.

       TRANSFER-LTC-REC-EXIT.
           EXIT.

      *************************************************
       TRANSFER-LTD-RECORDS SECTION.
      *************************************************

           MOVE " " TO ERR-FG.

           MOVE EXT-FILPATH-BASE           TO LTD-ALL-BASE.
           MOVE FROM-DATA-PATH             TO LTD-ALL-DATA.
           MOVE FROM-MACHINE               TO LTD-ALL-MACHINE.
           MOVE LTD-ALL-PATH               TO LTD-PATH.
           MOVE "Y"                        TO LTD-PATH-OVERRIDE.

           MOVE 0 TO SAVE-LT-SEQNO.

       TRANSFER-LTD-REC-NEXT.
           MOVE EXT-FILPATH-BASE           TO LTD-ALL-BASE.
           MOVE FROM-DATA-PATH             TO LTD-ALL-DATA.
           MOVE FROM-MACHINE               TO LTD-ALL-MACHINE.
           MOVE LTD-ALL-PATH               TO LTD-PATH.
           MOVE "Y"                        TO LTD-PATH-OVERRIDE.

           PERFORM OPEN-LTD1-FILE.

           MOVE LTD-PATH-OWNBR TO LTD-BRNO
                                  QLTD1-WBEG-BRNO
                                  QLTD1-WEND-BRNO.
           MOVE PD-ACCTNO      TO LTD-ACCTNO
                                  QLTD1-WBEG-ACCTNO
                                  QLTD1-WEND-ACCTNO.
           MOVE SAVE-LT-SEQNO  TO LTD-SEQNO
                                  QLTD1-WBEG-SEQNO.
           MOVE ALL "9"        TO QLTD1-WEND-SEQNO.

           PERFORM START-LTD1-FILE.

           PERFORM READ-LTD1-FILE-NEXT.
           PERFORM CLOSE-LTD1-FILE.
           IF IO-BAD OR (LTD-BRNO NOT = LTD-PATH-OWNBR)
                     OR (LTD-ACCTNO NOT = PD-ACCTNO)
              GO TO TRANSFER-LTD-REC-EXIT.

           MOVE LTD-SEQNO TO SAVE-LT-SEQNO.

      * WRITE PAYMENT RECORD INTO TO->LT-FILE:
           MOVE EXT-FILPATH-BASE           TO LTD-ALL-BASE.
           MOVE TO-MACHINE                 TO LTD-ALL-MACHINE.
           MOVE TO-DATA-PATH               TO LTD-ALL-DATA.
           MOVE LTD-ALL-PATH               TO LTD-PATH.
           MOVE "Y"                        TO LTD-PATH-OVERRIDE.

           PERFORM OPEN-LTD1-FILE.
           MOVE TO-BR          TO LTD-BRNO.
           MOVE TO-LN-ACCTNO   TO LTD-ACCTNO.
           MOVE SAVE-LT-SEQNO  TO LTD-SEQNO.
           PERFORM WRITE-LTD1-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

           PERFORM CLOSE-LTD1-FILE.
           ADD 1 TO SAVE-LT-SEQNO.
           GO TO TRANSFER-LTD-REC-NEXT.

       TRANSFER-LTD-REC-EXIT.
           EXIT.

      *********************************************
       TRANSFER-LTF-RECORDS SECTION.
      *********************************************

           MOVE " " TO ERR-FG.

           MOVE EXT-FILPATH-BASE           TO LTF-ALL-BASE.
           MOVE FROM-DATA-PATH             TO LTF-ALL-DATA.
           MOVE FROM-MACHINE               TO LTF-ALL-MACHINE.
           MOVE LTF-ALL-PATH               TO LTF-PATH.
           MOVE "Y"                        TO LTF-PATH-OVERRIDE.

           MOVE 0 TO SAVE-LT-SEQNO.

       TRANSFER-LTF-REC-NEXT.
           MOVE EXT-FILPATH-BASE           TO LTF-ALL-BASE.
           MOVE FROM-DATA-PATH             TO LTF-ALL-DATA.
           MOVE FROM-MACHINE               TO LTF-ALL-MACHINE.
           MOVE LTF-ALL-PATH               TO LTF-PATH.
           MOVE "Y"                        TO LTF-PATH-OVERRIDE.

           PERFORM OPEN-LTF1-FILE.

           MOVE LTF-PATH-OWNBR TO LTF-BRNO
                                  QLTF1-WBEG-BRNO
                                  QLTF1-WEND-BRNO.
           MOVE PD-ACCTNO      TO LTF-ACCTNO
                                  QLTF1-WBEG-ACCTNO
                                  QLTF1-WEND-ACCTNO.
           MOVE SAVE-LT-SEQNO  TO LTF-SEQNO
                                  QLTF1-WBEG-SEQNO.
           MOVE ALL "9"        TO QLTF1-WEND-SEQNO.

           PERFORM START-LTF1-FILE.

           PERFORM READ-LTF1-FILE-NEXT.
           PERFORM CLOSE-LTF1-FILE.
           IF IO-BAD OR (LTF-BRNO NOT = LTF-PATH-OWNBR) OR
                         (LTF-ACCTNO NOT = PD-ACCTNO)
              GO TO TRANSFER-LTF-REC-EXIT.

           MOVE LTF-SEQNO TO SAVE-LT-SEQNO.

      * WRITE PAYMENT RECORD INTO TO->LT-FILE:
           MOVE EXT-FILPATH-BASE           TO LTF-ALL-BASE.
           MOVE TO-MACHINE                 TO LTF-ALL-MACHINE.
           MOVE TO-DATA-PATH               TO LTF-ALL-DATA.
           MOVE LTF-ALL-PATH               TO LTF-PATH.
           MOVE "Y"                        TO LTF-PATH-OVERRIDE.

           PERFORM OPEN-LTF1-FILE.
           MOVE TO-BR          TO LTF-BRNO.
           MOVE TO-LN-ACCTNO   TO LTF-ACCTNO.
           MOVE SAVE-LT-SEQNO  TO LTF-SEQNO.
           PERFORM WRITE-LTF1-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

           PERFORM CLOSE-LTF1-FILE.
           ADD 1 TO SAVE-LT-SEQNO.
           GO TO TRANSFER-LTF-REC-NEXT.

       TRANSFER-LTF-REC-EXIT.
           EXIT.

      *************************************************
       TRANSFER-LTI-RECORDS SECTION.
      *************************************************

           MOVE " " TO ERR-FG.

           MOVE EXT-FILPATH-BASE           TO LTI-ALL-BASE.
           MOVE FROM-DATA-PATH             TO LTI-ALL-DATA.
           MOVE FROM-MACHINE               TO LTI-ALL-MACHINE.
           MOVE LTI-ALL-PATH               TO LTI-PATH.
           MOVE "Y"                        TO LTI-PATH-OVERRIDE.

           MOVE 0 TO SAVE-LT-SEQNO.

       TRANSFER-LTI-REC-NEXT.
           MOVE EXT-FILPATH-BASE           TO LTI-ALL-BASE.
           MOVE FROM-DATA-PATH             TO LTI-ALL-DATA.
           MOVE FROM-MACHINE               TO LTI-ALL-MACHINE.
           MOVE LTI-ALL-PATH               TO LTI-PATH.
           MOVE "Y"                        TO LTI-PATH-OVERRIDE.

           PERFORM OPEN-LTI1-FILE.

           MOVE LTI-PATH-OWNBR TO LTI-BRNO
                                  QLTI1-WBEG-BRNO
                                  QLTI1-WEND-BRNO.
           MOVE PD-ACCTNO      TO LTI-ACCTNO
                                  QLTI1-WBEG-ACCTNO
                                  QLTI1-WEND-ACCTNO.
           MOVE SAVE-LT-SEQNO  TO LTI-SEQNO
                                  QLTI1-WBEG-SEQNO.
           MOVE ALL "9"        TO QLTI1-WEND-SEQNO.

           PERFORM START-LTI1-FILE.

           PERFORM READ-LTI1-FILE-NEXT.
           PERFORM CLOSE-LTI1-FILE.
           IF IO-BAD OR (LTI-BRNO NOT = LTI-PATH-OWNBR) OR
                          (LTI-ACCTNO NOT = PD-ACCTNO)
              GO TO TRANSFER-LTI-REC-EXIT.

           MOVE LTI-SEQNO TO SAVE-LT-SEQNO.

      * WRITE PAYMENT RECORD INTO TO->LT-FILE:
           MOVE EXT-FILPATH-BASE           TO LTI-ALL-BASE.
           MOVE TO-MACHINE                 TO LTI-ALL-MACHINE.
           MOVE TO-DATA-PATH               TO LTI-ALL-DATA.
           MOVE LTI-ALL-PATH               TO LTI-PATH.
           MOVE "Y"                        TO LTI-PATH-OVERRIDE.

           PERFORM OPEN-LTI1-FILE.
           MOVE LTI-PATH-OWNBR TO LTI-BRNO.
           MOVE TO-LN-ACCTNO   TO LTI-ACCTNO.
           MOVE SAVE-LT-SEQNO  TO LTI-SEQNO.
           PERFORM WRITE-LTI1-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

           PERFORM CLOSE-LTI1-FILE.
           ADD 1 TO SAVE-LT-SEQNO.
           GO TO TRANSFER-LTI-REC-NEXT.

       TRANSFER-LTI-REC-EXIT.
           EXIT.

      *************************************************
       TRANSFER-LTL-RECORDS SECTION.
      *************************************************

           MOVE " " TO ERR-FG.

           MOVE EXT-FILPATH-BASE           TO LTL-ALL-BASE.
           MOVE FROM-DATA-PATH             TO LTL-ALL-DATA.
           MOVE FROM-MACHINE               TO LTL-ALL-MACHINE.
           MOVE LTL-ALL-PATH               TO LTL-PATH.
           MOVE "Y"                        TO LTL-PATH-OVERRIDE.

           MOVE 0 TO SAVE-LT-SEQNO.

       TRANSFER-LTL-REC-NEXT.
           MOVE EXT-FILPATH-BASE           TO LTL-ALL-BASE.
           MOVE FROM-DATA-PATH             TO LTL-ALL-DATA.
           MOVE FROM-MACHINE               TO LTL-ALL-MACHINE.
           MOVE LTL-ALL-PATH               TO LTL-PATH.
           MOVE "Y"                        TO LTL-PATH-OVERRIDE.

           PERFORM OPEN-LTL1-FILE.

           MOVE LTL-PATH-OWNBR TO LTL-BRNO
                                  QLTL1-WBEG-BRNO
                                  QLTL1-WEND-BRNO.
           MOVE PD-ACCTNO      TO LTL-ACCTNO
                                  QLTL1-WBEG-ACCTNO
                                  QLTL1-WEND-ACCTNO.
           MOVE SAVE-LT-SEQNO  TO LTL-SEQNO
                                  QLTL1-WBEG-SEQNO.
           MOVE ALL "9"        TO QLTL1-WEND-SEQNO.

           PERFORM START-LTL1-FILE.

           PERFORM READ-LTL1-FILE-NEXT.
           PERFORM CLOSE-LTL1-FILE.
           IF IO-BAD OR (LTL-BRNO NOT = LTL-PATH-OWNBR) OR
                         (LTL-ACCTNO NOT = PD-ACCTNO)
              GO TO TRANSFER-LTL-REC-EXIT.

           MOVE LTL-SEQNO TO SAVE-LT-SEQNO.

      * WRITE PAYMENT RECORD INTO TO->LT-FILE:
           MOVE EXT-FILPATH-BASE           TO LTL-ALL-BASE.
           MOVE TO-MACHINE                 TO LTL-ALL-MACHINE.
           MOVE TO-DATA-PATH               TO LTL-ALL-DATA.
           MOVE LTL-ALL-PATH               TO LTL-PATH.
           MOVE "Y"                        TO LTL-PATH-OVERRIDE.

           PERFORM OPEN-LTL1-FILE.
           MOVE TO-BR          TO LTL-BRNO.
           MOVE TO-LN-ACCTNO   TO LTL-ACCTNO.
           MOVE SAVE-LT-SEQNO  TO LTL-SEQNO.
           PERFORM WRITE-LTL1-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

           PERFORM CLOSE-LTL1-FILE.
           ADD 1 TO SAVE-LT-SEQNO.
           GO TO TRANSFER-LTL-REC-NEXT.

       TRANSFER-LTL-REC-EXIT.
           EXIT.

      *************************************************
       TRANSFER-LTP-RECORDS SECTION.
      *************************************************

           MOVE " " TO ERR-FG.

           MOVE EXT-FILPATH-BASE           TO LTP-ALL-BASE.
           MOVE FROM-DATA-PATH             TO LTP-ALL-DATA.
           MOVE FROM-MACHINE               TO LTP-ALL-MACHINE.
           MOVE LTP-ALL-PATH               TO LTP-PATH.
           MOVE "Y"                        TO LTP-PATH-OVERRIDE.

           MOVE 0 TO SAVE-LT-SEQNO.

       TRANSFER-LTP-REC-NEXT.
           MOVE EXT-FILPATH-BASE           TO LTP-ALL-BASE.
           MOVE FROM-DATA-PATH             TO LTP-ALL-DATA.
           MOVE FROM-MACHINE               TO LTP-ALL-MACHINE.
           MOVE LTP-ALL-PATH               TO LTP-PATH.
           MOVE "Y"                        TO LTP-PATH-OVERRIDE.

           PERFORM OPEN-LTP1-FILE.

           MOVE LTP-PATH-OWNBR TO LTP-BRNO
                                  QLTP1-WBEG-BRANCH
                                  QLTP1-WEND-BRANCH.
           MOVE PD-ACCTNO      TO LTP-ACCTNO
                                  QLTP1-WBEG-ACCTNO
                                  QLTP1-WEND-ACCTNO.
           MOVE SAVE-LT-SEQNO  TO LTP-SEQNO
                                  QLTP1-WBEG-SEQNO.
           MOVE ALL "9"        TO QLTP1-WEND-SEQNO.

           PERFORM START-LTP1-FILE.

           PERFORM READ-LTP1-FILE-NEXT.
           PERFORM CLOSE-LTP1-FILE.
           IF IO-BAD OR (LTP-BRNO NOT = LTP-PATH-OWNBR) OR
                 (LTP-ACCTNO NOT = PD-ACCTNO)
              GO TO TRANSFER-LTP-REC-EXIT.

           MOVE LTP-SEQNO TO SAVE-LT-SEQNO.

      * WRITE PAYMENT RECORD INTO TO->LT-FILE:
           MOVE EXT-FILPATH-BASE           TO LTP-ALL-BASE.
           MOVE TO-MACHINE                 TO LTP-ALL-MACHINE.
           MOVE TO-DATA-PATH               TO LTP-ALL-DATA.
           MOVE LTP-ALL-PATH               TO LTP-PATH.
           MOVE "Y"                        TO LTP-PATH-OVERRIDE.

           PERFORM OPEN-LTP1-FILE.
           MOVE TO-BR          TO LTP-BRNO.
           MOVE TO-LN-ACCTNO   TO LTP-ACCTNO.
           MOVE SAVE-LT-SEQNO  TO LTP-SEQNO.
           PERFORM WRITE-LTP1-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

           PERFORM CLOSE-LTP1-FILE.
           ADD 1 TO SAVE-LT-SEQNO.
           GO TO TRANSFER-LTP-REC-NEXT.

       TRANSFER-LTP-REC-EXIT.
           EXIT.

      *************************************************
      *    TRANSFER DEFICIENCY TRAILERS TO TO->LS-FILE
      *************************************************
       TRANSFER-LSD-REC SECTION.

           MOVE " "                        TO ERR-FG.
           MOVE EXT-FILPATH-BASE           TO LSD-ALL-BASE.
           MOVE FROM-MACHINE               TO LSD-ALL-MACHINE.
           MOVE FROM-DATA-PATH             TO LSD-ALL-DATA.
           MOVE LSD-ALL-PATH               TO LSD-PATH.
           MOVE "Y"                        TO LSD-PATH-OVERRIDE.

           MOVE 0 TO SAVE-LS-SEQNO.

       TRANSFER-LSD-REC-NEXT.
           MOVE EXT-FILPATH-BASE           TO LSD-ALL-BASE.
           MOVE FROM-MACHINE                 TO LSD-ALL-MACHINE.
           MOVE FROM-DATA-PATH               TO LSD-ALL-DATA.
           MOVE LSD-ALL-PATH               TO LSD-PATH.
           MOVE "Y"                        TO LSD-PATH-OVERRIDE.

           PERFORM OPEN-LSD1-FILE.

           MOVE LSD-PATH-OWNBR TO LSD-BRNO
                                  QLSD1-WBEG-BRNO
                                  QLSD1-WEND-BRNO.
           MOVE PD-ACCTNO      TO LSD-ACCTNO
                                  QLSD1-WBEG-ACCTNO
                                  QLSD1-WEND-ACCTNO.
           MOVE SAVE-LS-SEQNO  TO LSD-SEQNO
                                  QLSD1-WBEG-SEQNO.
           MOVE ALL "9"        TO QLSD1-WEND-SEQNO.

           PERFORM START-LSD1-FILE.

           PERFORM READ-LSD1-FILE-NEXT.
           PERFORM CLOSE-LSD1-FILE.
           IF IO-BAD OR (LSD-BRNO NOT = LSD-PATH-OWNBR) OR  
                         (LSD-ACCTNO NOT = PD-ACCTNO)
              GO TO TRANSFER-LSD-REC-EXIT.

           MOVE LSD-SEQNO TO SAVE-LS-SEQNO.

      * WRITE TRAILER RECORD INTO TO->LS-FILE:
           MOVE EXT-FILPATH-BASE           TO LSD-ALL-BASE.
           MOVE TO-MACHINE                 TO LSD-ALL-MACHINE
           MOVE TO-DATA-PATH               TO LSD-ALL-DATA
           MOVE LSD-ALL-PATH               TO LSD-PATH.
           MOVE "Y"                        TO LSD-PATH-OVERRIDE.

           PERFORM OPEN-LSD1-FILE.
           MOVE TO-BR         TO LSD-BRNO.
           MOVE TO-LN-ACCTNO  TO LSD-ACCTNO.
           MOVE SAVE-LS-SEQNO TO LSD-SEQNO.
           PERFORM WRITE-LSD1-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

           PERFORM CLOSE-LSD1-FILE.
           ADD 1 TO SAVE-LS-SEQNO.
           GO TO TRANSFER-LSD-REC-NEXT.

       TRANSFER-LSD-REC-EXIT.
           EXIT.

      *************************************************
      *    TRANSFER FOLLOWUP TRAILERS TO TO->LS-FILE
      *************************************************
       TRANSFER-LSF-REC SECTION.

           MOVE " "                        TO ERR-FG.
           MOVE EXT-FILPATH-BASE           TO LSF-ALL-BASE.
           MOVE FROM-MACHINE                 TO LSF-ALL-MACHINE.
           MOVE FROM-DATA-PATH               TO LSF-ALL-DATA.
           MOVE LSF-ALL-PATH               TO LSF-PATH.
           MOVE "Y"                        TO LSF-PATH-OVERRIDE.

           MOVE 0 TO SAVE-LS-SEQNO.

       TRANSFER-LSF-REC-NEXT.
           MOVE EXT-FILPATH-BASE           TO LSF-ALL-BASE.
           MOVE FROM-MACHINE                 TO LSF-ALL-MACHINE.
           MOVE FROM-DATA-PATH               TO LSF-ALL-DATA.
           MOVE LSF-ALL-PATH               TO LSF-PATH.
           MOVE "Y"                        TO LSF-PATH-OVERRIDE.

           PERFORM OPEN-LSF1-FILE.

           MOVE LSF-PATH-OWNBR TO LSF-BRNO
                                  QLSF1-WBEG-BRNO
                                  QLSF1-WEND-BRNO.
           MOVE PD-ACCTNO      TO LSF-ACCTNO
                                  QLSF1-WBEG-ACCTNO
                                  QLSF1-WEND-ACCTNO.
           MOVE SAVE-LS-SEQNO  TO LSF-SEQNO
                                  QLSF1-WBEG-SEQNO.
           MOVE ALL "9"        TO QLSF1-WEND-SEQNO.

           PERFORM START-LSF1-FILE.

           PERFORM READ-LSF1-FILE-NEXT.
           PERFORM CLOSE-LSF1-FILE.
           IF IO-BAD OR (LSF-BRNO NOT = LSF-PATH-OWNBR) OR
                         (LSF-ACCTNO NOT = PD-ACCTNO)
              GO TO TRANSFER-LSF-REC-EXIT.

           MOVE LSF-SEQNO TO SAVE-LS-SEQNO.

      * WRITE TRAILER RECORD INTO TO->LS-FILE:
           MOVE EXT-FILPATH-BASE           TO LSF-ALL-BASE.
           MOVE TO-MACHINE                 TO LSF-ALL-MACHINE
           MOVE TO-DATA-PATH               TO LSF-ALL-DATA
           MOVE LSF-ALL-PATH               TO LSF-PATH.
           MOVE "Y"                        TO LSF-PATH-OVERRIDE.

           PERFORM OPEN-LSF1-FILE.
           MOVE TO-BR         TO LSF-BRNO.
           MOVE TO-LN-ACCTNO  TO LSF-ACCTNO.
           MOVE SAVE-LS-SEQNO TO LSF-SEQNO.
           PERFORM WRITE-LSF1-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

           PERFORM CLOSE-LSF1-FILE.
           ADD 1 TO SAVE-LS-SEQNO.
           GO TO TRANSFER-LSF-REC-NEXT.

       TRANSFER-LSF-REC-EXIT.
           EXIT.

      *************************************************
      *    TRANSFER GPS TRAILERS TO TO->LS-FILE
      *************************************************
       TRANSFER-LSG-REC SECTION.

           MOVE " "                        TO ERR-FG.
           MOVE EXT-FILPATH-BASE           TO LSG-ALL-BASE.
           MOVE FROM-MACHINE                 TO LSG-ALL-MACHINE.
           MOVE FROM-DATA-PATH               TO LSG-ALL-DATA.
           MOVE LSG-ALL-PATH               TO LSG-PATH.
           MOVE "Y"                        TO LSG-PATH-OVERRIDE.

           MOVE 0 TO SAVE-LS-SEQNO.

       TRANSFER-LSG-REC-NEXT.
           MOVE EXT-FILPATH-BASE           TO LSG-ALL-BASE.
           MOVE FROM-MACHINE                 TO LSG-ALL-MACHINE.
           MOVE FROM-DATA-PATH               TO LSG-ALL-DATA.
           MOVE LSG-ALL-PATH               TO LSG-PATH.
           MOVE "Y"                        TO LSG-PATH-OVERRIDE.

           PERFORM OPEN-LSG1-FILE.

           MOVE LSG-PATH-OWNBR TO LSG-BRNO
                                  QLSG1-WBEG-BRNO
                                  QLSG1-WEND-BRNO.
           MOVE PD-ACCTNO      TO LSG-ACCTNO
                                  QLSG1-WBEG-ACCTNO
                                  QLSG1-WEND-ACCTNO.
           MOVE SAVE-LS-SEQNO  TO LSG-SEQNO
                                  QLSG1-WBEG-SEQNO.
           MOVE ALL "9"        TO QLSG1-WEND-SEQNO.

           PERFORM START-LSG1-FILE.
           PERFORM READ-LSG1-FILE-NEXT.
           PERFORM CLOSE-LSG1-FILE.
           IF IO-BAD OR (LSG-BRNO NOT = LSG-PATH-OWNBR) OR
                         (LSG-ACCTNO NOT = PD-ACCTNO)
              GO TO TRANSFER-LSG-REC-EXIT.

           MOVE LSG-SEQNO TO SAVE-LS-SEQNO.

      * WRITE TRAILER RECORD INTO TO->LS-FILE:
           MOVE EXT-FILPATH-BASE           TO LSG-ALL-BASE.
           MOVE TO-MACHINE                 TO LSG-ALL-MACHINE
           MOVE TO-DATA-PATH               TO LSG-ALL-DATA
           MOVE LSG-ALL-PATH               TO LSG-PATH.
           MOVE "Y"                        TO LSG-PATH-OVERRIDE.

           PERFORM OPEN-LSG1-FILE.
           MOVE TO-BR         TO LSG-BRNO.
           MOVE TO-LN-ACCTNO  TO LSG-ACCTNO.
           MOVE SAVE-LS-SEQNO TO LSG-SEQNO.
           PERFORM WRITE-LSG1-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

           PERFORM CLOSE-LSG1-FILE.
           ADD 1 TO SAVE-LS-SEQNO.
           GO TO TRANSFER-LSG-REC-NEXT.

       TRANSFER-LSG-REC-EXIT.
           EXIT.

      *************************************************
      *    TRANSFER JUDGMENT TRAILERS TO TO->LS-FILE
      *************************************************
       TRANSFER-LSJ-REC SECTION.

           MOVE " "                        TO ERR-FG.
           MOVE EXT-FILPATH-BASE           TO LSJ-ALL-BASE.
           MOVE FROM-MACHINE                 TO LSJ-ALL-MACHINE.
           MOVE FROM-DATA-PATH               TO LSJ-ALL-DATA.
           MOVE LSJ-ALL-PATH               TO LSJ-PATH.
           MOVE "Y"                        TO LSJ-PATH-OVERRIDE.

           MOVE 0 TO SAVE-LS-SEQNO.

       TRANSFER-LSJ-REC-NEXT.
           MOVE EXT-FILPATH-BASE           TO LSJ-ALL-BASE.
           MOVE FROM-MACHINE                 TO LSJ-ALL-MACHINE.
           MOVE FROM-DATA-PATH               TO LSJ-ALL-DATA.
           MOVE LSJ-ALL-PATH               TO LSJ-PATH.
           MOVE "Y"                        TO LSJ-PATH-OVERRIDE.

           PERFORM OPEN-LSJ1-FILE.

           MOVE LSJ-PATH-OWNBR TO LSJ-BRNO
                                  QLSJ1-WBEG-BRNO
                                  QLSJ1-WEND-BRNO.
           MOVE PD-ACCTNO      TO LSJ-ACCTNO
                                  QLSJ1-WBEG-ACCTNO
                                  QLSJ1-WEND-ACCTNO.
           MOVE SAVE-LS-SEQNO  TO LSJ-SEQNO
                                  QLSJ1-WBEG-SEQNO.
           MOVE ALL "9"        TO QLSJ1-WEND-SEQNO.

           PERFORM START-LSJ1-FILE.

           PERFORM READ-LSJ1-FILE-NEXT.
           PERFORM CLOSE-LSJ1-FILE.
           IF IO-BAD OR (LSJ-BRNO  NOT = LSJ-PATH-OWNBR) OR  
                         (LSJ-ACCTNO NOT = PD-ACCTNO)
      *       UNLOCK LSJ-FILE
              GO TO TRANSFER-LSJ-REC-EXIT.

           MOVE LSJ-SEQNO TO SAVE-LS-SEQNO.

      * WRITE TRAILER RECORD INTO TO->LS-FILE:
           MOVE EXT-FILPATH-BASE           TO LSJ-ALL-BASE.
           MOVE TO-MACHINE                 TO LSJ-ALL-MACHINE
           MOVE TO-DATA-PATH               TO LSJ-ALL-DATA
           MOVE LSJ-ALL-PATH               TO LSJ-PATH.
           MOVE "Y"                        TO LSJ-PATH-OVERRIDE.

           PERFORM OPEN-LSJ1-FILE.
           MOVE TO-BR         TO LSJ-BRNO.
           MOVE TO-LN-ACCTNO  TO LSJ-ACCTNO.
           MOVE SAVE-LS-SEQNO TO LSJ-SEQNO.
           PERFORM WRITE-LSJ1-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

           PERFORM CLOSE-LSJ1-FILE.
           ADD 1 TO SAVE-LS-SEQNO.
           GO TO TRANSFER-LSJ-REC-NEXT.

       TRANSFER-LSJ-REC-EXIT.
           EXIT.

      *************************************************
      *    TRANSFER LEGAL TRAILERS TO TO->LS-FILE
      *************************************************
       TRANSFER-LSL-REC SECTION.

           MOVE " "                        TO ERR-FG.
           MOVE EXT-FILPATH-BASE           TO LSL-ALL-BASE.
           MOVE FROM-MACHINE                 TO LSL-ALL-MACHINE.
           MOVE FROM-DATA-PATH               TO LSL-ALL-DATA.
           MOVE LSL-ALL-PATH               TO LSL-PATH.
           MOVE "Y"                        TO LSL-PATH-OVERRIDE.

           MOVE 0 TO SAVE-LS-SEQNO.

       TRANSFER-LSL-REC-NEXT.
           MOVE EXT-FILPATH-BASE           TO LSL-ALL-BASE.
           MOVE FROM-MACHINE                 TO LSL-ALL-MACHINE.
           MOVE FROM-DATA-PATH               TO LSL-ALL-DATA.
           MOVE LSL-ALL-PATH               TO LSL-PATH.
           MOVE "Y"                        TO LSL-PATH-OVERRIDE.

           PERFORM OPEN-LSL1-FILE.

           MOVE LSL-PATH-OWNBR TO LSL-BRNO
                                  QLSL1-WBEG-BRNO
                                  QLSL1-WEND-BRNO.
           MOVE PD-ACCTNO      TO LSL-ACCTNO
                                  QLSL1-WBEG-ACCTNO
                                  QLSL1-WEND-ACCTNO.
           MOVE SAVE-LS-SEQNO  TO LSL-SEQNO
                                  QLSL1-WBEG-SEQNO.
           MOVE ALL "9"        TO QLSL1-WEND-SEQNO.

           PERFORM START-LSL1-FILE.

           PERFORM READ-LSL1-FILE-NEXT.
           PERFORM CLOSE-LSL1-FILE.
           IF IO-BAD OR (LSL-BRNO NOT = LSL-PATH-OWNBR)  OR 
                         (LSL-ACCTNO NOT = PD-ACCTNO)
      *       UNLOCK LSL-FILE
              GO TO TRANSFER-LSL-REC-EXIT.

           MOVE LSL-SEQNO TO SAVE-LS-SEQNO.

      * WRITE TRAILER RECORD INTO TO->LS-FILE:
           MOVE EXT-FILPATH-BASE           TO LSL-ALL-BASE.
           MOVE TO-MACHINE                 TO LSL-ALL-MACHINE
           MOVE TO-DATA-PATH               TO LSL-ALL-DATA
           MOVE LSL-ALL-PATH               TO LSL-PATH.
           MOVE "Y"                        TO LSL-PATH-OVERRIDE.

           PERFORM OPEN-LSL1-FILE.
           MOVE TO-BR         TO LSL-BRNO.
           MOVE TO-LN-ACCTNO  TO LSL-ACCTNO.
           MOVE SAVE-LS-SEQNO TO LSL-SEQNO.
           PERFORM WRITE-LSL1-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

           PERFORM CLOSE-LSL1-FILE.
           ADD 1 TO SAVE-LS-SEQNO.
           GO TO TRANSFER-LSL-REC-NEXT.

       TRANSFER-LSL-REC-EXIT.
           EXIT.

      *************************************************
      *    TRANSFER MISC TRAILERS TO TO->LS-FILE
      *************************************************
       TRANSFER-LSM-REC SECTION.

           MOVE " "                        TO ERR-FG.
           MOVE EXT-FILPATH-BASE           TO LSM-ALL-BASE.
           MOVE FROM-MACHINE               TO LSM-ALL-MACHINE.
           MOVE FROM-DATA-PATH             TO LSM-ALL-DATA.
           MOVE LSM-ALL-PATH               TO LSM-PATH.
           MOVE "Y"                        TO LSM-PATH-OVERRIDE.

           MOVE 0 TO SAVE-LS-SEQNO.

       TRANSFER-LSM-REC-NEXT.
           MOVE EXT-FILPATH-BASE           TO LSM-ALL-BASE.
           MOVE FROM-MACHINE                 TO LSM-ALL-MACHINE.
           MOVE FROM-DATA-PATH               TO LSM-ALL-DATA.
           MOVE LSM-ALL-PATH               TO LSM-PATH.
           MOVE "Y"                        TO LSM-PATH-OVERRIDE.

           PERFORM OPEN-LSM1-FILE.

           MOVE LSM-PATH-OWNBR TO LSM-BRNO
                                  QLSM1-WBEG-BRNO
                                  QLSM1-WEND-BRNO.
           MOVE PD-ACCTNO      TO LSM-ACCTNO
                                  QLSM1-WBEG-ACCTNO
                                  QLSM1-WEND-ACCTNO.
           MOVE SAVE-LS-SEQNO  TO LSM-SEQNO
                                  QLSM1-WBEG-SEQNO.
           MOVE ALL "9"        TO QLSM1-WEND-SEQNO.

           PERFORM START-LSM1-FILE.

           PERFORM READ-LSM1-FILE-NEXT.
           PERFORM CLOSE-LSM1-FILE.
           IF IO-BAD OR ( LSM-BRNO NOT = LSM-PATH-OWNBR)              
                      OR (LSM-ACCTNO NOT = PD-ACCTNO)
      *       UNLOCK LSM-FILE
              GO TO TRANSFER-LSM-REC-EXIT.

           MOVE LSM-SEQNO TO SAVE-LS-SEQNO.

      * WRITE TRAILER RECORD INTO TO->LS-FILE:
           MOVE EXT-FILPATH-BASE           TO LSM-ALL-BASE.
           MOVE TO-MACHINE                 TO LSM-ALL-MACHINE
           MOVE TO-DATA-PATH               TO LSM-ALL-DATA
           MOVE LSM-ALL-PATH               TO LSM-PATH.
           MOVE "Y"                        TO LSM-PATH-OVERRIDE.

           PERFORM OPEN-LSM1-FILE.
           MOVE TO-BR         TO LSM-BRNO.
           MOVE TO-LN-ACCTNO  TO LSM-ACCTNO.
           MOVE SAVE-LS-SEQNO TO LSM-SEQNO.
           PERFORM WRITE-LSM1-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

           PERFORM CLOSE-LSM1-FILE.
           ADD 1 TO SAVE-LS-SEQNO.
           GO TO TRANSFER-LSM-REC-NEXT.

       TRANSFER-LSM-REC-EXIT.
           EXIT.

      *************************************************
      *    TRANSFER PDTH TRAILERS TO TO->LS-FILE
      *************************************************
       TRANSFER-LSP-REC SECTION.

           MOVE " "                        TO ERR-FG.
           MOVE EXT-FILPATH-BASE           TO LSP-ALL-BASE.
           MOVE FROM-MACHINE                 TO LSP-ALL-MACHINE.
           MOVE FROM-DATA-PATH               TO LSP-ALL-DATA.
           MOVE LSP-ALL-PATH               TO LSP-PATH.
           MOVE "Y"                        TO LSP-PATH-OVERRIDE.

           MOVE 0 TO SAVE-LS-SEQNO.

       TRANSFER-LSP-REC-NEXT.
           MOVE EXT-FILPATH-BASE           TO LSP-ALL-BASE.
           MOVE FROM-MACHINE                 TO LSP-ALL-MACHINE.
           MOVE FROM-DATA-PATH               TO LSP-ALL-DATA.
           MOVE LSP-ALL-PATH               TO LSP-PATH.
           MOVE "Y"                        TO LSP-PATH-OVERRIDE.

           PERFORM OPEN-LSP1-FILE.

           MOVE LSP-PATH-OWNBR TO LSP-BRNO
                                  QLSP1-WBEG-BRNO
                                  QLSP1-WEND-BRNO.
           MOVE PD-ACCTNO      TO LSP-ACCTNO
                                  QLSP1-WBEG-ACCTNO
                                  QLSP1-WEND-ACCTNO.
           MOVE SAVE-LS-SEQNO  TO LSP-SEQNO
                                  QLSP1-WBEG-SEQNO.
           MOVE ALL "9"        TO QLSP1-WEND-SEQNO.

           PERFORM START-LSP1-FILE.

           PERFORM READ-LSP1-FILE-NEXT.
           PERFORM CLOSE-LSP1-FILE.
           IF IO-BAD OR (LSP-BRNO NOT = LSP-PATH-OWNBR) OR 
                         (LSP-ACCTNO NOT = PD-ACCTNO)
      *       UNLOCK LSP-FILE
              GO TO TRANSFER-LSP-REC-EXIT.

           MOVE LSP-SEQNO TO SAVE-LS-SEQNO.

      * WRITE TRAILER RECORD INTO TO->LS-FILE:
           MOVE EXT-FILPATH-BASE           TO LSP-ALL-BASE.
           MOVE TO-MACHINE                 TO LSP-ALL-MACHINE
           MOVE TO-DATA-PATH               TO LSP-ALL-DATA
           MOVE LSP-ALL-PATH               TO LSP-PATH.
           MOVE "Y"                        TO LSP-PATH-OVERRIDE.

           PERFORM OPEN-LSP1-FILE.
           MOVE TO-BR         TO LSP-BRNO.
           MOVE TO-LN-ACCTNO  TO LSP-ACCTNO.
           MOVE SAVE-LS-SEQNO TO LSP-SEQNO.
           PERFORM WRITE-LSP1-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

           PERFORM CLOSE-LSP1-FILE.
           ADD 1 TO SAVE-LS-SEQNO.
           GO TO TRANSFER-LSP-REC-NEXT.

       TRANSFER-LSP-REC-EXIT.
           EXIT.

      *************************************************
      *    TRANSFER RESCHEDULE TRAILERS TO TO->LS-FILE
      *************************************************
       TRANSFER-LSR-REC SECTION.

           MOVE " "                        TO ERR-FG.
           MOVE EXT-FILPATH-BASE           TO LSR-ALL-BASE.
           MOVE FROM-MACHINE                 TO LSR-ALL-MACHINE.
           MOVE FROM-DATA-PATH               TO LSR-ALL-DATA.
           MOVE LSR-ALL-PATH               TO LSR-PATH.
           MOVE "Y"                        TO LSR-PATH-OVERRIDE.

           MOVE 0 TO SAVE-LS-SEQNO.

       TRANSFER-LSR-REC-NEXT.
           MOVE EXT-FILPATH-BASE           TO LSR-ALL-BASE.
           MOVE FROM-MACHINE                 TO LSR-ALL-MACHINE.
           MOVE FROM-DATA-PATH               TO LSR-ALL-DATA.
           MOVE LSR-ALL-PATH               TO LSR-PATH.
           MOVE "Y"                        TO LSR-PATH-OVERRIDE.

           PERFORM OPEN-LSR1-FILE.

           MOVE LSR-PATH-OWNBR TO LSR-BRNO
                                  QLSR1-WBEG-BRNO
                                  QLSR1-WEND-BRNO.
           MOVE PD-ACCTNO      TO LSR-ACCTNO
                                  QLSR1-WBEG-ACCTNO
                                  QLSR1-WEND-ACCTNO.
           MOVE SAVE-LS-SEQNO  TO LSR-SEQNO
                                  QLSR1-WBEG-SEQNO.
           MOVE ALL "9"        TO QLSR1-WEND-SEQNO.

           PERFORM START-LSR1-FILE.

           PERFORM READ-LSR1-FILE-NEXT.
           PERFORM CLOSE-LSR1-FILE.
           IF IO-BAD OR (LSR-BRNO NOT = LSR-PATH-OWNBR) OR  
                         (LSR-ACCTNO NOT = PD-ACCTNO)
              GO TO TRANSFER-LSR-REC-EXIT.

           MOVE LSR-SEQNO TO SAVE-LS-SEQNO.

      * WRITE TRAILER RECORD INTO TO->LS-FILE:
           MOVE EXT-FILPATH-BASE           TO LSR-ALL-BASE.
           MOVE TO-MACHINE                 TO LSR-ALL-MACHINE
           MOVE TO-DATA-PATH               TO LSR-ALL-DATA
           MOVE LSR-ALL-PATH               TO LSR-PATH.
           MOVE "Y"                        TO LSR-PATH-OVERRIDE.

           PERFORM OPEN-LSR1-FILE.
           MOVE TO-BR         TO LSR-BRNO.
           MOVE TO-LN-ACCTNO  TO LSR-ACCTNO.
           MOVE SAVE-LS-SEQNO TO LSR-SEQNO.
           PERFORM WRITE-LSR1-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

           PERFORM CLOSE-LSR1-FILE.
           ADD 1 TO SAVE-LS-SEQNO.
           GO TO TRANSFER-LSR-REC-NEXT.

       TRANSFER-LSR-REC-EXIT.
           EXIT.

      *************************************************
      *    TRANSFER REPO TRAILERS TO TO->LS-FILE
      *************************************************
       TRANSFER-LSX-REC SECTION.

           MOVE " "                        TO ERR-FG.
           MOVE EXT-FILPATH-BASE           TO LSX-ALL-BASE.
           MOVE FROM-MACHINE                 TO LSX-ALL-MACHINE.
           MOVE FROM-DATA-PATH               TO LSX-ALL-DATA.
           MOVE LSX-ALL-PATH               TO LSX-PATH.
           MOVE "Y"                        TO LSX-PATH-OVERRIDE.

           MOVE 0 TO SAVE-LS-SEQNO.

       TRANSFER-LSX-REC-NEXT.
           MOVE EXT-FILPATH-BASE           TO LSX-ALL-BASE.
           MOVE FROM-MACHINE                 TO LSX-ALL-MACHINE.
           MOVE FROM-DATA-PATH               TO LSX-ALL-DATA.
           MOVE LSX-ALL-PATH               TO LSX-PATH.
           MOVE "Y"                        TO LSX-PATH-OVERRIDE.

           PERFORM OPEN-LSX1-FILE.

           MOVE LSX-PATH-OWNBR TO LSX-BRNO
                                  QLSX1-WBEG-BRNO
                                  QLSX1-WEND-BRNO.
           MOVE PD-ACCTNO      TO LSX-ACCTNO
                                  QLSX1-WBEG-ACCTNO
                                  QLSX1-WEND-ACCTNO.
           MOVE SAVE-LS-SEQNO  TO LSX-SEQNO
                                  QLSX1-WBEG-SEQNO.
           MOVE ALL "9"        TO QLSX1-WEND-SEQNO.

           PERFORM START-LSX1-FILE.

           PERFORM READ-LSX1-FILE-NEXT.
           PERFORM CLOSE-LSX1-FILE.
           IF IO-BAD OR (LSX-BRNO NOT = LSX-PATH-OWNBR) OR   
                         (LSX-ACCTNO NOT = PD-ACCTNO)
              GO TO TRANSFER-LSX-REC-EXIT.

           MOVE LSX-SEQNO TO SAVE-LS-SEQNO.

      * WRITE TRAILER RECORD INTO TO->LS-FILE:
           MOVE EXT-FILPATH-BASE           TO LSX-ALL-BASE.
           MOVE TO-MACHINE                 TO LSX-ALL-MACHINE
           MOVE TO-DATA-PATH               TO LSX-ALL-DATA
           MOVE LSX-ALL-PATH               TO LSX-PATH.
           MOVE "Y"                        TO LSX-PATH-OVERRIDE.

           PERFORM OPEN-LSX1-FILE.
           MOVE TO-BR         TO LSX-BRNO.
           MOVE TO-LN-ACCTNO  TO LSX-ACCTNO.
           MOVE SAVE-LS-SEQNO TO LSX-SEQNO.
           PERFORM WRITE-LSX1-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

           PERFORM CLOSE-LSX1-FILE.
           ADD 1 TO SAVE-LS-SEQNO.
           GO TO TRANSFER-LSX-REC-NEXT.

       TRANSFER-LSX-REC-EXIT.
           EXIT.

      *************************************************
       TRANSFER-NO-REC SECTION.
      *********************************************
           MOVE " " TO ERR-FG.

           MOVE EXT-FILPATH-BASE           TO NO-ALL-BASE.
           MOVE FROM-DATA-PATH             TO NO-ALL-DATA.
           MOVE FROM-MACHINE               TO NO-ALL-MACHINE.
           MOVE NO-ALL-PATH                TO NO-PATH.
           MOVE "Y"                        TO NO-PATH-OVERRIDE.

           PERFORM OPEN-NO1-FILE.
           MOVE NO-PATH-OWNBR TO NO-BRNO.
           MOVE PD-ACCTNO     TO NO-ACCTNO.
           PERFORM READ-NO1-FILE.
           PERFORM CLOSE-NO1-FILE.
           IF IO-BAD
              GO TO TRANSFER-NO-REC-EXIT.

           MOVE EXT-FILPATH-BASE           TO NO-ALL-BASE.
           MOVE TO-DATA-PATH               TO NO-ALL-DATA.
           MOVE TO-MACHINE                 TO NO-ALL-MACHINE.
           MOVE NO-ALL-PATH                TO NO-PATH.
           MOVE "Y"                        TO NO-PATH-OVERRIDE.

           PERFORM OPEN-NO1-FILE.

           MOVE TO-BR        TO NO-BRNO.
           MOVE TO-LN-ACCTNO TO NO-ACCTNO.
           PERFORM WRITE-NO1-FILE.
           IF IO-BAD
               GO TO IO-ERROR.

           PERFORM CLOSE-NO1-FILE.

       TRANSFER-NO-REC-EXIT.
           EXIT.

      *************************************************
      *    TRANSFER LOAN MEMO TO TO->LM-FILE
      *************************************************
       TRANSFER-LM-REC SECTION.

           MOVE " " TO ERR-FG.
           MOVE EXT-FILPATH-BASE TO LM-ALL-BASE.
           MOVE FROM-DATA-PATH   TO LM-ALL-DATA.
           MOVE FROM-MACHINE     TO LM-ALL-MACHINE.
           MOVE LM-ALL-PATH      TO LM-PATH.
           MOVE "Y"              TO LM-PATH-OVERRIDE.

           MOVE 0 TO SAVE-LM-DATE.
           MOVE 0 TO SAVE-LM-SEQ.

       TRANSFER-LM-REC-NEXT.
           MOVE EXT-FILPATH-BASE TO LM-ALL-BASE.
           MOVE FROM-DATA-PATH   TO LM-ALL-DATA.
           MOVE FROM-MACHINE     TO LM-ALL-MACHINE.
           MOVE LM-ALL-PATH      TO LM-PATH.
           MOVE "Y"              TO LM-PATH-OVERRIDE.
           PERFORM OPEN-LM1-FILE.

           MOVE LM-PATH-OWNBR TO LM-BRNO.
           MOVE PD-ACCTNO     TO LM-ACCTNO.
           MOVE SAVE-LM-DATE  TO LM-DATE.
           MOVE SAVE-LM-SEQ   TO LM-SEQ.

           PERFORM START-LM1-FILE-SCAN.

           PERFORM READ-LM1-FILE-NEXT.
           PERFORM CLOSE-LM1-FILE.
           IF (IO-FG NOT = 0) OR (LM-BRNO NOT = LM-PATH-OWNBR)
                               OR (LM-ACCTNO NOT = PD-ACCTNO)
              GO TO TRANSFER-LM-REC-EXIT.

           MOVE LM-DATE TO SAVE-LM-DATE.
           MOVE LM-SEQ  TO SAVE-LM-SEQ.

      * WRITE LOAN MEMO RECORD INTO TO->LM-FILE:
           MOVE EXT-FILPATH-BASE TO LM-ALL-BASE.
           MOVE TO-DATA-PATH     TO LM-ALL-DATA.
           MOVE TO-MACHINE       TO LM-ALL-MACHINE.
           MOVE LM-ALL-PATH      TO LM-PATH.
           MOVE "Y"              TO LM-PATH-OVERRIDE.
           PERFORM OPEN-LM1-FILE.

           MOVE TO-BR        TO LM-BRNO.
           MOVE TO-LN-ACCTNO TO LM-ACCTNO.
           MOVE SAVE-LM-DATE TO LM-DATE.
           MOVE SAVE-LM-SEQ  TO LM-SEQ.
           PERFORM WRITE-LM1-FILE.
           IF IO-BAD
              PERFORM IO-ERROR
              PERFORM CLOSE-LM1-FILE
              GO TO TRANSFER-LM-REC-EXIT.

           PERFORM CLOSE-LM1-FILE.
      *----------------------------------------
      *CORRECTED TO AVOID CYCLING BACK TO SAME DATE WITH SEQ. NO. 1 AGAIN
      *             CAUSING A WRITE 22 ERROR:
      *JTG ADD 1 TO SAVE-LM-SEQ.

           IF (SAVE-LM-SEQ = 999 AND SAVE-LM-DATE = 99999999)
              GO TO TRANSFER-LM-REC-EXIT.

           IF SAVE-LM-SEQ = 999
              ADD 1 TO SAVE-LM-DATE
              MOVE 0 TO SAVE-LM-SEQ
           ELSE
              ADD 1 TO SAVE-LM-SEQ.
      *----------------------------------------
           GO TO TRANSFER-LM-REC-NEXT.

       TRANSFER-LM-REC-EXIT.
           EXIT.

      *************************************************
      *    TRANSFER LOAN BORROWER TO TO->BW-FILE
      *************************************************
       TRANSFER-BW-REC SECTION.

           MOVE " " TO ERR-FG.
           MOVE EXT-FILPATH-BASE TO BW-ALL-BASE.
           MOVE FROM-DATA-PATH   TO BW-ALL-DATA.
           MOVE FROM-MACHINE     TO BW-ALL-MACHINE.
           MOVE BW-ALL-PATH      TO BW-PATH.
           MOVE "Y"              TO BW-PATH-OVERRIDE.
           PERFORM OPEN-BW1-FILE.
           MOVE PD-OWNBR         TO BW-OWNBR.
           MOVE PD-SSNO(BW-SUB)  TO BW-SSNO.
           PERFORM READ-BW1-FILE.
           IF IO-BAD
              PERFORM CLOSE-BW1-FILE
              GO TO TRANSFER-BW-REC-EXIT.

           PERFORM CLOSE-BW1-FILE.

      * WRITE BORROWER RECORD INTO TO->BW-FILE:
           MOVE EXT-FILPATH-BASE TO BW-ALL-BASE.
           MOVE TO-DATA-PATH     TO BW-ALL-DATA.
           MOVE TO-MACHINE       TO BW-ALL-MACHINE.
           MOVE BW-ALL-PATH      TO BW-PATH.
           MOVE "Y"              TO BW-PATH-OVERRIDE.
           PERFORM OPEN-BW1-FILE.

           MOVE TO-BR        TO BW-OWNBR.
           IF PD-CURBAL = 0
              MOVE 0         TO BW-LOANS
           ELSE
              MOVE 1         TO BW-LOANS.
           MOVE PD-YTDINTPD  TO BW-INTYTD.
           MOVE PD-LYRINTPD  TO BW-INTLYR.
           MOVE 0            TO BW-RE-INTYTD
                                BW-RE-INTLYR.
           IF PD-SMALLOAN = "Y"
              MOVE PD-CURBAL TO BW-LIASMLN
              MOVE 0         TO BW-LIAOTH
           ELSE
              MOVE 0         TO BW-LIAOTH
              MOVE PD-CURBAL TO BW-LIAOTH.
           MOVE 0            TO BW-LASTPODATE
                                BW-LAST-FORM-DATE.
           MOVE 0            TO BW-LAST-FORM-NO.
           PERFORM WRITE-BW1-FILE.
           IF IO-GOOD
              GO TO TRANSFER-BW-REC-EXIT.

      * MUST ALREADY EXIST, UPDATE FIELDS
           MOVE TO-BR           TO BW-OWNBR.
           MOVE PD-SSNO(BW-SUB) TO BW-SSNO.
           PERFORM READ-BW1-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

           IF (PD-CURBAL NOT = 0)
              ADD 1 TO BW-LOANS.

           ADD PD-YTDINTPD TO BW-INTYTD.
           ADD PD-LYRINTPD TO BW-INTLYR.
           ADD 0 TO BW-RE-INTYTD.
           ADD 0 TO BW-RE-INTLYR.
           IF PD-SMALLOAN = "Y"
              ADD PD-CURBAL TO BW-LIASMLN
              ADD 0 TO BW-LIAOTH
           ELSE
              ADD 0 TO BW-LIASMLN
              ADD PD-CURBAL TO BW-LIAOTH.

           PERFORM REWRITE-BW1-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

       TRANSFER-BW-REC-EXIT.
           PERFORM CLOSE-BW1-FILE.

      *************************************************
      *    TRANSFER BORROWER TRAILER TO TO->BX-FILE
      *************************************************
       TRANSFER-BX-REC SECTION.

           MOVE EXT-FILPATH-BASE TO BX-ALL-BASE.
           MOVE FROM-DATA-PATH TO BX-ALL-DATA.
           MOVE FROM-MACHINE TO BX-ALL-MACHINE.
           MOVE BX-ALL-PATH TO BX-PATH.
           MOVE "Y" TO BX-PATH-OVERRIDE.
           PERFORM OPEN-BX1-FILE.

           MOVE PD-OWNBR        TO BX-BRNO.
           MOVE PD-SSNO(BW-SUB) TO BX-SSNO.
           PERFORM READ-BX1-FILE.
           IF IO-BAD
              PERFORM CLOSE-BX1-FILE
              GO TO TRANSFER-BX-REC-EXIT.

           PERFORM CLOSE-BX1-FILE.

      * WRITE BORROWER RECORD INTO TO->BX-FILE:
           MOVE EXT-FILPATH-BASE TO BX-ALL-BASE.
           MOVE TO-DATA-PATH TO BX-ALL-DATA.
           MOVE TO-MACHINE TO BX-ALL-MACHINE.
           MOVE BX-ALL-PATH TO BX-PATH.
           MOVE "Y" TO BX-PATH-OVERRIDE.
           PERFORM OPEN-BX1-FILE.

           MOVE TO-BR TO BX-BRNO.
           PERFORM WRITE-BX1-FILE.

       TRANSFER-BX-REC-EXIT.
           PERFORM CLOSE-BX1-FILE.

      *************************************************
      *    TRANSFER BORROWER MEMO TO TO->BM-FILE
      *************************************************
       TRANSFER-BM-REC SECTION.

      * FIRST REMOVE ALL THE CURRENT BORROWER MEMOS IN THE "TO" BRANCH
           PERFORM REMOVE-TO-BM-MEMOS-FIRST.

           MOVE " " TO ERR-FG.
           MOVE EXT-FILPATH-BASE TO BM-ALL-BASE.
           MOVE FROM-DATA-PATH TO BM-ALL-DATA.
           MOVE FROM-MACHINE TO BM-ALL-MACHINE.
           MOVE BM-ALL-PATH TO BM-PATH.
           MOVE "Y" TO BM-PATH-OVERRIDE.

           MOVE 0 TO SAVE-BM-DATE.
           MOVE 0 TO SAVE-FROM-BM-SEQ.

       TRANSFER-BM-REC-NEXT.
           MOVE EXT-FILPATH-BASE TO BM-ALL-BASE.
           MOVE FROM-DATA-PATH   TO BM-ALL-DATA.
           MOVE FROM-MACHINE     TO BM-ALL-MACHINE.
           MOVE BM-ALL-PATH      TO BM-PATH.
           MOVE "Y"              TO BM-PATH-OVERRIDE.
           PERFORM OPEN-BM1-FILE.

           MOVE BM-PATH-OWNBR    TO BM-BRNO.
           MOVE PD-SSNO(BW-SUB)  TO BM-SSNO.
           MOVE SAVE-BM-DATE     TO BM-DATE.
           MOVE SAVE-FROM-BM-SEQ TO BM-SEQ.

           PERFORM START-BM1-FILE-SCAN.

           PERFORM READ-BM1-FILE-NEXT.
           PERFORM CLOSE-BM1-FILE.
           IF IO-BAD OR (BM-BRNO NOT = PD-OWNBR) OR
                         (BM-SSNO NOT = PD-SSNO(BW-SUB))
              GO TO TRANSFER-BM-REC-EXIT.

           MOVE BM-REC  TO HOLD-BM-REC.

           MOVE BM-DATE TO SAVE-BM-DATE.
           MOVE BM-SEQ  TO SAVE-FROM-BM-SEQ
                           SAVE-TO-BM-SEQ.

      * WRITE BORROWER MEMO RECORD INTO TO->BM-FILE:
           MOVE EXT-FILPATH-BASE TO BM-ALL-BASE.
           MOVE TO-DATA-PATH TO BM-ALL-DATA.
           MOVE TO-MACHINE TO BM-ALL-MACHINE.
           MOVE BM-ALL-PATH TO BM-PATH.
           MOVE "Y" TO BM-PATH-OVERRIDE.
           PERFORM OPEN-BM1-FILE.

           MOVE HOLD-BM-REC TO BM-REC.
           MOVE TO-BR       TO BM-BRNO.
       TRANSFER-BM-REC-TO-AGAIN.
           PERFORM READ-BM1-FILE.
      * DIDNT FIND IT IN THE TARGET, SO WRITE IT
           IF IO-BAD
              MOVE HOLD-BM-REC    TO BM-REC
              MOVE TO-BR          TO BM-BRNO
              MOVE SAVE-TO-BM-SEQ TO BM-SEQ
              PERFORM WRITE-BM1-FILE
              PERFORM CLOSE-BM1-FILE
              IF (SAVE-FROM-BM-SEQ = 999 AND SAVE-BM-DATE = 99999999)
                 GO TO TRANSFER-BM-REC-EXIT
              END-IF
              IF SAVE-FROM-BM-SEQ = 999
                 ADD 1 TO SAVE-BM-DATE
                 MOVE 0 TO SAVE-FROM-BM-SEQ
                 GO TO TRANSFER-BM-REC-NEXT
              ELSE
                 ADD 1 TO SAVE-FROM-BM-SEQ
                 GO TO TRANSFER-BM-REC-NEXT.

      * IF IO-GOOD, CHECK TO SEE IF WE HAVE FOUND THE SAME RECORD
      * (MAYBE BORROWER HAS HAD OTHER ACCOUNTS TRANSFERED TO THIS BRANCH);
      * IF SO,  DON'T DUPLICATE THE SAME MEMO!      6/10/02   BARB
           IF BM-REC = HOLD-BM-REC
              PERFORM CLOSE-BM1-FILE
              IF (SAVE-FROM-BM-SEQ = 999 AND SAVE-BM-DATE = 99999999)
                 GO TO TRANSFER-BM-REC-EXIT
              END-IF
              IF SAVE-FROM-BM-SEQ = 999
                 ADD 1 TO SAVE-BM-DATE
                 MOVE 0 TO SAVE-FROM-BM-SEQ
                 GO TO TRANSFER-BM-REC-NEXT
              ELSE
                 ADD 1 TO SAVE-FROM-BM-SEQ
                 GO TO TRANSFER-BM-REC-NEXT.
      * NOW YOU HAVE A SOURCE MEMO THAT DOESNT MATCH THE SAME
      * KEY IN THE TARGET

           IF BM-SEQ < 999
              MOVE BM-SEQ TO SAVE-TO-BM-SEQ
              MOVE HOLD-BM-REC TO BM-REC
              MOVE SAVE-TO-BM-SEQ TO BM-SEQ
              ADD 1 TO SAVE-TO-BM-SEQ
                       BM-SEQ
              GO TO TRANSFER-BM-REC-TO-AGAIN
           ELSE
              IF (BM-SEQ = 999 AND SAVE-BM-DATE = 99999999)
                 GO TO TRANSFER-BM-REC-EXIT
              END-IF
              ADD 1  TO SAVE-BM-DATE
              MOVE 0 TO SAVE-FROM-BM-SEQ
              PERFORM CLOSE-BM1-FILE
              GO TO TRANSFER-BM-REC-NEXT.

       TRANSFER-BM-REC-EXIT.
           EXIT.

      *********************************
       REMOVE-TO-BM-MEMOS-FIRST SECTION.
      *********************************

           MOVE EXT-FILPATH-BASE TO BM-ALL-BASE.
           MOVE TO-DATA-PATH     TO BM-ALL-DATA.
           MOVE TO-MACHINE       TO BM-ALL-MACHINE.
           MOVE BM-ALL-PATH      TO BM-PATH.
           MOVE "Y" TO BM-PATH-OVERRIDE.
           PERFORM OPEN-BM1-FILE.

           MOVE TO-BR            TO BM-BRNO
                                    QBM1-WBEG-BRNO
                                    QBM1-WEND-BRNO.
           MOVE PD-SSNO(BW-SUB) TO BM-SSNO
                                   QBM1-WBEG-SSNO
                                   QBM1-WEND-SSNO.
           MOVE 0               TO BM-DATE
                                   QBM1-WBEG-DATE-REV.
           MOVE 0               TO BM-SEQ
                                   QBM1-WBEG-SEQ.
           MOVE ALL "9"         TO QLM1-WEND-DATE-REV
                                   QLM1-WEND-SEQ.

           PERFORM START-BM1-FILE.

       REMOVE-TO-BM-REC.

           PERFORM READ-BM1-FILE-NEXT.
           IF (IO-FG NOT = 0) OR (BM-BRNO NOT = TO-BR)
              GO TO EXIT-REMOVE-TO-BM-MEMOS.

           IF BM-SSNO NOT = PD-SSNO(BW-SUB)
              GO TO EXIT-REMOVE-TO-BM-MEMOS.

           PERFORM DELETE-BM1-FILE.

           GO TO REMOVE-TO-BM-REC.

       EXIT-REMOVE-TO-BM-MEMOS.
           PERFORM CLOSE-BM1-FILE.


      *************************************************
      *    CREATE TO->BM-FILE
      *************************************************
       CREATE-TOBM-FILE SECTION.
           MOVE " " TO ERR-FG.

           MOVE EXT-FILPATH-BASE TO BM-ALL-BASE.
           MOVE TO-DATA-PATH     TO BM-ALL-DATA.
           MOVE TO-MACHINE       TO BM-ALL-MACHINE.
           MOVE BM-ALL-PATH      TO BM-PATH.
           MOVE "Y"              TO BM-PATH-OVERRIDE.
           PERFORM OPEN-BM1-FILE.

           MOVE TO-BR           TO BM-BRNO.
           MOVE PD-SSNO(BW-SUB) TO BM-SSNO.
           SUBTRACT TRANS-DATE FROM 99999999 GIVING BM-DATE.
           MOVE 0 TO BM-SEQ.
       CREATE-TOBM-FILE-NEXT.
           ADD 1 TO BM-SEQ.
           PERFORM READ-BM1-FILE.
           IF IO-GOOD
      *       UNLOCK BM-FILE
              IF BM-SEQ < 999
                 GO TO CREATE-TOBM-FILE-NEXT
              ELSE
                 PERFORM CLOSE-BM1-FILE
                 GO TO CREATE-TOBM-FILE-EXIT.

           MOVE TO-BR  TO BM-BRNO.
           MOVE "9"    TO BM-MEMO-TYPE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *KC  MOVE SPACES TO BM-MEMO.
      * YOU DO NOT KNOW IF THE ACCOUNT # IS GOING TO CHANGE AT THIS POINT
      *KC  STRING "TFR FR BR " FROM-BRANCH " TO BR " TO-BR
      *KC            DELIMITED BY SIZE INTO BM-MEMO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * YOU DO NOT KNOW IF THE ACCOUNT # IS GOING TO CHANGE AT THIS POINT
           MOVE SPACES                     TO TFRMEMO-MEMO.
           MOVE "TFR FR BR"                TO TFRMEMO-PRIMARY.
           MOVE FROM-BRANCH                TO TFRMEMO-BRNO1.
           MOVE "TO BR"                    TO TFRMEMO-SECONDARY.
           MOVE TO-BR                      TO TFRMEMO-BRNO2.
           MOVE TFRMEMO-MEMO               TO BM-MEMO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           PERFORM WRITE-BM1-FILE.
           PERFORM CLOSE-BM1-FILE.

       CREATE-TOBM-FILE-EXIT.
           EXIT.

      *************************************************
      *    CLOSE OUT FROM->LN-FILE
      *************************************************
       CLOSE-FRLN-FILE SECTION.
           MOVE " " TO ERR-FG.

           MOVE EXT-FILPATH-BASE TO LN-ALL-BASE.
           MOVE FROM-DATA-PATH TO LN-ALL-DATA.
           MOVE FROM-MACHINE TO LN-ALL-MACHINE.
           MOVE LN-ALL-PATH TO LN-PATH.
           MOVE "Y" TO LN-PATH-OVERRIDE.

           PERFORM OPEN-LN1-FILE.
           MOVE FROM-BRANCH TO LN-OWNBR.
           MOVE PD-ACCTNO   TO LN-ACCTNO.
           PERFORM READ-LN1-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

           MOVE 0 TO LN-OTHBAL
                     LN-OT2BAL
                     LN-LCBAL
                     LN-INTBAL
                     LN-CURBAL.

           MOVE 0 TO LN-PURBAL.

           COMPUTE LN-ANTICADJ(1) = LN-ANTICERN(1) - LN-ACCERN(1).
           COMPUTE LN-ANTICADJ(2) = LN-ANTICERN(2) - LN-ACCERN(2).
           COMPUTE LN-ANTICADJ(3) = LN-ANTICERN(3) - LN-ACCERN(3).
           COMPUTE LN-ANTICADJ(4) = LN-ANTICERN(4) - LN-ACCERN(4).
           COMPUTE LN-ANTICADJ(5) = LN-ANTICERN(5) - LN-ACCERN(5).
           COMPUTE LN-ANTICADJ(6) = LN-ANTICERN(6) - LN-ACCERN(6).

           MOVE "BT" TO LN-POCD LN-PO-TRCD.
      * DO NOT OVERWRITE THE ORIGINAL PAIDOUT DATE OR THEY WILL START
      * REPORTING IN THE BUREAU AGAIN. BUT CONTINUE REPORTING "BT" AS THE
      * POCD SO THEY GET THE CORRECT STATUS SENT TO THE CREDIT BUREAU
           IF LN-POFFDATE = 0
              MOVE TRANS-DATE TO LN-POFFDATE
                                 LN-DATE-PAID-LAST.

           MOVE "Y" TO LN-CL-REBATE  LN-AH-REBATE
                       LN-PP-REBATE  LN-O1-REBATE
                       LN-O2-REBATE  LN-O3-REBATE
                       LN-O4-REBATE  LN-O5-REBATE
                       LN-INT-REBATE LN-SC-REBATE
                       LN-MF-REBATE  LN-DF-REBATE.

           PERFORM SET-FRBR-STATUS-FG.

      * SET 'LN-BI-FG = Y' TO TRIGGER BI-FILE UPDATE:
           MOVE "Y" TO LN-BI-FG.
           PERFORM REWRITE-LN1-FILE.
           PERFORM CLOSE-LN1-FILE.

       CLOSE-FRLN-FILE-EXIT.
           EXIT.

      *************************************************
      *    CLOSE OUT FROM->BW-FILE
      *************************************************
       CLOSE-FRBW-FILE SECTION.

           MOVE " " TO ERR-FG.
           MOVE EXT-FILPATH-BASE TO BW-ALL-BASE.
           MOVE FROM-DATA-PATH   TO BW-ALL-DATA.
           MOVE FROM-MACHINE     TO BW-ALL-MACHINE.
           MOVE BW-ALL-PATH      TO BW-PATH.
           MOVE "Y"              TO BW-PATH-OVERRIDE.

           PERFORM OPEN-BW1-FILE.
           MOVE PD-SSNO(BW-SUB) TO BW-SSNO.
           PERFORM READ-BW1-FILE.
           IF IO-BAD
              PERFORM CLOSE-BW1-FILE
              GO TO CLOSE-FRBW-FILE-EXIT.

      * IF TRANSFERRING PAID OUTS, THE BW-LOANS HAS ALREADY BEEN REDUCED
           IF PD-CURBAL NOT = 0
              SUBTRACT 1 FROM BW-LOANS
              ADD 1 TO BW-PRLOANS.

           SUBTRACT PD-YTDINTPD FROM BW-INTYTD.
           SUBTRACT PD-LYRINTPD FROM BW-INTLYR.
           IF PD-SMALLOAN = "Y"
              SUBTRACT PD-CURBAL FROM BW-LIASMLN
           ELSE
              SUBTRACT PD-CURBAL FROM BW-LIAOTH.
           PERFORM REWRITE-BW1-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

       CLOSE-FRBW-FILE-EXIT.
           PERFORM CLOSE-BW1-FILE.

      *************************************************
      *    CREATE FROM->LP-FILE
      *************************************************
       CREATE-FRLP-FILE SECTION.
           MOVE " " TO ERR-FG.

           MOVE EXT-FILPATH-BASE TO LP-ALL-BASE.
           MOVE FROM-DATA-PATH TO LP-ALL-DATA.
           MOVE FROM-MACHINE TO LP-ALL-MACHINE.
           MOVE LP-ALL-PATH TO LP-PATH.
           MOVE "Y" TO LP-PATH-OVERRIDE.

           PERFORM OPEN-LP1-FILE.

           MOVE LP-PATH-OWNBR TO LP-BRNO
                                 QLP1-WBEG-BRNO
                                 QLP1-WEND-BRNO.
           MOVE PD-ACCTNO     TO LP-ACCTNO
                                 QLP1-WBEG-ACCTNO
                                 QLP1-WEND-ACCTNO.
           MOVE ALL "9"       TO LP-SEQNO
                                 QLP1-WEND-SEQNO.
           MOVE ALL "0"       TO QLP1-WBEG-SEQNO.

           PERFORM START-LP1-FILE-NOT-GREATER.
           PERFORM READ-LP1-FILE-PREVIOUS.
           IF IO-BAD OR (LP-BRNO NOT = LP-PATH-OWNBR)
                      OR (PD-ACCTNO NOT = LP-ACCTNO)
      *       UNLOCK LP-FILE
              MOVE SPACES TO LP-REC
              MOVE LP-PATH-OWNBR TO LP-BRNO
              MOVE PD-ACCTNO     TO LP-ACCTNO
              MOVE 0             TO LP-SEQNO.
       CREATE-FRLP-FILE-NEXT.
           ADD 1 TO LP-SEQNO.
           PERFORM READ-LP1-FILE.
           IF IO-GOOD
      *       UNLOCK LP-FILE
              GO TO CREATE-FRLP-FILE-NEXT.

           MOVE TRANS-DATE TO LP-TRDATE
                              LP-PAYDATE.
           MOVE "BT"       TO LP-TRCD.
           MOVE "T"        TO REF.
           MOVE TO-BR      TO REF-BR.
           MOVE REF        TO LP-REFNO.
           COMPUTE LP-TRAMT = PD-OTHBAL + PD-OT2BAL + PD-INTBAL
                                         + PD-LCBAL + PD-CURBAL.
           MOVE PD-OTHBAL TO LP-APOTH.
           MOVE PD-OT2BAL TO LP-APOT2.
           MOVE PD-INTBAL TO LP-APINT.
           MOVE PD-LCBAL  TO LP-APLC.
           MOVE PD-CURBAL TO LP-APCUR.
           MOVE 0 TO LP-OTHBAL LP-OT2BAL
                     LP-INTBAL LP-LCBAL
                     LP-CURBAL.
           MOVE 0 TO LP-INTDUE.
           MOVE 0 TO LP-DLPROC.
           MOVE 0 TO LP-EARNED(1)
                     LP-EARNED(2)
                     LP-EARNED(3).
           MOVE 0 TO LP-INTPAID.
           MOVE PD-SSNO(1)        TO LP-SSNO.
           MOVE HOLD-LP-PDTH-DATE TO LP-PDTH-DATE.
           MOVE PD-LCPDTH-DATE    TO LP-LCPDTH-DATE.
           MOVE 0 TO LP-TILLNO LP-APINTOWE LP-DLPARVHELD-ADJ.
           MOVE HOLD-LP-IBPC      TO LP-IBPC.
           MOVE " " TO LP-MANUAL-RB.
           MOVE " " TO LP-REV.
           MOVE " " TO LP-STATUSFG.
           MOVE 0 TO LP-NEWSTAT-RATE LP-LCPARTIALS LP-LCPAID.

           PERFORM SET-POSTDATE.
           MOVE POST-USERID TO LP-USERID.
           MOVE POST-TIME TO LP-POSTTIME.
           MOVE POST-DATE TO LP-POSTDATE.
           PERFORM WRITE-LP1-FILE.
           PERFORM CLOSE-LP1-FILE.

       CREATE-FRLP-FILE-EXIT.
           EXIT.

      *************************************************
      *    CREATE FROM->LM-FILE
      *************************************************
       CREATE-FRLM-FILE SECTION.
           MOVE " " TO ERR-FG.

           MOVE EXT-FILPATH-BASE TO LM-ALL-BASE.
           MOVE FROM-DATA-PATH   TO LM-ALL-DATA.
           MOVE FROM-MACHINE     TO LM-ALL-MACHINE.
           MOVE LM-ALL-PATH      TO LM-PATH.
           MOVE "Y"              TO LM-PATH-OVERRIDE.
           PERFORM OPEN-LM1-FILE.

           MOVE LM-PATH-OWNBR TO LM-BRNO.
           MOVE PD-ACCTNO     TO LM-ACCTNO.
           SUBTRACT TRANS-DATE FROM 99999999 GIVING LM-DATE.
           MOVE 0             TO LM-SEQ.

       CREATE-FRLM-FILE-NEXT.
           IF LM-SEQ = 999
              GO TO CREATE-FRLM-FILE-EXIT.

           ADD 1 TO LM-SEQ.
           PERFORM READ-LM1-FILE.
           IF IO-FG = 0
      *       UNLOCK LM-FILE
              GO TO CREATE-FRLM-FILE-NEXT.

           MOVE FROM-BRANCH TO LM-BRNO.
           MOVE "9"         TO LM-MEMO-TYPE.
           MOVE SPACES                     TO TFRMEMO-MEMO.
           MOVE "TFR TO BR"                TO TFRMEMO-PRIMARY.
           MOVE TO-BR                      TO TFRMEMO-BRNO1.
           MOVE "FR BR"                    TO TFRMEMO-SECONDARY.
           MOVE FROM-BRANCH                TO TFRMEMO-BRNO2.
           MOVE ", FOR $"                  TO TFRMEMO-LM-FOR.
           MOVE PD-CURBAL                  TO TFRMEMO-LM-CURBAL.

           IF ( TO-LN-ACCTNO NOT = PD-ACCTNO )
              MOVE "--->"                  TO TFRMEMO-LM-DIRECTION
              MOVE TO-LN-ACCTNO            TO TFRMEMO-LM-ACCTNO.

           MOVE TFRMEMO-MEMO               TO LM-MEMO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           PERFORM WRITE-LM1-FILE.
           IF IO-BAD
              PERFORM IO-ERROR.

       CREATE-FRLM-FILE-EXIT.
           PERFORM CLOSE-LM1-FILE.

      *************************************************
      *    CREATE FROM->BM-FILE
      *************************************************
       CREATE-FRBM-FILE SECTION.
           MOVE " " TO ERR-FG.

           MOVE EXT-FILPATH-BASE TO BM-ALL-BASE.
           MOVE FROM-DATA-PATH   TO BM-ALL-DATA.
           MOVE FROM-MACHINE     TO BM-ALL-MACHINE.
           MOVE BM-ALL-PATH      TO BM-PATH.
           MOVE "Y"              TO BM-PATH-OVERRIDE.
           PERFORM OPEN-BM1-FILE.

           MOVE PD-OWNBR        TO BM-BRNO.
           MOVE PD-SSNO(BW-SUB) TO BM-SSNO.
           SUBTRACT TRANS-DATE FROM 99999999 GIVING BM-DATE.
           MOVE 0 TO BM-SEQ.
       CREATE-FRBM-FILE-NEXT.
           ADD 1 TO BM-SEQ.
           PERFORM READ-BM1-FILE.
           IF IO-GOOD
      *       UNLOCK BM-FILE
              IF BM-SEQ < 999
                 GO TO CREATE-FRBM-FILE-NEXT
              ELSE
                 PERFORM CLOSE-BM1-FILE
                 GO TO CREATE-FRBM-FILE-EXIT.

           MOVE FROM-BRANCH TO BM-BRNO.
           MOVE "9"         TO BM-MEMO-TYPE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *KC  MOVE SPACES      TO MESS BM-MEMO.
      *KC  IF TO-LN-ACCTNO NOT = PD-ACCTNO
      *KC     STRING " ---> " TO-LN-ACCTNO DELIMITED BY SIZE INTO MESS.
      *KC  STRING "TFR TO BR " TO-BR " FR BR " FROM-BRANCH ", RE: "
      *KC           PD-ACCTNO MESS DELIMITED BY SIZE INTO BM-MEMO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE SPACES                     TO TFRMEMO-MEMO.
           MOVE "TFR TO BR"                TO TFRMEMO-PRIMARY.
           MOVE TO-BR                      TO TFRMEMO-BRNO1.
           MOVE "FR BR"                    TO TFRMEMO-SECONDARY.
           MOVE FROM-BRANCH                TO TFRMEMO-BRNO2.
           MOVE ", RE:"                    TO TFRMEMO-BM-RE.
           MOVE PD-ACCTNO                  TO TFRMEMO-BM-ACCTNO1.

           IF ( TO-LN-ACCTNO NOT = PD-ACCTNO )
              MOVE "--->"                  TO TFRMEMO-BM-DIRECTION
              MOVE TO-LN-ACCTNO            TO TFRMEMO-BM-ACCTNO2.

           MOVE TFRMEMO-MEMO               TO BM-MEMO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           PERFORM WRITE-BM1-FILE.
           PERFORM CLOSE-BM1-FILE.

       CREATE-FRBM-FILE-EXIT.
           EXIT.

      *************************************************
      *    CREATE FROM->TR-FILE
      *************************************************
       CREATE-FRTR-FILE SECTION.
           MOVE " " TO ERR-FG.

           MOVE EXT-FILPATH-BASE TO TRB-ALL-BASE.
           MOVE FROM-DATA-PATH   TO TRB-ALL-DATA.
           MOVE FROM-MACHINE     TO TRB-ALL-MACHINE.
           MOVE TRB-ALL-PATH     TO TRB-PATH.
           MOVE "Y"              TO TRB-PATH-OVERRIDE.

           PERFORM OPEN-TRB1-FILE.

           MOVE FROM-BRANCH TO TB-BRNO.
           MOVE TRANS-DATE  TO TB-DATE.
           MOVE PD-CLASS    TO TB-CLASS.
           MOVE PD-ACCTNO   TO TB-ACCTNO.
           MOVE ZEROS       TO TB-LAWCODE.
           MOVE 0           TO HOLD-TB-LINE.

       CREATE-FRTR-FILE-EXISTS.
           ADD 1             TO HOLD-TB-LINE.
           MOVE HOLD-TB-LINE TO TB-LINE.

           MOVE TRB1-KEY TO LPWSTRB-TRB1-KEY.
           PERFORM READ-TRB1-FILE.
           IF IO-GOOD
      *       UNLOCK TRB-FILE
              GO TO CREATE-FRTR-FILE-EXISTS.

           PERFORM CLEAR-TRB-FILE.

           MOVE LPWSTRB-TRB1-KEY TO TRB1-KEY.

           MOVE "T"              TO REF.
           MOVE TO-BR            TO REF-BR.
           MOVE REF              TO TB-REFNO.
           MOVE TO-LN-ACCTNO     TO TB-REF-ACCTNO.

           MOVE PD-LOANTYPE TO TB-LOANTYPE.
           MOVE PD-SERVACCT TO TB-SERVACCT.
           MOVE PD-LOANDATE TO TB-LOANDATE.
           MOVE PD-PURCD    TO TB-PURCD.
           MOVE PD-SRCD     TO TB-SRCD.
           MOVE PD-COLLCD   TO TB-COLLCD.
           MOVE PD-ORGTERM  TO TB-ORGTERM.
           MOVE PD-DLRECOURSE TO TB-DLRECOURSE.
           MOVE PD-INSURED  TO TB-INSURED.
           MOVE PD-DLNO     TO TB-DLNO.

           MOVE PD-PLCD      TO TB-PLCD.
           MOVE PD-PLTYPE    TO TB-PLTYPE.
           MOVE PD-REPOCD    TO TB-REPOCD.
           MOVE PD-ACTIONCD2 TO TB-ACTIONCD2.

           COMPUTE TB-LNAMT = PD-SUM-LNAMT * -1.
           COMPUTE TB-FINCHG = PD-SUM-FINCHG * -1.
           COMPUTE TB-DLRVPRIN(1) = PD-DLRVPRIN(1) * -1.
           COMPUTE TB-DLRVPRIN(2) = PD-DLRVPRIN(2) * -1.
           COMPUTE TB-DLRVPRIN(3) = PD-DLRVPRIN(3) * -1.
           COMPUTE TB-DLRVCHGS(1) = PD-DLRVCHGS(1) * -1.
           COMPUTE TB-DLRVCHGS(2) = PD-DLRVCHGS(2) * -1.
           COMPUTE TB-DLRVCHGS(3) = PD-DLRVCHGS(3) * -1.
           COMPUTE TB-DLPARVPAID = PD-DLPARVPAID * -1.
           COMPUTE TB-DLPARVHELD = PD-DLPARVHELD * -1.
           COMPUTE TB-DLDISC = PD-DLDISC * -1.
           COMPUTE TB-PROCEEDS = PD-PROCEEDS * -1.
           COMPUTE TB-DLPARTRCAMT = PD-DLPARTRCAMT * -1.
           COMPUTE TB-PURBAL = PD-CURBAL * -1.
           COMPUTE TB-MAINTFEE = PD-SUM-MAINTFEE * -1.
           COMPUTE TB-SERCHG = PD-SUM-SERCHG * -1.
           COMPUTE TB-EXTCHG = PD-SUM-EXTCHG * -1.
           COMPUTE TB-INTCHG = PD-SUM-INTCHG * -1.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 10
              IF SUB NOT > 4
                 MOVE PD-SSNO(SUB) TO TB-SSNO(SUB)
                 MOVE PD-MAKERCD(SUB) TO TB-MAKERCD(SUB)
              END-IF
           END-PERFORM.

      * SET UNEARNED AMOUNTS INTO TB-WORKAMT'S:
           IF PD-SERVACCT = "N"
              MULTIPLY -1 BY SAVE-UNEARN(1) GIVING TB-UNEARN(1)
              MULTIPLY -1 BY SAVE-UNEARN(2) GIVING TB-UNEARN(2)
              MULTIPLY -1 BY SAVE-UNEARN(3) GIVING TB-UNEARN(3)
              MULTIPLY -1 BY SAVE-UNEARN(4) GIVING TB-UNEARN(4)
              MULTIPLY -1 BY SAVE-UNEARN(5) GIVING TB-UNEARN(5)
              MULTIPLY -1 BY SAVE-UNEARN(6) GIVING TB-UNEARN(6).

           PERFORM SET-POSTDATE.
           MOVE POST-USERID TO TB-USERID.
           MOVE POST-TIME TO TB-POSTTIME.
           MOVE POST-DATE TO TB-POSTDATE.

           PERFORM WRITE-TRB1-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

           PERFORM CLOSE-TRB1-FILE.

       CREATE-FRTR-FILE-EXIT.
           EXIT.

      *************************************************
       UPDATE-FRNO-FILE SECTION.
      *********************************************

      * RECALCULATE THE NOTICE IN THE FROM BRANCH AS IT
      * IS NOW PAIDOUT
           MOVE " " TO ERR-FG.

           MOVE EXT-FILPATH-BASE           TO NO-ALL-BASE.
           MOVE FROM-MACHINE               TO NO-ALL-MACHINE.
           MOVE FROM-DATA-PATH             TO NO-ALL-DATA.
           MOVE NO-ALL-PATH                TO NO-PATH.
           MOVE "Y"                        TO NO-PATH-OVERRIDE.

           PERFORM OPEN-NO1-FILE.
           MOVE NO-PATH-OWNBR TO NO-BRNO.
           MOVE PD-ACCTNO     TO NO-ACCTNO.
           PERFORM READ-NO1-FILE.
           IF IO-BAD
              GO TO UPDATE-FRNO-FILE-EXIT.

           MOVE LN-ACCTNO     TO CRNOR2-ACCTNO.
           MOVE TRANS-DATE    TO CRNOR2-SYSDATE.
           MOVE "CL/CRNOR2"    TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CRNOR2-LINK NO-REC
                                 POFF-WORKERS LN-REC.
           CANCEL FORM-PROGX.

           MOVE LN-OWNBR  TO NO-BRNO.
           MOVE LN-ACCTNO TO NO-ACCTNO.
           PERFORM REWRITE-NO1-FILE.

           PERFORM CLOSE-NO1-FILE.

       UPDATE-FRNO-FILE-EXIT.
           EXIT.

      *************************************************
      *    UPDATE FROM->DS-FILE
      *************************************************
       UPDATE-FRDS-FILE SECTION.
           MOVE " " TO ERR-FG.

           MOVE EXT-FILPATH-BASE TO DS-ALL-BASE.
           MOVE FROM-DATA-PATH   TO DS-ALL-DATA.
           MOVE FROM-MACHINE     TO DS-ALL-MACHINE.
           MOVE DS-ALL-PATH      TO DS-PATH.
           MOVE "Y" TO DS-PATH-OVERRIDE.

           PERFORM OPEN-DS1-FILE.
           MOVE "N" TO REC-EXISTS.
       UPDATE-FRDS-FILE-EXISTS.
           PERFORM SETUP-DS-DATE-CCYYMM.
           MOVE PD-OWNBR TO DS-BRANCH.
           MOVE PD-DLNO  TO DS-DLNO.
           MOVE PD-CLASS TO DS-CLASS.
           PERFORM READ-DS1-FILE.
           IF NOT IO-GOOD
              IF REC-EXISTS = "Y"
                 GO TO IO-ERROR
              ELSE
                 PERFORM DS1-FR-BR-CREATE-REC
                 GO TO UPDATE-FRDS-FILE-EXISTS.

      * REPO AND P&L INCLUDED IN DS-JDOSUNIT:
           IF PD-CURBAL NOT = 0
              IF PD-JDDATE NOT = 0
                 SUBTRACT PD-CURBAL FROM DS-JDOSAMT(DSCUR)
                 SUBTRACT 1 FROM DS-JDOSUNIT(DSCUR).

           IF (PD-CURBAL NOT = 0)
      * TEST FOR P&L,
           IF ( NOT (PD-PLCD = " " AND PD-PLDATE = 0) )

      * P&L   --------------------------------------------------------
      *       IF PL, SUBTRACT FROM PL OUTSTANDING;  IF PL & REPOED
      *       AFTER PL, ALSO SUBTRACT FROM REPO SLOTS 4 & 5
      *       --------------------------------------------------------
              SUBTRACT PD-CURBAL FROM DS-PLOSAMT(DSCUR)
              SUBTRACT 1 FROM DS-PLOSUNIT(DSCUR)
      *       - ONHAND OR SOLD REPO IN P&L RECEIVABLES:
              IF PD-REPOCD = "X" OR "S"
                 IF PD-REPOCD = "X"
                    SUBTRACT PD-CURBAL FROM DS-AMT-REPO(DSCUR DSNET 4)
                    SUBTRACT 1 FROM DS-NO-REPO(DSCUR DSNET 4)
                 ELSE
                    SUBTRACT PD-CURBAL FROM DS-AMT-REPO(DSCUR DSNET 5)
                    SUBTRACT 1 FROM DS-NO-REPO(DSCUR DSNET 5)
                 END-IF
              END-IF
           ELSE
      * NOT P&L:
      *       ------------------------------------------------------
      *           UPDATE DEALER WEIGHTED AVERAGE STATISTICS
      *       ------------------------------------------------------
              COMPUTE DS-SUMWGTTM = DS-SUMWGTTM + (PD-ORGTERM * -1)
              COMPUTE DS-SUMWGTAV = DS-SUMWGTAV + (PD-LNAMT * -1)
              COMPUTE DS-SUMWGTTA = DS-SUMWGTTA +
                                     (PD-ORGTERM * PD-LNAMT * -1)

      *       ------------------------------------------------------
      *           UPDATE DEALER RECOURSE STATISTICS
      *       ------------------------------------------------------
              PERFORM GET-RECOURSE-SUB
              IF SUB NOT = 0
                 SUBTRACT 1 FROM DS-RCOURSE-UNITS(SUB)
                 MOVE PD-CURBAL TO PART-RC-AMT
                 IF SUB = 2
                    IF PD-CURBAL > PD-DLPARTRCAMT
                       MOVE PD-DLPARTRCAMT TO PART-RC-AMT
                    END-IF
                 END-IF
                 SUBTRACT PART-RC-AMT FROM DS-RCOURSE-AMT(SUB)
              END-IF

      *     TEST FOR ON HAND REPO:
              IF LN-REPOCD = "X"
                 SUBTRACT PD-CURBAL FROM DS-AMT-REPO(DSCUR DSNET 1)
                 SUBTRACT 1 FROM DS-NO-REPO(DSCUR DSNET 1)
              ELSE
      *     TEST FOR SOLD REPO:
              IF LN-REPOCD = "S"
                 SUBTRACT PD-CURBAL FROM DS-AMT-REPO(DSCUR DSNET 3)
                 SUBTRACT 1 FROM DS-NO-REPO(DSCUR DSNET 3)
              ELSE
      *     NON REPO:
                 PERFORM UPDATE-DS1-FR-BR-DELINQ
                 SUBTRACT PD-CURBAL FROM DS-OUTSAMT(1)
                 SUBTRACT 1 FROM DS-OUTSUNIT(1).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    UPDATE UNEARNED STATISTICS NET
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF GP-UNEARNED-STATS = "Y"
              SUBTRACT SAVE-UNEARN(1) FROM DS-UNEARNED-AMT(1 1 1)
              SUBTRACT SAVE-UNEARN(2) FROM DS-UNEARNED-AMT(1 1 2)
              SUBTRACT SAVE-UNEARN(3) FROM DS-UNEARNED-AMT(1 1 3)
              IF NOT (GP-INCL-OTH-IN-EARNED-R )
                 SUBTRACT SAVE-UNEARN(4) FROM DS-UNEARNED-AMT(1 1 4)
              END-IF
              SUBTRACT SAVE-UNEARN(5) FROM DS-UNEARNED-AMT(1 1 5)
              IF NOT(GP-INCL-OTH-IN-EARNED-N OR
                                          GP-INCL-OTH-IN-EARNED-R)
                 SUBTRACT SAVE-UNEARN(6) FROM DS-UNEARNED-AMT(1 1 6).
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           PERFORM REWRITE-DS1-FILE.
           PERFORM CLOSE-DS1-FILE.

       UPDATE-FRDS-FILE-EXIT.
           EXIT.

      **********************************************
       DS1-FR-BR-CREATE-REC SECTION.
      **********************************************
           MOVE "Y" TO REC-EXISTS.
           MOVE " " TO DS-REC.

           PERFORM CLEAR-DS-FILE.

           PERFORM SETUP-DS-DATE-CCYYMM.
           MOVE PD-OWNBR TO DS-BRANCH.
           MOVE PD-DLNO TO DS-DLNO.
           MOVE PD-CLASS TO DS-CLASS.

           PERFORM WRITE-DS1-FILE.
           IF IO-BAD
              GO TO IO-ERROR.


      *************************************************
      *    UPDATE FROM->TY-FILE
      *************************************************
       UPDATE-FRTY-FILE SECTION.
           MOVE " " TO ERR-FG.

           MOVE EXT-FILPATH-BASE TO TY-ALL-BASE.
           MOVE FROM-DATA-PATH   TO TY-ALL-DATA.
           MOVE FROM-MACHINE     TO TY-ALL-MACHINE.
           MOVE TY-ALL-PATH      TO TY-PATH.
           MOVE "Y"              TO TY-PATH-OVERRIDE.

           PERFORM OPEN-TY1-FILE.
           MOVE "N" TO REC-EXISTS.
       UPDATE-FRTY-FILE-EXISTS.
           MOVE FROM-BRANCH TO TY-BRNO.
           PERFORM SET-TY-KEY.
           PERFORM READ-TY1-FILE.
           IF IO-GOOD
              GO TO UPDATE-FRTY-FILE-REC.
           IF REC-EXISTS = "Y"
              GO TO IO-ERROR.

           MOVE "Y" TO REC-EXISTS.
           PERFORM CLEAR-TY-FILE.
           MOVE FROM-BRANCH TO TY-BRNO.
           PERFORM SET-TY-KEY.
           PERFORM WRITE-TY1-FILE.
           IF IO-BAD AND E-CODE NOT = "22"
              GO TO IO-ERROR.
           GO TO UPDATE-FRTY-FILE-EXISTS.

       UPDATE-FRTY-FILE-REC.
           PERFORM GET-SRCD-SUB.

           IF (PD-CURBAL NOT = 0)
           IF SRCD-NOT-PL
      * NOT P&L:
              COMPUTE TY-SUMWGTTM = TY-SUMWGTTM + (PD-ORGTERM * -1)
              COMPUTE TY-SUMWGTAV = TY-SUMWGTAV + (PD-LNAMT * -1)
              COMPUTE TY-SUMWGTTA = TY-SUMWGTTA +
                                       (PD-ORGTERM * PD-LNAMT * -1)
      *       - ONHAND REPO:
              IF LN-REPOCD = "X"
                 SUBTRACT 1 FROM TY-NO-REPO(TYNET 1)
                 SUBTRACT PD-CURBAL FROM TY-AMT-REPO(TYNET 1)
                 SUBTRACT PD-OT2BAL FROM TY-O2-REPO(TYNET 1)
                 SUBTRACT PD-INTCHG FROM TY-INT-REPO(TYNET 1)
                 SUBTRACT PD-EXTCHG FROM TY-INT-REPO(TYNET 1)
              ELSE
      *       - SOLD REPO:
              IF LN-REPOCD = "S"
                 SUBTRACT 1 FROM TY-NO-REPO(TYNET 3)
                 SUBTRACT PD-CURBAL FROM TY-AMT-REPO(TYNET 3)
                 SUBTRACT PD-OT2BAL FROM TY-O2-REPO(TYNET 3)
                 SUBTRACT PD-INTCHG FROM TY-INT-REPO(TYNET 3)
                 SUBTRACT PD-EXTCHG FROM TY-INT-REPO(TYNET 3)
              ELSE
      *          - NON REPO:
                 PERFORM UPDATE-TY1-FR-BR-DELINQ
                 SUBTRACT 1 FROM TY-NO(TYNET SRCD)
                 SUBTRACT PD-CURBAL FROM TY-AMT(TYNET SRCD)
                 SUBTRACT PD-OT2BAL FROM TY-O2(TYNET SRCD)
                 SUBTRACT PD-INTCHG FROM TY-INT(TYNET SRCD)
                 SUBTRACT PD-EXTCHG FROM TY-INT(TYNET SRCD)
           ELSE
      *       - P&L (ACTIVE/INACTIVE):
              SUBTRACT 1 FROM TY-NO-PL(TYNET SRCD)
              SUBTRACT PD-CURBAL FROM TY-AMT-PL(TYNET SRCD)
              SUBTRACT PD-OT2BAL FROM TY-O2-PL(TYNET SRCD)
              SUBTRACT PD-INTCHG FROM TY-INT-PL(TYNET SRCD)
              SUBTRACT PD-EXTCHG FROM TY-INT-PL(TYNET SRCD)
      *       - ONHAND OR SOLD REPO IN P&L RECEIVABLES:
              IF LN-REPOCD = "X" OR "S"
                 IF LN-REPOCD = "X"
                    SUBTRACT 1 FROM TY-NO-REPO(TYNET 4)
                    SUBTRACT PD-CURBAL FROM TY-AMT-REPO(TYNET 4)
                    SUBTRACT PD-OT2BAL FROM TY-O2-REPO(TYNET 4)
                    SUBTRACT PD-INTCHG FROM TY-INT-REPO(TYNET 4)
                    SUBTRACT PD-EXTCHG FROM TY-INT-REPO(TYNET 4)
                 ELSE
                    SUBTRACT 1 FROM TY-NO-REPO(TYNET 5)
                    SUBTRACT PD-CURBAL FROM TY-AMT-REPO(TYNET 5)
                    SUBTRACT PD-OT2BAL FROM TY-O2-REPO(TYNET 5)
                    SUBTRACT PD-INTCHG FROM TY-INT-REPO(TYNET 5)
                    SUBTRACT PD-EXTCHG FROM TY-INT-REPO(TYNET 5).

      *----------------------------------------------------------------
      * NOTE:    ACTIVE & REPO & P&L DUAL LOGIC FOLLOWS
      *----------------------------------------------------------------
      * UPDATE LOANS SOLD/TRANSFERRED TO ANOTHER BRANCH.
      *----------------------------------------------------------------
           ADD 1 TO TY-NO-SOLD.
           ADD PD-CURBAL TO TY-AMT-SOLD.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    UPDATE UNEARNED STATISTICS NET
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF GP-UNEARNED-STATS = "Y"
              SUBTRACT SAVE-UNEARN(1) FROM TY-UNEARNED-AMT(1 1)
              SUBTRACT SAVE-UNEARN(2) FROM TY-UNEARNED-AMT(1 2)
              SUBTRACT SAVE-UNEARN(3) FROM TY-UNEARNED-AMT(1 3)
              IF NOT (GP-INCL-OTH-IN-EARNED-R)
                  SUBTRACT SAVE-UNEARN(4) FROM TY-UNEARNED-AMT(1 4)
              END-IF
              SUBTRACT SAVE-UNEARN(5) FROM TY-UNEARNED-AMT(1 5)
              IF NOT (GP-INCL-OTH-IN-EARNED-N OR
                                             GP-INCL-OTH-IN-EARNED-R)
                 SUBTRACT SAVE-UNEARN(6) FROM TY-UNEARNED-AMT(1 6).

           PERFORM REWRITE-TY1-FILE.
           PERFORM CLOSE-TY1-FILE.

       UPDATE-FRTY-FILE-EXIT.
           EXIT.

      *************************************************
      *    CREATE TO->TR-FILE
      *************************************************
       CREATE-TOTR-FILE SECTION.
           MOVE " " TO ERR-FG.

           MOVE EXT-FILPATH-BASE TO TRB-ALL-BASE.
           MOVE TO-DATA-PATH     TO TRB-ALL-DATA.
           MOVE TO-MACHINE       TO TRB-ALL-MACHINE.
           MOVE TRB-ALL-PATH     TO TRB-PATH.
           MOVE "Y"              TO TRB-PATH-OVERRIDE.

           PERFORM OPEN-TRB1-FILE.

           MOVE TO-BR        TO TB-BRNO.
           MOVE TRANS-DATE   TO TB-DATE.
           MOVE PD-CLASS     TO TB-CLASS.
           MOVE TO-LN-ACCTNO TO TB-ACCTNO.
           MOVE ZEROS        TO TB-LAWCODE.
           MOVE 0            TO HOLD-TB-LINE.

       CREATE-TOTR-FILE-EXISTS.
           ADD 1             TO HOLD-TB-LINE.
           MOVE HOLD-TB-LINE TO TB-LINE.

           MOVE TRB1-KEY TO LPWSTRB-TRB1-KEY.
           PERFORM READ-TRB1-FILE.
           IF IO-GOOD
      *       UNLOCK TRB-FILE
              GO TO CREATE-TOTR-FILE-EXISTS.

           PERFORM CLEAR-TRB-FILE.

           MOVE LPWSTRB-TRB1-KEY TO TRB1-KEY.

           MOVE "F"         TO REF.
           MOVE FROM-BRANCH TO REF-BR.
           MOVE REF         TO TB-REFNO.
           MOVE PD-ACCTNO   TO TB-REF-ACCTNO.

           MOVE PD-LOANTYPE TO TB-LOANTYPE.
           MOVE PD-SERVACCT TO TB-SERVACCT.
           MOVE PD-LOANDATE TO TB-LOANDATE.
           MOVE PD-PURCD    TO TB-PURCD.
           MOVE PD-SRCD     TO TB-SRCD.
           MOVE PD-COLLCD   TO TB-COLLCD.
           MOVE PD-ORGTERM  TO TB-ORGTERM.
           MOVE PD-DLRECOURSE TO TB-DLRECOURSE.
           MOVE PD-INSURED  TO TB-INSURED.
           MOVE PD-DLNO     TO TB-DLNO.

           MOVE PD-PLCD      TO TB-PLCD.
           MOVE PD-PLTYPE    TO TB-PLTYPE.
           MOVE PD-REPOCD    TO TB-REPOCD.
           MOVE PD-ACTIONCD2 TO TB-ACTIONCD2.

           COMPUTE TB-LNAMT = PD-SUM-LNAMT * 1.
           COMPUTE TB-FINCHG = PD-SUM-FINCHG * 1.
           COMPUTE TB-DLRVPRIN(1) = PD-DLRVPRIN(1) * 1.
           COMPUTE TB-DLRVPRIN(2) = PD-DLRVPRIN(2) * 1.
           COMPUTE TB-DLRVPRIN(3) = PD-DLRVPRIN(3) * 1.
           COMPUTE TB-DLRVCHGS(1) = PD-DLRVCHGS(1) * 1.
           COMPUTE TB-DLRVCHGS(2) = PD-DLRVCHGS(2) * 1.
           COMPUTE TB-DLRVCHGS(3) = PD-DLRVCHGS(3) * 1.
           COMPUTE TB-DLPARVPAID = PD-DLPARVPAID * 1.
           COMPUTE TB-DLPARVHELD = PD-DLPARVHELD * 1.
           COMPUTE TB-DLDISC = PD-DLDISC * 1.
           COMPUTE TB-PROCEEDS = PD-PROCEEDS * 1.
           COMPUTE TB-DLPARTRCAMT = PD-DLPARTRCAMT * 1.
           COMPUTE TB-PURBAL = PD-CURBAL * 1.
           COMPUTE TB-MAINTFEE = PD-SUM-MAINTFEE * 1.
           COMPUTE TB-SERCHG = PD-SUM-SERCHG * 1.
           COMPUTE TB-EXTCHG = PD-SUM-EXTCHG * 1.
           COMPUTE TB-INTCHG = PD-SUM-INTCHG * 1.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 10
              IF SUB NOT > 4
                 MOVE PD-SSNO(SUB) TO TB-SSNO(SUB)
                 MOVE PD-MAKERCD(SUB) TO TB-MAKERCD(SUB)
              END-IF
           END-PERFORM.

      * SET UNEARNED AMOUNTS INTO TB-WORKAMT'S:
           IF PD-SERVACCT = "N"
              MULTIPLY 1 BY SAVE-UNEARN(1) GIVING TB-UNEARN(1)
              MULTIPLY 1 BY SAVE-UNEARN(2) GIVING TB-UNEARN(2)
              MULTIPLY 1 BY SAVE-UNEARN(3) GIVING TB-UNEARN(3)
              MULTIPLY 1 BY SAVE-UNEARN(4) GIVING TB-UNEARN(4)
              MULTIPLY 1 BY SAVE-UNEARN(5) GIVING TB-UNEARN(5)
              MULTIPLY 1 BY SAVE-UNEARN(6) GIVING TB-UNEARN(6).

           PERFORM SET-POSTDATE.
           MOVE POST-USERID TO TB-USERID.
           MOVE POST-TIME TO TB-POSTTIME.
           MOVE POST-DATE TO TB-POSTDATE.

           PERFORM WRITE-TRB1-FILE.
           IF IO-BAD
              GO TO IO-ERROR.
           PERFORM CLOSE-TRB1-FILE.

       CREATE-TOTR-FILE-EXIT.
           EXIT.

      *************************************************
      *    UPDATE TO->DS-FILE
      *************************************************
       UPDATE-TODS-FILE SECTION.
           MOVE " " TO ERR-FG.

           MOVE EXT-FILPATH-BASE TO DS-ALL-BASE.
           MOVE TO-DATA-PATH TO DS-ALL-DATA.
           MOVE TO-MACHINE TO DS-ALL-MACHINE.
           MOVE DS-ALL-PATH TO DS-PATH.
           MOVE "Y" TO DS-PATH-OVERRIDE.
           PERFORM OPEN-DS1-FILE.
           MOVE "N" TO REC-EXISTS.
       UPDATE-TODS-FILE-EXISTS.
           PERFORM SETUP-DS-DATE-CCYYMM.
           MOVE TO-BR TO DS-BRANCH.
           IF TO-DLNO = 0
              MOVE PD-DLNO TO DS-DLNO
           ELSE
              MOVE TO-DLNO TO DS-DLNO.
           MOVE PD-CLASS TO DS-CLASS.
           PERFORM READ-DS1-FILE.
           IF NOT IO-GOOD
              IF REC-EXISTS = "Y"
                 GO TO IO-ERROR
              ELSE
                 PERFORM DS1-TO-BR-CREATE-REC
                 GO TO UPDATE-TODS-FILE-EXISTS.

      * REPO AND P&L INCLUDED IN DS-JDOSUNIT:
           IF PD-CURBAL NOT = 0
              IF PD-JDDATE NOT = 0
                 ADD PD-CURBAL TO DS-JDOSAMT(DSCUR)
                 ADD 1 TO DS-JDOSUNIT(DSCUR).

           IF (PD-CURBAL NOT = 0)
      * TEST FOR P&L,
           IF ( NOT (PD-PLCD = " " AND PD-PLDATE = 0) )
              ADD PD-CURBAL TO DS-PLOSAMT(DSCUR)
              ADD 1 TO DS-PLOSUNIT(DSCUR)
      *       - ONHAND OR SOLD REPO IN P&L RECEIVABLES:
              IF PD-REPOCD = "X" OR "S"
                 IF PD-REPOCD = "X"
                    ADD PD-CURBAL TO DS-AMT-REPO(DSCUR DSNET 4)
                    ADD 1 TO DS-NO-REPO(DSCUR DSNET 4)
                 ELSE
                    ADD PD-CURBAL TO DS-AMT-REPO(DSCUR DSNET 5)
                    ADD 1 TO DS-NO-REPO(DSCUR DSNET 5)
                 END-IF
              END-IF
           ELSE
      * NOT P&L:
      *       ------------------------------------------------------
      *           UPDATE DEALER WEIGHTED AVERAGE STATISTICS
      *       ------------------------------------------------------
              COMPUTE DS-SUMWGTTM = DS-SUMWGTTM + (PD-ORGTERM * 1)
              COMPUTE DS-SUMWGTAV = DS-SUMWGTAV + (PD-LNAMT * 1)
              COMPUTE DS-SUMWGTTA = DS-SUMWGTTA +
                                    (PD-ORGTERM * PD-LNAMT * 1)

      *       ------------------------------------------------------
      *           UPDATE DEALER RECOURSE STATISTICS
      *       ------------------------------------------------------
              PERFORM GET-RECOURSE-SUB
              IF SUB NOT = 0
                 ADD 1 TO DS-RCOURSE-UNITS(SUB)
                 MOVE PD-CURBAL TO PART-RC-AMT
                 IF SUB = 2
                    IF PD-CURBAL > PD-DLPARTRCAMT
                       MOVE PD-DLPARTRCAMT TO PART-RC-AMT
                    END-IF
                 END-IF
                 ADD PART-RC-AMT TO DS-RCOURSE-AMT(SUB)
              END-IF

      *     TEST FOR ON HAND REPO:
              IF LN-REPOCD = "X"
                 ADD PD-CURBAL TO DS-AMT-REPO(DSCUR DSNET 1)
                 ADD 1 TO DS-NO-REPO(DSCUR DSNET 1)
              ELSE
      *     TEST FOR SOLD REPO:
              IF LN-REPOCD = "S"
                 ADD PD-CURBAL TO DS-AMT-REPO(DSCUR DSNET 3)
                 ADD 1 TO DS-NO-REPO(DSCUR DSNET 3)
              ELSE
      *     NON REPO:
                 PERFORM UPDATE-DS1-TO-BR-DELINQ
                 ADD PD-CURBAL TO DS-OUTSAMT(1)
                 ADD 1 TO DS-OUTSUNIT(1).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    UPDATE UNEARNED STATISTICS NET
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF GP-UNEARNED-STATS = "Y"
              ADD SAVE-UNEARN(1) TO DS-UNEARNED-AMT(1 1 1)
              ADD SAVE-UNEARN(2) TO DS-UNEARNED-AMT(1 1 2)
              ADD SAVE-UNEARN(3) TO DS-UNEARNED-AMT(1 1 3)
              IF NOT (GP-INCL-OTH-IN-EARNED-R)
                 ADD SAVE-UNEARN(4) TO DS-UNEARNED-AMT(1 1 4)
              END-IF
              ADD SAVE-UNEARN(5) TO DS-UNEARNED-AMT(1 1 5)
              IF NOT (GP-INCL-OTH-IN-EARNED-N OR
                                             GP-INCL-OTH-IN-EARNED-R)
                 ADD SAVE-UNEARN(6) TO DS-UNEARNED-AMT(1 1 6).
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           PERFORM REWRITE-DS1-FILE.
           PERFORM CLOSE-DS1-FILE.

       UPDATE-TODS-FILE-EXIT.
           EXIT.

      ***********************************************
       DS1-TO-BR-CREATE-REC SECTION.
      ***********************************************
           MOVE "Y" TO REC-EXISTS.
           MOVE " " TO DS-REC.

           PERFORM CLEAR-DS-FILE.
           PERFORM SETUP-DS-DATE-CCYYMM.
           MOVE TO-BR TO DS-BRANCH.
           IF TO-DLNO = 0
              MOVE PD-DLNO TO DS-DLNO
           ELSE
              MOVE TO-DLNO TO DS-DLNO.
           MOVE PD-CLASS TO DS-CLASS.

           PERFORM WRITE-DS1-FILE.
           IF IO-BAD
              GO TO IO-ERROR.


      *************************************************
      *    UPDATE TO->TY-FILE
      *************************************************
       UPDATE-TOTY-FILE SECTION.
           MOVE " " TO ERR-FG.

           MOVE EXT-FILPATH-BASE TO TY-ALL-BASE.
           MOVE TO-DATA-PATH     TO TY-ALL-DATA.
           MOVE TO-MACHINE       TO TY-ALL-MACHINE.
           MOVE TY-ALL-PATH      TO TY-PATH.
           MOVE "Y"              TO TY-PATH-OVERRIDE.

           PERFORM OPEN-TY1-FILE.
           MOVE "N" TO REC-EXISTS.
       UPDATE-TOTY-FILE-EXISTS.
           MOVE TO-BR TO TY-BRNO.
           PERFORM SET-TY-KEY.
           PERFORM READ-TY1-FILE.
           IF IO-GOOD
              GO TO UPDATE-TOTY-FILE-REC.
           IF REC-EXISTS = "Y"
              GO TO IO-ERROR.

           MOVE "Y" TO REC-EXISTS.
           PERFORM CLEAR-TY-FILE.
           MOVE TO-BR TO TY-BRNO.
           PERFORM SET-TY-KEY.
           PERFORM WRITE-TY1-FILE.
           IF IO-BAD AND E-CODE NOT = "22"
              GO TO IO-ERROR.
           GO TO UPDATE-TOTY-FILE-EXISTS.

       UPDATE-TOTY-FILE-REC.
           PERFORM GET-SRCD-SUB.

           IF (PD-CURBAL NOT = 0)
           IF SRCD-NOT-PL
      * NOT P&L:
              COMPUTE TY-SUMWGTTM = TY-SUMWGTTM + (PD-ORGTERM * 1)
              COMPUTE TY-SUMWGTAV = TY-SUMWGTAV + (PD-LNAMT * 1)
              COMPUTE TY-SUMWGTTA = TY-SUMWGTTA +
                                       (PD-ORGTERM * PD-LNAMT * 1)
      *       - ONHAND REPO:
              IF LN-REPOCD = "X"
                 ADD 1 TO TY-NO-REPO(TYNET 1)
                 ADD PD-CURBAL TO TY-AMT-REPO(TYNET 1)
                 ADD PD-OT2BAL TO TY-O2-REPO(TYNET 1)
                 ADD PD-INTCHG TO TY-INT-REPO(TYNET 1)
                 ADD PD-EXTCHG TO TY-INT-REPO(TYNET 1)
              ELSE
      *       - SOLD REPO:
              IF LN-REPOCD = "S"
                 ADD 1 TO TY-NO-REPO(TYNET 3)
                 ADD PD-CURBAL TO TY-AMT-REPO(TYNET 3)
                 ADD PD-OT2BAL TO TY-O2-REPO(TYNET 3)
                 ADD PD-INTCHG TO TY-INT-REPO(TYNET 3)
                 ADD PD-EXTCHG TO TY-INT-REPO(TYNET 3)
              ELSE
      *       - NON REPO:
                 PERFORM UPDATE-TY1-TO-BR-DELINQ
                 ADD 1 TO TY-NO(TYNET SRCD)
                 ADD PD-CURBAL TO TY-AMT(TYNET SRCD)
                 ADD PD-OT2BAL TO TY-O2(TYNET SRCD)
                 ADD PD-INTCHG TO TY-INT(TYNET SRCD)
                 ADD PD-EXTCHG TO TY-INT(TYNET SRCD)
           ELSE
      *       - P&L (ACTIVE/INACTIVE):
              ADD 1 TO TY-NO-PL(TYNET SRCD)
              ADD PD-CURBAL TO TY-AMT-PL(TYNET SRCD)
              ADD PD-OT2BAL TO TY-O2-PL(TYNET SRCD)
              ADD PD-INTCHG TO TY-INT-PL(TYNET SRCD)
              ADD PD-EXTCHG TO TY-INT-PL(TYNET SRCD)
      *       - ONHAND OR SOLD REPO IN P&L RECEIVABLES:
              IF LN-REPOCD = "X" OR "S"
                 IF LN-REPOCD = "X"
                    ADD 1 TO TY-NO-REPO(TYNET 4)
                    ADD PD-CURBAL TO TY-AMT-REPO(TYNET 4)
                    ADD PD-OT2BAL TO TY-O2-REPO(TYNET 4)
                    ADD PD-INTCHG TO TY-INT-REPO(TYNET 4)
                    ADD PD-EXTCHG TO TY-INT-REPO(TYNET 4)
                 ELSE
                    ADD 1 TO TY-NO-REPO(TYNET 5)
                    ADD PD-CURBAL TO TY-AMT-REPO(TYNET 5)
                    ADD PD-OT2BAL TO TY-O2-REPO(TYNET 5)
                    ADD PD-INTCHG TO TY-INT-REPO(TYNET 5)
                    ADD PD-EXTCHG TO TY-INT-REPO(TYNET 5).

      *----------------------------------------------------------------
      * NOTE:    ACTIVE & REPO & P&L DUAL LOGIC FOLLOWS
      *----------------------------------------------------------------
      * UPDATE LOANS TRANSFERRED FROM ANOTHER BRANCH.
      *----------------------------------------------------------------
           ADD 1 TO TY-NO-TRANS.
           ADD PD-CURBAL TO TY-AMT-TRANS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    UPDATE UNEARNED STATISTICS NET
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF GP-UNEARNED-STATS = "Y"
              ADD SAVE-UNEARN(1) TO TY-UNEARNED-AMT(1 1)
              ADD SAVE-UNEARN(2) TO TY-UNEARNED-AMT(1 2)
              ADD SAVE-UNEARN(3) TO TY-UNEARNED-AMT(1 3)
              IF NOT (GP-INCL-OTH-IN-EARNED-R )
                ADD SAVE-UNEARN(4) TO TY-UNEARNED-AMT(1 4)
              END-IF
              ADD SAVE-UNEARN(5) TO TY-UNEARNED-AMT(1 5)
              IF NOT (GP-INCL-OTH-IN-EARNED-N OR
                                GP-INCL-OTH-IN-EARNED-R)
                 ADD SAVE-UNEARN(6) TO TY-UNEARNED-AMT(1 6).

           PERFORM REWRITE-TY1-FILE.
           PERFORM CLOSE-TY1-FILE.

       UPDATE-TOTY-FILE-EXIT.
           EXIT.

      *************************************************
      *    CREATE TO->LM-FILE
      *************************************************
       CREATE-TOLM-FILE SECTION.
           MOVE " " TO ERR-FG.

           MOVE EXT-FILPATH-BASE TO LM-ALL-BASE.
           MOVE TO-DATA-PATH     TO LM-ALL-DATA.
           MOVE TO-MACHINE       TO LM-ALL-MACHINE.
           MOVE LM-ALL-PATH      TO LM-PATH.
           MOVE "Y"              TO LM-PATH-OVERRIDE.
           PERFORM OPEN-LM1-FILE.

           MOVE LM-PATH-OWNBR TO LM-BRNO.
           MOVE TO-LN-ACCTNO  TO LM-ACCTNO.
           SUBTRACT TRANS-DATE FROM 99999999 GIVING LM-DATE.
           MOVE 0             TO LM-SEQ.
       CREATE-TOLM-FILE-NEXT.
           IF LM-SEQ = 999
              GO TO CREATE-TOLM-FILE-EXIT.

           ADD 1 TO LM-SEQ.
           PERFORM READ-LM1-FILE.
           IF IO-FG = 0
      *       UNLOCK LM-FILE
              GO TO CREATE-TOLM-FILE-NEXT.

           MOVE TO-BR TO LM-BRNO.
           MOVE "9"   TO LM-MEMO-TYPE.
           MOVE SPACES                     TO TFRMEMO-MEMO.
           MOVE "TFR FR BR"                TO TFRMEMO-PRIMARY.
           MOVE FROM-BRANCH                TO TFRMEMO-BRNO1.
           MOVE "TO BR"                    TO TFRMEMO-SECONDARY.
           MOVE TO-BR                      TO TFRMEMO-BRNO2.
           MOVE ", FOR $"                  TO TFRMEMO-LM-FOR.
           MOVE PD-CURBAL                  TO TFRMEMO-LM-CURBAL.

           IF ( TO-LN-ACCTNO NOT = PD-ACCTNO )
              MOVE "<---"                  TO TFRMEMO-LM-DIRECTION
              MOVE PD-ACCTNO               TO TFRMEMO-LM-ACCTNO.

           MOVE TFRMEMO-MEMO               TO LM-MEMO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           PERFORM WRITE-LM1-FILE.
           IF IO-BAD
              PERFORM IO-ERROR.

       CREATE-TOLM-FILE-EXIT.
           PERFORM CLOSE-LM1-FILE.

      ******************************************************
      *    ESTABLISH CONTRACTUAL AND RECENCY
      *    FOR UPDATE TY1  AND  DS1
      ******************************************************
       ESTABLISH-ORIG-TY-DS-NUMBERS SECTION.
           MOVE TRANS-DATE TO AGEING-DATE.
           PERFORM COMPUTE-CONTRACTUAL.
           PERFORM COMPUTE-RECENCY.
           PERFORM COMPUTE-PAID-AHEAD.

           MOVE CSUB TO SAVE-CSUB.
           MOVE RDSUB TO SAVE-RDSUB.
           MOVE PSUB TO SAVE-PSUB.

           IF GP-DELINQ-WITH-O2 = "Y"
              MOVE LN-CURBAL TO SAVE-OUTBAL
           ELSE
              SUBTRACT LN-OT2BAL FROM LN-CURBAL GIVING SAVE-OUTBAL.

      ******************************************************
      *    UPDATE DELINQUENCY FROM->TYFILE
      * NOTE: DON'T ALTER SRCD IN THIS ROUTINE
      ******************************************************
       UPDATE-TY1-FR-BR-DELINQ SECTION.
           IF SAVE-OUTBAL NOT = 0
              SUBTRACT 1 FROM TY-UNITS(TYREC SAVE-RDSUB)
                              TY-UNITS(TYCON SAVE-CSUB)
              SUBTRACT SAVE-OUTBAL
                         FROM TY-OUTBAL(TYREC SAVE-RDSUB)
                              TY-OUTBAL(TYCON SAVE-CSUB)
              IF LN-BNKRPTDATE NOT = 0
                 SUBTRACT 1 FROM TY-UNITS-BNKRPT(TYREC SAVE-RDSUB)
                                 TY-UNITS-BNKRPT(TYCON SAVE-CSUB)
                 SUBTRACT SAVE-OUTBAL
                            FROM TY-OUTBAL-BNKRPT(TYREC SAVE-RDSUB)
                                 TY-OUTBAL-BNKRPT(TYCON SAVE-CSUB).

      ******************************************************
      *    UPDATE DELINQUENCY FROM->DSFILE
      * NOTE: DON'T ALTER SRCD IN THIS ROUTINE
      ******************************************************
       UPDATE-DS1-FR-BR-DELINQ SECTION.
           IF SAVE-OUTBAL NOT = 0
              SUBTRACT 1 FROM DS-UNITS(DSNOW DSREC SAVE-RDSUB)
                              DS-UNITS(DSNOW DSCON SAVE-CSUB)
              SUBTRACT SAVE-OUTBAL
                         FROM DS-OUTBAL(DSNOW DSREC SAVE-RDSUB)
                              DS-OUTBAL(DSNOW DSCON SAVE-CSUB)
              IF LN-BNKRPTDATE NOT = 0
                 SUBTRACT 1 FROM DS-UNITS-BNKRPT(DSNOW DSREC SAVE-RDSUB)
                                  DS-UNITS-BNKRPT(DSNOW DSCON SAVE-CSUB)
                 SUBTRACT SAVE-OUTBAL
                          FROM DS-OUTBAL-BNKRPT(DSNOW DSREC SAVE-RDSUB)
                               DS-OUTBAL-BNKRPT(DSNOW DSCON SAVE-CSUB).

      ******************************************************
      *    UPDATE DELINQUENCY TO->TYFILE
      * NOTE: DON'T ALTER SRCD IN THIS ROUTINE
      ******************************************************
       UPDATE-TY1-TO-BR-DELINQ SECTION.
           IF SAVE-OUTBAL NOT = 0
              ADD 1 TO TY-UNITS(TYREC SAVE-RDSUB)
                       TY-UNITS(TYCON SAVE-CSUB)
              ADD SAVE-OUTBAL
                    TO TY-OUTBAL(TYREC SAVE-RDSUB)
                       TY-OUTBAL(TYCON SAVE-CSUB)
              IF LN-BNKRPTDATE NOT = 0
                 ADD 1 TO TY-UNITS-BNKRPT(TYREC SAVE-RDSUB)
                          TY-UNITS-BNKRPT(TYCON SAVE-CSUB)
                 ADD SAVE-OUTBAL
                       TO TY-OUTBAL-BNKRPT(TYREC SAVE-RDSUB)
                          TY-OUTBAL-BNKRPT(TYCON SAVE-CSUB).

      ******************************************************
      *    UPDATE DELINQUENCY TO->DSFILE
      * NOTE: DON'T ALTER SRCD IN THIS ROUTINE
      ******************************************************
       UPDATE-DS1-TO-BR-DELINQ SECTION.
           IF SAVE-OUTBAL NOT = 0
              ADD 1 TO DS-UNITS(DSNOW DSREC SAVE-RDSUB)
                       DS-UNITS(DSNOW DSCON SAVE-CSUB)
              ADD SAVE-OUTBAL TO DS-OUTBAL(DSNOW DSREC SAVE-RDSUB)
                                 DS-OUTBAL(DSNOW DSCON SAVE-CSUB)
              IF LN-BNKRPTDATE NOT = 0
                 ADD 1 TO DS-UNITS-BNKRPT(DSNOW DSREC SAVE-RDSUB)
                          DS-UNITS-BNKRPT(DSNOW DSCON SAVE-CSUB)
                 ADD SAVE-OUTBAL
                       TO DS-OUTBAL-BNKRPT(DSNOW DSREC SAVE-RDSUB)
                          DS-OUTBAL-BNKRPT(DSNOW DSCON SAVE-CSUB).

      ***********************************
       UPDATE-TOLS-MISC SECTION.
      ***********************************

      * WRITE LS-M-MISC FIELDS TO TRACK TRANSFER FROM INFO FOR METRO2 "L1"
      * SEGMENT - IN THE "TO" BRANCH ONLY TO TRACK WHERE IT CAME FROM

           MOVE EXT-FILPATH-BASE           TO LSM-ALL-BASE.
           MOVE TO-MACHINE                 TO LSM-ALL-MACHINE
           MOVE TO-DATA-PATH               TO LSM-ALL-DATA
           MOVE LSM-ALL-PATH               TO LSM-PATH.
           MOVE "Y"                        TO LSM-PATH-OVERRIDE.

           PERFORM OPEN-LSM1-FILE.
           MOVE TO-BR         TO LSM-BRNO.
           MOVE TO-LN-ACCTNO  TO LSM-ACCTNO.
           MOVE 1             TO LSM-SEQNO.
           PERFORM READ-LSM1-FILE.
           IF IO-FG = 0
              MOVE FROM-BRANCH TO LSM-TRANS-FROM-BRNO
              MOVE PD-ACCTNO   TO LSM-TRANS-FROM-ACCTNO
              MOVE TRANS-DATE  TO LSM-TRANS-FROM-DATE
              PERFORM REWRITE-LSM1-FILE
              GO TO EXIT-UPDATE-TOLS-MISC.

           PERFORM CLEAR-LSM-FILE.
           MOVE TO-BR        TO LSM-BRNO.
           MOVE TO-LN-ACCTNO TO LSM-ACCTNO.
           MOVE 1            TO LSM-SEQNO.

           MOVE FROM-BRANCH  TO LSM-TRANS-FROM-BRNO.
           MOVE PD-ACCTNO    TO LSM-TRANS-FROM-ACCTNO.
           MOVE TRANS-DATE   TO LSM-TRANS-FROM-DATE.

           PERFORM WRITE-LSM1-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

       EXIT-UPDATE-TOLS-MISC.
           PERFORM CLOSE-LSM1-FILE.

      ***********************************************
       INITIALIZATION SECTION.
      ***********************************************
           MOVE "BULKG2" TO BICREA-CALLED-FROM.
           MOVE "N" TO RECORDS-FOUND.
           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO AGEING-DATE TRANS-DATE.

           IF EXT-CONAME-USER-LOC NOT = TO-BR
              PERFORM VERIFY-PASSWORD
              IF PASSWDW-STAT NOT = "OK"
                 MOVE
            "YOU CAN'T ACCEPT A BULK TRANSFER FOR ANOTHER BRANCH"
                                                      TO MESS
                 PERFORM SEND-MESS
                 MOVE "X" TO ERRCD
                 GO TO INITIALIZATION-EXIT.

           PERFORM OPEN-LC1-FILE.
           PERFORM OPEN-SP1-FILE.
           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
           MOVE FR-GR TO GR-ID.
           PERFORM SEARCH-GR-INIT.

       SEND-FROM-GROUP-NAME.
           MOVE "S  A040FRGR." TO VDU.
           MOVE FR-GR TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A180FRGRNAME." TO VDU.
           MOVE GR-NAME TO VDUBUF.
           PERFORM SCREENIO.

       CHECK-NEXT-FROM-BRANCH-OPEN.
           PERFORM SEARCH-GR-NEXT.
           IF WGR-SRESULT = 0
              GO TO CHECK-TO-BRANCH-OPEN.

           MOVE WGR-SRESULT TO TEST-OPEN-BRANCH.
           PERFORM VERIFY-BRANCH-IS-OPEN.
           IF BR-OPEN-FG = "E"
              MOVE "PROBLEM GETTING 'FROM' BRANCH!" TO MESS
              PERFORM SEND-MESS
              MOVE "X" TO ERRCD
              GO TO INITIALIZATION-EXIT.
           IF BR-OPEN-FG NOT = " "
              MOVE "'FROM' BRANCH &&&& IS NOT OPEN FOR BUSINESS TODAY"
                                                           TO MESS
              INSPECT MESS REPLACING FIRST "&&&&" BY BR-NO
              PERFORM SEND-MESS
              MOVE "X" TO ERRCD
              GO TO INITIALIZATION-EXIT.

           GO TO CHECK-NEXT-FROM-BRANCH-OPEN.

       CHECK-TO-BRANCH-OPEN.
           MOVE TO-BR TO TEST-OPEN-BRANCH.
           PERFORM VERIFY-BRANCH-IS-OPEN.
           IF BR-OPEN-FG = "E"
              MOVE "PROBLEM GETTING 'TO' BRANCH!" TO MESS
              PERFORM SEND-MESS
              MOVE "X" TO ERRCD
              GO TO INITIALIZATION-EXIT.
           MOVE "SNNN040TOBR." TO VDU.
           MOVE TO-BR TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "S  A180TOBRNAME." TO VDU.
           MOVE BR-NAME TO VDUBUF HOLD-TO-BRNAME.
           PERFORM SCREENIO.
           IF BR-OPEN-FG NOT = " "
              MOVE "'TO' BRANCH IS NOT OPEN FOR BUSINESS TODAY"
                                                           TO MESS
              PERFORM SEND-MESS
              MOVE "X" TO ERRCD
              GO TO INITIALIZATION-EXIT.
      *  SAVE TO STATE FOR SET OF LN-OWNST:
           MOVE BR-STATE TO TO-STATE.

      * SET-WK-FILE:
           MOVE EXT-FILPATH-BASE TO BULK-ALL-BASE.
           MOVE TO-MACHINE TO BULK-ALL-MACHINE.
           MOVE FR-GR TO BULK-FROM-TO.
           INSPECT BULK-FROM-TO REPLACING FIRST "    " BY "BULK".
           INSPECT BULK-FROM-TO REPLACING FIRST "    " BY TO-BR.
           MOVE BULK-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT NOT = "00"
              MOVE
           "ACCOUNT SELECTION FOR BULK TRANSFER HAS NOT BEEN DONE!!"
                                                  TO MESS
              PERFORM SEND-MESS
              MOVE "X" TO ERRCD
              GO TO INITIALIZATION-EXIT.

           PERFORM OPEN-WK-FILE.
           MOVE " " TO WK-TYPE.
           MOVE 0 TO WK-BRNO
                     WK-CLASS
                     WK-ACCTNO.

           PERFORM START-WK-FILE.
           PERFORM READ-WK-FILE-NEXT.
           PERFORM CLOSE-WK-FILE.
           IF (IO-BAD) OR (WK-TYPE NOT = " ")
              MOVE "HEADER RECORD FOR SELECTION MISSING" TO MESS
              PERFORM SEND-MESS
              MOVE "X" TO ERRCD
              GO TO INITIALIZATION-EXIT.
           MOVE WK-TO-DLNO TO TO-DLNO.
           MOVE "SNNN060NOTOBE." TO VDU.
           MOVE WK-NOSEL TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN072AMTTOBE." TO VDU.
           MOVE WK-AMTSEL TO VDUNUM2.
           PERFORM SCREENIO.
           IF WK-NOSEL = 0
              MOVE "NO ACCOUNTS WERE FOUND TO BE BULK TRANSFERRED!"
                                                  TO MESS
              PERFORM SEND-MESS
              MOVE "X" TO ERRCD
              GO TO INITIALIZATION-EXIT.
           MOVE WK-PL-ONLY TO PL-ONLY.
           MOVE WK-ONHAND TO ONHAND.
           MOVE WK-SOLD TO SOLD.
           MOVE WK-REDEEM TO REDEEM.
           MOVE WK-ZBAL TO ZBAL.


       SETRP.
           MOVE " " TO ERRCD.
      *  MOVE 'U' TO STAT TO PROHIBIT PIGGYBACK PRINTING.
           MOVE "U" TO STAT.
           MOVE BULKG2-QUEUE-POS TO Q-POS.
           MOVE BULKG2-COPIES-POS TO C-POS.
           MOVE BULKG2-PRU-POS TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
           PERFORM PRU-IN.
           IF STAT NOT = "00"
              MOVE "X" TO ERRCD
              GO TO INITIALIZATION-EXIT.

           IF NOT (PRU-LP = "VDU" OR "PRU")
              MOVE "S  A310NOTFRP." TO VDU
              MOVE "# REMAINING TO BE TRANSFERRED :" TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A310AMTTFRP." TO VDU
              MOVE "$ REMAINING ................  :" TO VDUBUF
              PERFORM SCREENIO
              MOVE "SNNN060NOTFR." TO VDU
              MOVE WK-NOSEL TO VDUNUM0
              PERFORM SCREENIO
              MOVE "SNNN072AMTTFR." TO VDU
              MOVE WK-AMTSEL TO VDUNUM2
              PERFORM SCREENIO.

           IF GP-INTERIM-COUPONS = "Y"
              MOVE "S  A250COUPON-P." TO VDU
              MOVE "SET INTERIM COUPON FLAG :" TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A010COUPON." TO VDU
              MOVE "_" TO VDUBUF
              PERFORM SCREENIO.

           PERFORM ENTER-PARAMETERS.
           IF ERRCD NOT = " "
              IF ERRCD = "I"
                 GO TO SETRP
              ELSE
                 GO TO INITIALIZATION-EXIT.

      *MAKE SURE TO-BRANCH HASN'T CLOSED SINCE BEFORE ENTER-PARAMETERS
           MOVE TO-BR TO TEST-OPEN-BRANCH.
           PERFORM VERIFY-BRANCH-IS-OPEN.
           IF BR-OPEN-FG NOT = " "
              MOVE "'TO' BRANCH IS NOT OPEN FOR BUSINESS TODAY!" TO MESS
              PERFORM SEND-MESS
              MOVE "X" TO ERRCD
              GO TO INITIALIZATION-EXIT.

      *CREATE UPDATE TAGS IN SENDING & RECEIVING BRANCH'S ED DIRECTORIES
      *TO STOP BRANCHES FROM STARTING END-OF-DAYS WHILE BULK UPDATE RUNNING
       CREATE-UPDATE-TAGS.
      D    STOP MESS.
           MOVE EXT-FILPATH-BASE TO TO-TAG-ALL-BASE.
           MOVE TO-MACHINE       TO TO-TAG-ALL-MACHINE.
           MOVE FR-GR            TO TO-TAG-FROM-TO.
           INSPECT TO-TAG-FROM-TO REPLACING FIRST "       "
                                               BY "BULKUPD".
           INSPECT TO-TAG-FROM-TO REPLACING FIRST "    " BY TO-BR.
           MOVE SPACES TO SYSTEM-BUF.
           MOVE TO-TAG-CREATE TO SYSTEM-BUF.
           PERFORM SYSTEM-CALL.
           IF STAT NOT = "00"
              MOVE "CANNOT CREATE TAG FOR RECEIVING BRANCH" TO MESS
              PERFORM SEND-MESS
              MOVE "X" TO ERRCD
              GO TO INITIALIZATION-EXIT.

           MOVE FR-GR TO GR-ID.
           PERFORM SEARCH-GR-INIT.
       CREATE-NEXT-FROM-BRANCH-TAG.
           PERFORM SEARCH-GR-NEXT.
           IF WGR-SRESULT = 0
              GO TO SET-HEAD.
           MOVE WGR-SRESULT TO TEST-OPEN-BRANCH.
           PERFORM VERIFY-BRANCH-IS-OPEN.
           IF BR-OPEN-FG NOT = " "
              MOVE "'FROM' BRANCH &&&& IS NOT OPEN FOR BUSINESS TODAY"
                                                           TO MESS
              INSPECT MESS REPLACING FIRST "&&&&" BY BR-NO
              PERFORM SEND-MESS
              PERFORM REMOVE-UPDATE-TAGS
              MOVE "X" TO ERRCD
              GO TO INITIALIZATION-EXIT.

           MOVE EXT-FILPATH-BASE TO FROM-TAG-ALL-BASE.
           MOVE BR-MACHINE TO FROM-TAG-ALL-MACHINE.
           MOVE BR-NO TO FROM-TAG-FROM-TO.
           INSPECT FROM-TAG-FROM-TO REPLACING FIRST "    "
                                               BY FR-GR.
           INSPECT FROM-TAG-FROM-TO REPLACING FIRST "       "
                                               BY "BULKUPD".
           INSPECT FROM-TAG-FROM-TO REPLACING FIRST "    " BY TO-BR.
           MOVE SPACES TO SYSTEM-BUF.
           MOVE FROM-TAG-CREATE TO SYSTEM-BUF.
           PERFORM SYSTEM-CALL.
           IF STAT NOT = "00"
              MOVE "CANNOT CREATE TAG FOR 'FROM' BRANCH &&&&"
                 TO MESS
              INSPECT MESS REPLACING FIRST "&&&&" BY BR-NO
              PERFORM SEND-MESS
              MOVE "X" TO ERRCD
              GO TO INITIALIZATION-EXIT.

           GO TO CREATE-NEXT-FROM-BRANCH-TAG.

      * MUST SET UP NEW ENVIRONMENT FOR BRANCH YOU ARE UPDATING FROM!
      * TOO COMPLICATED TO UPDATED ULFILE FOR EACH FROM BRANCH IN
      * GROUP, NOT GONNA DO IT, AT LEAST FOR NOW.  BLV
      *    SET ENVIRONMENT "INTERBR" TO FR-BR.
      *    MOVE "LP/BULKG2" TO FORM-NAM.
      *    PERFORM LOGUID-CALL.

       SET-HEAD.
      
           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.
           MOVE FR-GR TO H-FRGR.
           MOVE TO-BR TO H-TOBR.
           MOVE TO-DLNO TO H-TO-DLNO.
           IF NOT (PRU-LP = "VDU" OR "PRU")
              MOVE "BRANCH TRANSFER UPDATE IN PROGRESS........."
                                                            TO MESS
              PERFORM SEND-LEGEND.

      *    IF GP-PURGE-TO-ARCHIVE-FG = "Y"
      *       PERFORM SETUP-LN-ARCHIVE-PATH.

           PERFORM OPEN-PR-FILE.

           PERFORM ZERO-TT-TOTALS.
           PERFORM ZERO-DD-TOTALS.
           PERFORM ZERO-NM-TOTALS.

       INITIALIZATION-EXIT.
           EXIT.

      ****************************************
       ENTER-PARAMETERS SECTION.
      ****************************************
       ENTER-PARM.

           IF GP-INTERIM-COUPONS NOT = "Y"
              GO TO REC-FORM.

       ENTER-COUPON.
           MOVE "E  A010COUPON." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO ENTER-PARM-ABORT.
           IF VDUSTATUS = "1>" GO TO ENTER-PARM-ABORT.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ENTER-COUPON.
           MOVE VDUBUF TO INTERIM-COUPON.
           IF INTERIM-COUPON NOT = "Y" AND NOT = "N"
              GO TO ENTER-COUPON.
       REC-FORM.
           MOVE "ENNN030FORMNO." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO ENTER-PARM-ABORT.
           IF VDUSTATUS = "1<"
              PERFORM FORM-SCAN
              IF IO-BAD
                 GO TO REC-FORM.
           IF VDUSTATUS = "1>" GO TO ENTER-PARM-ABORT.
           IF VDUSTATUS = "1U"
              IF GP-INTERIM-COUPONS = "Y"
                 GO TO ENTER-COUPON.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-FORM.
           MOVE VDUNUM0 TO FORMNO FD-FMNO.
           IF FORMNO = 0
              MOVE " " TO FD-DESC
           ELSE
              PERFORM OPEN-FD1-FILE
              PERFORM READ-FD1-FILE
              PERFORM CLOSE-FD1-FILE
              IF NOT IO-GOOD
                 GO TO REC-FORM
              ELSE
                 IF NOT (FD-1STFILE = 1 OR 2 OR 4)
                    MOVE "FIRST FILE MUST BE LN, BW, OR NO FILE"
                                                         TO MESS
                    PERFORM SEND-MESS
                    GO TO REC-FORM.
           MOVE "S  A300FORMALP." TO VDU.
           MOVE FD-DESC TO VDUBUF.
           PERFORM SCREENIO.

       ACCEPT-IT.
           MOVE "E  A010ACCEPT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO ENTER-PARM-ABORT.
           IF VDUSTATUS = "1U" GO TO REC-FORM.
           IF VDUSTATUS = "10"
              MOVE "I" TO ERRCD
              GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" GO TO ACCEPT-IT.
           IF NOT (VDUBUF = "Y"  OR "N") GO TO ACCEPT-IT.
           IF VDUBUF = "N"
              GO TO REC-FORM.
           GO TO EXIT-PARM.

       ENTER-PARM-ABORT.
           MOVE "X" TO ERRCD.

       EXIT-PARM.
           EXIT.


      ********************************
       FORM-SCAN SECTION.
      ********************************
           MOVE 0   TO FDSCAN-KEY.
           MOVE "C" TO FDSCAN-FMTYPE.
           MOVE "S" TO FDSCAN-ALLOW-CHG.
           MOVE "WI/FDSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH FDSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF FDSCAN-STATUS NOT = "00" OR FDSCAN-KEY = " "
               MOVE 9 TO IO-FG
               GO TO EXIT-FORM-SCAN.
           MOVE "SNNN030FORMNO." TO VDU.
           MOVE FDSCAN-KEY TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE FDSCAN-KEY TO VDUNUM0.

       EXIT-FORM-SCAN.
           EXIT.

      ***********************************************
       DETAIL-LINE-PROCESS SECTION.
      ***********************************************

           IF FIRST-TIME = "Y"
              MOVE EXT-CONAME-RPT-LINES TO LN-COUNT
              MOVE 1 TO LN-FEED
              MOVE "N" TO FIRST-TIME.
           IF PD-ACCTNO NOT = TO-LN-ACCTNO
              MOVE PD-ACCTNO TO DETAIL-LINE
              STRING "<------  ACCOUNT NO. IN BRANCH " FROM-BRANCH
                  DELIMITED BY SIZE INTO D-NAME-X
              MOVE 2 TO LN-FEED
              PERFORM WRITE-DETAIL-LINE.

           MOVE 1 TO BW-SUB.
           PERFORM PROCESS-BORROWER.
           PERFORM LPSTAT-DISPLAY.
           MOVE LPS-DISPLAY-LNNO TO HOLD-LPS-NUM.
           MOVE H-LPS-NUM TO D-LOAN-NUM.
           MOVE LN-1STPYDA TO D-DUEDAY.
           MOVE LN-DLNO TO D-DLNO.
           IF LN-DATE-PAID-LAST NOT = 0
              MOVE LN-DATE-PAID-LAST TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO D-PAYDATE
           ELSE
              MOVE LN-LOANDATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO D-PAYDATE
              MOVE "." TO D-DASH1 D-DASH2.

           IF RDPOT30 = "Y"
              MOVE "<P>" TO D-RECENCYX
           ELSE
           IF RDPOT30 = "M"
              MOVE "<M>" TO D-RECENCYX
           ELSE
           IF RDPOTCHGOFF = "Y"
              MOVE "<PC>" TO D-RECENCYX
           ELSE
           IF RDPOTCHGOFF = "M"
              MOVE "<PL>" TO D-RECENCYX
           ELSE
              MOVE RECDEL TO D-RECENCY.
           MOVE LN-RECPROFILE(1) TO D-PYRECPRO.
           INSPECT D-XPYRECPRO REPLACING ALL "9" BY "-".

      * DETERMINE ACCOUNT STATUS (IB-PC):
           MOVE TRANS-DATE TO IBPC-DATE.
           PERFORM IBPC-TEST.

           MOVE SAVE-UNEARN(1) TO UNEARN-INT.
           MOVE SAVE-UNEARN(3) TO UNEARN-SC.
           MOVE SAVE-UNEARN(4) TO UNEARN-MF.

           ADD UNEARN-INT UNEARN-SC UNEARN-MF GIVING D-UNEARNED.

           MOVE LN-CONPROFILE(1) TO D-PROFILE.
           INSPECT D-XPROFILE REPLACING ALL "9" BY "-".

           IF CPOT30 = "Y"
              MOVE "<P>" TO D-CONTUALX
           ELSE
           IF CPOT30 = "M"
              MOVE "<M>" TO D-CONTUALX
           ELSE
           IF CPOTCHGOFF = "Y"
              MOVE "<PC>" TO D-CONTUALX
           ELSE
           IF CPOTCHGOFF = "M"
              MOVE "<PL>" TO D-CONTUALX
           ELSE
              MOVE CONTRACTUAL TO D-CONTUAL.

           MOVE HOLD-LP-PDTH-DATE TO NUM-DATE.
           IF NOT LN-UNITPER-CD-DAYS
              MOVE NUM-YR TO NUM-DA.
           MOVE 0 TO NUM-CCYY.
           MOVE NUM-DATE TO D-PDTHRU.
           IF LN-OTHBAL NOT = 0
              MOVE LN-OTHBAL TO D-OTHBAL.
           IF LN-INTBAL NOT = 0 OR LN-LCBAL NOT = 0
              ADD LN-INTBAL LN-LCBAL GIVING D-INTBAL.
           IF LN-CURBAL NOT = 0
              MOVE LN-CURBAL TO D-CURBAL.
       DETAIL-LN-ACCTOT.
           MOVE 1 TO LEV.
           PERFORM DETAIL-LN-ACCTBL.
           MOVE 2 TO LEV.
           PERFORM DETAIL-LN-ACCTBL.
           MOVE 3 TO LEV.
           PERFORM DETAIL-LN-ACCTBL.
           PERFORM WRITE-DETAIL-LINE.
           IF PD-SSNO(2) NOT = 0
              MOVE 2 TO BW-SUB
              PERFORM PROCESS-BORROWER
              PERFORM WRITE-DETAIL-LINE.
           IF PD-SSNO(3) NOT = 0
              MOVE 3 TO BW-SUB
              PERFORM PROCESS-BORROWER
              PERFORM WRITE-DETAIL-LINE.
           IF PD-SSNO(4) NOT = 0
              MOVE 4 TO BW-SUB
              PERFORM PROCESS-BORROWER
              PERFORM WRITE-DETAIL-LINE.
           IF PD-ACCTNO NOT = TO-LN-ACCTNO
              PERFORM WRITE-DETAIL-LINE.

       END-DETAIL-LN-PROCESS.
           EXIT.

      ***********************************************
       DETAIL-LN-ACCTBL SECTION.
      ***********************************************
           ADD 1 TO T-NO(LEV SAVE-CSUB 1).
           ADD LN-OTHBAL TO T-OTH(LEV SAVE-CSUB 1).
           ADD LN-INTBAL LN-LCBAL TO T-INT(LEV SAVE-CSUB 1).
           ADD LN-CURBAL TO T-BAL(LEV SAVE-CSUB 1).
           ADD UNEARN-INT TO T-UNE(LEV SAVE-CSUB 1).
           ADD UNEARN-SC TO T-IER(LEV SAVE-CSUB 1).
           ADD UNEARN-MF TO T-ERN(LEV SAVE-CSUB 1).

           ADD 1 TO T-NO(LEV SAVE-RDSUB 2).
           ADD LN-OTHBAL TO T-OTH(LEV SAVE-RDSUB 2).
           ADD LN-INTBAL LN-LCBAL TO T-INT(LEV SAVE-RDSUB 2).
           ADD LN-CURBAL TO T-BAL(LEV SAVE-RDSUB 2).
           ADD UNEARN-INT TO T-UNE(LEV SAVE-RDSUB 2).
           ADD UNEARN-SC TO T-IER(LEV SAVE-RDSUB 2).
           ADD UNEARN-MF TO T-ERN(LEV SAVE-RDSUB 2).
           IF SAVE-PSUB = 0 OR SP-BANKRULE = "D"
              GO TO DETAIL-LN-ACCTBL-EXIT.

           ADD 1 TO T-NO(LEV SAVE-PSUB 3).
           ADD LN-OTHBAL TO T-OTH(LEV SAVE-PSUB 3).
           ADD LN-INTBAL LN-LCBAL TO T-INT(LEV SAVE-PSUB 3).
           ADD LN-CURBAL TO T-BAL(LEV SAVE-PSUB 3).
           ADD UNEARN-INT  TO T-UNE(LEV SAVE-PSUB 3).
           ADD UNEARN-SC  TO T-IER(LEV SAVE-PSUB 3).
           ADD UNEARN-MF TO T-ERN(LEV SAVE-PSUB 3).

       DETAIL-LN-ACCTBL-EXIT.
           EXIT.

      *******************************************
       PROCESS-BORROWER SECTION.
      *******************************************

           MOVE EXT-FILPATH-BASE TO BW-ALL-BASE.
           MOVE FROM-DATA-PATH   TO BW-ALL-DATA.
           MOVE FROM-MACHINE     TO BW-ALL-MACHINE.
           MOVE BW-ALL-PATH      TO BW-PATH.
           MOVE "Y"              TO BW-PATH-OVERRIDE.

           MOVE PD-OWNBR        TO BW-OWNBR.
           MOVE PD-SSNO(BW-SUB) TO BW-SSNO.
           PERFORM OPEN-BW1-FILE.
           PERFORM READ-BW1-FILE.
           PERFORM CLOSE-BW1-FILE.
           IF NOT IO-GOOD
              MOVE "**NOT ON FILE**" TO D-NAME
              GO TO END-PROCESS-BORROWER.

           MOVE BW-LNAME TO NAME-WORKER.
           MOVE BW-FNAME TO FIRST-NAME.
           INSPECT NAME-WORKER REPLACING FIRST "  " BY ",?".
           INSPECT NAME-WORKER REPLACING FIRST "                "
               BY FIRST-NAME.
           INSPECT NAME-WORKER REPLACING ALL "?" BY " ".
           MOVE NAME-WORKER TO D-NAME.

       END-PROCESS-BORROWER.
           EXIT.

      *******************************************
       VERIFY-BRANCH-IS-OPEN SECTION.
      *******************************************
           MOVE " " TO BR-OPEN-FG.

           MOVE TEST-OPEN-BRANCH TO BR-NO RC-BRNO.
           PERFORM READ-BR-FILE.
           IF IO-BAD
              MOVE "E" TO BR-OPEN-FG
              GO TO VERIFY-BRANCH-IS-OPEN-EXIT.

           IF (BR-DATA-PATH = " ") OR (BR-MACHINE = " ")
              MOVE "E" TO BR-OPEN-FG
              GO TO VERIFY-BRANCH-IS-OPEN-EXIT.

      * USE THE RCFILE TO DETERMINE IF A DAY IS OPEN.
           MOVE TRANS-DATE        TO RC-TRANS-DATE.
           PERFORM OPEN-RC1-FILE.
           PERFORM READ-RC1-FILE.
           PERFORM CLOSE-RC1-FILE.
           IF (IO-BAD) OR (RC-STATUS NOT = "A1")
               MOVE "N" TO BR-OPEN-FG.

       VERIFY-BRANCH-IS-OPEN-EXIT.
           EXIT.

      ********************************************************
      * REMOVE UPDATE TAGS FROM "FROM" BRANCHES & "TO" BRANCH
      * WILL TRY TO REMOVE ALL, EVEN THOUGH WE MAY BE
      * PERFORMING THIS WHEN WE FAILED HALF-WAY THROUGH
      * THE LOOP TO CREATE THEM....TRY TO REMOVE ALL ANYWAY
      ********************************************************
       REMOVE-UPDATE-TAGS SECTION.

           CALL "CBL_DELETE_FILE" USING TO-TAG-PATH GIVING STAT-99.
           IF STAT NOT = "00"
              MOVE "CANNOT REMOVE TAG FOR 'TO' BRANCH." TO MESS
              PERFORM SEND-MESS.

           MOVE FR-GR TO GR-ID.
           PERFORM SEARCH-GR-INIT.
       REMOVE-NEXT-FROM-BRANCH-TAG.
           PERFORM SEARCH-GR-NEXT.
           IF WGR-SRESULT = 0
              GO TO END-REMOVE-TAGS.
           MOVE WGR-SRESULT TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-BAD
              MOVE "ERROR REMOVING TAG FOR BRANCH &&&&!!" TO MESS
              INSPECT MESS REPLACING FIRST "&&&&" BY BR-NO
              PERFORM SEND-MESS
              GO TO REMOVE-NEXT-FROM-BRANCH-TAG.
           MOVE EXT-FILPATH-BASE TO FROM-TAG-ALL-BASE.
           MOVE BR-MACHINE TO FROM-TAG-ALL-MACHINE.
           MOVE BR-NO TO FROM-TAG-FROM-TO.
           INSPECT FROM-TAG-FROM-TO REPLACING FIRST "    "
                                               BY FR-GR.
           INSPECT FROM-TAG-FROM-TO REPLACING FIRST "       "
                                               BY "BULKUPD".

           INSPECT FROM-TAG-FROM-TO REPLACING FIRST "    " BY TO-BR.
           CALL "CBL_DELETE_FILE" USING FROM-TAG-PATH GIVING STAT-99.
           IF STAT NOT = "00"
              MOVE "CANNOT REMOVE TAG FOR 'FROM' BRANCH."
                 TO MESS
              INSPECT MESS REPLACING FIRST "&&&&" BY BR-NO
              PERFORM SEND-MESS.


           GO TO REMOVE-NEXT-FROM-BRANCH-TAG.

       END-REMOVE-TAGS.
           EXIT.

      ********************************************
       HEAD-PAGE SECTION.
      ********************************************

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
           MOVE HEAD3 TO PR-REC.
           PERFORM WRITE-PR-REC.
           MOVE HEAD4 TO PR-REC.
           PERFORM WRITE-PR-REC.
           IF SUMMARY-FG = "N"
              MOVE HEAD5 TO PR-REC
              PERFORM WRITE-PR-REC
              MOVE HEAD6 TO PR-REC
              PERFORM WRITE-PR-REC
              MOVE 7 TO LN-COUNT
              MOVE 2 TO LN-FEED
            ELSE
              MOVE 5 TO LN-COUNT
              MOVE 2 TO LN-FEED.
       END-HEAD-PAGE.
           EXIT.

      ********************************************
       WRITE-DETAIL-LINE SECTION.
      ********************************************
           ADD LN-FEED TO LN-COUNT.
           IF LN-COUNT > EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.
           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      ********************************************
       PRINT-RANGE-LINES SECTION.
      ********************************************
           MOVE "Y" TO SUMMARY-FG.
           MOVE EXT-CONAME-RPT-LINES TO LN-COUNT.
           MOVE 4 TO LN-FEED.

           MOVE "FORM REQUEST NO.   :" TO RANGE-LINE.
           MOVE FORMNO                 TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.


      ********************************************
       SET-BRANCH-HEADINGS SECTION.
      ********************************************

           MOVE HOLD-LN-OWNBR TO BR-NO H-BRNO.

           MOVE HOLD-TO-BRNAME TO H-BRDESC.
           MOVE "UNEARNED" TO HEAD5T.
           MOVE "CHARGES" TO HEAD6T.

      ********************************************
       CLASS-TOTALS SECTION.
      ********************************************

           MOVE "Y" TO SUMMARY-FG.
           MOVE EXT-CONAME-RPT-LINES TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE "CLASS TOTALS:" TO S-SUMNAME.
           PERFORM WRITE-DETAIL-LINE.
           MOVE 1 TO LEV.
           PERFORM PRINT-AGEING-SUMMARY.

           MOVE ZEROS TO TOTAL-TBL(1).
           MOVE "N" TO SUMMARY-FG.
           MOVE EXT-CONAME-RPT-LINES TO LN-COUNT.
           MOVE 1 TO LN-FEED.


      ********************************************
       COMPANY-TOTALS SECTION.
      ********************************************

           MOVE "Y" TO SUMMARY-FG.
           MOVE SPACES TO H-BRNOX.
           MOVE "**** ALL BRANCHES ****" TO H-BRDESC.
           MOVE EXT-CONAME-RPT-LINES TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE "COMPANY TOTALS:" TO S-SUMNAME.
           PERFORM WRITE-DETAIL-LINE.
           MOVE 3 TO LEV.
           PERFORM PRINT-AGEING-SUMMARY.

      ********************************************
       PRINT-AGEING-SUMMARY SECTION.
      ********************************************

           PERFORM ZERO-TT-TOTALS.
           PERFORM ACCUM-AGEING-TOTALS
              VARYING AGE FROM 1 BY 1 UNTIL AGE > 10
                AFTER TYP FROM 1 BY 1 UNTIL TYP > 3.
           MOVE 1 TO TYP.
           MOVE "********* CONTRACTUAL DELINQUENCY *********"
                                                       TO S-AGETYPE.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE SUMMARY1 TO DETAIL-LINE.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE 2 TO LN-FEED.
           PERFORM PRINT-DETAIL-SUM VARYING AGE
              FROM 1 BY 1 UNTIL AGE > 10.
           MOVE 2 TO LN-FEED.
           PERFORM PRINT-SUM-TOTALS.
           MOVE "  ********* RECENCY DELINQUENCY *********"
                                                      TO S-AGETYPE.
           MOVE 4 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE SUMMARY1 TO DETAIL-LINE.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE 2 TO LN-FEED.
           PERFORM PRINT-DETAIL-SUM
              VARYING AGE FROM 1 BY 1 UNTIL AGE > 10
                AFTER TYP FROM 2 BY 1 UNTIL TYP > 3.
           MOVE 2 TO TYP.
           MOVE 2 TO LN-FEED.
           PERFORM PRINT-SUM-TOTALS.
           MOVE 3 TO TYP.
           MOVE 1 TO LN-FEED.
           PERFORM PRINT-SUM-TOTALS.

      ********************************************
       PRINT-DETAIL-SUM SECTION.
      ********************************************

           IF T-NO(LEV AGE TYP) = 0
              IF T-BAL(LEV AGE TYP) = 0
                 IF T-INT(LEV AGE TYP) = 0
                    IF T-OTH(LEV AGE TYP) = 0
                       IF T-UNE(LEV AGE TYP) = 0
                          IF T-ERN(LEV AGE TYP) = 0
                             IF T-IER(LEV AGE TYP) = 0
                                GO TO END-PRINT-DETAIL-SUM.
           IF TYP = 3
              MOVE "  PAID-AHEAD" TO S-NAME
            ELSE
              MOVE AGE-DESC(AGE) TO S-NAME.
           IF T-NO(LEV AGE TYP) NOT = 0
              MOVE T-NO(LEV AGE TYP) TO S-NO
              IF TYP NOT = 3
                 IF TT-NOS(TYP) NOT = 0
                    COMPUTE S-NO-P ROUNDED =
                        ( T-NO(LEV AGE TYP) / TT-NOS(TYP) ) * 100
                   ELSE
                    MOVE 0 TO S-NO-P.
           IF T-BAL(LEV AGE TYP) NOT = 0
              MOVE T-BAL(LEV AGE TYP) TO S-BAL
              IF TYP NOT = 3
                 IF TT-BAL(TYP) NOT = 0
                    COMPUTE S-BAL-P ROUNDED =
                        ( T-BAL(LEV AGE TYP) / TT-BAL(TYP) ) * 100
                   ELSE
                    MOVE 0 TO S-BAL-P.
           IF T-INT(LEV AGE TYP) NOT = 0
              MOVE T-INT(LEV AGE TYP) TO S-INT
              IF TYP NOT = 3
                 IF TT-INT(TYP) NOT = 0
                    COMPUTE S-INT-P ROUNDED =
                        ( T-INT(LEV AGE TYP) / TT-INT(TYP) ) * 100
                   ELSE
                    MOVE 0 TO S-INT-P.
           IF T-OTH(LEV AGE TYP) NOT = 0
              MOVE T-OTH(LEV AGE TYP) TO S-OTH
              IF TYP NOT = 3
                 IF TT-OTH(TYP) NOT = 0
                    COMPUTE S-OTH-P ROUNDED =
                        ( T-OTH(LEV AGE TYP) / TT-OTH(TYP) ) * 100
                   ELSE
                    MOVE 0 TO S-OTH-P.
           IF T-UNE(LEV AGE TYP) NOT = 0
              MOVE T-UNE(LEV AGE TYP) TO S-UNE
              IF TYP NOT = 3
                 IF TT-UNE(TYP) NOT = 0
                    COMPUTE S-UNE-P ROUNDED =
                        ( T-UNE(LEV AGE TYP) / TT-UNE(TYP) ) * 100
                   ELSE
                    MOVE 0 TO S-UNE-P.
           IF T-ERN(LEV AGE TYP) NOT = 0
              MOVE T-ERN(LEV AGE TYP) TO S-ERN
              IF TYP NOT = 3
                 IF TT-ERN(TYP) NOT = 0
                    COMPUTE S-ERN-P ROUNDED =
                        ( T-ERN(LEV AGE TYP) / TT-ERN(TYP) ) * 100
                   ELSE
                    MOVE 0 TO S-ERN-P.
           IF T-IER(LEV AGE TYP) NOT = 0
              MOVE T-IER(LEV AGE TYP) TO S-IER
              IF TYP NOT = 3
                 IF TT-IER(TYP) NOT = 0
                    COMPUTE S-IER-P ROUNDED =
                        ( T-IER(LEV AGE TYP) / TT-IER(TYP) ) * 100
                   ELSE
                    MOVE 0 TO S-IER-P.
              PERFORM WRITE-DETAIL-LINE.
       END-PRINT-DETAIL-SUM.
           EXIT.

      **************************************
      *  CREATE FORM REQUESTS IN FROM BRANCH (EXCEPT FOR REGACC)
      *
      * VERY IMPORTANT THAT YOU ENTER THIS ROUTINE WITH ALL NEW LOAN
      * IN THE "TO" BRANCH INFORMATION SO THE CREATE NOTICE HAS THE OPEN
      * LOAN INFORMATION FOR REGACC, MOVES "FROM" LOAN FOR ALL OTHERS
      **************************************
       CREATE-FM-REC SECTION.
           IF LN-CONFIDENTCD = "Y" OR BW-CONFIDENTIAL = "Y"
              GO TO END-CREATE-FM-REC.

           MOVE EXT-FILPATH-BASE TO FM-ALL-BASE.
      *    IF GP-I-AM-REGACC
      *       MOVE TO-DATA-PATH TO FM-ALL-DATA
      *       MOVE TO-MACHINE TO FM-ALL-MACHINE
      *    ELSE
              MOVE FROM-DATA-PATH TO FM-ALL-DATA
              MOVE FROM-MACHINE TO FM-ALL-MACHINE.

           MOVE FM-ALL-PATH TO FM-PATH  HOLD-FM-PATH.
           MOVE "Y" TO FM-PATH-OVERRIDE.

           IF ( FD-OPTION-EXTRACTION )
              PERFORM GET-FM-PATH.

           PERFORM OPEN-FM1-FILE.

           PERFORM CLEAR-FM-FILE.

           MOVE SPACES TO LPWSFM-EDIT-FM-INITKEY.

           IF ( FD-OPTION-EXTRACTION )
      * USE ORIGINAL "FROM" OWNING BRANCH
              MOVE PD-OWNBR TO LPWSFM-EDIT-FM-BRNO2
                               LPWSFM-EDIT-FM-BW-BRNO2.

      * LN-REC HAS THE NEW "TO" BRANCH INFORMATION IN IT
           IF FD-1STFILE = 2
              MOVE LN-SSNO(1)            TO LPWSFM-EDIT-FM-SSNO
           ELSE
      * USE ORIGINAL "FROM" ACCTNO
              MOVE PD-ACCTNO          TO LPWSFM-EDIT-FM-ACCTNO
              MOVE FORMNO             TO LPWSFM-EDIT-FM-FORMNO.

           MOVE FM-PATH-OWNBR          TO FM-BRNO.
           MOVE LPWSFM-EDIT-FM-INITKEY TO FM-INITKEY.
           MOVE FORMNO                 TO FM-FMNO.

      *    IF GP-I-AM-REGACC
      *       IF ( FD-OPTION-EXTRACTION )
      * IF ACCTNO CHANGED, NEED TO MOVE IT BECAUSE THEY CREATE "TO" BRANCH INFO
      *          MOVE TO-LN-ACCTNO       TO LPWSFM-EDIT-FM-ACCTNO
      *          MOVE TO-BR              TO LPWSFM-EDIT-FM-BRNO2
      *          MOVE LPWSFM-EDIT-FM-INITKEY TO FM-INITKEY.

           MOVE FM1-KEY TO HOLD-FM1-KEY.
           PERFORM READ-FM1-FILE.

      * PRINT FORMS IN ZIP CODE ORDER
           MOVE BW-ZIP             TO FM-ZIP.
           MOVE SPACES             TO FM-PRINT.
           MOVE 0                  TO FM-STMT-COUNTER.
           MOVE TRANS-DATE         TO FM-TRDATE.
           MOVE EXT-CONAME-USER-ID TO FM-USERID.
           IF IO-GOOD
              PERFORM REWRITE-FM1-FILE
           ELSE
              MOVE HOLD-FM1-KEY TO FM1-KEY
              PERFORM WRITE-FM1-FILE.
           PERFORM CLOSE-FM1-FILE.

       CREATE-NO-REC.
           IF FD-1STFILE NOT = 4
              GO TO END-CREATE-FM-REC.

      * AT THIS POINT LN-REC HOLDS THE NEW "TO" BRANCH ACCOUNT INFO

      * REGACC WANTS THE "TO" NEW INFO BUT EVERYONE ELSE WANTS THE OLD
      * "FROM" BRANCH INFO
      *    IF NOT GP-I-AM-REGACC

           MOVE PD-RECORD TO LN-REC.

       END-CREATE-FM-REC.
           EXIT.

      * IF FORM IS AN "E" EXTRACTION TYPE FORM (FD-OPTION), THEN
      * PUT THE CORPORATE BRNO PATH IN THE FM-PATH, SO ALL FORM REQUESTS
      * ARE CREATED IN THE HOME OFFICE, THEN WHEN THEY GO "PRINT" THEM ALL
      * IT WILL CREATE AN ASCII FILE

      ********************************************
       GET-FM-PATH SECTION.
      ********************************************

           IF GP-CORP-BRNO = 0
              GO TO EXIT-GET-FM-PATH.

           MOVE BR-NO TO SAVE-BRANCH.

           MOVE GP-CORP-BRNO TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-FG = 9
              GO TO END-GET-FM-PATH.

           MOVE EXT-FILPATH-BASE TO FM-ALL-BASE.
           MOVE BR-MACHINE       TO FM-ALL-MACHINE.
           MOVE BR-DATA-PATH     TO FM-ALL-DATA.
           MOVE FM-ALL-PATH      TO FM-PATH.
           MOVE "Y"              TO FM-PATH-OVERRIDE.

       END-GET-FM-PATH.
      * GET ORIGINAL BRANCH BACK
           MOVE SAVE-BRANCH TO BR-NO.
           PERFORM READ-BR-FILE.

       EXIT-GET-FM-PATH.
           EXIT.

      ***************************************************
       VERIFY-PASSWORD SECTION.
      ***************************************************
           MOVE " " TO PASSWDW-STAT.
           MOVE "BULKGR" TO PASSWDW-PGM.
           MOVE 0 TO PASSWDW-SEQ.
           MOVE "PLEASE ENTER THE PASSWORD," TO PASSWDW-PROMPT1.
           MOVE "TO OVERRIDE YOUR BRANCH...:" TO PASSWDW-PROMPT2.
           MOVE "CL/PASSWD" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
           CANCEL FORM-PROGX.
           IF PASSWDW-STAT NOT = "OK"
              MOVE "OVERRIDES ARE NOT ALLOWED WITHOUT PASSWORD"
                                                       TO MESS
              PERFORM SEND-MESS.


       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBLP/LPEARN.CPY".
       COPY "LIBGB/SPPDTH.CPY".
       COPY "LIBLP/LPPDUE.CPY".
       COPY "LIBLP/LPCEPP.CPY".
       COPY "LIBLP/LPIBPC.CPY".
       COPY "LIBGB/AGEING.CPY".
       COPY "LIBLP/LPUPWK.CPY".
       COPY "LIBLP/LPSTAT.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBLP/LPXDTE.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/SCREEN.CPY".

       COPY "LIBLP/POSTDATE.CPY".
       COPY "LIBLP/LPSRCD.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBLP/LPLSMCL.CPY".

      *****************************************************
       FORM-RESET SECTION.
      *****************************************************
           MOVE EXT-CONAME-CONAME TO BULKG2-CONAME.
           MOVE EXT-CONAME-SYSDATE TO BULKG2-SYSDATE.
           DISPLAY BULKG2-SCREEN UPON BULKG2-WINDOW-HANDLE.
           MOVE BULKG2-FORM-FIELDS TO WR-BUF.
           MOVE BULKG2-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *****************************************************
       SETUP-LINK-INFO SECTION.
      *****************************************************
           MOVE BULKG2-HELP--FILE TO HELP-LINK-FILE.
           MOVE BULKG2-HELP--DIR  TO HELP-LINK-DIR.

      *****************************************************
       VDU-CONVERT-FIELD SECTION.
      *****************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/BULKG2_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *****************************************************
       MISC-ROUTINES SECTION.
      *****************************************************
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/GETENV.CPY".
      *COPY "LIBGB/LOGUID.CPY".
       COPY "LIBGB/RMFILE.CPY".
       EXCEPTION-ROUTINE.
           MOVE WK-ACCTNO TO D-NUMBER.
           PERFORM WRITE-DETAIL-LINE.
           PERFORM DELETE-WK-FILE.
       ZERO-TT-TOTALS.
           MOVE 1 TO SUB.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 3
              MOVE 0 TO TT-NOS(SUB) TT-BAL(SUB) TT-INT(SUB)
                        TT-OTH(SUB) TT-UNE(SUB) TT-ERN(SUB)
                        TT-IER(SUB)
           END-PERFORM.
       ZERO-DD-TOTALS.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 32
              MOVE 0 TO DD-NO(SUB) DD-BAL(SUB) DD-INT(SUB)
                        DD-OTH(SUB) DD-UNE(SUB)
           END-PERFORM.
       ZERO-NM-TOTALS.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 27
              MOVE 0 TO NM-NO(SUB) NM-BAL(SUB) NM-INT(SUB)
                        NM-OTH(SUB) NM-UNE(SUB)
           END-PERFORM.
       SET-TOBR-CONPROFILE.
           IF GP-TRANSFER-CONPROFILE = "N"
              MOVE ALL "9" TO LN-CONPROFILE(1)
                              LN-CONPROFILE(2)
                              LN-CONPROFILE(3)
                              LN-CONPROFILE(4)
                              LN-CONPROFILE(5).

      * UPDATE FROM BR LOAN STATUS FLAGS  'LOWER CASE T':
       SET-FRBR-STATUS-FG.
           SET STFG TO 1.
           SEARCH LN-STATUSFLAGS
              WHEN (LN-STATUSFLAGS(STFG) = " ")
                    MOVE X"74" TO LN-STATUSFLAGS(STFG).

      * UPDATE TO BR LOAN STATUS FLAGS  'T':
       SET-TOBR-STATUS-FG.
           SET STFG TO 1.
           SEARCH LN-STATUSFLAGS
              WHEN (LN-STATUSFLAGS(STFG) = " ")
                    MOVE "T" TO LN-STATUSFLAGS(STFG).

           IF GP-INTERIM-COUPONS = "Y"
              IF INTERIM-COUPON = "Y"
                 MOVE "Y" TO LN-INTERIM-COUPON.

       GET-RECOURSE-SUB.
           IF PD-DLRECOURSE = "F"
              MOVE 1 TO SUB
           ELSE
              IF PD-DLRECOURSE = "P"
                 MOVE 2 TO SUB
              ELSE
                 IF PD-DLRECOURSE = "N"
                    MOVE 3 TO SUB
                 ELSE
                    MOVE 0 TO SUB.

       SEND-LEGEND.
           MOVE MESS TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       SETUP-DS-DATE-CCYYMM.
           MOVE TRANS-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-YYYYMM.
           MOVE DATE-YYYYMM TO DS-DATE-CCYYMM.
       SET-TY-KEY.
           MOVE TRANS-DATE TO TY-DATE.
           MOVE PD-CLASS   TO TY-CLASS.
       PRINT-SUM-TOTALS.
           IF TYP = 3
              MOVE "  PAID-AHEAD" TO S-NAME
           ELSE
              MOVE "      TOTAL:" TO S-NAME.
           MOVE TT-NOS(TYP) TO S-NO.
           MOVE TT-BAL(TYP) TO S-BAL.
           MOVE TT-INT(TYP) TO S-INT.
           MOVE TT-OTH(TYP) TO S-OTH.
           MOVE TT-UNE(TYP) TO S-UNE.
           MOVE TT-ERN(TYP) TO S-ERN.
           MOVE TT-IER(TYP) TO S-IER.
           PERFORM WRITE-DETAIL-LINE.
       ACCUM-AGEING-TOTALS.
           ADD T-NO(LEV AGE TYP) TO TT-NOS(TYP).
           ADD T-BAL(LEV AGE TYP) TO TT-BAL(TYP).
           ADD T-INT(LEV AGE TYP) TO TT-INT(TYP).
           ADD T-OTH(LEV AGE TYP) TO TT-OTH(TYP).
           ADD T-UNE(LEV AGE TYP) TO TT-UNE(TYP).
           ADD T-ERN(LEV AGE TYP) TO TT-ERN(TYP).
           ADD T-IER(LEV AGE TYP) TO TT-IER(TYP).
       SET-CLASS-HEADINGS.
           MOVE HOLD-LN-CLASS TO H-LCNO.
           IF HOLD-LN-CLASS NOT = LC-CODE
              MOVE HOLD-LN-CLASS TO LC-CODE
              PERFORM READ-LC1-FILE
              IF NOT IO-GOOD
                MOVE "INVALID" TO LC-DESC.
           MOVE LC-DESC TO H-LCDESC.
           MOVE "N" TO SUMMARY-FG.
       RESET-TOT-PAGE.
           MOVE ZEROS TO TOTAL-TBL(3).
           MOVE "Y" TO SUMMARY-FG.
           MOVE EXT-CONAME-RPT-LINES TO LN-COUNT.
           MOVE 1 TO LN-FEED.


       COPY "LIBLP/LPTRBCL.CPY".
       COPY "LIBLP/LPFMCL.CPY".
       COPY "LIBLP/LPTYCL.CPY".
       COPY "LIBLP/LPDSCL.CPY".

      ***************************************
       IO-ROUTINE SECTION.
      ***************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPTYGS_SQL.CPY".
       COPY "LIBLP/LPTRBGS_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPNOGS_SQL.CPY".
       COPY "LIBLP/LPLXGGS_SQL.CPY".
       COPY "LIBLP/LPLXEGS_SQL.CPY".
       COPY "LIBLP/LPLXGS_SQL.CPY".
       COPY "LIBLP/LPLTPGS_SQL.CPY".
       COPY "LIBLP/LPLTLGS_SQL.CPY".
       COPY "LIBLP/LPLTIGS_SQL.CPY".
       COPY "LIBLP/LPLTFGS_SQL.CPY".
       COPY "LIBLP/LPLTDGS_SQL.CPY".
       COPY "LIBLP/LPLTCGS_SQL.CPY".
       COPY "LIBLP/LPLTBGS_SQL.CPY".
       COPY "LIBLP/LPLTAGS_SQL.CPY".
       COPY "LIBLP/LPLSXGS_SQL.CPY".
       COPY "LIBLP/LPLSRGS_SQL.CPY".
       COPY "LIBLP/LPLSPGS_SQL.CPY".
       COPY "LIBLP/LPLSMGS_SQL.CPY".
       COPY "LIBLP/LPLSLGS_SQL.CPY".
       COPY "LIBLP/LPLSJGS_SQL.CPY".
       COPY "LIBLP/LPLSGGS_SQL.CPY".
       COPY "LIBLP/LPLSFGS_SQL.CPY".
       COPY "LIBLP/LPLSDGS_SQL.CPY".
       COPY "LIBLP/LPLPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLMGS_SQL.CPY".
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBLP/LPFMGS_SQL.CPY".
       COPY "LIBLP/LPFDGS_SQL.CPY".
       COPY "LIBLP/LPESGS_SQL.CPY".
       COPY "LIBLP/LPDSGS_SQL.CPY".
       COPY "LIBLP/LPBXGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBLP/LPBMGS_SQL.CPY".
       COPY "LIBGB/GBRCGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBGBGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBGBIN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBRC1RN.CPY".
       COPY "LIBLP/LPLC1RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBLP/LPLX1IN.CPY".
       COPY "LIBLP/LPLXG1IN.CPY".
       COPY "LIBLP/LPLXE1IN.CPY".
       COPY "LIBLP/LPLN1IN.CPY".
       COPY "LIBLP/LPLM1IN.CPY".
       COPY "LIBLP/LPLP1IN.CPY".
       COPY "LIBLP/LPBW1IN.CPY".
       COPY "LIBLP/LPBM1IN.CPY".
       COPY "LIBLP/LPTY1IN.CPY".
       COPY "LIBLP/LPDS1IN.CPY".
       COPY "LIBLP/LPWKIN.CPY".
       COPY "LIBLP/LPTRB1IN.CPY".
       COPY "LIBLP/LPFD1RN.CPY".
       COPY "LIBLP/LPFM1IN.CPY".
       COPY "LIBLP/LPNO1IN.CPY".
       COPY "LIBLP/LPLTA1IN.CPY".
       COPY "LIBLP/LPLTB1IN.CPY".
       COPY "LIBLP/LPLTC1IN.CPY".
       COPY "LIBLP/LPLTD1IN.CPY".
       COPY "LIBLP/LPLTF1IN.CPY".
       COPY "LIBLP/LPLTI1IN.CPY".
       COPY "LIBLP/LPLTL1IN.CPY".
       COPY "LIBLP/LPLTP1IN.CPY".
       COPY "LIBLP/LPLSD1IN.CPY".
       COPY "LIBLP/LPLSF1IN.CPY".
       COPY "LIBLP/LPLSG1IN.CPY".
       COPY "LIBLP/LPLSJ1IN.CPY".
       COPY "LIBLP/LPLSL1IN.CPY".
       COPY "LIBLP/LPLSM1IN.CPY".
       COPY "LIBLP/LPLSP1IN.CPY".
       COPY "LIBLP/LPLSR1IN.CPY".
       COPY "LIBLP/LPLSX1IN.CPY".
       COPY "LIBLP/LPBX1IN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBLP/LPES1IN.CPY".

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      *********************************************
       END-PROGRAM SECTION.
      *********************************************
       END-RUN.
           PERFORM CLOSE-FILES.
           PERFORM PRU-OUT.
           IF NO-RECORDS-FOUND
              MOVE "NO RECORDS FOUND FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS.
       EXIT-PROG.
       STOP-RUN.
           MOVE SPACES TO BICREA-CALLED-FROM.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY BULKG2-SCREEN.
           DESTROY BULKG2-WINDOW-HANDLE.
           EXIT PROGRAM.
