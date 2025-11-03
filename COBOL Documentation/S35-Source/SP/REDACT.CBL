       IDENTIFICATION DIVISION.
       PROGRAM-ID.      REDACT.
       DATE-WRITTEN.    071114.
      *******************************************************************
      *
      * SSNO IN WKFILE & UPDATE WITH NEW SSNO, NAME, ADDRESS.  
      *
      *     *****************  NOTE!! *************************
      *     THIS PROGRAM USES THE RANDOM FUNCTION WHICH REQUIRES 
      *     RUNTIME OF AT LEAST 5.0.  SO IN A15, COMPILE WITH
      *     CB500.SH, WHICH HAS THE Z31 ARGUMENT SET TO 5.0 INSTEAD
      *     OF 3.1.  TO RUN, USE REDACT.SH (IN SH) IF YOU ARE ON
      *     A SYSTEM RUNNING A 3.1 RUNTIME, OTHERWISE, YOU CAN 
      *     JUST DO TCLP SP/REDACT.
      *     NOTE: JKC 2019-0531 SINCE THE SP/REDACT IS NOW COMPILED
      *           ON LAPTOP/PC, THE COMPILE SHELL IN CYGWIN (CB.SH)
      *           CALLS TO PFS_ECLIPSEACUSINGLECOMPILE.BAT WHICH DOES
      *           THE ACTUAL COMPILE.  SO MADE A CHANGE TO THE .BAT
      *           SCRIPT TO CHECK IF IT IS A15 AND REDACT PROGRAM
      *           WHICH WILL USE -Z80 NOT -Z31 OPTION!!!
      *
      *                 (LP)
      *  DESC:   CHANGE BORROWER SSNO, NAME, & ADDRESS TO RANDOM
      *          VALUES FOR TESTING.  OTHER CONFIDENTIAL ITEMS 
      *          WILL ALSO BE BLANKED OUT OR CHANGED, SUCH AS
      *          BANK ACCOUNT & ROUTING NUMBERS, PAYROLL INFO, ETC.
      *          SOME FILES, LIKE BORROWER MEMO OR LOAN MEMO, CANNOT
      *          BE REDACTED & WILL JUST GET RECREATED, EMPTY.
      *
      *  USAGE:  FIRST RUN THE SHELL TO CREATE TARGET DIRECTORY
      *          WHERE FILES WILL BE REDACTED & COPY GLOBAL FILES THERE.
      *          FOR EXAMPLE, YOU MIGHT COPY FILES FROM BEOM TO TEST.
      *          THE SHELL IS REDACT_CP.SH.   
      *
      *          NEXT, SET YOUR "FIL" ENVIRONMENT VARIABLE TO THE
      *          TARGET DATA SET, AND RUN THIS PROGRAM.   THE DATA
      *          IN THIS DIRECTORY WILL BE REDACTED.
      *          (PROGRAM NOT ALLOWED TO RUN IN "DATA"). 
      *          
      *          RUN THE FILE CREATE UTILITIES WITH OPTION #2 TO
      *          CREATING MISSING FILES:
      *            RUN GB/CRMAST TO CREATE MISSING GLOBAL FILES
      *            RUN GB/CRDATA TO CREATE MISSING LOCAL FILES (RUN
      *              FOR EACH BRANCH YOU PROCESSED
      *            IF RUNNING NOTES:
      *              RUN NP/CRDAT3 TO CREATE MISSING NOTES FILES
      *            IF RUNNING AP:
      *              RUN AP/CRDAT2 TO CREATE MISSING AP FILES
      *
      *          RUN RE-CREATE BORROWER INDEX FOR THE NEW SSNO'S.
      *          FOR ANY BRANCHES YOU HAVE REDACTED
      *
      *   PROGRAM FLOW:
      *
      *     FOR EACH BRANCH IN BRANCH OR GROUP RANGE, THE PATHS
      *     ARE SET UP (BW, BX & IV) ARE OPENED TO OUTPUT IN THE
      *     TARGET DIRECTORY, & THEN READ FROM THE SOURCE
      *     DIRECTORY.  BEFORE PROCESSING THE 1ST BRANCH, THE
      *     GLOBAL STUFF GETS REDACTED FIRST: BR-FILE, GP-FILE
      *     EM-FILE, VN-FILE, AND NOTES).  THEN LOCAL FILES
      *     GET REDACTED:
      *
      *     BRANCH SOURCE & TARGET PATHS ARE PASSED TO A SHELL,
      *     REDACT.SH, WHICH COPIES THE LOCAL FILES, THEN:
      *     MOVE THE SOURCE BORROWER INFO TO THE TARGET BORROWER
      *     RANDOMIZE BORROWER INFO, SSNO, NAME, ADDR, BIRTHDATE,
      *                              PHONE NUMBERS, EMPLOYER INFO
      *     WRITE NEW BORROWER IN TARGET FILE WITH RANDOMIZED STUFF
      *     WRITE NEW BORROWER TRAILER IN TARGET FILE WITH NEW SSNO
      *                                      & BLANKED OUT KYC STUFF.
      *     SAVE THE NEW SSNO, NAME & ADDRESS STUFF IN A WORK FILE
      *                                         INDEXED BY THE OLD SSNO.
      *     UPDATE THE NEW SSNO TO LN4-FILE
      *     FOR THE ACCOUNT I JUST UPDATED, NOW UPDATE THE NEW SSNO
      *          & INFO TO CORRESPONDING RECORDS IN LEFILE, LTFILE & EPFILE
      *     UPDATE THE NEW SSNO TO LN5-FILE, LN6-FILE, LN7-FILE, AND LH-FILE.
      *     IF WORLD, UPDATE THE NEW SSNO TO CI2-FILE & CI3-FILE.

      *  THEN, AFTER ALL THE BORROWERS ARE PROCESSED:
      *     UPDATE SPOUSE SSNO'S (READ ALL NEW TARGET BORROWERS, IF THEY
      *     HAVE A SPOUSE SSNO, READ WORK FILE BY OLD SSNO & UPDATE SSNO & NAME)
      *     UPDATE CKFILE, TRFILE (FOR MONTHS SELECTED), DLFILE, MAFILE, PAFILE.
      *
      * REV:
      * BLM 140909 SUPPORT PR# 4564
      * BLM 141219 FIX SPOUSE ROUTINE.  IF BW-SPSSNO IS NOT A BORROWER,
      *            RANDOMIZE SPOUSE INFO.  ADDED INTERUPT LOGIC.
      * BLM 141219 MOVE 123 TO EP ROUTING #, BR ROUTING #
      * BLM 150105 IF UPDATING GLOBAL, REDACT BANK INFO IN ALL BRANCH
      *            RECORDS, NOT JUST TARGET BRANCH.  FIXED GP BANK INFO.
      * BLM 150122 FIXED MISSING INITIALIZATION ON A FEW STRINGS; THIS
      *            CAUSED POTENTIAL GARBAGE ON THE END OF EMPLOYEE
      *            SS NAME & PREFERRED NAME, NOTES ALPHA FIELDS, AND
      *            INVESTOR SALUTATION FIELDS.
      * BLM 150402 CHANGED TO CROSS DATA PATHS, INSTEAD OF RUNNING FOR
      *            ONE BRANCH AT A TIME.  ADDED A SHELL, REDACT.SH, TO
      *            COPY A BRANCH'S FILES FROM SOURCE TO TARGET BEFORE
      *            THAT BRANCH IS REDACTED.  SUPPORT, PR# 4564B
      *  CS 151216 ADD LP-LTOUCH-DATE
      *
      * KEC 2015.1218 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         FIXED KEYS & RECS FOR IV-REC.
      *
      * KEC 2015.1222 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         MERGED IT-REC INTO IV-REC. REMOVED IT-REC FROM HERE.
      *  BAH 160128 SPLIT OUT LTFILE, PD0003
      *   CS 160511 REMOVED REDEFINE OF LE-FLAG/LE-SSNO AND MADE SEPARATE
      *             FIELDS.
      *  BAH 181009 GLOBAL CKFILE, L2G
      *  BAH 181009 GLOBAL CIFILE, L2G
      *  BAH 181012 GLOBAL DLFILE, L2G
      *  BAH 181030 GLOBAL LEFILE, L2G
      *  BAH 181127 GLOBAL LHFILE, L2G
      *  BAH 181217 GLOBAL LNFILE, L2G
      *  BAH 190103 GLOBAL LPFILE, L2G
      * KEC 2019-0128 A32 - GPFILE GP-REC MODIFICATION G2G FLR
      *  AMM 190206 GLOBAL PAFILE, L2G
      * KEC 190530 PR#1374 ADDED EO BATCH LOGIC (EOBL)
      *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
       COPY "LIBGB/GBFSWKI.CPY".

           SELECT SRC-BW-FILE ASSIGN TO SRC-BW-PATH
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY SRC-BW1-KEY
                  ALTERNATE RECORD KEY SRC-BW2-KEY
                                     = SRC-BW-OWNBR
                                       SRC-BW-LNAME
                                       SRC-BW-FNAME
                                       WITH DUPLICATES
                  ALTERNATE RECORD KEY SRC-BW3-KEY
                                     = SRC-BW-OWNBR
                                       SRC-BW-SPSSNO
                                       WITH DUPLICATES
                  ALTERNATE RECORD KEY SRC-BW4-KEY
                                     = SRC-BW-OWNBR
                                       SRC-BW-SPLNAME
                                       SRC-BW-SPFNAME
                                       WITH DUPLICATES
                  ALTERNATE RECORD KEY SRC-BW5-KEY WITH DUPLICATES
                  FILE STATUS FILE-STAT.
           SELECT SRC-BX-FILE ASSIGN TO SRC-BX-PATH
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY SRC-BX1-KEY
                  FILE STATUS FILE-STAT.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".

       FD  WK-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK-REC.
           03  WK-KEY.
               05  WK-OLD-SSNO       PIC 9(9).
           03  WK-NEW-SSNO           PIC 9(9).
           03  WK-NEW-FNAME          PIC X(16).
           03  WK-NEW-LNAME          PIC X(16).
           03  WK-NEW-BDATE          PIC 9(8).
           03  WK-NEW-ADR1           PIC X(24).
           03  WK-NEW-ADR2           PIC X(24).
           03  WK-NEW-CITY           PIC X(15).
           03  WK-NEW-STATE          PIC XX.
           03  WK-NEW-ZIP            PIC X(10).

      **************************************************
      *            SOURCE BORROWER FILE
      *   SRC-BW-REC FROM 'REPLACING LEADING' ON BW-REC.
      **************************************************
       FD  SRC-BW-FILE
           LABEL RECORDS ARE STANDARD.
           COPY "LIBLP/LP01BW.CPY" REPLACING LEADING "BW" BY "SRC-BW".

      **************************************************
      *            SOURCE BORROWER TRAILER FILE
      *   SRC-BX-REC FROM 'REPLACING LEADING' ON BX-REC.
      **************************************************
       FD  SRC-BX-FILE
           LABEL RECORDS ARE STANDARD.
           COPY "LIBLP/LP01BX.CPY" REPLACING LEADING "BX" BY "SRC-BX".

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)SP/REDACT.CBL S35 05/02/24-10:32:49 >".
      ***********************************
      *    PROGRAM WORKERS
      ***********************************
       COPY "LIBGB/GB01GP.CPY".
       COPY "LIBGB/GB01GP_SQL.CPY".
       COPY "LIBGB/GBWSGP.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".

       COPY "LIBLP/LP01TRB.CPY".
       COPY "LIBLP/LP01TRB_SQL.CPY".
       COPY "LIBLP/LPWSTRB.CPY".
       COPY "LIBLP/LP01TRC.CPY".
       COPY "LIBLP/LP01TRC_SQL.CPY".
       COPY "LIBLP/LPWSTRC.CPY".
       COPY "LIBLP/LP01TRJ.CPY".
       COPY "LIBLP/LP01TRJ_SQL.CPY".
       COPY "LIBLP/LPWSTRJ.CPY".
       COPY "LIBLP/LP01TRK.CPY".
       COPY "LIBLP/LP01TRK_SQL.CPY".
       COPY "LIBLP/LPWSTRK.CPY".
       COPY "LIBLP/LP01TRL.CPY".
       COPY "LIBLP/LP01TRL_SQL.CPY".
       COPY "LIBLP/LPWSTRL.CPY".
       COPY "LIBLP/LP01TRP.CPY".
       COPY "LIBLP/LP01TRP_SQL.CPY".
       COPY "LIBLP/LPWSTRP.CPY".
       COPY "LIBLP/LP01TRS.CPY".
       COPY "LIBLP/LP01TRS_SQL.CPY".
       COPY "LIBLP/LPWSTRS.CPY".
       COPY "LIBLP/LP01TRX.CPY".
       COPY "LIBLP/LP01TRX_SQL.CPY".
       COPY "LIBLP/LPWSTRX.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01BX.CPY".
       COPY "LIBLP/LP01BX_SQL.CPY".
       COPY "LIBLP/LPWSBX.CPY".
       COPY "LIBLP/LP01CI.CPY".
       COPY "LIBLP/LP01CI_SQL.CPY".
       COPY "LIBLP/LPWSCI.CPY".
       COPY "LIBLP/LP01CK.CPY".
       COPY "LIBLP/LP01CK_SQL.CPY".
       COPY "LIBLP/LPWSCK.CPY".
       COPY "LIBLP/LP01DL.CPY".
       COPY "LIBLP/LP01DL_SQL.CPY".
       COPY "LIBLP/LPWSDL.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01LP.CPY".
       COPY "LIBLP/LP01LP_SQL.CPY".
       COPY "LIBLP/LPWSLP.CPY".
       COPY "LIBLP/LP01LH.CPY".
       COPY "LIBLP/LP01LH_SQL.CPY".
       COPY "LIBLP/LPWSLH.CPY".
       COPY "LIBLP/LP01LE.CPY".
       COPY "LIBLP/LP01LE_SQL.CPY".
       COPY "LIBLP/LPWSLE.CPY".
       COPY "LIBLP/LP01LTB.CPY".
       COPY "LIBLP/LP01LTB_SQL.CPY".
       COPY "LIBLP/LPWSLTB.CPY".
       COPY "LIBLP/LP01LTL.CPY".
       COPY "LIBLP/LP01LTL_SQL.CPY".
       COPY "LIBLP/LPWSLTL.CPY".
       COPY "LIBLP/LP01PA.CPY".
       COPY "LIBLP/LP01PA_SQL.CPY".
       COPY "LIBLP/LPWSPA.CPY".

       01  WK-IFN                      PIC X(2) VALUE "WK".

       01  SRC-BW-PATH.
           03  SRC-BW-PATH1            PIC X(12).
           03  FILLER                  PIC X     VALUE "/".
           03  SRC-BW-ALL-DATA         PIC X(6).
           03  FILLER                  PIC X(10) VALUE
               "/GB/BWFILE".
       01  SRC-BX-PATH.
           03  SRC-BX-PATH1            PIC X(12).
           03  FILLER                  PIC X     VALUE "/".
           03  SRC-BX-ALL-DATA         PIC X(6).
           03  FILLER                  PIC X(10) VALUE
               "/GB/BXFILE".
      *01  SRC-IV-PATH.
      *    03  SRC-IV-PATH1            PIC X(12).
      *    03  FILLER                  PIC X(10) VALUE
      *        "/NP/IVFILE".
      *********************************************************
      *   DATA PATH FROM GETENV ON "FIL"
      *********************************************************
       01  DATA-PATH                   PIC X(35).
       01  DATA-PATH-FIL-BUFFER        REDEFINES DATA-PATH.
           03  FILLER                  PIC X(4).
           03  DATA-PATH-DATA          PIC X(4).
           03  FILLER                  PIC X(10).

      ******************************************************************
      *
      *   S C R E E N - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/EOBUF_EXT.CPY".
      *-----------------------------------------------------------------
           03  SOURCE-PATH.
               05  SOURCE-PATH1        PIC X(12).
               05  SOURCE-PATH-SLASH1  PIC X(1).
               05  SOURCE-PATH-DATA    PIC X(6).

           03  TARGET-PATH.
               05  TARGET-PATH1.
                   07  TARGET-PATH-SLASH1   PIC X.
                   07  TARGET-FIL           PIC X(11).
               05  TARGET-PATH-SLASH2       PIC X(1).
               05  TARGET-PATH-DATA         PIC X(6).
    
           03  BEG-BRANCH              PIC 9(4).
           03  END-BRANCH              PIC 9(4).

           03  GROUP-OPTION-WORKERS.
               05  GROUP-FLAG          PIC X.
               05  ENT-GROUP           PIC X(4).

           03  BEG-DATE                PIC 9(4).
           03  FILLER REDEFINES BEG-DATE.
               05  BEG-MM              PIC 99.
               05  BEG-YY              PIC 99.

           03  END-DATE                PIC 9(4).
           03  FILLER REDEFINES END-DATE.
               05  END-MM              PIC 99.
               05  END-YY              PIC 99.

           03  SKIP-LARGE-FG           PIC X.

      *=================================================================
       01  VERSION-CONTROL             PIC X(01)     VALUE "A".

       01  SAVE-SCREEN-RANGES.
           03  SV-BEG-BRANCH           PIC 9(04)     VALUE 9999.
           03  SV-END-BRANCH           PIC 9(04)     VALUE 9999.

           03  SV-GROUP-OPTION.
               05  SV-GROUP-FLAG       PIC X(01).
               05  SV-ENT-GROUP        PIC X(04).

      *-----------------------------------------------------------------
       01  UPDATE-GLOBAL-FG PIC X      VALUE "Y".

      *-----------------------------------------------------------------
       01  BEGINNING-DATE  PIC 9(8).
       01  ENDING-DATE     PIC 9(8).

      ******************************************************************
       01  LN-WORKERS.
           03  LN-FEED          PIC 99    VALUE 0.
           03  LN-COUNT         PIC 999   VALUE 99.
           03  LN-WK            PIC 999   VALUE 0.

       01  PAGE-NUMBER          PIC 9(4)  VALUE 0.

      *  "/$REL/SH/EC /$REL/SH/REDACT.SH".
       01  REDACT-CMD.
           03  FILLER             PIC X     VALUE "/".
           03  REDACT-REL         PIC X(7).
           03  FILLER             PIC X(4)  VALUE "/SH/".
           03  REDACT-EC          PIC X(3)  VALUE "EC ".
           03  FILLER             PIC X     VALUE "/".
           03  REDACT-REL2        PIC X(7).
           03  FILLER             PIC X(4)  VALUE "/SH/".
           03  REDACT-SHELL       PIC X(10) VALUE "REDACT.SH ".
           03  REDACT-SOURCE-PATH PIC X(19).
           03  FILLER             PIC X     VALUE " ".
           03  REDACT-TARGET-PATH PIC X(19).
           03  FILLER             PIC X     VALUE " ".
           03  REDACT-TRMMYY1     PIC 9(4).
           03  FILLER             PIC X     VALUE " ".
           03  REDACT-TRMMYY2     PIC 9(4).
           03  FILLER             PIC X     VALUE " ".
           03  REDACT-SKIP-LARGE  PIC X.

       01  ERRCD             PIC X.
       01  IX1               PIC 99      COMP-3.
       01  IX2               PIC 99      COMP-3.
       01  TYP               PIC 9       VALUE 0.
       01  ONE               PIC S9.
       01  ELE               PIC 9(2).
       01  ACTION.
           03  CHKEY         PIC X(4).
           03  CHNO          PIC 9(2).
       01  IO-TYPE           PIC X(3).
       01  RTNCD             PIC X(3).
       01  NEW-FLAG          PIC 9.
       01  FIRST-TIME        PIC 9       VALUE 0.
       01  FIRST-SCAN        PIC 9       VALUE 0.
       01  NEW-SSNO-EXISTS   PIC X       VALUE SPACES.
       01  NAME-WORKER       PIC X(36).
       01  D-CHECK-NO        PIC 9(6).
       01  D-SSNO            PIC 9999/99/9999.
       01  SUB               PIC 99.
       01  FNAME-SUB         PIC 999     VALUE 0.
       01  LNAME-SUB         PIC 999     VALUE 0.
       01  COUNTER           PIC 99      VALUE 0.
       77  D-RANDOM-10       PIC 9(10).
       77  RANDOM-10         PIC S9V9(10).
       77  D-RANDOM-9        PIC 9(9).
       01  RANDOM-WK         PIC 9(10).
       01  FILLER REDEFINES RANDOM-WK.
           03  FILLER        PIC X.
           03  RANDOM-9      PIC 9(9).
       01  FILLER REDEFINES RANDOM-WK.
           03  RANDOM-3A     PIC 999.
           03  RANDOM-3B     PIC 999.
           03  RANDOM-3C     PIC 999.
           03  RANDOM-1      PIC 9.
       01  FILLER REDEFINES RANDOM-WK.
           03  RANDOM-4      PIC 9(4).
           03  RANDOM-3D     PIC 9(3).
           03  RANDOM-3E     PIC 9(3).
       01  FILLER REDEFINES RANDOM-WK.
           03  RANDOM-2A     PIC 99.
           03  RANDOM-2B     PIC 99.
           03  RANDOM-2C     PIC 99.
           03  RANDOM-2D     PIC 99.
           03  RANDOM-2E     PIC 99.
       01  NEW-SSNO          PIC 9(9).
       01  FILLER REDEFINES NEW-SSNO.
           03  SS-1        PIC 9(3).
           03  SS-2        PIC 99.
           03  SS-3        PIC 9(4).
       01  FILLER REDEFINES NEW-SSNO.
           03  SS-FIRST4   PIC 9(4).
           03  SS-LAST5    PIC 9(5).
       01  FILLER REDEFINES NEW-SSNO.
           03  SS-CHAR1    PIC 9.
           03  FILLER      PIC 9(8).

       01  NEW-BIRTHDATE     PIC 9(8).
       01  TODAYS-DATE       PIC 9(8).
       01  CENTURY-WK        PIC 9(4) VALUE 2014.
       01  FILLER REDEFINES CENTURY-WK.
           03  CENTURY-1     PIC 99.
           03  CENTURY-2     PIC 99.
       01  BIRTH-YEAR        PIC 9(4).
       01  D-DATE            PIC 99/99/99.
       01  TIME-NOW          PIC 9(8).
       01  FILLER REDEFINES TIME-NOW.
           03  TIME-NOW-HR   PIC 99.
           03  TIME-NOW-MN   PIC 99.
           03  TIME-NOW-SSHH.
               05  TIME-NOW-SS   PIC 99.
               05  TIME-NOW-HH   PIC 99.
           03  TIME-NOW-SS-HH PIC 9(4) REDEFINES TIME-NOW-SSHH.
       01  RETRY-COUNTER     PIC 99.
       01  NEW-FNAME         PIC X(16).
       01  NEW-LNAME         PIC X(16).
       01  NEW-ADDR1         PIC X(40).
       01  NEW-ADDR-NO       PIC Z(4).
       01  NEW-ADDR-ST       PIC X(12).
       01  NEW-ADDR-TYPE     PIC X(12).
       01  NEW-ADDR2         PIC X(24).
       01  NEW-CITY          PIC X(16).
       01  NEW-STATE         PIC X(16).
       01  NEW-ZIP           PIC X(16).
       01  TEST-TR-CODE      PIC X.
           88  TR-TYPE-HAS-SSNO VALUE "B" "E" "F" "G" "J" "K"
                                      "L" "P" "S" "X".

       01  ALREADY-EXISTS-MESS.
           03  FILLER         PIC X(44) VALUE
           "SSNO # ALREADY EXISTS !!".
       01  ALREADY-EXISTS-MESS2.
           03  FILLER         PIC X(43) VALUE
           "ENTER 'YES' TO OVERWRITE, OR PRESS <ENTER> ".
           03  FILLER         PIC X(17) VALUE
           "TO CANCEL ".
 
 
       01  ORIGINAL-BW-REC   PIC X(BW-SIZE).
           COPY "LIBLP/LP01BM.CPY" REPLACING LEADING "BM" BY "HOLD-BM".
           COPY "LIBLP/LP01BX.CPY" REPLACING LEADING "BX" BY "HOLD-BX".
           COPY "LIBLP/LP01LE.CPY" REPLACING LEADING "LE" BY "HOLD-LE".
           COPY "LIBLP/LP01LH.CPY" REPLACING LEADING "LH" BY "HOLD-LH".
      
       01  FIRST-NAMES.
           03  FILLER        PIC X(48) VALUE
           "MARY        JOSEPH      MARTIN      WILLIAM     ".
           03  FILLER        PIC X(48) VALUE
           "SUSAN       MARY ELLEN  BILLY JO    LORETTA     ".
           03  FILLER        PIC X(48) VALUE
           "JONATHON    EDWIN       ALICE       THOMAS      ".
           03  FILLER        PIC X(48) VALUE
           "JUANITA     MARCUS      MICHAEL     BETH ANN    ".
           03  FILLER        PIC X(48) VALUE
           "JAMES J.    SALLY       NANCY       KATHERINE   ".
           03  FILLER        PIC X(48) VALUE
           "WILLIS J.   RUSSELL     SAMUEL      PHILLIP P.  ".
           03  FILLER        PIC X(48) VALUE
           "DALE        GARY        ALICIA      BARBARA     ".
           03  FILLER        PIC X(48) VALUE
           "LUCAS       PATRICIA    STEVEN      ANTHONY     ".
           03  FILLER        PIC X(48) VALUE
           "PETER       RACHEL      CLAIRE      FRANK       ".
           03  FILLER        PIC X(48) VALUE
           "ERIC        JEFFREY     JEANNIE     ELIZABETH   ".
           03  FILLER        PIC X(48) VALUE
           "MERRILL     MERRY       HOWARD      GOLDIE      ".
           03  FILLER        PIC X(48) VALUE
           "BONNIE      BARRY       ALVIE       BEV         ".
           03  FILLER        PIC X(48) VALUE
           "JAMES J.    LYNN        LESLIE      ROBIN       ".
           03  FILLER        PIC X(48) VALUE
           "WILLARD     JOANNE      CHRISTINA   MARJORY A.  ".
           03  FILLER        PIC X(48) VALUE
           "PHILLIP     JAY         EDWARD      JOEL        ".
           03  FILLER        PIC X(48) VALUE
           "PAMELA      ANNA        KARL        CARLA S.    ".
           03  FILLER        PIC X(48) VALUE
           "ASHTON      ROBIN       CHARLIE     WALLACE     ".
           03  FILLER        PIC X(48) VALUE
           "JACOB       NOAH        SETH        DARREN      ".
           03  FILLER        PIC X(48) VALUE
           "DERREK      NORMAN      JAMIE       GEORGE      ".
           03  FILLER        PIC X(48) VALUE
           "RANDALL     AXEL        BRENT       JUDSON      ".
           03  FILLER        PIC X(48) VALUE
           "DANIELLE    SARAH       KATIE       DINAH       ".
           03  FILLER        PIC X(48) VALUE
           "GRACIE LOU  LYNETTE     MIRANDA     JILLIAN     ".
           03  FILLER        PIC X(48) VALUE
           "SANDRA      MARGARET    MARGUERITE  CAITLIN     ".
           03  FILLER        PIC X(48) VALUE
           "WILLA       CAROLYN     KITTY       NICHOLE     ".
           03  FILLER        PIC X(48) VALUE
           "JUDITH      MARILYN     ELLEN       PATRICK     ".
           03  FILLER        PIC X(48) VALUE
           "NOAH        SOPHIA      LIAM        EMMA        ".
           03  FILLER        PIC X(48) VALUE
           "JACOB       OLIVIA      MASON       ISABELLA    ".
           03  FILLER        PIC X(48) VALUE
           "AVA         ETHAN       MIA         MICHAEL     ".
           03  FILLER        PIC X(48) VALUE
           "EMILY       ALEXANDRA   ABIGAIL     JAYDEN      ".
           03  FILLER        PIC X(48) VALUE
           "MADISON     ELIJAH      CHARLOTTE   AIDEN       ".
           03  FILLER        PIC X(48) VALUE
           "AVERY       SOFIA       BEN         CHLOE       ".
           03  FILLER        PIC X(48) VALUE
           "ELLA        JACKSON     HARPER      LOGAN       ".
           03  FILLER        PIC X(48) VALUE
           "AMELIA      CINDY       DREW        ANTHONY     ".
           03  FILLER        PIC X(48) VALUE
           "ADDISON     JOSEPH      EVELYN      JOSHUA      ".
           03  FILLER        PIC X(48) VALUE
           "NATALIE     ANDREW      JODY        HANNAH      ".
           03  FILLER        PIC X(48) VALUE
           "ZOEY        GABRIEL     SAMUEL      VICTORIA    ".
           03  FILLER        PIC X(48) VALUE
           "LILLIAN     CHRISTOPHER LILY        DYLAN       ".
           03  FILLER        PIC X(48) VALUE
           "BROOKLYN    ESTHER      SAMANTHA    LAYLA       ".
           03  FILLER        PIC X(48) VALUE
           "NATHAN      ZOE         CARTER      AUDREY      ".
           03  FILLER        PIC X(48) VALUE
           "LEAH        CALEB       LUKE        ALLISON     ".
           03  FILLER        PIC X(48) VALUE
           "CHRISTIAN   ANNA        HUNTER      AALIYAH     ".
           03  FILLER        PIC X(48) VALUE
           "HENRY       SAVANNAH    GEORGIA     OWEN        ".
           03  FILLER        PIC X(48) VALUE
           "GABRIELLA   CAMILIA     LANDON      ARIANNA     ".
           03  FILLER        PIC X(48) VALUE
           "JACK        WYATT       KAYLEE      SCARLET     ".
           03  FILLER        PIC X(48) VALUE
           "JONATHAN    HAILEY      ELI         ARIA        ".
           03  FILLER        PIC X(48) VALUE
           "ISAIAH      RILEY       SEBASTIAN   JAXON       ".
           03  FILLER        PIC X(48) VALUE
           "ALEXIS      BRAYDEN     SARAH       CLAIRE      ".
           03  FILLER        PIC X(48) VALUE
           "JULIAN      LEVI        GAVIN       NEVAEH      ".
           03  FILLER        PIC X(48) VALUE
           "SADIE       AARON       PEYTON      AUBREE      ".
           03  FILLER        PIC X(48) VALUE
           "OLIVER      JORDAN      CONNOR      EVAN        ".
           03  FILLER        PIC X(48) VALUE
           "NICHOLAS    PENELOPE    SERENITY    CHARLES     ".
           03  FILLER        PIC X(48) VALUE
           "ALLYSSA     BELLA       JEREMIAH    TAYLOR      ".
           03  FILLER        PIC X(48) VALUE
           "CAMERON     ADRIANNE    ALEXA       KYLIE       ".
           03  FILLER        PIC X(48) VALUE
           "THOMAS      ROBERT      MACKENZIE   CAROLINE    ".
           03  FILLER        PIC X(48) VALUE
           "TYLER       COLTON      AUTUMN      LUCY        ".
           03  FILLER        PIC X(48) VALUE
           "JACE        AUSTIN      ANGEL       ASHLEY      ".
           03  FILLER        PIC X(48) VALUE
           "DOMINIC     MADELYN     JOSIAH      VIOLET      ".
           03  FILLER        PIC X(48) VALUE
           "BRANDON     STELLA      AIDEN       BRIANNA     ".
           03  FILLER        PIC X(48) VALUE
           "MAY         KEVIN       PARKER      JOHN        ".
           03  FILLER        PIC X(48) VALUE
           "ELLEN       JULIA       JULES       SOPHIE      ".
           03  FILLER        PIC X(48) VALUE
           "CHASE       MELANIE     SKYLER      KATHERINE   ".
           03  FILLER        PIC X(48) VALUE
           "MILA        GRAYSON     KHLOE       JASON       ".
           03  FILLER        PIC X(48) VALUE
           "IAN         PAISLEY     ANNABELLE   BENTLEY     ".
           03  FILLER        PIC X(48) VALUE
           "ADAM        ALEXANDRIA  NORA        COOPER      ".
           03  FILLER        PIC X(48) VALUE
           "JUSTIN      LONDON      NOLAN       GIANNA      ".
           03  FILLER        PIC X(48) VALUE
           "HUDSON      NAOMI       EVA         JASE        ".
           03  FILLER        PIC X(48) VALUE
           "FAITH       CARSON      NATHAN      LAUREN      ".
           03  FILLER        PIC X(48) VALUE
           "NATHANIEL   JAXSON      NICHOLE     RUBY        ".
           03  FILLER        PIC X(48) VALUE
           "BRODY       MIKAYLA     LINCOLN     KAYLA       ".
           03  FILLER        PIC X(48) VALUE
           "LUIS        LYDIA       PIPER       TRISTAN     ".
           03  FILLER        PIC X(48) VALUE
           "DAMIAN      SYDNEY      CAMDEN      JOCELYN     ".
           03  FILLER        PIC X(48) VALUE
           "JUAN        MORGAN      BERTHA      BESSIE      ".
           03  FILLER        PIC X(48) VALUE
           "CLARENCE    ROB         ALBERT      ART         ".
           03  FILLER        PIC X(48) VALUE
           "FRANCIS     NELLIE      MINNIE      ETHEL       ".
           03  FILLER        PIC X(48) VALUE
           "FLORENCE    ARTHUR      CLARA       THOM        ".
           03  FILLER        PIC X(48) VALUE
           "CHUCK       JIMMIE      EDNA        HELEN       ".
           03  FILLER        PIC X(48) VALUE
           "MARY        JOSEPH      MARTIN      WILLIAM     ".
           03  FILLER        PIC X(48) VALUE
           "SUSAN       MARY ELLEN  BILLY JO    LORETTA     ".
           03  FILLER        PIC X(48) VALUE
           "JONATHON    EDWIN       ALICE       THOMAS      ".
           03  FILLER        PIC X(48) VALUE
           "JUANITA     MARCUS      MICHAEL     BETH ANN    ".
           03  FILLER        PIC X(48) VALUE
           "JAMES J.    SALLY       NANCY       KATHERINE   ".
           03  FILLER        PIC X(48) VALUE
           "WILLIS J.   RUSSELL     SAMUEL      PHILLIP P.  ".
           03  FILLER        PIC X(48) VALUE
           "DALE        GARY        ALICIA      BARBARA     ".
           03  FILLER        PIC X(48) VALUE
           "LUCAS       PATRICIA    STEVEN      ANTHONY     ".
           03  FILLER        PIC X(48) VALUE
           "PETER       RACHEL      CLAIRE      FRANK       ".
           03  FILLER        PIC X(48) VALUE
           "ERIC        JEFFREY     JEANNIE     ELIZABETH   ".
           03  FILLER        PIC X(48) VALUE
           "MERRILL     MERRY       HOWARD      GOLDIE      ".
           03  FILLER        PIC X(48) VALUE
           "BONNIE      BARRY       ALVIE       MARGE       ".
           03  FILLER        PIC X(48) VALUE
           "JAMES J.    LYNN        LESLIE      ROBIN       ".
           03  FILLER        PIC X(48) VALUE
           "WILLARD     JOANNA      CHRISTINA   MARJORY A.  ".
           03  FILLER        PIC X(48) VALUE
           "PHILLIP     JAY         EDWARD      JOEL        ".
           03  FILLER        PIC X(48) VALUE
           "PAMELA      ANNA        KARL        CARLA S.    ".
           03  FILLER        PIC X(48) VALUE
           "ASHTON      ROBIN       CHARLIE     WALLACE     ".
           03  FILLER        PIC X(48) VALUE
           "JACOB       NOAH        SETH        DARREN      ".
           03  FILLER        PIC X(48) VALUE
           "DERREK      NORMAN      JAMIE       GEORGE      ".
           03  FILLER        PIC X(48) VALUE
           "RANDALL     AXEL        BRENT       JUDSON      ".
           03  FILLER        PIC X(48) VALUE
           "DANIELLE    SARAH       KATIE       DINAH       ".
           03  FILLER        PIC X(48) VALUE
           "GRACIE LOU  LYNETTE     MIRANDA     JILLIAN     ".
           03  FILLER        PIC X(48) VALUE
           "SANDRA      MARGARET    MARGUERITE  CAITLIN     ".
           03  FILLER        PIC X(48) VALUE
           "WILLA       CAROLYN     KITTY       NICHOLE     ".
           03  FILLER        PIC X(48) VALUE
           "JUDITH      MARILYN     ELLEN       PATRICK     ".
           03  FILLER        PIC X(48) VALUE
           "NOAH        SOPHIA      LIAM        EMMA        ".
           03  FILLER        PIC X(48) VALUE
           "JACOB       OLIVIA      MASON       ISABELLA    ".
           03  FILLER        PIC X(48) VALUE
           "AVA         ETHAN       MIA         MICHAEL     ".
           03  FILLER        PIC X(48) VALUE
           "EMILY       ALEXANDRA   ABIGAIL     JAYDEN      ".
           03  FILLER        PIC X(48) VALUE
           "MADISON     ELIJAH      CHARLOTTE   AIDEN       ".
           03  FILLER        PIC X(48) VALUE
           "AVERY       SOFIA       BEN         CHRISTINE   ".
           03  FILLER        PIC X(48) VALUE
           "ELLA        JACKSON     HARPER      LOGAN       ".
           03  FILLER        PIC X(48) VALUE
           "AMELIA      AUBREY      DREW        ANTHONY     ".
           03  FILLER        PIC X(48) VALUE
           "ADDISON     JOSEPH      EVELYN      JOSHUA      ".
           03  FILLER        PIC X(48) VALUE
           "NATALIE     ANDREW      JODY        HANNAH      ".
           03  FILLER        PIC X(48) VALUE
           "ZOEY        GABRIEL     SAMUEL      VICTORIA    ".
           03  FILLER        PIC X(48) VALUE
           "LILLIAN     CHRISTOPHER LILY        DYLAN       ".
           03  FILLER        PIC X(48) VALUE
           "WILLIAM     ALDEN       SAMANTHA    LAYLA       ".
           03  FILLER        PIC X(48) VALUE
           "NATHAN      ZOE         CARTER      AUDREY      ".
           03  FILLER        PIC X(48) VALUE
           "LEAH        CALEB       LUKE        ALLISON     ".
           03  FILLER        PIC X(48) VALUE
           "CHRISTIAN   ANNA        HUNTER      AALIYAH     ".
           03  FILLER        PIC X(48) VALUE
           "HENRY       SAVANNAH    GEORGIA     OWEN        ".
           03  FILLER        PIC X(48) VALUE
           "GABRIELLA   CAMILIA     LANDON      ARIANNA     ".
           03  FILLER        PIC X(48) VALUE
           "JACK        WYATT       KAYLEE      SCARLET     ".
           03  FILLER        PIC X(48) VALUE
           "JONATHAN    HAILEY      ELI         ARIA        ".
           03  FILLER        PIC X(48) VALUE
           "ISAIAH      RILEY       SEBASTIAN   JAXON       ".
           03  FILLER        PIC X(48) VALUE
           "ALEXIS      BRAYDEN     SARAH       CLAIRE      ".
           03  FILLER        PIC X(48) VALUE
           "JULIAN      LEVI        GAVIN       NEVAEH      ".
           03  FILLER        PIC X(48) VALUE
           "SADIE       AARON       PEYTON      AUBREE      ".
           03  FILLER        PIC X(48) VALUE
           "OLIVER      JORDAN      JOHNNY      EVAN        ".
           03  FILLER        PIC X(48) VALUE
           "NICHOLAS    PENELOPE    SERENITY    CHARLES     ".
           03  FILLER        PIC X(48) VALUE
           "ALLYSSA     BELLA       JEREMIAH    TAYLOR      ".
           03  FILLER        PIC X(48) VALUE
           "CAMERON     ADRINNE     ALEXA       KYLIE       ".
           03  FILLER        PIC X(48) VALUE
           "THOMAS      ROBERT      MACKENZIE   CAROLINE    ".
           03  FILLER        PIC X(48) VALUE
           "TYLER       COLTON      AUTUMN      LUCY        ".
           03  FILLER        PIC X(48) VALUE
           "JACE        AUSTIN      ANGEL       ASHLEY      ".
           03  FILLER        PIC X(48) VALUE
           "DOMINIC     MADELYN     JOSIAH      VIOLET      ".
           03  FILLER        PIC X(48) VALUE
           "BRANDON     STELLA      AIDEN       BRIANNA     ".
           03  FILLER        PIC X(48) VALUE
           "MARIAN      KEVIN       PARKER      ZACHARY     ".
           03  FILLER        PIC X(48) VALUE
           "ELLIE MAE   JULIA       JULES       SOPHIE      ".
           03  FILLER        PIC X(48) VALUE
           "CHASE       MELANIE     SKYLER      KATHERINE   ".
           03  FILLER        PIC X(48) VALUE
           "MILA        GRAYSON     KHLOE       JASON       ".
           03  FILLER        PIC X(48) VALUE
           "IAN         PAISLEY     ANNABELLE   BENTLEY     ".
           03  FILLER        PIC X(48) VALUE
           "ADAM        ALEXANDRIA  NORA        COOPER      ".
           03  FILLER        PIC X(48) VALUE
           "JUSTIN      LONDON      NOLAN       GIANNA      ".
           03  FILLER        PIC X(48) VALUE
           "HUDSON      NAOMI       EVA         JASE        ".
           03  FILLER        PIC X(48) VALUE
           "FAITH       CARSON      NATHAN      LAUREN      ".
           03  FILLER        PIC X(48) VALUE
           "NATHANIEL   JAXSON      NICHOLE     RUBY        ".
           03  FILLER        PIC X(48) VALUE
           "BRODY       MIKAYLA     LINCOLN     KAYLA       ".
           03  FILLER        PIC X(48) VALUE
           "LOU         JUDY        JEAN        JOSEPH      ".
           03  FILLER        PIC X(48) VALUE
           "DAMIAN      SYDNEY      CAMDEN      JOCELYN     ".
           03  FILLER        PIC X(48) VALUE
           "JUAN        MORGAN      BERTHA      BESSIE      ".
           03  FILLER        PIC X(48) VALUE
           "CLARENCE    RORY        ALBERT      ART         ".
           03  FILLER        PIC X(48) VALUE
           "FRANCIS     NELLIE      MINNIE      ETHEL       ".
           03  FILLER        PIC X(48) VALUE
           "FLORENCE    ARTHUR      CLARA       THOM        ".
           03  FILLER        PIC X(48) VALUE
           "CHUCK       JIMMIE      EDNA        HELEN       ".
           03  FILLER        PIC X(48) VALUE
           "MARY        JOSEPH      MARTIN      WILLIAM     ".
           03  FILLER        PIC X(48) VALUE
           "SUSAN       MARY ELLEN  BILLY JO    LORETTA     ".
           03  FILLER        PIC X(48) VALUE
           "JONATHON    EDWIN       ALICE       THOMAS      ".
           03  FILLER        PIC X(48) VALUE
           "JUANITA     MARCUS      MICHAEL     BETH ANN    ".
           03  FILLER        PIC X(48) VALUE
           "JAMES J.    SALLY       NANCY       KATHERINE   ".
           03  FILLER        PIC X(48) VALUE
           "WILLIS J.   RUSSELL     SAMUEL      PHILLIP P.  ".
           03  FILLER        PIC X(48) VALUE
           "DALE        GARY        ALICIA      BARBARA     ".
           03  FILLER        PIC X(48) VALUE
           "LUCAS       PATRICIA    STEVEN      ANTHONY     ".
           03  FILLER        PIC X(48) VALUE
           "PETER       RACHEL      CLAIRE      FRANK       ".
           03  FILLER        PIC X(48) VALUE
           "ERIC        JEFFREY     JEANNIE     ELIZABETH   ".
           03  FILLER        PIC X(48) VALUE
           "MERRILL     MERRY       HOWARD      GOLDIE      ".
           03  FILLER        PIC X(48) VALUE
           "BONNIE      BARRY       ALVIE       BEVERLY     ".
           03  FILLER        PIC X(48) VALUE
           "JAMES J.    LYNN        LESLIE      ROBIN       ".
           03  FILLER        PIC X(48) VALUE
           "WILLARD     JOSIE       CHRISTINA   MARJORY A.  ".
           03  FILLER        PIC X(48) VALUE
           "PHILLIP     JAY         EDWARD      JOEL        ".
           03  FILLER        PIC X(48) VALUE
           "PAMELA      ANNA        KARL        CARLA S.    ".
           03  FILLER        PIC X(48) VALUE
           "ASHTON      ROBIN       CHARLIE     WALLACE     ".
           03  FILLER        PIC X(48) VALUE
           "JACOB       NOAH        SETH        DARREN      ".
           03  FILLER        PIC X(48) VALUE
           "DERREK      NORMAN      JAMIE       GEORGE      ".
           03  FILLER        PIC X(48) VALUE
           "RANDALL     AXEL        BRENT       JUDSON      ".
           03  FILLER        PIC X(48) VALUE
           "DANIELLE    SARAH       KATIE       DINAH       ".
           03  FILLER        PIC X(48) VALUE
           "GRACIE LOU  LYNETTE     MIRANDA     JILLIAN     ".
           03  FILLER        PIC X(48) VALUE
           "SANDRA      MARGARET    MARGUERITE  CAITLIN     ".
           03  FILLER        PIC X(48) VALUE
           "WILLA       CAROLYN     KITTY       NICHOLE     ".
           03  FILLER        PIC X(48) VALUE
           "JUDITH      MARILYN     ELLEN       PATRICK     ".
           03  FILLER        PIC X(48) VALUE
           "NOAH        SOPHIA      LIAM        EMMA        ".
           03  FILLER        PIC X(48) VALUE
           "JACOB       OLIVIA      MASON       ISABELLA    ".
           03  FILLER        PIC X(48) VALUE
           "AVA         ETHAN       MIA         MICHAEL     ".
           03  FILLER        PIC X(48) VALUE
           "EMILY       ALEXANDRA   ABIGAIL     JAYDEN      ".
           03  FILLER        PIC X(48) VALUE
           "MADISON     ELIJAH      CHARLOTTE   AIDEN       ".
           03  FILLER        PIC X(48) VALUE
           "AVERY       SOFIA       BEN         CHLOE       ".
           03  FILLER        PIC X(48) VALUE
           "ELLA        JACKSON     HARPER      LOGAN       ".
           03  FILLER        PIC X(48) VALUE
           "AMELIA      AUBRY       DREW        ANTHONY     ".
           03  FILLER        PIC X(48) VALUE
           "ADDISON     JOSEPH      EVELYN      JOSHUA      ".
           03  FILLER        PIC X(48) VALUE
           "NATALIE     ANDREW      JODY        HANNAH      ".
           03  FILLER        PIC X(48) VALUE
           "ZOEY        GABRIEL     SAMUEL      VICTORIA    ".
           03  FILLER        PIC X(48) VALUE
           "LILLIAN     CHRISTOPHER LILY        DYLAN       ".
           03  FILLER        PIC X(48) VALUE
           "BROOKE      ISAAC       SAMANTHA    LAYLA       ".
           03  FILLER        PIC X(48) VALUE
           "NATHAN      ZOE         CARTER      AUDREY      ".
           03  FILLER        PIC X(48) VALUE
           "LEAH        CALEB       LUKE        ALLISON     ".
           03  FILLER        PIC X(48) VALUE
           "CHRISTIAN   ANNA        HUNTER      AALIYAH     ".
           03  FILLER        PIC X(48) VALUE
           "HENRY       SAVANNAH    GEORGIA     OWEN        ".
           03  FILLER        PIC X(48) VALUE
           "GABRIELLA   CAMILIA     LANDON      ARIANNA     ".
           03  FILLER        PIC X(48) VALUE
           "JACK        WYATT       KATHY       SCARLET     ".
           03  FILLER        PIC X(48) VALUE
           "JONATHAN    HAILEY      ELI         ARIA        ".
           03  FILLER        PIC X(48) VALUE
           "ISAIAH      RILEY       SEBASTIAN   JAXON       ".
           03  FILLER        PIC X(48) VALUE
           "ALEXIS      BRAYDEN     SARAH       CLAIRE      ".
           03  FILLER        PIC X(48) VALUE
           "JULIAN      LEVI        GAVIN       NEVAEH      ".
           03  FILLER        PIC X(48) VALUE
           "SADIE       AARON       PEYTON      AUBREE      ".
           03  FILLER        PIC X(48) VALUE
           "OLIVER      JORDAN      JIM         EVAN        ".
           03  FILLER        PIC X(48) VALUE
           "NICHOLAS    PENELOPE    SERENITY    CHARLES     ".
           03  FILLER        PIC X(48) VALUE
           "ALLYSSA     BELLA       JEREMIAH    TAYLOR      ".
           03  FILLER        PIC X(48) VALUE
           "CAMERON     ADRINNE     ALEXA       KYLIE       ".
           03  FILLER        PIC X(48) VALUE
           "THOMAS      ROBERT      MACKENZIE   CAROLINE    ".
           03  FILLER        PIC X(48) VALUE
           "TYLER       COLTON      AUTUMN      LUCY        ".
           03  FILLER        PIC X(48) VALUE
           "JACE        AUSTIN      ANGEL       ASHLEY      ".
           03  FILLER        PIC X(48) VALUE
           "DOMINIC     MADELYN     JOSIAH      VIOLET      ".
           03  FILLER        PIC X(48) VALUE
           "BRANDON     STELLA      AIDEN       BRIANNA     ".
           03  FILLER        PIC X(48) VALUE
           "MAYA        KEVIN       PARKER      ZACH        ".
           03  FILLER        PIC X(48) VALUE
           "ELLIE MAE   JULIA       JULES       SOPHIE      ".
           03  FILLER        PIC X(48) VALUE
           "CHASE       MELANIE     SKYLER      KATHERINE   ".
           03  FILLER        PIC X(48) VALUE
           "MILA        GRAYSON     LETHA       JASON       ".
           03  FILLER        PIC X(48) VALUE
           "IAN         PAISLEY     ANNABELLE   BENTLEY     ".
           03  FILLER        PIC X(48) VALUE
           "ADAM        ALEXANDRIA  NORA        COOPER      ".
           03  FILLER        PIC X(48) VALUE
           "JUSTIN      LONDON      NOLAN       GIANNA      ".
           03  FILLER        PIC X(48) VALUE
           "HUDSON      NAOMI       EVA         JASE        ".
           03  FILLER        PIC X(48) VALUE
           "FAITH       CARSON      NATHAN      LAUREN      ".
           03  FILLER        PIC X(48) VALUE
           "NATHANIEL   JAXSON      NICHOLE     RUBY        ".
           03  FILLER        PIC X(48) VALUE
           "BRODY       MIKAYLA     LINCOLN     KAYLA       ".
           03  FILLER        PIC X(48) VALUE
           "CECELIA     BEULAH      JULIE       SUSANNAH    ".
           03  FILLER        PIC X(48) VALUE
           "DAMIAN      SYDNEY      CAMDEN      JOCELYN     ".
           03  FILLER        PIC X(48) VALUE
           "JUAN        MORGAN      BERTHA      BESSIE      ".
           03  FILLER        PIC X(48) VALUE
           "CLARENCE    ROY         ALBERT      ART         ".
           03  FILLER        PIC X(48) VALUE
           "FRANCIS     NELLIE      MINNIE      ETHEL       ".
           03  FILLER        PIC X(48) VALUE
           "FLORENCE    ARTHUR      CLARA       THOM        ".
           03  FILLER        PIC X(48) VALUE
           "CHUCK       JIMMIE      EDNA        HELEN       ".
           03  FILLER        PIC X(48) VALUE
           "BARBY       WILLIAM J.  NANCY ANN   GEORGE      ".
           03  FILLER        PIC X(48) VALUE
           "BILL        MARY        MARYANNE    JOHN        ".
           03  FILLER        PIC X(48) VALUE
           "ELIZABETH   JOSEPH      BRUCE       CLARK       ".
           03  FILLER        PIC X(48) VALUE
           "JEFFREY     JEFF        ANNA        CATHY       ".
           03  FILLER        PIC X(48) VALUE
           "ELLIS       BOB         JOHN J.     SUSAN       ".
           03  FILLER        PIC X(48) VALUE
           "CAROL       RYAN        STEVE       LINDA       ".
           03  FILLER        PIC X(48) VALUE
           "GARY        RUSS        DALE        SAM         ".
           03  FILLER        PIC X(48) VALUE
           "TIMOTHY     TIM         THOM        PAT         ".
           03  FILLER        PIC X(48) VALUE
           "JAMES       LYNN        GLENN       WALLY       ".
           03  FILLER        PIC X(48) VALUE
           "KAREN       SUE         GLORIA      PEGGY       ".
           03  FILLER        PIC X(48) VALUE
           "ELLEN       HELENE      NICK        BOBBY       ".
           03  FILLER        PIC X(48) VALUE
           "THOMAS      GREG        ROBERT L.   SCOTT       ".
           03  FILLER        PIC X(48) VALUE
           "PHIL        ANNIE       PAM         JOEY        ".
           03  FILLER        PIC X(48) VALUE
           "DANNY       SUSIE       CARL        PATTI       ".
           03  FILLER        PIC X(48) VALUE
           "LISA        DAVE        KELLY       GRACE       ".
           03  FILLER        PIC X(48) VALUE
           "CLARA       MINNIE      MICKEY      ARTHUR      ".
           03  FILLER        PIC X(48) VALUE
           "FRANK       CINDY       CYNTHIA     KEN         ".
           03  FILLER        PIC X(48) VALUE
           "ALAN        JAY         BETH        JUDY        ".
           03  FILLER        PIC X(48) VALUE
           "BRANDON     GARTH       SONNY       RODNEY      ".
           03  FILLER        PIC X(48) VALUE
           "DEMEE       DALTON      ZACHARY     PATRICK     ".
           03  FILLER        PIC X(48) VALUE
           "QUINN       ALBERT T.   CANNIN      KEONE       ".
           03  FILLER        PIC X(36) VALUE
           "JIM         LORI        ANDREA      ".
       01  FIRST-NAME REDEFINES FIRST-NAMES PIC X(12) OCCURS 999.
       01  LAST-NAMES.
           03  FILLER        PIC X(48) VALUE
           "SMITH       JOHNSON     WILLIAMS    JONES       ".
           03  FILLER        PIC X(48) VALUE
           "BROWN       DAVIS       MILLER      WILSON      ".
           03  FILLER        PIC X(48) VALUE
           "MOORE       TAYLOR      ANDERSON    THOMAS      ".
           03  FILLER        PIC X(48) VALUE
           "JACKSON     WHITE       HARRIS      MARTIN      ".
           03  FILLER        PIC X(48) VALUE
           "THOMPSON    GARCIA      MARTINEZ    ROBINSON    ".
           03  FILLER        PIC X(48) VALUE
           "CLARK       RODRIGUEZ   LEWIS       LEE         ".
           03  FILLER        PIC X(48) VALUE
           "WALKER      HALL        ALLEN       YOUNG       ".
           03  FILLER        PIC X(48) VALUE
           "HERNANDEZ   KING        WRIGHT      LOPEZ       ".
           03  FILLER        PIC X(48) VALUE
           "HILL        SCOTT       GREEN       ADAMS       ".
           03  FILLER        PIC X(48) VALUE
           "BAKER       GONZALEZ    NELSON      CARTER      ".
           03  FILLER        PIC X(48) VALUE
           "MITCHELL    PEREZ       ROBERTS     TURNER      ".
           03  FILLER        PIC X(48) VALUE
           "PHILLIPS    CAMPBELL    PARKER      EVANS       ".
           03  FILLER        PIC X(48) VALUE
           "EDWARDS     COLLINS     STEWART     SANCHEZ     ".
           03  FILLER        PIC X(48) VALUE
           "MORRIS      ROGERS      REED        COOK        ".
           03  FILLER        PIC X(48) VALUE
           "MORGAN      BELL        MURPHY      BAILEY      ".
           03  FILLER        PIC X(48) VALUE
           "RIVERA      COOPER      RICHARDSON  COX         ".
           03  FILLER        PIC X(48) VALUE
           "HOWARD      WARD        TORRES      PETERSON    ".
           03  FILLER        PIC X(48) VALUE
           "GRAY        RAMIREZ     JAMES       WATSON      ".
           03  FILLER        PIC X(48) VALUE
           "BROOKS      KELLY       SANDERS     PRICE       ".
           03  FILLER        PIC X(48) VALUE
           "BENNETT     WOOD        BARNES      ROSS        ".
           03  FILLER        PIC X(48) VALUE
           "HENDERSON   COLEMAN     JENKINS     PERRY       ".
           03  FILLER        PIC X(48) VALUE
           "POWELL      LONG        PATTERSON   HUGHES      ".
           03  FILLER        PIC X(48) VALUE
           "FLORES      WASHINGTON  BUTLER      SIMMONS     ".
           03  FILLER        PIC X(48) VALUE
           "FOSTER      GONZALES    BRYANT      ALEXANDER   ".
           03  FILLER        PIC X(48) VALUE
           "RUSSELL     GRIFFIN     DIAZ        HAYES       ".
           03  FILLER        PIC X(48) VALUE
           "MYERS       FORD        HAMILTON    GRAHAM      ".
           03  FILLER        PIC X(48) VALUE
           "SULLIVAN    WALLACE     WOODS       COLE        ".
           03  FILLER        PIC X(48) VALUE
           "WEST        JORDAN      OWENS       REYNOLDS    ".
           03  FILLER        PIC X(48) VALUE
           "FISHER      ELLIS       HARRISON    GIBSON      ".
           03  FILLER        PIC X(48) VALUE
           "MCDONALD    CRUZ        MARSHALL    ORTIZ       ".
           03  FILLER        PIC X(48) VALUE
           "GOMEZ       MURRAY      FREEMAN     WELLS       ".
           03  FILLER        PIC X(48) VALUE
           "WEBB        SIMPSON     STEVENS     TUCKER      ".
           03  FILLER        PIC X(48) VALUE
           "PORTER      HUNTER      HICKS       CRAWFORD    ".
           03  FILLER        PIC X(48) VALUE
           "HENRY       BOYD        MASON       MORALES     ".
           03  FILLER        PIC X(48) VALUE
           "KENNEDY     WARREN      DIXON       RAMOS       ".
           03  FILLER        PIC X(48) VALUE
           "REYES       BURNS       GORDON      SHAW        ".
           03  FILLER        PIC X(48) VALUE
           "HOLMES      RICE        ROBERTSON   HUNT        ".
           03  FILLER        PIC X(48) VALUE
           "BLACK       DANIELS     PALMER      MILLS       ".
           03  FILLER        PIC X(48) VALUE
           "NICHOLS     GRANT       KNIGHT      FERGUSON    ".
           03  FILLER        PIC X(48) VALUE
           "ROSE        STONE       HAWKINS     DUNN        ".
           03  FILLER        PIC X(48) VALUE
           "PERKINS     HUDSON      SPENCER     GARDNER     ".
           03  FILLER        PIC X(48) VALUE
           "STEPHENS    PAYNE       PIERCE      BERRY       ".
           03  FILLER        PIC X(48) VALUE
           "MATTHEWS    ARNOLD      WAGNER      WILLIS      ".
           03  FILLER        PIC X(48) VALUE
           "RAY         WATKINS     OLSON       CARROLL     ".
           03  FILLER        PIC X(48) VALUE
           "DUNCAN      SNYDER      HART        CUNNINGHAM  ".
           03  FILLER        PIC X(48) VALUE
           "BRADLEY     LANE        ANDREWS     RUIZ        ".
           03  FILLER        PIC X(48) VALUE
           "HARPER      FOX         RILEY       ARMSTRONG   ".
           03  FILLER        PIC X(48) VALUE
           "CARPENTER   WEAVER      GREENE      LAWRENCE    ".
           03  FILLER        PIC X(48) VALUE
           "ELLIOTT     CHAVEZ      SIMS        AUSTIN      ".
           03  FILLER        PIC X(48) VALUE
           "PETERS      KELLEY      FRANKLIN    LAWSON      ".
           03  FILLER        PIC X(48) VALUE
           "FIELDS      GUTIERREZ   RYAN        SCHMIDT     ".
           03  FILLER        PIC X(48) VALUE
           "CARR        VASQUEZ     CASTILLO    WHEELER     ".
           03  FILLER        PIC X(48) VALUE
           "CHAPMAN     OLIVER      MONTGOMERY  RICHARDS    ".
           03  FILLER        PIC X(48) VALUE
           "WILLIAMSON  JOHNSTON    BANKS       MEYER       ".
           03  FILLER        PIC X(48) VALUE
           "BISHOP      MCCOY       HOWELL      ALVAREZ     ".
           03  FILLER        PIC X(48) VALUE
           "MORRISON    HANSEN      FERNANDEZ   GARZA       ".
           03  FILLER        PIC X(48) VALUE
           "HARVEY      LITTLE      BURTON      STANLEY     ".
           03  FILLER        PIC X(48) VALUE
           "NGUYEN      GEORGE      JACOBS      REID        ".
           03  FILLER        PIC X(48) VALUE
           "KIM         FULLER      LYNCH       DEAN        ".
           03  FILLER        PIC X(48) VALUE
           "GILBERT     GARRETT     ROMERO      WELCH       ".
           03  FILLER        PIC X(48) VALUE
           "LARSON      FRAZIER     BURKE       HANSON      ".
           03  FILLER        PIC X(48) VALUE
           "DAY         MENDOZA     MORENO      BOWMAN      ".
           03  FILLER        PIC X(48) VALUE
           "MEDINA      FOWLER      BREWER      HOFFMAN     ".
           03  FILLER        PIC X(48) VALUE
           "CARLSON     SILVA       PEARSON     HOLLAND     ".
           03  FILLER        PIC X(48) VALUE
           "DOUGLAS     FLEMING     JENSEN      VARGAS      ".
           03  FILLER        PIC X(48) VALUE
           "BYRD        DAVIDSON    HOPKINS     MAY         ".
           03  FILLER        PIC X(48) VALUE
           "TERRY       HERRERA     WADE        SOTO        ".
           03  FILLER        PIC X(48) VALUE
           "WALTERS     CURTIS      NEAL        CALDWELL    ".
           03  FILLER        PIC X(48) VALUE
           "LOWE        JENNINGS    BARNETT     GRAVES      ".
           03  FILLER        PIC X(48) VALUE
           "JIMENEZ     HORTON      SHELTON     BARRETT     ".
           03  FILLER        PIC X(48) VALUE
           "OBRIEN      CASTRO      SUTTON      GREGORY     ".
           03  FILLER        PIC X(48) VALUE
           "MCKINNEY    LUCAS       MILES       CRAIG       ".
           03  FILLER        PIC X(48) VALUE
           "RODRIQUEZ   CHAMBERS    HOLT        LAMBERT     ".
           03  FILLER        PIC X(48) VALUE
           "FLETCHER    WATTS       BATES       HALE        ".
           03  FILLER        PIC X(48) VALUE
           "RHODES      PENA        BECK        NEWMAN      ".
           03  FILLER        PIC X(48) VALUE
           "HAYNES      MCDANIEL    MENDEZ      BUSH        ".
           03  FILLER        PIC X(48) VALUE
           "VAUGHN      PARKS       DAWSON      SANTIAGO    ".
           03  FILLER        PIC X(48) VALUE
           "NORRIS      HARDY       LOVE        STEELE      ".
           03  FILLER        PIC X(48) VALUE
           "CURRY       POWERS      SCHULTZ     BARKER      ".
           03  FILLER        PIC X(48) VALUE
           "GUZMAN      PAGE        MUNOZ       BALL        ".
           03  FILLER        PIC X(48) VALUE
           "KELLER      CHANDLER    WEBER       LEONARD     ".
           03  FILLER        PIC X(48) VALUE
           "WALSH       LYONS       RAMSEY      WOLFE       ".
           03  FILLER        PIC X(48) VALUE
           "SCHNEIDER   MULLINS     BENSON      SHARP       ".
           03  FILLER        PIC X(48) VALUE
           "BOWEN       DANIEL      BARBER      CUMMINGS    ".
           03  FILLER        PIC X(48) VALUE
           "HINES       BALDWIN     GRIFFITH    VALDEZ      ".
           03  FILLER        PIC X(48) VALUE
           "HUBBARD     SALAZAR     REEVES      WARNER      ".
           03  FILLER        PIC X(48) VALUE
           "STEVENSON   BURGESS     SANTOS      TATE        ".
           03  FILLER        PIC X(48) VALUE
           "CROSS       GARNER      MANN        MACK        ".
           03  FILLER        PIC X(48) VALUE
           "MOSS        THORNTON    DENNIS      MCGEE       ".
           03  FILLER        PIC X(48) VALUE
           "FARMER      DELGADO     AGUILAR     VEGA        ".
           03  FILLER        PIC X(48) VALUE
           "GLOVER      MANNING     COHEN       HARMON      ".
           03  FILLER        PIC X(48) VALUE
           "RODGERS     ROBBINS     NEWTON      TODD        ".
           03  FILLER        PIC X(48) VALUE
           "BLAIR       HIGGINS     INGRAM      REESE       ".
           03  FILLER        PIC X(48) VALUE
           "CANNON      STRICKLAND  TOWNSEND    POTTER      ".
           03  FILLER        PIC X(48) VALUE
           "GOODWIN     WALTON      ROWE        HAMPTON     ".
           03  FILLER        PIC X(48) VALUE
           "ORTEGA      PATTON      SWANSON     JOSEPH      ".
           03  FILLER        PIC X(48) VALUE
           "FRANCIS     GOODMAN     MALDONADO   YATES       ".
           03  FILLER        PIC X(48) VALUE
           "BECKER      ERICKSON    HODGES      RIOS        ".
           03  FILLER        PIC X(48) VALUE
           "CONNER      ADKINS      WEBSTER     NORMAN      ".
           03  FILLER        PIC X(48) VALUE
           "MALONE      HAMMOND     FLOWERS     COBB        ".
           03  FILLER        PIC X(48) VALUE
           "MOODY       QUINN       BLAKE       MAXWELL     ".
           03  FILLER        PIC X(48) VALUE
           "POPE        FLOYD       OSBORNE     PAUL        ".
           03  FILLER        PIC X(48) VALUE
           "MCCARTHY    GUERRERO    LINDSEY     ESTRADA     ".
           03  FILLER        PIC X(48) VALUE
           "SANDOVAL    GIBBS       TYLER       GROSS       ".
           03  FILLER        PIC X(48) VALUE
           "FITZGERALD  STOKES      DOYLE       SHERMAN     ".
           03  FILLER        PIC X(48) VALUE
           "SAUNDERS    WISE        COLON       GILL        ".
           03  FILLER        PIC X(48) VALUE
           "ALVARADO    GREER       PADILLA     SIMON       ".
           03  FILLER        PIC X(48) VALUE
           "WATERS      NUNEZ       BALLARD     SCHWARTZ    ".
           03  FILLER        PIC X(48) VALUE
           "MCBRIDE     HOUSTON     CHRISTENSEN KLEIN       ".
           03  FILLER        PIC X(48) VALUE
           "PRATT       BRIGGS      PARSONS     MCLAUGHLIN  ".
           03  FILLER        PIC X(48) VALUE
           "ZIMMERMAN   FRENCH      BUCHANAN    MORAN       ".
           03  FILLER        PIC X(48) VALUE
           "COPELAND    ROY         PITTMAN     BRADY       ".
           03  FILLER        PIC X(48) VALUE
           "MCCORMICK   HOLLOWAY    BROCK       POOLE       ".
           03  FILLER        PIC X(48) VALUE
           "FRANK       LOGAN       OWEN        BASS        ".
           03  FILLER        PIC X(48) VALUE
           "MARSH       DRAKE       WONG        JEFFERSON   ".
           03  FILLER        PIC X(48) VALUE
           "PARK        MORTON      ABBOTT      SPARKS      ".
           03  FILLER        PIC X(48) VALUE
           "PATRICK     NORTON      HUFF        CLAYTON     ".
           03  FILLER        PIC X(48) VALUE
           "MASSEY      LLOYD       FIGUEROA    CARSON      ".
           03  FILLER        PIC X(48) VALUE
           "BOWERS      ROBERSON    BARTON      TRAN        ".
           03  FILLER        PIC X(48) VALUE
           "LAMB        HARRINGTON  CASEY       BOONE       ".
           03  FILLER        PIC X(48) VALUE
           "CORTEZ      CLARKE      MATHIS      SINGLETON   ".
           03  FILLER        PIC X(48) VALUE
           "WILKINS     CAIN        BRYAN       UNDERWOOD   ".
           03  FILLER        PIC X(48) VALUE
           "HOGAN       MCKENZIE    COLLIER     LUNA        ".
           03  FILLER        PIC X(48) VALUE
           "PHELPS      MCGUIRE     ALLISON     BRIDGES     ".
           03  FILLER        PIC X(48) VALUE
           "WILKERSON   NASH        SUMMERS     ATKINS      ".
           03  FILLER        PIC X(48) VALUE
           "WILCOX      PITTS       CONLEY      MARQUEZ     ".
           03  FILLER        PIC X(48) VALUE
           "BURNETT     RICHARD     COCHRAN     CHASE       ".
           03  FILLER        PIC X(48) VALUE
           "DAVENPORT   HOOD        GATES       CLAY        ".
           03  FILLER        PIC X(48) VALUE
           "AYALA       SAWYER      ROMAN       VAZQUEZ     ".
           03  FILLER        PIC X(48) VALUE
           "DICKERSON   HODGE       ACOSTA      FLYNN       ".
           03  FILLER        PIC X(48) VALUE
           "ESPINOZA    NICHOLSON   MONROE      WOLF        ".
           03  FILLER        PIC X(48) VALUE
           "MORROW      KIRK        RANDALL     ANTHONY     ".
           03  FILLER        PIC X(48) VALUE
           "WHITAKER    OCONNOR     SKINNER     WARE        ".
           03  FILLER        PIC X(48) VALUE
           "MOLINA      KIRBY       HUFFMAN     BRADFORD    ".
           03  FILLER        PIC X(48) VALUE
           "CHARLES     GILMORE     DOMINGUEZ   ONEAL       ".
           03  FILLER        PIC X(48) VALUE
           "BRUCE       LANG        COMBS       KRAMER      ".
           03  FILLER        PIC X(48) VALUE
           "HEATH       HANCOCK     GALLAGHER   GAINES      ".
           03  FILLER        PIC X(48) VALUE
           "SHAFFER     SHORT       WIGGINS     MATHEWS     ".
           03  FILLER        PIC X(48) VALUE
           "MCCLAIN     FISCHER     WALL        SMALL       ".
           03  FILLER        PIC X(48) VALUE
           "MELTON      HENSLEY     BOND        DYER        ".
           03  FILLER        PIC X(48) VALUE
           "CAMERON     GRIMES      CONTRERAS   CHRISTIAN   ".
           03  FILLER        PIC X(48) VALUE
           "WYATT       BAXTER      SNOW        MOSLEY      ".
           03  FILLER        PIC X(48) VALUE
           "SHEPHERD    LARSEN      HOOVER      BEASLEY     ".
           03  FILLER        PIC X(48) VALUE
           "GLENN       PETERSEN    WHITEHEAD   MEYERS      ".
           03  FILLER        PIC X(48) VALUE
           "KEITH       GARRISON    VINCENT     SHIELDS     ".
           03  FILLER        PIC X(48) VALUE
           "HORN        SAVAGE      OLSEN       SCHROEDER   ".
           03  FILLER        PIC X(48) VALUE
           "HARTMAN     WOODARD     MUELLER     KEMP        ".
           03  FILLER        PIC X(48) VALUE
           "DELEON      BOOTH       PATEL       CALHOUN     ".
           03  FILLER        PIC X(48) VALUE
           "WILEY       EATON       CLINE       NAVARRO     ".
           03  FILLER        PIC X(48) VALUE
           "HARRELL     LESTER      HUMPHREY    PARRISH     ".
           03  FILLER        PIC X(48) VALUE
           "DURAN       HUTCHINSON  HESS        DORSEY      ".
           03  FILLER        PIC X(48) VALUE
           "BULLOCK     ROBLES      BEARD       DALTON      ".
           03  FILLER        PIC X(48) VALUE
           "AVILA       VANCE       RICH        BLACKWEL    ".
           03  FILLER        PIC X(48) VALUE
           "YORK        JOHNS       BLANKENSHIP TREVINO     ".
           03  FILLER        PIC X(48) VALUE
           "SALINAS     CAMPOS      PRUITT      MOSES       ".
           03  FILLER        PIC X(48) VALUE
           "CALLAHAN    GOLDEN      MONTOYA     HARDIN      ".
           03  FILLER        PIC X(48) VALUE
           "GUERRA      MCDOWELL    CAREY       STAFFORD    ".
           03  FILLER        PIC X(48) VALUE
           "GALLEGOS    HENSON      WILKINSON   BOOKER      ".
           03  FILLER        PIC X(48) VALUE
           "MERRITT     MIRANDA     ATKINSON    ORR         ".
           03  FILLER        PIC X(48) VALUE
           "DECKER      HOBBS       PRESTON     TANNER      ".
           03  FILLER        PIC X(48) VALUE
           "KNOX        PACHECO     STEPHENSON  GLASS       ".
           03  FILLER        PIC X(48) VALUE
           "ROJAS       SERRANO     MARKS       COORS       ".
           03  FILLER        PIC X(48) VALUE
           "HICKMAN     ENGLISH     SWEENEY     STRONG      ".
           03  FILLER        PIC X(48) VALUE
           "PRINCE      MCCLURE     CONWAY      WALTER      ".
           03  FILLER        PIC X(48) VALUE
           "ROTH        MAYNARD     FARRELL     LOWERY      ".
           03  FILLER        PIC X(48) VALUE
           "HURST       NIXON       WEISS       TRUJILLO    ".
           03  FILLER        PIC X(48) VALUE
           "ELLISON     SLOAN       JUAREZ      WINTERS     ".
           03  FILLER        PIC X(48) VALUE
           "MCLEAN      RANDOLPH    LEON        BOYER       ".
           03  FILLER        PIC X(48) VALUE
           "VILLARREAL  MCCALL      GENTRY      CARRILLO    ".
           03  FILLER        PIC X(48) VALUE
           "KENT        AYERS       LARA        SHANNON     ".
           03  FILLER        PIC X(48) VALUE
           "SEXTON      PACE        HULL        LEBLANC     ".
           03  FILLER        PIC X(48) VALUE
           "BROWNING    VELASQUEZ   LEACH       CHANG       ".
           03  FILLER        PIC X(48) VALUE
           "HOUSE       SELLERS     HERRING     NOBLE       ".
           03  FILLER        PIC X(48) VALUE
           "FOLEY       BARTLETT    MERCADO     LANDRY      ".
           03  FILLER        PIC X(48) VALUE
           "DURHAM      WALLS       BARR        MCKEE       ".
           03  FILLER        PIC X(48) VALUE
           "BAUER       RIVERS      EVERETT     BRADSHAW    ".
           03  FILLER        PIC X(48) VALUE
           "PUGH        VELEZ       RUSH        ESTES       ".
           03  FILLER        PIC X(48) VALUE
           "DODSON      MORSE       SHEPPARD    WEEKS       ".
           03  FILLER        PIC X(48) VALUE
           "CAMACHO     BEAN        BARRON      LIVINGSTON  ".
           03  FILLER        PIC X(48) VALUE
           "MIDDLETON   SPEARS      BRANCH      BLEVINS     ".
           03  FILLER        PIC X(48) VALUE
           "CHEN        KERR        MCCONNELL   HATFIELD    ".
           03  FILLER        PIC X(48) VALUE
           "HARDING     ASHLEY      SOLIS       HERMAN      ".
           03  FILLER        PIC X(48) VALUE
           "FROST       GILES       BLACKBURN   WILLIAM     ".
           03  FILLER        PIC X(48) VALUE
           "PENNINGTON  WOODWARD    FINLEY      MCINTOSH    ".
           03  FILLER        PIC X(48) VALUE
           "KOCH        BEST        SOLOMON     MCCULLOUGH  ".
           03  FILLER        PIC X(48) VALUE
           "DUDLEY      NOLAN       BLANCHARD   RIVAS       ".
           03  FILLER        PIC X(48) VALUE
           "BRENNAN     MEJIA       KANE        BENTON      ".
           03  FILLER        PIC X(48) VALUE
           "JOYCE       BUCKLEY     HALEY       VALENTINE   ".
           03  FILLER        PIC X(48) VALUE
           "MADDOX      RUSSO       MCKNIGHT    BUCK        ".
           03  FILLER        PIC X(48) VALUE
           "MOON        MCMILLAN    CROSBY      BERG        ".
           03  FILLER        PIC X(48) VALUE
           "DOTSON      MAYS        ROACH       CHURCH      ".
           03  FILLER        PIC X(48) VALUE
           "CHAN        RICHMOND    MEADOWS     FAULKNER    ".
           03  FILLER        PIC X(48) VALUE
           "ONEILL      KNAPP       KLINE       BARRY       ".
           03  FILLER        PIC X(48) VALUE
           "OCHOA       JACOBSON    GAY         AVERY       ".
           03  FILLER        PIC X(48) VALUE
           "HENDRICKS   HORNE       SHEPARD     HEBERT      ".
           03  FILLER        PIC X(48) VALUE
           "CHERRY      CARDENAS    MCINTYRE    WHITNEY     ".
           03  FILLER        PIC X(48) VALUE
           "WALLER      HOLMAN      DONALDSON   CANTU       ".
           03  FILLER        PIC X(48) VALUE
           "TERRELL     MORIN       GILLESPIE   FUENTES     ".
           03  FILLER        PIC X(48) VALUE
           "TILLMAN     SANFORD     BENTLEY     PECK        ".
           03  FILLER        PIC X(48) VALUE
           "KEY         SALAS       ROLLINS     GAMBLE      ".
           03  FILLER        PIC X(48) VALUE
           "DICKSON     BATTLE      SANTANA     CABRERA     ".
           03  FILLER        PIC X(48) VALUE
           "CERVANTES   HOWE        HINTON      HURLEY      ".
           03  FILLER        PIC X(48) VALUE
           "SPENCE      ZAMORA      YANG        MCNEIL      ".
           03  FILLER        PIC X(48) VALUE
           "SUAREZ      CASE        PETTY       GOULD       ".
           03  FILLER        PIC X(48) VALUE
           "MCFARLAND   SAMPSON     CARVER      BRAY        ".
           03  FILLER        PIC X(48) VALUE
           "ROSARIO     MACDONALD   STOUT       HESTER      ".
           03  FILLER        PIC X(48) VALUE
           "MELENDEZ    DILLON      FARLEY      HOPPER      ".
           03  FILLER        PIC X(48) VALUE
           "GALLOWAY    POTTS       BERNARD     JOYNER      ".
           03  FILLER        PIC X(48) VALUE
           "STEIN       AGUIRRE     OSBORN      MERCER      ".
           03  FILLER        PIC X(48) VALUE
           "BENDER      FRANCO      ROWLAND     SYKES       ".
           03  FILLER        PIC X(48) VALUE
           "BENJAMIN    TRAVIS      PICKETT     CRANE       ".
           03  FILLER        PIC X(48) VALUE
           "SEARS       MAYO        DUNLAP      HAYDEN      ".
           03  FILLER        PIC X(48) VALUE
           "WILDER      MCKAY       COFFEY      MCCARTY     ".
           03  FILLER        PIC X(48) VALUE
           "EWING       COOLEY      VAUGHAN     BONNER      ".
           03  FILLER        PIC X(48) VALUE
           "COTTON      HOLDER      STARK       FERRELL     ".
           03  FILLER        PIC X(48) VALUE
           "CANTRELL    FULTON      LYNN        LOTT        ".
           03  FILLER        PIC X(48) VALUE
           "CALDERON    ROSA        POLLARD     HOOPER      ".
           03  FILLER        PIC X(48) VALUE
           "BURCH       MULLEN      FRY         RIDDLE      ".
           03  FILLER        PIC X(48) VALUE
           "LEVY        DAVID       DUKE        ODONNELL    ".
           03  FILLER        PIC X(48) VALUE
           "GUY         MICHAEL     BRITT       FREDERICK   ".
           03  FILLER        PIC X(48) VALUE
           "DAUGHERTY   BERGER      DILLARD     ALSTON      ".
           03  FILLER        PIC X(48) VALUE
           "JARVIS      FRYE        RIGGS       CHANEY      ".
           03  FILLER        PIC X(48) VALUE
           "ODOM        DUFFY       FITZPATRICK VALENZUELA  ".
           03  FILLER        PIC X(48) VALUE
           "MERRILL     MAYER       ALFORD      MCPHERSON   ".
           03  FILLER        PIC X(48) VALUE
           "ACEVEDO     DONOVAN     BARRERA     ALBERT      ".
           03  FILLER        PIC X(48) VALUE
           "COTE        REILLY      COMPTON     AYMOND      ".
           03  FILLER        PIC X(48) VALUE
           "MOONEY      MCGOWAN     CRAFT       CLEVELAND   ".
           03  FILLER        PIC X(48) VALUE
           "CLEMONS     WYNN        NIELSEN     BAIRD       ".
           03  FILLER        PIC X(48) VALUE
           "STANTON     SNIDER      ROSALES     BRIGHT      ".
           03  FILLER        PIC X(48) VALUE
           "WITT        STUART      HAYS        HOLDEN      ".
           03  FILLER        PIC X(48) VALUE
           "RUTLEDGE    KINNEY      CLEMENTS    CASTANEDA   ".
           03  FILLER        PIC X(48) VALUE
           "SLATER      HAHN        EMERSON     CONRAD      ".
           03  FILLER        PIC X(48) VALUE
           "BURKS       DELANEY     PATE        LANCASTER   ".
           03  FILLER        PIC X(48) VALUE
           "SWEET       JUSTICE     TYSON       SHARPE      ".
           03  FILLER        PIC X(48) VALUE
           "WHITFIELD   TALLEY      MACIAS      IRWIN       ".
           03  FILLER        PIC X(48) VALUE
           "BURRIS      RATLIFF     MCCRAY      MADDEN      ".
           03  FILLER        PIC X(48) VALUE
           "KAUFMAN     BEACH       GOFF        CASH        ".
           03  FILLER        PIC X(48) VALUE
           "BOLTON      MCFADDEN    LEVINE      GOOD        ".
           03  FILLER        PIC X(48) VALUE
           "BYERS       KIRKLAND    KIDD        WORKMAN     ".
           03  FILLER        PIC X(48) VALUE
           "CARNEY      DALE        MCLEOD      HOLCOMB     ".
           03  FILLER        PIC X(48) VALUE
           "ENGLAND     FINCH       HEAD        BURT        ".
           03  FILLER        PIC X(48) VALUE
           "HENDRIX     SOSA        HANEY       FRANKS      ".
           03  FILLER        PIC X(48) VALUE
           "SARGENT     NIEVES      DOWNS       RASMUSSEN   ".
           03  FILLER        PIC X(48) VALUE
           "BIRD        HEWITT      LINDSAY     LE          ".
           03  FILLER        PIC X(48) VALUE
           "FOREMAN     VALENCIA    ONEIL       DELACRUZ    ".
           03  FILLER        PIC X(48) VALUE
           "VINSON      DEJESUS     HYDE        FORBES      ".
           03  FILLER        PIC X(48) VALUE
           "GILLIAM     GUTHRIE     WOOTEN      HUBER       ".
           03  FILLER        PIC X(48) VALUE
           "BARLOW      BOYLE       MCMAHON     BUCKNER     ".
           03  FILLER        PIC X(48) VALUE
           "ROCHA       PUCKETT     LANGLEY     KNOWLES     ".
           03  FILLER        PIC X(36) VALUE
           "COOKE       VELAZQUEZ   WHITLEY".
       01  LAST-NAME REDEFINES LAST-NAMES PIC X(12) OCCURS 999.
       01  EXTRA-LAST-NAMES.
           03  FILLER        PIC X(48) VALUE
           "SMITH       JONES       WILLIAMS    GRIFFITH    ".
           03  FILLER        PIC X(48) VALUE
           "WILSON      VAN ORMAN   THOMPSON    GOLDBERG    ".
           03  FILLER        PIC X(48) VALUE
           "JOHNSON     STANTON     SILVERMAN   BLACK       ".
           03  FILLER        PIC X(48) VALUE
           "WHITE       STONE       THOMAS      BROWNING    ".
           03  FILLER        PIC X(48) VALUE
           "WORTHMAN    PENNEY      GREEN       ROWE        ".
           03  FILLER        PIC X(48) VALUE
           "ROSEMONT    BRANNING    SHUMAKER    PHILIPS     ".
           03  FILLER        PIC X(48) VALUE
           "VAN BUSKIRK WILMINGTON  LARSON      SUMMERS     ".
           03  FILLER        PIC X(48) VALUE
           "WINTER      GREGORY     RENFRO      MONROE      ".
           03  FILLER        PIC X(48) VALUE
           "SMITHSON    WASHINGTON  HOFFMAN     TOMLINSON   ".
           03  FILLER        PIC X(48) VALUE
           "BALL        BILLINGSLY  LEEMON      JOHNSTON    ".
           03  FILLER        PIC X(48) VALUE
           "WINNINGHAM  CAMBRIDGE   MILLTON     REDFORD     ".
           03  FILLER        PIC X(48) VALUE
           "WINTERS     STANTON     PUFF        AUGSBURGER  ".
           03  FILLER        PIC X(48) VALUE
           "HILTON      SEIFERT     BINGHAM     STELLBERGER ".
           03  FILLER        PIC X(48) VALUE
           "BOYCE       FORDHAM     STONER      AZAR        ".
           03  FILLER        PIC X(48) VALUE
           "FRITZER     LARMAN      WILLIS      ROYSTON     ".
           03  FILLER        PIC X(48) VALUE
           "SHERMAN     PEABODY     PINTER      HOUSTON     ".
           03  FILLER        PIC X(48) VALUE
           "DALLAS      CUNNINGHAM  GAY         SCHAFFER    ".
           03  FILLER        PIC X(48) VALUE
           "PALETSKI    VAN OOSTER  COOLMAN     CROSBY      ".
           03  FILLER        PIC X(48) VALUE
           "TAYLOR      HERNANDEZ   MOORE       WALTERS     ".
           03  FILLER        PIC X(48) VALUE
           "CAMPBELL    COLLINS     EDWARDS     WRIGHT      ".
           03  FILLER        PIC X(48) VALUE
           "TURNER      ROBINSON    WALKER      JENKINS     ".
           03  FILLER        PIC X(48) VALUE
           "PETTERSON   JORDAN      RICHARDSON  PETERSON    ".
           03  FILLER        PIC X(48) VALUE
           "HENDERSON   COLEMAN     ELLIS       OWENS       ".
           03  FILLER        PIC X(48) VALUE
           "MCDONALD    RUIZ        KENNEDY     ROSS        ".
           03  FILLER        PIC X(36) VALUE
           "BARNES      ALEXANDER   WELLS       ".
       01  EXTRA-LAST-NAME REDEFINES EXTRA-LAST-NAMES
                                          PIC X(12) OCCURS 99.
       01  STREET-NAMES.
           03  FILLER        PIC X(36) VALUE
           "WASHINGTON  JEFFERSON   MADISON     ".
           03  FILLER        PIC X(36) VALUE
           "WAYNE       ADAMS       MONROE      ".
           03  FILLER        PIC X(36) VALUE
           "TYLER       GRANT       HOOVER      ".
           03  FILLER        PIC X(36) VALUE
           "MAPLE       OAK         ELM         ".
           03  FILLER        PIC X(36) VALUE
           "ASH         NORTH       SOUTH       ".
           03  FILLER        PIC X(36) VALUE
           "EAST        WEST        MAIN        ".
           03  FILLER        PIC X(36) VALUE
           "LAKE        HILL        SUNSET      ".
           03  FILLER        PIC X(36) VALUE
           "CRESCENT    ROSEWOOD    CEDAR       ".
           03  FILLER        PIC X(36) VALUE
           "PINE        PINEWOOD    ASTOR       ".
           03  FILLER        PIC X(36) VALUE
           "GREEN       FOXFIRE     STAR        ".
           03  FILLER        PIC X(36) VALUE
           "WILLIAMS    HACKER      FARM        ".
           03  FILLER        PIC X(36) VALUE
           "MORNINGSTAR FIRST       SECOND      ".
           03  FILLER        PIC X(36) VALUE
           "THIRD       APPLE       CHERRYWOOD  ".
           03  FILLER        PIC X(36) VALUE
           "DAVIS       BROOKSIDE   CHAPEL      ".
           03  FILLER        PIC X(36) VALUE
           "CHURCH      OTTAWA      INDIANA     ".
           03  FILLER        PIC X(36) VALUE
           "LATCH       DREXEL      COLUMBIA    ".
           03  FILLER        PIC X(36) VALUE
           "MCMILLAN    MILLER      GILBERT     ".
           03  FILLER        PIC X(36) VALUE
           "PENNSYLVANIAHOLT        FRANKLIN    ".
           03  FILLER        PIC X(36) VALUE
           "NORTHRUP    PERSHING    HOPE        ".
           03  FILLER        PIC X(36) VALUE
           "BRIGHTON    PALMER      SCOTT       ".
           03  FILLER        PIC X(36) VALUE
           "PERKINS     PARKVIEW    OAKDALE     ".
           03  FILLER        PIC X(36) VALUE
           "FOSTER      STEEPLE     OLD MILL    ".
           03  FILLER        PIC X(36) VALUE
           "CRESTVIEW   MOUNTAIN    VALLEY      ".
           03  FILLER        PIC X(36) VALUE
           "HILLSDALE   LAKE        RIVERVIEW   ".
           03  FILLER        PIC X(36) VALUE
           "GRASS       LAKELAND    TERRACE     ".
           03  FILLER        PIC X(36) VALUE
           "VALLEYVIEW  CANAL       BRIDGE      ".
           03  FILLER        PIC X(36) VALUE
           "PARK        ILLINOIS    ISLAND      ".
           03  FILLER        PIC X(36) VALUE
           "CLIFF       AIRPORT     COMSTOCK    ".
           03  FILLER        PIC X(36) VALUE
           "DALE        ROSEMARY    CAPITOL     ".
           03  FILLER        PIC X(36) VALUE
           "BILTMORE    ASTORIA     GLENVIEW    ".
           03  FILLER        PIC X(36) VALUE
           "REDWOOD     DOGWOOD     SHERIDAN    ".
           03  FILLER        PIC X(36) VALUE
           "SHERWOOD    WESTOVER    FAIRFAX     ".
           03  FILLER        PIC X(36) VALUE
           "LENOX       MAPLEGROVE  SOUTHWOOD   ".
       01  STREET-NAME REDEFINES STREET-NAMES PIC X(12) OCCURS 99.
       01  STREET-TYPES.
           03  FILLER        PIC X(36) VALUE
           "AVENUE      STREET      LANE        ".
           03  FILLER        PIC X(36) VALUE
           "CIRCLE      BLVD.       DRIVE       ".
           03  FILLER        PIC X(36) VALUE
           "PARKWAY     ROAD        COURT       ".
           03  FILLER        PIC X(36) VALUE
           "HIGHWAY     CROSSING    DRIVE       ".
           03  FILLER        PIC X(36) VALUE
           "PLACE       RIDGE       RUN         ".
           03  FILLER        PIC X(36) VALUE
           "TRACE       TRAIL       WAY         ".
           03  FILLER        PIC X(36) VALUE
           "BEND        CAUSEWAY    CORNER      ".
           03  FILLER        PIC X(36) VALUE
           "KNOLL       LANDING     LOOP        ".
           03  FILLER        PIC X(36) VALUE
           "AVENUE      STREET      LANE        ".
           03  FILLER        PIC X(36) VALUE
           "CIRCLE      BLVD.       DRIVE       ".
           03  FILLER        PIC X(36) VALUE
           "PARKWAY     ROAD        COURT       ".
           03  FILLER        PIC X(36) VALUE
           "HIGHWAY     CROSSING    DRIVE       ".
           03  FILLER        PIC X(36) VALUE
           "PLACE       RIDGE       RUN         ".
           03  FILLER        PIC X(36) VALUE
           "TRACE       TRAIL       WAY         ".
           03  FILLER        PIC X(36) VALUE
           "BEND        CAUSEWAY    CORNER      ".
           03  FILLER        PIC X(36) VALUE
           "KNOLL       LANDING     LOOP        ".
           03  FILLER        PIC X(36) VALUE
           "STREET      ROAD        AVENUE      ".
           03  FILLER        PIC X(36) VALUE
           "AVENUE      STREET      LANE        ".
           03  FILLER        PIC X(36) VALUE
           "CIRCLE      BLVD.       DRIVE       ".
           03  FILLER        PIC X(36) VALUE
           "STREET      ROAD        COURT       ".
           03  FILLER        PIC X(36) VALUE
           "ROUTE       CIRCLE      DRIVE       ".
           03  FILLER        PIC X(36) VALUE
           "PLACE       RIDGE       RUN         ".
           03  FILLER        PIC X(36) VALUE
           "TRACE       TRAIL       WAY         ".
           03  FILLER        PIC X(36) VALUE
           "BEND        STREET      CORNER      ".
           03  FILLER        PIC X(36) VALUE
           "DRIVE       LANDING     LOOP        ".
           03  FILLER        PIC X(36) VALUE
           "AVENUE      STREET      LANE        ".
           03  FILLER        PIC X(36) VALUE
           "CIRCLE      BLVD.       DRIVE       ".
           03  FILLER        PIC X(36) VALUE
           "LANE        ROAD        COURT       ".
           03  FILLER        PIC X(36) VALUE
           "HIGHWAY     CROSSING    DRIVE       ".
           03  FILLER        PIC X(36) VALUE
           "PLACE       ROAD        AVE         ".
           03  FILLER        PIC X(36) VALUE
           "TRACE       TRAIL       WAY         ".
           03  FILLER        PIC X(36) VALUE
           "BEND        CAUSEWAY    CORNER      ".
           03  FILLER        PIC X(36) VALUE
           "STREET      ROAD        AVENUE      ".
       01  STREET-TYPE REDEFINES STREET-TYPES PIC X(12) OCCURS 99.
       01  CITIES-TAB.
           03  FILLER PIC X(45) VALUE
           "ELM GROVE      BARTOW         JAMESTOWN      ".
           03  FILLER PIC X(45) VALUE
           "NORTHFIELD     RAMEY          SAN DIEGO      ".
           03  FILLER PIC X(45) VALUE
           "BROOKINGS      SILVER SPRING  HENDERSON      ".
           03  FILLER PIC X(45) VALUE
           "HOUSTON        WICKATUNK      CHATHAM        ".
           03  FILLER PIC X(45) VALUE
           "NATURAL BRIDGE LAWRENCE       DEXTER         ".
           03  FILLER PIC X(45) VALUE
           "PITTSBURGH     EAST TAWAS     JACKSONVILLE   ".
           03  FILLER PIC X(45) VALUE
           "STAMFORD       FLORAL         KULM           ".
           03  FILLER PIC X(45) VALUE
           "ROBBINS        GRAND VIEW     PURYEAR        ".
           03  FILLER PIC X(45) VALUE
           "WATERBURY      SAN ANTONIO    STOCKPORT      ".
           03  FILLER PIC X(45) VALUE
           "COEUR D ALENE  PRESTON        HONOLULU       ".
           03  FILLER PIC X(45) VALUE
           "N WILKESBORO   HOLLYWOOD      NASHVILLE      ".
           03  FILLER PIC X(45) VALUE
           "FREDERICA      RIO OSO        VESTAL         ".
           03  FILLER PIC X(45) VALUE
           "SAMBURG        MORGAN CITY    SUMMERFIELD    ".
           03  FILLER PIC X(45) VALUE
           "HOLABIRD       HARTSVILLE     WARD           ".
           03  FILLER PIC X(45) VALUE
           "NORWALK        TIGRETT        ALEXANDRIA     ".
           03  FILLER PIC X(45) VALUE
           "CORPUS CHRISTI BROOKFIELD     LIVERMORE      ".
           03  FILLER PIC X(45) VALUE
           "IJAMSVILLE     ELMER          FORT WAYNE     ".
           03  FILLER PIC X(45) VALUE
           "FARMINGTON     ELM GROVE      BARTOW         ".
           03  FILLER PIC X(45) VALUE
           "JAMESTOWN      NORTHFIELD     RAMEY          ".
           03  FILLER PIC X(45) VALUE
           "SAN DIEGO      BROOKINGS      SILVER SPRING  ".
           03  FILLER PIC X(45) VALUE
           "HENDERSON      HOUSTON        WICKATUNK      ".
           03  FILLER PIC X(45) VALUE
           "CHATHAM        NATURAL BRIDGE LAWRENCE       ".
           03  FILLER PIC X(45) VALUE
           "DEXTER         PITTSBURGH     EAST TAWAS     ".
           03  FILLER PIC X(45) VALUE
           "JACKSONVILLE   STAMFORD       FLORAL         ".
           03  FILLER PIC X(45) VALUE
           "KULM           ROBBINS        GRAND VIEW     ".
           03  FILLER PIC X(45) VALUE
           "PURYEAR        WATERBURY      SAN ANTONIO    ".
           03  FILLER PIC X(45) VALUE
           "STOCKPORT      COEUR D ALENE  PRESTON        ".
           03  FILLER PIC X(45) VALUE
           "HONOLULU       N WILKESBORO   HOLLYWOOD      ".
           03  FILLER PIC X(45) VALUE
           "NASHVILLE      FREDERICA      RIO OSO        ".
           03  FILLER PIC X(45) VALUE
           "VESTAL         SAMBURG        MORGAN CITY    ".
           03  FILLER PIC X(45) VALUE
           "SUMMERFIELD    HOLABIRD       HARTSVILLE     ".
           03  FILLER PIC X(45) VALUE
           "WARD           NORWALK        TIGRETT        ".
           03  FILLER PIC X(45) VALUE
           "ALEXANDRIA     CORPUS CHRISTI BROOKFIELD     ".
       01  CITY REDEFINES CITIES-TAB PIC X(15) OCCURS 99.
       01  STATES-TAB.
           03  FILLER PIC X(50) VALUE
           "WIFLKYMAPACASDMDNYTXNJMSNYMAMOPAMIFLNEAKNDCAWITNVT".
           03  FILLER PIC X(50) VALUE
           "TXIAIDWAHINCFLTNDECANYTNLAILSDINALCTTNOHTXILCOMDLA".
           03  FILLER PIC X(50) VALUE
           "INMIWIFLKYMAPACASDMDNYTXNJMSNYMAMOPAMIFLNEAKNDCAWI".
           03  FILLER PIC X(48) VALUE
           "TNVTTXIAIDWAHINCFLTNDECANYTNLAILSDINALCTTNOHTXIL".
       01  STATE REDEFINES STATES-TAB PIC X(2) OCCURS 99.
       01  ZIPS-TAB.
           03  FILLER PIC X(50) VALUE
           "53122338304262901354166719211557007209101365077029".
           03  FILLER PIC X(50) VALUE
           "07765387311366501843638411529548730322236897772534".
           03  FILLER PIC X(50) VALUE
           "58456956765483938251056717825752651838149805096817".
           03  FILLER PIC X(50) VALUE
           "28656330263724319946956741385138254703806228957540".
           03  FILLER PIC X(50) VALUE
           "47244369220685938070430017842660513805362175471424".
           03  FILLER PIC X(50) VALUE
           "46807483315312233830426290135416671921155700720910".
           03  FILLER PIC X(50) VALUE
           "13650770290776538731136650184363841152954873032223".
           03  FILLER PIC X(50) VALUE
           "68977725345845695676548393825105671782575265183814". 
           03  FILLER PIC X(50) VALUE
           "98050968172865633026372431994695674138513825470380".
           03  FILLER PIC X(45) VALUE
           "622895754047244369220685938070430017842660513".
       01  ZIP REDEFINES ZIPS-TAB PIC X(5) OCCURS 99.
       01  EMPLOYER-TAB.
           03  FILLER PIC X(48) VALUE
           "ACME INDUSTRIES ACE HARDWARE    GENERAL HOSPITAL".
           03  FILLER PIC X(48) VALUE
           "JILL'S DINER    ESSEX WIRE      VA HOSPITAL     ".
           03  FILLER PIC X(48) VALUE
           "APPLEBEE'S      SOCIAL SEC      JOE'S BAR       ".
           03  FILLER PIC X(48) VALUE
           "WILSON'S MARINE HOME DEPOT      GENERAL DYNAMICS".
           03  FILLER PIC X(48) VALUE
           "PIGGLY WIGGLY   IBM             MICROSOFT       ".
           03  FILLER PIC X(48) VALUE
           "ALLIED SHIPPING UPS             FED-EX          ".
           03  FILLER PIC X(48) VALUE
           "POST OFFICE     ABC INC.        RED LOBSTER     ".
           03  FILLER PIC X(48) VALUE
           "WALGREEN'S      STAPLES         CVS PHARMACY    ".
           03  FILLER PIC X(48) VALUE
           "WALMART         KROGERS         MERCURY MARINE  ".
           03  FILLER PIC X(48) VALUE
           "HONDA           BOB'S MOTORS    SAM'S CLUB      ".
           03  FILLER PIC X(48) VALUE
           "CONSUMERS POWER FORD MOTOR      CHRYSLER        ".
           03  FILLER PIC X(48) VALUE
           "WILLIAMS MOTORS AL'S BODY SHOP  RED ROOF        ".
           03  FILLER PIC X(48) VALUE
           "AC DELCO        GM              HOLIDAY INN     ".
           03  FILLER PIC X(48) VALUE
           "JM CONSTRUCTION AL'S GYM        ELMHURST LIBRARY".
           03  FILLER PIC X(48) VALUE
           "MENARD'S        TOYSRUS         THE BOOKWORM    ".
           03  FILLER PIC X(48) VALUE
           "CITY POWER      LOWES           ACME CONCRETE   ".
           03  FILLER PIC X(48) VALUE
           "JERRY'S GARAGE  CRESTWOOD MOTORSARBOR CEMETARY  ".
           03  FILLER PIC X(48) VALUE
           "JUNE'S DINER    HARRAH'S CASINO AT&T            ".
           03  FILLER PIC X(48) VALUE
           "SPRINT          VERIZON         GENERAL ELECTRIC".
           03  FILLER PIC X(48) VALUE
           "PANERA          TDAMERITRADE    APPLE           ".
           03  FILLER PIC X(48) VALUE
           "GOOGLE          ALTAVISTA       FRONTIER        ".
           03  FILLER PIC X(48) VALUE
           "DISH            HONDA           KIA MOTORS      ".
           03  FILLER PIC X(48) VALUE
           "MERCURY         DO-IT CENTER    WHOLE FOODS     ".
           03  FILLER PIC X(48) VALUE
           "STARBUCKS       JOHN'S NURSERY  HAPPY DAYCARE   ".
           03  FILLER PIC X(48) VALUE
           "MCDONALD'S      TARGET          HEWLETT PACKARD ".
           03  FILLER PIC X(48) VALUE
           "BANK OF AMERICA CITIGROUP       WELLS FARGO     ".
           03  FILLER PIC X(48) VALUE
           "BEST BUY        SAFEWAY         BOEING          ".
           03  FILLER PIC X(48) VALUE
           "JOHNSON CONTROLSCOSTCO          COMCAST         ".
           03  FILLER PIC X(48) VALUE
           "XEROX           EMERSON         HONEYWELL       ".
           03  FILLER PIC X(48) VALUE
           "MACY'S          DELL            AUNT MILLIES    ".
           03  FILLER PIC X(48) VALUE
           "BARNES & NOBLE  PETCO           PETSMART        ".
           03  FILLER PIC X(48) VALUE
           "ACE HARDWARE    AMAZON          SEARS           ".
           03  FILLER PIC X(48) VALUE
           "SAMSUNG         PANASONIC       SONY            ".
       01  EMPLOYER REDEFINES EMPLOYER-TAB PIC X(16) OCCURS 99.

       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/GRWORKW.CPY".

       COPY "LIBGB/CONNECTW_SQL.CPY".

       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".

      ******************************************************************
      *
      *   S C R E E N - M E M B E R S
      *
      ******************************************************************
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/REDACT_DEF.CPY".
       COPY "LIBSP/REDACT_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".
       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBSP/REDACT_SCN.CPY".

      ******************************************************************
      *
      *   P R O C E D U R E - D I V I S I O N
      *
      ******************************************************************
       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               SRC-BW-FILE SRC-BX-FILE
               WK-FILE PR-FILE.
       COPY "LIBGB/DECLRP.CPY".
            PERFORM CLOSE-SRC-BW1-FILE.
            PERFORM CLOSE-SRC-BX1-FILE.
            PERFORM CLOSE-WK-FILE.
            PERFORM CLOSE-BR-FILE.
            PERFORM CLOSE-GP-FILE.
            PERFORM CLOSE-GR1-FILE.
            PERFORM CLOSE-TRB1-FILE.
            PERFORM CLOSE-TRC1-FILE.
            PERFORM CLOSE-TRJ1-FILE.
            PERFORM CLOSE-TRK1-FILE.
            PERFORM CLOSE-TRL1-FILE.
            PERFORM CLOSE-TRP1-FILE.
            PERFORM CLOSE-TRS1-FILE.
            PERFORM CLOSE-TRX1-FILE.
            PERFORM CLOSE-BW1-FILE.
            PERFORM CLOSE-BX1-FILE.
            PERFORM CLOSE-CI2-FILE.
            PERFORM CLOSE-CI3-FILE.
            PERFORM CLOSE-CK1-FILE.
            PERFORM CLOSE-DL1-FILE.
            PERFORM CLOSE-LN4-FILE.
            PERFORM CLOSE-LN5-FILE.
            PERFORM CLOSE-LP1-FILE.
            PERFORM CLOSE-LH1-FILE.
            PERFORM CLOSE-LE1-FILE.
            PERFORM CLOSE-LTB1-FILE.
            PERFORM CLOSE-LTL1-FILE.
            PERFORM CLOSE-PA1-FILE.
             CLOSE PR-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ********************************
      *    MAIN PROGRAM MODULE
      ********************************
       MAIN-PROGRAM SECTION.
      D    STOP MESS. 

           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME              TO FORM-PATH.
           MOVE "REDACT"                   TO VDU-FORM-NAME.

       MAIN-MODULE.
           PERFORM INITIALIZATION.
           IF ( EXT-EOBUF-ERRCD NOT = SPACES )
              GO TO END-PROGRAM.

           IF IO-TYPE = "END"
              GO TO END-PROGRAM.

           IF IO-TYPE = "CLR"
             GO TO MAIN-MODULE.

           IF GROUP-FLAG = "N"
              MOVE BEG-BRANCH TO BR-KEY
              PERFORM START-BR-FILE
              GO TO NEXT-BRANCH-NO.

           MOVE ENT-GROUP TO GR-ID.
           PERFORM SEARCH-GR-INIT.

       NEXT-GROUP-LOC.
           IF GROUP-FLAG NOT = "Y"
              GO TO NEXT-BRANCH-NO.

           PERFORM SEARCH-GR-NEXT.
           IF WGR-SRESULT = 0
              GO TO END-ROUTINE.
           MOVE WGR-SRESULT TO BR-KEY.

           PERFORM READ-BR-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           GO TO MOVE-BRANCH-PATH-INFO.
      
       NEXT-BRANCH-NO.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-FG = 9 OR BR-NO > END-BRANCH
              GO TO END-ROUTINE.
      
       MOVE-BRANCH-PATH-INFO.

           MOVE EXT-FILPATH-BASE TO FILPATH-BASE.
           MOVE BR-MACHINE       TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH     TO FILPATH-DATA.

           MOVE "B"          TO BW-PATH-OVERRIDE
                                BX-PATH-OVERRIDE
                                CI-PATH-OVERRIDE
                                CK-PATH-OVERRIDE
                                DL-PATH-OVERRIDE
                                LN-PATH-OVERRIDE
                                LP-PATH-OVERRIDE
                                LH-PATH-OVERRIDE
                                LE-PATH-OVERRIDE
                                LTB-PATH-OVERRIDE
                                LTL-PATH-OVERRIDE
                                PA-PATH-OVERRIDE
                                TRB-PATH-OVERRIDE
                                TRC-PATH-OVERRIDE
                                TRJ-PATH-OVERRIDE
                                TRK-PATH-OVERRIDE
                                TRL-PATH-OVERRIDE
                                TRP-PATH-OVERRIDE
                                TRS-PATH-OVERRIDE
                                TRX-PATH-OVERRIDE.

           MOVE BR-DATA-PATH TO TRB-ALL-DATA
                                TRC-ALL-DATA
                                TRJ-ALL-DATA
                                TRK-ALL-DATA
                                TRL-ALL-DATA
                                TRP-ALL-DATA
                                TRS-ALL-DATA
                                TRX-ALL-DATA
                                SRC-BW-ALL-DATA
                                SRC-BX-ALL-DATA
                                SOURCE-PATH-DATA
                                TARGET-PATH-DATA.

           
           MOVE SOURCE-PATH1 TO  SRC-BW-PATH1
                                 SRC-BX-PATH1.

           MOVE SRC-BW-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF NOT STAT-VALID
              GO TO NEXT-GROUP-LOC.

           MOVE SRC-BX-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF NOT STAT-VALID
              GO TO NEXT-GROUP-LOC.

      * CALL SHELL TO COPY FILES TO TARGET DIRECTORY WHERE THEY WILL BE
      * REDACTED:
           MOVE "REL" TO GETENV-BUF.
           PERFORM GETENV-CALL.
           MOVE GETENV-BUF       TO REDACT-REL REDACT-REL2.
           CALL "C$TOLOWER" USING REDACT-EC, VALUE 3.
           CALL "C$TOLOWER" USING REDACT-SHELL, VALUE 10.
           MOVE SOURCE-PATH      TO REDACT-SOURCE-PATH.
           MOVE TARGET-PATH      TO REDACT-TARGET-PATH.
           MOVE BEG-DATE         TO REDACT-TRMMYY1.
           MOVE END-DATE         TO REDACT-TRMMYY2.
           MOVE SKIP-LARGE-FG    TO REDACT-SKIP-LARGE.
           MOVE REDACT-CMD TO SYSTEM-BUF.
           PERFORM SYSTEM-CALL.

      * WILL NOT CHECK STAT; TOO MANY THINGS CAN GIVE BAD STATUS,
      * TARGET FILES ALREADY EXIST, SOME OF SOURCE FILES DON'T, ETC.
      *    IF STAT-BAD
      *       GO TO IO-ERROR.

           IF UPDATE-GLOBAL-FG = "Y"
              PERFORM UPDATE-BR-FILE
              PERFORM UPDATE-GP-FILE
              MOVE "N" TO UPDATE-GLOBAL-FG.

      *    PERFORM LOAD-BW1-FILE.
      *    MOVE BW-SELECT-PATH TO ACCESS-BUF.
      *    PERFORM ACCESS-CALL.
      *    IF STAT NOT = "00"
      *       GO TO NEXT-GROUP-LOC.

           PERFORM OPEN-SRC-BW1-FILE.
           PERFORM OPEN-SRC-BX1-FILE.

      ******************************************************************
      * WE WILL OPEN BORROWER & BORROWER TRAILER TO OUTPUT, THEN
      * WRITE BORROWERS WITH REACTED INFO
      ******************************************************************
           PERFORM LOAD-BW1-FILE.
           PERFORM PURGE-OPEN-OUTPUT-BW.
      *    OPEN OUTPUT BW-FILE.
      *    IF FILE-STAT NOT = "00"
      *       GO TO IO-ERROR.

           PERFORM OPEN-BW1-FILE.

           PERFORM LOAD-BX1-FILE.
           PERFORM PURGE-OPEN-OUTPUT-BX.
      *    OPEN OUTPUT BX-FILE.
      *    IF FILE-STAT NOT = "00"
      *       GO TO IO-ERROR.

           PERFORM OPEN-BX1-FILE.

           MOVE "REDACTING BRANCH # &&&&." TO MESS.
           INSPECT MESS REPLACING FIRST "&&&&" BY BR-NO.
           PERFORM SEND-LEGEND.

           MOVE 0 TO SRC-BW-SSNO.
           PERFORM START-SRC-BW1-FILE.
       NEXT-SRC-BW.
           PERFORM READ-SRC-BW1-FILE-NEXT.
           IF IO-FG NOT = 0
              GO TO END-OF-GROUP.

           MOVE SRC-BW-SSNO TO SRC-BX-SSNO.
           PERFORM READ-SRC-BX1-FILE.
           IF IO-FG NOT = 0
              PERFORM CLEAR-SRC-BX-FILE.

           PERFORM UPDATE-MODULE.

           GO TO NEXT-SRC-BW.

       END-OF-GROUP.

           PERFORM UPDATE-SPOUSE-SSNOS.

           PERFORM UPDATE-CK1-FILE.
           PERFORM UPDATE-TR1-FILE.
           PERFORM UPDATE-DL1-FILE.
           PERFORM UPDATE-PA1-FILE.

           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-BX1-FILE.
           PERFORM CLOSE-SRC-BW1-FILE.
           PERFORM CLOSE-SRC-BX1-FILE.
           PERFORM CLOSE-WK-FILE.

           PERFORM OPEN-WK-FILE-OUTPUT.
           PERFORM CLOSE-WK-FILE.
           PERFORM OPEN-WK-FILE.

           GO TO NEXT-GROUP-LOC.

       END-ROUTINE.
           GO TO END-PROGRAM.

      ******************************************************************
      *
      *    I N I T I A L I Z A T I O N
      *
      ******************************************************************
       INITIALIZATION SECTION.
           MOVE SPACES TO EXT-EOBUF-ERRCD.

           MOVE "FIL"      TO GETENV-BUF.
           PERFORM GETENV-CALL.
           MOVE GETENV-BUF TO DATA-PATH.

           IF ( DATA-PATH-DATA = X"64617461" )
              MOVE "THIS UTILITY CANNOT BE RUN IN DATA!" TO MESS
              PERFORM SEND-MESS
              MOVE "END" TO IO-TYPE
              GO TO INITIALIZATION-EXIT.

           MOVE REDACT-QUEUE-POS           TO Q-POS.
           MOVE REDACT-COPIES-POS          TO C-POS.
           MOVE REDACT-PRU-POS             TO P-POS.
           MOVE SPACES                     TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           IF ( EXT-EOBUF-GROUP-FG = "Y" )
              MOVE BEG-BRANCH              TO SV-BEG-BRANCH
              MOVE END-BRANCH              TO SV-END-BRANCH
              MOVE GROUP-FLAG              TO SV-GROUP-FLAG
              MOVE ENT-GROUP               TO SV-ENT-GROUP
              MOVE EXT-EOBUF-BRNO          TO BEG-BRANCH
              MOVE EXT-EOBUF-BRNO          TO END-BRANCH
              MOVE "Y"                     TO GROUP-FLAG
              MOVE EXT-EOBUF-GROUP         TO ENT-GROUP
              MOVE EXT-EOBUF-GROUP-PRPATH  TO PRU-FILE
           ELSE
           IF ( BEG-BRANCH = ZEROES ) AND
              ( END-BRANCH = ZEROES )
              MOVE BEG-BRANCH              TO SV-BEG-BRANCH
              MOVE END-BRANCH              TO SV-END-BRANCH
              MOVE EXT-CONAME-USER-LOC     TO BEG-BRANCH
                                              END-BRANCH.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           PERFORM OPEN-WK-FILE-OUTPUT.
           PERFORM CLOSE-WK-FILE.
           PERFORM OPEN-WK-FILE.

           ACCEPT DATE-YYMMDD FROM DATE.
           PERFORM CONVERT-YYMMDD-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD    TO TODAYS-DATE.
           MOVE DATE-YYYYMMDD-YY TO CENTURY-2.
           SUBTRACT 18 FROM CENTURY-WK.
    
      * READ GP, MAY BE REDACTING GP, SO WILL READ FILE HERE INSTEAD OF
      * DOING GPENV
           PERFORM OPEN-GP-FILE.
           MOVE 1 TO GP-KEY.
           PERFORM READ-GP-FILE.
           IF IO-BAD 
              GO TO IO-ERROR.

       INITIALIZATION-EXIT.
           EXIT.

      ******************************************************************
      *
      *    E N T E R - P A R A M E T E R S
      *
      ******************************************************************
       ENTER-PARAMETERS SECTION.

           MOVE "/" TO SOURCE-PATH-SLASH1
                       TARGET-PATH-SLASH1
                       TARGET-PATH-SLASH2.

           IF ( EXT-ROUTE-BUF  = "00" )
              MOVE GETENV-BUF TO TARGET-FIL
           ELSE
           IF ( EXT-ROUTE-BUF  = "40" OR "50" ) AND
              ( TARGET-FIL     = SPACES       )
              MOVE GETENV-BUF TO TARGET-FIL.

           MOVE "S  A012TARGET." TO VDU.
           MOVE TARGET-PATH1 TO VDUBUF.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-SOURCE-PATH.
           MOVE "E  A120SOURCE." TO VDU.
           PERFORM SCREENIO.
           IF F7-KEY
              MOVE "END" TO IO-TYPE
              GO TO ENTER-PARAMETERS-EXIT.

           IF NOT ENTER-KEY GO TO ENTER-SOURCE-PATH.
           IF VDUBUF = SPACES
              GO TO ENTER-SOURCE-PATH.

      *    SOURCE FILE ACCESS CHECK:
      *    - TO PREVENT BOMBING OUT IF BRANCH'S FILES NOT ON THIS SYSTEM
      
           MOVE VDUBUF TO DIR-ACCESS-BUF SOURCE-PATH1.
           PERFORM DIR-ACCESS-CALL.
           IF NOT STAT-VALID
              MOVE "SOURCE PATH DOES NOT EXIST" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-SOURCE-PATH.

           IF SOURCE-PATH1 = TARGET-PATH1
              MOVE "SOURCE PATH CANNOT MATCH TARGET PATH."
                 TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-SOURCE-PATH.

      *----------------------------------------------------------------
       BR-01.
           MOVE "E  A000BR1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO ENTER-PARAMETERS-EXIT.
           IF VDUSTATUS = "16" GO TO ALL-BR.
           IF VDUSTATUS = "1>" GO TO ENTER-PARAMETERS-EXIT.
           IF VDUSTATUS = "1U" GO TO ENTER-SOURCE-PATH.
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
              GO TO ENTER-BEG-DATE.
      *----------------------------------------------------------------
       BRANCH-IS-NUMERIC.
           MOVE "N"           TO GROUP-FLAG.
           MOVE WGR-SRESULT   TO BEG-BRANCH.
           MOVE "SN N040BR1." TO VDU.
           MOVE BEG-BRANCH    TO VDUNUM0.
           PERFORM SCREENIO.
      *----------------------------------------------------------------
       BR-02.
           MOVE "ENNN040BR2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO ENTER-PARAMETERS-EXIT.
           IF VDUSTATUS = "10" GO TO ENTER-PARAMETERS-EXIT.
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BR-02.
           MOVE VDUNUM0 TO END-BRANCH.
           IF BEG-BRANCH > END-BRANCH
              MOVE "INVALID RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO BR-01.
           GO TO ENTER-BEG-DATE.
      *-----------------------------------------------------------------
       ALL-BR.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0             TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999          TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "N"           TO GROUP-FLAG.
      *----------------------------------------------------------------
       ENTER-BEG-DATE.

           IF ( EXT-ROUTE-BUF NOT = "00" )
              GO TO SKIP-LARGE-FILES.

           MOVE "ENNN040DATE1." TO VDU.
           PERFORM SCREENIO.
           IF F7-KEY
              MOVE "END" TO IO-TYPE
              GO TO ENTER-PARAMETERS-EXIT.
           IF F1-KEY
              GO TO ENTER-PARAMETERS-EXIT.
           IF UP-KEY GO TO ENTER-SOURCE-PATH.
           IF NOT ENTER-KEY 
              GO TO ENTER-BEG-DATE.
           MOVE VDUNUM0 TO BEG-DATE.
           IF BEG-MM < 1 OR > 12
              MOVE "PLEASE ENTER A VALID MONTH/YEAR" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-DATE.
           MOVE VDUNUM0 TO BEG-DATE.
      *----------------------------------------------------------------
       ENTER-END-DATE.

           IF ( EXT-ROUTE-BUF NOT = "00" )
              GO TO BR-02.

           MOVE "ENNN040DATE2." TO VDU.
           PERFORM SCREENIO.
           IF F7-KEY
              MOVE "END" TO IO-TYPE
              GO TO ENTER-PARAMETERS-EXIT.
           IF F1-KEY
              GO TO ENTER-PARAMETERS-EXIT.
           IF UP-KEY GO TO ENTER-BEG-DATE.
           IF NOT ENTER-KEY 
              GO TO ENTER-END-DATE.
           MOVE VDUNUM0 TO END-DATE.
           IF END-MM < 1 OR > 12
              MOVE "PLEASE ENTER A VALID MONTH/YEAR" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-END-DATE.
      *-----------------------------------------------------------------
       SKIP-LARGE-FILES.
           MOVE "E  A000SKIPLARGE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO ENTER-PARAMETERS-EXIT.
           IF VDUSTATUS = "10" GO TO ENTER-SOURCE-PATH.
           IF VDUSTATUS = "1U" GO TO ENTER-END-DATE.
           IF VDUSTATUS NOT = "00" GO TO SKIP-LARGE-FILES.
           MOVE VDUBUF TO SKIP-LARGE-FG.
           IF SKIP-LARGE-FG NOT = "N" 
              IF SKIP-LARGE-FG NOT = "Y"
                 GO TO SKIP-LARGE-FILES.

      *=================================================================
       ENTER-PARAMETERS-EXIT.
           EXIT.

      ******************************************************************
      *
      *    D I S P L A Y - P A R A M E T E R S
      *
      ******************************************************************
       DISPLAY-PARAMETERS SECTION.

           MOVE "S  A120SOURCE." TO VDU.
           MOVE SOURCE-PATH1     TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A120TARGET." TO VDU.
           MOVE TARGET-PATH1     TO VDUBUF.
           PERFORM SCREENIO.

           IF GROUP-FLAG = "Y"
              MOVE "S  A000BR1." TO VDU
              MOVE ENT-GROUP     TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A000BR2." TO VDU
              MOVE SPACES        TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "SNNN040BR1." TO VDU
              MOVE BEG-BRANCH    TO VDUNUM0
              PERFORM SCREENIO
              MOVE "SNNN040BR2." TO VDU
              MOVE END-BRANCH    TO VDUNUM0
              PERFORM SCREENIO.

           MOVE "S  A000SKIPLARGE." TO VDU.
           MOVE SKIP-LARGE-FG       TO VDUBUF.
           PERFORM SCREENIO.

      *=================================================================
       DISPLAY-PARAMETERS-EXIT.
           EXIT.

      ******************************************************************
      *
      *    E O - E X E C - A U D I T
      *
      ******************************************************************
       EO-EXEC-AUDIT SECTION.

           MOVE EXT-EOBUF-DATE1            TO DATE-YYYYMMDD.
           MOVE DATE-YYYYMMDD-MM           TO BEG-MM.
           MOVE DATE-YYYYMMDD-YY           TO BEG-YY.

           MOVE EXT-EOBUF-DATE2            TO DATE-YYYYMMDD.
           MOVE DATE-YYYYMMDD-MM           TO END-MM.
           MOVE DATE-YYYYMMDD-YY           TO END-YY.

       EXIT-EO-EXEC-AUDIT.
           EXIT.

      ***********************************************
       PURGE-OPEN-OUTPUT-BW SECTION.
      ***********************************************

           PERFORM OPEN-BW1-FILE.

           MOVE BR-NO         TO BW-OWNBR.
           MOVE 0             TO BW-SSNO.

           PERFORM START-BW1-FILE.

       PURGE-OPEN-OUTPUT-BW-NEXT.
           PERFORM READ-BW1-FILE-NEXT.
           IF ( IO-BAD ) OR ( BW-OWNBR NOT = BW-PATH-OWNBR )
              PERFORM CLOSE-BW1-FILE
              GO TO PURGE-OPEN-OUTPUT-BW-EXIT.

           PERFORM DELETE-BW1-FILE.
           IF ( IO-BAD )
              GO TO IO-ERROR.

           GO TO PURGE-OPEN-OUTPUT-BW-NEXT.

       PURGE-OPEN-OUTPUT-BW-EXIT.
           EXIT.

      ***********************************************
       PURGE-OPEN-OUTPUT-BX SECTION.
      ***********************************************

           PERFORM OPEN-BX1-FILE.

           MOVE BR-NO         TO BX-BRNO.
           MOVE 0             TO BX-SSNO.

           PERFORM START-BX1-FILE.

       PURGE-OPEN-OUTPUT-BX-NEXT.
           PERFORM READ-BX1-FILE-NEXT.
           IF ( IO-BAD ) OR ( BX-BRNO NOT = BX-PATH-OWNBR )
              PERFORM CLOSE-BX1-FILE
              GO TO PURGE-OPEN-OUTPUT-BX-EXIT.

           PERFORM DELETE-BX1-FILE.
           IF ( IO-BAD )
              GO TO IO-ERROR.

           GO TO PURGE-OPEN-OUTPUT-BX-NEXT.


       PURGE-OPEN-OUTPUT-BX-EXIT.
           EXIT.

      *************************************
      *     UPDATE NEW SOC.SEC.NO.
      *              MODULE
      *************************************
       UPDATE-MODULE SECTION.
           PERFORM UPDATE-BW1-FILE.
           IF RTNCD = "END"
              GO TO END-UPDATE-MODULE.

           PERFORM UPDATE-LN4-FILE.
           IF RTNCD = "END"
              GO TO END-UPDATE-MODULE.

           PERFORM UPDATE-LN5-FILE.
           IF RTNCD = "END"
              GO TO END-UPDATE-MODULE.

      
      *    PERFORM UPDATE-LN6-FILE.
      *    IF RTNCD = "END"
      *       GO TO END-UPDATE-MODULE.

      *    PERFORM UPDATE-LN7-FILE.
      *    IF RTNCD = "END"
      *       GO TO END-UPDATE-MODULE.

           PERFORM UPDATE-LH1-FILE.
           IF RTNCD = "END"
              GO TO END-UPDATE-MODULE.

           IF GP-I-AM-WORLD
              PERFORM UPDATE-CI-FILE
              IF RTNCD = "END"
                 GO TO END-UPDATE-MODULE.

       END-UPDATE-MODULE.
           EXIT.

      *******************************************
      *    CREATE BORROWER RECORD WITH NEW SSNO:
      *******************************************
       UPDATE-BW1-FILE SECTION.
           MOVE SPACES TO RTNCD.

       GET-RANDOM-SSNO.
           MOVE FUNCTION RANDOM() TO RANDOM-10.
           MULTIPLY RANDOM-10 BY 10000000000 GIVING RANDOM-WK.
           MOVE RANDOM-9 TO NEW-SSNO.
           IF SS-1 = 0      GO TO GET-RANDOM-SSNO.
           IF SS-2 = 0      GO TO GET-RANDOM-SSNO.
           IF SS-3 = 0      GO TO GET-RANDOM-SSNO.
           IF SS-FIRST4 = 0 GO TO GET-RANDOM-SSNO.
           IF SS-1 = 666    GO TO GET-RANDOM-SSNO.
           IF SS-CHAR1 = 8  GO TO GET-RANDOM-SSNO.
           IF SS-CHAR1 = 9  GO TO GET-RANDOM-SSNO.
           IF (SS-1 > 728 AND SS-1 < 750)
              GO TO GET-RANDOM-SSNO.
           IF (SS-1 > 771 AND SS-1 < 800)
              GO TO GET-RANDOM-SSNO.
           MOVE 0 TO RETRY-COUNTER.
       READ-NEW-BW.
           MOVE NEW-SSNO TO BW-SSNO.
           PERFORM READ-BW1-FILE.
           IF IO-FG NOT = 0
              GO TO CREATE-NEW-BW.

           ADD 1 TO RETRY-COUNTER.
           IF RETRY-COUNTER < 10
              ADD 1 TO NEW-SSNO
              GO TO READ-NEW-BW.

           MOVE "PROBLEM WITH SSNO &&&&&&&&&, ENDING PROCESS" TO MESS.
           INSPECT MESS REPLACING FIRST "&&&&&&&&&" BY SRC-BW-SSNO.
           PERFORM SEND-MESS.

           GO TO END-PROGRAM.

       CREATE-NEW-BW.
           MOVE SRC-BW-REC TO BW-REC.
           MOVE NEW-SSNO TO BW-SSNO.
           MOVE FUNCTION RANDOM() TO RANDOM-10.
           MULTIPLY RANDOM-10 BY 10000000000 GIVING RANDOM-WK.
           IF RANDOM-3A = 0 ADD 1 TO RANDOM-3A.
           MOVE FIRST-NAME(RANDOM-3A) TO BW-FNAME.
           IF RANDOM-3B = 0 ADD 1 TO RANDOM-3B.
           MOVE LAST-NAME(RANDOM-3B) TO BW-LNAME.
           IF RANDOM-4 = 0 ADD 1 TO RANDOM-4.
           IF RANDOM-3C = 0 ADD 1 TO RANDOM-3C.
           IF RANDOM-3A > 500
              MOVE RANDOM-3C TO NEW-ADDR-NO
           ELSE
              MOVE RANDOM-4 TO NEW-ADDR-NO.
           MOVE FUNCTION RANDOM() TO RANDOM-10.
           MULTIPLY RANDOM-10 BY 10000000000 GIVING RANDOM-WK.
           IF RANDOM-2A = 0 ADD 1 TO RANDOM-2A.
           MOVE STREET-NAME(RANDOM-2A) TO NEW-ADDR-ST.
           IF RANDOM-2B = 0 ADD 1 TO RANDOM-2B.
           MOVE STREET-TYPE(RANDOM-2B) TO NEW-ADDR-TYPE.
           MOVE SPACES TO BW-ADR1.
           STRING NEW-ADDR-NO " " NEW-ADDR-ST " " NEW-ADDR-TYPE
             DELIMITED BY "  " INTO BW-ADR1.
           CALL "C$JUSTIFY" USING BW-ADR1, "L".
           MOVE SPACES TO BW-ADR2.
           IF RANDOM-2C = 0 ADD 1 TO RANDOM-2C.
           MOVE CITY(RANDOM-2C)  TO BW-CITY.
           MOVE STATE(RANDOM-2C) TO BW-STATE.
           MOVE ZIP(RANDOM-2C)   TO BW-ZIP.

           IF RANDOM-2D = 0 ADD 1 TO RANDOM-2D.
           MOVE EMPLOYER(RANDOM-2D) TO BW-EMPLOYER.

           MOVE TODAYS-DATE TO NDTE-DATE.
           MOVE RANDOM-3D TO NDTE-HOLD.
           PERFORM INCREMENT-DAYS.
           IF RANDOM-3E = 0 ADD 1 TO RANDOM-3E.
           COMPUTE BIRTH-YEAR ROUNDED =
                CENTURY-WK - (.8 * (RANDOM-3E / 12)).
           MOVE BIRTH-YEAR TO NDTE-YY.
           MOVE NDTE-DATE TO BW-BDATE.

           MOVE FUNCTION RANDOM() TO RANDOM-10.
           MULTIPLY RANDOM-10 BY 10000000000 GIVING BW-PHONE.
           IF BW-PHONE < 2010000000
              ADD 3000000000 TO BW-PHONE.
           MOVE FUNCTION RANDOM() TO RANDOM-10.
           MULTIPLY RANDOM-10 BY 10000000000 GIVING BW-EMPPHONE.
           IF BW-EMPPHONE < 2010000000
              ADD 1000000000 TO BW-EMPPHONE.
           MOVE FUNCTION RANDOM() TO RANDOM-10.
           MULTIPLY RANDOM-10 BY 10000000000 GIVING BW-CELL-PHONE.
           IF BW-CELL-PHONE < 2010000000
              ADD 2000000000 TO BW-CELL-PHONE.
           MULTIPLY RANDOM-10 BY 10000000000 GIVING RANDOM-WK.
           IF RANDOM-2A = 0 ADD 1 TO RANDOM-2A.
           MOVE STREET-NAME(RANDOM-2A) TO NEW-ADDR-ST.
           IF RANDOM-2B = 0 ADD 1 TO RANDOM-2B.
           MOVE STREET-TYPE(RANDOM-2B) TO NEW-ADDR-TYPE.
           MOVE SPACES TO BW-EMPADDR1.
           STRING NEW-ADDR-NO " " NEW-ADDR-ST " " NEW-ADDR-TYPE
             DELIMITED BY "  " INTO BW-EMPADDR1.
           CALL "C$JUSTIFY" USING BW-EMPADDR1, "L".
           
           MOVE SPACES TO BW-EMPADDR2.
           STRING BW-CITY " " BW-STATE " " BW-ZIP
             DELIMITED BY "  " INTO BW-EMPADDR2.
           CALL "C$JUSTIFY" USING BW-EMPADDR2, "L".
      *    MOVE BW-CITY TO BW-EMPADDR2.
      *    INSPECT BW-EMPADDR2 REPLACING FIRST "    " BY ", &&".
      *    INSPECT BW-EMPADDR2 REPLACING FIRST "&&" BY BW-STATE.
      *    INSPECT BW-EMPADDR2 REPLACING FIRST "          " 
      *                                             BY BW-ZIP
      *                                  AFTER INITIAL " ".

           
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
              MOVE 0 TO BW-MISC-PHONE(SUB)
              MOVE "  " TO BW-MISC-PHONE-CD(SUB)
           END-PERFORM.
           MOVE SPACES TO BW-SPLNAME BW-SPFNAME.
           MOVE SPACES TO BW-EMAIL

           PERFORM WRITE-BW1-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.
       CREATE-BX.
           MOVE SRC-BX-REC TO BX-REC.
           MOVE NEW-SSNO   TO BX-SSNO.
           MOVE SPACES     TO BX-ALT-ADR1
                              BX-ALT-ADR2
                              BX-ALT-CITY
                              BX-ALT-STATE
                              BX-ALT-ZIP
                              BX-KYC-ADR1
                              BX-KYC-ADR2
                              BX-KYC-CITY
                              BX-KYC-STATE
                              BX-KYC-ZIP
                              BX-KYC-IDTYPE
                              BX-KYC-NUMBER.
           PERFORM WRITE-BX1-FILE.
           IF IO-FG NOT = 0 GO TO IO-ERROR.
       CREATE-WK.
           MOVE SRC-BW-SSNO TO WK-OLD-SSNO.
           MOVE NEW-SSNO    TO WK-NEW-SSNO.
           MOVE BW-FNAME    TO WK-NEW-FNAME.
           MOVE BW-LNAME    TO WK-NEW-LNAME.
           MOVE BW-BDATE    TO WK-NEW-BDATE.
           MOVE BW-ADR1     TO WK-NEW-ADR1.
           MOVE BW-ADR1     TO WK-NEW-ADR1.
           MOVE BW-CITY     TO WK-NEW-CITY.
           MOVE BW-STATE    TO WK-NEW-STATE.
           MOVE BW-ZIP      TO WK-NEW-ZIP.
           PERFORM WRITE-WK-FILE.
           IF IO-FG NOT = 0 GO TO IO-ERROR.

      ***************************************************
      *    CHANGE ALL LOAN RECORDS WHERE THIS BORROWER
      *    APPEARS AS A MAKER (LN-SSNO(1)):
      ****************************************************
       UPDATE-LN4-FILE SECTION.
           MOVE SPACES TO RTNCD.

       UPDATE-LN4-NEXT.
           PERFORM OPEN-LN4-FILE.
           MOVE SRC-BW-SSNO TO LN-SSNO(1).
           PERFORM START-LN4-FILE.
           IF IO-FG NOT = 0 
              GO TO END-UPDATE-LN4-FILE.

           PERFORM READ-LN4-FILE-NEXT.
           IF IO-BAD OR (LN-OWNBR NOT = LN-PATH-OWNBR)
                      OR (LN-SSNO(1) NOT = SRC-BW-SSNO)
              GO TO END-UPDATE-LN4-FILE.

           PERFORM UPDATE-LP1-FILE.
           MOVE NEW-SSNO TO LN-SSNO(1).
           PERFORM REWRITE-LN4-FILE.

           PERFORM UPDATE-LE1-FILE.

           PERFORM UPDATE-LTB1-FILE.
           PERFORM UPDATE-LTL1-FILE.

           GO TO UPDATE-LN4-NEXT.
       END-UPDATE-LN4-FILE.
           PERFORM CLOSE-LN4-FILE.

      ****************************************************
      *    CHANGE ALL LOAN RECORDS WHERE THIS BORROWER
      *    APPEARS AS A MAKER (LN-SSNO(2)):
      ****************************************************
       UPDATE-LN5-FILE SECTION.
           MOVE SPACES TO RTNCD.

       UPDATE-LN5-NEXT.
           PERFORM OPEN-LN5-FILE.
           MOVE SRC-BW-SSNO TO LN-SSNO(2).
           PERFORM START-LN5-FILE.
           IF IO-FG NOT = 0 
              GO TO END-UPDATE-LN5-FILE.

           PERFORM READ-LN5-FILE-NEXT.
           IF IO-BAD OR (LN-OWNBR NOT = LN-PATH-OWNBR)
                      OR (LN-SSNO(2) NOT = SRC-BW-SSNO)
              GO TO END-UPDATE-LN5-FILE.

           PERFORM UPDATE-LP1-FILE.
           MOVE NEW-SSNO TO LN-SSNO(2).
           PERFORM REWRITE-LN5-FILE.

           GO TO UPDATE-LN5-NEXT.
       END-UPDATE-LN5-FILE.
           PERFORM CLOSE-LN5-FILE.

      ****************************************************
      *    CHANGE ALL LOAN RECORDS WHERE THIS BORROWER
      *    APPEARS AS A MAKER (LN-SSNO(3)):
      ****************************************************
      *UPDATE-LN6-FILE SECTION.
      *    MOVE SPACES TO RTNCD.

      *UPDATE-LN6-NEXT.
      *    PERFORM OPEN-LN6-FILE.
      *    MOVE SRC-BW-SSNO TO LN-SSNO(3).
      *    PERFORM START-LN6-FILE.
      *    IF IO-FG NOT = 0 
      *       GO TO END-UPDATE-LN6-FILE.

      *    PERFORM READ-LN6-FILE-NEXT.
      *    IF IO-BAD OR (LN-OWNBR NOT = LN-PATH-OWNBR)
      *               OR (LN-SSNO(3) NOT = SRC-BW-SSNO)
      *       GO TO END-UPDATE-LN6-FILE.

      *    PERFORM UPDATE-LP1-FILE.
      *    MOVE NEW-SSNO TO LN-SSNO(3).
      *    PERFORM REWRITE-LN6-FILE.

      *    GO TO UPDATE-LN6-NEXT.
      *END-UPDATE-LN6-FILE.
      *    PERFORM CLOSE-LN6-FILE.

      ****************************************************
      *    CHANGE ALL LOAN RECORDS WHERE THIS BORROWER
      *    APPEARS AS A MAKER (LN-SSNO(4)):
      ****************************************************
      *UPDATE-LN7-FILE SECTION.
      *    MOVE SPACES TO RTNCD.

      *UPDATE-LN7-NEXT.
      *    PERFORM OPEN-LN7-FILE.
      *    MOVE SRC-BW-SSNO TO LN-SSNO(4).
      *    PERFORM START-LN7-FILE.
      *    IF IO-FG NOT = 0 
      *       GO TO END-UPDATE-LN7-FILE.

      *    PERFORM READ-LN7-FILE-NEXT.
      *    IF IO-BAD OR (LN-OWNBR NOT = LN-PATH-OWNBR)
      *               OR (LN-SSNO(4) NOT = SRC-BW-SSNO)
      *       GO TO END-UPDATE-LN7-FILE.

      *    PERFORM UPDATE-LP1-FILE.
      *    MOVE NEW-SSNO TO LN-SSNO(4).
      *    PERFORM REWRITE-LN7-FILE.

      *    GO TO UPDATE-LN7-NEXT.
      *END-UPDATE-LN7-FILE.
      *    PERFORM CLOSE-LN7-FILE.


      ****************************************************
      *    CHANGE ALL LOAN PAYMENT RECORDS WHERE THIS
      *    BORROWER APPEARS AS A MAKER (LP-SSNO):
      ****************************************************
       UPDATE-LP1-FILE SECTION.
           PERFORM OPEN-LP1-FILE.

           MOVE LP-PATH-OWNBR TO LP-BRNO.
           MOVE LN-ACCTNO     TO LP-ACCTNO.
           MOVE 0             TO LP-SEQNO.
           PERFORM START-LP1-FILE.
       UPDATE-LP1-NEXT.
           PERFORM READ-LP1-FILE-NEXT.
           IF IO-FG NOT = 0 
              GO TO END-UPDATE-LP1-FILE.

           IF (LP-PATH-OWNBR NOT = LP-BRNO) OR
               (LN-ACCTNO NOT = LP-ACCTNO)
              GO TO END-UPDATE-LP1-FILE.

           IF LP-SSNO = SRC-BW-SSNO
              MOVE NEW-SSNO TO LP-SSNO
              PERFORM REWRITE-LP1-FILE.
      *    ELSE
      *       UNLOCK LP-FILE.

           GO TO UPDATE-LP1-NEXT.

       END-UPDATE-LP1-FILE.
           PERFORM CLOSE-LP1-FILE.
 
      ****************************************************
      *    DELETE ALL LOAN HISTORY RECORDS WITH OLD SSNO
      *    KEY AND RE-CREATE WITH NEW KEY:
      *****************************************************
       UPDATE-LH1-FILE SECTION.
           MOVE SPACES TO RTNCD.

       UPDATE-LH1-NEXT.
           PERFORM OPEN-LH1-FILE.

           MOVE SRC-BW-SSNO TO LH-SSNO.
           MOVE 0           TO LH-ACCTNO.
           PERFORM START-LH1-FILE.
           PERFORM READ-LH1-FILE-NEXT.
           IF (IO-FG NOT = 0) OR (LH-BRNO NOT = LH-PATH-OWNBR)
              GO TO END-UPDATE-LH1-FILE.

           IF LH-SSNO NOT = SRC-BW-SSNO
              GO TO END-UPDATE-LH1-FILE.

           MOVE LH-REC TO HOLD-LH-REC.
           PERFORM DELETE-LH1-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           MOVE HOLD-LH-REC TO LH-REC.
           MOVE NEW-SSNO TO LH-SSNO.
           PERFORM WRITE-LH1-FILE.
           GO TO UPDATE-LH1-NEXT.
       END-UPDATE-LH1-FILE.
           PERFORM CLOSE-LH1-FILE.

      ****************************************************
      *    DELETE ALL LOAN ENDORSER RECORDS WITH OLD SSNO
      *    KEY AND RE-CREATE WITH NEW KEY:
      *****************************************************
       UPDATE-LE1-FILE SECTION.
           PERFORM OPEN-LE1-FILE.

           MOVE LE-PATH-OWNBR TO LE-BRNO.
           MOVE LN-ACCTNO     TO LE-ACCTNO.
           MOVE "E"           TO LE-TYPE.
           MOVE SRC-BW-SSNO   TO LE-SSNO.
           MOVE SPACES        TO LE-FLAG.
           MOVE 0             TO LE-OSACCTNO.
           PERFORM START-LE1-FILE.
       UPDATE-LE1-NEXT.
           PERFORM READ-LE1-FILE-NEXT.
           IF (IO-FG NOT = 0) OR (LE-BRNO NOT = LE-PATH-OWNBR)
              GO TO END-UPDATE-LE1-FILE.

           IF LE-ACCTNO NOT = LN-ACCTNO
              GO TO END-UPDATE-LE1-FILE.
           IF LE-TYPE NOT = "E"
              GO TO UPDATE-LE1-NEXT.
           IF LE-SSNO NOT = SRC-BW-SSNO
              GO TO END-UPDATE-LE1-FILE.

           MOVE LE-REC TO HOLD-LE-REC.
           PERFORM DELETE-LE1-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           MOVE HOLD-LE-REC TO LE-REC.
           MOVE NEW-SSNO TO LE-SSNO.
           PERFORM WRITE-LE1-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           GO TO UPDATE-LE1-NEXT.
       END-UPDATE-LE1-FILE.
           PERFORM CLOSE-LE1-FILE.

      ****************************************************
      * CLEAR CONFIDENTIAL INFO FROM LOAN TRAILERS
      *****************************************************
       UPDATE-LTB1-FILE SECTION.

           PERFORM OPEN-LTB1-FILE.
           MOVE LN-OWNBR  TO LTB-BRNO.
           MOVE LN-ACCTNO TO LTB-ACCTNO.
           MOVE 1         TO LTB-SEQNO.
           PERFORM READ-LTB1-FILE.
           IF IO-FG NOT = 0
              GO TO END-UPDATE-LTB1-FILE.

           MOVE SPACES   TO LTB-NAME(1).
           STRING BW-FNAME," ",BW-LNAME DELIMITED BY "  "
              INTO LTB-NAME(1).
           MOVE BW-ADR1  TO LTB-ADR1(1).
           MOVE BW-ADR2  TO LTB-ADR2(1).
           MOVE BW-CITY  TO LTB-CITY(1).
           MOVE BW-STATE TO LTB-STATE(1).
           MOVE BW-ZIP   TO LTB-ZIP(1).
           MOVE BW-PHONE TO LTB-PHONE(1).
           MOVE SPACES TO LTB-BANKRUPT-TABLE(2)
                          LTB-BANKRUPT-TABLE(3).
           MOVE 0 TO LTB-PHONE(2) 
                     LTB-PHONE(3).

           PERFORM REWRITE-LTB1-FILE.
       END-UPDATE-LTB1-FILE.
           PERFORM CLOSE-LTB1-FILE.

      ****************************************************
      * CLEAR CONFIDENTIAL INFO FROM LOAN TRAILERS
      *****************************************************
       UPDATE-LTL1-FILE SECTION.

           PERFORM OPEN-LTL1-FILE.
           MOVE LN-OWNBR  TO LTL-BRNO.
           MOVE LN-ACCTNO TO LTL-ACCTNO.
           MOVE 1         TO LTL-SEQNO.
           PERFORM READ-LTL1-FILE.
           IF IO-FG NOT = 0
              GO TO END-UPDATE-LTL1-FILE.

            MOVE SPACES   TO LTL-TITLE-NAME.
            STRING BW-FNAME, " ", BW-LNAME DELIMITED BY "  "
               INTO LTL-TITLE-NAME.
            MOVE BW-ADR1  TO LTL-TITLE-ADR
            MOVE BW-ADR2  TO LTL-TITLE-ADR2
            MOVE BW-CITY  TO LTL-TITLE-CITY
            MOVE BW-STATE TO LTL-TITLE-STATE
            MOVE BW-ZIP   TO LTL-TITLE-ZIP

            MOVE SPACES TO LTL-TITLE-COMMENT1
                           LTL-TITLE-COMMENT2.
             
           PERFORM REWRITE-LTL1-FILE.
       END-UPDATE-LTL1-FILE.
           PERFORM CLOSE-LTL1-FILE.

      ***********************************************************
      * CHANGE ALL CUST INFO RECORDS WHERE THIS BORROWER APPEARS
      *************************************************************
       UPDATE-CI-FILE SECTION.
           MOVE SPACES TO RTNCD.

       UPDATE-CI2-FILE.
           PERFORM OPEN-CI2-FILE.

           MOVE CI-PATH-OWNBR TO CI-BRNO.
           MOVE SRC-BW-SSNO   TO CI-SSNO.
           PERFORM START-CI2-FILE.
       UPDATE-CI2-NEXT.
           PERFORM READ-CI2-FILE-NEXT.
           IF IO-FG NOT = 0 OR (CI-BRNO NOT = CI-PATH-OWNBR)
              GO TO UPDATE-CI3-FILE.

           IF CI-SSNO NOT = SRC-BW-SSNO
              GO TO UPDATE-CI3-FILE.

           MOVE NEW-SSNO TO CI-SSNO.
           PERFORM REWRITE-CI2-FILE.
           GO TO UPDATE-CI2-NEXT.
       UPDATE-CI3-FILE.
           PERFORM CLOSE-CI2-FILE.

           PERFORM OPEN-CI3-FILE.
           MOVE CI-PATH-OWNBR TO CI-BRNO.
           MOVE SRC-BW-SSNO   TO CI-SSNO-2.
           PERFORM START-CI3-FILE.
       UPDATE-CI3-NEXT.
           PERFORM READ-CI3-FILE-NEXT.
           IF IO-FG NOT = 0 OR (CI-BRNO NOT = CI-PATH-OWNBR)
              GO TO END-UPDATE-CI-FILE.

           IF CI-SSNO-2 NOT = SRC-BW-SSNO
              GO TO END-UPDATE-CI-FILE.

           MOVE NEW-SSNO TO CI-SSNO-2.
           PERFORM REWRITE-CI3-FILE.
           GO TO UPDATE-CI3-NEXT.

       END-UPDATE-CI-FILE.
           PERFORM CLOSE-CI3-FILE.

      *******************************************
      * CREATE INVESTOR RECORD WITH NEW SSNO:
      *******************************************
      *UPDATE-IV1-FILE SECTION.
      *    MOVE SPACES TO RTNCD.

      *GET-RANDOM-IV-SSNO.
      *    MOVE FUNCTION RANDOM() TO RANDOM-10.
      *    MULTIPLY RANDOM-10 BY 10000000000 GIVING RANDOM-WK.
      *    MOVE RANDOM-9 TO NEW-SSNO.
      *    IF SS-1 = 0      GO TO GET-RANDOM-IV-SSNO.
      *    IF SS-2 = 0      GO TO GET-RANDOM-IV-SSNO.
      *    IF SS-3 = 0      GO TO GET-RANDOM-IV-SSNO.
      *    IF SS-FIRST4 = 0 GO TO GET-RANDOM-IV-SSNO.
      *    IF SS-1 = 666    GO TO GET-RANDOM-IV-SSNO.
      *    IF SS-CHAR1 = 8  GO TO GET-RANDOM-IV-SSNO.
      *    IF SS-CHAR1 = 9  GO TO GET-RANDOM-IV-SSNO.
      *    IF (SS-1 > 728 AND SS-1 < 750)
      *       GO TO GET-RANDOM-IV-SSNO.
      *    IF (SS-1 > 771 AND SS-1 < 800)
      *       GO TO GET-RANDOM-IV-SSNO.
      *    MOVE 0 TO RETRY-COUNTER.
      *READ-NEW-IV.
      *    MOVE NEW-SSNO TO IV-SSNO.
      *    PERFORM READ-IV1-FILE.
      *    IF IO-FG NOT = 0
      *       GO TO CREATE-NEW-IV.
      *    ADD 1 TO RETRY-COUNTER.
      *    IF RETRY-COUNTER < 10
      *       ADD 1 TO NEW-SSNO
      *       GO TO READ-NEW-IV.
      *    MOVE "PROBLEM WITH SSNO &&&&&&&&&&&&, ENDING PROCESS" TO MESS.
      *    MOVE SRC-IV-SSNO TO D-SSNO.
      *    INSPECT D-SSNO REPLACING ALL "/" BY "-".
      *    INSPECT MESS REPLACING FIRST "&&&&&&&&&&&&" BY D-SSNO.
      *    PERFORM SEND-MESS.
      *    GO TO END-PROGRAM.
      *CREATE-NEW-IV.
      *    MOVE SRC-IV-REC TO IV-REC.
      *    MOVE NEW-SSNO TO IV-SSNO.
      *    MOVE FUNCTION RANDOM() TO RANDOM-10.
      *    MULTIPLY RANDOM-10 BY 10000000000 GIVING RANDOM-WK.
      *    IF RANDOM-3A = 0 ADD 1 TO RANDOM-3A.
      *    MOVE FIRST-NAME(RANDOM-3A) TO IV-FNAME.
      *    IF RANDOM-3B = 0 ADD 1 TO RANDOM-3B.
      *    MOVE LAST-NAME(RANDOM-3B) TO IV-LNAME.
      *    IF RANDOM-4 = 0 ADD 1 TO RANDOM-4.
      *    IF RANDOM-3C = 0 ADD 1 TO RANDOM-3C.
      *    IF RANDOM-3A > 500
      *       MOVE RANDOM-3C TO NEW-ADDR-NO
      *    ELSE
      *       MOVE RANDOM-4 TO NEW-ADDR-NO.
      *    MOVE FUNCTION RANDOM() TO RANDOM-10.
      *    MULTIPLY RANDOM-10 BY 10000000000 GIVING RANDOM-WK.
      *    IF RANDOM-2A = 0 ADD 1 TO RANDOM-2A.
      *    MOVE STREET-NAME(RANDOM-2A) TO NEW-ADDR-ST.
      *    IF RANDOM-2B = 0 ADD 1 TO RANDOM-2B.
      *    MOVE STREET-TYPE(RANDOM-2B) TO NEW-ADDR-TYPE.
      *    MOVE SPACES TO IV-ADR1(1).
      *    STRING NEW-ADDR-NO " " NEW-ADDR-ST " " NEW-ADDR-TYPE
      *      DELIMITED BY "  " INTO IV-ADR1(1).
      *    CALL "C$JUSTIFY" USING IV-ADR1(1), "L".
      *    MOVE IV-ADR1(1) TO IV-ADR1(2).
      *    MOVE SPACES TO IV-ADR2(1) IV-ADR2(2).
      *    IF RANDOM-2C = 0 ADD 1 TO RANDOM-2C.
      *    MOVE CITY(RANDOM-2C)  TO IV-CITY(1) IV-CITY(2).
      *    MOVE STATE(RANDOM-2C) TO IV-STATE(1) IV-STATE(2).
      *    MOVE ZIP(RANDOM-2C)   TO IV-ZIP(1) IV-ZIP(2).

      *    MOVE TODAYS-DATE TO NDTE-DATE.
      *    MOVE RANDOM-3D TO NDTE-HOLD.
      *    PERFORM INCREMENT-DAYS.
      *    IF RANDOM-3E = 0 ADD 1 TO RANDOM-3E.
      *    COMPUTE BIRTH-YEAR ROUNDED =
      *         CENTURY-WK - (.8 * (RANDOM-3E / 12)).
      *    MOVE BIRTH-YEAR TO NDTE-YY.
      *    MOVE NDTE-DATE TO IV-BIRTH-DATE.

      *    MOVE FUNCTION RANDOM() TO RANDOM-10.
      *    MULTIPLY RANDOM-10 BY 10000000000 GIVING IV-PHONE(1).
      *    IF IV-PHONE(1) < 2010000000
      *       ADD 1000000000 TO IV-PHONE(1).
      *    MOVE IV-PHONE(1) TO IV-PHONE(2).

      *    MOVE SPACES TO NAME-WORKER.
      *    STRING IV-FNAME, " ", 
      *           IV-LNAME, DELIMITED BY SIZE INTO NAME-WORKER.
      *    MOVE NAME-WORKER TO IV-SALUTE(1)
      *                        IV-SALUTE(2).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    IT-REC WAS MERGED INTO IV-REC.               KEC 2015.1222
      *    THE KYC FIELDS WERE IN IT-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    MOVE IV-ADR1(1)  TO IV-KYC-ADR1.
      *    MOVE IV-CITY(1)  TO IV-KYC-CITY.
      *    MOVE IV-STATE(1) TO IV-KYC-STATE.
      *    MOVE IV-ZIP(1)   TO IV-KYC-ZIP.
      *    MOVE "D"         TO IV-KYC-IDTYPE.
      *    MOVE FUNCTION RANDOM() TO RANDOM-10.
      *    MULTIPLY RANDOM-10 BY 10000000000 GIVING RANDOM-WK.
      *    MOVE RANDOM-WK TO IV-KYC-NUMBER.
      *    MOVE TODAYS-DATE TO NDTE-DATE.
      *    COMPUTE NDTE-HOLD = RANDOM-3B * -1.
      *    PERFORM INCREMENT-DAYS.
      *    MOVE NDTE-DATE TO IV-KYC-ISSDTE.
      *    MOVE 48 TO NDTE-HOLD.
      *    PERFORM INCREMENT-MONTHS.
      *    MOVE NDTE-DATE TO IV-KYC-EXPDTE.
      *    MOVE IV-KYC-ISSDTE TO NDTE-DATE.
      *    COMPUTE NDTE-HOLD = RANDOM-2A * -1.
      *    PERFORM INCREMENT-DAYS.
      *    MOVE NDTE-DATE TO IV-KYC-LST-VER-DTE IV-KYC-ASK-DATE.
      *    MOVE "Y" TO IV-KYC-ASK-BUSINESS.
      *    

      *    PERFORM WRITE-IV1-FILE.
      *    IF IO-FG NOT = 0
      *       GO TO IO-ERROR.

      ***************************************************
      *    CHANGE ALL NOTE RECORDS WHERE THIS INVESTOR
      *    APPEARS (NT2-KEY):
      ****************************************************
      *UPDATE-NT2-FILE SECTION.
      *    MOVE SPACES TO RTNCD.
      *UPDATE-NT2-NEXT.
      *    PERFORM OPEN-NT2-FILE.
      *    MOVE SRC-IV-SSNO TO NT-SSNO(1).
      *    PERFORM START-NT2-FILE.
      *    IF IO-FG NOT = 0 
      *       GO TO END-UPDATE-NT2-FILE.
      *    PERFORM READ-NT2-FILE-NEXT.
      *    IF IO-FG NOT = 0
      *       GO TO END-UPDATE-NT2-FILE.
      *    IF NT-SSNO(1) NOT = SRC-IV-SSNO
      *       GO TO END-UPDATE-NT2-FILE.
      *    MOVE NEW-SSNO TO NT-SSNO(1).
      *    MOVE SPACES TO NAME-WORKER.
      *    STRING IV-FNAME, " ", 
      *           IV-LNAME, DELIMITED BY SIZE INTO NAME-WORKER.
      *    MOVE NAME-WORKER TO NT-ALPHA(1).
      *    MOVE SPACES TO NT-ALPHA(2).
      *    MOVE SPACES TO NT-ALPHA(3).
      *    PERFORM REWRITE-NT2-FILE.
      *    GO TO UPDATE-NT2-NEXT.
      *END-UPDATE-NT2-FILE.
      *    PERFORM CLOSE-NT2-FILE.

      ***************************************************
      *    CHANGE ALL NOTE RECORDS WHERE THIS INVESTOR
      *    APPEARS (NT3-KEY):
      ****************************************************
      *UPDATE-NT3-FILE SECTION.
      *    MOVE SPACES TO RTNCD.
      *UPDATE-NT3-NEXT.
      *    PERFORM OPEN-NT3-FILE.
      *    MOVE SRC-IV-SSNO TO NT-SSNO(2).
      *    PERFORM START-NT3-FILE.
      *    IF IO-FG NOT = 0 
      *       GO TO END-UPDATE-NT3-FILE.
      *    PERFORM READ-NT3-FILE-NEXT.
      *    IF IO-FG NOT = 0 
      *       GO TO END-UPDATE-NT3-FILE.
      *    IF NT-SSNO(2) NOT = SRC-IV-SSNO
      *       GO TO END-UPDATE-NT3-FILE.
      *    MOVE NEW-SSNO TO NT-SSNO(2).
      *    MOVE SPACES TO NAME-WORKER.
      *    STRING IV-FNAME, " ", 
      *           IV-LNAME, DELIMITED BY SIZE INTO NAME-WORKER.
      *    MOVE NAME-WORKER TO NT-ALPHA(2).
      *    PERFORM REWRITE-NT3-FILE.
      *    GO TO UPDATE-NT3-NEXT.
      *END-UPDATE-NT3-FILE.
      *    PERFORM CLOSE-NT3-FILE.

      ***************************************************
      *    CHANGE ALL NOTE RECORDS WHERE THIS INVESTOR
      *    APPEARS (NT4-KEY):
      ****************************************************
      *UPDATE-NT4-FILE SECTION.
      *    MOVE SPACES TO RTNCD.
      *UPDATE-NT4-NEXT.
      *    PERFORM OPEN-NT4-FILE.
      *    MOVE SRC-IV-SSNO TO NT-SSNO(3).
      *    PERFORM START-NT4-FILE.
      *    IF IO-FG NOT = 0 
      *       GO TO END-UPDATE-NT4-FILE.
      *    PERFORM READ-NT4-FILE-NEXT.
      *    IF IO-FG NOT = 0 
      *       GO TO END-UPDATE-NT4-FILE.
      *    IF NT-SSNO(3) NOT = SRC-IV-SSNO
      *       GO TO END-UPDATE-NT4-FILE.
      *    MOVE NEW-SSNO TO NT-SSNO(3).
      *    PERFORM REWRITE-NT4-FILE.
      *    GO TO UPDATE-NT4-NEXT.
      *END-UPDATE-NT4-FILE.
      *    PERFORM CLOSE-NT4-FILE.

      ***************************************************
      *    CHANGE ALL NOTE RECORDS WHERE THIS INVESTOR
      *    APPEARS (NT5-KEY):
      ****************************************************
      *UPDATE-NT5-FILE SECTION.
      *    MOVE SPACES TO RTNCD.
      *UPDATE-NT5-NEXT.
      *    PERFORM OPEN-NT5-FILE.
      *    MOVE SRC-IV-SSNO TO NT-SSNO(4).
      *    PERFORM START-NT5-FILE.
      *    IF IO-FG NOT = 0 
      *       GO TO END-UPDATE-NT5-FILE.
      *    PERFORM READ-NT5-FILE-NEXT.
      *    IF IO-FG NOT = 0 
      *       GO TO END-UPDATE-NT5-FILE.
      *    IF NT-SSNO(4) NOT = SRC-IV-SSNO
      *       GO TO END-UPDATE-NT5-FILE.
      *    MOVE NEW-SSNO TO NT-SSNO(4).
      *    PERFORM REWRITE-NT5-FILE.
      *    GO TO UPDATE-NT5-NEXT.
      *END-UPDATE-NT5-FILE.
      *    PERFORM CLOSE-NT5-FILE.

      ********************************************************
      *  CLEAR SRC-BX FIELDS
      ********************************************************
       CLEAR-SRC-BX-FILE SECTION.
           MOVE SPACES TO SRC-BX-REC.
           MOVE 0 TO SRC-BX-KYC-ISSDATE
                     SRC-BX-KYC-EXPDATE
                     SRC-BX-KYC-LAST-VERDATE.

               
      ****************************************************
      *  IF BORROWER REC HAS A SPOUSE SSNO, READ WORK FILE
      *  & UPDATE TO NEW-SSNO.
      ****************************************************
       UPDATE-SPOUSE-SSNOS SECTION.
           MOVE BW-PATH-OWNBR TO BW-OWNBR.
           MOVE 0             TO BW-SSNO.
           PERFORM START-BW1-FILE.
       NEXT-BW.
           PERFORM READ-BW1-FILE-NEXT.
           IF (IO-FG = 9) OR (BW-OWNBR NOT = BW-PATH-OWNBR)
              GO TO END-UPDATE-SPOUSE-SSNO.

           IF BW-SPSSNO = 0
              GO TO NEXT-BW.

           MOVE BW-SPSSNO TO WK-OLD-SSNO.
           PERFORM READ-WK-FILE.
           IF IO-FG NOT = 0
              GO TO REDACT-SPOUSE.

           MOVE WK-NEW-SSNO TO BW-SPSSNO.
           MOVE WK-NEW-FNAME TO BW-SPFNAME.
           MOVE WK-NEW-LNAME TO BW-SPLNAME.
           MOVE WK-NEW-BDATE TO BW-SP-BDATE.

           PERFORM REWRITE-BW1-FILE.
           GO TO NEXT-BW.

       REDACT-SPOUSE.
           MOVE FUNCTION RANDOM() TO RANDOM-10.
           MULTIPLY RANDOM-10 BY 10000000000 GIVING RANDOM-WK.
           MOVE RANDOM-9 TO BW-SPSSNO.
           IF RANDOM-3A = 0 ADD 1 TO RANDOM-3A.
           MOVE FIRST-NAME(RANDOM-3A) TO BW-SPFNAME.
           MOVE BW-LNAME TO BW-SPLNAME.
           IF RANDOM-3B = 0 ADD 1 TO RANDOM-3B.

           MOVE BW-BDATE TO NDTE-DATE.
           MOVE RANDOM-3B TO NDTE-HOLD.
           PERFORM INCREMENT-DAYS.
           MOVE NDTE-DATE TO BW-SP-BDATE.

           PERFORM REWRITE-BW1-FILE.
           GO TO NEXT-BW.
       END-UPDATE-SPOUSE-SSNO.
           EXIT.

      ****************************************************
      *    REMOVE ALL SSNO'S FROM CKFILE & CHANGE
      *    NAMES TO SOMETHING GENERIC
      ****************************************************
       UPDATE-CK1-FILE SECTION.
           PERFORM OPEN-CK1-FILE.

           MOVE CK-PATH-OWNBR TO CK-BRNO.
           MOVE 0             TO CK-NO.
           PERFORM START-CK1-FILE.
       UPDATE-CK1-NEXT.
           PERFORM READ-CK1-FILE-NEXT.
           IF IO-FG NOT = 0  OR (CK-BRNO NOT = CK-PATH-OWNBR)
              GO TO END-UPDATE-CK1-FILE.

           MOVE 0 TO CK-SSNO.
           MOVE CK-NO TO D-CHECK-NO.
           MOVE "PAYEE CHECK #&&&&&&" TO CK-PAYEE.
           INSPECT CK-PAYEE REPLACING FIRST "&&&&&&" BY D-CHECK-NO.
           MOVE "REASON CHECK #&&&&&&" TO CK-REASON.
           INSPECT CK-REASON REPLACING FIRST "&&&&&&" BY D-CHECK-NO.
           MOVE SPACES TO CK-ADR1 CK-ADR2 CK-CITY CK-STATE CK-ZIP.
           PERFORM REWRITE-CK1-FILE.
           GO TO UPDATE-CK1-NEXT.
       END-UPDATE-CK1-FILE.
           PERFORM CLOSE-CK1-FILE.

      **************************************************
      * CHANGE ALL TR RECORDS WHERE THIS BORROWER APPEARS
      ****************************************************
       UPDATE-TR1-FILE SECTION.
           MOVE SPACES TO RTNCD.

           MOVE BEG-MM TO DATE-MMDDYY-MM.
           MOVE BEG-YY TO DATE-MMDDYY-YY.
           MOVE 01     TO DATE-MMDDYY-DD.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO BEGINNING-DATE.
           MOVE END-MM TO DATE-MMDDYY-MM.
           MOVE END-YY TO DATE-MMDDYY-YY.
           MOVE 31     TO DATE-MMDDYY-DD.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO ENDING-DATE.

           PERFORM UPDATE-TRB-FILE.
           PERFORM UPDATE-TRS-FILE.
           PERFORM UPDATE-TRL-FILE.
           PERFORM UPDATE-TRJ-FILE.
           PERFORM UPDATE-TRK-FILE.
           PERFORM UPDATE-TRX-FILE.

           PERFORM UPDATE-TRC-FILE.
           PERFORM UPDATE-TRP-FILE.


      *************************************************
      *   PAYMENTS
      *************************************************
       UPDATE-TRP-FILE SECTION.
      *************************************************

      *    PERFORM LOAD-TRP1-FILE.
      *    MOVE TRP-SELECT-PATH TO ACCESS-BUF.
      *    PERFORM ACCESS-CALL.
      *    IF STAT NOT = "00"
      *       GO TO EXIT-UPDATE-TRP-FILE.

       PROCESS-TRP.
           PERFORM OPEN-TRP1-FILE.

           MOVE TRP-PATH-OWNBR  TO TP-BRNO.
           MOVE BEGINNING-DATE  TO TP-DATE.
           MOVE 0               TO TP-POOLID TP-CLASS TP-ACCTNO
                                   TP-LINE.
           PERFORM START-TRP1-FILE.
       NEXT-TRP.
           PERFORM READ-TRP1-FILE-NEXT.
           IF IO-FG NOT = 0 OR (TRP-PATH-OWNBR NOT = TP-BRNO)
              PERFORM CLOSE-TRP1-FILE
              GO TO EXIT-UPDATE-TRP-FILE.

           MOVE TP-SSNO TO WK-OLD-SSNO.
           PERFORM READ-WK-FILE.
           IF IO-GOOD
              MOVE WK-NEW-SSNO TO TP-SSNO
           ELSE
              MOVE 0 TO TP-SSNO.

           PERFORM REWRITE-TRP1-FILE.

           GO TO NEXT-TRP.

       EXIT-UPDATE-TRP-FILE.
           PERFORM CLOSE-TRP1-FILE.

      *************************************************
      *   BRANCH (BULK) TRANSFERS
      *************************************************
       UPDATE-TRB-FILE SECTION.
      *************************************************

           PERFORM LOAD-TRB1-FILE.
      *    MOVE TRB-SELECT-PATH TO ACCESS-BUF.
      *    PERFORM ACCESS-CALL.
      *    IF STAT NOT = "00"
      *       GO TO EXIT-UPDATE-TRB-FILE.

       PROCESS-TRB.
           PERFORM OPEN-TRB1-FILE.
           MOVE TRB-PATH-OWNBR  TO TB-BRNO.
           MOVE BEGINNING-DATE  TO TB-DATE.
           MOVE 0               TO TB-CLASS TB-ACCTNO TB-LAWCODE
                                   TB-LINE.
           PERFORM START-TRB1-FILE.
       NEXT-TRB.
           PERFORM READ-TRB1-FILE-NEXT.
           IF IO-FG NOT = 0 OR (TRB-PATH-OWNBR NOT = TB-BRNO)
              PERFORM CLOSE-TRB1-FILE
              GO TO EXIT-UPDATE-TRB-FILE.

           IF TB-DATE > ENDING-DATE
              PERFORM CLOSE-TRB1-FILE
              GO TO EXIT-UPDATE-TRB-FILE.

           MOVE 1 TO SUB.
       TRB-SSNO-LOOP.
           MOVE TB-SSNO(SUB) TO WK-OLD-SSNO.

           IF WK-OLD-SSNO = 0
              GO TO UPDATE-TRB-SSNO.

           PERFORM READ-WK-FILE.
           IF IO-FG NOT = 0
              MOVE 0 TO WK-NEW-SSNO.

           MOVE WK-NEW-SSNO TO TB-SSNO(SUB).

           IF SUB < 4
              ADD 1 TO SUB
              GO TO TRB-SSNO-LOOP.

       UPDATE-TRB-SSNO.
           PERFORM REWRITE-TRB1-FILE.

           GO TO NEXT-TRB.

       EXIT-UPDATE-TRB-FILE.
           PERFORM CLOSE-TRB1-FILE.

      *************************************************
      *   SALES FINANCE 
      *************************************************
       UPDATE-TRS-FILE SECTION.
      *************************************************

           PERFORM LOAD-TRS1-FILE.
      *    MOVE TRS-SELECT-PATH TO ACCESS-BUF.
      *    PERFORM ACCESS-CALL.
      *    IF STAT NOT = "00"
      *       GO TO EXIT-UPDATE-TRS-FILE.

       PROCESS-TRS.
           PERFORM OPEN-TRS1-FILE.
           MOVE TRS-PATH-OWNBR  TO TS-BRNO.
           MOVE BEGINNING-DATE  TO TS-DATE.
           MOVE 0               TO TS-CLASS TS-ACCTNO TS-LAWCODE
                                   TS-LINE.
           PERFORM START-TRS1-FILE.
       NEXT-TRS.
           PERFORM READ-TRS1-FILE-NEXT.
           IF IO-FG NOT = 0 OR (TRS-PATH-OWNBR NOT = TS-BRNO)
              PERFORM CLOSE-TRS1-FILE
              GO TO EXIT-UPDATE-TRS-FILE.

           IF TS-DATE > ENDING-DATE
              PERFORM CLOSE-TRS1-FILE
              GO TO EXIT-UPDATE-TRS-FILE.

           MOVE 1 TO SUB.
       TRS-SSNO-LOOP.
           MOVE TS-SSNO(SUB) TO WK-OLD-SSNO.

           IF WK-OLD-SSNO = 0
              GO TO UPDATE-TRS-SSNO.

           PERFORM READ-WK-FILE.
           IF IO-FG NOT = 0
              MOVE 0 TO WK-NEW-SSNO.

           MOVE WK-NEW-SSNO TO TS-SSNO(SUB).

           IF SUB < 4
              ADD 1 TO SUB
              GO TO TRS-SSNO-LOOP.

       UPDATE-TRS-SSNO.
           PERFORM REWRITE-TRS1-FILE.

           GO TO NEXT-TRS.

       EXIT-UPDATE-TRS-FILE.
           PERFORM CLOSE-TRS1-FILE.

      *************************************************
      *   DIRECT LOAN
      *************************************************
       UPDATE-TRL-FILE SECTION.
      *************************************************

           PERFORM LOAD-TRL1-FILE.
      *    MOVE TRL-SELECT-PATH TO ACCESS-BUF.
      *    PERFORM ACCESS-CALL.
      *    IF STAT NOT = "00"
      *       GO TO EXIT-UPDATE-TRL-FILE.

       PROCESS-TRL.
           PERFORM OPEN-TRL1-FILE.
           MOVE TRL-PATH-OWNBR  TO TL-BRNO.
           MOVE BEGINNING-DATE  TO TL-DATE.
           MOVE 0               TO TL-CLASS TL-ACCTNO TL-LAWCODE
                                   TL-LINE.
           PERFORM START-TRL1-FILE.
       NEXT-TRL.
           PERFORM READ-TRL1-FILE-NEXT.
           IF IO-FG NOT = 0 OR (TRL-PATH-OWNBR NOT = TL-BRNO)
              PERFORM CLOSE-TRL1-FILE
              GO TO EXIT-UPDATE-TRL-FILE.

           IF TL-DATE > ENDING-DATE
              PERFORM CLOSE-TRL1-FILE
              GO TO EXIT-UPDATE-TRL-FILE.

           MOVE 1 TO SUB.
       TRL-SSNO-LOOP.
           MOVE TL-SSNO(SUB) TO WK-OLD-SSNO.

           IF WK-OLD-SSNO = 0
              GO TO UPDATE-TRL-SSNO.

           PERFORM READ-WK-FILE.
           IF IO-FG NOT = 0
              MOVE 0 TO WK-NEW-SSNO.

           MOVE WK-NEW-SSNO TO TL-SSNO(SUB).

           IF SUB < 4
              ADD 1 TO SUB
              GO TO TRL-SSNO-LOOP.

       UPDATE-TRL-SSNO.
           PERFORM REWRITE-TRL1-FILE.

           GO TO NEXT-TRL.

       EXIT-UPDATE-TRL-FILE.
           PERFORM CLOSE-TRL1-FILE.

      *************************************************
      *   CLASS TRANSFERS WITH ONE BRANCH
      *************************************************
       UPDATE-TRJ-FILE SECTION.
      *************************************************

           PERFORM LOAD-TRJ1-FILE.
      *    MOVE TRJ-SELECT-PATH TO ACCESS-BUF.
      *    PERFORM ACCESS-CALL.
      *    IF STAT NOT = "00"
      *       GO TO EXIT-UPDATE-TRJ-FILE.

       PROCESS-TRJ.
           PERFORM OPEN-TRJ1-FILE.
           MOVE TRJ-PATH-OWNBR  TO TJ-BRNO.
           MOVE BEGINNING-DATE  TO TJ-DATE.
           MOVE 0               TO TJ-CLASS TJ-ACCTNO TJ-LAWCODE
                                   TJ-LINE.
           PERFORM START-TRJ1-FILE.
       NEXT-TRJ.
           PERFORM READ-TRJ1-FILE-NEXT.
           IF IO-FG NOT = 0 OR (TRJ-PATH-OWNBR NOT = TJ-BRNO)
              PERFORM CLOSE-TRJ1-FILE
              GO TO EXIT-UPDATE-TRJ-FILE.

           IF TJ-DATE > ENDING-DATE
              PERFORM CLOSE-TRJ1-FILE
              GO TO EXIT-UPDATE-TRJ-FILE.

           MOVE 1 TO SUB.
       TRJ-SSNO-LOOP.
           MOVE TJ-SSNO(SUB) TO WK-OLD-SSNO.

           IF WK-OLD-SSNO = 0
              GO TO UPDATE-TRJ-SSNO.

           PERFORM READ-WK-FILE.
           IF IO-FG NOT = 0
              MOVE 0 TO WK-NEW-SSNO.

           MOVE WK-NEW-SSNO TO TJ-SSNO(SUB).

           IF SUB < 4
              ADD 1 TO SUB
              GO TO TRJ-SSNO-LOOP.

       UPDATE-TRJ-SSNO.
           PERFORM REWRITE-TRJ1-FILE.

           GO TO NEXT-TRJ.

       EXIT-UPDATE-TRJ-FILE.
           PERFORM CLOSE-TRJ1-FILE.

      *************************************************
      *   CONVERSION
      *************************************************
       UPDATE-TRK-FILE SECTION.
      *************************************************

           PERFORM LOAD-TRK1-FILE.
      *    MOVE TRK-SELECT-PATH TO ACCESS-BUF.
      *    PERFORM ACCESS-CALL.
      *    IF STAT NOT = "00"
      *       GO TO EXIT-UPDATE-TRK-FILE.

       PROCESS-TRK.
           PERFORM OPEN-TRK1-FILE.
           MOVE TRK-PATH-OWNBR  TO TK-BRNO.
           MOVE BEGINNING-DATE  TO TK-DATE.
           MOVE 0               TO TK-CLASS TK-ACCTNO TK-LAWCODE
                                   TK-LINE.
           PERFORM START-TRK1-FILE.
       NEXT-TRK.
           PERFORM READ-TRK1-FILE-NEXT.
           IF IO-FG NOT = 0 OR (TRK-PATH-OWNBR NOT = TK-BRNO)
              PERFORM CLOSE-TRK1-FILE
              GO TO EXIT-UPDATE-TRK-FILE.

           IF TK-DATE > ENDING-DATE
              PERFORM CLOSE-TRK1-FILE
              GO TO EXIT-UPDATE-TRK-FILE.

           MOVE 1 TO SUB.
       TRK-SSNO-LOOP.
           MOVE TK-SSNO(SUB) TO WK-OLD-SSNO.

           IF WK-OLD-SSNO = 0
              GO TO UPDATE-TRK-SSNO.

           PERFORM READ-WK-FILE.
           IF IO-FG NOT = 0
              MOVE 0 TO WK-NEW-SSNO.

           MOVE WK-NEW-SSNO TO TK-SSNO(SUB).

           IF SUB < 4
              ADD 1 TO SUB
              GO TO TRK-SSNO-LOOP.

       UPDATE-TRK-SSNO.
           PERFORM REWRITE-TRK1-FILE.

           GO TO NEXT-TRK.

       EXIT-UPDATE-TRK-FILE.
           PERFORM CLOSE-TRK1-FILE.

      *************************************************
      *   BRANCH (BULK) TRANSFERS
      *************************************************
       UPDATE-TRX-FILE SECTION.
      *************************************************

           PERFORM LOAD-TRX1-FILE.
      *    MOVE TRX-SELECT-PATH TO ACCESS-BUF.
      *    PERFORM ACCESS-CALL.
      *    IF STAT NOT = "00"
      *       GO TO EXIT-UPDATE-TRX-FILE.

       PROCESS-TRX.
           PERFORM OPEN-TRX1-FILE.
           MOVE TRX-PATH-OWNBR  TO TX-BRNO.
           MOVE BEGINNING-DATE  TO TX-DATE.
           MOVE 0               TO TX-CLASS TX-ACCTNO TX-LAWCODE
                                   TX-LINE.
           PERFORM START-TRX1-FILE.
       NEXT-TRX.
           PERFORM READ-TRX1-FILE-NEXT.
           IF IO-FG NOT = 0 OR (TRX-PATH-OWNBR NOT = TX-BRNO)
              PERFORM CLOSE-TRX1-FILE
              GO TO EXIT-UPDATE-TRX-FILE.

           IF TX-DATE > ENDING-DATE
              PERFORM CLOSE-TRX1-FILE
              GO TO EXIT-UPDATE-TRX-FILE.

           MOVE 1 TO SUB.
       TRX-SSNO-LOOP.
           MOVE TX-SSNO(SUB) TO WK-OLD-SSNO.

           IF WK-OLD-SSNO = 0
              GO TO UPDATE-TRX-SSNO.

           PERFORM READ-WK-FILE.
           IF IO-FG NOT = 0
              MOVE 0 TO WK-NEW-SSNO.

           MOVE WK-NEW-SSNO TO TX-SSNO(SUB).

           IF SUB < 4
              ADD 1 TO SUB
              GO TO TRX-SSNO-LOOP.

       UPDATE-TRX-SSNO.
           PERFORM REWRITE-TRX1-FILE.

           GO TO NEXT-TRX.

       EXIT-UPDATE-TRX-FILE.
           PERFORM CLOSE-TRX1-FILE.

      *************************************************
      *   CHECKS
      *************************************************
       UPDATE-TRC-FILE SECTION.
      *************************************************

           PERFORM LOAD-TRC1-FILE.
      *    MOVE TRC-SELECT-PATH TO ACCESS-BUF.
      *    PERFORM ACCESS-CALL.
      *    IF STAT NOT = "00"
      *       GO TO EXIT-UPDATE-TRC-FILE.

       PROCESS-TRC.
           PERFORM OPEN-TRC1-FILE.

           MOVE TRC-PATH-OWNBR  TO TC-BRNO.
           MOVE BEGINNING-DATE  TO TC-KEY-DATE.
           MOVE 0               TO TC-CKNO TC-CLASS TC-SEQ.

           PERFORM START-TRC1-FILE.
       NEXT-TRC.
           PERFORM READ-TRC1-FILE-NEXT.
           IF IO-FG NOT = 0 OR (TRC-PATH-OWNBR NOT = TC-BRNO)
              PERFORM CLOSE-TRC1-FILE
              GO TO EXIT-UPDATE-TRC-FILE.

           IF TC-KEY-DATE > ENDING-DATE
              PERFORM CLOSE-TRC1-FILE
              GO TO EXIT-UPDATE-TRC-FILE.

           MOVE "CHECK PAYEE"  TO TC-PAYEE.
           MOVE "CHECK REASON" TO TC-REASON.

           PERFORM REWRITE-TRC1-FILE.

           GO TO NEXT-TRC.

       EXIT-UPDATE-TRC-FILE.
           PERFORM CLOSE-TRC1-FILE.

      ****************************************************
      *    REDACT BANK INFO FROM DEALER FILE
      ****************************************************
       UPDATE-DL1-FILE SECTION.
           PERFORM OPEN-DL1-FILE.

           MOVE DL-PATH-OWNBR TO DL-BRNO.
           MOVE 0     TO DL-NUM.
           PERFORM START-DL1-FILE.
       UPDATE-DL1-NEXT.
           PERFORM READ-DL1-FILE-NEXT.
           IF IO-FG NOT = 0 OR (DL-BRNO NOT = DL-PATH-OWNBR)
              GO TO END-UPDATE-DL1-FILE.

           IF DL-ACH-FG = "Y"
              MOVE FUNCTION RANDOM() TO RANDOM-10
              MULTIPLY RANDOM-10 BY 10000000000 GIVING RANDOM-WK
              MOVE RANDOM-WK TO DL-BANK-ACCTNO
              MOVE 123 TO DL-BANK-ROUTING-NO
           ELSE
              MOVE 0 TO DL-BANK-ACCTNO.
           PERFORM REWRITE-DL1-FILE.
           GO TO UPDATE-DL1-NEXT.
       END-UPDATE-DL1-FILE.
           PERFORM CLOSE-DL1-FILE.

      ****************************************************
      *    REDACT TAX ID FROM PAYEE FILE
      ****************************************************
       UPDATE-PA1-FILE SECTION.
           PERFORM OPEN-PA1-FILE.
       UPDATE-PA1-NEXT.
           PERFORM READ-PA1-FILE-NEXT.
           IF IO-FG NOT = 0 OR PA-BRNO NOT = PA-PATH-OWNBR
              GO TO END-UPDATE-PA1-FILE.
           MOVE FUNCTION RANDOM() TO RANDOM-10.
           MULTIPLY RANDOM-10 BY 10000000000 GIVING RANDOM-WK.
           MOVE RANDOM-WK TO PA-1099-ID.
           PERFORM REWRITE-PA1-FILE.
           GO TO UPDATE-PA1-NEXT.
       END-UPDATE-PA1-FILE.
           PERFORM CLOSE-PA1-FILE.

      ****************************************************
      *    REDACT INFO FROM BRANCH FILE
      ****************************************************
       UPDATE-BR-FILE SECTION.
       NEXT-BRANCH.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-FG NOT = 0
              GO TO END-UPDATE-BR.

           MOVE FUNCTION RANDOM() TO RANDOM-10.
           MULTIPLY RANDOM-10 BY 10000000000 GIVING RANDOM-WK.
           MOVE RANDOM-WK TO BR-BANK-ACCTNO.
           MOVE 123 TO BR-BANK-ROUTING-NO.
           PERFORM REWRITE-BR-FILE.
           GO TO NEXT-BRANCH.
       END-UPDATE-BR.
           IF GROUP-FLAG = "N"
              MOVE BEG-BRANCH TO BR-KEY
              PERFORM START-BR-FILE
              PERFORM READ-BR-FILE-NEXT.
              
      **********************************************************************
      * REDACT INFO IN GP-FILE
      **********************************************************************
       UPDATE-GP-FILE SECTION.
           PERFORM OPEN-GP-FILE.

FLR   *    IF UPDATE-GLOBAL-FG = "Y"                         *> FLR G2G
FLR   *       MOVE 6 TO GP-KEY
FLR   *       PERFORM READ-GP-FILE
FLR   *       IF IO-GOOD
FLR   *          MOVE FUNCTION RANDOM() TO RANDOM-10
FLR   *          MULTIPLY RANDOM-10 BY 10000000000 GIVING RANDOM-WK
FLR   *          MOVE RANDOM-WK TO GP-BANK2-ACCTNO
FLR   *          MOVE FUNCTION RANDOM() TO RANDOM-10
FLR   *          MULTIPLY RANDOM-10 BY 10000000000 GIVING RANDOM-WK
FLR   *          MOVE RANDOM-WK TO GP-FED-TAX-ID2
FLR   *          MOVE 123 TO GP-BANK2-ROUTING-NO
FLR   *          PERFORM REWRITE-GP-FILE.                    *> FLR G2G

           MOVE 1 TO GP-KEY.
           PERFORM READ-GP-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           IF UPDATE-GLOBAL-FG = "Y"
              MOVE FUNCTION RANDOM() TO RANDOM-10
              MULTIPLY RANDOM-10 BY 10000000000 GIVING RANDOM-WK
              MOVE RANDOM-WK TO GP-BANK-ACCTNO
              MOVE FUNCTION RANDOM() TO RANDOM-10
              MULTIPLY RANDOM-10 BY 10000000000 GIVING RANDOM-WK
              MOVE RANDOM-WK TO GP-FED-TAX-ID
              MOVE 123 TO GP-BANK-ROUTING-NO
              PERFORM REWRITE-GP-FILE.

           PERFORM CLOSE-GP-FILE.


      ******************************************************************
      *
      *    F O R M - R E S E T
      *
      ******************************************************************
       FORM-RESET SECTION.

           MOVE EXT-CONAME-CONAME          TO REDACT-CONAME.
           MOVE EXT-CONAME-SYSDATE         TO REDACT-SYSDATE.
           DISPLAY REDACT-SCREEN         UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE REDACT-FORM-FIELDS         TO WR-BUF.
           MOVE REDACT-FORM-LIST           TO WR-LIST.
           PERFORM CALL-WRFORM.

      *=================================================================
       FORM-RESET-EXIT.
           EXIT.

      ******************************************************************
      *
      *    S E T U P - L I N K - I N F O
      *
      ******************************************************************
       SETUP-LINK-INFO SECTION.

           MOVE REDACT-HELP--FILE          TO HELP-LINK-FILE.
           MOVE REDACT-HELP--DIR           TO HELP-LINK-DIR.

      *=================================================================
       SETUP-LINK-INFO-EXIT.
           EXIT.

      ******************************************************************
      *
      *    V D U - C O N V E R T - F I E L D
      *
      ******************************************************************
       VDU-CONVERT-FIELD SECTION.

           MOVE "00"                       TO STAT.
           MOVE VDU-FIELD-TO-CONVERT       TO VDU-ALP-FIELD.

           EVALUATE TRUE
              COPY "LIBSP/REDACT_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *=================================================================
       VDU-CONVERT-FIELD-EXIT.
           EXIT.

      ********************************
      *   MISC PARAGRAPHS
      ********************************
       MISC-PARAGRAPHS SECTION.
       COPY "LIBGB/GETDAT.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/RMFILE.CPY".


EOLB  *-----------------------------------------------------------------
       GET-A-SUB.
           IF COUNTER = 99
              MOVE 1 TO COUNTER
           ELSE
              ADD 1 TO COUNTER.
           MOVE COUNTER TO SUB.
       SETUP-CITY-STATE-ZIP.
           MOVE BW-CITY TO NAME-WORKER.
           INSPECT NAME-WORKER REPLACING FIRST "   " BY ",? ".
           INSPECT NAME-WORKER REPLACING FIRST "  " BY
                                            BW-STATE.
           INSPECT NAME-WORKER REPLACING FIRST "   " BY "?  ".
           INSPECT NAME-WORKER REPLACING FIRST "          "
                                            BY BW-ZIP.
           INSPECT NAME-WORKER REPLACING ALL "?" BY " ".
       RECORD-DELETED.
           MOVE "RECORD WAS DELETED!" TO MESS.
           PERFORM SEND-MESS.
       CHANGE-AGAIN.
           MOVE "PLEASE RE-INPUT YOUR CHANGES!" TO MESS.
           PERFORM SEND-MESS.
       UNDERGONE-CHANGE.
           MOVE
           "THIS RECORD HAS UNDERGONE CHANGE BY ANOTHER USER,"
           TO MESS.
           PERFORM SEND-MESS.
       SEND-LEGEND.
           IF ( EXT-ROUTE-BUF = "00" )
              MOVE "S  A000MESS." TO VDU
              MOVE MESS TO VDUBUF
              PERFORM SCREENIO.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
 
      *********************************
      *   FILE I-O ROUTINES
      *********************************
       IO-ROUTINE SECTION.
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/GBGPGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBLP/LPBXGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLPGS_SQL.CPY".
       COPY "LIBLP/LPLHGS_SQL.CPY".
       COPY "LIBLP/LPLEGS_SQL.CPY".
       COPY "LIBLP/LPLTBGS_SQL.CPY".
       COPY "LIBLP/LPLTLGS_SQL.CPY".
       COPY "LIBLP/LPCKGS_SQL.CPY".
       COPY "LIBLP/LPTRBGS_SQL.CPY".
       COPY "LIBLP/LPTRSGS_SQL.CPY".
       COPY "LIBLP/LPTRLGS_SQL.CPY".
       COPY "LIBLP/LPTRJGS_SQL.CPY".
       COPY "LIBLP/LPTRKGS_SQL.CPY".
       COPY "LIBLP/LPTRXGS_SQL.CPY".
       COPY "LIBLP/LPTRCGS_SQL.CPY".
       COPY "LIBLP/LPTRPGS_SQL.CPY".
       COPY "LIBLP/LPCIGS_SQL.CPY".
       COPY "LIBLP/LPDLGS_SQL.CPY".
       COPY "LIBLP/LPPAGS_SQL.CPY".
       COPY "LIBGB/GBBRIN.CPY".
       COPY "LIBGB/GBGPIN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBLP/LPBW1IN.CPY".
       COPY "LIBLP/LPBW2RN.CPY".
       COPY "LIBLP/LPBW3RN.CPY".
       COPY "LIBLP/LPBW4RN.CPY".
       COPY "LIBLP/LPBX1IN.CPY".
       COPY "LIBLP/LPLN4IN.CPY".
       COPY "LIBLP/LPLN5IN.CPY".
       COPY "LIBLP/LPLP1IN.CPY".
       COPY "LIBLP/LPLH1IN.CPY".
       COPY "LIBLP/LPLE1IN.CPY".
       COPY "LIBLP/LPLTB1IN.CPY".
       COPY "LIBLP/LPLTL1IN.CPY".
       COPY "LIBLP/LPCK1IN.CPY".
       COPY "LIBLP/LPTRB1IN.CPY".
       COPY "LIBLP/LPTRS1IN.CPY".
       COPY "LIBLP/LPTRL1IN.CPY".
       COPY "LIBLP/LPTRJ1IN.CPY".
       COPY "LIBLP/LPTRK1IN.CPY".
       COPY "LIBLP/LPTRX1IN.CPY".
       COPY "LIBLP/LPTRC1IN.CPY".
       COPY "LIBLP/LPTRP1IN.CPY".
       COPY "LIBGB/GBWKIN.CPY".
       COPY "LIBLP/LPCI2IN.CPY".
       COPY "LIBLP/LPCI3IN.CPY".
       COPY "LIBLP/LPDL1IN.CPY".
       COPY "LIBLP/LPPA1IN.CPY".
       OPEN-SRC-BW1-FILE.
           PERFORM OPEN-IT.
           MOVE SRC-BW-PATH TO E-FILE.
           OPEN INPUT SRC-BW-FILE.
           IF IO-FG = 8
              GO TO OPEN-SRC-BW1-FILE.
           IF IO-FG = 7
              CLOSE SRC-BW-FILE
              GO TO OPEN-SRC-BW1-FILE.
       READ-SRC-BW1-FILE.
           PERFORM READ-IT.
           MOVE SRC-BW-PATH TO E-FILE.
           MOVE SRC-BW1-KEY TO E-KEYX.
           READ SRC-BW-FILE.
           IF IO-FG = 8
              GO TO READ-SRC-BW1-FILE.
       READ-SRC-BW1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE SRC-BW-PATH TO E-FILE.
           MOVE SRC-BW1-KEY TO E-KEYX.
           READ SRC-BW-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-SRC-BW1-FILE-NEXT.
       START-SRC-BW1-FILE.
           PERFORM START-IT.
           MOVE SRC-BW-PATH TO E-FILE.
           MOVE SRC-BW1-KEY TO E-KEYX.
           START SRC-BW-FILE KEY NOT < SRC-BW1-KEY.
       CLOSE-SRC-BW1-FILE.
           PERFORM CLOSE-IT.
           CLOSE SRC-BW-FILE.
       OPEN-SRC-BX1-FILE.
           PERFORM OPEN-IT.
           MOVE SRC-BX-PATH TO E-FILE.
           OPEN INPUT SRC-BX-FILE.
           IF IO-FG = 8
             GO TO OPEN-SRC-BX1-FILE.
           IF IO-FG = 7
             CLOSE SRC-BX-FILE
             GO TO OPEN-SRC-BX1-FILE.
       READ-SRC-BX1-FILE.
           PERFORM READ-IT.
           MOVE SRC-BX-PATH TO E-FILE.
           MOVE SRC-BX1-KEY TO E-KEYX.
           READ SRC-BX-FILE.
           IF IO-FG = 8
              GO TO READ-SRC-BX1-FILE.
       CLOSE-SRC-BX1-FILE.
           PERFORM CLOSE-IT.
           CLOSE SRC-BX-FILE.
      * OPEN-SRC-IV1-FILE.
      *     PERFORM OPEN-IT.
      *     MOVE SRC-IV-PATH TO E-FILE.
      *     OPEN INPUT SRC-IV-FILE.
      *     IF IO-FG = 8
      *        GO TO OPEN-SRC-IV1-FILE.
      *     IF IO-FG = 7
      *        CLOSE SRC-IV-FILE
      *        GO TO OPEN-SRC-IV1-FILE.
      * READ-SRC-IV1-FILE.
      *     PERFORM READ-IT.
      *     MOVE SRC-IV-PATH TO E-FILE.
      *     MOVE SRC-IV1-KEY TO E-KEYX.
      *     READ SRC-IV-FILE.
      *     IF IO-FG = 8
      *        GO TO READ-SRC-IV1-FILE.
      * READ-SRC-IV1-FILE-NEXT.
      *     PERFORM READ-IT.
      *     MOVE SRC-IV-PATH TO E-FILE.
      *     MOVE SRC-IV1-KEY TO E-KEYX.
      *     READ SRC-IV-FILE NEXT.
      *     IF IO-FG = 8
      *        GO TO READ-SRC-IV1-FILE-NEXT.
      * START-SRC-IV1-FILE.
      *     PERFORM START-IT.
      *     MOVE SRC-IV-PATH TO E-FILE.
      *     MOVE SRC-IV1-KEY TO E-KEYX.
      *     START SRC-IV-FILE KEY NOT < SRC-IV1-KEY.
      * CLOSE-SRC-IV1-FILE.
      *     PERFORM CLOSE-IT.
      *     CLOSE SRC-IV-FILE.

       COPY "LIBGB/FERRORS.CPY".
       END-PROGRAM SECTION.

           PERFORM CLOSE-FILES.

           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.

           MOVE MKTEMP-BUF TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.
       EXIT-PROG.

           IF ( SV-BEG-BRANCH NOT = 9999 )
              IF ( EXT-EOBUF-GROUP-FG = "Y" )
                 MOVE SV-BEG-BRANCH        TO BEG-BRANCH
                 MOVE SV-END-BRANCH        TO END-BRANCH
                 MOVE SV-GROUP-FLAG        TO GROUP-FLAG
                 MOVE SV-ENT-GROUP         TO ENT-GROUP
              ELSE
                 MOVE SV-BEG-BRANCH        TO BEG-BRANCH
                 MOVE SV-END-BRANCH        TO END-BRANCH.

           IF ( EXT-ROUTE-BUF = "00" )
              MOVE "LP/LPMENU" TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           IF ( EXT-ROUTE-BUF = "00" OR "40" OR "50" )
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY REDACT-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
