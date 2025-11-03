       IDENTIFICATION DIVISION.
       PROGRAM-ID.      BLKONE.
       DATE-WRITTEN. 4-23-13
      ******************************************************************
      *    DIR :  LP
      *    DESC:  TRANSFER ONE ACCOUNT BETWEEN BRANCHES
      *           IN THIS PROGRAM YOU SELECT ONE ACCOUNT, SOME CHECKS ARE
      *           DONE, THEN LP/BULKT2 IS CALLED TO UPDATE THE TRANSFER
      *           WORLD PR# 865
      ****************************************************
      ****************************************************
      * N O T E  : : :
      *   PLEASE VERIFY THAT CLAST1 DOES NOT NEED YOUR CHANGE !!
      *   THEY ARE VERY SIMILAR PROGRAMS.
      ****************************************************
      ****************************************************
      *
      * REV:
      *  101494 BAH REMOVED ALL OCCURANCES OF LP2
      *  061695 JTG ADDED SET OF CRNORC-BR-NOT-OURS-REFUND
      *  072895 JTG ADDED LOGIC FOR DELSUB-SET ROUTINE
      *  081395 JTG CORRECTED TO SET WK-TO-DLNO FOR BULKT2.C
      *  083195 KEC ADDED LIBLP/LPNOCL
      *  049996 MJD ADDED PASSWORD TO ACCOUNT RANGES.
      *             RANGE OF MORE THAN 1 ACCOUNT NEEDS PASSWORD.
      *  MJD 071696 ADDED LIBLP/LPUPWK FOR CHANGES IN LIBLP/AGEING
      *  JTG 091196 ADDED P&L ONLY OPTION
      *             ADDED COPY OF "LIBLP/BULKTRW"
      *  KEC 111496 CHANGED LN-POCD FOR RESCISSION.
      *  MJD 121096 CHANGED TO PERFORM LPSTAT-DISPLAY AFTER READING BWFILE.
      *  JTG 030797 ALLOWED TRANSFER OF REPO ACCOUNTS
      *  BAH 052797 ADDED SET OF LT-PATH
      *  MJD 082297 CHANGED LN-REC/CONPROFILE TO LN-REC/CONPROFILE(1)
      *  GLF 090297 REMOVED ALL USE OF GLOBAL
      *  MJD 102397 CHANGED TO PRINT 30,60,90,120,150,180,210+,M,P,CUR
      *  BAH 011398 ADDED CLOSE-WK-FILE AFTER THE OPEN OUTPUT, BECAUSE
      *             IT OPENS IT AGAIN...MERC WAS GETTING EXCLUSIVE ERROR
      *  JTG 031898 ADDED OPTION TO I,S,O ZERO BALANCE ACCOUNTS, WORLD #184
      *             THAT IS EXCLUDING VOIDED AND BULK TRANSFERED ACCOUNTS.
      *
      *             CORRECTED PERFORM OF PARAGRAPH WHICH ALSO HAD A GO TO TO
      *             A PARAGRAPH OUTSIDE OF THE PARAGRAPH BEING PERFORMED
      *
      *             CHANGED PROGRAM TO READ LN1 OR LN2 DEPENDING ON WHETHER
      *             A SINGLE ACCOUNT RANGE WAS SELECTED.
      *
      *             ALSO CHANGED WK-KEY TO INCLUDE LN-CLASS, SO REPORTS ARE
      *             CORRECTLY ORDERED.
      *  BLV 052998 DON'T ALLOW ANOTHER TRANSFER TO BRANCH IF UPDATE
      *             TAG EXISTS (UPDATE IS IN PROCESS, NO OVERWRITE!)
      *  MJD 990730 ADDED GPENV(W). NEEDED FOR CHANGES TO LPEARN.
      *  GLF 990923 REPLACED IO-FG NUMBERS WITH IO-FG 88 LEVEL PER DEVELOPMENT
      *             MEETING (082599)
      *  BLV 991021 FIXED - WAS READING SPR BASED ON LN-CLASS INSTEAD OF
      *             LN SPR CLASS!
      *  BAH 991230 CHANGED THE TEST ON LN-POCD, WAS ALLOWING YOU TO TRANSFER
      *             ACCOUNTS IN RESCISSION
      *  BLV 000301 VALIDATE GI FOR TARGET BRANCH, IF INVALID, SKIP LOAN &
      *             DOCUMENT ON REPORT
      *  BAH 010713 ADDED OPTION TO KEEP SAME DEALER NO IN TARGET BRANCH,
      *             VERIFY IT EXIST AND THE NAMES MUST MATCH,  OR ITS SKIPPED
      *             TFC PR# 1531
      *  CS  020117 ADDED BANKRUPT (I,S,O) [PR#1703 REGIONAL ACC]
      *  CS  020117 ADDED PL DATE RANGE IF INCLUDING P&L [PR#1703 REGIONAL ACC]
      *  BAH 040819 DO NOT CREATE FORM REQUEST IF LOAN OR BORROWER IS
      *             CONFIDENTIAL, PR# 2343
      *  BAH 050317 ADDED PO DATE RANGE IF INCLUDING POFF, CENTRIX, JIM R
      *   CS 070508 IF F3 AT RECENCY OR CONTRACTUAL ACCOUNTS WITH STATUS >
      *             9999 WERE SKIPPED. SKIP TEST IF BEG = 0 AND ENDING = 9999
      *             PER JR, FIX   REGENCY
      *  BLM 081022 ADD OPTION FOR REPORT-ONLY/EXTRACT ONLY: IF FORM # IS
      *             SELECTED WILL PROMPT FOR EXTRACT ONLY & WILL ONLY CREATE
      *             FORM REQUEST & NOT CREATE BULK WORK FILE OR FLAGS; IF NO
      *             FORM # ENTERED WILL PROMPT FOR REPORT-ONLY.  ADDED WINDOW
      *             TO ALLOW SELECTING UP TO 20 ZIP CODES TO INCLUDE INSTEAD
      *             OF ZIP CODE RANGE.  WORLD PR# 604
      *  BLM 130917 FIX PROBLEM WHEN ENTERING ACCT TO TRANSFER.  IF YOU
      *             ENTERED AN ACCT NOT ELIGIBLE TO TRANSFER, IT WASN'T
      *             RESETTING ERRCD, SO YOU COULDN'T ENTER A VALID ACCT
      *             UNLESS YOU F7 OUT & COME BACK IN.  WORLD PR# 929
      *  BAH 151110 ADDED PASSWORD FOR P&L ACCOUNT, MUST EXIST AND BE FLAGGED
      *             SO IT WILL STILL STOP OTHER CUSTOMERS, "EN", WORLD PR#1116
      *  BAH 160125 SPLIT OUT LTFILE, PD0003
      *
      * KEC 2016.0216 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         FIXED KEYS & RECS FOR FD-REC.
      *         CHANGED FMNO/FORMNO/NEXTFM/SETFM FROM 3 TO 4 BYTES.
      * BAH 170508 ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 181203 GLOBAL LNFILE
      * BAH 20190919 REMOVED ACCESS-CALL ON LNFILE
      *  CS 20191015 ADDED EXIT-PATHNAME TO CALL OF BULKT2
      * BAH 2022.0320 DO NOT ALLOW A TRANSFER MORE THAN ONCE WITHIN SAME
      *               MONTH, BUT ALLOW IT IF THE ENTERED DATE IS SAME MONTH,
      *               SO ACQUISITIONS CAN GET TRANSFERRED OUT.  PROBLEM IS
      *               WITH METRO2 REPORTING THE "L1" SEGMENT. IF TRANSFERRED
      *               MORE THAN ONCE, THE "BT" DOES NOT GET REPORTED ANYMORE
      *               AND THOSE ACCOUNTS WONT GET CLOSED OUT, #1547
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

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LP/BLKONE.CBL S35 02/21/24-11:43:26 >".
      ****************************
      *     F I L E   S P E C S  *
      ****************************
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01LSM.CPY".
       COPY "LIBLP/LP01LSM_SQL.CPY".
       COPY "LIBLP/LPWSLSM.CPY".
       COPY "LIBGL/GL01GI.CPY".
       COPY "LIBGL/GL01GI_SQL.CPY".
       COPY "LIBGL/GLWSGI.CPY".

       COPY "LIBLP/BULKTRW.CPY".
       01  TRANSFER-BUF.
           03  FR-BR              PIC 9999.
           03  FR-BR-X           REDEFINES FR-BR
                                  PIC XXXX.
           03  FR-MACHINE         PIC XX.
           03  FR-DATA-PATH       PIC X(50).
           03  TO-BR              PIC 9999.
           03  TO-MACHINE         PIC XX.
           03  TO-DATA-PATH       PIC X(50).
           03  TO-DLNO            PIC 9(4)V99.
           03  TRANSFER-ACCT      PIC 9(8).
           03  BLKONE-BUF         PIC X       VALUE " ".

      **********************
      *   MISC WORKERS     *
      **********************
       01  RECORDS-FOUND        PIC X        VALUE " ".
           88  NO-RECORDS-FOUND              VALUE "N".
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".
       01  TRANS-DATE           PIC 9(8)     COMP-3 VALUE 0.
       01  VALID-GI-FG          PIC X.

       01  NOSEL                PIC S9(9).
       01  AMTSEL               PIC S9(9)V99.

      ***********************************
      *     S C R E E N   F I E L D S   *
      ***********************************
       01  ERRCD                  PIC X.

       01  SCREEN-FIELDS.
           03  ACCTNO            PIC 9(6).

       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       01   BR-SEQ            PIC 9  VALUE 0.
      * SEQUENCE 1 IS IN LP/BULKTR.C FOR TRANSFERRING ACROSS MACHINES.
       01   ACCT-SEQ          PIC 9  VALUE 2.
       01   PL-SEQ            PIC 9  VALUE 3.
      * WHEN ADDING PASSWORD SEQUENCES, THIS BULKT1 PROGRAM USES "BULKTR"
      * AS THE PROGRAM IN THE PASSWORD FILE, SO YOU MUST MAKE SURE
      * THE PROGRAM LP/BULKTR.C DOES NOT ALREADY USE THE SEQUENCE YOUR
      * ADDING.
       01  BLKONE-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/BLKONE_DEF.CPY".
       COPY "LIBLP/BLKONE_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

      /***************************************
      *                                      *
      *     P R O G R A M   C O N T R O L    *
      *                                      *
      ****************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBLP/BLKONE_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

      *************************************
       MAIN-MODULE SECTION.
      *************************************

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "BLKONE" TO VDU-FORM-NAME FORM-PROG.

           DISPLAY STANDARD GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
      *             WITH NO SCROLL
      *             WITH NO WRAP
                   HANDLE IS BLKONE-WINDOW-HANDLE.

       COPY "LIBGB/CHKSEC.CPY".
           PERFORM FORM-RESET.

           PERFORM INITIALIZATION.
           IF ERRCD NOT = " "
              GO TO END-PROGRAM.

           PERFORM BULK-TRANSFER.

           GO TO END-PROGRAM.

      ******************************************
       BULK-TRANSFER SECTION.
      ******************************************

           MOVE "Y" TO RECORDS-FOUND.
           ADD 1 TO NOSEL.
           ADD LN-CURBAL TO AMTSEL.

           MOVE LN-ACCTNO TO TRANSFER-ACCT.
           MOVE "Y" TO BLKONE-BUF.
           MOVE "LP/BULKT2" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH TRANSFER-BUF EXIT-PATHNAME.                  
           CANCEL FORM-PROGX.

       BULK-TRANSFER-EXIT.
           EXIT.

      ***********************************
       INITIALIZATION SECTION.
      ***********************************

       SETRP.
           MOVE " " TO ERRCD.

           PERFORM ENTER-PARAMETERS.
           IF ERRCD NOT = " "
              IF ERRCD = "I"
                 GO TO SETRP
              ELSE
                 GO TO INITIALIZATION-EXIT.

           MOVE "N" TO RECORDS-FOUND.

           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO TRANS-DATE.

       SET-WK-FILE.
           IF EXT-CONAME-USER-LOC NOT = FR-BR
              MOVE BR-SEQ TO PASSWDW-SEQ
              MOVE "TO OVERRIDE YOUR BRANCH...:" TO PASSWDW-PROMPT2
              PERFORM VERIFY-PASSWORD
              IF PASSWDW-STAT NOT = "OK"
                 MOVE "OVERRIDES ARE NOT ALLOWED WITHOUT PASSWORD"
                                                          TO MESS
                 PERFORM SEND-MESS
                 MOVE "CAN NOT BULK TRANSFER FOR ANOTHER BRANCH"
                                                          TO MESS
                 PERFORM SEND-MESS
                 MOVE "X" TO ERRCD
                 GO TO INITIALIZATION-EXIT.

       SET-HEAD.
           MOVE "BRANCH TRANSFER IN PROGRESS........." TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE 0 TO NOSEL AMTSEL.

       INITIALIZATION-EXIT.
           EXIT.

      ****************************************
       ENTER-PARAMETERS SECTION.
      ****************************************
       ENTER-PARM.

       FR-BRANCH.
           MOVE "ENNN040FRBR." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              GO TO EXIT-PARM-ABORT.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO FR-BRANCH.
           MOVE VDUNUM0 TO FR-BR D-FR-BR.

           MOVE FR-BR TO BR-NO.
           PERFORM GET-BR.
           IF NOT IO-GOOD
              MOVE "CAN'T ACCESS THE BRANCH" TO MESS
              PERFORM SEND-MESS
              GO TO FR-BRANCH.

           MOVE "S  A210FRBRN." TO VDU.
           MOVE BR-NAME TO VDUBUF D-FR-BR-NAME.
           PERFORM SCREENIO.

      * FROM BRANCH
           MOVE EXT-FILPATH-BASE           TO LN-ALL-BASE.
           MOVE BR-MACHINE                 TO LN-ALL-MACHINE.
           MOVE BR-DATA-PATH               TO LN-ALL-DATA.
           MOVE "Y"                        TO LN-PATH-OVERRIDE.
           MOVE LN-ALL-PATH                TO LN-PATH.

           MOVE BR-STATE                   TO FR-STATE.
           MOVE BR-MACHINE                 TO FR-MACHINE.
           MOVE BR-DATA-PATH               TO FR-DATA-PATH.
       TO-BRANCH.
           MOVE "ENNN040TOBR." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM-ABORT.
           IF VDUSTATUS = "10" GO TO FR-BRANCH.
           IF VDUSTATUS = "1U" GO TO FR-BRANCH.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO TO-BRANCH.
           MOVE VDUNUM0 TO TO-BR D-TO-BR.

           IF FR-BR = TO-BR
              MOVE "INVALID BRANCH RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO TO-BRANCH.

           MOVE TO-BR TO BR-NO.
           PERFORM GET-BR.
           IF NOT IO-GOOD
              MOVE "CAN'T ACCESS THE BRANCH" TO MESS
              PERFORM SEND-MESS
              GO TO TO-BRANCH.

           MOVE "S  A180TOBRN." TO VDU.
           MOVE BR-NAME TO VDUBUF D-TO-BR-NAME.
           PERFORM SCREENIO.

      * TO BRANCH
           MOVE EXT-FILPATH-BASE           TO LN-ALL-BASE.
           MOVE BR-MACHINE                 TO LN-ALL-MACHINE.
           MOVE BR-DATA-PATH               TO LN-ALL-DATA.
           MOVE "Y"                        TO LN-PATH-OVERRIDE.
           MOVE LN-ALL-PATH                TO LN-PATH.

           IF FR-STATE = "IL"
              IF BR-STATE NOT = "IL"
                 MOVE "TRANSFERS OUT OF IL NOT ALLOWED" TO MESS
              PERFORM SEND-MESS
              GO TO TO-BRANCH.

           IF BR-STATE = "IL"
              IF FR-STATE NOT = "IL"
                 MOVE "TRANSFERS INTO IL NOT ALLOWED" TO MESS
              PERFORM SEND-MESS
              GO TO TO-BRANCH.

           IF FR-STATE NOT = BR-STATE
              MOVE "S  A000MESSX." TO VDU
              MOVE
            "TO & FROM BRANCHES ARE DIFFERENT STATES, IS THIS CORRECT?"
                 TO VDUBUF
              PERFORM SCREENIO
              MOVE "E  A010MESSXX." TO VDU
              PERFORM SCREENIO
              IF VDUBUF NOT = "Y"
                 MOVE "S  A000MESSX." TO VDU
                 MOVE SPACES TO VDUBUF
                 PERFORM SCREENIO
                 MOVE "S  A000MESSXX." TO VDU
                 MOVE SPACES TO VDUBUF
                 PERFORM SCREENIO
                 GO TO FR-BRANCH
              ELSE
                 MOVE "S  A000MESSX." TO VDU
                 MOVE SPACES TO VDUBUF
                 PERFORM SCREENIO
                 MOVE "S  A000MESSXX." TO VDU
                 MOVE SPACES TO VDUBUF
                 PERFORM SCREENIO.

           MOVE BR-MACHINE                 TO TO-MACHINE.
           MOVE BR-DATA-PATH               TO TO-DATA-PATH.
           MOVE 0 TO TO-DLNO.

           IF FR-MACHINE NOT = TO-MACHINE
              PERFORM VERIFY-PASSWORD
              IF PASSWDW-STAT NOT = "OK"
                 GO TO EXIT-PARM-ABORT.

       REC-ACCT.
           MOVE "ENNN060ACCT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM-ABORT.
           IF VDUSTATUS = "1U" GO TO TO-BRANCH.
           IF VDUSTATUS = "10" GO TO FR-BRANCH.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-ACCT.
           MOVE VDUNUM0 TO ACCTNO.
           PERFORM VERIFY-ACCOUNT.
           IF ERRCD = "R"
              MOVE " " TO ERRCD
              GO TO REC-ACCT.

       ACCEPT-IT.
           MOVE "E RA010ACCEPT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM-ABORT.
           IF VDUSTATUS = "10" GO TO FR-BRANCH.
           IF VDUSTATUS = "1U" GO TO REC-ACCT.
           IF VDUSTATUS NOT = "00" GO TO ACCEPT-IT.
           IF NOT (VDUBUF = "Y"  OR "N") GO TO ACCEPT-IT.
           IF VDUBUF = "N"
              GO TO FR-BRANCH.
           GO TO EXIT-PARM.

       EXIT-PARM-ABORT.
           MOVE "X" TO ERRCD.

       EXIT-PARM.
           EXIT.

      ******************************************
       VERIFY-ACCOUNT SECTION.
      ******************************************
           MOVE EXT-FILPATH-BASE       TO LN-ALL-BASE
                                          LSM-ALL-BASE.
           MOVE FR-DATA-PATH           TO LN-ALL-DATA
                                          LSM-ALL-DATA.
           MOVE FR-MACHINE             TO LN-ALL-MACHINE
                                          LSM-ALL-MACHINE.
           MOVE LN-ALL-PATH            TO LN-PATH.
           MOVE LSM-ALL-PATH           TO LSM-PATH.
           MOVE "Y"                    TO LN-PATH-OVERRIDE
                                          LSM-PATH-OVERRIDE.

           PERFORM OPEN-LN1-FILE.
           MOVE FR-BR  TO LN-OWNBR.
           MOVE ACCTNO TO LN-ACCTNO.
           PERFORM READ-LN1-FILE.
           IF IO-BAD
              PERFORM CLOSE-LN1-FILE
              MOVE "INVALID ACCOUNT" TO MESS
              PERFORM SEND-MESS
              MOVE "R" TO ERRCD
              GO TO VERIFY-ACCOUNT-EXIT.

      * DO NOT TRANSFER VOIDS
      * DO NOT TRANSFER ACCOUNTS IN RESCISSION
      * DO NOT TRANSFER ACCOUNT "PB"'D INTO A RESCISSION
      * DO NOT TRANSFER ALREADY TRANSFERRED ACCOUNTS
           IF LN-POCD-NONLOAN OR LN-POCD-PB-RESC OR LN-POCD-TRANSFERRED
              MOVE "ACCOUNT VOID OR IN RESCISSION; TRANSFER NOT ALLOWED"
                 TO MESS
              PERFORM SEND-MESS
              MOVE "R" TO ERRCD
              GO TO VERIFY-ACCOUNT-EXIT.

      * NO BANKRUPTS:
           IF LN-BNKRPTDATE NOT = ZEROS
              MOVE "ACCOUNT IS IN BANKRUPCY; TRANSFER NOT ALLOWED"
                 TO MESS
              PERFORM SEND-MESS
              MOVE "R" TO ERRCD
              GO TO VERIFY-ACCOUNT-EXIT.

      * NO P&LS
           IF (LN-PLCD NOT = " ") OR (LN-PLDATE NOT = 0)
              MOVE PL-SEQ TO PASSWDW-SEQ
              MOVE "TO TRANSFER CHARGE OFF ACCOUNT:" TO PASSWDW-PROMPT2
              PERFORM PASSWORD-MUST-EXIST-AND-FLAGGED
              IF PASSWDW-STAT NOT = "OK"
                 MOVE "CHARGE OFF TRANSFER NOT ALLOWED" TO MESS
                 PERFORM SEND-MESS
                 MOVE "R" TO ERRCD
                 GO TO VERIFY-ACCOUNT-EXIT.

      *---------------------------------------------------------
      * TEST FOR OPTION OF NO ZERO BALANCE ACCOUNTS:
           IF LN-CURBAL = 0
              MOVE "ACCOUNT HAS ZERO BALANCE; TRANSFER NOT ALLOWED"
                 TO MESS
              PERFORM SEND-MESS
              MOVE "R" TO ERRCD
              GO TO VERIFY-ACCOUNT-EXIT.

           PERFORM VERIFY-GI.
           IF VALID-GI-FG = "N"
              MOVE
              "INVALID GL INTERFACE IN TARGET BRANCH; PLEASE RE-ENTER"
                 TO MESS
              PERFORM SEND-MESS
              MOVE "R" TO ERRCD
              GO TO VERIFY-ACCOUNT-EXIT.

      * DO NOT ALLOW A TRANSFER MORE THAN ONCE WITHIN SAME MONTH
      * BUT ALLOW IT IF THE ENTERED DATE IS SAME MONTH, SO ACQUISITIONS
      * CAN GET TRANSFERRED OUT
      * PROBLEM IS WITH METRO2 REPORTING THE "L1" SEGMENT. IF TRANSFERRED
      * MORE THAN ONCE, THE "BT" DOES NOT GET REPORTED ANYMORE AND THOSE
      * ACCOUNTS WONT GET CLOSED OUT
           IF LN-BR-TFR-FG = "Y"
              MOVE LN-ENTDATE       TO NUM-DATE
              MOVE TRANS-DATE       TO SYS-DATE
      * IF LOAN ENTERED DATE =  TRANSFER DATE, ALLOW IT THROUGH
              IF NOT (S-MM = NUM-MO AND S-YY = NUM-YR)
                 MOVE LN-OWNBR  TO LSM-BRNO
                 MOVE LN-ACCTNO TO LSM-ACCTNO
                 MOVE 1         TO LSM-SEQNO
                 PERFORM READ-LSM1-FILE
                 IF IO-FG = 0
                    MOVE LSM-TRANS-FROM-DATE TO NUM-DATE
                    MOVE TRANS-DATE          TO SYS-DATE
      * TEST IF ALREADY TRANSFERRED THIS MONTH
                    IF S-MM = NUM-MO AND S-YY = NUM-YR
                       MOVE "ACCOUNT HAS ALREADY TRANSFERRED THIS MONTH"
                                                 TO MESS
                       PERFORM SEND-MESS
                       MOVE "R" TO ERRCD
                       GO TO VERIFY-ACCOUNT-EXIT.

       VERIFY-ACCOUNT-EXIT.
           EXIT.

      *******************************************************
       VERIFY-GI SECTION.
      *******************************************************
           PERFORM OPEN-GI1-FILE.
           MOVE "Y" TO VALID-GI-FG.
           MOVE TO-BR TO GI-BRANCH.
           MOVE LN-CLASS TO GI-CLASS.
           PERFORM READ-GI1-FILE.
           IF IO-BAD
              MOVE "N" TO VALID-GI-FG.
           PERFORM CLOSE-GI1-FILE.

      *******************************************************
      * PASSWORD MUST EXIST SECTION
      *   LIKE VERIFY PASSWORD, BUT WILL GIVE BAD STATUS IF
      *   PASSWORD NOT IN FILE AND THE ENTER FLAG NOT 'Y'
      *   WILL ASK FOR PASSWORD & ACT JUST LIKE VERIFY
      *******************************************************
       PASSWORD-MUST-EXIST-AND-FLAGGED SECTION.
           MOVE "EN" TO PASSWDW-STAT.
           MOVE "BULKTR" TO PASSWDW-PGM.
           MOVE "PLEASE ENTER THE PASSWORD," TO PASSWDW-PROMPT1.
           MOVE "CL/PASSWD" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
           CANCEL FORM-PROGX.

      ***************************************************
       VERIFY-PASSWORD SECTION.
      ***************************************************
           MOVE " " TO PASSWDW-STAT.
           MOVE "BULKTR" TO PASSWDW-PGM.
           MOVE "PLEASE ENTER THE PASSWORD," TO PASSWDW-PROMPT1.
           MOVE "CL/PASSWD" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
           CANCEL FORM-PROGX.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".

      *******************************************************
       FORM-RESET SECTION.
      *******************************************************
           MOVE EXT-CONAME-CONAME TO BLKONE-CONAME.
           MOVE EXT-CONAME-SYSDATE TO BLKONE-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY BLKONE-SCREEN UPON BLKONE-WINDOW-HANDLE.
           MOVE BLKONE-FORM-FIELDS TO WR-BUF.
           MOVE BLKONE-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *******************************************************
       SETUP-LINK-INFO SECTION.
      *******************************************************
           MOVE BLKONE-HELP--FILE TO HELP-LINK-FILE.
           MOVE BLKONE-HELP--DIR  TO HELP-LINK-DIR.

      *******************************************************
       VDU-CONVERT-FIELD SECTION.
      *******************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/BLKONE_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *******************************************************
       MISC-ROUTINES SECTION.
      *******************************************************
       COPY "LIBGB/ACCESS.CPY".

       SEND-LEGEND.
           MOVE MESS TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       GET-BR.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.

      ****************************************
       IO-ROUTINE SECTION.
      ****************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-LSM1-FILE.
           PERFORM CLOSE-GI1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLSMGS_SQL.CPY".
       COPY "LIBGL/GLGIGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBLP/LPLSM1RN.CPY".
       COPY "LIBGL/GLGI1RN.CPY".

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      ****************************************
       END-PROGRAM SECTION.
      ****************************************
       END-RUN.
           PERFORM CLOSE-FILES.
           IF NO-RECORDS-FOUND
              MOVE "NO RECORDS FOUND FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS.
       EXIT-PROG.
           MOVE "SP/SPMENU" TO FORM-NAM
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY BLKONE-SCREEN.
           DESTROY BLKONE-WINDOW-HANDLE.
           EXIT PROGRAM.
