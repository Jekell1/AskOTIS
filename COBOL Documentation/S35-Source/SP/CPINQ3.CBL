       IDENTIFICATION DIVISION.
       PROGRAM-ID.      CPINQ3.
       DATE-WRITTEN.    060184.
      ******************************************************************
      *   DIR : SP
      *   DESC: VARIABLE INQUIRY DRIVER FROM F10 INQUIRY
      *
      *   CALLED: FROM CPINQ1.C AND OPINQ.C
      *
      * REV:
      *  JTG 012589 CHANGED FD-COLLFMTYPE TO FMTYPE
      *             CHANGED MEANING FROM Y TO C
      *  JTG 110789 REMOVED FORM DESCRIPTOR INFO EXCEPT DESC
      *  JTG 121489 ADDED DYNAMIC CREATE OF NOTICE RECORDS
      *  JTG 010490 STOPPED DISPLAY OF FORMS IF NOT 'MC'
      *             CHANGED DISPLAY TO SEQUENCE DOWN
      *  JTG 060992 ADDED SET OF CRNORC-BR-PDTH-NXDUE
      *  JTG 062695 ADDED SET OF CRNORC-BR-NOT-OURS-REFUND
      *  MJD 112597 EXPANDED FD-REC TO 2828 BYTES (ADDED 30)
      *  BAH 000210 ADDED LT-REC AND LS-REC (LIKE LONPS3)
      *  BAH 001016 ADDED FILE 16 (LN) LOAN APPENDAGE, REGENCY NEEDED REVOLVING
      *             FIELDS AND FILE 1 (LN) IS OUT OF ROOM , PR# 1324
      *
      * KEC 2016.0121 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         FIXED KEYS & RECS FOR FD-REC.
      *         ADDED NEW FDB-FILE; WAS IN FD-FILE.
      *         REMOVED FT-FILE; MERGED INTO FD-FILE.
      *
      *  BAH 160203 SPLIT OUT LTFILE, PD0003
      *  BAH 160209 SPLIT OUT LSFILE, PD0003
      *
      * KEC 2016.0216 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         FIXED KEYS & RECS FOR FD-REC.
      *         CHANGED FMNO/FORMNO/NEXTFM/SETFM FROM 3 TO 4 BYTES.
      *
      *  BAH 170824 ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 181004 GLOBAL BWFILE
      * BAH 181011 GLOBAL DLFILE
      * BAH 181212 GLOBAL LNFILE
      * BAH 190103 GLOBAL LPFILE
      * BAH 190109 GLOBAL NOFILE
      * BAH 190314 MOVED EDIT-FM-INITKEY TO LPWSFM-EDIT-FM-INITKEY 
      ******************************************************************
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
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)SP/CPINQ3.CBL S35 02/22/24-07:34:59 >".
       COPY "LIBGB/GB01GB.CPY".
       COPY "LIBGB/GB01GB_SQL.CPY".
       COPY "LIBGB/GBWSGB.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01DL.CPY".
       COPY "LIBLP/LP01DL_SQL.CPY".
       COPY "LIBLP/LPWSDL.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01LP.CPY".
       COPY "LIBLP/LP01LP_SQL.CPY".
       COPY "LIBLP/LPWSLP.CPY".
       COPY "LIBLP/LP01LTA.CPY".
       COPY "LIBLP/LP01LTA_SQL.CPY".
       COPY "LIBLP/LPWSLTA.CPY".
       COPY "LIBLP/LP01LTB.CPY".
       COPY "LIBLP/LP01LTB_SQL.CPY".
       COPY "LIBLP/LPWSLTB.CPY".
       COPY "LIBLP/LP01LTC.CPY".
       COPY "LIBLP/LP01LTC_SQL.CPY".
       COPY "LIBLP/LPWSLTC.CPY".
       COPY "LIBLP/LP01LTL.CPY".
       COPY "LIBLP/LP01LTL_SQL.CPY".
       COPY "LIBLP/LPWSLTL.CPY".
       COPY "LIBLP/LP01LSX.CPY".
       COPY "LIBLP/LP01LSX_SQL.CPY".
       COPY "LIBLP/LPWSLSX.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01FD.CPY".
       COPY "LIBLP/LP01FD_SQL.CPY".
       COPY "LIBLP/LPWSFD.CPY".
       COPY "LIBLP/LP01FDB.CPY".
       COPY "LIBLP/LP01FDB_SQL.CPY".
       COPY "LIBLP/LPWSFDB.CPY".
       COPY "LIBLP/LP01NO.CPY".
       COPY "LIBLP/LP01NO_SQL.CPY".
       COPY "LIBLP/LPWSNO.CPY".
      * NEEDED FOR LPWSFM-EDIT-FM-INITKEY
       COPY "LIBLP/LPWSFM.CPY".

       COPY "LIBLP/LTAUTOW.CPY".
       COPY "LIBLP/LP01WK.CPY".
      * IF THE LT BANKRUPT RECORD CHANGES, THIS NEEDS CHANGED TOO
       01  WK-BANKR-RECORD.
           03  WK-B-FILEDATE         PIC 9(8)  COMP-3.
           03  WK-B-CASE-NO          PIC X(9).
           03  WK-B-CHAPTER          PIC 99.
           03  WK-B-CHAPTER-X       REDEFINES WK-B-CHAPTER
                                         PIC XX.
           03  WK-B-MISC1            PIC X(30).
           03  WK-B-MISC2            PIC X(30).
           03  WK-B-MISC3            PIC X(30).
           03  WK-B-BDATE            PIC 9(8)    COMP-3.
           03  WK-B-WHO-FILED        PIC X.
           03  WK-B-ENDORSER-FILED   PIC X.
           03  WK-B-CLAIM-AMT        PIC S9(7)V99    COMP-3.
           03  WK-B-ACTIVE-INACTIVE  PIC X.
           03  WK-B-INT-LC-DUE       PIC S9(7)V99    COMP-3.
           03  WK-B-OTH-DUE          PIC S9(7)V99    COMP-3.
           03  WK-B-TDUEAMT          PIC S9(7)V99    COMP-3.

      * DEBTORS ATTORNEY
           03  WK-B-NAME             PIC X(24).
           03  WK-B-ADR1             PIC X(24).
           03  WK-B-ADR2             PIC X(24).
           03  WK-B-CITY             PIC X(15).
           03  WK-B-STATE            PIC XX.
           03  WK-B-ZIP              PIC X(10).
           03  WK-B-PHONE            PIC 9(10)    COMP-3.
           03  FILLER                PIC X(20).

      * CLERK OF BANKRUPTCY COURT
           03  WK-B-COURT-NAME       PIC X(24).
           03  WK-B-COURT-ADR1       PIC X(24).
           03  WK-B-COURT-ADR2       PIC X(24).
           03  WK-B-COURT-CITY       PIC X(15).
           03  WK-B-COURT-STATE      PIC XX.
           03  WK-B-COURT-ZIP        PIC X(10).
           03  WK-B-COURT-PHONE      PIC 9(10)    COMP-3.
           03  FILLER                PIC X(20).

      * TRUSTEE
           03  WK-B-TRUST-NAME       PIC X(24).
           03  WK-B-TRUST-ADR1       PIC X(24).
           03  WK-B-TRUST-ADR2       PIC X(24).
           03  WK-B-TRUST-CITY       PIC X(15).
           03  WK-B-TRUST-STATE      PIC XX.
           03  WK-B-TRUST-ZIP        PIC X(10).
           03  WK-B-TRUST-PHONE      PIC 9(10)    COMP-3.
           03  FILLER                PIC X(20).

      *=================================================================
      *   F-B-REC FROM 'REPLACING LEADING' ON FD-REC.
      *=================================================================
       COPY "LIBLP/LP01FD.CPY" REPLACING LEADING "FD" BY "F-B".

      **********************
      *   MISC WORKERS     *
      **********************
       01  F1-FG                PIC X.
       01  FM-INITKEY           PIC X(20).
       01  LN-NO                PIC 9(6) COMP.
       01  ROW-NO               PIC 99   COMP.
       01  FORM-CONT            PIC X VALUE "N".
       01  FORMS-EXIST          PIC X VALUE "N".
       01  MENU-ALL-FORMS       PIC X VALUE "N".

       01  ROW                  PIC 9(6) COMP.
       01  D-B-SUB              PIC 9(6) COMP.
       01  FIELD-SUB            PIC 9(6) COMP.
       01  FIRST-FIELD          PIC 9(2) COMP.
       01  FILE-CODE            PIC 9(2) COMP.
       01  DATA-ERR-FLAG        PIC 9    COMP VALUE 0.
       01  RECORD-EXISTS        PIC X.

       01  LAST-BR-KEY          PIC 9(4)  COMP-3 VALUE 0.
       01  LAST-FDB1-KEY        PIC 9(2)  VALUE ZEROES.

      ************************************************
      * MADE ALL THE "LAST" KEYS MATCH THEIR TRUE KEY SIZE
      * THESE ARE ONLY USED TO TEST IF THE RECORD NEEDS TO BE READ
      * IN AGAIN DOWN IN THE MAIN LOGIC.
      *
      * THESE HAVE NOTHING TO DO WITH FM-INITKEY, WHICH WILL NOT HAVE
      * THE BRANCH IN IT AND USED TO GET MOVED TO THE KEYS THAT
      * WERE JUST ACCTNO, CAN NOT DO THAT ANYMORE.
      *
      * **** DO NOT CHANGE WHETHER NUMERIC OR ALPHA, THAT IS BURIED
      * *** FURTHER DOWN IN THE CODE
      ************************************************
       01  LAST-LN1-KEY         PIC 9(12) COMP VALUE 0.
       01  LAST-BW1-KEY         PIC 9(13) COMP VALUE 0.
       01  LAST-LP1-KEY         PIC X(17) VALUE SPACES.
       01  LAST-DL1-KEY         PIC 9(10) COMP VALUE 0.
       01  LAST-NO1-KEY         PIC 9(12) COMP VALUE 0.
       01  LAST-LT-COLL-KEY     PIC 9(8)  COMP-3 VALUE 0.
       01  LAST-LT-AUTO-KEY     PIC 9(8)  COMP-3 VALUE 0.
       01  LAST-LT-BANKR-KEY    PIC 9(8)  COMP-3 VALUE 0.
       01  LAST-LSX1-KEY        PIC 9(14)  COMP-3 VALUE 0.
       01  KEY-BR-WK.
           03  INIT-BR          PIC 9(4).

       01  SFT-CT               PIC 9(6) COMP.
       01  SFT-SUB              PIC 9(6) COMP.
       01  SFT-SUB1             PIC 9(6) COMP.

       01  SAVE-SYS-DATE        PIC 9(8)  COMP  VALUE 0.
       01  NEXT-FMNO            PIC 9(4)  COMP.

      ***********************************
      *     S C R E E N   F I E L D S   *
      ***********************************
       01  LINE-LIST.
           03  FILLER          PIC X(32) VALUE
           "L01 L02 L03 L04 L05 L06 L07 L08 ".
           03  FILLER          PIC X(28) VALUE
           "L09 L10 L11 L12 L13 L14 L15.".
       01  LINE-LIST-TAB REDEFINES LINE-LIST.
           03  LINE-LIST-P     OCCURS 60 PIC X.

       01  LINE-BUF.
           03  D-LN            OCCURS 15.
               05  D-LN-LINE   OCCURS 2.
                   07  D-FMNO      PIC 9(4).
                   07  FILLER      PIC X(2).
                   07  D-DESC      PIC X(30).
                   07  FILLER      PIC X(4).

       01  HEAD-1.
           03  FILLER          PIC X(40) VALUE
           "FORM  DESCRIPTION-------------------".
           03  FILLER          PIC X(40) VALUE
           "FORM  DESCRIPTION-------------------".

       01  HEAD-2              PIC X(80) VALUE ALL "-".


      ******************************************************************
       COPY "LIBLP/LPLOANW.CPY".
       COPY "LIBLP/LPEXTW.CPY".
       COPY "LIBLP/LPEDTW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBCL/CRNOR2W.CPY".
       COPY "LIBLP/LPPOFFW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/CPINQ_DEF.CPY".
       COPY "LIBSP/CPINQ_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       01  FORM-LINK-AREA.
           03  CPINQ-PROG-BUF      PIC X(356).
           03  SOC-BUF             PIC X(11)    VALUE SPACES.
           03  INTERRUPT-FG        PIC X.
           03  AVAILABLE-BUF       PIC X(12).
           03  ARCHIVE-FG          PIC X.
           03  FLEX-INQ-BUF        PIC 9(3).
           03  FLEX-INQ-LINES      PIC 9(2).
           03  CPINQ-WINDOW-HANDLE PIC X(10).
           03  DATA-PATH-BUF.                   
               05  NEW-DATA-PATH               PIC X(21).
               05  OLD-DATA-PATH               PIC X(21).

       SCREEN SECTION.
       COPY "LIBSP/CPINQ_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME FORM-LINK-AREA EXIT-PATHNAME.

      **********************************
      *   C O N T R O L   M O D U L E  *
      **********************************
       MAIN-MODULE SECTION.
           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "CPINQ" TO VDU-FORM-NAME.
           MOVE " " TO VDU-WIMENU-SELECTION.

           MOVE CPINQ-PROG-BUF TO PROG-BUF.
           PERFORM INITIALIZATION.
           IF ERRCD NOT = " "
              GO TO END-INQ.
       NEXT-FM.
           PERFORM SELECT-FORM.
           IF IO-FG = 9
              GO TO END-INQ.

           PERFORM INITIAL-FILE.
           IF IO-FG = 9
              GO TO NEXT-FM.

      * PROVISION FOR UNLIMITED FORM LENGTH
       NEXT-LINKED-FORM.
           PERFORM PRINT-FORM.
           IF NEXT-FMNO = 0
              GO TO NEXT-FM.

           MOVE NEXT-FMNO TO FD-FMNO

           PERFORM GET-FD.

           GO TO NEXT-LINKED-FORM.
       END-INQ.
      * THE FOLLOWING LOGIC WAS ADDED TO DIRECT CPINQ AND LONPS1.

           MOVE "1" TO ERRCD.

           MOVE 1 TO NEW-PROG-SUB.

           GO TO END-ROUTINE.

      ***********************************
       INITIALIZATION SECTION.
      ***********************************
           IF FLEX-INQ-LINES < 1 OR FLEX-INQ-LINES > 15
              MOVE "ERR: # INQ LNS" TO MESS
              PERFORM SEND-MESS
              GO TO IO-ERROR.

      * CAP VARIABLE LINE LIST
           MULTIPLY FLEX-INQ-LINES BY 4 GIVING LN-NO.
           MOVE "." TO LINE-LIST-P(LN-NO).

           PERFORM OPEN-FD1-FILE.
           PERFORM OPEN-FDB1-FILE.
           PERFORM OPEN-LN1-FILE.
           PERFORM OPEN-BW1-FILE.
           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-NO1-FILE.
           MOVE ZERO TO F-B-FMNO.

           PERFORM OPEN-GB-FILE.
           MOVE GB-PATH-OWNBR TO GB-BRNO.
           MOVE 1             TO GB-NO.
           PERFORM READ-GB-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.
           PERFORM CLOSE-GB-FILE.

           IF BRANCH NOT = 0
              MOVE BRANCH TO BR-NO
           ELSE
              MOVE 1 TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.
           MOVE BR-NO TO LAST-BR-KEY.

           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO SAVE-SYS-DATE.
           MOVE " " TO ERRCD.

      * ITS OK IF THE BRANCH IS NOT IN THIS...
           MOVE LOAN-KEY TO LN-ACCTNO LAST-LN1-KEY.
           PERFORM READ-LN1-FILE.
           IF IO-FG = 9
              MOVE "LOAN# MUST BE ENTERED PRIOR TO INQUIRY" TO MESS
              PERFORM SEND-MESS
              MOVE "P" TO ERRCD.
           IF BORROWER-KEY = 0
              MOVE "BORROWER# MUST BE ENTERED PRIOR TO INQUIRY" TO MESS
              PERFORM SEND-MESS
              MOVE "P" TO ERRCD.
       EXIT-INITIALIZE.
           EXIT.

      ****************************
       SELECT-FORM SECTION.
      ****************************
           MOVE "S  A000HEAD:S." TO VDU.
           MOVE HEAD-1 TO VDUBUF.
           PERFORM SCREENIO.
       SELECT-FM.
      * FORM CHOICE PASSED FROM PRIOR PROGRAM:
           IF FLEX-INQ-BUF NOT = 0
              MOVE FLEX-INQ-BUF TO FD-FMNO
              GO TO VERIFY-FORM-CHOICE.

           PERFORM CLOSE-FD1-FILE.
           PERFORM OPEN-FD1-FILE.
           IF MENU-ALL-FORMS = "Y"
              MOVE 0 TO FD-FMNO
           ELSE
      * INQUIRIES BEGIN AT 500:
           MOVE 500 TO FD-FMNO.
           PERFORM START-FD1-FILE.
       NEXT-INQ-LINES-FD.
           MOVE " " TO LINE-BUF.
           MOVE 1 TO LN-NO ROW-NO.
       NEXT-FD-CHOICE.
           PERFORM READ-FD1-FILE-NEXT.
           IF IO-FG = 9
              GO TO DISPLAY-CHOICE.
           IF FD-PROG NOT = "MC"
              GO TO NEXT-FD-CHOICE.
           MOVE "Y" TO FORMS-EXIST.
           MOVE FD-FMNO TO D-FMNO(LN-NO ROW-NO).
           MOVE FD-DESC TO D-DESC(LN-NO ROW-NO).
           IF LN-NO = FLEX-INQ-LINES
              IF ROW-NO = 1
                 MOVE 1 TO LN-NO
                 MOVE 2 TO ROW-NO
                 GO TO NEXT-FD-CHOICE
              ELSE
                 GO TO DISPLAY-CHOICE.
           ADD 1 TO LN-NO.
           GO TO NEXT-FD-CHOICE.

       DISPLAY-CHOICE.
           IF LN-NO = 0
              IF IO-FG = 9
                 IF FORMS-EXIST = "Y"
                    GO TO SELECT-FM
                 ELSE
                    MOVE "NO INQUIRY FORMS LOCATED" TO MESS
                    PERFORM SEND-MESS
                    MOVE 9 TO IO-FG
                    GO TO EXIT-SELECT-FORM.
           PERFORM DISPLAY-ALL-LINES.
           MOVE "F1-EXIT, F3-MENU_ALL, <ENTER>-SCAN, FORM#" TO MESS.
           PERFORM SEND-LEGEND.
       ENTER-ACTION.
           MOVE "EN N030MEMODATE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE 9 TO IO-FG
              GO TO EXIT-SELECT-FORM.
           IF VDUSTATUS = "16"
              MOVE "Y" TO MENU-ALL-FORMS
              GO TO SELECT-FM.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-ACTION.
           IF VDUNUM0 = 0
              IF IO-FG = 0
                 GO TO NEXT-INQ-LINES-FD
              ELSE
                 GO TO SELECT-FM.
           MOVE VDUNUM0 TO FD-FMNO.
       VERIFY-FORM-CHOICE.
           PERFORM READ-FD1-FILE.
           IF IO-FG = 9
              MOVE "INVALID FORM#" TO MESS
              PERFORM SEND-MESS
              IF FLEX-INQ-BUF = 0
                 GO TO ENTER-ACTION
              ELSE
                 MOVE 0 TO FLEX-INQ-BUF
                 GO TO SELECT-FM.
           MOVE 0 TO FLEX-INQ-BUF.

           PERFORM GET-FD.
           IF FD-1STFILE = 1 OR 16
              MOVE LN-ACCTNO TO FM-INITKEY.
           IF FD-1STFILE = 2
              MOVE BORROWER-KEY TO BW-SSNO FM-INITKEY.
           IF FD-1STFILE = 4
              MOVE SPACES   TO LPWSFM-EDIT-FM-INITKEY
              MOVE LOAN-KEY TO NO-ACCTNO LPWSFM-EDIT-FM-ACCTNO
              MOVE FD-FMNO  TO           LPWSFM-EDIT-FM-FORMNO
              MOVE LPWSFM-EDIT-FM-INITKEY TO FM-INITKEY.
           IF FD-1STFILE = 5
              GO TO CANT-RUN.
           IF FD-1STFILE = 6
              MOVE LN-DLNO TO DL-DLNO
              MOVE DL-NUM TO FM-INITKEY.
           GO TO EXIT-SELECT-FORM.
       CANT-RUN.
           MOVE "INQUIRY CANNOT DISPLAY THIS FORM TYPE" TO MESS.
           PERFORM SEND-MESS.
           GO TO ENTER-ACTION.
       EXIT-SELECT-FORM.
           EXIT.

      ***********************************************
       INITIAL-FILE SECTION.
      ***********************************************
           GO TO INIT-LN1-FILE INIT-BW1-FILE
                 INIT-BR-FILE INIT-NO1-FILE
      * 5 LPFILE - NEVER A FIRST FILE  6 DLFILE - NEVER A FIRST FILE
                 INIT-BOGUS-FILE INIT-BOGUS-FILE
                 INIT-07-FILE INIT-08-FILE
                 INIT-GB-FILE INIT-LSX-FILE
      * LA, BA, MA
                 INIT-BOGUS-FILE INIT-BOGUS-FILE INIT-BOGUS-FILE
                 INIT-ML1-FILE  INIT-BANKR-FILE INIT-LN1-FILE
                 DEPENDING F-B-1STFILE.
           MOVE "FORM DRIVER WILL NOT RUN THIS FORM" TO MESS.
           PERFORM SEND-MESS.
           MOVE "FORM'S 1STFILE NOT DEFINED FOR THIS INQUIRY" TO MESS.
           PERFORM SEND-MESS.
           MOVE 9 TO IO-FG.
           GO TO EXIT-INITIAL-FILE.
       INIT-BOGUS-FILE.
           MOVE "FILE INACTIVE" TO MESS
           PERFORM SEND-MESS.
           MOVE 9 TO IO-FG.
           GO TO EXIT-INITIAL-FILE.
       INIT-LN1-FILE.
           MOVE FM-INITKEY            TO LPWSFM-EDIT-FM-INITKEY.
           MOVE LPWSFM-EDIT-FM-ACCTNO TO LN-ACCTNO.
           PERFORM READ-LN1-FILE.
           IF IO-FG = 9
              GO TO INIT-KEY-MISSING.
      *    UNLOCK LN-FILE.
           MOVE LN1-KEY TO LAST-LN1-KEY.
           GO TO EXIT-INITIAL-FILE.
       INIT-BW1-FILE.
           MOVE FM-INITKEY          TO LPWSFM-EDIT-FM-INITKEY.
           MOVE LPWSFM-EDIT-FM-SSNO TO BW-SSNO.
           PERFORM READ-BW1-FILE.
           IF IO-FG = 9
              GO TO INIT-KEY-MISSING.
           MOVE BW1-KEY TO LAST-BW1-KEY.
           GO TO EXIT-INITIAL-FILE.
       INIT-BR-FILE.
           MOVE FM-INITKEY TO KEY-BR-WK.
           IF INIT-BR NOT NUMERIC
              MOVE 9999 TO INIT-BR.
           MOVE INIT-BR TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-FG = 9
              GO TO INIT-KEY-MISSING.
           MOVE BR-NO TO LAST-BR-KEY.
           GO TO EXIT-INITIAL-FILE.
       INIT-NO1-FILE.
           MOVE FM-INITKEY            TO LPWSFM-EDIT-FM-INITKEY.
           MOVE LPWSFM-EDIT-FM-ACCTNO TO NO-ACCTNO.
           PERFORM READ-NO1-FILE.
           IF IO-FG = 9
              PERFORM BUILD-NO-REC
              MOVE 0 TO IO-FG.
      *    UNLOCK NO-FILE.
           MOVE NO1-KEY TO LAST-NO1-KEY.
           GO TO EXIT-INITIAL-FILE.
       INIT-07-FILE.
           MOVE "SEVENTH FILE INACTIVE" TO MESS
           PERFORM SEND-MESS.
           MOVE 9 TO IO-FG.
           GO TO EXIT-INITIAL-FILE.
       INIT-08-FILE.
           MOVE "EIGHTH FILE INACTIVE" TO MESS
           PERFORM SEND-MESS.
           MOVE 9 TO IO-FG.
           GO TO EXIT-INITIAL-FILE.
       INIT-GB-FILE.
      *    THIS FILE IS READ ONCE PER PROGRAM;
      *    VALUE, IF SUPPLIED, IS IGNORED (ALWAYS = 1)
           GO TO EXIT-INITIAL-FILE.
       INIT-LSX-FILE.
           MOVE "NINETH FILE INACTIVE" TO MESS
           PERFORM SEND-MESS.
           MOVE 9 TO IO-FG.
           GO TO EXIT-INITIAL-FILE.
       INIT-ML1-FILE.
           MOVE "MAIL LIST FILE INACTIVE" TO MESS.
           PERFORM SEND-MESS.
           MOVE 9 TO IO-FG.
           GO TO EXIT-INITIAL-FILE.
       INIT-BANKR-FILE.
           MOVE "BANKRUPT FILE INACTIVE" TO MESS.
           PERFORM SEND-MESS.
           MOVE 9 TO IO-FG.
           GO TO EXIT-INITIAL-FILE.
       INIT-KEY-MISSING.
           MOVE "RECORD NOT LOCATED FOR FORM TYPE" TO MESS.
           PERFORM SEND-MESS.
           MOVE 9 TO IO-FG.
       EXIT-INITIAL-FILE.
           EXIT.

      ****************************
       PRINT-FORM SECTION.
      ****************************

           MOVE "S  A000HEAD:S." TO VDU.
           MOVE HEAD-2 TO VDUBUF.
           PERFORM SCREENIO.
           MOVE 1 TO ROW.
           MOVE FIRST-FIELD TO FIELD-SUB.
      *    CONTINUED FORM - SEE NOTE BELOW
           IF FORM-CONT = "Y"
              MOVE "N" TO FORM-CONT
              GO TO PRINT-BODY.
           MOVE SPACE TO LINE-BUF.
           MOVE 0 TO LN-NO.
       PRINT-BODY.
           MOVE F-B-PROMPTS(ROW) TO EDT-LINE.
       NEXT-FIELD-SET.
           IF FIELD-SUB = 51
              GO TO PRINT-EDITED-LINE.
           IF F-B-FIELDROW(FIELD-SUB) NOT = ROW
              GO TO PRINT-EDITED-LINE.
           PERFORM READ-DATA-FILE.
           IF DATA-ERR-FLAG = 1
              MOVE "Y" TO EDT-CLEAR-FG
              GO TO PRINT-EDIT-FIELD.
           IF DATA-ERR-FLAG = 9
              MOVE E-FILE TO E-FILEX
              MOVE FILE-STAT TO E-CODE
              MOVE ERROR-MSG-HEAD TO EDT-LINE
              GO TO PRINT-EDITED-LINE.
           PERFORM EXTRACT-DATA.
       PRINT-EDIT-FIELD.
           MOVE F-B-FIELDCOL(FIELD-SUB) TO EDT-COL.
           MOVE F-B-FIELDED(FIELD-SUB) TO EDT-EDIT.
           PERFORM EDIT-DATA.
           ADD 1 TO FIELD-SUB.
           GO TO NEXT-FIELD-SET.
       PRINT-EDITED-LINE.
           PERFORM ALPHA-SHIFT.
           PERFORM WRITE-FORM-LINE.
           IF F1-FG = "Y"
              MOVE SPACES TO F1-FG
              GO TO EXIT-PRINT-FORM.
           IF ROW < F-B-BODY
              IF ROW < 30
                 ADD 1 TO ROW
                 GO TO PRINT-BODY.
      *    CONTINUED FORM - DO NOT RESTART DISPLAY
           IF NEXT-FMNO NOT = 0
              MOVE "Y" TO FORM-CONT
              GO TO EXIT-PRINT-FORM.
           IF LN-NO NOT = 0
              MOVE SPACE TO EDT-LINE
              PERFORM WRITE-FORM-LINE UNTIL LN-NO = 0
              IF F1-FG = "Y"
                 MOVE SPACES TO F1-FG.
       EXIT-PRINT-FORM.
           EXIT.

      /******************************************
      *      R E A D   D A T A   F I L E        *
      *   ALL DATABASE ERRORS ARE FATAL (THEY   *
      *   PREVENT ANY DOCUMENT FROM PRINTING)   *
      *   AND ARE THERFORE ROUTED TO IO-ERROR.  *
      *   0 OR SPACE KEYS SET DATA-ERR-FLAG TO  *
      *   1 AND RESULT IN SPACES FOR FIELD      *
      *   READ-ERROR SETS DATA-ERR-FLAG TO 9    *
      *   AND RESULTS IN THE FILE-ERRORS BANNER *
      *   REPLACING THAT LINE. ALL NON-ZERO     *
      *   DATA-ERR-FLAGS ARE NON-FATAL          *
      *******************************************

      **************************************************
       READ-DATA-FILE SECTION.
      **************************************************
           MOVE 0 TO DATA-ERR-FLAG.
           MOVE F-B-FIELDKEY(FIELD-SUB) TO DATA-FIELD.
           IF DATA-FIELD = 0
              GO TO GET-FIELD-SPECS.
           COMPUTE FILE-CODE = (DATA-FIELD - 1) / 100 + 1.
           PERFORM GET-DATABASE.
           IF IO-FG = 9
              MOVE "KEY FIELD NOT ESTABLISHED" TO MESS
              PERFORM SEND-MESS
              GO TO IO-ERROR.
           PERFORM SET-EXT-WK.
           GO TO KEY-LN1-REC KEY-BW1-REC
                 KEY-BR-REC KEY-NO1-REC
                 KEY-LP1-REC KEY-DL1-REC
                 KEY-F7-REC KEY-F8-REC
                 KEY-GB-REC KEY-LS1-REC
                 KEY-BOGUS-REC KEY-BOGUS-REC KEY-BOGUS-REC
                 KEY-ML1-REC KEY-BANKR-REC KEY-LN1-REC
                 DEPENDING FILE-CODE.
           MOVE "KEY CODE UNDEFINED" TO MESS.
           PERFORM SEND-MESS.
           MOVE "E" TO ERRCD.
           GO TO END-ROUTINE.
       KEY-BOGUS-REC.
           MOVE SPACES TO DATA-AREA.
           GO TO KEY-EXTRACT.
       KEY-LN1-REC.
           MOVE LN-REC TO DATA-AREA.
           GO TO KEY-EXTRACT.
       KEY-BW1-REC.
           MOVE BW-REC TO DATA-AREA.
           GO TO KEY-EXTRACT.
       KEY-BR-REC.
           MOVE BR-REC TO DATA-AREA.
           GO TO KEY-EXTRACT.
       KEY-NO1-REC.
           MOVE NO-REC TO DATA-AREA.
           GO TO KEY-EXTRACT.
       KEY-LP1-REC.
           MOVE LP-REC TO DATA-AREA.
           GO TO KEY-EXTRACT.
       KEY-DL1-REC.
           MOVE DL-REC TO DATA-AREA.
           GO TO KEY-EXTRACT.
       KEY-F7-REC.
           MOVE WK-LT-RECORD TO DATA-AREA.
           GO TO KEY-EXTRACT.
       KEY-F8-REC.
           MOVE WK-AUTO-INS TO DATA-AREA.
           GO TO KEY-EXTRACT.
       KEY-GB-REC.
           MOVE GB-REC TO DATA-AREA.
           GO TO KEY-EXTRACT.
       KEY-LS1-REC.
           MOVE LSX-REC TO DATA-AREA.
           GO TO KEY-EXTRACT.
       KEY-ML1-REC.
           GO TO KEY-EXTRACT.
       KEY-BANKR-REC.
           MOVE WK-BANKR-RECORD TO DATA-AREA.
           GO TO KEY-EXTRACT.
       KEY-EXTRACT.
           PERFORM EXTRACT-DATA.

      ********************************
      *   FIELD SPECIFICATIONS       *
      ********************************
       GET-FIELD-SPECS.
           MOVE F-B-FIELDNO(FIELD-SUB) TO DATA-FIELD.
           COMPUTE FILE-CODE = (DATA-FIELD - 1) / 100 + 1.
           PERFORM GET-DATABASE.
           IF IO-FG = 9
              MOVE "DATA FIELD NOT ESTABLISHED" TO MESS
              PERFORM SEND-MESS
              GO TO IO-ERROR.

           PERFORM SET-EXT-WK.
           IF F-B-FIELDKEY(FIELD-SUB) = 0
              GO TO LN1-NO-KEY BW1-NO-KEY
                    BR-NO-KEY NO1-NO-KEY
                    LP1-NO-KEY DL1-NO-KEY
                    F7-NO-KEY F8-NO-KEY
                    GB-NO-KEY LS1-NO-KEY
                    BOGUS-NO-KEY BOGUS-NO-KEY BOGUS-NO-KEY
                    ML1-NO-KEY BANKR-NO-KEY LN1-NO-KEY
                 DEPENDING FILE-CODE.

           GO TO GET-LN1-FILE GET-BW1-FILE
                 GET-BR-FILE GET-NO1-FILE
                 GET-LP1-FILE GET-DL1-FILE
                 GET-F7-FILE GET-F8-FILE
                 GET-GB-FILE GET-LS1-FILE
                 GET-BOGUS-FILE GET-BOGUS-FILE GET-BOGUS-FILE
                 GET-ML1-FILE GET-BANKR-FILE GET-LN1-FILE
                 DEPENDING FILE-CODE.

           MOVE "FILE CODE UNDEFINED" TO MESS.
           PERFORM SEND-MESS.
           MOVE "E" TO ERRCD.
           GO TO END-ROUTINE.
       GET-BOGUS-FILE.
           MOVE SPACES TO DATA-AREA.
           GO TO EXIT-READ-DATA-FILE.
       GET-LN1-FILE.
           IF DATA-NUMOUT = 0
              MOVE 1 TO DATA-ERR-FLAG
              GO TO EXIT-READ-DATA-FILE.
           IF DATA-NUMOUT = LAST-LN1-KEY
              MOVE LN-REC TO DATA-AREA
              GO TO EXIT-READ-DATA-FILE.
           MOVE DATA-NUMOUT TO LN-ACCTNO LAST-LN1-KEY.
           PERFORM READ-LN1-FILE.
           IF IO-FG = 9
              MOVE 0 TO LAST-LN1-KEY
              MOVE 9 TO DATA-ERR-FLAG
              GO TO EXIT-READ-DATA-FILE.
           MOVE LN-REC TO DATA-AREA.
           GO TO EXIT-READ-DATA-FILE.
       GET-BW1-FILE.
           IF DATA-NUMOUT = 0
              MOVE 1 TO DATA-ERR-FLAG
              GO TO EXIT-READ-DATA-FILE.
           IF DATA-NUMOUT = LAST-BW1-KEY
              MOVE BW-REC TO DATA-AREA
              GO TO EXIT-READ-DATA-FILE.
           MOVE DATA-NUMOUT TO BW-SSNO LAST-BW1-KEY.
           PERFORM READ-BW1-FILE.
           IF IO-FG = 9
              MOVE 0 TO LAST-BW1-KEY
              MOVE 9 TO DATA-ERR-FLAG
              GO TO EXIT-READ-DATA-FILE.
           MOVE BW-REC TO DATA-AREA.
           GO TO EXIT-READ-DATA-FILE.
       GET-BR-FILE.
           IF DATA-NUMOUT = 0
              MOVE 1 TO DATA-ERR-FLAG
              GO TO EXIT-READ-DATA-FILE.
           IF DATA-NUMOUT = LAST-BR-KEY
              MOVE BR-REC TO DATA-AREA
              GO TO EXIT-READ-DATA-FILE.
           MOVE DATA-NUMOUT TO BR-NO LAST-BR-KEY.
           PERFORM READ-BR-FILE.
           IF IO-FG = 9
              MOVE 0 TO LAST-BR-KEY
              MOVE 9 TO DATA-ERR-FLAG
              GO TO EXIT-READ-DATA-FILE.
           MOVE BR-REC TO DATA-AREA.
           GO TO EXIT-READ-DATA-FILE.
       GET-NO1-FILE.
           IF DATA-NUMOUT = 0
              MOVE 1 TO DATA-ERR-FLAG
              GO TO EXIT-READ-DATA-FILE.
           IF DATA-NUMOUT = LAST-NO1-KEY
              MOVE NO-REC TO DATA-AREA
              GO TO EXIT-READ-DATA-FILE.

           MOVE DATA-NUMOUT TO NO-ACCTNO LAST-NO1-KEY.
           PERFORM READ-NO1-FILE.
      *    UNLOCK NO-FILE.
           IF IO-FG = 9
              PERFORM BUILD-NO-REC
              MOVE 0 TO IO-FG.
           MOVE NO-REC TO DATA-AREA.
           GO TO EXIT-READ-DATA-FILE.
       GET-LP1-FILE.
           IF A-BUF = " "
              MOVE 1 TO DATA-ERR-FLAG
              GO TO EXIT-READ-DATA-FILE.
           IF A-BUF = LAST-LP1-KEY
              MOVE LP-REC TO DATA-AREA
              GO TO EXIT-READ-DATA-FILE.

           PERFORM OPEN-LP1-FILE.
           MOVE A-BUF TO LP1-KEY LAST-LP1-KEY.
           PERFORM READ-LP1-FILE.
           PERFORM CLOSE-LP1-FILE.
           IF IO-FG = 9
              MOVE " " TO LAST-LP1-KEY
              MOVE 9 TO DATA-ERR-FLAG
              GO TO EXIT-READ-DATA-FILE.
           MOVE LP-REC TO DATA-AREA.
           GO TO EXIT-READ-DATA-FILE.
       GET-DL1-FILE.
           IF DATA-NUMOUT = 0
              MOVE 1 TO DATA-ERR-FLAG
              GO TO EXIT-READ-DATA-FILE.
           IF DATA-NUMOUT = LAST-DL1-KEY
              MOVE DL-REC TO DATA-AREA
              GO TO EXIT-READ-DATA-FILE.
           PERFORM OPEN-DL1-FILE.
           MOVE DATA-NUMOUT TO DL-NUM LAST-DL1-KEY.
           PERFORM READ-DL1-FILE.
           PERFORM CLOSE-DL1-FILE.
           IF IO-FG = 9
              MOVE 0 TO LAST-DL1-KEY
              MOVE 9 TO DATA-ERR-FLAG
              GO TO EXIT-READ-DATA-FILE.
           MOVE DL-REC TO DATA-AREA.
           GO TO EXIT-READ-DATA-FILE.
       GET-F7-FILE.
           IF DATA-NUMOUT = 0
              MOVE 1 TO DATA-ERR-FLAG
              GO TO EXIT-READ-DATA-FILE.
           IF DATA-NUMOUT = LAST-LT-COLL-KEY
              MOVE WK-LT-RECORD TO DATA-AREA
              GO TO EXIT-READ-DATA-FILE.
           PERFORM GET-LT-COLL.
           IF RECORD-EXISTS = "N"
              MOVE 0 TO LAST-LT-COLL-KEY
              MOVE 9 TO DATA-ERR-FLAG
              GO TO EXIT-READ-DATA-FILE.
           MOVE WK-LT-RECORD TO DATA-AREA.
           GO TO EXIT-READ-DATA-FILE.
       GET-F8-FILE.
           IF DATA-NUMOUT = 0
              MOVE 1 TO DATA-ERR-FLAG
              GO TO EXIT-READ-DATA-FILE.
           IF DATA-NUMOUT = LAST-LT-AUTO-KEY
              MOVE WK-AUTO-INS TO DATA-AREA
              GO TO EXIT-READ-DATA-FILE.
           PERFORM GET-LT-AUTO.
           IF RECORD-EXISTS = "N"
              MOVE 0 TO LAST-LT-AUTO-KEY
              MOVE 9 TO DATA-ERR-FLAG
              GO TO EXIT-READ-DATA-FILE.
           MOVE WK-AUTO-INS TO DATA-AREA.
           GO TO EXIT-READ-DATA-FILE.
       GET-GB-FILE.
           MOVE GB-REC TO DATA-AREA.
           GO TO EXIT-READ-DATA-FILE.
       GET-LS1-FILE.
           IF DATA-NUMOUT = 0
              MOVE 1 TO DATA-ERR-FLAG
              GO TO EXIT-READ-DATA-FILE.
           IF DATA-NUMOUT = LAST-LSX1-KEY
              MOVE LSX-REC TO DATA-AREA
              GO TO EXIT-READ-DATA-FILE.

           PERFORM OPEN-LSX1-FILE.
           MOVE DATA-NUMOUT TO LSX-ACCTNO LAST-LSX1-KEY.
           MOVE 1           TO LSX-SEQNO.
           PERFORM READ-LSX1-FILE.
           PERFORM CLOSE-LSX1-FILE.
           IF IO-FG = 9
              MOVE 0 TO LAST-LSX1-KEY
              MOVE 9 TO DATA-ERR-FLAG
              GO TO EXIT-READ-DATA-FILE.
           MOVE LSX-REC TO DATA-AREA.
           GO TO EXIT-READ-DATA-FILE.
       GET-ML1-FILE.
           GO TO EXIT-READ-DATA-FILE.
       GET-BANKR-FILE.
           IF DATA-NUMOUT = 0
              MOVE 1 TO DATA-ERR-FLAG
              GO TO EXIT-READ-DATA-FILE.
           IF DATA-NUMOUT = LAST-LT-BANKR-KEY
              MOVE WK-BANKR-RECORD TO DATA-AREA
              GO TO EXIT-READ-DATA-FILE.
           PERFORM GET-LT-BANKRUPT.
           IF RECORD-EXISTS = "N"
              MOVE 0 TO LAST-LT-BANKR-KEY
              MOVE 9 TO DATA-ERR-FLAG
              GO TO EXIT-READ-DATA-FILE.
           MOVE WK-BANKR-RECORD TO DATA-AREA.
           GO TO EXIT-READ-DATA-FILE.

      ***********************************
      *   THESE ROUTINES ARE USED WHEN  *
      *   NO KEY IS PROVIDED (KEY=  0 ) *
      ***********************************
       BOGUS-NO-KEY.
           MOVE SPACES TO DATA-AREA.
           GO TO EXIT-READ-DATA-FILE.
       LN1-NO-KEY.
           MOVE LN-REC TO DATA-AREA.
           GO TO EXIT-READ-DATA-FILE.
       BW1-NO-KEY.
           MOVE BW-REC TO DATA-AREA.
           GO TO EXIT-READ-DATA-FILE.
       BR-NO-KEY.
           MOVE BR-REC TO DATA-AREA.
           GO TO EXIT-READ-DATA-FILE.
       NO1-NO-KEY.
           MOVE NO-REC TO DATA-AREA.
           GO TO EXIT-READ-DATA-FILE.
       LP1-NO-KEY.
           MOVE LP-REC TO DATA-AREA.
           GO TO EXIT-READ-DATA-FILE.
       DL1-NO-KEY.
           MOVE DL-REC TO DATA-AREA.
           GO TO EXIT-READ-DATA-FILE.
       F7-NO-KEY.
           GO TO EXIT-READ-DATA-FILE.
       F8-NO-KEY.
           GO TO EXIT-READ-DATA-FILE.
       GB-NO-KEY.
           MOVE GB-REC TO DATA-AREA.
           GO TO EXIT-READ-DATA-FILE.
       LS1-NO-KEY.
           GO TO EXIT-READ-DATA-FILE.
       ML1-NO-KEY.
           GO TO EXIT-READ-DATA-FILE.
       BANKR-NO-KEY.
           GO TO EXIT-READ-DATA-FILE.
       EXIT-READ-DATA-FILE.
           EXIT.

      ****************************
       WRITE-FORM-LINE SECTION.
      ****************************
           ADD 1 TO LN-NO.
           MOVE EDT-LINE-BUFFER TO D-LN(LN-NO).
           IF LN-NO = FLEX-INQ-LINES
              PERFORM DISPLAY-ALL-LINES
              PERFORM RETURN-TO-CONTINUE
              MOVE SPACE TO LINE-BUF
              MOVE 0 TO LN-NO.
           MOVE SPACE TO EDT-LINE-BUFFER.

      ****************************
       GET-FD SECTION.
      ****************************
      * FIELDS ARE SORTED BY ROW/COL IN FDMAIN.
      * 00/00 ROW/COL APPEAR AT LOW OFFSETS AND ARE SKIPPED.
           MOVE " " TO LAST-FDB1-KEY.
           IF FD1-KEY = F-B-FMNO
              GO TO EXIT-FIND-FIRST-FIELD.
           PERFORM READ-FD1-FILE
           IF IO-FG = 9
              MOVE "FORM# NOT LOCATED" TO MESS
              PERFORM SEND-MESS
              MOVE "?" TO ERRCD
              MOVE 0 TO LOAN-KEY
              GO TO END-ROUTINE.
           MOVE FD-REC TO F-B-REC
           MOVE FD-NEXTFM TO NEXT-FMNO.
           MOVE 1 TO FIRST-FIELD.
       FIND-1ST-FIELD.
           IF F-B-FIELDROW(FIRST-FIELD) NOT = 0
              GO TO EXIT-FIND-FIRST-FIELD.
           ADD 1 TO FIRST-FIELD.
           IF FIRST-FIELD < 51
              GO TO FIND-1ST-FIELD.
       EXIT-FIND-FIRST-FIELD.
           EXIT.

      ********************************************
       BUILD-NO-REC SECTION.
      ********************************************

           MOVE FM-INITKEY            TO LPWSFM-EDIT-FM-INITKEY.
           MOVE LPWSFM-EDIT-FM-ACCTNO TO NO-ACCTNO CRNOR2-ACCTNO.
           MOVE SAVE-SYS-DATE         TO CRNOR2-SYSDATE.
           MOVE "CL/CRNOR2" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CRNOR2-LINK NO-REC
                                 POFF-WORKERS LN-REC.
           CANCEL FORM-PROGX.

      ********************************************
       RETURN-TO-CONTINUE SECTION.
      ********************************************
       RETURN-TO-CONT.
           MOVE "F1 - END, <ENTER> TO CONTNUE" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E RA000SEL." TO VDU.
           PERFORM SCREENIO.
           MOVE SPACES TO F1-FG
           IF VDUSTATUS = "10"
              MOVE "Y" TO F1-FG
           ELSE
              IF VDUSTATUS NOT = "00"
                 GO TO RETURN-TO-CONT.

      ******************************************
       GET-LT-AUTO SECTION.
      ******************************************

           MOVE "N" TO RECORD-EXISTS.

           PERFORM OPEN-LTA1-FILE.

           MOVE BR-NO       TO LTA-BRNO.
           MOVE DATA-NUMOUT TO LTA-ACCTNO LAST-LT-AUTO-KEY.
           MOVE 1           TO LTA-SEQNO.
           PERFORM READ-LTA1-FILE.
      *    UNLOCK LTA-FILE.
           IF IO-FG NOT = 0
              MOVE SPACES TO WK-AUTO-INS
              MOVE 0 TO WK-A-PHONE
                        WK-A-EFF-DATE
                        WK-A-EXP-DATE
                        WK-A-CAN-DATE
                        WK-A-CONF-DATE
              MOVE 0 TO WK-A-BIL-COV
                        WK-A-PDL-COV
                        WK-A-PIP-COV
                        WK-A-UMB-COV
                        WK-A-COL-COV
           ELSE
              MOVE "Y" TO RECORD-EXISTS
              MOVE LTA-AUTO-RECORD TO WK-AUTO-INS.


      ******************************************
       GET-LT-COLL SECTION.
      ******************************************
           MOVE "N" TO RECORD-EXISTS.

           PERFORM OPEN-LTC1-FILE.

           MOVE BR-NO       TO LTC-BRNO.
           MOVE DATA-NUMOUT TO LTC-ACCTNO LAST-LT-COLL-KEY.
           MOVE 1           TO LTC-SEQNO.
           PERFORM READ-LTC1-FILE.
      *    UNLOCK LTC-FILE.
           IF IO-FG NOT = 0
              MOVE SPACES TO WK-AUTO-1
              MOVE 0 TO WK-MILEAGE-1 WK-VALUE-1
                        WK-SELLPR-1 WK-MISC-AMT-1
                        WK-VIN-CHG-DATE-1
              MOVE SPACES TO WK-AUTO-2
              MOVE 0 TO WK-MILEAGE-2 WK-VALUE-2
                        WK-SELLPR-2 WK-MISC-AMT-2
                        WK-VIN-CHG-DATE-2
              MOVE SPACES TO WK-HHGS-1
              MOVE SPACES TO WK-HHGS-2
              MOVE SPACES TO WK-HHGS-3
           ELSE
              MOVE "Y" TO RECORD-EXISTS
              MOVE LTC-AUTO(1) TO WK-AUTO-1
              MOVE LTC-AUTO(2) TO WK-AUTO-2
              MOVE LTC-AUTO(3) TO WK-AUTO-3
              MOVE LTC-HHGS(1) TO WK-HHGS-1
              MOVE LTC-HHGS(2) TO WK-HHGS-2
              MOVE LTC-HHGS(3) TO WK-HHGS-3.



      ******************************************
       GET-LT-BANKRUPT SECTION.
      ******************************************
           MOVE "N" TO RECORD-EXISTS.

           PERFORM OPEN-LTB1-FILE.

           MOVE BR-NO       TO LTB-BRNO.
           MOVE DATA-NUMOUT TO LTB-ACCTNO LAST-LT-BANKR-KEY.
           MOVE 1           TO LTB-SEQNO.
           PERFORM READ-LTB1-FILE.
      *    UNLOCK LTB-FILE.
           IF IO-FG NOT = 0
              MOVE SPACES TO WK-BANKR-RECORD
              MOVE 0 TO WK-B-FILEDATE WK-B-CHAPTER
                        WK-B-BDATE WK-B-CLAIM-AMT
                        WK-B-INT-LC-DUE WK-B-OTH-DUE WK-B-TDUEAMT
              MOVE 0 TO WK-B-PHONE WK-B-COURT-PHONE
              MOVE 0 TO WK-B-TRUST-PHONE
           ELSE
              MOVE "Y" TO RECORD-EXISTS
              MOVE LTB-BANKRUPT-AREA TO WK-BANKR-RECORD.


       COPY "LIBLP/LPEXT.CPY".
       COPY "LIBLP/LPEDT.CPY".
       COPY "LIBLP/LPMSC.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".

      ***********************************************
       FORM-RESET SECTION.
      ***********************************************
           DISPLAY CPINQ-SCREEN UPON CPINQ-WINDOW-HANDLE.
           MOVE CPINQ-FORM-FIELDS TO WR-BUF.
           MOVE CPINQ-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ***********************************************
       SETUP-LINK-INFO SECTION.
      ***********************************************
           MOVE CPINQ-HELP--FILE TO HELP-LINK-FILE.
           MOVE CPINQ-HELP--DIR TO HELP-LINK-DIR.

      ***********************************************
       VDU-CONVERT-FIELD SECTION.
      ***********************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/CPINQ_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ***********************************
       MISC-PARAGRAPHS SECTION.
      ***********************************
       COPY "LIBGB/ACCESS.CPY".
       SET-EXT-WK.
           MOVE FDB-DOFFSET(D-B-SUB) TO DATA-OFFSET.
           MOVE FDB-DDIGITS(D-B-SUB) TO DATA-DIGITS.
           MOVE FDB-DTYPE(D-B-SUB) TO DATA-TYPE.
           MOVE FDB-DDECIMAL(D-B-SUB) TO DATA-DECIMAL.
       DISPLAY-ALL-LINES.
           IF LINE-BUF NOT = " "
               MOVE LINE-BUF TO WR-BUF
               MOVE LINE-LIST TO WR-LIST
               PERFORM CALL-WRFORM.
       SEND-LEGEND.
           MOVE "S  A000MSEL." TO VDU.
           MOVE MESS TO VDUBUF.
           PERFORM SCREENIO.

      ******************************
       IO-ROUTINE SECTION.
      ******************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-NO1-FILE.
           PERFORM CLOSE-LTC1-FILE.
           PERFORM CLOSE-LTB1-FILE.
           PERFORM CLOSE-LTA1-FILE.
           PERFORM CLOSE-LSX1-FILE.
           PERFORM CLOSE-LP1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-FDB1-FILE.
           PERFORM CLOSE-FD1-FILE.
           PERFORM CLOSE-DL1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GB-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPNOGS_SQL.CPY".
       COPY "LIBLP/LPLTLGS_SQL.CPY".
       COPY "LIBLP/LPLTCGS_SQL.CPY".
       COPY "LIBLP/LPLTBGS_SQL.CPY".
       COPY "LIBLP/LPLTAGS_SQL.CPY".
       COPY "LIBLP/LPLSXGS_SQL.CPY".
       COPY "LIBLP/LPLPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPFDBGS_SQL.CPY".
       COPY "LIBLP/LPFDGS_SQL.CPY".
       COPY "LIBLP/LPDLGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBGBGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBGBRN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPDL1RN.CPY".
       COPY "LIBLP/LPFD1RN.CPY".
       COPY "LIBLP/LPFDB1RN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBLP/LPLP1RN.CPY".
       COPY "LIBLP/LPNO1RN.CPY".
       COPY "LIBLP/LPLTA1RN.CPY".
       COPY "LIBLP/LPLTC1RN.CPY".
       COPY "LIBLP/LPLTB1RN.CPY".
       COPY "LIBLP/LPLSX1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      ***********************************
       END-ROUTINE SECTION.
      ***********************************
       END-RUN.
           MOVE PROG-BUF TO CPINQ-PROG-BUF.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           EXIT PROGRAM.
