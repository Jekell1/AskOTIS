       IDENTIFICATION DIVISION.
       PROGRAM-ID.   PITBO2.
       DATE-WRITTEN. 2023.1204
      *****************************************************
      *
      *                      (SP)
      *    DESCRIPTION:  PITNEY BOWES/ TEXT EXTRACTS
      *
      *           TEXT EXT, MARKETING, 
      *           M,BR#, ACCT#, CELL#, FIRST, LAST, GLOBAL NAME, 
      *                  BRANCH PHONE, CASH AVAIL 
      *           IGNORES AVAILABLE CREDIT EDITS
      *
      *          - CAPABLE OF RUNNING FROM SINGLE BRANCH, BRANCH RANGE
      *             OR GROUP.
      *          - ASCII FORMAT. COMMA DELIMITED WITH CARRIAGE RETURN.
      *          - AUTOMATIC BREAKS AT 1 MEG. IT WILL NOT BREAK IN THE
      *            MIDDLE OF A BRANCH.
      *          - SAME RECORD LAYOUT FOR COMPATABILITY WITH PITNEY
      *            BOWES ADDRESSING MACHINE.
      *          - ALL FILES STORED IN THE GLOBAL ED DIRECTORY.
      *
      *        THE TEXT FILES ARE SAVED IN THE FOLLOWING FORMAT:
      *               TEXT1234MMDDYY.MK
      *               TEXTR1MMDDYY.MK
      *          1234 = BRNO OR GROUP : BEGINNING BRANCH # OR SELECTED GROUP
      *
      *         THE USER WILL BE PROMPTED IF DATED FILE ALREADY EXISTS
      *         IF THE FILE ALREADY EXIST AND THE USER CHOOSES TO CONTINUE,
      *         IT WILL BE OPENED TO OUTPUT !
      *
      * REV:
      *  BAH 2023.1204 #1632 COPY PITBOW TO PITBO2 AND REMOVE EVERYTHING
      *                EXCEPT TEXT OPTION 8 FOR MARKETING BRANCH MOVE
      *  RMZ 2024.0116 CHANGED JULIAN-BEG(END,CC) TO EXT-JULIAN-BEG(END,CC)
      ***********************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".

           SELECT TEXT-OUT-FILE ASSIGN TEXT-OUT-PATH
                  ORGANIZATION RELATIVE
                  ACCESS MODE DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RELATIVE KEY TEXT-OUT-KEY
                  FILE STATUS FILE-STAT.

           SELECT TEXT-SORT-FILE ASSIGN TEXT-SORT-PATH
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY TEXT-SORT-KEY
                  FILE STATUS FILE-STAT.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".

      *******************************************************
      *   TEXT OUT, ASCII OUTPUT FILE FOR TEXT FORMAT
      *******************************************************
       FD  TEXT-OUT-FILE
           LABEL RECORDS ARE STANDARD.
       01  TEXT-OUT-REC.
           03  TEXT-OUT-TYPE      PIC X.
           03  TEXT-OUT-OWNBR     PIC 9999.
           03  TEXT-OUT-ACCTNO    PIC 9(6).
           03  TEXT-OUT-CELL      PIC 9(10).
           03  TEXT-OUT-FNAME     PIC X(16).
           03  TEXT-OUT-LNAME     PIC X(16).
           03  TEXT-OUT-BR-NAME   PIC X(10).
           03  TEXT-OUT-BR-PHONE  PIC 9(10).
           03  TEXT-OUT-AVAILCR   PIC 9(4).99 BLANK ZERO.
           03  TEXT-OUT-1ST-DUE   PIC ZZ.
           03  TEXT-OUT-DUE-EXT   PIC XX.
           03  TEXT-OUT-RETURN-CD PIC XX.
           03  TEXT-OUT-CR        PIC X.
           03  TEXT-OUT-LF        PIC X.

       FD  TEXT-SORT-FILE
           LABEL RECORDS ARE STANDARD.
       01  TEXT-SORT-REC.
           03  TEXT-SORT-KEY.
               05  TEXT-SORT-LNAME PIC X(16).
               05  TEXT-SORT-FNAME PIC X(16).
               05  TEXT-SORT-CELL  PIC 9(10).
           03  TEXT-SORT-TYPE      PIC X.
           03  TEXT-SORT-OWNBR     PIC 9999.
           03  TEXT-SORT-ACCTNO    PIC 9(6).
           03  TEXT-SORT-BR-NAME   PIC X(10).
           03  TEXT-SORT-BR-PHONE  PIC 9(10).
           03  TEXT-SORT-AVAILCR   PIC 9(4).99.
           03  TEXT-SORT-1ST-DUE   PIC ZZ BLANK ZERO.
           03  TEXT-SORT-DUE-EXT   PIC XX.
           03  TEXT-SORT-RETURN-CD PIC XX.

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)SP/PITBO2.CBL S35 06/25/24-12:27:16 >".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
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
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBGB/GB01GB.CPY".
       COPY "LIBGB/GB01GB_SQL.CPY".
       COPY "LIBGB/GBWSGB.CPY".
       COPY "LIBLP/LP01LM.CPY".
       COPY "LIBLP/LP01LM_SQL.CPY".
       COPY "LIBLP/LPWSLM.CPY".
       COPY "LIBLP/ARRAYSPW.CPY". 

       COPY "LIBGB/GRSSPW.CPY".
       COPY "LIBGB/GRBRLSW.CPY".

       01  TEXT-OUT-PATH            PIC X(35).
       01  TEXT-OUT-PATH-WORKERS.
           03  TEXT-OUT-PATH-FIXED.
               05  TEXT-OUT-PATH-BASE     PIC X(9).
               05  FILLER                 PIC X       VALUE "/".
               05  TEXT-OUT-PATH-MACHINE  PIC X(2).
               05  FILLER                 PIC X(8) VALUE "/ED/TEXT".
           03  TEXT-OUT-PATH-BRNO         PIC 9999.
           03  TEXT-OUT-PATH-GROUP REDEFINES
                                          TEXT-OUT-PATH-BRNO PIC X(4).
           03  TEXT-OUT-PATH-MM        PIC 99.
           03  TEXT-OUT-PATH-DD        PIC 99.
           03  TEXT-OUT-PATH-YY        PIC 99.
           03  TEXT-OUT-PATH-DOT       PIC X      VALUE ".".
           03  TEXT-OUT-PATH-REPORT    PIC X(2)   VALUE "MK".

       01  PR-IFN               PIC X(2)     VALUE "PR".

       01  TEXT-OUT-KEY         PIC 9(6)  VALUE 0.
       01  TEXT-SORT-PATH       PIC X(35).

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  PASS-MINTERM                  PIC 999.
       01  PASS-MAXTERM                  PIC 999.
       01  PASS-OPENFORMER               PIC X.
       EXEC SQL END DECLARE SECTION END-EXEC.

       01  MISC-CONN-OPEN-SW             PIC X           VALUE "N".
       01  NEXT-BRANCH-NO                PIC 9(4)        VALUE 0.
       01  PROC-BRANCH-NO                PIC 9(4)        VALUE 0.
       01  LOOP-PROC-SUB                 PIC 9(4)        VALUE 0.

      **********************
      *   LINE WORKERS     *
      **********************
       01  LN-WORKERS.
           03  LN-FEED          PIC 999      VALUE 1.
           03  LN-COUNT         PIC 999      VALUE 99.
           03  LN-WK            PIC 999      VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

       01  ER-FEED              PIC 9        VALUE 1.
       01  CLASS-FG             PIC X(3)     VALUE SPACES.

      ***********************************
      *     P R I N T   B U F F E R S   *
      ***********************************
       01  HEAD1.
           03  H-DATE           PIC X(8)     VALUE " ".
           03  FILLER           PIC XX       VALUE " ".
           03  H-HR             PIC 99       VALUE 0.
           03  FILLER           PIC X        VALUE ":".
           03  H-MIN            PIC 99       VALUE 0.
           03  FILLER           PIC X(8)    VALUE " ".
           03  H-NAME           PIC X(40)    VALUE " ".
           03  FILLER           PIC X(8)    VALUE " ".
           03  FILLER           PIC X(5)     VALUE "PAGE:".
           03  H-PAGE-NO        PIC ZZZ9.

       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(15)    VALUE " ".
           03  H-EXTRACT        PIC X(40).

       01  HEAD3.
           03  FILLER           PIC X(10)    VALUE "BRANCH :  ".
           03  H-BRNO           PIC 9(4).
           03  FILLER           PIC X(2)     VALUE " ".
           03  H-BRDESC         PIC X(20).

       01  HEAD4-TEXT.
           03  FILLER      PIC X(31)    VALUE " NAME         ".
           03  FILLER      PIC X(22)    VALUE
               " CELL        ACCT#    ".
           03  HEAD4-1ST-DUE PIC X(19)  VALUE
               "1ST DUE          ".
           03  HEAD4-AVAIL   PIC X(8)  VALUE
               "NEXT DUE".
 
       01  DETAIL-LINE          PIC X(132) VALUE SPACES.

       01  DETAIL1 REDEFINES DETAIL-LINE.
           03  D-NAME        PIC X(26).
           03  FILLER        PIC XX.
           03  D-PHONE       PIC 999/999/9999.
           03  FILLER        PIC XXX.
           03  D-NUMBER      PIC Z(6).
           03  FILLER        PIC X(22).

       01  SUMMARY1 REDEFINES DETAIL-LINE.
           03  D-BRANCH-PROMPT  PIC X(10).
           03  D-BRNO           PIC Z(4).
           03  FILLER           PIC X(9).
           03  D-ACCTS-PROMPT   PIC X(15).
           03  D-COUNT          PIC Z(6)9.
           03  FILLER           PIC X(88).

       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03  RANGE-DESC       PIC X(26).
           03  RANGE-SELECTION  PIC X(106).
           03  FILLER           REDEFINES RANGE-SELECTION.
               05  RANGE-ALP1   PIC X(15).
               05  RANGE-NUM1   REDEFINES RANGE-ALP1
                                PIC Z(14)9.
               05  RANGE-DATE1  REDEFINES RANGE-ALP1
                                PIC 9999/99/99BBBBB.
               05  FILLER       PIC XX.
               05  RANGE-ALP2   PIC X(15).
               05  RANGE-NUM2   REDEFINES RANGE-ALP2
                                PIC Z(14)9.
               05  RANGE-DATE2  REDEFINES RANGE-ALP2
                                PIC 9999/99/99BBBBB.
               05  FILLER       PIC X(74).
           03  RANGE-TABLE      REDEFINES RANGE-SELECTION.
               05  RANGE-TAB    OCCURS 10
                                PIC XXXXX.
               05  FILLER       PIC X(56).

      **********************
      *   MISC WORKERS     *
      **********************
       01  MSEL-LIST            PIC X(5)     VALUE "MSEL.".

       01  CURRENT-COUNT        PIC 9(7)     VALUE 0.
       01  REC-COUNT            PIC 9(7)     VALUE 0.
       01  TOTAL-COUNT          PIC 9(7)     VALUE 0.

       01  RECORDS-FOUND        PIC X        VALUE " ".
           88  NO-RECORDS-FOUND              VALUE "N".

       01  PROFILE-OK           PIC X.
       01  PROFILE-HIGH         PIC 999.
       01  PROFILE-WORKER-01.
           03  PROFILE-WORKER   PIC 9(12).
           03  PROFILE-TAB REDEFINES PROFILE-WORKER
                                OCCURS 12 PIC 9.
       01  WORK-MONTHS          PIC 9(5).
       01  MARKETING-TEXT.
           03  FILLER           PIC X(22) VALUE
               "RENEWAL TEXT SENT FOR ".
           03  MARKETING-AVAILCR PIC $$$$$$.$$.
       01  EDIT-FNAME           PIC X(16).
       01  EDIT-FNAME-CHAR REDEFINES EDIT-FNAME PIC X OCCURS 16.
       01  WS-FNAME             PIC X(16).
       01  WS-FNAME-CHAR REDEFINES WS-FNAME PIC X OCCURS 16.
       01  TEST-CELL-NO         PIC 9(10).
       01  TEST-CELL REDEFINES TEST-CELL-NO PIC 9 OCCURS 10.
       01  DIGIT-COUNT          PIC 99.

       01  TRANS-DATE           PIC 9(8)     COMP-3 VALUE 0.
       01  SUB                  PIC 999.
       01  FIRST-TIME           PIC X        VALUE "Y".
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".

       01  LEGEND               PIC X(70).
       COPY "LIBLP/EMSFRG_CMD.CPY".

      ******************************************************************
      *
      *     S C R E E N   F I E L D S
      *
      ******************************************************************
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  BEG-BRANCH       PIC 9(4).
           03  END-BRANCH       PIC 9(4).
           03  PRINT-TOTALS     PIC X.
           03  GROUP-FLAG       PIC X.
           03  ENT-GROUP        PIC X(4).
           03  RCP-YN                   PIC X.
           03  PRINT-DETAIL             PIC X.
           03  REPORT-ONLY              PIC X.
           03  TEXTM-LOAN-FIELDS.
               05  TEXTM-BEG-DATE        PIC 99/99/99.
               05  TEXTM-END-DATE        PIC 99/99/99.
               05  TEXTM-BEG-CLASS       PIC 99.
               05  TEXTM-END-CLASS       PIC 99.
               05  TEXTM-MINTERM         PIC 999.
               05  TEXTM-MAXTERM         PIC 999.
               05  TEXTM-MINNXTPAY       PIC 99.
               05  TEXTM-MAXNXTPAY       PIC 99.
               05  TEXTM-MINLSTPAY       PIC 99.
               05  TEXTM-MAXLSTPAY       PIC 99.
               05  TEXTM-SRCD-BUF.
                   07  TEXTM-SRCD01      PIC XX.
                   07  TEXTM-SRCD02      PIC XX.
                   07  TEXTM-SRCD03      PIC XX.
                   07  TEXTM-SRCD04      PIC XX.
                   07  TEXTM-SRCD05      PIC XX.
                   07  TEXTM-SRCD06      PIC XX.
                   07  TEXTM-SRCD07      PIC XX.
                   07  TEXTM-SRCD08      PIC XX.
                   07  TEXTM-SRCD09      PIC XX.
                   07  TEXTM-SRCD10      PIC XX.
               05  TEXTM-XMON            PIC 9(2).
               05  TEXTM-HCON            PIC 9(3).
               05  TEXTM-HREC            PIC 9(3).
               05  TEXTM-CPOT            PIC X(1).
               05  TEXTM-RPOT            PIC X(1).
               05  TEXTM-MINAV           PIC 9(6).
               05  TEXTM-SKIP-MULT-ACCT  PIC X.
               05  TEXTM-CONT            PIC X.
               05  TEXTM-PCT-TERM        PIC 99.
               05  TEXTM-OPTIN-BUF.
                   07  TEXTM-OPTIN1      PIC X.
                   07  TEXTM-OPTIN2      PIC X.
                   07  TEXTM-OPTIN3      PIC X.
                   07  TEXTM-OPTIN4      PIC X.
               05  TEXTM-OPEN-FORMER-FG  PIC X.
               05  TEXTM-FILE-TYPE       PIC X.
               05  TEXTM-MEMO            PIC X(20).

       01  VERSION-CONTROL      PIC X VALUE "A".

       01  TEXT-OPTIN-LIST.
           03  FILLER          PIC X(28) VALUE
           "OPTIN1 OPTIN2 OPTIN3 OPTIN4.".

       01  TEXTM-SRCD-LIST.
           03  FILLER          PIC X(35) VALUE
           "SRCD01 SRCD02 SRCD03 SRCD04 SRCD05 ".
           03  FILLER          PIC X(35) VALUE
           "SRCD06 SRCD07 SRCD08 SRCD09 SRCD10.".

       01  TEXTM-BUF           PIC X(100) VALUE SPACES.
       01  TEXTM-LIST.
           03  FILLER          PIC X(20) VALUE
           "LNDT1 LNDT2 CL1 CL2 ".
           03  FILLER          PIC X(26) VALUE
           "MINTERM MAXTERM MINNXTPAY ".
           03  FILLER          PIC X(30) VALUE
           "MAXNXTPAY MINLSTPAY MAXLSTPAY ".
           03  FILLER          PIC X(35) VALUE
           "SRCD01 SRCD02 SRCD03 SRCD04 SRCD05 ".
           03  FILLER          PIC X(35) VALUE
           "SRCD06 SRCD07 SRCD08 SRCD09 SRCD10 ".
           03  FILLER          PIC X(20) VALUE
           "XMON HCON HREC CPOT ".
           03  FILLER          PIC X(11) VALUE
           "RPOT MINAV ".
           03  FILLER          PIC X(22) VALUE
           "MULTACCT CONT PCTTERM ".
           03  FILLER          PIC X(28) VALUE
           "OPTIN1 OPTIN2 OPTIN3 OPTIN4 ".
           03  FILLER          PIC X(15) VALUE
           "OPEN TYPE MEMO.".

       01  SV-BEG-BR            PIC 9999  VALUE 9999.
       01  SV-END-BR            PIC 9999  VALUE 9999.

       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBWI/PFSWINDOWSW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBLP/LPWKPATH.CPY".
       COPY "LIBGB/SPPDTHW.CPY".
       COPY "LIBWI/LCSCANW.CPY".
       COPY "LIBLP/LPCEPPW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBLP/LPSCHDW.CPY".
       COPY "LIBGB/AGEINGW.CPY".
       COPY "LIBLP/LPPDUEW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/PITBO2_DEF.CPY".
       COPY "LIBSP/PITBO3_DEF.CPY".
       COPY "LIBSP/PITBO2_WKS.CPY".
       COPY "LIBSP/PITBO3_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *****************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      *****************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBSP/PITBO2_SCN.CPY".
       COPY "LIBSP/PITBO3_SCN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE TEXT-OUT-FILE TEXT-SORT-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LTP1-FILE.
           PERFORM CLOSE-LTI1-FILE.
           PERFORM CLOSE-LN3-FILE.
           PERFORM CLOSE-LM1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-GB-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM SQL-CLOSE-UPDATE-CONNECTION.
           PERFORM CLOSE-MISC-CONNECTION.
           PERFORM CLOSE-PR-FILE.
           CLOSE
               TEXT-OUT-FILE TEXT-SORT-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       MAIN-MODULE SECTION.
           PERFORM SQL-CONNECT.
      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "PITBO2" TO VDU-FORM-NAME.

           PERFORM GET-GPENV.

           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO NUM-DATE TRANS-DATE.
           MOVE NUM-MO TO TEXT-OUT-PATH-MM.
           MOVE NUM-DA TO TEXT-OUT-PATH-DD.
           MOVE NUM-YR TO TEXT-OUT-PATH-YY.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

           MOVE "B"        TO BW-PATH-OVERRIDE
                              LN-PATH-OVERRIDE
                              LTI-PATH-OVERRIDE
                              LTP-PATH-OVERRIDE
                              GB-PATH-OVERRIDE
                              LM-PATH-OVERRIDE.

           MOVE 0              TO PASS-MINTERM PASS-MAXTERM.
           MOVE SPACE          TO PASS-OPENFORMER.

      * LOAD BRANCHES TO PROCESS USING STORED PROCEDURE SSP_TCLP_BRANCH_GROUP_LOGIC_SELECT
      *   INTO BGLS-BRANCH-TBL
           IF GROUP-FLAG = "Y"
               MOVE ENT-GROUP TO BGLS-PASS-BEG-BRGRP
               MOVE SPACES    TO BGLS-PASS-END-BRGRP
           ELSE
               MOVE BEG-BRANCH                TO BRANCHLIST-WORK-BRNO
               MOVE BRANCHLIST-WORK-BRNO-TEXT TO BGLS-PASS-BEG-BRGRP
               MOVE END-BRANCH                TO BRANCHLIST-WORK-BRNO
               MOVE BRANCHLIST-WORK-BRNO-TEXT TO BGLS-PASS-END-BRGRP.

           PERFORM LOAD-BGLS-BRANCH-LIST.

      * SET BRANCHLIST-PGM-PROC-MAX TO NUMBER OF BRANCHES TO PROCESS PER STORED PROCEDURE CURSOR
           MOVE 200 TO BRANCHLIST-PGM-PROC-MAX
 
           PERFORM OPEN-LN3-FILE.

      ****************************************************************************
      *  PROC-LOOP LOADS BRANCHLIST-TBL WITH AMOUNT OF
      *  BRANCHES TO PROCESS PER LOOP SET BY BRANCHLIST-PGM-PROC-MAX
      ****************************************************************************

       PROC-LOOP-INIT.
           INITIALIZE BRANCHLIST-TBL BRANCHLIST-SUB
                      BRANCHLIST-PROC-BRANCH-CTR 
                      PASS-BRANCHLIST PROC-BRANCH-NO.

       PROC-LOOP.
           ADD 1 TO LOOP-PROC-SUB.

           IF LOOP-PROC-SUB > BGLS-BRANCHES-RETR-CTR
              GO TO PROC-LOOP-CONT.

           ADD 1 TO BRANCHLIST-PROC-BRANCH-CTR
                    BRANCHLIST-SUB.

           MOVE BGLS-BRANCH-BRNO(LOOP-PROC-SUB) TO 
                            BRANCHLIST-BRNO(BRANCHLIST-SUB).
           IF BRANCHLIST-SUB > 1
              MOVE ","   TO BRANCHLIST-COMMA(BRANCHLIST-SUB).

           IF BRANCHLIST-PROC-BRANCH-CTR < BRANCHLIST-PGM-PROC-MAX 
               GO TO PROC-LOOP.
 
       PROC-LOOP-CONT.
           IF BRANCHLIST-SUB = 0
              GO TO END-ROUTINE.

           MOVE BRANCHLIST-TBL TO PASS-BRANCHLIST.

      ******************************************************************
      * PROCESS TEXT MARKETING SOLICITATION ACCOUNTS
      ******************************************************************

      *     MOVE BR-NO           TO LN-OWNBR
      *                             QLN3-WBEG-OWNBR
      *                             QLN3-WEND-OWNBR.
      *     MOVE 0               TO LN-ACCTNO
      *                             QLN3-WBEG-ACCTNO.
      *     MOVE ALL "9"         TO QLN3-WEND-ACCTNO.

           MOVE TEXTM-BEG-CLASS TO LN-CLASS
                                   QLN3-WBEG-CLASS.
           MOVE TEXTM-END-CLASS TO QLN3-WEND-CLASS.

           MOVE TEXTM-MINTERM        TO PASS-MINTERM.
           MOVE TEXTM-MAXTERM        TO PASS-MAXTERM.
           MOVE TEXTM-OPEN-FORMER-FG TO PASS-OPENFORMER.

           PERFORM START-LN3-FILE.  *> SSP - LN BW.

       NEXT-TEXTM-LN.
           PERFORM READ-LN3-FILE-NEXT.    *> SSP - LN, BW
           IF IO-BAD              *> OR (LN-OWNBR NOT = LN-PATH-OWNBR)
      *        PERFORM PROCESS-WORK-FILE
      *        GO TO END-OF-GROUP.
              MOVE 0 TO NEXT-BRANCH-NO
              PERFORM BRANCH-BREAK
              PERFORM CLOSE-LN3-FILE
              GO TO PROC-LOOP-INIT.

           IF LN-OWNBR NOT = PROC-BRANCH-NO
              MOVE LN-OWNBR TO NEXT-BRANCH-NO
              PERFORM BRANCH-BREAK
              MOVE NEXT-BRANCH-NO TO PROC-BRANCH-NO.

      * THIS CHECK ALSO IN STORED PROCEDURE 
           IF LN-POCD = "**"
              GO TO NEXT-TEXTM-LN.

      * SKIP ANY RENEWED ACCTS, SOLICITATION WILL PICK UP RENEWED LOAN
      * THIS CHECK ALSO IN STORED PROCEDURE 
           IF LN-POCD = "PB" OR "10"
              GO TO NEXT-TEXTM-LN.

      * SKIP PL ACCOUNTS:
      * THIS CHECK ALSO IN STORED PROCEDURE 
           IF LN-PLDATE NOT = 0
              GO TO NEXT-TEXTM-LN.

      * SKIP JUDGEMENTS:
      * THIS CHECK ALSO IN STORED PROCEDURE 
           IF LN-JDDATE NOT = 0
              GO TO NEXT-TEXTM-LN.

      * SKIP REPO, LEGAL, BANKRUPT, AND DEBTAID ACCOUNTS:
      * THIS CHECK ALSO IN STORED PROCEDURE 
           IF LN-REPODATE NOT = 0 OR
               LN-DEBTAIDATE NOT = 0 OR
                LN-LEGALDATE NOT = 0 OR
                 LN-BNKRPTDATE NOT = 0
                    GO TO NEXT-TEXTM-LN.

      * TEST CLASS RANGE
      * THIS CHECK ALSO IN STORED PROCEDURE 
           IF LN-CLASS > TEXTM-END-CLASS
              GO TO NEXT-TEXTM-LN.

      * TEST OPEN OR PAIDOUT SELECTION
      * THIS CHECK ALSO IN STORED PROCEDURE 
           IF TEXTM-OPEN-FORMER-FG = "F"
              IF LN-POFFDATE = 0
                 GO TO NEXT-TEXTM-LN.
           IF TEXTM-OPEN-FORMER-FG = "O"
              IF LN-POFFDATE NOT = 0
                 GO TO NEXT-TEXTM-LN.

      * THIS CHECK ALSO IN STORED PROCEDURE 
           IF LN-POCD = "BT"
              GO TO NEXT-TEXTM-LN.

           PERFORM GET-SPR.

      * TEST MINIMUM/MAXIMUM TO THE NEXT DUE PAYMENT:
           IF TEXTM-MINNXTPAY = 0
              IF TEXTM-MAXNXTPAY = 99
                 GO TO END-CHECK-TEXTM-NXTPAY.

      * TEST MINIMUM/MAXIMUM TO THE NEXT DUE PAYMENT: 
           MOVE LN-1STPYDATE TO PDTH-DATE-WORK.
           PERFORM PAID-THRU-CALCULATION.
           MOVE PDTH-DATE-FULL TO NDTE-DATE.
           MOVE 1 TO NDTE-HOLD.
           PERFORM INCREMENT-MONTHS.

           MOVE TRANS-DATE TO NUM-DATE.
           MOVE NDTE-DATE TO SYS-DATE.
           PERFORM TIM365.
           IF ELAPSED-DAYS < TEXTM-MINNXTPAY
                        OR ELAPSED-DAYS > TEXTM-MAXNXTPAY
              GO TO NEXT-TEXTM-LN.

       END-CHECK-TEXTM-NXTPAY.

      * TEST LOAN DATE RANGE LN DATE FOR OPEN, PO DATE FOR FORMER:
      * TEXTM-BEG-DATE IS 99/99/99, MAKE IT CORRECT

           MOVE TEXTM-BEG-DATE TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           IF TEXTM-OPEN-FORMER-FG = "O"
              MOVE LN-LOANDATE TO SYS-DATE
           ELSE
              MOVE LN-POFFDATE TO SYS-DATE.
           IF SYS-DATE < DATE-YYYYMMDD
              GO TO NEXT-TEXTM-LN.

           MOVE TEXTM-END-DATE TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           IF TEXTM-OPEN-FORMER-FG = "O"
              MOVE LN-LOANDATE TO SYS-DATE
           ELSE
              MOVE LN-POFFDATE TO SYS-DATE.
           IF SYS-DATE > DATE-YYYYMMDD
              GO TO NEXT-TEXTM-LN.

      * TEST MINIMUM/MAXIMUM ORIGINAL TERM:
      * THIS CHECK ALSO IN STORED PROCEDURE 
           IF LN-ORGTERM < TEXTM-MINTERM OR LN-ORGTERM > TEXTM-MAXTERM
              GO TO NEXT-TEXTM-LN.

      * TEST MINIMUM/MAXIMUM DAYS SINCE LAST PAYMENT:
           IF TEXTM-MINLSTPAY = 0
              IF TEXTM-MAXLSTPAY = 99
                 GO TO TEXTM-TEST-LOAN-SRCD.

           IF LN-DATE-PAID-LAST = 0
              GO TO NEXT-TEXTM-LN.

           MOVE LN-DATE-PAID-LAST TO NUM-DATE.
           MOVE TRANS-DATE TO SYS-DATE.
           PERFORM TIM365.
           IF ELAPSED-DAYS < TEXTM-MINLSTPAY
                        OR ELAPSED-DAYS > TEXTM-MAXLSTPAY
              GO TO NEXT-TEXTM-LN.

       TEXTM-TEST-LOAN-SRCD.
      * CHECK LOAN SRCD
           IF TEXTM-SRCD01 NOT = SPACES
              IF LN-SRCD = TEXTM-SRCD01
                 GO TO NEXT-TEXTM-LN.
           IF TEXTM-SRCD02 NOT = SPACES
              IF LN-SRCD = TEXTM-SRCD02
                 GO TO NEXT-TEXTM-LN.
           IF TEXTM-SRCD03 NOT = SPACES
              IF LN-SRCD = TEXTM-SRCD03
                 GO TO NEXT-TEXTM-LN.
           IF TEXTM-SRCD04 NOT = SPACES
              IF LN-SRCD = TEXTM-SRCD04
                 GO TO NEXT-TEXTM-LN.
           IF TEXTM-SRCD05 NOT = SPACES
              IF LN-SRCD = TEXTM-SRCD05
                 GO TO NEXT-TEXTM-LN.
           IF TEXTM-SRCD06 NOT = SPACES
              IF LN-SRCD = TEXTM-SRCD06
                 GO TO NEXT-TEXTM-LN.
           IF TEXTM-SRCD07 NOT = SPACES
              IF LN-SRCD = TEXTM-SRCD07
                 GO TO NEXT-TEXTM-LN.
           IF TEXTM-SRCD08 NOT = SPACES
              IF LN-SRCD = TEXTM-SRCD08
                 GO TO NEXT-TEXTM-LN.
           IF TEXTM-SRCD09 NOT = SPACES
              IF LN-SRCD = TEXTM-SRCD09
                 GO TO NEXT-TEXTM-LN.
           IF TEXTM-SRCD10 NOT = SPACES
              IF LN-SRCD = TEXTM-SRCD10
                 GO TO NEXT-TEXTM-LN.

      * TEST CONTRACTUAL PROFILE:
           MOVE TRANS-DATE TO AGEING-DATE.
           PERFORM COMPUTE-CONTRACTUAL.
           PERFORM COMPUTE-RECENCY.

           MOVE LN-CONPROFILE(1) TO PROFILE-WORKER.
           MOVE TEXTM-HCON TO PROFILE-HIGH.
           PERFORM TEST-TEXTM-PROFILE.
           IF PROFILE-OK NOT = "Y"
              GO TO NEXT-TEXTM-LN.

           IF TEXTM-CPOT = "S"
              IF CPOT30 = "Y"
                 GO TO NEXT-TEXTM-LN.
 
           IF TEXTM-CPOT = "O"
              IF CONTRACTUAL = 0
                 IF CPOT30 NOT = "Y"
                    GO TO NEXT-TEXTM-LN.

      * TEST RECENCY PROFILE:
           MOVE LN-RECPROFILE(1) TO PROFILE-WORKER.
           MOVE TEXTM-HREC TO PROFILE-HIGH.
           PERFORM TEST-TEXTM-PROFILE.
           IF PROFILE-OK NOT = "Y"
              GO TO NEXT-TEXTM-LN.

           IF TEXTM-RPOT = "S"
              IF RDPOT30 = "Y"
                 GO TO NEXT-TEXTM-LN.

           IF TEXTM-RPOT = "O"
              IF RECDEL = 0
                 IF RDPOT30 NOT = "Y"
                    GO TO NEXT-TEXTM-LN.

      * BWFILE DATA RETRIEVED IN STORED PROCEDURE
      *     MOVE LN-OWNBR   TO BW-OWNBR.
      *     MOVE LN-SSNO(1) TO BW-SSNO.
      *     PERFORM READ-BW1-FILE.
      *     IF IO-FG NOT = 0
      *        GO TO NEXT-TEXTM-LN.

      * SKIP IF CELL # IS 0, OR ALL SAME DIGIT (1111111111)
      * THIS CHECK ALSO IN STORED PROCEDURE 
           IF BW-CELL-PHONE = 0
              GO TO NEXT-TEXTM-LN.

           MOVE 0 TO DIGIT-COUNT.
           MOVE BW-CELL-PHONE TO TEST-CELL-NO.
           INSPECT TEST-CELL-NO TALLYING DIGIT-COUNT
                                 FOR ALL TEST-CELL(1).
           IF DIGIT-COUNT = 10
              GO TO NEXT-TEXTM-LN.

      * SKIP CONFIDENTIAL  AND NON-SOLICIT ACCOUNTS:
      * THIS CHECK ALSO IN STORED PROCEDURE 
           IF BW-SOLICIT = "N" OR BW-CONFIDENTIAL = "Y"
              GO TO NEXT-TEXTM-LN.

      * SKIP FORMER BORROWER IF BORROWER HAS CURRENT LOANS
           IF TEXTM-OPEN-FORMER-FG = "F"
              IF BW-LOANS > 0
                 GO TO NEXT-TEXTM-LN.

      * SKIP BORROWER IF MORE THAN ONE LOAN EXISTS
           IF TEXTM-SKIP-MULT-ACCT = "Y"
              IF BW-LOANS > 1
                 GO TO NEXT-TEXTM-LN.

      * CHECK BORROWER TEXT OPT IN
           IF TEXTM-OPTIN1 NOT = SPACES
              IF BW-PRIVACY-OPT-OUT = TEXTM-OPTIN1
                 GO TO NEXT-TEXTM-LN.
           IF TEXTM-OPTIN2 NOT = SPACES
              IF BW-PRIVACY-OPT-OUT = TEXTM-OPTIN2
                 GO TO NEXT-TEXTM-LN.
           IF TEXTM-OPTIN3 NOT = SPACES
              IF BW-PRIVACY-OPT-OUT = TEXTM-OPTIN3
                 GO TO NEXT-TEXTM-LN.
           IF TEXTM-OPTIN4 NOT = SPACES
              IF BW-PRIVACY-OPT-OUT = TEXTM-OPTIN4
                 GO TO NEXT-TEXTM-LN.

      * TEST PERCENT THROUGH TERM OF LOAN 
           IF TEXTM-PCT-TERM = 0
              GO TO TEST-NEXT-TEXTM-PARAMETER.

           COMPUTE WORK-MONTHS ROUNDED =
              (LN-SUM-ORGTERM * TEXTM-PCT-TERM) / 100 + .5.
           MOVE LN-1STPYDATE TO CEPP1-DATE.
           MOVE TRANS-DATE   TO CEPP2-DATE.
           PERFORM CONTRACTUAL-ELAPSED-PAYPER.
           IF CEPP-PERIODS < WORK-MONTHS
              GO TO NEXT-TEXTM-LN.

       TEST-NEXT-TEXTM-PARAMETER.
           MOVE "N" TO FIRST-TIME.

           PERFORM CREATE-WORK-FILE.

           GO TO NEXT-TEXTM-LN.

       END-OF-GROUP.
           PERFORM CLOSE-LM1-FILE.
           PERFORM OPEN-TEXT-SORT-FILE-OUTPUT.

           IF PRINT-TOTALS = "Y"
              IF REC-COUNT NOT = 0
                 MOVE "BRANCH #:" TO D-BRANCH-PROMPT
                 MOVE BR-NO TO D-BRNO
                 MOVE "NO OF ACCOUNTS:" TO D-ACCTS-PROMPT
                 MOVE REC-COUNT TO D-COUNT
                 MOVE 2 TO LN-FEED
                 PERFORM WRITE-DETAIL-LINE.

           ADD REC-COUNT TO TOTAL-COUNT.
           MOVE 0 TO REC-COUNT.

      *     GO TO NEXT-GROUP-LOC.

       END-ROUTINE.
           IF FIRST-TIME = "Y"
              MOVE "N" TO RECORDS-FOUND
              GO TO END-PROGRAM.

      * PRINT GRAND TOTAL:
      
           IF PRINT-TOTALS = "Y"
              MOVE "** TOTAL ** " TO D-BRANCH-PROMPT
              MOVE 0 TO D-BRNO
              MOVE "NO OF ACCOUNTS:" TO D-ACCTS-PROMPT
              MOVE TOTAL-COUNT TO D-COUNT
              MOVE 4 TO LN-FEED
              PERFORM WRITE-DETAIL-LINE.

           PERFORM PRINT-RANGE-LINES.

           GO TO END-PROGRAM.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************
           MOVE PITBO2-QUEUE-POS TO Q-POS.
           MOVE PITBO2-COPIES-POS TO C-POS.
           MOVE PITBO2-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
       COPY "LIBGB/EOINIT.CPY".

           IF BEG-BRANCH = 0
              IF END-BRANCH = 0
                 MOVE BEG-BRANCH TO SV-BEG-BR
                 MOVE END-BRANCH TO SV-END-BR
                 MOVE EXT-CONAME-USER-LOC TO BEG-BRANCH
                                             END-BRANCH.

           MOVE 0 TO REC-COUNT CURRENT-COUNT.

           IF EXT-ROUTE-BUF = "00"
              IF PRU-LP NOT = "VDU" AND NOT = "PRU"
                 MOVE SPACES TO WR-BUF
                 MOVE "PROGRAM IN PROGRESS......" TO WR-BUF
                 MOVE MESS-LIST TO WR-LIST
                 PERFORM CALL-WRFORM.

      * DO NOT OPEN WORK FILES IF PRINTING REPORT ONLY.
      
           PERFORM SET-WORK-PATH.
           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           IF STAT NOT = "00"
              MOVE "CANNOT MAKE SORT KEY FILE" TO MESS
              PERFORM SEND-MESS
              GO TO ENDIT.

           MOVE MKTEMP-BUF TO TEXT-SORT-PATH.
           PERFORM OPEN-TEXT-SORT-FILE-OUTPUT.

           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-BR-FILE.
      * MUST LEAVE SP1-FILE OPEN BECAUSE ADDONSPR READS IT
           PERFORM OPEN-SP1-FILE.

           PERFORM OPEN-PR-FILE.

           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           PERFORM OPEN-MISC-CONNECTION.
           PERFORM SQL-OPEN-UPDATE-CONNECTION.

           PERFORM ARRAYSP-LOAD.

           GO TO EXIT-INITIALIZE.

       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.
       EXIT-INITIALIZE.
           EXIT.

      ******************************************************************
       EO-EXEC-AUDIT SECTION.
      ******************************************************************
           MOVE EXT-EOBUF-DATE1 TO DATE-YYYYMMDD
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
           MOVE DATE-MMDDYY TO TEXTM-BEG-DATE
           MOVE EXT-EOBUF-DATE2 TO  DATE-YYYYMMDD
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
           MOVE DATE-MMDDYY TO TEXTM-END-DATE.

      ******************************************************************
       ENTER-PARAMETERS SECTION.
      ******************************************************************
       ENTER-PARM.

           IF VDUSTATUS = "1U"
              GO TO TOTAL-YN.

       BR-01.
           MOVE LEGEND-BR-01 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A000BR1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-BR.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.

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
              MOVE 0 TO WGR-SRESULT
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
              GO TO ENTER-OPTIONS.

      *-----------------------------------------------------------------
       BRANCH-IS-NUMERIC.
           MOVE "N" TO GROUP-FLAG.
           MOVE WGR-SRESULT TO BEG-BRANCH.
           MOVE "SN N040BR1." TO VDU.
           MOVE BEG-BRANCH TO VDUNUM0.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       BR-02.
           MOVE LEGEND-BR-02 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN040BR2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BR-02.
           MOVE VDUNUM0 TO END-BRANCH.

           IF BEG-BRANCH > END-BRANCH
              MOVE "INVALID BRANCH RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO BR-01.

           GO TO ENTER-OPTIONS.

      *-----------------------------------------------------------------
       ALL-BR.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "N" TO GROUP-FLAG.

       ENTER-OPTIONS.
           PERFORM OPEN-TEXTM-SOL-WINDOW.

           MOVE "S  A000TITLE." TO VDU
           MOVE "TEXT MARKETING (OTHER) EXTRACTION"  TO VDUBUF
           PERFORM SCREENIO.

           MOVE "S  A000MINAVPROMPT." TO VDU
           MOVE SPACES TO VDUBUF
           PERFORM SCREENIO.
           MOVE "S  A000MINAV." TO VDU
           MOVE SPACES TO VDUBUF
           PERFORM SCREENIO.

      * IF MODIFYING PARAMETERS IN BATCH THEN DISPLAY EOBUF, ELSE
      * SHOW DEFAULTS.
      
           IF EXT-ROUTE-BUF = "50"
              PERFORM DISPLAY-TEXTM-SOL-FIELDS.

       TEXTM-REC-OPEN-FORMER.
           MOVE "ENTER 'O' FOR OPEN ACCOUNTS, 'F' FOR FORMER:"
                                                       TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A010OPEN." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO WINDOW-EXIT-PARM.
           IF VDUSTATUS = "10" GO TO WINDOW-EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO TEXTM-REC-OPEN-FORMER.
           MOVE VDUBUF TO TEXTM-OPEN-FORMER-FG.

           IF NOT (TEXTM-OPEN-FORMER-FG = "O" OR "F")
              MOVE "PLEASE ENTER 'O' OR 'F'" TO MESS
              PERFORM SEND-MESS
              GO TO TEXTM-REC-OPEN-FORMER.
           IF TEXTM-OPEN-FORMER-FG = "O"
              MOVE "S  A000DATE1PROMPT." TO VDU
              MOVE "BEGINNING LOAN DATE   :" TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A000DATE2PROMPT." TO VDU
              MOVE "ENDING    LOAN DATE   :" TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "S  A000DATE1PROMPT." TO VDU
              MOVE "BEGINNING PAYOFF DATE :" TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A000DATE2PROMPT." TO VDU
              MOVE "ENDING    PAYOFF DATE :" TO VDUBUF
              PERFORM SCREENIO.
      *----------------------------------------------------------------
      *BEGINNNING LOAN DATE:
      *----------------------------------------------------------------
       TEXTM-REC-LNDT1.
           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO TEXTM-CL-01.
           IF TEXTM-OPEN-FORMER-FG = "O"
              MOVE "ENTER BEGINNING LOAN DATE, F3-ALL:" TO LEGEND
           ELSE
              MOVE "ENTER BEGINNING PAYOFF DATE, F3-ALL:" TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ERNM060LNDT1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO WINDOW-EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO WINDOW-EXIT-PARM.
           IF VDUSTATUS = "16" GO TO TEXTM-ALL-LNDT.
           IF VDUSTATUS = "1U" GO TO TEXTM-REC-OPEN-FORMER.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO TEXTM-REC-LNDT1.
           MOVE VDU-DATE-YYYYMMDD TO NUM-DATE.
           PERFORM TSTDAT.
           IF NUM-DATE = 0 GO TO TEXTM-REC-LNDT1.
      * TEXTM-BEG-DATE MUST STAY 99/99/99 UNTIL SCREEN INPUT CHANGES!
           MOVE VDUNUM0 TO TEXTM-BEG-DATE.
       TEXTM-REC-LNDT2.
           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO TEXTM-CL-01.

           IF TEXTM-OPEN-FORMER-FG = "O"
              MOVE "ENTER ENDING LOAN DATE:" TO LEGEND
           ELSE
              MOVE "ENTER ENDING PAYOFF DATE:" TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ERNM060LNDT2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO WINDOW-EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO WINDOW-EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO TEXTM-REC-LNDT1.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO TEXTM-REC-LNDT2.
           MOVE VDU-DATE-YYYYMMDD TO NUM-DATE.
           PERFORM TSTDAT.
           IF NUM-DATE = 0 GO TO TEXTM-REC-LNDT2.
      * TEXTM-END-DATE MUST STAY 99/99/99 UNTIL SCREEN INPUT CHANGES!
           MOVE VDUNUM0 TO TEXTM-END-DATE.

      * MUST STAY LIKE THIS! THESE DATES ARE 99/99/99 AND CANT CHANGE
           MOVE TEXTM-BEG-DATE TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO NUM-DATE.
           MOVE TEXTM-END-DATE TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO SYS-DATE.
           IF SYS-DATE < NUM-DATE
              GO TO TEXTM-REC-LNDT1.
           GO TO TEXTM-END-LNDT.
  
       TEXTM-ALL-LNDT.
           MOVE "S  A080LNDT1." TO VDU.
           MOVE EXT-JULIAN-BEG TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO TEXTM-BEG-DATE VDU-DATE-EDIT.
           PERFORM SCREENIO.
           MOVE "S  A080LNDT2." TO VDU.
           MOVE EXT-JULIAN-END TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO TEXTM-END-DATE VDU-DATE-EDIT.
           PERFORM SCREENIO.
       TEXTM-END-LNDT.
      *-----------------------------------------------------------------
       TEXTM-CL-01.
           MOVE "ENTER BEGINNING LOAN CLASS:" TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN020CL1." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO WINDOW-EXIT-PARM.
           IF VDUSTATUS = "10" GO TO WINDOW-EXIT-PARM.
           IF VDUSTATUS = "16" GO TO TEXTM-ALL-CL.
           IF VDUSTATUS = "1U" GO TO TEXTM-REC-LNDT2.
           IF VDUSTATUS = "1<"
              MOVE "BEG" TO CLASS-FG
              PERFORM CLASS-SCAN
              IF IO-FG NOT = 0
                 GO TO TEXTM-CL-01. 
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO TEXTM-CL-01.

           MOVE VDUNUM0 TO TEXTM-BEG-CLASS.

      *----------------------------------------------------------------
       TEXTM-CL-02.
           MOVE "ENTER ENDING LOAN CLASS:" TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN020CL2." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO WINDOW-EXIT-PARM.
           IF VDUSTATUS = "10" GO TO WINDOW-EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO TEXTM-CL-01.
           IF VDUSTATUS = "1<"
              MOVE "END" TO CLASS-FG
              PERFORM CLASS-SCAN
              IF IO-FG NOT = 0
                 GO TO TEXTM-CL-02.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO TEXTM-CL-02.

           MOVE VDUNUM0 TO TEXTM-END-CLASS.

           IF TEXTM-BEG-CLASS > TEXTM-END-CLASS
              MOVE "INVALID CLASS RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO TEXTM-CL-01.

           GO TO TEXTM-REC-MINTERM.
      *----------------------------------------------------------------
       TEXTM-ALL-CL.
           MOVE "SNNN020CL1." TO VDU.
           MOVE 0             TO TEXTM-BEG-CLASS VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020CL2." TO VDU.
           MOVE 99            TO TEXTM-END-CLASS VDUNUM0.
           PERFORM SCREENIO.
      *----------------------------------------------------------------
      *BEGIN LOAN TERM:
      *----------------------------------------------------------------
       TEXTM-REC-MINTERM.
           MOVE "ENNN030MINTERM." TO VDU.
           MOVE "ENTER MINIMUM ORIGINAL TERM, F3-ALL:" TO LEGEND.
           PERFORM SEND-LEGEND.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO WINDOW-EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO WINDOW-EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO TEXTM-CL-02.
           IF VDUSTATUS = "16" GO TO TEXTM-ALL-TERM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO TEXTM-REC-MINTERM.
           MOVE VDUNUM0 TO TEXTM-MINTERM.
       TEXTM-REC-MAXTERM.
           MOVE "ENTER MAXIMUM ORIGINAL TERM:" TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN030MAXTERM." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO WINDOW-EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO WINDOW-EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO TEXTM-REC-MINTERM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO TEXTM-REC-MAXTERM.
           MOVE VDUNUM0 TO TEXTM-MAXTERM.
           IF TEXTM-MINTERM > TEXTM-MAXTERM
              GO TO TEXTM-REC-MINTERM.
           GO TO TEXTM-END-TERM.
       TEXTM-ALL-TERM.
           MOVE "SNNN030MINTERM." TO VDU.
           MOVE 0 TO TEXTM-MINTERM VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN030MAXTERM." TO VDU.
           MOVE 999 TO TEXTM-MAXTERM VDUNUM0.
           PERFORM SCREENIO.
       TEXTM-END-TERM.
      *----------------------------------------------------------------
      *DAYS TO NEXT PAY:
      *----------------------------------------------------------------
       TEXTM-REC-MINNXTPAY.
           MOVE "ENNN020MINNXTPAY." TO VDU.
           MOVE
           "ENTER MINIMUM DAYS UNTIL NEXT PAYMENT DUE, F3-ALL:"
                                                     TO LEGEND.
           PERFORM SEND-LEGEND.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO WINDOW-EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO WINDOW-EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO TEXTM-REC-MAXTERM.
           IF VDUSTATUS = "16" GO TO TEXTM-ALL-NXTPAY.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO TEXTM-REC-MINNXTPAY.
           MOVE VDUNUM0 TO TEXTM-MINNXTPAY.

       TEXTM-REC-MAXNXTPAY.
           MOVE "ENTER MAXIMUM DAYS UNTIL NEXT PAYMENT DUE:" TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN020MAXNXTPAY." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO WINDOW-EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO WINDOW-EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO TEXTM-REC-MINNXTPAY.
           IF VDUSTATUS = "16" GO TO TEXTM-ALL-NXTPAY.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO TEXTM-REC-MAXNXTPAY.
           MOVE VDUNUM0 TO TEXTM-MAXNXTPAY.
           IF TEXTM-MINNXTPAY > TEXTM-MAXNXTPAY
              GO TO TEXTM-REC-MINNXTPAY.
           GO TO TEXTM-END-NXTPAY.
  
       TEXTM-ALL-NXTPAY.
           MOVE "SNNN020MINNXTPAY." TO VDU.
           MOVE 0 TO TEXTM-MINNXTPAY VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020MAXNXTPAY." TO VDU.
           MOVE 99 TO TEXTM-MAXNXTPAY VDUNUM0.
           PERFORM SCREENIO.
       TEXTM-END-NXTPAY.
      *----------------------------------------------------------------
      *DAYS LAST PAYMENT:
      *----------------------------------------------------------------
       TEXTM-REC-MINLSTPAY.
           MOVE "ENNN020MINLSTPAY." TO VDU.
           MOVE
           "ENTER MINIMUM DAYS SINCE LAST PAYMENT MADE, F3-ALL:"
                                                     TO LEGEND.
           PERFORM SEND-LEGEND.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO WINDOW-EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO WINDOW-EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO TEXTM-REC-MAXNXTPAY.
           IF VDUSTATUS = "16" GO TO TEXTM-ALL-LSTPAY.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO TEXTM-REC-MINLSTPAY.
           MOVE VDUNUM0 TO TEXTM-MINLSTPAY.
       TEXTM-REC-MAXLSTPAY.
           MOVE
           "ENTER MAXIMUM DAYS SINCE LAST PAYMENT MADE, F3-ALL:"
                                                     TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN020MAXLSTPAY." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO WINDOW-EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO WINDOW-EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO TEXTM-REC-MINLSTPAY.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO TEXTM-REC-MAXLSTPAY.
           MOVE VDUNUM0 TO TEXTM-MAXLSTPAY.
           IF TEXTM-MINLSTPAY > TEXTM-MAXLSTPAY
              GO TO TEXTM-REC-MINLSTPAY.
           GO TO TEXTM-END-LSTPAY.
  
       TEXTM-ALL-LSTPAY.
           MOVE "SNNN020MINLSTPAY." TO VDU.
           MOVE ZERO TO TEXTM-MINLSTPAY VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020MAXLSTPAY." TO VDU.
           MOVE 999 TO TEXTM-MAXLSTPAY VDUNUM0.
           PERFORM SCREENIO.
       TEXTM-END-LSTPAY.

       TEXTM-REC-OPTIN1.
           MOVE "ENTER TEXT OPTIN CODE TO SKIP" TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A020OPTIN1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO WINDOW-EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO WINDOW-EXIT-PARM.
           IF VDUSTATUS = "1U"
                 GO TO TEXTM-REC-MAXLSTPAY.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO TEXTM-REC-OPTIN1.
           MOVE VDUBUF TO TEXTM-OPTIN1.
           IF TEXTM-OPTIN1 = SPACES
              MOVE SPACES TO TEXTM-OPTIN-BUF
              MOVE TEXTM-OPTIN-BUF TO WR-BUF
              MOVE TEXT-OPTIN-LIST TO WR-LIST
              PERFORM CALL-WRFORM
              GO TO TEXTM-REC-SRCD01.
       TEXTM-REC-OPTIN2.
           MOVE "ENTER TEXT OPTIN CODE TO SKIP" TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A020OPTIN2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO WINDOW-EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO WINDOW-EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO TEXTM-REC-OPTIN1.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO TEXTM-REC-OPTIN2.
           MOVE VDUBUF TO TEXTM-OPTIN2.
           IF TEXTM-OPTIN2 = SPACES
              MOVE SPACES TO TEXTM-OPTIN2 TEXTM-OPTIN3 TEXTM-OPTIN4
              MOVE TEXTM-OPTIN-BUF TO WR-BUF
              MOVE TEXT-OPTIN-LIST TO WR-LIST
              PERFORM CALL-WRFORM
              GO TO TEXTM-REC-SRCD01.
       TEXTM-REC-OPTIN3.
           MOVE "ENTER TEXT OPTIN CODE TO SKIP" TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A020OPTIN3." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO WINDOW-EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO WINDOW-EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO TEXTM-REC-OPTIN2.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO TEXTM-REC-OPTIN3.
           MOVE VDUBUF TO TEXTM-OPTIN3.
           IF TEXTM-OPTIN3 = SPACES
              MOVE SPACES TO TEXTM-OPTIN3 TEXTM-OPTIN4
              MOVE TEXTM-OPTIN-BUF TO WR-BUF
              MOVE TEXT-OPTIN-LIST TO WR-LIST
              PERFORM CALL-WRFORM
              GO TO TEXTM-REC-SRCD01.
       TEXTM-REC-OPTIN4.
           MOVE "ENTER TEXT OPTIN CODE TO SKIP" TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A020OPTIN4." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO WINDOW-EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO WINDOW-EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO TEXTM-REC-OPTIN3.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO TEXTM-REC-OPTIN4.
           MOVE VDUBUF TO TEXTM-OPTIN4.
           IF TEXTM-OPTIN4 = SPACES
              MOVE TEXTM-OPTIN-BUF TO WR-BUF
              MOVE TEXT-OPTIN-LIST TO WR-LIST
              PERFORM CALL-WRFORM
              GO TO TEXTM-REC-SRCD01.

       TEXTM-REC-SRCD01.
           MOVE "ENTER LOAN SOURCE CODE TO SKIP" TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A020SRCD01." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO WINDOW-EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO WINDOW-EXIT-PARM.
           IF VDUSTATUS = "1U"
              GO TO TEXTM-REC-OPTIN1.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO TEXTM-REC-SRCD01.
           MOVE VDUBUF TO TEXTM-SRCD01.
           IF TEXTM-SRCD01 = SPACES
              MOVE SPACES TO TEXTM-SRCD-BUF
              MOVE TEXTM-SRCD-BUF TO WR-BUF
              MOVE TEXTM-SRCD-LIST TO WR-LIST
              PERFORM CALL-WRFORM
              GO TO TEXTM-REC-XMON.
       TEXTM-REC-SRCD02.
           MOVE "ENTER LOAN SOURCE CODE TO SKIP" TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A020SRCD02." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO WINDOW-EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO WINDOW-EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO TEXTM-REC-SRCD01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO TEXTM-REC-SRCD02.
           MOVE VDUBUF TO TEXTM-SRCD02.
           IF TEXTM-SRCD02 = SPACES
              MOVE SPACES TO TEXTM-SRCD02 TEXTM-SRCD03 TEXTM-SRCD04
                             TEXTM-SRCD05 TEXTM-SRCD06 TEXTM-SRCD07
                             TEXTM-SRCD08 TEXTM-SRCD09 TEXTM-SRCD10
              MOVE TEXTM-SRCD-BUF TO WR-BUF
              MOVE TEXTM-SRCD-LIST TO WR-LIST
              PERFORM CALL-WRFORM
              GO TO TEXTM-REC-XMON.
       TEXTM-REC-SRCD03.
           MOVE "ENTER LOAN SOURCE CODE TO SKIP" TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A020SRCD03." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO WINDOW-EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO WINDOW-EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO TEXTM-REC-SRCD02.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO TEXTM-REC-SRCD03.
           MOVE VDUBUF TO TEXTM-SRCD03.
           IF TEXTM-SRCD03 = SPACES
              MOVE SPACES TO TEXTM-SRCD03 TEXTM-SRCD04 TEXTM-SRCD05
                             TEXTM-SRCD06 TEXTM-SRCD07 TEXTM-SRCD08
                             TEXTM-SRCD09 TEXTM-SRCD10
              MOVE TEXTM-SRCD-BUF TO WR-BUF
              MOVE TEXTM-SRCD-LIST TO WR-LIST
              PERFORM CALL-WRFORM
              GO TO TEXTM-REC-XMON.
       TEXTM-REC-SRCD04.
           MOVE "ENTER LOAN SOURCE CODE TO SKIP" TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A020SRCD04." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO WINDOW-EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO WINDOW-EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO TEXTM-REC-SRCD03.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO TEXTM-REC-SRCD04.
           MOVE VDUBUF TO TEXTM-SRCD04.
           IF TEXTM-SRCD04 = SPACES
              MOVE SPACES TO TEXTM-SRCD04 TEXTM-SRCD05 TEXTM-SRCD06
                             TEXTM-SRCD07 TEXTM-SRCD08 TEXTM-SRCD09
                             TEXTM-SRCD10
              MOVE TEXTM-SRCD-BUF TO WR-BUF
              MOVE TEXTM-SRCD-LIST TO WR-LIST
              PERFORM CALL-WRFORM
              GO TO TEXTM-REC-XMON.
       TEXTM-REC-SRCD05.
           MOVE "ENTER LOAN SOURCE CODE TO SKIP" TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A020SRCD05." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO WINDOW-EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO WINDOW-EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO TEXTM-REC-SRCD04.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO TEXTM-REC-SRCD05.
           MOVE VDUBUF TO TEXTM-SRCD05.
           IF TEXTM-SRCD05 = SPACES
              MOVE SPACES TO TEXTM-SRCD05 TEXTM-SRCD06 TEXTM-SRCD07
                             TEXTM-SRCD08 TEXTM-SRCD09 TEXTM-SRCD10
              MOVE TEXTM-SRCD-BUF TO WR-BUF
              MOVE TEXTM-SRCD-LIST TO WR-LIST
              PERFORM CALL-WRFORM
              GO TO TEXTM-REC-XMON.
       TEXTM-REC-SRCD06.
           MOVE "ENTER LOAN SOURCE CODE TO SKIP" TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A020SRCD06." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO WINDOW-EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO WINDOW-EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO TEXTM-REC-SRCD05.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO TEXTM-REC-SRCD06.
           MOVE VDUBUF TO TEXTM-SRCD06.
           IF TEXTM-SRCD06 = SPACES
              MOVE SPACES TO TEXTM-SRCD06 TEXTM-SRCD07 TEXTM-SRCD08
                             TEXTM-SRCD09 TEXTM-SRCD10
              MOVE TEXTM-SRCD-BUF TO WR-BUF
              MOVE TEXTM-SRCD-LIST TO WR-LIST
              PERFORM CALL-WRFORM
              GO TO TEXTM-REC-XMON.
       TEXTM-REC-SRCD07.
           MOVE "ENTER LOAN SOURCE CODE TO SKIP" TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A020SRCD07." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO WINDOW-EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO WINDOW-EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO TEXTM-REC-SRCD06.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO TEXTM-REC-SRCD07.
           MOVE VDUBUF TO TEXTM-SRCD07.
           IF TEXTM-SRCD07 = SPACES
              MOVE SPACES TO TEXTM-SRCD07 TEXTM-SRCD08 TEXTM-SRCD09
                             TEXTM-SRCD10
              MOVE TEXTM-SRCD-BUF TO WR-BUF
              MOVE TEXTM-SRCD-LIST TO WR-LIST
              PERFORM CALL-WRFORM
              GO TO TEXTM-REC-XMON.
       TEXTM-REC-SRCD08.
           MOVE "ENTER LOAN SOURCE CODE TO SKIP" TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A020SRCD08." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO WINDOW-EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO WINDOW-EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO TEXTM-REC-SRCD07.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO TEXTM-REC-SRCD08.
           MOVE VDUBUF TO TEXTM-SRCD08.
           IF TEXTM-SRCD08 = SPACES
              MOVE SPACES TO TEXTM-SRCD08 TEXTM-SRCD09 TEXTM-SRCD10
              MOVE TEXTM-SRCD-BUF TO WR-BUF
              MOVE TEXTM-SRCD-LIST TO WR-LIST
              PERFORM CALL-WRFORM
              GO TO TEXTM-REC-XMON.
       TEXTM-REC-SRCD09.
           MOVE "ENTER LOAN SOURCE CODE TO SKIP" TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A020SRCD09." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO WINDOW-EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO WINDOW-EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO TEXTM-REC-SRCD08.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO TEXTM-REC-SRCD09.
           MOVE VDUBUF TO TEXTM-SRCD09.
           IF TEXTM-SRCD09 = SPACES
              MOVE SPACES TO TEXTM-SRCD09 TEXTM-SRCD10
              MOVE TEXTM-SRCD-BUF TO WR-BUF
              MOVE TEXTM-SRCD-LIST TO WR-LIST
              PERFORM CALL-WRFORM
              GO TO TEXTM-REC-XMON.
       TEXTM-REC-SRCD10.
           MOVE "ENTER LOAN SOURCE CODE TO SKIP" TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A020SRCD10." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO WINDOW-EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO WINDOW-EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO TEXTM-REC-SRCD09.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO TEXTM-REC-SRCD10.
           MOVE VDUBUF TO TEXTM-SRCD10.
           IF TEXTM-SRCD10 = SPACES
              MOVE SPACES TO TEXTM-SRCD10
              MOVE TEXTM-SRCD-BUF TO WR-BUF
              MOVE TEXTM-SRCD-LIST TO WR-LIST
              PERFORM CALL-WRFORM
              GO TO TEXTM-REC-XMON.

      *-----------------------------------------------------------------
      * NUMBER OF MONTHS TO VIEW PROFILES
      *-----------------------------------------------------------------
       TEXTM-REC-XMON.
           MOVE "ENTER NUMBER OF MONTHS TO VIEW PROFILES:" TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN020XMON." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO WINDOW-EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO WINDOW-EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO TEXTM-REC-SRCD01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO TEXTM-REC-XMON.
           MOVE VDUNUM0 TO TEXTM-XMON.
           IF TEXTM-XMON = 0 OR > 12
              MOVE "ENTER 1 THRU 12" TO MESS
              PERFORM SEND-MESS
              GO TO TEXTM-REC-XMON.
 
      *-----------------------------------------------------------------
      * HIGH CONTRACTUAL RATING
      *-----------------------------------------------------------------
       TEXTM-REC-HCON.
           MOVE "ENTER HIGHEST CONTRACTUAL PROFILE RATING ALLOWED:"
                                                            TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN030HCON." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO WINDOW-EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO WINDOW-EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO TEXTM-REC-XMON.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO TEXTM-REC-HCON.
           MOVE VDUNUM0 TO TEXTM-HCON.
           DIVIDE 30 INTO VDUNUM0.
           MULTIPLY 30 BY VDUNUM0.
           IF VDUNUM0 NOT = TEXTM-HCON
              MOVE "MUST BE '0' OR A MULTIPLE OF 30" TO MESS
              PERFORM SEND-MESS
              GO TO TEXTM-REC-HCON.
 
      *-----------------------------------------------------------------
      * HIGH RECENCY RATING
      *-----------------------------------------------------------------
       TEXTM-REC-HREC.
           MOVE "ENTER HIGHEST RECENCY PROFILE RATING ALLOWED:"
                                                            TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN030HREC." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO WINDOW-EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO WINDOW-EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO TEXTM-REC-HCON.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO TEXTM-REC-HREC.
           MOVE VDUNUM0 TO TEXTM-HREC.
           DIVIDE 30 INTO VDUNUM0.
           MULTIPLY 30 BY VDUNUM0.
           IF VDUNUM0 NOT = TEXTM-HREC
              MOVE "MUST BE '0' OR A MULTIPLE OF 30" TO MESS
              PERFORM SEND-MESS
              GO TO TEXTM-REC-HREC.
       TEXTM-REC-CPOT-ISO.
           MOVE "CONTRACTUAL POTENTIALS, INCLUDE, SKIP, ONLY?:" 
                                                       TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A010CPOT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO WINDOW-EXIT-PARM.
           IF VDUSTATUS = "10" GO TO WINDOW-EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO TEXTM-REC-HREC.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO TEXTM-REC-CPOT-ISO.
           MOVE VDUBUF TO TEXTM-CPOT.
           IF NOT (TEXTM-CPOT = "I" OR "S" OR "O")
              GO TO TEXTM-REC-CPOT-ISO.
       TEXTM-REC-RPOT-ISO.
           MOVE "RECENCY POTENTIALS, INCLUDE, SKIP, ONLY?:" TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A010RPOT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO WINDOW-EXIT-PARM.
           IF VDUSTATUS = "10" GO TO WINDOW-EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO TEXTM-REC-CPOT-ISO.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO TEXTM-REC-RPOT-ISO.
           MOVE VDUBUF TO TEXTM-RPOT.
           IF NOT (TEXTM-RPOT = "I" OR "S" OR "O")
              GO TO TEXTM-REC-RPOT-ISO.

      * AVAILABLE CREDIT:
      
           MOVE 0 TO TEXTM-MINAV.

       TEXTM-REC-SKIP-MULT-ACCT.
           MOVE "SKIP BORROWERS WITH MULTIPLE ACCOUNTS  (Y/N):"
                                                        TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A010MULTACCT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO WINDOW-EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO TEXTM-REC-RPOT-ISO.
           IF VDUSTATUS = "1D" GO TO TEXTM-REC-CONT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO TEXTM-REC-CONT.
           MOVE VDUBUF TO TEXTM-SKIP-MULT-ACCT.
           IF TEXTM-SKIP-MULT-ACCT NOT = "Y" 
              IF TEXTM-SKIP-MULT-ACCT NOT = "N" 
                 GO TO TEXTM-REC-SKIP-MULT-ACCT.

       TEXTM-REC-PCTTERM.
           MOVE "ENNN020PCTTERM." TO VDU.
           MOVE "ENTER PERCENT INTO TERM:" TO LEGEND.
           PERFORM SEND-LEGEND.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO WINDOW-EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO WINDOW-EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO TEXTM-REC-SKIP-MULT-ACCT.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO TEXTM-REC-PCTTERM.
           MOVE VDUNUM0 TO TEXTM-PCT-TERM.

      *----------------------------------------------------------------
      * TEXTM, FILE TYPE IDENTIFIER
      *----------------------------------------------------------------
       TEXTM-REC-FILE-TYPE.
           MOVE "ENTER FILE TYPE IDENTIFIER :" TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A010TYPE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO WINDOW-EXIT-PARM.
           IF VDUSTATUS = "10" GO TO WINDOW-EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO TEXTM-REC-PCTTERM.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO TEXTM-REC-FILE-TYPE.
           IF VDUBUF = "M"
              MOVE "'M' IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO TEXTM-REC-FILE-TYPE.
           MOVE VDUBUF TO TEXTM-FILE-TYPE.

      * TEXTM, MEMO (ONLY IF FILE TYPE NOT = "M"
      
       TEXTM-REC-MEMO.
           MOVE "ENTER MEMO :" TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A200MEMO." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO WINDOW-EXIT-PARM.
           IF VDUSTATUS = "10" GO TO WINDOW-EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO TEXTM-REC-FILE-TYPE.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO TEXTM-REC-MEMO.
           MOVE VDUBUF TO TEXTM-MEMO.
       TEXTM-REC-CONT.
           MOVE "CONTINUE  (Y/N):" TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A010CONT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO WINDOW-EXIT-PARM.
           IF VDUSTATUS = "1U"
              IF TEXTM-FILE-TYPE = "M"
              GO TO TEXTM-REC-FILE-TYPE
           ELSE
              GO TO TEXTM-REC-MEMO.
           IF VDUSTATUS = "1D" GO TO TEXTM-REC-CONT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO TEXTM-REC-CONT.
           MOVE VDUBUF TO TEXTM-CONT.
           IF TEXTM-CONT = "Y" 
              PERFORM CLOSE-WINDOW-ROUTINE
              GO TO TOTAL-YN.

           GO TO TEXTM-REC-CONT.

       TOTAL-YN.
           MOVE "PRINT RECORD COUNTS BY BRANCH (Y/N):" TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A010TOTAL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO TOTAL-YN.
           MOVE VDUBUF TO PRINT-TOTALS.
           IF PRINT-TOTALS  = "Y" OR "N"
              GO TO DETAIL-YN.
           GO TO TOTAL-YN.

       DETAIL-YN.
           MOVE "E  A010DETAIL." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO TOTAL-YN.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO DETAIL-YN.

           MOVE VDUBUF TO PRINT-DETAIL.

           IF NOT (PRINT-DETAIL = "Y" OR "N")
              GO TO DETAIL-YN.

       REPORT-ONLY-YN.
           MOVE "E  A010REPORTONLY." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO DETAIL-YN.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO REPORT-ONLY-YN.

           MOVE VDUBUF TO REPORT-ONLY.

           IF NOT (REPORT-ONLY = "Y" OR "N")
              GO TO REPORT-ONLY-YN.

       END-PARM.
           MOVE LEGEND-ACCEPT TO LEGEND.
           PERFORM SEND-LEGEND.
           GO TO EXIT-PARM.

       WINDOW-EXIT-PARM.
           PERFORM CLOSE-WINDOW-ROUTINE.
       EXIT-PARM.
           EXIT.

      ******************************************************************
       DISPLAY-PARAMETERS SECTION.
      ******************************************************************
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
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010TOTAL." TO VDU.
           MOVE PRINT-TOTALS TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010DETAIL." TO VDU.
           MOVE PRINT-DETAIL TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010REPORTONLY." TO VDU.
           MOVE REPORT-ONLY TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      ******************************************************************
       CREATE-WORK-FILE SECTION.
      ******************************************************************

           MOVE SPACES          TO TEXT-SORT-REC.
           PERFORM FIX-FIRST-NAME.
           MOVE EDIT-FNAME      TO TEXT-SORT-FNAME.
           MOVE BW-LNAME        TO TEXT-SORT-LNAME.
           MOVE BW-CELL-PHONE   TO TEXT-SORT-CELL.
           MOVE LN-OWNBR        TO TEXT-SORT-OWNBR.
           MOVE LN-ACCTNO       TO TEXT-SORT-ACCTNO.
           MOVE GB-COMPANY-NAME TO TEXT-SORT-BR-NAME.
           MOVE GB-PHONE        TO TEXT-SORT-BR-PHONE.

           MOVE "M"    TO TEXT-SORT-TYPE
           MOVE 0      TO TEXT-SORT-AVAILCR
           MOVE 0      TO TEXT-SORT-1ST-DUE
           MOVE SPACES TO TEXT-SORT-DUE-EXT.

           PERFORM WRITE-TEXT-SORT-FILE.

      * WE DO NOT WANT DUPLICATE RECORDS, SO SAME NAME & ZIP WILL
      * GET SKIPPED AS A DUPLICATE KEY; BUT DON'T ADD TO COUNT OR
      * PRINT DETAIL UNLESS WE KNOW WE WROTE SUCCESSFULLY:

           IF IO-GOOD
              ADD 1 TO REC-COUNT
              IF PRINT-DETAIL = "Y"
                 PERFORM CREATE-DETAIL-LINE.

           MOVE "N" TO FIRST-TIME.

       EXIT-CREATE-WORK-FILE.
           EXIT.

      ******************************************************************
       PROCESS-WORK-FILE SECTION.
      ******************************************************************

           IF REPORT-ONLY = "Y"
              GO TO EXIT-PROCESS-WORK-FILE.

           PERFORM OPEN-TEXT-SORT-FILE.
           MOVE SPACES TO TEXT-SORT-KEY.
           PERFORM START-TEXT-SORT-FILE.

       NEXT-TEXT-SORT-FILE.
           PERFORM READ-TEXT-SORT-FILE-NEXT.
           IF IO-FG = 9
              GO TO EXIT-PROCESS-WORK-FILE.

           MOVE SPACES             TO TEXT-OUT-REC.
           MOVE TEXTM-FILE-TYPE    TO TEXT-OUT-TYPE.
           MOVE TEXT-SORT-OWNBR    TO TEXT-OUT-OWNBR.
           MOVE TEXT-SORT-ACCTNO   TO TEXT-OUT-ACCTNO.
           MOVE TEXT-SORT-CELL     TO TEXT-OUT-CELL.
           MOVE TEXT-SORT-LNAME    TO TEXT-OUT-LNAME.
           MOVE TEXT-SORT-FNAME    TO TEXT-OUT-FNAME.
           MOVE TEXT-SORT-BR-NAME  TO TEXT-OUT-BR-NAME.
           MOVE TEXT-SORT-BR-PHONE TO TEXT-OUT-BR-PHONE.
           MOVE 0                  TO TEXT-OUT-AVAILCR.
           MOVE TEXT-SORT-1ST-DUE  TO TEXT-OUT-1ST-DUE.
           MOVE TEXT-SORT-DUE-EXT  TO TEXT-OUT-DUE-EXT.

           MOVE X"0D" TO TEXT-OUT-CR.
           MOVE X"0A" TO TEXT-OUT-LF.
          
           ADD 1 TO TEXT-OUT-KEY.
           PERFORM WRITE-TEXT-OUT-FILE.
           MOVE "N" TO FIRST-TIME.
           ADD 1 TO CURRENT-COUNT.

           PERFORM CREATE-TEXT-MEMO.

           GO TO NEXT-TEXT-SORT-FILE.
           
       EXIT-PROCESS-WORK-FILE.
           EXIT.

      ******************************************************************
       SET-WORK-PATH SECTION.
      ******************************************************************
           IF REPORT-ONLY = "Y"
              GO TO SET-WORK-PATH-EXIT.

           MOVE EXT-FILPATH-BASE     TO TEXT-OUT-PATH-BASE.
           MOVE EXT-FILPATH-MACHINE  TO TEXT-OUT-PATH-MACHINE.
           IF GROUP-FLAG = "Y"
              MOVE ENT-GROUP         TO TEXT-OUT-PATH-GROUP
           ELSE
              MOVE BEG-BRANCH        TO TEXT-OUT-PATH-BRNO.

      * MUST STRING OUTPUT FILE PATH TO REMOVE SPACES IF GROUP IS LESS
      * THAN 4 CHARACTERS

           MOVE SPACES TO TEXT-OUT-PATH.
           STRING TEXT-OUT-PATH-FIXED
                  TEXT-OUT-PATH-GROUP
                  TEXT-OUT-PATH-MM
                  TEXT-OUT-PATH-DD
                  TEXT-OUT-PATH-YY
                  TEXT-OUT-PATH-DOT
                  TEXT-OUT-PATH-REPORT
              DELIMITED BY " " INTO TEXT-OUT-PATH.

           MOVE TEXT-OUT-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT NOT = "00"
              PERFORM OPEN-TEXT-OUT-FILE-OUTPUT
              MOVE 0 TO TEXT-OUT-KEY
              MOVE 0 TO CURRENT-COUNT
              GO TO SET-WORK-PATH-EXIT.

           IF EXT-ROUTE-BUF NOT = "00"
              MOVE "E" TO EXT-EOBUF-ERRCD
              MOVE "FILE ALREADY EXISTS" TO MESS
              PERFORM SEND-MESS
              GO TO SET-WORK-PATH-EXIT.
           
       SET-TEXT-OUT-PATH-ASK.
           PERFORM ALARM.
           MOVE "FILE ALREADY EXISTS, CONTINUE (Y/N):" TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "R RA010ACCEPT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS NOT = "00"
              GO TO SET-TEXT-OUT-PATH-ASK.
           IF VDUBUF = "N"
              MOVE "E" TO EXT-EOBUF-ERRCD
              GO TO SET-WORK-PATH-EXIT.
           IF VDUBUF NOT = "Y"
              GO TO SET-TEXT-OUT-PATH-ASK.
           PERFORM OPEN-TEXT-OUT-FILE-OUTPUT.

           MOVE 0 TO TEXT-OUT-KEY.
           MOVE SPACES TO LEGEND.
           PERFORM SEND-LEGEND.
           GO TO SET-WORK-PATH-EXIT.

       SET-WORK-PATH-EXIT.
           EXIT.

      ****************************
       CREATE-DETAIL-LINE SECTION.
      ****************************

           MOVE SPACES TO D-NAME.
           STRING TEXT-SORT-FNAME," ",BW-LNAME DELIMITED BY "   "
                                               INTO D-NAME.
           IF BW-CELL-PHONE NOT = 0
              MOVE BW-CELL-PHONE TO D-PHONE
               INSPECT D-PHONE REPLACING ALL "/" BY "-".

           MOVE SPACES TO HEAD4-1ST-DUE
           MOVE SPACES TO HEAD4-AVAIL

           MOVE LN-ACCTNO TO D-NUMBER.

           PERFORM WRITE-DETAIL-LINE.


      ****************************
       WRITE-DETAIL-LINE SECTION.
      ****************************
           ADD LN-FEED LN-COUNT GIVING LN-WK.
           IF LN-WK > EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.
           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      *******************************
       HEAD-PAGE SECTION.
      *******************************
           MOVE "N" TO FIRST-TIME.
           ADD 1 TO PAGE-NUMBER.
           MOVE PAGE-NUMBER TO H-PAGE-NO.
           PERFORM GET-TIME.
           MOVE T-HH TO H-HR.
           MOVE T-MM TO H-MIN.

           MOVE HEAD1 TO PR-REC.
           MOVE 99 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE " TEXT MARKETING EXTRACTION (OTHER)" TO H-EXTRACT.
           MOVE HEAD2 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           IF PRINT-DETAIL = "Y"
              MOVE HEAD3 TO PR-REC
              MOVE 1 TO LN-FEED
              PERFORM WRITE-PR-REC
              MOVE HEAD4-TEXT TO PR-REC
              MOVE 2 TO LN-FEED
              PERFORM WRITE-PR-REC.

       END-HEAD-PAGE.
           IF PRINT-DETAIL = "Y"
              MOVE 5 TO LN-COUNT
           ELSE
              MOVE 2 TO LN-COUNT.
           MOVE 2 TO LN-FEED.

      ******************************************************************
       PRINT-RANGE-LINES SECTION.
      ******************************************************************
           MOVE EXT-CONAME-RPT-LINES           TO LN-COUNT.
           MOVE 4                      TO LN-FEED.

           MOVE "EXTRACTION FORMAT       :"    TO RANGE-DESC.
           MOVE  "TEXT FORMAT "        TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *--------------------------------------------------------------
           IF GROUP-FLAG = "Y"
              MOVE "SELECTED GROUP          :" TO RANGE-DESC
              MOVE ENT-GROUP                   TO RANGE-ALP1
           ELSE
              MOVE "BRANCH NUMBER RANGE     :" TO RANGE-DESC
              MOVE BEG-BRANCH                  TO RANGE-NUM1
              MOVE END-BRANCH                  TO RANGE-NUM2.
           PERFORM WRITE-DETAIL-LINE.
      *--------------------------------------------------------------
           MOVE "EXTRACTION TYPE         :"       TO RANGE-DESC.
           MOVE  "TEXT MARKETING EXTRACTION (OTHER)"
                                                  TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *--------------------------------------------------------------
           MOVE "PRINT RECORD COUNTS     :"    TO RANGE-DESC.
           MOVE PRINT-TOTALS                   TO RANGE-ALP1.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "PRINT DETAIL            :"    TO RANGE-DESC.
           MOVE PRINT-DETAIL                   TO RANGE-ALP1.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "REPORT ONLY             :"    TO RANGE-DESC.
           MOVE REPORT-ONLY                    TO RANGE-ALP1.
           PERFORM WRITE-DETAIL-LINE.
      *-------------------------------------------------------------
           MOVE "OPEN OR FORMER          :" TO RANGE-DESC
           MOVE TEXTM-OPEN-FORMER-FG        TO RANGE-ALP1
           PERFORM WRITE-DETAIL-LINE.

              MOVE "LOAN DATE RANGE         :" TO RANGE-DESC
      *        MOVE TEXTM-BEG-DATE             TO RANGE-DATE1
      *        MOVE TEXTM-END-DATE             TO RANGE-DATE2
              MOVE TEXTM-BEG-DATE     TO DATE-MMDDYY.
              PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
              MOVE DATE-YYYYMMDD              TO RANGE-DATE1.
              MOVE TEXTM-END-DATE     TO DATE-MMDDYY.
              PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
              MOVE DATE-YYYYMMDD              TO RANGE-DATE2.

              PERFORM WRITE-DETAIL-LINE
              MOVE "CLASS RANGE             :" TO RANGE-DESC
              MOVE TEXTM-BEG-CLASS             TO RANGE-NUM1
              MOVE TEXTM-END-CLASS             TO RANGE-NUM2
              PERFORM WRITE-DETAIL-LINE
              MOVE "LOAN TERM RANGE         :" TO RANGE-DESC
              MOVE TEXTM-MINTERM               TO RANGE-NUM1
              MOVE TEXTM-MAXTERM               TO RANGE-NUM2
              PERFORM WRITE-DETAIL-LINE
              MOVE "BEG/END DAYS TO NEXT PAY:" TO RANGE-DESC
              MOVE TEXTM-MINNXTPAY             TO RANGE-NUM1
              MOVE TEXTM-MAXNXTPAY             TO RANGE-NUM2
              PERFORM WRITE-DETAIL-LINE
              MOVE "BEG/END DAYS TO LAST PAY:" TO RANGE-DESC
              MOVE TEXTM-MINLSTPAY             TO RANGE-NUM1
              MOVE TEXTM-MAXLSTPAY             TO RANGE-NUM2
              PERFORM WRITE-DETAIL-LINE
              MOVE "SOURCE CODES TO SKIP    :" TO RANGE-DESC
              MOVE TEXTM-SRCD01                TO RANGE-TAB(1)
              MOVE TEXTM-SRCD02                TO RANGE-TAB(2)
              MOVE TEXTM-SRCD03                TO RANGE-TAB(3)
              MOVE TEXTM-SRCD04                TO RANGE-TAB(4)
              MOVE TEXTM-SRCD05                TO RANGE-TAB(5)
              MOVE TEXTM-SRCD06                TO RANGE-TAB(6)
              MOVE TEXTM-SRCD07                TO RANGE-TAB(7)
              MOVE TEXTM-SRCD08                TO RANGE-TAB(8)
              MOVE TEXTM-SRCD09                TO RANGE-TAB(9)
              MOVE TEXTM-SRCD10                TO RANGE-TAB(10)
              PERFORM WRITE-DETAIL-LINE
              MOVE "TEXT OPTIN CODES TO SKIP:" TO RANGE-DESC
              MOVE TEXTM-OPTIN1                TO RANGE-TAB(1)
              MOVE TEXTM-OPTIN2                TO RANGE-TAB(2)
              MOVE TEXTM-OPTIN3                TO RANGE-TAB(3)
              MOVE TEXTM-OPTIN4                TO RANGE-TAB(4)
              PERFORM WRITE-DETAIL-LINE
              MOVE "LAST 'X' MONTHS PROFILE :" TO RANGE-DESC
              MOVE TEXTM-XMON                  TO RANGE-NUM1
              PERFORM WRITE-DETAIL-LINE
              MOVE "HIGH CONTRACTUAL/RECENCY:" TO RANGE-DESC
              MOVE TEXTM-HCON                  TO RANGE-ALP1
              MOVE TEXTM-HREC                  TO RANGE-ALP2
              MOVE "SKP/INC/ONLY CPOTS,RPOTS:" TO RANGE-DESC
              MOVE TEXTM-CPOT                  TO RANGE-ALP1
              MOVE TEXTM-RPOT                  TO RANGE-ALP2
              PERFORM WRITE-DETAIL-LINE
              MOVE "SKIP MULITPLE ACCOUNTS  :" TO RANGE-DESC
              MOVE TEXTM-SKIP-MULT-ACCT        TO RANGE-ALP1
              PERFORM WRITE-DETAIL-LINE
              MOVE "PERCENT INTO TERM       :" TO RANGE-DESC
              MOVE TEXTM-PCT-TERM              TO RANGE-NUM1
              PERFORM WRITE-DETAIL-LINE
              MOVE "FILE TYPE IDENTIFIER    :" TO RANGE-DESC
              MOVE TEXTM-FILE-TYPE             TO RANGE-ALP1
              PERFORM WRITE-DETAIL-LINE
              MOVE "MEMO                    :" TO RANGE-DESC
              MOVE TEXTM-MEMO                  TO RANGE-ALP1
              PERFORM WRITE-DETAIL-LINE.

           MOVE 2                      TO LN-FEED.
           MOVE "OUTPUT FILE NAME        :"    TO RANGE-DESC.
           MOVE TEXT-OUT-PATH               TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.


      *************************************************
       OPEN-TEXTM-SOL-WINDOW SECTION.
      *************************************************
           MOVE 22 TO WINDOW-LINES.
           MOVE 78 TO WINDOW-COLUMNS.
           MOVE 2 TO WINDOW-BEGIN-Y.
           MOVE 2 TO WINDOW-BEGIN-X.
           MOVE "SP/PITBO3" TO WINDOW-FORM-NAM.
           MOVE "PITBO3" TO VDU-FORM-NAME.
           PERFORM OPEN-WINDOW.
           DISPLAY PITBO3-SCREEN.

       EXIT-OPEN-TEXTM-SOL-WINDOW.
           EXIT.

      *******************************************
      *    TEST PROFILES:
      *
      *    0 - CUR       6 - 180 TO 269
      *    1 - 30        7 - 270+
      *    2 - 60        8 - NOT USED
      *    3 - 90        9 - MONTH NOT GRADED
      *    4 - 120
      *    5 - 150
      *******************************************
       TEST-TEXTM-PROFILE SECTION.
      *******************************************
           MOVE "Y" TO PROFILE-OK.
           DIVIDE 30 INTO PROFILE-HIGH.
           IF PROFILE-HIGH > 6 
              IF PROFILE-HIGH < 9
                 MOVE 6 TO PROFILE-HIGH
              ELSE
                 MOVE 7 TO PROFILE-HIGH.
           MOVE 1 TO SUB.
       TEST-TEXTM-PROFILE-AGAIN.
           IF SUB > TEXTM-XMON OR > 12
              GO TO TEST-TEXTM-PROFILE-EXIT.
           IF PROFILE-TAB(SUB) = 9
              GO TO TEST-TEXTM-PROFILE-EXIT.
           IF PROFILE-TAB(SUB) NOT > PROFILE-HIGH
              ADD 1 TO SUB
              GO TO TEST-TEXTM-PROFILE-AGAIN.
           MOVE " " TO PROFILE-OK.
       TEST-TEXTM-PROFILE-EXIT.
           EXIT.

      ******************************************************************
      * FIX FIRST NAME, IF 2 FULL NAMES, EG MARY JANE, KEEP BOTH,
      *                 IF 1ST & INITIAL, EG MARY J, JUST KEEP MARY
      ******************************************************************
       FIX-FIRST-NAME SECTION.
           MOVE SPACES TO EDIT-FNAME.
           MOVE BW-FNAME TO WS-FNAME.
           MOVE 0 TO SUB.
       FIX-NAME-LOOP1.
           ADD 1 TO SUB.
           IF SUB > 16 GO TO EXIT-FIX-NAME.
           IF WS-FNAME-CHAR(SUB) NOT = " "
                 MOVE WS-FNAME-CHAR(SUB) TO EDIT-FNAME-CHAR(SUB)
                 GO TO FIX-NAME-LOOP1.
           IF WS-FNAME-CHAR(SUB + 1) NOT = " "
              IF WS-FNAME-CHAR(SUB + 2) = " "
                 GO TO EXIT-FIX-NAME.
           IF WS-FNAME-CHAR(SUB + 1) NOT = " "
              IF WS-FNAME-CHAR(SUB + 2) = "."
                 GO TO EXIT-FIX-NAME.
       FIX-NAME-LOOP2.
           ADD 1 TO SUB.
           IF SUB > 16 GO TO EXIT-FIX-NAME.
           IF WS-FNAME-CHAR(SUB) NOT = " "
              MOVE WS-FNAME-CHAR(SUB) TO EDIT-FNAME-CHAR(SUB).
              GO TO FIX-NAME-LOOP2.
       EXIT-FIX-NAME.
           EXIT.

      *************************************************
       CREATE-TEXT-MEMO SECTION.
      *************************************************

      * CREATE MEMO FOR TEXT MARKETING OR PMT REMINDERS
           MOVE TEXT-SORT-ACCTNO TO LM-ACCTNO.
           SUBTRACT TRANS-DATE FROM 99999999 GIVING LM-DATE.
           MOVE 0 TO LM-SEQ.

           PERFORM READ-LM1-FILE-MAX-SEQ.

       CREATE-MEMO-NEXT.
           IF LM-SEQ = 999
              GO TO CREATE-MEMO-EXIT.

           ADD 1 TO LM-SEQ.
      *     PERFORM READ-LM1-FILE.
      *     IF IO-FG = 0
      *       UNLOCK LM-FILE
      *        GO TO CREATE-MEMO-NEXT.

           MOVE TEXT-SORT-OWNBR TO LM-BRNO.
           MOVE "9 "            TO LM-MEMO-TYPE.
           MOVE " " TO MESS.
           MOVE SPACES TO LM-MEMO
           STRING TEXTM-MEMO, " TEXT SENT"
                        DELIMITED BY "  " INTO LM-MEMO.

           PERFORM WRITE-LM1-FILE.
           IF IO-FG = 9
              PERFORM IO-ERROR.

       CREATE-MEMO-EXIT.
           EXIT.

      *****************************************************************
       CLASS-SCAN SECTION.
      *****************************************************************
           MOVE 0   TO LCSCAN-KEY.
           MOVE " " TO LCSCAN-VERIFY.
           MOVE "WI/LCSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH LCSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF LCSCAN-STATUS NOT = "00" OR LCSCAN-KEY = 0
               MOVE 9 TO IO-FG
               GO TO EXIT-CLASS-SCAN.
           IF ( CLASS-FG = "BEG" )
              MOVE "SNNN020CL1." TO VDU
           ELSE
              MOVE "SNNN020CL2." TO VDU.
           MOVE LCSCAN-KEY TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE LCSCAN-KEY TO VDUNUM0.
           MOVE 0 TO IO-FG.

       EXIT-CLASS-SCAN.
           EXIT.

      ***************************************
       BRANCH-BREAK SECTION.
      ***************************************
           IF PROC-BRANCH-NO > 0
              PERFORM PROCESS-WORK-FILE
              PERFORM END-OF-GROUP.

           IF NEXT-BRANCH-NO = 0 
              GO TO BRANCH-BREAK-EXIT.

           MOVE NEXT-BRANCH-NO TO BR-NO.
           PERFORM READ-BR-GB-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.
           
           MOVE BR-MACHINE   TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH TO FILPATH-DATA.
           
           IF EXT-ROUTE-BUF = "00"
              IF PRU-LP NOT = "VDU" AND NOT = "PRU"
                 MOVE SPACES TO WR-BUF
                 STRING "PROGRAM IN PROGRESS......BRNO: " BR-NO
                       DELIMITED BY SIZE INTO WR-BUF
                 MOVE MESS-LIST TO WR-LIST
                 PERFORM CALL-WRFORM.

           IF PRINT-DETAIL = "Y"
              MOVE BR-NO   TO H-BRNO
              MOVE BR-NAME TO H-BRDESC
              MOVE EXT-CONAME-RPT-LINES TO LN-COUNT.

           PERFORM CLOSE-LM1-FILE.
           PERFORM OPEN-LM1-FILE.

       BRANCH-BREAK-EXIT.
           EXIT.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBWI/PFSWINDOWS.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/SPPDTH.CPY".
       COPY "LIBLP/LPCEPP.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBLP/LPSCHD.CPY".
       COPY "LIBGB/AGEING.CPY".
       COPY "LIBLP/LPPDUE.CPY".
       COPY "LIBLP/ARRAYSP.CPY".

      ************************************************
       FORM-RESET SECTION.
      ************************************************
           MOVE EXT-CONAME-CONAME TO PITBO2-CONAME.
           MOVE EXT-CONAME-SYSDATE TO PITBO2-SYSDATE.
           DISPLAY PITBO2-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE PITBO2-FORM-FIELDS TO WR-BUF.
           MOVE PITBO2-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       FORM-RESET-EXIT.
           EXIT.

      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE PITBO2-HELP--FILE TO HELP-LINK-FILE.
           MOVE PITBO2-HELP--DIR TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/PITBO2_EVA.CPY".
              COPY "LIBSP/PITBO3_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

     ******************************************************************
       OPEN-MISC-CONNECTION SECTION.
      ******************************************************************

           IF MISC-CONN-OPEN-SW = "N" 
               EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS MISC_CONNECT
                    USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
               END-EXEC
               MOVE "Y" TO MISC-CONN-OPEN-SW
               EXEC SQL SET CONNECTION C1 END-EXEC
               PERFORM SQL-IO-VALIDATION.

      ******************************************************************
       CLOSE-MISC-CONNECTION SECTION.
      ******************************************************************

           IF MISC-CONN-OPEN-SW = "Y"
               EXEC SQL SET CONNECTION MISC_CONNECT END-EXEC
               EXEC SQL COMMIT END-EXEC
               EXEC SQL DISCONNECT MISC_CONNECT END-EXEC
               EXEC SQL SET CONNECTION C1 END-EXEC
               MOVE "N" TO MISC-CONN-OPEN-SW.

      ******************************************************************
       MISC-PARA SECTION.
      ******************************************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
      *-----------------------------------------------------------------
       CLOSE-WINDOW-ROUTINE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN 
           DESTROY PITBO3-SCREEN.
           PERFORM CLOSE-WINDOW.
           MOVE "PITBO2" TO VDU-FORM-NAME.
       GET-GB.
           PERFORM OPEN-GB-FILE.
           MOVE BR-NO TO GB-BRNO.
           MOVE 1     TO GB-NO.
           PERFORM READ-GB-FILE.
           PERFORM CLOSE-GB-FILE.
      *-----------------------------------------------------------------
       GET-SPR.
           MOVE LN-ORGST    TO SP-ORGST.
           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           MOVE LN-LAWCODE  TO SP-LAWCODE.
           IF LPWSSP-SP1-KEY NOT = SP1-KEY
              MOVE SP1-KEY TO LPWSSP-SP1-KEY
      *        PERFORM READ-SP1-FILE
              PERFORM ARRAYSP-READ
              IF IO-FG NOT = 0
                 GO TO IO-ERROR.

      *-----------------------------------------------------------------
       DISPLAY-TEXTM-SOL-FIELDS.
           MOVE TEXTM-LOAN-FIELDS TO TEXTM-BUF.
           MOVE TEXTM-BUF TO WR-BUF.
           MOVE TEXTM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
      *-----------------------------------------------------------------
       SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE MSEL-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLTPGS_SQL.CPY".
       COPY "LIBLP/LPLTIGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLMGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBGBGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".

       COPY "LIBGB/GRBRLS_SQL.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBLP/LPLTI1RN.CPY".
       COPY "LIBLP/LPLTP1RN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBGB/GBGBRN.CPY".


      * COPY "LIBLP/LPLM1IN.CPY".
      * COPYMEMBER: LIBLP/LPLM1IN
       LOAD-LM1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( LM-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( LM-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO LM-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO LM-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO LM-ALL-DATA
              MOVE LM-ALL-PATH             TO LM-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( LM-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO LM-ALL-BASE
              MOVE FILPATH-MACHINE         TO LM-ALL-MACHINE
              MOVE FILPATH-DATA            TO LM-ALL-DATA
              MOVE LM-ALL-PATH             TO LM-PATH.

      *-----------------------------------------------------------------
       OPEN-LM1-FILE.
           PERFORM LOAD-LM1-FILE.
           PERFORM OPEN-IT.
           MOVE LM-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       READ-LM1-FILE.
           PERFORM READ-IT.
           MOVE LM-PATH-OWNBR  TO LM-BRNO. 
           MOVE LM-PATH-SQL    TO E-FILE.
           MOVE LM1-KEY        TO E-KEYX.
           INITIALIZE QLM-REC.

           MOVE LM-BRNO          TO QLM-BRNO.
           MOVE LM-ACCTNO        TO QLM-ACCTNO.
           MOVE LM-DATE-REVERSED TO QLM-DATE-REVERSED.
           MOVE LM-SEQ           TO QLM-SEQ.

           EXEC SQL
            SELECT LMFILE.LM_BRNO,
                   LMFILE.LM_ACCTNO,
                   LMFILE.LM_DATE_REVERSED,
                   LMFILE.LM_SEQ,
                   CAST(LMFILE.LM_LTOUCH_DATE AS VARCHAR(10)),
                   LMFILE.LM_LTOUCH_TIME_HHMM,
                   LMFILE.LM_LTOUCH_USERID,
                   LMFILE.LM_MEMO,
                   LMFILE.LM_MEMO_TYPE
            INTO :QLM-REC
            FROM DBO.LMFILE
            WHERE LMFILE.LM_BRNO   = :QLM-BRNO
              AND LMFILE.LM_ACCTNO = :QLM-ACCTNO
              AND LMFILE.LM_DATE_REVERSED = :QLM-DATE-REVERSED
              AND LMFILE.LM_SEQ = :QLM-SEQ
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LM1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-LM-FIELDS.

      *-----------------------------------------------------------------
       READ-LM1-FILE-MAX-SEQ.
           PERFORM READ-IT.
           MOVE LM-PATH-OWNBR  TO LM-BRNO. 
           MOVE LM-PATH-SQL    TO E-FILE.
           MOVE LM1-KEY        TO E-KEYX.
           INITIALIZE QLM-REC.

           MOVE LM-BRNO          TO QLM-BRNO.
           MOVE LM-ACCTNO        TO QLM-ACCTNO.
           MOVE LM-DATE-REVERSED TO QLM-DATE-REVERSED.

           EXEC SQL
            SELECT MAX(LMFILE.LM_SEQ)
            INTO :QLM-SEQ
            FROM DBO.LMFILE
            WHERE LMFILE.LM_BRNO   = :QLM-BRNO
              AND LMFILE.LM_ACCTNO = :QLM-ACCTNO
              AND LMFILE.LM_DATE_REVERSED = :QLM-DATE-REVERSED
            GROUP BY LMFILE.LM_BRNO, LMFILE.LM_ACCTNO,
                     LMFILE.LM_DATE_REVERSED
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 ) 
              GO TO READ-LM1-FILE-MAX-SEQ.

           IF ( IO-FG = 0 )
              MOVE QLM-SEQ TO LM-SEQ
           ELSE
              MOVE 0       TO LM-SEQ.

      *-----------------------------------------------------------------
       WRITE-LM1-FILE. 

           MOVE LM-PATH-OWNBR  TO LM-BRNO.
           ACCEPT LM-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE LM-PATH-SQL    TO E-FILE.
           MOVE LM1-KEY        TO E-KEYX.
           PERFORM SET-LM-FIELDS.

      * CHANGE CONNECTION TO UPDATE CONNECT
           MOVE '2' TO SQL-UPDATE-CONN-USE-SW.   
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

           EXEC SQL
            INSERT INTO DBO.LMFILE
             (LMFILE.LM_BRNO,
              LMFILE.LM_ACCTNO,
              LMFILE.LM_DATE_REVERSED,
              LMFILE.LM_SEQ,
              LMFILE.LM_LTOUCH_DATE,
              LMFILE.LM_LTOUCH_TIME_HHMM,
              LMFILE.LM_LTOUCH_USERID,
              LMFILE.LM_MEMO,
              LMFILE.LM_MEMO_TYPE)
            VALUES
             (:QLM-BRNO,
              :QLM-ACCTNO,
              :QLM-DATE-REVERSED,
              :QLM-SEQ,
              :QLM-LTOUCH-DATE,
              :QLM-LTOUCH-TIME-HHMM,
              :QLM-LTOUCH-USERID,
              :QLM-MEMO,
              :QLM-MEMO-TYPE)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

      * CHANGE CONNECTION BACK TO C1
           MOVE '1' TO SQL-UPDATE-CONN-USE-SW. 
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

           IF ( IO-FG = 8 ) 
              GO TO WRITE-LM1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       CLOSE-LM1-FILE.

           PERFORM CLOSE-IT.

      * COPY "LIBLP/LPLN3RN.CPY".
      * COPYMEMBER: LIBLP/LPLN3RN
      *-----------------------------------------------------------------
      *    STILL NEED TO LOAD THE FILE PATH WORKERS
      *-----------------------------------------------------------------
       LOAD-LN3-FILE.
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
       OPEN-LN3-FILE.
           PERFORM LOAD-LN3-FILE.
           PERFORM OPEN-IT.
           MOVE LN-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-LN3-FILE.  *> SSP - LN BW
           PERFORM START-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-PATH-SQL    TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING LN-OWNBR, LN-CLASS,LN-ACCTNO
              DELIMITED BY " " INTO E-KEYX.

           IF ( LN3-CURSOR-STAT-GOOD )
              PERFORM CLOSE-LN3-FILE.

      * CREATE TABLE #TEMP1_BRANCH_GROUP_TBL
      *   ( SEQ INT IDENTITY(1,1),
      *     BRNO INT ); 

      * INSERT INTO #TEMP1_BRANCH_GROUP_TBL (BRNO)
      *  SELECT P.VALUE FROM STRING_SPLIT(@BRANCHLIST, ',') P;

      *     SELECT LNFILE.LN_OWNBR,
      *            LNFILE.LN_ACCTNO,
      *            LNFILE.LN_CLASS,
      *            LNFILE.LN_SUBCLASS,
      *            LNFILE.LN_LAWCODE,
      *            LNFILE.LN_SSNO1,
      *            LNFILE.LN_MAKERCD1,
      *            LNFILE.LN_SSNO2,
      *            LNFILE.LN_MAKERCD2,
      *            LNFILE.LN_SSNO3,
      *            LNFILE.LN_MAKERCD3,
      *            LNFILE.LN_SSNO4,
      *            LNFILE.LN_MAKERCD4,
      *            LNFILE.LN_ORGST,
      *            LNFILE.LN_SPRCLASS,
      *            LNFILE.LN_CONVERCD,
      *            LNFILE.LN_SRCD,
      *            LNFILE.LN_POCD,
      *            LNFILE.LN_PLCD,
      *            LNFILE.LN_CONFIDENTCD,
      *            LNFILE.LN_ALLOTCD,
      *            LNFILE.LN_STATUSFG,
      *            LNFILE.LN_SALEFIN,
      *            LNFILE.LN_LOANTYPE,
      *            LNFILE.LN_ADDON_FG,
      *            LNFILE.LN_PAY_SCHLD_FG,
      *            LNFILE.LN_LC_CODE,
      *            CAST(LNFILE.LN_LOANDATE AS VARCHAR(10)),
      *            CAST(LNFILE.LN_PURDATE AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INTDATE AS VARCHAR(10)),
      *            CAST(LNFILE.LN_CASHDATE AS VARCHAR(10)),
      *            CAST(LNFILE.LN_ORIG_1STPYDATE AS VARCHAR(10)),
      *            CAST(LNFILE.LN_1STPYDATE AS VARCHAR(10)),
      *            CAST(LNFILE.LN_JDDATE AS VARCHAR(10)),
      *            CAST(LNFILE.LN_PLDATE AS VARCHAR(10)),
      *            CAST(LNFILE.LN_ACCELDATE AS VARCHAR(10)),
      *            CAST(LNFILE.LN_REDUDATE AS VARCHAR(10)),
      *            CAST(LNFILE.LN_POFFDATE AS VARCHAR(10)),
      *            CAST(LNFILE.LN_LEGALDATE AS VARCHAR(10)),
      *            CAST(LNFILE.LN_BNKRPTDATE AS VARCHAR(10)),
      *            CAST(LNFILE.LN_DEBTAIDATE AS VARCHAR(10)),
      *            CAST(LNFILE.LN_REPODATE AS VARCHAR(10)),
      *            LNFILE.LN_APRATE,
      *            LNFILE.LN_EFFRATE,
      *            LNFILE.LN_SMPRATE,
      *            LNFILE.LN_OVRDRATE,
      *            LNFILE.LN_ENDORATE,
      *            LNFILE.LN_JDRATE,
      *            LNFILE.LN_ACCRATE,
      *            LNFILE.LN_REDURATE,
      *            LNFILE.LN_LNAMT,
      *            LNFILE.LN_FINCHG,
      *            LNFILE.LN_INTCHG,
      *            LNFILE.LN_SERCHG,
      *            LNFILE.LN_EXTCHG,
      *            LNFILE.LN_MAINTFEE,
      *            LNFILE.LN_POINTS,
      *            LNFILE.LN_ORGTERM,
      *            LNFILE.LN_1STPYAMT,
      *            LNFILE.LN_NOREGPY,
      *            LNFILE.LN_REGPYAMT,
      *            LNFILE.LN_LASTPYAMT,
      *            LNFILE.LN_FEECODE_1,
      *            LNFILE.LN_FEECODE_2,
      *            LNFILE.LN_FEECODE_3,
      *            LNFILE.LN_FEECODE_4,
      *            LNFILE.LN_FEECODE_5,
      *            LNFILE.LN_FEECODE_6,
      *            LNFILE.LN_FEECODE_7,
      *            LNFILE.LN_FEECODE_8,
      *            LNFILE.LN_FEECODE_9,
      *            LNFILE.LN_FEECODE_10,
      *            LNFILE.LN_FEECODE_11,
      *            LNFILE.LN_FEECODE_12,
      *            LNFILE.LN_FEECODE_13,
      *            LNFILE.LN_FEECODE_14,
      *            LNFILE.LN_FEECODE_15,
      *            LNFILE.LN_FEECODE_16,
      *            LNFILE.LN_FEECODE_17,
      *            LNFILE.LN_FEECODE_18,
      *            LNFILE.LN_FEECODE_19,
      *            LNFILE.LN_FEECODE_20,
      *            LNFILE.LN_FEEAMT_1,
      *            LNFILE.LN_FEEAMT_2,
      *            LNFILE.LN_FEEAMT_3,
      *            LNFILE.LN_FEEAMT_4,
      *            LNFILE.LN_FEEAMT_5,
      *            LNFILE.LN_FEEAMT_6,
      *            LNFILE.LN_FEEAMT_7,
      *            LNFILE.LN_FEEAMT_8,
      *            LNFILE.LN_FEEAMT_9,
      *            LNFILE.LN_FEEAMT_10,
      *            LNFILE.LN_FEEAMT_11,
      *            LNFILE.LN_FEEAMT_12,
      *            LNFILE.LN_FEEAMT_13,
      *            LNFILE.LN_FEEAMT_14,
      *            LNFILE.LN_FEEAMT_15,
      *            LNFILE.LN_FEEAMT_16,
      *            LNFILE.LN_FEEAMT_17,
      *            LNFILE.LN_FEEAMT_18,
      *            LNFILE.LN_FEEAMT_19,
      *            LNFILE.LN_FEEAMT_20,
      *            LNFILE.LN_FEEOURS_1,
      *            LNFILE.LN_FEEOURS_2,
      *            LNFILE.LN_FEEOURS_3,
      *            LNFILE.LN_FEEOURS_4,
      *            LNFILE.LN_FEEOURS_5,
      *            LNFILE.LN_FEEOURS_6,
      *            LNFILE.LN_FEEOURS_7,
      *            LNFILE.LN_FEEOURS_8,
      *            LNFILE.LN_FEEOURS_9,
      *            LNFILE.LN_FEEOURS_10,
      *            LNFILE.LN_FEEOURS_11,
      *            LNFILE.LN_FEEOURS_12,
      *            LNFILE.LN_FEEOURS_13,
      *            LNFILE.LN_FEEOURS_14,
      *            LNFILE.LN_FEEOURS_15,
      *            LNFILE.LN_FEEOURS_16,
      *            LNFILE.LN_FEEOURS_17,
      *            LNFILE.LN_FEEOURS_18,
      *            LNFILE.LN_FEEOURS_19,
      *            LNFILE.LN_FEEOURS_20,
      *            LNFILE.LN_FEEFINCHG_1,
      *            LNFILE.LN_FEEFINCHG_2,
      *            LNFILE.LN_FEEFINCHG_3,
      *            LNFILE.LN_FEEFINCHG_4,
      *            LNFILE.LN_FEEFINCHG_5,
      *            LNFILE.LN_FEEFINCHG_6,
      *            LNFILE.LN_FEEFINCHG_7,
      *            LNFILE.LN_FEEFINCHG_8,
      *            LNFILE.LN_FEEFINCHG_9,
      *            LNFILE.LN_FEEFINCHG_10,
      *            LNFILE.LN_FEEFINCHG_11,
      *            LNFILE.LN_FEEFINCHG_12,
      *            LNFILE.LN_FEEFINCHG_13,
      *            LNFILE.LN_FEEFINCHG_14,
      *            LNFILE.LN_FEEFINCHG_15,
      *            LNFILE.LN_FEEFINCHG_16,
      *            LNFILE.LN_FEEFINCHG_17,
      *            LNFILE.LN_FEEFINCHG_18,
      *            LNFILE.LN_FEEFINCHG_19,
      *            LNFILE.LN_FEEFINCHG_20,
      *            LNFILE.LN_INSURED,
      *            LNFILE.LN_INSTYPES,
      *            LNFILE.LN_INSCOMP_1,
      *            LNFILE.LN_INSCOMP_2,
      *            LNFILE.LN_INSCOMP_3,
      *            LNFILE.LN_INSCOMP_4,
      *            LNFILE.LN_INSCOMP_5,
      *            LNFILE.LN_INSCOMP_6,
      *            LNFILE.LN_INSCOMP_7,
      *            LNFILE.LN_INSCOMP_8,
      *            LNFILE.LN_INSCOMP_9,
      *            LNFILE.LN_INSCOMP_10,
      *            LNFILE.LN_INSOURS_1,
      *            LNFILE.LN_INSOURS_2,
      *            LNFILE.LN_INSOURS_3,
      *            LNFILE.LN_INSOURS_4,
      *            LNFILE.LN_INSOURS_5,
      *            LNFILE.LN_INSOURS_6,
      *            LNFILE.LN_INSOURS_7,
      *            LNFILE.LN_INSOURS_8,
      *            LNFILE.LN_INSOURS_9,
      *            LNFILE.LN_INSOURS_10,
      *            CAST(LNFILE.LN_INSEFF_DATE_1 AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEFF_DATE_2 AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEFF_DATE_3 AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEFF_DATE_4 AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEFF_DATE_5 AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEFF_DATE_6 AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEFF_DATE_7 AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEFF_DATE_8 AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEFF_DATE_9 AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEFF_DATE_10 AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEXP_DATE_1 AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEXP_DATE_2 AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEXP_DATE_3 AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEXP_DATE_4 AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEXP_DATE_5 AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEXP_DATE_6 AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEXP_DATE_7 AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEXP_DATE_8 AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEXP_DATE_9 AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEXP_DATE_10 AS VARCHAR(10)),
      *            LNFILE.LN_INSPREM_1,
      *            LNFILE.LN_INSPREM_2,
      *            LNFILE.LN_INSPREM_3,
      *            LNFILE.LN_INSPREM_4,
      *            LNFILE.LN_INSPREM_5,
      *            LNFILE.LN_INSPREM_6,
      *            LNFILE.LN_INSPREM_7,
      *            LNFILE.LN_INSPREM_8,
      *            LNFILE.LN_INSPREM_9,
      *            LNFILE.LN_INSPREM_10,
      *            LNFILE.LN_INSCOVR_1,
      *            LNFILE.LN_INSCOVR_2,
      *            LNFILE.LN_INSCOVR_3,
      *            LNFILE.LN_INSCOVR_4,
      *            LNFILE.LN_INSCOVR_5,
      *            LNFILE.LN_INSCOVR_6,
      *            LNFILE.LN_INSCOVR_7,
      *            LNFILE.LN_INSCOVR_8,
      *            LNFILE.LN_INSCOVR_9,
      *            LNFILE.LN_INSCOVR_10,
      *            LNFILE.LN_INSFINCHG_1,
      *            LNFILE.LN_INSFINCHG_2,
      *            LNFILE.LN_INSFINCHG_3,
      *            LNFILE.LN_INSFINCHG_4,
      *            LNFILE.LN_INSFINCHG_5,
      *            LNFILE.LN_INSFINCHG_6,
      *            LNFILE.LN_INSFINCHG_7,
      *            LNFILE.LN_INSFINCHG_8,
      *            LNFILE.LN_INSFINCHG_9,
      *            LNFILE.LN_INSFINCHG_10,
      *            LNFILE.LN_PY_YYYYMM_RECDEL,
      *            LNFILE.LN_MTD_PAYS_RECDEL,
      *            LNFILE.LN_WCBC_CREDITLIM,
      *            LNFILE.LN_MIP_FG,
      *            CAST(LNFILE.LN_MIP_CLPDTH_DATE AS VARCHAR(10)),
      *            CAST(LNFILE.LN_MIP_AHPDTH_DATE AS VARCHAR(10)),
      *            CAST(LNFILE.LN_LCPDTH_DATE AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INTPDTH_DATE AS VARCHAR(10)),
      *            LNFILE.LN_CURBAL,
      *            LNFILE.LN_INTBAL,
      *            LNFILE.LN_LCBAL,
      *            LNFILE.LN_OTHBAL,
      *            LNFILE.LN_OT2BAL,
      *            LNFILE.LN_LCPAID,
      *            LNFILE.LN_LCPARTIALS,
      *            LNFILE.LN_ANTICERN_1,
      *            LNFILE.LN_ANTICERN_2,
      *            LNFILE.LN_ANTICERN_3,
      *            LNFILE.LN_ANTICERN_4,
      *            LNFILE.LN_ANTICERN_5,
      *            LNFILE.LN_ANTICERN_6,
      *            LNFILE.LN_ANTICERN_7,
      *            LNFILE.LN_ANTICERN_8,
      *            LNFILE.LN_ANTICERN_9,
      *            LNFILE.LN_ANTICERN_10,
      *            LNFILE.LN_ANTICADJ_1,
      *            LNFILE.LN_ANTICADJ_2,
      *            LNFILE.LN_ANTICADJ_3,
      *            LNFILE.LN_ANTICADJ_4,
      *            LNFILE.LN_ANTICADJ_5,
      *            LNFILE.LN_ANTICADJ_6,
      *            LNFILE.LN_ANTICADJ_7,
      *            LNFILE.LN_ANTICADJ_8,
      *            LNFILE.LN_ANTICADJ_9,
      *            LNFILE.LN_ANTICADJ_10,
      *            LNFILE.LN_UFERN_1,
      *            LNFILE.LN_UFERN_2,
      *            LNFILE.LN_UFERN_3,
      *            LNFILE.LN_UFERN_4,
      *            LNFILE.LN_UFERN_5,
      *            LNFILE.LN_UFERN_6,
      *            LNFILE.LN_UFERN_7,
      *            LNFILE.LN_UFERN_8,
      *            LNFILE.LN_UFERN_9,
      *            LNFILE.LN_UFERN_10,
      *            LNFILE.LN_TOTINT,
      *            LNFILE.LN_TOTLCHG,
      *            LNFILE.LN_TOTDEF,
      *            LNFILE.LN_TOTPAYMNTD,
      *            LNFILE.LN_TOTNODEF,
      *            LNFILE.LN_TOTNOALDEL,
      *            LNFILE.LN_CL_REBATE,
      *            LNFILE.LN_AH_REBATE,
      *            LNFILE.LN_PP_REBATE,
      *            LNFILE.LN_INT_REBATE,
      *            LNFILE.LN_SC_REBATE,
      *            LNFILE.LN_MF_REBATE,
      *            LNFILE.LN_DF_REBATE,
      *            LNFILE.LN_EX_REBATE,
      *            LNFILE.LN_O1_REBATE,
      *            LNFILE.LN_O2_REBATE,
      *            LNFILE.LN_O3_REBATE,
      *            LNFILE.LN_O4_REBATE,
      *            LNFILE.LN_O5_REBATE,
      *            LNFILE.LN_O6_REBATE,
      *            LNFILE.LN_O7_REBATE,
      *            LNFILE.LN_UN_O7_REB,
      *            LNFILE.LN_UN_O8_REB,
      *            LNFILE.LN_UN_O9_REB,
      *            LNFILE.LN_UN_O10_REB,
      *            LNFILE.LN_ESCROW_FG,
      *            CAST(LNFILE.LN_DATE_PAID_LAST AS VARCHAR(10)),
      *            LNFILE.LN_CREDITLIM,
      *            LNFILE.LN_OLDACCTNO,
      *            LNFILE.LN_PRLNNO,
      *            LNFILE.LN_SUM_FINCHG,
      *            LNFILE.LN_SUM_ORGTERM,
      *            LNFILE.LN_UNITPER_CD,
      *            LNFILE.LN_UNITPER_FREQ,
      *            LNFILE.LN_REPOCD,
      *            LNFILE.LN_CONPROFILE_1,
      *            LNFILE.LN_CONPROFILE_2,
      *            LNFILE.LN_CONPROFILE_3,
      *            LNFILE.LN_CONPROFILE_4,
      *            LNFILE.LN_CONPROFILE_5,
      *            LNFILE.LN_RECPROFILE_1,
      *            LNFILE.LN_RECPROFILE_2,
      *            LNFILE.LN_RECPROFILE_3,
      *            LNFILE.LN_RECPROFILE_4,
      *            LNFILE.LN_RECPROFILE_5,
      *            LNFILE.LN_PO_TRCD,
      *            LNFILE.LN_LBOX, 
      *            BWFILE.BW_OWNBR,
      *            BWFILE.BW_SSNO,
      *            BWFILE.BW_LNAME,
      *            BWFILE.BW_FNAME,
      *            BWFILE.BW_ADR1,
      *            BWFILE.BW_ADR2,
      *            BWFILE.BW_CITY,
      *            BWFILE.BW_STATE,
      *            BWFILE.BW_ZIP,
      *            BWFILE.BW_PHONE,
      *            BWFILE.BW_CELL_PHONE,
      *            BWFILE.BW_REPCODE,
      *            BWFILE.BW_LOANS,
      *            BWFILE.BW_LIASMLN,
      *            BWFILE.BW_LIAOTH,
      *            CAST(BWFILE.BW_LASTPODATE AS VARCHAR(10)),
      *            BWFILE.BW_PRLOANS,
      *            CAST(BWFILE.BW_LAST_RN_FOR10_DATE AS VARCHAR(10)),
      *            BWFILE.BW_SRCD,
      *            BWFILE.BW_SOLICIT,
      *            BWFILE.BW_CONFIDENTIAL,
      *            BWFILE.BW_PRIVACY_OPT_OUT
      *      FROM #TEMP1_BRANCH_GROUP_TBL T1
      *      INNER JOIN DBO.LNFILE LNFILE
      *       ON LNFILE.LN_OWNBR = T1.BRNO
      *     INNER JOIN DBO.BWFILE BWFILE
      *       ON BWFILE.BW_OWNBR  = LNFILE.LN_OWNBR 
      *      AND BWFILE.BW_SSNO   = LNFILE.LN_SSNO1
      *     WHERE LNFILE.LN_CLASS BETWEEN @QLN3_WBEG_CLASS AND @QLN3_WEND_CLASS
      *      AND LNFILE.LN_POCD NOT IN (''**'',''PB'', ''10'', ''BT'') 
      *      AND LNFILE.LN_PLDATE          = ''1900-01-01''
      *      AND LNFILE.LN_JDDATE          = ''1900-01-01''
      *      AND LNFILE.LN_REPODATE        = ''1900-01-01'' 
      *      AND LNFILE.LN_DEBTAIDATE      = ''1900-01-01'' 
      *      AND LNFILE.LN_LEGALDATE       = ''1900-01-01'' 
      *      AND LNFILE.LN_BNKRPTDATE      = ''1900-01-01''
      *      AND LNFILE.LN_DATE_PAID_LAST != ''1900-01-01''
      *      AND BWFILE.BW_CELL_PHONE     != 0
      *      AND BWFILE.BW_SOLICIT        != ''N''
      *      AND BWFILE.BW_CONFIDENTIAL   != ''Y''
      *      AND LNFILE.LN_ORGTERM BETWEEN @MINTERM AND @MAXTERM'
      *  IF @OPENFORMER = 'F'
      *    SELECT @SQL += ' AND (LNFILE.LN_POFFDATE != ''1900-01-01'')'
      *  IF @OPENFORMER = 'O'
      *    SELECT @SQL += ' AND (LNFILE.LN_POFFDATE = ''1900-01-01'')'
      *
      *   ORDER BY T1.SEQ, LNFILE.LN_CLASS, LNFILE.LN_ACCTNO

           EXEC SQL SET CONNECTION MISC_CONNECT END-EXEC.

           EXEC SQL
            DECLARE LN3_CURSOR CURSOR FOR
                :LN3-RETURN-CODE = 
                 EXEC SSP_PITBO2_LNFILE_SELECT
                  ( :PASS-BRANCHLIST, :QLN3-WBEG-CLASS, 
                    :QLN3-WEND-CLASS,
                    :PASS-MINTERM, :PASS-MAXTERM,:PASS-OPENFORMER)
                END-EXEC.

           EXEC SQL
               OPEN LN3_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QLN3-WBEG-CLASS
                          QLN3-WEND-CLASS.

           MOVE STAT---GOOD TO LN3-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       READ-LN3-FILE-NEXT.    *> SSP - LN, BW
           PERFORM READ-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-PATH-SQL    TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING LN-OWNBR, LN-CLASS,LN-ACCTNO
              DELIMITED BY " " INTO E-KEYX.
           INITIALIZE QLN-REC QBW-REC.

           EXEC SQL SET CONNECTION MISC_CONNECT END-EXEC.

           IF ( LN3-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH LN3_CURSOR INTO 
                   :QLN-OWNBR,
                   :QLN-ACCTNO,
                   :QLN-CLASS,
                   :QLN-SUBCLASS,
                   :QLN-LAWCODE,
                   :QLN-SSNO1,
                   :QLN-MAKERCD1,
                   :QLN-SSNO2,
                   :QLN-MAKERCD2,
                   :QLN-SSNO3,
                   :QLN-MAKERCD3,
                   :QLN-SSNO4,
                   :QLN-MAKERCD4,
                   :QLN-ORGST,
                   :QLN-SPRCLASS,
                   :QLN-CONVERCD,
                   :QLN-SRCD,
                   :QLN-POCD,
                   :QLN-PLCD,
                   :QLN-CONFIDENTCD,
                   :QLN-ALLOTCD,
                   :QLN-STATUSFG,
                   :QLN-SALEFIN,
                   :QLN-LOANTYPE,
                   :QLN-ADDON-FG,
                   :QLN-PAY-SCHLD-FG,
                   :QLN-LC-CODE,
                   :QLN-LOANDATE ,
                   :QLN-PURDATE ,
                   :QLN-INTDATE ,
                   :QLN-CASHDATE ,
                   :QLN-ORIG-1STPYDATE ,
                   :QLN-1STPYDATE ,
                   :QLN-JDDATE ,
                   :QLN-PLDATE ,
                   :QLN-ACCELDATE ,
                   :QLN-REDUDATE ,
                   :QLN-POFFDATE ,
                   :QLN-LEGALDATE ,
                   :QLN-BNKRPTDATE ,
                   :QLN-DEBTAIDATE ,
                   :QLN-REPODATE ,
                   :QLN-APRATE,
                   :QLN-EFFRATE,
                   :QLN-SMPRATE,
                   :QLN-OVRDRATE,
                   :QLN-ENDORATE,
                   :QLN-JDRATE,
                   :QLN-ACCRATE,
                   :QLN-REDURATE,
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
                   :QLN-INSEFF-DATE-1 ,
                   :QLN-INSEFF-DATE-2 ,
                   :QLN-INSEFF-DATE-3 ,
                   :QLN-INSEFF-DATE-4 ,
                   :QLN-INSEFF-DATE-5 ,
                   :QLN-INSEFF-DATE-6 ,
                   :QLN-INSEFF-DATE-7 ,
                   :QLN-INSEFF-DATE-8 ,
                   :QLN-INSEFF-DATE-9 ,
                   :QLN-INSEFF-DATE-10 ,
                   :QLN-INSEXP-DATE-1 ,
                   :QLN-INSEXP-DATE-2 ,
                   :QLN-INSEXP-DATE-3 ,
                   :QLN-INSEXP-DATE-4 ,
                   :QLN-INSEXP-DATE-5 ,
                   :QLN-INSEXP-DATE-6 ,
                   :QLN-INSEXP-DATE-7 ,
                   :QLN-INSEXP-DATE-8 ,
                   :QLN-INSEXP-DATE-9 ,
                   :QLN-INSEXP-DATE-10 ,
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
                   :QLN-WCBC-CREDITLIM,
                   :QLN-MIP-FG,
                   :QLN-MIP-CLPDTH-DATE ,
                   :QLN-MIP-AHPDTH-DATE ,
                   :QLN-LCPDTH-DATE ,
                   :QLN-INTPDTH-DATE ,
                   :QLN-CURBAL,
                   :QLN-INTBAL,
                   :QLN-LCBAL,
                   :QLN-OTHBAL,
                   :QLN-OT2BAL,
                   :QLN-LCPAID,
                   :QLN-LCPARTIALS,
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
                   :QLN-TOTINT,
                   :QLN-TOTLCHG,
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
                   :QLN-ESCROW-FG,
                   :QLN-DATE-PAID-LAST ,
                   :QLN-CREDITLIM,
                   :QLN-OLDACCTNO,
                   :QLN-PRLNNO,
                   :QLN-SUM-FINCHG,
                   :QLN-SUM-ORGTERM,
                   :QLN-UNITPER-CD,
                   :QLN-UNITPER-FREQ,
                   :QLN-REPOCD,
                   :QLN-CONPROFILE-1,
                   :QLN-CONPROFILE-2,
                   :QLN-CONPROFILE-3,
                   :QLN-CONPROFILE-4,
                   :QLN-CONPROFILE-5,
                   :QLN-RECPROFILE-1,
                   :QLN-RECPROFILE-2,
                   :QLN-RECPROFILE-3,
                   :QLN-RECPROFILE-4,
                   :QLN-RECPROFILE-5,
                   :QLN-PO-TRCD,
                   :QLN-LBOX, 
                   :QBW-OWNBR,
                   :QBW-SSNO,
                   :QBW-LNAME,
                   :QBW-FNAME,
                   :QBW-ADR1,
                   :QBW-ADR2,
                   :QBW-CITY,
                   :QBW-STATE,
                   :QBW-ZIP,
                   :QBW-PHONE,
                   :QBW-CELL-PHONE,
                   :QBW-REPCODE,
                   :QBW-LOANS,
                   :QBW-LIASMLN,
                   :QBW-LIAOTH,
                   :QBW-LASTPODATE ,
                   :QBW-PRLOANS,
                   :QBW-LAST-RN-FOR10-DATE,
                   :QBW-SRCD,
                   :QBW-SOLICIT,
                   :QBW-CONFIDENTIAL,
                   :QBW-PRIVACY-OPT-OUT
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )   
              GO TO READ-LN3-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-LN-FIELDS
              PERFORM GET-BW-FIELDS.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       CLOSE-LN3-FILE.
           PERFORM CLOSE-IT.
           IF ( LN3-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION MISC_CONNECT END-EXEC
              EXEC SQL
                   CLOSE LN3_CURSOR
              END-EXEC
              MOVE STAT---BAD TO LN3-CURSOR-STAT.

            EXEC SQL SET CONNECTION C1 END-EXEC.


       COPY "LIBGB/GBBRRN.CPY".
      *-----------------------------------------------------------------
       READ-BR-GB-FILE.
           PERFORM READ-IT.
           MOVE BR-PATH-SQL TO E-FILE.
           MOVE BR-NO TO E-KEYX.
           INITIALIZE QBR-REC QGB-REC.

           MOVE BR-NO TO QBR-NO.

           EXEC SQL
           SELECT BRFILE.BR_NO,
                  BRFILE.BR_NAME, 
                  BRFILE.BR_AVAIL_CREDIT, 
                  BRFILE.BR_DATA_PATH, 
                  BRFILE.BR_MACHINE, 
                  BRFILE.BR_FORCE_VALID_CITY_COUNTY, 
                  BRFILE.BR_NOT_OURS_REFUND, 
                  BRFILE.BR_LATECHG_ON_ALLOTMENTS, 
                  BRFILE.BR_LATECHG_ON_LBOX,
                  GBFILE.GB_BRNO,
                  GBFILE.GB_NO,
                  GBFILE.GB_COMPANY_NAME, 
                  GBFILE.GB_PHONE,
                  GBFILE.GB_HOLIDAY_1,
                  GBFILE.GB_HOLIDAY_2,
                  GBFILE.GB_HOLIDAY_3,
                  GBFILE.GB_HOLIDAY_4,
                  GBFILE.GB_HOLIDAY_5,
                  GBFILE.GB_HOLIDAY_6,
                  GBFILE.GB_HOLIDAY_7,
                  GBFILE.GB_HOLIDAY_8,
                  GBFILE.GB_HOLIDAY_9,
                  GBFILE.GB_HOLIDAY_10,
                  GBFILE.GB_HOLIDAY_11,
                  GBFILE.GB_HOLIDAY_12,
                  GBFILE.GB_HOLIDAY_13,
                  GBFILE.GB_HOLIDAY_14,
                  GBFILE.GB_HOLIDAY_15,
                  GBFILE.GB_HOLIDAY_16
            INTO  :QBR-NO,
                  :QBR-NAME,
                  :QBR-AVAIL-CREDIT, 
                  :QBR-DATA-PATH, 
                  :QBR-MACHINE, 
                  :QBR-FORCE-VALID-CITY-COUNTY, 
                  :QBR-NOT-OURS-REFUND, 
                  :QBR-LATECHG-ON-ALLOTMENTS, 
                  :QBR-LATECHG-ON-LBOX,
                  :QGB-BRNO,
                  :QGB-NO,
                  :QGB-COMPANY-NAME, 
                  :QGB-PHONE,
                  :QGB-HOLIDAY-1,
                  :QGB-HOLIDAY-2,
                  :QGB-HOLIDAY-3,
                  :QGB-HOLIDAY-4,
                  :QGB-HOLIDAY-5,
                  :QGB-HOLIDAY-6,
                  :QGB-HOLIDAY-7,
                  :QGB-HOLIDAY-8,
                  :QGB-HOLIDAY-9,
                  :QGB-HOLIDAY-10,
                  :QGB-HOLIDAY-11,
                  :QGB-HOLIDAY-12,
                  :QGB-HOLIDAY-13,
                  :QGB-HOLIDAY-14,
                  :QGB-HOLIDAY-15,
                  :QGB-HOLIDAY-16
            FROM DBO.BRFILE BRFILE
            INNER JOIN DBO.GBFILE GBFILE
              ON GBFILE.GB_BRNO = BRFILE.BR_NO
            WHERE BRFILE.BR_NO = :QBR-NO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 ) 
              GO TO READ-BR-GB-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-BR-FIELDS
              PERFORM GET-GB-FIELDS.


      *--------------------------------
       OPEN-TEXT-SORT-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE TEXT-SORT-PATH TO E-FILE.
           OPEN OUTPUT TEXT-SORT-FILE.
           IF IO-FG = 8
             GO TO OPEN-TEXT-SORT-FILE-OUTPUT.
           IF IO-FG = 7
             CLOSE TEXT-SORT-FILE
             GO TO OPEN-TEXT-SORT-FILE-OUTPUT.
      *    UNLOCK TEXT-SORT-FILE.
       OPEN-TEXT-SORT-FILE.
           PERFORM OPEN-IT.
           MOVE TEXT-SORT-PATH TO E-FILE.
           OPEN I-O TEXT-SORT-FILE.
           IF IO-FG = 8
             GO TO OPEN-TEXT-SORT-FILE.
           IF IO-FG = 7
             CLOSE TEXT-SORT-FILE
             GO TO OPEN-TEXT-SORT-FILE.
      *    UNLOCK TEXT-SORT-FILE.
       READ-TEXT-SORT-FILE-NEXT.
           PERFORM READ-IT.
           MOVE TEXT-SORT-PATH TO E-FILE.
           MOVE TEXT-SORT-KEY TO E-KEYX.
           READ TEXT-SORT-FILE NEXT.
           IF IO-FG = 8
             GO TO READ-TEXT-SORT-FILE-NEXT.
       START-TEXT-SORT-FILE.
           PERFORM START-IT.
           MOVE TEXT-SORT-PATH TO E-FILE.
           MOVE TEXT-SORT-KEY TO E-KEYX.
           START TEXT-SORT-FILE KEY NOT < TEXT-SORT-KEY.
      *    UNLOCK TEXT-SORT-FILE.
       WRITE-TEXT-SORT-FILE.
           PERFORM WRITE-IT.
           MOVE TEXT-SORT-PATH TO E-FILE.
           MOVE TEXT-SORT-KEY TO E-KEYX.
           WRITE TEXT-SORT-REC.
           IF IO-FG = 8
             GO TO WRITE-TEXT-SORT-FILE.
      *    UNLOCK TEXT-SORT-FILE.
       CLOSE-TEXT-SORT-FILE.
           PERFORM CLOSE-IT.
           CLOSE TEXT-SORT-FILE.
      *-----------------------------------
       OPEN-TEXT-OUT-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE TEXT-OUT-PATH TO E-FILE.
           OPEN OUTPUT TEXT-OUT-FILE.
           IF IO-FG = 8
              GO TO OPEN-TEXT-OUT-FILE-OUTPUT.
           IF IO-FG = 7
              CLOSE TEXT-OUT-FILE
              GO TO OPEN-TEXT-OUT-FILE-OUTPUT.
       WRITE-TEXT-OUT-FILE.
           PERFORM WRITE-IT.
           MOVE TEXT-OUT-PATH TO E-FILE.
           WRITE TEXT-OUT-REC.
           IF IO-FG = 8
             GO TO WRITE-TEXT-OUT-FILE.
       CLOSE-TEXT-OUT-FILE.
           PERFORM CLOSE-IT.
           CLOSE TEXT-OUT-FILE.


       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************
       END-RUN.
           PERFORM CLOSE-FILES.
           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.
           IF NO-RECORDS-FOUND
              MOVE "NO RECORDS FOUND FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS.
           MOVE MKTEMP-BUF TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.
       EXIT-PROG.
           IF SV-BEG-BR NOT = 9999
              MOVE SV-BEG-BR TO BEG-BRANCH
              MOVE SV-END-BR TO END-BRANCH.
           IF EXT-ROUTE-BUF = "00"
              MOVE EXIT-PATH TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY PITBO2-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
