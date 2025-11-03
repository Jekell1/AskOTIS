       IDENTIFICATION DIVISION.
       PROGRAM-ID.     AUDEXT.
       DATE-WRITTEN.   01/18/2007.
      ******************************************************************
      *                      (LP)
      *       DESCRIPTION: AUDIT/CONFIRMATION EXTRACTION
      *                        CUSTOM FOR WORLD
      *  WORLD NEEDS AN EXTRACTION TO ALLOW THEM TO SEND AUDIT CONFIRMATIONS
      *  FROM HOME OFFICE, USING A FILE SENT TO THE PRINT SHOP.
      *
      * REV:
      *  BAH 070118 NEW FOR WORLD PR# 519
      *  BAH 070212 MOVE EXT-EOBUF-DATES TO BEG AND END DATE
      *  BAH 140716 ADDED GROSS-LOAN, CURBAL AND ONLY SEND LAST 4 OF SSNO
      *                                            WORLD PR# 1004
      *  BAH 160128 SPLIT OUT LTFILE, PD0003
      *
      * KEC 2016.0211 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         FIXED KEYS & RECS FOR FD-REC.
      *         CHANGED FMNO/FORMNO/NEXTFM/SETFM FROM 3 TO 4 BYTES.
      * BAH 170320 ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 181203 GLOBAL LNFILE
      * BAH 20190930 REMOVED ACCESS-CALL LNFILE
      * BAH 20220803 HARD CODED START
      * BAH 20220929 ADDED INSURANCE WRITTEN FLAGS TO AUD-FILE #1571
      * MB  20231003 PERFORMANCE TUNING - ADDED STORED PROCEDURE
      *              ADDED STATE PARAMETER ARRAY PROCESSING
      * RMZ 20240116 CHANGED JULIAN-BEG(END,CC) TO EXT-JULIAN-BEG(END,CC)
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
      * WORK FILE FOR DUPLICATE ACCOUNTS
       COPY "LIBGB/GBFSWKI.CPY".
      * EXTRACT FILE
           SELECT AUDIT-FILE ASSIGN AUDIT-PATH
                  ORGANIZATION RELATIVE
                  ACCESS MODE DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RELATIVE KEY AUDIT-KEY
                  FILE STATUS FILE-STAT.

       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".
       FD  WK-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK-REC.
           03  WK-KEY.
               05  WK-BRNO        PIC 9(4).
               05  WK-ACCTNO      PIC 9(8).

       FD  AUDIT-FILE
           LABEL RECORDS ARE STANDARD.
       01  AUDIT-REC.
           03  AUD-BRNO            PIC 9(4).
           03  AUD-NUMBER          PIC 9(6).
           03  AUD-SSNO            PIC 9(4).
           03  AUD-LNAME           PIC X(16).
           03  AUD-FNAME           PIC X(16).
           03  AUD-ADR1            PIC X(24).
           03  AUD-ADR2            PIC X(24).
           03  AUD-CITY            PIC X(15).
           03  AUD-STATE           PIC XX.
           03  AUD-ZIP             PIC X(10).
           03  AUD-LOANDATE        PIC 9(6).
           03  AUD-ORIG-1STPAY     PIC 9(6).
           03  AUD-GROSS-LOAN      PIC 9(7).99.
           03  AUD-PROCEEDS        PIC 9(7).99.
           03  AUD-ORGTERM         PIC 9(3).
           03  AUD-REGPYAMT        PIC 9(7).99.
           03  AUD-CURBAL          PIC 9(6).99.
           03  AUD-PP-WRITTEN      PIC X.
           03  AUD-CL-AH-WRITTEN   PIC X.
           03  AUD-CL-ONLY-WRITTEN PIC X.
           03  AUD-AUTO-WRITTEN    PIC X.
           03  AUD-IUI-WRITTEN     PIC X.
           03  AUD-ADD-WRITTEN     PIC X.
           03  AUD-UMC-WRITTEN     PIC X.
           03  FILLER              PIC X(43).
           03  AUD-CR              PIC X.
           03  AUD-LF              PIC X.

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LP/AUDEXT.CBL S35 02/20/24-13:39:14 >".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01LTP.CPY".
       COPY "LIBLP/LP01LTP_SQL.CPY".
       COPY "LIBLP/LPWSLTP.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       01  AUDIT-PATH.
           03  AUDIT-PATH-DIRECTORY.
               05  AUDIT-PATH-BASE           PIC X(09).
               05  FILLER                    PIC X(01)  VALUE "/".
               05  AUDIT-PATH-MACHINE        PIC X(02).
               05  FILLER                    PIC X(04)  VALUE "/ED/".
           03  AUDIT-EXTRACT-NAME.
               05 AUDIT-PATH-EXTR            PIC X(3)   VALUE "AUD".
               05 AUDIT-PATH-MM              PIC 99.
               05 AUDIT-PATH-DD              PIC 99.
               05 AUDIT-PATH-YY              PIC 99.
               05 AUDIT-PATH-DOT             PIC X      VALUE ".".
               05 AUDIT-PATH-BRNO            PIC 9999.
               05 AUDIT-PATH-GROUP REDEFINES AUDIT-PATH-BRNO PIC X(4).


       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  PASS-INC-ZBAL                   PIC X.
       01  PASS-INC-PL                     PIC X.
       01  PASS-INC-BNKRPT                 PIC X.
       01  PASS-INC-CONFID                 PIC X.
       01  PASS-INC-REPO                   PIC X.
       01  PASS-INC-SRCD1                  PIC XX.
       01  PASS-INC-SRCD2                  PIC XX.
       01  PASS-INC-SRCD3                  PIC XX.
       01  PASS-INC-SRCD4                  PIC XX.
       01  PASS-INC-SRCD5                  PIC XX.
       01  PASS-INC-SRCD6                  PIC XX.
       01  PASS-INC-SRCD7                  PIC XX.
       01  PASS-INC-SRCD8                  PIC XX.
       01  PASS-INC-SRCD9                  PIC XX.
       01  PASS-INC-SRCD10                 PIC XX.
       01  PASS-BEG-LN-DATE                PIC X(10).
       01  PASS-END-LN-DATE                PIC X(10).
       01  PASS-OFFSET                     PIC 99.
       EXEC SQL END DECLARE SECTION END-EXEC.

       01  LOOP-PROC-BRANCH            PIC 9(4)        VALUE 0.
       01  LN3-CONN-OPEN-SW            PIC X           VALUE "N".
       01  SUB2                        PIC 9(5)        VALUE 0.

       01  WK-IFN               PIC XX      VALUE "WK".
       01  AUDIT-KEY            PIC 9(7)    VALUE 0.

       01  UMC-FEE              PIC S9(7)V99 COMP-3 VALUE 0.
       01  ADD-FEE              PIC S9(7)V99 COMP-3 VALUE 0.
       01  SUB                  PIC 999.
       01  BRANCH-COUNT         PIC 9(7)     VALUE 0.
       01  REC-COUNT            PIC 9(7)     VALUE 0.
       01  TOTAL-COUNT          PIC 9(7)     VALUE 0.
       01  TRY-TO-MEET-MINIMUM  PIC X.
       01  SOURCE-OK            PIC X.
       01  POBOX-OK             PIC X.
       01  TEST-ADDRESS.
           03  TEST-ADDR        PIC X(24).
           03  FILLER           REDEFINES TEST-ADDR.
               05  ADDR-FIRST3  PIC X(3).
               05  FILLER       PIC X(21).
           03  FILLER           REDEFINES TEST-ADDR.
               05  ADDR-FIRST5  PIC X(5).
               05  FILLER       PIC X(19).
           03  FILLER           REDEFINES TEST-ADDR.
               05  ADDR-FIRST6  PIC X(6).
               05  FILLER       PIC X(18).
           03  FILLER           REDEFINES TEST-ADDR.
               05  ADDR-FIRST7  PIC X(7).
               05  FILLER       PIC X(17).
           03  FILLER           REDEFINES TEST-ADDR.
               05  ADDR-FIRST8  PIC X(8).
               05  FILLER       PIC X(16).
           03  FILLER           REDEFINES TEST-ADDR.
               05  ADDR-FIRST9  PIC X(9).
               05  FILLER       PIC X(15).

      **********************
      *   LINE WORKERS     *
      **********************
       01  LN-WORKERS.
           03  LN-FEED          PIC 99       VALUE 0.
           03  LN-COUNT         PIC 99       VALUE 90.
           03  LN-WK            PIC 99       VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

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
           03  FILLER           PIC X(2)     VALUE " ".
           03  FILLER           PIC X(50)    VALUE
           " AUDIT CONFIRMATION ASCII FILE EXTRACTION  ".

       01  HEAD3.
           03  FILLER           PIC X(6) VALUE " ".
           03  FILLER           PIC X(17) VALUE "BRANCH ".
           03  FILLER           PIC X(15) VALUE "COUNT".

       01  DETAIL-LINE          PIC X(80) VALUE SPACES.

       01  DETAIL1 REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(5).
           03  D-BRNO           PIC Z(4).
           03  FILLER           PIC X(9).
           03  D-COUNT          PIC Z(6)9.

       01  DETAIL2 REDEFINES DETAIL-LINE.
           03  T-DESC           PIC X(18).
           03  T-COUNT          PIC Z(6)9.

       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(26).
           03  RANGE-SELECTION  PIC X(106).
           03  SOURCE-SEL REDEFINES RANGE-SELECTION.
               05  D-SRCD       PIC X(4) OCCURS 10.
               05  FILLER       PIC X(66).
           03  RANGE-DATE REDEFINES RANGE-SELECTION
                                PIC 99/99/9999.

       01  SYSTEM-DATE           PIC 9(8).
       01  FILLER REDEFINES SYSTEM-DATE.
           03  SYS-CC            PIC 99.
           03  SYS-YEAR          PIC 99.
           03  SYS-MONTH         PIC 99.
           03  SYS-DAY           PIC 99.
       01  HOLD-TEST             PIC X.
       01  BAL-WK                PIC 9(7) COMP-3 VALUE 0.
       01  HOLD-DIGIT            PIC 9.
       01  KOUNT                 PIC 999 VALUE 0.

       COPY "LIBGB/EOBUF_EXT.CPY".
           03  BEG-BRANCH       PIC 9(4).
           03  END-BRANCH       PIC 9(4).
           03  BEG-CL           PIC 99.
           03  END-CL           PIC 99.
           03  BEG-BAL          PIC 9(6).
           03  END-BAL          PIC 9(6).
           03  BEG-CAGE         PIC 9(4).
           03  END-CAGE         PIC 9(4).
           03  BEG-LNAME        PIC X.
           03  END-LNAME        PIC X.
           03  BEG-LN-DATE      PIC 9(8).
           03  END-LN-DATE      PIC 9(8).
           03  ANOEND-BUF       PIC 9.
           03  ANOEND-BUFX REDEFINES ANOEND-BUF PIC X.
           03  SKIP-BUF         PIC 999.
           03  CONFID-BUF       PIC X.
           03  ALLOT-BUF        PIC X.
           03  ZBAL-BUF         PIC X.
           03  PL-BUF           PIC X.
           03  JD-BUF           PIC X.
           03  DEBT-BUF         PIC X.
           03  LEGAL-BUF        PIC X.
           03  BANK-BUF         PIC X.
           03  REPO-BUF         PIC X.
           03  GROUP-FLAG       PIC X.
           03  ENT-GROUP        PIC X(4).
           03  SRCD-TABLE.
               05  SRCD         OCCURS 10 TIMES PIC XX.
           03  POBOX-BUF        PIC X.
           03  LBOX-BUF         PIC X.
           03  MIN-BRANCH       PIC 99.

       01  VERSION-CONTROL      PIC X VALUE "A".

       01  SV-BEG-BR           PIC 9(4)   VALUE 9999.
       01  SV-END-BR           PIC 9(4)   VALUE 9999.
       01  SV-GROUP-FLAG       PIC X     VALUE SPACES.
       01  SV-ENT-GROUP        PIC X(4)  VALUE SPACES.

       01  SRCD-LIST.
           03  FILLER           PIC X(30) VALUE
           "SRCD1 SRCD2 SRCD3 SRCD4 SRCD5 ".
           03  FILLER           PIC X(31) VALUE
           "SRCD6 SRCD7 SRCD8 SRCD9 SRCD10.".

       01  MESS-LIST            PIC X(5) VALUE "MESS.".

       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBLP/LPPDUEW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/AGEINGW.CPY".
       COPY "LIBGB/SPPDTHW.CPY".
       COPY "LIBLP/LPCEPPW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".

       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/AUDEXT_DEF.CPY".
       COPY "LIBLP/AUDEXT_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".
       COPY "LIBLP/ARRAYSPW.CPY". 

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBLP/AUDEXT_SCN.CPY".

      *-----------------------------------------------------------------
       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE AUDIT-FILE WK-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LTP1-FILE.
           PERFORM CLOSE-LN3-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-LN3-CONNECTION.
           CLOSE
               PR-FILE  AUDIT-FILE WK-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      *************************************************
       MAIN-MODULE SECTION.
      *************************************************

           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "AUDEXT" TO FORM-PROG VDU-FORM-NAME.
      D    STOP MESS.

           PERFORM GET-GPENV.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

           MOVE EXT-FILPATH-BASE    TO AUDIT-PATH-BASE.
           MOVE EXT-FILPATH-MACHINE TO AUDIT-PATH-MACHINE.
           MOVE SYS-DAY             TO AUDIT-PATH-DD.
           MOVE SYS-YEAR            TO AUDIT-PATH-YY.
           MOVE SYS-MONTH           TO AUDIT-PATH-MM.
           IF GROUP-FLAG = "Y"
              MOVE ENT-GROUP        TO AUDIT-PATH-GROUP
           ELSE
              MOVE BEG-BRANCH       TO AUDIT-PATH-BRNO.

           MOVE AUDIT-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT NOT = "00"
              PERFORM OPEN-AUDIT-FILE-OUTPUT
              MOVE 0 TO AUDIT-KEY
           ELSE
              MOVE "E" TO EXT-EOBUF-ERRCD
              MOVE "FILE ALREADY EXISTS" TO MESS
              PERFORM SEND-MESS
              GO TO END-ROUTINE.

           IF GROUP-FLAG = "N"
              MOVE BEG-BRANCH TO BR-NO
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

           MOVE WGR-SRESULT TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           GO TO MOVE-BRANCH-PATH-INFO.

       NEXT-BRANCH-NO.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-FG = 9 OR BR-NO > END-BRANCH
              GO TO END-ROUTINE.

       MOVE-BRANCH-PATH-INFO.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO BW-PATH-OVERRIDE
                                              LN-PATH-OVERRIDE
                                              LTP-PATH-OVERRIDE.

           IF NOT (PRU-LP = "VDU" OR "PRU")
              IF EXT-ROUTE-BUF = "00"
                 MOVE "AUDIT EXTRACTION IN PROGRESS BRANCH &&&&" TO MESS
                 INSPECT MESS REPLACING FIRST "&&&&" BY BR-NO
                 MOVE MESS TO WR-BUF
                 MOVE MESS-LIST TO WR-LIST
                 PERFORM CALL-WRFORM.

      ******* COUNTER FOR MINIMUM PER BRANCH
           MOVE 0 TO BRANCH-COUNT.
      *******

      * EMPTY WORK FILE TO VERIFY NO DUPLICATES IF YOU HAVE TO GO BACK AND
      * FIND MORE ACCOUNTS TO MEET THE MINIMUM

           PERFORM OPEN-WK-FILE-OUTPUT.
           PERFORM CLOSE-WK-FILE.
           PERFORM OPEN-WK-FILE.

           MOVE "N" TO TRY-TO-MEET-MINIMUM.

           PERFORM OPEN-BW1-FILE.
           PERFORM OPEN-LN3-FILE.

           MOVE BR-NO     TO LN-OWNBR
                             QLN3-WBEG-OWNBR
                             QLN3-WEND-OWNBR.
           MOVE BEG-CL    TO LN-CLASS
                             QLN3-WBEG-CLASS.
           MOVE END-CL    TO QLN3-WEND-CLASS.

      *     MOVE 0         TO LN-ACCTNO
      *                       QLN3-WBEG-ACCTNO.
      *     MOVE ALL "9"   TO QLN3-WEND-ACCTNO.

           MOVE PL-BUF     TO PASS-INC-PL.
           MOVE ZBAL-BUF   TO PASS-INC-ZBAL.
           MOVE BANK-BUF   TO PASS-INC-BNKRPT.
           MOVE CONFID-BUF TO PASS-INC-CONFID.
           MOVE REPO-BUF   TO PASS-INC-REPO.
           MOVE SRCD(1)    TO PASS-INC-SRCD1.
           MOVE SRCD(2)    TO PASS-INC-SRCD2.
           MOVE SRCD(3)    TO PASS-INC-SRCD3.
           MOVE SRCD(4)    TO PASS-INC-SRCD4.
           MOVE SRCD(5)    TO PASS-INC-SRCD5.
           MOVE SRCD(6)    TO PASS-INC-SRCD6.
           MOVE SRCD(7)    TO PASS-INC-SRCD7.
           MOVE SRCD(8)    TO PASS-INC-SRCD8.
           MOVE SRCD(9)    TO PASS-INC-SRCD9.
           MOVE SRCD(10)   TO PASS-INC-SRCD10.

           MOVE BEG-LN-DATE TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO PASS-BEG-LN-DATE.

           MOVE END-LN-DATE TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO PASS-END-LN-DATE.

           MOVE 0 TO PASS-OFFSET.

           PERFORM START-LN3-FILE.  *> SSP LN BW

       NEXT-LN.
           PERFORM READ-LN3-FILE-NEXT.   *> EMBEDDED LN BW
           IF IO-BAD               *> OR (LN-OWNBR NOT = LN-PATH-OWNBR)
              GO TO END-OF-GROUP.

      * THIS ALSO CHECKED IN STORED PROCEDURE WHERE CLAUSE
           IF LN-OWNBR > END-BRANCH
              GO TO END-OF-GROUP.

      *  POCD = "**" IS A DELETED CONTRACT
      * THIS ALSO CHECKED IN STORED PROCEDURE WHERE CLAUSE
           IF ( LN-POCD-NONLOAN )
              GO TO NEXT-LN.

      * THIS ALSO CHECKED IN STORED PROCEDURE WHERE CLAUSE
           IF LN-OWNBR < BEG-BRANCH
              GO TO NEXT-LN.

      * THIS ALSO CHECKED IN STORED PROCEDURE WHERE CLAUSE
           IF LN-CLASS < BEG-CL OR LN-CLASS > END-CL
              GO TO NEXT-LN.

           IF LN-LOANDATE < BEG-LN-DATE OR LN-LOANDATE > END-LN-DATE
              GO TO NEXT-LN.

      * THIS ALSO CHECKED IN STORED PROCEDURE WHERE CLAUSE
           IF PL-BUF NOT = "Y"
              IF LN-PLDATE NOT = 0
                 GO TO NEXT-LN.

           IF JD-BUF NOT = "Y"
              IF LN-JDDATE NOT = 0
                 GO TO NEXT-LN.

      * THIS ALSO CHECKED IN STORED PROCEDURE WHERE CLAUSE
           IF ZBAL-BUF NOT = "Y"
              IF LN-CURBAL = 0 AND
                  LN-INTBAL = 0 AND
                   LN-OTHBAL = 0
                 GO TO NEXT-LN.

           IF ALLOT-BUF NOT = "Y"
              IF LN-ALLOTCD = "Y"
                 GO TO NEXT-LN.

           IF LBOX-BUF = "N"
              IF LN-LBOX = "Y"
                 GO TO NEXT-LN.

      * THIS ALSO CHECKED IN STORED PROCEDURE WHERE CLAUSE
           IF REPO-BUF NOT = "Y"
              IF LN-REPOCD = "X" OR "S"
                 GO TO NEXT-LN.

           IF DEBT-BUF NOT = "Y"
              IF LN-DEBTAIDATE NOT = 0
                 GO TO NEXT-LN.

           IF LEGAL-BUF NOT = "Y"
              IF LN-LEGALDATE NOT = 0
                 GO TO NEXT-LN.

      * THIS ALSO CHECKED IN STORED PROCEDURE WHERE CLAUSE
           IF BANK-BUF NOT = "Y"
              IF LN-BNKRPTDATE NOT = 0
                 GO TO NEXT-LN.

           COMPUTE BAL-WK ROUNDED =
                   LN-CURBAL + LN-INTBAL + LN-LCBAL + LN-OTHBAL.
           IF BAL-WK < BEG-BAL OR BAL-WK > END-BAL
              GO TO NEXT-LN.

           IF SRCD(1) NOT = "**"
              PERFORM TEST-SOURCE-CODES
              IF SOURCE-OK NOT = "Y"
                 GO TO NEXT-LN.

           PERFORM COMPUTE-AGEING-CAT.
           IF CONTRACTUAL < BEG-CAGE OR CONTRACTUAL > END-CAGE
              GO TO NEXT-LN.

      * BW DATA RETURNED IN STORED PROCEDURE 
      *     MOVE LN-SSNO(1) TO BW-SSNO.
      *     PERFORM READ-BW1-FILE.
      *     IF NOT IO-GOOD
      *        MOVE SPACES TO BW-LNAME BW-FNAME BW-ZIP.

           MOVE BW-LNAME TO HOLD-TEST.
           IF HOLD-TEST < BEG-LNAME OR HOLD-TEST > END-LNAME
              GO TO NEXT-LN.

      * THIS ALSO CHECKED IN STORED PROCEDURE WHERE CLAUSE
           IF CONFID-BUF = "N"
              IF LN-CONFIDENTCD = "Y" OR BW-CONFIDENTIAL = "Y"
                 GO TO NEXT-LN.

           IF POBOX-BUF = "N"
              MOVE BW-ADR1 TO TEST-ADDR
              PERFORM TEST-POBOX
              IF POBOX-OK NOT = "Y"
                 GO TO NEXT-LN.

           IF POBOX-BUF = "N"
              MOVE BW-ADR2 TO TEST-ADDR
              PERFORM TEST-POBOX
              IF POBOX-OK NOT = "Y"
                 GO TO NEXT-LN.

      * DO NOT TEST ENDING DIGIT IF TRYING TO MEET MINIMUM
      * ANOEND-BUFX = * IS F3-ALL
           IF TRY-TO-MEET-MINIMUM = "N"
              IF ANOEND-BUFX NOT = "*"
                 MOVE LN-ACCTNO TO HOLD-DIGIT
                 IF HOLD-DIGIT < ANOEND-BUF OR HOLD-DIGIT > ANOEND-BUF
                    GO TO NEXT-LN.

           ADD 1 TO KOUNT.

      * DO NOT TEST 'EVERY N-TH'  ACCOUNT IF TRYING TO MEET MINIMUM LOOP
           IF TRY-TO-MEET-MINIMUM = "N"
              IF KOUNT NOT = SKIP-BUF
                 GO TO NEXT-LN.

           MOVE 0 TO KOUNT.

      * YOU MAY BE LOOKING FOR MORE ACCOUNTS BECAUSE YOU DIDNT MEET THE
      * MINIMUM PER BRANCH, MAKE SURE TO NOT GET A DUPLICATE.
           IF TRY-TO-MEET-MINIMUM = "Y"
              MOVE BR-NO TO WK-BRNO
              MOVE LN-ACCTNO TO WK-ACCTNO
              PERFORM READ-WK-FILE
              IF IO-FG = 0
                 GO TO NEXT-LN.

           ADD 1 TO REC-COUNT.

           PERFORM CREATE-AUDIT-FILE.

           MOVE BR-NO     TO WK-BRNO.
           MOVE LN-ACCTNO TO WK-ACCTNO.
           PERFORM WRITE-WK-FILE.

           ADD 1 TO BRANCH-COUNT.
           IF TRY-TO-MEET-MINIMUM = "Y"
              IF BRANCH-COUNT = MIN-BRANCH
                 GO TO END-OF-GROUP.

           GO TO NEXT-LN.

       END-OF-GROUP.
           IF TRY-TO-MEET-MINIMUM = "N"
              IF BRANCH-COUNT < MIN-BRANCH
                 MOVE "Y" TO TRY-TO-MEET-MINIMUM
                 MOVE BR-NO     TO LN-OWNBR
                                   QLN3-WBEG-OWNBR
                                   QLN3-WEND-OWNBR
                 MOVE BEG-CL    TO LN-CLASS
                                   QLN3-WBEG-CLASS
                 MOVE END-CL    TO QLN3-WEND-CLASS
      *           MOVE 0         TO LN-ACCTNO
      *                             QLN3-WBEG-ACCTNO
      *           MOVE ALL "9"   TO QLN3-WEND-ACCTNO
      * FROM MB CHANGE 20231003 -- SETTING PASS-OFFSET TO 2 WILL HAVE STORED PROCEDURE SKIP FIRST 2 ROWS
                 MOVE 2 TO PASS-OFFSET
                 PERFORM START-LN3-FILE  *> SSP LN BW
      *           PERFORM READ-LN3-FILE-NEXT
      * START AGAIN SO YOU ARE AT A NEW PART OF FILE THAN LAST RUN
      *           PERFORM READ-LN3-FILE-NEXT
                 GO TO NEXT-LN.

           PERFORM CLOSE-LN3-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-WK-FILE.

           MOVE BR-NO     TO D-BRNO.
           MOVE REC-COUNT TO D-COUNT.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           ADD REC-COUNT TO TOTAL-COUNT.
           MOVE 0 TO REC-COUNT.

           GO TO NEXT-GROUP-LOC.

       END-ROUTINE.
           IF TOTAL-COUNT > 0
              MOVE "TOTAL RECORDS :   " TO T-DESC
              MOVE TOTAL-COUNT TO T-COUNT
              MOVE 4 TO LN-FEED
              PERFORM WRITE-DETAIL-LINE.

           PERFORM PRINT-RANGE-LINES.

           GO TO END-PROGRAM.

      ************************************
       TEST-SOURCE-CODES SECTION.
      ************************************
           MOVE " " TO SOURCE-OK.
           MOVE 1 TO SUB.
       TEST-SOURCE-CODES-AGAIN.
           IF SUB > 10
              GO TO TEST-SOURCE-CODES-EXIT.
           IF LN-SRCD NOT = SRCD(SUB)
              ADD 1 TO SUB
              GO TO TEST-SOURCE-CODES-AGAIN.
           MOVE "Y" TO SOURCE-OK.
       TEST-SOURCE-CODES-EXIT.
           EXIT.

      ********************************************
       TEST-POBOX SECTION.
      ********************************************

      * TEST FOR SKIPPING P.O. BOX ADDRESSES
 
           MOVE "Y" TO POBOX-OK.

           IF ADDR-FIRST3  = "BOX"
              MOVE "N" TO POBOX-OK
              GO TO TEST-POBOX-EXIT.
           IF ADDR-FIRST3  = "POB"
              MOVE "N" TO POBOX-OK
              GO TO TEST-POBOX-EXIT.
           IF ADDR-FIRST5  = "POBOX"
              MOVE "N" TO POBOX-OK
              GO TO TEST-POBOX-EXIT.
           IF ADDR-FIRST5  = "P.O.B"
              MOVE "N" TO POBOX-OK
              GO TO TEST-POBOX-EXIT.
           IF ADDR-FIRST6  = "PO BOX"
              MOVE "N" TO POBOX-OK
              GO TO TEST-POBOX-EXIT.
           IF ADDR-FIRST7  = "P.O BOX"
              MOVE "N" TO POBOX-OK
              GO TO TEST-POBOX-EXIT.
           IF ADDR-FIRST7  = "PO. BOX"
              MOVE "N" TO POBOX-OK
              GO TO TEST-POBOX-EXIT.
           IF ADDR-FIRST8  = "P.O. BOX"
              MOVE "N" TO POBOX-OK
              GO TO TEST-POBOX-EXIT.
           IF ADDR-FIRST8  = "P. O. B."
              MOVE "N" TO POBOX-OK
              GO TO TEST-POBOX-EXIT.
           IF ADDR-FIRST9  = "P. O. BOX"
              MOVE "N" TO POBOX-OK
              GO TO TEST-POBOX-EXIT.

       TEST-POBOX-EXIT.
           EXIT.

      ***************************
       CREATE-AUDIT-FILE SECTION.
      ***************************
           MOVE SPACES TO AUDIT-REC.

           MOVE BR-NO      TO AUD-BRNO.
           MOVE LN-ACCTNO  TO AUD-NUMBER.
           MOVE LN-SSNO(1) TO AUD-SSNO.

           MOVE BW-LNAME   TO AUD-LNAME.
           MOVE BW-FNAME   TO AUD-FNAME.
           MOVE BW-ADR1    TO AUD-ADR1.
           MOVE BW-ADR2    TO AUD-ADR2.
           MOVE BW-CITY    TO AUD-CITY.
           MOVE BW-STATE   TO AUD-STATE.
           MOVE BW-ZIP     TO AUD-ZIP.

           MOVE LN-LOANDATE       TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY       TO AUD-LOANDATE.

           MOVE LN-ORIG-1STPYDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY       TO AUD-ORIG-1STPAY.

           COMPUTE AUD-GROSS-LOAN = LN-1STPYAMT
                     + LN-NOREGPY * LN-REGPYAMT + LN-LASTPYAMT.
           MOVE LN-PROCEEDS       TO AUD-PROCEEDS.
           MOVE LN-ORGTERM        TO AUD-ORGTERM.
           MOVE LN-REGPYAMT       TO AUD-REGPYAMT.
           MOVE LN-CURBAL         TO AUD-CURBAL.

      * SET THEM ALL TO "N", WHEN TESTING FOR STATES, THE OTHER STATES
      * WERE LEAVING THE FIELD BLANK, IT NEEDS TO BE "N"
           MOVE "N" TO AUD-PP-WRITTEN      AUD-CL-AH-WRITTEN
                       AUD-CL-ONLY-WRITTEN AUD-AUTO-WRITTEN
                       AUD-IUI-WRITTEN     AUD-ADD-WRITTEN
                       AUD-UMC-WRITTEN.

      * PROPERTY WRITTEN
           IF LN-INSPREM(3) NOT = 0
              MOVE "Y"         TO AUD-PP-WRITTEN
           ELSE
              MOVE "N"         TO AUD-PP-WRITTEN.
      * BOTH CREDIT LIFE AND A&H WRITTEN
           IF (LN-INSPREM(1) NOT = 0 AND LN-INSPREM(2) NOT = 0)
              MOVE "Y"         TO AUD-CL-AH-WRITTEN
           ELSE
              MOVE "N"         TO AUD-CL-AH-WRITTEN.
      * CREDIT LIFE ONLY
           IF (LN-INSPREM(1) NOT = 0 AND LN-INSPREM(2) = 0)
              MOVE "Y"         TO AUD-CL-ONLY-WRITTEN
           ELSE
              MOVE "N"         TO AUD-CL-ONLY-WRITTEN.
      * THESE STATES USE OTHER 1 AS AUTO
           IF LN-ORGST = "TN" OR "OK" OR "MS"
              IF LN-INSPREM(4) NOT = 0
                 MOVE "Y" TO AUD-AUTO-WRITTEN
              ELSE
                 MOVE "N" TO AUD-AUTO-WRITTEN.
      * THESE STATES USE OTHER 3 AS AUTO
           IF LN-ORGST = "AL" OR "GA" OR "ID" OR "IN" OR
                          "NM" OR "TX" OR "UT"
              IF LN-INSPREM(6) NOT = 0
                 MOVE "Y" TO AUD-AUTO-WRITTEN
              ELSE
                 MOVE "N" TO AUD-AUTO-WRITTEN.
      * THESE STATES USE OTHER 1 AS IUI
           IF LN-ORGST = "KY" OR "MO" OR "TX" OR "IN" OR
                          "NM" OR "ID" OR "MO" OR "UT" OR "AL"
              IF LN-INSPREM(4) NOT = 0
                 MOVE "Y" TO AUD-IUI-WRITTEN
              ELSE
                 MOVE "N" TO AUD-IUI-WRITTEN.
      * THESE STATES USE OTHER 3 AS IUI
           IF LN-ORGST = "TN" OR "SC" OR "OK"
              IF LN-INSPREM(6) NOT = 0
                 MOVE "Y" TO AUD-IUI-WRITTEN
              ELSE
                 MOVE "N" TO AUD-IUI-WRITTEN.
         
           MOVE 0 TO UMC-FEE ADD-FEE.
           PERFORM FIND-FEE-CODES VARYING SUB FROM 1 BY 1 UNTIL
                                          SUB > 10.

      * FEE CODE 97 IS AD&D
           IF ADD-FEE NOT = 0
              MOVE "Y" TO AUD-ADD-WRITTEN
           ELSE
              MOVE "N" TO AUD-ADD-WRITTEN.
      * FEE CODES 93 AND 94 ARE UMC
           IF UMC-FEE NOT = 0
              MOVE "Y" TO AUD-UMC-WRITTEN
           ELSE
              MOVE "N" TO AUD-UMC-WRITTEN.
        
           MOVE X"0D" TO AUD-CR.
           MOVE X"0A" TO AUD-LF.

           ADD 1 TO AUDIT-KEY.
           PERFORM WRITE-AUDIT-FILE.

      ************************
       INITIALIZATION SECTION.
      ************************
           MOVE AUDEXT-QUEUE-POS TO Q-POS.
           MOVE AUDEXT-COPIES-POS TO C-POS.
           MOVE AUDEXT-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO SYSTEM-DATE.

           MOVE EXT-CONAME-CONAME   TO H-NAME.
           MOVE EXT-CONAME-SYSDATE  TO H-DATE.
           MOVE EXT-CONAME-USER-ID  TO H-USERID.

           IF EXT-ROUTE-BUF = "00"
              MOVE " AUDIT EXTRACTION IN PROGRESS......." TO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           IF EXT-EOBUF-GROUP-FG = "Y"
              MOVE BEG-BRANCH TO SV-BEG-BR
              MOVE END-BRANCH TO SV-END-BR
              MOVE GROUP-FLAG TO SV-GROUP-FLAG
              MOVE ENT-GROUP TO SV-ENT-GROUP
              MOVE EXT-EOBUF-BRNO TO BEG-BRANCH END-BRANCH
              MOVE "N" TO GROUP-FLAG
              MOVE EXT-EOBUF-GROUP-PRPATH TO PRU-FILE
           ELSE
              IF ( BEG-BRANCH = 0 AND END-BRANCH = 0 )
                 MOVE BEG-BRANCH TO SV-BEG-BR
                 MOVE END-BRANCH TO SV-END-BR
                 MOVE EXT-CONAME-USER-LOC TO BEG-BRANCH END-BRANCH.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-PR-FILE.
           PERFORM OPEN-SP1-FILE.

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           PERFORM OPEN-WK-FILE-OUTPUT.
           PERFORM CLOSE-WK-FILE.

           PERFORM ARRAYSP-LOAD.

           GO TO EXIT-INITIALIZE.
       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.

       EXIT-INITIALIZE.
           EXIT.

      ********************************************
       EO-EXEC-AUDIT SECTION.
      ********************************************
           MOVE EXT-EOBUF-DATE1 TO BEG-LN-DATE.
           MOVE EXT-EOBUF-DATE2 TO END-LN-DATE.

      ********************************************
       ENTER-PARAMETERS SECTION.
      ********************************************
       ENTER-PARM.

           IF VDUSTATUS = "1U"
              GO TO ENTER-MIN-BRANCH.

       BR-01.
           MOVE "E  A040BR1." TO VDU.
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
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO BR-01.

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
              GO TO CL-01.

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
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO BR-02.
           MOVE VDUNUM0 TO END-BRANCH.
           IF BEG-BRANCH > END-BRANCH
              MOVE "INVALID BRANCH RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO BR-01.
           GO TO CL-01.
       ALL-BR.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "N" TO GROUP-FLAG.
       CL-01.
           MOVE "ENNN020CL1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-CL.
           IF VDUSTATUS = "1U" GO TO BR-02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CL-01.
           MOVE VDUNUM0 TO BEG-CL.
       CL-02.
           MOVE "ENNN020CL2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO CL-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CL-02.
           MOVE VDUNUM0 TO END-CL.
           IF BEG-CL > END-CL
              MOVE "INVALID CLASS RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO CL-01.
           GO TO BEGBAL.
       ALL-CL.
           MOVE "SNNN020CL1." TO VDU.
           MOVE 0 TO BEG-CL VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020CL2." TO VDU.
           MOVE 99 TO END-CL VDUNUM0.
           PERFORM SCREENIO.
       BEGBAL.
           MOVE "ENNN060BEGBAL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALLBAL.
           IF VDUSTATUS = "1U" GO TO CL-02.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO BEGBAL.
           MOVE VDUNUM0 TO BEG-BAL.
       ENDBAL.
           MOVE "ENNN060ENDBAL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BEGBAL.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO ENDBAL.
           MOVE VDUNUM0 TO END-BAL.
           IF BEG-BAL > END-BAL
              MOVE "INVALID BALANCE RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO BEGBAL.
           GO TO CA-01.
       ALLBAL.
           MOVE "SNNN060BEGBAL." TO VDU.
           MOVE 0 TO BEG-BAL VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN060ENDBAL." TO VDU.
           MOVE 999999 TO END-BAL VDUNUM0.
           PERFORM SCREENIO.
       CA-01.
           MOVE "ENNN040CAGE1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-CA.
           IF VDUSTATUS = "1U" GO TO ENDBAL.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CA-01.
           MOVE VDUNUM0 TO BEG-CAGE.
       CA-02.
           MOVE "ENNN040CAGE2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-CA.
           IF VDUSTATUS = "1U" GO TO CA-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CA-02.
           MOVE VDUNUM0 TO END-CAGE.
           IF BEG-CAGE > END-CAGE
              MOVE "INVALID CONTRACTUAL AGE RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO CA-01.
           GO TO NM-01.
       ALL-CA.
           MOVE "SNNN040CAGE1." TO VDU.
           MOVE 0 TO BEG-CAGE VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040CAGE2." TO VDU.
           MOVE 9999 TO END-CAGE VDUNUM0.
           PERFORM SCREENIO.
       NM-01.
           MOVE "E  A010NM1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-NM.
           IF VDUSTATUS = "1U" GO TO CA-02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO NM-01.
           MOVE VDUBUF TO BEG-LNAME.
           IF BEG-LNAME = " " GO TO NM-01.
       NM-02.
           MOVE "E  A010NM2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO NM-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO NM-02.
           MOVE VDUBUF TO END-LNAME.
           IF END-LNAME = " " GO TO NM-02.
           IF BEG-LNAME > END-LNAME
              MOVE " INVALID NAME RANGE !" TO MESS
              PERFORM SEND-MESS
              GO TO NM-01.
           GO TO DT-01.
       ALL-NM.
           MOVE "S  A010NM1." TO VDU.
           MOVE "A" TO BEG-LNAME VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A010NM2." TO VDU.
           MOVE "Z" TO END-LNAME VDUBUF.
           PERFORM SCREENIO.

       DT-01.
           MOVE "ERNM060LDATE1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-DT.
           IF VDUSTATUS = "1U" GO TO NM-02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO DT-01.
           MOVE VDU-DATE-YYYYMMDD TO BEG-LN-DATE.
       DT-02.
           MOVE "ERNM060LDATE2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO DT-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO DT-02.
           MOVE VDU-DATE-YYYYMMDD TO END-LN-DATE.

           IF END-LN-DATE < BEG-LN-DATE
              MOVE "INVALID DATE RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO DT-01.

           GO TO REC-SRCD1.
       ALL-DT.
           MOVE "S  A080LDATE1." TO VDU.
           MOVE EXT-JULIAN-BEG TO BEG-LN-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

           MOVE "S  A080LDATE2." TO VDU.
           MOVE EXT-JULIAN-END TO END-LN-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

       REC-SRCD1.
           MOVE "E  A020SRCD1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO DT-02.
           IF VDUSTATUS = "16" GO TO ALL-SRCD.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-SRCD1.
           MOVE VDUBUF TO SRCD(1).
           IF SRCD(1) = "**" GO TO REC-SRCD1.
           GO TO END-SRCD1.

       ALL-SRCD.
           MOVE SPACES TO SRCD-TABLE.
           MOVE "**" TO SRCD(1).
           MOVE SRCD-TABLE TO WR-BUF.
           MOVE SRCD-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
           GO TO ANOEND.
       END-SRCD1.

       REC-SRCD2.
           MOVE "E  A020SRCD2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-SRCD1.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-SRCD2.
           IF VDUBUF = SPACES
              GO TO ANOEND.
           MOVE VDUBUF TO SRCD(2).
       REC-SRCD3.
           MOVE "E  A020SRCD3." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-SRCD2.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-SRCD3.
           IF VDUBUF = SPACES
              GO TO ANOEND.
           MOVE VDUBUF TO SRCD(3).
       REC-SRCD4.
           MOVE "E  A020SRCD4." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-SRCD3.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-SRCD4.
           IF VDUBUF = SPACES
              GO TO ANOEND.
           MOVE VDUBUF TO SRCD(4).
       REC-SRCD5.
           MOVE "E  A020SRCD5." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-SRCD4.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-SRCD5.
           IF VDUBUF = SPACES
              GO TO ANOEND.
           MOVE VDUBUF TO SRCD(5).
       REC-SRCD6.
           MOVE "E  A020SRCD6." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-SRCD5
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-SRCD6.
           IF VDUBUF = SPACES
              GO TO ANOEND.
           MOVE VDUBUF TO SRCD(6).
       REC-SRCD7.
           MOVE "E  A020SRCD7." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-SRCD6.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-SRCD7.
           IF VDUBUF = SPACES
              GO TO ANOEND.
           MOVE VDUBUF TO SRCD(7).
       REC-SRCD8.
           MOVE "E  A020SRCD8." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-SRCD7
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-SRCD8.
           IF VDUBUF = SPACES
              GO TO ANOEND.
           MOVE VDUBUF TO SRCD(8).
       REC-SRCD9.
           MOVE "E  A020SRCD9." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-SRCD8.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-SRCD9.
           IF VDUBUF = SPACES
              GO TO ANOEND.
           MOVE VDUBUF TO SRCD(9).
       REC-SRCD10.
           MOVE "E  A020SRCD10." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REC-SRCD9.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REC-SRCD10.
           IF VDUBUF = SPACES
              GO TO ANOEND.
           MOVE VDUBUF TO SRCD(10).

      ***************************************
       ANOEND.
           MOVE "E  A010ANOEND." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALLANOEND.
           IF VDUSTATUS = "1U" GO TO REC-SRCD10.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO ANOEND.
           MOVE VDUBUF TO ANOEND-BUF.
           IF ANOEND-BUF NOT NUMERIC
              PERFORM INVALID-09
              GO TO ANOEND.
           GO TO SKIP.
       ALLANOEND.
           MOVE "S  A010ANOEND." TO VDU.
           MOVE "*" TO ANOEND-BUFX VDUBUF.
           PERFORM SCREENIO.
      ***************************************
       SKIP.
           MOVE "ENNN030SKIP." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ANOEND.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO SKIP.
           MOVE VDUNUM0 TO SKIP-BUF.
           IF SKIP-BUF = 0
              MOVE "ENTER: 1 - 999 ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO SKIP.
      ***************************************
       CONFIDS.
           MOVE "E  A010CONFID." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ENTER-MIN-BRANCH.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CONFIDS.
           MOVE VDUBUF TO CONFID-BUF.
           IF CONFID-BUF NOT = "Y" AND NOT = "N"
              PERFORM INVALID-YN
              GO TO CONFIDS.
      ***************************************
       ALLOTS.
           MOVE "E  A010ALLOT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO CONFIDS.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO ALLOTS.
           MOVE VDUBUF TO ALLOT-BUF.
           IF ALLOT-BUF NOT = "Y" AND NOT = "N"
              PERFORM INVALID-YN
              GO TO ALLOTS.
       LBOXS.
           MOVE "E  A010LBOX." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ALLOTS.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO LBOXS.
           MOVE VDUBUF TO LBOX-BUF.
           IF NOT (LBOX-BUF = "Y" OR "N")
              PERFORM INVALID-YN
              GO TO LBOXS.
       ZBALS.
           MOVE "E  A010ZBAL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO LBOXS.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO ZBALS.
           MOVE VDUBUF TO ZBAL-BUF.
           IF ZBAL-BUF NOT = "Y" AND NOT = "N"
              PERFORM INVALID-YN
              GO TO ZBALS.
       PLS.
           MOVE "E  A010PL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ZBALS.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO PLS.
           MOVE VDUBUF TO PL-BUF.
           IF PL-BUF NOT = "Y" AND NOT = "N"
              PERFORM INVALID-YN
              GO TO PLS.
           IF PL-BUF NOT = "Y"
              GO TO JDS.
       JDS.
           MOVE "E  A010JD." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO PLS.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO JDS.
           MOVE VDUBUF TO JD-BUF.
           IF JD-BUF NOT = "Y" AND NOT = "N"
              PERFORM INVALID-YN
              GO TO JDS.
       DEBTS.
           MOVE "E  A010DEBT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO JDS.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO DEBTS.
           MOVE VDUBUF TO DEBT-BUF.
           IF DEBT-BUF NOT = "Y" AND NOT = "N"
              PERFORM INVALID-YN
              GO TO DEBTS.
       LEGALS.
           MOVE "E  A010LEGAL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO DEBTS.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO LEGALS.
           MOVE VDUBUF TO LEGAL-BUF.
           IF LEGAL-BUF NOT = "Y" AND NOT = "N"
              PERFORM INVALID-YN
              GO TO LEGALS.
       BANKS.
           MOVE "E  A010BANK." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO LEGALS.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO BANKS.
           MOVE VDUBUF TO BANK-BUF.
           IF BANK-BUF NOT = "Y" AND NOT = "N"
              PERFORM INVALID-YN
              GO TO BANKS.
       REPOS.
           MOVE "E  A010REPO." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BANKS.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO REPOS.
           MOVE VDUBUF TO REPO-BUF.
           IF REPO-BUF NOT = "Y" AND NOT = "N"
              PERFORM INVALID-YN
              GO TO REPOS.
       POBOXES.
           MOVE "E  A010POBOX." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REPOS.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO POBOXES.
           MOVE VDUBUF TO POBOX-BUF.
           IF POBOX-BUF NOT = "Y" AND NOT = "N"
              PERFORM INVALID-YN
              GO TO POBOXES.
      **********************************************
       ENTER-MIN-BRANCH.
           MOVE "ENNN020MIN." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO SKIP.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO ENTER-MIN-BRANCH.
           MOVE VDUNUM0 TO MIN-BRANCH.


       EXIT-PARM.
           EXIT.

      ****************************
       DISPLAY-PARAMETERS SECTION.
      ****************************

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
           MOVE "SNNN020CL1." TO VDU.
           MOVE BEG-CL TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020CL2." TO VDU.
           MOVE END-CL TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN060BEGBAL." TO VDU.
           MOVE BEG-BAL TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN060ENDBAL." TO VDU.
           MOVE END-BAL TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040CAGE1." TO VDU.
           MOVE BEG-CAGE TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040CAGE2." TO VDU.
           MOVE END-CAGE TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "S  A010NM1." TO VDU.
           MOVE BEG-LNAME TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A010NM2." TO VDU.
           MOVE END-LNAME TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A080LDATE1." TO VDU.
           MOVE BEG-LN-DATE TO VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
           MOVE "S  A080LDATE2." TO VDU.
           MOVE END-LN-DATE TO VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

           MOVE SRCD-TABLE TO WR-BUF.
           MOVE SRCD-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
           MOVE "S  A010ANOEND." TO VDU.
           MOVE ANOEND-BUFX TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A030SKIP." TO VDU.
           MOVE SKIP-BUF TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A020MIN." TO VDU.
           MOVE MIN-BRANCH TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A010PL." TO VDU.
           MOVE PL-BUF TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A010JD." TO VDU.
           MOVE JD-BUF TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A010ZBAL." TO VDU.
           MOVE ZBAL-BUF TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A010CONFID." TO VDU.
           MOVE CONFID-BUF TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A010ALLOT." TO VDU.
           MOVE ALLOT-BUF TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A010LBOX." TO VDU.
           MOVE LBOX-BUF TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A010DEBT." TO VDU.
           MOVE DEBT-BUF TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A010LEGAL." TO VDU.
           MOVE LEGAL-BUF TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A010BANK." TO VDU.
           MOVE BANK-BUF TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A010REPO." TO VDU.
           MOVE REPO-BUF TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A010POBOX." TO VDU.
           MOVE POBOX-BUF TO VDUBUF.
           PERFORM SCREENIO.


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
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.

       END-HEAD-PAGE.
           MOVE 4 TO LN-COUNT.
           MOVE 2 TO LN-FEED.

      ****************************
       PRINT-RANGE-LINES SECTION.
      ****************************
           MOVE 3 TO LN-FEED.
           IF GROUP-FLAG = "Y"
              MOVE "GROUP ID                : " TO RANGE-LINE
              MOVE ENT-GROUP                   TO RANGE-SELECTION
           ELSE
              MOVE "BEGINNING BRANCH        : " TO RANGE-LINE
              MOVE BEG-BRANCH                  TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE
              MOVE "ENDING                  : " TO RANGE-LINE
              MOVE END-BRANCH                  TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
           MOVE 1 TO LN-FEED.
      
           MOVE "BEGINNING CLASS         : " TO RANGE-LINE.
           MOVE BEG-CL                         TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "ENDING                  : " TO RANGE-LINE.
           MOVE END-CL                         TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "BEGINNING BALANCE       : " TO RANGE-LINE.
           MOVE BEG-BAL                        TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "ENDING                  : " TO RANGE-LINE.
           MOVE END-BAL                        TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "CONTRACTUAL AGE         : " TO RANGE-LINE.
           MOVE BEG-CAGE                       TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "ENDING                  : " TO RANGE-LINE.
           MOVE END-CAGE                       TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "BEGINNING LAST NAME     : " TO RANGE-LINE.
           MOVE BEG-LNAME                      TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "ENDING                  : " TO RANGE-LINE.
           MOVE END-LNAME                      TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "BEG LOAN DATE           : " TO RANGE-LINE.
           MOVE  BEG-LN-DATE                 TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE  DATE-MMDDYYYY               TO RANGE-DATE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "END LOAN DATE           : " TO RANGE-LINE.
           MOVE  END-LN-DATE                 TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE  DATE-MMDDYYYY               TO RANGE-DATE.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "ACCOUNTS ENDING IN      : " TO RANGE-LINE.
           MOVE ANOEND-BUF                     TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "SELECTED SOURCE CODES   : "  TO RANGE-LINE.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 10
              MOVE SRCD(SUB)TO D-SRCD(SUB)
           END-PERFORM.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "INCLUDE EVERY X SELECTED: " TO RANGE-LINE.
           MOVE SKIP-BUF                       TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "INCLUDE CONFIDENTIAL    : " TO RANGE-LINE.
           MOVE CONFID-BUF                     TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "INCLUDE ALLOTMENTS      : " TO RANGE-LINE.
           MOVE ALLOT-BUF                      TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "INCLUDE LOCK BOXES      : " TO RANGE-LINE.
           MOVE LBOX-BUF                     TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "INCLUDE ZERO BALS       : " TO RANGE-LINE.
           MOVE ZBAL-BUF                       TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "INCLUDE P&L'S           : " TO RANGE-LINE.
           MOVE PL-BUF                         TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "INCLUDE JUDGEMENTS      : " TO RANGE-LINE.
           MOVE JD-BUF                         TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "INCLUDE DEBT AID        : " TO RANGE-LINE.
           MOVE DEBT-BUF                       TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "INCLUDE LEGAL           : " TO RANGE-LINE.
           MOVE LEGAL-BUF                      TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "INCLUDE BANKRUPTS       : " TO RANGE-LINE.
           MOVE BANK-BUF                       TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "INCLUDE REPOS           : " TO RANGE-LINE.
           MOVE REPO-BUF                       TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "INCLUDE PO BOXES        : " TO RANGE-LINE.
           MOVE POBOX-BUF                      TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "MINIMUM PER BRANCH      : " TO RANGE-LINE.
           MOVE MIN-BRANCH                   TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           PERFORM WRITE-DETAIL-LINE.

      ************************************************
       FORM-RESET SECTION.
      ************************************************
           MOVE EXT-CONAME-CONAME TO AUDEXT-CONAME.
           MOVE EXT-CONAME-SYSDATE TO AUDEXT-SYSDATE.
           DISPLAY AUDEXT-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE AUDEXT-FORM-FIELDS TO WR-BUF.
           MOVE AUDEXT-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE AUDEXT-HELP--FILE TO HELP-LINK-FILE.
           MOVE AUDEXT-HELP--DIR TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/AUDEXT_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      **********************************
       FIND-FEE-CODES SECTION.
      **********************************
           IF LN-FEECODE(SUB) = "93" OR "94"
              ADD LN-FEEAMT(SUB) TO UMC-FEE
           ELSE
           IF LN-FEECODE(SUB) = "97"
              ADD LN-FEEAMT(SUB) TO ADD-FEE.

       EXIT-FIND-FEE-CODES.
           EXIT.

      ************************************
       COMPUTE-AGEING-CAT SECTION.
      ************************************
           MOVE LN-ORGST    TO SP-ORGST.
           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           MOVE LN-LAWCODE  TO SP-LAWCODE.
           IF LPWSSP-SP1-KEY NOT = SP1-KEY
              MOVE SP1-KEY TO LPWSSP-SP1-KEY
      *        PERFORM READ-SP1-FILE
              PERFORM ARRAYSP-READ
              IF NOT IO-GOOD
                 GO TO IO-ERROR.

           MOVE SYSTEM-DATE TO AGEING-DATE.
           PERFORM COMPUTE-CONTRACTUAL.

       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/LPPDUE.CPY".
       COPY "LIBGB/AGEING.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBGB/SPPDTH.CPY".
       COPY "LIBLP/LPCEPP.CPY".
       COPY "LIBLP/ARRAYSP.CPY".

      **********************************************
       MISC SECTION.
      **********************************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/RMFILE.CPY".
       INVALID-YN.
           MOVE "ENTER: 'Y' OR 'N' ONLY" TO MESS.
           PERFORM SEND-MESS.
       INVALID-09.
           MOVE "ENTER: 0 - 9 ONLY" TO MESS.
           PERFORM SEND-MESS.

      ***********************************
       IO-ROUTINE SECTION.
      ***********************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLTPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".

       COPY "LIBLP/LPLTP1RN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".

      *COPY "LIBLP/LPLN3RN.CPY".
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

           IF LN3-CONN-OPEN-SW = "N" 
               EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS LN3_CONNECT
                    USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
               END-EXEC
               MOVE "Y" TO LN3-CONN-OPEN-SW
               EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       START-LN3-FILE.  *> SSP LN BW
           PERFORM START-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-PATH-SQL    TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING LN-OWNBR, LN-CLASS
              DELIMITED BY " " INTO E-KEYX.

           IF ( LN3-CURSOR-STAT-GOOD )
              PERFORM CLOSE-LN3-FILE.

           EXEC SQL SET CONNECTION LN3_CONNECT END-EXEC.

           EXEC SQL
            DECLARE LN3_CURSOR CURSOR FOR
              :LN3-RETURN-CODE = 
               EXEC SSP_AUDEXT_LNFILE_SELECT
                 (:QLN3-WBEG-OWNBR,:QLN3-WBEG-CLASS,
                  :QLN3-WEND-CLASS,:PASS-INC-ZBAL,:PASS-INC-PL,
                  :PASS-INC-BNKRPT,:PASS-INC-CONFID,:PASS-INC-REPO,
                  :PASS-INC-SRCD1,:PASS-INC-SRCD2,:PASS-INC-SRCD3,
                  :PASS-INC-SRCD4,:PASS-INC-SRCD5,:PASS-INC-SRCD6,
                  :PASS-INC-SRCD7,:PASS-INC-SRCD8,:PASS-INC-SRCD9,
                  :PASS-INC-SRCD9,:PASS-BEG-LN-DATE,:PASS-END-LN-DATE,
                  :PASS-OFFSET)
               END-EXEC.

      * 'SELECT LNFILE.LN_OWNBR,
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
      *            LNFILE.LN_SRCD,
      *            LNFILE.LN_POCD,
      *            LNFILE.LN_PLCD,
      *            LNFILE.LN_CONFIDENTCD,
      *            LNFILE.LN_ALLOTCD,
      *            LNFILE.LN_PAY_SCHLD_FG,
      *            CAST(LNFILE.LN_LOANDATE       AS VARCHAR(10)),
      *            CAST(LNFILE.LN_ORIG_1STPYDATE AS VARCHAR(10)),
      *            CAST(LNFILE.LN_1STPYDATE      AS VARCHAR(10)),
      *            CAST(LNFILE.LN_JDDATE         AS VARCHAR(10)),
      *            CAST(LNFILE.LN_PLDATE         AS VARCHAR(10)),
      *            CAST(LNFILE.LN_LEGALDATE      AS VARCHAR(10)),
      *            CAST(LNFILE.LN_BNKRPTDATE     AS VARCHAR(10)),
      *            CAST(LNFILE.LN_DEBTAIDATE     AS VARCHAR(10)),
      *            LNFILE.LN_PROCEEDS,
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
      *            LNFILE.LN_PY_YYYYMM_RECDEL,
      *            LNFILE.LN_MTD_PAYS_RECDEL,
      *            LNFILE.LN_CURBAL,
      *            LNFILE.LN_INTBAL,
      *            LNFILE.LN_LCBAL,
      *            LNFILE.LN_OTHBAL,
      *            LNFILE.LN_TOTPAYMNTD,
      *            LNFILE.LN_TOTNODEF,
      *            LNFILE.LN_TOTNOALDEL,
      *            CAST(LNFILE.LN_DATE_PAID_LAST AS VARCHAR(10)),
      *            LNFILE.LN_SUM_ORGTERM,
      *            LNFILE.LN_UNITPER_CD,
      *            LNFILE.LN_UNITPER_FREQ,
      *            LNFILE.LN_REPOCD,
      *            LNFILE.LN_LBOX,
      *            ISNULL(BWFILE.BW_OWNBR,0),
      *            ISNULL(BWFILE.BW_SSNO,0),
      *            ISNULL(BWFILE.BW_FNAME,''''),
      *            ISNULL(BWFILE.BW_LNAME,''''),
      *            ISNULL(BWFILE.BW_ADR1,''''),
      *            ISNULL(BWFILE.BW_ADR2,''''),
      *            ISNULL(BWFILE.BW_CITY,''''),
      *            ISNULL(BWFILE.BW_STATE,''''),
      *            ISNULL(BWFILE.BW_ZIP,''''),
      *            ISNULL(BWFILE.BW_CONFIDENTIAL,'''')
      *     FROM DBO.LNFILE LNFILE
      *      LEFT OUTER JOIN DBO.BWFILE BWFILE
      *       ON BWFILE.BW_OWNBR = LNFILE.LN_OWNBR 
      *      AND BWFILE.BW_SSNO  = LNFILE.LN_SSNO1
      *    WHERE LNFILE.LN_OWNBR = @QLN3_WBEG_OWNBR
      *      AND LNFILE.LN_CLASS BETWEEN @QLN3_WBEG_CLASS AND @QLN3_WEND_CLASS
      *      AND LNFILE.LN_POCD NOT IN (''**'', ''*I'', ''*R'', ''*U'')
      *      AND LNFILE.LN_LOANDATE BETWEEN @BEG_LNDATE AND @END_LNDATE'
      *  IF @INCZBAL = 'N'
      *  SELECT @SQL += ' AND (LNFILE.LN_CURBAL <> 0 OR LNFILE.LN_INTBAL <> 0 OR LNFILE.LN_OTHBAL <> 0)'
      * IF @INCPL = 'N'
      *  SELECT @SQL += ' AND (LNFILE.LN_PLDATE = ''1900-01-01'')'
      * IF @INCBNKRPT = 'N'
      *  SELECT @SQL += ' AND (LNFILE.LN_BNKRPTDATE = ''1900-01-01'')'
      * IF @INCCONFID = 'N'
      *  SELECT @SQL += ' AND (LNFILE.LN_CONFIDENTCD != ''Y'')'
      * IF @INCREPO = 'N'
      *  SELECT @SQL += ' AND (LNFILE.LN_REPOCD NOT IN (''X'', ''S''))'
      * IF @INCSRCD1 != '**'
      *  SELECT @SQL += ' AND LNFILE.LN_SRCD IN (''' +  @INCSRCD1 + ''',''' + @INCSRCD2 + ''',''' + @INCSRCD3 + '''' 
      *          +  ',''' + @INCSRCD4 + ''',''' + @INCSRCD5 + ''',''' + @INCSRCD6 + '''' 
      *          +  ',''' + @INCSRCD7 + ''',''' + @INCSRCD8 + ''',''' + @INCSRCD9 + '''' 
      *          +  ',''' + @INCSRCD10 + ''')'
      *    ORDER BY LNFILE.LN_CLASS, LNFILE.LN_ACCTNO 

           EXEC SQL
               OPEN LN3_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           EXEC SQL SET CONNECTION C1 END-EXEC.

           MOVE ZEROES TO QLN3-WBEG-OWNBR
                          QLN3-WBEG-CLASS
                          QLN3-WEND-CLASS.

           MOVE STAT---GOOD TO LN3-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-LN3-FILE-NEXT.   *> EMBEDDED LN BW
           PERFORM READ-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-PATH-SQL    TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING LN-OWNBR, LN-CLASS,LN-ACCTNO
              DELIMITED BY " " INTO E-KEYX.
           INITIALIZE QLN-REC QBW-REC.

           EXEC SQL SET CONNECTION LN3_CONNECT END-EXEC.

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
                   :QLN-SRCD,
                   :QLN-POCD,
                   :QLN-PLCD,
                   :QLN-CONFIDENTCD,
                   :QLN-ALLOTCD,
                   :QLN-PAY-SCHLD-FG,
                   :QLN-LOANDATE,
                   :QLN-ORIG-1STPYDATE,
                   :QLN-1STPYDATE,
                   :QLN-JDDATE,
                   :QLN-PLDATE,
                   :QLN-LEGALDATE,
                   :QLN-BNKRPTDATE,
                   :QLN-DEBTAIDATE,
                   :QLN-PROCEEDS,
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
                   :QLN-PY-YYYYMM-RECDEL,
                   :QLN-MTD-PAYS-RECDEL,
                   :QLN-CURBAL,
                   :QLN-INTBAL,
                   :QLN-LCBAL,
                   :QLN-OTHBAL,
                   :QLN-TOTPAYMNTD,
                   :QLN-TOTNODEF,
                   :QLN-TOTNOALDEL,
                   :QLN-DATE-PAID-LAST,
                   :QLN-SUM-ORGTERM,
                   :QLN-UNITPER-CD,
                   :QLN-UNITPER-FREQ,
                   :QLN-REPOCD,
                   :QLN-LBOX,
                   :QBW-OWNBR, 
                   :QBW-SSNO, 
                   :QBW-FNAME, 
                   :QBW-LNAME, 
                   :QBW-ADR1, 
                   :QBW-ADR2, 
                   :QBW-CITY, 
                   :QBW-STATE, 
                   :QBW-ZIP, 
                   :QBW-CONFIDENTIAL
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           EXEC SQL SET CONNECTION C1 END-EXEC.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LN3-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-LN-FIELDS
              PERFORM GET-BW-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-LN3-FILE.

           PERFORM CLOSE-IT.
           IF ( LN3-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION LN3_CONNECT END-EXEC
              EXEC SQL
                   CLOSE LN3_CURSOR
              END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE STAT---BAD TO LN3-CURSOR-STAT.

      *-----------------------------------------------------------------
       CLOSE-LN3-CONNECTION.

           IF LN3-CONN-OPEN-SW = "Y"
               EXEC SQL SET CONNECTION LN3_CONNECT END-EXEC
               EXEC SQL COMMIT END-EXEC
               EXEC SQL DISCONNECT LN3_CONNECT END-EXEC
               EXEC SQL SET CONNECTION C1 END-EXEC
               MOVE "N" TO LN3-CONN-OPEN-SW.
      *-----------------------------------------------------------------


       COPY "LIBGB/GBWKIN.CPY".
       OPEN-AUDIT-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE AUDIT-PATH TO E-FILE.
           OPEN OUTPUT AUDIT-FILE.
           IF IO-FG = 8
              GO TO OPEN-AUDIT-FILE-OUTPUT.
           IF IO-FG = 7
              CLOSE AUDIT-FILE
              GO TO OPEN-AUDIT-FILE-OUTPUT.
       WRITE-AUDIT-FILE.
           PERFORM WRITE-IT.
           MOVE AUDIT-PATH TO E-FILE.
           WRITE AUDIT-REC.
           IF IO-FG = 8
             GO TO WRITE-AUDIT-FILE.
       CLOSE-AUDIT-FILE.
           PERFORM CLOSE-IT.
           CLOSE AUDIT-FILE.

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      ***********************************
       END-PROGRAM SECTION.
      ***********************************
       END-RUN.
           PERFORM CLOSE-FILES.
           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.

           MOVE MKTEMP-BUF TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.
       EXIT-PROG.
           IF SV-BEG-BR NOT = 9999
              IF EXT-EOBUF-GROUP-FG = "Y"
                 MOVE SV-BEG-BR TO BEG-BRANCH
                 MOVE SV-END-BR TO END-BRANCH
                 MOVE SV-GROUP-FLAG TO GROUP-FLAG
                 MOVE SV-ENT-GROUP TO ENT-GROUP
              ELSE
                 MOVE SV-BEG-BR TO BEG-BRANCH
                 MOVE SV-END-BR TO END-BRANCH.

           IF EXT-ROUTE-BUF = "00"
              MOVE "LP/LPEYMU" TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN 
              DESTROY AUDEXT-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
