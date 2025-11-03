       IDENTIFICATION DIVISION.
       PROGRAM-ID.  EAMAIN.
      ******************************************************************
      *
      *                 (MN)
      *  DESCRIPTION:   EXTRACTION/ACTION FILE MAINTENANCE
      *
      *  REV:
      *  BAH 121688 ADDED 0 RECENCY OPTION FOR CITIZENS
      *  BAH 041789 EXPANDED ALPHA NAME RANGES TO 3 CHARACT.
      *  BAH 100190 ADDED EA-SETFM'S FOR COMAKERS (OCC)
      *  BAH 100390 ADDED SCAN
      *  BAH 062895 ADDED DLNO1, DLNO2, CONTRACTUAL-DAY1,
      *               CONTRACTUAL-DAY2  E021
      *  KEC 020295 ADDED EA-CQ-FG AND EA-GROUP-ID.
      *              ADDED SCAN FOR CODE FOR GROUP ID.
      *  KEC 020796 ADDED LOGIC TO PREVENT ACCESSING 'SET QUEUE' IF
      *              BR-COLLECTOR-QUEUE NOT = 'Y'.
      *  KEC 031396 EA-GROUP-ID WAS MOVED TO BE APART OF THE PRIMARY
      *              KEY OF THE EA-FILE.
      *  MJD 032296 SET NO RESET OF FIELDS IN SPEC TABLE.
      *  BLL 032996 ADD COLLECTOR POOL FILE
      *  KEC 062496 ADDED EA-REPO, EA-BANKRUPT, EA-PL AND
      *              EA-PL-IN-ACTIVE-BOTH; REVISED FORM.
      *  BLV 030497 ADDED EA-CONFIDENTIAL-FG, EA-1ST-PMT-MISSES
      *  BLV 032097 FIX GROUP SCAN
      *   CS 061997 ADDED EA-LEGAL, EA-DEBTAID, EA-JUDGEMENT
      *  MJD 112597 EXPANDED EA-REC TO 222 BYTES
      *   CS 042098 ADDED EA-ORIGIN; CHANGED FORM
      *  BLV 001016 ADDED EA-RPOTS, EA-MISPAYS FOR WORLD PR# 244
      *  BLV 001206 VERIFY IF BRANCH IS GLOBAL QUEUING, SELECT CD-EA-GROUP
      *             DESIGNATED AS GLOBAL QUEUING, AND VICE VERSA.  ALSO,
      *             IF GLOBAL QUEUING, USE GLOBAL CP-PATH AND GLOBAL
      *             COLLECTOR ID'S (CD-CN-BRANCH = 0); TFC PR# 60
      *   CS 010503 ADDED ALLOTMENTS(I,S,O) OPTION PR#1472 (ACE)
      *  BLV 011003 ADDED EA-LNDATE-MONTHS1, EA-LNDATE-MONTHS2,
      *             EA-MATURE-MONTHS1, EA-MATURE-MONTHS2; HAD TO COMPLETELY
      *             REVISE SCREEN TO FIT MORE FIELDS, REGWAN PR# 1587
      *  BLV 011011 CHANGED MONTHS SINCE LOAN DATE & MONTHS TIL MATURITY
      *             TO BE DAYS, REGWAN PR# 1587
      *  BLV 021029 ADDED EA-DAY-OF-MO-DUE1, EA-DAY-OF-MO-DUE2 AS FIELD
      *             13, SHIFTED ALL FIELDS DOWN 1, REGACC PR# 0009
      *  BLV 021030 NOTE, CURRENTLY CHECKING FOR RANGE OF 20-20 FOR NEW
      *             DAY OF MONTH DUE RANGE IN CPRUN, CQRUN - SO WILL GIVE
      *             A WARNING HERE THAT IT WILL NOT WORK
      *  BLV 030421 OOPS, WAS MOVING FIELD 21 (PROMISED) TO FIELD 22 (REPOS)
      *             IN DISPLAY ROUTINE, SO PROMISED SHOWED AS BLANK &
      *             REPOS GOT WIPED OUT BY PROMISED, CASHCO
      *  BLV 030630 MAKE VARIABLE PROMPTS FOR FIELDS 11 & 12, REGACC WILL
      *             USE FOR TRIAD PARAMETERS, REGACC PR# 0033
      *  BLM 130422 ADD EA-BORR-STATE, SECURITY AUTO PR# 4248
      *
      * KEC 2016.0229 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         FIXED KEYS & RECS FOR EA-REC (HOLD/ORIG/SAVE/WK).
      *         CHANGED CLNO/CLASS 2 TO 3 BYTES.
      *         CHANGED FMNO/FORMNO/NEXTFM/SETFM FROM 3 TO 4 BYTES.
      *
      * KEC 2016.0310 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         ADDED CD-EA-FILLER FOR CHANGE TO CD-EA-REC.
      *
      * KEC 2016.0325 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED CDSCAN-WINDOW-BUF TO HAVE MATCHING CD1-KEY
      *         LAYOUTS (FOR EACH CODE TYPE; CD-TYPE).
      *         CDSCAN-CD1-KEY IS THE NEW (PRIMARY) WITH REDEFINES OF
      *         CDSCAN-??-KEY (NEW).
      * CS  170420 ACTIVATE 8 DIGIT DATES, EA-1STFM-DATE1, EA-1STFM-DATE2
      *
      * KEC 2018.0409 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         INCREASE "8. LAST NAME:" AND ITS SECONDARY FROM
      *         3 TO 4 BYTES TO MATCH EA-ALPNAME1 & 2.
      *
      * KEC 2018-1026 CONVERTED CPFILE FOR A32 & A34
      *               CP-PATH-OVERRIDE TO "G" IF GLOBAL QUEUEING.
      *  CS 191111 FIXED SCAN KEY (CLASS)
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
           "@(#)MN/EAMAIN.CBL S35 02/21/24-12:34:12 >".
      *    **********************************
      *    *      PROGRAM WORKERS
      *    **********************************
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01LC.CPY".
       COPY "LIBLP/LP01LC_SQL.CPY".
       COPY "LIBLP/LPWSLC.CPY".
       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LP01CD_SQL.CPY".
       COPY "LIBLP/LPWSCD.CPY".
       COPY "LIBLP/LP01CDB.CPY".
       COPY "LIBLP/LP01CDB_SQL.CPY".
       COPY "LIBLP/LPWSCDB.CPY".
       COPY "LIBLP/LP01EA.CPY".
       COPY "LIBLP/LP01EA_SQL.CPY".
       COPY "LIBLP/LPWSEA.CPY".
       COPY "LIBLP/LP01FD.CPY".
       COPY "LIBLP/LP01FD_SQL.CPY".
       COPY "LIBLP/LPWSFD.CPY".
       COPY "LIBSP/SP01CP.CPY".
       COPY "LIBSP/SP01CP_SQL.CPY".
       COPY "LIBSP/SPWSCP.CPY".

       01  ERRCD           PIC X       VALUE SPACES.
       01  CHAN            PIC X(4)    VALUE "CHAN".
       01  ONE             PIC S9.
       01  ELE             PIC 9(2).
       01  ACTION.
           03  CHKEY       PIC X(4).
           03  CHNO        PIC 9(2).
       01  IO-TYPE         PIC X(3).
       01  NEW-FLAG        PIC 9.
       01  FIRST-SCAN      PIC 9       VALUE 0.
       01  BRANCH-NO       PIC 9(4)    VALUE 0.
       01  HOLD-EA-GQ-FG   PIC X       VALUE "N".

       01  HOLD-EA1-KEY    PIC X(15) VALUE SPACES.

      * USED FOR THE SCAN KEY ENTRY AND I GUESS WE ARE LEAVING THE CLASS 2 FOR
      * NOW
       01  WORK-EA1-KEY                PIC X(15).
       01  WORK-EA1-KEYX               REDEFINES WORK-EA1-KEY.
           03  WORK-EA-BRANCH          PIC 9(4).
           03  WORK-EA-CLASS           PIC 9(2).
           03  WORK-EA-GROUP-ID        PIC X(6).
           03  WORK-EA-SEQNO           PIC 99.

       01  PROMPTS.
           03  PROMPT-35.
               05 FILLER             PIC X(41) VALUE
               "'N'-DON'T SET COLLECTOR; 'Y'-SET LOAN COL".
               05 FILLER             PIC X(25) VALUE
               "LECTOR; 'P'-SET FROM POOL".

       01  PROMPT-261                PIC X(17) VALUE
           "ACT, INACT, BOTH:".
       01  PROMPT-261-LIST           PIC X(10) VALUE
           "PROMPT261.".
       01  PROMPT-361                PIC X(17) VALUE
           "COLLECTOR ID:".
       01  PROMPT-361-P              PIC X(17) VALUE
           "COLLECTOR POOL ID".
       01  PROMPT-361-LIST           PIC X(10) VALUE
           "PROMPT361.".

       01  ORIGINAL-EA-REC             PIC X(EA-SIZE)   VALUE SPACES.
       01  CHANGED-EA-REC              PIC X(EA-SIZE)   VALUE SPACES.

      *----------------------------------------------------------------
      * CODE-FOUND     : CODE IS " " OR EXISTS IN CD-FILE.
      * CODE-NOT FOUND : CODE DOES NOT EXIST IN CD-FILE.
      *----------------------------------------------------------------
       01  CODE-FOUND        PIC X     VALUE "Y".
           88 CODE-NOT-FOUND           VALUE "N".

      ******************************************************************
      *
      *    S C R E E N - D I S P L A Y - B U F F E R
      *
      ******************************************************************
       01  DISPLAY-BUF     VALUE SPACES.
           03  F01         PIC ZZZ9.
           03  F011        PIC ZZZ9.
           03  F02         PIC ZZZZ.99.
           03  F021        PIC ZZZZ.99.
           03  F03         PIC ZZZZZZ.99.
           03  F031        PIC ZZZZZZ.99.
           03  F04         PIC 99/99/99.
           03  F041        PIC 99/99/99.
           03  F05         PIC ZZZ9.
           03  F051        PIC ZZZ9.
           03  F06         PIC ZZZ9-.
           03  F061        PIC ZZZ9-.
           03  F07         PIC ZZZ9.
           03  F071        PIC ZZZ9.
           03  F08         PIC X(04).
           03  F081        PIC X(04).
           03  F09         PIC ZZZ9-.
           03  F091        PIC ZZZ9-.
           03  F10         PIC ZZZ9.99.
           03  F101        PIC ZZZ9.99.
           03  F11         PIC ZZZ9.
           03  F111        PIC ZZZ9.
           03  F12         PIC ZZZ9.
           03  F121        PIC ZZZ9.
           03  F13         PIC Z9.
           03  F131        PIC Z9.
           03  F14         PIC XXXX.
           03  F15         PIC XXXX.
           03  F16         PIC X.
           03  F17         PIC X.
           03  F18         PIC X.
           03  F19         PIC X.
           03  F20         PIC X.
           03  F21         PIC X.
           03  F22         PIC X.
           03  F23         PIC X.
           03  F24         PIC X.
           03  F25         PIC X.
           03  F26         PIC X.
           03  FPROMPT261  PIC X(15).
           03  F261        PIC X.
           03  F27         PIC X.
           03  F28         PIC X.
           03  F29         PIC X.
           03  F30         PIC X.
           03  F31         PIC XX.
           03  F32         PIC ZZ9.
           03  F321        PIC ZZ9.
           03  F322        PIC ZZ9.
           03  F323        PIC ZZ9.
           03  F33         PIC XXXX.
           03  F34         PIC X.
           03  F35         PIC Z9.
           03  F36         PIC X.
           03  FPROMPT361  PIC X(17).
           03  F361        PIC X(3).
           03  F37         PIC X(60).
      *-----------------------------------------------------------------
       01  DISPLAY-LIST.
           03  FILLER      PIC X(40)   VALUE
           "010 011 020 021 030 031 040 041 050 051 ".
           03  FILLER      PIC X(40)   VALUE
           "060 061 070 071 080 081 090 091 100 101 ".
           03  FILLER      PIC X(40)   VALUE
           "110 111 120 121 130 131 140 150 160 170 ".
           03  FILLER      PIC X(50)   VALUE
           "180 190 200 210 220 230 240 250 260 PROMPT261 261 ".
           03  FILLER      PIC X(44)   VALUE
           "270 280 290 300 310 320 321 322 323 330 340 ".
           03  FILLER      PIC X(26)    VALUE
           "350 360 PROMPT361 361 370.".

      ******************************************************************
      *
      *    E L E M E N T - S P E C I F I C A T I O N S
      *
      ******************************************************************
       01  MAX             PIC 99      VALUE 37.
       01  SPEC-TABLE.
      * 01 BEGINNING PAST DUE DAYS:  EA-PASTDUE-DAY1
           03  FILLER      PIC X(8)    VALUE "ENNN040 ".
      * 02 BEGINNING PAST DUE AMOUNT:  EA-PASTDUE-AMT1
           03  FILLER      PIC X(8)    VALUE "ENNN042 ".
      * 03 BEGINNING BALANCE DUE:  EA-ACCT-BAL1
           03  FILLER      PIC X(8)    VALUE "ENNN062 ".
      * 04 BEGINNING LAST FORM DATE:  EA-LSTFM-DATE1
           03  FILLER      PIC X(8)    VALUE "E NM060 ".
      * 05 BEGINNING LAST FORM #:  EA-LSTFM-NO1
           03  FILLER      PIC X(8)    VALUE "ENNN040 ".
      * 06 BEGINNING PAST PROMISE:  EA-PASTPROM-DAY1
           03  FILLER      PIC X(8)    VALUE "ENNS040 ".
      * 07 BEGINNING RECENCY:  EA-RECENCY-DAY1
           03  FILLER      PIC X(8)    VALUE "ENNN040 ".
      * 08 BEGINNING LAST NAME:  EA-ALPNAME1
           03  FILLER      PIC X(8)    VALUE "E NA040 ".
      * 09 BEGINNING CONTRACTUAL:  EA-CONTRACTUAL-DAY1
           03  FILLER      PIC X(8)    VALUE "ENNS040 ".
      * 10 BEGINNING DEALER:  EA-DLNO1
           03  FILLER      PIC X(8)    VALUE "ENNN042 ".
      * 11 BEGINNING DAYS SINCE LOAN DATE:  EA-LNDATE-DAYS1
           03  FILLER      PIC X(8)    VALUE "ENNN040 ".
      * 12 BEGINNING DAYS UNTIL MATURITY:  EA-MATURE-DAYS1
           03  FILLER      PIC X(8)    VALUE "ENNN040 ".
      * 13 BEGINNING DAY OF MONTH DUE:  EA-DAY-OF-MO-DUE1
           03  FILLER      PIC X(8)    VALUE "ENNN020 ".
      * 14 INCLUDE ACTION:  EA-ACTION
           03  FILLER      PIC X(8)    VALUE "E NA040 ".
      * 15 INCLUDE ORIGIN:  EA-ORIGIN
           03  FILLER      PIC X(8)    VALUE "E NA040 ".
      * 16 LOANDATE RECENCY:  EA-RECENCYDATE
           03  FILLER      PIC X(8)    VALUE "E NA010 ".
      * 17 CONTRACTUAL POTENTIAL 30'S: EA-CPOTS
           03  FILLER      PIC X(8)    VALUE "E NA010 ".
      * 18 RECENCY POTENTIAL 30'S: EA-RPOTS
           03  FILLER      PIC X(8)    VALUE "E NA010 ".
      * 19 MISS PAYS:  EA-MISPAYS
           03  FILLER      PIC X(8)    VALUE "E NA010 ".
      * 20 MATURED ACCOUNTS: EA-MATURED
           03  FILLER      PIC X(8)    VALUE "E NA010 ".
      * 21 PROMISED ACCOUNTS:  EA-PROMISED
           03  FILLER      PIC X(8)    VALUE "E NA010 ".
      * 22 REPO ACCOUNTS:  EA-REPO
           03  FILLER      PIC X(8)    VALUE "E NA010 ".
      * 23 LEGAL ACCOUNTS:  EA-LEGAL
           03  FILLER      PIC X(8)    VALUE "E NA010 ".
      * 24 DEBT AID ACCOUNTS:  EA-DEBTAID
           03  FILLER      PIC X(8)    VALUE "E NA010 ".
      * 25 BANKRUPT ACCOUNTS: EA-BANKUPT
           03  FILLER      PIC X(8)    VALUE "E NA010 ".
      * 26 P&L ACCOUNTS:  EA-PL
           03  FILLER      PIC X(8)    VALUE "E NA010 ".
      * 27 JUDGEMENTS: EA-JUDGEMENT
           03  FILLER      PIC X(8)    VALUE "E NA010 ".
      * 28 CONFIDENTIAL: EA-CONFIDENTIAL-FG
           03  FILLER      PIC X(8)    VALUE "E NA010 ".
      * 29 FIRST PAYMENT MISSES:  EA-1ST-PMT-MISSES
           03  FILLER      PIC X(8)    VALUE "E NA010 ".
      * 30 ALLOTMENTS: EA-ALLOTMENTS
           03  FILLER      PIC X(8)    VALUE "E NA010 ".
      * 31 PRIMARY BORROWERS STATE (1 OR ALL): EA-BORR-STATE
           03  FILLER      PIC X(8)    VALUE "E NA020 ".
      * 32 PRINT FORM, BORROWER 1, 2, 3, 4: EA-SETFM(1,2,3,4)
           03  FILLER      PIC X(8)    VALUE "ENNN030 ".
      * 33 SET ACTION CODE: EA-SETACT-CODE
           03  FILLER      PIC X(8)    VALUE "E NA040 ".
      * 34 INCREMENT DELINQUENCY COUNTER: EA-INC-CNT
           03  FILLER      PIC X(8)    VALUE "E NA010 ".
      * 35 SET PROMISE DATE INCREMENT: EA-INC-PROMISE
           03  FILLER      PIC X(8)    VALUE "ENNN020 ".
      * 36 SET-LOAN-COLLECTOR (Y/N/P) = YES, NO, OR USE COLLECTOR POOL
           03  FILLER      PIC X(8)    VALUE "E NA010 ".
      * 37 DESCRIPTION: EA-DESC
           03  FILLER      PIC X(8)    VALUE "E NA600 ".
      *-----------------------------------------------------------------
       01  SPEC-TABLE2     REDEFINES SPEC-TABLE.
           03  SPEC-REC    OCCURS 37.
               05  SPEC    PIC X(7).
               05  CHFG    PIC X.
      ******************************************************************
      *
      *     C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBWI/CDBSCNW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBWI/CPSCANW.CPY".
       COPY "LIBWI/CDSCANW.CPY".

       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       01  EAMAIN-WINDOW-HANDLE  PIC X(10).
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/EAMAIN_DEF.CPY".
       COPY "LIBMN/EAMAIN_WKS.CPY".

       COPY "LIBGB/GETFMW.CPY".
       01  CHANGE-BRNO-FG  PIC X       VALUE "N".

       SCREEN SECTION.
       COPY "LIBMN/EAMAIN_SCN.CPY".

      ******************************************************************
      *
      *     P R O C E D U R E - D I V I S I O N
      *
      ******************************************************************
       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME CHANGE-BRNO-FG.

      ******************************************************************
      *
      *     M A I N - P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.
           PERFORM SQL-CONNECT.
      D    STOP MESS.

       COPY "LIBGB/CHKSEC.CPY".

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "EAMAIN" TO VDU-FORM-NAME.

      * IF THIS WAS STANDARD, IT WOULD NOT CLEAR PREVIOUS MENU
           DISPLAY FLOATING GRAPHICAL WINDOW
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   ERASE SCREEN
                   HANDLE IS EAMAIN-WINDOW-HANDLE.

           MOVE EXT-CONAME-DAILY-PASSWORD TO CHAN.
           MOVE EXT-FILPATH-BRANCH TO BRANCH-NO.

       MAIN-MODULE.

           PERFORM FORM-RESET.

           PERFORM REC-EA-KEY-MODULE.
           IF IO-TYPE = "CLR"
              GO TO MAIN-MODULE.
           IF IO-TYPE = "END"
              GO TO END-ROUTINE.

           PERFORM ENTRY-MODULE.
           IF IO-TYPE = "RWR"
              PERFORM WRITE-MODULE.
           IF IO-TYPE = "DEL"
              PERFORM DELETE-MODULE.

           GO TO MAIN-MODULE.

      ******************************************************************
       REC-EA-KEY-MODULE SECTION.
      ******************************************************************
       REC-EA-KEY-MODULE-NO.

           MOVE 0 TO EA-BRANCH EA-CLASS EA-SEQNO.
           MOVE SPACES TO IO-TYPE.
           MOVE 0 TO NEW-FLAG.

      * WHETHER OR NOT TO ALLOW ENTERING DIFFERENT BRANCH NUMBERS
      * IN COLLECTION MENU, YOU CAN'T CHANGE BRNO; BUT IN MAINTENANCE
      * YOU CAN:
           IF CHANGE-BRNO-FG = "N"
              MOVE "SN N040BRNO." TO VDU
              MOVE BRANCH-NO TO EA-BRANCH VDUNUM0
              PERFORM SCREENIO
              PERFORM VERIFY-BRNO
              GO TO REC-EA-CLASS.

       ENTER-BRANCH.
           MOVE "EN N040BRNO." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1<"
              PERFORM ENTER-SCANKEY
              GO TO END-REC-EA-KEY-MODULE.

           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-REC-EA-KEY-MODULE.

           IF VDUSTATUS NOT = "00"
              GO TO REC-EA-KEY-MODULE-NO.

           MOVE VDUNUM0 TO EA-BRANCH.
           PERFORM VERIFY-BRNO.

           IF IO-FG NOT = 0
              GO TO REC-EA-KEY-MODULE-NO.

      *-----------------------------------------------------------------
       REC-EA-CLASS.

           MOVE "S  A200CLSDESC." TO VDU.
           MOVE SPACES TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "EN N020CLS." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" OR "1U"
              GO TO REC-EA-KEY-MODULE-NO.

           IF VDUSTATUS = "1<"
              IF VDUNUM0 NOT = 0
                 MOVE VDUNUM0 TO EA-CLASS
                 END-IF
              PERFORM ENTER-SCANKEY
              IF IO-TYPE = SPACES
                 PERFORM VERIFY-GROUP
                 IF IO-FG = 0
                    GO TO END-REC-EA-KEY-MODULE
                 ELSE
                    GO TO REC-EA-KEY-MODULE-NO
              ELSE
                 GO TO END-REC-EA-KEY-MODULE.

           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-REC-EA-KEY-MODULE.

           IF VDUSTATUS NOT = "00"
              GO TO REC-EA-CLASS.

           MOVE VDUNUM0 TO EA-CLASS.
           PERFORM VERIFY-CLASS-CODE.

           IF IO-FG NOT = 0
              GO TO REC-EA-CLASS.

      *-----------------------------------------------------------------
       REC-EA-GROUP-ID.
           MOVE "E  A060GROUP-ID." TO VDU.
           PERFORM SCREENIO.
           IF ( VDUSTATUS = "10" )
              MOVE "CLR" TO IO-TYPE
              GO TO END-REC-EA-KEY-MODULE.
           IF ( VDUSTATUS = "1U" )
              GO TO REC-EA-CLASS.
           IF ( VDUSTATUS = "1<" )
              PERFORM EA-GROUP-SCAN
              IF ( IO-FG = 9 )
                 GO TO REC-EA-GROUP-ID
              ELSE
                 MOVE CDSCAN-CODE TO VDUBUF EA-GROUP-ID
                 GO TO TEST-GROUP.
           IF ( VDUSTATUS NOT = "00" )
              GO TO REC-EA-GROUP-ID.

       TEST-GROUP.
           PERFORM OPEN-CD1-FILE.
           MOVE "EA"   TO CD-TYPE.
           MOVE VDUBUF TO CD-CODE EA-GROUP-ID.
           PERFORM READ-CD1-FILE.
           PERFORM CLOSE-CD1-FILE.
           IF ( IO-FG NOT = 0 )
              GO TO REC-EA-GROUP-ID.

           IF CD-EA-CQ-FG = "Y"
              MOVE "PLEASE EDIT THIS GROUP IN EXTRACTION QUEUE MAINT"
              TO MESS
              PERFORM SEND-MESS
              GO TO REC-EA-GROUP-ID.

           IF BR-GLOBAL-QUEUE = "Y"
              IF CD-EA-GQ-FG NOT = "Y"
                 MOVE "THIS GROUP IS NOT VALID FOR GLOBAL QUEUING!"
                 TO MESS
                 PERFORM SEND-MESS
                 GO TO REC-EA-GROUP-ID.

           IF BR-GLOBAL-QUEUE NOT = "Y"
              IF CD-EA-GQ-FG = "Y"
                 MOVE "THIS GROUP IS NOT VALID FOR LOCAL QUEUING!"
                 TO MESS
                 PERFORM SEND-MESS
                 GO TO REC-EA-GROUP-ID.

           MOVE CD-EA-GQ-FG TO HOLD-EA-GQ-FG.

       PASSWORD-REQUIRED.
           PERFORM VERIFY-PASSWORD.
           IF PASSWDW-STAT NOT = "OK"
              MOVE
           "EDITING THIS EXTRACTION IS NOT ALLOWED WITHOUT PASSWORD!!!"
                                                TO MESS
              PERFORM SEND-MESS
              GO TO REC-EA-GROUP-ID.

      *-----------------------------------------------------------------
       REC-EA-SEQNO.

           MOVE "EN N020SEQNO." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-REC-EA-KEY-MODULE.

           IF VDUSTATUS = "1U"
              GO TO REC-EA-GROUP-ID.

           IF VDUSTATUS = "1<"
              PERFORM ENTER-SCANKEY
              GO TO END-REC-EA-KEY-MODULE.

           IF VDUSTATUS NOT = "00"
              GO TO REC-EA-SEQNO.

           MOVE VDUNUM0 TO EA-SEQNO.

           IF EA-SEQNO = 0
              PERFORM EA-NEXT-AVAIL-SEQNO.

       READ-EAFILE.

           MOVE EA1-KEY TO HOLD-EA1-KEY.
           PERFORM OPEN-EA1-FILE.
           PERFORM READ-EA1-FILE.
           PERFORM CLOSE-EA1-FILE.

           IF IO-FG = 0
              MOVE EA-REC TO ORIGINAL-EA-REC
              PERFORM DISPLAY-FIELDS
            ELSE
              PERFORM NEW-RECORD-SETUP.

       END-REC-EA-KEY-MODULE.
           EXIT.

      ******************************************************************
       EA-NEXT-AVAIL-SEQNO SECTION.
      ******************************************************************

           PERFORM OPEN-EA1-FILE.

       EA-NEXT-AVAIL-CONT.

           ADD 1 TO EA-SEQNO.
           PERFORM READ-EA1-FILE.

           IF IO-FG = 0
              GO TO EA-NEXT-AVAIL-CONT.

           PERFORM CLOSE-EA1-FILE.
           MOVE "SN N020SEQNO." TO VDU.
           MOVE EA-SEQNO TO VDUNUM0.
           PERFORM SCREENIO.

       EA-NEXT-AVAIL-EXIT.
           EXIT.

      ******************************************************************
       ENTER-SCANKEY SECTION.
      ******************************************************************

           PERFORM OPEN-EA1-FILE.
           MOVE 0 TO FIRST-SCAN.

       ENTER-KEY.
           MOVE "E  A140SCANKEY." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCANKEY.

           IF VDUSTATUS = "1<"
              GO TO READ-EAFILE-NEXT.

           IF VDUSTATUS NOT = "00"
              GO TO ENTER-KEY.

           IF VDUBUF = SPACES
              IF FIRST-SCAN NOT = 0
                 MOVE EA1-KEY TO HOLD-EA1-KEY
                 MOVE EA-REC TO ORIGINAL-EA-REC
                 GO TO CLOSE-SCANKEY
              ELSE
                 GO TO ENTER-KEY.

           MOVE VDUBUF    TO WORK-EA1-KEY.

           IF CHANGE-BRNO-FG = "N"
              MOVE BRANCH-NO TO WORK-EA-BRANCH.

           MOVE WORK-EA-BRANCH   TO EA-BRANCH.
           MOVE WORK-EA-CLASS    TO EA-CLASS.
           MOVE WORK-EA-GROUP-ID TO EA-GROUP-ID.
           MOVE WORK-EA-SEQNO    TO EA-SEQNO.
           IF EA-BRANCH NOT NUMERIC
              MOVE 0 TO EA-BRANCH.
           IF EA-CLASS NOT NUMERIC
              MOVE 0 TO EA-CLASS.
           IF EA-SEQNO NOT NUMERIC
              MOVE 0 TO EA-SEQNO.

           PERFORM START-EA1-FILE.

      *-----------------------------------------------------------------
       READ-EAFILE-NEXT.

           IF FIRST-SCAN = 0
              PERFORM START-EA1-FILE
              MOVE 1 TO FIRST-SCAN.

           PERFORM READ-EA1-FILE-NEXT.
           IF IO-FG = 9
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCANKEY.
      *    UNLOCK EA-FILE.

           IF CHANGE-BRNO-FG = "N"
              IF EA-BRANCH NOT = BRANCH-NO     
                 MOVE "CLR" TO IO-TYPE
                 GO TO CLOSE-SCANKEY.

           IF EA-CQ-FG = "Y"
              GO TO READ-EAFILE-NEXT.

           PERFORM DISPLAY-SCAN-KEY.
           PERFORM DISPLAY-FIELDS.
           GO TO ENTER-KEY.

      *-----------------------------------------------------------------
       DISPLAY-SCAN-KEY.

           MOVE "SN N040BRNO." TO VDU.
           MOVE EA-BRANCH TO VDUNUM0.
           PERFORM SCREENIO.
           PERFORM VERIFY-BRNO.

           MOVE "S  N020CLS." TO VDU.
           MOVE EA-CLASS TO VDUNUM0.
           PERFORM SCREENIO.
           PERFORM VERIFY-CLASS-CODE.

           MOVE "S  A060GROUP-ID." TO VDU.
           MOVE EA-GROUP-ID TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  N020SEQNO." TO VDU.
           MOVE EA-SEQNO TO VDUNUM0.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       CLOSE-SCANKEY.
           PERFORM CLOSE-EA1-FILE.
           MOVE 0 TO FIRST-SCAN.

      *-----------------------------------------------------------------
       END-SCANKEY.
           EXIT.

      ******************************************************************
      *
      *    E N T R Y - M O D U L E
      *
      *    NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *               1 - INITIAL NEW RECORD ENTRY
      *               2 - MODIFY NEW RECORD
      *
      ******************************************************************
       ENTRY-MODULE SECTION.
       BEGIN-ENTRY.
           MOVE SPACES TO IO-TYPE.
           MOVE 1 TO ONE.

       CREATE-ROUTINE.
           IF NEW-FLAG NOT = 1
              GO TO MODIFY-ROUTINE.
           ADD ONE TO ELE.
           IF ELE < 1
              MOVE 1 TO ELE ONE.
           IF ELE NOT > MAX
              GO TO ENTER-ELE.
           MOVE 2 TO NEW-FLAG.

       MODIFY-ROUTINE.
           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO MODIFY-ROUTINE.
           MOVE VDUBUF TO ACTION.
           IF ACTION = SPACES
              MOVE "RWR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF ACTION = "DELETE"
              MOVE "DEL" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           INSPECT ACTION REPLACING ALL SPACES
              BY ZEROS.
           IF CHNO NOT NUMERIC
              GO TO MODIFY-ROUTINE.
           IF CHNO = 0
              GO TO MODIFY-ROUTINE.
           IF CHNO > MAX
              GO TO MODIFY-ROUTINE.
           MOVE CHNO TO ELE.

       ENTER-ELE.
           IF CHKEY NOT = ZEROS
              IF CHFG(ELE) = " "
                 IF NEW-FLAG NOT = 1
                    GO TO MODIFY-ROUTINE.
           IF CHFG(ELE) = "N"
              IF NEW-FLAG = 1
                 GO TO CREATE-ROUTINE
            ELSE
              IF CHKEY NOT = CHAN
                 GO TO MODIFY-ROUTINE.

           MOVE SPEC(ELE) TO VDU.
           MOVE ELE TO VDUROW.
           MOVE 0 TO VDUCOL.
           MOVE "." TO VDUDECIMAL.
           PERFORM SCREENIO.

       TEST-F4.
           IF VDUSTATUS = "1U"
              IF NEW-FLAG = 1
                 MOVE -1 TO ONE
                 GO TO CREATE-ROUTINE.

       TEST-F5.
           IF VDUSTATUS = "1D"
              MOVE 1 TO ONE
              GO TO GOTO-FIELD.

       TEST-F1.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-ELE.
           MOVE 1 TO ONE.

       GOTO-FIELD.
           GO TO F-01 F-02 F-03 F-04 F-05 F-06 F-07 F-08 F-09 F-10
                 F-11 F-12 F-13 F-14 F-15 F-16 F-17 F-18 F-19 F-20
                 F-21 F-22 F-23 F-24 F-25 F-26 F-27 F-28 F-29 F-30
                 F-31 F-32 F-33 F-34 F-35 F-36 F-37
                 DEPENDING ON ELE.

       F-01.
           MOVE VDUNUM0 TO EA-PASTDUE-DAY1.
       F-011.
           MOVE "ENNN040011." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO F-011.
           MOVE VDUNUM0 TO EA-PASTDUE-DAY2.
           IF EA-PASTDUE-DAY1 NOT = 0
              IF EA-PASTDUE-DAY2 < EA-PASTDUE-DAY1
                 MOVE " INVALID RANGE" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-02.
           MOVE VDUNUM2 TO EA-PASTDUE-AMT1.
       F-021.
           MOVE "ENNN042021." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO F-021.
           MOVE VDUNUM2 TO EA-PASTDUE-AMT2.
           IF EA-PASTDUE-AMT1 NOT = 0
              IF EA-PASTDUE-AMT2 < EA-PASTDUE-AMT1
                 MOVE " INVALID RANGE" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-03.
           MOVE VDUNUM2 TO EA-ACCT-BAL1.
       F-031.
           MOVE "ENNN062031." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO F-031.
           MOVE VDUNUM2 TO EA-ACCT-BAL2.
           IF EA-ACCT-BAL1 NOT = 0
              IF EA-ACCT-BAL2 < EA-ACCT-BAL1
                 MOVE " INVALID RANGE" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-04.
           MOVE VDU-DATE-YYYYMMDD TO EA-LSTFM-DATE1.
           IF EA-LSTFM-DATE2 NOT = 0
              MOVE EA-LSTFM-DATE1 TO NUM-DATE
              MOVE EA-LSTFM-DATE2 TO SYS-DATE
              PERFORM TIM
              IF ELAPSED-DAYS < 0
                 MOVE " INVALID RANGE" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.
       F-041.
           MOVE "E NM060041." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO F-041.
           MOVE VDU-DATE-YYYYMMDD TO EA-LSTFM-DATE2.
           IF EA-LSTFM-DATE1 NOT = 0
              MOVE EA-LSTFM-DATE1 TO NUM-DATE
              MOVE EA-LSTFM-DATE2 TO SYS-DATE
              PERFORM TIM
              IF ELAPSED-DAYS < 0
                 MOVE " INVALID RANGE" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-05.
           MOVE VDUNUM0 TO EA-LSTFM-NO1.
       F-051.
           MOVE "ENNN040051." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO F-051.
           MOVE VDUNUM0 TO EA-LSTFM-NO2.
           IF EA-LSTFM-NO1 NOT = 0
              IF EA-LSTFM-NO2 < EA-LSTFM-NO1
                 MOVE " INVALID RANGE" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-06.
           MOVE VDUNUM0 TO EA-PASTPROM-DAY1.
       F-061.
           MOVE "ENNS040061." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO F-061.
           MOVE VDUNUM0 TO EA-PASTPROM-DAY2.
           IF EA-PASTPROM-DAY1 NOT = 0
              IF EA-PASTPROM-DAY2 < EA-PASTPROM-DAY1
                 MOVE " INVALID RANGE" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-07.
           MOVE VDUNUM0 TO EA-RECENCY-DAY1.
       F-071.
           MOVE "ENNN040071." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO F-071.
           MOVE VDUNUM0 TO EA-RECENCY-DAY2.
           IF EA-RECENCY-DAY1 NOT = 0
              IF EA-RECENCY-DAY2 < EA-RECENCY-DAY1
                 MOVE " INVALID RANGE" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-08.
           MOVE VDUBUF TO EA-ALPNAME1.
       F-081.
           MOVE "E NA040081." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO F-081.
           MOVE VDUBUF TO EA-ALPNAME2.
           IF EA-ALPNAME2 < EA-ALPNAME1
              MOVE " INVALID RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-09.
           MOVE VDUNUM0 TO EA-CONTRACTUAL-DAY1.
       F-091.
           MOVE "ENNS040091." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO F-091.
           MOVE VDUNUM0 TO EA-CONTRACTUAL-DAY2.
           IF EA-CONTRACTUAL-DAY1 NOT = 0
              IF EA-CONTRACTUAL-DAY1 > EA-CONTRACTUAL-DAY2
                 MOVE " INVALID RANGE" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-10.
           MOVE VDUNUM2 TO EA-DLNO1.
       F-101.
           MOVE "ENNN042101." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO F-101.
           MOVE VDUNUM2 TO EA-DLNO2.
           IF EA-DLNO1 NOT = 0
              IF EA-DLNO1 > EA-DLNO2
                 MOVE " INVALID RANGE" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-11.
           MOVE VDUNUM0 TO EA-LNDATE-DAYS1.
       F-111.
           MOVE "ENNN040111." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO F-111.
           MOVE VDUNUM0 TO EA-LNDATE-DAYS2.
           IF EA-LNDATE-DAYS2 NOT = 0
              IF EA-LNDATE-DAYS1 > EA-LNDATE-DAYS2
                 MOVE " INVALID RANGE" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-12.
           MOVE VDUNUM0 TO EA-MATURE-DAYS1.
       F-121.
           MOVE "ENNN040121." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO F-121.
           MOVE VDUNUM0 TO EA-MATURE-DAYS2.
           IF EA-MATURE-DAYS2 NOT = 0
              IF EA-MATURE-DAYS1 > EA-MATURE-DAYS2
                 MOVE " INVALID RANGE" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-13.
           MOVE VDUNUM0 TO EA-DAY-OF-MO-DUE1.
           IF EA-DAY-OF-MO-DUE1 > 31
                 MOVE "ENTER A DAY OF THE MONTH" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.
       F-131.
           MOVE "ENNN020131." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO F-131.
           MOVE VDUNUM0 TO EA-DAY-OF-MO-DUE2.
           IF EA-DAY-OF-MO-DUE1 > EA-DAY-OF-MO-DUE2
              MOVE " INVALID RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           IF EA-DAY-OF-MO-DUE2 > 31
                 MOVE "ENTER A DAY OF THE MONTH" TO MESS
                 PERFORM SEND-MESS
                 GO TO F-131.
           IF EA-DAY-OF-MO-DUE1 = 20
              IF EA-DAY-OF-MO-DUE2 = 20
                 MOVE "WARNING, THIS RANGE WILL NOT WORK" TO MESS
                 PERFORM SEND-MESS.
           GO TO CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-14.
           MOVE VDUBUF TO EA-ACTION.
           GO TO CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-15.
           MOVE VDUBUF TO EA-ORIGIN.
           GO TO CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-16.
           MOVE VDUBUF TO EA-RECENCYDATE.
           IF VDUBUF NOT = "Y" AND NOT = "N"
              MOVE " ENTER: 'Y'-LOANDATE, 'N'-1STPYDATE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-17.
           MOVE VDUBUF TO EA-CPOTS.
           IF EA-CPOTS NOT = "I"
            IF EA-CPOTS NOT = "S"
             IF EA-CPOTS NOT = "O"
              MOVE " ENTER: 'I'-INCLUDE, 'S'-SKIP, 'O'-ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-18.
           MOVE VDUBUF TO EA-RPOTS.
           IF EA-RPOTS NOT = "I"
            IF EA-RPOTS NOT = "S"
             IF EA-RPOTS NOT = "O"
              MOVE " ENTER: 'I'-INCLUDE, 'S'-SKIP, 'O'-ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-19.
           MOVE VDUBUF TO EA-MISPAYS.
           IF EA-MISPAYS NOT = "I"
            IF EA-MISPAYS NOT = "S"
             IF EA-MISPAYS NOT = "O"
              MOVE " ENTER: 'I'-INCLUDE, 'S'-SKIP, 'O'-ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-20.
           MOVE VDUBUF TO EA-MATURED.
           IF EA-MATURED NOT = "I"
            IF EA-MATURED NOT = "S"
             IF EA-MATURED NOT = "O"
              MOVE " ENTER: 'I'-INCLUDE, 'S'-SKIP, 'O'-ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-21.
           MOVE VDUBUF TO EA-PROMISED.
           IF EA-PROMISED NOT = "I"
            IF EA-PROMISED NOT = "S"
             IF EA-PROMISED NOT = "O"
              MOVE " ENTER: 'I'-INCLUDE, 'S'-SKIP, 'O'-ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-22.
           MOVE VDUBUF TO EA-REPO.
           IF ( EA-REPO NOT = "I" )
            IF ( EA-REPO NOT = "S" )
             IF ( EA-REPO NOT = "O" )
                MOVE " ENTER: 'I'-INCLUDE, 'S'-SKIP, 'O'-ONLY" TO MESS
                PERFORM SEND-MESS
                GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
      *----------------------------------------------------------------
       F-23.
           MOVE VDUBUF TO EA-LEGAL.
           IF ( EA-LEGAL NOT = "I" )
            IF ( EA-LEGAL NOT = "S" )
             IF ( EA-LEGAL NOT = "O" )
                MOVE " ENTER: 'I'-INCLUDE, 'S'-SKIP, 'O'-ONLY" TO MESS
                PERFORM SEND-MESS
                GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
      *---------------------------------------------------------------
       F-24.
           MOVE VDUBUF TO EA-DEBTAID.
           IF ( EA-DEBTAID NOT = "I" )
            IF ( EA-DEBTAID NOT = "S" )
             IF ( EA-DEBTAID NOT = "O" )
                MOVE " ENTER: 'I'-INCLUDE, 'S'-SKIP, 'O'-ONLY" TO MESS
                PERFORM SEND-MESS
                GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
      *---------------------------------------------------------------
       F-25.
           MOVE VDUBUF TO EA-BANKRUPT.
           IF ( EA-BANKRUPT NOT = "I" )
            IF ( EA-BANKRUPT NOT = "S" )
             IF ( EA-BANKRUPT NOT = "O" )
                MOVE " ENTER: 'I'-INCLUDE, 'S'-SKIP, 'O'-ONLY" TO MESS
                PERFORM SEND-MESS
                GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-26.
           MOVE VDUBUF TO EA-PL.
           IF ( EA-PL NOT = "I" )
            IF ( EA-PL NOT = "S" )
             IF ( EA-PL NOT = "O" )
              MOVE " ENTER: 'I'-INCLUDE, 'S'-SKIP, 'O'-ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
      *-----------------------------------------------------------------
       F-261.
           IF ( EA-PL = "S" )
              MOVE "S  A220PROMPT261." TO VDU
              MOVE SPACES              TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A010261." TO VDU
              MOVE SPACES        TO VDUBUF EA-PL-IN-ACTIVE-BOTH
              PERFORM SCREENIO
              GO TO CREATE-ROUTINE.

           MOVE PROMPT-261-LIST TO WR-LIST.
           MOVE PROMPT-261 TO WR-BUF.
           PERFORM CALL-WRFORM.

           MOVE "E  A010261." TO VDU.
           PERFORM SCREENIO.
           IF ( VDUSTATUS = "1U" )
              GO TO ENTER-ELE.
           IF ( VDUSTATUS NOT = "00" AND NOT = "1D" )
              GO TO F-261.
           IF NOT ( VDUBUF = "A" OR "B" OR "I" )
              MOVE " ENTER: 'I'-INACTIVE, 'A'-ACTIVE, 'B'-BOTH" TO MESS
              PERFORM SEND-MESS
              GO TO F-261.
           MOVE VDUBUF TO EA-PL-IN-ACTIVE-BOTH.
           GO TO CREATE-ROUTINE.
      *--------------------------------------------------------------
       F-27.
           MOVE VDUBUF TO EA-JUDGEMENT.
           IF ( EA-JUDGEMENT NOT = "I" )
             IF ( EA-JUDGEMENT NOT = "S" )
             IF ( EA-JUDGEMENT NOT = "O" )
              MOVE " ENTER: 'I'-INCLUDE, 'S'-SKIP, 'O'-ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-28.
           MOVE VDUBUF TO EA-CONFIDENTIAL-FG.
           IF ( EA-CONFIDENTIAL-FG NOT = "I" )
             IF ( EA-CONFIDENTIAL-FG NOT = "S" )
             IF ( EA-CONFIDENTIAL-FG NOT = "O" )
              MOVE " ENTER: 'I'-INCLUDE, 'S'-SKIP, 'O'-ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-29.
           MOVE VDUBUF TO EA-1ST-PMT-MISSES.
           IF ( EA-1ST-PMT-MISSES NOT = "I" )
            IF ( EA-1ST-PMT-MISSES NOT = "S" )
             IF ( EA-1ST-PMT-MISSES NOT = "O" )
              MOVE " ENTER: 'I'-INCLUDE, 'S'-SKIP, 'O'-ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-30.
           MOVE VDUBUF TO EA-ALLOTMENTS.
           IF EA-ALLOTMENTS NOT = "I"
            IF EA-ALLOTMENTS NOT = "S"
             IF EA-ALLOTMENTS NOT = "O"
              MOVE " ENTER: 'I'-INCLUDE, 'S'-SKIP, 'O'-ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-31.
           MOVE VDUBUF TO EA-BORR-STATE.
           GO TO CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-32.
           MOVE VDUNUM0 TO EA-SETFM-NO FD-FMNO.
           IF EA-SETFM-NO NOT = 0
              PERFORM GET-FD-RECORD
              IF IO-FG NOT = 0
                 GO TO ENTER-ELE.
      *-----------------------------------------------------------------
       F-321.
           MOVE "ENNN030321." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO F-321.
           MOVE VDUNUM0 TO EA-SETFM(1) FD-FMNO.
           IF EA-SETFM(1) NOT = 0
              PERFORM GET-FD-RECORD
              IF IO-FG NOT = 0
                 GO TO F-321.
       F-322.
           MOVE "ENNN030322." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO F-321.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO F-322.
           MOVE VDUNUM0 TO EA-SETFM(2) FD-FMNO.
           IF EA-SETFM(2) NOT = 0
              PERFORM GET-FD-RECORD
              IF IO-FG NOT = 0
                 GO TO F-322.
       F-323.
           MOVE "ENNN030323." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO F-322.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO F-323.
           MOVE VDUNUM0 TO EA-SETFM(3) FD-FMNO.
           IF EA-SETFM(3) NOT = 0
              PERFORM GET-FD-RECORD
              IF IO-FG NOT = 0
                 GO TO F-323.
           GO TO CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-33.
           MOVE VDUBUF TO EA-SETACT-CD.
           GO TO CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-34.
           MOVE VDUBUF TO EA-INC-CNT.
           IF EA-INC-CNT NOT = "Y" AND NOT = "N"
             MOVE " ENTER 'Y' OR 'N' ONLY" TO MESS
             PERFORM SEND-MESS
             GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-35.
           MOVE VDUNUM0 TO EA-INC-PROMISED.
           GO TO CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-36.
           MOVE VDUBUF TO EA-SET-LN-COL.
           IF EA-SET-LN-COL NOT = "Y" AND NOT = "N" AND NOT = "P"
             MOVE " ENTER 'Y' OR 'N' OR 'P' (POOL)" TO MESS
             PERFORM SEND-MESS
             GO TO ENTER-ELE.
           IF EA-SET-LN-COL = "N"
              MOVE "===" TO EA-LN-COL-ID
              MOVE "S  A000PROMPT361." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A000361." TO VDU
              MOVE SPACES TO VDUBUF EA-COL-POOL
              PERFORM SCREENIO
              GO TO CREATE-ROUTINE.
       F-361.
           IF EA-SET-LN-COL = "P"
             GO TO F-361-POOL.

           MOVE PROMPT-361-LIST TO WR-LIST.
           MOVE PROMPT-361 TO WR-BUF.
           PERFORM CALL-WRFORM.

           MOVE "E  A030361." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS = "1<"
              PERFORM COLLECTOR-SCAN
              IF IO-FG NOT = 0
                 GO TO F-361.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO F-361.
           MOVE VDUBUF TO EA-LN-COL-ID.
           IF EA-LN-COL-ID = "   "
              MOVE "WARNING: LOAN COLLECTOR WILL BE RESET TO SPACES!"
              TO MESS
              PERFORM SEND-MESS
              GO TO CREATE-ROUTINE.
           PERFORM VERIFY-COLLECTOR-CODE.
           IF ( CODE-NOT-FOUND )
              GO TO F-361.
           GO TO CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-361-POOL.
           MOVE PROMPT-361-LIST TO WR-LIST.
           MOVE PROMPT-361-P TO WR-BUF.
           PERFORM CALL-WRFORM.

           MOVE "E  A030361." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS = "1<"
              PERFORM POOL-SCAN
              IF IO-FG NOT = 0
                 GO TO F-361-POOL.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO F-361-POOL.
           MOVE VDUBUF TO EA-COL-POOL.
           PERFORM VERIFY-COLLECTOR-POOL.
           IF ( CODE-NOT-FOUND )
              GO TO F-361-POOL.
           GO TO CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-37.
           MOVE VDUBUF TO EA-DESC.
           GO TO CREATE-ROUTINE.

       END-ENTRY-MODULE.
           EXIT.

      ******************************************************************
       WRITE-MODULE SECTION.
      ******************************************************************
           PERFORM OPEN-EA1-FILE.
           IF NEW-FLAG NOT = 0
              GO TO WRITE-RECORD.

           MOVE EA-REC TO CHANGED-EA-REC.
           IF ORIGINAL-EA-REC = CHANGED-EA-REC
              GO TO END-WRITE-MODULE.
           PERFORM READ-EA1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-WRITE-MODULE.
           IF EA-REC NOT = ORIGINAL-EA-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-WRITE-MODULE.
           MOVE CHANGED-EA-REC TO EA-REC.

           PERFORM REWRITE-EA1-FILE.
           GO TO END-WRITE-MODULE.
       WRITE-RECORD.
           PERFORM WRITE-EA1-FILE.
           IF IO-FG NOT = 0
              PERFORM ALREADY-CREATED.

       END-WRITE-MODULE.
           PERFORM CLOSE-EA1-FILE.

      ************************************
       DELETE-MODULE SECTION.
      ************************************
           IF NEW-FLAG NOT = 0
              GO TO END-DELETE-MODULE.

           PERFORM OPEN-EA1-FILE.
           PERFORM READ-EA1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-DELETE-MODULE.
           IF EA-REC NOT = ORIGINAL-EA-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-CHANGED
              GO TO END-DELETE-MODULE.

           PERFORM DELETE-EA1-FILE.
       END-DELETE-MODULE.
           PERFORM CLOSE-EA1-FILE.

      ************************************
       NEW-RECORD-SETUP SECTION.
      ************************************
           MOVE 1 TO NEW-FLAG.
           MOVE "S  A000NEW." TO VDU.
           MOVE "*** NEW RECORD ***" TO VDUBUF.
           PERFORM SCREENIO.

           PERFORM CLEAR-EA-FILE.
           MOVE "N" TO EA-CQ-FG.
           MOVE HOLD-EA1-KEY TO EA1-KEY.
           MOVE 0 TO ELE.

      ************************************
       DISPLAY-FIELDS SECTION.
      ************************************

           MOVE EA-PASTDUE-DAY1     TO F01.
           MOVE EA-PASTDUE-DAY2     TO F011.
           MOVE EA-PASTDUE-AMT1     TO F02.
           MOVE EA-PASTDUE-AMT2     TO F021.
           MOVE EA-ACCT-BAL1        TO F03.
           MOVE EA-ACCT-BAL2        TO F031.
           MOVE EA-LSTFM-DATE1      TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO F04.
           MOVE EA-LSTFM-DATE2      TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
           MOVE DATE-MMDDYY         TO F041.
           MOVE EA-LSTFM-NO1        TO F05.
           MOVE EA-LSTFM-NO2        TO F051.
           MOVE EA-PASTPROM-DAY1    TO F06.
           MOVE EA-PASTPROM-DAY2    TO F061.
           MOVE EA-RECENCY-DAY1     TO F07.
           MOVE EA-RECENCY-DAY2     TO F071.
      D    STOP EA-ALPNAME1.
           MOVE EA-ALPNAME1         TO F08.
           MOVE EA-ALPNAME2         TO F081.
           MOVE EA-CONTRACTUAL-DAY1 TO F09.
           MOVE EA-CONTRACTUAL-DAY2 TO F091.
           MOVE EA-DLNO1            TO F10.
           MOVE EA-DLNO2            TO F101.
           MOVE EA-LNDATE-DAYS1     TO F11.
           MOVE EA-LNDATE-DAYS2     TO F111.
           MOVE EA-MATURE-DAYS1     TO F12.
           MOVE EA-MATURE-DAYS2     TO F121.
           MOVE EA-DAY-OF-MO-DUE1   TO F13.
           MOVE EA-DAY-OF-MO-DUE2   TO F131.
           MOVE EA-ACTION           TO F14.
           MOVE EA-ORIGIN           TO F15.
           MOVE EA-RECENCYDATE      TO F16.
           MOVE EA-CPOTS            TO F17.
           MOVE EA-RPOTS            TO F18.
           MOVE EA-MISPAYS          TO F19.
           MOVE EA-MATURED          TO F20.
           MOVE EA-PROMISED         TO F21.
           MOVE EA-REPO             TO F22.
           MOVE EA-LEGAL            TO F23.
           MOVE EA-DEBTAID          TO F24.
           MOVE EA-BANKRUPT         TO F25.
           MOVE EA-PL               TO F26.

           IF ( EA-PL = "S" )
              MOVE SPACES               TO F261 FPROMPT261
           ELSE
              MOVE EA-PL-IN-ACTIVE-BOTH TO F261
              MOVE PROMPT-261           TO FPROMPT261.

           MOVE EA-JUDGEMENT        TO F27.
           MOVE EA-CONFIDENTIAL-FG  TO F28.
           MOVE EA-1ST-PMT-MISSES   TO F29.
           MOVE EA-ALLOTMENTS       TO F30.
           MOVE EA-BORR-STATE       TO F31.

           MOVE EA-SETFM-NO     TO F32.
           MOVE EA-SETFM(1)     TO F321.
           MOVE EA-SETFM(2)     TO F322.
           MOVE EA-SETFM(3)     TO F323.
           MOVE EA-SETACT-CD    TO F33.
           MOVE EA-INC-CNT      TO F34.
           MOVE EA-INC-PROMISED TO F35.
           MOVE EA-SET-LN-COL   TO F36.

           IF ( EA-SET-LN-COL = "Y" )
              MOVE EA-LN-COL-ID TO F361
              MOVE PROMPT-361   TO FPROMPT361
           ELSE
           IF ( EA-SET-LN-COL = "P" )
              MOVE EA-COL-POOL  TO F361
              MOVE PROMPT-361-P TO FPROMPT361
           ELSE
              MOVE SPACES       TO F361 FPROMPT361.

           MOVE EA-DESC TO F37.

           MOVE DISPLAY-LIST TO WR-LIST.
           MOVE DISPLAY-BUF TO WR-BUF.
           PERFORM CALL-WRFORM.

      ******************************************************************
       EA-GROUP-SCAN SECTION.
      ******************************************************************

           MOVE "EA"   TO CDSCAN-TYPE.
           MOVE SPACES TO CDSCAN-CODE.
           MOVE "N"    TO CDSCAN-EA-CQ-FG.
           IF BR-GLOBAL-QUEUE = "Y"
              MOVE "Y" TO CDSCAN-EA-GQ-FG
           ELSE
              MOVE "N" TO CDSCAN-EA-GQ-FG.

           MOVE "WI/CDSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CDSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF CDSCAN-STATUS NOT = "00" OR CDSCAN-CODE = SPACES
               MOVE 9 TO IO-FG
               GO TO EXIT-GROUP-SCAN.

           MOVE 0 TO IO-FG.
           MOVE "S  A060GROUP-ID." TO VDU.
           MOVE CDSCAN-CODE        TO VDUBUF.
           PERFORM SCREENIO.

       EXIT-GROUP-SCAN.
           EXIT.

      ******************************************************************
       COLLECTOR-SCAN SECTION.
      ******************************************************************

           MOVE "CN"      TO CDBSCN-TYPE.
           MOVE SPACES    TO CDBSCN-CODE.
           IF BR-GLOBAL-QUEUE = "Y"
              MOVE 0         TO CDBSCN-BRANCH
           ELSE
              MOVE EA-BRANCH TO CDBSCN-BRANCH.

           MOVE "WI/CDBSCN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CDBSCN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF ( CDBSCN-STATUS NOT = "00") OR (CDBSCN-CODE = SPACES) 
              MOVE 9 TO IO-FG
              GO TO EXIT-COLLECTOR-SCAN.

           MOVE 0 TO IO-FG.
           MOVE "S  A030361." TO VDU.
           MOVE CDBSCN-CODE TO VDUBUF.
           PERFORM SCREENIO.
       EXIT-COLLECTOR-SCAN.
           EXIT.

      ******************************************************************
       POOL-SCAN SECTION.
      ******************************************************************
           IF HOLD-EA-GQ-FG = "Y"
              MOVE "G" TO CPSCAN-TYPE
           ELSE
              MOVE " " TO CPSCAN-TYPE.
           MOVE "WI/CPSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CPSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF CPSCAN-STATUS NOT = "00" 
              MOVE 9 TO IO-FG
              GO TO EXIT-POOL-SCAN.

           MOVE 0 TO IO-FG.
           MOVE "S  A030361." TO VDU.
           MOVE CPSCAN-KEY TO VDUBUF.
           PERFORM SCREENIO.
       EXIT-POOL-SCAN.
           EXIT.

      *********************************************
       VERIFY-GROUP SECTION.
      *********************************************
           PERFORM OPEN-CD1-FILE.
           MOVE "EA"        TO CD-TYPE.
           MOVE EA-GROUP-ID TO CD-CODE.
           PERFORM READ-CD1-FILE.
           PERFORM CLOSE-CD1-FILE.
           IF ( IO-FG NOT = 0 )
              GO TO EXIT-VERIFY-GROUP.

           IF CD-EA-CQ-FG = "Y"
              MOVE "PLEASE EDIT THIS GROUP IN COLLECTOR QUEUE SET-UP"
              TO MESS
              PERFORM SEND-MESS
              MOVE 9 TO IO-FG
              GO TO EXIT-VERIFY-GROUP.

           IF BR-GLOBAL-QUEUE = "Y"
              IF CD-EA-GQ-FG NOT = "Y"
                 MOVE "THIS GROUP IS NOT VALID FOR GLOBAL QUEUING!"
                 TO MESS
                 PERFORM SEND-MESS
                 MOVE 9 TO IO-FG
                 GO TO EXIT-VERIFY-GROUP.

           IF BR-GLOBAL-QUEUE NOT = "Y"
              IF CD-EA-GQ-FG = "Y"
                 MOVE "THIS GROUP IS NOT VALID FOR LOCAL QUEUING!"
                 TO MESS
                 PERFORM SEND-MESS
                 MOVE 9 TO IO-FG
                 GO TO EXIT-VERIFY-GROUP.

           MOVE CD-EA-GQ-FG TO HOLD-EA-GQ-FG.

       EXIT-VERIFY-GROUP.
           EXIT.

      ******************************************************************
       VERIFY-COLLECTOR-CODE SECTION.
      ******************************************************************
           MOVE "Y" TO CODE-FOUND.

           MOVE "CN"      TO CDB-TYPE.
           IF BR-GLOBAL-QUEUE = "Y"
              MOVE 0 TO CDB-BRANCH
           ELSE
              MOVE EA-BRANCH TO CDB-BRANCH.
           MOVE EA-LN-COL-ID TO CDB-CODE.
           PERFORM OPEN-CDB1-FILE.
           PERFORM READ-CDB1-FILE.
           PERFORM CLOSE-CDB1-FILE.
           IF ( IO-FG NOT = 0 )
              MOVE "N" TO CODE-FOUND.

      ******************************************************************
       VERIFY-COLLECTOR-POOL SECTION.
      ******************************************************************
           MOVE "Y" TO CODE-FOUND.
           MOVE EA-COL-POOL TO CP-ID.
           IF HOLD-EA-GQ-FG = "Y"
              MOVE EXT-FILPATH-BASE    TO CP-GLOBAL-BASE
              MOVE EXT-FILPATH-MACHINE TO CP-GLOBAL-MACHINE
              MOVE "G"                 TO CP-PATH-OVERRIDE   
              MOVE CP-GLOBAL-PATH      TO CP-PATH
           ELSE
              MOVE " "                 TO CP-PATH-OVERRIDE. 
           PERFORM OPEN-CP1-FILE.
           PERFORM READ-CP1-FILE.
           PERFORM CLOSE-CP1-FILE.
           IF IO-FG NOT = 0
              MOVE "N" TO CODE-FOUND.

      ***************************************************
       VERIFY-PASSWORD SECTION.
      ***************************************************
           MOVE " " TO PASSWDW-STAT.
           MOVE EA-GROUP-ID TO PASSWDW-PGM.
           MOVE 0 TO PASSWDW-SEQ.
           MOVE "PLEASE ENTER THE PASSWORD," TO PASSWDW-PROMPT1.
           MOVE "TO EDIT THIS EXTRACTION:" TO PASSWDW-PROMPT2.
           MOVE "CL/PASSWD" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
           CANCEL FORM-PROGX.


      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBLP/LPEACL.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".

      **************************************************
       FORM-RESET SECTION.
      **************************************************
           MOVE EXT-CONAME-CONAME TO EAMAIN-CONAME.
           MOVE EXT-CONAME-SYSDATE TO EAMAIN-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY EAMAIN-SCREEN UPON EAMAIN-WINDOW-HANDLE.
           MOVE EAMAIN-FORM-FIELDS TO WR-BUF.
           MOVE EAMAIN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *****************************************
       VDU-CONVERT-FIELD SECTION.
      *****************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/EAMAIN_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ****************************************
       SETUP-LINK-INFO SECTION.
      ****************************************
           MOVE EAMAIN-HELP--FILE TO HELP-LINK-FILE.
           MOVE EAMAIN-HELP--DIR TO HELP-LINK-DIR.

      ******************************************************************
       MISC-ROUTINES SECTION.
      ******************************************************************
      *-----------------------------------------------------------------
       COPY "LIBGB/ACCESS.CPY".
      *-----------------------------------------------------------------
       RECORD-DELETED.
           MOVE " RECORD WAS DELETED!" TO MESS.
           PERFORM SEND-MESS.
       RECORD-CHANGED.
           MOVE " CHANGES WERE MADE TO THIS RECORD!" TO MESS.
           PERFORM SEND-MESS.
       CHANGE-AGAIN.
           MOVE " PLEASE RE-INPUT YOUR CHANGES!" TO MESS.
           PERFORM SEND-MESS.
       ALREADY-CREATED.
           MOVE " RECORD HAS BEEN ALREADY CREATED!" TO MESS.
           PERFORM SEND-MESS.
       UNDERGONE-CHANGE.
           MOVE
           " THIS RECORD HAS UNDERGONE CHANGE BY ANOTHER USER,"
           TO MESS.
           PERFORM SEND-MESS.
       VERIFY-BRNO.
           MOVE EA-BRANCH TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF IO-FG NOT = 0
              MOVE "* NOT ON FILE *" TO BR-NAME.
           MOVE "S  A200BRDESC." TO VDU.
           MOVE BR-NAME TO VDUBUF.
           PERFORM SCREENIO.
       VERIFY-CLASS-CODE.
           MOVE EA-CLASS TO LC-CODE.
           PERFORM OPEN-LC1-FILE.
           PERFORM READ-LC1-FILE.
           PERFORM CLOSE-LC1-FILE.
           IF IO-FG NOT = 0
              MOVE "* NOT ON FILE *" TO LC-DESC.
           MOVE "S  A020CLSDESC." TO VDU.
           MOVE LC-DESC TO VDUBUF.
           PERFORM SCREENIO.
       GET-FD-RECORD.
           PERFORM OPEN-FD1-FILE.
           PERFORM READ-FD1-FILE.
           PERFORM CLOSE-FD1-FILE.
           IF IO-FG NOT = 0
              MOVE " INVALID FORM NUMBER" TO MESS
              PERFORM SEND-MESS.

      ***************************
       IO-ROUTINES SECTION.
      ***************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-CP1-FILE.
           PERFORM CLOSE-LC1-FILE.
           PERFORM CLOSE-FD1-FILE.
           PERFORM CLOSE-EA1-FILE.
           PERFORM CLOSE-CDB1-FILE.
           PERFORM CLOSE-CD1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBSP/SPCPGS_SQL.CPY".
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBLP/LPFDGS_SQL.CPY".
       COPY "LIBLP/LPEAGS_SQL.CPY".
       COPY "LIBLP/LPCDBGS_SQL.CPY".
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPLC1RN.CPY".
       COPY "LIBLP/LPCD1RN.CPY".
       COPY "LIBLP/LPCDB1RN.CPY".
       COPY "LIBLP/LPEA1IN.CPY".
       COPY "LIBLP/LPFD1RN.CPY".
       COPY "LIBSP/SPCP1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      ******************************************
       END-ROUTINE SECTION.
      ******************************************
       END-RUN.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY EAMAIN-SCREEN.
           DESTROY EAMAIN-WINDOW-HANDLE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           EXIT PROGRAM.
