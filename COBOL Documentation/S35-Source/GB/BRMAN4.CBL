       IDENTIFICATION DIVISION.
       PROGRAM-ID.      BRMAN4.
      ******************************************************************
      *
      *  DIR :   GB
      *  DESC:   BRANCH FILE MAINTENANCE PAGE 4
      *
      * REV :
      *  JTG 062995 ADDED BR-AC-BEFORE-REPO, BR-AC-BEFORE-P4P5
      *  KEC 020896 ADDED BR-COLL-QUEUE-CON-REC.
      *  MJD 021996 ADDED "B" (BOTH) OPTION TO BR-SFDLO3-WINDOW-FG.
      *              RELIANCE WANTS THE WINDOW TO OPEN WHETHER THE INSURANCE
      *              IS OURS OR THEIRS.
      *  JTG 022096 ADDED BR-LNCHGS-WINDOW OPTION
      *  JTG 062596 ADDED EXPANDED APPLICATION INPUTE OPTION BR-EXPAND-APP-INPUT
      *  MJD 100396 ADDED BR-INBR-BILNSC-FG AND BR-INBR-BRIEF-FG FOR
      *             INTER BRANCH PROCESSING.
      *  MJD 012897 ADDED BR-BYINQ-START-PAGE AND BR-DSINQ-START-PAGE.
      *  JTG 022097 ADDED BR-APP-REQUIRED-FOR-INPUT  RE: STATISTICS UPDATE
      *  BLV 022597 ADDED BR-CREDIT-SCORING-FG
      *  BLV 041497 ADDED BR-COL-QUEUE-LOW-TO-HI
      *  MJD 072397 ADDED BR-AUTO-LNSTAT-WINDOW.
      *  BLV 072497 ADDED BR-TIME-ZONE-FACTOR
      *  JTG 081397 ADDED BR-DUEDATE-CHANGE-CD RELIANCE, #63
      *  BLV 103197 ADDED BR-GL-TYPE-ALLOWED, BR-GL-ENTRY-DEFAULT
      *  BLV 110497 ADDED BR-MIDSTREAM-1STPAY-FG
      *  JTG 111197 ADDED BR-DUEDATE-CHANGE-CD OPTION 'M' MERCURY
      *  JTG 111197 ADDED BR-NODEF-1ST-X-MONTHS OPTION MERCURY
      *  MJD 112197 EXPANDED BR-REC TO 850 BYTES
      *  JTG 120397 ADDED BR-NODEF-IF-ZERO-AMT OPTION MERCURY
      *  KEC 121797 ADDED PAGE 7.
      *  MJD 031398 ADDED 78 LEVEL: BR-SIZE
      *  KEC 111798 FIXED NEW RECORD ENTRY ROUTINE TO GO TO PAGE 5.
      *  BAH 990413 ADDED CALL TO CREATE CHANGES FILE AFTER REWRITE
      *  MJD 990901 CHANGED TO ALLOW BR-INBR-BILNSC-FG = "M" (MUL PR#925)
      *  MG  991005 FIXED VDUSTATUS/IO-FG/STAT PER DEVELOPMENT MEETING.
      *  BAH 000531 ADDED NEW VALUE FOR BR-NO-CASH-PROCEEDS "B" TO ALLOW
      *             CASH PROCEEDS ON PB ACCTS ONLY, SUNBELT PR# 1213
      *  BAH 030918 REMOVED F19. BR-LNCHGS-WINDOW, NOT USED ANYWHERE
      *  BAH 030918 REMOVED F1. BR-FAIRCO-TRW-OPT
      *  BLV 040218 ADDED F19, BR-JE-ALLOCATION-SKIP, FLAG TO SKIP
      *             BRANCH WHEN POSTING GROUP ALLOCATION IN GL
      *             JOURNAL ENTRIES, WORLD
      *  BLV 040324 CHANGE F19 BACK TO UNUSED FIELD, WILL PUT
      *             BR-JE-ALLOCATION-SKIP & BR-WORLD-BRANCH-TYPE
      *             ON PAGE 7
      *  BAH 070131 ADDED BR-MIN-MONTHS-DEF FOR REGACC PR# 162
      *  AMM 070307 CREATED A LOG FIELD CHANGE ROUTINE
      *  BLF 070906 ALLOW BR-COLL-QUEUE-CON-REC TO ACCEPT A VALUE OF "N",
      *             INDICATING NO DELINQUENCY IN QUEUE ORDER, QUEUE WILL
      *             JUST BE ORDERED BY BALANCE, PCCREDIT PR# 3042
      *   CS 080407 ADD BR-MIN-MONTHS-D2, PDTH MONTHS THAT MUST ELAPSE
      *             BEFORE POSTING A DF/D2 TO ACCOUNT WITH A PRIOR
      *             MULTIPLE DEFERMENT POSTING PR#202
      *  BAH 120514 REMOVED SPECIAL-FG
      *  BLM 130405 ADDED FIELD 19, BR-ACH-BANK-NO, CITIZENS PR# 4244
      *  KEC 140206 {PR#00967} WORLD <A15> [JIM ROSENAUER]
      *             ADDED A 'W' OPTION FOR BR-APP-REQUIRED-FOR-INPUT
      *             (F-20) ONLY FOR WORLD.
      *  BAH 151021 REMOVED B2FILE, PD0003
      * KEC 2016.0323 CHANGED CDSCAN-WINDOW-BUF TO HAVE MATCHING CD1-KEY
      *               LAYOUTS (FOR EACH CODE TYPE; CD-TYPE).
      *               CDSCAN-CD1-KEY IS THE NEW (PRIMARY) WITH REDEFINES OF
      *               CDSCAN-CD-??-KEY (NEW).
      *  BAH 20211116 LIMIT ACCESS BY PASSWORD TO ALL FIELDS, #1524
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

      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E - S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)GB/BRMAN4.CBL S35 09/20/22-12:46:35 >".

      ******************************************************************
      *
      *     P R O G R A M - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LP01CD_SQL.CPY".
       COPY "LIBLP/LPWSCD.CPY".

      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    ORIGINAL LOAN RECORD
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       COPY "LIBGB/GB01BR.CPY" REPLACING
            LEADING "BR" BY "BROR".
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CHANGED LOAN RECORD
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       COPY "LIBGB/GB01BR.CPY" REPLACING
            LEADING "BR" BY "BRCH".

      ******************************************************************
      *
      *    L M C - W O R K E R S
      *
      *=================================================================
      *    DESC: LOG-MAINTENANCE-CHGS WORKERS
      ******************************************************************
       01  LMC-DETAIL                  PIC X(55)      VALUE SPACES.
       01  FILLER                      REDEFINES LMC-DETAIL.
           03  LMC-BRNO                PIC 9(04).
           03  FILLER                  PIC X(01).
           03  LMC-FIELD               PIC X(03).
           03  FILLER                  PIC X(01).
           03  LMC-DESC                PIC X(09).
           03  FILLER                  PIC X(01).

           03  LMC-ORG-TEXT            PIC X(16).
           03  LMC-ORG-AMT             REDEFINES LMC-ORG-TEXT
                                       PIC ZZZ,ZZZ,ZZ9.99-.
           03  LMC-ORG-DATE            REDEFINES LMC-ORG-TEXT
                                       PIC 99/99/99.
           03  LMC-ORG-DLNO            REDEFINES LMC-ORG-TEXT
                                       PIC ZZZ9.99.
           03  LMC-ORG-GLNO            REDEFINES LMC-ORG-TEXT
                                       PIC ZZZZ/ZZZZZZ9.
           03  LMC-ORG-NUM             REDEFINES LMC-ORG-TEXT
                                       PIC ZZZZZZZZZ9-.
           03  LMC-ORG-PHONE           REDEFINES LMC-ORG-TEXT
                                       PIC 999/999/9999.
           03  LMC-ORG-SSNO            REDEFINES LMC-ORG-TEXT
                                       PIC 999/99/9999.

           03  LMC-TO-X                PIC X(04).

           03  LMC-CHG-TEXT            PIC X(16).
           03  LMC-CHG-AMT             REDEFINES LMC-CHG-TEXT
                                       PIC ZZZ,ZZZ,ZZ9.99-.
           03  LMC-CHG-DATE            REDEFINES LMC-CHG-TEXT
                                       PIC 99/99/99.
           03  LMC-CHG-DLNO            REDEFINES LMC-CHG-TEXT
                                       PIC ZZZ9.99.
           03  LMC-CHG-GLNO            REDEFINES LMC-CHG-TEXT
                                       PIC ZZZZ/ZZZZZZ9.
           03  LMC-CHG-NUM             REDEFINES LMC-CHG-TEXT
                                       PIC ZZZZZZZZZ9-.
           03  LMC-CHG-PHONE           REDEFINES LMC-CHG-TEXT
                                       PIC 999/999/9999.
           03  LMC-CHG-SSNO            REDEFINES LMC-CHG-TEXT
                                       PIC 999/99/9999.

      *-----------------------------------------------------------------

       01  ONE             PIC S9.
       01  ELE             PIC 9(2).
       01  ACTION.
           03  CHKEY       PIC X(4).
           03  CHNO        PIC 9(2).
       01  FORM-NUM REDEFINES ACTION.
           03  FORM-KEY    PIC X(4).
           03  FORM-P      PIC X.
           03  FORM-SEL    PIC 9.

       01  IO-TYPE         PIC X(3).
       01  NEW-FLAG        PIC 9.

       01  ORIGINAL-BR-REC    PIC X(BR-SIZE).
       01  CHANGED-BR-REC     PIC X(BR-SIZE).

       01  UPDATE-BR-STAT            PIC X(02) VALUE SPACES.
           88  UPDATE-BR-STAT-GOOD             VALUE "00".
           88  UPDATE-BR-STAT-BAD              VALUE "  " THRU "//"
                                                     "01" THRU "~~".


       01  BRMAIN-LINK       VALUE SPACES.
           03  FORM-NAME     PIC X(20).
           03  SAVEKEY       PIC X(4).
           03  NEW-REC       PIC X.
           03  ERRCD         PIC X.
           03  FORMNUM       PIC 9.
           03  BRMAIN-WINDOW-HANDLE PIC X(10).

      ******************************************************************
      *
      *    S C R E E N - D I S P L A Y - B U F F E R
      *
      ******************************************************************
       01  DISPLAY-BUF          VALUE SPACES.
           03  F01              PIC 99.
           03  F02              PIC X.
           03  F03              PIC XXX.
           03  F04              PIC X.
           03  F05              PIC XXX.
           03  F06              PIC X.
           03  F07              PIC X.
           03  F08              PIC X.
           03  F09              PIC X.
           03  F10              PIC X.
           03  F11              PIC X.
           03  F12              PIC X.
           03  F13              PIC X.
           03  F14              PIC X.
           03  F15              PIC X.
           03  F16              PIC X.
           03  F17              PIC X.
           03  F18              PIC X.
           03  F19              PIC 9.
           03  F20              PIC X.
           03  F21              PIC X.
           03  F22              PIC X.
           03  F23              PIC X.
           03  F24              PIC Z9.
           03  F25              PIC Z9.
           03  F26              PIC X.
           03  F27              PIC X.
           03  F28              PIC X.
           03  F29              PIC 9-.
           03  F30              PIC X.
           03  F31              PIC X.
           03  F32              PIC X.
           03  F33              PIC X.
           03  F34              PIC Z.
           03  F35              PIC X.
           03  F36              PIC 99.

      *-----------------------------------------------------------------
       01  DISPLAY-LIST.
           03  FILLER      PIC X(48) VALUE
           "010 020 030 040 050 060 070 080 090 100 ".
           03  FILLER      PIC X(52) VALUE
           "110 120 130 140 150 160 170 180 190 200 210 220 230 ".
           03  FILLER      PIC X(52) VALUE
           "240 250 260 270 280 290 300 310 320 330 340 350 360.".

      ******************************************************************
      *
      *    E L E M E N T - S P E C I F I C A T I O N S - # 1
      *
      ******************************************************************
       01  MAX            PIC 99   VALUE 36.
      *-----------------------------------------------------------------
       01  SPEC-TABLE.
      *- - - - - - - -  1. CREDIT BUREAU OPTION  - - - - - - - - - - - -
           03  FILLER     PIC X(8) VALUE "ENNN020 ".
           03  FILLER     PIC X(8) VALUE "E  A010 ".
           03  FILLER     PIC X(8) VALUE "E  A030 ".
           03  FILLER     PIC X(8) VALUE "E  A010 ".
           03  FILLER     PIC X(8) VALUE "E  A030 ".
           03  FILLER     PIC X(8) VALUE "E  A010 ".
           03  FILLER     PIC X(8) VALUE "E  A010 ".
           03  FILLER     PIC X(8) VALUE "E  A010 ".
           03  FILLER     PIC X(8) VALUE "E  A010 ".
      *- - - - - - - - 10. NO CASH PROCEEDS ON DISBURSE  - - - - - - - -
           03  FILLER     PIC X(8) VALUE "E  A010 ".
           03  FILLER     PIC X(8) VALUE "E  A010 ".
           03  FILLER     PIC X(8) VALUE "E  A010 ".
           03  FILLER     PIC X(8) VALUE "E  A010 ".
           03  FILLER     PIC X(8) VALUE "E  A010 ".
           03  FILLER     PIC X(8) VALUE "E  A010 ".
           03  FILLER     PIC X(8) VALUE "E  A010 ".
           03  FILLER     PIC X(8) VALUE "E  A010 ".
           03  FILLER     PIC X(8) VALUE "E  A010 ".
           03  FILLER     PIC X(8) VALUE "ENNN010 ".
           03  FILLER     PIC X(8) VALUE "E  A010 ".
           03  FILLER     PIC X(8) VALUE "E  A010 ".
           03  FILLER     PIC X(8) VALUE "E  A010 ".
           03  FILLER     PIC X(8) VALUE "E  A010 ".
           03  FILLER     PIC X(8) VALUE "ENNN020 ".
           03  FILLER     PIC X(8) VALUE "ENNN020 ".
           03  FILLER     PIC X(8) VALUE "E  A010 ".
           03  FILLER     PIC X(8) VALUE "E  A010 ".
           03  FILLER     PIC X(8) VALUE "E  A010 ".
           03  FILLER     PIC X(8) VALUE "ENNS010 ".
           03  FILLER     PIC X(8) VALUE "E  A010 ".
           03  FILLER     PIC X(8) VALUE "E  A010 ".
           03  FILLER     PIC X(8) VALUE "E  A010 ".
           03  FILLER     PIC X(8) VALUE "E  A010 ".
           03  FILLER     PIC X(8) VALUE "ENNN010 ".
           03  FILLER     PIC X(8) VALUE "E  A010 ".
           03  FILLER     PIC X(8) VALUE "ENNN020 ".

      *-----------------------------------------------------------------
       01  SPEC-TAB REDEFINES SPEC-TABLE.
           03  SPEC-REC OCCURS 36 TIMES.
               05  SPEC PIC X(7).
               05  CHFG PIC X.

       COPY "LIBWI/CDSCANW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBGB/BRMAN4_DEF.CPY".
       COPY "LIBGB/BRMAN4_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       78  FIELDS-SEQ                VALUE 0.

       COPY "LIBGB/GETFMW.CPY".
       01 FORM-LINK-AREA    PIC X(2000).
       SCREEN SECTION.
       COPY "LIBGB/BRMAN4_SCN.CPY".

      ******************************************************************
      *
      *     P R O C E D U R E - D I V I S I O N
      *
      ******************************************************************
       PROCEDURE DIVISION
             USING FORM-PATHNAME FORM-LINK-AREA EXIT-PATHNAME.

      ******************************************************************
       MAIN-PROGRAM SECTION.
      ******************************************************************
           PERFORM SQL-CONNECT.
      D    STOP MESS.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "BRMAN4" TO VDU-FORM-NAME.

           MOVE FORM-LINK-AREA TO BRMAIN-LINK.

       MAIN-MODULE.

           MOVE " " TO VDU-WIMENU-SELECTION
                       VDU-PGMENU-SELECTION.

           PERFORM FORM-RESET.

           PERFORM REC-BR-MODULE.
           IF IO-TYPE = "END"
              MOVE "X" TO ERRCD
              GO TO END-ROUTINE.
           IF IO-TYPE = "CLR"
              GO TO MAIN-MODULE.

           PERFORM ENTRY-MODULE.
           IF IO-TYPE = "RWR"
              PERFORM WRITE-MODULE.

           IF NEW-REC = "Y"
              MOVE "P" TO FORM-P
              MOVE 5 TO FORM-SEL.

           IF FORM-SEL NOT = 0
              IF FORM-P = "P"
                 MOVE BR-NO TO SAVEKEY
                 MOVE FORM-SEL TO FORMNUM
                 GO TO END-ROUTINE.

           MOVE "N" TO NEW-REC.

           GO TO MAIN-MODULE.

      ******************************************************************
       REC-BR-MODULE SECTION.
      ******************************************************************
       REC-BR-MODULE-NO.

           MOVE SPACES TO IO-TYPE.

           IF SAVEKEY NOT = " "
              MOVE SAVEKEY TO BR-NO
              MOVE " " TO SAVEKEY
              MOVE "SN N040BRANCH." TO VDU
              MOVE BR-NO TO VDUNUM0
              PERFORM SCREENIO
              GO TO READ-BRANCH.

           MOVE 0 TO NEW-FLAG.
           MOVE "EN N040BRANCH." TO VDU.
           PERFORM SCREENIO.

           IF F7-KEY
             MOVE "END" TO IO-TYPE
             GO TO END-REC-BR-MODULE.

           IF NOT ENTER-KEY
             GO TO REC-BR-MODULE-NO.

           IF VDUNUM0 = 0
             GO TO REC-BR-MODULE-NO.

           MOVE VDUNUM0 TO BR-NO.

      *-----------------------------------------------------------------
       READ-BRANCH.

           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.

           IF IO-BAD
              GO TO REC-BR-MODULE-NO.

           MOVE BR-REC TO ORIGINAL-BR-REC.
           PERFORM DISPLAY-FIELDS.
           MOVE 0 TO NEW-FLAG.

           IF NEW-REC = "Y"
              MOVE 1 TO NEW-FLAG.

      *-----------------------------------------------------------------
       END-REC-BR-MODULE.
           EXIT.

      ******************************************************************
      *    E N T R Y - M O D U L E
      *
      *    NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *               1 - INITIAL NEW RECORD ENTRY
      *               2 - MODIFY NEW RECORD
      *
      ******************************************************************
       ENTRY-MODULE SECTION.
      ******************************************************************
       BEGIN-ENTRY.

           MOVE SPACES TO IO-TYPE.
           MOVE 1 TO ONE.
           MOVE 0 TO ELE.

       CREATE-ROUTINE.

           IF NEW-FLAG NOT = 1
              GO TO MODIFY-ROUTINE.

           ADD ONE TO ELE.

           IF ELE < 1
              MOVE 1 TO ELE ONE.

           IF ELE NOT > MAX
              GO TO ENTER-ELE.

           MOVE 2 TO NEW-FLAG.

      *-----------------------------------------------------------------
       MODIFY-ROUTINE.

           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.

           IF F1-KEY
              MOVE "CLR" TO IO-TYPE
              MOVE "N" TO NEW-REC
              GO TO END-ENTRY-MODULE.

           IF NOT ENTER-KEY
              GO TO MODIFY-ROUTINE.

           MOVE VDUBUF TO ACTION.

           IF ACTION = SPACES
              MOVE "RWR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

           IF FORM-KEY = " " AND FORM-P = "P" AND FORM-SEL NUMERIC
              IF NOT (FORM-SEL = 1 OR 2 OR 3 OR 5 OR 6 OR 7)
                 GO TO MODIFY-ROUTINE
              ELSE
                 MOVE "RWR" TO IO-TYPE
                 GO TO EXIT-ENTRY-MODULE.

           INSPECT ACTION REPLACING ALL SPACES
              BY ZEROS.

           IF CHNO NOT NUMERIC
              GO TO MODIFY-ROUTINE.

           IF CHNO = 0
              GO TO MODIFY-ROUTINE.

           IF CHNO > MAX
              GO TO MODIFY-ROUTINE.

           MOVE CHNO TO ELE.

      * LIMIT ACCESS TO ALL FIELDS 
           IF NEW-FLAG = 0
              MOVE FIELDS-SEQ                    TO PASSWDW-SEQ
              MOVE "TO CHANGE THIS FIELD......." TO PASSWDW-PROMPT2
              PERFORM VERIFY-PASSWORD
              IF PASSWDW-STAT NOT = "OK"
                 GO TO MODIFY-ROUTINE.

      *-----------------------------------------------------------------
       ENTER-ELE.

           IF CHKEY NOT = ZEROS
              IF CHFG(ELE) = " "
                 IF NEW-FLAG NOT = 1
                    GO TO MODIFY-ROUTINE.

           IF CHFG(ELE) = "N"
              IF NEW-FLAG = 1
                 GO TO CREATE-ROUTINE
            ELSE
              IF CHKEY NOT = EXT-CONAME-DAILY-PASSWORD
                 GO TO MODIFY-ROUTINE.

           MOVE SPEC(ELE) TO VDU.
           MOVE ELE TO VDUROW.
           MOVE 0 TO VDUCOL.
           MOVE "." TO VDUDECIMAL.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       TEST-F4.
           IF UP-KEY
             IF NEW-FLAG = 1
              MOVE -1 TO ONE
              GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       TEST-F5.
           IF DOWN-KEY
              MOVE 1 TO ONE
              GO TO GOTO-FIELD.

      *-----------------------------------------------------------------
       TEST-F6.
           IF NOT F6-KEY
              GO TO TEST-F1.

           IF (ELE = 3 OR 5)
               MOVE "PU" TO CDSCAN-TYPE
               PERFORM CODE-SCAN
               IF IO-GOOD
                  GO TO GOTO-FIELD
               ELSE
                  GO TO ENTER-ELE.

      *-----------------------------------------------------------------
       TEST-F1.
           IF F1-KEY
              MOVE "CLR" TO IO-TYPE
              MOVE "N" TO NEW-REC
              GO TO END-ENTRY-MODULE.

           IF NOT ENTER-KEY
              GO TO ENTER-ELE.

           MOVE 1 TO ONE.

      *-----------------------------------------------------------------
       GOTO-FIELD.
           GO TO F-01 F-02 F-03 F-04 F-05 F-06 F-07 F-08 F-09 F-10
                 F-11 F-12 F-13 F-14 F-15 F-16 F-17 F-18 F-19 F-20
                 F-21 F-22 F-23 F-24 F-25 F-26 F-27 F-28 F-29 F-30
                 F-31 F-32 F-33 F-34 F-35 F-36
                 DEPENDING ON ELE.
           GO TO END-ENTRY-MODULE.

      *=================================================================
      *
      *    S E T - R E C O R D - E L E M E N T S
      *
      *=================================================================
       F-01.

           MOVE VDUNUM0 TO BR-MIN-MONTHS-DEF.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-02.
           MOVE VDUBUF TO BR-DEFAULT-SF-UNITPER-CD.

           IF NOT (VDUBUF = "M" OR "W" OR "B" OR "S" OR " ")
              MOVE "ENTER: 'M' OR 'W' OR 'B' OR 'S' OR ' '" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-03.
           MOVE VDUBUF TO BR-DEFAULT-SF-PURCD
                          CD-CODE.
           MOVE "PU"   TO CD-TYPE.
           PERFORM GET-CD.
           IF IO-BAD
                  AND BR-DEFAULT-SF-PURCD NOT = " "
              MOVE "INVALID CODE, F6-SCAN" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           PERFORM SEND-03.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-04.
           MOVE VDUBUF TO BR-DEFAULT-LN-UNITPER-CD.

           IF NOT (VDUBUF = "M" OR "W" OR "B" OR "S")
              MOVE "ENTER: 'M' OR 'W' OR 'B' OR 'S'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-05.
           MOVE VDUBUF TO BR-DEFAULT-LN-PURCD
                          CD-CODE.
           MOVE "PU"   TO CD-TYPE.
           PERFORM GET-CD.
           IF IO-BAD AND BR-DEFAULT-LN-PURCD NOT = " "
              MOVE "INVALID CODE, F6-SCAN" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           PERFORM SEND-05.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-06.
           MOVE VDUBUF TO BR-SET-COLLCD-DL-MFGCD.

           IF NOT (BR-SET-COLLCD-DL-MFGCD = "Y" OR "N")
              MOVE "ENTER 'Y' OR 'N'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-07.
           MOVE VDUBUF TO BR-NO-PROMPT-FOR-OTHBR.

           IF NOT (BR-NO-PROMPT-FOR-OTHBR = "Y" OR "N")
              MOVE "ENTER 'Y' OR 'N'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-08.
           MOVE VDUBUF TO BR-TAP-OTHER-DL.

           IF NOT (BR-TAP-OTHER-DL = "Y" OR "N")
              MOVE "ENTER 'Y' OR 'N'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-09.
           MOVE VDUBUF TO BR-AH-AND-CL.

           IF NOT (BR-AH-AND-CL = "Y" OR "N")
              MOVE "ENTER 'Y' OR 'N'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-10.
           MOVE VDUBUF TO BR-NO-CASH-PROCEEDS.

           IF NOT (BR-NO-CASH-PROCEEDS-VALID )
              MOVE "ENTER 'Y', 'N', OR 'B'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-11.
           MOVE VDUBUF TO BR-RI-BEFORE-REPO.

           IF NOT (BR-RI-BEFORE-REPO = "Y" OR "N")
              MOVE "ENTER 'Y' OR 'N'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-12.
           MOVE VDUBUF TO BR-RI-BEFORE-P4P5.

           IF NOT (BR-RI-BEFORE-P4P5 = "Y" OR "N")
              MOVE "ENTER 'Y' OR 'N'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-13.
           MOVE VDUBUF TO BR-NOT-OURS-REFUND.

           IF NOT (BR-NOT-OURS-REFUND = "Y" OR "N")
              MOVE "ENTER 'Y' OR 'N'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-14.
           MOVE VDUBUF TO BR-SFDLO3-WINDOW-FG.

           IF NOT (BR-SFDLO3-WINDOW-FG = "Y" OR "N" OR "B")
              MOVE "ENTER 'Y' OR 'N' OR 'B'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-15.
           MOVE VDUBUF TO BR-CALC-INS-ADJ.

           IF NOT (BR-CALC-INS-ADJ = "Y" OR "N")
              MOVE "ENTER 'Y' OR 'N'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-16.
           MOVE VDUBUF TO BR-AC-BEFORE-REPO.

           IF NOT (BR-AC-BEFORE-REPO = "Y" OR "N")
              MOVE "ENTER 'Y' OR 'N'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-17.
           MOVE VDUBUF TO BR-AC-BEFORE-P4P5.

           IF NOT (BR-AC-BEFORE-P4P5 = "Y" OR "N")
              MOVE "ENTER 'Y' OR 'N'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

       F-18.
           MOVE VDUBUF TO BR-COLL-QUEUE-CON-REC.

           IF NOT (BR-COLL-QUEUE-CON-REC = "C" OR "N")
              MOVE "ENTER 'C' OR 'N'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-19.
           MOVE VDUNUM0 TO BR-ACH-BANK-NO.
           IF NOT (BR-ACH-BANK-NO = 1 OR 2)
              MOVE "PLEASE ENTER '1' OR '2'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.

       F-20.

           IF NOT ( VDUBUF = "Y" OR "N" OR "W" )
              MOVE "ENTER 'Y' OR 'N' OR 'W'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           MOVE VDUBUF TO BR-APP-REQUIRED-FOR-INPUT.

           GO TO CREATE-ROUTINE.

       F-21.
           MOVE VDUBUF TO BR-EXPAND-APP-INPUT.

           IF NOT (BR-EXPAND-APP-INPUT = "Y" OR "N")
              MOVE "ENTER 'Y' OR 'N'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

       F-22.
           MOVE VDUBUF TO BR-INBR-BILNSC-FG.

           IF NOT BR-INBR-BILNSC-FG-VALID
              MOVE "INVALID ENTRY, USE F8-HELP FOR MORE INFORMATION"
                                                              TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

       F-23.
           MOVE VDUBUF TO BR-INBR-BRIEF-FG.

           IF NOT (BR-INBR-BRIEF-FG = "Y" OR "N")
              MOVE "ENTER 'Y' OR 'N'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

       F-24.
           MOVE VDUNUM0 TO BR-BYINQ-START-PAGE.

           IF NOT (BR-BYINQ-START-PAGE = 1 OR 2 OR 3 OR 4 OR 5 OR 6 OR
                                         7 OR 8 OR 9)
              MOVE "ENTER 1, 2, 3, 4, 5, 6, 7, 8 OR 9" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

       F-25.
           MOVE VDUNUM0 TO BR-DSINQ-START-PAGE.

           IF NOT (BR-DSINQ-START-PAGE = 1 OR 2 OR 3 OR 4)
              MOVE "ENTER 1, 2, 3, OR 4" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

       F-26.
           MOVE VDUBUF TO BR-CREDIT-SCORING-FG.

           IF NOT (BR-CREDIT-SCORING-FG = "Y" OR "N")
              MOVE "ENTER 'Y' OR 'N'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

       F-27.
           MOVE VDUBUF TO BR-COL-QUEUE-LOW-TO-HI.

           IF NOT (BR-COL-QUEUE-LOW-TO-HI = "Y" OR "N")
              MOVE "ENTER 'Y' OR 'N'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

       F-28.
           MOVE VDUBUF TO BR-AUTO-LNSTAT-WINDOW.

           IF NOT (BR-AUTO-LNSTAT-WINDOW = "Y" OR "N")
              MOVE "ENTER 'Y' OR 'N'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

       F-29.
           MOVE VDUNUM0 TO BR-TIME-ZONE-FACTOR.
           GO TO CREATE-ROUTINE.

       F-30.
           MOVE VDUBUF TO BR-DUEDATE-CHANGE-CD.
           IF NOT (BR-DUEDATE-CHANGE-CD = " " OR "M" OR "R")
              MOVE "ENTER ' ' OR 'M' OR 'R' ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.

       F-31.
           MOVE VDUBUF TO BR-GL-TYPE-ALLOWED.
           IF NOT (BR-GL-TYPE-ALLOWED = "N" OR "A" OR "B")
              MOVE "ENTER 'N' OR 'A' OR 'B' ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.

       F-32.
           MOVE VDUBUF TO BR-GL-ENTRY-DEFAULT.
           IF NOT (BR-GL-ENTRY-DEFAULT = "N" OR "A")
              MOVE "ENTER 'N' OR 'A' ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.

       F-33.
           MOVE VDUBUF TO BR-MIDSTREAM-1STPAY-FG.
           IF NOT (BR-MIDSTREAM-1STPAY-FG = "N" OR "Y")
              MOVE "ENTER 'N' OR 'Y' ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.

       F-34.
           MOVE VDUNUM0 TO BR-NODEF-1ST-X-MONTHS.
           GO TO CREATE-ROUTINE.

       F-35.
           MOVE VDUBUF TO BR-NODEF-IF-ZERO-AMT.
           IF NOT (BR-NODEF-IF-ZERO-AMT = "N" OR "Y")
              MOVE "ENTER 'N' OR 'Y' ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-36.
           MOVE VDUNUM0 TO BR-MIN-MONTHS-D2.
           GO TO CREATE-ROUTINE.

       END-ENTRY-MODULE.
           MOVE 0 TO FORM-SEL.

       EXIT-ENTRY-MODULE.
           EXIT.


      ******************************************************************
       VERIFY-PASSWORD SECTION.
      ******************************************************************
           MOVE " "                          TO PASSWDW-STAT.
           MOVE "BRMAN4"                     TO PASSWDW-PGM.
           MOVE "PLEASE ENTER THE PASSWORD," TO PASSWDW-PROMPT1.
           MOVE "CL/PASSWD"                  TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
           CANCEL FORM-PROGX.

       EXIT-VERIFY-PASSWORD.
           EXIT.

      ******************************************************************
       WRITE-MODULE SECTION.
      ******************************************************************

           MOVE STAT---BAD TO UPDATE-BR-STAT.

       REWRITE-RECORD.

           PERFORM OPEN-BR-FILE.
           MOVE BR-REC TO CHANGED-BR-REC.

           IF ORIGINAL-BR-REC = CHANGED-BR-REC
              GO TO END-WRITE-MODULE.

           PERFORM READ-BR-FILE.

           IF IO-BAD
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-WRITE-MODULE.

           IF BR-REC NOT = ORIGINAL-BR-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-WRITE-MODULE.

           MOVE CHANGED-BR-REC TO BR-REC.
           PERFORM REWRITE-BR-FILE.
           MOVE STAT---GOOD TO UPDATE-BR-STAT.

      * CODE TO WRITE CHANGE FILE.
      *    MOVE "C" TO ERRLOGW-ACTION.
      *    MOVE BR-NO TO ERRLOGW-KEY.
      *    MOVE FORM-PATHNAME TO ERRLOGW-PROGX.
      *    MOVE "CL/ERRLOG" TO FORM-NAM.
      *    CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
      *    CANCEL FORM-PROGX.

       END-WRITE-MODULE.
           PERFORM CLOSE-BR-FILE.

           IF ( UPDATE-BR-STAT-GOOD ) 
              PERFORM LOG-MAINTENANCE-CHGS.

      ******************************************************************
       LOG-MAINTENANCE-CHGS SECTION.
      ******************************************************************

           MOVE ORIGINAL-BR-REC               TO BROR-REC.
           MOVE CHANGED-BR-REC                TO BRCH-REC.

           MOVE SPACES                     TO LMC-DETAIL.
           MOVE BR-NO                      TO LMC-BRNO.
           MOVE " TO "                     TO LMC-TO-X.

           IF BROR-MIN-MONTHS-DEF NOT = BRCH-MIN-MONTHS-DEF
             MOVE "01 "                   TO LMC-FIELD
             MOVE "MIN MONTH"             TO LMC-DESC
             MOVE BROR-MIN-MONTHS-DEF     TO LMC-ORG-NUM
             MOVE BRCH-MIN-MONTHS-DEF     TO LMC-CHG-NUM
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-DEFAULT-SF-UNITPER-CD NOT =
              BRCH-DEFAULT-SF-UNITPER-CD
             MOVE "02 "                      TO LMC-FIELD
             MOVE "SF PAY CD"                TO LMC-DESC
             MOVE BROR-DEFAULT-SF-UNITPER-CD TO LMC-ORG-TEXT
             MOVE BRCH-DEFAULT-SF-UNITPER-CD TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-DEFAULT-SF-PURCD NOT = BRCH-DEFAULT-SF-PURCD
             MOVE "03 "                   TO LMC-FIELD
             MOVE "SF PUR CD"             TO LMC-DESC
             MOVE BROR-DEFAULT-SF-PURCD   TO LMC-ORG-TEXT
             MOVE BRCH-DEFAULT-SF-PURCD   TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-DEFAULT-LN-UNITPER-CD NOT =
              BRCH-DEFAULT-LN-UNITPER-CD
             MOVE "04 "                      TO LMC-FIELD
             MOVE "LN PAY CD"                TO LMC-DESC
             MOVE BROR-DEFAULT-LN-UNITPER-CD TO LMC-ORG-TEXT
             MOVE BRCH-DEFAULT-LN-UNITPER-CD TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-DEFAULT-LN-PURCD NOT = BRCH-DEFAULT-LN-PURCD
             MOVE "05 "                   TO LMC-FIELD
             MOVE "LN PUR CD"             TO LMC-DESC
             MOVE BROR-DEFAULT-LN-PURCD   TO LMC-ORG-TEXT
             MOVE BRCH-DEFAULT-LN-PURCD   TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-SET-COLLCD-DL-MFGCD NOT = BRCH-SET-COLLCD-DL-MFGCD
             MOVE "06 "                    TO LMC-FIELD
             MOVE "SET COLCD"              TO LMC-DESC
             MOVE BROR-SET-COLLCD-DL-MFGCD TO LMC-ORG-TEXT
             MOVE BRCH-SET-COLLCD-DL-MFGCD TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-NO-PROMPT-FOR-OTHBR NOT = BRCH-NO-PROMPT-FOR-OTHBR
             MOVE "07 "                    TO LMC-FIELD
             MOVE "BR PAPER "              TO LMC-DESC
             MOVE BROR-NO-PROMPT-FOR-OTHBR TO LMC-ORG-TEXT
             MOVE BRCH-NO-PROMPT-FOR-OTHBR TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-TAP-OTHER-DL NOT = BRCH-TAP-OTHER-DL
             MOVE "08 "                   TO LMC-FIELD
             MOVE "DLR RSV  "             TO LMC-DESC
             MOVE BROR-TAP-OTHER-DL       TO LMC-ORG-TEXT
             MOVE BRCH-TAP-OTHER-DL       TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-AH-AND-CL NOT = BRCH-AH-AND-CL
             MOVE "09 "                   TO LMC-FIELD
             MOVE "WRITE CL "             TO LMC-DESC
             MOVE BROR-AH-AND-CL          TO LMC-ORG-TEXT
             MOVE BRCH-AH-AND-CL          TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-NO-CASH-PROCEEDS NOT = BRCH-NO-CASH-PROCEEDS
             MOVE "10 "                   TO LMC-FIELD
             MOVE "NO PROCED"             TO LMC-DESC
             MOVE BROR-NO-CASH-PROCEEDS   TO LMC-ORG-TEXT
             MOVE BRCH-NO-CASH-PROCEEDS   TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-RI-BEFORE-REPO NOT = BRCH-RI-BEFORE-REPO
             MOVE "11 "                   TO LMC-FIELD
             MOVE "INT REPO "             TO LMC-DESC
             MOVE BROR-RI-BEFORE-REPO     TO LMC-ORG-TEXT
             MOVE BRCH-RI-BEFORE-REPO     TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-RI-BEFORE-P4P5 NOT = BRCH-RI-BEFORE-P4P5
             MOVE "12 "                   TO LMC-FIELD
             MOVE "INT P4 P5"             TO LMC-DESC
             MOVE BROR-RI-BEFORE-P4P5     TO LMC-ORG-TEXT
             MOVE BRCH-RI-BEFORE-P4P5     TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-NOT-OURS-REFUND NOT = BRCH-NOT-OURS-REFUND
             MOVE "13 "                   TO LMC-FIELD
             MOVE "REFND INS"             TO LMC-DESC
             MOVE BROR-NOT-OURS-REFUND    TO LMC-ORG-TEXT
             MOVE BRCH-NOT-OURS-REFUND    TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-SFDLO3-WINDOW-FG NOT = BRCH-SFDLO3-WINDOW-FG
             MOVE "14 "                   TO LMC-FIELD
             MOVE "DIS WINDW"             TO LMC-DESC
             MOVE BROR-SFDLO3-WINDOW-FG   TO LMC-ORG-TEXT
             MOVE BRCH-SFDLO3-WINDOW-FG   TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-CALC-INS-ADJ NOT = BRCH-CALC-INS-ADJ
             MOVE "15 "                   TO LMC-FIELD
             MOVE "CALC INS "             TO LMC-DESC
             MOVE BROR-CALC-INS-ADJ       TO LMC-ORG-TEXT
             MOVE BRCH-CALC-INS-ADJ       TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-AC-BEFORE-REPO NOT = BRCH-AC-BEFORE-REPO
             MOVE "16 "                   TO LMC-FIELD
             MOVE "REPO ACEL"             TO LMC-DESC
             MOVE BROR-AC-BEFORE-REPO     TO LMC-ORG-TEXT
             MOVE BRCH-AC-BEFORE-REPO     TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-AC-BEFORE-P4P5 NOT = BRCH-AC-BEFORE-P4P5
             MOVE "17 "                   TO LMC-FIELD
             MOVE "P4P5 ACEL"             TO LMC-DESC
             MOVE BROR-AC-BEFORE-P4P5     TO LMC-ORG-TEXT
             MOVE BRCH-AC-BEFORE-P4P5     TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-COLL-QUEUE-CON-REC NOT = BRCH-COLL-QUEUE-CON-REC
             MOVE "18 "                   TO LMC-FIELD
             MOVE "COL QUEUE"             TO LMC-DESC
             MOVE BROR-COLL-QUEUE-CON-REC TO LMC-ORG-TEXT
             MOVE BRCH-COLL-QUEUE-CON-REC TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-APP-REQUIRED-FOR-INPUT NOT =
              BRCH-APP-REQUIRED-FOR-INPUT
             MOVE "20 "                       TO LMC-FIELD
             MOVE "LN INPUT "                 TO LMC-DESC
             MOVE BROR-APP-REQUIRED-FOR-INPUT TO LMC-ORG-TEXT
             MOVE BRCH-APP-REQUIRED-FOR-INPUT TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-EXPAND-APP-INPUT NOT = BRCH-EXPAND-APP-INPUT
             MOVE "21 "                   TO LMC-FIELD
             MOVE "EXP INPUT"             TO LMC-DESC
             MOVE BROR-EXPAND-APP-INPUT   TO LMC-ORG-TEXT
             MOVE BRCH-EXPAND-APP-INPUT   TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-INBR-BILNSC-FG NOT = BRCH-INBR-BILNSC-FG
             MOVE "22 "                   TO LMC-FIELD
             MOVE "USE SCAN "             TO LMC-DESC
             MOVE BROR-INBR-BILNSC-FG     TO LMC-ORG-TEXT
             MOVE BRCH-INBR-BILNSC-FG     TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-INBR-BRIEF-FG NOT = BRCH-INBR-BRIEF-FG
             MOVE "23 "                   TO LMC-FIELD
             MOVE "BRIEF PST"             TO LMC-DESC
             MOVE BROR-INBR-BRIEF-FG      TO LMC-ORG-TEXT
             MOVE BRCH-INBR-BRIEF-FG      TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-BYINQ-START-PAGE NOT = BRCH-BYINQ-START-PAGE
             MOVE "24 "                   TO LMC-FIELD
             MOVE "BR STATS "             TO LMC-DESC
             MOVE BROR-BYINQ-START-PAGE   TO LMC-ORG-NUM
             MOVE BRCH-BYINQ-START-PAGE   TO LMC-CHG-NUM
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-DSINQ-START-PAGE NOT = BRCH-DSINQ-START-PAGE
             MOVE "25 "                   TO LMC-FIELD
             MOVE "DLR STATS"             TO LMC-DESC
             MOVE BROR-DSINQ-START-PAGE   TO LMC-ORG-NUM
             MOVE BRCH-DSINQ-START-PAGE   TO LMC-CHG-NUM
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-CREDIT-SCORING-FG NOT = BRCH-CREDIT-SCORING-FG
             MOVE "26 "                   TO LMC-FIELD
             MOVE "CRED SCOR"             TO LMC-DESC
             MOVE BROR-CREDIT-SCORING-FG  TO LMC-ORG-TEXT
             MOVE BRCH-CREDIT-SCORING-FG  TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-COL-QUEUE-LOW-TO-HI NOT = BRCH-COL-QUEUE-LOW-TO-HI
             MOVE "27 "                    TO LMC-FIELD
             MOVE "QUEUE DEL"              TO LMC-DESC
             MOVE BROR-COL-QUEUE-LOW-TO-HI TO LMC-ORG-TEXT
             MOVE BRCH-COL-QUEUE-LOW-TO-HI TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-AUTO-LNSTAT-WINDOW NOT = BRCH-AUTO-LNSTAT-WINDOW
             MOVE "28 "                   TO LMC-FIELD
             MOVE "AUTO OPEN"             TO LMC-DESC
             MOVE BROR-AUTO-LNSTAT-WINDOW TO LMC-ORG-NUM
             MOVE BRCH-AUTO-LNSTAT-WINDOW TO LMC-CHG-NUM
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-TIME-ZONE-FACTOR NOT = BRCH-TIME-ZONE-FACTOR
             MOVE "29 "                   TO LMC-FIELD
             MOVE "TIME ZONE"             TO LMC-DESC
             MOVE BROR-TIME-ZONE-FACTOR   TO LMC-ORG-NUM
             MOVE BRCH-TIME-ZONE-FACTOR   TO LMC-CHG-NUM
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-DUEDATE-CHANGE-CD NOT = BRCH-DUEDATE-CHANGE-CD
             MOVE "30 "                   TO LMC-FIELD
             MOVE "DUE DATE "             TO LMC-DESC
             MOVE BROR-DUEDATE-CHANGE-CD  TO LMC-ORG-TEXT
             MOVE BRCH-DUEDATE-CHANGE-CD  TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-GL-TYPE-ALLOWED NOT = BRCH-GL-TYPE-ALLOWED
             MOVE "31 "                   TO LMC-FIELD
             MOVE "GL TYPE  "             TO LMC-DESC
             MOVE BROR-GL-TYPE-ALLOWED    TO LMC-ORG-TEXT
             MOVE BRCH-GL-TYPE-ALLOWED    TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-GL-ENTRY-DEFAULT NOT = BRCH-GL-ENTRY-DEFAULT
             MOVE "32 "                   TO LMC-FIELD
             MOVE "GL DEFAUL"             TO LMC-DESC
             MOVE BROR-GL-ENTRY-DEFAULT   TO LMC-ORG-NUM
             MOVE BRCH-GL-ENTRY-DEFAULT   TO LMC-CHG-NUM
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-MIDSTREAM-1STPAY-FG NOT = BRCH-MIDSTREAM-1STPAY-FG
             MOVE "33 "                    TO LMC-FIELD
             MOVE "MISS 1PAY"              TO LMC-DESC
             MOVE BROR-MIDSTREAM-1STPAY-FG TO LMC-ORG-NUM
             MOVE BRCH-MIDSTREAM-1STPAY-FG TO LMC-CHG-NUM
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-NODEF-1ST-X-MONTHS NOT = BRCH-NODEF-1ST-X-MONTHS
             MOVE "34 "                   TO LMC-FIELD
             MOVE "DEFERMNTS"             TO LMC-DESC
             MOVE BROR-NODEF-1ST-X-MONTHS TO LMC-ORG-NUM
             MOVE BRCH-NODEF-1ST-X-MONTHS TO LMC-CHG-NUM
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-NODEF-IF-ZERO-AMT NOT = BRCH-NODEF-IF-ZERO-AMT
             MOVE "35 "                   TO LMC-FIELD
             MOVE "0 DEFER  "             TO LMC-DESC
             MOVE BROR-NODEF-IF-ZERO-AMT  TO LMC-ORG-TEXT
             MOVE BRCH-NODEF-IF-ZERO-AMT  TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

       LOG-MAINTENANCE-CHGS-EXIT.
           EXIT.

      ******************************************************************
       LOG-FIELD-CHANGE SECTION.
      ******************************************************************

           MOVE "F"           TO ERRLOGW-ACTION.
           MOVE LMC-DETAIL    TO ERRLOGW-DETAIL.
           MOVE FORM-PATHNAME TO ERRLOGW-PROGX.

           MOVE "CL/ERRLOG" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PROGX.

       LOG-FIELD-CHANGE-EXIT.
           EXIT.

      ******************************************************************
       DISPLAY-FIELDS SECTION.
      ******************************************************************

           MOVE SPACES TO DISPLAY-BUF.

           MOVE BR-MIN-MONTHS-DEF TO F01.

           MOVE BR-DEFAULT-SF-UNITPER-CD TO F02.
           MOVE BR-DEFAULT-SF-PURCD TO F03 CD-CODE.
           MOVE "PU" TO CD-TYPE.
           PERFORM GET-CD.
           PERFORM SEND-03.

           MOVE BR-DEFAULT-LN-UNITPER-CD TO F04.
           MOVE BR-DEFAULT-LN-PURCD TO F05 CD-CODE.
           MOVE "PU" TO CD-TYPE.
           PERFORM GET-CD.
           PERFORM SEND-05.

           MOVE BR-SET-COLLCD-DL-MFGCD TO F06.
           MOVE BR-NO-PROMPT-FOR-OTHBR TO F07.
           MOVE BR-TAP-OTHER-DL        TO F08.
           MOVE BR-AH-AND-CL           TO F09.
           MOVE BR-NO-CASH-PROCEEDS    TO F10.
           MOVE BR-RI-BEFORE-REPO      TO F11.
           MOVE BR-RI-BEFORE-P4P5      TO F12.
           MOVE BR-NOT-OURS-REFUND     TO F13.
           MOVE BR-SFDLO3-WINDOW-FG    TO F14.
           MOVE BR-CALC-INS-ADJ        TO F15.
           MOVE BR-AC-BEFORE-REPO      TO F16.
           MOVE BR-AC-BEFORE-P4P5      TO F17.
           MOVE BR-COLL-QUEUE-CON-REC  TO F18.
           MOVE BR-ACH-BANK-NO         TO F19.
           MOVE BR-APP-REQUIRED-FOR-INPUT TO F20.
           MOVE BR-EXPAND-APP-INPUT    TO F21.
           MOVE BR-INBR-BILNSC-FG      TO F22.
           MOVE BR-INBR-BRIEF-FG       TO F23.
           MOVE BR-BYINQ-START-PAGE    TO F24.
           MOVE BR-DSINQ-START-PAGE    TO F25.
           MOVE BR-CREDIT-SCORING-FG   TO F26.
           MOVE BR-COL-QUEUE-LOW-TO-HI TO F27.
           MOVE BR-AUTO-LNSTAT-WINDOW  TO F28.
           MOVE BR-TIME-ZONE-FACTOR    TO F29.
           MOVE BR-DUEDATE-CHANGE-CD   TO F30.
           MOVE BR-GL-TYPE-ALLOWED     TO F31.
           MOVE BR-GL-ENTRY-DEFAULT    TO F32.
           MOVE BR-MIDSTREAM-1STPAY-FG TO F33.

           IF BR-NODEF-1ST-X-MONTHS NOT NUMERIC
              MOVE 0 TO BR-NODEF-1ST-X-MONTHS.
           MOVE BR-NODEF-1ST-X-MONTHS  TO F34.
           MOVE BR-NODEF-IF-ZERO-AMT   TO F35.

           MOVE BR-MIN-MONTHS-D2  TO F36.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       DISPLAY-FIELDS-EXIT.
           EXIT.

      ******************************************************************
       CODE-SCAN SECTION.
      ******************************************************************

           MOVE SPACES TO CDSCAN-CODE.
           MOVE "WI/CDSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CDSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF CDSCAN-STATUS NOT = "00" OR CDSCAN-CODE = " "
              MOVE 9 TO IO-FG
              GO TO EXIT-CODE-SCAN.

           MOVE "S  A020  0." TO VDU.
           MOVE ELE TO VDUROW.
           MOVE CDSCAN-CODE TO VDUBUF.
           PERFORM SCREENIO.
           MOVE CDSCAN-CODE TO VDUBUF.
           MOVE 0 TO IO-FG.

       EXIT-CODE-SCAN.
           EXIT.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".

      **************************************************************************
       FORM-RESET SECTION.
      **************************************************************************
           MOVE EXT-CONAME-CONAME TO BRMAN4-CONAME.
           MOVE EXT-CONAME-SYSDATE TO BRMAN4-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY BRMAN4-SCREEN.
           DISPLAY BRMAN4-SCREEN UPON BRMAIN-WINDOW-HANDLE.
           MOVE BRMAN4-FORM-FIELDS TO WR-BUF.
           MOVE BRMAN4-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **************************************************************************
       SETUP-LINK-INFO SECTION.
      **************************************************************************
           MOVE BRMAN4-HELP--FILE TO HELP-LINK-FILE.
           MOVE BRMAN4-HELP--DIR TO HELP-LINK-DIR.

      **************************************************************************
       VDU-CONVERT-FIELD SECTION.
      **************************************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBGB/BRMAN4_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISC-PARAGRAPHS SECTION.
      ******************************************************************
       COPY "LIBGB/ACCESS.CPY".
      *-----------------------------------------------------------------
       RECORD-DELETED.
           MOVE "RECORD WAS DELETED!" TO MESS.
           PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       RECORD-CHANGED.
           MOVE "CHANGES WERE MADE TO THIS RECORD!" TO MESS.
           PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       CHANGE-AGAIN.
           MOVE "PLEASE RE-INPUT YOUR CHANGES!" TO MESS.
           PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       ALREADY-CREATED.
           MOVE "RECORD HAS BEEN ALREADY CREATED!" TO MESS.
           PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       UNDERGONE-CHANGE.
           MOVE
           "THIS RECORD HAS UNDERGONE CHANGE BY ANOTHER USER,"
           TO MESS.
           PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       ACTIVE-RECORD.
           MOVE "ACTIVE CUSTOMER CANNOT BE DELETED" TO MESS.
           PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       SEND-LEGEND.
           MOVE "S  A000MSEL." TO VDU.
           MOVE MESS TO VDUBUF.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       GET-CD.
           PERFORM OPEN-CD1-FILE.
           PERFORM READ-CD1-FILE.
           PERFORM CLOSE-CD1-FILE.
           IF IO-BAD
              MOVE "INVALID CODE" TO CD-DESC.

      *-----------------------------------------------------------------
       SEND-03.
           MOVE "S  A300031." TO VDU.

           IF CD-CODE NOT = " "
              MOVE CD-DESC TO VDUBUF.

           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       SEND-05.
           MOVE "S  A300051." TO VDU.

           IF CD-CODE NOT = " "
              MOVE CD-DESC TO VDUBUF.

           PERFORM SCREENIO.

      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-CD1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRIN.CPY".
       COPY "LIBLP/LPCD1IN.CPY".

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      ******************************************************************
       END-ROUTINE SECTION.
      ******************************************************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY BRMAN4-SCREEN.
           MOVE FORM-PATH TO FORM-PATHNAME.
           MOVE BRMAIN-LINK TO FORM-LINK-AREA.
           EXIT PROGRAM.
