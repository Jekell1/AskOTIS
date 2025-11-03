       IDENTIFICATION DIVISION.
       PROGRAM-ID.      BRMAN7.
      ******************************************************************
      *
      *  DIR :   GB
      *  DESC:   BRANCH FILE MAINTENANCE PAGE 7
      *  REV :
      *
      * REV:
      *  KEC 121797
      *  MJD 031398 ADDED 78 LEVEL: BR-SIZE
      *  BAH 032498 ADDED BR-IMMEDIATE-RECEIPT AND BR-IMMEDIATE-DOC-PRINT
      *  BAH 060298 ADDED BR-AUTO-PL-REF-AC FOR MERCURY #E180
      *  BAH 061598 MOVED BR-FXPRESS-OPTION TO BROPMN, PURCHASABLE !!
      *  KEC 011899 ADDED BR-REVOLVING-CONV-DATE               PR#: 0000
      *  BAH 990413 ADDED CALL TO CREATE CHANGES FILE AFTER REWRITE
      *  MJD 990621 ADDED BR-ALLOW-SYS-AS-EFFDATE
      *  MG  991005 FIXED VDUSTATUS/IO-FG/STAT PER DEVELOPMENT MEETING.
      *  KEC 991130 ADDED BR-LI-LISCAN-FG (CMSI/CSI)
      *  MJD 000911 ADDED BR-LBOX (MFC PR#227)
      *  JTG 001117 ADDED LOGIC FOR 'OV' OVERPAYMENT TRANSACTION PR#J015
      *  BLV 010605 ADDED BR-ALLOW-INTERBR: IF "N" DON'T ALLOW OTHER
      *             BRANCHES TO POST INTERBRANCH PMTS, REGACC PR# 1510
      *  BLV 011210 ADDED BR-COL-QUEUE-PAST-DUE, REGACC PR# 1677
      *  BAH 020405 REMOVED BR-FORCE-VALID-CITY-COUNTY, FIELD 17, MOVED TO
      *             BROPMN, WORLD #288
      *  CS  020925 ADDED BR-LI-DLCOMM-CMSI-PP-MAX, BR-LI-DLCOMM-CMSI-O3-MAX TO
      *             HOLD MAX DEALER COMMISION OF THESE INSURANCES.. FED FROM
      *             CMSI(LOAN INTERFACE UPD) TO LOAN MANAGER
      *  BAH 030409 REMOVED GP-I-AM-WORLD FOR ROADGARD PROMPTS !!!!!!!
      *  BAH 030507 ADDED 18. BR-ASSESS-LATE-CHGS, MODERN PR# 2036
      *  KEC 030918 {PR#00039} REGIONAL ACCEPTANCE <A15> [KEVIN MULLEN]
      *             ADDED 06. BR-LR-GROUP-COLLID (TRIAD)
      *  BLV 031120 ADD FIELD 31, BR-INS-PRODUCER, REDEFINITION OF
      *             BR-STMNT2 FOR WORLD, THEY NEED THIS IN BRANCH, OTHER
      *             CUSTOMERS WILL STILL USE THIS FIELD IN GLOBAL
      *  BLV 030324 ADD FIELDS 19 & 20, BR-JE-ALLOCATION-SKIP &
      *             BR-WORLD-BRANCH-TYPE, CUSTOM WORLD FLAGS
      *  BLV 030615 ADD FIELD 21, BR-COL-QUEUE-BEHAVIOR-FG, REGACC PR 59
      *  BAH 050224 ADDED BR-AUTO-PL-REF-AC VALUES OF "2" AND "3" FOR
      *             P2 AND P3 TO AUTO REFUND AND ACCEL, LENDMARK PR# 2473
      *             "2" = PL AND P2, "3" = PL, P2 AND P3 WILL ALL WORK
      *  BAH 061205 ADDED FIELD 22, BR-COUPON-STATEMENT, REGACC PR# 158
      *             ALSO ADDED THE B2FILE (BRANCH TRAILER)
      *  AMM 070308 CREATED A LOG FIELD CHANGE ROUTINE
      *  BAH 070710 REMOVED THE CODE FOR ONLY WORLD TO ACCESS FIELDS
      *             33, 34 AND 35, FINMGM PR# 3008
      *  BAH 090826 ADDED 24. BR-SKIP-MIDSTREAM-TRW FOR REGACC PR# 252
      *  BAH 120514 REMOVED SPECIAL-FG
      *  BLM 141230 ADD #26, ADP PAY GROUP, WORLD PR# 1035.  ALSO UPDATED
      *             MAINTENANCE LOG FOR FIELDS 23, 24, 25, 26
      *  BAH 151021 REMOVED B2FILE, REMOVED 32. REVOLVING CONVERS. DATE PD0003
      *  AMM 161031 WORLD PR#1200 ADDED BR-OPTION-PST-LASER
      *  AMM 181113 SDP-002-T02 REMOVED CREDIT BUREAU FIELDS
      *  BAH 20190702 REMOVED BR-USE-PARTIC-FLAT-TABLE PT0051
      *  BAH 20211116 LIMIT ACCESS BY PASSWORD TO ALL FIELDS, #1524
      *  BAH 20230628 ADDED B2-USE-NEW-WFARGO-ACCT #1616
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
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)GB/BRMAN7.CBL S35 09/20/22-13:12:30 >".
      ******************************************************************
       77  SCCS-IDENTIFIER PIC X(55) VALUE
           "@(#)GB/BRMAN7 A15 03/12/10-15:35:55 11/30/11-15:36:15 >".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".

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

      *----------------------------------------------------------------

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

       01  TEST-PAY-GROUP            PIC X(3).
       01  FILLER REDEFINES TEST-PAY-GROUP.
           03  TEST-PAY-GROUP-CHAR   PIC X OCCURS 3.

       01  BRMAIN-LINK       VALUE SPACES.
           03  FORM-NAME     PIC X(20).
           03  SAVEKEY       PIC X(4).
           03  NEW-REC       PIC X.
           03  ERRCD         PIC X.
           03  FORMNUM       PIC 9.
           03  BRMAIN-WINDOW-HANDLE PIC X(10).

      ******************************************************************
      *    S C R E E N    D I S P L A Y   B U F F E R
      ******************************************************************
       01  DISPLAY-BUF          VALUE SPACES.
           03  F01              PIC X(01).
           03  F02              PIC X(01).
           03  F03              PIC X(01).
           03  F04              PIC X(01).
           03  F05              PIC X(01).
           03  F06              PIC X(01).
           03  F07              PIC X(01).
           03  F08              PIC X(01).
           03  F09              PIC X(01).
           03  F10              PIC X(01).
           03  F11              PIC X(01).
           03  F12              PIC X(01).
           03  F13              PIC X(01).
           03  F14              PIC X(01).
           03  F15              PIC X(01).
           03  F16              PIC X(01).
           03  F17              PIC ZZZ9.99.
           03  F171             PIC ZZZ9.99.
           03  F18              PIC X(01).
           03  F19              PIC X(01).
           03  F20              PIC X(01).
           03  F21              PIC X(01).
           03  F22              PIC X(01).
           03  F23              PIC X(01).
           03  F24              PIC X(01).
           03  F25              PIC X(01).
           03  F26              PIC X(03).
           03  F27              PIC X(01).
           03  F28              PIC X(01).
           03  F29              PIC X(01).
           03  F30              PIC X(01).
           03  F31              PIC Z(08).
           03  F32              PIC X(8).
           03  F33              PIC X(02).
           03  F34              PIC X(02).
           03  F35              PIC X(02).

       01  DISPLAY-LIST.
           03  FILLER      PIC X(40)                      VALUE
               "010 020 030 040 050 060 070 080 090 100 ".
           03  FILLER      PIC X(44)                      VALUE
               "110 120 130 140 150 160 170 171 180 190 200 ".
           03  FILLER      PIC X(40)                      VALUE
               "210 220 230 240 250 260 270 280 290 300 ".
           03  FILLER      PIC X(20)                      VALUE
               "310 320 330 340 350.".

      ******************************************************************
      *       ELEMENT SPECIFICATIONS #1
      ******************************************************************
       01  MAX                            PIC 99       VALUE 35.
       01  SPEC-TABLE.
      * 01.  NOT-IN-USE     
           03  FILLER                     PIC X(8)     VALUE "E  A010 ".
      * 02.  NOT-IN-USE     
           03  FILLER                     PIC X(8)     VALUE "E  A010 ".
      * 03.  NOT-IN-USE      
           03  FILLER                     PIC X(8)     VALUE "E  A010 ".
      * 04.  NOT-IN-USE          
           03  FILLER                     PIC X(8)     VALUE "E  A010 ".
      * 05.  NOT-IN-USE
           03  FILLER                     PIC X(8)     VALUE "E  A010 ".
      * 06.  BR-LR-GROUP-COLLID
           03  FILLER                     PIC X(8)     VALUE "E  A010 ".
      * 07.  NOT-IN-USE
           03  FILLER                     PIC X(8)     VALUE "E  A010 ".
      * 08.  BR-IMMEDIATE-RECEIPT
           03  FILLER                     PIC X(8)     VALUE "E  A010 ".
      * 09.  BR-IMMEDIATE-DOC-PRINT
           03  FILLER                     PIC X(8)     VALUE "E  A010 ".
      * 10.  BR-AUTO-PL-REF-AC
           03  FILLER                     PIC X(8)     VALUE "E  A010 ".
      * 11.  BR-ALLOW-SYS-AS-EFFDATE
           03  FILLER                     PIC X(8)     VALUE "E NA010 ".
      * 12.  BR-LI-LISCAN-FG
           03  FILLER                     PIC X(8)     VALUE "E  A010 ".
      * 13.  BR-LBOX
           03  FILLER                     PIC X(8)     VALUE "E  A010 ".
      * 14.  BR-ALLOW-OV-TRCD
           03  FILLER                     PIC X(8)     VALUE "E  A010 ".
      * 15.  BR-ALLOW-INTERBR
           03  FILLER                     PIC X(8)     VALUE "E  A010 ".
      * 16.  BR-COL-QUEUE-PAST-DUE
           03  FILLER                     PIC X(8)     VALUE "E  A010 ".
      * 17.  BR-LI-DLCOMM-CMSI-PP-MAX
           03  FILLER                     PIC X(8)     VALUE "ENNN042 ".
      * 18.  BR-ASSESS-LATE-CHGS
           03  FILLER                     PIC X(8)     VALUE "E  A010 ".
      * 19.  BR-JE-ALLOCATION-SKIP
           03  FILLER                     PIC X(8)     VALUE "E  A010 ".
      * 20.  BR-WORLD-BRANCH-TYPE
           03  FILLER                     PIC X(8)     VALUE "E  A010 ".
      * 21.  BR-COL-QUEUE-BEHAVIOR-FG
           03  FILLER                     PIC X(8)     VALUE "E  A010 ".
      * 22.  BR-COUPON-STATEMENT
           03  FILLER                     PIC X(8)     VALUE "E  A010 ".
      * 23. N/A
           03  FILLER                     PIC X(8)     VALUE "E  A010N".
      * 24.  BR-SKIP-MIDSTREAM-TRW
           03  FILLER                     PIC X(8)     VALUE "E  A010 ".
      * 25.  BR-ALLOW-FORCE-OPEN-FG
           03  FILLER                     PIC X(8)     VALUE "E  A010 ".
      * 26.  BR-ADP-PAY-GROUP
           03  FILLER                     PIC X(8)     VALUE "E  A030 ".
      * 27.  BR-OPTION-PST-LASER
           03  FILLER                     PIC X(8)     VALUE "E  A010 ".
      * 28.  BR-USE-NEW-WFARGO-ACCT      
           03  FILLER                     PIC X(8)     VALUE "E  A010 ".
      * 29.  NOT-IN-USE
           03  FILLER                     PIC X(8)     VALUE "E  A010 ".
      * 30.  NOT-IN-USE
           03  FILLER                     PIC X(8)     VALUE "E  A010 ".
      * 31.  BR-INS-PRODUCER (REDEFINITION OF BR-STMNT2)
           03  FILLER                     PIC X(8)     VALUE "ENNN080 ".
      * 32.  NOT-IN-USE
           03  FILLER                     PIC X(8)     VALUE "E  A080N".
      * 33.  BR-ADD-INS-FEECODE
           03  FILLER                     PIC X(8)     VALUE "E  A020 ".
      * 34.  BR-ROAD-G-SGL-FEECODE
           03  FILLER                     PIC X(8)     VALUE "E  A020 ".
      * 35.  BR-ROAD-FAM-FEECODE
           03  FILLER                     PIC X(8)     VALUE "E  A020 ".

      *-----------------------------------------------------------------
       01  SPEC-TAB REDEFINES SPEC-TABLE.
           03  SPEC-REC OCCURS 35 TIMES.
               05  SPEC PIC X(7).
               05  CHFG PIC X.

       COPY "LIBGB/PASSWDW.CPY".
       78  FIELDS-SEQ                VALUE 0.
       78  28-SEQ                    VALUE 1.
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBGB/BRMAN7_DEF.CPY".
       COPY "LIBGB/BRMAN7_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       01 FORM-LINK-AREA    PIC X(2000).
       SCREEN SECTION.
       COPY "LIBGB/BRMAN7_SCN.CPY".

      ******************************************************************
      *
      *    P R O C E D U R E    D I V I S I O N
      *
      ******************************************************************
       PROCEDURE DIVISION
             USING FORM-PATHNAME FORM-LINK-AREA EXIT-PATHNAME.

      ******************************************************************
      *
      *    M A I N   M O D U L E
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.
           PERFORM SQL-CONNECT.

           PERFORM GET-GPENV.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "BRMAN7" TO VDU-FORM-NAME.

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
              MOVE "N" TO NEW-REC
              MOVE "P" TO FORM-P
              MOVE 1 TO FORM-SEL.

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

      *----------------------------------------------------------------
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

       END-REC-BR-MODULE.
           EXIT.

      ******************************************************************
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
           MOVE 0 TO ELE.
      *----------------------------------------------------------------
       CREATE-ROUTINE.
           IF NEW-FLAG NOT = 1
              GO TO MODIFY-ROUTINE.

           ADD ONE TO ELE.

           IF ELE < 1
              MOVE 1 TO ELE ONE.

           IF ELE NOT > MAX
              GO TO ENTER-ELE.

           MOVE 2 TO NEW-FLAG.

      *----------------------------------------------------------------
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
              IF NOT (FORM-SEL = 1 OR 2 OR 3 OR 4 OR 5 OR 6)
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

      * LIMIT ACCESS TO ALL FIELDS, EXCEPT 28 WHICH HAS ANOTHER PASSWORD
           IF NEW-FLAG = 0
              IF ELE NOT = 28
                 MOVE FIELDS-SEQ                    TO PASSWDW-SEQ
                 MOVE "TO CHANGE THIS FIELD......." TO PASSWDW-PROMPT2
                 PERFORM VERIFY-PASSWORD
                 IF PASSWDW-STAT NOT = "OK"
                    GO TO MODIFY-ROUTINE.

      *----------------------------------------------------------------
       ENTER-ELE.

      * THEY WANT TO ALLOW A "D" LOGIN TO ACCESS FIELD 28
           IF ELE = 28
              IF NEW-FLAG = 0
                 MOVE 28-SEQ                        TO PASSWDW-SEQ
                 MOVE "TO CHANGE THIS FIELD......." TO PASSWDW-PROMPT2
                 PERFORM VERIFY-PASSWORD
                 IF PASSWDW-STAT NOT = "OK"
                    GO TO MODIFY-ROUTINE.

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

      *----------------------------------------------------------------
       TEST-F4.
           IF UP-KEY
             IF NEW-FLAG = 1
              MOVE -1 TO ONE
              GO TO CREATE-ROUTINE.

      *----------------------------------------------------------------
       TEST-F5.
           IF DOWN-KEY
              MOVE 1 TO ONE
              GO TO GOTO-FIELD1.

      *----------------------------------------------------------------
       TEST-F1.
           IF F1-KEY
              MOVE "CLR" TO IO-TYPE
              MOVE "N" TO NEW-REC
              GO TO END-ENTRY-MODULE.

           IF NOT ENTER-KEY
              GO TO ENTER-ELE.

           MOVE 1 TO ONE.

      *----------------------------------------------------------------
       GOTO-FIELD1.
           GO TO F-01 F-02 F-03 F-04 F-05 F-06 F-07 F-08 F-09 F-10
                 F-11 F-12 F-13 F-14 F-15 F-16 F-17 F-18 F-19 F-20
                 F-21 F-22 F-23 F-24 F-25 F-26 F-27 F-28 F-29 F-30
                 F-31 F-32 F-33 F-34 F-35
                 DEPENDING ON ELE.
           GO TO END-ENTRY-MODULE.

      *****************************************************************
      *    S E T   R E C O R D   E L E M E N T S
      *****************************************************************
       F-01.
           GO TO CREATE-ROUTINE.

      *----------------------------------------------------------------
       F-02.
           GO TO CREATE-ROUTINE.

      *----------------------------------------------------------------
       F-03.
           GO TO CREATE-ROUTINE.

      *----------------------------------------------------------------
       F-04.
           GO TO CREATE-ROUTINE.

      *----------------------------------------------------------------
       F-05.
           GO TO CREATE-ROUTINE.

      *----------------------------------------------------------------
       F-06.

           MOVE VDUBUF TO BR-LR-GROUP-COLLID.
           GO TO CREATE-ROUTINE.

      *----------------------------------------------------------------
       F-07.
           MOVE VDUBUF TO BR-CONVERT-AUTO-ACCTNO.

           IF ( NOT BR-CONVERT-AUTO-ACCTNO = "N" ) AND
              ( NOT BR-CONVERT-AUTO-ACCTNO = "Y" )
              MOVE "INVALID: 'Y' OR 'N' ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

      *----------------------------------------------------------------
       F-08.
           MOVE VDUBUF TO BR-IMMEDIATE-RECEIPT.

           IF ( NOT BR-IMMEDIATE-RECEIPT = "N" ) AND
              ( NOT BR-IMMEDIATE-RECEIPT = "Y" )
              MOVE "INVALID: 'Y' OR 'N' ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

      *----------------------------------------------------------------
       F-09.
           MOVE VDUBUF TO BR-IMMEDIATE-DOC-PRINT.

           IF ( NOT BR-IMMEDIATE-DOC-PRINT = "N" ) AND
              ( NOT BR-IMMEDIATE-DOC-PRINT = "Y" )
              MOVE "INVALID: 'Y' OR 'N' ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

      *----------------------------------------------------------------
       F-10.
           MOVE VDUBUF TO BR-AUTO-PL-REF-AC.
           IF BR-AUTO-PL-REF-AC NOT = "Y" AND
                                 NOT = "N" AND
                                  NOT = "2" AND
                                   NOT = "3"
              MOVE "INVALID: 'Y', 'N', '2' OR '3' ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

      *----------------------------------------------------------------
       F-11.
           MOVE VDUBUF TO BR-ALLOW-SYS-AS-EFFDATE.

           IF NOT (BR-ALLOW-SYS-AS-EFFDATE = "N" OR "Y")
              MOVE "INVALID: 'Y' OR 'N' ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.

      *----------------------------------------------------------------
       F-12.
           MOVE VDUBUF TO BR-LI-LISCAN-FG.

           IF ( GP-LI-APPLICATION-FG NOT = "Y" ) AND
              ( BR-LI-LISCAN-FG      NOT = "N" )
              MOVE "INVALID: 'N' ONLY (GP)" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           IF NOT ( BR-LI-LISCAN-FG = "Y" OR "N" )
              MOVE "INVALID: 'Y' OR 'N' ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

      *----------------------------------------------------------------
       F-13.
           MOVE VDUBUF TO BR-LBOX.
           IF NOT ( BR-LBOX = "Y" OR "N" )
              MOVE "INVALID: 'Y' OR 'N' ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.

      *----------------------------------------------------------------
       F-14.
           MOVE VDUBUF TO BR-ALLOW-OV-TRCD.
           IF NOT ( BR-ALLOW-OV-TRCD = "Y" OR "N" )
              MOVE "INVALID: 'Y' OR 'N' ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.

      *----------------------------------------------------------------
       F-15.
           MOVE VDUBUF TO BR-ALLOW-INTERBR.
           IF NOT ( BR-ALLOW-INTERBR = "Y" OR "N" )
              MOVE "INVALID: 'Y' OR 'N' ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.

      *----------------------------------------------------------------
       F-16.
           MOVE VDUBUF TO BR-COL-QUEUE-PAST-DUE.
           IF NOT ( BR-COL-QUEUE-PAST-DUE = "Y" OR "N" )
              MOVE "INVALID: 'Y' OR 'N' ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.

      *----------------------------------------------------------------
       F-17.
           MOVE VDUNUM2 TO BR-LI-DLCOMM-CMSI-PP-MAX.

      *----------------------------------------------------------------
       F-171.
           MOVE "ENNN042171." TO VDU.
           PERFORM SCREENIO.
           IF UP-KEY
              GO TO ENTER-ELE.
           IF NOT ENTER-KEY
              GO TO F-171.

           MOVE VDUNUM2      TO BR-LI-DLCOMM-CMSI-O3-MAX.
           GO TO CREATE-ROUTINE.
      *----------------------------------------------------------------
       F-18.
           MOVE VDUBUF TO BR-ASSESS-LATE-CHGS.
           IF NOT ( BR-ASSESS-LATE-CHGS = "Y" OR "N" )
              MOVE "INVALID: 'Y' OR 'N' ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.

      *----------------------------------------------------------------
       F-19.
           MOVE VDUBUF TO BR-JE-ALLOCATION-SKIP.
           IF NOT ( BR-JE-ALLOCATION-SKIP = "Y" OR "N" )
              MOVE "INVALID: 'Y' OR 'N' ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.

      *----------------------------------------------------------------
       F-20.
           MOVE VDUBUF TO BR-WORLD-BRANCH-TYPE.
           IF NOT (BR-WORLD-BRANCH-TYPE =  " " OR "A" OR "1" OR "2")
              MOVE "ENTER ' ' OR 'A' OR '1' OR '2'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

      *----------------------------------------------------------------
       F-21.
           MOVE VDUBUF TO BR-COL-QUEUE-BEHAVIOR-FG.
           IF NOT ( BR-COL-QUEUE-BEHAVIOR-FG = "Y" OR "N" )
              MOVE "INVALID: 'Y' OR 'N' ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.

      *----------------------------------------------------------------
       F-22.
           MOVE VDUBUF TO BR-COUPON-STATEMENT.
           IF NOT ( BR-COUPON-STATEMENT = "C" OR "S" OR " ")
              MOVE "INVALID: 'C' OR 'S' OR ' ' ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.

      *----------------------------------------------------------------
       F-23.
           GO TO CREATE-ROUTINE.

      *----------------------------------------------------------------
       F-24.
           MOVE VDUBUF TO BR-SKIP-MIDSTREAM-TRW.
           IF NOT (BR-SKIP-MIDSTREAM-TRW = "Y" OR "N")
              MOVE "INVALID: 'Y' OR 'N' ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

      *----------------------------------------------------------------
       F-25.
           MOVE VDUBUF TO BR-ALLOW-FORCE-OPEN-FG
           IF NOT (BR-ALLOW-FORCE-OPEN-FG = "Y" OR "N")
              MOVE "INVALID: 'Y' OR 'N' ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

      *----------------------------------------------------------------
       F-26.
           MOVE VDUBUF TO BR-ADP-PAY-GROUP TEST-PAY-GROUP.
           IF (TEST-PAY-GROUP-CHAR(1) = " "
              OR TEST-PAY-GROUP-CHAR(2) = " "
                 OR TEST-PAY-GROUP-CHAR(3) = " ")
                    MOVE "PLEASE ENTER 3 CHARACTERS FOR PAY GROUP"
                                                         TO MESS
                    PERFORM SEND-MESS.
           GO TO CREATE-ROUTINE.

      *----------------------------------------------------------------
       F-27.
           MOVE VDUBUF TO BR-OPTION-PST-LASER
           IF NOT (BR-OPTION-PST-LASER = "Y" OR "N")
              MOVE "INVALID: 'Y' OR 'N' ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

      *----------------------------------------------------------------
       F-28.
           MOVE VDUBUF TO BR-USE-NEW-WFARGO-ACCT.
           IF NOT (BR-USE-NEW-WFARGO-ACCT = "Y" OR "N")
              MOVE "INVALID: 'Y' OR 'N' ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.

      *----------------------------------------------------------------
       F-29.
           GO TO CREATE-ROUTINE.

      *----------------------------------------------------------------
       F-30.
           GO TO CREATE-ROUTINE.

      *----------------------------------------------------------------
       F-31.
           MOVE VDUNUM0 TO BR-INS-PRODUCER-NUM.
           GO TO CREATE-ROUTINE.

      *----------------------------------------------------------------
       F-32.
           GO TO CREATE-ROUTINE.

      *----------------------------------------------------------------
       F-33.
           MOVE VDUBUF TO BR-ADD-INS-FEECODE.

           GO TO CREATE-ROUTINE.

      *----------------------------------------------------------------
       F-34.
           MOVE VDUBUF TO BR-ROAD-G-SGL-FEECODE.

           GO TO CREATE-ROUTINE.

      *----------------------------------------------------------------
       F-35.
           MOVE VDUBUF TO BR-ROAD-G-FAM-FEECODE.

           GO TO CREATE-ROUTINE.

       END-ENTRY-MODULE.
           MOVE 0 TO FORM-SEL.

       EXIT-ENTRY-MODULE.
           EXIT.

      ******************************************************************
       VERIFY-PASSWORD SECTION.
      ******************************************************************
           MOVE " "                          TO PASSWDW-STAT.
           MOVE "BRMAN7"                     TO PASSWDW-PGM.
           MOVE "PLEASE ENTER THE PASSWORD," TO PASSWDW-PROMPT1.
           MOVE "CL/PASSWD"                  TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
           CANCEL FORM-PROGX.

       EXIT-VERIFY-PASSWORD.
           EXIT.

      ***************************
       WRITE-MODULE SECTION.
      ***************************

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

           IF BROR-LR-GROUP-COLLID NOT = BRCH-LR-GROUP-COLLID
             MOVE "06 "                   TO LMC-FIELD
             MOVE "COLL ID "              TO LMC-DESC
             MOVE BROR-LR-GROUP-COLLID    TO LMC-ORG-TEXT
             MOVE BRCH-LR-GROUP-COLLID    TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-CONVERT-AUTO-ACCTNO NOT = BRCH-CONVERT-AUTO-ACCTNO
             MOVE "07 "                    TO LMC-FIELD
             MOVE "AUTO ACCT"              TO LMC-DESC
             MOVE BROR-CONVERT-AUTO-ACCTNO TO LMC-ORG-TEXT
             MOVE BRCH-CONVERT-AUTO-ACCTNO TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-IMMEDIATE-RECEIPT NOT = BRCH-IMMEDIATE-RECEIPT
             MOVE "08 "                   TO LMC-FIELD
             MOVE "RCPT OVER"             TO LMC-DESC
             MOVE BROR-IMMEDIATE-RECEIPT  TO LMC-ORG-TEXT
             MOVE BRCH-IMMEDIATE-RECEIPT  TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-IMMEDIATE-DOC-PRINT NOT = BRCH-IMMEDIATE-DOC-PRINT
             MOVE "09 "                    TO LMC-FIELD
             MOVE "DOC PRINT"              TO LMC-DESC
             MOVE BROR-IMMEDIATE-DOC-PRINT TO LMC-ORG-TEXT
             MOVE BRCH-IMMEDIATE-DOC-PRINT TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-AUTO-PL-REF-AC NOT = BRCH-AUTO-PL-REF-AC
             MOVE "10 "                   TO LMC-FIELD
             MOVE "AUTO REF"              TO LMC-DESC
             MOVE BROR-AUTO-PL-REF-AC     TO LMC-ORG-TEXT
             MOVE BRCH-AUTO-PL-REF-AC     TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-ALLOW-SYS-AS-EFFDATE NOT = BRCH-ALLOW-SYS-AS-EFFDATE
             MOVE "11 "                     TO LMC-FIELD
             MOVE "SYS EFFDT"               TO LMC-DESC
             MOVE BROR-ALLOW-SYS-AS-EFFDATE TO LMC-ORG-TEXT
             MOVE BRCH-ALLOW-SYS-AS-EFFDATE TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-LI-LISCAN-FG NOT = BRCH-LI-LISCAN-FG
             MOVE "12 "                   TO LMC-FIELD
             MOVE "SCAN WIND"             TO LMC-DESC
             MOVE BROR-LI-LISCAN-FG       TO LMC-ORG-TEXT
             MOVE BRCH-LI-LISCAN-FG       TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-LBOX NOT = BRCH-LBOX
             MOVE "13 "                   TO LMC-FIELD
             MOVE "LOCK BOX"              TO LMC-DESC
             MOVE BROR-LBOX               TO LMC-ORG-TEXT
             MOVE BRCH-LBOX               TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-ALLOW-OV-TRCD NOT = BRCH-ALLOW-OV-TRCD
             MOVE "14 "                   TO LMC-FIELD
             MOVE "TRAN CODE"             TO LMC-DESC
             MOVE BROR-ALLOW-OV-TRCD      TO LMC-ORG-TEXT
             MOVE BRCH-ALLOW-OV-TRCD      TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-ALLOW-INTERBR NOT = BRCH-ALLOW-INTERBR
             MOVE "15 "                   TO LMC-FIELD
             MOVE "BR PYMNTS"             TO LMC-DESC
             MOVE BROR-ALLOW-INTERBR      TO LMC-ORG-TEXT
             MOVE BRCH-ALLOW-INTERBR      TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-COL-QUEUE-PAST-DUE NOT = BRCH-COL-QUEUE-PAST-DUE
             MOVE "16 "                   TO LMC-FIELD
             MOVE "PDUE DAYS"             TO LMC-DESC
             MOVE BROR-COL-QUEUE-PAST-DUE TO LMC-ORG-TEXT
             MOVE BRCH-COL-QUEUE-PAST-DUE TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-LI-DLCOMM-CMSI-PP-MAX NOT =
              BRCH-LI-DLCOMM-CMSI-PP-MAX
             MOVE "17 "                      TO LMC-FIELD
             MOVE "DL MAX PP"                TO LMC-DESC
             MOVE BROR-LI-DLCOMM-CMSI-PP-MAX TO LMC-ORG-DLNO
             MOVE BRCH-LI-DLCOMM-CMSI-PP-MAX TO LMC-CHG-DLNO
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-LI-DLCOMM-CMSI-O3-MAX NOT =
              BRCH-LI-DLCOMM-CMSI-O3-MAX
             MOVE "171"                      TO LMC-FIELD
             MOVE "DL MAX O3"                TO LMC-DESC
             MOVE BROR-LI-DLCOMM-CMSI-O3-MAX TO LMC-ORG-DLNO
             MOVE BRCH-LI-DLCOMM-CMSI-O3-MAX TO LMC-CHG-DLNO
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-ASSESS-LATE-CHGS NOT = BRCH-ASSESS-LATE-CHGS
             MOVE "18 "                   TO LMC-FIELD
             MOVE "LATECHGS"              TO LMC-DESC
             MOVE BROR-ASSESS-LATE-CHGS   TO LMC-ORG-TEXT
             MOVE BRCH-ASSESS-LATE-CHGS   TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-JE-ALLOCATION-SKIP NOT = BRCH-JE-ALLOCATION-SKIP
             MOVE "19 "                   TO LMC-FIELD
             MOVE "GL ENTRY "             TO LMC-DESC
             MOVE BROR-JE-ALLOCATION-SKIP TO LMC-ORG-TEXT
             MOVE BRCH-JE-ALLOCATION-SKIP TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-WORLD-BRANCH-TYPE NOT = BRCH-WORLD-BRANCH-TYPE
             MOVE "20 "                   TO LMC-FIELD
             MOVE "BRTYPE FG"             TO LMC-DESC
             MOVE BROR-WORLD-BRANCH-TYPE  TO LMC-ORG-TEXT
             MOVE BRCH-WORLD-BRANCH-TYPE  TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-COL-QUEUE-BEHAVIOR-FG NOT =
              BRCH-COL-QUEUE-BEHAVIOR-FG
             MOVE "21 "                      TO LMC-FIELD
             MOVE "BEHAV GRD"                TO LMC-DESC
             MOVE BROR-COL-QUEUE-BEHAVIOR-FG TO LMC-ORG-TEXT
             MOVE BRCH-COL-QUEUE-BEHAVIOR-FG TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-COUPON-STATEMENT NOT = BRCH-COUPON-STATEMENT
             MOVE "22 "                   TO LMC-FIELD
             MOVE "COUP STAT"             TO LMC-DESC
             MOVE BROR-COUPON-STATEMENT   TO LMC-ORG-TEXT
             MOVE BRCH-COUPON-STATEMENT   TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-SKIP-MIDSTREAM-TRW NOT = BRCH-SKIP-MIDSTREAM-TRW         
             MOVE "24 "                   TO LMC-FIELD
             MOVE "TRW MIDST"             TO LMC-DESC
             MOVE BROR-SKIP-MIDSTREAM-TRW TO LMC-ORG-TEXT
             MOVE BRCH-SKIP-MIDSTREAM-TRW TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-ALLOW-FORCE-OPEN-FG NOT = BRCH-ALLOW-FORCE-OPEN-FG
             MOVE "25 "                    TO LMC-FIELD
             MOVE "FORC OPEN"              TO LMC-DESC
             MOVE BROR-ALLOW-FORCE-OPEN-FG TO LMC-ORG-TEXT
             MOVE BRCH-ALLOW-FORCE-OPEN-FG TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-ADP-PAY-GROUP NOT = BRCH-ADP-PAY-GROUP
             MOVE "26 "                   TO LMC-FIELD
             MOVE "ADP GROUP"             TO LMC-DESC
             MOVE BROR-ADP-PAY-GROUP   TO LMC-ORG-TEXT
             MOVE BRCH-ADP-PAY-GROUP   TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-OPTION-PST-LASER NOT = BRCH-OPTION-PST-LASER
             MOVE "27 "                    TO LMC-FIELD
             MOVE "LASER FLG"              TO LMC-DESC
             MOVE BROR-OPTION-PST-LASER    TO LMC-ORG-TEXT
             MOVE BRCH-OPTION-PST-LASER    TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-INS-PRODUCER-NUM NOT = BRCH-INS-PRODUCER-NUM
             MOVE "31 "                   TO LMC-FIELD
             MOVE "INS PR CD"             TO LMC-DESC
             MOVE BROR-INS-PRODUCER-NUM   TO LMC-ORG-NUM
             MOVE BRCH-INS-PRODUCER-NUM   TO LMC-CHG-NUM
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-ADD-INS-FEECODE NOT = BRCH-ADD-INS-FEECODE
             MOVE "33 "                   TO LMC-FIELD
             MOVE "AD&D INS"              TO LMC-DESC
             MOVE BROR-ADD-INS-FEECODE    TO LMC-ORG-TEXT
             MOVE BRCH-ADD-INS-FEECODE    TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-ROAD-G-SGL-FEECODE NOT = BRCH-ROAD-G-SGL-FEECODE
             MOVE "34 "                   TO LMC-FIELD
             MOVE "SNGL ROAD"             TO LMC-DESC
             MOVE BROR-ROAD-G-SGL-FEECODE TO LMC-ORG-TEXT
             MOVE BRCH-ROAD-G-SGL-FEECODE TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-ROAD-G-FAM-FEECODE NOT = BRCH-ROAD-G-FAM-FEECODE
             MOVE "35 "                   TO LMC-FIELD
             MOVE "FAM ROAD"              TO LMC-DESC
             MOVE BROR-ROAD-G-FAM-FEECODE TO LMC-ORG-TEXT
             MOVE BRCH-ROAD-G-FAM-FEECODE TO LMC-CHG-TEXT
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
           MOVE SPACES                  TO DISPLAY-BUF.

           MOVE SPACES                  TO F01.
           MOVE SPACES                  TO F02.
           MOVE SPACES                  TO F03.
           MOVE SPACES                  TO F04.
           MOVE SPACES                  TO F05.
           MOVE BR-LR-GROUP-COLLID      TO F06.
           MOVE BR-CONVERT-AUTO-ACCTNO  TO F07.
           MOVE BR-IMMEDIATE-RECEIPT    TO F08.
           MOVE BR-IMMEDIATE-DOC-PRINT  TO F09.
           MOVE BR-AUTO-PL-REF-AC       TO F10.
           MOVE BR-ALLOW-SYS-AS-EFFDATE TO F11.
           MOVE BR-LI-LISCAN-FG         TO F12.
           MOVE BR-LBOX                 TO F13.
           MOVE BR-ALLOW-OV-TRCD        TO F14.
           MOVE BR-ALLOW-INTERBR        TO F15.
           MOVE BR-COL-QUEUE-PAST-DUE   TO F16.
           MOVE BR-LI-DLCOMM-CMSI-PP-MAX TO F17.
           MOVE BR-LI-DLCOMM-CMSI-O3-MAX TO F171.
           MOVE BR-ASSESS-LATE-CHGS      TO F18.
           MOVE BR-JE-ALLOCATION-SKIP    TO F19.
           MOVE BR-WORLD-BRANCH-TYPE     TO F20.
           MOVE BR-COL-QUEUE-BEHAVIOR-FG TO F21.
           MOVE BR-COUPON-STATEMENT      TO F22.
           MOVE SPACES                   TO F23.
           MOVE BR-SKIP-MIDSTREAM-TRW    TO F24.
           MOVE BR-ALLOW-FORCE-OPEN-FG   TO F25.
           MOVE BR-ADP-PAY-GROUP         TO F26.
           MOVE BR-OPTION-PST-LASER      TO F27.
           MOVE BR-USE-NEW-WFARGO-ACCT   TO F28.
           MOVE SPACES                   TO F29.
           MOVE SPACES                   TO F30.
           MOVE BR-INS-PRODUCER-NUM      TO F31.
           MOVE SPACES                   TO F32.

           MOVE BR-ADD-INS-FEECODE       TO F33.
           MOVE BR-ROAD-G-SGL-FEECODE    TO F34.
           MOVE BR-ROAD-G-FAM-FEECODE    TO F35.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       DISPLAY-FIELDS-EXIT.
           EXIT.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/DATER.CPY".

      **************************************************************************
       FORM-RESET SECTION.
      **************************************************************************
           MOVE EXT-CONAME-CONAME TO BRMAN7-CONAME.
           MOVE EXT-CONAME-SYSDATE TO BRMAN7-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY BRMAN7-SCREEN.
           DISPLAY BRMAN7-SCREEN UPON BRMAIN-WINDOW-HANDLE.
           MOVE BRMAN7-FORM-FIELDS TO WR-BUF.
           MOVE BRMAN7-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **************************************************************************
       SETUP-LINK-INFO SECTION.
      **************************************************************************
           MOVE BRMAN7-HELP--FILE TO HELP-LINK-FILE.
           MOVE BRMAN7-HELP--DIR TO HELP-LINK-DIR.

      **************************************************************************
       VDU-CONVERT-FIELD SECTION.
      **************************************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBGB/BRMAN7_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISC-PARAGRAPHS SECTION.
      ******************************************************************
       COPY "LIBGB/ACCESS.CPY".
       RECORD-DELETED.
           MOVE "RECORD WAS DELETED!" TO MESS.
           PERFORM SEND-MESS.
       RECORD-CHANGED.
           MOVE "CHANGES WERE MADE TO THIS RECORD!" TO MESS.
           PERFORM SEND-MESS.
       CHANGE-AGAIN.
           MOVE "PLEASE RE-INPUT YOUR CHANGES!" TO MESS.
           PERFORM SEND-MESS.
       UNDERGONE-CHANGE.
           MOVE "THIS RECORD HAS UNDERGONE CHANGE BY ANOTHER USER,"
                                                   TO MESS.
           PERFORM SEND-MESS.
       SEND-LEGEND.
           MOVE "S  A000MSEL." TO VDU.
           MOVE MESS TO VDUBUF.
           PERFORM SCREENIO.

      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-BR-FILE.
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRIN.CPY".

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      ******************************************************************
       END-ROUTINE SECTION.
      ******************************************************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY BRMAN7-SCREEN.
           MOVE FORM-PATH TO FORM-PATHNAME.
           MOVE BRMAIN-LINK TO FORM-LINK-AREA.
           EXIT PROGRAM.
