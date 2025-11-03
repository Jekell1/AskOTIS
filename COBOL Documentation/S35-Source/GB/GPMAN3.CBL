       IDENTIFICATION DIVISION.
       PROGRAM-ID.      GPMAN3.
       DATE-WRITTEN.    041699.
      ******************************************************************
      *  DIR : GB
      *  DESC: GLOBAL PARAMETER MAINT - PAGE 3
      *        GENERAL LEDGER (RELATIVE RECORD 1)
      *
      *REV:
      * MJD 990416 ADDED GP-DELINQ-WITH-O2
      *  CS 990416 ADDED GP-IB-ACCRUE-EARN-ON-REPO
      * BLV 990424 ADDED GP-ALLOW-REPO-ON-PL
      * BAH 990701 ADDED GP-BK-PRIOR-TO-POSTING, TFC PR# 26
      * MJD 990719 ADDED GP-AMT-CASH-PROCEEDS, REIONAL MANAGEMENT PR# 899
      * MJD 990730 ADDED GP-INCL-OTH-IN-EARNED, REGENCY PR# 48
      * BLV 990921 ADDED GP-DL-MEMO-TOGGLE, TFC PR# 40
      * MJD 991005 ADDED GP-RN-FOR10-LN, (REGMGM PR#942)
      * MG  991006 FIXED VDUSTATUS/IO-FG/STAT PER DEVELOPMENT MEETING.
      * MJD 000207 ADDED GP-OT-INS-TERM (TFC PR#48)
      * MJD 000322 ADDED GP-UNEARNED-STATS (MERC PR#223)
      * BLV 000524 ADDED GP-RECDEL-REVERSAL-FG
      * MJD 000608 ADDED GP-PL-SX-ONLY (TFC PR#77)
      * BLV 000712 RENAMED GP-PL-SX-ONLY TO GP-ALLOW-PL-ON-SX (TFC PR#77)
      * MJD 000818 ADDED GP-TIME-ZONE-FACTOR AND GP-TIME-ZONE-DLS (TFC PR#25)
      * MJD 001002 ADDED GP-RV-MEMO (WORLD AS PER JTG)
      * BAH 010305 ADDED GP-IGNORE-LCHG-NXDUE, WORLD PR# 254, TO IGNORE
      *            LCHARGES FOR TDUE/NXDUE AMT (WHEN PAID AHEAD DUE TO LCHG)
      * CS  010522 CHANGED GP-RV-MEMO TO ACCEPT Y,N,A THRU Z (REGIONAL 1480)
      * JTG 010911 ADDED GP-NO-GL-ON-O2-WHEN-PL, CHANGED ENTRY OF 'O2' ON P&L
      *            TO STOP UPDATE OF GL MERC #260
      * BLV 011128 ADDED GP-RESCHEDULE-MEMO, UNITED PR# 1661
      * JTG 011219 ADDED OPTION FOR 'SS' GP-DONT-COLL-INT-ON-SS  REGACC PR#1684
      * BAH 020319 ADDED GP-CREATE-TP-FORMS TO CREATE FORM REQUESTS WHEN
      *            POSTING A "TP", MIDATL PR# 1757
      * JTG 020905 ADDED LN-DLCOMM-03-CMSI LN-DLCOMM-PP-CMSI  REGACC #1846
      *            ADDED GP-DLCOMM-CMSI-FG
      *            REGACC #0006
      * JTG 021011 ADDED GP-BATCH-PAY-REVISIONS FOR NEW BATCH PAY REVISIONS
      * JTG 021202 DELETED GP-BATCH-PAY-REVISIONS FOR NEW BATCH PAY REVISIONS
      * BLV 030212 ADDED FIELD 23, GP-EARLIEST-CLOSE-TIME, REGACC PR# 0018
      * BAH 031022 ADDED 26. DEFAULT O3 INSURANCE OURS FLAG, LENDMARK PR# 2180
      *            GP-DEFAULT-O3-INSOURS
      * BAH 031114 ADDED 27. GP-USE-BW-BAD-FLAGS FOR THE REGACC FIELDS, 2155
      * BAH 031119 ADDED 28. GP-NON-FILE-FEECODE FOR WORLD SMITH PROJECT
      * BAH 031120 ADDED 29. GP-CHBORR-PAST-DUE-DAYS FOR WORLD #337
      * BAH 040311 ADDED 30. GP-BACKDATE-MEMO FOR MEMO WINDOW TO POP OPEN
      *            WHEN BACKDATING PAYMENTS AND PAYOFFS,  WORLD PR# 344
      * JTG 040519 ADDED GP-NEW-REPO-UNEARNED-ACCTS
      *            NEW GL CONTROL FIELDS FOR:
      *                - UNEARNED INTEREST REPO
      *                - UNEARNED ADDON INTEREST REPO
      *            THE UNEARNED WILL MOVE TO THESE ACCOUNTS POSTING A 'SX'
      *            AND REVERSING A 'SV'
      *            THE UNEARNED WILL MOVE (BACK) FROM THESE ACCOUNTS POSTING
      *            A 'SV' AND REVERSING A 'SX'                    REGACC #0049
      * BAH 040824 ADDED GP-BK-FLAG-CONFIDENTIAL TO AUTOMATICALLY FLAG LOAN AS
      *            CONFIDENTIAL WHEN "BK" POSTED, AND FORCE BACK TO "N" WHEN
      *            REVERSED OR "BD" POSTED, LENDMARK PR# 2364
      * JTG 041014 ADDED GP-AMORTIZE-ACQUIST-FEE, THIS OPTION WILL TAKE THE
      *            AMOUNT ENTERED IN RESERVE PRINCIPAL 3 AND MOVE IT TO
      *            THE LOANS UNEARNED SERVICE CHARGE               REGACC #0081
      * BAH 050119 ADDED GP-DENY-FORM-NO-ADDRESS TO DENY ANY FORM REQUESTS IF
      *            THE BORROWER HAS NO ADR1, CITY, STATE OR ZIP, REGACC PR# 94
      * BAH 050713 ADDED GP-HIDE-FIRST-5-SSNO, REGACC PR# 109
      * BAH 050914 ADDED GP-DEFERMENT-APPROVAL, LENDMARK PR# 2568
      *  CS 081104 ADDED 'R' TO GP-INCL-OTH-IN-EARNED. WHEN 'R' NEITHER OTHER
      *            (ANTIC(6)) OR MAINTFEE(ANTIC(4)) WILL BE INCLUDED IN
      *            EARNED PR#210 REGACC
      *  CS 081121 GP-INCL-OTH-IN-EARNED-R IS COMPANY SPECIFIC TO REGACC AND
      *            NO ONE ELSE SHOULD USE, SO I TIED THIS FLAG THAT YOU MUST
      *            BE RUNNING AS REGACC PR#210
      * BAH 150622 ALLOW GP-BK-FLAG-CONFIDENTIAL TO BE "L" TO SET
      *            CONFIDENTIAL TO "Y" IF LEGAL DATE IS NOT 0, AND ALSO DO
      *            THE BANKRUPT TEST, NSA PR#4780
      * BAH 180515 ADDED ERRLOG
      * KEC 2019-0123 A32 - GPFILE GP-REC MODIFICATION
      *               ADDED PAGE 5 (GB/GPMAN5) FOR ACCOUNTS PAYABLE (AP)
      **************************************************************************
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
      *    W O R K I N G - S T O R A G E     S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)GB/GPMAN3.CBL S35 02/22/24-14:43:31 >".
      ******************************************************************
      *
      *     P R O G R A M - I F N
      *
      ******************************************************************
       COPY "LIBGB/GB01GP.CPY".
       COPY "LIBGB/GB01GP_SQL.CPY".
       COPY "LIBGB/GBWSGP.CPY".
       COPY "LIBGL/GL01GL.CPY".
       COPY "LIBGL/GL01GL_SQL.CPY".
       COPY "LIBGL/GLWSGL.CPY".
       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LP01CD_SQL.CPY".
       COPY "LIBLP/LPWSCD.CPY".

      ******************************************************************
      *    P R O G R A M     W O R K E R S
      ******************************************************************
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
       01  FIRST-TIME      PIC 9       VALUE 0.

      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    ORIGINAL RECORD GPOR-REC
      *    CHANGED RECORD GPCH-REC
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       COPY "LIBGB/GB01GP.CPY" REPLACING LEADING "GP" BY "GPOR".
       COPY "LIBGB/GB01GP.CPY" REPLACING LEADING "GP" BY "GPCH".

      ******************************************************************
      *    DESC: LOG-MAINTENANCE-CHGS WORKERS
      ******************************************************************
       01  LMC-DETAIL                  PIC X(55)      VALUE SPACES.
       01  FILLER                      REDEFINES LMC-DETAIL.
           03  LMC-FIELD               PIC X(03).
           03  FILLER                  PIC X(01).
           03  LMC-DESC                PIC X(14).
           03  FILLER                  PIC X(01).

           03  LMC-ORG-TEXT            PIC X(16).
           03  LMC-ORG-AMT             REDEFINES LMC-ORG-TEXT
                                       PIC ZZZ,ZZZ,ZZ9.99-.
           03  LMC-ORG-NUM             REDEFINES LMC-ORG-TEXT
                                       PIC ZZZZZZZZZ9-.

           03  LMC-TO-X                PIC X(04).

           03  LMC-CHG-TEXT            PIC X(16).
           03  LMC-CHG-AMT             REDEFINES LMC-CHG-TEXT
                                       PIC ZZZ,ZZZ,ZZ9.99-.
           03  LMC-CHG-NUM             REDEFINES LMC-CHG-TEXT
                                       PIC ZZZZZZZZZ9-.

       01  LOWER-A         PIC X    VALUE X'61'.
       01  LOWER-Z         PIC X    VALUE X'7A'.
      ******************************************************************
      *    C O N A M E
      ******************************************************************
       01 GPMAIN-LINK.
          03 SAVEKEY              PIC 99.
          03 NEW-REC              PIC X.
          03 ERRCD                PIC X.
          03 FORMNUM              PIC 9.
          03 GPMAIN-WINDOW-HANDLE PIC X(10).


      ******************************************************************
      *    S C R E E N    D I S P L A Y    B U F F E R
      ******************************************************************
       01  DISPLAY-BUF     VALUE SPACES.
           03  F01         PIC X(01).
           03  F02         PIC X(01).
           03  F03         PIC X(01).
           03  F04         PIC X(01).
           03  F05         PIC Z(05)9.9(02).
           03  F06         PIC X(01).
           03  F07         PIC X(01).
           03  F08         PIC X(01).
           03  F09         PIC X(01).
           03  F10         PIC X(01).
           03  F11         PIC X(01).
           03  F12         PIC X(01).
           03  F13         PIC X(01).
           03  F14         PIC Z9-.
           03  F15         PIC X(01).
           03  F16         PIC X(01).
           03  F17         PIC X(01).
           03  F18         PIC X(01).
           03  F19         PIC X(01).
           03  F20         PIC X(01).
           03  F21         PIC X(01).
           03  F22         PIC X(01).
           03  F23         PIC 9(04).
           03  F24         PIC X(01).
           03  F25         PIC X(01).
           03  F26         PIC X(01).
           03  F27         PIC X(01).
           03  F28         PIC X(02).
           03  F29         PIC 9(02).
           03  F30         PIC X(01).
           03  F31         PIC X(01).
           03  F32         PIC X(01).
           03  F33         PIC X(01).
           03  F34         PIC X(01).
           03  F35         PIC X(01).
           03  F36         PIC X(01).
      *-----------------------------------------------------------------
       01  DISPLAY-LIST.
           03  FILLER      PIC X(28) VALUE
           "010 020 030 040 050 060 070 ".
           03  FILLER      PIC X(32) VALUE
           "080 090 100 110 120 130 140 150 ".
           03  FILLER      PIC X(32) VALUE
           "160 170 180 190 200 210 220 230 ".
           03  FILLER      PIC X(28) VALUE
           "240 250 260 270 280 290 300 ".
           03  FILLER      PIC X(24) VALUE
           "310 320 330 340 350 360.".


      ******************************************************************
      *    E L E M E N T    S P E C I F I C A T I O N S
      ******************************************************************
       78  MAX                      VALUE 36.
       01  SPEC-TABLE.
      * 01. ACCRUAL EARNINGS ON ACCOUNTS IN REPO STATUS
           03  FILLER      PIC X(8)    VALUE "E NA010 ".
      * 02. DELINQUENCY WITH O2 INCLUDED
           03  FILLER      PIC X(8)    VALUE "E  A010 ".
      * 03. ALLOW REPO TRANSACTIONS ON PL ACCOUNT
           03  FILLER      PIC X(8)    VALUE "E  A010 ".
      * 04. ACCESS BK STATUS SCREEN PRIOR TO BK POSTING
           03  FILLER      PIC X(8)    VALUE "E  A010 ".
      * 05. AMT CASH PROCEEDS
           03  FILLER      PIC X(8)    VALUE "ENNN062 ".
      * 06. GP-INCL-OTH-IN-EARNINGS
           03  FILLER      PIC X(8)    VALUE "E  A010 ".
      * 07. GP-DL-MEMO-TOGGLE
           03  FILLER      PIC X(8)    VALUE "E  A010 ".
      * 08. GP-RN-FOR10-LN
           03  FILLER      PIC X(8)    VALUE "E  A010 ".
      * 09. GP-OT-INS-TERM
           03  FILLER      PIC X(8)    VALUE "E  A010 ".
      * 10. GP-UNEARNED-STATS
           03  FILLER      PIC X(8)    VALUE "E  A010 ".
      * 11. GP-RECDEL-REVERSAL-FG
           03  FILLER      PIC X(8)    VALUE "E  A010 ".
      * 12. GP-ALLOW-PL-ON-SX
           03  FILLER      PIC X(8)    VALUE "ENNA010 ".
      * 13. GP-CPCORP-CURBAL
           03  FILLER      PIC X(8)    VALUE "ENNA010 ".
      * 14. GP-TIME-ZONE-FACTOR
           03  FILLER      PIC X(8)    VALUE "ENNS020 ".
      * 15. GP-TIME-ZONE-DLS
           03  FILLER      PIC X(8)    VALUE "ENNA010 ".
      * 16. GP-RV-MEMO
           03  FILLER      PIC X(8)    VALUE "ENNA010 ".
      * 17. GP-IGNORE-LCHG-NXDUE
           03  FILLER      PIC X(8)    VALUE "ENNA010 ".
      * 18. GP-NO-GL-ON-O2-WHEN-PL
           03  FILLER      PIC X(8)    VALUE "ENNA010 ".
      * 19. GP-RESCHEDULE-MEMO
           03  FILLER      PIC X(8)    VALUE "E  A010 ".
      * 20. GP-DONT-COLL-INT-ON-SS
           03  FILLER      PIC X(8)    VALUE "E  A010 ".
      * 21. GP-CREATE-TP-FORMS
           03  FILLER      PIC X(8)    VALUE "E  A010 ".
      * 22. GP-DLCOMM-CMSI-FG
           03  FILLER      PIC X(8)    VALUE "ENNA010 ".
      * 23. GP-EARLIEST-CLOSE-TIME
           03  FILLER      PIC X(8)    VALUE "ENNN040 ".
      * 24. GP-GROUP-APPROVAL-FG
           03  FILLER      PIC X(8)    VALUE "ENNA010 ".
      * 25. GP-APPROVAL-MEMO
           03  FILLER      PIC X(8)    VALUE "ENNA010 ".
      * 26. GP-DEFAULT-O3-INSOURS
           03  FILLER      PIC X(8)    VALUE "E  A010 ".
      * 27. GP-USE-BW-BAD-FLAGS
           03  FILLER      PIC X(8)    VALUE "E  A010 ".
      * 28. GP-NON-FILE-FEECODE
           03  FILLER      PIC X(8)    VALUE "E  A020 ".
      * 29. GP-CHBORR-PAST-DUE-DAYS
           03  FILLER      PIC X(8)    VALUE "EN N020 ".
      * 30. GP-BACKDATE-MEMO
           03  FILLER      PIC X(8)    VALUE "E  A010 ".
      * 31. GP-NEW-REPO-UNEARNED-ACCTS
           03  FILLER      PIC X(8)    VALUE "E  A010 ".
      * 32. GP-BK-FLAG-CONFIDENTIAL
           03  FILLER      PIC X(8)    VALUE "E  A010".
      * 33. GP-AMORTIZE-ACQUIST-FEE
           03  FILLER      PIC X(8)    VALUE "E  A010 ".
      * 34. GP-DENY-FORM-NO-ADDRESS
           03  FILLER      PIC X(8)    VALUE "E  A010 ".
      * 35. GP-HIDE-FIRST-5-SSNO
           03  FILLER      PIC X(8)    VALUE "E  A010 ".
      * 36. N/A
           03  FILLER      PIC X(8)    VALUE "E  A010 ".

       01  SPEC-TABLE2     REDEFINES SPEC-TABLE.
           02  SPEC-REC    OCCURS MAX TIMES.
               05  SPEC    PIC X(7).
               05  CHFG    PIC X.

       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBGB/GPMAN3_DEF.CPY".
       COPY "LIBGB/GPMAN3_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/ACCESSW.CPY".

       COPY "LIBGB/GETFMW.CPY".
       01 FORM-LINK-AREA    PIC X(2000).
       SCREEN SECTION.
       COPY "LIBGB/GPMAN3_SCN.CPY".

      ******************************************************************
      *     P R O C E D U R E    D I V I S I O N
      ******************************************************************
       PROCEDURE DIVISION
            USING FORM-PATHNAME FORM-LINK-AREA EXIT-PATHNAME.

      ******************************************************************
       MAIN-PROGRAM SECTION.
      ******************************************************************
           PERFORM SQL-CONNECT.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "GPMAN3" TO VDU-FORM-NAME.
           MOVE FORM-LINK-AREA TO GPMAIN-LINK.
       MAIN-MODULE.
           PERFORM FORM-RESET.
           IF FIRST-TIME = 0
              MOVE 1 TO FIRST-TIME.

           PERFORM REC-GP-MODULE.
           IF IO-TYPE = "END"
              MOVE "X" TO ERRCD
              GO TO END-ROUTINE.

           PERFORM ENTRY-MODULE.
           IF IO-TYPE = "END"
              MOVE "X" TO ERRCD
              GO TO END-ROUTINE.
           IF IO-TYPE = "RWR"
              PERFORM WRITE-MODULE
              MOVE "X" TO ERRCD
              GO TO END-ROUTINE.
           IF IO-TYPE = "PAG"
              PERFORM WRITE-MODULE.
           IF NEW-REC = "Y"
              MOVE "P" TO FORM-P
              MOVE 3 TO FORM-SEL.

           IF FORM-SEL NOT = 0
              IF FORM-P = "P"
                 MOVE GP-KEY TO SAVEKEY
                 MOVE FORM-SEL TO FORMNUM
                 GO TO END-ROUTINE.

           MOVE "N" TO NEW-REC.

           GO TO MAIN-MODULE.

      ******************************************************************
       REC-GP-MODULE SECTION.
      ******************************************************************
       REC-GP-MODULE-NO.
           MOVE SPACES TO IO-TYPE.

           IF SAVEKEY NOT = "  "
              MOVE SAVEKEY TO GP-KEY
              MOVE "  " TO SAVEKEY
              GO TO READ-GLOBAL.

           MOVE 0 TO NEW-FLAG.
           MOVE 1 TO GP-KEY.
      *-----------------------------------------------------------------
       READ-GLOBAL.
           PERFORM OPEN-GP-FILE.
           PERFORM READ-GP-FILE.
           PERFORM CLOSE-GP-FILE.
           IF IO-BAD
              MOVE "END" TO IO-TYPE
              GO TO END-REC-GP-MODULE.

           MOVE GP-REC TO GPOR-REC.
           PERFORM DISPLAY-FIELDS.
           MOVE 0 TO NEW-FLAG.
           IF NEW-REC = "Y"
              MOVE 1 TO NEW-FLAG.

       END-REC-GP-MODULE.
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

       MODIFY-ROUTINE.
           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.

           IF F1-KEY
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

           IF F7-KEY
              MOVE "RWR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

           IF NOT ENTER-KEY
              GO TO MODIFY-ROUTINE.

           MOVE VDUBUF TO ACTION.
           IF FORM-KEY = " " AND FORM-P = "P" AND FORM-SEL NUMERIC
              IF NOT (FORM-SEL = 1 OR 2 OR 4)
                 GO TO MODIFY-ROUTINE
              ELSE
                 MOVE "PAG" TO IO-TYPE
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

      *-----------------------------------------------------------------
       ENTER-ELE.
           IF CHKEY NOT = ZEROS
              IF CHFG (ELE) = " "
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
       TEST-F7.
           IF F7-KEY
              MOVE "END" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

      *-----------------------------------------------------------------
       TEST-F4.
           IF UP-KEY
             IF NEW-FLAG = 1
              MOVE -1 TO ONE
              GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       TEST-F5.
           IF F5-KEY
              MOVE 1 TO ONE
              GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       TEST-F1.
           IF F1-KEY
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

           IF NOT ENTER-KEY
              GO TO ENTER-ELE.

           MOVE 1 TO ONE.

      *-----------------------------------------------------------------
       GOTO-FIELD.
           GO TO F-01 F-02 F-03 F-04 F-05 F-06 F-07
                 F-08 F-09 F-10 F-11 F-12 F-13 F-14
                 F-15 F-16 F-17 F-18 F-19 F-20 F-21
                 F-22 F-23 F-24 F-25 F-26 F-27 F-28
                 F-29 F-30 F-31 F-32 F-33 F-34 F-35
                 F-36
               DEPENDING ON ELE.

           GO TO END-ENTRY-MODULE.

      *=================================================================
      *
      *    SET RECORD ELEMENTS
      *
      *=================================================================
       F-01.
           MOVE VDUBUF TO GP-IB-ACCRUE-EARN-ON-REPO.

           IF NOT (GP-IB-ACCRUE-EARN-ON-REPO = "Y" OR "N")
              MOVE "ENTER 'Y' OR 'N'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-02.
           MOVE VDUBUF TO GP-DELINQ-WITH-O2.
           IF NOT (GP-DELINQ-WITH-O2 = "Y" OR "N")
              MOVE "ENTER 'Y' OR 'N' ONLY!" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-03.
           MOVE VDUBUF TO GP-ALLOW-REPO-ON-PL.
           IF NOT (GP-ALLOW-REPO-ON-PL = "Y" OR "N")
              MOVE "ENTER 'Y' OR 'N' ONLY!" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-04.
           MOVE VDUBUF TO GP-BK-PRIOR-TO-POSTING.
           IF NOT (GP-BK-PRIOR-TO-POSTING = "Y" OR "N")
              MOVE "ENTER 'Y' OR 'N' ONLY!" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-05.
           MOVE VDUNUM2 TO GP-AMT-CASH-PROCEEDS
           GO TO CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-06.

           MOVE VDUBUF TO GP-INCL-OTH-IN-EARNED.

           IF NOT (GP-INCL-OTH-IN-EARNED = "Y" OR "N" OR "R")
              MOVE "ENTER 'Y', 'R', OR 'N' ONLY!" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           IF GP-INCL-OTH-IN-EARNED = "R"
              MOVE "'R' IS NOT AVAILABLE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-07.
           MOVE VDUBUF TO GP-DL-MEMO-TOGGLE.
           IF NOT (GP-DL-MEMO-TOGGLE = "Y" OR "N")
              MOVE "ENTER 'Y' OR 'N' ONLY!" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-08.
           MOVE VDUBUF TO GP-RN-FOR10-LN.
           IF NOT (GP-RN-FOR10-LN = "Y" OR "N")
              MOVE "ENTER 'Y' OR 'N' ONLY!" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-09.
           MOVE VDUBUF TO GP-OT-INS-TERM.
           IF NOT (GP-OT-INS-TERM = "Y" OR "N")
              MOVE "ENTER 'Y' OR 'N' ONLY!" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-10.
           MOVE VDUBUF TO GP-UNEARNED-STATS.
           IF NOT (GP-UNEARNED-STATS = "Y" OR "N")
              MOVE "ENTER 'Y' OR 'N' ONLY!" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-11.
           MOVE VDUBUF TO GP-RECDEL-REVERSAL-FG.
           IF NOT (GP-RECDEL-REVERSAL-FG = "Y" OR "N")
              MOVE "ENTER 'Y' OR 'N' ONLY!" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
      *----------------------------------------------------------------
       F-12.
           MOVE VDUBUF TO GP-ALLOW-PL-ON-SX.
           IF NOT (GP-ALLOW-PL-ON-SX = "Y" OR "N")
              MOVE "ENTER 'Y' OR 'N' ONLY!" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
      *----------------------------------------------------------------
       F-13.
           MOVE VDUBUF TO GP-CPCORP-CURBAL.
           IF NOT (GP-CPCORP-CURBAL = "Y" OR "N")
              MOVE "ENTER 'Y' OR 'N' ONLY!" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
      *----------------------------------------------------------------
       F-14.
           MOVE VDUNUM0 TO GP-TIME-ZONE-FACTOR.
           IF GP-TIME-ZONE-FACTOR < -24 OR > +24
              MOVE "ENTER A VALID TIME ZONE FACTOR" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
      *----------------------------------------------------------------
       F-15.
           MOVE VDUBUF TO GP-TIME-ZONE-DLS.
           IF NOT (GP-TIME-ZONE-DLS = "Y" OR "N")
              MOVE "ENTER 'Y' OR 'N' ONLY!" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
      *-------------------------------------------------------------------
       F-16.
           MOVE VDUBUF TO GP-RV-MEMO.
           IF NOT (GP-RV-MEMO = "Y" OR "N" )
              AND  (GP-RV-MEMO < LOWER-A OR > LOWER-Z)

           STRING "MUST BE 'Y', 'N' '" LOWER-A "' THRU '"
                  LOWER-Z       DELIMITED BY SIZE INTO MESS

      *CS     MOVE "ENTER 'Y', 'N', OR LOWER CASE 'A' THRU 'Z' ONLY!"
      *CS       TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
      *-------------------------------------------------------------------
       F-17.
           MOVE VDUBUF TO GP-IGNORE-LCHG-NXDUE.
           IF NOT (GP-IGNORE-LCHG-NXDUE = "Y" OR "N")
              MOVE "ENTER 'Y' OR 'N' ONLY!" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
      *-------------------------------------------------------------------
       F-18.
           MOVE VDUBUF TO GP-NO-GL-ON-O2-WHEN-PL.
           IF NOT (GP-NO-GL-ON-O2-WHEN-PL = "Y" OR "N")
              MOVE "ENTER 'Y' OR 'N' ONLY!" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
      *-------------------------------------------------------------------
       F-19.
           MOVE VDUBUF TO GP-RESCHEDULE-MEMO.
           IF NOT (GP-RESCHEDULE-MEMO = "Y" OR "N")
              MOVE "ENTER 'Y' OR 'N' ONLY!" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
      *-------------------------------------------------------------------
       F-20.
           MOVE VDUBUF TO GP-DONT-COLL-INT-ON-SS.
           IF NOT (GP-DONT-COLL-INT-ON-SS = "Y" OR "N")
              MOVE "ENTER 'Y' OR 'N' ONLY!" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
      *-------------------------------------------------------------------
       F-21.
           MOVE VDUBUF TO GP-CREATE-TP-FORMS.
           IF NOT (GP-CREATE-TP-FORMS = "Y" OR "N")
              MOVE "ENTER 'Y' OR 'N' ONLY!" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
      *-------------------------------------------------------------------
       F-22.
           MOVE VDUBUF TO GP-DLCOMM-CMSI-FG.
           IF NOT (GP-DLCOMM-CMSI-FG = "Y" OR "N")
              MOVE "ENTER 'Y' OR 'N' ONLY!" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
      *-------------------------------------------------------------------
       F-23.
           MOVE VDUNUM0 TO GP-EARLIEST-CLOSE-TIME.
           IF GP-EARLIEST-CLOSE-TIME > 2399
              MOVE "ENTER A MILITIARY TIME 0-2399!" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
      *-------------------------------------------------------------------
       F-24.
           MOVE VDUBUF TO GP-GROUP-APPROVAL-FG.
           IF NOT (GP-GROUP-APPROVAL-FG = "Y" OR "N")
              MOVE "ENTER 'Y' OR 'N' ONLY!" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
      *-------------------------------------------------------------------
       F-25.
           MOVE VDUBUF TO GP-APPROVAL-MEMO.
           IF NOT (GP-APPROVAL-MEMO = "Y" OR "N")
              MOVE "ENTER 'Y' OR 'N' ONLY!" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
      *-------------------------------------------------------------------
       F-26.
           MOVE VDUBUF TO GP-DEFAULT-O3-INSOURS.
           IF NOT (GP-DEFAULT-O3-INSOURS = "Y" OR "N" OR "M" OR " ")
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
      *-------------------------------------------------------------------
       F-27.
           MOVE VDUBUF TO GP-USE-BW-BAD-FLAGS.
           IF NOT (GP-USE-BW-BAD-FLAGS = "Y" OR "N" OR " ")
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
      *-------------------------------------------------------------------
       F-28.
           MOVE VDUBUF TO GP-NON-FILE-FEECODE.
           GO TO CREATE-ROUTINE.
      *-------------------------------------------------------------------
       F-29.
           MOVE VDUNUM0 TO GP-CHBORR-PAST-DUE-DAYS.
           GO TO CREATE-ROUTINE.
      *-------------------------------------------------------------------
       F-30.
           MOVE VDUBUF TO GP-BACKDATE-MEMO.
           IF NOT (GP-BACKDATE-MEMO = "Y" OR "N" )
              AND  (GP-BACKDATE-MEMO < LOWER-A OR > LOWER-Z)

           STRING "MUST BE 'Y', 'N' '" LOWER-A "' THRU '"
                  LOWER-Z       DELIMITED BY SIZE INTO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
      *-------------------------------------------------------------------
       F-31.
           MOVE VDUBUF TO GP-NEW-REPO-UNEARNED-ACCTS.
           IF NOT (GP-NEW-REPO-UNEARNED-ACCTS = "Y" OR "N")
              MOVE "ENTER 'Y' OR 'N' ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
      *-------------------------------------------------------------------
       F-32.
           MOVE VDUBUF TO GP-BK-FLAG-CONFIDENTIAL.
           IF NOT (GP-BK-FLAG-CONFIDENTIAL = "Y" OR "N" OR "L")
              MOVE "ENTER 'Y', 'N' OR 'L' ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
      *-------------------------------------------------------------------
       F-33.
           MOVE VDUBUF TO GP-AMORTIZE-ACQUIST-FEE.
           IF NOT (GP-AMORTIZE-ACQUIST-FEE = "Y" OR "N")
              MOVE "ENTER 'Y' OR 'N' ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
      *-------------------------------------------------------------------
       F-34.
           MOVE VDUBUF TO GP-DENY-FORM-NO-ADDRESS.
           IF NOT (GP-DENY-FORM-NO-ADDRESS = "Y" OR "N")
              MOVE "ENTER 'Y' OR 'N' ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
      *-------------------------------------------------------------------
       F-35.
           MOVE VDUBUF TO GP-HIDE-FIRST-5-SSNO.
           IF NOT (GP-HIDE-FIRST-5-SSNO = "Y" OR "N")
              MOVE "ENTER 'Y' OR 'N' ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
      *-------------------------------------------------------------------
       F-36.
           MOVE VDUBUF TO GP-DEFERMENT-APPROVAL.
           IF NOT (GP-DEFERMENT-APPROVAL = "Y" OR "N")
              MOVE "ENTER 'Y' OR 'N' ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.
       END-ENTRY-MODULE.
           MOVE 0 TO FORM-SEL.
       EXIT-ENTRY-MODULE.
           EXIT.

      ******************************************************************
       WRITE-MODULE SECTION.
      ******************************************************************
           PERFORM OPEN-GP-FILE.

           IF NEW-FLAG NOT = 0
              GO TO WRITE-RECORD.

           MOVE GP-REC TO GPCH-REC.

           IF GPOR-REC = GPCH-REC
              GO TO END-WRITE-MODULE.

           PERFORM READ-GP-FILE.
           IF IO-BAD
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-WRITE-MODULE.

           IF GP-REC NOT = GPOR-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-WRITE-MODULE.

           PERFORM LOG-MAINTENANCE-CHGS.

           MOVE GPCH-REC TO GP-REC.
           PERFORM REWRITE-GP-FILE.

           GO TO END-WRITE-MODULE.

       WRITE-RECORD.
           PERFORM WRITE-GP-FILE.
           IF IO-BAD
              PERFORM ALREADY-CREATED.

       END-WRITE-MODULE.
           PERFORM CLOSE-GP-FILE.

      ******************************************************************
       LOG-MAINTENANCE-CHGS SECTION.
      ******************************************************************

           MOVE SPACES                     TO LMC-DETAIL.
           MOVE " TO "                     TO LMC-TO-X.

           IF GPOR-IB-ACCRUE-EARN-ON-REPO NOT =
                                       GPCH-IB-ACCRUE-EARN-ON-REPO
              MOVE "01 "                   TO LMC-FIELD
              MOVE "ACCRUE IB REPO"        TO LMC-DESC
              MOVE GPOR-IB-ACCRUE-EARN-ON-REPO      TO LMC-ORG-TEXT
              MOVE GPCH-IB-ACCRUE-EARN-ON-REPO      TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-DELINQ-WITH-O2 NOT = GPCH-DELINQ-WITH-O2
              MOVE "02 "                   TO LMC-FIELD
              MOVE "O2 WITH DELNQ"          TO LMC-DESC
              MOVE GPOR-DELINQ-WITH-O2    TO LMC-ORG-TEXT
              MOVE GPCH-DELINQ-WITH-O2    TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-ALLOW-REPO-ON-PL NOT = GPCH-ALLOW-REPO-ON-PL
              MOVE "03 "                   TO LMC-FIELD
              MOVE "ALLOW REPO PL"          TO LMC-DESC
              MOVE GPOR-ALLOW-REPO-ON-PL    TO LMC-ORG-TEXT
              MOVE GPCH-ALLOW-REPO-ON-PL    TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-BK-PRIOR-TO-POSTING NOT = GPCH-BK-PRIOR-TO-POSTING
              MOVE "04 "                   TO LMC-FIELD
              MOVE "BK SCREEN    "          TO LMC-DESC
              MOVE GPOR-BK-PRIOR-TO-POSTING    TO LMC-ORG-TEXT
              MOVE GPCH-BK-PRIOR-TO-POSTING    TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-AMT-CASH-PROCEEDS NOT = GPCH-AMT-CASH-PROCEEDS
              MOVE "05 "                   TO LMC-FIELD
              MOVE "AMT CASH PROC"          TO LMC-DESC
              MOVE GPOR-AMT-CASH-PROCEEDS    TO LMC-ORG-AMT
              MOVE GPCH-AMT-CASH-PROCEEDS    TO LMC-CHG-AMT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-INCL-OTH-IN-EARNED NOT = GPCH-INCL-OTH-IN-EARNED
              MOVE "06 "                   TO LMC-FIELD
              MOVE "INCL OTH EARN"          TO LMC-DESC
              MOVE GPOR-INCL-OTH-IN-EARNED    TO LMC-ORG-TEXT
              MOVE GPCH-INCL-OTH-IN-EARNED    TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-DL-MEMO-TOGGLE NOT = GPCH-DL-MEMO-TOGGLE
              MOVE "07 "                   TO LMC-FIELD
              MOVE "TOGGLE DLMEMO"          TO LMC-DESC
              MOVE GPOR-DL-MEMO-TOGGLE    TO LMC-ORG-TEXT
              MOVE GPCH-DL-MEMO-TOGGLE    TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-RN-FOR10-LN NOT = GPCH-RN-FOR10-LN
              MOVE "08 "                   TO LMC-FIELD
              MOVE "BASE RN < 10%"          TO LMC-DESC
              MOVE GPOR-RN-FOR10-LN    TO LMC-ORG-TEXT
              MOVE GPCH-RN-FOR10-LN    TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-OT-INS-TERM NOT = GPCH-OT-INS-TERM
              MOVE "09 "                   TO LMC-FIELD
              MOVE "ENTER TERM OT"          TO LMC-DESC
              MOVE GPOR-OT-INS-TERM    TO LMC-ORG-TEXT
              MOVE GPCH-OT-INS-TERM    TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-UNEARNED-STATS NOT = GPCH-UNEARNED-STATS
              MOVE "10 "                   TO LMC-FIELD
              MOVE "TRACK UNEARND"          TO LMC-DESC
              MOVE GPOR-UNEARNED-STATS    TO LMC-ORG-TEXT
              MOVE GPCH-UNEARNED-STATS    TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-RECDEL-REVERSAL-FG NOT = GPCH-RECDEL-REVERSAL-FG
              MOVE "11 "                   TO LMC-FIELD
              MOVE "RECDEL MTD(-)"          TO LMC-DESC
              MOVE GPOR-RECDEL-REVERSAL-FG    TO LMC-ORG-TEXT
              MOVE GPCH-RECDEL-REVERSAL-FG    TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-ALLOW-PL-ON-SX NOT = GPCH-ALLOW-PL-ON-SX
              MOVE "12 "                   TO LMC-FIELD
              MOVE "PL ONHAND REP"          TO LMC-DESC
              MOVE GPOR-ALLOW-PL-ON-SX    TO LMC-ORG-TEXT
              MOVE GPCH-ALLOW-PL-ON-SX    TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-CPCORP-CURBAL NOT = GPCH-CPCORP-CURBAL
              MOVE "13 "                   TO LMC-FIELD
              MOVE "CURBAL CPCORP"          TO LMC-DESC
              MOVE GPOR-CPCORP-CURBAL    TO LMC-ORG-TEXT
              MOVE GPCH-CPCORP-CURBAL    TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-TIME-ZONE-FACTOR NOT = GPCH-TIME-ZONE-FACTOR
              MOVE "14 "                   TO LMC-FIELD
              MOVE "TIME ZONE FAC"          TO LMC-DESC
              MOVE GPOR-TIME-ZONE-FACTOR    TO LMC-ORG-NUM
              MOVE GPCH-TIME-ZONE-FACTOR    TO LMC-CHG-NUM
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-TIME-ZONE-DLS NOT = GPCH-TIME-ZONE-DLS
              MOVE "15 "                   TO LMC-FIELD
              MOVE "TIME ZONE DAY"          TO LMC-DESC
              MOVE GPOR-TIME-ZONE-DLS    TO LMC-ORG-TEXT
              MOVE GPCH-TIME-ZONE-DLS    TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-RV-MEMO NOT = GPCH-RV-MEMO
              MOVE "16 "                   TO LMC-FIELD
              MOVE "MEMO  ON RV  "          TO LMC-DESC
              MOVE GPOR-RV-MEMO    TO LMC-ORG-TEXT
              MOVE GPCH-RV-MEMO    TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-IGNORE-LCHG-NXDUE NOT = GPCH-IGNORE-LCHG-NXDUE
              MOVE "17 "                   TO LMC-FIELD
              MOVE "IGNORE LCHG  "          TO LMC-DESC
              MOVE GPOR-IGNORE-LCHG-NXDUE    TO LMC-ORG-TEXT
              MOVE GPCH-IGNORE-LCHG-NXDUE    TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-NO-GL-ON-O2-WHEN-PL NOT = GPCH-NO-GL-ON-O2-WHEN-PL
              MOVE "18 "                   TO LMC-FIELD
              MOVE "NO GL O2 PL  "          TO LMC-DESC
              MOVE GPOR-NO-GL-ON-O2-WHEN-PL    TO LMC-ORG-TEXT
              MOVE GPCH-NO-GL-ON-O2-WHEN-PL    TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-RESCHEDULE-MEMO NOT = GPCH-RESCHEDULE-MEMO
              MOVE "19 "                   TO LMC-FIELD
              MOVE "MEMO RESCHED "          TO LMC-DESC
              MOVE GPOR-RESCHEDULE-MEMO    TO LMC-ORG-TEXT
              MOVE GPCH-RESCHEDULE-MEMO    TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-DONT-COLL-INT-ON-SS NOT = GPCH-DONT-COLL-INT-ON-SS
              MOVE "20 "                   TO LMC-FIELD
              MOVE "NO INT/LC SS "          TO LMC-DESC
              MOVE GPOR-DONT-COLL-INT-ON-SS    TO LMC-ORG-TEXT
              MOVE GPCH-DONT-COLL-INT-ON-SS    TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-CREATE-TP-FORMS NOT = GPCH-CREATE-TP-FORMS
              MOVE "21 "                   TO LMC-FIELD
              MOVE "FORMS ON TP  "          TO LMC-DESC
              MOVE GPOR-CREATE-TP-FORMS    TO LMC-ORG-TEXT
              MOVE GPCH-CREATE-TP-FORMS    TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-DLCOMM-CMSI-FG NOT = GPCH-DLCOMM-CMSI-FG
              MOVE "22 "                   TO LMC-FIELD
              MOVE "ENTER DLCOMM "          TO LMC-DESC
              MOVE GPOR-DLCOMM-CMSI-FG    TO LMC-ORG-TEXT
              MOVE GPCH-DLCOMM-CMSI-FG    TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-EARLIEST-CLOSE-TIME NOT = GPCH-EARLIEST-CLOSE-TIME
              MOVE "23 "                   TO LMC-FIELD
              MOVE "EARLY CLOSE  "          TO LMC-DESC
              MOVE GPOR-EARLIEST-CLOSE-TIME    TO LMC-ORG-NUM
              MOVE GPCH-EARLIEST-CLOSE-TIME    TO LMC-CHG-NUM
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-GROUP-APPROVAL-FG NOT = GPCH-GROUP-APPROVAL-FG
              MOVE "24 "                   TO LMC-FIELD
              MOVE "GROUP APPROVL"          TO LMC-DESC
              MOVE GPOR-GROUP-APPROVAL-FG    TO LMC-ORG-TEXT
              MOVE GPCH-GROUP-APPROVAL-FG    TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-APPROVAL-MEMO NOT = GPCH-APPROVAL-MEMO
              MOVE "25 "                   TO LMC-FIELD
              MOVE "APPROVL MEMO "          TO LMC-DESC
              MOVE GPOR-APPROVAL-MEMO    TO LMC-ORG-TEXT
              MOVE GPCH-APPROVAL-MEMO    TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-DEFAULT-O3-INSOURS NOT = GPCH-DEFAULT-O3-INSOURS
              MOVE "26 "                   TO LMC-FIELD
              MOVE "O3INS OURS FG"          TO LMC-DESC
              MOVE GPOR-DEFAULT-O3-INSOURS    TO LMC-ORG-TEXT
              MOVE GPCH-DEFAULT-O3-INSOURS    TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-USE-BW-BAD-FLAGS NOT = GPCH-USE-BW-BAD-FLAGS
              MOVE "27 "                   TO LMC-FIELD
              MOVE "BW BAD FLAGS "          TO LMC-DESC
              MOVE GPOR-USE-BW-BAD-FLAGS    TO LMC-ORG-TEXT
              MOVE GPCH-USE-BW-BAD-FLAGS    TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-NON-FILE-FEECODE NOT = GPCH-NON-FILE-FEECODE
              MOVE "28 "                   TO LMC-FIELD
              MOVE "NONFILE FEECD"          TO LMC-DESC
              MOVE GPOR-NON-FILE-FEECODE    TO LMC-ORG-TEXT
              MOVE GPCH-NON-FILE-FEECODE    TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-CHBORR-PAST-DUE-DAYS NOT = GPCH-CHBORR-PAST-DUE-DAYS
              MOVE "29 "                   TO LMC-FIELD
              MOVE "PASTDUE BWCHG"          TO LMC-DESC
              MOVE GPOR-CHBORR-PAST-DUE-DAYS    TO LMC-ORG-NUM
              MOVE GPCH-CHBORR-PAST-DUE-DAYS    TO LMC-CHG-NUM
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-BACKDATE-MEMO NOT = GPCH-BACKDATE-MEMO
              MOVE "30 "                   TO LMC-FIELD
              MOVE "MEMO BACKDATE"          TO LMC-DESC
              MOVE GPOR-BACKDATE-MEMO    TO LMC-ORG-TEXT
              MOVE GPCH-BACKDATE-MEMO    TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-NEW-REPO-UNEARNED-ACCTS NOT =
                                      GPCH-NEW-REPO-UNEARNED-ACCTS
              MOVE "31 "                   TO LMC-FIELD
              MOVE "GLNO FOR REPO"          TO LMC-DESC
              MOVE "MEMO BACKDATE"          TO LMC-DESC
              MOVE GPOR-NEW-REPO-UNEARNED-ACCTS TO LMC-ORG-TEXT
              MOVE GPCH-NEW-REPO-UNEARNED-ACCTS TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-BK-FLAG-CONFIDENTIAL NOT = GPCH-BK-FLAG-CONFIDENTIAL
              MOVE "32 "                   TO LMC-FIELD
              MOVE "FLAG CONF BK "          TO LMC-DESC
              MOVE GPOR-BK-FLAG-CONFIDENTIAL    TO LMC-ORG-TEXT
              MOVE GPCH-BK-FLAG-CONFIDENTIAL    TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-AMORTIZE-ACQUIST-FEE NOT = GPCH-AMORTIZE-ACQUIST-FEE
              MOVE "33 "                   TO LMC-FIELD
              MOVE "AMORTIZE ACQ "          TO LMC-DESC
              MOVE GPOR-AMORTIZE-ACQUIST-FEE    TO LMC-ORG-TEXT
              MOVE GPCH-AMORTIZE-ACQUIST-FEE    TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-DENY-FORM-NO-ADDRESS NOT = GPCH-DENY-FORM-NO-ADDRESS
              MOVE "34 "                   TO LMC-FIELD
              MOVE "DENY NO ADDR "          TO LMC-DESC
              MOVE GPOR-DENY-FORM-NO-ADDRESS    TO LMC-ORG-TEXT
              MOVE GPCH-DENY-FORM-NO-ADDRESS    TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-HIDE-FIRST-5-SSNO NOT = GPCH-HIDE-FIRST-5-SSNO
              MOVE "35 "                   TO LMC-FIELD
              MOVE "HIDE FIRST 5 "          TO LMC-DESC
              MOVE GPOR-HIDE-FIRST-5-SSNO    TO LMC-ORG-TEXT
              MOVE GPCH-HIDE-FIRST-5-SSNO    TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-DEFERMENT-APPROVAL NOT = GPCH-DEFERMENT-APPROVAL
              MOVE "36 "                   TO LMC-FIELD
              MOVE "DEF APPROVAL "          TO LMC-DESC
              MOVE GPOR-DEFERMENT-APPROVAL    TO LMC-ORG-TEXT
              MOVE GPCH-DEFERMENT-APPROVAL    TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.

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

           MOVE GP-IB-ACCRUE-EARN-ON-REPO TO F01.
           MOVE GP-DELINQ-WITH-O2         TO F02.
           MOVE GP-ALLOW-REPO-ON-PL       TO F03.
           MOVE GP-BK-PRIOR-TO-POSTING    TO F04.
           MOVE GP-AMT-CASH-PROCEEDS      TO F05.
           MOVE GP-INCL-OTH-IN-EARNED     TO F06.
           MOVE GP-DL-MEMO-TOGGLE         TO F07.
           MOVE GP-RN-FOR10-LN            TO F08.
           MOVE GP-OT-INS-TERM            TO F09.
           MOVE GP-UNEARNED-STATS         TO F10.
           MOVE GP-RECDEL-REVERSAL-FG     TO F11.
           MOVE GP-ALLOW-PL-ON-SX         TO F12.
           MOVE GP-CPCORP-CURBAL          TO F13.
           MOVE GP-TIME-ZONE-FACTOR       TO F14.
           MOVE GP-TIME-ZONE-DLS          TO F15.
           MOVE GP-RV-MEMO                TO F16.
           MOVE GP-IGNORE-LCHG-NXDUE      TO F17.
           MOVE GP-NO-GL-ON-O2-WHEN-PL    TO F18.
           MOVE GP-RESCHEDULE-MEMO        TO F19.
           MOVE GP-DONT-COLL-INT-ON-SS    TO F20.
           MOVE GP-CREATE-TP-FORMS        TO F21.
           MOVE GP-DLCOMM-CMSI-FG         TO F22.
           MOVE GP-EARLIEST-CLOSE-TIME    TO F23.
           MOVE GP-GROUP-APPROVAL-FG      TO F24.
           MOVE GP-APPROVAL-MEMO          TO F25.
           MOVE GP-DEFAULT-O3-INSOURS     TO F26.
           MOVE GP-USE-BW-BAD-FLAGS       TO F27.
           MOVE GP-NON-FILE-FEECODE       TO F28.
           MOVE GP-CHBORR-PAST-DUE-DAYS   TO F29.
           MOVE GP-BACKDATE-MEMO          TO F30.
           MOVE GP-NEW-REPO-UNEARNED-ACCTS TO F31.
           MOVE GP-BK-FLAG-CONFIDENTIAL   TO F32.
           MOVE GP-AMORTIZE-ACQUIST-FEE   TO F33.
           MOVE GP-DENY-FORM-NO-ADDRESS   TO F34.
           MOVE GP-HIDE-FIRST-5-SSNO      TO F35.
           MOVE GP-DEFERMENT-APPROVAL     TO F36.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       DISPLAY-FIELDS-EXIT.
           EXIT.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".

      **************************************************
       FORM-RESET SECTION.
      **************************************************
           MOVE EXT-CONAME-CONAME TO GPMAN3-CONAME.
           MOVE EXT-CONAME-SYSDATE TO GPMAN3-SYSDATE.
           DISPLAY GPMAN3-SCREEN UPON GPMAIN-WINDOW-HANDLE..
           MOVE GPMAN3-FORM-FIELDS TO WR-BUF.
           MOVE GPMAN3-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **************************************************
       SETUP-LINK-INFO SECTION.
      **************************************************
           MOVE GPMAN3-HELP--FILE TO HELP-LINK-FILE.
           MOVE GPMAN3-HELP--DIR TO HELP-LINK-DIR.

      **************************************************
       VDU-CONVERT-FIELD SECTION.
      **************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBGB/GPMAN3_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISC-PARAGRAPHS SECTION.
      ******************************************************************
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/ACCESS.CPY".

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

       ACTIVE-RECORD.
           MOVE " ACTIVE CUSTOMER CANNOT BE DELETED" TO MESS.
           PERFORM SEND-MESS.

      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-GP-FILE.
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBGL/GLGLGS_SQL.CPY".
       COPY "LIBGB/GBGPGS_SQL.CPY".
       COPY "LIBGB/GBGPIN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      ******************************************************************
       END-ROUTINE SECTION.
      ******************************************************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY GPMAN3-SCREEN.
           MOVE FORM-PATH TO FORM-PATHNAME.
           MOVE GPMAIN-LINK TO FORM-LINK-AREA.
           EXIT PROGRAM.
