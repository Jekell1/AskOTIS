       IDENTIFICATION DIVISION.
       PROGRAM-ID.      GPOPM2.
       DATE-WRITTEN.    041602.
      ******************************************************************
      *
      *  DIR :   GB
      *  DESC:   GP FILE HIDDEN GLOBAL PARAMETER FILE MAINT PAGE 2
      *
      * REV:
      * BAH 020626 ADDED 2. P&L/REPO REASON CODES, REGACC PR# 1775
      * JTG 020801 CHANGED FOR (MIP) MONTHLY INSURANCE PREMIUMS   REGENCY #1801
      *            ADDED GP-ALLOW-MIP-INSURANCE
      * BLV 020813 ADDED 4. GP-ENTER-LPO-FG, SOAUTO PR# 1885, DETERMINES
      *            WHETHER TO STOP FOR ENTRY OF LPO, REP-CD & PROCESS-CD
      *            IN LOAN ENTRY PROGRAMS
      * JTG 030312 ADDED GP-FORCE-INPUT-PP-COVERAGE                   WORLD #319
      * JTG 030318 ADDED GP-NO-SC-USE-DM -NO SALESFIN USE DIRECT MAIL WORLD #307
      * BAH 030328 ADDED GP-LR-TRAILER-TRIAD, NEW LOAN TRAILER  REGACC #26
      *  CS 030411 ADDED GP-NF-UPD-RREFNO  [CASHCO PR#2034]
      * BLV 030421 ADDED GP-EMPLOYEE-TIME-CARDS, CASHCO PR# 2045
      * BLF 060224 ADDED GP-LOAN-ACH-PROCESSING, LENDMARK PR# 2711
      * BLF 060420 ALLOW ENTRY OF "P" FOR GP-PL-REPO-REASON, FOR ENTRY OF
      *            REASON CODE ON A P&L, BUT NOT ON A REPO, LENDMARK PR# 2770
      * BLF 060611 ADDED GP-PAYEE-PROCESSING-FG, DEVELOPMENT PR# 2633
      * BAH 070307 ADDED GP-EXPANDED-FOLLOW-UP TRITON PR# 2937
      * MJD 081024 ADDED GP-ALLOW-MF-ON-IB TO ALLOW MFEES ON IB ACCOUNTS
      *             WORLD PR#590
      * BLM 100203 ADDED GP-ESCROW-FG, REGWAN PR# 3506
      * BLM 110211 ADDED GP-GPS-INFORMATION, PCCREDIT PR# 3699
      *  CS 110218 ADDED GP-USE-ADDRESS-BW-SCAN  PR#3720  MODERN PR#3720
      *            IF FLAG IS "Y", SCAN ON BW-ADR IS AVAILABLE IN F10
      * BAH 110223 ADDED GP-EXPANDED-LEGAL-WINDOW PR# 3707 UNITED
      * BAH 110328 ADDED GP-USE-SCRA(SERVICE MEMBER CIVIL RELIEF ACT)
      *                                                        REGACC PR# 289
      *  CS 111012 ADD GP-USE-ACCRUAL-EARNINGS(AE)FILE  PR#299 REGACC
      * KEC 120212 {PR#03978} PARADATA <A15> [DAVID SINCLAIR]
      *            CREATED THE ABILITY TO CHOOSE THE WAY THE 'MESSAGE'
      *            IS DISPLAYED ON THE ACCOUNT INQUIRY, PAYMENT
      *            POSTING AND COLLECTION WORK SCREENS.
      * BAH 120404 ADDED GP-JUDGMENT-WINDOW PR# 3990 CITIZENS
      * BAH 120514 REMOVED MAXPRU NONDISPLAY
      *  CS 131202 ADDED VALUE OF 'R' TO GP-USE-ACCRUAL-EARN-FILE. IF 'R', A
      *            PREPROCESSOR (CL/RAC347) WILL RUN THAT WILL UPDATE AEFILE AND
      *            LRFILE. ONLY REGACC WILL RUN THE PREPROCESSOR REGACC PR#347
      * BAH 180501 ADDED ERRLOG
      *  CS 181108 REMOVED #22 AEMAIN, REMOVED FOR A32
      * BAH 20190703 REMOVED GP-EMPLOYEE-TIME-CARDS #9, PT0161
      * BAH 20190709 REMOVED GP-LR-TRAILER-TRIAD, PT0151 
      *  CS 20190726 REMOVED TESTS ON GP-NF-UPD-REFNO(2)
      * BAH 230302 ADDED 25. TEST PP + AUTO INSCOVR MAX #1594
      * BAH 230419 REMOVED 25. TEST PP + AUTO INSCOVR MAX #1594A
      * BAH 20231019 ADDED 25. GP-CREATE-PAY-XTRP-XNO #1629
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
      *    W O R K I N G - S T O R A G E - S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)GB/GPOPM2 S35 10/23/23-12:35:50 >".

      ******************************************************************
      *
      *    P R O G R A M - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/GB01GP.CPY".
       COPY "LIBGB/GB01GP_SQL.CPY".
       COPY "LIBGB/GBWSGP.CPY".
       COPY "LIBSP/SPWSBB.CPY".
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

       01  GPOPMN-LINK        VALUE SPACES.
           03  FORM-NAME     PIC X(20).
           03  NEW-REC       PIC X.
           03  ERRCD         PIC X.
           03  FORMNUM       PIC 9.
           03  GPOPMN-WINDOW-HANDLE PIC X(10).

      ******************************************************************
      *
      *    S C R E E N - D I S P L A Y - B U F F E R
      *
      ******************************************************************
       01  DISPLAY-BUF    VALUE SPACES.
           03  F01         PIC X.
           03  F02         PIC X.
           03  F03         PIC X.
           03  F04         PIC X.
           03  F05         PIC X.
           03  F06         PIC X.
           03  F07         PIC X.
           03  F08         PIC X(5).
           03  F081        PIC X(5).
           03  F09         PIC X.
           03  F10         PIC X.
           03  F11         PIC X.
           03  F12         PIC X.
           03  F13         PIC X.
           03  F14         PIC X.
           03  F15         PIC X.
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

       01  DISPLAY-LIST.
           03  FILLER      PIC X(48) VALUE
           "010 020 030 040 050 060 070 080 081 090 100 110 ".
           03  FILLER      PIC X(60) VALUE
           "120 130 140 150 160 170 180 190 200 210 220 230 240 250.".

      ******************************************************************
      *
      *    E L E M E N T - S P E C I F I C A T I O N S - # 1
      *
      ******************************************************************
       78  MAX                      VALUE 25.
      *-----------------------------------------------------------------
       01  SPEC-TABLE.
      * 1. GP-CHECK-RECONCILIATION
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      * 2. GP-PL-REPO-REASON
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      * 3. GP-ALLOW-MIP-INSURANCE
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      * 4. GP-ENTER-LPO-FG
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      * 5. GP-FORCE-INPUT-PP-COVERAGE
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      * 6. GP-NO-SC-USE-DM
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      * 7. N/A
           03  FILLER      PIC X(8) VALUE "E NA010N".
      * 8. N/A                   
           03  FILLER      PIC X(8) VALUE "E NA050 ".
      * 9. N/A
           03  FILLER      PIC X(8) VALUE "E NA010N".
      *10. GP-LOAN-ACH-PROCESSING
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      *11. GP-PAYEE-PROCESSING-FG
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      *12. GP-EXPANDED-FOLLOW-UP
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      *13. GP-PURGE-TO-ARCHIVE-FG
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      *14. GP-ALLOW-MF-ON-IB
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      *15. GP-ESCROW-FG
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      *16. GP-SECURITY-FG
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      *17. GP-GPS-INFORMATION
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      *18. GP-CUST-INFO-FG
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      *19. GP-USE-ADDRESS-BW-SCAN
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      *20. GP-EXPANDED-LEGAL-WINDOW
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      *21. GP-USE-SCRA
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      *22. GP-USE-ACCRUAL-EARN-FILE
           03  FILLER      PIC X(8) VALUE "E NA010N".
      *23. GP-MESSAGE-ATTR
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      *24. GP-JUDGMENT-WINDOW
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      *25. GP-CREATE-PAY-XTRP-XNO
           03  FILLER      PIC X(8) VALUE "E NA010 ".

       01  SPEC-TAB        REDEFINES SPEC-TABLE.
           03  SPEC-REC    OCCURS MAX TIMES.
               05  SPEC    PIC X(7).
               05  CHFG    PIC X.

       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBGB/GPOPM2_DEF.CPY".
       COPY "LIBGB/GPOPM2_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       01 FORM-LINK-AREA    PIC X(2000).
       SCREEN SECTION.
       COPY "LIBGB/GPOPM2_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME FORM-LINK-AREA EXIT-PATHNAME.

      ******************************************************************
      *
      *    M A I N - P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.
           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "GPOPM2" TO VDU-FORM-NAME.
           MOVE FORM-LINK-AREA TO GPOPMN-LINK.

       MAIN-MODULE.

           PERFORM FORM-RESET.

           PERFORM REC-GP-MODULE.
           IF IO-TYPE = "END"
              MOVE "X" TO ERRCD
              GO TO END-ROUTINE.

           PERFORM ENTRY-MODULE.
           IF IO-TYPE = "RWR"
              PERFORM WRITE-MODULE.
           IF FORM-SEL NOT = 0
              IF FORM-P = "P"
                 MOVE FORM-SEL TO FORMNUM
                 GO TO END-ROUTINE.

           IF IO-TYPE = "END"
              MOVE "X" TO ERRCD
              GO TO END-ROUTINE.

           GO TO MAIN-MODULE.

      ******************************************************************
       REC-GP-MODULE SECTION.
      ******************************************************************
       REC-GP-MODULE-NO.

           MOVE 0 TO NEW-FLAG.
           MOVE SPACES TO IO-TYPE.
           MOVE 1 TO GP-KEY.

       READ-GLOBAL.
           PERFORM OPEN-GP-FILE.
           PERFORM READ-GP-FILE.
           PERFORM CLOSE-GP-FILE.

           IF IO-GOOD
              MOVE GP-REC TO GPOR-REC
              PERFORM DISPLAY-FIELDS
            ELSE
              PERFORM GLOBAL-NOT-FOUND
              GO TO REC-GP-MODULE-NO.

      *-----------------------------------------------------------------
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
      D    STOP MESS.
           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.

           IF F1-KEY
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

           IF F7-KEY
              MOVE "END" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

           IF NOT ENTER-KEY
              GO TO MODIFY-ROUTINE.

           MOVE VDUBUF TO ACTION.

           IF ACTION = SPACES
              MOVE "RWR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

           IF FORM-KEY = " " AND FORM-P = "P" AND FORM-SEL NUMERIC
              IF NOT (FORM-SEL = 1 )
                 GO TO MODIFY-ROUTINE
              ELSE
                 MOVE "RWR" TO IO-TYPE
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
              GO TO GOTO-FIELD1.

      *-----------------------------------------------------------------
       TEST-F1.
           IF F1-KEY
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

           IF NOT ENTER-KEY
              GO TO ENTER-ELE.

           MOVE 1 TO ONE.

      *-----------------------------------------------------------------
       GOTO-FIELD1.
           GO TO F-01 F-02 F-03 F-04 F-05 F-06 F-07 F-08 F-09 F-10
                 F-11 F-12 F-13 F-14 F-15 F-16 F-17 F-18 F-19 F-20
                 F-21 F-22 F-23 F-24 F-25
           DEPENDING ON ELE.

      *=================================================================
      *
      *    S E T - R E C O R D - E L E M E N T S
      *
      *=================================================================
       F-01.
           MOVE VDUBUF TO GP-CHECK-RECONCILIATION.
           IF NOT (GP-CHECK-RECONCILIATION = "Y" OR "N")
              PERFORM SEND-Y-OR-N
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-02.
           MOVE VDUBUF TO GP-PL-REPO-REASON.
           IF NOT (GP-PL-REPO-REASON = "Y" OR "N" OR "P")
              PERFORM SEND-Y-OR-N
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-03.
           MOVE VDUBUF TO GP-ALLOW-MIP-INSURANCE.
           IF NOT (GP-ALLOW-MIP-INSURANCE = "Y" OR "N")
              PERFORM SEND-Y-OR-N
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-04.
           MOVE VDUBUF TO GP-ENTER-LPO-FG.
           IF NOT (GP-ENTER-LPO-FG = "Y" OR "N")
              PERFORM SEND-Y-OR-N
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-05.
           MOVE VDUBUF TO GP-FORCE-INPUT-PP-COVERAGE.
           IF NOT (GP-FORCE-INPUT-PP-COVERAGE = "Y" OR "N")
              PERFORM SEND-Y-OR-N
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.

       F-06.
           MOVE VDUBUF TO GP-NO-SC-USE-DM.
           IF NOT (GP-NO-SC-USE-DM = "Y" OR "N")
              PERFORM SEND-Y-OR-N
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.

       F-07.
           GO TO CREATE-ROUTINE.

       F-08.
           GO TO CREATE-ROUTINE.                      

       F-09.
           GO TO CREATE-ROUTINE.

       F-10.
           MOVE VDUBUF TO GP-LOAN-ACH-PROCESSING.
           IF NOT (GP-LOAN-ACH-PROCESSING = "Y" OR "N")
              PERFORM SEND-Y-OR-N
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.

       F-11.
           MOVE VDUBUF TO GP-PAYEE-PROCESSING-FG.
           IF NOT (GP-PAYEE-PROCESSING-FG = "Y" OR "N")
              PERFORM SEND-Y-OR-N
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-12.
           MOVE VDUBUF TO GP-EXPANDED-FOLLOW-UP.
           IF NOT (GP-EXPANDED-FOLLOW-UP = "Y" OR "N" OR "T")
              MOVE "ENTER 'Y', 'N' OR 'T'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.

       F-13.
           MOVE VDUBUF TO GP-PURGE-TO-ARCHIVE-FG.
           IF NOT (GP-PURGE-TO-ARCHIVE-FG = "Y" OR "N")
              PERFORM SEND-Y-OR-N
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-14.
           MOVE VDUBUF TO GP-ALLOW-MF-ON-IB.
           IF NOT (GP-ALLOW-MF-ON-IB = "Y" OR "N")
              PERFORM SEND-Y-OR-N
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-15.
           MOVE VDUBUF TO GP-ESCROW-FG.
           IF NOT (GP-ESCROW-FG = "Y" OR "N")
              PERFORM SEND-Y-OR-N
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-16.
           MOVE VDUBUF TO GP-SECURITY-FG.
           IF NOT (GP-SECURITY-FG = "Y" OR "N")
              PERFORM SEND-Y-OR-N
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-17.
           MOVE VDUBUF TO GP-GPS-INFORMATION.
           IF NOT (GP-GPS-INFORMATION = "Y" OR "N")
              PERFORM SEND-Y-OR-N
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-18.
           MOVE VDUBUF TO GP-CUST-INFO-FG.
           IF NOT (GP-CUST-INFO-FG = "Y" OR "N")
              PERFORM SEND-Y-OR-N
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.

       F-19.
           MOVE VDUBUF TO GP-USE-ADDRESS-BW-SCAN.
           IF NOT (GP-USE-ADDRESS-BW-SCAN = "Y" OR "N")
              PERFORM SEND-Y-OR-N
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.

       F-20.
           MOVE VDUBUF TO GP-EXPANDED-LEGAL-WINDOW.
           IF NOT (GP-EXPANDED-LEGAL-WINDOW = "Y" OR "N")
              PERFORM SEND-Y-OR-N
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-21.
           MOVE VDUBUF TO GP-USE-SCRA.
           IF NOT (GP-USE-SCRA = "Y" OR "N")
              PERFORM SEND-Y-OR-N
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-22.
           GO TO CREATE-ROUTINE.
       F-23.
           MOVE VDUBUF TO GP-MESSAGE-ATTR.
           IF ( GP-MESSAGE-ATTR = SPACES )
              MOVE "N" TO GP-MESSAGE-ATTR.
           IF ( GP-MESSAGE-ATTR-NONE     ) OR
              ( GP-MESSAGE-ATTR-FLASH    ) OR
              ( GP-MESSAGE-ATTR-REVERSE  ) OR
              ( GP-MESSAGE-ATTR-BOTH     )
              NEXT SENTENCE
           ELSE
              MOVE "ERROR: (N)ONE, (F)LASH, (R)EVERSE OR (B)OTH" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.

       F-24.
           MOVE VDUBUF TO GP-JUDGMENT-WINDOW.
           IF NOT (GP-JUDGMENT-WINDOW = "Y" OR "N")
              PERFORM SEND-Y-OR-N
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.

       F-25.
           MOVE VDUBUF TO GP-CREATE-PAY-XTRP-XNO.
           IF NOT (GP-CREATE-PAY-XTRP-XNO = "Y" OR "N")
              PERFORM SEND-Y-OR-N
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.

       END-ENTRY-MODULE.
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
           MOVE 1 TO GP-KEY.
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

           IF GPOR-CHECK-RECONCILIATION NOT =
                                     GPOR-CHECK-RECONCILIATION
              MOVE "01 "                   TO LMC-FIELD
              MOVE "CHECK REC   "          TO LMC-DESC
              MOVE GPOR-CHECK-RECONCILIATION     TO LMC-ORG-TEXT
              MOVE GPCH-CHECK-RECONCILIATION     TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-PL-REPO-REASON NOT = GPCH-PL-REPO-REASON
              MOVE "02 "                   TO LMC-FIELD
              MOVE "PL REPO REASON"        TO LMC-DESC
              MOVE GPOR-PL-REPO-REASON      TO LMC-ORG-TEXT
              MOVE GPCH-PL-REPO-REASON      TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-ALLOW-MIP-INSURANCE NOT = GPCH-ALLOW-MIP-INSURANCE
              MOVE "03 "                   TO LMC-FIELD
              MOVE "MIP INSURANCE "        TO LMC-DESC
              MOVE GPOR-ALLOW-MIP-INSURANCE      TO LMC-ORG-TEXT
              MOVE GPCH-ALLOW-MIP-INSURANCE      TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-ENTER-LPO-FG NOT = GPCH-ENTER-LPO-FG
              MOVE "04 "                   TO LMC-FIELD
              MOVE "ENTER LPO     "        TO LMC-DESC
              MOVE GPOR-ENTER-LPO-FG      TO LMC-ORG-TEXT
              MOVE GPCH-ENTER-LPO-FG      TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-FORCE-INPUT-PP-COVERAGE NOT =
                                        GPCH-FORCE-INPUT-PP-COVERAGE
              MOVE "05 "                   TO LMC-FIELD
              MOVE "FORCE PP COVG "        TO LMC-DESC
              MOVE GPOR-FORCE-INPUT-PP-COVERAGE      TO LMC-ORG-TEXT
              MOVE GPCH-FORCE-INPUT-PP-COVERAGE      TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-NO-SC-USE-DM NOT = GPCH-NO-SC-USE-DM
              MOVE "06 "                   TO LMC-FIELD
              MOVE "NO SC USE DM  "        TO LMC-DESC
              MOVE GPOR-NO-SC-USE-DM      TO LMC-ORG-TEXT
              MOVE GPCH-NO-SC-USE-DM      TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-LOAN-ACH-PROCESSING NOT = GPCH-LOAN-ACH-PROCESSING
              MOVE "10 "                   TO LMC-FIELD
              MOVE "LOAN ACH PROC "        TO LMC-DESC
              MOVE GPOR-LOAN-ACH-PROCESSING      TO LMC-ORG-TEXT
              MOVE GPCH-LOAN-ACH-PROCESSING      TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-PAYEE-PROCESSING-FG NOT = GPCH-PAYEE-PROCESSING-FG
              MOVE "11 "                   TO LMC-FIELD
              MOVE "PAYEE PROCESS "        TO LMC-DESC
              MOVE GPOR-PAYEE-PROCESSING-FG      TO LMC-ORG-TEXT
              MOVE GPCH-PAYEE-PROCESSING-FG      TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-EXPANDED-FOLLOW-UP NOT = GPCH-EXPANDED-FOLLOW-UP
              MOVE "12 "                   TO LMC-FIELD
              MOVE "EXP FOLLOWUP  "        TO LMC-DESC
              MOVE GPOR-EXPANDED-FOLLOW-UP      TO LMC-ORG-TEXT
              MOVE GPCH-EXPANDED-FOLLOW-UP      TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-PURGE-TO-ARCHIVE-FG NOT = GPCH-PURGE-TO-ARCHIVE-FG
              MOVE "13 "                   TO LMC-FIELD
              MOVE "PURGE ARCHIVE "        TO LMC-DESC
              MOVE GPOR-PURGE-TO-ARCHIVE-FG      TO LMC-ORG-TEXT
              MOVE GPCH-PURGE-TO-ARCHIVE-FG      TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-ALLOW-MF-ON-IB NOT = GPCH-ALLOW-MF-ON-IB
              MOVE "14 "                   TO LMC-FIELD
              MOVE "ALLOW MF ON IB"        TO LMC-DESC
              MOVE GPOR-ALLOW-MF-ON-IB      TO LMC-ORG-TEXT
              MOVE GPCH-ALLOW-MF-ON-IB      TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-ESCROW-FG NOT = GPCH-ESCROW-FG
              MOVE "15 "                   TO LMC-FIELD
              MOVE "ESCROW        "        TO LMC-DESC
              MOVE GPOR-ESCROW-FG      TO LMC-ORG-TEXT
              MOVE GPCH-ESCROW-FG      TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-SECURITY-FG NOT = GPCH-SECURITY-FG
              MOVE "16 "                   TO LMC-FIELD
              MOVE "SECURITY      "        TO LMC-DESC
              MOVE GPOR-SECURITY-FG      TO LMC-ORG-TEXT
              MOVE GPCH-SECURITY-FG      TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-GPS-INFORMATION NOT = GPCH-GPS-INFORMATION
              MOVE "17 "                   TO LMC-FIELD
              MOVE "GPS INFO      "        TO LMC-DESC
              MOVE GPOR-GPS-INFORMATION      TO LMC-ORG-TEXT
              MOVE GPCH-GPS-INFORMATION      TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-CUST-INFO-FG NOT = GPCH-CUST-INFO-FG
              MOVE "18 "                   TO LMC-FIELD
              MOVE "CUSTOMER INFO "        TO LMC-DESC
              MOVE GPOR-CUST-INFO-FG      TO LMC-ORG-TEXT
              MOVE GPCH-CUST-INFO-FG      TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-USE-ADDRESS-BW-SCAN NOT = GPCH-USE-ADDRESS-BW-SCAN
              MOVE "19 "                   TO LMC-FIELD
              MOVE "ADDRESS BWSCAN"        TO LMC-DESC
              MOVE GPOR-USE-ADDRESS-BW-SCAN      TO LMC-ORG-TEXT
              MOVE GPCH-USE-ADDRESS-BW-SCAN      TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-EXPANDED-LEGAL-WINDOW NOT =
                                          GPCH-EXPANDED-LEGAL-WINDOW
              MOVE "20 "                   TO LMC-FIELD
              MOVE "EXPANDED LEGAL"        TO LMC-DESC
              MOVE GPOR-EXPANDED-LEGAL-WINDOW      TO LMC-ORG-TEXT
              MOVE GPCH-EXPANDED-LEGAL-WINDOW      TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-USE-SCRA NOT = GPCH-USE-SCRA
              MOVE "21 "                   TO LMC-FIELD
              MOVE "SCRA          "        TO LMC-DESC
              MOVE GPOR-USE-SCRA      TO LMC-ORG-TEXT
              MOVE GPCH-USE-SCRA      TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-USE-ACCRUAL-EARN-FILE NOT =
                                         GPCH-USE-ACCRUAL-EARN-FILE
              MOVE "22 "                      TO LMC-FIELD
              MOVE "ACCRL EARN FIL"           TO LMC-DESC
              MOVE GPOR-USE-ACCRUAL-EARN-FILE TO LMC-ORG-TEXT
              MOVE GPCH-USE-ACCRUAL-EARN-FILE TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-MESSAGE-ATTR NOT = GPCH-MESSAGE-ATTR
              MOVE "23 "                  TO LMC-FIELD
              MOVE "MSG ATTRIBUTES"       TO LMC-DESC
              MOVE GPOR-MESSAGE-ATTR      TO LMC-ORG-TEXT
              MOVE GPCH-MESSAGE-ATTR      TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-JUDGMENT-WINDOW NOT = GPCH-JUDGMENT-WINDOW
              MOVE "24 "                  TO LMC-FIELD
              MOVE "JUDGMNT WINDOW"       TO LMC-DESC
              MOVE GPOR-JUDGMENT-WINDOW   TO LMC-ORG-TEXT
              MOVE GPCH-JUDGMENT-WINDOW   TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-CREATE-PAY-XTRP-XNO NOT = GPCH-CREATE-PAY-XTRP-XNO
              MOVE "25 "                    TO LMC-FIELD
              MOVE "CREATE XTRP/NO"         TO LMC-DESC
              MOVE GPOR-CREATE-PAY-XTRP-XNO TO LMC-ORG-TEXT
              MOVE GPCH-CREATE-PAY-XTRP-XNO TO LMC-CHG-TEXT
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

           MOVE SPACES                     TO DISPLAY-BUF.
           MOVE GP-CHECK-RECONCILIATION    TO F01.
           MOVE GP-PL-REPO-REASON          TO F02.
           MOVE GP-ALLOW-MIP-INSURANCE     TO F03.
           MOVE GP-ENTER-LPO-FG            TO F04.
           MOVE GP-FORCE-INPUT-PP-COVERAGE TO F05.
           MOVE GP-NO-SC-USE-DM            TO F06.
           MOVE SPACES                     TO F07.
           MOVE SPACES                     TO F08.
           MOVE SPACES                     TO F081.
           MOVE SPACES                     TO F09.
           MOVE GP-LOAN-ACH-PROCESSING     TO F10.
           MOVE GP-PAYEE-PROCESSING-FG     TO F11.
           MOVE GP-EXPANDED-FOLLOW-UP      TO F12.
           MOVE GP-PURGE-TO-ARCHIVE-FG     TO F13.
           MOVE GP-ALLOW-MF-ON-IB          TO F14.
           MOVE GP-ESCROW-FG               TO F15.
           MOVE GP-SECURITY-FG             TO F16.
           MOVE GP-GPS-INFORMATION         TO F17.
           MOVE GP-CUST-INFO-FG            TO F18.
           MOVE GP-USE-ADDRESS-BW-SCAN     TO F19.
           MOVE GP-EXPANDED-LEGAL-WINDOW   TO F20.
           MOVE GP-USE-SCRA                TO F21.
           MOVE GP-USE-ACCRUAL-EARN-FILE   TO F22.
           MOVE GP-MESSAGE-ATTR            TO F23.
           MOVE GP-JUDGMENT-WINDOW         TO F24.
           MOVE GP-CREATE-PAY-XTRP-XNO     TO F25.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".

      **************************************************************************
       FORM-RESET SECTION.
      **************************************************************************
           MOVE EXT-CONAME-CONAME TO GPOPM2-CONAME.
           MOVE EXT-CONAME-SYSDATE TO GPOPM2-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY GPOPM2-SCREEN.
           DISPLAY GPOPM2-SCREEN UPON GPOPMN-WINDOW-HANDLE.
           MOVE GPOPM2-FORM-FIELDS TO WR-BUF.
           MOVE GPOPM2-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **************************************************************************
       SETUP-LINK-INFO SECTION.
      **************************************************************************
           MOVE GPOPM2-HELP--FILE TO HELP-LINK-FILE.
           MOVE GPOPM2-HELP--DIR TO HELP-LINK-DIR.

      **************************************************************************
       VDU-CONVERT-FIELD SECTION.
      **************************************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBGB/GPOPM2_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISC-PARAGRAPHS SECTION.
      ******************************************************************
       COPY "LIBGB/ACCESS.CPY".
       SEND-Y-OR-N.
           MOVE "MUST BE 'Y' OR 'N'" TO MESS.
           PERFORM SEND-MESS.
       SEND-Y-OR-R-N.
           MOVE "MUST BE 'Y' OR 'R' OR 'N'" TO MESS.
           PERFORM SEND-MESS.
       GLOBAL-NOT-FOUND.
           MOVE "GLOBAL PARAMETER RECORD NOT FOUND!" TO MESS.
           PERFORM SEND-MESS.
       RECORD-DELETED.
           MOVE "RECORD WAS DELETED!" TO MESS.
           PERFORM SEND-MESS.
       RECORD-CHANGED.
           MOVE "CHANGES WERE MADE TO THIS RECORD!" TO MESS.
           PERFORM SEND-MESS.
       CHANGE-AGAIN.
           MOVE "PLEASE RE-INPUT YOUR CHANGES!" TO MESS.
           PERFORM SEND-MESS.
       ALREADY-CREATED.
           MOVE "RECORD HAS BEEN ALREADY CREATED!" TO MESS.
           PERFORM SEND-MESS.
       UNDERGONE-CHANGE.
           MOVE
           "THIS RECORD HAS UNDERGONE CHANGE BY ANOTHER USER,"
           TO MESS.
           PERFORM SEND-MESS.

      ****************************************
       IO-ROUTINES SECTION.
      ****************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-GP-FILE.
       COPY "LIBGB/GBGPGS_SQL.CPY".
       COPY "LIBGB/GBGPIN.CPY".
       COPY "LIBGB/FERRORS.CPY".
            MOVE "E" TO ERRCD.

      ****************************************
       END-ROUTINE SECTION.
      ****************************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY GPOPM2-SCREEN.
           MOVE FORM-PATH TO FORM-PATHNAME.
           MOVE GPOPMN-LINK TO FORM-LINK-AREA.
           EXIT PROGRAM.
