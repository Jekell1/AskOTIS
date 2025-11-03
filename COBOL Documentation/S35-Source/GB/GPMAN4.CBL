       IDENTIFICATION DIVISION.
       PROGRAM-ID.      GPMAN4.
       DATE-WRITTEN.    102405.
      ******************************************************************
      * DIR : GB
      * DESC: GLOBAL PARAMETER MAINT - PAGE 4
      *
      * REV:
      * BLF 051024 ADD GP-INCLUDE-O2-IN-BAL, PR# 2623
      * BLF 060920 ADD GP-AUTO-BANKRUPT-WINDOW, WORLD PR# 493
      *
      * KEC 060921 {PR#00149} REGIONAL ACCEPTANCE <A15> [KEVIN MULLEN]
      *            ADDED 'GP-BW-SCORES-QUARTER-SUB'
      *  CS 061005 ADDED 'GP-POST-XX-BF-PAYMENTS' [PR#2659] SERVICE FINANCE
      * BAH 061113 ADDED GP-VALIDATE-ENDORSER-AGE COLTN PL# 558
      * BAH 070104 ADDED GP-ALLOW-MANUAL-CHECKS WORLD PR# 513
      * BLF 070323 ADDED FIELD 7, GP-NEXT-EMPLOYEE-NO, WORLD PR# 488
      * BLF 070411 ADDED FIELD 8, GP-USE-RESULT-WORKED-FG, SUPPORT PL# 584
      *  CS 070625 ADDED FIELD 9, GP-MAX-PROMISE-DAYS, TRITON PR#2992
      * BLF 080111 ADDED FIELD 10, GP-EDIT-BORROWER-NAME-FG, WORLD PR# 564
      * BAH 080212 ADDED FIELD 11, GP-RECAP-CLASS-TOTALS, HAMPTON PR# 3150
      *  CS 080623 ADDED FIELD 12, GP-BRING-AUTO-INS-ON-RENEWAL PR#3177 CITIZENS
      * BAH 090805 ADDED FIELD 13, GP-USE-COLLECT-LETTER-DAYS PR# 251 REGACC
      * BAH 091005 ADDED FIELD 14, GP-DISPLAY-BAD-ADR-FLAG PR# 256 REGACC
      * BAH 091209 ADDED FIELD 15, GP-DAYS-IDENT-NEW-PAPER PR# 262 REGACC
      * BAH 130521 ADDED FIELD 16, GP-DUE-29-30-31 PR# 4239 SOAUTO
      * BAH 141008 ADDED FIELD 17, GP-DEFAULT-SKIP-TRW PR#4629 COSMO
      *  CS 150602 ADDED FIELD 18, GP-EARLIEST-OPEN-TIME PR#369 REGACC
      * BAH 150810 ADDED FIELD 19, GP-TRANSFER-CONPROFILE, REGMGM PR# 4799
      * BLM 160418 ADDED FIELD 20, GP-BATCH-REFCD-DUP-DAYS, SUPPORT PR# 5048
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
           "@(#)GB/GPMAN4.CBL S35 02/22/24-14:42:20 >".

       COPY "LIBGB/GB01GP.CPY".
       COPY "LIBGB/GB01GP_SQL.CPY".
       COPY "LIBGB/GBWSGP.CPY".

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

       01  GPMAIN-LINK.
          03  SAVEKEY              PIC 99.
          03  NEW-REC              PIC X.
          03  ERRCD                PIC X.
          03  FORMNUM              PIC 9.
          03  GPMAIN-WINDOW-HANDLE PIC X(10).


      ******************************************************************
      *    S C R E E N    D I S P L A Y    B U F F E R
      ******************************************************************
       01  DISPLAY-BUF     VALUE SPACES.
           03  F01         PIC X(01).
           03  F02         PIC X(01).
           03  F03         PIC 9(01).
           03  F04         PIC X(01).
           03  F05         PIC X(01).
           03  F06         PIC X(01).
           03  F07         PIC ZZZ999.
           03  F08         PIC X(01).
           03  F09         PIC 9(2).
           03  F10         PIC X(01).
           03  F11         PIC X(01).
           03  F12         PIC X(01).
           03  F13         PIC X(01).
           03  F14         PIC X(01).
           03  F15         PIC ZZ9.
           03  F16         PIC X.
           03  F17         PIC X.
           03  F18         PIC 9(4).
           03  F19         PIC X.
           03  F20         PIC 9(3).
      *-----------------------------------------------------------------
       01  DISPLAY-LIST.
           03  FILLER      PIC X(56) VALUE
           "010 020 030 040 050 060 070 080 090 100 110 120 130 140 ".
           03  FILLER      PIC X(24) VALUE
           "150 160 170 180 190 200.".

      ******************************************************************
      *    E L E M E N T    S P E C I F I C A T I O N S
      ******************************************************************
       78  MAX                      VALUE 20.
       01  SPEC-TABLE.
      * 01. ACCRUAL EARNINGS ON ACCOUNTS IN REPO STATUS
           03  FILLER      PIC X(8)    VALUE "E NA010 ".
      * 02.  GP-AUTO-BANKRUPT-WINDOW
           03  FILLER      PIC X(8)    VALUE "E NA010 ".
      * 03.  GP-BW-SCORES-QUARTER-SUB
           03  FILLER      PIC X(8)    VALUE "ENNN010 ".
      * 04.  GP-POST-XX-BF-PAYMENTS
           03  FILLER      PIC X(8)    VALUE "E  A010 ".
      * 05.  GP-VALIDATE-ENDORSER-AGE
           03  FILLER      PIC X(8)    VALUE "E  A010 ".
      * 06.  GP-ALLWO-MANUAL-CHECKS
           03  FILLER      PIC X(8)    VALUE "E  A010 ".
      * 07.  GP-NEXT-EMPLOYEE-NO
           03  FILLER      PIC X(8)    VALUE "ENNN060 ".
      * 08.  GP-USE-RESULT-WORKED-FG
           03  FILLER      PIC X(8)    VALUE "E  A010 ".
      * 09.  GP-MAX-PROMISE-DAYS
           03  FILLER      PIC X(8)    VALUE "ENNN020 ".
      * 10.  GP-EDIT-BORROWER-NAME-FG
           03  FILLER      PIC X(8)    VALUE "E  A010 ".
      * 11.  GP-RECAP-CLASS-TOTALS
           03  FILLER      PIC X(8)    VALUE "E  A010 ".
      * 12.  GP-BRING-AUTO-INS-ON-RENEWAL
           03  FILLER      PIC X(8)    VALUE "E  A010 ".
      * 13.  GP-USE-MIN-FORM-SENT-DAYS
           03  FILLER      PIC X(8)    VALUE "E  A010 ".
      * 14.  GP-DISPLAY-BAD-ADR-FLAG
           03  FILLER      PIC X(8)    VALUE "E  A010 ".
      * 15.  GP-DAYS-IDENT-NEW-PAPER
           03  FILLER      PIC X(8)    VALUE "ENNN030 ".
      * 16.  GP-DUE-29-30-31
           03  FILLER      PIC X(8)    VALUE "E  A010 ".
      * 17.  GP-DEFAULT-SKIP-TRW
           03  FILLER      PIC X(8)    VALUE "E  A010 ".
      * 18.  GP-EARLIEST-OPEN-TIME     
           03  FILLER      PIC X(8)    VALUE "ENNN040 ".
      * 19.  GP-TRANSFER-CONPROFILE
           03  FILLER      PIC X(8)    VALUE "E  A010 ".
      * 20.  GP-BATCH-REFCD-DUP-DAYS
           03  FILLER      PIC X(8)    VALUE "ENNN030 ".

       01  SPEC-TABLE2     REDEFINES SPEC-TABLE.
           02  SPEC-REC    OCCURS MAX TIMES.
               05  SPEC    PIC X(7).
               05  CHFG    PIC X.

       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBGB/GPMAN4_DEF.CPY".
       COPY "LIBGB/GPMAN4_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/ACCESSW.CPY".

       COPY "LIBGB/GETFMW.CPY".
       01 FORM-LINK-AREA    PIC X(2000).
       SCREEN SECTION.
       COPY "LIBGB/GPMAN4_SCN.CPY".

      ******************************************************************
      *     P R O C E D U R E    D I V I S I O N
      ******************************************************************
       PROCEDURE DIVISION
            USING FORM-PATHNAME FORM-LINK-AREA EXIT-PATHNAME.

      ******************************************************************
      *     M A I N     P R O G R A M      M O D U L E
      ******************************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "GPMAN4" TO VDU-FORM-NAME.
           MOVE FORM-LINK-AREA TO GPMAIN-LINK.

      D    STOP MESS.
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
              MOVE 5 TO FORM-SEL.

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
           MOVE SPACES TO IO-TYPE.

           IF SAVEKEY NOT = "  "
              MOVE SAVEKEY TO GP-KEY
              MOVE "  " TO SAVEKEY
              GO TO READ-GLOBAL.

           MOVE 0 TO NEW-FLAG.
           MOVE 1 TO GP-KEY.

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
              IF NOT (FORM-SEL = 1 OR 2 OR 3)
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
           GO TO F-01 F-02 F-03 F-04 F-05 F-06 F-07 F-08 F-09 F-10
                 F-11 F-12 F-13 F-14 F-15 F-16 F-17 F-18 F-19 F-20
               DEPENDING ON ELE.

           GO TO END-ENTRY-MODULE.

      *=================================================================
      *
      *    SET RECORD ELEMENTS
      *
      *=================================================================
       F-01.
           MOVE VDUBUF TO GP-INCLUDE-O2-IN-BAL.

           IF NOT (GP-INCLUDE-O2-IN-BAL = "Y" OR "N")
              MOVE "ENTER 'Y' OR 'N'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-02.
           MOVE VDUBUF TO GP-AUTO-BANKRUPT-WINDOW.

           IF NOT (GP-AUTO-BANKRUPT-WINDOW = "Y" OR "N")
              MOVE "ENTER 'Y' OR 'N'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-03.
           MOVE VDUNUM0 TO GP-BW-SCORES-QUARTER-SUB.

           IF NOT ( GP-BW-SCORES-QUARTER-SUB = 1 OR 2 OR 3 OR 4 )
              MOVE "ENTER 1, 2, 3 OR 4" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-04.
           MOVE VDUBUF TO GP-POST-XX-BF-PAYMENTS.

           IF NOT (GP-POST-XX-BF-PAYMENTS = "Y" OR "N")
              MOVE "ENTER 'Y' OR 'N'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.


       F-05.
           MOVE VDUBUF TO GP-VALIDATE-ENDORSER-AGE.

           IF NOT (GP-VALIDATE-ENDORSER-AGE = "Y" OR "N")
              MOVE "ENTER 'Y' OR 'N'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

       F-06.
           MOVE VDUBUF TO GP-ALLOW-MANUAL-CHECKS.

           IF NOT (GP-ALLOW-MANUAL-CHECKS = "Y" OR "N")
              MOVE "ENTER 'Y' OR 'N'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

       F-07.
           MOVE VDUNUM0 TO GP-NEXT-EMPLOYEE-NO.

           GO TO CREATE-ROUTINE.

       F-08.
           MOVE VDUBUF TO GP-USE-RESULT-WORKED-FG.

           IF NOT (GP-USE-RESULT-WORKED-FG = "Y" OR "N")
              MOVE "ENTER 'Y' OR 'N'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.


       F-09.
           MOVE VDUNUM0 TO GP-MAX-PROMISE-DAYS.

           GO TO CREATE-ROUTINE.

       F-10.
           MOVE VDUBUF TO GP-EDIT-BORROWER-NAME-FG.

           IF NOT (GP-EDIT-BORROWER-NAME-FG = "Y" OR "N")
              MOVE "ENTER 'Y' OR 'N'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.


       F-11.
           MOVE VDUBUF TO GP-RECAP-CLASS-TOTALS.

           IF NOT (GP-RECAP-CLASS-TOTALS = "Y" OR "N")
              MOVE "ENTER 'Y' OR 'N'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

       F-12.
           MOVE VDUBUF TO GP-BRING-AUTO-INS-ON-RENEWAL.

           IF NOT (GP-BRING-AUTO-INS-ON-RENEWAL = "Y" OR "N")
              MOVE "ENTER 'Y' OR 'N'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

       F-13.
           MOVE VDUBUF TO GP-USE-MIN-FORM-SENT-DAYS.

           IF NOT (GP-USE-MIN-FORM-SENT-DAYS = "Y" OR "N")
              MOVE "ENTER 'Y' OR 'N'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

       F-14.
           MOVE VDUBUF TO GP-DISPLAY-BAD-ADR-FLAG.

           IF NOT (GP-DISPLAY-BAD-ADR-FLAG = "Y" OR "N")
              MOVE "ENTER 'Y' OR 'N'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

       F-15.
           MOVE VDUNUM0 TO GP-DAYS-IDENT-NEW-PAPER.

           GO TO CREATE-ROUTINE.

       F-16.
           MOVE VDUBUF TO GP-DUE-29-30-31.

           IF NOT (GP-DUE-29-30-31 = "Y" OR "N")
              MOVE "ENTER 'Y' OR 'N'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

       F-17.
           MOVE VDUBUF TO GP-DEFAULT-SKIP-TRW.

           IF NOT (GP-DEFAULT-SKIP-TRW = "Y" OR "N")
              MOVE "ENTER 'Y' OR 'N'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

       F-18.
           MOVE VDUNUM0 TO GP-EARLIEST-OPEN-TIME.
           IF GP-EARLIEST-OPEN-TIME > 2399
              MOVE "ENTER A MILITIARY TIME 0-2399!" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.

       F-19.
           MOVE VDUBUF TO GP-TRANSFER-CONPROFILE.

           IF NOT (GP-TRANSFER-CONPROFILE = "Y" OR "N")
              MOVE "ENTER 'Y' OR 'N'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

       F-20.
           MOVE VDUNUM0 TO GP-BATCH-REFCD-DUP-DAYS.
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

           IF GPOR-INCLUDE-O2-IN-BAL NOT = GPCH-INCLUDE-O2-IN-BAL
              MOVE "01 "                   TO LMC-FIELD
              MOVE "O2 IN BALANCE "        TO LMC-DESC
              MOVE GPOR-INCLUDE-O2-IN-BAL      TO LMC-ORG-TEXT
              MOVE GPCH-INCLUDE-O2-IN-BAL      TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-AUTO-BANKRUPT-WINDOW NOT = GPCH-AUTO-BANKRUPT-WINDOW
              MOVE "02 "                     TO LMC-FIELD
              MOVE "AUTO BANKRPT  "          TO LMC-DESC
              MOVE GPOR-AUTO-BANKRUPT-WINDOW    TO LMC-ORG-TEXT
              MOVE GPCH-AUTO-BANKRUPT-WINDOW    TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-BW-SCORES-QUARTER-SUB NOT =
                                         GPCH-BW-SCORES-QUARTER-SUB
              MOVE "03 "                     TO LMC-FIELD
              MOVE "BW SCORE PTR  "          TO LMC-DESC
              MOVE GPOR-BW-SCORES-QUARTER-SUB    TO LMC-ORG-TEXT
              MOVE GPCH-BW-SCORES-QUARTER-SUB    TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-POST-XX-BF-PAYMENTS NOT = GPCH-POST-XX-BF-PAYMENTS
              MOVE "04 "                   TO LMC-FIELD
              MOVE "XX CONV PMTS  "        TO LMC-DESC
              MOVE GPOR-POST-XX-BF-PAYMENTS      TO LMC-ORG-TEXT
              MOVE GPCH-POST-XX-BF-PAYMENTS      TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-VALIDATE-ENDORSER-AGE NOT =
                                          GPCH-VALIDATE-ENDORSER-AGE
              MOVE "05 "                   TO LMC-FIELD
              MOVE "VALID END AGE "        TO LMC-DESC
              MOVE GPOR-VALIDATE-ENDORSER-AGE      TO LMC-ORG-TEXT
              MOVE GPCH-VALIDATE-ENDORSER-AGE      TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-ALLOW-MANUAL-CHECKS NOT = GPCH-ALLOW-MANUAL-CHECKS
              MOVE "06 "                   TO LMC-FIELD
              MOVE "ALLOW MAN CHKS"        TO LMC-DESC
              MOVE GPOR-ALLOW-MANUAL-CHECKS      TO LMC-ORG-TEXT
              MOVE GPCH-ALLOW-MANUAL-CHECKS      TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-NEXT-EMPLOYEE-NO NOT = GPCH-NEXT-EMPLOYEE-NO
              MOVE "07 "                   TO LMC-FIELD
              MOVE "NEXT EMPLOYEE#"        TO LMC-DESC
              MOVE GPOR-NEXT-EMPLOYEE-NO      TO LMC-ORG-NUM
              MOVE GPCH-NEXT-EMPLOYEE-NO      TO LMC-CHG-NUM
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-USE-RESULT-WORKED-FG NOT = GPCH-USE-RESULT-WORKED-FG
              MOVE "08 "                   TO LMC-FIELD
              MOVE "RESULT WORKED "        TO LMC-DESC
              MOVE GPOR-USE-RESULT-WORKED-FG      TO LMC-ORG-TEXT
              MOVE GPCH-USE-RESULT-WORKED-FG      TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-MAX-PROMISE-DAYS NOT = GPCH-MAX-PROMISE-DAYS
              MOVE "09 "                   TO LMC-FIELD
              MOVE "MAX PROM DAYS "        TO LMC-DESC
              MOVE GPOR-MAX-PROMISE-DAYS      TO LMC-ORG-NUM
              MOVE GPCH-MAX-PROMISE-DAYS      TO LMC-CHG-NUM
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-EDIT-BORROWER-NAME-FG NOT =
                                       GPCH-EDIT-BORROWER-NAME-FG
              MOVE "10 "                   TO LMC-FIELD
              MOVE "EDIT BW NAME  "        TO LMC-DESC
              MOVE GPOR-EDIT-BORROWER-NAME-FG      TO LMC-ORG-TEXT
              MOVE GPCH-EDIT-BORROWER-NAME-FG      TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-RECAP-CLASS-TOTALS NOT = GPCH-RECAP-CLASS-TOTALS
              MOVE "11 "                   TO LMC-FIELD
              MOVE "RECAP CLASSTOT"        TO LMC-DESC
              MOVE GPOR-RECAP-CLASS-TOTALS      TO LMC-ORG-TEXT
              MOVE GPCH-RECAP-CLASS-TOTALS      TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-BRING-AUTO-INS-ON-RENEWAL NOT =
                                      GPCH-BRING-AUTO-INS-ON-RENEWAL
              MOVE "12 "                   TO LMC-FIELD
              MOVE "AUTOINS RENEWL"        TO LMC-DESC
              MOVE GPOR-BRING-AUTO-INS-ON-RENEWAL      TO LMC-ORG-TEXT
              MOVE GPCH-BRING-AUTO-INS-ON-RENEWAL      TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-USE-MIN-FORM-SENT-DAYS NOT =
                                        GPCH-USE-MIN-FORM-SENT-DAYS
              MOVE "13 "                   TO LMC-FIELD
              MOVE "COLL LETTR DAY"        TO LMC-DESC
              MOVE GPOR-USE-MIN-FORM-SENT-DAYS      TO LMC-ORG-TEXT
              MOVE GPCH-USE-MIN-FORM-SENT-DAYS      TO LMC-CHG-TEXT
           IF GPOR-DISPLAY-BAD-ADR-FLAG NOT = GPCH-DISPLAY-BAD-ADR-FLAG
              MOVE "14 "                   TO LMC-FIELD
              MOVE "DISP BADADR FG"        TO LMC-DESC
              MOVE GPOR-DISPLAY-BAD-ADR-FLAG      TO LMC-ORG-TEXT
              MOVE GPCH-DISPLAY-BAD-ADR-FLAG      TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-DAYS-IDENT-NEW-PAPER NOT = GPCH-DAYS-IDENT-NEW-PAPER
              MOVE "15 "                   TO LMC-FIELD
              MOVE "DAYS NEW PAPER"        TO LMC-DESC
              MOVE GPOR-DAYS-IDENT-NEW-PAPER      TO LMC-ORG-NUM
              MOVE GPCH-DAYS-IDENT-NEW-PAPER      TO LMC-CHG-NUM
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-DUE-29-30-31 NOT = GPCH-DUE-29-30-31
              MOVE "16 "                   TO LMC-FIELD
              MOVE "DUE 29,30,31  "        TO LMC-DESC
              MOVE GPOR-DUE-29-30-31      TO LMC-ORG-TEXT
              MOVE GPCH-DUE-29-30-31      TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-DEFAULT-SKIP-TRW NOT = GPCH-DEFAULT-SKIP-TRW
              MOVE "17 "                   TO LMC-FIELD
              MOVE "SKIP TRW FLAG "        TO LMC-DESC
              MOVE GPOR-DEFAULT-SKIP-TRW      TO LMC-ORG-TEXT
              MOVE GPCH-DEFAULT-SKIP-TRW      TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-EARLIEST-OPEN-TIME NOT = GPCH-EARLIEST-OPEN-TIME
              MOVE "18 "                   TO LMC-FIELD
              MOVE "EARLY OPEN DAY"        TO LMC-DESC
              MOVE GPOR-EARLIEST-OPEN-TIME      TO LMC-ORG-NUM
              MOVE GPCH-EARLIEST-OPEN-TIME      TO LMC-CHG-NUM
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-TRANSFER-CONPROFILE NOT = GPCH-TRANSFER-CONPROFILE
              MOVE "19 "                   TO LMC-FIELD
              MOVE "TRANSFER CPROF"        TO LMC-DESC
              MOVE GPOR-TRANSFER-CONPROFILE      TO LMC-ORG-TEXT
              MOVE GPCH-TRANSFER-CONPROFILE      TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-BATCH-REFCD-DUP-DAYS NOT = GPCH-BATCH-REFCD-DUP-DAYS
              MOVE "20 "                   TO LMC-FIELD
              MOVE "DUP REFCD DAYS"        TO LMC-DESC
              MOVE GPOR-BATCH-REFCD-DUP-DAYS      TO LMC-ORG-NUM
              MOVE GPCH-BATCH-REFCD-DUP-DAYS      TO LMC-CHG-NUM
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

           MOVE GP-INCLUDE-O2-IN-BAL         TO F01.
           MOVE GP-AUTO-BANKRUPT-WINDOW      TO F02.
           MOVE GP-BW-SCORES-QUARTER-SUB     TO F03.
           MOVE GP-POST-XX-BF-PAYMENTS       TO F04.
           MOVE GP-VALIDATE-ENDORSER-AGE     TO F05.
           MOVE GP-ALLOW-MANUAL-CHECKS       TO F06.
           MOVE GP-NEXT-EMPLOYEE-NO          TO F07.
           MOVE GP-USE-RESULT-WORKED-FG      TO F08.
           MOVE GP-MAX-PROMISE-DAYS          TO F09.
           MOVE GP-EDIT-BORROWER-NAME-FG     TO F10.
           MOVE GP-RECAP-CLASS-TOTALS        TO F11.
           MOVE GP-BRING-AUTO-INS-ON-RENEWAL TO F12.
           MOVE GP-USE-MIN-FORM-SENT-DAYS    TO F13.
           MOVE GP-DISPLAY-BAD-ADR-FLAG      TO F14.
           MOVE GP-DAYS-IDENT-NEW-PAPER      TO F15.
           IF GP-DAYS-IDENT-NEW-PAPER NOT NUMERIC
              MOVE 0 TO F15.
           MOVE GP-DUE-29-30-31              TO F16.
           MOVE GP-DEFAULT-SKIP-TRW          TO F17.
           MOVE GP-EARLIEST-OPEN-TIME        TO F18.
           MOVE GP-TRANSFER-CONPROFILE       TO F19.
           MOVE GP-BATCH-REFCD-DUP-DAYS      TO F20.
           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       DISPLAY-FIELDS-EXIT.
           EXIT.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".

      ***************************************************
       FORM-RESET SECTION.
      ***************************************************
           MOVE EXT-CONAME-CONAME TO GPMAN4-CONAME.
           MOVE EXT-CONAME-SYSDATE TO GPMAN4-SYSDATE.
           DISPLAY GPMAN4-SCREEN UPON GPMAIN-WINDOW-HANDLE.
           MOVE GPMAN4-FORM-FIELDS TO WR-BUF.
           MOVE GPMAN4-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM. 

       FORM-RESET-EXIT.
           EXIT.

      ***************************************************
       SETUP-LINK-INFO SECTION.
      ***************************************************
           MOVE GPMAN4-HELP--FILE TO HELP-LINK-FILE.
           MOVE GPMAN4-HELP--DIR TO HELP-LINK-DIR.

      ***************************************************
       VDU-CONVERT-FIELD SECTION.
      ***************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBGB/GPMAN4_EVA.CPY".
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
           MOVE " THIS RECORD HAS UNDERGONE CHANGE BY ANOTHER USER,"
                                                  TO MESS.
           PERFORM SEND-MESS.

      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-GP-FILE.
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
           DESTROY GPMAN4-SCREEN.
           MOVE FORM-PATH TO FORM-PATHNAME.
           MOVE GPMAIN-LINK TO FORM-LINK-AREA.
           EXIT PROGRAM.
