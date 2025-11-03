       IDENTIFICATION DIVISION.
       PROGRAM-ID.      GPMAN1.
       DATE-WRITTEN.    070395.
      ******************************************************************
      *  DIR : GB
      *  DESC: GLOBAL PARAMETER MAINT
      *        RELATIVE RECORD 1
      *
      * REV:
      *  MJD 070695 ADDED GP-FED-TAX-ID.
      *  BLL 071495 ADDED GP-CORP-NAME, GP-SHORT-NAME
      *  JTG 072095 ADDED GP-BANK-DEPOSIT-GL
      *  JTG 072595 ADDED GP-AGE-BUCKET-CD LOGIC
      *  JTG 081095 ADDED GP-THE-ONLY-DF-SRCD
      *  BAH 081495 ADDED GP-ALLOW-BATCH-CHECKS
      *  MJD 082495 ADDED GP-CASH-METHOD-ONLY
      *                   GP-IB-ACCRUALS-DONE.
      *  BLL 082895 ADDED GP-SPL-RETAIN-MONTHS
      *  KEC 082895 ADDED GP-NO-LC-IN-ARREARAGE-FG
      *  JTG 092995 ADDED GP-DEFAULT-O3-INS-COMPANY
      *  MJD 121195 ADDED GP-STOP-ON-PRIOR-FG.
      *  BAH 032696 ADDED PAGE 2
      *  MJD 050196 CHANGED INIT VALUE OF GP-KEY FROM 2 TO 1.
      *  BLV 101496 FIXED SIZE OF ORIG & CHANGED REC
      *  GLF 050297 CHANGED GP-SPL-RETAIN-MONTHS TO GP-EOD-RETAIN-MONTHS
      *  MJD 110398 ADDED GP-NEXT-NOTE
      *  MJD 990105 ADDED GP-NEXT-BATCH-ID
      *  MJD 990412 ADDED GP-BP-NEXT-BATCH.
      *   CS 990415 **IN PROCESS ADDING PAGE 3
      *  MJD 990421 ADDED GP-BP-NEXT-BATCH TO CLEAR ROUTINE.
      *  MJD 990720 ADDED GPAMT-CASH-PROCEEDS TO CLEAR ROUTINE.
      *  MG  991006 FIXED VDUSTATUS/IO-FG/STAT PER DEVELOPMENT MEETING.
      *  MJD 000420 ADDED GP-LB-NEXT-BATCH. (MERC PR#227)
      *  MJD 000822 ADDED GP-CALL-START-TIME AND GP-CALL-END-TIME TO
      *             CLEAR ROUTINE.  (TFC PR#25)
      * JTG 010530 ADDED GP-TRUE-NEXT-BATCH           (MERC PR#243/251)
      * JTG 011109 ADDED MONEYGRAM OPTION GP-MGRAM-NEXT-BATCH MERC PR#263
      * BAH 020515 ADDED GP-CORP-BRNO FOR REGACC PR# 1800
      * JTG 021011 ADDED NEW CODE FILE TYPE 'BR' - BATCH PAYMENT REFERENCE CODES
      *            BATCH PAYMENT REVISIONS ADDED GP-REFCD-NEXT-BATCHREGACC #0006
      * BAH 031120 ADDED GP-CHBORR-PAST-DUE-DAYS TO CLEAR RECORD, WORLD #337
      * JTG 031215 CHANGED GL UPDATE TO TEST NEW GLOBAL PARAMETER FLAGS TO
      *            DETERMINE IF GL FISCAL PERIOD HAS BEEN CLOSED.
      *            IF CLOSED STOP UPDATE.                            WORLD #J040
      * JTG 040407 ADDED GP-OTREF-NEXT-BATCH  900-909              CENTRIX #2254
      * BLF 051024 ADDED PAGE 4, PR# 2623
      * AJM 120926 ADDED LOGIC POPULATE PERIOD END DATES USING FISCAL FIRST DAY
      * BLM 131217 WHEN ALAN DID THE CHANGE ABOVE, SOMEHOW THERE WAS A
      *            "GO TO CREATE-ROUTINE" IN THE WRONG PLACE SO YOU COULD
      *            NOT GET TO THE CLOSE FLAG FOR FISCAL PERIOD 1, FIXED
      *            PL# 829
      * BLM 170313 FIX ENTRY & DISPLAY OF FISCAL DATES FOR 8 DIGIT DATES, PD0006
      * BAH 180507 ADDED ERRLOG
      * KEC 2019-0123 A32 - GPFILE GP-REC MODIFICATION 
      *               ADDED PAGE 5 (GB/GPMAN5) FOR ACCOUNTS PAYABLE (AP)
      * BAH 20190723 ADDED CLEAR-GP-FILE GBGPCL
      * BLM 20211022 FIX GL ACCT ENTRY SO YOU CAN ENTER OR NOT ENTER DASH OR
      *              LEADING ZEROES
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
           "@(#)GB/GPMAN1.CBL S35 02/22/24-14:42:56 >".
       COPY "LIBGB/GB01GP.CPY".
       COPY "LIBGB/GB01GP_SQL.CPY".
       COPY "LIBGB/GBWSGP.CPY".
       COPY "LIBGL/GL01GL.CPY".
       COPY "LIBGL/GL01GL_SQL.CPY".
       COPY "LIBGL/GLWSGL.CPY".

      ******************************************************************
      *    P R O G R A M     W O R K E R S
      ******************************************************************
       01  SUB             PIC 999     VALUE 0.
       01  SUB-DT          PIC 999     VALUE 0.
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

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * WORKERS FOR VERIFY-CHECK-DIGIT SECTION.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  TMP-BANK-ROUTING-NO   PIC 9(9).
       01  TMP-BANK-ROUT-NO    REDEFINES TMP-BANK-ROUTING-NO.
           03  TMP-RT-NO         PIC 9   OCCURS 9.

       01  WEIGHTING-FACTORS     PIC 9(8)  VALUE 37137137.
       01  WEIGHT-TABLEX       REDEFINES WEIGHTING-FACTORS.
           03 WEIGHT-TABLE       PIC 9    OCCURS 8.

       01  CK-SUB                PIC 9.
       01  TMP-RESULT            PIC 9(3)  VALUE 0.
       01  TMP-SUM               PIC 9(3)  VALUE 0.
       01  CHECK-SUM             PIC 9(3)  VALUE 0.
       01  CHECK-BASE            PIC 9(2)  VALUE 0.
       01  CHECK-MODIFIER        PIC 9     VALUE 0.
       01  WEIGHT-FACTOR         PIC 9(3)  VALUE 0.
       01  CHK-DIG-FG            PIC X     VALUE "Y".

      ******************************************************************
      *    C O N A M E
      ******************************************************************
       01 GPMAIN-LINK.
          03 SAVEKEY              PIC 99.
          03 NEW-REC              PIC X.
          03 ERRCD                PIC X.
          03 FORMNUM              PIC 9.
          03 GPMAIN-WINDOW-HANDLE PIC X(10).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  WORKERS FOR CHECK-GLNO SECTION.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  SCREEN-GL.
           03  SC-BRANCH         PIC 9(4).
           03  SC-DASH           PIC X.
           03  SC-NUMBER         PIC 9(7).
       01  SCREEN-GL-NUM REDEFINES SCREEN-GL PIC 9(12).
       01  WORK-GL               PIC 9(11).
       01  WORK-NUMBER  REDEFINES WORK-GL.
           03  WORK-BR           PIC 9(4).
           03  WORK-NO           PIC 9(7).
       01  DISP-GL.
           03  DISP-BR           PIC 9(4).
           03  FILLER            PIC X     VALUE "-".
           03  DISP-NO           PIC 9(7).

      ******************************************************************
      *    S C R E E N    D I S P L A Y    B U F F E R
      ******************************************************************
       01  DISPLAY-BUF     VALUE SPACES.
           03  F01         PIC 99/99/99     BLANK ZERO.
           03  DISPLAY-BUF-12-END-DATES.
               05  F02         PIC 99/99/99     BLANK ZERO.
               05  F021        PIC X.
               05  F03         PIC 99/99/99     BLANK ZERO.
               05  F031        PIC X.
               05  F04         PIC 99/99/99     BLANK ZERO.
               05  F041        PIC X.
               05  F05         PIC 99/99/99     BLANK ZERO.
               05  F051        PIC X.
               05  F06         PIC 99/99/99     BLANK ZERO.
               05  F061        PIC X.
               05  F07         PIC 99/99/99     BLANK ZERO.
               05  F071        PIC X.
               05  F08         PIC 99/99/99     BLANK ZERO.
               05  F081        PIC X.
               05  F09         PIC 99/99/99     BLANK ZERO.
               05  F091        PIC X.
               05  F10         PIC 99/99/99     BLANK ZERO.
               05  F101        PIC X.
               05  F11         PIC 99/99/99     BLANK ZERO.
               05  F111        PIC X.
               05  F12         PIC 99/99/99     BLANK ZERO.
               05  F121        PIC X.
               05  F13         PIC 99/99/99     BLANK ZERO.
               05  F131        PIC X.

      * BANK GL
           03  F14         PIC 9999/9999999 BLANK ZERO.
           03  F15         PIC 9999/9999999 BLANK ZERO.
           03  F16         PIC 9999/9999999 BLANK ZERO.
           03  F17         PIC Z(9)         BLANK ZERO.
      * BANK ACCOUNT NO.
           03  F18         PIC Z(15)        BLANK ZERO.

      * FEDERAL TAX ID
           03  F19         PIC Z(9)         BLANK ZERO.
      * CORP BRNO
           03  F20         PIC 9(4).
      * CORP NAME
           03  F201        PIC X(25).
      * SHORT NAME
           03  F21         PIC X(3).
      * BANK NAME
           03  F22         PIC X(23).
      * RETAIN MONTHS TR
           03  F23         PIC ZZ           BLANK ZERO.
      * RETAIN MONTHS EOD
           03  F24         PIC ZZ           BLANK ZERO.
      * NEXT NOTE NUMBER
           03  F25         PIC Z(6)         BLANK ZERO.
      * NEXT BATCH ID
           03  F26         PIC Z(5)         BLANK ZERO.
      * NEXT BP BATCH ID
           03  F27         PIC Z(3)         BLANK ZERO.
      * NEXT LB BATCH ID
           03  F28         PIC Z(3)         BLANK ZERO.
      * NEXT TRUE BATCH ID
           03  F29         PIC Z(3)         BLANK ZERO.
      * NEXT MGRAM BATCH ID
           03  F30         PIC Z(3)         BLANK ZERO.
      * NEXT REFCD BATCH ID
           03  F31         PIC Z(3)         BLANK ZERO.
      * NEXT OTREF BATCH ID
           03  F32         PIC Z(3)         BLANK ZERO.
      *-----------------------------------------------------------------
       01  DISPLAY-LIST.
           03  FILLER      PIC X(52) VALUE
           "010 020 021 030 031 040 041 050 051 060 061 070 071 ".
           03  FILLER      PIC X(52) VALUE
           "080 081 090 091 100 101 110 111 120 121 130 131 140 ".
           03  FILLER      PIC X(52) VALUE
           "150 160 170 180 190 200 201 210 220 230 240 250 260 ".
           03  FILLER      PIC X(24) VALUE
           "270 280 290 300 310 320.".

       01  DISPLAY-LIST-12-END-DATES.
           03  FILLER      PIC X(48) VALUE
           "020 021 030 031 040 041 050 051 060 061 070 071 ".
           03  FILLER      PIC X(48) VALUE
           "080 081 090 091 100 101 110 111 120 121 130 131 ".

      ******************************************************************
      *    E L E M E N T    S P E C I F I C A T I O N S
      ******************************************************************
       78  MAX                      VALUE 32.
       01  SPEC-TABLE.
      * 1. GENERAL LEDGER BEGINNING FISCAL MONTH
           03  FILLER      PIC X(8)    VALUE "ERNM060 ".
      * 2. LAST DAY OF PERIOD 1
           03  FILLER      PIC X(8)    VALUE "ERNM060 ".
      * 3. LAST DAY OF PERIOD 2
           03  FILLER      PIC X(8)    VALUE "ERNM060 ".
      * 4. LAST DAY OF PERIOD 3
           03  FILLER      PIC X(8)    VALUE "ERNM060 ".
      * 5. LAST DAY OF PERIOD 4
           03  FILLER      PIC X(8)    VALUE "ERNM060 ".
      * 6. LAST DAY OF PERIOD 5
           03  FILLER      PIC X(8)    VALUE "ERNM060 ".
      * 7. LAST DAY OF PERIOD 6
           03  FILLER      PIC X(8)    VALUE "ERNM060 ".
      * 8. LAST DAY OF PERIOD 7
           03  FILLER      PIC X(8)    VALUE "ERNM060 ".
      * 9. LAST DAY OF PERIOD 8
           03  FILLER      PIC X(8)    VALUE "ERNM060 ".
      * 10. LAST DAY OF PERIOD 9
           03  FILLER      PIC X(8)    VALUE "ERNM060 ".
      * 11. LAST DAY OF PERIOD 10
           03  FILLER      PIC X(8)    VALUE "ERNM060 ".
      * 12. LAST DAY OF PERIOD 11
           03  FILLER      PIC X(8)    VALUE "ERNM060 ".
      * 13. LAST DAY OF PERIOD 12
           03  FILLER      PIC X(8)    VALUE "ERNM060 ".
      * 14. BANK GL NO
           03  FILLER      PIC X(8)    VALUE "E RA120 ".
      * 15. BANK DEPOSIT GL NO
           03  FILLER      PIC X(8)    VALUE "E RA120 ".
      * 16. BANK CLEARING GL NO
           03  FILLER      PIC X(8)    VALUE "E RA120 ".
      * 17. BANK ROUTING NO
           03  FILLER      PIC X(8)    VALUE "ENNN090 ".
      * 18. BANK ACCOUNT NO
           03  FILLER      PIC X(8)    VALUE "ENNN150 ".
      * 19. FED TAX ID.
           03  FILLER      PIC X(8)    VALUE "ENNN090 ".
      * 20. CORPORATE BRNO
           03  FILLER      PIC X(8)    VALUE "ENNN040 ".
      * 21. SHORT NAME
           03  FILLER      PIC X(8)    VALUE "E NA030 ".
      * 22. BANK NAME
           03  FILLER      PIC X(8)    VALUE "E NA230 ".
      * 23. NO MONTHS TO RETAIN TR ( >2 )
           03  FILLER      PIC X(8)    VALUE "ENNN020 ".
      * 24. NO MONTHS TO RETAIN EODS ( >2 )
           03  FILLER      PIC X(8)    VALUE "ENNN020 ".
      * 25. NEXT NOTE NUMBER
           03  FILLER      PIC X(8)    VALUE "ENNN060 ".
      * 26. NEXT BATCH ID
           03  FILLER      PIC X(8)    VALUE "ENNN050 ".
      * 27. NEXT BP BATCH ID
           03  FILLER      PIC X(8)    VALUE "ENNN030 ".
      * 28. NEXT LB BATCH ID
           03  FILLER      PIC X(8)    VALUE "ENNN030 ".
      * 29. NEXT TRUE BATCH ID
           03  FILLER      PIC X(8)    VALUE "ENNN030 ".
      * 30. NEXT MGRAM BATCH ID
           03  FILLER      PIC X(8)    VALUE "ENNN030 ".
      * 31. NEXT REFCD BATCH ID
           03  FILLER      PIC X(8)    VALUE "ENNN030 ".
      * 32. NEXT OTREF BATCH ID
           03  FILLER      PIC X(8)    VALUE "ENNN030 ".
       01  SPEC-TABLE2     REDEFINES SPEC-TABLE.
           02  SPEC-REC    OCCURS MAX TIMES.
               05  SPEC    PIC X(7).
               05  CHFG    PIC X.
      ******************************************************************
      *
      *     C O P Y     L I B R A R Y     W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBGB/GPMAN1_DEF.CPY".
       COPY "LIBGB/GPMAN1_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/ACCESSW.CPY".

      ******************************************************************
      *
      *     P R O C E D U R E    D I V I S I O N
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       01 FORM-LINK-AREA    PIC X(2000).
       SCREEN SECTION.
       COPY "LIBGB/GPMAN1_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME FORM-LINK-AREA EXIT-PATHNAME.

      ******************************************************************
      *
      *     M A I N     P R O G R A M      M O D U L E
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.
           PERFORM SQL-CONNECT.
       GET-FORM.
      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "GPMAN1" TO VDU-FORM-NAME.
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
              MOVE 2 TO FORM-SEL.
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

       READ-GLOBAL.
           MOVE 1 TO GP-KEY.
           PERFORM OPEN-GP-FILE.
           PERFORM READ-GP-FILE.
           PERFORM CLOSE-GP-FILE.
           IF IO-GOOD
              MOVE GP-REC TO GPOR-REC
              PERFORM DISPLAY-FIELDS
              MOVE 0 TO NEW-FLAG
           ELSE
              PERFORM NEW-RECORD-SETUP.

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
      **************************************************
      **************************************************
       
           IF ACTION = "DELETE"
              PERFORM OPEN-GP-FILE
              PERFORM DELETE-GP-FILE
              PERFORM CLOSE-GP-FILE
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

      **************************************************
      **************************************************
           IF FORM-KEY = " " AND FORM-P = "P" AND FORM-SEL NUMERIC
              IF NOT (FORM-SEL = 2 OR 3 OR 4)
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

       TEST-F4.
           IF UP-KEY
             IF NEW-FLAG = 1
              MOVE -1 TO ONE
              GO TO CREATE-ROUTINE.

       TEST-F5.
           IF F5-KEY
              MOVE 1 TO ONE
              GO TO CREATE-ROUTINE.

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
                 F-29 F-30 F-31 F-32
               DEPENDING ON ELE.

           GO TO END-ENTRY-MODULE.

      *=================================================================
      *    SET RECORD ELEMENTS
      *=================================================================
       F-01.
           MOVE VDU-DATE-YYYYMMDD TO GP-BEG-FISCAL-DATE.
           MOVE GP-BEG-FISCAL-DATE TO NUM-DATE.

           IF NUM-MO > 12
              OR NUM-MO < 1
                 MOVE "PLEASE ENTER A MONTH FROM 1 TO 12" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.

      *----- GET VALID LAST DAY FOR PERIOD 1 (OF FISCAL YR)----
           MOVE GP-BEG-FISCAL-DATE TO NDTE-DATE.
           MOVE 31 TO NDTE-DD.
           PERFORM NEW-DATE-CALCULATION.
           MOVE NDTE-DATE TO GP-END-FISCAL-DATE(1).
           MOVE "N"      TO GP-CLOSED-FISCAL-FG(1).

      *----- GET VALID LAST DAY FOR PERIOD 2 THRU PERIOD 11 ---
           PERFORM GET-FISCAL-PERIOD-DATES VARYING SUB-DT FROM 2 BY 1
                                             UNTIL SUB-DT > 12.

           MOVE GP-END-FISCAL-DATE(1)   TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY             TO F02.
           MOVE GP-CLOSED-FISCAL-FG(1)  TO F021.
           MOVE GP-END-FISCAL-DATE(2)   TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY             TO F03.
           MOVE GP-CLOSED-FISCAL-FG(2)  TO F031.
           MOVE GP-END-FISCAL-DATE(3)   TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY             TO F04.
           MOVE GP-CLOSED-FISCAL-FG(3)  TO F041.
           MOVE GP-END-FISCAL-DATE(4)   TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY             TO F05.
           MOVE GP-CLOSED-FISCAL-FG(4)  TO F051.
           MOVE GP-END-FISCAL-DATE(5)   TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY             TO F06.
           MOVE GP-CLOSED-FISCAL-FG(5)  TO F061.
           MOVE GP-END-FISCAL-DATE(6)   TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY             TO F07.
           MOVE GP-CLOSED-FISCAL-FG(6)  TO F071.
           MOVE GP-END-FISCAL-DATE(7)   TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY             TO F08.
           MOVE GP-CLOSED-FISCAL-FG(7)  TO F081.
           MOVE GP-END-FISCAL-DATE(8)   TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY             TO F09.
           MOVE GP-CLOSED-FISCAL-FG(8)  TO F091.
           MOVE GP-END-FISCAL-DATE(9)   TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY             TO F10.
           MOVE GP-CLOSED-FISCAL-FG(9)  TO F101.
           MOVE GP-END-FISCAL-DATE(10)  TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY             TO F11.
           MOVE GP-CLOSED-FISCAL-FG(10) TO F111.
           MOVE GP-END-FISCAL-DATE(11)  TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY             TO F12.
           MOVE GP-CLOSED-FISCAL-FG(11) TO F121.
           MOVE GP-END-FISCAL-DATE(12)  TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY             TO F13.
           MOVE GP-CLOSED-FISCAL-FG(12) TO F131.

           MOVE DISPLAY-BUF-12-END-DATES TO WR-BUF.
           MOVE DISPLAY-LIST-12-END-DATES TO WR-LIST.
           PERFORM CALL-WRFORM.

      *--- MOVE "NOTE: THE FISCAL PERIODS HAVE BEEN MODIFIED"
      *---                                                TO MESS.
      *--- PERFORM SEND-MESS.

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-02.
           MOVE VDU-DATE-YYYYMMDD TO GP-END-FISCAL-DATE(1).
           MOVE GP-END-FISCAL-DATE(1) TO NUM-DATE.

           IF NUM-MO > 12
              OR NUM-MO < 1
                 MOVE "PLEASE ENTER A MONTH FROM 1 TO 12" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.

       F-021.
           MOVE "E  A010021." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-021.
           MOVE VDUBUF TO GP-CLOSED-FISCAL-FG(1).
           IF NOT (VDUBUF = "Y" OR "N")
              PERFORM ENTER-Y-OR-N
              GO TO F-021.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-03.
           MOVE VDU-DATE-YYYYMMDD TO GP-END-FISCAL-DATE(2).
           MOVE GP-END-FISCAL-DATE(2) TO NUM-DATE.

           IF NUM-MO > 12
              OR NUM-MO < 1
                 MOVE "PLEASE ENTER A MONTH FROM 1 TO 12" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.

       F-031.
           MOVE "E  A010031." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-031.
           MOVE VDUBUF TO GP-CLOSED-FISCAL-FG(2).
           IF NOT (VDUBUF = "Y" OR "N")
              PERFORM ENTER-Y-OR-N
              GO TO F-031.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-04.
           MOVE VDU-DATE-YYYYMMDD TO GP-END-FISCAL-DATE(3).
           MOVE GP-END-FISCAL-DATE(3) TO NUM-DATE.

           IF NUM-MO > 12
              OR NUM-MO < 1
                 MOVE "PLEASE ENTER A MONTH FROM 1 TO 12" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.

       F-041.
           MOVE "E  A010041." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-041.
           MOVE VDUBUF TO GP-CLOSED-FISCAL-FG(3).
           IF NOT (VDUBUF = "Y" OR "N")
              PERFORM ENTER-Y-OR-N
              GO TO F-041.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-05.
           MOVE VDU-DATE-YYYYMMDD TO GP-END-FISCAL-DATE(4).
           MOVE GP-END-FISCAL-DATE(4) TO NUM-DATE.

           IF NUM-MO > 12
              OR NUM-MO < 1
                 MOVE "PLEASE ENTER A MONTH FROM 1 TO 12" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.
       F-051.
           MOVE "E  A010051." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-051.
           MOVE VDUBUF TO GP-CLOSED-FISCAL-FG(4).
           IF NOT (VDUBUF = "Y" OR "N")
              PERFORM ENTER-Y-OR-N
              GO TO F-051.

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-06.
           MOVE VDU-DATE-YYYYMMDD TO GP-END-FISCAL-DATE(5).
           MOVE GP-END-FISCAL-DATE(5) TO NUM-DATE.

           IF NUM-MO > 12
              OR NUM-MO < 1
                 MOVE "PLEASE ENTER A MONTH FROM 1 TO 12" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.

       F-061.
           MOVE "E  A010061." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-061.
           MOVE VDUBUF TO GP-CLOSED-FISCAL-FG(5).
           IF NOT (VDUBUF = "Y" OR "N")
              PERFORM ENTER-Y-OR-N
              GO TO F-061.

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-07.
           MOVE VDU-DATE-YYYYMMDD TO GP-END-FISCAL-DATE(6).
           MOVE GP-END-FISCAL-DATE(6) TO NUM-DATE.

           IF NUM-MO > 12
              OR NUM-MO < 1
                 MOVE "PLEASE ENTER A MONTH FROM 1 TO 12" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.

       F-071.
           MOVE "E  A010071." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-071.
           MOVE VDUBUF TO GP-CLOSED-FISCAL-FG(6).
           IF NOT (VDUBUF = "Y" OR "N")
              PERFORM ENTER-Y-OR-N
              GO TO F-071.

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-08.
           MOVE VDU-DATE-YYYYMMDD TO GP-END-FISCAL-DATE(7).
           MOVE GP-END-FISCAL-DATE(7) TO NUM-DATE.

           IF NUM-MO > 12
              OR NUM-MO < 1
                 MOVE "PLEASE ENTER A MONTH FROM 1 TO 12" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.
       F-081.
           MOVE "E  A010081." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-081.
           MOVE VDUBUF TO GP-CLOSED-FISCAL-FG(7).
           IF NOT (VDUBUF = "Y" OR "N")
              PERFORM ENTER-Y-OR-N
              GO TO F-081.

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-09.
           MOVE VDU-DATE-YYYYMMDD TO GP-END-FISCAL-DATE(8).
           MOVE GP-END-FISCAL-DATE(8) TO NUM-DATE.
           IF NUM-MO > 12
              OR NUM-MO < 1
                 MOVE "PLEASE ENTER A MONTH FROM 1 TO 12" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.

       F-091.
           MOVE "E  A010091." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-091.
           MOVE VDUBUF TO GP-CLOSED-FISCAL-FG(8).
           IF NOT (VDUBUF = "Y" OR "N")
              PERFORM ENTER-Y-OR-N
              GO TO F-091.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-10.
           MOVE VDU-DATE-YYYYMMDD TO GP-END-FISCAL-DATE(9).
           MOVE GP-END-FISCAL-DATE(9) TO NUM-DATE.

           IF NUM-MO > 12
              OR NUM-MO < 1
                 MOVE "PLEASE ENTER A MONTH FROM 1 TO 12" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.
       F-101.
           MOVE "E  A010101." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-101.
           MOVE VDUBUF TO GP-CLOSED-FISCAL-FG(9).
           IF NOT (VDUBUF = "Y" OR "N")
              PERFORM ENTER-Y-OR-N
              GO TO F-101.

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-11.
           MOVE VDU-DATE-YYYYMMDD TO GP-END-FISCAL-DATE(10).
           MOVE GP-END-FISCAL-DATE(10) TO NUM-DATE.

           IF NUM-MO > 12
              OR NUM-MO < 1
                 MOVE "PLEASE ENTER A MONTH FROM 1 TO 12" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.

       F-111.
           MOVE "E  A010111." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-111.
           MOVE VDUBUF TO GP-CLOSED-FISCAL-FG(10).
           IF NOT (VDUBUF = "Y" OR "N")
              PERFORM ENTER-Y-OR-N
              GO TO F-111.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-12.
           MOVE VDU-DATE-YYYYMMDD TO GP-END-FISCAL-DATE(11).
           MOVE GP-END-FISCAL-DATE(11) TO NUM-DATE.

           IF NUM-MO > 12
              OR NUM-MO < 1
                 MOVE "PLEASE ENTER A MONTH FROM 1 TO 12" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.

       F-121.
           MOVE "E  A010121." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-121.
           MOVE VDUBUF TO GP-CLOSED-FISCAL-FG(11).
           IF NOT (VDUBUF = "Y" OR "N")
              PERFORM ENTER-Y-OR-N
              GO TO F-121.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-13.
           MOVE VDU-DATE-YYYYMMDD TO GP-END-FISCAL-DATE(12).
           MOVE GP-END-FISCAL-DATE(12) TO NUM-DATE.

           IF NUM-MO > 12
              OR NUM-MO < 1
                 MOVE "PLEASE ENTER A MONTH FROM 1 TO 12" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.

       F-131.
           MOVE "E  A010131." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-131.
           MOVE VDUBUF TO GP-CLOSED-FISCAL-FG(12).
           IF NOT (VDUBUF = "Y" OR "N")
              PERFORM ENTER-Y-OR-N
              GO TO F-131.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-14.
           PERFORM CHECK-GLNO.

           IF IO-BAD
              GO TO ENTER-ELE.

           MOVE GL-BR-ACCOUNT TO GP-BANK-GL.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-15.
           PERFORM CHECK-GLNO.

           IF IO-BAD
              GO TO ENTER-ELE.

           MOVE GL-BR-ACCOUNT TO GP-BANK-DEPOSIT-GL.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-16.
           PERFORM CHECK-GLNO.

           IF IO-BAD
              GO TO ENTER-ELE.

           MOVE GL-BR-ACCOUNT TO GP-BANK-CLEARING-GL.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-17.
           MOVE VDUNUM0 TO GP-BANK-ROUTING-NO.
           PERFORM VERIFY-CHECK-DIGIT.

           IF CHK-DIG-FG = "N"
              MOVE "INVALID ROUTING NUMBER. PLEASE RE-ENTER" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-18.
           MOVE VDUNUM0 TO GP-BANK-ACCTNO.
           GO TO CREATE-ROUTINE.

       F-19.
           MOVE VDUNUM0 TO GP-FED-TAX-ID.
           GO TO CREATE-ROUTINE.

       F-20.
           MOVE VDUNUM0 TO GP-CORP-BRNO.
       F-201.
           MOVE "E NA250201." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS NOT = "00" AND
              VDUSTATUS NOT = "1D"
              GO TO F-201.
           MOVE VDUBUF TO GP-CORP-NAME.
           GO TO CREATE-ROUTINE.

       F-21.
           MOVE VDUBUF TO GP-SHORT-NAME.
           GO TO CREATE-ROUTINE.

       F-22.
           MOVE VDUBUF TO GP-BANK-NAME.
           GO TO CREATE-ROUTINE.

       F-23.
           IF VDUNUM0 < 2
              MOVE "MUST RETAIN FOR 2 OR MORE MONTHS" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           MOVE VDUNUM0 TO GP-TR-RETAIN-MONTHS.
           GO TO CREATE-ROUTINE.

       F-24.
           IF VDUNUM0 < 1
              MOVE "MUST RETAIN FOR 1 OR MORE MONTHS" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           MOVE VDUNUM0 TO GP-EOD-RETAIN-MONTHS.
           GO TO CREATE-ROUTINE.

       F-25.
           MOVE VDUNUM0 TO GP-NEXT-NOTE.
           IF ( GP-NEXT-NOTE = ZEROES )
              MOVE "NEXT ACCOUNT; MINIMUM = 1, MAXIMUM = 999999"
               TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

       F-26.
           MOVE VDUNUM0 TO GP-NEXT-BATCH-ID.
           IF ( GP-NEXT-BATCH-ID = ZEROES )
              MOVE "NEXT BATCH ID; MINIMUM = 1, MAXIMUM = 99999"
               TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

       F-27.
           MOVE VDUNUM0 TO GP-BP-NEXT-BATCH.
           IF NOT GP-BP-NEXT-BATCH-VALID
              MOVE "NEXT BATCH ID; MINIMUM = 100, MAXIMUM = 199" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

       F-28.
           MOVE VDUNUM0 TO GP-LB-NEXT-BATCH.
           IF NOT GP-LB-NEXT-BATCH-VALID
              MOVE "NEXT BATCH ID; MINIMUM = 200, MAXIMUM = 499" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

       F-29.
           MOVE VDUNUM0 TO GP-TRUE-NEXT-BATCH.
           IF NOT GP-TRUE-NEXT-BATCH-VALID
              MOVE "NEXT BATCH ID; MINIMUM = 500, MAXIMUM = 599" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

       F-30.
           MOVE VDUNUM0 TO GP-MGRAM-NEXT-BATCH.
           IF NOT GP-MGRAM-NEXT-BATCH-VALID
              MOVE "NEXT BATCH ID; MINIMUM = 600, MAXIMUM = 699" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

       F-31.
           MOVE VDUNUM0 TO GP-REFCD-NEXT-BATCH.
           IF NOT GP-REFCD-NEXT-BATCH-VALID
              MOVE "NEXT BATCH ID; MINIMUM = 700, MAXIMUM = 899" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

       F-32.
           MOVE VDUNUM0 TO GP-OTREF-NEXT-BATCH.
           IF NOT GP-OTREF-NEXT-BATCH-VALID
              MOVE "NEXT BATCH ID; MINIMUM = 900, MAXIMUM = 909" TO MESS
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
           MOVE 1 TO GP-KEY.
           PERFORM WRITE-GP-FILE.
           IF IO-BAD
              PERFORM ALREADY-CREATED
           ELSE
              MOVE "Y" TO NEW-REC.

       END-WRITE-MODULE.
           PERFORM CLOSE-GP-FILE.

      ******************************************************************
       LOG-MAINTENANCE-CHGS SECTION.
      ******************************************************************

           MOVE SPACES                     TO LMC-DETAIL.
           MOVE " TO "                     TO LMC-TO-X.

           IF GPOR-BEG-FISCAL-DATE NOT = GPCH-BEG-FISCAL-DATE
              MOVE "01 "                   TO LMC-FIELD
              MOVE "BEG FISCAL  "          TO LMC-DESC
              MOVE GPOR-BEG-FISCAL-DATE    TO LMC-ORG-TEXT
              MOVE GPCH-BEG-FISCAL-DATE    TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-END-FISCAL-DATE(1) NOT = GPCH-END-FISCAL-DATE(1)
              MOVE "02 "                   TO LMC-FIELD
              MOVE "END FISCAL 1  "        TO LMC-DESC
              MOVE GPOR-END-FISCAL-DATE(1) TO LMC-ORG-TEXT
              MOVE GPCH-END-FISCAL-DATE(1) TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-CLOSED-FISCAL-FG(1) NOT = GPCH-CLOSED-FISCAL-FG(1)
              MOVE "021"                   TO LMC-FIELD
              MOVE "END FISCAL FG1"        TO LMC-DESC
              MOVE GPOR-CLOSED-FISCAL-FG(1) TO LMC-ORG-TEXT
              MOVE GPCH-CLOSED-FISCAL-FG(1) TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-END-FISCAL-DATE(2) NOT = GPCH-END-FISCAL-DATE(2)
              MOVE "03 "                   TO LMC-FIELD
              MOVE "END FISCAL 2  "        TO LMC-DESC
              MOVE GPOR-END-FISCAL-DATE(2) TO LMC-ORG-TEXT
              MOVE GPCH-END-FISCAL-DATE(2) TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-CLOSED-FISCAL-FG(2) NOT = GPCH-CLOSED-FISCAL-FG(2)
              MOVE "031"                   TO LMC-FIELD
              MOVE "END FISCAL FG2"        TO LMC-DESC
              MOVE GPOR-CLOSED-FISCAL-FG(2) TO LMC-ORG-TEXT
              MOVE GPCH-CLOSED-FISCAL-FG(2) TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-END-FISCAL-DATE(3) NOT = GPCH-END-FISCAL-DATE(3)
              MOVE "04 "                   TO LMC-FIELD
              MOVE "END FISCAL 3  "        TO LMC-DESC
              MOVE GPOR-END-FISCAL-DATE(3) TO LMC-ORG-TEXT
              MOVE GPCH-END-FISCAL-DATE(3) TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-CLOSED-FISCAL-FG(3) NOT = GPCH-CLOSED-FISCAL-FG(3)
              MOVE "041"                   TO LMC-FIELD
              MOVE "END FISCAL FG3"        TO LMC-DESC
              MOVE GPOR-CLOSED-FISCAL-FG(3) TO LMC-ORG-TEXT
              MOVE GPCH-CLOSED-FISCAL-FG(3) TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-END-FISCAL-DATE(4) NOT = GPCH-END-FISCAL-DATE(4)
              MOVE "05 "                   TO LMC-FIELD
              MOVE "END FISCAL 4  "        TO LMC-DESC
              MOVE GPOR-END-FISCAL-DATE(4) TO LMC-ORG-TEXT
              MOVE GPCH-END-FISCAL-DATE(4) TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-CLOSED-FISCAL-FG(4) NOT = GPCH-CLOSED-FISCAL-FG(4)
              MOVE "051"                   TO LMC-FIELD
              MOVE "END FISCAL FG4"        TO LMC-DESC
              MOVE GPOR-CLOSED-FISCAL-FG(4) TO LMC-ORG-TEXT
              MOVE GPCH-CLOSED-FISCAL-FG(4) TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-END-FISCAL-DATE(5) NOT = GPCH-END-FISCAL-DATE(5)
              MOVE "06 "                   TO LMC-FIELD
              MOVE "END FISCAL 5  "        TO LMC-DESC
              MOVE GPOR-END-FISCAL-DATE(5) TO LMC-ORG-TEXT
              MOVE GPCH-END-FISCAL-DATE(5) TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-CLOSED-FISCAL-FG(5) NOT = GPCH-CLOSED-FISCAL-FG(5)
              MOVE "061"                   TO LMC-FIELD
              MOVE "END FISCAL FG5"        TO LMC-DESC
              MOVE GPOR-CLOSED-FISCAL-FG(5) TO LMC-ORG-TEXT
              MOVE GPCH-CLOSED-FISCAL-FG(5) TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-END-FISCAL-DATE(6) NOT = GPCH-END-FISCAL-DATE(6)
              MOVE "07 "                   TO LMC-FIELD
              MOVE "END FISCAL 6  "        TO LMC-DESC
              MOVE GPOR-END-FISCAL-DATE(6) TO LMC-ORG-TEXT
              MOVE GPCH-END-FISCAL-DATE(6) TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-CLOSED-FISCAL-FG(6) NOT = GPCH-CLOSED-FISCAL-FG(6)
              MOVE "071"                   TO LMC-FIELD
              MOVE "END FISCAL FG6"        TO LMC-DESC
              MOVE GPOR-CLOSED-FISCAL-FG(6) TO LMC-ORG-TEXT
              MOVE GPCH-CLOSED-FISCAL-FG(6) TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-END-FISCAL-DATE(7) NOT = GPCH-END-FISCAL-DATE(7)
              MOVE "08 "                   TO LMC-FIELD
              MOVE "END FISCAL 7  "        TO LMC-DESC
              MOVE GPOR-END-FISCAL-DATE(7) TO LMC-ORG-TEXT
              MOVE GPCH-END-FISCAL-DATE(7) TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-CLOSED-FISCAL-FG(7) NOT = GPCH-CLOSED-FISCAL-FG(7)
              MOVE "081"                   TO LMC-FIELD
              MOVE "END FISCAL FG7"        TO LMC-DESC
              MOVE GPOR-CLOSED-FISCAL-FG(7) TO LMC-ORG-TEXT
              MOVE GPCH-CLOSED-FISCAL-FG(7) TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-END-FISCAL-DATE(8) NOT = GPCH-END-FISCAL-DATE(8)
              MOVE "09 "                   TO LMC-FIELD
              MOVE "END FISCAL 8  "        TO LMC-DESC
              MOVE GPOR-END-FISCAL-DATE(8) TO LMC-ORG-TEXT
              MOVE GPCH-END-FISCAL-DATE(8) TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-CLOSED-FISCAL-FG(8) NOT = GPCH-CLOSED-FISCAL-FG(8)
              MOVE "091"                   TO LMC-FIELD
              MOVE "END FISCAL FG8"        TO LMC-DESC
              MOVE GPOR-CLOSED-FISCAL-FG(8) TO LMC-ORG-TEXT
              MOVE GPCH-CLOSED-FISCAL-FG(8) TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-END-FISCAL-DATE(9) NOT = GPCH-END-FISCAL-DATE(9)
              MOVE "10 "                   TO LMC-FIELD
              MOVE "END FISCAL 9  "        TO LMC-DESC
              MOVE GPOR-END-FISCAL-DATE(9) TO LMC-ORG-TEXT
              MOVE GPCH-END-FISCAL-DATE(9) TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-CLOSED-FISCAL-FG(9) NOT = GPCH-CLOSED-FISCAL-FG(9)
              MOVE "101"                   TO LMC-FIELD
              MOVE "END FISCAL FG9"        TO LMC-DESC
              MOVE GPOR-CLOSED-FISCAL-FG(9) TO LMC-ORG-TEXT
              MOVE GPCH-CLOSED-FISCAL-FG(9) TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-END-FISCAL-DATE(10) NOT = GPCH-END-FISCAL-DATE(10)
              MOVE "11 "                   TO LMC-FIELD
              MOVE "END FISCAL 10 "        TO LMC-DESC
              MOVE GPOR-END-FISCAL-DATE(10) TO LMC-ORG-TEXT
              MOVE GPCH-END-FISCAL-DATE(10) TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-CLOSED-FISCAL-FG(10) NOT = GPCH-CLOSED-FISCAL-FG(10)
              MOVE "111"                   TO LMC-FIELD
              MOVE "END FISCALFG10"        TO LMC-DESC
              MOVE GPOR-CLOSED-FISCAL-FG(10) TO LMC-ORG-TEXT
              MOVE GPCH-CLOSED-FISCAL-FG(10) TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-END-FISCAL-DATE(11) NOT = GPCH-END-FISCAL-DATE(11)
              MOVE "12 "                   TO LMC-FIELD
              MOVE "END FISCAL 11 "        TO LMC-DESC
              MOVE GPOR-END-FISCAL-DATE(11) TO LMC-ORG-TEXT
              MOVE GPCH-END-FISCAL-DATE(11) TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-CLOSED-FISCAL-FG(11) NOT = GPCH-CLOSED-FISCAL-FG(11)
              MOVE "121"                   TO LMC-FIELD
              MOVE "END FISCALFG11"        TO LMC-DESC
              MOVE GPOR-CLOSED-FISCAL-FG(11) TO LMC-ORG-TEXT
              MOVE GPCH-CLOSED-FISCAL-FG(11) TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-END-FISCAL-DATE(12) NOT = GPCH-END-FISCAL-DATE(12)
              MOVE "13 "                   TO LMC-FIELD
              MOVE "END FISCAL 12 "        TO LMC-DESC
              MOVE GPOR-END-FISCAL-DATE(12) TO LMC-ORG-TEXT
              MOVE GPCH-END-FISCAL-DATE(12) TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-CLOSED-FISCAL-FG(12) NOT = GPCH-CLOSED-FISCAL-FG(12)
              MOVE "131"                   TO LMC-FIELD
              MOVE "END FISCALFG12"        TO LMC-DESC
              MOVE GPOR-CLOSED-FISCAL-FG(12) TO LMC-ORG-TEXT
              MOVE GPCH-CLOSED-FISCAL-FG(12) TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-BANK-GL NOT = GPCH-BANK-GL
              MOVE "14 "                   TO LMC-FIELD
              MOVE "BANK GL       "        TO LMC-DESC
              MOVE GPOR-BANK-GL            TO LMC-ORG-TEXT
              MOVE GPCH-BANK-GL            TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-BANK-DEPOSIT-GL NOT = GPCH-BANK-DEPOSIT-GL
              MOVE "15 "                   TO LMC-FIELD
              MOVE "BNK DEPOSIT GL"        TO LMC-DESC
              MOVE GPOR-BANK-DEPOSIT-GL    TO LMC-ORG-TEXT
              MOVE GPCH-BANK-DEPOSIT-GL    TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-BANK-CLEARING-GL NOT = GPCH-BANK-CLEARING-GL
              MOVE "16 "                   TO LMC-FIELD
              MOVE "BANK CLEAR GL "        TO LMC-DESC
              MOVE GPOR-BANK-CLEARING-GL   TO LMC-ORG-TEXT
              MOVE GPCH-BANK-CLEARING-GL   TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-BANK-ROUTING-NO NOT = GPCH-BANK-ROUTING-NO
              MOVE "17 "                   TO LMC-FIELD
              MOVE "BANK ROUTING #"        TO LMC-DESC
              MOVE GPOR-BANK-ROUTING-NO   TO LMC-ORG-TEXT
              MOVE GPCH-BANK-ROUTING-NO   TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-BANK-ACCTNO NOT = GPCH-BANK-ACCTNO
              MOVE "18 "                   TO LMC-FIELD
              MOVE "BANK ACCOUNT #"        TO LMC-DESC
              MOVE GPOR-BANK-ACCTNO   TO LMC-ORG-TEXT
              MOVE GPCH-BANK-ACCTNO   TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-FED-TAX-ID NOT = GPCH-FED-TAX-ID
              MOVE "19 "                   TO LMC-FIELD
              MOVE "FED TAXID    "        TO LMC-DESC
              MOVE GPOR-FED-TAX-ID   TO LMC-ORG-TEXT
              MOVE GPCH-FED-TAX-ID   TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-CORP-BRNO NOT = GPCH-CORP-BRNO
              MOVE "20 "                   TO LMC-FIELD
              MOVE "CORPORATE BRNO"        TO LMC-DESC
              MOVE GPOR-CORP-BRNO   TO LMC-ORG-NUM
              MOVE GPCH-CORP-BRNO   TO LMC-CHG-NUM
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-CORP-NAME NOT = GPCH-CORP-NAME
              MOVE "21 "                   TO LMC-FIELD
              MOVE "CORPORATE NAME"        TO LMC-DESC
              MOVE GPOR-CORP-NAME   TO LMC-ORG-TEXT
              MOVE GPCH-CORP-NAME   TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-SHORT-NAME NOT = GPCH-SHORT-NAME
              MOVE "22 "                   TO LMC-FIELD
              MOVE "SHORT NAME    "        TO LMC-DESC
              MOVE GPOR-SHORT-NAME   TO LMC-ORG-TEXT
              MOVE GPCH-SHORT-NAME   TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-BANK-NAME NOT = GPCH-BANK-NAME
              MOVE "23 "                   TO LMC-FIELD
              MOVE "BANK NAME     "        TO LMC-DESC
              MOVE GPOR-BANK-NAME   TO LMC-ORG-TEXT
              MOVE GPCH-BANK-NAME   TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-TR-RETAIN-MONTHS NOT = GPCH-TR-RETAIN-MONTHS
              MOVE "24 "                   TO LMC-FIELD
              MOVE "TR RETAIN MON "        TO LMC-DESC
              MOVE GPOR-TR-RETAIN-MONTHS   TO LMC-ORG-NUM
              MOVE GPCH-TR-RETAIN-MONTHS   TO LMC-CHG-NUM
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-EOD-RETAIN-MONTHS NOT = GPCH-EOD-RETAIN-MONTHS
              MOVE "25 "                   TO LMC-FIELD
              MOVE "EOD RETAIN MON"        TO LMC-DESC
              MOVE GPOR-EOD-RETAIN-MONTHS   TO LMC-ORG-NUM
              MOVE GPCH-EOD-RETAIN-MONTHS   TO LMC-CHG-NUM
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-NEXT-NOTE NOT = GPCH-NEXT-NOTE
              MOVE "26 "                   TO LMC-FIELD
              MOVE "NEXT NOTE #   "        TO LMC-DESC
              MOVE GPOR-NEXT-NOTE   TO LMC-ORG-NUM
              MOVE GPCH-NEXT-NOTE   TO LMC-CHG-NUM
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-NEXT-BATCH-ID NOT = GPCH-NEXT-BATCH-ID
              MOVE "27 "                   TO LMC-FIELD
              MOVE "NEXT BATCH ID "        TO LMC-DESC
              MOVE GPOR-NEXT-BATCH-ID   TO LMC-ORG-NUM
              MOVE GPCH-NEXT-BATCH-ID   TO LMC-CHG-NUM
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-LB-NEXT-BATCH NOT = GPCH-LB-NEXT-BATCH
              MOVE "28 "                   TO LMC-FIELD
              MOVE "LB NEXT BATCH "        TO LMC-DESC
              MOVE GPOR-LB-NEXT-BATCH   TO LMC-ORG-NUM
              MOVE GPCH-LB-NEXT-BATCH   TO LMC-CHG-NUM
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-TRUE-NEXT-BATCH NOT = GPCH-TRUE-NEXT-BATCH
              MOVE "29 "                   TO LMC-FIELD
              MOVE "TRUE NEXTBATCH"        TO LMC-DESC
              MOVE GPOR-TRUE-NEXT-BATCH   TO LMC-ORG-NUM
              MOVE GPCH-TRUE-NEXT-BATCH   TO LMC-CHG-NUM
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-MGRAM-NEXT-BATCH NOT = GPCH-MGRAM-NEXT-BATCH
              MOVE "30 "                   TO LMC-FIELD
              MOVE "MGRAMNEXTBATCH"        TO LMC-DESC
              MOVE GPOR-MGRAM-NEXT-BATCH   TO LMC-ORG-NUM
              MOVE GPCH-MGRAM-NEXT-BATCH   TO LMC-CHG-NUM
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-REFCD-NEXT-BATCH NOT = GPCH-REFCD-NEXT-BATCH
              MOVE "31 "                   TO LMC-FIELD
              MOVE "REFCDNEXTBATCH"        TO LMC-DESC
              MOVE GPOR-REFCD-NEXT-BATCH   TO LMC-ORG-NUM
              MOVE GPCH-REFCD-NEXT-BATCH   TO LMC-CHG-NUM
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-OTREF-NEXT-BATCH NOT = GPCH-OTREF-NEXT-BATCH
              MOVE "32 "                   TO LMC-FIELD
              MOVE "OTREFNEXTBATCH"        TO LMC-DESC
              MOVE GPOR-OTREF-NEXT-BATCH   TO LMC-ORG-NUM
              MOVE GPCH-OTREF-NEXT-BATCH   TO LMC-CHG-NUM
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
       NEW-RECORD-SETUP SECTION.
      ******************************************************************
           MOVE 1 TO NEW-FLAG.
           MOVE "S  A000NEW." TO VDU.
           MOVE "*** NEW RECORD ***" TO VDUBUF.
           PERFORM SCREENIO.

           PERFORM CLEAR-GP-FILE.

           MOVE 0 TO ELE.

      ******************************************************************
       DISPLAY-FIELDS SECTION.
      ******************************************************************
           MOVE SPACES TO DISPLAY-BUF.

           IF GP-BEG-FISCAL-DATE NOT NUMERIC
              MOVE 0 TO GP-BEG-FISCAL-DATE.

           MOVE 1 TO SUB.
           PERFORM CHECK-FOR-CLEAR UNTIL SUB = 13.
           MOVE GP-BEG-FISCAL-DATE      TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY             TO F01.
           MOVE GP-END-FISCAL-DATE(1)   TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY             TO F02.
           MOVE GP-CLOSED-FISCAL-FG(1)  TO F021.
           MOVE GP-END-FISCAL-DATE(2)   TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY             TO F03.
           MOVE GP-CLOSED-FISCAL-FG(2)  TO F031.
           MOVE GP-END-FISCAL-DATE(3)   TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY             TO F04.
           MOVE GP-CLOSED-FISCAL-FG(3)  TO F041.
           MOVE GP-END-FISCAL-DATE(4)   TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY             TO F05.
           MOVE GP-CLOSED-FISCAL-FG(4)  TO F051.
           MOVE GP-END-FISCAL-DATE(5)   TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY             TO F06.
           MOVE GP-CLOSED-FISCAL-FG(5)  TO F061.
           MOVE GP-END-FISCAL-DATE(6)   TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY             TO F07.
           MOVE GP-CLOSED-FISCAL-FG(6)  TO F071.
           MOVE GP-END-FISCAL-DATE(7)   TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY             TO F08.
           MOVE GP-CLOSED-FISCAL-FG(7)  TO F081.
           MOVE GP-END-FISCAL-DATE(8)   TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY             TO F09.
           MOVE GP-CLOSED-FISCAL-FG(8)  TO F091.
           MOVE GP-END-FISCAL-DATE(9)   TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY             TO F10.
           MOVE GP-CLOSED-FISCAL-FG(9)  TO F101.
           MOVE GP-END-FISCAL-DATE(10)  TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY             TO F11.
           MOVE GP-CLOSED-FISCAL-FG(10) TO F111.
           MOVE GP-END-FISCAL-DATE(11)  TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY             TO F12.
           MOVE GP-CLOSED-FISCAL-FG(11) TO F121.
           MOVE GP-END-FISCAL-DATE(12)  TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY             TO F13.
           MOVE GP-CLOSED-FISCAL-FG(12) TO F131.

           MOVE GP-BANK-GL TO WORK-GL.
           MOVE WORK-BR TO DISP-BR.
           MOVE WORK-NO TO DISP-NO.
           MOVE DISP-GL TO F14.

           MOVE GP-BANK-DEPOSIT-GL TO WORK-GL.
           MOVE WORK-BR TO DISP-BR.
           MOVE WORK-NO TO DISP-NO.
           MOVE DISP-GL TO F15.

           MOVE GP-BANK-CLEARING-GL TO WORK-GL.
           MOVE WORK-BR TO DISP-BR.
           MOVE WORK-NO TO DISP-NO.
           MOVE DISP-GL TO F16.

           MOVE GP-BANK-ROUTING-NO TO F17.
           MOVE GP-BANK-ACCTNO TO F18.

           MOVE GP-FED-TAX-ID TO F19.
           MOVE GP-CORP-BRNO TO F20.
           MOVE GP-CORP-NAME TO F201.
           MOVE GP-SHORT-NAME TO F21.
           MOVE GP-BANK-NAME TO F22.
           MOVE GP-TR-RETAIN-MONTHS  TO F23.
           MOVE GP-EOD-RETAIN-MONTHS TO F24.
           MOVE GP-NEXT-NOTE         TO F25.
           MOVE GP-NEXT-BATCH-ID     TO F26.
           MOVE GP-BP-NEXT-BATCH     TO F27.
           MOVE GP-LB-NEXT-BATCH     TO F28.
           MOVE GP-TRUE-NEXT-BATCH   TO F29.
           MOVE GP-MGRAM-NEXT-BATCH  TO F30.
           MOVE GP-REFCD-NEXT-BATCH  TO F31.
           MOVE GP-OTREF-NEXT-BATCH  TO F32.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       DISPLAY-FIELDS-EXIT.
           EXIT.

      ******************************************************************
       CHECK-FOR-CLEAR SECTION.
      ******************************************************************
           IF GP-END-FISCAL-DATE(SUB) NOT NUMERIC
              MOVE 0 TO GP-END-FISCAL-DATE(SUB).

           ADD 1 TO SUB.

      ******************************************************************
       CHECK-GLNO SECTION.
      ******************************************************************
           MOVE VDUBUF TO SCREEN-GL.
           CALL "C$JUSTIFY" USING SCREEN-GL, "R".
           INSPECT SCREEN-GL REPLACING LEADING " " BY "0".
           MOVE 0 TO WORK-GL.
           IF SCREEN-GL-NUM IS NUMERIC
              MOVE SCREEN-GL-NUM TO WORK-GL
              GO TO DISPLAY-GL. 
           IF SC-DASH = "-"
              IF (SC-BRANCH NUMERIC AND SC-NUMBER NUMERIC)
                 MOVE SC-BRANCH TO WORK-BR
                 MOVE SC-NUMBER TO WORK-NO.
           IF WORK-GL NOT NUMERIC
              MOVE 9 TO IO-FG
              GO TO EXIT-CHECK-GLNO.
       DISPLAY-GL.
           MOVE "S  A120" TO VDU.
           MOVE ELE TO VDUROW.
           MOVE 0   TO VDUCOL.
           MOVE "." TO VDUDECIMAL.
           MOVE WORK-BR TO DISP-BR.
           MOVE WORK-NO TO DISP-NO.
           MOVE DISP-GL TO VDUBUF.
           PERFORM SCREENIO.
       READ-GL.
           MOVE WORK-GL TO GL-BR-ACCOUNT.
           PERFORM OPEN-GL1-FILE.
           PERFORM READ-GL1-FILE.
           PERFORM CLOSE-GL1-FILE.

           IF IO-BAD
              MOVE "INVALID G/L#" TO MESS
              PERFORM SEND-MESS.

           IF GL-BR-ACCOUNT = 0
              MOVE 0 TO IO-FG.

       EXIT-CHECK-GLNO.
           EXIT.

      ******************************************************************
       VERIFY-CHECK-DIGIT SECTION.
      ******************************************************************
           MOVE GP-BANK-ROUTING-NO TO TMP-BANK-ROUTING-NO.

           MOVE 0 TO CHECK-SUM
                     CHECK-BASE
                     CHECK-MODIFIER
                     WEIGHT-FACTOR
                     TMP-RESULT.

           PERFORM COMPUTE-CHECK-SUM VARYING CK-SUB FROM 1 BY 1
                                     UNTIL CK-SUB > 8.

           DIVIDE 10 INTO CHECK-SUM GIVING CHECK-BASE
                                    REMAINDER CHECK-MODIFIER.

      * CHECK FOR REMAINDER IF Y THEN USE NEXT FACTOR OF 10.

           IF CHECK-MODIFIER NOT = 0
              ADD 1 TO CHECK-BASE.

           MULTIPLY CHECK-BASE BY 10 GIVING WEIGHT-FACTOR.

      * SUBTRACT SUM FROM WEIGHT FACTOR AND COMPARE TO CHECK DIGIT.

           SUBTRACT CHECK-SUM FROM WEIGHT-FACTOR.

           IF WEIGHT-FACTOR = TMP-RT-NO(9)
                MOVE "Y" TO CHK-DIG-FG
           ELSE
                MOVE "N" TO CHK-DIG-FG.

           GO TO EXIT-VERIFY-CHECK-DIGIT.

       COMPUTE-CHECK-SUM.
           MULTIPLY TMP-RT-NO(CK-SUB) BY WEIGHT-TABLE(CK-SUB) GIVING
                                           TMP-SUM.
           ADD TMP-SUM TO CHECK-SUM.

       EXIT-VERIFY-CHECK-DIGIT.
           EXIT.

      ******************************************************************
       GET-FISCAL-PERIOD-DATES SECTION.
      ******************************************************************
           MOVE 31 TO NDTE-DD.
           MOVE 1 TO NDTE-HOLD.
           PERFORM INCREMENT-MONTHS.
           MOVE NDTE-DATE TO GP-END-FISCAL-DATE(SUB-DT).
           MOVE "N"      TO GP-CLOSED-FISCAL-FG(SUB-DT).

       GET-FISCAL-PERIOD-DATES-EXIT.
           EXIT.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GBGPCL.CPY".

      ***************************************************
       FORM-RESET SECTION.
      ***************************************************
           MOVE EXT-CONAME-CONAME TO GPMAN1-CONAME.
           MOVE EXT-CONAME-SYSDATE TO GPMAN1-SYSDATE.
           DISPLAY GPMAN1-SCREEN UPON GPMAIN-WINDOW-HANDLE.
           MOVE GPMAN1-FORM-FIELDS TO WR-BUF.
           MOVE GPMAN1-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ***************************************************
       SETUP-LINK-INFO SECTION.
      ***************************************************
           MOVE GPMAN1-HELP--FILE TO HELP-LINK-FILE.
           MOVE GPMAN1-HELP--DIR TO HELP-LINK-DIR.

      ***************************************************
       VDU-CONVERT-FIELD SECTION.
      ***************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBGB/GPMAN1_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.


      ***********************************
       MISC-PARAGRAPHS SECTION.
      ***********************************
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
       ENTER-Y-OR-N.
           MOVE "ENTER 'Y' OR 'N' ONLY." TO MESS.
           PERFORM SEND-MESS.

      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-GL1-FILE.
           PERFORM CLOSE-GP-FILE.
       COPY "LIBGL/GLGLGS_SQL.CPY".
       COPY "LIBGB/GBGPGS_SQL.CPY".
       COPY "LIBGB/GBGPIN.CPY".
       COPY "LIBGL/GLGL1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      ********************************
       END-ROUTINE SECTION.
      ********************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY GPMAN1-SCREEN.
           MOVE FORM-PATH TO FORM-PATHNAME.
           MOVE GPMAIN-LINK TO FORM-LINK-AREA.
           EXIT PROGRAM.
