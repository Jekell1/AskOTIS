      * COPYMEMBER: LIBGB/GB01EM
      **********************************************************************
      *    EMPLOYEE FILE
      *
      * REV:
      * BLF 061012 ADDED EM-TYPE, WORLD PR# 488
      * BLF 070307 ADDED EM-CAR-ALLOWANCE, EM-PREFERRED-NAME, WORLD PR# 488
      * BLF 070308 ADDED 4TH KEY, EM-LOGIN, TO DEFAULT EMPLOYEE BASED ON
      *            LOGIN IN TIME SHEET ENTRY
      * BLF 070423 ADDED EM-BONUS-FG, EM-PAY-THRU-DATE, EM-VAC-DAYS-TO-PAY,
      *            EXPANDED UNUSED, WORLD PR# 488
      * BLF 070425 ADDED EM-UPDATE-FG, WORLD PR# 488
      * BLF 070524 ADDED EM-SALARY-TYPE, WORLD PR# 488
      * BLF 070829 ADDED EM5-KEY (EM-SSNO), WORLD CANNOT HAVE DUP SSNO'S
      *            GOING TO ADP SYSTEM, WORLD PR# 488
      * BLM 150113 ADDED EM-LOA-START-DATE (LEAVE OF ABSENCE START), 
      *                  EM-LOA-END-DATE, EM-PAY-CHANGE-DATE, WORLD PR# 1035
      * BLM 150209 ADDED EM-BRNO-CHANGE-DATE - SYSTEM DATE WHEN EMPLOYEE'S
      *            HOME BRANCH # WAS CHANGED, WORLD PR# 1035 ADDENDUM
      * BLM 161025 ADD EM-APPROVAL-LOGIN (FOR HOME OFFICE EMPLOYEES),
      *            WORLD PR# 1197
      **********************************************************************
       78  EM-SIZE                   VALUE 508.
       01  EM-REC.
           03  EM3-KEY.
               05  EM-BRNO           PIC 9(4).
               05  EM1-KEY.
                   07  EM-EMPID      PIC 9(6).
           03  EM2-KEY.
               05  EM-LNAME          PIC X(16).
               05  EM-FNAME          PIC X(16).
           03  EM-MID-INITIAL        PIC X.
           03  EM-ADR1               PIC X(24).
           03  EM-ADR2               PIC X(24).
           03  EM-CITY               PIC X(20).
           03  EM-STATE              PIC XX.
           03  EM-ZIP                PIC X(10).
           03  EM-PHONE              PIC 9(10)      COMP-6.
           03  EM5-KEY.
               05  EM-SSNO           PIC 9(9)       COMP-6.
           03  EM-SS-NAME            PIC X(32).
           03  EM-POSITION           PIC X(20).
           03  EM-BIRTH-DATE         PIC 9(6)       COMP-6.
           03  EM-HIRE-DATE          PIC 9(6)       COMP-6.
           03  EM-TERMINATE-DATE     PIC 9(6)       COMP-6.
           03  EM-COUNTRY-CITIZEN    PIC X(15).
           03  EM-ORIGIN             PIC X(4).
           03  EM-SEX                PIC X.
           03  EM-MARITAL-STAT       PIC X.
      *        ACTIVE CD: A-ACTIVE  T-TERMINATED  L-LAYOFF
           03  EM-ACTIVE-CD          PIC X.
           03  EM-REHIRE-FG          PIC X.
      *        PAYTYPE: H-HOURLY   S-SALARY
           03  EM-PAYTYPE            PIC X.
      *        PAYFREQ: 52-WEEKLY 26-BIWEEKLY  24-SEMI-MONTHLY 12-MONTHLY
           03  EM-PAYFREQ            PIC 99          COMP-6.
           03  EM-REG-HOURS          PIC 9(3)        COMP-6.
           03  EM-FED-ALLOWANCES     PIC 99          COMP-6.
           03  EM-ST-ALLOWANCES      PIC 99          COMP-6.
           03  EM-LOC-ALLOWANCES     PIC 99          COMP-6.
           03  EM-EXFTAX             PIC 9(4)V99     COMP-6.
           03  EM-EXSTAX             PIC 9(4)V99     COMP-6.
           03  EM-EXLTAX             PIC 9(4)V99     COMP-6.
           03  EM-SALARY             PIC 9(7)V99     COMP-6.
           03  EM-REGRATE            PIC 9999V99     COMP-6.
           03  EM-OT-RATE1           PIC 9999V99     COMP-6.
           03  EM-OT-RATE2           PIC 9999V99     COMP-6.
           03  EM-PERS-AVAIL         PIC S9(3)V99    COMP-3.
           03  EM-PERS-TAKEN         PIC S9(3)V99    COMP-3.
           03  EM-SICK-AVAIL         PIC S9(3)V99    COMP-3.
           03  EM-SICK-TAKEN         PIC S9(3)V99    COMP-3.
           03  EM-VAC-AVAIL          PIC S9(3)V99    COMP-3.
           03  EM-VAC-TAKEN          PIC S9(3)V99    COMP-3.
           03  EM-MTD-HRS-WORKED     PIC S9(3)V99    COMP-3.
           03  EM-YTD-HRS-WORKED     PIC S9(4)V99    COMP-3.
           03  EM-LYR-HRS-WORKED     PIC S9(4)V99    COMP-3.
           03  EM-EMERGENCY-CONTACT  PIC X(24).
           03  EM-EMERGENCY-PHONE    PIC 9(10)      COMP-6.
           03  EM-EMERGENCY-EXT      PIC 9(4)       COMP-6.
           03  EM-COMMENT1           PIC X(24).
           03  EM-COMMENT2           PIC X(24).
           03  EM-COMMENT3           PIC X(24).
           03  EM4-KEY.
               05  EM-LOGIN          PIC X(8).
      *        EMPLOYEE TYPE: F-FULLTIME  P-PARTTIME  T-TEMPORARY
           03  EM-TYPE               PIC X.
           03  EM-PREFERRED-NAME     PIC X(32).
           03  EM-CAR-ALLOWANCE      PIC 9999V99    COMP-6.
           03  EM-BONUS-FG           PIC X.
           03  EM-PAY-THRU-DATE      PIC 9(6)       COMP-6.
           03  EM-VAC-DAYS-TO-PAY    PIC 99         COMP-6.
           03  EM-UPDATE-FG          PIC X.
           03  EM-SALARY-TYPE        PIC X.
           03  EM-LOA-START-DATE     PIC 9(6)       COMP-6.
           03  EM-LOA-END-DATE       PIC 9(6)       COMP-6.
           03  EM-PAY-CHANGE-DATE    PIC 9(6)       COMP-6.
           03  EM-BRNO-CHANGE-DATE   PIC 9(6)       COMP-6.
           03  EM-APPROVAL-LOGIN     PIC X(8).
           03  FILLER                PIC X(58).
