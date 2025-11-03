       IDENTIFICATION DIVISION.
       PROGRAM-ID.      GPOPM1.
       DATE-WRITTEN.    032196.
      ******************************************************************
      *
      *  DIR :   GB
      *  DESC:   GP FILE HIDDEN GLOBAL PARAMETER FILE MAINT PAGE 1
      *
      * REV :
      *  MJD 042396 ADDED GP-ALLOW-RLOGINS.
      *  BAH 072696 ADDED GP-BATCH-MORE-THAN-DAY TO NOT FORCE YOU TO
      *             RELEASE BATCH CHECKS DAILY (INSTANT)
      *  BAH 072696 ADDED GP-PRINT-GL-RECAP TO ALLOW GL RECAP TO PRINT
      *             FROM BATCH PROCEDURE PRINT, MERCURY DOES NOT WANT THEM..
      *  BAH 060997 ADDED GP-DISPLAY-BANK-BAL TO DISPLAY BANK BALANCES
      *             WHEN CLOSING THE DAY FOR COLONIAL, TN PR#507
      *  MJD 032698 ADDED GP-RLOGIN-LOCAL-PROFILE AND GP-RLOGIN-REMOTE-PROFILE.
      *  BAH 040198 ADDED GP-REPODATE-PAYDATE FOR FEDERATED #663 AND RAC #518
      *  BAH 040298 ADDED GP-POST-INTO-FUTURE FROM A90
      *  JTG 101298 ADDED GP-ALLOW-REVOLVING-FG, REGENCY REVOL
      *  GLF 080499 ADDED GP-VALID-BDATE, REGENCY PR#57
      *  GLF 081899 ADDED GP-ARCHIVE-DIRECTORY
      *  MG  991006 FIXED VDUSTATUS/IO-FG/STAT PER DEVELOPMENT MEETING.
      *  KEC 991130 ADDED GP-LI-APPLICATION-FG (CMSI/CSI)
      *  MJD 000201 ADDED GP-BULLETIN-BOARD (WORLD)
      *  MJD 000209 EXPANDED GP-BULLETIN-BOARD TO X(15)
      *  JTG 000330 ADDED GP-PDTH-GT-SCHED-PAY MERCURY J001
      *  BAH 000608 ADDED GP-REF-PHONE (TFC PR# 37)
      *  MJD 000620 ADDED-GP-USE-CONRATE (REGACC PR#1220)
      *  MJD 000816 ADDED GP-TIME-ZONE-Q (TFC PR#25)
      *  MJD 001116 ADDED GP-PENALTY-INT (NEWS PR#1264)
      *  JTG 001215 ADDED GP-ALLOW-SET-FOR-NONCASH, MERCURY #239
      *  GLB 010502 ADDED GP-REVOLVE-MULTI; GP-REVOLVE-SPECIAL
      *  JTG 010516 ADDED GP-ALLOW-ENTRY-TRADEIN
      *             ALLOWED ENTRY OF 'TRADEIN' RE:SALE PRICE-DP-TI PAACO PR#1467
      *  CS  010607 ADDED GP-REPO-CODE   REGIONAL PR#1478
      *  JTG 010720 ADDED GP-ALLOW-MORTGAGE-INT-Z     LENDMARK PR#1045 & PR#1380
      *  BLV 010723 ADDED GP-PROM-AMT-MIN-PCT, DEVELOPMENT PR# 1489
      *  CS  010822 ADDED GP-BW-PRIVACY-OPT-OUT PR#1571 STLOUIS
      *  BAH 011102 ADDED GP-INTERIM-COUPONS FOR REGACC PR# 1632
      *  GLB 020104 ADDED GP-DL-ACH-FG REGACC PR# 1691 DIRECT DEPOSIT
      *  BLV 020215 ADDED GP-GL-NEW-JE, OPTION FOR NEW JOURNAL ENTRY PROGRAMS
      *  BAH 020227 ADDED GP-CLEAR-PROM-PDTH TO NOT CLEAR PROMISE DATE FIELDS
      *             UNLESS PDTH ADVANCES, REGACC PR# 1750
      *  BAH 020416 CHANGED GPOPMN TO GPOPM1, HAD TO ADD PAGE 2
      *  BAH 120514 REMOVED MAXPRU NONDISPLAY
      *  BAH 130125 A30 CONVERSION
      *  BAH 180430 ADDED ERRLOG
      *  BAH 2019.0610 REMOVED 14. GP-BULLETIN-BOARD , FORCED INTO LP DIRECTORY
      *  BAH 20190625 REMOVED 3 GP REVOLVING FIELDS, PT#0153
      *  BAH 2023.1006 CHANGED SCREEN FIELDS PROFILES TO MUST Y TO ALLOW
      *                LOWERCASE
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
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)GB/GPOPM1.CBL A32 07/09/19-12:20:45 >".

       COPY "LIBGB/GB01GP.CPY".
       COPY "LIBGB/GB01GP_SQL.CPY".
       COPY "LIBGB/GBWSGP.CPY".
       COPY "LIBSP/SPWSBB.CPY".

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

       01  TEST-24-TIME    PIC 9(04).
       01  TEST-24-TIME-ALP PIC X(04)
                            REDEFINES TEST-24-TIME.
       01  FILLER REDEFINES TEST-24-TIME.
           03  TEST-24-HH  PIC 9(02).
           03  TEST-24-MM  PIC 9(02).

       01  LOWER-A         PIC X    VALUE X'61'.
       01  LOWER-Z         PIC X    VALUE X'7A'.

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
           03  FORM-NAME            PIC X(20).
           03  NEW-REC              PIC X.
           03  ERRCD                PIC X.
           03  FORMNUM              PIC 9.
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
           03  F08         PIC X.
           03  F09         PIC X.
           03  F10         PIC X.
           03  F11         PIC X.
           03  F12         PIC X(4).
           03  F13         PIC X.
           03  F14         PIC X.
           03  F15         PIC X.
           03  F16         PIC X.
           03  F17         PIC X.
           03  F18         PIC X.
           03  F19         PIC 9999.
           03  F191        PIC 9999.
           03  F20         PIC X.
           03  F21         PIC X.
           03  F22         PIC X.
           03  F23         PIC X.
           03  F24         PIC X.
           03  F25         PIC X.
           03  F26         PIC X.
           03  F27         PIC ZZ9.
           03  F28         PIC X.
           03  F29         PIC X.
           03  F30         PIC X.
           03  F31         PIC X.
           03  F32         PIC X.
       01  DISPLAY-LIST.
           03  FILLER      PIC X(56) VALUE
           "010 020 030 040 050 060 070 080 090 100 110 120 130 140 ".
           03  FILLER      PIC X(56) VALUE
           "150 160 170 180 190 191 200 210 220 230 240 250 260 270 ".
           03  FILLER     PIC X(20) VALUE
           "280 290 300 310 320.".

      ******************************************************************
      *
      *    E L E M E N T - S P E C I F I C A T I O N S - # 1
      *
      ******************************************************************
       78  MAX                      VALUE 32.
      *-----------------------------------------------------------------
       01  SPEC-TABLE.
      * 1. GP-APPLICATION-FG
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      * 2. GP-ALLOW-RLOGINS
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      * 3. GP-RLOGIN-LOCAL-PROFILE
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      * 4. GP-RLOGIN-REMOTE-PROFILE
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      * 5. GP-BATCH-MORE-THAN-DAY
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      * 6. GP-PRINT-GL-RECAP
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      * 7. GP-DISPLAY-BANK-BAL
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      * 8. GP-REPODATE-PAYDATE
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      * 9. GP-POST-INTO-FUTURE
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      *10. N/A
           03  FILLER      PIC X(8) VALUE "E NA010N".
      *11. GP-VALID-BDATE
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      *12. GP-ARCHIVE-DIRECTORY
           03  FILLER      PIC X(8) VALUE "E NA040 ".
      *13. GP-LI-APPLICATION-FG
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      *14. N/A
           03  FILLER      PIC X(8) VALUE "E NA010N".
      *15. GP-PDTH-GT-SCHED-PAY
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      *16. GP-REF-PHONE
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      *17. GP-USE-CONRATE
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      *18. GP-TIME-ZONE-Q
           03  FILLER      PIC X(8) VALUE "E NA010 ".
      *19. GP-CALL-START-TIME
           03  FILLER      PIC X(8) VALUE "ENNN040 ".
      *20. GP-PENALTY-INT
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      *21. GP-ALLOW-SET-FOR-NONCASH
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      *22. N/A
           03  FILLER      PIC X(8) VALUE "E  A010N".
      *23. N/A
           03  FILLER      PIC X(8) VALUE "E  A010N".
      *24. GP-ALLOW-ENTRY-TRADEIN
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      *25. GP-REPO-CODE
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      *26. GP-ALLOW-MORTGAGE-INT-Z
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      *27. GP-PROM-AMT-MIN-PCT
           03  FILLER      PIC X(8) VALUE "EN N030 ".
      *28. GP-BW-OPT-OUT
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      *29. GP-INTERIM-COUPONS
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      *30. GP-DL-ACH-FG
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      *31. GP-GL-NEW-JE
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      *32. GP-CLEAR-PROM-PDTH
           03  FILLER      PIC X(8) VALUE "E  A010 ".

       01  SPEC-TAB        REDEFINES SPEC-TABLE.
           03  SPEC-REC    OCCURS MAX TIMES.
               05  SPEC    PIC X(7).
               05  CHFG    PIC X.

       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBGB/GPOPM1_DEF.CPY".
       COPY "LIBGB/GPOPM1_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       01 FORM-LINK-AREA    PIC X(2000).
       SCREEN SECTION.
       COPY "LIBGB/GPOPM1_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME FORM-LINK-AREA EXIT-PATHNAME.

      ******************************************************************
      *
      *    M A I N - P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.
           PERFORM SQL-CONNECT.
      D    STOP MESS.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "GPOPM1" TO VDU-FORM-NAME.
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

       READ-GLOBAL.
           MOVE 1 TO GP-KEY.
           PERFORM OPEN-GP-FILE.
           PERFORM READ-GP-FILE.
           PERFORM CLOSE-GP-FILE.
           IF IO-GOOD
              MOVE GP-REC TO GPOR-REC
              PERFORM DISPLAY-FIELDS
            ELSE
              PERFORM GLOBAL-NOT-FOUND
              GO TO REC-GP-MODULE-NO.

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
              IF NOT (FORM-SEL = 2 )
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

           IF GP-ALLOW-RLOGINS = "N"
              IF ELE = 3 OR 4
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
                 F-21 F-22 F-23 F-24 F-25 F-26 F-27 F-28 F-29 F-30
                 F-31 F-32
           DEPENDING ON ELE.

      *=================================================================
      *
      *    S E T - R E C O R D - E L E M E N T S
      *
      *=================================================================
       F-01.
           MOVE VDUBUF TO GP-APPLICATION-FG.
           IF NOT (GP-APPLICATION-FG = "Y" OR "N")
              PERFORM SEND-Y-OR-N
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-02.
           MOVE VDUBUF TO GP-ALLOW-RLOGINS.
           IF NOT (GP-ALLOW-RLOGINS = "Y" OR "N")
              PERFORM SEND-Y-OR-N
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-03.
           MOVE VDUBUF TO GP-RLOGIN-LOCAL-PROFILE
           IF (GP-RLOGIN-LOCAL-PROFILE < LOWER-A
                 OR GP-RLOGIN-LOCAL-PROFILE > LOWER-Z)
                    AND GP-RLOGIN-LOCAL-PROFILE NOT = " "
              PERFORM SEND-A-THRU-Z
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-04.
           MOVE VDUBUF TO GP-RLOGIN-REMOTE-PROFILE
           IF (GP-RLOGIN-REMOTE-PROFILE < LOWER-A
                 OR GP-RLOGIN-REMOTE-PROFILE > LOWER-Z)
                    AND GP-RLOGIN-LOCAL-PROFILE NOT = " "
              PERFORM SEND-A-THRU-Z
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-05.
           MOVE VDUBUF TO GP-BATCH-MORE-THAN-DAY.
           IF NOT (GP-BATCH-MORE-THAN-DAY = "Y" OR "N")
              PERFORM SEND-Y-OR-N
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-06.
           MOVE VDUBUF TO GP-PRINT-GL-RECAP.
           IF NOT (GP-PRINT-GL-RECAP = "Y" OR "N")
              PERFORM SEND-Y-OR-N
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-07.
           MOVE VDUBUF TO GP-DISPLAY-BANK-BAL.
           IF NOT (GP-DISPLAY-BANK-BAL = "Y" OR "N")
              PERFORM SEND-Y-OR-N
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-08.
           MOVE VDUBUF TO GP-REPODATE-PAYDATE.
           IF NOT (GP-REPODATE-PAYDATE = "Y" OR "N")
              PERFORM SEND-Y-OR-N
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-09.
           MOVE VDUBUF TO GP-POST-INTO-FUTURE.
           IF NOT (GP-POST-INTO-FUTURE = "Y" OR "N")
              PERFORM SEND-Y-OR-N
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-10.
           GO TO CREATE-ROUTINE.
       F-11.
           MOVE VDUBUF TO GP-VALID-BDATE.
           IF NOT (GP-VALID-BDATE = "Y" OR "N")
              PERFORM SEND-Y-OR-N
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-12.
           MOVE VDUBUF TO GP-ARCHIVE-DIRECTORY.
           GO TO CREATE-ROUTINE.
       F-13.
           MOVE VDUBUF TO GP-LI-APPLICATION-FG.
           IF NOT (GP-LI-APPLICATION-FG = "Y" OR "N")
              PERFORM SEND-Y-OR-N
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-14.
           GO TO CREATE-ROUTINE.
       F-15.
           MOVE VDUBUF TO GP-PDTH-GT-SCHED-PAY.
           IF NOT (GP-PDTH-GT-SCHED-PAY = "Y" OR "N")
              PERFORM SEND-Y-OR-N
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.

       F-16.
           MOVE VDUBUF TO GP-REF-PHONE.
           IF NOT (GP-REF-PHONE = "Y" OR "N")
              PERFORM SEND-Y-OR-N
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.

       F-17.
           MOVE VDUBUF TO GP-USE-CONRATE.
           IF NOT (GP-USE-CONRATE = "Y" OR "N")
              PERFORM SEND-Y-OR-N
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.

       F-18.
           MOVE VDUBUF TO GP-TIME-ZONE-Q.
           IF NOT (GP-TIME-ZONE-Q = "Y" OR "N")
              PERFORM SEND-Y-OR-N
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-19.
           MOVE VDUNUM0 TO TEST-24-TIME.
           MOVE "S  A040190." TO VDU.
           MOVE TEST-24-TIME-ALP TO VDUBUF.
           PERFORM SCREENIO.
           IF (TEST-24-HH > 23 OR TEST-24-MM > 59)
              MOVE "PLEASE ENTER A TIME IN THE RANGE OF 0000 - 2359"
                                                             TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           MOVE TEST-24-TIME TO GP-CALL-START-TIME.
       F-191.
           MOVE "ENNN040191." TO VDU.
           PERFORM SCREENIO.
           IF UP-KEY GO TO ENTER-ELE.
           IF NOT ENTER-KEY GO TO F-191.
           MOVE VDUNUM0 TO TEST-24-TIME.
           MOVE "S  A004191." TO VDU.
           MOVE TEST-24-TIME-ALP TO VDUBUF.
           PERFORM SCREENIO.
           IF (TEST-24-HH > 24 OR TEST-24-MM > 59)
              OR (TEST-24-HH = 24 AND TEST-24-MM NOT = 00)
              OR (TEST-24-TIME = 0000)
      *       OR (TEST-24-TIME = 0000 AND GP-CALL-START-TIME NOT = 0)
              MOVE "PLEASE ENTER A TIME IN THE RANGE OF 0001 - 2400"
                                                             TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           IF GP-CALL-START-TIME > TEST-24-TIME
              MOVE " INVALID TIME RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO F-191.
           MOVE TEST-24-TIME TO GP-CALL-END-TIME.
           GO TO CREATE-ROUTINE.
       F-20.
           MOVE VDUBUF TO GP-PENALTY-INT
           IF NOT (GP-PENALTY-INT = "Y" OR "N")
              PERFORM SEND-Y-OR-N
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-21.
           MOVE VDUBUF TO GP-ALLOW-SET-FOR-NONCASH.
           IF NOT (GP-ALLOW-SET-FOR-NONCASH = "Y" OR "N")
              PERFORM SEND-Y-OR-N
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-22.
           GO TO CREATE-ROUTINE.
       F-23.
           GO TO CREATE-ROUTINE.
       F-24.
           MOVE VDUBUF TO GP-ALLOW-ENTRY-TRADEIN.
           IF NOT (GP-ALLOW-ENTRY-TRADEIN = "Y" OR "N")
              PERFORM SEND-Y-OR-N
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.

       F-25.
           MOVE VDUBUF TO GP-REPO-CODE.
           IF NOT (GP-REPO-CODE-VALID)
              MOVE "ENTER: SPACE OR 'A' " TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.

       F-26.
           MOVE VDUBUF TO GP-ALLOW-MORTGAGE-INT-Z.
           IF NOT (GP-ALLOW-MORTGAGE-INT-Z = "Y" OR "N")
              PERFORM SEND-Y-OR-N
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.

       F-27.
           MOVE VDUNUM0 TO GP-PROM-AMT-MIN-PCT.
           GO TO CREATE-ROUTINE.

       F-28.
           MOVE VDUBUF TO GP-BW-PRIVACY-OPT-OUT.
           IF NOT (GP-BW-PRIVACY-OPT-OUT = "Y" OR "N")
              PERFORM SEND-Y-OR-N
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.

       F-29.
           MOVE VDUBUF TO GP-INTERIM-COUPONS.
           IF NOT (GP-INTERIM-COUPONS = "Y" OR "N")
              PERFORM SEND-Y-OR-N
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.


       F-30.
           MOVE VDUBUF TO GP-DL-ACH-FG.
           IF NOT (GP-DL-ACH-FG = "Y" OR "N")
              PERFORM SEND-Y-OR-N
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.

       F-31.
           MOVE VDUBUF TO GP-GL-NEW-JE.
           IF NOT (GP-GL-NEW-JE = "Y" OR "N")
              PERFORM SEND-Y-OR-N
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.

       F-32.
           MOVE VDUBUF TO GP-CLEAR-PROM-PDTH.
           IF NOT (GP-CLEAR-PROM-PDTH = "Y" OR "N")
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

           IF GPOR-APPLICATION-FG NOT = GPOR-APPLICATION-FG
              MOVE "01 "                   TO LMC-FIELD
              MOVE "APPLICA FLAG"          TO LMC-DESC
              MOVE GPOR-APPLICATION-FG     TO LMC-ORG-TEXT
              MOVE GPCH-APPLICATION-FG     TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-ALLOW-RLOGINS NOT = GPCH-ALLOW-RLOGINS
              MOVE "02 "                   TO LMC-FIELD
              MOVE "ALLOW RLOGINS "        TO LMC-DESC
              MOVE GPOR-ALLOW-RLOGINS      TO LMC-ORG-TEXT
              MOVE GPCH-ALLOW-RLOGINS      TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-RLOGIN-LOCAL-PROFILE NOT =
                                 GPCH-RLOGIN-LOCAL-PROFILE
              MOVE "03 "                   TO LMC-FIELD
              MOVE "RLOGIN LOCAL P"        TO LMC-DESC
              MOVE GPOR-RLOGIN-LOCAL-PROFILE  TO LMC-ORG-TEXT
              MOVE GPCH-RLOGIN-LOCAL-PROFILE  TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-RLOGIN-REMOTE-PROFILE NOT =
                                 GPCH-RLOGIN-REMOTE-PROFILE
              MOVE "04 "                   TO LMC-FIELD
              MOVE "RLOGIN REMTE P"        TO LMC-DESC
              MOVE GPOR-RLOGIN-REMOTE-PROFILE  TO LMC-ORG-TEXT
              MOVE GPCH-RLOGIN-REMOTE-PROFILE  TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-BATCH-MORE-THAN-DAY NOT = GPCH-BATCH-MORE-THAN-DAY
              MOVE "05 "                   TO LMC-FIELD
              MOVE "BATCH MORE DAY"        TO LMC-DESC
              MOVE GPOR-BATCH-MORE-THAN-DAY      TO LMC-ORG-TEXT
              MOVE GPCH-BATCH-MORE-THAN-DAY      TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-PRINT-GL-RECAP NOT = GPCH-PRINT-GL-RECAP
              MOVE "06 "                   TO LMC-FIELD
              MOVE "PRINT GL RECAP"        TO LMC-DESC
              MOVE GPOR-PRINT-GL-RECAP      TO LMC-ORG-TEXT
              MOVE GPCH-PRINT-GL-RECAP      TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-DISPLAY-BANK-BAL NOT = GPCH-DISPLAY-BANK-BAL
              MOVE "07 "                   TO LMC-FIELD
              MOVE "DISP BANK BAL "        TO LMC-DESC
              MOVE GPOR-DISPLAY-BANK-BAL      TO LMC-ORG-TEXT
              MOVE GPCH-DISPLAY-BANK-BAL      TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-REPODATE-PAYDATE NOT = GPCH-REPODATE-PAYDATE
              MOVE "08 "                   TO LMC-FIELD
              MOVE "REPODAT PAYDAT"        TO LMC-DESC
              MOVE GPOR-REPODATE-PAYDATE      TO LMC-ORG-TEXT
              MOVE GPCH-REPODATE-PAYDATE      TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-POST-INTO-FUTURE NOT = GPCH-POST-INTO-FUTURE
              MOVE "09 "                   TO LMC-FIELD
              MOVE "POST IN FUTURE"        TO LMC-DESC
              MOVE GPOR-POST-INTO-FUTURE      TO LMC-ORG-TEXT
              MOVE GPCH-POST-INTO-FUTURE      TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-VALID-BDATE NOT = GPCH-VALID-BDATE
              MOVE "11 "                   TO LMC-FIELD
              MOVE "VALID BDATE   "        TO LMC-DESC
              MOVE GPOR-VALID-BDATE      TO LMC-ORG-TEXT
              MOVE GPCH-VALID-BDATE      TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-ARCHIVE-DIRECTORY NOT = GPCH-ARCHIVE-DIRECTORY
              MOVE "12 "                   TO LMC-FIELD
              MOVE "ARCH DIRECTOYY"        TO LMC-DESC
              MOVE GPOR-ARCHIVE-DIRECTORY      TO LMC-ORG-TEXT
              MOVE GPCH-ARCHIVE-DIRECTORY      TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-LI-APPLICATION-FG NOT = GPCH-LI-APPLICATION-FG
              MOVE "13 "                   TO LMC-FIELD
              MOVE "LI APP FLAG   "        TO LMC-DESC
              MOVE GPOR-LI-APPLICATION-FG      TO LMC-ORG-TEXT
              MOVE GPCH-LI-APPLICATION-FG      TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-PDTH-GT-SCHED-PAY NOT = GPCH-PDTH-GT-SCHED-PAY
              MOVE "15 "                   TO LMC-FIELD
              MOVE "PDTH > SCH PAY"        TO LMC-DESC
              MOVE GPOR-PDTH-GT-SCHED-PAY      TO LMC-ORG-TEXT
              MOVE GPCH-PDTH-GT-SCHED-PAY      TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-REF-PHONE NOT = GPCH-REF-PHONE
              MOVE "16 "                   TO LMC-FIELD
              MOVE "REF PHONE     "        TO LMC-DESC
              MOVE GPOR-REF-PHONE      TO LMC-ORG-TEXT
              MOVE GPCH-REF-PHONE      TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-USE-CONRATE NOT = GPCH-USE-CONRATE
              MOVE "17 "                   TO LMC-FIELD
              MOVE "USE CONRATE   "        TO LMC-DESC
              MOVE GPOR-USE-CONRATE      TO LMC-ORG-TEXT
              MOVE GPCH-USE-CONRATE      TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-TIME-ZONE-Q NOT = GPCH-TIME-ZONE-Q
              MOVE "18 "                   TO LMC-FIELD
              MOVE "TIME ZONE QUE "        TO LMC-DESC
              MOVE GPOR-TIME-ZONE-Q      TO LMC-ORG-TEXT
              MOVE GPCH-TIME-ZONE-Q      TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-CALL-START-TIME NOT = GPCH-CALL-START-TIME
              MOVE "19 "                   TO LMC-FIELD
              MOVE "CALL STARTTIME"        TO LMC-DESC
              MOVE GPOR-CALL-START-TIME      TO LMC-ORG-NUM
              MOVE GPCH-CALL-START-TIME      TO LMC-CHG-NUM
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-CALL-END-TIME NOT = GPCH-CALL-END-TIME
              MOVE "191"                   TO LMC-FIELD
              MOVE "CALL END TIME "        TO LMC-DESC
              MOVE GPOR-CALL-END-TIME      TO LMC-ORG-NUM
              MOVE GPCH-CALL-END-TIME      TO LMC-CHG-NUM
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-PENALTY-INT NOT = GPCH-PENALTY-INT
              MOVE "20 "                   TO LMC-FIELD
              MOVE "PENALTY INT   "        TO LMC-DESC
              MOVE GPOR-PENALTY-INT      TO LMC-ORG-TEXT
              MOVE GPCH-PENALTY-INT      TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-ALLOW-SET-FOR-NONCASH NOT =
                                      GPCH-ALLOW-SET-FOR-NONCASH
              MOVE "21 "                   TO LMC-FIELD
              MOVE "SET 4 NONCASH "        TO LMC-DESC
              MOVE GPOR-ALLOW-SET-FOR-NONCASH      TO LMC-ORG-TEXT
              MOVE GPCH-ALLOW-SET-FOR-NONCASH      TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-ALLOW-ENTRY-TRADEIN NOT = GPCH-ALLOW-ENTRY-TRADEIN
              MOVE "24 "                   TO LMC-FIELD
              MOVE "ENTRY TRADEIN "        TO LMC-DESC
              MOVE GPOR-ALLOW-ENTRY-TRADEIN      TO LMC-ORG-TEXT
              MOVE GPCH-ALLOW-ENTRY-TRADEIN      TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-REPO-CODE NOT = GPCH-REPO-CODE
              MOVE "25 "                   TO LMC-FIELD
              MOVE "REPO CODE     "        TO LMC-DESC
              MOVE GPOR-REPO-CODE      TO LMC-ORG-TEXT
              MOVE GPCH-REPO-CODE      TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-ALLOW-MORTGAGE-INT-Z NOT = GPCH-ALLOW-MORTGAGE-INT-Z
              MOVE "26 "                   TO LMC-FIELD
              MOVE "MORTG INT Z   "        TO LMC-DESC
              MOVE GPOR-ALLOW-MORTGAGE-INT-Z      TO LMC-ORG-TEXT
              MOVE GPCH-ALLOW-MORTGAGE-INT-Z      TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-PROM-AMT-MIN-PCT NOT = GPCH-PROM-AMT-MIN-PCT
              MOVE "27 "                   TO LMC-FIELD
              MOVE "PROM AMT MIN %"        TO LMC-DESC
              MOVE GPOR-PROM-AMT-MIN-PCT      TO LMC-ORG-NUM
              MOVE GPCH-PROM-AMT-MIN-PCT      TO LMC-CHG-NUM
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-BW-PRIVACY-OPT-OUT NOT = GPCH-BW-PRIVACY-OPT-OUT
              MOVE "28 "                   TO LMC-FIELD
              MOVE "PRIVACY OPTOUT"        TO LMC-DESC
              MOVE GPOR-BW-PRIVACY-OPT-OUT      TO LMC-ORG-TEXT
              MOVE GPCH-BW-PRIVACY-OPT-OUT      TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-INTERIM-COUPONS NOT = GPCH-INTERIM-COUPONS
              MOVE "29 "                   TO LMC-FIELD
              MOVE "INTERIM COUPON"        TO LMC-DESC
              MOVE GPOR-INTERIM-COUPONS      TO LMC-ORG-TEXT
              MOVE GPCH-INTERIM-COUPONS      TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-DL-ACH-FG NOT = GPCH-DL-ACH-FG
              MOVE "30 "                   TO LMC-FIELD
              MOVE "DEALER ACH    "        TO LMC-DESC
              MOVE GPOR-DL-ACH-FG      TO LMC-ORG-TEXT
              MOVE GPCH-DL-ACH-FG      TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-GL-NEW-JE NOT = GPCH-GL-NEW-JE
              MOVE "31 "                   TO LMC-FIELD
              MOVE "NEW JOURNL ENT"        TO LMC-DESC
              MOVE GPOR-GL-NEW-JE      TO LMC-ORG-TEXT
              MOVE GPCH-GL-NEW-JE      TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-CLEAR-PROM-PDTH NOT = GPCH-CLEAR-PROM-PDTH
              MOVE "32 "                   TO LMC-FIELD
              MOVE "CLEAR PROMPDTH"        TO LMC-DESC
              MOVE GPOR-CLEAR-PROM-PDTH      TO LMC-ORG-TEXT
              MOVE GPCH-CLEAR-PROM-PDTH      TO LMC-CHG-TEXT
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

           MOVE SPACES                    TO DISPLAY-BUF.
           MOVE GP-APPLICATION-FG         TO F01.
           MOVE GP-ALLOW-RLOGINS          TO F02.
           MOVE GP-RLOGIN-LOCAL-PROFILE   TO F03.
           MOVE GP-RLOGIN-REMOTE-PROFILE  TO F04.
           MOVE GP-BATCH-MORE-THAN-DAY    TO F05.
           MOVE GP-PRINT-GL-RECAP         TO F06.
           MOVE GP-DISPLAY-BANK-BAL       TO F07.
           MOVE GP-REPODATE-PAYDATE       TO F08.
           MOVE GP-POST-INTO-FUTURE       TO F09.
           MOVE SPACES                    TO F10.
           MOVE GP-VALID-BDATE            TO F11.
           MOVE GP-ARCHIVE-DIRECTORY      TO F12.
           MOVE GP-LI-APPLICATION-FG      TO F13.
           MOVE SPACES                    TO F14.
           MOVE GP-PDTH-GT-SCHED-PAY      TO F15.
           MOVE GP-REF-PHONE              TO F16.
           MOVE GP-USE-CONRATE            TO F17.
           MOVE GP-TIME-ZONE-Q            TO F18.
           MOVE GP-CALL-START-TIME        TO F19.
           MOVE GP-CALL-END-TIME          TO F191.
           MOVE GP-PENALTY-INT            TO F20.
           MOVE GP-ALLOW-SET-FOR-NONCASH  TO F21.
           MOVE SPACES                    TO F22.
           MOVE SPACES                    TO F23.
           MOVE GP-ALLOW-ENTRY-TRADEIN    TO F24.
           MOVE GP-REPO-CODE              TO F25.
           MOVE GP-ALLOW-MORTGAGE-INT-Z   TO F26.
           MOVE GP-PROM-AMT-MIN-PCT       TO F27.
           MOVE GP-BW-PRIVACY-OPT-OUT     TO F28.
           MOVE GP-INTERIM-COUPONS        TO F29.
           MOVE GP-DL-ACH-FG              TO F30.
           MOVE GP-GL-NEW-JE              TO F31.
           MOVE GP-CLEAR-PROM-PDTH        TO F32.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".

      **************************************************************************
       FORM-RESET SECTION.
      **************************************************************************
           MOVE EXT-CONAME-CONAME TO GPOPM1-CONAME.
           MOVE EXT-CONAME-SYSDATE TO GPOPM1-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY GPOPM1-SCREEN.
           DISPLAY GPOPM1-SCREEN UPON GPOPMN-WINDOW-HANDLE.
           MOVE GPOPM1-FORM-FIELDS TO WR-BUF. 
           MOVE GPOPM1-FORM-LIST TO WR-LIST. 
           PERFORM CALL-WRFORM. 

       FORM-RESET-EXIT.
           EXIT.

      **************************************************************************
       SETUP-LINK-INFO SECTION.
      **************************************************************************
           MOVE GPOPM1-HELP--FILE TO HELP-LINK-FILE.
           MOVE GPOPM1-HELP--DIR TO HELP-LINK-DIR.

      **************************************************************************
       VDU-CONVERT-FIELD SECTION.
      **************************************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBGB/GPOPM1_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.


      ************************************
       MISC-PARAGRAPHS SECTION.
      ************************************
       COPY "LIBGB/SYSTEM.CPY".
       SEND-A-THRU-Z.
           STRING "MUST BE '"
                  LOWER-A
                  "' THRU '"
                  LOWER-Z  DELIMITED BY SIZE INTO MESS.
           PERFORM SEND-MESS.
      *-----------------------------------------------------------------
       SEND-Y-OR-N.
           MOVE "MUST BE 'Y' OR 'N'" TO MESS.
           PERFORM SEND-MESS.
      *-----------------------------------------------------------------
       GLOBAL-NOT-FOUND.
           MOVE "GLOBAL PARAMETER RECORD NOT FOUND!" TO MESS.
           PERFORM SEND-MESS.

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
           DESTROY GPOPM1-SCREEN.
           MOVE FORM-PATH TO FORM-PATHNAME.
           MOVE GPOPMN-LINK TO FORM-LINK-AREA.
           EXIT PROGRAM.
