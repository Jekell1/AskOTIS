       IDENTIFICATION DIVISION.
       PROGRAM-ID.      BRMAN1.
      *********************************************************
      *  DIR :   GB
      *  DESC:   BRANCH FILE MAINTENANCE PAGE 1
      * REV :
      *  MJD 063095 ADDED BR-BANK-CLEARING-GL
      *                   BR-BANK-ROUTING-NO
      *                   BR-BANK-ACCTNO.
      *  JTG 070595 ADDED CLEAR OF
      *             BR-REB-METHOD-2 BR-ACCRUAL-MONTHS-2
      *             BR-REBDAYS-MONTH-2
      *  JTG 072095 ADDED BR-GLBANK-DEPOSIT
      *  JFF 082896 ADDED GL-SCAN WINDOW.
      *  MJD 071797 ADDED BR-CAN-GROUP-ID FOR REFERENCE ONLY.
      *  MJD 082797 ADDED CLEAR OF BR-NO-FT-EMP AND BR-NO-PT-EMP.
      *  MJD 112197 EXPANDED BR-REC TO 850 BYTES
      *  KEC 121797 ADDED PAGE 7.
      *  MJD 031398 ADDED 78 LEVEL FOR BR REC SIZE
      *  BAH 052998 ALLOW BR-PRINTERFG "O" FOR OKIDATA TO ADJUST
      *             PRINT SIZE IN WI/K2180
      *  BAH 061098 ALLOW BR-PRINTERFG "L" FOR LASER TO ADJUST
      *             PRINT SIZE IN WI/K2180
      *  BLV 090298 TRASHED BR-EOM1-PROCEDURE, BR-EOM2-PROCEDURE;
      *             FIELDS NOT USED & ARE CONFUSING
      *  CS  102098 ADDED BR-FAX-NUMBER
      *  MJD 990226 ADDED PRINTER OPTION "2" FOR LASERJET IIP
      *  MJD 990323 ALLOW BR-PRINTERFG "P" FOR PANASONIC KXP-2130
      *             TO ADJUST PRINT SIZE IN WI/K2180
      *  BAH 990413 ADDED CALL TO CREATE CHANGES FILE AFTER REWRITE
      *  MJD 990722 REMOVED HARD CODED CLEAR AND ADDED CLEAR ROUTINE
      *  MG  991005 FIXED VDUSTATUS/IO-FG/STAT PER DEVELOPMENT MEETING.
      *  CS  040992 ADD BR-REGION-NUMBER [PR#76 REGIONAL]
      *  AMM 070305 CREATED A LOG FIELD CHANGE ROUTINE
      *  BAH 120514 REMOVED SPECIAL-FG FROM CONAME-BUF
      *   CS 130315 REMOVED LIBGB/TMMSG, NOW IN DECLARATIVES
      *  BAH 140911 FIXED PHONE ENTRY
      * KEC 2016.0309 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED CD-CL???? TO BE CD-CL-????.
      *  BLM 160920 ADD FIELD #28, BR-CHECK-DEPOSIT-GL, WORLD PR# 1188 
      * KEC 2018.0329 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         THE IO ROUTINES FOR THE BRANCH FILE (BRFILE) ARE
      *         HARDCODED SO ADDED TO THE WRITE & REWRITE PARAGRAPHS
      *         'ACCEPT BR-LTOUCH-DATE FROM CENTURY-DATE.'
      * KEC 20181008 REMOVED THE HARDCODED IO OF THE BRFILE AND
      *              ADDED LIBGB/GBBRIN; NOT SURE WHY I FELT IT WAS
      *              NEEDED TO BE HARDCODED.  TESTED THIS IN DEBUG
      *              AND STANDARD LOAD-BR-FILE PROPERLY WORKS.
      *  CS 20190124 ADD B2-WKDAY-BANKID, WORKDAY BANK INTEGRATION 
      * BLM 20211022 FIX GL ACCT ENTRIES SO YOU MAY ENTER WITH OR WITHOUT
      *              DASH AND WITH OR WITHOUT LEADING ZEROS
      * BAH 20211116 LIMIT ACCESS BY PASSWORD TO ALL FIELDS
      *              EXCEPT 21,22, AND 29, #1524
      * BAH 2022.0314 REMOVED EXT-SPANISH-FORMS 
      *****************************************************************
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
           "@(#)GB/BRMAN1.CBL S35 09/20/22-12:49:47 >".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGL/GL01GL.CPY".
       COPY "LIBGL/GL01GL_SQL.CPY".
       COPY "LIBGL/GLWSGL.CPY".
       COPY "LIBLP/LP01ST.CPY".
       COPY "LIBLP/LP01ST_SQL.CPY".
       COPY "LIBLP/LPWSST.CPY".
       COPY "LIBLP/LP01CK.CPY".
       COPY "LIBLP/LP01CK_SQL.CPY".
       COPY "LIBLP/LPWSCK.CPY".
       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LP01CD_SQL.CPY".
       COPY "LIBLP/LPWSCD.CPY".

      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  ORIGINAL BRANCH RECORD BROR
      *  CHANGED  BRANCH RECORD BRCH
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       COPY "LIBGB/GB01BR.CPY" REPLACING LEADING "BR" BY "BROR".
       COPY "LIBGB/GB01BR.CPY" REPLACING LEADING "BR" BY "BRCH".

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
       01  HOLD-KEY        PIC 9(4).

      *--- VERIFY CHECK DIGIT WORKERS -----------------------------
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
      *------------------------------------------------------------

       01  BRMAIN-LINK       VALUE SPACES.
           03  FORM-NAME     PIC X(20).
           03  SAVEKEY       PIC X(4).
           03  NEW-REC       PIC X.
           03  ERRCD         PIC X.
           03  FORMNUM       PIC 9.
           03  BRMAIN-WINDOW-HANDLE PIC X(10).

       01  SCREEN-GL.
           03  SC-BRANCH   PIC 9(4).
           03  SC-DASH     PIC X.
           03  SC-NUMBER   PIC 9(7).
       01  SCREEN-GL-NUM REDEFINES SCREEN-GL PIC 9(12).
       01  WORK-GL         PIC 9(11).
       01  WORK-NUMBER REDEFINES WORK-GL.
           03  WORK-BR     PIC 9(4).
           03  WORK-NO     PIC 9(7).
       01  DISP-GL.
           03  DISP-BR     PIC 9(4).
           03  FILLER      PIC X VALUE "-".
           03  DISP-NO     PIC 9(7).
      *----------------------------------------------------------
      * USED FOR GL WINDOW SCAN
       01  GL-SCAN-ERR         PIC X                 VALUE "N".
       01  W-G-GLNO.
           03  W-G-BR      PIC 9(4).
           03  W-G-DASH    PIC X VALUE "-".
           03  W-G-NUM     PIC 9(7).

       01  SUB                 PIC 9(6) COMP-3       VALUE 0.

      *    **********************************
      *    *      SCREEN DISPLAY BUFFER
      *    **********************************
       01  DISPLAY-BUF    VALUE SPACES.
           03  F01         PIC X(40).
           03  F02         PIC X(24).
           03  F03         PIC X(24).
           03  F04         PIC X(15).
           03  F05         PIC X(2).
           03  F06         PIC X(10).
           03  F07         PIC X(12).
           03  F071        PIC X(12).

      * PRINTERS
           03  F08         PIC 99.
           03  F081        PIC X.
           03  F09         PIC 99.
           03  F091        PIC X.
           03  F10         PIC 99.
           03  F101        PIC X.
           03  F11         PIC 99.
           03  F111        PIC X.
      * NEXT CHECK
           03  F12         PIC Z(6).

      * FEE GL BRNO
           03  F13         PIC ZZZ9.

      * CAN LOAN GROUP ID AD DESCRIPTION: REF ONLY, CHANGE IN BROPMN
           03  D-CAN-GROUP-ID PIC X(4).
           03  D-CAN-DESC     PIC X(20).

      * BANK BALANCE
           03  F14         PIC ZZZ,ZZZ,ZZ9.99-.
      * GL #'S
           03  F15         PIC X(12).
           03  F16         PIC X(12).
           03  F17         PIC X(12).
           03  F18         PIC X(12).
           03  F19         PIC X(12).

           03  F20         PIC X(12).
           03  F21         PIC Z(9).
           03  F22         PIC X(17).

      * DATA PATH
           03  F23         PIC X(6).
      * MACHINE
           03  F24         PIC X(2).
      * MANAGER
           03  F25         PIC X(25).
      * EOD BATCH PROCEDURE CODE
           03  F26         PIC X(3).
      * REGION NUMBER
           03  F27         PIC 9(2).
      * CHECK DEPOSIT GL#
           03  F28         PIC X(12).
      * WORKDAY BANK ID
           03  F29         PIC 9(4).
       01  DISPLAY-LIST.
           03  FILLER      PIC X(56) VALUE
           "010 020 030 040 050 060 070 071 080 081 090 091 100 101 ".
           03  FILLER      PIC X(28) VALUE
           "110 111 120 130 GRID GRDESC ".
           03  FILLER      PIC X(36) VALUE
           "140 150 160 170 180 190 200 210 220 ".
           03  FILLER      PIC X(28) VALUE
           "230 240 250 260 270 280 290.".

      *    **********************************
      *    *   ELEMENT SPECIFICATIONS #1
      *    **********************************
       01  MAX             PIC 99   VALUE 29.
       01  SPEC-TABLE.
      * 1. BRANCH NAME
           03  FILLER      PIC X(8) VALUE "E NA400 ".
      * 2. ADDRESS 1
           03  FILLER      PIC X(8) VALUE "E NA240 ".
      * 3. ADDRESS 2
           03  FILLER      PIC X(8) VALUE "E NA240 ".
      * 4. CITY
           03  FILLER      PIC X(8) VALUE "E NA150 ".
      * 5. STATE
           03  FILLER      PIC X(8) VALUE "E NA020 ".
      * 6. ZIP
           03  FILLER      PIC X(8) VALUE "E NA100 ".
      * 7. PHONE
           03  FILLER      PIC X(8) VALUE "E  A120 ".
      * 8. # OF PRINTER 1
           03  FILLER      PIC X(8) VALUE "ENNN020 ".
      * 9. # OF PRINTER 2
           03  FILLER      PIC X(8) VALUE "ENNN020 ".
      *10. # OF PRINTER 3
           03  FILLER      PIC X(8) VALUE "ENNN020 ".
      *11. # OF PRINTER 4
           03  FILLER      PIC X(8) VALUE "ENNN020 ".
      *12. NEXT CHECK NUMBER
           03  FILLER      PIC X(8) VALUE "ENNN060 ".
      *13. FEE CODE GLNO BRANCH
           03  FILLER      PIC X(8) VALUE "ENNN040 ".
      *14. BANK BALANCE
           03  FILLER      PIC X(8) VALUE "E NS092N".
      *15. BANK GL #
           03  FILLER      PIC X(8) VALUE "E RA120 ".
      *16. BANK DEPOSIT GL #
           03  FILLER      PIC X(8) VALUE "E RA120 ".
      *17. CASH ON HAND GL #
           03  FILLER      PIC X(8) VALUE "E RA120 ".
      *18. CASH OVER/SHORT GL #
           03  FILLER      PIC X(8) VALUE "E RA120 ".
      *19. RETAINED EARNINGS GL NUMBER
           03  FILLER      PIC X(8) VALUE "E RA120 ".
      *20. BANK CLEARING GL NUMBER
           03  FILLER      PIC X(8) VALUE "E RA120 ".
      *21. BANK-ROUTING NUMBER
           03  FILLER      PIC X(8) VALUE "ENNN090 ".
      *22. BANK ACCOUNT NUMBER
           03  FILLER      PIC X(8) VALUE "E NA170 ".
      *23. DATA PATH
           03  FILLER      PIC X(8) VALUE "E NA060 ".
      *24. MACHINE
           03  FILLER      PIC X(8) VALUE "E NA020 ".
      *25. MANAGER
           03  FILLER      PIC X(8) VALUE "E NA250 ".
      *26. EOD BATCH
           03  FILLER      PIC X(8) VALUE "E NA030 ".
      *27. REGION NUMBER
           03  FILLER      PIC X(8) VALUE "ENNN020 ".
      *28. CHECK DEPOSIT GL #
           03  FILLER      PIC X(8) VALUE "E RA120 ".
      *29. WKDAY BANK ID
           03  FILLER      PIC X(8) VALUE "ENNN040 ".

       01  SPEC-TAB       REDEFINES SPEC-TABLE.
           03  SPEC-REC   OCCURS 29 TIMES.
               05  SPEC   PIC X(7).   

               05  CHFG   PIC X.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/PASSWDW.CPY".
       78  FIELDS-SEQ                VALUE 0.
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/PHON400.CPY".
       COPY "LIBWI/GLSCN2W.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBGB/BRMAN1_DEF.CPY".
       COPY "LIBGB/BRMAN1_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       01 FORM-LINK-AREA    PIC X(2000).
       SCREEN SECTION.
       COPY "LIBGB/BRMAN1_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME FORM-LINK-AREA EXIT-PATHNAME.

      *******************************************
       MAIN-PROGRAM SECTION.
      *******************************************
           PERFORM SQL-CONNECT.
      D    STOP MESS.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "BRMAN1" TO VDU-FORM-NAME.
           MOVE FORM-LINK-AREA TO BRMAIN-LINK.

           MOVE " " TO VDU-WIMENU-SELECTION
                       VDU-PGMENU-SELECTION.

       MAIN-MODULE.

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
           IF IO-TYPE = "DEL"
              PERFORM DELETE-MODULE.
           IF NEW-REC = "Y"
              MOVE "P" TO FORM-P
              MOVE 2 TO FORM-SEL.
           IF FORM-SEL NOT = 0
              IF FORM-P = "P"
                 MOVE BR-NO TO SAVEKEY
                 MOVE FORM-SEL TO FORMNUM
                 GO TO END-ROUTINE.

           MOVE "N" TO NEW-REC.

           GO TO MAIN-MODULE.

      *************************************************
       REC-BR-MODULE SECTION.
      *************************************************
       REC-BR-MODULE-NO.
           MOVE SPACES TO IO-TYPE.
           IF SAVEKEY NOT = " "
              MOVE SAVEKEY TO BR-NO
              MOVE " " TO SAVEKEY
              MOVE "S  A040BRANCH." TO VDU
              MOVE BR-NO TO VDUBUF
              PERFORM SCREENIO
              GO TO READ-BRANCH.

           MOVE 0 TO NEW-FLAG.
           MOVE "EN N040BRANCH." TO VDU.
           PERFORM SCREENIO.
           IF F6-KEY
              PERFORM ENTER-SCANKEY
              GO TO END-REC-BR-MODULE.
           IF F7-KEY
              MOVE "END" TO IO-TYPE
              GO TO END-REC-BR-MODULE.
           IF NOT ENTER-KEY
              GO TO REC-BR-MODULE-NO.
           IF VDUNUM0 = 0
              GO TO REC-BR-MODULE-NO.
           MOVE VDUNUM0 TO BR-NO HOLD-KEY BR-NO.
       READ-BRANCH.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF IO-GOOD
              MOVE BR-REC TO BROR-REC
              PERFORM DISPLAY-FIELDS
              MOVE 0 TO NEW-FLAG
            ELSE
              PERFORM NEW-RECORD-SETUP.
       END-REC-BR-MODULE.
           EXIT.

      *************************************************
      *       ENTRY-MODULE
      *       NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *                  1 - INITIAL NEW RECORD ENTRY
      *                  2 - MODIFY NEW RECORD
      *************************************************
       ENTRY-MODULE SECTION.
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
           IF NOT ENTER-KEY
              GO TO MODIFY-ROUTINE.

           MOVE VDUBUF TO ACTION.
           IF ACTION = SPACES
              MOVE "RWR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF ACTION = "DELETE"
              MOVE "DEL" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF FORM-KEY = " " AND FORM-P = "P" AND FORM-SEL NUMERIC
              IF NOT (FORM-SEL = 2 OR 3 OR 4 OR 5 OR 6 OR 7)
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

      * LIMIT ACCESS TO ALL FIELDS EXCEPT 21, 22 AND 29
           IF NOT (ELE = 21 OR 22 OR 29)
              IF NEW-FLAG = 0
                 MOVE FIELDS-SEQ                    TO PASSWDW-SEQ
                 MOVE "TO CHANGE THIS FIELD......." TO PASSWDW-PROMPT2
                 PERFORM VERIFY-PASSWORD
                 IF PASSWDW-STAT NOT = "OK"
                    GO TO MODIFY-ROUTINE.

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
       TEST-F4.
           IF UP-KEY
             IF NEW-FLAG = 1
              MOVE -1 TO ONE
              GO TO CREATE-ROUTINE.
       TEST-F5.
           IF DOWN-KEY
              MOVE 1 TO ONE
              GO TO GOTO-FIELD1.
       TEST-F6.
           IF F6-KEY
              IF ELE = 15 OR 16 OR 17 OR 18 OR 19 OR 20 OR 28
                 PERFORM SCAN-GL-FILE
                 IF GL-SCAN-ERR = "Y"
                    GO TO ENTER-ELE.

      *    IF NOT ENTER-KEY
      *       GO TO ENTER-ELE.
           MOVE 1 TO ONE.
       TEST-F1.
           IF F1-KEY
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT ENTER-KEY
              GO TO ENTER-ELE.
           MOVE 1 TO ONE.
       GOTO-FIELD1.
           GO TO F-01 F-02 F-03 F-04 F-05 F-06 F-07
                 F-08 F-09 F-10 F-11 F-12 F-13 F-14
                 F-15 F-16 F-17 F-18 F-19 F-20 F-21
                 F-22 F-23 F-24 F-25 F-26 F-27 F-28
                 F-29
                 DEPENDING ON ELE.
           GO TO END-ENTRY-MODULE.
      *    **********************************
      *    *      SET RECORD ELEMENTS
      *    **********************************
       F-01.
           MOVE VDUBUF TO BR-NAME.
           GO TO CREATE-ROUTINE.
       F-02.
           MOVE VDUBUF TO BR-ADR1.
           GO TO CREATE-ROUTINE.
       F-03.
           MOVE VDUBUF TO BR-ADR2.
           GO TO CREATE-ROUTINE.
       F-04.
           MOVE VDUBUF TO BR-CITY.
           GO TO CREATE-ROUTINE.
       F-05.
           PERFORM OPEN-ST1-FILE.
           MOVE VDUBUF TO ST-CODE.
           PERFORM READ-ST1-FILE.
           IF IO-BAD
              GO TO ENTER-ELE.
           PERFORM CLOSE-ST1-FILE.
           MOVE VDUBUF TO BR-STATE.
           GO TO CREATE-ROUTINE.
       F-06.
           MOVE VDUBUF TO BR-ZIP.
           GO TO CREATE-ROUTINE.
       F-07.
           PERFORM ENTER-PHONE.
           IF TEL-BUFX = " "
              MOVE 0 TO TEL-BUF.
           IF TEL-BUFX IS NOT NUMERIC
              GO TO ENTER-ELE.
           MOVE TEL-BUF TO BR-PHONE.
           PERFORM SEND-PHONE.
           MOVE "S  A120070." TO VDU.
           MOVE "-" TO TEL-H1 TEL-H2.
           MOVE TEL-EDIT TO VDUBUF.
           PERFORM SCREENIO.

       F-071.
           MOVE "E  A120071." TO VDU.
           PERFORM SCREENIO.
           IF UP-KEY
              GO TO ENTER-ELE.
           IF NOT ENTER-KEY
              GO TO F-071.
           PERFORM ENTER-PHONE.
           IF TEL-BUFX = " "
              MOVE 0 TO TEL-BUF.
           IF TEL-BUFX IS NOT NUMERIC
              GO TO ENTER-ELE.
           MOVE TEL-BUF TO BR-FAX-NUMBER.
           PERFORM SEND-PHONE.
           MOVE "S  A120071." TO VDU.
           MOVE "-" TO TEL-H1 TEL-H2.
           MOVE TEL-EDIT TO VDUBUF.
           PERFORM SCREENIO.
           GO TO CREATE-ROUTINE.

       F-08.
           MOVE VDUNUM0 TO BR-PRINTER(1).

      *    IF BR-PRINTER(1) = 0
      *       MOVE SPACES TO BR-PRINTERFG(1)
      *       GO TO CREATE-ROUTINE.
       F-081.
           MOVE "E  A010081." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF UP-KEY
              GO TO ENTER-ELE.
           IF NOT ENTER-KEY
              GO TO F-081.
           MOVE VDUBUF TO BR-PRINTERFG(1).
           IF BR-PRINTERFG-VALID(1)
              GO TO CREATE-ROUTINE.
           PERFORM INVALID-PRINTERFG.
           GO TO F-081.
       F-09.
           MOVE VDUNUM0 TO BR-PRINTER(2).
      *    IF BR-PRINTER(2) = 0
      *       MOVE SPACES TO BR-PRINTERFG(2)
      *       GO TO CREATE-ROUTINE.
       F-091.
           MOVE "E  A010091." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF UP-KEY
              GO TO ENTER-ELE.
           IF NOT ENTER-KEY
              GO TO F-091.
           MOVE VDUBUF TO BR-PRINTERFG(2).
           IF BR-PRINTERFG-VALID(2)
              GO TO CREATE-ROUTINE.
           PERFORM INVALID-PRINTERFG.
           GO TO F-091.
       F-10.
           MOVE VDUNUM0 TO BR-PRINTER(3).
      *    IF BR-PRINTER(3) = 0
      *       MOVE SPACES TO BR-PRINTERFG(3)
      *       GO TO CREATE-ROUTINE.
       F-101.
           MOVE "E  A010101." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF UP-KEY
              GO TO ENTER-ELE.
           IF NOT ENTER-KEY
              GO TO F-101.
           MOVE VDUBUF TO BR-PRINTERFG(3).
           IF BR-PRINTERFG-VALID(3)
              GO TO CREATE-ROUTINE.
           PERFORM INVALID-PRINTERFG.
           GO TO F-101.
       F-11.
           MOVE VDUNUM0 TO BR-PRINTER(4).
      *    IF BR-PRINTER(4) = 0
      *       MOVE SPACES TO BR-PRINTERFG(4)
      *       GO TO CREATE-ROUTINE.
       F-111.
           MOVE "E  A010111." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF UP-KEY
              GO TO ENTER-ELE.
           IF NOT ENTER-KEY
              GO TO F-111.
           MOVE VDUBUF TO BR-PRINTERFG(4).
           IF BR-PRINTERFG-VALID(4)
              GO TO CREATE-ROUTINE.
           PERFORM INVALID-PRINTERFG.
           GO TO F-111.
       F-12.
           MOVE VDUNUM0 TO BR-NEXT-CHECK CK-NO.
           IF CK-NO NOT = 0
              MOVE BR-NO TO CK-BRNO
              PERFORM OPEN-CK1-FILE
              PERFORM READ-CK1-FILE
              PERFORM CLOSE-CK1-FILE
              IF IO-GOOD
                 MOVE "CHECK ALREADY EXISTS !" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-13.
           MOVE VDUNUM0 TO BR-FEE-GL-BRNO.
           GO TO CREATE-ROUTINE.
       F-14.
           MOVE VDUNUM2 TO BR-BANKBAL.
           GO TO CREATE-ROUTINE.
       F-15.
           PERFORM CHECK-GLNO.
           IF IO-BAD
              GO TO ENTER-ELE.
           MOVE GL-BR-ACCOUNT TO BR-GLBANK.
           GO TO CREATE-ROUTINE.
       F-16.
           PERFORM CHECK-GLNO.
           IF IO-BAD
              GO TO ENTER-ELE.
           MOVE GL-BR-ACCOUNT TO BR-GLBANK-DEPOSIT.
           GO TO CREATE-ROUTINE.
       F-17.
           PERFORM CHECK-GLNO.
           IF IO-BAD
              GO TO ENTER-ELE.
           MOVE GL-BR-ACCOUNT TO BR-GLONHAND.
           GO TO CREATE-ROUTINE.
       F-18.
           PERFORM CHECK-GLNO.
           IF IO-BAD
              GO TO ENTER-ELE.
           MOVE GL-BR-ACCOUNT TO BR-GLOVERSHORT.
           GO TO CREATE-ROUTINE.
       F-19.
           PERFORM CHECK-GLNO.
           IF IO-BAD
              GO TO ENTER-ELE.
           MOVE GL-BR-ACCOUNT TO BR-RETAIN.
           GO TO CREATE-ROUTINE.
       F-20.
           PERFORM CHECK-GLNO.
           IF IO-BAD
              GO TO ENTER-ELE.
           MOVE GL-BR-ACCOUNT TO BR-BANK-CLEARING-GL.
           GO TO CREATE-ROUTINE.
       F-21.
           MOVE VDUNUM0 TO BR-BANK-ROUTING-NO.
           PERFORM VERIFY-CHECK-DIGIT.
           IF CHK-DIG-FG = "N"
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-22.
           MOVE VDUBUF TO BR-BANK-ACCTNO.
           GO TO CREATE-ROUTINE.
       F-23.
           IF ( VDUBUF = SPACES )
              GO TO ENTER-ELE.
           MOVE VDUBUF TO BR-DATA-PATH.
           GO TO CREATE-ROUTINE.
       F-24.
           IF ( VDUBUF = SPACES )
              GO TO ENTER-ELE.
           MOVE VDUBUF TO BR-MACHINE.
           GO TO CREATE-ROUTINE.
       F-25.
           MOVE VDUBUF TO BR-MANAGER.
           GO TO CREATE-ROUTINE.
       F-26.
           MOVE VDUBUF TO BR-EOD-PROCEDURE.
           GO TO CREATE-ROUTINE.
       F-27.
           MOVE VDUNUM0 TO BR-REGION-NUMBER.
           GO TO CREATE-ROUTINE.
       F-28.
           PERFORM CHECK-GLNO.
           IF IO-BAD
              GO TO ENTER-ELE.
           MOVE GL-BR-ACCOUNT TO BR-CHECK-DEPOSIT-GL.
           GO TO CREATE-ROUTINE.
       F-29.
           MOVE VDUNUM0 TO BR-WKDAY-BANKID.
           GO TO CREATE-ROUTINE.
       END-ENTRY-MODULE.
           MOVE 0 TO FORM-SEL.
       EXIT-ENTRY-MODULE.
           EXIT.

      *************************************************
       ENTER-SCANKEY SECTION.
      *************************************************
           MOVE " ENTER BRANCH # OR F6 TO SCAN" TO MESS.
           PERFORM CALL-TMMSG.

           PERFORM OPEN-BR-FILE.
           MOVE 1 TO BR-NO.
           PERFORM START-BR-FILE.
       ENTER-KEY.
           MOVE "EN N040SCAN." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCAN.

           IF F6-KEY
              GO TO READ-FILES-NEXT.

           IF NOT ENTER-KEY
              GO TO ENTER-KEY.

           IF VDUNUM0 = 0
              MOVE BR-REC TO BROR-REC
              GO TO CLOSE-SCAN.

           MOVE VDUNUM0 TO BR-NO BR-NO.
           PERFORM START-BR-FILE.
       READ-FILES-NEXT.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-BAD
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCAN.

           MOVE "SN N040BRANCH." TO VDU.
           MOVE BR-NO TO VDUNUM0.
           PERFORM SCREENIO.

           PERFORM DISPLAY-FIELDS.

           GO TO ENTER-KEY.
       CLOSE-SCAN.
           MOVE SPACES TO MESS.
           PERFORM CALL-TMMSG.

           PERFORM CLOSE-BR-FILE.
       END-SCAN.
           EXIT.

      *************************************************
       SCAN-GL-FILE SECTION.
      *************************************************
           MOVE SPACES   TO GLSCN2-KEY.

           MOVE "WI/GLSCN2"               TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH GLSCN2-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.

           IF GLSCN2-KEY = SPACES
              MOVE "Y" TO GL-SCAN-ERR
              GO TO EXIT-SCAN-GL-FILE.

           MOVE GLSCN2-BRANCH  TO W-G-BR.
           MOVE GLSCN2-ACCOUNT TO W-G-NUM.
           MOVE "-" TO W-G-DASH.

           MOVE "S  A120  0." TO VDU.
           MOVE ELE TO VDUROW.
           MOVE W-G-GLNO TO VDUBUF.
           PERFORM SCREENIO.

           MOVE W-G-GLNO TO VDUBUF.
           MOVE "N" TO GL-SCAN-ERR.

       EXIT-SCAN-GL-FILE.
           EXIT.

      ******************************************************************
       VERIFY-PASSWORD SECTION.
      ******************************************************************
           MOVE " "                          TO PASSWDW-STAT.
           MOVE "BRMAN1"                     TO PASSWDW-PGM.
           MOVE "PLEASE ENTER THE PASSWORD," TO PASSWDW-PROMPT1.
           MOVE "CL/PASSWD"                  TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
           CANCEL FORM-PROGX.

       EXIT-VERIFY-PASSWORD.
           EXIT.

      ******************************************
       WRITE-MODULE SECTION.
      ******************************************
           PERFORM OPEN-BR-FILE.
           IF NEW-FLAG NOT = 0
              GO TO WRITE-RECORD.

           MOVE BR-REC TO BRCH-REC.

           IF BROR-REC = BRCH-REC
              GO TO END-WRITE-MODULE.

           PERFORM READ-BR-FILE.
           IF IO-BAD
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-WRITE-MODULE.
           IF BR-REC NOT = BROR-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-WRITE-MODULE.

           PERFORM LOG-MAINTENANCE-CHGS.

           MOVE BRCH-REC TO BR-REC.
           PERFORM REWRITE-BR-FILE.

           GO TO END-WRITE-MODULE.
       WRITE-RECORD.
           MOVE BR-NO TO BR-NO.
           PERFORM WRITE-BR-FILE.
           IF IO-BAD
              PERFORM ALREADY-CREATED
           ELSE
              MOVE "Y" TO NEW-REC.
       END-WRITE-MODULE.
           PERFORM CLOSE-BR-FILE.

      ******************************************************************
       LOG-MAINTENANCE-CHGS SECTION.
      ******************************************************************

           MOVE SPACES                     TO LMC-DETAIL.
           MOVE BR-NO                      TO LMC-BRNO.
           MOVE " TO "                     TO LMC-TO-X.

           IF BROR-NAME NOT = BRCH-NAME
             MOVE "01 "                   TO LMC-FIELD
             MOVE "BR NAME "              TO LMC-DESC
             MOVE BROR-NAME               TO LMC-ORG-TEXT
             MOVE BRCH-NAME               TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-ADR1 NOT = BRCH-ADR1
             MOVE "02 "                   TO LMC-FIELD
             MOVE "ADDR LN1"              TO LMC-DESC
             MOVE BROR-ADR1               TO LMC-ORG-TEXT
             MOVE BRCH-ADR1               TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-ADR2 NOT = BRCH-ADR2
             MOVE "03 "                   TO LMC-FIELD
             MOVE "ADDR LN2"              TO LMC-DESC
             MOVE BROR-ADR2               TO LMC-ORG-TEXT
             MOVE BRCH-ADR2               TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-CITY NOT = BRCH-CITY
             MOVE "04 "                   TO LMC-FIELD
             MOVE "BR CITY "              TO LMC-DESC
             MOVE BROR-CITY               TO LMC-ORG-TEXT
             MOVE BRCH-CITY               TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-STATE NOT = BRCH-STATE
             MOVE "05 "                   TO LMC-FIELD
             MOVE "BR STATE"              TO LMC-DESC
             MOVE BROR-STATE              TO LMC-ORG-TEXT
             MOVE BRCH-STATE              TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-ZIP NOT = BRCH-ZIP
             MOVE "06 "                   TO LMC-FIELD
             MOVE "BR ZIP  "              TO LMC-DESC
             MOVE BROR-ZIP                TO LMC-ORG-TEXT
             MOVE BRCH-ZIP                TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-PHONE NOT = BRCH-PHONE
             MOVE "07 "                   TO LMC-FIELD
             MOVE "BR PHONE"              TO LMC-DESC
             MOVE BROR-PHONE              TO LMC-ORG-PHONE
             INSPECT LMC-ORG-PHONE REPLACING ALL "/" BY "-"
             MOVE BRCH-PHONE              TO LMC-CHG-PHONE
             INSPECT LMC-CHG-PHONE REPLACING ALL "/" BY "-"
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-FAX-NUMBER NOT = BRCH-FAX-NUMBER
             MOVE "071"                   TO LMC-FIELD
             MOVE "FAX NUM "              TO LMC-DESC
             MOVE BROR-FAX-NUMBER         TO LMC-ORG-PHONE
             INSPECT LMC-ORG-PHONE REPLACING ALL "/" BY "-"
             MOVE BRCH-FAX-NUMBER         TO LMC-CHG-PHONE
             INSPECT LMC-CHG-PHONE REPLACING ALL "/" BY "-"
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-PRINTER(1) NOT = BRCH-PRINTER(1)
             MOVE "08 "                   TO LMC-FIELD
             MOVE "PRINT 1 "              TO LMC-DESC
             MOVE BROR-PRINTER(1)         TO LMC-ORG-NUM
             MOVE BRCH-PRINTER(1)         TO LMC-CHG-NUM
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-PRINTERFG(1) NOT = BRCH-PRINTERFG(1)
             MOVE "081"                   TO LMC-FIELD
             MOVE "PR FLAG1"              TO LMC-DESC
             MOVE BROR-PRINTERFG(1)       TO LMC-ORG-TEXT
             MOVE BRCH-PRINTERFG(1)       TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-PRINTER(2) NOT = BRCH-PRINTER(2)
             MOVE "09 "                   TO LMC-FIELD
             MOVE "PRINT 2 "              TO LMC-DESC
             MOVE BROR-PRINTER(2)         TO LMC-ORG-NUM
             MOVE BRCH-PRINTER(2)         TO LMC-CHG-NUM
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-PRINTERFG(2) NOT = BRCH-PRINTERFG(2)
             MOVE "091"                   TO LMC-FIELD
             MOVE "PR FLAG2"              TO LMC-DESC
             MOVE BROR-PRINTERFG(2)       TO LMC-ORG-TEXT
             MOVE BRCH-PRINTERFG(2)       TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-PRINTER(3) NOT = BRCH-PRINTER(3)
             MOVE "10 "                   TO LMC-FIELD
             MOVE "PRINT 3 "              TO LMC-DESC
             MOVE BROR-PRINTER(3)         TO LMC-ORG-NUM
             MOVE BRCH-PRINTER(3)         TO LMC-CHG-NUM
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-PRINTERFG(3) NOT = BRCH-PRINTERFG(3)
             MOVE "101"                   TO LMC-FIELD
             MOVE "PR FLAG3"              TO LMC-DESC
             MOVE BROR-PRINTERFG(3)       TO LMC-ORG-TEXT
             MOVE BRCH-PRINTERFG(3)       TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-PRINTER(4) NOT = BRCH-PRINTER(4)
             MOVE "11 "                   TO LMC-FIELD
             MOVE "PRINT 4 "              TO LMC-DESC
             MOVE BROR-PRINTER(4)         TO LMC-ORG-NUM
             MOVE BRCH-PRINTER(4)         TO LMC-CHG-NUM
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-PRINTERFG(4) NOT = BRCH-PRINTERFG(4)
             MOVE "111"                   TO LMC-FIELD
             MOVE "PR FLAG4"              TO LMC-DESC
             MOVE BROR-PRINTERFG(4)       TO LMC-ORG-TEXT
             MOVE BRCH-PRINTERFG(4)       TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-NEXT-CHECK NOT = BRCH-NEXT-CHECK
             MOVE "12 "                   TO LMC-FIELD
             MOVE "NXT CHECK"              TO LMC-DESC
             MOVE BROR-NEXT-CHECK         TO LMC-ORG-AMT
             MOVE BRCH-NEXT-CHECK         TO LMC-CHG-AMT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-FEE-GL-BRNO NOT = BRCH-FEE-GL-BRNO
             MOVE "13 "                   TO LMC-FIELD
             MOVE "FEE BRNO "              TO LMC-DESC
             MOVE BROR-FEE-GL-BRNO        TO LMC-ORG-NUM
             MOVE BRCH-FEE-GL-BRNO        TO LMC-CHG-NUM
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-BANKBAL NOT = BRCH-BANKBAL
             MOVE "14 "                   TO LMC-FIELD
             MOVE "BANK BAL"              TO LMC-DESC
             MOVE BROR-BANKBAL            TO LMC-ORG-AMT
             MOVE BRCH-BANKBAL            TO LMC-CHG-AMT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-GLBANK NOT = BRCH-GLBANK
             MOVE "15 "                   TO LMC-FIELD
             MOVE "GL BANK "              TO LMC-DESC
             MOVE BROR-GLBANK             TO LMC-ORG-GLNO
             MOVE BRCH-GLBANK             TO LMC-CHG-GLNO
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-GLBANK-DEPOSIT NOT = BRCH-GLBANK-DEPOSIT
             MOVE "16 "                   TO LMC-FIELD
             MOVE "BANK DEP"              TO LMC-DESC
             MOVE BROR-GLBANK-DEPOSIT     TO LMC-ORG-GLNO
             MOVE BRCH-GLBANK-DEPOSIT     TO LMC-CHG-GLNO
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-GLONHAND NOT = BRCH-GLONHAND
             MOVE "17 "                   TO LMC-FIELD
             MOVE "ON HAND "              TO LMC-DESC
             MOVE BROR-GLONHAND           TO LMC-ORG-GLNO
             MOVE BRCH-GLONHAND           TO LMC-CHG-GLNO
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-GLOVERSHORT NOT = BRCH-GLOVERSHORT
             MOVE "18 "                   TO LMC-FIELD
             MOVE "OV SHORT"              TO LMC-DESC
             MOVE BROR-GLOVERSHORT        TO LMC-ORG-GLNO
             MOVE BRCH-GLOVERSHORT        TO LMC-CHG-GLNO
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-RETAIN NOT = BRCH-RETAIN
             MOVE "19 "                   TO LMC-FIELD
             MOVE "RETAINED"              TO LMC-DESC
             MOVE BROR-RETAIN             TO LMC-ORG-GLNO
             MOVE BRCH-RETAIN             TO LMC-CHG-GLNO
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-BANK-CLEARING-GL NOT = BRCH-BANK-CLEARING-GL
             MOVE "20 "                   TO LMC-FIELD
             MOVE "BK CLEAR"              TO LMC-DESC
             MOVE BROR-BANK-CLEARING-GL   TO LMC-ORG-GLNO
             MOVE BRCH-BANK-CLEARING-GL   TO LMC-CHG-GLNO
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-BANK-ROUTING-NO NOT = BRCH-BANK-ROUTING-NO
             MOVE "21 "                   TO LMC-FIELD
             MOVE "ROUTING"              TO LMC-DESC
             MOVE BROR-BANK-ROUTING-NO    TO LMC-ORG-NUM
             MOVE BRCH-BANK-ROUTING-NO    TO LMC-CHG-NUM
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-BANK-ACCTNO NOT = BRCH-BANK-ACCTNO
             MOVE "22 "                   TO LMC-FIELD
             MOVE "BNK ACCT"              TO LMC-DESC
             MOVE BROR-BANK-ACCTNO        TO LMC-ORG-NUM
             MOVE BRCH-BANK-ACCTNO        TO LMC-CHG-NUM
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-DATA-PATH NOT = BRCH-DATA-PATH
             MOVE "23 "                   TO LMC-FIELD
             MOVE "DATA PTH"              TO LMC-DESC
             MOVE BROR-DATA-PATH          TO LMC-ORG-TEXT
             MOVE BRCH-DATA-PATH          TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-MACHINE NOT = BRCH-MACHINE
             MOVE "24 "                   TO LMC-FIELD
             MOVE "MACHINE "              TO LMC-DESC
             MOVE BROR-MACHINE            TO LMC-ORG-TEXT
             MOVE BRCH-MACHINE            TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-MANAGER NOT = BRCH-MANAGER
             MOVE "25 "                   TO LMC-FIELD
             MOVE "MANAGER "              TO LMC-DESC
             MOVE BROR-MANAGER            TO LMC-ORG-TEXT
             MOVE BRCH-MANAGER            TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-EOD-PROCEDURE NOT = BRCH-EOD-PROCEDURE
             MOVE "26 "                   TO LMC-FIELD
             MOVE "EOD BATCH"              TO LMC-DESC
             MOVE BROR-EOD-PROCEDURE      TO LMC-ORG-TEXT
             MOVE BRCH-EOD-PROCEDURE      TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-REGION-NUMBER NOT = BRCH-REGION-NUMBER
             MOVE "27 "                   TO LMC-FIELD
             MOVE "REG NUM "              TO LMC-DESC
             MOVE BROR-REGION-NUMBER      TO LMC-ORG-NUM
             MOVE BRCH-REGION-NUMBER      TO LMC-CHG-NUM
             PERFORM LOG-FIELD-CHANGE.

           IF BROR-CHECK-DEPOSIT-GL NOT = BRCH-CHECK-DEPOSIT-GL
             MOVE "28 "                   TO LMC-FIELD
             MOVE "CK DEP GL"             TO LMC-DESC
             MOVE BROR-CHECK-DEPOSIT-GL   TO LMC-ORG-GLNO
             MOVE BRCH-CHECK-DEPOSIT-GL   TO LMC-CHG-GLNO
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

      *********************************************
       DELETE-MODULE SECTION.
      *********************************************
           IF NEW-FLAG NOT = 0
              GO TO END-DELETE-MODULE.

           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           IF IO-BAD
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-DELETE-MODULE.

           IF BR-REC NOT = BROR-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-CHANGED
              GO TO END-DELETE-MODULE.

           PERFORM DELETE-BR-FILE.

           MOVE "C" TO ERRLOGW-ACTION.
           MOVE " " TO ERRLOGW-KEY.
           STRING BR-NO, " ", "DELETED" DELIMITED BY SIZE
               INTO ERRLOGW-KEY.
           MOVE FORM-PATHNAME TO ERRLOGW-PROGX.
           MOVE "CL/ERRLOG" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PROGX.

       END-DELETE-MODULE.
           PERFORM CLOSE-BR-FILE.

      ********************************************
       NEW-RECORD-SETUP SECTION.
      ********************************************
           MOVE 1 TO NEW-FLAG.
           MOVE "S  A000NEW." TO VDU.
           MOVE "*** NEW RECORD ***" TO VDUBUF.
           PERFORM SCREENIO.

           PERFORM CLEAR-BR-FILE.

           MOVE HOLD-KEY TO BR-NO.
           MOVE 0 TO ELE.

      ********************************************
       DISPLAY-FIELDS SECTION.
      ********************************************
           MOVE SPACES TO DISPLAY-BUF.
           MOVE BR-NAME TO F01.
           MOVE BR-ADR1 TO F02.
           MOVE BR-ADR2 TO F03.
           MOVE BR-CITY TO F04.
           MOVE BR-STATE TO F05.
           MOVE BR-ZIP TO F06.
           MOVE BR-PHONE TO TEL-BUF.
           PERFORM SEND-PHONE.
           MOVE TEL-EDIT TO F07.
           MOVE BR-FAX-NUMBER TO TEL-BUF.
           PERFORM SEND-PHONE.
           MOVE TEL-EDIT TO F071.
           MOVE TEL-EDIT TO F08.
           MOVE BR-PRINTER(1) TO F08.
           MOVE BR-PRINTERFG(1) TO F081.
           MOVE BR-PRINTER(2) TO F09.
           MOVE BR-PRINTERFG(2) TO F091.
           MOVE BR-PRINTER(3) TO F10.
           MOVE BR-PRINTERFG(3) TO F101.
           MOVE BR-PRINTER(4) TO F11.
           MOVE BR-PRINTERFG(4) TO F111.

           MOVE BR-NEXT-CHECK TO F12.
           IF BR-FEE-GL-BRNO NOT NUMERIC
              MOVE 0 TO BR-FEE-GL-BRNO.
           MOVE BR-FEE-GL-BRNO TO F13.

      * DISPLAY CAN LOAN GROUP ID AND DESC FOR REFERENCE ONLY, NO EDIT.
           MOVE BR-CAN-GROUP-ID TO D-CAN-GROUP-ID.
           IF BR-CAN-GROUP-ID = SPACES
              MOVE SPACES TO D-CAN-DESC
           ELSE
              PERFORM GET-CD
              MOVE CD-DESC TO D-CAN-DESC.

           MOVE BR-BANKBAL TO F14.
           MOVE BR-GLBANK TO WORK-GL.
           MOVE WORK-BR TO DISP-BR.
           MOVE WORK-NO TO DISP-NO.
           MOVE DISP-GL TO F15.
           MOVE BR-GLBANK-DEPOSIT TO WORK-GL.
           MOVE WORK-BR TO DISP-BR.
           MOVE WORK-NO TO DISP-NO.
           MOVE DISP-GL TO F16.
           MOVE BR-GLONHAND TO WORK-GL.
           MOVE WORK-BR TO DISP-BR.
           MOVE WORK-NO TO DISP-NO.
           MOVE DISP-GL TO F17.
           MOVE BR-GLOVERSHORT TO WORK-GL.
           MOVE WORK-BR TO DISP-BR.
           MOVE WORK-NO TO DISP-NO.
           MOVE DISP-GL TO F18.
           MOVE BR-RETAIN TO WORK-GL.
           MOVE WORK-BR TO DISP-BR.
           MOVE WORK-NO TO DISP-NO.
           MOVE DISP-GL TO F19.
           MOVE BR-BANK-CLEARING-GL TO WORK-GL.
           MOVE WORK-BR TO DISP-BR.
           MOVE WORK-NO TO DISP-NO.
           MOVE DISP-GL TO F20.
           MOVE BR-BANK-ROUTING-NO TO F21.
           MOVE BR-BANK-ACCTNO TO F22.
           MOVE BR-DATA-PATH TO F23.
           MOVE BR-MACHINE TO F24.
           MOVE BR-MANAGER TO F25.
           MOVE BR-EOD-PROCEDURE TO F26.
           MOVE BR-REGION-NUMBER TO F27.
           MOVE BR-CHECK-DEPOSIT-GL TO WORK-GL.
           MOVE WORK-BR TO DISP-BR.
           MOVE WORK-NO TO DISP-NO.
           MOVE DISP-GL TO F28.
           MOVE BR-WKDAY-BANKID TO F29.
           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       DISPLAY-FIELDS-EXIT.
           EXIT.

      ********************************************
       CHECK-GLNO SECTION.
      ********************************************
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

      *------------------------------------------------------------
      * CHECK DIGIT ROUTINE IS A STANDARD BANK ROUTING NUMBER ROUTINE
      * THE 9TH DIGIT MUST BE A VALID CHECK DIGIT BASED UPON THE FOLLOWING;
      * THE CHECK DIGIT IS BASED ON THE FIRST EIGHT DIGITS.                           * THE 1ST, 4TH AND 7TH ARE EACH MULTIPLIED BY 3;
      * THE 2ND, 5TH AND 8TH EACH BY 7; AND THE 3RD AND 6TH BY 1.
      * THE EIGHT PRODUCTS ARE ADDED. TO GET THE CHECK DIGIT, SUBTRACT THE
      * SUM OF THE PRODUCTS FROM THE NEXT MULTIPLE OF 10.
      *------------------------------------------------------------
      ********************************************
       VERIFY-CHECK-DIGIT SECTION.
      ********************************************
       VERIFY-CHK-DIGIT.
           MOVE BR-BANK-ROUTING-NO TO TMP-BANK-ROUTING-NO.
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
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/GBBRCL.CPY".
       COPY "LIBGB/DATER.CPY".

      *****************************************************
       FORM-RESET SECTION.
      *****************************************************
           MOVE EXT-CONAME-CONAME TO BRMAN1-CONAME.
           MOVE EXT-CONAME-SYSDATE TO BRMAN1-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY BRMAN1-SCREEN.
           DISPLAY BRMAN1-SCREEN UPON BRMAIN-WINDOW-HANDLE.
           MOVE BRMAN1-FORM-FIELDS TO WR-BUF.
           MOVE BRMAN1-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *****************************************************
       SETUP-LINK-INFO SECTION.
      *****************************************************
           MOVE BRMAN1-HELP--FILE TO HELP-LINK-FILE.
           MOVE BRMAN1-HELP--DIR TO HELP-LINK-DIR.

      *****************************************************
       VDU-CONVERT-FIELD SECTION.
      *****************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBGB/BRMAN1_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *****************************************************
       MISC-PARAGRAPHS SECTION.
      *****************************************************
       COPY "LIBGB/PHONEIO.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
       GET-CD.
           MOVE "CL"            TO CD-TYPE.
           MOVE BR-CAN-GROUP-ID TO CD-CODE.
           PERFORM OPEN-CD1-FILE.
           PERFORM READ-CD1-FILE.
           PERFORM CLOSE-CD1-FILE.
           IF IO-BAD
              MOVE "NOT FOUND" TO CD-DESC.
       INVALID-PRINTER.
           MOVE "INVALID PRINTER #!" TO MESS.
           PERFORM SEND-MESS.
       INVALID-PRINTERFG.
           MOVE "INVALID PRINTER FLAG!" TO MESS.
           PERFORM SEND-MESS.
           MOVE "PLEASE PRESS F8 FOR HELP" TO MESS.
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

      *************************************
       IO-ROUTINES SECTION.
      *************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-ST1-FILE.
           PERFORM CLOSE-CK1-FILE.
           PERFORM CLOSE-CD1-FILE.
           PERFORM CLOSE-GL1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPSTGS_SQL.CPY".
       COPY "LIBLP/LPCKGS_SQL.CPY".
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBGL/GLGLGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBLP/LPST1RN.CPY".
       COPY "LIBGL/GLGL1RN.CPY".
       COPY "LIBLP/LPCK1RN.CPY".
       COPY "LIBLP/LPCD1RN.CPY".
       COPY "LIBGB/GBBRIN.CPY". 

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      *************************************
       END-ROUTINE SECTION.
      *************************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY BRMAN1-SCREEN.
           MOVE FORM-PATH TO FORM-PATHNAME.
           MOVE BRMAIN-LINK TO FORM-LINK-AREA.
           EXIT PROGRAM.
