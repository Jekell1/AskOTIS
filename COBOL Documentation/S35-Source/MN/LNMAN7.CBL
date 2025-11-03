       IDENTIFICATION DIVISION.
       PROGRAM-ID.      LNMAN7.
      ******************************************************************
      *
      *   DESC: LOAN MASTER FILE MAINTENANCE   PAGE 7
      *
      *=================================================================
      * REV:
      * MJD 050895 ADDED CODE TO WRITE CHANGE FILE AFTER REWRITE
      * GLF 082296 ADDED LNSTAT FIELD TO BOTTOM OF PAGE.
      * JTG 111496 CHANGED TO SKIP O6-REBATE & O7-REBATE
      * KEC 111496 CHANGED LN-POCD FOR RESCISSION.
      * MJD 082197 CHANGED RECORD SIZE FROM 1500 TO 1570 ADD LOGIC FOR PAGE 10
      * GLF 090997 REPLACED 'IO' ON GP-FILE WITH GET 'GPENV'
      * BAH 091297 CHANGED SPREFUNDATE TO TRW-1ST-30
      * MJD 112197 EXPANDED LN-REC TO 1800 BYTES
      * MJD 072498 ALLOW ENTRY OF ZERO DATE FOR LN-TRW-1ST-30.
      * KEC 090398 DISPLAYING LN-REVOLVING FOR 'LNSTAT'.
      * KEC 091598 ADDED LOGIC TO ACCESS NEW PAGE 11.
      * JTG 093098 ALLOWED LN-CURBAL & LN-PURBAL TO GO NEGATIVE; REGENCY REVOL
      * JTG 120198 ADDED LOGIC TO ACCESS NEW PAGE 12.
      * BAH 990818 FIELD 10, "DISCOUNT" DISAPPEARED WHEN RESET PAGE, PL# 237
      * JTG 020905 ADDED LN-DLCOMM-O3-CMSI LN-DLCOMM-PP-CMSI  REGACC #1846
      * BAH 050601 CHANGED LN-TRW-1ST-30 TO LN-TRW-DOFD-DATE, DATE OF FIRST
      *            DELINQUENCY
      * AMM 051018 ADDED DETAILED, FIELD-BY-FIELD MAINTENANCE LOG IN
      *            /USR/DATA/R1/LG/CHG.YYMMDD
      * MJD 121025 A30 CONVERSION COMPLETE.
      * MJD 121217 ADDED LNMAIN-WINDOW-HANDLE TO SCREEN DISPLAY
      * MJD 130117 CHANGED CALL TMMSG TO PERFORM CALL-TMMSG
      *  CS 160616 REMOVED LN-SEQNO
      *  CS 160711 SETUP LN-OWNBR WILL BE IN LN1-KEY
      *  CS 170425 ACTIVATE 8 DIGIT DATES
      *
      * KEC 2017.0526 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          REMOVED ORIGINAL-REC AND CHANGED-REC WHICH NOW USING
      *                  LNOR-REC AND LNCH-REC.
      * BAH 181212 GLOBAL LNFILE
      * BAH 20190627 REMOVED LN-REVOLVING, PT0153
      * BAH 2024.0131 CALL CRNOR2 AND WRITE NOTICE RECORD
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)MN/LNMAN7.CBL S35 02/21/24-13:01:16 >".
      *    **********************************
      *    *      PROGRAM WORKERS
      *    **********************************
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01DL.CPY".
       COPY "LIBLP/LP01DL_SQL.CPY".
       COPY "LIBLP/LPWSDL.CPY".
       COPY "LIBLP/LP01NO.CPY".
       COPY "LIBLP/LP01NO_SQL.CPY".
       COPY "LIBLP/LPWSNO.CPY".

      *=================================================================
      *   LNCH-REC FROM 'REPLACING LEADING' ON LN-REC. [CHANGED-REC]
      *   LNOR-REC FROM 'REPLACING LEADING' ON LN-REC. [ORIGINAL-REC]
      *=================================================================
       COPY "LIBLP/LP01LN.CPY" REPLACING LEADING "LN" BY "LNCH".
       COPY "LIBLP/LP01LN.CPY" REPLACING LEADING "LN" BY "LNOR".

      *=================================================================
       01  WORK-ERRLOG-DETAIL.
           03  WORK-ERRLOG-BR           PIC 9(4).
           03  FILLER                   PIC X.
           03  WORK-ERRLOG-NUMBER       PIC Z(7).
           03  FILLER                   PIC X.
           03  WORK-ERRLOG-FIELD        PIC X(3).
           03  FILLER                   PIC X.
           03  WORK-ERRLOG-DESC         PIC X(8).
           03  FILLER                   PIC X.
           03  WORK-ERRLOG-LONG-ALP.
               05  WORK-ERRLOG-ORG-ALP  PIC X(13).
               05  WORK-ERRLOG-ORG-N0 REDEFINES
                   WORK-ERRLOG-ORG-ALP  PIC Z(12)-.
               05  WORK-ERRLOG-ORG-N2 REDEFINES
                   WORK-ERRLOG-ORG-ALP  PIC Z(9).99-.

               05  WORK-ERRLOG-TO-X     PIC X(3).

               05  WORK-ERRLOG-CHG-ALP  PIC X(13).
               05  WORK-ERRLOG-CHG-N0 REDEFINES
                   WORK-ERRLOG-CHG-ALP  PIC Z(12)-.
               05  WORK-ERRLOG-CHG-N2 REDEFINES
                   WORK-ERRLOG-CHG-ALP  PIC Z(9).99-.

       01  ONE               PIC S9.
       01  ELE               PIC 99     VALUE 0.
       01  ACTION            VALUE SPACES.
           03  CHKEY         PIC X(4).
           03  CHNO          PIC 9(2).
           03  CHNO-X REDEFINES CHNO
                             PIC X(2).
       01  FORM-NUM1     REDEFINES ACTION.
           03  FORM-KEY1     PIC X(4).
           03  FORM-P1       PIC X.
           03  FORM-SEL1     PIC 9.
       01  FORM-NUM2      REDEFINES ACTION.
           03  FORM-KEY2     PIC X(3).
           03  FORM-P2       PIC X.
           03  FORM-SEL2     PIC 99.
               88  FORM-2-DIGITS  VALUE 10 11.

       01  IO-TYPE           PIC X(3)   VALUE SPACES.
       01  NEW-FLAG          PIC 9      VALUE 0.
       01  FIRST-TIME        PIC 9      VALUE 0.
       01  FIRST-SCAN        PIC 9      VALUE 0.

      * 'PAGE-CHANGE-FLAG' IS FOR KNOWING IF COMING FROM ANOTHER PAGE
      
       01  PAGE-CHANGE-FLAG      PIC X(1)    VALUE "N".
           88  PAGE-CHANGE-NO                VALUE "N".
           88  PAGE-CHANGE-YES               VALUE "Y".

       01  DLR-TYP-LIST      PIC X(6)   VALUE "10P:S.".
       01  DLR-TYP-BUF       PIC X(10).

       01  CHAN          PIC X(4).
       01  LNMAIN-LINK.
           03  FORM-NAME     PIC X(20).
           03  SAVEKEY       PIC X(8).
           03  NEW-REC       PIC X.
           03  ERRCD         PIC X.
           03  FORMSEL       PIC 99.
           03  LNMAIN-WINDOW-HANDLE  PIC X(10).
       01  WS-ORGBR      PIC 9999.

      *    **********************************
      *    *      SCREEN DISPLAY BUFFER
      *    **********************************
       01  DISPLAY-BUF    VALUE SPACES.
           03  F01        PIC ZZZZ.ZZ.
           03  F02        PIC Z(6).ZZ.
           03  F03        PIC Z(6).ZZ.
           03  F04        PIC Z(6).ZZ.
           03  F05        PIC Z(6).ZZ.
           03  F06        PIC Z(6).ZZ.
           03  F07        PIC Z(6).ZZ.
           03  F08        PIC Z(6).ZZ.
           03  F081       PIC Z(6).ZZ.
           03  F09        PIC Z(6).ZZ.
           03  F091       PIC Z(6).ZZ.
           03  F10        PIC Z(6).ZZ.
           03  F11        PIC X.
           03  F12        PIC Z(6).ZZ.
           03  F13        PIC Z(5).
           03  F14        PIC Z(6).ZZ.
           03  F15        PIC Z(6).ZZ.
           03  F16        PIC ZZZ.
           03  F17        PIC Z(6).ZZ-.
           03  F18        PIC 99/99/99 BLANK ZERO.
      * CL REBATE
           03  F19        PIC X.
           03  F20        PIC X.
           03  F21        PIC X.
      * O1 REBATE
           03  F22        PIC X.
           03  F23        PIC X.
           03  F24        PIC X.
           03  F25        PIC X.
      * O5 REBATE
           03  F26        PIC X.
      * N/A
           03  F27        PIC X.
      * N/A
           03  F28        PIC X.
      * INT REBATE
           03  F29        PIC X.
           03  F30        PIC X.
           03  F31        PIC X.
           03  F32        PIC X.
           03  FLNSTAT    PIC X(30).

       01  DISPLAY-LIST.
           03  FILLER     PIC X(48) VALUE
               "010 020 030 040 050 060 070 080 081 090 091 100 ".
           03  FILLER     PIC X(20) VALUE
               "110 120 130 140 150 ".
           03  FILLER     PIC X(28) VALUE
               "160 170 180 190 200 210 220 ".
           03  FILLER     PIC X(47) VALUE
               "230 240 250 260 270 280 290 300 310 320 LNSTAT.".

      *    **********************************
      *    *   ELEMENT SPECIFICATIONS       *
      *    **********************************
       01  MAX            PIC 99   VALUE 32.
       01  SPEC-TABLE.
      * 01 DEALER NO
           03  FILLER     PIC X(8) VALUE "ENNN042N".
      * 02 RESERVE PRINCIPAL #1
           03  FILLER     PIC X(8) VALUE "ENNN062N".
           03  FILLER     PIC X(8) VALUE "ENNN062N".
           03  FILLER     PIC X(8) VALUE "ENNN062N".
      * 05 RESERVE CHARGES #1
           03  FILLER     PIC X(8) VALUE "ENNN062N".
           03  FILLER     PIC X(8) VALUE "ENNN062N".
           03  FILLER     PIC X(8) VALUE "ENNN062N".
      * 08 PARTICIPATING RESERVE HELD
           03  FILLER     PIC X(8) VALUE "ENNN062N".
      * 09 PARTICIPATING RESERVE PAID
           03  FILLER     PIC X(8) VALUE "ENNN062N".
      * 10 DEALER DISCOUNT
           03  FILLER     PIC X(8) VALUE "ENNN062N".
      * 11 RECOURSE CODE
           03  FILLER     PIC X(8) VALUE "E  A010N".
      * 12 PATRTIAL RECOURSE AMOUNT
           03  FILLER     PIC X(8) VALUE "ENNN062N".
      * 13 DEALER POOL
           03  FILLER     PIC X(8) VALUE "ENNN050N".
      * 14 DEALER CHECK
           03  FILLER     PIC X(8) VALUE "ENNN062N".
      * 15 ORIGINAL PURCHASE AMOUNT
           03  FILLER     PIC X(8) VALUE "ENNN062N".
      * 16 ORIGINAL PURCHASE TERM
           03  FILLER     PIC X(8) VALUE "ENNN030N".
      * 17 PURCHASE BALANCE
           03  FILLER     PIC X(8) VALUE "ENNS062N".
      * 18. SP REFUND DATE
           03  FILLER     PIC X(8) VALUE "ERNM060N".
      * 19. CL REBATE
           03  FILLER     PIC X(8) VALUE "E  A010N".
      * 20. AH REBATE
           03  FILLER     PIC X(8) VALUE "E  A010N".
      * 21. PP REBATE
           03  FILLER     PIC X(8) VALUE "E  A010N".
      * 22. O1 REBATE
           03  FILLER     PIC X(8) VALUE "E  A010N".
      * 23. O2 REBATE
           03  FILLER     PIC X(8) VALUE "E  A010N".
      * 24. O3 REBATE
           03  FILLER     PIC X(8) VALUE "E  A010N".
      * 25. O4 REBATE
           03  FILLER     PIC X(8) VALUE "E  A010N".
      * 26. O5 REBATE
           03  FILLER     PIC X(8) VALUE "E  A010N".
      * 27. N/A
           03  FILLER     PIC X(8) VALUE "E  A010N".
      * 28. N/A
           03  FILLER     PIC X(8) VALUE "E  A010N".
      * 29. INT REBATE
           03  FILLER     PIC X(8) VALUE "E  A010N".
      * 30. SC REBATE
           03  FILLER     PIC X(8) VALUE "E  A010N".
      * 31. MF REBATE
           03  FILLER     PIC X(8) VALUE "E  A010N".
      * 32. DF REBATE
           03  FILLER     PIC X(8) VALUE "E  A010N".

       01  SPEC-TAB      REDEFINES SPEC-TABLE.
           03  SPEC-REC   OCCURS 32 TIMES.
               05  SPEC   PIC X(7).
               05  CHFG   PIC X.

      ***********************************************
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBCL/CRNOR2W.CPY".
       COPY "LIBLP/LPPOFFW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/LNMAN7_DEF.CPY".
       COPY "LIBMN/LNMAN7_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/ACCESSW.CPY".

       COPY "LIBGB/GETFMW.CPY".
       01 FORM-LINK-AREA    PIC X(2000).
       SCREEN SECTION.
       COPY "LIBMN/LNMAN7_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME FORM-LINK-AREA EXIT-PATHNAME.

      ***************************
       MAIN-PROGRAM SECTION.
      ***************************
           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "LNMAN7" TO VDU-FORM-NAME.
           MOVE FORM-LINK-AREA TO LNMAIN-LINK.

           PERFORM GET-GPENV.

       MAIN-MODULE.
           PERFORM FORM-RESET.
           MOVE " " TO VDU-PGMENU-SELECTION.
           IF FIRST-TIME = 0
              MOVE 1 TO FIRST-TIME.

           IF GP-DLR-DISP-TYPE = "C"
              MOVE "COMMISSION" TO DLR-TYP-BUF
           ELSE
              MOVE "DISCOUNT  " TO DLR-TYP-BUF.

           MOVE DLR-TYP-BUF TO WR-BUF.
           MOVE DLR-TYP-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           PERFORM REC-LN-MODULE.
           IF IO-TYPE = "END"
              MOVE "X" TO ERRCD
              GO TO END-ROUTINE.
           IF IO-TYPE = "CLR"
              GO TO MAIN-MODULE.
           PERFORM ENTRY-MODULE.
           IF IO-TYPE = "RWR"
              PERFORM WRITE-MODULE.
           IF NEW-REC = "Y"
              MOVE "P" TO FORM-P1
              MOVE 8 TO FORM-SEL1.
           IF FORM-SEL1 NOT = 0
              IF FORM-P1 = "P"
                 MOVE LN-ACCTNO TO SAVEKEY
                 MOVE FORM-SEL1 TO FORMSEL
                 GO TO END-ROUTINE.
           IF FORM-SEL2 NOT = 0
              IF FORM-P2 = "P"
                 MOVE LN-ACCTNO TO SAVEKEY
                 MOVE FORM-SEL2 TO FORMSEL
                 GO TO END-ROUTINE.
           GO TO MAIN-MODULE.

      ***************************
       REC-LN-MODULE SECTION.
      ***************************
       REC-LN-MODULE-NO.
           MOVE 0 TO LN-ACCTNO LN-OWNBR FIRST-SCAN.
           MOVE SPACES TO IO-TYPE.
           IF SAVEKEY NOT = " "
              MOVE "Y" TO PAGE-CHANGE-FLAG
              MOVE SAVEKEY TO LN-ACCTNO
              MOVE " " TO SAVEKEY
              PERFORM DISPLAY-SCAN-KEY
              GO TO READ-LNFILE.

           MOVE "N" TO PAGE-CHANGE-FLAG.
           MOVE 0 TO NEW-FLAG.

       REC-LN-ACCTNO.
           MOVE "EN N060LNNO." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-REC-LN-MODULE.
           IF VDUSTATUS = "1<"
              PERFORM ENTER-SCANKEY
              GO TO END-REC-LN-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO REC-LN-ACCTNO.
           MOVE VDUNUM0 TO LN-ACCTNO.
           IF LN-ACCTNO = 0
              GO TO REC-LN-ACCTNO.
       READ-LNFILE.
           PERFORM OPEN-LN1-FILE.
           PERFORM READ-LN1-FILE.
           PERFORM CLOSE-LN1-FILE.
           IF IO-FG NOT = 0
              MOVE "* NOT ON FILE *" TO MESS
              PERFORM SEND-MESS
              GO TO REC-LN-MODULE-NO.

           IF ( LN-POCD-VOID )
              MOVE "ACCOUNT HAS BEEN DELETED" TO MESS
              PERFORM SEND-MESS
              GO TO REC-LN-MODULE-NO
           ELSE
           IF ( LN-POCD-RESCIND )
              MOVE "ACCOUNT HAS BEEN RESCINDED" TO MESS
              PERFORM SEND-MESS
              GO TO REC-LN-MODULE-NO
           ELSE
              MOVE LN-REC TO LNOR-REC
              PERFORM DISPLAY-FIELDS
              MOVE 0 TO NEW-FLAG
              IF NEW-REC = "Y"
                 MOVE 1 TO NEW-FLAG
              END-IF
              IF ( LN-POCD-INRESCISSION ) AND ( PAGE-CHANGE-NO )
                 MOVE "ACCOUNT IS CURRENTLY IN RESCISSION" TO MESS
                 PERFORM SEND-MESS
              END-IF.

       END-REC-LN-MODULE.
           EXIT.

      ************************************************
       ENTER-SCANKEY SECTION.
      ************************************************
           MOVE " ENTER LOAN #, OR F6 TO SCAN" TO MESS.
           PERFORM CALL-TMMSG.

           PERFORM OPEN-LN1-FILE.

           MOVE LN-PATH-OWNBR TO LN-OWNBR.

           PERFORM START-LN1-FILE.
       ENTER-KEY.
           MOVE "EN N080SCANKEY." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCANKEY.
           IF VDUSTATUS = "1<"
              GO TO READ-FILES-NEXT.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-KEY.
           IF VDUNUM0 = 0
              IF FIRST-SCAN NOT = 0
                 MOVE LN-REC TO LNOR-REC
                 IF ( LN-POCD-INRESCISSION )
                    MOVE "ACCOUNT IS CURRENTLY IN RESCISSION" TO MESS
                    PERFORM SEND-MESS
                 END-IF
                 GO TO CLOSE-SCANKEY
              ELSE
                 GO TO ENTER-KEY.
           MOVE VDUNUM0       TO LN-ACCTNO.
           MOVE LN-PATH-OWNBR TO LN-OWNBR.

           PERFORM START-LN1-FILE.
       READ-FILES-NEXT.
           PERFORM READ-LN1-FILE-NEXT.
           IF IO-BAD OR (LN-OWNBR NOT = LN-PATH-OWNBR)
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCANKEY.

           IF ( LN-POCD-VOID ) OR ( LN-POCD-RESCIND )
              GO TO READ-FILES-NEXT.
      *    UNLOCK LN-FILE.
           PERFORM DISPLAY-SCAN-KEY.
           PERFORM DISPLAY-FIELDS.
           MOVE 1 TO FIRST-SCAN.
           GO TO ENTER-KEY.
       DISPLAY-SCAN-KEY.
           MOVE "SN N060LNNO." TO VDU.
           MOVE LN-ACCTNO TO VDUNUM0.
           PERFORM SCREENIO.
       CLOSE-SCANKEY.
           MOVE 0 TO FIRST-SCAN.
           PERFORM CLOSE-LN1-FILE.
           MOVE SPACES TO MESS.
           PERFORM CALL-TMMSG.
       END-SCANKEY.
           EXIT.

      *************************************************
      *   ENTRY-MODULE
      *   NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *              1 - INITIAL NEW RECORD ENTRY
      *              2 - MODIFY NEW RECORD
      ************************************************
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
           IF (ELE = 14) AND (LN-SALEFIN NOT = "Y")
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
      * FORM-NUM1 CHECKS FOR PAGES 1 THRU 9
           IF FORM-KEY1 = " " AND FORM-P1 = "P" AND FORM-SEL1 NUMERIC
              IF FORM-SEL1 < 1 OR FORM-SEL1 = 7 OR FORM-SEL1 > 9
                 GO TO MODIFY-ROUTINE
              ELSE
                 GO TO CHECK-PAGE.
      * FORM-NUM2 CHECKS FOR PAGES 10 THRU 99
           IF FORM-KEY2 = " " AND FORM-P2 = "P" AND FORM-SEL2 NUMERIC
              IF NOT FORM-2-DIGITS
                 GO TO MODIFY-ROUTINE
              ELSE
                 GO TO CHECK-PAGE.
           GO TO CHECK-CHNO.
       CHECK-PAGE.
           MOVE "RWR" TO IO-TYPE.
           GO TO EXIT-ENTRY-MODULE.
       CHECK-CHNO.
           INSPECT ACTION REPLACING ALL SPACES
              BY ZEROS.
           IF CHNO-X NOT NUMERIC
              GO TO MODIFY-ROUTINE.
           IF CHNO = 0
              GO TO MODIFY-ROUTINE.
           IF CHNO > MAX
              GO TO MODIFY-ROUTINE.
           IF (CHNO = 14) AND (LN-SALEFIN NOT = "Y")
              GO TO MODIFY-ROUTINE.
           MOVE CHNO TO ELE.
       ENTER-ELE.
           IF CHKEY NOT = ZEROS
              IF CHFG(ELE) = " "
                 IF NEW-FLAG NOT = 1
                    GO TO MODIFY-ROUTINE.
           IF NEW-FLAG = 0
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
           GO TO A-01 A-02 A-03 A-04 A-05
                 A-06 A-07 A-08 A-09 A-10
                 A-11 A-12 A-13 A-14 A-15
                 A-16 A-17 A-18 A-19 A-20
                 A-21 A-22 A-23 A-24 A-25
                 A-26 A-27 A-28 A-29 A-30
                 A-31 A-32
                 DEPENDING ON ELE.
           GO TO END-ENTRY-MODULE.

      *    **********************************
      *    *      SET RECORD ELEMENTS
      *    **********************************
       A-01.
           MOVE VDUNUM2 TO DL-DLNO.
           IF DL-DLNO = 0
              GO TO ENTER-ELE.
           PERFORM OPEN-DL1-FILE.
           PERFORM READ-DL1-FILE.
           PERFORM CLOSE-DL1-FILE.
           IF IO-FG NOT = 0
              MOVE "*  NOT ON FILE  *" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           MOVE VDUNUM2 TO LN-DLNO.
           GO TO CREATE-ROUTINE.
       A-02.
           MOVE VDUNUM2 TO LN-DLRVPRIN(1).
           GO TO CREATE-ROUTINE.
       A-03.
           MOVE VDUNUM2 TO LN-DLRVPRIN(2).
           GO TO CREATE-ROUTINE.
       A-04.
           MOVE VDUNUM2 TO LN-DLRVPRIN(3).
           GO TO CREATE-ROUTINE.
       A-05.
           MOVE VDUNUM2 TO LN-DLRVCHGS(1).
           GO TO CREATE-ROUTINE.
       A-06.
           MOVE VDUNUM2 TO LN-DLRVCHGS(2).
           GO TO CREATE-ROUTINE.
       A-07.
           MOVE VDUNUM2 TO LN-DLRVCHGS(3).
           GO TO CREATE-ROUTINE.
       A-08.
           MOVE VDUNUM2 TO LN-DLPARVHELD.
      * DLCOMM-PP-CMSI:
       A-081.
           MOVE "ENNN062081." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO A-081.
           MOVE VDUNUM2 TO LN-DLCOMM-PP-CMSI.
           GO TO CREATE-ROUTINE.
       A-09.
           MOVE VDUNUM2 TO LN-DLPARVPAID.
      * DLCOMM-O3-CMSI:
       A-091.
           MOVE "ENNN062091." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO A-091.
           MOVE VDUNUM2 TO LN-DLCOMM-O3-CMSI.
           GO TO CREATE-ROUTINE.
       A-10.
           MOVE VDUNUM2 TO LN-DLDISC.
           GO TO CREATE-ROUTINE.
       A-11.
           IF VDUBUF NOT = "P" AND VDUBUF NOT = "F"
              AND VDUBUF NOT = "N"
                 GO TO ENTER-ELE.
           MOVE VDUBUF TO LN-DLRECOURSE.
           GO TO CREATE-ROUTINE.
       A-12.
           MOVE VDUNUM2 TO LN-DLPARTRCAMT.
           GO TO CREATE-ROUTINE.
       A-13.
           MOVE VDUNUM0 TO LN-DLPOOL.
           GO TO CREATE-ROUTINE.
       A-14.
           MOVE VDUNUM2 TO LN-PROCEEDS.
           GO TO CREATE-ROUTINE.
       A-15.
           MOVE VDUNUM2 TO LN-ORIGPURAMT.
           GO TO CREATE-ROUTINE.
       A-16.
           IF VDUNUM0 > LN-ORGTERM
              MOVE " EXCEEDS ORGINAL TERM" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           MOVE VDUNUM0 TO LN-ANTICTERM.
           GO TO CREATE-ROUTINE.
       A-17.
           IF VDUNUM2 < 0
              MOVE "BELOW ZERO ALLOWED FOR REVOLVING ACCOUNTS" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           MOVE VDUNUM2 TO LN-PURBAL.
           GO TO CREATE-ROUTINE.
       A-18.
           MOVE VDU-DATE-YYYYMMDD TO LN-TRW-DOFD-DATE.
           GO TO CREATE-ROUTINE.
       A-19.
           IF VDUBUF NOT = " " AND NOT = "Y" AND NOT = "C"
              MOVE "ENTER 'Y', 'C' OR SPACE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           MOVE VDUBUF TO LN-CL-REBATE.
           GO TO CREATE-ROUTINE.
       A-20.
           IF VDUBUF NOT = " " AND NOT = "Y" AND NOT = "C"
              MOVE "ENTER 'Y', 'C' OR SPACE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           MOVE VDUBUF TO LN-AH-REBATE.
           GO TO CREATE-ROUTINE.
       A-21.
           IF VDUBUF NOT = " " AND NOT = "Y" AND NOT = "C"
              MOVE "ENTER 'Y', 'C' OR SPACE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           MOVE VDUBUF TO LN-PP-REBATE.
           GO TO CREATE-ROUTINE.
       A-22.
           IF VDUBUF NOT = " " AND NOT = "Y" AND NOT = "C"
              MOVE "ENTER 'Y', 'C' OR SPACE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           MOVE VDUBUF TO LN-O1-REBATE.
           GO TO CREATE-ROUTINE.
       A-23.
           IF VDUBUF NOT = " " AND NOT = "Y" AND NOT = "C"
              MOVE "ENTER 'Y', 'C' OR SPACE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           MOVE VDUBUF TO LN-O2-REBATE.
           GO TO CREATE-ROUTINE.
       A-24.
           IF VDUBUF NOT = " " AND NOT = "Y" AND NOT = "C"
              MOVE "ENTER 'Y', 'C' OR SPACE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           MOVE VDUBUF TO LN-O3-REBATE.
           GO TO CREATE-ROUTINE.
       A-25.
           IF VDUBUF NOT = " " AND NOT = "Y" AND NOT = "C"
              MOVE "ENTER 'Y', 'C' OR SPACE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           MOVE VDUBUF TO LN-O4-REBATE.
           GO TO CREATE-ROUTINE.
       A-26.
           IF VDUBUF NOT = " " AND NOT = "Y" AND NOT = "C"
              MOVE "ENTER 'Y', 'C' OR SPACE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           MOVE VDUBUF TO LN-O5-REBATE.
           GO TO CREATE-ROUTINE.
       A-27.
      *    IF VDUBUF NOT = " " AND NOT = "Y" AND NOT = "C"
      *       MOVE "ENTER 'Y', 'C' OR SPACE" TO MESS
      *       PERFORM SEND-MESS
      *       GO TO ENTER-ELE.
      *    MOVE VDUBUF TO LN-O6-REBATE.
           GO TO CREATE-ROUTINE.
       A-28.
      *    IF VDUBUF NOT = " " AND NOT = "Y" AND NOT = "C"
      *       MOVE "ENTER 'Y', 'C' OR SPACE" TO MESS
      *       PERFORM SEND-MESS
      *       GO TO ENTER-ELE.
      *    MOVE VDUBUF TO LN-O7-REBATE.
           GO TO CREATE-ROUTINE.
       A-29.
           IF VDUBUF NOT = " " AND NOT = "Y" AND
              NOT = "C" AND NOT = "R"
              MOVE "ENTER 'Y', 'C', 'R' OR SPACE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           MOVE VDUBUF TO LN-INT-REBATE.
           GO TO CREATE-ROUTINE.
       A-30.
           IF VDUBUF NOT = " " AND NOT = "Y" AND NOT = "C"
              MOVE "ENTER 'Y', 'C' OR SPACE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           MOVE VDUBUF TO LN-SC-REBATE.
           GO TO CREATE-ROUTINE.
       A-31.
           IF VDUBUF NOT = " " AND NOT = "Y" AND NOT = "C"
              MOVE "ENTER 'Y', 'C' OR SPACE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           MOVE VDUBUF TO LN-MF-REBATE.
           GO TO CREATE-ROUTINE.
       A-32.
           IF VDUBUF NOT = " " AND NOT = "Y" AND NOT = "C"
              MOVE "ENTER 'Y', 'C' OR SPACE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           MOVE VDUBUF TO LN-DF-REBATE.
           GO TO CREATE-ROUTINE.

       END-ENTRY-MODULE.
           MOVE 0 TO FORM-SEL1 FORM-SEL2.
       EXIT-ENTRY-MODULE.
           EXIT.

      ************************************************
       WRITE-MODULE SECTION.
      ************************************************
           PERFORM OPEN-LN1-FILE.
           MOVE LN-REC TO LNCH-REC.
           IF LNOR-REC = LNCH-REC
              GO TO END-WRITE-MODULE.
           PERFORM READ-LN1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-WRITE-MODULE.
           IF LN-REC NOT = LNOR-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-WRITE-MODULE.

           PERFORM LOG-MAINTENANCE-CHGS.

           MOVE LNCH-REC TO LN-REC.

           PERFORM REWRITE-LN1-FILE.

           PERFORM UPDATE-NOTICE-FILE.

       END-WRITE-MODULE.
           PERFORM CLOSE-LN1-FILE.

      *******************************************
       LOG-MAINTENANCE-CHGS SECTION.
      *******************************************

           MOVE SPACES    TO WORK-ERRLOG-DETAIL.
           MOVE LN-OWNBR  TO WORK-ERRLOG-BR.
           MOVE LN-ACCTNO TO WORK-ERRLOG-NUMBER.
           MOVE "TO "     TO  WORK-ERRLOG-TO-X.

           IF LNOR-DLNO NOT = LNCH-DLNO
             MOVE "01 "                   TO WORK-ERRLOG-FIELD
             MOVE "DLNO    "              TO WORK-ERRLOG-DESC
             MOVE LNOR-DLNO               TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-DLNO               TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-DLRVPRIN(1) NOT = LNCH-DLRVPRIN(1)
             MOVE "02 "                   TO WORK-ERRLOG-FIELD
             MOVE "DLPRIN 1"              TO WORK-ERRLOG-DESC
             MOVE LNOR-DLRVPRIN(1)        TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-DLRVPRIN(1)        TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-DLRVPRIN(2) NOT = LNCH-DLRVPRIN(2)
             MOVE "03 "                   TO WORK-ERRLOG-FIELD
             MOVE "DLPRIN 2"              TO WORK-ERRLOG-DESC
             MOVE LNOR-DLRVPRIN(2)        TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-DLRVPRIN(2)        TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-DLRVPRIN(3) NOT = LNCH-DLRVPRIN(3)
             MOVE "04 "                   TO WORK-ERRLOG-FIELD
             MOVE "DLPRIN 3"              TO WORK-ERRLOG-DESC
             MOVE LNOR-DLRVPRIN(3)        TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-DLRVPRIN(3)        TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-DLRVCHGS(1) NOT = LNCH-DLRVCHGS(1)
             MOVE "05 "                   TO WORK-ERRLOG-FIELD
             MOVE "DLCHGS 1"              TO WORK-ERRLOG-DESC
             MOVE LNOR-DLRVCHGS(1)        TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-DLRVCHGS(1)        TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-DLRVCHGS(2) NOT = LNCH-DLRVCHGS(2)
             MOVE "06 "                   TO WORK-ERRLOG-FIELD
             MOVE "DLCHGS 2"              TO WORK-ERRLOG-DESC
             MOVE LNOR-DLRVCHGS(2)        TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-DLRVCHGS(2)        TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-DLRVCHGS(3) NOT = LNCH-DLRVCHGS(3)
             MOVE "07 "                   TO WORK-ERRLOG-FIELD
             MOVE "DLCHGS 3"              TO WORK-ERRLOG-DESC
             MOVE LNOR-DLRVCHGS(3)        TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-DLRVCHGS(3)        TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-DLPARVHELD NOT = LNCH-DLPARVHELD
             MOVE "08 "                   TO WORK-ERRLOG-FIELD
             MOVE "PARVHELD "             TO WORK-ERRLOG-DESC
             MOVE LNOR-DLPARVHELD         TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-DLPARVHELD         TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-DLCOMM-PP-CMSI NOT = LNCH-DLCOMM-PP-CMSI
             MOVE "081"                   TO WORK-ERRLOG-FIELD
             MOVE "COMM PP  "             TO WORK-ERRLOG-DESC
             MOVE LNOR-DLCOMM-PP-CMSI     TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-DLCOMM-PP-CMSI     TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-DLPARVPAID NOT = LNCH-DLPARVPAID
             MOVE "09 "                   TO WORK-ERRLOG-FIELD
             MOVE "PARVPAID "             TO WORK-ERRLOG-DESC
             MOVE LNOR-DLPARVPAID         TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-DLPARVPAID         TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-DLCOMM-O3-CMSI NOT = LNCH-DLCOMM-O3-CMSI
             MOVE "091"                   TO WORK-ERRLOG-FIELD
             MOVE "COMM O3  "             TO WORK-ERRLOG-DESC
             MOVE LNOR-DLCOMM-O3-CMSI     TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-DLCOMM-O3-CMSI     TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-DLDISC NOT = LNCH-DLDISC
             MOVE "10 "                   TO WORK-ERRLOG-FIELD
             MOVE "DLDISC  "              TO WORK-ERRLOG-DESC
             MOVE LNOR-DLDISC             TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-DLDISC             TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-DLRECOURSE NOT = LNCH-DLRECOURSE
             MOVE "11 "                   TO WORK-ERRLOG-FIELD
             MOVE "RECOURSE"              TO WORK-ERRLOG-DESC
             MOVE LNOR-DLRECOURSE         TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-DLRECOURSE         TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-DLPARTRCAMT NOT = LNCH-DLPARTRCAMT
             MOVE "12 "                   TO WORK-ERRLOG-FIELD
             MOVE "PARTCAMT"              TO WORK-ERRLOG-DESC
             MOVE LNOR-DLPARTRCAMT        TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-DLPARTRCAMT        TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-DLPOOL NOT = LNCH-DLPOOL
             MOVE "13 "                   TO WORK-ERRLOG-FIELD
             MOVE "DLPOOL  "              TO WORK-ERRLOG-DESC
             MOVE LNOR-DLPOOL             TO WORK-ERRLOG-ORG-N0
             MOVE LNCH-DLPOOL             TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-PROCEEDS NOT = LNCH-PROCEEDS
             MOVE "14 "                   TO WORK-ERRLOG-FIELD
             MOVE "PROCEEDS"              TO WORK-ERRLOG-DESC
             MOVE LNOR-PROCEEDS           TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-PROCEEDS           TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-ORIGPURAMT NOT = LNCH-ORIGPURAMT
             MOVE "15 "                   TO WORK-ERRLOG-FIELD
             MOVE "ORIG PUR"              TO WORK-ERRLOG-DESC
             MOVE LNOR-ORIGPURAMT         TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-ORIGPURAMT         TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-ANTICTERM NOT = LNCH-ANTICTERM
             MOVE "16 "                   TO WORK-ERRLOG-FIELD
             MOVE "ANT TERM"              TO WORK-ERRLOG-DESC
             MOVE LNOR-ANTICTERM          TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-ANTICTERM          TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-PURBAL NOT = LNCH-PURBAL
             MOVE "17 "                   TO WORK-ERRLOG-FIELD
             MOVE "PURBAL  "              TO WORK-ERRLOG-DESC
             MOVE LNOR-PURBAL             TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-PURBAL             TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-TRW-DOFD-DATE NOT = LNCH-TRW-DOFD-DATE
             MOVE "18 "                   TO WORK-ERRLOG-FIELD
             MOVE "TRW DOFD-DATE"         TO WORK-ERRLOG-DESC
             MOVE LNOR-TRW-DOFD-DATE      TO DATE-YYYYMMDD       
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-TRW-DOFD-DATE      TO DATE-YYYYMMDD         
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-CL-REBATE NOT = LNCH-CL-REBATE
             MOVE "19 "                   TO WORK-ERRLOG-FIELD
             MOVE "CLREBATE"              TO WORK-ERRLOG-DESC
             MOVE LNOR-CL-REBATE          TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-CL-REBATE          TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-AH-REBATE NOT = LNCH-AH-REBATE
             MOVE "20 "                   TO WORK-ERRLOG-FIELD
             MOVE "AHREBATE"              TO WORK-ERRLOG-DESC
             MOVE LNOR-AH-REBATE          TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-AH-REBATE          TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-PP-REBATE NOT = LNCH-PP-REBATE
             MOVE "21 "                   TO WORK-ERRLOG-FIELD
             MOVE "PPREBATE"              TO WORK-ERRLOG-DESC
             MOVE LNOR-PP-REBATE          TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-PP-REBATE          TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-O1-REBATE NOT = LNCH-O1-REBATE
             MOVE "22 "                   TO WORK-ERRLOG-FIELD
             MOVE "O1REBATE"              TO WORK-ERRLOG-DESC
             MOVE LNOR-O1-REBATE          TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-O1-REBATE          TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-O2-REBATE NOT = LNCH-O2-REBATE
             MOVE "23 "                   TO WORK-ERRLOG-FIELD
             MOVE "O2REBATE"              TO WORK-ERRLOG-DESC
             MOVE LNOR-O2-REBATE          TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-O2-REBATE          TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-O3-REBATE NOT = LNCH-O3-REBATE
             MOVE "24 "                   TO WORK-ERRLOG-FIELD
             MOVE "O3REBATE"              TO WORK-ERRLOG-DESC
             MOVE LNOR-O3-REBATE          TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-O3-REBATE          TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-O4-REBATE NOT = LNCH-O4-REBATE
             MOVE "25 "                   TO WORK-ERRLOG-FIELD
             MOVE "O4REBATE"              TO WORK-ERRLOG-DESC
             MOVE LNOR-O4-REBATE          TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-O4-REBATE          TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-O5-REBATE NOT = LNCH-O5-REBATE
             MOVE "26 "                   TO WORK-ERRLOG-FIELD
             MOVE "O5REBATE"              TO WORK-ERRLOG-DESC
             MOVE LNOR-O5-REBATE          TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-O5-REBATE          TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-INT-REBATE NOT = LNCH-INT-REBATE
             MOVE "29 "                   TO WORK-ERRLOG-FIELD
             MOVE "INREBATE"              TO WORK-ERRLOG-DESC
             MOVE LNOR-INT-REBATE         TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-INT-REBATE         TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-SC-REBATE NOT = LNCH-SC-REBATE
             MOVE "30 "                   TO WORK-ERRLOG-FIELD
             MOVE "SCREBATE"              TO WORK-ERRLOG-DESC
             MOVE LNOR-SC-REBATE          TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-SC-REBATE          TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-MF-REBATE NOT = LNCH-MF-REBATE
             MOVE "31 "                   TO WORK-ERRLOG-FIELD
             MOVE "MFREBATE"              TO WORK-ERRLOG-DESC
             MOVE LNOR-MF-REBATE          TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-MF-REBATE          TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-DF-REBATE NOT = LNCH-DF-REBATE
             MOVE "32 "                   TO WORK-ERRLOG-FIELD
             MOVE "DFREBATE"              TO WORK-ERRLOG-DESC
             MOVE LNOR-DF-REBATE          TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-DF-REBATE          TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

      *******************************************
       LOG-FIELD-CHANGE SECTION.
      *******************************************
      * CODE TO WRITE CHANGE FILE.

           MOVE "F" TO ERRLOGW-ACTION.
           MOVE WORK-ERRLOG-DETAIL TO ERRLOGW-DETAIL.
           MOVE FORM-PATHNAME TO ERRLOGW-PROGX.

           MOVE "CL/ERRLOG" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PROGX.

      ***********************************************
       UPDATE-NOTICE-FILE SECTION.
      ***********************************************
           PERFORM OPEN-NO1-FILE.
           MOVE LN-OWNBR  TO NO-BRNO.
           MOVE LN-ACCTNO TO NO-ACCTNO CRNOR2-ACCTNO.
           PERFORM READ-NO1-FILE.

           ACCEPT CRNOR2-SYSDATE FROM CENTURY-DATE.
           MOVE "CL/CRNOR2"    TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CRNOR2-LINK NO-REC
                                 POFF-WORKERS LN-REC.
           CANCEL FORM-PROGX.

           MOVE LN-OWNBR  TO NO-BRNO.
           MOVE LN-ACCTNO TO NO-ACCTNO.
           IF IO-FG = 0
              PERFORM REWRITE-NO1-FILE
           ELSE
              PERFORM WRITE-NO1-FILE.

           PERFORM CLOSE-NO1-FILE.
       EXIT-UPDATE-NOTICE-FILE.
           EXIT.
      *******************************************
       DISPLAY-FIELDS SECTION.
      *******************************************
           MOVE LN-DLNO           TO F01.
           MOVE LN-DLRVPRIN(1)    TO F02.
           MOVE LN-DLRVPRIN(2)    TO F03.
           MOVE LN-DLRVPRIN(3)    TO F04.
           MOVE LN-DLRVCHGS(1)    TO F05.
           MOVE LN-DLRVCHGS(2)    TO F06.
           MOVE LN-DLRVCHGS(3)    TO F07.
           MOVE LN-DLPARVHELD     TO F08.
           MOVE LN-DLCOMM-PP-CMSI TO F081.
           MOVE LN-DLPARVPAID     TO F09.
           MOVE LN-DLCOMM-O3-CMSI TO F091.
           MOVE LN-DLDISC TO F10.
           MOVE LN-DLRECOURSE TO F11.
           MOVE LN-DLPARTRCAMT TO F12.
           MOVE LN-DLPOOL TO F13.
           IF LN-SALEFIN = "Y"
              MOVE LN-PROCEEDS TO F14.
           MOVE LN-ORIGPURAMT TO F15.
           MOVE LN-ANTICTERM TO F16.
           MOVE LN-PURBAL TO F17.
           MOVE LN-TRW-DOFD-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO F18.
           MOVE LN-CL-REBATE TO F19.
           MOVE LN-AH-REBATE TO F20.
           MOVE LN-PP-REBATE TO F21.
           MOVE LN-O1-REBATE TO F22.
           MOVE LN-O2-REBATE TO F23.
           MOVE LN-O3-REBATE TO F24.
           MOVE LN-O4-REBATE TO F25.
           MOVE LN-O5-REBATE TO F26.
      *    MOVE LN-O6-REBATE TO F27.
      *    MOVE LN-O7-REBATE TO F28.
           MOVE LN-INT-REBATE TO F29.
           MOVE LN-SC-REBATE TO F30.
           MOVE LN-MF-REBATE TO F31.
           MOVE LN-DF-REBATE TO F32.
           MOVE LN-ORGBR TO WS-ORGBR.

           STRING LN-ORGST, "-", WS-ORGBR, "-", LN-SPRCLASS, "(",
                  LN-CLASS, ")", "-", LN-SUBCLASS, "-", LN-LAWCODE,
                  "-", LN-SMALLOAN, LN-SALEFIN, LN-SERVACCT, LN-LOANTYPE
                  DELIMITED BY "  " INTO FLNSTAT.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       DISPLAY-FIELDS-EXIT.
           EXIT.


      ************************************************
       FORM-RESET SECTION.
      ************************************************
           MOVE EXT-CONAME-CONAME TO LNMAN7-CONAME.
           MOVE EXT-CONAME-SYSDATE TO LNMAN7-SYSDATE.
           MOVE EXT-CONAME-DAILY-PASSWORD TO CHAN.
           DISPLAY LNMAN7-SCREEN UPON LNMAIN-WINDOW-HANDLE.
           MOVE LNMAN7-FORM-FIELDS TO WR-BUF.
           MOVE LNMAN7-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE LNMAN7-HELP--FILE TO HELP-LINK-FILE.
           MOVE LNMAN7-HELP--DIR TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/LNMAN7_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

       COPY "LIBGB/GPENV.CPY".

      ************************************************
       MISC-PARAGRAPHS SECTION.
      ************************************************
       COPY "LIBGB/GETENV.CPY".
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
       ALREADY-CREATED.
	   MOVE "RECORD HAS BEEN ALREADY CREATED!" TO MESS.
	   PERFORM SEND-MESS.
       UNDERGONE-CHANGE.
	   MOVE
	   "THIS RECORD HAS UNDERGONE CHANGE BY ANOTHER USER,"
	   TO MESS.
	   PERFORM SEND-MESS.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".
	
      ************************************************
       IO-ROUTINES SECTION.
      ************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-DL1-FILE.
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPNOGS_SQL.CPY".
       COPY "LIBLP/LPDLGS_SQL.CPY".
       COPY "LIBLP/LPLN1IN.CPY".
       COPY "LIBLP/LPNO1IN.CPY".
       COPY "LIBLP/LPDL1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      ************************************************
       END-ROUTINE SECTION.
      ************************************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY LNMAN7-SCREEN.
           MOVE FORM-PATH TO FORM-PATHNAME.
           MOVE LNMAIN-LINK TO FORM-LINK-AREA.
           EXIT PROGRAM.

