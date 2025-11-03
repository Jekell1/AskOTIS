       IDENTIFICATION DIVISION.
       PROGRAM-ID.      LNMAN9.
      ******************************************************************
      *
      *   DESC: LOAN MASTER FILE MAINTENANCE   PAGE 9
      *
      *=================================================================
      * REV:
      * MJD 050895 ADDED CODE TO WRITE CHANGE FILE AFTER REWRITE
      * KEC 082595 ADDED LN-MTD-PRIN, LC, INT, OT, AND EX.
      * KEC 082595 ADDED LN-MONTHEND-BAL.
      * KEC 083095 ADDED LN-BNKRPT-CHAPTER.
      * GLF 082496 ADDED LNSTAT TO BOTTOM OF PAGE.`
      * KEC 111496 CHANGED LN-POCD FOR RESCISSION.
      * KEC 012397 ADDED LN-APPNO.
      * JTG 082097 ADDED LN-PY-YYYYMM-RECDEL & LN-MTD-PAYS-RECDEL
      * MJD 082187 CHANGED RECORD SIZE FROM 1500 TO 1570
      *            ADD LOGIC FOR PAGE 10.
      * MJD 112197 EXPANDED LN-REC TO 1800 BYTES
      * BAH 010898 ADDED LN-TRW-STATUS
      * JTG 050498 ADDED LN-TOT-PARTIAL-CHGOFF, MERCURY TRCD = 'PX'
      * MJD 072498 FIXED BUG, SETTING NEW FLAG TO "N" BEFORE GOING
      *            TO PAGE 10. RESULT: PAGE 10 DOES NOT WALK THROUGH
      *            FIRELDS ON NEW RECORD ENTRY.
      * KEC 090398 DISPLAYING LN-REVOLVING FOR 'LNSTAT'.
      * KEC 091598 ADDED LOGIC TO ACCESS NEW PAGE 11.
      * JTG 120198 ADDED LOGIC TO ACCESS NEW PAGE 12.
      * MJD 990203 FIXED BUG. FIELD 21 WAS REDISPLAYING IN FIELD 14
      * KEC 1999.0203 ADDED LN-TRW-SPECIAL-CODE        (TFC) PR#: 00023
      *
      * JTG 020123 EXPANDED MTD-PAYS-RECDEL FIELD TO 1,000,000 LENDMARK PR#J021
      * BAH 050927 ADDED LN-TRW-COMPLIANCE-CODE LIBERAL PR# 2591
      * AMM 051018 ADDED DETAILED, FIELD-BY-FIELD MAINTENANCE LOG IN
      *            /USR/DATA/R1/LG/CHG.YYMMDD
      * BAH 100326 LN-TRW-STATUS SHOULD NOT FORCE NUMERICS, PL# 679 SOAUTO
      * MJD 121025 A30 CONVERSION COMPLETE.
      * MJD 121217 ADDED LNMAIN-WINDOW-HANDLE TO SCREEN DISPLAY
      * MJD 130117 CHANGED CALL TMMSG TO PERFORM CALL-TMMSG
      *
      * KEC 2016.0509 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED LN-APPNO TO USE LN-APP-NUMBER FOR MA1-KEY.
      * CS 160617 REMOVED LN-SEQNO, MOVE EXT-FILPATH-BRNO TO LN-OWNBR
      *            PRIOR TO READ
      * CS 170425 ACTIVATE 8 DIGIT DATES
      *
      * KEC 2017.0526 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          REMOVED ORIGINAL-REC AND CHANGED-REC WHICH NOW USING
      *                  LNOR-REC AND LNCH-REC.
      *
      * BAH 170914 RENAMED LN-PYMMYY-RECDEL TO LN-PY-YYYYMM-RECDEL AND FIXED
      *            LOGIC TO WORK
      * BAH 181212 GLOBAL LNFILE
      * BAH 20190627 REMOVED LN-REVOLVING, PT0153
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
           "@(#)MN/LNMAN9.CBL S35 02/21/24-12:30:31 >".

      *    **********************************
      *    *      PROGRAM WORKERS
      *    **********************************
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".

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
       01  AMT-HOLDER        PIC 9(6)V99 COMP-3 VALUE 0.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    'PAGE-CHANGE-FLAG' IS FOR KNOWING IF COMING FROM ANOTHER PAGE
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  PAGE-CHANGE-FLAG      PIC X(1)    VALUE "N".
           88  PAGE-CHANGE-NO                VALUE "N".
           88  PAGE-CHANGE-YES               VALUE "Y".

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
           03  F01        PIC Z(6).99        BLANK ZERO.
           03  F02        PIC Z(6).99        BLANK ZERO.
           03  F03        PIC Z(6).99        BLANK ZERO.
           03  F04        PIC Z(5).99        BLANK ZERO.
           03  F05        PIC Z(4).99        BLANK ZERO.
           03  F06        PIC Z(4).99        BLANK ZERO.
           03  F07        PIC Z(3)           BLANK ZERO.
           03  F08        PIC Z(6).99        BLANK ZERO.
           03  F09        PIC Z(6).99        BLANK ZERO.
           03  F10        PIC 99/99/99       BLANK ZERO.
           03  F11        PIC Z(5)9.99-      BLANK ZERO.
           03  F12        PIC Z(4)9.99-      BLANK ZERO.
           03  F13        PIC Z(4)9.99-      BLANK ZERO.
           03  F14        PIC Z(4)9.99-      BLANK ZERO.
           03  F15        PIC Z(4)9.99-      BLANK ZERO.
           03  F16        PIC Z(5)9.99       BLANK ZERO.
           03  F17        PIC 9(2)           BLANK ZERO.
           03  F18        PIC X.
           03  F19        PIC X.
           03  F20        PIC Z(9).
           03  F21        PIC 99/99          BLANK ZERO.
           03  F22        PIC Z(7).99-       BLANK ZERO.
           03  F23        PIC XX.
           03  F231       PIC XX.
           03  F232       PIC XX.
           03  F24        PIC Z(6).99-       BLANK ZERO.
           03  FLNSTAT    PIC X(30).

       01  DISPLAY-LIST.
           03  FILLER     PIC X(40) VALUE
               "010 020 030 040 050 060 070 080 090 100 ".
           03  FILLER     PIC X(40) VALUE
               "110 120 130 140 150 160 170 180 190 200 ".
           03  FILLER     PIC X(31)  VALUE
               "210 220 230 231 232 240 LNSTAT.".

      *    **********************************
      *    *   ELEMENT SPECIFICATIONS       *
      *    **********************************
       01  MAX            PIC 99   VALUE 24.
       01  SPEC-TABLE.
      * LN-SUM-LNAMT
           03  FILLER     PIC X(8) VALUE "EN N062N".
      * LN-SUM-FINCHG
           03  FILLER     PIC X(8) VALUE "EN N062N".
      * LN-SUM-INTCHG
           03  FILLER     PIC X(8) VALUE "EN N062N".
      * LN-SUM-SERCHG
           03  FILLER     PIC X(8) VALUE "EN N052N".
      * LN-SUM-EXTCHG
           03  FILLER     PIC X(8) VALUE "EN N042N".
      * LN-SUM-MAINTFEE
           03  FILLER     PIC X(8) VALUE "EN N042N".
      * LN-SUM-ORGTERM
           03  FILLER     PIC X(8) VALUE "EN N030N".
      * LN-SUM-ORIGPURAMT
           03  FILLER     PIC X(8) VALUE "EN N062N".
      * LN-SUM-REGPYAMT
           03  FILLER     PIC X(8) VALUE "EN N062N".
      * LN-INS-CONFIRM-DATE
           03  FILLER     PIC X(8) VALUE "ERNM060N".
      * 11. MONEYS APPLIED TO PRINCIPAL.
           03  FILLER     PIC X(8) VALUE "EN S062N".
      * 12. MONEYS APPLIED TO LATE CHARGES.
           03  FILLER     PIC X(8) VALUE "EN S052N".
      * 13. MONEYS APPLIED TO INTEREST.
           03  FILLER     PIC X(8) VALUE "EN S052N".
      * 14. MONEYS APPLIED TO OTHER 1 & 2.
           03  FILLER     PIC X(8) VALUE "EN S052N".
      * 15. MONEYS APPLIED RE: EXCEPTION PAYMENTS.
           03  FILLER     PIC X(8) VALUE "EN S052N".
      * 16. END OF MONTH BALANCE
           03  FILLER     PIC X(8) VALUE "EN N062N".
      * 17. BANKRUPTCY CHAPTER
           03  FILLER     PIC X(8) VALUE "EN N020N".
      * 18. BEG OF MONTH CONTR STATUS
           03  FILLER     PIC X(8) VALUE "E  A010N".
      * 19. BEG OF MONTH RECENCY STATUS
           03  FILLER     PIC X(8) VALUE "E  A010N".
      * 20. APPLICATION NUMBER
           03  FILLER     PIC X(8) VALUE "ENNN090N".
      * 21. FULLPYMMYY (WORLD)
           03  FILLER     PIC X(8) VALUE "ENNN040N".
      * 22. MTD-PAYMENTS (WORLD)
           03  FILLER     PIC X(8) VALUE "ENNS072N".
      * 23. TRW-STATUS
           03  FILLER     PIC X(8) VALUE "E  A020N".
      * 24. TOT-PARTIAL-CHGOFF
           03  FILLER     PIC X(8) VALUE "ENNS062N".

       01  SPEC-TAB REDEFINES SPEC-TABLE.
           03  SPEC-REC OCCURS 24 TIMES.
               05  SPEC PIC X(7).
               05  CHFG PIC X.

      ***********************************************
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/LNMAN9_DEF.CPY".
       COPY "LIBMN/LNMAN9_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/GETFMW.CPY".
       01 FORM-LINK-AREA    PIC X(2000).

       SCREEN SECTION.
       COPY "LIBMN/LNMAN9_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME FORM-LINK-AREA EXIT-PATHNAME.

      ******************************************
       MAIN-PROGRAM SECTION.
      ******************************************
           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "LNMAN9" TO VDU-FORM-NAME.
           MOVE FORM-LINK-AREA TO LNMAIN-LINK.

       MAIN-MODULE.
           PERFORM FORM-RESET.

           MOVE " " TO VDU-PGMENU-SELECTION.
           IF FIRST-TIME = 0
              MOVE 1 TO FIRST-TIME.

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
              MOVE "P" TO FORM-P2
              MOVE 10 TO FORM-SEL2
              MOVE " " TO SAVEKEY.
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

      *****************************************
       REC-LN-MODULE SECTION.
      *****************************************
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

      ***********************************************
       ENTER-SCANKEY SECTION.
      ***********************************************
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

      ************************************************
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
              IF FORM-SEL1 < 1 OR FORM-SEL1 > 8
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
                IF CHKEY NOT = EXT-CONAME-DAILY-PASSWORD
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
           GO TO A-01 A-02 A-03 A-04 A-05 A-06 A-07 A-08 A-09 A-10
                 A-11 A-12 A-13 A-14 A-15 A-16 A-17 A-18 A-19 A-20
                 A-21 A-22 A-23 A-24
                 DEPENDING ON ELE.
           GO TO END-ENTRY-MODULE.

      *    **********************************
      *    *      SET RECORD ELEMENTS
      *    **********************************
       A-01.
           MOVE VDUNUM2 TO LN-SUM-LNAMT.
           GO TO CREATE-ROUTINE.
       A-02.
           MOVE VDUNUM2 TO LN-SUM-FINCHG.
           GO TO CREATE-ROUTINE.
       A-03.
           MOVE VDUNUM2 TO LN-SUM-INTCHG.
           GO TO CREATE-ROUTINE.
       A-04.
           MOVE VDUNUM2 TO LN-SUM-SERCHG.
           GO TO CREATE-ROUTINE.
       A-05.
           MOVE VDUNUM2 TO LN-SUM-EXTCHG.
           GO TO CREATE-ROUTINE.
       A-06.
           MOVE VDUNUM2 TO LN-SUM-MAINTFEE.
           DIVIDE VDUNUM2 BY LN-SUM-ORGTERM GIVING VDUNUM2
                                          REMAINDER AMT-HOLDER.
           IF AMT-HOLDER NOT = 0
              MOVE "FEE MUST BE EVENLY DIVISABLE BY TERM" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-07.
           MOVE VDUNUM0 TO LN-SUM-ORGTERM.
           GO TO CREATE-ROUTINE.
       A-08.
           MOVE VDUNUM2 TO LN-SUM-ORIGPURAMT.
           GO TO CREATE-ROUTINE.
       A-09.
           MOVE VDUNUM2 TO LN-SUM-REGPYAMT.
           GO TO CREATE-ROUTINE.
       A-10.
           MOVE VDU-DATE-YYYYMMDD TO LN-INS-CONFIRM-DATE.
           GO TO CREATE-ROUTINE.
       A-11.
           MOVE VDUNUM2 TO LN-MTD-PRIN.
           GO TO CREATE-ROUTINE.
       A-12.
           MOVE VDUNUM2 TO LN-MTD-LC.
           GO TO CREATE-ROUTINE.
       A-13.
           MOVE VDUNUM2 TO LN-MTD-INT.
           GO TO CREATE-ROUTINE.
       A-14.
           MOVE VDUNUM2 TO LN-MTD-OT.
           GO TO CREATE-ROUTINE.
       A-15.
           MOVE VDUNUM2 TO LN-MTD-EX.
           GO TO CREATE-ROUTINE.
       A-16.
           MOVE VDUNUM2 TO LN-MONTHEND-BAL.
           GO TO CREATE-ROUTINE.
       A-17.
           MOVE VDUNUM0 TO LN-BNKRPT-CHAPTER.
           GO TO CREATE-ROUTINE.
       A-18.
           MOVE VDUBUF TO LN-BMO-CON-STATUS.
           IF NOT (LN-BMO-CON-STATUS = "Y" OR "M" OR "0" OR "1"
                                 OR "2" OR "3" OR "4" OR "5" OR "6"
                                 OR "7"OR "8" OR "9"
                  )
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-19.
           MOVE VDUBUF TO LN-BMO-REC-STATUS.
           IF NOT (LN-BMO-REC-STATUS = "Y" OR "M" OR "0" OR "1"
                                 OR "2" OR "3" OR "4" OR "5" OR "6"
                                 OR "7"OR "8" OR "9"
                  )
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.

       A-20.
           MOVE VDUNUM0 TO LN-APP-NUMBER.
           GO TO CREATE-ROUTINE.
       A-21.
           MOVE VDUNUM0 TO DATE-MMYY F21.
           PERFORM CONVERT-MMYY-TO-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMM.
           MOVE DATE-YYYYMM TO LN-PY-YYYYMM-RECDEL.
           MOVE "S  A050210." TO VDU.
           MOVE F21 TO VDUBUF.
           PERFORM SCREENIO.
           GO TO CREATE-ROUTINE.
       A-22.
           MOVE VDUNUM2 TO LN-MTD-PAYS-RECDEL.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       A-23.
           MOVE VDUBUF TO LN-TRW-STATUS.
           IF LN-TRW-STATUS = "0 " OR " 0"
              MOVE "MUST BE 00 OR SPACES TO CLEAR IT " TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           IF LN-TRW-STATUS = "04" OR "21" OR "66" OR "67" OR "68"
                             OR "69" OR "70" OR "85" OR "86" OR
                                  "87" OR "98" OR "42"
              MOVE "NOT VALID WITH METRO2 !!" TO MESS
              PERFORM SEND-MESS
              MOVE "TRY USING A SPECIAL CODE" TO MESS
              PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       A-231.
           MOVE "E  A020231." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-ELE.
           IF (        F1-KEY ) MOVE "CLR" TO IO-TYPE
                                GO TO END-ENTRY-MODULE.
           IF ( NOT ENTER-KEY ) GO TO A-231.

           MOVE VDUBUF TO LN-TRW-SPECIAL-CODE.

      *-----------------------------------------------------------------
       A-232.
           MOVE "E  A020232." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO A-231.
           IF (        F1-KEY ) MOVE "CLR" TO IO-TYPE
                                GO TO END-ENTRY-MODULE.
           IF ( NOT ENTER-KEY ) GO TO A-232.

           MOVE VDUBUF TO LN-TRW-COMPLIANCE-CODE.
           IF NOT LN-TRW-COMPLIANCE-VALID
              IF LN-TRW-COMPLIANCE-CODE NOT = SPACES
                 GO TO A-232.

           GO TO CREATE-ROUTINE.
      *-----------------------------------------------------------------
       A-24.
           MOVE VDUNUM2 TO LN-TOT-PARTIAL-CHGOFF.
           GO TO CREATE-ROUTINE.

       END-ENTRY-MODULE.
           MOVE 0 TO FORM-SEL1 FORM-SEL2.
       EXIT-ENTRY-MODULE.
           EXIT.

      ***********************************************
       WRITE-MODULE SECTION.
      ***********************************************
       REWRITE-RECORD.
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

       END-WRITE-MODULE.
           PERFORM CLOSE-LN1-FILE.

      ***********************************************
       LOG-MAINTENANCE-CHGS SECTION.
      ***********************************************

           MOVE SPACES    TO WORK-ERRLOG-DETAIL.
           MOVE LN-OWNBR  TO WORK-ERRLOG-BR.
           MOVE LN-ACCTNO TO WORK-ERRLOG-NUMBER.
           MOVE "TO "     TO  WORK-ERRLOG-TO-X.

           IF LNOR-SUM-LNAMT NOT = LNCH-SUM-LNAMT
             MOVE "01 "                   TO WORK-ERRLOG-FIELD
             MOVE "SUMLNAMT"              TO WORK-ERRLOG-DESC
             MOVE LNOR-SUM-LNAMT          TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-SUM-LNAMT          TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-SUM-FINCHG NOT = LNCH-SUM-FINCHG
             MOVE "02 "                   TO WORK-ERRLOG-FIELD
             MOVE "FINCHG  "              TO WORK-ERRLOG-DESC
             MOVE LNOR-SUM-FINCHG         TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-SUM-FINCHG         TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-SUM-INTCHG NOT = LNCH-SUM-INTCHG
             MOVE "03 "                   TO WORK-ERRLOG-FIELD
             MOVE "INTCHG  "              TO WORK-ERRLOG-DESC
             MOVE LNOR-SUM-INTCHG         TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-SUM-INTCHG         TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-SUM-SERCHG NOT = LNCH-SUM-SERCHG
             MOVE "04 "                   TO WORK-ERRLOG-FIELD
             MOVE "SERCHG  "              TO WORK-ERRLOG-DESC
             MOVE LNOR-SUM-SERCHG         TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-SUM-SERCHG         TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-SUM-EXTCHG NOT = LNCH-SUM-EXTCHG
             MOVE "05 "                   TO WORK-ERRLOG-FIELD
             MOVE "EXTCHG  "              TO WORK-ERRLOG-DESC
             MOVE LNOR-SUM-EXTCHG         TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-SUM-EXTCHG         TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-SUM-MAINTFEE NOT = LNCH-SUM-MAINTFEE
             MOVE "06 "                   TO WORK-ERRLOG-FIELD
             MOVE "MAINTFEE"              TO WORK-ERRLOG-DESC
             MOVE LNOR-SUM-MAINTFEE       TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-SUM-MAINTFEE       TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-SUM-ORGTERM NOT = LNCH-SUM-ORGTERM
             MOVE "07 "                   TO WORK-ERRLOG-FIELD
             MOVE "ORGTERM "              TO WORK-ERRLOG-DESC
             MOVE LNOR-SUM-ORGTERM        TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-SUM-ORGTERM        TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-SUM-ORIGPURAMT NOT = LNCH-SUM-ORIGPURAMT
             MOVE "08 "                   TO WORK-ERRLOG-FIELD
             MOVE "ORIG PUR"              TO WORK-ERRLOG-DESC
             MOVE LNOR-SUM-ORIGPURAMT     TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-SUM-ORIGPURAMT     TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-SUM-REGPYAMT NOT = LNCH-SUM-REGPYAMT
             MOVE "09 "                   TO WORK-ERRLOG-FIELD
             MOVE "REGPYAMT"              TO WORK-ERRLOG-DESC
             MOVE LNOR-SUM-REGPYAMT       TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-SUM-REGPYAMT       TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-INS-CONFIRM-DATE = LNCH-INS-CONFIRM-DATE
             MOVE "10 "                   TO WORK-ERRLOG-FIELD
             MOVE "CONFIRM "              TO WORK-ERRLOG-DESC
             MOVE LNOR-INS-CONFIRM-DATE   TO DATE-YYYYMMDD       
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO WORK-ERRLOG-ORG-N0
             MOVE LNCH-INS-CONFIRM-DATE   TO DATE-YYYYMMDD      
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-MTD-PRIN NOT = LNCH-MTD-PRIN
             MOVE "11 "                   TO WORK-ERRLOG-FIELD
             MOVE "MTDPRIN"               TO WORK-ERRLOG-DESC
             MOVE LNOR-MTD-PRIN           TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-MTD-PRIN           TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-MTD-LC NOT = LNCH-MTD-LC
             MOVE "12 "                   TO WORK-ERRLOG-FIELD
             MOVE "MTD LC  "              TO WORK-ERRLOG-DESC
             MOVE LNOR-MTD-LC             TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-MTD-LC             TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-MTD-INT NOT = LNCH-MTD-INT
             MOVE "13 "                   TO WORK-ERRLOG-FIELD
             MOVE "MTD INT "              TO WORK-ERRLOG-DESC
             MOVE LNOR-MTD-INT            TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-MTD-INT            TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-MTD-OT NOT = LNCH-MTD-OT
             MOVE "14 "                   TO WORK-ERRLOG-FIELD
             MOVE "MTD OT  "              TO WORK-ERRLOG-DESC
             MOVE LNOR-MTD-OT             TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-MTD-OT             TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-MTD-EX NOT = LNCH-MTD-EX
             MOVE "15 "                   TO WORK-ERRLOG-FIELD
             MOVE "MTD EX  "              TO WORK-ERRLOG-DESC
             MOVE LNOR-MTD-EX             TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-MTD-EX             TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-MONTHEND-BAL NOT = LNCH-MONTHEND-BAL
             MOVE "16 "                   TO WORK-ERRLOG-FIELD
             MOVE "MONTHEND"              TO WORK-ERRLOG-DESC
             MOVE LNOR-MONTHEND-BAL       TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-MONTHEND-BAL       TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE

           IF LNOR-BNKRPT-CHAPTER NOT = LNCH-BNKRPT-CHAPTER
             MOVE "17 "                   TO WORK-ERRLOG-FIELD
             MOVE "BNKRPTCH"              TO WORK-ERRLOG-DESC
             MOVE LNOR-BNKRPT-CHAPTER     TO WORK-ERRLOG-ORG-N0
             MOVE LNCH-BNKRPT-CHAPTER     TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-BMO-CON-STATUS NOT = LNCH-BMO-CON-STATUS
             MOVE "18 "                   TO WORK-ERRLOG-FIELD
             MOVE "CON STAT"              TO WORK-ERRLOG-DESC
             MOVE LNOR-BMO-CON-STATUS     TO WORK-ERRLOG-ORG-N0
             MOVE LNCH-BMO-CON-STATUS     TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-BMO-REC-STATUS NOT = LNCH-BMO-REC-STATUS
             MOVE "19 "                   TO WORK-ERRLOG-FIELD
             MOVE "REC STAT"              TO WORK-ERRLOG-DESC
             MOVE LNOR-BMO-REC-STATUS     TO WORK-ERRLOG-ORG-N0
             MOVE LNCH-BMO-REC-STATUS     TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-APP-NUMBER NOT = LNCH-APP-NUMBER
             MOVE "20 "                   TO WORK-ERRLOG-FIELD
             MOVE "APPNO   "              TO WORK-ERRLOG-DESC
             MOVE LNOR-APP-NUMBER         TO WORK-ERRLOG-ORG-N0
             MOVE LNCH-APP-NUMBER         TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-PY-YYYYMM-RECDEL NOT = LNCH-PY-YYYYMM-RECDEL
             MOVE "21 "                   TO WORK-ERRLOG-FIELD
             MOVE "RECDEL  "              TO WORK-ERRLOG-DESC
             MOVE LNOR-PY-YYYYMM-RECDEL    TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-PY-YYYYMM-RECDEL    TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-MTD-PAYS-RECDEL NOT = LNCH-MTD-PAYS-RECDEL
             MOVE "22 "                   TO WORK-ERRLOG-FIELD
             MOVE "PYRECDEL"              TO WORK-ERRLOG-DESC
             MOVE LNOR-MTD-PAYS-RECDEL    TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-MTD-PAYS-RECDEL    TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-TRW-STATUS NOT = LNCH-TRW-STATUS
             MOVE "23 "                   TO WORK-ERRLOG-FIELD
             MOVE "TRW STAT"              TO WORK-ERRLOG-DESC
             MOVE LNOR-TRW-STATUS         TO WORK-ERRLOG-ORG-N0
             MOVE LNCH-TRW-STATUS         TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-TRW-SPECIAL-CODE NOT = LNCH-TRW-SPECIAL-CODE
             MOVE "231"                   TO WORK-ERRLOG-FIELD
             MOVE "TRW SPCD"              TO WORK-ERRLOG-DESC
             MOVE LNOR-TRW-SPECIAL-CODE   TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-TRW-SPECIAL-CODE   TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-TRW-COMPLIANCE-CODE NOT = LNCH-TRW-COMPLIANCE-CODE
             MOVE "232"                    TO WORK-ERRLOG-FIELD
             MOVE "TRW COCD"               TO WORK-ERRLOG-DESC
             MOVE LNOR-TRW-COMPLIANCE-CODE TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-TRW-COMPLIANCE-CODE TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-TOT-PARTIAL-CHGOFF NOT = LNCH-TOT-PARTIAL-CHGOFF
             MOVE "24 "                   TO WORK-ERRLOG-FIELD
             MOVE "TOTCHGOF"              TO WORK-ERRLOG-DESC
             MOVE LNOR-TOT-PARTIAL-CHGOFF TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-TOT-PARTIAL-CHGOFF TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

      ***********************************************
       LOG-FIELD-CHANGE SECTION.
      ***********************************************
      * CODE TO WRITE CHANGE FILE.

           MOVE "F" TO ERRLOGW-ACTION.
           MOVE WORK-ERRLOG-DETAIL TO ERRLOGW-DETAIL.
           MOVE FORM-PATHNAME TO ERRLOGW-PROGX.

           MOVE "CL/ERRLOG" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PROGX.

      ***********************************************
       DISPLAY-FIELDS SECTION.
      ***********************************************
           MOVE LN-SUM-LNAMT TO F01.
           MOVE LN-SUM-FINCHG TO F02.
           MOVE LN-SUM-INTCHG TO F03.
           MOVE LN-SUM-SERCHG TO F04.
           MOVE LN-SUM-EXTCHG TO F05.
           MOVE LN-SUM-MAINTFEE TO F06.
           MOVE LN-SUM-ORGTERM TO F07.
           MOVE LN-SUM-ORIGPURAMT TO F08.
           MOVE LN-SUM-REGPYAMT TO F09.
           MOVE LN-INS-CONFIRM-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY         TO F10.
           MOVE LN-MTD-PRIN     TO F11.
           MOVE LN-MTD-LC       TO F12.
           MOVE LN-MTD-INT      TO F13.
           MOVE LN-MTD-OT       TO F14.
           MOVE LN-MTD-EX       TO F15.
           MOVE LN-MONTHEND-BAL TO F16.
           MOVE LN-BNKRPT-CHAPTER    TO F17.
           MOVE LN-BMO-CON-STATUS    TO F18.
           MOVE LN-BMO-REC-STATUS    TO F19.
           MOVE LN-APP-NUMBER        TO F20.
           MOVE LN-PY-YYYYMM-RECDEL   TO DATE-YYYYMM.
           PERFORM CONVERT-YYYYMM-TO-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-MMYY.
           MOVE DATE-MMYY            TO F21.
           MOVE LN-MTD-PAYS-RECDEL TO F22.
           MOVE LN-TRW-STATUS TO F23.
           MOVE LN-TRW-SPECIAL-CODE TO F231.
           MOVE LN-TRW-COMPLIANCE-CODE TO F232.
           MOVE LN-TOT-PARTIAL-CHGOFF TO F24.
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


      ***************************************************
       FORM-RESET SECTION.
      ***************************************************
           MOVE EXT-CONAME-CONAME TO LNMAN9-CONAME.
           MOVE EXT-CONAME-SYSDATE TO LNMAN9-SYSDATE
           DISPLAY LNMAN9-SCREEN UPON LNMAIN-WINDOW-HANDLE.
           MOVE LNMAN9-FORM-FIELDS TO WR-BUF.
           MOVE LNMAN9-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ***************************************************
       SETUP-LINK-INFO SECTION.
      ***************************************************
           MOVE LNMAN9-HELP--FILE TO HELP-LINK-FILE.
           MOVE LNMAN9-HELP--DIR TO HELP-LINK-DIR.

      ***************************************************
       VDU-CONVERT-FIELD SECTION.
      ***************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/LNMAN9_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".

      ****************************************
       MISC-PARAGRAPHS SECTION.
      ****************************************
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
       ACTIVE-RECORD.
           MOVE "ACTIVE CUSTOMER CANNOT BE DELETED" TO MESS.
           PERFORM SEND-MESS.


      ***************************************************
       IO-ROUTINES SECTION.
      ***************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-LN1-FILE.
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLN1IN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      ***************************************************
       END-ROUTINE SECTION.
      ***************************************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY LNMAN9-SCREEN.
           MOVE FORM-PATH TO FORM-PATHNAME.
           MOVE LNMAIN-LINK TO FORM-LINK-AREA.
           EXIT PROGRAM.

