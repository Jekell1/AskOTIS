       IDENTIFICATION DIVISION.
       PROGRAM-ID.      LNMAN6.
      ******************************************************************
      *
      *   DIR :  MN
      *   DESC: LOAN MASTER FILE MAINTENANCE   PAGE 6
      *
      *=================================================================
      * REV:
      * MJD 050895 ADDED CODE TO WRITE CHANGE FILE AFTER REWRITE
      * GLF 082296 ADDED LNSTAT TO BOTTOM OF PAGE.
      * JFF 101696 ADDED #11,12,13 - LN-ERN-IB-PDTH-DATE,LN-ERN-IB-PRINBAL,
      *            LN-ERN-IB-INTOWE.
      * KEC 111496 CHANGED LN-POCD FOR RESCISSION.
      * JTG 071697 CHANGED DISPLAY OF EARNINGS FOR BULK TRANSFERED ACCOUNTS
      * MJD 082197 CHANGED RECORD SIZE FROM 1500 TO 1570
      *            ADD LOGIC FOR PAGE 10.
      * MJD 112197 EXPANDED LN-REC TO 1800 BYTES
      * MJD 072498 ALLOW ENTRY OF ZERO DATE FOR LN-ERN-IB-PDTH-DATE.
      * MJD 082698 ADDED SCREEN DISPLAY OF LN-PRDLDISC-UNEARN.
      * KEC 090398 DISPLAYING LN-REVOLVING FOR 'LNSTAT'.
      * KEC 091598 ADDED LOGIC TO ACCESS NEW PAGE 11.
      * JTG 120198 ADDED LOGIC TO ACCESS NEW PAGE 12.
      * JTG 990326 ADDED LN-LAST-ACCRUAL-DATE, SP-ERNDELQFRMLA = 'C'   TFC #005
      * MJD 990802 ADDED GPENV(W). NEEDED FOR CHANGES TO LPEARN.
      * JTG 000204 ADDED 06A FOR DISPLAY OF 6. OTH : OR 6. COST :
      *                                                         REGENCY #G-2-009
      * CD  010122 ADDED DISPLY OF LN-LMO-PRIOR-INTEREST
      * JTG 040501 ADDED LOGIC TO CAUSE THE UNEARNED LOAN COST TO ROLL TO
      *            THE NEW LOAN AFTER 'RENEWAL'. NOTE: THIS COST IS REFLECTIVE
      *            OF THE DISCOUNT WHEN PURCHASING LOAN PAPER NOT CARRIED AS
      *            SALES PAPER WITH DEALER DISCOUNT.
      *            ADDED SP-LNCOST-FRMLA = 'R'                    REGENCY #2287
      * AMM 051018 ADDED DETAILED, FIELD-BY-FIELD MAINTENANCE LOG IN
      *             /USR/DATA/R1/LG/CHG.YYMMDD
      * MJD 121025 A30 CONVERSION COMPLETE.
      * MJD 121217 ADDED LNMAIN-WINDOW-HANDLE TO SCREEN DISPLAY
      * MJD 130117 CHANGED CALL TMMSG TO PERFORM CALL-TMMSG
      *  CS 160616 REMOVED LN-SEQNO
      *  CS 160711 SETUP LN-OWNBR WILL BE PART OF KEY
      *  CS 170425 ACTIVATE 8 DIGIT DATES
      *
      * KEC 2017.0526 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          REMOVED ORIGINAL-REC AND CHANGED-REC WHICH NOW USING
      *                  LNOR-REC AND LNCH-REC.
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
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)MN/LNMAN6 S34 10/26/21-10:41:20 >".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
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
       01  SUB               PIC 99       COMP-3 VALUE 0.
       01  ELE               PIC 99              VALUE 0.
       01  WORK-AMT          PIC S9(7)V99 COMP-3.
       01  ACTION            VALUE SPACES.
           03  CHKEY         PIC X(4).
           03  CHNO          PIC 9(2).
           03  CHNO-X    REDEFINES CHNO
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
       01  FIRST-SCAN        PIC 9      VALUE 0.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    'PAGE-CHANGE-FLAG' IS FOR KNOWING IF COMING FROM ANOTHER PAGE
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  PAGE-CHANGE-FLAG      PIC X(1)    VALUE "N".
           88  PAGE-CHANGE-NO                VALUE "N".
           88  PAGE-CHANGE-YES               VALUE "Y".


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
      *    *   SCREEN DISPLAY BUFFERS
      *    **********************************
       01  DISPLAY-BUF        VALUE SPACES.
           03  F-ATERM        PIC Z(3).
           03  F-PRDLDSC-UNE  PIC ZZZZZZZ.ZZ-.
           03  DISPLAY-TABLE  OCCURS 6 TIMES.
               05  F          PIC ZZZZZZ.ZZ-.
               05  F1         PIC ZZZZZZ.ZZ-.
               05  F2         PIC ZZZZZZ.ZZ-.
               05  F3         PIC ZZZZZZ.ZZ-.
               05  F4         PIC ZZZZZZ.ZZ-.
               05  F4-ALP     REDEFINES F4
                              PIC X(10).
               05  F5         PIC ZZZZZZ.ZZ.
           03  F7             PIC ZZZZZZ.ZZ-.
           03  F8             PIC X.
           03  F9             PIC ZZZZZZ.ZZ-.
           03  F10            PIC ZZZZZZ.ZZ-.
           03  F11            PIC 99/99/99 BLANK ZERO.
           03  F12            PIC ZZZZZZ.ZZ-.
           03  F13            PIC ZZZZZZ.ZZ-.
           03  F14            PIC 99/99/99 BLANK ZERO.
           03  F15            PIC ZZZZZZ.ZZ-.
           03  FLNSTAT        PIC X(30).

       01  DISPLAY-LIST.
           03  FILLER     PIC X(34) VALUE
               "ATERM D01 010 011 012 013 014 015 ".
           03  FILLER     PIC X(24) VALUE
               "020 021 022 023 024 025 ".
           03  FILLER     PIC X(24) VALUE
               "030 031 032 033 034 035 ".
           03  FILLER     PIC X(24) VALUE
               "040 041 042 043 044 045 ".
           03  FILLER     PIC X(24) VALUE
               "050 051 052 053 054 055 ".
           03  FILLER     PIC X(24) VALUE
               "060 061 062 063 064 065 ".
           03  FILLER     PIC X(43) VALUE
               "070 080 090 100 110 120 130 140 150 LNSTAT.".

       01  DISPLAY-TOTALS-BUF  VALUE SPACES.
           03  T               PIC ZZZZZZ.ZZ-.
           03  T1              PIC ZZZZZZ.ZZ-.
           03  T2              PIC ZZZZZZ.ZZ-.
           03  T3              PIC ZZZZZZ.ZZ-.
           03  T4              PIC ZZZZZZ.ZZ-.
           03  T4-ALP          REDEFINES T4
                               PIC X(10).
           03  T5              PIC ZZZZZZ.ZZ.
       01  DISPLAY-TOTALS-LIST.
           03  FILLER     PIC X(24) VALUE
               "990 991 992 993 994 995.".

      *    **********************************
      *    *   ELEMENT SPECIFICATIONS       *
      *    **********************************
       01  MAX            PIC 99   VALUE 15.
       01  SPEC-TABLE.
           03  FILLER     PIC X(8) VALUE "ENNS062N".
           03  FILLER     PIC X(8) VALUE "ENNS062N".
           03  FILLER     PIC X(8) VALUE "ENNS062N".
           03  FILLER     PIC X(8) VALUE "ENNS062N".
           03  FILLER     PIC X(8) VALUE "ENNS062N".
           03  FILLER     PIC X(8) VALUE "ENNS062N".
           03  FILLER     PIC X(8) VALUE "ENNS062N".
           03  FILLER     PIC X(8) VALUE "E  A010N".
           03  FILLER     PIC X(8) VALUE "ENNS062N".
           03  FILLER     PIC X(8) VALUE "ENNS062N".
           03  FILLER     PIC X(8) VALUE "ERNM060N".
           03  FILLER     PIC X(8) VALUE "ENNS062N".
           03  FILLER     PIC X(8) VALUE "ENNS062N".
           03  FILLER     PIC X(8) VALUE "ERNM060N".
           03  FILLER     PIC X(8) VALUE "ENNS062N".

       01  SPEC-TAB REDEFINES SPEC-TABLE.
           03  SPEC-REC OCCURS 15 TIMES.
               05  SPEC PIC X(7).
               05  CHFG PIC X.

      ***********************************************
       COPY "LIBLP/LPEARNW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/LNMAN6_DEF.CPY".
       COPY "LIBMN/LNMAN6_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBCL/CRNOR2W.CPY".
       COPY "LIBLP/LPPOFFW.CPY".

       COPY "LIBGB/GETFMW.CPY".
       01 FORM-LINK-AREA    PIC X(2000).
       SCREEN SECTION.
       COPY "LIBMN/LNMAN6_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME FORM-LINK-AREA EXIT-PATHNAME.

      *********************************************
       MAIN-PROGRAM SECTION.
      *********************************************
           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "LNMAN6" TO VDU-FORM-NAME.
           MOVE FORM-LINK-AREA TO LNMAIN-LINK.

       MAIN-MODULE.
           MOVE " " TO VDU-PGMENU-SELECTION.
           PERFORM FORM-RESET.

           PERFORM GET-GPENV.

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
              MOVE 7 TO FORM-SEL1.
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

      **********************************************
       REC-LN-MODULE SECTION.
      **********************************************
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
       ENTER-SCAN.
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
              IF FORM-SEL1 < 1 OR FORM-SEL1 = 6 OR FORM-SEL1 > 9
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
           INSPECT ACTION REPLACING ALL SPACES BY ZEROS.
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
           GO TO A-01 A-01 A-01 A-01 A-01
                 A-01 A-07 A-08 A-09 A-10
                 A-11 A-12 A-13 A-14 A-15
                 DEPENDING ON ELE.

      *    **********************************
      *    *      SET RECORD ELEMENTS
      *    **********************************
      * ORIG ANTICIPATED:
       A-01.
           MOVE VDUNUM2 TO LN-ANTICERN(ELE).

      * ANTICIPATED ADJ:
       A-011.
           MOVE "ENNS062  1." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO A-011.
           MOVE VDUNUM2 TO LN-ANTICADJ(ELE).
           SUBTRACT LN-ANTICADJ(ELE) FROM LN-ANTICERN(ELE).

      * NET ANTICIPATED:
           MOVE "SN S062  2." TO VDU.
           MOVE ELE TO VDUROW.
           MOVE LN-ANTICERN(ELE) TO VDUNUM2.
           PERFORM SCREENIO.

      * ACCERN:
       A-013.
           MOVE "ENNS062  3." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO A-011.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO A-013.
           MOVE VDUNUM2 TO LN-ACCERN(ELE).

      * UNEARNED:
           MOVE "SN S062  4." TO VDU.
           MOVE ELE TO VDUROW.
           IF LN-PLCD = " " AND LN-SERVACCT NOT = "Y"
              SUBTRACT LN-ACCERN(ELE) FROM LN-ANTICERN(ELE)
                                               GIVING VDUNUM2
           ELSE
              MOVE 0 TO VDUNUM2.
           PERFORM SCREENIO.

      * UPFRONT EARNINGS:
       A-015.
      * PREPAIRING FOR NEGATIVE ANTIC
      *    IF ELE = 2 OR 6
      *       MOVE "ENNN062  5." TO VDU
      *    ELSE
              MOVE "ENNN062  5." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO A-013.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO A-015.

           ADD LN-ANTICERN(ELE) LN-ANTICADJ(ELE) GIVING WORK-AMT.

           IF WORK-AMT > 0
            IF LN-ANTICADJ(ELE) NOT = 0
             IF VDUNUM2 > WORK-AMT
                MOVE "UPFRONT EARNINGS CAN'T EXCEED ANTICIPATED"
                                                           TO MESS
               PERFORM SEND-MESS
               GO TO A-015.

           IF WORK-AMT > 0
            IF LN-ANTICADJ(ELE) = 0
             IF VDUNUM2 > LN-ACCERN(ELE)
                MOVE "UPFRONT EARNINGS MUST BE INCLUDED IN EARNINGS"
                                                           TO MESS
               PERFORM SEND-MESS
               GO TO A-015.
           MOVE VDUNUM2 TO LN-UFERN(ELE).

           PERFORM DISPLAY-TOTALS.
           GO TO CREATE-ROUTINE.

       A-07.
           MOVE VDUNUM2 TO LN-PRIOR-UNEARNED.
           GO TO CREATE-ROUTINE.

       A-08.
           MOVE VDUBUF TO LN-ERNDELQ-STOP.
           IF NOT (LN-ERNDELQ-STOP = " " OR "Y")
              MOVE "ENTER ' ' OR 'Y'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.

       A-09.
           MOVE VDUNUM2 TO LN-NONACRERN.
           GO TO CREATE-ROUTINE.

       A-10.
           MOVE VDUNUM2 TO LN-PRIOR-NONACRERN.
           GO TO CREATE-ROUTINE.
       A-11.
           MOVE VDU-DATE-YYYYMMDD TO LN-ERN-IB-PDTH-DATE.
           GO TO CREATE-ROUTINE.
       A-12.
           MOVE VDUNUM2 TO LN-ERN-IB-PRINBAL.
           GO TO CREATE-ROUTINE.
       A-13.
           MOVE VDUNUM2 TO LN-ERN-IB-INTOWE.
           GO TO CREATE-ROUTINE.
       A-14.
           MOVE VDU-DATE-YYYYMMDD TO LN-LAST-ACCRUAL-DATE.
           GO TO CREATE-ROUTINE.

       A-15.
           MOVE VDUNUM2 TO LN-LMO-PRIOR-INTEREST.
           GO TO CREATE-ROUTINE.

       END-ENTRY-MODULE.
           MOVE 0 TO FORM-SEL1 FORM-SEL2.
       EXIT-ENTRY-MODULE.
           EXIT.

      ********************************************
       WRITE-MODULE SECTION.
      ********************************************
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

      ********************************************
       LOG-MAINTENANCE-CHGS SECTION.
      ********************************************

           MOVE SPACES    TO WORK-ERRLOG-DETAIL.
           MOVE LN-OWNBR  TO WORK-ERRLOG-BR.
           MOVE LN-ACCTNO TO WORK-ERRLOG-NUMBER.
           MOVE "TO "     TO  WORK-ERRLOG-TO-X.

           IF LNOR-ACCERN(1) NOT = LNCH-ACCERN(1)
             MOVE "01 "              TO WORK-ERRLOG-FIELD
             MOVE "EARNINGS1"        TO WORK-ERRLOG-DESC
             MOVE LNOR-ACCERN(1)     TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-ACCERN(1)     TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.
           IF LNOR-ANTICERN(1) NOT = LNCH-ANTICERN(1)
             MOVE "01 "              TO WORK-ERRLOG-FIELD
             MOVE "ANTIC1"        TO WORK-ERRLOG-DESC
             MOVE LNOR-ANTICERN(1)     TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-ANTICERN(1)     TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.
           IF LNOR-UFERN(1) NOT = LNCH-UFERN(1)
             MOVE "01 "              TO WORK-ERRLOG-FIELD
             MOVE "UFERN1"           TO WORK-ERRLOG-DESC
             MOVE LNOR-UFERN(1)     TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-UFERN(1)     TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.
           IF LNOR-ACCERN(2) NOT = LNCH-ACCERN(2)
             MOVE "02 "              TO WORK-ERRLOG-FIELD
             MOVE "EARNINGS2"        TO WORK-ERRLOG-DESC
             MOVE LNOR-ACCERN(2)     TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-ACCERN(2)     TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.
           IF LNOR-ANTICERN(2) NOT = LNCH-ANTICERN(2)
             MOVE "02 "              TO WORK-ERRLOG-FIELD
             MOVE "ANTIC2"        TO WORK-ERRLOG-DESC
             MOVE LNOR-ANTICERN(2)     TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-ANTICERN(2)     TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.
           IF LNOR-UFERN(2) NOT = LNCH-UFERN(2)
             MOVE "02 "              TO WORK-ERRLOG-FIELD
             MOVE "UFERN2"           TO WORK-ERRLOG-DESC
             MOVE LNOR-UFERN(2)     TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-UFERN(2)     TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.
           IF LNOR-ACCERN(3) NOT = LNCH-ACCERN(3)
             MOVE "03 "              TO WORK-ERRLOG-FIELD
             MOVE "EARNINGS3"        TO WORK-ERRLOG-DESC
             MOVE LNOR-ACCERN(3)     TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-ACCERN(3)     TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.
           IF LNOR-ANTICERN(3) NOT = LNCH-ANTICERN(3)
             MOVE "03 "              TO WORK-ERRLOG-FIELD
             MOVE "ANTIC3"        TO WORK-ERRLOG-DESC
             MOVE LNOR-ANTICERN(3)     TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-ANTICERN(3)     TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.
           IF LNOR-UFERN(3) NOT = LNCH-UFERN(3)
             MOVE "03 "              TO WORK-ERRLOG-FIELD
             MOVE "UFERN3"           TO WORK-ERRLOG-DESC
             MOVE LNOR-UFERN(3)     TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-UFERN(3)     TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.
           IF LNOR-ACCERN(4) NOT = LNCH-ACCERN(4)
             MOVE "04 "              TO WORK-ERRLOG-FIELD
             MOVE "EARNINGS4"        TO WORK-ERRLOG-DESC
             MOVE LNOR-ACCERN(4)     TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-ACCERN(4)     TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.
           IF LNOR-ANTICERN(4) NOT = LNCH-ANTICERN(4)
             MOVE "04 "              TO WORK-ERRLOG-FIELD
             MOVE "ANTIC4"        TO WORK-ERRLOG-DESC
             MOVE LNOR-ANTICERN(4)     TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-ANTICERN(4)     TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.
           IF LNOR-UFERN(4) NOT = LNCH-UFERN(4)
             MOVE "04 "              TO WORK-ERRLOG-FIELD
             MOVE "UFERN4"           TO WORK-ERRLOG-DESC
             MOVE LNOR-UFERN(4)     TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-UFERN(4)     TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.
           IF LNOR-ACCERN(5) NOT = LNCH-ACCERN(5)
             MOVE "05 "              TO WORK-ERRLOG-FIELD
             MOVE "EARNINGS5"        TO WORK-ERRLOG-DESC
             MOVE LNOR-ACCERN(5)     TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-ACCERN(5)     TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.
           IF LNOR-ANTICERN(5) NOT = LNCH-ANTICERN(5)
             MOVE "05 "              TO WORK-ERRLOG-FIELD
             MOVE "ANTIC4"        TO WORK-ERRLOG-DESC
             MOVE LNOR-ANTICERN(4)     TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-ANTICERN(4)     TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.
           IF LNOR-UFERN(5) NOT = LNCH-UFERN(5)
             MOVE "05 "              TO WORK-ERRLOG-FIELD
             MOVE "UFERN5"           TO WORK-ERRLOG-DESC
             MOVE LNOR-UFERN(5)     TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-UFERN(5)     TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.
           IF LNOR-ACCERN(6) NOT = LNCH-ACCERN(6)
             MOVE "06 "              TO WORK-ERRLOG-FIELD
             MOVE "EARNINGS6"        TO WORK-ERRLOG-DESC
             MOVE LNOR-ACCERN(6)     TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-ACCERN(6)     TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.
           IF LNOR-ANTICERN(6) NOT = LNCH-ANTICERN(6)
             MOVE "06 "              TO WORK-ERRLOG-FIELD
             MOVE "ANTIC4"        TO WORK-ERRLOG-DESC
             MOVE LNOR-ANTICERN(4)     TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-ANTICERN(4)     TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.
           IF LNOR-UFERN(6) NOT = LNCH-UFERN(6)
             MOVE "06 "              TO WORK-ERRLOG-FIELD
             MOVE "UFERN6"           TO WORK-ERRLOG-DESC
             MOVE LNOR-UFERN(6)     TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-UFERN(6)     TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-PRIOR-UNEARNED NOT = LNCH-PRIOR-UNEARNED
             MOVE "07 "                   TO WORK-ERRLOG-FIELD
             MOVE "UNEARNED"              TO WORK-ERRLOG-DESC
             MOVE LNOR-PRIOR-UNEARNED     TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-PRIOR-UNEARNED     TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-ERNDELQ-STOP NOT = LNCH-ERNDELQ-STOP
             MOVE "08 "                   TO WORK-ERRLOG-FIELD
             MOVE "ERNDELQ "              TO WORK-ERRLOG-DESC
             MOVE LNOR-ERNDELQ-STOP       TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-ERNDELQ-STOP       TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-NONACRERN NOT = LNCH-NONACRERN
             MOVE "09 "                   TO WORK-ERRLOG-FIELD
             MOVE "NOACRERN"              TO WORK-ERRLOG-DESC
             MOVE LNOR-NONACRERN          TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-NONACRERN          TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-PRIOR-NONACRERN NOT = LNCH-PRIOR-NONACRERN
             MOVE "10 "                   TO WORK-ERRLOG-FIELD
             MOVE "PRIORACR"              TO WORK-ERRLOG-DESC
             MOVE LNOR-PRIOR-NONACRERN    TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-PRIOR-NONACRERN    TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-ERN-IB-PDTH-DATE NOT = LNCH-ERN-IB-PDTH-DATE
             MOVE "11 "                   TO WORK-ERRLOG-FIELD
             MOVE "ERNIBPDT"              TO WORK-ERRLOG-DESC
             MOVE LNOR-ERN-IB-PDTH-DATE   TO DATE-YYYYMMDD       
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-ERN-IB-PDTH-DATE   TO DATE-YYYYMMDD       
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-ERN-IB-PRINBAL NOT = LNCH-ERN-IB-PRINBAL
             MOVE "12 "                   TO WORK-ERRLOG-FIELD
             MOVE "IBPRINBA"              TO WORK-ERRLOG-DESC
             MOVE LNOR-ERN-IB-PRINBAL     TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-ERN-IB-PRINBAL     TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-ERN-IB-INTOWE NOT = LNCH-ERN-IB-INTOWE
             MOVE "13 "                   TO WORK-ERRLOG-FIELD
             MOVE "IBINTOWE"              TO WORK-ERRLOG-DESC
             MOVE LNOR-ERN-IB-INTOWE      TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-ERN-IB-INTOWE      TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-LAST-ACCRUAL-DATE NOT = LNCH-LAST-ACCRUAL-DATE
             MOVE "14 "                   TO WORK-ERRLOG-FIELD
             MOVE "ACCRDATE"              TO WORK-ERRLOG-DESC
             MOVE LNOR-LAST-ACCRUAL-DATE  TO DATE-YYYYMMDD        
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO WORK-ERRLOG-ORG-ALP
             MOVE LNCH-LAST-ACCRUAL-DATE  TO DATE-YYYYMMDD            
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LNOR-LMO-PRIOR-INTEREST NOT = LNCH-LMO-PRIOR-INTEREST
             MOVE "15 "                   TO WORK-ERRLOG-FIELD
             MOVE "LMO INT "              TO WORK-ERRLOG-DESC
             MOVE LNOR-LMO-PRIOR-INTEREST TO WORK-ERRLOG-ORG-N2
             MOVE LNCH-LMO-PRIOR-INTEREST TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

      **************************************************
       LOG-FIELD-CHANGE SECTION.
      **************************************************
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
      **************************************************
       DISPLAY-FIELDS SECTION.
      **************************************************
           MOVE 0 TO SUB.
           PERFORM DISPLAY-FIELDS-COMPONENTS 6 TIMES.
           MOVE LN-PRIOR-UNEARNED TO F7.
           MOVE LN-ERNDELQ-STOP TO F8.
           MOVE LN-NONACRERN TO F9.
           MOVE LN-PRIOR-NONACRERN TO F10.
           MOVE LN-ERN-IB-PDTH-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY        TO  F11.
           MOVE LN-ERN-IB-PRINBAL TO F12.
           MOVE LN-ERN-IB-INTOWE TO F13.
           MOVE LN-LAST-ACCRUAL-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY          TO F14.
           MOVE LN-LMO-PRIOR-INTEREST TO F15.
           MOVE LN-ANTICTERM TO F-ATERM.
           MOVE LN-ORGBR TO WS-ORGBR.

           STRING LN-ORGST, "-", WS-ORGBR, "-", LN-SPRCLASS, "(",
                  LN-CLASS, ")", "-", LN-SUBCLASS, "-", LN-LAWCODE,
                  "-", LN-SMALLOAN, LN-SALEFIN, LN-SERVACCT, LN-LOANTYPE
                  DELIMITED BY "  " INTO FLNSTAT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * DISPLAY PRIOR DEALER DISCOUNT UNEARNED FOR REFERENCE ONLY.
      *  ACTUAL INPUT IS ON PAGE 10.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  JTG 040501 ADDED LOGIC TO CAUSE THE UNEARNED LOAN COST TO ROLL TO
      *             THE NEW LOAN AFTER 'RENEWAL'. NOTE: THIS COST IS REFLECTIVE
      *             OF THE DISCOUNT WHEN PURCHASING LOAN PAPER NOT CARRIED AS
      *             SALES PAPER WITH DEALER DISCOUNT.
      *             ADDED SP-LNCOST-FRMLA = 'R'                    REGENCY #2287
      *       NOTE: LN-PRDLDISC-UNEARN WILL HOLD THE COST (DISCOUNT)
      *             FOR DIRECT LOANS, EVEN THOUGH IT IS NOT TIED TO A DEALER.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE LN-PRDLDISC-UNEARN TO F-PRDLDSC-UNE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST
           PERFORM CALL-WRFORM.

           PERFORM DISPLAY-TOTALS.
           GO TO DISPLAY-FIELDS-EXIT.

       DISPLAY-FIELDS-COMPONENTS.
           ADD 1 TO SUB.
           IF LN-POCD NOT = "BT"
              ADD LN-ANTICADJ(SUB) LN-ANTICERN(SUB) GIVING F(SUB).
           MOVE LN-ANTICADJ(SUB) TO F1(SUB).
           MOVE LN-ANTICERN(SUB) TO F2(SUB).
           MOVE LN-ACCERN(SUB) TO F3(SUB).
           IF LN-PLCD = " " AND LN-SERVACCT NOT = "Y"
              SUBTRACT LN-ACCERN(SUB) FROM LN-ANTICERN(SUB)
                                              GIVING F4(SUB)
              IF F4-ALP(SUB) NOT = " "
                 PERFORM DISPLAY-FIELDS-COMPONENTS-RB
              ELSE
                 NEXT SENTENCE
           ELSE
              MOVE 0 TO F4(SUB).
           MOVE LN-UFERN(SUB) TO F5(SUB).

       DISPLAY-FIELDS-COMPONENTS-RB.
      * SKIP REBATED COMPONENTS:
           IF SUB = 1 AND
              ( LN-ANTICADJ(1) NOT = 0
                      OR (NOT (LN-REBATE(4) = " " OR "C")) )
              IF LN-POCD = "BT"
                 MOVE "    BULKTR" TO F4-ALP(1)
              ELSE
                 MOVE "  REBATED?" TO F4-ALP(1)
           ELSE
           IF SUB = 3 AND
              ( LN-ANTICADJ(3) NOT = 0
                      OR (NOT (LN-REBATE(5) = " " OR "C")) )
              IF LN-POCD = "BT"
                 MOVE "    BULKTR" TO F4-ALP(3)
              ELSE
                 MOVE "  REBATED?" TO F4-ALP(3)
           ELSE
           IF SUB = 4 AND
              ( LN-ANTICADJ(4) NOT = 0
                      OR (NOT (LN-REBATE(6) = " " OR "C")) )
              IF LN-POCD = "BT"
                 MOVE "    BULKTR" TO F4-ALP(4)
              ELSE
                 MOVE "  REBATED?" TO F4-ALP(4).

       DISPLAY-FIELDS-EXIT.
           EXIT.

       COPY "LIBLP/LPEARN.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".

      **********************************************************
       FORM-RESET SECTION.
      **********************************************************
           MOVE EXT-CONAME-CONAME TO LNMAN6-CONAME.
           MOVE EXT-CONAME-SYSDATE TO LNMAN6-SYSDATE.
           MOVE EXT-CONAME-DAILY-PASSWORD TO CHAN.
           DISPLAY LNMAN6-SCREEN UPON LNMAIN-WINDOW-HANDLE.
           MOVE LNMAN6-FORM-FIELDS TO WR-BUF.
           MOVE LNMAN6-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **********************************************************
       SETUP-LINK-INFO SECTION.
      **********************************************************
           MOVE LNMAN6-HELP--FILE TO HELP-LINK-FILE.
           MOVE LNMAN6-HELP--DIR TO HELP-LINK-DIR.

      **********************************************************
       VDU-CONVERT-FIELD SECTION.
      **********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/LNMAN6_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      **********************************************************
       MISC-PARAGRAPHS SECTION.
      **********************************************************
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/ACCESS.CPY".
       DISPLAY-TOTALS.
           PERFORM COMPUTE-EARNINGS.
           IF GP-INCL-OTH-IN-EARNED = "Y"
              MOVE "S  A08006A:S." TO VDU
              MOVE "6. OTH :" TO VDUBUF
           ELSE
              MOVE "S  A08006A." TO VDU
              MOVE "6. COST:" TO VDUBUF
           END-IF
           PERFORM SCREENIO.

           ADD EARN-TOT-ANTICADJ EARN-TOT-ANTICERN GIVING T.
           MOVE EARN-TOT-ANTICADJ TO T1.
           MOVE EARN-TOT-ANTICERN TO T2.
           MOVE EARN-TOT-ACCERN TO T3.
           MOVE EARN-TOT-UNEARN TO T4.
           IF LN-PLCD NOT = " "
              MOVE "NONE (PL)" TO T4-ALP.
           IF LN-SERVACCT = "Y"
              MOVE "NONE (SA)" TO T4-ALP.
           MOVE EARN-TOT-UFERN TO T5.

           MOVE DISPLAY-TOTALS-LIST TO WR-LIST.
           MOVE DISPLAY-TOTALS-BUF TO WR-BUF.
           PERFORM CALL-WRFORM.
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
           MOVE
           "THIS RECORD HAS UNDERGONE CHANGE BY ANOTHER USER,"
           TO MESS.
           PERFORM SEND-MESS.

      **************************************************
       IO-ROUTINES SECTION.
      **************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-NO1-FILE.
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPNOGS_SQL.CPY".
       COPY "LIBLP/LPLN1IN.CPY".
       COPY "LIBLP/LPNO1IN.CPY".

       COPY "LIBGB/FERRORS.CPY".

      **************************************************
       END-ROUTINE SECTION.
      **************************************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY LNMAN6-SCREEN.
           MOVE FORM-PATH TO FORM-PATHNAME.
           MOVE LNMAIN-LINK TO FORM-LINK-AREA.
           EXIT PROGRAM.
