       IDENTIFICATION DIVISION.
       PROGRAM-ID.      LONPH0.
       DATE-WRITTEN.    012484.
      *****************************************************************
      *                 (LP)
      *  DESCRIPTION:   TRANSACTION RECAP INQUIRY
      *
      * REV:
      *  MJD 081495 ADDED LOGIC FOR GP-THE-ONLY-SF-SRCD.
      *  MJD 020296 CHANGED PLA & PLI. PLA NOW REPRESENTS PL-P5 ACTIVE.
      *             PLA NOW REPRESENTS PL-P5 ACTIVE. P&L RECOVERIES NOW
      *             INCLUDES PRINCOL, LATECOL, INTCOL, AND O2COL FOR PL-P5.
      *  MJD 060597 ADDED O3 REBATED
      *  GLF 090897 REPLACED 'IO' ON GP-FILE WITH GET 'GPENV'
      *  BAH 030298 ADDED CKWRITTEN(7) INTERIM DEPOSITS TO THE SCREEN
      *             REMOVED ALL REFERENCES TO TT FILE
      *  MJD 050598 ADDED LOGIC FOR "RB" AND "RO" TRANSACTION/SOURCE CODES.
      *  BAH 062698 TILLNO IS NOW ALPHANUMERIC
      *  JTG 990611 ADDED LOGIC TO READ POSTING BRANCH RECORD FOR REVOLVING
      *  MJD 000929 ADDED TY-LOCKBOX (MFC PR#237)
      *  BLF 050217 DISPLAY CA-NET-CASH-NOTES IN CASH DRAWER SECTION & INCLUDE
      *             IN DRAWER CLOSING CALCULATION, MULLEN PL# 466
      *   CS 121220 ADDED TCLP-MAIN-WINDOW-HANDLE
      *  BAH 140814 ADDED TEST FOR LPSMNU IN HELP
      *  BAH 160307 CHANGED TILLNO TO 9(2) INSTEAD OF PIC X, PD0003
      *  BLM 160310 FIX FOR NEW TY DATE FORMAT, PD0003
      *  BAH 161104 FIXED TEST OF TY-DATE TO DATE-YYYYMMDD, NOT DATE-YYMMDD
      *             WAS NOT FINDING ANY RECORDS
      * BAH 170629 ACTIVATE 8 DIGIT DATES, PD0006
      *  CS 190815 FORCED BRANCE NUMBER TO BR-NO
      *            ALSO, ADDED A DESTROY LONPH-SCREEN ON F7, THE DAILY
      *            PROCESSING MENU WAS DISPLAYED OVER THE LONPH SCREEN
      * BAH 2022.0314 REMOVED EXT-SPANISH-FORMS AND ALL MX FORMS
      * BAH 2022.0712 CHANGED DRAWER ENTRY TO ENNN020 SO IT DOESNT RESET
      *               AND WIPE OUT THE DRAWER #
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


       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LP/LONPH0.CBL S34 05/24/21-12:15:19 >".
      **********************************
      *       F I L E   I F N          *
      **********************************
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01LC.CPY".
       COPY "LIBLP/LP01LC_SQL.CPY".
       COPY "LIBLP/LPWSLC.CPY".
       COPY "LIBLP/LP01CA.CPY".
       COPY "LIBLP/LP01CA_SQL.CPY".
       COPY "LIBLP/LPWSCA.CPY".
       COPY "LIBLP/LP01TY.CPY".
       COPY "LIBLP/LP01TY_SQL.CPY".
       COPY "LIBLP/LPWSTY.CPY".

       01  SUB             PIC 9(6) COMP-3.
       01  SUB4            PIC 9(6) COMP-3.
       01  ACT             PIC 9(6) COMP-3.
       01  ACTX            PIC 9(6) COMP-3.
       01  RECORDS-EXIST   PIC X VALUE "N".
       01  MSEL-LIST       PIC X(5) VALUE "MSEL.".

       01  HOLD-BRANCH1    PIC 9(4) VALUE 0.
       01  HOLD-LOAN-TYPE  PIC 9(3) VALUE 0.
       01  HOLD-BRANCH2    PIC 9(4) VALUE 0.
       01  HOLD-TILL-NO    PIC 99   VALUE 0.

      *----------------------------------------------------------------
      * WORKERS FOR ACCOM-PL-TOTALS SECTION.
      *----------------------------------------------------------------
       01  PL-TYPE              PIC 9(6) COMP.
       01  PL-ACTY              PIC 9(6) COMP.
       01  PL-SUB3              PIC 9(6) COMP.
       01  TOTAL-NO-PL          PIC S9(9)V99.
       01  TOTAL-AMT-PL         PIC S9(9)V99.
       01  TOTAL-O2-PL          PIC S9(9)V99.
       01  TOTAL-INT-PL         PIC S9(9)V99.

       01  TOTAL-PLA-RECV       PIC S9(9)V99 COMP.


       01  W-PL-NUMBERS.
           03  W-PL-TBL        OCCURS 2.
               05  W-ACT-TBLP  OCCURS 2.
                   07  W-NOP   PIC S9(5)     COMP-3.
                   07  W-AMTP  PIC S9(9)V99  COMP-3.
               05  W-RECP      PIC S9(9)V99  COMP-3.

       01  WY-NOR-NUMBERS.
           03  WY-NOR-TBL       OCCURS 8.
               05  WY-ACT-TBLN  OCCURS 2.
                   07  WY-NON   PIC S9(5)     COMP-3.
                   07  WY-AMTN  PIC S9(9)V99  COMP-3.
       01  W-COLL.
           03  W-COLAMT        OCCURS 5
                               PIC S9(9)V99  COMP-3.
           03  W-RNPRIN        PIC S9(9)V99  COMP-3.
           03  W-REBAMT        OCCURS 10
                               PIC S9(9)V99  COMP-3.
       01  W-TILLS.
           03  W-T-AMT         OCCURS 6
                               PIC S9(9)V99 COMP-3.
       01  W-CHECKS.
           03  W-C-AMT         OCCURS 7
                               PIC S9(9)V99 COMP-3.
       01  W-LBOX.
           03  W-C-LBOX        PIC S9(7)V99  COMP-3.

       01  W-ALLOT.
           03  W-C-ALLOT        PIC S9(7)V99  COMP-3.

      **********************************
      *  TODAY'S TOTALS DISPLAY BUFFER *
      **********************************
       01  DISPLAY-BUF1        VALUE " ".
           03  D-BR1           PIC 9(4).
           03  D-BR1-NAME      PIC X(20).
           03  D-LOAN-TYPE     PIC 99.
           03  D-LOAN-DESC     PIC X(20).
           03  D-PL-TBL        OCCURS 2.
               05  ACT-TBLP     OCCURS 2.
                   07  D-NOP   PIC ZZZ9-      BLANK ZERO.
                   07  D-AMTP  PIC ZZZZZ9.99- BLANK ZERO.
               05  D-RECP      PIC ZZZZZ9.99- BLANK ZERO.
           03  D-NOR-TBL       OCCURS 8.
               05  D-PX        PIC X(3).
               05  ACT-TBLN     OCCURS 2.
                   07  D-NON   PIC ZZZZ9-       BLANK ZERO.
                   07  D-AMTN  PIC ZZZZZZZ9.99- BLANK ZERO.
           03  D-COLAMT        OCCURS 5
                                PIC ZZZZZZZ9.99- BLANK ZERO.
           03  D-RNPRIN        PIC ZZZZZZZ9.99- BLANK ZERO.
           03  D-REBAMT        OCCURS 10
                                PIC ZZZZZZZ9.99- BLANK ZERO.
           03  D-CHECKS        OCCURS 7.
               05  D-C-AMT     PIC ZZZZZZZ9.99- BLANK ZERO.
           03  D-ALLOT         PIC ZZZZZZZ9.99- BLANK ZERO.
           03  D-LBOX          PIC ZZZZZZZ9.99- BLANK ZERO.

       01  DISPLAY-BUF2         REDEFINES DISPLAY-BUF1.
           03  D-BR2            PIC 9(4).
           03  D-TILL-NO        PIC 99.
           03  D-TILLS          OCCURS 6.
               05  D-T-AMT      PIC Z,ZZZ,ZZ9.99-.

       01  DISPLAY-LIST1.
           03  FILLER      PIC X(47) VALUE
           "BR1 BRX LT LTX PLA PLI P1:S A1 P2:S A2 P3:S A3 ".
           03  FILLER      PIC X(41) VALUE
           "P4:S A4 P5:S A5 P6:S A6 P7:S A7 P8:S TOT ".
           03  FILLER      PIC X(37) VALUE
           "COTH CINT CLC CDEF CPRIN RP CL AH PP ".
           03  FILLER      PIC X(27) VALUE
           "OT1 OT2 OT3 INT SCH MFE DF ".
           03  FILLER       PIC X(27) VALUE
           "C1 C2 C3 C4 C5 C6 C7 AL LB.".

       01  DISPLAY-LIST2.
           03  FILLER      PIC X(28) VALUE
           "BR2 TL T1 T2 NOTES T3 T4 T5.".

       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBLP/LPMENUW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBLP/LPSETTYW.CPY".

       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".

       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/LONPH_DEF.CPY".
       COPY "LIBLP/LPSMNU_DEF.CPY".
       COPY "LIBLP/LPLNMU_DEF.CPY".
       COPY "LIBLP/LPCAMU_DEF.CPY".
       COPY "LIBLP/LPVDMU_DEF.CPY".

       COPY "LIBLP/LONPH_WKS.CPY".
       COPY "LIBLP/LPSMNU_WKS.CPY".
       COPY "LIBLP/LPLNMU_WKS.CPY".
       COPY "LIBLP/LPCAMU_WKS.CPY".
       COPY "LIBLP/LPVDMU_WKS.CPY".

       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBLP/LPLOANW.CPY".
       COPY "LIBLP/LPPFBUF.CPY".

       SCREEN SECTION.
       COPY "LIBLP/LONPH_SCN.CPY".
       COPY "LIBLP/LPSMNU_SCN.CPY".
       COPY "LIBLP/LPLNMU_SCN.CPY".
       COPY "LIBLP/LPCAMU_SCN.CPY".
       COPY "LIBLP/LPVDMU_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME PROG-BUF UPDATE-BUF.

      ***********************************
      *   C O N T R O L   M O D U L E   *
      ***********************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "LONPH"       TO VDU-FORM-NAME.

           PERFORM FORM-RESET.

           PERFORM OPEN-LC1-FILE.
           PERFORM OPEN-BR-FILE.

           MOVE BRANCH TO BR-NO.
           PERFORM READ-BR-FILE.

           PERFORM GET-GPENV.

       ENTER-BRANCH1.

      *A15 ONLY ALLOWS YOU ACCESS TO YOUR BRANCH SO FORCE THE BRANCH
           MOVE "SNNN040BR1." TO VDU.
           MOVE BR-NO TO VDUNUM0.                  
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY LONPH-SCREEN
              PERFORM MENU-SELECTION
              IF MENU-FG = 99
                 GO TO ENTER-BRANCH1
              ELSE
                 GO TO END-ROUTINE.

           IF VDUSTATUS = "1:"
              GO TO ENTER-BRANCH2.
           IF VDUSTATUS NOT = "00"
                     AND NOT = "16"
              GO TO ENTER-BRANCH1.
           IF VDUNUM0 = 0
              IF VDUSTATUS NOT = "16"
                 GO TO ENTER-BRANCH1.

           MOVE VDUNUM0 TO HOLD-BRANCH1.
       ENTER-LOAN-TYPE.
           MOVE "LOAN CLASS,F3-ALL,F5-DRAWER,F7-EXIT" TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "EN N020LT." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN 
              DESTROY LONPH-SCREEN
              PERFORM MENU-SELECTION
              IF MENU-FG = 99
                 GO TO ENTER-LOAN-TYPE
              ELSE
                 GO TO END-ROUTINE.
           IF VDUSTATUS = "1U"
              GO TO ENTER-BRANCH1.
           IF VDUSTATUS = "1:"
              GO TO ENTER-BRANCH2.
           IF VDUSTATUS NOT = "00"
                     AND NOT = "16"
              GO TO ENTER-LOAN-TYPE.
           IF VDUNUM0 = 0
              IF VDUSTATUS NOT = "16"
                 GO TO ENTER-LOAN-TYPE.
           MOVE VDUNUM0 TO HOLD-LOAN-TYPE.

           PERFORM TY-ROUTINE.

           GO TO ENTER-BRANCH1.

       ENTER-BRANCH2.

           MOVE "SNNN040BR2." TO VDU.
           MOVE BR-NO TO VDUNUM0.                  
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY LONPH-SCREEN
              PERFORM MENU-SELECTION
              IF MENU-FG = 99
                 GO TO ENTER-BRANCH2
              ELSE
                 GO TO END-ROUTINE.
           IF VDUSTATUS = "1:"
              GO TO ENTER-BRANCH1.
           IF VDUSTATUS NOT = "00"
                     AND NOT = "16"
              GO TO ENTER-BRANCH2.
           IF VDUNUM0 = 0
              IF VDUSTATUS NOT = "16"
                 GO TO ENTER-BRANCH2.
           MOVE VDUNUM0 TO HOLD-BRANCH2.
       ENTER-TILL-NO.
           MOVE "DRAWER,F3-ALL,F5-LOANS,F7-EXIT" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENNN020TL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY LONPH-SCREEN
              PERFORM MENU-SELECTION
              IF MENU-FG = 99
                 GO TO ENTER-TILL-NO
              ELSE
                 GO TO END-ROUTINE.
           IF VDUSTATUS = "1U"
              GO TO ENTER-BRANCH2.
           IF VDUSTATUS = "1:"
              GO TO ENTER-BRANCH1.
           IF VDUSTATUS NOT = "00"
                     AND NOT = "16"
              GO TO ENTER-TILL-NO.
           IF VDUNUM0 = 0
              IF VDUSTATUS NOT = "16"
                 GO TO ENTER-TILL-NO.
           MOVE VDUNUM0 TO HOLD-TILL-NO.

           PERFORM TILL-ROUTINE.

           GO TO ENTER-BRANCH2.

      *********************************
       TY-ROUTINE SECTION.
      *********************************

           MOVE " " TO MESS.
           PERFORM SEND-LEGEND.

           MOVE SPACES TO DISPLAY-BUF1.

           PERFORM OPEN-TY1-FILE.

           MOVE HOLD-BRANCH1   TO BR-NO
                                  TY-BRNO
                                  D-BR1
                                  QTY1-WBEG-BRNO
                                  QTY1-WEND-BRNO.
       
           MOVE TRANS-DATE     TO TY-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTY1-WBEG-DATE
                                       QTY1-WEND-DATE.

      * START TY-FILE AT CLASS 0 TO CATCH ALLOTMENTS

           MOVE 0              TO TY-CLASS
                                  QTY1-WBEG-CLASS.
           MOVE ALL "9"        TO QTY1-WEND-CLASS.

           MOVE HOLD-LOAN-TYPE TO D-LOAN-TYPE
                                  LC-CODE.

           PERFORM START-TY1-FILE.

           PERFORM CLEAR-TY-TOTALS.

           MOVE "A L L" TO BR-NAME.
           IF HOLD-BRANCH1 NOT = 0
              PERFORM READ-BR-FILE
              IF IO-FG = 9
                 MOVE "INVALID" TO BR-NAME.
           MOVE BR-NAME TO D-BR1-NAME.

           MOVE "A L L" TO LC-DESC.
           IF HOLD-LOAN-TYPE NOT = 0
              PERFORM READ-LC1-FILE
              IF IO-FG = 9
                 MOVE "INVALID" TO LC-DESC.
           MOVE LC-DESC TO D-LOAN-DESC.

           MOVE "N" TO RECORDS-EXIST.
       GET-TY-REC.
           PERFORM READ-TY1-FILE-NEXT.
           IF IO-FG = 9 OR (TY-PATH-OWNBR NOT = TY-BRNO)
              GO TO END-TY-ROUTINE.

           IF HOLD-BRANCH1 NOT = 0
              IF HOLD-BRANCH1 NOT = TY-BRNO
                 GO TO GET-TY-REC.

           IF TRANS-DATE NOT = TY-DATE
              GO TO GET-TY-REC.

           IF HOLD-LOAN-TYPE NOT = 0
              IF TY-CLASS NOT = HOLD-LOAN-TYPE
                 IF TY-CLASS NOT = 0
                    GO TO GET-TY-REC.
           MOVE "Y" TO RECORDS-EXIST.
           PERFORM ACCUM-TY-TOTALS.
           PERFORM ACCUM-TY-CHECKS.
           GO TO GET-TY-REC.

       END-TY-ROUTINE.
           IF RECORDS-EXIST = "N"
              MOVE "NO RECORDS FOR SELECTION" TO MESS
              PERFORM SEND-MESS
              GO TO EXIT-TY-ROUTINE.

           PERFORM SEND-TY-TOTALS.
       EXIT-TY-ROUTINE.
           PERFORM CLOSE-TY1-FILE.

      *********************************
       ACCUM-TY-TOTALS SECTION.
      *********************************
           MOVE 1 TO PL-TYPE.
           MOVE 1 TO PL-ACTY.
      *  TY PLA  NET GAIN / LOSS
           PERFORM ACCUM-PL-TOTALS.
           ADD TOTAL-NO-PL TO W-NOP(1 1).
           ADD TOTAL-AMT-PL TO W-AMTP(1 1).
           MOVE 2 TO PL-ACTY.
      *  TY PLA  NEW VOLUME
           PERFORM ACCUM-PL-TOTALS.
           ADD TOTAL-NO-PL TO W-NOP(1 2).
           ADD TOTAL-AMT-PL TO W-AMTP(1 2).
           MOVE 2 TO PL-TYPE.
           MOVE 1 TO PL-ACTY.
      *  TY PLI  NET GAIN / LOSS
           PERFORM ACCUM-PL-TOTALS.
           ADD TOTAL-NO-PL TO W-NOP(2 1).
           ADD TOTAL-AMT-PL TO W-AMTP(2 1).
           MOVE 2 TO PL-ACTY.
      *  TY PLI   NEW VOLUME
           PERFORM ACCUM-PL-TOTALS.
           ADD TOTAL-NO-PL TO W-NOP(2 2).
           ADD TOTAL-AMT-PL TO W-AMTP(2 2).

           MOVE 0 TO TOTAL-PLA-RECV.
           PERFORM VARYING SUB4 FROM 1 BY 1 UNTIL SUB4 > 5
             ADD TY-PL-PRINCOL(SUB4) TO TOTAL-PLA-RECV
             ADD TY-PL-INTCOL(SUB4) TO TOTAL-PLA-RECV
             ADD TY-PL-LATECOL(SUB4) TO TOTAL-PLA-RECV
             ADD TY-PL-O2COL(SUB4) TO TOTAL-PLA-RECV
           END-PERFORM.
           ADD TOTAL-PLA-RECV TO W-RECP(1).

           MOVE "NB" TO D-PX(1).
           IF GP-THE-ONLY-SF-SRCD NOT = " "
              MOVE GP-THE-ONLY-SF-SRCD TO D-PX(2)
           ELSE
              MOVE "RF" TO D-PX(2).
           MOVE "FB" TO D-PX(3).
           MOVE "PB" TO D-PX(4).
           MOVE "RN" TO D-PX(5).
           MOVE "SC" TO D-PX(6).
           MOVE "OT" TO D-PX(7).
           MOVE "  " TO D-PX(8).

           MOVE 0 TO SUB.
           MOVE 1 TO ACT
                     ACTX.
       NEXT-TY-NON-ADD.
           ADD 1 TO SUB.
           ADD TY-NO(SUB ACT) TO WY-NON(ACTX SUB).
           ADD TY-AMT(SUB ACT) TO WY-AMTN(ACTX SUB).
           ADD TY-NO(SUB ACT) TO WY-NON(8 SUB).
           ADD TY-AMT(SUB ACT) TO WY-AMTN(8 SUB).
           IF SUB < 2
              GO TO NEXT-TY-NON-ADD.
           IF ACT < 9
              MOVE 0 TO SUB
              IF ACT = 4 OR 5
                 ADD 1 TO ACT
                 MOVE 4 TO ACTX
                 GO TO NEXT-TY-NON-ADD
              ELSE
                 ADD 1 TO ACT
                          ACTX
                 GO TO NEXT-TY-NON-ADD.
      *    COMBINE 10-"RB" AND 11-"RO" INTO 7-"RN" FOR DISPLAY.
           IF ACT < 11
              MOVE 0 TO SUB
              ADD 1 TO ACT
              MOVE 5 TO ACTX
              GO TO NEXT-TY-NON-ADD.

           ADD TY-OTHCOL  TO W-COLAMT(1).
           ADD TY-INTCOL  TO W-COLAMT(2).
           ADD TY-LATECOL TO W-COLAMT(3).
           ADD TY-DEFCOL  TO W-COLAMT(4).
           ADD TY-PRINCOL TO W-COLAMT(5).

           ADD TY-RNPRIN  TO W-RNPRIN.

           ADD TY-CLREB1 TO W-REBAMT(1).
           ADD TY-CLREB2 TO W-REBAMT(1).
           ADD TY-CLREB3 TO W-REBAMT(1).
           ADD TY-CLREB4 TO W-REBAMT(1).
           ADD TY-AHREB  TO W-REBAMT(2).
           ADD TY-PPREB  TO W-REBAMT(3).
           ADD TY-O1REB  TO W-REBAMT(4).
           ADD TY-O2REB  TO W-REBAMT(5).
           ADD TY-O3REB  TO W-REBAMT(6).
           ADD TY-INTREB TO W-REBAMT(7).
           ADD TY-SCREB  TO W-REBAMT(8).
           ADD TY-MFREB  TO W-REBAMT(9).
           ADD TY-DFREB  TO W-REBAMT(10).

      *********************************
       ACCUM-TY-CHECKS SECTION.
      *********************************
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 7
              ADD TY-CKWRITTEN(SUB) TO W-C-AMT(SUB)
           END-PERFORM.

           ADD TY-ALLOTMENTS TO W-C-ALLOT.
           ADD TY-LOCKBOX    TO W-C-LBOX.

      *********************************
       SEND-TY-TOTALS SECTION.
      *********************************
           MOVE 0 TO SUB.
           MOVE 1 TO ACT.
       NEXT-TY-NOP-MOVE.
           ADD 1 TO SUB.
           MOVE W-NOP(ACT SUB) TO D-NOP(ACT SUB).
           MOVE W-AMTP(ACT SUB) TO D-AMTP(ACT SUB).
           IF SUB < 2
              GO TO NEXT-TY-NOP-MOVE.

           MOVE W-RECP(ACT) TO D-RECP(ACT).
           IF ACT < 2
              ADD 1 TO ACT
              MOVE 0 TO SUB
              GO TO NEXT-TY-NOP-MOVE.

           MOVE 0 TO SUB.
           MOVE 1 TO ACT.
       NEXT-TY-NON-MOVE.
           ADD 1 TO SUB.
           MOVE WY-NON(ACT SUB) TO D-NON(ACT SUB).
           MOVE WY-AMTN(ACT SUB) TO D-AMTN(ACT SUB).
           IF SUB < 2
              GO TO NEXT-TY-NON-MOVE.
           IF ACT < 8
              ADD 1 TO ACT
              MOVE 0 TO SUB
              GO TO NEXT-TY-NON-MOVE.
           MOVE 0 TO SUB.
       NEXT-TY-COLAMT-MOVE.
           ADD 1 TO SUB.
           MOVE W-COLAMT(SUB) TO D-COLAMT(SUB).
           IF SUB < 5
              GO TO NEXT-TY-COLAMT-MOVE.
           MOVE 0 TO SUB.

           MOVE W-RNPRIN TO D-RNPRIN.

       NEXT-TY-REBAMT-MOVE.
           ADD 1 TO SUB.
           MOVE W-REBAMT(SUB) TO D-REBAMT(SUB).
           IF SUB < 10
              GO TO NEXT-TY-REBAMT-MOVE.
           MOVE 0 TO SUB.
       NEXT-CK-AMT-MOVE.
           ADD 1 TO SUB.
           MOVE W-C-AMT(SUB) TO D-C-AMT(SUB).
      * MAKE INTERIM DEPOSITS POSITIVE
           IF SUB = 7
              MULTIPLY W-C-AMT(7) BY -1 GIVING D-C-AMT(7).
           IF SUB < 7
             GO TO NEXT-CK-AMT-MOVE.
       ALLOT-MOVE.
           MOVE W-C-ALLOT TO D-ALLOT.
           MOVE W-C-LBOX TO D-LBOX.

           MOVE DISPLAY-LIST1 TO WR-LIST.
           MOVE DISPLAY-BUF1  TO WR-BUF.
           PERFORM CALL-WRFORM.


      *********************************
       CLEAR-TY-TOTALS SECTION.
      *********************************
           MOVE 0 TO SUB.
           MOVE 1 TO ACT.
       NEXT-TY-NOP-CLEAR.
           ADD 1 TO SUB.
           MOVE 0 TO W-NOP(ACT SUB).
           MOVE 0 TO W-AMTP(ACT SUB).
           IF SUB < 2
              GO TO NEXT-TY-NOP-CLEAR.
           MOVE 0 TO W-RECP(ACT).
           IF ACT < 2
              ADD 1 TO ACT
              MOVE 0 TO SUB
              GO TO NEXT-TY-NOP-CLEAR.
           MOVE 0 TO SUB.
           MOVE 1 TO ACT.
       NEXT-TY-NON-CLEAR.
           ADD 1 TO SUB.
           MOVE 0 TO WY-NON(ACT SUB).
           MOVE 0 TO WY-AMTN(ACT SUB).
           IF SUB < 2
              GO TO NEXT-TY-NON-CLEAR.
           IF ACT < 8
              ADD 1 TO ACT
              MOVE 0 TO SUB
              GO TO NEXT-TY-NON-CLEAR.
           MOVE 0 TO SUB.
       NEXT-TY-COLAMT-CLEAR.
           ADD 1 TO SUB.
           MOVE 0 TO W-COLAMT(SUB).
           IF SUB < 5
              GO TO NEXT-TY-COLAMT-CLEAR.
           MOVE 0 TO SUB.

           MOVE 0 TO W-RNPRIN.

       NEXT-TY-REBAMT-CLEAR.
           ADD 1 TO SUB.
           MOVE 0 TO W-REBAMT(SUB).
           IF SUB < 10
              GO TO NEXT-TY-REBAMT-CLEAR.
           MOVE 0 TO SUB.
       NEXT-CK-AMT-CLEAR.
           ADD 1 TO SUB.
           MOVE 0 TO W-C-AMT(SUB).
           IF SUB < 7
             GO TO NEXT-CK-AMT-CLEAR.
       ALLOT-CLEAR.
           MOVE 0 TO W-C-ALLOT.
           MOVE 0 TO W-C-LBOX.

      *********************************
       TILL-ROUTINE SECTION.
      *********************************
           MOVE " " TO MESS.
           PERFORM SEND-LEGEND.

           MOVE " " TO DISPLAY-BUF1.

           PERFORM OPEN-CA1-FILE.

           MOVE HOLD-BRANCH2 TO BR-NO CA-BRNO D-BR2
                                QCA1-WBEG-BRNO
                                QCA1-WEND-BRNO.

           MOVE TRANS-DATE   TO CA-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QCA1-WBEG-DATE
                                        QCA1-WEND-DATE.

           MOVE HOLD-TILL-NO TO CA-TILLNO D-TILL-NO
                                QCA1-WBEG-TILLNO.
           MOVE ALL "9"      TO QCA1-WEND-TILLNO.

           PERFORM START-CA1-FILE.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
              MOVE 0 TO W-T-AMT(SUB)
           END-PERFORM.

           MOVE "A L L" TO BR-NAME.
           MOVE "N" TO RECORDS-EXIST.
       GET-CA-REC.
           PERFORM READ-CA1-FILE-NEXT.
           IF (IO-FG = 9) OR (CA-BRNO NOT = CA-PATH-OWNBR)
              GO TO END-TILL-ROUTINE.

           IF HOLD-BRANCH2 NOT = 0
              IF HOLD-BRANCH2 NOT = CA-BRNO
                 GO TO END-TILL-ROUTINE.

           IF CA-DATE NOT = TRANS-DATE
              GO TO GET-CA-REC.

           IF HOLD-TILL-NO NOT = 0
              IF CA-TILLNO NOT = HOLD-TILL-NO
                 GO TO GET-CA-REC.

           MOVE "Y" TO RECORDS-EXIST.
           PERFORM ACCUM-TILL-TOTALS.

           GO TO GET-CA-REC.
       END-TILL-ROUTINE.
           IF RECORDS-EXIST = "N"
              MOVE "NO RECORDS FOR SELECTION" TO MESS
              PERFORM SEND-MESS
              GO TO EXIT-TILL-ROUTINE.

           PERFORM SEND-TILL-TOTALS.

       EXIT-TILL-ROUTINE.
           PERFORM CLOSE-CA1-FILE.


      *********************************
       ACCUM-TILL-TOTALS SECTION.
      *********************************
           ADD CA-BEGBAL TO W-T-AMT(1).
           ADD CA-RUNNING TO W-T-AMT(2).
           ADD CA-NET-CASH-NOTES TO W-T-AMT(3).
           ADD W-T-AMT(1) W-T-AMT(2) W-T-AMT(3) GIVING W-T-AMT(4).
           IF CA-BALBY NOT = " "
              ADD CA-ENDBAL TO W-T-AMT(5)
              SUBTRACT W-T-AMT(4) FROM W-T-AMT(5) GIVING W-T-AMT(6).

      *******************************************
       SEND-TILL-TOTALS SECTION.
      *******************************************
           MOVE 0 TO SUB.
       NEXT-CA-NO-MOVE.
           ADD 1 TO SUB.
           MOVE W-T-AMT(SUB) TO D-T-AMT(SUB).
           IF SUB < 6
              GO TO NEXT-CA-NO-MOVE.

           MOVE DISPLAY-LIST2 TO WR-LIST.
           MOVE DISPLAY-BUF2 TO WR-BUF.
           PERFORM CALL-WRFORM.


      *****************************************************************
      *  ACCUM P&L TOTALS SECTION:
      *
      *  INPUT :  PL-TYPE
      *           PL-ACTY
      *
      *  OUTOUT : TOTAL-NO-PL
      *           TOTAL-AMT-PL
      *           TOTAL-O2-PL
      *           TOTAL-INT-PL
      *
      *  PL-TYPE = 1 - TY ACTIVE P&L TOTALS.
      *            2 - TY INACTIVE P&L TOTALS.
      *
      *            --------------------------------------
      *  PL-PERIOD 1 - CURRENT         |  1 - MTD
      *            2 - END LAST MONTH  |  2 - YTD
      *            3 - END-LAST YEAR   |  3 - LYR
      *            --------------------------------------
      *  PL-ACTY   1 - NET GAIN/LOSS   |  2 - NEW VOLUME
      *            --------------------------------------
      *
      *  PL-SUB3   1 - P1 ACTIVE          2 - P1 INACTIVE
      *            3 - P2 ACTIVE          4 - P2 INACTIVE
      *            5 - P3 ACTIVE          6 - P3 INACTIVE
      *            7 - P4 ACTIVE          8 - P4 INACTIVE
      *            9 - P5 ACTIVE         10 - P5 INACTIVE
      *
      *****************************************************************
       ACCUM-PL-TOTALS SECTION.
           MOVE 0 TO TOTAL-NO-PL
                     TOTAL-AMT-PL
                     TOTAL-O2-PL
                     TOTAL-INT-PL.

           IF PL-TYPE = 1 OR 5
              PERFORM ACCUM-PL-TY
                    VARYING PL-SUB3 FROM 1 BY 2 UNTIL PL-SUB3 > 9.

           IF PL-TYPE = 2 OR 6
              PERFORM ACCUM-PL-TY
                    VARYING PL-SUB3 FROM 2 BY 2 UNTIL PL-SUB3 > 10.

           GO TO EXIT-ACCUM-PL-TOTALS.

       ACCUM-PL-TY.
           ADD TY-NO-PL(PL-ACTY PL-SUB3) TO TOTAL-NO-PL.
           ADD TY-AMT-PL(PL-ACTY PL-SUB3) TO TOTAL-AMT-PL.
           ADD TY-O2-PL(PL-ACTY PL-SUB3) TO TOTAL-O2-PL.
           ADD TY-INT-PL(PL-ACTY PL-SUB3) TO TOTAL-INT-PL.

       EXIT-ACCUM-PL-TOTALS.
           EXIT.

      **************************************************************************
       FORM-RESET SECTION.
      **************************************************************************
           MOVE LPLOAN-SYSDATE TO LONPH-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY LONPH-SCREEN UPON TCLP-MAIN-WINDOW-HANDLE.
           MOVE LONPH-FORM-FIELDS TO WR-BUF.
           MOVE LONPH-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **************************************************************************
       SETUP-LINK-INFO SECTION.
      **************************************************************************
           IF VDU-FORM-NAME = "LPSMNU"
              MOVE LPSMNU-HELP--FILE TO HELP-LINK-FILE
              MOVE LPSMNU-HELP--DIR  TO HELP-LINK-DIR
           ELSE
              MOVE LONPH-HELP--FILE TO HELP-LINK-FILE
              MOVE LONPH-HELP--DIR TO HELP-LINK-DIR.

      **************************************************************************
       VDU-CONVERT-FIELD SECTION.
      **************************************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/LONPH_EVA.CPY".
              COPY "LIBLP/LPSMNU_EVA.CPY".
              COPY "LIBLP/LPLNMU_EVA.CPY".
              COPY "LIBLP/LPCAMU_EVA.CPY".
              COPY "LIBLP/LPVDMU_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBLP/LPMENU.CPY".

      *********************************
       MISC-PARAGRAPHS SECTION.
      *********************************
       COPY "LIBGB/SYSTEM.CPY".
       SEND-LEGEND.
           MOVE MESS TO WR-BUF.
           MOVE MSEL-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *********************************
       IO-ROUTINE SECTION.
      *********************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-TY1-FILE.
           PERFORM CLOSE-LC1-FILE.
           PERFORM CLOSE-CA1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPTYGS_SQL.CPY".
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBLP/LPCAGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPLC1RN.CPY".
       COPY "LIBLP/LPCA1RN.CPY".
       COPY "LIBLP/LPTY1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      *********************************
       END-ROUTINE SECTION.
      *********************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY LONPH-SCREEN.
           EXIT PROGRAM.
