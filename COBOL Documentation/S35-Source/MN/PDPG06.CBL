       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PDPG06.
       AUTHOR.        KEC.
       DATE-WRITTEN.  09-12-2000.
      ******************************************************************
      *
      * IDENTIFICATION: MN/PDPG06.C
      *
      * DESCRIPTION   : SCRATCH PAD FILE MAINTENANCE - PAGE 06
      *
      *-----------------------------------------------------------------
      * REV:
      * KEC 000912 ***NEW*** ADDED PER BARB & BETH.
      * JTG 040501 ADDED LOGIC TO CAUSE THE UNEARNED LOAN COST TO ROLL TO
      *            THE NEW LOAN AFTER 'RENEWAL'. NOTE: THIS COST IS REFLECTIVE
      *            OF THE DISCOUNT WHEN PURCHASING LOAN PAPER NOT CARRIED AS
      *            SALES PAPER WITH DEALER DISCOUNT.
      *            ADDED SP-LNCOST-FRMLA = 'R'                    REGENCY #2287
      * MJD 131127 A30 CONVERSION COMPLETED
      *  CS 170504 ACTIVATE 8 DIGIT DATES
      * KEC 2017.0710 ADDED 'PD' FILE PREFIX FOR WORKING STORAGE FIELDS.
      * BAH 190114 GLOBAL PDFILE
      *  CS 2020.01.17 CHANGED SCANKEY ENTRY TO ALPHA FROM NUMERIC
      *                ENTRY IS ID(PIC X),BRANCH,SS
      *  CS 20200120 CHANGED SCANKEY ENTRY TO  "E RA150SCANKEY." TO VDU.
      *  CS 20200123 REMOVED BRANCH FROM KEY ENTRY AND SCANKEY ENTRY, ITS
      *              LOCAL IN NATURE. KEY IS ID,BR,SOCIAL SO ADDED TESTS IN
      *              SCAN IF PD-BRANCH NOT = PD-PATH-OWNBR MOVE IN THE
      *              NEXT PD-ID, SO IF ITS AN 'L'(LOAN) AND THE BRANCH
      *              CHANGES, MOVE IN "Q" AS THE PD-ID AND READ-NEXT
      *  CS 20200127 SCANKEY STILL MUCKED WHEN BRANCH CHANGES, WAS
      *              SETTING UP HOLD-KEY BUT NOTHING WAS MOVED TO PD1-KEY
      *  CS 20200129 WRITING PAGE 6 CAUSED YOU TO LOSE PD2-KEY.
      *              WAS MOVING PD TO LN, PERFORM COMPUTE EARNINGS AND THEN
      *              MOVING LN TO PD.  INSTEAD OF MOVING THE LN TO PD, 
      *              AFTER THE PERFORM COMPUTE-EARNINGS, I MOVED BACK THE
      *              LN-EARN STUFF TO THE PD STUFF
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
           "@(#)MN/PDPG06.CBL S35 02/21/24-12:55:48 >".

      ******************************************************************
      *
      *    F I L E - R E C O R D - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01PD.CPY".
       COPY "LIBLP/LP01PD_SQL.CPY".
       COPY "LIBLP/LPWSPD.CPY".

       01  CHANGED-PD-REC              PIC X(PD-SIZE)   VALUE SPACES.
       01  ORIGINAL-PD-REC             PIC X(PD-SIZE)   VALUE SPACES.

       01  HOLD-PD1-KEY.
           03  HOLD-PD-ID              PIC X(01)        VALUE SPACES.
           03  HOLD-PD-BRANCH          PIC 9(04)        VALUE ZEROES.
           03  HOLD-PD-SOC-SEC         PIC 9(10)        VALUE ZEROES.

       01  WORK-PD1-KEY.
           03  WORK-PD-ID              PIC X       VALUE SPACES.
           03  WORK-PD-SOC-SEC         PIC 9(10)   VALUE ZEROS.


      ******************************************************************
       01  ONE               PIC S9.
       01  SUB               PIC 99       COMP-3 VALUE 0.
       01  ELE               PIC 99              VALUE 0.
       01  WORK-AMT          PIC S9(7)V99 COMP-3.
       01  IO-TYPE           PIC X(3)   VALUE SPACES.
       01  NEW-FLAG          PIC 9      VALUE 0.
       01  FIRST-SCAN        PIC 9      VALUE 0.

      *    'PAGE-CHANGE-FLAG' IS FOR KNOWING IF COMING FROM ANOTHER PAGE
       01  PAGE-CHANGE-FLAG      PIC X(1)    VALUE "N".
           88  PAGE-CHANGE-NO                VALUE "N".
           88  PAGE-CHANGE-YES               VALUE "Y".


       01  WS-ORGBR              PIC 9999.

       01  SCANKEY-STAT                PIC X(02)     VALUE SPACES.

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
           03  FILLER     PIC X(39) VALUE
               "070 080 090 100 110 120 130 140 LNSTAT.".

      *-----------------------------------------------------------------
       01  DISPLAY-KEY-BUF                           VALUE SPACES.
           03  D-KEY-ID                PIC X(01).
      *    03  D-KEY-BRANCH            PIC Z(04).
           03  D-KEY-SOC-SEC           PIC Z(10).

      *-----------------------------------------------------------------
       01  DISPLAY-KEY-LIST.
           03  FILLER PIC X(16) VALUE "KEY-ID KEY-SSNO.".

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
       01  MAX            PIC 99   VALUE 14.
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

       01  SPEC-TAB REDEFINES SPEC-TABLE.
           03  SPEC-REC OCCURS 14 TIMES.
               05  SPEC PIC X(7).
               05  CHFG PIC X.

      ***********************************************
       COPY "LIBLP/LPEARNW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".

       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBMN/PDMAINW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/PDPG06_DEF.CPY".
       COPY "LIBMN/PDPG06_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/ACCESSW.CPY".

       COPY "LIBGB/GETFMW.CPY".
       01 FORM-LINK-AREA    PIC X(2000).

       SCREEN SECTION.
       COPY "LIBMN/PDPG06_SCN.CPY".

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

           PERFORM INITIALIZATION.

       MAIN-PROGRAM-LOOP.

           PERFORM ENTER-KEY.

           IF ( IO-TYPE = "END" )
              MOVE "X" TO ERRCD
              GO TO MAIN-PROGRAM-EXIT.

           IF ( IO-TYPE = "CLR" )
              GO TO MAIN-PROGRAM-LOOP.

           PERFORM ENTRY-MODULE.

           IF ( IO-TYPE = "RWR" )
              PERFORM WRITE-MODULE.

           IF ( NEW-REC = "Y" )
              MOVE "P" TO FORM-P1
              MOVE 07  TO FORM-SEL1.

           IF ( FORM-SEL1 NOT = ZEROES ) AND
              ( FORM-P1       = "P"    )
              MOVE PD1-KEY TO SAVEKEY
              MOVE FORM-SEL1 TO FORMSEL
              GO TO MAIN-PROGRAM-EXIT.

           IF ( FORM-SEL2 NOT = ZEROES ) AND
              ( FORM-P2       = "P"    )
              MOVE PD1-KEY TO SAVEKEY
              MOVE FORM-SEL2 TO FORMSEL
              GO TO MAIN-PROGRAM-EXIT.

           GO TO MAIN-PROGRAM-LOOP.

       MAIN-PROGRAM-EXIT.
           GO TO END-PROGRAM.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************

           PERFORM GET-GPENV.

       COPY "LIBGB/CHKSEC.CPY".

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "PDPG06" TO VDU-FORM-NAME.
           MOVE FORM-LINK-AREA TO PDMAIN-LINK.
           MOVE SPACES TO VDU-PGMENU-SELECTION.

       INITIALIZATION-EXIT.
           EXIT.

      ******************************************************************
       ENTER-KEY SECTION.
      ******************************************************************
       ENTER-KEY-BEGIN.

           PERFORM FORM-RESET.

           MOVE SPACES TO PD-ID.
           MOVE ZEROES TO PD-BRANCH.
           MOVE ZEROES TO PD-SOC-SEC.

           MOVE ZEROES TO FIRST-SCAN.
           MOVE SPACES TO IO-TYPE.

           IF ( SAVEKEY NOT = SPACES )
              MOVE "Y"     TO PAGE-CHANGE-FLAG
              MOVE SAVEKEY TO PD1-KEY
              MOVE SPACES  TO SAVEKEY
              PERFORM DISPLAY-KEY
              GO TO ENTER-KEY-GET-RECORD.

           MOVE "N"    TO PAGE-CHANGE-FLAG.
           MOVE ZEROES TO NEW-FLAG.

           MOVE EXT-FILPATH-BRNO  TO BR-NO PD-BRANCH HOLD-PD-BRANCH.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF ( IO-BAD )
              MOVE "INVALID: BRANCH NUMBER IS NOT ON FILE" TO MESS
              PERFORM SEND-MESS
              MOVE "END" TO IO-TYPE
              GO TO ENTER-KEY-EXIT.

       ENTER-KEY-ID.

           MOVE "E  A010KEY-ID." TO VDU.
           PERFORM SCREENIO.

           IF (        F1-KEY ) GO TO ENTER-KEY-BEGIN.
           IF (        F6-KEY ) PERFORM ENTER-SCANKEY
                                IF ( STAT-GOOD )
                                   GO TO ENTER-KEY-GET-RECORD
                                ELSE
                                   PERFORM ENTER-KEY-BEGIN.
           IF (        F7-KEY ) GO TO ENTER-KEY-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-KEY-ID.

           MOVE VDUBUF TO PD-ID HOLD-PD-ID.

           IF NOT ( PD-ID = "B" OR "L" OR "Q" OR "R" )
              MOVE "INVALID: ONLY (B)ORROWER, (L)OAN," TO MESS
              PERFORM SEND-MESS
              MOVE "(R)ENEWALS OR (Q)UOTES" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-KEY-ID.

       ENTER-KEY-SSNO.

           MOVE "ENNN100KEY-SSNO." TO VDU.
           PERFORM SCREENIO.

           IF (        F1-KEY ) GO TO ENTER-KEY-BEGIN.
           IF (        F6-KEY ) PERFORM ENTER-SCANKEY
                                IF ( STAT-GOOD )
                                   GO TO ENTER-KEY-GET-RECORD
                                ELSE
                                   PERFORM ENTER-KEY-BEGIN.
           IF (        F7-KEY ) GO TO ENTER-KEY-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-KEY-SSNO.

           IF ( VDUNUM0 = ZEROES )
              GO TO ENTER-KEY-SSNO.

           MOVE VDUNUM0 TO PD-SOC-SEC HOLD-PD-SOC-SEC.

       ENTER-KEY-GET-RECORD.

           PERFORM OPEN-PD1-FILE.
           PERFORM READ-PD1-FILE.
           PERFORM CLOSE-PD1-FILE.
           IF ( IO-BAD )
              MOVE "* NOT ON FILE *" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-KEY-BEGIN.

           MOVE ZEROES TO NEW-FLAG.
           MOVE PD-REC TO ORIGINAL-PD-REC.
           PERFORM DISPLAY-FIELDS.

           IF ( NEW-REC = "Y" )
              MOVE 1 TO NEW-FLAG.

           GO TO ENTER-KEY-EXIT.

       ENTER-KEY-END.

           IF ( F7-KEY ) MOVE "END" TO IO-TYPE.

       ENTER-KEY-EXIT.
           EXIT.

      ******************************************************************
       ENTER-SCANKEY SECTION.
      ******************************************************************

           MOVE "XX" TO SCANKEY-STAT.

           MOVE " ENTER LOAN #, OR F6 TO SCAN" TO MESS.
           PERFORM CALL-TMMSG.

           PERFORM OPEN-PD1-FILE.
           MOVE EXT-FILPATH-BRNO TO PD-BRANCH.
           MOVE "B"              TO PD-ID HOLD-PD-ID.

           PERFORM START-PD1-FILE.

       ENTER-SCANKEY-ENTRY.

           MOVE "E RA110SCANKEY." TO VDU.
           PERFORM SCREENIO.

           IF (        F1-KEY ) GO TO ENTER-SCANKEY-END.
           IF (        F6-KEY ) GO TO ENTER-SCANKEY-READ-NEXT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-SCANKEY-ENTRY.

           IF ( VDUBUF = SPACES )
              IF ( FIRST-SCAN NOT = ZEROES )
                 MOVE PD1-KEY TO HOLD-PD1-KEY
                 MOVE PD-REC  TO ORIGINAL-PD-REC
                 MOVE "00"    TO SCANKEY-STAT
                 GO TO ENTER-SCANKEY-END
              ELSE
                 GO TO ENTER-SCANKEY-ENTRY.

           MOVE VDUBUF          TO WORK-PD1-KEY.
           MOVE WORK-PD-ID      TO PD-ID HOLD-PD-ID.
           MOVE HOLD-PD-BRANCH  TO PD-BRANCH.
           IF ( WORK-PD-SOC-SEC NOT NUMERIC )
              GO TO ENTER-SCANKEY-ENTRY
           ELSE
              MOVE WORK-PD-SOC-SEC TO PD-SOC-SEC HOLD-PD-SOC-SEC.

           PERFORM START-PD1-FILE.

       ENTER-SCANKEY-READ-NEXT.

           PERFORM READ-PD1-FILE-NEXT.
           IF (IO-BAD)
              GO TO ENTER-SCANKEY-END.

           IF PD-BRANCH NOT = PD-PATH-OWNBR
              GO TO ENTER-SCANKEY-READ-NEXT.

      *    UNLOCK PD-FILE.
           PERFORM DISPLAY-KEY.
           PERFORM DISPLAY-FIELDS.
           MOVE 1 TO FIRST-SCAN.
           GO TO ENTER-SCANKEY-ENTRY.

       ENTER-SCANKEY-END.

           MOVE ZEROES TO FIRST-SCAN.
           PERFORM CLOSE-PD1-FILE.

           MOVE SPACES TO MESS.
           PERFORM CALL-TMMSG.

           MOVE SCANKEY-STAT TO STAT.

      *-----------------------------------------------------------------
       ENTER-SCANKEY-EXIT.
           EXIT.

      ******************************************************************
      *
      *    E N T R Y - M O D U L E
      *
      *    NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *               1 - INITIAL NEW RECORD ENTRY
      *               2 - MODIFY NEW RECORD
      ******************************************************************
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
           GO TO A-01 A-01 A-01 A-01 A-01
                 A-01 A-07 A-08 A-09 A-10
                 A-11 A-12 A-13 A-14
                 DEPENDING ON ELE.

      *    **********************************
      *    *      SET RECORD ELEMENTS
      *    **********************************
      * ORIG ANTICIPATED:
       A-01.
           MOVE VDUNUM2 TO PD-ANTICERN(ELE).

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
           MOVE VDUNUM2 TO PD-ANTICADJ(ELE).
           SUBTRACT PD-ANTICADJ(ELE) FROM PD-ANTICERN(ELE).

      * NET ANTICIPATED:
           MOVE "SN S062  2." TO VDU.
           MOVE ELE TO VDUROW.
           MOVE PD-ANTICERN(ELE) TO VDUNUM2.
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
           MOVE VDUNUM2 TO PD-ACCERN(ELE).

      * UNEARNED:
           MOVE "SN S062  4." TO VDU.
           MOVE ELE TO VDUROW.
           IF PD-PLCD = " " AND PD-SERVACCT NOT = "Y"
              SUBTRACT PD-ACCERN(ELE) FROM PD-ANTICERN(ELE)
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

           ADD PD-ANTICERN(ELE) PD-ANTICADJ(ELE) GIVING WORK-AMT.

           IF WORK-AMT > 0
            IF PD-ANTICADJ(ELE) NOT = 0
             IF VDUNUM2 > WORK-AMT
                MOVE "UPFRONT EARNINGS CAN'T EXCEED ANTICIPATED"
                                                           TO MESS
               PERFORM SEND-MESS
               GO TO A-015.

           IF WORK-AMT > 0
            IF PD-ANTICADJ(ELE) = 0
             IF VDUNUM2 > PD-ACCERN(ELE)
                MOVE "UPFRONT EARNINGS MUST BE INCLUDED IN EARNINGS"
                                                           TO MESS
               PERFORM SEND-MESS
               GO TO A-015.
           MOVE VDUNUM2 TO PD-UFERN(ELE).

           PERFORM DISPLAY-TOTALS.

           GO TO CREATE-ROUTINE.

       A-07.
           MOVE VDUNUM2 TO PD-PRIOR-UNEARNED.
           GO TO CREATE-ROUTINE.

       A-08.
           MOVE VDUBUF TO PD-ERNDELQ-STOP.
           IF NOT (PD-ERNDELQ-STOP = " " OR "Y")
              MOVE "ENTER ' ' OR 'Y'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.

       A-09.
           MOVE VDUNUM2 TO PD-NONACRERN.
           GO TO CREATE-ROUTINE.

       A-10.
           MOVE VDUNUM2 TO PD-PRIOR-NONACRERN.
           GO TO CREATE-ROUTINE.
       A-11.
           MOVE VDU-DATE-YYYYMMDD TO PD-ERN-IB-PDTH-DATE.
           GO TO CREATE-ROUTINE.
       A-12.
           MOVE VDUNUM2 TO PD-ERN-IB-PRINBAL.
           GO TO CREATE-ROUTINE.
       A-13.
           MOVE VDUNUM2 TO PD-ERN-IB-INTOWE.
           GO TO CREATE-ROUTINE.
       A-14.
           MOVE VDU-DATE-YYYYMMDD TO PD-LAST-ACCRUAL-DATE.
           GO TO CREATE-ROUTINE.

       END-ENTRY-MODULE.
           MOVE 0 TO FORM-SEL1 FORM-SEL2.
       EXIT-ENTRY-MODULE.
           EXIT.

      ******************************************************************
       WRITE-MODULE SECTION.
      ******************************************************************

           PERFORM OPEN-PD1-FILE.

           MOVE PD-REC TO CHANGED-PD-REC.

           IF ( ORIGINAL-PD-REC = CHANGED-PD-REC )
              GO TO WRITE-MODULE-END.

           PERFORM READ-PD1-FILE.

           IF ( IO-BAD )
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO WRITE-MODULE-END.

           IF ( PD-REC NOT = ORIGINAL-PD-REC )
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO WRITE-MODULE-END.

           MOVE CHANGED-PD-REC TO PD-REC.
           PERFORM REWRITE-PD1-FILE.

           MOVE "C"             TO ERRLOGW-ACTION.
           MOVE PD-ID           TO HOLD-PD-ID.
           MOVE PD-BRANCH       TO HOLD-PD-BRANCH.
           MOVE PD-SOC-SEC      TO HOLD-PD-SOC-SEC.
           MOVE HOLD-PD1-KEY TO ERRLOGW-KEY.
           MOVE FORM-PATHNAME   TO ERRLOGW-PROGX.

           MOVE "CL/ERRLOG" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PROGX.

       WRITE-MODULE-END.

           PERFORM CLOSE-PD1-FILE.

       WRITE-MODULE-EXIT.
           EXIT.

      ****************************************
       DISPLAY-FIELDS SECTION.
      ****************************************

           MOVE 0 TO SUB.
           PERFORM DISPLAY-FIELDS-COMPONENTS 6 TIMES.

           MOVE PD-PRIOR-UNEARNED TO F7.
           MOVE PD-ERNDELQ-STOP TO F8.
           MOVE PD-NONACRERN TO F9.
           MOVE PD-PRIOR-NONACRERN TO F10.
           MOVE PD-ERN-IB-PDTH-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY         TO F11.
           MOVE PD-ERN-IB-PRINBAL TO F12.
           MOVE PD-ERN-IB-INTOWE TO F13.
           MOVE PD-LAST-ACCRUAL-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY          TO F14.
           MOVE PD-ANTICTERM TO F-ATERM.
           MOVE PD-ORGBR TO WS-ORGBR.

           STRING PD-ORGST, "-", WS-ORGBR, "-", PD-SPRCLASS, "(",
                  PD-CLASS, ")", "-", PD-SUBCLASS, "-", PD-LAWCODE,
                  "-", PD-SMALLOAN, PD-SALEFIN, PD-SERVACCT, PD-LOANTYPE
                  DELIMITED BY "  " INTO FLNSTAT.

      * DISPLAY PRIOR DEALER DISCOUNT UNEARNED FOR REFERENCE ONLY.
      *  ACTUAL INPUT IS ON PAGE 10.
      *  JTG 040501 ADDED LOGIC TO CAUSE THE UNEARNED LOAN COST TO ROLL TO
      *             THE NEW LOAN AFTER 'RENEWAL'. NOTE: THIS COST IS REFLECTIVE
      *             OF THE DISCOUNT WHEN PURCHASING LOAN PAPER NOT CARRIED AS
      *             SALES PAPER WITH DEALER DISCOUNT.
      *             ADDED SP-LNCOST-FRMLA = 'R'                    REGENCY #2287
      *       NOTE: LN-PRDLDISC-UNEARN WILL HOLD THE COST (DISCOUNT)
      *             FOR DIRECT LOANS, EVEN THOUGH IT IS NOT TIED TO A DEALER.
           MOVE PD-PRDLDISC-UNEARN TO F-PRDLDSC-UNE.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           PERFORM DISPLAY-TOTALS.

           GO TO DISPLAY-FIELDS-EXIT.

       DISPLAY-FIELDS-COMPONENTS.
           ADD 1 TO SUB.
           IF PD-POCD NOT = "BT"
              ADD PD-ANTICADJ(SUB) PD-ANTICERN(SUB) GIVING F(SUB).
           MOVE PD-ANTICADJ(SUB) TO F1(SUB).
           MOVE PD-ANTICERN(SUB) TO F2(SUB).
           MOVE PD-ACCERN(SUB) TO F3(SUB).
           IF PD-PLCD = " " AND PD-SERVACCT NOT = "Y"
              SUBTRACT PD-ACCERN(SUB) FROM PD-ANTICERN(SUB)
                                              GIVING F4(SUB)
              IF F4-ALP(SUB) NOT = " "
                 PERFORM DISPLAY-FIELDS-COMPONENTS-RB
              ELSE
                 NEXT SENTENCE
           ELSE
              MOVE 0 TO F4(SUB).
           MOVE PD-UFERN(SUB) TO F5(SUB).

       DISPLAY-FIELDS-COMPONENTS-RB.
      * SKIP REBATED COMPONENTS:
           IF SUB = 1 AND
              ( PD-ANTICADJ(1) NOT = 0
                      OR (NOT (PD-REBATE(4) = " " OR "C")) )
              IF PD-POCD = "BT"
                 MOVE "    BULKTR" TO F4-ALP(1)
              ELSE
                 MOVE "  REBATED?" TO F4-ALP(1)
           ELSE
           IF SUB = 3 AND
              ( PD-ANTICADJ(3) NOT = 0
                      OR (NOT (PD-REBATE(5) = " " OR "C")) )
              IF PD-POCD = "BT"
                 MOVE "    BULKTR" TO F4-ALP(3)
              ELSE
                 MOVE "  REBATED?" TO F4-ALP(3)
           ELSE
           IF SUB = 4 AND
              ( PD-ANTICADJ(4) NOT = 0
                      OR (NOT (PD-REBATE(6) = " " OR "C")) )
              IF PD-POCD = "BT"
                 MOVE "    BULKTR" TO F4-ALP(4)
              ELSE
                 MOVE "  REBATED?" TO F4-ALP(4).

       DISPLAY-FIELDS-EXIT.
           EXIT.

      ******************************************************************
       DISPLAY-KEY SECTION.
      ******************************************************************

           MOVE PD-ID      TO D-KEY-ID.
           MOVE PD-SOC-SEC TO D-KEY-SOC-SEC.

           MOVE DISPLAY-KEY-BUF TO WR-BUF.
           MOVE DISPLAY-KEY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       DISPLAY-KEY-EXIT.
           EXIT.

      **************************************************************************
       FORM-RESET SECTION.
      **************************************************************************
           MOVE EXT-CONAME-CONAME TO PDPG06-CONAME.
           MOVE EXT-CONAME-SYSDATE TO PDPG06-SYSDATE.
           DISPLAY SPACES UPON PDMAIN-WINDOW-HANDLE
                               LINE 0 COL 0 ERASE SCREEN.
           DISPLAY PDPG06-SCREEN UPON PDMAIN-WINDOW-HANDLE.
           MOVE PDPG06-FORM-FIELDS TO WR-BUF.
           MOVE PDPG06-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **************************************************************************
       SETUP-LINK-INFO SECTION.
      **************************************************************************
           MOVE PDPG06-HELP--FILE TO HELP-LINK-FILE.
           MOVE PDPG06-HELP--DIR TO HELP-LINK-DIR.

      **************************************************************************
       VDU-CONVERT-FIELD SECTION.
      **************************************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/PDPG06_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

       COPY "LIBLP/LPEARN.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".

      **************************************************************************
       MISC-PARAGRAPHS SECTION.
      **************************************************************************
       COPY "LIBGB/ACCESS.CPY".
       DISPLAY-TOTALS.
           MOVE PD-REC TO LN-REC.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 10
             MOVE PD-ANTICERN(SUB)   TO LN-ANTICERN(SUB)
             MOVE PD-ANTICADJ(SUB)   TO LN-ANTICADJ(SUB)
             MOVE PD-UFERN(SUB)      TO LN-UFERN(SUB)
             MOVE PD-ACCERN(SUB)     TO LN-ACCERN(SUB)
             MOVE PD-EARN-ORDER(SUB) TO LN-EARN-ORDER(SUB)
           END-PERFORM.
           PERFORM COMPUTE-EARNINGS.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 10
             MOVE LN-ANTICERN(SUB)   TO PD-ANTICERN(SUB)
             MOVE LN-ANTICADJ(SUB)   TO PD-ANTICADJ(SUB)
             MOVE LN-UFERN(SUB)      TO PD-UFERN(SUB)
             MOVE LN-ACCERN(SUB)     TO PD-ACCERN(SUB)
             MOVE LN-EARN-ORDER(SUB) TO PD-EARN-ORDER(SUB)
           END-PERFORM.

      *THIS CAUSED A LOSS OF PD2-KEY, SO WHEN TRYING TO DO A RECALL
      *OF LOAN IT COULDNT FIND IT
      *    MOVE LN-REC TO PD-REC.
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
           IF PD-PLCD NOT = " "
              MOVE "NONE (PL)" TO T4-ALP.
           IF PD-SERVACCT = "Y"
              MOVE "NONE (SA)" TO T4-ALP.
           MOVE EARN-TOT-UFERN TO T5.

           MOVE DISPLAY-TOTALS-BUF TO WR-BUF.
           MOVE DISPLAY-TOTALS-LIST TO WR-LIST.
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
       NOT-EXPIRED.
           MOVE "THIS RECORD HAS NOT EXPIRED - CANNOT DELETE" TO MESS.
           PERFORM SEND-MESS.

      **************************************************************************
       IO-ROUTINES SECTION.
      **************************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-PD1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPPDGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPPD1IN.CPY".

       COPY "LIBGB/FERRORS.CPY".

      **************************************************************************
       END-PROGRAM SECTION.
      **************************************************************************

           PERFORM CLOSE-FILES.

       EXIT-PROG.

       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY PDPG06-SCREEN.
           MOVE FORM-PATH TO FORM-PATHNAME.
           MOVE PDMAIN-LINK TO FORM-LINK-AREA.
           EXIT PROGRAM.
