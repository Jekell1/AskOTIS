       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PDPG05.
       AUTHOR.        KEC.
       DATE-WRITTEN.  09-12-2000.
      ******************************************************************
      *
      * IDENTIFICATION: MN/PDPG05.C
      *
      * DESCRIPTION   : SCRATCH PAD FILE MAINTENANCE - PAGE 05
      *
      * REV:
      * KEC 000912 {PR#00000} PARADATA FINANCIAL [BARB & BETH]
      * JTG 020123 EXPANDED PAYMNTD FIELDS TO 1,000,000        LENDMARK PR#J021
      *   081117 CS ADD TEST GP-INCL-OTH-IN-EARNED = 'R'. IF 'R' EXCLUDE
      *             OTHER(ANTICERN(6) AND MAINTFEE(ANTICERN(4) FROM
      *             ANTICIPATED     REGACC PR#210
      * MJD 131127 A30 CONVERSION COMPLETED
      * BAH 161018 CHANGED 12 TO PD-COUPON-STATEMENT FROM PD-LEDGERCNT
      *  CS 170504 ACTIVATE 8 DIGIT DATES
      * KEC 2017.0710 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          ADDED 'PD' FILE PREFIX FOR WORKING STORAGE FIELDS.
      * BAH 190114 GLOBAL PDFILE
      *  CS 2020.01.17 CHANGED SCANKEY ENTRY TO ALPHA FROM NUMERIC
      *                ENTRY IS ID(PIC X),BRANCH,SS
      *  CS 20200120 CHANGED SCANKEY ENTRY TO  "E RA150SCANKEY." TO VDU.
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
           "@(#)MN/PDPG05.CBL S35 02/21/24-12:36:47 >".

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
       01  ELE               PIC 99     VALUE 0.
       01  ACCERN            PIC S9(6)V99 COMP.
       01  IO-TYPE           PIC X(3)   VALUE SPACES.
       01  NEW-FLAG          PIC 9      VALUE 0.
       01  FIRST-SCAN        PIC 9      VALUE 0.

      *    'PAGE-CHANGE-FLAG' IS FOR KNOWING IF COMING FROM ANOTHER PAGE
       01  PAGE-CHANGE-FLAG      PIC X(1)    VALUE "N".
           88  PAGE-CHANGE-NO                VALUE "N".
           88  PAGE-CHANGE-YES               VALUE "Y".

       01  SCANKEY-STAT                PIC X(02)     VALUE SPACES.

       01  WS-ORGBR      PIC 9999.

      *    **********************************
      *    *      SCREEN DISPLAY BUFFER
      *    **********************************
       01  DISPLAY-BUF    VALUE SPACES.
           03  F01        PIC Z(6).ZZ-.
           03  F02        PIC Z(5).ZZ-.
           03  F03        PIC Z(5).ZZ-.
           03  F04        PIC Z(5).ZZ-.
           03  F05        PIC Z(5).ZZ-.
           03  F06        PIC 99/99/99 BLANK ZERO.
           03  F07        PIC 99/99/99 BLANK ZERO.
           03  F08        PIC 99/99/99 BLANK ZERO.
           03  F09        PIC Z(5).ZZ-.
           03  F10        PIC Z(5).ZZ-.
           03  F11        PIC ZZZZ.
           03  F111       PIC ZZZZ.
           03  F12        PIC ZZZ.
           03  F13        PIC ZZZ.
           03  F14        PIC Z(6).ZZ.
           03  F15        PIC Z(6).ZZ.
           03  F16        PIC Z(7).ZZ-.
           03  F161       PIC Z(7).ZZ-.
           03  F17        PIC Z(7).ZZ-.
           03  F18        PIC Z(7).ZZ-.
           03  F19        PIC Z(6).ZZ-.
           03  F20        PIC Z(03).
           03  F201       PIC Z(6).ZZ-.
           03  F21        PIC Z(6).ZZ-.
           03  F22        PIC Z(6).ZZ-.
           03  F23        PIC Z(5).ZZ-.
           03  F24        PIC Z(5).ZZ-.
           03  F25        PIC Z(6).ZZ-.
           03  F26        PIC ZZ-.
           03  F261       PIC ZZ-.
           03  F27        PIC ZZ-.
           03  F271       PIC ZZ-.
           03  F28        PIC Z(5).ZZ-.
           03  F29        PIC Z(5).ZZ-.
           03  F30        PIC Z(5).ZZ-.
           03  FLNSTAT    PIC X(30).

       01  DISPLAY-LIST.
           03  FILLER     PIC X(20) VALUE
               "010 020 030 040 050 ".
           03  FILLER     PIC X(20) VALUE
               "060 070 080 090 100 ".
           03  FILLER     PIC X(24) VALUE
               "110 111 120 130 140 150 ".
           03  FILLER     PIC X(28) VALUE
               "160 161 170 180 190 200 201 ".
           03  FILLER     PIC X(20) VALUE
               "210 220 230 240 250 ".
           03  FILLER     PIC X(35) VALUE
               "260 261 270 271 280 290 300 LNSTAT.".

      *-----------------------------------------------------------------
       01  DISPLAY-KEY-BUF                           VALUE SPACES.
           03  D-KEY-ID                PIC X(01).
      *    03  D-KEY-BRANCH            PIC Z(04).
           03  D-KEY-SOC-SEC           PIC Z(10).

      *-----------------------------------------------------------------
       01  DISPLAY-KEY-LIST.
      *    03  FILLER PIC X(27) VALUE "KEY-ID KEY-BRANCH KEY-SSNO.".
           03  FILLER PIC X(16) VALUE "KEY-ID KEY-SSNO.".

      *    **********************************
      *    *   ELEMENT SPECIFICATIONS       *
      *    **********************************
       78  MAX                     VALUE 30.
       01  SPEC-TABLE.
      * 1.
           03 FILLER      PIC X(8) VALUE "ENNS062N".
      * 2.
           03 FILLER      PIC X(8) VALUE "ENNS052N".
      * 3.
           03 FILLER      PIC X(8) VALUE "ENNS052N".
      * 4. OTHER   BALANCE
           03 FILLER      PIC X(8) VALUE "ENNS052N".
      * 5. OTHER 2 BALANCE
           03 FILLER      PIC X(8) VALUE "ENNS052N".
      * 6. LATE CHARGE PDTH
           03 FILLER      PIC X(8) VALUE "ERNM060N".
      * 7.
           03 FILLER      PIC X(8) VALUE "ERNM060N".
      * 8.
           03 FILLER      PIC X(8) VALUE "ERNM060N".
      * 9.
           03 FILLER      PIC X(8) VALUE "ENNS052N".
      *10.
           03 FILLER      PIC X(8) VALUE "ENNS052N".
      *11.
           03 FILLER      PIC X(8) VALUE "ENNN040N".
      *12. PD-COUPON-STATEMENT
           03 FILLER      PIC X(8) VALUE "ENNN010N".
      *13.
           03 FILLER      PIC X(8) VALUE "ENNN030N".
      *14.
           03 FILLER      PIC X(8) VALUE "ENNN062N".
      *15.
           03 FILLER      PIC X(8) VALUE "ENNN062N".
      *16.
           03 FILLER      PIC X(8) VALUE "ENNN072N".
      *17.
           03 FILLER      PIC X(8) VALUE "ENNS062N".
      *18.
           03 FILLER      PIC X(8) VALUE "ENNS072N".
      *19.
           03 FILLER      PIC X(8) VALUE "ENNN072N".
      *20.  PD-TOTNOLCHG
           03 FILLER      PIC X(8) VALUE "ENNS030N".
      *21.
           03 FILLER      PIC X(8) VALUE "ENNS062N".
      *22.
           03 FILLER      PIC X(8) VALUE "ENNS062N".
      *23.
           03 FILLER      PIC X(8) VALUE "ENNS062N".
      *24.
           03 FILLER      PIC X(8) VALUE "ENNS052N".
      *25.
           03 FILLER      PIC X(8) VALUE "ENNS052N".
      *26.
           03 FILLER      PIC X(8) VALUE "ENNS020N".
      *27.
           03 FILLER      PIC X(8) VALUE "ENNS020N".
      *28.
           03 FILLER      PIC X(8) VALUE "ENNS052N".
      *29.
           03 FILLER      PIC X(8) VALUE "ENNS052N".
      *30.
           03 FILLER      PIC X(8) VALUE "ENNS052N".
      *31.
           03 FILLER      PIC X(8) VALUE "ENNS052N".

       01  SPEC-TAB REDEFINES SPEC-TABLE.
           03  SPEC-REC OCCURS MAX TIMES.
               05  SPEC PIC X(7).
               05  CHFG PIC X.

      ***********************************************
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBMN/PDMAINW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/PDPG05_DEF.CPY".
       COPY "LIBMN/PDPG05_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/ACCESSW.CPY".

       COPY "LIBGB/GETFMW.CPY".
       01 FORM-LINK-AREA    PIC X(2000).

       SCREEN SECTION.
       COPY "LIBMN/PDPG05_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME FORM-LINK-AREA EXIT-PATHNAME.

      ******************************************************************
      *
      *    M A I N - P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.
           PERFORM SQL-CONNECT.

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
              MOVE 06  TO FORM-SEL1.

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
           MOVE "PDPG05" TO VDU-FORM-NAME.
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
      * FORM-NUM1 CHECKS FOR PAGES 1 THRU 8
           IF FORM-KEY1 = " " AND FORM-P1 = "P" AND FORM-SEL1 NUMERIC
              IF FORM-SEL1 < 1 OR FORM-SEL1 = 5 OR FORMSEL > 9
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
           IF CHNO = 18
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
           GO TO A-01 A-02 A-03 A-04 A-05
                 A-06 A-07 A-08 A-09 A-10
                 A-11 A-12 A-13 A-14 A-15
                 A-16 A-17 A-18 A-19 A-20
                 A-21 A-22 A-23 A-24 A-25
                 A-26 A-27 A-28 A-29 A-30
                 DEPENDING ON ELE.
           GO TO END-ENTRY-MODULE.

      *    **********************************
      *    *      SET RECORD ELEMENTS
      *    **********************************
       A-01.
           IF VDUNUM2 < 0
              MOVE "BELOW ZERO ONLY ALLOWED FOR REVOLVING ACCOUNTS"
                                                            TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           MOVE VDUNUM2 TO PD-CURBAL.
           GO TO CREATE-ROUTINE.
       A-02.
           MOVE VDUNUM2 TO PD-INTBAL.
           GO TO CREATE-ROUTINE.
       A-03.
           MOVE VDUNUM2 TO PD-LCBAL.
           GO TO CREATE-ROUTINE.
       A-04.
           MOVE VDUNUM2 TO PD-OTHBAL.
           GO TO CREATE-ROUTINE.
       A-05.
           MOVE VDUNUM2 TO PD-OT2BAL.
           GO TO CREATE-ROUTINE.
       A-06.
           MOVE VDU-DATE-YYYYMMDD TO NUM-DATE
                           PD-LCPDTH-DATE.
           PERFORM TSTDAT.
           IF NUM-DATE = 0
              GO TO ENTER-ELE.
           MOVE PD-UNITPER-CD TO DATER-UNITPER-CD.
           MOVE PD-UNITPER-FREQ TO DATER-UNITPER-FREQ.
           MOVE PD-1STPYDATE TO NUM-DATE.
           MOVE PD-LCPDTH-DATE TO SYS-DATE.
           PERFORM TIMUPER.
           IF ELAPSED-UNITPER-REM NOT = 0
              MOVE "DATE IS NOT ON A PAY PERIOD CYCLE DATE!!!!"
                                                     TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-07.
           MOVE VDU-DATE-YYYYMMDD TO PD-INTPDTH-DATE.
           GO TO CREATE-ROUTINE.
       A-08.
           MOVE VDU-DATE-YYYYMMDD  TO PD-1STPYDATE.
           GO TO CREATE-ROUTINE.
       A-09.
           MOVE VDUNUM2 TO PD-LCPAID.
           GO TO CREATE-ROUTINE.
       A-10.
           MOVE VDUNUM2 TO PD-LCPARTIALS.
           GO TO CREATE-ROUTINE.
       A-11.
           IF VDUNUM0 NOT > 0
              MOVE "MUST BE > 0" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           MOVE VDUNUM0 TO PD-STMT-COUNTER.
       A-111.
           MOVE "ENNN040111." TO VDU.
           PERFORM SCREENIO.
           IF UP-KEY GO TO ENTER-ELE.
           IF NOT (ENTER-KEY OR DOWN-KEY) GO TO A-111.
           IF VDUNUM0 NOT > 0
              MOVE "COUNTER MUST BE GREATER THAN ZERO!" TO MESS
              PERFORM SEND-MESS
              GO TO A-111.
           IF VDUNUM0 > PD-STMT-COUNTER
              MOVE "PRIOR CANNOT EXCEED THE CURRENT COUNTER" TO MESS
              PERFORM SEND-MESS
              GO TO A-111.
           MOVE VDUNUM0 TO PD-PRIOR-STMT-COUNTER.
           GO TO CREATE-ROUTINE.
       A-12.
           MOVE VDUBUF TO PD-COUPON-STATEMENT.
           IF NOT (PD-COUPON-STATEMENT = " " OR "C" OR "S" OR "E")
              MOVE "ENTER ' ', 'C', 'S' OR 'E'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-13.
           GO TO CREATE-ROUTINE.
       A-14.
           MOVE VDUNUM2 TO PD-RECASTINT.
           GO TO CREATE-ROUTINE.
       A-15.
           MOVE VDUNUM2 TO PD-RECASTPRIN.
           GO TO CREATE-ROUTINE.
       A-16.
           MOVE VDUNUM2 TO PD-TOTPAYMNTD.
       A-161.
           MOVE "ENNS072161." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO A-161.
           MOVE VDUNUM2 TO PD-TOTEXCPAYMNTD.
           GO TO CREATE-ROUTINE.
       A-17.
           MOVE VDUNUM2 TO PD-TOTERN.
           GO TO CREATE-ROUTINE.
       A-18.
           GO TO CREATE-ROUTINE.
       A-19.
           MOVE VDUNUM2 TO PD-TOTINT.
           GO TO CREATE-ROUTINE.
       A-20.
           MOVE VDUNUM0 TO PD-TOTNOLCHG.
       A-201.
           MOVE "ENNS052201." TO VDU.
           PERFORM SCREENIO.
           IF UP-KEY GO TO ENTER-ELE.
           IF NOT (ENTER-KEY OR DOWN-KEY) GO TO A-201.
           MOVE VDUNUM2 TO PD-TOTLCHG.
           GO TO CREATE-ROUTINE.
       A-21.
           MOVE VDUNUM2 TO PD-TOTDEF.
           GO TO CREATE-ROUTINE.
       A-22.
           MOVE VDUNUM2 TO PD-TOTSERACCT.
           GO TO CREATE-ROUTINE.
       A-23.
           MOVE VDUNUM2 TO PD-INTWAIVERS.
           GO TO CREATE-ROUTINE.
       A-24.
           MOVE VDUNUM2 TO PD-LCWAIVERS.
           GO TO CREATE-ROUTINE.
       A-25.
           MOVE VDUNUM2 TO PD-TOTOTH.
           GO TO CREATE-ROUTINE.
       A-26.
           MOVE VDUNUM0 TO PD-TOTNODEF.
       A-261.
           MOVE "ENNN030261." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO ENTER-ELE.
           IF NOT (ENTER-KEY OR DOWN-KEY) GO TO A-261.
           MOVE VDUNUM0 TO PD-TOTNODEFACT.
           GO TO CREATE-ROUTINE.
       A-27.
           MOVE VDUNUM0 TO PD-YTDNODEF.
       A-271.
           MOVE "ENNN030271." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO ENTER-ELE.
           IF NOT (ENTER-KEY OR DOWN-KEY) GO TO A-271.
           MOVE VDUNUM0 TO PD-YTDNODEFACT.
           GO TO CREATE-ROUTINE.
       A-28.
           MOVE VDUNUM2 TO PD-TOTINTPD.
           GO TO CREATE-ROUTINE.
       A-29.
           MOVE VDUNUM2 TO PD-YTDINTPD.
           GO TO CREATE-ROUTINE.
       A-30.
           MOVE VDUNUM2 TO PD-LYRINTPD.
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

      ******************************************************************
       DISPLAY-FIELDS SECTION.
      ******************************************************************

           MOVE PD-CURBAL TO F01.
           MOVE PD-INTBAL TO F02.
           MOVE PD-LCBAL TO F03.
           MOVE PD-OTHBAL TO F04.
           MOVE PD-OT2BAL TO F05.
           MOVE PD-LCPDTH-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY    TO F06.

           MOVE PD-INTPDTH-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY     TO F07.

           MOVE PD-1STPYDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY  TO F08.

           MOVE PD-LCPAID TO F09.
           MOVE PD-LCPARTIALS TO F10.
           MOVE PD-STMT-COUNTER TO F11.
           MOVE PD-PRIOR-STMT-COUNTER TO F111.
           MOVE PD-COUPON-STATEMENT TO F12.
           MOVE SPACES TO F13.
           MOVE PD-RECASTINT TO F14.
           MOVE PD-RECASTPRIN TO F15.
           MOVE PD-TOTPAYMNTD TO F16.
           MOVE PD-TOTEXCPAYMNTD TO F161.
           MOVE PD-TOTERN TO F17.
           COMPUTE ACCERN = PD-ACCERN(1) + PD-ACCERN(2)
                          + PD-ACCERN(3) + PD-ACCERN(5).
           IF NOT( GP-INCL-OTH-IN-EARNED-N OR
                                GP-INCL-OTH-IN-EARNED-R)
              ADD PD-ACCERN(6) TO ACCERN.

           IF NOT(GP-INCL-OTH-IN-EARNED-R)
              ADD PD-ACCERN(4) TO ACCERN.

           MOVE ACCERN TO F18.
           MOVE PD-TOTINT TO F19.
           MOVE PD-TOTNOLCHG TO F20.
           MOVE PD-TOTLCHG TO F201.
           MOVE PD-TOTDEF TO F21.
           MOVE PD-TOTSERACCT TO F22.
           MOVE PD-INTWAIVERS TO F23.
           MOVE PD-LCWAIVERS TO F24.
           MOVE PD-TOTOTH TO F25.
           MOVE PD-TOTNODEF TO F26.
           MOVE PD-TOTNODEFACT TO F261.
           MOVE PD-YTDNODEF TO F27.
           MOVE PD-YTDNODEFACT TO F271.
           MOVE PD-TOTINTPD TO F28.
           MOVE PD-YTDINTPD TO F29.
           MOVE PD-LYRINTPD TO F30.
           MOVE PD-ORGBR TO WS-ORGBR.

           STRING PD-ORGST, "-", WS-ORGBR, "-", PD-SPRCLASS, "(",
                  PD-CLASS, ")", "-", PD-SUBCLASS, "-", PD-LAWCODE,
                  "-", PD-SMALLOAN, PD-SALEFIN, PD-SERVACCT, PD-LOANTYPE
                  DELIMITED BY "  " INTO FLNSTAT.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
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
           MOVE EXT-CONAME-CONAME TO PDPG05-CONAME.
           MOVE EXT-CONAME-SYSDATE TO PDPG05-SYSDATE.
           DISPLAY SPACES UPON PDMAIN-WINDOW-HANDLE
                               LINE 0 COL 0 ERASE SCREEN.
           DISPLAY PDPG05-SCREEN UPON PDMAIN-WINDOW-HANDLE.
           MOVE PDPG05-FORM-FIELDS TO WR-BUF.
           MOVE PDPG05-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **************************************************************************
       SETUP-LINK-INFO SECTION.
      **************************************************************************
           MOVE PDPG05-HELP--FILE TO HELP-LINK-FILE.
           MOVE PDPG05-HELP--DIR TO HELP-LINK-DIR.

      **************************************************************************
       VDU-CONVERT-FIELD SECTION.
      **************************************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/PDPG05_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/GPENV.CPY".

      **************************************************************************
       MISC-PARAGRAPHS SECTION.
      **************************************************************************
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
           DESTROY PDPG05-SCREEN.
           MOVE FORM-PATH TO FORM-PATHNAME.
           MOVE PDMAIN-LINK TO FORM-LINK-AREA.
           EXIT PROGRAM.
