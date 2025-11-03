       IDENTIFICATION DIVISION.
       PROGRAM-ID.      SSMAIN.
       DATE-WRITTEN.    011211.
      ******************************************************************
      *
      *                 (MN)
      *  DESCRIPTION:   CREDIT SCORE STATISTICS FILE MAINTENANCE
      *                 WORLD PR# 726
      *
      * REV:
      * BLM 131213 REMOVED LOGIC FOR PAGE 2, THERE IS NO PAGE 2 AT THIS
      *            TIME.  I THINK THIS MUST HAVE BEEN A RUSH JOB, I
      *            DON'T THINK JIM WANTED A MAINTENANCE BUT I COULDN'T
      *            TEST WITHOUT SOMETHING SO A SLAPPED THIS TOGETHER.
      *            I THINK IT'S MISSING THE PL TABLE.  MIKE CAUGHT THIS
      *            WHEN CONVERTING.  NOT TAKING THE TIME TO WRITE PAGE 2
      *            NOW, AT LEAST IT WON'T SAY "MISSING FORM" IF
      *            YOU TYPE P2
      * KEC 2017.0224 FIXED ITEMS FOR SS-CLASS GOING FROM 2 TO 3 BYTES.
      *               FIXED SCANKEY ENTRY (NOT WORKING IN A15).
      *  CS 181016 GLOBAL SS ACCESS CALL USING SS-SELECT-PATH
      * BAH 20190930 REMOVED ACCESS-CALL SSFILE
      * BAH 20220412 REMOVED ALL PAGE 2 LOGIC, MESSY
      ******************************************************************
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
      *    W O R K I N G - S T O R A G E    S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)MN/SSMAIN.CBL S35 02/23/24-10:19:16 >".

      ******************************************************************
      *
      *     F I L E - R E C O R D S - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01LC.CPY".
       COPY "LIBLP/LP01LC_SQL.CPY".
       COPY "LIBLP/LPWSLC.CPY".
       COPY "LIBLP/LP01SS.CPY".
       COPY "LIBLP/LP01SS_SQL.CPY".
       COPY "LIBLP/LPWSSS.CPY".

      *=================================================================
       01  CHANGED-SS-REC              PIC X(SS-SIZE).
       01  ORIGINAL-SS-REC             PIC X(SS-SIZE).

      *=================================================================
       01  HOLD-SS1-KEY                   PIC X(13)     VALUE SPACES.
       01  FILLER                         REDEFINES HOLD-SS1-KEY.
           03  HOLD-SS-DATE-YYYYMM        PIC 9(06).
           03  FILLER           REDEFINES HOLD-SS-DATE-YYYYMM.
               05  HOLD-SS-DATE-YYYYMM-CC PIC 9(02).
               05  HOLD-SS-DATE-YYYYMM-YY PIC 9(02).
               05  HOLD-SS-DATE-YYYYMM-MM PIC 9(02).
           03  HOLD-SS-BRNO               PIC 9(04).
           03  HOLD-SS-CLASS              PIC 9(03).

       01  SAVE-SS-DATE-YYYYMM         PIC 9(06)     VALUE ZEROES.

       01  SCANKEY-FIELD               PIC 9(7).
       01  FILLER                      REDEFINES SCANKEY-FIELD.
           03  SCANKEY-SS-BRNO         PIC 9(4).
           03  SCANKEY-SS-CLASS        PIC 9(3).

      ******************************************************************
      *
      *     M I S C E L L A N E O U S - W O R K E R S
      *
      ******************************************************************
       01  ERRCD           PIC X       VALUE SPACES.
       01  PAGE-TOTALS     VALUE ZEROS.
           03  TOT-UNITS   PIC S9(8)     OCCURS 5.
           03  TOT-OUTBAL  PIC S9(10)V99 OCCURS 5.

       01  ONE             PIC S9.
       01  ELE            PIC 9(2).
       01  ELEMENT         PIC 9(2).
       01  ACTION                VALUE SPACES.
           03  CHKEY             PIC X(4).
           03  CHNO              PIC 9(2).
           03  CHNO-X           REDEFINES CHNO
                                 PIC X(2).
       01  IO-TYPE         PIC X(3).
       01  NEW-FLAG        PIC 9.
       01  FIRST-SCAN      PIC 9 VALUE 0.

       01  WS-STOP-ON-SCANKEY-FG       PIC X(1)      VALUE "Y".

       01  HOLD-BRNAME     PIC X(20).
       01  HOLD-LCDESC     PIC X(20).

       01  DISPLAY-DATE    PIC 9(4).
       01  DISPLAY-DATEX   REDEFINES DISPLAY-DATE.
           03  DISPLAY-MO  PIC 99.
           03  DISPLAY-YR  PIC 99.
       01  DATE-LIST       PIC X(5)  VALUE "MMYY.".

      ******************************************************************
      *    SCREEN DISPLAY BUFFER 
      ******************************************************************
       01  DISPLAY-BUF1          VALUE " ".
           03  DISPLAY-SCORES    OCCURS 8.
               05  DISPLAY-DEL.
                   07  D-NO      PIC ZZZZZZ9- OCCURS 5.
                   07  D-AMT     PIC ZZZZZZZZ9.99- OCCURS 5.

       01  DISPLAY-LIST1.
           03  FILLER     PIC X(40) VALUE
               "010 011 012 013 014 020 021 022 023 024 ".
           03  FILLER     PIC X(40) VALUE
               "030 031 032 033 034 040 041 042 043 044 ".
           03  FILLER     PIC X(40) VALUE
               "050 051 052 053 054 060 061 062 063 064 ".
           03  FILLER     PIC X(40) VALUE
               "070 071 072 073 074 080 081 082 083 084 ".
           03  FILLER     PIC X(40) VALUE
               "090 091 092 093 094 100 101 102 103 104 ".
           03  FILLER     PIC X(40) VALUE
               "110 111 112 113 114 120 121 122 123 124 ".
           03  FILLER     PIC X(40) VALUE
               "130 131 132 133 134 140 141 142 143 144 ".
           03  FILLER     PIC X(40) VALUE
               "150 151 152 153 154 160 161 162 163 164.".

       01  TOTAL-BUF1             VALUE " ".
           03  D-TOT-NO   PIC ZZZZZZ9- OCCURS 5.
           03  D-TOT-AMT  PIC ZZZZZZZZ9.99- OCCURS 5.

       01  TOTAL-LIST1.
           03  FILLER     PIC X(35) VALUE
               "TOTNO1 TOTNO2 TOTNO3 TOTNO4 TOTNO5 ".
           03  FILLER     PIC X(40) VALUE
               "TOTAMT1 TOTAMT2 TOTAMT3 TOTAMT4 TOTAMT5.".

      ******************************************************************
      *    ELEMENT SPECIFICATIONS ----- PAGE 1
      ******************************************************************
       01  MAX              PIC 99  VALUE 16.
       01  SPEC-TABLE1.
           03  SPEC1-QTY   PIC X(8) VALUE "ENNS060 ".
           03  SPEC1-AMT   PIC X(8) VALUE "ENNS102 ".

       01  TEST-ELE       PIC 99.
           88 ELE-QTY     VALUE 1 3 5 7 9 11 13 15.
           88 ELE-AMT     VALUE 2 4 6 8 10 12 14 16.


      ******************************************************************
      *
      *     C O P Y    L I B R A R Y    W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBLP/LPSSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       01  SSMAIN-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/SSMAN1_DEF.CPY".
       COPY "LIBMN/SSMAN1_WKS.CPY".

       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBMN/SSMAN1_SCN.CPY".

      ******************************************************************
      *
      *    P R O C E D U R E - D I V I S I O N
      *
      ******************************************************************
       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.

      ******************************************************************
      *
      *    M A I N    P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.
           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE "SSMAN1"      TO VDU-FORM-NAME.

       COPY "LIBGB/CHKSEC.CPY".

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS SSMAIN-WINDOW-HANDLE.

       MAIN-MODULE.

           PERFORM FORM-RESET.

           PERFORM REC-SS-MODULE.
           IF IO-TYPE = "END"
              GO TO END-ROUTINE.

           IF IO-TYPE = "CLR"
              GO TO MAIN-MODULE.

           PERFORM ENTRY-MODULE.
           IF IO-TYPE = "RWR"
              PERFORM WRITE-MODULE.

           IF IO-TYPE = "DEL"
             PERFORM DELETE-MODULE.

           GO TO MAIN-MODULE.

      ******************************************************************
       REC-SS-MODULE SECTION.
      ******************************************************************
       REC-SS-MODULE-NO.
           MOVE 0 TO NEW-FLAG.

           MOVE " " TO IO-TYPE.

           MOVE 1 TO CUR-SUB.
           MOVE "S  A010CUR-ORIG." TO VDU.
           MOVE "CUR SCORE" TO VDUBUF.
           PERFORM SCREENIO.

           PERFORM GET-TODAYS-DATE.
           GO TO SETUP-SS-DATE.

      *----------------------------------------------------------------
       ENTER-DATE.
           MOVE "EN N040MMYY." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1<"
              PERFORM SCAN-MESS
              GO TO ENTER-DATE.

           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-REC-SS-MODULE.

           IF VDUSTATUS NOT = "00"
              GO TO REC-SS-MODULE-NO.

           IF VDUNUM0 = 0
              GO TO REC-SS-MODULE-NO.

           MOVE VDUNUM0 TO DISPLAY-DATE.

       SETUP-SS-DATE.
           MOVE DISPLAY-MO TO HOLD-SS-DATE-YYYYMM-MM.
           MOVE DISPLAY-YR TO HOLD-SS-DATE-YYYYMM-YY.
           MOVE 20         TO HOLD-SS-DATE-YYYYMM-CC.

           MOVE HOLD-SS-DATE-YYYYMM TO SS-DATE-YYYYMM.

       ENTER-BRANCH.
           MOVE "EN N040BRANCH." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1<"
              PERFORM SCAN-MESS
              GO TO ENTER-BRANCH.

           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-REC-SS-MODULE.

           IF VDUSTATUS = "1U"
              GO TO ENTER-DATE.

           IF VDUSTATUS NOT = "00"
              GO TO ENTER-BRANCH.

           IF VDUNUM0 = 0
              GO TO REC-SS-MODULE-NO.

           MOVE VDUNUM0 TO BR-NO
                           SS-BRNO
                           HOLD-SS-BRNO.

           PERFORM LOAD-BRANCH-MODULE.
           IF ( BR-PATH = SPACES )
              MOVE "INVALID BRANCH" TO MESS
              PERFORM SEND-MESS
              GO TO REC-SS-MODULE-NO.

           MOVE BR-NAME TO HOLD-BRNAME.

       GET-CLASS.
           MOVE "EN N020LC." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1U"
              GO TO ENTER-BRANCH.

           IF ( VDUSTATUS = "1<" )
              PERFORM ENTER-SCANKEY
              GO TO END-REC-SS-MODULE.

           IF VDUSTATUS = "1>"
             MOVE "END" TO IO-TYPE
             GO TO END-REC-SS-MODULE.

           IF VDUSTATUS NOT = "00"
             GO TO GET-CLASS.

           IF VDUNUM0 = 0
             GO TO GET-CLASS.

           MOVE VDUNUM0 TO SS-CLASS
                           HOLD-SS-CLASS
                           LC-CODE.

           PERFORM OPEN-LC1-FILE.
           PERFORM READ-LC1-FILE.
           PERFORM CLOSE-LC1-FILE.
           IF IO-FG NOT = 0
              MOVE "INVALID CLASS" TO MESS
              PERFORM SEND-MESS
              GO TO GET-CLASS.

           MOVE LC-DESC TO HOLD-LCDESC.

           PERFORM OPEN-SS1-FILE.
           PERFORM READ-SS1-FILE.
           PERFORM CLOSE-SS1-FILE.
           IF IO-FG = 0
              MOVE SS-REC TO ORIGINAL-SS-REC
              PERFORM DISPLAY-FIELDS
            ELSE
              PERFORM NEW-RECORD-SETUP.

       END-REC-SS-MODULE.
           EXIT.

      ******************************************************************
      *    E N T R Y - M O D U L E
      *
      *    NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *               1 - INITIAL NEW RECORD ENTRY
      *               2 - MODIFY NEW RECORD
      *
      ******************************************************************
       ENTRY-MODULE SECTION.
           MOVE " " TO IO-TYPE.
           MOVE 1 TO ONE.

       CREATE-ROUTINE.
           IF NEW-FLAG NOT = 1
              GO TO MODIFY-ROUTINE.

           ADD ONE TO ELEMENT.

           IF ELEMENT < 1
              MOVE 1 TO ELEMENT ONE.
           IF ELEMENT > MAX
              MOVE 2 TO NEW-FLAG
              GO TO MODIFY-ROUTINE.

           MOVE ELEMENT TO ELE.
           GO TO ENTER-ELE.

      *-----------------------------------------------------------------
       MODIFY-ROUTINE.
           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

           IF F3-KEY
              PERFORM CHANGE-SCORE-TYPE
              GO TO MODIFY-ROUTINE.

           IF VDUSTATUS NOT = "00"
              GO TO MODIFY-ROUTINE.

           MOVE VDUBUF TO ACTION.

           IF ACTION = " "
              MOVE "RWR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

           IF ACTION = "DELETE"
              MOVE "DEL" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

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

           MOVE ELE TO TEST-ELE.
           IF ELE-QTY
              MOVE SPEC1-QTY TO VDU
           ELSE
              MOVE SPEC1-AMT TO VDU.
           MOVE ELE TO VDUROW.

           MOVE 0 TO VDUCOL.
           MOVE "." TO VDUDECIMAL.
           PERFORM SCREENIO.

       TEST-UP.
           IF VDUSTATUS = "1U"
              IF NEW-FLAG = 1
                 MOVE -1 TO ONE
                 GO TO CREATE-ROUTINE.

       TEST-DOWN.
           IF VDUSTATUS = "1D"
              MOVE 1 TO ONE
              GO TO GOTO-FIELD1.

      *-----------------------------------------------------------------
       TEST-F1.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-ELE.
           MOVE 1 TO ONE.

      *-----------------------------------------------------------------
       GOTO-FIELD1.
           IF ELE < 3
              MOVE 1 TO SCORE-SUB
           ELSE IF ELE < 5
              MOVE 2 TO SCORE-SUB
           ELSE IF ELE < 7
              MOVE 3 TO SCORE-SUB
           ELSE IF ELE < 9
              MOVE 4 TO SCORE-SUB
           ELSE IF ELE < 11
              MOVE 5 TO SCORE-SUB
           ELSE IF ELE < 13
              MOVE 6 TO SCORE-SUB
           ELSE IF ELE < 15
              MOVE 7 TO SCORE-SUB
           ELSE
              MOVE 8 TO SCORE-SUB.

           IF ELE-QTY
              GO TO F-QTY
           ELSE
              GO TO F-AMT.

           GO TO END-ENTRY-MODULE.

       F-QTY.
           MOVE 1 TO DEL-SUB.
           MOVE VDUNUM0 TO SS-UNITS(SCORE-SUB CUR-SUB DEL-SUB).

           PERFORM DISPLAY-TOTALS.

       F-QTY-POTS.
           MOVE SPEC1-QTY TO VDU.
           MOVE ELE TO VDUROW.
           MOVE 1 TO VDUCOL.
           MOVE "." TO VDUDECIMAL.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO F-QTY-POTS.

           MOVE 2 TO DEL-SUB.
           MOVE VDUNUM0 TO SS-UNITS(SCORE-SUB CUR-SUB DEL-SUB).

           PERFORM DISPLAY-TOTALS.
       F-QTY-30S.
           MOVE SPEC1-QTY TO VDU.
           MOVE ELE TO VDUROW.
           MOVE 2 TO VDUCOL.
           MOVE "." TO VDUDECIMAL.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO F-QTY-POTS.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO F-QTY-30S.
           MOVE 3 TO DEL-SUB.
           MOVE VDUNUM0 TO SS-UNITS(SCORE-SUB CUR-SUB DEL-SUB).
           PERFORM DISPLAY-TOTALS.
       F-QTY-60S.
           MOVE SPEC1-QTY TO VDU.
           MOVE ELE TO VDUROW.
           MOVE 3 TO VDUCOL.
           MOVE "." TO VDUDECIMAL.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO F-QTY-30S.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO F-QTY-60S.
           MOVE 4 TO DEL-SUB.
           MOVE VDUNUM0 TO SS-UNITS(SCORE-SUB CUR-SUB DEL-SUB).
           PERFORM DISPLAY-TOTALS.
       F-QTY-90S.
           MOVE SPEC1-QTY TO VDU.
           MOVE ELE TO VDUROW.
           MOVE 4 TO VDUCOL.
           MOVE "." TO VDUDECIMAL.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO F-QTY-60S.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO F-QTY-90S.
           MOVE 5 TO DEL-SUB.
           MOVE VDUNUM0 TO SS-UNITS(SCORE-SUB CUR-SUB DEL-SUB).
           PERFORM DISPLAY-TOTALS.
           GO TO CREATE-ROUTINE.
       F-AMT.
           MOVE 1 TO DEL-SUB.
           MOVE VDUNUM2 TO SS-OUTBAL(SCORE-SUB CUR-SUB DEL-SUB).
           PERFORM DISPLAY-TOTALS.
       F-AMT-POTS.
           MOVE SPEC1-AMT TO VDU.
           MOVE ELE TO VDUROW.
           MOVE 1 TO VDUCOL.
           MOVE "." TO VDUDECIMAL.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO F-AMT-POTS.
           MOVE 2 TO DEL-SUB.
           MOVE VDUNUM2 TO SS-OUTBAL(SCORE-SUB CUR-SUB DEL-SUB).
           PERFORM DISPLAY-TOTALS.
       F-AMT-30S.
           MOVE SPEC1-AMT TO VDU.
           MOVE ELE TO VDUROW.
           MOVE 2 TO VDUCOL.
           MOVE "." TO VDUDECIMAL.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO F-AMT-POTS.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO F-AMT-30S.
           MOVE 3 TO DEL-SUB.
           MOVE VDUNUM2 TO SS-OUTBAL(SCORE-SUB CUR-SUB DEL-SUB).
           PERFORM DISPLAY-TOTALS.
       F-AMT-60S.
           MOVE SPEC1-AMT TO VDU.
           MOVE ELE TO VDUROW.
           MOVE 3 TO VDUCOL.
           MOVE "." TO VDUDECIMAL.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO F-AMT-30S.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO F-AMT-60S.
           MOVE 4 TO DEL-SUB.
           MOVE VDUNUM2 TO SS-OUTBAL(SCORE-SUB CUR-SUB DEL-SUB).
           PERFORM DISPLAY-TOTALS.
       F-AMT-90S.
           MOVE SPEC1-AMT TO VDU.
           MOVE ELE TO VDUROW.
           MOVE 4 TO VDUCOL.
           MOVE "." TO VDUDECIMAL.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO F-AMT-60S.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO F-AMT-90S.
           MOVE 5 TO DEL-SUB.
           MOVE VDUNUM2 TO SS-OUTBAL(SCORE-SUB CUR-SUB DEL-SUB).
           PERFORM DISPLAY-TOTALS.
           GO TO CREATE-ROUTINE.

       END-ENTRY-MODULE.
           EXIT.

      ******************************************************************
       ENTER-SCANKEY SECTION.
      ******************************************************************

           MOVE "S  A140SCANKEYX."         TO VDU.
           MOVE "SCAN (BRNOCL):"           TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "Y" TO WS-STOP-ON-SCANKEY-FG.
           MOVE SS-BRNO TO BR-NO.
           MOVE SS-DATE-YYYYMM TO SAVE-SS-DATE-YYYYMM.

           PERFORM OPEN-BR-FILE.
           PERFORM START-BR-FILE.

       READ-BR-NEXT.
           PERFORM READ-BR-FILE-NEXT.
           IF ( IO-FG = 9 )
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCANKEY.

           IF BR-MACHINE = SPACES
              GO TO READ-BR-NEXT.

           PERFORM LOAD-BRANCH-ACCESS.

           IF ( SS-PATH-SQL = SPACES )  
              GO TO READ-BR-NEXT.

           MOVE 0 TO FIRST-SCAN.

       ENTER-SCAN.

           MOVE ZEROES       TO SS-CLASS.
           MOVE BR-NO       TO SS-BRNO.
           MOVE SAVE-SS-DATE-YYYYMM TO SS-DATE-YYYYMM.
           PERFORM OPEN-SS1-FILE.

       ENTER-KEY.
           IF ( WS-STOP-ON-SCANKEY-FG = "Y" )
              MOVE "EN N070SCANKEY." TO VDU
              PERFORM SCREENIO
           ELSE
              GO TO READ-BYFILE-NEXT.

           IF ( VDUSTATUS = "10" )
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCANKEY.

           IF ( VDUSTATUS = "1<" )
              GO TO READ-BYFILE-NEXT.

           IF ( VDUSTATUS NOT = "00" )
              GO TO ENTER-KEY.

           IF ( VDUNUM0 = 0 )
              IF ( FIRST-SCAN NOT = 0 )
                 MOVE SS1-KEY TO HOLD-SS1-KEY
                 MOVE SS-REC TO ORIGINAL-SS-REC
                 GO TO CLOSE-SCANKEY
              ELSE
                 GO TO ENTER-KEY.

           MOVE VDUNUM0                    TO SCANKEY-FIELD.
           MOVE SCANKEY-SS-BRNO            TO SS-BRNO.
           MOVE SCANKEY-SS-CLASS           TO SS-CLASS.

           PERFORM START-SS1-FILE.

      *-----------------------------------------------------------------
       READ-BYFILE-NEXT.
           IF ( FIRST-SCAN = 0 )
              PERFORM START-SS1-FILE
              MOVE 1 TO FIRST-SCAN.

           PERFORM READ-SS1-FILE-NEXT.
           IF ( IO-FG = 9 )
              MOVE "N" TO WS-STOP-ON-SCANKEY-FG
              GO TO READ-BR-NEXT.

           IF ( SS-DATE-YYYYMM NOT = HOLD-SS-DATE-YYYYMM )
              MOVE "N" TO WS-STOP-ON-SCANKEY-FG
              GO TO READ-BR-NEXT.

           IF ( SS-BRNO NOT = BR-NO )
                MOVE "N" TO WS-STOP-ON-SCANKEY-FG
                GO TO READ-BR-NEXT.

           MOVE "Y" TO WS-STOP-ON-SCANKEY-FG.
      *    UNLOCK SS-FILE.
           PERFORM DISPLAY-SCAN-KEY.
           PERFORM DISPLAY-FIELDS.
           GO TO ENTER-KEY.

       DISPLAY-SCAN-KEY.
           MOVE "SN N040BRANCH." TO VDU.
           MOVE SS-BRNO TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SN N030LC." TO VDU.
           MOVE SS-CLASS TO VDUNUM0.
           PERFORM SCREENIO.

       CLOSE-SCANKEY.

           MOVE "S  A140SCANKEYX."         TO VDU.
           MOVE SPACES                     TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A070SCANKEY."          TO VDU.
           MOVE SPACES                     TO VDUBUF.
           PERFORM SCREENIO.

           PERFORM CLOSE-SS1-FILE.
           MOVE 0 TO FIRST-SCAN.

       END-SCANKEY.
           EXIT.

      *****************************************************************
       WRITE-MODULE SECTION.
      *****************************************************************
           PERFORM OPEN-SS1-FILE.

           IF NEW-FLAG NOT = 0
              GO TO WRITE-RECORD.

           MOVE SS-REC TO CHANGED-SS-REC.

           IF ORIGINAL-SS-REC = CHANGED-SS-REC
              GO TO END-WRITE-MODULE.

           PERFORM READ-SS1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-WRITE-MODULE.

           IF SS-REC NOT = ORIGINAL-SS-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-WRITE-MODULE.

           MOVE CHANGED-SS-REC TO SS-REC.
           PERFORM REWRITE-SS1-FILE.

      *  RECORD THAT THERE WAS A CHANGE IN A CHANGE FILE.

           MOVE "C" TO ERRLOGW-ACTION.
           MOVE SS1-KEY TO ERRLOGW-KEY.
           MOVE FORM-PATHNAME TO ERRLOGW-PROGX.
           MOVE "CL/ERRLOG" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PROGX.

           GO TO END-WRITE-MODULE.

       WRITE-RECORD.
           PERFORM WRITE-SS1-FILE.
           IF IO-FG NOT = 0
              PERFORM ALREADY-CREATED.

       END-WRITE-MODULE.
           PERFORM CLOSE-SS1-FILE.

      *****************************************************************
       DELETE-MODULE SECTION.
      *****************************************************************
           IF NEW-FLAG NOT = 0
              GO TO END-DELETE-MODULE.

           PERFORM OPEN-SS1-FILE.
           PERFORM READ-SS1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-DELETE-MODULE.

           IF SS-REC NOT = ORIGINAL-SS-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-CHANGED
              GO TO END-DELETE-MODULE.

           PERFORM DELETE-SS1-FILE.

       END-DELETE-MODULE.
           PERFORM CLOSE-SS1-FILE.

      *****************************************************************
       NEW-RECORD-SETUP SECTION.
      *****************************************************************
           MOVE 1 TO NEW-FLAG.

           MOVE "S  A000NEW." TO VDU.
           MOVE "* NEW RECORD *" TO VDUBUF.
           PERFORM SCREENIO.

           PERFORM CLEAR-SS-FILE.
           MOVE HOLD-SS1-KEY TO SS1-KEY.
           MOVE 0 TO ELE
                     ELEMENT.

      *****************************************************************
       DISPLAY-FIELDS SECTION.
      *****************************************************************
           MOVE " " TO DISPLAY-BUF1.

           PERFORM VARYING DEL-SUB FROM 1 BY 1 UNTIL DEL-SUB > 5
              AFTER SCORE-SUB FROM 1 BY 1 UNTIL SCORE-SUB > 8
                 MOVE SS-UNITS(SCORE-SUB CUR-SUB DEL-SUB) TO
                                  D-NO(SCORE-SUB DEL-SUB)
                 MOVE SS-OUTBAL(SCORE-SUB CUR-SUB DEL-SUB) TO
                                  D-AMT(SCORE-SUB DEL-SUB)
              END-PERFORM.

           MOVE DISPLAY-BUF1 TO WR-BUF.
           MOVE DISPLAY-LIST1 TO WR-LIST.
           PERFORM CALL-WRFORM.
           PERFORM DISPLAY-TOTALS.

       DISPLAY-FIELDS-EXIT.
           EXIT.

      *****************************************************************
       DISPLAY-TOTALS SECTION.
      *****************************************************************

           MOVE ZEROS TO PAGE-TOTALS.

           PERFORM VARYING DEL-SUB FROM 1 BY 1 UNTIL DEL-SUB > 5
              AFTER SCORE-SUB FROM 1 BY 1 UNTIL SCORE-SUB > 8
                 ADD SS-UNITS(SCORE-SUB CUR-SUB DEL-SUB)
                    TO TOT-UNITS(DEL-SUB)
                 ADD SS-OUTBAL(SCORE-SUB CUR-SUB DEL-SUB)
                    TO TOT-OUTBAL(DEL-SUB)
           END-PERFORM.

           MOVE " " TO TOTAL-BUF1.
           PERFORM VARYING DEL-SUB FROM 1 BY 1 UNTIL DEL-SUB > 5
              MOVE TOT-UNITS(DEL-SUB)  TO D-TOT-NO(DEL-SUB)
              MOVE TOT-OUTBAL(DEL-SUB) TO D-TOT-AMT(DEL-SUB)
           END-PERFORM.

           MOVE TOTAL-BUF1 TO WR-BUF.
           MOVE TOTAL-LIST1 TO WR-LIST.
           PERFORM CALL-WRFORM.


      *****************************************************************
       CHANGE-SCORE-TYPE SECTION.
      *****************************************************************
           IF CUR-SUB = 1
               MOVE 2 TO CUR-SUB
               MOVE "S  A010CUR-ORIG." TO VDU
               MOVE "ORIG SCORE" TO VDUBUF
               PERFORM SCREENIO
           ELSE
               MOVE 1 TO CUR-SUB
               MOVE "S  A010CUR-ORIG." TO VDU
               MOVE "CUR SCORE" TO VDUBUF
               PERFORM SCREENIO.

           PERFORM DISPLAY-FIELDS.


      ******************************************************************
       LOAD-BRANCH-MODULE SECTION.
      ******************************************************************
     
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF ( IO-BAD )
              MOVE SPACES                  TO BR-PATH
              GO TO LOAD-BRANCH-MODULE-EXIT.
     
           PERFORM LOAD-BRANCH-ACCESS.
     
       LOAD-BRANCH-MODULE-EXIT.
           EXIT.

      ******************************************************************
       LOAD-BRANCH-ACCESS SECTION.
      ******************************************************************
     
           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.
     
           MOVE "B"                        TO SS-PATH-OVERRIDE.
     
       LOAD-BRANCH-ACCESS-EXIT.
           EXIT.


      *****************************************************************
      *
      *     C O P Y    L I B R A R Y    S E C T I O N S
      *
      *****************************************************************
       COPY "LIBLP/LPSSCL.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".

      ****************************************************
       FORM-RESET SECTION.
      ****************************************************
           MOVE EXT-CONAME-SYSDATE TO SSMAN1-SYSDATE.
           DISPLAY SPACES UPON SSMAIN-WINDOW-HANDLE
                               LINE 0 COL 0 ERASE SCREEN.
           DISPLAY SSMAN1-SCREEN UPON SSMAIN-WINDOW-HANDLE.
           MOVE SSMAN1-FORM-FIELDS TO WR-BUF.
           MOVE SSMAN1-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ****************************************************
       SETUP-LINK-INFO SECTION.
      ****************************************************
           MOVE SSMAN1-HELP--FILE TO HELP-LINK-FILE.
           MOVE SSMAN1-HELP--DIR  TO HELP-LINK-DIR.

      ****************************************************
       VDU-CONVERT-FIELD SECTION.
      ****************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/SSMAN1_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *****************************************************************
       MISC-PARAGRAPHS SECTION.
      *****************************************************************
       COPY "LIBGB/ACCESS.CPY".
       RECORD-DELETED.
           MOVE "RECORD WAS DELETED!" TO MESS.
           PERFORM SEND-MESS.

      *----------------------------------------------------------------
       RECORD-CHANGED.
           MOVE "CHANGES WERE MADE TO THIS RECORD!" TO MESS.
           PERFORM SEND-MESS.

      *----------------------------------------------------------------
       CHANGE-AGAIN.
           MOVE "PLEASE RE-INPUT YOUR CHANGES!" TO MESS.
           PERFORM SEND-MESS.

      *----------------------------------------------------------------
       ALREADY-CREATED.
           MOVE "RECORD HAS BEEN ALREADY CREATED!" TO MESS.
           PERFORM SEND-MESS.

      *----------------------------------------------------------------
       UNDERGONE-CHANGE.
           MOVE
           "THIS RECORD HAS UNDERGONE CHANGE BY ANOTHER USER,"
           TO MESS.
           PERFORM SEND-MESS.

      *----------------------------------------------------------------
       ACTIVE-RECORD.
           MOVE "ACTIVE CUSTOMER CANNOT BE DELETED" TO MESS.
           PERFORM SEND-MESS.

      *----------------------------------------------------------------
       GET-TODAYS-DATE.
           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           MOVE ALP-MO TO DISPLAY-MO.
           MOVE ALP-YR TO DISPLAY-YR.
           MOVE DISPLAY-DATE TO WR-BUF.
           MOVE DATE-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *----------------------------------------------------------------
       SCAN-MESS.
           MOVE "SCAN ONLY ON 'CLASS'" TO MESS.
           PERFORM SEND-MESS.

      *****************************************************************
       IO-ROUTINES SECTION.
      *****************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-SS1-FILE.
           PERFORM CLOSE-LC1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPSSGS_SQL.CPY".
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBLP/LPLC1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPSS1IN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      *****************************************************************
       END-ROUTINE SECTION.
      *****************************************************************
       END-RUN.
           PERFORM CLOSE-FILES.

       EXIT-PROG.
           MOVE "LP/LPM3MU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           DESTROY SSMAIN-WINDOW-HANDLE.
           EXIT PROGRAM.
