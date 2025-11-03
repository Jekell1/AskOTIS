       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PDPG02.
       AUTHOR.        KEC.
       DATE-WRITTEN.  09-12-2000.
      ******************************************************************
      *
      * IDENTIFICATION: MN/PDPG02.C
      *
      * DESCRIPTION   : SCRATCH PAD FILE MAINTENANCE - PAGE 02
      *
      *-----------------------------------------------------------------
      * REV:
      * KEC 000912 {PR#00000} PARADATA FINANCIAL [BARB & BETH]
      *             ***NEW*** ADDED PER BARB & BETH.
      *
      * MJD 131127 A30 CONVERSION COMPLETED
      *  CS 170504 ACTIVATE 8 DIGIT DATES
      *
      * KEC 2017.0710 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          ADDED 'PD' FILE PREFIX FOR WORKING STORAGE FIELDS.
      * BAH 190114 GLOBAL PDFILE
      *  CS 20200114 REPLACED SCANKEY ENTRY FROM NUMERIC TO ALPHA
      *  CS 20200120 CHANGED SCANKEY ENTRY TO  "E RA150SCANKEY." TO VDU.
      *  CS 20200127 FIXED SCAN KEY WHEN BRANCH CHANGES
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
           "@(#)MN/PDPG02.CBL S35 02/21/24-13:17:27 >".

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
       COPY "LIBLP/LPMDTEW.CPY".

      *=================================================================
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
       01  IO-TYPE           PIC X(3)   VALUE SPACES.
       01  NEW-FLAG          PIC 9      VALUE 0.
       01  FIRST-SCAN        PIC 9      VALUE 0.

      * 'PAGE-CHANGE-FLAG' IS FOR KNOWING IF COMING FROM ANOTHER PAGE

       01  PAGE-CHANGE-FLAG      PIC X(1)    VALUE "N".
           88  PAGE-CHANGE-NO                VALUE "N".
           88  PAGE-CHANGE-YES               VALUE "Y".

       01  SCANKEY-STAT                PIC X(02)     VALUE SPACES.

       01  WS-ORGBR          PIC 9999.

      *    **********************************
      *    *      SCREEN DISPLAY BUFFER
      *    **********************************
       01  DISPLAY-BUF    VALUE SPACES.
           03  F01        PIC ZZ/ZZ/ZZ.
           03  F02        PIC ZZ/ZZ/ZZ.
           03  F03        PIC ZZ/ZZ/ZZ.
           03  F04        PIC ZZ/ZZ/ZZ.
           03  F05        PIC ZZ/ZZ/ZZ.
           03  F06        PIC ZZ/ZZ/ZZ.
           03  F07        PIC ZZ/ZZ/ZZ.
           03  F08        PIC ZZ/ZZ/ZZ.
           03  FLNORGMA   PIC ZZ/ZZ/ZZ.
           03  FLNUPDTMA  PIC ZZ/ZZ/ZZ.
           03  F09        PIC ZZ/ZZ/ZZ.
           03  F10        PIC ZZ/ZZ/ZZ.
           03  F11        PIC ZZ/ZZ/ZZ.
           03  F111       PIC ZZZZZZ.ZZ.
           03  F12        PIC ZZ/ZZ/ZZ.
           03  F13        PIC ZZ/ZZ/ZZ.
           03  F14        PIC ZZ/ZZ/ZZ.
           03  F15        PIC ZZ/ZZ/ZZ.
           03  F16        PIC ZZ/ZZ/ZZ.
           03  F17        PIC ZZ/ZZ/ZZ.
           03  F171       PIC ZZ/ZZ/ZZ.
           03  F172       PIC ZZ/ZZ/ZZ.
           03  F18        PIC ZZZ.ZZZ.
           03  F19        PIC ZZZ.ZZZ.
           03  F20        PIC ZZZ.ZZZ.
           03  F21        PIC ZZZ.ZZZ.
           03  F22        PIC ZZZ.ZZZ.
           03  F23        PIC ZZZ.ZZZ.
           03  F24        PIC ZZZ.ZZZ.
           03  F25        PIC ZZZ.ZZZ.
           03  F26        PIC ZZZ.ZZZ.
           03  F27        PIC ZZZ.ZZZ.
           03  F28        PIC ZZZ.ZZ.
           03  FLNSTAT    PIC X(30).
       01  DISPLAY-LIST.
           03  FILLER     PIC X(20) VALUE
               "010 020 030 040 050 ".
           03  FILLER     PIC X(38) VALUE
               "060 070 080 LNORGMA LNUPDTMA 090 100 ".
           03  FILLER     PIC X(24) VALUE
               "110 111 120 130 140 150 ".
           03  FILLER     PIC X(28) VALUE
               "160 170 171 172 180 190 200 ".
           03  FILLER     PIC X(39) VALUE
               "210 220 230 240 250 260 270 280 LNSTAT.".

      *-----------------------------------------------------------------
       01  DISPLAY-KEY-BUF                           VALUE SPACES.
           03  D-KEY-ID                PIC X(01).
      *    03  D-KEY-BRANCH            PIC Z(04).
           03  D-KEY-SOC-SEC           PIC Z(10).

      *-----------------------------------------------------------------
       01  DISPLAY-KEY-LIST.
      *    03  FILLER PIC X(27) VALUE "KEY-ID KEY-BRANCH KEY-SSNO.".
           03  FILLER PIC X(16) VALUE "KEY-ID KEY-SSNO.".

      *    ********************************* 
      *    *   ELEMENT SPECIFICATIONS       *
      *    **********************************
       78  MAX                     VALUE 28.
       01  SPEC-TABLE.
           03  FILLER     PIC X(8) VALUE "E NM060N".
           03  FILLER     PIC X(8) VALUE "E NM060N".
           03  FILLER     PIC X(8) VALUE "E NM060N".
           03  FILLER     PIC X(8) VALUE "E NM060N".
           03  FILLER     PIC X(8) VALUE "E NM060N".
           03  FILLER     PIC X(8) VALUE "ERNM060N".
           03  FILLER     PIC X(8) VALUE "ERNM060N".
           03  FILLER     PIC X(8) VALUE "ERNM060N".
           03  FILLER     PIC X(8) VALUE "ERNM060N".
           03  FILLER     PIC X(8) VALUE "ERNM060N".
           03  FILLER     PIC X(8) VALUE "ERNM060N".
           03  FILLER     PIC X(8) VALUE "ERNM060N".
           03  FILLER     PIC X(8) VALUE "ERNM060N".
           03  FILLER     PIC X(8) VALUE "ERNM060N".
           03  FILLER     PIC X(8) VALUE "ERNM060N".
           03  FILLER     PIC X(8) VALUE "ERNM060N".
           03  FILLER     PIC X(8) VALUE "ERNM060N".
           03  FILLER     PIC X(8) VALUE "EN N033N".
           03  FILLER     PIC X(8) VALUE "EN N033N".
           03  FILLER     PIC X(8) VALUE "EN N033N".
           03  FILLER     PIC X(8) VALUE "EN N033N".
           03  FILLER     PIC X(8) VALUE "EN N033N".
           03  FILLER     PIC X(8) VALUE "EN N033N".
           03  FILLER     PIC X(8) VALUE "EN N033N".
           03  FILLER     PIC X(8) VALUE "EN N033N".
           03  FILLER     PIC X(8) VALUE "EN N033N".
           03  FILLER     PIC X(8) VALUE "EN N033N".
           03  FILLER     PIC X(8) VALUE "EN N032N".

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
       COPY "LIBMN/PDPG02_DEF.CPY".
       COPY "LIBMN/PDPG02_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/ACCESSW.CPY".

       COPY "LIBGB/GETFMW.CPY".
       01 FORM-LINK-AREA    PIC X(2000).

       SCREEN SECTION.
       COPY "LIBMN/PDPG02_SCN.CPY".

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
              MOVE 03  TO FORM-SEL1.

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
       COPY "LIBGB/CHKSEC.CPY".

           MOVE SPACES TO VDU-PGMENU-SELECTION.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "PDPG02" TO VDU-FORM-NAME.
           MOVE FORM-LINK-AREA TO PDMAIN-LINK.

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

           IF (VDUBUF  = SPACES )
              IF ( FIRST-SCAN NOT = ZEROES )
                 MOVE PD1-KEY TO HOLD-PD1-KEY
                 MOVE PD-REC  TO ORIGINAL-PD-REC
                 MOVE "00"    TO SCANKEY-STAT
                 GO TO ENTER-SCANKEY-END
              ELSE
                 GO TO ENTER-SCANKEY-ENTRY.

           MOVE VDUBUF             TO WORK-PD1-KEY.
           MOVE WORK-PD-ID         TO PD-ID HOLD-PD-ID.
           MOVE HOLD-PD-BRANCH     TO PD-BRANCH.
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
      * FORM-NUM1 CHECKS FOR PAGES 1 THRU 9
           IF FORM-KEY1 = " " AND FORM-P1 = "P" AND FORM-SEL1 NUMERIC
              IF FORM-SEL1 < 1 OR FORM-SEL1 = 2 OR FORM-SEL1 > 9
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
           GO TO A-01 A-02 A-03 A-04 A-05
                 A-06 A-07 A-08 A-09 A-10
                 A-11 A-12 A-13 A-14 A-15
                 A-16 A-17 A-18 A-19 A-20
                 A-21 A-22 A-23 A-24 A-25
                 A-26 A-27 A-28
                 DEPENDING ON ELE.
           GO TO END-ENTRY-MODULE.

      *    **********************************
      *    *      SET RECORD ELEMENTS
      *    **********************************
       A-01.
           MOVE VDU-DATE-YYYYMMDD TO PD-ENTDATE.
           GO TO CREATE-ROUTINE.
       A-02.
           MOVE VDU-DATE-YYYYMMDD TO PD-PURDATE.
           GO TO CREATE-ROUTINE.
       A-03.
           MOVE VDU-DATE-YYYYMMDD TO PD-LOANDATE.
           GO TO CREATE-ROUTINE.
       A-04.
           MOVE VDU-DATE-YYYYMMDD TO PD-ORIG-1STPYDATE.
           GO TO CREATE-ROUTINE.
       A-05.
           MOVE VDU-DATE-YYYYMMDD TO PD-INTDATE.
           GO TO CREATE-ROUTINE.
       A-06.
           MOVE VDU-DATE-YYYYMMDD TO PD-FILEDATE.
           GO TO CREATE-ROUTINE.
       A-07.
           MOVE VDU-DATE-YYYYMMDD TO PD-CASHDATE.
           GO TO CREATE-ROUTINE.
       A-08.
           GO TO CREATE-ROUTINE.
       A-09.
           MOVE VDU-DATE-YYYYMMDD TO PD-POFFDATE.
           GO TO CREATE-ROUTINE.
       A-10.
           MOVE VDU-DATE-YYYYMMDD TO PD-JDDATE.
           GO TO CREATE-ROUTINE.
       A-11.
           MOVE VDU-DATE-YYYYMMDD TO PD-PLDATE.
       A-111.
           MOVE "ENNN062111." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO A-111.
           MOVE VDUNUM2 TO PD-PLAMT.
           IF PD-PLDATE = 0
              IF (PD-PLAMT NOT = 0)
                 MOVE "P&&L AMOUNT MUST BE ZERO!!" TO MESS
                 PERFORM SEND-MESS
                 GO TO A-111
              ELSE
                 NEXT SENTENCE
           ELSE
              IF PD-PLAMT = 0
                 MOVE "P&&L AMOUNT CAN'T BE ZERO!!" TO MESS
                 PERFORM SEND-MESS
                 GO TO A-111.
           GO TO CREATE-ROUTINE.
       A-12.
           MOVE VDU-DATE-YYYYMMDD TO PD-ACCELDATE.
           GO TO CREATE-ROUTINE.
       A-13.
           MOVE VDU-DATE-YYYYMMDD TO PD-REDUDATE.
           GO TO CREATE-ROUTINE.
       A-14.
           MOVE VDU-DATE-YYYYMMDD TO PD-LEGALDATE.
           GO TO CREATE-ROUTINE.
       A-15.
           MOVE VDU-DATE-YYYYMMDD TO PD-BNKRPTDATE.
           GO TO CREATE-ROUTINE.
       A-16.
           MOVE VDU-DATE-YYYYMMDD TO PD-DEBTAIDATE.
           GO TO CREATE-ROUTINE.
       A-17.
           MOVE VDU-DATE-YYYYMMDD TO PD-REPODATE.
           IF PD-REPODATE = 0
              IF PD-REPOCD = "X"
                 MOVE "LOAN REPO CODE = 'X', MUST ENTER!" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE
              ELSE
              IF PD-REPOCD = "V"
                 MOVE "LOAN REPO CODE = 'V', MUST ENTER!" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE
              ELSE
              IF PD-REPOCD = "S"
                 MOVE "LOAN REPO CODE = 'S', MUST ENTER!" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.
       A-171.
           MOVE "ERNM060171." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO A-171.
           MOVE VDU-DATE-YYYYMMDD TO PD-REDEEMDATE.
           IF PD-REDEEMDATE = 0
                    AND PD-REPOCD = "V"
              MOVE "LOAN REPO CODE = 'V', MUST ENTER!" TO MESS
              PERFORM SEND-MESS
              GO TO A-171.
       A-172.
           MOVE "ERNM060172." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO A-172.
           MOVE VDU-DATE-YYYYMMDD TO PD-SOLDDATE.
           IF PD-SOLDDATE = 0
              IF PD-REPOCD = "S"
                 MOVE "LOAN REPO CODE = 'S', MUST ENTER!" TO MESS
                 PERFORM SEND-MESS
                 GO TO A-172.
           GO TO CREATE-ROUTINE.
       A-18.
           MOVE VDUNUM3 TO PD-APRATE.
           GO TO CREATE-ROUTINE.
       A-19.
           MOVE VDUNUM3 TO PD-EFFRATE.
           GO TO CREATE-ROUTINE.
       A-20.
           MOVE VDUNUM3 TO PD-SMPRATE.
           GO TO CREATE-ROUTINE.
       A-21.
           MOVE VDUNUM3 TO PD-OVRDRATE.
           GO TO CREATE-ROUTINE.
       A-22.
           MOVE VDUNUM3 TO PD-ENDORATE.
           GO TO CREATE-ROUTINE.
       A-23.
           MOVE VDUNUM3 TO PD-YLDRATE.
           GO TO CREATE-ROUTINE.

       A-24.
           MOVE VDUNUM3 TO PD-CONRATE.
           GO TO CREATE-ROUTINE.
       A-25.
           MOVE VDUNUM3 TO PD-JDRATE.
           GO TO CREATE-ROUTINE.
       A-26.
           IF VDUNUM3 > 999.998
              MOVE "RATE MUST BE LESS THAN 999.999" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           MOVE VDUNUM3 TO PD-ACCRATE.
           GO TO CREATE-ROUTINE.
       A-27.
           IF VDUNUM3 > 999.996
              MOVE "RATE MUST BE LESS THAN 999.997" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           MOVE VDUNUM3 TO PD-REDURATE.
           GO TO CREATE-ROUTINE.
       A-28.
           MOVE VDUNUM2 TO PD-SERVPER.
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

      * CODE TO WRITE CHANGE FILE 

           MOVE "C"             TO ERRLOGW-ACTION.
           MOVE PD-ID           TO HOLD-PD-ID.
           MOVE PD-BRANCH       TO HOLD-PD-BRANCH.
           MOVE PD-SOC-SEC      TO HOLD-PD-SOC-SEC.
           MOVE HOLD-PD1-KEY    TO ERRLOGW-KEY.
           MOVE FORM-PATHNAME   TO ERRLOGW-PROGX.

           MOVE "CL/ERRLOG" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PROGX.

       WRITE-MODULE-END.

           PERFORM CLOSE-PD1-FILE.

       WRITE-MODULE-EXIT.
           EXIT.

      ********************************************
       DISPLAY-FIELDS SECTION.
      ********************************************
           MOVE PD-ENTDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO F01.

           MOVE PD-PURDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO F02.

           MOVE PD-LOANDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO F03.

           MOVE PD-ORIG-1STPYDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY       TO F04.

           MOVE PD-INTDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO F05.

           MOVE PD-FILEDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO F06.

           MOVE PD-CASHDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO F07.
           
      *-----------------------ORIGINAL MATURITY---------------------
           MOVE "Y" TO MDTE-FLAG.
           PERFORM MATURITY-DATE-CALCULATION.
           MOVE MDTE-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO FLNORGMA.
      *----------------------UPDATED MATURITY-----------------------
           PERFORM MATURITY-DATE-CALCULATION.
           MOVE MDTE-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO FLNUPDTMA.

           MOVE PD-POFFDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO F09.

           MOVE PD-JDDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO F10.

           MOVE PD-PLDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO F11.

           MOVE PD-PLAMT TO F111.
           MOVE PD-ACCELDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY  TO F12.

           MOVE PD-REDUDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO F13.

           MOVE PD-LEGALDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY  TO F14.

           MOVE PD-BNKRPTDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY   TO F15.

           MOVE PD-DEBTAIDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY   TO F16.

           MOVE PD-REPODATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO F17.

           MOVE PD-REDEEMDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY   TO F171.

           MOVE PD-SOLDDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO F172.

           MOVE PD-APRATE TO F18.
           MOVE PD-EFFRATE TO F19.
           MOVE PD-SMPRATE TO F20.
           MOVE PD-OVRDRATE TO F21.
           MOVE PD-ENDORATE TO F22.
           MOVE PD-YLDRATE TO F23.
           MOVE PD-CONRATE TO F24.
           MOVE PD-JDRATE TO F25.
           MOVE PD-ACCRATE TO F26.
           MOVE PD-REDURATE TO F27.
           MOVE PD-SERVPER TO F28.
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

      ********************************************
       DISPLAY-KEY SECTION.
      ********************************************

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
           MOVE EXT-CONAME-CONAME TO PDPG02-CONAME.
           MOVE EXT-CONAME-SYSDATE TO PDPG02-SYSDATE.
           DISPLAY SPACES UPON PDMAIN-WINDOW-HANDLE
                               LINE 0 COL 0 ERASE SCREEN.
           DISPLAY PDPG02-SCREEN UPON PDMAIN-WINDOW-HANDLE.
           MOVE PDPG02-FORM-FIELDS TO WR-BUF.
           MOVE PDPG02-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **************************************************************************
       SETUP-LINK-INFO SECTION.
      **************************************************************************
           MOVE PDPG02-HELP--FILE TO HELP-LINK-FILE.
           MOVE PDPG02-HELP--DIR TO HELP-LINK-DIR.

      **************************************************************************
       VDU-CONVERT-FIELD SECTION.
      **************************************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/PDPG02_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
      *
      *     C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/LPMDTEPD.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/ERRMSG.CPY".

      ******************************************************************
       MISC-PARAGRAPHS SECTION.
      ******************************************************************
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
       NOT-EXPIRED.
           MOVE "THIS RECORD HAS NOT EXPIRED - CANNOT DELETE" TO MESS.
           PERFORM SEND-MESS.

      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-PD1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPPDGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPPD1IN.CPY".

       COPY "LIBGB/FERRORS.CPY".

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************

           PERFORM CLOSE-FILES.

       EXIT-PROG.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY PDPG02-SCREEN.
           MOVE FORM-PATH TO FORM-PATHNAME.
           MOVE PDMAIN-LINK TO FORM-LINK-AREA.
           EXIT PROGRAM.

