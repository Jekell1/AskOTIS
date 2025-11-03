       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PDPG08.
       AUTHOR.        KEC.
       DATE-WRITTEN.  09-12-2000.
      ******************************************************************
      *
      * IDENTIFICATION: MN/PDPG08.C
      *
      * DESCRIPTION   : SCRATCH PAD FILE MAINTENANCE - PAGE 08
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
           "@(#)MN/PDPG08.CBL S34 06/06/21-16:23:44 >".

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
       01  IO-TYPE           PIC X(3)   VALUE SPACES.
       01  NEW-FLAG          PIC 9      VALUE 0.
       01  FIRST-SCAN        PIC 9      VALUE 0.

      * 'PAGE-CHANGE-FLAG' IS FOR KNOWING IF COMING FROM ANOTHER PAGE
      
       01  PAGE-CHANGE-FLAG      PIC X(1)    VALUE "N".
           88  PAGE-CHANGE-NO                VALUE "N".
           88  PAGE-CHANGE-YES               VALUE "Y".

       01  WS-ORGBR      PIC 9999.

       01  SCANKEY-STAT                PIC X(02)     VALUE SPACES.

      *    **********************************
      *    *      SCREEN DISPLAY BUFFER
      *    **********************************
       01  DISPLAY-BUF    VALUE SPACES.
           03  F01        PIC XXX.
           03  F02        PIC XXXX.
           03  F03        PIC 99/99/99 BLANK ZERO.
           03  F04        PIC Z(6).ZZ.
           03  F05        PIC 99/99/99 BLANK ZERO.
           03  F06        PIC 99/99/99 BLANK ZERO.
           03  F07        PIC Z(6).ZZ.
           03  F08        PIC ZZZ.
           03  F09        PIC 99/99/99 BLANK ZERO.
           03  F10        PIC ZZ.
           03  F11        PIC ZZ.
           03  F12        PIC ZZ.
           03  F121       PIC ZZ.
           03  F13        PIC 99/99/99 BLANK ZERO.
           03  F14        PIC X(4).
           03  F15        PIC X(30).
           03  F16        PIC Z(6).
           03  F17        PIC ZZZ-.
           03  F18        PIC ZZZ.
           03  F19        PIC 99/99/99 BLANK ZERO.
           03  F20        PIC ZZ.
           03  F21        PIC 9(12).
           03  F21-ALP REDEFINES F21
                          PIC X(12).
           03  F22        PIC 9(12).
           03  F22-ALP REDEFINES F22
                          PIC X(12).
           03  F23        PIC 9(12).
           03  F23-ALP REDEFINES F23
                          PIC X(12).
           03  F24        PIC 9(12).
           03  F24-ALP REDEFINES F24
                          PIC X(12).
           03  F25        PIC 9(12).
           03  F25-ALP REDEFINES F25
                          PIC X(12).
           03  F26        PIC 9(12).
           03  F26-ALP REDEFINES F26
                          PIC X(12).
           03  F27        PIC 9(12).
           03  F27-ALP REDEFINES F27
                          PIC X(12).
           03  F28        PIC 9(12).
           03  F28-ALP REDEFINES F28
                          PIC X(12).
           03  F29        PIC 9(12).
           03  F29-ALP REDEFINES F29
                          PIC X(12).
           03  F30        PIC 9(12).
           03  F30-ALP REDEFINES F30
                          PIC X(12).
           03  F31        PIC X(4).
           03  F32        PIC X(8).
           03  F33        PIC X.
           03  FLNSTAT    PIC X(30).

       01  DISPLAY-LIST.
           03  FILLER     PIC X(20) VALUE
               "010 020 030 040 050 ".
           03  FILLER     PIC X(20) VALUE
               "060 070 080 090 100 ".
           03  FILLER     PIC X(24) VALUE
               "110 120 121 130 140 150 ".
           03  FILLER     PIC X(20) VALUE
               "160 170 180 190 200 ".
           03  FILLER     PIC X(20) VALUE
               "210 220 230 240 250 ".
           03  FILLER     PIC X(20) VALUE
               "260 270 280 290 300 ".
           03  FILLER     PIC X(19) VALUE
               "310 320 330 LNSTAT.".

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
       01  MAX            PIC 99   VALUE 33.
       01  SPEC-TABLE.
      * COLLECTION INFORMATION:
           03  FILLER     PIC X(8) VALUE "EN A030N".
           03  FILLER     PIC X(8) VALUE "EN A040N".
           03  FILLER     PIC X(8) VALUE "ERNM060N".
           03  FILLER     PIC X(8) VALUE "ENNN062N".
           03  FILLER     PIC X(8) VALUE "ERNM080N".
           03  FILLER     PIC X(8) VALUE "ERNM060N".
           03  FILLER     PIC X(8) VALUE "ENNN062N".
           03  FILLER     PIC X(8) VALUE "ENNN030N".
           03  FILLER     PIC X(8) VALUE "ERNM060N".
           03  FILLER     PIC X(8) VALUE "ENNN020N".
           03  FILLER     PIC X(8) VALUE "ENNN020N".
           03  FILLER     PIC X(8) VALUE "ENNN020N".
           03  FILLER     PIC X(8) VALUE "ERNM060N".
           03  FILLER     PIC X(8) VALUE "EN A040N".

      * 15. BEEP MESSAG
           03  FILLER     PIC X(8) VALUE "EN A300N".

      * SOLICITATION INFORMATION:
           03  FILLER     PIC X(8) VALUE "ENNN060N".
           03  FILLER     PIC X(8) VALUE "ENNS030N".
           03  FILLER     PIC X(8) VALUE "ENNN030N".
           03  FILLER     PIC X(8) VALUE "ERNM060N".
           03  FILLER     PIC X(8) VALUE "ENNN020N".

      * PROFILES
           03  FILLER     PIC X(8) VALUE "ENNN120N".
           03  FILLER     PIC X(8) VALUE "ENNN120N".
           03  FILLER     PIC X(8) VALUE "ENNN120N".
           03  FILLER     PIC X(8) VALUE "ENNN120N".
           03  FILLER     PIC X(8) VALUE "ENNN120N".
           03  FILLER     PIC X(8) VALUE "ENNN120N".
           03  FILLER     PIC X(8) VALUE "ENNN120N".
           03  FILLER     PIC X(8) VALUE "ENNN120N".
           03  FILLER     PIC X(8) VALUE "ENNN120N".
           03  FILLER     PIC X(8) VALUE "ENNN120N".

           03  FILLER     PIC X(8) VALUE "EN A040N".
           03  FILLER     PIC X(8) VALUE "EN A080N".
           03  FILLER     PIC X(8) VALUE "EN A010N".

       01  SPEC-TAB REDEFINES SPEC-TABLE.
           03  SPEC-REC OCCURS 33 TIMES.
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
       COPY "LIBMN/PDPG08_DEF.CPY".
       COPY "LIBMN/PDPG08_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/ACCESSW.CPY".

       COPY "LIBGB/GETFMW.CPY".
       01 FORM-LINK-AREA    PIC X(2000).

       SCREEN SECTION.
       COPY "LIBMN/PDPG08_SCN.CPY".

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
              MOVE 09  TO FORM-SEL1.

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
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "PDPG08" TO VDU-FORM-NAME.
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

      *-----------------------------------------------------------------
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


      *-----------------------------------------------------------------
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

      *-----------------------------------------------------------------
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

      *-----------------------------------------------------------------
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
              IF FORM-SEL1 < 1 OR FORM-SEL1 > 9
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
                 A-26 A-27 A-28 A-29 A-30
                 A-31 A-32 A-33
                 DEPENDING ON ELE.
           GO TO END-ENTRY-MODULE.

      *    **********************************
      *    *      SET RECORD ELEMENTS
      *    **********************************
       A-01.
           MOVE VDUBUF TO PD-COLLECTOR.
           GO TO CREATE-ROUTINE.
       A-02.
           MOVE VDUBUF TO PD-ACTIONCD.
           GO TO CREATE-ROUTINE.
       A-03.
           MOVE VDU-DATE-YYYYMMDD  TO PD-PROMDATE.
           GO TO CREATE-ROUTINE.
       A-04.
           MOVE VDUNUM2 TO PD-PROMAMT.
           GO TO CREATE-ROUTINE.
       A-05.
           MOVE VDU-DATE-YYYYMMDD  TO PD-PROMDATE2.
           GO TO CREATE-ROUTINE.
       A-06.
           MOVE VDU-DATE-YYYYMMDD  TO PD-DATE-PAID-LAST.
           GO TO CREATE-ROUTINE.
       A-07.
           MOVE VDUNUM2 TO PD-AMT-PAID-LAST.
           GO TO CREATE-ROUTINE.
       A-08.
           MOVE VDUNUM0 TO PD-LASTCFORMNO.
           GO TO CREATE-ROUTINE.
       A-09.
           MOVE VDU-DATE-YYYYMMDD  TO PD-LASTCFORMDATE.
           GO TO CREATE-ROUTINE.
       A-10.
           MOVE VDUNUM0 TO PD-DELCNT.
           GO TO CREATE-ROUTINE.
       A-11.
           MOVE VDUNUM0 TO PD-DELCNT2.
           GO TO CREATE-ROUTINE.
       A-12.
           MOVE VDUNUM0 TO PD-TOTNOALDEL.
       A-121.
           MOVE "ENNN020121." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS NOT = "00" AND
              VDUSTATUS NOT = "1D"
              GO TO A-121.
           MOVE VDUNUM0 TO PD-YTDNOALDEL.
           GO TO CREATE-ROUTINE.

       A-13.
           MOVE VDU-DATE-YYYYMMDD  TO PD-FOLLOWUP-DATE.
           GO TO CREATE-ROUTINE.

       A-14.
           MOVE VDUBUF TO PD-ORIGIN.
           GO TO CREATE-ROUTINE.

       A-15.
           MOVE VDUBUF TO PD-MESSAGE.
           GO TO CREATE-ROUTINE.

       A-16.
           MOVE VDUNUM0 TO PD-CREDITLIM.
           GO TO CREATE-ROUTINE.
       A-17.
           MOVE VDUNUM0 TO PD-CRSCORE.
           GO TO CREATE-ROUTINE.
       A-18.
           MOVE VDUNUM0 TO PD-LASTSFORMNO.
           GO TO CREATE-ROUTINE.
       A-19.
           MOVE VDU-DATE-YYYYMMDD  TO PD-LASTSFORMDATE.
           GO TO CREATE-ROUTINE.
       A-20.
           MOVE VDUNUM0 TO PD-SOLCNT.
           GO TO CREATE-ROUTINE.

       A-21.
           MOVE VDUNUM0 TO PD-CONPROFILE(1) F21.
           INSPECT F21-ALP REPLACING ALL '9' BY '-'.
           MOVE "S  A120210." TO VDU.
           MOVE F21-ALP TO VDUBUF.
           PERFORM SCREENIO.
           GO TO CREATE-ROUTINE.
       A-22.
           MOVE VDUNUM0 TO PD-CONPROFILE(2) F22.
           INSPECT F22-ALP REPLACING ALL '9' BY '-'.
           MOVE "S  A120220." TO VDU.
           MOVE F22-ALP TO VDUBUF.
           PERFORM SCREENIO.
           GO TO CREATE-ROUTINE.
       A-23.
           MOVE VDUNUM0 TO PD-CONPROFILE(3) F23.
           INSPECT F23-ALP REPLACING ALL '9' BY '-'.
           MOVE "S  A120230." TO VDU.
           MOVE F23-ALP TO VDUBUF.
           PERFORM SCREENIO.
           GO TO CREATE-ROUTINE.
       A-24.
           MOVE VDUNUM0 TO PD-CONPROFILE(4) F24.
           INSPECT F24-ALP REPLACING ALL '9' BY '-'.
           MOVE "S  A120240." TO VDU.
           MOVE F24-ALP TO VDUBUF.
           PERFORM SCREENIO.
           GO TO CREATE-ROUTINE.
       A-25.
           MOVE VDUNUM0 TO PD-CONPROFILE(5) F25.
           INSPECT F25-ALP REPLACING ALL '9' BY '-'.
           MOVE "S  A120250." TO VDU.
           MOVE F25-ALP TO VDUBUF.
           PERFORM SCREENIO.
           GO TO CREATE-ROUTINE.
       A-26.
           MOVE VDUNUM0 TO PD-RECPROFILE(1) F26.
           INSPECT F26-ALP REPLACING ALL '9' BY '-'.
           MOVE "S  A120260." TO VDU.
           MOVE F26-ALP TO VDUBUF.
           PERFORM SCREENIO.
           GO TO CREATE-ROUTINE.
       A-27.
           MOVE VDUNUM0 TO PD-RECPROFILE(2) F27.
           INSPECT F27-ALP REPLACING ALL '9' BY '-'.
           MOVE "S  A120270." TO VDU.
           MOVE F27-ALP TO VDUBUF.
           PERFORM SCREENIO.
           GO TO CREATE-ROUTINE.
       A-28.
           MOVE VDUNUM0 TO PD-RECPROFILE(3) F28.
           INSPECT F28-ALP REPLACING ALL '9' BY '-'.
           MOVE "S  A120280." TO VDU.
           MOVE F28-ALP TO VDUBUF.
           PERFORM SCREENIO.
           GO TO CREATE-ROUTINE.
       A-29.
           MOVE VDUNUM0 TO PD-RECPROFILE(4) F29.
           INSPECT F29-ALP REPLACING ALL '9' BY '-'.
           MOVE "S  A120290." TO VDU.
           MOVE F29-ALP TO VDUBUF.
           PERFORM SCREENIO.
           GO TO CREATE-ROUTINE.
       A-30.
           MOVE VDUNUM0 TO PD-RECPROFILE(5) F30.
           INSPECT F30-ALP REPLACING ALL '9' BY '-'.
           MOVE "S  A120300." TO VDU.
           MOVE F30-ALP TO VDUBUF.
           PERFORM SCREENIO.
           GO TO CREATE-ROUTINE.
       A-31.
           MOVE VDUBUF TO PD-LPO.
           GO TO CREATE-ROUTINE.
       A-32.
           MOVE VDUBUF TO PD-REP-CD.
           GO TO CREATE-ROUTINE.
       A-33.
           MOVE VDUBUF TO PD-PROCESS-CD.
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
           MOVE PD-COLLECTOR TO F01.
           MOVE PD-ACTIONCD TO F02.
           MOVE PD-PROMDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.           
           MOVE DATE-MMDDYY TO F03.

           MOVE PD-PROMAMT TO F04.
           MOVE PD-PROMDATE2 TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.           
           MOVE DATE-MMDDYY  TO F05.

           MOVE PD-DATE-PAID-LAST TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.           
           MOVE DATE-MMDDYY       TO F06.

           MOVE PD-AMT-PAID-LAST TO F07.
           MOVE PD-LASTCFORMNO TO F08.

           MOVE PD-LASTCFORMDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.           
           MOVE DATE-MMDDYY      TO F09.

           MOVE PD-DELCNT TO F10.
           MOVE PD-DELCNT2 TO F11.
           MOVE PD-TOTNOALDEL TO F12.
           MOVE PD-YTDNOALDEL TO F121.
           MOVE PD-FOLLOWUP-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.           
           MOVE DATE-MMDDYY      TO F13.

           MOVE PD-ORIGIN TO F14.
           MOVE PD-MESSAGE TO F15.
           MOVE PD-CREDITLIM TO F16.
           MOVE PD-CRSCORE TO F17.
           MOVE PD-LASTSFORMNO TO F18.
           MOVE PD-LASTSFORMDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.           
           MOVE DATE-MMDDYY      TO F19.

           MOVE PD-SOLCNT TO F20.
           MOVE PD-ORGBR TO WS-ORGBR.

           STRING PD-ORGST, "-", WS-ORGBR, "-", PD-SPRCLASS, "(",
                  PD-CLASS, ")", "-", PD-SUBCLASS, "-", PD-LAWCODE,
                  "-", PD-SMALLOAN, PD-SALEFIN, PD-SERVACCT, PD-LOANTYPE
                  DELIMITED BY "  " INTO FLNSTAT.

           MOVE PD-CONPROFILE(1) TO F21.
           INSPECT F21-ALP REPLACING ALL '9' BY '-'.
           MOVE PD-CONPROFILE(2) TO F22.
           INSPECT F22-ALP REPLACING ALL '9' BY '-'.
           MOVE PD-CONPROFILE(3) TO F23.
           INSPECT F23-ALP REPLACING ALL '9' BY '-'.
           MOVE PD-CONPROFILE(4) TO F24.
           INSPECT F24-ALP REPLACING ALL '9' BY '-'.
           MOVE PD-CONPROFILE(5) TO F25.
           INSPECT F25-ALP REPLACING ALL '9' BY '-'.

           MOVE PD-RECPROFILE(1) TO F26.
           INSPECT F26-ALP REPLACING ALL '9' BY '-'.
           MOVE PD-RECPROFILE(2) TO F27.
           INSPECT F27-ALP REPLACING ALL '9' BY '-'.
           MOVE PD-RECPROFILE(3) TO F28.
           INSPECT F28-ALP REPLACING ALL '9' BY '-'.
           MOVE PD-RECPROFILE(4) TO F29.
           INSPECT F29-ALP REPLACING ALL '9' BY '-'.
           MOVE PD-RECPROFILE(5) TO F30.
           INSPECT F30-ALP REPLACING ALL '9' BY '-'.
           MOVE PD-LPO TO F31.
           MOVE PD-REP-CD TO F32.
           MOVE PD-PROCESS-CD TO F33.

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
           MOVE EXT-CONAME-CONAME TO PDPG08-CONAME.
           MOVE EXT-CONAME-SYSDATE TO PDPG08-SYSDATE.
           DISPLAY SPACES UPON PDMAIN-WINDOW-HANDLE
                               LINE 0 COL 0 ERASE SCREEN.
           DISPLAY PDPG08-SCREEN UPON PDMAIN-WINDOW-HANDLE.
           MOVE PDPG08-FORM-FIELDS TO WR-BUF.
           MOVE PDPG08-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **************************************************************************
       SETUP-LINK-INFO SECTION.
      **************************************************************************
           MOVE PDPG08-HELP--FILE TO HELP-LINK-FILE.
           MOVE PDPG08-HELP--DIR TO HELP-LINK-DIR.

      **************************************************************************
       VDU-CONVERT-FIELD SECTION.
      **************************************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/PDPG08_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

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
       NOT-EXPIRED.
           MOVE "THIS RECORD HAS NOT EXPIRED - CANNOT DELETE" TO MESS.
           PERFORM SEND-MESS.
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".

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
           DESTROY PDPG08-SCREEN.
           MOVE FORM-PATH TO FORM-PATHNAME.
           MOVE PDMAIN-LINK TO FORM-LINK-AREA.
           EXIT PROGRAM.
