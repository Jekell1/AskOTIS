       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PDPG03.
       AUTHOR.        KEC.
       DATE-WRITTEN.  09-12-2000.
      ******************************************************************
      *
      * IDENTIFICATION: MN/PDPG03.C
      *
      * DESCRIPTION   : SCRATCH PAD FILE MAINTENANCE - PAGE 03
      *
      *-----------------------------------------------------------------
      * REV:
      * KEC 000912 {PR#00000} PARADATA FINANCIAL [BARB & BETH]
      * MJD 121127 A30 CONVERSION COMPLETED
      * KEC 2016.0329 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED ELSCAN WORKERS TO MATCH CD1-KEY FOR EL LAYOUT.
      * KEC 2017.0710 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          ADDED 'PD' FILE PREFIX FOR WORKING STORAGE FIELDS.
      * BAH 190114 GLOBAL PDFILE
      *  CS 20200114 REPLACED SCANKEY NUMERIC ENTRY WITH ALPHA
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
           "@(#)MN/PDPG03.CBL S35 02/21/24-12:36:29 >".

      ******************************************************************
      *
      *    F I L E - R E C O R D - W O R K E R S
      *
      ******************************************************************
       COPY "LIBLP/LP01PD.CPY".
       COPY "LIBLP/LP01PD_SQL.CPY".
       COPY "LIBLP/LPWSPD.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LP01CD_SQL.CPY".
       COPY "LIBLP/LPWSCD.CPY".

       
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
       01  SUB               PIC 99     COMP-3 VALUE 0.
       01  ELE               PIC 99     VALUE 0.
       01  NOREG-FG          PIC X      VALUE " ".
       01  TOTAL-FEE-INS-FINCHG   PIC S9(7)V99.
       01  IO-TYPE           PIC X(3)   VALUE SPACES.
       01  NEW-FLAG          PIC 9      VALUE 0.
       01  FIRST-SCAN        PIC 9      VALUE 0.
       01  AMT-HOLDER        PIC 9(6)V99 COMP-3.

       
      *    'PAGE-CHANGE-FLAG' IS FOR KNOWING IF COMING FROM ANOTHER PAGE
       
       01  PAGE-CHANGE-FLAG      PIC X(1)    VALUE "N".
           88  PAGE-CHANGE-NO                VALUE "N".
           88  PAGE-CHANGE-YES               VALUE "Y".

       01  SCANKEY-STAT                PIC X(02)     VALUE SPACES.

       01  WS-ORGBR          PIC 9999.

      *    **********************************
      *    *      SCREEN DISPLAY BUFFER
      *    **********************************
       01  DISPLAY-BUF    VALUE SPACES.
           03  F01        PIC Z(6).ZZ.
           03  F02        PIC Z(6).ZZ.
           03  F03        PIC Z(6).ZZ.
           03  F04        PIC ZZZ.
           03  F05        PIC Z(6).ZZ.
           03  F06        PIC Z(6).ZZ.
           03  F061       PIC ZZ9.
           03  F07        PIC Z(6).ZZ.
           03  F08        PIC Z(7).ZZ-.
           03  F09        PIC Z(7).ZZ-.
           03  F091       PIC X.
           03  F10        PIC Z(6).ZZ.
           03  F11        PIC X.
           03  F12        PIC X.
           03  F13        PIC ZZ9.
           03  F131       PIC ZZ9.999999999999.
           03  F132       PIC ZZ9.
           03  F133       PIC X(30).
           03  F14        PIC Z(6).ZZ.
           03  F15        PIC Z(6).ZZ.
           03  F16        PIC Z(4).ZZ.
           03  F17        PIC Z(5).ZZ.
           03  F18        PIC ZZZ.ZZ.
           03  F181       PIC Z(6).ZZ.
           03  F19        PIC ZZ.ZZ.
           03  F20        PIC ZZZ-.
           03  F21        PIC X.
           03  F22        PIC Z(7).ZZ-.
           03  F23        PIC Z(7).ZZ-.
           03  F24        PIC ZZ.
           03  FLNSTAT    PIC X(30).
       01  DISPLAY-LIST.
           03  FILLER     PIC X(20) VALUE
               "010 020 030 040 050 ".
           03  FILLER     PIC X(32) VALUE
               "060 061 070 080 090 091 100 110 ".
           03  FILLER     PIC X(32) VALUE
               "120 130 131 132 133 140 150 160 ".
           03  FILLER     PIC X(43) VALUE
               "170 180 181 190 200 210 220 230 240 LNSTAT.".
       01  UNITPER-LIST   PIC X(8)  VALUE "131 132.".

      *-----------------------------------------------------------------
       01  DISPLAY-KEY-BUF                           VALUE SPACES.
           03  D-KEY-ID                PIC X(01).
      *    03  D-KEY-BRANCH            PIC Z(04).
           03  D-KEY-SOC-SEC           PIC Z(10).

      *-----------------------------------------------------------------
       01  DISPLAY-KEY-LIST.
           03  FILLER PIC X(16) VALUE "KEY-ID KEY-SSNO.".

      *    **********************************
      *    *   ELEMENT SPECIFICATIONS       *
      *    **********************************
       01  MAX            PIC 99   VALUE 24.
       01  SPEC-TABLE.
      *01.
           03  FILLER     PIC X(8) VALUE "EN N062N".
      *02.
           03  FILLER     PIC X(8) VALUE "EN N062N".
      *03.
           03  FILLER     PIC X(8) VALUE "EN N062N".
      *04.
           03  FILLER     PIC X(8) VALUE "EN N030N".
      *05.
           03  FILLER     PIC X(8) VALUE "EN N062N".
      *06.
           03  FILLER     PIC X(8) VALUE "EN N062N".
      *07.
           03  FILLER     PIC X(8) VALUE "EN N062N".
      *08.
           03  FILLER     PIC X(8) VALUE "EN S072N".
      *09.
           03  FILLER     PIC X(8) VALUE "EN S072N".
      *10.
           03  FILLER     PIC X(8) VALUE "EN S062N".
      *11. PD-PROC-DISBURSED
           03  FILLER     PIC X(8) VALUE "E  A010N".
      *12.
           03  FILLER     PIC X(8) VALUE "E  A010N".
      *13.
           03  FILLER     PIC X(8) VALUE "ENNN030N".
      *14.
           03  FILLER     PIC X(8) VALUE "EN N062N".
      *15.
           03  FILLER     PIC X(8) VALUE "EN N062N".
      *16.
           03  FILLER     PIC X(8) VALUE "EN N042N".
      *17.
           03  FILLER     PIC X(8) VALUE "EN N052N".
      *18.
           03  FILLER     PIC X(8) VALUE "EN N032N".
      *19.
           03  FILLER     PIC X(8) VALUE "EN N022N".
      *20.
           03  FILLER     PIC X(8) VALUE "EN N020N".
      *21.
           03  FILLER     PIC X(8) VALUE "E  A010N".
      *22.
           03  FILLER     PIC X(8) VALUE "EN S072N".
      *23.
           03  FILLER     PIC X(8) VALUE "EN S072N".
      *24.
           03  FILLER     PIC X(8) VALUE "ENNN020N".

       01  SPEC-TAB REDEFINES SPEC-TABLE.
           03  SPEC-REC OCCURS 24 TIMES.
               05  SPEC PIC X(7).
               05  CHFG PIC X.

      ***********************************************
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBLP/LPUPERW.CPY".
       COPY "LIBLP/UPERTSTW.CPY".
       COPY "LIBWI/ELSCANW.CPY".
       COPY "LIBMN/PDMAINW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/PDPG03_DEF.CPY".
       COPY "LIBMN/PDPG03_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/ACCESSW.CPY".

       COPY "LIBGB/GETFMW.CPY".
       01 FORM-LINK-AREA    PIC X(2000).

       SCREEN SECTION.
       COPY "LIBMN/PDPG03_SCN.CPY".

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
              MOVE 04  TO FORM-SEL1.

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

           MOVE EXT-CONAME-USER-LOC TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF ( IO-BAD )
              MOVE 9990 TO BR-NO
              PERFORM OPEN-BR-FILE
              PERFORM READ-BR-FILE
              PERFORM CLOSE-BR-FILE
              IF ( IO-BAD )
                 MOVE "BRANCH RECORD MISSING" TO MESS
                 PERFORM SEND-MESS
                 GO TO END-PROGRAM.

       COPY "LIBGB/CHKSEC.CPY".

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "PDPG03" TO VDU-FORM-NAME.
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
           IF PD-SALEFIN = "Y" AND ELE = 8
              GO TO CREATE-ROUTINE.
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
              PERFORM SET-TEST-NOREGPY
              IF NOREG-FG = " "
                 MOVE "RWR" TO IO-TYPE
                 GO TO END-ENTRY-MODULE
              ELSE
                 GO TO MODIFY-ROUTINE.

      * FORM-NUM1 CHECKS FOR PAGES 1 THRU 9
           IF FORM-KEY1 = " " AND FORM-P1 = "P" AND FORM-SEL1 NUMERIC
              IF FORM-SEL1 < 1 OR FORM-SEL1 = 3 OR FORM-SEL1 > 9
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
           PERFORM SET-TEST-NOREGPY.
           IF NOREG-FG = " "
              MOVE "RWR" TO IO-TYPE
              GO TO EXIT-ENTRY-MODULE
           ELSE
              GO TO MODIFY-ROUTINE.

       CHECK-CHNO.
           INSPECT ACTION REPLACING ALL SPACES
              BY ZEROS.
           IF CHNO-X NOT NUMERIC
              GO TO MODIFY-ROUTINE.
           IF CHNO = 0
              GO TO MODIFY-ROUTINE.
           IF CHNO > MAX
              GO TO MODIFY-ROUTINE.
           IF (CHNO = 8 AND PD-SALEFIN = "Y")
               GO TO MODIFY-ROUTINE.
           MOVE CHNO TO ELE.

       ENTER-ELE.
           IF ELE = 11 OR 20
              GO TO CREATE-ROUTINE.

           IF ELE = 22
              IF BR-LC-CODE NOT = "Y"
                 GO TO CREATE-ROUTINE.

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
       TEST-F6.
           IF VDUSTATUS = "1<"
              IF ELE = 24
                 PERFORM CODE-SCAN
                 IF IO-FG = 0
                    GO TO GOTO-FIELD
                 ELSE
                    GO TO ENTER-ELE.
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
                 A-21 A-22 A-23 A-24
                 DEPENDING ON ELE.
           GO TO END-ENTRY-MODULE.

       A-01.
           MOVE VDUNUM2 TO PD-SELLPR.
           GO TO CREATE-ROUTINE.
       A-02.
           MOVE VDUNUM2 TO PD-DOWNPY.
           GO TO CREATE-ROUTINE.
       A-03.
           MOVE VDUNUM2 TO PD-LNAMT.
           IF PD-LNAMT = 0
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-04.
           IF VDUNUM0 < PD-ANTICTERM
              MOVE " LESS THAN ANTICERN(1) TERM" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           MOVE VDUNUM0 TO PD-ORGTERM.
           IF NEW-FLAG NOT = 1
              PERFORM ALARM
              PERFORM SET-TEST-NOREGPY.
           GO TO CREATE-ROUTINE.
       A-05.
           IF VDUNUM2 = 0
              GO TO ENTER-ELE.
           MOVE VDUNUM2 TO PD-1STPYAMT.
           GO TO CREATE-ROUTINE.
       A-06.
           MOVE VDUNUM2 TO PD-REGPYAMT.
           IF NEW-FLAG NOT = 1
              PERFORM ALARM
              PERFORM SET-TEST-NOREGPY.
           GO TO CREATE-ROUTINE.
       A-07.
           MOVE VDUNUM2 TO PD-LASTPYAMT.
           PERFORM ALARM.
           PERFORM SET-TEST-NOREGPY.
           GO TO CREATE-ROUTINE.
       A-08.
           MOVE VDUNUM2 TO PD-PROCEEDS.
           GO TO CREATE-ROUTINE.
       A-09.
           MOVE VDUNUM2 TO PD-PROCASH.
       A-091.
           MOVE "E  A010091." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO A-091.
           MOVE VDUBUF TO PD-TILLNO.
           IF PD-TILLNO = SPACES
              GO TO A-091.
           GO TO CREATE-ROUTINE.
       A-10.
           MOVE VDUNUM2 TO PD-TRADEIN.
           GO TO CREATE-ROUTINE.
       A-11.
           MOVE VDUBUF TO PD-PROC-DISBURSED.
           IF NOT (PD-PROC-DISBURSED = "Y" OR "N")
              MOVE "ENTER 'Y' OR 'N'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-12.
           MOVE VDUBUF TO PD-UNITPER-CD
                          UPERTST-UNITPER-CD.
           PERFORM UPERTST-UNITPER-CODE.
           IF UPERTST-ERRCD NOT = " "
              GO TO ENTER-ELE.
           IF NEW-FLAG NOT = 1
              MOVE 13 TO ELE
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-13.
      D    STOP MESS.
           MOVE VDUNUM0 TO UPERTST-UNITPER-FREQ PD-UNITPER-FREQ.
           MOVE PD-UNITPER-CD TO UPERTST-UNITPER-CD.
           PERFORM UPERTST-UNITPER-FREQUENCY.
           IF UPERTST-ERRCD NOT = " "
              GO TO ENTER-ELE.

           MOVE PD-UNITPER-CD TO UPER-UNITPER-CD.
           MOVE PD-UNITPER-FREQ TO UPER-UNITPER-FREQ.
           MOVE PD-INTDATE TO UPER-INTDATE.
           MOVE PD-ORIG-1STPYDATE TO UPER-1STPYDATE.
           MOVE PD-ORGTERM TO UPER-TERM.
           PERFORM UPER-CALCULATION.
           IF UPER-ERRCD NOT = " "
              GO TO ENTER-ELE.

           MOVE UPER-UNITPER-PER-YEAR TO PD-UNITPER-PER-YEAR
                                         F131.
           MOVE UPER-DAYS-IN-UNITPER TO PD-DAYS-IN-UNITPER
                                        F132.
           MOVE F131 TO WR-BUF.
           MOVE UNITPER-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           PERFORM UPERTST-SET-MESS.
           MOVE "S  A300133." TO VDU.
           MOVE UPERTST-MESS TO VDUBUF.
           PERFORM SCREENIO.
           GO TO CREATE-ROUTINE.
       A-14.
           MOVE VDUNUM2 TO PD-FINCHG.
           GO TO CREATE-ROUTINE.
       A-15.
           MOVE VDUNUM2 TO PD-INTCHG.
           GO TO CREATE-ROUTINE.
       A-16.
           MOVE VDUNUM2 TO PD-EXTCHG.
           GO TO CREATE-ROUTINE.
       A-17.
           MOVE VDUNUM2 TO PD-SERCHG.
           GO TO CREATE-ROUTINE.
       A-18.
           DIVIDE VDUNUM2 BY PD-ORGTERM GIVING PD-MAINTFEE
                                          REMAINDER AMT-HOLDER.
           IF AMT-HOLDER NOT = 0
              MOVE "FEE MUST BE EVENLY DIVISABLE BY TERM" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-19.
           MOVE VDUNUM2 TO PD-POINTS.
           GO TO CREATE-ROUTINE.
       A-20.
           GO TO CREATE-ROUTINE.
       A-21.
           IF VDUBUF NOT = "A"
                  AND NOT = " "
              MOVE "ENTER: 'A' OR SPACE ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           MOVE VDUBUF TO PD-STATE-FLAG.
           GO TO CREATE-ROUTINE.
       A-22.
           MOVE VDUNUM2 TO PD-STATE-WORKER.
           GO TO CREATE-ROUTINE.
       A-23.
           MOVE VDUNUM2 TO PD-STATE-WORKER2.
           GO TO CREATE-ROUTINE.
       A-24.
           MOVE VDUNUM0 TO PD-LC-CODE.
           PERFORM VERIFY-EL-CODE.
           IF IO-FG NOT = 0 AND PD-LC-CODE NOT = 0
              MOVE "INVALID CODE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.

       END-ENTRY-MODULE.
           MOVE 0 TO FORM-SEL1
                     FORM-SEL2.
       EXIT-ENTRY-MODULE.
           EXIT.

      ******************************************************************
       CODE-SCAN SECTION.
      ******************************************************************

           MOVE "EL"   TO ELSCAN-TYPE.
           MOVE ZEROES TO ELSCAN-CODE.
           MOVE "WI/ELSCAN " TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ELSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.

           IF ( ELSCAN-STATUS NOT = "00") OR (ELSCAN-CODE = 0 )
               MOVE 9 TO IO-FG
               GO TO EXIT-CODE-SCAN.

           MOVE "SNNN020220." TO VDU.
           MOVE ELSCAN-CODE   TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE ZEROES        TO IO-FG.
           MOVE ELSCAN-CODE   TO VDUNUM0.

       EXIT-CODE-SCAN.
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
           MOVE PD-SELLPR TO F01.
           MOVE PD-DOWNPY TO F02.
           MOVE PD-LNAMT TO F03.
           MOVE PD-ORGTERM TO F04.
           MOVE PD-1STPYAMT TO F05.
           MOVE PD-REGPYAMT TO F06.
           MOVE PD-NOREGPY TO F061.
           MOVE PD-LASTPYAMT TO F07.

           MOVE PD-PROCEEDS TO F08.
           MOVE PD-PROCASH TO F09.
           MOVE PD-TILLNO TO F091.
           MOVE PD-TRADEIN TO F10.
           MOVE PD-PROC-DISBURSED TO F11.
           MOVE PD-UNITPER-CD TO F12.
           MOVE PD-UNITPER-FREQ TO F13.
           MOVE PD-UNITPER-PER-YEAR TO F131.
           MOVE PD-DAYS-IN-UNITPER TO F132.
           MOVE PD-FINCHG TO F14.
           MOVE PD-INTCHG TO F15.
           MOVE PD-EXTCHG TO F16.
           MOVE PD-SERCHG TO F17.
           MULTIPLY PD-MAINTFEE BY PD-ORGTERM GIVING F18.
           MOVE 0 TO TOTAL-FEE-INS-FINCHG.
           PERFORM GET-FEE-INS-FINCHG
                VARYING SUB FROM 1 BY 1
                  UNTIL SUB > 10.
           MOVE TOTAL-FEE-INS-FINCHG TO F181.
           MOVE PD-POINTS TO F19.
           MOVE PD-STATE-FLAG TO F21.
           MOVE PD-STATE-WORKER TO F22.
           MOVE PD-STATE-WORKER2 TO F23.
           MOVE PD-LC-CODE TO F24.
           MOVE PD-ORGBR TO WS-ORGBR.

           STRING PD-ORGST, "-", WS-ORGBR, "-", PD-SPRCLASS, "(",
                  PD-CLASS, ")", "-", PD-SUBCLASS, "-", PD-LAWCODE,
                  "-", PD-SMALLOAN, PD-SALEFIN, PD-SERVACCT, PD-LOANTYPE
                  DELIMITED BY "  " INTO FLNSTAT.

           MOVE PD-UNITPER-CD TO UPERTST-UNITPER-CD.
           MOVE PD-UNITPER-FREQ TO UPERTST-UNITPER-FREQ.
           PERFORM UPERTST-SET-MESS.
           MOVE UPERTST-MESS TO F133.

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

      ******************************************************************
       GET-FEE-INS-FINCHG SECTION.
      ******************************************************************
           IF SUB < 9
              IF PD-INSOURS(SUB) NOT = "M"
                 IF PD-INSFINCHG(SUB) = "Y"
                    ADD PD-INSPREM(SUB) TO TOTAL-FEE-INS-FINCHG.
           IF PD-FEEOURS(SUB) NOT = "M"
              IF PD-FEEFINCHG(SUB) = "Y"
                 ADD PD-FEEAMT(SUB) TO TOTAL-FEE-INS-FINCHG.

      *******************************************
       SET-TEST-NOREGPY SECTION.
      *******************************************
           MOVE " " TO NOREG-FG.

           SUBTRACT 1 FROM PD-ORGTERM GIVING PD-NOREGPY.
           IF PD-LASTPYAMT NOT = 0
              SUBTRACT 1 FROM PD-NOREGPY.
           MOVE "SNNN030061." TO VDU.
           MOVE PD-NOREGPY TO VDUNUM0.
           PERFORM SCREENIO.
           IF PD-ORGTERM = 1
              IF PD-LASTPYAMT NOT = 0
                 MOVE "E" TO NOREG-FG
                 MOVE "LAST PAYMENT MUST BE ZERO" TO MESS
                 PERFORM SEND-MESS.
           IF PD-ORGTERM > 1
              IF NOT (PD-1STPYAMT > 0
                          AND PD-REGPYAMT > 0
                            AND PD-LASTPYAMT > 0
                     )
                 MOVE "E" TO NOREG-FG
                 MOVE "FIRST, REGULAR AND LAST PAYMENTS CAN'T BE ZERO"
                                                               TO MESS
                 PERFORM SEND-MESS.

       SET-TEST-NOREGPY-EXIT.
           EXIT.

      **************************************************************************
       FORM-RESET SECTION.
      **************************************************************************
           MOVE EXT-CONAME-CONAME TO PDPG03-CONAME.
           MOVE EXT-CONAME-SYSDATE TO PDPG03-SYSDATE.
           DISPLAY SPACES UPON PDMAIN-WINDOW-HANDLE
                               LINE 0 COL 0 ERASE SCREEN.
           DISPLAY PDPG03-SCREEN UPON PDMAIN-WINDOW-HANDLE.
           MOVE PDPG03-FORM-FIELDS TO WR-BUF.
           MOVE PDPG03-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **************************************************************************
       SETUP-LINK-INFO SECTION.
      **************************************************************************
           MOVE PDPG03-HELP--FILE TO HELP-LINK-FILE.
           MOVE PDPG03-HELP--DIR TO HELP-LINK-DIR.

      **************************************************************************
       VDU-CONVERT-FIELD SECTION.
      **************************************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/PDPG03_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/LPUPER.CPY".
       COPY "LIBLP/UPERTST.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/ERRMSG.CPY".

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
       VERIFY-EL-CODE.
           PERFORM OPEN-CD1-FILE.
           MOVE "EL"       TO CD-TYPE.
           MOVE PD-LC-CODE TO CD-EL-CODE.
           MOVE SPACES     TO CD-EL-FILLER.
           PERFORM READ-CD1-FILE.
           PERFORM CLOSE-CD1-FILE.

      **************************************************************************
       IO-ROUTINES SECTION.
      **************************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-PD1-FILE.
           PERFORM CLOSE-CD1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPPDGS_SQL.CPY".
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBLP/LPPD1IN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPCD1RN.CPY".

       COPY "LIBGB/FERRORS.CPY".

      **************************************************************************
       END-PROGRAM SECTION.
      **************************************************************************

           PERFORM CLOSE-FILES.

       EXIT-PROG.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY PDPG03-SCREEN.
           MOVE FORM-PATH TO FORM-PATHNAME.
           MOVE PDMAIN-LINK TO FORM-LINK-AREA.
           EXIT PROGRAM.
