       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PDPG91.
       AUTHOR.        MG.
       DATE-WRITTEN.  09-13-2000.
      ******************************************************************
      *
      * IDENTIFICATION: MN/PDPG91.C
      *
      * DESCRIPTION   : SCRATCH PAD FILE MAINTENANCE - PAGE 91
      *
      *-----------------------------------------------------------------
      * REV:
      * MG  000913 {PR#00000} PARADATA FINANCIAL [BARB & BETH]
      *             ***NEW*** ADDED PER BARB & BETH.
      * MJD 131127 A30 CONVERSION COMPLETED
      *
      * KEC 2017.0710 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          ADDED 'PD' FILE PREFIX FOR WORKING STORAGE FIELDS.
      *
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

      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E - S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)MN/PDPG91.CBL S34 06/06/21-16:29:39 >".

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
      *
      *    M I S C E L L A N E O U S - W O R K E R S
      *
      ******************************************************************
       01  FIRST-SCAN               PIC 9(01)        VALUE ZEROES.

       01  IO-TYPE                  PIC X(03)        VALUE SPACES.
       01  NEW-FLAG                 PIC 9(01)        VALUE ZEROES.

       01  NONCOMP-PD1-KEY.
           03  NONCOMP-PD-ID        PIC X(01)        VALUE SPACES.
           03  NONCOMP-PD-BRANCH    PIC 9(04)        VALUE ZEROES.
           03  NONCOMP-PD-SOC-SEC   PIC 9(10)        VALUE ZEROES.

       01  ONE                      PIC S9(1).

      *    'PAGE-CHANGE-FLAG' IS FOR KNOWING IF COMING FROM ANOTHER PAGE
       01  PAGE-CHANGE-FLAG         PIC X(01)        VALUE "N".
           88  PAGE-CHANGE-NO                        VALUE "N".
           88  PAGE-CHANGE-YES                       VALUE "Y".

       01  SCANKEY-STAT             PIC X(02)        VALUE SPACES.
       01  WS-ORGBR                 PIC 9(04)        VALUE ZEROES.

       01  DISPLAY-BUF                VALUE SPACES.
           03  F01                    PIC 9(06).99.
           03  F02                    PIC X(01).
           03  F03                    PIC X(01).
           03  F04                    PIC X(01).
           03  F05                    PIC X(01).
           03  F06                    PIC X(01).
           03  F07                    PIC X(01).
           03  F08                    PIC X(01).
           03  F09                    PIC X(01).
           03  F10                    PIC X(01).
           03  F11                    PIC X(01).
           03  F12                    PIC X(01).
           03  F13                    PIC X(01).
           03  F14                    PIC X(01).
           03  F15                    PIC X(01).
           03  F16                    PIC X(01).
           03  F17                    PIC X(01).
           03  F18                    PIC X(01).
           03  F19                    PIC X(02).
           03  F20                    PIC X(02).
           03  F21                    PIC X(02).
           03  F22                    PIC X(02).
           03  F23                    PIC X(02).
           03  F24                    PIC X(02).
           03  F25                    PIC X(02).
           03  F26                    PIC X(02).
           03  F27                    PIC X(02).
           03  F28                    PIC X(02).
           03  F29                    PIC 9(07).99-.
           03  F30                    PIC 9(07).99-.
           03  F31                    PIC 9(07).99-.
           03  F32                    PIC 9(07).99-.
           03  F33                    PIC 99.99.
           03  F34                    PIC X(01).
           03  FLNSTAT                PIC X(30).

      *-----------------------------------------------------------------
       01  DISPLAY-LIST.
           03  FILLER PIC X(20) VALUE "010 020 030 040 050 ".
           03  FILLER PIC X(20) VALUE "060 070 080 090 100 ".
           03  FILLER PIC X(20) VALUE "110 120 130 140 150 ".
           03  FILLER PIC X(20) VALUE "160 170 180 190 200 ".
           03  FILLER PIC X(20) VALUE "210 220 230 240 250 ".
           03  FILLER PIC X(20) VALUE "260 270 280 290 300 ".
           03  FILLER PIC X(16) VALUE "310 320 330 340 ".
           03  FILLER PIC X(07) VALUE "LNSTAT.".

      *-----------------------------------------------------------------
       01  DISPLAY-KEY-BUF                           VALUE SPACES.
           03  D-KEY-ID                PIC X(01).
           03  D-KEY-SOC-SEC           PIC Z(10).

      *-----------------------------------------------------------------
       01  DISPLAY-KEY-LIST.
           03  FILLER PIC X(16) VALUE "KEY-ID KEY-SSNO.".

       78  MAX                      VALUE 34.
       01  SPEC-TABLE.
           03  FILLER                  PIC X(08)     VALUE "ENNN062 ".
           03  FILLER                  PIC X(08)     VALUE "E  A010 ".
           03  FILLER                  PIC X(08)     VALUE "E  A010 ".
           03  FILLER                  PIC X(08)     VALUE "E  A010 ".
      * 05
           03  FILLER                  PIC X(08)     VALUE "E  A010 ".
           03  FILLER                  PIC X(08)     VALUE "E  A010 ".
           03  FILLER                  PIC X(08)     VALUE "E  A010 ".
           03  FILLER                  PIC X(08)     VALUE "E  A010 ".
           03  FILLER                  PIC X(08)     VALUE "E  A010 ".
      * 10
           03  FILLER                  PIC X(08)     VALUE "E  A010 ".
           03  FILLER                  PIC X(08)     VALUE "E  A010 ".
           03  FILLER                  PIC X(08)     VALUE "E  A010 ".
           03  FILLER                  PIC X(08)     VALUE "E  A010 ".
           03  FILLER                  PIC X(08)     VALUE "E  A010 ".
      * 15
           03  FILLER                  PIC X(08)     VALUE "E  A010 ".
           03  FILLER                  PIC X(08)     VALUE "E  A010 ".
           03  FILLER                  PIC X(08)     VALUE "E  A010 ".
           03  FILLER                  PIC X(08)     VALUE "E  A010 ".
           03  FILLER                  PIC X(08)     VALUE "E  A020 ".
      * 20
           03  FILLER                  PIC X(08)     VALUE "E  A020 ".
           03  FILLER                  PIC X(08)     VALUE "E  A020 ".
           03  FILLER                  PIC X(08)     VALUE "E  A020 ".
           03  FILLER                  PIC X(08)     VALUE "E  A020 ".
           03  FILLER                  PIC X(08)     VALUE "E  A020 ".
      * 25
           03  FILLER                  PIC X(08)     VALUE "E  A020 ".
           03  FILLER                  PIC X(08)     VALUE "E  A020 ".
           03  FILLER                  PIC X(08)     VALUE "E  A020 ".
           03  FILLER                  PIC X(08)     VALUE "E  A020 ".
           03  FILLER                  PIC X(08)     VALUE "ENNS072 ".
      * 30
           03  FILLER                  PIC X(08)     VALUE "ENNS072 ".
           03  FILLER                  PIC X(08)     VALUE "ENNS072 ".
           03  FILLER                  PIC X(08)     VALUE "ENNS072 ".
           03  FILLER                  PIC X(08)     VALUE "ENNN022 ".
      * 34
           03  FILLER                  PIC X(08)     VALUE "E  A010 ".

      *-----------------------------------------------------------------
       01  SPEC-TAB            REDEFINES SPEC-TABLE.
           03  SPEC-REC                  OCCURS MAX TIMES.
               05  SPEC                  PIC X(07).
               05  CHFG                  PIC X(01).

       01  ELE                           PIC 9(02)     VALUE ZEROES.
           88  ELE-NOT-AVAILABLE                       VALUE 0.

      *
      *    C O P Y - L I B R A R Y - W O R K E R S
      *
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBMN/PDMAINW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/PDPG91_DEF.CPY".
       COPY "LIBMN/PDPG91_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/ACCESSW.CPY".

       COPY "LIBGB/GETFMW.CPY".
       01 FORM-LINK-AREA    PIC X(2000).

       SCREEN SECTION.
       COPY "LIBMN/PDPG91_SCN.CPY".

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
              MOVE "N"    TO NEW-REC
              MOVE "P"    TO FORM-P1
              MOVE 1      TO FORM-SEL1
              MOVE SPACES TO SAVEKEY.

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
           MOVE "PDPG91" TO VDU-FORM-NAME.
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
           MOVE 1      TO ONE.
           MOVE ZEROES TO ELE.

       CREATE-ROUTINE.

           IF ( NEW-FLAG NOT = 1 )
              GO TO MODIFY-ROUTINE.

           ADD ONE TO ELE.

           IF ( ELE     < 1       ) MOVE 1 TO ELE ONE.
           IF ( ELE-NOT-AVAILABLE ) GO TO CREATE-ROUTINE.
           IF ( ELE NOT > MAX     ) GO TO ENTER-ELE.

           MOVE 2 TO NEW-FLAG.

      *-----------------------------------------------------------------
       MODIFY-ROUTINE.

           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.

           IF ( VDUSTATUS = "10" )
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

           IF ( VDUSTATUS NOT = "00" )
              GO TO MODIFY-ROUTINE.

           MOVE VDUBUF TO ACTION.

           IF ( ACTION = SPACES )
              MOVE "RWR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

      * FORM-NUM1 CHECKS FOR PAGES 1 THRU 9
           IF ( FORM-KEY1 = SPACES ) AND
              ( FORM-P1   = "P"    ) AND
              ( FORM-SEL1 NUMERIC  )
              IF ( FORM-SEL1 < 1 ) OR ( FORM-SEL1 > 9 )
                 GO TO MODIFY-ROUTINE
              ELSE
                 GO TO CHECK-PAGE.

      * FORM-NUM2 CHECKS FOR PAGES 10 THRU 99
           IF ( FORM-KEY2 = SPACES ) AND
              ( FORM-P2   = "P"    ) AND
              ( FORM-SEL2 NUMERIC  )
              IF ( FORM-CURRENT-PAGE-91 ) OR ( NOT FORM-2-DIGITS )
                 GO TO MODIFY-ROUTINE
              ELSE
                 GO TO CHECK-PAGE.

           GO TO CHECK-CHNO.

      *-----------------------------------------------------------------
       CHECK-PAGE.
           MOVE "RWR" TO IO-TYPE.
           GO TO EXIT-ENTRY-MODULE.

      *-----------------------------------------------------------------
       CHECK-CHNO.
           INSPECT ACTION REPLACING ALL SPACES BY ZEROS.

           IF ( CHNO-X NOT NUMERIC )
              GO TO MODIFY-ROUTINE.

           IF ( CHNO = ZEROES )
              GO TO MODIFY-ROUTINE.

           IF ( CHNO > MAX )
              GO TO MODIFY-ROUTINE.

           MOVE CHNO TO ELE.

      *-----------------------------------------------------------------
       ENTER-ELE.

           IF ( ELE-NOT-AVAILABLE )
              GO TO MODIFY-ROUTINE.

           IF ( CHKEY    NOT = ZEROES ) AND
              ( CHFG(ELE)    = SPACES ) AND
              ( NEW-FLAG NOT = 1      )
              GO TO MODIFY-ROUTINE.

           IF ( NEW-FLAG = ZEROES ) AND
              ( CHFG(ELE) = "N"   )
              IF ( NEW-FLAG = 1 )
                 GO TO CREATE-ROUTINE
              ELSE
              IF ( CHKEY NOT = EXT-CONAME-DAILY-PASSWORD )
                 GO TO MODIFY-ROUTINE.

      *-----------------------------------------------------------------
      *    ENTER ON SELECTED FIELD.
      *-----------------------------------------------------------------
           MOVE SPEC(ELE) TO VDU.
           MOVE ELE       TO VDUROW.
           MOVE ZEROES    TO VDUCOL.
           MOVE "."       TO VDUDECIMAL.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       TEST-F1-KEY.

           IF ( NOT F1-KEY ) GO TO TEST-F2-KEY.

           MOVE "CLR" TO IO-TYPE
           GO TO END-ENTRY-MODULE.

      *-----------------------------------------------------------------
       TEST-F2-KEY.

           IF ( NOT F2-KEY ) GO TO TEST-F3-KEY.

      *-----------------------------------------------------------------
       TEST-F3-KEY.

           IF ( NOT F3-KEY ) GO TO TEST-F4-KEY.

      *-----------------------------------------------------------------
       TEST-F4-KEY.

           IF ( NOT F4-KEY ) GO TO TEST-F5-KEY.

      *-----------------------------------------------------------------
       TEST-F5-KEY.

           IF ( NOT F5-KEY ) GO TO TEST-F6-KEY.

      *-----------------------------------------------------------------
       TEST-F6-KEY.

           IF ( NOT F6-KEY ) GO TO TEST-F7-KEY.

      *-----------------------------------------------------------------
       TEST-F7-KEY.

           IF ( NOT F7-KEY ) GO TO TEST-F8-KEY.

      *-----------------------------------------------------------------
       TEST-F8-KEY.

           IF ( NOT F8-KEY ) GO TO TEST-F9-KEY.

      *-----------------------------------------------------------------
       TEST-F9-KEY.

           IF ( NOT F9-KEY ) GO TO TEST-F10-KEY.

      *-----------------------------------------------------------------
       TEST-F10-KEY.

           IF ( NOT F10-KEY ) GO TO TEST-F11-KEY.

      *-----------------------------------------------------------------
       TEST-F11-KEY.

           IF ( NOT F11-KEY ) GO TO TEST-F12-KEY.

      *-----------------------------------------------------------------
       TEST-F12-KEY.

           IF ( NOT F12-KEY ) GO TO TEST-DOWN-KEY.

      *-----------------------------------------------------------------
       TEST-DOWN-KEY.

           IF ( NOT DOWN-KEY ) GO TO TEST-UP-KEY.

           MOVE 1 TO ONE
           GO TO GOTO-FIELD.

      *-----------------------------------------------------------------
       TEST-UP-KEY.

           IF ( NOT UP-KEY ) GO TO TEST-ENTER-KEY.

           IF ( NEW-FLAG = 1 )
              MOVE -1 TO ONE
              GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       TEST-ENTER-KEY.

           IF ( NOT ENTER-KEY ) GO TO ENTER-ELE.

           MOVE 1 TO ONE.

      *-----------------------------------------------------------------
       GOTO-FIELD.

           GO TO F-01 F-02 F-03 F-04 F-05 F-06 F-07 F-08 F-09 F-10
                 F-11 F-12 F-13 F-14 F-15 F-16 F-17 F-18 F-19 F-20
                 F-21 F-22 F-23 F-24 F-25 F-26 F-27 F-28 F-29 F-30
                 F-31 F-32 F-33 F-34
                 DEPENDING ON ELE.

           GO TO END-ENTRY-MODULE.

      *
      *    S E T - R E C O R D - E L E M E N T S
      *
       F-01.
           MOVE VDUNUM2 TO PD-FINAMT.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-02.
           MOVE VDUBUF TO PD-PROCEEDS-FG.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-03.
           MOVE VDUBUF TO PD-POINTS-FG.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-04.
           MOVE VDUBUF TO PD-OVRDRATE-FG.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-05.
           MOVE VDUBUF TO PD-INS-FG(1).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-06.
           MOVE VDUBUF TO PD-INS-FG(2).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-07.
           MOVE VDUBUF TO PD-INS-FG(3).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-08.
           MOVE VDUBUF TO PD-INS-FG(4).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-09.
           MOVE VDUBUF TO PD-INS-FG(5).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-10.
           MOVE VDUBUF TO PD-INS-FG(6).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-11.
           MOVE VDUBUF TO PD-INS-FG(7).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-12.
           MOVE VDUBUF TO PD-INS-FG(8).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-13.
           MOVE VDUBUF TO PD-INS-FG(9).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-14.
           MOVE VDUBUF TO PD-INS-FG(10).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-15.
           MOVE VDUBUF TO PD-SERCHG-FG.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-16.
           MOVE VDUBUF TO PD-MAINTFEE-FG.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-17.
           MOVE VDUBUF TO PD-EXTCHG-FG.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-18.
           MOVE VDUBUF TO PD-REGPYAMT-FG.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-19.
           MOVE VDUBUF TO PD-FEECD(1).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-20.
           MOVE VDUBUF TO PD-FEECD(2).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-21.
           MOVE VDUBUF TO PD-FEECD(3).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-22.
           MOVE VDUBUF TO PD-FEECD(4).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-23.
           MOVE VDUBUF TO PD-FEECD(5).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-24.
           MOVE VDUBUF TO PD-FEECD(6).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-25.
           MOVE VDUBUF TO PD-FEECD(7).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-26.
           MOVE VDUBUF TO PD-FEECD(8).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-27.
           MOVE VDUBUF TO PD-FEECD(9).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-28.
           MOVE VDUBUF TO PD-FEECD(10).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-29.
           MOVE VDUNUM2 TO PD-PRPAID-AHEAD.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-30.
           MOVE VDUNUM2 TO PD-RESTRICT-ADJ.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-31.
           MOVE VDUNUM2 TO PD-DUAL-LOAN-ADJ.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-32.
           MOVE VDUNUM2 TO PD-CAN-LOAN-MAX.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-33.
           MOVE VDUNUM2 TO PD-PERDOWN.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-34.
           MOVE VDUBUF TO PD-SELLPR-FG.
           GO TO CREATE-ROUTINE.

       END-ENTRY-MODULE.

           MOVE ZEROES TO FORM-SEL1 FORM-SEL2.

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
           MOVE PD-ID           TO NONCOMP-PD-ID.
           MOVE PD-BRANCH       TO NONCOMP-PD-BRANCH.
           MOVE PD-SOC-SEC      TO NONCOMP-PD-SOC-SEC.
           MOVE NONCOMP-PD1-KEY TO ERRLOGW-KEY.
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

           MOVE PD-ORGBR                  TO WS-ORGBR.
           MOVE SPACES                    TO DISPLAY-BUF.
           MOVE PD-FINAMT                 TO F01.
           MOVE PD-PROCEEDS-FG            TO F02.
           MOVE PD-POINTS-FG              TO F03.
           MOVE PD-OVRDRATE-FG            TO F04.
           MOVE PD-INS-FG (1)             TO F05.
           MOVE PD-INS-FG (2)             TO F06.
           MOVE PD-INS-FG (3)             TO F07.
           MOVE PD-INS-FG (4)             TO F08.
           MOVE PD-INS-FG (5)             TO F09.
           MOVE PD-INS-FG (6)             TO F10.
           MOVE PD-INS-FG (7)             TO F11.
           MOVE PD-INS-FG (8)             TO F12.
           MOVE PD-INS-FG (9)             TO F13.
           MOVE PD-INS-FG(10)             TO F14.
           MOVE PD-SERCHG-FG              TO F15.
           MOVE PD-MAINTFEE-FG            TO F16.
           MOVE PD-EXTCHG-FG              TO F17.
           MOVE PD-REGPYAMT-FG            TO F18.
           MOVE PD-FEECD (1)              TO F19.
           MOVE PD-FEECD (2)              TO F20.
           MOVE PD-FEECD (3)              TO F21.
           MOVE PD-FEECD (4)              TO F22.
           MOVE PD-FEECD (5)              TO F23.
           MOVE PD-FEECD (6)              TO F24.
           MOVE PD-FEECD (7)              TO F25.
           MOVE PD-FEECD (8)              TO F26.
           MOVE PD-FEECD (9)              TO F27.
           MOVE PD-FEECD(10)              TO F28.
           MOVE PD-PRPAID-AHEAD           TO F29.
           MOVE PD-RESTRICT-ADJ           TO F30.
           MOVE PD-DUAL-LOAN-ADJ          TO F31.
           MOVE PD-CAN-LOAN-MAX           TO F32.
           MOVE PD-PERDOWN                TO F33.
           MOVE PD-SELLPR-FG              TO F34.

      * DISPLAYING THE LOAN STATUS
           STRING PD-ORGST, "-", WS-ORGBR, "-", PD-SPRCLASS, "(",
                  PD-CLASS, ")", "-", PD-SUBCLASS, "-", PD-LAWCODE,
                  "-", PD-SMALLOAN, PD-SALEFIN, PD-SERVACCT, PD-LOANTYPE
                  DELIMITED BY "  " INTO FLNSTAT.

       END-DISPLAY-FIELDS.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       EXIT-DISPLAY-FIELDS.
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
           MOVE EXT-CONAME-CONAME TO PDPG91-CONAME.
           MOVE EXT-CONAME-SYSDATE TO PDPG91-SYSDATE.
           DISPLAY SPACES UPON PDMAIN-WINDOW-HANDLE
                               LINE 0 COL 0 ERASE SCREEN.
           DISPLAY PDPG91-SCREEN UPON PDMAIN-WINDOW-HANDLE.
           MOVE PDPG91-FORM-FIELDS TO WR-BUF.
           MOVE PDPG91-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **************************************************************************
       SETUP-LINK-INFO SECTION.
      **************************************************************************
           MOVE PDPG91-HELP--FILE TO HELP-LINK-FILE.
           MOVE PDPG91-HELP--DIR TO HELP-LINK-DIR.

      **************************************************************************
       VDU-CONVERT-FIELD SECTION.
      **************************************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/PDPG91_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".

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
           DESTROY PDPG91-SCREEN.
           MOVE FORM-PATH TO FORM-PATHNAME.
           MOVE PDMAIN-LINK TO FORM-LINK-AREA.

           EXIT PROGRAM.
