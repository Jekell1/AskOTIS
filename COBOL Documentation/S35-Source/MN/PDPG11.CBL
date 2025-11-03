       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PDPG11.
       AUTHOR.        KEC.
       DATE-WRITTEN.  09-12-2000.
      ******************************************************************
      *
      * IDENTIFICATION: MN/PDPG11.C
      *
      * DESCRIPTION   : SCRATCH PAD FILE MAINTENANCE - PAGE 11
      *
      *-----------------------------------------------------------------
      * REV:
      * KEC 000912 {PR#00000} PARADATA FINANCIAL [BARB & BETH]
      *             ***NEW*** ADDED PER BARB & BETH.
      * MJD 131127 A30 CONVERSION COMPLETED
      * BAH 161018 A30 NEW PAGE 11, REMOVED ALL AGEING STUFF
      *  CS 170504 ACTIVATE 8 DIGIT DATES
      *
      * KEC 2017.0710 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          ADDED 'PD' FILE PREFIX FOR WORKING STORAGE FIELDS.
      *
      * BAH 190114 GLOBAL PDFILE
      * BAH 20190627 REMOVED REVOLVING FIELDS 9 - 13, PT0153
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

      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E - S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)MN/PDPG11.CBL S35 02/21/24-12:30:22 >".

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
       01  CURR-DATE                                 VALUE ZEROES.
           03  CURR-MO              PIC 9(02).
           03  CURR-DAY             PIC 9(02).
           03  CURR-YR              PIC 9(02).

       01  FIRST-SCAN               PIC 9(01)        VALUE ZEROES.

       01  IO-TYPE                  PIC X(03)        VALUE SPACES.
       01  NEW-FLAG                 PIC 9(01)        VALUE ZEROES.

       01  ONE                      PIC S9(1).

      * PAGE-CHANGE-FLAG IS FOR KNOWING IF COMING FROM ANOTHER PAGE
       01  PAGE-CHANGE-FLAG         PIC X(01)        VALUE "N".
           88  PAGE-CHANGE-NO                        VALUE "N".
           88  PAGE-CHANGE-YES                       VALUE "Y".


       01  SCANKEY-STAT             PIC X(02)        VALUE SPACES.

      *
      *    S C R E E N - D I S P L A Y - B U F F E R
      *
       01  DISPLAY-BUF                VALUE SPACES.
           03  F01                    PIC X.
           03  F02                    PIC Z(6)9.99-   BLANK ZERO.
           03  F03                    PIC Z(6)9.99-   BLANK ZERO.
           03  F04                    PIC X.
           03  F05                    PIC 9(8)        BLANK ZERO.
           03  F06                    PIC X.
           03  F07                    PIC X.
           03  F08                    PIC 9(3)        BLANK ZERO.
           03  F09                    PIC 99/99/99    BLANK ZERO.
           03  F10                    PIC 99/99/99    BLANK ZERO.
           03  F11                    PIC X.
           03  F12                    PIC X.
           03  F13                    PIC 9(3)        BLANK ZERO.

      *-----------------------------------------------------------------
       01  DISPLAY-LIST.
           03  FILLER PIC X(28) VALUE "010 020 030 040 050 060 070 ".
           03  FILLER PIC X(24) VALUE "080 090 100 110 120 130.".

      *-----------------------------------------------------------------
       01  DISPLAY-KEY-BUF                           VALUE SPACES.
           03  D-KEY-ID                PIC X(01).
      *    03  D-KEY-BRANCH            PIC Z(04).
           03  D-KEY-SOC-SEC           PIC Z(10).

      *-----------------------------------------------------------------
       01  DISPLAY-KEY-LIST.
      *    03  FILLER PIC X(27) VALUE "KEY-ID KEY-BRANCH KEY-SSNO.".
           03  FILLER PIC X(16) VALUE "KEY-ID KEY-SSNO.".

      *
      *    E L E M E N T - S P E C I F I C A T I O N S
      *
       01  SPEC-TABLE.
           03  FILLER   PIC X(08)   VALUE "E  A010N".
           03  FILLER   PIC X(08)   VALUE "ENNS072N".
           03  FILLER   PIC X(08)   VALUE "ENNS072N".
           03  FILLER   PIC X(08)   VALUE "E  A010N".
           03  FILLER   PIC X(08)   VALUE "ENNN080N".
           03  FILLER   PIC X(08)   VALUE "E  A010N".
           03  FILLER   PIC X(08)   VALUE "E  A010N".
           03  FILLER   PIC X(08)   VALUE "ENNN030N".
           03  FILLER   PIC X(08)   VALUE "ERNM060N".
           03  FILLER   PIC X(08)   VALUE "ERNM060N".
           03  FILLER   PIC X(08)   VALUE "E  A010N".
           03  FILLER   PIC X(08)   VALUE "E  A010N".
           03  FILLER   PIC X(08)   VALUE "ENNN030N".
      *-----------------------------------------------------------------
       01  SPEC-TAB            REDEFINES SPEC-TABLE.
           03  SPEC-REC                  OCCURS 13 TIMES.
               05  SPEC                  PIC X(07).
               05  CHFG                  PIC X(01).

       01  MAX                           PIC 9(02)     VALUE 13.
       01  ELE                           PIC 9(02)     VALUE ZEROES.

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
       COPY "LIBMN/PDPG11_DEF.CPY".
       COPY "LIBMN/PDPG11_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/ACCESSW.CPY".

       COPY "LIBGB/GETFMW.CPY".
       01 FORM-LINK-AREA    PIC X(2000).

       SCREEN SECTION.
       COPY "LIBMN/PDPG11_SCN.CPY".

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
              MOVE "N" TO NEW-REC
              MOVE "P" TO FORM-P1
              MOVE 1  TO FORM-SEL1
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
           MOVE EXT-CONAME-SYSDATE TO C-SYSDATE.
           MOVE C-SYSMO  TO CURR-MO.
           MOVE C-SYSDAY TO CURR-DAY.
           MOVE C-SYSYR  TO CURR-YR.

       COPY "LIBGB/CHKSEC.CPY".

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "PDPG11" TO VDU-FORM-NAME.
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
      *----------------------------------------------------
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
      *ENTER-KEY-BRANCH.

      *    MOVE "ENNN040KEY-BRANCH." TO VDU.
      *    PERFORM SCREENIO.

      *    IF (        F1-KEY ) GO TO ENTER-KEY-BEGIN.
      *    IF (        F6-KEY ) PERFORM ENTER-SCANKEY
      *                         IF ( STAT-GOOD )
      *                            GO TO ENTER-KEY-GET-RECORD
      *                         ELSE
      *                            PERFORM ENTER-KEY-BEGIN.
      *    IF (        F7-KEY ) GO TO ENTER-KEY-END.
      *    IF ( NOT ENTER-KEY ) GO TO ENTER-KEY-BRANCH.

      *    IF ( VDUNUM0 = ZEROES )
      *       GO TO ENTER-KEY-BRANCH.

      *    MOVE VDUNUM0 TO PD-BRANCH HOLD-PD-BRANCH.

      *KC->PERFORM OPEN-BR-FILE.
      *    PERFORM READ-BR-FILE.
      *    PERFORM CLOSE-BR-FILE.
      *
      *    IF ( IO-BAD )
      *       MOVE "INVALID: BRANCH NUMBER IS NOT ON FILE" TO MESS
      *       PERFORM SEND-MESS
      *KC->   GO TO ENTER-KEY-BRANCH.

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
              IF ( FORM-CURRENT-PAGE-11 ) OR ( NOT FORM-2-DIGITS )
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
                 F-11 F-12 F-13
                 DEPENDING ON ELE.

           GO TO END-ENTRY-MODULE.

      *
      *    S E T - R E C O R D - E L E M E N T S
      *
       F-01.
           MOVE VDUBUF TO PD-PARVHELD-PAID-FG.
           IF NOT (PD-PARVHELD-PAID-FG  = "Y" OR "N" OR "R" OR "V")
              MOVE "ENTER 'Y' OR 'N' OR 'R' ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-02.
           MOVE VDUNUM2 TO PD-IL-GROSS-INCOME.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-03.
           MOVE VDUNUM2 TO PD-IL-GROSS-INCOME2.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-04.
           MOVE VDUBUF TO PD-GARNISH-EXTR.
           GO TO CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-05.
      *WCBC EMPLOYEE ID
           MOVE VDUNUM0  TO PD-EMP-ID.
           GO TO CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-06.
           MOVE VDUBUF   TO PD-1099-C-FG.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-07.
           MOVE VDUBUF TO PD-ESCROW-FG.
           IF PD-ESCROW-FG NOT = "Y"
              IF PD-ESCROW-FG NOT = "N"
                 IF PD-ESCROW-FG NOT = "I"
                    MOVE "PLEASE ENTER 'Y' OR 'N' OR 'I'" TO MESS
                    PERFORM SEND-MESS
                    GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-08.
           MOVE VDUNUM0  TO PD-NEXIS-CRSCORE.
           GO TO CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-09.
           GO TO CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-10.
           GO TO CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-11.
           GO TO CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-12.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-13.
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

           MOVE SPACES                    TO DISPLAY-BUF.
           MOVE PD-PARVHELD-PAID-FG       TO F01.

           MOVE PD-IL-GROSS-INCOME        TO F02.
           MOVE PD-IL-GROSS-INCOME2       TO F03.
           MOVE PD-GARNISH-EXTR           TO F04.
           MOVE PD-EMP-ID                 TO F05.
           MOVE PD-1099-C-FG              TO F06.
           MOVE PD-ESCROW-FG              TO F07.
           MOVE PD-NEXIS-CRSCORE          TO F08.
           MOVE 0                         TO F09.
           MOVE 0                         TO F10.
           MOVE SPACES                    TO F11.
           MOVE SPACES                    TO F12.
           MOVE 0                         TO F13.

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
           MOVE EXT-CONAME-CONAME TO PDPG11-CONAME.
           MOVE EXT-CONAME-SYSDATE TO PDPG11-SYSDATE.
           DISPLAY SPACES UPON PDMAIN-WINDOW-HANDLE
                               LINE 0 COL 0 ERASE SCREEN.
           DISPLAY PDPG11-SCREEN UPON PDMAIN-WINDOW-HANDLE.
           MOVE PDPG11-FORM-FIELDS TO WR-BUF.
           MOVE PDPG11-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **************************************************************************
       SETUP-LINK-INFO SECTION.
      **************************************************************************
           MOVE PDPG11-HELP--FILE TO HELP-LINK-FILE.
           MOVE PDPG11-HELP--DIR TO HELP-LINK-DIR.

      **************************************************************************
       VDU-CONVERT-FIELD SECTION.
      **************************************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/PDPG11_EVA.CPY".
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
           DESTROY PDPG11-SCREEN.
           MOVE FORM-PATH TO FORM-PATHNAME.
           MOVE PDMAIN-LINK TO FORM-LINK-AREA.
           EXIT PROGRAM.
