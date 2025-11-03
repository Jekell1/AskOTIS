       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PDPG01.
       AUTHOR.        KEC.
       DATE-WRITTEN.  09-11-2000.
      ******************************************************************
      *
      * IDENTIFICATION: MN/PDPG01.C
      *
      * DESCRIPTION   : SCRATCH PAD FILE MAINTENANCE - PAGE 01
      *
      *-----------------------------------------------------------------
      * REV:
      * KEC 000911 {PR#00000} PARADATA FINANCIAL [BARB & BETH]
      *             ***NEW*** ADDED PER BARB & BETH.
      * MJD 000919 ADDED PD-LBOX (MFC PR#227)
      * BLV 021008 REMOVED VERIFY-DELETE ROUTINE WHICH HAD A BUNCH OF
      *            CHECKS LEFT OVER FROM PDMAIN AND STOPPED YOU FROM
      *            DELETING A QUOTE OR SAVED LOAN.  JUST ADDED A WARNING
      *            AND VERIFICATION WHEN DELETING A RENEWAL RECORD
      *
      * KEC 2017.0710 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          ADDED 'PD' FILE PREFIX FOR WORKING STORAGE FIELDS.
      *
      * BAH 190114 GLOBAL PDFILE, L2G
      * BAH 20190625 REMOVED LC-REVOLVING, PT#0153
      * BAH 20190627 REMOVED PD-REVOLVING, PT0153
      *  CS 20200114 CHANGED SCANKEY FROM NUMERIC TO ALPHA
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
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)MN/PDPG01 S34 09/08/21-09:42:55 >".

      ******************************************************************
      *
      *    F I L E - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01LC.CPY".
       COPY "LIBLP/LP01LC_SQL.CPY".
       COPY "LIBLP/LPWSLC.CPY".
       COPY "LIBLP/LP01PD.CPY".
       COPY "LIBLP/LP01PD_SQL.CPY".
       COPY "LIBLP/LPWSPD.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBLP/LP01ST.CPY".
       COPY "LIBLP/LP01ST_SQL.CPY".
       COPY "LIBLP/LPWSST.CPY".

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
      *
      *    M I S C E L L A N E O U S - W O R K E R S
      *
      ******************************************************************
       01  BW-SKIP                     PIC X(01).

       01  FIRST-SCAN                  PIC 9(01)     VALUE ZEROES.

       01  INDX                        PIC 9(01).
       01  IO-TYPE                     PIC X(03)     VALUE SPACES.
       01  NEW-FLAG                    PIC 9(01)     VALUE ZEROES.
       01  TEST-POCD                   PIC XX.
       01  FILLER REDEFINES TEST-POCD.
           03  TEST-POCD1  PIC X.
           03  TEST-POCD2  PIC X.

       01  PAGE-CHANGE-FLAG            PIC X(01)     VALUE "N".
           88  PAGE-CHANGE-NO                        VALUE "N".
           88  PAGE-CHANGE-YES                       VALUE "Y".

       01  SCANKEY-STAT                PIC X(02)     VALUE SPACES.
       01  SUB                         PIC 9(02)     VALUE ZEROES.
       01  SUBX                        PIC 9(02)     VALUE ZEROES.
       01  SUBY                        PIC 9(02)     VALUE ZEROES.
       01  VDU-INDX                    PIC 9(02).

       01  WK-MAKERCD                  PIC X(01)     VALUE " ".
           88  GOOD-MAKERCD                          VALUE "M" "J" "E".

       01  X-ST-BR                                   VALUE SPACES.
           03  X-STATE                 PIC X(02).
           03  X-HYPH                  PIC X(01).
           03  X-BR                    PIC 9(04).

      ******************************************************************
      *
      *    D I S P L A Y - W O R K E R S
      *
      ******************************************************************
       01  DISPLAY-BUF                               VALUE SPACES.
           03  F01                     PIC ZZZZZ9.
           03  F011                    PIC ZZ.
           03  F02                     PIC Z9.
           03  F03                     PIC X(07).
           03  F04                     PIC Z9.
           03  F05                     PIC Z9.
           03  F06                     PIC Z9.
           03  F07                     PIC X(01).
           03  F08                     PIC X(11).
           03  F081                    PIC X(01).
           03  F082                    PIC X(20).
           03  F09                     PIC X(11).
           03  F091                    PIC X(01).
           03  F092                    PIC X(20).
           03  F10                     PIC X(11).
           03  F101                    PIC X(01).
           03  F102                    PIC X(20).
           03  F11                     PIC X(11).
           03  F111                    PIC X(01).
           03  F112                    PIC X(20).
           03  F12                     PIC X(10).
           03  F13                     PIC X(01).
           03  F14                     PIC X(07).
           03  F141                    PIC X(01).
           03  F15                     PIC X(03).
           03  F16                     PIC X(02).
           03  F17                     PIC X(20).
           03  F18                     PIC X(02).
           03  F19                     PIC X(01).
           03  F20                     PIC X(01).
           03  F201                    PIC X(01).
           03  F21                     PIC X(01).
           03  F22                     PIC X(01).
           03  F23                     PIC X(01).
           03  F24                     PIC X(01).
           03  F25                     PIC X(01).
           03  F251                    PIC X(01).
           03  F26                     PIC X(01).
           03  F27                     PIC X(02).
           03  F271                    PIC X(02).
           03  F28                     PIC X(01).
           03  F29                     PIC X(01).
           03  F30                     PIC X(02).
           03  F31                     PIC X(01).
           03  FLAGS.
               05  FSMALLOAN           PIC X(01).
               05  FSALEFIN            PIC X(01).
               05  FSERVACCT           PIC X(01).
               05  FLOANTYPE           PIC X(01).
               05  FGIFTNET            PIC X(01).

      *-----------------------------------------------------------------
       01  DISPLAY-LIST.
           03  FILLER PIC X(30) VALUE "010 011 020 030 040 050 060   ".
           03  FILLER PIC X(30) VALUE "070 080 081 082 090 091 092   ".
           03  FILLER PIC X(30) VALUE "100 101 102 110 111 112 120   ".
           03  FILLER PIC X(30) VALUE "130 140 141 150 160 170 180   ".
           03  FILLER PIC X(30) VALUE "190 200 201 210 220 230 240   ".
           03  FILLER PIC X(30) VALUE "250 251 260 270 271 280 290   ".
           03  FILLER PIC X(14) VALUE "300 310 FLAGS.".

      *-----------------------------------------------------------------
       01  DISPLAY-KEY-BUF                           VALUE SPACES.
           03  D-KEY-ID                PIC X(01).
           03  D-KEY-SOC-SEC           PIC Z(10).

      *-----------------------------------------------------------------
       01  DISPLAY-KEY-LIST.
           03  FILLER PIC X(16) VALUE "KEY-ID KEY-SSNO.".

      ******************************************************************
      *
      *    E L E M E N T - S P E C I F I C A T I O N S
      *
      ******************************************************************
       01  ELE                         PIC 9(02)     VALUE ZEROES.
       78  MAX                                       VALUE 31.
       01  ONE                         PIC S9(1).

      *-----------------------------------------------------------------
       01  SPEC-TABLE.
           03  FILLER                  PIC X(08)     VALUE "ENNN060N".
           03  FILLER                  PIC X(08)     VALUE "EN N020N".
           03  FILLER                  PIC X(08)     VALUE "E  A050N".
           03  FILLER                  PIC X(08)     VALUE "EN N020N".
           03  FILLER                  PIC X(08)     VALUE "EN N020N".
           03  FILLER                  PIC X(08)     VALUE "E  N020N".
           03  FILLER                  PIC X(08)     VALUE "E  A010N".
           03  FILLER                  PIC X(08)     VALUE "ERNX090N".
           03  FILLER                  PIC X(08)     VALUE "ERNX090N".
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -{10}
           03  FILLER                  PIC X(08)     VALUE "ERNX090N".
           03  FILLER                  PIC X(08)     VALUE "ERNX090N".
           03  FILLER                  PIC X(08)     VALUE "E  A100N".
           03  FILLER                  PIC X(08)     VALUE "E  A010N".
           03  FILLER                  PIC X(08)     VALUE "E  A050N".
           03  FILLER                  PIC X(08)     VALUE "E  A030N".
           03  FILLER                  PIC X(08)     VALUE "E  A020N".
           03  FILLER                  PIC X(08)     VALUE "E  A200N".
           03  FILLER                  PIC X(08)     VALUE "E  A020N".
           03  FILLER                  PIC X(08)     VALUE "E  A010N".
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -{20}
           03  FILLER                  PIC X(08)     VALUE "E  A010N".
           03  FILLER                  PIC X(08)     VALUE "E  A010N".
           03  FILLER                  PIC X(08)     VALUE "E  A010N".
           03  FILLER                  PIC X(08)     VALUE "E  A010N".
           03  FILLER                  PIC X(08)     VALUE "E  A010N".
           03  FILLER                  PIC X(08)     VALUE "E  A010N".
           03  FILLER                  PIC X(08)     VALUE "E  A020N".
           03  FILLER                  PIC X(08)     VALUE "E  A010N".
           03  FILLER                  PIC X(08)     VALUE "E  A010N".
           03  FILLER                  PIC X(08)     VALUE "E  A010N".
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -{30}
           03  FILLER                  PIC X(08)     VALUE "E  A020N".
           03  FILLER                  PIC X(08)     VALUE "E  A010N".

      *-----------------------------------------------------------------
       01  SPEC-TAB                    REDEFINES SPEC-TABLE.
           03  SPEC-REC                OCCURS MAX TIMES.
               05  SPEC                PIC X(07).
               05  CHFG                PIC X(01).

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBMN/PDMAINW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/PDPG01_DEF.CPY".
       COPY "LIBMN/PDPG01_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/ACCESSW.CPY".

       COPY "LIBGB/GETFMW.CPY".
       01 FORM-LINK-AREA    PIC X(2000).

       SCREEN SECTION.
       COPY "LIBMN/PDPG01_SCN.CPY".

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

           IF ( IO-TYPE = "DEL" )
              PERFORM DELETE-MODULE.

           IF ( NEW-REC = "Y" )
              MOVE "P" TO FORM-P1
              MOVE 2 TO FORM-SEL1.

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

           MOVE "N" TO NEW-REC.
           GO TO MAIN-PROGRAM-LOOP.

       MAIN-PROGRAM-EXIT.
           GO TO END-PROGRAM.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************

       COPY "LIBGB/CHKSEC.CPY".

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "PDPG01" TO VDU-FORM-NAME.
           MOVE FORM-LINK-AREA TO PDMAIN-LINK.
           MOVE SPACES TO VDU-PGMENU-SELECTION.
           PERFORM OPEN-BW1-FILE.

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

           IF (        F1-KEY ) PERFORM ENTER-KEY-BEGIN.
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

           IF (        F1-KEY ) PERFORM ENTER-KEY-BEGIN.
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
           IF ( IO-GOOD )
              MOVE ZEROES TO NEW-FLAG
              MOVE PD-REC TO ORIGINAL-PD-REC
              PERFORM DISPLAY-FIELDS
           ELSE
              PERFORM NEW-RECORD-SETUP.

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


           IF ( VDUBUF  = SPACES )
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

           IF ( ELE     < 1   ) MOVE 1 TO ELE ONE.
           IF ( ELE NOT > MAX ) GO TO ENTER-ELE.

           MOVE 2 TO NEW-FLAG.

       MODIFY-ROUTINE.

           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.

           IF (        F1-KEY ) MOVE "CLR" TO IO-TYPE
                                GO TO ENTRY-MODULE-END.
           IF ( NOT ENTER-KEY ) GO TO MODIFY-ROUTINE.

           MOVE VDUBUF TO ACTION.

           IF ( ACTION = SPACES )
              MOVE "RWR" TO IO-TYPE
              GO TO CHECK-PAGE.

           IF ( ACTION = "DELETE" )
              IF PD-ID = "R"
                 MOVE "WARNING; YOU ARE DELETING A RENEWAL RECORD!"
                    TO MESS
                 PERFORM SEND-MESS
                 MOVE "TYPE 'YES' TO CONTINUE OR 'NO' TO CANCEL"
                    TO MESS
                 PERFORM CALL-TMMSG
                 MOVE "R RA060SEL." TO VDU
                 PERFORM SCREENIO
                 IF VDUBUF NOT = "   YES"
                    MOVE SPACES TO MESS
                    PERFORM CALL-TMMSG
                    GO TO MODIFY-ROUTINE
                 END-IF
              END-IF
              MOVE "DEL" TO IO-TYPE
              GO TO ENTRY-MODULE-END.

      * CHECK PAGES 01 THRU 09 

           IF ( FORM-KEY1 = SPACES ) AND
              ( FORM-P1   = "P"    ) AND
              ( FORM-SEL1 NUMERIC  )
              IF ( FORM-SEL1 < 2 ) OR
                 ( FORM-SEL1 > 9 )
                 GO TO MODIFY-ROUTINE
              ELSE
                 GO TO CHECK-PAGE.

      * CHECK PAGES 10 THRU 99 

           IF ( FORM-KEY2 = SPACES ) AND
              ( FORM-P2   = "P"    ) AND
              ( FORM-SEL2 NUMERIC  )
              IF ( NOT FORM-2-DIGITS )
                 GO TO MODIFY-ROUTINE
              ELSE
                 GO TO CHECK-PAGE.

           GO TO CHECK-CHNO.

       CHECK-PAGE.

           PERFORM TEST-SPR.

           IF ( IO-BAD )
              PERFORM INVALID-SPR
              GO TO MODIFY-ROUTINE.

           IF ( NEW-FLAG = 1 OR 2 )
              MOVE SP-EARN-ORDER(1) TO PD-EARN-ORDER(1)
              MOVE SP-EARN-ORDER(2) TO PD-EARN-ORDER(2)
              MOVE SP-EARN-ORDER(3) TO PD-EARN-ORDER(3)
              MOVE SP-EARN-ORDER(4) TO PD-EARN-ORDER(4)
              MOVE SP-EARN-ORDER(5) TO PD-EARN-ORDER(5)
              MOVE SP-EARN-ORDER(6) TO PD-EARN-ORDER(6).

           MOVE "RWR" TO IO-TYPE.
           GO TO ENTRY-MODULE-EXIT.

       CHECK-CHNO.

           INSPECT ACTION REPLACING ALL SPACES BY ZEROES.

           IF ( CHNO-X NOT NUMERIC ) GO TO MODIFY-ROUTINE.
           IF ( CHNO = ZEROES      ) GO TO MODIFY-ROUTINE.
           IF ( CHNO > MAX         ) GO TO MODIFY-ROUTINE.

           MOVE CHNO TO ELE.

       ENTER-ELE.

           IF CHKEY NOT = ZEROS
              IF CHFG(ELE) = " "
                 IF NEW-FLAG NOT = 1
                    GO TO MODIFY-ROUTINE.

           IF ELE NOT = 27
              IF NEW-FLAG = 0
                 IF CHFG(ELE) = "N"
                    IF CHKEY NOT = EXT-CONAME-DAILY-PASSWORD
                       GO TO MODIFY-ROUTINE.

           MOVE SPEC(ELE) TO VDU.
           MOVE ELE       TO VDUROW.
           MOVE ZEROES    TO VDUCOL.
           MOVE "."       TO VDUDECIMAL.
           PERFORM SCREENIO.

           IF ( F1-KEY )
              MOVE "CLR" TO IO-TYPE
              GO TO ENTRY-MODULE-END.

           IF ( DOWN-KEY )
              MOVE 1 TO ONE
              GO TO GOTO-FIELD.

           IF ( UP-KEY )
              IF ( NEW-FLAG = 1 )
                 MOVE -1 TO ONE
                 GO TO CREATE-ROUTINE.

           IF ( NOT ENTER-KEY )
              GO TO ENTER-ELE.

           MOVE 1 TO ONE.

       GOTO-FIELD.

           GO TO F-01 F-02 F-03 F-04 F-05 F-06 F-07 F-08 F-09 F-10
                 F-11 F-12 F-13 F-14 F-15 F-16 F-17 F-18 F-19 F-20
                 F-21 F-22 F-23 F-24 F-25 F-26 F-27 F-28 F-29 F-30
                 F-31
                 DEPENDING ON ELE.

           GO TO ENTRY-MODULE-END.

       F-01.
           MOVE VDUNUM0 TO PD-ACCTNO.

           IF ( PD-ACCTNO = 0)
              MOVE "SNNN020011." TO VDU
              MOVE ZEROES        TO VDUNUM0
              PERFORM SCREENIO
              GO TO CREATE-ROUTINE.

           GO TO CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-02.
           MOVE VDUNUM0 TO LC-CODE
                           PD-CLASS.

           PERFORM VALIDATE-PD-CLASS.

           IF ( IO-FG NOT = ZEROES )
              GO TO ENTER-ELE.

           IF ( NEW-FLAG NOT = 1 )
              IF ( LC-LOANTYPE  NOT = PD-LOANTYPE  ) OR
                 ( LC-SALEFIN   NOT = PD-SALEFIN   ) OR
                 ( LC-SERVACCT  NOT = PD-SERVACCT  )
                 MOVE "CLASSES ARE NOT CONSISTENT IN TYPES" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE
              END-IF
           ELSE
              MOVE LC-SMALLOAN  TO PD-SMALLOAN
              MOVE LC-SALEFIN   TO PD-SALEFIN
              MOVE LC-SERVACCT  TO PD-SERVACCT
              MOVE LC-LOANTYPE  TO PD-LOANTYPE
              MOVE LC-GIFTNET   TO PD-GIFTNET.

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-03.
           MOVE VDUBUF TO X-ST-BR.
           IF X-BR NOT NUMERIC
              GO TO ENTER-ELE.
           MOVE X-STATE TO ST1-KEY.
           PERFORM GET-ST1-FILE.
           IF IO-FG NOT = 0
              PERFORM INVALID-STATE
              GO TO ENTER-ELE.
           MOVE X-BR TO BR-NO.
           PERFORM GET-BR-FILE.
           IF IO-FG NOT = 0
              PERFORM INVALID-BRANCH
              GO TO ENTER-ELE.

           MOVE X-BR TO PD-ORGBR.
           MOVE X-STATE TO PD-ORGST.
           MOVE "-" TO X-HYPH.
           MOVE "S  A070030." TO VDU.
           MOVE X-ST-BR TO VDUBUF.
           PERFORM SCREENIO.
           IF NEW-FLAG NOT = 1
              PERFORM TEST-SPR
              IF IO-FG = 9
                 PERFORM INVALID-SPR.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-04.
           MOVE VDUNUM0 TO PD-SPRCLASS
                           LC-CODE.

           PERFORM VALIDATE-PD-CLASS.

           IF ( IO-FG NOT = ZEROES )
              MOVE "INVALID CLASS" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           IF ( LC-LOANTYPE  NOT = PD-LOANTYPE  ) OR
              ( LC-SALEFIN   NOT = PD-SALEFIN   ) OR
              ( LC-SERVACCT  NOT = PD-SERVACCT  ) 
              MOVE "CLASSES ARE NOT CONSISTENT IN TYPES" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           IF ( NEW-FLAG NOT = 1 )
              PERFORM TEST-SPR
              IF ( IO-FG NOT = ZEROES )
                 PERFORM INVALID-SPR.

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-05.
           MOVE VDUNUM0 TO PD-SUBCLASS.
           IF NEW-FLAG NOT = 1
              PERFORM TEST-SPR
              IF IO-FG = 9
                 PERFORM INVALID-SPR.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-06.
           IF VDUNUM0 = 0
              GO TO ENTER-ELE.
           MOVE VDUNUM0 TO PD-LAWCODE.
           PERFORM TEST-SPR.
           IF IO-FG = 9
              PERFORM INVALID-SPR.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-07.
      *    IF NOT (VDUBUF = "Y" OR " ")
      *       MOVE " ENTER: ' ' OR 'Y'" TO MESS
      *       PERFORM SEND-MESS
      *       GO TO ENTER-ELE.
      *    MOVE VDUBUF TO PD-MORELN.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-08.
           MOVE 1 TO INDX.
           PERFORM MODIFY-BORROWER.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-09.
           MOVE 2 TO INDX.
           PERFORM MODIFY-BORROWER.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-10.
           MOVE 3 TO INDX.
           PERFORM MODIFY-BORROWER.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-11.
           MOVE 4 TO INDX.
           PERFORM MODIFY-BORROWER.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-12.
           MOVE VDUBUF TO PD-STATFLAGS.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-13.
           IF VDUBUF NOT = " "
                  AND NOT = "J"
                   AND NOT = "A"
                    AND NOT = "R"
              MOVE "STATUS MUST BE J, A, R OR SPACE" TO MESS
              PERFORM SEND-MESS
              GO ENTER-ELE.
           MOVE VDUBUF TO PD-STATUSFG.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-14.
           MOVE VDUBUF TO X-ST-BR.
           IF X-BR NOT NUMERIC
              GO TO ENTER-ELE.
           MOVE X-STATE TO ST1-KEY.
           PERFORM GET-ST1-FILE.
           IF IO-FG NOT = 0
              PERFORM INVALID-STATE
              GO TO ENTER-ELE.
           MOVE X-BR TO BR-NO.
           PERFORM GET-BR-FILE.
           IF IO-FG NOT = 0
              PERFORM INVALID-BRANCH
              GO TO ENTER-ELE.

           MOVE X-BR TO PD-OWNBR.
           MOVE X-STATE TO PD-OWNST.
           MOVE "-" TO X-HYPH.
           MOVE "S  A070140." TO VDU.
           MOVE X-ST-BR TO VDUBUF.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       F-141.
           MOVE "E  A010141." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-141.
           MOVE VDUBUF TO PD-BR-TFR-FG.
           IF PD-BR-TFR-FG NOT = "Y" AND NOT = "N"
              GO TO F-141.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-15.
           MOVE VDUBUF TO PD-PURCD.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-16.
           MOVE VDUBUF TO PD-COLLCD.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-17.
           MOVE VDUBUF TO PD-COLLID.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-18.
           MOVE VDUBUF TO PD-SRCD.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-19.
           IF VDUBUF NOT = "Y"
                  AND NOT = "N"
              PERFORM ENTER-Y-OR-N
              GO TO ENTER-ELE.
           MOVE VDUBUF TO PD-CONFIDENTCD.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-20.
           IF VDUBUF NOT = "Y"
                  AND NOT = "N"
              PERFORM ENTER-Y-OR-N
              GO TO ENTER-ELE.
           MOVE VDUBUF TO PD-ALLOTCD.
           IF PD-ALLOTCD = "Y"
              MOVE "S  A010201." TO VDU
              MOVE "N" TO VDUBUF PD-LBOX
              PERFORM SCREENIO
              GO TO CREATE-ROUTINE.
       A-201.
           MOVE "E  A010201." TO VDU
           PERFORM SCREENIO.
           IF UP-KEY GO TO ENTER-ELE.
           IF NOT ENTER-KEY GO TO A-201.
           IF NOT (VDUBUF = "Y" OR "N")
              PERFORM ENTER-Y-OR-N
              GO TO A-201.
           MOVE VDUBUF TO PD-LBOX.
           IF PD-LBOX = "Y"
              MOVE "S  A010200." TO VDU
              MOVE "N" TO VDUBUF PD-ALLOTCD
              PERFORM SCREENIO.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-21.
           IF VDUBUF NOT = "Y"
                  AND NOT = "N"
                   AND NOT = "B"
                    AND NOT = "X"
                     AND NOT = "I"
                      AND NOT = "R"
              PERFORM ENTER-Y-OR-N
              GO TO ENTER-ELE.
           MOVE VDUBUF TO PD-CONVERCD.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-22.
           IF VDUBUF NOT = "Y"
                  AND NOT = "N"
              PERFORM ENTER-Y-OR-N
              GO TO ENTER-ELE.
           MOVE VDUBUF TO PD-MIDSTREAM.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-23.
           IF VDUBUF NOT = "Y"
                  AND NOT = "N"
              PERFORM ENTER-Y-OR-N
              GO TO ENTER-ELE.
           MOVE VDUBUF TO PD-SKIP-TRW.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-24.
           IF VDUBUF NOT = " "
              MOVE "ENTER A SPACE ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           MOVE VDUBUF TO PD-RECASTCD.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-25.
           IF VDUBUF NOT = "P"
                  AND NOT = "I"
                   AND NOT = "W"
                    AND NOT = " "
              MOVE " ENTER: 'RTN' 'P' 'I' OR 'W'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           MOVE VDUBUF TO PD-PLCD.
           IF PD-PLCD = " "
              MOVE "S  A010250." TO VDU
              PERFORM SCREENIO
              MOVE " " TO PD-PLTYPE
              GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-251.
           MOVE "E  A010251." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-251.
           MOVE VDUBUF TO PD-PLTYPE.
           IF NOT (PD-PLTYPE = "P" OR "2" OR "3" OR "4" OR "5")
              MOVE "ENTER: 'P' OR '2' OR '3' OR '4' OR '5'" TO MESS
              PERFORM SEND-MESS
              GO TO F-251.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-26.
           IF VDUBUF NOT = "X"
                  AND NOT = "V"
                   AND NOT = "S"
                    AND NOT = " "
              MOVE " ENTER: 'RTN' 'X' 'V' OR 'S'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           MOVE VDUBUF TO PD-REPOCD.
           IF NEW-FLAG NOT = 1
              IF PD-REPOCD = "X"
                      AND PD-REPODATE = 0
                 MOVE "LOAN REPO DATE = 0, PLEASE INPUT ONE!" TO MESS
                 PERFORM SEND-MESS
              ELSE
              IF PD-REPOCD = "V"
                      AND PD-REDEEMDATE = 0
                 MOVE "LOAN REPO REDEEM DATE = 0, PLEASE INPUT ONE!"
                                                                TO MESS
                 PERFORM SEND-MESS
              ELSE
              IF PD-REPOCD = "S"
                      AND PD-SOLDDATE = 0
                 MOVE "LOAN REPO SOLD DATE = 0, PLEASE INPUT ONE!"
                                                                TO MESS
                 PERFORM SEND-MESS
              ELSE
              IF PD-REPOCD = " "
                      AND (NOT (PD-REPODATE = 0 AND PD-SOLDDATE = 0))
                 MOVE "LOAN REPO DATE'S ARE SET, PLEASE CLEAR THEM!"
                                                                TO MESS
                 PERFORM SEND-MESS.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-27.
           MOVE VDUBUF TO PD-POCD TEST-POCD.

           IF ( TEST-POCD1 = "*" ) AND ( NOT PD-POCD-NONLOAN )
              MOVE "INVALID NON-LOAN PAYOFF CODE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

      *-----------------------------------------------------------------
       F-271.
           IF NOT PD-POCD-POFF
              MOVE "S  A020271." TO VDU
              MOVE " " TO PD-PO-TRCD
              PERFORM SCREENIO
              GO TO CREATE-ROUTINE.

           MOVE "E  A020271." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-271.
           MOVE VDUBUF TO PD-PO-TRCD.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-28.
           IF VDUBUF NOT = "Y"
                  AND NOT = " "
              PERFORM ENTER-Y-OR-SP
              GO TO ENTER-ELE.
           MOVE VDUBUF TO PD-ADDON-FG.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-29.
           IF VDUBUF NOT = "Y"
                  AND NOT = " "
              PERFORM ENTER-Y-OR-SP
              GO TO ENTER-ELE.
           MOVE VDUBUF TO PD-PAY-SCHLD-FG.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-30.
           MOVE VDUBUF TO PD-CAN-LOAN-NO.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-31.
           MOVE VDUBUF TO PD-ACCT-FROZEN.
           IF NOT (PD-ACCT-FROZEN = "Y" OR "N")
              PERFORM ENTER-Y-OR-N
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.

       ENTRY-MODULE-END.

           MOVE ZEROES TO FORM-SEL1 FORM-SEL2.

       ENTRY-MODULE-EXIT.
           EXIT.

      ******************************************************************
       WRITE-MODULE SECTION.
      ******************************************************************

           PERFORM OPEN-PD1-FILE.

           IF ( NEW-FLAG NOT = ZEROES )
              GO TO WRITE-RECORD.

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

      * INSURE ALL SOC-SEC-NO'S IN LOAN RECORD ARE UNIQUE

           PERFORM VARYING SUBX FROM 1 BY 1 UNTIL SUBX > 4
              ADD 1 TO SUBX GIVING SUBY
              PERFORM VARYING SUB FROM SUBY BY 1 UNTIL SUB > 4
                 IF ( PD-SSNO(SUB) NOT = ZEROES       ) AND
                    ( PD-SSNO(SUB)     = PD-SSNO(SUBX))
                    MOVE ZEROES TO PD-SSNO(SUB)
                    MOVE SPACES TO PD-MAKERCD(SUB)
                 END-IF
              END-PERFORM
           END-PERFORM.

      * INSURE NO EMPTY SOC-SEC-NO SLOTS

           PERFORM 2 TIMES
              IF (PD-SSNO(2) = 0)
                       AND ( (PD-SSNO(3) NOT = 0)
                                    OR (PD-SSNO(4) NOT = 0)
                           )
                 MOVE PD-SSNO   (3) TO PD-SSNO   (2)
                 MOVE PD-MAKERCD(3) TO PD-MAKERCD(2)
                 MOVE PD-SSNO   (4) TO PD-SSNO   (3)
                 MOVE PD-MAKERCD(4) TO PD-MAKERCD(3)
                 MOVE ZEROES        TO PD-SSNO   (4)
                 MOVE SPACES        TO PD-MAKERCD(4)
              END-IF
           END-PERFORM.

           IF PD-SSNO(3) = 0 AND PD-SSNO(4) NOT = 0
              MOVE PD-SSNO(4) TO PD-SSNO(3)
              MOVE PD-MAKERCD(4) TO PD-MAKERCD(3)
              MOVE 0 TO PD-SSNO(4)
              MOVE " " TO PD-MAKERCD(4).

           PERFORM REWRITE-PD1-FILE.

           MOVE "C"             TO ERRLOGW-ACTION.
           MOVE PD-ID           TO HOLD-PD-ID.
           MOVE PD-BRANCH       TO HOLD-PD-BRANCH.
           MOVE PD-SOC-SEC      TO HOLD-PD-SOC-SEC.
           MOVE HOLD-PD1-KEY    TO ERRLOGW-KEY.
           MOVE FORM-PATHNAME   TO ERRLOGW-PROGX.

           MOVE "CL/ERRLOG" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PROGX.

           GO TO WRITE-MODULE-END.

       WRITE-RECORD.

           PERFORM WRITE-PD1-FILE.
           IF ( IO-BAD )
              PERFORM ALREADY-CREATED
           ELSE
              MOVE "Y" TO NEW-REC.

       WRITE-MODULE-END.
           PERFORM CLOSE-PD1-FILE.

       WRITE-MODULE-EXIT.
           EXIT.

      ******************************************************************
       DELETE-MODULE SECTION.
      ******************************************************************

           IF ( NEW-FLAG NOT = ZEROES )
              GO TO DELETE-MODULE-END.

           PERFORM OPEN-PD1-FILE.
           PERFORM READ-PD1-FILE.

           IF ( IO-FG NOT = ZEROES )
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO DELETE-MODULE-END.

           IF ( PD-REC NOT = ORIGINAL-PD-REC )
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-CHANGED
              GO TO DELETE-MODULE-END.

           PERFORM DELETE-PD1-FILE.

       DELETE-MODULE-END.

           PERFORM CLOSE-PD1-FILE.
           MOVE SPACES TO MESS.
           PERFORM CALL-TMMSG.

       DELETE-MODULE-EXIT.
           EXIT.

      ******************************************************************
       NEW-RECORD-SETUP SECTION.
      ******************************************************************

           MOVE 1 TO NEW-FLAG.

           MOVE "S  A000NEW."        TO VDU.
           MOVE "*** NEW RECORD ***" TO VDUBUF.
           PERFORM SCREENIO.

           PERFORM CLEAR-PD-FILE.

           MOVE HOLD-PD-ID      TO PD-ID.
           MOVE HOLD-PD-BRANCH  TO PD-BRANCH.
           MOVE HOLD-PD-SOC-SEC TO PD-SOC-SEC.
           MOVE ZEROES TO ELE.

       NEW-RECORD-EXIT.
           EXIT.

      ******************************************************************
       DISPLAY-FIELDS SECTION.
      ******************************************************************

           MOVE PD-ACCTNO                  TO F01.
           MOVE PD-CLASS                   TO F02.
           MOVE "-"                        TO X-HYPH.
           MOVE PD-ORGBR                   TO X-BR.
           MOVE PD-ORGST                   TO X-STATE.
           MOVE X-ST-BR                    TO F03.
           MOVE PD-SPRCLASS                TO F04.
           MOVE PD-SUBCLASS                TO F05.
           MOVE PD-LAWCODE                 TO F06.
      *    MOVE PD-MORELN                  TO F07.
           MOVE PD-STATFLAGS               TO F12.
           MOVE PD-STATUSFG                TO F13.
           MOVE PD-OWNBR                   TO X-BR.
           MOVE PD-OWNST                   TO X-STATE.
           MOVE X-ST-BR                    TO F14.
           MOVE PD-BR-TFR-FG               TO F141.
           MOVE PD-PURCD                   TO F15.
           MOVE PD-COLLCD                  TO F16.
           MOVE PD-COLLID                  TO F17.
           MOVE PD-SRCD                    TO F18.
           MOVE PD-CONFIDENTCD             TO F19.
           MOVE PD-ALLOTCD                 TO F20.
           MOVE PD-LBOX                    TO F201.
           MOVE PD-CONVERCD                TO F21.
           MOVE PD-MIDSTREAM               TO F22.
           MOVE PD-SKIP-TRW                TO F23.
           MOVE PD-RECASTCD                TO F24.
           MOVE PD-PLCD                    TO F25.
           MOVE PD-PLTYPE                  TO F251.
           MOVE PD-REPOCD                  TO F26.
           MOVE PD-POCD                    TO F27.
           MOVE PD-PO-TRCD                 TO F271.
           MOVE PD-ADDON-FG                TO F28.
           MOVE PD-PAY-SCHLD-FG            TO F29.
           MOVE PD-CAN-LOAN-NO             TO F30.
           MOVE PD-ACCT-FROZEN             TO F31.

           MOVE PD-SMALLOAN                TO FSMALLOAN.
           MOVE PD-SALEFIN                 TO FSALEFIN.
           MOVE PD-SERVACCT                TO FSERVACCT.
           MOVE PD-LOANTYPE                TO FLOANTYPE.
           MOVE PD-GIFTNET                 TO FGIFTNET.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           PERFORM DISPLAY-THE-BORROWERS
               VARYING INDX FROM 1 BY 1 UNTIL INDX > 4.

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
       DISPLAY-THE-BORROWERS SECTION.
      ******************************************************************

           COMPUTE VDU-INDX = INDX + 7.

           MOVE PD-SSNO(INDX) TO BW-SSNO.
           PERFORM OPEN-BW1-FILE.
           PERFORM READ-BW1-FILE.
           PERFORM CLOSE-BW1-FILE.

           MOVE "S  X110 " TO VDU.
           MOVE VDU-INDX   TO VDUROW.
           MOVE ZEROES     TO VDUCOL.
           MOVE "."        TO VDUDECIMAL.

           IF ( PD-SSNO(INDX) NOT = ZEROES )
              MOVE PD-SSNO(INDX) TO VDU-SSNO-EDIT.

           PERFORM SCREENIO.

           MOVE "S  A200 " TO VDU.
           MOVE VDU-INDX   TO VDUROW.
           MOVE 2          TO VDUCOL.
           MOVE "."        TO VDUDECIMAL.

           STRING BW-FNAME," ",BW-LNAME DELIMITED BY "   "
                                        INTO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A010 "       TO VDU.
           MOVE VDU-INDX         TO VDUROW.
           MOVE 1                TO VDUCOL.
           MOVE "."              TO VDUDECIMAL.
           MOVE SPACES           TO VDUBUF.
           MOVE PD-MAKERCD(INDX) TO VDUBUF.
           PERFORM SCREENIO.

           MOVE SPACES TO BW-REC.

       DISPLAY-THE-BORROWERS-EXIT.
           EXIT.

      ******************************************************************
       MODIFY-BORROWER SECTION.
      ******************************************************************

           MOVE "N" TO BW-SKIP.

           IF ( VDUNUM0 = ZEROES )
              MOVE "Y"    TO BW-SKIP
              MOVE ZEROES TO PD-SSNO(INDX)
              MOVE SPACES TO BW-FNAME BW-LNAME PD-MAKERCD(INDX)
           ELSE
              PERFORM MODIFY-SSN.

           MOVE "S  X110 "    TO VDU.
           MOVE ELE           TO VDUROW.
           MOVE 0             TO VDUCOL.
           MOVE "."           TO VDUDECIMAL.
           MOVE PD-SSNO(INDX) TO VDU-SSNO-EDIT.
           PERFORM SCREENIO.

           MOVE "S  A200 " TO VDU.
           MOVE ELE        TO VDUROW.
           MOVE 2          TO VDUCOL.
           MOVE "."        TO VDUDECIMAL.
           STRING BW-FNAME," ",BW-LNAME DELIMITED BY "   "
                                        INTO VDUBUF.
           PERFORM SCREENIO.

           IF ( BW-SKIP = "Y" )
              GO TO MB-CD.

      * BORROWER SSNO NOT FOUND IN BWFILE 
           IF ( BW-SKIP = "X" )
              GO TO MB-EXIT.

       MB-MC.

           MOVE "E  A010 " TO VDU.
           MOVE ELE        TO VDUROW.
           MOVE 1          TO VDUCOL.
           MOVE "."        TO VDUDECIMAL.
           PERFORM SCREENIO.

           MOVE VDUBUF TO WK-MAKERCD.

           IF NOT ( GOOD-MAKERCD )
              MOVE "ENTER: 'M' 'J' OR 'E'" TO MESS
              PERFORM SEND-MESS
              GO TO MB-MC.

           MOVE VDUBUF TO PD-MAKERCD(INDX).

       MB-CD.

           MOVE "S  A010 "       TO VDU.
           MOVE ELE              TO VDUROW.
           MOVE 1                TO VDUCOL.
           MOVE "."              TO VDUDECIMAL.
           MOVE SPACES           TO VDUBUF.
           MOVE PD-MAKERCD(INDX) TO VDUBUF.
           PERFORM SCREENIO.

       MB-EXIT.
           EXIT.

      ******************************************************************
       MODIFY-SSN SECTION.
      ******************************************************************

           MOVE VDUNUM0 TO BW-SSNO.
           PERFORM OPEN-BW1-FILE.
           PERFORM READ-BW1-FILE.
           PERFORM CLOSE-BW1-FILE.
           IF ( IO-BAD )
              MOVE "X"               TO BW-SKIP
              MOVE "* NOT ON FILE *" TO MESS
              PERFORM SEND-MESS
           ELSE
              MOVE BW-SSNO TO PD-SSNO(INDX).

       MODIFY-SSN-EXIT.
           EXIT.

      **************************************************************************
       FORM-RESET SECTION.
      **************************************************************************
           MOVE EXT-CONAME-CONAME TO PDPG01-CONAME.
           MOVE EXT-CONAME-SYSDATE TO PDPG01-SYSDATE.
           DISPLAY SPACES UPON PDMAIN-WINDOW-HANDLE
                               LINE 0 COL 0 ERASE SCREEN.
           DISPLAY PDPG01-SCREEN UPON PDMAIN-WINDOW-HANDLE.
           MOVE PDPG01-FORM-FIELDS TO WR-BUF.
           MOVE PDPG01-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **************************************************************************
       SETUP-LINK-INFO SECTION.
      **************************************************************************
           MOVE PDPG01-HELP--FILE TO HELP-LINK-FILE.
           MOVE PDPG01-HELP--DIR TO HELP-LINK-DIR.

      **************************************************************************
       VDU-CONVERT-FIELD SECTION.
      **************************************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/PDPG01_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".

       COPY "LIBLP/LPPDCL.CPY".

      ******************************************************************
       MISCELLANEOUS-PARAGRAPHS SECTION.
      ******************************************************************
       COPY "LIBGB/ACCESS.CPY".
       ENTER-Y-OR-N.
           MOVE "ENTER: 'Y' OR 'N'" TO MESS.
           PERFORM SEND-MESS.
       ENTER-Y-OR-SP.
           MOVE "ENTER: 'Y' OR ' '" TO MESS.
           PERFORM SEND-MESS.
       GET-BR-FILE.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
       GET-ST1-FILE.
           PERFORM OPEN-ST1-FILE.
           PERFORM READ-ST1-FILE.
           PERFORM CLOSE-ST1-FILE.
       TEST-SPR.
           MOVE PD-ORGST TO SP-ORGST.
           MOVE PD-SPRCLASS TO SP-SPRCLASS.
           MOVE PD-SUBCLASS TO SP-SUBCLASS.
           MOVE PD-LAWCODE TO SP-LAWCODE.
           PERFORM OPEN-SP1-FILE.
           PERFORM READ-SP1-FILE.
           PERFORM CLOSE-SP1-FILE.
       INVALID-BRANCH.
           MOVE "INVALID BRANCH NO" TO MESS.
           PERFORM SEND-MESS.
       INVALID-STATE.
           MOVE "INVALID STATE CODE" TO MESS.
           PERFORM SEND-MESS.
       INVALID-SPR.
           MOVE "STATE PARAMETER RECORD NOT ON FILE" TO MESS.
           PERFORM SEND-MESS.
       VALIDATE-PD-CLASS.
           PERFORM OPEN-LC1-FILE.
           PERFORM READ-LC1-FILE.
           PERFORM CLOSE-LC1-FILE.

           IF IO-FG NOT = 0
              MOVE " INVALID LOAN CLASS" TO MESS
              PERFORM SEND-MESS.
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
           MOVE "THIS RECORD HAS UNDERGONE CHANGE BY ANOTHER USER,"
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
           PERFORM CLOSE-ST1-FILE.
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-PD1-FILE.
           PERFORM CLOSE-LC1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPSTGS_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPPDGS_SQL.CPY".
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".

       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPLC1RN.CPY".
       COPY "LIBLP/LPPD1IN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBLP/LPST1RN.CPY".

       COPY "LIBGB/FERRORS.CPY".

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************

           PERFORM CLOSE-FILES.

       EXIT-PROG.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY PDPG01-SCREEN.
           MOVE FORM-PATH TO FORM-PATHNAME.
           MOVE PDMAIN-LINK TO FORM-LINK-AREA.
           EXIT PROGRAM.

