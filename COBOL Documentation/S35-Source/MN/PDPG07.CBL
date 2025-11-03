       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PDPG07.
       AUTHOR.        KEC.
       DATE-WRITTEN.  09-12-2000.
      ******************************************************************
      *
      * IDENTIFICATION: MN/PDPG07.C
      *
      * DESCRIPTION   : SCRATCH PAD FILE MAINTENANCE - PAGE 07
      *
      *-----------------------------------------------------------------
      * REV:
      * KEC 000912 {PR#00000} PARADATA FINANCIAL [BARB & BETH]
      *             ***NEW*** ADDED PER BARB & BETH.
      *
      *  JTG 020905 ADDED PD-DLCOMM-03-CMSI PD-DLCOMM-PP-CMSI  REGACC #1846
      *  BLF 050120 FIXED PD-DLCOMM-03-CMSI, SB PD-DLCOMM-O3-CMSI
      *  MJD 131127 A30 CONVERSION COMPLETED
      *   CS 170504 ACTIVATE 8 DIGIT DATES
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
           "@(#)MN/PDPG07.CBL S34 06/06/21-16:23:09 >".

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
       COPY "LIBLP/LP01DL.CPY".
       COPY "LIBLP/LP01DL_SQL.CPY".
       COPY "LIBLP/LPWSDL.CPY".

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

      *    'PAGE-CHANGE-FLAG' IS FOR KNOWING IF COMING FROM ANOTHER PAGE
       01  PAGE-CHANGE-FLAG      PIC X(1)    VALUE "N".
           88  PAGE-CHANGE-NO                VALUE "N".
           88  PAGE-CHANGE-YES               VALUE "Y".

       01  DLR-TYP-LIST      PIC X(6)   VALUE "10P:P.".
       01  DLR-TYP-BUF       PIC X(10).

       01  WS-ORGBR      PIC 9999.

       01  SCANKEY-STAT                PIC X(02)     VALUE SPACES.

      *    **********************************
      *    *      SCREEN DISPLAY BUFFER
      *    **********************************
       01  DISPLAY-BUF    VALUE SPACES.
           03  F01        PIC ZZZZ.ZZ.
           03  F02        PIC Z(6).ZZ.
           03  F03        PIC Z(6).ZZ.
           03  F04        PIC Z(6).ZZ.
           03  F05        PIC Z(6).ZZ.
           03  F06        PIC Z(6).ZZ.
           03  F07        PIC Z(6).ZZ.
           03  F08        PIC Z(6).ZZ.
           03  F081       PIC Z(6).ZZ.
           03  F09        PIC Z(6).ZZ.
           03  F091       PIC Z(6).ZZ.
           03  F10        PIC Z(6).ZZ.
           03  F11        PIC X.
           03  F12        PIC Z(6).ZZ.
           03  F13        PIC Z(5).
           03  F14        PIC Z(6).ZZ.
           03  F15        PIC Z(6).ZZ.
           03  F16        PIC ZZZ.
           03  F17        PIC Z(6).ZZ-.
           03  F18        PIC 99/99/99 BLANK ZERO.
      * CL REBATE
           03  F19        PIC X.
           03  F20        PIC X.
           03  F21        PIC X.
      * O1 REBATE
           03  F22        PIC X.
           03  F23        PIC X.
           03  F24        PIC X.
           03  F25        PIC X.
      * O5 REBATE
           03  F26        PIC X.
      * N/A
           03  F27        PIC X.
      * N/A
           03  F28        PIC X.
      * INT REBATE
           03  F29        PIC X.
           03  F30        PIC X.
           03  F31        PIC X.
           03  F32        PIC X.
           03  FLNSTAT    PIC X(30).

       01  DISPLAY-LIST.
           03  FILLER     PIC X(48) VALUE
               "010 020 030 040 050 060 070 080 081 090 091 100 ".
           03  FILLER     PIC X(20) VALUE
               "110 120 130 140 150 ".
           03  FILLER     PIC X(28) VALUE
               "160 170 180 190 200 210 220 ".
           03  FILLER     PIC X(47) VALUE
               "230 240 250 260 270 280 290 300 310 320 LNSTAT.".

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
       01  MAX            PIC 99   VALUE 32.
       01  SPEC-TABLE.
      * 01 DEALER NO
           03  FILLER     PIC X(8) VALUE "ENNN042N".
      * 02 RESERVE PRINCIPAL #1
           03  FILLER     PIC X(8) VALUE "ENNN062N".
           03  FILLER     PIC X(8) VALUE "ENNN062N".
           03  FILLER     PIC X(8) VALUE "ENNN062N".
      * 05 RESERVE CHARGES #1
           03  FILLER     PIC X(8) VALUE "ENNN062N".
           03  FILLER     PIC X(8) VALUE "ENNN062N".
           03  FILLER     PIC X(8) VALUE "ENNN062N".
      * 08 PARTICIPATING RESERVE HELD
           03  FILLER     PIC X(8) VALUE "ENNN062N".
      * 09 PARTICIPATING RESERVE PAID
           03  FILLER     PIC X(8) VALUE "ENNN062N".
      * 10 DEALER DISCOUNT
           03  FILLER     PIC X(8) VALUE "ENNN062N".
      * 11 RECOURSE CODE
           03  FILLER     PIC X(8) VALUE "E  A010N".
      * 12 PATRTIAL RECOURSE AMOUNT
           03  FILLER     PIC X(8) VALUE "ENNN062N".
      * 13 DEALER POOL
           03  FILLER     PIC X(8) VALUE "ENNN050N".
      * 14 DEALER CHECK
           03  FILLER     PIC X(8) VALUE "ENNN062N".
      * 15 ORIGINAL PURCHASE AMOUNT
           03  FILLER     PIC X(8) VALUE "ENNN062N".
      * 16 ORIGINAL PURCHASE TERM
           03  FILLER     PIC X(8) VALUE "ENNN030N".
      * 17 PURCHASE BALANCE
           03  FILLER     PIC X(8) VALUE "ENNS062N".
      * 18. SP REFUND DATE
           03  FILLER     PIC X(8) VALUE "ERNM060N".
      * 19. CL REBATE
           03  FILLER     PIC X(8) VALUE "E  A010N".
      * 20. AH REBATE
           03  FILLER     PIC X(8) VALUE "E  A010N".
      * 21. PP REBATE
           03  FILLER     PIC X(8) VALUE "E  A010N".
      * 22. O1 REBATE
           03  FILLER     PIC X(8) VALUE "E  A010N".
      * 23. O2 REBATE
           03  FILLER     PIC X(8) VALUE "E  A010N".
      * 24. O3 REBATE
           03  FILLER     PIC X(8) VALUE "E  A010N".
      * 25. O4 REBATE
           03  FILLER     PIC X(8) VALUE "E  A010N".
      * 26. O5 REBATE
           03  FILLER     PIC X(8) VALUE "E  A010N".
      * 27. N/A
           03  FILLER     PIC X(8) VALUE "E  A010N".
      * 28. N/A
           03  FILLER     PIC X(8) VALUE "E  A010N".
      * 29. INT REBATE
           03  FILLER     PIC X(8) VALUE "E  A010N".
      * 30. SC REBATE
           03  FILLER     PIC X(8) VALUE "E  A010N".
      * 31. MF REBATE
           03  FILLER     PIC X(8) VALUE "E  A010N".
      * 32. DF REBATE
           03  FILLER     PIC X(8) VALUE "E  A010N".

       01  SPEC-TAB      REDEFINES SPEC-TABLE.
           03  SPEC-REC   OCCURS 32 TIMES.
               05  SPEC   PIC X(7).
               05  CHFG   PIC X.

      ***********************************************
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBMN/PDMAINW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/PDPG07_DEF.CPY".
       COPY "LIBMN/PDPG07_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/ACCESSW.CPY".

       COPY "LIBGB/GETFMW.CPY".
       01 FORM-LINK-AREA    PIC X(2000).

       SCREEN SECTION.
       COPY "LIBMN/PDPG07_SCN.CPY".

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
              MOVE 08  TO FORM-SEL1.

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

           PERFORM GET-GPENV.

       COPY "LIBGB/CHKSEC.CPY".
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "PDPG07" TO VDU-FORM-NAME.
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
           IF (ELE = 14) AND (PD-SALEFIN NOT = "Y")
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
              IF FORM-SEL1 < 1 OR FORM-SEL1 = 7 OR FORM-SEL1 > 9
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
           IF (CHNO = 14) AND (PD-SALEFIN NOT = "Y")
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
                 A-31 A-32
                 DEPENDING ON ELE.
           GO TO END-ENTRY-MODULE.

      *    **********************************
      *    *      SET RECORD ELEMENTS
      *    **********************************
       A-01.
           MOVE VDUNUM2 TO DL-DLNO.
           IF DL-DLNO = 0
              GO TO ENTER-ELE.
           PERFORM OPEN-DL1-FILE.
           PERFORM READ-DL1-FILE.
           PERFORM CLOSE-DL1-FILE.
           IF IO-FG NOT = 0
              MOVE "*  NOT ON FILE  *" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           MOVE VDUNUM2 TO PD-DLNO.
           GO TO CREATE-ROUTINE.
       A-02.
           MOVE VDUNUM2 TO PD-DLRVPRIN(1).
           GO TO CREATE-ROUTINE.
       A-03.
           MOVE VDUNUM2 TO PD-DLRVPRIN(2).
           GO TO CREATE-ROUTINE.
       A-04.
           MOVE VDUNUM2 TO PD-DLRVPRIN(3).
           GO TO CREATE-ROUTINE.
       A-05.
           MOVE VDUNUM2 TO PD-DLRVCHGS(1).
           GO TO CREATE-ROUTINE.
       A-06.
           MOVE VDUNUM2 TO PD-DLRVCHGS(2).
           GO TO CREATE-ROUTINE.
       A-07.
           MOVE VDUNUM2 TO PD-DLRVCHGS(3).
           GO TO CREATE-ROUTINE.
       A-08.
           MOVE VDUNUM2 TO PD-DLPARVHELD.
      * DLCOMM-PP-CMSI:
       A-081.
           MOVE "ENNN062081." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO A-081.
           MOVE VDUNUM2 TO PD-DLCOMM-PP-CMSI.
           GO TO CREATE-ROUTINE.
       A-09.
           MOVE VDUNUM2 TO PD-DLPARVPAID.
      * DLCOMM-O3-CMSI:
       A-091.
           MOVE "ENNN062091." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO A-091.
           MOVE VDUNUM2 TO PD-DLCOMM-O3-CMSI.
           GO TO CREATE-ROUTINE.
       A-10.
           MOVE VDUNUM2 TO PD-DLDISC.
           GO TO CREATE-ROUTINE.
       A-11.
           IF VDUBUF NOT = "P" AND VDUBUF NOT = "F"
              AND VDUBUF NOT = "N"
                 GO TO ENTER-ELE.
           MOVE VDUBUF TO PD-DLRECOURSE.
           GO TO CREATE-ROUTINE.
       A-12.
           MOVE VDUNUM2 TO PD-DLPARTRCAMT.
           GO TO CREATE-ROUTINE.
       A-13.
           MOVE VDUNUM0 TO PD-DLPOOL.
           GO TO CREATE-ROUTINE.
       A-14.
           MOVE VDUNUM2 TO PD-PROCEEDS.
           GO TO CREATE-ROUTINE.
       A-15.
           MOVE VDUNUM2 TO PD-ORIGPURAMT.
           GO TO CREATE-ROUTINE.
       A-16.
           IF VDUNUM0 > PD-ORGTERM
              MOVE " EXCEEDS ORGINAL TERM" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           MOVE VDUNUM0 TO PD-ANTICTERM.
           GO TO CREATE-ROUTINE.
       A-17.
           IF VDUNUM2 < 0
              MOVE "BELOW ZERO ALLOWED FOR REVOLVING ACCOUNTS"
                                                            TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           MOVE VDUNUM2 TO PD-PURBAL.
           GO TO CREATE-ROUTINE.
       A-18.
           MOVE VDU-DATE-YYYYMMDD TO PD-TRW-DOFD-DATE.
           GO TO CREATE-ROUTINE.
       A-19.
           IF VDUBUF NOT = " " AND NOT = "Y" AND NOT = "C"
              MOVE "ENTER 'Y', 'C' OR SPACE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           MOVE VDUBUF TO PD-CL-REBATE.
           GO TO CREATE-ROUTINE.
       A-20.
           IF VDUBUF NOT = " " AND NOT = "Y" AND NOT = "C"
              MOVE "ENTER 'Y', 'C' OR SPACE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           MOVE VDUBUF TO PD-AH-REBATE.
           GO TO CREATE-ROUTINE.
       A-21.
           IF VDUBUF NOT = " " AND NOT = "Y" AND NOT = "C"
              MOVE "ENTER 'Y', 'C' OR SPACE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           MOVE VDUBUF TO PD-PP-REBATE.
           GO TO CREATE-ROUTINE.
       A-22.
           IF VDUBUF NOT = " " AND NOT = "Y" AND NOT = "C"
              MOVE "ENTER 'Y', 'C' OR SPACE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           MOVE VDUBUF TO PD-O1-REBATE.
           GO TO CREATE-ROUTINE.
       A-23.
           IF VDUBUF NOT = " " AND NOT = "Y" AND NOT = "C"
              MOVE "ENTER 'Y', 'C' OR SPACE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           MOVE VDUBUF TO PD-O2-REBATE.
           GO TO CREATE-ROUTINE.
       A-24.
           IF VDUBUF NOT = " " AND NOT = "Y" AND NOT = "C"
              MOVE "ENTER 'Y', 'C' OR SPACE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           MOVE VDUBUF TO PD-O3-REBATE.
           GO TO CREATE-ROUTINE.
       A-25.
           IF VDUBUF NOT = " " AND NOT = "Y" AND NOT = "C"
              MOVE "ENTER 'Y', 'C' OR SPACE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           MOVE VDUBUF TO PD-O4-REBATE.
           GO TO CREATE-ROUTINE.
       A-26.
           IF VDUBUF NOT = " " AND NOT = "Y" AND NOT = "C"
              MOVE "ENTER 'Y', 'C' OR SPACE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           MOVE VDUBUF TO PD-O5-REBATE.
           GO TO CREATE-ROUTINE.
       A-27.
      *    IF VDUBUF NOT = " " AND NOT = "Y" AND NOT = "C"
      *       MOVE "ENTER 'Y', 'C' OR SPACE" TO MESS
      *       PERFORM SEND-MESS
      *       GO TO ENTER-ELE.
      *    MOVE VDUBUF TO PD-O6-REBATE.
           GO TO CREATE-ROUTINE.
       A-28.
      *    IF VDUBUF NOT = " " AND NOT = "Y" AND NOT = "C"
      *       MOVE "ENTER 'Y', 'C' OR SPACE" TO MESS
      *       PERFORM SEND-MESS
      *       GO TO ENTER-ELE.
      *    MOVE VDUBUF TO PD-O7-REBATE.
           GO TO CREATE-ROUTINE.
       A-29.
           IF VDUBUF NOT = " " AND NOT = "Y" AND
              NOT = "C" AND NOT = "R"
              MOVE "ENTER 'Y', 'C', 'R' OR SPACE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           MOVE VDUBUF TO PD-INT-REBATE.
           GO TO CREATE-ROUTINE.
       A-30.
           IF VDUBUF NOT = " " AND NOT = "Y" AND NOT = "C"
              MOVE "ENTER 'Y', 'C' OR SPACE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           MOVE VDUBUF TO PD-SC-REBATE.
           GO TO CREATE-ROUTINE.
       A-31.
           IF VDUBUF NOT = " " AND NOT = "Y" AND NOT = "C"
              MOVE "ENTER 'Y', 'C' OR SPACE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           MOVE VDUBUF TO PD-MF-REBATE.
           GO TO CREATE-ROUTINE.
       A-32.
           IF VDUBUF NOT = " " AND NOT = "Y" AND NOT = "C"
              MOVE "ENTER 'Y', 'C' OR SPACE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           MOVE VDUBUF TO PD-DF-REBATE.
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

      **********************************************
       DISPLAY-FIELDS SECTION.
      **********************************************
           MOVE PD-DLNO           TO F01.
           MOVE PD-DLRVPRIN(1)    TO F02.
           MOVE PD-DLRVPRIN(2)    TO F03.
           MOVE PD-DLRVPRIN(3)    TO F04.
           MOVE PD-DLRVCHGS(1)    TO F05.
           MOVE PD-DLRVCHGS(2)    TO F06.
           MOVE PD-DLRVCHGS(3)    TO F07.
           MOVE PD-DLPARVHELD     TO F08.
           MOVE PD-DLCOMM-PP-CMSI TO F081.
           MOVE PD-DLPARVPAID     TO F09.
           MOVE PD-DLCOMM-O3-CMSI TO F091.
           MOVE PD-DLDISC TO F10.
           MOVE PD-DLRECOURSE TO F11.
           MOVE PD-DLPARTRCAMT TO F12.
           MOVE PD-DLPOOL TO F13.
           IF PD-SALEFIN = "Y"
              MOVE PD-PROCEEDS TO F14.
           MOVE PD-ORIGPURAMT TO F15.
           MOVE PD-ANTICTERM TO F16.
           MOVE PD-PURBAL TO F17.
           MOVE PD-TRW-DOFD-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY      TO F18.

           MOVE PD-CL-REBATE TO F19.
           MOVE PD-AH-REBATE TO F20.
           MOVE PD-PP-REBATE TO F21.
           MOVE PD-O1-REBATE TO F22.
           MOVE PD-O2-REBATE TO F23.
           MOVE PD-O3-REBATE TO F24.
           MOVE PD-O4-REBATE TO F25.
           MOVE PD-O5-REBATE TO F26.
      *    MOVE PD-O6-REBATE TO F27.
      *    MOVE PD-O7-REBATE TO F28.
           MOVE PD-INT-REBATE TO F29.
           MOVE PD-SC-REBATE TO F30.
           MOVE PD-MF-REBATE TO F31.
           MOVE PD-DF-REBATE TO F32.
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

       COPY "LIBGB/GPENV.CPY".

      **************************************************************************
       FORM-RESET SECTION.
      **************************************************************************
           MOVE EXT-CONAME-CONAME TO PDPG07-CONAME.
           MOVE EXT-CONAME-SYSDATE TO PDPG07-SYSDATE.
           DISPLAY SPACES UPON PDMAIN-WINDOW-HANDLE
                               LINE 0 COL 0 ERASE SCREEN.
           DISPLAY PDPG07-SCREEN UPON PDMAIN-WINDOW-HANDLE.
           MOVE PDPG07-FORM-FIELDS TO WR-BUF.
           MOVE PDPG07-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM. 

           IF ( GP-DLR-DISP-TYPE = "C" )
              MOVE "COMMISSION" TO DLR-TYP-BUF
           ELSE
              MOVE "DISCOUNT  " TO DLR-TYP-BUF.

           MOVE DLR-TYP-BUF TO WR-BUF.
           MOVE DLR-TYP-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **************************************************************************
       SETUP-LINK-INFO SECTION.
      **************************************************************************
           MOVE PDPG07-HELP--FILE TO HELP-LINK-FILE.
           MOVE PDPG07-HELP--DIR TO HELP-LINK-DIR.

      **************************************************************************
       VDU-CONVERT-FIELD SECTION.
      **************************************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/PDPG07_EVA.CPY".
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
           PERFORM CLOSE-DL1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPPDGS_SQL.CPY".
       COPY "LIBLP/LPDLGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPPD1IN.CPY".
       COPY "LIBLP/LPDL1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************

           PERFORM CLOSE-FILES.
       EXIT-PROG.

       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY PDPG07-SCREEN.
           MOVE FORM-PATH TO FORM-PATHNAME.
           MOVE PDMAIN-LINK TO FORM-LINK-AREA.
           EXIT PROGRAM.
