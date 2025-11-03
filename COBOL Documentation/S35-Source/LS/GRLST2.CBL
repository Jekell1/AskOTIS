       IDENTIFICATION DIVISION.
       PROGRAM-ID.       GRLST2.
      **************************************************************
      *                 (LS)
      *
      *  DESCRIPTION:   GROUP FILE (AUDIT) LISTING
      *
      *  REV:
      *  BLV 990301 ADD BLANK LINE AT END SO PERUSE WILL WORK
      *  BLV 990930 ADD OPTION TO CHECK FOR TRAN FILES FOR BRANCHES
      *             MISSING FROM SELECTED GROUP. THIS IS MEANT TO
      *             HELP AUDIT LARGE MULTI-BRANCH CUSTOMERS WHO
      *             DO ALL THEIR MONTH-END PROCESSING BY GROUP.
      *  BLV 040322 IF I-AM-WORLD, PRINT BR-WORLD-BRANCH-TYPE &
      *             ACCUMULATE & PRINT TOTALS FOR DIFFERENT TYPES
      *  BLV 040330 IF I-AM-WORLD, ALSO PRINT & TOTAL JOURNAL
      *             ENTRY ALLOCATION SKIPS
      *  GLB 040526 FIX HEADINGS; ADD IF I-AM-WORLD, PRINT BRANCH
      *             INSURANCE PRODUCER (BR-INS-PRODUCER)
      *  BLF 061110 EXPANDED ALL GROUP SUBSCRIPTS TO ACCOMODATE
      *             EXPANDED GROUP SEARCH TABLES, WORLD PR# XXXX
      *  BAH 190426 CHANGED CHECK FOR TRAN FILES TO READ THE TRP1-FILE
      *             AND SEE IF ANY TRANSACTIONS FOR THE MONTH WERE 
      *             POSTED FOR GIVEN BRANCH
      * BAH 20190930 REMOVED ACCESS-CALL TRPFILE
      * BLM 20210810 REMOVE TP-CODE, TP-POOLID NO LONGER IN KEY
      **************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSWKI.CPY".
       COPY "LIBGB/GBFSPR.CPY".
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.

       COPY "LIBGB/GBFDPR.CPY".
      **************************************************
      *          WORK FILE                     *
      **************************************************
       FD  WK-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK-REC.
           03  WK-KEY.
               05  WK-BRNO              PIC 9(4).
               05  WK-SEQ               PIC 9.
           03  WK-SCURR-TABLE.
               05  WK-SCURR   PIC 9999  COMP  OCCURS 15.

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LS/GRLST2.CBL S35 08/17/22-12:40:33 >".

       COPY "LIBLP/LP01TRP.CPY".
       COPY "LIBLP/LP01TRP_SQL.CPY".
       COPY "LIBLP/LPWSTRP.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
      ***************************
      *  FILE PATH DEFINITIONS  *
      ***************************
       01  WK-IFN              PIC XX       VALUE "WK".
       01  PR-IFN              PIC X(11)    VALUE "0DPRINT".

       01  LN-WORKERS.
           03  LN-FEED         PIC 99       VALUE 0.
           03  LN-COUNT        PIC 99       VALUE 61.
           03  LN-WK           PIC 99       VALUE 0.
       01  PAGE-NUMBER         PIC 999      VALUE 0.

       01  SUB                 PIC 9999     VALUE 0.
       01  SUB2                PIC 9999     VALUE 0.
       01  HOLD-SEQ            PIC 9999     VALUE 0.
       01  TOTAL-BRANCHES      PIC 9999     VALUE 0.
       01  BRANCH-COUNT        PIC 9999     VALUE 0.
       01  ERROR-COUNT         PIC 9999     VALUE 0.
       01  DUP-COUNT           PIC 9999     VALUE 0.
       01  R1-COUNT            PIC 9999     VALUE 0.
       01  R2-COUNT            PIC 9999     VALUE 0.
       01  SKIP-COUNT          PIC 9999     VALUE 0.
       01  TYPE-A-COUNT        PIC 9999     VALUE 0.
       01  TYPE-1-COUNT        PIC 9999     VALUE 0.
       01  TYPE-2-COUNT        PIC 9999     VALUE 0.
       01  BAD-TYPE-COUNT      PIC 9999     VALUE 0.
       01  MISSING-COUNT       PIC 9999     VALUE 0.
       01  BAD-TR-COUNT        PIC 9999     VALUE 0.
       01  BAD-INS-COUNT       PIC 9999     VALUE 0.

       01  MESS-LIST           PIC X(5)     VALUE "MESS.".
       01  ERRCD               PIC X        VALUE SPACES.

       01  HOLD-GR-KEY         PIC X(4).
       01  PRINT-EXCEPTIONS    PIC X.
       01  TR-FG               PIC X.
       01  END-DATE.
           03  END-MONTH       PIC 9(2).
           03  END-YEAR        PIC 9(2).
       01  EDATE               REDEFINES END-DATE PIC 9(4).

       01  R1                  PIC XX VALUE X'7231'.
       01  R2                  PIC XX VALUE X'7232'.

      *    ****************************************
      *    *            PRINT BUFFERS
      *    ****************************************
       01  HEAD1.
           03  HD-DATE         PIC X(8)     VALUE SPACES.
           03  FILLER          PIC XX       VALUE SPACES.
           03  HD-TIME         PIC X(8).
           03  FILLER          PIC X(28)    VALUE SPACES.
           03  HD-NAME         PIC X(40)    VALUE SPACES.
           03  FILLER          PIC X(37)    VALUE SPACES.
           03  FILLER          PIC X(5)     VALUE "PAGE ".
           03  HD-PAGE-NO      PIC ZZZ9     VALUE SPACES.
       01  HEAD2.
           03  FILLER          PIC X(10)    VALUE "USER ID:".
           03  HD-USERID       PIC X(10).
           03  FILLER          PIC X(31)    VALUE SPACES.
           03  FILLER          PIC X(30)    VALUE
           "  BRANCH GROUP FILE LISTING ".
           03  FILLER          PIC X(45)    VALUE SPACES.
           03  FILLER          PIC X(6)     VALUE "GRLIST".
       01  HEAD3.
           03  FILLER          PIC X(6)     VALUE "BRNO ".
           03  FILLER          PIC X(6)     VALUE "MACH ".
           03  FILLER          PIC X(10)    VALUE "CITY/STATE".
           03  FILLER          PIC X(17)    VALUE SPACES.
           03  FILLER          PIC X(13)    VALUE "PARENT GROUPS".
           03  FILLER          PIC X(56)    VALUE SPACES.
           03  HEAD3-WORLD     PIC X(12).
           03  FILLER          PIC X(2)    VALUE SPACES.
           03  HEAD4-WORLD     PIC X(8).

       01  DETAIL-LINE                      VALUE SPACES.
           03  D-BRNO          PIC ZZZ9.
           03  FILLER          PIC X(3).
           03  D-MACHINE       PIC XX.
           03  FILLER          PIC XXX.
           03  D-BRNAME        PIC X(25).
           03  FILLER          PIC XX.
           03  D-MSG.
               05  D-PARENTS       OCCURS 15.
                   07  D-GROUP     PIC XXXX.
                   07  FILLER      PIC XX.
           03  WORLD-TYPES REDEFINES D-MSG.
               05  FILLER          PIC X(72).
               05  D-SKIP          PIC X.
               05  FILLER          PIC X(6).
               05  D-TYPE          PIC X.
               05  FILLER          PIC X(2).
               05  D-INS-PRODUCER  PIC Z(8).
           03  FILLER           PIC X(32).

       01  TOTAL-LINE REDEFINES DETAIL-LINE.
           03  FILLER          PIC X(12).
           03  D-TOTAL-DESC    PIC X(30).
           03  FILLER          PIC XXX.
           03  D-TOTAL         PIC ZZ,ZZZ.

       01  GRLST2-WINDOW-HANDLE PIC X(10).

       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/PHONE.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLS/GRLST2_DEF.CPY".
       COPY "LIBLS/GRLST2_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ****************************************
      *     P R O G R A M   C O N T R O L    *
      ****************************************
       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBLS/GRLST2_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               WK-FILE PR-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-TRP1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           CLOSE
               WK-FILE PR-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      ************************************************
       MAIN-MODULE SECTION.
      ************************************************
           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".

           MOVE "GRLST2" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS GRLST2-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           IF EXT-ROUTE-BUF NOT = "00"                                          
              MOVE "PROGRAM NOT BATCHABLE" TO MESS                              
              PERFORM SEND-MESS        
              MOVE "99" TO EXT-ROUTE-BUF                                        
              GO TO END-PROGRAM.                                                

           PERFORM INITIALIZATION.

       READ-GRFILE-NEXT.
           PERFORM SEARCH-GR-NEXT.
           IF WGR-SRESULT = 0
              GO TO PRINT-ROUTINE.

           MOVE 0 TO HOLD-SEQ.
       CREATE-WK-REC.
           MOVE WGR-SRESULT TO WK-BRNO.
           MOVE HOLD-SEQ TO WK-SEQ.
           MOVE WGR-SCURR-TABLE TO WK-SCURR-TABLE.
           PERFORM WRITE-WK-FILE.
           IF IO-FG = 9
              IF E-CODE NOT = "22"
                 GO TO IO-ERROR
              ELSE
                 IF HOLD-SEQ < 10
                    ADD 1 TO HOLD-SEQ
                    GO TO CREATE-WK-REC
                 ELSE
                    GO TO IO-ERROR.
           GO TO READ-GRFILE-NEXT.

       PRINT-ROUTINE.
           MOVE 0 TO WK-KEY WK-SEQ BRANCH-COUNT ERROR-COUNT
                                   R1-COUNT     R2-COUNT
                                   DUP-COUNT
                                   SKIP-COUNT
                                   TYPE-A-COUNT
                                   TYPE-1-COUNT
                                   TYPE-2-COUNT
                                   BAD-TYPE-COUNT
                                   BAD-INS-COUNT.
           PERFORM START-WK-FILE.

       NEXT-WORK-REC.
           PERFORM READ-WK-FILE-NEXT.
           IF IO-FG = 9
              GO TO TOTAL-ROUTINE.

           ADD 1 TO BRANCH-COUNT.

           MOVE WK-BRNO TO D-BRNO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-FG = 9
              MOVE "*** INVALID BRANCH!!! ***" TO D-BRNAME
              ADD 1 TO ERROR-COUNT
           ELSE
              MOVE BR-CITY TO D-BRNAME
              INSPECT D-BRNAME REPLACING FIRST "  " BY ", "
              INSPECT D-BRNAME REPLACING FIRST "  "
                 BY BR-STATE AFTER INITIAL " ".
           IF WK-SEQ > 0
              MOVE "*** DUPLICATE BRANCH! ***" TO D-BRNAME
              ADD 1 TO DUP-COUNT.
           IF BR-MACHINE = R1
              ADD 1 TO R1-COUNT
           ELSE IF BR-MACHINE = R2
              ADD 1 TO R2-COUNT
           ELSE
              ADD 1 TO ERROR-COUNT
              MOVE "*** INVALID MACHINE!! ***" TO D-BRNAME.
           MOVE BR-JE-ALLOCATION-SKIP TO D-SKIP.
           IF BR-JE-ALLOCATION-SKIP = "Y"
              ADD 1 TO SKIP-COUNT.

           MOVE BR-WORLD-BRANCH-TYPE TO D-TYPE.

           IF BR-WORLD-BRANCH-TYPE = "A"
              ADD 1 TO TYPE-A-COUNT
           ELSE IF BR-WORLD-BRANCH-TYPE = "1"
              ADD 1 TO TYPE-1-COUNT
           ELSE IF BR-WORLD-BRANCH-TYPE = "2"
              ADD 1 TO TYPE-2-COUNT
           ELSE IF BR-WORLD-BRANCH-TYPE NOT = " "
              ADD 1 TO BAD-TYPE-COUNT.

           MOVE BR-INS-PRODUCER-NUM TO D-INS-PRODUCER.

           MOVE BR-MACHINE TO D-MACHINE.
           MOVE 1 TO SUB.
       PARENT-LOOP.
           IF WK-SCURR(SUB) NOT = 0
              MOVE WK-SCURR(SUB) TO SUB2
              MOVE WGR-SID(SUB2) TO D-GROUP(SUB).
           ADD 1 TO SUB.
           IF SUB < 16
              GO TO PARENT-LOOP.
           PERFORM WRITE-DETAIL-LINE.
           GO TO NEXT-WORK-REC.

       TOTAL-ROUTINE.
           MOVE 0 TO BR-NO.
           PERFORM START-BR-FILE.

       COUNT-TOTAL-BRANCHES.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-FG = 9
              GO TO EXIT-TOTAL-BRANCHES.

           ADD 1 TO TOTAL-BRANCHES.

           IF PRINT-EXCEPTIONS = "N"
              GO TO COUNT-TOTAL-BRANCHES.

           MOVE BR-NO TO WK-BRNO.
           MOVE 0      TO WK-SEQ.
           PERFORM START-WK-FILE.
           PERFORM READ-WK-FILE-NEXT.
           IF IO-FG = 0
              IF WK-BRNO = BR-NO
                 GO TO COUNT-TOTAL-BRANCHES.

           ADD 1 TO MISSING-COUNT.

           IF MISSING-COUNT = 1
              MOVE 2 TO LN-FEED
              MOVE "**** MISSING BRANCHES ****" TO D-TOTAL-DESC
              PERFORM WRITE-DETAIL-LINE.

           IF TR-FG = "N"
              GO TO SKIP-TRFILE-CHECK.

           IF BR-MACHINE NOT = EXT-FILPATH-MACHINE
              GO TO SKIP-TRFILE-CHECK.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO TRP-PATH-OVERRIDE.

           PERFORM OPEN-TRP1-FILE.

           MOVE BR-NO          TO TP-BRNO
                                  QTRP1-WBEG-BRNO
                                  QTRP1-WEND-BRNO.
           MOVE END-MONTH      TO DATE-YYYYMMDD-MM.
           MOVE END-YEAR       TO DATE-YYYYMMDD-YY.
           MOVE 01             TO DATE-YYYYMMDD-DD.
           MOVE DATE-YYYYMMDD  TO TP-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRP1-WBEG-DATE.

           MOVE END-MONTH TO DATE-MMDDYY-MM.
           MOVE END-YEAR  TO DATE-MMDDYY-YY.
           MOVE 31        TO DATE-MMDDYY-DD.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO NDTE-DATE.
           PERFORM NEW-DATE-CALCULATION.
           MOVE NDTE-DATE     TO  SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRP1-WEND-DATE.

           MOVE 0                   TO TP-CLASS TP-ACCTNO TP-LINE
                                       QTRP1-WBEG-CLASS
                                       QTRP1-WBEG-ACCTNO
                                       QTRP1-WBEG-LINE.
           MOVE ALL "9"             TO QTRP1-WEND-CLASS
                                       QTRP1-WEND-ACCTNO
                                       QTRP1-WEND-LINE.

           MOVE TP-DATE              TO NDTE-DATE.
           MOVE 1                    TO NDTE-HOLD.
           PERFORM INCREMENT-DAYS.
           MOVE NDTE-DATE            TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QTRP1-WSBEG-DATE.

           IF ( TP-CLASS = 999 )
              MOVE TP-CLASS          TO QTRP1-WSBEG-CLASS
           ELSE
              COMPUTE QTRP1-WSBEG-CLASS  = TP-CLASS + 1.

           IF ( TP-ACCTNO = 99999999 )
              MOVE TP-ACCTNO         TO QTRP1-WSBEG-ACCTNO
           ELSE
              COMPUTE QTRP1-WSBEG-ACCTNO = TP-ACCTNO + 1.

           IF ( TP-LINE = 999 )
              MOVE TP-LINE           TO QTRP1-WSBEG-LINE
           ELSE
              COMPUTE QTRP1-WSBEG-LINE   = TP-LINE + 1.


           PERFORM START-TRP1-FILE.
           PERFORM READ-TRP1-FILE-NEXT.
           IF (IO-FG = 0) AND (TRP-PATH-OWNBR = TP-BRNO) 
              MOVE "TRANSACTION FOR MM/YY EXISTS!!!!!" TO D-MSG
              INSPECT D-MSG REPLACING FIRST "MM" BY END-MONTH
              INSPECT D-MSG REPLACING FIRST "YY" BY END-YEAR
              ADD 1 TO BAD-TR-COUNT.
           PERFORM CLOSE-TRP1-FILE.

       SKIP-TRFILE-CHECK.
           MOVE BR-NO TO D-BRNO.
           MOVE BR-MACHINE TO D-MACHINE.
           MOVE BR-CITY TO D-BRNAME.
           INSPECT D-BRNAME REPLACING FIRST "  " BY ", ".
           INSPECT D-BRNAME REPLACING FIRST "  "
              BY BR-STATE AFTER INITIAL " ".
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           GO TO COUNT-TOTAL-BRANCHES.
       EXIT-TOTAL-BRANCHES.
           MOVE 3 TO LN-FEED.
           MOVE "**** GROUP TOTALS ****" TO D-TOTAL-DESC.
           PERFORM WRITE-DETAIL-LINE.
           MOVE 2 TO LN-FEED.
           MOVE "TOTAL NO OF BRANCHES ON SYSTEM" TO D-TOTAL-DESC.
           MOVE TOTAL-BRANCHES TO D-TOTAL.
           PERFORM WRITE-DETAIL-LINE.
           MOVE 1 TO LN-FEED.
           MOVE "TOTAL NO. OF BRANCHES IN GROUP" TO D-TOTAL-DESC.
           MOVE BRANCH-COUNT TO D-TOTAL.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "TOTAL NO. OF R1 BRANCHES " TO D-TOTAL-DESC.
           MOVE R1-COUNT TO D-TOTAL.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "TOTAL NO. OF R2 BRANCHES " TO D-TOTAL-DESC.
           MOVE R2-COUNT TO D-TOTAL.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "TOTAL NO. OF ERRORS " TO D-TOTAL-DESC.
           MOVE ERROR-COUNT TO D-TOTAL.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "TOTAL NO. OF DUPLICATES " TO D-TOTAL-DESC.
           MOVE DUP-COUNT TO D-TOTAL.
           PERFORM WRITE-DETAIL-LINE.
           IF TR-FG = "Y"
              MOVE "TOTAL MISSING WITH TRFILES " TO D-TOTAL-DESC
              MOVE BAD-TR-COUNT TO D-TOTAL
              PERFORM WRITE-DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.
           IF SKIP-COUNT NOT = 0
              MOVE "TOTAL NO. JE ALLOCATION SKIPS" TO D-TOTAL-DESC
              MOVE SKIP-COUNT TO D-TOTAL
              PERFORM WRITE-DETAIL-LINE.
           IF TYPE-A-COUNT NOT = 0
              MOVE "TOTAL NO. STATE ADMIN BRANCHES" TO D-TOTAL-DESC
              MOVE TYPE-A-COUNT TO D-TOTAL
              PERFORM WRITE-DETAIL-LINE.
           IF TYPE-1-COUNT NOT = 0
              MOVE "TOTAL NO. FIRST YEAR DE NOVO'S" TO D-TOTAL-DESC
              MOVE TYPE-1-COUNT TO D-TOTAL
              PERFORM WRITE-DETAIL-LINE.
           IF TYPE-2-COUNT NOT = 0
              MOVE "TOTAL NO. 2ND YEAR DE NOVO'S  " TO D-TOTAL-DESC
              MOVE TYPE-2-COUNT TO D-TOTAL
              PERFORM WRITE-DETAIL-LINE.
           IF BAD-TYPE-COUNT NOT = 0
              MOVE "TOTAL NO. INVALID BRANCH TYPES" TO D-TOTAL-DESC
              MOVE BAD-TYPE-COUNT TO D-TOTAL
              PERFORM WRITE-DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           GO TO END-PROG.

      ****************************************
       INITIALIZATION SECTION.
      ****************************************
       SET-PR.
           MOVE GRLST2-QUEUE-POS TO Q-POS.
           MOVE GRLST2-COPIES-POS TO C-POS.
           MOVE GRLST2-PRU-POS TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
           PERFORM PRU-IN.
           IF STAT NOT = "00"
              GO TO END-PROGRAM.

       REC-GRID.
           MOVE "E  A000GRID." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO END-PROGRAM.

           IF VDUSTATUS = "1<"
              MOVE "WI/GRSCAN" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH GRSCAN-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              IF GRSCAN-STATUS NOT = "00"
                 GO TO REC-GRID
              ELSE
                 MOVE "S  A000GRID." TO VDU
                 MOVE GRSCAN-KEY TO VDUBUF
                 PERFORM SCREENIO.
           MOVE VDUBUF TO WGR-WORD HOLD-GR-KEY.
           MOVE 0 TO WGR-MIN-LEVEL.
           PERFORM CHECK-MEMBER.
           IF WGR-ERR > 0
              MOVE WGR-MSG(WGR-ERR) TO MESS
              PERFORM SEND-MESS
              GO TO REC-GRID.
       REC-EXCEPTIONS.
           MOVE "E  A000EXCP." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO END-PROGRAM.
           IF VDUSTATUS NOT = "00" GO TO REC-EXCEPTIONS.
           MOVE VDUBUF TO PRINT-EXCEPTIONS.
           IF PRINT-EXCEPTIONS = "N"
              GO TO ACCEPT-IT.
           IF PRINT-EXCEPTIONS NOT = "Y"
              GO TO REC-EXCEPTIONS.

       CHK-TRS-EXIST.
           MOVE "E  A000TRFG." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO END-PROGRAM.
           IF VDUSTATUS NOT = "00" GO TO CHK-TRS-EXIST.
           MOVE VDUBUF TO TR-FG.
           IF TR-FG = "N"
              GO TO ACCEPT-IT.
           IF TR-FG NOT = "Y"
              GO TO CHK-TRS-EXIST.

       DTE.
           MOVE "ENNN040ENDDATE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U" GO TO CHK-TRS-EXIST.
           IF VDUSTATUS = "1>" GO TO END-PROGRAM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO DTE.
           MOVE VDUNUM0 TO EDATE.

           IF END-MONTH < 1 OR > 12
              MOVE "INVALID MONTH" TO MESS
              PERFORM SEND-MESS
              GO TO DTE.

      *-----------------------------------------------------------------

       ACCEPT-IT.
           MOVE "E  A000ACCEPT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO END-PROGRAM.
           IF VDUSTATUS = "18" GO TO REC-GRID.
           IF VDUBUF = "Y" GO TO CONTINUE-IT.
           IF VDUBUF = "N" GO TO CONTINUE-IT.
           GO TO ACCEPT-IT.
       CONTINUE-IT.

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           PERFORM OPEN-WK-FILE-OUTPUT.
           PERFORM CLOSE-WK-FILE.
           PERFORM OPEN-WK-FILE.

           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-BR-FILE.
           MOVE HOLD-GR-KEY TO GR-ID.
           PERFORM SEARCH-GR-INIT.

           MOVE " LOC/GROUP FILE LISTING IN PROGRESS " TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           PERFORM OPEN-PR-FILE.

      *************************************
       WRITE-DETAIL-LINE SECTION.
      *************************************
           ADD LN-FEED LN-COUNT GIVING LN-WK.
           IF LN-WK GREATER THAN EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.
           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE SPACES TO DETAIL-LINE.
           MOVE 1 TO LN-FEED.

      *************************************
       HEAD-PAGE SECTION.
      *************************************
           ADD 1 TO PAGE-NUMBER.
           MOVE PAGE-NUMBER TO HD-PAGE-NO.
           PERFORM GET-TIME.
           MOVE TIME-EDIT TO HD-TIME.
           MOVE EXT-CONAME-CONAME TO HD-NAME.
           MOVE EXT-CONAME-USER-ID TO HD-USERID.
           MOVE EXT-CONAME-SYSDATE TO HD-DATE.
           MOVE HEAD1 TO PR-REC.
           MOVE 99 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE HEAD2 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE "JE SKIP TYPE" TO HEAD3-WORLD.
           MOVE "INS PROD"     TO HEAD4-WORLD.
           MOVE HEAD3 TO PR-REC.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.

       END-HEAD-PAGE.
           MOVE 2 TO LN-COUNT.
           MOVE 2 TO LN-FEED.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/SCREEN.CPY".

      ****************************************************
       FORM-RESET SECTION.
      ****************************************************
           MOVE EXT-CONAME-CONAME TO GRLST2-CONAME.
           MOVE EXT-CONAME-SYSDATE TO GRLST2-SYSDATE.
           DISPLAY GRLST2-SCREEN UPON GRLST2-WINDOW-HANDLE.
           MOVE GRLST2-FORM-FIELDS TO WR-BUF.
           MOVE GRLST2-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ****************************************************
       SETUP-LINK-INFO SECTION.
      ****************************************************
           MOVE GRLST2-HELP--FILE TO HELP-LINK-FILE.
           MOVE GRLST2-HELP--DIR  TO HELP-LINK-DIR.

      ****************************************************
       VDU-CONVERT-FIELD SECTION.
      ****************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLS/GRLST2_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.


      ****************************************************
       MISC SECTION.
      ****************************************************
       COPY "LIBGB/PHONEIO.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/GETENV.CPY".

      ****************************************************
       IO-ROUTINES SECTION.
      ****************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPTRPGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBLP/LPTRP1RN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBWKIN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      ****************************************************
       END-PROGRAM SECTION.
      ****************************************************
       END-PROG.
           PERFORM CLOSE-FILES.
           PERFORM PRU-OUT.
           MOVE MKTEMP-BUF TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.
       EXIT-PROG.
           MOVE "LP/LPL2MU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY GRLST2-SCREEN.
           DESTROY GRLST2-WINDOW-HANDLE.
           EXIT PROGRAM.
