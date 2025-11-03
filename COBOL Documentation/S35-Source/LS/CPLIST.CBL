       IDENTIFICATION DIVISION.
       PROGRAM-ID.   CPLIST.
       DATE-WRITTEN. 01-15-01
      ******************************************************************
      *
      *                      (LS)
      *  DESCRIPTION:  COLLECTOR POOLS LIST
      *
       
      * REV:
      * KEC 2018-1026 CONVERTED CPFILE FOR A32 & A34;
      *               CP-PATH-OVERRIDE TO "G" IF GLOBAL QUEUEING.
      * KEC 2019-0405 FIXED HOW TO CHECK AFTER READ-CP1-FILE-NEXT
      *               FOR GLOBAL AND LOCAL USAGE.
      * BAH 20190930 REMOVED ACCESS-CALL CPFILE
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".


       
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".

      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E - S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LS/CPLIST.CBL S35 02/21/24-12:03:56 >".

       
      *
      *    F I L E - W O R K E R S
      *
       
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".

       COPY "LIBSP/SP01CP.CPY".
       COPY "LIBSP/SP01CP_SQL.CPY".
       COPY "LIBSP/SPWSCP.CPY".

       
      *
      *    M I S C E L L A N E O U S - W O R K E R S
      *
       

       01  FIRST-TIME                      PIC X(01)    VALUE SPACES.
       01  FIRST-TIME-PRINT-COLLCD         PIC X        VALUE SPACES.

       01  LEGEND                          PIC X(60)    VALUE SPACES.
       01  LEGEND-LIST                     PIC X(07)    VALUE "LEGEND.".

       01  PROGRAM-EXIT-STATUS             PIC X(01)    VALUE "P".
           88  PROGRAM-EXIT-AFTER-RUN                   VALUE "A".
           88  PROGRAM-EXIT-PRIOR-RUN                   VALUE "P".

       01  PROGRESS-LINE-TEXT.
           03  FILLER                      PIC X(34)    VALUE
               "COLLECTOR POOL LISTING IN PROGRESS".
           03  FILLER                      PIC X(23)    VALUE
               ".......................".

       01  RECORDS-FOUND                   PIC X(01)    VALUE SPACES.
           88  NO-RECORDS-FOUND                         VALUE "N".

       01  SUBSCRIPTS.
           03  COLL-SUB                    PIC 9(2)     VALUE ZEROS.
           03  COLLECTORS-ON-LINE          PIC 9(2)     VALUE ZEROS.

       01  ERRCD                        PIC X(01).


       
      *
      *    S C R E E N - R A N G E S
      *
       
       01  SCREEN-RANGES.
           05  BEG-BRANCH               PIC 9(04).
           05  END-BRANCH               PIC 9(04).
           05  BEG-POOL                 PIC 9(03).
           05  END-POOL                 PIC 9(03).
           05  COLLECTOR-IDS            PIC X.
           05  INC-GLOBAL-POOL          PIC X.
           05  GROUP-FLAG               PIC X(01).
           05  ENT-GROUP                PIC X(04).

       
       01  SCREEN-RANGES-LIST.
           03  FILLER                   PIC X(50)      VALUE
               "BEG-BRANCH END-BRANCH BEG-POOL END-POOL".

           03  FILLER                   PIC X(30)      VALUE
               "COLLECTOR-IDS INC-GLOBAL-POOL.".

       
      *
      *    L I N E - W O R K E R S
      *
       
       01  LN-WORKERS.
           03  LN-FEED                       PIC 9(02)     VALUE ZEROES.
           03  LN-COUNT                      PIC 9(03)     VALUE 99.
           03  LN-WK                         PIC 9(03)     VALUE ZEROES.

       01  PAGE-NUMBER                       PIC 9(04)     VALUE ZEROES.

       
      *
      *    H E A D E R - B U F F E R S
      *
       
       01  HEAD1.
           03  H-DATE           PIC X(08)    VALUE SPACES.
           03  FILLER           PIC X(02)    VALUE SPACES.
           03  H-HR             PIC 9(02)    VALUE 0.
           03  FILLER           PIC X(01)    VALUE ":".
           03  H-MIN            PIC 9(02)    VALUE 0.
           03  FILLER           PIC X(31)    VALUE SPACES.
           03  H-NAME           PIC X(40)    VALUE SPACES.
           03  FILLER           PIC X(37)    VALUE SPACES.
           03  FILLER           PIC X(05)    VALUE "PAGE ".
           03  H-PAGE-NO        PIC ZZZ9.

       
       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(24)    VALUE SPACES.
           03  FILLER           PIC X(43)    VALUE
               " - - -   COLLECTOR POOL FILE LISTING  - - -".
           03  FILLER           PIC X(45)    VALUE SPACES.


       
       01  HEAD3.
           03  FILLER           PIC X(10)    VALUE "BRANCH :".
           03  H-BRNO           PIC 9(04).
           03  FILLER           PIC X(03)    VALUE SPACES.
           03  H-BRDESC         PIC X(40).
           03  FILLER           PIC X(75)    VALUE SPACES.

       
       01  HEAD4.
           03  FILLER            PIC X(18)    VALUE SPACES.
           03  FILLER            PIC X(9)     VALUE "COLLECTOR".
           03  FILLER            PIC X(07)    VALUE SPACES.
           03  FILLER            PIC X(9)     VALUE "POOL DESC".
           03  FILLER            PIC X(14)    VALUE SPACES.
           03  FILLER            PIC X(5)     VALUE "ALPHA".
           03  FILLER            PIC X(5)     VALUE SPACES.
       
       01  HEAD5.
           03  FILLER            PIC X(18)    VALUE SPACES.
           03  FILLER            PIC X(5)     VALUE "POOL ".
           03  H-POOL-TYPE       PIC X(08)    VALUE SPACES.
           03  FILLER            PIC X(5)     VALUE SPACES.
           03  FILLER            PIC X(9)     VALUE SPACES.
           03  FILLER            PIC X(12)    VALUE SPACES.
           03  FILLER            PIC X(4)     VALUE "FLAG".
       

       
      *
      *    D E T A I L - L I N E - B U F F E R
      *
       
       01  DETAIL-LINE                      PIC X(132)     VALUE SPACES.

       
       01  D-LINE1                REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(18).
           03  D-POOLID                     PIC X(4).
           03  FILLER                       PIC X(12).
           03  D-POOL-DESC                  PIC X(20).
           03  FILLER                       PIC X(3).
           03  D-ALPHA-FG                   PIC X.
           03  FILLER                       PIC X(5).
           03  D-COLLCD-PROMPT              PIC X(14).
           03  D-COLLCD                     PIC X(49).
       
       01  RANGE-LINE             REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(22).
           03  RANGE-SELECTION              PIC X(20).
           03  RANGE-BRANCH-NUM   REDEFINES RANGE-SELECTION.
               05  RANGE-BR-NO              PIC ZZZ9.
               05  FILLER                   PIC X(16).

       01  CPLIST-WINDOW-HANDLE PIC X(10).
      ******************************************************************
      *
      *     C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLS/CPLIST_DEF.CPY".
       COPY "LIBLS/CPLIST_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBLS/CPLIST_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE.
       
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-CP1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
       
           PERFORM CLOSE-PR-FILE.
       
       COPY "LIBGB/DECLAR2.CPY".
       
      ******************************************************************
      *
      *    M A I N - M O D U L E
      *
      ******************************************************************
       MAIN-MODULE SECTION.
           PERFORM SQL-CONNECT.
       
      D    STOP MESS.
           MOVE "CPLIST" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS CPLIST-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           PERFORM INITIALIZATION.
           IF ( ERRCD NOT = SPACES )
              GO TO END-PROGRAM.

           PERFORM PROCESS-POOLS-GLOBAL.

           PERFORM PROCESS-POOLS-LOCAL.

           PERFORM PRINT-RANGE-LINES.

       MAIN-PROGRAM-EXIT.

           GO TO END-PROGRAM.

      ********************************************************
       PROCESS-POOLS-GLOBAL SECTION.
      ********************************************************

           IF NOT ((BEG-BRANCH = 0 AND END-BRANCH = 0) OR
                   (BEG-BRANCH = 0 AND INC-GLOBAL-POOL = "Y"))
              GO TO PROCESS-POOLS-GB-EXIT.


           MOVE EXT-FILPATH-BASE           TO CP-GLOBAL-BASE.
           MOVE EXT-FILPATH-MACHINE        TO CP-GLOBAL-MACHINE.
           MOVE "G"                        TO CP-PATH-OVERRIDE.
           MOVE CP-GLOBAL-PATH             TO CP-PATH.

           PERFORM PROCESS-CP-FILE.

           PERFORM CLOSE-CP1-FILE.

           MOVE "Y" TO FIRST-TIME.

       PROCESS-POOLS-GB-EXIT.
           EXIT.

      *************************************************
       PROCESS-POOLS-LOCAL SECTION.
      *************************************************

           IF ( GROUP-FLAG = "N" )
              MOVE BEG-BRANCH TO BR-NO
              PERFORM START-BR-FILE
              GO TO NEXT-BRANCH-NO.

           MOVE ENT-GROUP TO GR-ID.
           PERFORM SEARCH-GR-INIT.

       NEXT-GROUP-LOCALITY.

           IF ( GROUP-FLAG NOT = "Y" )
              GO TO NEXT-BRANCH-NO.

           PERFORM SEARCH-GR-NEXT.

           IF ( WGR-SRESULT = ZEROES )
              GO TO PROCESS-POOLS-LOCAL-EXIT.

           MOVE WGR-SRESULT TO BR-NO.
           PERFORM READ-BR-FILE.

           IF ( IO-FG NOT = ZEROES )
              GO TO IO-ERROR.

           GO TO MOVE-BRANCH-PATH-INFO.

       NEXT-BRANCH-NO.

           PERFORM READ-BR-FILE-NEXT.
           IF ( IO-FG NOT = ZEROES ) OR ( BR-NO > END-BRANCH )
              GO TO PROCESS-POOLS-LOCAL-EXIT.

       MOVE-BRANCH-PATH-INFO.
           IF BR-GLOBAL-QUEUE = "Y"
              GO TO NEXT-GROUP-LOCALITY
           ELSE
              MOVE EXT-FILPATH-BASE        TO FILPATH-BASE
              MOVE BR-MACHINE              TO FILPATH-MACHINE
              MOVE BR-DATA-PATH            TO FILPATH-DATA
              MOVE "B"                     TO CP-PATH-OVERRIDE.

           PERFORM PROCESS-CP-FILE.

           PERFORM CLOSE-CP1-FILE.

           MOVE "Y" TO FIRST-TIME.

           GO TO NEXT-GROUP-LOCALITY.

       PROCESS-POOLS-LOCAL-EXIT.
           EXIT.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************
       COPY "LIBGB/CHKSEC.CPY".

           MOVE FORM-PATHNAME TO FORM-PATH.

           PERFORM ENTER-PARAMETERS.
           IF ( ERRCD = "X" )
              GO TO END-INITIALIZATION.

           MOVE PROGRESS-LINE-TEXT TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE EXT-CONAME-CONAME  TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-PR-FILE.

           MOVE "Y" TO FIRST-TIME.
           MOVE "A" TO PROGRAM-EXIT-STATUS.
           MOVE "N" TO RECORDS-FOUND.

           GO TO EXIT-INITIALIZATION.

       END-INITIALIZATION.

           MOVE "X" TO ERRCD.

       EXIT-INITIALIZATION.
           EXIT.

      ******************************************************************
       ENTER-PARAMETERS SECTION.
      ******************************************************************

           MOVE SPACES TO ERRCD.

       ENTER-PRINT-RANGE.
           MOVE CPLIST-QUEUE-POS TO Q-POS.
           MOVE CPLIST-COPIES-POS TO C-POS.
           MOVE CPLIST-PRU-POS TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

           PERFORM PRU-IN.
           IF ( STAT NOT = "00" )
              GO TO END-ENTER-PARAMETERS.

       ENTER-BEG-BRANCH.

           MOVE "R  A000BEG-BRANCH." TO VDU.
           PERFORM SCREENIO.

           IF ( F1-KEY ) GO TO ENTER-PRINT-RANGE.
           IF ( F3-KEY ) GO TO ALL-BRANCHES.
           IF ( F7-KEY ) GO TO END-ENTER-PARAMETERS.

           IF ( F6-KEY )
              MOVE "WI/GRSCAN" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH GRSCAN-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              IF GRSCAN-STATUS NOT = "00"
                 GO TO ENTER-BEG-BRANCH
              ELSE
                 MOVE "S  A000BEG-BRANCH." TO VDU
                 MOVE GRSCAN-KEY TO VDUBUF GR1-KEY
                 PERFORM SCREENIO
                 MOVE "00" TO VDUSTATUS.

           IF NOT ( DOWN-KEY OR ENTER-KEY ) GO TO ENTER-BEG-BRANCH.


           MOVE VDUBUF TO WGR-WORD ENT-GROUP.

           IF ( WGR-WORD-ZERO )
              MOVE ZEROES TO WGR-SRESULT
              GO TO BRANCH-IS-NUMERIC.

           MOVE ZEROES TO WGR-MIN-LEVEL.
           PERFORM CHECK-MEMBER.

           IF ( WGR-ERR > ZEROES )
              MOVE WGR-MSG(WGR-ERR) TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-BRANCH.

           IF ( WGR-TYPE = "A" )
              MOVE "S  A000END-BRANCH." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
              MOVE "Y"    TO GROUP-FLAG
              MOVE ZEROES TO BEG-BRANCH
              MOVE 9999   TO END-BRANCH
              GO TO CHECK-BRANCH-SECURITY.

       
       BRANCH-IS-NUMERIC.
           MOVE "N"                  TO GROUP-FLAG.
           MOVE WGR-SRESULT          TO BEG-BRANCH.
           MOVE "SN N040BEG-BRANCH." TO VDU.
           MOVE BEG-BRANCH           TO VDUNUM0.
           PERFORM SCREENIO.

       ENTER-END-BRANCH.
           MOVE "ENNN040END-BRANCH." TO VDU.
           PERFORM SCREENIO.

           IF (                    F1-KEY ) GO TO ENTER-PRINT-RANGE.
           IF (                    F7-KEY ) GO TO END-ENTER-PARAMETERS.
           IF (                    UP-KEY ) GO TO ENTER-BEG-BRANCH.
           IF NOT ( DOWN-KEY OR ENTER-KEY ) GO TO ENTER-END-BRANCH.

           MOVE VDUNUM0 TO END-BRANCH.

           IF ( BEG-BRANCH > END-BRANCH )
              MOVE "INVALID BRANCH RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-BRANCH.

           IF ( BEG-BRANCH = ZEROES ) AND
              ( END-BRANCH = ZEROES )
              MOVE "S  A200DISPLAY-TYPE." TO VDU
              MOVE "GLOBAL POOL ONLY"    TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "S  A200DISPLAY-TYPE." TO VDU
              MOVE SPACES                 TO VDUBUF
              PERFORM SCREENIO.


           GO TO CHECK-BRANCH-SECURITY.

       ALL-BRANCHES.
           MOVE "SNNN040BEG-BRANCH." TO VDU.
           MOVE ZEROES               TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.
       
           MOVE "SNNN040END-BRANCH." TO VDU.
           MOVE 9999                 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.
       
           MOVE "N"                    TO GROUP-FLAG.
           MOVE "S  A200DISPLAY-TYPE." TO VDU.
           MOVE SPACES                 TO VDUBUF.
           PERFORM SCREENIO.

       
       CHECK-BRANCH-SECURITY.

           PERFORM BR-RANGE-SECURITY.

           IF ( BR-SECURITY-OKAY = "N" )
              GO TO ENTER-BEG-BRANCH.
       
       CHECK-FOR-GLOBAL-POOLS.
           IF (BEG-BRANCH NOT = ZERO) OR
               (BEG-BRANCH = 0 AND END-BRANCH = 0)
                 MOVE "S  A200GLOBAL-PRMT." TO VDU
                 MOVE SPACES TO VDUBUF
                 PERFORM SCREENIO
                 MOVE "S  A010INC-GLOBAL." TO VDU
                 MOVE SPACES TO VDUBUF INC-GLOBAL-POOL
                 PERFORM SCREENIO
                 GO TO ENTER-BEG-CPOOL.

           MOVE "S  A200GLOBAL-PRMT." TO VDU.
           MOVE "INCLUDE GLOBAL POOL" TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "E  A010INC-GLOBAL." TO VDU.
           PERFORM SCREENIO.

           IF (  UP-KEY ) GO TO ENTER-END-BRANCH.

           IF VDUBUF NOT = "Y" AND NOT =  "N"
              GO TO CHECK-FOR-GLOBAL-POOLS.

           MOVE VDUBUF TO INC-GLOBAL-POOL.

       
       ENTER-BEG-CPOOL.

           MOVE "E  A030BEG-POOL." TO VDU.
           PERFORM SCREENIO.

           IF (                    F1-KEY ) GO TO ENTER-PRINT-RANGE.
           IF (                    F3-KEY ) GO TO ALL-CPOOLS.
           IF (                    F7-KEY ) GO TO END-ENTER-PARAMETERS.
           IF (                    UP-KEY ) GO TO ENTER-END-BRANCH.
           IF NOT ( DOWN-KEY OR ENTER-KEY ) GO TO ENTER-BEG-CPOOL.

           IF ( VDUBUF = SPACES )
              GO TO ENTER-BEG-CPOOL
           ELSE
              MOVE VDUBUF TO BEG-POOL.

       
       ENTER-END-CPOOL.

           MOVE "E  A010END-POOL." TO VDU.
           PERFORM SCREENIO.

           IF (                    F1-KEY ) GO TO ENTER-PRINT-RANGE.
           IF (                    F3-KEY ) GO TO ALL-CPOOLS.
           IF (                    F7-KEY ) GO TO END-ENTER-PARAMETERS.
           IF (                    UP-KEY ) GO TO ENTER-BEG-CPOOL.
           IF NOT ( DOWN-KEY OR ENTER-KEY ) GO TO ENTER-END-CPOOL.

           IF ( VDUBUF = SPACES )
              GO TO ENTER-END-CPOOL
           ELSE
              MOVE VDUBUF TO END-POOL.

           IF ( BEG-POOL > END-POOL )
               MOVE " INVALID CODE RANGE !" TO MESS
               PERFORM SEND-MESS
               GO TO ENTER-BEG-CPOOL.

           GO TO INC-COLLECTOR-IDS.

       
       ALL-CPOOLS.
           MOVE "S  A010BEG-POOL." TO VDU.
           MOVE  SPACES            TO VDUBUF BEG-POOL.
           PERFORM SCREENIO.

           MOVE "S  A010END-POOL." TO VDU.
           MOVE "ZZZ"              TO VDUBUF END-POOL.
           PERFORM SCREENIO.

       
       INC-COLLECTOR-IDS.

           MOVE "E  A010INC-COLLID." TO VDU.
           PERFORM SCREENIO.

           IF (                    F1-KEY ) GO TO ENTER-PRINT-RANGE.
           IF (                    F7-KEY ) GO TO END-INITIALIZATION.
           IF (                    UP-KEY ) GO TO ENTER-END-CPOOL.
           IF NOT ( DOWN-KEY OR ENTER-KEY ) GO TO INC-COLLECTOR-IDS.

           IF NOT ( VDUBUF = "Y" OR "N" )
              GO TO INC-COLLECTOR-IDS.

           MOVE VDUBUF TO COLLECTOR-IDS.

       
       ENTER-ACCEPT-YN.

           MOVE SPACES           TO ERRCD.
           MOVE "E  A010ACCEPT." TO VDU.
           PERFORM SCREENIO.

           IF (                    F1-KEY ) GO TO ENTER-PRINT-RANGE.
           IF (                    F7-KEY ) GO TO END-INITIALIZATION.
           IF (                    UP-KEY ) GO TO INC-COLLECTOR-IDS.
           IF NOT ( DOWN-KEY OR ENTER-KEY ) GO TO ENTER-ACCEPT-YN.

           IF ( VDUBUF         = "N"  ) GO TO ENTER-BEG-BRANCH.
           IF ( VDUBUF         = "Y"  ) GO TO EXIT-ENTER-PARAMETERS.

           GO TO ENTER-ACCEPT-YN.

       
       END-ENTER-PARAMETERS.

           MOVE "X" TO ERRCD.

       
       EXIT-ENTER-PARAMETERS.
           EXIT.

      ******************************************************************
       PROCESS-CP-FILE SECTION.
      ******************************************************************
       
           MOVE 99       TO LN-COUNT.
           IF CP-PATH-OVERRIDE-GLOBAL
              MOVE 0             TO CP-BRNO
           ELSE
              MOVE CP-PATH-OWNBR TO CP-BRNO.
           MOVE CP-BRNO  TO QCP1-WBEG-BRNO
                            QCP1-WEND-BRNO.
           MOVE BEG-POOL TO CP-ID
                            QCP1-WBEG-ID.
           MOVE END-POOL TO QCP1-WEND-ID.

           PERFORM OPEN-CP1-FILE.
           PERFORM START-CP1-FILE.

       NEXT-COLLECTOR-POOL.
           PERFORM READ-CP1-FILE-NEXT.
           IF ( IO-FG NOT = ZEROES )
               GO TO EXIT-PROCESS-CP-FILE.

           IF ( CP-PATH-OVERRIDE-GLOBAL ) AND
              ( CP-BRNO NOT = ZEROES    )
              MOVE 9 TO IO-FG
              GO TO EXIT-PROCESS-CP-FILE
           ELSE
              IF ( NOT CP-PATH-OVERRIDE-GLOBAL ) AND
                 ( CP-BRNO NOT = CP-PATH-OWNBR )
                 MOVE 9 TO IO-FG
                 GO TO EXIT-PROCESS-CP-FILE.

           IF ( CP-ID   > END-POOL )
               GO TO EXIT-PROCESS-CP-FILE.

           IF ( FIRST-TIME = "Y" )
              MOVE "N" TO FIRST-TIME
              MOVE "Y" TO RECORDS-FOUND.

           PERFORM DETAIL-LINE-PROCESS.

           GO TO NEXT-COLLECTOR-POOL.

       
       EXIT-PROCESS-CP-FILE.
           EXIT.

      ******************************************************************
       WRITE-DETAIL-LINE SECTION.
      ******************************************************************
       
           ADD LN-FEED LN-COUNT GIVING LN-WK.

           IF ( LN-WK > EXT-CONAME-RPT-LINES )
               PERFORM HEAD-PAGE.

           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.

           ADD LN-FEED      TO LN-COUNT.
           MOVE 1           TO LN-FEED.
           MOVE SPACES      TO DETAIL-LINE.
       
       EXIT-WRITE-DETAIL-LINE.
           EXIT.

      ******************************************************************
       HEAD-PAGE SECTION.
      ******************************************************************

           MOVE "N"         TO FIRST-TIME.
           ADD 1            TO PAGE-NUMBER.
           MOVE PAGE-NUMBER TO H-PAGE-NO.
           PERFORM GET-TIME.
           MOVE T-HH        TO H-HR.
           MOVE T-MM        TO H-MIN.

           MOVE HEAD1       TO PR-REC.
           MOVE 99          TO LN-FEED.
           PERFORM WRITE-PR-REC.

       
           MOVE HEAD2 TO PR-REC.
           MOVE 1     TO LN-FEED.
           PERFORM WRITE-PR-REC.

       
           MOVE BR-NO   TO H-BRNO.
           MOVE BR-NAME TO H-BRDESC
           MOVE HEAD3   TO PR-REC.
           MOVE 1       TO LN-FEED.
           PERFORM WRITE-PR-REC.

       
           MOVE HEAD4 TO PR-REC.
           MOVE 2     TO LN-FEED.
           PERFORM WRITE-PR-REC.


       
           IF BR-NO > 0
              MOVE SPACES TO H-POOL-TYPE
           ELSE
              MOVE "[GLOBAL]" TO H-POOL-TYPE.
           MOVE HEAD5 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

       
           MOVE ALL "=" TO PR-REC.
           MOVE 1       TO LN-FEED
           PERFORM WRITE-PR-REC.

       
       END-HEAD-PAGE.
           MOVE 7 TO LN-COUNT.
           MOVE 1 TO LN-FEED.
       
       EXIT-HEAD-PAGE.
           EXIT.

      ******************************************************************
       DETAIL-LINE-PROCESS SECTION.
      ******************************************************************
       
           MOVE CP-ID   TO D-POOLID.
           MOVE CP-DESC TO D-POOL-DESC.
           MOVE CP-ALPHA-FLAG TO D-ALPHA-FG.
           MOVE 2 TO LN-FEED.
           IF COLLECTOR-IDS NOT = "Y"
              PERFORM WRITE-DETAIL-LINE
              GO TO EXIT-DETAIL-LINE-PROCESS.

       
           MOVE "Y"   TO FIRST-TIME-PRINT-COLLCD.
           MOVE ZEROS TO COLLECTORS-ON-LINE.
           MOVE ZEROS TO COLL-SUB.

       
       PRINT-COLLECTOR-IDS.

           ADD 1 TO COLL-SUB.

           IF ( COLL-SUB > 80                ) OR
              ( CP-COL-ID(COLL-SUB) = SPACES )
              GO TO FINISH-PRINT-COLLECTOR-IDS.

       
           INSPECT CP-COL-ID(COLL-SUB) REPLACING ALL " " BY "~".

           STRING D-COLLCD," ", CP-COL-ID(COLL-SUB)
                  DELIMITED BY "  " INTO D-COLLCD.

           ADD 1 TO COLLECTORS-ON-LINE.

           IF ( COLLECTORS-ON-LINE < 12 )
              GO TO PRINT-COLLECTOR-IDS.

       
           IF ( FIRST-TIME-PRINT-COLLCD = "Y" )
              MOVE "N" TO FIRST-TIME-PRINT-COLLCD
              MOVE "POOL MEMBERS: " TO D-COLLCD-PROMPT
           ELSE
              MOVE "[ *M O R E *] " TO D-COLLCD-PROMPT.

           INSPECT D-COLLCD REPLACING ALL "~" BY " ".
           PERFORM WRITE-DETAIL-LINE.
           MOVE ZERO TO COLLECTORS-ON-LINE.

           GO TO PRINT-COLLECTOR-IDS.

       
       FINISH-PRINT-COLLECTOR-IDS.

           IF ( D-COLLCD = SPACES             ) AND
              ( FIRST-TIME-PRINT-COLLCD = "N" )
              GO TO EXIT-DETAIL-LINE-PROCESS.

           IF ( FIRST-TIME-PRINT-COLLCD = "Y" )
              MOVE "POOL MEMBERS: " TO D-COLLCD-PROMPT
           ELSE
              MOVE "[ *M O R E *] " TO D-COLLCD-PROMPT.

           INSPECT D-COLLCD REPLACING ALL "~" BY " ".
           PERFORM WRITE-DETAIL-LINE.

       EXIT-DETAIL-LINE-PROCESS.
           EXIT.

      ******************************************************************
       PRINT-RANGE-LINES SECTION.
      ******************************************************************
       
           MOVE ALL "-" TO DETAIL-LINE.
           MOVE 5       TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
       
           IF ( GROUP-FLAG = "Y" )
              MOVE "GROUP ID             :" TO RANGE-LINE
              MOVE ENT-GROUP              TO RANGE-SELECTION
              MOVE 1                      TO LN-FEED
              PERFORM WRITE-DETAIL-LINE
           ELSE
              MOVE "BEGINNING BRANCH    :" TO RANGE-LINE
              MOVE BEG-BRANCH             TO RANGE-BR-NO
              MOVE 1                      TO LN-FEED
              PERFORM WRITE-DETAIL-LINE
              MOVE "ENDING    BRANCH    :" TO RANGE-LINE
              MOVE END-BRANCH             TO RANGE-BR-NO
              MOVE 1                      TO LN-FEED
              PERFORM WRITE-DETAIL-LINE.

           IF INC-GLOBAL-POOL = "Y" OR "N"
              MOVE "INCLUDE GLOBAL POOL :"  TO RANGE-LINE
              MOVE INC-GLOBAL-POOL         TO RANGE-SELECTION
              MOVE 1 TO LN-FEED
              PERFORM WRITE-DETAIL-LINE.
       
           MOVE "BEGINNING POOL ID   :" TO RANGE-LINE.
           MOVE BEG-POOL               TO RANGE-SELECTION.
           MOVE 1                      TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

       
           MOVE "ENDING POOL         :" TO RANGE-LINE.
           MOVE END-POOL               TO RANGE-SELECTION.
           MOVE 1                      TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

       
           MOVE "INCLUDE POOL MEMBERS:" TO RANGE-LINE.
           MOVE COLLECTOR-IDS          TO RANGE-SELECTION.
           MOVE 1                      TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

       
       EXIT-PRINT-RANGE-LINES.
           EXIT.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/BRSECURE.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/TIMEIO.CPY".

      ****************************************************
       FORM-RESET SECTION.
      ****************************************************
           MOVE EXT-CONAME-CONAME TO CPLIST-CONAME.
           MOVE EXT-CONAME-SYSDATE TO CPLIST-SYSDATE.
           DISPLAY CPLIST-SCREEN UPON CPLIST-WINDOW-HANDLE.
           MOVE CPLIST-FORM-FIELDS TO WR-BUF.
           MOVE CPLIST-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ****************************************************
       SETUP-LINK-INFO SECTION.
      ****************************************************
           MOVE CPLIST-HELP--FILE TO HELP-LINK-FILE.
           MOVE CPLIST-HELP--DIR  TO HELP-LINK-DIR.

      ****************************************************
       VDU-CONVERT-FIELD SECTION.
      ****************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLS/CPLIST_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISCELLANEOUS-PARAGRAPHS SECTION.
      ******************************************************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETPR.CPY".
       
       SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE LEGEND-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ******************************************************************
       IO-ROUTINE SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBSP/SPCPGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBGB/GBPRINT.CPY".

       COPY "LIBSP/SPCP1RN.CPY".

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************
       
       END-RUN.

           PERFORM CLOSE-FILES.

           IF ( NO-RECORDS-FOUND )
              MOVE " NO RECORDS FOUND FOR SELECTED RANGE !" TO MESS
              PERFORM SEND-MESS
              GO TO EXIT-PROG.

           IF ( PROGRAM-EXIT-AFTER-RUN )
              PERFORM PRU-OUT.

       
       EXIT-PROG.
           MOVE "LP/LPLSMU" TO FORM-NAM.
           MOVE FORM-PATH   TO FORM-PATHNAME.

       
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY CPLIST-SCREEN.
           DESTROY CPLIST-WINDOW-HANDLE.
           EXIT PROGRAM.
