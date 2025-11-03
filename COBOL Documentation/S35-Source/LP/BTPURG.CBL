       IDENTIFICATION  DIVISION.
       PROGRAM-ID.       BTPURG.
       AUTHOR.              MJD.
       DATE-WRITTEN. 10/12/1999.
      ******************************************************************
      *
      *  DESCRIPTION:  PROGRAM TO PURGE PAYMENTS, PAYMENT TRAILERS,
      *                AND MEMOS FOR LOANS TRANSFERRED TO ANOTHER BRANCH.
      *     LEAVES THE LOAN AND THE LOAN TRAILERS, SO THEY CAN HAVE A
      *     TRAIL BACK TO THIS LOAN. DONT NEED ALL THE PAYMENTS OR MEMOS
      *                SELECT TRANSFER DATE AND WHETHER TO PURGE
      *                MEMO'S AND/OR PAYMENTS. DO NOT PURGE MEMOS
      *                AND PAYMENTS REFLECTING TRANSFERS (POCD = BT)
      *                ALLOW BRANCH/GROUP AND ACCOUNT NUMBER RANGES.
      *
      *-----------------------------------------------------------------
      * REV: 020900 KEEP BOTH TFR FR AND TFR TO MEMO, WE WE CAN TRACE
      *             BACK AN ACCOUNT
      *  MJD 000417 FIXED BUG, PAYMENTS WERE ONLY GETTING DELETED FOR
      *             FIRST ACCOUNT PROCESSED.
      *  BLV 000419 FIXED BUG, LOANS WERE DELETING FROM YOUR FIL
      *             BRANCH, NOT YOUR BRANCH RANGE !! LPFXLN COPY MEMBER
      *             STILL HAD RELATIVE PATH ../../LP/LNFILE, CHANGED
      *             TO USE LPFSLN
      *  MJD 000424 CHANGED TOTAL COUNTS TO BE 9(10) (WAS 9(06).
      *             FIXED BUG IN RANGE LINES. IT WAS NOT PRINTING GROUP
      *             NAME.
      *
      * KEC 2000.0426 {PR#00000} MERCURY FINANCE - CINDY;
      *                FIXED A PROBLEM OF ENTERING THE PARAMETERS WITHIN
      *                A BATCH AND IT WAS SKIPPING THE QUESTIONS ON WHAT
      *                TO PURGE.  THIS PER CINDY!
      *
      * BAH 170509 ACTIVATE 8 DIGIT DATES, PD0006
      *
      * KEC 2017.0613 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *         ADDED LIBLP/TFRMEMOW; USED TO CREATE/CHECK MEMOS (BM/LM)
      *         
      * BAH 181127 GLOBAL LMFILE
      * BAH 181203 GLOBAL LNFILE
      * BAH 181221 GLOBAL LPFILE
      * BAH 190116 GLOBAL LXFILE, SPLIT
      * BAH 20190922 REMOVED ACCESS-CALL LNFILE
      * BAH 20200827 REMOVED RESET ON BR1 
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
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
           "@(#)LP/BTPURG.CBL J35 12/21/23-08:48:40 >".

      *-----------------------------------------------------------------
      *
      * FILE WORKERS
      *
      *-----------------------------------------------------------------
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01LM.CPY".
       COPY "LIBLP/LP01LM_SQL.CPY".
       COPY "LIBLP/LPWSLM.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01LP.CPY".
       COPY "LIBLP/LP01LP_SQL.CPY".
       COPY "LIBLP/LPWSLP.CPY".
       COPY "LIBLP/LP01LXE.CPY".
       COPY "LIBLP/LP01LXE_SQL.CPY".
       COPY "LIBLP/LPWSLXE.CPY".
       COPY "LIBLP/LP01LXG.CPY".
       COPY "LIBLP/LP01LXG_SQL.CPY".
       COPY "LIBLP/LPWSLXG.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".

      *-----------------------------------------------------------------
      *
      * MISCELLANEOUS WORKERS
      *
      *-----------------------------------------------------------------

       01  FIRST-MEMO                   PIC X(01)         VALUE "N".
       01  FIRST-NAME                   PIC X(16).
       01  FIRST-TIME                   PIC X(01)         VALUE "Y".

       01  LEGEND                       PIC X(60)         VALUE SPACES.
       01  MESS-LIST                    PIC X(5)     VALUE "MESS.".

       01  NAME-WORKER                  PIC X(34).

       01  PROGRESS-LINE.
           03  FILLER                   PIC X(08)         VALUE
                                                             "BRANCH: ".
           03  PROGRESS-LINE-BRANCH     PIC 9(04)         VALUE ZEROES.
           03  PROGRESS-LINE-BRANCHX    REDEFINES PROGRESS-LINE-BRANCH
                                        PIC X(04).
           03  PROGRESS-LINE-TEXT       PIC X(48)         VALUE SPACES.


       01  PROGRESS-MESSAGE-PURGE       PIC X(48)         VALUE
           " ----- PURGE IN PROGRESS........................".

       01  PROGRESS-MESSAGE-P-AND-R      PIC X(48)         VALUE
           " ----- PURGE AND REPORT IN PROGRESS.............".

       01  PROGRESS-MESSAGE-REPORT      PIC X(48)         VALUE
           " ----- REPORT IN PROGRESS.......................".

       01  TOTAL-TABLE              OCCURS 3.
           03  PURGED-COUNT             PIC 9(10)  COMP-3 VALUE ZEROES.
           03  PURGED-LM                PIC 9(10)  COMP-3 VALUE ZEROS.
           03  PURGED-LP                PIC 9(10)  COMP-3 VALUE ZEROS.
           03  PURGED-LX                PIC 9(10)  COMP-3 VALUE ZEROS.

       01  RECORDS-FOUND                PIC X(01)         VALUE SPACES.
           88  NO-RECORDS-FOUND                           VALUE "N".
           88  SKIP-RECORDS-FOUND                         VALUE SPACES.

       01  HOLD-SYSDATE                      PIC 9(8).

      *-----------------------------------------------------------------
      *
      * SCREEN FIELDS
      *
      *-----------------------------------------------------------------
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  ACTION                        PIC X(01).
               88  ACTION-BOTH                        VALUE "B".
               88  ACTION-PURGE                       VALUE "P".
               88  ACTION-REPORT                      VALUE "R".
               88  ACTION-VALID                       VALUE "B" "P" "R".
           03  BEG-BRANCH                    PIC 9(04).
           03  BEG-ACCT                      PIC 9(8).
           03  END-BRANCH                    PIC 9(04).
           03  END-ACCT                      PIC 9(8).
           03  BEG-PO-DATE                   PIC 9(8).
           03  END-PO-DATE                   PIC 9(8).
           03  PURGE-MEMOS-YN                PIC X(01).
           03  PURGE-PAYMENTS-YN             PIC X(01).
           03  GROUP-OPTION-BUFFERS.
               05  GROUP-FLAG                PIC X(01).
               05  ENT-GROUP                 PIC X(04).

       01  VERSION-CONTROL                   PIC X(01)     VALUE "A".

       01  SAVE-BEG-BRANCH                   PIC 9(04)     VALUE ZEROES.
       01  SAVE-END-BRANCH                   PIC 9(04)     VALUE ZEROES.

       01  LN-FEED                       PIC 9(02)     VALUE ZEROES.
       01  LN-COUNT                      PIC 9(02)     VALUE ZEROES.

       01  PAGE-NUMBER                       PIC 9(04)     VALUE ZEROES.

      *-----------------------------------------------------------------
      *
      *    H E A D E R - B U F F E R S
      *
      *-----------------------------------------------------------------
       01  HEAD1.
           03  H-DATE                      PIC X(08)    VALUE SPACES.
           03  FILLER                      PIC X(02)   VALUE SPACES.
           03  H-HR                        PIC 9(02)   VALUE ZEROES.
           03  FILLER                      PIC X(01)   VALUE ":".
           03  H-MIN                       PIC 9(02)   VALUE ZEROES.
           03  FILLER                      PIC X(31)   VALUE SPACES.
           03  H-NAME                      PIC X(40)   VALUE SPACES.
           03  FILLER                      PIC X(37)   VALUE SPACES.
           03  FILLER                      PIC X(05)   VALUE "PAGE ".
           03  H-PAGE-NO                   PIC ZZZ9.

      *-----------------------------------------------------------------
       01  HEAD2.
           03  FILLER                      PIC X(10)   VALUE "USER ID:".
           03  H-USERID                    PIC X(10).
           03  FILLER                      PIC X(26)   VALUE SPACES.
           03  FILLER                      PIC X(41)   VALUE
               "- - - LOAN MASTER PURGE REPORT - - -".
           03  FILLER                      PIC X(45)   VALUE SPACES.

      *-----------------------------------------------------------------
       01  HEAD3.
           03  FILLER                      PIC X(10)  VALUE "BRANCH:".
           03  H-BRNO                      PIC Z(04).
           03  FILLER                      PIC X(03).
           03  H-BRDESC                    PIC X(30).
           03  FILLER                      PIC X(10)  VALUE SPACES.
           03  FILLER                      PIC X(57)  VALUE
           "                                         ------ PURGED FI".
           03  FILLER                      PIC X(11)  VALUE
           "ILES ------".
       01  HEAD4.
           03  FILLER                      PIC X(57)  VALUE
           "ACCOUNT           SOCIAL SEC       NAME                  ".
           03  FILLER                      PIC X(57)  VALUE
           "                  MAKERCD   PO DATE      MEMOS   PAYMENTS".
           03  FILLER                      PIC X(10)  VALUE
           "  TRAILERS".
      *-----------------------------------------------------------------
       01  HEAD7.
           03  FILLER                      PIC X(46)   VALUE
               "----- LOAN ENDORSERS ------  BORROWER-SSNO".

           03  FILLER                      PIC X(51)   VALUE
               "BORROWER-NAME           ACCT-NO       LOAN-DATE".

           03  FILLER                      PIC X(35)   VALUE
               "OBLIGATED-RATE     OUTSTANDING-BAL".

      *-----------------------------------------------------------------
       01  HEAD8.
           03  FILLER                      PIC X(52)   VALUE
               " TRAN   PAY  IB TR          TRANS    --- APPLIED ---".

           03  FILLER                      PIC X(17)   VALUE SPACES.

           03  FILLER                      PIC X(23)   VALUE
               "------- BALANCES ------".

           03  FILLER                      PIC X(38)   VALUE SPACES.

      *-----------------------------------------------------------------
       01  HEAD9.
           03  FILLER                      PIC X(52)   VALUE
           " DATE   DATE PC CD REFNO   AMOUNT    INT/LC     PRIN".

           03  FILLER                      PIC X(42)   VALUE
           "  PDTH LCPD      OTH       INT      PRIN  ".

           03  FILLER                      PIC X(31)   VALUE
           "  EARNED  PROCEEDS      GL-DIST".

      *-----------------------------------------------------------------
       01  HEAD-10.
           03  FILLER            PIC X(12)   VALUE "DESCRIPTION ".
           03  FILLER            PIC X(18)   VALUE "MAKE              ".
           03  FILLER            PIC X(18)   VALUE "MODEL             ".
           03  FILLER            PIC X(06)   VALUE "YEAR  ".
           03  FILLER            PIC X(10)   VALUE "STYLE     ".
           03  FILLER            PIC X(10)   VALUE "CLASS     ".
           03  FILLER            PIC X(18)   VALUE "IDENTIFICATION NO ".

      *-----------------------------------------------------------------
       01  TEXT2A.
           03  FILLER                      PIC X(48)   VALUE
               "    CO OURS EFF DATE EXP DATE   TERM  COVERAGE".

           03  FILLER                      PIC X(16)   VALUE
               "PREMIUM    COMM.".

      *-----------------------------------------------------------------
       01  TEXT2B.
           03  FILLER                        PIC X(54)     VALUE
               "CD DESCRIPTION        AMOUNT O  CD DESCRIPTION".

           03  FILLER                        PIC X(9)      VALUE
               "AMOUNT O".

      *-----------------------------------------------------------------
       01  TEXT3A.
           03  FILLER                        PIC X(50)     VALUE
               " ORIG ANTIC - ADJ ANTIC = NET ANTIC  - EARNINGS".

           03  FILLER                        PIC X(23)     VALUE
               "= UNEARNED    ..UPFRONT".

      *-----------------------------------------------------------------
       01  TEXT3B.
           03  FILLER                        PIC X(50)     VALUE
               "  --------- - --------- = ---------- - ----------".

           03  FILLER                        PIC X(23)     VALUE
               "= ----------  ---------".

      *-----------------------------------------------------------------
      *
      * DETAIL LINE BUFFER
      *
      *-----------------------------------------------------------------
       01  DETAIL-LINE                      PIC X(132)     VALUE SPACES.
       01  D-LINE                 REDEFINES DETAIL-LINE.
           03  D-ACCTNO                     PIC X(16).
           03  FILLER                       PIC X(02).
           03  D-SSNO                       PIC 999/99/9999.
           03  FILLER                       PIC X(06).
           03  D-NAME                       PIC X(36).
           03  FILLER                       PIC X(06).
           03  D-MAKERCD                    PIC X(01).
           03  FILLER                       PIC X(06).
           03  D-PODATE                     PIC 99/99/99.
           03  D-P-LM                       PIC Z(10).
           03  D-P-LP                       PIC Z(10).
           03  D-P-LX                       PIC Z(10).

      *-----------------------------------------------------------------
       78  78-TASK        VALUE "TASK              :".
       78  78-GROUP       VALUE "GROUP             :".
       78  78-BEG-BR      VALUE "BEGINNING BRANCH  :".
       78  78-END-BR      VALUE "ENDING            :".
       78  78-BEG-ACCT    VALUE "BEGINNING ACCOUNT :".
       78  78-END-ACCT    VALUE "BEGINNING ACCOUNT :".
       78  78-BEG-DATE    VALUE "BEGINNING PO DATE :".
       78  78-END-DATE    VALUE "BEGINNING PO DATE :".
       78  78-SYSDATE     VALUE "SYSTEM DATE       :".
       01  RANGE-LINE             REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(19).
           03  RANGE-SELECTION.
               04  R-ALPHA                  PIC X(10).
               04  R-NUMERIC      REDEFINES R-ALPHA
                                            PIC 9(10).
               04  R-DATEX        REDEFINES R-ALPHA.
                   05 R-DATE                PIC 9999/99/99.
       01  FILLER REDEFINES DETAIL-LINE.
           03  FILLER                   PIC X(21).
           03  R-ACCT                   PIC Z(10).
           03  FILLER                   PIC X(101).

      ******************************************************************
      *
      * COPY LIBRARY WORKERS
      *
      ******************************************************************
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBLP/LPEARNW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBLP/LPSTATW.CPY".
       COPY "LIBLP/TFRMEMOW.CPY".
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/BTPURG_DEF.CPY".
       COPY "LIBLP/BTPURG_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBLP/BTPURG_SCN.CPY".

      *----------------------------------------------------------------
       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LXG1-FILE.
           PERFORM CLOSE-LXE1-FILE.
           PERFORM CLOSE-LP1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-LM1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ******************************************************************
      *
      * MAIN MODULE
      *
      ******************************************************************
       MAIN-MODULE SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "BTPURG"      TO FORM-PROG VDU-FORM-NAME.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = SPACES
              GO TO END-PROGRAM.

           PERFORM GET-GPENV.

      *-----------------------------------------------------------------
       BEGIN-GROUP-LOCALITY.

           IF GROUP-FLAG = "N"
              MOVE BEG-BRANCH TO BR-NO
              PERFORM START-BR-FILE
              GO TO NEXT-BRANCH-NO.

           MOVE ENT-GROUP TO GR-ID.
           PERFORM SEARCH-GR-INIT.

       NEXT-GROUP-LOCALITY.
           IF GROUP-FLAG NOT = "Y"
              GO TO NEXT-BRANCH-NO.

           PERFORM SEARCH-GR-NEXT.

           IF WGR-SRESULT = ZEROES
              GO TO END-MAIN-MODULE.

           MOVE WGR-SRESULT TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

           GO TO MOVE-BRANCH-PATH-INFO.

      *-----------------------------------------------------------------
       NEXT-BRANCH-NO.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-FG = 9  OR BR-NO > END-BRANCH
              GO TO END-MAIN-MODULE.

      *-----------------------------------------------------------------
       MOVE-BRANCH-PATH-INFO.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO BW-PATH-OVERRIDE
                                              LM-PATH-OVERRIDE
                                              LN-PATH-OVERRIDE
                                              LP-PATH-OVERRIDE
                                              LXG-PATH-OVERRIDE
                                              LXE-PATH-OVERRIDE.

           PERFORM OPEN-BW1-FILE.
           PERFORM OPEN-LM1-FILE.
           PERFORM OPEN-LN1-FILE.
           PERFORM OPEN-LP1-FILE.
           PERFORM OPEN-LXE1-FILE.
           PERFORM OPEN-LXG1-FILE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    PURGE LOAN RECORDS THAT FALL INTO RANGE
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE ZEROS TO PURGED-COUNT(1).
           PERFORM LOAN-PURGE-MODULE.

      *-----------------------------------------------------------------
       END-OF-GROUP.

           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-LM1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-LP1-FILE.
           PERFORM CLOSE-LXE1-FILE.
           PERFORM CLOSE-LXG1-FILE.

           IF RECORDS-FOUND = "Y"
              PERFORM BRANCH-TOTALS
              PERFORM SET-BRANCH-HEADINGS.

           IF FIRST-TIME = "N"
              MOVE "Y" TO FIRST-TIME
              MOVE "Y" TO RECORDS-FOUND.

           GO TO NEXT-GROUP-LOCALITY.

       END-MAIN-MODULE.
           IF NO-RECORDS-FOUND
              GO TO EXIT-MAIN-MODULE.
           PERFORM BRANCH-TOTALS.
           PERFORM GRAND-TOTALS.
           PERFORM PRINT-RANGE-LINES.

       EXIT-MAIN-MODULE.
           GO TO END-PROGRAM.

      ******************************************************************
      *
      * INITIALIZATION
      *
      ******************************************************************
       INITIALIZATION SECTION.
           MOVE BTPURG-QUEUE-POS TO Q-POS.
           MOVE BTPURG-COPIES-POS TO C-POS.
           MOVE BTPURG-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           IF EXT-EOBUF-ERRCD = "E" GO TO END-INITIALIZATION.

           MOVE ZEROES TO BR-NO.
           PERFORM SEND-PROGRESS-LINE-LN.

           MOVE "N" TO RECORDS-FOUND.

           IF BEG-BRANCH = 0
              IF END-BRANCH = 0
                 MOVE BEG-BRANCH TO SAVE-BEG-BRANCH
                 MOVE END-BRANCH TO SAVE-END-BRANCH
                 MOVE EXT-CONAME-USER-LOC  TO BEG-BRANCH
                                              END-BRANCH.

           PERFORM OPEN-PR-FILE.
           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-SP1-FILE.

      *-----------------------------------------------------------------
       SET-HEAD.
           MOVE EXT-CONAME-CONAME  TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO HOLD-SYSDATE.
           MOVE EXT-CONAME-USER-ID  TO H-USERID.

           MOVE ZEROS TO PURGED-COUNT(3) PURGED-COUNT(3) PURGED-COUNT(3)
                         PURGED-LM(1)    PURGED-LP(1)    PURGED-LX(1)
                         PURGED-LM(2)    PURGED-LP(2)    PURGED-LX(2)
                         PURGED-LM(3)    PURGED-LP(3)    PURGED-LX(3).

           GO TO EXIT-INITIALIZE.

       END-INITIALIZATION.
           MOVE "X" TO EXT-EOBUF-ERRCD.

       EXIT-INITIALIZE.
           EXIT.

      ******************************************************************
      *
      * EO EXEC AUDIT
      *
      ******************************************************************
       EO-EXEC-AUDIT SECTION.
           MOVE EXT-JULIAN-BEG  TO BEG-PO-DATE.
           MOVE EXT-EOBUF-DATE2 TO END-PO-DATE.

      ******************************************************************
      *
      * ENTER PARAMETERS
      *
      ******************************************************************
       ENTER-PARAMETERS SECTION.
           IF UP-KEY GO TO ENTER-END-DATE.

      *-----------------------------------------------------------------
       ENTER-ACTION-PRB.
           MOVE "E  A010ACT." TO VDU.
           PERFORM SCREENIO.

           IF F1-KEY GO TO ENTER-PARAMETERS-EXIT.
           IF F7-KEY GO TO ENTER-PARAMETERS-EXIT.
           IF NOT ( DOWN-KEY OR ENTER-KEY ) GO TO ENTER-ACTION-PRB.

           MOVE VDUBUF TO ACTION.
           IF ACTION-VALID GO TO ENTER-BEG-BRANCH.
           GO TO ENTER-ACTION-PRB.

      *-----------------------------------------------------------------
       ENTER-BEG-BRANCH.
           MOVE "E  A000BR1." TO VDU.
           PERFORM SCREENIO.

           IF F1-KEY GO TO ENTER-PARAMETERS-EXIT.
           IF F3-KEY GO TO ENTER-ALL-BRANCHS.
           IF F6-KEY
              MOVE "WI/GRSCAN" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH GRSCAN-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              IF GRSCAN-STATUS NOT = "00"
                 GO TO ENTER-BEG-BRANCH
              ELSE
                 MOVE "S  A000BR1." TO VDU
                 MOVE GRSCAN-KEY TO VDUBUF GR1-KEY
                 PERFORM SCREENIO
                 MOVE "00" TO VDUSTATUS.
           IF F7-KEY GO TO ENTER-PARAMETERS-EXIT.
           IF UP-KEY GO TO ENTER-ACTION-PRB.
           IF NOT ( DOWN-KEY OR ENTER-KEY ) GO TO ENTER-BEG-BRANCH.

           MOVE VDUBUF TO WGR-WORD ENT-GROUP.
           IF WGR-WORD-ZERO
              MOVE 0 TO WGR-SRESULT
              GO TO BRANCH-IS-NUMERIC.

           MOVE ZEROES TO WGR-MIN-LEVEL.
           PERFORM CHECK-MEMBER.

           IF WGR-ERR > ZEROES
              MOVE WGR-MSG(WGR-ERR) TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-BRANCH.

           IF WGR-TYPE = "A"
              MOVE "S  A000BR2." TO VDU
              MOVE SPACES        TO VDUBUF
              PERFORM SCREENIO
              MOVE "Y"           TO GROUP-FLAG
              MOVE 0             TO BEG-BRANCH
              MOVE 9999          TO END-BRANCH
              GO TO CHECK-BRANCH-SECURITY.

       BRANCH-IS-NUMERIC.
           MOVE "N"           TO GROUP-FLAG.
           MOVE WGR-SRESULT   TO BEG-BRANCH.
           MOVE "SN N040BR1." TO VDU.
           MOVE BEG-BRANCH    TO VDUNUM0.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       ENTER-END-BRANCH.

           MOVE "ENNN040BR2." TO VDU.
           PERFORM SCREENIO.

           IF F1-KEY GO TO ENTER-PARAMETERS-EXIT.
           IF F7-KEY GO TO ENTER-PARAMETERS-EXIT.
           IF UP-KEY GO TO ENTER-BEG-BRANCH.
           IF NOT ( DOWN-KEY OR ENTER-KEY ) GO TO ENTER-END-BRANCH.

           MOVE VDUNUM0 TO END-BRANCH.

           IF BEG-BRANCH > END-BRANCH
              MOVE "INVALID BRANCH RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-BRANCH.

           GO TO CHECK-BRANCH-SECURITY.

       ENTER-ALL-BRANCHS.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0             TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999          TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "N" TO GROUP-FLAG.

       CHECK-BRANCH-SECURITY.

           PERFORM BR-RANGE-SECURITY.

           IF BR-SECURITY-OKAY = "N"
              GO TO ENTER-BEG-BRANCH.

      *-----------------------------------------------------------------
       ENTER-BEG-ACCT.
           MOVE "ENNN060AC1." TO VDU.
           PERFORM SCREENIO.

           IF F1-KEY GO TO ENTER-PARAMETERS-EXIT.
           IF F3-KEY GO TO ENTER-ALL-ACCTS.
           IF F7-KEY GO TO ENTER-PARAMETERS-EXIT.
           IF UP-KEY GO TO ENTER-END-BRANCH.
           IF NOT ( DOWN-KEY OR ENTER-KEY ) GO TO ENTER-BEG-ACCT.
           MOVE VDUNUM0 TO BEG-ACCT.

      *-----------------------------------------------------------------
       ENTER-END-ACCT.
           MOVE "ENNN060AC2." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO ENTER-PARAMETERS-EXIT.
           IF F3-KEY GO TO ENTER-ALL-ACCTS.
           IF F7-KEY GO TO ENTER-PARAMETERS-EXIT.
           IF UP-KEY GO TO ENTER-BEG-ACCT.
           IF NOT ( DOWN-KEY OR ENTER-KEY ) GO TO ENTER-END-ACCT.

           MOVE VDUNUM0 TO END-ACCT.
           IF BEG-ACCT > END-ACCT
              MOVE "INVALID ACCOUNT RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-ACCT.

           GO TO ENTER-BEG-DATE.

       ENTER-ALL-ACCTS.
           MOVE "SNNN060AC1." TO VDU.
           MOVE ZEROES        TO BEG-ACCT VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN060AC2." TO VDU.
           MOVE 999999        TO END-ACCT VDUNUM0.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       ENTER-BEG-DATE.
           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO ENTER-PURGE-MEMOS.
           MOVE "ERNM060DT1." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO ENTER-PARAMETERS-EXIT.
           IF F3-KEY GO TO ENTER-ALL-DATES.
           IF F7-KEY GO TO ENTER-PARAMETERS-EXIT.
           IF UP-KEY GO TO ENTER-END-ACCT.
           IF NOT ( DOWN-KEY OR ENTER-KEY ) GO TO ENTER-BEG-DATE.
           MOVE VDU-DATE-YYYYMMDD TO BEG-PO-DATE.

      *-----------------------------------------------------------------
       ENTER-END-DATE.
           MOVE "ERNM060DT2." TO VDU.
           PERFORM SCREENIO.

           IF F1-KEY GO TO ENTER-PARAMETERS-EXIT.
           IF F7-KEY GO TO ENTER-PARAMETERS-EXIT.
           IF UP-KEY GO TO ENTER-BEG-DATE.
           IF NOT ( DOWN-KEY OR ENTER-KEY ) GO TO ENTER-END-DATE.
           MOVE VDU-DATE-YYYYMMDD     TO END-PO-DATE.

           IF END-PO-DATE < BEG-PO-DATE
              MOVE "INVALID DATE RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-DATE.

           GO TO ENTER-PURGE-MEMOS.

       ENTER-ALL-DATES.
           MOVE "S  8080DT1." TO VDU.
           MOVE EXT-JULIAN-BEG TO BEG-PO-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
           MOVE "S  8080DT2." TO VDU.
           MOVE EXT-JULIAN-END TO END-PO-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       ENTER-PURGE-MEMOS.
           MOVE "E  A010PMEMO." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO ENTER-PARAMETERS-EXIT.
           IF F7-KEY GO TO ENTER-PARAMETERS-EXIT.
           IF UP-KEY
              IF EXT-ROUTE-BUF = "40" OR "50"
                 GO TO ENTER-END-ACCT
              ELSE
                 GO TO ENTER-END-DATE.
           IF NOT ( DOWN-KEY OR ENTER-KEY ) GO TO ENTER-PURGE-MEMOS.
           MOVE VDUBUF TO PURGE-MEMOS-YN.
           IF NOT (PURGE-MEMOS-YN = "Y" OR "N")
              GO TO ENTER-PURGE-MEMOS.

      *-----------------------------------------------------------------
       ENTER-PURGE-PAYMENTS.
           MOVE "E  A010PPAY." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO ENTER-PARAMETERS-EXIT.
           IF F7-KEY GO TO ENTER-PARAMETERS-EXIT.
           IF UP-KEY GO TO ENTER-PURGE-MEMOS.
           IF NOT ( DOWN-KEY OR ENTER-KEY ) GO TO ENTER-PURGE-PAYMENTS.
           MOVE VDUBUF TO PURGE-PAYMENTS-YN.
           IF NOT (PURGE-PAYMENTS-YN = "Y" OR "N")
              GO TO ENTER-PURGE-PAYMENTS.


           IF PURGE-MEMOS-YN = "N" AND PURGE-PAYMENTS-YN = "N"
              MOVE "NO RECORDS SELECTED FOR REPORT/PURGE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-PURGE-MEMOS.
      *-----------------------------------------------------------------
       ENTER-ACCEPT-YN.
           ADD 1 EXT-CONAME-RPT-LINES GIVING LN-COUNT.

       ENTER-PARAMETERS-EXIT.
           EXIT.

      ******************************************************************
      *
      * DISPLAY PARAMETERS SECTION
      *
      ******************************************************************
       DISPLAY-PARAMETERS SECTION.
           IF GROUP-FLAG = "Y"
              MOVE "S  A000BR1." TO VDU
              MOVE ENT-GROUP TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A000BR2." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "SNNN040BR1." TO VDU
              MOVE BEG-BRANCH TO VDUNUM0
              PERFORM SCREENIO
              MOVE "SNNN040BR2." TO VDU
              MOVE END-BRANCH TO VDUNUM0
              PERFORM SCREENIO.
      *    -------------------------------------------------------------
           MOVE "S  A010ACT." TO VDU.
           MOVE ACTION        TO VDUBUF.
           PERFORM SCREENIO.
      *    -------------------------------------------------------------
           MOVE "SNNN060AC1." TO VDU.
           MOVE BEG-ACCT      TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN060AC2." TO VDU.
           MOVE END-ACCT      TO VDUNUM0.
           PERFORM SCREENIO.
      *    -------------------------------------------------------------
           MOVE "S  8080DT1." TO VDU.
           MOVE BEG-PO-DATE   TO VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
           MOVE "S  8080DT2." TO VDU.
           MOVE END-PO-DATE   TO VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
      *    -------------------------------------------------------------
           MOVE "S  A010PMEMO." TO VDU.
           MOVE PURGE-MEMOS-YN TO VDUBUF.
           PERFORM SCREENIO.
      *    -------------------------------------------------------------
           MOVE "S  A010PPAY." TO VDU.
           MOVE PURGE-PAYMENTS-YN TO VDUBUF.
           PERFORM SCREENIO.
      *    -------------------------------------------------------------
       DISPLAY-PARAMETERS-EXIT.
           EXIT.

      ******************************************************************
      *
      * LOAN PURGE MODULE
      *
      ******************************************************************
       LOAN-PURGE-MODULE SECTION.
           PERFORM SEND-PROGRESS-LINE-LN.

           MOVE LN-PATH-OWNBR TO QLN1-WBEG-OWNBR
                                 QLN1-WEND-OWNBR.
           MOVE BEG-ACCT      TO QLN1-WBEG-ACCTNO.
           MOVE END-ACCT      TO QLN1-WEND-ACCTNO.

           PERFORM START-LN1-FILE.
       NEXT-LN.
           PERFORM READ-LN1-FILE-NEXT.
           IF IO-FG = 9 OR (LN-OWNBR NOT = LN-PATH-OWNBR)
              GO TO EXIT-LOAN-PURGE-MODULE.

           IF LN-OWNBR > END-BRANCH
      *       UNLOCK LN-FILE
              GO TO EXIT-LOAN-PURGE-MODULE.

           IF LN-ACCTNO > END-ACCT
      *       UNLOCK LN-FILE
              GO TO EXIT-LOAN-PURGE-MODULE.

      *    -------------------------------------------------------------
      *    TEST THAT ACCOUNT HAS BEEN TRANSFERRED
      *    -------------------------------------------------------------
           IF LN-POCD NOT = "BT"
              GO TO NEXT-LN.

      *    -------------------------------------------------------------
      *    TEST TRANSFER DATE RANGE
      *    -------------------------------------------------------------
           IF (LN-POFFDATE < BEG-PO-DATE) OR
                 (LN-POFFDATE > END-PO-DATE)
      *       UNLOCK LN-FILE
              GO TO NEXT-LN.


           IF FIRST-TIME = "Y"
              PERFORM SET-BRANCH-HEADINGS.

           MOVE ZEROS TO PURGED-LM(1) PURGED-LX(1) PURGED-LP(1).

           IF PURGE-MEMOS-YN = "Y"
              PERFORM PURGE-LOAN-MEMOS.

           IF PURGE-PAYMENTS-YN = "Y"
              PERFORM PURGE-LOAN-PAYMENTS.

           IF ACTION-BOTH OR ACTION-REPORT
              PERFORM DETAIL-LINE-PROCESS.

           ADD 1 TO PURGED-COUNT(1) PURGED-COUNT(2).
           ADD PURGED-LM(1) TO PURGED-LM(2).
           ADD PURGED-LP(1) TO PURGED-LP(2).
           ADD PURGED-LX(1) TO PURGED-LX(2).

           MOVE "N" TO FIRST-TIME.
           GO TO NEXT-LN.

       EXIT-LOAN-PURGE-MODULE.
           EXIT.

      ******************************************************************
      *
      * DETAIL LINE PROCESS
      *
      ******************************************************************
       DETAIL-LINE-PROCESS SECTION.
           IF FIRST-TIME = "Y"
              MOVE "N" TO FIRST-TIME
              MOVE EXT-CONAME-RPT-LINES TO LN-COUNT
              MOVE 1 TO LN-FEED
              MOVE "Y" TO RECORDS-FOUND.

           MOVE LN-SSNO(1) TO BW-SSNO.
           PERFORM FIND-BORROWER.

      * LPSTAT USES SPR FIELDS
           MOVE LN-ORGST     TO SP-ORGST.
           MOVE LN-SPRCLASS  TO SP-SPRCLASS.
           MOVE LN-SUBCLASS  TO SP-SUBCLASS.
           MOVE LN-LAWCODE   TO SP-LAWCODE.
           IF LPWSSP-SP1-KEY NOT = SP1-KEY
              MOVE SP1-KEY TO LPWSSP-SP1-KEY
              PERFORM READ-SP1-FILE.

           PERFORM LPSTAT-DISPLAY.
           MOVE LPS-DISPLAY-LNNO TO D-ACCTNO.
           MOVE LN-SSNO(01) TO D-SSNO.
           INSPECT D-SSNO REPLACING ALL "/" BY"-".
           MOVE LN-MAKERCD(01) TO D-MAKERCD.
           MOVE LN-POFFDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-PODATE.
           MOVE PURGED-LM(1) TO D-P-LM.
           MOVE PURGED-LP(1) TO D-P-LP.
           MOVE PURGED-LX(1) TO D-P-LX.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF LN-SSNO(2) NOT = ZEROS
              MOVE LN-SSNO(2) TO BW-SSNO
              PERFORM FIND-BORROWER
              MOVE LN-SSNO(02) TO D-SSNO
              INSPECT D-SSNO REPLACING ALL "/" BY"-"
              MOVE LN-MAKERCD(02) TO D-MAKERCD
              PERFORM WRITE-DETAIL-LINE.

           IF LN-SSNO(3) NOT = ZEROS
              MOVE LN-SSNO(3) TO BW-SSNO
              PERFORM FIND-BORROWER
              MOVE LN-SSNO(03) TO D-SSNO
              INSPECT D-SSNO REPLACING ALL "/" BY"-"
              MOVE LN-MAKERCD(03) TO D-MAKERCD
              PERFORM WRITE-DETAIL-LINE.

           IF LN-SSNO(4) NOT = ZEROS
              MOVE LN-SSNO(4) TO BW-SSNO
              PERFORM FIND-BORROWER
              MOVE LN-SSNO(04) TO D-SSNO
              INSPECT D-SSNO REPLACING ALL "/" BY"-"
              MOVE LN-MAKERCD(04) TO D-MAKERCD
              PERFORM WRITE-DETAIL-LINE.

       DETAIL-LINE-PROCESS-EXIT.
           EXIT.

      ******************************************************************
      *
      * PURGE LOAN MEMOS
      *
      ******************************************************************
       PURGE-LOAN-MEMOS SECTION.
           MOVE "N"           TO FIRST-MEMO.

           MOVE LN-OWNBR        TO QLM1-WBEG-BRNO
                                   QLM1-WEND-BRNO.
           MOVE LN-ACCTNO       TO QLM1-WBEG-ACCTNO
                                   QLM1-WEND-ACCTNO.
           MOVE 0               TO LM-DATE LM-SEQ
                                   QLM1-WBEG-DATE-REV
                                   QLM1-WBEG-SEQ.
           MOVE ALL "9"         TO QLM1-WEND-DATE-REV
                                   QLM1-WEND-SEQ.

           PERFORM START-LM1-FILE.

       NEXT-LM.
           PERFORM READ-LM1-FILE-NEXT.
           IF IO-BAD OR (LM-BRNO NOT = LM-PATH-OWNBR)
              GO TO PURGE-LOAN-MEMOS-EXIT.

           IF LM-ACCTNO NOT = LN-ACCTNO
      *       UNLOCK LM-FILE
              GO TO PURGE-LOAN-MEMOS-EXIT.

      *    -------------------------------------------------------------
      *    DO NOT DELETE MEMO'S WITH TRANSFER INFORMATION
      *    -------------------------------------------------------------
      *- - - - - - - - - - - - - - - - - - - - - - - -{ KEC 2017.0613 }
      *    MOVE LM-MEMO TO WS-TEST-MEMO.
      *    IF WS-TFR-FROM = "TFR FR BR" OR "TFR TO BR"
      *       UNLOCK LM-FILE
      *       GO TO NEXT-LM.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE LM-MEMO                    TO TFRMEMO-MEMO.

           IF ( TFRMEMO-PRIMARY = "TFR FR BR" ) OR
              ( TFRMEMO-PRIMARY = "TFR TO BR" )
      *       UNLOCK LM-FILE
              GO TO NEXT-LM.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ACTION-BOTH OR ACTION-PURGE
              PERFORM DELETE-LM1-FILE.

           ADD 1 TO PURGED-LM(1).

      *    UNLOCK LM-FILE.
           GO TO NEXT-LM.

       PURGE-LOAN-MEMOS-EXIT.
           EXIT.

      ******************************************************************
      *
      *  PURGE LOAN PAYMENTS
      *
      ******************************************************************
       PURGE-LOAN-PAYMENTS SECTION.

           MOVE LP-PATH-OWNBR TO LP-BRNO
                                 QLP1-WBEG-BRNO
                                 QLP1-WEND-BRNO.
           MOVE LN-ACCTNO     TO LP-ACCTNO
                                 QLP1-WBEG-ACCTNO
                                 QLP1-WEND-ACCTNO.
           MOVE 0             TO LP-SEQNO
                                 QLP1-WBEG-SEQNO.
           MOVE ALL "9"       TO QLP1-WEND-SEQNO.

           PERFORM START-LP1-FILE.

       NEXT-PAYMENT.
           PERFORM READ-LP1-FILE-NEXT.
           IF IO-BAD OR (LP-BRNO NOT = LP-PATH-OWNBR)
                      OR (LP-ACCTNO NOT = LN-ACCTNO)
      *       UNLOCK LP-FILE
              GO TO PURGE-LOAN-PAYMENTS-EXIT.

           IF LP-TRCD = "BT" OR LP-REVTRCD = "BT"
      *       UNLOCK LP-FILE
              GO TO NEXT-PAYMENT.

           PERFORM PURGE-LX1-FILE.

           IF ACTION-BOTH OR ACTION-PURGE
              PERFORM DELETE-LP1-FILE.

           ADD 1 TO PURGED-LP(1).

      *    UNLOCK LP-FILE.
           GO TO NEXT-PAYMENT.

       PURGE-LOAN-PAYMENTS-EXIT.
           EXIT.


      ******************************************************************
      *
      *  PURGE LOAN PAYMENT TRAILER
      *
      ******************************************************************
       PURGE-LX1-FILE SECTION.
      *    ----------------------------------------------------------
      *    PURGE LX-E : EARNINGS TRAILER RECORD
      *    ----------------------------------------------------------
           MOVE LP-BRNO   TO LXE-BRNO.
           MOVE LP-ACCTNO TO LXE-ACCTNO.
           MOVE LP-SEQNO  TO LXE-SEQNO.
           PERFORM READ-LXE1-FILE.
           IF IO-GOOD
              ADD 1 TO PURGED-LX(1)
              IF ACTION-BOTH OR ACTION-PURGE
                 PERFORM DELETE-LXE1-FILE.

      *    ----------------------------------------------------------
      *    PURGE LX-G : GENERAL LEDGER DISTRIBUTION RECORD
      *    ----------------------------------------------------------
           MOVE LP-BRNO   TO LXG-BRNO.
           MOVE LP-ACCTNO TO LXG-ACCTNO.
           MOVE LP-SEQNO  TO LXG-SEQNO.
           PERFORM READ-LXG1-FILE.
           IF IO-GOOD
              ADD 1 TO PURGED-LX(1)
              IF ACTION-BOTH OR ACTION-PURGE
                 PERFORM DELETE-LXG1-FILE.

        PURGE-LX1-FILE-EXIT.
           EXIT.

      ******************************************************************
      *
      * FIND BORROWER SECTION
      *
      ******************************************************************
       FIND-BORROWER SECTION.
           PERFORM READ-BW1-FILE.
           IF IO-BAD
               MOVE "** NO BORROWER MASTER **" TO D-NAME
               GO TO EXIT-FIND-BORROWER.

      *    UNLOCK BW-FILE.
           MOVE BW-LNAME TO NAME-WORKER.
           INSPECT NAME-WORKER REPLACING FIRST "  " BY ",?".
           MOVE BW-FNAME TO FIRST-NAME.
           INSPECT NAME-WORKER
                   REPLACING FIRST "                " BY FIRST-NAME.
           INSPECT NAME-WORKER REPLACING ALL "?" BY " ".
           MOVE NAME-WORKER TO D-NAME.
       EXIT-FIND-BORROWER.
           EXIT.

      ******************************************************************
      *
      * WRITE DETAIL LINE
      *
      ******************************************************************
       WRITE-DETAIL-LINE SECTION.
           ADD LN-FEED      TO LN-COUNT.
           IF LN-COUNT > EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.

           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           MOVE 1           TO LN-FEED.
           MOVE SPACES      TO DETAIL-LINE.
       EXIT-WRITE-DETAIL-LINE.
           EXIT.

      ******************************************************************
      *
      * HEAD PAGE
      *
      ******************************************************************
       HEAD-PAGE SECTION.
           ADD  1           TO PAGE-NUMBER.
           MOVE PAGE-NUMBER TO H-PAGE-NO.
           PERFORM GET-TIME.
           MOVE T-HH        TO H-HR.
           MOVE T-MM        TO H-MIN.

           MOVE HEAD1 TO PR-REC.
           MOVE 99    TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE HEAD2 TO PR-REC.
           MOVE 1     TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE HEAD3 TO PR-REC.
           MOVE 1     TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE HEAD4 TO PR-REC.
           MOVE 2     TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE 2 TO LN-FEED.
           MOVE 5 TO LN-COUNT.

       EXIT-HEAD-PAGE.
           EXIT.

      ******************************************************************
      *
      * PRINT RANGE LINES
      *
      ******************************************************************
       PRINT-RANGE-LINES SECTION.
           MOVE 78-TASK       TO RANGE-LINE.
           MOVE ACTION        TO R-ALPHA.
           MOVE 2             TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF GROUP-FLAG = "Y"
              MOVE 78-GROUP   TO RANGE-LINE
              MOVE ENT-GROUP  TO R-ALPHA
           ELSE
              MOVE 78-BEG-BR  TO RANGE-LINE
              MOVE BEG-BRANCH TO R-NUMERIC
              PERFORM WRITE-DETAIL-LINE
              MOVE 78-END-BR  TO RANGE-LINE
              MOVE BEG-BRANCH TO R-NUMERIC.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 78-BEG-ACCT   TO RANGE-LINE.
           MOVE BEG-ACCT      TO R-NUMERIC.
           PERFORM WRITE-DETAIL-LINE.
           MOVE 78-END-ACCT   TO RANGE-LINE.
           MOVE END-ACCT      TO R-NUMERIC.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 78-BEG-DATE   TO RANGE-LINE.
           MOVE BEG-PO-DATE   TO R-DATE.
           PERFORM WRITE-DETAIL-LINE.
           MOVE 78-END-DATE   TO RANGE-LINE.
           MOVE END-PO-DATE   TO R-DATE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 78-SYSDATE    TO RANGE-LINE.
           MOVE HOLD-SYSDATE  TO R-DATE.
           PERFORM WRITE-DETAIL-LINE.

       EXIT-PRINT-RANGE-LINES.
           EXIT.


      ******************************************************************
      *
      * SET BRANCH HEADINGS SECTION
      *
      ******************************************************************
       SET-BRANCH-HEADINGS SECTION.
           MOVE BR-NO TO H-BRNO.
           MOVE BR-NAME TO H-BRDESC.

      ******************************************************************
      *
      * BRANCH TOTALS SECTION
      *
      ******************************************************************
       BRANCH-TOTALS SECTION.
           IF PURGED-COUNT(2) = 0
              GO TO BRANCH-TOTALS-EXIT.

           MOVE "BRANCH TOTALS          :" TO RANGE-LINE.
           MOVE PURGED-COUNT(2)            TO R-ACCT.
           MOVE PURGED-LM(2)               TO D-P-LM.
           MOVE PURGED-LP(2)               TO D-P-LP.
           MOVE PURGED-LX(2)               TO D-P-LX.
           MOVE 4             TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           ADD PURGED-COUNT(2) TO PURGED-COUNT(3).
           ADD PURGED-LM(2)    TO PURGED-LM(3).
           ADD PURGED-LP(2)    TO PURGED-LP(3).
           ADD PURGED-LX(2)    TO PURGED-LX(3).
           MOVE ZEROS TO PURGED-LM(2) PURGED-LP(2)
                         PURGED-LX(2) PURGED-COUNT(2).
       BRANCH-TOTALS-EXIT.
           EXIT.

      ******************************************************************
      *
      * GRAND TOTALS SECTION
      *
      ******************************************************************
       GRAND-TOTALS SECTION.
           MOVE "GRAND-TOTALS           :" TO RANGE-LINE.
           MOVE PURGED-COUNT(3)            TO R-ACCT.
           MOVE PURGED-LM(3)               TO D-P-LM.
           MOVE PURGED-LP(3)               TO D-P-LP.
           MOVE PURGED-LX(3)               TO D-P-LX.
           MOVE 2             TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      ******************************************************************
      *
      * COPY LIBRARY SECTIONS
      *
      ******************************************************************
       COPY "LIBGB/BRSECURE.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/TIMEIO.CPY".

       COPY "LIBLP/LPEARN.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBLP/LPSTAT.CPY".


      ************************************************
       FORM-RESET SECTION.
      ************************************************
           MOVE EXT-CONAME-CONAME TO BTPURG-CONAME.
           MOVE EXT-CONAME-SYSDATE TO BTPURG-SYSDATE.
           DISPLAY BTPURG-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE BTPURG-FORM-FIELDS TO WR-BUF.
           MOVE BTPURG-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       FORM-RESET-EXIT.
           EXIT.


      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE BTPURG-HELP--FILE TO HELP-LINK-FILE.
           MOVE BTPURG-HELP--DIR TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/BTPURG_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
      *
      * MISCELLANEOUS PARAGRAPHS
      *
      ******************************************************************
       MISCELLANEOUS-PARAGRAPHS SECTION.
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/SYSTEM.CPY".
      *-----------------------------------------------------------------
       SEND-PROGRESS-LINE-LN.

           IF BR-NO = ZEROES
              MOVE "...."  TO PROGRESS-LINE-BRANCHX
           ELSE
              MOVE BR-NO   TO PROGRESS-LINE-BRANCH.

           IF ACTION-BOTH
              MOVE PROGRESS-MESSAGE-P-AND-R TO PROGRESS-LINE-TEXT
           ELSE
           IF ACTION-PURGE
              MOVE PROGRESS-MESSAGE-PURGE   TO PROGRESS-LINE-TEXT
           ELSE
           IF ACTION-REPORT
              MOVE PROGRESS-MESSAGE-REPORT  TO PROGRESS-LINE-TEXT
           ELSE
              MOVE SPACES                   TO PROGRESS-LINE-TEXT.

           IF EXT-ROUTE-BUF = "00"
              MOVE PROGRESS-LINE TO LEGEND
              PERFORM SEND-LEGEND.

      *-----------------------------------------------------------------
       SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ******************************************************************
      *
      * IO ROUTINES
      *
      ******************************************************************
       IO-ROUTINE SECTION.
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLXGGS_SQL.CPY".
       COPY "LIBLP/LPLXEGS_SQL.CPY".
       COPY "LIBLP/LPLPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLMGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBLP/LPBW1IN.CPY".
       COPY "LIBLP/LPLM1IN.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBLP/LPLP1IN.CPY".
       COPY "LIBLP/LPLXE1IN.CPY".
       COPY "LIBLP/LPLXG1IN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      ******************************************************************
      *
      * END PROGRAM
      *
      ******************************************************************
       END-PROGRAM SECTION.
       END-RUN.
           PERFORM CLOSE-FILES.
           IF SKIP-RECORDS-FOUND
              GO TO EXIT-PROG.

           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.

           IF NO-RECORDS-FOUND
              MOVE "NO RECORDS FOUND FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS.

       EXIT-PROG.
           IF SAVE-BEG-BRANCH NOT = 9999
              MOVE SAVE-BEG-BRANCH TO BEG-BRANCH
              MOVE SAVE-END-BRANCH TO END-BRANCH.

           IF EXT-ROUTE-BUF = "00"
              MOVE EXIT-PATH  TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY BTPURG-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
