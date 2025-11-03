       IDENTIFICATION DIVISION.
       PROGRAM-ID.    DTPURG.
       DATE-WRITTEN.  101085
      ******************************************************************
      *
      * IDENTIFICATION:  LP/DTPURG.C
      *
      * DESCRIPTION   :  DEALER TRANSACTION PURGE
      *                  DELETES ANY RECORD THAT HAS 0 PROCEEDS
      *                   OR PROCEEDS = PAYMENTS
      *
      *-----------------------------------------------------------------
      * REV:
      * MJD 1995.0915 ADDED DS-DATE LOGIC.
      * JTG 1998.0304 CORRECTED LOGIC
      * KEC 2000.0421 {PR#00000} FIXED DUE TO THE FIXING OF
      *                "LIBLP/LPFXDT" AND "LIBLP/LPFXDS" TO NOT HAVE
      *                HARDCODED FILE PATHS.
      *                ADDED 'EXITPROG' LOGIC.
      *                ADDED 'GROUP' LOGIC.
      *
      * GLB 011129 REWORK LOGIC FOR ENHANCED DT-RECORD/DEALER MODS
      * GLB 040721 BUGS; TESTING LOW DEALER ENTRY, PROCESS-BRANCH-NEXT
      *            -DS (TOO MANY "OR'S")
      * BAH 120531 CHANGED PDATE TO 9(6) AND USED CORRECT SCREENIO DATE
      *             EDITS.
      * BLM 160512 FIX FOR NEW DS-DATE FORMAT, PD0003
      * KEC 2017.0105 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          CHANGED DS-DATE TO DS-DATE-CCYYMM FOR CONSISTENCY
      *          WITH THE DATE FIELDS IN BY & UD FILE.
      *
      * 20171026 BAH ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 181015 GLOBAL DSFILE
      * BAH 20190924 REMOVED ACCESS-CALL DSFILE
      * BAH 20200827 REMOVED RESET ON BR1 
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".

       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".

      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LP/DTPURG.CBL S35 02/20/24-13:49:20 >".

       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01DS.CPY".
       COPY "LIBLP/LP01DS_SQL.CPY".
       COPY "LIBLP/LPWSDS.CPY".
       COPY "LIBLP/LP01DT.CPY".
       COPY "LIBLP/LP01DT_SQL.CPY".
       COPY "LIBLP/LPWSDT.CPY".

      ******************************************************************
      *
      *    M I S C E L L A N E O U S - W O R K E R S
      *
      ******************************************************************
       01  BALANCE-WORKERS.
           03  BAL-DLPARVHELD          PIC S9(07)V99.
           03  BAL-DLRVCHGS            PIC S9(07)V99 OCCURS 3 TIMES.
           03  BAL-DLRVPRIN            PIC S9(07)V99 OCCURS 3 TIMES.
           03  TOT-DLPARVHELD          PIC S9(07)V99.
           03  TOT-DLRVCHGS            PIC S9(07)V99 OCCURS 3 TIMES.
           03  TOT-DLRVPRIN            PIC S9(07)V99 OCCURS 3 TIMES.
           03  TOT-UNPAID              PIC S9(07)V99.

      *-----------------------------------------------------------------
       01  HOLD-DATE-CCYYMM            PIC 9(06).
       01  HOLD-RSVPRIN             PIC S9(8)V99.
       01  HOLD-RSVCHGS             PIC S9(8)V99.
       01  HOLD-RSVHELD             PIC S9(8)V99.
       01  HOLD-DUE                 PIC S9(8)V99.
       01  HOLD-PROCEEDS            PIC S9(8)V99.


       01  CHECK-TRCD           PIC XX.
           88  DEALER-ORIGINAL  VALUE "NW" "PU" "AV".
      *-----------------------------------------------------------------
       01  LEGEND                      PIC X(80)     VALUE SPACES.
       01  LEGEND-LIST                 PIC X(05)     VALUE "MESS.".

      *-----------------------------------------------------------------
       01  PRINT-LINE-WORKERS.
           03  LN-COUNT                PIC 9(03)     VALUE 99.
           03  LN-FEED                 PIC 9(02)     VALUE ZEROES.
           03  LN-WK                   PIC 9(03)     VALUE ZEROES.
           03  PAGE-NUMBER             PIC 9(04)     VALUE ZEROES.

      *-----------------------------------------------------------------
       01  WORKER1                     PIC S9(7)V99.

      ******************************************************************
      *
      *    S C R E E N - R A N G E - F I E L D S
      *
      ******************************************************************
       COPY "LIBGB/EOBUF_EXT.CPY".
      *-----------------------------------------------------------------
           03  PDATE                   PIC 9(8).

           03  BEG-BRANCH              PIC 9(04).
           03  END-BRANCH              PIC 9(04).

           03  GROUP-OPTIONS.
               05  GROUP-FLAG          PIC X(01).
               05  ENT-GROUP           PIC X(04).
           03  BEG-DLNO                PIC 9(4)V99.
           03  END-DLNO                PIC 9(4)V99.
           03  END-DLNUM REDEFINES END-DLNO PIC 9(6).
           03  BEG-CLASS               PIC 9(3).
           03  END-CLASS               PIC 9(3).
           03  RSV-FG                  PIC X.

      *-----------------------------------------------------------------
       01  VERSION-CONTROL             PIC X(01)     VALUE "C".

       01  SAVE-BRANCH-RANGE.
           03  SAVE-BEG-BRANCH         PIC 9(04).
           03  SAVE-END-BRANCH         PIC 9(04).

       01  SAVE-GROUP-OPTIONS.
           03  SAVE-GROUP-FLAG         PIC X(01).
           03  SAVE-ENT-GROUP          PIC X(04).

      ******************************************************************
      *
      *    H E A D E R - L I N E - W O R K E R S
      *
      ******************************************************************
       01  HEAD1.
           03  H-DATE                  PIC X(08)     VALUE SPACES.
           03  FILLER                  PIC X(02)     VALUE SPACES.
           03  H-HR                    PIC 9(02)     VALUE ZEROES.
           03  FILLER                  PIC X(01)     VALUE ":".
           03  H-MIN                   PIC 9(02)     VALUE ZEROES.
           03  FILLER                  PIC X(61)     VALUE SPACES.
           03  FILLER                  PIC X(17)     VALUE SPACES.
           03  FILLER                  PIC X(05)     VALUE "PAGE ".
           03  H-PAGE-NO               PIC ZZZ9.

      *-----------------------------------------------------------------
       01  HEAD2.
           03  FILLER                  PIC X(10)     VALUE "USER ID:".
           03  H-USERID                PIC X(10).
           03  FILLER                  PIC X(16)     VALUE SPACES.
           03  FILLER                  PIC X(42)     VALUE
                           "- - DEALER TRANSACTION FILE EXCEPTIONS - -".
           03  FILLER                  PIC X(44)     VALUE SPACES.

      *-----------------------------------------------------------------
       01  HEAD3.
           03  FILLER                  PIC X(42)     VALUE
                       "BRANCH   DEALER  CLASS                    ".
           03  FILLER                  PIC X(46)     VALUE
                       "RESERVE   SP RESERVE  PAR RESERVE       UNPAID".

      ******************************************************************
      *
      *    D E T A I L - L I N E - W O R K E R S
      *
      ******************************************************************
       01  DETAIL-LINE.
           03  FILLER                  PIC X(02).
           03  D-BRNO                  PIC 9(04).
           03  FILLER                  PIC X(04).
           03  D-NUM                   PIC Z(04).99.
           03  FILLER                  PIC X(03).
           03  D-CLASS                 PIC 9(02).
           03  FILLER                  PIC X(03).
           03  D-DESC                  PIC X(14).
           03  D-DLRVPRIN              PIC Z(08).99- OCCURS 3 TIMES.
           03  FILLER                  PIC X(37).
           03  D-DLPARVHELD            PIC Z(07).99-.
           03  FILLER                  PIC X(02).
           03  D-UNPAID                PIC Z(07).99-.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - W O R K E R S
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
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/DTPURG_DEF.CPY".
       COPY "LIBLP/DTPURG_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ****************************************
      *                                      *
      *     P R O G R A M   C O N T R O L    *
      *                                      *
      ****************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBLP/DTPURG_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
                  PR-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-DT1-FILE.
           PERFORM CLOSE-DS1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ******************************************************************
       MAIN-PROGRAM SECTION.
      ******************************************************************

           PERFORM SQL-CONNECT.

           MOVE "DTPURG" TO VDU-FORM-NAME FORM-PROG.
      D    STOP MESS.

           PERFORM INITIALIZATION.
           IF ( EXT-EOBUF-ERRCD NOT = SPACES )
              GO TO MAIN-PROGRAM-EXIT.

           PERFORM PRIMARY-DRIVER-MODULE.

       MAIN-PROGRAM-EXIT.
           GO TO END-PROGRAM.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************

           MOVE DTPURG-QUEUE-POS TO Q-POS.
           MOVE DTPURG-COPIES-POS TO C-POS.
           MOVE DTPURG-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           IF EXT-ROUTE-BUF = "00"
              MOVE "DEALER TRANSACTION PURGE IN PROGRESS.." TO LEGEND
              PERFORM SEND-LEGEND.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( BEG-BRANCH = ZEROES ) AND
              ( END-BRANCH = ZEROES )
              MOVE BEG-BRANCH TO SAVE-BEG-BRANCH
              MOVE END-BRANCH TO SAVE-END-BRANCH
              MOVE EXT-CONAME-USER-LOC  TO BEG-BRANCH
                                           END-BRANCH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-EOBUF-GROUP-FG = "Y" )
              MOVE BEG-BRANCH         TO SAVE-BEG-BRANCH
              MOVE END-BRANCH         TO SAVE-END-BRANCH
              MOVE GROUP-FLAG         TO SAVE-GROUP-FLAG
              MOVE ENT-GROUP          TO SAVE-ENT-GROUP
              MOVE EXT-EOBUF-GROUP        TO ENT-GROUP
              MOVE ZEROES             TO BEG-BRANCH
              MOVE 9999               TO END-BRANCH
              MOVE "Y"                TO GROUP-FLAG
              MOVE EXT-EOBUF-GROUP-PRPATH TO PRU-FILE.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-PR-FILE.

       INITIALIZATION-EXIT.
           EXIT.

      ******************************************************************
       EO-EXEC-AUDIT SECTION.
      ******************************************************************

           MOVE EXT-EOBUF-DATE2 TO PDATE.

       EO-EXEC-AUDIT-EXIT.
           EXIT.

      ******************************************************************
       ENTER-PARAMETERS SECTION.
      ******************************************************************
       ENTER-PARM.
           IF ( UP-KEY )
              GO TO ENTER-PDATE.

       BR-01.

           MOVE "E  A000BR1." TO VDU.
           PERFORM SCREENIO.
           IF (        F1-KEY ) GO TO EXIT-PARM.
           IF (        F3-KEY ) GO TO ENTER-ALL-BRANCH.
           IF (        F6-KEY ) PERFORM SCAN-GR-FILE.
           IF (        F7-KEY ) GO TO EXIT-PARM.
           IF ( NOT ENTER-KEY ) GO TO BR-01.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO WGR-WORD ENT-GROUP.

           IF ( WGR-WORD-ZERO )
              GO TO BRANCH-IS-NUMERIC.

           MOVE ZEROES TO WGR-MIN-LEVEL.
           PERFORM CHECK-MEMBER.

           IF ( WGR-ERR > ZEROES )
              MOVE WGR-MSG(WGR-ERR) TO MESS
              PERFORM SEND-MESS
              GO TO BR-01.

           IF ( WGR-TYPE = "A" )
              MOVE "S  A000BR2." TO VDU
              MOVE SPACES        TO VDUBUF
              PERFORM SCREENIO
              MOVE "Y"           TO GROUP-FLAG
              MOVE ZEROES        TO BEG-BRANCH
              MOVE 9999          TO END-BRANCH
              GO TO CHECK-BRANCH-SECURITY.

      *----------------------------------------------------------------
       BRANCH-IS-NUMERIC.
           MOVE "N"           TO GROUP-FLAG.
           MOVE WGR-SRESULT   TO BEG-BRANCH.
           MOVE "SN N040BR1." TO VDU.
           MOVE BEG-BRANCH    TO VDUNUM0.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
       BR-02.

           MOVE "ENNN040BR2." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO BR-01.
           IF (        F1-KEY ) GO TO EXIT-PARM.
           IF (        F7-KEY ) GO TO EXIT-PARM.
           IF ( NOT ENTER-KEY ) GO TO BR-02.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO END-BRANCH.

           IF ( BEG-BRANCH > END-BRANCH )
              MOVE "INVALID BRANCH RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO BR-01.

           IF ( END-BRANCH = ZEROES )
              GO TO BR-02.

           GO TO CHECK-BRANCH-SECURITY.

      *-----------------------------------------------------------------
       ENTER-ALL-BRANCH.

           MOVE "N"           TO GROUP-FLAG.
           MOVE "SNNN040BR1." TO VDU.
           MOVE ZEROES        TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999          TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       CHECK-BRANCH-SECURITY.

           PERFORM BR-RANGE-SECURITY.

           IF ( BR-SECURITY-OKAY = "N" )
              GO TO BR-01.

      *=================================================================
       DL-01.
           MOVE "ENNN042DL1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-DL.
           IF VDUSTATUS = "1U" GO TO BR-02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF STAT NOT = "00" AND NOT = "1D" GO TO DL-01.
           MOVE VDUNUM2 TO BEG-DLNO.

      *-----------------------------------------------------------------
       DL-02.
           MOVE "ENNN042DL2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO DL-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF STAT NOT = "00" AND NOT = "1D" GO TO DL-02.
           MOVE VDUNUM2 TO END-DLNO.

           IF BEG-DLNO > END-DLNO
              MOVE "INVALID DEALER RANGE!" TO MESS
              PERFORM SEND-MESS
              GO TO DL-01.

           GO TO DC-01.

      *-----------------------------------------------------------------
       ALL-DL.
           MOVE "SNNN042DL1." TO VDU.
           MOVE 0 TO BEG-DLNO VDUNUM2.
           PERFORM SCREENIO.
           MOVE "SNNN042DL2." TO VDU.
           MOVE 9999.99 TO END-DLNO VDUNUM2.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       DC-01.
           MOVE "ENNN020DC1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-DC.
           IF VDUSTATUS = "1U" GO TO DL-02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF STAT NOT = "00" AND NOT = "1D" GO TO DC-01.
           MOVE VDUNUM0 TO BEG-CLASS.

      *-----------------------------------------------------------------
       DC-02.
           MOVE "ENNN020DC2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO DC-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF STAT NOT = "00" AND NOT = "1D" GO TO DC-02.
           MOVE VDUNUM0 TO END-CLASS.

           IF BEG-CLASS > END-CLASS
              MOVE "INVALID CLASS RANGE!" TO MESS
              PERFORM SEND-MESS
              GO TO DC-01.

           GO TO ENTER-PDATE.

      *-----------------------------------------------------------------
       ALL-DC.
           MOVE "SNNN020DC1." TO VDU.
           MOVE 0 TO BEG-CLASS VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN020DC2." TO VDU.
           MOVE 99 TO END-CLASS VDUNUM0.
           PERFORM SCREENIO.

       ENTER-PDATE.

           IF ( EXT-ROUTE-BUF NOT = "00" )
              GO TO ENTER-RSVFG.

           MOVE "ERNM060DT." TO VDU.
           PERFORM SCREENIO.

           IF (        F1-KEY ) GO TO EXIT-PARM.
           IF (        F7-KEY ) GO TO EXIT-PARM.
           IF (        UP-KEY ) GO TO BR-02.
           IF ( NOT ENTER-KEY ) GO TO ENTER-PDATE.

           MOVE VDU-DATE-YYYYMMDD TO PDATE.

           IF PDATE = 0
              GO TO ENTER-PDATE.

      *=================================================================
       ENTER-RSVFG.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "E  A000RSVFG." TO VDU.
           PERFORM SCREENIO.

           IF (        F1-KEY ) GO TO EXIT-PARM.
           IF (        F7-KEY ) GO TO EXIT-PARM.
           IF (        UP-KEY ) GO TO ENTER-PDATE.
           IF ( NOT ENTER-KEY ) GO TO ENTER-RSVFG.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO RSV-FG.

           IF RSV-FG  = "Y" GO TO EXIT-PARM.
           IF RSV-FG  = "N" GO TO EXIT-PARM.
           GO TO ENTER-RSVFG.

       EXIT-PARM.
           EXIT.

      ******************************************************************
       DISPLAY-PARAMETERS SECTION.
      ******************************************************************

           IF ( GROUP-FLAG = "Y" )
              MOVE "S  A000BR1." TO VDU
              MOVE ENT-GROUP     TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A000BR2." TO VDU
              MOVE SPACES        TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "SNNN040BR1." TO VDU
              MOVE BEG-BRANCH    TO VDUNUM0
              PERFORM SCREENIO
              MOVE "SNNN040BR2." TO VDU
              MOVE END-BRANCH    TO VDUNUM0
              PERFORM SCREENIO.

           MOVE "SN N042DL1." TO VDU.
           MOVE BEG-DLNO      TO VDUNUM2.
           PERFORM SCREENIO.
           MOVE "SN N042DL2." TO VDU.
           MOVE END-DLNO      TO VDUNUM2.
           PERFORM SCREENIO.
           MOVE "SN N020DC1." TO VDU.
           MOVE BEG-CLASS     TO VDUNUM2.
           PERFORM SCREENIO.
           MOVE "SN N020DC2." TO VDU.
           MOVE END-CLASS     TO VDUNUM2.
           PERFORM SCREENIO.

       DISPLAY-PARAMETERS-EXIT.
           EXIT.

      ******************************************************************
       SCAN-GR-FILE SECTION.
      ******************************************************************

           MOVE "WI/GRSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH GRSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF GRSCAN-STATUS NOT = "00"
              MOVE "1<" TO VDUSTATUS
              GO TO SCAN-GR-FILE-EXIT.

           MOVE "S  A000BR1." TO VDU.
           MOVE GRSCAN-KEY    TO VDUBUF GR1-KEY.
           PERFORM SCREENIO.

           MOVE "00"       TO VDUSTATUS.
           MOVE GRSCAN-KEY TO VDUBUF.

       SCAN-GR-FILE-EXIT.
           EXIT.

      ******************************************************************
       PRIMARY-DRIVER-MODULE SECTION.
      ******************************************************************

           IF ( GROUP-FLAG = "N" )
              MOVE BEG-BRANCH TO BR-NO
              PERFORM START-BR-FILE
              GO TO NEXT-BRANCH-RECORD.

           MOVE ENT-GROUP TO GR-ID.
           PERFORM SEARCH-GR-INIT.

       NEXT-GROUP-RECORD.

           IF ( GROUP-FLAG = "N" )
              GO TO NEXT-BRANCH-RECORD.

           PERFORM SEARCH-GR-NEXT.

           IF ( WGR-SRESULT = ZEROES )
              GO TO PRIMARY-DRIVER-MODULE-EXIT.

           MOVE WGR-SRESULT TO BR-NO.
           PERFORM READ-BR-FILE.

           IF ( IO-BAD     )
              GO TO IO-ERROR.

           GO TO MOVE-BRANCH-PATH-INFO.

      *-----------------------------------------------------------------
       NEXT-BRANCH-RECORD.

           PERFORM READ-BR-FILE-NEXT.
           IF ( IO-BAD             ) OR
              ( BR-NO > END-BRANCH )
              GO TO PRIMARY-DRIVER-MODULE-EXIT.

      *-----------------------------------------------------------------
       MOVE-BRANCH-PATH-INFO.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO DS-PATH-OVERRIDE.

           PERFORM PROCESS-BRANCH.

           GO TO NEXT-GROUP-RECORD.

       PRIMARY-DRIVER-MODULE-EXIT.
           EXIT.

      ******************************************************************
       PROCESS-BRANCH SECTION.
      ******************************************************************

           PERFORM OPEN-DS1-FILE.
           PERFORM OPEN-DT1-FILE.

           MOVE PDATE       TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-YYYYMM.
           MOVE DATE-YYYYMM TO HOLD-DATE-CCYYMM DS-DATE-CCYYMM
                               QDS1-WBEG-DATE-CCYYMM
                               QDS1-WEND-DATE-CCYYMM.
           MOVE BR-NO       TO DS-BRANCH
                               QDS1-WBEG-BRANCH
                               QDS1-WEND-BRANCH.
           MOVE BEG-DLNO    TO DS-DLNO.
           MOVE DS-NUM      TO QDS1-WBEG-NUM.
           MOVE END-DLNUM   TO QDS1-WEND-NUM.
           
           MOVE BEG-CLASS   TO DS-CLASS
                               QDS1-WBEG-CLASS.
           MOVE END-CLASS   TO QDS1-WEND-CLASS.

           PERFORM START-DS1-FILE.

       PROCESS-BRANCH-NEXT-DS.

           PERFORM READ-DS1-FILE-NEXT.
           IF ( IO-BAD                                 ) OR
              ( DS-DATE-CCYYMM     > HOLD-DATE-CCYYMM  ) OR
              ( DS-BRANCH      NOT = BR-NO             )
              GO TO PROCESS-BRANCH-END.

           IF ( DS-BRANCH < BEG-BRANCH ) OR
              ( DS-BRANCH > END-BRANCH ) OR
              ( DS-DLNO   < BEG-DLNO   ) OR
              ( DS-DLNO   > END-DLNO   ) OR
              ( DS-CLASS  < BEG-CLASS  ) OR
              ( DS-CLASS  > END-CLASS  )
                 GO TO PROCESS-BRANCH-NEXT-DS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE ZEROES TO BAL-DLRVCHGS(1)     TOT-DLRVCHGS(1)
                          BAL-DLRVCHGS(2)     TOT-DLRVCHGS(2)
                          BAL-DLRVCHGS(3)     TOT-DLRVCHGS(3)
                          BAL-DLRVPRIN(1)     TOT-DLRVPRIN(1)
                          BAL-DLRVPRIN(2)     TOT-DLRVPRIN(2)
                          BAL-DLRVPRIN(3)     TOT-DLRVPRIN(3)
                          BAL-DLPARVHELD      TOT-DLPARVHELD
                                              TOT-UNPAID.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           MOVE DS-BRANCH TO DT-BRNO
                             QDT1-WBEG-BRNO
                             QDT1-WEND-BRNO.
           MOVE DS-NUM     TO DT-NUM
                              QDT1-WBEG-NUM
                              QDT1-WEND-NUM.
           MOVE DS-CLASS   TO DT-CLASS
                              QDT1-WBEG-CLASS
                              QDT1-WEND-CLASS.

           MOVE 19000101   TO DT-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QDT1-WBEG-DATE.
           MOVE EXT-JULIAN-END       TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QDT1-WEND-DATE.

           MOVE 0          TO DT-ACCTNO DT-SEQ
                              QDT1-WBEG-ACCTNO
                              QDT1-WBEG-SEQ.

           MOVE ALL "9"    TO QDT1-WEND-ACCTNO
                              QDT1-WEND-SEQ.
 
           PERFORM START-DT1-FILE.

       PROCESS-BRANCH-NEXT-DT.

           PERFORM READ-DT1-FILE-NEXT.
           IF ( IO-BAD                   ) OR
              ( DT-BRNO  NOT = DS-BRANCH ) OR
              ( DT-CLASS NOT = DS-CLASS  ) OR
              ( DT-NUM   NOT = DS-NUM    )
              GO TO PROCESS-BRANCH-END-DT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  TEST PROCEEDS, IF NOT PAID SKIP PURGE
           COMPUTE HOLD-DUE =  DT-PROCEEDS.
           IF DT-REVERSE = "Y"
              MOVE 0          TO HOLD-DUE.
           COMPUTE HOLD-DUE = HOLD-DUE - DT-PAYMENTS.
           IF ( HOLD-DUE NOT = ZEROES )
      *       UNLOCK DT-FILE
              GO TO PROCESS-BRANCH-NEXT-DT.

           COMPUTE HOLD-RSVPRIN  =
                  (DT-DLRVPRIN(1) + DT-DLRVPRIN(2) + DT-DLRVPRIN(3) ).
           COMPUTE HOLD-RSVCHGS =
                  (DT-DLRVCHGS(1) + DT-DLRVCHGS(2) + DT-DLRVCHGS(3) ).
           COMPUTE HOLD-RSVHELD = DT-DLPARVHELD.
           IF DT-REVERSE = "Y"
              MOVE 0          TO HOLD-PROCEEDS HOLD-RSVPRIN
                                 HOLD-RSVCHGS  HOLD-RSVHELD.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           COMPUTE HOLD-RSVPRIN  =  HOLD-RSVPRIN - DT-DLRVPRIN-PAY.
           COMPUTE HOLD-RSVCHGS =   HOLD-RSVCHGS - DT-DLRVCHGS-PAY.
           COMPUTE HOLD-RSVHELD =   HOLD-RSVHELD - DT-DLPARVHELD-PAY.

      * IF ORIGINAL PROCEEDS RECORD, TEST FLAG TO DETERMINE IF
      * THE RECORD SHOULD BE PURGED/SUMMARIZED IF RESERVES
      * ARE CARRYING A BALANCE
           MOVE DT-TRCD TO CHECK-TRCD.
           IF NOT (DEALER-ORIGINAL)
              GO TO ADD-UP-PROCEEDS.
           IF RSV-FG = "N"
              GO TO ADD-UP-PROCEEDS.
           IF HOLD-RSVPRIN NOT = 0 OR
              HOLD-RSVCHGS NOT = 0 OR
              HOLD-RSVHELD NOT = 0
                 GO TO PROCESS-BRANCH-NEXT-DT.

      * DONT NEED THIS ?  GLB
       ADD-UP-PROCEEDS.
           ADD HOLD-DUE TO TOT-UNPAID.

       ADD-UP-PRIN.
           IF DT-DLRVPRIN(1) NOT = 0
              ADD HOLD-RSVPRIN TO TOT-DLRVPRIN(1)
              ADD HOLD-RSVPRIN TO BAL-DLRVPRIN(1)
              GO TO ADD-UP-RSV.
           IF DT-DLRVPRIN(2) NOT = 0
              ADD HOLD-RSVPRIN TO TOT-DLRVPRIN (2)
              ADD HOLD-RSVPRIN TO BAL-DLRVPRIN (2)
              GO TO ADD-UP-RSV.
           ADD HOLD-RSVPRIN TO TOT-DLRVPRIN (3).
           ADD HOLD-RSVPRIN TO BAL-DLRVPRIN (3).
       ADD-UP-RSV.
           IF DT-DLRVCHGS(1) NOT = 0
              ADD HOLD-RSVCHGS TO TOT-DLRVCHGS(1)
              ADD HOLD-RSVCHGS TO BAL-DLRVCHGS(1)
              GO TO ADD-UP-HELD.
           IF DT-DLRVCHGS(2) NOT = 0
              ADD HOLD-RSVCHGS TO TOT-DLRVCHGS (2)
              ADD HOLD-RSVCHGS TO BAL-DLRVCHGS (2)
              GO TO ADD-UP-HELD.
           ADD HOLD-RSVCHGS TO TOT-DLRVCHGS (3).
           ADD HOLD-RSVCHGS TO BAL-DLRVCHGS (3).
           ADD HOLD-RSVCHGS TO TOT-DLRVCHGS (3).
       ADD-UP-HELD.
           ADD HOLD-RSVHELD TO TOT-DLPARVHELD.
           ADD HOLD-RSVHELD TO BAL-DLPARVHELD.

      *- - - - - - - - - - - - - - - - - - - - - -{ PURGE 'DT' RECORD  }
           PERFORM DELETE-DT1-FILE.

           GO TO PROCESS-BRANCH-NEXT-DT.

      *=================================================================
       PROCESS-BRANCH-END-DT.

           PERFORM VERIFY-DEALER-STATISTICS.
           IF  BAL-DLRVPRIN(1) = 0
              AND BAL-DLRVPRIN(2) = 0
              AND BAL-DLRVPRIN(3) = 0
              AND BAL-DLRVCHGS(1) = 0
              AND BAL-DLRVCHGS(2) = 0
              AND BAL-DLRVCHGS(3) = 0
              AND BAL-DLPARVHELD  = 0
                 GO TO PROCESS-BRANCH-NEXT-DS.

           PERFORM CREATE-BAL-FWD.

           GO TO PROCESS-BRANCH-NEXT-DS.

      *=================================================================
       PROCESS-BRANCH-END.

           PERFORM CLOSE-DS1-FILE.
           PERFORM CLOSE-DT1-FILE.

       PROCESS-BRANCH-EXIT.
           EXIT.

      ******************************************************************
       CREATE-BAL-FWD SECTION.
      ******************************************************************
       CREATE-BAL-FWD-BEGIN.

           MOVE DS-BRANCH TO DT-BRNO.
           MOVE DS-NUM    TO DT-NUM.
           MOVE DS-CLASS  TO DT-CLASS.
           MOVE ZEROES    TO DT-ACCTNO DT-DATE DT-SEQ.
           PERFORM READ-DT1-FILE.
           IF ( IO-BAD )
              GO TO CREATE-BAL-FWD-NEW.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE BAL-DLRVPRIN(1) TO DT-DLRVPRIN(1).
           MOVE BAL-DLRVPRIN(2) TO DT-DLRVPRIN(2).
           MOVE BAL-DLRVPRIN(3) TO DT-DLRVPRIN(3).
           MOVE BAL-DLRVCHGS(1) TO DT-DLRVCHGS(1).
           MOVE BAL-DLRVCHGS(2) TO DT-DLRVCHGS(2).
           MOVE BAL-DLRVCHGS(3) TO DT-DLRVCHGS(3).
           MOVE BAL-DLPARVHELD  TO DT-DLPARVHELD.
           MOVE PDATE           TO DT-CKDATE.
           PERFORM REWRITE-DT1-FILE.

           GO TO CREATE-BAL-FWD-EXIT.

       CREATE-BAL-FWD-NEW.

           PERFORM CLEAR-DT-FILE.
           MOVE DS-BRANCH TO DT-BRNO.
           MOVE DS-NUM    TO DT-NUM.
           MOVE DS-CLASS  TO DT-CLASS.
           MOVE "BF"      TO DT-TRCD.
           PERFORM WRITE-DT1-FILE.
           IF ( IO-BAD )
              GO TO IO-ERROR.

           GO TO CREATE-BAL-FWD-BEGIN.

       CREATE-BAL-FWD-EXIT.
           EXIT.

      ******************************************************************
      *    COMPARE DEALER DETAIL TO STATISTICS RECORD AND PRINT
      *    EXCEPTION REPORT IF DISCREPENCIES
      ******************************************************************
       VERIFY-DEALER-STATISTICS SECTION.

           IF ( TOT-DLRVPRIN(1) NOT = DS-DLRVPRIN(1) )
              PERFORM PREPARE-EXCEPTIONS
              GO TO VERIFY-DEALER-STATS-EXIT.

           IF ( TOT-DLRVPRIN(2) NOT = DS-DLRVPRIN(2) )
              PERFORM PREPARE-EXCEPTIONS
              GO TO VERIFY-DEALER-STATS-EXIT.

           IF ( TOT-DLPARVHELD NOT = DS-DLPARVHELD )
              PERFORM PREPARE-EXCEPTIONS
              GO TO VERIFY-DEALER-STATS-EXIT.

           IF ( TOT-UNPAID NOT = DS-UNPAIDCK )
              PERFORM PREPARE-EXCEPTIONS
              GO TO VERIFY-DEALER-STATS-EXIT.

       VERIFY-DEALER-STATS-EXIT.
           EXIT.

      ******************************************************************
      *    PRINT DISCREPENCIES DETAIL VS. STATISTICS
      ******************************************************************
       PREPARE-EXCEPTIONS SECTION.

           MOVE DS-DLNO         TO D-NUM.
           MOVE DS-BRANCH       TO D-BRNO.
           MOVE DS-CLASS        TO D-CLASS.
           MOVE "DETAIL"        TO D-DESC.
           MOVE TOT-DLRVPRIN(1) TO D-DLRVPRIN(1).
           MOVE TOT-DLRVPRIN(2) TO D-DLRVPRIN(2).
           MOVE TOT-DLPARVHELD  TO D-DLPARVHELD.
           MOVE TOT-UNPAID      TO D-UNPAID.

           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "STATISTICS"   TO D-DESC.
           MOVE DS-DLRVPRIN(1) TO D-DLRVPRIN(1).
           MOVE DS-DLRVPRIN(2) TO D-DLRVPRIN(2).
           MOVE DS-DLPARVHELD  TO D-DLPARVHELD.
           MOVE DS-UNPAIDCK    TO D-UNPAID.

           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           SUBTRACT TOT-DLRVPRIN(1) FROM DS-DLRVPRIN(1) GIVING WORKER1.
           ADD WORKER1 TO BAL-DLRVPRIN(1).
           SUBTRACT TOT-DLRVPRIN(2) FROM DS-DLRVPRIN(2) GIVING WORKER1.
           ADD WORKER1 TO BAL-DLRVPRIN(2).
           SUBTRACT TOT-DLPARVHELD FROM DS-DLPARVHELD GIVING WORKER1.
           ADD WORKER1 TO BAL-DLPARVHELD.

       PREPARE-EXCEPTIONS-EXIT.
           EXIT.

      ******************************************************************
       WRITE-DETAIL-LINE SECTION.
      ******************************************************************

           ADD LN-FEED LN-COUNT GIVING LN-WK.
           IF ( LN-WK > 58 )
              PERFORM HEAD-PAGE.

           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.

           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

       WRITE-DETAIL-LINE-EXIT.
           EXIT.

      ******************************************************************
       HEAD-PAGE SECTION.
      ******************************************************************

           ADD 1            TO PAGE-NUMBER.
           MOVE PAGE-NUMBER TO H-PAGE-NO.
           PERFORM GET-TIME.
           MOVE T-HH        TO H-HR.
           MOVE T-MM        TO H-MIN.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE HEAD1       TO PR-REC.
           MOVE 99          TO LN-FEED.
           PERFORM WRITE-PR-REC.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -{02}
           MOVE EXT-CONAME-USER-ID TO H-USERID.
           MOVE HEAD2  TO PR-REC.
           MOVE 1      TO LN-FEED.
           PERFORM WRITE-PR-REC.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -{03}
           MOVE HEAD3 TO PR-REC.
           MOVE 2     TO LN-FEED.
           PERFORM WRITE-PR-REC.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 4 TO LN-COUNT.
           MOVE 2 TO LN-FEED.

       HEAD-PAGE-EXIT.
           EXIT.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/BRSECURE.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBLP/LPDTCL.CPY".

      ************************************
       FORM-RESET SECTION.
      ************************************
           MOVE EXT-CONAME-CONAME TO DTPURG-CONAME.
           MOVE EXT-CONAME-SYSDATE TO DTPURG-SYSDATE.
           DISPLAY DTPURG-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE DTPURG-FORM-FIELDS TO WR-BUF.
           MOVE DTPURG-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       FORM-RESET-EXIT.
           EXIT.


      ************************************
       SETUP-LINK-INFO SECTION.
      ************************************
           MOVE DTPURG-HELP--FILE TO HELP-LINK-FILE.
           MOVE DTPURG-HELP--DIR TO HELP-LINK-DIR.

      ************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/DTPURG_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISC-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/GETPR.CPY".
      *-----------------------------------------------------------------
       SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE LEGEND-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBLP/LPDSGS_SQL.CPY".
       COPY "LIBLP/LPDTGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBLP/LPDS1RN.CPY".
       COPY "LIBLP/LPDT1IN.CPY".

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************
       END-RUN.

           PERFORM CLOSE-FILES.

           IF ( EXT-ROUTE-BUF = "00" )
              PERFORM PRU-OUT.

       EXIT-PROG.

           IF ( EXT-ROUTE-BUF = "00" )
              MOVE EXIT-PATH  TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY DTPURG-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
